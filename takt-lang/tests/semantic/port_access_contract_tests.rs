//! Контракт доступа к порту.
//!
//! # Что нормировано
//!
//! **Чтение порта - выражение, запись - оператор.** Как это отображается,
//! задаёт цель, но **не по своему усмотрению**, а по таблице ниже: она и есть
//! контракт, который получает новая цель вместо образца для подражания.
//!
//! | Цель | Чтение | Запись |
//! |---|---|---|
//! | `c` | `(*ptr->read_<класс>)(ПОРТ, ptr->userdata)` | `(*ptr->write_<класс>)(ПОРТ, значение, ptr->userdata);` |
//! | `c-hal` | то же (отличие цели - таблица адресов и умолчательный HAL) | то же |
//! | `rust` | `hal.read_<тип>(In<Тип>Port::Имя)` | `hal.write_<тип>(Out<Тип>Port::Имя, значение);` |
//! | `st` | имя порта (вход `FUNCTION_BLOCK`) | `имя := значение;` (выход блока) |
//! | `st-at` | имя порта (`VAR_EXTERNAL` над размещённой глобальной) | `имя := значение;` |
//! | `sv` | имя входного порта модуля | `имя_next = значение;` (защёлкивание - в `always_ff`) |
//! | `sv-mmio` | имя сигнала бита регистрового файла | `имя_next = значение;` |
//!
//! **Анонимное обращение** `#0xАДРЕС as ТИП` входит в тот же
//! контракт: чтение - выражение, запись - оператор. Отображают его лишь цели,
//! знающие адресное пространство:
//!
//! | Цель | Чтение | Запись |
//! |---|---|---|
//! | `c-hal` | `((ТИП)(*(volatile uintN_t*)(uintptr_t)0xАДРЕСu))` | `*(volatile uintN_t*)... = ...;` (поле - чтением-изменением-записью) |
//! | `st-at` | имя размещённой ячейки (`AT_<адрес>_<бит>_<ширина>`) | `имя := значение;` |
//! | `sv-mmio` | имя сигнала ячейки | `имя_next = значение;` |
//! | `c`, `rust`, `st`, `sv` | **отказ** с названной причиной | **отказ** |
//!
//! Имя ячейки строит компилятор (`AnonPortAccess::synthetic_name`) - одно и то же у
//! эталона, `st-at` и `sv-mmio`. Разойдясь на символ, сверка трасс сравнивала бы разные
//! величины.
//!
//! `ptr` у цели `c` - `model` в корневой модели и `main` в под-модели: HAL живёт у
//! корня, а порт может быть объявлен на любом уровне.
//!
//! У целей `sv`/`sv-mmio` запись идёт в **`_next`**, а не в регистр: иначе `v := 1; w
//! := v;` дало бы значение предыдущего такта, и такой модуль приняли бы **оба**
//! инструмента SV (главный капкан ).

use takt_lang::generator::GenerateOptions;

/// Фикстура: чтение числового входа, запись числового выхода, проводка бита.
const SRC: &str = "in btn: bit at 0x100:0;\n\
                   in level: u8 at 0x101;\n\
                   out led: bit at 0x200:1;\n\
                   out value: u8 at 0x201;\n\
                   var seen: u8 := 0;\n\
                   start Run {\n\
                       always { seen := level; value := seen + 1; led := btn; }\n\
                       ref Run: btn = 1;\n\
                   }";

/// Присваивание внутри выражения - нарушение контракта "запись - оператор".
const NESTED_ASSIGN: &str = "in btn: bit at 0x100:0;\n\
                             out value: u8 at 0x201;\n\
                             var seen: u8 := 0;\n\
                             start Run {\n\
                                 always { seen := (value := 3) + 1; }\n\
                                 ref Run: btn = 1;\n\
                             }";

fn out_dir(tag: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0187_06_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    dir
}

/// Компилирует фикстуру целью и возвращает текст вывода.
fn generate(tag: &str, target: &str) -> String {
    let dir = out_dir(tag);
    let path = dir.to_str().expect("путь");
    let env = takt_lang::parse_defines(&[]).expect("среда");
    let opts = GenerateOptions::default();
    let ext = match target {
        "c" | "c-hal" => "c",
        "rust" => "rs",
        "st" | "st-at" => "st",
        _ => "sv",
    };
    match target {
        "c" => {
            takt_lang::compile_to_c(tag, SRC, path, &[], &opts).expect("цель c");
        }
        "c-hal" => {
            takt_lang::compile_to_c_hal(tag, SRC, path, &[], &[], &env, &opts).expect("цель c-hal");
        }
        "rust" => {
            takt_lang::compile_to_rust(tag, SRC, path, &[], &opts).expect("цель rust");
        }
        "st" => {
            takt_lang::compile_to_st(tag, SRC, path, &[], &opts).expect("цель st");
        }
        "st-at" => {
            takt_lang::compile_to_st_at(tag, SRC, path, &[], &[], &env, &opts).expect("цель st-at");
        }
        "sv" => {
            takt_lang::compile_to_sv(tag, SRC, path, &[], &opts).expect("цель sv");
        }
        "sv-mmio" => {
            takt_lang::compile_to_sv_mmio(tag, SRC, path, &[], &[], &env, &opts)
                .expect("цель sv-mmio");
        }
        other => panic!("неизвестная цель {other}"),
    }
    std::fs::read_to_string(dir.join(format!("{tag}.{ext}"))).expect("вывод цели")
}

/// Цель `c`: доступ - колбэки HAL, по одному на класс значения.
///
/// Имя перечислителя порта несёт сегмент `PORT_` с: порты и состояния - перечислители
/// одной области видимости C, и `out settled` рядом с `state Settled` давали один
/// элемент дважды. Форма имени видна пользователю (она в сигнатуре колбэка), поэтому
/// смена - ломающее изменение, сделанное осознанно: запрещать естественное именование
/// (светофор `red`/`Red`) нельзя.
#[test]
fn c_access_matches_contract() {
    let text = generate("cacc", "c");
    assert!(
        text.contains("(*model->read_numeric)(CACC_PORT_LEVEL, 0, model->userdata)"),
        "чтение числового входа — колбэком `read_numeric` с индексом элемента (0533):\n{text}"
    );
    assert!(
        text.contains(
            "(*model->write_numeric)(CACC_PORT_VALUE, 0, model->seen + 1, model->userdata);"
        ),
        "запись числового выхода — колбэк с индексом элемента (0533):\n{text}"
    );
    assert!(
        text.contains("(*model->write_bit)(CACC_PORT_LED, 0, (*model->read_bit)(CACC_PORT_BTN, 0, model->userdata), model->userdata);"),
        "проводка бита — чтение внутри записи, у обеих номер разряда (0533):\n{text}"
    );
}

/// Цель `c-hal` отличается таблицей адресов и принятым по умолчанию HAL, но **не** формой доступа:
/// те же колбэки.
#[test]
fn c_hal_access_matches_contract() {
    let text = generate("chalacc", "c-hal");
    assert!(
        text.contains("(*model->read_numeric)(CHALACC_PORT_LEVEL, 0, model->userdata)")
            && text.contains("(*model->write_numeric)(CHALACC_PORT_VALUE, 0,"),
        "цель c-hal обязана печатать тот же доступ, что и c:\n{text}"
    );
}

/// Цель `rust`: доступ - методы трейта HAL, порт - вариант перечисления.
#[test]
fn rust_access_matches_contract() {
    let text = generate("rustacc", "rust");
    assert!(
        text.contains("hal.read_u8(InU8Port::Level)"),
        "чтение — метод трейта с вариантом входного порта:\n{text}"
    );
    assert!(
        text.contains("hal.write_u8(OutU8Port::Value,"),
        "запись — метод трейта с вариантом выходного порта:\n{text}"
    );
    assert!(
        text.contains("hal.write_bit(OutBitPort::Led,"),
        "битовый порт пишется `write_bit`:\n{text}"
    );
}

/// Цель `st`: порт - вход/выход `FUNCTION_BLOCK`, доступ - по имени.
#[test]
fn st_access_matches_contract() {
    let text = generate("stacc", "st");
    assert!(
        text.contains("seen := level;"),
        "чтение входа — по имени порта:\n{text}"
    );
    assert!(
        text.contains("value := seen + 1;"),
        "запись выхода — оператором присваивания:\n{text}"
    );
}

/// Цель `st-at`: порт размещён глобально, блок видит его через `VAR_EXTERNAL`, но
/// **форма доступа та же** - по имени.
#[test]
fn st_at_access_matches_contract() {
    let text = generate("stataccess", "st-at");
    assert!(
        text.contains("VAR_EXTERNAL"),
        "порт цели st-at виден блоку как внешняя переменная:\n{text}"
    );
    assert!(
        text.contains("seen := level;") && text.contains("value := seen + 1;"),
        "доступ печатается так же, как у цели st:\n{text}"
    );
}

/// Анонимное обращение входит в тот же контракт: чтение - выражение, запись - оператор;
/// отображение задаёт цель, а не печатник по своему усмотрению.
///
/// Тест нужен по той же причине, что и у именованного порта: документ без
/// теста - договорённость, которую следующая правка нарушит молча.
#[test]
fn anon_access_matches_contract() {
    const ANON: &str = "var seen: u8 := 0;\n\
                        start Run {\n\
                            always { seen := #0x2000 as u8; #0x2004 as u8 := seen + 1; }\n\
                            ref Run: seen < 10;\n\
                        }";
    let dir = out_dir("anonacc");
    let path = dir.to_str().expect("путь");
    let env = takt_lang::parse_defines(&[]).expect("среда");
    let opts = GenerateOptions::default();

    takt_lang::compile_to_c_hal("anonacc", ANON, path, &[], &[], &env, &opts).expect("цель c-hal");
    let c_hal = std::fs::read_to_string(dir.join("anonacc.c")).expect("вывод c-hal");
    assert!(
        c_hal.contains("model->seen = ((uint8_t)(*(volatile uint8_t*)(uintptr_t)0x2000u));"),
        "чтение ячейки — выражение с разыменованием:\n{c_hal}"
    );
    assert!(
        c_hal.contains("(*(volatile uint8_t*)(uintptr_t)0x2004u) = (uint8_t)(model->seen + 1);"),
        "запись ячейки — оператор присваивания по адресу:\n{c_hal}"
    );

    takt_lang::compile_to_st_at("anonacc", ANON, path, &[], &[], &env, &opts).expect("цель st-at");
    let st_at = std::fs::read_to_string(dir.join("anonacc.st")).expect("вывод st-at");
    assert!(
        st_at.contains("seen := AT_2000_0_8;") && st_at.contains("AT_2004_0_8 := seen + 1;"),
        "у цели st-at доступ идёт по имени размещённой ячейки:\n{st_at}"
    );

    takt_lang::compile_to_sv_mmio("anonacc", ANON, path, &[], &[], &env, &opts)
        .expect("цель sv-mmio");
    let sv = std::fs::read_to_string(dir.join("anonacc.sv")).expect("вывод sv-mmio");
    assert!(
        sv.contains("AT_2004_0_8_next = "),
        "запись ячейки идёт в комбинационную пару `_next`:\n{sv}"
    );

    // Отказ целей без адресного пространства - тоже часть контракта, но он проверяется
    // там, где живут его коды: `takt-lang/tests/anon_port_tests.rs` (`CC-021`,
    // `ST-018`, `SV-017` и отказ цели `rust`). Дублировать проверку здесь значило бы
    // завести второй источник истины о тех же кодах.
}

/// Цель `sv`: чтение - имя порта модуля, запись - в комбинационный `_next`.
#[test]
fn sv_access_matches_contract() {
    let text = generate("svacc", "sv");
    assert!(
        text.contains("value_next = (svacc_seen_next + 1);"),
        "запись выхода идёт в `_next`, а не в регистр:\n{text}"
    );
    assert!(
        text.contains("led_next = btn;"),
        "чтение входа — по имени порта модуля:\n{text}"
    );
    assert!(
        text.contains("value <= value_next;"),
        "защёлкивание `_next` — в регистровой части:\n{text}"
    );
}

/// Цель `sv-mmio`: порт - бит регистрового файла, но форма доступа автомата та же, что
/// у `sv` (`_next`); отличается только происхождение сигнала.
#[test]
fn sv_mmio_access_matches_contract() {
    let text = generate("mmioacc", "sv-mmio");
    assert!(
        text.contains("value_next = (mmioacc_seen_next + 1);") && text.contains("led_next = btn;"),
        "автомат обращается к битам регистрового файла той же формой:\n{text}"
    );
    assert!(
        text.contains("reg_addr") && text.contains("reg_rdata"),
        "адресованные порты обслуживает регистровый интерфейс:\n{text}"
    );
}

/// Запись внутри выражения отвергается **разбором** - одинаково для всех целей, а не
/// каждой по-своему.
///
/// До исправления [](../../docs/fixes/-assignment-is-statement-in-grammar.md) правило держала
/// семантика (`SE-095`); теперь его держит грамматика, и код стал синтаксическим
/// (`SY-006`) с позицией самого токена `:=`.
#[test]
fn nested_assignment_is_rejected_once_for_all_targets() {
    let codes: Vec<String> =
        takt_lang::collect_compile_diagnostics("probe.takt", NESTED_ASSIGN, &[], false)
            .into_iter()
            .filter_map(|d| d.code)
            .collect();
    assert!(
        codes.contains(&"SY-006".to_string()),
        "присваивание внутри выражения обязано отвергаться разбором: {codes:?}"
    );
}

/// **Контрпример:** законные позиции записи молчат - оператор тела и шаг цикла.
///
/// Без этой проверки правка, запрещающая присваивание везде, прошла бы предыдущий
/// тест и сломала бы весь корпус.
#[test]
fn statement_positions_stay_legal() {
    let src = "in btn: bit at 0x100:0;\n\
               out value: u8 at 0x201;\n\
               var acc: u8 := 0;\n\
               start Run {\n\
                   always {\n\
                       value := acc + 1;\n\
                       for var i: u8 := 0; i < 3; i := i + 1 { acc := acc + i; }\n\
                   }\n\
                   ref Run: btn = 1;\n\
               }";
    let codes: Vec<String> = takt_lang::collect_compile_diagnostics("probe.takt", src, &[], false)
        .into_iter()
        .filter_map(|d| d.code)
        .collect();
    assert!(
        codes.is_empty(),
        "запись как оператор и шаг цикла — законные позиции: {codes:?}"
    );
}
