//! Интеграционные тесты цели Structured Text (IEC 61131-3).
//!
//! отображение типов и секции объявлений. Тесты идут через публичный API
//! [`takt_lang::compile_to_st`] - то есть проверяют файл, который реально увидит
//! инженер ПЛК, а не промежуточное представление.
//!
//! Ожидаемые строки **сняты зондом** с фактического вывода компилятора (правило
//! `CLAUDE.md`: сперва зонд, затем assertions против захваченного).
//!
//! ## Чего эти тесты не доказывают
//!
//! Валидность ST по стандарту доказывает только внешний транспилятор MatIEC `iec2c`; в
//! CI его нет. На сегодня вывод `iec2c` **отвергает** - у `FUNCTION_BLOCK` нет тела
//! (`CASE state OF` - ). Проверено, что это **единственное** препятствие: те же файлы с
//! подставленным фиктивным телом принимаются по всему корпусу `examples/`.

use std::fs;
use std::path::{Path, PathBuf};
use takt_lang::generator::GenerateOptions;

/// Компилирует фикстуру в ST и возвращает текст порождённого файла.
fn compile_fixture(name: &str) -> String {
    let src_path = format!("tests/data/st/valid/{}.takt", name);
    let source = fs::read_to_string(&src_path).unwrap_or_else(|e| panic!("{}: {}", src_path, e));
    let out_dir = target_dir(name);
    let _ = fs::remove_dir_all(&out_dir);
    takt_lang::compile_to_st(
        &src_path,
        &source,
        out_dir.to_str().unwrap(),
        &[],
        &GenerateOptions::default(),
    )
    .unwrap_or_else(|d| panic!("компиляция {} провалилась: {:?}", name, d));
    fs::read_to_string(out_dir.join(format!("{}.st", name)))
        .unwrap_or_else(|e| panic!("нет порождённого .st для {}: {}", name, e))
}

/// Каталог вывода теста - уникальный **по тесту**, а не по фикстуре.
///
/// Прежде имя строилось только из фикстуры, и это держало весь проект на
/// `--test-threads=1`: три теста берут `types_all`, два - `enum_struct`, а
/// [`compile_fixture`] начинает с `remove_dir_all` - параллельно один тест сносит
/// каталог, пока другой в него пишет (`Os { code: 22, InvalidInput }`).
///
/// Ключ - имя потока: тестовый харнесс Rust называет поток именем теста. В однопоточном
/// режиме имени нет (тест идёт в главном потоке), и ключом остаётся фикстура - там
/// гонок и не бывает.
fn target_dir(name: &str) -> PathBuf {
    let owner = std::thread::current()
        .name()
        .map(|t| t.replace(|c: char| !c.is_ascii_alphanumeric(), "_"))
        .unwrap_or_else(|| "single".to_string());
    Path::new(env!("CARGO_TARGET_TMPDIR")).join(format!("st_{}_{}", name, owner))
}

/// Используемая переменная-массив попадает в ST настоящим `ARRAY`.
///
/// Прямой контрпример дефекту Д1b: на этом же входе цель `c` печатает `uint4_t` -
/// размер массива подставлен вместо разрядности, тип не существует.
#[test]
fn test_st_array_variable_is_emitted_as_real_array() {
    let st = compile_fixture("array_var");
    assert!(
        st.contains("data : ARRAY [0..3] OF USINT;"),
        "массив [u8; 4] обязан дать ARRAY [0..3] OF USINT:\n{st}"
    );
    assert!(
        !st_code(&st).contains("uint4_t"),
        "разрядность не должна подменяться числом элементов:\n{st}"
    );
}

/// Скалярные типы отображаются по нормативной таблице T1..T11.
///
/// Ключевые пункты: `float` -> `LREAL` (f64 - как симулятор), а не `REAL` (f32, дефект
/// Д3); `bit` -> `BOOL`, а не `int` (дефект Д2). `i16` -> `INT` - в IEC `INT`
/// 16-битный, это не опечатка.
#[test]
fn test_st_scalar_types_follow_iec_table() {
    let st = compile_fixture("types_all");
    for expected in [
        "flag : BOOL := TRUE;",
        "ready : BOOL := FALSE;",
        "ratio : LREAL := 0.0;",
        "small : USINT := 0;",
        "signed_small : INT := 0;",
        "wide : UDINT := 0;",
        "signed_wide : LINT := 0;",
    ] {
        assert!(st.contains(expected), "нет объявления '{expected}':\n{st}");
    }
    assert!(
        !st.contains(": REAL "),
        "float обязан давать LREAL (f64), а не REAL (f32):\n{st}"
    );
}

/// Варианты перечисления становятся именованными константами.
///
/// Не перечислимым типом IEC: MatIEC отвергает явные значения вариантов.
/// Значения - `Bottom = 80`, `Top` наследует `81` (снято зондом с
/// `examples/elevator.takt:117`).
#[test]
fn test_st_enum_variants_become_named_constants() {
    let st = compile_fixture("enum_struct");
    assert!(st.contains("VAR CONSTANT"), "нет секции констант:\n{st}");
    assert!(
        st.contains("Floor_Bottom : USINT := 80;"),
        "нет константы Floor_Bottom:\n{st}"
    );
    assert!(
        st.contains("Floor_Top : USINT := 81;"),
        "Top обязан наследовать значение 81:\n{st}"
    );
}

/// Структура объявляется через `TYPE ... END_TYPE` **до** блока.
///
/// В IEC 61131-3 порядок объявлений значим: тип обязан быть известен к моменту
/// использования.
#[test]
fn test_st_struct_type_is_declared_before_function_block() {
    let st = compile_fixture("enum_struct");
    let type_decl = st.find("TYPE").expect("нет объявления TYPE");
    let fb = st.find("FUNCTION_BLOCK").expect("нет FUNCTION_BLOCK");
    assert!(
        type_decl < fb,
        "TYPE обязан предшествовать FUNCTION_BLOCK:\n{st}"
    );
    assert!(st.contains("END_STRUCT;"), "структура не закрыта:\n{st}");
    assert!(
        st.contains("origin : Coord;"),
        "переменная структурного типа ссылается на объявленный тип:\n{st}"
    );
}

/// Массив нулевого размера -> `ST-007`; файл **не создаётся**.
///
/// Проверяется и код диагностики, и отсутствие вывода: половинчатый файл был бы хуже
/// отказа - его бы отдали в среду ПЛК.
#[test]
fn test_st_zero_sized_array_fails_with_st007_and_writes_nothing() {
    let src_path = "tests/data/st/invalid/array_zero.takt";
    let source = fs::read_to_string(src_path).unwrap();
    let out_dir = target_dir("array_zero");
    let _ = fs::remove_dir_all(&out_dir);
    let err = takt_lang::compile_to_st(
        src_path,
        &source,
        out_dir.to_str().unwrap(),
        &[],
        &GenerateOptions::default(),
    )
    .expect_err("массив нулевого размера обязан отвергаться");
    assert_eq!(
        err.code.as_deref(),
        Some("ST-007"),
        "диагностика: {:?}",
        err
    );
    assert!(
        !out_dir.join("array_zero.st").exists(),
        "при отказе файл ST не должен создаваться"
    );
}

/// Минимальная модель даёт каркас: вход в стартовое состояние, `CASE`
/// с состояниями и `END`.
///
/// Вход печатается **до** `CASE` и не расходует скан.
#[test]
fn test_minimal_model_emits_case_skeleton() {
    let st = compile_fixture("minimal_fb");
    assert!(st.contains("FUNCTION_BLOCK MinimalFb"), "нет блока:\n{st}");
    assert!(st.contains("CASE state OF"), "нет CASE:\n{st}");
    let init = st
        .find("IF state = 0 THEN")
        .expect("нет входа в стартовое состояние");
    let case = st.find("CASE state OF").expect("нет CASE");
    assert!(
        init < case,
        "вход в стартовое обязан идти ДО CASE — иначе он стоит скана:\n{st}"
    );
    assert!(
        !st.contains("0: (* INIT *)"),
        "ветвь INIT вернулась в CASE:\n{st}"
    );
    assert!(
        st.contains("state : USINT := 0;"),
        "нет переменной автомата: ноль и есть «ещё не входили»\n{st}"
    );
}

/// Несколько `ref` -> `IF`/`ELSIF` в порядке объявления.
#[test]
fn test_multiple_refs_keep_declaration_order() {
    let st = compile_fixture("transitions");
    let first = st.find("IF n = 1 THEN").expect("нет первого перехода");
    let second = st.find("ELSIF n = 2 THEN").expect("нет второго перехода");
    assert!(first < second, "порядок ref обязан сохраняться:\n{st}");
}

/// `always` до проверок; `exit` источника перед `enter` цели.
///
/// Порядок сверен с зондом цели `c`, а не предположен.
#[test]
fn test_always_precedes_checks_and_exit_precedes_enter() {
    let st = compile_fixture("enter_always");
    let always = st.find("n := n + 1;").expect("нет always");
    let check = st.find("IF n = 1 THEN").expect("нет проверки перехода");
    let exit = st.find("n := 2;").expect("нет exit источника");
    let enter = st.find("n := 3;").expect("нет enter цели");
    assert!(always < check, "always обязан идти до проверок:\n{st}");
    assert!(
        exit < enter,
        "exit источника обязан идти до enter цели:\n{st}"
    );
}

/// `A | B` -> экземпляры, последовательные вызовы, конъюнкция `is_done`.
#[test]
fn test_parallel_composition_joins_by_conjunction() {
    let st = compile_fixture("parallel");
    assert!(
        st.contains("FUNCTION_BLOCK ParallelA"),
        "нет блока A:\n{st}"
    );
    assert!(
        st.contains("FUNCTION_BLOCK ParallelB"),
        "нет блока B:\n{st}"
    );
    assert!(
        st.contains(".is_done AND ") && st.contains(".is_done THEN"),
        "завершение параллели — конъюнкция is_done:\n{st}"
    );
}

/// `A + B` -> вложенный `CASE` по счётчику шагов, а не параллель.
///
/// Фикстура называется `sequence`, а не `concat`, намеренно: имя модели берётся из
/// имени файла, а `CONCAT` - **стандартная функция IEC** (склейка строк), и
/// `FUNCTION_BLOCK Concat` транспилятор отвергает. Пространство имён POU и стандартной
/// библиотеки - общее.
#[test]
fn test_concatenation_uses_step_counter_not_parallel() {
    let st = compile_fixture("sequence");
    assert!(
        st.contains("_step"),
        "у конкатенации обязан быть счётчик шагов:\n{st}"
    );
    assert!(
        st.matches("CASE ").count() >= 2,
        "конкатенация — вложенный CASE:\n{st}"
    );
}

/// Цель `st`: порты - входы/выходы блока, адрес не эмитится.
#[test]
fn test_ports_without_at_in_plain_st() {
    let st = compile_fixture("ports_at");
    assert!(st.contains("btn : BOOL;"), "нет входного порта:\n{st}");
    assert!(st.contains("lamp : BOOL;"), "нет выходного порта:\n{st}");
    assert!(
        !st_code(&st).contains("AT %"),
        "цель st адрес не эмитит:\n{st}"
    );
}

/// Цель `st-at`: тот же исходник даёт `VAR_GLOBAL ... AT %...` в `CONFIGURATION`.
///
/// Пара с тестом выше: цели **асимметричны**, и это фиксируется с обеих сторон.
#[test]
fn test_same_ports_get_locations_in_st_at() {
    let src_path = "tests/data/st/valid/ports_at.takt";
    let source = fs::read_to_string(src_path).unwrap();
    let out_dir = target_dir("ports_at_at");
    let _ = fs::remove_dir_all(&out_dir);
    takt_lang::compile_to_st_at(
        src_path,
        &source,
        out_dir.to_str().unwrap(),
        &[],
        &[],
        &takt_lang::AddressEnv::default(),
        &GenerateOptions::default(),
    )
    .expect("st-at должен компилироваться");
    let st = fs::read_to_string(out_dir.join("ports_at.st")).unwrap();
    assert!(
        st.contains("btn AT %IX256.0 : BOOL;"),
        "нет локации входа:\n{st}"
    );
    assert!(
        st.contains("lamp AT %QX1280.0 : BOOL;"),
        "нет локации выхода:\n{st}"
    );
    assert!(
        st.contains("VAR_EXTERNAL"),
        "блок обязан видеть порты извне:\n{st}"
    );
    assert!(
        st.contains("CONFIGURATION"),
        "VAR_GLOBAL требует обёртки:\n{st}"
    );
}

/// Компилирует контрпример целью `st-at` и возвращает диагностику.
fn compile_invalid_at(name: &str, map: Option<&str>) -> Vec<String> {
    let src_path = format!("tests/data/st/invalid/{}.takt", name);
    let source = fs::read_to_string(&src_path).unwrap();
    let entries = match map {
        Some(m) => {
            let map_src = fs::read_to_string(format!("tests/data/st/invalid/{}", m)).unwrap();
            takt_lang::parse_address_map(&map_src, 0).expect("карта должна разбираться")
        }
        None => Vec::new(),
    };
    let out_dir = target_dir(&format!("inv_{}", name));
    let _ = fs::remove_dir_all(&out_dir);
    // Коды, а не сами диагностики: `Diagnostic` из крейта не экспортирован, а проверять
    // всё равно нужно код, а не текст сообщения.
    match takt_lang::compile_to_st_at(
        &src_path,
        &source,
        out_dir.to_str().unwrap(),
        &[],
        &entries,
        &takt_lang::AddressEnv::default(),
        &GenerateOptions::default(),
    ) {
        Ok(warnings) => warnings.iter().filter_map(|d| d.code.clone()).collect(),
        Err(d) => d.code.into_iter().collect(),
    }
}

/// Порт-массив в `st-at` разворачивается по элементам.
///
/// Прежде тест закреплял `ST-004` ("локация IEC адресует скаляр"), и это было
/// **дефектом, закреплённым тестом**: довод верен для массива целиком, но не для порта -
/// по элементам он раскладывается в скалярные, и каждый получает свою локацию.
#[test]
fn test_array_port_is_split_into_elements() {
    let dir = std::env::temp_dir().join(format!(
        "takt_0417_stat_{}_{}",
        std::process::id(),
        std::thread::current()
            .name()
            .unwrap_or("t")
            .replace(':', "_")
    ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    let source = std::fs::read_to_string("tests/data/st/valid/port_array_at.takt")
        .expect("фикстура читается");
    takt_lang::compile_to_st_at(
        "port_array_at",
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &[],
        &takt_lang::address_map::AddressEnv::default(),
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порт-массив разворачивается и размещается");
    let text = std::fs::read_to_string(dir.join("port_array_at.st")).expect("чтение вывода");
    assert!(
        text.contains("arr_0 AT %IB768"),
        "элемент обязан получить свою локацию по базовому адресу:\n{text}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// Используемый порт без адреса в `st-at` -> `SE-052`.
///
/// Диагностика приходит из слоя 0020 **без изменений** - переиспользование, а не
/// дублирование правила.
#[test]
fn test_used_port_without_address_is_se052() {
    let diags = compile_invalid_at("port_no_address", None);
    assert!(
        diags.iter().any(|c| c == "SE-052"),
        "ожидался SE-052, получено: {diags:?}"
    );
}

/// Висячая запись карты -> `SE-051` (вероятная опечатка).
///
/// Заодно фиксируется `SE-050`: карта перекрывает inline-адрес `btn` - наложение по
/// замыслу.
#[test]
fn test_dangling_map_entry_is_se051_and_overlay_is_se050() {
    let diags = compile_invalid_at("dangling_map_entry", Some("dangling_map_entry.map"));
    assert!(diags.iter().any(|c| c == "SE-051"), "нет SE-051: {diags:?}");
    assert!(
        diags.iter().any(|c| c == "SE-050"),
        "нет SE-050 (оверлей): {diags:?}"
    );
}

/// **Закрывает РИ4.** Внешняя карта переопределяет МК-адреса на ПЛК-локации.
///
/// `elevator.takt` написан под МК-адресацию: `in sensors_1: u8 at 268435456;`
/// (`0x10000000`) -> `AT %IB268435456`, чего не имеет ни один ПЛК. Модель при этом
/// **корректна** (для целей `c`/`c-hal`) и не правится: ПЛК-локации живут во внешней
/// карте, а генератор берёт их по приоритету (inline < `address` < карта).
#[test]
fn test_external_map_overrides_mcu_addresses_with_plc_locations() {
    let source = fs::read_to_string("../examples/elevator.takt").unwrap();
    let map_src = fs::read_to_string("../examples/elevator.plc.map").unwrap();
    let entries = takt_lang::parse_address_map(&map_src, 0).expect("карта должна разбираться");
    let out_dir = target_dir("elevator_at");
    let _ = fs::remove_dir_all(&out_dir);

    takt_lang::compile_to_st_at(
        "../examples/elevator.takt",
        &source,
        out_dir.to_str().unwrap(),
        &[],
        &entries,
        &takt_lang::AddressEnv::default(),
        &GenerateOptions::default(),
    )
    .expect("st-at с картой должен компилироваться");

    let st = fs::read_to_string(out_dir.join("elevator.st")).unwrap();
    assert!(
        st.contains("sensors_1 AT %IB0"),
        "карта обязана переопределить адрес на ПЛК-локацию:\n{}",
        &st[..st.len().min(2000)]
    );
    assert!(
        !st.contains("%IB268435456"),
        "МК-адрес не должен доезжать до ПЛК-локации"
    );
    assert!(
        st.contains("CONFIGURATION"),
        "st-at обязана эмитить обёртку: VAR_GLOBAL вне CONFIGURATION недопустим"
    );
}

/// Цель `st` карту адресов **не** потребляет: она порождает библиотеку блоков.
#[test]
fn test_plain_st_target_ignores_addresses_and_emits_no_configuration() {
    let st = compile_fixture("types_all");
    assert!(!st.contains("AT %"), "цель st адреса не эмитит:\n{st}");
    assert!(
        !st.contains("CONFIGURATION"),
        "цель st обёртки не требует — это библиотека блоков:\n{st}"
    );
}

/// Комментарии порождённого файла - только в форме IEC `(* ... *)`.
///
/// Проверка не косметическая: `//` и `/* */` для компилятора ST - синтаксическая
/// ошибка, то есть файл не приняла бы ни одна среда ПЛК.
#[test]
fn test_st_output_uses_iec_comments_only() {
    let st = compile_fixture("types_all");
    assert!(!st.contains("//"), "C-комментарий недопустим в ST:\n{st}");
    assert!(!st.contains("/*"), "C-комментарий недопустим в ST:\n{st}");
}

/// Компилирует исходник в ST из строки и возвращает результат (для проверок диагностик,
/// где ожидается **ошибка**). Имя файла задаёт имя корневой модели.
fn compile_st_source(
    filename: &str,
    source: &str,
) -> Result<Vec<takt_lang::diagnostics::Diagnostic>, takt_lang::diagnostics::Diagnostic> {
    // Каталог уникален по тесту: `prog.takt` берут несколько тестов, а ниже идёт
    // `remove_dir_all` - общий каталог связал бы их гонкой ровно так же, как это делал
    // `target_dir` до починки.
    let out_dir = Path::new(env!("CARGO_TARGET_TMPDIR")).join(format!(
        "st_src_{}_{}",
        filename.replace('.', "_"),
        std::thread::current()
            .name()
            .map(|t| t.replace(|c: char| !c.is_ascii_alphanumeric(), "_"))
            .unwrap_or_else(|| "single".to_string())
    ));
    let _ = fs::remove_dir_all(&out_dir);
    fs::create_dir_all(&out_dir).unwrap();
    takt_lang::compile_to_st(
        filename,
        source,
        out_dir.to_str().unwrap(),
        &[],
        &GenerateOptions::default(),
    )
}

/// **`ST-014` на имени модели, совпавшем со стандартной библиотекой IEC.**
///
/// Имя корневой модели берётся из **имени файла**: `concat.takt` -> `FUNCTION_BLOCK
/// Concat`, а `CONCAT` - стандартная функция IEC. `iec2c` даёт `invalid function block
/// name`; `ST-014` называет причину.
#[test]
fn test_st014_on_model_name_colliding_with_stdlib() {
    let err = compile_st_source(
        "concat.takt",
        "var x: u8 := 0;\nstart S { always { x := x + 1; } }\n",
    )
    .expect_err("модель Concat обязана дать ST-014");
    assert_eq!(err.code.as_deref(), Some("ST-014"), "код диагностики");
}

/// **`ST-014` на переменной `left` с указанием причины.**
///
/// `iec2c` отвечает обманчиво: `invalid located variable declaration` (про `AT %...`,
/// которых в объявлении нет). `ST-014` называет настоящую причину.
#[test]
fn test_st014_on_variable_named_left_names_the_cause() {
    let err = compile_st_source(
        "prog.takt",
        "var left: u8 := 0;\nstart S { always { left := left + 1; } }\n",
    )
    .expect_err("переменная left обязана дать ST-014");
    assert_eq!(err.code.as_deref(), Some("ST-014"));
    let msg = err.message.to_lowercase();
    assert!(
        msg.contains("iec 61131-3") && msg.contains("переименуйте"),
        "текст обязан называть причину и путь решения:\n{}",
        err.message
    );
}

/// **Регистронезависимость: `LEFT`, `Left`, `left` -> одинаково.**
#[test]
fn test_st014_case_insensitive() {
    for name in ["left", "LEFT", "Left"] {
        let src = format!(
            "var {n}: u8 := 0;\nstart S {{ always {{ {n} := {n} + 1; }} }}\n",
            n = name
        );
        let err = compile_st_source("prog.takt", &src)
            .expect_err(&format!("'{}' обязано дать ST-014", name));
        assert_eq!(err.code.as_deref(), Some("ST-014"), "имя '{}'", name);
    }
}

/// **Набор стандартных имён (функции, ключевое слово SFC) -> `ST-014`.**
#[test]
fn test_st014_various_reserved_names() {
    for name in ["abs", "min", "sel", "limit", "step"] {
        let src = format!(
            "var {n}: u8 := 0;\nstart S {{ always {{ {n} := {n} + 1; }} }}\n",
            n = name
        );
        let err = compile_st_source("prog.takt", &src)
            .expect_err(&format!("'{}' обязано дать ST-014", name));
        assert_eq!(err.code.as_deref(), Some("ST-014"), "имя '{}'", name);
    }
}

/// Имя, которое `iec2c` **принимает**, `ST-014` давать не должна - иначе ложное
/// срабатывание сломало бы валидную модель (`remaining` - то, во что 0030 переименовала
/// `left`).
#[test]
fn test_st014_accepts_valid_names() {
    compile_st_source(
        "prog.takt",
        "var remaining: u8 := 0;\nstart S { always { remaining := remaining + 1; } }\n",
    )
    .expect("valid имя remaining должно компилироваться без ST-014");
}

/// **Тест исправления [](../fixes/-st-fn-dedup-silent.md), Tier 1.**
///
/// Две модели с одноимённой `fn helper`, но разными телами (`x+1` и `x+2`). До
/// префиксации цель `st` дедуплицировала их по голому имени: эмитилась **одна**
/// `FUNCTION helper` с телом первой модели, `iec2c` её принимал (rc=0), а модель B
/// молча считала `w = 2` вместо `w = 3` - переход `Done2` не срабатывал никогда.
///
/// Ожидаемое (эталон - цель `c`): **две разные** функции с префиксом модели- владельца,
/// и каждая модель зовёт **свою**.
///
/// **Проверка `iec2c` этот дефект не ловит** - он и был зелёным, пока дефект жил (дублей в
/// тексте нет - ловить нечего). Тест - здесь; потактовую верность модели B (`w = 3`)
/// доказывает сверка `conformance_st_tests`.
#[test]
fn test_st_same_named_fns_get_distinct_prefixed_functions() {
    let st = compile_fixture("dup_fn");
    // Две разные функции - по одной на модель (без склейки).
    assert!(
        st.contains("FUNCTION DupFnA_helper : USINT"),
        "нет функции модели A с префиксом:\n{st}"
    );
    assert!(
        st.contains("FUNCTION DupFnB_helper : USINT"),
        "нет функции модели B с префиксом:\n{st}"
    );
    // Тела различны - тело B не должно быть потеряно (Tier-1 симптом).
    assert!(
        st.contains("DupFnA_helper := x + 1;"),
        "тело функции A искажено:\n{st}"
    );
    assert!(
        st.contains("DupFnB_helper := x + 2;"),
        "тело функции B потеряно/искажено — ровно симптом фикса 0041-01:\n{st}"
    );
    // Ровно две `FUNCTION ...helper` - не одна (склейка) и не больше.
    let helper_fns = st.matches("_helper : USINT").count();
    assert_eq!(helper_fns, 2, "ожидались ровно две функции helper:\n{st}");
    // Каждая модель зовёт свою функцию (имена вызова совпадают с объявлением).
    assert!(
        st.contains("w := DupFnA_helper(1);") && st.contains("w := DupFnB_helper(1);"),
        "вызовы должны адресовать функцию своей модели:\n{st}"
    );
    // Голого `helper` (без префикса) не осталось - склейки нет.
    assert!(
        !st.contains("FUNCTION helper "),
        "голое имя FUNCTION helper означало бы возврат склейки:\n{st}"
    );
}

/// Текст вывода ST без комментариев - блочных `(* ... *)` и построчных.
///
/// Нужен с: комментарии автора модели переносятся в вывод, и фикстура, объясняющая
/// словами "адрес не эмитится, а `st-at` даёт `AT %IX256.0`", приносит эти слова в
/// порождённый ST. Проверка на отсутствие конструкции обязана смотреть на код - иначе
/// она красна из-за собственного пояснения (тот же класс, что у сверок фикстур
/// `conformance_fixed_*`).
fn st_code(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    let mut rest = text;
    while let Some(open) = rest.find("(*") {
        out.push_str(&rest[..open]);
        let Some(close) = rest[open..].find("*)") else {
            return out;
        };
        rest = &rest[open + close + 2..];
    }
    out.push_str(rest);
    out
}
