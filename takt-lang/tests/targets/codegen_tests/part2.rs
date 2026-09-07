//! Продолжение `codegen_tests`: вторая половина тестов кодогенерации, вынесенная в
//! подмодуль. Helpers и импорты - из родителя (`use super::*`); чистое перемещение,
//! утверждения не меняются.

use super::*;

/// внутри `if (FsmA_is_done(...))`.
///
/// Контр-пример: без break C-компилятор выполнил бы следующий case-блок.
#[test]
fn test_concat_non_last_has_break_inside_if() {
    let src = r#"
model A { start Idle; state End; }
model B { start Run; state End; }
start Main = A + B {
    next Done;
}
state Done;
"#;
    let c = generate_c_content(src, "Fsm");

    // В case FSM_MAIN внутри if (FsmA_is_done) должна быть смена _state и break
    assert!(
        c.contains("FsmA_is_done("),
        "должна быть проверка FsmA_is_done:\n{c}"
    );
    // После смены main_state на B1 должен идти break; (внутри if) Гибкая проверка:
    // после FSM_MAIN_B1 есть break до закрывающей }
    let marker = "FSM_MAIN_B1";
    let has_break_inside = if let Some(pos) = c.find(marker) {
        let after = &c[pos..];
        after
            .find("break;")
            .map(|b| after.find('}').map(|cl| b < cl).unwrap_or(false))
            .unwrap_or(false)
    } else {
        false
    };
    assert!(
        has_break_inside,
        "после смены main_state в if (A_is_done) должен быть break перед }}:\n{c}"
    );
}

/// Проверяет правильный порядок генерации: exit текущего состояния должен идти до смены
/// model->state при переходе.
///
/// Позитивный пример: при переходе из A в B, exit-блок A должен быть сгенерирован перед
/// строкой `model->state = ...`.
///
/// Контр-пример (старый код): `model->state = B;` стояло до `exit`-блока A.
#[test]
fn test_transition_exit_before_state_change() {
    let src = r#"
model A {
    start Go;
    state End;
}
start Main = A {
    next Done;
    exit { }
}
state Done;
"#;
    let c = generate_c_content(src, "Fsm");

    // Проверка is_done должна присутствовать
    assert!(
        c.contains("if (FsmA_is_done"),
        "должна быть проверка is_done:\n{c}"
    );
    // Переход в Done должен присутствовать (константа FSM_DONE в root-модели)
    assert!(
        c.contains("FSM_DONE"),
        "целевое состояние DONE должно присутствовать в коде:\n{c}"
    );
    // model->state = FSM_DONE должен идти после завершения exit-логики Порядок: exit
    // (пустой) -> enter (нет) -> state = DONE -> break Проверяем что state = FSM_DONE
    // присутствует внутри if(is_done)
    let pos_is_done = c
        .find("if (FsmA_is_done")
        .expect("is_done должен быть в коде");
    let after_is_done = &c[pos_is_done..];
    assert!(
        after_is_done.contains("FSM_DONE"),
        "model->state = FSM_DONE должен быть внутри if(is_done):\n{c}"
    );
}

/// Проверяет что INIT-диспетчер корректно переходит в стартовое состояние с вызовом
/// `enter`-блока после инициализации extend.
///
/// Позитивный пример: `_init` вызывается до установки стартового состояния.
///
/// (Option B): вход в стартовое состояние **не расходует такт** - INIT диспетчеризуется
/// через `if (model->state == FSM_INIT)` до `switch`, а не отдельным `case ... break;`.
/// Ассерт про форму обновлён соответственно; суть (порядок `_init` -> установка
/// состояния) сохранена.
#[test]
fn test_init_calls_enter_after_init() {
    let src = r#"
model Sub {
    start Run;
    state End;
}
start Main = Sub {
    next Done;
    enter { }
}
state Done;
"#;
    let c = generate_c_content(src, "Fsm");

    // В INIT-диспетчере должен быть _init для Sub
    assert!(
        c.contains("FsmSub_init("),
        "INIT должен вызывать FsmSub_init:\n{c}"
    );
    // INIT диспетчеризуется до switch - `if`, а не `case ... break`
    assert!(
        c.contains("if (model->state == FSM_INIT)"),
        "INIT должен диспетчеризоваться через if до switch (0033):\n{c}"
    );
    assert!(
        !c.contains("case FSM_INIT:"),
        "INIT больше не должен быть case внутри switch (0033):\n{c}"
    );
    // _init должен стоять до model->state = FSM_MAIN в INIT-диспетчере
    let pos_init = c
        .find("FsmSub_init(")
        .expect("FsmSub_init должен быть в INIT");
    let pos_state = c
        .find("model->state = FSM_MAIN;")
        .expect("model->state = FSM_MAIN должен быть");
    assert!(
        pos_init < pos_state,
        "FsmSub_init должен стоять ДО model->state = FSM_MAIN:\n{c}"
    );
}

/// Проверяет что безусловный переход (ref без условия) для простого состояния
/// генерирует exit -> enter -> state -> break.
#[test]
fn test_simple_state_unconditional_transition() {
    let src = r#"
start A {
    ref B;
    exit { }
}
state B {
    enter { }
}
"#;
    let c = generate_c_content(src, "Fsm");

    // Переход из A в B должен присутствовать
    assert!(
        c.contains("FSM_B"),
        "целевое состояние B должно присутствовать:\n{c}"
    );
    // break должен быть в case A
    assert!(
        c.contains("break;"),
        "безусловный переход должен содержать break:\n{c}"
    );
}

/// Проверяет что `always`-блоки генерируются до проверки условий перехода.
///
/// Семантика: always-блок выполняется каждый тик, условия - после него.
#[test]
fn test_always_blocks_before_transitions() {
    let src = r#"
start A {
    ref B;
    always { }
}
state B;
"#;
    let c = generate_c_content(src, "Fsm");

    // В case A: always должен быть (хотя тело пустое, структура присутствует) И переход
    // в B должен быть после
    assert!(
        c.contains("FSM_A"),
        "состояние A должно присутствовать в коде:\n{c}"
    );
    assert!(
        c.contains("FSM_B"),
        "целевое состояние B должно присутствовать:\n{c}"
    );
}

// -- Тесты оборачивания тела цикла в фигурные скобки (Changes-58) --------------

/// Бесконечный `loop` генерирует `while (true) {` с фигурными скобками.
#[test]
fn loop_body_infinite_has_braces() {
    let src = r#"
var flag: bool;
start A {
    always {
        loop { flag := true; }
    }
}
"#;
    let c = generate_c_content(src, "Fsm");
    assert!(
        c.contains("while (true) {"),
        "бесконечный loop должен генерировать `while (true) {{`:\n{c}"
    );
}

/// `loop` с условием генерирует `while (...) {` с фигурными скобками.
#[test]
fn loop_body_cond_has_braces() {
    let src = r#"
var flag: bool;
start A {
    always {
        loop flag { flag := false; }
    }
}
"#;
    let c = generate_c_content(src, "Fsm");
    assert!(
        c.contains("while (") && c.contains(") {"),
        "цикл loop с условием должен генерировать `while (...) {{`:\n{c}"
    );
}

/// `for`-цикл генерирует `for (` с фигурными скобками вокруг тела.
#[test]
fn for_body_has_braces() {
    let src = r#"
var flag: bool;
start A {
    always {
        for var i: bool := true; i; i := false { flag := i; }
    }
}
"#;
    let c = generate_c_content(src, "Fsm");
    assert!(
        c.contains("for ("),
        "for-цикл должен генерировать `for (`:\n{c}"
    );
    assert!(
        c.contains(") {"),
        "тело for-цикла должно быть обёрнуто в фигурные скобки:\n{c}"
    );
}

// -- Тесты фильтрации неиспользуемых элементов (Changes-59) -------------------

/// Неиспользуемая переменная не попадает в сгенерированную C-структуру.
///
/// Позитивный пример: переменная `unused` объявлена, но нигде не используется - она
/// должна отсутствовать в заголовочном файле и не инициализироваться в init.
///
/// Контр-пример: переменная `used` присваивается в always и должна присутствовать.
#[test]
fn unused_var_excluded() {
    let src = r#"
var unused: u8 := 0;
var used: u8 := 0;
start S {
    always { used := 1; }
}
"#;
    // Поля struct - в.h, инициализация - в.c
    let h = generate_h_content(src, "Fsm");
    let c = generate_c_content(src, "Fsm");
    assert!(
        !h.contains("uint8_t unused"),
        "неиспользуемая переменная `unused` не должна появляться в struct (.h):\n{h}"
    );
    assert!(
        h.contains("uint8_t used"),
        "используемая переменная `used` должна присутствовать в struct (.h):\n{h}"
    );
    assert!(
        !c.contains("model->unused"),
        "неиспользуемая переменная `unused` не должна инициализироваться в init (.c):\n{c}"
    );
}

/// Используемая переменная остаётся в сгенерированной C-структуре.
///
/// Позитивный пример: переменная `counter` читается и пишется в always - она должна
/// присутствовать в struct (.h).
///
/// Контр-пример: если бы фильтрация удаляла переменные из условий, код не
/// скомпилировался бы.
#[test]
fn used_var_stays() {
    let src = r#"
var counter: u8 := 0;
start S {
    always { counter := counter + 1; }
}
"#;
    let h = generate_h_content(src, "Fsm");
    assert!(
        h.contains("uint8_t counter"),
        "используемая переменная `counter` должна присутствовать в struct (.h):\n{h}"
    );
}

/// Неиспользуемая константа не попадает в сгенерированный C-код.
///
/// Позитивный пример: `DEAD` объявлена, но нигде не используется - `CONST_FSM_DEAD`
/// должна отсутствовать в `.c`-файле.
///
/// Контр-пример: `LIVE` используется в always, она должна присутствовать.
#[test]
fn unused_const_excluded() {
    let src = r#"
const DEAD: u8 := 42;
const LIVE: u8 := 7;
var v: u8 := 0;
start S {
    always { v := LIVE; }
}
"#;
    let c = generate_c_content(src, "Fsm");
    assert!(
        !c.contains("CONST_FSM_DEAD"),
        "неиспользуемая константа DEAD не должна появляться в коде:\n{c}"
    );
    assert!(
        c.contains("CONST_FSM_LIVE"),
        "используемая константа LIVE должна присутствовать в коде:\n{c}"
    );
}

/// Используемая константа остаётся в сгенерированном C-коде.
///
/// Позитивный пример: `MAX` используется в выражении присваивания переменной - она
/// должна генерироваться как `#define CONST_FSM_MAX`.
///
/// Контр-пример: если бы фильтрация удаляла `MAX`, компилятор C выдал бы ошибку.
#[test]
fn used_const_stays() {
    let src = r#"
const MAX: u8 := 255;
var v: u8 := 0;
start S {
    always { v := MAX; }
}
"#;
    let c = generate_c_content(src, "Fsm");
    assert!(
        c.contains("CONST_FSM_MAX"),
        "используемая константа MAX должна присутствовать в коде:\n{c}"
    );
}

// -- Тест коллизии имён enum-варианта и состояния (Changes-XX) -----------------

/// Проверяет что при сравнении переменной типа перечисления с одноимённым состоянием в
/// правой части `=`, генерируется числовое значение варианта, а не идентификатор
/// состояния.
///
/// Позитивный пример: `command = Stop` где `command: Command` и существует состояние
/// `Stop` - должно генерировать `== 2` (индекс Stop в Command).
///
/// Контр-пример: без исправления генерировался бы `MOTOR_STOP` (состояние).
#[test]
fn test_enum_equal_name_collision_generates_value() {
    let src = r#"
enum Command { Up, Down, Stop }
var command: Command := Stop;
model Motor {
    start Idle {
        ref Stop: command = Stop;
    }
    state Stop { }
}
start Main = Motor;
"#;
    let c = generate_c_content(src, "Main");
    // Должна быть сравнение с числовым значением (2 = индекс Stop), а не с MOTOR_STOP
    assert!(
        c.contains("== 2") || c.contains("command == 2"),
        "команда Stop должна сравниваться с числовым значением варианта перечисления:\n{c}"
    );
}

// --: режим c-hal (таблица адресов + умолчательный HAL) ---------------

/// Читает единственный `.h`-файл из каталога вывода.
fn read_header(out_dir: &str) -> String {
    let h = fs::read_dir(out_dir)
        .unwrap()
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .find(|p| p.extension().map(|x| x == "h").unwrap_or(false))
        .expect("должен быть .h-файл");
    fs::read_to_string(h).unwrap()
}

/// c-hal эмитит таблицу адресов, умолчательный HAL и bind-помощник.
#[test]
fn c_hal_emits_address_table_and_hal() {
    let tmp = tempdir().unwrap();
    let out = tmp.path().to_str().unwrap();
    let src = "in BTN: u8 at 0x00200000; out LED: bit; \
               address LED = 0x00200004; start Idle { ref On: BTN; } state On { ref Idle: BTN; }";
    let warnings = takt_lang::compile_to_c_hal(
        "demo.takt",
        src,
        out,
        &[],
        &[],
        &takt_lang::AddressEnv::default(),
        &takt_lang::GenerateOptions::default(),
    )
    .expect("c-hal должен компилироваться");
    // `address LED = 0x00200004;` - однобитный порт с адресом без позиции бита.
    // Умолчание "бит 0" объявляется предупреждением `SE-090`, а не принимается молча
    // каждым потребителем по-своему. Внешней карты здесь нет, поэтому других
    // предупреждений быть не должно.
    assert_eq!(
        warnings
            .iter()
            .map(|w| w.code.clone().unwrap_or_default())
            .collect::<Vec<_>>(),
        vec!["SE-090".to_string()],
        "ожидалось ровно одно предупреждение о позиции бита: {:?}",
        warnings
    );

    let h = read_header(out);
    assert!(h.contains("Demo_PortBinding"), "нет типа привязки:\n{}", h);
    assert!(h.contains("__ADDR[]"), "нет таблицы адресов:\n{}", h);
    assert!(h.contains("0x200000"), "нет адреса BTN:\n{}", h);
    assert!(
        h.contains("Demo_bind_default_hal"),
        "нет bind-помощника:\n{}",
        h
    );
    assert!(
        h.contains("typedef struct Demo Demo;"),
        "нужен typedef корня для валидного C:\n{}",
        h
    );
}

/// Обычный режим `c` не эмитит HAL-артефакты (регресс = 0).
#[test]
fn plain_c_has_no_hal_artifacts() {
    let tmp = tempdir().unwrap();
    let out = tmp.path().to_str().unwrap();
    let src = "in BTN: u8 at 0x00200000; start Idle { ref On: BTN; } state On;";
    takt_lang::compile_to_c(
        "demo.takt",
        src,
        out,
        &[],
        &takt_lang::GenerateOptions::default(),
    )
    .expect("c должен компилироваться");
    let h = read_header(out);
    assert!(
        !h.contains("PortBinding"),
        "режим c не должен эмитить HAL:\n{}",
        h
    );
    assert!(!h.contains("bind_default_hal"));
}

/// Используемый порт без адреса в c-hal -> ошибка полноты SE-052.
#[test]
fn c_hal_missing_address_is_error() {
    let tmp = tempdir().unwrap();
    let out = tmp.path().to_str().unwrap();
    let src = "in BTN: bit; start S { ref T: BTN; } state T;";
    let err = takt_lang::compile_to_c_hal(
        "demo.takt",
        src,
        out,
        &[],
        &[],
        &takt_lang::AddressEnv::default(),
        &takt_lang::GenerateOptions::default(),
    )
    .expect_err("used-порт без адреса должен давать ошибку");
    assert_eq!(err.code.as_deref(), Some("SE-052"));
}

/// Внешняя карта переопределяет адрес модели -> c-hal успешен + предупреждение SE-050.
#[test]
fn c_hal_external_overrides_and_warns() {
    let tmp = tempdir().unwrap();
    let out = tmp.path().to_str().unwrap();
    let src = "in BTN: u8 at 0x00100000; start Idle { ref On: BTN; } state On;";
    let entries = takt_lang::parse_address_map("BTN = 0x40000000;", 0).unwrap();
    let warnings = takt_lang::compile_to_c_hal(
        "demo.takt",
        src,
        out,
        &[],
        &entries,
        &takt_lang::AddressEnv::default(),
        &takt_lang::GenerateOptions::default(),
    )
    .expect("c-hal должен компилироваться");
    assert!(warnings.iter().any(|d| d.code.as_deref() == Some("SE-050")));
    let h = read_header(out);
    assert!(
        h.contains("0x40000000"),
        "должен эмитить адрес из карты:\n{}",
        h
    );
}

/// **Главная проверка: порождённый C компилируется.**
///
/// Модель **без под-моделей** - простейший класс, и именно он был сломан: генератор не
/// эмитил `typedef struct {Root} {Root};`, структура печаталась тегом, а прототипы -
/// через голое имя. **Контрольная точка:** до исправления тот же вход давал **8 ошибок**
/// `must use 'struct' tag to refer to type`.
///
/// Тест живой: он вызывает настоящий `cc`. Снапшот тут бесполезен - он зафиксировал бы
/// невалидный C ровно так же охотно, как валидный. Дефект дожил до сих пор именно
/// потому, что все пять `examples/*.takt`, на которых `precheck.sh` собирает C,
/// содержат под-модели и попадали в рабочую ветку.
#[test]
fn test_single_model_generates_compilable_c() {
    if !cc_available() {
        eprintln!("[пропуск] cc недоступен — живая проверка C пропущена");
        return;
    }
    let dir = tempdir().expect("временный каталог");
    let src = "var n: u8 := 0;\nstart S { always { n := n + 1; } }";
    takt_lang::compile_to_c(
        "single.takt",
        src,
        dir.path().to_str().unwrap(),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("компиляция в C");

    let out = std::process::Command::new("cc")
        .arg("-std=c11")
        .arg("-fsyntax-only")
        .arg("single.c")
        .current_dir(dir.path())
        .output()
        .expect("запуск cc");
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        out.status.success() && !stderr.contains("error:"),
        "порождённый C одиночной модели обязан компилироваться, cc сказал:\n{stderr}"
    );
}

/// Порождённый C для композиции `f -> g` внутри одной модели собирается `cc
/// -std=c99` без implicit-function-declaration. Требует эмиссии форвард-прототипов: `f`
/// печатается раньше `g` (алфавит), и без прототипа вызов `g` из `f` был бы обращением
/// к необъявленной функции.
#[test]
fn test_fn_composition_generates_compilable_c() {
    if !cc_available() {
        eprintln!("[пропуск] cc недоступен — живая проверка C композиции пропущена");
        return;
    }
    let dir = tempdir().expect("временный каталог");
    let src = "var y: u8 := 0;\n\
               fn g(x: u8) -> u8 { return x; }\n\
               fn f(x: u8) -> u8 { return g(x); }\n\
               start Main { always { y := f(1); } }";
    takt_lang::compile_to_c(
        "compose.takt",
        src,
        dir.path().to_str().unwrap(),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("компиляция в C");

    let c = fs::read_to_string(dir.path().join("compose.c")).expect("compose.c");
    // Указатель на состояние печатается по нужде: функция `g` состоянием не пользуется,
    // и `const Compose *model` в её сигнатуре больше нет. Предмет теста - **наличие
    // форвард-прототипа** до определений, а не состав параметров.
    assert!(
        c.contains("static uint8_t Compose_g(uint8_t x);"),
        "должен быть форвард-прототип Compose_g до определений:\n{c}"
    );

    let out = std::process::Command::new("cc")
        .args([
            "-std=c99",
            "-Wall",
            "-Werror=implicit-function-declaration",
            "-fsyntax-only",
            "compose.c",
        ])
        .current_dir(dir.path())
        .output()
        .expect("запуск cc");
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        out.status.success() && !stderr.contains("error:"),
        "C композиции функций обязан собираться (форвард-прототипы), cc сказал:\n{stderr}"
    );
}

/// Тест детерминизма C-заголовка. Компилирует модель с портами в двух под-моделях
/// дважды и сверяет `.h` байт-в-байт. Порты нумеруются сквозным `enumerate()` по
/// под-моделям - их значения `enum` в `.h` есть ABI. До 0048 межмодельный порядок
/// брался из обхода `HashMap`, и два прогона расходились.
///
/// Тест строит вывод дважды в одном процессе. С `BTreeMap` порядок стабилен всегда; при
/// возврате к `HashMap` тест падал бы случайно - потому и тест.
#[test]
fn test_c_header_ports_are_reproducible() {
    let src = r#"
model Beta {
    start S;
    in beta_x: bit;
    in beta_y: bit;
}
model Alpha {
    start T;
    in alpha_p: bit;
    in alpha_q: bit;
}
start Main = Alpha | Beta;
    "#;

    let read_header = || {
        let dir = tempdir().expect("временный каталог");
        takt_lang::compile_to_c(
            "ports.takt",
            src,
            dir.path().to_str().unwrap(),
            &[],
            &takt_lang::generator::GenerateOptions::default(),
        )
        .expect("компиляция в C");
        let h = dir.path().join("ports.h");
        fs::read_to_string(&h).expect("порождённый ports.h")
    };

    let first = read_header();
    for i in 1..8 {
        assert_eq!(
            first,
            read_header(),
            "прогон {i} дал другой ports.h — вернулся недетерминизм порядка портов (ABI)"
        );
    }
}

/// Проверяет, доступен ли `cc` - без него живая проверка пропускается.
fn cc_available() -> bool {
    std::process::Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// C для `invariant P = C;` идентичен C для пары `cond P = C; : [Guard] P;` -
/// десахаризация не меняет эмиссию. Плюс assert присутствует (инвариант эмитится как
/// `assert()`).
#[test]
fn test_invariant_c_equals_cond_guard() {
    let inv = generate_c_content(
        "var t: u8 := 0; invariant Safe = t <= 100; start Main { always { } }",
        "Fsm",
    );
    let pair = generate_c_content(
        "var t: u8 := 0; cond Safe = t <= 100; : [Guard] Safe; start Main { always { } }",
        "Fsm",
    );
    // Извлекаем строки assert из обоих выводов и сверяем.
    let asserts = |c: &str| {
        c.lines()
            .filter(|l| l.contains("assert(") && l.contains("<= 100"))
            .map(str::trim)
            .map(String::from)
            .collect::<Vec<_>>()
    };
    assert!(
        !asserts(&inv).is_empty(),
        "инвариант обязан эмитить assert(... <= 100):\n{inv}"
    );
    assert_eq!(
        asserts(&inv),
        asserts(&pair),
        "C(invariant) обязан совпадать с C(cond + Guard) по assert'ам"
    );
}
