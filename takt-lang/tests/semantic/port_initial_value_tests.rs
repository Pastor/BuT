//! Начальное значение выходного порта в целях `c`, `c-hal` и `rust`.
//!
//! # Что доказывается
//!
//! 1. Значение выставляется **до первого такта** - в `_init` цели `c` и в
//!    `new()`/`init()` цели `rust`, а не в теле такта (там живут блоки `enter`,
//!    контракт ).
//! 2. Значение доезжает до цели **литералом**: свёртка в семантике
//!    (`declaration::resolve_port_init`) снимает вопрос "в чьём контексте
//!    печатать выражение", из-за которого цели разошлись бы - у цели `rust`
//!    порты под-моделей выставляет корень.
//! 3. Невычислимое значение - **отказ `SE-094`**, а не молчаливая потеря.
//! 4. Порт, к которому тело автомата не обращается, значение всё равно
//!    получает: начальное значение - это запись, то есть использование.

use std::process::Command;
use takt_lang::generator::GenerateOptions;

/// Порт с адресом и значением, порт со значением без обращения из тела, вход.
///
/// `level` в теле не используется: он проверяет, что начальное значение само по себе
/// делает порт задействованным (иначе цель `rust` не завела бы вариант перечисления, а
/// эмиссия сослалась бы на несуществующее имя). Тело не проводит вход на выход (`ready
/// := btn;`) намеренно: у цели `rust` такая проводка даёт `E0499` - **открытый** дефект
/// [](../../docs/fixes/-rust-port-to-port-borrow.md), к этой задаче отношения не
/// имеющий. Проверка `clippy` ниже упал бы на нём, а не на проверяемом свойстве.
const WITH_INIT: &str = "in btn: bit at 0x40000000:0;\n\
                         out ready: bit at 0x40000004:0 := 1;\n\
                         out level: u8 at 0x40000008 := 7;\n\
                         var seen: bit := 0;\n\
                         start S { always { seen := btn; ready := seen; } }";

/// Значение через константу и арифметику - свёртка обязана дать литерал.
const FOLDED: &str = "const BASE: u8 := 5;\n\
                      out level: u8 at 0x40000008 := BASE + 2;\n\
                      var n: u8 := 0;\n\
                      start S { always { n := n + 1; } }";

/// Значение, не вычислимое при компиляции: переменная модели.
///
/// Вызов функции сюда **не годится**: `const_eval` интерпретирует тело константной
/// функции, и `:= pick()` законно сворачивается. Не вычисляется именно то, что известно
/// лишь в такте.
const NOT_CONST: &str = "var n: u8 := 1;\n\
                         out level: u8 at 0x40000008 := n;\n\
                         start S { always { n := n + 1; } }";

fn build_dir(tag: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0187_03_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание каталога");
    dir
}

/// Порождает C и возвращает каталог вывода вместе с текстом `.c`.
fn generate_c(tag: &str, source: &str) -> (std::path::PathBuf, String) {
    let dir = build_dir(tag);
    takt_lang::compile_to_c(
        tag,
        source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение C");
    let text = std::fs::read_to_string(dir.join(format!("{tag}.c"))).expect("чтение .c");
    (dir, text)
}

/// Порождает Rust и возвращает каталог вывода вместе с текстом модуля.
fn generate_rust(tag: &str, source: &str) -> (std::path::PathBuf, String) {
    let dir = build_dir(tag);
    takt_lang::compile_to_rust(
        tag,
        source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение Rust");
    let text = std::fs::read_to_string(dir.join(format!("{tag}.rs"))).expect("чтение модуля");
    (dir, text)
}

/// Тело функции `_init` порождённого C.
fn c_init_body(text: &str, root: &str) -> String {
    let head = format!("void {root}_init(");
    let start = text.find(&head).unwrap_or_else(|| {
        panic!("в выводе нет функции {head}…:\n{text}");
    });
    let rest = &text[start..];
    let end = rest.find("\n}").expect("конец функции _init");
    rest[..end].to_string()
}

// -- Цель `c` -----------------------------------------------------------------

/// Значение выставляется в `_init` - записью через тот же колбэк HAL, что и запись из
/// тела автомата.
#[test]
fn c_writes_initial_value_in_init() {
    let (_dir, text) = generate_c("cinit", WITH_INIT);
    let init = c_init_body(&text, "Cinit");
    assert!(
        init.contains("(*model->write_bit)(CINIT_PORT_READY, 0, 1, model->userdata);"),
        "битовый порт со значением обязан получить его в `_init`:\n{init}"
    );
    assert!(
        init.contains("(*model->write_numeric)(CINIT_PORT_LEVEL, 0, 7, model->userdata);"),
        "числовой порт со значением обязан получить его в `_init` — \
         в том числе порт, к которому тело автомата не обращается:\n{init}"
    );
}

/// **Контрпример:** у порта без `:=` записи в `_init` не появляется.
///
/// Без этой проверки "починка", пишущая ноль всякому выходу, прошла бы предыдущий тест
/// и изменила бы поведение всего корпуса.
#[test]
fn c_port_without_value_is_not_written() {
    let src = "out ready: bit at 0x40000004:0;\nstart S { always { ready := 1; } }";
    let (_dir, text) = generate_c("cnoinit", src);
    let init = c_init_body(&text, "Cnoinit");
    assert!(
        !init.contains("write_bit"),
        "порт без начального значения в `_init` не пишется:\n{init}"
    );
}

/// Значение входного порта в `_init` не пишется: у входа его не бывает вовсе
/// (`SE-092`), а запись шла бы по чужой таблице адресов.
#[test]
fn c_input_port_is_never_written() {
    let (_dir, text) = generate_c("cin", WITH_INIT);
    assert!(
        !text.contains("CINIT_PORT_BTN, "),
        "входной порт записи не получает:\n{text}"
    );
}

/// Константа и арифметика сворачиваются в литерал ещё в семантике.
#[test]
fn c_folds_constant_expression_to_literal() {
    let (_dir, text) = generate_c("cfold", FOLDED);
    let init = c_init_body(&text, "Cfold");
    assert!(
        init.contains("(*model->write_numeric)(CFOLD_PORT_LEVEL, 0, 7, model->userdata);"),
        "`BASE + 2` обязано доехать до цели литералом 7, а не выражением:\n{init}"
    );
}

// -- Цель `rust` --------------------------------------------------------------

/// Значение выставляется и в `new()`, и в `init()`: это разные входы.
#[test]
fn rust_writes_initial_value_in_new_and_init() {
    let (_dir, text) = generate_rust("rinit", WITH_INIT);
    assert!(
        text.contains("let mut this = Self {"),
        "запись в порт идёт после конструирования — конструктор обязан \
         получить временную привязку:\n{text}"
    );
    assert!(
        text.contains("this.hal.write_bit(OutBitPort::Ready, true);"),
        "`new()` обязан выставить начальное значение:\n{text}"
    );
    assert!(
        text.contains("this.hal.write_u8(OutU8Port::Level, 7);"),
        "`new()` обязан выставить значение порта, не упомянутого в теле:\n{text}"
    );
    assert!(
        text.contains("self.hal.write_bit(OutBitPort::Ready, true);"),
        "`init()` обязан выставить то же значение — иначе сброс разошёлся бы \
         с целью `c`, где `_reset` зовёт `_init`:\n{text}"
    );
}

/// **Контрпример:** без начальных значений форма `new()` прежняя.
///
/// Временная привязка появляется только там, где есть что писать: иначе изменился бы
/// вывод всего корпуса (R12).
#[test]
fn rust_constructor_shape_unchanged_without_values() {
    let src = "out ready: bit at 0x40000004:0;\nstart S { always { ready := 1; } }";
    let (_dir, text) = generate_rust("rnoinit", src);
    assert!(
        text.contains("Self {") && !text.contains("let mut this = Self {"),
        "без начальных значений конструктор печатается как прежде:\n{text}"
    );
}

/// Порт под-модели выставляет **корень**: у под-модели доступа к HAL в конструкторе
/// нет, а перечисления портов общие на файл.
#[test]
fn rust_root_writes_nested_model_port() {
    let src = "model Inner { out temp: u8 at 0x40000010 := 3; \
               start T { always { temp := 3; } } }\n\
               start Root = Inner;";
    let (_dir, text) = generate_rust("rnested", src);
    assert!(
        text.contains("this.hal.write_u8(OutU8Port::Temp, 3);"),
        "значение порта под-модели выставляет корень:\n{text}"
    );
}

// -- Диагностика --------------------------------------------------------------

/// Невычислимое значение - `SE-094` с названной причиной, а не тихая потеря.
#[test]
fn non_constant_value_is_rejected() {
    let dir = build_dir("se094");
    let err = takt_lang::compile_to_c(
        "se094",
        NOT_CONST,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .expect_err("невычислимое начальное значение обязано отвергаться");
    assert_eq!(err.code.as_deref(), Some("SE-094"), "{err:?}");
    assert!(
        err.message.contains("level"),
        "диагностика называет порт: {}",
        err.message
    );
}

// -- Проверки: те же инструменты, что в precheck.sh ------------------------------

fn tool_available(tool: &str, arg: &str) -> bool {
    Command::new(tool)
        .arg(arg)
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Порождённый C собирается `cc -Wall -Werror` (проверка 0171).
#[test]
fn generated_c_passes_cc_gate() {
    if !tool_available("cc", "--version") {
        eprintln!("[ПРОПУСК] generated_c_passes_cc_gate: cc не найден");
        return;
    }
    let (dir, _) = generate_c("cgate", WITH_INIT);
    let out = Command::new("cc")
        .args(["-Wall", "-Werror", "-c"])
        .arg(dir.join("cgate.c"))
        .arg("-o")
        .arg(dir.join("cgate.o"))
        .output()
        .expect("запуск cc");
    assert!(
        out.status.success(),
        "порождённый C обязан собираться под `-Wall -Werror`:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
}

/// Порождённый Rust принимается `clippy -D warnings` (проверка цели `rust`).
///
/// Здесь это не формальность: запись после конструирования `Self` требует `let mut
/// this`, и лишний `mut` - сам по себе отказ гейта (`unused_mut`).
#[test]
fn generated_rust_passes_clippy_gate() {
    if !tool_available("clippy-driver", "--version") {
        eprintln!("[ПРОПУСК] generated_rust_passes_clippy_gate: clippy-driver не найден");
        return;
    }
    let (dir, _) = generate_rust("rgate", WITH_INIT);
    let wrapper = dir.join("gate.rs");
    std::fs::write(
        &wrapper,
        format!(
            "#![no_std]\n#[path = \"{}\"]\npub mod generated;\n",
            dir.join("rgate.rs").display()
        ),
    )
    .expect("запись обёртки");
    let out = Command::new("clippy-driver")
        .args(["--edition", "2021", "--crate-type=lib", "-D", "warnings"])
        .arg(&wrapper)
        .arg("--out-dir")
        .arg(dir.join("out"))
        .output()
        .expect("запуск clippy-driver");
    assert!(
        out.status.success(),
        "порождённый Rust обязан приниматься `clippy -D warnings`:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
}
