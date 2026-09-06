//! Не-ASCII имя в нижнем регистре у цели `rust` -.
//!
//! # Что здесь тестытся
//!
//! Имена языка могут быть не-ASCII, а приведение к CamelCase шло через
//! `to_ascii_uppercase`: не-ASCII буква оставалась строчной. Поэтому `out кнопка`
//! давало вариант перечисления `кнопка`, и `clippy -D warnings` - теми же флагами, что
//! в проверке предкоммита, - отвечал **ошибкой**: "variant `кнопка` should have an upper
//! camel case name". Код возврата `taktc` при этом был нулевым.
//!
//!
//! | Вход | Цель `rust` | `clippy -D warnings` |
//! |---|---|---|
//! | `out кнопка` | `кнопка,` | **error** upper camel case |
//! | `out Кнопка` **(контроль)** | `Кнопка,` | принято |
//! | `out button` **(контроль)** | `Button,` | принято |
//!
//! Контроли показывают, что дело в **регистре**, а не в алфавите: ASCII-имя цель
//! поднимала в верхний регистр всегда.
//!
//! **Коллизия регистра не нова и ловится:** `кнопка` и `Кнопка` дают одно имя, и об
//! этом говорит существующая `RS-005` - ровно как о паре `button`/`Button` до фичи.
//! Нового класса правка не заводит.
//!
//! **Корпус класс не покрывает:** не-ASCII имён в `examples/` нет ни одного.

use std::path::PathBuf;
use std::process::Command;
use takt_lang::generator::GenerateOptions;

const LOWER: &str = "out кнопка: bit at 0x100:0;\n\
                     var n: u8 := 0;\n\
                     start Run { always { кнопка := 1; n := n + 1; } ref Run: n < 3; }\n";

const UPPER: &str = "out Кнопка: bit at 0x100:0;\n\
                     var n: u8 := 0;\n\
                     start Run { always { Кнопка := 1; n := n + 1; } ref Run: n < 3; }\n";

const ASCII: &str = "out button: bit at 0x100:0;\n\
                     var n: u8 := 0;\n\
                     start Run { always { button := 1; n := n + 1; } ref Run: n < 3; }\n";

const COLLIDE: &str = "out кнопка: bit at 0x100:0;\n\
                       out Кнопка: bit at 0x100:1;\n\
                       var n: u8 := 0;\n\
                       start Run {\n\
                           always { кнопка := 1; Кнопка := 0; n := n + 1; }\n\
                           ref Run: n < 3;\n\
                       }\n";

fn build_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0299_{thread}_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание каталога");
    dir
}

fn generate_rust(tag: &str, source: &str) -> (PathBuf, String) {
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

/// **T1.** Не-ASCII имя поднимается в верхний регистр.
#[test]
fn non_ascii_name_is_capitalised() {
    let (_dir, text) = generate_rust("lower", LOWER);
    assert!(
        text.contains("Кнопка,") && text.contains("OutBitPort::Кнопка"),
        "вариант обязан быть в верхнем регистре:\n{text}"
    );
    assert!(
        !text.contains("    кнопка,"),
        "строчного варианта остаться не должно:\n{text}"
    );
}

/// **T2. Контроль: имена, работавшие и прежде, не изменились.**
#[test]
fn already_correct_names_are_untouched() {
    let (_dir, upper) = generate_rust("upper", UPPER);
    assert!(upper.contains("Кнопка,"), "не-ASCII с заглавной:\n{upper}");
    let (_dir, ascii) = generate_rust("ascii", ASCII);
    assert!(ascii.contains("Button,"), "ASCII-имя:\n{ascii}");
}

/// **T3.** Слипание имён после приведения регистра ловит `RS-005`.
///
/// Механизм не новый: ту же пару `button`/`Button` он ловил и до фичи. Без этой
/// проверки правка выглядела бы как "завела класс коллизий", которого на самом деле не
/// завела.
#[test]
fn case_collision_is_reported() {
    let dir = build_dir("collide");
    let err = takt_lang::compile_to_rust(
        "collide",
        COLLIDE,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .expect_err("слипшиеся имена обязаны отвергаться");
    assert_eq!(err.code.as_deref(), Some("RS-005"), "код коллизии");
    assert!(
        err.message.contains("после приведения регистра"),
        "отказ обязан называть причину:\n{}",
        err.message
    );
}

/// **T4.** Порождённый модуль принимается `clippy -D warnings` - как в проверке.
#[test]
fn generated_rust_passes_clippy_gate() {
    let available = Command::new("clippy-driver")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false);
    if !available {
        eprintln!("[ПРОПУСК] generated_rust_passes_clippy_gate: clippy-driver не найден");
        return;
    }
    for (tag, src) in [
        ("gate_lower", LOWER),
        ("gate_upper", UPPER),
        ("gate_ascii", ASCII),
    ] {
        let (dir, _) = generate_rust(tag, src);
        let wrapper = dir.join("gate.rs");
        std::fs::write(
            &wrapper,
            format!(
                "#![no_std]\n#[path = \"{}\"]\npub mod generated;\n",
                dir.join(format!("{tag}.rs")).display()
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
            "порождённый Rust ({tag}) обязан приниматься `clippy -D warnings`:\n{}",
            String::from_utf8_lossy(&out.stderr)
        );
    }
}
