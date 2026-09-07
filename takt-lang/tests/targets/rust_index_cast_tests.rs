//! Приведение индекса к `usize` печатается по нужде.
//!
//! # Что здесь проверяется
//!
//! Индексация массива в цели `rust` печаталась `self.arr[1 as usize]` - с
//! приведением **всегда**. Для индекса-переменной оно необходимо (типы Takt -
//! `u8`...`i64`), для литерала избыточно: `clippy::unnecessary_cast` под
//! `-D warnings` - теми же флагами, что стоят в проверке предкоммита, - отвечает
//! **ошибкой**, то есть порождённый модуль не проходит проверка проекта.
//!
//!
//! **Проверка цели `rust` этого не видел по устройству:** он гоняет только корпус, а там
//! индексируют **переменной** - литеральных индексов в `examples/` нет ни одного.
//!
//! **Контроль обязателен**: без проверки "переменная по-прежнему приводится" правка
//! "убрать `as usize`" прошла бы положительный тест и сломала бы весь корпус.

use std::path::PathBuf;
use std::process::Command;
use takt_lang::generator::GenerateOptions;

/// Оба вида индекса рядом: литерал и переменная, в чтении и в записи.
const BOTH: &str = "var arr: [u8; 3] := { 0, 0, 0 };\n\
                    var i: u8 := 1;\n\
                    var seen: u8 := 0;\n\
                    start Run {\n\
                        always {\n\
                            arr[1] := 7;\n\
                            arr[i] := 9;\n\
                            seen := arr[1];\n\
                        }\n\
                        ref Run;\n\
                    }\n";

/// Индекс в условии - второй печатник, у которого было своё `as usize`.
const IN_CONDITION: &str = "var arr: [u8; 3] := { 0, 0, 0 };\n\
                            var i: u8 := 1;\n\
                            var seen: u8 := 0;\n\
                            start Run {\n\
                                always { seen := 1; }\n\
                                ref Done: arr[2] = 0;\n\
                            }\n\
                            state Done { always { seen := arr[i]; } }\n";

fn build_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0263_{thread}_{tag}"));
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

/// Литеральный индекс печатается без приведения, переменная - с ним.
#[test]
fn literal_index_has_no_cast_variable_keeps_it() {
    let (_dir, text) = generate_rust("idx", BOTH);
    assert!(
        text.contains("self.arr[1] = 7;") && text.contains("self.seen = self.arr[1];"),
        "литеральный индекс печатается без приведения:\n{text}"
    );
    assert!(
        text.contains("self.arr[self.i as usize] = 9;"),
        "индекс-переменная приведение сохраняет:\n{text}"
    );
    assert!(
        !text.contains("[1 as usize]"),
        "приведения литерала остаться не должно:\n{text}"
    );
}

/// Печатник условий подчиняется тому же правилу.
///
/// У него было своё `as usize`; носитель теперь один на обоих (`subscript`), иначе
/// правило разъехалось бы молча.
#[test]
fn condition_printer_follows_the_same_rule() {
    let (_dir, text) = generate_rust("idx_cond", IN_CONDITION);
    assert!(
        text.contains("self.arr[2]"),
        "литеральный индекс в условии — без приведения:\n{text}"
    );
    assert!(
        text.contains("self.arr[self.i as usize]"),
        "переменная в теле приведение сохраняет:\n{text}"
    );
}

/// Порождённый модуль принимается `clippy -D warnings` - как в проверке.
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
    for (tag, src) in [("clippy_idx", BOTH), ("clippy_cond", IN_CONDITION)] {
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
