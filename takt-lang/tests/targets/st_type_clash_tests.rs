//! Имя переменной, совпавшее с именем типа, у цели `st`.

use std::process::Command;
use takt_lang::generator::GenerateOptions;

/// Имя переменной совпадает с именем структуры.
const CLASH: &str = "struct Pair { lo: u8, hi: u8 }\n\
                     var pair: Pair := {1, 2};\n\
                     var n: u8 := 0;\n\
                     out a: u8 at 0x100;\n\
                     start Run { always { n := n + 1; pair.lo := n; a := pair.lo; } \
                     ref Done: n > 3; }\n\
                     state Done { }\n";

/// **Контрпример:** то же самое с непересекающимся именем.
const CLEAN: &str = "struct Pair { lo: u8, hi: u8 }\n\
                     var point: Pair := {1, 2};\n\
                     var n: u8 := 0;\n\
                     out a: u8 at 0x100;\n\
                     start Run { always { n := n + 1; point.lo := n; a := point.lo; } \
                     ref Done: n > 3; }\n\
                     state Done { }\n";

fn compile_st(
    tag: &str,
    source: &str,
) -> Result<std::path::PathBuf, takt_lang::diagnostics::Diagnostic> {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0378_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    takt_lang::compile_to_st(
        "probe",
        source,
        dir.to_str().expect("путь"),
        &[],
        &GenerateOptions::default(),
    )
    .map(|_| dir)
}

/// Столкновение имени с типом - отказ `ST-023`, называющий оба имени.
#[test]
fn variable_named_like_type_is_refused() {
    let err = compile_st("clash", CLASH).expect_err("ожидался отказ цели");
    assert_eq!(err.code.as_deref(), Some("ST-023"), "код отказа");
    assert!(
        err.message.contains("'pair'") && err.message.contains("'Pair'"),
        "отказ обязан назвать ОБА имени: {}",
        err.message
    );
    assert!(
        err.message.contains("остаётся валидной"),
        "отказ принадлежит цели — текст обязан это сказать: {}",
        err.message
    );
}

/// **Контрпример:** непересекающееся имя переводится, и `iec2c` вывод
/// принимает.
///
/// Без него правило читалось бы как "структуры у цели `st` не работают".
#[test]
fn distinct_name_is_translated_and_accepted() {
    let dir = compile_st("clean", CLEAN).expect("вывод обязан порождаться");
    let prefix = std::env::var("IEC2C_PREFIX")
        .unwrap_or_else(|_| format!("{}/.local", std::env::var("HOME").unwrap_or_default()));
    let iec2c = std::path::Path::new(&prefix).join("bin").join("iec2c");
    let lib = std::path::Path::new(&prefix)
        .join("share")
        .join("matiec")
        .join("lib");
    if !iec2c.is_file() || !lib.join("ieclib.txt").is_file() {
        eprintln!("[ПРОПУСК] distinct_name_is_translated_and_accepted: нет iec2c");
        return;
    }
    let out_dir = dir.join("iec");
    std::fs::create_dir_all(&out_dir).expect("каталог iec2c");
    let out = Command::new(&iec2c)
        .arg("-I")
        .arg(&lib)
        .arg("-T")
        .arg(&out_dir)
        .arg(dir.join("probe.st"))
        .output()
        .expect("запуск iec2c");
    assert!(
        out.status.success(),
        "вывод обязан приниматься MatIEC:\n{}",
        String::from_utf8_lossy(&out.stdout)
    );
    let _ = std::fs::remove_dir_all(&dir);
}
