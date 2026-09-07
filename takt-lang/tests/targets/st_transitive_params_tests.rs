//! Разделяемые переменные передаются транзитивно у цели `st`.
//!
//! # Что проверяется
//!
//! - вложенный вызов: `VAR_IN_OUT` вызывающей функции включает нужды вызываемой;
//! - цепочка длиной три - признак транзитивен, а не "на один шаг";
//! - `iec2c` принимает вывод (главный тест: класс был невалидным ST);
//! - контроль: чистая функция параметров не получает.

use std::path::PathBuf;
use std::process::Command;

use takt_lang::generator::GenerateOptions;

/// Вложенный вызов: `outer` зовёт `hot`, читающую переменную модели.
const NESTED: &str = "out sum: u8 at 0x2000;\nvar ticks: u8 := 0;\n\
     fn hot() -> u8 { return ticks + 1; }\n\
     fn outer(k: u8) -> u8 { return k + hot(); }\n\
     start Run { always { ticks := ticks + 1; sum := outer(ticks); } ref Run; }\n";

/// Цепочка длиной три: `outer` -> `mid` -> `hot`, и своя переменная у `outer`.
const CHAIN: &str = "out sum: u8 at 0x2000;\nvar ticks: u8 := 0;\nvar gain: u8 := 2;\n\
     fn hot() -> u8 { return ticks + 1; }\n\
     fn mid(v: u8) -> u8 { return v + hot(); }\n\
     fn outer(k: u8) -> u8 { return mid(k) * gain; }\n\
     start Run { always { ticks := ticks + 1; sum := outer(ticks); } ref Run; }\n";

/// **Контроль:** обе функции чистые - параметров состояния быть не должно.
const PURE: &str = "out sum: u8 at 0x2000;\nvar ticks: u8 := 0;\n\
     fn twice(v: u8) -> u8 { return v + v; }\n\
     fn outer(k: u8) -> u8 { return twice(k) + 1; }\n\
     start Run { always { ticks := ticks + 1; sum := outer(ticks); } ref Run; }\n";

fn out_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0505_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог вывода");
    dir
}

fn generate(tag: &str, source: &str) -> (PathBuf, String) {
    let dir = out_dir(tag);
    takt_lang::compile_to_st(
        "probe",
        source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .unwrap_or_else(|d| panic!("порождение ST ({tag}): {}", d.message));
    let text = std::fs::read_to_string(dir.join("probe.st")).expect("чтение вывода");
    (dir, text)
}

/// Тело POU по имени функции - от заголовка до `END_FUNCTION`.
fn function_text<'a>(text: &'a str, name: &str) -> &'a str {
    let head = format!("FUNCTION {name} :");
    let start = text
        .find(&head)
        .unwrap_or_else(|| panic!("в выводе нет функции {name}:\n{text}"));
    let rest = &text[start..];
    let end = rest.find("END_FUNCTION").expect("конец функции");
    &rest[..end]
}

/// Арбитр IEC: путь к `iec2c` и его библиотеке (`None` - арбитра нет).
fn arbiter() -> Option<(PathBuf, PathBuf)> {
    let prefix = std::env::var("IEC2C_PREFIX")
        .unwrap_or_else(|_| format!("{}/.local", std::env::var("HOME").unwrap_or_default()));
    let iec2c = PathBuf::from(&prefix).join("bin").join("iec2c");
    let lib = PathBuf::from(&prefix)
        .join("share")
        .join("matiec")
        .join("lib");
    (iec2c.is_file() && lib.join("ieclib.txt").is_file()).then_some((iec2c, lib))
}

/// Вызывающая функция объявляет то, что нужно вызываемой.
#[test]
fn caller_declares_callee_needs() {
    let (dir, text) = generate("nested", NESTED);
    let outer = function_text(&text, "Probe_outer");
    assert!(
        outer.contains("VAR_IN_OUT") && outer.contains("ticks : USINT;"),
        "вызывающая обязана объявить переменную вызываемой:\n{outer}"
    );
    assert!(
        outer.contains("Probe_hot(ticks)"),
        "аргумент вызова берётся из того же списка:\n{outer}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// Признак транзитивен, а не "на один шаг": цепочка из трёх функций.
#[test]
fn need_is_transitive_along_the_chain() {
    let (dir, text) = generate("chain", CHAIN);
    for (name, expected) in [
        ("Probe_mid", "ticks : USINT;"),
        ("Probe_outer", "ticks : USINT;"),
    ] {
        let body = function_text(&text, name);
        assert!(
            body.contains(expected),
            "{name} обязана объявить '{expected}':\n{body}"
        );
    }
    // Своя переменная вызывающей - на месте рядом с унаследованной.
    let outer = function_text(&text, "Probe_outer");
    assert!(
        outer.contains("gain : USINT;"),
        "собственная переменная не теряется:\n{outer}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// `iec2c` принимает вывод - главный тест класса.
#[test]
fn generated_st_is_accepted_by_the_arbiter() {
    let Some((iec2c, lib)) = arbiter() else {
        eprintln!("[ПРОПУСК] арбитра iec2c нет; форма вывода проверена отдельно");
        return;
    };
    for (tag, source) in [("acc_nested", NESTED), ("acc_chain", CHAIN)] {
        let (dir, _) = generate(tag, source);
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
        let text = String::from_utf8_lossy(&out.stderr).to_string()
            + &String::from_utf8_lossy(&out.stdout);
        assert!(
            !text.contains("error"),
            "iec2c обязан принять вывод ({tag}):\n{text}"
        );
        let _ = std::fs::remove_dir_all(&dir);
    }
}

/// **Контроль:** чистые функции параметров состояния не получают.
///
/// Без него правка читалась бы как "печатать `VAR_IN_OUT` всем подряд": лишняя секция -
/// это лишний аргумент у каждого вызова, и вывод, который читают люди.
#[test]
fn pure_functions_get_no_state_params() {
    let (dir, text) = generate("pure", PURE);
    for name in ["Probe_twice", "Probe_outer"] {
        let body = function_text(&text, name);
        assert!(
            !body.contains("VAR_IN_OUT"),
            "{name} состояния не касается — секции быть не должно:\n{body}"
        );
    }
    let _ = std::fs::remove_dir_all(&dir);
}
