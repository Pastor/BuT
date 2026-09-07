//! результат компиляции печатает одна функция - на все цели.
//!
//! # Что здесь ловится
//!
//! 1. **`--verbose` действует у всех целей** - канонический (абсолютный) путь
//!    входа. Проверяется на обеих ветвях прежнего деления: `st` (была
//!    "простая") и `st-at` (была "адресная").
//! 2. **Контрпример:** без флага печатается путь **как передан**. Без него
//!    "канонический путь" ловился бы и у сломанной реализации, печатающей его
//!    всегда.
//! 3. **`--quiet` глушит результат** у обеих ветвей.
//! 4. **Форма одна:** путь выхода печатается со слэшем (это каталог) в обоих
//!    режимах - прежняя verbose-ветвь теряла слэш без причины.

use std::path::PathBuf;
use std::process::Command;

/// Модель с адресованными портами: годится и для `st`, и для `st-at`.
const MODEL: &str = "\
model M {
    out lamp: bit at 0x40000000:0;
    in button: bit at 0x40000004:0;
    start S {
        always {
            lamp := button;
        }
    }
}
start Main = M;
";

fn taktc() -> Command {
    Command::new(env!("CARGO_BIN_EXE_taktc"))
}

/// Уникальный по тесту каталог (: тесты идут параллельно; двоеточие из имени потока
/// вычищается - ).
fn work_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("main")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0283_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание каталога");
    dir
}

/// Компилирует модель целью `target` с добавочными флагами; возвращает stderr.
fn compile(tag: &str, target: &str, extra: &[&str]) -> (String, PathBuf) {
    let dir = work_dir(tag);
    let input = dir.join("probe.takt");
    std::fs::write(&input, MODEL).expect("запись модели");
    let out = dir.join("out");
    let mut cmd = taktc();
    cmd.arg("compile").arg("-t").arg(target);
    for flag in extra {
        cmd.arg(flag);
    }
    let result = cmd
        .arg(&input)
        .arg("-o")
        .arg(&out)
        .current_dir(&dir)
        .output()
        .expect("запуск taktc");
    assert!(
        result.status.success(),
        "компиляция целью {target} провалилась:\n{}",
        String::from_utf8_lossy(&result.stderr)
    );
    (String::from_utf8_lossy(&result.stderr).to_string(), input)
}

/// `--verbose` даёт канонический путь входа у цели, которая была "простой".
#[test]
fn verbose_prints_canonical_input_for_simple_target() {
    let (stderr, input) = compile("simple_verbose", "st", &["--verbose"]);
    let canonical = std::fs::canonicalize(&input).expect("канонический путь");
    assert!(
        stderr.contains(&canonical.display().to_string()),
        "ожидался канонический путь входа:\n{stderr}"
    );
}

/// `--verbose` даёт канонический путь входа и у адрес-потребляющей цели.
///
/// Это и есть предмет: без правки `st-at` флаг игнорирует.
#[test]
fn verbose_prints_canonical_input_for_address_target() {
    let (stderr, input) = compile("address_verbose", "st-at", &["--verbose"]);
    let canonical = std::fs::canonicalize(&input).expect("канонический путь");
    assert!(
        stderr.contains(&canonical.display().to_string()),
        "`--verbose` обязан действовать и у адрес-потребляющих целей:\n{stderr}"
    );
}

/// **Контрпример:** без флага печатается путь как передан, а не канонический.
#[test]
fn without_verbose_input_path_is_as_given() {
    let (stderr, input) = compile("address_plain", "st-at", &[]);
    let canonical = std::fs::canonicalize(&input).expect("канонический путь");
    assert!(
        !stderr.contains(&canonical.display().to_string()),
        "без `--verbose` канонический путь печатать не за что:\n{stderr}"
    );
    assert!(
        stderr.contains("Скомпилировано:"),
        "результат обязан быть напечатан:\n{stderr}"
    );
}

/// `--quiet` глушит результат у обеих ветвей прежнего деления.
#[test]
fn quiet_suppresses_result_for_both_kinds() {
    for (tag, target) in [("quiet_simple", "st"), ("quiet_address", "st-at")] {
        let (stderr, _) = compile(tag, target, &["--quiet"]);
        assert!(
            !stderr.contains("Скомпилировано:"),
            "`--quiet` обязан глушить результат цели {target}:\n{stderr}"
        );
    }
}

/// Форма одна: путь выхода печатается со слэшем в обоих режимах.
#[test]
fn output_path_is_printed_with_slash_in_both_modes() {
    for (tag, extra) in [
        ("slash_plain", &[][..]),
        ("slash_verbose", &["--verbose"][..]),
    ] {
        let (stderr, _) = compile(tag, "st", extra);
        let line = stderr
            .lines()
            .find(|l| l.contains("Скомпилировано:"))
            .unwrap_or_else(|| panic!("нет строки результата:\n{stderr}"));
        assert!(
            line.contains("/out/ ("),
            "путь выхода — каталог и печатается со слэшем:\n{line}"
        );
    }
}
