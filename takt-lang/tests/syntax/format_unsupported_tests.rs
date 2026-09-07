//! отказ форматтера - диагностика с позицией. Обновлено.
//!
//! Со строкой вместо диагностики `FormatError::Unsupported` отказ выглядит так:
//!
//! ```text
//! Ошибка форматирования 'f1.takt': печать узла 'Formula' пока не поддерживается форматтером
//! ```
//!
//! Отказ форматтера строится общей формой и несёт позицию узла; `Debug`-дампа
//! внутренней структуры в сообщении быть не должно.

use std::path::{Path, PathBuf};
use std::process::Command;

/// Элемент `formula` - прежний пример непечатаемого узла.
const FORMULA_ELEMENT: &str = "\
model M {
    formula \"ltl\" { }
    start S { always { } }
}
start Main = M;
";

/// Оператор `assembly` в теле блока - та ветвь, что печатала `Debug`-дамп.
const ASSEMBLY_STATEMENT: &str = "\
model M {
    start S {
        always {
            assembly \"x86\" { }
        }
    }
}
start Main = M;
";

/// Файл с синтаксической ошибкой - единственная достижимая ветвь отказа.
const PARSE_ERROR: &str = "\
model M {
    var x u8 := 1;
    start S { always { } }
}
start Main = M;
";

fn taktc() -> Command {
    Command::new(env!("CARGO_BIN_EXE_taktc"))
}

fn work_dir(tag: &str) -> PathBuf {
    // Каталог уникален по тесту: прогон параллельный, а помощник начинает с очистки.
    // Имя потока несёт `::` - его чистим.
    let thread = std::thread::current()
        .name()
        .unwrap_or("unnamed")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_fmt_unsupported_{thread}_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание временного каталога");
    dir
}

fn fixture(tag: &str, source: &str) -> PathBuf {
    let path = work_dir(tag).join("probe.takt");
    std::fs::write(&path, source).expect("запись пробы");
    path
}

fn fmt_check(path: &Path) -> (String, i32) {
    let out = taktc()
        .arg("fmt")
        .arg("--check")
        .arg(path)
        .output()
        .expect("запуск taktc fmt --check");
    (
        String::from_utf8_lossy(&out.stderr).into_owned(),
        out.status.code().unwrap_or(-1),
    )
}

/// **Контроль:** прежние примеры непечатаемых узлов теперь печатаются.
///
/// Без него набор молча превратился бы в кладбище: тесты, требовавшие отказа, сняты, а
/// утверждение, которое их заменило, нигде бы не проверялось.
#[test]
fn formula_and_assembly_are_printed_now() {
    for (tag, source) in [
        ("formula_ok", FORMULA_ELEMENT),
        ("assembly_ok", ASSEMBLY_STATEMENT),
    ] {
        let printed = takt_lang::format::format_source(source)
            .unwrap_or_else(|e| panic!("{tag}: печать обязана удаваться: {e:?}"));
        assert!(!printed.is_empty(), "{tag}: вывод пуст");
        // Отказ ушёл насовсем: результат разбирается обратно.
        takt_lang::parse(&printed, 0).unwrap_or_else(|e| panic!("{tag}: {e:?}"));
    }
}

/// **Форма печати диагностики:** путь, строка, колонка, код - на своих местах.
///
/// Проверяется на ветви `Parse`: она единственная достижимая. Формат общий у обеих
/// ветвей `FormatError` - это и было предметом.
#[test]
fn refusal_shape_carries_path_line_column_and_code() {
    let (stderr, code) = fmt_check(&fixture("shape_p", PARSE_ERROR));

    let shape = |line: &str| -> Option<(String, u32, u32, String)> {
        let (head, tail) = line.split_once(": Ошибка компиляции [")?;
        let diagnostic = tail.split(']').next()?.to_string();
        let mut parts = head.rsplitn(3, ':');
        let column = parts.next()?.parse().ok()?;
        let line_no = parts.next()?.parse().ok()?;
        Some((parts.next()?.to_string(), line_no, column, diagnostic))
    };

    let parsed = stderr
        .lines()
        .find_map(shape)
        .unwrap_or_else(|| panic!("ошибка разбора не в общей форме: {stderr:?}"));

    assert!(parsed.0.ends_with("probe.takt"), "путь: {parsed:?}");
    assert_eq!(parsed.3, "SY-002", "код: {parsed:?}");
    assert!(
        parsed.1 > 0 && parsed.2 > 0,
        "позиции ненулевые: {parsed:?}"
    );
    assert_eq!(code, 1, "отказ — это код 1");
}

/// **`--stdin`:** код и текст есть, префикса пути нет - файла не существует.
#[test]
fn stdin_refusal_has_code_but_no_path_prefix() {
    use std::io::Write;
    use std::process::Stdio;

    let mut child = taktc()
        .arg("fmt")
        .arg("--stdin")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("запуск taktc fmt --stdin");
    child
        .stdin
        .as_mut()
        .expect("stdin")
        .write_all(PARSE_ERROR.as_bytes())
        .expect("запись в stdin");
    let out = child.wait_with_output().expect("ожидание taktc");
    let stderr = String::from_utf8_lossy(&out.stderr);

    assert!(
        stderr.contains("Ошибка компиляции [SY-002]"),
        "код обязан быть и без файла: {stderr:?}"
    );
    assert!(
        !stderr.contains(".takt:"),
        "пути у stdin нет — координаты выдумывать нельзя: {stderr:?}"
    );
    assert_eq!(out.status.code(), Some(1));
}

/// **Вызывающему достаётся структура, а не строка.**
///
/// Библиотечный уровень: у отказа есть код и позиция, то есть языковой сервер волен
/// показать его диагностикой в редакторе, а не только записать в журнал.
#[test]
fn library_refusal_carries_code_and_location() {
    let err = takt_lang::format::format_source(PARSE_ERROR)
        .expect_err("исходник не разбирается — обязан быть отказ");
    let takt_lang::format::FormatError::Parse(diagnostics) = err else {
        panic!("ожидалась ветвь Parse, получено: {err:?}");
    };
    let first = diagnostics.first().expect("диагностика есть");

    assert_eq!(first.code.as_deref(), Some("SY-002"));
    assert!(
        matches!(first.loc, takt_lang::diagnostics::Location::Source(0, _, _)),
        "позиция обязана указывать в разбираемый файл: {:?}",
        first.loc
    );
}
