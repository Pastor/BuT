//! предупреждение о неявной булевости доезжает до пользователя.
//!
//! Проверка `SE-037` (Ce11) существовала с давних пор, имела публичную обёртку
//! `implicit_bool_warnings` и десятки юнит-тестов - и **не была подключена** к единой
//! точке предупреждений (`collect_model_warnings`). Ни `taktc compile`, ни редактор не
//! печатали ни одного её сообщения: проверка считалась и выбрасывалась.
//!
//! # Что здесь ловится
//!
//! 1. **Доставка** - предупреждение видит и командная строка, и редактор.
//! 2. **Тишина на законном** - `&`/`|` над булевыми операндами, `after`, доступ
//!    к одному биту предупреждения не дают. Замер до уточнения: 51 срабатывание
//!    на корпусе, документе и исправлениетурах, все ложные.
//! 3. **Настоящее срабатывание сохранено** - число в позиции условия ловится.
//! 4. **Корпус молчит** - иначе предупреждение, которое всегда горит, перестают
//!    читать.

use std::path::{Path, PathBuf};
use std::process::Command;

/// Условие - переменная `[bit;8]`: настоящее неявное приведение числа.
const NUMERIC_CONDITION: &str = "\
model M {
    var counter: [bit;8] := 0;
    start S {
        always {
            counter := counter + 1;
        }
        ref T: counter;
    }
    state T;
}
start Main = M;
";

/// Законные записи, которые проверка обязана пропускать: `&`/`|` над булевыми
/// операндами (иной конъюнкции условная грамматика не даёт), доступ к одному биту,
/// выдержка `after`.
const LEGITIMATE: &str = "\
model M {
    in ready: bit;
    in fault: bit;
    var flags: [bit;8] := 0;
    start S {
        ref T: ready & !fault;
        ref U: flags.3;
        ref V: after 5s;
    }
    state T;
    state U;
    state V;
}
start Main = M;
";

fn taktc() -> Command {
    Command::new(env!("CARGO_BIN_EXE_taktc"))
}

fn work_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("main")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0232_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог теста");
    dir
}

/// stderr прогона `taktc compile`.
fn compile_stderr(tag: &str, source: &str) -> String {
    let dir = work_dir(tag);
    let path = dir.join("probe.takt");
    std::fs::write(&path, source).expect("запись фикстуры");
    let out = taktc()
        .arg("compile")
        .arg(&path)
        .arg("-o")
        .arg(dir.join("out"))
        .output()
        .expect("запуск taktc compile");
    String::from_utf8_lossy(&out.stderr).into_owned()
}

/// Строки `SE-037` о неявной булевости (не о недетерминизме: код общий).
fn implicit_bool_lines(stderr: &str) -> Vec<&str> {
    stderr
        .lines()
        .filter(|l| l.contains("[SE-037]") && l.contains("условие перехода"))
        .collect()
}

/// **Доставка в командную строку.**
#[test]
fn compile_prints_implicit_bool_warning() {
    let stderr = compile_stderr("cli", NUMERIC_CONDITION);
    let lines = implicit_bool_lines(&stderr);

    assert_eq!(lines.len(), 1, "ожидалось одно предупреждение: {stderr:?}");
    assert!(
        lines[0].contains("переменная 'counter' типа [bit;8]"),
        "текст обязан называть причину: {:?}",
        lines[0]
    );
    assert!(
        lines[0].contains("probe.takt:"),
        "позиция обязана быть (фича 0228): {:?}",
        lines[0]
    );
}

/// **Доставка в редактор** - тем же каналом, что и в командную строку.
///
/// До 0232 у языкового сервера был **свой** список из двух проверок, а в единой точке
/// их восемь: редактор молча не показывал шесть видов предупреждений. Здесь
/// проверяется, что списка больше нет.
#[cfg(feature = "lsp")]
#[test]
fn editor_shows_implicit_bool_warning() {
    let diagnostics = takt_lang::lsp::collect_diagnostics(NUMERIC_CONDITION);
    let found: Vec<_> = diagnostics
        .iter()
        .filter(|d| {
            d.code == Some(lsp_types::NumberOrString::String("SE-037".to_string()))
                && d.message.contains("условие перехода")
        })
        .collect();

    assert_eq!(found.len(), 1, "редактор обязан показать: {diagnostics:?}");
    assert_eq!(
        found[0].severity,
        Some(lsp_types::DiagnosticSeverity::WARNING),
        "уровень — предупреждение"
    );
}

/// **Тишина на законных записях.**
///
/// Три класса, снятые замером: `&`/`|` над булевыми операндами, доступ к одному биту,
/// выдержка. Первый особенно важен: логических `&&`/`||` условная грамматика **не
/// принимает**, то есть `&` - единственная форма конъюнкции, и предупреждать о ней
/// значило бы предупреждать о единственной записи.
#[test]
fn legitimate_conditions_are_silent() {
    let stderr = compile_stderr("legit", LEGITIMATE);
    assert!(
        implicit_bool_lines(&stderr).is_empty(),
        "законные записи не должны давать SE-037: {stderr}"
    );
}

/// **Числовой операнд `&`/`|` по-прежнему ловится.**
///
/// Проверка узости послабления: булев операнд рядом с числовым не делает условие
/// булевым.
#[test]
fn numeric_operand_of_bitwise_still_warns() {
    let source = "\
model M {
    in ready: bit;
    var counter: [bit;8] := 0;
    start S {
        always {
            counter := counter + 1;
        }
        ref T: ready & counter;
    }
    state T;
}
start Main = M;
";
    let stderr = compile_stderr("mixed", source);
    assert_eq!(
        implicit_bool_lines(&stderr).len(),
        1,
        "числовой операнд обязан остаться нарушением: {stderr}"
    );
}

/// **Корпус молчит.**
///
/// Предупреждение, которое всегда горит, перестают читать, поэтому тест стоит здесь.
#[test]
fn corpus_and_book_are_silent() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("корень репозитория");
    let mut files = Vec::new();
    collect(&root.join("examples"), &mut files);
    collect(&root.join("book/src"), &mut files);
    assert!(files.len() >= 20, "корпус не найден: {}", files.len());

    let mut offenders = Vec::new();
    for file in &files {
        let source = std::fs::read_to_string(file).expect("исходник читается");
        let search = vec![
            root.join("examples").display().to_string(),
            root.join("examples/include").display().to_string(),
            file.parent().expect("каталог").display().to_string(),
        ];
        let Ok((ast, _)) = takt_lang::parse(&source, 0) else {
            continue;
        };
        let Ok(model) = takt_lang::semantic::tree::construct_model(&ast, None, &search) else {
            continue;
        };
        for w in takt_lang::semantic::warnings::collect_model_warnings(&ast, &model) {
            if w.code.as_deref() == Some("SE-037") && w.message.contains("условие перехода")
            {
                offenders.push(format!("{}: {}", file.display(), w.message));
            }
        }
    }

    assert!(
        offenders.is_empty(),
        "корпус и примеры документа обязаны молчать:\n{}",
        offenders.join("\n")
    );
}

fn collect(dir: &Path, out: &mut Vec<PathBuf>) {
    let entries = std::fs::read_dir(dir)
        .unwrap_or_else(|e| panic!("каталог {} не читается: {e}", dir.display()));
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            if path.file_name().is_some_and(|n| n == "generated") {
                continue;
            }
            collect(&path, out);
        } else if path.extension().is_some_and(|e| e == "takt") {
            out.push(path);
        }
    }
}
