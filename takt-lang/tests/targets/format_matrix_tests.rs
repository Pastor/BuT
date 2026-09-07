//! Сплошной перебор форматтера по корпусу матрицы.
//!
//! # Что доказывает набор
//!
//! Форматтер печатает **каждый** узел АСД, и его канон обязан быть **идемпотентным** и
//! **смыслосохраняющим**. Проверялось это корпусом `examples/` - витриной языка, где не
//! всякое сочетание встречается. Здесь входы берутся у генератора матрицы
//! ([`matrix_probes`](super::matrix_probes)): те же формы реализации, объявления,
//! порты, формулы, импорты, параметры, адреса и время, что проверяются у целей.
//!
//! Три свойства на каждый вход:
//!
//! 1. **`fmt` не отказывает** - узел, которого он не печатает, даёт `FM-001`;
//! 2. **идемпотентность**: `fmt(fmt(x)) == fmt(x)`;
//! 3. **смысл сохранён**: цель `c` порождает из отформатированного файла
//!    **байт в байт** то же, что из исходного.
//!
//! Третье свойство - главное. Формат - форма, а не смысл: разойдись они, `fmt` тихо
//! менял бы поведение прошивки, и ни один линтер этого не увидел бы.

use std::path::{Path, PathBuf};
use std::process::Command;

use super::matrix_probes::{Touch, case_name, cases, extra_flags, library_files, source};

fn taktc() -> Command {
    Command::new(env!("CARGO_BIN_EXE_taktc"))
}

/// Уникальный по тесту каталог.
fn work_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("main")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0462_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог теста");
    dir
}

/// Форматирует файл на месте; отдаёт `Err(stderr)`, если `fmt` отказал.
fn format_file(path: &Path) -> Result<(), String> {
    let out = taktc()
        .arg("fmt")
        .arg(path)
        .output()
        .expect("запуск taktc fmt");
    if out.status.success() {
        Ok(())
    } else {
        Err(String::from_utf8_lossy(&out.stderr).into_owned())
    }
}

/// Компилирует файл целью `c`; отдаёт текст вывода либо `None`, если цель отказала
/// (законные границы таблицы целей - не предмет этого набора).
fn emit_c(dir: &Path, input: &Path, touch: Touch, out_name: &str) -> Option<String> {
    let out_dir = dir.join(out_name);
    let ok = taktc()
        .arg("compile")
        .args(["-t", "c"])
        .args(
            extra_flags(touch)
                .into_iter()
                .map(|flag| flag.replace("{dir}", &dir.display().to_string())),
        )
        .arg(input)
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("запуск taktc compile")
        .status
        .success();
    ok.then(|| std::fs::read_to_string(out_dir.join("probe.c")).unwrap_or_default())
}

/// Перебор: форматтер печатает всякий вход, идемпотентен и не меняет смысла.
#[test]
fn formatter_is_total_idempotent_and_meaning_preserving() {
    let all = cases();
    let mut failures: Vec<String> = Vec::new();
    for (shape, touch, kind) in &all {
        let name = case_name(*shape, *touch, *kind);
        let dir = work_dir(&name);
        let input = dir.join("probe.takt");
        std::fs::write(&input, source(*shape, *touch, *kind)).expect("запись пробы");
        for file in library_files(*touch) {
            std::fs::write(dir.join(file.name), file.text).expect("запись библиотеки");
        }

        // Смысл "до": вывод цели из исходного файла.
        let before = emit_c(&dir, &input, *touch, "before");

        if let Err(err) = format_file(&input) {
            failures.push(format!("{name}: fmt отказал:\n{err}"));
            continue;
        }
        let once = std::fs::read_to_string(&input).expect("файл после fmt");

        // Идемпотентность: второй прогон ничего не меняет.
        if let Err(err) = format_file(&input) {
            failures.push(format!("{name}: повторный fmt отказал:\n{err}"));
            continue;
        }
        let twice = std::fs::read_to_string(&input).expect("файл после второго fmt");
        if once != twice {
            failures.push(format!("{name}: fmt не идемпотентен"));
            continue;
        }

        // Смысл "после": вывод цели обязан совпасть байт в байт.
        let after = emit_c(&dir, &input, *touch, "after");
        if before != after {
            failures.push(format!(
                "{name}: форматирование изменило вывод цели — формат затронул смысл"
            ));
        }
    }
    assert!(
        failures.is_empty(),
        "форматтер разошёлся с ожиданием в {} случаях из {}:\n{}",
        failures.len(),
        all.len(),
        failures.join("\n")
    );
}

/// `--check` на отформатированном файле молчит, на неотформатированном - нет.
#[test]
fn check_mode_reports_only_unformatted() {
    let dir = work_dir("check");
    let input = dir.join("probe.takt");
    // Неканоничная раскладка: два оператора в строке, отступ в два пробела.
    std::fs::write(
        &input,
        "model Wrap {\n  var k: u8 := 0;\n  out led: u8;\n  start Go {\n    always { k := k + 1; led := k; }\n    ref Go: k < 5;\n  }\n}\nstart Main = Wrap;\n",
    )
    .expect("запись пробы");

    let unformatted = taktc()
        .arg("fmt")
        .arg("--check")
        .arg(&input)
        .output()
        .expect("запуск taktc fmt --check");
    assert!(
        !unformatted.status.success(),
        "`--check` промолчал на неотформатированном файле"
    );

    format_file(&input).expect("fmt обязан отформатировать вход");
    let formatted = taktc()
        .arg("fmt")
        .arg("--check")
        .arg(&input)
        .output()
        .expect("запуск taktc fmt --check");
    assert!(
        formatted.status.success(),
        "`--check` пожаловался на канон:\n{}",
        String::from_utf8_lossy(&formatted.stderr)
    );
}
