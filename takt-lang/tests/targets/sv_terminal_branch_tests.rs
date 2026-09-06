//! Терминальная ветвь `case` у цели `sv` не задваивается.

use std::path::PathBuf;
use std::process::Command;
use takt_lang::generator::GenerateOptions;

/// Модель с состоянием, названным `End`.
const SRC: &str = "var flag: u8 := 0;\n\
     start Go {\n    always { flag := flag + 1; }\n    ref End: flag > 2;\n}\n\
     state End {\n    enter { flag := 0; }\n}\n";

fn out_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0412_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог вывода");
    dir
}

fn generate(tag: &str, src: &str) -> (PathBuf, String) {
    let dir = out_dir(tag);
    takt_lang::compile_to_sv(
        tag,
        src,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение SV");
    let text = std::fs::read_to_string(dir.join(format!("{tag}.sv"))).expect("чтение вывода");
    (dir, text)
}

fn verilator_available() -> bool {
    Command::new("verilator")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Предмет: вариант терминального состояния встречается в `case` **один** раз.
#[test]
fn terminal_branch_is_printed_once() {
    let (_, text) = generate("sv0412", SRC);
    let count = text
        .lines()
        .filter(|l| l.trim_start().starts_with("SV0412_END:"))
        .count();
    assert_eq!(
        count, 1,
        "ветвь терминального состояния обязана быть одна:\n{text}"
    );
}

/// **Контроль:** у модели без состояния `End` терминальная ветвь печатается.
///
/// Без него правка читалась бы как "терминальной ветви больше нет", и `verilator`
/// ответил бы `CASEINCOMPLETE` - обмен одного отказа на другой.
#[test]
fn terminal_branch_is_still_printed_when_absent() {
    let src = "var flag: u8 := 0;\n\
         start Go {\n    always { flag := flag + 1; }\n    ref Done: flag > 2;\n}\n\
         state Done { }\n";
    let (_, text) = generate("sv0412c", src);
    assert!(
        text.contains("SV0412C_END: begin end"),
        "терминальная ветвь обязана печататься:\n{text}"
    );
}

/// Линт цели принимает вывод - он и был арбитром.
#[test]
fn generated_sv_passes_the_lint() {
    if !verilator_available() {
        eprintln!("[ПРОПУСК] `verilator` не найден; текст вывода уже проверен");
        return;
    }
    let (dir, _) = generate("sv0412t", SRC);
    let out = Command::new("verilator")
        .current_dir(&dir)
        .args(["--lint-only", "-Wall", "sv0412t.sv"])
        .output()
        .expect("запуск verilator");
    assert!(
        out.status.success(),
        "verilator обязан принять вывод:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
}
