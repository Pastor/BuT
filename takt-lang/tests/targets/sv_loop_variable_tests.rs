//! Разряды переменной цикла у цели `sv` гасятся поглотителем.

use std::path::PathBuf;
use std::process::Command;
use takt_lang::generator::GenerateOptions;

/// Цикл в теле состояния.
const IN_STATE: &str = "var data: [u8; 4] := {1, 2, 3, 4};\n\
                        var acc: u8 := 0;\n\
                        out probe: u8 at 0x100;\n\
                        start Run {\n\
                            always {\n\
                                acc := 0;\n\
                                for var i: u8 := 0; i < 4; i := i + 1 {\n\
                                    acc := acc + data[i];\n\
                                }\n\
                                probe := acc;\n\
                            }\n\
                            ref Run;\n\
                        }\n";

/// Цикл в теле функции.
const IN_FUNCTION: &str = "fn total(w: [u8; 4]) -> u8 {\n\
                               var s: u8 := 0;\n\
                               for var i: u8 := 0; i < 4; i := i + 1 {\n\
                                   s := s + w[i];\n\
                               }\n\
                               return s;\n\
                           }\n\
                           var data: [u8; 4] := {1, 2, 3, 4};\n\
                           var acc: u8 := 0;\n\
                           out probe: u8 at 0x100;\n\
                           start Run {\n\
                               always { acc := total(data); probe := acc; }\n\
                               ref Run;\n\
                           }\n";

fn build_dir(tag: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!(
        "takt_0425_{tag}_{}_{}",
        std::process::id(),
        std::thread::current()
            .name()
            .unwrap_or("t")
            .replace(':', "_")
    ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    dir
}

fn generate(tag: &str, source: &str) -> (PathBuf, String) {
    let dir = build_dir(tag);
    takt_lang::compile_to_sv(
        tag,
        source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение SystemVerilog");
    let text = std::fs::read_to_string(dir.join(format!("{tag}.sv"))).expect("чтение вывода");
    (dir, text)
}

/// **T1.** Поглотитель печатается в обоих местах.
#[test]
fn loop_variable_gets_a_sink() {
    for (tag, src) in [("state", IN_STATE), ("function", IN_FUNCTION)] {
        let (dir, text) = generate(tag, src);
        assert!(
            text.contains("_unused_i") && text.contains("_unused_i = &{1'b0, i};"),
            "переменная цикла обязана получить поглотитель ({tag}):\n{text}"
        );
        let _ = std::fs::remove_dir_all(&dir);
    }
}

/// **T2.** Вывод принимается `verilator -Wall` - флагами проверки цели.
#[test]
fn generated_sv_passes_lint() {
    let available = Command::new("verilator")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false);
    if !available {
        eprintln!("[ПРОПУСК] generated_sv_passes_lint: verilator не найден");
        return;
    }
    for (tag, src) in [("lint_state", IN_STATE), ("lint_function", IN_FUNCTION)] {
        let (dir, _) = generate(tag, src);
        let out = Command::new("verilator")
            .current_dir(&dir)
            .args(["--lint-only", "-Wall", &format!("{tag}.sv")])
            .output()
            .expect("запуск verilator");
        assert!(
            out.status.success(),
            "verilator обязан принять вывод ({tag}):\n{}",
            String::from_utf8_lossy(&out.stderr)
        );
        let _ = std::fs::remove_dir_all(&dir);
    }
}

/// **Контроль:** тип переменной цикла не сужается - его задал автор.
///
/// Сужение объявления сменило бы семантику переполнения: поэтому гасятся разряды, а не
/// меняется тип.
#[test]
fn loop_variable_keeps_declared_width() {
    let (dir, text) = generate("width", IN_STATE);
    assert!(
        text.contains("automatic logic [7:0] i;"),
        "объявление обязано сохранить ширину типа автора:\n{text}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}
