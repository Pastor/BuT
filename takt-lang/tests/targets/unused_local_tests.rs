//! Неиспользуемая локальная переменная гасится заглушкой.

use std::process::Command;
use takt_lang::generator::GenerateOptions;

/// Локальная объявлена и ни разу не упомянута.
const DEAD: &str = "var n: u8 := 0; out o: u8 at 0x100; \
                    start Run { always { n := n + 1; var spare: u8 := n + 5; o := n; } \
                    ref Done: n > 3; } state Done { }";

/// **Контрпример:** локальная, которую тело читает.
const LIVE: &str = "var n: u8 := 0; out o: u8 at 0x100; \
                    start Run { always { n := n + 1; var used: u8 := n + 5; o := used; } \
                    ref Done: n > 3; } state Done { }";

fn generate(tag: &str, target: &str, source: &str) -> (std::path::PathBuf, String) {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0376_{tag}_{target}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    let path = dir.to_str().expect("путь");
    let opts = GenerateOptions::default();
    match target {
        "c" => takt_lang::compile_to_c("probe", source, path, &[], &opts).map(|_| ()),
        _ => takt_lang::compile_to_rust("probe", source, path, &[], &opts).map(|_| ()),
    }
    .unwrap_or_else(|e| panic!("порождение для '{target}': {e:?}"));
    let ext = if target == "c" { "c" } else { "rs" };
    let text = std::fs::read_to_string(dir.join(format!("probe.{ext}"))).expect("чтение");
    (dir, text)
}

/// Заглушка печатается там, где переменная не используется.
#[test]
fn unused_local_gets_a_stub() {
    let (_d, c) = generate("dead", "c", DEAD);
    assert!(
        c.contains("(void)spare;"),
        "без заглушки `cc -Werror` отвечает -Wunused-variable:\n{c}"
    );
    let (_d, rust) = generate("dead", "rust", DEAD);
    assert!(
        rust.contains("let _ = spare;"),
        "без заглушки `rustc -D warnings` отвечает «unused variable»:\n{rust}"
    );
}

/// **Контрпример:** используемая локальная заглушки не получает.
///
/// Без него правило читалось бы как "гасим любое локальное объявление", а лишняя
/// заглушка - мусор в выводе (и `clippy::no_effect` в перспективе).
#[test]
fn used_local_gets_no_stub() {
    let (_d, c) = generate("live", "c", LIVE);
    assert!(!c.contains("(void)used;"), "заглушка здесь не нужна:\n{c}");
    let (_d, rust) = generate("live", "rust", LIVE);
    assert!(
        !rust.contains("let _ = used;"),
        "заглушка здесь не нужна:\n{rust}"
    );
}

/// Вывод принимается Проверками целей - теми же флагами, что в предкоммите.
///
/// Именно здесь класс и жил: `taktc` возвращал ноль, а собрать вывод было нельзя.
/// Корпус его не покрывает - неиспользуемых локальных в `examples/` нет ни одной.
#[test]
fn generated_output_passes_target_gates() {
    let cc_available = Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false);
    if cc_available {
        let (dir, _) = generate("gate", "c", DEAD);
        let out = Command::new("cc")
            .args(["-std=c11", "-Wall", "-Wextra", "-Werror", "-c"])
            .arg(dir.join("probe.c"))
            .args(["-o", "/dev/null"])
            .output()
            .expect("запуск cc");
        assert!(
            out.status.success(),
            "вывод цели `c` обязан собираться флагами гейта:\n{}",
            String::from_utf8_lossy(&out.stderr)
        );
    } else {
        eprintln!("[ПРОПУСК] gate `c`: нет cc");
    }

    let clippy_available = Command::new("clippy-driver")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false);
    if !clippy_available {
        eprintln!("[ПРОПУСК] gate `rust`: нет clippy-driver");
        return;
    }
    let (dir, _) = generate("gate", "rust", DEAD);
    let out = Command::new("clippy-driver")
        .args(["--edition", "2021", "--crate-type=lib", "-D", "warnings"])
        .arg(dir.join("probe.rs"))
        .arg("--out-dir")
        .arg(dir.join("out"))
        .output()
        .expect("запуск clippy-driver");
    assert!(
        out.status.success(),
        "вывод цели `rust` обязан приниматься гейтом:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
}
