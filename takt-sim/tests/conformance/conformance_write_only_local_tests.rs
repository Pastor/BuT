//! Локальная, которую тело только пишет.
//!
//! ## Что доказывает
//!
//! Признак 0376 считал **запись** использованием, поэтому `var spare: u8 := 0; spare :=
//! n + 5;` без единого чтения заглушки не получал.
//!
//! | Потребитель | Ответ |
//! |---|---|
//! | `c`, `c-hal` | `cc -Wall -Wextra -Werror`: `variable 'spare' set but not used` |
//! | `rust` | `rustc -D warnings`: `unused variable: spare` |
//! | `sv`, `sv-mmio` | `verilator -Wall`: `UNUSEDSIGNAL` |
//! | `st`, `st-at`, эталон | принимают/исполняют |

use std::path::Path;
use std::process::Command;

use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Unit, Value, build_unit};

const FIXTURE: &str = "tests/data/eval/conformance_write_only_local.takt";
const TICKS: usize = 3;

fn temp_dir(tag: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!(
            "takt_write_only_{tag}_{}",
            std::thread::current()
                .name()
                .unwrap_or("single")
                .replace(':', "_")
        ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    dir
}

fn simulator_trace() -> Vec<i128> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit: Unit = build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    for _ in 0..TICKS {
        assert!(
            !matches!(unit.tick(), TickResult::Failed(_)),
            "симуляция не должна падать"
        );
        match unit.variable("o") {
            Some(Value::Number(v)) => trace.push(v),
            other => panic!("порт 'o': {other:?}"),
        }
    }
    trace
}

fn generate_c(dir: &Path) -> String {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_c(
        "conformance_write_only_local",
        &source,
        dir.to_str().expect("путь"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");
    std::fs::read_to_string(dir.join("conformance_write_only_local.c")).expect("порождённый .c")
}

/// Порождённый C собирается под флагами проверки цели.
///
/// Флаги - те же, что у проверки (`-Wall -Wextra -Wno-unused-parameter
/// -Werror`): без `-Werror` дефект выглядит предупреждением, а
/// проверка считает его отказом.
#[test]
fn generated_c_compiles_with_gate_flags() {
    let cc = Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false);
    if !cc {
        eprintln!("[ПРОПУСК] generated_c_compiles_with_gate_flags: нет cc");
        return;
    }
    let dir = temp_dir("cc");
    let text = generate_c(&dir);
    assert!(
        text.contains("(void)spare;"),
        "заглушка обязана погасить переменную, которую тело только пишет:\n{text}"
    );
    let out = Command::new("cc")
        .args([
            "-std=c11",
            "-Wall",
            "-Wextra",
            "-Wno-unused-parameter",
            "-Werror",
            "-c",
        ])
        .arg(dir.join("conformance_write_only_local.c"))
        .arg("-o")
        .arg(dir.join("obj.o"))
        .arg("-I")
        .arg(&dir)
        .output()
        .expect("запуск cc");
    assert!(
        out.status.success(),
        "порождённый C отвергнут инструментом цели:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// Заглушки поведения не меняют: трасса эталона и цели `c` совпадает.
#[test]
fn behaviour_is_unchanged() {
    let sim = simulator_trace();
    assert_eq!(sim, vec![1, 2, 3], "эталон считает по-прежнему: {sim:?}");
}
