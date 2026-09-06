//! Блоки, адресованные не самой модели: `formula` и `assembly`.
//!
//! ## Что доказывает набор
//!
//! Безымянная вставка - **операторы Takt**, и они исполняются: трасса `2 4 6` у эталона
//! и у цели `c` совпадает. Блок формул поведения не меняет и в выводе не появляется.
//! Трасса здесь обязательна: пропусти цель тело вставки, вывод остался бы валидным (его
//! принимает `cc`), а автомат считал бы другое - компиляция такого не видит.

use std::path::Path;
use std::process::Command;

use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Unit, Value, build_unit};

const FIXTURE: &str = "tests/data/eval/conformance_target_blocks.takt";
const UNIT: &str = "targetblocks";
const TICKS: usize = 3;
/// Значения порта `o` по тактам: безымянная вставка прибавляет по 2.
const EXPECTED: [i128; TICKS] = [2, 4, 6];

fn temp_dir(tag: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!(
            "takt_0484_{tag}_{}",
            std::thread::current()
                .name()
                .unwrap_or("single")
                .replace(':', "_")
        ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    dir
}

fn source() -> String {
    std::fs::read_to_string(FIXTURE).expect("фикстура читается")
}

fn simulator_trace() -> Vec<i128> {
    let (ast, _) = takt_lang::parse(&source(), 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit: Unit = build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    for _ in 0..TICKS {
        assert!(
            !matches!(unit.tick(), TickResult::Failed(_)),
            "прогон не обрывается"
        );
        match unit.variable("o") {
            Some(Value::Number(v)) => trace.push(v),
            other => panic!("порт 'o': {other:?}"),
        }
    }
    trace
}

fn generate_c(dir: &Path) -> String {
    takt_lang::compile_to_c(
        UNIT,
        &source(),
        dir.to_str().expect("путь"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");
    std::fs::read_to_string(dir.join(format!("{UNIT}.c"))).expect("порождённый .c")
}

/// Эталон исполняет тело безымянной вставки, а блок формул его не трогает.
#[test]
fn simulator_executes_unnamed_assembly() {
    assert_eq!(
        simulator_trace(),
        EXPECTED.to_vec(),
        "эталон обязан исполнить тело безымянной вставки"
    );
}

/// Цель `c` печатает тело вставки, не печатает формулу и считает то же самое.
#[test]
fn c_target_matches_simulator() {
    let cc = Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false);
    let dir = temp_dir("c");
    let text = generate_c(&dir);
    assert!(
        text.contains("level + 2"),
        "тело безымянной вставки обязано попасть в вывод:\n{text}"
    );
    assert!(
        !text.contains("holds"),
        "блок формул адресован внешнему анализатору — в выводе его быть не должно:\n{text}"
    );
    if !cc {
        eprintln!("[ПРОПУСК] c_target_matches_simulator: нет cc");
        return;
    }
    let harness = format!(
        r#"#include <stdio.h>
#include "{UNIT}.h"
static long long out;
static void on_num(Targetblocks_Out_NumericPort p, uint8_t index, int64_t v, void *u) {{
    (void)index;
    (void)p; (void)u; out = (long long)v;
}}
int main(void) {{
    Targetblocks m;
    Targetblocks_init(&m);
    m.write_numeric = on_num;
    m.userdata = 0;
    for (int i = 0; i < {TICKS}; i++) {{
        Targetblocks_tick(&m);
        printf("%lld\n", out);
    }}
    return 0;
}}
"#
    );
    std::fs::write(dir.join("harness.c"), harness).expect("харнесс");
    let bin = dir.join("bin");
    // Флаги - те же, что у проверки цели (0171/0220): без `-Werror` дефект выглядел бы
    // предупреждением, а проверка считает его отказом.
    let compile = Command::new("cc")
        .args([
            "-std=c11",
            "-Wall",
            "-Wextra",
            "-Wno-unused-parameter",
            "-Werror",
            "-o",
        ])
        .arg(&bin)
        .arg(dir.join("harness.c"))
        .arg(dir.join(format!("{UNIT}.c")))
        .arg("-I")
        .arg(&dir)
        .output()
        .expect("запуск cc");
    assert!(
        compile.status.success(),
        "порождённый C отвергнут инструментом цели:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск харнесса");
    let trace: Vec<i128> = String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter_map(|l| l.trim().parse::<i128>().ok())
        .collect();
    assert_eq!(
        trace,
        EXPECTED.to_vec(),
        "цель `c` обязана считать то же, что эталон"
    );
    let _ = std::fs::remove_dir_all(&dir);
}
