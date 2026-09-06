//! Сверка индексации параметра-массива.
//!
//! Отказ приходил из **семантики** (`SE-003`), то есть одинаково у эталона и всех
//! восьми целей: язык принимал объявление, которое не работало. Значит расхождения
//! между потребителями не было - сверка здесь доказывает, что исправленное поведение
//! **верно**, а не только принимается.
//!
//! Контрольная функция без индексации параметра работала всегда.

use std::path::Path;
use std::process::Command;

const FIXTURE: &str = "tests/data/eval/conformance_array_param.takt";
const UNIT: &str = "conformance_array_param";

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Значения `(head, tail, ctl)` у эталона после первого такта.
fn simulator_values() -> (i128, i128, i128) {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = takt_sim::build_unit(model).expect("построение Unit");
    let _ = unit.tick();
    let number = |name: &str| match unit.variable(name) {
        Some(takt_sim::Value::Number(v)) => v,
        other => panic!("порт '{name}' обязан быть числом, получено {other:?}"),
    };
    (number("o_head"), number("o_tail"), number("o_ctl"))
}

/// Те же значения у порождённого C.
fn generated_c_values(dir: &Path) -> (i128, i128, i128) {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_c(
        UNIT,
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");

    let harness = format!(
        r#"#include <stdio.h>
#include "{UNIT}.h"

static long v[3] = {{0, 0, 0}};

static void wr(ConformanceArrayParam_Out_NumericPort port, uint8_t index, int64_t value, void *ud) {{
    (void)index;
    (void)ud;
    switch (port) {{
        case CONFORMANCE_ARRAY_PARAM_PORT_O_HEAD: v[0] = (long)value; break;
        case CONFORMANCE_ARRAY_PARAM_PORT_O_TAIL: v[1] = (long)value; break;
        default: v[2] = (long)value; break;
    }}
}}

int main(void) {{
    ConformanceArrayParam m = {{0}};
    m.write_numeric = wr;
    ConformanceArrayParam_init(&m);
    ConformanceArrayParam_tick(&m);
    printf("%ld %ld %ld\n", v[0], v[1], v[2]);
    return 0;
}}
"#
    );
    let harness_path = dir.join("harness_param.c");
    std::fs::write(&harness_path, harness).expect("запись харнесса");
    let bin = dir.join("param_bin");
    let compile = Command::new("cc")
        .args(["-std=c11", "-Wall", "-Wextra", "-Werror", "-I"])
        .arg(dir)
        .arg(dir.join(format!("{UNIT}.c")))
        .arg(&harness_path)
        .arg("-o")
        .arg(&bin)
        .output()
        .expect("запуск cc");
    assert!(
        compile.status.success(),
        "порождённый C не компилируется:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск собранного C");
    assert!(run.status.success(), "харнесс упал");
    let out = String::from_utf8_lossy(&run.stdout);
    let nums: Vec<i128> = out
        .split_whitespace()
        .filter_map(|t| t.parse().ok())
        .collect();
    assert_eq!(nums.len(), 3, "харнесс печатает три числа: {out}");
    (nums[0], nums[1], nums[2])
}

/// Значения совпадают: `first` даёт 7, `at_index` - 9, контроль - 3.
#[test]
fn array_parameter_index_matches_simulator_and_generated_c() {
    let reference = simulator_values();
    assert_eq!(
        reference,
        (7, 9, 3),
        "эталон: первый элемент 7, элемент по индексу 9, контроль 3"
    );

    if !cc_available() {
        eprintln!(
            "[ПРОПУСК] array_parameter_index_matches_simulator_and_generated_c: cc не найден"
        );
        return;
    }
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!(
            "takt_0346_{}",
            std::thread::current()
                .name()
                .unwrap_or("single")
                .replace(':', "_")
        ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    let generated = generated_c_values(&dir);
    assert_eq!(
        reference, generated,
        "трассы разошлись: эталон {reference:?}, цель c {generated:?}"
    );
}
