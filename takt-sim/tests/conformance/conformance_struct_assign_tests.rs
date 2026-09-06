//! Сверка присваивания агрегата в теле.
//!
//! Сборка доказывает валидность; верность даёт только число. Поэлементная
//! запись, перепутавшая поля местами, компилируется прекрасно - а агрегат
//! **позиционный**, и порядок полей берётся у объявления.
//!
//! Наблюдаемых две: структура и массив. Массив - контрольный вход (он работал с ).

use std::path::Path;
use std::process::Command;

const FIXTURE: &str = "tests/data/eval/conformance_struct_assign.takt";
const UNIT: &str = "conformance_struct_assign";

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Значения `(sum, actl)` у эталона после первого такта.
fn simulator_values() -> (i128, i128) {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = takt_sim::build_unit(model).expect("построение Unit");
    let _ = unit.tick();
    let number = |name: &str| match unit.variable(name) {
        Some(takt_sim::Value::Number(v)) => v,
        other => panic!("порт '{name}' обязан быть числом, получено {other:?}"),
    };
    (number("o_sum"), number("o_ctl"))
}

/// Те же значения у порождённого C.
fn generated_c_values(dir: &Path) -> (i128, i128) {
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

static long sum_v = 0;
static long ctl_v = 0;

static void wr(ConformanceStructAssign_Out_NumericPort port, uint8_t index, int64_t v, void *ud) {{
    (void)index;
    (void)ud;
    if (port == CONFORMANCE_STRUCT_ASSIGN_PORT_O_SUM) {{
        sum_v = (long)v;
    }} else {{
        ctl_v = (long)v;
    }}
}}

int main(void) {{
    ConformanceStructAssign m = {{0}};
    m.write_numeric = wr;
    ConformanceStructAssign_init(&m);
    ConformanceStructAssign_tick(&m);
    printf("%ld %ld\n", sum_v, ctl_v);
    return 0;
}}
"#
    );
    let harness_path = dir.join("harness_agg.c");
    std::fs::write(&harness_path, harness).expect("запись харнесса");
    let bin = dir.join("agg_bin");
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
    assert_eq!(nums.len(), 2, "харнесс печатает два числа: {out}");
    (nums[0], nums[1])
}

/// Значения совпадают: `sum = 7` (3 + 4), контрольный массив - 11 (5 + 6).
#[test]
fn aggregate_assignment_matches_simulator_and_generated_c() {
    let reference = simulator_values();
    assert_eq!(
        reference,
        (7, 11),
        "эталон: поля структуры 3 и 4, элементы массива 5 и 6"
    );

    if !cc_available() {
        eprintln!("[ПРОПУСК] aggregate_assignment_matches_simulator_and_generated_c: cc не найден");
        return;
    }
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!(
            "takt_0340_conf_{}",
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
