//! Шесть форм цикла - потактовая сверка значений.
//!
//! # Что доказывает набор
//!
//! Перебор целей отвечает на вопрос "принял ли вывод инструмент". Для цикла этого мало:
//! потерянная итерация, лишний проход или проигнорированный `continue` дают
//! **валидный** код с другим числом шагов. Отличает исходы только значение.
//!
//! Приращения форм разные (3, 3, 3, 4, 3, 1): при одинаковых потеря целого цикла
//! сошлась бы в сумме и осталась незамеченной.
//!
//! Цель `sv` в набор не входит: четыре из шести форм она отвергает законно - цикл там
//! обязан разворачиваться в схему. Её единственную пригодную форму (`for` со
//! статическими границами) проверяет своя сверка разворота.

use std::path::Path;
use std::process::Command;

const FIXTURE_DIR: &str = "tests/data/eval";
const MODEL: &str = "loop_forms.takt";
const UNIT: &str = "loop_forms";
/// Один такт: все шесть циклов исполняются в нём.
const TICKS: usize = 1;
/// Сумма приращений: 3 + 3 + 3 + 4 + 3 + 1.
const EXPECTED: i128 = 17;

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

fn source() -> String {
    std::fs::read_to_string(Path::new(FIXTURE_DIR).join(MODEL)).expect("фикстура читается")
}

/// Трасса порта `beat` у эталона-симулятора.
fn simulator_trace() -> Vec<i128> {
    let text = source();
    let (ast, _) = takt_lang::parse(&text, 0).expect("разбор фикстуры");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[FIXTURE_DIR.to_string()])
        .expect("семантика фикстуры");
    let mut unit = takt_sim::build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    for _ in 0..TICKS {
        let _ = unit.tick();
        match unit.variable("beat") {
            Some(takt_sim::Value::Number(v)) => trace.push(v),
            other => panic!("порт 'beat' обязан быть числом, получено {other:?}"),
        }
    }
    trace
}

/// Уникальный по тесту каталог.
fn work_dir(tag: &str) -> std::path::PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("main")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0477_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог теста");
    dir
}

/// Трасса порта `beat` у порождённого C.
fn generated_c_trace(dir: &Path) -> Vec<i128> {
    let text = source();
    takt_lang::compile_to_c(
        UNIT,
        &text,
        dir.to_str().expect("путь в UTF-8"),
        &[FIXTURE_DIR.to_string()],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");

    let harness = format!(
        r#"#include <stdio.h>
#include "{UNIT}.h"

static long last_beat = 0;

static void wr_num(LoopForms_Out_NumericPort port, uint8_t index, int64_t v, void *ud) {{
    (void)index;
    (void)port;
    (void)ud;
    last_beat = (long)v;
}}

int main(void) {{
    LoopForms m = {{0}};
    m.write_numeric = wr_num;
    LoopForms_init(&m);
    for (int i = 0; i < {TICKS}; i++) {{
        LoopForms_tick(&m);
        printf("%ld\n", last_beat);
    }}
    return 0;
}}
"#
    );
    let harness_path = dir.join("harness_loops.c");
    std::fs::write(&harness_path, harness).expect("запись харнесса");

    let bin = dir.join("loops_bin");
    let compile = Command::new("cc")
        .args(["-std=c11", "-Wall", "-Werror", "-I"])
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
    assert!(run.status.success(), "собранный C завершился с ошибкой");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .map(|l| l.trim().parse::<i128>().expect("число в строке вывода"))
        .collect()
}

/// **Шесть форм цикла считают одинаково у эталона и у порождённого C.**
#[test]
fn loop_forms_match_simulator_and_generated_c() {
    let sim = simulator_trace();
    assert_eq!(
        sim,
        vec![EXPECTED],
        "эталон: сумма приращений шести форм цикла"
    );
    if !cc_available() {
        eprintln!("cc недоступен — сверка с целью `c` пропущена");
        return;
    }
    let dir = work_dir("c");
    assert_eq!(
        sim,
        generated_c_trace(&dir),
        "эталон и порождённый C разошлись на формах цикла"
    );
}
