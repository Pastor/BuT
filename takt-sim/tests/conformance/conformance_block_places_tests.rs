//! Потактовая сверка: места блоков `assembly` выровнены.

use std::path::Path;
use std::process::Command;

const FIXTURE: &str = "tests/data/eval/conformance_block_places.takt";
/// Тактов: два в `Run`, такт ухода и такт в терминальном состоянии.
const TICKS: usize = 4;

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Потактовая трасса порта `probe` у эталона.
fn simulator_trace() -> Vec<i128> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор фикстуры");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = takt_sim::build_unit(model).expect("построение Unit");
    let mut trace = Vec::with_capacity(TICKS);
    for _ in 0..TICKS {
        let _ = unit.tick();
        match unit.variable("probe") {
            Some(takt_sim::Value::Number(v)) => trace.push(v),
            other => panic!("порт 'probe' обязан быть числом, получено {other:?}"),
        }
    }
    trace
}

/// Та же трасса у порождённого C.
fn generated_c_trace(dir: &Path) -> Vec<i128> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_c(
        "conformance_block_places",
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");

    let harness = format!(
        r#"#include <stdio.h>
#include "conformance_block_places.h"

static long last;

static void write_numeric(ConformanceBlockPlaces_Out_NumericPort port, uint8_t index, int64_t val, void *ud) {{
    (void)index;
    (void)ud; (void)port;
    last = (long)val;
}}

int main(void) {{
    ConformanceBlockPlaces m = {{0}};
    ConformanceBlockPlaces_init(&m);
    m.write_numeric = write_numeric;
    m.userdata = 0;
    for (int i = 0; i < {TICKS}; i++) {{
        ConformanceBlockPlaces_tick(&m);
        printf("%ld\n", last);
    }}
    return 0;
}}
"#
    );
    std::fs::write(dir.join("harness.c"), harness).expect("харнесс");

    let bin = dir.join("bin");
    let compile = Command::new("cc")
        .args(["-std=c11", "-I"])
        .arg(dir)
        .arg(dir.join("conformance_block_places.c"))
        .arg(dir.join("harness.c"))
        .arg("-o")
        .arg(&bin)
        .output()
        .expect("cc");
    assert!(
        compile.status.success(),
        "порождённый C не компилируется:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск C");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter_map(|l| l.trim().parse().ok())
        .collect()
}

/// Предмет: вставка уровня модели переживает уход в терминальное состояние.
#[test]
fn model_level_assembly_outlives_the_state() {
    let reference = simulator_trace();
    // Числа названы явно: `43` на четвёртом такте - весь предмет. Модельный счётчик
    // дошёл до 4, счётчик состояния остался на 3.
    assert_eq!(
        reference,
        vec![10, 21, 32, 43],
        "предусловие: вставка модели исполняется и после ухода из `Run`"
    );

    if !cc_available() {
        eprintln!("[ПРОПУСК] model_level_assembly_outlives_the_state: `cc` не найден");
        return;
    }
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!(
            "takt_0518_places_{}",
            std::thread::current()
                .name()
                .unwrap_or("t")
                .replace(':', "_")
        ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    let generated = generated_c_trace(&dir);
    assert_eq!(
        reference, generated,
        "трассы обязаны совпадать: эталон {reference:?}, цель c {generated:?}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}
