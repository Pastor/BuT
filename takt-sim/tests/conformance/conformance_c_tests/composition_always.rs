//! Сверка model-level `always` у модели-**композиции** с целью `c`.
//!
//! Вынесено подмодулем: `conformance_c_tests.rs` упирается в лимит размера модуля, а
//! правило требует делить по логике, а не расширять реестр долга. Помощники (`c_trace`,
//! `simulate_trace`, `cc_available`) - из родителя.

use super::*;

const FIXTURE: &str = "tests/data/eval/conformance_composition_always.takt";

/// **R1/R2/R5:** тело `always` модели-композиции исполняется ровно
/// раз за такт и потактово совпадает с целью `c`.
#[test]
fn composition_model_always_matches_generated_c() {
    let vars = ["n"];
    let sim = simulate_trace(FIXTURE, &vars);
    // Модель не завершается (`ref Count;`), поэтому трасса - ровно TRACE_TICKS тактов,
    // и на каждом счётчик обязан вырасти на единицу.
    let expected: Vec<Vec<i128>> = (1..=TRACE_TICKS as i128).map(|n| vec![n]).collect();
    assert_eq!(
        sim, expected,
        "эталон обязан исполнять тело владельца-композиции РОВНО раз за такт: \
         пропуск дал бы 0,0,0… (дефект 0194), двойное исполнение — 2,4,6… \
         (дефект 0181-01)"
    );

    if !cc_available() {
        eprintln!("[ПРОПУСК] composition_model_always: `cc` не найден");
        return;
    }
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!(
            "takt_0194_conformance_{}",
            std::thread::current().name().unwrap_or("single")
        ));
    std::fs::create_dir_all(&dir).expect("каталог сборки");
    let c = c_trace(&dir, FIXTURE, "compalw", "Compalw", "entry", &vars);
    assert_eq!(
        sim, c,
        "потактовые трассы эталона и порождённого C обязаны совпадать: контракт \
         0083 («model-level `always` — каждый такт») цель `c` соблюдала всегда, \
         эталон — только у модели со своими состояниями.\nсимулятор={sim:?}\nC={c:?}"
    );
}
