//! Сверка формы `model M = A | B { ... }` с целью `c`.
//!
//! Отдельный подмодуль по предмету: здесь реализация модели.
//! Помощники (`c_trace`, `simulate_trace`, `cc_available`) - из родителя.
//!
//! # Что доказывает
//!
//! Форма развёрнута в состояние **одинаково** для эталона и цели: если бы разворот
//! делался только в эталоне (как в правке, где разрешался
//! `implements` в поле), эталон считал бы, а цель отказывала - и сверка бы этого не
//! пережила.
//!
//! Тело накапливающее: проверяется и то, что `always` владельца исполняется, и то, что
//! **ровно раз за такт**.

use super::*;

const FIXTURE: &str = "tests/data/eval/conformance_model_implement.takt";

/// Трассы эталона и цели `c` совпадают потактово.
#[test]
fn model_implement_form_matches_generated_c() {
    let vars = ["n"];
    let sim = simulate_trace(FIXTURE, &vars);
    let expected: Vec<Vec<i128>> = (1..=TRACE_TICKS as i128).map(|n| vec![n]).collect();
    assert_eq!(
        sim, expected,
        "тело владельца обязано исполняться РОВНО раз за такт: пропуск дал бы \
         нули, двойное исполнение — 2,4,6…"
    );

    if !cc_available() {
        eprintln!("[ПРОПУСК] model_implement_form: `cc` не найден");
        return;
    }
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!(
            "takt_0199_conformance_{}",
            std::thread::current().name().unwrap_or("single")
        ));
    std::fs::create_dir_all(&dir).expect("каталог сборки");
    let c = c_trace(&dir, FIXTURE, "mimpl", "Mimpl", "entry", &vars);
    assert_eq!(
        sim, c,
        "форма обязана разворачиваться ОДИНАКОВО у эталона и цели:\n\
         симулятор={sim:?}\nC={c:?}"
    );
}
