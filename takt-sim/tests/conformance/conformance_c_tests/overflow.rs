//! Сверка семантики переполнения беззнакового целого с целью `c`.
//!
//! Вынесено из `conformance_c_tests.rs` подмодулем: файл упирался в лимит размера
//! модуля, и правило требует делить по логике. Помощники (`c_trace`, `simulate_trace`,
//! `cc_available`) берутся из родителя через `use super::*`.

use super::*;

const OVERFLOW_FIXTURE: &str = "tests/data/eval/conformance_overflow.takt";

/// Обёртка `u8` совпадает у симулятора и порождённого C.
///
/// Почему это не "и так очевидно": в C обёртка беззнакового - **определённое**
/// поведение (C11 6.2.5p9), и цель `c` получает его даром, потому что печатает
/// `uint8_t`. Сверка нужна как **пиннинг правила**: если эталон-симулятор когда-нибудь
/// начнёт, скажем, диагностировать переполнение вместо обёртки, расхождение с C
/// поймается здесь, а не у пользователя.
#[test]
fn unsigned_overflow_wraps_like_generated_c() {
    let vars = ["t"];
    let sim = simulate_trace(OVERFLOW_FIXTURE, &vars);
    // Пиннинг правила S1: обёртка происходит на третьем такте (255 + 1 -> 0).
    assert_eq!(
        sim,
        vec![vec![254], vec![255], vec![0], vec![1], vec![2], vec![3]],
        "ожидаемая трасса симулятора: 254, 255, 0 (обёртка), 1, 2, 3"
    );

    if !cc_available() {
        eprintln!(
            "[ПРОПУСК] unsigned_overflow_wraps_like_generated_c: `cc` не найден \
             (трасса симулятора пришпилена выше)"
        );
        return;
    }
    let dir: PathBuf = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join("takt_conformance_0127_overflow");
    std::fs::create_dir_all(&dir).expect("каталог сборки");
    let c = c_trace(
        &dir,
        OVERFLOW_FIXTURE,
        "conformance_overflow",
        "ConformanceOverflow",
        "entry",
        &vars,
    );
    assert_eq!(
        sim, c,
        "обёртка беззнакового обязана совпадать.\nсимулятор={sim:?}\nC={c:?}"
    );
}
