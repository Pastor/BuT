//! Сверка переноса `q(m, n)` к ширине **формата** с целью `c`.
//!
//! Вынесено из `conformance_c_tests.rs` подмодулем: файл упирался в лимит размера
//! модуля, и правило требует делить по логике. Помощники (`c_trace`, `simulate_trace`,
//! `cc_available`) берутся из родителя через `use super::*`.

use super::*;

/// (цель C): перенос идёт к **W**, а не к ширине хранения.
///
/// Формат намеренно `q(6, 6)`: `W = 12`, тип хранения - 16 бит. Прежние фикстуры сверки
/// состояли только из `q(8, 8)` и `q(4, 4)`, где `W` совпадает с шириной хранения, и
/// потому расхождение (`(int16_t)` вместо переноса к 12 битам) было для них
/// **невидимо** - дыра в покрытии, а не в коде проверки.
#[test]
fn fixed_wrap_to_width_matches_generated_c() {
    let vars = ["acc"];
    let fixture = "tests/data/eval/conformance_fixed_wrap_w12.takt";
    let sim = simulate_trace(fixture, &vars);
    assert_eq!(
        sim,
        vec![
            vec![-1664],
            vec![-1152],
            vec![-640],
            vec![-128],
            vec![384],
            vec![896],
        ],
        "q(6,6): 1920 + 512 = 2432 → перенос к 12 битам даёт −1664 (−26.0); \
         перенос к 16 битам дал бы 2432 (38.0)"
    );

    if !cc_available() {
        eprintln!("[ПРОПУСК] fixed_wrap_to_width_matches_generated_c: `cc` не найден");
        return;
    }
    let dir: PathBuf = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join("takt_conformance_fixed_wrap_w12");
    std::fs::create_dir_all(&dir).expect("каталог сборки");
    let c = c_trace(
        &dir,
        fixture,
        "conformance_fixed_wrap_w12",
        "ConformanceFixedWrapW12",
        "entry",
        &vars,
    );
    assert_eq!(
        sim, c,
        "перенос к W (не к ширине хранения) обязан совпасть с C.\nсим={sim:?}\nC={c:?}"
    );
}
