//! Сверка переноса `q(m, n)` к ширине **формата** с целью `sv`.
//!
//! Отдельный подмодуль по предмету: здесь ширина формата, а
//! модуля. Помощники (`sv_trace_signed`, `simulate_trace`, `verilator_available`,
//! `build_dir`) берутся из родителя через `use super::*`.

use super::*;

/// (цель sv): перенос к **W** - тот же формат `q(6, 6)`, на котором цели
/// `c`/`rust`/`st` расходились с эталоном.
///
/// Цель `sv` была права **и до исправления** (`logic signed [W-1:0]` - ровно `W` бит), но
/// теста у этого свойства не было: сверка шла на `q(8, 8)`, где обе границы
/// совпадают. Тест закрепляет верное поведение, чтобы правка ширины хранения не увела и
/// его.
#[test]
fn fixed_wrap_to_width_matches_generated_sv() {
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
        "q(6,6): перенос к 12 битам (repr ∈ [−2048, 2047])"
    );

    if !verilator_available() {
        eprintln!("[ПРОПУСК] fixed_wrap_to_width_matches_generated_sv: verilator не найден");
        return;
    }
    let dir = build_dir("fixedw12");
    let sv = sv_trace_signed(
        &dir,
        fixture,
        "conformance_fixed_wrap_w12",
        &["conformance_fixed_wrap_w12_fixed_wrap_w12_acc"],
        sim.len(),
        &takt_lang::generator::GenerateOptions::default(),
    );
    assert_eq!(
        sim, sv,
        "перенос к W обязан совпасть с RTL.\nсимулятор={sim:?}\nRTL={sv:?}"
    );
}
