//! Сверка **насыщения** `q(m, n) sat` с целью `sv`.
//!
//! Вынесено подмодулем по той же причине, что и `fixed_width.rs`: родительский файл
//! отдельный подмодуль по предмету. Помощники (`sv_trace_signed`, `simulate_trace`,
//! `verilator_available`, `build_dir`) берутся из родителя через `use super::*`.
//!
//! Цель `sv` считает промежуток в **удвоенной** ширине: у `logic signed [W-1:0]` места
//! под сумму нет, и сравнение с границами шло бы по уже обёрнутому значению. Такой RTL
//! синтезируется молча - именно поэтому вердикт даёт сверка значений, а не проверки.

use super::*;

/// Трасса представлений фикстуры насыщения (та же, что у целей `c` и `st`: тестовые
/// файлы - разные крейты, общего модуля у них нет).
fn expected_sat_w12() -> Vec<Vec<i128>> {
    vec![
        vec![512, -512, 512],
        vec![1024, -1024, 1024],
        vec![1536, -1536, 1536],
        vec![2047, -2048, 2047],
        vec![2047, -2048, 2047],
        vec![2047, -2048, 2047],
    ]
}

/// Насыщение прижимает к границам формата на обеих границах и на
/// крае унарного минуса `−(−2^(W−1))` - побитово как у эталона.
#[test]
fn fixed_saturation_matches_generated_sv() {
    let vars = ["up", "down", "neg"];
    let fixture = "tests/data/eval/conformance_fixed_sat_w12.takt";
    let sim = simulate_trace(fixture, &vars);
    assert_eq!(
        sim,
        expected_sat_w12(),
        "q(6,6) sat: прижатие к repr ∈ [−2048, 2047]"
    );

    if !verilator_available() {
        eprintln!("[ПРОПУСК] fixed_saturation_matches_generated_sv: verilator не найден");
        return;
    }
    let dir = build_dir("fixedsatw12");
    let sv = sv_trace_signed(
        &dir,
        fixture,
        "conformance_fixed_sat_w12",
        &[
            "conformance_fixed_sat_w12_fixed_sat_w12_up",
            "conformance_fixed_sat_w12_fixed_sat_w12_down",
            "conformance_fixed_sat_w12_fixed_sat_w12_neg",
        ],
        sim.len(),
        &takt_lang::generator::GenerateOptions::default(),
    );
    assert_eq!(
        sim, sv,
        "насыщение обязано совпасть с RTL.\nсимулятор={sim:?}\nRTL={sv:?}"
    );
}
