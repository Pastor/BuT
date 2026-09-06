//! Структуры в цели `sv` - эталон == порождённый SystemVerilog.
//!
//! # Что ловится
//!
//! Цель отображала `TypeNode::Struct` в имя `<имя>_t`, но `typedef` не эмитила вовсе:
//! `verilator` отвечал `Can't find typedef/interface` - **при нулевом** коде возврата
//! `taktc`. Агрегатный инициализатор при этом отвергался `SV-002`.

use super::*;

/// Та же исправлениетура, что у сверки цели `st`: наблюдаемая `sum = kp + ki`.
const SRC: &str = include_str!("../../data/eval/conformance_struct_init.takt");

#[test]
fn struct_fields_match_generated_sv() {
    if !verilator_available() {
        eprintln!("verilator недоступен — сверка `sv` пропущена");
        return;
    }
    let dir = build_dir("struct_init");
    let path = fixture(&dir, "structinit", SRC);

    let expected = simulate_trace(path.to_str().expect("путь в UTF-8"), &["Loop::sum"]);
    let actual = sv_trace(
        &dir,
        path.to_str().expect("путь в UTF-8"),
        "structinit",
        &["structinit_loop_sum"],
        4,
    );
    let common: Vec<Vec<i128>> = actual.iter().take(expected.len()).cloned().collect();
    assert_eq!(
        common, expected,
        "трасса SystemVerilog разошлась с эталоном:\nsv     = {actual:?}\nэталон = {expected:?}"
    );
    assert!(
        expected.first().is_some_and(|row| row[0] == 5),
        "контроль: на первом такте sum обязана быть 5 (2 + 3): {expected:?}"
    );
}
