//! Двухрежимный эталон `float` и native-проверки целей.
//!
//! : `native`-`float` и Q-`float` - **разные** численные семантики, автор выбирает
//! явно флагами. Здесь проверяется:
//!
//! - **Двухрежимность эталона-симулятора**: без трансформации `float`
//!   считается как `f64` (`Value::Real`), с ней - как `q(m, n)` (`Value::Fixed`,
//!   repr). Это тест направления: мутация "эталон native, цель Q" дала бы
//!   `Real` там, где Q-цель ждёт repr, - сверка обязана различать режимы.
//! - **Native по умолчанию**: `--float-as-q` **без** `--float-embedded`
//!   оставляет `c`/`rust`/`st` на нативном `double`/`f64`/`LREAL`; Q-путь - только
//!   со вторым флагом. Молчаливого Q быть не должно.
//!
//! Потактовые Q-сверки целей - в `conformance_{c,rust,st}_tests.rs`
//! (`float_embedded_*`); файл вынесен из них ради лимита размера модуля.

use takt_lang::semantic::tree::construct_model;
use takt_sim::{Value, build_unit};

const FLOAT_Q_FIXTURE: &str = "tests/data/eval/conformance_float_q.takt";

/// Опции: точность задана, `--float-embedded` не включён (native для c/rust/st).
#[allow(clippy::field_reassign_with_default)] // GenerateOptions - #[non_exhaustive]
fn float_as_q_only(m: u8, n: u8) -> takt_lang::generator::GenerateOptions {
    let mut o = takt_lang::generator::GenerateOptions::default();
    o.float_as_q = Some((m, n));
    o
}

/// Эталон двухрежимен. Без трансформации `acc` - вещественное (`Value::Real`,
/// native `f64`); с трансформацией - `Value::Fixed` (repr q(8,8)). Разные представления
/// ⇒ сверка ведётся внутри режима, а не между ними.
#[test]
fn float_native_and_q_modes_differ() {
    let source = std::fs::read_to_string(FLOAT_Q_FIXTURE).expect("фикстура читается");

    // Native режим (трансформация не применена): acc - вещественное.
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение юнита");
    unit.tick();
    assert!(
        matches!(unit.variable("acc"), Some(Value::Real(_))),
        "native-режим float: acc — Value::Real (f64), не Fixed"
    );

    // Q-режим (трансформация применена): acc - представление q(8,8) (−3.0 -> −768).
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    takt_lang::semantic::lower_float::lower_float_to_fixed(model.clone(), 8, 8)
        .expect("float → q(8,8)");
    let mut unit = build_unit(model).expect("построение юнита");
    unit.tick();
    assert!(
        matches!(unit.variable("acc"), Some(Value::Fixed { repr: -768, .. })),
        "Q-режим float: acc — Fixed repr −768 (−3.0), иная семантика, чем native"
    );
}

/// `--float-as-q` без `--float-embedded` ->
/// `double` (Q-путь только со вторым флагом).
#[test]
fn float_as_q_without_embedded_is_native_c() {
    let dir = std::env::temp_dir().join(format!("takt_float_native_c_{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let source = std::fs::read_to_string(FLOAT_Q_FIXTURE).expect("фикстура");
    takt_lang::compile_to_c(
        "cfq",
        &source,
        dir.to_str().unwrap(),
        &[],
        &float_as_q_only(8, 8),
    )
    .expect("порождение C");
    let h = std::fs::read_to_string(dir.join("cfq.h")).expect(".h");
    assert!(
        h.contains("double acc"),
        "без --float-embedded float в C остаётся native double.\n{h}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}
