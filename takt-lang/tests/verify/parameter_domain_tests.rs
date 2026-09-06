//! Параметр модели в предикате - постоянная величина.
//!
//! # Что теперь
//!
//! проверяется **значение по умолчанию модели**. Параметр, которому не присваивают в
//! теле (признак `mutated`, 0185), даёт домен из одного значения - своего умолчания.
//!
//! Параметр, которому в теле присваивают, остаётся переменной: умолчание признака -
//! "изменяемый", и не размеченный параметр обязан вести себя как прежде.

use takt_lang::parse;
use takt_lang::semantic::tree::construct_model;
use takt_lang::verification::verify::{Verdict, verify_model};

/// Вердикт свойства у модели `Probe` (область формулы - по месту объявления).
fn verdict(src: &str, phi_src: &str) -> Verdict {
    let (ast, _) = parse(src, 0).expect("разбор модели");
    let root = construct_model(&ast, None, &[]).expect("построение дерева");
    let phi = takt_lang::parse_ltl_property(phi_src).expect("разбор формулы");
    let probe = root
        .borrow()
        .models
        .get("Probe")
        .cloned()
        .expect("модель Probe");
    let m = probe.borrow();
    verify_model(&m, &phi)
}

/// Модель со счётчиком и порогом, объявленным как `decl`.
fn source(decl: &str, body: &str) -> String {
    format!(
        "model Probe {{\n\
         \x20   {decl}\n\
         \x20   var ticks: u8 := 0;\n\
         \x20   out ticks_out: u8 at 0x300;\n\
         \x20   cond Below = ticks < limit;\n\
         \x20   start Cycle {{\n\
         \x20       always {{ ticks := ticks + 1; ticks_out := ticks; {body} }}\n\
         \x20       ref Cycle: ticks < 200;\n\
         \x20   }}\n\
         }}\n\
         start Main = Probe;\n"
    )
}

/// **T1.** Предикат с параметром проверяется - как с константой.
#[test]
fn parameter_predicate_is_verified() {
    let with_param = verdict(
        &source("parameter limit: u8 := 10;", ""),
        "G (!Below | Below)",
    );
    assert!(
        matches!(with_param, Verdict::Holds),
        "тавтология обязана держаться: параметр — величина сборки, {with_param:?}"
    );
}

/// **T2.** Вердикт содержателен: `G Below` над тем же входом нарушается.
///
/// Без этой проверки T1 доказывал бы лишь то, что задача уложилась в потолок.
#[test]
fn verdict_is_meaningful() {
    let v = verdict(&source("parameter limit: u8 := 10;", ""), "G Below");
    assert!(
        matches!(v, Verdict::Violated { .. }),
        "счётчик перерастает порог 10 — свойство обязано нарушаться: {v:?}"
    );
}

/// **T3.** Параметр и константа дают один вердикт - в этом предмет фичи.
#[test]
fn parameter_matches_const() {
    let param = verdict(&source("parameter limit: u8 := 10;", ""), "G Below");
    let konst = verdict(&source("const limit: u8 := 10;", ""), "G Below");
    assert_eq!(
        format!("{param:?}"),
        format!("{konst:?}"),
        "тождественные записи обязаны давать один вердикт"
    );
}

/// **T4. Контроль:** параметр, которому присваивают, остаётся переменной.
///
/// Признак `mutated` заполняет анализ изменяемости, и его умолчание - "изменяемый".
/// Записав значение в теле, автор делает параметр обычной переменной - и абстракция
/// обязана вернуться к полному домену.
#[test]
fn mutated_parameter_stays_a_variable() {
    let v = verdict(
        &source("parameter limit: u8 := 10;", "limit := limit + 1;"),
        "G (!Below | Below)",
    );
    assert!(
        matches!(v, Verdict::Unsupported { .. }),
        "изменяемый параметр — свободная переменная, домен `u8` не влезает в потолок: {v:?}"
    );
}
