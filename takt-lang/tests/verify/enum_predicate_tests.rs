//! Предикат над переменной перечислимого типа.
//!
//! # Что тестытся
//!
//! Свойство над перечислимой переменной **проверяется** (держится либо нарушается), а
//! не отвергается; контроль - тот же вход с `float`, где причина законна.

use takt_lang::parse;
use takt_lang::semantic::tree::construct_model;
use takt_lang::verification::verify::{Verdict, verify_model};

/// Вердикт свойства у модели `owner` (либо у корня, если имя не задано).
///
/// Область формулы - по месту объявления: `cond`, объявленный во вложенной модели,
/// проверяется у неё, а не у корня. Именно этот случай и нёс дефект: перечисление лежит
/// уровнем выше переменной.
fn verdict_in(src: &str, owner: Option<&str>, phi_src: &str) -> Verdict {
    let (ast, _) = parse(src, 0).expect("разбор модели");
    let root = construct_model(&ast, None, &[]).expect("построение дерева");
    let phi = takt_lang::parse_ltl_property(phi_src).expect("разбор формулы");
    let target = match owner {
        Some(name) => root
            .borrow()
            .models
            .get(name)
            .cloned()
            .unwrap_or_else(|| panic!("модель '{name}' не найдена")),
        None => root,
    };
    let m = target.borrow();
    verify_model(&m, &phi)
}

fn verdict(src: &str, phi_src: &str) -> Verdict {
    verdict_in(src, None, phi_src)
}

/// Перечисление объявлено на уровне файла - вне модели, которая им пользуется.
///
/// Именно так пишут примеры проекта: `enum` стоит рядом с моделями, а не внутри каждой.
/// Вердикт проверяется у той модели, где объявлен `cond` (область формулы - по месту
/// объявления, 0051), поэтому здесь модель одна и она же корневая.
const OUTER_ENUM: &str = "enum Mode { Idle, Run }\n\
     model Probe {\n\
     \x20   var mode: Mode := Idle;\n\
     \x20   var ticks: u8 := 0;\n\
     \x20   out ticks_out: u8 at 0x300;\n\
     \x20   cond IsRun = mode = Run;\n\
     \x20   start Cycle {\n\
     \x20       always { ticks := ticks + 1; mode := Run; ticks_out := ticks; }\n\
     \x20       ref Cycle: ticks < 200;\n\
     \x20   }\n\
     }\n\
     start Main = Probe;\n";

/// Тавтология над перечислимой переменной **проверяется** и держится.
#[test]
fn enum_predicate_is_verified() {
    let v = verdict_in(OUTER_ENUM, Some("Probe"), "G (!IsRun | IsRun)");
    assert!(
        matches!(v, Verdict::Holds),
        "предикат над `enum` обязан проверяться, а не отвергаться: {v:?}"
    );
}

/// Нарушаемое свойство над тем же предикатом даёт контрпример.
///
/// Контроль обязателен: "проверяется" без него означало бы лишь, что вердикт перестал
/// быть отказом - а не что перебор значений идёт.
#[test]
fn enum_predicate_can_be_violated() {
    let v = verdict_in(OUTER_ENUM, Some("Probe"), "G IsRun");
    assert!(
        matches!(v, Verdict::Violated { .. }),
        "данные абстрагированы, и свойство обязано нарушаться: {v:?}"
    );
}

/// **Контроль:** у `float` причина законна - домен неперечислим.
#[test]
fn float_predicate_is_still_unsupported() {
    let src = "var level: float := 0.0;\n\
         var ticks: u8 := 0;\n\
         out ticks_out: u8 at 0x300;\n\
         cond High = level > 1.0;\n\
         start Cycle {\n\
         \x20   always { ticks := ticks + 1; ticks_out := ticks; }\n\
         \x20   ref Cycle: ticks < 200;\n\
         }\n";
    let v = verdict(src, "G High");
    assert!(
        matches!(v, Verdict::Unsupported { .. }),
        "домен `float` перебрать нельзя — отказ обязан остаться: {v:?}"
    );
}
