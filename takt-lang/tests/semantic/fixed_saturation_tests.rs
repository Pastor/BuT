//! Синтаксис и семантика насыщающего fixed-point `q(m, n) sat`.
//!
//! Здесь проверяется **граница языка**: что разбирается, что отвергается и с какой
//! диагностикой. Поведение самой арифметики (прижатие к границам) - предмет эталона и
//! сверок.
//!
//! Ключевой тест файла - `modifier_typo_is_rejected`: без него опечатка в слове `sat`
//! дала бы **молчаливый перенос** там, где автор просил насыщение. Это ровно тот класс
//! молчаливого расхождения, ради которого фича заведена.

use takt_lang::semantic::tree::construct_model;
use takt_lang::semantic::type_node::TypeNode;

/// Строит модель из исходника; возвращает диагностики как строку.
fn compile(src: &str) -> Result<(), String> {
    let (ast, _) = takt_lang::parse(src, 0).map_err(|e| format!("{e:?}"))?;
    construct_model(&ast, None, &[])
        .map(|_| ())
        .map_err(|d| format!("{}: {}", d.code.clone().unwrap_or_default(), d.message))
}

/// Тип переменной `name` в построенной модели.
fn var_type(src: &str, name: &str) -> TypeNode {
    let (ast, _) = takt_lang::parse(src, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let borrowed = model.borrow();
    let var = borrowed.variables.get(name).expect("переменная объявлена");
    var.ty().clone()
}

const PROLOGUE: &str = "model M { start S; }\nstart Main = M;\n";

/// A1: `q(8, 8) sat` разбирается и даёт формат с признаком насыщения.
#[test]
fn saturating_format_parses() {
    let src = format!("var a: q(8, 8) sat := 1.5;\n{PROLOGUE}");
    assert_eq!(
        var_type(&src, "a"),
        TypeNode::Fixed {
            m: 8,
            n: 8,
            sat: true
        }
    );
}

/// A1: умолчание не изменилось - `q(8, 8)` по-прежнему перенос.
#[test]
fn plain_format_stays_wrapping() {
    let src = format!("var a: q(8, 8) := 1.5;\n{PROLOGUE}");
    assert_eq!(
        var_type(&src, "a"),
        TypeNode::Fixed {
            m: 8,
            n: 8,
            sat: false
        }
    );
}

/// A2 (ключевой): слово, отличное от `sat`, отвергается `SE-104`.
///
/// Проверяется именно **опечатка**, а не заведомо чужое слово: `sta` - то, что автор
/// реально напишет, и молчаливый приём такой записи вернул бы перенос под видом
/// насыщения.
#[test]
fn modifier_typo_is_rejected() {
    let src = format!("var a: q(8, 8) sta := 1.5;\n{PROLOGUE}");
    let err = compile(&src).expect_err("опечатка модификатора обязана отвергаться");
    assert!(
        err.starts_with("SE-104"),
        "ожидался SE-104, получено: {err}"
    );
    assert!(
        err.contains("sat"),
        "текст обязан называть допустимое слово: {err}"
    );
}

/// A3: смешение `sat` и не-`sat` - ошибка (правило "формат обязан совпасть").
///
/// Код - существующий `SE-059`, а не новый: правило одно, и разделение его надвое дало
/// бы два места, обязанные давать один вердикт (замер ).
#[test]
fn mixing_saturating_and_wrapping_is_rejected() {
    let src = "var a: q(8, 8) sat := 1.5;\n\
               var b: q(8, 8) := 2.0;\n\
               var r: q(8, 8) sat := 0.0;\n\
               model M { start S { always { r := a + b; } } }\n\
               start Main = M;\n";
    let err = compile(src).expect_err("смешение форматов обязано отвергаться");
    assert!(err.starts_with("SE-059"), "ожидался SE-059: {err}");
    assert!(
        err.contains("q(8, 8) sat") && err.contains("q(8, 8)"),
        "текст обязан назвать ОБА формата, иначе сообщение бессмысленно: {err}"
    );
}

/// A3: явное приведение к насыщающему формату законно.
#[test]
fn explicit_cast_to_saturating_is_accepted() {
    let src = "var a: q(8, 8) sat := 1.5;\n\
               var b: q(8, 8) := 2.0;\n\
               var r: q(8, 8) sat := 0.0;\n\
               model M { start S { always { r := a + (b as q(8, 8) sat); } } }\n\
               start Main = M;\n";
    assert!(compile(src).is_ok(), "явное приведение обязано приниматься");
}

/// Печать типа несёт модификатор: без него текст диагностики о смешении выглядел бы как
/// "нельзя смешивать q(8, 8) и q(8, 8)".
#[test]
fn type_display_carries_modifier() {
    assert_eq!(
        TypeNode::Fixed {
            m: 8,
            n: 8,
            sat: true
        }
        .to_string(),
        "q(8, 8) sat"
    );
    assert_eq!(
        TypeNode::Fixed {
            m: 8,
            n: 8,
            sat: false
        }
        .to_string(),
        "q(8, 8)"
    );
}

/// Форматтер печатает модификатор: он часть смысла программы, а не оформления.
#[test]
fn formatter_keeps_modifier() {
    let src = "var a: q(8,8) sat := 1.5;\nmodel M { start S; }\nstart Main = M;\n";
    let out = takt_lang::format::format_source(src).expect("форматирование");
    assert!(
        out.contains("q(8, 8) sat"),
        "модификатор обязан пережить форматирование: {out}"
    );
}
