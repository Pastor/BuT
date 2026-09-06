//! Приведение агрегата к массиву вычисляется при компиляции.

use takt_lang::parse;
use takt_lang::semantic::tree::construct_model;

fn value_of(src: &str) -> Result<String, takt_lang::diagnostics::Diagnostic> {
    let (ast, _) = parse(src, 0).expect("разбор");
    let model = construct_model(&ast, None, &[])?;
    let borrowed = model.borrow();
    Ok(format!(
        "{:?}",
        borrowed.variables.get("a").expect("объявление 'a'")
    ))
}

fn declare(expr: &str) -> String {
    value_of(&format!(
        "var a: [u8; 2] := {expr};\nstart Run {{ ref Run; }}\n"
    ))
    .expect("вход законен")
}

/// Предмет: приведение сворачивается в сам агрегат.
#[test]
fn aggregate_cast_is_folded() {
    let text = declare("{1, 2} as [u8; 2]");
    assert!(text.contains('1') && text.contains('2'), "{text}");
    assert!(
        !text.contains("Cast"),
        "приведение обязано исчезнуть из дерева — иначе цель видит его вместо агрегата:\n{text}"
    );
}

/// Элементы приводятся **правилом целого**: `300 as u8` - это 44.
///
/// Второго знания о переносе не заводится: тот же носитель, что у обычного
/// целочисленного приведения.
#[test]
fn elements_use_the_integer_rule() {
    let text = declare("{300, 2} as [u8; 2]");
    assert!(
        text.contains("44"),
        "элемент обязан обернуться по правилу целого:\n{text}"
    );
}

/// **Контроль:** агрегат без приведения работает как прежде.
#[test]
fn plain_aggregate_is_unchanged() {
    let text = declare("{1, 2}");
    assert!(text.contains('1') && text.contains('2'), "{text}");
}

/// **Граница:** несовпадение длины ветвь **не** судит - она возвращает
/// прежнее поведение.
///
/// Это не пропуск, а названная граница: длину агрегата сегодня не проверяет никто
/// (эталон хранит три элемента в двухэлементном массиве, цель `c` отвечает `CC-017`), и
/// класс шире предмета фичи - он вынесен кандидатом.
#[test]
fn length_mismatch_keeps_previous_behaviour() {
    let text = value_of("var a: [u8; 2] := {1, 2, 3} as [u8; 2];\nstart Run { ref Run; }\n")
        .expect("вход по-прежнему принимается семантикой");
    assert!(text.contains('3'), "{text}");
}
