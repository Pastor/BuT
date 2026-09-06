//! Дробное приведение вычисляется при компиляции - общий носитель представления.

use takt_lang::parse;
use takt_lang::semantic::const_eval::fixed_repr;
use takt_lang::semantic::tree::construct_model;

/// Строит дерево и возвращает печать объявления `v`.
fn value_of(src: &str) -> String {
    let (ast, _) = parse(src, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let borrowed = model.borrow();
    format!("{:?}", borrowed.variables.get("v").expect("объявление 'v'"))
}

fn declare(expr: &str) -> String {
    value_of(&format!(
        "var v: q(4, 4) := {expr};\nstart Run {{ ref Run; }}\n"
    ))
}

/// Предмет: приведение сворачивается, и представление совпадает с эталонным.
///
/// `1.5 as q(4, 4)` - это 24 (1.5 · 16).
#[test]
fn fractional_cast_is_folded() {
    let text = declare("1.5 as q(4, 4)");
    assert!(
        text.contains("24"),
        "инициализатор обязан свернуться в представление 24:\n{text}"
    );
}

/// Целое масштабируется сдвигом: `3 as q(4, 4)` - это 48.
#[test]
fn integer_cast_is_folded() {
    let text = declare("3 as q(4, 4)");
    assert!(text.contains("48"), "{text}");
}

/// Округление - **floor к −∞**, и проверяется на отрицательном.
///
/// На положительных floor и усечение совпадают, и дефект был бы невидим: `−1.1 as q(4,
/// 4)` даёт −18, а не −17.
#[test]
fn rounding_is_floor_towards_minus_infinity() {
    assert!(declare("1.1 as q(4, 4)").contains("17"));
    let negative = declare("-1.1 as q(4, 4)");
    assert!(
        negative.contains("-18"),
        "floor к −∞ обязан дать −18, а не −17:\n{negative}"
    );
}

/// Перенос и насыщение - по `W = m + n`, и они расходятся.
///
/// `20.0 as q(4, 4)` переносится в `4.0` (repr 64), `... sat` прижимается к `7.9375`
/// (repr 127).
#[test]
fn overflow_wraps_or_saturates_by_format() {
    assert!(declare("20.0 as q(4, 4)").contains("64"));
    let saturated =
        value_of("var v: q(4, 4) sat := 20.0 as q(4, 4) sat;\nstart Run { ref Run; }\n");
    assert!(saturated.contains("127"), "{saturated}");
}

/// **Устройство:** носитель реализует и 0170.
///
/// Эталон зовёт **эти же** функции (`takt-sim::eval::fixed`), и вторая реализация
/// разошлась бы с ними значениями - довод.
#[test]
fn carrier_implements_adr_0061() {
    assert_eq!(fixed_repr::from_int(3, 4), 48);
    assert_eq!(fixed_repr::from_decimal(15, 1, 4), Some(24));
    assert_eq!(fixed_repr::from_decimal(-11, 1, 4), Some(-18));
    assert_eq!(fixed_repr::normalize(320, 4, 4, false), 64);
    assert_eq!(fixed_repr::normalize(320, 4, 4, true), 127);
}

/// **Контроль:** литерал без приведения работает как прежде.
///
/// Без него "приведение сворачивается" означало бы "свёртка трогает всё подряд":
/// авторский литерал обязан идти прежним путём, и его непредставимость по-прежнему
/// судит `SE-058`.
#[test]
fn plain_literal_is_unchanged() {
    assert!(declare("1.5").contains("24"));
    let (ast, _) = parse("var v: q(4, 4) := 1.1;\nstart Run { ref Run; }\n", 0).expect("разбор");
    let err = construct_model(&ast, None, &[]).expect_err("непредставимый литерал отвергается");
    assert_eq!(err.code.as_deref(), Some("SE-058"), "{err:?}");
}
