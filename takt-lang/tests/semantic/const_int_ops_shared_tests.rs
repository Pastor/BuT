//! Одна таблица целочисленных операций на два вычислителя -.
//!
//! # Что здесь проверяется
//!
//! Правила целочисленной арифметики константного вычисления были написаны
//! **дважды**: у общего вычислителя (`const_eval`) и у выражения адреса
//! (`address_map::eval`). Обе таблицы перечисляли те же операции, ту же обёртку
//! и ту же границу сдвига - и уже разошлись формулировкой сообщения о сдвиге.
//! Теперь таблица одна (`const_eval::int_ops`), а у вызывающих остаётся своё:
//! носитель, набор допустимых форм и **тексты диагностик**.
//!
//! Поэтому проверяется не "функция вызвана", а **наблюдаемое**: одинаковые выражения в
//! двух разных позициях языка дают одинаковые значения, а особенности адреса (запрет
//! `адрес:бит`, отсутствие сравнений, сужение к `i64`) сохранены.

use takt_lang::semantic::tree::construct_model;
use takt_lang::{parse, resolve_addresses};

/// Адрес порта `p` после разрешения - путь выражения адреса.
fn address_of(expr: &str) -> Result<i64, String> {
    let src = format!(
        "in p: bit;\naddress p = {expr};\nvar n: u8 := 0;\nstart Run {{ always {{ n := n + 1; }} ref Run; }}\n"
    );
    let (ast, _) = parse(&src, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).map_err(|d| d.code.unwrap_or_default())?;
    let resolution = resolve_addresses(model, &[], &Default::default());
    let diagnostics: Vec<String> = resolution
        .diagnostics
        .iter()
        .filter(|d| d.level == takt_lang::diagnostics::Level::Error)
        .map(|d| format!("{}: {}", d.code.clone().unwrap_or_default(), d.message))
        .collect();
    if let Some(first) = diagnostics.first() {
        return Err(first.clone());
    }
    resolution
        .map
        .values()
        .find(|a| a.name == "p")
        .map(|a| a.addr)
        .ok_or_else(|| "адрес не разрешён".to_string())
}

/// Значение константы `K` - путь общего вычислителя.
///
/// Тип задан **явно** и широким: с выведенным типом ширину задал бы левый операнд (`1` ->
/// `[bit;8]`), и `1 << 8` нормировалось бы в ноль - сравнивались бы не таблицы
/// операций, а ширины типов.
fn const_of(expr: &str) -> Result<i128, String> {
    let src = format!(
        "const K: i64 := {expr};\nvar n: u8 := 0;\nstart Run {{ always {{ n := n + 1; }} ref Run; }}\n"
    );
    let (ast, _) = parse(&src, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).map_err(|d| d.code.unwrap_or_default())?;
    let model = model.borrow();
    let var = model.variables.get("K").expect("константа 'K'");
    match var {
        takt_lang::semantic::VariableNode::Const { expr, .. } => match expr {
            takt_lang::semantic::ExpressionNode::Number(v) => Ok(*v),
            other => Err(format!("не литерал: {other:?}")),
        },
        other => Err(format!("не константа: {other:?}")),
    }
}

/// **T1.** Одно выражение - одно значение в обеих позициях языка.
///
/// Список нарочно перебирает все арифметические и побитовые операции: разойдись таблицы
/// хоть в одной, тест назовёт её.
#[test]
fn same_expression_gives_same_value_in_both_evaluators() {
    let cases = [
        ("2 + 3", 5),
        ("10 - 4", 6),
        ("6 * 7", 42),
        ("40 / 5", 8),
        ("41 % 5", 1),
        ("1 << 8", 256),
        ("256 >> 4", 16),
        ("0xF0 & 0x3C", 0x30),
        ("0xF0 | 0x0F", 0xFF),
        ("0xFF ^ 0x0F", 0xF0),
    ];
    let mut mismatched = Vec::new();
    for (expr, expected) in cases {
        let by_address = address_of(expr);
        let by_const = const_of(expr);
        if by_address != Ok(expected) || by_const != Ok(i128::from(expected)) {
            mismatched.push(format!(
                "{expr}: адрес {by_address:?}, константа {by_const:?}"
            ));
        }
    }
    assert!(mismatched.is_empty(), "значения разошлись: {mismatched:?}");
}

/// **T2.** Деление на ноль отвергают оба - каждый своим кодом.
#[test]
fn division_by_zero_is_refused_by_both() {
    let by_address = address_of("8 / 0").expect_err("адрес обязан отказать");
    assert!(
        by_address.contains("SE-055") && by_address.contains("деление на ноль"),
        "адрес: {by_address}"
    );
    assert!(
        const_of("8 / 0").is_err(),
        "общий вычислитель обязан отказать"
    );
}

/// **T3.** Граница сдвига одна и та же (`0..63`).
#[test]
fn shift_range_is_the_same() {
    assert!(
        address_of("1 << 64").is_err(),
        "адрес: сдвиг на 64 законен?"
    );
    assert!(
        const_of("1 << 64").is_err(),
        "общий вычислитель: сдвиг на 64 законен?"
    );
    assert_eq!(address_of("1 << 63"), Ok(1i64 << 63));
}

/// **T4.** Особенность адреса цела: сравнение адресом быть не может.
///
/// Отсекает его **матчер** выражения адреса (он принимает только арифметику и побитовые
/// операции), а не общая таблица. Тест проверяет именно это разделение: таблица операций
/// стала общей, а **набор допустимых форм** остался у адреса своим.
#[test]
fn comparison_is_still_unsupported_in_address() {
    let err = address_of("1 < 2").expect_err("сравнение адресом быть не может");
    assert!(
        err.contains("SE-055") && err.contains("поддержаны только"),
        "отказ адреса на сравнении изменился: {err}"
    );
}

/// **T5.** Сужение к `i64` - прежняя обёртка по 64 битам.
///
/// Вычисление идёт в `i128`, поэтому равносильность проверяется наблюдаемо: `i64::MAX +
/// 1` обязан дать `i64::MIN`.
#[test]
fn address_arithmetic_wraps_at_64_bits() {
    let expr = format!("{} + 1", i64::MAX);
    assert_eq!(address_of(&expr), Ok(i64::MIN));
}
