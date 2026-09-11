//! Числовые операции цели `st`, не выразимые прямым оператором IEC.
//!
//! Сюда попадает то, что в IEC 61131-3 **есть, но означает другое**:
//!
//! - арифметический сдвиг вправо (`SHR` работает над битовой строкой, то есть
//!   логически);
//! - целая степень (`**` определён над вещественным, и `iec2c` отвергает его
//!   над целым).
//!
//! Граница модуля - ответственность: печать выражения отвечает "как выглядит операция",
//! а этот модуль - "чем заменить операцию, которой в целевом языке нет".

use crate::diagnostics::Diagnostic;
use crate::diagnostics::lang::keys;
use crate::generator::st::st_expr::{print_expression, unsupported};
use crate::msg;
use crate::semantic::{ExpressionNode, ModelNode};

/// Арифметический сдвиг вправо знакового: **floor**-деление на `2ⁿ`.
///
/// # Ошибки
///
/// `ST-011` - величина сдвига не литерал: `2ⁿ` тогда пришлось бы считать функцией
/// `EXPT`, которая в IEC возвращает вещественное, и целочисленность результата
/// держалась бы на приведении. Отказ честнее.
pub(in crate::generator::st) fn arithmetic_shift_right(
    a: &ExpressionNode,
    b: &ExpressionNode,
    model: &ModelNode,
) -> Result<String, Diagnostic> {
    let ExpressionNode::Number(bits) = unwrap_parens(b) else {
        return Err(unsupported(&msg!(keys::ST_WHAT_ASR_VARIABLE)));
    };
    if *bits < 0 || *bits > 62 {
        return Err(unsupported(&msg!(keys::ST_WHAT_ASR_RANGE)));
    }
    let divisor = 1_i128 << bits;
    let value = print_expression(a, model)?;
    // `SEL(G, IN0, IN1)` выбирает `IN0` при `G = FALSE`: положительное делится как
    // есть, отрицательное - со сдвигом делимого вниз (floor).
    Ok(format!(
        "SEL({value} < 0, {value} / {divisor}, ({value} - {}) / {divisor})",
        divisor - 1
    ))
}

/// Снимает скобки - величина сдвига могла быть записана `(1)`.
fn unwrap_parens(expr: &ExpressionNode) -> &ExpressionNode {
    match expr {
        ExpressionNode::Parenthesis(inner) => unwrap_parens(inner),
        other => other,
    }
}

/// Разворачивает целую степень в умножения.
///
/// # Форма
///
/// Умножение целых в IEC определено, и переполнение ведёт себя как у прочей арифметики
/// цели.
///
/// # Ошибки
///
/// `ST-011` - показатель не литерал либо отрицателен: разворот тогда невозможен, а
/// `EXPT` вернул бы вещественное.
pub(in crate::generator::st) fn power(
    a: &ExpressionNode,
    b: &ExpressionNode,
    model: &ModelNode,
) -> Result<String, Diagnostic> {
    let ExpressionNode::Number(exp) = strip_parens(b) else {
        return Err(unsupported(&msg!(keys::ST_WHAT_POWER_VARIABLE)));
    };
    if *exp < 0 || *exp > 64 {
        return Err(unsupported(&msg!(keys::ST_WHAT_POWER_RANGE)));
    }
    if *exp == 0 {
        return Ok(String::from("1"));
    }
    let base = print_expression(a, model)?;
    let factors = std::iter::repeat_n(base, usize::try_from(*exp).unwrap_or(1))
        .collect::<Vec<_>>()
        .join(" * ");
    Ok(format!("({factors})"))
}

/// Снимает скобки у показателя степени.
fn strip_parens(expr: &ExpressionNode) -> &ExpressionNode {
    match expr {
        ExpressionNode::Parenthesis(inner) => strip_parens(inner),
        other => other,
    }
}
