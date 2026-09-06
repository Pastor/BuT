//! Числовые операции цели `st`, не выразимые прямым оператором IEC.
//!
//! Сюда попадает то, что в IEC 61131-3 **есть, но означает другое**:
//!
//! - арифметический сдвиг вправо (`SHR` работает над битовой строкой, то есть
//!   логически) -;
//! - целая степень (`**` определён над вещественным, и `iec2c` отвергает его
//!   над целым) -.
//!
//! Вынесено из `st_expr` по границе ответственности: печать выражения отвечает "как
//! выглядит операция", этот модуль - "чем заменить операцию, которой в целевом языке
//! нет". Поводом был проверка размера модуля, границей - смысл.

use crate::diagnostics::Diagnostic;
use crate::generator::st::st_expr::{print_expression, unsupported};
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
        return Err(unsupported(
            "арифметический сдвиг вправо знакового на ПЕРЕМЕННУЮ величину: в IEC              61131-3 он выражается делением на 2ⁿ, и степень обязана быть              известна при компиляции",
        ));
    };
    if *bits < 0 || *bits > 62 {
        return Err(unsupported(
            "арифметический сдвиг вправо знакового на такую величину: делитель              2ⁿ не представим",
        ));
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
        return Err(unsupported(
            "возведение в степень с ПЕРЕМЕННЫМ показателем: в IEC 61131-3 \
             оператор '**' определён над вещественным, а разворот в умножения \
             требует показателя, известного при компиляции",
        ));
    };
    if *exp < 0 || *exp > 64 {
        return Err(unsupported(
            "возведение в такую степень: показатель обязан быть неотрицательным \
             и не больше 64 — разворот в умножения иначе не выразим",
        ));
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
