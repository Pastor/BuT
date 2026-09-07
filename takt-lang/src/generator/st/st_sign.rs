//! Сравнение операндов разной знаковости - цель `st`.
//!
//! Печать "как есть" здесь не годится: `iec2c` отвергает вывод ("Data type mismatch for
//! '<' expression") при нулевом коде возврата `taktc`. Формы выбраны прогоном
//! инструмента: преобразование `X_TO_Y` к общему типу и раскрытие проверкой знака там,
//! где общего типа нет (`u64` против знакового).
//!
//! Печатников два - условий и выражений: условие ребра приходит `ConditionNode`, а
//! условие `if` в теле - `ExpressionNode`. Правка одного чинит половину входов.

use super::st_expr::{binary, binary_cond, wrap_cond, wrap_expr};
use crate::diagnostics::Diagnostic;
use crate::semantic::type_node::TypeNode;
use crate::semantic::{ConditionNode, ExpressionNode, ModelNode};

/// Сравнение операндов разной знаковости.
///
/// Печать "как есть" даёт отказ `iec2c` ("Data type mismatch for '<' expression") при
/// нулевом коде возврата `taktc`. Формы выбраны прогоном инструмента: преобразование
/// `X_TO_Y` к общему типу и раскрытие проверкой знака там, где общего типа нет.
pub(super) fn compare_cond(
    a: &ConditionNode,
    op: &str,
    b: &ConditionNode,
    model: &ModelNode,
) -> Result<String, Diagnostic> {
    // Имя варианта рядом с целым операндом печатается значением. У цели `st` мнемонику
    // объявляет `VAR CONSTANT` того же POU, но перечисление, пришедшее из библиотеки
    // вместе с моделью и не названное в списке импорта, до дерева импортёра не доезжает,
    // и `iec2c` отвечает "Ambiguous enumerate value or Variable not declared in this
    // scope" при нулевом коде возврата `taktc`. Значение доезжает всегда: оно лежит в
    // самом узле.
    if let Some(low) = crate::generator::enum_compare::lowered(a, b) {
        let (lt, rt) = if low.variant_is_left {
            (low.value.to_string(), wrap_cond(b, model)?)
        } else {
            (wrap_cond(a, model)?, low.value.to_string())
        };
        return Ok(format!("{lt} {op} {rt}"));
    }
    let (ta, tb) = (
        crate::generator::mixed_sign::operand_type_cond(a),
        crate::generator::mixed_sign::operand_type_cond(b),
    );
    match crate::generator::mixed_sign::plan(ta.as_ref(), tb.as_ref()) {
        crate::generator::mixed_sign::Plan::AsIs => binary_cond(a, op, b, model),
        crate::generator::mixed_sign::Plan::Widen { bits } => {
            let Some(target) = crate::generator::st::st_type::iec_integer_name(bits as u8, true)
            else {
                return binary_cond(a, op, b, model);
            };
            let conv =
                |node: &ConditionNode, ty: &Option<TypeNode>| -> Result<String, Diagnostic> {
                    let text = wrap_cond(node, model)?;
                    let Some(TypeNode::Integer { bits, signed }) = ty else {
                        return Ok(text);
                    };
                    match crate::generator::st::st_type::iec_integer_name(*bits, *signed) {
                        Some(from) if from != target => Ok(format!("{from}_TO_{target}({text})")),
                        _ => Ok(text),
                    }
                };
            Ok(format!("{} {op} {}", conv(a, &ta)?, conv(b, &tb)?))
        }
        crate::generator::mixed_sign::Plan::SignGuard { signed_is_left } => {
            let (lt, rt) = (wrap_cond(a, model)?, wrap_cond(b, model)?);
            let (signed, unsigned) = if signed_is_left {
                (lt.as_str(), rt.as_str())
            } else {
                (rt.as_str(), lt.as_str())
            };
            // Операнд печатается дважды: в условии Takt эффектов не бывает -
            // присваивание есть оператор.
            let neg = format!("({signed} < 0)");
            let same = if signed_is_left {
                format!("(LINT_TO_ULINT({signed}) {op} {unsigned})")
            } else {
                format!("({unsigned} {op} LINT_TO_ULINT({signed}))")
            };
            let negative_wins = crate::generator::mixed_sign::negative_wins(op, signed_is_left);
            Ok(if negative_wins {
                format!("({neg} OR {same})")
            } else {
                format!("(NOT {neg} AND {same})")
            })
        }
    }
}
/// Сравнение операндов разной знаковости в выражении.
///
/// Правило одно с печатником условий (`compare_cond`); здесь - путь тела, где условие
/// `if` приходит выражением.
pub(super) fn expr_compare(
    a: &ExpressionNode,
    op: &str,
    b: &ExpressionNode,
    model: &ModelNode,
) -> Result<String, Diagnostic> {
    let (ta, tb) = (
        crate::generator::mixed_sign::operand_type_expr(a),
        crate::generator::mixed_sign::operand_type_expr(b),
    );
    match crate::generator::mixed_sign::plan(ta.as_ref(), tb.as_ref()) {
        crate::generator::mixed_sign::Plan::AsIs => binary(a, op, b, model),
        crate::generator::mixed_sign::Plan::Widen { bits } => {
            let Some(target) = crate::generator::st::st_type::iec_integer_name(bits as u8, true)
            else {
                return binary(a, op, b, model);
            };
            let conv =
                |node: &ExpressionNode, ty: &Option<TypeNode>| -> Result<String, Diagnostic> {
                    let text = wrap_expr(node, model)?;
                    let Some(TypeNode::Integer { bits, signed }) = ty else {
                        return Ok(text);
                    };
                    match crate::generator::st::st_type::iec_integer_name(*bits, *signed) {
                        Some(from) if from != target => Ok(format!("{from}_TO_{target}({text})")),
                        _ => Ok(text),
                    }
                };
            Ok(format!("{} {op} {}", conv(a, &ta)?, conv(b, &tb)?))
        }
        crate::generator::mixed_sign::Plan::SignGuard { signed_is_left } => {
            let (lt, rt) = (wrap_expr(a, model)?, wrap_expr(b, model)?);
            let (signed, unsigned) = if signed_is_left {
                (lt.as_str(), rt.as_str())
            } else {
                (rt.as_str(), lt.as_str())
            };
            let neg = format!("({signed} < 0)");
            let same = if signed_is_left {
                format!("(LINT_TO_ULINT({signed}) {op} {unsigned})")
            } else {
                format!("({unsigned} {op} LINT_TO_ULINT({signed}))")
            };
            let negative_wins = crate::generator::mixed_sign::negative_wins(op, signed_is_left);
            Ok(if negative_wins {
                format!("({neg} OR {same})")
            } else {
                format!("(NOT {neg} AND {same})")
            })
        }
    }
}

/// Нужно ли приводить операнды арифметики к типу приёмника.
///
/// Признак узкий: оба операнда - **именованные значения** целого типа, и хотя бы у
/// одного тип отличается от приёмника. Литерал сюда не входит: у него типа нет, он
/// подстраивается под приёмник сам.
pub(super) fn operands_need_cast(
    l: &ExpressionNode,
    r: &ExpressionNode,
    target: &TypeNode,
) -> bool {
    let (Some(lt), Some(rt)) = (
        crate::generator::mixed_sign::operand_type_expr(l),
        crate::generator::mixed_sign::operand_type_expr(r),
    ) else {
        return false;
    };
    matches!(target, TypeNode::Integer { .. })
        && matches!(lt, TypeNode::Integer { .. })
        && matches!(rt, TypeNode::Integer { .. })
        && (lt != *target || rt != *target)
}

/// Печатает арифметику с операндами, приведёнными к типу приёмника.
///
/// Преобразование - стандартное `X_TO_Y` IEC; имя типа берётся у `st_type`, второго
/// списка имён быть не должно.
pub(super) fn arith_in_target(
    value: &ExpressionNode,
    target: &TypeNode,
    model: &ModelNode,
) -> Result<String, Diagnostic> {
    let (op, l, r) = match value {
        ExpressionNode::Add(l, r) => ("+", l, r),
        ExpressionNode::Subtract(l, r) => ("-", l, r),
        ExpressionNode::Multiply(l, r) => ("*", l, r),
        ExpressionNode::Divide(l, r) => ("/", l, r),
        ExpressionNode::Modulo(l, r) => ("MOD", l, r),
        // Ветвь зовётся только для арифметики (охрана в `coerce_to`).
        _ => return crate::generator::st::st_expr::print_expression(value, model),
    };
    let TypeNode::Integer { bits, signed } = target else {
        return crate::generator::st::st_expr::print_expression(value, model);
    };
    let Some(to) = crate::generator::st::st_type::iec_integer_name(*bits, *signed) else {
        return crate::generator::st::st_expr::print_expression(value, model);
    };
    let conv = |node: &ExpressionNode| -> Result<String, Diagnostic> {
        let text = wrap_expr(node, model)?;
        let Some(TypeNode::Integer { bits, signed }) =
            crate::generator::mixed_sign::operand_type_expr(node)
        else {
            return Ok(text);
        };
        match crate::generator::st::st_type::iec_integer_name(bits, signed) {
            Some(from) if from != to => Ok(format!("{from}_TO_{to}({text})")),
            _ => Ok(text),
        }
    };
    Ok(format!("{} {op} {}", conv(l)?, conv(r)?))
}

/// Печатает значение с преобразованием к типу приёмника.
pub(super) fn value_in_target(
    value: &ExpressionNode,
    target: &TypeNode,
    model: &ModelNode,
) -> Result<String, Diagnostic> {
    let text = wrap_expr(value, model)?;
    let (
        TypeNode::Integer { bits, signed },
        Some(TypeNode::Integer {
            bits: from_bits,
            signed: from_signed,
        }),
    ) = (
        target,
        crate::generator::mixed_sign::operand_type_expr(value),
    )
    else {
        return Ok(text);
    };
    let (Some(to), Some(from)) = (
        crate::generator::st::st_type::iec_integer_name(*bits, *signed),
        crate::generator::st::st_type::iec_integer_name(from_bits, from_signed),
    ) else {
        return Ok(text);
    };
    if from == to {
        return Ok(text);
    }
    Ok(format!("{from}_TO_{to}({text})"))
}
