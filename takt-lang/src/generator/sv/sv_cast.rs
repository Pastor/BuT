//! Приведение `as` к целочисленному типу в цели `sv`.
//!
//! Вынесено из `sv_expr.rs` по границе ответственности: печать выражения отвечает на
//! вопрос "как выглядит операция", а этот модуль - "как выглядит смена типа". Поводом
//! был проверка размера модуля, границей - смысл.

use crate::diagnostics::Diagnostic;
use crate::generator::sv::sv_expr::sv002;
use crate::generator::sv::sv_expr::{Scope, print_expression};
use crate::semantic::ExpressionNode;
use crate::semantic::type_node::TypeNode;

/// Приведение к **целочисленному** типу - размерная форма `<W>'(выражение)`.
///
/// # Форма
///
/// - беззнаковая цель - `W'(expr)`: усечение старших разрядов и дополнение
///   нулями заданы стандартом, и это ровно правило (обёртка `mod 2ⁿ`);
/// - знаковая - `$signed(W'(expr))`: без `$signed` сравнение и арифметический
///   сдвиг работали бы как беззнаковые.
///
/// Ширина берётся у **цели** приведения. Форма без числа перед апострофом (`'(...)`)
/// означает "ширина по контексту" - молчаливое расширение до ширины приёмника, а не то,
/// что просит автор.
///
/// # Ошибки
///
/// `SV-002` - цель не скалярная (массив, структура): у такого приведения нет одной
/// ширины, и печатать его нечем.
pub(in crate::generator::sv) fn integer_cast(
    inner: &ExpressionNode,
    ty: &TypeNode,
    scope: &Scope,
) -> Result<String, Diagnostic> {
    let Some(width) = crate::generator::sv::sv_type::scalar_width(ty) else {
        return Err(sv002(
            "приведение типа (`as`) к нескалярному типу: у массива и структуры \
             нет одной ширины",
        ));
    };
    let value = print_expression(inner, scope)?;
    let sized = format!("{width}'({value})");
    Ok(match ty {
        TypeNode::Integer { signed: true, .. } => format!("$signed({sized})"),
        _ => sized,
    })
}

/// Знаково ли выражение - для выбора арифметического сдвига.
///
/// Признак **синтаксический и отестный**: он смотрит на объявленный тип операнда и на
/// явное приведение. Не узнав знака, отвечает `false`, то есть печатается прежний
/// логический сдвиг - ошибка в сторону прежнего поведения, а не в сторону нового.
pub(in crate::generator::sv) fn is_signed_expression(expr: &ExpressionNode) -> bool {
    match expr {
        ExpressionNode::Variable(var) => matches!(
            var.borrow().ty(),
            TypeNode::Integer { signed: true, .. } | TypeNode::Fixed { .. }
        ),
        ExpressionNode::Cast(_, ty) => matches!(
            ty,
            TypeNode::Integer { signed: true, .. } | TypeNode::Fixed { .. }
        ),
        ExpressionNode::Parenthesis(inner) | ExpressionNode::Negate(inner) => {
            is_signed_expression(inner)
        }
        _ => false,
    }
}

/// Оператор сдвига вправо: арифметический для знакового.
///
/// В SystemVerilog `>>` - **логический** сдвиг даже над `logic signed`: проба verilator
/// 2026-08-20 дала `-8 >> 1 = 124` против `-8 >>> 1 = -4`. Эталон, `c` и `rust` дают
/// −4, то есть цель расходилась значением молча.
pub(in crate::generator::sv) fn shift_right_operator(left: &ExpressionNode) -> &'static str {
    if is_signed_expression(left) {
        ">>>"
    } else {
        ">>"
    }
}

/// Целая степень с литеральным показателем - разворот в умножения.
///
/// # Ошибки
///
/// `SV-002` - показатель переменный либо отрицательный.
pub(in crate::generator::sv) fn power(
    base: &ExpressionNode,
    exp: &ExpressionNode,
    scope: &Scope,
) -> Result<String, Diagnostic> {
    let Some(value) = literal(exp) else {
        return Err(sv002(
            "возведение в степень с ПЕРЕМЕННЫМ показателем: в синтезируемом RTL \
             степень обязана разворачиваться в схему, то есть иметь показатель, \
             известный при синтезе",
        ));
    };
    if !(0..=64).contains(&value) {
        return Err(sv002(
            "возведение в такую степень: показатель обязан быть неотрицательным \
             и не больше 64 — разворот в умножения иначе не выразим",
        ));
    }
    if value == 0 {
        return Ok(String::from("1"));
    }
    let printed = print_expression(base, scope)?;
    let factors = std::iter::repeat_n(printed, usize::try_from(value).unwrap_or(1))
        .collect::<Vec<_>>()
        .join(" * ");
    Ok(format!("({factors})"))
}

/// Целое значение литерала показателя.
fn literal(expr: &ExpressionNode) -> Option<i128> {
    match expr {
        ExpressionNode::Number(v) => Some(*v),
        ExpressionNode::Parenthesis(inner) => literal(inner),
        _ => None,
    }
}
