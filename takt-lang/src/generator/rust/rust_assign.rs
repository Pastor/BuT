//! Печать присваивания и обёрточной арифметики - цель `rust`.
//!
//! Выделено из `rust_expr` по границе **ответственности**: печать выражения отвечает на
//! вопрос "как выглядит значение", а этот модуль - на вопрос "как выглядит запись
//! значения в место" (порт, разряд, переменная) и "когда арифметика печатается
//! обёрткой".
//!
//! Свёртка `x := x + 1` живёт здесь же: она есть свойство записи, а не значения -
//! `clippy::assign_op_pattern` отвергает развёрнутую форму под `-D warnings`.

use super::rust_expr::{
    Scope, coerce_to, expression_type, is_wrapping_arith, print_expression, unwrap_outer,
    write_port,
};
use crate::diagnostics::Diagnostic;
use crate::generator::rust::rust_fixed;
use crate::parser::ast::Member;
use crate::semantic::ExpressionNode;
use crate::semantic::VariableNode;
use crate::semantic::type_node::TypeNode;

/// Тип приёмника, знающий поля структуры.
///
/// Общий `expression_type` отвечает на поле `None` осознанно: объявление структуры
/// лежит в модели, а туда он не смотрит. Здесь модель есть - она в [`Scope`], - поэтому
/// вопрос решается на месте, а не переписыванием носителя.
fn target_type_with_fields(target: &ExpressionNode, scope: &Scope) -> Option<TypeNode> {
    if let ExpressionNode::BitAccess(base, Member::Identifier(field)) = target
        && let Some(TypeNode::Struct(name)) = target_type_with_fields(base, scope)
        && let Some(def) = scope.model.search_struct(&name)
    {
        return def
            .fields
            .iter()
            .find(|(fname, _)| fname.as_str() == field.name)
            .map(|(_, ty)| ty.clone());
    }
    expression_type(target)
}

/// Печатает присваивание; запись в порт превращает в вызов HAL.
pub(crate) fn assign(
    target: &ExpressionNode,
    value: &ExpressionNode,
    scope: &Scope,
) -> Result<String, Diagnostic> {
    if let ExpressionNode::Variable(var) = target {
        let borrowed = var.borrow();
        if let VariableNode::Port {
            name,
            ty,
            direction,
            loc,
            ..
        } = &*borrowed
        {
            let printed = coerce_to(value, ty, scope)?;
            return write_port(name, ty, *direction, unwrap_outer(&printed), scope, *loc);
        }
        if let VariableNode::Const { name, loc, .. } = &*borrowed {
            return Err(Diagnostic::error(
                *loc,
                format!("Присваивание в константу '{}' недопустимо", name),
            )
            .with_code("RS-019"));
        }
    }
    // Запись одного разряда.
    if let ExpressionNode::BitAccess(inner, Member::Number(bit)) = target {
        return crate::generator::rust::rust_bit::assign_bit(inner, *bit, value, scope);
    }
    let target_text = print_expression(target, scope)?;
    // Не косметика: clippy считает `x = x + 1` ручной реализацией составного
    // присваивания (`assign_op_pattern`) и под `-D warnings` отвергает. Совпадение
    // операнда проверяется по напечатанному тексту, а не по узлам: текст - это ровно
    // то, что увидит компилятор.
    if let Some(compound) = compound_assign(&target_text, value, scope)? {
        return Ok(compound);
    }
    // Тип приёмника - с учётом полей структуры: `conf.mode := Run;` приходит как
    // `Number(1)`, и без типа поля вариант печатался числом - `rustc` отвечал `E0308:
    // mismatched types` при НУЛЕВОМ коде возврата `taktc`. Тот же класс у цели `sv`
    // (`ENUMVALUE`) и у элемента массива: "тип приёмника известен - значение печатается
    // по нему".
    let ty = target_type_with_fields(target, scope);
    let printed = match &ty {
        Some(ty) => coerce_to(value, ty, scope)?,
        None => print_expression(value, scope)?,
    };
    // Присваиваемое значение - ещё одна позиция, где внешние скобки лишние: `x = (a -
    // b);` даёт `unnecessary parentheses around assigned value`.
    Ok(format!("{} = {}", target_text, unwrap_outer(&printed)))
}

/// Строит составное присваивание (`x += 1`), если значение имеет форму `x op ...`.
fn compound_assign(
    target_text: &str,
    value: &ExpressionNode,
    scope: &Scope,
) -> Result<Option<String>, Diagnostic> {
    // Q-арифметика не сворачивается: `x := x * y` над q - это масштабный `takt_q`-путь,
    // а не нативное `x *= y` (то дало бы целочисленное умножение представлений без
    // сдвига на n - молча неверный результат и паника на переполнении в debug).
    if rust_fixed::fixed_format_in(value, scope.model).is_some() {
        return Ok(None);
    }
    // Беззнаковая арифметика печатается обёрткой (`wrapping_*`): свернуть её в `x += 1`
    // нельзя - `+=` в debug паникует на переполнении, а правило языка требует обёртки
    // mod 2^N.
    if is_wrapping_arith(value) {
        return Ok(None);
    }
    let (op, lhs, rhs) = match value {
        ExpressionNode::Add(a, b) => ("+=", a, b),
        ExpressionNode::Subtract(a, b) => ("-=", a, b),
        ExpressionNode::Multiply(a, b) => ("*=", a, b),
        ExpressionNode::Divide(a, b) => ("/=", a, b),
        ExpressionNode::Modulo(a, b) => ("%=", a, b),
        ExpressionNode::BitwiseAnd(a, b) => ("&=", a, b),
        ExpressionNode::BitwiseOr(a, b) => ("|=", a, b),
        ExpressionNode::BitwiseXor(a, b) => ("^=", a, b),
        ExpressionNode::ShiftLeft(a, b) => ("<<=", a, b),
        ExpressionNode::ShiftRight(a, b) => (">>=", a, b),
        _ => return Ok(None),
    };
    // Печать операнда здесь - Предикат, а не вывод: её отказ значит "свернуть нельзя",
    // и настоящую диагностику даст печать значения ниже по пути, где известен приёмник.
    //
    // Глотание ограничено одним сравнением: печать самого значения (`rhs_text` и путь
    // `assign`) ошибку по-прежнему пробрасывает, иначе повторился бы - печатник,
    // теряющий оператор молча.
    match print_expression(lhs, scope) {
        Ok(text) if text == target_text => {}
        _ => return Ok(None),
    }
    let rhs_text = print_expression(rhs, scope)?;
    Ok(Some(format!(
        "{} {} {}",
        target_text,
        op,
        unwrap_outer(&rhs_text)
    )))
}
