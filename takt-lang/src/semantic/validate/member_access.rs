//! Проверка доступа к члену: поле структуры (`SE-061`) и **разряд за объявленной
//! шириной** (`SE-125`).
//!
//! Доступ `p.field` к несуществующему полю структуры прежде **не проверялся**
//! семантикой: генератор C печатал `model->p.NOSUCHFIELD` молча, и ошибку ловил лишь
//! `cc` ("no member named ...") - поздно и невнятно. Симулятор ловит то же на
//! исполнении (`SIM-027`). Этот проход даёт **компайл-тайм** диагностику в `takt-lang`,
//! единую для всех целей.
//!
//! Проверка **консервативна**: срабатывает только когда база доступа надёжно
//! разрешается в **структурный** тип, а поля в нём нет. Неразрешимую базу (напр.
//! элемент массива структур) пропускаем - ложное срабатывание хуже пропуска, а
//! `cc`/симулятор остаются страховкой.
//!
//! # Разряд за объявленной шириной (`SE-125`)
//!
//! Оба доступа - по имени и по номеру - суть один вид узла (`BitAccess`), поэтому
//! проверяет их **один** обход.
//!
//! Замер 2026-08-22 на `var w: [bit;96]; w.100 := 1;`: эталон пишет,
//! `c`/`c-hal` и `rust` печатают и их инструменты принимают, `st` отказывает
//! (широких векторов не переводит), а **`verilator` вывод `sv` отвергает**
//! (`SELRANGE: Selection index out of range: 100:100 outside 95:0`) при
//! **нулевом** коде возврата `taktc`: цель печатает регистр шириной по
//! объявлению.
//!
//! объявленная ширина - **контракт**, и разряд за ней есть ошибка. Раскладка по словам
//! (`[bit;96]` занимает два слова) - деталь представления, о которой автор знать не
//! обязан.
//!
//! **Граница - выражение, чей тип не выводится** (результат вызова и т. п.): проверка
//! консервативна, и там остаётся `SIM-011` эталона. Заявленной в границы "переменный
//! индекс" в языке **нет вовсе**: `w.idx` разбирается как доступ к полю с именем `idx` -
//! показал замер 2026-08-23.

use super::*;
use crate::parser::ast::{Identifier, Member};
use crate::semantic::type_node::TypeNode;
use crate::semantic::validate::base_type::{base_type, cond_base_type};

/// Ce19 (`SE-061`): доступ к несуществующему полю структуры.
///
/// Обходит инициализаторы переменных, условия, тела именованных блоков модели и
/// состояний, тела функций и условия рёбер `ref`; на каждом `x.field` (где `x`
/// разрешается в структуру) проверяет наличие поля.
pub fn check_struct_field_access(model: Rc<RefCell<ModelNode>>) -> Result<(), Diagnostic> {
    let borrowed = model.borrow();

    for var in borrowed.variables.values() {
        if let Some(expr) = var_initializer(var) {
            check_expr(expr, &borrowed)?;
        }
    }
    for cond in borrowed.conditions.values() {
        check_cond(&cond.value, &borrowed)?;
    }
    for block in &borrowed.named_blocks {
        if let Some(stmt) = block.statement() {
            check_stmt(stmt, &borrowed)?;
        }
    }
    for func in borrowed.functions.values() {
        if let FunctionDefinitionNode::Local { body, .. } = func {
            check_stmt(body, &borrowed)?;
        }
    }
    for state in borrowed.states.values() {
        for block in state.named_blocks() {
            if let Some(stmt) = block.statement() {
                check_stmt(stmt, &borrowed)?;
            }
        }
        for reference in state.references() {
            check_cond(&reference.cond, &borrowed)?;
        }
    }
    Ok(())
}

fn var_initializer(var: &VariableNode) -> Option<&ExpressionNode> {
    match var {
        VariableNode::Simple { expr, .. } | VariableNode::Const { expr, .. } => {
            if matches!(expr, ExpressionNode::None) {
                None
            } else {
                Some(expr)
            }
        }
        _ => None,
    }
}

/// Если `x.N` выходит за **объявленную** ширину - диагностика `SE-125`.
///
/// Ширина берётся у типа базы: у бит-вектора - объявленное число разрядов (носитель
/// `bit_vector::is_bit_vector`), у целого - его биты. Прочие типы пропускаются: у них
/// разряд судит `SIM-011`/`SE-030`, и второе правило здесь разошлось бы с первым.
///
/// Отрицательный номер разряда грамматика не строит: `.` принимает `number`, а
/// переменного номера в языке нет вовсе (`x.i` - доступ к полю `i`).
fn check_bit_index(
    inner: &ExpressionNode,
    index: i128,
    loc: crate::diagnostics::Location,
    model: &ModelNode,
) -> Result<(), Diagnostic> {
    let Some(ty) = base_type(inner, model) else {
        return Ok(());
    };
    let width = match crate::semantic::bit_vector::is_bit_vector(&ty) {
        Some(bits) => i128::from(bits),
        None => match ty {
            TypeNode::Integer { bits, .. } => i128::from(bits),
            _ => return Ok(()),
        },
    };
    if index < width {
        return Ok(());
    }
    Err(Diagnostic::error(
        loc,
        format!(
            "разряд {index} за объявленной шириной значения ({width} бит): \
             обращаться можно к разрядам 0..{}",
            width - 1
        ),
    )
    .with_code("SE-125"))
}

/// Если `x.field` обращается к структуре без такого поля - диагностика `SE-061`.
fn check_member(
    inner: &ExpressionNode,
    field: &Identifier,
    model: &ModelNode,
) -> Result<(), Diagnostic> {
    match base_type(inner, model) {
        Some(TypeNode::Struct(name)) => {
            if let Some(s) = model.search_struct(&name)
                && !s.fields.iter().any(|(f, _)| *f == field.name)
            {
                return Err(Diagnostic::error(
                    field.loc,
                    format!("структура '{}' не содержит поля '{}'", name, field.name),
                )
                .with_code("SE-061"));
            }
        }
        // Поле у значения, которое структурой не является: `x.foo` при `x: u8`
        // компилятор принимал молча, а эталон отвечал `SIM-012` в такте. Тип базы здесь
        // известен, и ответ обязан быть тем же, что у индексации не массива (`SE-030`).
        //
        // Разряд (`x.3`) под правило не подпадает: там член - Число, и его судит
        // `SE-125`. Тип, который носитель не выводит, молчит по-прежнему - за пропуском
        // стоит диагностика эталона, за ложным отказом незаконно отвергнутая программа.
        Some(ty)
            if !matches!(
                ty,
                TypeNode::Inference | TypeNode::Unsupported | TypeNode::Unit
            ) =>
        {
            return Err(Diagnostic::error(
                field.loc,
                format!(
                    "обращение к полю '{}' возможно только у структуры",
                    field.name
                ),
            )
            .with_code("SE-030"));
        }
        _ => {}
    }
    Ok(())
}

fn check_expr(expr: &ExpressionNode, model: &ModelNode) -> Result<(), Diagnostic> {
    match expr {
        ExpressionNode::BitAccess(inner, Member::Identifier(field)) => {
            check_member(inner, field, model)?;
            check_expr(inner, model)?;
        }
        // Разряд за объявленной шириной (`SE-125`). Позиция - у самой базы: у номера
        // разряда своей координаты в АСД нет.
        ExpressionNode::BitAccess(inner, Member::Number(index)) => {
            check_bit_index(inner, *index, inner.loc(), model)?;
            check_expr(inner, model)?;
        }
        ExpressionNode::Not(e)
        | ExpressionNode::BitwiseNot(e)
        | ExpressionNode::UnaryPlus(e)
        | ExpressionNode::Negate(e)
        | ExpressionNode::Parenthesis(e)
        // `BitAccess` разобран выше - обе формы члена (имя и номер) имеют свои ветви, и
        // общей здесь больше не нужно.
        | ExpressionNode::CodeBlock(e, _)
        | ExpressionNode::NamedFunctionBox(e, _)
        | ExpressionNode::Cast(e, _) => check_expr(e, model)?,
        ExpressionNode::Add(l, r)
        | ExpressionNode::Subtract(l, r)
        | ExpressionNode::Multiply(l, r)
        | ExpressionNode::Divide(l, r)
        | ExpressionNode::Modulo(l, r)
        | ExpressionNode::Power(l, r)
        | ExpressionNode::BitwiseAnd(l, r)
        | ExpressionNode::BitwiseXor(l, r)
        | ExpressionNode::BitwiseOr(l, r)
        | ExpressionNode::ShiftLeft(l, r)
        | ExpressionNode::ShiftRight(l, r)
        | ExpressionNode::And(l, r)
        | ExpressionNode::Or(l, r)
        | ExpressionNode::Equal(l, r)
        | ExpressionNode::NotEqual(l, r)
        | ExpressionNode::Less(l, r)
        | ExpressionNode::More(l, r)
        | ExpressionNode::LessEqual(l, r)
        | ExpressionNode::MoreEqual(l, r)
        | ExpressionNode::Assign(l, r) => {
            check_expr(l, model)?;
            check_expr(r, model)?;
        }
        ExpressionNode::ConditionalOperator(c, t, e) => {
            check_expr(c, model)?;
            check_expr(t, model)?;
            check_expr(e, model)?;
        }
        ExpressionNode::Array(items)
        | ExpressionNode::Initializer(items)
        | ExpressionNode::Function(_, items) => {
            for item in items {
                check_expr(item, model)?;
            }
        }
        _ => {}
    }
    Ok(())
}

fn check_cond(cond: &ConditionNode, model: &ModelNode) -> Result<(), Diagnostic> {
    match cond {
        // Разряд за объявленной шириной - и в условии тоже: печатников у условий и
        // выражений два, значит и проверка обязана стоять в обоих обходах.
        ConditionNode::BitAccess(inner, Member::Number(index)) => {
            if let Some(base) = cond_base_type(inner, model) {
                let width = match crate::semantic::bit_vector::is_bit_vector(&base) {
                    Some(bits) => Some(i128::from(bits)),
                    None => match base {
                        TypeNode::Integer { bits, .. } => Some(i128::from(bits)),
                        _ => None,
                    },
                };
                if let Some(width) = width
                    && *index >= width
                {
                    return Err(Diagnostic::error(
                        crate::diagnostics::Location::Codegen,
                        format!(
                            "разряд {index} за объявленной шириной значения ({width} бит): \
                             обращаться можно к разрядам 0..{}",
                            width - 1
                        ),
                    )
                    .with_code("SE-125"));
                }
            }
            check_cond(inner, model)?;
        }
        ConditionNode::BitAccess(inner, Member::Identifier(field)) => {
            // База условия - выражение; для проверки поля переиспользуем разбор
            // выражений через мостовую конвертацию не нужен: доступ к полю в условии
            // несёт `ConditionNode`, у которого база - тоже условие.
            check_cond_member(inner, field, model)?;
            check_cond(inner, model)?;
        }
        // `BitAccess` разобран выше обеими формами члена.
        ConditionNode::Not(c) | ConditionNode::Parenthesis(c) => check_cond(c, model)?,
        ConditionNode::And(l, r)
        | ConditionNode::Or(l, r)
        | ConditionNode::Equal(l, r)
        | ConditionNode::NotEqual(l, r)
        | ConditionNode::Less(l, r)
        | ConditionNode::More(l, r)
        | ConditionNode::LessEqual(l, r)
        | ConditionNode::MoreEqual(l, r)
        | ConditionNode::Add(l, r)
        | ConditionNode::Subtract(l, r) => {
            check_cond(l, model)?;
            check_cond(r, model)?;
        }
        ConditionNode::Function(_, args, _) => {
            for arg in args {
                check_cond(arg, model)?;
            }
        }
        _ => {}
    }
    Ok(())
}

fn check_cond_member(
    inner: &ConditionNode,
    field: &Identifier,
    model: &ModelNode,
) -> Result<(), Diagnostic> {
    if let Some(TypeNode::Struct(name)) = cond_base_type(inner, model)
        && let Some(s) = model.search_struct(&name)
        && !s.fields.iter().any(|(f, _)| *f == field.name)
    {
        return Err(Diagnostic::error(
            field.loc,
            format!("структура '{}' не содержит поля '{}'", name, field.name),
        )
        .with_code("SE-061"));
    }
    Ok(())
}

fn check_stmt(stmt: &StatementNode, model: &ModelNode) -> Result<(), Diagnostic> {
    match stmt {
        StatementNode::Block(stmts) => {
            for s in stmts {
                check_stmt(s, model)?;
            }
        }
        StatementNode::Expression(e, _) => check_expr(e, model)?,
        StatementNode::If {
            cond, then_, else_, ..
        } => {
            check_expr(cond, model)?;
            check_stmt(then_, model)?;
            if let Some(e) = else_ {
                check_stmt(e, model)?;
            }
        }
        StatementNode::Loop { cond, body, .. } => {
            if let Some(c) = cond {
                check_expr(c, model)?;
            }
            check_stmt(body, model)?;
        }
        StatementNode::For {
            init,
            cond,
            step,
            body,
            ..
        } => {
            if let Some(s) = init {
                check_stmt(s, model)?;
            }
            if let Some(c) = cond {
                check_expr(c, model)?;
            }
            if let Some(s) = step {
                check_expr(s, model)?;
            }
            check_stmt(body, model)?;
        }
        StatementNode::Variable(_, _, Some(e), _) => check_expr(e, model)?,
        StatementNode::Return(Some(e), _) => check_expr(e, model)?,
        StatementNode::Match { expr, arms, .. } => {
            check_expr(expr, model)?;
            for arm in arms {
                check_stmt(&arm.body, model)?;
            }
        }
        _ => {}
    }
    Ok(())
}
