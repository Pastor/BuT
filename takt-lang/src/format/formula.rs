//! Печать блока `formula`.
//!
//! ## Канон
//!
//! Заголовок - `formula { ` либо `formula "диалект" { `, тело - по оператору на строке
//! с обычным отступом, закрывающая скобка на своей строке. Вложенный блок печатается
//! тем же правилом. Точки с запятой в этом языке нет - её нет и в грамматике.
//!
//! **Диалект печатается**: до грамматика его разбирала и выбрасывала, поэтому `formula
//! "smt" { ... }` вернулся бы из форматтера без `"smt"` - тихая потеря исходника, ровно
//! то, ради чего форматтер и отказывал на этом узле.

use super::{FormatError, Out};
use crate::parser::ast;

/// Печатает блок формул с заголовком (`formula` уровня модели или тела).
pub(crate) fn print_block(
    out: &mut Out,
    dialect: Option<&ast::StringLiteral>,
    block: &ast::FormulaBlock,
) -> Result<(), FormatError> {
    let head = match dialect {
        Some(dialect) => format!("formula {} ", super::expr::one_string(dialect)),
        None => "formula ".to_string(),
    };
    print_body(out, &head, block)
}

/// Печатает тело блока: `{`, операторы с отступом, `}`.
fn print_body(out: &mut Out, head: &str, block: &ast::FormulaBlock) -> Result<(), FormatError> {
    if block.statements.is_empty() {
        out.node_line(&block.loc, &format!("{head}{{}}"));
        return Ok(());
    }
    out.node_line(&block.loc, &format!("{head}{{"));
    out.up();
    for statement in &block.statements {
        print_statement(out, statement)?;
    }
    // Комментарий последней строкой тела принадлежит этому телу: без явной выдачи его
    // подхватил бы `leading()` следующего элемента - уже за скобкой.
    if let Some((_, end)) = super::comments::span(&block.loc) {
        out.comments_before(end.saturating_sub(1));
    }
    out.down();
    out.line("}");
    Ok(())
}

/// Печатает оператор блока формул.
fn print_statement(out: &mut Out, statement: &ast::FormulaStatement) -> Result<(), FormatError> {
    match statement {
        ast::FormulaStatement::Block(block) => print_body(out, "", block),
        // Грамматика этот вариант не строит, но печать у него очевидна, и
        // отказ здесь означал бы отказ на законной записи, появись для неё правило.
        ast::FormulaStatement::Expression(loc, node) => {
            out.node_line(loc, &expression(node)?);
            Ok(())
        }
        ast::FormulaStatement::Function(function) => {
            out.node_line(&function.loc, &call(function)?);
            Ok(())
        }
        // Узел восстановления после ошибки: до печати он не доходит - форматтер
        // работает по разобранному дереву, а разбор с ошибкой отдаёт `Err`. Молчаливо
        // пропустить его нельзя: это была бы потеря куска исходника.
        ast::FormulaStatement::Error(loc) => {
            Err(super::unsupported(*loc, "ошибка в блоке formula"))
        }
    }
}

/// Печатает вызов функции формулы: `имя(аргумент, ...)`.
fn call(function: &ast::FormulaFunction) -> Result<String, FormatError> {
    let arguments = function
        .arguments
        .iter()
        .map(expression)
        .collect::<Result<Vec<_>, FormatError>>()?
        .join(", ");
    Ok(format!("{}({arguments})", function.id.name))
}

/// Печатает выражение блока формул.
///
/// Аннотация типа (`42:тип`) - часть записи автора и сохраняется: у этого языка своя
/// система типов, и форматтер не вправе решать, нужна ли она.
fn expression(node: &ast::FormulaExpression) -> Result<String, FormatError> {
    use ast::FormulaExpression as F;
    Ok(match node {
        F::Bool(_, value, ty) => format!("{value}{}", annotation(ty)),
        F::Number(loc, value, ty) => {
            format!("{}{}", super::literal::number(*loc, *value), annotation(ty))
        }
        F::String(literal, ty) => {
            format!("{}{}", super::expr::one_string(literal), annotation(ty))
        }
        F::Variable(id) => id.name.clone(),
        F::Function(function) => call(function)?,
        F::SuffixAccess(_, base, member) => format!("{}.{}", expression(base)?, member.name),
        F::Parenthesis(_, inner) => format!("({})", expression(inner)?),
    })
}

/// Аннотация типа литерала: `:имя` либо пустая строка.
fn annotation(ty: &Option<ast::Identifier>) -> String {
    match ty {
        Some(ty) => format!(":{}", ty.name),
        None => String::new(),
    }
}
