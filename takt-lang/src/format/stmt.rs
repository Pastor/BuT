//! Печать операторов и блоков.

use super::{FormatError, Out, expr};
use crate::parser::ast;

/// Печатает блок с заголовком: `<head>{ ... }` с телом на отдельных строках.
///
/// Используется и для именованных блоков (`enter`/`exit`/`always`), и для тел функций -
/// раскладка у них одна.
pub(crate) fn block_with_head(
    out: &mut Out,
    head: &str,
    statement: &ast::Statement,
) -> Result<(), FormatError> {
    let ast::Statement::Block { statements, .. } = statement else {
        // Тело не-блок: печатаем как одиночный оператор в фигурных скобках, чтобы канон
        // был единообразным.
        out.line(&format!("{head}{{"));
        out.up();
        print(out, statement)?;
        out.down();
        out.line("}");
        return Ok(());
    };
    if statements.is_empty() {
        out.line(&format!("{head}{{}}"));
        return Ok(());
    }
    out.line(&format!("{head}{{"));
    out.up();
    for s in statements {
        print(out, s)?;
    }
    // Комментарий последней строкой тела: следующего оператора нет, и без этого вызова
    // его выдал бы `leading()` следующего элемента - уже за скобкой, привязанным к
    // чужому узлу.
    close_body(out, statement);
    out.down();
    out.line("}");
    Ok(())
}

/// Печатает комментарии, оставшиеся внутри тела, перед его закрывающей скобкой.
fn close_body(out: &mut Out, block: &ast::Statement) {
    let ast::Statement::Block { loc, .. } = block else {
        return;
    };
    if let Some((_, end)) = super::comments::span(loc) {
        out.comments_before(end);
    }
}

/// Печатает оператор.
///
/// `ast::Statement` помечен `#[non_exhaustive]`, поэтому ветка `_` **вынужденная** (как
/// `TypeNode` в ). Она возвращает **ошибку**: неизвестный оператор нельзя молча
/// выбросить из исходника пользователя.
pub(crate) fn print(out: &mut Out, statement: &ast::Statement) -> Result<(), FormatError> {
    // Ведущие комментарии и пустая строка выдаются здесь, до печати самого оператора.
    //
    // Для неблочных это не двойная выдача: `leading` потребляет комментарии до
    // смещения, и повторный вызов внутри `node_line` уже ничего не находит.
    let loc = statement.loc();
    out.blank_before(&loc);
    out.leading_for(&loc);
    print_inner(out, statement)
}

/// Печатает сам оператор - без выдачи ведущих комментариев.
///
/// Отдельная точка нужна ветви `else`: там строка ждёт продолжения (`} else `), и
/// выдать в неё комментарий значило бы разорвать K&R-раскладку.
#[allow(clippy::wildcard_enum_match_arm)]
fn print_inner(out: &mut Out, statement: &ast::Statement) -> Result<(), FormatError> {
    use ast::Statement as S;
    match statement {
        S::Block { statements, .. } => {
            out.line("{");
            out.up();
            for s in statements {
                print(out, s)?;
            }
            close_body(out, statement);
            out.down();
            out.line("}");
            Ok(())
        }
        S::Expression(loc, e) => {
            out.node_line(loc, &format!("{};", expr::expression(e)?));
            Ok(())
        }
        S::Variable(loc, define, init) => {
            let head = expr::variable_define(define)?;
            match init {
                Some(init) => {
                    out.node_line(loc, &format!("{head} := {};", expr::expression(init)?))
                }
                None => out.node_line(loc, &format!("{head};")),
            }
            Ok(())
        }
        // K&R-раскладка канона стиля: `}` и `else` - на одной строке. Цепочка
        // печатается тем же приёмом: закрывающую скобку ветки `then` уже поставил
        // `block_with_head`, `join` продолжает её строку, а заголовок следующей ветки
        // печатается без отступа - блоком (`{`) или рекурсивным `print` (`if cond {`).
        // Отдельной печати для `else if` больше не нужно: прежде она давала три строки
        // (`}` / `else` / `if ...`).
        S::If(_, cond, then_, else_) => {
            block_with_head(out, &format!("if {} ", expr::expression(cond)?), then_)?;
            let Some(else_) = else_ else {
                return Ok(());
            };
            out.join(" else ");
            if matches!(**else_, S::If(..)) {
                print_inner(out, else_)
            } else {
                // Заголовка у простого `else` нет вовсе: `{` даёт сам блок.
                block_with_head(out, "", else_)
            }
        }
        S::Loop(_, cond, body, keyword) => {
            // Печатаем слово автора: `while` - синоним `loop`, и переписывать одно в
            // другое форматтер не вправе.
            let word = match keyword {
                ast::LoopKeyword::Loop => "loop",
                ast::LoopKeyword::While => "while",
            };
            let head = match cond {
                Some(cond) => format!("{word} {} ", expr::expression(cond)?),
                None => format!("{word} "),
            };
            block_with_head(out, &head, body)
        }
        S::For(_, init, cond, step, body) => {
            let init = match init {
                Some(init) => single_line(init)?,
                None => ";".to_string(),
            };
            let cond = match cond {
                Some(cond) => expr::expression(cond)?,
                None => String::new(),
            };
            let step = match step {
                Some(step) => expr::expression(step)?,
                None => String::new(),
            };
            let head = format!("for {init} {cond}; {step} ");
            match body {
                Some(body) => block_with_head(out, &head, body),
                None => {
                    out.line(&format!("{head}{{}}"));
                    Ok(())
                }
            }
        }
        S::Continue(loc) => {
            out.node_line(loc, "continue;");
            Ok(())
        }
        S::Break(loc) => {
            out.node_line(loc, "break;");
            Ok(())
        }
        S::Return(loc, value) => {
            match value {
                Some(value) => out.node_line(loc, &format!("return {};", expr::expression(value)?)),
                None => out.node_line(loc, "return;"),
            }
            Ok(())
        }
        S::Match(loc, subject, arms) => {
            out.node_line(loc, &format!("match {} {{", expr::expression(subject)?));
            out.up();
            for arm in arms {
                let patterns = arm
                    .patterns
                    .iter()
                    .map(|p| match p {
                        ast::MatchPattern::Wildcard(_) => Ok("_".to_string()),
                        ast::MatchPattern::Value(e) => expr::expression(e),
                    })
                    .collect::<Result<Vec<_>, FormatError>>()?
                    // Разделитель образцов - Запятая (`CommaOne<MatchPattern>` в
                    // грамматике). Печать через `|` меняла смысл: `0 | 1` читается
                    // обратно как один образец-выражение (побитовое или), а не как два.
                    .join(", ");
                block_with_head(out, &format!("{patterns} => "), &arm.body)?;
            }
            out.down();
            out.line("}");
            Ok(())
        }
        // 0044: `assert` языка Takt в блоке кода (`: c;` / `: [Guard] c;`). Форма
        // автора сохраняется печатью инлайн-формулы (синонимы не канонизируются).
        S::InlineFormula(f) => {
            out.line(&expr::inline_formula(f)?);
            Ok(())
        }
        // Вставка на языке цели: `assembly "c" { ... }`. Тело - обычный блок операторов
        // Takt, поэтому печатается общей раскладкой.
        S::Assembly { dialect, block, .. } => {
            let head = match dialect {
                Some(dialect) => format!("assembly {} ", expr::one_string(dialect)),
                None => "assembly ".to_string(),
            };
            block_with_head(out, &head, block)
        }
        // Блок формул в теле: свой язык, своя печать.
        S::Formula { dialect, block, .. } => {
            super::formula::print_block(out, dialect.as_ref(), block)
        }
        // Вынужденная ветка: перечисление `#[non_exhaustive]`. Отказ, а не молчаливая
        // потеря оператора. Позиция и **название вида** берутся у самого узла: прежде
        // здесь печатался `Debug`-дамп со всей внутренней структурой оператора.
        other => Err(super::unsupported(other.loc(), other.kind_name())),
    }
}

/// Печатает оператор в одну строку (для заголовка `for`).
fn single_line(statement: &ast::Statement) -> Result<String, FormatError> {
    use ast::Statement as S;
    match statement {
        S::Variable(_, define, init) => {
            let head = expr::variable_define(define)?;
            Ok(match init {
                Some(init) => format!("{head} := {};", expr::expression(init)?),
                None => format!("{head};"),
            })
        }
        S::Expression(_, e) => Ok(format!("{};", expr::expression(e)?)),
        other => Err(super::unsupported(
            other.loc(),
            &format!("{} в инициализаторе for", other.kind_name()),
        )),
    }
}
