//! Статически известные границы цикла `for`.
//!
//! Носитель отвечает на один вопрос: **сколько раз и с какими значениями** исполнится
//! `for`, если это известно до исполнения. Разбирается тройка:
//!
//! - `init` - объявление переменной цикла с **литеральным** началом;
//! - `cond` - сравнение **той же** переменной с литералом (`<`, `<=`, `>`,
//!   `>=`, `!=`);
//! - `step` - присваивание вида `k := k + литерал` либо `k := k - литерал`.
//!
//! Всё прочее - "границы неизвестны": гадать здесь нельзя.
//!
//! # Кто спрашивает
//!
//! - цель `sv` - цикл в синтезируемом RTL обязан разворачиваться в схему
//!   и она же кладёт свой предел числа итераций;
//! - подстановка тела с ранним возвратом - ей нужен другой факт:
//!   **завершается ли цикл независимо от тела**. Ранний выход подставляется
//!   обёрткой "выхода ещё не было" вокруг тела, и если условие продолжения
//!   зависело от тела, цикл стал бы бесконечным.
//!
//! Носитель один на оба вопроса намеренно: второй разбор тройки разошёлся бы с первым
//! молча, а цена расхождения - цикл, который цель развернула иначе, чем сочла
//! семантика.
//!
//! Значения считаются **тем же** способом, что исполнил бы эталон: шаг прибавляется к
//! текущему значению, условие проверяется перед телом. Ошибка здесь дала бы молча
//! другое число итераций - расхождение, которого не увидит ни один линтер.

use crate::semantic::{ExpressionNode, StatementNode};

/// Статически известный ход цикла: имя переменной и значения по итерациям.
#[derive(Debug, Clone)]
pub struct Bounds {
    /// Имя переменной цикла - ей присваивается значение перед каждой копией.
    pub name: String,
    /// Значения по итерациям, в порядке исполнения.
    pub values: Vec<i128>,
}

/// Разбирает `for` на статические границы; `None` - границы неизвестны либо итераций
/// больше `limit`.
///
/// `limit` принадлежит **спрашивающему**: у цели `sv` это размер схемы, у подстановки -
/// лишь защита от долгого счёта.
pub fn bounds(
    init: Option<&StatementNode>,
    cond: Option<&ExpressionNode>,
    step: Option<&ExpressionNode>,
    limit: usize,
) -> Option<Bounds> {
    let StatementNode::Variable(name, _, Some(start), _) = init? else {
        return None;
    };
    let mut value = literal(start)?;
    let (bound, compare) = comparison(cond?, name)?;
    let delta = increment(step?, name)?;
    let mut values = Vec::new();
    while compare(value, bound) {
        values.push(value);
        if values.len() > limit {
            return None;
        }
        value = value.checked_add(delta)?;
    }
    Some(Bounds {
        name: name.clone(),
        values,
    })
}

/// Целое значение литерала.
fn literal(expr: &ExpressionNode) -> Option<i128> {
    match expr {
        ExpressionNode::Number(v) => Some(*v),
        ExpressionNode::Parenthesis(inner) => literal(inner),
        _ => None,
    }
}

/// Предикат продолжения цикла: "текущее значение против границы".
type Continues = fn(i128, i128) -> bool;

/// Разбирает условие продолжения: граница и предикат сравнения.
fn comparison(cond: &ExpressionNode, name: &str) -> Option<(i128, Continues)> {
    let (left, right, compare): (_, _, Continues) = match cond {
        ExpressionNode::Less(l, r) => (l, r, |a, b| a < b),
        ExpressionNode::LessEqual(l, r) => (l, r, |a, b| a <= b),
        ExpressionNode::More(l, r) => (l, r, |a, b| a > b),
        ExpressionNode::MoreEqual(l, r) => (l, r, |a, b| a >= b),
        ExpressionNode::NotEqual(l, r) => (l, r, |a, b| a != b),
        _ => return None,
    };
    if !is_variable(left, name) {
        return None;
    }
    Some((literal(right)?, compare))
}

/// Разбирает шаг: `k := k + литерал` либо `k := k - литерал`.
fn increment(step: &ExpressionNode, name: &str) -> Option<i128> {
    let ExpressionNode::Assign(target, value) = step else {
        return None;
    };
    if !is_variable(target, name) {
        return None;
    }
    match value.as_ref() {
        ExpressionNode::Add(l, r) if is_variable(l, name) => literal(r),
        ExpressionNode::Subtract(l, r) if is_variable(l, name) => Some(-literal(r)?),
        _ => None,
    }
}

/// Ссылается ли выражение на переменную цикла.
fn is_variable(expr: &ExpressionNode, name: &str) -> bool {
    match expr {
        ExpressionNode::Variable(var) => var.borrow().name() == name,
        ExpressionNode::Parenthesis(inner) => is_variable(inner, name),
        _ => false,
    }
}
