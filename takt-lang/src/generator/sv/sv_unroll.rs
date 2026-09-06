//! Разворот цикла `for` у цели `sv`.
//!
//! В синтезируемом RTL цикл обязан разворачиваться в схему, то есть иметь границы,
//! известные на этапе синтеза. Сам разбор тройки `init`/`cond`/`step` живёт в общем
//! носителе [`semantic::loop_bounds`](crate::semantic::loop_bounds): тот же факт нужен
//! подстановке тела с ранним возвратом, и второй разбор разошёлся бы с первым молча.
//!
//! **Предел итераций назван и мал** ([`MAX_ITERATIONS`]) - и он свойство цели, а не
//! языка: развёрнутый цикл есть **схема**, и тысяча итераций - тысяча копий тела. Отказ
//! с числом честнее, чем модуль, который не помещается в кристалл.

use crate::semantic::{ExpressionNode, StatementNode};

/// Предел числа итераций разворота.
///
/// Цифра - не свойство языка, а граница разумного размера схемы: 64 копии тела ещё
/// читаемы в порождённом модуле, дальше отказ полезнее.
pub(in crate::generator::sv) const MAX_ITERATIONS: usize = 64;

/// Развёрнутый цикл: имя переменной и значения, которые она принимает.
pub(in crate::generator::sv) struct Unrolled {
    /// Имя переменной цикла - ей присваивается значение перед каждой копией.
    pub name: String,
    /// Значения по итерациям, в порядке исполнения.
    pub values: Vec<i128>,
}

/// Разбирает `for` на статические границы; `None` - границы неизвестны либо итераций
/// больше [`MAX_ITERATIONS`].
pub(in crate::generator::sv) fn unroll(
    init: Option<&StatementNode>,
    cond: Option<&ExpressionNode>,
    step: Option<&ExpressionNode>,
) -> Option<Unrolled> {
    let bounds = crate::semantic::loop_bounds::bounds(init, cond, step, MAX_ITERATIONS)?;
    Some(Unrolled {
        name: bounds.name,
        values: bounds.values,
    })
}
