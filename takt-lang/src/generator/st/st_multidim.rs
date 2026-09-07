//! Многомерный массив у цели `st`: согласование формы с объявлением.
//!
//! В IEC 61131-3 массивы не вкладываются, поэтому `st_type` печатает `[[u8; 2]; 2]`
//! многомерной формой `ARRAY [0..1, 0..1] OF USINT`. Всё остальное обязано следовать
//! этому объявлению - иначе `iec2c` отвергает вывод ("Number of subscripts/indexes does
//! not match") либо, что хуже, принимает его и теряет данные молча.
//!
//! Здесь живут оба носителя такого согласования: сбор цепочки индексаций и уплощение
//! агрегата-инициализатора. Модуль отдельный, потому что правило одно, а звать его
//! приходится из двух печатников выражений и из печатника объявлений.

use crate::diagnostics::Diagnostic;
use crate::semantic::type_node::TypeNode;
use crate::semantic::{ConditionNode, ExpressionNode, ModelNode};

use super::st_cond::print_condition;
use super::st_decl::literal_init;
use super::st_expr::print_expression;

/// Собирает цепочку индексаций в корень и список индексов.
///
/// `grid[1][0]` приходит деревом `Subscript(Subscript(grid, 1), 0)`, а в IEC 61131-3
/// вложенных массивов **нет**: `st_type` печатает `[[u8; 2]; 2]` многомерной формой
/// `ARRAY [0..1, 0..1] OF USINT`, и индексация обязана следовать объявлению - `grid[1,
/// 0]`.
///
/// Цепочка обрывается на **любом** другом узле, поэтому `ps[1].x[0]` (массив структур с
/// полем-массивом) остаётся двумя раздельными индексациями: там это два разных массива,
/// и объединять их индексы нельзя.
pub(crate) fn expression_subscript_chain(
    expr: &ExpressionNode,
    model: &ModelNode,
) -> Result<(String, Vec<String>), Diagnostic> {
    let mut indices = Vec::new();
    let mut current = expr;
    while let ExpressionNode::ArraySubscript(base, index) = current {
        indices.push(print_expression(index, model)?);
        current = base;
    }
    indices.reverse();
    Ok((print_expression(current, model)?, indices))
}

/// Тот же сбор для условий: печатников **два**, и правка одного чинит половину входов.
pub(crate) fn condition_subscript_chain(
    cond: &ConditionNode,
    model: &ModelNode,
) -> Result<(String, Vec<String>), Diagnostic> {
    let mut indices = Vec::new();
    let mut current = cond;
    while let ConditionNode::ArraySubscript(base, index) = current {
        indices.push(print_condition(index, model)?);
        current = base;
    }
    indices.reverse();
    Ok((print_condition(current, model)?, indices))
}

/// Элементы агрегата массива плоским списком.
///
/// Тип многомерного массива уплощается (`[[u8; 2]; 2]` -> `ARRAY [0..1, 0..1] OF
/// USINT`, T12), и инициализатор обязан следовать объявлению. Вложенную форму `[[1, 2],
/// [3, 4]]` `iec2c` **принимает**, но переводит в `{{1,2,}}` - вторая строка теряется
/// **молча**; замер 2026-08-21 дал `other = 0` у цели `st` против `3` у эталона при
/// нулевом коде возврата `taktc`. Форма `[1, 2, 3, 4]` проверена прогоном `iec2c` +
/// `cc`: значение `grid[1, 0]` равно 3.
///
/// Уплощается только **агрегат внутри агрегата**: элемент, записанный скаляром
/// (упакованный `[bit;N<=64]` - ), печатается своим значением.
pub(crate) fn flat_array_items(
    items: &[ExpressionNode],
    elem: &TypeNode,
    model: Option<&ModelNode>,
) -> Option<Vec<String>> {
    let mut parts = Vec::new();
    for item in items {
        if let TypeNode::Array(_, inner) = elem
            && !matches!(**inner, TypeNode::Struct(_))
            && let ExpressionNode::Initializer(sub) | ExpressionNode::Array(sub) = item
        {
            parts.extend(flat_array_items(sub, inner, model)?);
        } else {
            parts.push(literal_init(item, elem, model)?);
        }
    }
    Some(parts)
}

/// Путь к листу агрегата в форме IEC.
///
/// Подряд идущие индексы сливаются в одну пару скобок - так же, как индексация цепочкой
/// (`grid[1, 0]`): в IEC 61131-3 массивы не вкладываются, и адресация обязана следовать
/// объявлению.
pub(crate) fn iec_suffix(path: &[crate::generator::aggregate::Step]) -> String {
    use crate::generator::aggregate::Step;
    let mut out = String::new();
    let mut indices: Vec<String> = Vec::new();
    let flush = |out: &mut String, indices: &mut Vec<String>| {
        if !indices.is_empty() {
            out.push('[');
            out.push_str(&indices.join(", "));
            out.push(']');
            indices.clear();
        }
    };
    for step in path {
        match step {
            Step::Index(i) => indices.push(i.to_string()),
            Step::Field(f) => {
                flush(&mut out, &mut indices);
                out.push('.');
                out.push_str(f);
            }
        }
    }
    flush(&mut out, &mut indices);
    out
}

#[cfg(test)]
mod tests {
    use crate::generator::aggregate::Step;

    use super::iec_suffix;

    #[test]
    fn nested_indices_collapse_into_one_bracket() {
        assert_eq!(
            iec_suffix(&[Step::Index(1), Step::Index(0)]),
            "[1, 0]".to_string()
        );
    }

    /// Поле разрывает цепочку: `pts[1].x` - два разных адресуемых объекта.
    #[test]
    fn field_breaks_the_chain() {
        assert_eq!(
            iec_suffix(&[Step::Index(1), Step::Field("x".to_string()), Step::Index(0)]),
            "[1].x[0]".to_string()
        );
    }
}
