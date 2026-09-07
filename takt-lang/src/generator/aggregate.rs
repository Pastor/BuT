//! Места записи агрегата - общий носитель целей.
//!
//! `x := {3, 4};` присваивает агрегат: массиву - поэлементно по индексу, структуре - по
//! **имени поля**. Правило одно на три цели (`c`, `st`, `sv`), и держать его копиями
//! значило бы разойтись - как уже разошлись цели `st` и `sv`, печатавшие `body[0] :=
//! 3;` для структуры.

use crate::semantic::ExpressionNode;
use crate::semantic::type_node::TypeNode;

/// Место записи `index`-го элемента агрегата.
pub(crate) struct Place {
    /// Суффикс к базе: `[0]` для массива, `.x` для поля структуры.
    pub(crate) suffix: String,
    /// Тип элемента - для приведения значения к типу приёмника.
    pub(crate) ty: Option<TypeNode>,
}

/// Места записи агрегата длины `count` в приёмник типа `ty`.
///
/// `fields` - поля структуры-приёмника, если приёмник структура; их достаёт вызывающий,
/// потому что цели хранят объявления по-разному (`ModelNode` у `st` и `c`, снимок карты
/// у `sv`). Носитель отвечает за **правило выбора формы**, а не за поиск объявления.
///
/// Для структуры порядок берётся у **объявления**: агрегат позиционный, и сортировка
/// развела бы значения по чужим полям.
///
/// Неизвестный вид приёмника даёт индексную форму - прежнее поведение целей.
pub(crate) fn places(
    fields: Option<&[(String, TypeNode)]>,
    ty: Option<&TypeNode>,
    count: usize,
) -> Vec<Place> {
    if let Some(list) = fields {
        return list
            .iter()
            .take(count)
            .map(|(field, field_ty)| Place {
                suffix: format!(".{field}"),
                ty: Some(field_ty.clone()),
            })
            .collect();
    }
    let elem = match ty {
        Some(TypeNode::Array(_, elem)) => Some((**elem).clone()),
        _ => None,
    };
    (0..count)
        .map(|index| Place {
            suffix: format!("[{index}]"),
            ty: elem.clone(),
        })
        .collect()
}

/// Шаг пути к листу агрегата.
///
/// Форму адресации выбирает **цель**, а не носитель: у `c` и `sv` это `[i][j]`, а у
/// `st` многомерный массив адресуется одной парой скобок (`[i, j]`) - потому путь и
/// хранится шагами, а не строкой.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Step {
    /// Элемент массива по индексу.
    Index(usize),
    /// Поле структуры по имени.
    Field(String),
}

/// Лист агрегата: путь от базы, тип приёмника и записываемое выражение.
pub(crate) struct Leaf<'a> {
    pub(crate) path: Vec<Step>,
    pub(crate) ty: Option<TypeNode>,
    pub(crate) value: &'a ExpressionNode,
}

/// Поиск полей структуры по её имени - его ведёт вызывающий.
///
/// Цели хранят объявления по-разному (`ModelNode` у `st` и `c`, снимок карты у `sv`),
/// поэтому носитель принимает функцию, а не модель.
pub(crate) type FieldsOf<'a> = dyn Fn(&str) -> Option<Vec<(String, TypeNode)>> + 'a;

/// Раскрывает агрегат до листьев - рекурсивно.
///
/// `fields_of` отдаёт поля структуры по её имени: цели хранят объявления по-разному
/// (`ModelNode` у `st` и `c`, снимок карты у `sv`), поэтому поиск принадлежит
/// вызывающему, а носитель отвечает за **правило раскрытия**.
///
/// Элемент, который сам является агрегатом, раскрывается дальше; всё прочее становится
/// листом. Бит-вектор `[bit;N<=64]` агрегатом не считается: это упакованное значение, и
/// его инициализатор - число.
pub(crate) fn leaves<'a>(
    ty: Option<&TypeNode>,
    items: &'a [ExpressionNode],
    fields_of: &FieldsOf<'_>,
) -> Vec<Leaf<'a>> {
    let mut out = Vec::new();
    collect(ty, items, fields_of, &mut Vec::new(), &mut out);
    out
}

fn collect<'a>(
    ty: Option<&TypeNode>,
    items: &'a [ExpressionNode],
    fields_of: &FieldsOf<'_>,
    prefix: &mut Vec<Step>,
    out: &mut Vec<Leaf<'a>>,
) {
    let fields = match ty {
        Some(TypeNode::Struct(name)) => fields_of(name),
        _ => None,
    };
    let places = places(fields.as_deref(), ty, items.len());
    for (item, place) in items.iter().zip(places) {
        let step = match place.suffix.strip_prefix('.') {
            Some(field) => Step::Field(field.to_string()),
            None => Step::Index(out_index(&place.suffix)),
        };
        prefix.push(step);
        let nested = match (item, place.ty.as_ref()) {
            (ExpressionNode::Initializer(inner) | ExpressionNode::Array(inner), Some(elem_ty))
                if is_aggregate_type(elem_ty, fields_of) =>
            {
                Some((inner.as_slice(), elem_ty))
            }
            _ => None,
        };
        match nested {
            Some((inner, elem_ty)) => collect(Some(elem_ty), inner, fields_of, prefix, out),
            None => out.push(Leaf {
                path: prefix.clone(),
                ty: place.ty.clone(),
                value: item,
            }),
        }
        prefix.pop();
    }
}

/// Индекс из суффикса вида `[N]`, который построил [`places`].
fn out_index(suffix: &str) -> usize {
    suffix
        .trim_start_matches('[')
        .trim_end_matches(']')
        .parse()
        .unwrap_or(0)
}

/// Раскрывается ли значение такого типа дальше.
fn is_aggregate_type(ty: &TypeNode, fields_of: &FieldsOf<'_>) -> bool {
    match ty {
        TypeNode::Array(_, _) => crate::semantic::bit_vector::is_bit_vector(ty).is_none(),
        TypeNode::Struct(name) => fields_of(name).is_some(),
        _ => false,
    }
}

/// Путь к листу в форме C-подобных целей: `[i]` и `.f` подряд.
///
/// Годится для `c`, `c-hal`, `sv` и `sv-mmio`. Цель `st` собирает свою форму:
/// многомерный массив в IEC адресуется одной парой скобок (`[i, j]`), и общий суффикс
/// там был бы неверен.
pub(crate) fn c_like_suffix(path: &[Step]) -> String {
    path.iter()
        .map(|step| match step {
            Step::Index(i) => format!("[{i}]"),
            Step::Field(f) => format!(".{f}"),
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn num(n: i128) -> ExpressionNode {
        ExpressionNode::Number(n)
    }

    fn u8_ty() -> TypeNode {
        TypeNode::Integer {
            bits: 8,
            signed: false,
        }
    }

    fn no_fields(_: &str) -> Option<Vec<(String, TypeNode)>> {
        None
    }

    /// Вложенный массив раскрывается до листьев: путь несёт оба индекса.
    #[test]
    fn nested_array_is_flattened_to_leaves() {
        let ty = TypeNode::Array(2, Box::new(TypeNode::Array(2, Box::new(u8_ty()))));
        let items = vec![
            ExpressionNode::Initializer(vec![num(1), num(2)]),
            ExpressionNode::Initializer(vec![num(3), num(4)]),
        ];
        let got = leaves(Some(&ty), &items, &no_fields);
        let paths: Vec<_> = got.iter().map(|l| l.path.clone()).collect();
        assert_eq!(
            paths,
            vec![
                vec![Step::Index(0), Step::Index(0)],
                vec![Step::Index(0), Step::Index(1)],
                vec![Step::Index(1), Step::Index(0)],
                vec![Step::Index(1), Step::Index(1)],
            ]
        );
    }

    /// Массив структур: индекс, затем имя поля - порядок берётся у объявления.
    #[test]
    fn array_of_structs_uses_field_names() {
        let ty = TypeNode::Array(2, Box::new(TypeNode::Struct("Point".to_string())));
        let items = vec![
            ExpressionNode::Initializer(vec![num(1), num(2)]),
            ExpressionNode::Initializer(vec![num(3), num(4)]),
        ];
        let fields = |name: &str| {
            (name == "Point").then(|| vec![("x".to_string(), u8_ty()), ("y".to_string(), u8_ty())])
        };
        let got = leaves(Some(&ty), &items, &fields);
        let paths: Vec<_> = got.iter().map(|l| l.path.clone()).collect();
        assert_eq!(
            paths,
            vec![
                vec![Step::Index(0), Step::Field("x".to_string())],
                vec![Step::Index(0), Step::Field("y".to_string())],
                vec![Step::Index(1), Step::Field("x".to_string())],
                vec![Step::Index(1), Step::Field("y".to_string())],
            ]
        );
    }

    /// Плоский агрегат остаётся плоским: рекурсия не "углубляет" скаляры.
    #[test]
    fn flat_aggregate_stays_flat() {
        let ty = TypeNode::Array(2, Box::new(u8_ty()));
        let items = vec![num(7), num(8)];
        let got = leaves(Some(&ty), &items, &no_fields);
        assert_eq!(got.len(), 2);
        assert_eq!(got[0].path, vec![Step::Index(0)]);
        assert_eq!(got[1].path, vec![Step::Index(1)]);
    }

    /// Бит-вектор `[bit;8]` - упакованный скаляр: агрегатом не считается.
    #[test]
    fn packed_bit_vector_is_a_leaf() {
        let ty = TypeNode::Array(2, Box::new(TypeNode::Array(8, Box::new(TypeNode::Bit))));
        let items = vec![num(255), num(3)];
        let got = leaves(Some(&ty), &items, &no_fields);
        assert_eq!(got.len(), 2, "оба элемента — листья, а не восемь разрядов");
    }
}
