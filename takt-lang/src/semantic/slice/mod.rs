//! Границы среза массива - общий носитель.
//!
//! `part := src[1:3];` копирует полуинтервал `[l, r)`. Правило одно на четыре цели
//! (`c`, `st`, `rust`, `sv`), и держать его копиями значило бы разойтись - тот же
//! довод, что у носителя мест агрегата.
//!
//! **Модуль живёт в семантике, а не в генераторе**: границы среза - свойство **языка**,
//! и разворот среза в аргументе вызова (`semantic/slice_argument.rs`) спрашивает те же
//! значения, что печатники целей. Пока носитель лежал в `generator/`, семантика не
//! могла его позвать, не перевернув слои.

/// Длина источника, если срез над ним выразим поэлементно.
///
/// **Бит-вектор `[bit;N]` под правило не подпадает:** при `N <= 64` это упакованный
/// Скаляр, и `res[0] = mem[1];` над `uint8_t` - невалидный C; при `N > 64` - массив
/// слов, и "элемент" там разряд, а не слово. Эталон такой срез тоже не исполняет
/// (`SIM-010` "не является массивом"), поэтому цели обязаны отказывать - извлечение
/// разрядов есть отдельная конструкция.
pub(crate) fn elementwise_len(ty: &TypeNode) -> Option<u16> {
    if crate::semantic::bit_vector::is_bit_vector(ty).is_some() {
        return None;
    }
    match ty {
        TypeNode::Array(n, _) => Some(*n),
        _ => None,
    }
}

/// Длина базы-выражения, если срез над ней выразим поэлементно.
///
/// База стала выражением (`b.data[0:2]`), поэтому тип берётся у общего носителя
/// `semantic::validate::base_type`, а не у переменной. `None` - "поэлементно нельзя":
/// либо тип не выводится, либо это бит-вектор.
pub(crate) fn elementwise_len_of(
    base: &crate::semantic::ExpressionNode,
    model: &crate::semantic::ModelNode,
) -> Option<u16> {
    let ty = crate::semantic::validate::base_type::base_type(base, model)?;
    elementwise_len(&ty)
}

/// Начало и длина среза `[from, to)` источника длины `src_len`.
///
/// Пропущенная граница означает край: `src[:2]` - от нуля, `src[2:]` - до конца,
/// `src[:]` - весь массив.
///
/// Значения приходят проверенными (`SE-029`), поэтому функция не судит, а
/// **нормирует**: отрицательное начало и конец за краем прижимаются, чтобы
/// печать не строила индексов вне массива даже при будущем ослаблении
/// семантики.
pub(crate) mod argument;

use crate::semantic::type_node::TypeNode;

pub(crate) fn bounds(from: Option<i128>, to: Option<i128>, src_len: u16) -> (u16, u16) {
    let len = i128::from(src_len);
    let start = from.unwrap_or(0).clamp(0, len);
    let end = to.unwrap_or(len).clamp(start, len);
    (start as u16, (end - start) as u16)
}

#[cfg(test)]
mod tests {
    use super::bounds;

    #[test]
    fn omitted_bounds_mean_edges() {
        assert_eq!(bounds(None, Some(2), 4), (0, 2), "src[:2]");
        assert_eq!(bounds(Some(2), None, 4), (2, 2), "src[2:]");
        assert_eq!(bounds(None, None, 4), (0, 4), "src[:]");
        assert_eq!(bounds(Some(1), Some(3), 4), (1, 2), "src[1:3]");
    }

    /// Бит-вектор - скаляр либо массив слов: поэлементного среза у него нет.
    #[test]
    fn bit_vector_is_not_elementwise() {
        use super::elementwise_len;
        use crate::semantic::type_node::TypeNode;
        assert_eq!(
            elementwise_len(&TypeNode::Array(8, Box::new(TypeNode::Bit))),
            None,
            "`[bit;8]` — упакованный скаляр (0078)"
        );
        assert_eq!(
            elementwise_len(&TypeNode::Array(128, Box::new(TypeNode::Bit))),
            None,
            "`[bit;128]` — массив СЛОВ, элемент там не разряд"
        );
        assert_eq!(
            elementwise_len(&TypeNode::Array(
                4,
                Box::new(TypeNode::Integer {
                    bits: 8,
                    signed: false
                })
            )),
            Some(4),
            "настоящий массив"
        );
    }

    /// Нормирование, а не суд: значения приходят проверенными `SE-029`.
    #[test]
    fn out_of_range_is_clamped_not_rejected() {
        assert_eq!(bounds(Some(1), Some(9), 4), (1, 3), "конец за краем");
        assert_eq!(bounds(Some(3), Some(1), 4), (3, 0), "перевёрнутый интервал");
        assert_eq!(bounds(Some(-2), Some(2), 4), (0, 2), "отрицательное начало");
    }
}
