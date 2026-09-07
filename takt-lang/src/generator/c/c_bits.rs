//! Бит-вектор `[bit;N]` в цели `c`: разряд, инициализация, копирование.
//!
//! ## Границы
//!
//! Переводятся ровно те формы, которые исполняет эталон: инициализация, копирование
//! вектора в вектор, чтение и запись разряда. Арифметику и сравнение над словами не
//! умеет и эталон (`SIM-005` в такте), поэтому там цель отказывает `CC-022` с причиной,
//! а не печатает выражение, означающее в C арифметику указателя.

use crate::semantic::bit_vector::{self, BitVectorLayout};
use crate::semantic::{ExpressionNode, VariableNode};

/// Маска одного разряда. Суффикс `ull` обязателен: без него литерал - 32-битный
/// `unsigned int`, и разряд >= 32 даёт `shift count >= width of type`.
pub(in crate::generator::c) const ONE: &str = "1ull";

/// Число слов носителя, если это бит-вектор в представлении "массив слов".
///
/// `None` - либо не бит-вектор, либо `N <= 64` (скаляр): в обоих случаях печать
/// остаётся прежней.
///
/// Тип берётся из ячейки `ExpressionNode::Variable` - снимка, снятого при разрешении
/// имени. Для объявленного типа он верен; при `Inference` вернётся `None`, то есть
/// печать останется скалярной, а не станет отказом.
pub(in crate::generator::c) fn words_of(expr: &ExpressionNode) -> Option<u16> {
    let ExpressionNode::Variable(var_rc) = expr else {
        return None;
    };
    let var = var_rc.borrow();
    let (VariableNode::Simple { ty, .. }
    | VariableNode::Const { ty, .. }
    | VariableNode::Port { ty, .. }) = &*var
    else {
        return None;
    };
    words_of_type(ty)
}

/// Число слов по типу - та же развилка, но для мест, где тип уже под рукой
/// (инициализация в `_init`).
pub(in crate::generator::c) fn words_of_type(
    ty: &crate::semantic::type_node::TypeNode,
) -> Option<u16> {
    let n = bit_vector::is_bit_vector(ty)?;
    match bit_vector::layout(n) {
        BitVectorLayout::Words { count } => Some(count),
        BitVectorLayout::Scalar { .. } => None,
    }
}

/// Доступ к слову носителя: `base[i]`.
fn word(base: &str, index: u16) -> String {
    format!("{base}[{index}]")
}

/// Чтение разряда `x.K`.
///
/// Скаляр - сдвиг самого носителя; массив слов - сдвиг **своего** слова: позиция
/// берётся у `bit_vector::bit_slot`, того же носителя, которым пользуются эталон
/// (`eval/access.rs`) и цель `st`.
pub(in crate::generator::c) fn read_bit(base: &str, words: Option<u16>, bit: u64) -> String {
    let Some(count) = words else {
        return format!("(({base} >> {bit}) & {ONE})");
    };
    let (w, off) = bit_vector::bit_slot(u32::try_from(bit).unwrap_or(u32::MAX));
    if w >= count {
        // Разряд за пределом вектора: читается ноль. Печатать доступ за границу массива
        // нельзя - это UB в порождённой прошивке.
        return "0ull".to_string();
    }
    format!("(({} >> {off}) & {ONE})", word(base, w))
}

/// Запись разряда `x.K := v` - "очистить и установить".
///
/// Значение берётся младшим битом (`rhs & 1`): правило общее у целей `c`, `rust` и `sv`
/// и записано в документе.
pub(in crate::generator::c) fn write_bit(
    base: &str,
    words: Option<u16>,
    bit: u64,
    rhs: &str,
) -> Option<String> {
    let Some(count) = words else {
        return Some(format!(
            "{base} = ({base} & ~({ONE} << {bit})) | (({rhs} & {ONE}) << {bit})"
        ));
    };
    let (w, off) = bit_vector::bit_slot(u32::try_from(bit).unwrap_or(u32::MAX));
    if w >= count {
        return None;
    }
    let cell = word(base, w);
    Some(format!(
        "{cell} = ({cell} & ~({ONE} << {off})) | (({rhs} & {ONE}) << {off})"
    ))
}

/// Копирование вектора в вектор - по словам через оператор "запятая".
///
/// В C массив не является изменяемым lvalue (`model->w = model->x` отвергается),
/// а `memcpy` потянул бы `<string.h>` ради двух присваиваний. Запятая даёт
/// выражение, а присваивание в языке Takt - оператор, поэтому форма
/// уместна там же, где стояло обычное присваивание. Проба: `cc -Wall -Wextra
/// -Werror` принимает.
pub(in crate::generator::c) fn copy_words(dst: &str, src: &str, count: u16) -> String {
    let items: Vec<String> = (0..count)
        .map(|i| format!("{} = {}", word(dst, i), word(src, i)))
        .collect();
    format!("({})", items.join(", "))
}

/// Заполнение вектора числом: младшее слово получает значение, прочие - ноль.
///
/// Литерал шире 64 бит языком не принимается (`LE-009`), поэтому старших слов значение
/// не касается - они обнуляются.
pub(in crate::generator::c) fn fill_words(dst: &str, count: u16, literal: &str) -> String {
    let items: Vec<String> = (0..count)
        .map(|i| {
            if i == 0 {
                format!("{} = ({literal})", word(dst, i))
            } else {
                format!("{} = 0ull", word(dst, i))
            }
        })
        .collect();
    format!("({})", items.join(", "))
}

/// Операция, чей операнд - бит-вектор шире 64 бит.
///
/// Возвращает знак операции для текста отказа. Разбираются арифметика, побитовые
/// операции и сравнения - то есть всё, что над массивом слов в C означало бы арифметику
/// указателя. Присваивание и битовый доступ сюда **не** входят: их цель печатает по
/// словам.
///
/// Разбор намеренно не исчерпывающий: узел, которого здесь нет, печатается обычным
/// путём. Это граница отказа, а не пропуск - эталон те же операции не поддерживает
/// вовсе (`SIM-005` в такте), и отказ здесь лишь даёт сообщение своей диагностикой, а
/// не чужим компилятором.
pub(in crate::generator::c) fn wide_operand(expr: &ExpressionNode) -> Option<&'static str> {
    let (op, l, r) = match expr {
        ExpressionNode::Add(l, r) => ("+", l, r),
        ExpressionNode::Subtract(l, r) => ("-", l, r),
        ExpressionNode::Multiply(l, r) => ("*", l, r),
        ExpressionNode::Divide(l, r) => ("/", l, r),
        ExpressionNode::Modulo(l, r) => ("%", l, r),
        ExpressionNode::Power(l, r) => ("**", l, r),
        ExpressionNode::BitwiseAnd(l, r) => ("&", l, r),
        ExpressionNode::BitwiseOr(l, r) => ("|", l, r),
        ExpressionNode::BitwiseXor(l, r) => ("^", l, r),
        ExpressionNode::ShiftLeft(l, r) => ("<<", l, r),
        ExpressionNode::ShiftRight(l, r) => (">>", l, r),
        ExpressionNode::Equal(l, r) => ("=", l, r),
        ExpressionNode::NotEqual(l, r) => ("!=", l, r),
        ExpressionNode::Less(l, r) => ("<", l, r),
        ExpressionNode::More(l, r) => (">", l, r),
        ExpressionNode::LessEqual(l, r) => ("<=", l, r),
        ExpressionNode::MoreEqual(l, r) => (">=", l, r),
        _ => return None,
    };
    if words_of(l).is_some() || words_of(r).is_some() {
        Some(op)
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scalar_access_keeps_shape_but_widens_mask() {
        assert_eq!(read_bit("model->v", None, 35), "((model->v >> 35) & 1ull)");
        assert_eq!(
            write_bit("model->v", None, 35, "1").expect("скаляр пишется всегда"),
            "model->v = (model->v & ~(1ull << 35)) | ((1 & 1ull) << 35)"
        );
    }

    #[test]
    fn word_access_uses_bit_slot() {
        // Бит 70 живёт в слове 1 со смещением 6 - как у эталона.
        assert_eq!(
            read_bit("model->w", Some(2), 70),
            "((model->w[1] >> 6) & 1ull)"
        );
        assert_eq!(
            write_bit("model->w", Some(2), 70, "1").expect("бит в пределах вектора"),
            "model->w[1] = (model->w[1] & ~(1ull << 6)) | ((1 & 1ull) << 6)"
        );
    }

    #[test]
    fn bit_beyond_vector_is_not_printed_out_of_bounds() {
        assert_eq!(read_bit("model->w", Some(2), 200), "0ull");
        assert!(write_bit("model->w", Some(2), 200, "1").is_none());
    }

    #[test]
    fn copy_and_fill_go_word_by_word() {
        assert_eq!(
            copy_words("model->w", "model->x", 2),
            "(model->w[0] = model->x[0], model->w[1] = model->x[1])"
        );
        assert_eq!(
            fill_words("model->w", 2, "0"),
            "(model->w[0] = (0), model->w[1] = 0ull)"
        );
    }
}
