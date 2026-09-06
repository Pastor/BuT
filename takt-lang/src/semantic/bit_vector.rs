//! Единое правило упаковки бит-вектора `[bit;N]` в родные типы цели.
//!
//! `[T;N]` - массив из N элементов T. **Особый случай T ∈ {`bit`, `bool`}** - массив
//! упаковывается в родные беззнаковые типы целевого языка:
//!
//! - **N <= 64** -> **скаляр** ширины `round_up(N) ∈ {8, 16, 32, 64}`
//!   (`[bit;8]`->8, `[bit;12]`->16, `[bit;3]`->8);
//! - **N > 64** -> **массив слов** `uint64_t[⌈N/64⌉]` (`[bit;128]`->2 слова,
//!   `[bit;100]`->2 слова с добивкой).
//!
//! Правило живёт **здесь**, в одном месте (образец - `enum_facts` 0060): цели C/
//! Rust/ST и симулятор зовут его, чтобы не разъехаться. SV бит-вектор упаковывает
//! **сам** (нативный `logic [N-1:0]` любой ширины - слова ему не нужны), но
//! [`is_bit_vector`] использует и он.
//!
//! Бит-доступ `x.k`/`x[k]` считается по представлению: скаляр - сдвиг/маска; слова -
//! слово `k / 64`, бит `k % 64` ([`bit_slot`]).

use super::type_node::TypeNode;

/// Ширина машинного слова для представления "массив слов" (N > 64).
pub const WORD_BITS: u16 = 64;

/// `Some(N)`, если `ty` - бит-вектор `[bit;N]`/`[bool;N]` (элемент - `Bit`/`Bool`, `N >=
/// 1`); иначе `None`. Дискриминатор - **тип элемента**: `[u8;4]` (элемент-скаляр) -
/// настоящий массив, а не бит-вектор. Нулевая ширина (`[bit;0]`) - вырожденный случай,
/// отдаётся общим тестам нулевого размера (ST-007 и т.п.), а не упаковывается.
#[must_use]
pub fn is_bit_vector(ty: &TypeNode) -> Option<u16> {
    match ty {
        TypeNode::Array(n, elem) if *n >= 1 && matches!(**elem, TypeNode::Bit | TypeNode::Bool) => {
            Some(*n)
        }
        _ => None,
    }
}

/// Представление бит-вектора в родных типах цели.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BitVectorLayout {
    /// N <= 64 -> один скаляр указанной ширины (∈ {8, 16, 32, 64}).
    Scalar {
        /// Разрядность скаляра, округлённая вверх до родной: 8/16/32/64.
        width: u16,
    },
    /// N > 64 -> массив из `count` слов по [`WORD_BITS`] бит (`uint64_t[count]`).
    Words {
        /// Число слов = `⌈N / 64⌉`.
        count: u16,
    },
}

/// Округляет разрядность вверх до ближайшей родной (8/16/32/64). Определена для `1 <= n
/// <= 64`; `n = 0` -> 8 (защитно; нулевой размер отсекает валидатор).
#[must_use]
pub fn round_up(n: u16) -> u16 {
    match n {
        0..=8 => 8,
        9..=16 => 16,
        17..=32 => 32,
        _ => 64,
    }
}

/// Представление `[bit;N]`: скаляр (округление вверх) при N <= 64, иначе массив из
/// `⌈N/64⌉` 64-битных слов.
#[must_use]
pub fn layout(n: u16) -> BitVectorLayout {
    if n <= WORD_BITS {
        BitVectorLayout::Scalar { width: round_up(n) }
    } else {
        BitVectorLayout::Words {
            count: n.div_ceil(WORD_BITS),
        }
    }
}

/// Позиция бита `k` в представлении "массив слов": `(индекс слова, смещение в слове)` =
/// `(k / 64, k % 64)`. Для скалярного представления смещение - сам `k` (слово 0).
#[must_use]
pub fn bit_slot(k: u32) -> (u16, u32) {
    let w = u16::try_from(k / u32::from(WORD_BITS)).unwrap_or(u16::MAX);
    (w, k % u32::from(WORD_BITS))
}

/// Означает ли индекс по этому типу разряд.
///
/// `[bit; N]` - упакованный скаляр, и `x[3]` над ним означает ровно то же, что
/// `x.3`. Пока узлов было два, каждый потребитель понимал индексную форму
/// по-своему: цель `c` печатала индексацию скаляра ("subscripted value is not
/// an array" у `cc`), цель `st` - индексацию `USINT`, а эталон отвечал
/// `SIM-010` на ту же запись, которую по точке исполнял. Все три - при
/// **нулевом** коде возврата `taktc`.
///
/// `None` значит "тип неизвестен" - тогда сведения не происходит: ошибка в сторону
/// "оставить как есть" стоит одной невалидной формы, а ошибка в другую сторону
/// превратила бы элемент массива в разряд числа.
///
/// Сводится только литеральный индекс, и решает это вызывающий: у переменного разряда
/// нет ни формы `x.i` в языке, ни поддержки у эталона.
pub fn indexes_a_bit(ty: Option<&TypeNode>) -> bool {
    ty.is_some_and(|ty| is_bit_vector(ty).is_some())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn is_bit_vector_discriminates_element() {
        assert_eq!(
            is_bit_vector(&TypeNode::Array(8, Box::new(TypeNode::Bit))),
            Some(8)
        );
        assert_eq!(
            is_bit_vector(&TypeNode::Array(4, Box::new(TypeNode::Bool))),
            Some(4)
        );
        // Настоящий массив скаляров - не бит-вектор.
        assert_eq!(
            is_bit_vector(&TypeNode::Array(
                4,
                Box::new(TypeNode::Integer {
                    bits: 8,
                    signed: false
                })
            )),
            None
        );
        assert_eq!(is_bit_vector(&TypeNode::Bit), None);
    }

    #[test]
    fn round_up_to_native_width() {
        assert_eq!(round_up(1), 8);
        assert_eq!(round_up(3), 8);
        assert_eq!(round_up(8), 8);
        assert_eq!(round_up(9), 16);
        assert_eq!(round_up(12), 16);
        assert_eq!(round_up(32), 32);
        assert_eq!(round_up(33), 64);
        assert_eq!(round_up(64), 64);
    }

    #[test]
    fn layout_scalar_and_words() {
        assert_eq!(layout(8), BitVectorLayout::Scalar { width: 8 });
        assert_eq!(layout(12), BitVectorLayout::Scalar { width: 16 });
        assert_eq!(layout(64), BitVectorLayout::Scalar { width: 64 });
        assert_eq!(layout(65), BitVectorLayout::Words { count: 2 });
        assert_eq!(layout(100), BitVectorLayout::Words { count: 2 });
        assert_eq!(layout(128), BitVectorLayout::Words { count: 2 });
        assert_eq!(layout(129), BitVectorLayout::Words { count: 3 });
    }

    #[test]
    fn bit_slot_splits_word_and_offset() {
        assert_eq!(bit_slot(0), (0, 0));
        assert_eq!(bit_slot(5), (0, 5));
        assert_eq!(bit_slot(63), (0, 63));
        assert_eq!(bit_slot(64), (1, 0));
        assert_eq!(bit_slot(70), (1, 6));
        assert_eq!(bit_slot(128), (2, 0));
    }

    #[test]
    fn only_a_packed_bit_vector_indexes_a_bit() {
        let bits = TypeNode::Array(8, Box::new(TypeNode::Bit));
        let bytes = TypeNode::Array(
            8,
            Box::new(TypeNode::Integer {
                bits: 8,
                signed: false,
            }),
        );
        assert!(
            indexes_a_bit(Some(&bits)),
            "`[bit;8]` индексируется разрядом"
        );
        // Массив не битов сведению не подлежит: у него индекс - элемент, и подмена
        // превратила бы `bus[1]` в разряд числа.
        assert!(!indexes_a_bit(Some(&bytes)));
        assert!(
            !indexes_a_bit(Some(&TypeNode::Bit)),
            "скаляр не индексируется"
        );
        // "Неизвестно" значит "не сводим": сведение наугад дороже пропуска.
        assert!(!indexes_a_bit(None));
    }
}
