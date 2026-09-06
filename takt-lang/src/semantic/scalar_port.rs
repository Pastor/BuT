//! Машинная ширина скалярного типа - для целей, где порт есть целое.

use crate::semantic::ModelNode;
use crate::semantic::type_node::TypeNode;

/// Как цель-компилятор представляет скалярное значение: ширина и знак.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct ScalarRepr {
    /// Машинная ширина: 8, 16, 32 либо 64 бита.
    pub(crate) bits: u8,
    /// Знаковое ли представление.
    pub(crate) signed: bool,
}

/// Представление скалярного типа целым; `None` - у типа его нет.
///
/// `None` отвечают массив, структура, `float` (у него своё представление), бит-вектор
/// шире слова и всё, что не является значением на границе. `bit`/`bool` тоже дают
/// `None`: у целей это **отдельная** категория порта (метод `write_bit`, локация
/// `%QX`), и подменять её целым нельзя.
pub(crate) fn scalar_repr(ty: &TypeNode, model: &ModelNode) -> Option<ScalarRepr> {
    match ty {
        TypeNode::Integer { bits, signed } => Some(ScalarRepr {
            bits: *bits,
            signed: *signed,
        }),
        // Длительность - целое в миллисекундах.
        TypeNode::Duration => Some(ScalarRepr {
            bits: crate::semantic::duration::VALUE_BITS,
            signed: false,
        }),
        // `q(m, n)` хранится знаковым целым ширины `W = m + n`, округлённой до машинной
        // (тот же носитель, которым цели выбирают имя типа).
        TypeNode::Fixed { m, n, .. } => Some(ScalarRepr {
            bits: crate::semantic::type_node::type_fixed::fixed_storage_bits(m + n),
            signed: true,
        }),
        // Бит-вектор `[bit;N]`, N <= 64 - Упакованное беззнаковое целое родной ширины:
        // `[bit;12]` == `u16`. N > 64 - массив слов, и целым он не представим: там
        // `None` (у `rust` это `RS-016`, и отказ верен).
        //
        // Пропуск этой ветви и был дефектом 0488: носитель обещал "скаляр", а цель
        // `st-at` отвечала `ST-004` на порту `[bit;12]`, который прочие семь
        // потребителей переводят.
        ty if crate::semantic::bit_vector::is_bit_vector(ty).is_some() => {
            let n = crate::semantic::bit_vector::is_bit_vector(ty)?;
            match crate::semantic::bit_vector::layout(n) {
                crate::semantic::bit_vector::BitVectorLayout::Scalar { width } => {
                    Some(ScalarRepr {
                        bits: u8::try_from(width).unwrap_or(64),
                        signed: false,
                    })
                }
                crate::semantic::bit_vector::BitVectorLayout::Words { .. } => None,
            }
        }
        // Перечисление: знак и ширину задаёт набор вариантов.
        TypeNode::Enum(name) => {
            let facts = model.search_enum(name).and_then(|e| e.facts())?;
            Some(ScalarRepr {
                bits: u8::try_from(facts.machine_bits()).unwrap_or(64),
                signed: facts.signed,
            })
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn model() -> ModelNode {
        ModelNode::default()
    }

    /// Целое отдаёт свою ширину и знак без изменений.
    #[test]
    fn integer_keeps_its_width() {
        let repr = scalar_repr(
            &TypeNode::Integer {
                bits: 16,
                signed: true,
            },
            &model(),
        )
        .expect("целое скалярно");
        assert_eq!(
            repr,
            ScalarRepr {
                bits: 16,
                signed: true
            }
        );
    }

    /// Длительность - беззнаковое целое миллисекунд.
    #[test]
    fn duration_is_unsigned_millis() {
        let repr = scalar_repr(&TypeNode::Duration, &model()).expect("длительность скалярна");
        assert!(!repr.signed);
        assert_eq!(repr.bits, crate::semantic::duration::VALUE_BITS);
    }

    /// `q(m, n)` - знаковое целое машинной ширины, а не `m + n`.
    ///
    /// Ровно здесь ломался наивный перенос: `q(6, 6)` - это 12 бит, а типа `i12` нет ни
    /// у одной цели.
    #[test]
    fn fixed_rounds_up_to_machine_width() {
        let repr = scalar_repr(
            &TypeNode::Fixed {
                m: 6,
                n: 6,
                sat: false,
            },
            &model(),
        )
        .expect("q скалярно");
        assert_eq!(
            repr,
            ScalarRepr {
                bits: 16,
                signed: true
            }
        );
    }

    /// Бит - не целое: у целей это своя категория порта.
    #[test]
    fn bit_is_not_an_integer() {
        assert!(scalar_repr(&TypeNode::Bit, &model()).is_none());
        assert!(scalar_repr(&TypeNode::Bool, &model()).is_none());
    }

    /// Бит-вектор до слова - упакованное беззнаковое целое родной ширины.
    ///
    /// Ровно эту ветвь пропустила, и `st-at` отвечал `ST-004` на порту `[bit;12]`,
    /// который переводят семь прочих потребителей.
    #[test]
    fn packed_bit_vector_is_unsigned_integer() {
        let vector = TypeNode::Array(12, Box::new(TypeNode::Bit));
        assert_eq!(
            scalar_repr(&vector, &model()),
            Some(ScalarRepr {
                bits: 16,
                signed: false
            })
        );
    }

    /// Бит-вектор шире слова - массив слов, а не скаляр.
    #[test]
    fn wide_bit_vector_is_not_scalar() {
        let words = TypeNode::Array(96, Box::new(TypeNode::Bit));
        assert!(scalar_repr(&words, &model()).is_none());
    }

    /// Составной тип скалярного представления не имеет.
    #[test]
    fn composite_has_no_scalar_repr() {
        let array = TypeNode::Array(
            4,
            Box::new(TypeNode::Integer {
                bits: 8,
                signed: false,
            }),
        );
        assert!(scalar_repr(&array, &model()).is_none());
        assert!(scalar_repr(&TypeNode::Struct("Pair".to_string()), &model()).is_none());
    }
}
