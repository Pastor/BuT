//! Домен типа для верификации по данным - Один носитель.
//!
//! # Бит-вектор - скаляр, а не массив
//!
//! `[bit; N]` при `N <= 64` - **упакованное беззнаковое целое**: цель `c` печатает его
//! как `uint8_t`/`uint16_t`, эталон хранит числом. Один лишь верификатор считал его
//! массивом и на этом основании отказывал - `var mask: u8` проверялся, а тождественный
//! ему `var mask: [bit; 8]` получал "не проверено: ... массив". Домен берётся по
//! **объявленной** ширине `N` (ширина - контракт), а не по машинной: у `[bit; 3]`
//! значений восемь, и это выгодно - потолок задачи считается по доменам.
//!
//! `N > 64` - массив слов, и он остаётся вне охвата: там перебирать нечего.

use crate::semantic::{ModelNode, type_node::TypeNode};
use std::collections::BTreeSet;

/// Перечислимый домен типа: диапазон целых либо список значений.
///
/// Диапазон не материализуется до последнего: у `u32` значений 4·10⁹, и решение "за
/// потолком" принимается по размеру, а не по списку.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Domain {
    /// Целые от `lo` до `hi` включительно.
    Range { lo: i128, hi: i128 },
    /// Перечисление: различные значения вариантов, по возрастанию.
    Values(Vec<i128>),
}

impl Domain {
    /// Число значений домена.
    pub(crate) fn size(&self) -> u128 {
        match self {
            Self::Range { lo, hi } => (hi - lo + 1) as u128,
            Self::Values(v) => v.len() as u128,
        }
    }

    /// Значения домена списком - материализация после прохождения потолка.
    pub(crate) fn values(&self) -> Vec<i128> {
        match self {
            Self::Range { lo, hi } => (*lo..=*hi).collect(),
            Self::Values(v) => v.clone(),
        }
    }
}

/// Домен типа; `None` - перебирать нечего (`float`, `q`, `duration`, массив, структура,
/// бит-вектор шире слова).
pub(crate) fn of(ty: &TypeNode, model: &ModelNode) -> Option<Domain> {
    match ty {
        TypeNode::Bit | TypeNode::Bool => Some(Domain::Range { lo: 0, hi: 1 }),
        TypeNode::Integer { bits, signed } => {
            let n = 1i128 << *bits;
            Some(if *signed {
                Domain::Range {
                    lo: -(n / 2),
                    hi: n / 2 - 1,
                }
            } else {
                Domain::Range { lo: 0, hi: n - 1 }
            })
        }
        // Объявление ищется подъёмом к родителям (`search_enum`), а не в карте самой
        // модели: перечисление, объявленное на уровне файла, для вложенной модели
        // "отсутствовало".
        TypeNode::Enum(name) => {
            let e = model.search_enum(name)?;
            let set: BTreeSet<i128> = e.variants.iter().map(|(_, v)| *v).collect();
            Some(Domain::Values(set.into_iter().collect()))
        }
        // Упакованный бит-вектор - беззнаковое целое объявленной ширины.
        ty => {
            let n = crate::semantic::bit_vector::is_bit_vector(ty)?;
            match crate::semantic::bit_vector::layout(n) {
                crate::semantic::bit_vector::BitVectorLayout::Scalar { .. } => {
                    Some(Domain::Range {
                        lo: 0,
                        hi: (1i128 << n) - 1,
                    })
                }
                crate::semantic::bit_vector::BitVectorLayout::Words { .. } => None,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Беззнаковое целое: домен `0..2ⁿ − 1`.
    #[test]
    fn unsigned_integer_starts_at_zero() {
        let d = of(
            &TypeNode::Integer {
                bits: 8,
                signed: false,
            },
            &ModelNode::default(),
        )
        .expect("целое перечислимо");
        assert_eq!(d, Domain::Range { lo: 0, hi: 255 });
        assert_eq!(d.size(), 256);
    }

    /// Знаковое целое: домен симметричен и включает `-2ⁿ⁻¹`.
    #[test]
    fn signed_integer_covers_negatives() {
        let d = of(
            &TypeNode::Integer {
                bits: 8,
                signed: true,
            },
            &ModelNode::default(),
        )
        .expect("целое перечислимо");
        assert_eq!(d, Domain::Range { lo: -128, hi: 127 });
        assert_eq!(d.size(), 256);
    }

    /// Бит-вектор до слова: домен по объявленной ширине.
    ///
    /// Ровно здесь ломается наивный перенос машинной ширины: `[bit; 3]` - это `u8` по
    /// хранению, но значений у него восемь, а не 256.
    #[test]
    fn packed_bit_vector_uses_declared_width() {
        let ty = TypeNode::Array(3, Box::new(TypeNode::Bit));
        let d = of(&ty, &ModelNode::default()).expect("вектор перечислим");
        assert_eq!(d, Domain::Range { lo: 0, hi: 7 });
        assert_eq!(d.values().len(), 8);
    }

    /// Бит-вектор шире слова - массив слов, перебирать нечего.
    #[test]
    fn wide_bit_vector_has_no_domain() {
        let ty = TypeNode::Array(96, Box::new(TypeNode::Bit));
        assert!(of(&ty, &ModelNode::default()).is_none());
    }

    /// Составные и дробные типы домена не имеют.
    #[test]
    fn composite_and_fractional_have_no_domain() {
        let model = ModelNode::default();
        assert!(of(&TypeNode::Rational, &model).is_none());
        assert!(
            of(
                &TypeNode::Fixed {
                    m: 4,
                    n: 4,
                    sat: false
                },
                &model
            )
            .is_none()
        );
        assert!(of(&TypeNode::Duration, &model).is_none());
        assert!(of(&TypeNode::Struct("Pair".to_string()), &model).is_none());
        assert!(
            of(
                &TypeNode::Array(
                    2,
                    Box::new(TypeNode::Integer {
                        bits: 8,
                        signed: false
                    })
                ),
                &model
            )
            .is_none()
        );
    }
}
