//! Что передаётся в функцию по ссылке у цели `rust`.

use crate::semantic::bit_vector::{BitVectorLayout, is_bit_vector, layout};
use crate::semantic::type_node::TypeNode;

/// Передаётся ли параметр этого типа по ссылке.
///
/// Массив - да: по значению это копия на каждый вызов, тогда как цель `c` передаёт
/// указатель, а `st` - `VAR_IN_OUT`.
///
/// Упакованный бит-вектор (`[bit;N <= 64]`) - **скаляр**, и печать типа у него не
/// массивная: передавать его по ссылке значило бы развести форму аргумента и форму
/// параметра.
///
/// Признак общий у печати аргумента и печати сигнатуры: разъехавшись, они дают `E0308` -
/// то есть вывод, который не собирается.
pub(crate) fn is_array_by_reference(ty: &TypeNode) -> bool {
    matches!(ty, TypeNode::Array(..)) && !is_bit_vector_scalar(ty)
}

/// Упакованный ли это бит-вектор (`[bit;N <= 64]` - скаляр).
fn is_bit_vector_scalar(ty: &TypeNode) -> bool {
    matches!(
        is_bit_vector(ty).map(layout),
        Some(BitVectorLayout::Scalar { .. })
    )
}
