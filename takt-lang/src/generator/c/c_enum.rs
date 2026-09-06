//! Значение перечислимого типа печатается **именем константы**.
//!
//! # Правило
//!
//! Там, где **известен тип** перечислимого операнда (присваивание, начальная
//! инициализация, сравнение с литералом), число заменяется именем константы.
//!
//! **Догадываться нельзя**: значение, не совпадающее ни с одним вариантом, печатается
//! **числом** - перечислимой переменной можно присвоить произвольное число, и подмена
//! его именем "похожего" варианта была бы тихой ложью.
//!
//! Имя строит `c_names::enum_constant` - **та же** функция, что печатает `#define`.
//! Второй формулы здесь нет и быть не должно.

use crate::semantic::ModelNode;
use crate::semantic::minimap::Name;
use crate::semantic::type_node::TypeNode;
use std::cell::RefCell;
use std::rc::Rc;

/// Имя константы для значения `value` перечислимого типа `ty`.
///
/// `None` - печатать как есть: тип не перечислимый, перечисление не найдено, владелец
/// недоступен либо значение не совпадает ни с одним вариантом.
///
/// `scope` - модель, из которой видно перечисление (владелец переменной либо текущая
/// модель): поиск идёт `search_enum`, то есть по цепочке `upper`.
pub(in crate::generator::c) fn constant_of(
    ty: &TypeNode,
    value: i128,
    scope: &Rc<RefCell<ModelNode>>,
) -> Option<String> {
    let TypeNode::Enum(enum_name) = ty else {
        return None;
    };
    let def = scope.borrow().search_enum(enum_name)?;
    let (variant, _) = def.variants.iter().find(|(_, v)| *v == value)?;
    // Владелец берётся у самого узла, а не у модели, которую печатаем: перечисление
    // могло быть унаследовано от родителя, и тогда `#define` объявлен с его именем.
    let owner = def.upper.as_ref().and_then(|w| w.upgrade())?;
    Some(crate::generator::c::c_names::enum_constant(
        &Name::from(owner),
        enum_name,
        variant,
    ))
}
