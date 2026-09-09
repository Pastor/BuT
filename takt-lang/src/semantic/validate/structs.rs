//! Структуры: пустое объявление, дубли полей, типы полей.
//!
//! Часть модуля `validate`.

use crate::diagnostics::lang::keys;
use crate::msg;
use super::*;

/// Структура без полей - `SE-115`.
pub fn validate_empty_structs(model: Rc<RefCell<ModelNode>>) -> Vec<Diagnostic> {
    let borrowed = model.borrow();
    // Накопление по объявлениям: две пустые структуры дают две диагностики, иначе автор
    // чинит по одной за прогон.
    borrowed
        .structs
        .values()
        .filter(|st| st.fields.is_empty())
        .map(|st| {
            Diagnostic::declaration_error(
                st.loc,
                msg!(keys::SE_115_STRUCT_WITHOUT_FIELDS, name = st.name),
            )
            .with_code("SE-115")
        })
        .collect()
}

/// Ce17: проверяет, что в каждой структуре модели нет дублирующихся имён полей.
///
/// ## Правило Ce17
///
/// Каждое поле структурного типа должно иметь уникальное имя внутри одного объявления
/// `struct`. Повторное объявление поля с тем же именем - ошибка.
///
/// ## Примеры (Takt)
///
/// ```text
/// // Корректно
/// struct Point { x: bit, y: bit }
///
/// // Ce17: поле x объявлено дважды
/// struct Bad { x: bit, x: bit }
/// ```
///
/// # Возвращаемое значение
///
/// [`Diagnostic`] уровня `Error` с кодом Ce17 при первом нарушении, `None` если
/// дублирований нет.
pub fn check_duplicate_struct_fields(model: Rc<RefCell<ModelNode>>) -> Option<Diagnostic> {
    let structs: Vec<_> = model.borrow().structs.values().cloned().collect();

    for s in &structs {
        let mut seen: HashSet<&str> = HashSet::new();
        for (field_name, _) in &s.fields {
            if !seen.insert(field_name.as_str()) {
                return Some(
                    Diagnostic::error(
                        s.loc,
                        msg!(keys::SE_040_STRUCT_DUPLICATE_FIELD, name = s.name, field = field_name),
                    )
                    .with_code("SE-040"),
                );
            }
        }
    }

    // Рекурсивная проверка вложенных моделей
    let nested: Vec<Rc<RefCell<ModelNode>>> =
        model.borrow().models.values().map(Rc::clone).collect();
    for nested_model in nested {
        if let Some(diag) = check_duplicate_struct_fields(nested_model) {
            return Some(diag);
        }
    }

    None
}

/// Ce18: проверяет, что типы полей всех структур разрешены и известны.
///
/// ## Правило Ce18
///
/// Каждое поле структуры должно иметь тип, который либо является встроенным (`bit`,
/// `bool`, массив), либо объявлен в области видимости (псевдоним, перечисление или
/// другая структура). Ссылка на неизвестный тип - ошибка.
///
/// ## Примеры (Takt)
///
/// ```text
/// // Корректно
/// struct Vec2 { x: [bit;16], y: [bit;16] }
///
/// // Ce18: Ghost не объявлен
/// struct Bad { val: Ghost }
/// ```
///
/// # Возвращаемое значение
///
/// [`Diagnostic`] уровня `Error` с кодом Ce18 при первом нарушении, `None` если все
/// типы полей известны.
pub fn check_struct_field_types(model: Rc<RefCell<ModelNode>>) -> Option<Diagnostic> {
    let structs: Vec<_> = model.borrow().structs.values().cloned().collect();

    for s in &structs {
        for (field_name, field_ty) in &s.fields {
            if let TypeNode::Struct(type_name) = field_ty {
                // Проверяем, что структурный тип поля существует в области видимости
                if model.borrow().search_struct(type_name).is_none() {
                    return Some(
                        Diagnostic::error(
                            s.loc,
                            msg!(
                                keys::SE_041_STRUCT_FIELD_UNKNOWN_TYPE,
                                field = field_name,
                                name = s.name,
                                ty = type_name
                            ),
                        )
                        .with_code("SE-041"),
                    );
                }
            }
        }
    }

    // Рекурсивная проверка вложенных моделей
    let nested: Vec<Rc<RefCell<ModelNode>>> =
        model.borrow().models.values().map(Rc::clone).collect();
    for nested_model in nested {
        if let Some(diag) = check_struct_field_types(nested_model) {
            return Some(diag);
        }
    }

    None
}
