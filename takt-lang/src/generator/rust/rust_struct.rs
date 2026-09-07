//! Литерал структуры и умолчание для цели `rust`.
//!
//! Знание "как выглядит агрегат структуры" самостоятельно: оно повторяет правило
//! именования полей из `rust_decl` и обязано меняться вместе с ним.

use crate::diagnostics::{Diagnostic, Location};
use crate::generator::rust::rust_expr::{Scope, coerce_to, unsupported};
use crate::generator::rust::rust_name::rust_type_name;
use crate::semantic::type_node::TypeNode;
use crate::semantic::{ExpressionNode, ModelNode};

/// Литерал структуры: `Gains { kp: 2, ki: 3 }`.
///
/// Порядок значений - **объявленный** (инициализатор языка позиционный), имена полей
/// нормируются тем же правилом, что и объявление в `rust_decl`.
///
/// # Ошибки
/// `RS-011`, если структура не объявлена либо число значений не совпало с
/// числом полей: молча дополнять умолчаниями нельзя - это тихо иное значение.
pub(crate) fn struct_literal(
    name: &str,
    items: &[ExpressionNode],
    scope: &Scope,
) -> Result<String, Diagnostic> {
    let def = scope
        .model
        .search_struct(name)
        .ok_or_else(|| unsupported(&format!("структура '{name}' не объявлена")))?;
    if def.fields.len() != items.len() {
        return Err(unsupported(&format!(
            "инициализатор структуры '{name}': объявлено полей {}, значений {}",
            def.fields.len(),
            items.len()
        )));
    }
    let mut parts = Vec::with_capacity(items.len());
    for ((field, field_ty), value) in def.fields.iter().zip(items) {
        parts.push(format!(
            "{}: {}",
            crate::semantic::naming::normalize_lowercase_snakecase(field.clone()),
            coerce_to(value, field_ty, scope)?
        ));
    }
    Ok(format!(
        "{} {{ {} }}",
        rust_type_name(name, Location::Codegen)?,
        parts.join(", ")
    ))
}

/// Выводится ли `#[derive(Default)]` для типа.
///
/// Знание не о печати типа, а о **стандартной библиотеке Rust**: `Default` не выводится
/// у перечисления (нужен атрибут `#[default]` на варианте), а `impl Default for [T; N]`
/// существует только до `N = 32`. Структура, содержащая такое поле, наследует ответ -
/// отсюда рекурсия.
///
/// Длина массива берётся у **напечатанного** типа: бит-вектор шире 64 бит печатается
/// массивом слов `[u64; ⌈N/64⌉]`, и считать надо слова, а не разряды.
///
/// Признак нужен потому, что "печатать `impl Default` всегда" роняет проверка цели:
/// `clippy::derivable_impls` под `-D warnings` - отказ сборки. Там, где `derive` не
/// выводится, линт молчит по построению.
pub(crate) fn derives_default(ty: &TypeNode, model: &ModelNode) -> bool {
    // Вопрос "выводится ли derive У этой структуры" решают её поля, и каждое поле
    // спрашивается о другом - "есть ли у типа `Default`".
    if let TypeNode::Struct(name) = ty {
        return match model.search_struct(name) {
            Some(def) => def.fields.iter().all(|(_, ty)| has_default(ty)),
            // Структура не объявлена: отказ придёт из печати объявления, выбирать форму
            // умолчания уже незачем.
            None => true,
        };
    }
    has_default(ty)
}

/// Есть ли у типа реализация `Default` - любая, не обязательно выводимая.
///
/// Разница с [`derives_default`] и есть предмет: структура получает `Default` всегда
/// (цель печатает ей либо `derive`, либо ручной `impl`), поэтому поле-структура выводу
/// `derive` у владельца не мешает.
fn has_default(ty: &TypeNode) -> bool {
    match ty {
        // У перечисления `Default` не выводится вовсе.
        TypeNode::Enum(_) => false,
        TypeNode::Array(n, elem) => {
            // Бит-вектор шире 64 бит - массив слов `[u64; K]`: элемент `u64` умолчание
            // имеет, считаем K.
            if let Some(words) = crate::generator::rust::rust_bit::words_of_type(ty) {
                return words <= ARRAY_DEFAULT_LIMIT;
            }
            // `[bit; N <= 64]` - упакованный скаляр, длины у него нет.
            if crate::semantic::bit_vector::is_bit_vector(ty).is_some() {
                return true;
            }
            *n <= ARRAY_DEFAULT_LIMIT && has_default(elem)
        }
        // Структура `Default` имеет всегда - см. шапку функции.
        TypeNode::Struct(_) => true,
        _ => true,
    }
}

/// Предел длины массива, для которого стандартная библиотека Rust даёт `impl Default
/// for [T; N]`.
const ARRAY_DEFAULT_LIMIT: u16 = 32;

/// Тело `impl Default` структуры - литерал полей с умолчаниями.
///
/// Порядок полей - **объявленный** (как у [`struct_literal`]), значение каждого поля
/// даёт `default_value`: знание "чему равно умолчание типа" остаётся у одного носителя.
///
/// # Ошибки
/// `RS-014`, если умолчание поля не строится.
pub(crate) fn default_literal(
    name: &str,
    def: &crate::semantic::StructDefinitionNode,
    model: &ModelNode,
) -> Result<String, Diagnostic> {
    let mut parts = Vec::with_capacity(def.fields.len());
    for (field, field_ty) in &def.fields {
        parts.push(format!(
            "{}: {}",
            crate::semantic::naming::normalize_lowercase_snakecase(field.clone()),
            crate::generator::rust::rust_decl::default_value(field_ty, model)?
        ));
    }
    Ok(format!(
        "{} {{ {} }}",
        rust_type_name(name, def.loc)?,
        parts.join(", ")
    ))
}
