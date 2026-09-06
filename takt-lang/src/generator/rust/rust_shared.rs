//! Общие переменные корня и структура `Shared` для цели `rust`.
//!
//! Под-модель читает состояние корня, хотя `self.cabin.tick(&mut self)` заимствует
//! `self` дважды (E0499). Поэтому общие переменные передаются **параметром**. С они
//! свёрнуты в одну структуру `<Root>Shared`, ретранслируемую вниз по композиции
//! параметром `&mut Shared`, - вместо передачи по одной, которая упиралась в
//! `clippy::too_many_arguments` и требовала заглушки (единственного исключения из
//! политики (а) ).
//!
//! `Shared` = **объединение** нужд под-моделей: переменная корня, не нужная ни одной
//! под-модели, в состав не входит и остаётся прямым полем (иначе имя типа лжёт, а поле
//! ловит `dead_code`). Порядок полей задан `BTreeMap`/сортировкой (детерминизм 0048).

use crate::diagnostics::{Diagnostic, Location};
use crate::generator::indent::Printer;
use crate::generator::rust::rust_map::RustMap;
use crate::generator::rust::rust_name::rust_value_name;
use crate::generator::rust::rust_type::rust_type;
use crate::semantic::VariableNode;
use crate::semantic::minimap::{Element, Name};
use crate::semantic::type_node::TypeNode;
use std::collections::{BTreeMap, BTreeSet};

/// Общие переменные корня, нужные **конкретной** под-модели `sub`.
///
/// Список считается по **фактическому** использованию (сама под-модель плюс корневые
/// функции, которые она вызывает), а не "все переменные корня": лишний параметр -
/// лишний обязательный аргумент у каждого вызова.
pub(crate) fn shared_variables(map: &RustMap, sub: &Name) -> Vec<(String, TypeNode)> {
    let Some(root) = map.root_model_node() else {
        return Vec::new();
    };
    let Ok(sub_model) = map.raw_model_at(sub.clone()) else {
        return Vec::new();
    };
    // Использование считает общий носитель: он видит и вложенные модели, и те, которыми
    // Реализованы состояния.
    let usage = crate::semantic::usage_tree::usage_with_implementations(&sub_model);
    let own: Vec<String> = sub_model.borrow().variables.keys().cloned().collect();
    let root_ref = root.borrow();

    // Переменные корня, нужные функциям, которые под-модель вызывает: `travel_time`
    // объявлена в корне и читает его переменные, а `compute_usage(sub)` обходит только
    // Свои тела. Цель `c` такой задачи не знает - там под-модель отдаёт указатель
    // `main`.
    let mut from_calls: BTreeSet<String> = BTreeSet::new();
    for fname in &usage.functions {
        let Ok(needs) = crate::generator::rust::rust_needs::needs_of_call(
            fname,
            &sub_model.borrow(),
            &mut BTreeSet::new(),
        ) else {
            continue;
        };
        from_calls.extend(needs.vars.into_keys());
    }

    let mut shared = Vec::new();
    for (name, var) in &root_ref.variables {
        // Константы не разделяются: они неизменны и живут на уровне модуля.
        let VariableNode::Simple { ty, .. } = var else {
            continue;
        };
        let used = usage.variables.contains(name) || from_calls.contains(name);
        if used && !own.contains(name) {
            shared.push((name.clone(), ty.clone()));
        }
    }
    shared
}

/// Состав структуры `Shared` корня - **объединение** нужд всех под-моделей.
///
/// Общие переменные root-центричны (`shared_variables` считает нужды от корня), поэтому
/// `Shared` - **одна** структура корня, ретранслируемая вниз.
pub(crate) fn shared_union(map: &RustMap) -> Vec<(String, TypeNode)> {
    let mut union: BTreeMap<String, TypeNode> = BTreeMap::new();
    for element in map.using_models() {
        let Element::Model { name, .. } = element else {
            continue;
        };
        for (vname, ty) in shared_variables(map, &name) {
            union.entry(vname).or_insert(ty);
        }
    }
    union.into_iter().collect()
}

/// Имя типа `Shared` корня: `<Root>Shared`.
///
/// Тип **один на корень** (а не голое `Shared`): в плоском модуле цели `rust` модели
/// соседствуют, и `Shared` без преисправления столкнулось бы. Уточнение реализации к (тот
/// приводил имя `Shared` условно).
pub(crate) fn shared_type_name(map: &RustMap) -> String {
    format!("{}Shared", map.root_name().unique_camelcase())
}

/// Множество имён общих переменных корня (для фильтрации прямых полей).
pub(crate) fn union_names(map: &RustMap, is_root: bool) -> BTreeSet<String> {
    if is_root {
        shared_union(map).into_iter().map(|(n, _)| n).collect()
    } else {
        BTreeSet::new()
    }
}

/// Печатает объявление `struct <Root>Shared { ... }` - **приватное** (: `pub` заглушила
/// бы `dead_code`-теста видимостью).
pub(crate) fn emit_shared_struct(
    p: &mut Printer,
    map: &RustMap,
    shared: &[(String, TypeNode)],
) -> Result<(), Diagnostic> {
    if shared.is_empty() {
        return Ok(());
    }
    p.ident(&format!("struct {} {{", shared_type_name(map)))
        .nl();
    p.up();
    for (vname, ty) in shared {
        p.ident(&format!(
            "{}: {},",
            rust_value_name(vname, Location::Codegen)?,
            rust_type(ty, &format!("общая переменная '{}'", vname))?
        ))
        .nl();
    }
    p.down();
    p.ident("}").nl().nl();
    Ok(())
}

/// Печатает блок `shared: <Root>Shared { ... }` в конструкторе `new`.
///
/// `inits` - уже посчитанные значения инициализаторов общих переменных (по имени
/// переменной); печать идёт в порядке `union` (детерминизм 0048).
///
/// **Умолчание строит `default_value` - тот же носитель, что у прямых полей**
/// . Прежде здесь стояла строка `"Default::default()"`, то есть
/// **второй** носитель знания "чему равно умолчание типа": он работал, лишь пока
/// у структуры выводится `Default` ( показала, что выводится он не
/// всегда), и разошёлся бы с первым при первом же новом типе - класс
/// 0084/0193/0195.
///
/// **Ветвь защитная: достижимого входа у неё нет.** Зонд 2026-08-21 (`panic!` вместо
/// фолбэка) прошёл весь корпус и все 13 наборов тестов, включая входы с общей
/// переменной корня и с переменной, читаемой только функцией, которую зовёт под-модель.
/// Переменная попадает в `union` из объявлений корня, а `model_fields` корня их и
/// обходит - значит значение в `inits` есть. Ветвь оставлена: молчаливая подмена
/// умолчания была бы хуже, чем лишний путь.
pub(crate) fn emit_shared_new_block(
    p: &mut Printer,
    map: &RustMap,
    union: &[(String, TypeNode)],
    inits: &BTreeMap<String, String>,
) -> Result<(), Diagnostic> {
    if union.is_empty() {
        return Ok(());
    }
    p.ident(&format!("shared: {} {{", shared_type_name(map)))
        .nl();
    p.up();
    let root = map.root_model_node();
    for (vname, ty) in union {
        let value = match inits.get(vname) {
            Some(value) => value.clone(),
            None => match root.as_ref() {
                Some(model) => {
                    crate::generator::rust::rust_decl::default_value(ty, &model.borrow())?
                }
                // Корня нет - умолчание строить не из чего; прежнее поведение.
                None => "Default::default()".to_string(),
            },
        };
        p.ident(&format!(
            "{}: {},",
            rust_value_name(vname, Location::Codegen)?,
            value
        ))
        .nl();
    }
    p.down();
    p.ident("},").nl();
    Ok(())
}
