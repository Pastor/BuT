//! Валидация семантических узлов языка Takt.
//!
//! Проверяет семантические инварианты после построения дерева. Проверки выполняются
//! рекурсивно для всех вложенных моделей.
//!
//! ## Текущие проверки
//!
//! - Модель, содержащая состояния, должна иметь ровно одно начальное
//!   состояние (`start`). Модели без состояний (только с объявлениями
//!   переменных, типов и т.п.) от этой проверки освобождены.
//!
//! - Переменная типа `bit` может быть инициализирована только значениями
//!   `0`, `1`, `true` или `false`. Любое другое числовое значение - ошибка.
//!
//! - Условие перехода (`ref`) не должно содержать неявного приведения
//!   числового типа к булевому. Использование переменных числового типа
//!   (например, `[bit;8]`) без явного сравнения порождает предупреждение
//!   [`check_implicit_bool_conditions`].

use crate::diagnostics::{Diagnostic, Location};
use crate::parser::ast;
use crate::semantic::condition::resolve_condition;
use crate::semantic::type_node::TypeNode;
use crate::semantic::{
    ConditionNode, ExpressionNode, FunctionDefinitionNode, ModelNode, PortDirection, ReferenceNode,
    StateNode, StateNodeKind, StatementNode, VariableNode,
};
use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

mod aggregate_length;
mod arity;
mod assignment_place;
mod assignment_position;
pub(crate) mod base_type;
mod bodies;
mod common;
mod constant_conditions;
pub mod depth;
mod entry;
mod enums;
mod fixed;
mod formulas;
mod implemented;
mod implicit_bool;
mod init_undefined_read;
// `pub(crate)`, а не `mod`: границы целочисленного типа (`type_range`) нужны и свёртке
// инициализатора - вторая копия границ разошлась бы с проверкой `SE-089`.
mod duplicate_match_arm;
pub(crate) mod literal_range;
mod member_access;
mod name_collisions;
mod nondeterminism;
mod port_init;
mod ports;
mod states;
pub(crate) mod struct_cycle;
mod structs;
mod types;
mod unreachable_edge;

#[cfg(test)]
mod tests;
#[cfg(test)]
mod tests_ce15_array_size;
#[cfg(test)]
mod tests_ce4_declarations;
#[cfg(test)]
mod tests_implicit_bool;

// Внутреннее: помощники, которые зовут `validate_model` и соседние подмодули. `use
// super::*` в каждом подмодуле подхватывает их отсюда.
use common::{
    get_state_loc, get_state_name, validate_cond, validate_conditions, validate_expression,
    validate_reference,
};
use enums::{
    validate_bit_values, validate_empty_enums, validate_enum_type_declarations,
    validate_enum_values,
};
use fixed::check_fixed_mixing;
use ports::{check_port_addresses, validate_variables};
use states::{model_only_one_start_state, validate_state_references};
use types::check_array_sizes;

// Реэкспорт: внешние пути импорта не меняются. Потребители - `semantic/tree.rs` (5
// имён) и `lib.rs` (6 имён по пути `semantic::validate::...`) - не правятся.
pub(crate) use common::reachable_targets;
pub use constant_conditions::check_constant_conditions;
pub use duplicate_match_arm::check_duplicate_match_arms;
pub use entry::validate_entry_model;
pub use enums::check_enum_type_safety;
pub use implicit_bool::check_implicit_bool_conditions;
pub use nondeterminism::check_nondeterministic_transitions;
pub use ports::{check_port_address_completeness, warn_nested_model_ports};
pub use states::{check_transition_completeness, check_unreachable_states};
pub use structs::{
    check_duplicate_struct_fields, check_struct_field_types, validate_empty_structs,
};
pub use types::{check_recursive_type_aliases, check_type_alias_cycles_ast};
pub use unreachable_edge::check_unreachable_edges;

/// Проверяет модель, останавливаясь на первой ошибке.
///
/// Контракт прежний (183 вызова в проекте), но реализация - "первая из
/// [`validate_model_all`]": два входа, написанные порознь, разошлись бы, и пользователь
/// получал бы разный ответ в зависимости от того, кто спрашивает.
pub fn validate_model(model: Rc<RefCell<ModelNode>>) -> Result<(), Diagnostic> {
    match validate_model_all(model).into_iter().next() {
        Some(diagnostic) => Err(diagnostic),
        None => Ok(()),
    }
}

/// Проверяет модель, собирая **все** найденные ошибки.
///
/// Проверки идут по **готовому** дереву и независимы друг от друга, поэтому каждая
/// может сообщить о своём, не мешая соседям: пользователь видит причины, а не первую
/// попавшуюся.
///
/// ## Что накапливается, а что нет
///
/// - **Между проверками** - да: все четырнадцать высказываются.
/// - **Внутри проверки** - по-разному: пять из них возвращают `Vec` и отдают всё
///   найденное, остальные устроены как цикл с ранним возвратом и дают по одной
///   ошибке на модель. Углубление - отдельная работа (граница ).
/// - **Вложенные модели** обходятся рекурсивно, и их диагностики добавляются к
///   общему списку.
///
/// Порядок здесь - порядок проверок и обхода (детерминированного); упорядочить по
/// позиции в тексте - задача выдачи
/// ([`diagnostics::normalize`](crate::diagnostics::normalize)).
pub fn validate_model_all(model: Rc<RefCell<ModelNode>>) -> Vec<Diagnostic> {
    let mut found = Vec::new();

    // Каждая проверка накапливает диагностики: с `Result<(), Diagnostic>` на модель
    // приходилось бы не больше одной ошибки, и две неверные переменные давали бы одно
    // сообщение. Правило накопления - одна диагностика на элемент, все элементы
    // высказываются; внутри одного выражения ранний выход сохранён, потому что дальше по
    // нему пошли бы следствия первой ошибки.
    let checks: [Vec<Diagnostic>; 14] = [
        model_only_one_start_state(model.clone()),
        // SE-106: модель без состояний, поставленная в реализацию. Без отказа один вход
        // давал шесть разных ответов, и два из них молчаливые: `plantuml` печатал
        // диаграмму с переходом в никуда, эталон исполнял пустую трассу.
        implemented::validate_implemented_models(model.clone()),
        // SE-099 и SE-113: чтение неопределённой памяти в инициализаторе объявления -
        // ячейки по адресу и порта по имени. Без запрета эталон дал бы ноль, `c-hal` -
        // чтение регистра, а `st` молча потеряла бы инициализатор. Правило одно, обход
        // один, кодов два.
        init_undefined_read::validate_undefined_reads_in_initializers(model.clone()),
        validate_bit_values(model.clone()),
        // SE-105: перечисление без вариантов - отказ на объявлении. Без него
        // использование пустого перечисления даёт бессодержательное `SE-043` "...не
        // является вариантом перечисления (допустимые варианты: )". Порядок в этом
        // массиве на порядок сообщений не влияет: выдачу упорядочивает
        // `diagnostics::normalize` по позиции в тексте.
        validate_empty_enums(model.clone()),
        // SE-115: структура без полей - отказ на объявлении, симметрично SE-105. Без
        // него цель `c` печатала расширение GNU, а `iec2c` отвергал порождённый ST:
        // проверка проекта класс не видел (он гоняет `cc` без `-pedantic`, а в корпусе
        // пустых структур нет).
        validate_empty_structs(model.clone()),
        validate_enum_values(model.clone()),
        validate_enum_type_declarations(model.clone()),
        validate_state_references(model.clone()),
        validate_variables(model.clone()),
        validate_conditions(model.clone()),
        check_array_sizes(model.clone()),
        check_port_addresses(model.clone()),
        check_fixed_mixing(model.clone()), // T6: запрет смешения q(m, n)
    ];
    found.extend(checks.into_iter().flatten());

    // Рекурсивные псевдонимы: проверка отдаёт все циклы сразу, и вызывающий берёт их
    // целиком - иначе накопление здесь написано, но выбрасывается.
    found.extend(check_recursive_type_aliases(model.clone()));

    // Ce17/Ce18: структуры - по одной ошибке от каждой проверки.
    found.extend(check_duplicate_struct_fields(model.clone()));
    found.extend(check_struct_field_types(model.clone()));

    // SE-061: доступ к несуществующему полю структуры.
    found.extend(member_access::check_struct_field_access(model.clone()).err());

    // SE-089: литерал не помещается в тип приёмника. Отдаёт все находки: одна ошибка на
    // литерал, а не "первая на модель".
    found.extend(literal_range::check_literal_ranges(model.clone()));
    found.extend(aggregate_length::check_aggregate_lengths(model.clone()));

    // Тела модели - один обход, два судьи (`validate/bodies.rs`):
    //  * SE-026 и SE-027: направление порта во всех позициях. Обойди проход тела
    //    блоков и функций стороной - и нарушение уедет в цели, расходясь вплоть до
    //    записи по адресу другого порта;
    //  * SE-095: присваивание - оператор, а не выражение. Без него `x := (led := 1) + 1`
    //    не отвергает никто: диагностику дают чужие инструменты на порождённом файле, а
    //    цель `c` для переменных исполняет то, чего не исполняет эталон.
    found.extend(bodies::check_bodies(model.clone()));

    // SE-100 и SE-101: столкновения имён, неразрешимые в пространстве имён цели. Отказ
    // даёт свой компилятор, а не чужие инструменты (`cc`, `iec2c`): обе формы не
    // работают ни в одной цели, поэтому запрещается неработающая запись, а отказ несёт
    // позицию в исходнике.
    found.extend(name_collisions::check_name_collisions(model.clone()));

    // SE-025 и прочие проверки условия - на охранных формулах. Судья здесь тот же, что
    // у `cond` и рёбер: проверка лишь доставляет ему условия.
    found.extend(formulas::validate_formulas(model.clone()));

    // SE-092: начальное значение у входного порта - ошибка. Временное SE-093
    // ("выставляют не все цели") снято: цели умеют все.
    found.extend(port_init::check_port_initializers(model.clone()));

    let nested: Vec<Rc<RefCell<ModelNode>>> =
        model.borrow().models.values().map(Rc::clone).collect();
    for nested_model in nested {
        found.extend(validate_model_all(nested_model));
    }
    found
}
