//! Стадии тел: именованные блоки, функции, ссылки состояний (стадии 4-6).
//!
//! Эти три стадии отличаются от остальных природой своих элементов.
//!
//! Стадии 0-3 строят предпосылки - имена, составные состояния, переменные, именованные
//! условия. Ошибка в предпосылке делает следствием всё, что на неё опирается, поэтому
//! там разбор терминален.
//!
//! Здесь элементы - **соседи**: от разрешённого блока или тела функции не зависит
//! другой такой же.
//!
//! Накопление - **внутри** стадии. Между стадиями переход остаётся терминальным:
//! разрешённые функции суть предпосылка для тел блоков, а тела блоков - для ссылок
//! состояний.
//!
//! Упавший элемент кладётся в дерево неразрешённым - иначе обход сожмётся и соседи не
//! будут просмотрены. Наружу такое дерево не выходит: при непустом списке ошибок
//! `stages::construct_stages` отдаёт `Err` и дерево отбрасывает.

use crate::diagnostics::Diagnostic;
use crate::parser::ast;
use crate::semantic::function::construct_function;
use crate::semantic::named_block::resolve_named_blocks;
use crate::semantic::reference::resolve_state_references;
use crate::semantic::tree::{resolve_formulas, resolve_state_named_blocks};
use crate::semantic::{FunctionDefinitionNode, ModelNode};
use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet};
use std::rc::Rc;

/// Этап 4: разрешение операторов в именованных блоках кода.
///
/// Выполняет три задачи:
/// 1. Разрешает блоки на уровне модели (`model.named_blocks`).
/// 2. Разрешает блоки в состояниях модели (`state.named_blocks`).
/// 3. Рекурсивно применяет этот же процесс ко всем вложенным моделям,
///    передавая контекст вложенной модели (для корректного разрешения
///    переменных во вложенных областях видимости).
///
/// Ошибка разрешения **пробрасывается**, а не гасится: оператор, оставленный как
/// [`StatementNode::Unresolved`], цель `c` печатает пустотой, а симулятор пропускает -
/// теряется не диагностика, а сам оператор.
///
/// # Накопление по соседям
///
/// Элементы этой стадии - соседи: от разрешённого блока не зависит никто, поэтому
/// ошибка в одном не делает следствием другую. Обход продолжается, а диагностики
/// копятся: неизвестные имена в телах разных состояний - две самостоятельные причины,
/// и автор вправе увидеть обе.
///
/// Между **стадиями** накопления нет: выход этой стадии - предпосылка для следующей, и
/// продолжение через предпосылку даёт каскад.
///
/// Упавший элемент кладётся в дерево **неразрешённым**, чтобы обход не сжался:
/// следующие соседи (и вложенные модели) обязаны быть просмотрены. Само дерево при
/// непустом списке ошибок отбрасывается вызывающим и наружу не выходит - потребители
/// неполного дерева не видят никогда.
pub(crate) fn construct_model_stage4(
    model: Rc<RefCell<ModelNode>>,
) -> Result<Rc<RefCell<ModelNode>>, Vec<Diagnostic>> {
    let mut errors: Vec<Diagnostic> = Vec::new();

    // Разрешаем формулы на уровне текущей модели
    let formulas = std::mem::take(&mut model.borrow_mut().formulas);
    match resolve_formulas(formulas.clone(), model.clone()) {
        Ok(resolved) => model.borrow_mut().formulas = resolved,
        Err(d) => {
            errors.push(d);
            model.borrow_mut().formulas = formulas;
        }
    }

    // Разрешаем блоки на уровне текущей модели
    let named_blocks = std::mem::take(&mut model.borrow_mut().named_blocks);
    match resolve_named_blocks(named_blocks.clone(), model.clone()) {
        Ok(resolved) => model.borrow_mut().named_blocks = resolved,
        Err(d) => {
            errors.push(d);
            model.borrow_mut().named_blocks = named_blocks;
        }
    }

    // Разрешаем блоки в состояниях текущей модели
    let states = std::mem::take(&mut model.borrow_mut().states);
    let mut resolved_states = BTreeMap::new();
    for (state_name, state) in states {
        match resolve_state_named_blocks(state.clone(), model.clone()) {
            Ok(resolved) => {
                resolved_states.insert(state_name, resolved);
            }
            Err(d) => {
                errors.push(d);
                resolved_states.insert(state_name, state);
            }
        }
    }
    model.borrow_mut().states = resolved_states;

    // Рекурсивно обрабатываем вложенные модели с их собственным контекстом
    let nested: Vec<(String, Rc<RefCell<ModelNode>>)> = model
        .borrow()
        .models
        .iter()
        .map(|(k, v)| (k.clone(), Rc::clone(v)))
        .collect();
    let mut models = BTreeMap::new();
    for (name, nested_model) in nested {
        match construct_model_stage4(Rc::clone(&nested_model)) {
            Ok(resolved) => {
                models.insert(name, resolved);
            }
            Err(mut ds) => {
                errors.append(&mut ds);
                models.insert(name, nested_model);
            }
        }
    }
    model.borrow_mut().models = models;

    if errors.is_empty() {
        Ok(Rc::clone(&model))
    } else {
        Err(errors)
    }
}

/// # Накопление по вершинам графа вызовов
///
/// Функции - соседи **лишь частично**: `g`, зовущая упавшую `f`, дала бы следствие, а
/// не причину (разрешаемая функция изымается из карты, и `g` не нашла бы её вовсе).
/// Поэтому ошибка в теле `f` **исключает из обхода** функции, транзитивно зависящие от
/// `f`; независимые ветви графа продолжают разрешаться, и их диагностики копятся.
///
/// Граф уже построен для топологического порядка - второго обхода не заводится;
/// "зависимые от `f`" берутся из него же.
pub(crate) fn construct_model_stage5(
    model: Rc<RefCell<ModelNode>>,
) -> Result<Rc<RefCell<ModelNode>>, Vec<Diagnostic>> {
    // Тела функций разрешаются в топологическом порядке графа вызовов и без изъятия
    // карты: при разборе вызывающей `search_func` видит уже разрешённые функции
    // (композиция `f -> g`). Цикл вызовов отвергает `SE-053`.
    let bodies: BTreeMap<String, Option<ast::Statement>> = {
        let borrowed = model.borrow();
        borrowed
            .functions
            .iter()
            .filter_map(|(name, f)| match f {
                FunctionDefinitionNode::Unresolved(def) => Some((name.clone(), def.body.clone())),
                // Уже разрешённые (например, встроенные) в граф не входят.
                _ => None,
            })
            .collect()
    };
    let loc = model.borrow().loc;
    let graph = crate::semantic::callgraph::build_call_graph(&bodies);
    // Цикл (`SE-053`) - свойство графа, а не отдельной функции: накапливать нечего,
    // отказ терминален.
    let order = crate::semantic::callgraph::topological_order(&graph, loc).map_err(|d| vec![d])?;

    let mut errors: Vec<Diagnostic> = Vec::new();
    // Функции, чьи тела не разрешились, и всё, что от них зависит. Порядок
    // топологический (вызываемая раньше вызывающей), поэтому транзитивность получается
    // сама: к моменту разбора `g` её `f` уже помечена.
    let mut failed: BTreeSet<String> = BTreeSet::new();

    for name in order {
        if graph
            .get(&name)
            .is_some_and(|callees| callees.iter().any(|c| failed.contains(c)))
        {
            // Следствие, а не причина: вызываемая не разрешилась, и её вызов дал бы
            // "функция не найдена". Диагностику не заводим.
            failed.insert(name);
            continue;
        }
        // Изымаем только разрешаемую функцию; её вызываемые уже в карте и разрешены
        // (топологический порядок), поэтому вызовы к ним встроят разрешённые узлы. Само
        // изъятие даёт `search_func` не найти функцию в её же теле - но прямой
        // самовызов уже отвергнут как цикл выше.
        let func = model.borrow_mut().functions.remove(&name);
        if let Some(func) = func {
            match construct_function(func.clone(), model.clone()) {
                Ok(resolved) => {
                    model.borrow_mut().functions.insert(name, resolved);
                }
                Err(d) => {
                    errors.push(d);
                    failed.insert(name.clone());
                    model.borrow_mut().functions.insert(name, func);
                }
            }
        }
    }

    // Рекурсивно обрабатываем вложенные модели с их собственным контекстом. Карта
    // функций текущей (родительской) модели уже восстановлена - поэтому вложенные
    // модели видят разрешённые функции родителя (межмодельный `fn->fn`).
    let nested: Vec<(String, Rc<RefCell<ModelNode>>)> = model
        .borrow()
        .models
        .iter()
        .map(|(k, v)| (k.clone(), Rc::clone(v)))
        .collect();
    let mut models = BTreeMap::new();
    for (name, nested_model) in nested {
        match construct_model_stage5(Rc::clone(&nested_model)) {
            Ok(resolved) => {
                models.insert(name, resolved);
            }
            Err(mut ds) => {
                errors.append(&mut ds);
                models.insert(name, nested_model);
            }
        }
    }
    model.borrow_mut().models = models;

    if errors.is_empty() {
        Ok(Rc::clone(&model))
    } else {
        Err(errors)
    }
}

/// Условия рёбер `ref` - **соседи**: от разрешённого ребра не зависит ни одно другое,
/// поэтому стадия накапливает диагностики по состояниям и вложенным моделям. Упавшее
/// состояние остаётся неразрешённым, чтобы обход не сжался; при непустом списке ошибок
/// дерево отбрасывается вызывающим.
pub(crate) fn construct_model_stage6(
    model: Rc<RefCell<ModelNode>>,
) -> Result<Rc<RefCell<ModelNode>>, Vec<Diagnostic>> {
    let mut errors: Vec<Diagnostic> = Vec::new();
    let states = model.borrow().states.clone();

    let mut prepared_states = BTreeMap::new();
    for (name, state) in states.iter() {
        match resolve_state_references(state) {
            Ok(prepared) => {
                prepared_states.insert(name.clone(), prepared);
            }
            Err(d) => {
                errors.push(d);
                prepared_states.insert(name.clone(), state.clone());
            }
        }
    }
    model.borrow_mut().states = prepared_states;

    // Клонируем список вложенных моделей до рекурсивного вызова
    let nested: Vec<(String, Rc<RefCell<ModelNode>>)> = model
        .borrow()
        .models
        .iter()
        .map(|(k, v)| (k.clone(), Rc::clone(v)))
        .collect();

    let mut models = BTreeMap::new();
    for (name, nested_model) in nested {
        match construct_model_stage6(Rc::clone(&nested_model)) {
            Ok(resolved) => {
                models.insert(name, resolved);
            }
            Err(mut ds) => {
                errors.append(&mut ds);
                models.insert(name, nested_model);
            }
        }
    }
    model.borrow_mut().models = models;

    if errors.is_empty() {
        Ok(Rc::clone(&model))
    } else {
        Err(errors)
    }
}
