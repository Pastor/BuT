//! Контекст модели: значения переменных по дереву `ModelNode`.
//!
//! Вынесено из `builder.rs`: файл вместе с этим кодом дал 1024 строки при лимите 1000,
//! а контекст - самостоятельная тема: он отвечает на вопрос "где живёт значение", тогда
//! как строитель - на вопрос "из чего собран юнит".

use crate::context::Context;
use crate::eval::value::Value;
use crate::unit::builder::{coerce_initial, default_field, var_expr, var_type};
use crate::unit::initial::eval_expr_in;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use takt_lang::semantic::{ModelNode, VariableNode};

// -- ModelNodeContext ----------------------------------------------------------

/// Контекст с иерархической структурой, зеркалирующей цепочку ModelNode.upper.
///
/// Хранит прямую ссылку на ModelNode (Rc) - переменные не копируются.
/// При запросе переменной:
/// 1. Проверяет локальный кэш (Value уже вычислен ранее).
/// 2. Запрашивает `model.variables` напрямую, вычисляет Value из ExpressionNode.
/// 3. Копирует результат в кэш (ленивая инициализация).
/// 4. Если в текущей модели не найдено - поднимается к `parent` (ModelNode.upper).
///
/// Для параллельных моделей `parent` является общим (`Rc`) - изменения одной подмодели
/// сразу видны остальным через общий родительский контекст.
pub(crate) struct ModelNodeContext {
    model: Rc<RefCell<ModelNode>>,
    /// Ленивый кэш вычисленных значений.
    ///
    /// `pub(crate)`: строитель юнита сбрасывает его при записи значения - иначе
    /// следующее чтение вернуло бы прежнее (поле открыто при выносе модуля,; прежде оба
    /// жили в одном файле).
    pub(crate) cache: RefCell<HashMap<String, Value>>,
    parent: Option<Rc<RefCell<dyn Context>>>,
    /// Стенд внешних функций текущего шага сценария.
    ///
    /// Ставится перед тактом (`Unit::set_extern_stubs`) - как модельное время; пустой
    /// стенд означает прежний отказ `SIM-019`.
    extern_stubs: crate::context::ExternStubs,
    /// Реестр текущих состояний моделей прогона.
    ///
    /// Наполняется только в контексте-Корне цепочки: и чтение, и запись поднимаются к
    /// нему через `parent`. Так модели-сёстры, у которых свои контексты и общий
    /// родитель, видят состояния друг друга - тем же путём, каким видят общие
    /// переменные корня.
    states: RefCell<HashMap<String, String>>,
}

impl ModelNodeContext {
    pub(crate) fn new(model: Rc<RefCell<ModelNode>>) -> Self {
        let parent = model
            .borrow()
            .upper
            .as_ref()
            .and_then(|w| w.upgrade())
            .map(|parent_rc| {
                Rc::new(RefCell::new(ModelNodeContext::new(parent_rc))) as Rc<RefCell<dyn Context>>
            });
        Self {
            model,
            cache: RefCell::new(HashMap::new()),
            parent,
            extern_stubs: Default::default(),
            states: RefCell::new(HashMap::new()),
        }
    }

    pub(crate) fn new_with_parent(
        model: Rc<RefCell<ModelNode>>,
        parent: Option<Rc<RefCell<dyn Context>>>,
    ) -> Self {
        Self {
            model,
            cache: RefCell::new(HashMap::new()),
            parent,
            extern_stubs: Default::default(),
            states: RefCell::new(HashMap::new()),
        }
    }
}

impl Context for ModelNodeContext {
    /// Стенд свой, а при пустом - родительский: ветви композиции читают значения того
    /// же шага сценария, что и корень.
    fn extern_result(&self, name: &str, args: &[Value]) -> Option<Value> {
        if !self.extern_stubs.is_empty()
            && let Some(value) = self.extern_stubs.result(name, args)
        {
            return Some(value);
        }
        self.parent
            .as_ref()
            .and_then(|p| p.borrow().extern_result(name, args))
    }

    fn set_extern_stubs(&mut self, stubs: crate::context::ExternStubs) {
        self.extern_stubs = stubs;
    }

    fn get_value(&self, name: &str) -> Option<Value> {
        if let Some(v) = self.cache.borrow().get(name) {
            return Some(v.clone());
        }
        let value = {
            let borrowed = self.model.borrow();
            borrowed.variables.get(name).and_then(|var| {
                match eval_expr_in(var_expr(var), &borrowed) {
                    Some(v) => Some(coerce_initial(v, var, &borrowed)),
                    // Переменная без инициализатора -> нулевое значение по типу (как
                    // default-init в C). распространяет политику на все типы:
                    // `default_field` покрывает bool/rational/fixed/array/struct/целое
                    // единообразно.
                    None => var_type(var).map(|ty| default_field(ty, &borrowed)),
                }
            })
        };
        if let Some(value) = value {
            self.cache
                .borrow_mut()
                .insert(name.to_string(), value.clone());
            return Some(value);
        }
        self.parent
            .as_ref()
            .and_then(|p| p.borrow().get_value(name))
    }

    fn set_value(&mut self, name: &str, value: Value) {
        if self.model.borrow().variables.contains_key(name) {
            self.cache.borrow_mut().insert(name.to_string(), value);
        } else if let Some(parent) = &self.parent {
            parent.borrow_mut().set_value(name, value);
        } else {
            self.cache.borrow_mut().insert(name.to_string(), value);
        }
    }

    /// Состояние модели прогона: вопрос адресуется корню цепочки - там лежит общий
    /// реестр, туда же пишут все узлы.
    fn model_state(&self, model: &str) -> Option<String> {
        match &self.parent {
            Some(parent) => parent.borrow().model_state(model),
            None => self.states.borrow().get(model).cloned(),
        }
    }

    /// Запись состояния - в тот же корневой реестр.
    fn set_model_state(&self, model: &str, state: &str) {
        match &self.parent {
            Some(parent) => parent.borrow().set_model_state(model, state),
            None => {
                self.states
                    .borrow_mut()
                    .insert(model.to_string(), state.to_string());
            }
        }
    }

    /// Определение структуры по имени: `search_struct` учитывает родительские модели по
    /// слабым ссылкам `upper`.
    fn find_struct(&self, name: &str) -> Option<takt_lang::semantic::StructDefinitionNode> {
        self.model.borrow().search_struct(name)
    }

    /// Перечисляет значения состояния модели для снимка.
    ///
    /// Идёт по именам `model.variables`, вычисляя значение через собственный
    /// `get_value` (что попутно материализует ленивый кэш). **Константы исключаются** -
    /// их значение задано исходником, восстанавливать из файла опасно (исходник мог
    /// измениться). Родитель накладывается **первым**, затем перекрывается значениями
    /// текущей модели - та же приоритетность, что у `get_value` (локальное имя
    /// выигрывает у родительского).
    fn dump(&self) -> HashMap<String, Value> {
        let mut out: HashMap<String, Value> = self
            .parent
            .as_ref()
            .map(|p| p.borrow().dump())
            .unwrap_or_default();
        let names: Vec<String> = {
            let borrowed = self.model.borrow();
            borrowed
                .variables
                .iter()
                .filter(|(_, var)| !matches!(var, VariableNode::Const { .. }))
                .map(|(name, _)| name.clone())
                .collect()
        };
        for name in names {
            if let Some(value) = self.get_value(&name) {
                out.insert(name, value);
            }
        }
        out
    }
}
