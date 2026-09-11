//! Снимок семантической карты модели для генератора Structured Text.
//!
//! Обёртка над [`Map`] из [`crate::semantic::minimap`] по образцу
//! [`CMap`](crate::generator::c) - снимок дерева плюс множество используемых имён
//! ([`UsageSet`]), чтобы не эмитить в `FUNCTION_BLOCK` объявления, которых модель не
//! использует.

use crate::address_map::ResolvedAddress;
use crate::diagnostics::lang::keys;
use crate::diagnostics::{Diagnostic, Location};
use crate::msg;
use crate::semantic::minimap::{Element, Map, Name, StateExtend};
use crate::semantic::type_node::TypeNode;
use crate::semantic::unused::UsageSet;
use crate::semantic::{ModelNode, VariableNode};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// Снимок модели, подготовленный для генерации Structured Text.
pub(crate) struct StMap {
    filename: String,
    map: Map,
    /// Множество используемых имён модели (для фильтрации неиспользуемых элементов).
    usage: UsageSet,
    /// Режим потребления адресов (`st-at`): эмитить `AT %...` у портов.
    ///
    /// Соответствует [`GenerateOptions::hal`](crate::generator::GenerateOptions::hal);
    /// потребляется, здесь только переносится в снимок.
    at_addresses: bool,
    /// Разрешённые адреса портов: `resolve_addresses` с приоритетом источников.
    addresses: HashMap<String, ResolvedAddress>,
    /// Профиль времени: "часы" (штатный `TON`) либо "такты" (счётчик). Разрешается
    /// общим слоем `resolve_profile` в `generate` - по образцу `CMap`.
    time_profile: crate::semantic::duration::TimeProfile,
    /// Форма печати автомата: `CASE` либо таблица переходов.
    fsm: crate::generator::FsmForm,
    /// Комментарии автора модели для переноса в вывод.
    comments: Option<std::rc::Rc<crate::generator::comments::SourceComments>>,
}

/// Собирает имена моделей из дерева реализации состояния.
fn collect_extend_models(extend: &StateExtend, out: &mut Vec<String>) {
    match extend {
        StateExtend::None => {}
        StateExtend::Model(name, _) => out.push(name.unique().to_string()),
        StateExtend::Concatenation(steps) | StateExtend::Parallel(steps) => {
            steps.iter().for_each(|s| collect_extend_models(s, out))
        }
    }
}

impl StMap {
    /// Строит снимок модели из семантического дерева.
    ///
    /// # Ошибки
    /// Возвращает [`Diagnostic`], если у модели нет стартового состояния.
    pub fn new(
        filename: &str,
        model: &ModelNode,
        at_addresses: bool,
        addresses: HashMap<String, ResolvedAddress>,
    ) -> Result<Self, Diagnostic> {
        let model_rc = Rc::new(RefCell::new(model.copy(None, None)));
        let usage = crate::semantic::unused::compute_usage(Rc::clone(&model_rc));
        Ok(Self {
            filename: filename.to_string(),
            map: Map::create(model_rc)?,
            usage,
            at_addresses,
            addresses,
            time_profile: crate::semantic::duration::TimeProfile::default(),
            fsm: crate::generator::FsmForm::default(),
            comments: None,
        })
    }

    /// Задаёт форму печати автомата; умолчание - `CASE`. Комментарии автора модели.
    pub(crate) fn with_comments(
        mut self,
        comments: Option<std::rc::Rc<crate::generator::comments::SourceComments>>,
    ) -> Self {
        self.comments = comments;
        self
    }

    pub(crate) fn with_fsm(mut self, fsm: crate::generator::FsmForm) -> Self {
        self.fsm = fsm;
        self
    }

    /// Печатается ли автомат таблицей переходов (`--fsm=table`).
    pub(crate) fn fsm_table(&self) -> bool {
        self.fsm == crate::generator::FsmForm::Table
    }

    /// Узел состояния по имени - для общего носителя строк таблицы.
    pub(crate) fn raw_state_at(
        &self,
        name: Name,
    ) -> Result<Rc<RefCell<crate::semantic::StateNode>>, Diagnostic> {
        self.map
            .state_at(Some(name.unique().to_string()))
            .ok_or_else(|| {
                Diagnostic::error(
                    Location::Codegen,
                    msg!(keys::ST_012_STATE_NOT_IN_MAP, name = name),
                )
                .with_code("ST-012")
            })
    }

    /// Задаёт профиль времени; умолчание - "часы" (аддитивно).
    pub(crate) fn with_time_profile(
        mut self,
        profile: crate::semantic::duration::TimeProfile,
    ) -> Self {
        self.time_profile = profile;
        self
    }

    /// Профиль времени, выбранный для генерации.
    pub(crate) fn time_profile(&self) -> crate::semantic::duration::TimeProfile {
        self.time_profile
    }

    /// Разрешённый адрес порта по имени (цель `st-at`).
    pub(crate) fn address_of(&self, port: &str) -> Option<&ResolvedAddress> {
        self.addresses.get(port)
    }

    /// Возвращает базовое имя файла (без расширения).
    pub fn get_filename(&self) -> &str {
        &self.filename
    }

    /// Возвращает имя корневой модели.
    pub fn root_name(&self) -> Name {
        self.map.root_name()
    }

    /// Возвращает элемент корневой модели (`Element::Model`).
    pub fn model(&self) -> Element {
        self.map.model()
    }

    /// Возвращает список имён состояний корневой модели.
    #[allow(dead_code)]
    pub fn states(&self) -> Vec<Name> {
        self.map.states()
    }

    /// Возвращает элемент состояния по имени, или `None`, если не найден либо не
    /// является состоянием.
    #[allow(dead_code)]
    pub fn state_at(&self, name: Name) -> Option<Element> {
        if let Some(element) = self.map.element_at(name)
            && element.is_state()
        {
            Some(element)
        } else {
            None
        }
    }

    /// Возвращает список подмоделей, используемых через `StateExtend`.
    pub fn using_models(&self) -> Vec<Element> {
        self.map.used_models()
    }

    /// Возвращает уникальные имена моделей, чьи экземпляры заводит данная модель.
    ///
    /// Нужно для порядка объявления: в ST тип экземпляра обязан быть известен к моменту
    /// объявления, а опережающие ссылки - нестандартное расширение.
    pub(crate) fn instantiated_by(&self, model: &Name) -> Vec<String> {
        let Some(Element::Model { states, .. }) = self.map.element_at(model.clone()) else {
            return Vec::new();
        };
        let mut out = Vec::new();
        for state in states {
            if let Some(Element::StateExtend { extend, .. }) = self.map.element_at(state) {
                collect_extend_models(&extend, &mut out);
            }
        }
        out
    }

    /// Возвращает элемент карты по имени (состояние, модель либо `StateExtend`).
    pub fn element_of(&self, name: &Name) -> Option<Element> {
        self.map.element_at(name.clone())
    }

    /// Возвращает переменные **корня**, которыми пользуется под-модель.
    ///
    /// Это список для `VAR_IN_OUT` под-FB и, он же, для аргументов вызова -
    /// **один источник истины**: разойдись они, порождённый ST перестал бы
    /// собираться (либо, хуже, связал бы не те переменные).
    ///
    /// В цели `c` этой задачи нет: там под-модель получает указатель `main` и читает
    /// `main->lift_request` (Ф7). В переносимом подмножестве ST указателей нет, поэтому
    /// общие переменные передаются по ссылке через `VAR_IN_OUT` (вариант О1-в; проба П7
    /// подтвердила, что MatIEC это принимает).
    ///
    /// Список считается по фактическому использованию (`compute_usage`), а не "все
    /// переменные корня": лишний `VAR_IN_OUT` - это лишний обязательный аргумент у
    /// каждого вызова.
    pub(crate) fn shared_variables(&self, sub: &Name) -> Vec<(String, TypeNode)> {
        let Some(root) = self.map.model_at(None) else {
            return Vec::new();
        };
        let Some(sub_model) = self.map.model_at(Some(sub.unique().to_string())) else {
            return Vec::new();
        };
        // Общий носитель: он видит и вложенные модели, и те, которыми реализованы
        // состояния.
        let usage = crate::semantic::usage_tree::usage_with_implementations(&sub_model);
        let own: Vec<String> = sub_model.borrow().variables.keys().cloned().collect();
        let root_ref = root.borrow();

        let mut shared: Vec<(String, TypeNode)> = Vec::new();
        let mut names: Vec<&String> = root_ref.variables.keys().collect();
        names.sort();
        for name in names {
            // Константы не разделяются: они неизменны и объявляются в каждом FB своей
            // секцией `VAR CONSTANT`.
            let (VariableNode::Simple { ty, .. } | VariableNode::Port { ty, .. }) =
                &root_ref.variables[name]
            else {
                continue;
            };
            let used = usage.variables.contains(name) || usage.ports.contains(name);
            if used && !own.contains(name) {
                shared.push((name.clone(), ty.clone()));
            }
        }
        shared
    }

    /// Возвращает корневую модель (только для чтения).
    #[allow(dead_code)]
    pub(crate) fn root_model_node(&self) -> Option<Rc<RefCell<ModelNode>>> {
        self.map.model_at(None)
    }

    /// Возвращает модель по имени.
    ///
    /// # Ошибки
    /// [`Diagnostic`] с кодом `ST-012`, если модели с таким именем нет.
    pub(crate) fn raw_model_at(&self, name: Name) -> Result<Rc<RefCell<ModelNode>>, Diagnostic> {
        self.map
            .model_at(Some(name.unique().to_string()))
            .ok_or_else(|| {
                Diagnostic::error(
                    Location::Codegen,
                    msg!(keys::ST_012_MODEL_NOT_FOUND, name = name),
                )
                .with_code("ST-012")
            })
    }

    /// Возвращает ссылку на множество используемых имён модели.
    #[allow(dead_code)]
    pub fn usage(&self) -> &UsageSet {
        &self.usage
    }

    /// Включён ли режим потребления адресов (`st-at`).
    #[allow(dead_code)]
    pub fn at_addresses(&self) -> bool {
        self.at_addresses
    }
}

/// Карта цели `st` - источник состояний для общего носителя строк таблицы.
impl crate::generator::table::StateSource for StMap {
    fn state_element(&self, name: Name) -> Option<Element> {
        self.state_at(name)
    }

    fn state_node(
        &self,
        name: Name,
    ) -> Result<Rc<RefCell<crate::semantic::StateNode>>, Diagnostic> {
        self.raw_state_at(name)
    }
}
