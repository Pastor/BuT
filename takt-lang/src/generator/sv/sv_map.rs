//! Снимок семантической карты модели для генератора SystemVerilog.
//!
//! Обёртка над [`Map`] из [`crate::semantic::minimap`] по образцу `RustMap` и `StMap` -
//! снимок дерева плюс множество используемых имён ([`UsageSet`]).

use crate::diagnostics::{Diagnostic, Location};
use crate::semantic::minimap::{Element, Map, Name};
use crate::semantic::unused::UsageSet;
use crate::semantic::{ModelNode, StateNode};
use std::cell::RefCell;
use std::rc::Rc;

/// Снимок модели, подготовленный для генерации SystemVerilog.
pub(crate) struct SvMap {
    filename: String,
    map: Map,
    /// Множество используемых имён модели (фильтр неиспользуемых элементов).
    usage: UsageSet,
    /// Эмитить ли guard-проверки - флаг `--guard-disable` наоборот.
    guard_enable: bool,
    /// Профиль времени: "часы" (вход `time_ms`) либо "такты" (счётчик).
    time_profile: crate::semantic::duration::TimeProfile,
    /// Форма печати автомата: `unique case` либо таблица переходов.
    fsm: crate::generator::FsmForm,
    /// Комментарии автора модели для переноса в вывод.
    comments: Option<std::rc::Rc<crate::generator::comments::SourceComments>>,
}

impl SvMap {
    /// Строит снимок модели из семантического дерева.
    ///
    /// # Ошибки
    /// [`Diagnostic`], если у модели нет стартового состояния (`SE-011`).
    pub(crate) fn new(
        filename: &str,
        model: &ModelNode,
        guard_enable: bool,
    ) -> Result<Self, Diagnostic> {
        let model_rc = Rc::new(RefCell::new(model.copy(None, None)));
        let usage = crate::semantic::unused::compute_usage(Rc::clone(&model_rc));
        Ok(Self {
            filename: filename.to_string(),
            map: Map::create(model_rc)?,
            usage,
            guard_enable,
            time_profile: crate::semantic::duration::TimeProfile::default(),
            fsm: crate::generator::FsmForm::default(),
            comments: None,
        })
    }

    /// Задаёт профиль времени; умолчание - "часы" (аддитивно). Задаёт форму печати
    /// автомата; умолчание - `unique case`. Комментарии автора модели.
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

    /// Базовое имя выходного файла (без расширения).
    pub(crate) fn get_filename(&self) -> &str {
        &self.filename
    }

    /// Имя корневой модели.
    pub(crate) fn root_name(&self) -> Name {
        self.map.root_name()
    }

    /// Элемент корневой модели (вариант [`Element::Model`]).
    pub(crate) fn model(&self) -> Element {
        self.map.model()
    }

    /// Эмитить ли guard-проверки.
    #[allow(dead_code)]
    pub(crate) fn guard_enable(&self) -> bool {
        self.guard_enable
    }

    /// Ссылка на множество используемых имён.
    #[allow(dead_code)]
    pub(crate) fn usage(&self) -> &UsageSet {
        &self.usage
    }

    /// Подмодели, используемые через `StateExtend`.
    #[allow(dead_code)]
    pub(crate) fn using_models(&self) -> Vec<Element> {
        self.map.used_models()
    }

    /// Элемент карты по имени - **только если он состояние**.
    #[allow(dead_code)]
    pub(crate) fn state_at(&self, name: Name) -> Option<Element> {
        self.map
            .element_at(name)
            .filter(|element| element.is_state())
    }

    /// Элемент карты по имени (состояние, модель либо `StateExtend`).
    #[allow(dead_code)]
    pub(crate) fn element_of(&self, name: &Name) -> Option<Element> {
        self.map.element_at(name.clone())
    }

    /// Элемент **модели** по имени - корневой в том числе.
    ///
    /// Корень в `elements` не лежит: снимок держит его отдельным полем (`Map::model`).
    /// Без этой развилки обход уровней спотыкался бы ровно на корневой модели - то есть
    /// на самой частой.
    pub(crate) fn model_element_of(&self, name: &Name) -> Option<Element> {
        if name.unique() == self.root_name().unique() {
            return Some(self.model());
        }
        self.map.element_at(name.clone())
    }

    /// Модель по имени.
    ///
    /// # Ошибки
    /// [`Diagnostic`] с кодом `SV-010`, если модели с таким именем нет.
    #[allow(dead_code)]
    pub(crate) fn raw_model_at(&self, name: Name) -> Result<Rc<RefCell<ModelNode>>, Diagnostic> {
        self.map
            .model_at(Some(name.unique().to_string()))
            .ok_or_else(|| {
                Diagnostic::error(Location::Codegen, format!("Модель '{}' не найдена", name))
                    .with_code("SV-010")
            })
    }

    /// Корневая модель. Позиция корневой модели в исходнике - для переноса её
    /// комментария.
    pub(crate) fn root_model_loc(&self) -> crate::diagnostics::Location {
        self.root_model_node()
            .map_or(crate::diagnostics::Location::Codegen, |m| m.borrow().loc)
    }

    pub(crate) fn root_model_node(&self) -> Option<Rc<RefCell<ModelNode>>> {
        self.map.model_at(None)
    }

    /// Состояние по уникальному имени.
    ///
    /// # Ошибки
    /// [`Diagnostic`] с кодом `SV-011`, если состояния с таким именем нет.
    #[allow(dead_code)]
    pub(crate) fn raw_state_at(&self, name: Name) -> Result<Rc<RefCell<StateNode>>, Diagnostic> {
        self.map
            .state_at(Some(name.unique().to_string()))
            .ok_or_else(|| {
                Diagnostic::error(
                    crate::generator::site::at(Location::Codegen),
                    format!("Состояние '{}' не найдено", name),
                )
                .with_code("SV-011")
            })
    }
}

/// Карта цели `sv` - источник состояний для общего носителя строк таблицы.
impl crate::generator::table::StateSource for SvMap {
    fn state_element(&self, name: Name) -> Option<Element> {
        self.state_at(name)
    }

    fn state_node(&self, name: Name) -> Result<Rc<RefCell<StateNode>>, Diagnostic> {
        self.raw_state_at(name)
    }
}
