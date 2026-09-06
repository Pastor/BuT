//! Снимок семантической карты модели для генератора Rust.
//!
//! Обёртка над [`Map`] из [`crate::semantic::minimap`] по образцу
//! [`CMap`](crate::generator::c) и `StMap` — снимок дерева плюс множество
//! используемых имён ([`UsageSet`]).
//!
//! Детерминизм (фича 0048) достаётся даром: `Map::elements` — `BTreeMap`, ключ
//! `Name` имеет ручной `Ord` по паре `(unique, local)`. Собственных сортировок
//! здесь заводить не нужно — порядок задан типом контейнера.

use crate::diagnostics::{Diagnostic, Location};
use crate::semantic::duration::TimeProfile;
use crate::semantic::minimap::{Element, Map, Name};
use crate::semantic::unused::UsageSet;
use crate::semantic::{ModelNode, StateNode};
use std::cell::RefCell;
use std::rc::Rc;

/// Снимок модели, подготовленный для генерации Rust.
pub(crate) struct RustMap {
    filename: String,
    map: Map,
    /// Множество используемых имён модели (фильтр неиспользуемых элементов).
    usage: UsageSet,
    /// Эмитить ли guard-проверки (`assert!`) — флаг `--guard-disable` наоборот.
    guard_enable: bool,
    /// Форма печати автомата (фича 0440): `match` либо таблица переходов.
    fsm: crate::generator::FsmForm,
    /// Комментарии автора модели для переноса в вывод (фича 0535, задача 05).
    comments: Option<std::rc::Rc<crate::generator::comments::SourceComments>>,
    /// Профиль времени (фича 0134): «часы» либо «такты». Разрешается общим слоем
    /// (`resolve_profile`) в `generate` и кладётся сюда — по образцу `CMap`.
    time_profile: TimeProfile,
}

impl RustMap {
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
            fsm: crate::generator::FsmForm::default(),
            comments: None,
            time_profile: TimeProfile::default(),
        })
    }

    /// Задаёт профиль времени (фича 0134). Умолчание — «часы»; существующие
    /// вызовы `new` его не повторяют (аддитивно).
    pub(crate) fn with_time_profile(mut self, profile: TimeProfile) -> Self {
        self.time_profile = profile;
        self
    }

    /// Профиль времени, выбранный для генерации.
    pub(crate) fn time_profile(&self) -> TimeProfile {
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
    /// Задаёт форму печати автомата (фича 0440).
    /// Комментарии автора модели (фича 0535, задача 05).
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

    pub(crate) fn guard_enable(&self) -> bool {
        self.guard_enable
    }

    /// Ссылка на множество используемых имён.
    pub(crate) fn usage(&self) -> &UsageSet {
        &self.usage
    }

    /// Подмодели, используемые через `StateExtend`.
    pub(crate) fn using_models(&self) -> Vec<Element> {
        self.map.used_models()
    }

    /// Элемент карты по имени — **только если он состояние**.
    ///
    /// Фильтр `is_state()` — то же, что у `CMap::state_at`: он и делает
    /// «недостижимое состояние → варианта в `enum` нет». Для цели `rust` это не
    /// оптимизация, а условие прохождения гейта: неконструируемый вариант валит
    /// `-D warnings` по `dead_code` (решение R9, задача 0050-02).
    pub(crate) fn state_at(&self, name: Name) -> Option<Element> {
        self.map
            .element_at(name)
            .filter(|element| element.is_state())
    }

    /// Элемент карты по имени (состояние, модель либо `StateExtend`).
    pub(crate) fn element_of(&self, name: &Name) -> Option<Element> {
        self.map.element_at(name.clone())
    }

    /// Модель по имени.
    ///
    /// # Ошибки
    /// [`Diagnostic`] с кодом `RS-012`, если модели с таким именем нет.
    pub(crate) fn raw_model_at(&self, name: Name) -> Result<Rc<RefCell<ModelNode>>, Diagnostic> {
        self.map
            .model_at(Some(name.unique().to_string()))
            .ok_or_else(|| {
                Diagnostic::error(Location::Codegen, format!("Модель '{}' не найдена", name))
                    .with_code("RS-012")
            })
    }

    /// Семантический узел модели по её **уникальному** имени (фича 0185).
    ///
    /// Нужен печати аргументов инстанцирования: тип параметра берётся у целевой
    /// модели, а не угадывается по литералу.
    pub(crate) fn model_node_by_unique(&self, unique: &str) -> Option<Rc<RefCell<ModelNode>>> {
        self.map.model_at(Some(unique.to_string()))
    }

    /// Корневая модель.
    pub(crate) fn root_model_node(&self) -> Option<Rc<RefCell<ModelNode>>> {
        self.map.model_at(None)
    }

    /// Состояние по уникальному имени.
    ///
    /// # Ошибки
    /// [`Diagnostic`] с кодом `RS-013`, если состояния с таким именем нет.
    pub(crate) fn raw_state_at(&self, name: Name) -> Result<Rc<RefCell<StateNode>>, Diagnostic> {
        self.map
            .state_at(Some(name.unique().to_string()))
            .ok_or_else(|| {
                Diagnostic::error(
                    crate::generator::site::at(Location::Codegen),
                    format!("Состояние '{}' не найдено", name),
                )
                .with_code("RS-013")
            })
    }
}

/// Карта цели `rust` — источник состояний для общего носителя строк таблицы
/// (фича 0440).
impl crate::generator::table::StateSource for RustMap {
    fn state_element(&self, name: Name) -> Option<Element> {
        self.state_at(name)
    }

    fn state_node(&self, name: Name) -> Result<Rc<RefCell<StateNode>>, Diagnostic> {
        self.raw_state_at(name)
    }
}
