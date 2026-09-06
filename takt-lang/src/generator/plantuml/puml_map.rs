//! Снимок семантической карты модели для генератора PlantUML.
//!
//! Обёртка над [`Map`] из [`crate::semantic::minimap`], предоставляющая только те
//! методы, которые нужны для построения диаграммы состояний.

use crate::diagnostics::Diagnostic;
use crate::semantic::ModelNode;
use crate::semantic::minimap::{Element, Map, Name};
use std::cell::RefCell;
use std::rc::Rc;

/// Снимок модели, подготовленный для генерации PlantUML-диаграммы.
pub(crate) struct PumlMap {
    filename: String,
    map: Map,
}

impl PumlMap {
    /// Строит снимок модели из семантического дерева.
    ///
    /// # Ошибки
    /// Возвращает [`Diagnostic`], если у модели нет стартового состояния.
    pub fn new(filename: &str, model: &ModelNode) -> Result<Self, Diagnostic> {
        let model_rc = Rc::new(RefCell::new(model.copy(None, None)));
        Ok(Self {
            filename: filename.to_string(),
            map: Map::create(model_rc)?,
        })
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
    pub fn states(&self) -> Vec<Name> {
        self.map.states()
    }

    /// Возвращает элемент состояния по имени, или `None` если не найден / не является
    /// состоянием.
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
}
