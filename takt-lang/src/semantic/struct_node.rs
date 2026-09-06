//! Семантический узел структурного типа (`struct`) языка Takt.
//!
//! Модуль реализует [`StructDefinitionNode`] - структуру данных, описывающую
//! именованный структурный тип с полями и их типами.
//!
//! Структуры создаются в ходе семантического анализа при обработке объявлений `struct`
//! в исходном тексте.

use crate::diagnostics::Location;
use crate::semantic::type_node::TypeNode;

/// Семантический узел структурного типа (NI3).
///
/// Описывает именованную структуру: набор именованных полей с типами.
///
/// # Пример Takt
///
/// ```text
/// struct Point { x: [bit;16], y: [bit;16] }
/// var pos: Point = ...;
/// ```
#[derive(Default, Debug, Clone)]
pub struct StructDefinitionNode {
    /// Имя структуры.
    pub name: String,
    /// Поля структуры: `(имя_поля, тип_поля)`.
    pub fields: Vec<(String, TypeNode)>,
    /// Позиция объявления в исходном тексте.
    pub loc: Location,
}

impl PartialEq for StructDefinitionNode {
    fn eq(&self, other: &Self) -> bool {
        // loc игнорируется: не является частью семантической идентичности
        self.name == other.name && self.fields == other.fields
    }
}

impl Eq for StructDefinitionNode {}

impl StructDefinitionNode {
    /// Создаёт новый `StructDefinitionNode` с именем и полями.
    ///
    /// # Пример
    ///
    /// ```
    /// use takt_lang::semantic::StructDefinitionNode;
    /// use takt_lang::semantic::type_node::TypeNode;
    ///
    /// let s = StructDefinitionNode::new("Point", &[
    ///     ("x", TypeNode::Array(16, Box::new(TypeNode::Bit))),
    ///     ("y", TypeNode::Array(16, Box::new(TypeNode::Bit))),
    /// ]);
    /// assert_eq!(s.name, "Point");
    /// assert_eq!(s.fields.len(), 2);
    /// ```
    pub fn new(name: &str, fields: &[(&str, TypeNode)]) -> Self {
        StructDefinitionNode {
            name: name.to_string(),
            fields: fields
                .iter()
                .map(|(n, t)| (n.to_string(), t.clone()))
                .collect(),
            loc: Location::Implicit,
        }
    }

    /// Ищет тип поля по имени.
    ///
    /// Возвращает `Some(&TypeNode)`, если поле найдено.
    ///
    /// # Пример
    ///
    /// ```
    /// use takt_lang::semantic::StructDefinitionNode;
    /// use takt_lang::semantic::type_node::TypeNode;
    ///
    /// let s = StructDefinitionNode::new("Vec2", &[("x", TypeNode::Bit), ("y", TypeNode::Bit)]);
    /// assert_eq!(s.find_field("x"), Some(&TypeNode::Bit));
    /// assert!(s.find_field("z").is_none());
    /// ```
    pub fn find_field(&self, field_name: &str) -> Option<&TypeNode> {
        self.fields
            .iter()
            .find(|(name, _)| name == field_name)
            .map(|(_, ty)| ty)
    }

    /// Возвращает `true`, если поле с данным именем существует.
    pub fn has_field(&self, field_name: &str) -> bool {
        self.find_field(field_name).is_some()
    }
}
