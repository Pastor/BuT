//! Семантические узлы языка Takt.
//!
//! [`ModelNode`] - модель со словарями состояний, переменных, условий, функций и
//! моделей; [`StateNode`] - состояние, [`ReferenceNode`] - переход, [`ConditionNode`] -
//! условие, [`VariableNode`] - переменная, [`Extend`] - реализация.

pub mod bounds_guard;
mod condition_node;
pub mod default_value;
mod expression_node;
pub mod time_ast;
pub use {condition_node::ConditionNode, expression_node::ExpressionNode};
pub mod anon_collect;
pub mod anon_port;
pub use anon_collect::{
    collect_anon_ports, collect_anon_ports_local, collect_anon_ports_local_node,
};
pub use anon_port::AnonPortAccess;
pub mod bit_vector;
mod builtin;
pub(crate) mod callgraph;
pub mod condition;
pub mod const_eval;
mod declaration;
pub(crate) mod docs;
pub mod duration;
pub mod enum_node;
mod expression;
pub mod extend;
mod extend_args;
pub mod formula;
pub(crate) mod fresh;
mod function;
pub(crate) mod import; // правила поиска файла нужны и области LSP
pub mod index;
pub mod inline;
pub(crate) mod internal;
pub mod loop_bounds;
pub mod lower_float;
pub mod ltl_check;
/// Снимок достижимых состояний и моделей - плоская карта [`Map`](minimap::Map).
pub mod minimap;
mod named_block;
pub(crate) mod named_blocks;
mod named_code_block;
pub mod terminal;
pub use match_node::{MatchArmNode, MatchPatternNode};
pub use {named_code_block::NamedCodeBlockDefinitionNode, reference::ReferenceNode};
pub(crate) mod match_arms;
mod match_node;
pub(crate) mod naming;
pub(crate) mod parameter_const;
mod reference;
pub(crate) mod scalar_port;
pub(crate) mod slice;
pub(crate) mod specialize;
pub(crate) mod stages;
// Позиция оператора и равенство без неё.
mod statement;
mod statement_loc;
pub mod struct_node;
pub(crate) mod target_block;
pub(crate) mod test_constants;
pub mod tree;
pub(crate) mod type_inference;
pub mod type_node;
pub(crate) mod type_registry;
pub mod unused;
pub mod usage_tree;
pub mod usages;
pub(crate) mod validate;
mod variable;
pub(crate) mod walk;
pub use variable::{AddressBindingNode, ParameterNode, VariableNode};
pub mod warnings;

use crate::diagnostics::Location;
use crate::parser::ast;
pub use crate::parser::ast::PortDirection;
use crate::parser::ast::{Member, NamedArgument, ParameterList, Type};
pub use crate::semantic::enum_node::{EnumDefinitionNode, EnumFacts, enum_default, enum_facts};
pub use crate::semantic::formula::Formula;
pub use crate::semantic::struct_node::StructDefinitionNode;
use extend::Extend;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::fmt::Debug;
use std::rc::{Rc, Weak};
use type_node::TypeNode;

/// Семантический узел модели (конечного автомата): контекст, имя, словарь состояний и
/// реализация (`implements`).
///
/// Поля [`doc`](ModelNode::doc) и [`docs`](ModelNode::docs) заполняет
/// [`construct_model_with_docs`](tree::construct_model_with_docs) - строками `///`.
#[derive(Default, Debug)]
pub struct ModelNode {
    /// Имя модели (`None` для анонимной корневой модели).
    pub name: Option<String>,
    /// Позиция объявления модели в исходном тексте.
    pub loc: Location,
    /// Модель уровнем выше (слабая ссылка для предотвращения циклов Rc).
    pub upper: Option<Weak<RefCell<ModelNode>>>,
    /// Вложенные именованные модели.
    pub models: BTreeMap<String, Rc<RefCell<ModelNode>>>,
    /// Именованные блоки кода (`enter`, `exit`, `always`, ...).
    pub named_blocks: Vec<NamedCodeBlockDefinitionNode>,
    /// Объявленные функции.
    pub functions: BTreeMap<String, FunctionDefinitionNode>,
    /// Объявленные переменные.
    pub variables: BTreeMap<String, VariableNode>,
    /// Параметры модели в **порядке объявления**. Само значение параметра живёт в
    /// [`ModelNode::variables`] обычным [`VariableNode::Simple`]: в режиме генерации по
    /// умолчанию (`--parameters=assign`) параметр **и есть** поле экземпляра, и всякий
    /// потребитель дерева, ничего не знающий о параметрах, обращается с ним верно. Здесь -
    /// только то, что от переменной его отличает: **имя, позиция объявления и порядок**.
    /// Порядок значим: по нему строится ключ дедупликации специализаций
    /// (`--parameters=specialize`,, п.
    pub parameters: Vec<ParameterNode>,
    /// Объявленные псевдонимы типов.
    pub types: BTreeMap<String, TypeNode>,
    /// Позиции объявлений псевдонимов типов: имя -> позиция в исходном тексте.
    pub type_locs: BTreeMap<String, Location>,
    /// Сырые АСД-типы псевдонимов: имя -> оригинальный AST-тип до разрешения.
    ///
    /// Используется `check_recursive_type_aliases` в `validate.rs` для обнаружения
    /// циклических ссылок между псевдонимами (Ce16).
    pub raw_type_defs: BTreeMap<String, ast::Type>,
    /// Сырые АСД-операторы именованных блоков: `(имя_блока, оригинальный_оператор)`.
    ///
    /// Используется `SemanticIndex` (I8) для индексации объявлений локальных переменных
    /// внутри `enter`/`exit`/`always`-блоков, позиции которых теряются при разрешении.
    pub named_block_raw: Vec<(String, ast::Statement)>,
    /// Объявленные условия переходов.
    pub conditions: BTreeMap<String, ConditionDefinitionNode>,
    /// Объявленные перечисления (Ce4).
    pub enums: BTreeMap<String, EnumDefinitionNode>,
    /// Объявленные структурные типы (NI3).
    pub structs: BTreeMap<String, StructDefinitionNode>,
    /// Состояния модели: имя -> узел состояния.
    pub states: BTreeMap<String, StateNode>,
    /// Информация о реализации (зарезервировано).
    pub implements: Extend,
    /// Документация самой модели (строки из `///`-комментариев перед `model`).
    ///
    /// Заполняется [`construct_model_with_docs`](tree::construct_model_with_docs).
    /// Пусто у анонимной корневой модели и при использовании
    /// [`construct_model`](tree::construct_model).
    pub doc: Vec<String>,
    /// Документация именованных элементов модели. Ключ - имя элемента (переменной,
    /// состояния, функции, типа, условия). Значение - список строк из `///`-комментариев,
    /// предшествующих объявлению.
    pub docs: BTreeMap<String, Vec<String>>,
    /// Встроенные формулы модели.
    pub formulas: Vec<Formula>,
    /// Привязки адресов портов оператором `address`.
    ///
    /// Каждый элемент - один оператор `address Имя = <выражение>;`. Разрешение
    /// (привязка к порту, приоритет источников inline/`address`/внешняя карта) и
    /// диагностики выполняет [`check_port_addresses`](validate::check_port_addresses).
    pub address_defs: Vec<AddressBindingNode>,
    /// Происхождение модели: объявлена здесь или пришла через `import`.
    pub origin: ModelOrigin,
    /// Частота тактирования, объявленная `clock 1kHz;`, в герцах.
    ///
    /// `None` - не объявлена, действует профиль "часы". Приоритет с флагом
    /// `--tick-hz` разрешает [`duration::resolve_profile`](duration::resolve_profile) -
    /// **одной** функцией, чтобы источники не перекрывали друг друга по-разному
    /// в разных целях.
    pub clock_hz: Option<u64>,
}

/// Происхождение модели в дереве.
///
/// Признак **относителен файлу, в дерево которого узел вставлен**, а не абсолютное
/// свойство модели: вложенная модель `lib.takt`, взятая формой `import { A as B } from
/// "lib.takt";`, для `lib.takt` локальна, а для `main.takt` импортирована.
///
/// Заведён потому, что иначе импорт **неотличим** от локальной вложенной модели: проход
/// 0 кладёт их в один и тот же [`ModelNode::models`], а `Location` несёт `file_no`,
/// который везде равен нулю.
///
/// Потребитель - область проверки [`verify_all`](crate::verify_all): встретив
/// `Imported`, обход отсекает поддерево **целиком**. Проверять `origin` у каждого узла
/// по отдельности недостаточно - вложенные модели импортированного файла локальны для
/// него и несут `Local`.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub enum ModelOrigin {
    /// Модель объявлена в том же файле, что и её родитель.
    #[default]
    Local,
    /// Модель пришла через `import` (любая из трёх форм).
    Imported,
}

impl ModelNode {
    pub(crate) fn name(&self) -> &str {
        if self.name.is_none() {
            return "";
        }
        self.name.as_ref().unwrap().as_str()
    }
}

impl ModelNode {
    pub(crate) fn get_start_state(&self) -> Option<StateNode> {
        self.states
            .clone()
            .into_values()
            .filter(|state| state.kind() == StateNodeKind::Start)
            .collect::<Vec<StateNode>>()
            .first()
            .cloned()
    }

    /// Возвращает все конечные состояния модели.
    #[allow(dead_code)]
    pub(crate) fn get_end_states(&self) -> Vec<StateNode> {
        self.states
            .clone()
            .into_values()
            .filter(|state| state.kind() == StateNodeKind::End)
            .collect::<Vec<StateNode>>()
    }

    pub(crate) fn copy(
        &self,
        new_name: Option<String>,
        upper: Option<Rc<RefCell<ModelNode>>>,
    ) -> ModelNode {
        let name = if new_name.is_none() {
            self.name.clone()
        } else {
            new_name.clone()
        };
        ModelNode {
            name,
            loc: self.loc,
            upper: upper.map(|p| Rc::downgrade(&p)),
            models: self.models.clone(),
            named_blocks: self.named_blocks.clone(),
            functions: self.functions.clone(),
            variables: self.variables.clone(),
            // Параметры - свойство модели, а не имени, под которым она видна (тот же
            // довод, что у `origin` и `clock_hz` ниже).
            parameters: self.parameters.clone(),
            types: self.types.clone(),
            type_locs: self.type_locs.clone(),
            raw_type_defs: self.raw_type_defs.clone(),
            named_block_raw: self.named_block_raw.clone(),
            conditions: self.conditions.clone(),
            enums: self.enums.clone(),
            structs: self.structs.clone(),
            states: self.states.clone(),
            implements: self.implements.clone(),
            doc: self.doc.clone(),
            docs: self.docs.clone(),
            formulas: self.formulas.clone(),
            address_defs: self.address_defs.clone(),
            // Копия наследует происхождение: переименование модели её источник не
            // меняет.
            origin: self.origin,
            // ...и частоту тактирования: она свойство модели, а не имени, под которым
            // модель видна импортёру.
            clock_hz: self.clock_hz,
        }
    }
}

impl PartialEq for ModelNode {
    fn eq(&self, other: &Self) -> bool {
        // upper, loc, type_locs игнорируются: не являются частью идентичности модели
        self.name == other.name
            && self.models == other.models
            && self.named_blocks == other.named_blocks
            && self.functions == other.functions
            && self.variables == other.variables
            && self.types == other.types
            && self.conditions == other.conditions
            && self.enums == other.enums
            && self.states == other.states
            && self.implements == other.implements
            && self.formulas == other.formulas
    }
}

impl Eq for ModelNode {}

impl ModelNode {
    /// Возвращает `true`, если модель содержит хотя бы одно состояние.
    ///
    /// # Примеры
    ///
    /// ```
    /// use takt_lang::parse;
    /// use takt_lang::semantic::tree::construct_model;
    ///
    /// // Модель без состояний
    /// let (ast, _) = parse("type Byte = [bit;8];", 0).unwrap();
    /// let node = construct_model(&ast, None, &[]).unwrap();
    /// assert!(!node.borrow().has_states());
    ///
    /// // Модель с состоянием
    /// let (ast, _) = parse("start S;", 0).unwrap();
    /// let node = construct_model(&ast, None, &[]).unwrap();
    /// assert!(node.borrow().has_states());
    /// ```
    pub fn has_states(&self) -> bool {
        !self.states.is_empty()
    }

    /// Ищет модель по имени в текущем контексте и во всех родительских.
    ///
    /// Обходит цепочку `upper`-ссылок вверх до тех пор, пока модель не найдена или
    /// цепочка не исчерпана.
    ///
    /// # Примеры
    ///
    /// ```
    /// use takt_lang::parse;
    /// use takt_lang::semantic::tree::construct_model;
    ///
    /// let (ast, _) = parse("model Inner { start S; }", 0).unwrap();
    /// let root = construct_model(&ast, None, &[]).unwrap();
    /// // Вложенная модель "Inner" доступна из корня
    /// assert!(root.borrow().search_model("Inner").is_some());
    /// // Несуществующая модель возвращает None
    /// assert!(root.borrow().search_model("Ghost").is_none());
    /// ```
    pub fn search_model(&self, name: &str) -> Option<Rc<RefCell<ModelNode>>> {
        if let Some(model) = self.models.get(name) {
            Some(Rc::clone(model))
        } else if let Some(model) = self.upper.as_ref().and_then(|w| w.upgrade()) {
            return model.borrow().search_model(name);
        } else {
            None
        }
    }

    /// Ищет переменную по имени в текущем контексте и во всех родительских.
    ///
    /// Аналогично [`search_model`](ModelNode::search_model), обходит цепочку
    /// `upper`-ссылок вверх.
    ///
    /// # Примеры
    ///
    /// ```
    /// use takt_lang::parse;
    /// use takt_lang::semantic::tree::construct_model;
    ///
    /// let (ast, _) = parse("var x: bit := false;", 0).unwrap();
    /// let root = construct_model(&ast, None, &[]).unwrap();
    /// assert!(root.borrow().search_var("x").is_some());
    /// assert!(root.borrow().search_var("y").is_none());
    /// ```
    pub fn search_var(&self, name: &str) -> Option<VariableNode> {
        if let Some(var) = self.variables.get(name) {
            Some(var.clone())
        } else if let Some(model) = self.upper.as_ref().and_then(|w| w.upgrade()) {
            return model.borrow().search_var(name);
        } else {
            None
        }
    }

    /// Ищет именованное условие по `name`, обходя цепочку `upper`.
    pub fn search_cond(&self, name: &str) -> Option<ConditionDefinitionNode> {
        if let Some(cond) = self.conditions.get(name) {
            Some(cond.clone())
        } else if let Some(model) = self.upper.as_ref().and_then(|w| w.upgrade()) {
            return model.borrow().search_cond(name);
        } else {
            None
        }
    }

    /// Ищет объявление перечисления по `name`, обходя цепочку `upper` (Ce4).
    ///
    /// Возвращает клон [`EnumDefinitionNode`], если перечисление найдено в текущем или
    /// родительском контексте.
    ///
    /// # Пример
    ///
    /// ```
    /// use takt_lang::semantic::{ModelNode, EnumDefinitionNode};
    ///
    /// let mut model = ModelNode::default();
    /// let e = EnumDefinitionNode::new("Color", &[("Red", None), ("Green", None)]);
    /// model.enums.insert("Color".to_string(), e);
    /// assert!(model.search_enum("Color").is_some());
    /// assert!(model.search_enum("Size").is_none());
    /// ```
    pub fn search_enum(&self, name: &str) -> Option<EnumDefinitionNode> {
        if let Some(e) = self.enums.get(name) {
            Some(e.clone())
        } else if let Some(model) = self.upper.as_ref().and_then(|w| w.upgrade()) {
            return model.borrow().search_enum(name);
        } else {
            None
        }
    }

    /// Ищет вариант перечисления по имени варианта среди всех доступных перечислений
    /// (NI6).
    ///
    /// Обходит все перечисления текущего контекста и родительских. Возвращает
    /// `(имя_перечисления, числовое_значение)` при нахождении.
    ///
    /// # Пример
    ///
    /// ```
    /// use takt_lang::semantic::{ModelNode, EnumDefinitionNode};
    ///
    /// let mut model = ModelNode::default();
    /// let e = EnumDefinitionNode::new("Direction", &[("North", None), ("South", Some(180))]);
    /// model.enums.insert("Direction".to_string(), e);
    /// let result = model.search_enum_variant("North");
    /// assert!(result.is_some());
    /// let (enum_node, value) = result.unwrap();
    /// assert_eq!(enum_node.name, "Direction");
    /// assert_eq!(value, 0);
    /// assert_eq!(model.search_enum_variant("East"), None);
    /// ```
    pub fn search_enum_variant(&self, variant_name: &str) -> Option<(EnumDefinitionNode, i128)> {
        for enum_node in self.enums.values() {
            if let Some(val) = enum_node.find_variant(variant_name) {
                return Some((enum_node.clone(), val));
            }
        }
        if let Some(model) = self.upper.as_ref().and_then(|w| w.upgrade()) {
            return model.borrow().search_enum_variant(variant_name);
        }
        None
    }

    /// Ищет структурный тип по имени, включая родительские модели (NI3).
    pub fn search_struct(&self, name: &str) -> Option<StructDefinitionNode> {
        if let Some(s) = self.structs.get(name) {
            Some(s.clone())
        } else if let Some(model) = self.upper.as_ref().and_then(|w| w.upgrade()) {
            model.borrow().search_struct(name)
        } else {
            None
        }
    }

    /// Возвращает список всех именованных блоков с заданным именем.
    pub fn get_named_blocks(&self, name: &str) -> Vec<&NamedCodeBlockDefinitionNode> {
        self.named_blocks
            .iter()
            .filter(|b| b.name() == name)
            .collect()
    }

    /// Возвращает первый именованный блок с заданным именем, если он есть.
    pub fn get_named_block(&self, name: &str) -> Option<&NamedCodeBlockDefinitionNode> {
        self.named_blocks.iter().find(|b| b.name() == name)
    }

    /// Ищет объявление функции по `name`, обходя цепочку `upper`.
    pub fn search_func(&self, name: &str) -> Option<Rc<RefCell<FunctionDefinitionNode>>> {
        if let Some(func) = self.functions.get(name) {
            Some(Rc::new(RefCell::new(func.clone())))
        } else if let Some(model) = self.upper.as_ref().and_then(|w| w.upgrade()) {
            return model.borrow().search_func(name);
        } else {
            None
        }
    }

    /// Ищет состояние по имени в текущей модели, затем рекурсивно в родительской.
    pub fn search_state(&self, name: &str) -> Option<Rc<RefCell<StateNode>>> {
        if let Some(state) = self.states.get(name) {
            Some(Rc::new(RefCell::new(state.clone())))
        } else if let Some(model) = self.upper.as_ref().and_then(|w| w.upgrade()) {
            return model.borrow().search_state(name);
        } else {
            None
        }
    }

    /// Ищет псевдоним типа по имени, поднимаясь по цепочке родительских моделей.
    pub fn search_type(&self, name: &str) -> Option<Rc<RefCell<TypeNode>>> {
        if let Some(type_) = self.types.get(name) {
            Some(Rc::new(RefCell::new(type_.clone())))
        } else if let Some(model) = self.upper.as_ref().and_then(|w| w.upgrade()) {
            return model.borrow().search_type(name);
        } else {
            None
        }
    }

    /// Возвращает документацию самой модели.
    ///
    /// Заполняется только при использовании
    /// [`construct_model_with_docs`](tree::construct_model_with_docs). Возвращает
    /// пустой срез для анонимной корневой модели или при использовании
    /// [`construct_model`](tree::construct_model).
    ///
    /// # Примеры
    ///
    /// ```
    /// # use takt_lang::parse;
    /// # use takt_lang::semantic::tree::construct_model_with_docs;
    /// let (ast, comments) = parse("/// Тест\nmodel M { start S; }", 0).unwrap();
    /// let root = construct_model_with_docs(&ast, None, &[], &comments).unwrap();
    /// let m = root.borrow().search_model("M").unwrap();
    /// assert_eq!(m.borrow().own_doc(), ["Тест"]);
    /// ```
    pub fn own_doc(&self) -> &[String] {
        &self.doc
    }

    /// Возвращает документацию именованного элемента (состояния, переменной, функции,
    /// типа, условия).
    ///
    /// Заполняется только при использовании
    /// [`construct_model_with_docs`](tree::construct_model_with_docs). Возвращает
    /// пустой срез, если документация отсутствует или не загружена.
    ///
    /// # Примеры
    ///
    /// ```
    /// # use takt_lang::parse;
    /// # use takt_lang::semantic::tree::construct_model_with_docs;
    /// let (ast, comments) = parse("/// Состояние.\nstart S;", 0).unwrap();
    /// let root = construct_model_with_docs(&ast, None, &[], &comments).unwrap();
    /// assert_eq!(root.borrow().element_doc("S"), ["Состояние."]);
    /// ```
    pub fn element_doc(&self, name: &str) -> &[String] {
        self.docs.get(name).map(|v| v.as_slice()).unwrap_or(&[])
    }
}

/// Семантическое определение функции.
#[derive(Default, Debug, Clone)]
pub enum FunctionDefinitionNode {
    /// Определение отсутствует.
    #[default]
    None,
    /// Неразрешённое AST-определение.
    Unresolved(ast::FunctionDefine),
    /// Локальная функция с телом.
    Local {
        /// Родительская модель (слабая ссылка для предотвращения циклов Rc).
        upper: Option<Weak<RefCell<ModelNode>>>,
        /// Позиция объявления функции в исходном тексте.
        loc: Location,
        /// Имя функции.
        name: String,
        /// Список параметров: `(имя, тип)`.
        params: Vec<(String, TypeNode)>,
        /// Возвращаемый тип.
        ret: TypeNode,
        /// Тело функции.
        body: StatementNode,
        /// Исходное АСД-определение.
        ///
        /// Константный вычислитель (`semantic::const_eval`) интерпретирует
        /// **АСД** тела: разрешённый [`StatementNode`] потребовал бы второго
        /// интерпретатора рядом с первым. Хранится при функции, а не отдельной
        /// картой в модели: иначе поиск определения разошёлся бы с
        /// [`search_func`](ModelNode::search_func).
        raw: Box<ast::FunctionDefine>,
    },
    /// Внешняя функция (без тела).
    External {
        /// Родительская модель (слабая ссылка для предотвращения циклов Rc).
        upper: Option<Weak<RefCell<ModelNode>>>,
        /// Позиция объявления функции в исходном тексте.
        loc: Location,
        /// Имя функции.
        name: String,
        /// Список параметров: `(имя, тип)`.
        params: Vec<(String, TypeNode)>,
        /// Возвращаемый тип.
        ret: TypeNode,
    },
    /// Встроенная функция языка.
    Builtin(&'static str, &'static [(&'static str, TypeNode)], TypeNode),
}

impl PartialEq for FunctionDefinitionNode {
    fn eq(&self, other: &Self) -> bool {
        // upper и loc игнорируются: не являются частью семантической идентичности
        // функции
        match (self, other) {
            (Self::None, Self::None) => true,
            (Self::Unresolved(a), Self::Unresolved(b)) => a == b,
            (
                Self::Local {
                    name: n1,
                    params: p1,
                    ret: r1,
                    body: b1,
                    ..
                },
                Self::Local {
                    name: n2,
                    params: p2,
                    ret: r2,
                    body: b2,
                    ..
                },
            ) => n1 == n2 && p1 == p2 && r1 == r2 && b1 == b2,
            (
                Self::External {
                    name: n1,
                    params: p1,
                    ret: r1,
                    ..
                },
                Self::External {
                    name: n2,
                    params: p2,
                    ret: r2,
                    ..
                },
            ) => n1 == n2 && p1 == p2 && r1 == r2,
            (Self::Builtin(n1, p1, r1), Self::Builtin(n2, p2, r2)) => {
                n1 == n2 && p1 == p2 && r1 == r2
            }
            _ => false,
        }
    }
}

impl Eq for FunctionDefinitionNode {}

impl FunctionDefinitionNode {
    /// Возвращает позицию объявления функции в исходном тексте.
    ///
    /// Для [`None`](FunctionDefinitionNode::None),
    /// [`Unresolved`](FunctionDefinitionNode::Unresolved) и
    /// [`Builtin`](FunctionDefinitionNode::Builtin) возвращает [`Location::Implicit`].
    pub fn loc(&self) -> Location {
        match self {
            FunctionDefinitionNode::Local { loc, .. }
            | FunctionDefinitionNode::External { loc, .. } => *loc,
            _ => Location::Implicit,
        }
    }

    /// Возвращает имя функции (пустая строка для `None` и `Unresolved`).
    pub fn name(&self) -> &str {
        match self {
            FunctionDefinitionNode::Local { name, .. }
            | FunctionDefinitionNode::External { name, .. } => name,
            FunctionDefinitionNode::Builtin(name, ..) => name,
            _ => "",
        }
    }
}

/// Семантический узел вызова или ссылки на функцию.
#[derive(Default, Debug, PartialEq, Eq, Clone)]
pub enum FunctionNode {
    /// Функция не задана.
    #[default]
    None,
    /// Неразрешённый вызов: `(имя, аргументы)`.
    Unresolved(String, Vec<ExpressionNode>),
    /// Вызов локальной функции.
    Local(Rc<RefCell<FunctionDefinitionNode>>, Vec<ExpressionNode>),
    /// Вызов внешней функции.
    External(Rc<RefCell<FunctionDefinitionNode>>, Vec<ExpressionNode>),
}

/// Семантический оператор языка Takt.
///
/// После семантического анализа (этап 4) все варианты `Unresolved` заменяются
/// конкретными разрешёнными вариантами.
#[derive(Default, Debug, Clone)]
pub enum StatementNode {
    /// Оператор отсутствует (умолчание).
    #[default]
    None,
    /// "Сырой" АСД-оператор, ещё не прошедший понижение.
    Unresolved(ast::Statement),
    /// Блок операторов `{ ... }`.
    Block(Vec<StatementNode>),
    /// Оператор-выражение и его позиция (0264; у выражения своей нет).
    Expression(Box<ExpressionNode>, crate::diagnostics::Location),
    /// Условный оператор `if`.
    If {
        /// Условие.
        cond: Box<ExpressionNode>,
        /// Тело.
        then_: Box<StatementNode>,
        /// Ветка `else` (если задана).
        else_: Option<Box<StatementNode>>,
        /// Позиция заголовка `if` в исходнике.
        loc: Location,
    },
    /// Цикл `loop [условие]`.
    Loop {
        /// Условие продолжения (`None` - бесконечный цикл).
        cond: Option<Box<ExpressionNode>>,
        /// Тело цикла.
        body: Box<StatementNode>,
        /// Позиция заголовка цикла в исходнике.
        loc: Location,
    },
    /// Цикл `for`; поле `loc` - позиция заголовка.
    For {
        /// Инициализация (опционально).
        init: Option<Box<StatementNode>>,
        /// Условие продолжения (опционально).
        cond: Option<Box<ExpressionNode>>,
        /// Выражение шага (опционально).
        step: Option<Box<ExpressionNode>>,
        /// Тело цикла.
        body: Box<StatementNode>,
        /// Позиция заголовка.
        loc: Location,
    },
    /// Объявление: `(имя, тип, инициализатор?, позиция)` - позиция с 0386.
    Variable(String, TypeNode, Option<Box<ExpressionNode>>, Location),
    /// Оператор `return [выражение]` и его позиция.
    Return(Option<Box<ExpressionNode>>, Location),
    /// Оператор `continue` и его позиция.
    Continue(Location),
    /// Оператор `break` и его позиция.
    Break(Location),
    /// Встроенная формула `: условие1[, условие2, ...];`
    InlineFormula(Vec<Formula>),
    /// Блок формул внешнего анализатора `formula [диалект] { ... }`: цели и эталон его
    /// пропускают. Не `Unresolved` - тот означает дефект.
    Formula(Box<crate::parser::ast::FormulaBlock>),
    /// Вставка операторов для одной цели: `assembly ["цель"] { ... }`.
    Assembly {
        /// Язык вывода, чья печать включает тело; `None` - все цели и эталон.
        target: Option<String>,
        /// Тело вставки - обычные операторы Takt.
        body: Box<StatementNode>,
        /// Позиция заголовка вставки в исходнике.
        loc: Location,
    },
    /// Оператор `match`: `match expr { patterns => body, ... }`.
    Match {
        /// Разбираемое выражение.
        expr: Box<ExpressionNode>,
        /// Ветки оператора.
        arms: Vec<MatchArmNode>,
        /// Позиция заголовка `match` в исходнике.
        loc: Location,
    },
}

// --- Ce4: Перечисления --------------------------------------------------------
/// Семантический узел именованного условия.
///
/// Хранит имя условия и его разрешённое значение. Заполняется в ходе третьего прохода
/// построения модели ([`extract_conditions`]).
///
/// [`extract_conditions`]: crate::semantic::condition::extract_conditions
#[derive(Default, Debug, Clone)]
pub struct ConditionDefinitionNode {
    /// Имя условия, как объявлено в источнике (`cond имя = ...`).
    pub name: String,
    /// Позиция объявления условия в исходном тексте.
    pub loc: Location,
    /// Разрешённое значение условия.
    pub value: ConditionNode,
    /// Родительская модель (слабая ссылка для предотвращения циклов Rc).
    pub upper: Option<Weak<RefCell<ModelNode>>>,
}

impl PartialEq for ConditionDefinitionNode {
    fn eq(&self, other: &Self) -> bool {
        // upper и loc игнорируются: не являются частью идентичности узла
        self.name == other.name && self.value == other.value
    }
}

impl Eq for ConditionDefinitionNode {}

impl ConditionDefinitionNode {
    /// Возвращает имя условия.
    pub fn name(&self) -> &str {
        &self.name
    }
}

/// Состояние конечного автомата.
///
/// Три варианта:
/// - [`Unresolved`](StateNode::Unresolved) - заглушка на время первого прохода построения.
/// - [`Simple`](StateNode::Simple) - обычное состояние без реализации.
/// - [`Implement`](StateNode::Implement) - состояние с реализацией (`= Модель`),
///   может иметь оператор `next`.
#[derive(Default, Debug, Clone)]
// Вариант Implement крупнее Simple из-за поля `implements`, которое содержит векторы.
// Боксирование поля `next` допустимо, но требует масштабного рефакторинга -
// откладываем.
#[allow(clippy::large_enum_variant)]
pub enum StateNode {
    /// Состояние не разрешено (временная заглушка при построении дерева).
    #[default]
    Unresolved,
    /// Обычное состояние: контекст, имя и список ссылок на переходы.
    Simple {
        /// Родительская модель (слабая ссылка для предотвращения циклов Rc).
        upper: Option<Weak<RefCell<ModelNode>>>,
        /// Позиция объявления состояния в исходном тексте.
        loc: Location,
        /// Именованные блоки кода (`enter`, `exit`, `always`, ...).
        named_blocks: Vec<NamedCodeBlockDefinitionNode>,
        /// Имя состояния.
        name: String,
        /// Ссылки-переходы (`ref Имя [: Условие]`).
        references: Vec<ReferenceNode<StateNode>>,
        /// Разновидность состояния (обычное, начальное, конечное).
        kind: StateNodeKind,
        /// Встроенные формулы состояния.
        formulas: Vec<Formula>,
    },
    /// Состояние с реализацией (`= Модель`): может иметь `next`-переход.
    Implement {
        /// Родительская модель (слабая ссылка для предотвращения циклов Rc).
        upper: Option<Weak<RefCell<ModelNode>>>,
        /// Позиция объявления состояния в исходном тексте.
        loc: Location,
        /// Именованные блоки кода (`enter`, `exit`, `always`, ...).
        named_blocks: Vec<NamedCodeBlockDefinitionNode>,
        /// Имя состояния.
        name: String,
        /// Ссылки-переходы.
        references: Vec<ReferenceNode<StateNode>>,
        /// Информация о реализации (зарезервировано).
        implements: Extend,
        /// Единственный `next`-переход (если задан).
        next: Option<ReferenceNode<StateNode>>,
        /// Разновидность состояния (обычное, начальное, конечное).
        kind: StateNodeKind,
        /// Встроенные формулы состояния.
        formulas: Vec<Formula>,
    },
}

impl StateNode {
    pub(crate) fn kind(&self) -> StateNodeKind {
        match self {
            StateNode::Unresolved => StateNodeKind::Simple,
            StateNode::Simple { kind, .. } | StateNode::Implement { kind, .. } => *kind,
        }
    }

    /// Завершает ли состояние автомат (правило - `semantic::terminal`).
    ///
    /// Тело считается наравне с рёбрами: состояние с `always` работает вечно, и уводить
    /// из него автомат нельзя - автор написал "всегда", а не "однажды".
    pub(crate) fn is_terminated(&self) -> bool {
        crate::semantic::terminal::node_is_terminal(self)
    }

    /// Возвращает срез формул, связанных с состоянием.
    pub fn formulas(&self) -> &[Formula] {
        match self {
            StateNode::Simple { formulas, .. } | StateNode::Implement { formulas, .. } => formulas,
            _ => &[],
        }
    }
}

impl PartialEq for StateNode {
    fn eq(&self, other: &Self) -> bool {
        // upper и loc игнорируются: не являются частью семантической идентичности
        // состояния
        match (self, other) {
            (StateNode::Unresolved, StateNode::Unresolved) => true,
            (
                StateNode::Simple {
                    name: n1,
                    named_blocks: nb1,
                    references: r1,
                    kind: k1,
                    formulas: f1,
                    ..
                },
                StateNode::Simple {
                    name: n2,
                    named_blocks: nb2,
                    references: r2,
                    kind: k2,
                    formulas: f2,
                    ..
                },
            ) => n1 == n2 && nb1 == nb2 && r1 == r2 && k1 == k2 && f1 == f2,
            (
                StateNode::Implement {
                    name: n1,
                    named_blocks: nb1,
                    references: r1,
                    implements: i1,
                    next: nx1,
                    kind: k1,
                    formulas: f1,
                    ..
                },
                StateNode::Implement {
                    name: n2,
                    named_blocks: nb2,
                    references: r2,
                    implements: i2,
                    next: nx2,
                    kind: k2,
                    formulas: f2,
                    ..
                },
            ) => {
                n1 == n2 && nb1 == nb2 && r1 == r2 && i1 == i2 && nx1 == nx2 && k1 == k2 && f1 == f2
            }
            _ => false,
        }
    }
}

impl Eq for StateNode {}

/// Разновидность состояния FSM.
#[derive(Default, Debug, PartialEq, Eq, Clone, Copy)]
pub enum StateNodeKind {
    /// Обычное состояние.
    #[default]
    Simple,
    /// Начальное состояние (`start`).
    Start,
    /// Конечное состояние (`end`).
    End,
}

impl StateNode {
    /// Возвращает позицию объявления состояния в исходном тексте.
    ///
    /// Для [`Unresolved`](StateNode::Unresolved) возвращает [`Location::Implicit`].
    pub fn loc(&self) -> Location {
        match self {
            StateNode::Simple { loc, .. } | StateNode::Implement { loc, .. } => *loc,
            StateNode::Unresolved => Location::Implicit,
        }
    }

    /// Возвращает имя состояния.
    pub fn name(&self) -> &str {
        match self {
            StateNode::Unresolved => "",
            StateNode::Simple { name, .. } => name,
            StateNode::Implement { name, .. } => name,
        }
    }

    /// Возвращает ссылку на родительскую модель состояния.
    pub fn upper(&self) -> Option<Rc<RefCell<ModelNode>>> {
        match self {
            StateNode::Simple { upper, .. } | StateNode::Implement { upper, .. } => {
                upper.as_ref().and_then(|w| w.upgrade())
            }
            StateNode::Unresolved => None,
        }
    }

    /// Возвращает список именованных блоков состояния.
    pub fn named_blocks(&self) -> &[NamedCodeBlockDefinitionNode] {
        match self {
            StateNode::Unresolved => &[],
            StateNode::Simple { named_blocks, .. } => named_blocks,
            StateNode::Implement { named_blocks, .. } => named_blocks,
        }
    }

    /// Возвращает список ссылок-переходов состояния (`ref`-рёбер).
    pub fn references(&self) -> &[ReferenceNode<StateNode>] {
        match self {
            StateNode::Unresolved => &[],
            StateNode::Simple { references, .. } => references,
            StateNode::Implement { references, .. } => references,
        }
    }

    /// Ищет именованный блок в состоянии по его имени. Возвращает список всех
    /// именованных блоков с заданным именем.
    pub fn get_named_blocks(&self, name: &str) -> Vec<&NamedCodeBlockDefinitionNode> {
        self.named_blocks()
            .iter()
            .filter(|b| b.name() == name)
            .collect()
    }

    /// Возвращает первый именованный блок с заданным именем, если он есть.
    pub fn get_named_block(&self, name: &str) -> Option<&NamedCodeBlockDefinitionNode> {
        self.named_blocks().iter().find(|b| b.name() == name)
    }
}

// `ConditionNode` вынесен в `condition_node.rs` (лимит размера модуля): чистое
// перемещение, путь `semantic::ConditionNode` держит реэкспорт ниже.

/// Разрешённый семантический узел выражения (заглушка - будет расширено).
#[derive(Default, Debug, PartialEq, Eq, Clone)]
pub enum ExpressionDefinitionNode {
    /// Узел выражения ещё не разрешён (значение по умолчанию).
    #[default]
    None,
}

#[cfg(test)]
mod tests;
