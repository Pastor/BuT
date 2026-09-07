//! Узел объявления значения: переменная, порт, константа, параметр модели.
//!
//! "Объявленное значение" - самостоятельная ответственность, а не часть описания
//! модели.

use crate::diagnostics::Location;
use crate::parser::ast::PortDirection;
use crate::semantic::type_node::TypeNode;
use crate::semantic::{ExpressionNode, ModelNode};
use std::cell::RefCell;
use std::rc::{Rc, Weak};

/// Семантический узел переменной.
///
/// Варианты:
/// - [`Unresolved`](VariableNode::Unresolved) - временная заглушка.
/// - [`Simple`](VariableNode::Simple) - обычная изменяемая переменная.
/// - [`Port`](VariableNode::Port) - порт, отображённый на адрес.
/// - [`Const`](VariableNode::Const) - константа.
///
/// Каждый разрешённый вариант хранит [`Location`] из исходного текста - позицию
/// объявления переменной. Это поле используется при формировании диагностических
/// сообщений, чтобы указывать конкретное место ошибки.
#[derive(Default, Debug, Clone)]
pub enum VariableNode {
    /// Не разрешено (временная заглушка при построении дерева).
    #[default]
    Unresolved,
    /// Изменяемая переменная.
    Simple {
        /// Родительская модель (слабая ссылка для предотвращения циклов Rc).
        upper: Option<Weak<RefCell<ModelNode>>>,
        /// Позиция объявления переменной в исходном тексте.
        loc: Location,
        /// Имя переменной.
        name: String,
        /// Тип переменной.
        ty: TypeNode,
        /// Инициализирующее выражение.
        expr: ExpressionNode,
    },
    /// Порт ввода-вывода, объявляется через `in` (входной) или `out` (выходной).
    Port {
        /// Родительская модель (слабая ссылка для предотвращения циклов Rc).
        upper: Option<Weak<RefCell<ModelNode>>>,
        /// Позиция объявления порта в исходном тексте.
        loc: Location,
        /// Имя переменной.
        name: String,
        /// Тип переменной.
        ty: TypeNode,
        /// Размещение порта - выражение адреса из `at <адрес>`.
        ///
        /// `ExpressionNode::None`, если `at` не написан: адрес тогда приходит по имени
        /// порта (оператор `address`, внешняя карта), а полноту проверяет слой адресов
        /// после разрешения источников, а не объявление.
        ///
        /// Переименование заставляет компилятор показать все места.
        address: ExpressionNode,
        /// Начальное значение порта - выражение из `:=`.
        ///
        /// `ExpressionNode::None`, если не задано. У **входного** порта задавать его
        /// незаконно (`SE-092`): значение входа приходит извне.
        init: ExpressionNode,
        /// Направление порта (входной / выходной).
        direction: PortDirection,
    },
    /// Константа.
    Const {
        /// Родительская модель (слабая ссылка для предотвращения циклов Rc).
        upper: Option<Weak<RefCell<ModelNode>>>,
        /// Позиция объявления константы в исходном тексте.
        loc: Location,
        /// Имя константы.
        name: String,
        /// Тип константы.
        ty: TypeNode,
        /// Значение константы.
        expr: ExpressionNode,
    },
}

impl PartialEq for VariableNode {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Unresolved, Self::Unresolved) => true,
            (
                Self::Simple {
                    name: n1,
                    ty: t1,
                    expr: e1,
                    ..
                },
                Self::Simple {
                    name: n2,
                    ty: t2,
                    expr: e2,
                    ..
                },
            ) => n1 == n2 && t1 == t2 && e1 == e2,
            (
                Self::Port {
                    name: n1,
                    ty: t1,
                    address: a1,
                    init: i1,
                    ..
                },
                Self::Port {
                    name: n2,
                    ty: t2,
                    address: a2,
                    init: i2,
                    ..
                },
            ) => n1 == n2 && t1 == t2 && a1 == a2 && i1 == i2,
            (
                Self::Const {
                    name: n1,
                    ty: t1,
                    expr: e1,
                    ..
                },
                Self::Const {
                    name: n2,
                    ty: t2,
                    expr: e2,
                    ..
                },
            ) => n1 == n2 && t1 == t2 && e1 == e2,
            _ => false,
        }
    }
}

impl Eq for VariableNode {}

impl VariableNode {
    /// Возвращает позицию объявления переменной в исходном тексте.
    ///
    /// Для [`Unresolved`](VariableNode::Unresolved) возвращает [`Location::Implicit`],
    /// так как заглушка не привязана к конкретному месту в коде.
    pub fn loc(&self) -> Location {
        match self {
            VariableNode::Simple { loc, .. }
            | VariableNode::Port { loc, .. }
            | VariableNode::Const { loc, .. } => *loc,
            VariableNode::Unresolved => Location::Implicit,
        }
    }

    /// Возвращает имя переменной (пустая строка для `Unresolved`).
    pub fn name(&self) -> &str {
        match self {
            VariableNode::Simple { name, .. }
            | VariableNode::Port { name, .. }
            | VariableNode::Const { name, .. } => name,
            VariableNode::Unresolved => "",
        }
    }

    /// Возвращает тип переменной (`Inference` для `Unresolved`).
    pub fn ty(&self) -> &TypeNode {
        match self {
            VariableNode::Simple { ty, .. }
            | VariableNode::Port { ty, .. }
            | VariableNode::Const { ty, .. } => ty,
            VariableNode::Unresolved => &TypeNode::Inference,
        }
    }

    /// Возвращает ссылку на родительскую модель.
    pub fn upper(&self) -> Option<Rc<RefCell<ModelNode>>> {
        match self {
            VariableNode::Simple { upper, .. }
            | VariableNode::Port { upper, .. }
            | VariableNode::Const { upper, .. } => upper.as_ref().and_then(|w| w.upgrade()),
            VariableNode::Unresolved => None,
        }
    }
}

/// Параметр модели - то, чем он отличается от переменной.
///
/// Значение параметра лежит в [`ModelNode::variables`] обычным
/// [`VariableNode::Simple`]: в режиме генерации по умолчанию (`--parameters=assign`)
/// параметр **и есть** поле экземпляра, и всякий потребитель дерева, ничего не знающий
/// о параметрах, обращается с ним верно (: механизм, требующий правки в каждом из пяти
/// потребителей, расходится молча). Здесь - только то, что от переменной его отличает:
/// имя, позиция объявления и, неявно, порядок (индекс в [`ModelNode::parameters`]).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParameterNode {
    /// Имя параметра - ключ в [`ModelNode::variables`].
    pub name: String,
    /// Позиция объявления в исходном тексте.
    pub loc: Location,
    /// Присваивают ли параметру в теле модели.
    ///
    /// Заполняется анализом изменяемости
    /// ([`parameter_const::mark_mutated`](crate::semantic::parameter_const::mark_mutated))
    /// на стадии 0. `false` - параметр есть **константа**, и в режиме
    /// `--parameters=specialize` он эмитируется константой (уточнение 2: константность
    /// выводится, а не объявляется).
    ///
    /// Значение по умолчанию - `true` ("изменяемый"): не размеченный параметр обязан
    /// вести себя как переменная. Обратное умолчание сделало бы пропуск анализа
    /// неверным выводом, а не потерянной оптимизацией.
    pub mutated: bool,
}

/// Привязка адреса к порту оператором `address`.
///
/// Хранит имя целевого порта, позицию оператора и выражение-адрес. Выражение остаётся
/// "сырым" ([`ExpressionNode::Unresolved`]) до понижения в конкретный адрес
/// потребителем (C-генерация).
#[derive(Debug, Clone)]
pub struct AddressBindingNode {
    /// Имя порта, которому назначается адрес.
    pub port: String,
    /// Позиция оператора `address` в исходном тексте.
    pub loc: Location,
    /// Выражение-адрес (сырое АСД до понижения).
    pub value: ExpressionNode,
}
