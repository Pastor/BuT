//! Области видимости слоя использований.
//!
//! Взять готовые `search_*` не вышло по существу: они возвращают узел, чей `loc`
//! покрывает **весь оператор** объявления, а слою нужна позиция **имени** (иначе правка
//! затрёт объявление целиком).
//!
//! Локальные области (параметры функции, `var` внутри блока) семантика в таком виде
//! вообще не отдаёт, а затенение ими переменной модели реально - значит вести их
//! приходится всё равно.

use super::SymbolId;
use std::collections::HashMap;

/// Вид объявленного символа.
///
/// Нужен потребителю, чтобы решать по виду (переименование имени модели запрещено - оно
/// экспортируемо), и обходу - чтобы различать пространства имён.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    /// Переменная модели (`var`).
    Variable,
    /// Константа модели (`const`).
    Const,
    /// Параметр модели (`parameter`).
    ///
    /// Отдельно от [`SymbolKind::Parameter`] (параметр функции) и от
    /// [`SymbolKind::Variable`]: имя параметра появляется ещё и в аргументе
    /// инстанцирования (`M(Y := 200)`), где принадлежит **чужой** модели.
    ModelParameter,
    /// Порт (`in`/`out`/`inout`).
    Port,
    /// Локальная переменная блока или тела функции.
    Local,
    /// Параметр функции.
    Parameter,
    /// Функция (`fn`), в том числе внешняя.
    Function,
    /// Состояние автомата.
    State,
    /// Именованное условие (`cond`) либо инвариант (`invariant`).
    Condition,
    /// Псевдоним типа (`type`).
    TypeAlias,
    /// Перечисление (`enum`).
    Enum,
    /// Вариант перечисления.
    EnumVariant,
    /// Структурный тип (`struct`).
    Struct,
    /// Модель (`model`) либо имя, введённое `import "путь" as Имя;`.
    Model,
    /// Имя, введённое выборочным импортом (`import { a, b as c } from "...";`).
    ///
    /// Вид объявления в источнике этому слою неизвестен и известен быть не может:
    /// `collect_usages` работает по одному файлу, а объявление живёт в соседнем.
    /// Поэтому такой символ отвечает на ссылку в **любом** пространстве имён - см.
    Imported,
}

impl SymbolKind {
    /// Пространство имён, в котором символ разрешается.
    pub(super) fn namespace(self) -> Namespace {
        match self {
            Self::Variable
            | Self::Const
            | Self::ModelParameter
            | Self::Port
            | Self::Local
            | Self::Parameter => Namespace::Value,
            Self::Function => Namespace::Callable,
            Self::State => Namespace::State,
            Self::Condition => Namespace::Condition,
            Self::TypeAlias | Self::Enum | Self::Struct => Namespace::Type,
            Self::EnumVariant => Namespace::Value,
            Self::Model => Namespace::Model,
            // Собственного пространства у выборочного импорта нет: он подставляет
            // объявление чужого файла. Поиск идёт через `matches`.
            Self::Imported => Namespace::Value,
        }
    }

    /// Отвечает ли символ на ссылку, ищущую имя в пространстве `ns`?
    ///
    /// Для всех видов это равенство пространств; исключение - [`SymbolKind::Imported`],
    /// чей вид задан **чужим** файлом и здесь неизвестен: он подходит любому
    /// пространству. Ошибка такого допущения односторонняя - лишняя связь внутри одного
    /// файла, где имя и так занято импортом, а не потерянная связь.
    pub(super) fn matches(self, ns: Namespace) -> bool {
        matches!(self, Self::Imported) || self.namespace() == ns
    }
}

/// Пространство имён: где искать имя, встреченное в данной позиции.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Namespace {
    /// Значение: переменная, константа, порт, локальная, параметр, вариант enum.
    Value,
    /// Вызываемое: функция.
    Callable,
    /// Состояние автомата.
    State,
    /// Именованное условие.
    Condition,
    /// Тип: псевдоним, перечисление, структура.
    Type,
    /// Модель.
    Model,
}

/// Объявленный символ.
#[derive(Debug, Clone, Copy)]
pub(super) struct Symbol {
    /// Идентичность (позиция имени в объявлении).
    pub id: SymbolId,
    /// Вид символа.
    pub kind: SymbolKind,
}

/// Один уровень области видимости: имя -> объявленные под ним символы.
///
/// Список, а не одно значение: имя может быть занято в разных пространствах (состояние
/// `Done` и переменная `Done` не конфликтуют).
type Level = HashMap<String, Vec<Symbol>>;

/// Стек областей: уровни моделей (снаружи внутрь) и локальные уровни.
///
/// Плюс реестр **членов каждой модели** - он нужен единственной, но обязательной форме:
/// `S(Ping) = End` адресует состояние **чужой** модели, которой в стеке нет (она
/// соседняя, а не объемлющая). Без реестра правая часть осталась бы неразрешённой, то
/// есть переименование состояния отказывало бы на ровном месте.
#[derive(Debug, Default)]
pub(super) struct Scopes {
    models: Vec<Level>,
    locals: Vec<Level>,
    members: HashMap<SymbolId, Level>,
}

impl Scopes {
    /// Входит в новую модель.
    pub(super) fn push_model(&mut self) {
        self.models.push(Level::new());
    }

    /// Выходит из модели.
    pub(super) fn pop_model(&mut self) {
        self.models.pop();
    }

    /// Входит в локальную область (тело функции, блок, ветка).
    pub(super) fn push_local(&mut self) {
        self.locals.push(Level::new());
    }

    /// Выходит из локальной области.
    pub(super) fn pop_local(&mut self) {
        self.locals.pop();
    }

    /// Объявляет символ в текущей модели.
    pub(super) fn declare_in_model(&mut self, name: &str, symbol: Symbol) {
        if let Some(level) = self.models.last_mut() {
            level.entry(name.to_string()).or_default().push(symbol);
        }
    }

    /// Объявляет символ в текущей локальной области.
    ///
    /// Локальные области перекрывают модельные: `var x` внутри блока
    /// **затеняет** одноимённую переменную модели, и цель `c` печатает именно
    /// локальную. Поиск поэтому идёт сверху вниз по локальным уровням и лишь
    /// затем по моделям.
    pub(super) fn declare_local(&mut self, name: &str, symbol: Symbol) {
        if let Some(level) = self.locals.last_mut() {
            level.entry(name.to_string()).or_default().push(symbol);
        } else {
            self.declare_in_model(name, symbol);
        }
    }

    /// Ищет имя в заданном пространстве: сначала локальные области (изнутри наружу),
    /// затем модели (изнутри наружу - как `search_*` идёт по `upper`).
    pub(super) fn resolve(&self, name: &str, ns: Namespace) -> Option<Symbol> {
        self.locals
            .iter()
            .rev()
            .chain(self.models.iter().rev())
            .find_map(|level| {
                level
                    .get(name)?
                    .iter()
                    .rev()
                    .find(|s| s.kind.matches(ns))
                    .copied()
            })
    }

    /// Записывает члена модели в реестр (предпроход).
    pub(super) fn declare_member(&mut self, model: SymbolId, name: &str, symbol: Symbol) {
        self.members
            .entry(model)
            .or_default()
            .entry(name.to_string())
            .or_default()
            .push(symbol);
    }

    /// Ищет имя среди членов заданной модели (`S(Ping) = End`).
    pub(super) fn resolve_member(
        &self,
        model: SymbolId,
        name: &str,
        ns: Namespace,
    ) -> Option<Symbol> {
        self.members
            .get(&model)?
            .get(name)?
            .iter()
            .rev()
            .find(|s| s.kind.matches(ns))
            .copied()
    }

    /// Ищет имя в нескольких пространствах по порядку предпочтения.
    ///
    /// Порядок значим: в условии ребра `ref Stop: Fast;` имя `Fast` - это либо
    /// переменная, либо именованное условие, и переменная имеет преимущество - тот же
    /// порядок, что у перехода к декларации (`search_var`, затем прочее).
    pub(super) fn resolve_any(&self, name: &str, spaces: &[Namespace]) -> Option<Symbol> {
        spaces.iter().find_map(|ns| self.resolve(name, *ns))
    }
}
