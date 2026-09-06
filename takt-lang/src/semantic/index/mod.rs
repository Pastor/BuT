//! Индекс семантических узлов для поиска по позиции в исходном тексте.
//!
//! Модуль реализует [`SemanticIndex`] - структуру данных, позволяющую LSP-сервису за
//! O(n) найти наиболее конкретный узел семантического дерева, покрывающий заданное
//! байтовое смещение в исходном тексте.
//!
//! ## Принцип работы
//!
//! 1. [`SemanticIndex::build`] обходит семантическое дерево [`ModelNode`] и
//!    собирает записи вида `(start_byte, end_byte, SemanticNodeRef)` из позиций
//!    всех объявлений: переменных, функций, состояний, типов, условий, перечислений
//!    и вложенных моделей.
//! 2. Записи сортируются по `start_byte`.
//! 3. [`SemanticIndex::node_at_offset`] перебирает записи и возвращает запись с
//!    наименьшим диапазоном, покрывающим заданное смещение ("наиболее конкретный"
//!    или "внутренний" узел).
//!
//! ## Пример
//!
//! ```
//! use takt_lang::parse;
//! use takt_lang::semantic::tree::construct_model;
//! use takt_lang::semantic::index::SemanticIndex;
//!
//! let src = "var x: bit := false;";
//! let (ast, _) = parse(src, 0).unwrap();
//! let model = construct_model(&ast, None, &[]).unwrap();
//! let index = SemanticIndex::build(&model);
//!
//! // Смещение 4 - внутри имени переменной "x"
//! let node = index.node_at_offset(4);
//! assert!(node.is_some());
//! assert_eq!(node.unwrap().name, "x");
//! ```

use crate::diagnostics::{Location, ROOT_FILE_NO};
use crate::parser::ast;
// Полное имя: короткое `Extend` заняла `std::iter::Extend`.
use crate::semantic::extend::Extend as ModelExtend;
use crate::semantic::{
    ConditionNode, ExpressionNode, FunctionDefinitionNode, ModelNode, NamedCodeBlockDefinitionNode,
    StateNode, StateNodeKind, StatementNode, VariableNode,
};
use std::cell::RefCell;
use std::rc::Rc;

// Сбор записей индекса вынесен в подмодуль. Реэкспорт держит контракт - вызовы
// `collect_*` в impl и тестах прежние.
mod collect;
use collect::*;

/// Вид семантического узла.
///
/// Используется в [`SemanticNodeRef`] для указания категории найденного элемента, что
/// позволяет LSP-сервису формировать корректный ответ (hover, go-to-definition и др.)
/// без дополнительного поиска по имени.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SemanticNodeKind {
    /// Обычная изменяемая переменная (`var`).
    Variable,
    /// Константа (`const`).
    Const,
    /// Порт ввода-вывода (`port`).
    Port,
    /// Локальная функция (`fn`).
    Function,
    /// Внешняя функция (`extern fn`).
    ExternFunction,
    /// Обычное состояние автомата (`state`).
    State,
    /// Ссылка-переход на другое состояние (`ref Имя [: условие]`).
    Reference,
    /// Ссылка на идентификатор внутри условия перехода (переменная или функция).
    ReferenceCondition,
    /// Ссылка на **модель** по имени: `start Main = Helper;`, `S(Helper)`.
    ///
    /// Единственный вид ссылки, способный указать в **другой файл**: имя, связанное
    /// `import`, - это корень импортированного файла. Поэтому на нём и стоит
    /// кросс-файловый переход.
    ///
    /// Не путать с [`Model`](SemanticNodeKind::Model): тот - **объявление** (`model
    /// Helper { ... }`), этот - **использование** имени.
    ReferenceModel,
    /// Ссылка на **состояние** по имени внутри условия: `End` в `S(Ping) = End`.
    ///
    /// Не путать с [`State`](SemanticNodeKind::State) (**объявление** `state End { ...
    /// }`) и с [`Reference`](SemanticNodeKind::Reference) (цель `ref`-перехода). Этот -
    /// **использование** имени состояния в условии).
    ReferenceState,
    /// Начальное состояние автомата (`start`).
    StartState,
    /// Конечное состояние автомата (`end`).
    EndState,
    /// Псевдоним типа (`type`).
    TypeAlias,
    /// Именованное условие перехода (`cond`).
    Condition,
    /// Перечисление (`enum`).
    Enum,
    /// Именованная модель конечного автомата (`model`).
    Model,
    /// Локальная переменная внутри блока (`enter`, `exit`, `always`, тела функции).
    LocalVar,
}

/// Ссылка на узел семантического дерева с позицией в исходном тексте.
///
/// Содержит имя элемента, его вид и позицию объявления (`loc`). Не хранит сам узел
/// семантического дерева - для получения полных данных узла используйте методы поиска
/// [`ModelNode`]: [`search_var`](ModelNode::search_var),
/// [`search_func`](ModelNode::search_func), [`search_state`](ModelNode::search_state) и
/// т.д.
#[derive(Debug, Clone)]
pub struct SemanticNodeRef {
    /// Имя элемента (переменной, функции, состояния и т.д.).
    pub name: String,
    /// Вид узла.
    pub kind: SemanticNodeKind,
    /// Позиция объявления в исходном тексте.
    pub loc: Location,
    /// Модель, в которой объявлен элемент (для поиска в правильном контексте области
    /// видимости).
    pub model: Option<Rc<RefCell<ModelNode>>>,
}

/// Внутренняя запись индекса.
#[derive(Debug)]
struct IndexEntry {
    /// Начало диапазона (байтовое смещение, включительно).
    start: usize,
    /// Конец диапазона (байтовое смещение, включительно).
    end: usize,
    /// Ссылка на семантический узел.
    node_ref: SemanticNodeRef,
}

impl IndexEntry {
    /// Номер файла, которому принадлежит запись.
    ///
    /// **Выводится из позиции узла, а не хранится отдельным полем** - намеренно:
    /// во всех местах построения `start`/`end` берутся из того же `Location`,
    /// что кладётся в `node_ref.loc`. Отдельное поле пришлось бы проставлять в
    /// каждом из них, и первое же забытое место вернуло бы файлослепоту - ровно
    /// тот дефект, который чинит.
    ///
    /// Позиция без файла (`Codegen`/`Implicit`/...) в индекс не попадает: записи
    /// создаются только под `Location::Source`.
    fn file_no(&self) -> u64 {
        match self.node_ref.loc {
            Location::Source(file_no, _, _) => file_no as u64,
            _ => ROOT_FILE_NO,
        }
    }
}

/// Индекс семантических узлов, упорядоченный по байтовому смещению.
///
/// Строится из корневого [`ModelNode`] методом [`build`](SemanticIndex::build).
/// Позволяет за O(n) найти наиболее конкретный узел, покрывающий заданное байтовое
/// смещение в исходном тексте.
pub struct SemanticIndex {
    /// Записи, отсортированные по полю `start` для предсказуемого обхода.
    entries: Vec<IndexEntry>,
}

impl std::fmt::Debug for SemanticIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SemanticIndex")
            .field("entries_count", &self.entries.len())
            .finish()
    }
}

impl SemanticIndex {
    /// Строит индекс из корневого узла семантической модели.
    ///
    /// Рекурсивно обходит все вложенные модели, собирая позиции объявлений переменных,
    /// функций, состояний, псевдонимов типов, условий, перечислений и именованных
    /// моделей.
    ///
    /// Элементы с [`Location::Builtin`], [`Location::Implicit`] и [`Location::Codegen`]
    /// в индекс не включаются.
    ///
    /// # Пример
    ///
    /// ```
    /// use takt_lang::parse;
    /// use takt_lang::semantic::tree::construct_model;
    /// use takt_lang::semantic::index::SemanticIndex;
    ///
    /// let src = "var x: bit := false; start S;";
    /// let (ast, _) = parse(src, 0).unwrap();
    /// let model = construct_model(&ast, None, &[]).unwrap();
    /// let index = SemanticIndex::build(&model);
    /// assert!(index.len() >= 2); // минимум: переменная + состояние
    /// ```
    pub fn build(model: &Rc<RefCell<ModelNode>>) -> Self {
        let mut entries = Vec::new();
        collect_model_entries(model, &mut entries);
        // Сортируем по началу диапазона - позволяет прерывать перебор при start >
        // offset
        entries.sort_by_key(|e| e.start);
        SemanticIndex { entries }
    }

    /// Возвращает наиболее конкретный узел **корневого файла**, покрывающий смещение
    /// `offset`.
    ///
    /// Среди всех записей, чей диапазон `[start, end]` содержит `offset`, выбирается
    /// та, у которой наименьший размер диапазона - т.е. наиболее специфичный
    /// (внутренний) узел.
    ///
    /// # Пример
    ///
    /// ```
    /// use takt_lang::parse;
    /// use takt_lang::semantic::tree::construct_model;
    /// use takt_lang::semantic::index::{SemanticIndex, SemanticNodeKind};
    ///
    /// let src = "var counter: bit := false;";
    /// let (ast, _) = parse(src, 0).unwrap();
    /// let model = construct_model(&ast, None, &[]).unwrap();
    /// let index = SemanticIndex::build(&model);
    ///
    /// // Смещение 4 - на символе 'c' в "counter"
    /// let node = index.node_at_offset(4);
    /// assert!(node.is_some());
    /// let node = node.unwrap();
    /// assert_eq!(node.name, "counter");
    /// assert_eq!(node.kind, SemanticNodeKind::Variable);
    ///
    /// // Смещение за пределами всех объявлений
    /// let node_none = index.node_at_offset(99999);
    /// assert!(node_none.is_none());
    /// ```
    pub fn node_at_offset(&self, offset: usize) -> Option<&SemanticNodeRef> {
        self.node_at_offset_in_file(ROOT_FILE_NO, offset)
    }

    /// То же, что [`node_at_offset`](Self::node_at_offset), но в заданном файле.
    ///
    /// Смещение адресует текст **одного** файла, поэтому пара `(file_no, offset)` -
    /// минимальный ключ, которым узел вообще можно найти однозначно.
    pub fn node_at_offset_in_file(&self, file_no: u64, offset: usize) -> Option<&SemanticNodeRef> {
        let mut best: Option<&IndexEntry> = None;
        let mut best_size = usize::MAX;

        for entry in &self.entries {
            // Записи отсортированы по start: как только start > offset - дальше нет
            // смысла. Сортировка сквозная по всем файлам, но проверка остаётся верной:
            // у последующих записей start только больше.
            if entry.start > offset {
                break;
            }
            // Чужой файл: его смещения относятся к его тексту - сравнивать не с чем.
            if entry.file_no() != file_no {
                continue;
            }
            // Проверяем, что offset попадает в диапазон [start, end] (включительно)
            if entry.end >= offset {
                let size = entry.end.saturating_sub(entry.start);
                if size < best_size {
                    best_size = size;
                    best = Some(entry);
                }
            }
        }

        best.map(|e| &e.node_ref)
    }

    /// Возвращает количество записей в индексе.
    ///
    /// Используется преимущественно в тестах для проверки корректности построения.
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Возвращает `true`, если индекс не содержит ни одной записи.
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Ищет первый узел с заданным именем и видом.
    ///
    /// Используется для поиска декларации по имени при кросс-файловых переходах.
    ///
    /// Возвращает `None`, если узел с таким именем и видом не найден.
    pub fn find_by_name(&self, name: &str, kind: &SemanticNodeKind) -> Option<&SemanticNodeRef> {
        self.entries
            .iter()
            .find(|e| e.node_ref.name == name && &e.node_ref.kind == kind)
            .map(|e| &e.node_ref)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse;
    use crate::semantic::tree::construct_model;

    /// Вспомогательная функция: парсит источник и строит индекс.
    fn build_index(src: &str) -> SemanticIndex {
        let (ast, _) = parse(src, 0).expect("ошибка парсинга");
        let model = construct_model(&ast, None, &[]).expect("ошибка семантики");
        SemanticIndex::build(&model)
    }

    // -- Юнит-тесты SemanticIndex ----------------------------------------------

    /// Пустая программа -> пустой индекс.
    #[test]
    fn empty_source_gives_empty_index() {
        let index = build_index("");
        assert!(index.is_empty());
    }

    /// Одна переменная индексируется корректно.
    #[test]
    fn single_variable_is_indexed() {
        let src = "var x: bit := false;";
        let index = build_index(src);
        // Индекс должен содержать хотя бы запись о переменной
        assert!(!index.is_empty());
    }

    /// Поиск по смещению внутри объявления переменной возвращает её имя.
    #[test]
    fn node_at_offset_finds_variable() {
        //           0123456789...
        let src = "var x: bit := false;";
        let index = build_index(src);
        // Смещение 4 - символ 'x'
        let node = index.node_at_offset(4);
        assert!(node.is_some(), "должен найти переменную");
        let node = node.unwrap();
        assert_eq!(node.name, "x");
        assert_eq!(node.kind, SemanticNodeKind::Variable);
    }

    /// Поиск за пределами всех объявлений возвращает None.
    #[test]
    fn node_at_offset_out_of_range_returns_none() {
        let src = "var x: bit := false;";
        let index = build_index(src);
        // Смещение 99999 - далеко за концом файла
        assert!(index.node_at_offset(99999).is_none());
    }

    /// Состояние индексируется и находится по смещению.
    #[test]
    fn state_is_indexed() {
        // 012345678901234
        let src = "start Init;";
        let index = build_index(src);
        let node = index.node_at_offset(6);
        assert!(node.is_some(), "должен найти состояние");
        let node = node.unwrap();
        assert_eq!(node.name, "Init");
        assert_eq!(node.kind, SemanticNodeKind::StartState);
    }

    /// Функция индексируется и находится по смещению.
    #[test]
    fn function_is_indexed() {
        // Параметры функции не доступны в выражениях тела - используем литерал
        let src = "fn add(a: bit) -> bit { return true; }";
        let index = build_index(src);
        // Смещение 3 - символ 'a' в "add"
        let node = index.node_at_offset(3);
        assert!(node.is_some(), "должен найти функцию");
        let node = node.unwrap();
        assert_eq!(node.name, "add");
        assert_eq!(node.kind, SemanticNodeKind::Function);
    }

    /// Псевдоним типа индексируется.
    #[test]
    fn type_alias_is_indexed() {
        let src = "type Byte = [bit;8];";
        let index = build_index(src);
        // Смещение 5 - символ 'B' в "Byte"
        let node = index.node_at_offset(5);
        assert!(node.is_some(), "должен найти псевдоним типа");
        let node = node.unwrap();
        assert_eq!(node.name, "Byte");
        assert_eq!(node.kind, SemanticNodeKind::TypeAlias);
    }

    /// Именованное условие индексируется.
    #[test]
    fn condition_is_indexed() {
        let src = "cond IsReady = true;";
        let index = build_index(src);
        // Смещение 5 - символ 'I' в "IsReady"
        let node = index.node_at_offset(5);
        assert!(node.is_some(), "должен найти условие");
        let node = node.unwrap();
        assert_eq!(node.name, "IsReady");
        assert_eq!(node.kind, SemanticNodeKind::Condition);
    }

    /// Перечисление индексируется.
    #[test]
    fn enum_is_indexed() {
        // Перечисление объявляется внутри модели; варианты разделяются запятыми
        let src = "model M { enum Color { Red, Green, Blue } start S; }";
        let index = build_index(src);
        // Смещение 16 - символ 'C' в "Color"
        let node = index.node_at_offset(16);
        assert!(node.is_some(), "должен найти перечисление");
        let node = node.unwrap();
        assert_eq!(node.name, "Color");
        assert_eq!(node.kind, SemanticNodeKind::Enum);
    }

    /// Именованная модель индексируется.
    #[test]
    fn model_is_indexed() {
        let src = "model Blinker { start On; state Off; }";
        let index = build_index(src);
        // Смещение 6 - символ 'B' в "Blinker"
        let node = index.node_at_offset(6);
        assert!(node.is_some(), "должен найти модель");
        let node = node.unwrap();
        assert_eq!(node.name, "Blinker");
        assert_eq!(node.kind, SemanticNodeKind::Model);
    }

    /// Несколько элементов: поиск возвращает наиболее конкретный.
    #[test]
    fn multiple_elements_most_specific_returned() {
        // 0 1 2 3 0123456789012345678901234567890123456789
        let src = "var alpha: bit := false; start Beta;";
        let index = build_index(src);
        // Смещение 4 - 'a' в "alpha"
        let node = index.node_at_offset(4).expect("должен найти элемент");
        assert_eq!(node.name, "alpha");
        // Смещение 30 - 'B' в "Beta"
        let node2 = index.node_at_offset(30).expect("должен найти состояние");
        assert_eq!(node2.name, "Beta");
    }

    /// len() возвращает корректное количество записей.
    #[test]
    fn len_matches_element_count() {
        // 1 переменная + 1 состояние = минимум 2
        let src = "var x: bit := false; start S;";
        let index = build_index(src);
        assert!(index.len() >= 2);
    }

    /// Константа индексируется с правильным видом.
    #[test]
    fn const_kind_is_indexed() {
        let src = "const LIMIT: bit := true;";
        let index = build_index(src);
        let node = index.node_at_offset(6);
        assert!(node.is_some(), "должен найти константу");
        let node = node.unwrap();
        assert_eq!(node.name, "LIMIT");
        assert_eq!(node.kind, SemanticNodeKind::Const);
    }

    /// Внешняя функция индексируется с видом ExternFunction.
    #[test]
    fn extern_function_kind_is_indexed() {
        let src = "extern fn send(data: bit);";
        let index = build_index(src);
        let node = index.node_at_offset(10);
        assert!(node.is_some(), "должен найти extern fn");
        let node = node.unwrap();
        assert_eq!(node.name, "send");
        assert_eq!(node.kind, SemanticNodeKind::ExternFunction);
    }

    // -- Тесты collect_ast_condition_entries -----------------------------------

    /// Переменная с Source-позицией добавляет ReferenceCondition-запись.
    ///
    /// # Пример
    /// `ast::Condition::Variable(id@"flag", loc=Source(0,5,9))` ->
    /// `IndexEntry { start:5, end:9, name:"flag", kind:ReferenceCondition }`
    #[test]
    fn ast_condition_variable_adds_reference_condition() {
        use crate::diagnostics::Location;
        use crate::parser::ast::Identifier;

        let mut entries = Vec::new();
        let model = Rc::new(RefCell::new(super::super::ModelNode::default()));
        let id = Identifier {
            loc: Location::Source(0, 5, 9),
            name: "flag".into(),
        };
        let cond = crate::parser::ast::Condition::Variable(id);
        collect_ast_condition_entries(&cond, &model, &mut entries);

        assert_eq!(entries.len(), 1, "ожидается одна запись");
        assert_eq!(entries[0].node_ref.name, "flag");
        assert_eq!(
            entries[0].node_ref.kind,
            SemanticNodeKind::ReferenceCondition
        );
        assert_eq!(entries[0].start, 5);
        assert_eq!(entries[0].end, 9);
    }

    /// Переменная с Builtin-позицией не добавляет записей.
    ///
    /// # Контрпример
    /// `ast::Condition::Variable(id@"built", loc=Builtin)` -> `(нет записей)`
    #[test]
    fn ast_condition_variable_builtin_loc_no_entry() {
        use crate::diagnostics::Location;
        use crate::parser::ast::Identifier;

        let mut entries = Vec::new();
        let model = Rc::new(RefCell::new(super::super::ModelNode::default()));
        let id = Identifier {
            loc: Location::Builtin,
            name: "built".into(),
        };
        let cond = crate::parser::ast::Condition::Variable(id);
        collect_ast_condition_entries(&cond, &model, &mut entries);

        assert!(
            entries.is_empty(),
            "Builtin-позиция не должна давать запись"
        );
    }

    /// Бинарный оператор (AND) рекурсивно обходит оба операнда.
    ///
    /// # Пример
    /// `And(loc, Variable("a",1..2), Variable("b",5..6))` ->
    /// `IndexEntry("a",1,2), IndexEntry("b",5,6)`
    #[test]
    fn ast_condition_and_recurses_both_operands() {
        use crate::diagnostics::Location;
        use crate::parser::ast::{Condition as AstCond, Identifier};

        let mut entries = Vec::new();
        let model = Rc::new(RefCell::new(super::super::ModelNode::default()));
        let loc = Location::Source(0, 0, 10);
        let a = AstCond::Variable(Identifier {
            loc: Location::Source(0, 1, 2),
            name: "a".into(),
        });
        let b = AstCond::Variable(Identifier {
            loc: Location::Source(0, 5, 6),
            name: "b".into(),
        });
        let cond = AstCond::And(loc, Box::new(a), Box::new(b));
        collect_ast_condition_entries(&cond, &model, &mut entries);

        assert_eq!(entries.len(), 2, "должны добавиться обе переменные");
        let names: Vec<&str> = entries.iter().map(|e| e.node_ref.name.as_str()).collect();
        assert!(names.contains(&"a"), "ожидается переменная 'a'");
        assert!(names.contains(&"b"), "ожидается переменная 'b'");
    }

    /// Функция в условии добавляет запись для имени функции.
    ///
    /// # Пример
    /// `Function(loc, id@"check", [Variable("x")])` ->
    /// `IndexEntry("check"), IndexEntry("x")`
    #[test]
    fn ast_condition_function_adds_function_entry() {
        use crate::diagnostics::Location;
        use crate::parser::ast::{Condition as AstCond, Identifier};

        let mut entries = Vec::new();
        let model = Rc::new(RefCell::new(super::super::ModelNode::default()));
        let fn_id = Identifier {
            loc: Location::Source(0, 0, 5),
            name: "check".into(),
        };
        let arg = AstCond::Variable(Identifier {
            loc: Location::Source(0, 6, 7),
            name: "x".into(),
        });
        let cond = AstCond::Function(Location::Source(0, 0, 8), fn_id, vec![arg]);
        collect_ast_condition_entries(&cond, &model, &mut entries);

        assert_eq!(
            entries.len(),
            2,
            "ожидается запись для функции и для аргумента"
        );
        assert_eq!(entries[0].node_ref.name, "check");
        assert_eq!(entries[1].node_ref.name, "x");
    }

    /// Числовой литерал в условии не добавляет записей.
    ///
    /// # Контрпример
    /// `ast::Condition::Number(loc, 42)` -> `(нет записей)`
    #[test]
    fn ast_condition_number_literal_no_entry() {
        use crate::diagnostics::Location;
        use crate::parser::ast::Condition as AstCond;

        let mut entries = Vec::new();
        let model = Rc::new(RefCell::new(super::super::ModelNode::default()));
        let cond = AstCond::Number(Location::Source(0, 0, 2), 42);
        collect_ast_condition_entries(&cond, &model, &mut entries);

        assert!(
            entries.is_empty(),
            "числовой литерал не должен давать запись"
        );
    }

    /// NOT-оператор рекурсивно обходит вложенное условие.
    ///
    /// # Пример
    /// `Not(loc, Variable("ready", 4..9))` -> `IndexEntry("ready", 4, 9)`
    #[test]
    fn ast_condition_not_recurses_into_operand() {
        use crate::diagnostics::Location;
        use crate::parser::ast::{Condition as AstCond, Identifier};

        let mut entries = Vec::new();
        let model = Rc::new(RefCell::new(super::super::ModelNode::default()));
        let inner = AstCond::Variable(Identifier {
            loc: Location::Source(0, 4, 9),
            name: "ready".into(),
        });
        let cond = AstCond::Not(Location::Source(0, 3, 9), Box::new(inner));
        collect_ast_condition_entries(&cond, &model, &mut entries);

        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].node_ref.name, "ready");
    }

    // -- Тесты collect_condition_entries (семантическое условие) --------------

    /// Разрешённое условие с переменной добавляет ReferenceCondition-запись с позицией
    /// использования.
    ///
    /// # Пример
    /// `ref T: flag;` - `flag` разрешена в `Condition::Variable(var, loc_use)` ->
    /// в индексе есть запись `ReferenceCondition` для `flag` по позиции её имени в условии.
    #[test]
    fn collect_condition_entries_resolved_variable_adds_entry() {
        // 0 1 2 3 4 5 012345678901234567890123456789012345678901234567890123456789
        let src = "var flag: bit := false; start S { ref T: flag; } state T;";
        let index = build_index(src);
        let rc_entries: Vec<_> = index
            .entries
            .iter()
            .filter(|e| e.node_ref.kind == SemanticNodeKind::ReferenceCondition)
            .collect();
        // Ожидается ровно одна запись - для "flag" в условии перехода
        assert_eq!(
            rc_entries.len(),
            1,
            "ожидается одна ReferenceCondition-запись для 'flag': {:?}",
            rc_entries
        );
        assert_eq!(rc_entries[0].node_ref.name, "flag");
    }

    /// Condition::None не добавляет записей.
    #[test]
    fn collect_condition_entries_none_no_entry() {
        let mut entries = Vec::new();
        let model = Rc::new(RefCell::new(super::super::ModelNode::default()));
        collect_condition_entries(&ConditionNode::None, &model, &mut entries);
        assert!(
            entries.is_empty(),
            "Condition::None не должен давать записей"
        );
    }

    /// Condition::Bool не добавляет записей (нет идентификатора).
    #[test]
    fn collect_condition_entries_bool_no_entry() {
        let mut entries = Vec::new();
        let model = Rc::new(RefCell::new(super::super::ModelNode::default()));
        collect_condition_entries(&ConditionNode::Bool(true), &model, &mut entries);
        assert!(
            entries.is_empty(),
            "Condition::Bool не должен давать записей"
        );
    }

    // -- Тесты collect_ast_expression_entries ---------------------------------

    /// Переменная в выражении добавляет ReferenceCondition-запись.
    ///
    /// # Пример
    /// `ast::Expression::Variable(id@"speed", loc=Source(0,0,5))` ->
    /// `IndexEntry { start:0, end:5, name:"speed", kind:ReferenceCondition }`
    #[test]
    fn ast_expression_variable_adds_entry() {
        use crate::diagnostics::Location;
        use crate::parser::ast::{Expression as AstExpr, Identifier};

        let mut entries = Vec::new();
        let model = Rc::new(RefCell::new(super::super::ModelNode::default()));
        let id = Identifier {
            loc: Location::Source(0, 0, 5),
            name: "speed".into(),
        };
        let expr = AstExpr::Variable(id);
        collect_ast_expression_entries(&expr, &model, &mut entries);

        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].node_ref.name, "speed");
        assert_eq!(
            entries[0].node_ref.kind,
            SemanticNodeKind::ReferenceCondition
        );
    }

    /// Присваивание рекурсивно обходит левую и правую части.
    ///
    /// # Пример
    /// `Assign(_, Variable("x"), Variable("y"))` ->
    /// `IndexEntry("x"), IndexEntry("y")`
    #[test]
    fn ast_expression_assign_recurses_both_sides() {
        use crate::diagnostics::Location;
        use crate::parser::ast::{Expression as AstExpr, Identifier};

        let mut entries = Vec::new();
        let model = Rc::new(RefCell::new(super::super::ModelNode::default()));
        let x = AstExpr::Variable(Identifier {
            loc: Location::Source(0, 0, 1),
            name: "x".into(),
        });
        let y = AstExpr::Variable(Identifier {
            loc: Location::Source(0, 4, 5),
            name: "y".into(),
        });
        let expr = AstExpr::Assign(Location::Source(0, 0, 5), Box::new(x), Box::new(y));
        collect_ast_expression_entries(&expr, &model, &mut entries);

        assert_eq!(entries.len(), 2);
        let names: Vec<&str> = entries.iter().map(|e| e.node_ref.name.as_str()).collect();
        assert!(names.contains(&"x"));
        assert!(names.contains(&"y"));
    }

    /// Числовой литерал в выражении не добавляет записей.
    ///
    /// # Контрпример
    /// `ast::Expression::Number(loc, 7)` -> `(нет записей)`
    #[test]
    fn ast_expression_number_literal_no_entry() {
        use crate::diagnostics::Location;
        use crate::parser::ast::Expression as AstExpr;

        let mut entries = Vec::new();
        let model = Rc::new(RefCell::new(super::super::ModelNode::default()));
        let expr = AstExpr::Number(Location::Source(0, 0, 1), 7);
        collect_ast_expression_entries(&expr, &model, &mut entries);

        assert!(
            entries.is_empty(),
            "числовой литерал не должен давать запись"
        );
    }

    // -- Тесты collect_named_block_entries ------------------------------------

    /// Условие `true` (Bool) не создаёт ReferenceCondition-записей.
    ///
    /// # Контрпример
    /// `ref T: true;` - литерал, не переменная -> нет ReferenceCondition.
    #[test]
    fn named_block_bool_condition_no_reference_condition() {
        let src = "var x: bit := false; start S { always { x := true; } ref T: true; } state T;";
        let index = build_index(src);
        let rc_entries: Vec<_> = index
            .entries
            .iter()
            .filter(|e| e.node_ref.kind == SemanticNodeKind::ReferenceCondition)
            .collect();
        // `true` - литерал, не переменная -> нет записей ReferenceCondition
        assert!(
            rc_entries.is_empty(),
            "bool-литерал в условии не должен давать ReferenceCondition: {:?}",
            rc_entries
        );
    }

    /// Переменная в условии перехода индексируется по позиции использования.
    ///
    /// # Пример
    /// `ref T: flag;` - `flag` появляется в индексе как `ReferenceCondition`
    /// по позиции имени `flag` в исходном тексте (use-site), а не по позиции объявления.
    #[test]
    fn condition_variable_use_site_is_indexed() {
        // 0 1 2 3 0123456789012345678901234567890123456789012345678901234567
        let src = "var flag: bit := false; start S { ref T: flag; } state T;";
        // ^^^^ позиция "flag" = 40..44
        let index = build_index(src);
        // Находим запись ReferenceCondition по use-site позиции (offset внутри "flag")
        let node = index.node_at_offset(41); // 'l' в "flag"
        assert!(node.is_some(), "должна найтись запись для 'flag' в условии");
        let node = node.unwrap();
        assert_eq!(node.name, "flag");
        assert_eq!(node.kind, SemanticNodeKind::ReferenceCondition);
    }

    /// Переменная в условии AND-перехода: обе стороны индексируются.
    ///
    /// # Пример
    /// `ref T: a & b;` -> записи для `a` и `b` по их позициям в условии.
    #[test]
    fn condition_and_both_variables_indexed() {
        let src = "var a: bit := false; var b: bit := true; start S { ref T: a & b; } state T;";
        let index = build_index(src);
        let rc: Vec<_> = index
            .entries
            .iter()
            .filter(|e| e.node_ref.kind == SemanticNodeKind::ReferenceCondition)
            .collect();
        assert_eq!(rc.len(), 2, "ожидается по одной записи для 'a' и 'b'");
        let names: Vec<&str> = rc.iter().map(|e| e.node_ref.name.as_str()).collect();
        assert!(names.contains(&"a"), "ожидается 'a'");
        assert!(names.contains(&"b"), "ожидается 'b'");
    }

    /// NamedCodeBlock::None не вызывает паники.
    #[test]
    fn named_block_none_does_not_panic() {
        let mut entries = Vec::new();
        let model = Rc::new(RefCell::new(super::super::ModelNode::default()));
        collect_named_block_entries(&NamedCodeBlockDefinitionNode::None, &model, &mut entries);
        assert!(entries.is_empty());
    }

    /// collect_statement_entries на Statement::None не вызывает паники.
    #[test]
    fn statement_entries_none_does_not_panic() {
        let mut entries = Vec::new();
        let model = Rc::new(RefCell::new(super::super::ModelNode::default()));
        collect_statement_entries(&StatementNode::None, &model, &mut entries);
        assert!(entries.is_empty());
    }

    /// collect_ast_statement_entries рекурсивно обходит Block.
    ///
    /// # Пример
    /// `Block { stmts: [Expression(_, Variable("v"))] }` -> `IndexEntry("v")`
    #[test]
    fn ast_statement_block_recurses_into_expressions() {
        use crate::diagnostics::Location;
        use crate::parser::ast::{Expression as AstExpr, Identifier, Statement as AstStmt};

        let mut entries = Vec::new();
        let model = Rc::new(RefCell::new(super::super::ModelNode::default()));
        let id = Identifier {
            loc: Location::Source(0, 5, 6),
            name: "v".into(),
        };
        let inner_expr = AstExpr::Variable(id);
        let inner_stmt = AstStmt::Expression(Location::Source(0, 5, 7), inner_expr);
        let block = AstStmt::Block {
            loc: Location::Source(0, 0, 10),
            unchecked: false,
            statements: vec![inner_stmt],
        };
        collect_ast_statement_entries(&block, &model, &mut entries);

        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].node_ref.name, "v");
    }
}
