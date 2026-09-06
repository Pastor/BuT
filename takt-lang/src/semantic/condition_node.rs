//! Семантический узел условия ([`ConditionNode`]).
//!
//! Вынесен из `semantic/mod.rs` **чистым перемещением**: файл пришпилен реестром
//! размеров (`scripts/module-size-baseline.txt`) и расти не имеет права, а узел -
//! самостоятельное знание. Путь `semantic::ConditionNode` сохранён реэкспортом, поэтому
//! потребители не тронуты.

use super::*;

/// Условие перехода между состояниями.
///
/// В текущей реализации поддерживается только вариант [`None`](ConditionNode::None),
/// означающий безусловный переход. Полный набор условий - в будущих версиях.
///
/// `PartialEq` реализован вручную: поле `Location` в вариантах `Variable` и `Function`
/// не участвует в сравнении - оно несёт позицию использования (use-site), а не часть
/// семантической идентичности условия.
#[derive(Default, Debug, Clone)]
pub enum ConditionNode {
    /// Безусловный переход (условие не задано или не разрешено).
    #[default]
    None,
    /// Заглушка для условия, которое ещё не было разрешено.
    Unresolved(ast::Condition),
    /// Доступ к элементу массива: `id[индекс]`. Доступ к элементу массива:
    /// `база[индекс]`.
    ArraySubscript(Box<ConditionNode>, Box<ConditionNode>),
    /// Скобки: `(условие)`.
    Parenthesis(Box<ConditionNode>),
    /// Доступ к биту: `условие.член`.
    BitAccess(Box<ConditionNode>, Member),
    /// Вызов функции: `id(аргументы,*)`.
    ///
    /// Третье поле - позиция имени функции в исходном тексте (use-site).
    Function(
        Rc<RefCell<FunctionDefinitionNode>>,
        Vec<Box<ConditionNode>>,
        Location,
    ),
    /// Логическое не: `!условие`.
    Not(Box<ConditionNode>),
    /// Сложение: `левое + правое`.
    Add(Box<ConditionNode>, Box<ConditionNode>),
    /// Вычитание: `левое - правое`.
    Subtract(Box<ConditionNode>, Box<ConditionNode>),
    /// Побитовое И: `левое & правое`.
    And(Box<ConditionNode>, Box<ConditionNode>),
    /// Побитовое или: `левое | правое`.
    Or(Box<ConditionNode>, Box<ConditionNode>),
    /// Меньше: `левое < правое`.
    Less(Box<ConditionNode>, Box<ConditionNode>),
    /// Больше: `левое > правое`.
    More(Box<ConditionNode>, Box<ConditionNode>),
    /// Меньше или равно: `левое <= правое`.
    LessEqual(Box<ConditionNode>, Box<ConditionNode>),
    /// Больше или равно: `левое >= правое`.
    MoreEqual(Box<ConditionNode>, Box<ConditionNode>),
    /// Равенство: `левое = правое`.
    Equal(Box<ConditionNode>, Box<ConditionNode>),
    /// Неравенство: `левое != правое`.
    NotEqual(Box<ConditionNode>, Box<ConditionNode>),
    /// Целочисленный литерал (носитель - `i128`).
    Number(i128),
    /// Литерал длительности в наносекундах.
    Duration(i64),
    /// Выдержка от входа в состояние: `after 3s`, в наносекундах.
    ///
    /// Сахар: своей семантики времени не вводит - истинно, когда с момента входа в
    /// **текущее состояние** прошло не меньше указанного. Отсчёт ведёт исполнитель
    /// (симулятор - по модельным часам, цели - по своему счётчику).
    After(i64),
    /// Выдержка в **тактах** от входа в состояние: `after 3t`.
    ///
    /// Отдельно от [`After`](ConditionNode::After): такт физической длительности не
    /// имеет, поэтому частота такой выдержке не нужна - она работает в любом профиле
    /// времени.
    AfterTicks(i64),
    /// Выдержка, длительность которой **вычисляется в такте**: `after (v + 1s)`.
    ///
    /// Отдельно от [`After`](ConditionNode::After): у той значение известно
    /// компилятору, и цели печатают число. Здесь вложенное условие даёт длительность во
    /// время работы автомата - симулятор считает её в наносекундах, цели в
    /// миллисекундах (представление ).
    ///
    /// Операнды обязаны быть **длительностями** (литералы, переменные, порты и
    /// константы типа `duration`): смешение с числом запрещено языком (`SE-065`), и
    /// выдержка исключения не делает.
    AfterExpr(Box<ConditionNode>),
    /// Вещественный литерал: `(строка, отрицательный)`.
    Rational(String, bool),
    /// Конкатенация строковых литералов.
    String(Vec<String>),
    /// Булевый литерал.
    Bool(bool),
    /// Анонимное обращение к ячейке по адресу: `#0x346619.4`.
    ///
    /// Тот же узел, что [`ExpressionNode::AnonPort`], в условии. Ширину доступа в
    /// условии задать нечем (`as` в грамматике условий нет), поэтому сюда доходит
    /// только битовая форма - словную отвергает свёртка `SE-097`.
    AnonPort(AnonPortAccess),
    /// Переменная.
    ///
    /// Второе поле - позиция использования переменной в исходном тексте (use-site), а
    /// не позиция объявления. Позволяет индексу LSP найти узел по курсору.
    Variable(Rc<RefCell<VariableNode>>, Location),
    /// Ссылка на модель: `S(Ping)`.
    ///
    /// Второе поле - позиция **использования** (use-site), как у
    /// [`Variable`](ConditionNode::Variable). Без неё переход к декларации на имени
    /// модели невозможен: разрешение стирает позицию, и индексу LSP нечего сопоставить
    /// с курсором.
    Model(Rc<RefCell<ModelNode>>, Location),
    /// Имя состояния той же модели в условии (`x = Done`); 2-е поле - use-site для LSP.
    State(Rc<RefCell<StateNode>>, Location),
    /// Вариант перечисления (Ce4/NI6).
    ///
    /// Поля: `(определение перечисления, имя варианта, числовое значение варианта)`.
    EnumVariant(Rc<RefCell<EnumDefinitionNode>>, String, i128),
}

impl PartialEq for ConditionNode {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::None, Self::None) => true,
            (Self::Unresolved(a), Self::Unresolved(b)) => a == b,
            (Self::ArraySubscript(v1, n1), Self::ArraySubscript(v2, n2)) => v1 == v2 && n1 == n2,
            (Self::Parenthesis(a), Self::Parenthesis(b)) => a == b,
            (Self::BitAccess(a, ma), Self::BitAccess(b, mb)) => a == b && ma == mb,
            // Location (use-site) намеренно игнорируется: идентичность - семантическая
            (Self::Function(f1, args1, _), Self::Function(f2, args2, _)) => {
                f1 == f2 && args1 == args2
            }
            (Self::Not(a), Self::Not(b)) => a == b,
            (Self::Add(l1, r1), Self::Add(l2, r2)) => l1 == l2 && r1 == r2,
            (Self::Subtract(l1, r1), Self::Subtract(l2, r2)) => l1 == l2 && r1 == r2,
            (Self::And(l1, r1), Self::And(l2, r2)) => l1 == l2 && r1 == r2,
            (Self::Or(l1, r1), Self::Or(l2, r2)) => l1 == l2 && r1 == r2,
            (Self::Less(l1, r1), Self::Less(l2, r2)) => l1 == l2 && r1 == r2,
            (Self::More(l1, r1), Self::More(l2, r2)) => l1 == l2 && r1 == r2,
            (Self::LessEqual(l1, r1), Self::LessEqual(l2, r2)) => l1 == l2 && r1 == r2,
            (Self::MoreEqual(l1, r1), Self::MoreEqual(l2, r2)) => l1 == l2 && r1 == r2,
            (Self::Equal(l1, r1), Self::Equal(l2, r2)) => l1 == l2 && r1 == r2,
            (Self::NotEqual(l1, r1), Self::NotEqual(l2, r2)) => l1 == l2 && r1 == r2,
            (Self::Number(a), Self::Number(b)) => a == b,
            (Self::Duration(a), Self::Duration(b)) => a == b,
            (Self::After(a), Self::After(b)) => a == b,
            (Self::AfterTicks(a), Self::AfterTicks(b)) => a == b,
            (Self::AfterExpr(a), Self::AfterExpr(b)) => a == b,
            (Self::Rational(s1, n1), Self::Rational(s2, n2)) => s1 == s2 && n1 == n2,
            (Self::String(a), Self::String(b)) => a == b,
            (Self::Bool(a), Self::Bool(b)) => a == b,
            // Location (use-site) намеренно игнорируется
            (Self::Variable(v1, _), Self::Variable(v2, _)) => v1 == v2,
            (Self::Model(a, _), Self::Model(b, _)) => a == b,
            (Self::State(a, _), Self::State(b, _)) => a == b,
            _ => false,
        }
    }
}

impl ConditionNode {
    /// Безусловно ли ребро с этим условием.
    ///
    /// **Единственное место, где принимается это решение.** Прежде его
    /// принимали **пять** потребителей порознь - цели `c`, `st`, `rust`, `sv` и
    /// эталон, - и они разошлись: `rust` и `sv` считали безусловным ещё и
    /// [`ConditionNode::Unresolved`], то есть **условное** ребро с
    /// неразрешённым условием срабатывало у них всегда. Вывод при этом
    /// валиден - автомат другой, и молча (класс, ради которого 0236 изъяла ту
    /// же ветвь у цели `c`, но только у неё).
    ///
    /// **`Unresolved` безусловным не считается.** Неразрешённое условие значит "имя не
    /// найдено", а не "условия нет"; такой вход отсекает `SE-025` до генерации, и
    /// потому сегодня сюда не доходит. Но недостижимость держит **другая** фича: стоит
    /// ей ослабнуть - и потребитель, считающий `Unresolved` безусловным, начнёт молча
    /// менять автомат. Отдав узел печатнику, получаем громкий отказ (`RS-011`,
    /// `SV-002`, `CC-023`, `ST-011`) вместо тихой подмены.
    #[must_use]
    pub fn is_unconditional(&self) -> bool {
        matches!(self, ConditionNode::None)
    }
}

impl Eq for ConditionNode {}
