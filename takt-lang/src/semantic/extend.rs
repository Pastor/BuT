//! Построение и развёртка структуры [`Extend`] - реализации модели.
//!
//! Модуль предоставляет две публичные функции:
//! - [`unroll_extend_expression`] - разворачивает выражение в плоскую
//!   структуру [`Extend::Concatenation`] / [`Extend::Parallel`].

use crate::diagnostics::lang::keys;
use crate::msg;
use crate::diagnostics::{Diagnostic, Location};
use crate::parser::ast;
use crate::semantic::extend_args;
use crate::semantic::{ExpressionNode, ModelNode, StateNode, StateNodeKind};
use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

/// Реализация модели: описывает, как состояние или корневой автомат составлен из
/// именованных моделей.
///
/// - [`Unresolved`](Extend::Unresolved) - временная заглушка до второго прохода.
/// - [`Model`](Extend::Model) - ссылка на конкретную именованную модель.
/// - [`Parentless`](Extend::Parentless) - обёртка без родителя (скобки).
/// - [`Concatenation`](Extend::Concatenation) - последовательная компоновка `A + B`.
/// - [`Parallel`](Extend::Parallel) - параллельная компоновка `A | B`.
#[derive(Default, Debug, Clone)]
pub enum Extend {
    /// Реализация не задана (значение по умолчанию для безымянной корневой модели).
    #[default]
    None,
    /// "Сырое" АСД-выражение реализации, ожидающее разрешения на этапе stage1.
    Unresolved(ast::Expression),
    /// Ссылка на конкретную именованную модель.
    ///
    /// Второе поле - позиция **использования** (use-site): где имя модели написано, а
    /// не где она объявлена. Разрешение стирало её, из-за чего переход к декларации на
    /// имени модели был невозможен - узла под курсором просто не существовало.
    ///
    /// Третье поле - **аргументы инстанцирования** `M(Y := 200)`. Они привязаны к месту
    /// использования, а не к модели: одна и та же модель в двух местах настраивается
    /// по-разному. Пустой вектор - вызов без аргументов, поведение прежнее.
    Model(Rc<RefCell<ModelNode>>, Location, Vec<ParameterArgument>),
    /// Скобочная группировка: `(реализация)`.
    Parentless(Box<Extend>),

    /// Последовательная компоновка: `левое + правое + ...`.
    Concatenation(Vec<Box<Extend>>),
    /// Параллельная компоновка: `левое | правое | ...`.
    Parallel(Vec<Box<Extend>>),
}

/// Аргумент инстанцирования модели: `M(ИМЯ := ВЫРАЖЕНИЕ)`.
///
/// Значение уже **вычислено** константным вычислителем (`const_eval`) и понижено: за
/// границей семантики выражения аргумента не существует, есть литерал. Применяют его
/// потребители дерева (/05) - каждый своим печатником выражений, то есть общим для цели
/// путём.
#[derive(Debug, Clone)]
pub struct ParameterArgument {
    /// Имя параметра целевой модели.
    pub name: String,
    /// Позиция **имени** в аргументе - для диагностик о самом аргументе.
    pub loc: Location,
    /// Вычисленное значение - понижённый литерал.
    pub value: ExpressionNode,
}

/// Равенство аргумента - это равенство **настройки**: имя и значение. Позиция
/// игнорируется (тот же довод, что у [`Extend`] ниже, и та же причина).
///
/// Автовыведённое равенство включало `loc`, и это был дефект: цель `sv`
/// сравнивает наборы аргументов, чтобы отвергнуть **разные** настройки одной модели при
/// уплощении (`SV-016`), - а с позицией в равенстве два **текстуально одинаковых**
/// вызова `Tuner(gain := 100) | Tuner(gain := 100)` оказывались "разными наборами" и
/// получали отказ. Заявленное поведение ("одинаковые наборы законны") не выполнялось;
/// вскрыто прогоном всех целей в двух режимах,.
impl PartialEq for ParameterArgument {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.value == other.value
    }
}

impl Eq for ParameterArgument {}

/// Равенство реализаций **игнорирует позицию использования**.
///
/// Не стиль, а условие корректности. `Extend` сравнивается транзитивно:
/// `ModelNode`/`StateNode` сравнивают своё поле `implements`, а `ConditionNode`
/// сравнивает `Rc<RefCell<ModelNode>>`. Оставь позицию в автовыведённом равенстве - и
/// две ссылки на **одну и ту же** модель из разных мест текста стали бы разными узлами.
/// Тот же приём и по той же причине уже применён к
/// [`ConditionNode::Variable`](crate::semantic::ConditionNode::Variable) ("Location
/// (use-site) намеренно игнорируется").
///
/// Узел определяется тем, **на что** он ссылается, а не тем, где написан.
impl PartialEq for Extend {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Extend::None, Extend::None) => true,
            (Extend::Unresolved(a), Extend::Unresolved(b)) => a == b,
            // Позиция (use-site) намеренно игнорируется - см. док выше. А вот аргументы
            // инстанцирования игнорировать нельзя: `M(Y := 100)` и `M(Y := 200)`
            // ссылаются на одну модель, но это разные реализации. Приравняв их, мы
            // получили бы один экземпляр там, где автор написал два разных.
            (Extend::Model(a, _, aa), Extend::Model(b, _, ba)) => a == b && aa == ba,
            (Extend::Parentless(a), Extend::Parentless(b)) => a == b,
            (Extend::Concatenation(a), Extend::Concatenation(b)) => a == b,
            (Extend::Parallel(a), Extend::Parallel(b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for Extend {}

impl Extend {
    /// Возвращает человекочитаемое имя варианта или имя модели.
    pub fn name(&self) -> String {
        match self {
            Extend::None => "None".to_string(),
            Extend::Unresolved(_) => "Unresolved".to_string(),
            Extend::Model(model, _, _) => model.clone().borrow().name().to_string(),
            Extend::Parentless(implement) => implement.name(),
            Extend::Concatenation(_) => "Concatenation".to_string(),
            Extend::Parallel(_) => "Parallel".to_string(),
        }
    }
}

impl Display for Extend {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Extend::None => write!(f, "None"),
            Extend::Unresolved(_) => write!(f, "Unresolved"),
            Extend::Model(model, _, _) => {
                write!(f, "{}", model.borrow().name.clone().unwrap_or_default())
            }
            Extend::Parentless(extends) => write!(f, "({})", extends),
            Extend::Concatenation(extends) => write!(
                f,
                "{}",
                extends
                    .iter()
                    .map(|i| i.to_string())
                    .collect::<Vec<String>>()
                    .join(" + ")
            ),
            Extend::Parallel(implements) => write!(
                f,
                "{}",
                implements
                    .iter()
                    .map(|i| i.to_string())
                    .collect::<Vec<String>>()
                    .join(" | ")
            ),
        }
    }
}

/// Плоская конкатенация: операнд, уже являющийся цепочкой, разворачивается.
fn concatenate(left: Extend, right: Extend) -> Extend {
    let mut items: Vec<Box<Extend>> = Vec::new();
    for side in [left, right] {
        match side {
            Extend::Concatenation(seq) => items.extend(seq),
            other => items.push(Box::new(other)),
        }
    }
    Extend::Concatenation(items)
}

/// Плоское объединение: операнд, уже являющийся параллелью, разворачивается.
fn parallelize(left: Extend, right: Extend) -> Extend {
    let mut items: Vec<Box<Extend>> = Vec::new();
    for side in [left, right] {
        match side {
            Extend::Parallel(p) => items.extend(p),
            other => items.push(Box::new(other)),
        }
    }
    Extend::Parallel(items)
}

/// Владелец вложенной модели с таким именем - для подсказки к `SE-001`.
///
/// `import "lib.takt";` вносит к импортёру **обёртку** по имени файла, а объявленная
/// внутри неё модель снаружи не видна. Запись `start Main = Helper;` естественна и
/// получает "Модель 'Helper' не найдена" - утверждение, которое автор читает как "такой
/// модели нет" и идёт переносить состояния на верхний уровень. На деле
/// форма есть: выборочный импорт `import { Helper } from "lib.takt";` работает - это
/// проверено прогоном, и именно он снял с фичи её первоначальный объём ("ввести
/// квалифицированную ссылку").
///
/// Обход - **один уровень** вложенности и по `BTreeMap`, то есть детерминированный.
/// Глубже подсказка молчит: граница названа, а не забыта.
fn nested_owner_of(
    model: &Rc<RefCell<ModelNode>>,
    name: &str,
) -> Option<(String, crate::diagnostics::Location)> {
    let scope = model.borrow();
    for (owner_name, owner) in &scope.models {
        if let Some(nested) = owner.borrow().models.get(name) {
            return Some((owner_name.clone(), nested.borrow().loc));
        }
    }
    // Имя может быть объявлено уровнем выше - ищем там же, где искал `search_model`,
    // чтобы подсказка не зависела от места записи.
    let upper = scope.upper.as_ref().and_then(|w| w.upgrade())?;
    drop(scope);
    nested_owner_of(&upper, name)
}

/// Строит `SE-001` - и добавляет подсказку, если модель есть внутри соседней.
fn model_not_found(model: &Rc<RefCell<ModelNode>>, id: &ast::Identifier) -> Diagnostic {
    let diagnostic =
        Diagnostic::error(id.loc, msg!(keys::SE_001_MODEL_NOT_FOUND, name = id.name)).with_code("SE-001");
    match nested_owner_of(model, &id.name) {
        // Позиция заметки - Объявление вложенной модели, то есть чужой файл. Координата
        // там не печатается, и текст остаётся чистым - а сама позиция доезжает до
        // редактора, который умеет открыть нужный файл.
        Some((owner, nested_loc)) => diagnostic.with_note(
            nested_loc,
            format!(
                "модель '{}' объявлена внутри модели '{owner}': подключение файла целиком \
                 вносит только её, а вложенные модели снаружи не видны. Подключите нужную \
                 выборочно: import {{ {} }} from \"…\";",
                id.name, id.name
            ),
        ),
        None => diagnostic,
    }
}

/// Разворачивает **АСД**-выражение расширения прямо в [`Extend`].
fn unroll_ast_extend(
    expr: ast::Expression,
    model: Rc<RefCell<ModelNode>>,
) -> Result<Extend, Diagnostic> {
    match expr {
        ast::Expression::Variable(id) => {
            let found = model
                .as_ref()
                .borrow()
                .search_model(&id.name)
                .ok_or_else(|| model_not_found(&model, &id))?;
            // Позиция имени - то, ради чего разворот идёт по АСД.
            Ok(Extend::Model(found, id.loc, Vec::new()))
        }
        // Инстанцирование с аргументами: `M(Y := 200)`. Грамматика видит здесь вызов
        // функции - моделью его делает позиция (выражение реализации), поэтому имя
        // ищется среди моделей.
        ast::Expression::Function(call_loc, id, args) => {
            let found = model
                .as_ref()
                .borrow()
                .search_model(&id.name)
                .ok_or_else(|| model_not_found(&model, &id))?;
            let arguments =
                extend_args::parse_arguments(&found, &id.name, &args, call_loc, &model)?;
            Ok(Extend::Model(found, id.loc, arguments))
        }
        ast::Expression::Parenthesis(_, inner) => unroll_ast_extend(*inner, model),
        ast::Expression::Add(_, left, right) => {
            let left = unroll_ast_extend(*left, model.clone())?;
            let right = unroll_ast_extend(*right, model)?;
            Ok(concatenate(left, right))
        }
        ast::Expression::BitwiseOr(_, left, right) => {
            let left = unroll_ast_extend(*left, model.clone())?;
            let right = unroll_ast_extend(*right, model)?;
            Ok(parallelize(left, right))
        }
        // Прочие формы реализацией быть не могут. Диагностика несёт код и позицию:
        // `Debug` узла АСД был бы сообщением о внутреннем устройстве, а не об ошибке
        // автора.
        other => Err(Diagnostic::error(
            arg_loc(&other).unwrap_or(Location::Implicit),
            msg!(keys::SE_081_IMPLEMENTATION_FORM),
        )
        .with_code("SE-081")),
    }
}

/// Позиция выражения, если она у варианта есть (для диагностики выше).
fn arg_loc(expr: &ast::Expression) -> Option<Location> {
    match expr {
        ast::Expression::Variable(id) => Some(id.loc),
        ast::Expression::Assign(loc, _, _)
        | ast::Expression::Number(loc, _)
        | ast::Expression::Function(loc, _, _)
        | ast::Expression::Parenthesis(loc, _) => Some(*loc),
        _ => None,
    }
}

/// Разворачивает семантическое выражение расширения в плоскую структуру [`Extend`],
/// объединяя цепочки `+` в [`Extend::Concatenation`] и `|` в [`Extend::Parallel`].
pub(crate) fn unroll_extend_expression(
    expression: ExpressionNode,
    model: Rc<RefCell<ModelNode>>,
) -> Result<Extend, Diagnostic> {
    let model = Rc::clone(&model);
    match expression {
        // Путь продукта: реализация приходит сырым АСД (`tree.rs`, stage1).
        ExpressionNode::Unresolved(expr) => unroll_ast_extend(expr, model),
        // Уже разрешённая модель: позиции использования у неё нет и взять негде.
        // Разрешённая модель: ни позиции использования, ни аргументов у неё нет и взять
        // негде - этим путём идут узлы, собранные кодогеном.
        ExpressionNode::Model(model) => Ok(Extend::Model(
            Rc::clone(&model),
            Location::Implicit,
            Vec::new(),
        )),
        ExpressionNode::Parenthesis(expression) => unroll_extend_expression(*expression, model),
        ExpressionNode::Add(left, right) => {
            let left = unroll_extend_expression(*left, model.clone())?;
            let right = unroll_extend_expression(*right, model)?;
            Ok(concatenate(left, right))
        }
        ExpressionNode::BitwiseOr(left, right) => {
            let left = unroll_extend_expression(*left, model.clone())?;
            let right = unroll_extend_expression(*right, model)?;
            Ok(parallelize(left, right))
        }
        // Защитная ветвь без `Debug`-дампа: форму реализации проверяет `SE-081` раньше
        // и называет допустимые записи, поэтому сюда доходит только то, что семантика
        // уже пропустила.
        _ => Err(crate::semantic::internal::internal(
            "выражение реализации не сводится к композиции моделей",
        )),
    }
}

// -- Реализация модели (`model M = A | B { ... }`) - ------------------

/// Имя синтетического состояния, в которое разворачивается реализация модели.
///
/// Коллизии с пользовательским состоянием быть не может **по построению**: модель с
/// реализацией и собственными состояниями отвергается [`SE-101`].
///
/// [`SE-101`]: expand_model_implement
const MODEL_IMPLEMENT_STATE: &str = "Implement";

/// Разворачивает `model M = A | B { ... }` в модель с синтетическим стартовым
/// состоянием-реализацией.
///
/// # Ошибки
///
/// **`SE-101`**, если у модели есть и реализация, и собственные состояния: модель не
/// может быть одновременно композицией и автоматом. Без отказа такая запись молча
/// теряет реализацию - исполняется только собственное состояние, а `= A | B` не доходит
/// ни до эталона, ни до целей.
pub(super) fn expand_model_implement(model: &Rc<RefCell<ModelNode>>) -> Result<(), Diagnostic> {
    let Extend::Unresolved(expr) = model.borrow().implements.clone() else {
        return Ok(());
    };
    let loc = model.borrow().loc;
    if let Some((name, state_loc)) = model
        .borrow()
        .states
        .iter()
        .map(|(name, state)| (name.clone(), state_location(state)))
        .next()
    {
        return Err(Diagnostic::error(
            loc,
            msg!(
                keys::SE_101_IMPLEMENTATION_WITH_OWN_STATE,
                model = model.borrow().name.clone().unwrap_or_default(),
                name = name
            ),
        )
        .with_code("SE-101")
        .with_note(state_loc, msg!(keys::SE_101_OWN_STATE_NOTE, name = name)));
    }
    let state = StateNode::Implement {
        upper: Some(Rc::downgrade(model)),
        loc,
        named_blocks: Vec::new(),
        name: MODEL_IMPLEMENT_STATE.to_string(),
        references: Vec::new(),
        implements: Extend::Unresolved(expr),
        next: None,
        kind: StateNodeKind::Start,
        formulas: Vec::new(),
    };
    let mut borrowed = model.borrow_mut();
    borrowed
        .states
        .insert(MODEL_IMPLEMENT_STATE.to_string(), state);
    borrowed.implements = Extend::None;
    Ok(())
}

/// Позиция состояния - для примечания диагностики.
fn state_location(state: &StateNode) -> Location {
    match state {
        StateNode::Simple { loc, .. } | StateNode::Implement { loc, .. } => *loc,
        StateNode::Unresolved => Location::Builtin,
    }
}

/// Модели, которыми реализованы состояния этой модели.
///
/// Они **не** лежат в `ModelNode::models`: там объявленные внутри, а реализация
/// ссылается на модель, объявленную где угодно в файле. Признаки целей ("нужен ли
/// указатель на корень" у `c`, "какие общие переменные передать" у `rust`) обязаны
/// считать их детьми по вызову: тикает их та же функция и передаёт им своё состояние.
///
/// Пропуск даёт вывод, который отвергают чужие инструменты при нулевом коде возврата
/// `taktc`: `cc` - "use of undeclared identifier 'main'", `rustc` - "cannot find value
/// `shared` in this scope".
pub fn implementation_children(
    model: &crate::semantic::ModelNode,
) -> Vec<std::rc::Rc<std::cell::RefCell<crate::semantic::ModelNode>>> {
    let mut found = Vec::new();
    for state in model.states.values() {
        if let crate::semantic::StateNode::Implement { implements, .. } = state {
            collect_extend_models(implements, &mut found);
        }
    }
    found
}

/// Собирает модели из дерева реализации ([`Extend`]).
///
/// Разбор **исчерпывающий**: пропущенная форма означала бы "ребёнка не видно", то есть
/// отказ чужого инструмента при нулевом коде возврата.
fn collect_extend_models(
    extend: &Extend,
    found: &mut Vec<std::rc::Rc<std::cell::RefCell<crate::semantic::ModelNode>>>,
) {
    match extend {
        Extend::Model(rc, _, _) => found.push(std::rc::Rc::clone(rc)),
        Extend::Parentless(inner) => collect_extend_models(inner, found),
        Extend::Concatenation(items) | Extend::Parallel(items) => {
            for item in items {
                collect_extend_models(item, found);
            }
        }
        Extend::None | Extend::Unresolved(_) => {}
    }
}

#[cfg(test)]
mod tests {
    use crate::diagnostics::Location;
    use crate::parse;
    use crate::parser::ast;
    use crate::semantic::extend::{Extend, unroll_extend_expression};
    use crate::semantic::test_constants::tests::SRC;
    use crate::semantic::test_constants::tests::model_node;
    use crate::semantic::tree::construct_model;
    use crate::semantic::{ExpressionNode, StateNode};

    #[test]
    fn test_unroll_implement_expression() {
        let (ast, _) = parse(SRC, 0).unwrap();
        let model_rc = construct_model(&ast, None, &[]).unwrap();

        let implement = unroll_extend_expression(
            ExpressionNode::Unresolved(ast::Expression::Variable(ast::Identifier::new("A"))),
            model_rc.clone(),
        )
        .unwrap();
        assert!(matches!(implement, Extend::Model(_, _, _)));
        let implement = unroll_extend_expression(
            ExpressionNode::Unresolved(ast::Expression::BitwiseOr(
                Location::Implicit,
                Box::new(ast::Expression::Variable(ast::Identifier::new("A"))),
                Box::new(ast::Expression::Variable(ast::Identifier::new("B"))),
            )),
            model_rc.clone(),
        )
        .unwrap();
        assert!(matches!(implement, Extend::Parallel(_)));
        // start Entry = A | B | (A + B) -> Parallel([A, B, Sequence([A, B])])
        let implement = unroll_extend_expression(
            ExpressionNode::Unresolved(ast::Expression::BitwiseOr(
                Location::Implicit,
                Box::new(ast::Expression::BitwiseOr(
                    Location::Implicit,
                    Box::new(ast::Expression::Variable(ast::Identifier::new("A"))),
                    Box::new(ast::Expression::Variable(ast::Identifier::new("B"))),
                )),
                Box::new(ast::Expression::Parenthesis(
                    Location::Implicit,
                    Box::new(ast::Expression::Add(
                        Location::Implicit,
                        Box::new(ast::Expression::Variable(ast::Identifier::new("A"))),
                        Box::new(ast::Expression::Variable(ast::Identifier::new("B"))),
                    )),
                )),
            )),
            model_rc.clone(),
        )
        .unwrap();
        assert_eq!(
            implement,
            Extend::Parallel(vec![
                Box::new(Extend::Model(
                    model_rc.borrow().search_model("A").unwrap(),
                    Location::Implicit,
                    Vec::new(),
                )),
                Box::new(Extend::Model(
                    model_rc.borrow().search_model("B").unwrap(),
                    Location::Implicit,
                    Vec::new(),
                )),
                Box::new(Extend::Concatenation(vec![
                    Box::new(Extend::Model(
                        model_rc.borrow().search_model("A").unwrap(),
                        Location::Implicit,
                        Vec::new(),
                    )),
                    Box::new(Extend::Model(
                        model_rc.borrow().search_model("B").unwrap(),
                        Location::Implicit,
                        Vec::new(),
                    )),
                ]))
            ])
        );
    }

    #[test]
    fn test_unroll_implement_expressions() {
        // Реализация в дереве плоская: состояние с `+` остаётся
        // Extend::Concatenation(items) со списком элементов - упаковки в синтетическую
        // модель нет (решение, разбор - ). unroll_extend_expression раскрывает цепочки
        // + и () в плоский Concatenation: A + (B + C) + D -> Concatenation([A, B, C,
        // D]) (скобки прозрачны).
        let (ast, _) = parse(SRC, 0).unwrap();
        let model_rc = construct_model(&ast, None, &[]).unwrap();

        // Next1..Next10: верхний уровень - конкатенация -> плоский Concatenation.
        // Количество элементов = ожидаемое число ступеней.
        let seq_states_with_item_count = [
            ("Next1", 3usize), // A + B + (A|B) -> [A, B, Par]
            ("Next2", 3),      // A + (B|A) + B -> [A, Par, B]
            ("Next3", 4),      // A + (B + A) + B (скобки прозрачны) -> [A, B, A, B]
            ("Next4", 4),      // A + (B + A) + (B|A) -> [A, B, A, Par]
            ("Next5", 3),      // (A|B) + (A + B) -> [Par, A, B]
            ("Next6", 4),      // (A|B) + (A + B) + (A|B) -> [Par, A, B, Par]
            ("Next7", 6),      // ... + (A + B) -> [Par,A,B,Par,A,B]
            ("Next8", 7),      // ... + (A|B) -> + Par
            ("Next9", 9),      // ... + (A + B) -> + A, B
            ("Next10", 11),    // ... + (A + B) -> + A, B
        ];
        for (name, expected_items) in seq_states_with_item_count {
            let state = model_rc.borrow().search_state(name).unwrap();
            let StateNode::Implement { ref implements, .. } = *state.borrow() else {
                panic!("State {name} is not an Implement node")
            };
            let Extend::Concatenation(items) = implements else {
                panic!("State {name}: ожидался Extend::Concatenation, получили: {implements}");
            };
            assert_eq!(
                items.len(),
                expected_items,
                "State {name}: конкатенация должна содержать {expected_items} элементов, содержит {}",
                items.len()
            );
        }

        // Entry = A | B | (A + B): верхний уровень - параллель -> Parallel.
        {
            let state = model_rc.borrow().search_state("Entry").unwrap();
            let StateNode::Implement { ref implements, .. } = *state.borrow() else {
                panic!("State Entry is not an Implement node")
            };
            assert!(
                matches!(implements, Extend::Parallel(_)),
                "Entry = A | B | (A + B): ожидался Extend::Parallel, получили: {implements}"
            );
        }
    }

    #[test]
    fn test_extend_name() {
        let model = model_node("MyModel", None);

        assert_eq!(Extend::None.name(), "None");
        assert_eq!(
            Extend::Unresolved(ast::Expression::Variable(ast::Identifier::new("X"))).name(),
            "Unresolved"
        );
        assert_eq!(
            Extend::Model(model.clone(), Location::Implicit, Vec::new()).name(),
            "MyModel"
        );
        assert_eq!(
            Extend::Parentless(Box::new(Extend::Model(
                model.clone(),
                Location::Implicit,
                Vec::new()
            )))
            .name(),
            "MyModel"
        );
        assert_eq!(
            Extend::Concatenation(vec![Box::new(Extend::Model(
                model.clone(),
                Location::Implicit,
                Vec::new(),
            ))])
            .name(),
            "Concatenation"
        );
        assert_eq!(
            Extend::Parallel(vec![Box::new(Extend::Model(
                model.clone(),
                Location::Implicit,
                Vec::new(),
            ))])
            .name(),
            "Parallel"
        );
    }

    #[test]
    fn test_extend_display() {
        let a = model_node("A", None);
        let b = model_node("B", None);

        assert_eq!(format!("{}", Extend::None), "None");
        assert_eq!(
            format!(
                "{}",
                Extend::Unresolved(ast::Expression::Variable(ast::Identifier::new("X")))
            ),
            "Unresolved"
        );
        assert_eq!(
            format!(
                "{}",
                Extend::Model(a.clone(), Location::Implicit, Vec::new())
            ),
            "A"
        );
        assert_eq!(
            format!(
                "{}",
                Extend::Parentless(Box::new(Extend::Model(
                    a.clone(),
                    Location::Implicit,
                    Vec::new()
                )))
            ),
            "(A)"
        );
        assert_eq!(
            format!(
                "{}",
                Extend::Concatenation(vec![
                    Box::new(Extend::Model(a.clone(), Location::Implicit, Vec::new())),
                    Box::new(Extend::Model(b.clone(), Location::Implicit, Vec::new())),
                ])
            ),
            "A + B"
        );
        assert_eq!(
            format!(
                "{}",
                Extend::Parallel(vec![
                    Box::new(Extend::Model(a.clone(), Location::Implicit, Vec::new())),
                    Box::new(Extend::Model(b.clone(), Location::Implicit, Vec::new())),
                ])
            ),
            "A | B"
        );
    }

    #[test]
    fn test_unroll_model_not_found() {
        let (ast, _) = parse(SRC, 0).unwrap();
        let model_rc = construct_model(&ast, None, &[]).unwrap();

        let result = unroll_extend_expression(
            ExpressionNode::Unresolved(ast::Expression::Variable(ast::Identifier::new(
                "NonExistent",
            ))),
            model_rc.clone(),
        );
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.message.contains("NonExistent"));
    }

    #[test]
    fn test_unroll_unsupported_expression() {
        let model = model_node("Root", None);

        // ExpressionNode::BitwiseAnd не поддерживается - должна вернуть ошибку
        let result = unroll_extend_expression(
            ExpressionNode::BitwiseAnd(
                Box::new(ExpressionNode::Model(model.clone())),
                Box::new(ExpressionNode::Model(model.clone())),
            ),
            model.clone(),
        );
        assert!(result.is_err());
    }
}
