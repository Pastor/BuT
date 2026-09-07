//! Глубина дерева АСД: измерение и предел разбора.
//!
//! ## Как устроено
//!
//! Обход **итеративный**: рабочий стек живёт в куче, поэтому измерение глубины само по
//! себе стек не расходует (иначе лекарство повторяло бы болезнь). Раскрытие узла в
//! дочерние - [`children::push_children`], разбор там исчерпывающий (новый узел языка
//! валит сборку).

mod children;
mod dismantle;

pub(crate) use dismantle::dismantle;
// Раскрытие узла в дочерние переиспользует счёт покрытия конструкций
// (`crate::coverage`): второй обход дерева разошёлся бы с этим.
pub(crate) use children::push_children;

use crate::diagnostics::{Diagnostic, Location};
use crate::parser::ast;

/// Предельная глубина дерева АСД, принимаемая разбором.
///
/// Обоснование значения - в описании модуля.
pub(crate) const MAX_PARSE_DEPTH: usize = 96;

/// Предел разбора обязан быть строго выше семантического (`SE-062`).
///
/// Иначе внятная диагностика о вложенности стала бы недостижимой: разбор отвергал бы
/// программу раньше, чем семантика успевала назвать причину. Проверку держит
/// **компилятор**, а не тест: условие целиком известно на этапе сборки, и тест лишь
/// повторял бы его позже.
const _: () = assert!(
    crate::semantic::validate::depth::MAX_NESTING_DEPTH < MAX_PARSE_DEPTH,
    "предел разбора обязан быть выше семантического, иначе SE-062 недостижим"
);

/// Ссылка на узел АСД для итеративного обхода.
///
/// Перечисление намеренно покрывает **все** рекурсивные типы дерева, а не одни
/// выражения: глубину набирают и вложенные модели, и типы (`[[bit;2];2]`), и блоки
/// формул.
#[derive(Clone, Copy)]
pub(crate) enum NodeRef<'a> {
    /// Модель (в том числе вложенная).
    Model(&'a ast::Model),
    /// Элемент модели.
    Element(&'a ast::ModelElement),
    /// Определение состояния.
    State(&'a ast::StateDefine),
    /// Элемент состояния.
    StateElement(&'a ast::StateElement),
    /// Объявление переменной, константы, параметра или порта.
    Variable(&'a ast::VariableDefine),
    /// Определение функции.
    Function(&'a ast::FunctionDefine),
    /// Параметр функции.
    Parameter(&'a ast::Parameter),
    /// Тип.
    Type(&'a ast::Type),
    /// Оператор.
    Statement(&'a ast::Statement),
    /// Ветка `match`.
    MatchArm(&'a ast::MatchArm),
    /// Именованный аргумент вызова.
    NamedArgument(&'a ast::NamedArgument),
    /// Выражение.
    Expression(&'a ast::Expression),
    /// Условие перехода.
    Condition(&'a ast::Condition),
    /// Встроенная формула (`Guard` или `LTL`).
    InlineFormula(&'a ast::InlineFormulaDefine),
    /// Формула LTL.
    Ltl(&'a ast::LtlExpr),
    /// Блок формулы.
    FormulaBlock(&'a ast::FormulaBlock),
    /// Оператор формулы.
    FormulaStatement(&'a ast::FormulaStatement),
    /// Выражение формулы.
    FormulaExpression(&'a ast::FormulaExpression),
    /// Вызов функции формулы.
    FormulaFunction(&'a ast::FormulaFunction),
}

/// Пропускает дерево наружу, если его глубина в пределах [`MAX_PARSE_DEPTH`].
///
/// При превышении отдаёт диагностику `SY-005` (позиция + названный предел), а само
/// дерево **утилизирует без рекурсии**: производный `Drop` на такой глубине уронил бы
/// процесс ровно в момент отказа.
pub(crate) fn check(model: ast::Model) -> Result<ast::Model, Diagnostic> {
    match measure(&model, MAX_PARSE_DEPTH) {
        Ok(_) => Ok(model),
        Err(loc) => {
            let diagnostic = Diagnostic::parser_error(
                loc,
                format!(
                    "превышен предел вложенности разбора ({MAX_PARSE_DEPTH}): \
                     дерево программы вложено слишком глубоко"
                ),
            )
            .with_code("SY-005");
            dismantle(model);
            Err(diagnostic)
        }
    }
}

/// Измеряет глубину дерева, обрывая работу на превышении `limit`.
///
/// Возвращает фактическую глубину (если она не больше `limit`) либо позицию
/// **первого встреченного** узла глубже предела. "Первого" - в порядке обхода, а
/// не текста: для диагностики важно место, а не то, какое из глубоких мест
/// названо (их в таком вводе тысячи).
///
/// Для чистого замера (без предела) вызывается с `usize::MAX`.
pub(crate) fn measure(root: &ast::Model, limit: usize) -> Result<usize, Location> {
    let mut stack: Vec<(NodeRef<'_>, usize, Location)> = vec![(NodeRef::Model(root), 1, root.loc)];
    let mut children: Vec<NodeRef<'_>> = Vec::new();
    let mut deepest = 0;

    while let Some((node, depth, inherited)) = stack.pop() {
        // Позиция узла, а при её отсутствии - ближайшего известного предка: сообщение
        // без координат честнее выдуманных (приём модуля `SE-062`).
        let loc = node_loc(node).unwrap_or(inherited);
        if depth > limit {
            return Err(loc);
        }
        deepest = deepest.max(depth);

        children.clear();
        children::push_children(node, &mut children);
        stack.extend(children.drain(..).map(|child| (child, depth + 1, loc)));
    }

    Ok(deepest)
}

/// Позиция узла, если она у него есть.
///
/// У контейнеров (элемент модели, объявление, тип) собственной позиции либо нет, либо
/// она совпадает с позицией родителя - такие узлы отдают `None`, и обход наследует
/// позицию предка.
fn node_loc(node: NodeRef<'_>) -> Option<Location> {
    match node {
        NodeRef::Model(model) => Some(model.loc),
        NodeRef::State(state) => Some(state.loc),
        NodeRef::Function(function) => Some(function.loc),
        NodeRef::Parameter(parameter) => Some(parameter.loc),
        NodeRef::Statement(statement) => Some(statement.loc()),
        NodeRef::MatchArm(arm) => Some(arm.loc),
        NodeRef::NamedArgument(argument) => Some(argument.loc),
        NodeRef::Expression(expression) => Some(expression.loc()),
        NodeRef::Condition(condition) => Some(condition.loc()),
        NodeRef::Ltl(formula) => Some(ltl_loc(formula)),
        NodeRef::FormulaBlock(block) => Some(block.loc),
        NodeRef::FormulaFunction(function) => Some(function.loc),
        NodeRef::Element(_)
        | NodeRef::StateElement(_)
        | NodeRef::Variable(_)
        | NodeRef::Type(_)
        | NodeRef::InlineFormula(_)
        | NodeRef::FormulaStatement(_)
        | NodeRef::FormulaExpression(_) => None,
    }
}

/// Позиция формулы LTL.
fn ltl_loc(formula: &ast::LtlExpr) -> Location {
    match formula {
        ast::LtlExpr::True(loc)
        | ast::LtlExpr::False(loc)
        | ast::LtlExpr::Not(loc, _)
        | ast::LtlExpr::Next(loc, _)
        | ast::LtlExpr::Finally(loc, _)
        | ast::LtlExpr::Globally(loc, _)
        | ast::LtlExpr::And(loc, _, _)
        | ast::LtlExpr::Or(loc, _, _)
        | ast::LtlExpr::Until(loc, _, _)
        | ast::LtlExpr::Release(loc, _, _)
        | ast::LtlExpr::Implies(loc, _, _)
        | ast::LtlExpr::Parenthesis(loc, _) => *loc,
        ast::LtlExpr::Atom(identifier) => identifier.loc,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Разбирает исходник, минуя проверку глубины: измерению нужен именно
    /// неограниченный разбор, иначе тест проверял бы сам себя.
    fn parse_ast(src: &str) -> ast::Model {
        crate::parse_without_depth_limit(src, 0)
            .expect("исходник пробы обязан разбираться")
            .0
    }

    /// Глубина растёт ровно на один уровень за скобку.
    #[test]
    fn each_parenthesis_adds_exactly_one_level() {
        let depth_of = |parens: usize| {
            let src = format!(
                "var x: u8 := 0;\nstart S {{\n  always {{ x := {}1{}; }}\n  ref S: x = 9;\n}}\n",
                "(".repeat(parens),
                ")".repeat(parens)
            );
            measure(&parse_ast(&src), usize::MAX).expect("предела нет")
        };
        let base = depth_of(1);
        assert_eq!(depth_of(2), base + 1, "вторая скобка — ещё один уровень");
        assert_eq!(depth_of(5), base + 4, "пятая скобка — четвёртый сверх базы");
    }

    /// Цепочка без скобок углубляет дерево так же, как скобки.
    ///
    /// Именно поэтому счёт идёт по узлам, а не по балансу скобок: `1+1+...` даёт левое
    /// дерево той же глубины при нулевом балансе.
    #[test]
    fn operator_chain_grows_depth_too() {
        let src = format!(
            "var x: u8 := 0;\nstart S {{\n  always {{ x := {}; }}\n  ref S: x = 9;\n}}\n",
            vec!["1"; 40].join("+")
        );
        let depth = measure(&parse_ast(&src), usize::MAX).expect("предела нет");
        assert!(
            depth >= 40,
            "цепочка из 40 слагаемых обязана дать глубину не меньше 40, получено {depth}"
        );
    }

    /// Обрыв по пределу отдаёт позицию, а не только факт.
    #[test]
    fn limit_reports_location() {
        let src = format!(
            "var x: u8 := 0;\nstart S {{\n  always {{ x := {}1{}; }}\n  ref S: x = 9;\n}}\n",
            "(".repeat(MAX_PARSE_DEPTH * 2),
            ")".repeat(MAX_PARSE_DEPTH * 2)
        );
        let loc = measure(&parse_ast(&src), MAX_PARSE_DEPTH).expect_err("предел обязан сработать");
        assert!(
            matches!(loc, Location::Source(..)),
            "позиция обязана указывать на место в файле, получено {loc:?}"
        );
    }

    /// Обычная модель до предела не достаёт - с большим запасом.
    #[test]
    fn ordinary_model_is_far_below_the_limit() {
        let src = "var x: u8 := 0;\n\
                   fn step(a: u8) -> u8 { return a + 1; }\n\
                   start S {\n  \
                     always { if x < 3 { x := step(x); } }\n  \
                     ref S: x = 9;\n\
                   }\n";
        let depth = measure(&parse_ast(src), usize::MAX).expect("предела нет");
        assert!(
            depth * 2 < MAX_PARSE_DEPTH,
            "обычная модель обязана укладываться в половину предела, получено {depth}"
        );
    }

    /// Живой корпус лежит далеко от предела.
    ///
    /// Тест соразмерности: предел вводится ради порождённого текста, а не ради
    /// написанных спецификаций. Если корпус подберётся к границе, предел надо
    /// пересматривать - и узнать об этом лучше от теста, чем от пользователя.
    #[test]
    fn corpus_is_far_below_the_limit() {
        let examples = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("корень репозитория")
            .join("examples");
        let mut worst = (0usize, String::new());
        for entry in std::fs::read_dir(&examples).expect("каталог примеров") {
            let path = entry.expect("запись каталога").path();
            if path.extension().is_none_or(|ext| ext != "takt") {
                continue;
            }
            let source = std::fs::read_to_string(&path).expect("чтение примера");
            let Ok((ast, _)) = crate::parse(&source, 0) else {
                continue; // разбор примера проверяют другие тесты
            };
            let depth = measure(&ast, usize::MAX).expect("предела нет");
            if depth > worst.0 {
                worst = (depth, path.display().to_string());
            }
        }
        assert!(worst.0 > 0, "корпус примеров не прочитан: {examples:?}");
        assert!(
            worst.0 * 2 <= MAX_PARSE_DEPTH,
            "глубина корпуса подобралась к пределу: {} у {} (предел {MAX_PARSE_DEPTH})",
            worst.0,
            worst.1
        );
    }

    /// Измерение не рекурсивно: дерево, роняющее любую рекурсию по нему, измеряется
    /// штатно.
    #[test]
    fn measuring_a_very_deep_tree_does_not_recurse() {
        let src = format!(
            "var x: u8 := 0;\nstart S {{\n  always {{ x := {}; }}\n  ref S: x = 9;\n}}\n",
            vec!["1"; 5_000].join("+")
        );
        let ast = parse_ast(&src);
        let depth = measure(&ast, usize::MAX).expect("предела нет");
        assert!(
            depth > 5_000,
            "глубина обязана быть не меньше цепочки: {depth}"
        );
    }
}
