//! Отказ цели `c` на неразрешённом узле - единая воронка.
//!
//! # Устройство
//!
//! Код и текст диагностики собираются здесь один раз; печатники воронку лишь зовут.
//! Второй конструктор разошёлся бы формулировкой с первым молча. Устройство то же, что у
//! `format::unsupported(loc, вид)` с кодом `FM-001`: один код, вид узла назван словом,
//! позиция берётся из АСД-узла, лежащего в полезной нагрузке `Unresolved`.
//!
//! **Не всякий `Unresolved` - дефект.** Правая часть паттерна `S(Модель) = Состояние`
//! приходит в цель неразрешённой по инварианту языка: прохода `resolve_state_references`
//! в проекте нет. Её разбирает `c_expr::condition::generate_state_comparison` до общего
//! печатника, и до этой воронки она не доходит. Заводя новую форму, проверь порядок:
//! сначала разбор законной формы, потом отказ.

use crate::diagnostics::lang::keys;
use crate::diagnostics::{Diagnostic, Location};
use crate::msg;

/// Вид узла, дошедшего до печатника цели `c` неразрешённым.
///
/// Перечисление, а не строка: тест фичи (`tests` ниже) обязан **перечислить** виды и
/// упасть списком, если какой-то перестал отвечать `CC-023`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::generator::c) enum UnresolvedNode {
    /// Условие перехода либо охранной формулы.
    Condition,
    /// Выражение в теле блока или функции.
    Expression,
    /// Оператор тела блока или функции.
    Statement,
    /// Выражения нет вовсе (`ExpressionNode::None`) - узел пуст, полезной нагрузки, а с
    /// нею и позиции, у него нет.
    EmptyExpression,
    /// Переменная не разрешена (`VariableNode::Unresolved`).
    Variable,
    /// У порта не разрешён владелец: слабая ссылка `upper` не поднимается.
    ///
    /// Второе поле - место, где это обнаружено (чтение, запись, доступ к биту): у
    /// самого узла позиции нет, и без уточнения три разных дефекта дали бы одно
    /// неразличимое сообщение.
    PortOwner(&'static str),
    /// Определение функции не разрешено (`FunctionDefinitionNode::Unresolved`).
    ///
    /// Второе поле - имя функции, если оно известно.
    Function(Option<String>),
    /// Значение константы не вычислено к моменту генерации.
    ///
    /// Второе поле - имя константы.
    ConstantValue(String),
}

impl UnresolvedNode {
    /// Название вида узла для текста диагностики (с согласованным родом).
    pub(in crate::generator::c) fn phrase(&self) -> String {
        match self {
            UnresolvedNode::Condition => msg!(keys::CC_023_NODE_CONDITION),
            UnresolvedNode::Expression => msg!(keys::CC_023_NODE_EXPRESSION),
            UnresolvedNode::Statement => msg!(keys::CC_023_NODE_STATEMENT),
            UnresolvedNode::EmptyExpression => msg!(keys::CC_023_NODE_EMPTY_EXPRESSION),
            UnresolvedNode::Variable => msg!(keys::CC_023_NODE_VARIABLE),
            UnresolvedNode::PortOwner(where_) => {
                msg!(keys::CC_023_NODE_PORT_OWNER, r#where = where_)
            }
            UnresolvedNode::Function(Some(name)) => {
                msg!(keys::CC_023_NODE_FUNCTION_NAMED, name = name)
            }
            UnresolvedNode::Function(None) => msg!(keys::CC_023_NODE_FUNCTION),
            UnresolvedNode::ConstantValue(name) => {
                msg!(keys::CC_023_NODE_CONST_VALUE, name = name)
            }
        }
    }

    /// Все виды - для теста (перечисление обязано быть полным).
    #[cfg(test)]
    pub(in crate::generator::c) fn all() -> Vec<UnresolvedNode> {
        vec![
            UnresolvedNode::Condition,
            UnresolvedNode::Expression,
            UnresolvedNode::Statement,
            UnresolvedNode::EmptyExpression,
            UnresolvedNode::Variable,
            UnresolvedNode::PortOwner("чтение"),
            UnresolvedNode::Function(Some("pid_step".to_string())),
            UnresolvedNode::ConstantValue("LIMIT".to_string()),
        ]
    }
}

/// Строит отказ печати неразрешённого узла - диагностику **`CC-023`**.
///
/// `loc` - позиция **самого узла** (`ast::Condition::loc()`, `ast::Expression::loc()`,
/// `ast::Statement::loc()`), а не места, где отказ обнаружен: в пачке диагностик
/// сообщение без координаты бесполезно.
///
/// Состояние, о котором сообщает `CC-023`, из корректной программы
/// **недостижимо** - его отсекает семантика (`SE-025`/`SE-003`). Это защита в
/// глубину: недостижимость держит **другая** фича, и каждая новая конструкция
/// языка способна открыть путь снова. Молчание в этом месте уже стоило проекту
/// дефекта.
pub(in crate::generator::c) fn refuse(loc: Location, node: UnresolvedNode) -> Diagnostic {
    Diagnostic::error(loc, msg!(keys::CC_023_REFUSAL, what = node.phrase())).with_code("CC-023")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::generator::c::c_expr::{
        generate_code_block, generate_condition_expr, generate_stmt_expression,
    };
    use crate::generator::c::c_map::CMap;
    use crate::generator::indent::Printer;
    use crate::parser::ast;
    use crate::semantic::minimap::Element;
    use crate::semantic::{ConditionNode, ExpressionNode, StateNode, StatementNode};
    use crate::{parse, semantic};

    /// Позиция, которую несёт подложенный узел: по ней сверяем, что диагностика
    /// указывает на **узел**, а не на место обнаружения.
    const PROBE: Location = Location::Source(7, 11, 13);

    /// Минимальные карта и владелец для вызова печатников (образец `c_source`).
    fn map_and_owner(src: &str) -> (CMap, Element) {
        let (ast_model, _) = parse(src, 0).expect("разбор");
        let model_rc = semantic::tree::construct_model(&ast_model, None, &[]).expect("дерево");
        model_rc.borrow_mut().name = Some("Probe".to_string());
        let model = model_rc.borrow();
        let map = CMap::new(model.name(), &model, true).expect("карта");
        let owner = Element::Model {
            name: map.root_name().clone(),
            states: map.states().clone(),
            start: map.start().clone(),
        };
        (map, owner)
    }

    /// Неразрешённое условие с известной позицией.
    fn unresolved_condition() -> ConditionNode {
        ConditionNode::Unresolved(ast::Condition::Number(PROBE, 1))
    }

    /// Модель-заглушка: сама по себе корректна, узлы подкладываются тестом.
    const SRC: &str = "var lev: u8 := 0;\nstart Run { always { lev := lev + 1; } }\n";

    /// Неразрешённое условие даёт `CC-023` с позицией узла.
    #[test]
    fn unresolved_condition_refuses_with_position() {
        let (map, owner) = map_and_owner(SRC);
        let diagnostic = generate_condition_expr(&unresolved_condition(), &map, &owner)
            .expect_err("ожидался отказ на неразрешённом условии");
        assert_eq!(diagnostic.code.as_deref(), Some("CC-023"));
        assert_eq!(diagnostic.loc, PROBE);
        assert!(
            diagnostic.message.contains("неразрешённое условие"),
            "текст не называет вид узла: {}",
            diagnostic.message
        );
    }

    /// Безусловный переход (`ConditionNode::None`) даёт пустую строку: отказ отделён от
    /// штатного случая, а не заменил его.
    #[test]
    fn absent_condition_still_prints_empty() {
        let (map, owner) = map_and_owner(SRC);
        let text = generate_condition_expr(&ConditionNode::None, &map, &owner).expect("не отказ");
        assert!(text.is_empty(), "ожидалась пустая строка, получено: {text}");
    }

    /// Неразрешённый оператор даёт `CC-023`, а не молчаливый пропуск.
    ///
    /// Пропуск здесь - потеря оператора при рапорте об успехе.
    #[test]
    fn unresolved_statement_refuses_with_position() {
        let (map, owner) = map_and_owner(SRC);
        let mut buf = String::new();
        let mut printer = Printer::new(4, &mut buf);
        let stmt = StatementNode::Unresolved(ast::Statement::Continue(PROBE));
        let diagnostic = generate_code_block(&mut printer, &map, &owner, vec![], &stmt, true)
            .expect_err("ожидался отказ на неразрешённом операторе");
        assert_eq!(diagnostic.code.as_deref(), Some("CC-023"));
        assert_eq!(diagnostic.loc, PROBE);
        assert!(
            diagnostic.message.contains("неразрешённый оператор"),
            "текст не называет вид узла: {}",
            diagnostic.message
        );
        assert!(buf.is_empty(), "в вывод попал текст: {buf}");
    }

    /// Неразрешённое выражение даёт `CC-023` с позицией.
    #[test]
    fn unresolved_expression_refuses_with_position() {
        let (map, owner) = map_and_owner(SRC);
        let mut buf = String::new();
        let mut printer = Printer::new(4, &mut buf);
        let expr = ExpressionNode::Unresolved(ast::Expression::Number(PROBE, 1));
        let diagnostic = generate_stmt_expression(&mut printer, &map, &owner, vec![], &expr, true)
            .expect_err("ожидался отказ на неразрешённом выражении");
        assert_eq!(diagnostic.code.as_deref(), Some("CC-023"));
        assert_eq!(diagnostic.loc, PROBE);
        assert!(
            diagnostic.message.contains("неразрешённое выражение"),
            "текст не называет вид узла: {}",
            diagnostic.message
        );
    }

    /// Ребро с неразрешённым условием не печатается безусловным переходом: ответ -
    /// `CC-018` с позицией ребра и причиной `CC-023` заметкой.
    #[test]
    fn unresolved_edge_condition_is_not_an_unconditional_transition() {
        let (ast_model, _) = parse(
            "var lev: u8 := 0;\n\
             start Run { ref Done: lev < 3; }\n\
             state Done { always { lev := 1; } }\n",
            0,
        )
        .expect("разбор");
        let model_rc = semantic::tree::construct_model(&ast_model, None, &[]).expect("дерево");
        model_rc.borrow_mut().name = Some("Probe".to_string());
        // Подкладываем неразрешённое условие в ребро: из исходника такое дерево не
        // построить - `construct_model` отвергает вход `SE-025`.
        {
            let mut model = model_rc.borrow_mut();
            let state = model.states.get_mut("Run").expect("состояние Run");
            let StateNode::Simple { references, .. } = state else {
                panic!("ожидалось простое состояние");
            };
            references[0].cond = unresolved_condition();
        }
        let model = model_rc.borrow();
        let map = CMap::new(model.name(), &model, true).expect("карта");
        let diagnostic = crate::generator::c::c_source::generate_source(map.get_filename(), &map)
            .expect_err("ожидался отказ на ребре с неразрешённым условием");
        assert_eq!(diagnostic.code.as_deref(), Some("CC-018"));
        assert!(
            diagnostic
                .notes
                .iter()
                .any(|n| n.message.contains("CC-023")),
            "причина не приложена заметкой: {:?}",
            diagnostic.notes
        );
    }

    /// Каждый вид узла из [`UnresolvedNode::ALL`] обязан иметь своё название в тексте и
    /// отдавать `CC-023`.
    ///
    /// Падает списком: новый вид, забытый в `phrase`, называется поимённо. Проверять
    /// "программа падает" здесь нечем - дерево с неразрешённым узлом из исходника не
    /// построить, `construct_model` отвергает такой вход кодами `SE-025` и `SE-003`, -
    /// поэтому мера теста в том, что каждая ветвь высказывается.
    #[test]
    fn every_kind_of_unresolved_node_is_named_and_coded() {
        let mut seen: Vec<String> = Vec::new();
        let mut broken: Vec<String> = Vec::new();
        for node in UnresolvedNode::all() {
            let diagnostic = refuse(PROBE, node.clone());
            if diagnostic.code.as_deref() != Some("CC-023") {
                broken.push(format!("{node:?}: код {:?}", diagnostic.code));
            }
            if diagnostic.loc != PROBE {
                broken.push(format!("{node:?}: позиция потеряна"));
            }
            if !diagnostic.message.contains(&node.phrase()) {
                broken.push(format!("{node:?}: текст не называет вид узла"));
            }
            if seen.contains(&node.phrase()) {
                broken.push(format!("{node:?}: название вида не отличает его от других"));
            }
            seen.push(node.phrase());
        }
        assert!(
            broken.is_empty(),
            "виды узлов без отказа CC-023: {broken:?}"
        );
    }
}
