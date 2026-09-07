//! Общий обход мест объявления формул.
//!
//! Формула объявляется в **шести** местах: тело модели, тело состояния, именованные
//! блоки состояния и модели, тела функций и вложенные операторы
//! (`StatementNode::InlineFormula` внутри `if`/`loop`/`for`/`match`/блока). Список этих
//! мест написан **один раз** - здесь; потребители получают уже собранные листья и
//! различаются только фильтром:
//!
//! - диагностики и верификация LTL - [`ltl_check`](crate::semantic::ltl_check)
//!   (`SE-055`/`SE-056`, `taktc verify`);
//! - проверка охранных формул - `validate/formulas.rs` (`SE-025` и прочие
//!   проверки условия).
//!
//! **Второго списка мест быть не должно.** Появится новое место объявления формулы -
//! добавлять его нужно здесь, иначе оно молча выпадет из обоих потребителей. Опасен и
//! фильтр на выходе обхода: выброси он `Guard`, и охранная формула с опечаткой
//! промолчит.

use crate::diagnostics::Location;
use crate::semantic::formula::Formula;
use crate::semantic::{ConditionNode, FunctionDefinitionNode, ModelNode, StatementNode};
use crate::verification::ltl::Ltl;

/// Объявленная формула: **лист** дерева [`Formula`].
///
/// Вариант `Formula::Formulas` (запись `: [Guard] a, [LTL] b;`) здесь невозможен по
/// построению - обход его разворачивает, - поэтому у потребителя не появляется
/// недостижимой ветки (: недостижимый код нельзя ни проверить, ни удержать от
/// расхождения). `Formula::None` объявлением не является и сайта не порождает.
#[derive(Debug, Clone)]
pub enum FormulaLeaf {
    /// Темпоральная формула `: [LTL] φ;`.
    Ltl(Ltl),
    /// Охранная формула `: [Guard] c;`, краткая `: c;` либо десахаризация `invariant
    /// Имя = c;`.
    ///
    /// Имя инварианта сайт **не несёт**: оно метаданные диагностики симулятора
    /// (`SIM-025`), и ни один потребитель обхода его не спрашивает - а неиспользуемое
    /// поле нечем удержать от расхождения. Понадобится - берётся из самой
    /// [`Formula::Guard`].
    Guard(ConditionNode),
}

/// Место объявления формулы: сама формула, её позиция и **область**.
///
/// Область нужна потому, что формула, объявленная в теле состояния, говорит о прогонах
/// **из этого состояния**, а не от старта. Десахаризацию делает потребитель-верификатор
/// ([`verify_all`](crate::verify_all)), а не этот сборщик: диагностикам нужна
/// **авторская** формула - иначе сообщение указывало бы на то, чего автор не писал.
#[derive(Debug, Clone)]
pub struct FormulaSite {
    /// Формула, как её написал автор (без десахаризации области).
    pub formula: FormulaLeaf,
    /// Позиция самой формулы; вместилище - запасной ход.
    ///
    /// Координата вместилища здесь не годится: две формулы уровня модели напечатались
    /// бы с одной и той же строкой `1:1`. Свою позицию несут обе формы - и охранная, и
    /// темпоральная.
    ///
    /// Запасной ход остаётся: формула, построенная вне разбора, координаты не имеет -
    /// там и печатается место вместилища.
    pub loc: Location,
    /// Имя состояния, в теле которого объявлена формула; `None` - уровень модели (тело
    /// модели, её именованные блоки, тела функций).
    pub state: Option<String>,
}

/// Собирает формулы, объявленные **непосредственно** в этой модели, вместе с их
/// позициями и областями.
///
/// Вложенные модели **не обходятся**: их формулы говорят о состояниях своей модели и
/// проверяются против её же графа. Рекурсию по моделям ведут сами потребители - им же
/// принадлежит и решение, что делать с диагностиками вложенных.
pub(crate) fn model_formula_sites(model: &ModelNode) -> Vec<FormulaSite> {
    let mut out = Vec::new();

    // 1. Формулы уровня модели.
    for f in &model.formulas {
        collect_formula(f, model.loc, None, &mut out);
    }
    // Формулы уровня состояний: прямые (`: [...]` в теле состояния) и внутри
    // именованных блоков состояния (`always`/`enter`/`exit`). И те и другие объявлены в
    // области состояния: `enter`/`exit`/`always` исполняются, лишь когда автомат в нём, -
    // область у них та же, что у тела.
    for state in model.states.values() {
        let scope = Some(state.name().to_string());
        for f in state.formulas() {
            collect_formula(f, state.loc(), scope.clone(), &mut out);
        }
        for block in state.named_blocks() {
            if let Some(stmt) = block.statement() {
                walk_statement(stmt, state.loc(), scope.clone(), &mut out);
            }
        }
    }
    // 4. Формулы в телах именованных блоков модели (`enter`/`exit`/`always`).
    for block in &model.named_blocks {
        if let Some(stmt) = block.statement() {
            walk_statement(stmt, model.loc, None, &mut out);
        }
    }
    // 5. Формулы в телах функций: функцию вправе позвать любое состояние,
    // поэтому область - модель, а не состояние вызова.
    for func in model.functions.values() {
        if let FunctionDefinitionNode::Local { body, .. } = func {
            walk_statement(body, model.loc, None, &mut out);
        }
    }

    out
}

/// Обходит оператор в поисках встроенных формул (`: [...] ...;` в блоке кода) - шестое
/// место объявления.
fn walk_statement(
    stmt: &StatementNode,
    loc: Location,
    scope: Option<String>,
    out: &mut Vec<FormulaSite>,
) {
    match stmt {
        StatementNode::InlineFormula(formulas) => {
            for f in formulas {
                collect_formula(f, loc, scope.clone(), out);
            }
        }
        StatementNode::Block(stmts) => {
            for s in stmts {
                walk_statement(s, loc, scope.clone(), out);
            }
        }
        StatementNode::If { then_, else_, .. } => {
            walk_statement(then_, loc, scope.clone(), out);
            if let Some(e) = else_ {
                walk_statement(e, loc, scope.clone(), out);
            }
        }
        StatementNode::Loop { body, .. } => walk_statement(body, loc, scope.clone(), out),
        StatementNode::For { init, body, .. } => {
            if let Some(i) = init {
                walk_statement(i, loc, scope.clone(), out);
            }
            walk_statement(body, loc, scope.clone(), out);
        }
        StatementNode::Match { arms, .. } => {
            for arm in arms {
                walk_statement(&arm.body, loc, scope.clone(), out);
            }
        }
        // Прочие операторы формул не содержат.
        _ => {}
    }
}

/// Разворачивает [`Formula`] в листья.
///
/// Разбор **исчерпывающий**, без `_`: новый вид формулы обязан заставить принять
/// решение здесь, а не выпасть молча из обоих потребителей.
fn collect_formula(
    formula: &Formula,
    loc: Location,
    scope: Option<String>,
    out: &mut Vec<FormulaSite>,
) {
    match formula {
        // Позиция берётся у самой формулы: с координатой вместилища три формулы уровня
        // модели печатали бы `SE-055` с одной и той же строкой `1:1`, и понять, о
        // которой речь, было бы нельзя. Запасной ход тот же, что у охранной формы:
        // формула, построенная вне разбора, своей позиции не имеет.
        Formula::LTL(ltl, own) => out.push(FormulaSite {
            formula: FormulaLeaf::Ltl(ltl.clone()),
            loc: if matches!(own, crate::diagnostics::Location::Builtin) {
                loc
            } else {
                *own
            },
            state: scope,
        }),
        // Позиция берётся у самой формулы, а `loc` вместилища остаётся запасным ходом:
        // у формулы, построенной вне разбора (`condition_to_formula`), своей позиции
        // нет.
        Formula::Guard(cond, _name, own) => out.push(FormulaSite {
            formula: FormulaLeaf::Guard(cond.clone()),
            loc: if matches!(own, crate::diagnostics::Location::Builtin) {
                loc
            } else {
                *own
            },
            state: scope,
        }),
        Formula::Formulas(inner) => {
            for f in inner {
                collect_formula(f, loc, scope.clone(), out);
            }
        }
        // Пустая формула объявлением не является: сайта у неё нет.
        Formula::None => {}
    }
}
