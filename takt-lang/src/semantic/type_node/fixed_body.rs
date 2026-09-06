//! Понижение q-литерала в телах и условиях.
//!
//! # Правило
//!
//! Литерал понижается там, где **известен тип приёмника**: присваивание, сравнение с
//! q-операндом, аргумент вызова, возврат функции. Это те же "позиции приёмника", что у
//! 0335/0336, только вопрос решается **в семантике** - за её границей формы не
//! существует, и потребителям не по чему расходиться (приём 0143/0185/0192).
//!
//! **Условие ребра доезжает сюда разрешённым** - и это замер, а не чтение инварианта.
//! Первая редакция обходила ещё и сырой `ast::Condition`, полагаясь на правило "условия
//! рёбер `ref` не разрешаются"; правило верно, но относится к **одной форме** -
//! паттерну `S(Модель) = Состояние`. Стадия 6 разрешает остальные, а
//! `resolve_condition` отдаёт `Unresolved` ровно на неразрешённом **имени**, где ни
//! литерала, ни сравнения не бывает. Зонд 2026-08-22: обход сырого АСД не сработал ни
//! разу на всём корпусе и тестах, и его отключение не уронило ни одного из ~3600 тестов -
//! поэтому он снят (уроки 0233 и 0278: недостижимую ветвь нельзя ни проверить, ни
//! удержать от расхождения).
//!
//! Носитель представления - прежний `lower_fixed_literal`: второго знания о масштабе 2ⁿ
//! не заводится.
//!
//! # Место записи - не только имя
//!
//! Приёмником 0381 считала **голое имя**: формат q брался у ячейки ссылки. Поле
//! структуры и элемент массива под правило не подпали, хотя тип объявлен рядом.
//!
//! | Запись | эталон | цель `c` | `st`, `rust`, `sv` |
//! |---|---|---|---|
//! | `g.kp := 2.0;` при `kp: q(8, 8)` | `2` | `model->g.kp = 2.0;` -> **0** | вывод отвергают их инструменты |
//! | `gains[0] := 2.0;` | верно | то же | то же |
//! | `if g.kp > 1.0`, `ref Done: g.kp > 0.25;` | `SIM-005` **в такте** | считает по представлению | то же |
//!
//! Тип места спрашивается у **общего** носителя
//! [`validate::base_type`](crate::semantic::validate::base_type) - того же, которым
//! живут `SE-061`, `SE-028`/`SE-030` и поэлементная печать среза. Своего разбора
//! "структура -> поле" и "массив -> элемент" здесь нет: три копии одного правила
//! (выражение, условие, сырой АСД) разъехались бы молча.

use std::cell::RefCell;
use std::collections::BTreeMap;
use std::collections::HashSet;
use std::rc::Rc;

use crate::diagnostics::{Diagnostic, Location};
use crate::semantic::type_node::TypeNode;
use crate::semantic::type_node::type_fixed::lower_fixed_literal;
use crate::semantic::validate::base_type::{base_type, cond_base_type};
use crate::semantic::{
    ConditionDefinitionNode, ConditionNode, ExpressionNode, FunctionDefinitionNode, ModelNode,
    NamedCodeBlockDefinitionNode, StateNode, StatementNode,
};

/// Понижает q-литералы в телах и условиях модели и её под-моделей.
pub(crate) fn lower_fixed_literals(model: &Rc<RefCell<ModelNode>>) -> Result<(), Diagnostic> {
    let mut visited = HashSet::new();
    lower_model(model, &mut visited)
}

fn lower_model(
    model: &Rc<RefCell<ModelNode>>,
    visited: &mut HashSet<*const RefCell<ModelNode>>,
) -> Result<(), Diagnostic> {
    if !visited.insert(Rc::as_ptr(model)) {
        return Ok(()); // разделяемая под-модель уже обработана
    }
    let nested: Vec<Rc<RefCell<ModelNode>>> = model.borrow().models.values().cloned().collect();
    lower_bodies(model)?;
    for child in &nested {
        lower_model(child, visited)?;
    }
    Ok(())
}

/// Понижает литералы в телах и условиях одной модели.
///
/// Тела **изымаются** из модели на время обхода (`mem::take`) и возвращаются на место.
/// Причина - носитель типа места читает `ModelNode` (структуры, объявления), а
/// изменяемое заимствование модели этого не допускает: обход, написанный "в лоб", падал
/// бы `BorrowMutError` на первом же поле структуры. Тела и таблицы объявлений -
/// непересекающиеся части узла, поэтому изъятие наблюдаемо только внутри этой функции;
/// ошибка возвращается **после** того, как тела положены обратно.
fn lower_bodies(model: &Rc<RefCell<ModelNode>>) -> Result<(), Diagnostic> {
    let (mut functions, mut named_blocks, mut conditions, mut states) = {
        let mut b = model.borrow_mut();
        (
            std::mem::take(&mut b.functions),
            std::mem::take(&mut b.named_blocks),
            std::mem::take(&mut b.conditions),
            std::mem::take(&mut b.states),
        )
    };
    let outcome = lower_taken(
        &model.borrow(),
        &mut functions,
        &mut named_blocks,
        &mut conditions,
        &mut states,
    );
    {
        let mut b = model.borrow_mut();
        b.functions = functions;
        b.named_blocks = named_blocks;
        b.conditions = conditions;
        b.states = states;
    }
    outcome
}

fn lower_taken(
    model: &ModelNode,
    functions: &mut BTreeMap<String, FunctionDefinitionNode>,
    named_blocks: &mut [NamedCodeBlockDefinitionNode],
    conditions: &mut BTreeMap<String, ConditionDefinitionNode>,
    states: &mut BTreeMap<String, StateNode>,
) -> Result<(), Diagnostic> {
    for func in functions.values_mut() {
        lower_function(func, model)?;
    }
    for blk in named_blocks.iter_mut() {
        lower_block(blk, model)?;
    }
    for cond in conditions.values_mut() {
        lower_condition(&mut cond.value, model)?;
    }
    for state in states.values_mut() {
        lower_state(state, model)?;
    }
    Ok(())
}

fn lower_function(func: &mut FunctionDefinitionNode, model: &ModelNode) -> Result<(), Diagnostic> {
    let FunctionDefinitionNode::Local { ret, body, .. } = func else {
        return Ok(());
    };
    let ret_fixed = fixed_of_type(ret);
    lower_stmt(body, model, ret_fixed)
}

fn lower_block(
    blk: &mut NamedCodeBlockDefinitionNode,
    model: &ModelNode,
) -> Result<(), Diagnostic> {
    match blk {
        NamedCodeBlockDefinitionNode::Enter { body, .. }
        | NamedCodeBlockDefinitionNode::Exit { body, .. }
        | NamedCodeBlockDefinitionNode::Always { body, .. }
        | NamedCodeBlockDefinitionNode::Unknown { body, .. }
        | NamedCodeBlockDefinitionNode::Every { body, .. } => lower_stmt(body, model, None),
        // Пустой и неразрешённый блоки тела не несут.
        NamedCodeBlockDefinitionNode::None | NamedCodeBlockDefinitionNode::Unresolved(_, _) => {
            Ok(())
        }
    }
}

fn lower_state(state: &mut StateNode, model: &ModelNode) -> Result<(), Diagnostic> {
    let (blocks, refs) = match state {
        StateNode::Simple {
            named_blocks,
            references,
            ..
        }
        | StateNode::Implement {
            named_blocks,
            references,
            ..
        } => (named_blocks, references),
        StateNode::Unresolved => return Ok(()),
    };
    for blk in blocks.iter_mut() {
        lower_block(blk, model)?;
    }
    // Условие ребра `ref` - сырой АСД (инвариант проекта), и цели печатают его,
    // разрешая имена против модели: понижение обязано идти туда же.
    for reference in refs.iter_mut() {
        lower_condition(&mut reference.cond, model)?;
    }
    Ok(())
}

/// Формат `q(m, n)` у типа, если он такой.
fn fixed_of_type(ty: &TypeNode) -> Option<(u8, u8)> {
    match ty {
        TypeNode::Fixed { m, n, .. } => Some((*m, *n)),
        _ => None,
    }
}

/// Формат q у места в выражении (переменная, поле структуры, элемент массива).
///
/// Разбор цепочки принадлежит носителю
/// [`base_type`](crate::semantic::validate::base_type) - своего знания о структурах и
/// массивах здесь нет.
fn fixed_of_expr(expr: &ExpressionNode, model: &ModelNode) -> Option<(u8, u8)> {
    base_type(expr, model).as_ref().and_then(fixed_of_type)
}

/// Понижает литерал на месте, если он числовой; прочее не трогает.
fn lower_literal(expr: &mut ExpressionNode, (m, n): (u8, u8)) -> Result<(), Diagnostic> {
    if !matches!(
        expr,
        ExpressionNode::Number(_) | ExpressionNode::Rational(_, _)
    ) {
        return Ok(());
    }
    if let Some(v) = lower_fixed_literal(expr, m, n, Location::Codegen)? {
        *expr = ExpressionNode::Number(v);
    }
    Ok(())
}

fn lower_stmt(
    stmt: &mut StatementNode,
    model: &ModelNode,
    ret: Option<(u8, u8)>,
) -> Result<(), Diagnostic> {
    match stmt {
        StatementNode::Block(items) => {
            for s in items.iter_mut() {
                lower_stmt(s, model, ret)?;
            }
        }
        StatementNode::Expression(e, _) => lower_expr(e, model)?,
        StatementNode::If {
            cond, then_, else_, ..
        } => {
            lower_expr(cond, model)?;
            lower_stmt(then_, model, ret)?;
            if let Some(e) = else_ {
                lower_stmt(e, model, ret)?;
            }
        }
        StatementNode::Loop { cond, body, .. } => {
            if let Some(c) = cond {
                lower_expr(c, model)?;
            }
            lower_stmt(body, model, ret)?;
        }
        StatementNode::For {
            init,
            cond,
            step,
            body,
            ..
        } => {
            if let Some(s) = init {
                lower_stmt(s, model, ret)?;
            }
            if let Some(c) = cond {
                lower_expr(c, model)?;
            }
            if let Some(s) = step {
                lower_expr(s, model)?;
            }
            lower_stmt(body, model, ret)?;
        }
        // Локальное объявление: приёмник - объявленный тип.
        StatementNode::Variable(_, ty, init, _) => {
            let fixed = fixed_of_type(ty);
            if let Some(e) = init {
                lower_expr(e, model)?;
                if let Some(f) = fixed {
                    lower_literal(e, f)?;
                }
            }
        }
        // Возврат: приёмник - объявленный тип функции.
        StatementNode::Return(Some(e), _) => {
            lower_expr(e, model)?;
            if let Some(f) = ret {
                lower_literal(e, f)?;
            }
        }
        StatementNode::Match { expr, arms, .. } => {
            lower_expr(expr, model)?;
            for arm in arms.iter_mut() {
                lower_stmt(&mut arm.body, model, ret)?;
            }
        }
        // Вставка для цели - операторы Takt: обход спускается в тело. Блок формул
        // адресован внешнему анализатору, операторов Takt в нём нет.
        StatementNode::Assembly { body, .. } => lower_stmt(body, model, ret)?,
        StatementNode::None
        | StatementNode::Unresolved(_)
        | StatementNode::Return(None, _)
        | StatementNode::Continue(_)
        | StatementNode::Break(_)
        | StatementNode::Formula(_)
        | StatementNode::InlineFormula(_) => {}
    }
    Ok(())
}

/// Понижает литералы в выражении: присваивание, сравнение, аргумент вызова.
fn lower_expr(expr: &mut ExpressionNode, model: &ModelNode) -> Result<(), Diagnostic> {
    // Сначала вглубь: приёмник виден на своём уровне.
    for child in children_mut(expr) {
        lower_expr(child, model)?;
    }
    match expr {
        ExpressionNode::Assign(target, value) => {
            if let Some(f) = fixed_of_expr(target, model) {
                lower_literal(value, f)?;
            }
        }
        ExpressionNode::Equal(l, r)
        | ExpressionNode::NotEqual(l, r)
        | ExpressionNode::Less(l, r)
        | ExpressionNode::More(l, r)
        | ExpressionNode::LessEqual(l, r)
        | ExpressionNode::MoreEqual(l, r) => {
            // **Арифметика сюда не входит:** `gain + 1.5` отвергает `SE-059` (неявное
            // смешение q и float), и понижение литерала там лишь ухудшало бы сообщение -
            // вместо "'q(8, 8)' и 'float'" автор получал "'q(8, 8)' и '[bit;16]'", то
            // есть тип, которого он не писал. Сравнение - другое дело: оно законно и
            // работает.
            match (fixed_of_expr(l, model), fixed_of_expr(r, model)) {
                (Some(f), None) => lower_literal(r, f)?,
                (None, Some(f)) => lower_literal(l, f)?,
                _ => {}
            }
        }
        ExpressionNode::Function(def, args) => {
            let params: Vec<Option<(u8, u8)>> = match &*def.borrow() {
                FunctionDefinitionNode::Local { params, .. } => {
                    params.iter().map(|(_, ty)| fixed_of_type(ty)).collect()
                }
                _ => Vec::new(),
            };
            for (arg, param) in args.iter_mut().zip(params) {
                if let Some(f) = param {
                    lower_literal(arg, f)?;
                }
            }
        }
        _ => {}
    }
    Ok(())
}

/// Понижает литералы в разрешённом условии (`cond`, формулы).
fn lower_condition(cond: &mut ConditionNode, model: &ModelNode) -> Result<(), Diagnostic> {
    use ConditionNode as C;
    match cond {
        // `Unresolved` не значит "сырое условие ребра".
        //
        // завела здесь обход сырого АСД со всеми сравнениями, полагая, что условия
        // рёбер до целей доезжают неразрешёнными. Ни числа, ни сравнения в этой обёртке
        // не бывает по построению, а сам случай - правая часть паттерна `S(Модель) =
        // Состояние` (инвариант проекта), где обе стороны суть имена. Зонд подтвердил:
        // на всём корпусе и тестах обход сырого АСД не сработал ни разу, а отключение
        // его целиком не уронило ни одного теста.
        //
        // Тест предпосылки - `fixed_place_tests::edge_condition_is_resolved`: он
        // падает, если условие ребра со сравнением снова станет сырым (так будет, если
        // проход переедет выше стадии 6).
        C::Unresolved(_) => Ok(()),
        C::Equal(l, r)
        | C::NotEqual(l, r)
        | C::Less(l, r)
        | C::More(l, r)
        | C::LessEqual(l, r)
        | C::MoreEqual(l, r) => {
            lower_condition(l, model)?;
            lower_condition(r, model)?;
            match (cond_fixed_of(l, model), cond_fixed_of(r, model)) {
                (Some(f), None) => lower_cond_literal(r, f),
                (None, Some(f)) => lower_cond_literal(l, f),
                _ => Ok(()),
            }
        }
        C::And(l, r) | C::Or(l, r) => {
            lower_condition(l, model)?;
            lower_condition(r, model)
        }
        C::Not(inner) | C::Parenthesis(inner) => lower_condition(inner, model),
        _ => Ok(()),
    }
}

/// Формат q у места в разрешённом условии - тот же носитель.
fn cond_fixed_of(cond: &ConditionNode, model: &ModelNode) -> Option<(u8, u8)> {
    cond_base_type(cond, model).as_ref().and_then(fixed_of_type)
}

/// Понижает числовой литерал разрешённого условия.
fn lower_cond_literal(cond: &mut ConditionNode, (m, n): (u8, u8)) -> Result<(), Diagnostic> {
    let expr = match cond {
        ConditionNode::Number(k) => ExpressionNode::Number(*k),
        ConditionNode::Rational(s, neg) => ExpressionNode::Rational(s.clone(), *neg),
        _ => return Ok(()),
    };
    if let Some(v) = lower_fixed_literal(&expr, m, n, Location::Codegen)? {
        *cond = ConditionNode::Number(v);
    }
    Ok(())
}

/// Дети выражения - для спуска вглубь.
fn children_mut(expr: &mut ExpressionNode) -> Vec<&mut ExpressionNode> {
    use ExpressionNode as E;
    match expr {
        E::Assign(a, b)
        | E::Equal(a, b)
        | E::NotEqual(a, b)
        | E::Less(a, b)
        | E::More(a, b)
        | E::LessEqual(a, b)
        | E::MoreEqual(a, b)
        | E::Add(a, b)
        | E::Subtract(a, b)
        | E::Multiply(a, b)
        | E::Divide(a, b)
        | E::Modulo(a, b)
        | E::Power(a, b)
        | E::And(a, b)
        | E::Or(a, b)
        | E::BitwiseAnd(a, b)
        | E::BitwiseOr(a, b)
        | E::BitwiseXor(a, b)
        | E::ShiftLeft(a, b)
        | E::ShiftRight(a, b)
        | E::ArraySubscript(a, b) => vec![a, b],
        E::Parenthesis(a) | E::Not(a) | E::BitwiseNot(a) | E::Negate(a) | E::Cast(a, _) => vec![a],
        E::Function(_, args) => args.iter_mut().collect(),
        _ => Vec::new(),
    }
}
