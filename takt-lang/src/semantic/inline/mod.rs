//! Подстановка тела функции в место вызова.
//!
//! # Правило
//!
//! Вызов функции, помеченной атрибутом `[inline]` (а при `--inline=auto` - ещё и
//! небольшой функции с 1...3 вызовами), заменяется **телом**: параметры становятся
//! локальными объявлениями с именами `takt_inline_<n>_<параметр>`, результат - таким же
//! объявлением `takt_inline_<n>_ret`, а сам вызов - ссылкой на него.
//!
//! ```text
//! probe := twice(n) + 1;
//! ⇓
//! var takt_inline_1_v: u8 := n;
//! var takt_inline_1_ret: u8 := takt_inline_1_v * 2;
//! probe := takt_inline_1_ret + 1;
//! ```
//!
//! За границей семантики подстановки не существует: печатники **восьми** целей не
//! трогаются вовсе (приём 0143/0192/0199/0400). Функция, оставшаяся без вызовов,
//! перестаёт печататься сама - фильтр "используется ли функция" (`UsageSet::functions`)
//! у целей уже есть.
//!
//! **Эталон проход не зовёт, и это замысел.** Подстановка меняет форму, а не поведение,
//! поэтому сверка "эталон против прошивки с подстановкой" и доказывает тождественность.
//! Позови её обе стороны - сверка перестала бы видеть дефект подстановки.
//!
//! **Атрибут действует всегда, эвристика - по флагу.** Атрибут написан автором, а
//! эвристика меняет вывод **всего корпуса** разом (снимки 0274 и
//! `examples/generated/`), поэтому её умолчание - `off`.
//!
//! # Названные границы первой редакции
//!
//! Вызов остаётся вызовом (и это не ошибка), если:
//!
//! - тело содержит `return` не последним оператором либо несколько возвратов -
//!   подстановка потребовала бы признака раннего выхода; при явном `[inline]`
//!   об этом говорит `SE-128`, при эвристике - молчаливый пропуск;
//! - функция ничего не возвращает (`Unit`): заменять нечем - у оператора-вызова
//!   нет значения, а изымать оператор проход не вправе;
//! - функция объявлена **не в той модели**, где стоит вызов: тела ссылаются на
//!   объявления своего владельца, и перенос сменил бы адресата;
//! - вызов стоит в условии цикла, в шаге `for` или в условии ребра: там
//!   выражение вычисляется многократно либо вне тела, и вынести объявление
//!   перед ним нельзя.

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use crate::diagnostics::{Diagnostic, Location};
use crate::semantic::fresh::{Fresh, taken_names};
use crate::semantic::function::{InlineMode, inline_mode};
use crate::semantic::type_node::TypeNode;
use crate::semantic::{
    ExpressionNode, FunctionDefinitionNode, ModelNode, NamedCodeBlockDefinitionNode, StateNode,
    StatementNode, VariableNode,
};

mod early;
mod rename;

/// Префикс имён подстановки. Обязан быть допустимым идентификатором **целевых** языков -
/// C, IEC, Rust, SystemVerilog.
const PREFIX: &str = "takt_inline_";

/// Порог эвристики: тело не длиннее пяти операторов.
const MAX_BODY_STATEMENTS: usize = 5;

/// Верхняя граница числа вызовов для эвристики.
const MAX_CALLS: usize = 3;

/// Подставляет тела функций по всему дереву.
///
/// `heuristic` - включена ли эвристика (`--inline=auto`). Атрибут `[inline]` действует
/// независимо от неё.
pub fn inline_functions(model: &Rc<RefCell<ModelNode>>, heuristic: bool) {
    let taken = taken_names(model);
    let mut counter = Fresh::new(PREFIX, &taken);
    let mut calls = HashMap::new();
    count_calls(model, &mut HashSet::new(), &mut calls);
    let mut ctx = Ctx {
        fresh: &mut counter,
        calls: &calls,
        heuristic,
    };
    inline_model(model, &mut HashSet::new(), &mut ctx);
}

/// Состояние обхода.
struct Ctx<'a> {
    fresh: &'a mut Fresh<'a>,
    /// Сколько раз вызвана функция (ключ - имя): предмет эвристики.
    calls: &'a HashMap<String, usize>,
    heuristic: bool,
}

/// Считает вызовы функций по всем телам программы.
fn count_calls(
    model: &Rc<RefCell<ModelNode>>,
    visited: &mut HashSet<*const RefCell<ModelNode>>,
    out: &mut HashMap<String, usize>,
) {
    if !visited.insert(Rc::as_ptr(model)) {
        return;
    }
    let b = model.borrow();
    for func in b.functions.values() {
        if let FunctionDefinitionNode::Local { body, .. } = func {
            count_in_stmt(body, out);
        }
    }
    for blk in &b.named_blocks {
        count_in_block(blk, out);
    }
    for st in b.states.values() {
        for blk in state_blocks(st) {
            count_in_block(blk, out);
        }
    }
    let nested: Vec<Rc<RefCell<ModelNode>>> = b.models.values().cloned().collect();
    drop(b);
    for child in &nested {
        count_calls(child, visited, out);
    }
}

fn count_in_block(blk: &NamedCodeBlockDefinitionNode, out: &mut HashMap<String, usize>) {
    if let Some(body) = block_body(blk) {
        count_in_stmt(body, out);
    }
}

fn count_in_stmt(stmt: &StatementNode, out: &mut HashMap<String, usize>) {
    crate::semantic::walk::walk_stmt_exprs(stmt, &mut |expr| count_in_expr(expr, out));
}

fn count_in_expr(expr: &ExpressionNode, out: &mut HashMap<String, usize>) {
    if let ExpressionNode::Function(def, _) = expr
        && let FunctionDefinitionNode::Local { name, .. } = &*def.borrow()
    {
        *out.entry(name.clone()).or_insert(0) += 1;
    }
}

/// Именованные блоки состояния - в одном месте, чтобы обходы не разошлись.
fn state_blocks(state: &StateNode) -> &[NamedCodeBlockDefinitionNode] {
    match state {
        StateNode::Simple { named_blocks, .. } | StateNode::Implement { named_blocks, .. } => {
            named_blocks
        }
        StateNode::Unresolved => &[],
    }
}

fn block_body(blk: &NamedCodeBlockDefinitionNode) -> Option<&StatementNode> {
    match blk {
        NamedCodeBlockDefinitionNode::Enter { body, .. }
        | NamedCodeBlockDefinitionNode::Exit { body, .. }
        | NamedCodeBlockDefinitionNode::Always { body, .. }
        | NamedCodeBlockDefinitionNode::Unknown { body, .. }
        | NamedCodeBlockDefinitionNode::Every { body, .. } => Some(body),
        NamedCodeBlockDefinitionNode::None | NamedCodeBlockDefinitionNode::Unresolved(_, _) => None,
    }
}

fn block_body_mut(blk: &mut NamedCodeBlockDefinitionNode) -> Option<&mut StatementNode> {
    match blk {
        NamedCodeBlockDefinitionNode::Enter { body, .. }
        | NamedCodeBlockDefinitionNode::Exit { body, .. }
        | NamedCodeBlockDefinitionNode::Always { body, .. }
        | NamedCodeBlockDefinitionNode::Unknown { body, .. }
        | NamedCodeBlockDefinitionNode::Every { body, .. } => Some(body),
        NamedCodeBlockDefinitionNode::None | NamedCodeBlockDefinitionNode::Unresolved(_, _) => None,
    }
}

fn inline_model(
    model: &Rc<RefCell<ModelNode>>,
    visited: &mut HashSet<*const RefCell<ModelNode>>,
    ctx: &mut Ctx<'_>,
) {
    if !visited.insert(Rc::as_ptr(model)) {
        return; // разделяемая под-модель уже обойдена
    }
    let nested: Vec<Rc<RefCell<ModelNode>>> = model.borrow().models.values().cloned().collect();
    inline_bodies(model, ctx);
    for child in &nested {
        inline_model(child, visited, ctx);
    }
}

/// Обходит тела одной модели.
///
/// Тела **изымаются** на время обхода (`mem::take`) и возвращаются: подстановка читает
/// объявление функции у той же модели, и изменяемое заимствование этого не допускает
/// (тот же приём, что в 0400).
fn inline_bodies(model: &Rc<RefCell<ModelNode>>, ctx: &mut Ctx<'_>) {
    let (mut functions, mut named_blocks, mut states) = {
        let mut b = model.borrow_mut();
        (
            std::mem::take(&mut b.functions),
            std::mem::take(&mut b.named_blocks),
            std::mem::take(&mut b.states),
        )
    };
    // Тела функций обходятся первыми: подставленный в них текст попадёт в место вызова
    // уже развёрнутым, если очередь дошла до вызывающей функции раньше.
    let snapshot = functions.clone();
    for func in functions.values_mut() {
        if let FunctionDefinitionNode::Local { body, .. } = func {
            inline_stmt(body, &snapshot, model, ctx);
        }
    }
    let snapshot = functions.clone();
    for blk in named_blocks.iter_mut() {
        if let Some(body) = block_body_mut(blk) {
            inline_stmt(body, &snapshot, model, ctx);
        }
    }
    for st in states.values_mut() {
        let blocks = match st {
            StateNode::Simple { named_blocks, .. } | StateNode::Implement { named_blocks, .. } => {
                named_blocks
            }
            StateNode::Unresolved => continue,
        };
        for blk in blocks.iter_mut() {
            if let Some(body) = block_body_mut(blk) {
                inline_stmt(body, &snapshot, model, ctx);
            }
        }
    }
    let mut b = model.borrow_mut();
    b.functions = functions;
    b.named_blocks = named_blocks;
    b.states = states;
}

/// Множество функций модели - снимок на время обхода её тел.
type Functions = std::collections::BTreeMap<String, FunctionDefinitionNode>;

/// Обходит оператор; подставить тело можно **только внутри блока** - там есть куда
/// вставить объявления.
fn inline_stmt(
    stmt: &mut StatementNode,
    funcs: &Functions,
    owner: &Rc<RefCell<ModelNode>>,
    ctx: &mut Ctx<'_>,
) {
    match stmt {
        StatementNode::Block(items) => {
            let mut out: Vec<StatementNode> = Vec::with_capacity(items.len());
            for mut item in std::mem::take(items) {
                inline_stmt(&mut item, funcs, owner, ctx);
                let mut prelude = Vec::new();
                expand_in_statement(&mut item, funcs, owner, ctx, &mut prelude);
                out.extend(prelude);
                out.push(item);
            }
            *items = out;
        }
        StatementNode::If { then_, else_, .. } => {
            inline_stmt(then_, funcs, owner, ctx);
            if let Some(alt) = else_ {
                inline_stmt(alt, funcs, owner, ctx);
            }
        }
        StatementNode::Loop { body, .. } => inline_stmt(body, funcs, owner, ctx),
        StatementNode::For { init, body, .. } => {
            if let Some(i) = init {
                inline_stmt(i, funcs, owner, ctx);
            }
            inline_stmt(body, funcs, owner, ctx);
        }
        StatementNode::Match { arms, .. } => {
            for arm in arms.iter_mut() {
                inline_stmt(&mut arm.body, funcs, owner, ctx);
            }
        }
        _ => {}
    }
}

/// Подставляет тела в выражениях одного оператора.
///
/// Условие цикла и шаг `for` намеренно не обходятся: они вычисляются на каждой
/// итерации, а объявления `prelude` встают **перед** оператором - вынос изменил бы
/// поведение. Там вызов остаётся вызовом.
fn expand_in_statement(
    stmt: &mut StatementNode,
    funcs: &Functions,
    owner: &Rc<RefCell<ModelNode>>,
    ctx: &mut Ctx<'_>,
    prelude: &mut Vec<StatementNode>,
) {
    match stmt {
        // Позиция берётся у самого оператора: у выражения своей нет.
        StatementNode::Expression(expr, loc) => {
            let at = *loc;
            expand_in_expr(expr, funcs, owner, ctx, prelude, at);
        }
        StatementNode::Return(Some(expr), _) => {
            expand_in_expr(expr, funcs, owner, ctx, prelude, Location::Implicit);
        }
        StatementNode::Variable(_, _, Some(init), loc) => {
            let at = *loc;
            expand_in_expr(init, funcs, owner, ctx, prelude, at);
        }
        StatementNode::If { cond, .. } => {
            expand_in_expr(cond, funcs, owner, ctx, prelude, Location::Implicit);
        }
        StatementNode::Match { expr, .. } => {
            expand_in_expr(expr, funcs, owner, ctx, prelude, Location::Implicit);
        }
        _ => {}
    }
}

/// Спускается по выражению, заменяя подходящие вызовы ссылкой на результат.
fn expand_in_expr(
    expr: &mut ExpressionNode,
    funcs: &Functions,
    owner: &Rc<RefCell<ModelNode>>,
    ctx: &mut Ctx<'_>,
    prelude: &mut Vec<StatementNode>,
    loc: Location,
) {
    crate::semantic::walk::walk_expr_mut(expr, &mut |node| {
        if let Some(replacement) = expand_call(node, funcs, owner, ctx, prelude, loc) {
            *node = replacement;
        }
    });
}

/// Строит подстановку одного вызова; `None` - вызов остаётся вызовом.
fn expand_call(
    expr: &ExpressionNode,
    funcs: &Functions,
    owner: &Rc<RefCell<ModelNode>>,
    ctx: &mut Ctx<'_>,
    prelude: &mut Vec<StatementNode>,
    loc: Location,
) -> Option<ExpressionNode> {
    let ExpressionNode::Function(def, args) = expr else {
        return None;
    };
    let name = match &*def.borrow() {
        FunctionDefinitionNode::Local { name, .. } => name.clone(),
        _ => return None,
    };
    // Объявление берётся у снимка модели, а не у ячейки вызова: в ячейке снимок, снятый
    // при разрешении, и подстановки, сделанные в теле функции, в него не попадают.
    let Some(FunctionDefinitionNode::Local {
        params,
        ret,
        body,
        raw,
        upper,
        ..
    }) = funcs.get(&name)
    else {
        return None;
    };
    // Функция чужой модели: её тело адресует объявления своего владельца.
    if !upper
        .as_ref()
        .and_then(|w| w.upgrade())
        .is_some_and(|m| Rc::ptr_eq(&m, owner))
    {
        return None;
    }
    if matches!(ret, TypeNode::Unit) {
        return None;
    }
    if args.len() != params.len() {
        return None; // арность судит SE-122 раньше; здесь просто не наше дело
    }
    if !wanted(raw, body, &name, ctx) {
        return None;
    }
    let index = ctx.fresh.next_index();
    let ret_name = fresh_local(ctx, index, "ret");
    // Форма подстановки: хвостовой возврат подставляется прямо, ранний - через признак
    // выхода. Обе формы строит один носитель, и он же отвечает судье атрибута
    // (`SE-128`).
    let body_stmts = match split_tail_return(body) {
        Some((head, value)) => tail_form(&head, value, &ret_name, ret, loc),
        None => {
            let borrowed = owner.borrow();
            // Препятствие спрашивается здесь, а не только у судьи атрибута: под
            // эвристикой отказа быть не должно, а подставить такое тело нельзя.
            if early::obstacle(body, ret, &borrowed).is_some() {
                return None;
            }
            let done_name = fresh_local(ctx, index, "done");
            early::lower(body, &ret_name, ret, &done_name, owner, &borrowed)?.stmts
        }
    };

    let mut map = HashMap::new();
    for (param, _) in params {
        map.insert(param.clone(), fresh_local(ctx, index, param));
    }
    let mut locals = HashSet::new();
    for stmt in &body_stmts {
        crate::semantic::fresh::collect_locals(stmt, &mut locals);
    }
    // Имена самой подстановки переименовывать не надо: они уже свежие.
    for local in locals {
        if local.starts_with(PREFIX) {
            continue;
        }
        let renamed = fresh_local(ctx, index, &local);
        map.insert(local, renamed);
    }

    for ((param, ty), arg) in params.iter().zip(args) {
        prelude.push(StatementNode::Variable(
            map[param].clone(),
            ty.clone(),
            Some(Box::new(arg.clone())),
            Location::Implicit,
        ));
    }
    for mut stmt in body_stmts {
        rename::rename_stmt(&mut stmt, &map, owner);
        prelude.push(stmt);
    }
    Some(ExpressionNode::Variable(Rc::new(RefCell::new(
        VariableNode::Simple {
            upper: Some(Rc::downgrade(owner)),
            loc: Location::Implicit,
            name: ret_name,
            ty: ret.clone(),
            expr: ExpressionNode::None,
        },
    ))))
}

/// Форма подстановки для тела с хвостовым возвратом: операторы тела, затем объявление
/// результата с инициализатором.
///
/// Объявление именно **с инициализатором**: отложенная инициализация даёт у цели `rust`
/// `clippy::needless_late_init`, а лишний `mut` - "variable does not need to be
/// mutable".
fn tail_form(
    head: &[StatementNode],
    value: ExpressionNode,
    ret_name: &str,
    ret: &TypeNode,
    loc: Location,
) -> Vec<StatementNode> {
    let mut out = head.to_vec();
    out.push(StatementNode::Variable(
        ret_name.to_string(),
        ret.clone(),
        Some(Box::new(value)),
        loc_or_implicit(loc),
    ));
    out
}

/// Позиция синтетического объявления: место вызова, если оно известно.
fn loc_or_implicit(loc: Location) -> Location {
    match loc {
        Location::Source(_, _, _) => loc,
        _ => Location::Implicit,
    }
}

/// Свежее имя `takt_inline_<n>_<имя>`, проверенное на занятость.
fn fresh_local(ctx: &mut Ctx<'_>, index: usize, name: &str) -> String {
    let mut candidate = format!("{PREFIX}{index}_{name}");
    let mut suffix = 0usize;
    while ctx.fresh.is_taken(&candidate) {
        suffix += 1;
        candidate = format!("{PREFIX}{index}_{name}_{suffix}");
    }
    candidate
}

/// Подставлять ли эту функцию: атрибут сильнее эвристики.
fn wanted(
    raw: &crate::parser::ast::FunctionDefine,
    body: &StatementNode,
    name: &str,
    ctx: &Ctx<'_>,
) -> bool {
    match inline_mode(raw) {
        InlineMode::Always => true,
        InlineMode::Never => false,
        InlineMode::Auto => {
            ctx.heuristic
                && statement_count(body) <= MAX_BODY_STATEMENTS
                && matches!(ctx.calls.get(name), Some(&n) if (1..=MAX_CALLS).contains(&n))
        }
    }
}

/// Размер тела в операторах: блок сам по себе не считается, вложенные - да.
pub(crate) fn statement_count(stmt: &StatementNode) -> usize {
    match stmt {
        StatementNode::None => 0,
        StatementNode::Block(items) => items.iter().map(statement_count).sum(),
        StatementNode::If { then_, else_, .. } => {
            1 + statement_count(then_) + else_.as_ref().map_or(0, |s| statement_count(s))
        }
        StatementNode::Loop { body, .. } => 1 + statement_count(body),
        StatementNode::For { init, body, .. } => {
            1 + init.as_ref().map_or(0, |s| statement_count(s)) + statement_count(body)
        }
        StatementNode::Match { arms, .. } => {
            1 + arms.iter().map(|a| statement_count(&a.body)).sum::<usize>()
        }
        _ => 1,
    }
}

/// Разбивает тело на "операторы до возврата" и возвращаемое выражение.
///
/// `None` - возврат не единственный, не последний либо отсутствует: подстановка такой
/// формы требует признака раннего выхода (названная граница).
pub(crate) fn split_tail_return(
    body: &StatementNode,
) -> Option<(Vec<StatementNode>, ExpressionNode)> {
    let items: Vec<StatementNode> = match body {
        StatementNode::Block(items) => items.clone(),
        other => vec![other.clone()],
    };
    let (last, head) = items.split_last()?;
    let StatementNode::Return(Some(expr), _) = last else {
        return None;
    };
    if head.iter().any(has_return) {
        return None;
    }
    Some((head.to_vec(), (**expr).clone()))
}

/// Есть ли `return` где-нибудь внутри оператора.
pub(crate) fn has_return(stmt: &StatementNode) -> bool {
    match stmt {
        StatementNode::Return(_, _) => true,
        StatementNode::Block(items) => items.iter().any(has_return),
        StatementNode::If { then_, else_, .. } => {
            has_return(then_) || else_.as_ref().is_some_and(|s| has_return(s))
        }
        StatementNode::Loop { body, .. } => has_return(body),
        StatementNode::For { init, body, .. } => {
            init.as_ref().is_some_and(|s| has_return(s)) || has_return(body)
        }
        StatementNode::Match { arms, .. } => arms.iter().any(|a| has_return(&a.body)),
        _ => false,
    }
}

/// Препятствие подстановке, если оно есть.
///
/// Хвостовой возврат подставляется прямо, ранний - через признак выхода; отказ остаётся
/// лишь там, где формы нет вовсе.
pub(crate) fn inline_obstacle(
    body: &StatementNode,
    ret: &TypeNode,
    model: &ModelNode,
) -> Option<early::Obstacle> {
    if split_tail_return(body).is_some() {
        return None;
    }
    early::obstacle(body, ret, model)
}

/// Отказ `SE-128`: `[inline]` на функции, тело которой подстановкой не выражается.
/// Причина **называется** - по ней автор понимает, что менять.
pub(crate) fn inline_refusal(loc: Location, name: &str, why: early::Obstacle) -> Diagnostic {
    Diagnostic::error(
        loc,
        format!(
            "функция '{name}' помечена атрибутом 'inline', но её тело не сводится к подстановке: {}",
            why.text()
        ),
    )
    .with_code("SE-128")
}
