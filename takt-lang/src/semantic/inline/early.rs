//! Подстановка тела с ранним возвратом.
//!
//! # Правило
//!
//! Тело, выходящее раньше конца, подставляется через **признак выхода**: результат
//! объявляется переменной, каждый `return` становится записью в неё и взводом признака,
//! а операторы, стоящие после возможного выхода, идут под условием "выхода ещё не
//! было".
//!
//! ```text
//! [inline] fn pick(v: u8) -> u8 {
//!     if v > 3 { return 1; }
//!     return v;
//! }
//! ⇓
//! var takt_inline_1_ret: u8 := 0;
//! var takt_inline_1_done: bit := 0;
//! if (v > 3) { takt_inline_1_ret := 1; takt_inline_1_done := 1; }
//! if !takt_inline_1_done { takt_inline_1_ret := v; }
//! ```
//!
//! **Результат объявляется С начальным значением, и это замер, а не вкус.**
//! Форма "объявление без инициализатора + условные присваивания" валидна у
//! семи потребителей, а `rustc` отвечает `E0381: used binding is
//! possibly-uninitialized`: его анализ не знает,
//! что один из путей срабатывает всегда. С начальным значением вывод принимают
//! **все восемь** целей и оба инструмента SV.
//!
//! **Признак объявляется, только если он читается.** Тело, где выход последний по
//! тексту, обёрток не порождает, а мёртвая запись `done := 1` дала бы у `rust` "value
//! assigned is never read" под `-D warnings`.
//!
//! # Названные границы
//!
//! Возврат **внутри цикла** (`loop`, `while`, `for`) под правило не подпадает: выход из
//! цикла потребовал бы `break` под тем же признаком, а у цели `sv` цикл ещё и
//! разворачивается при элаборации. Тип результата, у которого нет представимого
//! начального значения (массив, структура), - тоже.

use std::cell::RefCell;
use std::rc::Rc;

use crate::diagnostics::Location;
use crate::semantic::type_node::TypeNode;
use crate::semantic::{ExpressionNode, ModelNode, StatementNode, VariableNode};

/// Почему тело подстановкой не выражается.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Obstacle {
    /// Возврата в теле нет вовсе, а значение функция обязана дать.
    NoReturn,
    /// Возврат стоит внутри цикла, чьё завершение зависит от тела.
    ReturnInOpenLoop,
    /// У типа результата нет представимого начального значения.
    NoDefaultValue,
}

impl Obstacle {
    /// Текст причины для диагностики `SE-128`.
    pub(crate) fn text(self) -> &'static str {
        match self {
            Obstacle::NoReturn => "в теле нет ни одного 'return', и значение брать неоткуда",
            Obstacle::ReturnInOpenLoop => {
                "'return' стоит внутри цикла, число итераций которого неизвестно заранее: \
                 подстановка гасит тело признаком выхода, и такой цикл стал бы бесконечным \
                 (у 'for' со счётчиком от литерала до литерала возврат подставляется)"
            }
            Obstacle::NoDefaultValue => {
                "у типа результата нет начального значения, с которым объявляется временная \
                 (массив и структура сюда не входят)"
            }
        }
    }
}

/// Проверяет, выражается ли тело подстановкой; `None` - выражается.
pub(crate) fn obstacle(
    body: &StatementNode,
    ret: &TypeNode,
    model: &ModelNode,
) -> Option<Obstacle> {
    if !super::has_return(body) {
        return Some(Obstacle::NoReturn);
    }
    if return_in_open_loop(body) {
        return Some(Obstacle::ReturnInOpenLoop);
    }
    if default_value(ret, model).is_none() {
        return Some(Obstacle::NoDefaultValue);
    }
    None
}

/// Предел счёта итераций при проверке границ цикла.
///
/// Принадлежит **этому** правилу, а не языку: цикл длиннее просто не признаётся
/// статическим, и возврат из него остаётся вызовом. У цели `sv` свой предел - размер
/// схемы (`sv_unroll::MAX_ITERATIONS`).
const MAX_STATIC_ITERATIONS: usize = 4096;

/// Есть ли `return` внутри цикла, чьё завершение зависит от тела.
///
/// Цикл со **статически известным** числом итераций сюда не входит: он завершится сам,
/// даже когда его тело погашено признаком выхода. Признак "границы известны" берётся у
/// общего носителя [`semantic::loop_bounds`](crate::semantic::loop_bounds) - того же,
/// которым цель `sv` разворачивает цикл в схему.
fn return_in_open_loop(stmt: &StatementNode) -> bool {
    match stmt {
        // `loop`/`while`: условие продолжения считает тело, а подстановка его гасит -
        // цикл стал бы бесконечным.
        StatementNode::Loop { body, .. } => super::has_return(body),
        StatementNode::For {
            init,
            cond,
            step,
            body,
            ..
        } => {
            if init.as_ref().is_some_and(|s| super::has_return(s)) {
                return true;
            }
            if !super::has_return(body) {
                return return_in_open_loop(body);
            }
            crate::semantic::loop_bounds::bounds(
                init.as_deref(),
                cond.as_deref(),
                step.as_deref(),
                MAX_STATIC_ITERATIONS,
            )
            .is_none()
                || return_in_open_loop(body)
        }
        StatementNode::Block(items) => items.iter().any(return_in_open_loop),
        StatementNode::If { then_, else_, .. } => {
            return_in_open_loop(then_) || else_.as_ref().is_some_and(|s| return_in_open_loop(s))
        }
        StatementNode::Match { arms, .. } => arms.iter().any(|a| return_in_open_loop(&a.body)),
        _ => false,
    }
}

/// Начальное значение временной результата.
///
/// У перечисления это **первый по тексту** вариант, а не ноль: ноль может не
/// принадлежать набору, и `sv` отвечал бы `ENUMVALUE`.
pub(crate) fn default_value(ty: &TypeNode, model: &ModelNode) -> Option<ExpressionNode> {
    match ty {
        TypeNode::Bit | TypeNode::Duration | TypeNode::Fixed { .. } | TypeNode::Integer { .. } => {
            Some(ExpressionNode::Number(0))
        }
        TypeNode::Bool => Some(ExpressionNode::Bool(false)),
        TypeNode::Array(_, elem) if matches!(**elem, TypeNode::Bit) => {
            Some(ExpressionNode::Number(0))
        }
        TypeNode::Enum(name) => model
            .search_enum(name)
            .and_then(|e| crate::semantic::enum_default(&e.variants).map(|(_, v)| v))
            .map(ExpressionNode::Number),
        _ => None,
    }
}

/// Готовая подстановка тела с ранним возвратом.
pub(crate) struct Lowered {
    /// Операторы, встающие перед местом вызова.
    pub(crate) stmts: Vec<StatementNode>,
}

/// Понижает тело в операторы с признаком выхода.
///
/// Имена `ret` и `done` приходят готовыми: их выдаёт тот же счётчик свежих имён, что и
/// остальным именам подстановки (носитель `semantic::fresh`).
pub(crate) fn lower(
    body: &StatementNode,
    ret_name: &str,
    ret_ty: &TypeNode,
    done_name: &str,
    owner: &Rc<RefCell<ModelNode>>,
    model: &ModelNode,
) -> Option<Lowered> {
    let default = default_value(ret_ty, model)?;
    let mut ctx = Lowering {
        ret_name: ret_name.to_string(),
        ret_ty: ret_ty.clone(),
        done_name: done_name.to_string(),
        owner: Rc::clone(owner),
        done_read: false,
    };
    let items = match body {
        StatementNode::Block(items) => items.clone(),
        other => vec![other.clone()],
    };
    let body_stmts = ctx.sequence(&items, true);

    let mut stmts = vec![StatementNode::Variable(
        ret_name.to_string(),
        ret_ty.clone(),
        Some(Box::new(default)),
        Location::Implicit,
    )];
    if ctx.done_read {
        stmts.push(StatementNode::Variable(
            done_name.to_string(),
            TypeNode::Bit,
            Some(Box::new(ExpressionNode::Number(0))),
            Location::Implicit,
        ));
        stmts.extend(body_stmts);
    } else {
        // Признак никто не читает - значит и взводить его незачем: мёртвая запись дала
        // бы у `rust` "value assigned is never read".
        stmts.extend(body_stmts.into_iter().map(|s| strip_done(s, done_name)));
    }
    Some(Lowered { stmts })
}

/// Состояние понижения.
struct Lowering {
    ret_name: String,
    ret_ty: TypeNode,
    done_name: String,
    owner: Rc<RefCell<ModelNode>>,
    /// Признак читается хотя бы одной обёрткой "выхода ещё не было".
    done_read: bool,
}

impl Lowering {
    /// Понижает последовательность операторов одного блока.
    ///
    /// `tail` - стоит ли сам блок в хвостовой позиции тела: тогда его последний
    /// оператор тоже хвостовой, и возврат в нём признак не взводит.
    fn sequence(&mut self, items: &[StatementNode], tail: bool) -> Vec<StatementNode> {
        let mut out = Vec::with_capacity(items.len());
        let mut guarded = false;
        for (index, item) in items.iter().enumerate() {
            let last = tail && index + 1 == items.len();
            let lowered = self.statement(item, last);
            if guarded {
                out.push(self.guarded(lowered));
            } else {
                out.push(lowered);
            }
            if super::has_return(item) {
                guarded = true;
            }
        }
        out
    }

    /// Понижает один оператор.
    ///
    /// `tail` значит "после этого оператора ничего не исполняется": возврат в такой
    /// позиции признак **не взводит**. Иначе у цели `rust` последняя запись мертва, и
    /// `rustc` под `-D warnings` отвечает "value assigned to ... is never read" (замер
    /// 2026-08-31).
    fn statement(&mut self, stmt: &StatementNode, tail: bool) -> StatementNode {
        match stmt {
            StatementNode::Return(Some(expr), _) => {
                let mut out = vec![self.assign(self.ret_ref(), (**expr).clone())];
                if !tail {
                    out.push(self.assign(self.done_ref(), ExpressionNode::Number(1)));
                }
                StatementNode::Block(out)
            }
            StatementNode::Return(None, _) if tail => StatementNode::Block(Vec::new()),
            StatementNode::Return(None, _) => {
                self.assign(self.done_ref(), ExpressionNode::Number(1))
            }
            StatementNode::Block(items) => StatementNode::Block(self.sequence(items, tail)),
            StatementNode::If {
                cond,
                then_,
                else_,
                loc,
            } => StatementNode::If {
                cond: cond.clone(),
                then_: Box::new(self.statement(then_, tail)),
                else_: else_.as_ref().map(|s| Box::new(self.statement(s, tail))),
                loc: *loc,
            },
            StatementNode::Match { expr, arms, loc } => StatementNode::Match {
                loc: *loc,
                expr: expr.clone(),
                arms: arms
                    .iter()
                    .map(|arm| {
                        let mut copy = arm.clone();
                        copy.body = Box::new(self.statement(&arm.body, tail));
                        copy
                    })
                    .collect(),
            },
            // Цикл со статическими границами: тело понижается и целиком уходит под
            // признак выхода - итерации после возврата прокручиваются вхолостую, а
            // завершает цикл его собственный счётчик. Цикл, завершение которого зависит
            // от тела, сюда не доходит: его отсекает `obstacle`.
            //
            // Возврат внутри цикла никогда не хвостовой: после него будут следующие
            // итерации, и признак обязан взводиться.
            StatementNode::For {
                init,
                cond,
                step,
                body,
                loc,
            } if super::has_return(body) => {
                let lowered = self.statement(body, false);
                StatementNode::For {
                    init: init.clone(),
                    cond: cond.clone(),
                    step: step.clone(),
                    body: Box::new(self.guarded(lowered)),
                    // Позиция заголовка переносится: подстановка меняет тело, а не
                    // место цикла.
                    loc: *loc,
                }
            }
            other => other.clone(),
        }
    }

    /// Ставит оператор под условие "выхода ещё не было".
    ///
    /// Условное ветвление **сливается** с обёрткой (`if !done && cond`), а не
    /// вкладывается в неё: вложенная форма валидна у семи потребителей, а `clippy` под
    /// `-D warnings` отвечает `collapsible_if` - "this `if` statement can be collapsed"
    fn guarded(&mut self, lowered: StatementNode) -> StatementNode {
        self.done_read = true;
        let not_done = ExpressionNode::Not(Box::new(self.done_ref()));
        // Блок из одного оператора - это тот же оператор: снятие обёртки открывает
        // слияние условий ниже (тело цикла приходит блоком).
        let lowered = match lowered {
            StatementNode::Block(items) if items.len() == 1 => {
                items.into_iter().next().unwrap_or(StatementNode::None)
            }
            other => other,
        };
        match lowered {
            StatementNode::If {
                cond,
                then_,
                else_: None,
                loc,
            } => StatementNode::If {
                cond: Box::new(ExpressionNode::And(Box::new(not_done), cond)),
                then_,
                else_: None,
                loc,
            },
            other => StatementNode::If {
                cond: Box::new(not_done),
                loc: other.loc(),
                then_: Box::new(StatementNode::Block(vec![other])),
                else_: None,
            },
        }
    }

    /// Присваивание как оператор-выражение.
    fn assign(&self, place: ExpressionNode, value: ExpressionNode) -> StatementNode {
        StatementNode::Expression(
            Box::new(ExpressionNode::Assign(Box::new(place), Box::new(value))),
            Location::Implicit,
        )
    }

    fn ret_ref(&self) -> ExpressionNode {
        self.cell(&self.ret_name, self.ret_ty.clone())
    }

    fn done_ref(&self) -> ExpressionNode {
        self.cell(&self.done_name, TypeNode::Bit)
    }

    /// Ссылка на синтетическую переменную подстановки.
    ///
    /// Своя ячейка на каждое употребление - так же, как их создаёт разрешение
    /// выражений: общей ячейки у ссылок на переменную нет, и идентичность даёт имя.
    fn cell(&self, name: &str, ty: TypeNode) -> ExpressionNode {
        ExpressionNode::Variable(Rc::new(RefCell::new(VariableNode::Simple {
            upper: Some(Rc::downgrade(&self.owner)),
            loc: Location::Implicit,
            name: name.to_string(),
            ty,
            expr: ExpressionNode::None,
        })))
    }
}

/// Убирает записи в признак выхода - когда его никто не читает.
fn strip_done(stmt: StatementNode, done_name: &str) -> StatementNode {
    match stmt {
        StatementNode::Block(items) => StatementNode::Block(
            items
                .into_iter()
                .map(|s| strip_done(s, done_name))
                .filter(|s| !is_done_assignment(s, done_name))
                .collect(),
        ),
        StatementNode::If {
            cond,
            then_,
            else_,
            loc,
        } => StatementNode::If {
            cond,
            then_: Box::new(strip_done(*then_, done_name)),
            else_: else_.map(|s| Box::new(strip_done(*s, done_name))),
            loc,
        },
        StatementNode::Match { expr, arms, loc } => StatementNode::Match {
            loc,
            expr,
            arms: arms
                .into_iter()
                .map(|mut arm| {
                    arm.body = Box::new(strip_done(*arm.body, done_name));
                    arm
                })
                .collect(),
        },
        other => other,
    }
}

/// Оператор - это запись в признак выхода?
fn is_done_assignment(stmt: &StatementNode, done_name: &str) -> bool {
    let StatementNode::Expression(expr, _) = stmt else {
        return false;
    };
    let ExpressionNode::Assign(place, _) = &**expr else {
        return false;
    };
    let ExpressionNode::Variable(cell) = &**place else {
        return false;
    };
    matches!(&*cell.borrow(), VariableNode::Simple { name, .. } if name == done_name)
}
