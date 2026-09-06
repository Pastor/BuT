//! Трансформация `float → q(m, n)` над семантическим деревом (фича 0096).
//!
//! Делает работу с `float` **прозрачной**: автор пишет вещественный тип, а
//! представление выбирается при компиляции глобальной Q-точностью `(m, n)`
//! (CLI-флаг `--float-as-q=m.n`, задача 0096-01). Проход заменяет каждый
//! [`TypeNode::Rational`] на [`TypeNode::Fixed`]`{ m, n }` и понижает
//! литерал-инициализатор `float`-переменной в **представление** (`Number(repr)`),
//! переиспользуя всю Q-инфраструктуру фичи 0061 ([`lower_fixed_var`],
//! [`lower_fixed_literal`], кодоген `sv_fixed`/`c_expr::fixed`/…, `eval::fixed`).
//!
//! Проход применяется к **обеим** сторонам потактовой сверки — к модели цели
//! **и** к модели эталона-симулятора (`build_unit`), — поэтому сверка ведётся
//! **внутри** Q-режима (ADR 0096, драйвер 2).
//!
//! # Главная засада: переменная существует в ДВУХ представлениях
//!
//! [`VariableNode`] лежит owned в [`ModelNode::variables`] (для объявлений —
//! struct-полей, reset-значений) **и** за `Rc<RefCell<…>>` в
//! [`ExpressionNode::Variable`]/[`ConditionNode::Variable`] (для целей с
//! `fixed_format`, читающих `var.borrow().ty()` при выборе Q-арифметики). Проход
//! обязан мутировать **оба**: иначе объявление станет `q`, а арифметика останется
//! `float` (или наоборот) — молча неверный код (ровно класс дефекта, ради
//! которого ADR требует сверку).
//!
//! # Что понижается, а что нет (паритет с 0061)
//!
//! - **Тип** `Rational → Fixed{m,n}` — везде: объявления переменных/констант,
//!   поля `variables`-map **и** `Rc`-ячейки в выражениях/условиях, цели `Cast`,
//!   типы параметров/возврата функций, локальные `var` в телах.
//! - **Литерал-инициализатор** верхнеуровневой `var`/`const` (`variables`-map)
//!   понижается в `Number(repr)` через [`lower_fixed_var`]. SV печатает `repr`,
//!   симулятор трактует `Number` как **сырое** представление
//!   (`eval::coerce_to_fixed_store`) — согласовано побитово.
//! - **Литералы `Rational` в телах** (арифметика, присваивания) **НЕ** трогаются
//!   — ровно как у явного `q(m, n)` в 0061: `binary` симулятора требует **оба**
//!   операнда `Fixed` (`Fixed + Number` разошёлся бы с SV, где `Number` —
//!   сырой repr). Оставшийся `Rational` в теле → громкая `SV-003`, а не тихий
//!   расчёт. Тела моделей на `float` пишутся на переменных (образец — `regulator`).
//!
//! # Идемпотентность
//!
//! Понижается **только** `Rational` (тип) и литерал-инициализатор. Повторный
//! проход видит `Fixed`/`Number` и ничего не меняет. `Rc`-переменные разделяются
//! между использованиями → `ty`-мутация многократна, но идемпотентна
//! (`Fixed → Fixed`). Разделяемые под-модели (`Rc<RefCell<ModelNode>>` в
//! композиции) обходятся один раз (набор посещённых по указателю).

use crate::diagnostics::Diagnostic;
use crate::semantic::type_node::TypeNode;
use crate::semantic::type_node::type_fixed;
use crate::semantic::{
    ConditionNode, ExpressionNode, FunctionDefinitionNode, MatchPatternNode, ModelNode,
    NamedCodeBlockDefinitionNode, StateNode, StatementNode, VariableNode,
};
use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

/// Понижает все `float` (`TypeNode::Rational`) в дереве `model` в `q(m, n)`
/// (фича 0096). Рекурсивно по вложенным моделям; разделяемые под-модели
/// обходятся однократно.
///
/// # Ошибки
///
/// Пробрасывает `SE-058` из [`lower_fixed_literal`], если литерал-инициализатор
/// `float`-переменной не представим точно в `q(m, n)` глобальной точности или
/// выходит за диапазон типа.
///
/// [`lower_fixed_literal`]: type_fixed::lower_fixed_literal
pub fn lower_float_to_fixed(model: Rc<RefCell<ModelNode>>, m: u8, n: u8) -> Result<(), Diagnostic> {
    let mut visited: HashSet<*const RefCell<ModelNode>> = HashSet::new();
    lower_model(&model, m, n, &mut visited)
}

/// Понижает один уровень модели и рекурсирует во вложенные.
fn lower_model(
    model: &Rc<RefCell<ModelNode>>,
    m: u8,
    n: u8,
    visited: &mut HashSet<*const RefCell<ModelNode>>,
) -> Result<(), Diagnostic> {
    if !visited.insert(Rc::as_ptr(model)) {
        return Ok(()); // разделяемая под-модель уже обработана
    }

    // Снимаем ссылки на вложенные модели до мутирующего borrow_mut: рекурсия в
    // них идёт после закрытия текущего borrow.
    let nested: Vec<Rc<RefCell<ModelNode>>> = model.borrow().models.values().cloned().collect();

    {
        let mut b = model.borrow_mut();

        // 1. Объявления переменных/констант: тип + понижение литерала (variables-map).
        let names: Vec<String> = b.variables.keys().cloned().collect();
        for name in names {
            let cur = b.variables.get(&name).cloned().unwrap();
            let lowered = lower_declared_var(&cur, m, n)?;
            b.variables.insert(name, lowered);
        }

        // 2. Псевдонимы типов (`type real = float;`).
        for ty in b.types.values_mut() {
            *ty = lower_ty(ty, m, n);
        }

        // 3. Функции: сигнатура (params/ret) + обход тела (эмитируется из map).
        let fnames: Vec<String> = b.functions.keys().cloned().collect();
        for fname in fnames {
            let cur = b.functions.get(&fname).cloned().unwrap();
            let lowered = lower_function(cur, m, n)?;
            b.functions.insert(fname, lowered);
        }

        // 4. Именованные блоки уровня модели (`always` и т. п.).
        for blk in b.named_blocks.iter_mut() {
            lower_block(blk, m, n)?;
        }

        // 5. Именованные условия (`cond … = …;`).
        for c in b.conditions.values_mut() {
            lower_cond(&mut c.value, m, n)?;
        }

        // 6. Состояния: тела именованных блоков. Рёбра (`references[].cond`)
        //    хранятся как `Unresolved(ast::Condition)` (инвариант проекта) —
        //    кодоген разрешает их против уже понижённой `variables`-map, поэтому
        //    здесь не трогаются (и обход `object` создал бы цикл по состояниям).
        for st in b.states.values_mut() {
            lower_state(st, m, n)?;
        }
    }

    for child in &nested {
        lower_model(child, m, n, visited)?;
    }
    Ok(())
}

/// `Rational → Fixed{m,n}` (рекурсивно в элемент массива); прочие типы — как есть.
fn lower_ty(ty: &TypeNode, m: u8, n: u8) -> TypeNode {
    match ty {
        // ⚠️ `sat: false` — правило 6 ADR 0170: флаг `--float-as-q` даёт формат
        // БЕЗ насыщения. Признак есть часть типа, который автор пишет сам, а CLI
        // логику автомата не меняет (принцип 0042). Насыщающий регулятор
        // записывается явным `q(m, n) sat`.
        TypeNode::Rational => TypeNode::Fixed { m, n, sat: false },
        TypeNode::Array(k, elem) => TypeNode::Array(*k, Box::new(lower_ty(elem, m, n))),
        other => other.clone(),
    }
}

/// Понижает **объявленную** переменную: меняет тип и, если исходный тип был
/// `float` (`Rational`), понижает литерал-инициализатор в `Number(repr)`
/// (переиспользуя [`lower_fixed_var`](type_fixed::lower_fixed_var), путь 0061).
///
/// ⚠️ Гейт «исходный тип — `Rational`» держит **идемпотентность**: повторный
/// проход видит `Fixed` (не `Rational`) и инициализатор **не** трогает. Без него
/// [`lower_fixed_var`] понизила бы уже понижённый `Number(repr)` **ещё раз**
/// (трактуя repr как целое значение → `repr · 2ⁿ`) — двойное понижение. Тот же
/// гейт защищает **явный** `q(m, n)` 0061: его инициализатор уже понижён на этапе
/// построения, повторное понижение вывело бы значение за диапазон.
fn lower_declared_var(var: &VariableNode, m: u8, n: u8) -> Result<VariableNode, Diagnostic> {
    let was_float = matches!(var_ty(var), Some(TypeNode::Rational));
    let retyped = retype_var(var, m, n);
    if was_float {
        Ok(type_fixed::lower_fixed_var(&retyped)?.unwrap_or(retyped))
    } else {
        Ok(retyped)
    }
}

/// Объявленный тип переменной (`None` для [`VariableNode::Unresolved`]).
fn var_ty(var: &VariableNode) -> Option<&TypeNode> {
    match var {
        VariableNode::Simple { ty, .. }
        | VariableNode::Port { ty, .. }
        | VariableNode::Const { ty, .. } => Some(ty),
        VariableNode::Unresolved => None,
    }
}

/// Меняет **тип** переменной `Rational → Fixed`, инициализатор оставляет как есть.
fn retype_var(var: &VariableNode, m: u8, n: u8) -> VariableNode {
    match var {
        VariableNode::Simple {
            upper,
            loc,
            name,
            ty,
            expr,
        } => VariableNode::Simple {
            upper: upper.clone(),
            loc: *loc,
            name: name.clone(),
            ty: lower_ty(ty, m, n),
            expr: expr.clone(),
        },
        VariableNode::Port {
            upper,
            loc,
            name,
            ty,
            address,
            init,
            direction,
        } => VariableNode::Port {
            upper: upper.clone(),
            loc: *loc,
            name: name.clone(),
            ty: lower_ty(ty, m, n),
            address: address.clone(),
            init: init.clone(),
            direction: *direction,
        },
        VariableNode::Const {
            upper,
            loc,
            name,
            ty,
            expr,
        } => VariableNode::Const {
            upper: upper.clone(),
            loc: *loc,
            name: name.clone(),
            ty: lower_ty(ty, m, n),
            expr: expr.clone(),
        },
        VariableNode::Unresolved => VariableNode::Unresolved,
    }
}

/// Мутирует **тип** `Rc`-ячейки переменной (второе представление, читаемое
/// `fixed_format`/`extract_type`). Только тип: инициализатор ячейки кодогеном не
/// эмитится (объявления идут из `variables`-map), а эталон-симулятор `Rational`
/// приводит масштабированием (`coerce_to_fixed_store`, ветвь `Real`).
fn retype_var_cell(rc: &Rc<RefCell<VariableNode>>, m: u8, n: u8) {
    let cur = rc.borrow().clone();
    *rc.borrow_mut() = retype_var(&cur, m, n);
}

/// Понижает сигнатуру и тело функции (тело эмитируется из `functions`-map).
fn lower_function(
    f: FunctionDefinitionNode,
    m: u8,
    n: u8,
) -> Result<FunctionDefinitionNode, Diagnostic> {
    match f {
        FunctionDefinitionNode::Local {
            upper,
            loc,
            name,
            params,
            ret,
            mut body,
            raw,
        } => {
            let params = params
                .into_iter()
                .map(|(pn, pt)| (pn, lower_ty(&pt, m, n)))
                .collect();
            let ret = lower_ty(&ret, m, n);
            lower_stmt(&mut body, m, n)?;
            Ok(FunctionDefinitionNode::Local {
                upper,
                loc,
                name,
                params,
                ret,
                body,
                // Понижение `float` → `q` меняет типы разрешённого тела; сырое
                // АСД остаётся исходным — константный вычислитель работает с
                // текстом автора, а не с результатом флага сборки (0096).
                raw,
            })
        }
        FunctionDefinitionNode::External {
            upper,
            loc,
            name,
            params,
            ret,
        } => {
            let params = params
                .into_iter()
                .map(|(pn, pt)| (pn, lower_ty(&pt, m, n)))
                .collect();
            let ret = lower_ty(&ret, m, n);
            Ok(FunctionDefinitionNode::External {
                upper,
                loc,
                name,
                params,
                ret,
            })
        }
        // None/Unresolved/Builtin — без разрешённой сигнатуры/тела.
        other => Ok(other),
    }
}

/// Мутирует сигнатуру `Rc`-ячейки функции в узле вызова (для вывода типа
/// возврата на call-site). Тело **не** обходится: эмитируется тело из
/// `functions`-map, а не эта копия.
fn retype_fn_cell(rc: &Rc<RefCell<FunctionDefinitionNode>>, m: u8, n: u8) {
    let mut f = rc.borrow_mut();
    match &mut *f {
        FunctionDefinitionNode::Local { params, ret, .. }
        | FunctionDefinitionNode::External { params, ret, .. } => {
            for p in params.iter_mut() {
                p.1 = lower_ty(&p.1, m, n);
            }
            *ret = lower_ty(ret, m, n);
        }
        _ => {}
    }
}

/// Понижает тело именованного блока (`enter`/`exit`/`always`/`Unknown`).
fn lower_block(blk: &mut NamedCodeBlockDefinitionNode, m: u8, n: u8) -> Result<(), Diagnostic> {
    match blk {
        NamedCodeBlockDefinitionNode::Enter { body, .. }
        | NamedCodeBlockDefinitionNode::Exit { body, .. }
        | NamedCodeBlockDefinitionNode::Always { body, .. }
        | NamedCodeBlockDefinitionNode::Unknown { body, .. }
        | NamedCodeBlockDefinitionNode::Every { body, .. } => lower_stmt(body, m, n),
        NamedCodeBlockDefinitionNode::None | NamedCodeBlockDefinitionNode::Unresolved(_, _) => {
            Ok(())
        }
    }
}

/// Понижает тела именованных блоков состояния.
fn lower_state(st: &mut StateNode, m: u8, n: u8) -> Result<(), Diagnostic> {
    match st {
        StateNode::Simple { named_blocks, .. } | StateNode::Implement { named_blocks, .. } => {
            for blk in named_blocks.iter_mut() {
                lower_block(blk, m, n)?;
            }
            Ok(())
        }
        StateNode::Unresolved => Ok(()),
    }
}

/// Рекурсивно понижает оператор: тип локального `var` + все вложенные выражения.
fn lower_stmt(stmt: &mut StatementNode, m: u8, n: u8) -> Result<(), Diagnostic> {
    match stmt {
        StatementNode::Block(stmts) => {
            for s in stmts.iter_mut() {
                lower_stmt(s, m, n)?;
            }
        }
        StatementNode::Expression(e, _) => lower_expr(e, m, n)?,
        StatementNode::If {
            cond, then_, else_, ..
        } => {
            lower_expr(cond, m, n)?;
            lower_stmt(then_, m, n)?;
            if let Some(e) = else_ {
                lower_stmt(e, m, n)?;
            }
        }
        StatementNode::Loop { cond, body, .. } => {
            if let Some(c) = cond {
                lower_expr(c, m, n)?;
            }
            lower_stmt(body, m, n)?;
        }
        StatementNode::For {
            init,
            cond,
            step,
            body,
            ..
        } => {
            if let Some(s) = init {
                lower_stmt(s, m, n)?;
            }
            if let Some(c) = cond {
                lower_expr(c, m, n)?;
            }
            if let Some(s) = step {
                lower_expr(s, m, n)?;
            }
            lower_stmt(body, m, n)?;
        }
        // Локальное объявление: понижаем **тип**; литерал-инициализатор в теле
        // не понижаем (паритет с 0061 — понижаются лишь `variables`-map).
        StatementNode::Variable(_, ty, init, _) => {
            *ty = lower_ty(ty, m, n);
            if let Some(e) = init {
                lower_expr(e, m, n)?;
            }
        }
        StatementNode::Return(Some(e), _) => lower_expr(e, m, n)?,
        StatementNode::Match { expr, arms, .. } => {
            lower_expr(expr, m, n)?;
            for arm in arms.iter_mut() {
                for p in arm.patterns.iter_mut() {
                    if let MatchPatternNode::Value(e) = p {
                        lower_expr(e, m, n)?;
                    }
                }
                lower_stmt(&mut arm.body, m, n)?;
            }
        }
        // Вставка для цели — операторы Takt (0484): обход спускается в тело.
        // Блок формул адресован внешнему анализатору, операторов Takt в нём нет.
        StatementNode::Assembly { body, .. } => lower_stmt(body, m, n)?,
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

/// Рекурсивно понижает выражение: тип `Rc`-переменных, цель `Cast`, сигнатуры
/// вызываемых функций. Литералы `Rational` в теле **не** трогаются (см. модульную
/// документацию).
fn lower_expr(expr: &mut ExpressionNode, m: u8, n: u8) -> Result<(), Diagnostic> {
    match expr {
        // Бинарные (арифметика, сравнения, логика, битовые, сдвиги, присваивание).
        ExpressionNode::Add(l, r)
        | ExpressionNode::Subtract(l, r)
        | ExpressionNode::Multiply(l, r)
        | ExpressionNode::Divide(l, r)
        | ExpressionNode::Modulo(l, r)
        | ExpressionNode::Power(l, r)
        | ExpressionNode::ShiftLeft(l, r)
        | ExpressionNode::ShiftRight(l, r)
        | ExpressionNode::BitwiseAnd(l, r)
        | ExpressionNode::BitwiseXor(l, r)
        | ExpressionNode::BitwiseOr(l, r)
        | ExpressionNode::Less(l, r)
        | ExpressionNode::More(l, r)
        | ExpressionNode::LessEqual(l, r)
        | ExpressionNode::MoreEqual(l, r)
        | ExpressionNode::Equal(l, r)
        | ExpressionNode::NotEqual(l, r)
        | ExpressionNode::And(l, r)
        | ExpressionNode::Or(l, r)
        | ExpressionNode::Assign(l, r) => {
            lower_expr(l, m, n)?;
            lower_expr(r, m, n)?;
        }
        // Унарные и обёртки.
        ExpressionNode::Parenthesis(e)
        | ExpressionNode::BitAccess(e, _)
        | ExpressionNode::NamedFunctionBox(e, _)
        | ExpressionNode::Not(e)
        | ExpressionNode::BitwiseNot(e)
        | ExpressionNode::UnaryPlus(e)
        | ExpressionNode::Negate(e) => lower_expr(e, m, n)?,
        // Приведение: понижаем операнд и **целевой тип** (`… as float` → `… as q`).
        ExpressionNode::Cast(e, ty) => {
            lower_expr(e, m, n)?;
            *ty = lower_ty(ty, m, n);
        }
        ExpressionNode::ConditionalOperator(c, t, e) => {
            lower_expr(c, m, n)?;
            lower_expr(t, m, n)?;
            lower_expr(e, m, n)?;
        }
        ExpressionNode::CodeBlock(e, stmt) => {
            lower_expr(e, m, n)?;
            lower_stmt(stmt, m, n)?;
        }
        ExpressionNode::Function(f, args) => {
            retype_fn_cell(f, m, n);
            for a in args.iter_mut() {
                lower_expr(a, m, n)?;
            }
        }
        ExpressionNode::Array(args) | ExpressionNode::Initializer(args) => {
            for a in args.iter_mut() {
                lower_expr(a, m, n)?;
            }
        }
        // База — выражение (фича 0358): обходится тем же понижением.
        ExpressionNode::ArraySubscript(base, idx) => {
            lower_expr(base, m, n)?;
            lower_expr(idx, m, n)?;
        }
        ExpressionNode::ArraySlice(base, _, _) => lower_expr(base, m, n)?,
        ExpressionNode::Variable(v) => retype_var_cell(v, m, n),
        // Листья и узлы без `float`-типа.
        ExpressionNode::None
        | ExpressionNode::Unresolved(_)
        | ExpressionNode::Number(_)
        // Понижение `float → q` длительности не касается.
        | ExpressionNode::Duration(_)
        | ExpressionNode::Rational(_, _)
        | ExpressionNode::String(_)
        | ExpressionNode::Type(_)
        | ExpressionNode::Address(_, _)
        // Тип обращения по адресу (фича 0189) задан автором приведением;
        // понижение `float` его не касается — `float` там не выразим.
        | ExpressionNode::AnonPort(_)
        | ExpressionNode::Bool(_)
        | ExpressionNode::Model(_)
        | ExpressionNode::Condition(_)
        | ExpressionNode::List(_) => {}
    }
    Ok(())
}

/// Рекурсивно понижает условие (`cond`-определения; рёбра — `Unresolved`).
fn lower_cond(cond: &mut ConditionNode, m: u8, n: u8) -> Result<(), Diagnostic> {
    match cond {
        ConditionNode::Add(l, r)
        | ConditionNode::Subtract(l, r)
        | ConditionNode::And(l, r)
        | ConditionNode::Or(l, r)
        | ConditionNode::Less(l, r)
        | ConditionNode::More(l, r)
        | ConditionNode::LessEqual(l, r)
        | ConditionNode::MoreEqual(l, r)
        | ConditionNode::Equal(l, r)
        | ConditionNode::NotEqual(l, r) => {
            lower_cond(l, m, n)?;
            lower_cond(r, m, n)?;
        }
        ConditionNode::Parenthesis(e) | ConditionNode::Not(e) | ConditionNode::BitAccess(e, _) => {
            lower_cond(e, m, n)?
        }
        ConditionNode::ArraySubscript(base, idx) => {
            lower_cond(base, m, n)?;
            lower_cond(idx, m, n)?;
        }
        ConditionNode::Function(f, args, _) => {
            retype_fn_cell(f, m, n);
            for a in args.iter_mut() {
                lower_cond(a, m, n)?;
            }
        }
        ConditionNode::Variable(v, _) => retype_var_cell(v, m, n),
        // Вычисляемая выдержка (фича 0183) несёт условие внутри — его переменные
        // подлежат тому же перетипированию, что и все прочие.
        ConditionNode::AfterExpr(inner) => lower_cond(inner, m, n)?,
        // Листья, ссылки на модель/состояние, литералы, `Unresolved` рёбер.
        ConditionNode::None
        | ConditionNode::Unresolved(_)
        | ConditionNode::Number(_)
        | ConditionNode::Duration(_)
        | ConditionNode::After(_)
        | ConditionNode::AfterTicks(_)
        | ConditionNode::Rational(_, _)
        | ConditionNode::String(_)
        | ConditionNode::Bool(_)
        | ConditionNode::AnonPort(_)
        | ConditionNode::Model(_, _)
        | ConditionNode::State(..)
        | ConditionNode::EnumVariant(_, _, _) => {}
    }
    Ok(())
}

// ── Тесты ─────────────────────────────────────────────────────────────────────

/// Применяет трансформацию `float → q(m, n)` (фича 0096), если она включена
/// для цели опциями генерации.
///
/// `embedded_gate` — требуется ли `--float-embedded`: `true` для программных
/// целей `c`/`rust`/`st` (native по умолчанию, Q — только с флагом), `false`
/// для `sv` (нативного `float` там нет, `q` подставляется всегда при заданной
/// точности).
///
/// Без `--float-as-q` не делает ничего (корпус неизменен). Мутирует модель на
/// месте — вызывать **перед** генерацией.
///
/// ⚠️ Живёт здесь, а не в `lib.rs`: тот пришпилен реестром размеров, и место
/// для нового освобождается выносом (фича 0397).
pub(crate) fn apply_float_lowering(
    model: &std::rc::Rc<std::cell::RefCell<crate::semantic::ModelNode>>,
    options: &crate::generator::GenerateOptions,
    embedded_gate: bool,
) -> Result<(), crate::diagnostics::Diagnostic> {
    if let Some((m, n)) = options.float_as_q
        && (!embedded_gate || options.float_embedded)
    {
        lower_float_to_fixed(std::rc::Rc::clone(model), m, n)?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic::tree::construct_model;

    /// Разбирает и строит модель из исходника Takt.
    fn build(src: &str) -> Rc<RefCell<ModelNode>> {
        let (ast, _) = crate::parse(src, 0).expect("разбор");
        construct_model(&ast, None, &[]).expect("семантика")
    }

    /// Тип и инициализатор переменной под-модели `sub` файловой модели.
    fn sub_var(model: &Rc<RefCell<ModelNode>>, sub: &str, var: &str) -> (TypeNode, ExpressionNode) {
        let m = model.borrow();
        let sub = m.models.get(sub).expect("под-модель").borrow();
        match sub.variables.get(var).expect("переменная") {
            VariableNode::Simple { ty, expr, .. } | VariableNode::Const { ty, expr, .. } => {
                (ty.clone(), expr.clone())
            }
            other => panic!("не Simple/Const: {other:?}"),
        }
    }

    const SRC: &str = "
        model M {
            var a: float := 1.5;
            var b: float := 0.0;
            start S { always { b := a + a; } }
        }
        start Entry = M;
    ";

    /// `float`-объявление → `Fixed{8,8}`, литерал-инициализатор → `Number(repr)`.
    /// `1.5` в q(8,8) = 384; `0.0` = 0.
    #[test]
    fn float_var_becomes_fixed_with_repr() {
        let model = build(SRC);
        lower_float_to_fixed(model.clone(), 8, 8).expect("float → q(8,8)");
        let (ty_a, init_a) = sub_var(&model, "M", "a");
        assert_eq!(
            ty_a,
            TypeNode::Fixed {
                m: 8,
                n: 8,
                sat: false
            }
        );
        assert_eq!(init_a, ExpressionNode::Number(384));
        let (ty_b, init_b) = sub_var(&model, "M", "b");
        assert_eq!(
            ty_b,
            TypeNode::Fixed {
                m: 8,
                n: 8,
                sat: false
            }
        );
        assert_eq!(init_b, ExpressionNode::Number(0));
    }

    /// Идемпотентность: второй проход не понижает `Number` повторно (иначе
    /// `repr · 2ⁿ` — двойное понижение) и не трогает уже `Fixed`.
    #[test]
    fn transformation_is_idempotent() {
        let model = build(SRC);
        lower_float_to_fixed(model.clone(), 8, 8).expect("проход 1");
        lower_float_to_fixed(model.clone(), 8, 8).expect("проход 2");
        let (ty_a, init_a) = sub_var(&model, "M", "a");
        assert_eq!(
            ty_a,
            TypeNode::Fixed {
                m: 8,
                n: 8,
                sat: false
            }
        );
        assert_eq!(
            init_a,
            ExpressionNode::Number(384),
            "повторный проход не должен давать 384·256"
        );
    }

    /// Разная точность даёт разное представление того же литерала: `1.5` в
    /// q(4,4) = 24 (1.5·16), в q(16,16) = 98304 (1.5·65536).
    #[test]
    fn precision_scales_representation() {
        let m4 = build(SRC);
        lower_float_to_fixed(m4.clone(), 4, 4).expect("q(4,4)");
        assert_eq!(sub_var(&m4, "M", "a").1, ExpressionNode::Number(24));

        let m16 = build(SRC);
        lower_float_to_fixed(m16.clone(), 16, 16).expect("q(16,16)");
        assert_eq!(sub_var(&m16, "M", "a").1, ExpressionNode::Number(98304));
    }

    /// Rc-ячейка переменной в теле (`b := a + a`) тоже получает тип `Fixed` —
    /// иначе арифметика цели осталась бы `float` при `q`-объявлении (молча
    /// неверный код). Проверяем через `extract_type` тела блока `always`.
    #[test]
    fn variable_cell_in_body_is_retyped() {
        use crate::semantic::type_inference::extract_type;
        let model = build(SRC);
        lower_float_to_fixed(model.clone(), 8, 8).expect("float → q(8,8)");
        let sub = model.borrow().models.get("M").expect("под-модель").clone();
        // Достаём выражение `a + a` из блока always состояния S.
        let sub_b = sub.borrow();
        let state = sub_b.states.get("S").expect("состояние S");
        let StateNode::Simple { named_blocks, .. } = state else {
            panic!("S не Simple");
        };
        let body = named_blocks
            .iter()
            .find_map(|b| b.statement())
            .expect("тело always");
        // always { b := a + a; } → Block[Expression(Assign(b, Add(a, a)))]
        let StatementNode::Block(stmts) = body else {
            panic!("тело не блок");
        };
        let StatementNode::Expression(e, _) = &stmts[0] else {
            panic!("не выражение");
        };
        let ExpressionNode::Assign(_, rhs) = e.as_ref() else {
            panic!("не присваивание");
        };
        // rhs = a + a; его тип обязан быть Fixed{8,8} (Rc-ячейки понижены).
        let ty = extract_type(rhs, sub.clone()).expect("тип rhs");
        assert_eq!(
            ty,
            TypeNode::Fixed {
                m: 8,
                n: 8,
                sat: false
            }
        );
    }

    /// Литерал вне диапазона точности → `SE-058` (проброс из `lower_fixed_literal`),
    /// а не тихое округление. `200.0` в q(8,8) (max ≈ 127.996) непредставимо.
    #[test]
    fn out_of_range_literal_is_se058() {
        let model = build(
            "
            model M { var a: float := 200.0; start S {} }
            start Entry = M;
            ",
        );
        let err = lower_float_to_fixed(model, 8, 8).unwrap_err();
        assert_eq!(err.code.as_deref(), Some("SE-058"));
    }
}
