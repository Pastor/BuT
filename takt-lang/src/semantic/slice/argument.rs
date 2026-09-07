//! Подъём составного значения во временную переменную: срез в аргументе вызова и
//! результат вызова, возвращающего массив либо структуру.
//!
//! Оба правила - один приём: конструкция, которую цели не выражают на месте, заменяется
//! парой "объявление + присваивание", и за границей семантики её не существует. Держать
//! их вместе стоит потому, что механика у них общая (свежее имя, проверенное на
//! занятость; позиция от оператора; обход тел), а разъехавшись, два прохода дали бы два
//! разных имени временной в одном теле.
//!
//! # Правило
//!
//! Проход заводит перед оператором временное объявление и присваивание, а аргумент
//! заменяет ссылкой на него:
//!
//! ```text
//! o := first(src[1:3]);
//! ⇓
//! var <tmp>: [u8; 2];
//! <tmp> := src[1:3];
//! o := first(<tmp>);
//! ```
//!
//! За границей семантики среза в аргументе не существует - печатники целей не трогаются
//! вовсе (приём 0143/0185/0192/0199).
//!
//! **Форма разворота выбрана замером, а не рассуждением.** предлагала объявление **с
//! инициализатором** (`var t: [T; N] := src[a:b];`) - но срез в инициализаторе
//! локального объявления не переводит ни одна цель, и обход, названный там "дешёвым и
//! известным", сам не работал. Работает пара "объявление + присваивание": присваивание
//! среза цели переводят с 0355.
//!
//! Разворот стал возможен только после трёх смежных починок, каждая из которых нашлась
//! прогоном инструментов целей: 0409 (локальный массив в аргументе у `st`), 0410
//! (лишний `mut` и `needless_late_init` у `rust`), 0411 (`E0381` на отложенном массиве
//! у `rust`).
//!
//! # Имя временной переменной
//!
//! `takt_slice_<n>` - и оно **проверяется на занятость**: имя обязано быть
//! допустимым идентификатором **целевых** языков (первая редакция брала `#...`,
//! и `cc` отвечал "expected identifier", а `iec2c` - "invalid variable(s)
//! declaration"), а значит написать такое же может и автор. Занятые имена
//! собираются со всей модели - объявления и локальные переменные тел -
//! **до** обхода; молчаливое затенение чужого имени здесь было бы тем же
//! классом, что `SE-086` у специализации.
//!
//! Позиция - `Location::Implicit`: имени в тексте нет, и ложная координата хуже
//! отсутствующей.

use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

use crate::diagnostics::{Diagnostic, Location};
use crate::semantic::type_node::TypeNode;
use crate::semantic::{
    ExpressionNode, FunctionDefinitionNode, ModelNode, NamedCodeBlockDefinitionNode, StateNode,
    StatementNode, VariableNode,
};

/// Префикс имени временной переменной. Обязан быть допустимым идентификатором
/// **целевых** языков - C, IEC, Rust, SystemVerilog.
const PREFIX: &str = "takt_slice_";

/// Разворачивает срезы в аргументах вызовов по всему дереву.
pub(crate) fn expand_slice_arguments(model: &Rc<RefCell<ModelNode>>) -> Result<(), Diagnostic> {
    let mut visited = HashSet::new();
    // Множество занятых имён и счётчик свежих - общий носитель `semantic::fresh` (он же
    // обслуживает подстановку тела функции, 0444): второй сборщик разошёлся бы с первым
    // молча.
    let taken = crate::semantic::fresh::taken_names(model);
    let mut fresh = crate::semantic::fresh::Fresh::new(PREFIX, &taken);
    let mut ctx = Ctx { fresh: &mut fresh };
    expand_model(model, &mut visited, &mut ctx)
}

/// Состояние обхода: счётчик свежих имён.
struct Ctx<'a> {
    fresh: &'a mut crate::semantic::fresh::Fresh<'a>,
}

impl Ctx<'_> {
    /// Свободное имя временной переменной.
    fn fresh_name(&mut self) -> String {
        self.fresh.fresh_name()
    }
}

fn expand_model(
    model: &Rc<RefCell<ModelNode>>,
    visited: &mut HashSet<*const RefCell<ModelNode>>,
    ctx: &mut Ctx<'_>,
) -> Result<(), Diagnostic> {
    if !visited.insert(Rc::as_ptr(model)) {
        return Ok(()); // разделяемая под-модель уже обойдена
    }
    let nested: Vec<Rc<RefCell<ModelNode>>> = model.borrow().models.values().cloned().collect();
    expand_bodies(model, ctx)?;
    for child in &nested {
        expand_model(child, visited, ctx)?;
    }
    Ok(())
}

/// Обходит тела одной модели.
///
/// Тела **изымаются** на время обхода (`mem::take`) и возвращаются на место: тип базы
/// читает `ModelNode`, и изменяемое заимствование модели этого не допускает (тот же
/// приём, что в `type_node::fixed_body`).
fn expand_bodies(model: &Rc<RefCell<ModelNode>>, ctx: &mut Ctx<'_>) -> Result<(), Diagnostic> {
    let (mut functions, mut named_blocks, mut states) = {
        let mut b = model.borrow_mut();
        (
            std::mem::take(&mut b.functions),
            std::mem::take(&mut b.named_blocks),
            std::mem::take(&mut b.states),
        )
    };
    {
        let borrowed = model.borrow();
        for func in functions.values_mut() {
            if let FunctionDefinitionNode::Local { body, .. } = func {
                expand_stmt(body, &borrowed, model, ctx);
            }
        }
        for blk in named_blocks.iter_mut() {
            expand_block(blk, &borrowed, model, ctx);
        }
        for st in states.values_mut() {
            match st {
                StateNode::Simple { named_blocks, .. }
                | StateNode::Implement { named_blocks, .. } => {
                    for blk in named_blocks.iter_mut() {
                        expand_block(blk, &borrowed, model, ctx);
                    }
                }
                StateNode::Unresolved => {}
            }
        }
    }
    let mut b = model.borrow_mut();
    b.functions = functions;
    b.named_blocks = named_blocks;
    b.states = states;
    Ok(())
}

fn expand_block(
    blk: &mut NamedCodeBlockDefinitionNode,
    model: &ModelNode,
    owner: &Rc<RefCell<ModelNode>>,
    ctx: &mut Ctx<'_>,
) {
    match blk {
        NamedCodeBlockDefinitionNode::Enter { body, .. }
        | NamedCodeBlockDefinitionNode::Exit { body, .. }
        | NamedCodeBlockDefinitionNode::Always { body, .. }
        | NamedCodeBlockDefinitionNode::Unknown { body, .. }
        | NamedCodeBlockDefinitionNode::Every { body, .. } => expand_stmt(body, model, owner, ctx),
        NamedCodeBlockDefinitionNode::None | NamedCodeBlockDefinitionNode::Unresolved(_, _) => {}
    }
}

/// Обходит оператор; развернуть срез можно **только внутри блока** - там есть куда
/// вставить объявление и присваивание.
fn expand_stmt(
    stmt: &mut StatementNode,
    model: &ModelNode,
    owner: &Rc<RefCell<ModelNode>>,
    ctx: &mut Ctx<'_>,
) {
    match stmt {
        StatementNode::Block(items) => {
            let mut out: Vec<StatementNode> = Vec::with_capacity(items.len());
            for mut item in std::mem::take(items) {
                expand_stmt(&mut item, model, owner, ctx);
                let mut prelude = Vec::new();
                lift_in_statement(&mut item, model, owner, ctx, &mut prelude);
                out.extend(prelude);
                out.push(item);
            }
            *items = out;
        }
        StatementNode::If { then_, else_, .. } => {
            expand_stmt(then_, model, owner, ctx);
            if let Some(alt) = else_ {
                expand_stmt(alt, model, owner, ctx);
            }
        }
        StatementNode::Loop { body, .. } => expand_stmt(body, model, owner, ctx),
        StatementNode::For { init, body, .. } => {
            if let Some(i) = init {
                expand_stmt(i, model, owner, ctx);
            }
            expand_stmt(body, model, owner, ctx);
        }
        StatementNode::Match { arms, .. } => {
            for arm in arms.iter_mut() {
                expand_stmt(&mut arm.body, model, owner, ctx);
            }
        }
        _ => {}
    }
}

/// Выносит срезы-аргументы одного оператора в `prelude`.
fn lift_in_statement(
    stmt: &mut StatementNode,
    model: &ModelNode,
    owner: &Rc<RefCell<ModelNode>>,
    ctx: &mut Ctx<'_>,
    prelude: &mut Vec<StatementNode>,
) {
    match stmt {
        // Позиция берётся у самого оператора: у выражения её нет, и синтетическое
        // присваивание обязано указывать туда же, куда исходное.
        StatementNode::Expression(expr, loc) => {
            let at = *loc;
            lift_in_expr(expr, model, owner, ctx, prelude, at)
        }
        StatementNode::Return(Some(expr), _) => {
            lift_in_expr(expr, model, owner, ctx, prelude, Location::Implicit)
        }
        StatementNode::Variable(_, _, Some(init), loc) => {
            let at = *loc;
            lift_in_expr(init, model, owner, ctx, prelude, at)
        }
        _ => {}
    }
}

/// Спускается по выражению и заменяет срез-аргумент ссылкой на временную.
fn lift_in_expr(
    expr: &mut ExpressionNode,
    model: &ModelNode,
    owner: &Rc<RefCell<ModelNode>>,
    ctx: &mut Ctx<'_>,
    prelude: &mut Vec<StatementNode>,
    loc: Location,
) {
    match expr {
        ExpressionNode::Function(func, args) => {
            // Типы параметров - для подъёма агрегата-литерала: тип временной берётся у
            // Объявления, а не угадывается по литералу.
            let params: Vec<TypeNode> = match &*func.borrow() {
                FunctionDefinitionNode::Local { params, .. }
                | FunctionDefinitionNode::External { params, .. } => {
                    params.iter().map(|(_, ty)| ty.clone()).collect()
                }
                _ => Vec::new(),
            };
            for (index, arg) in args.iter_mut().enumerate() {
                // Сперва вложенные вызовы: `outer(inner(src[0:2]))`.
                lift_in_expr(arg, model, owner, ctx, prelude, loc);
                if let Some(replacement) = lift_slice(arg, model, owner, ctx, prelude, loc) {
                    *arg = replacement;
                    continue;
                }
                if let Some(ty) = params.get(index)
                    && let Some(replacement) = lift_aggregate(arg, ty, owner, ctx, prelude, loc)
                {
                    *arg = replacement;
                }
            }
        }
        ExpressionNode::Assign(target, value) => {
            lift_in_expr(target, model, owner, ctx, prelude, loc);
            lift_in_expr(value, model, owner, ctx, prelude, loc);
        }
        // Доступ к полю результата вызова (`make(k).y`) - тот же приём: `iec2c` такую
        // запись отвергает целиком, а у `sv` verilator её принимает и отвечает только
        // yosys.
        ExpressionNode::BitAccess(base, _) => {
            lift_in_expr(base, model, owner, ctx, prelude, loc);
            // Здесь поднимается результат любого типа, не только составного: разряд
            // результата (`twice(k).0`) yosys тоже не принимает, а цель `st` на нём
            // отказывает `ST-011` - при том, что эталон, `c` и `rust` запись исполняют.
            if let Some(replacement) =
                lift_call_value(base, owner, ctx, prelude, loc, CallLift::Any)
            {
                **base = replacement;
            }
        }
        // Индексация результата вызова (`pair(k)[1]`): результат поднимается во
        // временную, и цели видят обычную переменную-массив.
        //
        // Форма выбрана прогоном обоих инструментов SV: `pair(k)[1]` verilator
        // принимает, а yosys отвечает "syntax error, unexpected '['"; `iec2c` ту же
        // запись отвергает целиком.
        ExpressionNode::ArraySubscript(base, index) => {
            lift_in_expr(base, model, owner, ctx, prelude, loc);
            lift_in_expr(index, model, owner, ctx, prelude, loc);
            if let Some(replacement) =
                lift_call_value(base, owner, ctx, prelude, loc, CallLift::Composite)
            {
                **base = replacement;
            }
        }
        ExpressionNode::Parenthesis(inner) => lift_in_expr(inner, model, owner, ctx, prelude, loc),
        // Прочие формы обходить не нужно: вызов с аргументом-срезом либо стоит здесь,
        // либо внутри вызова, разобранного выше. Пропущенная форма даёт прежнее
        // поведение (отказ цели), а не порчу вывода.
        _ => {}
    }
}

/// Строит временную переменную для среза, если это он.
///
/// `None` - аргумент срезом не является либо срез поэлементно невыразим (бит-вектор,
/// 0078): пусть отвечает прежний путь.
fn lift_slice(
    arg: &ExpressionNode,
    model: &ModelNode,
    owner: &Rc<RefCell<ModelNode>>,
    ctx: &mut Ctx<'_>,
    prelude: &mut Vec<StatementNode>,
    loc: Location,
) -> Option<ExpressionNode> {
    let ExpressionNode::ArraySlice(base, from, to) = arg else {
        return None;
    };
    // Длина источника и тип элемента - у общего носителя (0355/0358): второго знания о
    // срезе не заводится.
    let src_ty = crate::semantic::validate::base_type::base_type(base, model)?;
    let src_len = super::elementwise_len(&src_ty)?;
    let TypeNode::Array(_, elem) = &src_ty else {
        return None;
    };
    let (_, len) = super::bounds(*from, *to, src_len);
    let ty = TypeNode::Array(len, elem.clone());

    let name = ctx.fresh_name();
    // Объявление без инициализатора: срез в инициализаторе локального объявления не
    // переводит ни одна цель, а присваивание - переводят все. Форма выбрана замером.
    prelude.push(StatementNode::Variable(
        name.clone(),
        ty.clone(),
        None,
        Location::Implicit,
    ));
    let cell = Rc::new(RefCell::new(VariableNode::Simple {
        upper: Some(Rc::downgrade(owner)),
        loc: Location::Implicit,
        name: name.clone(),
        ty,
        expr: ExpressionNode::None,
    }));
    prelude.push(StatementNode::Expression(
        Box::new(ExpressionNode::Assign(
            Box::new(ExpressionNode::Variable(Rc::clone(&cell))),
            Box::new(arg.clone()),
        )),
        loc,
    ));
    Some(ExpressionNode::Variable(cell))
}

/// Поднимает агрегат-Литерал аргумента во временную переменную.
///
/// `None` - аргумент литералом не является либо тип параметра не составной: пусть
/// отвечает прежний путь.
///
/// Замер 2026-09-02: `pick({1, 2})` при `fn pick(a: Pair)` цель `c` печатала как
/// `Fnstruct_pick({1, 2})` - `cc` отвечает "expected expression" при нулевом коде
/// возврата `taktc`; `st` и `sv` отказывали (`ST-011`, `SV-002`), а эталон и `rust`
/// запись исполняли. Тот же приём, что у среза: за границей семантики агрегата в
/// аргументе не существует.
///
/// Объявление идёт без инициализатора, а значение - присваиванием: агрегат в
/// инициализаторе локального объявления переводят не все цели, а присваивание - все
/// (0345, 0330). Форма та же, что у среза, и по той же причине.
fn lift_aggregate(
    arg: &ExpressionNode,
    ty: &TypeNode,
    owner: &Rc<RefCell<ModelNode>>,
    ctx: &mut Ctx<'_>,
    prelude: &mut Vec<StatementNode>,
    loc: Location,
) -> Option<ExpressionNode> {
    if !matches!(
        arg,
        ExpressionNode::Initializer(_) | ExpressionNode::Array(_)
    ) {
        return None;
    }
    if !matches!(ty, TypeNode::Array(..) | TypeNode::Struct(_)) {
        return None;
    }
    let name = ctx.fresh_name();
    prelude.push(StatementNode::Variable(
        name.clone(),
        ty.clone(),
        None,
        Location::Implicit,
    ));
    let cell = Rc::new(RefCell::new(VariableNode::Simple {
        upper: Some(Rc::downgrade(owner)),
        loc: Location::Implicit,
        name,
        ty: ty.clone(),
        expr: ExpressionNode::None,
    }));
    prelude.push(StatementNode::Expression(
        Box::new(ExpressionNode::Assign(
            Box::new(ExpressionNode::Variable(Rc::clone(&cell))),
            Box::new(arg.clone()),
        )),
        loc,
    ));
    Some(ExpressionNode::Variable(cell))
}

/// Что поднимать: только составной результат либо любой.
#[derive(Clone, Copy, PartialEq, Eq)]
enum CallLift {
    /// Массив и структура (индексация результата, /0432).
    Composite,
    /// Любой тип (доступ к полю и к разряду результата).
    Any,
}

/// Поднимает результат вызова во временную переменную.
///
/// `None` - база не вызов либо её тип под правило не подпадает: пусть отвечает прежний
/// путь (обращение к члену переменной ничего не меняет).
///
/// Бит-вектор (`[bit;N <= 64]`) составным не считается: он скаляр.
fn lift_call_value(
    base: &ExpressionNode,
    owner: &Rc<RefCell<ModelNode>>,
    ctx: &mut Ctx<'_>,
    prelude: &mut Vec<StatementNode>,
    loc: Location,
    what: CallLift,
) -> Option<ExpressionNode> {
    let ExpressionNode::Function(def, _) = base else {
        return None;
    };
    let ty = match &*def.borrow() {
        FunctionDefinitionNode::Local { ret, .. }
        | FunctionDefinitionNode::External { ret, .. } => ret.clone(),
        FunctionDefinitionNode::Builtin(_, _, ret) => ret.clone(),
        FunctionDefinitionNode::None | FunctionDefinitionNode::Unresolved(_) => return None,
    };
    if matches!(ty, TypeNode::Unit) {
        return None;
    }
    if what == CallLift::Composite {
        let composite = matches!(ty, TypeNode::Array(_, _) | TypeNode::Struct(_))
            && crate::semantic::bit_vector::is_bit_vector(&ty).is_none();
        if !composite {
            return None;
        }
    }
    let name = ctx.fresh_name();
    prelude.push(StatementNode::Variable(
        name.clone(),
        ty.clone(),
        None,
        Location::Implicit,
    ));
    let cell = Rc::new(RefCell::new(VariableNode::Simple {
        upper: Some(Rc::downgrade(owner)),
        loc: Location::Implicit,
        name: name.clone(),
        ty,
        expr: ExpressionNode::None,
    }));
    prelude.push(StatementNode::Expression(
        Box::new(ExpressionNode::Assign(
            Box::new(ExpressionNode::Variable(Rc::clone(&cell))),
            Box::new(base.clone()),
        )),
        loc,
    ));
    Some(ExpressionNode::Variable(cell))
}
