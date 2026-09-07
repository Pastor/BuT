//! Интерпретатор операторов и вызовов функций.
//!
//! # Один интерпретатор, а не два
//!
//! Тела блоков (`enter`/`exit`/`always`) и тела функций исполняет **одна и та же**
//! функция [`exec_statement`]. Соблазн был скомпилировать блоки в замыкания (как
//! раньше), а тела `fn` интерпретировать отдельно - но это ровно тот грех, ради
//! устранения которого принят: два механизма расходятся. Поэтому "компиляция" блока
//! свелась к тонкой обёртке [`compile_block_body`], которая заводит область видимости и
//! зовёт тот же интерпретатор.
//!
//! # Область видимости
//!
//! `var` не должна попадать в переменные модели, поэтому тело блока исполняется в
//! [`BlockScope`] - **одном на весь блок** (имена собираются рекурсивно на этапе
//! компиляции). Область плоская осознанно: запись в **не-локальное** имя обязана
//! попадать в `write_ctx` (контекст модели), а не в контекст чтения (юнит) - на этом
//! держится видимость переменных между **параллельными** моделями, то есть сценарии
//! `stacker_*`.
//!
//! Тело функции исполняется в [`FunctionScope`]: параметры и локальные `var` - свои, а
//! запись в прочие имена делегируется наружу, в контекст вызывающего (и далее в
//! `write_ctx`).

use crate::context::Context;
use crate::eval::ops;
use crate::eval::place::PlaceSegment;
use crate::eval::value::Value;
use crate::expression::eval_expression;
use crate::predicate::create_predicate;
use crate::unit::{Execution, Flow};
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::rc::Rc;
use takt_lang::diagnostics::{Diagnostic, Location};
use takt_lang::parser::ast::Member;
use takt_lang::semantic::formula::Formula;
use takt_lang::semantic::type_node::TypeNode;
use takt_lang::semantic::{
    ExpressionNode, FunctionDefinitionNode, MatchPatternNode, StatementNode, VariableNode,
};

/// Предохранитель от зацикливания: симулятор не имеет права зависнуть.
const MAX_ITERATIONS: u32 = 100_000;

/// S10: предел глубины рекурсии - симулятор не имеет права переполнить стек.
const MAX_CALL_DEPTH: u32 = 256;

thread_local! {
    /// Текущая глубина вложенных вызовов функций.
    static CALL_DEPTH: Cell<u32> = const { Cell::new(0) };
}

// -- Области видимости ---------------------------------------------------------

/// Область видимости тела блока: локальные `var` + запись наружу в `write_ctx`.
struct BlockScope<'a> {
    locals: HashMap<String, Value>,
    outer: &'a mut dyn Context,
    write: Rc<RefCell<dyn Context>>,
}

impl Context for BlockScope<'_> {
    /// Стенд внешних функций - у объемлющего контекста.
    ///
    /// Без этой передачи стенд не доезжает: тело блока и тело функции
    /// исполняются в своей области видимости, и вызов `extern fn` спрашивает
    /// **её**, а не модель. Умолчание трейта (`None`) означало бы "стенда нет"
    /// -- прогон отвечал бы `SIM-019` при заданной подмене.
    fn extern_result(&self, name: &str, args: &[Value]) -> Option<Value> {
        self.outer.extern_result(name, args)
    }

    fn since_state_entry_ns(&self) -> i64 {
        self.outer.since_state_entry_ns()
    }

    fn ticks_in_state(&self) -> u64 {
        self.outer.ticks_in_state()
    }

    /// Состояние другой модели прогона - у объемлющего контекста.
    ///
    /// Как и стенд `extern`, умолчание трейта здесь означало бы "модель не запущена":
    /// условие `S(Модель) = Состояние` внутри `if` тела блока отвечало бы `SIM-036` на
    /// живой модели.
    fn model_state(&self, model: &str) -> Option<String> {
        self.outer.model_state(model)
    }

    fn set_model_state(&self, model: &str, state: &str) {
        self.outer.set_model_state(model, state);
    }

    fn emit_output(&self, text: &str) {
        self.outer.emit_output(text);
    }

    fn take_output(&self) -> Vec<String> {
        self.outer.take_output()
    }

    fn get_value(&self, name: &str) -> Option<Value> {
        self.locals
            .get(name)
            .cloned()
            .or_else(|| self.outer.get_value(name))
    }

    fn set_value(&mut self, name: &str, value: Value) {
        if self.locals.contains_key(name) {
            self.locals.insert(name.to_string(), value);
        } else {
            // Не локальная - пишем туда же, куда писала прежняя реализация.
            self.write.borrow_mut().set_value(name, value);
        }
    }

    /// Реестр структур берётся из `write` - контекста модели, единого источника, - а не
    /// из `outer`: им может быть `Unit`, структур не знающий, и его пришлось бы растить
    /// методом `find_struct`.
    fn find_struct(&self, name: &str) -> Option<takt_lang::semantic::StructDefinitionNode> {
        self.write.borrow().find_struct(name)
    }
}

/// Область видимости тела функции: параметры и локальные `var`.
struct FunctionScope<'a> {
    locals: HashMap<String, Value>,
    outer: &'a mut dyn Context,
}

impl Context for FunctionScope<'_> {
    /// Стенд внешних функций - у объемлющего контекста.
    ///
    /// Без этой передачи стенд не доезжает: тело блока и тело функции
    /// исполняются в своей области видимости, и вызов `extern fn` спрашивает
    /// **её**, а не модель. Умолчание трейта (`None`) означало бы "стенда нет"
    /// -- прогон отвечал бы `SIM-019` при заданной подмене.
    fn extern_result(&self, name: &str, args: &[Value]) -> Option<Value> {
        self.outer.extern_result(name, args)
    }

    fn since_state_entry_ns(&self) -> i64 {
        self.outer.since_state_entry_ns()
    }

    fn ticks_in_state(&self) -> u64 {
        self.outer.ticks_in_state()
    }

    /// Состояние другой модели прогона - у объемлющего контекста.
    ///
    /// Как и стенд `extern`, умолчание трейта здесь означало бы "модель не запущена":
    /// условие `S(Модель) = Состояние` внутри `if` тела блока отвечало бы `SIM-036` на
    /// живой модели.
    fn model_state(&self, model: &str) -> Option<String> {
        self.outer.model_state(model)
    }

    fn set_model_state(&self, model: &str, state: &str) {
        self.outer.set_model_state(model, state);
    }

    fn emit_output(&self, text: &str) {
        self.outer.emit_output(text);
    }

    fn take_output(&self) -> Vec<String> {
        self.outer.take_output()
    }

    fn get_value(&self, name: &str) -> Option<Value> {
        self.locals
            .get(name)
            .cloned()
            .or_else(|| self.outer.get_value(name))
    }

    fn set_value(&mut self, name: &str, value: Value) {
        if self.locals.contains_key(name) {
            self.locals.insert(name.to_string(), value);
        } else {
            // Запись в глобальное имя из тела функции уходит наружу по цепочке.
            self.outer.set_value(name, value);
        }
    }

    fn find_struct(&self, name: &str) -> Option<takt_lang::semantic::StructDefinitionNode> {
        self.outer.find_struct(name)
    }
}

// -- Служебное -----------------------------------------------------------------

/// Значение локальной переменной до инициализации.
fn default_value(ty: &TypeNode) -> Value {
    match ty {
        TypeNode::Bool => Value::Boolean(false),
        TypeNode::Rational => Value::Real(0.0),
        _ => Value::Number(0),
    }
}

/// Собирает имена, объявленные в дереве оператора (область плоская).
fn collect_locals(stmt: &StatementNode, out: &mut Vec<(String, Value)>) {
    match stmt {
        StatementNode::Variable(name, ty, _, _) => out.push((name.clone(), default_value(ty))),
        // Вставка: эталон исполняет только безымянную - он не является ни одной из
        // целей. Объявления именованной вставки в область не идут: её тело эталон не
        // исполняет.
        StatementNode::Assembly { target, body, .. } => {
            if target.is_none() {
                collect_locals(body, out);
            }
        }
        StatementNode::Block(stmts) => stmts.iter().for_each(|s| collect_locals(s, out)),
        StatementNode::If { then_, else_, .. } => {
            collect_locals(then_, out);
            if let Some(else_) = else_ {
                collect_locals(else_, out);
            }
        }
        StatementNode::Loop { body, .. } => collect_locals(body, out),
        StatementNode::For { init, body, .. } => {
            if let Some(init) = init {
                collect_locals(init, out);
            }
            collect_locals(body, out);
        }
        StatementNode::Match { arms, .. } => {
            arms.iter().for_each(|arm| collect_locals(&arm.body, out));
        }
        StatementNode::None
        | StatementNode::Unresolved(_)
        | StatementNode::Expression(_, _)
        | StatementNode::Return(_, _)
        | StatementNode::Break(_)
        | StatementNode::Continue(_)
        | StatementNode::Formula(_)
        | StatementNode::InlineFormula(_) => {}
    }
}

// -- Точка входа: тело именованного блока -------------------------------------

/// Оборачивает тело блока в область видимости и зовёт интерпретатор.
pub(crate) fn compile_block_body(
    stmt: &StatementNode,
    write_ctx: Rc<RefCell<dyn Context>>,
) -> Vec<Execution> {
    if matches!(stmt, StatementNode::None) {
        return vec![];
    }
    let stmt = stmt.clone();
    let mut declared = Vec::new();
    collect_locals(&stmt, &mut declared);
    let f: Execution = Rc::new(move |ctx: &mut dyn Context| {
        let mut scope = BlockScope {
            locals: declared.iter().cloned().collect(),
            outer: ctx,
            write: write_ctx.clone(),
        };
        // Ошибка пробрасывается наверх: до она печаталась в stderr и терялась. Теперь
        // её увидит `TickResult::Failed` -> CLI.
        exec_statement(&stmt, &mut scope).map(|_| Flow::Normal)
    });
    vec![f]
}

// -- Интерпретатор -------------------------------------------------------------

/// Исполняет оператор.
///
/// Разбор исчерпывающий: каждый вариант `StatementNode` обработан явно.
pub(crate) fn exec_statement(
    stmt: &StatementNode,
    ctx: &mut dyn Context,
) -> Result<Flow, Diagnostic> {
    match stmt {
        StatementNode::None | StatementNode::Unresolved(_) => Ok(Flow::Normal),
        // Обязательство внешнему анализатору: эталон его не проверяет - поведение
        // модели блок не меняет, как и у восьми целей.
        StatementNode::Formula(_) => Ok(Flow::Normal),
        // Вставка: эталон - не цель, поэтому исполняет только безымянную. Исполни он
        // тело, помеченное `"c"`, сверка `c` ↔ эталон сошлась бы, а `rust` ↔ эталон
        // разошлась бы: вердикт сверки зависел бы от того, какую цель ей поручили.
        StatementNode::Assembly { target, body, .. } => {
            if target.is_none() {
                exec_statement(body, ctx)
            } else {
                Ok(Flow::Normal)
            }
        }
        StatementNode::Block(stmts) => {
            for stmt in stmts {
                match exec_statement(stmt, ctx)? {
                    Flow::Normal => {}
                    // Поток управления пробрасывается наружу до цикла/функции.
                    flow => return Ok(flow),
                }
            }
            Ok(Flow::Normal)
        }
        StatementNode::Expression(expr, _) => exec_expression(expr, ctx),
        StatementNode::If {
            cond, then_, else_, ..
        } => {
            if eval_bool(cond, ctx)? {
                exec_statement(then_, ctx)
            } else if let Some(else_) = else_ {
                exec_statement(else_, ctx)
            } else {
                Ok(Flow::Normal)
            }
        }
        StatementNode::Variable(name, ty, init, _) => {
            let Some(init) = init else {
                // Без инициализатора значение уже расставлено областью видимости.
                return Ok(Flow::Normal);
            };
            let value = eval_expression(init, ctx)?;
            let value = crate::context::coerce_via(ctx, value, ty)
                .map_err(|e| e.to_diagnostic(Location::Builtin))?;
            ctx.set_value(name, value);
            Ok(Flow::Normal)
        }
        StatementNode::Loop { cond, body, .. } => exec_loop(cond.as_deref(), body, ctx),
        StatementNode::For {
            init,
            cond,
            step,
            body,
            ..
        } => exec_for(init.as_deref(), cond.as_deref(), step.as_deref(), body, ctx),
        StatementNode::Match { expr, arms, .. } => {
            let subject = eval_expression(expr, ctx)?;
            for arm in arms {
                if arm_matches(&arm.patterns, &subject, ctx)? {
                    return exec_statement(&arm.body, ctx);
                }
            }
            Ok(Flow::Normal)
        }
        StatementNode::Return(expr, _) => {
            let value = match expr {
                Some(expr) => Some(eval_expression(expr, ctx)?),
                None => None,
            };
            Ok(Flow::Return(value))
        }
        StatementNode::Break(_) => Ok(Flow::Break),
        StatementNode::Continue(_) => Ok(Flow::Continue),
        // `assert` языка Takt (`: c;`, `: [Guard] c;`) проверяется в точке записи - как
        // `assert()` в порождённом C. Нарушение даёт `SIM-025` и доходит до
        // `TickResult::Failed`; ошибка вычисления самого условия - свой код `SIM`.
        // Темпоральная формула здесь пропускается: её проверяет `taktc verify`.
        StatementNode::InlineFormula(formulas) => {
            for f in formulas {
                if let Formula::Guard(cond, name, _) = f {
                    let pred = create_predicate(cond);
                    match pred.evaluate(ctx)? {
                        true => {}
                        false => {
                            let named =
                                name.as_ref().map(|n| format!(" '{n}'")).unwrap_or_default();
                            return Err(Diagnostic::error(
                                Location::Implicit,
                                format!("нарушен инвариант{named}"),
                            )
                            .with_code("SIM-025"));
                        }
                    }
                }
            }
            Ok(Flow::Normal)
        }
    }
}

/// Раскладывает левую часть присваивания в корневую переменную + путь полей.
///
/// `Ok(Some((var, [сегменты])))` для `Variable` (path пуст), `p.x` (`[Field(x)]`),
/// `o.i.v` (`[Field(i), Field(v)]`), `data[i]` (`[Index(i)]`, индекс **вычислен** через
/// `ctx`), `data[i].f` (`[Index(i), Field(f)]`); `Ok(None)` для прочих lvalue - их
/// запись даёт `SIM-017`. `Err` - индекс не вычислился/не целый (`SIM-010`). Разбор
/// рекурсивный (не строковый разбор имени).
///
/// **Под `Ok(None)` остаётся ровно одна законная форма** - запись среза (`arr[0:2] :=
/// 1`), которую не переводит ни один потребитель (`CC-022`, `RS-011`, `ST-011`,
/// `SV-002`). Запись бита исполняется с, а всё, что не есть место записи вовсе,
/// отвергает семантика (`SE-111`, `SE-112`) ещё до прогона.
fn resolve_place<'a>(
    lhs: &'a ExpressionNode,
    path: &mut Vec<PlaceSegment>,
    ctx: &mut dyn Context,
) -> Result<Option<&'a Rc<RefCell<VariableNode>>>, Diagnostic> {
    match lhs {
        ExpressionNode::Variable(v) => Ok(Some(v)),
        ExpressionNode::Parenthesis(inner) => resolve_place(inner, path, ctx),
        ExpressionNode::BitAccess(inner, Member::Identifier(field)) => {
            let root = resolve_place(inner, path, ctx)?;
            if root.is_some() {
                path.push(PlaceSegment::Field(field.name.clone()));
            }
            Ok(root)
        }
        // Запись одного разряда: `flags.3 := 1`. Сегмент кладётся тем же порядком, что
        // поле и индекс, - правило замены живёт в ядре (`eval/place.rs`), адаптер
        // только раскладывает путь.
        ExpressionNode::BitAccess(inner, Member::Number(bit)) => {
            let root = resolve_place(inner, path, ctx)?;
            if root.is_some() {
                path.push(PlaceSegment::Bit(*bit));
            }
            Ok(root)
        }
        // Запись в элемент массива: индекс вычисляем тем же вычислителем, что и чтение
        // (`expression.rs`), чтобы `data[i] := ...` и `data[i]` смотрели на одну
        // переменную. Индекс - часть места, а не корня, поэтому кладём его сегментом.
        // База - Выражение: корень места ищется в ней рекурсивно, как у поля и разряда,
        // поэтому `b.data[1] := 5;` кладёт путь `.data[1]` от корня `b`.
        ExpressionNode::ArraySubscript(base, index_expr) => {
            let loc = base.loc();
            let root = resolve_place(base, path, ctx)?;
            if root.is_none() {
                return Ok(None);
            }
            let Value::Number(idx) = eval_expression(index_expr, ctx)? else {
                return Err(
                    Diagnostic::error(loc, "индекс массива должен быть целым".to_string())
                        .with_code("SIM-010"),
                );
            };
            let Ok(index) = usize::try_from(idx) else {
                return Err(
                    Diagnostic::error(loc, format!("индекс {idx} вне границ массива"))
                        .with_code("SIM-010"),
                );
            };
            path.push(PlaceSegment::Index(index));
            Ok(root)
        }
        _ => Ok(None),
    }
}

/// Позиция для диагностики `SIM-017` (неподдержанная левая часть присваивания) - вместо
/// `Location::Builtin`.
fn loc_of_assign(lhs: &ExpressionNode) -> Location {
    match lhs {
        ExpressionNode::Variable(v) => v.borrow().loc(),
        // База - выражение: позиция берётся у неё.
        ExpressionNode::ArraySubscript(base, _) | ExpressionNode::ArraySlice(base, _, _) => {
            loc_of_assign(base)
        }
        ExpressionNode::Parenthesis(inner) | ExpressionNode::BitAccess(inner, _) => {
            loc_of_assign(inner)
        }
        _ => Location::Builtin,
    }
}

/// Исполняет выражение-оператор.
///
/// Присваивание - с приведением к типу цели. Прочие выражения (в первую очередь
/// вызовы функций, Д3) вычисляются ради побочного эффекта.
fn exec_expression(expr: &ExpressionNode, ctx: &mut dyn Context) -> Result<Flow, Diagnostic> {
    match expr {
        ExpressionNode::Assign(lhs, rhs) => {
            // Запись по анонимному адресу: ячейка моделируется синтетическим портом,
            // поэтому "место" ей не нужно - значение кладётся под именем ячейки и видно
            // в трассе.
            if let ExpressionNode::AnonPort(access) = lhs.as_ref() {
                let value = eval_expression(rhs, ctx)?;
                let value = crate::context::coerce_via(ctx, value, &access.ty)
                    .map_err(|e| e.to_diagnostic(takt_lang::diagnostics::Location::Implicit))?;
                crate::anon_cell::write(access, value, ctx);
                return Ok(Flow::Normal);
            }
            // Левая часть раскладывается в корень + путь сегментов (`p.x` -> root `p`,
            // `[Field(x)]`; `data[i]` -> `[Index(i)]`). Присваивание всей переменной -
            // path пуст (сюда же копия структуры `q := p`).
            let mut path: Vec<PlaceSegment> = Vec::new();
            let Some(var_rc) = resolve_place(lhs, &mut path, ctx)? else {
                // Текст называет единственную оставшуюся форму. Срез не переводит ни
                // один потребитель, поэтому отказ эталона идёт заодно с целями, а не
                // вместо них.
                return Err(Diagnostic::error(
                    loc_of_assign(lhs),
                    "запись в срез массива ('x[a:b] := …') не поддерживается: срез не \
                     переводит ни одна цель генерации, и эталон отказывает вместе с \
                     ними. Присвойте элементы по отдельности ('x[a] := …')"
                        .to_string(),
                )
                .with_code("SIM-017"));
            };
            let (name, ty, loc) = {
                let b = var_rc.borrow();
                (b.name().to_string(), b.ty().clone(), b.loc())
            };
            let rhs_value = eval_expression(rhs, ctx)?;
            if path.is_empty() {
                // Присваивание всей переменной (в т.ч. копия структуры `q := p`).
                let value = crate::context::coerce_via(ctx, rhs_value, &ty)
                    .map_err(|e| e.to_diagnostic(loc))?;
                ctx.set_value(&name, value);
            } else {
                // Запись в поле или элемент массива: читаем текущее значение, обновляем
                // по пути сегментов (лист приводится к типу поля/элемента), пишем
                // обратно. `ty` корня даёт тип элемента.
                let current = ctx.get_value(&name).ok_or_else(|| {
                    Diagnostic::error(loc, format!("переменная '{name}' не найдена"))
                        .with_code("SIM-009")
                })?;
                let updated =
                    crate::context::update_place_via(ctx, current, Some(&ty), &path, rhs_value)
                        .map_err(|e| e.to_diagnostic(loc))?;
                ctx.set_value(&name, updated);
            }
            Ok(Flow::Normal)
        }
        // Д3: вызов-оператор (`log_temp(x);`) - раньше молча отбрасывался.
        _ => {
            eval_expression(expr, ctx)?;
            Ok(Flow::Normal)
        }
    }
}

fn exec_loop(
    cond: Option<&ExpressionNode>,
    body: &StatementNode,
    ctx: &mut dyn Context,
) -> Result<Flow, Diagnostic> {
    let mut iterations = 0_u32;
    loop {
        if let Some(cond) = cond
            && !eval_bool(cond, ctx)?
        {
            break;
        }
        match exec_statement(body, ctx)? {
            Flow::Normal | Flow::Continue => {}
            Flow::Break => break,
            // `return` изнутри цикла выходит из функции целиком.
            flow @ Flow::Return(_) => return Ok(flow),
        }
        iterations += 1;
        if iterations >= MAX_ITERATIONS {
            return Err(Diagnostic::error(
                Location::Builtin,
                format!("превышен предел итераций цикла ({MAX_ITERATIONS})"),
            )
            .with_code("SIM-018"));
        }
    }
    Ok(Flow::Normal)
}

fn exec_for(
    init: Option<&StatementNode>,
    cond: Option<&ExpressionNode>,
    step: Option<&ExpressionNode>,
    body: &StatementNode,
    ctx: &mut dyn Context,
) -> Result<Flow, Diagnostic> {
    if let Some(init) = init {
        exec_statement(init, ctx)?;
    }
    let mut iterations = 0_u32;
    loop {
        if let Some(cond) = cond
            && !eval_bool(cond, ctx)?
        {
            break;
        }
        match exec_statement(body, ctx)? {
            Flow::Normal | Flow::Continue => {}
            Flow::Break => break,
            flow @ Flow::Return(_) => return Ok(flow),
        }
        if let Some(step) = step {
            exec_expression(step, ctx)?;
        }
        iterations += 1;
        if iterations >= MAX_ITERATIONS {
            return Err(Diagnostic::error(
                Location::Builtin,
                format!("превышен предел итераций цикла for ({MAX_ITERATIONS})"),
            )
            .with_code("SIM-018"));
        }
    }
    Ok(Flow::Normal)
}

fn arm_matches(
    patterns: &[MatchPatternNode],
    subject: &Value,
    ctx: &mut dyn Context,
) -> Result<bool, Diagnostic> {
    for pattern in patterns {
        let matched = match pattern {
            MatchPatternNode::Wildcard => true,
            MatchPatternNode::Value(expr) => {
                let value = eval_expression(expr, ctx)?;
                let equal = ops::apply_binary(ops::BinOp::Equal, subject, &value)
                    .map_err(|e| e.to_diagnostic(Location::Builtin))?;
                ops::to_bool(&equal).map_err(|e| e.to_diagnostic(Location::Builtin))?
            }
        };
        if matched {
            return Ok(true);
        }
    }
    Ok(false)
}

fn eval_bool(expr: &ExpressionNode, ctx: &mut dyn Context) -> Result<bool, Diagnostic> {
    let value = eval_expression(expr, ctx)?;
    ops::to_bool(&value).map_err(|e| e.to_diagnostic(Location::Builtin))
}

// -- Вызов функции -------------------------------------------------------------

/// Вызывает функцию: связывает параметры, исполняет тело, возвращает значение.
///
/// Используется обоими адаптерами - выражений и условий (Д3, Д4).
pub(crate) fn call_function(
    func: &Rc<RefCell<FunctionDefinitionNode>>,
    args: &[Value],
    ctx: &mut dyn Context,
) -> Result<Value, Diagnostic> {
    let (params, ret, body, loc, name) = {
        let borrowed = func.borrow();
        match &*borrowed {
            FunctionDefinitionNode::Local {
                params,
                ret,
                body,
                loc,
                name,
                ..
            } => (
                params.clone(),
                ret.clone(),
                body.clone(),
                *loc,
                name.clone(),
            ),
            // Тела нет: процедура - no-op; функция со значением - отказ, а не тихий
            // ноль (решение ).
            FunctionDefinitionNode::External { ret, loc, name, .. } => {
                // Стенд сценария: значение задаёт шаг - там же, где задаются входы
                // портов. Умолчания нет: не задали - прежний отказ. Ноль по умолчанию
                // сделал бы прогон зелёным при расхождении с прошивкой.
                if let Some(value) = ctx.extern_result(name, args) {
                    return Ok(value);
                }
                return if matches!(ret, TypeNode::Unit) {
                    Ok(Value::Number(0))
                } else {
                    Err(Diagnostic::error(
                        *loc,
                        format!(
                            "внешняя функция '{name}' не имеет тела: симуляция значения невозможна"
                        ),
                    )
                    .with_code("SIM-019"))
                };
            }
            // Встроенные функции. Вычисление живёт в `eval::builtin` и идёт через те же
            // операции, что исполняют `<` и унарный минус в теле модели, - иначе эталон
            // разошёлся бы с целями на q-формате, насыщении и обёртке.
            //
            // `debug` сюда не доходит: его перехватывает вычислитель выражений до
            // вычисления аргументов (строковый литерал значением не представим).
            // Остаётся `S(Модель)` в позиции выражения - её не переводит ни одна цель
            // (`CC-022`, `SV-002`, отказ у `rust`), поэтому эталон отказывает тоже:
            // симулятор, "умеющий больше" целей, дал бы ложную уверенность.
            FunctionDefinitionNode::Builtin(name, _, _) => {
                if let Some(value) = crate::eval::builtin::apply(name, args)
                    .map_err(|e| e.to_diagnostic(Location::Builtin))?
                {
                    return Ok(value);
                }
                // Ветвь недостижима из корректной программы, и это проверено:
                // единственная невычислимая встроенная - `S`, а её аргумент-модель
                // отвергает вычислитель выражений раньше (`модель как выражение`); цель
                // `c` тот же вход отвергает `CC-022`. Отказ оставлен страховкой - как
                // `CC-023` у цели `c`: молчаливое значение развело бы трассу эталона с
                // прошивкой.
                //
                // Достижима она была через вызов с неверным числом аргументов
                // (`min(1)`): арность не проверял никто, и текст, говорящий про
                // `S(Модель)`, объяснял автору **чужой** случай. Вход закрыт
                // (`SE-122`), а сообщение больше не называет причину, которой может не
                // быть.
                return Err(Diagnostic::error(
                    Location::Builtin,
                    format!(
                        "встроенная функция '{name}' значения в этой позиции не \
                         имеет — вычислить его нечем"
                    ),
                )
                .with_code("SIM-020"));
            }
            FunctionDefinitionNode::None | FunctionDefinitionNode::Unresolved(_) => {
                return Err(Diagnostic::error(
                    Location::Builtin,
                    "неразрешённая функция не может быть вызвана".to_string(),
                )
                .with_code("SIM-016"));
            }
        }
    };

    if params.len() != args.len() {
        return Err(Diagnostic::error(
            loc,
            format!(
                "функция '{name}': ожидалось аргументов {}, передано {}",
                params.len(),
                args.len()
            ),
        )
        .with_code("SIM-021"));
    }

    // S10: без предела рекурсия переполнила бы стек - симулятор упал бы вместо
    // диагностики.
    let depth = CALL_DEPTH.with(|d| {
        let next = d.get() + 1;
        d.set(next);
        next
    });
    let result = call_local(&params, &ret, &body, args, ctx, depth, loc, &name);
    CALL_DEPTH.with(|d| d.set(d.get() - 1));
    result
}

#[allow(clippy::too_many_arguments)]
fn call_local(
    params: &[(String, TypeNode)],
    ret: &TypeNode,
    body: &StatementNode,
    args: &[Value],
    ctx: &mut dyn Context,
    depth: u32,
    loc: Location,
    name: &str,
) -> Result<Value, Diagnostic> {
    if depth > MAX_CALL_DEPTH {
        return Err(Diagnostic::error(
            loc,
            format!("превышена глубина рекурсии ({MAX_CALL_DEPTH}) при вызове '{name}'"),
        )
        .with_code("SIM-022"));
    }

    // Параметры приводятся к объявленным типам - как при записи в переменную.
    let mut locals = HashMap::new();
    for ((param, ty), value) in params.iter().zip(args) {
        let coerced =
            crate::context::coerce_via(ctx, value.clone(), ty).map_err(|e| e.to_diagnostic(loc))?;
        locals.insert(param.clone(), coerced);
    }
    let mut declared = Vec::new();
    collect_locals(body, &mut declared);
    locals.extend(declared);

    let mut scope = FunctionScope { locals, outer: ctx };
    let flow = exec_statement(body, &mut scope)?;

    match flow {
        Flow::Return(Some(value)) => {
            crate::context::coerce_via(&scope, value, ret).map_err(|e| e.to_diagnostic(loc))
        }
        // Процедура без значения либо `return;` - числовой ноль как нейтральное
        // значение (вызов-оператор результат игнорирует).
        Flow::Return(None) | Flow::Normal => {
            if matches!(ret, TypeNode::Unit) {
                Ok(Value::Number(0))
            } else {
                Err(
                    Diagnostic::error(loc, format!("функция '{name}' не вернула значение"))
                        .with_code("SIM-023"),
                )
            }
        }
        Flow::Break | Flow::Continue => Err(Diagnostic::error(
            loc,
            format!("'{name}': break/continue вне цикла"),
        )
        .with_code("SIM-024")),
    }
}
