//! Табличная форма автомата у цели `rust`.
//!
//! # Идиома принадлежит целевому языку
//!
//! Строки таблицы одни у всех целей - их собирает общий носитель
//! [`generator::table`](crate::generator::table). Форма же у каждой цели своя: в C это
//! массив структур с **указателями на функции**, здесь - `static` с номерами стража и
//! действия плюс два метода, разбирающих номер `match`-ем.
//!
//! **Указателей на функции здесь быть не может, и это не вкус.** Такт под-модели
//! обобщён по HAL (`fn tick<H: Hal>(...)`), то есть тип стража зависит от параметра
//! **метода**; элемент с таким типом нельзя объявить ни `const`, ни `static` (параметры
//! метода в них не видны), а собирать массив указателей на каждом такте - плата за
//! форму, которой у формы `switch` нет. Номер + `match` даёт ту же таблицу-данные без
//! этой платы.
//!
//! **Поля печатаются по нужде.** Поле, которого никто не читает, - это `dead_code`, то
//! есть отказ проверки цели (`-D warnings`): если у модели нет ни одного стража, поля
//! `guard` в строке нет вовсе, и метода `guard` - тоже.

use crate::diagnostics::Diagnostic;
use crate::generator::indent::Printer;
use crate::generator::rust::rust_blocks::emit_named_blocks;
use crate::generator::rust::rust_chain::{concat_steps, node_done, seq_enum_name, seq_field_name};
use crate::generator::rust::rust_ctx::ModelEmit;
use crate::generator::rust::rust_expr::{Scope, condition_as_bool, unwrap_outer};
use crate::generator::rust::rust_stmt::StmtOutput;
use crate::generator::table::{self, Row, RowTarget};
use crate::semantic::minimap::{Name, StateExtend};

/// Печатает методы табличной формы: стражи, действия и диспетчер.
///
/// Зовётся внутри `impl` модели, сразу после `tick`.
pub(crate) fn emit_methods(
    p: &mut Printer,
    ctx: &ModelEmit,
    scope: &mut Scope,
    warnings: &mut Vec<Diagnostic>,
) -> Result<(), Diagnostic> {
    let rows = expressible_rows(ctx)?;
    if rows.is_empty() {
        return Ok(());
    }
    let mut out = StmtOutput::default();
    let params = params(ctx);
    let args = args(ctx);
    let generics = if needs_hal_param(ctx) { "<H: Hal>" } else { "" };

    let guards = guard_texts(ctx, &rows, scope)?;
    if !guards.is_empty() {
        // Тело печатается в буфер: по нему решается, нужна ли заглушка неиспользуемого
        // параметра (приём - признак берётся у напечатанного текста, а не у дерева).
        let mut body = String::new();
        {
            let mut buffered = p.fork(&mut body);
            buffered.up();
            buffered.ident("match id {").nl();
            buffered.up();
            for (id, text) in &guards {
                buffered.ident(&format!("{id} => {text},")).nl();
            }
            buffered.ident("_ => true,").nl();
            buffered.down();
            buffered.ident("}").nl();
            buffered.down();
        }
        p.ident(&format!(
            "fn takt_guard{generics}(&mut self, id: u16{params}) -> bool {{"
        ))
        .nl();
        p.up();
        emit_unused_params(p, ctx, &body);
        p.down();
        p.print(&body);
        p.ident("}").nl().nl();
    }

    let actions = action_ids(&rows);
    if !actions.is_empty() {
        let mut body = String::new();
        {
            let mut buffered = p.fork(&mut body);
            buffered.up();
            // Единственное действие печатается `if`, а не `match`: `match` с одной
            // ветвью и `_ => {}` - это `clippy::single_match`, то есть отказ проверки цели
            // (`-D warnings`). Форма выбрана прогоном clippy, а не вкусом.
            let single = actions.len() == 1;
            if single {
                buffered.ident(&format!("if id == {} {{", actions[0])).nl();
            } else {
                buffered.ident("match id {").nl();
            }
            buffered.up();
            for id in &actions {
                let row = &rows[*id];
                if !single {
                    buffered.ident(&format!("{id} => {{")).nl();
                    buffered.up();
                }
                emit_named_blocks(
                    &mut buffered,
                    &row.exit_state.borrow(),
                    "exit",
                    scope,
                    &mut out,
                )?;
                if let Some(enter) = &row.enter_state {
                    emit_named_blocks(&mut buffered, &enter.borrow(), "enter", scope, &mut out)?;
                }
                if !single {
                    buffered.down();
                    buffered.ident("}").nl();
                }
            }
            if !single {
                buffered.ident("_ => {}").nl();
            }
            buffered.down();
            buffered.ident("}").nl();
            buffered.down();
        }
        p.ident(&format!(
            "fn takt_action{generics}(&mut self, id: u16{params}) {{"
        ))
        .nl();
        p.up();
        emit_unused_params(p, ctx, &body);
        p.down();
        p.print(&body);
        p.ident("}").nl().nl();
    }

    let mut body = String::new();
    {
        let mut buffered = p.fork(&mut body);
        buffered.up();
        buffered
            .ident(&format!("for row in {}.iter() {{", static_name(ctx)))
            .nl();
        buffered.up();
        buffered.ident("if row.from != self.state {").nl();
        buffered.up();
        buffered.ident("continue;").nl();
        buffered.down();
        buffered.ident("}").nl();
        if !guards.is_empty() {
            buffered
                .ident(&format!(
                    "if !self.takt_guard(row.guard{}) {{",
                    args_call(&args)
                ))
                .nl();
            buffered.up();
            buffered.ident("continue;").nl();
            buffered.down();
            buffered.ident("}").nl();
        }
        if !actions.is_empty() {
            buffered
                .ident(&format!(
                    "self.takt_action(row.action{});",
                    args_call(&args)
                ))
                .nl();
        }
        buffered.ident("self.state = row.to;").nl();
        buffered.ident("return;").nl();
        buffered.down();
        buffered.ident("}").nl();
        buffered.down();
    }
    p.ident(&format!("fn takt_dispatch{generics}(&mut self{params}) {{"))
        .nl();
    p.up();
    emit_unused_params(p, ctx, &body);
    p.down();
    p.print(&body);
    p.ident("}").nl().nl();
    warnings.append(&mut out.warnings);
    Ok(())
}

/// Печатает данные таблицы: тип строки и `static` с самими строками.
///
/// Данные живут на уровне модуля, а не внутри `impl`: тип строки не зависит от HAL, а
/// `static` не пересобирается на каждом такте.
pub(crate) fn emit_data(p: &mut Printer, ctx: &ModelEmit) -> Result<(), Diagnostic> {
    let rows = expressible_rows(ctx)?;
    if rows.is_empty() {
        return Ok(());
    }
    let has_guard = rows.iter().any(|r| r.done.is_some() || r.cond.is_some());
    let has_action = !action_ids(&rows).is_empty();
    let state_enum = &ctx.table.enum_name;
    let row_type = row_type_name(ctx);

    p.ident(&format!("struct {row_type} {{")).nl();
    p.up();
    p.ident(&format!("from: {state_enum},")).nl();
    if has_guard {
        p.ident("guard: u16,").nl();
    }
    if has_action {
        p.ident("action: u16,").nl();
    }
    p.ident(&format!("to: {state_enum},")).nl();
    p.down();
    p.ident("}").nl().nl();

    p.ident(&format!(
        "static {}: [{row_type}; {}] = [",
        static_name(ctx),
        rows.len()
    ))
    .nl();
    p.up();
    for (index, row) in rows.iter().enumerate() {
        let mut cells = vec![format!("from: {}", ctx.table.path_of(&row.from)?)];
        if has_guard {
            cells.push(format!("guard: {}", guard_id(&rows, index)));
        }
        if has_action {
            cells.push(format!("action: {}", index));
        }
        cells.push(format!("to: {}", target_path(ctx, row)?));
        p.ident(&format!("{row_type} {{ {} }},", cells.join(", ")))
            .nl();
    }
    p.down();
    p.ident("];").nl().nl();
    Ok(())
}

/// Номер стража строки: у безусловной строки без реализации - "страж не нужен", и им
/// служит номер, которого нет в `match` (ветвь `_ => true`).
fn guard_id(rows: &[Row], index: usize) -> usize {
    let row = &rows[index];
    if row.done.is_some() || row.cond.is_some() {
        index
    } else {
        rows.len()
    }
}

/// Номера строк, у которых есть действие (блоки `exit`/`enter`).
fn action_ids(rows: &[Row]) -> Vec<usize> {
    let mut ids = Vec::new();
    for (index, row) in rows.iter().enumerate() {
        let has_exit = !row.exit_state.borrow().get_named_blocks("exit").is_empty();
        let has_enter = row
            .enter_state
            .as_ref()
            .is_some_and(|s| !s.borrow().get_named_blocks("enter").is_empty());
        if has_exit || has_enter {
            ids.push(index);
        }
    }
    ids
}

/// Тексты стражей: номер строки -> выражение `bool`.
fn guard_texts(
    ctx: &ModelEmit,
    rows: &[Row],
    scope: &mut Scope,
) -> Result<Vec<(usize, String)>, Diagnostic> {
    let mut out = Vec::new();
    for (index, row) in rows.iter().enumerate() {
        let done = match &row.done {
            None => None,
            Some((state, extend)) => done_predicate(ctx, state, extend)?,
        };
        let cond = match &row.cond {
            None => None,
            Some((cond, loc)) => Some(condition_as_bool(cond, scope).map_err(|di| {
                let target = row
                    .target_name()
                    .map(|n| n.local().to_string())
                    .unwrap_or_else(|| "END".to_string());
                Diagnostic::error_with_note(
                    *loc,
                    format!(
                        "условный переход в состояние '{target}' не переводится в Rust: {}",
                        di.message
                    ),
                    di.loc,
                    match &di.code {
                        Some(code) => format!("причина [{}]: {}", code, di.message),
                        None => format!("причина: {}", di.message),
                    },
                )
                .with_code("RS-020")
            })?),
        };
        let text = match (done, cond) {
            (None, None) => continue,
            (Some(done), None) => done,
            (None, Some(cond)) => unwrap_outer(&cond).to_string(),
            (Some(done), Some(cond)) => format!("{done} && ({})", unwrap_outer(&cond)),
        };
        out.push((index, text));
    }
    Ok(out)
}

/// Строки, выразимые целью: у состояния, чью реализацию цель не тикает, перехода нет -
/// как и в форме `match`.
fn expressible_rows(ctx: &ModelEmit) -> Result<Vec<Row>, Diagnostic> {
    let mut kept = Vec::new();
    for row in table::rows(ctx.element, ctx.map)? {
        let keep = match &row.done {
            None => true,
            Some((state, extend)) => done_predicate(ctx, state, extend)?.is_some(),
        };
        if keep {
            kept.push(row);
        }
    }
    Ok(kept)
}

/// Предикат "реализация состояния завершена".
///
/// Берётся у тех же полей, по которым печатается тик реализации
/// (`rust_tick::emit_extend`): экземпляры под-моделей и поле шага цепочки. Второе
/// знание о раскладке разошлось бы с печатью такта при первой правке.
fn done_predicate(
    ctx: &ModelEmit,
    state: &Name,
    extend: &StateExtend,
) -> Result<Option<String>, Diagnostic> {
    if !ctx
        .instances
        .iter()
        .any(|(n, _)| n.unique() == state.unique())
    {
        return Ok(None);
    }
    let prefix = state.local_lowercase_snakecase();
    match extend {
        StateExtend::None => Ok(None),
        // Готовность узла считает общий носитель: вложенная цепочка отвечает своим
        // счётчиком, а не конъюнкцией `is_done()` своих моделей, и глубина у неё любая.
        StateExtend::Model(_, _) | StateExtend::Parallel(_) => {
            let done = node_done(ctx.name, state, extend, &mut Vec::new(), &prefix)?;
            if done.is_empty() {
                return Ok(None);
            }
            Ok(Some(done.join(" && ")))
        }
        // У цепочки состояния готовность - последний шаг, доигранный до конца:
        // терминального варианта `Done` у неё нет (выход печатает сам последний шаг).
        StateExtend::Concatenation(items) => {
            let steps = concat_steps(items, &prefix, state)?;
            let Some(last) = steps.last() else {
                return Ok(None);
            };
            let mut done = vec![format!(
                "self.{} == {}::{}",
                seq_field_name(state, &[])?,
                seq_enum_name(ctx.name, state, &[])?,
                last.variant
            )];
            let site = items
                .iter()
                .enumerate()
                .filter(|(_, item)| !matches!(item, StateExtend::None))
                .map(|(idx, _)| idx)
                .nth(steps.len() - 1)
                .unwrap_or(0);
            done.extend(node_done(
                ctx.name,
                state,
                &last.node,
                &mut vec![site],
                &last.prefix,
            )?);
            Ok(Some(done.join(" && ")))
        }
    }
}

/// Путь варианта состояния-приёмника строки.
fn target_path(ctx: &ModelEmit, row: &Row) -> Result<String, Diagnostic> {
    match &row.to {
        RowTarget::State(name) => ctx.table.path_of(name),
        RowTarget::End => Ok(ctx.table.end_path()),
    }
}

/// Имя `static` с таблицей переходов модели.
fn static_name(ctx: &ModelEmit) -> String {
    format!("{}_TRANSITIONS", ctx.name.unique_uppercase_snakecase())
}

/// Имя типа строки таблицы.
fn row_type_name(ctx: &ModelEmit) -> String {
    format!("{}Transition", ctx.name.unique_camelcase())
}

/// Нужен ли методам параметр HAL (у корня он - поле `self.hal`).
fn needs_hal_param(ctx: &ModelEmit) -> bool {
    !ctx.is_root && ctx.uses_hal
}

/// Параметры методов таблицы - те же, что у `tick`.
fn params(ctx: &ModelEmit) -> String {
    let mut params = String::new();
    if !ctx.is_root && !ctx.shared.is_empty() {
        params.push_str(&format!(
            ", shared: &mut {}",
            crate::generator::rust::rust_shared::shared_type_name(ctx.map)
        ));
    }
    if needs_hal_param(ctx) {
        params.push_str(", hal: &mut H");
    }
    params
}

/// Аргументы вызова методов таблицы из такта.
fn args(ctx: &ModelEmit) -> Vec<&'static str> {
    let mut args = Vec::new();
    if !ctx.is_root && !ctx.shared.is_empty() {
        args.push("shared");
    }
    if needs_hal_param(ctx) {
        args.push("hal");
    }
    args
}

/// Хвост списка аргументов вызова (`, shared, hal`).
fn args_call(args: &[&'static str]) -> String {
    if args.is_empty() {
        String::new()
    } else {
        format!(", {}", args.join(", "))
    }
}

/// Печатает вызов диспетчера в такте.
pub(crate) fn emit_dispatch_call(p: &mut Printer, ctx: &ModelEmit) -> Result<(), Diagnostic> {
    if expressible_rows(ctx)?.is_empty() {
        return Ok(());
    }
    let args = args(ctx);
    p.ident(&format!("self.takt_dispatch({});", args.join(", ")))
        .nl();
    Ok(())
}

/// Гасит неиспользуемые параметры метода-стража.
///
/// Признак - упоминание имени в **напечатанных** текстах стражей (приём ):
/// неиспользуемый параметр под `-D warnings` - отказ проверки цели.
fn emit_unused_params(p: &mut Printer, ctx: &ModelEmit, body: &str) {
    if !ctx.is_root && !ctx.shared.is_empty() && !mentions(body, "shared") {
        p.ident("let _ = shared;").nl();
    }
    if needs_hal_param(ctx) && !mentions(body, "hal") {
        p.ident("let _ = hal;").nl();
    }
}

/// Встречается ли идентификатор в тексте как отдельное слово.
fn mentions(text: &str, ident: &str) -> bool {
    text.split(|c: char| !(c.is_alphanumeric() || c == '_'))
        .any(|word| word == ident)
}
