//! Табличная форма автомата у цели `c`.
//!
//! # Правило
//!
//! Порядок строк **совпадает** с порядком, в котором цель печатает переходы формой
//! `switch`: рёбра `ref` в порядке объявления, затем `next`, затем подстановка `END`
//! терминальному состоянию. Диспетчер берёт **первую** строку, у которой `from` равен
//! текущему состоянию и страж истинен, - поэтому поведение обеих форм тождественно.
//! Тест этому - потактовая сверка (`conformance_fsm_table_tests`), а не факт
//! компиляции: таблица, переставившая две строки, собирается тем же `cc` без замечаний.
//!
//! **Тела состояний остаются в `switch`.** Таблицей выражается **переход**, а не
//! содержимое такта: блоки `always`/`every`, тик реализации состояния и проверки формул
//! печатает прежний печатник. Иначе завелось бы второе знание о теле - /0193/0195.
//!
//! **Композиция выражается таблицей вся**. Завершение реализации - предикат: у
//! реализации одной моделью это `M_is_done(&model->x)`, у параллели - конъюнкция
//! готовностей ветвей, у цепочки - "цепочка на последнем шаге, и он завершён". Машина
//! шагов при этом остаётся в теле такта: она ведёт переходы **внутри** состояния, а
//! таблица - выход **наружу**. Отказ `CC-025`, которым называла границу, выведен:
//! входа, который таблицей не выражается, не осталось.

use crate::diagnostics::Diagnostic;
use crate::generator::indent::Printer;
use crate::semantic::minimap::{Element, Name, StateExtend};

use crate::generator::table::{self, Row, RowTarget};

use super::c_blocks::generate_named_blocks;
use super::c_expr::generate_condition_expr;
use super::c_map::CMap;

/// Печатает таблицу переходов модели, стражи, действия и диспетчер.
///
/// Зовётся **перед** `_init`: всё напечатанное здесь статично и обязано стоять выше
/// `_tick`, который зовёт диспетчер.
pub(super) fn emit_transition_table(
    printer: &mut Printer,
    model: &Element,
    map: &CMap,
    wants_root: bool,
) -> Result<(), Diagnostic> {
    // Строки собирает общий носитель: порядок задан правилами языка, и цели по нему
    // расходиться нечем. Цели остаётся печать.
    let collected = expressible_rows(model, map)?;
    let struct_name = model.name().unique_camelcase();
    let table_name = format!("{}_TRANSITIONS", model.name().unique_uppercase_snakecase());
    // Указатель на корень печатается по той же нужде, что у `_tick`: страж и действие -
    // вынесенные наружу куски его тела.
    let params = signature(&struct_name, map, wants_root);
    let args = if wants_root { "model, main" } else { "model" };

    printer
        .print(&format!("typedef bool (*{struct_name}_Guard)({params});"))
        .nl();
    printer
        .print(&format!("typedef void (*{struct_name}_Action)({params});"))
        .nl();
    printer.print("typedef struct {").nl();
    printer.up();
    printer.ident("int from;").nl();
    printer.ident(&format!("{struct_name}_Guard guard;")).nl();
    printer.ident(&format!("{struct_name}_Action action;")).nl();
    printer.ident("int to;").nl();
    printer.down();
    printer
        .print(&format!("}} {struct_name}_Transition;"))
        .nl()
        .nl();

    let mut cells = Vec::new();
    for (index, row) in collected.iter().enumerate() {
        let guard = emit_guard(printer, row, map, model, (&struct_name, &params), index)?;
        let action = emit_action(printer, row, map, model, (&struct_name, &params), index)?;
        cells.push((
            row.from.unique_uppercase_snakecase(),
            guard,
            action,
            target_variant(model, row),
        ));
    }

    if cells.is_empty() {
        // Переходов у модели нет: массив нулевой длины в C недопустим, поэтому таблицы
        // не печатается вовсе, а диспетчер пуст. Форма такта при этом одна на все
        // модели - вызов из `_tick` остаётся.
        printer
            .print(&format!("static void {struct_name}_dispatch({params}) {{"))
            .nl();
        printer.up().ident("(void)model;").nl();
        if wants_root {
            printer.ident("(void)main;").nl();
        }
        printer.down().print("}").nl().nl();
        return Ok(());
    }

    printer
        .print(&format!(
            "static const {struct_name}_Transition {table_name}[] = {{"
        ))
        .nl();
    printer.up();
    for (from, guard, action, to) in &cells {
        printer
            .ident(&format!(
                "{{ {from}, {}, {}, {to} }},",
                guard.as_deref().unwrap_or("0"),
                action.as_deref().unwrap_or("0")
            ))
            .nl();
    }
    printer.down();
    printer.print("};").nl().nl();

    printer
        .print(&format!("static void {struct_name}_dispatch({params}) {{"))
        .nl();
    printer.up();
    printer
        .ident(&format!(
            "const unsigned count = sizeof({table_name}) / sizeof({table_name}[0]);"
        ))
        .nl();
    printer.ident("unsigned index = 0;").nl();
    printer.ident("for (; index < count; index++) {").nl();
    printer.up();
    printer
        .ident(&format!(
            "const {struct_name}_Transition *row = &{table_name}[index];"
        ))
        .nl();
    printer
        .ident("if (row->from != (int)model->state) {")
        .nl()
        .up()
        .ident("continue;")
        .nl()
        .down()
        .ident("}")
        .nl();
    printer
        .ident(&format!("if (row->guard != 0 && !row->guard({args})) {{"))
        .nl()
        .up()
        .ident("continue;")
        .nl()
        .down()
        .ident("}")
        .nl();
    printer
        .ident("if (row->action != 0) {")
        .nl()
        .up()
        .ident(&format!("row->action({args});"))
        .nl()
        .down()
        .ident("}")
        .nl();
    printer.ident("model->state = row->to;").nl();
    printer.ident("return;").nl();
    printer.down();
    printer.ident("}").nl();
    printer.down();
    printer.print("}").nl().nl();
    Ok(())
}

/// Печатает вызов диспетчера в теле `_tick` (после `switch` по состоянию).
pub(super) fn emit_dispatch_call(printer: &mut Printer, model: &Element, wants_root: bool) {
    let args = if wants_root { "model, main" } else { "model" };
    printer
        .ident(&format!(
            "{}_dispatch({args});",
            model.name().unique_camelcase()
        ))
        .nl();
}

/// Нужен ли модели указатель на корень в `_tick`.
///
/// Носитель один на две точки: сигнатуру стража с действием и вызов диспетчера.
/// Разъедься они - вывод не собрался бы ("too few arguments").
pub(super) fn wants_root(model: &Element, map: &CMap) -> bool {
    if model.name().eq(&map.root_name()) {
        return false;
    }
    map.raw_model_at(model.name().clone()).is_ok_and(|rc| {
        super::c_needs::model_fn_needs_root(
            &rc,
            super::c_needs::ModelFn::Tick,
            super::c_time::is_clock_profile(map),
        )
    })
}

/// Сигнатура стража, действия и диспетчера модели.
fn signature(struct_name: &str, map: &CMap, wants_root: bool) -> String {
    if wants_root {
        format!(
            "{struct_name} *model, {} *main",
            map.root_name().unique_camelcase()
        )
    } else {
        format!("{struct_name} *model")
    }
}

/// Строки таблицы, выразимые целью `c`.
///
/// Носитель строк не знает языка цели, поэтому фильтр "завершение реализации выразимо"
/// живёт здесь: состояние, чью реализацию печать такта не ведёт (вложенная цепочка
/// шагом), строк не даёт - ровно как в форме `switch`, где переход ему не печатается
/// вовсе.
fn expressible_rows(model: &Element, map: &CMap) -> Result<Vec<Row>, Diagnostic> {
    let is_main = model.name().eq(&map.root_name());
    let mut kept = Vec::new();
    for row in table::rows(model, map)? {
        let expressible = match &row.done {
            None => true,
            Some((state, extend)) => matches!(
                done_predicate(extend, state, map, is_main)?,
                Done::Predicate(_)
            ),
        };
        if expressible {
            kept.push(row);
        }
    }
    Ok(kept)
}

/// Перечислитель состояния-приёмника строки.
fn target_variant(model: &Element, row: &Row) -> String {
    match &row.to {
        RowTarget::State(name) => name.unique_uppercase_snakecase(),
        RowTarget::End => format!("{}_END", model.name().unique_uppercase_snakecase()),
    }
}

/// Ответ о завершении реализации состояния.
enum Done {
    /// Предикат завершения - выражение C для стража строки.
    Predicate(String),
    /// Перехода у состояния нет вовсе: строк не будет, и это не отказ - форма `switch`
    /// на таком входе тоже не печатает перехода.
    NoTransition,
}

/// Предикат "реализация состояния завершена" для строки таблицы.
///
/// Реализация одной моделью даёт `M_is_done(&model->x)`; параллель - конъюнкцию
/// готовностей своих ветвей, цепочка - "цепочка на последнем шаге, и он завершён". Все
/// три берутся **у тех же носителей**, что печатают тик:
/// `c_compose::generate_parallel_items_tick` и `c_chain` - своё знание о раскладке
/// полей разошлось бы с ними при первой же правке.
fn done_predicate(
    extend: &StateExtend,
    state_name: &Name,
    map: &CMap,
    is_main: bool,
) -> Result<Done, Diagnostic> {
    match extend {
        StateExtend::Model(name, _) => Ok(Done::Predicate(format!(
            "{}_is_done(&model->{})",
            name.unique_camelcase(),
            state_name.local_lowercase_snakecase()
        ))),
        StateExtend::Parallel(steps) => {
            // Текст тиков здесь не нужен - нужен только список готовностей, который
            // носитель возвращает попутно. Печать уходит в буфер и выбрасывается: тела
            // состояний печатает `c_model`, и второй их копии в выводе быть не должно.
            let mut sink = String::new();
            let mut scratch = Printer::new(4, &mut sink);
            let access = format!("model->{}", state_name.local_lowercase_snakecase());
            let upper = state_name.unique_uppercase_snakecase();
            let done = super::c_compose::generate_parallel_items_tick(
                &mut scratch,
                map,
                &access,
                &upper,
                steps,
                is_main,
            );
            if done.is_empty() {
                return Ok(Done::NoTransition);
            }
            Ok(Done::Predicate(done.join(" && ")))
        }
        // Цепочка `A + B`: машина шагов остаётся в теле такта - она ведёт переходы
        // Внутри состояния, - а наружу состояние уходит при одном условии: "цепочка на
        // последнем шаге, и он завершён". Это же условие печатает форма `switch`, и
        // берётся оно у общего носителя имён `c_chain`.
        StateExtend::Concatenation(items) => Ok(super::c_chain::chain_done(
            map,
            (
                &state_name.local_lowercase_snakecase(),
                &state_name.unique_uppercase_snakecase(),
            ),
            items,
            is_main,
        )
        .map_or(Done::NoTransition, Done::Predicate)),
        StateExtend::None => Ok(Done::NoTransition),
    }
}

/// Печатает функцию-страж строки; `None` - строка безусловна.
fn emit_guard(
    printer: &mut Printer,
    row: &Row,
    map: &CMap,
    model: &Element,
    (struct_name, params): (&str, &str),
    index: usize,
) -> Result<Option<String>, Diagnostic> {
    let is_main = model.name().eq(&map.root_name());
    // Предикат завершения реализации печатает цель: носитель строк отдаёт реализацию
    // как есть, а форма у каждой цели своя.
    let done = match &row.done {
        None => None,
        Some((state, extend)) => match done_predicate(extend, state, map, is_main)? {
            Done::Predicate(text) => Some(text),
            // Строки такого состояния отфильтрованы в `expressible_rows`.
            Done::NoTransition => return Ok(None),
        },
    };
    let cond = match &row.cond {
        None => None,
        Some((cond, loc)) => Some(condition_text(cond, *loc, row, map, model)?),
    };
    let text = match (done, cond) {
        (None, None) => return Ok(None),
        (Some(done), None) => done,
        (None, Some(cond)) => cond,
        (Some(done), Some(cond)) => format!("{done} && ({cond})"),
    };
    let name = format!("{struct_name}_guard_{index}");
    printer
        .print(&format!("static bool {name}({params}) {{"))
        .nl();
    printer.up();
    emit_unused(printer, &text, params);
    printer.ident(&format!("return {text};")).nl();
    printer.down();
    printer.print("}").nl().nl();
    Ok(Some(name))
}

/// Печатает условие ребра, оборачивая отказ печатника в `CC-018`.
///
/// Позиция - **ребро**, а не место в генераторе: иначе автору негде искать причину.
/// Причина прикладывается заметкой, как в форме `switch`.
fn condition_text(
    cond: &crate::semantic::ConditionNode,
    loc: crate::diagnostics::Location,
    row: &Row,
    map: &CMap,
    model: &Element,
) -> Result<String, Diagnostic> {
    generate_condition_expr(cond, map, model).map_err(|di| {
        let target = row
            .target_name()
            .map(|n| n.local().to_string())
            .unwrap_or_else(|| "END".to_string());
        Diagnostic::error_with_note(
            loc,
            format!(
                "условный переход в состояние '{target}' не переводится в C: {}",
                di.message
            ),
            di.loc,
            match &di.code {
                Some(code) => format!("причина [{}]: {}", code, di.message),
                None => format!("причина: {}", di.message),
            },
        )
        .with_code("CC-018")
    })
}

/// Печатает функцию-действие строки (`exit` источника + `enter` приёмника); `None` -
/// блоков нет, и печатать нечего.
fn emit_action(
    printer: &mut Printer,
    row: &Row,
    map: &CMap,
    model: &Element,
    (struct_name, params): (&str, &str),
    index: usize,
) -> Result<Option<String>, Diagnostic> {
    // Тело печатается в буфер: по нему решается, нужна ли функция вообще и нужна ли
    // заглушка неиспользуемого параметра (приём ).
    let mut body = String::new();
    {
        let mut buffered = printer.fork(&mut body);
        buffered.up();
        generate_named_blocks(&mut buffered, &row.exit_state.borrow(), map, model, "exit")?;
        if let Some(enter) = &row.enter_state {
            generate_named_blocks(&mut buffered, &enter.borrow(), map, model, "enter")?;
        }
        buffered.down();
    }
    if body.trim().is_empty() {
        return Ok(None);
    }
    let name = format!("{struct_name}_action_{index}");
    printer
        .print(&format!("static void {name}({params}) {{"))
        .nl();
    printer.up();
    emit_unused(printer, &body, params);
    printer.down();
    printer.print(&body);
    printer.print("}").nl().nl();
    Ok(Some(name))
}

/// Печатает заглушки неиспользуемых параметров.
///
/// Признак - упоминание имени в **напечатанном** тексте тела: тот же приём, что у
/// заглушки параметра функции. Ошибка в безопасную сторону: лишняя заглушка законна,
/// пропущенная даёт предупреждение `cc`.
fn emit_unused(printer: &mut Printer, body: &str, params: &str) {
    if super::c_params::is_unused(body, "model") {
        printer.ident(&super::c_params::unused_guard("model")).nl();
    }
    if params.contains("*main") && super::c_params::is_unused(body, "main") {
        printer.ident(&super::c_params::unused_guard("main")).nl();
    }
}
