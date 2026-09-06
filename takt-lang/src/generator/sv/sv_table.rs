//! Табличная форма автомата у цели `sv` (фича 0441).
//!
//! # Идиома принадлежит целевому языку, и выбрана она ДВУМЯ инструментами
//!
//! Строки собирает общий носитель
//! [`generator::table`](crate::generator::table) — он же обслуживает `c`,
//! `rust` и `st`. Форма же здесь своя, и каждая её черта — вывод из прогона, а
//! не вкус:
//!
//! - строки — **плоский вектор** `localparam logic [W*N-1:0]` с частичным
//!   выбором `[i*W +: W]`. Распакованный массив (`localparam state_e X [0:N-1]`)
//!   и упакованный двумерный (`logic [N-1:0][W-1:0]`) **verilator принимает, а
//!   yosys отвергает синтаксически** — тот самый случай, ради которого гейт
//!   держит два инструмента (урок 0045). Тот же плоский вектор цель уже
//!   печатает у массива в параметре функции (фича 0369);
//! - просмотр строк — **настоящий `for`** со статической границей: он
//!   разворачивается при элаборации, то есть даёт ровно ту же приоритетную
//!   цепочку, что печатает форма `unique case`;
//! - страж и действие — `case (i)` внутри цикла: указателей на функции в RTL
//!   нет, а номер строки известен на каждой итерации после разворота.
//!
//! ⚠️ **Служебные признаки объявляются сигналами модуля**, а не `automatic`
//! внутри блока: диспетчер под-модели печатается **в глубине** ветви `case`
//! родителя (композиция у цели уплощена), а объявления в SV стоят в начале
//! блока — `automatic` там оказался бы после операторов.
//!
//! ⚠️ **Момент проверки совпадает с формой `unique case`.** Готовность
//! композиции читается по `_next` (инвариант 0045), готовность цепочки — по
//! регистру шага, который в комбинационном блоке не меняется. Поэтому диспетчер
//! в конце блока видит ровно те значения, при которых переход печатает форма по
//! умолчанию.

use crate::diagnostics::Diagnostic;
use crate::generator::indent::Printer;
use crate::generator::table::{self, Row, RowTarget};
use crate::semantic::minimap::{Element, Name};

use super::sv_blocks::emit_named_blocks;
use super::sv_expr::print_condition;
use super::sv_expr::sv002;
use super::sv_fsm::{Fsm, end_variant, state_enum_name, state_variants, state_width};
use super::sv_map::SvMap;
use super::sv_time;

/// Признак «строка сработала» у модели.
pub(crate) fn fired_signal(model: &Name) -> String {
    format!("takt_fired_{}", model.unique_lowercase_snakecase())
}

/// Признак «страж строки истинен» у модели.
pub(crate) fn ok_signal(model: &Name) -> String {
    format!("takt_ok_{}", model.unique_lowercase_snakecase())
}

/// Имя вектора состояний-источников.
fn from_vector(model: &Name) -> String {
    format!("{}_TRANS_FROM", model.unique_uppercase_snakecase())
}

/// Имя вектора состояний-приёмников.
fn to_vector(model: &Name) -> String {
    format!("{}_TRANS_TO", model.unique_uppercase_snakecase())
}

/// Строки таблицы модели.
pub(crate) fn rows_of(map: &SvMap, model: &Name) -> Result<Vec<Row>, Diagnostic> {
    let Some(element) = map.model_element_of(model) else {
        return Ok(Vec::new());
    };
    table::rows(&element, map)
}

/// Печатает `localparam`-векторы строк для всех моделей программы.
///
/// Зовётся ПОСЛЕ перечислений состояний: вектор ссылается на их варианты
/// (порядок разделов файла — урок 0347).
pub(crate) fn emit_tables(p: &mut Printer, map: &SvMap, models: &[Name]) -> Result<(), Diagnostic> {
    for model in models {
        let rows = rows_of(map, model)?;
        if rows.is_empty() {
            continue;
        }
        let width = width_of(map, model)?;
        let bits = width * rows.len();
        let from: Vec<String> = rows
            .iter()
            .rev()
            .map(|row| format!("{width}'({})", row.from.unique_uppercase_snakecase()))
            .collect();
        let to: Vec<String> = rows
            .iter()
            .rev()
            .map(|row| format!("{width}'({})", target_variant(model, row)))
            .collect();
        p.ident(&format!(
            "localparam logic [{}:0] {} = {{{}}};",
            bits - 1,
            from_vector(model),
            from.join(", ")
        ))
        .nl();
        p.ident(&format!(
            "localparam logic [{}:0] {} = {{{}}};",
            bits - 1,
            to_vector(model),
            to.join(", ")
        ))
        .nl()
        .nl();
    }
    Ok(())
}

/// Печатает объявления служебных признаков диспетчеров.
pub(crate) fn emit_signals(
    p: &mut Printer,
    map: &SvMap,
    models: &[Name],
) -> Result<(), Diagnostic> {
    for model in models {
        if rows_of(map, model)?.is_empty() {
            continue;
        }
        p.ident(&format!("logic {};", fired_signal(model))).nl();
        p.ident(&format!("logic {};", ok_signal(model))).nl();
    }
    Ok(())
}

/// Печатает умолчания признаков диспетчеров — в начале `always_comb`.
///
/// ⚠️ Без них yosys объявляет признак **защёлкой**: диспетчер под-модели стоит
/// внутри ветви `case` родителя, и на прочих ветвях присваивания нет. Verilator
/// при этом модуль принимает — снова тот случай, ради которого гейт держит два
/// инструмента.
pub(crate) fn emit_defaults(
    p: &mut Printer,
    map: &SvMap,
    models: &[Name],
) -> Result<(), Diagnostic> {
    for model in models {
        if rows_of(map, model)?.is_empty() {
            continue;
        }
        p.ident(&format!("{} = 1'b0;", fired_signal(model))).nl();
        p.ident(&format!("{} = 1'b1;", ok_signal(model))).nl();
    }
    Ok(())
}

/// Печатает диспетчер модели — после её `unique case`.
///
/// `ready` — предикаты готовности реализаций состояний, собранные при печати
/// тел: их значение цель вычисляет по ходу инлайна композиции, и второго знания
/// о раскладке её регистров здесь не заводится.
pub(crate) fn emit_dispatcher(
    p: &mut Printer,
    map: &SvMap,
    fsm: &Fsm,
    model: &Name,
    ready: &[(String, String)],
) -> Result<(), Diagnostic> {
    let rows = rows_of(map, model)?;
    if rows.is_empty() {
        return Ok(());
    }
    let width = width_of(map, model)?;
    let count = rows.len();
    let reg = fsm
        .state_reg
        .get(model.unique())
        .ok_or_else(|| sv002(&format!("регистр состояния модели '{}'", model)))?;
    let enum_name = state_enum_name(model);
    let fired = fired_signal(model);
    let ok = ok_signal(model);

    let guards = guard_texts(map, fsm, model, &rows, ready)?;
    let actions: Vec<usize> = rows
        .iter()
        .enumerate()
        .filter(|(_, row)| has_action(row))
        .map(|(index, _)| index)
        .collect();

    p.ident(&format!("{fired} = 1'b0;")).nl();
    p.ident(&format!("{ok} = 1'b1;")).nl();
    p.ident(&format!(
        "for (int unsigned takt_row = 0; takt_row < {count}; takt_row++) begin"
    ))
    .nl();
    p.up();
    p.ident(&format!(
        "if (!{fired} && {enum_name}'({}[takt_row * {width} +: {width}]) == {reg}) begin",
        from_vector(model)
    ))
    .nl();
    p.up();
    if guards.is_empty() {
        p.ident(&format!("{ok} = 1'b1;")).nl();
    } else {
        p.ident(&format!("{ok} = 1'b1;")).nl();
        p.ident("case (takt_row)").nl();
        p.up();
        for (index, text) in &guards {
            p.ident(&format!("{index}: {ok} = {text};")).nl();
        }
        p.ident("default: ;").nl();
        p.down();
        p.ident("endcase").nl();
    }
    p.ident(&format!("if ({ok}) begin")).nl();
    p.up();
    if !actions.is_empty() {
        p.ident("case (takt_row)").nl();
        p.up();
        for index in &actions {
            p.ident(&format!("{index}: begin")).nl();
            p.up();
            emit_action(p, fsm, &rows[*index])?;
            p.down();
            p.ident("end").nl();
        }
        p.ident("default: ;").nl();
        p.down();
        p.ident("endcase").nl();
    }
    p.ident(&format!(
        "{reg}_next = {enum_name}'({}[takt_row * {width} +: {width}]);",
        to_vector(model)
    ))
    .nl();
    p.ident(&format!("{fired} = 1'b1;")).nl();
    p.down();
    p.ident("end").nl();
    p.down();
    p.ident("end").nl();
    p.down();
    p.ident("end").nl();
    Ok(())
}

/// Печатает действие строки: `exit` источника, затем `enter` приёмника.
fn emit_action(p: &mut Printer, fsm: &Fsm, row: &Row) -> Result<(), Diagnostic> {
    emit_named_blocks(p, &row.exit_state.borrow(), fsm, "exit")?;
    if let Some(enter) = &row.enter_state {
        emit_named_blocks(p, &enter.borrow(), fsm, "enter")?;
    }
    Ok(())
}

/// Тексты стражей: номер строки → выражение.
fn guard_texts(
    map: &SvMap,
    fsm: &Fsm,
    model: &Name,
    rows: &[Row],
    ready: &[(String, String)],
) -> Result<Vec<(usize, String)>, Diagnostic> {
    let mut out = Vec::new();
    for (index, row) in rows.iter().enumerate() {
        let done = match &row.done {
            None => None,
            Some((state, _)) => ready
                .iter()
                .find(|(name, _)| name == state.unique())
                .map(|(_, text)| text.clone()),
        };
        let cond = match &row.cond {
            None => None,
            Some((cond, _)) => {
                let scope = fsm.scope();
                Some(
                    match sv_time::after_guard(&fsm.time_levels, map, model, cond, &scope) {
                        Some(guard) => guard?,
                        None => print_condition(cond, &scope)?,
                    },
                )
            }
        };
        let text = match (done, cond) {
            (None, None) => continue,
            (Some(done), None) => done,
            (None, Some(cond)) => cond,
            (Some(done), Some(cond)) => format!("({done}) && ({cond})"),
        };
        out.push((index, text));
    }
    Ok(out)
}

/// Есть ли у строки действие (блоки `exit` источника либо `enter` приёмника).
fn has_action(row: &Row) -> bool {
    let exit = !row.exit_state.borrow().get_named_blocks("exit").is_empty();
    let enter = row
        .enter_state
        .as_ref()
        .is_some_and(|s| !s.borrow().get_named_blocks("enter").is_empty());
    exit || enter
}

/// Перечислитель состояния-приёмника строки.
fn target_variant(model: &Name, row: &Row) -> String {
    match &row.to {
        RowTarget::State(name) => name.unique_uppercase_snakecase(),
        RowTarget::End => end_variant(model),
    }
}

/// Ширина перечисления состояний модели — тем же счётом, что у его печати.
fn width_of(map: &SvMap, model: &Name) -> Result<usize, Diagnostic> {
    let Some(Element::Model { states, .. }) = map.model_element_of(model) else {
        return Ok(1);
    };
    state_width(model, &state_variants(model, &states))
}
