//! Эмиссия именованных блоков (`enter`/`always`/`exit`) и охранных формул в цель
//! SystemVerilog.
//!
//! Вынесено из `sv_fsm.rs`. Два источника блоков - состояние и **сама модель**
//! (model-level `always`); обе печати идут в `always_comb` над `_next`-сигналами.
//!
//! Охранные формулы печатаются **там же, где их печатает цель `c`**: формулы модели -
//! перед `unique case`, формулы состояния - в начале его ветви.

use crate::diagnostics::Diagnostic;
use crate::generator::indent::Printer;
use crate::generator::sv::sv_expr::print_condition;
use crate::generator::sv::sv_fsm::Fsm;
use crate::generator::sv::sv_stmt::print_statement;
use crate::semantic::formula::Formula;
use crate::semantic::{ModelNode, StateNode};

/// Печатает именованные блоки состояния (`enter`/`always`/`exit`).
pub(crate) fn emit_named_blocks(
    p: &mut Printer,
    state: &StateNode,
    fsm: &Fsm,
    block: &str,
) -> Result<(), Diagnostic> {
    for b in state.get_named_blocks(block) {
        if let Some(stmt) = b.statement() {
            emit_block_body(p, stmt, fsm)?;
        }
    }
    Ok(())
}

/// Печатает тело блока: объявления локальных, затем операторы.
///
/// Объявления локальных переменных идут **до** операторов: прежде они не печатались
/// вовсе, и вывод был невалиден при нулевом коде возврата.
///
/// **Локальная переменная, содержащая структуру, поднимается в начало
/// `always_comb`**. Тело пишет её поля (`tmp.lo = ...`), а внутри
/// ветви `case` yosys такую запись полным присваиванием не считает: "Latch
/// inferred for signal `$unnamed_block$1.tmp.lo`" - при том что verilator
/// модуль принимает, а `taktc` возвращает ноль. Формы измерены прогоном
/// **обоих** инструментов: именованный блок, `= '0`, `'{N{'0}}` и
/// объявление на уровне модуля без умолчаний yosys отвергает; принимает он
/// объявление в начале процесса с нулевыми умолчаниями по листьям.
///
/// Прочие локальные (скаляр, перечисление, массив скаляров) печатаются
/// **на месте**, как прежде: подъём изменил бы вывод корпуса без нужды.
fn emit_block_body(
    p: &mut Printer,
    stmt: &crate::semantic::StatementNode,
    fsm: &Fsm,
) -> Result<(), Diagnostic> {
    let mut locals = Vec::new();
    crate::generator::sv::sv_stmt::hoist_locals(stmt, &mut locals);
    let scope = fsm.scope();
    let fields_of = |name: &str| scope.structs.get(name).cloned();
    let mut inline = Vec::new();
    for (name, ty) in locals {
        if crate::generator::sv::sv_array::contains_struct(ty, &fields_of) {
            crate::generator::sv::sv_locals::hoist(&fsm.hoisted_locals, scope.structs, name, ty)?;
        } else {
            inline.push((name, ty));
        }
    }
    // Локальные, значение которых нигде не читается, получают поглотитель: иначе
    // `verilator -Wall` отвечает `UNUSEDSIGNAL`, а проверка цели считает предупреждение
    // ошибкой.
    let mut unread = crate::semantic::unused::unread_locals(stmt);
    // Переменная цикла читается частично - тот же поглотитель.
    crate::generator::sv::sv_stmt::loop_variables(stmt, &mut unread);
    // Локальная, прочитанная только как индекс, читается частично тоже: индекс
    // печатается сужением, старшие разряды не читает никто.
    let local_names: Vec<String> = inline.iter().map(|(n, _)| (*n).to_string()).collect();
    crate::generator::sv::sv_stmt::index_only_variables(stmt, &local_names, &mut unread);
    crate::generator::sv::sv_stmt::emit_hoisted_locals_auto(p, &inline, &unread)?;
    print_statement(p, stmt, &scope)?;
    // Присваивание поглотителя идёт после тела: `always_comb`, читающий сигнал раньше
    // записи, verilator встречает `ALWCOMBORDER` ("behavior may imply latch"), а проверка
    // цели считает предупреждение ошибкой.
    crate::generator::sv::sv_stmt::emit_local_sinks(p, &inline, &unread);
    Ok(())
}

/// Печатает именованные блоки **уровня модели**: `always` вне состояния. Аналог
/// [`emit_named_blocks`], но источник - сама модель.
pub(crate) fn emit_model_named_blocks(
    p: &mut Printer,
    model: &ModelNode,
    fsm: &Fsm,
    block: &str,
) -> Result<(), Diagnostic> {
    for b in model.get_named_blocks(block) {
        if let Some(stmt) = b.statement() {
            emit_block_body(p, stmt, fsm)?;
        }
    }
    Ok(())
}

/// Преамбула ветви состояния: охранные формулы, затем блоки `always`.
///
/// Одна функция вместо двух вызовов подряд - потому что порядок здесь **часть
/// контракта**: проверка стоит до действий такта, как `assert` в начале `_tick` цели
/// `c`. Разведи их по вызывающему коду - и следующая правка переставит их местами, не
/// заметив, что меняет семантику.
pub(crate) fn emit_state_prelude(
    p: &mut Printer,
    map: &crate::generator::sv::sv_map::SvMap,
    state: &StateNode,
    fsm: &Fsm,
) -> Result<(), Diagnostic> {
    if map.guard_enable() {
        emit_state_guards(p, state, fsm)?;
    }
    emit_named_blocks(p, state, fsm, "always")
}

/// Преамбула уровня модели: охранные формулы уровня модели, затем model-level `always`.
/// Порядок - тот же и по той же причине.
pub(crate) fn emit_model_prelude(
    p: &mut Printer,
    map: &crate::generator::sv::sv_map::SvMap,
    model: &ModelNode,
    fsm: &Fsm,
) -> Result<(), Diagnostic> {
    emit_model_named_blocks(p, model, fsm, "always")?;
    if map.guard_enable() {
        emit_model_guards(p, model, fsm)?;
    }
    Ok(())
}

/// Печатает охранные формулы **состояния**.
fn emit_state_guards(p: &mut Printer, state: &StateNode, fsm: &Fsm) -> Result<(), Diagnostic> {
    for formula in state.formulas() {
        emit_guard(p, formula, &fsm.scope())?;
    }
    Ok(())
}

/// Печатает охранные формулы **модели** - перед `unique case`, как в цели `c`.
fn emit_model_guards(p: &mut Printer, model: &ModelNode, fsm: &Fsm) -> Result<(), Diagnostic> {
    for formula in &model.formulas {
        emit_guard(p, formula, &fsm.scope())?;
    }
    Ok(())
}

/// Печатает одну формулу как immediate assertion.
///
/// **Форма выбрана пробой, а не вкусом** (2026-08-15): `assert (условие);` принимают
/// Оба инструмента проверки, а `assert (условие) else $error("...");` verilator принимает,
/// но **yosys отвергает** (`syntax error, unexpected TOK_ELSE`). Поэтому имя инварианта
/// в цель `sv` **не переносится**: цель `rust` его печатает, `c` игнорирует, `sv` не
/// может по устройству синтезатора. Добавите `else` - проверка покраснеет на yosys, пройдя
/// verilator.
///
/// Место - `always_comb`, рядом с телом уровня. Условие **читает регистры**
/// (`Scope::registered`: чтение из `name`, запись в `name_next`), то есть значения,
/// устойчивые в пределах такта, - комбинационных ложных срабатываний не будет. Синтез
/// не задет: yosys кладёт проверку в ячейку `$check`, а логика остаётся прежней (проба:
/// 8 `$_DFF_PN0_` с проверкой и без).
pub(crate) fn emit_guard(
    p: &mut Printer,
    formula: &Formula,
    scope: &crate::generator::sv::sv_scope::Scope,
) -> Result<(), Diagnostic> {
    match formula {
        // Имя инварианта не печатается - см. оговорку о yosys выше.
        Formula::Guard(cond, _, _) => {
            let text = print_condition(cond, scope)?;
            if !text.is_empty() {
                p.ident(&format!("assert ({});", text)).nl();
            }
            Ok(())
        }
        Formula::Formulas(items) => {
            for item in items {
                emit_guard(p, item, scope)?;
            }
            Ok(())
        }
        // LTL цель `sv` не верифицирует (как и прочие цели): о ней говорит SE-055 из
        // семантики. Пустая формула объявлением не является.
        Formula::LTL(_, _) | Formula::None => Ok(()),
    }
}
