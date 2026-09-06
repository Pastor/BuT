//! Генерация автоматов состояний: init, tick, is_done, reset функции модели.
//!
//! Содержит логику генерации C-функций для всех моделей: [`generate_model_functions`],
//! [`generate_function_prototypes`] и вспомогательные функции для работы с
//! параллельными и последовательными состояниями.

use super::c_expr::{generate_condition_expr, generate_formula_check};
use crate::diagnostics::{Diagnostic, Location};
use crate::generator::c;
use crate::generator::c::c_map::CMap;
use crate::generator::indent::Printer;
use crate::semantic::StateNode;
use crate::semantic::minimap::{Element, Name, StateExtend};

/// Генерирует именованные блоки состояния (enter/exit/always).
use super::c_blocks::{generate_model_named_blocks, generate_named_blocks};
use super::c_model_init::{generate_concat_item_init, generate_model_init};

/// Генерирует прототипы функций для всех используемых моделей.
pub(super) fn generate_function_prototypes(
    printer: &mut Printer,
    map: &CMap,
) -> Result<(), Diagnostic> {
    let root_name = map.root_name();
    let sorted_models = c::topological_sort_models(map, map.using_models());

    if !sorted_models.is_empty() {
        for element in &sorted_models {
            let Element::Model { name, .. } = element else {
                continue;
            };
            let s = name.unique_camelcase();
            // Прототип обязан совпасть с определением: признак тот же (`c_needs`), и
            // второй его копии здесь быть не должно. Прототип обязан совпасть с
            // определением, а нужда считается на функцию: сигнатуры четырёх функций
            // одной модели законно расходятся.
            let param_for = |which| {
                if map.raw_model_at(name.clone()).is_ok_and(|rc| {
                    crate::generator::c::c_needs::model_fn_needs_root(
                        &rc,
                        which,
                        crate::generator::c::c_time::is_clock_profile(map),
                    )
                }) {
                    format!(", {} *main", root_name.unique_camelcase())
                } else {
                    String::new()
                }
            };
            let init_param = param_for(crate::generator::c::c_needs::ModelFn::Init);
            let tick_param = param_for(crate::generator::c::c_needs::ModelFn::Tick);
            printer
                .print(&format!(
                    "static void {0}_init({0} *model{1});",
                    s, init_param
                ))
                .nl();
            printer
                .print(&format!(
                    "static void {0}_tick({0} *model{1});",
                    s, tick_param
                ))
                .nl();
            printer
                .print(&format!(
                    // `_is_done` указателя не получает никогда.
                    "static bool {0}_is_done(const {0} *model);",
                    s
                ))
                .nl();
        }
        printer.nl();
    }
    Ok(())
}

/// Генерирует переходы между состояниями для простого состояния [`Element::State`].
///
/// Для каждой ссылки (`ref`-перехода) формирует:
/// - безусловный переход (`ConditionNode::None`): `exit -> enter -> state`, после
///   чего обход рёбер **прекращается**: ветвь `case` закрывает общий
///   `break;`, а всё, что стоит за безусловным ребром, недостижимо
/// - условный переход: `if (cond) { exit -> enter -> state -> break }` - здесь
///   `break` обязателен, он выходит из `switch` изнутри блока
///
/// Возвращает `true`, если напечатано **безусловное** ребро: всё, что за ним,
/// недостижимо, и вызывающий обязан прекратить печать переходов.
fn generate_state_transitions(
    printer: &mut Printer,
    raw_state: &StateNode,
    map: &CMap,
    model: &Element,
    _model_name: &Name,
    states: &[Name],
) -> Result<bool, Diagnostic> {
    for reference in raw_state.references() {
        let Some(target) = states.iter().find(|n| n.local() == reference.name).cloned() else {
            continue; // целевое состояние не найдено в достижимых состояниях
        };
        // Условие есть у всего, кроме `None`.
        //
        // `Unresolved` отсюда изъят: прежде неразрешённое условие считалось отсутствием
        // условия, и ребро печаталось **безусловным** переходом. Это опаснее пустой
        // строки в печатнике: вывод валиден, `cc` молчит, а автомат другой - переход
        // срабатывает всегда. Теперь такое ребро идёт в печатник, получает `CC-023` и
        // доезжает до автора обёрткой `CC-018` (позиция ребра + причина заметкой,
        // устройство ). Переход объявляет своё место: условие ребра - не оператор, и
        // отказ в нём печатался без координаты вовсе.
        crate::generator::site::enter(reference.location);
        let has_cond = !reference.cond.is_unconditional();
        if has_cond {
            match generate_condition_expr(&reference.cond, map, model) {
                Ok(cond_str) => {
                    printer.ident(&format!("if ({}) {{", cond_str)).up().nl();
                    generate_named_blocks(printer, raw_state, map, model, "exit")?;
                    let target_rc = map.raw_state_at(target.clone())?;
                    let target_raw = &*target_rc.borrow();
                    generate_named_blocks(printer, target_raw, map, model, "enter")?;
                    printer
                        .ident(&format!(
                            "model->state = {};",
                            target.unique_uppercase_snakecase()
                        ))
                        .nl();
                    printer.ident("break;").nl();
                    printer.down().ident("}").nl();
                }
                // было - печать комментария-заглушки "условный переход не
                // поддерживается" в порождаемый C и **проглатывание** `Err`.
                // Последствия (проба воспроизводила): переход не генерировался, `taktc`
                // печатал "Скомпилировано" и завершался с кодом 0, а порождённый C
                // собирался без замечаний - комментарий сборку не ломает. На выходе -
                // мёртвый автомат, обнаруживаемый на объекте, а не в CI.
                //
                // Ошибка, а не предупреждение: при коде возврата 0 главный риск
                // сохраняется. Ужесточение не ломает ни одной работающей сборки - оно
                // ломает ровно те, что были сломаны и без него, просто молча (корпус
                // `examples/` эту ветку не задевает - проверено прогоном).
                Err(di) => {
                    return Err(Diagnostic::error_with_note(
                        // R3: позиция `ref` в исходнике, а не Location::Codegen - иначе
                        // пользователю негде искать причину.
                        reference.location,
                        format!(
                            "условный переход в состояние '{}' не переводится в C: {}",
                            target.local(),
                            di.message
                        ),
                        di.loc,
                        // R2: исходная причина приложена заметкой, а не схлопнута в
                        // строку - код исходной диагностики иначе теряется.
                        match &di.code {
                            Some(code) => format!("причина [{}]: {}", code, di.message),
                            None => format!("причина: {}", di.message),
                        },
                    )
                    .with_code("CC-018"));
                }
            }
        } else {
            // Безусловный переход: exit -> enter -> state.
            //
            // . Собственного `break;` у него нет, и цикл по рёбрам на
            // нём заканчивается - ровно как в трёх остальных целях
            // (`rust_tick.rs`, `st_model.rs`, `sv_fsm.rs`: "всё, что за
            // безусловным ребром, недостижимо"). Прежде цель `c` печатала и
            // `break;`, и хвост рёбер: ветвь `case` закрывается общим `break;`
            // (см. хвост печати `switch` ниже), поэтому в выводе выходила пара
            // `break; break;` - 7 мест в корпусе, - а рёбра после безусловного
            // печатались недостижимым кодом. `cc -Wall -Wextra
            // -Wunreachable-code` об этом молчит, то есть цена - шум в коде,
            // который читает человек.
            //
            // Поведение тождественно: удаляется исключительно то, до чего исполнение не
            // доходит.
            generate_named_blocks(printer, raw_state, map, model, "exit")?;
            let target_rc = map.raw_state_at(target.clone())?;
            let target_raw = &*target_rc.borrow();
            generate_named_blocks(printer, target_raw, map, model, "enter")?;
            printer
                .ident(&format!(
                    "model->state = {};",
                    target.unique_uppercase_snakecase()
                ))
                .nl();
            return Ok(true);
        }
    }
    Ok(false)
}

/// Генерирует переход из расширенного состояния (Parallel / Concatenation).
///
/// При пустом `next` выполняет `model->state = {MODEL}_END; break;`. Иначе -
/// устанавливает целевое состояние и генерирует блоки `exit` / `enter`. Закрывающую `}`
/// добавляет вызывающий код.
fn generate_extend_transition(
    printer: &mut Printer,
    raw_state: &StateNode,
    map: &CMap,
    model: &Element,
    model_name: &Name,
    next: &Name,
    states: &[Name],
) -> Result<(), Diagnostic> {
    // Собственные рёбра состояния-композиции - Перед `next`/`END`.
    //
    // Правило языка задано: реализация тикается до проверки переходов, и переход
    // берётся по её завершении. Эталон проверяет сначала `ref`-рёбра в порядке
    // объявления, затем `next` (последним - иначе безусловный `next` затенил бы их все,
    // см. `build_transitions` симулятора). Цели печатали только `next`, и вход `start
    // Entry = A | B { ref Finish: cond; }` давал ДРУГОЙ автомат: в `Finish` никто не
    // шёл, а при ложном условии цель уходила в `END` там, где эталон ждёт. Вывод при
    // этом валиден, и ни один инструмент целевого языка об этом не говорит - вердикт
    // дают только потактовые сверки.
    if generate_state_transitions(printer, raw_state, map, model, model_name, states)? {
        // Сработало безусловное ребро: всё, что за ним, недостижимо.
        return Ok(());
    }
    // `END` подставляется только состоянию без переходов. У эталона завершение узла
    // (`Terminated`) наступает при **пустом** списке переходов; если переходы есть, но
    // ни один не сработал, узел **остаётся** в состоянии.
    if next.local().is_empty() && !raw_state.references().is_empty() {
        printer.ident("break;").nl();
        return Ok(());
    }
    // Состояние С телом автомат не завершает: рёбер у него может не быть вовсе, и
    // уводить его в `END` нельзя - оно работает, пока его не увели. Признак общий с
    // эталоном (`StateNode::is_terminated`), иначе прошивка замирала бы там, где модель
    // считает дальше.
    if next.local().is_empty() && !raw_state.is_terminated() {
        printer.ident("break;").nl();
        return Ok(());
    }
    if next.local().is_empty() {
        // Переход в терминальное состояние: exit текущего -> state = END -> break
        generate_named_blocks(printer, raw_state, map, model, "exit")?;
        printer
            .ident(&format!(
                "model->state = {}_END;",
                model_name.unique_uppercase_snakecase()
            ))
            .nl();
        printer.ident("break;").nl();
    } else {
        // Переход в следующее состояние: exit текущего -> enter следующего -> state ->
        // break
        generate_named_blocks(printer, raw_state, map, model, "exit")?;
        let next_raw = map.raw_state_at(next.clone())?;
        let next_raw = &*next_raw.borrow();
        generate_named_blocks(printer, next_raw, map, model, "enter")?;
        printer
            .ident(&format!(
                "model->state = {};",
                next.unique_uppercase_snakecase()
            ))
            .nl();
        printer.ident("break;").nl();
    }
    Ok(())
}

/// Генерирует tick-логику для состояния с конкатенационной компоновкой.
///
/// Формирует цепочку `if / else if` по полю `{state_local}_state`: каждый вариант
/// тикает активный элемент и при его завершении инициализирует следующий или выполняет
/// переход (через [`generate_extend_transition`]).
#[allow(clippy::too_many_arguments)]
fn generate_concat_tick(
    printer: &mut Printer,
    state_local: &str,
    state_unique_upper: &str,
    items: &[StateExtend],
    call_append: &str,
    append: &str,
    raw_state: &StateNode,
    map: &CMap,
    model: &Element,
    model_name: &Name,
    next: &Name,
    states: &[Name],
) -> Result<(), Diagnostic> {
    // Имена машины шагов - у общего носителя: их знают печать такта, инициализация шага
    // и страж строки табличной формы, и копий у одного правила быть не должно.
    let state_field = c::c_chain::step_field(state_local);
    for (idx, item) in items.iter().enumerate() {
        // Имя варианта enum для текущего элемента; `None` - шаг такой формы печать
        // такта не ведёт.
        let Some(current_variant) = c::c_chain::step_variant(state_unique_upper, item, idx) else {
            continue;
        };

        // Открываем if / else if
        if idx == 0 {
            printer
                .ident(&format!("if ({} == {}) {{", state_field, current_variant))
                .up()
                .nl();
        } else {
            printer
                .down()
                .ident(&format!(
                    "}} else if ({} == {}) {{",
                    state_field, current_variant
                ))
                .up()
                .nl();
        }

        let is_last = idx + 1 >= items.len();

        match item {
            StateExtend::Model(name, _) => {
                let field = c::c_chain::model_access(state_local, name, idx);
                // Тик текущего элемента
                let arg = map.root_arg(
                    name,
                    call_append == ", model",
                    crate::generator::c::c_needs::ModelFn::Tick,
                );
                printer
                    .ident(&format!(
                        "{}_tick(&{}{});",
                        name.unique_camelcase(),
                        field,
                        arg
                    ))
                    .nl();
                // Внешний переход последнего шага в табличной форме печатает таблица:
                // здесь остаётся только машина шагов - она ведёт переходы внутри
                // состояния.
                if is_last && map.fsm_table() {
                    continue;
                }
                // Проверяем завершение
                printer
                    .ident(&format!(
                        "if ({}_is_done(&{})) {{",
                        name.unique_camelcase(),
                        field,
                    ))
                    .up()
                    .nl();
                if is_last {
                    generate_extend_transition(
                        printer, raw_state, map, model, model_name, next, states,
                    )?;
                } else {
                    let next_variant = generate_concat_item_init(
                        printer,
                        map,
                        model,
                        (state_local, state_unique_upper),
                        &items[idx + 1],
                        idx + 1,
                        append,
                    )?;
                    printer
                        .ident(&format!("{} = {};", state_field, next_variant))
                        .nl();
                    printer.ident("break;").nl();
                }
                printer.down().ident("}").nl();
            }
            StateExtend::Parallel(inner) => {
                let parallel_access = c::c_chain::parallel_access(state_local, idx);
                let nested_upper = c::c_chain::parallel_upper(state_unique_upper, idx);
                let done_exprs = crate::generator::c::c_compose::generate_parallel_items_tick(
                    printer,
                    map,
                    &parallel_access,
                    &nested_upper,
                    inner,
                    call_append == ", model",
                );
                if is_last && map.fsm_table() {
                    // Внешний переход печатает таблица.
                    continue;
                }
                if !done_exprs.is_empty() {
                    printer
                        .ident(&format!("if ({}) {{", done_exprs.join(" && ")))
                        .up()
                        .nl();
                    if is_last {
                        generate_extend_transition(
                            printer, raw_state, map, model, model_name, next, states,
                        )?;
                    } else {
                        let next_variant = generate_concat_item_init(
                            printer,
                            map,
                            model,
                            (state_local, state_unique_upper),
                            &items[idx + 1],
                            idx + 1,
                            append,
                        )?;
                        printer
                            .ident(&format!("{} = {};", state_field, next_variant))
                            .nl();
                        printer.ident("break;").nl();
                    }
                    printer.down().ident("}").nl();
                }
            }
            _ => {}
        }
    }
    // Закрываем последний if / else if блок
    if !items.is_empty() {
        printer.down().ident("}").nl();
    }
    Ok(())
}

fn generate_model_tick(
    printer: &mut Printer,
    model: &Element,
    map: &CMap,
) -> Result<(), Diagnostic> {
    let is_main = model.name().eq(&map.root_name());
    let model_name = model.name();
    let Element::Model {
        start,
        states,
        name,
    } = model
    else {
        return Err(Diagnostic::error(
            Location::Codegen,
            "Элемент не является моделью".to_string(),
        )
        .with_code("CC-006"));
    };

    // Проверки Guard-формул модели
    if map.guard_enable() {
        let raw_model = map.raw_model_at(model_name.clone())?;
        for formula in &raw_model.borrow().formulas {
            generate_formula_check(printer, map, model, formula)?;
        }
    }

    // 0033 (Option B): вход в стартовое состояние не расходует такт. Работа INIT
    // (инициализация вложенных, блоки `enter`, установка стартового состояния)
    // диспетчеризуется в `if` до `switch` и не завершается `break`, поэтому тело
    // стартового состояния исполняется в этом же такте - как в симуляторе
    // (`enter_initial_state`). Блоки `enter` остаются внутри `_tick` (могут писать в
    // порты), а не выносятся в `_init` вне цикла сканирования. Правка рекурсивна по
    // уровням: вложенный `_tick` делает то же, поэтому сдвиг обнуляется на любой
    // глубине.
    let raw_state = map.raw_state_at(start.clone())?;
    let raw_state = &*raw_state.borrow();
    let append = if !is_main { ", main" } else { ", model" };
    let call_append = if !is_main { ", main" } else { ", model" };
    printer
        .ident(&format!(
            "if (model->state == {}_INIT) {{",
            name.unique_uppercase_snakecase()
        ))
        .up()
        .nl();
    // Инициализация вложенных вынесена в `_init` (0033, R6). Здесь - только поведение
    // входа: блоки `enter` (могут писать в порты, поэтому внутри такта) и установка
    // стартового состояния. Без `break` - тело исполняется в этом же такте.
    let start_variant_upper = match map.state_at(start.clone()) {
        Some(Element::State { name, .. }) => name.unique_uppercase_snakecase(),
        Some(Element::StateExtend { name, .. }) => name.unique_uppercase_snakecase(),
        _ => {
            return Err(Diagnostic::error(
                Location::Codegen,
                "Начальное состояние модели не определено".to_string(),
            )
            .with_code("CC-008"));
        }
    };
    generate_named_blocks(printer, raw_state, map, model, "enter")?;
    printer
        .ident(&format!("model->state = {};", start_variant_upper))
        .nl();
    printer.down().ident("}").nl();

    // именованные блоки **уровня модели** (`always` вне состояния) исполняются каждый
    // такт до диспетчеризации состояния, безусловно по состоянию - как шаг 2
    // `execution("always")` симулятора (эталон). он - тело синтетического состояния).
    let raw_model = map.raw_model_at(model_name.clone())?;
    generate_model_named_blocks(printer, &raw_model.borrow(), map, model, "always")?;

    // Тело стартового состояния исполняется в том же такте (без `break` выше).
    printer.ident("switch (model->state) {").up().nl();
    let mut end_already_defined = false;
    for state_name in states.iter() {
        let raw_state = map.raw_state_at(state_name.clone())?;
        let raw_state = &*raw_state.borrow();
        let Some(state) = map.state_at(state_name.clone()) else {
            continue; // недостижимое состояние - пропускаем генерацию case
        };
        printer
            .ident("case ")
            .print(&state_name.unique_uppercase_snakecase())
            .print(": {")
            .up()
            .nl();

        // Проверка Guard-формул состояния
        if map.guard_enable() {
            for formula in raw_state.formulas() {
                generate_formula_check(printer, map, model, formula)?;
            }
        }

        generate_named_blocks(printer, raw_state, map, model, "always")?;
        // Периодические блоки `every` - после `always`, как в симуляторе (шаг
        // `execute_every` идёт следом за `execution("always")`).
        crate::generator::c::c_every::emit_state_body(
            printer,
            map,
            model,
            &raw_model.borrow(),
            state_name.local(),
        )?;
        // было - цепочка `if let / else if let / else`, где хвостовой `else` печатал в
        // порождаемый C комментарий-заглушку "пока не реализовано". Ветка недостижима
        // (доказательство - в ветке `Element::Model` ниже), то есть это мёртвый код,
        // маскировавшийся под незавершённую работу.
        //
        // Исчерпывающий `match` без ветки `_` заменяет дисциплину на проверку
        // компилятором: новый вариант `Element` теперь валит сборку, а не выпадает в
        // молчаливый no-op. Цепочка `if let` этого не давала - для неё компилятор
        // обязан допустить `else`.
        match state {
            // Табличная форма: переходы простого состояния - строки таблицы, и в теле
            // `case` их не печатается вовсе. Тело при этом прежнее: блоки `always`,
            // `every`, проверки формул.
            Element::State { .. } if map.fsm_table() => {}
            Element::State { .. } => {
                generate_state_transitions(printer, raw_state, map, model, &model_name, states)?;
                // Терминальное состояние (нет переходов) - явно переходим в END
                // Исключение: если состояние уже является END (не добавляем
                // самопереход)
                if raw_state.is_terminated() && !state_name.local().to_uppercase().eq("END") {
                    generate_named_blocks(printer, raw_state, map, model, "exit")?;
                    printer
                        .ident(&format!(
                            "model->state = {}_END;",
                            model_name.unique_uppercase_snakecase()
                        ))
                        .nl();
                }
            }
            Element::StateExtend { extend, next, .. } => {
                // цепочка по `StateExtend` тоже стала исчерпывающим `match` - у неё не
                // было ветки `None`, и такое состояние молча не порождало ничего, даже
                // комментария - тише заглушки №1.
                match extend {
                    StateExtend::Model(name, _) => {
                        let arg = map.root_arg(
                            &name,
                            call_append == ", model",
                            crate::generator::c::c_needs::ModelFn::Tick,
                        );
                        printer
                            .ident(&format!(
                                "{}_tick(&model->{}",
                                name.unique_camelcase(),
                                state_name.local_lowercase_snakecase()
                            ))
                            .print(arg)
                            .print(");")
                            .nl();
                        // Табличная форма: тик реализации - тело такта и остаётся
                        // здесь, а её завершение становится стражем строки
                        // (`M_is_done(&model->x)`).
                        if !map.fsm_table() {
                            printer
                                .ident(&format!(
                                    "if ({}_is_done(&model->{}",
                                    name.unique_camelcase(),
                                    state_name.local_lowercase_snakecase()
                                ))
                                .print(")) {")
                                .up()
                                .nl();
                            // Переход печатает общая функция: прежде эта ветвь несла
                            // свою копию "next либо END", и собственные рёбра состояния
                            // теряла вместе с ней.
                            generate_extend_transition(
                                printer,
                                raw_state,
                                map,
                                model,
                                &model_name,
                                &next,
                                states,
                            )?;
                            printer.down().ident("}").nl();
                        }
                    }
                    StateExtend::Parallel(steps) => {
                        let local = state_name.local_lowercase_snakecase();
                        let unique_upper = state_name.unique_uppercase_snakecase();
                        let access = format!("model->{}", local);
                        let done_exprs =
                            crate::generator::c::c_compose::generate_parallel_items_tick(
                                printer,
                                map,
                                &access,
                                &unique_upper,
                                &steps,
                                call_append == ", model",
                            );
                        // Внешний переход состояния-параллели в табличной форме
                        // печатает таблица: здесь остаётся только тик ветвей.
                        if !done_exprs.is_empty() && !map.fsm_table() {
                            printer
                                .ident(&format!("if ({}) {{", done_exprs.join(" && ")))
                                .up()
                                .nl();
                            generate_extend_transition(
                                printer,
                                raw_state,
                                map,
                                model,
                                &model_name,
                                &next,
                                states,
                            )?;
                            printer.down().ident("}").nl();
                        }
                    }
                    StateExtend::Concatenation(steps) => {
                        let local = state_name.local_lowercase_snakecase();
                        let unique_upper = state_name.unique_uppercase_snakecase();
                        generate_concat_tick(
                            printer,
                            &local,
                            &unique_upper,
                            &steps,
                            call_append,
                            append,
                            raw_state,
                            map,
                            model,
                            &model_name,
                            &next,
                            states,
                        )?;
                    }
                    // ветки для этого варианта не было - состояние с неразрешённой
                    // реализацией молча не порождало ничего. Достижимость не
                    // установлена (проба через `Extend::Unresolved` отклоняется
                    // семантикой раньше кодогенерации), поэтому поведение сохранено
                    // прежним - пустая генерация, - но теперь оно явное и обосновано
                    // здесь, а не подразумевается отсутствием ветки. объём не
                    // расширяет: доказать достижимость и выбрать между `Err` и пустой
                    // генерацией - предмет отдельной работы (кандидат в бэклоге).
                    StateExtend::None => {}
                }
            }
            // Недостижимо по контракту `CMap::state_at` (`c_map.rs:125`): он отдаёт
            // элемент, только если `element.is_state()`, а тот - `matches!(self,
            // Element::State { .. } | Element::StateExtend { .. })` (`minimap.rs:137`).
            // То есть `Element::Model` сюда не попадает никогда; прежде здесь печатался
            // комментарий-заглушка.
            //
            // `unreachable!`, а не `Err`: инвариант обеспечен типом-Фильтром, а не
            // входными данными пользователя - диагностика предлагала бы ему исправить
            // то, чего он не писал.
            Element::Model { .. } => unreachable!(
                "CMap::state_at отдаёт только State/StateExtend (фильтр is_state); \
                 Element::Model сюда не попадает"
            ),
        }
        printer.ident("break;").nl();
        printer.down().ident("}").nl();
        if !end_already_defined {
            end_already_defined = state_name.local().to_uppercase().eq("END");
        }
    }
    if !end_already_defined {
        printer
            .ident("case ")
            .print(&name.unique_uppercase_snakecase())
            .print("_END: {")
            .up()
            .nl();
        printer.ident("break;").nl();
        printer.down().ident("}").nl();
    }
    // Вариант `_INIT` до `switch` не доходит (снят диспетчером выше, 0033), но остаётся
    // значением перечисления - `default` гасит -Wswitch, не пряча при этом реальный
    // пропуск состояния (все достижимые состояния имеют `case`).
    printer.ident("default: break;").nl();
    printer.down().ident("}").nl();
    // Табличная форма: переходы просматривает диспетчер - После тел состояний и до
    // обновления счётчика выдержки, ровно там, где стоял переход внутри `case`.
    if map.fsm_table() {
        c::c_table::emit_dispatch_call(printer, model, c::c_table::wants_root(model, map));
    }
    // Счётчик выдержки обновляется в конце такта - так значение, видимое условиям на
    // такте M, равно числу тактов, прошедших с входа в состояние. Смена состояния в
    // этом такте означает вход, поэтому счётчик становится 1 (на следующем такте с
    // входа пройдёт ровно один такт), иначе растёт. Обновление стоит здесь один раз, а
    // не рядом с каждым из десяти присваиваний `model->state` - второй экземпляр этой
    // логики неминуемо разъехался бы с первым.
    let raw_model = map.raw_model_at(model_name.clone())?;
    // HAL-указатель: `model` у корня, `main` у под-модели (как порты).
    let hal_ptr = if model_name.eq(&map.root_name()) {
        "model"
    } else {
        "main"
    };
    crate::generator::c::c_time::emit_state_time_update(
        printer,
        map,
        &raw_model.borrow(),
        hal_ptr,
    )?;
    Ok(())
}

/// Печатает `(void)main;`, если тело под-модели указателем на корень не пользуется.
///
/// Зовётся, только когда параметр напечатан: у корневой модели его нет вовсе, у
/// под-модели - по нужде.
fn emit_unused_guard(printer: &mut Printer, body: &str, has_root_param: bool) {
    // Вопрос задаётся, только если параметр есть: у корневой модели его не было
    // никогда, а у под-модели он теперь печатается по нужде - заглушка над
    // несуществующим именем даёт "use of undeclared identifier".
    if !has_root_param {
        return;
    }
    if crate::generator::c::c_params::is_unused(body, "main") {
        printer
            .ident(&crate::generator::c::c_params::unused_guard("main"))
            .nl();
    }
}

/// Генерирует все C-функции для модели: init, tick, reset, is_done.
pub(super) fn generate_model_functions(
    printer: &mut Printer,
    model: &Element,
    map: &CMap,
) -> Result<(), Diagnostic> {
    let is_main = model.name().eq(&map.root_name());
    let Element::Model {
        name,
        states: _,
        start: _,
    } = model
    else {
        return Err(Diagnostic::error(
            Location::Codegen,
            format!("Model {} not defined", model.name().unique_camelcase()),
        )
        .with_code("CC-006"));
    };
    // Указатель на корень печатается по нужде: прежде он стоял в сигнатуре всякой
    // под-модели, а тело пользовалось им не везде - 45 заглушек `(void)main;` в
    // корпусе. Признак - общий носитель `c_needs`. Нужда считается на функцию: у
    // `_init` и `_tick` тела разные, а `_is_done` указателем не пользуется никогда.
    let needs = |which| {
        !is_main
            && map.raw_model_at(model.name().clone()).is_ok_and(|rc| {
                crate::generator::c::c_needs::model_fn_needs_root(
                    &rc,
                    which,
                    crate::generator::c::c_time::is_clock_profile(map),
                )
            })
    };
    let root_param = |wanted: bool| {
        if wanted {
            format!(", {} *main", map.root_name().unique_camelcase())
        } else {
            String::new()
        }
    };
    let wants_init = needs(crate::generator::c::c_needs::ModelFn::Init);
    let wants_tick = needs(crate::generator::c::c_needs::ModelFn::Tick);
    let init_append = root_param(wants_init);
    let tick_append = root_param(wants_tick);
    let init_call_append = if wants_init {
        String::from(", main")
    } else {
        String::new()
    };
    let struct_name = name.unique_camelcase();
    // Табличная форма печатается перед функциями модели: стражи, действия и таблица
    // статичны, а диспетчер зовётся из `_tick` - в C определение обязано стоять выше
    // вызова.
    if map.fsm_table() {
        c::c_table::emit_transition_table(printer, model, map, wants_tick)?;
    }
    printer
        .print("void ")
        .print(&struct_name)
        .print("_init(")
        .print(&struct_name)
        .print(" *model")
        .print(&init_append)
        .print(") {")
        .nl();
    // NOTICE: init
    printer.up();
    printer.ident("assert(0 != model);").nl();
    // Тело печатается в буфер: по нему решается, нужна ли заглушка неиспользуемого
    // параметра.
    let mut init_body = String::new();
    {
        let mut buffered = printer.fork(&mut init_body);
        generate_model_init(&mut &mut buffered, model, map)?;
    }
    emit_unused_guard(printer, &init_body, wants_init);
    printer.print(&init_body);
    printer.down();
    printer.print("}").nl().nl();
    printer
        .print("void ")
        .print(&struct_name)
        .print("_tick(")
        .print(&struct_name)
        .print(" *model")
        .print(&tick_append)
        .print(") {")
        .nl();
    // NOTICE: tick
    printer.up();
    printer.ident("assert(0 != model);").nl();
    // Проверка параметра печатается, только если он есть.
    if wants_tick {
        printer.ident("assert(0 != main);").nl();
    }
    let mut tick_body = String::new();
    {
        let mut buffered = printer.fork(&mut tick_body);
        generate_model_tick(&mut &mut buffered, model, map)?;
    }
    emit_unused_guard(printer, &tick_body, wants_tick);
    printer.print(&tick_body);
    printer.down();
    printer.print("}").nl().nl();
    printer
        .print("void ")
        .print(&struct_name)
        .print("_reset(")
        .print(&struct_name)
        .print(" *model")
        .print(&init_append)
        .print(") {")
        .nl();
    printer
        .up()
        .ident(format!("{}_init(model", struct_name).as_str())
        .print(&init_call_append)
        .print(");")
        .down()
        .nl();
    printer.print("}").nl().nl();
    printer
        .print("bool ")
        .print(&struct_name)
        .print("_is_done(const ")
        .print(&struct_name)
        .print(" *model")
        .print(") {")
        .nl();
    // Единственное терминальное состояние модели - всегда END
    let cond = format!("model->state == {}_END", name.unique_uppercase_snakecase());
    printer.up();
    // Тело `_is_done` указателем на корень не пользуется никогда, и с он туда не
    // печатается вовсе - заглушка не нужна.
    printer
        .ident("return ")
        .print(cond.as_str())
        .print(";")
        .down()
        .nl();
    printer.print("}").nl().nl();
    Ok(())
}
