//! Печать композиции состояний - цель `c`.
//!
//! Выделено из `c_model` по границе ответственности: модель отвечает за автомат
//! целиком, а этот модуль - за один вопрос, "как тикают ветви составного состояния":
//! параллельные - все разом, последовательные - по шагам, и вложенные цепочки внутри
//! параллели тоже.

use super::c_map::CMap;
use crate::generator::indent::Printer;
use crate::semantic::minimap::StateExtend;

/// Печатает такт вложенной последовательности.
///
/// Форма - та же машина шагов, что у последовательности верхнего уровня: на такте
/// исполняется один шаг, а по его завершении инициализируется следующий. Порядок
/// обязателен: `A + B` значит "сначала A, потом B", и параллельная печать (как делала
/// цель `rust`) даёт другой автомат.
///
/// Завершение цепочки - состояние `_END`, а не "все шаги готовы": у шага, который ещё
/// не начинался, `_is_done` спрашивать нельзя - его поле не инициализировано.
fn generate_nested_chain_tick(
    printer: &mut Printer,
    map: &CMap,
    parent_access: &str,
    parent_unique_upper: &str,
    items: &[StateExtend],
    caller_is_main: bool,
) {
    let state_field = format!("{}.state", parent_access);
    for (idx, item) in items.iter().enumerate() {
        let StateExtend::Model(name, _) = item else {
            // Вложенная параллель внутри вложенной цепочки в объём фичи не входит:
            // названная граница, а не пропуск (см. карточку 0426).
            continue;
        };
        let variant = format!(
            "{}_{}{}",
            parent_unique_upper,
            name.unique_uppercase_snakecase(),
            idx
        );
        let field = format!(
            "{}.{}{}",
            parent_access,
            name.local_lowercase_snakecase(),
            idx
        );
        let head = if idx == 0 { "if" } else { "} else if" };
        if idx == 0 {
            printer
                .ident(&format!("{head} ({state_field} == {variant}) {{"))
                .up()
                .nl();
        } else {
            printer
                .down()
                .ident(&format!("{head} ({state_field} == {variant}) {{"))
                .up()
                .nl();
        }
        let arg = map.root_arg(
            name,
            caller_is_main,
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
        printer
            .ident(&format!(
                "if ({}_is_done(&{})) {{",
                name.unique_camelcase(),
                field
            ))
            .up()
            .nl();
        match items.get(idx + 1) {
            Some(StateExtend::Model(next, _)) => {
                let next_field = format!(
                    "{}.{}{}",
                    parent_access,
                    next.local_lowercase_snakecase(),
                    idx + 1
                );
                let next_variant = format!(
                    "{}_{}{}",
                    parent_unique_upper,
                    next.unique_uppercase_snakecase(),
                    idx + 1
                );
                let init_arg = map.root_arg(
                    next,
                    caller_is_main,
                    crate::generator::c::c_needs::ModelFn::Init,
                );
                printer
                    .ident(&format!(
                        "{}_init(&{}{});",
                        next.unique_camelcase(),
                        next_field,
                        init_arg
                    ))
                    .nl();
                printer
                    .ident(&format!("{state_field} = {next_variant};"))
                    .nl();
            }
            _ => {
                printer
                    .ident(&format!("{state_field} = {parent_unique_upper}_END;"))
                    .nl();
            }
        }
        printer.down().ident("}").nl();
    }
    if items.iter().any(|i| matches!(i, StateExtend::Model(..))) {
        printer.down().ident("}").nl();
    }
}

/// Генерирует вызовы `_tick` для элементов параллельного блока.
///
/// Возвращает список C-выражений `{Name}_is_done(...)` для итоговой проверки готовности
/// всех веток. Вложенные параллели также тикаются рекурсивно.
pub(in crate::generator::c) fn generate_parallel_items_tick(
    printer: &mut Printer,
    map: &CMap,
    parent_access: &str,
    parent_unique_upper: &str,
    items: &[StateExtend],
    caller_is_main: bool,
) -> Vec<String> {
    let mut done_exprs = Vec::new();
    for (idx, item) in items.iter().enumerate() {
        match item {
            StateExtend::Model(name, _) => {
                let field = format!(
                    "{}.{}{}",
                    parent_access,
                    name.local_lowercase_snakecase(),
                    idx
                );
                let arg = map.root_arg(
                    name,
                    caller_is_main,
                    crate::generator::c::c_needs::ModelFn::Tick,
                );
                printer
                    .ident(&format!(
                        "{}_tick(&{}{});",
                        name.unique_camelcase(),
                        field,
                        arg,
                    ))
                    .nl();
                // `_is_done` указателя не принимает.
                done_exprs.push(format!("{}_is_done(&{})", name.unique_camelcase(), field));
            }
            StateExtend::Parallel(inner) => {
                let nested_access = format!("{}.parallel{}", parent_access, idx);
                let nested_upper = format!("{}_PARALLEL{}", parent_unique_upper, idx);
                let inner_done = generate_parallel_items_tick(
                    printer,
                    map,
                    &nested_access,
                    &nested_upper,
                    inner,
                    caller_is_main,
                );
                if !inner_done.is_empty() {
                    done_exprs.push(format!("({})", inner_done.join(" && ")));
                }
            }
            // Вложенная последовательность внутри параллели.
            //
            // Прежде эта ветвь была `_ => {}`: цепочка `A + B` внутри `| C` не тикала
            // Вовсе - прошивка исполняла половину автомата, а `cc` ловил это лишь
            // косвенно, по `unused-function`.
            StateExtend::Concatenation(inner) => {
                let nested_access = format!("{}.concat{}", parent_access, idx);
                let nested_upper = format!("{}_CONCAT{}", parent_unique_upper, idx);
                generate_nested_chain_tick(
                    printer,
                    map,
                    &nested_access,
                    &nested_upper,
                    inner,
                    caller_is_main,
                );
                done_exprs.push(format!("{}.state == {}_END", nested_access, nested_upper));
            }
            StateExtend::None => {}
        }
    }
    done_exprs
}
