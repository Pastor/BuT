//! Машина шагов последовательной композиции у цели `c` - Один носитель имён.
//!
//! # Что здесь живёт
//!
//! Состояние-цепочка (`state S = A + B;`) исполняется в порождённом C машиной шагов:
//! поле `model->{s}_state` хранит активный шаг, перечислитель шага называет его, а поля
//! `model->{s}_{имя}{idx}` держат сами шаги. Эти имена знают **три** места: печать
//! такта (`c_model::generate_concat_tick`), инициализация шага
//! (`c_model_init::generate_concat_item_init`) и страж строки табличной формы
//! (`c_table`).
//!
//! Первые два уже держали имя варианта **каждое своё** - две копии одного правила,
//! разъехаться которым мешала только внимательность. Третий потребитель сделал бы копий
//! три, поэтому имена вынесены сюда: правка формы имени теперь одна на всех.
//!
//! **Разбор шага не исчерпывающий, и это осознанно.** Печать такта умеет шаг-модель и
//! шаг-параллель; вложенная цепочка шагом (`(A + B) + C`) не тикается вовсе - она и не
//! даёт варианта. Носитель повторяет это решение (`None`), а не изобретает своё:
//! расширится печать - расширится и он.

use crate::semantic::minimap::{Name, StateExtend};

use super::c_map::CMap;
use crate::generator::indent::Printer;

/// Поле активного шага цепочки в структуре модели.
pub(super) fn step_field(state_local: &str) -> String {
    format!("model->{state_local}_state")
}

/// Перечислитель активного шага.
///
/// `None` - шаг такой формы печать такта не ведёт (вложенная цепочка, неразрешённая
/// реализация): у него нет ни варианта, ни тика.
pub(super) fn step_variant(
    state_unique_upper: &str,
    item: &StateExtend,
    idx: usize,
) -> Option<String> {
    match item {
        StateExtend::Model(name, _) => Some(format!(
            "{}_{}{}",
            state_unique_upper,
            name.local_lowercase_snakecase().to_uppercase(),
            idx,
        )),
        StateExtend::Parallel(_) => Some(parallel_upper(state_unique_upper, idx)),
        StateExtend::Concatenation(_) | StateExtend::None => None,
    }
}

/// Поле шага-модели в структуре несущей модели.
pub(super) fn model_access(state_local: &str, name: &Name, idx: usize) -> String {
    format!(
        "model->{}_{}{}",
        state_local,
        name.local_lowercase_snakecase(),
        idx
    )
}

/// Поле шага-параллели в структуре несущей модели.
pub(super) fn parallel_access(state_local: &str, idx: usize) -> String {
    format!("model->{state_local}_parallel{idx}")
}

/// Преисправление перечислителей вложенной параллели шага.
pub(super) fn parallel_upper(state_unique_upper: &str, idx: usize) -> String {
    format!("{state_unique_upper}_PARALLEL{idx}")
}

/// Предикат "шаг завершён" - то самое условие, под которым печать такта исполняет
/// переход из состояния-цепочки.
///
/// `None` - шаг не тикается (см. шапку модуля), то есть завершения у него не наступает
/// никогда.
///
/// Готовность параллели берётся у **того же** носителя, что печатает её тик
/// (`c_compose::generate_parallel_items_tick`): он возвращает список готовностей
/// попутно с печатью, и второе знание о раскладке её полей разошлось бы с ним при
/// первой правке. Текст тика уходит в буфер и выбрасывается - тела печатает `c_model`.
pub(super) fn step_done(
    map: &CMap,
    (state_local, state_unique_upper): (&str, &str),
    item: &StateExtend,
    idx: usize,
    caller_is_main: bool,
) -> Option<String> {
    match item {
        StateExtend::Model(name, _) => Some(format!(
            "{}_is_done(&{})",
            name.unique_camelcase(),
            model_access(state_local, name, idx)
        )),
        StateExtend::Parallel(inner) => {
            let mut sink = String::new();
            let mut scratch = Printer::new(4, &mut sink);
            let done = super::c_compose::generate_parallel_items_tick(
                &mut scratch,
                map,
                &parallel_access(state_local, idx),
                &parallel_upper(state_unique_upper, idx),
                inner,
                caller_is_main,
            );
            if done.is_empty() {
                return None;
            }
            Some(done.join(" && "))
        }
        StateExtend::Concatenation(_) | StateExtend::None => None,
    }
}

/// Условие внешнего перехода состояния-цепочки: "цепочка на последнем шаге, и он
/// завершён".
///
/// Именно при нём печать такта исполняет переход из состояния-цепочки, поэтому это же
/// выражение служит стражем строки табличной формы.
pub(super) fn chain_done(
    map: &CMap,
    (state_local, state_unique_upper): (&str, &str),
    items: &[StateExtend],
    caller_is_main: bool,
) -> Option<String> {
    let idx = items.len().checked_sub(1)?;
    let last = items.get(idx)?;
    let variant = step_variant(state_unique_upper, last, idx)?;
    let done = step_done(
        map,
        (state_local, state_unique_upper),
        last,
        idx,
        caller_is_main,
    )?;
    Some(format!(
        "{} == {} && {}",
        step_field(state_local),
        variant,
        done
    ))
}
