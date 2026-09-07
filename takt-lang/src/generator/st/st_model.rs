//! Отображение автомата модели в `CASE state OF` внутри `FUNCTION_BLOCK`.
//!
//! **часть 1: простые модели**. Подключает
//! печатники: `st_expr` (условия), `st_stmt` (тела блоков), `st_func` (функции).
//!
//! ## Изоморфизм с целью `c` - по зонду, а не по памяти
//!
//! Форма снята с **реального** вывода `taktc compile -t c`  и
//! воспроизводится один в один:
//!
//! | Факт | Зонд C | ST |
//! |---|---|---|
//! | Ф3: `INIT` исполняет `enter` стартового и переходит в него | `case ..._INIT: { enter; state = ..._START; }` | ветвь `0:` |
//! | Ф4: `enter` **целевого** инлайнится в переход | `if (cond) { enter_target; state = ..._T; }` | тело `IF` |
//! | `exit` **источника** - перед `enter` цели | `n := 2; n := 3;` (зонд `exit_probe`) | то же |
//! | Ф5: порядок `ref` = порядок проверки, первый выигрывает | цепочка `if ... break;` | `IF ... ELSIF ...` |
//! | Ф8: состояние без исходящих `ref` исполняет `exit` и уходит в `END` | `case B: { n = 4; state = END; }` | то же |
//!
//! `always` исполняется **первым** в ветви - до проверок переходов.
//!
//! ## Факты MatIEC, определившие форму
//!
//! - **Пустая ветвь `CASE` недопустима**: `1: ;` -> `invalid statement in case
//!   element of ST 'CASE' statement`. Поэтому терминальная ветвь `END` печатает
//!   само-присваивание `state := <END>;` - семантически пустое, синтаксически
//!   обязательное.
//! - Перечислимых типов состояний нет, поэтому номера состояний -
//!   числовые литералы, а не имена; читаемость держится на комментариях.

use crate::diagnostics::{Diagnostic, Location};
use crate::generator::indent::Printer;
use crate::generator::st::st_compose::{Instance, emit_composition};
use crate::generator::st::st_map::StMap;
use crate::generator::st::st_stmt::{Hoisted, StmtOutput, print_statement};
use crate::generator::st::st_time;
use crate::semantic::minimap::{Element, Name, StateExtend};
use crate::semantic::type_node::TypeNode;
use crate::semantic::{ExpressionNode, VariableNode};
use crate::semantic::{ModelNode, NamedCodeBlockDefinitionNode, StateNode};
use std::collections::HashMap;

/// Номер синтетического состояния `INIT`.
///
/// Ноль не случаен: холодный старт ПЛК обнуляет `VAR`, поэтому автомат сам оказывается
/// в `INIT` без отдельного вызова инициализации.
const INIT_STATE: usize = 0;

/// Результат печати тела: побочные эффекты операторов плюс экземпляры под-FB.
#[derive(Default, Debug)]
pub(crate) struct BodyOutput {
    /// Поднятые объявления и предупреждения печатника операторов.
    pub stmt: StmtOutput,
    /// Экземпляры под-FB, которые вызывающий обязан объявить в `VAR`.
    pub instances: Vec<Instance>,
}

/// Таблица номеров состояний модели: `INIT` = 0, состояния, `END` последним.
pub(crate) struct StateTable {
    /// Номер по уникальному имени состояния.
    numbers: HashMap<String, usize>,
    /// Состояния в порядке печати (номер = индекс + 1).
    ordered: Vec<Name>,
    /// Номер синтетического `END`.
    pub(crate) end: usize,
}

impl StateTable {
    /// Строит таблицу номеров.
    ///
    /// Порядок - лексикографический по уникальному имени. Та же причина, по которой
    /// отсортированы подмодели.
    pub fn build(states: &[Name]) -> Self {
        let mut ordered: Vec<Name> = states.to_vec();
        ordered.sort_by(|a, b| a.unique().cmp(b.unique()));
        let mut numbers = HashMap::new();
        for (i, name) in ordered.iter().enumerate() {
            numbers.insert(name.unique().to_string(), i + 1);
        }
        let end = ordered.len() + 1;
        Self {
            numbers,
            ordered,
            end,
        }
    }

    /// Номер состояния по имени.
    pub(crate) fn number_of(&self, name: &str) -> Option<usize> {
        self.numbers.get(name).copied()
    }
}

/// Печатает тело `FUNCTION_BLOCK`: `CASE state OF` и признак завершения.
///
/// Возвращает побочные результаты печати тел (поднятые объявления, предупреждения
/// `ST-010`) - вызывающий обязан объявить поднятое в шапке POU.
///
/// # Ошибки
/// - `ST-011` - состояние с композицией (`= M1 | M2`): часть 2.
/// - `ST-013` - переход ведёт в неизвестное состояние.
/// - Диагностики печатников выражений и операторов.
pub(crate) fn emit_body(
    p: &mut Printer,
    map: &StMap,
    element: &Element,
    model: &ModelNode,
    table: &StateTable,
) -> Result<BodyOutput, Diagnostic> {
    let Element::Model { start, .. } = element else {
        return Err(Diagnostic::error(
            crate::generator::site::at(Location::Codegen),
            "Тело автомата строится только для модели".to_string(),
        )
        .with_code("ST-012"));
    };
    let mut out = BodyOutput::default();

    // model-level `always` (вне состояния) - каждый такт до `CASE`, безусловно по
    // состоянию (эталон - шаг 2 `execution("always")` симулятора).
    emit_model_block(p, model, "always", &mut out.stmt)?;

    // Вход в стартовое состояние - До `CASE` и без расхода скана. Ровно как цель `c`:
    // `if (model->state == ..._INIT) { ... }` перед `switch`, без `break`, - тот же
    // скан попадает в ветвь стартового состояния.
    //
    // Ветвью `CASE` это выразить нельзя: `CASE` в IEC не проваливается в следующую
    // ветвь, поэтому `0: state := 1;` заканчивал скан, ничего не исполнив.
    let start_no = table.number_of(start.unique()).ok_or_else(|| {
        unknown_state(&format!(
            "стартовое состояние '{}' отсутствует в таблице номеров",
            start
        ))
    })?;
    p.ident(&format!("IF state = {} THEN (* первый скан *)", INIT_STATE))
        .nl();
    p.up();
    // Инициализатор, не выразимый объявлением IEC, печатается здесь. Сегодня это массив
    // структур: ни одна из трёх проверенных форм (`[(1, 2), ...]`, `[(v := 1, ...),
    // ...]`, `((v := 1, ...), ...)`) `iec2c` не принимается, а молчаливая потеря
    // значения - расхождение с эталоном.
    emit_deferred_inits(p, model)?;
    let start_state = raw_state(model, start)?;
    emit_block(p, &start_state, "enter", model, &mut out.stmt)?;
    p.ident(&format!("state := {}; (* {} *)", start_no, start.local()))
        .nl();
    p.down();
    p.ident("END_IF;").nl();

    p.ident("CASE state OF").nl();
    p.up();

    for name in &table.ordered {
        let number = table
            .number_of(name.unique())
            .ok_or_else(|| unknown_state(name.unique()))?;
        let state = raw_state(model, name)?;
        p.ident(&format!("{}: (* {} *)", number, name.local())).nl();
        p.up();
        // Тело ветви печатается в буфер и, если оказалось пустым, заменяется
        // само-присваиванием: пустая ветвь `CASE` в IEC недопустима ("invalid statement
        // in case element").
        //
        // Пустой ветвь бывает у двух форм: табличной и у состояния, всё тело которого -
        // `enter`, исполняемый на входе, при отсутствии рёбер.
        let mut body = String::new();
        {
            let mut buffered = p.fork(&mut body);
            emit_state(&mut buffered, map, name, &state, model, table, &mut out)?;
        }
        if body.trim().is_empty() {
            p.ident(&format!("state := {}; (* тело ветви пусто *)", number))
                .nl();
        } else {
            p.print(&body);
        }
        p.down();
    }

    // Терминальная ветвь. Само-присваивание - не описка: пустая ветвь `CASE`
    // синтаксически недопустима ("invalid statement in case element"), а семантически
    // здесь ничего происходить не должно.
    p.ident(&format!("{}: (* END *)", table.end)).nl();
    p.up();
    p.ident(&format!("state := {};", table.end)).nl();
    p.down();

    p.down();
    p.ident("END_CASE;").nl();

    // Табличная форма: переходы просматривает диспетчер - После тел состояний и до
    // обновления счётчика сканов, ровно там, где стоял переход внутри ветви `CASE`.
    if map.fsm_table() {
        super::st_table::emit_dispatcher(p, map, element, model, table, &mut out)?;
    }

    // Обновление счётчика сканов: вход в состояние (state <> prev) сбрасывает счётчик в
    // 1, иначе он растёт - одним сравнением в конце скана, как
    // `c_time::emit_state_time_update`.
    if st_time::needs_dwell(map, model) {
        let dwell = st_time::DWELL_FIELD;
        let prev = st_time::PREV_STATE_FIELD;
        p.ident(&format!("IF state <> {} THEN", prev)).nl();
        p.up();
        p.ident(&format!("{} := 1;", dwell)).nl();
        // Аккумуляторы `every` обнуляются при входе - период с нуля.
        for e in st_time::model_every(model) {
            p.ident(&format!("{} := 0;", st_time::every_field(e.idx)))
                .nl();
        }
        p.down();
        p.ident("ELSE").nl();
        p.up();
        p.ident(&format!("{} := {} + 1;", dwell, dwell)).nl();
        p.down();
        p.ident("END_IF;").nl();
        p.ident(&format!("{} := state;", prev)).nl();
        out.stmt.hoisted.push(Hoisted {
            name: dwell.to_string(),
            ty: st_time::dwell_type(),
        });
        out.stmt.hoisted.push(Hoisted {
            name: prev.to_string(),
            ty: st_time::prev_state_type(),
        });
    }

    // Признак завершения - выход FB; по нему родитель узнаёт об окончании.
    p.ident(&format!("is_done := state = {};", table.end)).nl();
    Ok(out)
}

/// Печатает содержимое ветви одного состояния.
fn emit_state(
    p: &mut Printer,
    map: &StMap,
    name: &Name,
    state: &StateNode,
    model: &ModelNode,
    table: &StateTable,
    out: &mut BodyOutput,
) -> Result<(), Diagnostic> {
    // `always` - первым в ветви, до проверок переходов (S8, Ф5).
    emit_block(p, state, "always", model, &mut out.stmt)?;

    // Периодические блоки `every` - после `always`. Профиль "часы" ->
    // самосбрасывающийся `TON` (`IN := NOT Q` - классический IEC-мигатель); "такты" ->
    // счётчик-аккумулятор `takt_everyN` (как цель `c`).
    for e in st_time::model_every(model) {
        if e.state != name.local() {
            continue;
        }
        if st_time::is_clock(map) {
            let ton = st_time::every_timer(e.idx);
            p.ident(&format!(
                "{ton}(IN := NOT {ton}.Q, PT := {});",
                crate::semantic::duration::time_literal(e.period_nanos)
            ))
            .nl();
            out.instances.push(Instance {
                name: ton.clone(),
                fb_type: st_time::TON_TYPE.to_string(),
                init: None,
            });
            p.ident(&format!("IF {ton}.Q THEN")).nl();
            p.up();
            print_statement(e.body, model, p, &mut out.stmt, None)?;
            p.down();
            p.ident("END_IF;").nl();
        } else {
            let acc = st_time::every_field(e.idx);
            let units = crate::semantic::duration::units_or_diagnostic(
                e.period_nanos,
                map.time_profile(),
                Location::Codegen,
                "период 'every'",
            )?;
            p.ident(&format!(
                "IF {dwell} - {acc} >= {units} THEN",
                dwell = st_time::DWELL_FIELD
            ))
            .nl();
            p.up();
            print_statement(e.body, model, p, &mut out.stmt, None)?;
            p.ident(&format!("{acc} := {acc} + {units};")).nl();
            p.down();
            p.ident("END_IF;").nl();
            out.stmt.hoisted.push(Hoisted {
                name: acc,
                ty: st_time::every_field_type(),
            });
        }
    }

    // Состояние с реализацией (`= Модель`, `= M1 | M2`) - композиция.
    if let Some(Element::StateExtend { extend, next, .. }) = map.element_of(name)
        && !matches!(extend, StateExtend::None)
    {
        return emit_composition(p, map, name, &extend, &next, table, out, state, model);
    }

    let references = match state {
        StateNode::Simple { references, .. } | StateNode::Implement { references, .. } => {
            references.clone()
        }
        StateNode::Unresolved => Vec::new(),
    };

    // Табличная форма: переходы печатает таблица, а в ветви остаётся только тело такта.
    // Таймеры выдержки - часть тела: их взводит каждый скан тот же носитель, чьи имена
    // читает страж строки.
    if map.fsm_table() {
        super::st_edges::emit_edge_timers(p, map, name, &references, out);
        return Ok(());
    }

    // Признак терминальности - Общий (`StateNode::is_terminated`): состояние С телом
    // переходов не требует и автомат не завершает. Пока цель судила по одним рёбрам,
    // `always` без переходов исполнялся ровно один скан, тогда как эталон крутит его
    // вечно.
    if state.is_terminated() {
        // Состояние без переходов и без тела терминально: исполняет `exit` и уходит в
        // `END` - как `case B` в зонде `exit_probe` (Ф8).
        emit_block(p, state, "exit", model, &mut out.stmt)?;
        p.ident(&format!("state := {}; (* END *)", table.end)).nl();
        return Ok(());
    }

    super::st_edges::emit_edges(p, map, (name, state), model, table, out, &references)?;
    Ok(())
}

/// Печатает переход: `exit` источника, `enter` цели, смена состояния.
///
/// Порядок снят зондом (`exit_probe`), а не предположен: `exit` источника исполняется
/// **перед** `enter` цели, и оба - в такте перехода (Ф4).
pub(crate) fn emit_transition(
    p: &mut Printer,
    source: &StateNode,
    target_name: &str,
    target_number: usize,
    model: &ModelNode,
    out: &mut StmtOutput,
) -> Result<(), Diagnostic> {
    emit_block(p, source, "exit", model, out)?;
    if let Some(target) = model.states.get(target_name) {
        emit_block(p, target, "enter", model, out)?;
    }
    p.ident(&format!(
        "state := {}; (* {} *)",
        target_number, target_name
    ))
    .nl();
    Ok(())
}

/// Печатает тело именованного блока (`enter`/`exit`/`always`) состояния.
pub(crate) fn emit_block(
    p: &mut Printer,
    state: &StateNode,
    kind: &str,
    model: &ModelNode,
    out: &mut StmtOutput,
) -> Result<(), Diagnostic> {
    let blocks = match state {
        StateNode::Simple { named_blocks, .. } | StateNode::Implement { named_blocks, .. } => {
            named_blocks
        }
        StateNode::Unresolved => return Ok(()),
    };
    for block in blocks {
        let body = match (kind, block) {
            ("enter", NamedCodeBlockDefinitionNode::Enter { body, .. })
            | ("exit", NamedCodeBlockDefinitionNode::Exit { body, .. })
            | ("always", NamedCodeBlockDefinitionNode::Always { body, .. }) => body,
            _ => continue,
        };
        print_statement(body, model, p, out, None)?;
    }
    Ok(())
}

/// Печатает тело именованного блока **уровня модели**: `always` вне состояния. Аналог
/// [`emit_block`], но источник - сама модель.
fn emit_model_block(
    p: &mut Printer,
    model: &ModelNode,
    kind: &str,
    out: &mut StmtOutput,
) -> Result<(), Diagnostic> {
    for block in &model.named_blocks {
        let body = match (kind, block) {
            ("enter", NamedCodeBlockDefinitionNode::Enter { body, .. })
            | ("exit", NamedCodeBlockDefinitionNode::Exit { body, .. })
            | ("always", NamedCodeBlockDefinitionNode::Always { body, .. }) => body,
            _ => continue,
        };
        print_statement(body, model, p, out, None)?;
    }
    Ok(())
}

/// Возвращает семантический узел состояния по имени карты.
fn raw_state(model: &ModelNode, name: &Name) -> Result<StateNode, Diagnostic> {
    model
        .states
        .get(name.local())
        .cloned()
        .ok_or_else(|| unknown_state(name.local()))
}

impl StateTable {
    /// Номер состояния по **локальному** имени (как его пишет `ref`).
    pub(crate) fn number_of_local(&self, local: &str) -> Option<usize> {
        self.ordered
            .iter()
            .find(|n| n.local() == local)
            .and_then(|n| self.number_of(n.unique()))
    }
}

/// Строит диагностику `ST-013` - переход в неизвестное состояние.
pub(crate) fn unknown_state(what: &str) -> Diagnostic {
    Diagnostic::error(Location::Codegen, format!("Автомат ST: {}", what)).with_code("ST-013")
}

/// Печатает инициализаторы, которые объявление IEC выразить не может.
///
/// Сегодня это **массив структур**: `iec2c` не принимает агрегат такого типа в `VAR` ни
/// в одной из проверенных форм. Значения кладутся операторами первого скана - до входа
/// в стартовое состояние, то есть до любого тела, которое их прочитает.
///
/// Место записи выбирает общий носитель (`generator::aggregate`): поле структуры
/// адресуется **по имени**, а не по индексу.
fn emit_deferred_inits(p: &mut Printer, model: &ModelNode) -> Result<(), Diagnostic> {
    for (name, var) in &model.variables {
        let VariableNode::Simple { ty, expr, .. } = var else {
            continue;
        };
        // Поле-Массив внутри структуры: агрегат объявления его не принимает, значение
        // кладётся здесь - до входа в стартовое состояние.
        if let TypeNode::Struct(struct_name) = ty
            && let ExpressionNode::Initializer(items) | ExpressionNode::Array(items) = expr
            && let Some(def) = model.search_struct(struct_name)
        {
            for ((field, field_ty), value) in def.fields.iter().zip(items) {
                if !crate::generator::st::st_decl::field_is_deferred(field_ty) {
                    continue;
                }
                let (ExpressionNode::Initializer(inner) | ExpressionNode::Array(inner)) = value
                else {
                    continue;
                };
                // Агрегат раскрывается до листьев общим носителем: отложенным бывает и
                // поле-массив, и поле-Структура - у второго путь длиннее одного шага, и
                // своя одноуровневая печать теряла бы вложенные значения молча.
                let fields_of = |sname: &str| model.search_struct(sname).map(|d| d.fields);
                for leaf in crate::generator::aggregate::leaves(Some(field_ty), inner, &fields_of) {
                    let text = match &leaf.ty {
                        Some(ty) => {
                            crate::generator::st::st_expr::coerce_to(leaf.value, ty, model)?
                        }
                        None => crate::generator::st::st_expr::print_expression(leaf.value, model)?,
                    };
                    let suffix = crate::generator::st::st_multidim::iec_suffix(&leaf.path);
                    p.ident(&format!("{name}.{field}{suffix} := {text};")).nl();
                }
            }
            continue;
        }
        let TypeNode::Array(_, elem) = ty else {
            continue;
        };
        let TypeNode::Struct(struct_name) = &**elem else {
            continue;
        };
        let (ExpressionNode::Initializer(items) | ExpressionNode::Array(items)) = expr else {
            continue;
        };
        let fields = model.search_struct(struct_name).map(|def| def.fields);
        for (index, item) in items.iter().enumerate() {
            let (ExpressionNode::Initializer(inner) | ExpressionNode::Array(inner)) = item else {
                continue;
            };
            let places =
                crate::generator::aggregate::places(fields.as_deref(), Some(elem), inner.len());
            for (value, place) in inner.iter().zip(places) {
                let text = match &place.ty {
                    Some(field_ty) => {
                        crate::generator::st::st_expr::coerce_to(value, field_ty, model)?
                    }
                    None => crate::generator::st::st_expr::print_expression(value, model)?,
                };
                p.ident(&format!("{name}[{index}]{} := {text};", place.suffix))
                    .nl();
            }
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic::tree::construct_model;

    /// Печатает тело автомата корневой модели исходника.
    fn body_of(src: &str) -> String {
        let (ast, _) = crate::parse(src, 0).unwrap();
        let rc = construct_model(&ast, None, &[]).unwrap();
        rc.borrow_mut().name = Some("Root".to_string());
        let map = StMap::new("root", &rc.borrow(), false, Default::default()).unwrap();
        let element = map.model();
        let Element::Model { states, .. } = &element else {
            panic!("корень не модель");
        };
        let table = StateTable::build(states);
        let model = rc.borrow();
        let mut text = String::new();
        let mut p = Printer::new(4, &mut text);
        emit_body(&mut p, &map, &element, &model, &table).expect("тело должно печататься");
        text
    }

    /// Автомат превращается в `CASE state OF ... END_CASE;`.
    #[test]
    fn test_emits_case_state_of() {
        let st = body_of("var n: u8 := 0;\nstart A { always { n := n + 1; } }");
        assert!(st.contains("CASE state OF"), "нет CASE:\n{st}");
        assert!(st.contains("END_CASE;"), "CASE обязан закрываться:\n{st}");
    }

    /// Вход в стартовое состояние идёт **до** `CASE`: `enter`, затем переход.
    ///
    /// Сверка с зондом C (Ф3): `if (state == ..._INIT) { enter; state = ..._START; }`
    /// **перед** `switch`. Ноль не случаен: холодный старт ПЛК обнуляет `VAR`,
    /// поэтому автомат сам оказывается в `INIT`.
    ///
    /// Порядок `enter` -> переход проверяется здесь, а не глазами: при выносе ветви из
    /// `CASE` его легко потерять.
    #[test]
    fn test_init_runs_enter_then_transitions_before_case() {
        let st = body_of("var n: u8 := 0;\nstart A { enter { n := 1; } }");
        let init = st
            .find(&format!("IF state = {INIT_STATE} THEN"))
            .expect("нет входа в стартовое состояние");
        let enter = st.find("n := 1;").expect("нет enter стартового");
        let go = st.find("state := 1;").expect("нет перехода в стартовое");
        let case = st.find("CASE state OF").expect("нет CASE");
        assert!(
            init < enter && enter < go,
            "порядок INIT→enter→переход:\n{st}"
        );
        assert!(
            go < case,
            "вход в стартовое обязан идти ДО CASE — иначе он стоит скана \
             (контракт 0033):\n{st}"
        );
    }

    /// Ветви `INIT` внутри `CASE` больше нет - она стоила скана на каждом уровне
    /// вложенности.
    #[test]
    fn test_no_init_branch_inside_case() {
        let st = body_of("var n: u8 := 0;\nstart A { always { n := n + 1; } }");
        assert!(
            !st.contains(&format!("{INIT_STATE}: (* INIT *)")),
            "ветвь INIT вернулась в CASE: `CASE` в IEC не проваливается, \
             поэтому такая ветвь заканчивает скан, ничего не исполнив:\n{st}"
        );
    }

    /// `always` исполняется первым в ветви - до проверок переходов.
    #[test]
    fn test_always_runs_before_transition_checks() {
        let st = body_of(
            "var n: u8 := 0;\ncond Go = n = 1;\n\
             start A { always { n := n + 1; } ref B: Go; }\nstate B {}",
        );
        let always = st.find("n := n + 1;").expect("нет always");
        let check = st.find("IF n = 1 THEN").expect("нет проверки перехода");
        assert!(always < check, "always обязан идти до проверок:\n{st}");
    }

    /// `exit` источника исполняется **перед** `enter` цели - по зонду, не по догадке.
    ///
    /// Зонд C (`exit_probe`): `if (model->n == 1) { model->n = 2; model->n = 3; ... }` -
    /// где `n := 2` это `exit` источника, а `n := 3` - `enter` цели.
    #[test]
    fn test_exit_of_source_runs_before_enter_of_target() {
        let st = body_of(
            "var n: u8 := 0;\ncond Go = n = 1;\n\
             start A { exit { n := 2; } ref B: Go; }\n\
             state B { enter { n := 3; } }",
        );
        let exit = st.find("n := 2;").expect("нет exit источника");
        let enter = st.find("n := 3;").expect("нет enter цели");
        assert!(
            exit < enter,
            "exit источника обязан идти до enter цели:\n{st}"
        );
    }

    /// Несколько `ref` -> цепочка `IF/ELSIF`: порядок объявления = порядок проверки.
    ///
    /// Сверка с зондом C (Ф5, `stacker.c:82-101`): три независимых `if` с `break`,
    /// первый сработавший выигрывает - это и есть `ELSIF`-цепочка.
    #[test]
    fn test_multiple_refs_become_if_elsif_chain_in_source_order() {
        let st = body_of(
            "var n: u8 := 0;\ncond G1 = n = 1;\ncond G2 = n = 2;\n\
             start A { ref B: G1; ref C: G2; }\nstate B {}\nstate C {}",
        );
        let first = st.find("IF n = 1 THEN").expect("нет первого перехода");
        let second = st.find("ELSIF n = 2 THEN").expect("нет второго перехода");
        assert!(first < second, "порядок ref обязан сохраняться:\n{st}");
        assert!(st.contains("END_IF;"), "цепочка обязана закрываться:\n{st}");
    }

    /// Состояние без исходящих `ref` терминально: `exit` и уход в `END` (Ф8).
    #[test]
    fn test_state_without_refs_runs_exit_and_goes_to_end() {
        let st = body_of(
            "var n: u8 := 0;\ncond Go = n = 1;\n\
             start A { ref B: Go; }\nstate B { exit { n := 4; } }",
        );
        let b_branch = st.find("(* B *)").expect("нет ветви B");
        let tail = &st[b_branch..];
        assert!(
            tail.contains("n := 4;"),
            "exit терминального не исполнен:\n{st}"
        );
        assert!(tail.contains("(* END *)"), "нет ухода в END:\n{st}");
    }

    /// Терминальная ветвь `END` печатает само-присваивание, а не пустоту.
    ///
    /// Не косметика: пустая ветвь `CASE` синтаксически недопустима - `iec2c` отвечает
    /// "invalid statement in case element of ST 'CASE' statement".
    #[test]
    fn test_end_branch_is_not_empty_because_iec_forbids_it() {
        let st = body_of("var n: u8 := 0;\nstart A { always { n := n + 1; } }");
        // Ищем именно метку ветви (`2: (* END *)`), а не комментарий `(* END *)` у
        // перехода: первое вхождение подстроки - как раз переход.
        let label = st
            .lines()
            .position(|l| l.trim_start().starts_with("2: (* END *)"))
            .expect("нет метки ветви END");
        let body = st.lines().nth(label + 1).unwrap_or("");
        assert!(
            body.trim_start().starts_with("state :="),
            "ветвь END обязана содержать оператор, иначе iec2c её отвергнет:\n{st}"
        );
    }

    /// Признак завершения - выход FB: по нему родитель узнаёт об окончании.
    #[test]
    fn test_is_done_reflects_end_state() {
        let st = body_of("var n: u8 := 0;\nstart A { always { n := n + 1; } }");
        assert!(
            st.contains("is_done := state = "),
            "нет признака завершения:\n{st}"
        );
    }

    /// Одиночная под-модель (`= Controller`) -> экземпляр под-FB и вызов.
    #[test]
    fn test_single_submodel_becomes_instance_call() {
        let st = body_of("model M { start Q { } }\nvar n: u8 := 0;\nstart Entry = M;");
        assert!(st.contains("m0("), "нет вызова экземпляра под-FB:\n{st}");
        assert!(
            st.contains("IF m0.is_done THEN"),
            "завершение композиции — по is_done под-FB:\n{st}"
        );
    }

    /// Параллельная композиция: вызовы последовательны, завершение - конъюнкция.
    ///
    /// Сверка с зондом C (Ф6, `stacker.c:414-439`): под-модели вызываются
    /// последовательно в одном такте родителя, в порядке объявления; завершение -
    /// конъюнкция `is_done`. Настоящей конкурентности нет.
    #[test]
    fn test_parallel_composition_calls_sequentially_and_joins_by_conjunction() {
        let st = body_of(
            "model A { start Q { } }\nmodel B { start R { } }\n\
             var n: u8 := 0;\nstart Main = A | B;",
        );
        let a = st.find("a0(").expect("нет вызова A");
        let b = st.find("b1(").expect("нет вызова B");
        assert!(
            a < b,
            "порядок вызовов обязан совпадать с порядком объявления:\n{st}"
        );
        assert!(
            st.contains("IF a0.is_done AND b1.is_done THEN"),
            "завершение — конъюнкция is_done:\n{st}"
        );
    }

    /// Экземпляры нумеруются: одна модель может входить в композицию несколько раз.
    ///
    /// Вход не гипотетический: `elevator.takt:198` включает `Engine` пять раз.
    #[test]
    fn test_repeated_model_gets_distinct_instances() {
        let st = body_of("model A { start Q { } }\nvar n: u8 := 0;\nstart Main = A | A;");
        assert!(st.contains("a0("), "нет первого экземпляра:\n{st}");
        assert!(
            st.contains("a1("),
            "повторная модель обязана получить свой экземпляр:\n{st}"
        );
    }

    /// Последовательная композиция (`M1 + M2`) - вложенный `CASE` по счётчику шагов.
    ///
    /// Форма из зонда цели `c` (`extend_complex.h`): у конкатенации там свой `enum`
    /// шагов, отдельный от состояния модели. Шаг сменяется по `is_done` своей группы -
    /// то есть модели идут последовательно, а не параллельно.
    #[test]
    fn test_concatenation_becomes_nested_case_over_step_counter() {
        let st = body_of(
            "model A { start Q { } }\nmodel B { start R { } }\n\
             var n: u8 := 0;\nstart Main = A + B;",
        );
        assert!(
            st.contains("CASE main_step OF"),
            "нет счётчика шагов:\n{st}"
        );
        let a = st.find("main_a0(").expect("нет шага A");
        let b = st.find("main_b1(").expect("нет шага B");
        assert!(a < b, "шаги обязаны идти в порядке объявления:\n{st}");
        assert!(
            st.contains("main_step := 1;"),
            "шаг обязан сменяться по is_done группы:\n{st}"
        );
    }

    /// Нумерация состояний устойчива между запусками.
    ///
    /// Тест против недетерминизма: `states()` обходит `HashMap`, а номер состояния -
    /// часть ABI порождённого ПЛК-кода.
    #[test]
    fn test_state_numbering_is_deterministic() {
        let src = "var n: u8 := 0;\ncond G = n = 1;\n\
                   start A { ref B: G; }\nstate B { ref C: G; }\nstate C {}";
        let first = body_of(src);
        for i in 1..6 {
            assert_eq!(first, body_of(src), "прогон {i} дал другую нумерацию");
        }
    }
}
