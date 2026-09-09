//! Печать условий перехода, включая `S(Модель) = Состояние`.
//!
//! Часть модуля `c_expr`.

use super::*;
use crate::diagnostics::lang::keys;
use crate::msg;

// Модель, о **текущем состоянии** которой идёт речь в левой части сравнения:
// `S(Модель)` либо краткая форма `Модель`. Обе записи означают одно и то же и дают один
// и тот же C.
//
// **Скобки здесь не разворачиваются - и не нужно.** Скобочные формы (`(S(Ping)) = End`,
// `S((Ping)) = End`, `S(Ping) = (End)`) канонизирует семантика до генератора:
// `resolve_condition` снимает прозрачные скобки паттерна `S(Модель)` в единой воронке
// разбора. Сюда условие приходит уже каноничным `Function(S, [Model])`.
use crate::semantic::condition::state_of::{compared_state_name, state_of_model};

/// Печатает сравнение текущего состояния модели с её состоянием: `<путь к модели>.state
/// == {MODEL}_{STATE}`.
///
/// Общая реализация для `=` и `!=` (`op`): ветки различались **только** оператором и
/// были дословными копиями по сорок строк каждая.
fn generate_state_comparison(
    model: &Rc<RefCell<ModelNode>>,
    right: &ConditionNode,
    op: &str,
    map: &CMap,
    owner: &Element,
) -> Result<String, Diagnostic> {
    // Разбор трёх форм правой части - общий с эталоном: своя копия разошлась бы на
    // форме, которую видит только один из потребителей. Защитная ветвь идёт без
    // `Debug`-дампа: он напечатал бы внутреннее представление узла целиком.
    let Some(eq_name) = compared_state_name(right) else {
        return Err(Diagnostic::error(
            crate::generator::site::at(Location::Codegen),
            msg!(keys::CC_STATE_OF_ARGUMENT),
        )
        .with_code("CC-013"));
    };

    let model_name = Name::from(model.clone());
    let using_models = map.using_models();
    let element = using_models
        .iter()
        .find(|m| m.name().eq(&model_name))
        .ok_or_else(|| {
            Diagnostic::error(
                crate::generator::site::at(Location::Codegen),
                msg!(keys::CC_MODEL_NOT_FOUND, name = model_name),
            )
            .with_code("CC-012")
        })?;

    let Element::Model { states, .. } = element else {
        return Err(Diagnostic::error(
            crate::generator::site::at(Location::Codegen),
            msg!(keys::CC_NOT_A_MODEL, name = model_name),
        )
        .with_code("CC-006"));
    };

    let state = states
        .iter()
        .find(|s| s.local() == eq_name)
        .ok_or_else(|| {
            Diagnostic::error(
                crate::generator::site::at(Location::Codegen),
                msg!(
                    keys::CC_STATE_NOT_IN_MODEL,
                    state = eq_name,
                    model = model_name
                ),
            )
            .with_code("CC-011")
        })?;

    let is_same_model = model_name.eq(&owner.name());
    let is_root_model = model.borrow().upper.is_none();
    let is_root_owner = owner.name().eq(&map.root_name());

    // Поле целевой модели лежит в структуре её родителя, поэтому база пути зависит от
    // родства с владельцем условия. Для модели-Сестры это давало
    // `model->entry.ping0.state` внутри `SrefPong_tick`: `cc` -> "no member named
    // 'entry' in 'struct SrefPong'". Дефект не проявлялся, потому что
    // единственный вход в эту ветку - `S(Модель) = Состояние`, а он до генератора не
    // доходил вовсе.
    let parent_is_owner = model
        .borrow()
        .upper
        .as_ref()
        .and_then(|w| w.upgrade())
        .map(|p| Name::from(p).eq(&owner.name()))
        .unwrap_or(false);

    let path = if is_same_model {
        // Своё же состояние.
        "model->state".to_string()
    } else if is_root_model && !is_root_owner {
        "main->state".to_string()
    } else if parent_is_owner {
        // Поле своей структуры - самый короткий путь.
        let field = field_name_in_parent(model).unwrap_or_else(|| {
            normalize_lowercase_snakecase(model.borrow().name.clone().unwrap_or_default())
        });
        format!("model->{}.state", field)
    } else {
        let chain = path_from_root(model).ok_or_else(|| {
            Diagnostic::error(
                crate::generator::site::at(Location::Codegen),
                msg!(
                    keys::CC_019_STATE_UNREACHABLE,
                    name = model_name,
                    owner = owner.name()
                ),
            )
            .with_code("CC-019")
        })?;
        format!("main->{}.state", chain)
    };

    let state_const = format!(
        "{}_{}",
        model_name.unique_uppercase_snakecase(),
        normalize_lowercase_snakecase(state.local().to_string()).to_uppercase()
    );

    Ok(format!("{} {} {}", path, op, state_const))
}

thread_local! {
    /// Параметры функции, в теле которой печатается условие.
    static FUNCTION_PARAMS: std::cell::RefCell<Vec<(String, crate::semantic::type_node::TypeNode)>> =
        const { std::cell::RefCell::new(Vec::new()) };
}

/// Объявляет параметры функции, чьё тело печатается сейчас.
pub(in crate::generator::c) fn enter_function_params(
    params: Vec<(String, crate::semantic::type_node::TypeNode)>,
) {
    FUNCTION_PARAMS.with(|p| *p.borrow_mut() = params);
}

/// Снимает параметры. Парен [`enter_function_params`].
pub(in crate::generator::c) fn leave_function_params() {
    FUNCTION_PARAMS.with(|p| p.borrow_mut().clear());
}

fn current_params() -> Vec<(String, crate::semantic::type_node::TypeNode)> {
    FUNCTION_PARAMS.with(|p| p.borrow().clone())
}

/// Преобразует [`ConditionNode`] в строку C-выражения.
///
/// Используется при генерации условий переходов для простых состояний. Возвращает
/// пустую строку для безусловных переходов (`ConditionNode::None`).
pub(in crate::generator::c) fn generate_condition_expr(
    cond: &ConditionNode,
    map: &CMap,
    owner: &Element,
) -> Result<String, Diagnostic> {
    match cond {
        // Безусловный переход - штатный случай: условия нет, печатать нечего.
        ConditionNode::None => Ok(String::new()),
        // Неразрешённое условие - Не отсутствие условия. Позиция берётся у самого узла:
        // сообщение без координаты в пачке диагностик бесполезно.
        ConditionNode::Unresolved(raw) => Err(crate::generator::c::c_unresolved::refuse(
            raw.loc(),
            crate::generator::c::c_unresolved::UnresolvedNode::Condition,
        )),
        ConditionNode::Bool(b) => Ok(if *b { "true" } else { "false" }.to_string()),
        // Анонимное обращение в условии: в условие доходит только битовая форма -
        // ширину в грамматике условий задать нечем.
        ConditionNode::AnonPort(access) => {
            if !map.hal() {
                return Err(crate::generator::c::c_anon::refuse_plain_c());
            }
            Ok(crate::generator::c::c_anon::read(access))
        }
        // Длительность: эмиссия - задача этой цели; до неё явный отказ, а не печать
        // наносекунд обычным числом. Выдержка `after`: сравнение по разности единиц
        // профиля. Счётчик `_dwell` обнуляется при входе в состояние и увеличивается в
        // конце такта, поэтому его значение равно числу тактов, прошедших с входа, -
        // ровно то, что меряет эталон модельным временем.
        ConditionNode::After(nanos) => after_condition(*nanos, map, owner),
        // Вычисляемая выдержка: значение известно лишь в такте, поэтому сравнивается не
        // число, а напечатанное выражение - в миллисекундах (представление `duration` в
        // целях).
        ConditionNode::AfterExpr(inner) => {
            let expr = generate_condition_expr(inner, map, owner)?;
            after_dynamic_condition(&expr, map, owner)
        }
        // Выдержка в тактах частоты не требует: счётчик и так считает такты.
        ConditionNode::AfterTicks(ticks) => Ok(format!("{} >= {}", dwell_access(), ticks)),
        // Литерал длительности вне `after` - сравнение со значением типа `duration`.
        // Печатается **миллисекундами**, как и значение; пересчёт зовёт общий слой.
        ConditionNode::Duration(nanos) => Ok(crate::semantic::duration::value_millis(
            *nanos,
            Location::Codegen,
            &msg!(keys::CC_WHAT_DURATION_IN_CONDITION),
        )?
        .to_string()),
        ConditionNode::Number(n) => Ok(crate::generator::c::c_literal::c_int_literal(*n)),
        ConditionNode::Rational(s, neg) => {
            if *neg {
                Ok(format!("-{}", s))
            } else {
                Ok(s.clone())
            }
        }
        ConditionNode::String(parts) => Ok(format!("\"{}\"", parts.join(""))),
        ConditionNode::Not(inner) => Ok(format!(
            "!({})",
            generate_condition_expr(inner, map, owner)?
        )),
        ConditionNode::Parenthesis(inner) => {
            Ok(format!("({})", generate_condition_expr(inner, map, owner)?))
        }
        ConditionNode::Add(l, r) => Ok(format!(
            "{} + {}",
            generate_condition_expr(l, map, owner)?,
            generate_condition_expr(r, map, owner)?
        )),
        ConditionNode::Subtract(l, r) => Ok(format!(
            "{} - {}",
            generate_condition_expr(l, map, owner)?,
            generate_condition_expr(r, map, owner)?
        )),
        ConditionNode::And(l, r) => Ok(format!(
            "{} && {}",
            generate_condition_expr(l, map, owner)?,
            generate_condition_expr(r, map, owner)?
        )),
        ConditionNode::Or(l, r) => Ok(format!(
            "{} || {}",
            generate_condition_expr(l, map, owner)?,
            generate_condition_expr(r, map, owner)?
        )),
        ConditionNode::Less(l, r) => compare(l, "<", r, map, owner),
        ConditionNode::More(l, r) => compare(l, ">", r, map, owner),
        ConditionNode::LessEqual(l, r) => compare(l, "<=", r, map, owner),
        ConditionNode::MoreEqual(l, r) => compare(l, ">=", r, map, owner),
        ConditionNode::Equal(l, r) => {
            if let Some(model) = state_of_model(l) {
                generate_state_comparison(model, r, "==", map, owner)
            } else {
                compare(l, "==", r, map, owner)
            }
        }
        ConditionNode::NotEqual(l, r) => {
            if let Some(model) = state_of_model(l) {
                generate_state_comparison(model, r, "!=", map, owner)
            } else {
                compare(l, "!=", r, map, owner)
            }
        }
        ConditionNode::Variable(var_rc, _) => {
            let var = var_rc.borrow();
            // Параметры функции, если печатается её тело: без них имя параметра
            // печаталось обращением к полю модели.
            let params = current_params();
            if let VariableNode::Simple { upper, .. } = &*var
                && let Some(s) =
                    resolve_simple_var_in_context(var.name(), upper, &params, owner, map, true)
            {
                return Ok(s);
            }
            resolve_variable_c_expr(&var, &params, map, owner, true)
        }
        ConditionNode::EnumVariant(_, _, value) => Ok(value.to_string()),
        // База - выражение: печатается тем же печатником условий.
        ConditionNode::ArraySubscript(base, idx) => {
            let idx_str = generate_condition_expr(idx, map, owner)?;
            // У порта индекс - часть обращения к HAL: условие `src[3]` печаталось
            // индексацией результата чтения, то есть C, не собирающимся ни одним
            // компилятором. Правило то же, что в выражениях, и потому спрашивается тот
            // Же носитель.
            if let ConditionNode::Variable(var_rc, _) = base.as_ref()
                && let VariableNode::Port {
                    direction,
                    name,
                    ty,
                    upper,
                    ..
                } = &*var_rc.borrow()
            {
                let Some(model_rc) = upper.as_ref().and_then(|w| w.upgrade()) else {
                    return Err(crate::generator::c::c_unresolved::refuse(
                        crate::generator::site::at(Location::Codegen),
                        crate::generator::c::c_unresolved::UnresolvedNode::PortOwner(
                            keys::CC_WHAT_ELEMENT_READ_IN_CONDITION,
                        ),
                    ));
                };
                let variant = crate::generator::c::c_names::port_enum_variant(
                    &Name::from(model_rc),
                    name,
                    *direction,
                    crate::parser::ast::PortDirection::In,
                );
                let ptr = if owner.name().eq(&map.root_name()) {
                    "model"
                } else {
                    "main"
                };
                return Ok(crate::generator::c::c_port_call::read(
                    PortClass::from_type(ty),
                    ptr,
                    &variant,
                    &idx_str,
                ));
            }
            let base_str = generate_condition_expr(base, map, owner)?;
            Ok(format!("{base_str}[{idx_str}]"))
        }
        ConditionNode::BitAccess(inner, member) => {
            match member {
                Member::Identifier(id) => {
                    // Доступ к полю структуры: inner.field
                    let inner_str = generate_condition_expr(inner, map, owner)?;
                    Ok(format!("{}.{}", inner_str, id.name))
                }
                Member::Number(n) => {
                    // Битовый доступ: проверяем, является ли inner портовой переменной
                    if let ConditionNode::Variable(var_rc, _) = inner.as_ref() {
                        let var = var_rc.borrow();
                        if let VariableNode::Port {
                            direction,
                            name,
                            ty,
                            upper,
                            ..
                        } = &*var
                        {
                            let model_name =
                                if let Some(rc) = upper.as_ref().and_then(|w| w.upgrade()) {
                                    Name::from(rc)
                                } else {
                                    return Err(crate::generator::c::c_unresolved::refuse(
                                    crate::diagnostics::Location::Codegen,
                                    crate::generator::c::c_unresolved::UnresolvedNode::PortOwner(
                                        keys::CC_WHAT_BIT_ACCESS_IN_CONDITION,
                                    ),
                                ));
                                };
                            let cls = PortClass::from_type(ty);
                            let variant = crate::generator::c::c_names::port_enum_variant(
                                &model_name,
                                name,
                                *direction,
                                crate::parser::ast::PortDirection::In,
                            );
                            // В условиях всегда has_model=true; ptr зависит от owner
                            let ptr = if owner.name().eq(&map.root_name()) {
                                "model"
                            } else {
                                "main"
                            };
                            return match cls {
                                // Разряд адресуется самим вызовом.
                                PortClass::Bit => Ok(crate::generator::c::c_port_call::read_bit(
                                    ptr,
                                    &variant,
                                    &n.to_string(),
                                )),
                                PortClass::Numeric => {
                                    let read = crate::generator::c::c_port_call::read_numeric(
                                        ptr,
                                        &variant,
                                        crate::generator::c::c_port_call::SCALAR_INDEX,
                                    );
                                    Ok(format!("(({read} >> {n}) & 1u)"))
                                }
                                PortClass::Rational => Err(Diagnostic::error(
                                    crate::generator::site::at(Location::Codegen),
                                    msg!(keys::CC_001_BIT_ACCESS_ON_FLOAT_PORT),
                                )
                                .with_code("CC-001")),
                            };
                        }
                    }
                    // Обычная переменная/выражение: ((inner >> N) & 1u)
                    let inner_str = generate_condition_expr(inner, map, owner)?;
                    Ok(format!("(({} >> {}) & 1u)", inner_str, n))
                }
            }
        }
        ConditionNode::Function(fun_rc, args, _) => {
            let fun = fun_rc.borrow();
            // Пропускаем неразрешённые и пустые функции - они не могут быть
            // сгенерированы
            if !matches!(
                *fun,
                FunctionDefinitionNode::Local { .. }
                    | FunctionDefinitionNode::External { .. }
                    | FunctionDefinitionNode::Builtin { .. }
            ) {
                return Err(Diagnostic::error(
                    crate::generator::site::at(Location::Codegen),
                    msg!(keys::CC_UNRESOLVED_FUNCTION_IN_CONDITION),
                )
                .with_code("CC-002"));
            }
            let fn_name = get_function_name(&fun);
            let args_strs: Result<Vec<_>, _> = args
                .iter()
                .map(|a| generate_condition_expr(a, map, owner))
                .collect();
            let args_strs = args_strs?;
            // Локальная функция принимает указатель на состояние первым аргументом, и
            // по нужде: тем же признаком, по которому он попал или не попал в сигнатуру.
            //
            // Признак спрашивается у общего носителя, а не считается здесь заново:
            // печатников вызова у цели два (выражения и условия), и передавай второй
            // указатель безусловно - функция без обращения к состоянию объявлялась бы
            // без него, а из условия ребра звалась бы с ним, и `cc` отвечал бы "too many
            // arguments" при нулевом коде возврата `taktc`.
            if let FunctionDefinitionNode::Local { upper, .. } = &*fun {
                let owner_rc = upper.as_ref().and_then(|w| w.upgrade());
                let wants_state = match &owner_rc {
                    Some(rc) => crate::generator::c::c_needs::needs_state(&fun, &rc.borrow())?,
                    // Владельца нет - судить не о чем; прежнее поведение.
                    None => true,
                };
                let mut all_args = Vec::new();
                if wants_state {
                    let first_arg = if owner.name().eq(&map.root_name()) {
                        "model"
                    } else {
                        "main"
                    };
                    all_args.push(first_arg.to_string());
                }
                all_args.extend(args_strs);
                Ok(format!("{}({})", fn_name, all_args.join(", ")))
            } else {
                Ok(format!("{}({})", fn_name, args_strs.join(", ")))
            }
        }
        ConditionNode::Model(_, _) | ConditionNode::State(_, _) => Err(Diagnostic::error(
            crate::generator::site::at(Location::Codegen),
            msg!(keys::CC_MODEL_OR_STATE_IN_CONDITION),
        )
        .with_code("CC-003")),
    }
}

/// Имя поля-счётчика времени, проведённого в текущем состоянии.
///
/// Имя начинается с `takt_`, чтобы не столкнуться с полем автора: имена Takt
/// нормализуются в snake_case без этого префикса.
pub(in crate::generator::c) const DWELL_FIELD: &str = "takt_dwell";

/// Имя поля-метки времени входа в состояние - профиль "часы".
///
/// Хранит `now_ms()` момента входа; выдержка сравнивается **разностью** `(uintN)(now -
/// t0) >= D_MS` (обёртка беззнакового нормирована ).
pub(in crate::generator::c) const ENTRY_MS_FIELD: &str = "takt_entry_ms";

/// Имя поля "состояние на конец предыдущего такта".
///
/// Нужно, чтобы вход в состояние определялся **одним** сравнением в конце такта, а не
/// десятью правками рядом с присваиваниями `model->state`.
pub(in crate::generator::c) const PREV_STATE_FIELD: &str = "takt_prev_state";

/// Печатает условие выдержки `after` в обоих профилях времени.
///
/// - "такты": счётчик `takt_dwell >= D_TICKS` (инкремент в конце такта).
/// - "часы": метка входа `takt_entry_ms` и внешний источник `now_ms`; сравнение
///   **разностью** `(uintN)(now - t0) >= D_MS` - беззнаковая обёртка нормирована
///   поэтому `t0 + D <= now` (переполнение) не даёт молча неверного
///   результата. Ширина `N` - общая с объявлением поля (см. `c_time`).
fn after_condition(nanos: i64, map: &CMap, owner: &Element) -> Result<String, Diagnostic> {
    let profile = map.time_profile();
    let units = crate::semantic::duration::units_or_diagnostic(
        nanos,
        profile,
        Location::Codegen,
        &msg!(keys::CC_WHAT_AFTER),
    )?;
    match profile {
        crate::semantic::duration::TimeProfile::Ticks { .. } => {
            Ok(format!("{} >= {}", dwell_access(), units))
        }
        crate::semantic::duration::TimeProfile::Clock => {
            let bits = crate::generator::c::c_time::clock_marker_bits(map)?;
            // HAL (`now_ms`/`userdata`) - на корневой структуре: `model` в корне,
            // `main` в под-модели (как порты). Метка `takt_entry_ms` - на self.
            let hal = if owner.name().eq(&map.root_name()) {
                "model"
            } else {
                "main"
            };
            let now = format!(
                "{hal}->{}({hal}->userdata)",
                crate::generator::c::FUNCTION_TIME_NOW_MS
            );
            Ok(format!(
                "(uint{bits}_t)((uint{bits}_t){now} - model->{ENTRY_MS_FIELD}) >= {units}"
            ))
        }
    }
}

/// Условие **вычисляемой** выдержки.
///
/// `expr` - уже напечатанное выражение в миллисекундах.
///
/// Профиль "часы" сравнивает миллисекунды напрямую; профиль "такты" переводит
/// миллисекунды в такты множителем `hertz / 1000`, который обязан быть целым.
fn after_dynamic_condition(expr: &str, map: &CMap, owner: &Element) -> Result<String, Diagnostic> {
    let profile = map.time_profile();
    match crate::semantic::duration::ticks_per_milli(profile, Location::Codegen)? {
        Some(1) => Ok(format!("{} >= {expr}", dwell_access())),
        Some(multiplier) => Ok(format!("{} >= ({expr}) * {multiplier}", dwell_access())),
        None => {
            let bits = crate::generator::c::c_time::clock_marker_bits(map)?;
            let hal = if owner.name().eq(&map.root_name()) {
                "model"
            } else {
                "main"
            };
            let now = format!(
                "{hal}->{}({hal}->userdata)",
                crate::generator::c::FUNCTION_TIME_NOW_MS
            );
            Ok(format!(
                "(uint{bits}_t)((uint{bits}_t){now} - model->{ENTRY_MS_FIELD}) >= ({expr})"
            ))
        }
    }
}

/// Доступ к счётчику времени состояния изнутри такта своей модели.
///
/// Условие ребра печатается при генерации такта **своей** модели, поэтому путь - всегда
/// `model->...` (как у `model->state` для собственного состояния).
pub(in crate::generator::c) fn dwell_access() -> String {
    format!("model->{DWELL_FIELD}")
}

/// Печатает сравнение, восстанавливая имя константы перечисления.
///
/// Сравнение перечислимой переменной с литералом - второе место, где **известен тип**
/// значения (первое - присваивание).
///
/// Сторона роли не играет: `c = Go` и `Go = c` дают одно и то же, и автор вправе
/// написать любую.
fn generate_comparison(
    left: &ConditionNode,
    right: &ConditionNode,
    op: &str,
    map: &CMap,
    owner: &Element,
) -> Result<String, Diagnostic> {
    let lhs = match enum_constant_for_comparison(right, left) {
        Some(name) => name,
        None => generate_condition_expr(left, map, owner)?,
    };
    let rhs = match enum_constant_for_comparison(left, right) {
        Some(name) => name,
        None => generate_condition_expr(right, map, owner)?,
    };
    Ok(format!("{lhs} {op} {rhs}"))
}

/// Имя константы для `value`, если тип задаёт `typed` - переменная перечислимого типа.
///
/// `None` - печатать обычным путём.
fn enum_constant_for_comparison(typed: &ConditionNode, value: &ConditionNode) -> Option<String> {
    let ConditionNode::Variable(var_rc, _) = typed else {
        return None;
    };
    let ConditionNode::Number(n) = value else {
        return None;
    };
    let var = var_rc.borrow();
    let (VariableNode::Simple { ty, upper, .. }
    | VariableNode::Const { ty, upper, .. }
    | VariableNode::Port { ty, upper, .. }) = &*var
    else {
        return None;
    };
    let scope = upper.as_ref().and_then(|w| w.upgrade())?;
    crate::generator::c::c_enum::constant_of(ty, *n, &scope)
}

/// Сравнение операндов разной знаковости.
///
/// На 8, 16 и 32 битах операнды продвигаются до `int`, и печать "как есть" верна. На 64
/// битах - нет: `int64_t < uint64_t` считается беззнаковым, `-1 < 200` даёт ложь, а
/// `cc -Wextra -Werror` отвечает `-Wsign-compare`.
///
/// Общего типа для `u64` и знакового в C нет, поэтому правило раскрывается проверкой
/// знака: отрицательное меньше любого беззнакового. Операнд печатается дважды - в
/// условии Takt эффектов не бывает.
fn compare(
    l: &ConditionNode,
    op: &str,
    r: &ConditionNode,
    map: &CMap,
    owner: &Element,
) -> Result<String, Diagnostic> {
    // Обычный случай печатает существующий печатник сравнений: он восстанавливает имя
    // константы перечисления, и своя печать это знание теряет - поймал чужой тест
    // `c_enum_constants_tests`.
    let plain = |map: &CMap, owner: &Element| generate_comparison(l, r, op, map, owner);
    match crate::generator::mixed_sign::plan(
        crate::generator::mixed_sign::operand_type_cond(l).as_ref(),
        crate::generator::mixed_sign::operand_type_cond(r).as_ref(),
    ) {
        // Продвижение до `int` уже делает своё дело; лишнее приведение изменило бы
        // вывод корпуса без нужды.
        crate::generator::mixed_sign::Plan::AsIs
        | crate::generator::mixed_sign::Plan::Widen { .. } => plain(map, owner),
        crate::generator::mixed_sign::Plan::SignGuard { signed_is_left } => {
            let lt = generate_condition_expr(l, map, owner)?;
            let rt = generate_condition_expr(r, map, owner)?;
            let (signed, unsigned) = if signed_is_left {
                (lt.as_str(), rt.as_str())
            } else {
                (rt.as_str(), lt.as_str())
            };
            let neg = format!("({signed} < 0)");
            let same = if signed_is_left {
                format!("((uint64_t)({signed}) {op} ({unsigned}))")
            } else {
                format!("(({unsigned}) {op} (uint64_t)({signed}))")
            };
            let negative_wins = crate::generator::mixed_sign::negative_wins(op, signed_is_left);
            Ok(if negative_wins {
                format!("({neg} || {same})")
            } else {
                format!("(!{neg} && {same})")
            })
        }
    }
}
