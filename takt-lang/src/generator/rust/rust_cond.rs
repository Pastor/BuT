//! Трансляция **условий** Takt в Rust (`print_condition` и спутники).
//!
//! Вынесено из `rust_expr.rs`: чистое перемещение, вывод байт-в-байт неизменен.
//! Печатник условий отделён от печатника выражений намеренно (: у `=` разная семантика -
//! равенство в условии, присваивание в выражении); здесь - только ветвь условий.

use crate::diagnostics::{Diagnostic, Location};
use crate::generator::rust::rust_expr::{
    Scope, bit_mask, call_arguments, member_index, rational, unsupported, variable,
};
use crate::generator::rust::rust_fixed::function_return;
use crate::generator::rust::rust_name::{rust_type_name, rust_value_name};
use crate::semantic::type_node::TypeNode;
use crate::semantic::{ConditionNode, ExpressionNode, FunctionDefinitionNode};

/// Транслирует условие Takt в выражение `bool` Rust.
///
/// # Ошибки
/// [`RS-011`] на непереводимой конструкции.
pub(crate) fn print_condition(cond: &ConditionNode, scope: &Scope) -> Result<String, Diagnostic> {
    match cond {
        // Литерал длительности вне `after` - сравнение со значением типа `duration`.
        // Печатается **миллисекундами**, как и значение.
        ConditionNode::Duration(nanos) => Ok(crate::semantic::duration::value_millis(
            *nanos,
            Location::Codegen,
            "литерал длительности в условии",
        )?
        .to_string()),
        // Выдержка `after`: профиль "такты" -> счётчик `takt_dwell`; профиль "часы" ->
        // метка `now_ms` и сравнение разностью с обёрткой.
        ConditionNode::After(nanos) => {
            let units = crate::semantic::duration::units_or_diagnostic(
                *nanos,
                scope.time_profile,
                Location::Codegen,
                "выдержка 'after'",
            )?;
            match scope.time_profile {
                crate::semantic::duration::TimeProfile::Ticks { .. } => {
                    Ok(crate::generator::rust::rust_time::dwell_after_expr(units))
                }
                crate::semantic::duration::TimeProfile::Clock => {
                    let hal = scope.hal_receiver("выдержка 'after'")?;
                    Ok(crate::generator::rust::rust_time::clock_after_expr(
                        hal, units,
                    ))
                }
            }
        }
        // Вычисляемая выдержка: сравнивается напечатанное выражение в миллисекундах, а
        // не число.
        ConditionNode::AfterExpr(inner) => {
            let expr = print_condition(inner, scope)?;
            match crate::semantic::duration::ticks_per_milli(scope.time_profile, Location::Codegen)?
            {
                Some(1) => Ok(crate::generator::rust::rust_time::dwell_after_dynamic(
                    &expr,
                )),
                Some(multiplier) => Ok(crate::generator::rust::rust_time::dwell_after_dynamic(
                    &format!("({expr}) * {multiplier}"),
                )),
                None => {
                    let hal = scope.hal_receiver("вычисляемая выдержка 'after'")?;
                    Ok(crate::generator::rust::rust_time::clock_after_dynamic(
                        hal, &expr,
                    ))
                }
            }
        }
        // Тактовая выдержка `after Nt`: частота не нужна - счётчик и так считает такты.
        ConditionNode::AfterTicks(ticks) => Ok(
            crate::generator::rust::rust_time::dwell_after_expr(*ticks as u64),
        ),
        ConditionNode::Number(n) => Ok(n.to_string()),
        ConditionNode::Rational(text, negative) => Ok(rational(text, *negative)),
        ConditionNode::Bool(b) => Ok(b.to_string()),
        ConditionNode::Variable(var, _) => variable(&var.borrow(), scope),
        ConditionNode::Parenthesis(inner) => print_condition(inner, scope),
        ConditionNode::Not(a) => Ok(format!("(!{})", condition_as_bool(a, scope)?)),

        // `=` в условии - равенство. Именно ради этого различия печатник условий
        // отделён от печатника выражений.
        //
        // Спецформа `S(Модель) = Состояние` перехватывается до общего случая: её
        // операнды - модель и имя состояния, а не значения, и обычным сравнением их не
        // напечатать.
        ConditionNode::Equal(a, b) => match state_comparison(a, b, "==", scope)? {
            Some(text) => Ok(text),
            None => match boolean_comparison(a, "==", b, scope)? {
                Some(text) => Ok(text),
                None => match variant_as_value(a, "==", b, scope)? {
                    Some(text) => Ok(text),
                    None => match enum_comparison(a, "==", b, scope)? {
                        Some(text) => Ok(text),
                        None => cond_binary(a, "==", b, scope),
                    },
                },
            },
        },
        ConditionNode::NotEqual(a, b) => match state_comparison(a, b, "!=", scope)? {
            Some(text) => Ok(text),
            None => match boolean_comparison(a, "!=", b, scope)? {
                Some(text) => Ok(text),
                None => match variant_as_value(a, "!=", b, scope)? {
                    Some(text) => Ok(text),
                    None => match enum_comparison(a, "!=", b, scope)? {
                        Some(text) => Ok(text),
                        None => cond_binary(a, "!=", b, scope),
                    },
                },
            },
        },
        ConditionNode::Less(a, b) => cond_compare(a, "<", b, scope),
        ConditionNode::More(a, b) => cond_compare(a, ">", b, scope),
        ConditionNode::LessEqual(a, b) => cond_compare(a, "<=", b, scope),
        ConditionNode::MoreEqual(a, b) => cond_compare(a, ">=", b, scope),
        ConditionNode::Add(a, b) => cond_binary(a, "+", b, scope),
        ConditionNode::Subtract(a, b) => cond_binary(a, "-", b, scope),

        // `&`/`|` в условии Takt - побитовые (как в C). На `bool` в Rust они определены
        // и дают `bool`, поэтому трансляция один в один законна и для булевых
        // операндов, и для целых.
        ConditionNode::And(a, b) => cond_bool_binary(a, "&", b, scope),
        ConditionNode::Or(a, b) => cond_bool_binary(a, "|", b, scope),

        ConditionNode::EnumVariant(def, variant, _) => {
            let enum_name = rust_type_name(&def.borrow().name, def.borrow().loc)?;
            Ok(format!(
                "{}::{}",
                enum_name,
                rust_type_name(variant, def.borrow().loc)?
            ))
        }

        // База - выражение: печатается тем же печатником условий.
        ConditionNode::ArraySubscript(base, index) => {
            Ok(crate::generator::rust::rust_expr::subscript(
                &print_condition(base, scope)?,
                &print_condition(index, scope)?,
                matches!(index.as_ref(), ConditionNode::Number(_)),
            ))
        }

        ConditionNode::BitAccess(inner, member) => {
            let base = print_condition(inner, scope)?;
            // Поле структуры - не разряд: печатается как есть.
            if let crate::parser::ast::Member::Identifier(name) = member {
                return Ok(crate::generator::rust::rust_bit::field_access(
                    &base, &name.name,
                ));
            }
            Ok(bit_mask(&base, member_index(member)?))
        }

        ConditionNode::Function(def, args, loc) => {
            let printed = args
                .iter()
                .map(|a| print_condition(a, scope))
                .collect::<Result<Vec<_>, _>>()?;
            let borrowed = def.borrow();
            match &*borrowed {
                FunctionDefinitionNode::Builtin(name, _, _) => Err(Diagnostic::error(
                    *loc,
                    format!(
                        "Встроенная функция '{}' в условии перехода не транслируется \
                         в Rust: поддержано только 'S(Модель) = Состояние'",
                        name
                    ),
                )
                .with_code("RS-011")),
                local @ FunctionDefinitionNode::Local { name, .. } => Ok(format!(
                    "{}({})",
                    rust_value_name(name, *loc)?,
                    call_arguments(local, &printed, scope)?.join(", ")
                )),
                FunctionDefinitionNode::External { name, .. } => Ok(format!(
                    "{}.{}({})",
                    scope.hal_receiver(&format!("вызов внешней функции '{}'", name))?,
                    rust_value_name(name, *loc)?,
                    printed.join(", ")
                )),
                FunctionDefinitionNode::None | FunctionDefinitionNode::Unresolved(_) => {
                    Err(unsupported("неразрешённая функция в условии"))
                }
            }
        }

        ConditionNode::None => Err(unsupported("пустое условие")),
        ConditionNode::Unresolved(_) => Err(unsupported("неразрешённое условие")),
        ConditionNode::String(_) => Err(unsupported("строковый литерал в условии")),
        ConditionNode::Model(_, _) => Err(unsupported(
            "модель в позиции условия вне формы 'S(Модель) = Состояние'",
        )),
        ConditionNode::State(..) => Err(unsupported(
            "состояние в позиции условия вне формы 'S(Модель) = Состояние'",
        )),
        // Анонимное обращение - см. оговорку у печатника выражений.
        ConditionNode::AnonPort(_) => Err(unsupported(
            "обращение к ячейке по адресу ('#0x…'): цель rust адресов не знает — \
             доступ по адресу дают цели 'c-hal', 'st-at' и 'sv-mmio'",
        )),
    }
}

/// Распознаёт спецформу `S(Модель) = Состояние` и печатает сравнение состояний.
///
/// Возвращает `None`, если условие к этой форме не относится - тогда печатается обычное
/// сравнение.
fn state_comparison(
    left: &ConditionNode,
    right: &ConditionNode,
    op: &str,
    scope: &Scope,
) -> Result<Option<String>, Diagnostic> {
    let Some(model) = model_of(left) else {
        return Ok(None);
    };
    let state_name = match right {
        ConditionNode::Variable(v, ..) => v.borrow().name().to_string(),
        // Неразрешённое имя - Штатный случай (см. заголовок функции).
        ConditionNode::Unresolved(crate::parser::ast::Condition::Variable(id)) => id.name.clone(),
        // Имя, случайно совпавшее с состоянием объемлющей модели: семантика разрешила
        // его в чужой области. Берём только имя.
        ConditionNode::State(state, _) => state.borrow().name().to_string(),
        _ => return Ok(None),
    };

    let unique = crate::semantic::minimap::Name::from(std::rc::Rc::clone(&model));
    let field = scope
        .instances
        .iter()
        .find(|(u, _)| u == unique.unique())
        .map(|(_, f)| f.clone())
        .ok_or_else(|| {
            // Причина названа поимённо: "экземпляр не найден" - верно, но автору
            // неясно, что делать. Наблюдать соседа цель `rust` не может по устройству:
            // под-модель получает `&mut self` и общую структуру `<Root>Shared`,
            // указателя на корень у неё нет (решение 0059). Цель `c` ту же запись
            // переводит через `main`.
            unsupported(&format!(
                "условие по состоянию модели '{}': наблюдать состояние СОСЕДНЕЙ \
                 модели цель 'rust' не умеет — под-модель получает только своё \
                 состояние и общие переменные корня, но не соседей. Проверяйте \
                 состояние из модели-родителя композиции (там экземпляр доступен) \
                 либо свяжите модели общей переменной корня; ту же запись \
                 переводят цели 'c' и 'sv'",
                unique.local()
            ))
        })?;

    // Поле `state` и перечисление состояний приватны, но лежат в этом же модуле -
    // обращение законно. Имя перечисления строится той же формулой, что и в
    // `rust_model::StateTable`.
    Ok(Some(format!(
        "(self.{}.state {} {}State::{})",
        field,
        op,
        unique.unique_camelcase(),
        rust_type_name(&state_name, Location::Codegen)?
    )))
}

/// Извлекает модель из левой части: `Модель` либо `S(Модель)`.
///
/// Разбор формы - общий на проект (`semantic::condition::state_of`); здесь лишь
/// клонирование `Rc` под сигнатуру вызывающего.
fn model_of(
    cond: &ConditionNode,
) -> Option<std::rc::Rc<std::cell::RefCell<crate::semantic::ModelNode>>> {
    crate::semantic::condition::state_of::state_of_model(cond).map(std::rc::Rc::clone)
}

/// Печатает бинарное условие. Скобки - см. [`binary`](super::rust_expr).
fn cond_binary(
    a: &ConditionNode,
    op: &str,
    b: &ConditionNode,
    scope: &Scope,
) -> Result<String, Diagnostic> {
    Ok(format!(
        "({} {} {})",
        print_condition(a, scope)?,
        op,
        print_condition(b, scope)?
    ))
}

/// Сравнение операндов разной знаковости.
///
/// Прежде печаталось как есть, и `self.s < self.u` при `i8`/`u8` давало
/// **`E0308`** при нулевом коде возврата `taktc`: в Rust смешанное сравнение не
/// компилируется вовсе. Операнды приводятся к типу, вмещающему оба; когда
/// такого типа нет (`u64` против знакового) - раскрытие проверкой знака.
fn cond_compare(
    a: &ConditionNode,
    op: &str,
    b: &ConditionNode,
    scope: &Scope,
) -> Result<String, Diagnostic> {
    // Порядковое сравнение с именем варианта (`op < Hlt`) - та же пара, что и
    // равенство: у переменной целый тип, у имени в Rust его нет.
    if let Some(text) = variant_as_value(a, op, b, scope)? {
        return Ok(text);
    }
    match crate::generator::mixed_sign::plan(
        crate::generator::mixed_sign::operand_type_cond(a).as_ref(),
        crate::generator::mixed_sign::operand_type_cond(b).as_ref(),
    ) {
        crate::generator::mixed_sign::Plan::AsIs => cond_binary(a, op, b, scope),
        crate::generator::mixed_sign::Plan::Widen { bits } => Ok(format!(
            "(({} as i{bits}) {op} ({} as i{bits}))",
            print_condition(a, scope)?,
            print_condition(b, scope)?
        )),
        crate::generator::mixed_sign::Plan::SignGuard { signed_is_left } => {
            let (lt, rt) = (print_condition(a, scope)?, print_condition(b, scope)?);
            let (signed, unsigned) = if signed_is_left {
                (lt.as_str(), rt.as_str())
            } else {
                (rt.as_str(), lt.as_str())
            };
            // Операнд печатается дважды - в условии Takt эффектов не бывает
            // (присваивание есть оператор, 0187).
            let neg = format!("({signed} < 0)");
            let same = if signed_is_left {
                format!("(({signed} as u64) {op} {unsigned})")
            } else {
                format!("({unsigned} {op} ({signed} as u64))")
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

/// Печатает сравнение булева операнда (`bit`/`bool`) с литералом -.
///
/// Возвращает `None`, если форма не эта: тогда печатается обычное сравнение.
fn boolean_comparison(
    a: &ConditionNode,
    op: &str,
    b: &ConditionNode,
    scope: &Scope,
) -> Result<Option<String>, Diagnostic> {
    /// Операнд булев по статическому типу?
    fn is_boolean(cond: &ConditionNode) -> bool {
        matches!(
            condition_type(cond),
            Some(TypeNode::Bool) | Some(TypeNode::Bit)
        )
    }
    /// Литерал, читаемый как булев: `true`/`false` и число (0 - ложь).
    fn literal(cond: &ConditionNode) -> Option<bool> {
        match cond {
            ConditionNode::Bool(v) => Some(*v),
            ConditionNode::Number(n) => Some(*n != 0),
            ConditionNode::Parenthesis(inner) => literal(inner),
            _ => None,
        }
    }

    // Литерал ищется с обеих сторон: `btn = 1` и `1 = btn` одинаково законны. Проверка
    // "операнд не литерал" обязательна, иначе `true = false` попало бы в первую ветвь и
    // потеряло бы вторую половину.
    let (operand, value) = match (literal(a), literal(b)) {
        (None, Some(v)) if is_boolean(a) => (a, v),
        (Some(v), None) if is_boolean(b) => (b, v),
        _ => return Ok(None),
    };

    let printed = print_condition(operand, scope)?;
    let positive = (op == "==") == value;
    Ok(Some(if positive {
        printed
    } else {
        format!("(!{})", printed)
    }))
}

/// Печатает сравнение целого операнда с именем варианта -.
///
/// Возвращает `None`, если пара иная: тогда печать идёт прежним путём.
fn variant_as_value(
    a: &ConditionNode,
    op: &str,
    b: &ConditionNode,
    scope: &Scope,
) -> Result<Option<String>, Diagnostic> {
    let Some(low) = crate::generator::enum_compare::lowered(a, b) else {
        return Ok(None);
    };
    let (left, right) = if low.variant_is_left {
        (low.value.to_string(), print_condition(b, scope)?)
    } else {
        (print_condition(a, scope)?, low.value.to_string())
    };
    Ok(Some(format!("({left} {op} {right})")))
}

/// Печатает сравнение перечислимого операнда с числом -.
///
/// Возвращает `None`, если форма не эта: тогда печатается обычное сравнение.
fn enum_comparison(
    a: &ConditionNode,
    op: &str,
    b: &ConditionNode,
    scope: &Scope,
) -> Result<Option<String>, Diagnostic> {
    /// Имя перечисления по статическому типу операнда.
    fn enum_name(cond: &ConditionNode) -> Option<String> {
        match condition_type(cond) {
            Some(TypeNode::Enum(name)) => Some(name),
            _ => None,
        }
    }
    /// Числовой литерал (в том числе в скобках).
    fn number(cond: &ConditionNode) -> Option<i128> {
        match cond {
            ConditionNode::Number(n) => Some(*n),
            ConditionNode::Parenthesis(inner) => number(inner),
            _ => None,
        }
    }

    let (operand, name, value) = match (number(a), number(b)) {
        (None, Some(v)) => match enum_name(a) {
            Some(name) => (a, name, v),
            None => return Ok(None),
        },
        (Some(v), None) => match enum_name(b) {
            Some(name) => (b, name, v),
            None => return Ok(None),
        },
        _ => return Ok(None),
    };

    let printed = print_condition(operand, scope)?;
    let variant = crate::generator::rust::rust_expr::enum_variant_literal(&name, value, scope)?;
    Ok(Some(format!("({printed} {op} {variant})")))
}

/// Печатает `&`/`|`, приводя операнды к `bool`.
///
/// Операнд-порт (`elevator_motor_sensor_u`) в Takt используется как условие напрямую; в
/// Rust `bool & bool` законно, но `u8 & bool` - нет.
fn cond_bool_binary(
    a: &ConditionNode,
    op: &str,
    b: &ConditionNode,
    scope: &Scope,
) -> Result<String, Diagnostic> {
    Ok(format!(
        "({} {} {})",
        condition_as_bool(a, scope)?,
        op,
        condition_as_bool(b, scope)?
    ))
}

/// Возвращает тип условия, если он выводится статически.
pub(crate) fn condition_type(cond: &ConditionNode) -> Option<TypeNode> {
    match cond {
        ConditionNode::Bool(_) => Some(TypeNode::Bool),
        ConditionNode::Number(_) => Some(TypeNode::Integer {
            bits: 32,
            signed: true,
        }),
        ConditionNode::Rational(_, _) => Some(TypeNode::Rational),
        ConditionNode::Variable(var, _) => Some(var.borrow().ty().clone()),
        ConditionNode::Parenthesis(inner) => condition_type(inner),
        ConditionNode::Equal(_, _)
        | ConditionNode::NotEqual(_, _)
        | ConditionNode::Less(_, _)
        | ConditionNode::More(_, _)
        | ConditionNode::LessEqual(_, _)
        | ConditionNode::MoreEqual(_, _)
        | ConditionNode::Not(_)
        | ConditionNode::And(_, _)
        | ConditionNode::Or(_, _)
        // Выдержка `after` - булево условие: `takt_dwell >= N` либо сравнение метки
        // `now_ms` разностью, оба дают `bool`.
        | ConditionNode::After(_)
        | ConditionNode::AfterTicks(_)
        // Вычисляемая выдержка - тоже сравнение, то есть `bool`. Без этой ветви цель
        // отвечала `RS-011` ("тип не выводится") на готовом булевом выражении: разбор
        // кончается `_ => None`, и компилятор о пропуске не сообщает.
        | ConditionNode::AfterExpr(_) => Some(TypeNode::Bool),
        // Разряд (`x.3`) логичен, а поле структуры (`v.a`) - Нет.
        //
        // Тот же класс закрыла у печатника выражений; печатников два, и правка одного
        // оставила второй.
        //
        // Тип поля здесь неизвестен - объявление структуры лежит в модели, а сюда она
        // не передаётся, - поэтому честный ответ `None`: приведение не применяется,
        // печать идёт обычным путём.
        ConditionNode::BitAccess(_, member) => match member {
            crate::parser::ast::Member::Number(_) => Some(TypeNode::Bool),
            crate::parser::ast::Member::Identifier(_) => None,
        },
        // База - выражение: тип берётся у неё рекурсивно.
        ConditionNode::ArraySubscript(base, _) => match condition_type(base) {
            Some(TypeNode::Array(_, elem)) => Some((*elem).clone()),
            _ => None,
        },
        ConditionNode::Function(def, _, _) => function_return(&def.borrow()),
        // Вариант перечисления имеет тип своего перечисления: нужен, чтобы `command =
        // Up` сравнивалось, а не приводилось к bool.
        ConditionNode::EnumVariant(def, _, _) => Some(TypeNode::Enum(def.borrow().name.clone())),
        _ => None,
    }
}

/// Печатает условие, приводя его к `bool`.
///
/// Точка входа для guard'ов рёбер: `ref Next: x;` при `x : u8` в C означает `if (x)`, в
/// Rust - `if x != 0`.
pub(crate) fn condition_as_bool(cond: &ConditionNode, scope: &Scope) -> Result<String, Diagnostic> {
    let printed = print_condition(cond, scope)?;
    match condition_type(cond) {
        Some(TypeNode::Bool) | Some(TypeNode::Bit) => Ok(printed),
        Some(TypeNode::Rational) => Ok(format!("({} != 0.0)", printed)),
        Some(TypeNode::Integer { .. }) => Ok(format!("({} != 0)", printed)),
        _ => Err(crate::generator::rust::rust_expr::unsupported(&format!(
            "условие '{}': тип не выводится, приведение к bool построить нельзя",
            printed
        ))),
    }
}

/// числом.
pub(crate) fn print_as_bool(expr: &ExpressionNode, scope: &Scope) -> Result<String, Diagnostic> {
    let printed = crate::generator::rust::rust_expr::print_expression(expr, scope)?;
    match crate::generator::rust::rust_fixed::expression_type(expr) {
        Some(TypeNode::Bool) | Some(TypeNode::Bit) => Ok(printed),
        Some(TypeNode::Rational) => Ok(format!("({} != 0.0)", printed)),
        Some(TypeNode::Integer { .. }) => Ok(format!("({} != 0)", printed)),
        // Тип не выведен - угадывать нельзя. Молчаливое `!= 0` при `bool` дало бы
        // ошибку сборки в порождённом коде, то есть у пользователя, а не здесь.
        _ => Err(unsupported(&format!(
            "условие '{}': тип не выводится, приведение к bool построить нельзя",
            printed
        ))),
    }
}
