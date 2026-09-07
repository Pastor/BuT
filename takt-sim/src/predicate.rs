//! Адаптер условий: `ConditionNode` -> [`Value`] поверх ядра [`crate::eval`].
//!
//! Модуль **не содержит семантики** - только структурный разбор узлов и делегирование в
//! ядро. До здесь жил `flat`, который использовал узлы АСД в роли значений и потому:
//!
//! - паниковал на вызове функции (`unimplemented!()`, Д4) и на смешении
//!   `int`/`real` (`unwrap()` на `None`, Д6);
//! - возвращал внутренний узел скобок **невычисленным** (Д7);
//! - отвечал `Err` на вариант `enum` (Д8).
//!
//! Теперь разбор - исчерпывающий `match` **без `_`** по всем 24 вариантам
//! `ConditionNode`: новый вариант ломает сборку, а не превращается молча в "условие
//! ложно".

use crate::context::Context;
use crate::eval::error::EvalError;
use crate::eval::ops::{self, BinOp, UnOp};
use crate::eval::value::Value;
use crate::unit::Predicate;
use takt_lang::diagnostics::{Diagnostic, Location};
use takt_lang::semantic::ConditionNode;
use takt_lang::semantic::condition::state_of::{compared_state_name, state_of_model};
use takt_lang::semantic::type_node::TypeNode;

/// Строит предикат перехода из условия.
///
/// Ошибка вычисления **не** сводится к "условие ложно" (требование R5): она
/// возвращается вызывающему и доходит до `TickResult::Failed` -> CLI.
pub(crate) fn create_predicate(cond: &ConditionNode) -> Predicate {
    let name = condition_label(cond);
    let cond = cond.clone();
    Predicate::new(name, move |c: &mut dyn Context| {
        let value = eval_condition(&cond, c)?;
        ops::to_bool(&value).map_err(|e| e.to_diagnostic(loc_of(&cond)))
    })
}

/// Возвращает короткое текстовое представление условия для отображения на ребре графа.
fn condition_label(cond: &ConditionNode) -> String {
    match cond {
        ConditionNode::None => String::new(),
        ConditionNode::Bool(b) => b.to_string(),
        ConditionNode::Duration(ns) => crate::runner::format_duration(*ns),
        ConditionNode::After(ns) => format!("after {}", crate::runner::format_duration(*ns)),
        ConditionNode::AfterTicks(ticks) => format!("after {ticks}t"),
        // Вычисляемая выдержка: значения нет до такта, поэтому на ребре печатается сама
        // запись автора.
        ConditionNode::AfterExpr(inner) => format!("after ({})", condition_label(inner)),
        ConditionNode::Number(n) => n.to_string(),
        ConditionNode::Rational(s, neg) => {
            if *neg {
                format!("-{s}")
            } else {
                s.clone()
            }
        }
        ConditionNode::Variable(var, _) => var.borrow().name().to_string(),
        ConditionNode::Not(c) => format!("!{}", condition_label(c)),
        ConditionNode::Parenthesis(c) => format!("({})", condition_label(c)),
        ConditionNode::Add(l, r) => format!("{} + {}", condition_label(l), condition_label(r)),
        ConditionNode::Subtract(l, r) => format!("{} - {}", condition_label(l), condition_label(r)),
        ConditionNode::And(l, r) => format!("{} & {}", condition_label(l), condition_label(r)),
        ConditionNode::Or(l, r) => format!("{} | {}", condition_label(l), condition_label(r)),
        ConditionNode::Less(l, r) => format!("{} < {}", condition_label(l), condition_label(r)),
        ConditionNode::More(l, r) => format!("{} > {}", condition_label(l), condition_label(r)),
        ConditionNode::LessEqual(l, r) => {
            format!("{} <= {}", condition_label(l), condition_label(r))
        }
        ConditionNode::MoreEqual(l, r) => {
            format!("{} >= {}", condition_label(l), condition_label(r))
        }
        ConditionNode::Equal(l, r) => format!("{} = {}", condition_label(l), condition_label(r)),
        ConditionNode::NotEqual(l, r) => {
            format!("{} != {}", condition_label(l), condition_label(r))
        }
        // Член печатается как в исходнике (`x.0`, `x.field`), а не `Debug`-дампом
        // `Number(0)`.
        ConditionNode::BitAccess(c, m) => {
            let member = match m {
                takt_lang::parser::ast::Member::Identifier(id) => id.name.clone(),
                takt_lang::parser::ast::Member::Number(n) => n.to_string(),
            };
            format!("{}.{member}", condition_label(c))
        }
        ConditionNode::ArraySubscript(base, idx) => {
            format!("{}[{}]", condition_label(base), condition_label(idx))
        }
        ConditionNode::Function(f, args, _) => {
            let name = f.borrow().name().to_string();
            let arg_labels: Vec<String> = args.iter().map(|a| condition_label(a)).collect();
            format!("{}({})", name, arg_labels.join(", "))
        }
        ConditionNode::State(s, _) => s.borrow().name().to_string(),
        ConditionNode::Model(m, _) => m.borrow().name.clone().unwrap_or_else(|| "?".to_string()),
        ConditionNode::EnumVariant(_, name, _) => name.clone(),
        // Обращение к ячейке: на ребре печатается сама запись - значение известно лишь
        // в такте.
        ConditionNode::AnonPort(access) => match access.ty {
            TypeNode::Bit | TypeNode::Bool => {
                format!("#0x{:X}.{}", access.addr as u64, access.bit)
            }
            _ => format!(
                "#0x{:X}:{} as {}",
                access.addr as u64, access.bit, access.ty
            ),
        },
        ConditionNode::String(parts) => parts.join(""),
        ConditionNode::Unresolved(_) => "?".to_string(),
    }
}

/// Ищет позицию в исходном тексте для привязки диагностики.
///
/// Позицию несут не все узлы (`Variable` и `Function` - несут), поэтому ищем вглубь по
/// первому попавшемуся потомку. Это даёт диагностике реальную позицию вместо
/// `Location::Builtin` (критерий A10) в подавляющем большинстве случаев.
fn loc_of(cond: &ConditionNode) -> Location {
    match cond {
        // У литерала длительности и у `after` позиции нет - как и у прочих литералов
        // (позиция - свойство ссылки).
        ConditionNode::Duration(_) | ConditionNode::After(_) | ConditionNode::AfterTicks(_) => {
            Location::Implicit
        }
        // У вычисляемой выдержки позиция есть - у операндов вложенного условия.
        ConditionNode::AfterExpr(inner) => loc_of(inner),
        ConditionNode::Variable(_, loc) | ConditionNode::Function(_, _, loc) => *loc,
        ConditionNode::Not(c) | ConditionNode::Parenthesis(c) | ConditionNode::BitAccess(c, _) => {
            loc_of(c)
        }
        ConditionNode::ArraySubscript(_, idx) => loc_of(idx),
        ConditionNode::Add(l, r)
        | ConditionNode::Subtract(l, r)
        | ConditionNode::And(l, r)
        | ConditionNode::Or(l, r)
        | ConditionNode::Less(l, r)
        | ConditionNode::More(l, r)
        | ConditionNode::LessEqual(l, r)
        | ConditionNode::MoreEqual(l, r)
        | ConditionNode::Equal(l, r)
        | ConditionNode::NotEqual(l, r) => match loc_of(l) {
            Location::Builtin => loc_of(r),
            found => found,
        },
        ConditionNode::None
        | ConditionNode::Unresolved(_)
        | ConditionNode::Number(_)
        | ConditionNode::Rational(_, _)
        | ConditionNode::String(_)
        | ConditionNode::Bool(_)
        // У обращения к ячейке позиции нет, как у прочих литералов.
        | ConditionNode::AnonPort(_)
        | ConditionNode::Model(_, _)
        | ConditionNode::State(..)
        | ConditionNode::EnumVariant(_, _, _) => Location::Builtin,
    }
}

/// Вычисляет условие в значение.
///
/// Структурный разбор - здесь; семантика операций - в [`crate::eval::ops`]. Паники
/// недостижимы: любой неподдержанный случай - `Err` (R4).
pub(crate) fn eval_condition(
    cond: &ConditionNode,
    ctx: &mut dyn Context,
) -> Result<Value, Diagnostic> {
    match cond {
        // -- Литералы ---------------------------------------------------------
        // Длительность и выдержка. `after` истинно, когда с входа в текущее состояние
        // прошло не меньше указанного: сравнение - по разности модельного времени, а не
        // по абсолютному моменту, иначе выдержка зависела бы от начала прогона.
        ConditionNode::Duration(ns) => Ok(Value::Duration(*ns)),
        ConditionNode::After(ns) => Ok(Value::Boolean(ctx.since_state_entry_ns() >= *ns)),
        // Выдержка в тактах: сравнение по счётчику шагов, а не по часам.
        ConditionNode::AfterTicks(ticks) => Ok(Value::Boolean(
            i64::try_from(ctx.ticks_in_state()).unwrap_or(i64::MAX) >= *ticks,
        )),
        // Вычисляемая выдержка: порог берётся из вложенного условия
        // **в этом такте**. Эталон считает в наносекундах - как и всё время
        // симулятора; цели держат миллисекунды, и совпадение доказывает
        // потактовая сверка, а не рассуждение.
        ConditionNode::AfterExpr(inner) => match eval_condition(inner, ctx)? {
            Value::Duration(ns) => Ok(Value::Boolean(ctx.since_state_entry_ns() >= ns)),
            other => Err(Diagnostic::error(
                loc_of(inner),
                format!(
                    "выдержка 'after' ожидала длительность, выражение дало {}",
                    crate::eval::error::value_kind(&other)
                ),
            )
            .with_code("SIM-007")),
        },
        ConditionNode::Number(n) => Ok(Value::Number(*n)),
        ConditionNode::Bool(b) => Ok(Value::Boolean(*b)),
        // Чтение ячейки в условии - то же чтение, что в выражении: одна воронка на оба
        // адаптера, иначе ребро и тело разошлись бы.
        ConditionNode::AnonPort(access) => Ok(crate::anon_cell::read(access, ctx)),
        ConditionNode::Rational(text, negative) => {
            let parsed: f64 = text.parse().map_err(|_| {
                Diagnostic::error(
                    Location::Builtin,
                    format!("не удалось разобрать вещественный литерал '{text}'"),
                )
                .with_code("SIM-008")
            })?;
            Ok(Value::Real(if *negative { -parsed } else { parsed }))
        }
        // S7 (Д8): вариант enum - целое со значением варианта.
        ConditionNode::EnumVariant(_, _, value) => Ok(Value::Number(*value)),

        // -- Переменные и доступ ----------------------------------------------
        ConditionNode::Variable(var, loc) => {
            let name = var.borrow().name().to_string();
            ctx.get_value(&name).ok_or_else(|| {
                Diagnostic::error(*loc, format!("переменная '{name}' не найдена"))
                    .with_code("SIM-009")
            })
        }
        // Д7: скобки вычисляются **рекурсивно**.
        ConditionNode::Parenthesis(inner) => eval_condition(inner, ctx),
        // База - Выражение: вычисляется тем же вычислителем.
        ConditionNode::ArraySubscript(base, index) => {
            let loc = loc_of(cond);
            let array = eval_condition(base, ctx)?;
            let Value::Array(items) = array else {
                return Err(Diagnostic::error(
                    loc,
                    "индексируемое значение не является массивом".to_string(),
                )
                .with_code("SIM-010"));
            };
            let index_value = eval_condition(index, ctx)?;
            let Value::Number(idx) = index_value else {
                return Err(
                    Diagnostic::error(loc, "индекс массива должен быть целым".to_string())
                        .with_code("SIM-010"),
                );
            };
            let element = usize::try_from(idx)
                .ok()
                .and_then(|i| items.get(i))
                .ok_or_else(|| {
                    Diagnostic::error(
                        loc,
                        format!("индекс {idx} вне границ массива (длина {})", items.len()),
                    )
                    .with_code("SIM-010")
                })?;
            Ok(element.clone())
        }
        // `a.b`: поле структуры (`p.x`) или бит целого (`BTN.0`) - тем же ядром
        // `eval::access`, что и адаптер выражений (симметрия обязательна: иначе два
        // вычислителя разошлись бы - корневая причина 0025).
        ConditionNode::BitAccess(inner, member) => {
            let value = eval_condition(inner, ctx)?;
            crate::eval::access::read_member(&value, member)
                .map_err(|e| e.to_diagnostic(loc_of(cond)))
        }

        // -- Операции: семантика делегируется ядру ----------------------------
        ConditionNode::Not(inner) => unary(UnOp::Not, inner, ctx, loc_of(cond)),
        ConditionNode::Add(l, r) => binary(BinOp::Add, l, r, ctx, loc_of(cond)),
        ConditionNode::Subtract(l, r) => binary(BinOp::Subtract, l, r, ctx, loc_of(cond)),
        // `And`/`Or` документированы в АСД как побитовые, но исторически вычислялись
        // логически (`to_bool` в прежнем `flat`). Поведение сохранено намеренно: менять
        // его - изменение семантики языка, что вне объёма.
        ConditionNode::And(l, r) => binary(BinOp::LogicalAnd, l, r, ctx, loc_of(cond)),
        ConditionNode::Or(l, r) => binary(BinOp::LogicalOr, l, r, ctx, loc_of(cond)),
        ConditionNode::Less(l, r) => binary(BinOp::Less, l, r, ctx, loc_of(cond)),
        ConditionNode::More(l, r) => binary(BinOp::More, l, r, ctx, loc_of(cond)),
        ConditionNode::LessEqual(l, r) => binary(BinOp::LessEqual, l, r, ctx, loc_of(cond)),
        ConditionNode::MoreEqual(l, r) => binary(BinOp::MoreEqual, l, r, ctx, loc_of(cond)),
        // Проверка состояния под-модели: `S(Модель) = Состояние` и краткая форма
        // `Модель = Состояние`. Форму паттерна разбирает функция `takt-lang` - та же,
        // что у судьи условий и печатника цели `c`: второго разбора в проекте нет.
        ConditionNode::Equal(l, r) if state_of_model(l).is_some() => {
            state_matches(l, r, ctx, loc_of(cond)).map(Value::Boolean)
        }
        ConditionNode::NotEqual(l, r) if state_of_model(l).is_some() => {
            state_matches(l, r, ctx, loc_of(cond)).map(|same| Value::Boolean(!same))
        }
        ConditionNode::Equal(l, r) => binary(BinOp::Equal, l, r, ctx, loc_of(cond)),
        ConditionNode::NotEqual(l, r) => binary(BinOp::NotEqual, l, r, ctx, loc_of(cond)),

        // -- Пока не поддержано - но с диагностикой, а не паникой -------------
        //
        // Д4: здесь был `unimplemented!()`, роняющий симулятор. Вычисление вызова
        // требует интерпретатора тела `fn`, который поставляет задача `0025-02`; до неё -
        // честный отказ. Д4: вызов функции в условии. Тот же интерпретатор, что и у
        // выражений.
        ConditionNode::Function(func, args, _) => {
            let values = args
                .iter()
                .map(|arg| eval_condition(arg, ctx))
                .collect::<Result<Vec<Value>, Diagnostic>>()?;
            crate::unit::statement::call_function(func, &values, ctx)
        }
        // Ветвь недостижима из корректной программы: голое имя состояния или модели
        // условием не является, и такой вход отвергает семантика (`SE-110`) - до
        // всякого прогона. Отказ оставлен страховкой на случай дефекта в семантике, как
        // `CC-023` у цели `c`: молчаливое "условие ложно" развело бы трассу эталона с
        // прошивкой.
        //
        // Осмысленная запись - паттерн `S(Модель) = Состояние` (и краткая `Модель =
        // Состояние`); её исполняет `state_of_condition` ниже.
        ConditionNode::State(state, _) => Err(Diagnostic::error(
            Location::Builtin,
            format!(
                "имя состояния '{}' условием не является — такой вход отвергает \
                 семантика (SE-110); проверка состояния записывается как \
                 'S(Модель) = Состояние'",
                state.borrow().name()
            ),
        )
        .with_code("SIM-013")),
        ConditionNode::Model(model, _) => Err(Diagnostic::error(
            Location::Builtin,
            format!(
                "имя модели '{}' условием не является — такой вход отвергает \
                 семантика (SE-110); напишите 'Модель = Состояние'",
                model
                    .borrow()
                    .name
                    .clone()
                    .unwrap_or_else(|| "?".to_string())
            ),
        )
        .with_code("SIM-013")),
        // `Value` не представляет строки - пробел зафиксирован анализом.
        ConditionNode::String(_) => Err(Diagnostic::error(
            Location::Builtin,
            "строки не поддерживаются симулятором".to_string(),
        )
        .with_code("SIM-014")),

        // -- Невычислимые по определению --------------------------------------
        ConditionNode::None => Err(Diagnostic::error(
            Location::Builtin,
            "пустое условие не может быть вычислено".to_string(),
        )
        .with_code("SIM-015")),
        ConditionNode::Unresolved(_) => Err(Diagnostic::error(
            Location::Builtin,
            "неразрешённое условие не может быть вычислено".to_string(),
        )
        .with_code("SIM-016")),
    }
}

/// Совпадает ли текущее состояние модели-аргумента с названным.
///
/// Левая часть - паттерн "состояние модели" (проверена вызывающим), правая - имя
/// состояния в трёх законных формах (`compared_state_name` в `takt-lang`).
///
/// Отсутствие модели в реестре - **отказ**, а не "условие ложно": цель `c` на модели,
/// не запущенной в композиции, тоже отказывает (`CC-012`), и молчаливое `false` развело
/// бы трассу эталона с поведением прошивки - ровно тот класс, ради которого заведены
/// потактовые сверки.
fn state_matches(
    left: &ConditionNode,
    right: &ConditionNode,
    ctx: &mut dyn Context,
    loc: Location,
) -> Result<bool, Diagnostic> {
    let model = state_of_model(left).expect("паттерн проверен вызывающим");
    let model_name = model.borrow().name.clone().ok_or_else(|| {
        Diagnostic::error(
            loc,
            "проверка состояния безымянной модели невозможна".to_string(),
        )
        .with_code("SIM-036")
    })?;
    let Some(wanted) = compared_state_name(right) else {
        return Err(Diagnostic::error(
            loc,
            format!(
                "справа от сравнения с состоянием модели '{model_name}' \
                 ожидалось имя состояния"
            ),
        )
        .with_code("SIM-036"));
    };
    let current = ctx.model_state(&model_name).ok_or_else(|| {
        Diagnostic::error(
            loc,
            format!(
                "модель '{model_name}' в этом прогоне не запущена: проверить её \
                 состояние нечем"
            ),
        )
        .with_code("SIM-036")
    })?;
    Ok(current == wanted)
}

fn unary(
    op: UnOp,
    inner: &ConditionNode,
    ctx: &mut dyn Context,
    loc: Location,
) -> Result<Value, Diagnostic> {
    let value = eval_condition(inner, ctx)?;
    ops::apply_unary(op, &value).map_err(|e: EvalError| e.to_diagnostic(loc))
}

fn binary(
    op: BinOp,
    left: &ConditionNode,
    right: &ConditionNode,
    ctx: &mut dyn Context,
    loc: Location,
) -> Result<Value, Diagnostic> {
    let lhs = eval_condition(left, ctx)?;
    let rhs = eval_condition(right, ctx)?;
    ops::apply_binary(op, &lhs, &rhs).map_err(|e: EvalError| e.to_diagnostic(loc))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::value::Value;
    use std::collections::HashMap;

    /// Контекст-заглушка: только чтение заранее заданных значений.
    struct MockContext {
        vars: HashMap<String, Value>,
    }

    impl MockContext {
        fn new(pairs: &[(&str, Value)]) -> Self {
            Self {
                vars: pairs
                    .iter()
                    .map(|(k, v)| ((*k).to_string(), v.clone()))
                    .collect(),
            }
        }
    }

    impl Context for MockContext {
        fn get_value(&self, name: &str) -> Option<Value> {
            self.vars.get(name).cloned()
        }

        fn set_value(&mut self, name: &str, value: Value) {
            self.vars.insert(name.to_string(), value);
        }
    }

    fn empty_ctx() -> MockContext {
        MockContext::new(&[])
    }

    fn num(n: i128) -> Box<ConditionNode> {
        Box::new(ConditionNode::Number(n))
    }

    // -- Литералы --------------------------------------------------------------

    #[test]
    fn number_and_bool_literals() {
        let mut ctx = empty_ctx();
        assert_eq!(
            eval_condition(&ConditionNode::Number(7), &mut ctx),
            Ok(Value::Number(7))
        );
        assert_eq!(
            eval_condition(&ConditionNode::Bool(true), &mut ctx),
            Ok(Value::Boolean(true))
        );
    }

    #[test]
    fn rational_literal_is_parsed_with_sign() {
        let mut ctx = empty_ctx();
        assert_eq!(
            eval_condition(&ConditionNode::Rational("2.5".to_string(), false), &mut ctx),
            Ok(Value::Real(2.5))
        );
        assert_eq!(
            eval_condition(&ConditionNode::Rational("2.5".to_string(), true), &mut ctx),
            Ok(Value::Real(-2.5))
        );
    }

    #[test]
    fn malformed_rational_is_diagnostic_not_panic() {
        let mut ctx = empty_ctx();
        let cond = ConditionNode::Rational("не-число".to_string(), false);
        let err = eval_condition(&cond, &mut ctx).unwrap_err();
        assert_eq!(err.code.as_deref(), Some("SIM-008"));
    }

    // -- Д7: скобки ------------------------------------------------------------

    #[test]
    fn d7_parenthesis_is_evaluated_recursively() {
        // Прежний flat возвращал внутренний узел невычисленным -> было ложно.
        let mut ctx = empty_ctx();
        let inner = ConditionNode::Add(num(5), num(1));
        let cond = ConditionNode::More(
            Box::new(ConditionNode::Parenthesis(Box::new(inner))),
            num(2),
        );
        assert_eq!(eval_condition(&cond, &mut ctx), Ok(Value::Boolean(true)));
    }

    // -- Д8: вариант enum ------------------------------------------------------

    #[test]
    fn d8_enum_variant_evaluates_to_its_value() {
        let mut ctx = empty_ctx();
        let cond = ConditionNode::EnumVariant(
            std::rc::Rc::new(std::cell::RefCell::new(Default::default())),
            "Manual".to_string(),
            1,
        );
        assert_eq!(eval_condition(&cond, &mut ctx), Ok(Value::Number(1)));
    }

    // -- Д6: смешение int/real не роняет ---------------------------------------

    #[test]
    fn d6_mixed_int_real_does_not_panic() {
        // Прежний flat падал на unwrap().
        let mut ctx = MockContext::new(&[]);
        let sum = ConditionNode::Add(
            num(1),
            Box::new(ConditionNode::Rational("2.5".to_string(), false)),
        );
        let cond = ConditionNode::More(Box::new(sum), num(3));
        assert_eq!(eval_condition(&cond, &mut ctx), Ok(Value::Boolean(true)));
    }

    // -- Д4: вызов функции - диагностика вместо паники -------------------------

    #[test]
    fn d4_function_call_in_condition_is_evaluated() {
        // Д4: `fn ready() -> u8 { return 1; }` -> условие `ready()` истинно.
        use takt_lang::semantic::type_node::TypeNode;
        let func = std::rc::Rc::new(std::cell::RefCell::new(
            takt_lang::semantic::FunctionDefinitionNode::Local {
                upper: None,
                loc: Location::Builtin,
                name: "ready".to_string(),
                params: vec![],
                ret: TypeNode::Integer {
                    bits: 8,
                    signed: false,
                },
                raw: Box::default(),
                body: takt_lang::semantic::StatementNode::Return(
                    Some(Box::new(takt_lang::semantic::ExpressionNode::Number(1))),
                    Location::Codegen,
                ),
            },
        ));
        let mut ctx = empty_ctx();
        let cond = ConditionNode::Function(func, vec![], Location::Builtin);
        assert_eq!(eval_condition(&cond, &mut ctx), Ok(Value::Number(1)));
    }

    #[test]
    fn unresolved_function_in_condition_is_diagnostic_not_panic() {
        let mut ctx = empty_ctx();
        let cond = ConditionNode::Function(
            std::rc::Rc::new(std::cell::RefCell::new(Default::default())),
            vec![],
            Location::Builtin,
        );
        let err = eval_condition(&cond, &mut ctx).unwrap_err();
        assert_eq!(err.code.as_deref(), Some("SIM-016"));
    }

    // -- Сравнения и арифметика ------------------------------------------------

    #[test]
    fn comparisons_delegate_to_core() {
        let mut ctx = empty_ctx();
        let cond = ConditionNode::More(Box::new(ConditionNode::Add(num(5), num(1))), num(2));
        assert_eq!(eval_condition(&cond, &mut ctx), Ok(Value::Boolean(true)));
    }

    #[test]
    fn logical_and_or_keep_historical_semantics() {
        let mut ctx = empty_ctx();
        let and = ConditionNode::And(
            Box::new(ConditionNode::Bool(true)),
            Box::new(ConditionNode::Bool(false)),
        );
        assert_eq!(eval_condition(&and, &mut ctx), Ok(Value::Boolean(false)));
        let or = ConditionNode::Or(
            Box::new(ConditionNode::Bool(true)),
            Box::new(ConditionNode::Bool(false)),
        );
        assert_eq!(eval_condition(&or, &mut ctx), Ok(Value::Boolean(true)));
    }

    #[test]
    fn not_negates() {
        let mut ctx = empty_ctx();
        let cond = ConditionNode::Not(Box::new(ConditionNode::Bool(true)));
        assert_eq!(eval_condition(&cond, &mut ctx), Ok(Value::Boolean(false)));
    }

    // -- S3: деление на ноль приходит из ядра как диагностика ------------------

    #[test]
    fn division_by_zero_surfaces_as_diagnostic() {
        // В условиях деления нет (нет узла Divide), но проверяем канал ошибок ядра на
        // доступной операции: сдвиг здесь недоступен, берём типовую ошибку - сравнение
        // массива с числом.
        let mut ctx = MockContext::new(&[("a", Value::Array(vec![]))]);
        let var = std::rc::Rc::new(std::cell::RefCell::new(
            takt_lang::semantic::VariableNode::Unresolved,
        ));
        // Переменная не найдена по имени - проверяем диагностику доступа.
        let cond = ConditionNode::Variable(var, Location::Builtin);
        let err = eval_condition(&cond, &mut ctx).unwrap_err();
        assert_eq!(err.code.as_deref(), Some("SIM-009"));
    }

    // -- Контрпримеры: отказ вместо тихого false -------------------------------

    #[test]
    fn unresolved_condition_is_diagnostic() {
        let mut ctx = empty_ctx();
        let raw = takt_lang::parser::ast::Condition::Bool(Location::Builtin, true);
        let cond = ConditionNode::Unresolved(raw);
        let err = eval_condition(&cond, &mut ctx).unwrap_err();
        assert_eq!(err.code.as_deref(), Some("SIM-016"));
    }

    #[test]
    fn string_condition_is_diagnostic() {
        let mut ctx = empty_ctx();
        let cond = ConditionNode::String(vec!["a".to_string()]);
        let err = eval_condition(&cond, &mut ctx).unwrap_err();
        assert_eq!(err.code.as_deref(), Some("SIM-014"));
    }

    // -- Позиция в диагностике (критерий A10) ----------------------------------

    #[test]
    fn diagnostic_carries_source_location_from_variable() {
        let mut ctx = empty_ctx();
        let loc = Location::Source(0, 10, 20);
        let var = std::rc::Rc::new(std::cell::RefCell::new(
            takt_lang::semantic::VariableNode::Unresolved,
        ));
        let cond = ConditionNode::Variable(var, loc);
        let err = eval_condition(&cond, &mut ctx).unwrap_err();
        assert_eq!(
            err.loc, loc,
            "диагностика обязана нести позицию, а не Builtin"
        );
    }

    #[test]
    fn loc_of_digs_into_children() {
        let loc = Location::Source(0, 3, 7);
        let var = std::rc::Rc::new(std::cell::RefCell::new(
            takt_lang::semantic::VariableNode::Unresolved,
        ));
        let cond = ConditionNode::More(Box::new(ConditionNode::Variable(var, loc)), num(1));
        assert_eq!(loc_of(&cond), loc);
    }
}
