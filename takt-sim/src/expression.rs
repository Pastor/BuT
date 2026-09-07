//! Адаптер выражений: `ExpressionNode` -> [`Value`] поверх ядра [`crate::eval`].
//!
//! Симметричен [`crate::predicate`] (адаптер условий) и, как и он, **не содержит
//! семантики** - только структурный разбор и делегирование в ядро.
//!
//! # Что здесь чинится
//!
//! До тела блоков `enter`/`exit`/`always` вычислял
//! `unit/builder.rs::eval_expression_rt`, покрывавший **6 из ~45** вариантов
//! `ExpressionNode`; всё остальное уходило в `_ => None`, а `None` приводил к
//! **молчаливому пропуску присваивания** (Д1, Д2). Здесь разбор исчерпывающий:
//! новый вариант ломает сборку, а невычислимое выражение даёт `Err`, который
//! вызывающий обязан обработать.
//!
//! # Позиции в диагностиках
//!
//! `ExpressionNode::Variable` - в отличие от `ConditionNode::Variable` - позиции
//! использования **не несёт**, поэтому диагностика привязывается к позиции
//! *объявления* переменной (`VariableNode::loc()`). Это менее точно, чем в
//! условиях, но существенно лучше `Location::Builtin`.
//!
//! Сам поиск позиции живёт **не здесь**: с это метод
//! [`ExpressionNode::loc`](takt_lang::semantic::ExpressionNode::loc) в `takt-lang`.

use crate::context::Context;
use crate::eval::error::EvalError;
use crate::eval::ops::{self, BinOp, UnOp};
use crate::eval::value::Value;
use crate::eval::{self as eval_core};
use takt_lang::diagnostics::{Diagnostic, Location};
use takt_lang::semantic::ExpressionNode;

/// Вычисляет выражение в значение.
///
/// Паники недостижимы: любой неподдержанный случай - `Err`.
pub(crate) fn eval_expression(
    expr: &ExpressionNode,
    ctx: &mut dyn Context,
) -> Result<Value, Diagnostic> {
    match expr {
        // -- Литералы ---------------------------------------------------------
        // Длительность несёт **наносекунды** - каноническое представление языка;
        // профиль ("часы"/"такты") есть свойство генерации, а не модели.
        //
        // Здесь стояла заглушка-отказ "до ", и она пережила саму: адаптер условий
        // (`predicate.rs`) и вычислитель начальных значений (`unit/initial.rs`)
        // значение длительности отдавали, а **тело блока** отказывалось его вычислять -
        // то есть обещанное документом `left := pause + 750ms;` давало `SIM-007`. Три
        // места разбирают один узел, и одно разошлось с двумя.
        ExpressionNode::Duration(ns) => Ok(Value::Duration(*ns)),
        ExpressionNode::Number(n) => Ok(Value::Number(*n)),
        ExpressionNode::Bool(b) => Ok(Value::Boolean(*b)),
        ExpressionNode::Rational(text, negative) => parse_rational(text, *negative),
        // Адресный литерал `адрес:бит` - значение самого адреса.
        ExpressionNode::Address(addr, _bit) => Ok(Value::Number(i128::from(*addr))),
        // Анонимное обращение к ячейке: чтение памяти. Ячейка моделируется
        // синтетическим портом - её значение видно в трассе, и потому сверка с целью
        // `c-hal` возможна потактово, а не "по факту компиляции".
        ExpressionNode::AnonPort(access) => Ok(crate::anon_cell::read(access, ctx)),

        // -- Переменные и доступ ----------------------------------------------
        ExpressionNode::Variable(var) => {
            let borrowed = var.borrow();
            ctx.get_value(borrowed.name()).ok_or_else(|| {
                Diagnostic::error(
                    borrowed.loc(),
                    format!("переменная '{}' не найдена", borrowed.name()),
                )
                .with_code("SIM-009")
            })
        }
        ExpressionNode::Parenthesis(inner) => eval_expression(inner, ctx),
        // База - Выражение: `b.data[1]` вычисляется тем же вычислителем, что и прочие
        // выражения, поэтому знание о цепочке места остаётся одно.
        ExpressionNode::ArraySubscript(base, index) => {
            let loc = base.loc();
            let array = eval_expression(base, ctx)?;
            let Value::Array(items) = array else {
                return Err(Diagnostic::error(
                    loc,
                    "индексируемое значение не является массивом".to_string(),
                )
                .with_code("SIM-010"));
            };
            let Value::Number(idx) = eval_expression(index, ctx)? else {
                return Err(
                    Diagnostic::error(loc, "индекс массива должен быть целым".to_string())
                        .with_code("SIM-010"),
                );
            };
            usize::try_from(idx)
                .ok()
                .and_then(|i| items.get(i).cloned())
                .ok_or_else(|| {
                    Diagnostic::error(
                        loc,
                        format!("индекс {idx} вне границ массива (длина {})", items.len()),
                    )
                    .with_code("SIM-010")
                })
        }
        ExpressionNode::ArraySlice(base, from, to) => {
            let loc = base.loc();
            let array = eval_expression(base, ctx)?;
            let Value::Array(items) = array else {
                return Err(Diagnostic::error(
                    loc,
                    "срезаемое значение не является массивом".to_string(),
                )
                .with_code("SIM-010"));
            };
            let start = usize::try_from(from.unwrap_or(0)).unwrap_or(0);
            let end = to
                .map(|t| usize::try_from(t).unwrap_or(0))
                .unwrap_or(items.len());
            items
                .get(start..end)
                .map(|slice| Value::Array(slice.to_vec()))
                .ok_or_else(|| {
                    Diagnostic::error(
                        loc,
                        format!(
                            "срез [{start}:{end}] вне границ массива (длина {})",
                            items.len()
                        ),
                    )
                    .with_code("SIM-010")
                })
        }
        // `a.b`: поле структуры (`p.x`) или бит целого (`BTN.0`). Различение - по
        // вычисленному значению, в общем ядре `eval::access` (адаптеры
        // `expression`/`predicate` его не дублируют).
        ExpressionNode::BitAccess(inner, member) => {
            let value = eval_expression(inner, ctx)?;
            eval_core::access::read_member(&value, member).map_err(|e| e.to_diagnostic(expr.loc()))
        }

        // -- Унарные ----------------------------------------------------------
        ExpressionNode::Not(inner) => unary(UnOp::Not, inner, ctx),
        ExpressionNode::BitwiseNot(inner) => unary(UnOp::BitwiseNot, inner, ctx),
        ExpressionNode::UnaryPlus(inner) => unary(UnOp::UnaryPlus, inner, ctx),
        ExpressionNode::Negate(inner) => unary(UnOp::Negate, inner, ctx),

        // -- Арифметика - то, чего не умел eval_expression_rt (Д1) ------------
        ExpressionNode::Add(l, r) => binary(BinOp::Add, l, r, ctx),
        ExpressionNode::Subtract(l, r) => binary(BinOp::Subtract, l, r, ctx),
        ExpressionNode::Multiply(l, r) => binary(BinOp::Multiply, l, r, ctx),
        ExpressionNode::Divide(l, r) => binary(BinOp::Divide, l, r, ctx),
        ExpressionNode::Modulo(l, r) => binary(BinOp::Modulo, l, r, ctx),
        ExpressionNode::Power(l, r) => binary(BinOp::Power, l, r, ctx),
        ExpressionNode::ShiftLeft(l, r) => binary(BinOp::ShiftLeft, l, r, ctx),
        ExpressionNode::ShiftRight(l, r) => binary(BinOp::ShiftRight, l, r, ctx),
        ExpressionNode::BitwiseAnd(l, r) => binary(BinOp::BitwiseAnd, l, r, ctx),
        ExpressionNode::BitwiseXor(l, r) => binary(BinOp::BitwiseXor, l, r, ctx),
        ExpressionNode::BitwiseOr(l, r) => binary(BinOp::BitwiseOr, l, r, ctx),

        // -- Сравнения и логика -----------------------------------------------
        ExpressionNode::Less(l, r) => binary(BinOp::Less, l, r, ctx),
        ExpressionNode::More(l, r) => binary(BinOp::More, l, r, ctx),
        ExpressionNode::LessEqual(l, r) => binary(BinOp::LessEqual, l, r, ctx),
        ExpressionNode::MoreEqual(l, r) => binary(BinOp::MoreEqual, l, r, ctx),
        ExpressionNode::Equal(l, r) => binary(BinOp::Equal, l, r, ctx),
        ExpressionNode::NotEqual(l, r) => binary(BinOp::NotEqual, l, r, ctx),
        // В `ExpressionNode` побитовые операции - отдельные варианты, поэтому
        // `And`/`Or` здесь именно логические (в отличие от `ConditionNode`).
        ExpressionNode::And(l, r) => binary(BinOp::LogicalAnd, l, r, ctx),
        ExpressionNode::Or(l, r) => binary(BinOp::LogicalOr, l, r, ctx),

        // -- Составные --------------------------------------------------------
        ExpressionNode::ConditionalOperator(cond, then_, else_) => {
            let cond_value = eval_expression(cond, ctx)?;
            let taken = ops::to_bool(&cond_value).map_err(|e| e.to_diagnostic(cond.loc()))?;
            if taken {
                eval_expression(then_, ctx)
            } else {
                eval_expression(else_, ctx)
            }
        }
        // Приведение типа. Для q(m, n) каст **масштабирует** (int/float ↔ q), тогда как
        // запись в переменную трактует Number как готовое представление - поэтому
        // отдельное ядро `cast_to_type`.
        ExpressionNode::Cast(inner, ty) => {
            let value = eval_expression(inner, ctx)?;
            eval_core::cast_to_type(value, ty).map_err(|e| e.to_diagnostic(inner.loc()))
        }
        ExpressionNode::Array(items) | ExpressionNode::Initializer(items) => {
            let values = items
                .iter()
                .map(|item| eval_expression(item, ctx))
                .collect::<Result<Vec<Value>, Diagnostic>>()?;
            Ok(Value::Array(values))
        }

        // -- Пока не поддержано - но с диагностикой, а не тихим пропуском -----
        //
        // Вызовы функций требуют интерпретатора тела `fn`. Д3/Д4:
        // вызов функции. Аргументы вычисляются в контексте вызывающего, тело исполняет
        // общий интерпретатор (`unit::statement`).
        ExpressionNode::Function(func, args) => {
            // `debug` перехватывается до вычисления аргументов: его аргумент -
            // строковый литерал, а `Value` строк не представляет, и общий путь упал бы
            // на "строки не поддерживаются". Печать идёт в stderr: stdout занят трассой
            // прогона, которую читают сверки.
            if let Some(text) = debug_argument(func, args) {
                eprintln!("debug: {text}");
                return Ok(Value::Number(0));
            }
            let values = args
                .iter()
                .map(|arg| eval_expression(arg, ctx))
                .collect::<Result<Vec<Value>, Diagnostic>>()?;
            crate::unit::statement::call_function(func, &values, ctx)
        }
        ExpressionNode::NamedFunctionBox(_, _) => {
            Err(unsupported("вызов с именованными аргументами", expr.loc()))
        }
        ExpressionNode::CodeBlock(_, _) => Err(unsupported("блок кода как выражение", expr.loc())),
        // Присваивание внутри выражения требует доступа на запись, которого у
        // вычислителя нет. Присваивание-оператор обрабатывает `builder.rs`.
        ExpressionNode::Assign(_, _) => {
            Err(unsupported("присваивание внутри выражения", expr.loc()))
        }
        // `Value` не представляет строки - пробел зафиксирован анализом.
        ExpressionNode::String(_) => Err(unsupported("строки", expr.loc())),
        ExpressionNode::Type(_) => Err(unsupported("тип как выражение", expr.loc())),
        ExpressionNode::Model(_) => Err(unsupported("модель как выражение", expr.loc())),
        // Именованное условие в позиции выражения вычисляется тем же адаптером условий,
        // что и на ребре. Иначе эталон отвечает `SIM-014`, а цель `c` печатает для того
        // же входа макрос `COND_...`, которого нигде не определяет, то есть выдаёт
        // невалидный C при нулевом коде возврата.
        //
        // Своего разбора здесь нет и быть не должно: условие судит `predicate.rs`, и
        // второе знание о нём разошлось бы с первым.
        ExpressionNode::Condition(cond) => {
            crate::predicate::eval_condition(&cond.borrow().value, ctx)
        }
        ExpressionNode::List(_) => Err(unsupported("список параметров", expr.loc())),

        // -- Невычислимые по определению --------------------------------------
        ExpressionNode::None => Err(Diagnostic::error(
            expr.loc(),
            "пустое выражение не может быть вычислено".to_string(),
        )
        .with_code("SIM-015")),
        ExpressionNode::Unresolved(_) => Err(Diagnostic::error(
            expr.loc(),
            "неразрешённое выражение не может быть вычислено".to_string(),
        )
        .with_code("SIM-016")),
    }
}

/// Текст `debug("...")`, если вызов - именно встроенная `debug` со строковым литералом.
///
/// Форму задаёт цель `rust` (`hal.debug("текст")`): аргумент обязан быть литералом,
/// потому что форматирования в `no_std` нет. Эталон следует той же форме - иначе он
/// принимал бы модели, которые прошивка не соберёт.
fn debug_argument(
    func: &std::rc::Rc<std::cell::RefCell<takt_lang::semantic::FunctionDefinitionNode>>,
    args: &[ExpressionNode],
) -> Option<String> {
    let borrowed = func.borrow();
    let takt_lang::semantic::FunctionDefinitionNode::Builtin(name, _, _) = &*borrowed else {
        return None;
    };
    if *name != "debug" {
        return None;
    }
    match args {
        [ExpressionNode::String(parts)] => Some(parts.join("")),
        _ => None,
    }
}

fn unsupported(what: &str, loc: Location) -> Diagnostic {
    Diagnostic::error(loc, format!("{what} не поддерживается симулятором")).with_code("SIM-014")
}

fn parse_rational(text: &str, negative: bool) -> Result<Value, Diagnostic> {
    let parsed: f64 = text.parse().map_err(|_| {
        Diagnostic::error(
            Location::Builtin,
            format!("не удалось разобрать вещественный литерал '{text}'"),
        )
        .with_code("SIM-008")
    })?;
    Ok(Value::Real(if negative { -parsed } else { parsed }))
}

fn unary(op: UnOp, inner: &ExpressionNode, ctx: &mut dyn Context) -> Result<Value, Diagnostic> {
    let value = eval_expression(inner, ctx)?;
    ops::apply_unary(op, &value).map_err(|e: EvalError| e.to_diagnostic(inner.loc()))
}

fn binary(
    op: BinOp,
    left: &ExpressionNode,
    right: &ExpressionNode,
    ctx: &mut dyn Context,
) -> Result<Value, Diagnostic> {
    let lhs = eval_expression(left, ctx)?;
    let rhs = eval_expression(right, ctx)?;
    ops::apply_binary(op, &lhs, &rhs).map_err(|e: EvalError| {
        let loc = match left.loc() {
            Location::Builtin => right.loc(),
            found => found,
        };
        e.to_diagnostic(loc)
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;
    use takt_lang::semantic::StatementNode;
    use takt_lang::semantic::type_node::TypeNode;

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

    fn num(n: i128) -> Box<ExpressionNode> {
        Box::new(ExpressionNode::Number(n))
    }

    // -- Д1: арифметика, которой не было ---------------------------------------

    #[test]
    fn d1_add_is_evaluated() {
        // Ядро дефекта Д1: `a + 1` уходило в `_ => None`.
        let mut ctx = MockContext::new(&[]);
        let expr = ExpressionNode::Add(num(5), num(1));
        assert_eq!(eval_expression(&expr, &mut ctx), Ok(Value::Number(6)));
    }

    #[test]
    fn d1_all_arithmetic_operators_work() {
        let mut ctx = MockContext::new(&[]);
        for (expr, expected) in [
            (ExpressionNode::Subtract(num(5), num(3)), 2),
            (ExpressionNode::Multiply(num(5), num(3)), 15),
            (ExpressionNode::Divide(num(7), num(2)), 3),
            (ExpressionNode::Modulo(num(7), num(2)), 1),
            (ExpressionNode::Power(num(2), num(10)), 1024),
            (ExpressionNode::ShiftLeft(num(1), num(8)), 256),
            (ExpressionNode::ShiftRight(num(256), num(8)), 1),
            (ExpressionNode::BitwiseAnd(num(6), num(3)), 2),
            (ExpressionNode::BitwiseOr(num(6), num(3)), 7),
            (ExpressionNode::BitwiseXor(num(6), num(3)), 5),
        ] {
            assert_eq!(
                eval_expression(&expr, &mut ctx),
                Ok(Value::Number(expected)),
                "провал на {expr:?}"
            );
        }
    }

    #[test]
    fn comparisons_and_logic() {
        let mut ctx = MockContext::new(&[]);
        assert_eq!(
            eval_expression(&ExpressionNode::Less(num(1), num(2)), &mut ctx),
            Ok(Value::Boolean(true))
        );
        assert_eq!(
            eval_expression(
                &ExpressionNode::And(
                    Box::new(ExpressionNode::Bool(true)),
                    Box::new(ExpressionNode::Bool(false))
                ),
                &mut ctx
            ),
            Ok(Value::Boolean(false))
        );
    }

    #[test]
    fn parenthesis_recurses() {
        let mut ctx = MockContext::new(&[]);
        let inner = ExpressionNode::Add(num(5), num(1));
        let expr = ExpressionNode::More(
            Box::new(ExpressionNode::Parenthesis(Box::new(inner))),
            num(2),
        );
        assert_eq!(eval_expression(&expr, &mut ctx), Ok(Value::Boolean(true)));
    }

    #[test]
    fn conditional_operator_picks_branch() {
        let mut ctx = MockContext::new(&[]);
        let expr = ExpressionNode::ConditionalOperator(
            Box::new(ExpressionNode::Bool(true)),
            num(1),
            num(2),
        );
        assert_eq!(eval_expression(&expr, &mut ctx), Ok(Value::Number(1)));
    }

    #[test]
    fn cast_uses_core_coercion() {
        // Cast опирается на то же coerce_to_type, что и запись в переменную.
        let mut ctx = MockContext::new(&[]);
        let expr = ExpressionNode::Cast(
            num(300),
            TypeNode::Integer {
                bits: 8,
                signed: false,
            },
        );
        assert_eq!(eval_expression(&expr, &mut ctx), Ok(Value::Number(44)));
    }

    #[test]
    fn array_literal_builds_array() {
        let mut ctx = MockContext::new(&[]);
        let expr =
            ExpressionNode::Array(vec![ExpressionNode::Number(1), ExpressionNode::Number(2)]);
        assert_eq!(
            eval_expression(&expr, &mut ctx),
            Ok(Value::Array(vec![Value::Number(1), Value::Number(2)]))
        );
    }

    // -- Ошибки: отказ вместо тихого пропуска (Д2) -----------------------------

    #[test]
    fn d2_division_by_zero_is_diagnostic_not_silence() {
        let mut ctx = MockContext::new(&[]);
        let expr = ExpressionNode::Divide(num(10), num(0));
        let err = eval_expression(&expr, &mut ctx).unwrap_err();
        assert_eq!(err.code.as_deref(), Some("SIM-001"));
    }

    #[test]
    fn function_call_returns_value() {
        // Д3: `fn f(n: u8) -> u8 { return n + 1; }` -> f(41) = 42.
        let func = std::rc::Rc::new(std::cell::RefCell::new(
            takt_lang::semantic::FunctionDefinitionNode::Local {
                upper: None,
                loc: Location::Builtin,
                name: "f".to_string(),
                params: vec![(
                    "n".to_string(),
                    TypeNode::Integer {
                        bits: 8,
                        signed: false,
                    },
                )],
                ret: TypeNode::Integer {
                    bits: 8,
                    signed: false,
                },
                raw: Box::default(),
                body: StatementNode::Return(
                    Some(Box::new(ExpressionNode::Add(
                        Box::new(ExpressionNode::Variable(std::rc::Rc::new(
                            std::cell::RefCell::new(takt_lang::semantic::VariableNode::Simple {
                                upper: None,
                                loc: Location::Builtin,
                                name: "n".to_string(),
                                ty: TypeNode::Integer {
                                    bits: 8,
                                    signed: false,
                                },
                                expr: ExpressionNode::Number(0),
                            }),
                        ))),
                        num(1),
                    ))),
                    Location::Codegen,
                ),
            },
        ));
        let mut ctx = MockContext::new(&[]);
        let expr = ExpressionNode::Function(func, vec![ExpressionNode::Number(41)]);
        assert_eq!(eval_expression(&expr, &mut ctx), Ok(Value::Number(42)));
    }

    #[test]
    fn unresolved_function_is_diagnostic_not_panic() {
        // Контрпример: неразрешённая функция - отказ, а не паника.
        let mut ctx = MockContext::new(&[]);
        let expr = ExpressionNode::Function(
            std::rc::Rc::new(std::cell::RefCell::new(Default::default())),
            vec![],
        );
        let err = eval_expression(&expr, &mut ctx).unwrap_err();
        assert_eq!(err.code.as_deref(), Some("SIM-016"));
    }

    #[test]
    fn string_expression_is_diagnostic() {
        let mut ctx = MockContext::new(&[]);
        let expr = ExpressionNode::String(vec!["a".to_string()]);
        let err = eval_expression(&expr, &mut ctx).unwrap_err();
        assert_eq!(err.code.as_deref(), Some("SIM-014"));
    }

    #[test]
    fn none_expression_is_diagnostic() {
        let mut ctx = MockContext::new(&[]);
        let err = eval_expression(&ExpressionNode::None, &mut ctx).unwrap_err();
        assert_eq!(err.code.as_deref(), Some("SIM-015"));
    }
}
