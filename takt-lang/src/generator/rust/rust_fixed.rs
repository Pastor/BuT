//! Q-арифметика fixed-point `q(m, n)` для цели Rust.
//!
//! Нормативные правила - обязаны совпасть **побитово** с эталоном симулятора
//! ([`takt_sim::eval::fixed`]) и прочими целями:
//!
//! - `+`/`−` - сложение представлений с wraparound к `W = m + n` (сужение `as
//!   i{W}` - в Rust это усечение битов, а не паника: `i64`-промежуток не
//!   переполняется, `as i{W}` даёт two's-complement перенос, как `wrap(_, W)`);
//! - `*` - точное произведение в `i128`, затем `>> n`. В Rust `>>` знакового
//!   **арифметический** (floor к −∞) и **определён** - в отличие от C (C11
//!   6.5.7p5), где та же трансляция опиралась бы на implementation-defined;
//! - `/` - делимое `<< n` (в Rust `<<` знакового определён), затем деление
//!   (усечение к нулю).
//!
//! Здесь же живёт [`expression_type`] - вывод типа выражения для приведения к `bool` в
//! позиции условия и для восстановления вариантов перечисления; он тематически рядом с
//! детектором формата [`fixed_format`].

use crate::diagnostics::Diagnostic;
use crate::diagnostics::lang::keys;
use crate::generator::rust::rust_expr::{Scope, print_expression, unsupported};
use crate::generator::rust::rust_type::rust_type;
use crate::msg;
use crate::semantic::type_node::TypeNode;
use crate::semantic::type_node::type_fixed::fixed_storage_bits;
use crate::semantic::{ExpressionNode, FunctionDefinitionNode};

/// Возвращает тип выражения, если он выводится статически.
///
/// Нужен для приведения к `bool` в позиции условия: в C `if (x)` при `x : u8` законно,
/// в Rust - ошибка типа. Без типа операнда угадывать нельзя - тот же урок, что у ST
/// (`ST-011`: "без типа операнда имя функции не построить").
pub(crate) fn expression_type(expr: &ExpressionNode) -> Option<TypeNode> {
    match expr {
        ExpressionNode::Bool(_) => Some(TypeNode::Bool),
        ExpressionNode::Number(_) => Some(TypeNode::Integer {
            bits: 32,
            signed: true,
        }),
        ExpressionNode::Rational(_, _) => Some(TypeNode::Rational),
        ExpressionNode::Variable(var) => Some(var.borrow().ty().clone()),
        ExpressionNode::Parenthesis(inner) => expression_type(inner),
        ExpressionNode::Cast(_, ty) => Some(ty.clone()),
        // Сравнения и логические операции дают `bool` независимо от операндов.
        ExpressionNode::Less(_, _)
        | ExpressionNode::More(_, _)
        | ExpressionNode::LessEqual(_, _)
        | ExpressionNode::MoreEqual(_, _)
        | ExpressionNode::Equal(_, _)
        | ExpressionNode::NotEqual(_, _)
        | ExpressionNode::And(_, _)
        | ExpressionNode::Or(_, _)
        | ExpressionNode::Not(_) => Some(TypeNode::Bool),
        // `x.N` (разряд) логичен, а `s.field` (поле структуры) - Нет.
        //
        // Тип поля здесь неизвестен - объявление структуры лежит в модели, а сюда она
        // не передаётся, - поэтому честный ответ `None`: приведение не применяется,
        // печать идёт обычным путём.
        ExpressionNode::BitAccess(_, member) => match member {
            crate::parser::ast::Member::Number(_) => Some(TypeNode::Bool),
            crate::parser::ast::Member::Identifier(_) => None,
        },
        // База - выражение: тип берётся у неё рекурсивно.
        ExpressionNode::ArraySubscript(base, _) => match expression_type(base) {
            Some(TypeNode::Array(_, elem)) => Some((*elem).clone()),
            _ => None,
        },
        // Именованное условие - всегда логическое: `cond` есть условие по построению.
        // Без этой ветви `if ready { ... }` отвергался `RS-011` "тип не выводится",
        // хотя выводить нечего.
        ExpressionNode::Condition(_) => Some(TypeNode::Bool),
        // Тип вызова - Объявленный возврат функции. Без этого `if is_ready()` при `fn
        // is_ready() -> bool` не приводится к bool: тип "не выводится", и честная
        // диагностика RS-011 срабатывает там, где всё известно.
        ExpressionNode::Function(def, _) => function_return(&def.borrow()),
        _ => None,
    }
}

/// Возвращает объявленный тип результата функции.
pub(crate) fn function_return(def: &FunctionDefinitionNode) -> Option<TypeNode> {
    match def {
        FunctionDefinitionNode::Local { ret, .. }
        | FunctionDefinitionNode::External { ret, .. } => Some(ret.clone()),
        // У встроенных возврат описан тем же полем (`min`/`max`/... - Numeric, `debug` -
        // Unit): угадывать не требуется.
        FunctionDefinitionNode::Builtin(_, _, ret) => Some(ret.clone()),
        FunctionDefinitionNode::None | FunctionDefinitionNode::Unresolved(_) => None,
    }
}

/// Арифметическая операция над `q(m, n)`.
#[derive(Clone, Copy)]
pub(crate) enum FixedOp {
    Add,
    Subtract,
    Multiply,
    Divide,
}

/// Формат `q(m, n)` выражения, если его тип - `Fixed`.
///
/// `SE-059` гарантирует единый формат обоих операндов арифметики, поэтому у бинарного
/// узла достаточно взять формат любой стороны, у которой он выводится.
pub(crate) fn fixed_format_in(
    expr: &ExpressionNode,
    model: &crate::semantic::ModelNode,
) -> Option<(u8, u8, bool)> {
    // Сперва - собственный вывод цели: он не требует модели и покрывает большинство
    // узлов.
    if let Some(found) = fixed_format(expr) {
        return Some(found);
    }
    // Затем - разбор с оглядкой на модель: тип поля структуры объявлен в ней, и без
    // него `g.kp as u8` печаталось без масштабирования - замер 2026-08-21 дал у эталона
    // `1`, а у цели `rust` **128**, молча и при нулевом коде возврата `taktc`.
    match expr {
        ExpressionNode::Parenthesis(inner) | ExpressionNode::Negate(inner) => {
            fixed_format_in(inner, model)
        }
        ExpressionNode::Add(a, b)
        | ExpressionNode::Subtract(a, b)
        | ExpressionNode::Multiply(a, b)
        | ExpressionNode::Divide(a, b) => {
            fixed_format_in(a, model).or_else(|| fixed_format_in(b, model))
        }
        ExpressionNode::BitAccess(inner, crate::parser::ast::Member::Identifier(field)) => {
            match field_type(inner, &field.name, model)? {
                TypeNode::Fixed { m, n, sat } => Some((m, n, sat)),
                _ => None,
            }
        }
        _ => None,
    }
}

/// Тип поля структуры по выражению-базе.
///
/// Разбирается та же цепочка, что и у места записи: переменная, элемент массива,
/// скобки. Объявление структуры берётся у модели - второго знания о полях цель не
/// заводит.
fn field_type(
    base: &ExpressionNode,
    field: &str,
    model: &crate::semantic::ModelNode,
) -> Option<TypeNode> {
    let base_ty = match base {
        ExpressionNode::Variable(var) => var.borrow().ty().clone(),
        ExpressionNode::Parenthesis(inner) => return field_type(inner, field, model),
        ExpressionNode::ArraySubscript(inner, _) => match expression_type(inner)? {
            TypeNode::Array(_, elem) => (*elem).clone(),
            _ => return None,
        },
        _ => return None,
    };
    let TypeNode::Struct(name) = base_ty else {
        return None;
    };
    model
        .search_struct(&name)?
        .fields
        .iter()
        .find(|(f, _)| f == field)
        .map(|(_, ty)| ty.clone())
}

pub(crate) fn fixed_format(expr: &ExpressionNode) -> Option<(u8, u8, bool)> {
    // Признак насыщения едет вместе с разрядностями: печатник обязан знать, чем
    // закрывать операцию - переносом или прижатием.
    if let Some(TypeNode::Fixed { m, n, sat }) = expression_type(expr) {
        return Some((m, n, sat));
    }
    match expr {
        ExpressionNode::Add(a, b)
        | ExpressionNode::Subtract(a, b)
        | ExpressionNode::Multiply(a, b)
        | ExpressionNode::Divide(a, b) => fixed_format(a).or_else(|| fixed_format(b)),
        ExpressionNode::Negate(a) | ExpressionNode::Parenthesis(a) => fixed_format(a),
        _ => None,
    }
}

/// Тип хранения `q(m, n)` в Rust: `i{S}`, где `S` - округлённая вверх ширина.
fn storage(m: u8, n: u8) -> String {
    format!("i{}", fixed_storage_bits(m + n))
}

/// Оборачивает готовое выражение-представление в перенос к **W** и хранение `i{S}`.
///
/// Приведение `as i{S}` переносит к ширине хранения, а правило требует переноса к
/// `W = m + n`: при `W = 12` это 16 бит против 12, то есть другая граница. Совпадают они
/// лишь когда `W` есть 8, 16, 32 или 64, - а таков весь корпус.
///
/// При `W = S` форма печати не меняется: иначе поедут снимки `examples/generated`.
/// Перенос делается парой сдвигов (в Rust `<<` не паникует на потере старших бит, а `>>`
/// знакового арифметический), а не маской: так знак восстанавливается тем же выражением.
/// `wide` говорит, что `inner` посчитан в `i128` (произведение, деление, пересчёт
/// `q -> q`): такому выражению нужен явный `as i64` перед сдвигами, а уже `i64` его
/// давать нельзя - `clippy::unnecessary_cast` под `-D warnings`.
fn wrap_to(inner: &str, m: u8, n: u8, wide: bool, sat: bool) -> String {
    let s = storage(m, n);
    let w = m + n;
    let narrowed = if wide {
        format!("({inner}) as i64")
    } else {
        inner.to_string()
    };
    // Насыщение: прижатие к границам формата, а не типа хранения - поэтому оно нужно
    // всегда, в том числе при `W = S`. Границы печатаются литералами: выражение вида
    // `i64::MIN >> k` спорно читается и придирчиво разбирается clippy.
    if sat {
        let max = (1i64 << (w - 1)) - 1;
        let min = -(1i64 << (w - 1));
        return format!("(({narrowed}).clamp({min}, {max}) as {s})");
    }
    if w == fixed_storage_bits(w) {
        return format!("(({inner}) as {s})");
    }
    let shift = 64 - w;
    format!("((({narrowed}) << {shift} >> {shift}) as {s})")
}

/// `2^n` как целочисленный литерал.
fn pow2(n: u8) -> u64 {
    1u64 << n
}

/// Печатает бинарную q-операцию: результат сужается к `i{W}` (wraparound к W).
pub(crate) fn binary(
    op: FixedOp,
    a: &ExpressionNode,
    b: &ExpressionNode,
    scope: &Scope,
    m: u8,
    n: u8,
    sat: bool,
) -> Result<String, Diagnostic> {
    // Операнды не оборачиваются в скобки: `print_expression` уже скобкует составные
    // узлы, а лишняя пара -> clippy::double_parens под `-D warnings`. Приоритет `as`
    // выше `+`/`*`/`>>`, поэтому `la as i64 + lb as i64` группирует верно и без скобок
    // вокруг операндов.
    let (la, lb) = (print_expression(a, scope)?, print_expression(b, scope)?);
    // Приведение операнда печатается по нужде: у целочисленного литерала тип выводится
    // из соседа, а `512 as i128` - это `clippy::unnecessary_cast`, то есть отказ проверки
    // самой цели. Литерал попадает сюда с: до неё приведение дробного литерала к `q`
    // цель отвергала.
    let (wa64, wb64) = (widened(&la, "i64"), widened(&lb, "i64"));
    let (wa128, wb128) = (widened(&la, "i128"), widened(&lb, "i128"));
    let inner = match op {
        FixedOp::Add => format!("{wa64} + {wb64}"),
        FixedOp::Subtract => format!("{wa64} - {wb64}"),
        // Точное произведение 2W, floor к −∞ через арифметический `>>` (в Rust
        // определён для знакового, C-цель обходит C11 хелпером).
        FixedOp::Multiply => format!("({wa128} * {wb128}) >> {n}"),
        // Делимое <- n влево (в Rust `<<` знакового определён), деление к нулю. Скобка
        // вокруг `(la as i128)` обязательна: `la as i128 << n` Rust парсит как generic
        // `i128<...>`, а не сдвиг (E0747-подобная ошибка).
        FixedOp::Divide => format!("(({wa128}) << {n}) / {wb128}"),
    };
    let wide = matches!(op, FixedOp::Multiply | FixedOp::Divide);
    Ok(wrap_to(&inner, m, n, wide, sat))
}

/// Печатает унарный минус над `q(m, n)`: `−repr` с wraparound к W.
pub(crate) fn negate(
    inner: &ExpressionNode,
    scope: &Scope,
    m: u8,
    n: u8,
    sat: bool,
) -> Result<String, Diagnostic> {
    Ok(wrap_to(
        &format!("-{} as i64", print_expression(inner, scope)?),
        m,
        n,
        false,
        sat,
    ))
}

/// Печатает приведение `expr as T`, когда источник **или** цель - `q(m, n)`.
///
/// Масштабирование: `int`/`bool` ↔ `q` - сдвиг на `2^n` (в Rust `<<`/`>>` знакового
/// определены), `float` -> `q` - недоступно в `no_std` (нет `f64::floor` без `libm`) ->
/// честная `RS-011`.
pub(crate) fn cast(
    inner: &ExpressionNode,
    target: &TypeNode,
    scope: &Scope,
) -> Result<String, Diagnostic> {
    // Формат источника спрашивается С оглядкой на модель: без неё приведение из поля
    // структуры печаталось без масштабирования.
    let src = fixed_format_in(inner, scope.model);
    let printed = print_expression(inner, scope)?;
    match (src, target) {
        // q -> q: пересчёт дробных разрядов (влево - сдвиг, вправо - floor `>>`).
        (Some((_, from_n, _)), TypeNode::Fixed { m: tm, n: tn, sat }) => {
            // Скобка вокруг `(printed as i128)` обязательна перед `<<`/`>>` (иначе Rust
            // парсит `i128<...>` как generic, не сдвиг).
            let inner = if tn >= &from_n {
                format!("({printed} as i128) << {}", tn - from_n)
            } else {
                format!("({printed} as i128) >> {}", from_n - tn)
            };
            Ok(wrap_to(&inner, *tm, *tn, true, *sat))
        }
        // q -> float: repr / 2^n (точно представимо в f64).
        (Some((_, from_n, _)), TypeNode::Rational) => {
            Ok(format!("({printed} as f64 / {}.0)", pow2(from_n)))
        }
        // q -> целое/бит: floor(repr / 2^n) = целая часть (арифметический `>>`).
        (Some((_, from_n, _)), _) => {
            let t = rust_type(target, &msg!(keys::RS_WHAT_Q_TO_INT))?;
            Ok(format!("((({printed} as i64) >> {from_n}) as {t})"))
        }
        // Литерал -> q: значение известно при компиляции.
        //
        // Именно это и обещал текст соседнего отказа ("литеральный float понижается на
        // этапе компиляции"), но в теле не делал никто: замер 2026-08-22 дал `RS-011`
        // на записи, которую эталон исполняет. Счёт - у общего носителя
        // (`const_eval::fixed_literal`).
        (None, TypeNode::Fixed { .. })
            if let Some(repr) =
                crate::semantic::const_eval::fixed_literal::cast_repr(inner, target) =>
        {
            Ok(format!("{repr}"))
        }
        // float -> q: floor(f · 2^n) - нет в no_std без libm.
        (None, TypeNode::Fixed { .. })
            if matches!(expression_type(inner), Some(TypeNode::Rational)) =>
        {
            Err(unsupported(&msg!(keys::RS_WHAT_FLOAT_TO_Q)))
        }
        // целое/бит -> q: repr = v · 2^n с wraparound к W.
        (None, TypeNode::Fixed { m: tm, n: tn, sat }) => Ok(wrap_to(
            &format!("({printed} as i64) << {tn}"),
            *tm,
            *tn,
            false,
            *sat,
        )),
        // Ни источник, ни цель не q - вызывающий не должен был звать сюда.
        (None, _) => {
            let t = rust_type(target, &msg!(keys::GEN_WHAT_CAST))?;
            Ok(format!("({printed} as {t})"))
        }
    }
}

/// Операнд с приведением к широкому типу - **по нужде**.
///
/// Признак задаётся напечатанному тексту, а не узлу: печать литерала принадлежит
/// соседним ветвям (представление `q` считает `const_eval::fixed_literal`), и второй
/// способ узнать "это литерал" разошёлся бы с первым.
fn widened(printed: &str, ty: &str) -> String {
    if is_int_literal(printed) {
        printed.to_string()
    } else {
        format!("{printed} as {ty}")
    }
}

/// Истина, если текст - целочисленный литерал (возможно, со знаком минус).
fn is_int_literal(text: &str) -> bool {
    let digits = text.strip_prefix('-').unwrap_or(text);
    !digits.is_empty() && digits.bytes().all(|b| b.is_ascii_digit())
}
