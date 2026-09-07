//! Fixed-point `q(m, n)`: построение типа, границы, понижение литерала.
//!
//! Вынесено из [`type_node`](super::type_node): модуль упёрся в лимит размера, и
//! правило требует делить **по логике**. Граница здесь естественная - всё, что знает
//! про формат `q`, живёт вместе: конструирование (`SE-057`, `SE-104`), ширина хранения,
//! диапазон представления и понижение вещественного литерала (`SE-058`).

use super::TypeNode;
use crate::diagnostics::{Diagnostic, Location};
use crate::semantic::{ExpressionNode, VariableNode};

/// Строит [`TypeNode::Fixed`] из `q(m, n)`, проверяя конструктор и границы: `ctor ==
/// "q"`, `m >= 1`, `n >= 1`, `m + n <= 64`.
///
/// # Коды диагностик
///
/// - `SE-057` - конструктор типа не `q` (единственный fixed-point-конструктор),
///   либо границы `m`/`n`/`W` нарушены.
pub(crate) fn construct_fixed(
    loc: Location,
    ctor: &str,
    m: i128,
    n: i128,
    modifier: Option<&str>,
) -> Result<TypeNode, Diagnostic> {
    if ctor != "q" {
        return Err(Diagnostic::declaration_error(
            loc,
            format!(
                "неизвестный конструктор типа '{}(…, …)'; единственный параметрический \
                 тип — fixed-point 'q(m, n)'",
                ctor
            ),
        )
        .with_code("SE-057"));
    }
    // `m` включает знаковый бит, поэтому `m >= 1`; `n >= 1` - тип обязан иметь дробную
    // часть, иначе это просто знаковое целое `i{m}`. `W <= 64` - представление
    // умещается в `i64` ().
    let bound = |what: &str| {
        Diagnostic::declaration_error(
            loc,
            format!(
                "fixed-point 'q({}, {})': {} (требуется m ≥ 1, n ≥ 1, m + n ≤ 64)",
                m, n, what
            ),
        )
        .with_code("SE-057")
    };
    if m < 1 {
        return Err(bound("целых бит m < 1 (m включает знаковый бит)"));
    }
    if n < 1 {
        return Err(bound("дробных бит n < 1"));
    }
    if m + n > 64 {
        return Err(bound("полная ширина m + n > 64"));
    }
    // Постфиксный модификатор: единственное допустимое слово - `sat`. Отвергать прочие
    // Обязательно: опечатка (`q(8,8) sta`) иначе дала бы молчаливый перенос там, где
    // автор просил насыщение, - ровно тот класс молчаливого расхождения, который фича и
    // закрывает.
    let sat = match modifier {
        None => false,
        Some("sat") => true,
        Some(other) => {
            return Err(Diagnostic::declaration_error(
                loc,
                format!(
                    "после формата fixed-point 'q({}, {})' допустим только модификатор \
                     'sat' (насыщение вместо переноса), получено '{}'",
                    m, n, other
                ),
            )
            .with_code("SE-104"));
        }
    };
    Ok(TypeNode::Fixed {
        m: m as u8,
        n: n as u8,
        sat,
    })
}

/// Машинная ширина хранения fixed-point `q(m, n)` для **программных** целей
/// (`c`/`rust`/`st`): наименьшая из 8/16/32/64, вмещающая `W` бит - в этих целях нет
/// `i12`. Цель `sv` ширину **не** округляет (`logic signed [W-1:0]`). `W <= 64`
/// гарантирован построением типа ([`construct_fixed`]).
pub(crate) fn fixed_storage_bits(w: u8) -> u8 {
    match w {
        0..=8 => 8,
        9..=16 => 16,
        17..=32 => 32,
        _ => 64,
    }
}

/// Диапазон **представлений** `v` типа `q(m, n)` (знаковое `intW`, `W = m + n`):
/// `[-2^(W-1), 2^(W-1) - 1]`.
pub(crate) fn fixed_repr_range(m: u8, n: u8) -> (i128, i128) {
    let w = (m + n) as u32;
    let half = 1i128 << (w - 1);
    (-half, half - 1)
}

/// Переводит числовой литерал (`ExpressionNode::Number`/`Rational`) в
/// **представление** `v` типа `q(m, n)` -.
///
/// Перевод **точный** (без `f64`): `x = мантисса / 10^exp`, представление `v = x ·
/// 2^n`. Если `v` **не целое** (`x` непредставим в формате) или **вне диапазона** типа -
/// ошибка `SE-058`, а не тихое округление (драйвер 3 ).
///
/// - `Ok(Some(v))` - литерал понижен в представление `v` (цели эмитят целое);
/// - `Ok(None)` - `expr` не числовой литерал (арифметика/приведение - не здесь);
/// - `Err(_)` - непредставим или вне диапазона (`SE-058`).
pub(crate) fn lower_fixed_literal(
    expr: &ExpressionNode,
    m: u8,
    n: u8,
    loc: Location,
) -> Result<Option<i128>, Diagnostic> {
    // (мантисса, десятичный порядок): значение = мантисса / 10^exp.
    let (mantissa, exp): (i128, u32) = match expr {
        // Целый литерал приходит уже вычисленным: показатель степени учёл лексер,
        // поэтому здесь порядок нулевой.
        ExpressionNode::Number(k) => (*k, 0),
        ExpressionNode::Rational(s, neg) => {
            // Текст хранится как написан и может нести показатель (`2.5e3`).
            let (num_text, exp_text) = match s.find(['e', 'E']) {
                Some(i) => (&s[..i], &s[i + 1..]),
                None => (s.as_str(), ""),
            };
            let (int_part, frac_part) = num_text.split_once('.').unwrap_or((num_text, ""));
            let digits = format!("{}{}", int_part, frac_part);
            let raw: i128 = digits
                .parse()
                .map_err(|_| se058(loc, m, n, s, "не число"))?;
            let raw = if *neg { -raw } else { raw };

            // Десятичный порядок: дробная часть его повышает, показатель - понижает.
            // Значение = мантисса / 10^(frac_len − показатель).
            let frac_len = i64::try_from(frac_part.len()).unwrap_or(i64::MAX);
            let e: i64 = if exp_text.is_empty() {
                0
            } else {
                exp_text
                    .parse()
                    .map_err(|_| se058(loc, m, n, s, "неверный показатель степени"))?
            };
            let scale = frac_len.saturating_sub(e);
            if scale < 0 {
                // Показатель перевесил дробную часть: значение целое, домножаем.
                let up = u32::try_from(-scale)
                    .map_err(|_| se058(loc, m, n, s, "слишком большой показатель степени"))?;
                let factor = 10i128
                    .checked_pow(up)
                    .ok_or_else(|| se058(loc, m, n, s, "слишком большой показатель степени"))?;
                (
                    raw.checked_mul(factor)
                        .ok_or_else(|| se058(loc, m, n, s, "слишком большой литерал"))?,
                    0,
                )
            } else {
                (
                    raw,
                    u32::try_from(scale)
                        .map_err(|_| se058(loc, m, n, s, "слишком большой показатель степени"))?,
                )
            }
        }
        _ => return Ok(None),
    };

    // v = мантисса · 2^n / 10^exp - целое ⟺ делится нацело.
    let num = mantissa
        .checked_mul(1i128 << n)
        .ok_or_else(|| se058(loc, m, n, &expr_text(expr), "слишком большой литерал"))?;
    let den = 10i128.checked_pow(exp).unwrap_or(i128::MAX);
    if num % den != 0 {
        return Err(se058(
            loc,
            m,
            n,
            &expr_text(expr),
            "не представим точно (дробь не кратна 2⁻ⁿ)",
        ));
    }
    let v = num / den;
    let (min, max) = fixed_repr_range(m, n);
    if v < min || v > max {
        return Err(se058(loc, m, n, &expr_text(expr), "вне диапазона типа"));
    }
    Ok(Some(v))
}

/// Понижает числовой литерал-инициализатор `q(m, n)`-переменной в
/// **представление** `Number(v)`. Возвращает новый узел, если
/// переменная - `Simple`/`Const` с типом `Fixed` и литеральным инициализатором;
/// иначе `None` (тип не `q` либо инициализатор - не литерал). Тело вынесено сюда
/// из [`type_inference`](super::type_inference) ради лимита размера модуля.
pub(crate) fn lower_fixed_var(var: &VariableNode) -> Result<Option<VariableNode>, Diagnostic> {
    use crate::semantic::VariableNode as V;
    match var {
        V::Simple {
            upper,
            loc,
            name,
            ty: TypeNode::Fixed { m, n, sat },
            expr,
        } => Ok(lower_fixed_literal(expr, *m, *n, *loc)?.map(|v| V::Simple {
            upper: upper.clone(),
            loc: *loc,
            name: name.clone(),
            // Признак переносится как есть: понижение литерала меняет запись значения,
            // а не семантику переполнения объявленного типа.
            ty: TypeNode::Fixed {
                m: *m,
                n: *n,
                sat: *sat,
            },
            expr: ExpressionNode::Number(v),
        })),
        V::Const {
            upper,
            loc,
            name,
            ty: TypeNode::Fixed { m, n, sat },
            expr,
        } => Ok(lower_fixed_literal(expr, *m, *n, *loc)?.map(|v| V::Const {
            upper: upper.clone(),
            loc: *loc,
            name: name.clone(),
            ty: TypeNode::Fixed {
                m: *m,
                n: *n,
                sat: *sat,
            },
            expr: ExpressionNode::Number(v),
        })),
        _ => Ok(None),
    }
}

/// `SE-058` - литерал не представим точно в `q(m, n)`.
fn se058(loc: Location, m: u8, n: u8, lit: &str, why: &str) -> Diagnostic {
    Diagnostic::declaration_error(
        loc,
        format!(
            "литерал '{}' не представим в fixed-point 'q({}, {})': {}",
            lit, m, n, why
        ),
    )
    .with_code("SE-058")
}

/// Текстовое представление числового литерала для диагностики.
fn expr_text(expr: &ExpressionNode) -> String {
    match expr {
        ExpressionNode::Number(k) => k.to_string(),
        ExpressionNode::Rational(s, neg) => {
            if *neg {
                format!("-{}", s)
            } else {
                s.clone()
            }
        }
        _ => "<выражение>".to_string(),
    }
}

// -- Тесты ---------------------------------------------------------------------
