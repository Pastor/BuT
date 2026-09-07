//! Q-арифметика fixed-point `q(m, n)` для цели SV.
//!
//! Ради этой цели фича и заведена: в синтезируемом RTL плавающей точки нет (`SV-003`),
//! а fixed-point даёт дробную арифметику в аппаратуре.
//!
//! Ширина в SV не округляется, в отличие от `c`, `rust` и `st`: `q(m, n)` есть
//! `logic signed [W-1:0]`, где `W = m + n`. Правила совпадают с эталоном (`eval::fixed`)
//! и целями `c`, `rust`, `st` побитово:
//!
//! - `+`/`−` - сложение представлений, wraparound к `W` (size-cast `W'(...)`);
//! - `*` - точное произведение шириной `2W`, затем **арифметический** сдвиг
//!   `>>> n` (в SV `>>>` знакового определён - floor к −∞);
//! - `/` - делимое `<<< n` в `2W`, затем знаковое деление (усечение к нулю).
//!   Синтезируется в аппаратный делитель, о чём цель предупреждает.
//!
//! Знаковость восстанавливается на каждом уровне `$signed(...)`: операнды - `logic
//! signed`, но подвыражения-строки теряют её при вложении.

use crate::diagnostics::{Diagnostic, Location};
use crate::generator::sv::sv_expr::{Scope, print_expression, sv002};
use crate::semantic::type_node::TypeNode;
use crate::semantic::{ExpressionNode, VariableNode};

/// Арифметическая операция над `q(m, n)`.
#[derive(Clone, Copy)]
pub(crate) enum FixedOp {
    Add,
    Subtract,
    Multiply,
    Divide,
}

/// Формат `q(m, n)` выражения, если его тип - `Fixed` (рекурсивно по арифметике;
/// `SE-059` гарантирует единый формат операндов).
pub(crate) fn fixed_format_in(
    expr: &ExpressionNode,
    structs: &std::collections::BTreeMap<String, Vec<(String, TypeNode)>>,
) -> Option<(u8, u8, bool)> {
    if let Some(found) = fixed_format(expr) {
        return Some(found);
    }
    // Поле структуры: его тип объявлен в модели, и без него `g.kp as u8` печаталось без
    // масштабирования - замер 2026-08-21 дал у эталона `1`, а у цели **128**, молча и
    // при нулевом коде возврата.
    match expr {
        ExpressionNode::Parenthesis(a) | ExpressionNode::Negate(a) => fixed_format_in(a, structs),
        ExpressionNode::Add(a, b)
        | ExpressionNode::Subtract(a, b)
        | ExpressionNode::Multiply(a, b)
        | ExpressionNode::Divide(a, b) => {
            fixed_format_in(a, structs).or_else(|| fixed_format_in(b, structs))
        }
        ExpressionNode::BitAccess(inner, crate::parser::ast::Member::Identifier(field)) => {
            match field_type(inner, &field.name, structs)? {
                TypeNode::Fixed { m, n, sat } => Some((m, n, sat)),
                _ => None,
            }
        }
        _ => None,
    }
}

/// Тип поля структуры по выражению-базе.
///
/// Спуск по типу (`переменная(.поле | [индекс])*`) делает общий носитель
/// `validate::base_type_with`: у цели `sv` модели нет, она носит снимок карты, поэтому
/// объявления передаются замыканием. Своего разбора цепочки здесь нет - он разошёлся бы
/// с тем, которым живут `SE-061`, `SE-028`, `SE-030` и печать среза.
fn field_type(
    base: &ExpressionNode,
    field: &str,
    structs: &std::collections::BTreeMap<String, Vec<(String, TypeNode)>>,
) -> Option<TypeNode> {
    let fields_of = |name: &str| structs.get(name).cloned();
    let base_ty = crate::semantic::validate::base_type::base_type_with(base, &fields_of)?;
    let TypeNode::Struct(name) = base_ty else {
        return None;
    };
    structs
        .get(&name)?
        .iter()
        .find(|(f, _)| f == field)
        .map(|(_, ty)| ty.clone())
}

pub(crate) fn fixed_format(expr: &ExpressionNode) -> Option<(u8, u8, bool)> {
    match expr {
        ExpressionNode::Variable(var) => var_fixed(&var.borrow()),
        // Признак насыщения едет вместе с разрядностями.
        ExpressionNode::Cast(_, TypeNode::Fixed { m, n, sat }) => Some((*m, *n, *sat)),
        ExpressionNode::Parenthesis(a) | ExpressionNode::Negate(a) => fixed_format(a),
        ExpressionNode::Add(a, b)
        | ExpressionNode::Subtract(a, b)
        | ExpressionNode::Multiply(a, b)
        | ExpressionNode::Divide(a, b) => fixed_format(a).or_else(|| fixed_format(b)),
        _ => None,
    }
}

/// Формат `q(m, n)` переменной, если её тип - `Fixed`.
fn var_fixed(var: &VariableNode) -> Option<(u8, u8, bool)> {
    match var.ty() {
        TypeNode::Fixed { m, n, sat } => Some((*m, *n, *sat)),
        _ => None,
    }
}

/// Прижимает выражение шириной `w2` к границам представления `intW`.
///
/// Считается в **удвоенной** ширине: у `logic signed [W-1:0]` места под промежуток нет,
/// и сравнение с границами шло бы уже по обёрнутому значению.
///
/// Подвыражение повторяется трижды (сравнение сверху, снизу, значение). Это цена
/// отсутствия хелперов у цели `sv`: инфраструктуры эмиссии функций рядом с арифметикой
/// нет, а вводить её ради одной формы дороже, чем повтор, который синтезатор схлопывает
/// в общий узел. Синтезируемость доказывают оба проверки (verilator + yosys).
fn saturate_sv(inner: &str, w: u32, w2: u32) -> String {
    let max = (1i64 << (w - 1)) - 1;
    let min_abs = 1i64 << (w - 1);
    format!(
        "({w}'((({inner}) > {w2}'sd{max}) ? {w2}'sd{max}          : ((({inner}) < -{w2}'sd{min_abs}) ? -{w2}'sd{min_abs} : ({inner}))))"
    )
}

/// Знаковое W-битное значение операнда: `$signed(<printed>)`.
fn signed(printed: &str) -> String {
    format!("$signed({printed})")
}

/// Печатает бинарную q-операцию. Результат - `W'(...)` (wraparound к W).
pub(crate) fn binary(
    op: FixedOp,
    l: &ExpressionNode,
    r: &ExpressionNode,
    scope: &Scope,
    m: u8,
    n: u8,
    sat: bool,
) -> Result<String, Diagnostic> {
    let w = (m + n) as u32;
    let w2 = 2 * w;
    let (la, lb) = (print_expression(l, scope)?, print_expression(r, scope)?);
    if sat {
        // Промежуток - в 2W, прижатие - к границам W.
        let inner = match op {
            FixedOp::Add => format!("{w2}'({}) + {w2}'({})", signed(&la), signed(&lb)),
            FixedOp::Subtract => format!("{w2}'({}) - {w2}'({})", signed(&la), signed(&lb)),
            FixedOp::Multiply => {
                format!("({w2}'({}) * {w2}'({})) >>> {n}", signed(&la), signed(&lb))
            }
            FixedOp::Divide => format!("({w2}'({}) <<< {n}) / {w2}'({})", signed(&la), signed(&lb)),
        };
        return Ok(saturate_sv(&inner, w, w2));
    }
    match op {
        FixedOp::Add => Ok(format!("({w}'({} + {}))", signed(&la), signed(&lb))),
        FixedOp::Subtract => Ok(format!("({w}'({} - {}))", signed(&la), signed(&lb))),
        // Точное произведение 2W -> floor к −∞ арифметическим `>>>` ().
        FixedOp::Multiply => Ok(format!(
            "({w}'((({w2}'({}) * {w2}'({})) >>> {n})))",
            signed(&la),
            signed(&lb)
        )),
        // Делимое <- n влево в 2W, знаковое деление (усечение к нулю, как сим).
        FixedOp::Divide => Ok(format!(
            "({w}'((({w2}'({}) <<< {n}) / {w2}'({}))))",
            signed(&la),
            signed(&lb)
        )),
    }
}

/// Печатает унарный минус над `q(m, n)`: `−repr` с wraparound к W.
pub(crate) fn negate(
    inner: &ExpressionNode,
    scope: &Scope,
    m: u8,
    n: u8,
    sat: bool,
) -> Result<String, Diagnostic> {
    let w = (m + n) as u32;
    let printed = signed(&print_expression(inner, scope)?);
    if sat {
        // Край `−(−2^(W−1))`: в 2W он представим, и прижатие даёт максимум.
        let w2 = 2 * w;
        return Ok(saturate_sv(&format!("-{w2}'({printed})"), w, w2));
    }
    Ok(format!("({w}'(-{printed}))"))
}

/// Печатает приведение `expr as T`, когда источник **или** цель - `q(m, n)`.
///
/// `q ↔ int` масштабируют сдвигом на `2ⁿ` (в SV `>>>`/`<<<` знакового определены). `q ↔
/// float` невозможно: в синтезируемом RTL плавающей точки нет -> `SV-003` (: `float` в
/// цели `sv` остаётся запрещён).
pub(crate) fn cast(
    inner: &ExpressionNode,
    target: &TypeNode,
    scope: &Scope,
) -> Result<String, Diagnostic> {
    // Литерал -> q: значение известно при компиляции.
    //
    // Проверка стоит до печати источника: вещественный литерал сам по себе отвергается
    // `SV-002` ("в синтезируемом RTL плавающей точки нет"), и печать пришла бы раньше -
    // отказ на записи, которую эталон исполняет. Плавающей точки в выводе не
    // появляется: печатается уже посчитанное целое представление (счёт - у общего
    // носителя `const_eval::fixed_literal`).
    if let Some(repr) = crate::semantic::const_eval::fixed_literal::cast_repr(inner, target) {
        let w = match target {
            TypeNode::Fixed { m, n, .. } => (m + n) as u32,
            _ => unreachable!("cast_repr отвечает только на цель q(m, n)"),
        };
        return Ok(sized_signed_literal(repr, w));
    }
    // Формат источника - с оглядкой на объявления структур.
    let src = fixed_format_in(inner, scope.structs);
    let printed = print_expression(inner, scope)?;
    match (src, target) {
        // q -> q: пересчёт дробных разрядов (влево - сдвиг, вправо - floor `>>>`).
        (Some((_, from_n, _)), TypeNode::Fixed { m: tm, n: tn, sat }) => {
            let tw = (tm + tn) as u32;
            let inner_expr = if tn >= &from_n {
                format!("{} <<< {}", signed(&printed), tn - from_n)
            } else {
                format!("{} >>> {}", signed(&printed), from_n - tn)
            };
            if *sat {
                let tw2 = 2 * tw;
                return Ok(saturate_sv(&format!("{tw2}'({inner_expr})"), tw, tw2));
            }
            Ok(format!("({tw}'({inner_expr}))"))
        }
        // q ↔ float - недопустимо в синтезируемом RTL.
        (Some(_), TypeNode::Rational) => Err(sv003_cast()),
        // q -> целое/бит: floor(repr / 2ⁿ) арифметическим сдвигом.
        (Some((_, from_n, _)), _) => {
            let bits = int_bits(target)?;
            Ok(format!("({bits}'({} >>> {from_n}))", signed(&printed)))
        }
        // float -> q - источника float в синтезируемом RTL нет.
        (None, TypeNode::Fixed { .. }) if is_rational(inner) => Err(sv003_cast()),
        // целое/бит -> q: repr = v · 2ⁿ с wraparound к W.
        (None, TypeNode::Fixed { m: tm, n: tn, sat }) => {
            let w = (tm + tn) as u32;
            if *sat {
                let w2 = 2 * w;
                return Ok(saturate_sv(
                    &format!("{w2}'({}) <<< {tn}", signed(&printed)),
                    w,
                    w2,
                ));
            }
            Ok(format!("({w}'({} <<< {tn}))", signed(&printed)))
        }
        // Ни источник, ни цель не q - вызывающий не должен был звать сюда.
        (None, _) => Err(sv002("приведение типа (`as`)")),
    }
}

/// Разрядность целого/битового целевого типа приведения.
fn int_bits(target: &TypeNode) -> Result<u32, Diagnostic> {
    match target {
        TypeNode::Integer { bits, .. } => Ok(*bits as u32),
        TypeNode::Bit | TypeNode::Bool => Ok(1),
        _ => Err(sv002("приведение q → нецелого типа")),
    }
}

/// Истина, если тип источника приведения - вещественный (`float`).
fn is_rational(expr: &ExpressionNode) -> bool {
    match expr {
        ExpressionNode::Rational(_, _) => true,
        ExpressionNode::Variable(var) => matches!(var.borrow().ty(), TypeNode::Rational),
        ExpressionNode::Parenthesis(a) => is_rational(a),
        _ => false,
    }
}

/// `SV-003` - приведение между `q` и `float`: плавающей точки в RTL нет.
fn sv003_cast() -> Diagnostic {
    Diagnostic::error(
        Location::Codegen,
        "приведение между q(m, n) и float в цели 'sv': в синтезируемом RTL \
         плавающей точки нет (SV-003 для float в силе)"
            .to_string(),
    )
    .with_code("SV-003")
}

/// Знаковый литерал представления шириной `w` - форма, принятая обоими инструментами
/// SV.
///
/// Ширина указывается явно: безразмерный литерал внутри выражения даёт
/// `WIDTHEXPAND`/`WIDTHCONCAT`, а проверка цели считает предупреждение ошибкой.
/// Отрицательное значение печатается через унарный минус - размерный литерал в SV
/// беззнаков.
fn sized_signed_literal(repr: i64, w: u32) -> String {
    if repr < 0 {
        format!("({w}'(-{w}'d{}))", repr.unsigned_abs())
    } else {
        format!("{w}'d{repr}")
    }
}
