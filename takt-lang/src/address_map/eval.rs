//! Вычислитель выражения адреса: свёртка в константу `(addr, bit)`.
//!
//! Тема самостоятельна и держит **инвариант единственной арифметики**: [`apply_binary`]
//! обслуживает **оба** матчера (сырой АСД у оператора `address` и понижённый
//! [`ExpressionNode`] у inline). `CLAUDE.md` предупреждает - "разъехавшись, они дали бы
//! **разный адрес для одного текста**".

use super::env::AddressEnv;
use crate::diagnostics::{Diagnostic, Location};
use crate::semantic::{ExpressionNode, ModelNode, VariableNode};
use std::cell::RefCell;
use std::rc::Rc;

/// Значение выражения адреса: число и необязательный бит (`0xADDR:bit`).
type AddrValue = (i64, Option<i64>);

/// `SE-054` - имя в выражении адреса не разрешается ни define'ом, ни `const`.
fn undefined_symbol(loc: Location, name: &str) -> Diagnostic {
    Diagnostic::error(
        loc,
        format!(
            "выражение адреса ссылается на неопределённый символ '{}' \
             (нет ни `--define {}=…`, ни `const {}` в модели)",
            name, name, name
        ),
    )
    .with_code("SE-054")
}

/// `SE-055` - выражение адреса не сворачивается в константу.
fn not_constant(loc: Location, reason: &str) -> Diagnostic {
    Diagnostic::error(
        loc,
        format!("выражение адреса не сворачивается в константу: {}", reason),
    )
    .with_code("SE-055")
}

/// Применяет бинарную операцию к вычисленным операндам.
///
/// Единственное место арифметики адреса - им пользуются **оба** матчера (по
/// [`ExpressionNode`] и по сырому АСД). Разъехавшись, они дали бы разный адрес для
/// одного и того же текста в зависимости от того, каким путём выражение попало в
/// разрешение (inline против оператора `address`).
///
/// **Сами правила арифметики живут выше** - в
/// [`int_ops`](crate::semantic::const_eval::int_ops), общей таблице константного
/// вычисления. Здесь остаётся только то, что принадлежит **адресу**: запрет операнда
/// формы `адрес:бит`, сужение к `i64` и собственные тексты диагностик (`SE-055`).
///
/// Сужение `as i64` **равносильно** прежней обёртке `wrapping_*` по 64 битам (тест -
/// тест `narrowing_to_i64_matches_64bit_wrapping` в `int_ops`).
///
/// Сравнения и логика в выражении адреса **не поддержаны**, и отсекают их не здесь, а
/// **матчеры** (`eval_ast_addr`/`eval_node_addr`): они принимают только арифметику и
/// побитовые операции, а прочее отвергают, называя список допустимых форм. Ветвь `Bool`
/// ниже - защита в глубину: если матчер когда-то пропустит сравнение, адресом булево
/// всё равно не станет.
fn apply_binary(
    op: &str,
    left: AddrValue,
    right: AddrValue,
    loc: Location,
) -> Result<AddrValue, Diagnostic> {
    use crate::semantic::const_eval::int_ops::{IntOpError, IntOutcome, int_binary};
    // Бит - свойство записи адреса (`0x1000:3`), а не число: арифметика над ним
    // бессмысленна, и молча его терять нельзя.
    if left.1.is_some() || right.1.is_some() {
        return Err(not_constant(
            loc,
            "операнд формы `адрес:бит` не участвует в арифметике",
        ));
    }
    match int_binary(op, i128::from(left.0), i128::from(right.0)) {
        Ok(IntOutcome::Int(v)) => Ok((v as i64, None)),
        // Булево значение адресом быть не может.
        Ok(IntOutcome::Bool(_)) | Err(IntOpError::UnsupportedOperator) => Err(not_constant(
            loc,
            "операция не поддержана в выражении адреса",
        )),
        Err(IntOpError::DivisionByZero) => Err(not_constant(loc, "деление на ноль")),
        Err(IntOpError::RemainderByZero) => Err(not_constant(loc, "остаток от деления на ноль")),
        Err(IntOpError::ShiftOutOfRange) => {
            Err(not_constant(loc, "сдвиг допустим только на 0..63 бит"))
        }
        // Степень до выражения адреса не доходит: форму `Power` матчер не разбирает
        // вовсе. Ветвь - защита в глубину, как соседняя `Bool` выше.
        Err(IntOpError::ExponentOutOfRange) => Err(not_constant(
            loc,
            "показатель степени отрицателен либо шире 32 бит",
        )),
    }
}

/// Вычисляет **сырое АСД**-выражение адреса - путь оператора `address`.
///
/// Значение оператора хранится как `ExpressionNode::Unresolved(ast::Expression)` и
/// семантикой не понижается (`tree.rs`): ни один из 7 проходов к `address_defs` не
/// обращается. Поэтому здесь разбирается АСД напрямую.
fn eval_ast_addr(
    expr: &crate::parser::ast::Expression,
    scope: &Rc<RefCell<ModelNode>>,
    env: &AddressEnv,
    seen: &mut Vec<String>,
) -> Result<AddrValue, Diagnostic> {
    use crate::parser::ast::Expression as E;
    // Бинарная операция разворачивается явно: замыкание заимствовало бы `seen`
    // изменяемо на всё тело match.
    macro_rules! bin {
        ($op:literal, $l:expr, $r:expr, $loc:expr) => {{
            let left = eval_ast_addr($l, scope, env, seen)?;
            let right = eval_ast_addr($r, scope, env, seen)?;
            apply_binary($op, left, right, $loc)
        }};
    }
    match expr {
        E::Number(loc, n) => Ok((narrow_addr_literal(*n, *loc)?, None)),
        E::Address(_, a, b) => Ok((*a, Some(*b))),
        E::Variable(id) => resolve_symbol(&id.name, id.loc, scope, env, seen),
        E::Parenthesis(_, inner) => eval_ast_addr(inner, scope, env, seen),
        E::UnaryPlus(_, inner) => eval_ast_addr(inner, scope, env, seen),
        E::Negate(loc, inner) => {
            let (v, bit) = eval_ast_addr(inner, scope, env, seen)?;
            unary_bitless(v.wrapping_neg(), bit, *loc)
        }
        E::BitwiseNot(loc, inner) => {
            let (v, bit) = eval_ast_addr(inner, scope, env, seen)?;
            unary_bitless(!v, bit, *loc)
        }
        E::Add(loc, l, r) => bin!("+", l, r, *loc),
        E::Subtract(loc, l, r) => bin!("-", l, r, *loc),
        E::Multiply(loc, l, r) => bin!("*", l, r, *loc),
        E::Divide(loc, l, r) => bin!("/", l, r, *loc),
        E::Modulo(loc, l, r) => bin!("%", l, r, *loc),
        E::ShiftLeft(loc, l, r) => bin!("<<", l, r, *loc),
        E::ShiftRight(loc, l, r) => bin!(">>", l, r, *loc),
        E::BitwiseAnd(loc, l, r) => bin!("&", l, r, *loc),
        E::BitwiseOr(loc, l, r) => bin!("|", l, r, *loc),
        E::BitwiseXor(loc, l, r) => bin!("^", l, r, *loc),
        other => Err(not_constant(
            other.loc(),
            "поддержаны только целочисленные литералы, символы и операции \
             `+ - * / % << >> & | ^ ~`",
        )),
    }
}

/// Сужает литерал (`i128`) до носителя адреса (`i64`).
///
/// Адрес и бит живут в `i64`, а литерал шире: `u64`-маска законна как значение, но
/// адресом быть не может. Сужение - явный отказ `SE-055`: молчаливое `as i64` дало бы
/// адрес, которого автор не писал.
fn narrow_addr_literal(value: i128, loc: Location) -> Result<i64, Diagnostic> {
    i64::try_from(value).map_err(|_| {
        not_constant(
            loc,
            "числовой литерал не помещается в знаковое 64-битное значение адреса",
        )
    })
}

/// Унарная операция: бит в ней бессмыслен - см.
fn unary_bitless(value: i64, bit: Option<i64>, loc: Location) -> Result<AddrValue, Diagnostic> {
    if bit.is_some() {
        return Err(not_constant(
            loc,
            "операнд формы `адрес:бит` не участвует в арифметике",
        ));
    }
    Ok((value, None))
}

/// Разрешает имя в выражении адреса: **сначала** define, **затем** `const` модели.
///
/// Порядок: платформенный слой главнее модели (прямая симметрия с наложением
/// внешней карты `SE-050`). Предупреждение о перекрытии (`SE-053`) выдаёт вызывающий:
/// здесь нет позиции объявления `const`.
fn resolve_symbol(
    name: &str,
    loc: Location,
    scope: &Rc<RefCell<ModelNode>>,
    env: &AddressEnv,
    seen: &mut Vec<String>,
) -> Result<AddrValue, Diagnostic> {
    if let Some(v) = env.lookup(name) {
        // Define побеждает `const`, но перекрытие обязано быть заметным -
        // прямая симметрия с наложением внешней карты (`SE-050`): платформенный слой
        // главнее модели, однако молча её не подменяет.
        if let Some(VariableNode::Const { loc: const_loc, .. }) = scope.borrow().search_var(name) {
            env.note_override(name, const_loc);
        }
        return Ok(v);
    }
    // Цикл `const A := B; const B := A;` - иначе вычислитель зациклится.
    if seen.iter().any(|s| s == name) {
        return Err(not_constant(
            loc,
            &format!("циклическая ссылка через '{}'", name),
        ));
    }
    let Some(var) = scope.borrow().search_var(name) else {
        return Err(undefined_symbol(loc, name));
    };
    let VariableNode::Const { expr, .. } = &var else {
        // Параметр модели - величина, которая станет константой в режиме
        // `--parameters=specialize`: диагностика **называет режим**, иначе автор читал
        // бы "не константа" о том, что константой быть может.
        if crate::semantic::parameter_const::is_parameter(&var) {
            return Err(crate::semantic::parameter_const::compile_time_parameter(
                loc,
                name,
                "адрес порта",
            ));
        }
        // Переменная или порт: их значение известно только в времени выполнения.
        return Err(not_constant(
            loc,
            &format!(
                "'{}' — не константа (адрес обязан быть известен при сборке)",
                name
            ),
        ));
    };
    seen.push(name.to_string());
    let value = eval_addr_value(expr, scope, env, seen);
    seen.pop();
    value
}

/// Вычисляет выражение адреса, уже понижённое семантикой, - путь inline.
fn eval_addr_value(
    expr: &ExpressionNode,
    scope: &Rc<RefCell<ModelNode>>,
    env: &AddressEnv,
    seen: &mut Vec<String>,
) -> Result<AddrValue, Diagnostic> {
    macro_rules! bin {
        ($op:literal, $l:expr, $r:expr) => {{
            let left = eval_addr_value($l, scope, env, seen)?;
            let right = eval_addr_value($r, scope, env, seen)?;
            apply_binary($op, left, right, Location::Implicit)
        }};
    }
    match expr {
        ExpressionNode::Number(n) => Ok((narrow_addr_literal(*n, Location::Implicit)?, None)),
        ExpressionNode::Address(a, b) => Ok((*a, Some(*b))),
        // Сырой АСД: значение оператора `address` семантикой не понижается.
        ExpressionNode::Unresolved(ast_expr) => eval_ast_addr(ast_expr, scope, env, seen),
        // Имя, разрешённое семантикой в объявление (путь inline).
        ExpressionNode::Variable(var_rc) => {
            let borrowed = var_rc.borrow();
            let name = borrowed.name().to_string();
            let loc = borrowed.loc();
            drop(borrowed);
            resolve_symbol(&name, loc, scope, env, seen)
        }
        ExpressionNode::Parenthesis(inner) => eval_addr_value(inner, scope, env, seen),
        ExpressionNode::UnaryPlus(inner) => eval_addr_value(inner, scope, env, seen),
        ExpressionNode::Negate(inner) => {
            let (v, bit) = eval_addr_value(inner, scope, env, seen)?;
            unary_bitless(v.wrapping_neg(), bit, Location::Implicit)
        }
        ExpressionNode::BitwiseNot(inner) => {
            let (v, bit) = eval_addr_value(inner, scope, env, seen)?;
            unary_bitless(!v, bit, Location::Implicit)
        }
        ExpressionNode::Add(l, r) => bin!("+", l, r),
        ExpressionNode::Subtract(l, r) => bin!("-", l, r),
        ExpressionNode::Multiply(l, r) => bin!("*", l, r),
        ExpressionNode::Divide(l, r) => bin!("/", l, r),
        ExpressionNode::Modulo(l, r) => bin!("%", l, r),
        ExpressionNode::ShiftLeft(l, r) => bin!("<<", l, r),
        ExpressionNode::ShiftRight(l, r) => bin!(">>", l, r),
        ExpressionNode::BitwiseAnd(l, r) => bin!("&", l, r),
        ExpressionNode::BitwiseOr(l, r) => bin!("|", l, r),
        ExpressionNode::BitwiseXor(l, r) => bin!("^", l, r),
        _ => Err(not_constant(
            Location::Implicit,
            "поддержаны только целочисленные литералы, символы и операции \
             `+ - * / % << >> & | ^ ~`",
        )),
    }
}

/// Вычисляет выражение адреса в константу `(addr, bit)`.
///
/// # Чем отличается от прежнего понижения
///
///
/// ```text
/// _ => None,   // символ, арифметика, всё прочее - "адреса нет", без диагностики
/// ```
///
/// То есть `address BTN = BTN_ADDR;` и `address BTN = 0x100000 + 4;` **молча теряли
/// адрес**, а пользователь получал `SE-052` "порт не имеет адреса" - диагностику,
/// называющую следствие вместо причины.
///
/// Теперь неудача **всегда** названа: `SE-054` (неопределённый символ) либо `SE-055`
/// (не сворачивается в константу).
///
/// # Возврат
///
/// - `Ok(None)` - выражения адреса нет вовсе (порт без инициализатора и без
///   оператора `address`); полноту проверяет вызывающий (`SE-052`).
/// - `Ok(Some(v))` - вычислено.
/// - `Err(d)` - `SE-054`/`SE-055`.
pub(super) fn eval_addr_expr(
    expr: &ExpressionNode,
    scope: &Rc<RefCell<ModelNode>>,
    env: &AddressEnv,
) -> Result<Option<AddrValue>, Diagnostic> {
    if matches!(expr, ExpressionNode::None) {
        return Ok(None);
    }
    let mut seen = Vec::new();
    eval_addr_value(expr, scope, env, &mut seen).map(Some)
}
