//! Печать `match` целью `rust` - цепочка `if`/`else if` (выделено ).
//!
//! Образцы Takt - произвольные выражения, а `match x { y => ... }` в Rust связал бы `y`
//! как новое имя вместо сравнения с ним: молчаливое связывание дало бы всегда-истинную
//! первую ветку, то есть тихо неверный автомат. Поэтому здесь печатается цепочка
//! условий.
//!
//! Модуль выделен по границе ответственности: печать `match` знает про пустую ветвь,
//! про слияние вложенного `if` и про порядок ветвей - это отдельный предмет, а не часть
//! общего печатника операторов.

use crate::diagnostics::Diagnostic;
use crate::generator::indent::Printer;
use crate::generator::rust::rust_expr::{Scope, print_as_bool, print_expression};
use crate::generator::rust::rust_stmt::{StmtOutput, print_statement};
use crate::semantic::{ExpressionNode, MatchArmNode, MatchPatternNode, StatementNode};

/// Печатает `match` цепочкой `if`/`else if`.
pub(crate) fn print_match(
    expr: &ExpressionNode,
    arms: &[MatchArmNode],
    scope: &mut Scope,
    p: &mut Printer,
    out: &mut StmtOutput,
) -> Result<usize, Diagnostic> {
    let subject = print_expression(expr, scope)?;
    // Образец сравнивается с типом разбираемого выражения: `match mode { 0 => ... }`
    // при `mode : Mode` приходит числом, и без обратного отображения получилось бы
    // `self.mode == 0` - сравнение перечисления с целым.
    let subject_type = crate::generator::rust::rust_expr::expression_type(expr);
    let mut first = true;
    let mut wildcard: Option<&StatementNode> = None;
    for arm in arms {
        // Берётся первая `_`-ветвь: вторая недостижима, а прежде цепочка печаталась по
        // последней - то есть исполнялось не то, что считает эталон.
        if wildcard.is_none()
            && arm
                .patterns
                .iter()
                .any(|p| matches!(p, MatchPatternNode::Wildcard))
        {
            wildcard = Some(&arm.body);
        }
    }
    // Тело `_`-ветки печатается В буфер: пустая ветвь (`_ => {}` - "прочие значения
    // ничего не делают", запись из практики) давала `} else { }`, а `clippy` под `-D
    // warnings` отвечает "this `else` branch is empty" - отказ проверки самой цели при
    // нулевом коде возврата `taktc`. Тот же приём, что у пустого `if`.
    //
    // Буфер заполняется до цепочки: печатать ли `_`-ветвь, знать нужно раньше - от
    // этого зависит, можно ли слить последнюю ветвь с её вложенным `if`.
    let mut wildcard_text = String::new();
    if let Some(body) = wildcard {
        let mut buffer = p.fork(&mut wildcard_text);
        buffer.up();
        print_statement(body, scope, &mut buffer, out)?;
        buffer.down();
    }
    // Индекс последней ветви цепочки: только её тело можно сливать с условием - после
    // неё `else` нет, и "провал" никуда не ведёт.
    let last_printed = arms.iter().rposition(|arm| {
        arm.patterns
            .iter()
            .any(|p| matches!(p, MatchPatternNode::Value(_)))
    });
    for (index, arm) in arms.iter().enumerate() {
        if arm
            .patterns
            .iter()
            .any(|p| matches!(p, MatchPatternNode::Wildcard))
        {
            continue;
        }
        // Ветвь, чей образец повторяет более ранний, недостижима: `match` берёт первое
        // совпадение. `clippy` под `-D warnings` отвечает "these `if` branches have the
        // same condition" - отказ проверки самой цели при нулевом коде возврата `taktc`.
        if crate::semantic::match_arms::pattern_repeats_above(arms, index) {
            continue;
        }
        let mut tests = Vec::new();
        for pattern in &arm.patterns {
            let MatchPatternNode::Value(value) = pattern else {
                continue;
            };
            let printed = match &subject_type {
                Some(ty) => crate::generator::rust::rust_expr::coerce_to(value, ty, scope)?,
                None => print_expression(value, scope)?,
            };
            tests.push(format!("{} == {}", subject, printed));
        }
        if tests.is_empty() {
            continue;
        }
        // Слияние вложенного `if` с условием ветви - только у последней ветви и только
        // когда `_`-ветви в выводе нет. Иначе слияние изменило бы автомат: при истинном
        // образце и ложном внутреннем условии управление ушло бы в следующую ветвь,
        // тогда как вложенный `if` просто ничего не делает.
        let mergeable = Some(index) == last_printed && wildcard_text.trim().is_empty();
        let (extra, body) = if mergeable {
            unwrap_if_chain(&arm.body)
        } else {
            (Vec::new(), arm.body.as_ref())
        };
        let mut guard = tests.join(" || ");
        if !extra.is_empty() {
            if tests.len() > 1 {
                guard = format!("({guard})");
            }
            for cond in &extra {
                guard = format!("{guard} && {}", print_as_bool(cond, scope)?);
            }
        }
        // Тело печатается В буфер: пустая ветвь образца даёт `if x { }`, а `clippy` под
        // `-D warnings` отвечает "this `if` branch is empty".
        let mut body_text = String::new();
        {
            let mut buffer = p.fork(&mut body_text);
            buffer.up();
            print_statement(body, scope, &mut buffer, out)?;
            buffer.down();
        }
        if body_text.trim().is_empty() {
            continue;
        }
        let head = if first { "if" } else { "} else if" };
        first = false;
        p.ident(&format!("{head} {guard} {{")).nl();
        p.print(&body_text);
    }
    match (wildcard_text.trim().is_empty(), first) {
        // Только `_`-ветка: цепочки нет, тело печатается как есть.
        (false, true) => {
            p.print(&wildcard_text);
        }
        (false, false) => {
            p.ident("} else {").nl();
            p.print(&wildcard_text);
            p.ident("}").nl();
        }
        (true, false) => {
            p.ident("}").nl();
        }
        (true, true) => {}
    }
    Ok(0)
}

/// Раскручивает цепочку `if a { if b { ... } }` в список условий.
///
/// Возвращает условия по порядку, тело самого внутреннего `if` и ветвь `else` внешнего.
/// Слияние идёт, пока выполняются все условия правила:
///
/// - у внешнего `if` нет `else` (иначе `clippy` схлопывания не требует);
/// - тело - ровно один оператор, и это `if`;
/// - у внутреннего `if` нет `else`.
///
/// Границы замерены прогоном `clippy`: на всех трёх соседних формах линт
/// молчит, и трогать их значило бы менять вывод без повода.
///
/// Условие в Takt эффектов не имеет (присваивание - оператор, 0187), поэтому конъюнкция
/// вычисляет то же самое; порядок проверок сохранён - `&&` в Rust ленив, как и
/// вложенный `if`.
pub(crate) fn collapse_nested_if<'a>(
    cond: &'a ExpressionNode,
    then_: &'a StatementNode,
    else_: &'a Option<Box<StatementNode>>,
) -> (
    Vec<&'a ExpressionNode>,
    &'a StatementNode,
    &'a Option<Box<StatementNode>>,
) {
    let mut conds = vec![cond];
    if else_.is_some() {
        return (conds, then_, else_);
    }
    let (inner, body) = unwrap_if_chain(then_);
    conds.extend(inner);
    (conds, body, else_)
}

/// Раскручивает тело, состоящее ровно из одного `if` без `else`.
///
/// Возвращает условия раскрученных `if` по порядку и тело самого внутреннего. Пустой
/// список означает "раскручивать нечего" - тело печатается как есть.
fn unwrap_if_chain(body: &StatementNode) -> (Vec<&ExpressionNode>, &StatementNode) {
    let mut conds = Vec::new();
    let mut current = body;
    loop {
        let inner = match current {
            StatementNode::Block(items) if items.len() == 1 => &items[0],
            other => other,
        };
        let StatementNode::If {
            cond,
            then_,
            else_: None,
            ..
        } = inner
        else {
            break;
        };
        conds.push(cond.as_ref());
        current = then_;
    }
    (conds, current)
}
