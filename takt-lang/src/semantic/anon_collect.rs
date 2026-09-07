//! Сбор анонимных обращений модели - `#0x346619:0 as u64`.

use crate::diagnostics::{Diagnostic, Location};
use crate::semantic::anon_port::AnonPortAccess;
use crate::semantic::{
    ConditionNode, ExpressionNode, FunctionDefinitionNode, ModelNode, StateNode, StatementNode,
};
use std::cell::RefCell;
use std::collections::BTreeSet;
use std::rc::Rc;

/// Ячейка в порядке эмиссии: адрес, затем бит, затем ширина.
///
/// Ключ сортировки отдельным типом, потому что `TypeNode` порядка не имеет, а заводить
/// его ради одного потребителя незачем.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Key(i64, i64, u16);

/// Собирает все ячейки, к которым обращается модель и её под-модели.
pub fn collect_anon_ports(model: &Rc<RefCell<ModelNode>>) -> Vec<AnonPortAccess> {
    collect(model, true)
}

/// Собирает ячейки **только этой** модели, без под-моделей.
///
/// Нужна цели `st-at`: `VAR_EXTERNAL` объявляется у того `FUNCTION_BLOCK`, который к
/// ячейке обращается, а у под-модели блок свой.
pub fn collect_anon_ports_local(model: &Rc<RefCell<ModelNode>>) -> Vec<AnonPortAccess> {
    collect(model, false)
}

/// Собирает ячейки одной модели по ссылке на узел (без `Rc`).
///
/// Нужна печатнику объявлений `st-at`: он держит `&ModelNode`, а не `Rc`.
pub fn collect_anon_ports_local_node(model: &ModelNode) -> Vec<AnonPortAccess> {
    let mut found: BTreeSet<(Key, String)> = BTreeSet::new();
    let mut cells: Vec<AnonPortAccess> = Vec::new();
    walk_model_node(model, false, &mut found, &mut cells);
    finish(cells)
}

fn collect(model: &Rc<RefCell<ModelNode>>, recursive: bool) -> Vec<AnonPortAccess> {
    let mut found: BTreeSet<(Key, String)> = BTreeSet::new();
    let mut cells: Vec<AnonPortAccess> = Vec::new();
    walk_model(model, recursive, &mut found, &mut cells);
    finish(cells)
}

/// Приводит собранное к детерминированному порядку.
///
/// Порядок - по ключу, а не по встрече: эмиссия обязана быть одинаковой при любых
/// перестановках в исходнике.
fn finish(mut cells: Vec<AnonPortAccess>) -> Vec<AnonPortAccess> {
    cells.sort_by_key(|a| Key(a.addr, a.bit, a.width_bits()));
    cells.dedup_by_key(|a| Key(a.addr, a.bit, a.width_bits()));
    cells
}

fn note(
    access: &AnonPortAccess,
    found: &mut BTreeSet<(Key, String)>,
    out: &mut Vec<AnonPortAccess>,
) {
    let key = (
        Key(access.addr, access.bit, access.width_bits()),
        access.ty.to_string(),
    );
    if found.insert(key) {
        out.push(access.clone());
    }
}

fn walk_model(
    model: &Rc<RefCell<ModelNode>>,
    recursive: bool,
    found: &mut BTreeSet<(Key, String)>,
    out: &mut Vec<AnonPortAccess>,
) {
    let borrowed = model.borrow();
    walk_model_node(&borrowed, recursive, found, out);
}

fn walk_model_node(
    model: &ModelNode,
    recursive: bool,
    found: &mut BTreeSet<(Key, String)>,
    out: &mut Vec<AnonPortAccess>,
) {
    let (funcs, blocks, states, conds, nested) = (
        model.functions.values().cloned().collect::<Vec<_>>(),
        model.named_blocks.clone(),
        model.states.values().cloned().collect::<Vec<_>>(),
        model.conditions.values().cloned().collect::<Vec<_>>(),
        model.models.values().map(Rc::clone).collect::<Vec<_>>(),
    );
    for func in &funcs {
        if let FunctionDefinitionNode::Local { body, .. } = func {
            walk_stmt(body, found, out);
        }
    }
    for block in &blocks {
        if let Some(stmt) = block.statement() {
            walk_stmt(stmt, found, out);
        }
    }
    for cond in &conds {
        walk_cond(&cond.value, found, out);
    }
    for state in &states {
        let (named_blocks, references) = match state {
            StateNode::Simple {
                named_blocks,
                references,
                ..
            }
            | StateNode::Implement {
                named_blocks,
                references,
                ..
            } => (named_blocks, references),
            StateNode::Unresolved => continue,
        };
        for block in named_blocks {
            if let Some(stmt) = block.statement() {
                walk_stmt(stmt, found, out);
            }
        }
        // Условия рёбер: `ref Next: #0x100.0;` - такое же обращение, как в теле.
        for reference in references {
            walk_cond(&reference.cond, found, out);
        }
    }
    if recursive {
        for child in &nested {
            walk_model(child, true, found, out);
        }
    }
}

/// Предупреждения `SE-096` о записи по анонимному адресу.
///
/// Направление у ячейки не объявлено, поэтому компилятор не может проверить, что запись
/// по этому адресу законна: за `#0x...` может стоять регистр только для чтения, чужая
/// периферия или вовсе неотображённая память. Чтение молчит - оно безопаснее (решение
/// 3B ).
///
/// Текст **называет способ снять** предупреждение: объявить именованный порт с тем же
/// адресом. Иначе диагностика, которую нечем погасить, превращается в шум и её глушат
/// целиком.
pub fn anon_write_warnings(model: &Rc<RefCell<ModelNode>>) -> Vec<Diagnostic> {
    let mut found = Vec::new();
    warn_model(model, &mut found);
    found
}

fn warn_model(model: &Rc<RefCell<ModelNode>>, out: &mut Vec<Diagnostic>) {
    let (funcs, blocks, states, nested) = {
        let b = model.borrow();
        (
            b.functions.values().cloned().collect::<Vec<_>>(),
            b.named_blocks.clone(),
            b.states.values().cloned().collect::<Vec<_>>(),
            b.models.values().map(Rc::clone).collect::<Vec<_>>(),
        )
    };
    for func in &funcs {
        if let FunctionDefinitionNode::Local { body, .. } = func {
            warn_stmt(body, out);
        }
    }
    for block in &blocks {
        if let Some(stmt) = block.statement() {
            warn_stmt(stmt, out);
        }
    }
    for state in &states {
        let named_blocks = match state {
            StateNode::Simple { named_blocks, .. } | StateNode::Implement { named_blocks, .. } => {
                named_blocks
            }
            StateNode::Unresolved => continue,
        };
        for block in named_blocks {
            if let Some(stmt) = block.statement() {
                warn_stmt(stmt, out);
            }
        }
    }
    for child in &nested {
        warn_model(child, out);
    }
}

/// Обходит оператор, ища присваивание, целью которого является ячейка.
fn warn_stmt(stmt: &StatementNode, out: &mut Vec<Diagnostic>) {
    // Обход по телам - тот же, что у сбора ячеек: цели записи встречаются всюду, где
    // встречается выражение.
    let mut cells: Vec<AnonPortAccess> = Vec::new();
    let mut seen: BTreeSet<(Key, String)> = BTreeSet::new();
    walk_stmt_writes(stmt, &mut seen, &mut cells);
    for cell in cells {
        out.push(
            Diagnostic::warning(
                Location::Codegen,
                format!(
                    "запись по адресу '#0x{:X}' идёт в ячейку с НЕобъявленным \
                     направлением: проверить её законность компилятор не может. \
                     Объявите именованный порт с этим адресом \
                     ('out имя: {} at 0x{:X}{};'), и запись станет проверяемой",
                    cell.addr as u64,
                    cell.ty,
                    cell.addr as u64,
                    if cell.bit == 0 {
                        String::new()
                    } else {
                        format!(":{}", cell.bit)
                    }
                ),
            )
            .with_code("SE-096"),
        );
    }
}

/// Собирает ячейки, стоящие **целью присваивания** (а не операндом чтения).
fn walk_stmt_writes(
    stmt: &StatementNode,
    seen: &mut BTreeSet<(Key, String)>,
    out: &mut Vec<AnonPortAccess>,
) {
    match stmt {
        StatementNode::Block(items) => {
            for item in items {
                walk_stmt_writes(item, seen, out);
            }
        }
        StatementNode::Expression(expr, _) => walk_expr_writes(expr, seen, out),
        StatementNode::If {
            cond, then_, else_, ..
        } => {
            walk_expr_writes(cond, seen, out);
            walk_stmt_writes(then_, seen, out);
            if let Some(other) = else_ {
                walk_stmt_writes(other, seen, out);
            }
        }
        StatementNode::Loop { cond, body, .. } => {
            if let Some(cond) = cond {
                walk_expr_writes(cond, seen, out);
            }
            walk_stmt_writes(body, seen, out);
        }
        StatementNode::For {
            init,
            cond,
            step,
            body,
            ..
        } => {
            if let Some(init) = init {
                walk_stmt_writes(init, seen, out);
            }
            if let Some(cond) = cond {
                walk_expr_writes(cond, seen, out);
            }
            if let Some(step) = step {
                walk_expr_writes(step, seen, out);
            }
            walk_stmt_writes(body, seen, out);
        }
        StatementNode::Match { expr, arms, .. } => {
            walk_expr_writes(expr, seen, out);
            for arm in arms {
                walk_stmt_writes(&arm.body, seen, out);
            }
        }
        _ => {}
    }
}

/// Ищет цели записи в выражении: интересует **левая** часть присваивания.
///
/// Рекурсия нужна потому, что присваивание бывает и глубже верхнего уровня:
/// именованным аргументом вызова (`Pid(kp := #0x100 as u8)`).
fn walk_expr_writes(
    expr: &ExpressionNode,
    seen: &mut BTreeSet<(Key, String)>,
    out: &mut Vec<AnonPortAccess>,
) {
    match expr {
        ExpressionNode::Assign(left, right) => {
            if let ExpressionNode::AnonPort(access) = left.as_ref() {
                note(access, seen, out);
            }
            walk_expr_writes(left, seen, out);
            walk_expr_writes(right, seen, out);
        }
        ExpressionNode::Parenthesis(inner)
        | ExpressionNode::BitAccess(inner, _)
        | ExpressionNode::CodeBlock(inner, _)
        | ExpressionNode::NamedFunctionBox(inner, _)
        | ExpressionNode::Not(inner)
        | ExpressionNode::UnaryPlus(inner)
        | ExpressionNode::Negate(inner)
        | ExpressionNode::Cast(inner, _)
        | ExpressionNode::BitwiseNot(inner) => walk_expr_writes(inner, seen, out),
        ExpressionNode::Function(_, args)
        | ExpressionNode::Array(args)
        | ExpressionNode::Initializer(args) => {
            for arg in args {
                walk_expr_writes(arg, seen, out);
            }
        }
        ExpressionNode::ConditionalOperator(cond, then_, else_) => {
            walk_expr_writes(cond, seen, out);
            walk_expr_writes(then_, seen, out);
            walk_expr_writes(else_, seen, out);
        }
        other => {
            // Бинарные узлы: присваивание могло попасть в любой операнд.
            if let (Some(left), Some(right)) = binary_operands(other) {
                walk_expr_writes(left, seen, out);
                walk_expr_writes(right, seen, out);
            }
        }
    }
}

/// Операнды бинарного узла, если он бинарный.
fn binary_operands(expr: &ExpressionNode) -> (Option<&ExpressionNode>, Option<&ExpressionNode>) {
    match expr {
        ExpressionNode::Power(l, r)
        | ExpressionNode::Multiply(l, r)
        | ExpressionNode::Divide(l, r)
        | ExpressionNode::Modulo(l, r)
        | ExpressionNode::Add(l, r)
        | ExpressionNode::Subtract(l, r)
        | ExpressionNode::ShiftLeft(l, r)
        | ExpressionNode::ShiftRight(l, r)
        | ExpressionNode::BitwiseAnd(l, r)
        | ExpressionNode::BitwiseXor(l, r)
        | ExpressionNode::BitwiseOr(l, r)
        | ExpressionNode::Less(l, r)
        | ExpressionNode::More(l, r)
        | ExpressionNode::LessEqual(l, r)
        | ExpressionNode::MoreEqual(l, r)
        | ExpressionNode::Equal(l, r)
        | ExpressionNode::NotEqual(l, r)
        | ExpressionNode::And(l, r)
        | ExpressionNode::Or(l, r) => (Some(l), Some(r)),
        _ => (None, None),
    }
}

fn walk_stmt(
    stmt: &StatementNode,
    found: &mut BTreeSet<(Key, String)>,
    out: &mut Vec<AnonPortAccess>,
) {
    match stmt {
        StatementNode::Block(items) => {
            for item in items {
                walk_stmt(item, found, out);
            }
        }
        StatementNode::Expression(expr, _) => walk_expr(expr, found, out),
        StatementNode::If {
            cond, then_, else_, ..
        } => {
            walk_expr(cond, found, out);
            walk_stmt(then_, found, out);
            if let Some(other) = else_ {
                walk_stmt(other, found, out);
            }
        }
        StatementNode::Loop { cond, body, .. } => {
            if let Some(cond) = cond {
                walk_expr(cond, found, out);
            }
            walk_stmt(body, found, out);
        }
        StatementNode::For {
            init,
            cond,
            step,
            body,
            ..
        } => {
            if let Some(init) = init {
                walk_stmt(init, found, out);
            }
            if let Some(cond) = cond {
                walk_expr(cond, found, out);
            }
            if let Some(step) = step {
                walk_expr(step, found, out);
            }
            walk_stmt(body, found, out);
        }
        StatementNode::Variable(_, _, Some(expr), _) => walk_expr(expr, found, out),
        StatementNode::Return(Some(expr), _) => walk_expr(expr, found, out),
        StatementNode::Match { expr, arms, .. } => {
            walk_expr(expr, found, out);
            for arm in arms {
                walk_stmt(&arm.body, found, out);
            }
        }
        // Прочие операторы выражений не несут (тот же список, что у обхода судей
        // `validate/bodies.rs`).
        _ => {}
    }
}

fn walk_expr(
    expr: &ExpressionNode,
    found: &mut BTreeSet<(Key, String)>,
    out: &mut Vec<AnonPortAccess>,
) {
    match expr {
        ExpressionNode::AnonPort(access) => note(access, found, out),
        ExpressionNode::Parenthesis(inner)
        | ExpressionNode::BitAccess(inner, _)
        | ExpressionNode::CodeBlock(inner, _)
        | ExpressionNode::NamedFunctionBox(inner, _)
        | ExpressionNode::Not(inner)
        | ExpressionNode::UnaryPlus(inner)
        | ExpressionNode::Negate(inner)
        | ExpressionNode::Cast(inner, _)
        | ExpressionNode::BitwiseNot(inner) => walk_expr(inner, found, out),
        ExpressionNode::Power(l, r)
        | ExpressionNode::Multiply(l, r)
        | ExpressionNode::Divide(l, r)
        | ExpressionNode::Modulo(l, r)
        | ExpressionNode::Add(l, r)
        | ExpressionNode::Subtract(l, r)
        | ExpressionNode::ShiftLeft(l, r)
        | ExpressionNode::ShiftRight(l, r)
        | ExpressionNode::BitwiseAnd(l, r)
        | ExpressionNode::BitwiseXor(l, r)
        | ExpressionNode::BitwiseOr(l, r)
        | ExpressionNode::Less(l, r)
        | ExpressionNode::More(l, r)
        | ExpressionNode::LessEqual(l, r)
        | ExpressionNode::MoreEqual(l, r)
        | ExpressionNode::Equal(l, r)
        | ExpressionNode::NotEqual(l, r)
        | ExpressionNode::And(l, r)
        | ExpressionNode::Or(l, r)
        | ExpressionNode::Assign(l, r) => {
            walk_expr(l, found, out);
            walk_expr(r, found, out);
        }
        ExpressionNode::ConditionalOperator(cond, then_, else_) => {
            walk_expr(cond, found, out);
            walk_expr(then_, found, out);
            walk_expr(else_, found, out);
        }
        ExpressionNode::Function(_, args)
        | ExpressionNode::Array(args)
        | ExpressionNode::Initializer(args) => {
            for arg in args {
                walk_expr(arg, found, out);
            }
        }
        ExpressionNode::ArraySubscript(_, index) => walk_expr(index, found, out),
        // Листья: обращений внутри не несут.
        //
        // Перечислены явно, без `_ =>`: новый узел языка обязан получить решение здесь.
        // Пропущенное обращение - это ячейка, которую `st-at` не объявит, а `sv-mmio`
        // не заведёт сигналом, то есть **невалидный вывод** при рапорте об успехе.
        ExpressionNode::None
        | ExpressionNode::Unresolved(_)
        | ExpressionNode::ArraySlice(_, _, _)
        | ExpressionNode::Number(_)
        | ExpressionNode::Duration(_)
        | ExpressionNode::Rational(_, _)
        | ExpressionNode::String(_)
        | ExpressionNode::Type(_)
        | ExpressionNode::Address(_, _)
        | ExpressionNode::Bool(_)
        | ExpressionNode::Variable(_)
        | ExpressionNode::Model(_)
        | ExpressionNode::Condition(_)
        | ExpressionNode::List(_) => {}
    }
}

fn walk_cond(
    cond: &ConditionNode,
    found: &mut BTreeSet<(Key, String)>,
    out: &mut Vec<AnonPortAccess>,
) {
    match cond {
        ConditionNode::AnonPort(access) => note(access, found, out),
        ConditionNode::Not(inner)
        | ConditionNode::Parenthesis(inner)
        | ConditionNode::BitAccess(inner, _)
        | ConditionNode::AfterExpr(inner)
        | ConditionNode::ArraySubscript(_, inner) => walk_cond(inner, found, out),
        ConditionNode::Add(l, r)
        | ConditionNode::Subtract(l, r)
        | ConditionNode::And(l, r)
        | ConditionNode::Or(l, r)
        | ConditionNode::Less(l, r)
        | ConditionNode::More(l, r)
        | ConditionNode::LessEqual(l, r)
        | ConditionNode::MoreEqual(l, r)
        | ConditionNode::Equal(l, r)
        | ConditionNode::NotEqual(l, r) => {
            walk_cond(l, found, out);
            walk_cond(r, found, out);
        }
        ConditionNode::Function(_, args, _) => {
            for arg in args {
                walk_cond(arg, found, out);
            }
        }
        // Прочие условия обращений не несут.
        _ => {}
    }
}
