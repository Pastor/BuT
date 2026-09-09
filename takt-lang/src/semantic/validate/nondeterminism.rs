//! Недетерминированные переходы (Ce14): пересечение условий рёбер.
//!
//! Часть модуля `validate`.

use super::*;
use crate::diagnostics::lang::keys;
use crate::msg;

/// Одностороннее ограничение переменной относительно целочисленного литерала.
///
/// Используется для обнаружения попарного перекрытия условий переходов (NI4).
#[derive(Debug, Clone, PartialEq, Eq)]
enum Constraint {
    /// `var = n`
    Eq(i128),
    /// `var != n`
    Ne(i128),
    /// `var < n`
    Lt(i128),
    /// `var <= n`
    Le(i128),
    /// `var > n`
    Gt(i128),
    /// `var >= n`
    Ge(i128),
}

/// Проверяет детерминированность переходов в состояниях модели.
///
/// Предупреждает если несколько `ref`-переходов из одного состояния не имеют условий
/// (безусловные переходы - `Condition::None`) - это явная недетерминированность:
/// непонятно, в какое состояние перейти.
///
/// # Возвращаемое значение
///
/// Вектор [`Diagnostic`] уровня Warning для каждого состояния с более чем одним
/// безусловным переходом или с перекрывающимися условиями.
pub fn check_nondeterministic_transitions(model: Rc<RefCell<ModelNode>>) -> Vec<Diagnostic> {
    let mut warnings = Vec::new();
    check_nondeterministic_model(model, &mut warnings);
    warnings
}

/// Извлекает из условия вида `var OP number` пару `(имя-переменной, Constraint)`.
///
/// Поддерживает только простые атомарные условия на одну переменную. Возвращает `None`
/// для составных условий (AND, OR, NOT и т.д.).
///
/// Идентификация переменной выполняется по имени, а не по указателю `Rc`, потому что
/// `resolve_condition` создаёт новый `Rc` при каждом обращении к одной и той же
/// переменной.
fn extract_simple_constraint(cond: &ConditionNode) -> Option<(String, Constraint)> {
    /// Извлекает имя переменной из узла `ConditionNode::Variable`.
    fn var_name(node: &ConditionNode) -> Option<String> {
        if let ConditionNode::Variable(v, _) = node {
            Some(v.borrow().name().to_owned())
        } else {
            None
        }
    }

    match cond {
        ConditionNode::Equal(l, r) => match (l.as_ref(), r.as_ref()) {
            (var, ConditionNode::Number(n)) => var_name(var).map(|name| (name, Constraint::Eq(*n))),
            (ConditionNode::Number(n), var) => var_name(var).map(|name| (name, Constraint::Eq(*n))),
            _ => None,
        },
        ConditionNode::NotEqual(l, r) => match (l.as_ref(), r.as_ref()) {
            (var, ConditionNode::Number(n)) => var_name(var).map(|name| (name, Constraint::Ne(*n))),
            (ConditionNode::Number(n), var) => var_name(var).map(|name| (name, Constraint::Ne(*n))),
            _ => None,
        },
        ConditionNode::Less(l, r) => match (l.as_ref(), r.as_ref()) {
            (var, ConditionNode::Number(n)) => var_name(var).map(|name| (name, Constraint::Lt(*n))),
            (ConditionNode::Number(n), var) => {
                // n < var -> var > n
                var_name(var).map(|name| (name, Constraint::Gt(*n)))
            }
            _ => None,
        },
        ConditionNode::LessEqual(l, r) => match (l.as_ref(), r.as_ref()) {
            (var, ConditionNode::Number(n)) => var_name(var).map(|name| (name, Constraint::Le(*n))),
            (ConditionNode::Number(n), var) => {
                // n <= var -> var >= n
                var_name(var).map(|name| (name, Constraint::Ge(*n)))
            }
            _ => None,
        },
        ConditionNode::More(l, r) => match (l.as_ref(), r.as_ref()) {
            (var, ConditionNode::Number(n)) => var_name(var).map(|name| (name, Constraint::Gt(*n))),
            (ConditionNode::Number(n), var) => {
                // n > var -> var < n
                var_name(var).map(|name| (name, Constraint::Lt(*n)))
            }
            _ => None,
        },
        ConditionNode::MoreEqual(l, r) => match (l.as_ref(), r.as_ref()) {
            (var, ConditionNode::Number(n)) => var_name(var).map(|name| (name, Constraint::Ge(*n))),
            (ConditionNode::Number(n), var) => {
                // n >= var -> var <= n
                var_name(var).map(|name| (name, Constraint::Le(*n)))
            }
            _ => None,
        },
        // Скобки - прозрачны
        ConditionNode::Parenthesis(inner) => extract_simple_constraint(inner),
        _ => None,
    }
}

/// Проверяет, могут ли два ограничения одновременно выполняться.
///
/// Возвращает `true`, если существует целое число, удовлетворяющее обоим.
fn constraints_overlap(a: &Constraint, b: &Constraint) -> bool {
    use Constraint::*;
    match (a, b) {
        // Eq vs *
        (Eq(x), Eq(y)) => x == y,
        (Eq(x), Ne(y)) => x != y,
        (Eq(x), Lt(y)) => x < y,
        (Eq(x), Le(y)) => x <= y,
        (Eq(x), Gt(y)) => x > y,
        (Eq(x), Ge(y)) => x >= y,
        // Ne vs *
        (Ne(x), Eq(y)) => x != y,
        (Ne(x), Ne(_y)) => *x != i128::MAX, // всегда истинно (хотя бы одно значение)
        (Ne(_), Lt(_)) => true,             // всегда есть значение != x и < y
        (Ne(_), Le(_)) => true,
        (Ne(_), Gt(_)) => true,
        (Ne(_), Ge(_)) => true,
        // Lt vs *
        (Lt(x), Eq(y)) => *y < *x,
        (Lt(_), Ne(_)) => true,
        (Lt(_), Lt(_)) => true,        // (-∞, x-1] ∩ (-∞, y-1] всегда непусто
        (Lt(_), Le(_)) => true,        // (-∞, x-1] ∩ (-∞, y] всегда непусто
        (Lt(x), Gt(y)) => *y + 1 < *x, // (y, ∞) ∩ (-∞, x-1): нужно y+1 < x
        (Lt(x), Ge(y)) => *y < *x,     // [y, ∞) ∩ (-∞, x-1): нужно y < x
        // Le vs *
        (Le(x), Eq(y)) => *y <= *x,
        (Le(_), Ne(_)) => true,
        (Le(_), Lt(_)) => true,    // (-∞, x] ∩ (-∞, y-1] всегда непусто
        (Le(_), Le(_)) => true,    // (-∞, x] ∩ (-∞, y] всегда непусто
        (Le(x), Gt(y)) => *y < *x, // нужно y < x (хотя бы y+1 <= x)
        (Le(x), Ge(y)) => *y <= *x,
        // Gt vs *
        (Gt(x), Eq(y)) => *y > *x,
        (Gt(_), Ne(_)) => true,
        (Gt(x), Lt(y)) => *x + 1 < *y,
        (Gt(x), Le(y)) => *x < *y,
        (Gt(_), Gt(_)) => true, // (x, ∞) ∩ (y, ∞) всегда непусто
        (Gt(_), Ge(_)) => true, // (x, ∞) ∩ [y, ∞) всегда непусто
        // Ge vs *
        (Ge(x), Eq(y)) => *y >= *x,
        (Ge(_), Ne(_)) => true,
        (Ge(x), Lt(y)) => *x < *y,
        (Ge(x), Le(y)) => *x <= *y,
        (Ge(_), Gt(_)) => true, // [x, ∞) ∩ (y, ∞) всегда непусто
        (Ge(_), Ge(_)) => true, // [x, ∞) ∩ [y, ∞) всегда непусто
    }
}

fn check_nondeterministic_model(model: Rc<RefCell<ModelNode>>, warnings: &mut Vec<Diagnostic>) {
    let borrowed = model.borrow();
    let model_name = borrowed.name.clone().unwrap_or_default();

    for (state_name, state) in &borrowed.states {
        let references: &[ReferenceNode<StateNode>] = match state {
            StateNode::Simple { references, .. } => references,
            StateNode::Implement { references, .. } => references,
            StateNode::Unresolved => continue,
        };

        let prefix = if model_name.is_empty() {
            msg!(keys::WHAT_STATE, name = state_name)
        } else {
            msg!(
                keys::WHAT_MODEL_STATE,
                model = model_name,
                state = state_name
            )
        };

        // Ce14: подсчёт безусловных переходов. Что считать безусловным - решает
        // `ConditionNode::is_unconditional`, а не эта проверка: правило одно на восемь
        // потребителей.
        let unconditional_count = references
            .iter()
            .filter(|r| r.cond.is_unconditional())
            .count();

        if unconditional_count > 1 {
            warnings.push(
                Diagnostic::warning(
                    state.loc(),
                    msg!(
                        keys::SE_037_UNCONDITIONAL_TRANSITIONS,
                        prefix = prefix,
                        count = unconditional_count
                    ),
                )
                .with_code("SE-037"),
            );
        }

        // NI4: Анализ структурного и интервального перекрытия условных переходов
        let conditional: Vec<_> = references
            .iter()
            .filter(|r| !r.cond.is_unconditional())
            .collect();

        for i in 0..conditional.len() {
            for j in (i + 1)..conditional.len() {
                let cond_i = &conditional[i].cond;
                let cond_j = &conditional[j].cond;

                // Структурно одинаковые условия - гарантированное перекрытие
                if cond_i == cond_j {
                    warnings.push(
                        Diagnostic::warning(
                            conditional[i].location,
                            msg!(
                                keys::SE_042_SAME_CONDITION,
                                prefix = prefix,
                                first = conditional[i].name,
                                second = conditional[j].name
                            ),
                        )
                        .with_code("SE-042"),
                    );
                    continue;
                }

                // Интервальный анализ простых атомарных условий
                if let (Some((var_i, constr_i)), Some((var_j, constr_j))) = (
                    extract_simple_constraint(cond_i),
                    extract_simple_constraint(cond_j),
                ) {
                    // Условия на одну и ту же переменную
                    if var_i == var_j && constraints_overlap(&constr_i, &constr_j) {
                        warnings.push(
                            Diagnostic::warning(
                                conditional[i].location,
                                msg!(
                                    keys::SE_042_CONDITIONS_MAY_OVERLAP,
                                    prefix = prefix,
                                    first = conditional[i].name,
                                    second = conditional[j].name
                                ),
                            )
                            .with_code("SE-042"),
                        );
                    }
                }
            }
        }
    }

    // Рекурсивно для вложенных моделей
    let nested: Vec<Rc<RefCell<ModelNode>>> = borrowed.models.values().map(Rc::clone).collect();
    drop(borrowed);

    for nested_model in nested {
        check_nondeterministic_model(nested_model, warnings);
    }
}
