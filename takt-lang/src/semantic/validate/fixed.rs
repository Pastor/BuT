//! Запрет неявного смешения типов в арифметике: `q(m, n)` (T6 фичи 0061,
//! `SE-059`) и `duration` (правило 5 ADR 0134, `SE-065`).
//!
//! Оба правила живут в **одном** обходе намеренно: арифметическое выражение
//! обходится один раз, и второй экземпляр обхода неизбежно разъехался бы с
//! первым (прецедент — два матчера адреса фичи 0042, обязанные давать один
//! результат).
//!
//! Правило 6 ADR 0061: `q(m, n)` в арифметике сочетается **только** с тем же
//! `q(m, n)`. Смешение с иным `q`, целым, `float` и т. п. — ошибка `SE-059`, а
//! не молчаливая потеря точности (иначе [`wider_type`] вернул бы `Unsupported`,
//! и расхождение уехало бы в кодоген). Выход — явное приведение `… as q(m, n)`
//! (узел `Cast`), который смешение снимает.
//!
//! Обход повторяет [`unused`](crate::semantic::unused): выражения
//! инициализаторов, тел функций, именованных блоков и блоков состояний.
//! Вложенные модели обходит [`validate_model`](super::validate_model), поэтому
//! здесь — только текущий уровень.
//!
//! [`wider_type`]: crate::semantic::type_inference::wider_type

use super::*;
use crate::semantic::type_inference::extract_type;

/// Проверяет запрет смешения `q(m, n)` во всех выражениях **уровня** модели.
pub(super) fn check_fixed_mixing(model: Rc<RefCell<ModelNode>>) -> Vec<Diagnostic> {
    // Снимаем владельческие копии до обхода: `extract_type` не берёт
    // `borrow_mut`, но держать borrow через `?`-возвраты незачем.
    let (vars, funcs, blocks, states) = {
        let b = model.borrow();
        (
            b.variables.values().cloned().collect::<Vec<_>>(),
            b.functions.values().cloned().collect::<Vec<_>>(),
            b.named_blocks.clone(),
            b.states.values().cloned().collect::<Vec<_>>(),
        )
    };

    // Накопление по элементам (фича 0151): объявление, функция, блок и
    // состояние — самостоятельные места; внутри каждого остаётся первая
    // ошибка (дальше по тому же выражению пошли бы следствия).
    let mut out = Vec::new();
    for var in &vars {
        if let Some(expr) = var_init(var) {
            out.extend(check_expr(expr, &model).err());
        }
    }
    for func in &funcs {
        if let FunctionDefinitionNode::Local { body, .. } = func {
            out.extend(check_stmt(body, &model).err());
        }
    }
    for block in &blocks {
        if let Some(stmt) = block.statement() {
            out.extend(check_stmt(stmt, &model).err());
        }
    }
    for state in &states {
        out.extend(check_state(state, &model).err());
    }
    out
}

/// Инициализатор переменной (или `None` для `Unresolved`).
fn var_init(var: &VariableNode) -> Option<&ExpressionNode> {
    match var {
        VariableNode::Simple { expr, .. } | VariableNode::Const { expr, .. } => Some(expr),
        // У порта берётся **начальное значение**, а не адрес (фича 0187):
        // смешение типов fixed-point — свойство значения; адрес — целое число
        // и к арифметике `q(m, n)` отношения не имеет.
        VariableNode::Port { init, .. } => Some(init),
        VariableNode::Unresolved => None,
    }
}

/// Блоки и рёбра состояния (условия рёбер — отдельная грамматика, здесь не
/// проверяются: смешение — свойство **выражений**, правило 6 ADR).
fn check_state(state: &StateNode, model: &Rc<RefCell<ModelNode>>) -> Result<(), Diagnostic> {
    let named_blocks = match state {
        StateNode::Simple { named_blocks, .. } | StateNode::Implement { named_blocks, .. } => {
            named_blocks
        }
        StateNode::Unresolved => return Ok(()),
    };
    for block in named_blocks {
        if let Some(stmt) = block.statement() {
            check_stmt(stmt, model)?;
        }
    }
    Ok(())
}

/// Рекурсивно проходит оператор, проверяя все вложенные выражения.
fn check_stmt(stmt: &StatementNode, model: &Rc<RefCell<ModelNode>>) -> Result<(), Diagnostic> {
    match stmt {
        StatementNode::Block(stmts) => {
            for s in stmts {
                check_stmt(s, model)?;
            }
        }
        StatementNode::Expression(e, _) => check_expr(e, model)?,
        StatementNode::If {
            cond, then_, else_, ..
        } => {
            check_expr(cond, model)?;
            check_stmt(then_, model)?;
            if let Some(e) = else_ {
                check_stmt(e, model)?;
            }
        }
        StatementNode::Loop { cond, body, .. } => {
            if let Some(c) = cond {
                check_expr(c, model)?;
            }
            check_stmt(body, model)?;
        }
        StatementNode::For {
            init,
            cond,
            step,
            body,
            ..
        } => {
            if let Some(s) = init {
                check_stmt(s, model)?;
            }
            if let Some(c) = cond {
                check_expr(c, model)?;
            }
            if let Some(s) = step {
                check_expr(s, model)?;
            }
            check_stmt(body, model)?;
        }
        StatementNode::Variable(_, _, Some(e), _) => check_expr(e, model)?,
        StatementNode::Return(Some(e), _) => check_expr(e, model)?,
        StatementNode::Match { expr, arms, .. } => {
            check_expr(expr, model)?;
            for arm in arms {
                check_stmt(&arm.body, model)?;
            }
        }
        // Вставка для цели — операторы Takt (0484): обход спускается в тело.
        // Блок формул адресован внешнему анализатору, операторов Takt в нём нет.
        StatementNode::Assembly { body, .. } => check_stmt(body, model)?,
        StatementNode::None
        | StatementNode::Unresolved(_)
        | StatementNode::Variable(_, _, None, _)
        | StatementNode::Return(None, _)
        | StatementNode::Continue(_)
        | StatementNode::Break(_)
        | StatementNode::Formula(_)
        | StatementNode::InlineFormula(_) => {}
    }
    Ok(())
}

/// Рекурсивно проходит выражение. У арифметических бинарных узлов дополнительно
/// проверяет запрет смешения — **после** рекурсии в операнды, чтобы самое
/// вложенное смешение сообщалось первым.
fn check_expr(expr: &ExpressionNode, model: &Rc<RefCell<ModelNode>>) -> Result<(), Diagnostic> {
    match expr {
        // Арифметика: сюда бьёт правило 6.
        ExpressionNode::Add(l, r)
        | ExpressionNode::Subtract(l, r)
        | ExpressionNode::Multiply(l, r)
        | ExpressionNode::Divide(l, r)
        | ExpressionNode::Modulo(l, r)
        | ExpressionNode::Power(l, r) => {
            check_expr(l, model)?;
            check_expr(r, model)?;
            check_mixing(l, r, model)?;
        }
        // Прочие бинарные (сравнения, логика, битовые, сдвиги, присваивание):
        // только рекурсия — смешение ловит арифметика, а `as` — узел `Cast`.
        ExpressionNode::ShiftLeft(l, r)
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
            check_expr(l, model)?;
            check_expr(r, model)?;
        }
        // Унарные и обёртки. `Cast` рекурсирует в операнд, но само приведение
        // смешением не является — оно его и снимает.
        ExpressionNode::Parenthesis(e)
        | ExpressionNode::BitAccess(e, _)
        | ExpressionNode::NamedFunctionBox(e, _)
        | ExpressionNode::Not(e)
        | ExpressionNode::BitwiseNot(e)
        | ExpressionNode::UnaryPlus(e)
        | ExpressionNode::Negate(e)
        | ExpressionNode::Cast(e, _) => check_expr(e, model)?,
        ExpressionNode::ConditionalOperator(c, t, e) => {
            check_expr(c, model)?;
            check_expr(t, model)?;
            check_expr(e, model)?;
        }
        ExpressionNode::CodeBlock(e, stmt) => {
            check_expr(e, model)?;
            check_stmt(stmt, model)?;
        }
        ExpressionNode::Function(_, args)
        | ExpressionNode::Array(args)
        | ExpressionNode::Initializer(args) => {
            for a in args {
                check_expr(a, model)?;
            }
        }
        // Листья: подвыражений нет.
        ExpressionNode::None
        | ExpressionNode::Unresolved(_)
        | ExpressionNode::ArraySubscript(_, _)
        | ExpressionNode::ArraySlice(_, _, _)
        | ExpressionNode::Number(_)
        // Длительность — не fixed-point: смешение `duration` с числом ловит
        // отдельная проверка (SE-065), а не эта.
        | ExpressionNode::Duration(_)
        | ExpressionNode::Rational(_, _)
        | ExpressionNode::String(_)
        | ExpressionNode::Type(_)
        | ExpressionNode::Address(_, _)
        // Тип обращения по адресу (фича 0189) задан приведением автора: если это
        // `q(m, n)`, проверку ведут общие правила fixed-арифметики по типу узла.
        | ExpressionNode::AnonPort(_)
        | ExpressionNode::Bool(_)
        | ExpressionNode::Variable(_)
        | ExpressionNode::Model(_)
        | ExpressionNode::Condition(_)
        | ExpressionNode::List(_) => {}
    }
    Ok(())
}

/// Правило 6: если хотя бы один операнд — `q(m, n)`, а типы операндов не
/// совпадают, это смешение → `SE-059`. Типы, которые вывести не удалось,
/// пропускаются (их ошибки поднимут свои проходы).
fn check_mixing(
    l: &ExpressionNode,
    r: &ExpressionNode,
    model: &Rc<RefCell<ModelNode>>,
) -> Result<(), Diagnostic> {
    let (Ok(lt), Ok(rt)) = (
        extract_type(l, model.clone()),
        extract_type(r, model.clone()),
    ) else {
        return Ok(());
    };
    let involves_fixed =
        matches!(lt, TypeNode::Fixed { .. }) || matches!(rt, TypeNode::Fixed { .. });
    if involves_fixed && lt != rt {
        let loc = first_loc(l)
            .or_else(|| first_loc(r))
            .unwrap_or(Location::Implicit);
        return Err(se059(loc, &lt, &rt));
    }
    // Время (фича 0134, правило 5 ADR): `duration` сочетается с `duration`.
    // Проверка живёт здесь, а не в своём обходе, намеренно: обход арифметики
    // один, и второй его экземпляр разъехался бы с первым — прецедент известен
    // (два матчера адреса фичи 0042 обязаны были давать один результат).
    let involves_duration = lt == TypeNode::Duration || rt == TypeNode::Duration;
    if involves_duration && lt != rt {
        let loc = first_loc(l)
            .or_else(|| first_loc(r))
            .unwrap_or(Location::Implicit);
        return Err(se065(loc, &lt, &rt));
    }
    Ok(())
}

/// `SE-065` — смешение `duration` с числом (правило 5 ADR 0134).
///
/// Умножение длительности на целое (`DWELL * 2`) языком **разрешено**, но
/// сегодня отвергается вместе с прочим смешением: масштабирование введёт та
/// задача, которая заведёт для него правило вывода типа. Ошибиться в сторону
/// запрета безопасно (запрет снимается аддитивно), в сторону разрешения — нет.
fn se065(loc: Location, lt: &TypeNode, rt: &TypeNode) -> Diagnostic {
    Diagnostic::declaration_error(
        loc,
        format!(
            "смешение типов '{}' и '{}' в арифметике запрещено: длительность              сочетается только с длительностью",
            lt, rt
        ),
    )
    .with_code("SE-065")
}

/// `SE-059` — неявное смешение `q(m, n)` с другим типом (правило 6 ADR 0061).
///
/// ⚠️ Модификатор `sat` (фича 0170) входит в формат, поэтому `q(8, 8)` и
/// `q(8, 8) sat` — разные типы, и их смешение ловится **этим же** правилом.
/// Отдельный код для него заводить не стали (отступление от ADR 0170, замер
/// задачи 0170-01): правило одно — «формат обязан совпасть», — и разделение его
/// надвое дало бы два места, обязанные давать один вердикт. Имена типов в тексте
/// печатаются вместе с модификатором (`Display for TypeNode`), поэтому сообщение
/// остаётся содержательным: «смешение 'q(8, 8) sat' и 'q(8, 8)'».
fn se059(loc: Location, lt: &TypeNode, rt: &TypeNode) -> Diagnostic {
    Diagnostic::declaration_error(
        loc,
        format!(
            "неявное смешение типов '{}' и '{}' в арифметике fixed-point запрещено; \
             приведите операнд явно ('… as q(m, n)' либо '… as q(m, n) sat')",
            lt, rt
        ),
    )
    .with_code("SE-059")
}

/// Позиция для диагностики: первая переменная/порт/константа в выражении.
/// У `ExpressionNode` собственной позиции нет (фича 0056), поэтому берём её у
/// ближайшего именованного узла.
fn first_loc(expr: &ExpressionNode) -> Option<Location> {
    match expr {
        ExpressionNode::Variable(v) => var_loc(&v.borrow()),
        // База — выражение (фича 0358): позиция берётся у неё.
        ExpressionNode::ArraySubscript(base, _) | ExpressionNode::ArraySlice(base, _, _) => {
            first_loc(base)
        }
        ExpressionNode::Parenthesis(e)
        | ExpressionNode::BitAccess(e, _)
        | ExpressionNode::Not(e)
        | ExpressionNode::BitwiseNot(e)
        | ExpressionNode::UnaryPlus(e)
        | ExpressionNode::Negate(e)
        | ExpressionNode::Cast(e, _) => first_loc(e),
        ExpressionNode::Add(l, r)
        | ExpressionNode::Subtract(l, r)
        | ExpressionNode::Multiply(l, r)
        | ExpressionNode::Divide(l, r)
        | ExpressionNode::Modulo(l, r)
        | ExpressionNode::Power(l, r) => first_loc(l).or_else(|| first_loc(r)),
        _ => None,
    }
}

/// Позиция объявления переменной.
fn var_loc(var: &VariableNode) -> Option<Location> {
    match var {
        VariableNode::Simple { loc, .. }
        | VariableNode::Port { loc, .. }
        | VariableNode::Const { loc, .. } => Some(*loc),
        VariableNode::Unresolved => None,
    }
}
