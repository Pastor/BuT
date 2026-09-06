//! Общие помощники проверок: условия, выражения, доступ к состояниям.
//!
//! Часть модуля `validate`.

use super::*;
use crate::semantic::condition::state_of::state_of_model;

pub(super) fn validate_cond(
    context: Option<ConditionNode>,
    cond: &ConditionNode,
    model: Rc<RefCell<ModelNode>>,
) -> Result<(), Diagnostic> {
    let _borrowed = model.borrow();
    match cond.clone() {
        ConditionNode::None => {}
        ConditionNode::Unresolved(cond) => {
            #[allow(clippy::collapsible_if)]
            if let Some(context) = context
                && let ast::Condition::Variable(id) = cond.clone()
            {
                // Левый операнд - "текущее состояние модели"? Форму паттерна разбирает
                // Одна функция на проект: прежде судья знал только `S(Модель)`, тогда
                // как цели `c` и `rust` знают и краткое `Модель`, - и `ref X: E !=
                // End;` отвергался `SE-025` на записи, которую генератор переводит.
                if let Some(model) = state_of_model(&context) {
                    let model = model.borrow();
                    let model_name = model
                        .name
                        .clone()
                        .unwrap_or_else(|| "<анонимная>".to_string());
                    model.search_state(&id.name).ok_or_else(|| {
                        Diagnostic::error(
                            id.loc,
                            format!(
                                "Состояние '{}' не найдено в модели '{}': справа от \
                                 '=' в проверке 'S(Модель) = Состояние' стоит состояние \
                                 названной модели, а не любое имя",
                                id.name, model_name
                            ),
                        )
                        .with_code("SE-033")
                    })?;
                    return Ok(());
                }
            }

            if let ConditionNode::Unresolved(_) = resolve_condition(&cond, model.clone())? {
                // Цитата - текст исходника, а не `Debug`-дамп узла: прежде сообщение
                // выглядело как "Неразрешённое условие: Variable(Identifier { loc:
                // Source(0, 51, 54), name: "qqq" })" - внутреннее представление вместо
                // записи автора. Печатью занимается форматтер; узел, который он не
                // умеет, оставляет сообщение без цитаты - но дампа не будет никогда.
                let quoted = crate::format::condition_text(&cond)
                    .map(|text| format!(" '{text}'"))
                    .unwrap_or_default();
                return Err(Diagnostic::error(
                    cond.loc(),
                    format!(
                        "неразрешённое условие перехода{quoted}: имя не найдено среди \
                         переменных, портов, условий `cond` и состояний"
                    ),
                )
                .with_code("SE-025"));
            }
        }
        ConditionNode::ArraySubscript(_, _) => {}
        ConditionNode::Parenthesis(cond) => {
            validate_cond(None, &cond, model.clone())?;
        }
        ConditionNode::BitAccess(cond, _) => {
            validate_cond(None, &cond, model.clone())?;
        }
        ConditionNode::Function(_, conds, _) => {
            for cond in conds {
                validate_cond(None, &cond, model.clone())?;
            }
        }
        ConditionNode::Not(cond) => {
            validate_cond(None, &cond, model.clone())?;
        }
        ConditionNode::Add(left, right) => {
            validate_cond(None, &left, model.clone())?;
            validate_cond(None, &right, model.clone())?;
        }
        ConditionNode::Subtract(left, right) => {
            validate_cond(None, &left, model.clone())?;
            validate_cond(None, &right, model.clone())?;
        }
        ConditionNode::And(left, right) => {
            validate_cond(None, &left, model.clone())?;
            validate_cond(None, &right, model.clone())?;
        }
        ConditionNode::Or(left, right) => {
            validate_cond(None, &left, model.clone())?;
            validate_cond(None, &right, model.clone())?;
        }
        ConditionNode::Less(left, right) => {
            validate_cond(None, &left, model.clone())?;
            validate_cond(None, &right, model.clone())?;
        }
        ConditionNode::More(left, right) => {
            validate_cond(None, &left, model.clone())?;
            validate_cond(None, &right, model.clone())?;
        }
        ConditionNode::LessEqual(left, right) => {
            validate_cond(None, &left, model.clone())?;
            validate_cond(None, &right, model.clone())?;
        }
        ConditionNode::MoreEqual(left, right) => {
            validate_cond(None, &left, model.clone())?;
            validate_cond(None, &right, model.clone())?;
        }
        ConditionNode::Equal(left, right) => {
            // Левый операнд паттерна `S(Модель) = Состояние` (и краткой формы `Модель =
            // Состояние`) - это `ConditionNode::Model`, то есть ровно то, что ветвь
            // ниже отвергает как "не условие" (SE-110). Форму паттерна разбирает
            // Единственная функция `state_of_model`: второй разбор здесь разъехался бы
            // с ней при первой же правке.
            if state_of_model(&left).is_none() {
                validate_cond(None, &left, model.clone())?;
            }
            validate_cond(Some(*left.clone()), &right, model.clone())?;
        }
        ConditionNode::NotEqual(left, right) => {
            if state_of_model(&left).is_none() {
                validate_cond(None, &left, model.clone())?;
            }
            // Передаём контекст левого операнда - как в Equal - для проверки паттерна
            // `S(Model) != СостояниеИмя`: имя состояния должно быть валидным в
            // указанной модели.
            validate_cond(Some(*left.clone()), &right, model.clone())?;
        }
        ConditionNode::Number(_) => {}
        ConditionNode::Duration(_) | ConditionNode::After(_) | ConditionNode::AfterTicks(_) => {}
        // Вычисляемая выдержка: вложенное выражение - обычное условие, и его проверки
        // (чтение `out`-порта, неизвестное имя) обязаны работать так же, как везде.
        ConditionNode::AfterExpr(inner) => {
            validate_cond(None, &inner, model.clone())?;
        }
        ConditionNode::Rational(_, _) => {}
        ConditionNode::String(_) => {}
        ConditionNode::Bool(_) => {}
        // Обращение к ячейке в условии - только чтение, проверять нечего.
        ConditionNode::AnonPort(_) => {}
        ConditionNode::Variable(var_rc, _) => {
            // Чтение из `out`-порта запрещено в условии (SE-027)
            if let VariableNode::Port {
                direction: PortDirection::Out,
                name,
                loc,
                ..
            } = &*var_rc.borrow()
            {
                return Err(Diagnostic::error(
                    *loc,
                    format!("Чтение из выходного порта '{}' запрещено", name),
                )
                .with_code("SE-027"));
            }
        }
        // Голое имя модели или состояния условием не является.
        //
        // Не переводит конструкцию никто, поэтому отвергает её компилятор - один раз и
        // с позицией имени.
        //
        // Осмысленное уже выразимо: "состояние под-модели" - это `S(Модель) =
        // Состояние` или краткая `Модель = Состояние`, и левый операнд такого паттерна
        // сюда не доходит (см. `Equal`/`NotEqual` выше).
        ConditionNode::Model(model_rc, loc) => {
            let name = model_rc
                .borrow()
                .name
                .clone()
                .unwrap_or_else(|| "?".to_string());
            return Err(Diagnostic::error(
                loc,
                format!(
                    "имя модели '{name}' само по себе условием не является: \
                     напишите 'S({name}) = Состояние' или краткую форму \
                     '{name} = Состояние'"
                ),
            )
            .with_code("SE-110"));
        }
        ConditionNode::State(state_rc, loc) => {
            let name = state_rc.borrow().name().to_string();
            return Err(Diagnostic::error(
                loc,
                format!(
                    "имя состояния '{name}' само по себе условием не является: \
                     проверка состояния записывается как 'S(Модель) = {name}' \
                     или краткой формой 'Модель = {name}'"
                ),
            )
            .with_code("SE-110"));
        }
        ConditionNode::EnumVariant(_, _, _) => {}
    }
    Ok(())
}

/// Является ли переменная **выходным** портом.
///
/// Вынесено: предикат нужен и проверке чтения, и исключению для левой части
/// присваивания - два места, где ошибиться значит либо запретить законную запись, либо
/// разрешить незаконное чтение.
fn is_out_port(var: &Rc<RefCell<VariableNode>>) -> bool {
    matches!(
        &*var.borrow(),
        VariableNode::Port {
            direction: PortDirection::Out,
            ..
        }
    )
}

/// Объявление, к части которого обращается место записи: `res.tail.b`, `bus[1].lo`,
/// `led.0` - всё это обращения к `res`, `bus`, `led`.
///
/// Спуск идёт по цепочке любой длины.
fn place_base(expr: &ExpressionNode) -> Option<Rc<RefCell<VariableNode>>> {
    match expr {
        ExpressionNode::Variable(var) => Some(var.clone()),
        ExpressionNode::BitAccess(inner, _)
        | ExpressionNode::ArraySubscript(inner, _)
        | ExpressionNode::Parenthesis(inner) => place_base(inner),
        _ => None,
    }
}

pub(super) fn validate_expression(
    expr: &ExpressionNode,
    model: Rc<RefCell<ModelNode>>,
) -> Result<(), Diagnostic> {
    let _borrowed = model.borrow();
    match expr {
        ExpressionNode::None => {}
        ExpressionNode::Unresolved(_) => {}
        ExpressionNode::ArraySubscript(_, _) => {}
        ExpressionNode::ArraySlice(_, _, _) => {}
        ExpressionNode::Parenthesis(expr)
        | ExpressionNode::BitAccess(expr, _)
        | ExpressionNode::CodeBlock(expr, _)
        | ExpressionNode::NamedFunctionBox(expr, _)
        | ExpressionNode::Not(expr)
        | ExpressionNode::UnaryPlus(expr)
        | ExpressionNode::Negate(expr)
        | ExpressionNode::Cast(expr, _)
        | ExpressionNode::BitwiseNot(expr) => {
            validate_expression(expr, model.clone())?;
        }
        ExpressionNode::Power(left, right)
        | ExpressionNode::Multiply(left, right)
        | ExpressionNode::Divide(left, right)
        | ExpressionNode::Modulo(left, right)
        | ExpressionNode::Add(left, right)
        | ExpressionNode::Subtract(left, right)
        | ExpressionNode::ShiftLeft(left, right)
        | ExpressionNode::ShiftRight(left, right)
        | ExpressionNode::BitwiseAnd(left, right)
        | ExpressionNode::BitwiseXor(left, right)
        | ExpressionNode::BitwiseOr(left, right)
        | ExpressionNode::Less(left, right)
        | ExpressionNode::More(left, right)
        | ExpressionNode::LessEqual(left, right)
        | ExpressionNode::MoreEqual(left, right)
        | ExpressionNode::Equal(left, right)
        | ExpressionNode::NotEqual(left, right)
        | ExpressionNode::And(left, right)
        | ExpressionNode::Or(left, right) => {
            validate_expression(left, model.clone())?;
            validate_expression(right, model.clone())?;
        }
        ExpressionNode::Assign(left, right) => {
            // Запись в `in`-порт запрещена (SE-026). Место записи - вся цепочка
            // обращения, а не её первый шаг.
            if let Some(var_rc) = place_base(left)
                && let VariableNode::Port {
                    direction: PortDirection::In,
                    name,
                    loc,
                    ..
                } = &*var_rc.borrow()
            {
                return Err(Diagnostic::error(
                    *loc,
                    format!("Запись во входной порт '{}' запрещена", name),
                )
                .with_code("SE-026"));
            }
            // Левая часть присваивания - **место записи**, а не чтение: рекурсировать в
            // неё нельзя, иначе законное `led := 1;` для выходного порта дало бы
            // `SE-027` "чтение выходного порта".
            //
            // Прежде исключение было написано только для `BitAccess` (`led.0 := 1;`),
            // потому что проверка работала лишь на условиях, где присваиваний не
            // бывает. распространила её на тела блоков - и форма `led := 1;` стала
            // достижимой, поэтому исключение обобщено на обе формы цели записи.
            //
            // Цепочка обращения бывает длиннее одного шага: `res.tail.b := v` и `bus[1]
            // := v` - тоже записи в выходной порт.
            let target_is_out_port = place_base(left).as_ref().is_some_and(is_out_port);
            if !target_is_out_port {
                validate_expression(left, model.clone())?;
            }
            validate_expression(right, model.clone())?;
        }
        ExpressionNode::ConditionalOperator(left, right, other) => {
            validate_expression(left, model.clone())?;
            validate_expression(right, model.clone())?;
            validate_expression(other, model.clone())?;
        }
        ExpressionNode::Number(_) => {}
        ExpressionNode::Duration(_) => {}
        ExpressionNode::Rational(_, _) => {}
        ExpressionNode::String(_) => {}
        ExpressionNode::Type(_) => {}
        ExpressionNode::Address(_, _) => {}
        // Обращение к ячейке по адресу: направления у неё нет - проверять нечего.
        // Запись даёт **предупреждение** `SE-096`, а предупреждения вырабатывает слой
        // `semantic::warnings`, не судья.
        ExpressionNode::AnonPort(_) => {}
        ExpressionNode::Bool(_) => {}
        ExpressionNode::Variable(var_rc) => {
            // Чтение из `out`-порта запрещено (SE-027)
            if let VariableNode::Port {
                direction: PortDirection::Out,
                name,
                loc,
                ..
            } = &*var_rc.borrow()
            {
                return Err(Diagnostic::error(
                    *loc,
                    format!("Чтение из выходного порта '{}' запрещено", name),
                )
                .with_code("SE-027"));
            }
        }
        ExpressionNode::Model(_model) => {}
        ExpressionNode::Condition(cond) => {
            validate_cond(None, &cond.borrow().value, model.clone())?;
        }
        ExpressionNode::List(_) => {}
        // Вызов функции: сперва **арность**, затем аргументы.
        ExpressionNode::Function(def, exprs) => {
            if let Some(diagnostic) = super::arity::check_call(def, exprs) {
                return Err(diagnostic);
            }
            for expr in exprs {
                validate_expression(expr, model.clone())?;
            }
        }
        ExpressionNode::Array(exprs) | ExpressionNode::Initializer(exprs) => {
            for expr in exprs {
                validate_expression(expr, model.clone())?;
            }
        }
    }
    Ok(())
}

pub(super) fn validate_reference(
    reference: &ReferenceNode<StateNode>,
    model: Rc<RefCell<ModelNode>>,
) -> Result<(), Diagnostic> {
    validate_cond(None, &reference.cond, model.clone())?;
    Ok(())
}

pub(super) fn validate_conditions(model: Rc<RefCell<ModelNode>>) -> Vec<Diagnostic> {
    let borrowed = model.borrow();
    // Накопление по именованным условиям: каждое `cond` - своё объявление.
    let mut out = Vec::new();
    for cond in borrowed.conditions.values() {
        out.extend(validate_cond(None, &cond.value, model.clone()).err());
    }
    out
}

pub(super) fn get_state_name(state: &StateNode) -> &str {
    match state {
        StateNode::Simple { name, .. } | StateNode::Implement { name, .. } => name.as_str(),
        StateNode::Unresolved => "",
    }
}

pub(super) fn get_state_loc(state: &StateNode) -> Location {
    match state {
        StateNode::Simple { loc, .. } | StateNode::Implement { loc, .. } => *loc,
        StateNode::Unresolved => Location::Builtin,
    }
}

/// Имена состояний, достижимых из `state` за один переход: цели `ref`-ссылок и (для
/// состояния-реализации) цель `next`.
///
/// Общий источник истины о рёбрах графа FSM: используется анализом достижимости
/// (SE-046) и построением структуры Крипке).
pub(crate) fn reachable_targets(state: &StateNode) -> Vec<String> {
    match state {
        StateNode::Simple { references, .. } => references.iter().map(|r| r.name.clone()).collect(),
        StateNode::Implement {
            references, next, ..
        } => {
            let mut targets: Vec<String> = references.iter().map(|r| r.name.clone()).collect();
            if let Some(n) = next {
                targets.push(n.name.clone());
            }
            targets
        }
        StateNode::Unresolved => vec![],
    }
}
