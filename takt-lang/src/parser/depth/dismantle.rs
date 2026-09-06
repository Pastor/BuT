//! Безопасная утилизация отвергнутого дерева АСД.
//!
//! ## Как устроено
//!
//! Дерево разбирается **итеративно**: узел снимается с рабочего стека (он в куче), его
//! дети **перемещаются** в тот же стек, после чего сам узел разрушается уже плоским -
//! рекурсии `Drop` достаётся один уровень. Заглушки не нужны: дети вынимаются по
//! значению, а не заменяются.
//!
//! ## Правило модуля: никаких `_ =>` по узлам языка
//!
//! `#![deny(clippy::wildcard_enum_match_arm)]`, как и в счётчике глубины: узел,
//! выпавший из разбора, уничтожался бы рекурсивно - то есть дефект вернулся бы молча,
//! на самом глубоком вводе.
#![deny(clippy::wildcard_enum_match_arm)]

use crate::parser::ast;

/// Узел АСД, взятый во владение, - элемент рабочего стека утилизации.
enum Owned {
    Model(ast::Model),
    Element(ast::ModelElement),
    State(ast::StateDefine),
    StateElement(ast::StateElement),
    Variable(ast::VariableDefine),
    Function(ast::FunctionDefine),
    Parameter(ast::Parameter),
    Type(ast::Type),
    Statement(ast::Statement),
    MatchArm(ast::MatchArm),
    NamedArgument(ast::NamedArgument),
    Expression(ast::Expression),
    Condition(ast::Condition),
    InlineFormula(ast::InlineFormulaDefine),
    Ltl(ast::LtlExpr),
    FormulaBlock(ast::FormulaBlock),
    FormulaStatement(ast::FormulaStatement),
    FormulaExpression(ast::FormulaExpression),
    FormulaFunction(ast::FormulaFunction),
}

/// Разбирает дерево на плоские узлы и уничтожает их без рекурсии.
pub(crate) fn dismantle(root: ast::Model) {
    let mut stack = vec![Owned::Model(root)];
    while let Some(node) = stack.pop() {
        take_children(node, &mut stack);
    }
}

/// Перемещает детей узла в стек; сам узел разрушается по выходу из функции.
fn take_children(node: Owned, out: &mut Vec<Owned>) {
    match node {
        Owned::Model(model) => {
            if let Some(implements) = model.implements {
                out.push(Owned::Expression(implements));
            }
            out.extend(model.elements.into_iter().map(Owned::Element));
        }
        Owned::Element(element) => take_element(element, out),
        Owned::State(state) => {
            if let Some(implements) = state.implements {
                out.push(Owned::Expression(implements));
            }
            out.extend(state.elements.into_iter().map(Owned::StateElement));
        }
        Owned::StateElement(element) => take_state_element(element, out),
        Owned::Variable(variable) => take_variable(variable, out),
        Owned::Function(function) => {
            take_parameter_list(function.params, out);
            if let Some(ty) = function.return_type {
                out.push(Owned::Type(ty));
            }
            if let Some(body) = function.body {
                out.push(Owned::Statement(body));
            }
        }
        Owned::Parameter(parameter) => out.push(Owned::Expression(parameter.ty)),
        Owned::Type(ty) => take_type(ty, out),
        Owned::Statement(statement) => take_statement(statement, out),
        Owned::MatchArm(arm) => {
            for pattern in arm.patterns {
                match pattern {
                    ast::MatchPattern::Value(expression) => {
                        out.push(Owned::Expression(expression));
                    }
                    ast::MatchPattern::Wildcard(_) => {}
                }
            }
            out.push(Owned::Statement(*arm.body));
        }
        Owned::NamedArgument(argument) => out.push(Owned::Expression(argument.expr)),
        Owned::Expression(expression) => take_expression(expression, out),
        Owned::Condition(condition) => take_condition(condition, out),
        Owned::InlineFormula(formula) => match formula {
            ast::InlineFormulaDefine::Guard { conditions, .. } => {
                out.extend(conditions.into_iter().map(Owned::Condition));
            }
            ast::InlineFormulaDefine::Ltl { formulas, .. } => {
                out.extend(formulas.into_iter().map(Owned::Ltl));
            }
        },
        Owned::Ltl(formula) => take_ltl(formula, out),
        Owned::FormulaBlock(block) => {
            out.extend(block.statements.into_iter().map(Owned::FormulaStatement));
        }
        Owned::FormulaStatement(statement) => match statement {
            ast::FormulaStatement::Expression(_, expression) => {
                out.push(Owned::FormulaExpression(expression));
            }
            ast::FormulaStatement::Block(block) => out.push(Owned::FormulaBlock(block)),
            ast::FormulaStatement::Function(function) => {
                out.push(Owned::FormulaFunction(*function));
            }
            ast::FormulaStatement::Error(_) => {}
        },
        Owned::FormulaExpression(expression) => take_formula_expression(expression, out),
        Owned::FormulaFunction(function) => {
            out.extend(function.arguments.into_iter().map(Owned::FormulaExpression));
        }
    }
}

/// Разбирает элемент модели.
fn take_element(element: ast::ModelElement, out: &mut Vec<Owned>) {
    match element {
        ast::ModelElement::Function(function) => out.push(Owned::Function(*function)),
        ast::ModelElement::Formula(formula) => out.push(Owned::FormulaBlock(formula.formula)),
        ast::ModelElement::Assembly(block) => out.push(Owned::Statement(*block)),
        ast::ModelElement::Condition(condition) => out.push(Owned::Condition(condition.value)),
        ast::ModelElement::Invariant(invariant) => out.push(Owned::Condition(invariant.value)),
        ast::ModelElement::Variable(variable) => out.push(Owned::Variable(*variable)),
        ast::ModelElement::Type(ty) => out.push(Owned::Type(ty.ty)),
        ast::ModelElement::State(state) => out.push(Owned::State(*state)),
        ast::ModelElement::Model(model) => out.push(Owned::Model(*model)),
        ast::ModelElement::NamedBlockCode(block) => out.push(Owned::Statement(block.statement)),
        ast::ModelElement::Struct(structure) => {
            out.extend(
                structure
                    .fields
                    .into_iter()
                    .map(|field| Owned::Type(field.ty)),
            );
        }
        ast::ModelElement::InlineFormula(formula) => out.push(Owned::InlineFormula(*formula)),
        ast::ModelElement::Address(address) => out.push(Owned::Expression(address.value)),
        // Листовые: вложенных узлов не несут.
        ast::ModelElement::Import(_)
        | ast::ModelElement::StraySemicolon(_)
        | ast::ModelElement::Enum(_)
        | ast::ModelElement::Clock(_) => {}
    }
}

/// Разбирает элемент состояния.
fn take_state_element(element: ast::StateElement, out: &mut Vec<Owned>) {
    match element {
        ast::StateElement::Reference(_, _, condition) => {
            if let Some(condition) = condition {
                out.push(Owned::Condition(condition));
            }
        }
        ast::StateElement::NamedBlockCode(block) => out.push(Owned::Statement(block.statement)),
        ast::StateElement::InlineFormula(formula) => out.push(Owned::InlineFormula(*formula)),
        // Обязательство и вставка уровня состояния.
        ast::StateElement::Formula(formula) => out.push(Owned::FormulaBlock(formula.formula)),
        ast::StateElement::Assembly(block) => out.push(Owned::Statement(*block)),
        ast::StateElement::Invariant(invariant) => out.push(Owned::Condition(invariant.value)),
        ast::StateElement::Every(every) => out.push(Owned::Statement(every.body)),
        // Листовые.
        ast::StateElement::Next(_) | ast::StateElement::StraySemicolon(_) => {}
    }
}

/// Разбирает объявление переменной, константы, параметра или порта.
fn take_variable(variable: ast::VariableDefine, out: &mut Vec<Owned>) {
    match variable {
        ast::VariableDefine::Variable {
            typ, initializer, ..
        } => {
            if let Some(typ) = typ {
                out.push(Owned::Type(typ));
            }
            if let Some(initializer) = initializer {
                out.push(Owned::Expression(initializer));
            }
        }
        // Два необязательных выражения: размещение и инициализатор. Невынутое поддерево
        // уничтожалось бы рекурсивным `Drop`.
        ast::VariableDefine::Port {
            typ,
            address,
            initializer,
            ..
        } => {
            if let Some(typ) = typ {
                out.push(Owned::Type(typ));
            }
            if let Some(address) = address {
                out.push(Owned::Expression(address));
            }
            if let Some(initializer) = initializer {
                out.push(Owned::Expression(initializer));
            }
        }
        ast::VariableDefine::Constant {
            typ, initializer, ..
        }
        | ast::VariableDefine::Parameter {
            typ, initializer, ..
        } => {
            if let Some(typ) = typ {
                out.push(Owned::Type(typ));
            }
            out.push(Owned::Expression(initializer));
        }
    }
}

/// Разбирает список параметров.
fn take_parameter_list(params: ast::ParameterList, out: &mut Vec<Owned>) {
    out.extend(
        params
            .into_iter()
            .filter_map(|(_, parameter)| parameter.map(Owned::Parameter)),
    );
}

/// Разбирает тип.
fn take_type(ty: ast::Type, out: &mut Vec<Owned>) {
    match ty {
        ast::Type::Array { element_type, .. } => out.push(Owned::Type(*element_type)),
        ast::Type::Function { params, returns } => {
            take_parameter_list(params, out);
            if let Some(returns) = returns {
                take_parameter_list(returns, out);
            }
        }
        // Листовые.
        ast::Type::Address { .. }
        | ast::Type::Bit
        | ast::Type::Bool
        | ast::Type::Rational
        | ast::Type::Duration
        | ast::Type::Fixed(..)
        | ast::Type::Alias(_)
        | ast::Type::Enum(_)
        | ast::Type::Struct(_)
        | ast::Type::Unit => {}
    }
}

/// Разбирает оператор.
fn take_statement(statement: ast::Statement, out: &mut Vec<Owned>) {
    match statement {
        ast::Statement::Block { statements, .. } => {
            out.extend(statements.into_iter().map(Owned::Statement));
        }
        ast::Statement::Assembly { block, .. } => out.push(Owned::Statement(*block)),
        ast::Statement::Formula { block, .. } => out.push(Owned::FormulaBlock(*block)),
        ast::Statement::Args(_, arguments) => {
            out.extend(arguments.into_iter().map(Owned::NamedArgument));
        }
        ast::Statement::If(_, condition, then_branch, else_branch) => {
            out.push(Owned::Expression(condition));
            out.push(Owned::Statement(*then_branch));
            if let Some(else_branch) = else_branch {
                out.push(Owned::Statement(*else_branch));
            }
        }
        ast::Statement::Loop(_, condition, body, _) => {
            if let Some(condition) = condition {
                out.push(Owned::Expression(condition));
            }
            out.push(Owned::Statement(*body));
        }
        ast::Statement::Expression(_, expression) => out.push(Owned::Expression(expression)),
        ast::Statement::Variable(_, variable, initializer) => {
            out.push(Owned::Variable(*variable));
            if let Some(initializer) = initializer {
                out.push(Owned::Expression(initializer));
            }
        }
        ast::Statement::For(_, init, condition, step, body) => {
            if let Some(init) = init {
                out.push(Owned::Statement(*init));
            }
            if let Some(condition) = condition {
                out.push(Owned::Expression(*condition));
            }
            if let Some(step) = step {
                out.push(Owned::Expression(*step));
            }
            if let Some(body) = body {
                out.push(Owned::Statement(*body));
            }
        }
        ast::Statement::Return(_, value) => {
            if let Some(value) = value {
                out.push(Owned::Expression(value));
            }
        }
        ast::Statement::InlineFormula(formula) => out.push(Owned::InlineFormula(*formula)),
        ast::Statement::Match(_, subject, arms) => {
            out.push(Owned::Expression(*subject));
            out.extend(arms.into_iter().map(Owned::MatchArm));
        }
        // Листовые.
        ast::Statement::Continue(_)
        | ast::Statement::Break(_)
        | ast::Statement::Error(_)
        | ast::Statement::StraySemicolon(_) => {}
    }
}

/// Разбирает выражение.
fn take_expression(expression: ast::Expression, out: &mut Vec<Owned>) {
    use ast::Expression as E;
    match expression {
        // Унарные.
        E::ArraySubscript(_, _, operand)
        | E::Parenthesis(_, operand)
        | E::BitAccess(_, operand, _)
        | E::Not(_, operand)
        | E::BitwiseNot(_, operand)
        | E::UnaryPlus(_, operand)
        | E::Negate(_, operand) => out.push(Owned::Expression(*operand)),

        // Бинарные.
        E::Power(_, left, right)
        | E::Multiply(_, left, right)
        | E::Divide(_, left, right)
        | E::Modulo(_, left, right)
        | E::Add(_, left, right)
        | E::Subtract(_, left, right)
        | E::ShiftLeft(_, left, right)
        | E::ShiftRight(_, left, right)
        | E::BitwiseAnd(_, left, right)
        | E::BitwiseXor(_, left, right)
        | E::BitwiseOr(_, left, right)
        | E::Less(_, left, right)
        | E::More(_, left, right)
        | E::LessEqual(_, left, right)
        | E::MoreEqual(_, left, right)
        | E::Equal(_, left, right)
        | E::NotEqual(_, left, right)
        | E::And(_, left, right)
        | E::Or(_, left, right)
        | E::Assign(_, left, right) => {
            out.push(Owned::Expression(*left));
            out.push(Owned::Expression(*right));
        }

        E::ConditionalOperator(_, condition, then_branch, else_branch) => {
            out.push(Owned::Expression(*condition));
            out.push(Owned::Expression(*then_branch));
            out.push(Owned::Expression(*else_branch));
        }
        E::Function(_, _, arguments) | E::Array(_, arguments) | E::Initializer(_, arguments) => {
            out.extend(arguments.into_iter().map(Owned::Expression));
        }
        E::CodeBlock(_, subject, block) => {
            out.push(Owned::Expression(*subject));
            out.push(Owned::Statement(*block));
        }
        E::NamedFunction(_, subject, arguments) => {
            out.push(Owned::Expression(*subject));
            out.extend(arguments.into_iter().map(Owned::NamedArgument));
        }
        E::List(_, params) => take_parameter_list(params, out),
        E::Cast(_, operand, ty) => {
            out.push(Owned::Expression(*operand));
            out.push(Owned::Type(ty));
        }
        E::Type(_, ty) => out.push(Owned::Type(ty)),

        // Листовые.
        E::ArraySlice(..)
        | E::Number(..)
        | E::Duration(..)
        | E::Rational(..)
        | E::String(_)
        | E::Address(..)
        | E::AnonAddress(..)
        | E::Bool(..)
        | E::Variable(_) => {}
    }
}

/// Разбирает условие перехода.
fn take_condition(condition: ast::Condition, out: &mut Vec<Owned>) {
    use ast::Condition as C;
    match condition {
        // Унарные.
        C::ArraySubscript(_, _, operand)
        | C::Parenthesis(_, operand)
        | C::BitAccess(_, operand, _)
        | C::Not(_, operand)
        | C::AfterExpr(_, operand) => out.push(Owned::Condition(*operand)),

        // Бинарные.
        C::Add(_, left, right)
        | C::Subtract(_, left, right)
        | C::And(_, left, right)
        | C::Or(_, left, right)
        | C::Less(_, left, right)
        | C::More(_, left, right)
        | C::LessEqual(_, left, right)
        | C::MoreEqual(_, left, right)
        | C::Equal(_, left, right)
        | C::NotEqual(_, left, right) => {
            out.push(Owned::Condition(*left));
            out.push(Owned::Condition(*right));
        }

        C::Function(_, _, arguments) => {
            out.extend(arguments.into_iter().map(Owned::Condition));
        }

        // Листовые.
        C::Number(..)
        | C::Duration(..)
        | C::After(..)
        | C::AfterTicks(..)
        | C::Rational(..)
        | C::String(_)
        | C::AnonAddress(..)
        | C::Bool(..)
        | C::Variable(_) => {}
    }
}

/// Разбирает формулу LTL.
fn take_ltl(formula: ast::LtlExpr, out: &mut Vec<Owned>) {
    use ast::LtlExpr as L;
    match formula {
        L::Not(_, operand)
        | L::Next(_, operand)
        | L::Finally(_, operand)
        | L::Globally(_, operand)
        | L::Parenthesis(_, operand) => out.push(Owned::Ltl(*operand)),

        L::And(_, left, right)
        | L::Or(_, left, right)
        | L::Until(_, left, right)
        | L::Release(_, left, right)
        | L::Implies(_, left, right) => {
            out.push(Owned::Ltl(*left));
            out.push(Owned::Ltl(*right));
        }

        // Листовые.
        L::True(_) | L::False(_) | L::Atom(_) => {}
    }
}

/// Разбирает выражение формулы.
fn take_formula_expression(expression: ast::FormulaExpression, out: &mut Vec<Owned>) {
    use ast::FormulaExpression as F;
    match expression {
        F::Function(function) => out.push(Owned::FormulaFunction(*function)),
        F::SuffixAccess(_, operand, _) | F::Parenthesis(_, operand) => {
            out.push(Owned::FormulaExpression(*operand));
        }
        // Листовые.
        F::Bool(..) | F::Number(..) | F::String(..) | F::Variable(_) => {}
    }
}
