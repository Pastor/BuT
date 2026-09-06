//! Раскрытие узла АСД в его дочерние узлы.
//!
//! ## Правило модуля: никаких `_ =>` по узлам языка
//!
//! Разбор исчерпывающий, `#![deny(clippy::wildcard_enum_match_arm)]` не даёт написать
//! `_ =>`. Это замысел: узел, выпавший из раскрытия, **не считается** при измерении
//! глубины - а значит, дерево произвольной глубины прошло бы проверку и уронило первого
//! же рекурсивного потребителя (печать форматтера, `Clone`, `Drop`). Новый узел языка
//! обязан валить сборку этого модуля.
//!
//! Ветки листовых узлов (литералы, `break`, `continue`, имена) перечисляются явно - так
//! видно, что узел рассмотрен, а не забыт.
#![deny(clippy::wildcard_enum_match_arm)]

use super::NodeRef;
use crate::parser::ast;

/// Кладёт дочерние узлы `node` в рабочий стек обхода.
///
/// Порядок укладки значения не имеет: обход ищет максимум глубины, а не первое
/// вхождение в порядке текста.
pub(crate) fn push_children<'a>(node: NodeRef<'a>, out: &mut Vec<NodeRef<'a>>) {
    match node {
        NodeRef::Model(model) => {
            if let Some(implements) = &model.implements {
                out.push(NodeRef::Expression(implements));
            }
            out.extend(model.elements.iter().map(NodeRef::Element));
        }
        NodeRef::Element(element) => push_element(element, out),
        NodeRef::State(state) => {
            if let Some(implements) = &state.implements {
                out.push(NodeRef::Expression(implements));
            }
            out.extend(state.elements.iter().map(NodeRef::StateElement));
        }
        NodeRef::StateElement(element) => push_state_element(element, out),
        NodeRef::Variable(variable) => push_variable(variable, out),
        NodeRef::Function(function) => {
            push_parameter_list(&function.params, out);
            if let Some(ty) = &function.return_type {
                out.push(NodeRef::Type(ty));
            }
            if let Some(body) = &function.body {
                out.push(NodeRef::Statement(body));
            }
        }
        NodeRef::Parameter(parameter) => out.push(NodeRef::Expression(&parameter.ty)),
        NodeRef::Type(ty) => push_type(ty, out),
        NodeRef::Statement(statement) => push_statement(statement, out),
        NodeRef::MatchArm(arm) => {
            for pattern in &arm.patterns {
                match pattern {
                    ast::MatchPattern::Value(expression) => {
                        out.push(NodeRef::Expression(expression));
                    }
                    ast::MatchPattern::Wildcard(_) => {}
                }
            }
            out.push(NodeRef::Statement(&arm.body));
        }
        NodeRef::NamedArgument(argument) => out.push(NodeRef::Expression(&argument.expr)),
        NodeRef::Expression(expression) => push_expression(expression, out),
        NodeRef::Condition(condition) => push_condition(condition, out),
        NodeRef::InlineFormula(formula) => match formula {
            ast::InlineFormulaDefine::Guard { conditions, .. } => {
                out.extend(conditions.iter().map(NodeRef::Condition));
            }
            ast::InlineFormulaDefine::Ltl { formulas, .. } => {
                out.extend(formulas.iter().map(NodeRef::Ltl));
            }
        },
        NodeRef::Ltl(formula) => push_ltl(formula, out),
        NodeRef::FormulaBlock(block) => {
            out.extend(block.statements.iter().map(NodeRef::FormulaStatement));
        }
        NodeRef::FormulaStatement(statement) => match statement {
            ast::FormulaStatement::Expression(_, expression) => {
                out.push(NodeRef::FormulaExpression(expression));
            }
            ast::FormulaStatement::Block(block) => out.push(NodeRef::FormulaBlock(block)),
            ast::FormulaStatement::Function(function) => {
                out.push(NodeRef::FormulaFunction(function));
            }
            ast::FormulaStatement::Error(_) => {}
        },
        NodeRef::FormulaExpression(expression) => push_formula_expression(expression, out),
        NodeRef::FormulaFunction(function) => {
            out.extend(function.arguments.iter().map(NodeRef::FormulaExpression));
        }
    }
}

/// Раскрывает элемент модели.
fn push_element<'a>(element: &'a ast::ModelElement, out: &mut Vec<NodeRef<'a>>) {
    match element {
        ast::ModelElement::Function(function) => out.push(NodeRef::Function(function)),
        ast::ModelElement::Formula(formula) => out.push(NodeRef::FormulaBlock(&formula.formula)),
        ast::ModelElement::Assembly(block) => out.push(NodeRef::Statement(block)),
        ast::ModelElement::Condition(condition) => out.push(NodeRef::Condition(&condition.value)),
        ast::ModelElement::Invariant(invariant) => out.push(NodeRef::Condition(&invariant.value)),
        ast::ModelElement::Variable(variable) => out.push(NodeRef::Variable(variable)),
        ast::ModelElement::Type(ty) => out.push(NodeRef::Type(&ty.ty)),
        ast::ModelElement::State(state) => out.push(NodeRef::State(state)),
        ast::ModelElement::Model(model) => out.push(NodeRef::Model(model)),
        ast::ModelElement::NamedBlockCode(block) => out.push(NodeRef::Statement(&block.statement)),
        ast::ModelElement::Struct(structure) => {
            out.extend(
                structure
                    .fields
                    .iter()
                    .map(|field| NodeRef::Type(&field.ty)),
            );
        }
        ast::ModelElement::InlineFormula(formula) => out.push(NodeRef::InlineFormula(formula)),
        ast::ModelElement::Address(address) => out.push(NodeRef::Expression(&address.value)),
        // Листовые: вложенных узлов не несут - только имена, литералы и пути.
        ast::ModelElement::Import(_)
        | ast::ModelElement::StraySemicolon(_)
        | ast::ModelElement::Enum(_)
        | ast::ModelElement::Clock(_) => {}
    }
}

/// Раскрывает элемент состояния.
fn push_state_element<'a>(element: &'a ast::StateElement, out: &mut Vec<NodeRef<'a>>) {
    match element {
        ast::StateElement::Reference(_, _, condition) => {
            if let Some(condition) = condition {
                out.push(NodeRef::Condition(condition));
            }
        }
        ast::StateElement::NamedBlockCode(block) => out.push(NodeRef::Statement(&block.statement)),
        // Обязательство и вставка уровня состояния.
        ast::StateElement::Formula(formula) => out.push(NodeRef::FormulaBlock(&formula.formula)),
        ast::StateElement::Assembly(block) => out.push(NodeRef::Statement(block)),
        ast::StateElement::InlineFormula(formula) => out.push(NodeRef::InlineFormula(formula)),
        ast::StateElement::Invariant(invariant) => out.push(NodeRef::Condition(&invariant.value)),
        ast::StateElement::Every(every) => out.push(NodeRef::Statement(&every.body)),
        // Листовые: имя следующего состояния и одиночная `;`.
        ast::StateElement::Next(_) | ast::StateElement::StraySemicolon(_) => {}
    }
}

/// Раскрывает объявление переменной, константы, параметра или порта.
fn push_variable<'a>(variable: &'a ast::VariableDefine, out: &mut Vec<NodeRef<'a>>) {
    match variable {
        ast::VariableDefine::Variable {
            typ, initializer, ..
        } => {
            if let Some(typ) = typ {
                out.push(NodeRef::Type(typ));
            }
            if let Some(initializer) = initializer {
                out.push(NodeRef::Expression(initializer));
            }
        }
        // Порт несёт два необязательных выражения: размещение `at <адрес>` и
        // инициализатор. Пропустить размещение значит не измерить его глубину - а
        // дерево глубже предела выпустить наружу.
        ast::VariableDefine::Port {
            typ,
            address,
            initializer,
            ..
        } => {
            if let Some(typ) = typ {
                out.push(NodeRef::Type(typ));
            }
            if let Some(address) = address {
                out.push(NodeRef::Expression(address));
            }
            if let Some(initializer) = initializer {
                out.push(NodeRef::Expression(initializer));
            }
        }
        ast::VariableDefine::Constant {
            typ, initializer, ..
        }
        | ast::VariableDefine::Parameter {
            typ, initializer, ..
        } => {
            if let Some(typ) = typ {
                out.push(NodeRef::Type(typ));
            }
            out.push(NodeRef::Expression(initializer));
        }
    }
}

/// Раскрывает список параметров (`ParameterList`).
fn push_parameter_list<'a>(params: &'a ast::ParameterList, out: &mut Vec<NodeRef<'a>>) {
    out.extend(
        params
            .iter()
            .filter_map(|(_, parameter)| parameter.as_ref().map(NodeRef::Parameter)),
    );
}

/// Раскрывает тип.
fn push_type<'a>(ty: &'a ast::Type, out: &mut Vec<NodeRef<'a>>) {
    match ty {
        ast::Type::Array { element_type, .. } => out.push(NodeRef::Type(element_type)),
        ast::Type::Function { params, returns } => {
            push_parameter_list(params, out);
            if let Some(returns) = returns {
                push_parameter_list(returns, out);
            }
        }
        // Листовые: примитивы, ссылки по имени и адресный тип.
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

/// Раскрывает оператор.
fn push_statement<'a>(statement: &'a ast::Statement, out: &mut Vec<NodeRef<'a>>) {
    match statement {
        ast::Statement::Block { statements, .. } => {
            out.extend(statements.iter().map(NodeRef::Statement));
        }
        ast::Statement::Assembly { block, .. } => out.push(NodeRef::Statement(block)),
        ast::Statement::Formula { block, .. } => out.push(NodeRef::FormulaBlock(block)),
        ast::Statement::Args(_, arguments) => {
            out.extend(arguments.iter().map(NodeRef::NamedArgument));
        }
        ast::Statement::If(_, condition, then_branch, else_branch) => {
            out.push(NodeRef::Expression(condition));
            out.push(NodeRef::Statement(then_branch));
            if let Some(else_branch) = else_branch {
                out.push(NodeRef::Statement(else_branch));
            }
        }
        ast::Statement::Loop(_, condition, body, _) => {
            if let Some(condition) = condition {
                out.push(NodeRef::Expression(condition));
            }
            out.push(NodeRef::Statement(body));
        }
        ast::Statement::Expression(_, expression) => out.push(NodeRef::Expression(expression)),
        ast::Statement::Variable(_, variable, initializer) => {
            out.push(NodeRef::Variable(variable));
            if let Some(initializer) = initializer {
                out.push(NodeRef::Expression(initializer));
            }
        }
        ast::Statement::For(_, init, condition, step, body) => {
            if let Some(init) = init {
                out.push(NodeRef::Statement(init));
            }
            if let Some(condition) = condition {
                out.push(NodeRef::Expression(condition));
            }
            if let Some(step) = step {
                out.push(NodeRef::Expression(step));
            }
            if let Some(body) = body {
                out.push(NodeRef::Statement(body));
            }
        }
        ast::Statement::Return(_, value) => {
            if let Some(value) = value {
                out.push(NodeRef::Expression(value));
            }
        }
        ast::Statement::InlineFormula(formula) => out.push(NodeRef::InlineFormula(formula)),
        ast::Statement::Match(_, subject, arms) => {
            out.push(NodeRef::Expression(subject));
            out.extend(arms.iter().map(NodeRef::MatchArm));
        }
        // Листовые: переходы управления, узел ошибки и одиночная `;`.
        ast::Statement::Continue(_)
        | ast::Statement::Break(_)
        | ast::Statement::Error(_)
        | ast::Statement::StraySemicolon(_) => {}
    }
}

/// Раскрывает выражение.
fn push_expression<'a>(expression: &'a ast::Expression, out: &mut Vec<NodeRef<'a>>) {
    use ast::Expression as E;
    match expression {
        // Унарные.
        E::ArraySubscript(_, _, operand)
        | E::Parenthesis(_, operand)
        | E::BitAccess(_, operand, _)
        | E::Not(_, operand)
        | E::BitwiseNot(_, operand)
        | E::UnaryPlus(_, operand)
        | E::Negate(_, operand) => out.push(NodeRef::Expression(operand)),

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
            out.push(NodeRef::Expression(left));
            out.push(NodeRef::Expression(right));
        }

        E::ConditionalOperator(_, condition, then_branch, else_branch) => {
            out.push(NodeRef::Expression(condition));
            out.push(NodeRef::Expression(then_branch));
            out.push(NodeRef::Expression(else_branch));
        }
        E::Function(_, _, arguments) | E::Array(_, arguments) | E::Initializer(_, arguments) => {
            out.extend(arguments.iter().map(NodeRef::Expression));
        }
        E::CodeBlock(_, subject, block) => {
            out.push(NodeRef::Expression(subject));
            out.push(NodeRef::Statement(block));
        }
        E::NamedFunction(_, subject, arguments) => {
            out.push(NodeRef::Expression(subject));
            out.extend(arguments.iter().map(NodeRef::NamedArgument));
        }
        E::List(_, params) => push_parameter_list(params, out),
        E::Cast(_, operand, ty) => {
            out.push(NodeRef::Expression(operand));
            out.push(NodeRef::Type(ty));
        }
        E::Type(_, ty) => out.push(NodeRef::Type(ty)),

        // Листовые: литералы, срез (границы - числа) и ссылка на переменную.
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

/// Раскрывает условие перехода.
fn push_condition<'a>(condition: &'a ast::Condition, out: &mut Vec<NodeRef<'a>>) {
    use ast::Condition as C;
    match condition {
        // Унарные.
        C::ArraySubscript(_, _, operand)
        | C::Parenthesis(_, operand)
        | C::BitAccess(_, operand, _)
        | C::Not(_, operand)
        | C::AfterExpr(_, operand) => out.push(NodeRef::Condition(operand)),

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
            out.push(NodeRef::Condition(left));
            out.push(NodeRef::Condition(right));
        }

        C::Function(_, _, arguments) => {
            out.extend(arguments.iter().map(NodeRef::Condition));
        }

        // Листовые: литералы, выдержки с готовым значением и ссылка на переменную.
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

/// Раскрывает формулу LTL.
fn push_ltl<'a>(formula: &'a ast::LtlExpr, out: &mut Vec<NodeRef<'a>>) {
    use ast::LtlExpr as L;
    match formula {
        L::Not(_, operand)
        | L::Next(_, operand)
        | L::Finally(_, operand)
        | L::Globally(_, operand)
        | L::Parenthesis(_, operand) => out.push(NodeRef::Ltl(operand)),

        L::And(_, left, right)
        | L::Or(_, left, right)
        | L::Until(_, left, right)
        | L::Release(_, left, right)
        | L::Implies(_, left, right) => {
            out.push(NodeRef::Ltl(left));
            out.push(NodeRef::Ltl(right));
        }

        // Листовые: константы и атом.
        L::True(_) | L::False(_) | L::Atom(_) => {}
    }
}

/// Раскрывает выражение формулы.
fn push_formula_expression<'a>(expression: &'a ast::FormulaExpression, out: &mut Vec<NodeRef<'a>>) {
    use ast::FormulaExpression as F;
    match expression {
        F::Function(function) => out.push(NodeRef::FormulaFunction(function)),
        F::SuffixAccess(_, operand, _) | F::Parenthesis(_, operand) => {
            out.push(NodeRef::FormulaExpression(operand));
        }
        // Листовые: литералы с аннотацией типа и ссылка на переменную.
        F::Bool(..) | F::Number(..) | F::String(..) | F::Variable(_) => {}
    }
}
