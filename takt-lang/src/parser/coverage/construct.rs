//! Классификатор узла АСД: какую **конструкцию языка** он представляет.
//!
//! ## Правило модуля: никаких `_ =>` по узлам языка
//!
//! Разбор исчерпывающий, `#![deny(clippy::wildcard_enum_match_arm)]` не даёт написать
//! `_ =>`. Это замысел: конструкция, выпавшая из классификатора, не считается **ни
//! встреченной, ни требуемой** - то есть проверка покрытия молча перестал бы её проверять,
//! а именно от такого молчания фича и заведена. Новый узел языка обязан валить сборку
//! этого модуля.
//!
//! ## Форма имени
//!
//! Имя вида - `Перечисление::Вариант`, буква в букву как в [`crate::parser::ast`]: так
//! проверка сверяет **свой** ответ с объявлением АСД и ловит расхождение, а не хранит
//! второй список имён.
#![deny(clippy::wildcard_enum_match_arm)]

use super::super::ast;
use super::super::depth::NodeRef;

/// Кладёт в `out` виды конструкций, которые представляет узел `node`.
///
/// Видов бывает больше одного: узел несёт и собственный вариант, и "плоские" свойства,
/// отдельными узлами обхода не являющиеся - направление порта, вид цикла, паттерны
/// веток `match`, форму импорта. Пропустив их, проверка не заметил бы, что в корпусе нет ни
/// одного `inout` или ни одного `_` в `match`.
pub(super) fn classify(node: NodeRef<'_>, out: &mut Vec<&'static str>) {
    match node {
        NodeRef::Model(model) => {
            if model.implements.is_some() {
                out.push("Model::implements");
            }
        }
        NodeRef::Element(element) => element_kind(element, out),
        NodeRef::State(state) => {
            // Вид состояния необязателен: `state S { ... }` без слова `start` или `end` -
            // обычное состояние, и вариант в АСД отсутствует.
            if let Some(kind) = &state.kind {
                out.push(state_kind(kind));
            }
            if state.implements.is_some() {
                out.push("StateDefine::implements");
            }
        }
        NodeRef::StateElement(element) => out.push(state_element_kind(element)),
        NodeRef::Variable(variable) => variable_kind(variable, out),
        NodeRef::Function(function) => {
            out.push(if function.body.is_some() {
                "FunctionDefine::local"
            } else {
                "FunctionDefine::extern"
            });
        }
        NodeRef::Parameter(_) | NodeRef::NamedArgument(_) | NodeRef::FormulaBlock(_) => {}
        NodeRef::Type(ty) => out.push(type_kind(ty)),
        NodeRef::Statement(statement) => statement_kind(statement, out),
        NodeRef::MatchArm(arm) => {
            for pattern in &arm.patterns {
                out.push(match pattern {
                    ast::MatchPattern::Value(_) => "MatchPattern::Value",
                    ast::MatchPattern::Wildcard(_) => "MatchPattern::Wildcard",
                });
            }
        }
        NodeRef::Expression(expression) => expression_kind(expression, out),
        NodeRef::Condition(condition) => out.push(condition_kind(condition)),
        NodeRef::InlineFormula(formula) => out.push(match formula {
            ast::InlineFormulaDefine::Guard { .. } => "InlineFormulaDefine::Guard",
            ast::InlineFormulaDefine::Ltl { .. } => "InlineFormulaDefine::Ltl",
        }),
        NodeRef::Ltl(formula) => out.push(ltl_kind(formula)),
        NodeRef::FormulaStatement(statement) => out.push(match statement {
            ast::FormulaStatement::Expression(..) => "FormulaStatement::Expression",
            ast::FormulaStatement::Block(_) => "FormulaStatement::Block",
            ast::FormulaStatement::Function(_) => "FormulaStatement::Function",
            ast::FormulaStatement::Error(_) => "FormulaStatement::Error",
        }),
        NodeRef::FormulaExpression(expression) => out.push(formula_expression_kind(expression)),
        NodeRef::FormulaFunction(_) => {}
    }
}

/// Вид комментария: он живёт не в дереве, а рядом с ним (результат `parse`).
pub(super) fn comment_kind(comment: &ast::Comment) -> &'static str {
    match comment {
        ast::Comment::Line { .. } => "Comment::Line",
        ast::Comment::DocLine { .. } => "Comment::DocLine",
        ast::Comment::Block { .. } => "Comment::Block",
    }
}

/// Элемент модели; у импорта добирается его форма и форма пути.
fn element_kind(element: &ast::ModelElement, out: &mut Vec<&'static str>) {
    out.push(match element {
        ast::ModelElement::Import(_) => "ModelElement::Import",
        ast::ModelElement::Function(_) => "ModelElement::Function",
        ast::ModelElement::Formula(_) => "ModelElement::Formula",
        ast::ModelElement::Assembly(_) => "ModelElement::Assembly",
        ast::ModelElement::Condition(_) => "ModelElement::Condition",
        ast::ModelElement::Invariant(_) => "ModelElement::Invariant",
        ast::ModelElement::Variable(_) => "ModelElement::Variable",
        ast::ModelElement::Type(_) => "ModelElement::Type",
        ast::ModelElement::State(_) => "ModelElement::State",
        ast::ModelElement::Model(_) => "ModelElement::Model",
        ast::ModelElement::NamedBlockCode(_) => "ModelElement::NamedBlockCode",
        ast::ModelElement::StraySemicolon(_) => "ModelElement::StraySemicolon",
        ast::ModelElement::Enum(_) => "ModelElement::Enum",
        ast::ModelElement::Struct(_) => "ModelElement::Struct",
        ast::ModelElement::InlineFormula(_) => "ModelElement::InlineFormula",
        ast::ModelElement::Address(_) => "ModelElement::Address",
        ast::ModelElement::Clock(_) => "ModelElement::Clock",
    });
    if let ast::ModelElement::Import(import) = element {
        import_kind(import, out);
    }
}

/// Форма директивы импорта и форма её пути.
fn import_kind(import: &ast::ImportDefine, out: &mut Vec<&'static str>) {
    let path = match import {
        ast::ImportDefine::Plain(path, _) => {
            out.push("ImportDefine::Plain");
            path
        }
        ast::ImportDefine::GlobalSymbol(path, ..) => {
            out.push("ImportDefine::GlobalSymbol");
            path
        }
        ast::ImportDefine::Rename(path, ..) => {
            out.push("ImportDefine::Rename");
            path
        }
    };
    out.push(match path {
        ast::ImportPath::Filename(_) => "ImportPath::Filename",
        ast::ImportPath::Path(_) => "ImportPath::Path",
    });
}

/// Вид состояния: стартовое, конечное, обычное.
fn state_kind(kind: &ast::StateKind) -> &'static str {
    match kind {
        ast::StateKind::Start => "StateKind::Start",
        ast::StateKind::End => "StateKind::End",
        ast::StateKind::Next => "StateKind::Next",
    }
}

/// Элемент тела состояния.
fn state_element_kind(element: &ast::StateElement) -> &'static str {
    match element {
        ast::StateElement::Next(..) => "StateElement::Next",
        ast::StateElement::Reference(..) => "StateElement::Reference",
        ast::StateElement::NamedBlockCode(_) => "StateElement::NamedBlockCode",
        ast::StateElement::StraySemicolon(_) => "StateElement::StraySemicolon",
        ast::StateElement::InlineFormula(_) => "StateElement::InlineFormula",
        ast::StateElement::Formula(_) => "StateElement::Formula",
        ast::StateElement::Assembly(_) => "StateElement::Assembly",
        ast::StateElement::Invariant(_) => "StateElement::Invariant",
        ast::StateElement::Every(_) => "StateElement::Every",
    }
}

/// Вид объявления; у порта добирается направление.
fn variable_kind(variable: &ast::VariableDefine, out: &mut Vec<&'static str>) {
    out.push(match variable {
        ast::VariableDefine::Variable { .. } => "VariableDefine::Variable",
        ast::VariableDefine::Port { .. } => "VariableDefine::Port",
        ast::VariableDefine::Constant { .. } => "VariableDefine::Constant",
        ast::VariableDefine::Parameter { .. } => "VariableDefine::Parameter",
    });
    if let ast::VariableDefine::Port {
        direction, address, ..
    } = variable
    {
        out.push(match direction {
            ast::PortDirection::In => "PortDirection::In",
            ast::PortDirection::Out => "PortDirection::Out",
            ast::PortDirection::InOut => "PortDirection::InOut",
        });
        if address.is_some() {
            out.push("VariableDefine::Port::at");
        }
    }
}

/// Вид типа.
fn type_kind(ty: &ast::Type) -> &'static str {
    match ty {
        ast::Type::Address { .. } => "Type::Address",
        ast::Type::Bit => "Type::Bit",
        ast::Type::Bool => "Type::Bool",
        ast::Type::Rational => "Type::Rational",
        ast::Type::Duration => "Type::Duration",
        ast::Type::Fixed(..) => "Type::Fixed",
        ast::Type::Alias(_) => "Type::Alias",
        ast::Type::Array { .. } => "Type::Array",
        ast::Type::Enum(_) => "Type::Enum",
        ast::Type::Struct(_) => "Type::Struct",
        ast::Type::Function { .. } => "Type::Function",
        ast::Type::Unit => "Type::Unit",
    }
}

/// Вид оператора; у цикла добирается ключевое слово (`loop` или `while`).
fn statement_kind(statement: &ast::Statement, out: &mut Vec<&'static str>) {
    out.push(match statement {
        ast::Statement::Block { .. } => "Statement::Block",
        ast::Statement::Assembly { .. } => "Statement::Assembly",
        ast::Statement::Formula { .. } => "Statement::Formula",
        ast::Statement::Args(..) => "Statement::Args",
        ast::Statement::If(..) => "Statement::If",
        ast::Statement::Loop(..) => "Statement::Loop",
        ast::Statement::Expression(..) => "Statement::Expression",
        ast::Statement::Variable(..) => "Statement::Variable",
        ast::Statement::For(..) => "Statement::For",
        ast::Statement::Continue(_) => "Statement::Continue",
        ast::Statement::Break(_) => "Statement::Break",
        ast::Statement::Return(..) => "Statement::Return",
        ast::Statement::Error(_) => "Statement::Error",
        ast::Statement::StraySemicolon(_) => "Statement::StraySemicolon",
        ast::Statement::InlineFormula(_) => "Statement::InlineFormula",
        ast::Statement::Match(..) => "Statement::Match",
    });
    if let ast::Statement::If(_, _, _, Some(_)) = statement {
        out.push("Statement::If::else");
    }
    if let ast::Statement::Loop(_, _, _, keyword) = statement {
        out.push(match keyword {
            ast::LoopKeyword::Loop => "LoopKeyword::Loop",
            ast::LoopKeyword::While => "LoopKeyword::While",
        });
    }
}

/// Вид выражения; у доступа к члену добирается форма члена (`.имя` или `.0`).
fn expression_kind(expression: &ast::Expression, out: &mut Vec<&'static str>) {
    use ast::Expression as E;
    out.push(match expression {
        E::ArraySubscript(..) => "Expression::ArraySubscript",
        E::ArraySlice(..) => "Expression::ArraySlice",
        E::Parenthesis(..) => "Expression::Parenthesis",
        E::BitAccess(..) => "Expression::BitAccess",
        E::Function(..) => "Expression::Function",
        E::CodeBlock(..) => "Expression::CodeBlock",
        E::NamedFunction(..) => "Expression::NamedFunction",
        E::Not(..) => "Expression::Not",
        E::BitwiseNot(..) => "Expression::BitwiseNot",
        E::UnaryPlus(..) => "Expression::UnaryPlus",
        E::Negate(..) => "Expression::Negate",
        E::Power(..) => "Expression::Power",
        E::Multiply(..) => "Expression::Multiply",
        E::Divide(..) => "Expression::Divide",
        E::Modulo(..) => "Expression::Modulo",
        E::Add(..) => "Expression::Add",
        E::Subtract(..) => "Expression::Subtract",
        E::ShiftLeft(..) => "Expression::ShiftLeft",
        E::ShiftRight(..) => "Expression::ShiftRight",
        E::BitwiseAnd(..) => "Expression::BitwiseAnd",
        E::BitwiseXor(..) => "Expression::BitwiseXor",
        E::BitwiseOr(..) => "Expression::BitwiseOr",
        E::Less(..) => "Expression::Less",
        E::More(..) => "Expression::More",
        E::LessEqual(..) => "Expression::LessEqual",
        E::MoreEqual(..) => "Expression::MoreEqual",
        E::Equal(..) => "Expression::Equal",
        E::NotEqual(..) => "Expression::NotEqual",
        E::And(..) => "Expression::And",
        E::Or(..) => "Expression::Or",
        E::ConditionalOperator(..) => "Expression::ConditionalOperator",
        E::Assign(..) => "Expression::Assign",
        E::Number(..) => "Expression::Number",
        E::Duration(..) => "Expression::Duration",
        E::Rational(..) => "Expression::Rational",
        E::String(_) => "Expression::String",
        E::Type(..) => "Expression::Type",
        E::Address(..) => "Expression::Address",
        E::AnonAddress(..) => "Expression::AnonAddress",
        E::Bool(..) => "Expression::Bool",
        E::Variable(_) => "Expression::Variable",
        E::List(..) => "Expression::List",
        E::Array(..) => "Expression::Array",
        E::Initializer(..) => "Expression::Initializer",
        E::Cast(..) => "Expression::Cast",
    });
    if let E::BitAccess(_, _, member) = expression {
        out.push(member_kind(member));
    }
}

/// Форма доступа к члену: по имени или по номеру.
fn member_kind(member: &ast::Member) -> &'static str {
    match member {
        ast::Member::Identifier(_) => "Member::Identifier",
        ast::Member::Number(_) => "Member::Number",
    }
}

/// Вид условия перехода.
fn condition_kind(condition: &ast::Condition) -> &'static str {
    use ast::Condition as C;
    match condition {
        C::ArraySubscript(..) => "Condition::ArraySubscript",
        C::Parenthesis(..) => "Condition::Parenthesis",
        C::BitAccess(..) => "Condition::BitAccess",
        C::Function(..) => "Condition::Function",
        C::Not(..) => "Condition::Not",
        C::Add(..) => "Condition::Add",
        C::Subtract(..) => "Condition::Subtract",
        C::And(..) => "Condition::And",
        C::Or(..) => "Condition::Or",
        C::Less(..) => "Condition::Less",
        C::More(..) => "Condition::More",
        C::LessEqual(..) => "Condition::LessEqual",
        C::MoreEqual(..) => "Condition::MoreEqual",
        C::Equal(..) => "Condition::Equal",
        C::NotEqual(..) => "Condition::NotEqual",
        C::Number(..) => "Condition::Number",
        C::Duration(..) => "Condition::Duration",
        C::After(..) => "Condition::After",
        C::AfterTicks(..) => "Condition::AfterTicks",
        C::AfterExpr(..) => "Condition::AfterExpr",
        C::Rational(..) => "Condition::Rational",
        C::String(_) => "Condition::String",
        C::Bool(..) => "Condition::Bool",
        C::AnonAddress(..) => "Condition::AnonAddress",
        C::Variable(_) => "Condition::Variable",
    }
}

/// Вид узла LTL-формулы.
fn ltl_kind(formula: &ast::LtlExpr) -> &'static str {
    use ast::LtlExpr as L;
    match formula {
        L::True(_) => "LtlExpr::True",
        L::False(_) => "LtlExpr::False",
        L::Atom(_) => "LtlExpr::Atom",
        L::Not(..) => "LtlExpr::Not",
        L::Next(..) => "LtlExpr::Next",
        L::Finally(..) => "LtlExpr::Finally",
        L::Globally(..) => "LtlExpr::Globally",
        L::And(..) => "LtlExpr::And",
        L::Or(..) => "LtlExpr::Or",
        L::Until(..) => "LtlExpr::Until",
        L::Release(..) => "LtlExpr::Release",
        L::Implies(..) => "LtlExpr::Implies",
        L::Parenthesis(..) => "LtlExpr::Parenthesis",
    }
}

/// Вид выражения внутри блока `formula`.
fn formula_expression_kind(expression: &ast::FormulaExpression) -> &'static str {
    use ast::FormulaExpression as F;
    match expression {
        F::Bool(..) => "FormulaExpression::Bool",
        F::Number(..) => "FormulaExpression::Number",
        F::String(..) => "FormulaExpression::String",
        F::Variable(_) => "FormulaExpression::Variable",
        F::Function(_) => "FormulaExpression::Function",
        F::SuffixAccess(..) => "FormulaExpression::SuffixAccess",
        F::Parenthesis(..) => "FormulaExpression::Parenthesis",
    }
}
