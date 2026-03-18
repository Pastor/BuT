use but_grammar::ast::Loc;
use but_grammar::diagnostics::Diagnostic;
use std::fmt::Display;

#[derive(Debug, Clone)]
pub enum Expression {
    Identifier(String),
    Parentheses(Box<Expression>),
    Or(Box<Self>, Box<Self>),
    Add(Box<Self>, Box<Self>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Expression::Identifier(id) => id.clone(),
            Expression::Or(left, right) => left.to_string() + " | " + &right.to_string(),
            Expression::Add(left, right) => left.to_string() + " + " + &right.to_string(),
            Expression::Parentheses(ex) => "( ".to_string() + &ex.to_string() + " )",
        };
        write!(f, "{}", str)
    }
}

pub fn transform(expression: but_grammar::ast::Expression) -> Result<Expression, Vec<Diagnostic>> {
    if let but_grammar::ast::Expression::BitwiseOr(_, left, right) = expression {
        let left = transform(*left)?;
        let right = transform(*right)?;
        Ok(Expression::Or(Box::new(left), Box::new(right)))
    } else if let but_grammar::ast::Expression::Add(_, left, right) = expression {
        let left = transform(*left)?;
        let right = transform(*right)?;
        Ok(Expression::Add(Box::new(left), Box::new(right)))
    } else if let but_grammar::ast::Expression::Variable(id) = expression {
        Ok(Expression::Identifier(id.name.clone()))
    } else if let but_grammar::ast::Expression::Parenthesis(_, ex) = expression {
        Ok(Expression::Parentheses(Box::new(transform(*ex)?)))
    } else {
        Err(vec![
            Diagnostic::decl_error(
                Loc::Builtin,
                format!("Can't understanding expression: {:?}", expression).to_string(),
            )
            .into(),
        ])
    }
}
