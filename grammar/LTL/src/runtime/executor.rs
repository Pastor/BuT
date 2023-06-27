use crate::ast::block::Block;
use crate::ast::expression::{BinaryOpcode, Expr, UnaryOpcode};
use crate::ast::program::Program;
use crate::ast::statement::Statement;
use crate::ast::value::Value;
use crate::runtime::error::RuntimeError;
use crate::runtime::context::{Context, VariableError};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ExpressionError {
    #[error("Unable to evalutate expression {0}: {1}")]
    VariableError(String, VariableError),
    #[error("Unable to evalutate expression {0}: {1}")]
    OperationError(String, OperationError),
}

pub fn evalutate_expression(ctx: &mut Context, expr: &Expr) -> Result<Value, ExpressionError> {
    match expr {
        Expr::Constant(n) => Ok(n.clone()),
        Expr::BinaryOp(exp1, opcode, exp2) => {
            let value_1 = evalutate_expression(ctx, exp1)?;
            let value_2 = evalutate_expression(ctx, exp2)?;
            let result = match opcode {
                BinaryOpcode::Mul => value_1 * value_2,
                BinaryOpcode::Div => value_1 / value_2,
                BinaryOpcode::Add => value_1 + value_2,
                BinaryOpcode::Sub => value_1 - value_2,
                BinaryOpcode::Conj => conjunction(value_1, value_2),
                BinaryOpcode::Disj => disjunction(value_1, value_2),
                BinaryOpcode::Equals => equals(value_1, value_2),
                BinaryOpcode::NotEquals => not_equals(value_1, value_2),
                BinaryOpcode::Greater => greater(value_1, value_2),
                BinaryOpcode::GreaterEquals => greater_equals(value_1, value_2),
                BinaryOpcode::Lower => lower(value_1, value_2),
                BinaryOpcode::LowerEquals => lower_equals(value_1, value_2),
            };
            result.map_err(|e| ExpressionError::OperationError(expr.to_string(), e))
        }
        Expr::Identifier(variable) => ctx
            .variable_value(variable)
            .map_err(|e| ExpressionError::VariableError(expr.to_string(), e)),
        Expr::UnaryOp(op, exp) => {
            let result = match op {
                UnaryOpcode::Not => !evalutate_expression(ctx, exp)?,
            };
            result.map_err(|e| ExpressionError::OperationError(expr.to_string(), e))
        }
    }
}

pub fn execute_condition(
    mut ctx: Context,
    expr: &Expr,
    then_block: &[Statement],
    else_block: &Option<Vec<Statement>>,
) -> Result<Frame, RuntimeError> {
    let value = evalutate_expression(&mut ctx, expr)?;
    if let Value::Bool(value) = value {
        let mut block_frame = Frame::new(Box::new(ctx));
        if value {
            block_frame = execute_statements(block_frame, then_block)?;
        } else if let Some(else_block) = else_block {
            block_frame = execute_statements(block_frame, else_block)?;
        }
        Ok(*block_frame.take_parent().unwrap())
    } else {
        Err(RuntimeError::NonBooleanCondition(
            expr.to_string(),
            (&value).into(),
        ))
    }
}

pub fn execute_block(ctx: Context, block: &Block) -> Result<Frame, RuntimeError> {
    match block {
        Block::StatementsBlock(statements) => {
            let mut block_frame = Frame::new(Box::new(ctx));
            block_frame = execute_statements(block_frame, statements)?;
            Ok(*block_frame.take_parent().unwrap())
        }
        Block::Condition {
            expression,
            then_block,
            else_block,
        } => execute_condition(ctx, expression.as_ref(), then_block, else_block),
    }
}

pub fn execute_statement(mut ctx: Context, statement: &Statement) -> Result<Frame, RuntimeError> {
    match statement {
        Statement::Assignment {
            identifier,
            expression,
        } => {
            let value = evalutate_expression(&mut ctx, expression)?;
            ctx.assign_value(identifier, value)?;
            Ok(ctx)
        }
        Statement::Definition {
            identifier,
            expression,
            value_type,
        } => {
            let value = evalutate_expression(&mut ctx, expression)?;
            ctx.define_variable(identifier.clone(), value_type.clone(), value)?;
            Ok(ctx)
        }
        Statement::Block(block) => {
            ctx = execute_block(ctx, block)?;
            Ok(ctx)
        }
    }
}

pub fn execute_statements(
    mut ctx: Context,
    statements: &[Statement],
) -> Result<Frame, RuntimeError> {
    for statement in statements {
        ctx = execute_statement(ctx, statement)?;
    }
    Ok(ctx)
}

pub fn execute_program(frame: Frame, program: &Program) -> Result<Frame, RuntimeError> {
    execute_statements(frame, &program.statements)
}

#[cfg(test)]
mod test {
    use crate::ast::grammar;
    use crate::ast::value::Value;
    use crate::runtime::executor::evalutate_expression;
    use crate::Context;
    use rstest::*;

    #[rstest]
    #[case("1 + 2 * 3 - 4", Value::Int(3))]
    #[case("!0", Value::Int(-1))]
    #[case("!-1", Value::Int(0))]
    #[case("(1 + 2) * (3 - 4)", Value::Int(-3))]
    #[case("true || false", Value::Bool(true))]
    #[case("true && false", Value::Bool(false))]
    #[case("!false", Value::Bool(true))]
    #[case("!true", Value::Bool(false))]
    #[case("2 < 3", Value::Bool(true))]
    #[case("2 <= 3", Value::Bool(true))]
    #[case("2 <= 3", Value::Bool(true))]
    #[case("2 >= 2", Value::Bool(true))]
    #[case("2 >= 1", Value::Bool(true))]
    #[case("2 == 2", Value::Bool(true))]
    #[case("2 != 2", Value::Bool(false))]
    #[case("2 != 3", Value::Bool(true))]
    #[case("\"abc\" == \"abc\"", Value::Bool(true))]
    #[case("\"abc\" < \"xyz\"", Value::Bool(true))]
    #[case("\"abc\" <= \"xyz\"", Value::Bool(true))]
    #[case("\"abc\" >= \"xyz\"", Value::Bool(false))]
    #[case("true && false || true || true && false", Value::Bool(true))]
    #[case("true && (false || true || true) && false", Value::Bool(false))]
    #[case("\"abc \" + 5.5", Value::String("abc 5.5".to_owned()))]
    #[case("2 == 2 && 3 == 3", Value::Bool(true))]
    #[case("100 * 2 == 200 && 120 > 120 - 1", Value::Bool(true))]
    #[case("100 * 2 < 200 || 120 <= 120 - 1", Value::Bool(false))]
    #[case("!(100 * 2 < 200) && !(120 <= 120 - 1)", Value::Bool(true))]
    fn test_evalutate_expression(#[case] expression: &str, #[case] expected: Value) {
        let parsed = grammar::FormulaParser::new()
            .parse(expression)
            .expect("Unable to parse expression");
        let mut root_ctx = Context::default();
        let value = evalutate_expression(&mut root_ctx, parsed.as_ref()).unwrap();
        assert_eq!(expected, value)
    }
}
