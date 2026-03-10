use std::collections::HashMap;

use but_grammar::ast::{Condition, Expression, Statement};

/// Преобразовать узел AST `Condition` в строку C-выражения.
pub fn condition_to_c(cond: &Condition) -> String {
    match cond {
        Condition::Variable(id) => id.name.clone(),
        Condition::BoolLiteral(_, b) => if *b { "1".to_string() } else { "0".to_string() },
        Condition::NumberLiteral(_, n) => n.to_string(),
        Condition::RationalNumberLiteral(_, s, neg) => {
            if *neg { format!("-{}", s) } else { s.clone() }
        }
        Condition::HexNumberLiteral(_, s, _) => s.clone(),
        Condition::Equal(_, l, r) => format!("({} == {})", condition_to_c(l), condition_to_c(r)),
        Condition::NotEqual(_, l, r) => format!("({} != {})", condition_to_c(l), condition_to_c(r)),
        Condition::Less(_, l, r) => format!("({} < {})", condition_to_c(l), condition_to_c(r)),
        Condition::LessEqual(_, l, r) => format!("({} <= {})", condition_to_c(l), condition_to_c(r)),
        Condition::More(_, l, r) => format!("({} > {})", condition_to_c(l), condition_to_c(r)),
        Condition::MoreEqual(_, l, r) => format!("({} >= {})", condition_to_c(l), condition_to_c(r)),
        Condition::And(_, l, r) => format!("({} && {})", condition_to_c(l), condition_to_c(r)),
        Condition::Or(_, l, r) => format!("({} || {})", condition_to_c(l), condition_to_c(r)),
        Condition::Not(_, inner) => format!("(!{})", condition_to_c(inner)),
        Condition::Add(_, l, r) => format!("({} + {})", condition_to_c(l), condition_to_c(r)),
        Condition::Subtract(_, l, r) => format!("({} - {})", condition_to_c(l), condition_to_c(r)),
        Condition::Parenthesis(_, inner) => format!("({})", condition_to_c(inner)),
        Condition::FunctionCall(_, id, args) => {
            let args_str = args.iter().map(condition_to_c).collect::<Vec<_>>().join(", ");
            format!("{}({})", id.name, args_str)
        }
        Condition::MemberAccess(_, base, _) => condition_to_c(base),
        Condition::ArraySubscript(_, id, idx) => format!("{}[{}]", id.name, idx),
        Condition::StringLiteral(lits) => {
            format!("\"{}\"", lits.iter().map(|l| l.string.as_str()).collect::<String>())
        }
        Condition::HexLiteral(lits) => lits.first().map(|l| l.hex.clone()).unwrap_or_default(),
    }
}

/// Преобразовать `Condition` в строку Verilog-выражения.
pub fn condition_to_verilog(cond: &Condition) -> String {
    match cond {
        Condition::Variable(id) => id.name.clone(),
        Condition::BoolLiteral(_, b) => if *b { "1'b1".to_string() } else { "1'b0".to_string() },
        Condition::NumberLiteral(_, n) => n.to_string(),
        Condition::HexNumberLiteral(_, s, _) => s.replace("0x", "32'h"),
        Condition::Equal(_, l, r) => {
            format!("({} == {})", condition_to_verilog(l), condition_to_verilog(r))
        }
        Condition::NotEqual(_, l, r) => {
            format!("({} != {})", condition_to_verilog(l), condition_to_verilog(r))
        }
        Condition::Less(_, l, r) => {
            format!("({} < {})", condition_to_verilog(l), condition_to_verilog(r))
        }
        Condition::LessEqual(_, l, r) => {
            format!("({} <= {})", condition_to_verilog(l), condition_to_verilog(r))
        }
        Condition::More(_, l, r) => {
            format!("({} > {})", condition_to_verilog(l), condition_to_verilog(r))
        }
        Condition::MoreEqual(_, l, r) => {
            format!("({} >= {})", condition_to_verilog(l), condition_to_verilog(r))
        }
        Condition::And(_, l, r) => {
            format!("({} && {})", condition_to_verilog(l), condition_to_verilog(r))
        }
        Condition::Or(_, l, r) => {
            format!("({} || {})", condition_to_verilog(l), condition_to_verilog(r))
        }
        Condition::Not(_, inner) => format!("(!{})", condition_to_verilog(inner)),
        Condition::Add(_, l, r) => {
            format!("({} + {})", condition_to_verilog(l), condition_to_verilog(r))
        }
        Condition::Subtract(_, l, r) => {
            format!("({} - {})", condition_to_verilog(l), condition_to_verilog(r))
        }
        Condition::Parenthesis(_, inner) => format!("({})", condition_to_verilog(inner)),
        other => condition_to_c(other),
    }
}

/// Преобразовать `Expression` в строку C-выражения.
pub fn expr_to_c(expr: &Expression) -> String {
    match expr {
        Expression::NumberLiteral(_, n) => n.to_string(),
        Expression::HexNumberLiteral(_, s, _) => s.clone(),
        Expression::RationalNumberLiteral(_, s, neg) => {
            if *neg { format!("-{}", s) } else { s.clone() }
        }
        Expression::BoolLiteral(_, b) => if *b { "1".to_string() } else { "0".to_string() },
        Expression::StringLiteral(lits) => {
            format!("\"{}\"", lits.iter().map(|l| l.string.as_str()).collect::<String>())
        }
        Expression::Variable(id) => id.name.clone(),
        Expression::Parenthesis(_, inner) => format!("({})", expr_to_c(inner)),
        Expression::Assign(_, l, r) => format!("{} = {}", expr_to_c(l), expr_to_c(r)),
        Expression::AssignAdd(_, l, r) => format!("{} += {}", expr_to_c(l), expr_to_c(r)),
        Expression::AssignSubtract(_, l, r) => format!("{} -= {}", expr_to_c(l), expr_to_c(r)),
        Expression::AssignMultiply(_, l, r) => format!("{} *= {}", expr_to_c(l), expr_to_c(r)),
        Expression::AssignDivide(_, l, r) => format!("{} /= {}", expr_to_c(l), expr_to_c(r)),
        Expression::AssignModulo(_, l, r) => format!("{} %= {}", expr_to_c(l), expr_to_c(r)),
        Expression::Add(_, l, r) => format!("({} + {})", expr_to_c(l), expr_to_c(r)),
        Expression::Subtract(_, l, r) => format!("({} - {})", expr_to_c(l), expr_to_c(r)),
        Expression::Multiply(_, l, r) => format!("({} * {})", expr_to_c(l), expr_to_c(r)),
        Expression::Divide(_, l, r) => format!("({} / {})", expr_to_c(l), expr_to_c(r)),
        Expression::Modulo(_, l, r) => format!("({} % {})", expr_to_c(l), expr_to_c(r)),
        Expression::Equal(_, l, r) => format!("({} == {})", expr_to_c(l), expr_to_c(r)),
        Expression::NotEqual(_, l, r) => format!("({} != {})", expr_to_c(l), expr_to_c(r)),
        Expression::Less(_, l, r) => format!("({} < {})", expr_to_c(l), expr_to_c(r)),
        Expression::LessEqual(_, l, r) => format!("({} <= {})", expr_to_c(l), expr_to_c(r)),
        Expression::More(_, l, r) => format!("({} > {})", expr_to_c(l), expr_to_c(r)),
        Expression::MoreEqual(_, l, r) => format!("({} >= {})", expr_to_c(l), expr_to_c(r)),
        Expression::And(_, l, r) => format!("({} && {})", expr_to_c(l), expr_to_c(r)),
        Expression::Or(_, l, r) => format!("({} || {})", expr_to_c(l), expr_to_c(r)),
        Expression::Not(_, inner) => format!("(!{})", expr_to_c(inner)),
        Expression::Negate(_, inner) => format!("(-{})", expr_to_c(inner)),
        Expression::BitwiseAnd(_, l, r) => format!("({} & {})", expr_to_c(l), expr_to_c(r)),
        Expression::BitwiseOr(_, l, r) => format!("({} | {})", expr_to_c(l), expr_to_c(r)),
        Expression::BitwiseXor(_, l, r) => format!("({} ^ {})", expr_to_c(l), expr_to_c(r)),
        Expression::ShiftLeft(_, l, r) => format!("({} << {})", expr_to_c(l), expr_to_c(r)),
        Expression::ShiftRight(_, l, r) => format!("({} >> {})", expr_to_c(l), expr_to_c(r)),
        Expression::FunctionCall(_, id, args) => {
            let args_str = args.iter().map(expr_to_c).collect::<Vec<_>>().join(", ");
            format!("{}({})", id.name, args_str)
        }
        Expression::PostIncrement(_, e) => format!("{}++", expr_to_c(e)),
        Expression::PostDecrement(_, e) => format!("{}--", expr_to_c(e)),
        Expression::PreIncrement(_, e) => format!("++{}", expr_to_c(e)),
        Expression::PreDecrement(_, e) => format!("--{}", expr_to_c(e)),
        Expression::ConditionalOperator(_, c, t, e) => {
            format!("({} ? {} : {})", expr_to_c(c), expr_to_c(t), expr_to_c(e))
        }
        _ => "/* не поддерживается */".to_string(),
    }
}

/// Преобразовать Statement в C-код (отступ `indent` пробелов).
pub fn stmt_to_c(stmt: &Statement, indent: usize) -> String {
    let pad = " ".repeat(indent);
    match stmt {
        Statement::Block { statements, .. } => {
            let inner = statements
                .iter()
                .map(|s| stmt_to_c(s, indent + 4))
                .collect::<String>();
            format!("{}{{\n{}{}}}\n", pad, inner, pad)
        }
        Statement::Expression(_, expr) => format!("{}{};\n", pad, expr_to_c(expr)),
        Statement::If(_, cond, then_s, else_s) => {
            let mut out = format!("{}if ({}) ", pad, expr_to_c(cond));
            out.push_str(&stmt_to_c(then_s, indent));
            if let Some(else_body) = else_s {
                out.push_str(&format!("{}else ", pad));
                out.push_str(&stmt_to_c(else_body, indent));
            }
            out
        }
        Statement::While(_, cond, body) => {
            format!("{}while ({}) {}", pad, expr_to_c(cond), stmt_to_c(body, indent))
        }
        Statement::For(_, init, cond, post, body) => {
            let init_str = init.as_ref().map(|s| stmt_to_c(s, 0).trim_end_matches('\n').trim_end_matches(';').to_string()).unwrap_or_default();
            let cond_str = cond.as_ref().map(|e| expr_to_c(e)).unwrap_or_default();
            let post_str = post.as_ref().map(|e| expr_to_c(e)).unwrap_or_default();
            let body_str = body.as_ref().map(|b| stmt_to_c(b, indent)).unwrap_or_default();
            format!("{}for ({}; {}; {}) {}", pad, init_str, cond_str, post_str, body_str)
        }
        Statement::Return(_, Some(expr)) => format!("{}return {};\n", pad, expr_to_c(expr)),
        Statement::Return(_, None) => format!("{}return;\n", pad),
        Statement::Continue(_) => format!("{}continue;\n", pad),
        Statement::Break(_) => format!("{}break;\n", pad),
        Statement::VariableDefinition(_, decl, init) => {
            let type_str = type_to_c(&decl.ty);
            let name = decl.name.as_ref().map(|n| n.name.as_str()).unwrap_or("_");
            if let Some(e) = init {
                format!("{}{} {} = {};\n", pad, type_str, name, expr_to_c(e))
            } else {
                format!("{}{} {};\n", pad, type_str, name)
            }
        }
        _ => format!("{}/* неподдерживаемый оператор */\n", pad),
    }
}

/// Преобразовать тип BuT в строку типа C.
pub fn type_to_c(ty: &but_grammar::ast::Type) -> String {
    use but_grammar::ast::Type;
    match ty {
        Type::Bool => "int".to_string(),
        Type::String => "char*".to_string(),
        Type::Rational => "double".to_string(),
        Type::Address => "unsigned long".to_string(),
        Type::Alias(id) => {
            match id.name.as_str() {
                "int" | "i32" => "int32_t".to_string(),
                "u8" | "byte" => "uint8_t".to_string(),
                "u16" => "uint16_t".to_string(),
                "u32" => "uint32_t".to_string(),
                "u64" => "uint64_t".to_string(),
                "i64" => "int64_t".to_string(),
                "real" | "f64" | "f32" => "double".to_string(),
                "bool" => "int".to_string(),
                "str" | "string" => "char*".to_string(),
                "bit" => "uint8_t".to_string(),
                _ => id.name.clone(),
            }
        }
        Type::Array { element_count, element_type, .. } => {
            format!("{}[{}]", type_to_c(element_type), element_count)
        }
        _ => "int".to_string(),
    }
}

/// Разрешить псевдоним типа через таблицу псевдонимов (до 8 уровней вложенности).
///
/// Если `ty` — `Type::Alias(name)` и имя найдено в таблице, возвращается разрешённый тип.
/// Повторяется рекурсивно (например, `type A = B; type B = u8;` → `u8`).
/// При превышении глубины или отсутствии записи возвращается оригинальный тип.
pub fn resolve_alias(
    ty: &but_grammar::ast::Type,
    aliases: &HashMap<String, but_grammar::ast::Type>,
) -> but_grammar::ast::Type {
    resolve_alias_depth(ty, aliases, 8)
}

fn resolve_alias_depth(
    ty: &but_grammar::ast::Type,
    aliases: &HashMap<String, but_grammar::ast::Type>,
    depth: u8,
) -> but_grammar::ast::Type {
    use but_grammar::ast::Type;
    if depth == 0 {
        return ty.clone();
    }
    match ty {
        Type::Alias(id) => match aliases.get(&id.name) {
            Some(resolved) => resolve_alias_depth(resolved, aliases, depth - 1),
            None => ty.clone(),
        },
        _ => ty.clone(),
    }
}

/// Преобразовать тип BuT в строку типа C с разрешением псевдонимов из таблицы.
pub fn type_to_c_ctx(
    ty: &but_grammar::ast::Type,
    aliases: &HashMap<String, but_grammar::ast::Type>,
) -> String {
    type_to_c(&resolve_alias(ty, aliases))
}

/// Преобразовать тип BuT в строку типа Structured Text.
pub fn type_to_st(ty: &but_grammar::ast::Type) -> String {
    use but_grammar::ast::Type;
    match ty {
        Type::Bool => "BOOL".to_string(),
        Type::String => "STRING".to_string(),
        Type::Rational => "REAL".to_string(),
        Type::Alias(id) => match id.name.as_str() {
            "int" | "i32" | "i64" => "INT".to_string(),
            "u8" | "byte" => "BYTE".to_string(),
            "u16" => "WORD".to_string(),
            "u32" => "DWORD".to_string(),
            "u64" => "LWORD".to_string(),
            "real" | "f64" | "f32" => "REAL".to_string(),
            "bool" => "BOOL".to_string(),
            "str" | "string" => "STRING".to_string(),
            "bit" => "BYTE".to_string(),
            _ => id.name.clone(),
        },
        _ => "INT".to_string(),
    }
}

/// Преобразовать тип BuT в строку типа ST с разрешением псевдонимов из таблицы.
pub fn type_to_st_ctx(
    ty: &but_grammar::ast::Type,
    aliases: &HashMap<String, but_grammar::ast::Type>,
) -> String {
    type_to_st(&resolve_alias(ty, aliases))
}

#[cfg(test)]
mod tests {
    use super::*;
    use but_grammar::ast::{Condition, Expression, Identifier, Loc, Type};

    // Вспомогательная функция: создать заглушку позиции
    fn loc() -> Loc {
        Loc::Source(0, 0, 0)
    }

    // Вспомогательная функция: создать идентификатор
    fn ident(name: &str) -> Identifier {
        Identifier::new(name)
    }

    // Вспомогательная функция: создать псевдоним типа
    fn alias(name: &str) -> Type {
        Type::Alias(ident(name))
    }

    // ===== condition_to_c =====

    #[test]
    fn condition_variable_в_c() {
        let cond = Condition::Variable(ident("flag"));
        assert_eq!(condition_to_c(&cond), "flag");
    }

    #[test]
    fn condition_bool_true_в_c() {
        let cond = Condition::BoolLiteral(loc(), true);
        assert_eq!(condition_to_c(&cond), "1");
    }

    #[test]
    fn condition_bool_false_в_c() {
        let cond = Condition::BoolLiteral(loc(), false);
        assert_eq!(condition_to_c(&cond), "0");
    }

    #[test]
    fn condition_number_literal_в_c() {
        let cond = Condition::NumberLiteral(loc(), 42);
        assert_eq!(condition_to_c(&cond), "42");
    }

    #[test]
    fn condition_equal_в_c() {
        let l = Box::new(Condition::Variable(ident("x")));
        let r = Box::new(Condition::NumberLiteral(loc(), 0));
        let cond = Condition::Equal(loc(), l, r);
        assert_eq!(condition_to_c(&cond), "(x == 0)");
    }

    #[test]
    fn condition_not_equal_в_c() {
        let l = Box::new(Condition::Variable(ident("a")));
        let r = Box::new(Condition::NumberLiteral(loc(), 1));
        let cond = Condition::NotEqual(loc(), l, r);
        assert_eq!(condition_to_c(&cond), "(a != 1)");
    }

    #[test]
    fn condition_and_в_c() {
        let l = Box::new(Condition::Variable(ident("a")));
        let r = Box::new(Condition::Variable(ident("b")));
        let cond = Condition::And(loc(), l, r);
        assert_eq!(condition_to_c(&cond), "(a && b)");
    }

    #[test]
    fn condition_or_в_c() {
        let l = Box::new(Condition::Variable(ident("p")));
        let r = Box::new(Condition::Variable(ident("q")));
        let cond = Condition::Or(loc(), l, r);
        assert_eq!(condition_to_c(&cond), "(p || q)");
    }

    #[test]
    fn condition_not_в_c() {
        let inner = Box::new(Condition::Variable(ident("done")));
        let cond = Condition::Not(loc(), inner);
        assert_eq!(condition_to_c(&cond), "(!done)");
    }

    #[test]
    fn condition_less_в_c() {
        let l = Box::new(Condition::Variable(ident("i")));
        let r = Box::new(Condition::NumberLiteral(loc(), 10));
        let cond = Condition::Less(loc(), l, r);
        assert_eq!(condition_to_c(&cond), "(i < 10)");
    }

    #[test]
    fn condition_more_в_c() {
        let l = Box::new(Condition::Variable(ident("i")));
        let r = Box::new(Condition::NumberLiteral(loc(), 5));
        let cond = Condition::More(loc(), l, r);
        assert_eq!(condition_to_c(&cond), "(i > 5)");
    }

    #[test]
    fn condition_add_в_c() {
        let l = Box::new(Condition::Variable(ident("a")));
        let r = Box::new(Condition::NumberLiteral(loc(), 3));
        let cond = Condition::Add(loc(), l, r);
        assert_eq!(condition_to_c(&cond), "(a + 3)");
    }

    #[test]
    fn condition_subtract_в_c() {
        let l = Box::new(Condition::Variable(ident("a")));
        let r = Box::new(Condition::NumberLiteral(loc(), 1));
        let cond = Condition::Subtract(loc(), l, r);
        assert_eq!(condition_to_c(&cond), "(a - 1)");
    }

    #[test]
    fn condition_parenthesis_в_c() {
        let inner = Box::new(Condition::Variable(ident("x")));
        let cond = Condition::Parenthesis(loc(), inner);
        assert_eq!(condition_to_c(&cond), "(x)");
    }

    // ===== expr_to_c =====

    #[test]
    fn expr_number_literal_в_c() {
        let expr = Expression::NumberLiteral(loc(), 100);
        assert_eq!(expr_to_c(&expr), "100");
    }

    #[test]
    fn expr_bool_true_в_c() {
        let expr = Expression::BoolLiteral(loc(), true);
        assert_eq!(expr_to_c(&expr), "1");
    }

    #[test]
    fn expr_bool_false_в_c() {
        let expr = Expression::BoolLiteral(loc(), false);
        assert_eq!(expr_to_c(&expr), "0");
    }

    #[test]
    fn expr_variable_в_c() {
        let expr = Expression::Variable(ident("result"));
        assert_eq!(expr_to_c(&expr), "result");
    }

    #[test]
    fn expr_add_в_c() {
        let l = Box::new(Expression::Variable(ident("x")));
        let r = Box::new(Expression::NumberLiteral(loc(), 5));
        let expr = Expression::Add(loc(), l, r);
        assert_eq!(expr_to_c(&expr), "(x + 5)");
    }

    #[test]
    fn expr_subtract_в_c() {
        let l = Box::new(Expression::Variable(ident("y")));
        let r = Box::new(Expression::NumberLiteral(loc(), 2));
        let expr = Expression::Subtract(loc(), l, r);
        assert_eq!(expr_to_c(&expr), "(y - 2)");
    }

    #[test]
    fn expr_assign_в_c() {
        let l = Box::new(Expression::Variable(ident("output")));
        let r = Box::new(Expression::NumberLiteral(loc(), 1));
        let expr = Expression::Assign(loc(), l, r);
        assert_eq!(expr_to_c(&expr), "output = 1");
    }

    #[test]
    fn expr_assign_add_в_c() {
        let l = Box::new(Expression::Variable(ident("x")));
        let r = Box::new(Expression::NumberLiteral(loc(), 1));
        let expr = Expression::AssignAdd(loc(), l, r);
        assert_eq!(expr_to_c(&expr), "x += 1");
    }

    #[test]
    fn expr_assign_subtract_в_c() {
        let l = Box::new(Expression::Variable(ident("x")));
        let r = Box::new(Expression::NumberLiteral(loc(), 1));
        let expr = Expression::AssignSubtract(loc(), l, r);
        assert_eq!(expr_to_c(&expr), "x -= 1");
    }

    #[test]
    fn expr_multiply_в_c() {
        let l = Box::new(Expression::Variable(ident("a")));
        let r = Box::new(Expression::NumberLiteral(loc(), 3));
        let expr = Expression::Multiply(loc(), l, r);
        assert_eq!(expr_to_c(&expr), "(a * 3)");
    }

    #[test]
    fn expr_not_в_c() {
        let inner = Box::new(Expression::Variable(ident("flag")));
        let expr = Expression::Not(loc(), inner);
        assert_eq!(expr_to_c(&expr), "(!flag)");
    }

    #[test]
    fn expr_negate_в_c() {
        let inner = Box::new(Expression::Variable(ident("val")));
        let expr = Expression::Negate(loc(), inner);
        assert_eq!(expr_to_c(&expr), "(-val)");
    }

    #[test]
    fn expr_parenthesis_в_c() {
        let inner = Box::new(Expression::NumberLiteral(loc(), 99));
        let expr = Expression::Parenthesis(loc(), inner);
        assert_eq!(expr_to_c(&expr), "(99)");
    }

    // ===== type_to_c =====

    #[test]
    fn type_u8_в_c() {
        assert_eq!(type_to_c(&alias("u8")), "uint8_t");
    }

    #[test]
    fn type_u16_в_c() {
        assert_eq!(type_to_c(&alias("u16")), "uint16_t");
    }

    #[test]
    fn type_u32_в_c() {
        assert_eq!(type_to_c(&alias("u32")), "uint32_t");
    }

    #[test]
    fn type_bit_в_c() {
        assert_eq!(type_to_c(&alias("bit")), "uint8_t");
    }

    #[test]
    fn type_bool_alias_в_c() {
        assert_eq!(type_to_c(&alias("bool")), "int");
    }

    #[test]
    fn type_bool_в_c() {
        assert_eq!(type_to_c(&Type::Bool), "int");
    }

    #[test]
    fn type_string_в_c() {
        assert_eq!(type_to_c(&Type::String), "char*");
    }

    #[test]
    fn type_rational_в_c() {
        assert_eq!(type_to_c(&Type::Rational), "double");
    }

    #[test]
    fn type_неизвестный_alias_в_c() {
        assert_eq!(type_to_c(&alias("MyType")), "MyType");
    }

    // ===== type_to_st =====

    #[test]
    fn type_bool_в_st() {
        assert_eq!(type_to_st(&Type::Bool), "BOOL");
    }

    #[test]
    fn type_string_в_st() {
        assert_eq!(type_to_st(&Type::String), "STRING");
    }

    #[test]
    fn type_rational_в_st() {
        assert_eq!(type_to_st(&Type::Rational), "REAL");
    }

    #[test]
    fn type_u8_в_st() {
        assert_eq!(type_to_st(&alias("u8")), "BYTE");
    }

    #[test]
    fn type_u16_в_st() {
        assert_eq!(type_to_st(&alias("u16")), "WORD");
    }

    #[test]
    fn type_u32_в_st() {
        assert_eq!(type_to_st(&alias("u32")), "DWORD");
    }

    #[test]
    fn type_u64_в_st() {
        assert_eq!(type_to_st(&alias("u64")), "LWORD");
    }

    #[test]
    fn type_bit_в_st() {
        assert_eq!(type_to_st(&alias("bit")), "BYTE");
    }

    #[test]
    fn type_bool_alias_в_st() {
        assert_eq!(type_to_st(&alias("bool")), "BOOL");
    }

    #[test]
    fn type_int_в_st() {
        assert_eq!(type_to_st(&alias("int")), "INT");
    }

    #[test]
    fn type_неизвестный_alias_в_st() {
        assert_eq!(type_to_st(&alias("CustomType")), "CustomType");
    }

    #[test]
    fn type_address_в_st() {
        // Address не является Alias/Bool/String/Rational — возвращает "INT"
        assert_eq!(type_to_st(&Type::Address), "INT");
    }

    // ===== resolve_alias =====

    fn make_aliases(pairs: &[(&str, Type)]) -> HashMap<String, Type> {
        pairs.iter().map(|(k, v)| (k.to_string(), v.clone())).collect()
    }

    #[test]
    fn resolve_alias_известный_псевдоним_раскрывается() {
        let aliases = make_aliases(&[("MyByte", alias("u8"))]);
        let resolved = resolve_alias(&alias("MyByte"), &aliases);
        assert_eq!(resolved, alias("u8"));
    }

    #[test]
    fn resolve_alias_неизвестный_псевдоним_остаётся_как_есть() {
        let aliases = make_aliases(&[]);
        let resolved = resolve_alias(&alias("Unknown"), &aliases);
        assert_eq!(resolved, alias("Unknown"));
    }

    #[test]
    fn resolve_alias_цепочка_псевдонимов_раскрывается() {
        // Counter → MyInt → i32
        let aliases = make_aliases(&[
            ("Counter", alias("MyInt")),
            ("MyInt", alias("i32")),
        ]);
        let resolved = resolve_alias(&alias("Counter"), &aliases);
        assert_eq!(resolved, alias("i32"));
    }

    #[test]
    fn resolve_alias_примитивный_тип_не_изменяется() {
        let aliases = make_aliases(&[]);
        assert_eq!(resolve_alias(&Type::Bool, &aliases), Type::Bool);
        assert_eq!(resolve_alias(&Type::Rational, &aliases), Type::Rational);
        assert_eq!(resolve_alias(&Type::String, &aliases), Type::String);
    }

    #[test]
    fn resolve_alias_псевдоним_на_массив() {
        let arr = Type::Array {
            loc: but_grammar::ast::Loc::Source(0, 0, 0),
            element_count: 8,
            element_type: Box::new(alias("bit")),
        };
        let aliases = make_aliases(&[("Byte", arr.clone())]);
        let resolved = resolve_alias(&alias("Byte"), &aliases);
        assert_eq!(resolved, arr);
    }

    // ===== type_to_c_ctx =====

    #[test]
    fn type_to_c_ctx_разрешает_пользовательский_псевдоним_в_c() {
        // type MyByte = u8; → uint8_t
        let aliases = make_aliases(&[("MyByte", alias("u8"))]);
        assert_eq!(type_to_c_ctx(&alias("MyByte"), &aliases), "uint8_t");
    }

    #[test]
    fn type_to_c_ctx_цепочка_псевдонимов_в_c() {
        // type Counter = MyU32; type MyU32 = u32; → uint32_t
        let aliases = make_aliases(&[
            ("Counter", alias("MyU32")),
            ("MyU32", alias("u32")),
        ]);
        assert_eq!(type_to_c_ctx(&alias("Counter"), &aliases), "uint32_t");
    }

    #[test]
    fn type_to_c_ctx_без_псевдонима_работает_как_type_to_c() {
        let aliases = make_aliases(&[]);
        assert_eq!(type_to_c_ctx(&alias("u16"), &aliases), "uint16_t");
        assert_eq!(type_to_c_ctx(&Type::Bool, &aliases), "int");
    }

    #[test]
    fn type_to_c_ctx_неизвестный_псевдоним_возвращает_имя() {
        let aliases = make_aliases(&[]);
        assert_eq!(type_to_c_ctx(&alias("CustomType"), &aliases), "CustomType");
    }

    #[test]
    fn type_to_c_ctx_псевдоним_bool_в_c() {
        let aliases = make_aliases(&[("Flag", Type::Bool)]);
        assert_eq!(type_to_c_ctx(&alias("Flag"), &aliases), "int");
    }

    // ===== type_to_st_ctx =====

    #[test]
    fn type_to_st_ctx_разрешает_пользовательский_псевдоним_в_st() {
        // type Counter = u32; → DWORD
        let aliases = make_aliases(&[("Counter", alias("u32"))]);
        assert_eq!(type_to_st_ctx(&alias("Counter"), &aliases), "DWORD");
    }

    #[test]
    fn type_to_st_ctx_цепочка_псевдонимов_в_st() {
        // type MyWord = MyU16; type MyU16 = u16; → WORD
        let aliases = make_aliases(&[
            ("MyWord", alias("MyU16")),
            ("MyU16", alias("u16")),
        ]);
        assert_eq!(type_to_st_ctx(&alias("MyWord"), &aliases), "WORD");
    }

    #[test]
    fn type_to_st_ctx_без_псевдонима_работает_как_type_to_st() {
        let aliases = make_aliases(&[]);
        assert_eq!(type_to_st_ctx(&alias("u8"), &aliases), "BYTE");
        assert_eq!(type_to_st_ctx(&Type::Bool, &aliases), "BOOL");
    }

    #[test]
    fn type_to_st_ctx_псевдоним_rational_в_st() {
        let aliases = make_aliases(&[("Speed", Type::Rational)]);
        assert_eq!(type_to_st_ctx(&alias("Speed"), &aliases), "REAL");
    }
}
