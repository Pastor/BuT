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
