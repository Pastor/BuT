use std::collections::HashMap;

use but_grammar::ast::{Condition, Expression, Statement};

/// Convert an AST `Condition` node to a C expression string.
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
        Condition::MemberAccess(_, base, member) => {
            match member {
                but_grammar::ast::Member::Number(n) => {
                    // Bit N access: ((variable) >> N) & 1
                    format!("(({}) >> {}) & 1", condition_to_c(base), n)
                }
                but_grammar::ast::Member::Identifier(id) => {
                    // Struct field access
                    format!("({}).{}", condition_to_c(base), id.name)
                }
            }
        }
        Condition::ArraySubscript(_, id, idx) => format!("{}[{}]", id.name, idx),
        Condition::StringLiteral(lits) => {
            format!("\"{}\"", lits.iter().map(|l| l.string.as_str()).collect::<String>())
        }
        Condition::HexLiteral(lits) => lits.first().map(|l| l.hex.clone()).unwrap_or_default(),
    }
}

/// Convert a `Condition` to a Verilog expression string.
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
        Condition::MemberAccess(_, base, member) => {
            match member {
                but_grammar::ast::Member::Number(n) => {
                    // In Verilog bit access: base[N]
                    format!("{}[{}]", condition_to_verilog(base), n)
                }
                but_grammar::ast::Member::Identifier(id) => {
                    format!("{}.{}", condition_to_verilog(base), id.name)
                }
            }
        }
        other => condition_to_c(other),
    }
}

/// Convert an `Expression` to a C expression string.
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
        Expression::Assign(_, l, r) => {
            // Write to a specific bit: a.N = expr → a = (a & ~(1UL << N)) | ((expr & 1) << N)
            if let Expression::MemberAccess(_, base, but_grammar::ast::Member::Number(n)) = l.as_ref() {
                let base_str = expr_to_c(base);
                let rhs_str = expr_to_c(r);
                format!(
                    "{0} = (({0}) & ~(1UL << {1})) | (({2} & 1) << {1})",
                    base_str, n, rhs_str
                )
            } else {
                format!("{} = {}", expr_to_c(l), expr_to_c(r))
            }
        }
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
        Expression::ArraySubscript(_, id, idx) => format!("{}[{}]", id.name, idx),
        Expression::PostIncrement(_, e) => format!("{}++", expr_to_c(e)),
        Expression::PostDecrement(_, e) => format!("{}--", expr_to_c(e)),
        Expression::PreIncrement(_, e) => format!("++{}", expr_to_c(e)),
        Expression::PreDecrement(_, e) => format!("--{}", expr_to_c(e)),
        Expression::ConditionalOperator(_, c, t, e) => {
            format!("({} ? {} : {})", expr_to_c(c), expr_to_c(t), expr_to_c(e))
        }
        Expression::MemberAccess(_, base, member) => {
            match member {
                but_grammar::ast::Member::Number(n) => {
                    // Read bit N: ((variable) >> N) & 1
                    format!("(({}) >> {}) & 1", expr_to_c(base), n)
                }
                but_grammar::ast::Member::Identifier(id) => {
                    // Struct field access
                    format!("({}).{}", expr_to_c(base), id.name)
                }
            }
        }
        _ => "/* unsupported */".to_string(),
    }
}

/// Convert a Statement to C code with a given indent style and nesting level.
pub fn stmt_to_c(stmt: &Statement, style: &crate::IndentStyle, level: usize) -> String {
    let pad = style.level(level);
    match stmt {
        Statement::Block { statements, .. } => {
            let inner = statements
                .iter()
                .map(|s| stmt_to_c(s, style, level + 1))
                .collect::<String>();
            format!("{}{{\n{}{}}}\n", pad, inner, pad)
        }
        Statement::Expression(_, expr) => format!("{}{};\n", pad, expr_to_c(expr)),
        Statement::If(_, cond, then_s, else_s) => {
            let mut out = format!("{}if ({}) ", pad, expr_to_c(cond));
            out.push_str(&stmt_to_c(then_s, style, level));
            if let Some(else_body) = else_s {
                out.push_str(&format!("{}else ", pad));
                out.push_str(&stmt_to_c(else_body, style, level));
            }
            out
        }
        Statement::While(_, cond, body) => {
            format!("{}while ({}) {}", pad, expr_to_c(cond), stmt_to_c(body, style, level))
        }
        Statement::For(_, init, cond, post, body) => {
            let init_str = init.as_ref().map(|s| stmt_to_c(s, style, 0).trim_end_matches('\n').trim_end_matches(';').to_string()).unwrap_or_default();
            let cond_str = cond.as_ref().map(|e| expr_to_c(e)).unwrap_or_default();
            let post_str = post.as_ref().map(|e| expr_to_c(e)).unwrap_or_default();
            let body_str = body.as_ref().map(|b| stmt_to_c(b, style, level)).unwrap_or_default();
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
        _ => format!("{}/* unsupported statement */\n", pad),
    }
}

/// Check whether a type is the primitive bit type (`bit`).
fn is_bit_alias(ty: &but_grammar::ast::Type) -> bool {
    matches!(ty, but_grammar::ast::Type::Alias(id) if id.name == "bit")
}

/// Convert N bits to a C type using the smallest containing type rule.
///
/// - N ≤ 8  → `uint8_t`
/// - N ≤ 16 → `uint16_t`
/// - N ≤ 32 → `uint32_t`
/// - N ≤ 64 → `uint64_t`
/// - N > 64 → `uint64_t[⌈N/64⌉]` (array aligned to 64-bit boundary)
fn bit_count_to_c(count: u16) -> String {
    match count {
        1..=8   => "uint8_t".to_string(),
        9..=16  => "uint16_t".to_string(),
        17..=32 => "uint32_t".to_string(),
        33..=64 => "uint64_t".to_string(),
        n       => format!("uint64_t[{}]", (n as u32 + 63) / 64),
    }
}

/// Convert N bits to an ST type using the smallest containing type rule.
///
/// - N ≤ 8  → `BYTE`
/// - N ≤ 16 → `WORD`
/// - N ≤ 32 → `DWORD`
/// - N ≤ 64 → `LWORD`
/// - N > 64 → `ARRAY [0..⌈N/64⌉-1] OF LWORD`
fn bit_count_to_st(count: u16) -> String {
    match count {
        1..=8   => "BYTE".to_string(),
        9..=16  => "WORD".to_string(),
        17..=32 => "DWORD".to_string(),
        33..=64 => "LWORD".to_string(),
        n => {
            let words = (n as u32 + 63) / 64;
            format!("ARRAY [0..{}] OF LWORD", words - 1)
        }
    }
}

/// Convert a BuT type to a C type string.
///
/// Primitive language types: `bit` (1 bit) and `float` (floating-point number).
/// All other types are derived from `bit` via the `[N: bit]` construct.
pub fn type_to_c(ty: &but_grammar::ast::Type) -> String {
    use but_grammar::ast::Type;
    match ty {
        Type::Bool => "int".to_string(),
        Type::String => "char*".to_string(),
        Type::Rational => "double".to_string(),
        Type::Address => "unsigned long".to_string(),
        Type::Alias(id) => {
            match id.name.as_str() {
                // Primitive type: single bit → uint8_t (the nearest C integer type)
                "bit" => "uint8_t".to_string(),
                // Primitive type: floating-point number → float
                "float" => "float".to_string(),
                // Built-in aliases for compatibility with code without std.but
                "u8" | "byte" => "uint8_t".to_string(),
                "u16" => "uint16_t".to_string(),
                "u32" => "uint32_t".to_string(),
                "u64" => "uint64_t".to_string(),
                "u128" => "uint64_t[2]".to_string(),
                "int" | "i32" => "int32_t".to_string(),
                "i64" => "int64_t".to_string(),
                "real" | "f64" | "f32" => "double".to_string(),
                "bool" => "int".to_string(),
                "str" | "string" => "char*".to_string(),
                _ => id.name.clone(),
            }
        }
        Type::Array { element_count, element_type, .. } => {
            if is_bit_alias(element_type) {
                // Bit array → derived C integer type
                bit_count_to_c(*element_count)
            } else {
                // Array of arbitrary elements
                format!("{}[{}]", type_to_c(element_type), element_count)
            }
        }
        _ => "int".to_string(),
    }
}

/// Resolve a type alias through the alias table (up to 8 levels deep).
///
/// If `ty` is `Type::Alias(name)` and the name is found in the table, the resolved type is returned.
/// Repeated recursively (e.g. `type A = B; type B = u8;` → `u8`).
/// If the depth is exceeded or the entry is missing, the original type is returned.
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

/// Convert a BuT type to a C type string, resolving aliases from the table.
pub fn type_to_c_ctx(
    ty: &but_grammar::ast::Type,
    aliases: &HashMap<String, but_grammar::ast::Type>,
) -> String {
    type_to_c(&resolve_alias(ty, aliases))
}

/// Convert a BuT type to a Structured Text type string.
///
/// Primitive language types: `bit` (1 bit → BOOL) and `float` (floating-point → REAL).
/// Derived types `[N: bit]` map to BYTE/WORD/DWORD/LWORD/array.
pub fn type_to_st(ty: &but_grammar::ast::Type) -> String {
    use but_grammar::ast::Type;
    match ty {
        Type::Bool => "BOOL".to_string(),
        Type::String => "STRING".to_string(),
        Type::Rational => "REAL".to_string(),
        Type::Array { element_count, element_type, .. } => {
            if is_bit_alias(element_type) {
                // Bit array → derived ST type
                bit_count_to_st(*element_count)
            } else {
                // Array of arbitrary elements
                format!("ARRAY [0..{}] OF {}", element_count - 1, type_to_st(element_type))
            }
        }
        Type::Alias(id) => match id.name.as_str() {
            // Primitive type: single bit → BOOL (1-bit IEC 61131-3 type)
            "bit" => "BOOL".to_string(),
            // Primitive type: floating-point number → REAL
            "float" => "REAL".to_string(),
            // Built-in aliases for compatibility with code without std.but
            "u8" | "byte" => "BYTE".to_string(),
            "u16" => "WORD".to_string(),
            "u32" => "DWORD".to_string(),
            "u64" => "LWORD".to_string(),
            "u128" => "ARRAY [0..1] OF LWORD".to_string(),
            "int" | "i32" | "i64" => "INT".to_string(),
            "real" | "f64" | "f32" => "REAL".to_string(),
            "bool" => "BOOL".to_string(),
            "str" | "string" => "STRING".to_string(),
            _ => id.name.clone(),
        },
        _ => "INT".to_string(),
    }
}

/// Convert a BuT type to an ST type string, resolving aliases from the table.
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

    // Helper function: create a dummy location
    fn loc() -> Loc {
        Loc::Source(0, 0, 0)
    }

    // Helper function: create an identifier
    fn ident(name: &str) -> Identifier {
        Identifier::new(name)
    }

    // Helper function: create a type alias
    fn alias(name: &str) -> Type {
        Type::Alias(ident(name))
    }

    // ===== condition_to_c =====

    #[test]
    fn condition_variable_to_c() {
        let cond = Condition::Variable(ident("flag"));
        assert_eq!(condition_to_c(&cond), "flag");
    }

    #[test]
    fn condition_bool_true_to_c() {
        let cond = Condition::BoolLiteral(loc(), true);
        assert_eq!(condition_to_c(&cond), "1");
    }

    #[test]
    fn condition_bool_false_to_c() {
        let cond = Condition::BoolLiteral(loc(), false);
        assert_eq!(condition_to_c(&cond), "0");
    }

    #[test]
    fn condition_number_literal_to_c() {
        let cond = Condition::NumberLiteral(loc(), 42);
        assert_eq!(condition_to_c(&cond), "42");
    }

    #[test]
    fn condition_equal_to_c() {
        let l = Box::new(Condition::Variable(ident("x")));
        let r = Box::new(Condition::NumberLiteral(loc(), 0));
        let cond = Condition::Equal(loc(), l, r);
        assert_eq!(condition_to_c(&cond), "(x == 0)");
    }

    #[test]
    fn condition_not_equal_to_c() {
        let l = Box::new(Condition::Variable(ident("a")));
        let r = Box::new(Condition::NumberLiteral(loc(), 1));
        let cond = Condition::NotEqual(loc(), l, r);
        assert_eq!(condition_to_c(&cond), "(a != 1)");
    }

    #[test]
    fn condition_and_to_c() {
        let l = Box::new(Condition::Variable(ident("a")));
        let r = Box::new(Condition::Variable(ident("b")));
        let cond = Condition::And(loc(), l, r);
        assert_eq!(condition_to_c(&cond), "(a && b)");
    }

    #[test]
    fn condition_or_to_c() {
        let l = Box::new(Condition::Variable(ident("p")));
        let r = Box::new(Condition::Variable(ident("q")));
        let cond = Condition::Or(loc(), l, r);
        assert_eq!(condition_to_c(&cond), "(p || q)");
    }

    #[test]
    fn condition_not_to_c() {
        let inner = Box::new(Condition::Variable(ident("done")));
        let cond = Condition::Not(loc(), inner);
        assert_eq!(condition_to_c(&cond), "(!done)");
    }

    #[test]
    fn condition_less_to_c() {
        let l = Box::new(Condition::Variable(ident("i")));
        let r = Box::new(Condition::NumberLiteral(loc(), 10));
        let cond = Condition::Less(loc(), l, r);
        assert_eq!(condition_to_c(&cond), "(i < 10)");
    }

    #[test]
    fn condition_more_to_c() {
        let l = Box::new(Condition::Variable(ident("i")));
        let r = Box::new(Condition::NumberLiteral(loc(), 5));
        let cond = Condition::More(loc(), l, r);
        assert_eq!(condition_to_c(&cond), "(i > 5)");
    }

    #[test]
    fn condition_add_to_c() {
        let l = Box::new(Condition::Variable(ident("a")));
        let r = Box::new(Condition::NumberLiteral(loc(), 3));
        let cond = Condition::Add(loc(), l, r);
        assert_eq!(condition_to_c(&cond), "(a + 3)");
    }

    #[test]
    fn condition_subtract_to_c() {
        let l = Box::new(Condition::Variable(ident("a")));
        let r = Box::new(Condition::NumberLiteral(loc(), 1));
        let cond = Condition::Subtract(loc(), l, r);
        assert_eq!(condition_to_c(&cond), "(a - 1)");
    }

    #[test]
    fn condition_parenthesis_to_c() {
        let inner = Box::new(Condition::Variable(ident("x")));
        let cond = Condition::Parenthesis(loc(), inner);
        assert_eq!(condition_to_c(&cond), "(x)");
    }

    // ===== expr_to_c =====

    #[test]
    fn expr_number_literal_to_c() {
        let expr = Expression::NumberLiteral(loc(), 100);
        assert_eq!(expr_to_c(&expr), "100");
    }

    #[test]
    fn expr_bool_true_to_c() {
        let expr = Expression::BoolLiteral(loc(), true);
        assert_eq!(expr_to_c(&expr), "1");
    }

    #[test]
    fn expr_bool_false_to_c() {
        let expr = Expression::BoolLiteral(loc(), false);
        assert_eq!(expr_to_c(&expr), "0");
    }

    #[test]
    fn expr_variable_to_c() {
        let expr = Expression::Variable(ident("result"));
        assert_eq!(expr_to_c(&expr), "result");
    }

    #[test]
    fn expr_add_to_c() {
        let l = Box::new(Expression::Variable(ident("x")));
        let r = Box::new(Expression::NumberLiteral(loc(), 5));
        let expr = Expression::Add(loc(), l, r);
        assert_eq!(expr_to_c(&expr), "(x + 5)");
    }

    #[test]
    fn expr_subtract_to_c() {
        let l = Box::new(Expression::Variable(ident("y")));
        let r = Box::new(Expression::NumberLiteral(loc(), 2));
        let expr = Expression::Subtract(loc(), l, r);
        assert_eq!(expr_to_c(&expr), "(y - 2)");
    }

    #[test]
    fn expr_assign_to_c() {
        let l = Box::new(Expression::Variable(ident("output")));
        let r = Box::new(Expression::NumberLiteral(loc(), 1));
        let expr = Expression::Assign(loc(), l, r);
        assert_eq!(expr_to_c(&expr), "output = 1");
    }

    #[test]
    fn expr_assign_add_to_c() {
        let l = Box::new(Expression::Variable(ident("x")));
        let r = Box::new(Expression::NumberLiteral(loc(), 1));
        let expr = Expression::AssignAdd(loc(), l, r);
        assert_eq!(expr_to_c(&expr), "x += 1");
    }

    #[test]
    fn expr_assign_subtract_to_c() {
        let l = Box::new(Expression::Variable(ident("x")));
        let r = Box::new(Expression::NumberLiteral(loc(), 1));
        let expr = Expression::AssignSubtract(loc(), l, r);
        assert_eq!(expr_to_c(&expr), "x -= 1");
    }

    #[test]
    fn expr_multiply_to_c() {
        let l = Box::new(Expression::Variable(ident("a")));
        let r = Box::new(Expression::NumberLiteral(loc(), 3));
        let expr = Expression::Multiply(loc(), l, r);
        assert_eq!(expr_to_c(&expr), "(a * 3)");
    }

    #[test]
    fn expr_not_to_c() {
        let inner = Box::new(Expression::Variable(ident("flag")));
        let expr = Expression::Not(loc(), inner);
        assert_eq!(expr_to_c(&expr), "(!flag)");
    }

    #[test]
    fn expr_negate_to_c() {
        let inner = Box::new(Expression::Variable(ident("val")));
        let expr = Expression::Negate(loc(), inner);
        assert_eq!(expr_to_c(&expr), "(-val)");
    }

    #[test]
    fn expr_parenthesis_to_c() {
        let inner = Box::new(Expression::NumberLiteral(loc(), 99));
        let expr = Expression::Parenthesis(loc(), inner);
        assert_eq!(expr_to_c(&expr), "(99)");
    }

    // ===== type_to_c =====

    #[test]
    fn type_u8_to_c() {
        assert_eq!(type_to_c(&alias("u8")), "uint8_t");
    }

    #[test]
    fn type_u16_to_c() {
        assert_eq!(type_to_c(&alias("u16")), "uint16_t");
    }

    #[test]
    fn type_u32_to_c() {
        assert_eq!(type_to_c(&alias("u32")), "uint32_t");
    }

    #[test]
    fn type_bit_to_c() {
        assert_eq!(type_to_c(&alias("bit")), "uint8_t");
    }

    #[test]
    fn type_bool_alias_to_c() {
        assert_eq!(type_to_c(&alias("bool")), "int");
    }

    #[test]
    fn type_bool_to_c() {
        assert_eq!(type_to_c(&Type::Bool), "int");
    }

    #[test]
    fn type_string_to_c() {
        assert_eq!(type_to_c(&Type::String), "char*");
    }

    #[test]
    fn type_rational_to_c() {
        assert_eq!(type_to_c(&Type::Rational), "double");
    }

    #[test]
    fn type_unknown_alias_to_c() {
        assert_eq!(type_to_c(&alias("MyType")), "MyType");
    }

    // ===== type_to_st =====

    #[test]
    fn type_bool_to_st() {
        assert_eq!(type_to_st(&Type::Bool), "BOOL");
    }

    #[test]
    fn type_string_to_st() {
        assert_eq!(type_to_st(&Type::String), "STRING");
    }

    #[test]
    fn type_rational_to_st() {
        assert_eq!(type_to_st(&Type::Rational), "REAL");
    }

    #[test]
    fn type_u8_to_st() {
        assert_eq!(type_to_st(&alias("u8")), "BYTE");
    }

    #[test]
    fn type_u16_to_st() {
        assert_eq!(type_to_st(&alias("u16")), "WORD");
    }

    #[test]
    fn type_u32_to_st() {
        assert_eq!(type_to_st(&alias("u32")), "DWORD");
    }

    #[test]
    fn type_u64_to_st() {
        assert_eq!(type_to_st(&alias("u64")), "LWORD");
    }

    #[test]
    fn type_bit_to_st() {
        // Single bit — logical type (BOOL in IEC 61131-3), not BYTE
        assert_eq!(type_to_st(&alias("bit")), "BOOL");
    }

    #[test]
    fn type_bool_alias_to_st() {
        assert_eq!(type_to_st(&alias("bool")), "BOOL");
    }

    #[test]
    fn type_int_to_st() {
        assert_eq!(type_to_st(&alias("int")), "INT");
    }

    #[test]
    fn type_unknown_alias_to_st() {
        assert_eq!(type_to_st(&alias("CustomType")), "CustomType");
    }

    #[test]
    fn type_address_to_st() {
        // Address is not Alias/Bool/String/Rational — returns "INT"
        assert_eq!(type_to_st(&Type::Address), "INT");
    }

    // ===== resolve_alias =====

    fn make_aliases(pairs: &[(&str, Type)]) -> HashMap<String, Type> {
        pairs.iter().map(|(k, v)| (k.to_string(), v.clone())).collect()
    }

    #[test]
    fn resolve_alias_known_alias_is_resolved() {
        let aliases = make_aliases(&[("MyByte", alias("u8"))]);
        let resolved = resolve_alias(&alias("MyByte"), &aliases);
        assert_eq!(resolved, alias("u8"));
    }

    #[test]
    fn resolve_alias_unknown_alias_stays_unchanged() {
        let aliases = make_aliases(&[]);
        let resolved = resolve_alias(&alias("Unknown"), &aliases);
        assert_eq!(resolved, alias("Unknown"));
    }

    #[test]
    fn resolve_alias_chain_is_resolved() {
        // Counter → MyInt → i32
        let aliases = make_aliases(&[
            ("Counter", alias("MyInt")),
            ("MyInt", alias("i32")),
        ]);
        let resolved = resolve_alias(&alias("Counter"), &aliases);
        assert_eq!(resolved, alias("i32"));
    }

    #[test]
    fn resolve_alias_primitive_type_unchanged() {
        let aliases = make_aliases(&[]);
        assert_eq!(resolve_alias(&Type::Bool, &aliases), Type::Bool);
        assert_eq!(resolve_alias(&Type::Rational, &aliases), Type::Rational);
        assert_eq!(resolve_alias(&Type::String, &aliases), Type::String);
    }

    #[test]
    fn resolve_alias_to_array() {
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
    fn type_to_c_ctx_resolves_user_alias_to_c() {
        // type MyByte = u8; → uint8_t
        let aliases = make_aliases(&[("MyByte", alias("u8"))]);
        assert_eq!(type_to_c_ctx(&alias("MyByte"), &aliases), "uint8_t");
    }

    #[test]
    fn type_to_c_ctx_alias_chain_to_c() {
        // type Counter = MyU32; type MyU32 = u32; → uint32_t
        let aliases = make_aliases(&[
            ("Counter", alias("MyU32")),
            ("MyU32", alias("u32")),
        ]);
        assert_eq!(type_to_c_ctx(&alias("Counter"), &aliases), "uint32_t");
    }

    #[test]
    fn type_to_c_ctx_without_alias_works_like_type_to_c() {
        let aliases = make_aliases(&[]);
        assert_eq!(type_to_c_ctx(&alias("u16"), &aliases), "uint16_t");
        assert_eq!(type_to_c_ctx(&Type::Bool, &aliases), "int");
    }

    #[test]
    fn type_to_c_ctx_unknown_alias_returns_name() {
        let aliases = make_aliases(&[]);
        assert_eq!(type_to_c_ctx(&alias("CustomType"), &aliases), "CustomType");
    }

    #[test]
    fn type_to_c_ctx_bool_alias_to_c() {
        let aliases = make_aliases(&[("Flag", Type::Bool)]);
        assert_eq!(type_to_c_ctx(&alias("Flag"), &aliases), "int");
    }

    // ===== type_to_st_ctx =====

    #[test]
    fn type_to_st_ctx_resolves_user_alias_to_st() {
        // type Counter = u32; → DWORD
        let aliases = make_aliases(&[("Counter", alias("u32"))]);
        assert_eq!(type_to_st_ctx(&alias("Counter"), &aliases), "DWORD");
    }

    #[test]
    fn type_to_st_ctx_alias_chain_to_st() {
        // type MyWord = MyU16; type MyU16 = u16; → WORD
        let aliases = make_aliases(&[
            ("MyWord", alias("MyU16")),
            ("MyU16", alias("u16")),
        ]);
        assert_eq!(type_to_st_ctx(&alias("MyWord"), &aliases), "WORD");
    }

    #[test]
    fn type_to_st_ctx_without_alias_works_like_type_to_st() {
        let aliases = make_aliases(&[]);
        assert_eq!(type_to_st_ctx(&alias("u8"), &aliases), "BYTE");
        assert_eq!(type_to_st_ctx(&Type::Bool, &aliases), "BOOL");
    }

    #[test]
    fn type_to_st_ctx_rational_alias_to_st() {
        let aliases = make_aliases(&[("Speed", Type::Rational)]);
        assert_eq!(type_to_st_ctx(&alias("Speed"), &aliases), "REAL");
    }

    // ===== Type system: primitive types bit and float =====

    // --- float → C and ST ---

    // ===== Bit access: condition_to_c =====

    fn member_num(n: i64) -> but_grammar::ast::Member {
        but_grammar::ast::Member::Number(n)
    }
    fn member_ident(name: &str) -> but_grammar::ast::Member {
        but_grammar::ast::Member::Identifier(ident(name))
    }

    #[test]
    fn condition_bit_access_read_to_c() {
        // a.5 → ((a) >> 5) & 1
        let cond = Condition::MemberAccess(
            loc(),
            Box::new(Condition::Variable(ident("a"))),
            member_num(5),
        );
        assert_eq!(condition_to_c(&cond), "((a) >> 5) & 1");
    }

    #[test]
    fn condition_bit_access_zero_bit_to_c() {
        // a.0 → ((a) >> 0) & 1
        let cond = Condition::MemberAccess(
            loc(),
            Box::new(Condition::Variable(ident("flags"))),
            member_num(0),
        );
        assert_eq!(condition_to_c(&cond), "((flags) >> 0) & 1");
    }

    #[test]
    fn condition_bit_access_struct_field_to_c() {
        // obj.field → (obj).field
        let cond = Condition::MemberAccess(
            loc(),
            Box::new(Condition::Variable(ident("obj"))),
            member_ident("field"),
        );
        assert_eq!(condition_to_c(&cond), "(obj).field");
    }

    #[test]
    fn condition_bit_access_to_verilog() {
        // a.5 → a[5]
        let cond = Condition::MemberAccess(
            loc(),
            Box::new(Condition::Variable(ident("a"))),
            member_num(5),
        );
        assert_eq!(condition_to_verilog(&cond), "a[5]");
    }

    // ===== Bit access: expr_to_c =====

    #[test]
    fn expr_bit_access_read_to_c() {
        // a.3 → ((a) >> 3) & 1
        let expr = Expression::MemberAccess(
            loc(),
            Box::new(Expression::Variable(ident("a"))),
            member_num(3),
        );
        assert_eq!(expr_to_c(&expr), "((a) >> 3) & 1");
    }

    #[test]
    fn expr_bit_access_large_index_to_c() {
        // data.63 → ((data) >> 63) & 1
        let expr = Expression::MemberAccess(
            loc(),
            Box::new(Expression::Variable(ident("data"))),
            member_num(63),
        );
        assert_eq!(expr_to_c(&expr), "((data) >> 63) & 1");
    }

    #[test]
    fn expr_bit_write_true_to_c() {
        // a.5 = 1 → a = ((a) & ~(1UL << 5)) | ((1 & 1) << 5)
        let lhs = Expression::MemberAccess(
            loc(),
            Box::new(Expression::Variable(ident("a"))),
            member_num(5),
        );
        let rhs = Expression::BoolLiteral(loc(), true);
        let expr = Expression::Assign(loc(), Box::new(lhs), Box::new(rhs));
        assert_eq!(expr_to_c(&expr), "a = ((a) & ~(1UL << 5)) | ((1 & 1) << 5)");
    }

    #[test]
    fn expr_bit_write_false_to_c() {
        // a.5 = 0 → a = ((a) & ~(1UL << 5)) | ((0 & 1) << 5)
        let lhs = Expression::MemberAccess(
            loc(),
            Box::new(Expression::Variable(ident("a"))),
            member_num(5),
        );
        let rhs = Expression::BoolLiteral(loc(), false);
        let expr = Expression::Assign(loc(), Box::new(lhs), Box::new(rhs));
        assert_eq!(expr_to_c(&expr), "a = ((a) & ~(1UL << 5)) | ((0 & 1) << 5)");
    }

    #[test]
    fn expr_bit_write_zero_bit_to_c() {
        // flags.0 = 1 → flags = ((flags) & ~(1UL << 0)) | ((1 & 1) << 0)
        let lhs = Expression::MemberAccess(
            loc(),
            Box::new(Expression::Variable(ident("flags"))),
            member_num(0),
        );
        let rhs = Expression::NumberLiteral(loc(), 1);
        let expr = Expression::Assign(loc(), Box::new(lhs), Box::new(rhs));
        assert_eq!(expr_to_c(&expr), "flags = ((flags) & ~(1UL << 0)) | ((1 & 1) << 0)");
    }

    #[test]
    fn expr_plain_assign_unchanged() {
        // x = 5 (not bit access) → x = 5
        let lhs = Expression::Variable(ident("x"));
        let rhs = Expression::NumberLiteral(loc(), 5);
        let expr = Expression::Assign(loc(), Box::new(lhs), Box::new(rhs));
        assert_eq!(expr_to_c(&expr), "x = 5");
    }

    #[test]
    fn type_float_to_c() {
        // Primitive type float → float (C)
        assert_eq!(type_to_c(&alias("float")), "float");
    }

    #[test]
    fn type_float_to_st() {
        // Primitive type float → REAL (ST / IEC 61131-3)
        assert_eq!(type_to_st(&alias("float")), "REAL");
    }

    // --- Bit arrays → derived integer types (C) ---

    fn bit_array(count: u16) -> Type {
        Type::Array {
            loc: Loc::Source(0, 0, 0),
            element_count: count,
            element_type: Box::new(alias("bit")),
        }
    }

    #[test]
    fn type_array_8_bits_to_c() {
        // [8: bit] = u8 → uint8_t
        assert_eq!(type_to_c(&bit_array(8)), "uint8_t");
    }

    #[test]
    fn type_array_16_bits_to_c() {
        // [16: bit] = u16 → uint16_t
        assert_eq!(type_to_c(&bit_array(16)), "uint16_t");
    }

    #[test]
    fn type_array_32_bits_to_c() {
        // [32: bit] = u32 → uint32_t
        assert_eq!(type_to_c(&bit_array(32)), "uint32_t");
    }

    #[test]
    fn type_array_64_bits_to_c() {
        // [64: bit] = u64 → uint64_t
        assert_eq!(type_to_c(&bit_array(64)), "uint64_t");
    }

    #[test]
    fn type_array_128_bits_to_c() {
        // [128: bit] = u128 → uint64_t[2] (array of 2 × 64 bits)
        assert_eq!(type_to_c(&bit_array(128)), "uint64_t[2]");
    }

    #[test]
    fn type_array_256_bits_to_c() {
        // [256: bit] → uint64_t[4]
        assert_eq!(type_to_c(&bit_array(256)), "uint64_t[4]");
    }

    // --- Bit arrays → derived integer types (ST) ---

    #[test]
    fn type_array_8_bits_to_st() {
        // [8: bit] = u8 → BYTE
        assert_eq!(type_to_st(&bit_array(8)), "BYTE");
    }

    #[test]
    fn type_array_16_bits_to_st() {
        // [16: bit] = u16 → WORD
        assert_eq!(type_to_st(&bit_array(16)), "WORD");
    }

    #[test]
    fn type_array_32_bits_to_st() {
        // [32: bit] = u32 → DWORD
        assert_eq!(type_to_st(&bit_array(32)), "DWORD");
    }

    #[test]
    fn type_array_64_bits_to_st() {
        // [64: bit] = u64 → LWORD
        assert_eq!(type_to_st(&bit_array(64)), "LWORD");
    }

    #[test]
    fn type_array_128_bits_to_st() {
        // [128: bit] = u128 → ARRAY [0..1] OF LWORD
        assert_eq!(type_to_st(&bit_array(128)), "ARRAY [0..1] OF LWORD");
    }

    // --- Derived types via std.but-style aliases ---

    #[test]
    fn type_to_c_ctx_u8_via_bit_array() {
        // type u8 = [8: bit]; → uint8_t
        let aliases = make_aliases(&[("u8", bit_array(8))]);
        assert_eq!(type_to_c_ctx(&alias("u8"), &aliases), "uint8_t");
    }

    #[test]
    fn type_to_c_ctx_u16_via_bit_array() {
        // type u16 = [16: bit]; → uint16_t
        let aliases = make_aliases(&[("u16", bit_array(16))]);
        assert_eq!(type_to_c_ctx(&alias("u16"), &aliases), "uint16_t");
    }

    #[test]
    fn type_to_c_ctx_u32_via_bit_array() {
        // type u32 = [32: bit]; → uint32_t
        let aliases = make_aliases(&[("u32", bit_array(32))]);
        assert_eq!(type_to_c_ctx(&alias("u32"), &aliases), "uint32_t");
    }

    #[test]
    fn type_to_c_ctx_u64_via_bit_array() {
        // type u64 = [64: bit]; → uint64_t
        let aliases = make_aliases(&[("u64", bit_array(64))]);
        assert_eq!(type_to_c_ctx(&alias("u64"), &aliases), "uint64_t");
    }

    #[test]
    fn type_to_c_ctx_u128_via_bit_array() {
        // type u128 = [128: bit]; → uint64_t[2]
        let aliases = make_aliases(&[("u128", bit_array(128))]);
        assert_eq!(type_to_c_ctx(&alias("u128"), &aliases), "uint64_t[2]");
    }

    #[test]
    fn type_to_st_ctx_u8_via_bit_array() {
        // type u8 = [8: bit]; → BYTE
        let aliases = make_aliases(&[("u8", bit_array(8))]);
        assert_eq!(type_to_st_ctx(&alias("u8"), &aliases), "BYTE");
    }

    #[test]
    fn type_to_st_ctx_u32_via_bit_array() {
        // type u32 = [32: bit]; → DWORD
        let aliases = make_aliases(&[("u32", bit_array(32))]);
        assert_eq!(type_to_st_ctx(&alias("u32"), &aliases), "DWORD");
    }

    #[test]
    fn type_to_st_ctx_u128_via_bit_array() {
        // type u128 = [128: bit]; → ARRAY [0..1] OF LWORD
        let aliases = make_aliases(&[("u128", bit_array(128))]);
        assert_eq!(type_to_st_ctx(&alias("u128"), &aliases), "ARRAY [0..1] OF LWORD");
    }

    #[test]
    fn type_to_c_ctx_bool_via_bit() {
        // type bool = bit; → uint8_t
        let aliases = make_aliases(&[("bool", alias("bit"))]);
        assert_eq!(type_to_c_ctx(&alias("bool"), &aliases), "uint8_t");
    }

    #[test]
    fn type_to_st_ctx_bool_via_bit() {
        // type bool = bit; → BOOL
        let aliases = make_aliases(&[("bool", alias("bit"))]);
        assert_eq!(type_to_st_ctx(&alias("bool"), &aliases), "BOOL");
    }

    #[test]
    fn type_to_c_ctx_counter_chain_via_std() {
        // type u32 = [32: bit]; type Counter = u32; → uint32_t
        let aliases = make_aliases(&[
            ("u32", bit_array(32)),
            ("Counter", alias("u32")),
        ]);
        assert_eq!(type_to_c_ctx(&alias("Counter"), &aliases), "uint32_t");
    }

    #[test]
    fn type_u128_alias_to_c() {
        // Built-in alias u128 without std.but → uint64_t[2]
        assert_eq!(type_to_c(&alias("u128")), "uint64_t[2]");
    }

    #[test]
    fn type_u128_alias_to_st() {
        // Built-in alias u128 without std.but → ARRAY [0..1] OF LWORD
        assert_eq!(type_to_st(&alias("u128")), "ARRAY [0..1] OF LWORD");
    }
}
