/// Модуль абстрактного синтаксического дерева языка Takt.
pub mod ast;
/// Узел АСД "условие перехода".
pub mod ast_cond;
pub mod ast_expr;

/// Покрытие конструкций языка примерами: счёт видов узлов АСД.
pub mod coverage;
/// Глубина дерева АСД и предел разбора.
pub(crate) mod depth;
/// Ошибки лексического анализа.
pub mod lex_error;
/// Модуль лексического анализатора Takt.
pub mod lexer;
/// Границы числового литерала.
mod literal_range;
/// Литералы длительности и частоты.
pub mod time_literal;
pub mod token;

/// Имеет ли выражение **эффект**, то есть законно ли оно как оператор.
///
/// Оператором-выражением остаются присваивание (`led := 1;`) и вызов функции
/// (`beep();`): у них есть действие. Прочее - вычисление, результат которого некуда
/// деть; такая запись отвергается парсером (`SY-007`,).
///
/// Скобки прозрачны: `(f(x));` - тот же вызов.
///
/// Живёт здесь, а не в грамматике: правило языка обязано быть **читаемым** и
/// проверяемым тестом, а не спрятанным в действии LALRPOP.
pub fn expression_has_effect(expr: &ast::Expression) -> bool {
    match expr {
        // Присваивание - запись; вызов - действие (в том числе именованный, которым
        // инстанцируется модель).
        ast::Expression::Assign(..)
        | ast::Expression::Function(..)
        | ast::Expression::NamedFunction(..)
        // Блок кода как выражение исполняется ради своих операторов.
        | ast::Expression::CodeBlock(..) => true,
        ast::Expression::Parenthesis(_, inner) => expression_has_effect(inner),
        _ => false,
    }
}

/// Разбирает строку адресного литерала вида `"0xNNNN:bit"` в пару `(адрес, бит)`.
///
/// Используется в правилах LALRPOP-грамматики для токена
/// [`lexer::Token::AddressLiteral`]. Поддерживает шестнадцатеричную (`0x...`) и
/// десятичную форму адреса.
///
/// # Примеры
///
/// ```
/// use takt_lang::parser::parse_address_literal;
/// assert_eq!(parse_address_literal("0x00200000:0"), (2097152, 0));
/// assert_eq!(parse_address_literal("0xFF:3"),       (255, 3));
/// ```
pub fn parse_address_literal(s: &str) -> (i64, i64) {
    if let Some(pos) = s.rfind(':') {
        let addr_part = &s[..pos];
        let bit_part = &s[pos + 1..];
        let addr = if addr_part.starts_with("0x") || addr_part.starts_with("0X") {
            i64::from_str_radix(&addr_part[2..], 16).unwrap_or(0)
        } else {
            addr_part.parse::<i64>().unwrap_or(0)
        };
        let bit = bit_part.parse::<i64>().unwrap_or(0);
        (addr, bit)
    } else {
        (0, 0)
    }
}
