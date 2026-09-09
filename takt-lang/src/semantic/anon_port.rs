//! Анонимное обращение к ячейке по адресу - `#0x346619:0 as u64`.
//!
//! ## Что здесь живёт
//!
//! **Единая воронка свёртки.** Форма записи собирается из трёх узлов АСД,
//! которые в языке уже есть: сам адрес (`Expression::AnonAddress`), приведение
//! (`as T`) и битовый доступ (`.N`). Свёртка их в тройку `{адрес, бит, ширина}`
//! написана **один раз** и вызывается обоими потребителями - построением
//! выражений и построением условий. Разъехавшись, они дали бы разный доступ для
//! одного текста (: "арифметика - в одном месте";:
//! "канонизация - в единой воронке").
//!
//! ## Пределы - те же, что у именованного порта
//!
//! Бит в `[0, 63]` и слово не шире 64 бит - правило (`SE-060`). Анонимная ячейка и
//! порт, размещённый по тому же адресу, - одна и та же память: два закона для неё
//! недопустимы.

use crate::diagnostics::lang::keys;
use crate::msg;
use crate::diagnostics::{Diagnostic, Location};
use crate::parser::ast;
use crate::semantic::type_inference::ast_type_to_node;
use crate::semantic::type_node::TypeNode;
use crate::semantic::type_node::type_fixed::fixed_storage_bits;

/// Наибольшая позиция бита в слове доступа.
pub const MAX_ANON_BIT: i64 = 63;

/// Наибольшая разрядность слова доступа, бит.
pub const MAX_ANON_WIDTH: u16 = 64;

/// Разрешённое анонимное обращение: адрес, позиция бита и тип доступа.
///
/// Тройка `{адрес, бит, ширина}` - та же, которой оперируют умолчательный HAL цели `c-hal`
/// (`{addr, bit, width}`) и регистровый файл цели `sv-mmio` (`reg[bit +: width]`).
/// Собственного представления фича не заводит намеренно: разойдясь, они дали бы разный
/// доступ к одной ячейке.
#[derive(Debug, Clone)]
pub struct AnonPortAccess {
    /// Адрес ячейки.
    pub addr: i64,
    /// Позиция младшего бита поля в слове.
    pub bit: i64,
    /// Тип доступа: он же задаёт разрядность поля.
    pub ty: TypeNode,
    /// Место обращения в исходнике.
    ///
    /// Нужно координате отказа: цели без адресного пространства (`c`, `st`, `sv`)
    /// отвергают такую модель до первой строки вывода, когда носитель позиции ещё пуст, -
    /// и сообщение печаталось без места.
    ///
    /// В равенство поле **не входит** (`PartialEq` ниже написан руками): два обращения
    /// к одной ячейке из разных строк - одна и та же ячейка, и путать её идентичность с
    /// местом нельзя (тот же урок, что у `Extend`).
    pub loc: Location,
}

impl PartialEq for AnonPortAccess {
    fn eq(&self, other: &Self) -> bool {
        self.addr == other.addr && self.bit == other.bit && self.ty == other.ty
    }
}

impl Eq for AnonPortAccess {}

impl AnonPortAccess {
    /// Разрядность поля в битах.
    pub fn width_bits(&self) -> u16 {
        width_of(&self.ty).unwrap_or(1)
    }

    /// Имя синтетического порта, которым ячейка представляется целям и эталону.
    ///
    /// Имени у анонимного обращения нет по определению, но `st-at` (локация принадлежит
    /// **объявлению**), `sv-mmio` (сигнал регистрового файла) и симулятор (значение
    /// наблюдается в трассе) без имени работать не умеют. Имя строится **здесь**, а не
    /// у каждого потребителя: одна ячейка обязана называться одинаково во всех трёх,
    /// иначе сверка эталона с целью сравнит разные величины.
    ///
    /// Форма - `AT_<адрес>_<бит>_<ширина>`: без `0x` (иначе `0X` в верхнем регистре у
    /// IEC) и без ведущего подчёркивания (идентификатор IEC 61131-3 не вправе с него
    /// начинаться).
    pub fn synthetic_name(&self) -> String {
        format!(
            "AT_{:X}_{}_{}",
            self.addr as u64,
            self.bit,
            self.width_bits()
        )
    }
}

/// Разрядность типа в битах, если доступ к памяти такой ширины осмыслен.
///
/// `None` - тип к прямому доступу непригоден; вызывающий обязан дать `SE-098`, а не
/// выбрать ширину за автора (: молчаливое умолчание `_ => 4` в ширине доступа к MMIO
/// уже стоило проекту дефекта).
fn width_of(ty: &TypeNode) -> Option<u16> {
    match ty {
        TypeNode::Bit | TypeNode::Bool => Some(1),
        TypeNode::Integer { bits, .. } => Some(u16::from(*bits)),
        TypeNode::Fixed { m, n, .. } => Some(u16::from(fixed_storage_bits(m + n))),
        _ => None,
    }
}

/// Свёртка анонимного обращения в **выражении**.
///
/// Возвращает `None`, если выражение анонимным обращением не является, - тогда
/// вызывающий строит узел как обычно.
pub(crate) fn fold_expression(
    expr: &ast::Expression,
) -> Option<Result<AnonPortAccess, Diagnostic>> {
    match strip_parens_expr(expr) {
        // `#A as T` - ширину задаёт приведение.
        ast::Expression::Cast(loc, inner, ty) => {
            let (addr, bit) = anon_of_expr(inner)?;
            Some(build(*loc, addr, bit, None, ast_type_to_node(ty)))
        }
        // `#A.N` - один бит; позиция задана точкой.
        ast::Expression::BitAccess(loc, inner, member) => {
            let (addr, bit) = anon_of_expr(inner)?;
            Some(build(*loc, addr, bit, Some(member), TypeNode::Bit))
        }
        // Голое `#A` - ширина не задана.
        ast::Expression::AnonAddress(loc, addr, bit) => Some(Err(width_missing(*loc, *addr, *bit))),
        _ => None,
    }
}

/// Свёртка анонимного обращения в **условии**.
///
/// В грамматике условий приведения нет (`as` там не разбирается), поэтому законна
/// только битовая форма; словную отвергает [`width_missing`] - диагностика **называет**
/// причину, вместо того чтобы парсер перечислял ожидаемые токены.
pub(crate) fn fold_condition(cond: &ast::Condition) -> Option<Result<AnonPortAccess, Diagnostic>> {
    match strip_parens_cond(cond) {
        ast::Condition::BitAccess(loc, inner, member) => {
            let (addr, bit) = anon_of_cond(inner)?;
            Some(build(*loc, addr, bit, Some(member), TypeNode::Bit))
        }
        ast::Condition::AnonAddress(loc, addr, bit) => Some(Err(width_missing(*loc, *addr, *bit))),
        _ => None,
    }
}

/// Снимает скобки у выражения.
///
/// Снятие действует **только** здесь, у операнда анонимного обращения: глобально скобки
/// снимать нельзя - генераторы печатают их как группировку, и в цели `rust` приоритеты
/// разойдутся с C молча.
fn strip_parens_expr(expr: &ast::Expression) -> &ast::Expression {
    let mut current = expr;
    while let ast::Expression::Parenthesis(_, inner) = current {
        current = inner;
    }
    current
}

/// Снимает скобки у условия (см. оговорку у [`strip_parens_expr`]).
fn strip_parens_cond(cond: &ast::Condition) -> &ast::Condition {
    let mut current = cond;
    while let ast::Condition::Parenthesis(_, inner) = current {
        current = inner;
    }
    current
}

/// Возвращает `(адрес, записанный бит)`, если операнд - анонимное обращение.
fn anon_of_expr(expr: &ast::Expression) -> Option<(i128, Option<i64>)> {
    match strip_parens_expr(expr) {
        ast::Expression::AnonAddress(_, addr, bit) => Some((*addr, *bit)),
        _ => None,
    }
}

/// Возвращает `(адрес, записанный бит)`, если операнд условия - анонимное обращение.
fn anon_of_cond(cond: &ast::Condition) -> Option<(i128, Option<i64>)> {
    match strip_parens_cond(cond) {
        ast::Condition::AnonAddress(_, addr, bit) => Some((*addr, *bit)),
        _ => None,
    }
}

/// Собирает и проверяет тройку доступа.
///
/// `literal_bit` - позиция из адресного литерала (`#A:3`), `member` - позиция из
/// битового доступа (`#A.3`). Заданы обе - это две разные записи одного свойства в
/// одном обращении, и выбрать за автора нельзя.
fn build(
    loc: Location,
    addr: i128,
    literal_bit: Option<i64>,
    member: Option<&ast::Member>,
    ty: TypeNode,
) -> Result<AnonPortAccess, Diagnostic> {
    let addr = i64::try_from(addr).map_err(|_| {
        Diagnostic::error(
            loc,
            msg!(keys::SE_098_ADDRESS_TOO_WIDE, addr = addr),
        )
        .with_code("SE-098")
    })?;

    let bit = match (literal_bit, member) {
        (Some(literal), Some(_)) => {
            return Err(Diagnostic::error(
                loc,
                msg!(
                    keys::SE_098_BIT_GIVEN_TWICE,
                    literal = literal,
                    addr = format!("{addr:X}")
                ),
            )
            .with_code("SE-098"));
        }
        (_, Some(ast::Member::Number(n))) => *n,
        (_, Some(ast::Member::Identifier(id))) => {
            return Err(Diagnostic::error(
                loc,
                msg!(keys::SE_098_BIT_POSITION_IS_A_NAME, name = id.name),
            )
            .with_code("SE-098"));
        }
        (literal, None) => i128::from(literal.unwrap_or(0)),
    };

    let bit = i64::try_from(bit).unwrap_or(i64::MAX);
    let width = width_of(&ty).ok_or_else(|| {
        Diagnostic::error(
            loc,
            msg!(keys::SE_098_TYPE_WITHOUT_WIDTH, ty = ty),
        )
        .with_code("SE-098")
    })?;

    if !(0..=MAX_ANON_BIT).contains(&bit) {
        return Err(Diagnostic::error(
            loc,
            msg!(keys::SE_098_BIT_OUT_OF_RANGE, bit = bit, max = MAX_ANON_BIT),
        )
        .with_code("SE-098"));
    }
    if bit + i64::from(width) > i64::from(MAX_ANON_WIDTH) {
        return Err(Diagnostic::error(
            loc,
            msg!(
                keys::SE_098_FIELD_OUT_OF_WORD,
                width = width,
                bit = bit,
                max = MAX_ANON_WIDTH
            ),
        )
        .with_code("SE-098"));
    }

    Ok(AnonPortAccess { addr, bit, ty, loc })
}

/// `SE-097` - ширина доступа не задана.
///
/// Видна построителям выражений и условий: у них есть собственная ветвь по
/// узлу `AnonAddress`, которую воронка перехватывает раньше. Ветвь оставлена
/// **отвечающей**, а не `unreachable!`: если воронка когда-нибудь перестанет
/// ловить форму, автор получит диагностику, а инструмент - не панику.
pub(crate) fn width_missing(loc: Location, addr: i128, bit: Option<i64>) -> Diagnostic {
    let written = match bit {
        Some(bit) => format!("#0x{:X}:{}", addr as u64, bit),
        None => format!("#0x{:X}", addr as u64),
    };
    Diagnostic::error(
        loc,
        msg!(
            keys::SE_097_ACCESS_WITHOUT_WIDTH,
            written = written,
            addr = format!("{:X}", addr as u64)
        ),
    )
    .with_code("SE-097")
}

#[cfg(test)]
mod tests {
    use super::*;

    fn loc() -> Location {
        Location::Implicit
    }

    fn anon_expr(addr: i128, bit: Option<i64>) -> ast::Expression {
        ast::Expression::AnonAddress(loc(), addr, bit)
    }

    fn u8_type() -> ast::Type {
        ast::Type::Alias(ast::Identifier {
            loc: loc(),
            name: "u8".to_string(),
        })
    }

    /// `#0x100 as u8` - ширина берётся у приведения, бит по умолчанию нулевой.
    #[test]
    fn cast_gives_width() {
        let expr = ast::Expression::Cast(loc(), Box::new(anon_expr(0x100, None)), u8_type());
        let access = fold_expression(&expr).expect("свёртка").expect("тройка");
        assert_eq!(access.addr, 0x100);
        assert_eq!(access.bit, 0);
        assert_eq!(access.width_bits(), 8);
    }

    /// `#0x100.4` - один бит: ширина 1, позиция из точки.
    #[test]
    fn bit_access_gives_single_bit() {
        let expr = ast::Expression::BitAccess(
            loc(),
            Box::new(anon_expr(0x100, None)),
            ast::Member::Number(4),
        );
        let access = fold_expression(&expr).expect("свёртка").expect("тройка");
        assert_eq!((access.bit, access.width_bits()), (4, 1));
    }

    /// `#0x100` без ширины - `SE-097`, а не умолчание.
    #[test]
    fn bare_address_is_width_missing() {
        let diag = fold_expression(&anon_expr(0x100, None))
            .expect("свёртка")
            .expect_err("ошибка");
        assert_eq!(diag.code.as_deref(), Some("SE-097"));
    }

    /// `#0x100:3.4` - позиция бита задана дважды.
    #[test]
    fn bit_position_twice_is_error() {
        let expr = ast::Expression::BitAccess(
            loc(),
            Box::new(anon_expr(0x100, Some(3))),
            ast::Member::Number(4),
        );
        let diag = fold_expression(&expr)
            .expect("свёртка")
            .expect_err("ошибка");
        assert_eq!(diag.code.as_deref(), Some("SE-098"));
    }

    /// Поле, выходящее за слово доступа, отвергается.
    #[test]
    fn field_beyond_word_is_error() {
        let expr = ast::Expression::Cast(loc(), Box::new(anon_expr(0x100, Some(60))), u8_type());
        let diag = fold_expression(&expr)
            .expect("свёртка")
            .expect_err("ошибка");
        assert_eq!(diag.code.as_deref(), Some("SE-098"));
    }

    /// Скобки у операнда прозрачны: `(#0x100) as u8` - то же обращение.
    #[test]
    fn parenthesis_around_operand_is_transparent() {
        let inner = ast::Expression::Parenthesis(loc(), Box::new(anon_expr(0x100, None)));
        let expr = ast::Expression::Cast(loc(), Box::new(inner), u8_type());
        let access = fold_expression(&expr).expect("свёртка").expect("тройка");
        assert_eq!(access.addr, 0x100);
    }

    /// Имя синтетического порта пригодно для IEC: без `0x` и без ведущего `_`.
    #[test]
    fn synthetic_name_is_iec_friendly() {
        let access = AnonPortAccess {
            addr: 0x346619,
            bit: 4,
            ty: TypeNode::Bit,
            loc: Location::Codegen,
        };
        assert_eq!(access.synthetic_name(), "AT_346619_4_1");
    }
}
