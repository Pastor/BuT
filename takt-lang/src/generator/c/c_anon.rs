//! Анонимное обращение к ячейке в цели `c-hal` - `#0x346619:0 as u64`.
//!
//! ## Слово доступа - минимальное, содержащее поле
//!
//! Правило то же, что у бит-порта (`word_bytes_for_bit`, / ): читать байт и сдвигать на
//! `bit` нельзя - при бите >= 8 это чтение не того разряда, при бите >= 32 - UB (`>>`
//! не меньше ширины). Здесь слово выбирается по **старшему** занятому разряду `bit +
//! width`.
//!
//! ## Запись - чтение-изменение-запись, кроме случая целого слова
//!
//! Поле уже слова (`#0x100:3 as u8`) записывается RMW: иначе запись затёрла бы соседние
//! разряды регистра. Когда поле занимает слово целиком, RMW не нужен - и не печатается:
//! лишнее чтение MMIO имеет побочный эффект (регистры, очищаемые чтением).

use crate::diagnostics::lang::keys;
use crate::diagnostics::{Diagnostic, Location};
use crate::msg;
use crate::semantic::AnonPortAccess;
use crate::semantic::type_node::TypeNode;

/// Разрядность слова доступа (в байтах) по старшему занятому разряду.
fn word_bytes(top_bit: u16) -> u16 {
    match top_bit {
        b if b <= 8 => 1,
        b if b <= 16 => 2,
        b if b <= 32 => 4,
        _ => 8,
    }
}

/// Беззнаковый C-тип слова доступа.
fn word_type(access: &AnonPortAccess) -> String {
    let top = u16::try_from(access.bit).unwrap_or(0) + access.width_bits();
    format!("uint{}_t", word_bytes(top) * 8)
}

/// C-тип **значения** поля: им приводится результат чтения.
///
/// У знакового поля это существенно: `(int8_t)` восстанавливает знак, тогда как
/// беззнаковое чтение дало бы 200 вместо −56.
fn value_type(access: &AnonPortAccess) -> String {
    match &access.ty {
        TypeNode::Bit | TypeNode::Bool => "bool".to_string(),
        TypeNode::Integer { bits, signed: true } => format!("int{bits}_t"),
        TypeNode::Integer {
            bits,
            signed: false,
        } => format!("uint{bits}_t"),
        // `q(m, n)` хранится целым той же разрядности.
        TypeNode::Fixed { .. } => format!("int{}_t", access.width_bits()),
        // Прочие типы отсеяны свёрткой (`SE-098`): ширины доступа у них нет.
        other => format!("/* {other} */ uint64_t"),
    }
}

/// Разыменование слова по адресу: `(*(volatile uintN_t*)0xADDRu)`.
fn word_deref(access: &AnonPortAccess) -> String {
    format!(
        "(*(volatile {ty}*)(uintptr_t)0x{addr:X}u)",
        ty = word_type(access),
        addr = access.addr as u64
    )
}

/// Маска поля в слове доступа (без сдвига).
fn mask(access: &AnonPortAccess) -> String {
    let width = access.width_bits();
    let ty = word_type(access);
    if u32::from(width) >= u32::from(word_bytes(width) * 8) && access.bit == 0 {
        // Поле занимает слово целиком - маска тождественна.
        return String::new();
    }
    format!("(({ty})((({ty})1u << {width}) - 1u))")
}

/// Занимает ли поле слово доступа целиком (тогда ни сдвига, ни маски не нужно).
fn is_whole_word(access: &AnonPortAccess) -> bool {
    let top = u16::try_from(access.bit).unwrap_or(0) + access.width_bits();
    access.bit == 0 && access.width_bits() == word_bytes(top) * 8
}

/// Чтение поля: выражение языка C.
pub(super) fn read(access: &AnonPortAccess) -> String {
    let value = value_type(access);
    if is_whole_word(access) {
        return format!("(({value}){deref})", deref = word_deref(access));
    }
    let shifted = if access.bit == 0 {
        word_deref(access)
    } else {
        format!(
            "({deref} >> {bit})",
            deref = word_deref(access),
            bit = access.bit
        )
    };
    format!("(({value})({shifted} & {mask}))", mask = mask(access))
}

/// Запись значения в поле: выражение-присваивание языка C.
///
/// `rhs` - уже напечатанная правая часть.
pub(super) fn write(access: &AnonPortAccess, rhs: &str) -> String {
    let word = word_type(access);
    let deref = word_deref(access);
    if is_whole_word(access) {
        return format!("{deref} = ({word})({rhs})");
    }
    let mask = mask(access);
    let bit = access.bit;
    format!(
        "{deref} = ({word})(({deref} & ({word})~(({word}){mask} << {bit})) \
         | ((({word})({rhs}) & {mask}) << {bit}))"
    )
}

/// Отказ цели `c`: адресов она не знает по устройству.
pub(super) fn refuse_plain_c() -> Diagnostic {
    Diagnostic::error(
        crate::generator::site::at(Location::Codegen),
        msg!(keys::CC_021_ADDRESS_CELL),
    )
    .with_code("CC-021")
}

#[cfg(test)]
mod tests {
    use super::*;

    fn access(addr: i64, bit: i64, ty: TypeNode) -> AnonPortAccess {
        AnonPortAccess {
            addr,
            bit,
            ty,
            loc: crate::diagnostics::Location::Codegen,
        }
    }

    /// Слово целиком: ни сдвига, ни маски - лишнее чтение MMIO не печатается.
    #[test]
    fn whole_word_read_is_plain_deref() {
        let text = read(&access(
            0x346619,
            0,
            TypeNode::Integer {
                bits: 32,
                signed: false,
            },
        ));
        assert_eq!(
            text, "((uint32_t)(*(volatile uint32_t*)(uintptr_t)0x346619u))",
            "получено: {text}"
        );
    }

    /// Один бит: сдвиг и маска, слово - минимальное, содержащее бит.
    #[test]
    fn single_bit_read_uses_containing_word() {
        let text = read(&access(0x100, 33, TypeNode::Bit));
        assert!(
            text.contains("volatile uint64_t") && text.contains(">> 33"),
            "бит 33 обязан читаться 64-разрядным словом: {text}"
        );
    }

    /// Знаковое поле приводится к знаковому типу - иначе −56 читается как 200.
    #[test]
    fn signed_field_keeps_sign() {
        let text = read(&access(
            0x100,
            8,
            TypeNode::Integer {
                bits: 8,
                signed: true,
            },
        ));
        assert!(text.contains("(int8_t)"), "получено: {text}");
    }

    /// Запись в поле уже слова - чтение-изменение-запись.
    #[test]
    fn partial_write_is_read_modify_write() {
        let text = write(
            &access(
                0x100,
                3,
                TypeNode::Integer {
                    bits: 8,
                    signed: false,
                },
            ),
            "value",
        );
        assert!(
            text.contains('&') && text.contains('|') && text.contains("<< 3"),
            "получено: {text}"
        );
    }

    /// Запись в целое слово - без чтения: оно имеет побочный эффект.
    #[test]
    fn whole_word_write_has_no_read() {
        let text = write(
            &access(
                0x100,
                0,
                TypeNode::Integer {
                    bits: 16,
                    signed: false,
                },
            ),
            "value",
        );
        assert_eq!(
            text.matches("volatile").count(),
            1,
            "лишнее чтение MMIO: {text}"
        );
    }
}
