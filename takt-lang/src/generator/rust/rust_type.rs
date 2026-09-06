//! Отображение типов Takt в типы Rust.
//!
//! ## Разрядность `#[repr]` - по диапазону, а не `u8`
//!
//! Тот же урок ST-генератор получил постфактум (`CLAUDE.md`: "Разрядность перечисления
//! считается по диапазону вариантов, а не берётся `USINT`, как предполагал "); здесь он
//! учтён **до** написания кода.

use crate::diagnostics::{Diagnostic, Location};
use crate::generator::rust::rust_name::rust_type_name;
use crate::semantic::enum_facts;
use crate::semantic::type_node::TypeNode;

/// Строит диагностику `RS-014` - тип не представим в Rust.
fn rs014(what: &str, ty: &TypeNode) -> Diagnostic {
    Diagnostic::error(
        Location::Codegen,
        format!("{}: тип '{}' не представим в Rust", what, ty),
    )
    .with_code("RS-014")
}

/// Отображает тип Takt в тип Rust.
///
/// `what` - что именно объявляется (`переменная 'x'`, `параметр 'p'`), чтобы
/// диагностика указывала на место, а не на абстрактный тип.
///
/// # Ошибки
/// [`RS-014`], если тип не отображается. Тихого пропуска нет **намеренно**: в
/// цели `c` неиспользуемая переменная просто исчезает из структуры
/// (`c_header.rs:344`), и отличить "отфильтровали" от "не смогли перевести"
/// снаружи невозможно (наследие ).
pub(crate) fn rust_type(ty: &TypeNode, what: &str) -> Result<String, Diagnostic> {
    match ty {
        // Точное соответствие. В цели `c` - `int` (дефект 0029, Д2). Тип `duration`:
        // целое без знака в **миллисекундах** - та же единица, что у приведения `as` и
        // у профиля "часы", поэтому граница "длительность ↔ число" не порождает
        // арифметики.
        TypeNode::Duration => Ok(format!("u{}", crate::semantic::duration::VALUE_BITS)),
        TypeNode::Bit => Ok("bool".to_string()),
        TypeNode::Bool => Ok("bool".to_string()),
        // f64, а не f32: симулятор считает в f64 (`eval::Value::Real`), и только при
        // совпадении точности сверка потактовых трасс имеет смысл. Цель `c` по
        // умолчанию даёт `double`, но `--float-width=32` может дать `float`; для цели
        // `rust` ширина не настраивается (см.
        TypeNode::Rational => Ok("f64".to_string()),
        // Fixed-point q(m, n): знаковое целое, вмещающее W = m + n бит, округлённое
        // вверх до i8/i16/i32/i64 (машинных ширин Rust; `>>` знакового в Rust определён
        // как арифметический). Масштабирование при `*`/`/` -.
        TypeNode::Fixed { m, n, .. } => Ok(format!(
            "i{}",
            crate::semantic::type_node::type_fixed::fixed_storage_bits(m + n)
        )),
        TypeNode::Integer { bits, signed } => {
            let prefix = if *signed { "i" } else { "u" };
            match bits {
                8 | 16 | 32 | 64 => Ok(format!("{}{}", prefix, bits)),
                // Разрядностей, кроме 8/16/32/64, конструктор типов не строит; ветка -
                // тест на случай расширения `TypeNode::Integer`.
                other => Err(rs014(
                    what,
                    &TypeNode::Integer {
                        bits: *other,
                        signed: *signed,
                    },
                )),
            }
        }
        // Бит-вектор `[bit;N]`: упакованный скаляр `u{round_up(N)}` (N <= 64) либо
        // массив слов `[u64; ⌈N/64⌉]` (N > 64) - как в цели C. Так `[bit;8]` и `u8`
        // дают один тип.
        TypeNode::Array(n, elem) => {
            if let Some(nbits) = crate::semantic::bit_vector::is_bit_vector(ty) {
                use crate::semantic::bit_vector::{self, BitVectorLayout};
                return Ok(match bit_vector::layout(nbits) {
                    BitVectorLayout::Scalar { width } => format!("u{}", width),
                    BitVectorLayout::Words { count } => {
                        format!("[u{}; {}]", bit_vector::WORD_BITS, count)
                    }
                });
            }
            // Настоящий массив скаляров. Вложенность бесплатна - в отличие от ST, где
            // `ARRAY OF ARRAY` отвергается MatIEC.
            Ok(format!("[{}; {}]", rust_type(elem, what)?, n))
        }
        TypeNode::Enum(name) => rust_type_name(name, Location::Codegen),
        TypeNode::Struct(name) => rust_type_name(name, Location::Codegen),
        TypeNode::Unit => Ok("()".to_string()),
        // Ниже - то, что представления не имеет. Ветки `_` нет намеренно: `TypeNode`
        // помечен `#[non_exhaustive]`, но внутри крейта-объявителя атрибут не
        // действует, поэтому исчерпывающий разбор здесь возможен - и обязателен.
        // Добавление варианта в `TypeNode` обязано валить сборку, а не тихо
        // проваливаться в чужую ветку.
        TypeNode::Address(_, _)
        | TypeNode::Inference
        | TypeNode::Unsupported
        | TypeNode::BuiltinString
        | TypeNode::BuiltinModel
        | TypeNode::BuiltinState
        | TypeNode::BuiltinNumeric => Err(rs014(what, ty)),
    }
}

/// Подбирает разрядность `#[repr]` перечисления по **диапазону его вариантов**.
///
/// Значения вариантов - `i64` (`EnumDefinitionNode::variants`), поэтому знаковость
/// определяется наличием отрицательных значений, а ширина - наибольшим по модулю
/// вариантом.
pub(crate) fn enum_repr(variants: &[(String, i128)]) -> &'static str {
    // Знак и ширина - из общего факта: цель лишь отображает его в имя repr. Свой каскад
    // извлечения диапазона удалён.
    match enum_facts(variants) {
        Some(f) => match (f.signed, f.machine_bits()) {
            (true, 8) => "i8",
            (true, 16) => "i16",
            (true, 32) => "i32",
            (true, _) => "i64",
            (false, 8) => "u8",
            (false, 16) => "u16",
            (false, 32) => "u32",
            (false, _) => "u64",
        },
        // Пустое перечисление - поведение сохраняется сегодняшним (`u8`).
        None => "u8",
    }
}

/// Отвергает `--float-width=32` для цели `rust`.
///
/// Молчаливое игнорирование флага недопустимо: пользователь решил бы, что получил
/// `f32`, тогда как `Rational` -> `f64` - решение, от которого зависит сверка с
/// симулятором. Лучше отказать, чем соврать.
pub(crate) fn reject_float_width(width: crate::generator::FloatWidth) -> Result<(), Diagnostic> {
    if width == crate::generator::FloatWidth::W32 {
        return Err(Diagnostic::error(
            Location::Codegen,
            "--float-width=32 несовместим с целью 'rust': вещественный тип Takt \
             отображается в f64 — так же, как считает симулятор (решение ADR 0050). \
             Уберите флаг либо используйте --float-width=64"
                .to_string(),
        )
        .with_code("RS-015"));
    }
    Ok(())
}

/// Совпадают ли тип операнда и цель приведения после отображения в Rust.
///
/// Сравниваются **напечатанные** типы, а не типы Takt: `duration` отображается в `u32`,
/// и `d as u32` даёт `self.d as u32` - это `clippy::unnecessary_cast`, то есть отказ
/// проверки цели при нулевом коде возврата `taktc`. Типы Takt здесь различны, и признак
/// 0361 такую запись не ловил.
///
/// Тип операнда берётся у **именованного значения**: у литерала и выражения он
/// печатнику неизвестен, и опускать приведение там было бы догадкой.
pub(crate) fn same_printed_type(inner: &crate::semantic::ExpressionNode, ty: &TypeNode) -> bool {
    let Some(from) = crate::generator::mixed_sign::operand_type_expr(inner) else {
        return false;
    };
    match (
        rust_type(&from, "приведение типа"),
        rust_type(ty, "приведение типа"),
    ) {
        (Ok(from), Ok(to)) => from == to,
        // Неотобразимый тип судит печать самого приведения - здесь молчим.
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// **Тест против повторения дефекта 0029** (в C - `int`).
    #[test]
    fn bit_maps_to_bool() {
        assert_eq!(rust_type(&TypeNode::Bit, "тест").unwrap(), "bool");
    }

    /// `bool` -> `bool`.
    #[test]
    fn bool_maps_to_bool() {
        assert_eq!(rust_type(&TypeNode::Bool, "тест").unwrap(), "bool");
    }

    /// `Rational` -> `f64`.
    ///
    /// **Тест против повторения расхождения с симулятором**: цель `c` даёт
    /// `float` (f32), из-за чего сверка вещественных недостижима.
    #[test]
    fn rational_maps_to_f64_not_f32() {
        let ty = rust_type(&TypeNode::Rational, "тест").unwrap();
        assert_eq!(ty, "f64");
        assert_ne!(ty, "f32", "f32 разошёлся бы с f64 симулятора");
    }

    /// Целые отображаются один в один.
    #[test]
    fn integers_map_natively() {
        let cases = [
            (8, false, "u8"),
            (16, false, "u16"),
            (32, false, "u32"),
            (64, false, "u64"),
            (8, true, "i8"),
            (16, true, "i16"),
            (32, true, "i32"),
            (64, true, "i64"),
        ];
        for (bits, signed, expected) in cases {
            assert_eq!(
                rust_type(&TypeNode::Integer { bits, signed }, "тест").unwrap(),
                expected
            );
        }
    }

    /// `[u8; 4]` -> `[u8; 4]`, а **не** `uint4_t`.
    ///
    /// Тест против дефекта 0029 (Д1): в C `Array(size, elem)` даёт `uint{size}_t`,
    /// где `size` - число элементов, то есть несуществующий тип.
    #[test]
    fn array_maps_natively_not_to_uint4_t() {
        let ty = TypeNode::Array(
            4,
            Box::new(TypeNode::Integer {
                bits: 8,
                signed: false,
            }),
        );
        let out = rust_type(&ty, "тест").unwrap();
        assert_eq!(out, "[u8; 4]");
        assert!(!out.contains("uint4_t"), "повторён дефект 0029: {}", out);
    }

    /// Вложенный массив отображается нативно; внутренний бит-вектор упаковывается.
    ///
    /// `[[bit;4];2]` - массив из 2 бит-векторов `[bit;4]`, каждый упакован в `u8`,
    /// поэтому внешний -> `[u8; 2]`.
    #[test]
    fn nested_array_maps_natively() {
        let inner = TypeNode::Array(4, Box::new(TypeNode::Bit));
        let outer = TypeNode::Array(2, Box::new(inner));
        assert_eq!(rust_type(&outer, "тест").unwrap(), "[u8; 2]");
    }

    /// Перечисление отображается в тип с именем в CamelCase.
    #[test]
    fn enum_maps_to_camelcase_type() {
        assert_eq!(
            rust_type(&TypeNode::Enum("action".to_string()), "тест").unwrap(),
            "Action"
        );
    }

    /// **Контрпример:** непереводимый тип даёт диагностику, а не тихий пропуск.
    #[test]
    fn unrepresentable_type_is_rs014() {
        for ty in [
            TypeNode::Inference,
            TypeNode::Unsupported,
            TypeNode::BuiltinString,
            TypeNode::Address(0x10, None),
        ] {
            let err = rust_type(&ty, "переменная 'x'").unwrap_err();
            assert_eq!(err.code.as_deref(), Some("RS-014"), "тип {:?}", ty);
            assert!(
                err.message.contains("переменная 'x'"),
                "диагностика должна указывать на место: {}",
                err.message
            );
        }
    }

    /// Перечисление, влезающее в байт, получает `u8`.
    #[test]
    fn enum_repr_small_is_u8() {
        let variants = vec![("Idle".to_string(), 0), ("Run".to_string(), 255)];
        assert_eq!(enum_repr(&variants), "u8");
    }

    /// **Ключевой тест A6:** вариант `Idle = 670` даёт `u16`, а не `u8`.
    ///
    /// Реальный случай корпуса - `elevator.takt:121`.
    #[test]
    fn enum_repr_670_is_u16_not_u8() {
        let variants = vec![("Idle".to_string(), 670), ("Up".to_string(), 671)];
        assert_eq!(
            enum_repr(&variants),
            "u16",
            "разрядность обязана считаться по диапазону вариантов"
        );
    }

    /// Границы разрядности: 255 -> u8, 256 -> u16, 65535 -> u16, 65536 -> u32.
    #[test]
    fn enum_repr_boundaries() {
        let at = |v: i128| enum_repr(&[("V".to_string(), v)]);
        assert_eq!(at(255), "u8");
        assert_eq!(at(256), "u16");
        assert_eq!(at(65535), "u16");
        assert_eq!(at(65536), "u32");
        assert_eq!(at(i128::from(u32::MAX)), "u32");
        assert_eq!(at(i128::from(u32::MAX) + 1), "u64");
    }

    /// Отрицательный вариант даёт знаковый `repr`.
    #[test]
    fn enum_repr_negative_is_signed() {
        assert_eq!(enum_repr(&[("V".to_string(), -1)]), "i8");
        assert_eq!(enum_repr(&[("V".to_string(), -200)]), "i16");
        assert_eq!(
            enum_repr(&[("A".to_string(), -1), ("B".to_string(), 40000)]),
            "i32"
        );
    }

    /// **Контрпример:** `--float-width=32` для цели `rust` - ошибка, а не молчание.
    #[test]
    fn float_width_32_is_rejected() {
        let err = reject_float_width(crate::generator::FloatWidth::W32).unwrap_err();
        assert_eq!(err.code.as_deref(), Some("RS-015"));
        assert!(reject_float_width(crate::generator::FloatWidth::W64).is_ok());
    }
}
