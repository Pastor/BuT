//! Порты Takt -> HAL-трейт Rust.
//!
//! ## Чем это отличается от цели `c`
//!
//! Цель `c` описывает порты парой указателей на функции плюс `void *userdata`
//! (`examples/generated/c/elevator_mini.h`):
//!
//! ```c
//! void  (*write_bit)(ElevatorMini_Out_BitPort port, bool val, void *userdata);
//! bool  (*read_bit )(ElevatorMini_In_BitPort port, void *userdata);
//! void  *userdata;
//! ```
//!
//! Плата: тип `userdata` стёрт - за корректность приведения отвечает пользователь;
//! забыть проставить указатель = вызов по нулевому адресу.
//!
//! Здесь то же самое выражено трейтом:
//!
//! ```rust,ignore
//! pub trait Hal {
//!     fn read_bit(&mut self, port: InBitPort) -> bool;
//!     fn write_bit(&mut self, port: OutBitPort, value: bool);
//! }
//! ```
//!
//! `userdata` **исчезает как понятие**: состояние HAL живёт в самом `H`, типобезопасно,
//! а конструктор требует `hal` - "забыть колбэк" невозможно by construction.
//!
//! Карта адресов здесь не потребляется: это аналог режима `c`, а не `c-hal`.

use crate::diagnostics::lang::keys;
use crate::diagnostics::{Diagnostic, Location};
use crate::generator::rust::rust_type::rust_type;
use crate::msg;
use crate::semantic::bit_vector::{self, BitVectorLayout};
use crate::semantic::type_node::TypeNode;

/// Категория порта - задаёт имя перечисления и пару методов трейта.
///
/// Категория выводится из типа порта, поэтому состав трейта определяется
/// **фактическим набором портов модели**: модель с одними битовыми портами
/// получает трейт из двух методов, а не "на все случаи жизни".
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct PortClass {
    /// Суффикс имён (`Bit` -> `InBitPort`/`OutBitPort`, `read_bit`/`write_bit`).
    tag: String,
    /// Тип значения порта в Rust (`bool`, `u8`, `f64`).
    value_type: String,
}

impl PortClass {
    /// Имя перечисления входных портов этой категории.
    pub(crate) fn in_enum(&self) -> String {
        format!("In{}Port", self.tag)
    }

    /// Имя перечисления выходных портов этой категории.
    pub(crate) fn out_enum(&self) -> String {
        format!("Out{}Port", self.tag)
    }

    /// Имя метода чтения (`read_bit`).
    pub(crate) fn read_fn(&self) -> String {
        format!("read_{}", self.tag.to_lowercase())
    }

    /// Имя метода записи (`write_bit`).
    pub(crate) fn write_fn(&self) -> String {
        format!("write_{}", self.tag.to_lowercase())
    }

    /// Тип значения порта в Rust.
    pub(crate) fn value_type(&self) -> &str {
        &self.value_type
    }
}

/// Определяет категорию порта по его типу.
///
/// # Ошибки
/// [`RS-016`], если тип порта не ложится на метод трейта. Составные типы
/// (массив, перечисление, структура) портами быть не могут: HAL - это граница с
/// железом, где значение либо бит, либо число. Диагностика, а не тихий пропуск.
pub(crate) fn port_class(
    ty: &TypeNode,
    port: &str,
    loc: Location,
    model: &crate::semantic::ModelNode,
) -> Result<PortClass, Diagnostic> {
    // Бит-вектор `[bit;N]`, N <= 64 - упакованное беззнаковое число ширины
    // `round_up(N)`: нормализуем к `Integer`, дальше как обычное порт-число (`[bit;8]`
    // == `u8`). N > 64 (массив слов) остаётся `Array` -> `RS-016` ниже: HAL-числом
    // слова быть не могут. Скалярный тип, у которого есть машинное представление целым,
    // приходит в HAL целым: перечисление, длительность и `q(m, n)` - значения на
    // границе, а не составные типы. Ширину и знак даёт один носитель
    // `semantic::scalar_port` - тот же, которым размещает порт цель `st-at`.
    //
    // `bit`/`bool` носитель намеренно не считает целым: у них своя категория порта
    // (`write_bit`), и подмена сломала бы её.
    let from_scalar;
    let ty = match crate::semantic::scalar_port::scalar_repr(ty, model) {
        Some(repr) if !matches!(ty, TypeNode::Integer { .. }) => {
            from_scalar = TypeNode::Integer {
                bits: repr.bits,
                signed: repr.signed,
            };
            &from_scalar
        }
        _ => ty,
    };
    let normalized;
    let ty = match bit_vector::is_bit_vector(ty).map(bit_vector::layout) {
        Some(BitVectorLayout::Scalar { width }) => {
            normalized = TypeNode::Integer {
                bits: u8::try_from(width).unwrap_or(64),
                signed: false,
            };
            &normalized
        }
        _ => ty,
    };
    match ty {
        // `bit` и `bool` - одна категория: оба дают `bool`.
        TypeNode::Bit | TypeNode::Bool => Ok(PortClass {
            tag: "Bit".to_string(),
            value_type: "bool".to_string(),
        }),
        TypeNode::Rational => Ok(PortClass {
            tag: "F64".to_string(),
            value_type: "f64".to_string(),
        }),
        TypeNode::Integer { bits, signed } => {
            let value_type = rust_type(ty, &msg!(keys::RS_WHAT_PORT, name = port))?;
            Ok(PortClass {
                tag: format!("{}{}", if *signed { "I" } else { "U" }, bits),
                value_type,
            })
        }
        other => Err(Diagnostic::error(
            loc,
            msg!(keys::RS_016_PORT_TYPE_NOT_HAL, name = port, ty = other),
        )
        .with_code("RS-016")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Пустая модель: перечислений в ней нет - их разбирает свой набор.
    fn model() -> crate::semantic::ModelNode {
        crate::semantic::ModelNode::default()
    }

    fn loc() -> Location {
        Location::Codegen
    }

    /// Битовый порт даёт категорию `Bit`: `read_bit`/`write_bit`, значение `bool`.
    #[test]
    fn bit_port_gives_bit_class() {
        let class = port_class(&TypeNode::Bit, "P", loc(), &model()).unwrap();
        assert_eq!(class.in_enum(), "InBitPort");
        assert_eq!(class.out_enum(), "OutBitPort");
        assert_eq!(class.read_fn(), "read_bit");
        assert_eq!(class.write_fn(), "write_bit");
        assert_eq!(class.value_type(), "bool");
    }

    /// `bool`-порт неотличим от `bit`-порта: обе категории дают `bool`.
    #[test]
    fn bool_port_shares_bit_class() {
        let a = port_class(&TypeNode::Bit, "P", loc(), &model()).unwrap();
        let b = port_class(&TypeNode::Bool, "P", loc(), &model()).unwrap();
        assert_eq!(a, b);
    }

    /// Числовой порт даёт категорию по своему типу.
    #[test]
    fn integer_port_gives_typed_class() {
        let class = port_class(
            &TypeNode::Integer {
                bits: 8,
                signed: false,
            },
            "P",
            loc(),
            &model(),
        )
        .unwrap();
        assert_eq!(class.in_enum(), "InU8Port");
        assert_eq!(class.read_fn(), "read_u8");
        assert_eq!(class.value_type(), "u8");
    }

    /// Вещественный порт даёт `f64` - как в симуляторе.
    #[test]
    fn rational_port_gives_f64_class() {
        let class = port_class(&TypeNode::Rational, "P", loc(), &model()).unwrap();
        assert_eq!(class.read_fn(), "read_f64");
        assert_eq!(class.value_type(), "f64");
    }

    /// Бит-вектор `[bit;N]`, N <= 64 - порт-**число** `u{round_up(N)}` (`[bit;12]` ->
    /// `u16`), а не `RS-016`: упаковка делает его скаляром.
    #[test]
    fn bit_vector_port_is_packed_number() {
        let class = port_class(
            &TypeNode::Array(12, Box::new(TypeNode::Bit)),
            "R",
            loc(),
            &model(),
        )
        .unwrap();
        assert_eq!(class.value_type(), "u16");
        let byte = port_class(
            &TypeNode::Array(8, Box::new(TypeNode::Bit)),
            "R",
            loc(),
            &model(),
        )
        .unwrap();
        assert_eq!(byte.value_type(), "u8");
    }

    /// **Контрпример:** тип порта, не сводимый к HAL-числу, даёт `RS-016`, а не
    /// тихий пропуск. Настоящий массив скаляров (`[u8;4]`) и бит-вектор из слов
    /// (`[bit;128]` -> `[u64;2]`) HAL-числом быть не могут.
    #[test]
    fn composite_port_type_is_rs016() {
        let real_array = TypeNode::Array(
            4,
            Box::new(TypeNode::Integer {
                bits: 8,
                signed: false,
            }),
        );
        let err = port_class(&real_array, "DATA", loc(), &model()).unwrap_err();
        assert_eq!(err.code.as_deref(), Some("RS-016"));
        assert!(
            err.message.contains("DATA"),
            "называет порт: {}",
            err.message
        );

        // Бит-вектор N > 64 - массив слов, не HAL-число.
        let words = TypeNode::Array(128, Box::new(TypeNode::Bit));
        assert_eq!(
            port_class(&words, "W", loc(), &model())
                .unwrap_err()
                .code
                .as_deref(),
            Some("RS-016")
        );
    }
}
