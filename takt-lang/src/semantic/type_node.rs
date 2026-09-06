//! Построение семантических узлов типов языка Takt.
//!
//! Основная функция [`construct_type`] преобразует АСД-тип [`Type`] в семантический
//! [`TypeNode`].
//!
//! Вместо `&BTreeMap<String, TypeNode>` функция принимает `Rc<RefCell<ModelNode>>`, что
//! позволяет искать типы через цепочку родительских моделей.
//!
//! ## Поддерживаемые типы
//!
//! | АСД (`ast::Type`) | Семантический узел |
//! |-------------------------------|--------------------------------|
//! | `Type::Bit` | `TypeNode::Bit` |
//! | `Type::Bool` | `TypeNode::Bool` |
//! | `Type::Rational` | `TypeNode::Rational` |
//! | `Type::Unit` | `TypeNode::Unit` |
//! | `Type::Array { N, T }` | `TypeNode::Array(N, T)` |
//! | `Type::Address { addr, bit }` | `TypeNode::Address(addr, bit)` |
//! | `Type::Enum("Color")` | `TypeNode::Enum("Color")` |
//! | `Type::Alias("bit")` | `TypeNode::Bit` |
//! | `Type::Alias("bool")` | `TypeNode::Bool` |
//! | `Type::Alias("float")` | `TypeNode::Rational` |
//! | `Type::Alias("unit")` | `TypeNode::Unit` |
//! | `Type::Alias(local)` | значение из таблицы типов модели |
//! | `Type::Function { .. }` | `TypeNode::Unsupported` |
//! | `None` | `TypeNode::Inference` |
//!
//! **Примечание о `Type::Enum`:** `construct_type` не проверяет, объявлено ли
//! перечисление; эта проверка выполняется в `validate_model` через
//! `validate_enum_type_declarations`. Данное разделение позволяет обрабатывать
//! взаимные ссылки между перечислениями и переменными.

pub(crate) mod fixed_body;
pub mod type_fixed;

use crate::diagnostics::Diagnostic;
use crate::diagnostics::lang::keys;
use crate::msg;
use crate::parser::ast::Type;
use crate::semantic::ModelNode;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;
use type_fixed::construct_fixed;

/// Встроенный тип по его имени в исходнике (`u8`, `bit`, `duration`, ...).
///
/// **Единственное место**, где имя встроенного типа превращается в
/// [`TypeNode`]. Заведено исправлением: знание было продублировано -
/// [`construct_type`] знало целочисленные псевдонимы (`u8`...`i64`), а
/// context-free [`ast_type_to_node`](crate::semantic::type_inference::ast_type_to_node)
/// (через который идёт **приведение** `x as T`) знало только
/// `bit`/`bool`/`float`/`unit`. Из-за расхождения `5 as u8` давало
/// `TypeNode::Unsupported`, и симулятор падал с `SIM-007` - на совершенно
/// законном коде.
///
/// `None` - имя не встроенное (пользовательский псевдоним либо опечатка); решение, что
/// с этим делать, принимает вызывающий.
pub fn builtin_type_by_name(name: &str) -> Option<TypeNode> {
    let integer = |bits, signed| Some(TypeNode::Integer { bits, signed });
    match name {
        "bit" => Some(TypeNode::Bit),
        "bool" => Some(TypeNode::Bool),
        "float" => Some(TypeNode::Rational),
        "unit" => Some(TypeNode::Unit),
        // Длительность: грамматика отдаёт примитивы псевдонимом.
        "duration" => Some(TypeNode::Duration),
        "u8" => integer(8, false),
        "u16" => integer(16, false),
        "u32" => integer(32, false),
        "u64" => integer(64, false),
        "i8" => integer(8, true),
        "i16" => integer(16, true),
        "i32" => integer(32, true),
        "i64" => integer(64, true),
        _ => None,
    }
}

/// Строит [`TypeNode`] из опционального АСД-типа [`Type`].
///
/// Если тип не задан (`None`), возвращает [`TypeNode::Inference`] - заглушку для
/// последующего вывода типа.
///
/// # Ошибки
///
/// Возвращает [`Diagnostic`], если тип является псевдонимом, которого нет в таблице
/// типов `map`.
pub(crate) fn construct_type(
    typ: Option<Type>,
    model: Rc<RefCell<ModelNode>>,
) -> Result<TypeNode, Diagnostic> {
    if typ.is_none() {
        return Ok(TypeNode::Inference);
    }
    match typ.unwrap() {
        Type::Address { address, bit } => Ok(TypeNode::Address(address, bit)),
        Type::Bit => Ok(TypeNode::Bit),
        Type::Bool => Ok(TypeNode::Bool),
        Type::Rational => Ok(TypeNode::Rational),
        // Вариант `Type::Duration` грамматикой не порождается - как и
        // `Bit`/`Bool`/`Rational`: примитивные типы приходят из грамматики псевдонимом
        // (`Type::Alias`) и связываются по имени ниже. Ветка оставлена для полноты
        // разбора узла.
        Type::Duration => Ok(TypeNode::Duration),
        Type::Fixed(loc, ctor, m, n, modifier) => {
            construct_fixed(loc, &ctor, m, n, modifier.as_deref())
        }
        Type::Alias(def) => {
            // Пользовательский псевдоним в таблице типов модели берёт приоритет над
            // встроенными именами (u8, i32 и пр.), что позволяет переопределять
            // встроенные типы на уровне модели для обратной совместимости.
            if let Some(rc) = model.borrow().search_type(&def.name) {
                return Ok(rc.borrow().clone());
            }
            // Встроенные имена - через единый разбор: второй экземпляр этого знания уже
            // разъезжался с первым.
            builtin_type_by_name(&def.name).ok_or_else(|| {
                Diagnostic::declaration_error(
                    def.loc,
                    msg!(keys::SE_034_LOCAL_TYPE_NOT_FOUND, name = def.name),
                )
                .with_code("SE-034")
            })
        }
        Type::Array {
            element_type,
            element_count,
            ..
        } => Ok(TypeNode::Array(
            element_count,
            Box::new(construct_type(Some(*element_type), model)?),
        )),
        Type::Function { .. } => Ok(TypeNode::Unsupported),
        Type::Unit => Ok(TypeNode::Unit),
        // Type::Enum используется только в парсере как узел грамматики; в качестве типа
        // переменной не поддерживается на уровне семантики.
        Type::Enum(name) => Ok(TypeNode::Enum(name.clone())),
        // Type::Struct - ссылка на объявленный структурный тип.
        Type::Struct(name) => Ok(TypeNode::Struct(name.clone())),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    // Функции fixed-point живут в подмодуле `type_fixed` (рефакторинг 0170): тесты
    // зовут их оттуда, а не через `super::*`.
    use super::type_fixed::{fixed_repr_range, fixed_storage_bits, lower_fixed_literal};
    use crate::diagnostics::Location;
    use crate::parser::ast::{Identifier, Type};
    use crate::semantic::ExpressionNode;
    use crate::semantic::ModelNode;
    use std::cell::RefCell;
    use std::collections::BTreeMap;
    use std::rc::Rc;

    /// Вспомогательная функция: создаёт пустую модель.
    fn empty_model() -> Rc<RefCell<ModelNode>> {
        Rc::new(RefCell::new(ModelNode::default()))
    }

    // -- Примитивные типы ------------------------------------------------------

    /// `None` -> `TypeNode::Inference`.
    #[test]
    fn none_gives_inference() {
        assert_eq!(
            construct_type(None, empty_model()).unwrap(),
            TypeNode::Inference
        );
    }

    /// `Type::Bit` -> `TypeNode::Bit`.
    #[test]
    fn bit_gives_bit() {
        assert_eq!(
            construct_type(Some(Type::Bit), empty_model()).unwrap(),
            TypeNode::Bit
        );
    }

    /// `Type::Bool` -> `TypeNode::Bool`.
    #[test]
    fn bool_gives_bool() {
        assert_eq!(
            construct_type(Some(Type::Bool), empty_model()).unwrap(),
            TypeNode::Bool
        );
    }

    /// `Type::Rational` -> `TypeNode::Rational`.
    #[test]
    fn rational_gives_rational() {
        assert_eq!(
            construct_type(Some(Type::Rational), empty_model()).unwrap(),
            TypeNode::Rational
        );
    }

    /// `Type::Unit` -> `TypeNode::Unit`.
    #[test]
    fn unit_gives_unit() {
        assert_eq!(
            construct_type(Some(Type::Unit), empty_model()).unwrap(),
            TypeNode::Unit
        );
    }

    /// `Type::Address { addr, bit }` -> `TypeNode::Address(addr, bit)`.
    #[test]
    fn address_gives_address() {
        let ty = Type::Address {
            address: 0x1234,
            bit: Some(3),
        };
        assert_eq!(
            construct_type(Some(ty), empty_model()).unwrap(),
            TypeNode::Address(0x1234, Some(3))
        );
    }

    /// `Type::Address` без бита -> `TypeNode::Address(addr, None)`.
    #[test]
    fn address_without_bit() {
        let ty = Type::Address {
            address: 0xFF,
            bit: None,
        };
        assert_eq!(
            construct_type(Some(ty), empty_model()).unwrap(),
            TypeNode::Address(0xFF, None)
        );
    }

    // -- Псевдонимы встроенных типов -------------------------------------------

    fn alias(name: &str) -> Type {
        Type::Alias(Identifier::new(name))
    }

    /// `Alias("bit")` -> `TypeNode::Bit`.
    #[test]
    fn alias_bit_gives_bit() {
        assert_eq!(
            construct_type(Some(alias("bit")), empty_model()).unwrap(),
            TypeNode::Bit
        );
    }

    /// `Alias("bool")` -> `TypeNode::Bool`.
    #[test]
    fn alias_bool_gives_bool() {
        assert_eq!(
            construct_type(Some(alias("bool")), empty_model()).unwrap(),
            TypeNode::Bool
        );
    }

    /// `Alias("float")` -> `TypeNode::Rational`.
    #[test]
    fn alias_float_gives_rational() {
        assert_eq!(
            construct_type(Some(alias("float")), empty_model()).unwrap(),
            TypeNode::Rational
        );
    }

    /// `Alias("unit")` -> `TypeNode::Unit`.
    #[test]
    fn alias_unit_gives_unit() {
        assert_eq!(
            construct_type(Some(alias("unit")), empty_model()).unwrap(),
            TypeNode::Unit
        );
    }

    // -- Пользовательские псевдонимы -------------------------------------------

    /// Псевдоним из таблицы типов модели разрешается в соответствующий `TypeNode`.
    ///
    /// # Пример (Takt)
    /// ```but
    /// type byte8 = [bit;8];
    /// var x: byte8 = 0;   // alias "byte8" -> Array(8, Bit)
    /// ```
    #[test]
    fn local_alias_resolves_from_map() {
        let mut map = BTreeMap::new();
        map.insert(
            "byte8".to_string(),
            TypeNode::Array(8, Box::new(TypeNode::Bit)),
        );
        let model = Rc::new(RefCell::new(ModelNode {
            types: map,
            ..Default::default()
        }));
        assert_eq!(
            construct_type(Some(alias("byte8")), model).unwrap(),
            TypeNode::Array(8, Box::new(TypeNode::Bit))
        );
    }

    /// Встроенный псевдоним `u8` разрешается в `Integer { bits: 8, signed: false }`.
    #[test]
    fn builtin_u8_resolves_to_integer() {
        let model = Rc::new(RefCell::new(ModelNode::default()));
        assert_eq!(
            construct_type(Some(alias("u8")), model).unwrap(),
            TypeNode::Integer {
                bits: 8,
                signed: false
            }
        );
    }

    /// Встроенный псевдоним `i32` разрешается в `Integer { bits: 32, signed: true }`.
    #[test]
    fn builtin_i32_resolves_to_integer() {
        let model = Rc::new(RefCell::new(ModelNode::default()));
        assert_eq!(
            construct_type(Some(alias("i32")), model).unwrap(),
            TypeNode::Integer {
                bits: 32,
                signed: true
            }
        );
    }

    /// Контрпример: псевдоним, отсутствующий в таблице, - ошибка.
    ///
    /// # Контрпример (Takt)
    /// ```but
    /// var x: Unknown = 0;   // ошибка: тип Unknown не объявлен
    /// ```
    #[test]
    fn unknown_alias_is_error() {
        let result = construct_type(Some(alias("Unknown")), empty_model());
        assert!(
            result.is_err(),
            "неизвестный псевдоним должен давать ошибку"
        );
        let err = result.unwrap_err();
        assert!(
            err.message.contains("Unknown"),
            "сообщение должно содержать имя неизвестного типа: {}",
            err.message
        );
    }

    // -- Массивы ---------------------------------------------------------------

    /// `Type::Array { N=8, T=Bit }` -> `TypeNode::Array(8, Bit)`.
    #[test]
    fn array_bit_8() {
        let ty = Type::Array {
            loc: Location::default(),
            element_type: Box::new(Type::Bit),
            element_count: 8,
        };
        assert_eq!(
            construct_type(Some(ty), empty_model()).unwrap(),
            TypeNode::Array(8, Box::new(TypeNode::Bit))
        );
    }

    /// Вложенный массив: `[[bit;4];2]` -> `Array(2, Array(4, Bit))`.
    #[test]
    fn nested_array() {
        let inner = Type::Array {
            loc: Location::default(),
            element_type: Box::new(Type::Bit),
            element_count: 4,
        };
        let outer = Type::Array {
            loc: Location::default(),
            element_type: Box::new(inner),
            element_count: 2,
        };
        assert_eq!(
            construct_type(Some(outer), empty_model()).unwrap(),
            TypeNode::Array(2, Box::new(TypeNode::Array(4, Box::new(TypeNode::Bit))))
        );
    }

    /// `Type::Function { .. }` -> `TypeNode::Unsupported`.
    #[test]
    fn function_type_is_unsupported() {
        use crate::parser::ast::ParameterList;
        let ty = Type::Function {
            params: ParameterList::default(),
            returns: None,
        };
        assert_eq!(
            construct_type(Some(ty), empty_model()).unwrap(),
            TypeNode::Unsupported
        );
    }

    // -- Ce4: перечисления -----------------------------------------------------

    /// `Type::Enum("Color")` -> `TypeNode::Enum("Color")`.
    ///
    /// # Пример (Takt)
    /// ```text
    /// enum Color {
    ///     Red = 0,
    ///     Green = 1
    /// }
    /// var c: Color = 0;   // тип аннотации -> TypeNode::Enum("Color")
    /// ```
    #[test]
    fn enum_type_gives_enum_node() {
        assert_eq!(
            construct_type(Some(Type::Enum("Color".to_string())), empty_model()).unwrap(),
            TypeNode::Enum("Color".to_string())
        );
    }

    /// `Type::Enum` с пустым именем -> `TypeNode::Enum("")`.
    ///
    /// Имя не проверяется в `construct_type` - валидация в `validate_model`.
    #[test]
    fn enum_type_empty_name() {
        assert_eq!(
            construct_type(Some(Type::Enum(String::new())), empty_model()).unwrap(),
            TypeNode::Enum(String::new())
        );
    }

    /// `Type::Enum` не зависит от таблицы типов (псевдонимов).
    ///
    /// # Контр-пример
    /// Наличие псевдонима "Color" в таблице не влияет на `Type::Enum("Color")` -
    /// они обрабатываются независимо.
    #[test]
    fn enum_type_ignores_type_alias_table() {
        let mut map = BTreeMap::new();
        // В таблице типов есть "Color" как псевдоним - но Type::Enum идёт своим путём
        map.insert(
            "Color".to_string(),
            TypeNode::Array(8, Box::new(TypeNode::Bit)),
        );
        let model = Rc::new(RefCell::new(ModelNode {
            types: map,
            ..Default::default()
        }));
        // Type::Enum("Color") всё равно -> TypeNode::Enum("Color"), не Array
        assert_eq!(
            construct_type(Some(Type::Enum("Color".to_string())), model).unwrap(),
            TypeNode::Enum("Color".to_string())
        );
    }

    /// `Display` показывает читаемые имена типов, а не Rust-Debug.
    #[test]
    fn type_node_display() {
        assert_eq!(TypeNode::Bit.to_string(), "bit");
        assert_eq!(TypeNode::Bool.to_string(), "bool");
        assert_eq!(TypeNode::Rational.to_string(), "float");
        assert_eq!(TypeNode::Unit.to_string(), "unit");
        assert_eq!(
            TypeNode::Integer {
                bits: 8,
                signed: false
            }
            .to_string(),
            "u8"
        );
        assert_eq!(
            TypeNode::Integer {
                bits: 16,
                signed: true
            }
            .to_string(),
            "i16"
        );
        assert_eq!(
            TypeNode::Integer {
                bits: 64,
                signed: false
            }
            .to_string(),
            "u64"
        );
        assert_eq!(
            TypeNode::Array(8, Box::new(TypeNode::Bit)).to_string(),
            "[bit;8]"
        );
        assert_eq!(TypeNode::Enum("Color".to_string()).to_string(), "Color");
        assert_eq!(TypeNode::Struct("Packet".to_string()).to_string(), "Packet");
        assert_eq!(TypeNode::Inference.to_string(), "_");
        assert_eq!(
            TypeNode::Fixed {
                m: 8,
                n: 8,
                sat: false,
            }
            .to_string(),
            "q(8, 8)"
        );
    }

    // -- Fixed-point q(m, n) ----------------------------

    fn fixed(ctor: &str, m: i128, n: i128) -> Result<TypeNode, Diagnostic> {
        construct_type(
            Some(Type::Fixed(
                Location::Implicit,
                ctor.to_string(),
                m,
                n,
                None,
            )),
            empty_model(),
        )
    }

    /// `q(8, 8)` -> `TypeNode::Fixed { m: 8, n: 8, sat: false }` (T1).
    #[test]
    fn fixed_q_8_8_builds() {
        assert_eq!(
            fixed("q", 8, 8).unwrap(),
            TypeNode::Fixed {
                m: 8,
                n: 8,
                sat: false,
            }
        );
    }

    /// Границы `m >= 1`, `n >= 1`, `m + n <= 64` - ошибка `SE-057` (T2).
    #[test]
    fn fixed_bounds_are_rejected() {
        for (m, n) in [(0, 8), (8, 0), (40, 40), (-1, 8)] {
            let err = fixed("q", m, n).unwrap_err();
            assert_eq!(
                err.code.as_deref(),
                Some("SE-057"),
                "q({m}, {n}) обязан быть ошибкой границ"
            );
        }
    }

    /// Граница ровно `m + n = 64` допустима.
    #[test]
    fn fixed_width_exactly_64_ok() {
        assert_eq!(
            fixed("q", 32, 32).unwrap(),
            TypeNode::Fixed {
                m: 32,
                n: 32,
                sat: false,
            }
        );
    }

    /// Конструктор не `q` -> `SE-057` (иных параметрических типов нет, T17-смежно).
    #[test]
    fn fixed_non_q_constructor_is_rejected() {
        let err = fixed("foo", 8, 8).unwrap_err();
        assert_eq!(err.code.as_deref(), Some("SE-057"));
    }

    /// Машинная ширина хранения: округление вверх до 8/16/32/64.
    #[test]
    fn fixed_storage_bits_rounds_up() {
        assert_eq!(fixed_storage_bits(8), 8);
        assert_eq!(fixed_storage_bits(12), 16);
        assert_eq!(fixed_storage_bits(16), 16);
        assert_eq!(fixed_storage_bits(24), 32);
        assert_eq!(fixed_storage_bits(64), 64);
    }

    fn rat(s: &str, neg: bool) -> ExpressionNode {
        ExpressionNode::Rational(s.to_string(), neg)
    }

    /// Литерал точен: `1.5` в `q(8, 8)` -> представление `384` (T4).
    #[test]
    fn fixed_literal_1_5_is_384() {
        let v = lower_fixed_literal(&rat("1.5", false), 8, 8, Location::Implicit).unwrap();
        assert_eq!(v, Some(384));
    }

    /// Показатель степени в тексте литерала учитывается.
    ///
    /// Текст рационального литерала хранится как написан и с 0144 может нести
    /// показатель (`2.5e2`).
    #[test]
    fn fixed_literal_with_exponent() {
        // 2.5e2 = 250 -> 250·2⁸ = 64000.
        let v = lower_fixed_literal(&rat("2.5e2", false), 16, 8, Location::Implicit).unwrap();
        assert_eq!(v, Some(64_000));
        // Та же величина без показателя обязана дать то же представление.
        let plain = lower_fixed_literal(&rat("250.0", false), 16, 8, Location::Implicit).unwrap();
        assert_eq!(v, plain, "форма записи не должна менять представление");
    }

    /// Показатель, перевешивающий дробную часть, даёт целое: `1.5e1` = 15.
    #[test]
    fn fixed_literal_exponent_outweighs_fraction() {
        let v = lower_fixed_literal(&rat("1.5e1", false), 8, 8, Location::Implicit).unwrap();
        assert_eq!(v, Some(15 * 256));
    }

    /// Отрицательный литерал: `-1.5` -> `-384`.
    #[test]
    fn fixed_literal_negative() {
        let v = lower_fixed_literal(&rat("1.5", true), 8, 8, Location::Implicit).unwrap();
        assert_eq!(v, Some(-384));
    }

    /// Целочисленный литерал масштабируется: `3` в `q(8, 8)` -> `768` (3·2⁸).
    #[test]
    fn fixed_literal_integer_scales() {
        let v = lower_fixed_literal(&ExpressionNode::Number(3), 8, 8, Location::Implicit).unwrap();
        assert_eq!(v, Some(768));
    }

    /// Непредставимый литерал `0.001` -> `SE-058` (T3), а не тихое округление.
    #[test]
    fn fixed_literal_unrepresentable_is_se058() {
        let err = lower_fixed_literal(&rat("0.001", false), 8, 8, Location::Implicit).unwrap_err();
        assert_eq!(err.code.as_deref(), Some("SE-058"));
    }

    /// Вне диапазона: `200.0` в `q(8, 8)` (max = 127.996...) -> `SE-058`.
    #[test]
    fn fixed_literal_out_of_range_is_se058() {
        let err = lower_fixed_literal(&rat("200.0", false), 8, 8, Location::Implicit).unwrap_err();
        assert_eq!(err.code.as_deref(), Some("SE-058"));
    }

    /// Не литерал (переменная и т. п.) -> `None` (обрабатывается арифметикой).
    #[test]
    fn fixed_literal_non_literal_is_none() {
        let v = lower_fixed_literal(&ExpressionNode::None, 8, 8, Location::Implicit).unwrap();
        assert_eq!(v, None);
    }

    /// Диапазон представлений `q(8, 8)` - `[-32768, 32767]` (знаковое i16).
    #[test]
    fn fixed_repr_range_is_signed_width() {
        assert_eq!(fixed_repr_range(8, 8), (-32768, 32767));
    }
}

/// Семантический узел типа данных.
///
/// Варианты:
/// - [`Detecting`](TypeNode::Inference) - тип выводится (временная заглушка).
/// - [`Address`](TypeNode::Address) - адресный тип порта `(адрес, бит?)`.
/// - [`Bit`](TypeNode::Bit) - 1-битный примитив (`bit`).
/// - [`Bool`](TypeNode::Bool) - булев тип (`bool`).
/// - [`Rational`](TypeNode::Rational) - вещественное число (`float`).
/// - [`Array`](TypeNode::Array) - массив исправлениеированного размера `(N, элемент)`.
/// - [`Enum`](TypeNode::Enum) - перечисление (Ce4).
/// - [`Struct`](TypeNode::Struct) - структурный тип (NI3).
/// - [`Unsupported`](TypeNode::Unsupported) - неподдерживаемый тип (например, функциональный).
#[derive(Default, Debug, PartialEq, Eq, Clone)]
#[non_exhaustive]
pub enum TypeNode {
    /// Тип ещё не определён (вывод типа в процессе).
    #[default]
    Inference,
    /// Адресный тип порта: `(адрес, номер_бита?)`.
    Address(u64, Option<u64>),
    /// 1-битный примитив (`bit`).
    Bit,
    /// Тип `bool` - булев тип (`true`/`false`).
    Bool,
    /// Тип с плавающей точкой (`float`).
    Rational,
    /// Длительность (`duration`): целое число **наносекунд**.
    ///
    /// Отдельный тип, а не целое: единица обязана быть частью типа, иначе она теряется
    /// в первом же присваивании (довод тот же, что у `Fixed`). Пересчёт в единицы
    /// профиля - [`semantic::duration`](crate::semantic::duration).
    Duration,
    /// Fixed-point `q(m, n)`: знаковый, дополнительный код; `m` целых бит **включая
    /// знак**, `n` дробных, полная ширина `W = m + n <= 64`. Представимое значение - `v
    /// · 2⁻ⁿ`, где `v : intW`. Границы гарантированы построением ([`construct_type`]);
    /// арифметика - нормативная.
    Fixed {
        /// Целые биты, включая знаковый (`m >= 1`).
        m: u8,
        /// Дробные биты (`n >= 1`).
        n: u8,
        /// Насыщение вместо переноса при переполнении.
        ///
        /// Признак - часть **формата**, а не свойство переменной: арифметика получает
        /// операнды выражениями, и у промежуточного результата взять его больше
        /// неоткуда. Отсюда же следует, что смешение `sat` и не-`sat` в одной операции -
        /// ошибка (`SE-103`), как и смешение разных `q`.
        ///
        /// Поле участвует в равенстве типов. Места, сравнивающие формат ради
        /// **ширины** (выбор `int{S}_t`, `logic signed [W-1:0]`), обязаны
        /// сравнивать `m`/`n`, а не тип целиком.
        sat: bool,
    },
    /// Массив исправлениеированного размера: `(количество_элементов, тип_элемента)`.
    Array(u16, Box<TypeNode>),
    /// Перечисление (Ce4): именованный тип с исправлениеированным набором значений.
    ///
    /// Хранит имя перечисления.
    Enum(String),
    /// Неподдерживаемый тип (например, функциональный).
    Unsupported,
    /// Пустой тип.
    Unit,
    /// Встроенный строковый тип (внутренний, для встроенных функций).
    BuiltinString,
    /// Встроенный тип модели (внутренний, для встроенных функций).
    BuiltinModel,
    /// Встроенный тип состояния (внутренний, для встроенных функций).
    BuiltinState,
    /// Встроенный числовой тип (внутренний, для встроенных функций).
    ///
    /// Обозначает "любой числовой тип": `Bit`, `Rational`, `Array(_, Bit)`.
    /// Используется для параметров и возвращаемых значений математических встроенных
    /// функций (`min`, `max`, `abs`).
    BuiltinNumeric,
    /// Структурный тип (NI3): именованная структура с полями.
    ///
    /// Хранит имя структуры.
    Struct(String),
    /// Встроенный целочисленный тип: `u8`/`i8`...`u64`/`i64`.
    ///
    /// `bits` - разрядность (8, 16, 32, 64); `signed` - знаковость.
    Integer {
        /// Ширина в битах: 8, 16, 32 или 64.
        bits: u8,
        /// `true` -> знаковый (`int{bits}_t`), `false` -> беззнаковый (`uint{bits}_t`).
        signed: bool,
    },
}

impl fmt::Display for TypeNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeNode::Bit => write!(f, "bit"),
            TypeNode::Bool => write!(f, "bool"),
            TypeNode::Rational => write!(f, "float"),
            TypeNode::Duration => write!(f, "duration"),
            // Модификатор печатается: имя типа попадает в текст диагностик
            // (`SE-059`/`SE-103` о смешении), и без него сообщение "нельзя смешивать
            // q(8, 8) и q(8, 8)" было бы бессмысленным.
            TypeNode::Fixed { m, n, sat } => {
                write!(f, "q({}, {})", m, n)?;
                if *sat {
                    write!(f, " sat")?;
                }
                Ok(())
            }
            TypeNode::Unit => write!(f, "unit"),
            TypeNode::Integer { bits, signed } => {
                write!(f, "{}{}", if *signed { "i" } else { "u" }, bits)
            }
            TypeNode::Array(n, elem) => write!(f, "[{};{}]", elem, n),
            TypeNode::Enum(name) => write!(f, "{}", name),
            TypeNode::Struct(name) => write!(f, "{}", name),
            TypeNode::Address(addr, Some(bit)) => write!(f, "0x{:X}:{}", addr, bit),
            TypeNode::Address(addr, None) => write!(f, "0x{:X}", addr),
            TypeNode::BuiltinString => write!(f, "string"),
            TypeNode::BuiltinModel => write!(f, "model"),
            TypeNode::BuiltinState => write!(f, "state"),
            TypeNode::BuiltinNumeric => write!(f, "numeric"),
            TypeNode::Inference => write!(f, "_"),
            TypeNode::Unsupported => write!(f, "?"),
        }
    }
}
