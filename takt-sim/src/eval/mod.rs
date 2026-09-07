//! Ядро вычисления симулятора - **единственное** место семантики значений.
//!
//! # Как модуль удерживает инвариант
//!
//! [`deny(clippy::wildcard_enum_match_arm)`] запрещает подстановочные ветки по вариантам
//! перечислений: новый вариант `Value`, `TypeNode` или `BinOp` ломает сборку, а не
//! превращается молча в "невычислимо" ветвью `_ => None`. Линт настроен точечно на
//! модуле, как предписывает `docs/CODE.md`. Снятие этого `deny` - как и пометка узла
//! `#[non_exhaustive]` - ловит проверка `scripts/check-exhaustive-nodes.sh`: оба пути
//! отключили бы инвариант молча, при зелёной сборке.
//!
//! # Разделение ответственности
//!
//! - [`ops`] - тип-**независимая** семантика: операции над значениями.
//! - [`coerce_to_type`] - тип-**зависимая**: приведение и усечение по объявленному
//!   типу. Вызывается на месте присваивания, где известен тип цели.
//! - [`error`] - структурированная ошибка без позиции; позицию добавляет адаптер.
//!
//! Арифметика ведётся в `i64` и `f64`, сужение - при записи. Это повторяет модель C:
//! продвижение операндов до `int` и сужение при присваивании.

#![deny(clippy::wildcard_enum_match_arm)]

pub(crate) mod access;
pub(crate) mod builtin;
pub(crate) mod duration;
pub(crate) mod error;
pub(crate) mod fixed;
pub(crate) mod ops;
pub(crate) mod place;
pub(crate) mod value;

use takt_lang::semantic::StructDefinitionNode;
use takt_lang::semantic::bit_vector::{self, BitVectorLayout};
use takt_lang::semantic::type_node::TypeNode;

use crate::eval::error::{EvalError, value_kind};
use crate::eval::value::Value;

/// Реестр структурных типов: даёт определение `struct` по имени.
///
/// Живёт в ядре (семантика приведения - здесь), реализуется адаптером над
/// [`takt_lang::semantic::ModelNode::search_struct`] (учитывает поиск вверх по
/// родительским моделям). Без реестра [`coerce_to_type`] не может привести `{1, 2}` к
/// `Point`: ей неизвестны ни число полей, ни их типы, ни порядок.
pub(crate) trait StructRegistry {
    fn find_struct(&self, name: &str) -> Option<StructDefinitionNode>;
}

/// Пустой реестр - для приведений вне модели (тесты, каст): структур нет.
pub(crate) struct EmptyStructs;

impl StructRegistry for EmptyStructs {
    fn find_struct(&self, _name: &str) -> Option<StructDefinitionNode> {
        None
    }
}

/// Приводит значение к объявленному типу переменной.
///
/// Вызывается **на месте присваивания**: там известен тип цели (`VariableNode::ty()`),
/// и там же результат может быть отвергнут. Внутрь `Context::set_value` приведение не
/// убрано намеренно - метод объявлен без `Result`, а S2 обязан уметь отказать; см.
///
/// # Соответствие C
///
/// - Беззнаковые: обёртка mod 2^bits - как в C (`uint8_t x = 255; x + 1` -> `0`).
/// - Знаковые: выход за диапазон - UB в C, поэтому **ошибка**, а не обёртка.
// Единственная подстановочная ветка в модуле - и она вынужденная, а не забытая.
//
// `TypeNode` в крейте `takt-lang` помечен `#[non_exhaustive]` (как предписывает
// `docs/CODE.md` для публичных перечислений), поэтому Rust **требует** от внешнего
// крейта ветку `_`, даже когда перечислены все известные варианты. Механизм ("новый
// вариант ломает сборку") здесь не работает - но он и не был нужен для типов: корневая
// причина фичи жила в разборе `ExpressionNode`/`ConditionNode`, а те
// `#[non_exhaustive]` **не** помечены, значит для адаптеров гарантия сохраняется
// полностью.
//
// Ветка `_` здесь безопасна: она возвращает **ошибку**, а не `None`. Неизвестный тип
// приведёт к диагностике, а не к тихому пропуску - то есть к тому же наблюдаемому
// поведению, что и явно перечисленный неподдерживаемый тип.
///
/// Приведение к типу **без** реестра структур - для вызовов вне модели (тесты, каст).
/// Структурная цель без реестра даёт диагностику, а не тихий пропуск.
pub(crate) fn coerce_to_type(value: Value, ty: &TypeNode) -> Result<Value, EvalError> {
    coerce_to_type_with(value, ty, &EmptyStructs)
}

/// Приведение к типу с реестром структур: та же семантика, что и [`coerce_to_type`], но
/// структурная цель (`Struct`/массив структур) приводится по определению из `structs`.
#[allow(clippy::wildcard_enum_match_arm)]
pub(crate) fn coerce_to_type_with(
    value: Value,
    ty: &TypeNode,
    structs: &dyn StructRegistry,
) -> Result<Value, EvalError> {
    match ty {
        TypeNode::Integer { bits, signed } => coerce_integer(value, *bits, *signed),
        // S7: вариант enum - целое; разрядность подбирает генератор C по максимуму.
        TypeNode::Enum(_) => coerce_integer(value, 64, true),
        // S6: `bit` - один бит. Сверка по `[bit;N]` по-прежнему исключена, но по другой
        // причине: C видит бит-вектор скаляром, симулятор - массивом из N значений. Это
        // вопрос семантики языка (кандидат в `FEATURES.md`), а не дефект эталона.
        TypeNode::Bit => Ok(Value::Number(to_integer(&value, ty)? & 1)),
        // q(m, n): запись в fixed-point-переменную. `Number` - **сырое** представление
        // (грамматика уже понизила литерал: `1.5` -> `384`), поэтому не масштабируется;
        // `Real` (прямое `a := 2.0`) масштабируется; `Fixed` - пересчёт формата (обычно
        // тождественный). Отличие от каста (`cast_to_type`): там `int` масштабируется -
        // время выполнения- целое ещё не в представлении. Оба пути дают один результат, т.к.
        // литерал предмасштабирован грамматикой, а время выполнения-целое - нет.
        TypeNode::Fixed { m, n, sat } => coerce_to_fixed_store(value, *m, *n, *sat),
        TypeNode::Bool => match &value {
            Value::Boolean(b) => Ok(Value::Boolean(*b)),
            Value::Number(n) => Ok(Value::Boolean(*n != 0)),
            Value::Real(_)
            | Value::Array(_)
            | Value::Fixed { .. }
            | Value::Struct { .. }
            | Value::Duration(_) => Err(EvalError::NotCoercible {
                value: value_kind(&value),
                ty: "bool".to_string(),
            }),
        },
        // Длительность: значение уже в наносекундах - канон языка, поэтому приведение
        // тождественно. Число сюда не приводится: это и есть запрет смешения (`SE-065`)
        // на стороне эталона.
        TypeNode::Duration => match &value {
            Value::Duration(ns) => Ok(Value::Duration(*ns)),
            Value::Number(_)
            | Value::Real(_)
            | Value::Boolean(_)
            | Value::Array(_)
            | Value::Fixed { .. }
            | Value::Struct { .. } => Err(EvalError::NotCoercible {
                value: value_kind(&value),
                ty: "duration".to_string(),
            }),
        },
        TypeNode::Rational => match &value {
            Value::Real(f) => Ok(Value::Real(*f)),
            Value::Number(n) => Ok(Value::Real(*n as f64)),
            Value::Fixed { repr, n, .. } => Ok(Value::Real(fixed::to_real(*repr, *n))),
            Value::Boolean(_) | Value::Array(_) | Value::Struct { .. } | Value::Duration(_) => {
                Err(EvalError::NotCoercible {
                    value: value_kind(&value),
                    ty: "float".to_string(),
                })
            }
        },
        // Бит-вектор `[bit;N]`: упакованный скаляр (N <= 64) либо массив 64-битных слов
        // (N > 64) - как в целях. Настоящий массив скаляров идёт своим путём.
        TypeNode::Array(size, elem) => match bit_vector::is_bit_vector(ty) {
            Some(n) => coerce_bit_vector(value, n),
            None => coerce_array(value, *size, elem, structs),
        },
        // Адресный тип порта: значение порта - целое машинное слово.
        TypeNode::Address(_, _) => match &value {
            Value::Number(n) => Ok(Value::Number(*n)),
            Value::Boolean(b) => Ok(Value::Number(i128::from(*b))),
            Value::Real(_)
            | Value::Array(_)
            | Value::Fixed { .. }
            | Value::Struct { .. }
            | Value::Duration(_) => Err(EvalError::NotCoercible {
                value: value_kind(&value),
                ty: "адресный порт".to_string(),
            }),
        },
        // Структурная цель: инициализатор `{...}` (пришёл как `Array`, адаптер не знает
        // типа) приводится по определению; `Struct` того же типа копируется целиком (`q
        // := p`).
        TypeNode::Struct(name) => coerce_struct(value, name, structs),
        TypeNode::Inference => Err(EvalError::UnsupportedType {
            ty: "невыведенный тип".to_string(),
        }),
        TypeNode::Unit => Err(EvalError::UnsupportedType {
            ty: "пустой тип".to_string(),
        }),
        TypeNode::Unsupported => Err(EvalError::UnsupportedType {
            ty: "неподдерживаемый тип".to_string(),
        }),
        TypeNode::BuiltinString => Err(EvalError::UnsupportedType {
            ty: "строка".to_string(),
        }),
        TypeNode::BuiltinModel => Err(EvalError::UnsupportedType {
            ty: "модель".to_string(),
        }),
        TypeNode::BuiltinState => Err(EvalError::UnsupportedType {
            ty: "состояние".to_string(),
        }),
        TypeNode::BuiltinNumeric => match &value {
            Value::Number(_) | Value::Real(_) | Value::Fixed { .. } => Ok(value),
            Value::Boolean(_) | Value::Array(_) | Value::Struct { .. } | Value::Duration(_) => {
                Err(EvalError::NotCoercible {
                    value: value_kind(&value),
                    ty: "числовой тип".to_string(),
                })
            }
        },
        // Вынужденная ветка: `TypeNode` - `#[non_exhaustive]` (см. комментарий над
        // функцией). Отказ с диагностикой, а не тихий пропуск. Имя типа - через
        // `Display`, а не `Debug`: сообщение читает автор программы на Takt, и `Integer
        // { bits: 8, signed: false }` ему ни о чём не говорит.
        _ => Err(EvalError::UnsupportedType { ty: ty.to_string() }),
    }
}

/// Целочисленное значение из [`Value`] (вещественное усекается к нулю, как в C).
fn to_integer(value: &Value, ty: &TypeNode) -> Result<i128, EvalError> {
    match value {
        Value::Number(n) => Ok(*n),
        Value::Boolean(b) => Ok(i128::from(*b)),
        // C усекает float->int в сторону нуля.
        Value::Real(f) => Ok(*f as i128),
        // q(m, n) -> целая часть (floor): `repr >> n`. Штатно сюда не попадает
        // (смешение q с целым - `SE-059`); перевод q->int идёт через `cast_to_type`.
        Value::Fixed { repr, n, .. } => Ok(i128::from(fixed::to_integer_part(*repr, *n))),
        Value::Array(_) | Value::Struct { .. } | Value::Duration(_) => {
            Err(EvalError::NotCoercible {
                value: value_kind(value),
                ty: ty.to_string(),
            })
        }
    }
}

/// Запись значения в переменную типа `q(m, n)` (см. арм `TypeNode::Fixed` в
/// [`coerce_to_type`]).
fn coerce_to_fixed_store(value: Value, m: u8, n: u8, sat: bool) -> Result<Value, EvalError> {
    let repr: i128 = match &value {
        Value::Number(k) => *k, // сырое представление (грамматика масштабировала)
        Value::Fixed { repr, n: n2, .. } => {
            // Пересчёт дробных разрядов (обычно n == n2 -> тождество).
            if *n2 >= n {
                (*repr as i128) >> (*n2 - n)
            } else {
                (*repr as i128) << (n - *n2)
            }
        }
        Value::Real(f) => (f * (1u64 << n) as f64).floor() as i128,
        Value::Boolean(_) | Value::Array(_) | Value::Struct { .. } | Value::Duration(_) => {
            return Err(EvalError::NotCoercible {
                value: value_kind(&value),
                ty: format!("q({m}, {n})"),
            });
        }
    };
    // Запись в переменную `sat` тоже насыщает: иначе присваивание стало бы "дырой" в
    // семантике формата - значение легло бы в переменную обёрнутым.
    Ok(Value::Fixed {
        repr: if sat {
            fixed::saturate(repr, m + n)
        } else {
            fixed::wrap(repr, m + n)
        },
        m,
        n,
        sat,
    })
}

/// Приведение `expr as ty` (узел `Cast`). В отличие от [`coerce_to_type`] (запись -
/// представление уже готово), каст **семантически конвертирует** значение между
/// доменами: `int`/`float`/`bool` ↔ `q(m, n)` масштабируют на 2ⁿ (: приведения только
/// явные).
///
/// `#[allow(wildcard_enum_match_arm)]`: цель, отличная от `Rational`, приводится единым
/// путём (целая часть -> усечение по типу); перечислять все варианты
/// `#[non_exhaustive]`-типа - шум, как и у [`coerce_to_type`].
#[allow(clippy::wildcard_enum_match_arm)]
pub(crate) fn cast_to_type(value: Value, ty: &TypeNode) -> Result<Value, EvalError> {
    // Цель - q(m, n): масштабируем значение к представлению.
    if let TypeNode::Fixed { m, n, sat } = ty {
        return fixed::cast_to_fixed(&value, *m, *n, *sat);
    }
    // Источник - q(m, n), цель иная: разворачиваем и приводим обычным путём.
    if let Value::Fixed { repr, n, .. } = value {
        return match ty {
            TypeNode::Rational => Ok(Value::Real(fixed::to_real(repr, n))),
            // int/bit/enum: целая часть (floor), затем усечение по цели.
            _ => coerce_to_type(
                Value::Number(i128::from(fixed::to_integer_part(repr, n))),
                ty,
            ),
        };
    }
    // Длительность: мост к числам - **миллисекунды**. Пересчёт зовётся из общего слоя
    // `semantic::duration`, а не считается здесь: цели обязаны получить тот же ответ на
    // вопрос "сколько это миллисекунд".
    if let Value::Duration(ns) = value {
        return coerce_to_type(
            Value::Number(i128::from(takt_lang::semantic::duration::to_millis(ns))),
            ty,
        );
    }
    if matches!(ty, TypeNode::Duration) {
        // Длительность живёт в `i64` наносекунд: её границу не трогала, поэтому число
        // шире `i64` миллисекунд - переполнение, а не молчаливое усечение.
        let millis = i64::try_from(to_integer(&value, ty)?)
            .map_err(|_| EvalError::ArithmeticOverflow { op: "as duration" })?;
        return takt_lang::semantic::duration::from_millis(millis)
            .map(Value::Duration)
            .ok_or(EvalError::ArithmeticOverflow { op: "as duration" });
    }
    // Ни источник, ни цель не q и не duration - прежнее поведение каста (= запись).
    coerce_to_type(value, ty)
}

/// S1/S2/S9: усечение (беззнаковые) либо проверка диапазона (знаковые).
///
/// **64-битный тип идёт общим путём**. С носителем `i128` спецслучая больше нет - маска
/// и границы считаются той же формулой, что для узких типов.
fn coerce_integer(value: Value, bits: u8, signed: bool) -> Result<Value, EvalError> {
    let ty = TypeNode::Integer { bits, signed };
    let n = to_integer(&value, &ty)?;
    // Само правило живёт в общем носителе: компилятор считает приведение теми же
    // формулами, и вторая реализация разошлась бы с этой значениями. Здесь остаётся
    // перевод ошибки в диагностику эталона - `SIM-003` в такте против `SE-121` при
    // сборке: текст у них разный.
    takt_lang::semantic::const_eval::int_cast::integer(n, bits, signed)
        .map(Value::Number)
        .map_err(|overflow| EvalError::SignedOverflow {
            value: overflow.value,
            bits: overflow.bits,
        })
}

/// Поэлементное приведение массива с проверкой длины. `structs` протаскивается -
/// элементом может быть структура (`[Point; 4]`). Приведение к бит-вектору `[bit;N]`.
///
/// **N <= 64** - упакованный **скаляр** `Value::Number`, приведённый как
/// беззнаковое целое ширины `round_up(N)` (то есть идентично `uN` - `[bit;8]` ==
/// `u8`). Битовый паттерн в `i64` достаточен: бит-доступ его читает.
///
/// **N > 64** - `Value::Array` из `⌈N/64⌉` слов-`Number`: одиночное `Number`
/// раскладывается в младшее слово (остальные - 0), готовый массив слов -
/// усекается/дополняется до нужного числа.
fn coerce_bit_vector(value: Value, n: u16) -> Result<Value, EvalError> {
    match bit_vector::layout(n) {
        BitVectorLayout::Scalar { width } => {
            coerce_integer(value, u8::try_from(width).unwrap_or(64), false)
        }
        BitVectorLayout::Words { count } => {
            let count = usize::from(count);
            let words = match value {
                Value::Number(v) => {
                    let mut w = vec![Value::Number(0); count];
                    w[0] = Value::Number(v);
                    w
                }
                Value::Array(items) => {
                    let mut w: Vec<Value> = items.into_iter().take(count).collect();
                    w.resize(count, Value::Number(0));
                    w
                }
                other @ (Value::Boolean(_)
                | Value::Real(_)
                | Value::Fixed { .. }
                | Value::Struct { .. }
                | Value::Duration(_)) => {
                    return Err(EvalError::NotCoercible {
                        value: value_kind(&other),
                        ty: format!("[bit;{n}]"),
                    });
                }
            };
            Ok(Value::Array(words))
        }
    }
}

fn coerce_array(
    value: Value,
    size: u16,
    elem: &TypeNode,
    structs: &dyn StructRegistry,
) -> Result<Value, EvalError> {
    let Value::Array(items) = value else {
        return Err(EvalError::NotCoercible {
            value: value_kind(&value),
            ty: format!("массив [{size}]"),
        });
    };
    if items.len() != usize::from(size) {
        return Err(EvalError::NotCoercible {
            value: "массив другой длины",
            ty: format!("массив [{size}]"),
        });
    }
    let coerced = items
        .into_iter()
        .map(|item| coerce_to_type_with(item, elem, structs))
        .collect::<Result<Vec<Value>, EvalError>>()?;
    Ok(Value::Array(coerced))
}

/// Приведение к структурному типу `name`.
///
/// Инициализатор `{...}` приходит как [`Value::Array`] (адаптер типа не знает) -
/// приводится **по позиции** в объявленном порядке полей (рекурсивно). Значение
/// [`Value::Struct`] того же типа копируется как есть (`q := p`); другого типа -
/// `StructTypeMismatch` (симметрия с C, запрещающим неявную конверсию структур).
fn coerce_struct(
    value: Value,
    name: &str,
    structs: &dyn StructRegistry,
) -> Result<Value, EvalError> {
    let def = structs
        .find_struct(name)
        .ok_or_else(|| EvalError::UnsupportedType {
            ty: format!("структура '{name}' (определение не найдено)"),
        })?;
    match value {
        // Позиционный инициализатор `{1, 2}` -> поля в объявленном порядке.
        Value::Array(items) => {
            if items.len() != def.fields.len() {
                return Err(EvalError::StructArity {
                    name: name.to_string(),
                    expected: def.fields.len(),
                    got: items.len(),
                });
            }
            let fields = def
                .fields
                .iter()
                .zip(items)
                .map(|((fname, fty), item)| {
                    coerce_to_type_with(item, fty, structs).map(|v| (fname.clone(), v))
                })
                .collect::<Result<Vec<(String, Value)>, EvalError>>()?;
            Ok(Value::Struct {
                name: name.to_string(),
                fields,
            })
        }
        // Копирование структуры целиком: имена типов обязаны совпасть.
        Value::Struct { name: got, fields } => {
            if got != name {
                return Err(EvalError::StructTypeMismatch {
                    expected: name.to_string(),
                    got,
                });
            }
            Ok(Value::Struct { name: got, fields })
        }
        // Прочие значения к структуре не приводятся (модуль под
        // `deny(wildcard_enum_match_arm)` - варианты перечислены явно).
        scalar @ (Value::Number(_)
        | Value::Real(_)
        | Value::Boolean(_)
        | Value::Fixed { .. }
        | Value::Duration(_)) => Err(EvalError::NotCoercible {
            value: value_kind(&scalar),
            ty: format!("структура '{name}'"),
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn u(bits: u8) -> TypeNode {
        TypeNode::Integer {
            bits,
            signed: false,
        }
    }

    fn i(bits: u8) -> TypeNode {
        TypeNode::Integer { bits, signed: true }
    }

    // -- S1: обёртка беззнакового (сверено с cc -std=c11) ----------------------

    #[test]
    fn s1_u8_wraps_on_overflow() {
        // C: uint8_t y = 255; y = y + 1; -> 0 (проверено пробой на cc).
        assert_eq!(
            coerce_to_type(Value::Number(256), &u(8)),
            Ok(Value::Number(0))
        );
    }

    #[test]
    fn s9_u8_truncates_large_value() {
        // 300 mod 256 = 44.
        assert_eq!(
            coerce_to_type(Value::Number(300), &u(8)),
            Ok(Value::Number(44))
        );
    }

    #[test]
    fn s1_u8_negative_wraps_like_c_cast() {
        // C: (uint8_t)-1 == 255.
        assert_eq!(
            coerce_to_type(Value::Number(-1), &u(8)),
            Ok(Value::Number(255))
        );
    }

    #[test]
    fn s4_shift_result_truncates_to_u8() {
        // S4: u8 `x << 8` -> в C определено: 1<<8 = 256, запись в uint8_t -> 0.
        assert_eq!(
            coerce_to_type(Value::Number(256), &u(8)),
            Ok(Value::Number(0))
        );
    }

    #[test]
    fn s1_u16_wraps() {
        assert_eq!(
            coerce_to_type(Value::Number(65_536), &u(16)),
            Ok(Value::Number(0))
        );
    }

    #[test]
    fn u8_in_range_is_unchanged() {
        assert_eq!(
            coerce_to_type(Value::Number(42), &u(8)),
            Ok(Value::Number(42))
        );
    }

    // -- S2: знаковое переполнение -> ошибка ------------------------------------

    #[test]
    fn s2_i8_overflow_is_error_not_wrap() {
        // В C это UB - не воспроизводим (принцип ).
        assert_eq!(
            coerce_to_type(Value::Number(128), &i(8)),
            Err(EvalError::SignedOverflow {
                value: 128,
                bits: 8
            })
        );
    }

    #[test]
    fn s2_i8_underflow_is_error() {
        assert!(matches!(
            coerce_to_type(Value::Number(-129), &i(8)),
            Err(EvalError::SignedOverflow { .. })
        ));
    }

    #[test]
    fn i8_boundaries_are_accepted() {
        assert_eq!(
            coerce_to_type(Value::Number(127), &i(8)),
            Ok(Value::Number(127))
        );
        assert_eq!(
            coerce_to_type(Value::Number(-128), &i(8)),
            Ok(Value::Number(-128))
        );
    }

    // -- S6: bit ---------------------------------------------------------------

    #[test]
    fn s6_bit_truncates_to_single_bit() {
        // `var f: bit := 1; f := f + 1;` даёт 2 & 1, то есть 0.
        assert_eq!(
            coerce_to_type(Value::Number(2), &TypeNode::Bit),
            Ok(Value::Number(0))
        );
        assert_eq!(
            coerce_to_type(Value::Number(3), &TypeNode::Bit),
            Ok(Value::Number(1))
        );
    }

    // -- S7: enum --------------------------------------------------------------

    #[test]
    fn s7_enum_variant_is_integer() {
        assert_eq!(
            coerce_to_type(Value::Number(1), &TypeNode::Enum("Mode".to_string())),
            Ok(Value::Number(1))
        );
    }

    // -- bool / float ----------------------------------------------------------

    #[test]
    fn bool_from_number_follows_c() {
        assert_eq!(
            coerce_to_type(Value::Number(2), &TypeNode::Bool),
            Ok(Value::Boolean(true))
        );
        assert_eq!(
            coerce_to_type(Value::Number(0), &TypeNode::Bool),
            Ok(Value::Boolean(false))
        );
    }

    #[test]
    fn rational_accepts_int_and_real() {
        assert_eq!(
            coerce_to_type(Value::Number(3), &TypeNode::Rational),
            Ok(Value::Real(3.0))
        );
        assert_eq!(
            coerce_to_type(Value::Real(0.5), &TypeNode::Rational),
            Ok(Value::Real(0.5))
        );
    }

    #[test]
    fn int_from_real_truncates_toward_zero_like_c() {
        assert_eq!(
            coerce_to_type(Value::Real(2.9), &u(8)),
            Ok(Value::Number(2))
        );
    }

    // -- Массивы ---------------------------------------------------------------

    #[test]
    fn array_coerces_elementwise() {
        let value = Value::Array(vec![Value::Number(256), Value::Number(1)]);
        let ty = TypeNode::Array(2, Box::new(u(8)));
        assert_eq!(
            coerce_to_type(value, &ty),
            Ok(Value::Array(vec![Value::Number(0), Value::Number(1)]))
        );
    }

    #[test]
    fn array_length_mismatch_is_error() {
        let value = Value::Array(vec![Value::Number(1)]);
        let ty = TypeNode::Array(2, Box::new(u(8)));
        assert!(matches!(
            coerce_to_type(value, &ty),
            Err(EvalError::NotCoercible { .. })
        ));
    }

    // -- Контрпримеры: явная диагностика вместо тихого пропуска ----------------

    #[test]
    fn t22_struct_type_is_explicit_diagnostic() {
        // Структуры этим приведением не поддерживаются, и об этом сообщается.
        let err = coerce_to_type(Value::Number(1), &TypeNode::Struct("P".to_string()));
        assert!(matches!(err, Err(EvalError::UnsupportedType { .. })));
        assert!(err.unwrap_err().message().contains("структура"));
    }

    #[test]
    fn array_to_scalar_is_error() {
        assert!(matches!(
            coerce_to_type(Value::Array(vec![]), &u(8)),
            Err(EvalError::NotCoercible { .. })
        ));
    }

    #[test]
    fn unsupported_types_report_reason() {
        for ty in [
            TypeNode::Inference,
            TypeNode::Unit,
            TypeNode::Unsupported,
            TypeNode::BuiltinString,
            TypeNode::BuiltinModel,
            TypeNode::BuiltinState,
        ] {
            assert!(
                matches!(
                    coerce_to_type(Value::Number(1), &ty),
                    Err(EvalError::UnsupportedType { .. })
                ),
                "ожидалась диагностика для {ty:?}"
            );
        }
    }
}
