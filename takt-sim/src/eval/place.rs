//! Обновление значения "по месту" (lvalue): запись в поле структуры, в элемент массива
//! и в отдельный бит.
//!
//! Адаптер ([`crate::unit::statement`]) раскладывает левую часть `p.x := ...` /
//! `data[i] := ...` / `flags.3 := ...` в корень + путь **сегментов** ([`PlaceSegment`])
//! и зовёт [`update`]; семантика замены живёт **здесь**, в ядре под
//! `deny(wildcard_enum_match_arm)`. Точечность обязательна: `p.x := 7` **не**
//! пересоздаёт структуру и не трогает `p.y`; `data[0] := 7` не трогает `data[1]`;
//! `flags.3 := 1` не трогает прочие разряды.
//!
//! ## Правило записи бита
//!
//! В разряд кладётся **младший бит значения** (`v & 1`), поэтому `flags.3 := 2` бит
//! **очищает**. Правило не выдумано здесь: его печатают цели `c` (`(rhs & 1u) << N`) и
//! `sv` (присваивание разряду есть усечение до младшего бита) - замер.

use takt_lang::semantic::type_node::TypeNode;

use super::error::{EvalError, value_kind};
use super::value::Value;
use super::{StructRegistry, coerce_to_type_with};

/// Сегмент пути к месту записи: поле структуры, индекс массива или номер разряда.
///
/// Индекс уже вычислен адаптером в `usize`: у адаптера есть контекст, а ядро АСД не
/// знает. Поле - имя. Номер разряда - литерал (`Member::Number`), другой формы у него в
/// языке нет.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum PlaceSegment {
    /// `.имя` - поле структуры.
    Field(String),
    /// `[i]` - элемент массива по вычисленному индексу.
    Index(usize),
    /// `.N` - отдельный разряд целого или упакованного бит-вектора.
    ///
    /// Разряд - **лист пути по построению**: у бита нет ни полей, ни элементов, ни
    /// разрядов. Продолжение за ним отвергается, а не игнорируется молча.
    Bit(i128),
}

/// Заменяет значение по пути сегментов `path` в `value`, возвращая обновлённое целое.
/// `ty` - **объявленный** тип `value` (тип корневой переменной на верхнем вызове),
/// протаскивается вниз, чтобы лист приводился к типу поля/элемента.
///
/// Лист приводится к объявленному типу: `p.y := 300` при `y: u8` даёт `44`, и
/// `data[0] := 300` при `[u8;4]` - тоже `44`.
/// Несоответствие селектора виду значения (поле у не-структуры, индекс у не-массива),
/// неизвестное поле, выход за границы -> [`EvalError`]. Если тип недоступен (`None`) -
/// лист без приведения (консервативно, как без реестра).
pub(crate) fn update(
    value: Value,
    ty: Option<&TypeNode>,
    path: &[PlaceSegment],
    new: Value,
    structs: &dyn StructRegistry,
) -> Result<Value, EvalError> {
    let Some((segment, rest)) = path.split_first() else {
        // Пустой путь: замена целиком. Вызывается только с непустым путём -
        // присваивание всей переменной идёт мимо `update` (ветка `Variable`).
        return Ok(new);
    };
    match segment {
        PlaceSegment::Field(field) => update_field(value, field, rest, new, structs),
        PlaceSegment::Index(index) => update_index(value, ty, *index, rest, new, structs),
        PlaceSegment::Bit(bit) => update_bit(value, *bit, rest, new),
    }
}

/// Запись в поле структуры. Тип поля берётся из реестра `structs` (в `TypeNode::Struct`
/// полей нет - только имя), поэтому `ty` здесь не нужен.
fn update_field(
    value: Value,
    field: &str,
    rest: &[PlaceSegment],
    new: Value,
    structs: &dyn StructRegistry,
) -> Result<Value, EvalError> {
    let Value::Struct { name, mut fields } = value else {
        return Err(EvalError::FieldOfNonStruct {
            value: value_kind(&value),
        });
    };
    let Some(idx) = fields.iter().position(|(f, _)| f == field) else {
        return Err(EvalError::UnknownField {
            name,
            field: field.to_string(),
        });
    };
    let field_ty = structs.find_struct(&name).and_then(|def| {
        def.fields
            .iter()
            .find(|(fname, _)| fname == field)
            .map(|(_, t)| t.clone())
    });
    let new_field = if rest.is_empty() {
        match field_ty {
            Some(ty) => coerce_to_type_with(new, &ty, structs)?,
            None => new,
        }
    } else {
        update(fields[idx].1.clone(), field_ty.as_ref(), rest, new, structs)?
    };
    fields[idx].1 = new_field;
    Ok(Value::Struct { name, fields })
}

/// Запись в элемент массива по индексу. Тип элемента берётся из `ty =
/// TypeNode::Array(_, elem)` - реестр тут ни при чём (массив имён полей не имеет).
/// Выход за границы -> `ArrayIndexOutOfBounds`; запись индекса в не-массив ->
/// `IndexOfNonArray`.
fn update_index(
    value: Value,
    ty: Option<&TypeNode>,
    index: usize,
    rest: &[PlaceSegment],
    new: Value,
    structs: &dyn StructRegistry,
) -> Result<Value, EvalError> {
    let Value::Array(mut items) = value else {
        return Err(EvalError::IndexOfNonArray {
            value: value_kind(&value),
        });
    };
    if index >= items.len() {
        return Err(EvalError::ArrayIndexOutOfBounds {
            index,
            len: items.len(),
        });
    }
    let elem_ty = match ty {
        Some(TypeNode::Array(_, elem)) => Some(elem.as_ref()),
        _ => None,
    };
    let new_elem = if rest.is_empty() {
        match elem_ty {
            Some(t) => coerce_to_type_with(new, t, structs)?,
            None => new,
        }
    } else {
        update(items[index].clone(), elem_ty, rest, new, structs)?
    };
    items[index] = new_elem;
    Ok(Value::Array(items))
}

/// Запись одного разряда - **зеркало** чтения ([`crate::eval::access::read_member`]).
///
/// Диспетчеризация та же и по тому же признаку - **виду значения**, а не имени узла:
/// `Number`/`Boolean` - целое, `Array` - упакованный бит-вектор `[bit;N > 64]`. Коды
/// ошибок берутся у чтения: один носитель не вправе отвечать по-разному на чтение и на
/// запись.
///
/// Записываемое значение сводится к **младшему биту** (`v & 1`), как у целей `c` и
/// `sv`; `Boolean` - к `0`/`1`. Прочие виды значения бита не имеют.
fn update_bit(
    value: Value,
    bit: i128,
    rest: &[PlaceSegment],
    new: Value,
) -> Result<Value, EvalError> {
    // Разряд - лист: `flags.3.x` и `flags.3[0]` смысла не имеют.
    if !rest.is_empty() {
        return Err(EvalError::BitOfNonInteger {
            value: value_kind(&value),
        });
    }
    let one = bit_of_value(&new)?;
    match value {
        Value::Number(n) => Ok(Value::Number(set_bit(n, bit, one)?)),
        // У логического значения есть только нулевой разряд, и он - оно само.
        Value::Boolean(_) => {
            if bit == 0 {
                Ok(Value::Boolean(one == 1))
            } else {
                // У логического ровно один разряд - так и говорим.
                Err(EvalError::BitIndexOutOfRange { bit, width: 1 })
            }
        }
        // Бит-вектор `[bit;N]` при N > 64: слово `k / 64`, смещение `k % 64` - та же
        // раскладка, что у чтения.
        Value::Array(mut words) => {
            // Ширина вектора известна точно - по числу слов.
            let width = words.len() * 64;
            let Ok(k) = u32::try_from(bit) else {
                return Err(EvalError::BitIndexOutOfRange { bit, width });
            };
            let (w, off) = takt_lang::semantic::bit_vector::bit_slot(k);
            let slot = words.get_mut(usize::from(w));
            match slot {
                Some(Value::Number(word)) => {
                    *word = set_bit(*word, i128::from(off), one)?;
                    Ok(Value::Array(words))
                }
                _ => Err(EvalError::BitIndexOutOfRange { bit, width }),
            }
        }
        Value::Struct { name, .. } => Err(EvalError::BitIndexOfStruct { name }),
        // Разряда у величины нет: те же виды и тот же код, что у чтения
        // (`read_member`). Ветки `_` здесь нет намеренно - новый вид значения обязан
        // завалить сборку, а не молча стать "не целым".
        other @ (Value::Real(_) | Value::Fixed { .. } | Value::Duration(_)) => {
            Err(EvalError::BitOfNonInteger {
                value: value_kind(&other),
            })
        }
    }
}

/// Младший бит записываемого значения: правило целей `c` и `sv`.
fn bit_of_value(new: &Value) -> Result<i128, EvalError> {
    match new {
        Value::Number(n) => Ok(n & 1),
        Value::Boolean(b) => Ok(i128::from(*b)),
        other @ (Value::Real(_)
        | Value::Array(_)
        | Value::Fixed { .. }
        | Value::Struct { .. }
        | Value::Duration(_)) => Err(EvalError::BitOfNonInteger {
            value: value_kind(other),
        }),
    }
}

/// Заменяет разряд `bit` целого `bits` значением `one` (0 или 1).
///
/// Диапазон - тот же `[0, 64)`, что у чтения ([`crate::eval::access::read_member`]):
/// шире носитель `i128` не обещает, а расхождение границ чтения и записи дало бы один и
/// тот же разряд то доступным, то нет.
fn set_bit(bits: i128, bit: i128, one: i128) -> Result<i128, EvalError> {
    if !(0..64).contains(&bit) {
        // Ширина носителя - та же, что у чтения (`read_bit`): границы чтения и записи
        // обязаны совпадать, иначе один разряд то доступен, то нет.
        return Err(EvalError::BitIndexOutOfRange { bit, width: 64 });
    }
    let mask = 1i128 << bit;
    Ok(if one == 1 { bits | mask } else { bits & !mask })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::EmptyStructs;

    fn point(x: i128, y: i128) -> Value {
        Value::Struct {
            name: "Point".to_string(),
            fields: vec![
                ("x".to_string(), Value::Number(x)),
                ("y".to_string(), Value::Number(y)),
            ],
        }
    }

    fn field(name: &str) -> Vec<PlaceSegment> {
        vec![PlaceSegment::Field(name.to_string())]
    }

    #[test]
    fn update_field_is_pointwise() {
        // p.x:= 7 не трогает p.y (без реестра - без приведения типа поля).
        let updated = update(
            point(1, 2),
            None,
            &field("x"),
            Value::Number(7),
            &EmptyStructs,
        )
        .unwrap();
        assert_eq!(updated, point(7, 2));
    }

    #[test]
    fn update_unknown_field_is_error() {
        let err = update(
            point(1, 2),
            None,
            &field("z"),
            Value::Number(0),
            &EmptyStructs,
        )
        .unwrap_err();
        assert_eq!(err.code(), "SIM-027");
    }

    #[test]
    fn update_field_on_non_struct_is_error() {
        let err = update(
            Value::Number(5),
            None,
            &field("x"),
            Value::Number(0),
            &EmptyStructs,
        )
        .unwrap_err();
        assert_eq!(err.code(), "SIM-012");
    }

    // -- Массивы --------------------------------------------------

    fn arr(vals: &[i128]) -> Value {
        Value::Array(vals.iter().map(|&n| Value::Number(n)).collect())
    }

    fn index(i: usize) -> Vec<PlaceSegment> {
        vec![PlaceSegment::Index(i)]
    }

    #[test]
    fn update_array_element_is_pointwise() {
        // data[1]:= 9 не трогает соседей (без типа - без приведения элемента).
        let updated = update(
            arr(&[1, 2, 3]),
            None,
            &index(1),
            Value::Number(9),
            &EmptyStructs,
        )
        .unwrap();
        assert_eq!(updated, arr(&[1, 9, 3]));
    }

    #[test]
    fn update_array_out_of_bounds_is_error() {
        let err = update(
            arr(&[1, 2]),
            None,
            &index(5),
            Value::Number(0),
            &EmptyStructs,
        )
        .unwrap_err();
        assert_eq!(err.code(), "SIM-010");
    }

    #[test]
    fn update_index_on_non_array_is_error() {
        let err = update(
            Value::Number(5),
            None,
            &index(0),
            Value::Number(0),
            &EmptyStructs,
        )
        .unwrap_err();
        assert_eq!(err.code(), "SIM-010");
    }

    #[test]
    fn update_array_element_coerced_to_elem_type() {
        // `data[0] := 300` при `[u8;2]` даёт 44: усечение идёт по типу элемента.
        let u8_ty = TypeNode::Integer {
            bits: 8,
            signed: false,
        };
        let arr_ty = TypeNode::Array(2, Box::new(u8_ty));
        let updated = update(
            arr(&[0, 0]),
            Some(&arr_ty),
            &index(0),
            Value::Number(300),
            &EmptyStructs,
        )
        .unwrap();
        assert_eq!(updated, arr(&[44, 0]));
    }
}
