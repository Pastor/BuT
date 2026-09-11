//! Размещение портов по карте адресов: `AT %...` для цели `st-at`.
//!
//! ## Цели асимметричны - и это не косметика
//!
//! - `st` - **библиотека блоков**: порты суть `VAR_INPUT`/`VAR_OUTPUT` у
//!   `FUNCTION_BLOCK`, адрес не эмитится, вывод от `--address-map` не зависит.
//! - `st-at` - **программа для ПЛК целиком**: порты становятся размещёнными
//!   глобальными переменными (`VAR_GLOBAL ... AT %IX256.0`), а блоки видят их через
//!   `VAR_EXTERNAL`.
//!
//! Асимметрия навязана фактом, а не вкусом: `VAR_GLOBAL` вне `CONFIGURATION`
//! недопустим, поэтому `st-at` обязана печатать полную обёртку `CONFIGURATION`,
//! `RESOURCE`, `TASK`, `PROGRAM`, тогда как `st` обходится голыми блоками.
//!
//! ## Правила локации
//!
//! | Что | Откуда | Значение |
//! |---|---|---|
//! | Класс | `direction` порта | `In`->`%I`, `Out`->`%Q`, `InOut`->`%M` |
//! | Размер | **`TypeNode`**, а не C-тип | `BOOL`->`X`, 8->`B`, 16->`W`, 32->`D`, 64->`L`, `LREAL`->`L` |
//! | Номер | `ResolvedAddress::addr` | **десятичный**: `0x` стандарт не допускает |
//! | Бит | `ResolvedAddress::bit` | только для `BOOL`: `%IX256.0` |
//!
//! `InOut`->`%M` - соглашение: двунаправленной локации в IEC нет, а `%M` (память) ближе
//! всего по смыслу.

use crate::address_map::{AddressSource, ResolvedAddress};
use crate::diagnostics::lang::keys;
use crate::diagnostics::{Diagnostic, Location};
use crate::msg;
use crate::semantic::PortDirection;
use crate::semantic::type_node::TypeNode;

/// Строит текст локации (`%IX256.0`) и комментарий-пояснение к порту.
///
/// Возвращает `(локация, комментарий, предупреждения)`.
///
/// # Ошибки
/// `ST-004` - тип порта не имеет локации (`Array`/`Enum`/`Struct`) либо адрес
/// отрицателен.
pub(crate) fn location_of(
    name: &str,
    ty: &TypeNode,
    direction: PortDirection,
    resolved: &ResolvedAddress,
    model: &crate::semantic::ModelNode,
) -> Result<(String, String, Vec<Diagnostic>), Diagnostic> {
    let mut warnings = Vec::new();

    if resolved.addr < 0 {
        return Err(no_location(&msg!(
            keys::ST_WHAT_NEGATIVE_ADDRESS,
            name = name,
            addr = resolved.addr
        )));
    }

    let class = match direction {
        PortDirection::In => "I",
        PortDirection::Out => "Q",
        // "Двунаправленной" локации в IEC нет; `%M` (память) - ближайший смысл.
        PortDirection::InOut => "M",
    };

    let is_bool = matches!(ty, TypeNode::Bit | TypeNode::Bool);
    let size = size_of(ty, model)
        .ok_or_else(|| no_location(&msg!(keys::ST_WHAT_NO_LOCATION, name = name, ty = ty)))?;

    let location = if is_bool {
        // Бит обязателен для `%IX`: без него адресуется не тот объект.
        //
        // позиция бита **нормирована слоем адресов** - однобитному порту без позиции
        // там подставляется ноль и выдаётся `SE-090`. Поэтому ветки "бита нет" здесь
        // больше не существует, а прежнее предупреждение `ST-005` выведено из
        // обращения: одно решение языка принималось тремя потребителями порознь (см.
        // Ноль как умолчание оставлен только на случай карты, пришедшей мимо нормировки -
        // печатать отрицательный бит в локацию IEC нельзя.
        let bit = match resolved.bit {
            Some(b) if b >= 0 => b,
            Some(b) => {
                return Err(no_location(&msg!(
                    keys::ST_WHAT_NEGATIVE_BIT,
                    name = name,
                    bit = b
                )));
            }
            None => 0,
        };
        format!("%{}X{}.{}", class, resolved.addr, bit)
    } else {
        // У не-BOOL локации бита нет: игнорировать молча нельзя - в исходнике он
        // написан, и автор вправе думать, что он что-то значит.
        if resolved.bit.is_some() {
            warnings.push(
                Diagnostic::warning(
                    crate::generator::site::at(Location::Codegen),
                    msg!(
                        keys::ST_006_BIT_IGNORED,
                        name = name,
                        class = class,
                        size = size
                    ),
                )
                .with_code("ST-006"),
            );
        }
        format!("%{}{}{}", class, size, resolved.addr)
    };

    // Комментарий обязателен: делает интерпретацию (`0x100` -> `256`) проверяемой
    // глазами и облегчает наладку на стенде.
    let comment = format!(
        "(* 0x{:X}{}, источник: {} *)",
        resolved.addr,
        resolved.bit.map(|b| format!(":{}", b)).unwrap_or_default(),
        source_name(resolved.source)
    );
    Ok((location, comment, warnings))
}

/// Буква размера локации по типу Takt.
///
/// Размер берётся из `TypeNode`, а **не** из C-типа: цель `c` для `bit` печатает `int`
/// (дефект Д2 ), и наследовать эту ошибку в адресацию нельзя - `%IX` и `%ID` указывают
/// на разные ячейки.
fn size_of(ty: &TypeNode, model: &crate::semantic::ModelNode) -> Option<&'static str> {
    Some(match ty {
        TypeNode::Bit | TypeNode::Bool => "X",
        TypeNode::Integer { bits: 8, .. } => "B",
        TypeNode::Integer { bits: 16, .. } => "W",
        TypeNode::Integer { bits: 32, .. } => "D",
        TypeNode::Integer { bits: 64, .. } => "L",
        // `LREAL` - 64 бита.
        TypeNode::Rational => "L",
        // Прочие скаляры цель печатает целым, и локация даётся именно целому:
        // перечисление, длительность и `q(m, n)`. Ширину даёт один носитель
        // `semantic::scalar_port` - тот же, которым цель `rust` выбирает метод
        // HAL-трейта.
        _ => match crate::semantic::scalar_port::scalar_repr(ty, model)?.bits {
            8 => "B",
            16 => "W",
            32 => "D",
            _ => "L",
        },
    })
}

/// Человекочитаемое имя источника адреса (для комментария).
fn source_name(source: AddressSource) -> &'static str {
    match source {
        AddressSource::Inline => "inline",
        AddressSource::Operator => "оператор address",
        AddressSource::External => "внешняя карта",
    }
}

/// Строит диагностику `ST-004` - порт без выразимой локации.
fn no_location(what: &str) -> Diagnostic {
    // Позицию даёт общий носитель: своей у отказа размещения нет.
    Diagnostic::error(
        crate::generator::site::at(Location::Codegen),
        msg!(keys::ST_004_PLACEMENT, what = what),
    )
    .with_code("ST-004")
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Пустая модель: перечислений в ней нет, и `size_of` их не ищет.
    ///
    /// Тесты размещения проверяют скаляры; перечислимый порт - предмет отдельного
    /// набора, где модель несёт объявление.
    fn model() -> crate::semantic::ModelNode {
        crate::semantic::ModelNode::default()
    }

    fn resolved(addr: i64, bit: Option<i64>) -> ResolvedAddress {
        ResolvedAddress {
            addr,
            bit,
            source: AddressSource::Inline,
            ty: crate::semantic::type_node::TypeNode::Bit,
            direction: crate::semantic::PortDirection::In,
            name: "port".to_string(),
        }
    }

    /// Входной `bit` -> `%IX<адрес>.<бит>`.
    ///
    /// Сверка с ручным прогоном правил (план ): `in task_valid: bit at 0x100:0;` ->
    /// `task_valid AT %IX256.0 : BOOL;`.
    #[test]
    fn test_input_bit_becomes_ix_with_bit() {
        let (loc, _, w) = location_of(
            "task_valid",
            &TypeNode::Bit,
            PortDirection::In,
            &resolved(256, Some(0)),
            &model(),
        )
        .unwrap();
        assert_eq!(loc, "%IX256.0");
        assert!(w.is_empty(), "предупреждений быть не должно");
    }

    /// Выходной `bit` -> `%QX...`: класс берётся из направления порта.
    #[test]
    fn test_output_bit_becomes_qx() {
        let (loc, _, _) = location_of(
            "cmd_fork",
            &TypeNode::Bit,
            PortDirection::Out,
            &resolved(1280, Some(0)),
            &model(),
        )
        .unwrap();
        assert_eq!(loc, "%QX1280.0");
    }

    /// Адрес печатается десятичным: `0x` стандарт не допускает.
    #[test]
    fn test_address_is_decimal_not_hex() {
        let (loc, comment, _) = location_of(
            "p",
            &TypeNode::Bit,
            PortDirection::In,
            &resolved(256, Some(0)),
            &model(),
        )
        .unwrap();
        assert!(loc.contains("256"), "адрес обязан быть десятичным: {loc}");
        assert!(!loc.contains("0x"), "0x в локации недопустим: {loc}");
        // Но в комментарии исходная запись сохраняется - для наладки.
        assert!(
            comment.contains("0x100"),
            "нет исходного адреса в комментарии: {comment}"
        );
    }

    /// Размер - из `TypeNode`: `u8` -> `B`, `u16` -> `W`, `u32` -> `D`, `u64` -> `L`.
    #[test]
    fn test_size_letter_comes_from_takt_type() {
        let cases = [(8u8, "B"), (16, "W"), (32, "D"), (64, "L")];
        for (bits, letter) in cases {
            let ty = TypeNode::Integer {
                bits,
                signed: false,
            };
            let (loc, _, _) =
                location_of("p", &ty, PortDirection::In, &resolved(512, None), &model()).unwrap();
            assert_eq!(loc, format!("%I{}512", letter), "разрядность {bits}");
        }
    }

    /// Не-`BOOL` порт с битом: бит игнорируется, но **громко** - `ST-006`.
    ///
    /// Вход не гипотетический: `stacker.takt` систематически пишет `:0` даже для
    /// `u8`-портов (`in pos_stack: u8 at 0x200:0;`).
    #[test]
    fn test_non_bool_port_with_bit_warns_st006_and_ignores_bit() {
        let ty = TypeNode::Integer {
            bits: 8,
            signed: false,
        };
        let (loc, _, w) = location_of(
            "pos_stack",
            &ty,
            PortDirection::In,
            &resolved(512, Some(0)),
            &model(),
        )
        .unwrap();
        assert_eq!(loc, "%IB512", "у байтовой локации бита нет");
        assert_eq!(w.len(), 1, "игнорирование бита обязано быть громким");
        assert_eq!(w[0].code.as_deref(), Some("ST-006"));
    }

    /// `BOOL`-порт без бита печатает `.0` **молча**.
    ///
    /// Теперь позиция бита нормируется слоем адресов (`SE-090`), то есть до печати
    /// локации бита "нет" уже не бывает - предупреждать второй раз значило бы говорить
    /// об одном решении дважды. Сама ветка оставлена как защита от карты, пришедшей
    /// мимо нормировки, и обязана оставаться **тихой**: тест ловит возврат дубля.
    #[test]
    fn test_bool_port_without_bit_is_silent_after_normalisation() {
        let (loc, _, w) = location_of(
            "p",
            &TypeNode::Bool,
            PortDirection::In,
            &resolved(256, None),
            &model(),
        )
        .unwrap();
        assert_eq!(loc, "%IX256.0");
        assert!(
            w.is_empty(),
            "умолчание объявляет слой адресов (SE-090), а не печатник локации: {w:?}"
        );
    }

    /// `InOut` -> `%M`: "двунаправленной" локации в IEC нет.
    #[test]
    fn test_inout_port_uses_memory_class() {
        let (loc, _, _) = location_of(
            "p",
            &TypeNode::Bit,
            PortDirection::InOut,
            &resolved(16, Some(1)),
            &model(),
        )
        .unwrap();
        assert_eq!(loc, "%MX16.1");
    }

    /// Составной тип локации не имеет -> `ST-004`, а не выдумка.
    ///
    /// Бит-вектор `[bit;4]` составным типом не является: до слова это упакованный
    /// скаляр, и размещается он наравне с целым. Настоящий составной тип - массив
    /// целых: его цель разворачивает по листам либо отвергает, но локации у него нет.
    #[test]
    fn test_composite_port_has_no_location_st004() {
        let ty = TypeNode::Array(
            4,
            Box::new(TypeNode::Integer {
                bits: 8,
                signed: false,
            }),
        );
        let err = location_of(
            "arr",
            &ty,
            PortDirection::In,
            &resolved(768, None),
            &model(),
        )
        .expect_err("массив не размещается");
        assert_eq!(err.code.as_deref(), Some("ST-004"));
    }

    /// Отрицательный адрес -> `ST-004`: номер локации IEC неотрицателен.
    #[test]
    fn test_negative_address_is_st004() {
        let err = location_of(
            "p",
            &TypeNode::Bit,
            PortDirection::In,
            &resolved(-1, Some(0)),
            &model(),
        )
        .expect_err("отрицательный адрес обязан отвергаться");
        assert_eq!(err.code.as_deref(), Some("ST-004"));
    }
}
