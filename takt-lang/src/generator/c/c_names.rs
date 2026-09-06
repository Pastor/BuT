//! Имена перечислителей цели `c`.

use crate::semantic::minimap::Name;
use crate::semantic::naming::normalize_lowercase_snakecase;

/// Имя перечислителя порта: `<МОДЕЛЬ>_PORT_<ПОРТ>`.
///
/// Сегмент `PORT_` разводит пространство портов с пространством состояний, которое
/// печатается как `<МОДЕЛЬ>_<СОСТОЯНИЕ>`.
pub(in crate::generator::c) fn port_enum_variant(
    model_name: &Name,
    port: &str,
    declared: crate::parser::ast::PortDirection,
    side: crate::parser::ast::PortDirection,
) -> String {
    use crate::parser::ast::PortDirection;
    // Двунаправленный порт попадает в оба перечисления (`_In_...` и `_Out_...`), а
    // перечислители в C делят одну область видимости - имя обязано их различать.
    //
    // Сегмент печатается только двунаправленному порту: имена портов видны пользователю
    // (сигнатура HAL-колбэка), и смена формы у однонаправленных была бы ломающей без
    // нужды.
    let suffix = match declared {
        PortDirection::InOut if side == PortDirection::In => "_IN",
        PortDirection::InOut => "_OUT",
        _ => "",
    };
    format!(
        "{}_PORT_{}{}",
        model_name.unique_uppercase_snakecase(),
        normalize_lowercase_snakecase(port.to_string()).to_uppercase(),
        suffix
    )
}

/// Имя константы перечисления: `ENUM_<МОДЕЛЬ>_<ПЕРЕЧИСЛЕНИЕ>_<ВАРИАНТ>`.
pub(in crate::generator::c) fn enum_constant(
    model_name: &Name,
    enum_name: &str,
    variant: &str,
) -> String {
    format!(
        "ENUM_{}_{}_{}",
        model_name.unique_uppercase_snakecase(),
        normalize_lowercase_snakecase(enum_name.to_string()).to_uppercase(),
        normalize_lowercase_snakecase(variant.to_string()).to_uppercase()
    )
}
