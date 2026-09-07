//! Обращение к bit-порту у цели `c`: один носитель на все места.
//!
//! # Контракт
//!
//! ```c
//! void (*write_bit)(Port port, uint8_t bit, bool val, void *userdata);
//! bool (*read_bit )(Port port, uint8_t bit, void *userdata);
//! ```
//!
//! Разряд принимают **все** bit-порты, а не только векторные: у скалярного `out lamp:
//! bit` он равен [`SCALAR_BIT`].
//!
//! Числовым порт `[bit;N]` быть не может: тогда разряд пишется чтением, правкой и
//! записью через `write_numeric`, а запись индексом (`heater[3] := 1`) печатается
//! индексацией результата чтения - `(*model->read_numeric)(PORT, ud)[3] = 1;`, чего не
//! собирает ни один компилятор C, при нулевом коде возврата `taktc`.

/// Разряд скалярного `bit`/`bool`-порта.
///
/// Именованная константа, а не `0` по месту: ноль здесь означает "порт из одного
/// разряда", и в семи местах он читался бы как случайная цифра.
pub(in crate::generator::c) const SCALAR_BIT: &str = "0";

/// Обращение К записи порта любой категории.
///
/// Одна воронка на три категории: развилка `match cls` жила в каждом из мест печати, и
/// добавление аргумента означало правку каждой ветви каждого места. Забытая ветвь даёт
/// вызов с другим числом аргументов - невалидный C при нулевом коде возврата.
pub(in crate::generator::c) fn write(
    cls: super::PortClass,
    ptr: &str,
    variant: &str,
    index: &str,
    value: &str,
) -> String {
    match cls {
        super::PortClass::Bit => write_bit(ptr, variant, index, value),
        super::PortClass::Rational => write_float(ptr, variant, index, value),
        super::PortClass::Numeric => write_numeric(ptr, variant, index, value),
    }
}

/// Обращение К чтению порта любой категории.
pub(in crate::generator::c) fn read(
    cls: super::PortClass,
    ptr: &str,
    variant: &str,
    index: &str,
) -> String {
    match cls {
        super::PortClass::Bit => read_bit(ptr, variant, index),
        super::PortClass::Rational => read_float(ptr, variant, index),
        super::PortClass::Numeric => read_numeric(ptr, variant, index),
    }
}

/// Индекс скалярного порта - того, у которого элемент один.
///
/// То же число, что [`SCALAR_BIT`], но другое понятие: у bit-порта это номер разряда, у
/// числового - номер элемента. Разные имена держат смысл на виду в семи местах печати.
pub(in crate::generator::c) const SCALAR_INDEX: &str = "0";

/// Запись элемента порта: `(*model->write_numeric)(PORT, индекс, значение, ud)`.
pub(in crate::generator::c) fn write_numeric(
    ptr: &str,
    variant: &str,
    index: &str,
    value: &str,
) -> String {
    format!(
        "(*{ptr}->{f})({variant}, {index}, {value}, {ptr}->userdata)",
        f = super::FUNCTION_PORT_WRITE_NUMERIC
    )
}

/// Чтение элемента порта: `(*model->read_numeric)(PORT, индекс, ud)`.
pub(in crate::generator::c) fn read_numeric(ptr: &str, variant: &str, index: &str) -> String {
    format!(
        "(*{ptr}->{f})({variant}, {index}, {ptr}->userdata)",
        f = super::FUNCTION_PORT_READ_NUMERIC
    )
}

/// Запись элемента вещественного порта.
pub(in crate::generator::c) fn write_float(
    ptr: &str,
    variant: &str,
    index: &str,
    value: &str,
) -> String {
    format!(
        "(*{ptr}->{f})({variant}, {index}, {value}, {ptr}->userdata)",
        f = super::FUNCTION_PORT_WRITE_FLOAT
    )
}

/// Чтение элемента вещественного порта.
pub(in crate::generator::c) fn read_float(ptr: &str, variant: &str, index: &str) -> String {
    format!(
        "(*{ptr}->{f})({variant}, {index}, {ptr}->userdata)",
        f = super::FUNCTION_PORT_READ_FLOAT
    )
}

/// Запись разряда порта: `(*model->write_bit)(PORT, разряд, значение, ud)`.
pub(in crate::generator::c) fn write_bit(
    ptr: &str,
    variant: &str,
    bit: &str,
    value: &str,
) -> String {
    format!(
        "(*{ptr}->{f})({variant}, {bit}, {value}, {ptr}->userdata)",
        f = super::FUNCTION_PORT_WRITE_BIT
    )
}

/// Чтение разряда порта: `(*model->read_bit)(PORT, разряд, ud)`.
pub(in crate::generator::c) fn read_bit(ptr: &str, variant: &str, bit: &str) -> String {
    format!(
        "(*{ptr}->{f})({variant}, {bit}, {ptr}->userdata)",
        f = super::FUNCTION_PORT_READ_BIT
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_bit_port_call_carries_the_bit_number() {
        // Предмет - Форма вызова: она обязана совпасть с сигнатурой, напечатанной в
        // заголовке. Разъедутся - `cc` ответит "too few arguments", а `taktc` промолчит
        // нулевым кодом возврата.
        assert_eq!(
            write_bit("model", "PROBE_PORT_HEATER", "3", "1"),
            "(*model->write_bit)(PROBE_PORT_HEATER, 3, 1, model->userdata)"
        );
        assert_eq!(
            read_bit("main", "PROBE_PORT_SRC", SCALAR_BIT),
            "(*main->read_bit)(PROBE_PORT_SRC, 0, main->userdata)"
        );
        // Скалярный порт - разряд (индекс) ноль, а не отсутствие аргумента: контракт
        // один на все порты.
        assert_eq!(SCALAR_BIT, "0");
        assert_eq!(SCALAR_INDEX, "0");
        assert_eq!(
            write_numeric("model", "PROBE_PORT_BUS", "model->i", "7"),
            "(*model->write_numeric)(PROBE_PORT_BUS, model->i, 7, model->userdata)"
        );
        // Индекс - Выражение, а не число: переменный индекс (`bus[i]`) при прежнем
        // устройстве не выражался вовсе - разворот порта по листам выбирал лист только
        // по литералу, а на переменной печатал индексацию несуществующего имени.
        assert_eq!(
            read_numeric("main", "PROBE_PORT_BUS", "main->i + 1"),
            "(*main->read_numeric)(PROBE_PORT_BUS, main->i + 1, main->userdata)"
        );
    }
}
