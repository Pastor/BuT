//! Форма записи числового литерала при форматировании.
//!
//! # Правило
//!
//! Число печатается **как написано**, если запись доступна и означает то же значение.
//! Это тот же принцип, по которому уже печатается литерал длительности (`1m30s` не
//! канонизируется в `90s`) и по которому не канонизируются синонимы `while`/`loop`.
//!
//! # Устройство
//!
//! Исходник кладётся в **потоковый** носитель на время форматирования - как носитель
//! позиции отказа у целей. Причина та же: печатники выражений зовутся из двух десятков
//! мест, и протаскивание исходника через все сигнатуры стоило бы дороже, чем даёт.
//!
//! **Сброс обязателен** (`reset`): состояние переживает вызов, и без сброса следующий
//! файл печатался бы по чужому исходнику.
//!
//! **Значение проверяется** (`same_value`): подстрока берётся по позиции узла, и
//! разойдись позиция с текстом - форматтер напечатал бы чужое число. При несовпадении
//! печатается десятичное представление, то есть прежнее поведение. Молчаливой порчи
//! этот путь не допускает **по построению**.

use std::cell::RefCell;

use crate::diagnostics::Location;

thread_local! {
    /// Исходник форматируемого файла; `None` - вне форматирования.
    static SOURCE: RefCell<Option<String>> = const { RefCell::new(None) };
}

/// Запоминает исходник на время форматирования.
pub(crate) fn set(source: &str) {
    SOURCE.with(|cell| *cell.borrow_mut() = Some(source.to_string()));
}

/// Забывает исходник. Зовётся в конце форматирования - **всегда**.
pub(crate) fn reset() {
    SOURCE.with(|cell| *cell.borrow_mut() = None);
}

/// Печать числа: запись автора, если она означает то же значение.
pub(crate) fn number(loc: Location, value: i128) -> String {
    text_at(loc)
        .filter(|written| same_value(written, value))
        .unwrap_or_else(|| value.to_string())
}

/// Текст исходника на месте узла.
fn text_at(loc: Location) -> Option<String> {
    let Location::Source(_, from, to) = loc else {
        return None;
    };
    let (from, to) = (usize::try_from(from).ok()?, usize::try_from(to).ok()?);
    SOURCE.with(|cell| {
        let borrowed = cell.borrow();
        let source = borrowed.as_ref()?;
        source.get(from..to).map(str::to_string)
    })
}

/// Означает ли запись то же значение.
///
/// Разбор повторяет лексер в одном: разделитель разрядов `_` не значим. Прочие формы
/// (рациональное, длительность) сюда не попадают - у них свои узлы АСД и своя печать.
fn same_value(written: &str, value: i128) -> bool {
    let cleaned: String = written.chars().filter(|&c| c != '_').collect();
    let parsed = match cleaned
        .strip_prefix("0x")
        .or_else(|| cleaned.strip_prefix("0X"))
    {
        Some(hex) => i128::from_str_radix(hex, 16).ok(),
        None => cleaned.parse::<i128>().ok(),
    };
    parsed == Some(value)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Запись автора возвращается, когда означает то же значение.
    ///
    /// Позиция считается поиском, а не вписывается числом: смещение, подобранное на
    /// глаз, проверяло бы арифметику теста, а не правило.
    #[test]
    fn keeps_written_form() {
        let source = "const M: u8 := 0xF0;";
        let at = source.find("0xF0").expect("литерал в пробе");
        set(source);
        let loc = Location::source(0, at, at + "0xF0".len());
        assert_eq!(number(loc, 240), "0xF0");
        reset();
    }

    /// Разошлась позиция - печатается десятичное, а не чужой текст.
    #[test]
    fn falls_back_when_text_means_other_value() {
        set("const M: u8 := 0xF0;");
        // Позиция указывает на `const` - значение не совпадает.
        let loc = Location::source(0, 0, 5);
        assert_eq!(number(loc, 240), "240");
        reset();
    }

    /// Вне форматирования носитель пуст, и печать прежняя.
    #[test]
    fn without_source_prints_decimal() {
        reset();
        assert_eq!(number(Location::source(0, 0, 4), 240), "240");
    }
}
