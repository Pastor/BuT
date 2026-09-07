//! Литералы длительности и частоты.
//!
//! Две новые лексемы языка:
//!
//! - **длительность** - `3s`, `500ms`, составная `1m30s`, `2h30m`;
//! - **частота** - `1kHz`, `1000Hz`, `8MHz` (объявление `clock`).
//!
//! Модуль отвечает **только** за разбор единиц и перевод в каноническое представление:
//! длительность - целое число **наносекунд** (`i64`, запас ±292 года), частота -
//! **герцы** (`u64`). Сканирование по тексту и выдача токенов - в
//! [`lexer`](super::lexer); арифметика профилей ("длительность -> миллисекунды/такты") -
//! в семантическом слое, а не здесь (анализа: пересчёт живёт в одном
//! месте).
//!
//! Вынесено отдельным модулем не по стилю: `lexer.rs` - 881 строка при лимите 1000
//! (`scripts/check-module-size.sh`), и разбор единиц туда не помещается.

/// Единица длительности.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum TimeUnit {
    /// Наносекунда (`ns`).
    Nano,
    /// Микросекунда (`us`).
    Micro,
    /// Миллисекунда (`ms`).
    Milli,
    /// Секунда (`s`).
    Second,
    /// Минута (`m`).
    Minute,
    /// Час (`h`).
    Hour,
}

impl TimeUnit {
    /// Единица по её записи в исходном тексте (регистр значим).
    ///
    /// Регистр значим намеренно: `M` и `m` у времени различаются в 60 раз, а у частоты
    /// (`MHz`) - в миллион; молчаливое приравнивание дало бы ошибку, которую не видно в
    /// тексте.
    pub fn parse(word: &str) -> Option<Self> {
        match word {
            "ns" => Some(Self::Nano),
            "us" => Some(Self::Micro),
            "ms" => Some(Self::Milli),
            "s" => Some(Self::Second),
            "m" => Some(Self::Minute),
            "h" => Some(Self::Hour),
            _ => None,
        }
    }

    /// Сколько наносекунд в одной такой единице.
    pub fn nanos(self) -> i64 {
        match self {
            Self::Nano => 1,
            Self::Micro => 1_000,
            Self::Milli => 1_000_000,
            Self::Second => 1_000_000_000,
            Self::Minute => 60 * 1_000_000_000,
            Self::Hour => 3_600 * 1_000_000_000,
        }
    }

    /// Старшинство единицы: чем больше, тем крупнее.
    ///
    /// Нужно составной форме: в `1m30s` слагаемые обязаны идти **строго по убыванию**
    /// (`1s30m` - ошибка, а не "60 с + 30 мин").
    pub fn rank(self) -> u8 {
        match self {
            Self::Nano => 0,
            Self::Micro => 1,
            Self::Milli => 2,
            Self::Second => 3,
            Self::Minute => 4,
            Self::Hour => 5,
        }
    }
}

/// Единица частоты (`clock 1kHz;`).
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum FrequencyUnit {
    /// Герц (`Hz`).
    Hz,
    /// Килогерц (`kHz`).
    KHz,
    /// Мегагерц (`MHz`).
    MHz,
}

impl FrequencyUnit {
    /// Единица по её записи в исходном тексте (регистр значим).
    pub fn parse(word: &str) -> Option<Self> {
        match word {
            "Hz" => Some(Self::Hz),
            "kHz" => Some(Self::KHz),
            "MHz" => Some(Self::MHz),
            _ => None,
        }
    }

    /// Сколько герц в одной такой единице.
    pub fn hertz(self) -> u64 {
        match self {
            Self::Hz => 1,
            Self::KHz => 1_000,
            Self::MHz => 1_000_000,
        }
    }
}

/// Единица, распознанная за числом.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Suffix {
    /// Единица длительности.
    Time(TimeUnit),
    /// Единица частоты.
    Frequency(FrequencyUnit),
    /// **Такты** (`3t`) - выдержка в шагах логики, а не в физическом времени.
    ///
    /// Отдельная единица, а не длительность: такт в наносекундах не выражается, пока не
    /// известна частота, - и именно поэтому тактовая выдержка **не требует** её знать.
    Ticks,
}

/// Длина буквенного "хвоста" в начале `tail` (ASCII-буквы подряд).
///
/// Возвращает длину в байтах; единицы - ASCII, поэтому байты и символы совпадают.
pub fn alpha_run(tail: &str) -> usize {
    tail.bytes().take_while(u8::is_ascii_alphabetic).count()
}

/// Распознаёт единицу, стоящую сразу за числом: `(единица, длина в байтах)`.
///
/// Буквенный хвост сопоставляется **целиком**: `3msg` единицей не является (хвост
/// `msg`), и число с идентификатором остаётся тем, чем было, - то есть ошибкой разбора,
/// как и до фичи. Так исключено молчаливое "съедание" части имени.
pub fn scan_suffix(tail: &str) -> Option<(Suffix, usize)> {
    let len = alpha_run(tail);
    if len == 0 {
        return None;
    }
    let word = &tail[..len];
    if word == "t" {
        return Some((Suffix::Ticks, len));
    }
    if let Some(unit) = TimeUnit::parse(word) {
        return Some((Suffix::Time(unit), len));
    }
    FrequencyUnit::parse(word).map(|unit| (Suffix::Frequency(unit), len))
}

/// Значение слагаемого длительности в наносекундах.
///
/// `None` - переполнение `i64` (около 292 лет); вызывающий обязан дать лексическую
/// диагностику, а не обернуть значение молча.
pub fn term_nanos(value: i64, unit: TimeUnit) -> Option<i64> {
    value.checked_mul(unit.nanos())
}

/// Значение частоты в герцах; `None` - переполнение `u64` либо отрицательное.
pub fn frequency_hertz(value: i64, unit: FrequencyUnit) -> Option<u64> {
    u64::try_from(value).ok()?.checked_mul(unit.hertz())
}

/// Разобранный литерал времени.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Literal {
    /// Длительность в наносекундах.
    Duration(i64),
    /// Частота в герцах.
    Frequency(u64),
    /// Число тактов (`3t`).
    Ticks(i64),
}

/// Отказ разбора литерала времени.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum ScanError {
    /// Значение вне представимого диапазона (`i64` нс / `u64` Гц).
    OutOfRange,
    /// Слагаемые составной формы идут не по убыванию (`1s30m`).
    Order,
}

/// Читает литерал времени: целое `value` уже прочитано, `tail` - текст сразу за ним.
///
/// - `None` - единицы нет, литерал числовой (поведение до сохраняется);
/// - `Some(Ok((литерал, длина)))` - прочитано `длина` байт хвоста;
/// - `Some(Err(_))` - единица есть, но литерал неверен.
///
/// Составная форма (`1m30s`, `2h30m`) читается здесь же: слагаемые обязаны идти
/// **строго по убыванию** единиц. Без этого `1s30m` пришлось бы либо принимать
/// (и молча складывать), либо ловить позже - а позже уже неизвестно, что автор
/// написал одним литералом.
pub fn scan_literal(
    value: i64,
    tail: &str,
) -> Option<std::result::Result<(Literal, usize), ScanError>> {
    let (suffix, len) = scan_suffix(tail)?;
    let unit = match suffix {
        // Такты составной формы не имеют: `1t30t` бессмысленно.
        Suffix::Ticks => return Some(Ok((Literal::Ticks(value), len))),
        Suffix::Frequency(unit) => {
            return Some(
                frequency_hertz(value, unit).map_or(Err(ScanError::OutOfRange), |hz| {
                    Ok((Literal::Frequency(hz), len))
                }),
            );
        }
        Suffix::Time(unit) => unit,
    };

    let Some(mut total) = term_nanos(value, unit) else {
        return Some(Err(ScanError::OutOfRange));
    };
    let mut prev = unit;
    let mut pos = len;

    // Составная форма: за единицей снова идут цифры.
    while tail[pos..].starts_with(|c: char| c.is_ascii_digit()) {
        let digits = tail[pos..].bytes().take_while(u8::is_ascii_digit).count();
        let Ok(value) = tail[pos..pos + digits].parse::<i64>() else {
            return Some(Err(ScanError::OutOfRange));
        };
        pos += digits;
        let Some((Suffix::Time(unit), len)) = scan_suffix(&tail[pos..]) else {
            // За цифрами нет единицы длительности (`1m30`, `1m30Hz`) - литерал неполон;
            // это отказ, а не "прочитаем часть".
            return Some(Err(ScanError::Order));
        };
        if unit.rank() >= prev.rank() {
            return Some(Err(ScanError::Order));
        }
        let Some(term) = term_nanos(value, unit) else {
            return Some(Err(ScanError::OutOfRange));
        };
        let Some(sum) = total.checked_add(term) else {
            return Some(Err(ScanError::OutOfRange));
        };
        total = sum;
        prev = unit;
        pos += len;
    }

    Some(Ok((Literal::Duration(total), pos)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn units_parse_case_sensitively() {
        assert_eq!(TimeUnit::parse("ms"), Some(TimeUnit::Milli));
        assert_eq!(TimeUnit::parse("m"), Some(TimeUnit::Minute));
        // Регистр значим: `M` - не минута.
        assert_eq!(TimeUnit::parse("M"), None);
        assert_eq!(TimeUnit::parse("S"), None);
    }

    #[test]
    fn frequency_units_do_not_collide_with_time() {
        // `h` - час, `Hz` - герц: различаются хвостом целиком, а не первой буквой.
        assert_eq!(TimeUnit::parse("h"), Some(TimeUnit::Hour));
        assert_eq!(TimeUnit::parse("Hz"), None);
        assert_eq!(FrequencyUnit::parse("Hz"), Some(FrequencyUnit::Hz));
        assert_eq!(FrequencyUnit::parse("h"), None);
        assert_eq!(FrequencyUnit::parse("MHz"), Some(FrequencyUnit::MHz));
    }

    #[test]
    fn suffix_matches_whole_alpha_run() {
        assert_eq!(scan_suffix("ms"), Some((Suffix::Time(TimeUnit::Milli), 2)));
        // `msg` - не единица: имя не должно "объедаться" до `ms`.
        assert_eq!(scan_suffix("msg"), None);
        assert_eq!(scan_suffix("sec"), None);
        assert_eq!(scan_suffix(""), None);
        assert_eq!(scan_suffix("30s"), None);
    }

    #[test]
    fn nanos_are_canonical() {
        assert_eq!(term_nanos(3, TimeUnit::Second), Some(3_000_000_000));
        assert_eq!(term_nanos(500, TimeUnit::Milli), Some(500_000_000));
        assert_eq!(term_nanos(1, TimeUnit::Hour), Some(3_600_000_000_000));
    }

    #[test]
    fn overflow_is_reported_not_wrapped() {
        assert_eq!(term_nanos(i64::MAX, TimeUnit::Second), None);
        assert_eq!(frequency_hertz(-1, FrequencyUnit::Hz), None);
        assert_eq!(frequency_hertz(i64::MAX, FrequencyUnit::MHz), None);
    }

    #[test]
    fn simple_and_composite_forms() {
        assert_eq!(
            scan_literal(3, "s;"),
            Some(Ok((Literal::Duration(3_000_000_000), 1)))
        );
        assert_eq!(
            scan_literal(500, "ms;"),
            Some(Ok((Literal::Duration(500_000_000), 2)))
        );
        // 1m30s = 90 с; длина хвоста - 4 байта (`m30s`).
        assert_eq!(
            scan_literal(1, "m30s;"),
            Some(Ok((Literal::Duration(90_000_000_000), 4)))
        );
        assert_eq!(
            scan_literal(1, "kHz;"),
            Some(Ok((Literal::Frequency(1_000), 3)))
        );
    }

    #[test]
    fn composite_requires_descending_units() {
        // `1s30m` - не "полторы минуты", а отказ: порядок единиц значим.
        assert_eq!(scan_literal(1, "s30m;"), Some(Err(ScanError::Order)));
        // Повтор единицы тоже отказ (`1m30m`).
        assert_eq!(scan_literal(1, "m30m;"), Some(Err(ScanError::Order)));
        // Незавершённая форма: за цифрами нет единицы.
        assert_eq!(scan_literal(1, "m30;"), Some(Err(ScanError::Order)));
    }

    #[test]
    fn no_unit_leaves_number_alone() {
        // Поведение до сохраняется: за числом идентификатор - не наше дело.
        assert_eq!(scan_literal(3, "msg;"), None);
        assert_eq!(scan_literal(3, ";"), None);
        assert_eq!(scan_literal(3, " + 1"), None);
    }

    #[test]
    fn rank_orders_units_for_composite_form() {
        assert!(TimeUnit::Hour.rank() > TimeUnit::Minute.rank());
        assert!(TimeUnit::Minute.rank() > TimeUnit::Second.rank());
        assert!(TimeUnit::Milli.rank() > TimeUnit::Micro.rank());
    }
}
