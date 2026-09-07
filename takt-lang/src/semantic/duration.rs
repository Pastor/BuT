//! Профили времени и пересчёт длительности.
//!
//! **Единственное место арифметики времени в проекте** (анализа,
//! ). Ни один генератор и ни один слой симулятора своей
//! арифметики длительности не заводит: разъехавшись, они дали бы разное число
//! тактов для одного текста - ровно тот класс дефекта, который проект ловит
//! потактовыми сверками. Прецеденты общего слоя: `enum_facts` (разрядность
//! перечислений), `bit_vector` (упаковка `[bit;N]`), `lower_float`.
//!
//! ## Два профиля
//!
//! - **"Часы"** - умолчание. Длительность сравнивается с показаниями внешнего
//!   источника времени; частота **не нужна**, и модель остаётся годной для
//!   потактовой отладки. Квант - **1 мс** (ограничение реализации источника).
//! - **"Такты"** - включается объявлением частоты (`clock 1kHz;` в модели либо
//!   флагом `--tick-hz`, флаг переопределяет). Длительность пересчитывается в
//!   число тактов; источник времени не нужен. Квант - период такта.
//!
//! Иных способов выбора профиля нет: **частота задана -> такты, не задана -> часы**.
//!
//! ## Непредставимое - ошибка, а не округление
//!
//! `500us` в профиле "часы" и `500ms` при 3 Гц (1.5 такта) дают **`SE-063`**. Прецедент -
//! `SE-058` для fixed-point: непредставимый литерал `q(8,8) := 0.001` отвергается, а не
//! округляется. Округление здесь означало бы выдержку, не равную заявленной, - молча.

use crate::diagnostics::{Diagnostic, Location};

/// Квант профиля "часы" - миллисекунда, выраженная в наносекундах.
///
/// Это **ограничение реализации источника времени**, а не свойство языка: литерал
/// `250us` разбирается лексером всегда, но представим лишь в профиле "такты" с
/// достаточной частотой.
pub const CLOCK_QUANTUM_NS: i64 = 1_000_000;

/// Наносекунд в секунде.
const NANOS_PER_SECOND: i64 = 1_000_000_000;

/// Профиль времени, выбранный для компиляции.
#[derive(Copy, Clone, PartialEq, Eq, Debug, Default)]
pub enum TimeProfile {
    /// Внешний источник времени; длительность меряется миллисекундами.
    #[default]
    Clock,
    /// Счёт тактов с известной частотой.
    Ticks {
        /// Частота тактирования в герцах.
        hertz: u64,
    },
}

impl TimeProfile {
    /// Название профиля для сообщений (единая формулировка на весь проект).
    pub fn name(self) -> &'static str {
        match self {
            Self::Clock => "часы",
            Self::Ticks { .. } => "такты",
        }
    }

    /// Название единицы профиля в родительном падеже: "мс" либо "тактов".
    pub fn unit_name(self) -> &'static str {
        match self {
            Self::Clock => "мс",
            Self::Ticks { .. } => "тактов",
        }
    }
}

/// Причина отказа пересчёта.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum DurationError {
    /// Длительность не кратна кванту профиля (`SE-063`).
    NotRepresentable,
    /// Длительность не помещается в счётчик даже 64 бит (`SE-064`).
    TooLarge,
    /// Отрицательная длительность (в языке её нет; тест от чужой ошибки).
    Negative,
}

/// Пересчитывает длительность (наносекунды) в единицы профиля.
///
/// Профиль "часы" -> миллисекунды; профиль "такты" -> число тактов. Дробный результат -
/// **отказ** (`NotRepresentable`), а не округление:.
///
/// # Примеры
///
/// ```
/// use takt_lang::semantic::duration::{units, TimeProfile};
///
/// // 3 с в профиле "часы" - 3000 мс.
/// assert_eq!(units(3_000_000_000, TimeProfile::Clock), Ok(3_000));
/// // 3 с при 1 кГц - 3000 тактов.
/// assert_eq!(units(3_000_000_000, TimeProfile::Ticks { hertz: 1_000 }), Ok(3_000));
/// // 500 мс при 3 Гц - 1.5 такта: непредставимо.
/// assert!(units(500_000_000, TimeProfile::Ticks { hertz: 3 }).is_err());
/// ```
pub fn units(nanos: i64, profile: TimeProfile) -> Result<u64, DurationError> {
    if nanos < 0 {
        return Err(DurationError::Negative);
    }
    let quantum = match profile {
        TimeProfile::Clock => CLOCK_QUANTUM_NS,
        TimeProfile::Ticks { hertz } => {
            if hertz == 0 {
                return Err(DurationError::NotRepresentable);
            }
            // Период такта в наносекундах. Частота, не делящая секунду нацело (например
            // 3 Гц), даёт дробный период - тогда кратность проверяется по произведению,
            // а не по периоду: см. ветку ниже.
            let hertz = i64::try_from(hertz).map_err(|_| DurationError::TooLarge)?;
            if NANOS_PER_SECOND % hertz != 0 {
                // Точная проверка без потери: nanos·hertz должно делиться на 10⁹.
                let product = nanos.checked_mul(hertz).ok_or(DurationError::TooLarge)?;
                if product % NANOS_PER_SECOND != 0 {
                    return Err(DurationError::NotRepresentable);
                }
                return u64::try_from(product / NANOS_PER_SECOND)
                    .map_err(|_| DurationError::TooLarge);
            }
            NANOS_PER_SECOND / hertz
        }
    };
    if nanos % quantum != 0 {
        return Err(DurationError::NotRepresentable);
    }
    u64::try_from(nanos / quantum).map_err(|_| DurationError::TooLarge)
}

/// Длительность в **миллисекундах** - единица приведения `duration -> число`.
///
/// `as` над длительностью даёт миллисекунды. Единица одна на весь проект: и симулятор,
/// и цели обязаны звать эту функцию, иначе "сколько миллисекунд в этой выдержке"
/// получит два разных ответа.
///
/// **Остаток отбрасывается к нулю** (`250us as u32` -> `0`): `as` - явное приведение, и
/// потеря точности здесь выбор автора, как у `float -> int`. Неявного пути потерять
/// единицу нет: без `as` смешение с числом запрещено (`SE-065`).
pub fn to_millis(nanos: i64) -> i64 {
    nanos / CLOCK_QUANTUM_NS
}

/// Длительность из **миллисекунд** - обратное приведение `число -> duration`.
///
/// `None` - переполнение `i64` наносекунд (свыше ~292 лет): обёртки здесь быть не
/// должно, иначе выдержка молча стала бы другой.
pub fn from_millis(millis: i64) -> Option<i64> {
    millis.checked_mul(CLOCK_QUANTUM_NS)
}

/// Наносекунды -> литерал `TIME` языка IEC 61131-3 (`T#3000ms`) - цель `st`.
///
/// **Третье** место, где длительность превращается в текст (после единиц профиля
/// и приведения к числу), и оно обязано жить в общем слое,
/// а не форматироваться в генераторе. Форма - миллисекунды (`T#{ms}ms`): всегда
/// валидна для MatIEC и точна, ведь квант профиля "часы" - 1 мс. Отрицательная
/// длительность языком не порождается; тест - `TIME`-литерал берёт `abs`.
pub fn time_literal(nanos: i64) -> String {
    format!("T#{}ms", to_millis(nanos).abs())
}

/// Разрядность представления `duration` **как значения** в целях.
///
/// 32 бита в миллисекундах - 49.7 суток. Выбор: 64-битная арифметика на МК программная,
/// а 16 бит дали бы всего 65 секунд.
pub const VALUE_BITS: u8 = 32;

/// Пересчитывает длительность в **миллисекунды представления целей**.
///
/// Единица представления - та же, что у приведения `as` и у профиля "часы": тогда
/// граница "длительность ↔ число" не порождает арифметики и не может разойтись с
/// эталоном (драйвер 4 ).
///
/// # Ошибки
///
/// - `SE-063` - длительность не кратна миллисекунде (`250us` как **значение**
///   невыразим: округление до нуля было бы молчаливой ложью);
/// - `SE-064` - не влезает в [`VALUE_BITS`] бит.
///
/// Профиль времени здесь **не участвует**: значение живёт в миллисекундах независимо от
/// того, тактами или часами меряется выдержка. Профиль нужен лишь там, где значение
/// сравнивают со счётчиком выдержки.
pub fn value_millis(nanos: i64, loc: Location, what: &str) -> Result<u64, Diagnostic> {
    if nanos < 0 {
        return Err(Diagnostic::error(
            loc,
            format!("{what}: отрицательная длительность не представима"),
        )
        .with_code("SE-063"));
    }
    if nanos % CLOCK_QUANTUM_NS != 0 {
        return Err(Diagnostic::error(
            loc,
            format!(
                "{what}: длительность не кратна миллисекунде — значение типа \
                 duration представимо целым числом миллисекунд (округление молча \
                 изменило бы выдержку)"
            ),
        )
        .with_code("SE-063"));
    }
    let millis = u64::try_from(nanos / CLOCK_QUANTUM_NS).map_err(|_| {
        Diagnostic::error(loc, format!("{what}: длительность не представима")).with_code("SE-064")
    })?;
    if millis > u64::from(u32::MAX) {
        return Err(Diagnostic::error(
            loc,
            format!(
                "{what}: длительность больше {} мс — не помещается в {VALUE_BITS}-битное \
                 представление значения (около 49.7 суток)",
                u32::MAX
            ),
        )
        .with_code("SE-064"));
    }
    Ok(millis)
}

/// Множитель "миллисекунды -> такты" для **вычисляемой** выдержки.
///
/// У константной выдержки пересчёт делает компилятор ([`units`]), и его точность
/// проверяется `SE-063`. У вычисляемой значение известно лишь в такте, поэтому
/// сравнение приходится вести в тактах: `dwell >= expr_ms * (hertz / 1000)`. Множитель
/// обязан быть **целым** - иначе любой ответ был бы округлением, то есть выдержкой не
/// той, что заявлена.
///
/// `None` - профиль "часы": там выдержка меряется миллисекундами напрямую, и множитель
/// не нужен.
///
/// # Ошибки
///
/// `SE-073` - частота не кратна 1000 Гц.
pub fn ticks_per_milli(profile: TimeProfile, loc: Location) -> Result<Option<u64>, Diagnostic> {
    match profile {
        TimeProfile::Clock => Ok(None),
        TimeProfile::Ticks { hertz } if hertz % 1_000 == 0 => Ok(Some(hertz / 1_000)),
        TimeProfile::Ticks { hertz } => Err(Diagnostic::error(
            loc,
            format!(
                "вычисляемая выдержка требует частоты, кратной 1000 Гц: при {hertz} Гц \
                 миллисекунда — не целое число тактов, и сравнение округлялось бы молча. \
                 Либо задайте кратную частоту, либо оставьте выдержку константной \
                 (её компилятор пересчитывает точно)"
            ),
        )
        .with_code("SE-073")),
    }
}

/// Наименьшая разрядность счётчика (8/16/32/64), вмещающая `value`.
///
/// `None` - не вмещается даже в 64 бита (`SE-064`). Разрядность выбирает
/// **компилятор** (отчёта `DIFF.md`): счётчик, объявленный узко,
/// молча зациклил бы выдержку.
///
/// Сравнение "истекло ли" ведётся **разностью** беззнаковых счётчиков (`now - t0 >=
/// D`), поэтому обёртка не ломает выдержки короче полупериода счётчика - обёртка
/// беззнакового нормирована.
pub fn counter_bits(value: u64) -> Option<u8> {
    match value {
        v if v <= u64::from(u8::MAX) => Some(8),
        v if v <= u64::from(u16::MAX) => Some(16),
        v if v <= u64::from(u32::MAX) => Some(32),
        _ => Some(64),
    }
}

/// Пересчитывает длительность и превращает отказ в диагностику.
///
/// `what` описывает место (`константа 'DWELL'`) и попадает в сообщение: пачка
/// диагностик без указания предмета бесполезна.
pub fn units_or_diagnostic(
    nanos: i64,
    profile: TimeProfile,
    loc: Location,
    what: &str,
) -> Result<u64, Diagnostic> {
    units(nanos, profile).map_err(|error| match error {
        DurationError::NotRepresentable => {
            let detail = match profile {
                TimeProfile::Clock => {
                    "квант профиля «часы» — 1 мс; выразите длительность целым числом миллисекунд"
                        .to_string()
                }
                TimeProfile::Ticks { hertz } => format!(
                    "при частоте {hertz} Гц длительность не кратна периоду такта; \
                     выберите кратную длительность или другую частоту"
                ),
            };
            Diagnostic::error(
                loc,
                format!(
                    "{what}: длительность {nanos} нс непредставима в профиле «{}» — {detail}",
                    profile.name()
                ),
            )
            .with_code("SE-063")
        }
        DurationError::TooLarge => Diagnostic::error(
            loc,
            format!(
                "{what}: длительность {nanos} нс не помещается в счётчик времени \
                 (профиль «{}», максимум — 64 бита)",
                profile.name()
            ),
        )
        .with_code("SE-064"),
        DurationError::Negative => Diagnostic::error(
            loc,
            format!("{what}: отрицательная длительность ({nanos} нс) не имеет смысла"),
        )
        .with_code("SE-063"),
    })
}

/// Сверяет частоту: объявление `clock` - **контракт**, флаг обязан подтвердить.
///
/// Требование  развернуло прежнее ("флаг переопределяет объявление") в
/// правило **подтверждения**: если модель объявила частоту, сборка обязана передать
/// совпадающий `--tick-hz`. Выражено **одной** функцией намеренно: у карты адресов
/// приоритет источников размазался по слоям, и это стоило отдельной проработки.
///
/// | Объявление | Флаг | Итог |
/// |---|---|---|
/// | есть | нет | `Err` `SE-069` (частота не подтверждена) |
/// | есть | != | `Err` `SE-070` (частота не совпадает) |
/// | есть | = | `Ok(Ticks)` |
/// | нет | есть | `Ok(Ticks)` (автор частоту не ограничивал) |
/// | нет | нет | `Ok(Clock)` |
///
/// Проверка касается только **генерации кода**: симулятор частоту берёт из объявления
/// как период такта  и эту функцию не зовёт.
///
/// Позиция диагностики - [`Location::Implicit`]: контракт относится ко **всей** модели,
/// а не к строке; `ModelNode` местоположения `clock` не хранит (`mod.rs` пришпилен
/// реестром размеров - заводить поле нельзя).
pub fn resolve_profile(
    model_clock_hz: Option<u64>,
    flag_tick_hz: Option<u64>,
) -> Result<TimeProfile, Diagnostic> {
    match (model_clock_hz, flag_tick_hz) {
        (Some(declared), None) => Err(Diagnostic::error(
            Location::Implicit,
            format!("модель требует тактирования {declared} Гц; передайте --tick-hz={declared}"),
        )
        .with_code("SE-069")),
        (Some(declared), Some(flag)) if declared != flag => Err(Diagnostic::error(
            Location::Implicit,
            format!(
                "модель требует тактирования {declared} Гц, сборка задаёт {flag} Гц; \
                 приведите --tick-hz к объявленной частоте"
            ),
        )
        .with_code("SE-070")),
        // Объявление подтверждено флагом (declared == flag).
        (Some(hertz), Some(_)) => Ok(TimeProfile::Ticks { hertz }),
        // Автор частоту не ограничивал - сборка задаёт её флагом.
        (None, Some(hertz)) => Ok(TimeProfile::Ticks { hertz }),
        (None, None) => Ok(TimeProfile::Clock),
    }
}

/// Перечисляет выдержки `after` модели и то, во что каждая пересчиталась.
///
/// Требование (/): сборка обязана печатать, во что превратилась каждая длительность, -
/// иначе пользователь не видит, "столько же это тактов, сколько он думал". Возвращает
/// **предупреждения** (канал 0081, глушится `--quiet`), по одному на выдержку `after`
/// (профиль-зависимую); такты `after Nt` конверсии не требуют и здесь не перечисляются.
///
/// Непредставимая выдержка **пропускается**: её громкую ошибку (`SE-063`/`SE-064`) даст
/// кодоген - дублировать нечего. Обходит и вложенные модели композиции.
pub fn describe_durations(
    model: &crate::semantic::ModelNode,
    profile: TimeProfile,
) -> Vec<Diagnostic> {
    let mut out = Vec::new();
    collect_duration_notes(model, profile, &mut out);
    out
}

fn collect_duration_notes(
    model: &crate::semantic::ModelNode,
    profile: TimeProfile,
    out: &mut Vec<Diagnostic>,
) {
    let suffix = match profile {
        TimeProfile::Clock => String::new(),
        TimeProfile::Ticks { hertz } => format!(", {hertz} Гц"),
    };
    for state in model.states.values() {
        for reference in state.references() {
            if let crate::semantic::ConditionNode::After(nanos) = reference.cond
                && let Ok(units) = units(nanos, profile)
            {
                out.push(
                    Diagnostic::warning(
                        reference.location,
                        format!(
                            "выдержка after → {}: {} нс = {} {} (профиль «{}»{})",
                            reference.name,
                            nanos,
                            units,
                            profile.unit_name(),
                            profile.name(),
                            suffix,
                        ),
                    )
                    .with_code("SE-071"),
                );
            }
        }
    }
    for nested in model.models.values() {
        collect_duration_notes(&nested.borrow(), profile, out);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn clock_profile_counts_milliseconds() {
        assert_eq!(units(3_000_000_000, TimeProfile::Clock), Ok(3_000));
        assert_eq!(units(500_000_000, TimeProfile::Clock), Ok(500));
        assert_eq!(units(0, TimeProfile::Clock), Ok(0));
    }

    #[test]
    fn sub_millisecond_is_refused_in_clock_profile() {
        // `500us` - мельче кванта источника времени.
        assert_eq!(
            units(500_000, TimeProfile::Clock),
            Err(DurationError::NotRepresentable)
        );
        assert_eq!(
            units(40, TimeProfile::Clock),
            Err(DurationError::NotRepresentable)
        );
    }

    #[test]
    fn tick_profile_counts_ticks() {
        let khz = TimeProfile::Ticks { hertz: 1_000 };
        assert_eq!(units(3_000_000_000, khz), Ok(3_000));
        assert_eq!(units(1_000_000, khz), Ok(1));
        // 1 МГц: микросекунды представимы, в отличие от профиля "часы".
        let mhz = TimeProfile::Ticks { hertz: 1_000_000 };
        assert_eq!(units(500_000, mhz), Ok(500));
    }

    #[test]
    fn non_integral_tick_count_is_refused() {
        // Пример из: `500ms` при 3 Гц = 1.5 такта.
        assert_eq!(
            units(500_000_000, TimeProfile::Ticks { hertz: 3 }),
            Err(DurationError::NotRepresentable)
        );
        // Та же частота, но кратная длительность - принимается.
        assert_eq!(units(1_000_000_000, TimeProfile::Ticks { hertz: 3 }), Ok(3));
        // 1 такт при 3 Гц (⅓ с) записать точно нельзя - и это отказ.
        assert_eq!(
            units(333_333_333, TimeProfile::Ticks { hertz: 3 }),
            Err(DurationError::NotRepresentable)
        );
    }

    #[test]
    fn millis_conversion_is_the_single_bridge_to_numbers() {
        // `as` над длительностью даёт миллисекунды.
        assert_eq!(to_millis(3_000_000_000), 3_000);
        assert_eq!(to_millis(250_000_000), 250);
        // Остаток отбрасывается к нулю: `as` - явная потеря, выбор автора.
        assert_eq!(to_millis(500_000), 0);
        assert_eq!(to_millis(1_500_000), 1);
        assert_eq!(to_millis(-1_500_000), -1);
        // Обратно.
        assert_eq!(from_millis(250), Some(250_000_000));
        assert_eq!(from_millis(0), Some(0));
        assert_eq!(
            from_millis(i64::MAX),
            None,
            "переполнение — отказ, не обёртка"
        );
    }

    #[test]
    fn counter_width_is_the_narrowest_that_fits() {
        assert_eq!(counter_bits(255), Some(8));
        assert_eq!(counter_bits(256), Some(16));
        assert_eq!(counter_bits(65_535), Some(16));
        assert_eq!(counter_bits(65_536), Some(32));
        assert_eq!(counter_bits(u64::from(u32::MAX) + 1), Some(64));
    }

    #[test]
    fn profile_contract_confirms_or_rejects() {
        // Объявление есть, флаг не передан -> SE-069 (частота не подтверждена).
        let missing = resolve_profile(Some(1_000), None).expect_err("объявление требует флага");
        assert_eq!(missing.code.as_deref(), Some("SE-069"));
        assert!(
            missing.message.contains("1000") && missing.message.contains("--tick-hz"),
            "{missing:?}"
        );

        // Объявление есть, флаг не совпал -> SE-070 (частота не совпадает).
        let mismatch =
            resolve_profile(Some(1_000), Some(8_000_000)).expect_err("несовпадение — отказ");
        assert_eq!(mismatch.code.as_deref(), Some("SE-070"));
        assert!(
            mismatch.message.contains("1000") && mismatch.message.contains("8000000"),
            "{mismatch:?}"
        );

        // Объявление подтверждено флагом -> профиль "такты".
        assert_eq!(
            resolve_profile(Some(1_000), Some(1_000)),
            Ok(TimeProfile::Ticks { hertz: 1_000 })
        );
        // Автор частоту не ограничивал - сборка задаёт её флагом.
        assert_eq!(
            resolve_profile(None, Some(50)),
            Ok(TimeProfile::Ticks { hertz: 50 })
        );
        // Ни объявления, ни флага -> профиль "часы".
        assert_eq!(resolve_profile(None, None), Ok(TimeProfile::Clock));
    }

    #[test]
    fn diagnostic_names_the_place_and_the_reason() {
        let diagnostic = units_or_diagnostic(
            500_000,
            TimeProfile::Clock,
            Location::Implicit,
            "константа 'DWELL'",
        )
        .expect_err("500us в профиле «часы» непредставима");
        assert_eq!(diagnostic.code.as_deref(), Some("SE-063"));
        assert!(
            diagnostic.message.contains("константа 'DWELL'"),
            "{diagnostic:?}"
        );
        assert!(diagnostic.message.contains("1 мс"), "{diagnostic:?}");
    }
}
