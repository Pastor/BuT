//! Применимость флага сборки к цели - один носитель.

/// Флаг, применимый не ко всякой цели.
struct Restricted {
    /// Как флаг записан автором - для текста отказа.
    flag: &'static str,
    /// Что флаг делает - вторая половина текста отказа.
    what: &'static str,
    /// Цели, которые флаг понимают.
    targets: &'static [&'static str],
}

/// Таблица ограниченных флагов.
const RESTRICTED: &[Restricted] = &[
    Restricted {
        flag: "--fsm=table",
        what: "Табличную форму автомата печатают цели",
        targets: &["c", "c-hal", "rust", "st", "st-at", "sv", "sv-mmio"],
    },
    Restricted {
        flag: "--bus=apb",
        what: "Адаптер шины печатает цель",
        targets: &["sv-mmio"],
    },
];

/// Проверяет применимость поднятых флагов к цели.
///
/// `raised` - какие из ограниченных флагов заданы (по имени из таблицы). Возвращает
/// готовый текст отказа либо `Ok(())`.
pub fn check(target: &str, raised: &[&str]) -> Result<(), String> {
    for flag in raised {
        let Some(entry) = RESTRICTED.iter().find(|e| e.flag == *flag) else {
            continue;
        };
        if entry.targets.contains(&target) {
            continue;
        }
        return Err(format!(
            "Ошибка: {} не поддерживается целью '{}'. {}: {}",
            entry.flag,
            target,
            entry.what,
            entry.targets.join(", ")
        ));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::check;

    #[test]
    fn table_form_is_refused_for_plantuml() {
        let err = check("plantuml", &["--fsm=table"]).unwrap_err();
        assert!(err.contains("--fsm=table"), "текст: {err}");
        assert!(err.contains("plantuml"), "текст: {err}");
    }

    #[test]
    fn bus_is_refused_for_every_target_but_sv_mmio() {
        for target in ["c", "c-hal", "rust", "st", "st-at", "sv", "plantuml"] {
            let err = check(target, &["--bus=apb"]).unwrap_err();
            assert!(err.contains("sv-mmio"), "цель {target}, текст: {err}");
        }
        assert!(check("sv-mmio", &["--bus=apb"]).is_ok());
    }

    #[test]
    fn unrestricted_flag_passes_anywhere() {
        assert!(check("plantuml", &[]).is_ok());
    }
}
