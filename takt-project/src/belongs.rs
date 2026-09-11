//! Какой модели принадлежит сценарий.
//!
//! # Правило
//!
//! Сценарий `S.json` принадлежит модели с основой `M`, если `S` равно `M` либо
//! начинается с `M_`; из подходящих моделей побеждает **самая длинная** основа.
//! Иначе у проекта с моделями `elevator` и `elevator_mini` сценарий
//! `elevator_mini_floor2.json` достался бы обеим, а прогон `elevator` пошёл бы по
//! чужим входам.

/// Модель, которой принадлежит сценарий: самая длинная подходящая основа.
///
/// # Аргументы
/// - `scenario` - имя файла сценария (`elevator_mini_floor2.json`);
/// - `models` - основы моделей проекта (`elevator`, `elevator_mini`).
pub fn owner_of<'a>(scenario: &str, models: impl IntoIterator<Item = &'a str>) -> Option<&'a str> {
    let stem = scenario.strip_suffix(".json")?;
    models
        .into_iter()
        .filter(|model| fits(stem, model))
        .max_by_key(|model| model.len())
}

/// Сценарии модели по правилу принадлежности, по алфавиту.
///
/// # Аргументы
/// - `model` - основа модели;
/// - `scenarios` - имена файлов сценариев проекта;
/// - `models` - основы всех моделей проекта: у кого из них основа длиннее, тот
///   забирает свои сценарии.
pub fn scenarios_of<'a>(
    model: &str,
    scenarios: impl IntoIterator<Item = &'a str>,
    models: &[&str],
) -> Vec<String> {
    let mut out: Vec<String> = scenarios
        .into_iter()
        .filter(|name| owner_of(name, models.iter().copied()) == Some(model))
        .map(str::to_string)
        .collect();
    out.sort();
    out
}

fn fits(stem: &str, model: &str) -> bool {
    !model.is_empty()
        && (stem == model
            || stem
                .strip_prefix(model)
                .is_some_and(|rest| rest.starts_with('_')))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_longest_model_takes_its_scenarios() {
        let models = ["elevator", "elevator_mini"];
        assert_eq!(
            owner_of("elevator_mini_floor2.json", models),
            Some("elevator_mini")
        );
        assert_eq!(
            owner_of("elevator_mini.json", models),
            Some("elevator_mini")
        );
        assert_eq!(owner_of("elevator.json", models), Some("elevator"));
        assert_eq!(owner_of("elevator_rush.json", models), Some("elevator"));
    }

    #[test]
    fn a_prefix_without_an_underscore_does_not_count() {
        // `heaterx.json` - не сценарий `heater`: граница имени - подчёркивание.
        assert_eq!(owner_of("heaterx.json", ["heater"]), None);
        assert_eq!(owner_of("heater.md", ["heater"]), None, "не сценарий");
        assert_eq!(owner_of("orphan.json", ["heater"]), None);
    }

    #[test]
    fn scenarios_of_a_model_are_sorted_and_exclusive() {
        let scenarios = [
            "elevator_mini_floor2.json",
            "elevator_b.json",
            "elevator_a.json",
            "pump.json",
        ];
        let models = ["elevator", "elevator_mini", "pump"];
        assert_eq!(
            scenarios_of("elevator", scenarios, &models),
            ["elevator_a.json", "elevator_b.json"]
        );
        assert_eq!(
            scenarios_of("elevator_mini", scenarios, &models),
            ["elevator_mini_floor2.json"]
        );
    }
}
