//! Пары "модель - сценарии" для страницы.
//!
//! Правило принадлежности сценария модели - у крейта проекта, тот же носитель, что у
//! командной строки и скрипта проектов примеров: своей копии правила у страницы нет.

use std::collections::BTreeMap;

use serde::Deserialize;

use crate::reply;

/// Запрос пар "модель - сценарии": имена файлов проекта.
#[derive(Debug, Deserialize)]
pub struct ScenariosRequest {
    names: Vec<String>,
}

/// Сценарии каждой модели по правилу принадлежности крейта проекта: из моделей,
/// подходящих по имени, сценарий достаётся самой длинной основе.
pub fn scenarios(request: &ScenariosRequest) -> String {
    let stem = |name: &str| takt_project::stem_of(name).map(|(s, k)| (s.to_string(), k));
    let models: Vec<String> = request
        .names
        .iter()
        .filter_map(|n| match stem(n) {
            Some((s, takt_project::Kind::Takt)) => Some(s),
            _ => None,
        })
        .collect();
    let stems: Vec<&str> = models.iter().map(String::as_str).collect();
    let scenario_names = request
        .names
        .iter()
        .filter(|n| matches!(stem(n), Some((_, takt_project::Kind::Scenario))));
    let pairs: BTreeMap<String, Vec<String>> = models
        .iter()
        .map(|model| {
            (
                format!("{model}.takt"),
                takt_project::scenarios_of(
                    model,
                    scenario_names.clone().map(String::as_str),
                    &stems,
                ),
            )
        })
        .collect();
    reply::ok(serde_json::json!({ "scenarios": pairs }))
}
