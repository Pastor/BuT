//! Граф модели для схемы: ответ `takt_lang::layout` в форме страницы.
//!
//! Формы здесь нет: её строит `takt_lang::layout::json`, и ту же форму берёт
//! сервер. Заведи модуль свою - панель плагина рисовала бы не тот автомат, что
//! страница, и расхождение пришло бы молча.

use takt_lang::layout::{self, json};

use crate::reply;

/// Граф модели по тексту.
///
/// Неразбираемый текст - отказ с диагностикой разбора, как у компиляции: страница
/// показывает пустой лист и диагностику, а не падает.
pub fn graph(source: &str) -> String {
    match layout::graph_of(source) {
        Ok(graph) => reply::ok(json::graph_json(graph, source)),
        Err(diagnostic) => reply::failed(&diagnostic, source),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::Value;

    const ELEVATOR: &str = include_str!("../../examples/elevator.takt");

    fn json(text: &str) -> Value {
        serde_json::from_str(text).expect("ответ моста - JSON")
    }

    /// Лифт: два листа, у `Engine` пять узлов и семь рёбер, позиции - диапазонами.
    #[test]
    fn elevator_graph_has_sheets_nodes_and_edges() {
        let reply = json(&graph(ELEVATOR));
        assert_eq!(reply["ok"], Value::Bool(true), "{reply}");
        let sheets = reply["sheets"].as_array().unwrap();
        assert_eq!(sheets.len(), 2);
        assert_eq!(sheets[0]["path"], "/");
        assert_eq!(sheets[1]["path"], "Engine");
        let engine = &sheets[1];
        assert_eq!(engine["nodes"].as_array().unwrap().len(), 5);
        assert_eq!(engine["edges"].as_array().unwrap().len(), 7);
        assert_eq!(engine["start"], "Idle");
        let idle = &engine["nodes"][0];
        assert_eq!(idle["name"], "Idle");
        assert_eq!(idle["kind"], "start");
        assert_eq!(idle["rank"], 0);
        assert!(idle["name_range"]["start_line"].is_number(), "{idle}");
        let middle = sheets[0]["nodes"]
            .as_array()
            .unwrap()
            .iter()
            .find(|n| n["name"] == "Middle")
            .unwrap();
        assert_eq!(middle["kind"], "composition");
        assert_eq!(middle["implements"]["chain"].as_array().unwrap().len(), 5);
        assert_eq!(
            middle["implements"]["chain"][1]["group"]["parallel"]
                .as_array()
                .unwrap()
                .len(),
            2
        );
        assert_eq!(middle["implements"]["chain"][0]["model"]["path"], "Engine");
    }

    /// Неразбираемый текст - отказ с диагностикой, не паника; повтор даёт тот же ответ.
    #[test]
    fn broken_source_is_a_diagnostic_and_answers_repeat() {
        let reply = json(&graph("start S {"));
        assert_eq!(reply["ok"], Value::Bool(false), "{reply}");
        assert!(reply["error"]["code"].is_string(), "{reply}");
        assert_eq!(graph(ELEVATOR), graph(ELEVATOR), "ответ детерминирован");
    }
}
