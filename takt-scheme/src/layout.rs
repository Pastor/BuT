//! Файл раскладки `.takt-ui`: чтение.
//!
//! Форму и правила задаёт холст страницы (`web/static/layout.js`): формат 1, ключ
//! листа - путь модели, у листа композиции - `<путь>#<состояние>`, ключ ребра -
//! `<от>><к>:<номер>`. Читатель здесь так же терпим, как страничный: запись не той
//! формы отбрасывается молча - файл подсказка, и чужая ступень не вправе ни
//! рисоваться, ни ронять чертёж. Строг он в одном: версия формата - иная
//! версия означает иную форму, и чертёж по ней солгал бы.

use std::collections::BTreeMap;

use serde_json::Value;

use crate::geometry::{Corners, Crossing, ENTRY_PORT, LabelPlace, PORTS, Point};
use crate::js::round;

/// Версия формата файла раскладки.
pub const FORMAT: u64 = 1;

/// Ступени вида: имя настройки и допустимые значения, первое - умолчание (таблица -
/// у `VIEW` холста).
pub const VIEW: [(&str, &[&str]); 12] = [
    ("edgeWidth", &["thin", "normal", "bold"]),
    ("nodeWidth", &["normal", "thin", "bold"]),
    ("arrow", &["open", "solid", "line"]),
    ("stateFont", &["gost", "mono"]),
    ("stateSize", &["md", "sm", "lg"]),
    ("condFont", &["gost", "mono"]),
    ("condSize", &["sm", "xs", "md"]),
    ("gamma", &["color", "draft", "contrast"]),
    ("grid", &["medium", "small", "large", "off"]),
    ("snap", &["true", "false"]),
    ("marks", &["true", "false"]),
    ("crossing", &["hop", "gap"]),
];

/// Место легенды.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LegendPlace {
    Bottom,
    Right,
    Float,
}

/// Запись ребра листа.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct EdgeRecord {
    pub points: Vec<Point>,
    pub label: Option<LabelPlace>,
    pub end_from: Option<u32>,
    pub end_to: Option<u32>,
    pub name: Option<String>,
}

/// Запись листа.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct SheetRecord {
    /// Точка стрелки начального состояния; `None` - умолчание.
    pub entry: Option<u32>,
    pub nodes: BTreeMap<String, Point>,
    pub names: BTreeMap<String, String>,
    pub edges: BTreeMap<String, EdgeRecord>,
}

/// Раскладка схемы.
#[derive(Debug, Clone, PartialEq)]
pub struct Layout {
    pub corners: Corners,
    /// Умолчание места знака условия.
    pub label_place: LabelPlace,
    /// Ступени вида, отличные от умолчания.
    pub view: BTreeMap<String, String>,
    pub legend: LegendPlace,
    pub sheets: BTreeMap<String, SheetRecord>,
}

impl Default for Layout {
    fn default() -> Self {
        Self {
            corners: Corners::Square,
            label_place: LabelPlace::Center,
            view: BTreeMap::new(),
            legend: LegendPlace::Bottom,
            sheets: BTreeMap::new(),
        }
    }
}

impl Layout {
    /// Ступень вида: записанная либо умолчание.
    pub fn view_of(&self, key: &str) -> &str {
        self.view
            .get(key)
            .map(String::as_str)
            .or_else(|| {
                VIEW.iter()
                    .find(|(name, _)| *name == key)
                    .map(|(_, values)| values[0])
            })
            .unwrap_or_default()
    }

    /// Вид пересечения рёбер.
    pub fn crossing(&self) -> Crossing {
        if self.view_of("crossing") == "gap" {
            Crossing::Gap
        } else {
            Crossing::Hop
        }
    }

    /// Точка стрелки начального состояния листа.
    pub fn entry_of(&self, key: &str) -> u32 {
        self.sheets
            .get(key)
            .and_then(|s| s.entry)
            .unwrap_or(ENTRY_PORT)
    }
}

/// Ключ записи листа композиции: путь листа-владельца и имя составного состояния.
pub fn composition_key(path: &str, state: &str) -> String {
    format!("{path}#{state}")
}

/// Ключ ребра: `<от>><к>:<номер>`.
pub fn edge_key(from: &str, to: &str, ordinal: u32) -> String {
    format!("{from}>{to}:{ordinal}")
}

/// Отказ чтения файла раскладки: причина словами.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Unreadable(pub String);

/// Читает файл раскладки.
///
/// # Ошибки
/// Текст не JSON, не объект либо версия формата не та.
pub fn parse(text: &str) -> Result<Layout, Unreadable> {
    let raw: Value = serde_json::from_str(text)
        .map_err(|error| Unreadable(format!("файл раскладки не читается: {error}")))?;
    let Value::Object(raw) = raw else {
        return Err(Unreadable(
            "файл раскладки не читается: это не объект".into(),
        ));
    };
    let format = raw.get("format").and_then(Value::as_u64);
    if format != Some(FORMAT) {
        let shown = raw
            .get("format")
            .map_or_else(|| "нет".to_string(), Value::to_string);
        return Err(Unreadable(format!(
            "файл раскладки формата {shown}, а инструмент знает {FORMAT}"
        )));
    }
    let mut layout = Layout {
        corners: match raw.get("corners").and_then(Value::as_str) {
            Some("round") => Corners::Round,
            Some("bezier") => Corners::Bezier,
            _ => Corners::Square,
        },
        label_place: match raw.get("labelPlace").and_then(Value::as_str) {
            Some("start") => LabelPlace::Start,
            Some("end") => LabelPlace::End,
            _ => LabelPlace::Center,
        },
        legend: match raw
            .get("legend")
            .and_then(|l| l.get("place"))
            .and_then(Value::as_str)
        {
            Some("right") => LegendPlace::Right,
            Some("float") => LegendPlace::Float,
            _ => LegendPlace::Bottom,
        },
        ..Layout::default()
    };
    if let Some(Value::Object(view)) = raw.get("view") {
        for (key, values) in VIEW {
            let value = match view.get(key) {
                Some(Value::String(s)) => s.clone(),
                Some(Value::Bool(b)) => b.to_string(),
                _ => continue,
            };
            if values.contains(&value.as_str()) && value != values[0] {
                layout.view.insert(key.to_string(), value);
            }
        }
    }
    if let Some(Value::Object(sheets)) = raw.get("sheets") {
        for (path, stored) in sheets {
            if let Some(sheet) = sheet_record(stored) {
                layout.sheets.insert(path.clone(), sheet);
            }
        }
    }
    Ok(layout)
}

fn sheet_record(stored: &Value) -> Option<SheetRecord> {
    let Value::Object(stored) = stored else {
        return None;
    };
    let mut sheet = SheetRecord {
        entry: stored
            .get("entry")
            .and_then(port)
            .filter(|p| *p != ENTRY_PORT),
        ..SheetRecord::default()
    };
    if let Some(Value::Object(nodes)) = stored.get("nodes") {
        for (name, point) in nodes {
            if let (Some(x), Some(y)) = (number(point.get("x")), number(point.get("y"))) {
                sheet.nodes.insert(name.clone(), [round(x), round(y)]);
            }
        }
    }
    if let Some(Value::Object(names)) = stored.get("names") {
        for (name, alias) in names {
            if let Some(text) = alias.as_str().map(str::trim).filter(|t| !t.is_empty()) {
                sheet.names.insert(name.clone(), text.to_string());
            }
        }
    }
    if let Some(Value::Object(edges)) = stored.get("edges") {
        for (key, record) in edges {
            if parse_edge_key(key).is_none() || !record.is_object() {
                continue;
            }
            sheet.edges.insert(key.clone(), edge_record(record));
        }
    }
    Some(sheet)
}

fn edge_record(record: &Value) -> EdgeRecord {
    let points = record
        .get("points")
        .and_then(Value::as_array)
        .map(|list| {
            list.iter()
                .filter_map(|p| match p.as_array().map(Vec::as_slice) {
                    Some([x, y]) => Some([round(x.as_f64()?), round(y.as_f64()?)]),
                    _ => None,
                })
                .collect()
        })
        .unwrap_or_default();
    let label =
        record
            .get("label")
            .and_then(|label| match label.get("place").and_then(Value::as_str) {
                Some("start") => Some(LabelPlace::Start),
                Some("end") => Some(LabelPlace::End),
                Some("own") => Some(LabelPlace::Own(
                    round(number(label.get("x"))?),
                    round(number(label.get("y"))?),
                )),
                _ => None,
            });
    let ends = record.get("ends");
    EdgeRecord {
        points,
        label,
        end_from: ends.and_then(|e| e.get("from")).and_then(port),
        end_to: ends.and_then(|e| e.get("to")).and_then(port),
        name: record
            .get("name")
            .and_then(Value::as_str)
            .map(str::trim)
            .filter(|t| !t.is_empty())
            .map(str::to_string),
    }
}

/// Разбирает ключ ребра; `None` - ключ не той формы.
pub fn parse_edge_key(key: &str) -> Option<(&str, &str, u32)> {
    let (pair, ordinal) = key.rsplit_once(':')?;
    let ordinal: u32 = ordinal
        .parse()
        .ok()
        .filter(|_| ordinal.chars().all(|c| c.is_ascii_digit()))?;
    let (from, to) = pair.rsplit_once('>')?;
    (!from.is_empty() && !to.is_empty()).then_some((from, to, ordinal))
}

fn number(value: Option<&Value>) -> Option<f64> {
    value.and_then(Value::as_f64).filter(|v| v.is_finite())
}

fn port(value: &Value) -> Option<u32> {
    value
        .as_u64()
        .and_then(|p| u32::try_from(p).ok())
        .filter(|p| *p < PORTS)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_layout_is_read_leniently_but_its_format_strictly() {
        let text = r#"{
          "format": 1, "corners": "round", "labelPlace": "end",
          "view": { "crossing": "gap", "gamma": "draft", "edgeWidth": "чужая", "snap": false },
          "legend": { "place": "right" },
          "sheets": {
            "/": {
              "entry": 12,
              "nodes": { "A": { "x": 10.4, "y": 20.6 }, "Bad": { "x": "нет" } },
              "names": { "A": " Ожидание ", "Empty": "  " },
              "edges": {
                "A>B:0": { "points": [[1, 2.4]], "label": { "place": "own", "x": 7.2, "y": 8 }, "ends": { "from": 3, "to": 99 } },
                "битый": { "points": [[1, 1]] }
              }
            }
          }
        }"#;
        let layout = parse(text).expect("файл");
        assert_eq!(layout.corners, Corners::Round);
        assert_eq!(layout.label_place, LabelPlace::End);
        assert_eq!(layout.crossing(), Crossing::Gap);
        assert_eq!(layout.view_of("gamma"), "draft");
        assert_eq!(
            layout.view_of("edgeWidth"),
            "thin",
            "чужая ступень - умолчание"
        );
        assert_eq!(layout.view_of("snap"), "false");
        assert_eq!(layout.legend, LegendPlace::Right);
        let sheet = &layout.sheets["/"];
        assert_eq!(sheet.entry, Some(12));
        assert_eq!(sheet.nodes.get("A"), Some(&[10.0, 21.0]));
        assert!(!sheet.nodes.contains_key("Bad"));
        assert_eq!(sheet.names.get("A").map(String::as_str), Some("Ожидание"));
        assert!(!sheet.names.contains_key("Empty"));
        let edge = &sheet.edges["A>B:0"];
        assert_eq!(edge.points, vec![[1.0, 2.0]]);
        assert_eq!(edge.label, Some(LabelPlace::Own(7.0, 8.0)));
        assert_eq!(
            (edge.end_from, edge.end_to),
            (Some(3), None),
            "номер вне шестнадцати отброшен"
        );
        assert!(!sheet.edges.contains_key("битый"));

        assert!(
            parse(r#"{"format": 2}"#)
                .expect_err("формат")
                .0
                .contains('2')
        );
        assert!(parse("не json").is_err());
    }

    #[test]
    fn edge_keys_are_parsed_as_the_page_writes_them() {
        assert_eq!(parse_edge_key("A>B:0"), Some(("A", "B", 0)));
        assert_eq!(
            parse_edge_key("Heater#1>Pump#2:1"),
            Some(("Heater#1", "Pump#2", 1))
        );
        assert_eq!(parse_edge_key("A>B"), None);
        assert_eq!(parse_edge_key("A>B:-1"), None);
        assert_eq!(edge_key("A", "B", 3), "A>B:3");
        assert_eq!(composition_key("/", "Main"), "/#Main");
    }
}
