//! Форма графа для потребителя вне компилятора.
//!
//! Граф рисуют двое - страница онлайн-редактора и панель плагина, - а форма
//! ответа обязана быть общей: заведи её каждый у себя, и они разойдутся молча.
//! Здесь проверяется сама форма и то, что второго её носителя в дереве нет.

use takt_lang::layout;

/// Модель с двумя состояниями и переходом в обе стороны.
const MODEL: &str = r#"
in temperature: u8;
out heater: bit;

const HOT := 24;

start Heating {
    always { heater := 1; }
    ref Cooling: temperature >= HOT;
}

state Cooling {
    always { heater := 0; }
    ref Heating: temperature <= 20;
}
"#;

fn graph_value(source: &str) -> serde_json::Value {
    let graph = layout::graph_of(source).expect("модель разбирается");
    serde_json::to_value(layout::json::graph_json(graph, source)).expect("форма сериализуется")
}

#[test]
fn graph_json_carries_sheets_nodes_and_edges() {
    let value = graph_value(MODEL);
    let sheets = value["sheets"].as_array().expect("листы - массив");
    assert_eq!(sheets.len(), 1, "у модели без вложенных один лист");

    let nodes = sheets[0]["nodes"].as_array().expect("узлы - массив");
    let names: Vec<&str> = nodes.iter().map(|n| n["name"].as_str().unwrap()).collect();
    assert_eq!(
        names,
        vec!["Heating", "Cooling"],
        "порядок узлов - объявления"
    );
    assert_eq!(
        nodes[0]["start"],
        serde_json::json!(true),
        "стартовое состояние названо"
    );
    assert_eq!(sheets[0]["start"], serde_json::json!("Heating"));

    let edges = sheets[0]["edges"].as_array().expect("рёбра - массив");
    assert_eq!(edges.len(), 2, "два перехода");
    assert_eq!(edges[0]["from"], serde_json::json!("Heating"));
    assert_eq!(edges[0]["to"], serde_json::json!("Cooling"));
    assert!(
        edges[0]["condition"].as_str().unwrap().contains("HOT"),
        "цитата условия берётся из исходника"
    );
}

#[test]
fn graph_json_positions_are_lsp_ranges() {
    // Потребитель ставит курсор по этим числам: строки и колонки с нуля, как в
    // протоколе. Смещения байт увели бы курсор на кириллице.
    let value = graph_value(MODEL);
    let range = &value["sheets"][0]["nodes"][0]["name_range"];
    assert_eq!(
        range["start_line"],
        serde_json::json!(6),
        "строка объявления, с нуля"
    );
    assert_eq!(
        range["start_character"],
        serde_json::json!(6),
        "колонка имени"
    );
    assert_eq!(range["end_character"], serde_json::json!(13));
}

#[test]
fn graph_form_has_a_single_carrier() {
    // Второй носитель формы разошёлся бы с первым молча: страница и панель
    // нарисовали бы разные автоматы, а инструменты промолчали бы.
    let roots = [
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../takt-wasm/src"),
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("src/bin"),
    ];
    let mut found = Vec::new();
    for root in roots {
        let mut stack = vec![root];
        while let Some(dir) = stack.pop() {
            let Ok(entries) = std::fs::read_dir(&dir) else {
                continue;
            };
            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_dir() {
                    stack.push(path);
                    continue;
                }
                if path.extension().is_none_or(|ext| ext != "rs") {
                    continue;
                }
                let text = std::fs::read_to_string(&path).unwrap_or_default();
                for name in ["struct SheetJson", "struct NodeJson", "struct EdgeJson"] {
                    if text.contains(name) {
                        found.push(format!("{}: {name}", path.display()));
                    }
                }
            }
        }
    }
    assert!(
        found.is_empty(),
        "форма графа объявлена мимо общего носителя: {found:?}"
    );
}
