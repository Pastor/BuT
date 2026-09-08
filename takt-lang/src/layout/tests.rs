//! Проверки графа модели: счёт по корпусу, ярусы, цитаты условий, реализация.

use super::*;
use std::path::PathBuf;

/// Каталог примеров рядом с крейтом.
fn examples_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("examples")
}

/// Все `.takt` каталога, отсортированные по имени.
fn takt_files(dir: &std::path::Path) -> Vec<PathBuf> {
    let mut files: Vec<PathBuf> = std::fs::read_dir(dir)
        .unwrap_or_else(|e| panic!("каталог {}: {e}", dir.display()))
        .filter_map(|entry| entry.ok().map(|e| e.path()))
        .filter(|path| path.extension().is_some_and(|ext| ext == "takt"))
        .collect();
    files.sort();
    files
}

/// Число состояний и переходов модели по АСД, по всем моделям файла.
fn counts_by_ast(model: &ast::Model) -> (usize, usize) {
    let mut states = 0;
    let mut edges = 0;
    for element in &model.elements {
        match element {
            ModelElement::State(state) => {
                states += 1;
                edges += state
                    .elements
                    .iter()
                    .filter(|e| matches!(e, StateElement::Reference(..) | StateElement::Next(_)))
                    .count();
            }
            ModelElement::Model(nested) => {
                let (s, e) = counts_by_ast(nested);
                states += s;
                edges += e;
            }
            _ => {}
        }
    }
    (states, edges)
}

fn sheet<'a>(graph: &'a Graph, path: &str) -> &'a Sheet {
    graph
        .sheets
        .iter()
        .find(|s| s.path == path)
        .unwrap_or_else(|| {
            panic!(
                "листа {path} нет: {:?}",
                graph.sheets.iter().map(|s| &s.path)
            )
        })
}

fn node<'a>(sheet: &'a Sheet, name: &str) -> &'a Node {
    sheet
        .nodes
        .iter()
        .find(|n| n.name == name)
        .unwrap_or_else(|| panic!("узла {name} нет на листе {}", sheet.path))
}

/// На всём корпусе примеров граф отвечает, счёт узлов и рёбер совпадает со счётом по
/// АСД, а повторный вызов даёт то же самое.
#[test]
fn corpus_graphs_count_states_and_edges_deterministically() {
    let root = examples_dir();
    let mut files = takt_files(&root);
    files.extend(takt_files(&root.join("language")));
    assert!(files.len() >= 10, "корпус усох: {} файлов", files.len());
    for path in files {
        let source = std::fs::read_to_string(&path).unwrap();
        let (model, _) =
            crate::parse(&source, 0).unwrap_or_else(|d| panic!("{}: {:?}", path.display(), d));
        let first = graph(&model, &source);
        let second = graph(&model, &source);
        assert_eq!(first, second, "{}: два вызова разошлись", path.display());
        let (states, edges) = counts_by_ast(&model);
        let got_states: usize = first.sheets.iter().map(|s| s.nodes.len()).sum();
        let got_edges: usize = first.sheets.iter().map(|s| s.edges.len()).sum();
        assert_eq!(got_states, states, "{}: узлы", path.display());
        assert_eq!(got_edges, edges, "{}: рёбра", path.display());
        assert_eq!(first.sheets[0].path, ROOT_PATH, "корень идёт первым");
    }
}

/// Лифт: два листа, у `Engine` пять узлов и семь рёбер, ярусы от `Idle` до `DoorOpening`.
#[test]
fn elevator_has_engine_sheet_with_ranks() {
    let source = std::fs::read_to_string(examples_dir().join("elevator.takt")).unwrap();
    let graph = graph_of(&source).expect("лифт разбирается");
    assert_eq!(
        graph
            .sheets
            .iter()
            .map(|s| s.path.as_str())
            .collect::<Vec<_>>(),
        vec![ROOT_PATH, "Engine"]
    );
    let engine = sheet(&graph, "Engine");
    assert_eq!(engine.name, "Engine");
    assert_eq!(engine.nodes.len(), 5);
    assert_eq!(engine.edges.len(), 7);
    assert_eq!(engine.start.as_deref(), Some("Idle"));
    assert_eq!(node(engine, "Idle").rank, 0);
    assert_eq!(node(engine, "DoorClosing").rank, 1);
    assert_eq!(node(engine, "MovingUp").rank, 2);
    assert_eq!(node(engine, "MovingDown").rank, 2);
    let last = engine.nodes.iter().map(|n| n.rank).max().unwrap();
    assert_eq!(
        node(engine, "DoorOpening").rank,
        last,
        "прибытие - последний ярус"
    );
    assert_eq!(node(engine, "Idle").kind, NodeKind::Start);
    assert_eq!(
        node(engine, "DoorOpening").kind,
        NodeKind::State,
        "тело `enter` и `next`"
    );
    // Ребро `next` безусловно, ребро `ref` несёт цитату условия.
    let next = engine
        .edges
        .iter()
        .find(|e| e.kind == EdgeKind::Next)
        .expect("у DoorOpening есть next");
    assert_eq!(
        (next.from.as_str(), next.to.as_str()),
        ("DoorOpening", "Idle")
    );
    assert_eq!(next.condition, None);
    let closing = engine
        .edges
        .iter()
        .find(|e| e.from == "Idle" && e.to == "DoorClosing")
        .unwrap();
    assert_eq!(closing.condition.as_deref(), Some("ShouldMove"));

    // Корень: `Main` и `Middle` - композиции, `End` - конец; цепочка `Middle` из пяти
    // шагов, второй шаг - две ветви, и все ссылаются на лист `Engine`.
    let root = sheet(&graph, ROOT_PATH);
    let main = node(root, "Main");
    assert_eq!(main.kind, NodeKind::Composition);
    assert!(main.start, "композиция остаётся стартовым состоянием");
    assert_eq!(node(root, "End").kind, NodeKind::End);
    let Some(Implement::Chain(steps)) = node(root, "Middle").implements.as_ref() else {
        panic!("Middle - цепочка: {:?}", node(root, "Middle").implements);
    };
    assert_eq!(steps.len(), 5);
    let Implement::Group(inner) = &steps[1] else {
        panic!("второй шаг в скобках: {:?}", steps[1]);
    };
    let Implement::Parallel(branches) = inner.as_ref() else {
        panic!("в скобках параллель: {inner:?}");
    };
    assert_eq!(branches.len(), 2);
    for branch in branches {
        assert_eq!(
            branch,
            &Implement::Model {
                name: "Engine".into(),
                path: Some("Engine".into()),
                loc: match branch {
                    Implement::Model { loc, .. } => *loc,
                    other => panic!("ветвь - модель: {other:?}"),
                },
            }
        );
    }
}

/// Самопереход, два ребра одной пары и недостижимое состояние.
#[test]
fn self_loop_ordinals_and_unreachable_rank() {
    let source = "var x: u8 := 0;\n\
                  start A {\n    ref A: x = 1;\n    ref B: x = 2;\n    ref B: x > 2;\n}\n\
                  state B {\n    next A;\n}\n\
                  state C {\n    ref A: x = 0;\n}\n";
    let graph = graph_of(source).unwrap();
    let root = sheet(&graph, ROOT_PATH);
    let loop_edge = root
        .edges
        .iter()
        .find(|e| e.from == "A" && e.to == "A")
        .unwrap();
    assert_eq!(loop_edge.ordinal, 0);
    assert_eq!(loop_edge.condition.as_deref(), Some("x = 1"));
    let to_b: Vec<u32> = root
        .edges
        .iter()
        .filter(|e| e.from == "A" && e.to == "B")
        .map(|e| e.ordinal)
        .collect();
    assert_eq!(to_b, vec![0, 1], "номер считается внутри пары");
    assert_eq!(node(root, "A").rank, 0);
    assert_eq!(node(root, "B").rank, 1);
    assert_eq!(
        node(root, "C").rank,
        2,
        "недостижимое - ниже всех достижимых"
    );
    assert_eq!(node(root, "C").kind, NodeKind::State);
}

/// Порядок внутри яруса: под предшественником, ничья - по имени.
#[test]
fn order_follows_predecessors_then_names() {
    let source = "var x: u8 := 0;\n\
                  start S {\n    ref Q: x = 1;\n    ref P: x = 2;\n}\n\
                  state P {\n    ref Pb: x = 3;\n    ref Pa: x = 4;\n}\n\
                  state Q {\n    ref Qa: x = 5;\n}\n\
                  state Pa;\nstate Pb;\nstate Qa;\n";
    let graph = graph_of(source).unwrap();
    let root = sheet(&graph, ROOT_PATH);
    assert_eq!((node(root, "P").rank, node(root, "P").order), (1, 0));
    assert_eq!((node(root, "Q").rank, node(root, "Q").order), (1, 1));
    // Дети `P` стоят перед ребёнком `Q`, между собой - по имени.
    assert_eq!(node(root, "Pa").order, 0);
    assert_eq!(node(root, "Pb").order, 1);
    assert_eq!(node(root, "Qa").order, 2);
    for name in ["Pa", "Pb", "Qa"] {
        assert_eq!(
            node(root, name).kind,
            NodeKind::End,
            "{name}: без рёбер и тела"
        );
    }
}

/// Условие цитируется из исходника: многострочная запись схлопывается в строку.
#[test]
fn condition_quote_is_taken_from_source() {
    let source = "var x: u8 := 0;\nvar y: bit := 0;\n\
                  start A {\n    ref B:\n        x > 3\n        & y;\n}\nstate B;\n";
    let graph = graph_of(source).unwrap();
    let edge = &sheet(&graph, ROOT_PATH).edges[0];
    assert_eq!(edge.condition.as_deref(), Some("x > 3 & y"));
    assert_eq!(edge.kind, EdgeKind::Ref);
    assert!(matches!(edge.loc, Location::Source(0, _, _)));
}

/// Тело `always` удерживает автомат: состояние без рёбер с телом - не конец.
#[test]
fn body_keeps_a_state_from_being_an_end() {
    let source = "var n: u8 := 0;\nstart Run {\n    always {\n        n := n + 1;\n    }\n}\n";
    let graph = graph_of(source).unwrap();
    let run = node(sheet(&graph, ROOT_PATH), "Run");
    assert_eq!(run.kind, NodeKind::Start);
    assert!(run.start);
    let source = "var n: u8 := 0;\nstart A {\n    ref B: n = 1;\n}\nstate B {\n    enter {\n        n := 0;\n    }\n}\n";
    let graph = graph_of(source).unwrap();
    assert_eq!(
        node(sheet(&graph, ROOT_PATH), "B").kind,
        NodeKind::End,
        "`enter` одноразов и не удерживает"
    );
}

/// Имя модели ищется вверх по вложенности; чужое имя остаётся без листа.
#[test]
fn implements_resolve_upwards_and_imports_stay_unresolved() {
    let source = "model Outer {\n    model Inner {\n        start I;\n    }\n    \
                  start O = Inner {\n        next Done;\n    }\n    state Done;\n}\n\
                  model Lib {\n    start L;\n}\n\
                  start Top = Outer + Lib + Foreign {\n    next Fin;\n}\nstate Fin;\n";
    let graph = graph_of(source).unwrap();
    assert_eq!(
        graph
            .sheets
            .iter()
            .map(|s| s.path.as_str())
            .collect::<Vec<_>>(),
        vec![ROOT_PATH, "Outer", "Outer/Inner", "Lib"]
    );
    let outer = sheet(&graph, "Outer");
    assert_eq!(
        node(outer, "O").implements,
        Some(Implement::Model {
            name: "Inner".into(),
            path: Some("Outer/Inner".into()),
            loc: match &node(outer, "O").implements {
                Some(Implement::Model { loc, .. }) => *loc,
                other => panic!("{other:?}"),
            },
        })
    );
    let Some(Implement::Chain(items)) = &node(sheet(&graph, ROOT_PATH), "Top").implements else {
        panic!("Top - цепочка");
    };
    let paths: Vec<Option<&str>> = items
        .iter()
        .map(|i| match i {
            Implement::Model { path, .. } => path.as_deref(),
            other => panic!("{other:?}"),
        })
        .collect();
    assert_eq!(paths, vec![Some("Outer"), Some("Lib"), None]);
    let next = &sheet(&graph, ROOT_PATH).edges[0];
    assert_eq!(next.kind, EdgeKind::Next);
    assert!(
        matches!(next.loc, Location::Source(0, _, _)),
        "у `next` позиция имени цели"
    );
}

/// Реализация уровня модели живёт у листа, а не у узла.
#[test]
fn model_level_implement_is_a_sheet_property() {
    let source = "model A {\n    start S;\n}\nmodel B {\n    start S;\n}\n\
                  model M = A | B {\n}\nstart Go = M;\n";
    let graph = graph_of(source).unwrap();
    let m = sheet(&graph, "M");
    assert!(m.nodes.is_empty());
    assert!(matches!(m.implements, Some(Implement::Parallel(ref items)) if items.len() == 2));
    assert_eq!(sheet(&graph, ROOT_PATH).implements, None);
}

/// Неразбираемый текст - диагностика, а не паника.
#[test]
fn unparsable_source_yields_a_diagnostic() {
    let error = graph_of("start S {").expect_err("незакрытая скобка");
    assert!(error.code.is_some(), "{error:?}");
    let empty = graph_of("").expect("пустой файл разбирается");
    assert_eq!(empty.sheets.len(), 1);
    assert!(empty.sheets[0].nodes.is_empty());
    assert_eq!(empty.sheets[0].start, None);
}
