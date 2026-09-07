//! Генератор диаграмм состояний PlantUML из семантического дерева Takt.
//!
//! Модуль транслирует семантическое дерево [`ModelNode`] в файл `.puml` с диаграммой
//! состояний в формате PlantUML.
//!
//! ## Формат вывода
//!
//! - Корневая модель: начальный переход `[*] --> <start>` и переходы между состояниями.
//! - Подмодели (из `StateExtend`): отдельные блоки `state <Name> { ... }`.
//! - Аннотации к составным состояниям: `StateName : A | B` или `StateName : A + B`.
//! - Терминальные состояния (нет исходящих переходов): `State --> [*]`.

mod puml_map;

use crate::diagnostics::{Diagnostic, Location};
use crate::generator::Generator as AsGenerator;
use crate::generator::header::{CommentStyle, file_header};
use crate::generator::{GenerateOptions, GeneratedFile, Output};
use crate::semantic::ModelNode;
use crate::semantic::minimap::{Element, StateExtend};
use crate::semantic::naming::normalize_lowercase_snakecase;
use puml_map::PumlMap;

/// Генератор PlantUML-диаграмм для модели Takt.
pub struct Generator {}

impl AsGenerator for Generator {
    fn generate_texts(
        &self,
        model: &ModelNode,
        _options: &GenerateOptions,
    ) -> Result<Output, Diagnostic> {
        let map = PumlMap::new(
            &normalize_lowercase_snakecase(model.name().to_string()),
            model,
        )?;
        let diagram = generate_diagram(&map)?;
        let filename = map.get_filename();
        Ok(Output {
            files: vec![GeneratedFile {
                name: filename.to_owned() + ".puml",
                text: diagram,
            }],
            // Предупреждений у цели `plantuml` нет - канал общий.
            warnings: Vec::new(),
        })
    }

    fn write_failure(&self, error: &std::io::Error) -> Diagnostic {
        Diagnostic::warning(Location::Codegen, format!("{error}")).with_code("PU-001")
    }
}

/// Возвращает текстовую аннотацию для выражения расширения состояния.
///
/// - `Model(A)` -> `"A"`
/// - `Parallel([A, B])` -> `"A | B"`
/// - `Concatenation([A, B])` -> `"A + B"`
fn extend_annotation(extend: &StateExtend) -> String {
    match extend {
        StateExtend::None => String::new(),
        StateExtend::Model(name, _) => name.local().to_string(),
        StateExtend::Concatenation(items) => items
            .iter()
            .map(extend_annotation)
            .collect::<Vec<_>>()
            .join(" + "),
        StateExtend::Parallel(items) => items
            .iter()
            .map(extend_annotation)
            .collect::<Vec<_>>()
            .join(" | "),
    }
}

/// Записывает в `out` переходы для одного элемента состояния с заданным `indent`.
///
/// - [`Element::State`]: стрелки к каждому ref; если ref нет - терминальная стрелка `[*]`.
/// - [`Element::StateExtend`]: аннотация расширения (если есть) + стрелка к `next` или `[*]`.
fn write_state_transitions(element: &Element, out: &mut String, indent: &str) {
    match element {
        Element::State {
            name,
            references,
            terminal,
        } => {
            // Стрелка в `[*]` - только у терминального состояния: состояние с телом и
            // без рёбер работает вечно, и рисовать ему конец значило бы показывать
            // автомат, которого нет.
            if *terminal {
                out.push_str(&format!("{}{} --> [*]\n", indent, name.local()));
            } else {
                for r in references {
                    out.push_str(&format!("{}{} --> {}\n", indent, name.local(), r.local()));
                }
            }
        }
        Element::StateExtend { name, extend, next } => {
            let annotation = extend_annotation(extend);
            if !annotation.is_empty() {
                out.push_str(&format!("{}{} : {}\n", indent, name.local(), annotation));
            }
            if next.local().is_empty() {
                out.push_str(&format!("{}{} --> [*]\n", indent, name.local()));
            } else {
                out.push_str(&format!(
                    "{}{} --> {}\n",
                    indent,
                    name.local(),
                    next.local()
                ));
            }
        }
        Element::Model { .. } => {}
    }
}

/// Генерирует текст PlantUML-диаграммы состояний для переданного снимка модели.
fn generate_diagram(map: &PumlMap) -> Result<String, Diagnostic> {
    let mut out = String::new();

    // Шапка стоит после `@startuml`, а не перед ним. PlantUML читает только то, что
    // лежит между `@startuml` и `@enduml`, - текст снаружи он игнорирует, и шапка перед
    // открывающей строкой не была бы частью диаграммы: вставив её вместе с блоком в
    // другой файл, читатель потерял бы предупреждение. Внутри блока `'` в начале строки -
    // комментарий языка.
    //
    // Выбор сделан по правилу языка, а не прогоном: арбитра `plantuml` в проекте нет -
    // проверка цели судит генерацию, но не отрисовку. Это названная граница.
    out.push_str("@startuml\n");
    for line in file_header("PlantUML state diagram", CommentStyle::Quote) {
        out.push_str(&line);
        out.push('\n');
    }
    out.push('\n');
    out.push_str(&format!("title {}\n\n", map.root_name().unique_camelcase()));

    let Element::Model { start, .. } = map.model() else {
        unreachable!()
    };
    out.push_str(&format!("[*] --> {}\n", start.local()));

    for state_name in map.states() {
        if let Some(element) = map.state_at(state_name) {
            write_state_transitions(&element, &mut out, "");
        }
    }

    for model_elem in map.using_models() {
        let Element::Model {
            name,
            states,
            start,
        } = &model_elem
        else {
            continue;
        };
        out.push('\n');
        out.push_str(&format!("state {} {{\n", name.local()));
        out.push_str(&format!("    [*] --> {}\n", start.local()));
        for state_name in states {
            if let Some(element) = map.state_at(state_name.clone()) {
                write_state_transitions(&element, &mut out, "    ");
            }
        }
        out.push_str("}\n");
    }

    out.push_str("\n@enduml\n");

    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse;
    use crate::semantic::tree::construct_model;

    fn make_map(src: &str, name: &str) -> PumlMap {
        let (ast, _) = parse(src, 0).unwrap();
        let model_rc = construct_model(&ast, None, &[]).unwrap();
        model_rc.borrow_mut().name = Some(name.to_string());
        let model = model_rc.borrow();
        PumlMap::new(name, &model).unwrap()
    }

    /// Любая модель должна содержать маркеры начала и конца PlantUML.
    #[test]
    fn test_generate_diagram_any_model_contains_startuml_and_enduml() {
        let map = make_map("start S;", "Root");
        let diagram = generate_diagram(&map).unwrap();
        assert!(
            diagram.contains("@startuml"),
            "отсутствует @startuml:\n{diagram}"
        );
        assert!(
            diagram.contains("@enduml"),
            "отсутствует @enduml:\n{diagram}"
        );
    }

    /// Начальное состояние должно получить переход от псевдосостояния `[*]`.
    #[test]
    fn test_generate_diagram_start_state_has_initial_transition() {
        let map = make_map("start S;", "Root");
        let diagram = generate_diagram(&map).unwrap();
        assert!(
            diagram.contains("[*] --> S"),
            "отсутствует начальный переход:\n{diagram}"
        );
    }

    /// Состояние без исходящих переходов должно получить терминальную стрелку `[*]`.
    #[test]
    fn test_generate_diagram_state_without_refs_generates_terminal_arrow() {
        let map = make_map("start S;", "Root");
        let diagram = generate_diagram(&map).unwrap();
        assert!(
            diagram.contains("S --> [*]"),
            "отсутствует терминальный переход:\n{diagram}"
        );
    }

    /// Переход `ref` между состояниями должен генерировать стрелку `A --> B`.
    #[test]
    fn test_generate_diagram_ref_transition_generates_arrow() {
        let map = make_map("start S { ref T; } state T;", "Root");
        let diagram = generate_diagram(&map).unwrap();
        assert!(
            diagram.contains("S --> T"),
            "отсутствует переход S --> T:\n{diagram}"
        );
        assert!(
            diagram.contains("T --> [*]"),
            "T должен быть терминальным:\n{diagram}"
        );
    }

    /// Подмодель, используемая через `StateExtend`, должна порождать отдельный `state`
    /// блок.
    #[test]
    fn test_generate_diagram_submodel_generates_state_block() {
        let src = "model A { start S; } start Entry = A;";
        let map = make_map(src, "Root");
        let diagram = generate_diagram(&map).unwrap();
        assert!(
            diagram.contains("state A {"),
            "отсутствует блок подмодели:\n{diagram}"
        );
        assert!(
            diagram.contains("[*] --> S"),
            "отсутствует начальный переход подмодели:\n{diagram}"
        );
    }

    /// Состояние с `StateExtend::Model` должно получить аннотацию с именем модели.
    #[test]
    fn test_generate_diagram_single_extend_generates_annotation() {
        let src = "model A { start S; } start E = A;";
        let map = make_map(src, "Root");
        let diagram = generate_diagram(&map).unwrap();
        assert!(
            diagram.contains("E : A"),
            "отсутствует аннотация расширения:\n{diagram}"
        );
    }

    /// Параллельная композиция `A | B` должна отражаться в аннотации через `|`.
    #[test]
    fn test_generate_diagram_parallel_extend_generates_pipe_annotation() {
        let src = "model A { start S; } model B { start T; } start E = A | B;";
        let map = make_map(src, "Root");
        let diagram = generate_diagram(&map).unwrap();
        assert!(
            diagram.contains("E : A | B"),
            "отсутствует аннотация параллельной композиции:\n{diagram}"
        );
    }

    /// Тест детерминизма. PlantUML - единственный генератор без единой локальной
    /// сортировки: порядок подмоделей и состояний он берёт напрямую из общего слоя.
    /// Карта строится заново на каждой итерации - иначе проверялся бы кэш, а не обход.
    #[test]
    fn test_generate_diagram_is_deterministic() {
        let src = "model B { start T; } model A { start S; } model C { start U; } \
                   start E = A | B | C;";
        let first = generate_diagram(&make_map(src, "Root")).unwrap();
        for i in 1..8 {
            assert_eq!(
                first,
                generate_diagram(&make_map(src, "Root")).unwrap(),
                "прогон {i} дал другую диаграмму — вернулся недетерминизм порядка"
            );
        }
    }

    /// Блоки подмоделей печатаются в устойчивом (лексикографическом) порядке, а не в
    /// порядке обхода общего слоя. Модели объявлены `B`, `A` - вывод обязан поставить
    /// `A` перед `B`. С недетерминированным обходом тест падал бы случайно (≈половина
    /// прогонов), поэтому он и тест.
    #[test]
    fn test_generate_diagram_submodels_in_stable_order() {
        let src = "model B { start T; } model A { start S; } start E = A | B;";
        let diagram = generate_diagram(&make_map(src, "Root")).unwrap();
        let a = diagram.find("state A {").expect("нет блока A");
        let b = diagram.find("state B {").expect("нет блока B");
        assert!(
            a < b,
            "порядок подмоделей должен быть устойчивым, а не зависеть от обхода:\n{diagram}"
        );
    }
}
