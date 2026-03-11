/// Вспомогательные функции для работы со свойствами `behavior` и `end`.
use but_grammar::ast::{Expression, ModelDefinition, ModelPart, Property, StatePart};

/// Тип компоновки конечных автоматов (значение свойства `behavior`).
///
/// Синтаксис в языке BuT:
/// ```text
/// behavior -> A + B + C;   // последовательная: A → B → C
/// behavior -> A | B | C;   // параллельная: все одновременно
/// behavior -> A;            // выбор: запустить A (одну модель)
/// ```
#[derive(Debug, Clone, PartialEq)]
pub enum BehaviorKind {
    /// Последовательное выполнение: M1 завершается → запускается M2 и т.д.
    /// Синтаксис: `M1 + M2 + M3`
    Sequential(Vec<String>),
    /// Параллельное выполнение: все модели работают одновременно до завершения всех.
    /// Синтаксис: `M1 | M2 | M3`
    Parallel(Vec<String>),
    /// Выбор одной модели для выполнения.
    /// Синтаксис: `M` (одно имя без операторов)
    Choice(Vec<String>),
}

impl BehaviorKind {
    /// Возвращает список имён моделей, участвующих в компоновке.
    pub fn models(&self) -> &[String] {
        match self {
            BehaviorKind::Sequential(m) | BehaviorKind::Parallel(m) | BehaviorKind::Choice(m) => m,
        }
    }
}

/// Найти и разобрать свойство `behavior` в модели.
///
/// Синтаксис значения свойства:
/// - `A + B + C`  → [`BehaviorKind::Sequential`]  (оператор `+`)
/// - `A | B | C`  → [`BehaviorKind::Parallel`]    (оператор `|`)
/// - `A`          → [`BehaviorKind::Choice`]       (одиночное имя модели)
///
/// Возвращает `None`, если свойство отсутствует или выражение не распознано.
pub fn find_behavior(model: &ModelDefinition) -> Option<BehaviorKind> {
    for part in &model.parts {
        if let ModelPart::PropertyDefinition(pd) = part {
            if pd.name.as_ref().map(|n| n.name.as_str()) == Some("behavior") {
                if let Property::Expression(expr) = &pd.value {
                    return parse_behavior_expr(expr);
                }
            }
        }
    }
    None
}

/// Разобрать выражение компоновки из AST.
///
/// - `Add(A, B)` и вложенные Add  → Sequential
/// - `BitwiseOr(A, B)` и вложенные BitwiseOr → Parallel
/// - `Variable(A)`                 → Choice(["A"])
fn parse_behavior_expr(expr: &Expression) -> Option<BehaviorKind> {
    match expr {
        Expression::Add(..) => {
            let names = flatten_add(expr);
            if names.is_empty() { None } else { Some(BehaviorKind::Sequential(names)) }
        }
        Expression::BitwiseOr(..) => {
            let names = flatten_bitor(expr);
            if names.is_empty() { None } else { Some(BehaviorKind::Parallel(names)) }
        }
        Expression::Variable(id) => {
            Some(BehaviorKind::Choice(vec![id.name.clone()]))
        }
        _ => None,
    }
}

/// Рекурсивно извлечь имена моделей из левоассоциативного дерева `Add`.
///
/// Пример: `A + B + C` → `Add(Add(A, B), C)` → ["A", "B", "C"]
fn flatten_add(expr: &Expression) -> Vec<String> {
    match expr {
        Expression::Add(_, left, right) => {
            let mut names = flatten_add(left);
            names.extend(flatten_add(right));
            names
        }
        Expression::Variable(id) => vec![id.name.clone()],
        _ => vec![],
    }
}

/// Рекурсивно извлечь имена моделей из левоассоциативного дерева `BitwiseOr`.
///
/// Пример: `A | B | C` → `BitwiseOr(BitwiseOr(A, B), C)` → ["A", "B", "C"]
fn flatten_bitor(expr: &Expression) -> Vec<String> {
    match expr {
        Expression::BitwiseOr(_, left, right) => {
            let mut names = flatten_bitor(left);
            names.extend(flatten_bitor(right));
            names
        }
        Expression::Variable(id) => vec![id.name.clone()],
        _ => vec![],
    }
}

/// Найти свойство `end` в модели.
///
/// Возвращает ссылку на `Property`, если свойство присутствует.
pub fn find_end_property(model: &ModelDefinition) -> Option<&Property> {
    for part in &model.parts {
        if let ModelPart::PropertyDefinition(pd) = part {
            if pd.name.as_ref().map(|n| n.name.as_str()) == Some("end") {
                return Some(&pd.value);
            }
        }
    }
    None
}

/// Найти терминальные состояния модели — состояния без исходящих переходов (`ref`).
///
/// Терминальное состояние означает завершение работы автомата.
pub fn find_terminal_states(model: &ModelDefinition) -> Vec<String> {
    model
        .parts
        .iter()
        .filter_map(|p| {
            if let ModelPart::StateDefinition(sd) = p {
                let name = sd.name.as_ref()?.name.clone();
                let has_refs =
                    sd.parts.iter().any(|sp| matches!(sp, StatePart::Reference(..)));
                if !has_refs { Some(name) } else { None }
            } else {
                None
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_first_model(src: &str) -> ModelDefinition {
        let unit = but_grammar::parse(src, 0).expect("парсинг прошёл успешно").0;
        if let but_grammar::ast::SourceUnitPart::ModelDefinition(md) = unit.0.into_iter().next().unwrap() {
            *md
        } else {
            panic!("первый элемент не является моделью")
        }
    }

    // ── Тесты синтаксиса behavior ──────────────────────────────────────────────

    #[test]
    fn поиск_behavior_последовательного() {
        // behavior -> A + B; — оператор + означает Sequential
        let md = parse_first_model(r#"
model Pipe {
    behavior -> Phase1 + Phase2;
}
"#);
        let bk = find_behavior(&md);
        assert!(matches!(bk, Some(BehaviorKind::Sequential(_))), "ожидается Sequential, получено: {:?}", bk);
        if let Some(BehaviorKind::Sequential(names)) = bk {
            assert_eq!(names, vec!["Phase1", "Phase2"]);
        }
    }

    #[test]
    fn поиск_behavior_последовательного_три_модели() {
        // A + B + C левоассоциативно → Add(Add(A, B), C) → ["A", "B", "C"]
        let md = parse_first_model(r#"
model Pipeline {
    behavior -> Calibrate + Process + Store;
}
"#);
        let bk = find_behavior(&md);
        assert!(matches!(bk, Some(BehaviorKind::Sequential(_))));
        if let Some(BehaviorKind::Sequential(names)) = bk {
            assert_eq!(names, vec!["Calibrate", "Process", "Store"]);
        }
    }

    #[test]
    fn поиск_behavior_параллельного() {
        // behavior -> A | B; — оператор | означает Parallel
        let md = parse_first_model(r#"
model Combo {
    behavior -> Alpha | Beta;
}
"#);
        let bk = find_behavior(&md);
        assert!(matches!(bk, Some(BehaviorKind::Parallel(_))), "ожидается Parallel, получено: {:?}", bk);
        if let Some(BehaviorKind::Parallel(names)) = bk {
            assert_eq!(names, vec!["Alpha", "Beta"]);
        }
    }

    #[test]
    fn поиск_behavior_параллельного_три_модели() {
        // A | B | C → Parallel(["A", "B", "C"])
        let md = parse_first_model(r#"
model Prep {
    behavior -> Calibrate | Process | Store;
}
"#);
        if let Some(BehaviorKind::Parallel(names)) = find_behavior(&md) {
            assert_eq!(names, vec!["Calibrate", "Process", "Store"]);
        } else {
            panic!("ожидается Parallel с тремя моделями");
        }
    }

    #[test]
    fn поиск_behavior_выбора_одной_модели() {
        // behavior -> A; — одно имя без операторов → Choice
        let md = parse_first_model(r#"
model Wrapper {
    behavior -> Worker;
}
"#);
        let bk = find_behavior(&md);
        assert!(matches!(bk, Some(BehaviorKind::Choice(_))), "ожидается Choice, получено: {:?}", bk);
        if let Some(BehaviorKind::Choice(names)) = bk {
            assert_eq!(names, vec!["Worker"]);
        }
    }

    #[test]
    fn поиск_behavior_отсутствует() {
        // Обычный автомат без свойства behavior
        let md = parse_first_model(r#"
model Plain {
    state S {}
    start -> S;
}
"#);
        assert!(find_behavior(&md).is_none(), "у обычного автомата не должно быть behavior");
    }

    // ── Тесты свойства end ────────────────────────────────────────────────────

    #[test]
    fn поиск_end_свойство_присутствует() {
        let md = parse_first_model(r#"
model M {
    state A {}
    start -> A;
    end -> { done = 1; }
}
"#);
        assert!(find_end_property(&md).is_some(), "свойство end должно быть найдено");
    }

    #[test]
    fn поиск_end_свойство_отсутствует() {
        let md = parse_first_model(r#"
model M {
    state A {}
    start -> A;
}
"#);
        assert!(find_end_property(&md).is_none(), "свойство end должно отсутствовать");
    }

    // ── Тесты терминальных состояний ─────────────────────────────────────────

    #[test]
    fn терминальные_состояния_одно() {
        let md = parse_first_model(r#"
model M {
    state Working { ref Done: done; }
    state Done { }
    start -> Working;
}
"#);
        let terms = find_terminal_states(&md);
        assert_eq!(terms, vec!["Done"], "только Done должно быть терминальным");
    }

    #[test]
    fn терминальные_состояния_несколько() {
        let md = parse_first_model(r#"
model M {
    state A { ref B: x; ref C: y; }
    state B { }
    state C { }
    start -> A;
}
"#);
        let terms = find_terminal_states(&md);
        assert!(terms.contains(&"B".to_string()), "B должно быть терминальным");
        assert!(terms.contains(&"C".to_string()), "C должно быть терминальным");
    }

    #[test]
    fn терминальные_состояния_отсутствуют_если_все_имеют_переходы() {
        let md = parse_first_model(r#"
model M {
    state A { ref B: x; }
    state B { ref A: y; }
    start -> A;
}
"#);
        assert!(find_terminal_states(&md).is_empty(), "цикл — нет терминальных состояний");
    }

    // ── Тесты вспомогательного метода models() ────────────────────────────────

    #[test]
    fn behavior_kind_models_возвращает_срез() {
        let seq = BehaviorKind::Sequential(vec!["A".into(), "B".into()]);
        assert_eq!(seq.models(), &["A", "B"]);
        let par = BehaviorKind::Parallel(vec!["X".into(), "Y".into(), "Z".into()]);
        assert_eq!(par.models(), &["X", "Y", "Z"]);
        let ch = BehaviorKind::Choice(vec!["Worker".into()]);
        assert_eq!(ch.models(), &["Worker"]);
    }
}
