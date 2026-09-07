//! Интеграционные тесты семантики, часть 4 (вынос из `semantic_tests.rs`).
//!
//! Хелперы и импорты - из родителя через `use super::*` (приём /08).

use super::*;

/// Безусловный переход (`ref B`) оставляет `Condition::None`.
///
/// # Пример (Takt)
/// ```but
/// start A { ref B; }
/// state B;
/// ```
#[test]
fn ref_no_cond_is_none() {
    use takt_lang::semantic::ConditionNode;
    let src = "start A { ref B; } state B;";
    let node = build(src);
    let state_a = &node.states["A"];
    if let StateNode::Simple { references, .. } = state_a {
        assert_eq!(references.len(), 1);
        assert_eq!(references[0].cond, ConditionNode::None);
    } else {
        panic!("ожидался StateNode::Simple для A");
    }
}

/// Булев литерал `true` в ref разрешается в `Condition::Bool(true)`.
///
/// # Пример (Takt)
/// ```but
/// start A { ref B: true; }
/// state B;
/// ```
#[test]
fn ref_cond_bool_literal_is_resolved() {
    use takt_lang::semantic::ConditionNode;
    let src = "start A { ref B: true; } state B;";
    let node = build(src);
    let state_a = &node.states["A"];
    if let StateNode::Simple { references, .. } = state_a {
        assert_eq!(references[0].cond, ConditionNode::Bool(true));
    } else {
        panic!("ожидался StateNode::Simple для A");
    }
}

/// Сравнение в ref разрешается в `Condition::Equal`.
///
/// # Пример (Takt)
/// ```but
/// var x: [bit;8] = 0;
/// start A { ref B: x = 255; }
/// state B;
/// ```
#[test]
fn ref_cond_comparison_is_resolved() {
    use takt_lang::semantic::ConditionNode;
    let src = "var x: [bit;8] := 0; start A { ref B: x = 255; } state B;";
    let node = build(src);
    let state_a = &node.states["A"];
    if let StateNode::Simple { references, .. } = state_a {
        assert!(
            matches!(references[0].cond, ConditionNode::Equal(_, _)),
            "ожидалось Condition::Equal, получено {:?}",
            references[0].cond
        );
    } else {
        panic!("ожидался StateNode::Simple");
    }
}

/// Контрпример: арифметика в ref-условии даёт предупреждение "арифметическое
/// вычитание".
///
/// # Контрпример (Takt)
/// ```but
/// var x: [bit;8] = 0;
/// start A { ref B: x - 1; }
/// state B;
/// ```
#[test]
fn se11_subtract_in_ref_gives_warning() {
    let src = "var x: [bit;8] := 0; start A { ref B: x - 1; } state B;";
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let root = construct_model(&ast, None, &[]).expect("ошибка построения");
    let warnings = implicit_bool_warnings(&root);
    assert_eq!(warnings.len(), 1, "вычитание должно давать предупреждение");
    assert!(
        warnings[0].message.contains("вычитание"),
        "сообщение должно упоминать вычитание: {}",
        warnings[0].message
    );
}

/// Контрпример: побитовое И в ref-условии даёт предупреждение "побитовое И".
///
/// # Контрпример (Takt)
/// ```but
/// var x: [bit;8] = 0;
/// start A { ref B: x & 1; }
/// state B;
/// ```
#[test]
fn se11_bitwise_and_in_ref_gives_warning() {
    let src = "var x: [bit;8] := 0; start A { ref B: x & 1; } state B;";
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let root = construct_model(&ast, None, &[]).expect("ошибка построения");
    let warnings = implicit_bool_warnings(&root);
    assert_eq!(
        warnings.len(),
        1,
        "побитовое И должно давать предупреждение"
    );
    assert!(
        warnings[0].message.contains("побитовое И"),
        "сообщение должно упоминать тип операции: {}",
        warnings[0].message
    );
}

/// Контрпример: числовой литерал в ref-условии даёт предупреждение с указанием числа.
///
/// # Контрпример (Takt)
/// ```but
/// start A { ref B: 42; }
/// state B;
/// ```
#[test]
fn se11_resolved_number_literal_in_ref_has_value_in_message() {
    let src = "start A { ref B: 42; } state B;";
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let root = construct_model(&ast, None, &[]).expect("ошибка построения");
    let warnings = implicit_bool_warnings(&root);
    assert_eq!(
        warnings.len(),
        1,
        "числовой литерал должен давать предупреждение"
    );
    assert!(
        warnings[0].message.contains("42"),
        "сообщение должно упоминать значение: {}",
        warnings[0].message
    );
}

/// Пример файла с разрешёнными условиями - без ошибок и предупреждений.
#[test]
fn ref_cond_resolved_file_is_valid() {
    let src = std::fs::read_to_string("tests/data/semantic/valid/ref_cond_resolved.takt")
        .expect("не удалось прочитать файл");
    let (ast, _) = parse(&src, 0).expect("ошибка разбора");
    let root = construct_model(&ast, None, &[]).expect("ошибка построения семантики");
    let warnings = implicit_bool_warnings(&root);
    assert!(
        warnings.is_empty(),
        "файл с разрешёнными условиями не должен давать предупреждений: {:?}",
        warnings
    );
}

/// Контрпример файла с арифметическим условием - одно предупреждение Се11.
#[test]
fn ref_cond_arithmetic_file_gives_warning() {
    let src = std::fs::read_to_string("tests/data/semantic/valid/ref_cond_arithmetic.takt")
        .expect("не удалось прочитать файл");
    let (ast, _) = parse(&src, 0).expect("ошибка разбора");
    let root = construct_model(&ast, None, &[]).expect("ошибка построения семантики");
    let warnings = implicit_bool_warnings(&root);
    assert_eq!(
        warnings.len(),
        1,
        "арифметический файл должен давать ровно одно предупреждение"
    );
    assert!(
        warnings[0].message.contains("сложение"),
        "сообщение должно упоминать тип операции: {}",
        warnings[0].message
    );
}

// --- Тесты родительских ссылок (требование 3) --------------------------------

/// Переменная хранит ссылку на родительскую модель (`upper`).
///
/// Тест использует `Rc` напрямую (не `.take()`), чтобы родительская модель оставалась
/// живой и Weak-ссылка могла быть разыменована.
///
/// # Пример (Takt)
/// ```but
/// var flag: bit = false;
/// ```
#[test]
fn variable_node_has_parent_upper() {
    let (ast, _) = parse("var flag: bit := false;", 0).unwrap();
    let root = construct_model(&ast, None, &[]).unwrap();
    let var = root
        .borrow()
        .search_var("flag")
        .expect("переменная flag не найдена");
    let upper = var.upper();
    assert!(
        upper.is_some(),
        "переменная должна иметь ссылку на родительскую модель"
    );
}

/// Константа хранит ссылку на родительскую модель.
///
/// Тест использует `Rc` напрямую (не `.take()`), чтобы родительская модель оставалась
/// живой и Weak-ссылка могла быть разыменована.
#[test]
fn const_node_has_parent_upper() {
    let (ast, _) = parse("const C: u8 := 0;", 0).unwrap();
    let root = construct_model(&ast, None, &[]).unwrap();
    let var = root
        .borrow()
        .search_var("C")
        .expect("константа C не найдена");
    assert!(
        var.upper().is_some(),
        "константа должна иметь ссылку на родительскую модель"
    );
}

/// Именованное условие хранит ссылку на родительскую модель.
///
/// # Пример (Takt)
/// ```but
/// cond Done = true;
/// ```
#[test]
fn condition_node_has_parent_upper() {
    let node = build("cond Done = true;");
    let cond = node
        .conditions
        .get("Done")
        .expect("условие Done не найдено");
    assert!(
        cond.upper.is_some(),
        "именованное условие должно иметь ссылку на родительскую модель"
    );
}

/// Вложенная переменная ссылается на свою (вложенную) модель, не на корень.
///
/// # Пример (Takt)
/// ```but
/// model Inner { var x: bit = false; start S; }
/// ```
#[test]
fn nested_variable_upper_points_to_inner_model() {
    let (ast, _) = parse("model Inner { var x: bit := false; start S; }", 0).unwrap();
    let root = construct_model(&ast, None, &[]).unwrap();
    let inner = root
        .borrow()
        .search_model("Inner")
        .expect("Inner не найдена");
    let var = inner
        .borrow()
        .search_var("x")
        .expect("переменная x не найдена");
    let upper = var.upper().expect("переменная должна иметь upper");
    // upper должен ссылаться на Inner, а не на корневую модель
    assert_eq!(
        upper.borrow().name,
        Some("Inner".to_string()),
        "upper переменной должен указывать на модель Inner"
    );
}

/// Метод `upper()` у VariableNode::Unresolved возвращает None.
#[test]
fn unresolved_variable_upper_is_none() {
    use takt_lang::semantic::VariableNode;
    let unresolved = VariableNode::Unresolved;
    assert!(unresolved.upper().is_none());
}

/// Вспомогательные методы `name()` и `ty()` у VariableNode работают корректно.
#[test]
fn variable_node_name_and_ty_methods() {
    use takt_lang::semantic::type_node::TypeNode;
    let node = build("var flag: bit := false;");
    let var = node.search_var("flag").expect("flag не найдена");
    assert_eq!(var.name(), "flag");
    assert_eq!(*var.ty(), TypeNode::Bit);
}

// --- С4: интеграционные тесты локальных переменных в блоках ------------------

/// `tests/data/semantic/valid/local_var_in_block.takt` - var внутри always - без
/// ошибок.
///
/// # Пример (Takt)
/// ```but
/// var flag: bit = false;
/// start Running {
///     always {
///         var x: bit = false;
///         x = true;
///         flag = x;
///     }
/// }
/// ```
#[test]
fn example_local_var_in_block_is_valid() {
    build_file("tests/data/semantic/valid/local_var_in_block.takt").unwrap();
}

/// `tests/data/semantic/valid/local_var_in_for.takt` - var в инициализаторе for - без
/// ошибок.
///
/// # Пример (Takt)
/// ```but
/// var result: bit = false;
/// start S {
///     always {
///         for var i: bit = false; i; i = false { result = i; }
///     }
/// }
/// ```
#[test]
fn example_local_var_in_for_is_valid() {
    build_file("tests/data/semantic/valid/local_var_in_for.takt").unwrap();
}

/// `tests/data/semantic/valid/local_var_nested.takt` - вложенные блоки с затенением -
/// без ошибок.
///
/// # Пример (Takt)
/// ```but
/// var x: bit = true;
/// start S {
///     always {
///         { var x: bit = false; x = false; }
///         x = true;   // <- model-level x, не локальная
///     }
/// }
/// ```
#[test]
fn example_local_var_nested_is_valid() {
    build_file("tests/data/semantic/valid/local_var_nested.takt").unwrap();
}

/// Переменная через `upper()` позволяет найти другие переменные той же модели.
///
/// Демонстрирует, что `upper` действительно предоставляет доступ к контексту.
/// Используем `Rc<RefCell<ModelNode>>` напрямую (без `.take()`), чтобы `upper` внутри
/// переменных ссылался на живой узел модели.
#[test]
fn variable_upper_gives_access_to_sibling_vars() {
    let (ast, _) = parse("var a: bit := false; var b: bit := false;", 0).expect("ошибка разбора");
    let root = construct_model(&ast, None, &[]).expect("ошибка построения");
    let var_a = root.borrow().search_var("a").expect("a не найдена");
    let upper = var_a.upper().expect("upper должен быть Some");
    // Через upper можно найти переменную b
    assert!(
        upper.borrow().search_var("b").is_some(),
        "через upper переменной a должна быть доступна переменная b"
    );
}

// --- SA8: тесты отсутствия циклических сильных Rc-ссылок ------------------

/// Модель с условиями не создаёт сильных циклов (SA8).
#[test]
fn no_strong_cycle_with_conditions() {
    use std::rc::Rc;
    let (ast, _) = parse("var x: bit := false; cond Done = x = false; start S;", 0).unwrap();
    let root = construct_model(&ast, None, &[]).unwrap();
    assert_eq!(
        Rc::strong_count(&root),
        1,
        "модель с условиями: счётчик Rc должен быть 1"
    );
}

/// Модель с именованными блоками не создаёт сильных циклов (SA8).
#[test]
fn no_strong_cycle_with_named_blocks() {
    use std::rc::Rc;
    let (ast, _) = parse("var x: bit := false; start S { always { x := x; } }", 0).unwrap();
    let root = construct_model(&ast, None, &[]).unwrap();
    assert_eq!(
        Rc::strong_count(&root),
        1,
        "модель с блоками: счётчик Rc должен быть 1"
    );
}

// --- Ce5: Проверка достижимости и полноты переходов --------------------------

/// Вспомогательная функция: строит модель как Rc и возвращает корень.
fn build_rc(src: &str) -> std::rc::Rc<std::cell::RefCell<takt_lang::semantic::ModelNode>> {
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    construct_model(&ast, None, &[]).expect("ошибка построения")
}

/// Вспомогательная функция: строит модель из файла как Rc.
fn build_file_rc(
    path: &str,
) -> Result<
    std::rc::Rc<std::cell::RefCell<takt_lang::semantic::ModelNode>>,
    takt_lang::diagnostics::Diagnostic,
> {
    let src = std::fs::read_to_string(path)
        .unwrap_or_else(|e| panic!("не могу прочитать {}: {}", path, e));
    let (ast, _) = parse(&src, 0).expect("ошибка разбора файла");
    construct_model(&ast, None, &[])
}

/// Вспомогательная функция: строит модель и возвращает предупреждения Ce5.
fn ce5_warnings(src: &str) -> Vec<takt_lang::diagnostics::Diagnostic> {
    let root = build_rc(src);
    transition_completeness_warnings(&root)
}

/// Модель с одним терминальным состоянием - нет предупреждений Ce5.
#[test]
fn ce5_single_terminal_no_warning() {
    // Finish - терминальное (нет переходов)
    let warns = ce5_warnings("start Start { ref Finish: true; } state Finish;");
    assert!(
        warns.is_empty(),
        "состояние без переходов терминально, предупреждений быть не должно: {:?}",
        warns
    );
}

/// Цепочка состояний с терминальным в конце - нет предупреждений.
#[test]
fn ce5_chain_with_terminal_no_warning() {
    let warns = ce5_warnings("start A { ref B: true; } state B { ref C: true; } state C;");
    assert!(
        warns.is_empty(),
        "цепочка с терминальным в конце: предупреждений не должно быть"
    );
}

/// Цикл без терминального - предупреждение Ce5.2 (нет терминальных).
#[test]
fn ce5_cycle_no_terminal_gives_warning() {
    let warns = ce5_warnings("start A { ref B: true; } state B { ref A: true; }");
    assert!(
        !warns.is_empty(),
        "цикл без терминального состояния должен давать предупреждение"
    );
    let msg = &warns[0].message;
    assert!(
        msg.contains("терминальн"),
        "сообщение должно упоминать терминальные состояния: {}",
        msg
    );
}

/// Предупреждение Ce5.2 имеет уровень Warning.
#[test]
fn ce5_no_terminal_warning_level() {
    use takt_lang::diagnostics::Level;
    let warns = ce5_warnings("start A { ref B: true; } state B { ref A: true; }");
    assert!(!warns.is_empty());
    assert_eq!(warns[0].level, Level::Warning);
}

/// Состояние без пути к терминальному - предупреждение Ce5.1.
#[test]
fn ce5_state_no_path_to_terminal_warning() {
    // C -> D -> C (цикл), A -> B -> C; B - терминальный, C/D не имеют пути
    let warns = ce5_warnings(
        "start A { ref B: true; ref C: true; } state B; \
         state C { ref D: true; } state D { ref C: true; }",
    );
    // Должны быть предупреждения о C и D
    let has_cd = warns
        .iter()
        .any(|w| w.message.contains('C') || w.message.contains('D'));
    assert!(
        has_cd,
        "состояния C и D не имеют пути к терминальному: {:?}",
        warns
    );
}

/// Модель без состояний - нет предупреждений Ce5.
#[test]
fn ce5_no_states_no_warning() {
    let warns = ce5_warnings("var x: bit := false;");
    assert!(
        warns.is_empty(),
        "модель без состояний не должна давать предупреждений Ce5"
    );
}

/// Предупреждение Ce5.3: ref + next в одном состоянии.
#[test]
fn ce5_ref_and_next_together_warning() {
    let warns = ce5_warnings(
        "model M { start S; } \
         start A = M { ref B: true; next C; } \
         state B; state C;",
    );
    let has_warn = warns
        .iter()
        .any(|w| w.message.contains("ref") && w.message.contains("next"));
    assert!(
        has_warn,
        "ref + next вместе должны давать предупреждение Ce5.3: {:?}",
        warns
    );
}

/// Только next без ref - нет предупреждения Ce5.3.
#[test]
fn ce5_only_next_no_ref_no_warn() {
    let warns = ce5_warnings(
        "model M { start S; } \
         start A = M { next B; } \
         state B;",
    );
    // Не должно быть предупреждения о ref+next
    let has_ref_next = warns
        .iter()
        .any(|w| w.message.contains("ref") && w.message.contains("next"));
    assert!(
        !has_ref_next,
        "только next без ref не должен давать предупреждение Ce5.3"
    );
}

/// Файл ce5_terminal_states.takt - нет предупреждений.
#[test]
fn example_ce5_terminal_states_valid() {
    let root = build_file_rc("tests/data/semantic/valid/ce5_terminal_states.takt")
        .expect("ошибка построения");
    let warns = transition_completeness_warnings(&root);
    assert!(
        warns.is_empty(),
        "ce5_terminal_states.takt не должен давать предупреждений: {:?}",
        warns
    );
}

/// Файл ce5_no_warn_terminal.takt - нет предупреждений.
#[test]
fn example_ce5_no_warn_terminal_valid() {
    let root = build_file_rc("tests/data/semantic/valid/ce5_no_warn_terminal.takt")
        .expect("ошибка построения");
    let warns = transition_completeness_warnings(&root);
    assert!(
        warns.is_empty(),
        "ce5_no_warn_terminal.takt не должен давать предупреждений: {:?}",
        warns
    );
}

/// Файл ce5_no_terminal.takt - предупреждение о нет терминальных.
#[test]
fn example_ce5_no_terminal_warns() {
    let root = build_file_rc("tests/data/semantic/invalid/ce5_no_terminal.takt")
        .expect("ошибка построения");
    let warns = transition_completeness_warnings(&root);
    assert!(
        !warns.is_empty(),
        "ce5_no_terminal.takt должен давать предупреждение"
    );
}

/// Файл ce5_double_next.takt - ошибка семантики (два next).
#[test]
fn example_ce5_double_next_error() {
    let src = std::fs::read_to_string("tests/data/semantic/invalid/ce5_double_next.takt")
        .expect("файл не найден");
    let (ast, _) = parse(&src, 0).expect("ошибка разбора");
    let result = construct_model(&ast, None, &[]);
    assert!(
        result.is_err(),
        "ce5_double_next.takt должен давать ошибку семантики"
    );
}

/// Файл ce5_next_with_ref.takt - предупреждение Ce5.3.
#[test]
fn example_ce5_next_with_ref_warns() {
    let root = build_file_rc("tests/data/semantic/invalid/ce5_next_with_ref.takt")
        .expect("ошибка построения");
    let warns = transition_completeness_warnings(&root);
    let has_warn = warns
        .iter()
        .any(|w| w.message.contains("ref") && w.message.contains("next"));
    assert!(
        has_warn,
        "ce5_next_with_ref.takt должен давать предупреждение Ce5.3: {:?}",
        warns
    );
}

// --- Ce4: Перечисления --------------------------------------------------------

/// EnumNode::new создаёт варианты с автоинкрементом значений.
#[test]
fn ce4_enum_node_auto_increment() {
    let e = EnumDefinitionNode::new(
        "Status",
        &[("Idle", None), ("Active", None), ("Done", None)],
    );
    assert_eq!(e.name, "Status");
    assert_eq!(e.variants[0], ("Idle".to_string(), 0));
    assert_eq!(e.variants[1], ("Active".to_string(), 1));
    assert_eq!(e.variants[2], ("Done".to_string(), 2));
}

/// EnumNode::new принимает явные значения для вариантов.
#[test]
fn ce4_enum_node_explicit_values() {
    let e = EnumDefinitionNode::new(
        "Color",
        &[("Red", Some(10)), ("Green", Some(20)), ("Blue", Some(30))],
    );
    assert_eq!(e.find_variant("Red"), Some(10));
    assert_eq!(e.find_variant("Green"), Some(20));
    assert_eq!(e.find_variant("Blue"), Some(30));
}

/// EnumNode::find_variant возвращает None для несуществующего варианта.
#[test]
fn ce4_enum_find_variant_missing() {
    let e = EnumDefinitionNode::new("Dir", &[("North", None), ("South", None)]);
    assert_eq!(e.find_variant("East"), None);
    assert_eq!(e.find_variant("West"), None);
}

/// EnumNode::has_variant возвращает true/false корректно.
#[test]
fn ce4_enum_has_variant() {
    let e = EnumDefinitionNode::new("Speed", &[("Slow", None), ("Fast", None)]);
    assert!(e.has_variant("Slow"));
    assert!(e.has_variant("Fast"));
    assert!(!e.has_variant("Medium"));
}

/// ModelNode::search_enum находит перечисление по имени.
#[test]
fn ce4_search_enum_finds_enum() {
    use takt_lang::semantic::ModelNode;
    let mut model = ModelNode::default();
    let e = EnumDefinitionNode::new("Color", &[("Red", None), ("Green", None)]);
    model.enums.insert("Color".to_string(), e.clone());
    let found = model.search_enum("Color");
    assert!(found.is_some());
    assert_eq!(found.unwrap().name, "Color");
}

/// ModelNode::search_enum возвращает None для несуществующего перечисления.
#[test]
fn ce4_search_enum_returns_none() {
    use takt_lang::semantic::ModelNode;
    let model = ModelNode::default();
    assert!(model.search_enum("NonExistent").is_none());
}

/// TypeNode::Enum хранит имя перечисления.
#[test]
fn ce4_type_node_enum_variant() {
    use takt_lang::semantic::type_node::TypeNode;
    let ty = TypeNode::Enum("Status".to_string());
    if let TypeNode::Enum(name) = &ty {
        assert_eq!(name, "Status");
    } else {
        panic!("TypeNode::Enum не создан корректно");
    }
}

/// Два разных EnumNode не равны.
#[test]
fn ce4_enum_nodes_not_equal() {
    let a = EnumDefinitionNode::new("A", &[("X", None)]);
    let b = EnumDefinitionNode::new("B", &[("Y", None)]);
    assert_ne!(a, b);
}

/// EnumNode с одинаковым содержимым равны.
#[test]
fn ce4_enum_nodes_equal() {
    let a = EnumDefinitionNode::new("Status", &[("Ok", Some(0)), ("Err", Some(1))]);
    let b = EnumDefinitionNode::new("Status", &[("Ok", Some(0)), ("Err", Some(1))]);
    assert_eq!(a, b);
}

/// Файл ce4_enum_basic.takt разбирается без ошибок.
#[test]
fn example_ce4_enum_basic_valid() {
    build_file("tests/data/semantic/valid/ce4_enum_basic.takt")
        .expect("ce4_enum_basic.takt должен разбираться без ошибок");
}

/// ModelNode с enums корректно сравнивается (PartialEq включает enums).
#[test]
fn ce4_model_eq_with_enums() {
    use takt_lang::semantic::ModelNode;
    let mut m1 = ModelNode::default();
    let mut m2 = ModelNode::default();
    let e = EnumDefinitionNode::new("Dir", &[("North", None)]);
    m1.enums.insert("Dir".to_string(), e.clone());
    m2.enums.insert("Dir".to_string(), e.clone());
    assert_eq!(m1, m2);
}

// --- Ce6: Расширенный двунаправленный вывод типов ----------------------------

/// Ce6: Тип переменной выводится из возвращаемого типа функции (bool).
///
/// `fn getbool() -> bool { return true; }` `var result = getbool();` -> тип `result` =
/// `Bool`
#[test]
fn ce6_type_inferred_from_function_return() {
    use takt_lang::semantic::type_node::TypeNode;
    let node = build(
        "fn getbool() -> bool { return true; } \
         var result := getbool(); start S;",
    );
    let var_result = node
        .search_var("result")
        .expect("переменная result не найдена");
    let ty = var_result.ty().clone();
    assert_eq!(
        ty,
        TypeNode::Bool,
        "тип result должен быть Bool (из возвращаемого типа getbool)"
    );
}

/// Ce6: Тип переменной выводится из другой переменной (цепочка вывода).
///
/// `var x: bit = false; var y = x;` -> тип `y` = тип `x` = `Bit`
#[test]
fn ce6_type_chain_from_variable() {
    use takt_lang::semantic::type_node::TypeNode;
    // Явно задаём тип x, чтобы избежать зависимости от порядка обработки HashMap
    let node = build("var x: bit := false; var y := x; start S;");
    let var_y = node.search_var("y").expect("переменная y не найдена");
    let ty = var_y.ty().clone();
    assert_eq!(ty, TypeNode::Bit, "тип y должен быть Bit (через x: bit)");
}

/// Ce6: Вывод типа булевой переменной из функции, возвращающей bool.
#[test]
fn ce6_bool_return_type_inferred() {
    use takt_lang::semantic::type_node::TypeNode;
    let node = build(
        "fn check() -> bool { return true; } \
         var flag := check(); start S;",
    );
    let var_flag = node.search_var("flag").expect("переменная flag не найдена");
    let ty = var_flag.ty().clone();
    assert_eq!(ty, TypeNode::Bool, "тип flag должен быть Bool");
}

/// Ce6: Функция с типом [bit;32] - тип переменной = [bit;32].
#[test]
fn ce6_array32_return_type_inferred() {
    use takt_lang::semantic::type_node::TypeNode;
    let node = build(
        "fn get32() -> [bit;32] { return 0; } \
         var val := get32(); start S;",
    );
    let var_val = node.search_var("val").expect("переменная val не найдена");
    let ty = var_val.ty().clone();
    assert_eq!(
        ty,
        TypeNode::Array(32, Box::new(TypeNode::Bit)),
        "тип val должен быть [bit;32]"
    );
}

/// Ce6: Переменная с явным типом не перезаписывается выводом Ce6.
#[test]
fn ce6_explicit_type_not_overwritten_by_function() {
    use takt_lang::semantic::type_node::TypeNode;
    let node = build(
        "fn getbool() -> bool { return true; } \
         var result: bit := getbool(); start S;",
    );
    let var_result = node
        .search_var("result")
        .expect("переменная result не найдена");
    let ty = var_result.ty().clone();
    assert_eq!(
        ty,
        TypeNode::Bit,
        "явный тип bit не должен быть перезаписан"
    );
}

/// Ce6: Файл ce6_type_from_func.takt разбирается без ошибок.
#[test]
fn example_ce6_type_from_func_valid() {
    build_file("tests/data/semantic/valid/ce6_type_from_func.takt")
        .expect("ce6_type_from_func.takt должен разбираться без ошибок");
}

/// Ce6: в `ce6_type_inference_chain.takt` тип выведен по цепочке `x -> y`.
///
/// Прежде тест проверял, что файл **разбирается без ошибок**, - и проходил всё время,
/// пока обещанный самой фикстурой вывод типа не работал: `y` оставался с типом `_`, а
/// форму не переводила ни одна цель. Проверять надо то, ради чего фикстура заведена, -
/// сам тип.
#[test]
fn example_ce6_type_inference_chain_infers_type_of_second_variable() {
    let model = build_file("tests/data/semantic/valid/ce6_type_inference_chain.takt")
        .expect("ce6_type_inference_chain.takt должен разбираться без ошибок");
    let y = model
        .variables
        .get("y")
        .expect("объявление 'y' не найдено в фикстуре");
    assert_eq!(
        format!("{:?}", y.ty()),
        "Array(8, Bit)",
        "тип 'y' не выведен из переменной 'x'"
    );
}

// --- Тесты FE6: Составные типы в параметрах функций --------------------------

/// FE6: Функция с параметром типа [bit;8] - разбирается без ошибок.
#[test]
fn test_fn_array_param() {
    let node = build_file("tests/data/semantic/valid/fn_array_param.takt")
        .expect("fn_array_param.takt должен разбираться без ошибок");
    let m = node
        .search_model("M")
        .expect("модель M должна быть найдена");
    let borrowed = m.borrow();
    // Функция process должна присутствовать
    assert!(
        borrowed.functions.contains_key("process"),
        "функция process должна быть объявлена"
    );
}

/// FE6: Функция с псевдонимом типа в параметре - разбирается без ошибок.
#[test]
fn test_fn_alias_param() {
    let node = build(
        "\
         fn process(data: u8) -> bit { return 0; } \
         start S {}",
    );
    // Функция process должна присутствовать
    assert!(
        node.functions.contains_key("process"),
        "функция process должна быть объявлена"
    );
}

// --- Тесты FE3: Диагностика неиспользуемых переменных (Ce13) -----------------

/// FE3: Переменная без использования даёт ровно одно предупреждение Ce13.
#[test]
fn test_unused_variable_warning() {
    use takt_lang::diagnostics::Level;
    let _node = build_file("tests/data/semantic/valid/unused_variable.takt")
        .expect("unused_variable.takt должен разбираться без ошибок");
    let model_rc = {
        let (ast, _) = parse(
            &std::fs::read_to_string("tests/data/semantic/valid/unused_variable.takt").unwrap(),
            0,
        )
        .unwrap();
        construct_model(&ast, None, &[]).unwrap()
    };
    let warnings = takt_lang::unused_variable_warnings(model_rc);
    assert_eq!(
        warnings.len(),
        1,
        "должно быть ровно одно предупреждение Ce13, получено: {:?}",
        warnings
    );
    assert_eq!(
        warnings[0].level,
        Level::Warning,
        "уровень должен быть Warning"
    );
    assert_eq!(
        warnings[0].code.as_deref(),
        Some("SE-036"),
        "код предупреждения Ce13 должен быть SE-036"
    );
    assert!(
        warnings[0].message.contains("unused"),
        "сообщение должно содержать имя переменной 'unused': {}",
        warnings[0].message
    );
}

/// Тест детерминизма диагностик. Предупреждения Ce13 собираются обходом словаря
/// `variables`; до 0048 (`HashMap`) их порядок плавал между прогонами, теперь
/// (`BTreeMap`) он лексикографический и устойчивый. Переменные объявлены `z_var`,
/// `a_var`, `m_var` - предупреждения обязаны идти в порядке `a_var`, `m_var`, `z_var`,
/// а не в порядке объявления или обхода.
#[test]
fn test_unused_variable_warnings_are_deterministic_and_sorted() {
    let src = "model M { start S; var z_var: bit; var a_var: bit; var m_var: bit; }";
    let names_of = || {
        let (ast, _) = parse(src, 0).unwrap();
        let model = construct_model(&ast, None, &[]).unwrap();
        takt_lang::unused_variable_warnings(model)
            .iter()
            .map(|w| {
                ["a_var", "m_var", "z_var"]
                    .iter()
                    .find(|n| w.message.contains(**n))
                    .copied()
                    .unwrap_or("?")
                    .to_string()
            })
            .collect::<Vec<_>>()
    };
    let first = names_of();
    assert_eq!(
        first,
        vec!["a_var", "m_var", "z_var"],
        "предупреждения должны идти в лексикографическом порядке имён, получено: {first:?}"
    );
    for i in 1..8 {
        assert_eq!(
            first,
            names_of(),
            "прогон {i} дал другой порядок предупреждений — вернулся недетерминизм"
        );
    }
}

/// FE3: Если все переменные используются - предупреждений Ce13 нет.
#[test]
fn test_all_vars_used_no_warning() {
    let (ast, _) = parse(
        &std::fs::read_to_string("tests/data/semantic/valid/all_vars_used.takt").unwrap(),
        0,
    )
    .unwrap();
    let model_rc = construct_model(&ast, None, &[]).unwrap();
    let warnings = takt_lang::unused_variable_warnings(model_rc);
    assert!(
        warnings.is_empty(),
        "не должно быть предупреждений Ce13, получено: {:?}",
        warnings
    );
}

// --- Bug #5: переменная родительской модели, используемая в подмодели -------
