//! Дополнительные интеграционные тесты семантического анализа Takt.
//!
//! Проверяют:
//! - поиск моделей и переменных в дереве видимости;
//! - построение типов из различных вариантов [`ast::Type`];
//! - компоновку реализаций (`+`, `|`, скобки);
//! - обнаружение дублирующихся имён моделей;
//! - ошибочные пути: некорректный тип порта, несуществующий псевдоним и др.;
//! - импорт моделей из файлов (`import "file.takt"`, `import "file.takt" as Name`);
//! - файлы-примеры из `tests/data/semantic/`.

use takt_lang::parse;
use takt_lang::semantic::StatementNode;
use takt_lang::semantic::enum_node::EnumDefinitionNode;
use takt_lang::semantic::extend::Extend;
use takt_lang::semantic::tree::{
    construct_model, construct_model_with_docs, implicit_bool_warnings,
    transition_completeness_warnings,
};
use takt_lang::semantic::type_node::TypeNode;
use takt_lang::semantic::{StateNode, VariableNode};
// --- Вспомогательная функция --------------------------------------------------

/// Разбирает Takt-программу и возвращает корневой [`ModelNode`].
fn build(src: &str) -> takt_lang::semantic::ModelNode {
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    construct_model(&ast, None, &[])
        .expect("ошибка построения семантического дерева")
        .take()
}

/// Разбирает Takt-программу и ожидает ошибку семантического анализа.
fn build_err(src: &str) -> takt_lang::diagnostics::Diagnostic {
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    construct_model(&ast, None, &[]).expect_err("ожидалась ошибка")
}

// --- Тесты search_model -------------------------------------------------------

/// `search_model` находит вложенную модель по имени.
#[test]
fn search_model_finds_nested_model() {
    let (ast, _) = parse("model Inner { start S; }", 0).unwrap();
    let root = construct_model(&ast, None, &[]).unwrap();
    assert!(
        root.borrow().search_model("Inner").is_some(),
        "Inner должна быть найдена в корневом контексте"
    );
}

/// `search_model` возвращает `None` для несуществующей модели.
#[test]
fn search_model_returns_none_for_unknown() {
    let (ast, _) = parse("model Inner { start S; }", 0).unwrap();
    let root = construct_model(&ast, None, &[]).unwrap();
    assert!(
        root.borrow().search_model("Ghost").is_none(),
        "Несуществующая модель должна давать None"
    );
}

/// `search_model` поднимается по цепочке `upper` до родителя.
#[test]
fn search_model_walks_upper_chain() {
    let (ast, _) = parse(
        "model Outer { model Inner { start S; } start A = Inner; }",
        0,
    )
    .unwrap();
    let root = construct_model(&ast, None, &[]).unwrap();
    let outer = root.borrow().search_model("Outer").unwrap();
    // Inner вложена в Outer - можно найти из Outer
    assert!(
        outer.borrow().search_model("Inner").is_some(),
        "Inner должна быть найдена изнутри Outer"
    );
}

// --- Тесты search_var --------------------------------------------------------

/// `search_var` находит переменную на верхнем уровне.
#[test]
fn search_var_finds_global_variable() {
    let node = build("var x: bit := false;");
    assert!(
        node.search_var("x").is_some(),
        "Переменная x должна быть найдена"
    );
}

/// `search_var` возвращает `None` для необъявленной переменной.
#[test]
fn search_var_returns_none_for_unknown() {
    let node = build("var x: bit := false;");
    assert!(
        node.search_var("y").is_none(),
        "Необъявленная переменная y должна давать None"
    );
}

/// `search_var` находит константу.
#[test]
fn search_var_finds_const() {
    let node = build("const C: u8 := 0xFF;");
    assert!(
        node.search_var("C").is_some(),
        "Константа C должна быть найдена"
    );
    assert!(
        matches!(node.search_var("C").unwrap(), VariableNode::Const { .. }),
        "C должна быть VariableNode::Const"
    );
}

/// `search_var` находит порт.
#[test]
fn search_var_finds_port() {
    let node = build("in P: u8 at 0x00100000;");
    assert!(node.search_var("P").is_some(), "Порт P должен быть найден");
    assert!(
        matches!(node.search_var("P").unwrap(), VariableNode::Port { .. }),
        "P должна быть VariableNode::Port"
    );
}

// --- Тесты construct_type ----------------------------------------------------

/// `bit` разрешается в `TypeNode::Bit`.
#[test]
fn type_bit_resolves_to_type_node_bit() {
    let node = build("var x: bit := false;");
    if let Some(VariableNode::Simple { ty, .. }) = node.search_var("x") {
        assert_eq!(ty, TypeNode::Bit, "bit должен разрешаться в TypeNode::Bit");
    } else {
        panic!("переменная x не найдена или не является Simple");
    }
}

/// `[bit;8]` разрешается в `TypeNode::Array(8, Box<TypeNode::Bit>)`.
#[test]
fn type_array_resolves_correctly() {
    let node = build("var x: [bit;8] := 0;");
    if let Some(VariableNode::Simple { ty, .. }) = node.search_var("x") {
        assert_eq!(
            ty,
            TypeNode::Array(8, Box::new(TypeNode::Bit)),
            "Массив должен разрешаться в Array(8, Bit)"
        );
    } else {
        panic!("переменная x не найдена");
    }
}

/// Псевдоним типа `Byte = [bit;8]` раскрывается в `TypeNode::Array`.
///
/// Прежде псевдоним звался `u8` и **затенял** встроенный тип; с это `SE-107`, поэтому
/// проверка идёт на законном имени.
#[test]
fn type_alias_resolves_through_map() {
    let node = build("type Byte = [bit;8]; var x: Byte := 0;");
    if let Some(VariableNode::Simple { ty, .. }) = node.search_var("x") {
        assert_eq!(
            ty,
            TypeNode::Array(8, Box::new(TypeNode::Bit)),
            "Псевдоним Byte должен раскрыться в Array(8, Bit)"
        );
    } else {
        panic!("переменная x не найдена");
    }
}

/// Встроенный тип `u8` (без переопределения) даёт `Integer { bits:8, signed:false }`.
#[test]
fn builtin_u8_gives_integer_type() {
    let node = build("var x: u8 := 0; start S;");
    if let Some(VariableNode::Simple { ty, .. }) = node.search_var("x") {
        assert_eq!(
            ty,
            takt_lang::semantic::type_node::TypeNode::Integer {
                bits: 8,
                signed: false
            },
        );
    } else {
        panic!("переменная x не найдена");
    }
}

/// Встроенный тип `i32` даёт `Integer { bits:32, signed:true }`.
#[test]
fn builtin_i32_gives_integer_type() {
    let node = build("var x: i32 := 0; start S;");
    if let Some(VariableNode::Simple { ty, .. }) = node.search_var("x") {
        assert_eq!(
            ty,
            takt_lang::semantic::type_node::TypeNode::Integer {
                bits: 32,
                signed: true
            },
        );
    } else {
        panic!("переменная x не найдена");
    }
}

/// Встроенный псевдоним `bool` разрешается в `TypeNode::Bool`.
#[test]
fn type_alias_bool_resolves_to_bit() {
    let node = build("var flag: bool := false;");
    if let Some(VariableNode::Simple { ty, .. }) = node.search_var("flag") {
        assert_eq!(ty, TypeNode::Bool);
    } else {
        panic!("переменная flag не найдена");
    }
}

/// Встроенный псевдоним `float` разрешается в `TypeNode::Rational`.
#[test]
fn type_alias_float_resolves_to_rational() {
    let node = build("var r: float := 0;");
    if let Some(VariableNode::Simple { ty, .. }) = node.search_var("r") {
        assert_eq!(ty, TypeNode::Rational);
    } else {
        panic!("переменная r не найдена");
    }
}

/// Несуществующий псевдоним типа - ошибка.
#[test]
fn unknown_type_alias_is_error() {
    let (ast, _) = parse("var x: UnknownType := 0;", 0).unwrap();
    let result = construct_model(&ast, None, &[]);
    assert!(
        result.is_err(),
        "Неизвестный псевдоним типа должен давать ошибку"
    );
}

// --- Тесты construct_implement -----------------------------------------------

/// Простая реализация (`= M`) разрешается в `Implement::Model`.
#[test]
fn implement_single_model_resolves() {
    let node = build("start A = M { } state B; model M { start S; }");
    if let StateNode::Implement { implements, .. } = &node.states["A"] {
        assert!(
            matches!(implements, Extend::Model(_, _, _)),
            "Простая реализация должна разрешаться в Implement::Model"
        );
    } else {
        panic!("ожидался StateNode::Implement");
    }
}

/// Реализация с `+` (последовательная компоновка) остаётся **плоской**:
/// `Extend::Concatenation` со списком элементов.
///
/// Упаковки в синтетическую модель со ступенями `Step0...StepN` нет: путь отвергнут
/// решением (безусловный `next` между ступенями давал бы неверный тайминг), разбор.
#[test]
fn implement_add_composition_resolves() {
    let node = build("start Entry = M1 + M2; model M1 { start S; } model M2 { start T; }");
    if let StateNode::Implement { implements, .. } = &node.states["Entry"] {
        assert!(
            matches!(implements, Extend::Concatenation(items) if items.len() == 2),
            "Компоновка M1 + M2 должна давать Extend::Concatenation с 2 элементами, получили: {:?}",
            implements
        );
    } else {
        panic!("ожидался StateNode::Implement для Entry");
    }
}

/// Реализация с `|` (параллельная компоновка) разрешается в `Implement::Parallel`.
#[test]
fn implement_or_composition_resolves() {
    let node = build("start Entry = M1 | M2; model M1 { start S; } model M2 { start T; }");
    if let StateNode::Implement { implements, .. } = &node.states["Entry"] {
        assert!(
            matches!(implements, Extend::Parallel(_)),
            "Компоновка | должна разрешаться в Implement::Parallel"
        );
    } else {
        panic!("ожидался StateNode::Implement для Entry");
    }
}

/// Неизвестная модель в `implements` - ошибка.
#[test]
fn implement_unknown_model_is_error() {
    let (ast, _) = parse("start A = Ghost { }", 0).unwrap();
    let result = construct_model(&ast, None, &[]);
    assert!(
        result.is_err(),
        "Ссылка на несуществующую модель должна давать ошибку"
    );
    let err = result.unwrap_err();
    assert!(
        err.message.contains("Ghost"),
        "Сообщение ошибки должно содержать имя модели: {}",
        err.message
    );
}

// --- Тесты дублирования имён -------------------------------------------------

/// Два состояния с одинаковым именем: второе (`state S`) перезаписывает первое (`start
/// S`) в HashMap, после чего модель остаётся без start-состояния. Это корректно
/// обнаруживается валидатором как ошибка.
///
/// # Контрпример (Takt)
/// ```but
/// start S;   // добавляется как Start
/// state S;   // перезаписывает - Start исчезает -> ошибка валидации
/// ```
#[test]
fn duplicate_state_names_overwrite_causes_validation_error() {
    let (ast, _) = parse("start S; state S;", 0).unwrap();
    let result = construct_model(&ast, None, &[]);
    assert!(
        result.is_err(),
        "дублирующееся имя состояния удаляет start — должна быть ошибка валидации"
    );
}

/// Два вложенных `model` с одинаковым именем - ошибка (реализация TODO).
#[test]
fn duplicate_nested_model_name_is_error() {
    let (ast, _) = parse(
        "model Outer { model M { start S; } model M { start T; } start A; }",
        0,
    )
    .unwrap();
    let result = construct_model(&ast, None, &[]);
    assert!(
        result.is_err(),
        "Дублирующееся имя вложенной модели должно давать ошибку"
    );
    let err = result.unwrap_err();
    assert!(
        err.message.contains("M"),
        "Сообщение ошибки должно содержать имя модели: {}",
        err.message
    );
}

// --- Тесты ошибок портов ------------------------------------------------------

/// Порт без явного типа - ошибка.
#[test]
fn port_without_type_is_error() {
    let (ast, _) = parse("in P := 0x00100000;", 0).unwrap();
    let result = construct_model(&ast, None, &[]);
    assert!(result.is_err(), "Порт без типа должен давать ошибку");
}

/// Инициализатор **выходного** порта - начальное значение, и построение его принимает.
///
/// Прежде тест назывался "порт с инициализатором не-адресом" и стоял на
/// **входном** порте: до 0187 инициализатор означал адрес, и не-адрес просто
/// игнорировался. Теперь у входа начального значения быть не может (`SE-092`) -
/// проба переехала на выход, где значение законно.
#[test]
fn output_port_initial_value_is_accepted() {
    let (ast, _) = parse("out P: u8 := 1; start S;", 0).unwrap();
    let result = construct_model(&ast, None, &[]);
    assert!(
        result.is_ok(),
        "начальное значение выходного порта обязано приниматься: {:?}",
        result.err()
    );
}

/// Начальное значение **входного** порта отвергается (`SE-092`).
#[test]
fn input_port_initial_value_is_rejected() {
    let (ast, _) = parse("in P: u8 := 1; start S;", 0).unwrap();
    let result = construct_model(&ast, None, &[]);
    let code = result.err().and_then(|d| d.code);
    assert_eq!(
        code.as_deref(),
        Some("SE-092"),
        "значение входа приходит извне — задавать его нечем"
    );
}

/// оператор `address Имя = <адрес>;` привязывается к порту и сохраняется в
/// `address_defs` модели.
#[test]
fn address_operator_is_captured_in_address_defs() {
    let model = build_from_src("in BTN: u8; address BTN = 0x00200000; start Idle;")
        .expect("модель с оператором address должна строиться");
    assert_eq!(
        model.address_defs.len(),
        1,
        "должна быть одна привязка адреса"
    );
    assert_eq!(
        model.address_defs[0].port, "BTN",
        "привязка должна указывать на порт BTN"
    );
}

/// адрес отдельным оператором (без inline) - модель валидна.
#[test]
fn example_port_address_separate_is_valid() {
    let model = build_file("tests/data/semantic/valid/port_address_separate.takt")
        .expect("модель с отдельными адресами портов должна строиться");
    assert_eq!(
        model.address_defs.len(),
        2,
        "должны быть привязки адресов для BTN и LED"
    );
}

/// (R4/SE-049): адрес задан и inline, и оператором `address`.
#[test]
fn port_address_conflict_inline_and_operator_is_error() {
    let err = build_file_err("tests/data/semantic/invalid/port_address_conflict.takt");
    assert_eq!(
        err.code.as_deref(),
        Some("SE-049"),
        "конфликт inline + address должен давать SE-049, получено: {:?}",
        err.code
    );
}

/// (R4/SE-049): несколько операторов `address` для одного порта.
#[test]
fn port_address_duplicate_operator_is_error() {
    let err = build_from_src(
        "in BTN: bit; address BTN = 0x00200000; address BTN = 0x00200004; start Idle;",
    )
    .expect_err("два оператора address для одного порта должны давать ошибку");
    assert_eq!(
        err.code.as_deref(),
        Some("SE-049"),
        "повторная привязка адреса должна давать SE-049, получено: {:?}",
        err.code
    );
}

/// (R5/SE-048): `address` для несуществующего порта.
#[test]
fn port_address_dangling_reference_is_error() {
    let err = build_file_err("tests/data/semantic/invalid/port_address_dangling.takt");
    assert_eq!(
        err.code.as_deref(),
        Some("SE-048"),
        "висячая привязка должна давать SE-048, получено: {:?}",
        err.code
    );
}

// ---: fixed-point q(m, n) - смешение  и приведение  ---------

/// T6: неявное смешение `q(8, 8)` с `u8` в арифметике -> ошибка SE-059, а не молчаливая
/// потеря точности.
#[test]
fn fixed_mixing_with_integer_is_se059() {
    let err = build_file_err("tests/data/semantic/invalid/fixed_mixing.takt");
    assert_eq!(
        err.code.as_deref(),
        Some("SE-059"),
        "смешение q(8, 8) и u8 должно давать SE-059, получено: {:?}",
        err.code
    );
}

/// T6: два разных формата `q` - тоже смешение (SE-059).
#[test]
fn fixed_mixing_different_formats_is_se059() {
    let err = build_from_src(
        "model M { var a: q(8, 8) := 1.5; var b: q(4, 4) := 1.5; \
         start S { always { a := a + b; } ref S: a = b; } } start E = M;",
    )
    .expect_err("q(8,8) + q(4,4) — смешение");
    assert_eq!(
        err.code.as_deref(),
        Some("SE-059"),
        "получено: {:?}",
        err.code
    );
}

/// T6: `q + q` одного формата - допустимо (тот же тип, не смешение).
#[test]
fn fixed_same_format_addition_is_valid() {
    let node = build_from_src(
        "model M { var a: q(8, 8) := 1.5; var c: q(8, 8) := 0.5; \
         start S { always { a := a + c; } ref S: a = c; } } start E = M;",
    );
    assert!(
        node.is_ok(),
        "q(8,8) + q(8,8) должно быть валидно: {:?}",
        node.err()
    );
}

/// T7: явное приведение `u8 as q(8, 8)` снимает смешение.
#[test]
fn fixed_cast_resolves_mixing() {
    let node = build_file("tests/data/semantic/valid/fixed_cast.takt");
    assert!(
        node.is_ok(),
        "приведение `b as q(8, 8)` должно сделать выражение валидным: {:?}",
        node.err()
    );
}

// --- Тесты корневой модели ---------------------------------------------------

/// Корневая модель без объявлений пуста и не имеет имени.
#[test]
fn root_model_empty_has_no_name_and_no_states() {
    let node = build("");
    assert_eq!(node.name, None);
    assert!(!node.has_states());
    assert!(node.variables.is_empty());
    assert!(node.models.is_empty());
    assert!(node.types.is_empty());
}

/// Несколько именованных моделей регистрируются в корне.
#[test]
fn multiple_named_models_all_registered() {
    let node = build("model A { start S; } model B { start T; } model C { start U; }");
    assert!(node.search_model("A").is_some(), "A не найдена");
    assert!(node.search_model("B").is_some(), "B не найдена");
    assert!(node.search_model("C").is_some(), "C не найдена");
    assert_eq!(node.models.len(), 3, "ожидались 3 модели");
}

/// Несколько переменных регистрируются в корне.
#[test]
fn multiple_global_variables_all_registered() {
    let node = build("var a: bit := false; var b: bit := true; var c: bit := false;");
    assert!(node.search_var("a").is_some());
    assert!(node.search_var("b").is_some());
    assert!(node.search_var("c").is_some());
}

// --- Контр-примеры -----------------------------------------------------------

/// `ref` к несуществующему состоянию в модели - ошибка.
#[test]
fn ref_to_nonexistent_state_in_model_is_error() {
    let (ast, _) = parse("model M { start A { ref Z; } }", 0).unwrap();
    let result = construct_model(&ast, None, &[]);
    assert!(
        result.is_err(),
        "Ссылка на несуществующее состояние должна давать ошибку"
    );
}

/// Два оператора `next` в одном Implement-состоянии - ошибка.
#[test]
fn two_next_in_same_state_is_error() {
    let (ast, _) = parse(
        "start A = M { next B; next C; } state B; state C; model M { start S; }",
        0,
    )
    .unwrap();
    let result = construct_model(&ast, None, &[]);
    assert!(
        result.is_err(),
        "Два next в одном состоянии должны давать ошибку"
    );
}

/// Тип без переменных не регистрируется в `variables`.
#[test]
fn type_definition_not_in_variables() {
    let node = build("type MyType = bit;");
    assert!(
        node.search_var("MyType").is_none(),
        "Псевдоним типа не должен попадать в переменные"
    );
    assert!(
        node.types.contains_key("MyType"),
        "Псевдоним типа должен быть в types"
    );
}

// --- Интеграционные тесты импорта --------------------------------------------

/// Вспомогательная функция: создаёт временную директорию с.takt-файлом. Возвращает
/// (TempDir, путь_к_файлу) - TempDir нужно держать живым до конца теста.
fn write_tmp_lam(name: &str, content: &str) -> (tempfile::TempDir, String) {
    let dir = tempfile::tempdir().unwrap();
    let p = dir.path().join(name);
    std::fs::write(&p, content).unwrap();
    let dir_str = dir.path().to_string_lossy().into_owned();
    (dir, dir_str)
}

/// `import "file.takt"` - успешный импорт простой модели из файла. Импортированная
/// модель доступна по нормализованному имени.
#[test]
fn plain_import_registers_model() {
    let (_dir, dir_str) = write_tmp_lam("ping.takt", "model Ping { start S; }");

    let src = r#"import "ping.takt";"#;
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let root = construct_model(&ast, None, &[dir_str]).expect("ошибка построения семантики");

    assert!(
        root.borrow().search_model("Ping").is_some(),
        "Модель Ping должна быть импортирована"
    );
}

/// Имя модели из импортированного файла нормализуется в CamelCase: `my_model.takt` ->
/// `MyModel`.
#[test]
fn plain_import_normalizes_filename_to_camel_case() {
    let (_dir, dir_str) = write_tmp_lam("my_model.takt", "start S;");

    let src = r#"import "my_model.takt";"#;
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let root = construct_model(&ast, None, &[dir_str]).expect("ошибка построения семантики");

    assert!(
        root.borrow().search_model("MyModel").is_some(),
        "my_model.takt должен регистрироваться как MyModel"
    );
    assert!(
        root.borrow().search_model("my_model").is_none(),
        "имя в snake_case не должно быть зарегистрировано"
    );
}

/// `import "file.takt" as Alias` - модель доступна под заданным именем.
#[test]
fn global_symbol_import_registers_under_alias() {
    let (_dir, dir_str) = write_tmp_lam("engine.takt", "start S;");

    let src = r#"import "engine.takt" as Motor;"#;
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let root = construct_model(&ast, None, &[dir_str]).expect("ошибка построения семантики");

    assert!(
        root.borrow().search_model("Motor").is_some(),
        "Модель должна быть доступна под именем Motor"
    );
}

/// Дублирующийся `import` одного и того же имени -> ошибка.
#[test]
fn duplicate_import_plain_is_error() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("dup.takt"), "start S;").unwrap();
    let dir_str = dir.path().to_string_lossy().into_owned();

    // Два одинаковых импорта
    let src = r#"import "dup.takt"; import "dup.takt";"#;
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let result = construct_model(&ast, None, &[dir_str]);
    assert!(result.is_err(), "Дублирующийся импорт должен давать ошибку");
    let err = result.unwrap_err();
    assert!(
        err.message.contains("уже объявлена"),
        "Сообщение должно содержать «уже объявлена»: {}",
        err.message
    );
}

/// Файл импорта не найден -> ошибка с понятным сообщением.
#[test]
fn import_missing_file_is_error() {
    let src = r#"import "ghost.takt";"#;
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let result = construct_model(&ast, None, &["/nonexistent_dir_xyz".to_string()]);
    assert!(
        result.is_err(),
        "Импорт несуществующего файла должен давать ошибку"
    );
    let err = result.unwrap_err();
    assert!(
        err.message.contains("не найден"),
        "Сообщение об ошибке должно содержать «не найден»: {}",
        err.message
    );
}

/// Файл импорта содержит синтаксическую ошибку -> ошибка при построении семантики.
#[test]
fn import_file_with_parse_error_is_error() {
    let (_dir, dir_str) = write_tmp_lam("broken.takt", "model {"); // синтаксическая ошибка

    let src = r#"import "broken.takt";"#;
    let (ast, _) = parse(src, 0).expect("ошибка разбора основного файла");
    let result = construct_model(&ast, None, &[dir_str]);
    assert!(
        result.is_err(),
        "Импорт файла с ошибкой разбора должен давать ошибку"
    );
}

/// Импортированная модель видна при разрешении `implements` в основном файле.
#[test]
fn imported_model_usable_in_implements() {
    // Корень подключаемого файла обязан иметь состояние: `import` вносит модель по
    // имени файла, и обёртка без состояний в реализации - `SE-106`.
    let (_dir, dir_str) = write_tmp_lam(
        "worker.takt",
        "model Worker { start S; }\nstart Root = Worker;",
    );

    let src = r#"
        import "worker.takt";
        start Entry = Worker { }
        state Done;
    "#;
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let root = construct_model(&ast, None, &[dir_str]).expect("ошибка построения семантики");

    // Entry реализует Worker - должно быть найдено без ошибок
    assert!(root.borrow().states.contains_key("Entry"));
}

/// `import "file.takt" as Name` с несуществующим файлом -> ошибка.
#[test]
fn global_symbol_import_missing_file_is_error() {
    let src = r#"import "ghost.takt" as Ghost;"#;
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let result = construct_model(&ast, None, &["/nonexistent".to_string()]);
    assert!(
        result.is_err(),
        "Импорт несуществующего файла должен давать ошибку"
    );
}

/// Имя из импорта через `as` не совпадает с нормализованным именем файла. Проверяем,
/// что старое имя (по имени файла) не регистрируется.
#[test]
fn global_symbol_import_only_alias_registered() {
    let (_dir, dir_str) = write_tmp_lam("engine.takt", "start S;");

    let src = r#"import "engine.takt" as Motor;"#;
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let root = construct_model(&ast, None, &[dir_str]).expect("ошибка построения семантики");

    // Только алиас должен быть зарегистрирован
    assert!(root.borrow().search_model("Motor").is_some());
    assert!(
        root.borrow().search_model("Engine").is_none(),
        "Нормализованное имя файла не должно регистрироваться при использовании as"
    );
}

// --- Тесты search_func и search_cond -----------------------------------------

/// `search_cond` находит именованное условие по имени.
#[test]
fn search_cond_finds_named_condition() {
    let node = build("cond Done = true;");
    assert!(
        node.search_cond("Done").is_some(),
        "условие 'Done' должно быть найдено"
    );
}

/// `search_cond` возвращает `None` для несуществующего условия.
#[test]
fn search_cond_returns_none_for_unknown() {
    let node = build("cond Done = true;");
    assert!(
        node.search_cond("missing").is_none(),
        "несуществующее условие должно давать None"
    );
}

/// `search_func` возвращает `None` когда функций нет.
#[test]
fn search_func_returns_none_when_no_functions() {
    let node = build("var x: bit := false;");
    assert!(
        node.search_func("any_func").is_none(),
        "search_func должен вернуть None, если функций нет"
    );
}

// --- Тесты construct_implement (исправление переполнения стека) ---------------

/// Implement-состояние без `next` успешно строится без переполнения стека.
///
/// **Регрессионный тест**: ранее приводил к бесконечной рекурсии в
/// `construct_implement` из-за заглушки `construct_expression`, которая
/// всегда возвращала `Expression::Unresolved`.
#[test]
fn implement_without_next_no_stack_overflow() {
    let node = build("start A = M { } state B; model M { start S; }");
    if let StateNode::Implement {
        next, implements, ..
    } = &node.states["A"]
    {
        assert!(next.is_none(), "next должен быть None");
        assert!(
            matches!(implements, Extend::Model(_, _, _)),
            "реализация должна разрешиться в Implement::Model"
        );
    } else {
        panic!("ожидался StateNode::Implement для A");
    }
}

/// Скобочная компоновка `(M1 + M2)` разрешается корректно.
///
/// Проверяет ветку `ast::Expression::Parenthesis` в `construct_implement_ast`: `(M1 +
/// M2)` раскрывается до плоской `Concatenation([M1, M2])` - скобки прозрачны,
/// синтетической модели не возникает.
#[test]
fn implement_parenthesized_add_resolves() {
    let node = build("start E = (M1 + M2) { } model M1 { start S; } model M2 { start T; }");
    if let StateNode::Implement { implements, .. } = &node.states["E"] {
        assert!(
            matches!(implements, Extend::Concatenation(items) if items.len() == 2),
            "скобочная компоновка (M1 + M2) должна давать Extend::Concatenation с 2 элементами, получили: {:?}",
            implements
        );
    } else {
        panic!("ожидался StateNode::Implement для E");
    }
}

// --- Тесты поиска переменных в цепочке upper ---------------------------------

/// Переменная из родительской области видимости видна во вложенной модели.
#[test]
fn nested_model_sees_parent_variable() {
    let (ast, _) = parse("var global_flag: bit := false; model Inner { start S; }", 0).unwrap();
    let root = construct_model(&ast, None, &[]).unwrap();
    let inner = root.borrow().search_model("Inner").unwrap();
    // Inner должна видеть переменную из родительского контекста через upper
    assert!(
        inner.borrow().search_var("global_flag").is_some(),
        "вложенная модель должна видеть переменную родителя"
    );
}

/// Переменная из вложенной модели недоступна в родительской (область видимости строга).
#[test]
fn parent_does_not_see_nested_variable() {
    let node = build("model Inner { var local: bit := false; start S; } start Root;");
    // Корневая модель не знает о переменной Inner
    assert!(
        node.search_var("local").is_none(),
        "родитель не должен видеть переменные вложенной модели"
    );
}

// --- Тесты типов: вложенные массивы и массивы массивов -----------------------

/// Тип `[[bit;4];2]` разрешается в `Array(2, Array(4, Bit))`.
#[test]
fn type_nested_array_resolves() {
    let node = build("var x: [[bit;4];2] := 0;");
    if let Some(VariableNode::Simple { ty, .. }) = node.search_var("x") {
        assert_eq!(
            ty,
            TypeNode::Array(2, Box::new(TypeNode::Array(4, Box::new(TypeNode::Bit)))),
            "вложенный массив должен разрешаться рекурсивно"
        );
    } else {
        panic!("переменная x не найдена");
    }
}

/// Псевдоним, используемый внутри типа массива, раскрывается правильно.
#[test]
fn type_alias_inside_array_resolves() {
    // u4 = [bit;4], затем var x: [u4; 3]
    let node = build("type u4 = [bit;4]; var x: [u4;3] := 0;");
    if let Some(VariableNode::Simple { ty, .. }) = node.search_var("x") {
        assert_eq!(
            ty,
            TypeNode::Array(3, Box::new(TypeNode::Array(4, Box::new(TypeNode::Bit)))),
            "псевдоним внутри массива должен раскрываться"
        );
    } else {
        panic!("переменная x не найдена");
    }
}

// --- Тесты сообщений об ошибках -----------------------------------------------

/// Сообщение об ошибке для неизвестной модели содержит её имя.
#[test]
fn error_message_contains_missing_model_name() {
    let err = build_err("start A = Phantom { }");
    assert!(
        err.message.contains("Phantom"),
        "сообщение об ошибке должно содержать 'Phantom': {}",
        err.message
    );
}

/// Сообщение об ошибке для неизвестного псевдонима типа содержит его имя.
#[test]
fn error_message_contains_missing_type_name() {
    let (ast, _) = parse("var x: NoSuchType := 0;", 0).unwrap();
    let err = construct_model(&ast, None, &[]).unwrap_err();
    assert!(
        err.message.contains("NoSuchType"),
        "сообщение должно содержать 'NoSuchType': {}",
        err.message
    );
}

/// Сообщение об ошибке для неизвестной ссылки ref содержит имя состояния.
#[test]
fn error_message_contains_missing_ref_name() {
    let (ast, _) = parse("start A { ref Xyz; }", 0).unwrap();
    let err = construct_model(&ast, None, &[]).unwrap_err();
    assert!(
        err.message.contains("Xyz"),
        "сообщение должно содержать 'Xyz': {}",
        err.message
    );
}

// --- Тесты файлов-примеров из tests/data/semantic/ ----------------------------

/// Вспомогательная функция: читает.takt-файл и строит семантическое дерево.
fn build_file(
    path: &str,
) -> Result<takt_lang::semantic::ModelNode, takt_lang::diagnostics::Diagnostic> {
    let src = std::fs::read_to_string(path)
        .unwrap_or_else(|e| panic!("не могу прочитать {}: {}", path, e));
    let (ast, _) = parse(&src, 0).expect("ошибка разбора файла");
    construct_model(&ast, None, &[]).map(|m| m.take())
}

/// Строит семантическую модель из исходного кода Takt.
fn build_from_src(
    src: &str,
) -> Result<takt_lang::semantic::ModelNode, takt_lang::diagnostics::Diagnostic> {
    let (ast, _) = parse(src, 0).expect("ошибка разбора исходного кода");
    construct_model(&ast, None, &[]).map(|m| m.take())
}

/// Разбирает Takt-файл и ожидает семантическую ошибку.
fn build_file_err(path: &str) -> takt_lang::diagnostics::Diagnostic {
    let src = std::fs::read_to_string(path)
        .unwrap_or_else(|e| panic!("не могу прочитать {}: {}", path, e));
    let (ast, _) = parse(&src, 0).expect("ошибка разбора файла");
    construct_model(&ast, None, &[]).expect_err("ожидалась ошибка семантического анализа")
}

/// `tests/data/semantic/valid/simple_fsm.takt` - строится без ошибок.
#[test]
fn example_simple_fsm_is_valid() {
    let node = build_file("tests/data/semantic/valid/simple_fsm.takt").unwrap();
    assert!(node.has_states(), "FSM должен иметь состояния");
    assert!(
        node.states.contains_key("Start"),
        "состояние Start должно присутствовать"
    );
    assert!(
        node.states.contains_key("Finish"),
        "состояние Finish должно присутствовать"
    );
}

#[path = "semantic_tests/part2.rs"]
mod part2;
#[path = "semantic_tests/part3.rs"]
mod part3;
#[path = "semantic_tests/part4.rs"]
mod part4;
#[path = "semantic_tests/part5.rs"]
mod part5;
#[path = "semantic_tests/part6.rs"]
mod part6;
