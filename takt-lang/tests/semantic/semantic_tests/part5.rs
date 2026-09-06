//! Интеграционные тесты семантики, часть 5 (вынос из `semantic_tests.rs`).
//!
//! Хелперы и импорты - из родителя через `use super::*` (приём /08).

use super::*;

/// Bug #5: переменная, объявленная в корневой (родительской) модели и используемая
/// только в коде подмодели, не должна получать предупреждение Ce13 "переменная
/// объявлена, но нигде не используется".
///
/// До исправления `check_model_unused` строил множество `used` только из кода текущего
/// уровня модели. Код вложенных моделей (их `always`-блоки, тела функций и т.д.) не
/// сканировался, поэтому переменная родительской модели, которую использует подмодель,
/// ошибочно считалась неиспользуемой.
#[test]
fn test_parent_var_used_in_submodel_no_unused_warning() {
    let src = r#"
        var shared: bit := 0;
        model Sub {
            start S { always { shared := 1; } }
        }
        start Main = Sub;
    "#;
    let (ast, _) = takt_lang::parse(src, 0).expect("ошибка разбора");
    let model_rc = construct_model(&ast, None, &[]).expect("ошибка построения");
    let warnings = takt_lang::unused_variable_warnings(model_rc);
    let warned_names: Vec<String> = warnings
        .iter()
        .filter_map(|w| {
            if w.code.as_deref() == Some("SE-036") {
                Some(w.message.clone())
            } else {
                None
            }
        })
        .collect();
    assert!(
        !warned_names.iter().any(|m| m.contains("shared")),
        "shared используется в подмодели Sub — предупреждения быть не должно, получено: {:?}",
        warned_names
    );
}

// --- Тесты FE4: Проверка детерминированности переходов (Ce14) -----------------

/// FE4: Два безусловных перехода из одного состояния - предупреждение Ce14.
#[test]
fn test_nondeterministic_transitions() {
    use takt_lang::diagnostics::Level;
    let (ast, _) = parse(
        &std::fs::read_to_string("tests/data/semantic/valid/nondeterministic_warn.takt").unwrap(),
        0,
    )
    .unwrap();
    let model_rc = construct_model(&ast, None, &[]).unwrap();
    let warnings = takt_lang::nondeterministic_transition_warnings(model_rc);
    assert_eq!(
        warnings.len(),
        1,
        "должно быть одно предупреждение Ce14, получено: {:?}",
        warnings
    );
    assert_eq!(warnings[0].level, Level::Warning);
    assert_eq!(
        warnings[0].code.as_deref(),
        Some("SE-037"),
        "код предупреждения Ce14 должен быть SE-037"
    );
}

/// FE4: Переходы с условиями - предупреждений Ce14 нет.
#[test]
fn test_deterministic_no_warning() {
    let (ast, _) = parse(
        &std::fs::read_to_string("tests/data/semantic/valid/deterministic_transitions.takt")
            .unwrap(),
        0,
    )
    .unwrap();
    let model_rc = construct_model(&ast, None, &[]).unwrap();
    let warnings = takt_lang::nondeterministic_transition_warnings(model_rc);
    assert!(
        warnings.is_empty(),
        "детерминированные переходы не должны давать предупреждений Ce14: {:?}",
        warnings
    );
}

// --- Тесты FE1: Перечисления --------------------------------------------------

/// FE1: Базовое перечисление - разбирается без ошибок, варианты присутствуют в
/// EnumNode.
#[test]
fn test_enum_basic() {
    let node = build_file("tests/data/semantic/valid/enum_basic.takt")
        .expect("enum_basic.takt должен разбираться без ошибок");
    // Перечисление находится во вложенной модели M
    let m = node
        .search_model("M")
        .expect("модель M должна быть найдена");
    let borrowed = m.borrow();
    assert!(
        borrowed.enums.contains_key("Direction"),
        "перечисление Direction должно быть в модели M"
    );
    let dir_enum = borrowed.enums.get("Direction").unwrap();
    assert_eq!(dir_enum.find_variant("North"), Some(0), "North = 0");
    assert_eq!(dir_enum.find_variant("South"), Some(1), "South = 1");
    assert_eq!(dir_enum.find_variant("East"), Some(2), "East = 2");
    assert_eq!(dir_enum.find_variant("West"), Some(3), "West = 3");
}

// --- Тесты FE2: Вывод типа из пользовательских псевдонимов -------------------

/// FE2: Переменная, инициализированная результатом функции с псевдонимом типа - тип
/// выводится как Array(8, Bit) через разрешение псевдонима u8 = [bit;8].
#[test]
fn test_type_alias_inference() {
    use takt_lang::semantic::type_node::TypeNode;
    let node = build_file("tests/data/semantic/valid/type_alias_inference.takt")
        .expect("type_alias_inference.takt должен разбираться без ошибок");
    let m = node
        .search_model("M")
        .expect("модель M должна быть найдена");
    let borrowed = m.borrow();
    let x_var = borrowed
        .search_var("x")
        .expect("переменная x должна быть найдена");
    let ty = x_var.ty().clone();
    assert_eq!(
        ty,
        TypeNode::Array(8, Box::new(TypeNode::Bit)),
        "тип x должен быть [bit;8] через псевдоним Byte"
    );
}

/// FE1: Перечисление с явными значениями - значения соответствуют объявлению.
#[test]
fn test_enum_with_values() {
    let node = build_file("tests/data/semantic/valid/enum_with_values.takt")
        .expect("enum_with_values.takt должен разбираться без ошибок");
    let m = node
        .search_model("M")
        .expect("модель M должна быть найдена");
    let borrowed = m.borrow();
    assert!(
        borrowed.enums.contains_key("Priority"),
        "перечисление Priority должно быть в модели M"
    );
    let prio = borrowed.enums.get("Priority").unwrap();
    assert_eq!(prio.find_variant("Low"), Some(0), "Low = 0");
    assert_eq!(prio.find_variant("Medium"), Some(5), "Medium = 5");
    assert_eq!(prio.find_variant("High"), Some(10), "High = 10");
}

// --- Ce4: Интеграционные тесты enum-типизированных переменных -----------------

/// Ce4: переменная с явным типом-перечислением разбирается без ошибок.
///
/// # Пример (Takt)
/// ```text
/// enum Direction {
///     North = 0,
///     South = 1,
///     East = 2,
///     West = 3
/// }
/// var dir: Direction = 0;
/// ```
#[test]
fn ce4_enum_typed_var_valid() {
    let node = build_file("tests/data/semantic/valid/enum_typed_var.takt")
        .expect("enum_typed_var.takt должен разбираться без ошибок");
    // Тип переменной dir должен быть TypeNode::Enum("Direction")
    if let Some(VariableNode::Simple { ty, .. }) = node.search_var("dir") {
        assert_eq!(
            ty,
            TypeNode::Enum("Direction".to_string()),
            "тип переменной dir должен быть TypeNode::Enum(\"Direction\")"
        );
    } else {
        panic!("переменная dir не найдена");
    }
}

/// Ce4: переменная с типом необъявленного перечисления -> ошибка Ce4.
///
/// # Контр-пример (Takt)
/// ```text
/// var current: Status = 0;   // Status не объявлен -> ошибка Ce4
/// start S;
/// ```
#[test]
fn ce4_undeclared_enum_type_gives_error() {
    let err = build_file_err("tests/data/semantic/invalid/ce4_undeclared_enum_type.takt");
    assert!(
        err.message.contains("Ce4") || err.message.contains("Status"),
        "ошибка должна упоминать Ce4 или имя перечисления: {}",
        err.message
    );
}

/// Ce4: переменная с enum-типом и недопустимым значением -> ошибка NI6.
///
/// # Контр-пример (Takt)
/// ```text
/// enum Color {
///     Red = 0,
///     Green = 1,
///     Blue = 2
/// }
/// var c: Color = 99;   // 99 не является вариантом Color -> NI6
/// ```
#[test]
fn ce4_enum_typed_var_invalid_value() {
    let err = build_file_err("tests/data/semantic/invalid/ce4_enum_type_wrong_value.takt");
    assert!(
        err.message.contains("NI6") || err.message.contains("99") || err.message.contains("Color"),
        "ошибка должна упоминать NI6, значение или имя enum: {}",
        err.message
    );
}

/// Ce4: переменная с типом enum инициализируется вариантом через имя
/// (Expression::Number).
///
/// Вариант `North` (значение 0) разрешается в `Number(0)`. Тип переменной остаётся
/// `TypeNode::Enum("Direction")`.
#[test]
fn ce4_enum_variant_used_as_initializer() {
    // North - вариант enum Direction, разрешается в Number(0)
    let src = "enum Direction { North = 0, South = 1 } var dir: Direction := North; start S;";
    let node = build(src);
    if let Some(VariableNode::Simple { ty, .. }) = node.search_var("dir") {
        assert_eq!(
            ty,
            TypeNode::Enum("Direction".to_string()),
            "явная аннотация типа сохраняется даже при инициализации через вариант"
        );
    } else {
        panic!("переменная dir не найдена");
    }
}

/// Ce4: два перечисления в одной модели - оба доступны независимо.
///
/// # Пример (Takt)
/// ```text
/// enum Color {
///     Red = 0,
///     Green = 1
/// }
/// enum Priority {
///     Low = 0,
///     High = 1
/// }
/// var c: Color = 0;
/// var p: Priority = 1;
/// start S;
/// ```
#[test]
fn ce4_two_enums_in_model() {
    let src = "enum Color { Red = 0, Green = 1 } \
               enum Priority { Low = 0, High = 1 } \
               var c: Color := 0; \
               var p: Priority := 1; \
               start S;";
    let node = build(src);
    if let Some(VariableNode::Simple { ty, .. }) = node.search_var("c") {
        assert_eq!(ty, TypeNode::Enum("Color".to_string()));
    } else {
        panic!("переменная c не найдена");
    }
    if let Some(VariableNode::Simple { ty, .. }) = node.search_var("p") {
        assert_eq!(ty, TypeNode::Enum("Priority".to_string()));
    } else {
        panic!("переменная p не найдена");
    }
}

/// Ce4: варианты перечисления из родительской области видимости доступны через поиск.
///
/// Аннотации типов (`var d: Dir`) работают только в той же области видимости, где
/// объявлен enum (аналогично псевдонимам `type`). Но `search_enum_variant` поднимается
/// по цепочке `upper` и находит вариант из внешней модели.
///
/// # Пример (Takt)
/// ```text
/// enum Dir {
///     N = 0,
///     S = 1
/// }
/// model Inner {
///     var d: [bit;8] = N;  // N разрешается в 0 через search_enum_variant
///     start S;
/// }
/// start Root = Inner;
/// ```
#[test]
fn ce4_enum_variant_accessible_from_nested_model() {
    // Вариант N (=0) из Dir должен быть доступен в Inner через search_enum_variant.
    // Используем Rc напрямую, чтобы не потерять upper-ссылку через.take().
    let src = "enum Dir { N = 0, S = 1 } \
               model Inner { var d: [bit;8] := N; start S; } \
               start Root = Inner;";
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let root = construct_model(&ast, None, &[]).expect("ошибка построения дерева");
    let inner = root
        .borrow()
        .search_model("Inner")
        .expect("модель Inner должна быть найдена");
    // Вариант должен быть найден через цепочку upper -> root
    assert!(
        inner.borrow().search_enum_variant("N").is_some(),
        "вариант N из родительского enum Dir должен быть доступен в Inner"
    );
    let result = inner.borrow().search_enum_variant("N");
    assert!(result.is_some(), "N должен иметь значение 0 из Dir");
    let (enum_node, value) = result.unwrap();
    assert_eq!(enum_node.name, "Dir", "enum должен называться Dir");
    assert_eq!(value, 0, "N должен иметь значение 0");
}

/// Ce4: enum, объявленный внутри модели, доступен только в ней (не в родителе).
///
/// # Контр-пример
/// Enum из вложенной модели не виден в родительской через аннотации типов.
#[test]
fn ce4_enum_declared_inside_model_local_to_it() {
    let src = "model Inner { enum Status { Ok = 0, Err = 1 } var s: Status := 0; start S; } \
               start Root = Inner;";
    let node = build(src);
    // В Inner: enum Status и переменная s: Status должны работать
    let inner = node
        .search_model("Inner")
        .expect("модель Inner должна быть найдена");
    let borrowed = inner.borrow();
    assert!(
        borrowed.enums.contains_key("Status"),
        "enum Status должен быть объявлен в Inner"
    );
    if let Some(VariableNode::Simple { ty, .. }) = borrowed.search_var("s") {
        assert_eq!(ty, TypeNode::Enum("Status".to_string()));
    } else {
        panic!("переменная s не найдена в Inner");
    }
    // В корневой модели enum Status недоступен
    assert!(
        node.search_enum("Status").is_none() || !node.enums.contains_key("Status"),
        "enum Status не должен быть виден в корневой модели напрямую"
    );
}

// --- Тесты NI3: Структурные типы ---------------------------------------------

/// NI3: Объявление структуры добавляет её в `model.structs`.
#[test]
fn test_struct_registers_in_model() {
    let src = r#"
struct Point { x: [bit;16], y: [bit;16] }
start S;
"#;
    let node = build_from_src(src).expect("должен разобраться без ошибок");
    assert!(
        node.structs.contains_key("Point"),
        "struct Point должен быть в model.structs, есть: {:?}",
        node.structs.keys().collect::<Vec<_>>()
    );
}

/// NI3: Поля структуры хранят корректные типы.
#[test]
fn test_struct_fields_have_correct_types() {
    use takt_lang::semantic::type_node::TypeNode;
    let src = r#"
struct Vec2 { x: [bit;16], y: [bit;16] }
start S;
"#;
    let node = build_from_src(src).expect("должен разобраться без ошибок");
    let s = node
        .structs
        .get("Vec2")
        .expect("Vec2 должен быть в structs");
    assert_eq!(s.fields.len(), 2);
    assert_eq!(s.fields[0].0, "x");
    assert_eq!(s.fields[0].1, TypeNode::Array(16, Box::new(TypeNode::Bit)));
    assert_eq!(s.fields[1].0, "y");
    assert_eq!(s.fields[1].1, TypeNode::Array(16, Box::new(TypeNode::Bit)));
}

/// NI3: Переменная структурного типа имеет TypeNode::Struct.
#[test]
fn test_struct_variable_type() {
    use takt_lang::semantic::type_node::TypeNode;
    let src = r#"
struct Flags { active: bit, ready: bit }
var ctrl: Flags := 0;
start S;
"#;
    let node = build_from_src(src).expect("должен разобраться без ошибок");
    let var_node = node
        .variables
        .get("ctrl")
        .expect("ctrl должен быть в переменных");
    assert_eq!(
        var_node.ty(),
        &TypeNode::Struct("Flags".to_string()),
        "тип переменной ctrl должен быть TypeNode::Struct"
    );
}

/// NI3: Структура регистрируется также в таблице псевдонимов типов.
#[test]
fn test_struct_in_types_map() {
    use takt_lang::semantic::type_node::TypeNode;
    let src = r#"
struct Config { value: [bit;8] }
start S;
"#;
    let node = build_from_src(src).expect("должен разобраться без ошибок");
    assert!(
        node.types.contains_key("Config"),
        "Config должен быть в types"
    );
    assert_eq!(
        node.types.get("Config"),
        Some(&TypeNode::Struct("Config".to_string()))
    );
}

/// NI3: `search_struct` находит структуру в текущем контексте.
#[test]
fn test_search_struct() {
    let src = r#"
struct MyStruct { field: bit }
start S;
"#;
    let node = build_from_src(src).expect("должен разобраться без ошибок");
    assert!(
        node.search_struct("MyStruct").is_some(),
        "search_struct должен найти MyStruct"
    );
    assert!(
        node.search_struct("Unknown").is_none(),
        "search_struct не должен находить несуществующую структуру"
    );
}

/// NI3: Интеграционный тест - файл `struct_types.takt` разбирается семантически.
#[test]
fn test_struct_types_file_semantic() {
    let node = build_file("tests/data/semantic/valid/struct_types.takt")
        .expect("struct_types.takt должен разбираться без ошибок");
    assert!(
        node.structs.contains_key("Point"),
        "struct Point должен быть в semantic модели"
    );
    assert!(
        node.structs.contains_key("Flags"),
        "struct Flags должен быть в semantic модели"
    );
}

// --- Тесты NI4: Анализ перекрытия условий переходов --------------------------

/// NI4: Одинаковые условия `level = 5` на два разных перехода - предупреждение NI4.
#[test]
fn test_ni4_duplicate_condition_warns() {
    use takt_lang::diagnostics::Level;
    let src = std::fs::read_to_string("tests/data/semantic/invalid/condition_overlap_eq.takt")
        .expect("файл condition_overlap_eq.takt должен существовать");
    let (ast, _) = parse(&src, 0).unwrap();
    let model_rc = construct_model(&ast, None, &[]).unwrap();
    let warnings = takt_lang::nondeterministic_transition_warnings(model_rc);
    let ni4_warnings: Vec<_> = warnings
        .iter()
        .filter(|w| w.code.as_deref() == Some("SE-042"))
        .collect();
    assert!(
        !ni4_warnings.is_empty(),
        "ожидалось предупреждение NI4 для одинаковых условий, получено: {:?}",
        warnings
    );
    assert_eq!(ni4_warnings[0].level, Level::Warning);
}

/// NI4: Перекрывающиеся интервальные условия `level < 10` и `level < 20` -
/// предупреждение NI4.
#[test]
fn test_ni4_interval_overlap_warns() {
    let src =
        std::fs::read_to_string("tests/data/semantic/invalid/condition_overlap_interval.takt")
            .expect("файл condition_overlap_interval.takt должен существовать");
    let (ast, _) = parse(&src, 0).unwrap();
    let model_rc = construct_model(&ast, None, &[]).unwrap();
    let warnings = takt_lang::nondeterministic_transition_warnings(model_rc);
    let ni4_warnings: Vec<_> = warnings
        .iter()
        .filter(|w| w.code.as_deref() == Some("SE-042"))
        .collect();
    assert!(
        !ni4_warnings.is_empty(),
        "ожидалось предупреждение NI4 для перекрывающихся условий level<10 и level<20, получено: {:?}",
        warnings
    );
}

/// NI4: Непересекающиеся условия `level < 10` и `level > 20` - предупреждений NI4 нет.
#[test]
fn test_ni4_non_overlapping_no_warn() {
    let src = std::fs::read_to_string("tests/data/semantic/valid/no_condition_overlap.takt")
        .expect("файл no_condition_overlap.takt должен существовать");
    let (ast, _) = parse(&src, 0).unwrap();
    let model_rc = construct_model(&ast, None, &[]).unwrap();
    let warnings = takt_lang::nondeterministic_transition_warnings(model_rc);
    let ni4_warnings: Vec<_> = warnings
        .iter()
        .filter(|w| w.code.as_deref() == Some("SE-042"))
        .collect();
    assert!(
        ni4_warnings.is_empty(),
        "непересекающиеся условия level<10 и level>20 не должны давать предупреждений NI4: {:?}",
        ni4_warnings
    );
}

/// NI4: Условия `x = 3` и `x < 10` перекрываются (3 < 10) - предупреждение NI4.
#[test]
fn test_ni4_eq_lt_overlap_warns() {
    let src = r#"
var x: [bit;8] := 0;
start S {
    ref A: x = 3;
    ref B: x < 10;
}
state A { ref S: x != 3; }
state B { ref S: x >= 10; }
"#;
    let (ast, _) = parse(src, 0).unwrap();
    let model_rc = construct_model(&ast, None, &[]).unwrap();
    let warnings = takt_lang::nondeterministic_transition_warnings(model_rc);
    let ni4_warnings: Vec<_> = warnings
        .iter()
        .filter(|w| w.code.as_deref() == Some("SE-042"))
        .collect();
    assert!(
        !ni4_warnings.is_empty(),
        "x=3 и x<10 перекрываются (3 < 10) — ожидалось NI4, получено: {:?}",
        warnings
    );
}

/// NI4: Условия `x = 15` и `x < 10` не перекрываются (15 >= 10) - предупреждений NI4
/// нет.
#[test]
fn test_ni4_eq_lt_no_overlap_no_warn() {
    let src = r#"
var x: [bit;8] := 0;
start S {
    ref A: x = 15;
    ref B: x < 10;
}
state A { ref S: x != 15; }
state B { ref S: x >= 10; }
"#;
    let (ast, _) = parse(src, 0).unwrap();
    let model_rc = construct_model(&ast, None, &[]).unwrap();
    let warnings = takt_lang::nondeterministic_transition_warnings(model_rc);
    let ni4_warnings: Vec<_> = warnings
        .iter()
        .filter(|w| w.code.as_deref() == Some("SE-042"))
        .collect();
    assert!(
        ni4_warnings.is_empty(),
        "x=15 и x<10 не перекрываются — не должно быть NI4, получено: {:?}",
        ni4_warnings
    );
}

// --- I5: Ce16 - рекурсивные псевдонимы типов ---------------------------------

/// Ce16: прямая рекурсия `type A = [A; 8]` - ошибка.
#[test]
fn i5_direct_recursive_type_alias_is_error() {
    let err = build_err("type A = [A; 8]; start S;");
    assert_eq!(
        err.code.as_deref(),
        Some("SE-039"),
        "код ошибки Ce16 должен быть SE-039: {:?}",
        err.code
    );
    assert!(
        err.message.contains("'A'") || err.message.contains("A"),
        "ошибка должна упоминать псевдоним 'A': {}",
        err.message
    );
}

/// Ce16: взаимная рекурсия `type A = [B; 4]; type B = [A; 2];` - ошибка.
#[test]
fn i5_mutual_recursive_type_alias_is_error() {
    let err = build_err("type A = [B; 4]; type B = [A; 2]; start S;");
    assert_eq!(
        err.code.as_deref(),
        Some("SE-039"),
        "код ошибки Ce16 должен быть SE-039: {:?}",
        err.code
    );
}

/// Ce16: линейная цепочка без цикла - OK.
#[test]
fn i5_non_recursive_type_alias_ok() {
    let model = build("type A = [bit; 8]; type B = [A; 2]; var x: B := 0; start S;");
    assert!(
        model.types.contains_key("A"),
        "псевдоним A должен быть в types"
    );
    assert!(
        model.types.contains_key("B"),
        "псевдоним B должен быть в types"
    );
}

/// Ce16: прямая рекурсия из тестового файла `recursive_type_alias.takt`.
#[test]
fn i5_file_recursive_type_alias() {
    let src = std::fs::read_to_string("tests/data/semantic/invalid/recursive_type_alias.takt")
        .expect("не удалось прочитать файл");
    let err = build_err(&src);
    assert_eq!(
        err.code.as_deref(),
        Some("SE-039"),
        "код ошибки Ce16 должен быть SE-039: {:?}",
        err.code
    );
}

/// Ce16: взаимная рекурсия из тестового файла `mutual_recursive_type_alias.takt`.
#[test]
fn i5_file_mutual_recursive_type_alias() {
    let src =
        std::fs::read_to_string("tests/data/semantic/invalid/mutual_recursive_type_alias.takt")
            .expect("не удалось прочитать файл");
    let err = build_err(&src);
    assert_eq!(
        err.code.as_deref(),
        Some("SE-039"),
        "код ошибки Ce16 должен быть SE-039: {:?}",
        err.code
    );
}

/// Ce16: корректные псевдонимы из тестового файла `non_recursive_type_alias.takt` - OK.
#[test]
fn i5_file_non_recursive_type_alias_ok() {
    let src = std::fs::read_to_string("tests/data/semantic/valid/non_recursive_type_alias.takt")
        .expect("не удалось прочитать файл");
    let _model = build(&src);
    // Если дошли сюда - нет ошибки Ce16
}

// --- NI3: Структурные типы (Ce17, Ce18) --------------------------------------

/// NI3: базовое объявление структуры парсится и строится без ошибок.
#[test]
fn struct_basic_parses() {
    let model = build("struct Point { x: [bit;16], y: [bit;16] } start Idle;");
    assert!(
        model.structs.contains_key("Point"),
        "структура Point должна быть в model.structs"
    );
}

/// NI3: поля структуры доступны в семантическом узле.
#[test]
fn struct_fields_accessible() {
    let model = build("struct Pair { a: bit, b: bit } start Idle;");
    let s = model
        .structs
        .get("Pair")
        .expect("структура Pair должна существовать");
    assert_eq!(s.fields.len(), 2, "структура Pair должна содержать 2 поля");
    assert!(s.find_field("a").is_some(), "поле a должно быть найдено");
    assert!(s.find_field("b").is_some(), "поле b должно быть найдено");
    assert!(s.find_field("z").is_none(), "поле z не должно существовать");
}

/// NI3: структура регистрируется как тип и переменная с этим типом разрешается.
#[test]
fn struct_as_var_type_resolves() {
    let model =
        build("struct Coord { x: [bit;16], y: [bit;16] } var origin: Coord := 0; start Idle;");
    assert!(
        model.structs.contains_key("Coord"),
        "структура Coord должна быть в model.structs"
    );
    assert!(
        model.variables.contains_key("origin"),
        "переменная origin должна быть объявлена"
    );
}

/// NI3: тестовый файл `struct_basic.takt` - без ошибок.
#[test]
fn struct_basic_file_ok() {
    let src = std::fs::read_to_string("tests/data/semantic/valid/struct_basic.takt")
        .expect("не удалось прочитать файл");
    let _model = build(&src);
}

/// NI3: тестовый файл `struct_as_var_type.takt` - без ошибок.
#[test]
fn struct_as_var_type_file_ok() {
    let src = std::fs::read_to_string("tests/data/semantic/valid/struct_as_var_type.takt")
        .expect("не удалось прочитать файл");
    let _model = build(&src);
}

/// Ce17: дублирующееся поле структуры - ошибка.
#[test]
fn struct_duplicate_field_error() {
    let (ast, _) =
        parse("struct Bad { x: bit, y: bit, x: bit } start Idle;", 0).expect("ошибка разбора");
    let result = construct_model(&ast, None, &[]);
    assert!(
        result.is_err(),
        "дублирующееся поле структуры должно давать ошибку Ce17"
    );
    let err = result.unwrap_err();
    assert_eq!(
        err.code.as_deref(),
        Some("SE-040"),
        "код ошибки Ce17 должен быть SE-040: {:?}",
        err.code
    );
}

/// Ce17: тестовый файл `struct_duplicate_field.takt` - ошибка Ce17.
#[test]
fn struct_duplicate_field_file_error() {
    let src = std::fs::read_to_string("tests/data/semantic/invalid/struct_duplicate_field.takt")
        .expect("не удалось прочитать файл");
    let (ast, _) = parse(&src, 0).expect("ошибка разбора файла");
    let result = construct_model(&ast, None, &[]);
    assert!(
        result.is_err(),
        "файл с дублирующимися полями должен давать ошибку Ce17"
    );
    let err = result.unwrap_err();
    assert_eq!(
        err.code.as_deref(),
        Some("SE-040"),
        "код ошибки Ce17 должен быть SE-040: {:?}",
        err.code
    );
}

// --- Task 1: вызов extern функций ---------------------------------------------

/// Extern-функция в блоке always разрешается без ошибок семантики.
#[test]
fn extern_fn_in_always_resolves_ok() {
    let src = std::fs::read_to_string("tests/data/semantic/valid/extern_fn_in_always.takt")
        .expect("не удалось прочитать файл");
    let (ast, _) = parse(&src, 0).expect("ошибка разбора");
    let result = construct_model(&ast, None, &[]);
    assert!(
        result.is_ok(),
        "extern fn в always не должна давать ошибку: {:?}",
        result.err()
    );
}

/// Extern-функция после локальной переменной разрешается без ошибок.
#[test]
fn extern_fn_after_local_var_resolves_ok() {
    let src = std::fs::read_to_string("tests/data/semantic/valid/extern_fn_local_var.takt")
        .expect("не удалось прочитать файл");
    let (ast, _) = parse(&src, 0).expect("ошибка разбора");
    let result = construct_model(&ast, None, &[]);
    assert!(
        result.is_ok(),
        "extern fn после local var не должна давать ошибку: {:?}",
        result.err()
    );
}

/// Extern-функция в always доступна в функциях модели (`search_func` находит External).
#[test]
fn extern_fn_is_resolvable_via_search_func() {
    use takt_lang::semantic::FunctionDefinitionNode;
    let src = r#"
extern fn my_log(v: u8);
model M {
    var x: u8 := 0;
    start S { always { my_log(x); } }
}
start Root = M;
"#;
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let model_rc = construct_model(&ast, None, &[]).expect("ошибка семантики");
    let model = model_rc.borrow();
    let func = model
        .search_func("my_log")
        .expect("extern fn my_log должна быть найдена");
    assert!(
        matches!(*func.borrow(), FunctionDefinitionNode::External { .. }),
        "extern fn должна быть External, получено: {:?}",
        *func.borrow()
    );
}

// --- Inline formula ----------------------------------------------------------

/// Встроенная формула на уровне модели разрешается и попадает в `model.formulas`.
#[test]
fn test_inline_formula_model_resolved() {
    let (ast, _) =
        takt_lang::parse("var x: bit := false; : x; start S;", 0).expect("ошибка разбора");
    let node = construct_model(&ast, None, &[]).expect("ошибка построения");
    assert_eq!(
        node.borrow().formulas.len(),
        1,
        "ожидалась 1 встроенная формула в модели"
    );
}

// --- Bug #4: переменная в BitAccess-условии ошибочно считается неиспользуемой --

/// Bug #4: переменная, используемая только через битовый доступ в условии перехода
/// (`x.0`), не должна получать предупреждение Ce13.
///
/// До исправления `collect_from_condition` (и `usage_from_condition`) обрабатывали
/// `ConditionNode::BitAccess(_, _) => {}` - внутренний узел полностью игнорировался,
/// поэтому переменная `flag`, упоминаемая только как `flag.0` в условии, не попадала в
/// множество используемых.
#[test]
fn test_var_used_in_condition_bitaccess_no_unused_warning() {
    let src = r#"
        var flag: bit := 0;
        cond BitSet = flag.0;
        start S { ref Done: BitSet; }
        state Done;
    "#;
    let (ast, _) = takt_lang::parse(src, 0).expect("ошибка разбора");
    let model_rc = construct_model(&ast, None, &[]).expect("ошибка построения");
    let warnings = takt_lang::unused_variable_warnings(model_rc);
    let warned_names: Vec<String> = warnings
        .iter()
        .filter_map(|w| {
            if w.code.as_deref() == Some("SE-036") {
                Some(w.message.clone())
            } else {
                None
            }
        })
        .collect();
    assert!(
        !warned_names.iter().any(|m| m.contains("flag")),
        "flag используется в условии через BitAccess — предупреждения быть не должно, получено: {:?}",
        warned_names
    );
}

/// Встроенная формула на уровне состояния разрешается и попадает в `state.formulas`.
#[test]
fn test_inline_formula_state_resolved() {
    let (ast, _) =
        takt_lang::parse("var x: bit := false; start S { : x; }", 0).expect("ошибка разбора");
    let node = construct_model(&ast, None, &[]).expect("ошибка построения");
    let state = node
        .borrow()
        .states
        .get("S")
        .cloned()
        .expect("состояние S не найдено");
    if let StateNode::Simple { formulas, .. } = state {
        assert_eq!(
            formulas.len(),
            1,
            "ожидалась 1 встроенная формула в состоянии S"
        );
    } else {
        panic!("ожидался Simple state");
    }
}

// ---: диагностика недостижимых состояний ---------------------------

/// Изолированное состояние без входящих переходов генерирует предупреждение SE-046.
#[test]
fn test_unreachable_state_generates_warning() {
    let src = "start A { ref B; } state B; state Orphan;";
    let (ast, _) = takt_lang::parse(src, 0).expect("ошибка разбора");
    let model = construct_model(&ast, None, &[]).expect("ошибка построения");
    let warnings = takt_lang::unreachable_state_warnings(model);
    assert_eq!(
        warnings.len(),
        1,
        "ожидалось 1 предупреждение SE-046, получено: {:?}",
        warnings
    );
    assert_eq!(warnings[0].code.as_deref(), Some("SE-046"));
    assert!(warnings[0].message.contains("Orphan"));
}

/// Модель без недостижимых состояний не генерирует предупреждений.
#[test]
fn test_all_reachable_states_no_warning() {
    let src = "start A { ref B: true; } state B { ref A: true; }";
    let (ast, _) = takt_lang::parse(src, 0).expect("ошибка разбора");
    let model = construct_model(&ast, None, &[]).expect("ошибка построения");
    let warnings = takt_lang::unreachable_state_warnings(model);
    assert!(
        warnings.is_empty(),
        "предупреждений быть не должно: {:?}",
        warnings
    );
}

/// Несколько недостижимых состояний - несколько предупреждений.
#[test]
fn test_multiple_unreachable_states() {
    let src = "start A; state Ghost1; state Ghost2;";
    let (ast, _) = takt_lang::parse(src, 0).expect("ошибка разбора");
    let model = construct_model(&ast, None, &[]).expect("ошибка построения");
    let warnings = takt_lang::unreachable_state_warnings(model);
    assert_eq!(
        warnings.len(),
        2,
        "ожидалось 2 предупреждения SE-046, получено: {:?}",
        warnings
    );
}

// ---: `from` как ключевое слово ------------------------------------

/// `from` больше не является допустимым идентификатором - разбор должен выдать ошибку.
#[test]
fn test_from_is_reserved_keyword_not_identifier() {
    let result = takt_lang::parse("var from: bit = 0; start S;", 0);
    assert!(
        result.is_err(),
        "ожидалась ошибка разбора — 'from' зарезервировано"
    );
}

/// Синтаксис `import {{ A }} from \"file\"` разбирается без ошибок.
#[test]
fn test_import_from_keyword_parses_correctly() {
    use takt_lang::parser::ast::ModelElement;
    // Создаём минимальный импорт (файл не существует, проверяем только АСД)
    let (ast, errs) = takt_lang::parse(r#"import { A } from "shared.takt";"#, 0)
        .expect("ошибка разбора import from");
    assert!(errs.is_empty(), "ошибок разбора быть не должно: {:?}", errs);
    let has_import = ast
        .elements
        .iter()
        .any(|e| matches!(e, ModelElement::Import(_)));
    assert!(has_import, "ожидался элемент Import в АСД");
}

// ---: предупреждение о неизвестных именованных блоках --------------
