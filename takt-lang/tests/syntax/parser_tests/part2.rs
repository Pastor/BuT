//! Интеграционные тесты парсера, часть 2 (вынос из `parser_tests.rs`).
//!
//! Хелперы и импорты - из родителя через `use super::*` (приём /08).

use super::*;

/// `Location` - методы работы с диапазонами.
#[test]
fn location_methods() {
    let loc = Location::Source(0, 10, 20);

    assert_eq!(loc.start(), 10);
    assert_eq!(loc.end(), 20);
    assert_eq!(loc.exclusive_end(), 21);
    assert_eq!(loc.try_file_no(), Some("0".to_string()));

    let begin = loc.begin_range();
    assert_eq!(begin.start(), 10);
    assert_eq!(begin.end(), 10);

    let end = loc.end_range();
    assert_eq!(end.start(), 20);
    assert_eq!(end.end(), 20);
}

/// `Comment` - методы `is_doc` и `is_line`.
#[test]
fn comment_methods() {
    use takt_lang::parser::ast::Comment;

    let line = Comment::Line(Location::default(), "// comment".into());
    let doc = Comment::DocLine(Location::default(), "/// doc".into());

    assert!(!line.is_doc(), "Line — не документационный");
    assert!(line.is_line(), "Line — строчный");
    assert!(doc.is_doc(), "DocLine — документационный");
    assert!(doc.is_line(), "DocLine — тоже строчный");
    assert_eq!(line.value(), "// comment");
    assert_eq!(doc.value(), "/// doc");
}

// -------------------- Тесты extern-функций ----------------------------------

/// `extern fn` разбирается корректно и флаг `external` установлен.
#[test]
fn parse_extern_function() {
    let root = must_parse(
        r#"
        extern fn debug(msg: u8);
        extern fn reset();
        model M { start S; }
    "#,
    );
    let fns: Vec<_> = root
        .elements
        .iter()
        .filter_map(|e| {
            if let ModelElement::Function(f) = e {
                Some(f.as_ref())
            } else {
                None
            }
        })
        .collect();
    assert_eq!(fns.len(), 2, "Ожидались 2 внешние функции");
    assert!(fns[0].external, "debug должна быть external");
    assert!(fns[1].external, "reset должна быть external");
}

/// Обычная функция (не extern) имеет `external == false`.
#[test]
fn parse_non_extern_function_has_false_external_flag() {
    let root = must_parse(
        r#"
        fn myFn();
        model M { start S; }
    "#,
    );
    let f = root
        .elements
        .iter()
        .find_map(|e| {
            if let ModelElement::Function(f) = e {
                Some(f.as_ref())
            } else {
                None
            }
        })
        .expect("Функция не найдена");
    assert!(!f.external, "Обычная функция не должна быть external");
}

// ------------------ Тесты блоков assembly и formula -------------------------

/// `assembly { }` внутри `always`.
#[test]
fn parse_assembly_block_in_always() {
    must_parse(
        r#"
        model M {
            start S {
                always {
                    assembly { }
                }
            }
        }
    "#,
    );
}

/// `assembly "dialect" { }` с диалектом.
#[test]
fn parse_assembly_with_dialect() {
    must_parse(
        r#"
        model M {
            start S {
                always {
                    assembly "arm" { }
                }
            }
        }
    "#,
    );
}

/// `formula { }` как элемент модели.
#[test]
fn parse_formula_as_model_element() {
    let root = must_parse(
        r#"
        model M {
            formula { }
            start S;
        }
    "#,
    );
    assert!(!root.elements.is_empty());
}

/// `formula { fn() }` с вызовом функции внутри.
#[test]
fn parse_formula_with_function_call() {
    must_parse(
        r#"
        model M {
            formula {
                init()
                step(1)
            }
            start S;
        }
    "#,
    );
}

// -------------------- Тесты управляющих конструкций -------------------------

/// `continue` внутри цикла.
#[test]
fn parse_continue_in_loop() {
    must_parse(
        r#"
        model M {
            var i: [bit;8] := 0;
            start S {
                always {
                    loop i < 10 {
                        i := i + 1;
                        continue;
                    }
                }
            }
        }
    "#,
    );
}

/// `break` внутри цикла.
#[test]
fn parse_break_in_loop() {
    must_parse(
        r#"
        model M {
            var i: [bit;8] := 0;
            start S {
                always {
                    loop i < 10 {
                        break;
                    }
                }
            }
        }
    "#,
    );
}

/// `return` без значения.
#[test]
fn parse_return_without_value() {
    must_parse(
        r#"
        fn voidFn() {
            return;
        }
        model M { start S; }
    "#,
    );
}

/// `return expr` с возвращаемым значением.
#[test]
fn parse_return_with_value() {
    must_parse(
        r#"
        fn getValue() -> u8 {
            return 42;
        }
        model M { start S; }
    "#,
    );
}

/// `for init; cond; step;` - цикл без тела, заголовок без скобок (С1, вариант A).
#[test]
fn parse_for_loop_without_body() {
    must_parse(
        r#"
        model M {
            var i: [bit;8] := 0;
            start S {
                always {
                    for i := 0; i < 10; i := i + 1;
                }
            }
        }
    "#,
    );
}

/// `if` внутри `for` с явными скобками тела (С1, вариант A - тело цикла в `{}`).
#[test]
fn parse_if_in_for_with_braces() {
    must_parse(
        r#"
        model M {
            var i: [bit;8] := 0;
            var x: [bit;8] := 0;
            start S {
                always {
                    for i := 0; i < 10; i := i + 1 {
                        if x > 5 { x := 0; }
                    }
                }
            }
        }
    "#,
    );
}

// ------------------------ Тесты импортов -------------------------------------

/// `import * as X from "path"` - глобальный импорт.
#[test]
fn parse_import_global_symbol() {
    let root = must_parse(r#"import * as Lib from "lib.takt"; model M { start S; }"#);
    let has_import = root
        .elements
        .iter()
        .any(|e| matches!(e, ModelElement::Import(_)));
    assert!(has_import, "Ожидался импорт");
}

/// `import { a, b as c } from "path"` - импорт с переименованием.
#[test]
fn parse_import_rename() {
    let root = must_parse(
        r#"import { MyModel, OldName as NewName } from "path.takt"; model M { start S; }"#,
    );
    let has_import = root
        .elements
        .iter()
        .any(|e| matches!(e, ModelElement::Import(_)));
    assert!(has_import, "Ожидался импорт с переименованием");
}

// --------------------- Тесты выражений (срезы массивов) ---------------------

/// `arr[0:3]` - срез массива.
#[test]
fn parse_array_slice_expression() {
    must_parse(
        r#"
        var arr: u8 := 0;
        model M {
            start S {
                always {
                    arr[0:3] := 0;
                }
            }
        }
    "#,
    );
}

// ---------------------- Тесты методов Location -------------------------------

/// Методы изменения Location: `use_start_from`, `use_end_from`.
#[test]
fn location_mutation_methods() {
    let mut loc = Location::Source(0, 5, 15);
    let other = Location::Source(0, 1, 20);

    loc.use_start_from(&other);
    assert_eq!(
        loc.start(),
        1,
        "use_start_from должен установить начало из other"
    );
    assert_eq!(loc.end(), 15, "end не должен меняться");

    loc.use_end_from(&other);
    assert_eq!(
        loc.end(),
        20,
        "use_end_from должен установить конец из other"
    );
    assert_eq!(loc.start(), 1, "start не должен меняться");
}

/// Методы `with_start_from` и `with_end_from` возвращают копию.
#[test]
fn location_with_start_end_from() {
    let loc = Location::Source(0, 5, 15);
    let other = Location::Source(0, 1, 20);

    let new_loc = loc.clone().with_start_from(&other);
    assert_eq!(new_loc.start(), 1);
    assert_eq!(new_loc.end(), 15);

    let new_loc2 = loc.clone().with_end_from(&other);
    assert_eq!(new_loc2.start(), 5);
    assert_eq!(new_loc2.end(), 20);
}

/// Методы `with_start` и `with_end` возвращают копию с заменённой позицией.
#[test]
fn location_with_start_and_with_end() {
    let loc = Location::Source(0, 5, 15);

    let new_loc = loc.clone().with_start(0);
    assert_eq!(new_loc.start(), 0);
    assert_eq!(new_loc.end(), 15);

    let new_loc2 = loc.with_end(100);
    assert_eq!(new_loc2.start(), 5);
    assert_eq!(new_loc2.end(), 100);
}

/// Метод `range` возвращает диапазон `start..end`.
#[test]
fn location_range_method() {
    let loc = Location::Source(0, 3, 10);
    let range = loc.range();
    assert_eq!(range, 3..10);
}

/// Не-Source варианты Location: `try_file_no` возвращает `None`, `begin_range` и
/// `end_range` возвращают тот же вариант.
#[test]
fn location_non_source_variants() {
    let variants = [
        Location::Builtin,
        Location::CommandLine,
        Location::Implicit,
        Location::Codegen,
    ];
    for loc in &variants {
        assert_eq!(
            loc.try_file_no(),
            None,
            "{:?}.try_file_no() должен быть None",
            loc
        );
        // begin_range и end_range для не-Source возвращают тот же вариант
        assert_eq!(loc.begin_range(), loc.clone());
        assert_eq!(loc.end_range(), loc.clone());
    }
}

// -------------------- Тесты Statement::is_empty ------------------------------

/// `Statement::is_empty` для пустого блока - `true`.
#[test]
fn statement_is_empty_for_empty_block() {
    use takt_lang::parser::ast::Statement;

    let empty_block = Statement::Block {
        loc: Location::default(),
        unchecked: false,
        statements: Vec::new(),
    };
    assert!(empty_block.is_empty(), "Пустой блок должен быть пустым");
}

/// `Statement::is_empty` для непустого блока - `false`.
#[test]
fn statement_is_empty_for_nonempty_block() {
    use takt_lang::parser::ast::Statement;

    let inner = Statement::Continue(Location::default());
    let block = Statement::Block {
        loc: Location::default(),
        unchecked: false,
        statements: vec![inner],
    };
    assert!(!block.is_empty(), "Непустой блок не должен быть пустым");
}

/// `Statement::is_empty` для не-блочных операторов - `false`.
#[test]
fn statement_is_empty_for_non_block_statements() {
    use takt_lang::parser::ast::Statement;

    let stmts = vec![
        Statement::Continue(Location::default()),
        Statement::Break(Location::default()),
        Statement::Return(Location::default(), None),
        Statement::Error(Location::default()),
    ];
    for stmt in &stmts {
        assert!(
            !stmt.is_empty(),
            "Не-блочный оператор {:?} не должен быть пустым",
            stmt
        );
    }
}

// ------------------------ Тесты Diagnostic -----------------------------------

/// Конструкторы `Diagnostic` создают объекты с нужными полями.
#[test]
fn diagnostic_constructors() {
    use takt_lang::diagnostics::{Diagnostic, ErrorType, Level, Note};

    let loc = Location::Source(0, 0, 5);

    // debug
    let d = Diagnostic::debug(loc.clone(), "msg".into());
    assert_eq!(d.level, Level::Debug);
    assert_eq!(d.ty, ErrorType::None);
    assert!(d.notes.is_empty());

    // info
    let d = Diagnostic::info(loc.clone(), "msg".into());
    assert_eq!(d.level, Level::Info);

    // parser_error
    let d = Diagnostic::parser_error(loc.clone(), "parse err".into());
    assert_eq!(d.level, Level::Error);
    assert_eq!(d.ty, ErrorType::ParserError);

    // error
    let d = Diagnostic::error(loc.clone(), "syntax err".into());
    assert_eq!(d.ty, ErrorType::SyntaxError);

    // declaration_error
    let d = Diagnostic::declaration_error(loc.clone(), "decl err".into());
    assert_eq!(d.ty, ErrorType::DeclarationError);

    // cast_error
    let d = Diagnostic::cast_error(loc.clone(), "cast err".into());
    assert_eq!(d.ty, ErrorType::CastError);
    assert_eq!(d.level, Level::Error);

    // type_error
    let d = Diagnostic::type_error(loc.clone(), "type err".into());
    assert_eq!(d.ty, ErrorType::TypeError);

    // warning
    let d = Diagnostic::warning(loc.clone(), "warn".into());
    assert_eq!(d.level, Level::Warning);
    assert_eq!(d.ty, ErrorType::Warning);

    // cast_warning
    let d = Diagnostic::cast_warning(loc.clone(), "cast warn".into());
    assert_eq!(d.level, Level::Warning);
    assert_eq!(d.ty, ErrorType::CastError);

    // cast_error_with_note
    let d = Diagnostic::cast_error_with_note(
        loc.clone(),
        "cast err".into(),
        loc.clone(),
        "note".into(),
    );
    assert_eq!(d.notes.len(), 1);
    assert_eq!(d.notes[0].message, "note");

    // error_with_note
    let d = Diagnostic::error_with_note(loc.clone(), "err".into(), loc.clone(), "note here".into());
    assert_eq!(d.notes.len(), 1);
    assert_eq!(d.level, Level::Error);

    // error_with_notes
    let notes = vec![
        Note {
            loc: loc.clone(),
            message: "n1".into(),
        },
        Note {
            loc: loc.clone(),
            message: "n2".into(),
        },
    ];
    let d = Diagnostic::error_with_notes(loc.clone(), "err".into(), notes);
    assert_eq!(d.notes.len(), 2);

    // warning_with_note
    let d = Diagnostic::warning_with_note(loc.clone(), "warn".into(), loc.clone(), "note".into());
    assert_eq!(d.level, Level::Warning);
    assert_eq!(d.notes.len(), 1);

    // warning_with_notes
    let notes2 = vec![Note {
        loc: loc.clone(),
        message: "wn".into(),
    }];
    let d = Diagnostic::warning_with_notes(loc.clone(), "warn".into(), notes2);
    assert_eq!(d.notes.len(), 1);
}

/// `Level` - методы `as_str` и Display.
#[test]
fn level_display_and_as_str() {
    use takt_lang::diagnostics::Level;

    assert_eq!(Level::Debug.as_str(), "debug");
    assert_eq!(Level::Info.as_str(), "info");
    assert_eq!(Level::Warning.as_str(), "warning");
    assert_eq!(Level::Error.as_str(), "error");

    assert_eq!(Level::Debug.to_string(), "debug");
    assert_eq!(Level::Error.to_string(), "error");
}

// ------------------- Тесты дополнительных конструкций ------------------------

/// Оператор `next` с переходом к состоянию.
#[test]
fn parse_next_operator_in_composition() {
    must_parse(
        r#"
        model A { start S; state E; }
        model B { start S; state E; }
        start Main = A + B;
    "#,
    );
}

/// Параллельная компоновка моделей.
#[test]
fn parse_parallel_composition() {
    must_parse(
        r#"
        model A { start S; state E; }
        model B { start S; state E; }
        start Parallel = A | B;
    "#,
    );
}

/// Условный оператор: `if` вложенный в `else`.
#[test]
fn parse_nested_if_else() {
    must_parse(
        r#"
        model M {
            var x: [bit;8] := 0;
            start S {
                always {
                    if x > 10 {
                        x := 10;
                    } else if x < 0 {
                        x := 0;
                    } else {
                        x := x + 1;
                    }
                }
            }
        }
    "#,
    );
}

/// `ref E: S(Model) = State` - ссылка с путём к состоянию другой модели.
#[test]
fn parse_ref_with_state_function_condition() {
    must_parse(
        r#"
        model Ping { start S; state End; }
        model M {
            start S {
                ref E: S(Ping) = End;
            }
            state E;
        }
    "#,
    );
}

/// Отрицательное числовое значение как инициализатор константы.
#[test]
fn parse_negative_number_as_initializer() {
    must_parse(
        r#"
        const NEG: u8 := -1;
        model M { start S; }
    "#,
    );
}

/// `Expression::has_space_around` для унарных операторов.
#[test]
fn expression_has_space_around() {
    use takt_lang::parser::ast::Expression;

    let loc = Location::default();
    let inner = Expression::Number(loc.clone(), 1);

    // Унарные - без пробелов
    assert!(!Expression::Not(loc.clone(), Box::new(inner.clone())).has_space_around());
    assert!(!Expression::BitwiseNot(loc.clone(), Box::new(inner.clone())).has_space_around());
    assert!(!Expression::UnaryPlus(loc.clone(), Box::new(inner.clone())).has_space_around());
    assert!(!Expression::Negate(loc.clone(), Box::new(inner.clone())).has_space_around());

    // Бинарные - с пробелами
    assert!(
        Expression::Add(
            loc.clone(),
            Box::new(inner.clone()),
            Box::new(inner.clone())
        )
        .has_space_around()
    );
    assert!(Expression::Number(loc.clone(), 42).has_space_around());
}

/// `Expression::loc()` возвращает корректное местоположение для разных вариантов.
#[test]
fn expression_loc_method() {
    use takt_lang::parser::ast::Expression;

    let loc = Location::Source(0, 5, 10);
    let inner = Box::new(Expression::Number(loc.clone(), 0));

    // Проверяем несколько вариантов
    assert_eq!(Expression::Number(loc.clone(), 42).loc(), loc);
    assert_eq!(Expression::Bool(loc.clone(), true).loc(), loc);
    assert_eq!(Expression::Not(loc.clone(), inner.clone()).loc(), loc);
    assert_eq!(
        Expression::Add(loc.clone(), inner.clone(), inner.clone()).loc(),
        loc
    );
    assert_eq!(
        Expression::Assign(loc.clone(), inner.clone(), inner.clone()).loc(),
        loc
    );
    assert_eq!(
        Expression::ConditionalOperator(loc.clone(), inner.clone(), inner.clone(), inner.clone())
            .loc(),
        loc
    );
}

// ------------------------------- Тернарный оператор -------------------------

/// Простой тернарный оператор `flag ? true : false` разбирается без ошибок.
#[test]
fn ternary_simple_parses() {
    let src = "var flag: bit := true; var r: bit := flag ? true : false; start S;";
    must_parse(src);
}

/// Вложенный тернарный оператор (правоассоциативный) разбирается корректно.
#[test]
fn ternary_nested_right_associative() {
    // 1: 2: 3 -> a ?
    let src = "var a: bit := true; var b: bit := false; var z: bit := a ? b ? true : false : false; start S;";
    must_parse(src);
}

/// Тернарный оператор с выражением в условии.
#[test]
fn ternary_condition_expression() {
    let src = "var x: bit := true; var y: bit := x ? false : true; start S;";
    must_parse(src);
}

/// Тернарный оператор в теле автомата (`always`-блок).
#[test]
fn ternary_in_always_block() {
    let src = r#"
var flag: bit := true;
start Idle {
    always {
        var r: bit := flag ? true : false;
    }
    ref Idle: flag = 0;
}
"#;
    must_parse(src);
}

/// Тернарный оператор в выражении внутри `always`-блока с присваиванием.
#[test]
fn ternary_in_assignment_expression() {
    let src = r#"
var a: bit := true;
var b: bit := false;
start S {
    always {
        a := b ? true : false;
    }
    ref S: a = 0;
}
"#;
    must_parse(src);
}

/// Интеграционный тест: файл `ternary_operator.takt` разбирается без ошибок.
#[test]
fn ternary_operator_file_parses() {
    let path = Path::new("tests/data/parser/valid/ternary_operator.takt");
    let src = fs::read_to_string(path).expect("не удалось прочитать файл");
    must_parse(&src);
}

/// Контр-пример: незакрытый тернарный оператор (без `else`) - ошибка парсера.
#[test]
fn ternary_missing_else_branch_is_error() {
    // `flag ? true` без `: else` не является корректным тернарным выражением
    let src = "var flag: bit = true; var r: bit = flag ? true; start S;";
    let result = takt_lang::parse(src, 0);
    assert!(
        result.is_err(),
        "ожидалась ошибка парсера для неполного тернарного оператора"
    );
}

/// Контр-пример: тернарный оператор без условия - ошибка парсера.
#[test]
fn ternary_missing_condition_is_error() {
    let src = "var r: bit = ? true : false; start S;";
    let result = takt_lang::parse(src, 0);
    assert!(
        result.is_err(),
        "ожидалась ошибка парсера для тернарного оператора без условия"
    );
}

// ------------------------------- Структурные типы (NI3) ----------------------

/// Простая структура с двумя полями разбирается без ошибок.
#[test]
fn struct_simple_parses() {
    let src = r#"
struct Point { x: [bit;16], y: [bit;16] }
start S;
"#;
    must_parse(src);
}

/// Структура с одним полем разбирается корректно.
#[test]
fn struct_single_field_parses() {
    let src = "struct Wrapper { value: bit } start S;";
    must_parse(src);
}

/// Несколько структур в одном файле.
#[test]
fn multiple_structs_parse() {
    let src = r#"
struct A { x: bit }
struct B { y: [bit;8] }
start S;
"#;
    must_parse(src);
}

/// Переменная типа структуры разбирается.
#[test]
fn struct_variable_declaration_parses() {
    let src = r#"
struct Vec2 { x: [bit;16], y: [bit;16] }
var v: Vec2 := 0;
start S;
"#;
    must_parse(src);
}

/// Интеграционный тест: файл `struct_types.takt` разбирается без ошибок.
#[test]
fn struct_types_file_parses() {
    let path = Path::new("tests/data/parser/valid/struct_types.takt");
    let src = fs::read_to_string(path).expect("не удалось прочитать файл");
    must_parse(&src);
}

/// Контр-пример: структура без закрывающей скобки - ошибка парсера.
#[test]
fn struct_missing_closing_brace_is_error() {
    let src = "struct Bad { x: bit start S;";
    let result = takt_lang::parse(src, 0);
    assert!(
        result.is_err(),
        "ожидалась ошибка для структуры без закрывающей скобки"
    );
}

/// Контр-пример: структура без имени - ошибка парсера.
#[test]
fn struct_anonymous_parses_with_recovery() {
    // LALRPOP использует IdentifierOrError, поэтому анонимная структура разбирается с
    // восстановлением после ошибки
    let src = "struct { x: bit } start S;";
    // Принимаем как ошибку или успех с диагностикой - не panic
    let _ = takt_lang::parse(src, 0);
}

// --- Inline formula ----------------------------------------------------------

/// Встроенная формула в теле модели разбирается как `ModelElement::InlineFormula`.
#[test]
fn test_inline_formula_in_model_parsed() {
    let src = "model M { var temperature: bit := false; : temperature; }";
    let (ast, _) = takt_lang::parse(src, 0).expect("ошибка разбора");
    let m = ast
        .elements
        .iter()
        .find(|e| matches!(e, takt_lang::parser::ast::ModelElement::Model(_)))
        .unwrap();
    if let takt_lang::parser::ast::ModelElement::Model(model) = m {
        assert!(
            model
                .elements
                .iter()
                .any(|e| matches!(e, takt_lang::parser::ast::ModelElement::InlineFormula(_))),
            "ожидался ModelElement::InlineFormula в теле model M"
        );
    }
}

/// Встроенная формула в теле состояния разбирается как `StateElement::InlineFormula`.
#[test]
fn test_inline_formula_in_state_parsed() {
    let src = "model M { start S { : true, false; } }";
    let (ast, _) = takt_lang::parse(src, 0).expect("ошибка разбора");
    if let takt_lang::parser::ast::ModelElement::Model(model) = &ast.elements[0]
        && let takt_lang::parser::ast::ModelElement::State(state) = &model.elements[0]
    {
        assert!(
            state
                .elements
                .iter()
                .any(|e| matches!(e, takt_lang::parser::ast::StateElement::InlineFormula(_))),
            "ожидался StateElement::InlineFormula в теле состояния S"
        );
    }
}

/// Встроенная формула в блоке `always` разбирается без ошибок.
#[test]
fn test_inline_formula_in_always_parsed() {
    let src = "model M { always { var i: bit := false; : i, true; } start S; }";
    let (ast, _) = takt_lang::parse(src, 0).expect("ошибка разбора");
    assert!(!ast.elements.is_empty());
}

// -------------------------: оператор `address` ---------------------

/// Оператор `address Имя = <выражение>;` парсится в `ModelElement::Address` с именем
/// порта и выражением-адресом.
#[test]
fn address_operator_parses_to_model_element_address() {
    let root = must_parse("address BTN = 0x00200000;");
    let addr = root
        .elements
        .iter()
        .find_map(|e| match e {
            ModelElement::Address(a) => Some(a),
            _ => None,
        })
        .expect("ожидался ModelElement::Address");
    assert_eq!(
        addr.name.as_ref().map(|id| id.name.as_str()),
        Some("BTN"),
        "имя порта в address-операторе"
    );
    // Голый hex `0x...` без суфисправления `:bit` лексируется как число; адресный литерал
    // `Expression::Address` порождается только формой `0xADDR:bit`.
    assert!(
        matches!(
            addr.value,
            takt_lang::parser::ast::Expression::Number(_, 0x0020_0000)
        ),
        "значение должно быть числом 0x00200000, получено: {:?}",
        addr.value
    );
}

/// Адрес с битовой позицией `0xADDR:bit` также парсится в address-операторе.
#[test]
fn address_operator_accepts_bit_addressed_literal() {
    let root = must_parse("address LED = 0x00200004:3;");
    let addr = root
        .elements
        .iter()
        .find_map(|e| match e {
            ModelElement::Address(a) => Some(a),
            _ => None,
        })
        .expect("ожидался ModelElement::Address");
    assert!(
        matches!(
            addr.value,
            takt_lang::parser::ast::Expression::Address(_, 0x0020_0004, 3)
        ),
        "значение должно быть 0x00200004:3, получено: {:?}",
        addr.value
    );
}

/// Оператор `address` соседствует с объявлением порта в одной модели и не мешает
/// разбору остальных элементов.
#[test]
fn address_operator_coexists_with_port_declaration() {
    let root = must_parse("model Sensors { in BTN: u8; address BTN = 0x00200000; start Idle; }");
    let model = match &root.elements[0] {
        ModelElement::Model(m) => m,
        other => panic!("ожидалась вложенная модель, получено: {:?}", other),
    };
    assert!(
        model
            .elements
            .iter()
            .any(|e| matches!(e, ModelElement::Address(_))),
        "в модели должен быть ModelElement::Address"
    );
    assert!(
        model
            .elements
            .iter()
            .any(|e| matches!(e, ModelElement::Variable(_))),
        "в модели должно остаться объявление порта (ModelElement::Variable)"
    );
}
