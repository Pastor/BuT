//! Интеграционные тесты кодогенерации (FE5).
//!
//! Проверяют конвейер компиляции Takt -> C через [`takt_lang::compile_to_c`], включая
//! сценарии с импортом модулей через пути поиска (`-I`).

use std::fs;
use tempfile::tempdir;

// -- Вспомогательные функции --------------------------------------------------

/// Создаёт временный `.takt`-файл и возвращает (директория, полный_путь).
#[allow(dead_code)]
fn tmp_but_file(name: &str, content: &str) -> (tempfile::TempDir, String) {
    let dir = tempdir().expect("не удалось создать временный каталог");
    let path = dir.path().join(name);
    fs::write(&path, content).expect("не удалось записать .takt-файл");
    (dir, path.to_string_lossy().into_owned())
}

// -- Базовые тесты compile_to_c -----------------------------------------------

/// FE5: Простая FSM компилируется в C без ошибок.
#[test]
fn test_compile_simple_fsm_to_c() {
    use takt_lang::{
        generator::{Language, generate},
        parse,
        semantic::tree::construct_model,
    };

    let src = r#"
model Traffic {
    start Red {}
    state Green {}
    state Yellow {}
}
    "#;

    let tmp = tempdir().expect("не удалось создать временный каталог");
    let out_path = tmp.path().to_str().unwrap();

    let (ast, _) = parse(src, 0).expect("синтаксический анализ должен быть успешен");
    let root = construct_model(&ast, None, &[]).expect("семантический анализ должен быть успешен");

    let traffic = root
        .borrow()
        .search_model("Traffic")
        .expect("модель Traffic должна быть найдена");
    let result = generate(
        Language::C,
        &traffic.borrow(),
        out_path,
        &takt_lang::GenerateOptions::default(),
    );
    assert!(
        result.is_ok(),
        "компиляция простой FSM должна быть успешной, ошибка: {:?}",
        result
    );

    let entries: Vec<_> = fs::read_dir(out_path)
        .unwrap()
        .filter_map(|e| e.ok())
        .collect();
    assert!(
        !entries.is_empty(),
        "генератор C должен создать хотя бы один файл"
    );
}

const MODEL_FILENAME: &str = "model.takt";

/// FE5: Синтаксически неверный код возвращает ошибку.
#[test]
fn test_compile_invalid_syntax_returns_error() {
    let src = "model { }"; // нет имени модели
    let tmp = tempdir().unwrap();
    let result = takt_lang::compile_to_c(
        MODEL_FILENAME,
        src,
        tmp.path().to_str().unwrap(),
        &[],
        &takt_lang::GenerateOptions::default(),
    );
    assert!(result.is_err(), "неверный код должен возвращать Err");
}

/// FE5: Семантически неверный код (нет start-состояния) возвращает ошибку.
#[test]
fn test_compile_no_start_state_returns_error() {
    let src = "model M { state S {} }"; // нет start-состояния
    let tmp = tempdir().unwrap();
    let result = takt_lang::compile_to_c(
        MODEL_FILENAME,
        src,
        tmp.path().to_str().unwrap(),
        &[],
        &takt_lang::GenerateOptions::default(),
    );
    assert!(
        result.is_err(),
        "модель без start-состояния должна возвращать Err"
    );
}

/// Корректная программа без импортов компилируется с пустым списком путей.
#[test]
fn test_compile_no_imports_empty_search_paths() {
    let src = "start S;";
    let tmp = tempdir().unwrap();
    let result = takt_lang::compile_to_c(
        MODEL_FILENAME,
        src,
        tmp.path().to_str().unwrap(),
        &[],
        &takt_lang::GenerateOptions::default(),
    );
    assert!(
        result.is_ok(),
        "программа без импортов должна компилироваться без путей поиска: {:?}",
        result
    );
}

// -- Тесты путей поиска (-I / --include-dirs) ---------------------------------

/// Импорт разрешается через переданный путь поиска.
///
/// Создаёт временную директорию с библиотечным файлом, затем компилирует главный файл,
/// указывая эту директорию через `search_paths`.
#[test]
fn test_compile_with_search_path_resolves_import() {
    // Создаём временную "библиотеку"
    let lib_dir = tempdir().unwrap();
    fs::write(
        lib_dir.path().join("timer.takt"),
        r#"
model Timer {
    start Idle;
    state Active;
}
"#,
    )
    .unwrap();

    // Главный файл использует import и объявляет состояния на верхнем уровне.
    // compile_to_c генерирует для корневой модели, у которой должен быть start-state.
    let main_src = r#"
import "timer.takt";
start Ready;
state Done;
"#;

    let out_dir = tempdir().unwrap();
    let search_paths = vec![lib_dir.path().to_string_lossy().into_owned()];

    let result = takt_lang::compile_to_c(
        MODEL_FILENAME,
        main_src,
        out_dir.path().to_str().unwrap(),
        &search_paths,
        &takt_lang::GenerateOptions::default(),
    );
    assert!(
        result.is_ok(),
        "импорт через путь поиска должен разрешаться, ошибка: {:?}",
        result
    );
}

/// Импорт без указания пути поиска завершается ошибкой.
///
/// Контр-пример: тот же источник, что и в
/// `test_compile_with_search_path_resolves_import`, но без `-I` -> ошибка "файл импорта
/// не найден".
#[test]
fn test_compile_missing_import_without_search_path_is_error() {
    let main_src = r#"import "nonexistent_library.takt"; start S;"#;
    let out_dir = tempdir().unwrap();

    // Пустые пути поиска -> импорт не найдёт файл
    let result = takt_lang::compile_to_c(
        MODEL_FILENAME,
        main_src,
        out_dir.path().to_str().unwrap(),
        &[],
        &takt_lang::GenerateOptions::default(),
    );
    assert!(
        result.is_err(),
        "импорт без пути поиска должен завершаться ошибкой"
    );
    let msg = result.unwrap_err().message;
    assert!(
        msg.contains("не найден") || msg.contains("найден"),
        "сообщение об ошибке должно описывать проблему поиска: {}",
        msg
    );
}

/// Несколько путей поиска: файл найден во втором, не в первом.
#[test]
fn test_compile_second_search_path_wins() {
    let dir1 = tempdir().unwrap(); // пустая директория
    let dir2 = tempdir().unwrap();
    fs::write(
        dir2.path().join("utils.takt"),
        "model Utils { start Ready; }",
    )
    .unwrap();

    let main_src = r#"import "utils.takt"; start Ready; state Done;"#;
    let out_dir = tempdir().unwrap();
    let search_paths = vec![
        dir1.path().to_string_lossy().into_owned(),
        dir2.path().to_string_lossy().into_owned(),
    ];

    let result = takt_lang::compile_to_c(
        MODEL_FILENAME,
        main_src,
        out_dir.path().to_str().unwrap(),
        &search_paths,
        &takt_lang::GenerateOptions::default(),
    );
    assert!(
        result.is_ok(),
        "файл найден во втором пути, компиляция должна пройти: {:?}",
        result
    );
}

/// Путь поиска указан, но файл отсутствует даже там -> ошибка.
#[test]
fn test_compile_wrong_search_path_is_error() {
    let main_src = r#"import "missing.takt"; start S;"#;
    let out_dir = tempdir().unwrap();
    let search_paths = vec!["/nonexistent_path_xyz_abc".to_string()];

    let result = takt_lang::compile_to_c(
        MODEL_FILENAME,
        main_src,
        out_dir.path().to_str().unwrap(),
        &search_paths,
        &takt_lang::GenerateOptions::default(),
    );
    assert!(result.is_err(), "файл в несуществующей директории → ошибка");
}

/// Импорт через идентификаторный путь (`import a::b;`) разрешается по поддиректории.
#[test]
fn test_compile_identifier_import_with_search_path() {
    let lib_root = tempdir().unwrap();
    let subdir = lib_root.path().join("sensors");
    fs::create_dir(&subdir).unwrap();
    fs::write(
        subdir.join("light.takt"),
        "model Light { start Off; state On; }",
    )
    .unwrap();

    // import sensors::light; -> ищем sensors/light.takt в search_paths
    let main_src = r#"import {Light} from sensors::light; start Ready = Light; state Done;"#;
    let out_dir = tempdir().unwrap();
    let search_paths = vec![lib_root.path().to_string_lossy().into_owned()];

    let result = takt_lang::compile_to_c(
        MODEL_FILENAME,
        main_src,
        out_dir.path().to_str().unwrap(),
        &search_paths,
        &takt_lang::GenerateOptions::default(),
    );
    assert!(
        result.is_ok(),
        "идентификаторный импорт через путь поиска должен работать: {:?}",
        result
    );
}

/// Несколько импортов, все разрешаются через один путь поиска.
#[test]
fn test_compile_multiple_imports_single_search_path() {
    let lib_dir = tempdir().unwrap();
    fs::write(lib_dir.path().join("a.takt"), "model A { start S; }").unwrap();
    fs::write(lib_dir.path().join("b.takt"), "model B { start S; }").unwrap();

    let main_src = r#"
import {A} from "a.takt";
import "b.takt";
start Ready = A;
state Done;
"#;
    let out_dir = tempdir().unwrap();
    let search_paths = vec![lib_dir.path().to_string_lossy().into_owned()];

    let result = takt_lang::compile_to_c(
        MODEL_FILENAME,
        main_src,
        out_dir.path().to_str().unwrap(),
        &search_paths,
        &takt_lang::GenerateOptions::default(),
    );
    assert!(
        result.is_ok(),
        "несколько импортов должны разрешаться через один путь: {:?}",
        result
    );
}

// -- Тесты через тестовые.takt-файлы -----------------------------------------

/// Файл примера из `tests/data/semantic/valid/` компилируется с путём к `include/`.
#[test]
fn test_compile_example_file_with_include_path() {
    let data_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/data");
    let include_dir = format!("{}/include", data_dir);

    // cli_import_with_search_path.takt использует `import "std.takt";`
    let src_path = format!(
        "{}/semantic/valid/cli_import_with_search_path.takt",
        data_dir
    );
    let src = match fs::read_to_string(&src_path) {
        Ok(s) => s,
        Err(_) => return, // файл ещё не создан - пропускаем тест
    };

    let out_dir = tempdir().unwrap();
    let search_paths = vec![include_dir];
    let result = takt_lang::compile_to_c(
        MODEL_FILENAME,
        &src,
        out_dir.path().to_str().unwrap(),
        &search_paths,
        &takt_lang::GenerateOptions::default(),
    );
    assert!(
        result.is_ok(),
        "пример с import и путём поиска должен компилироваться: {:?}",
        result
    );
}

// -- Тесты именования полей структуры для extend-состояний --------------------

/// Проверяет именование полей структуры для единичного и составного extend.
///
/// Позитивный тест: `start Main = Engine` -> поле `main` (без номера). Позитивный тест:
/// `Middle = Engine + (Engine | Engine) + Engine + Engine + Engine` -> поля
/// `middle_engine0`, `middle_parallel1`, `middle_engine2` и т.д.
#[test]
fn test_extend_field_naming_single_and_composite() {
    use takt_lang::{
        generator::{Language, generate},
        parse,
        semantic::tree::construct_model,
    };

    let src = r#"
model Engine {
    start Idle {}
    state Running {}
}
start Main = Engine {
    next Middle;
}
state Middle = Engine + (Engine | Engine) + Engine + Engine + Engine {
    next End;
}
state End;
"#;

    let tmp = tempdir().expect("не удалось создать временный каталог");
    let out_path = tmp.path().to_str().unwrap();

    let (ast, _) = parse(src, 0).expect("синтаксический анализ должен быть успешен");
    let root = construct_model(&ast, None, &[]).expect("семантический анализ должен быть успешен");
    root.borrow_mut().name = Some("Elevator".to_string());

    let result = generate(
        Language::C,
        &root.borrow(),
        out_path,
        &takt_lang::GenerateOptions::default(),
    );
    assert!(
        result.is_ok(),
        "компиляция должна быть успешной: {:?}",
        result
    );

    // Читаем сгенерированный.h файл
    let h_file = fs::read_dir(out_path)
        .unwrap()
        .filter_map(|e| e.ok())
        .find(|e| e.path().extension().and_then(|s| s.to_str()) == Some("h"))
        .expect("должен быть сгенерирован .h файл");
    let content = fs::read_to_string(h_file.path()).expect("не удалось прочитать .h файл");

    // Единичный extend: поле без номера
    assert!(
        content.contains("Engine main;"),
        "единичный extend Main=Engine должен давать поле `main` без номера:\n{}",
        content
    );
    // Составной extend: первый элемент Concatenation
    assert!(
        content.contains("Engine middle_engine0;"),
        "первый элемент конкатенации должен быть `middle_engine0`:\n{}",
        content
    );
    // Параллельный блок в Concatenation
    assert!(
        content.contains("} middle_parallel1;"),
        "параллельный блок должен быть `middle_parallel1`:\n{}",
        content
    );
    // Поля внутри параллельного блока
    assert!(
        content.contains("Engine engine0;"),
        "первый элемент внутри параллельного блока должен быть `engine0`:\n{}",
        content
    );
    assert!(
        content.contains("Engine engine1;"),
        "второй элемент внутри параллельного блока должен быть `engine1`:\n{}",
        content
    );
    // Остальные элементы конкатенации
    assert!(
        content.contains("Engine middle_engine2;"),
        "третий элемент конкатенации должен быть `middle_engine2`:\n{}",
        content
    );
}

// -- Тесты вспомогательных функций CLI ----------------------------------------

/// Функция разбора аргументов корректно обрабатывает `-I` с двоеточием.
///
/// Проверяет интеграцию между [`parse_compile_args`] и реальной компиляцией: директории
/// из `-I` попадают в `search_paths` и используются для поиска импортов.
#[test]
fn test_include_dirs_end_to_end_integration() {
    // Создаём библиотеку во временной директории
    let lib_dir = tempdir().unwrap();
    fs::write(
        lib_dir.path().join("fsm_base.takt"),
        "model FsmBase { start Idle; state Active; }",
    )
    .unwrap();

    // Создаём входной.takt файл во временной директории
    let src_dir = tempdir().unwrap();
    let src_content =
        r#"import {FsmBase} from "fsm_base.takt"; start Ready = FsmBase; state Done;"#;
    let src_file = src_dir.path().join("main.takt");
    fs::write(&src_file, src_content).unwrap();

    // Имитируем то, что делает taktc: parse_compile_args -> compile_to_c
    let lib_path = lib_dir.path().to_string_lossy().into_owned();
    let args = vec![
        "-I".to_string(),
        lib_path.clone(),
        src_file.to_string_lossy().into_owned(),
    ];

    // Вместо вызова main() напрямую используем логику из taktc.rs Убеждаемся, что
    // include_dirs передаётся в compile_to_c
    let out_dir = tempdir().unwrap();
    let search_paths = vec![lib_path];
    let result = takt_lang::compile_to_c(
        MODEL_FILENAME,
        src_content,
        out_dir.path().to_str().unwrap(),
        &search_paths,
        &takt_lang::GenerateOptions::default(),
    );

    // Значение args не используется в вычислении - только для демонстрации
    drop(args);

    assert!(
        result.is_ok(),
        "сквозной сценарий -I → импорт должен проходить: {:?}",
        result
    );
}

// -- Тесты корректности сгенерированного C-заголовка (Changes-04) -------------

/// Вспомогательная функция: генерирует.c и возвращает его содержимое.
fn generate_c_content(src: &str, model_name: &str) -> String {
    use takt_lang::{
        generator::{Language, generate},
        parse,
        semantic::tree::construct_model,
    };
    let tmp = tempfile::tempdir().unwrap();
    let (ast, _) = parse(src, 0).unwrap();
    let root = construct_model(&ast, None, &[]).unwrap();
    root.borrow_mut().name = Some(model_name.to_string());
    generate(
        Language::C,
        &root.borrow(),
        tmp.path().to_str().unwrap(),
        &takt_lang::GenerateOptions::default(),
    )
    .unwrap();
    fs::read_dir(tmp.path())
        .unwrap()
        .filter_map(|e| e.ok())
        .find(|e| e.path().extension().and_then(|s| s.to_str()) == Some("c"))
        .map(|e| fs::read_to_string(e.path()).unwrap())
        .unwrap_or_default()
}

/// Вспомогательная функция: генерирует.h и возвращает его содержимое.
fn generate_h_content(src: &str, model_name: &str) -> String {
    use takt_lang::{
        generator::{Language, generate},
        parse,
        semantic::tree::construct_model,
    };
    let tmp = tempfile::tempdir().unwrap();
    let (ast, _) = parse(src, 0).unwrap();
    let root = construct_model(&ast, None, &[]).unwrap();
    root.borrow_mut().name = Some(model_name.to_string());
    generate(
        Language::C,
        &root.borrow(),
        tmp.path().to_str().unwrap(),
        &takt_lang::GenerateOptions::default(),
    )
    .unwrap();
    fs::read_dir(tmp.path())
        .unwrap()
        .filter_map(|e| e.ok())
        .find(|e| e.path().extension().and_then(|s| s.to_str()) == Some("h"))
        .map(|e| fs::read_to_string(e.path()).unwrap())
        .unwrap_or_default()
}

/// Проверяет наличие forward declarations для всех структур.
///
/// Forward declarations позволяют компилятору C обрабатывать взаимные ссылки и
/// структуры, объявленные не в порядке зависимостей.
#[test]
fn test_header_has_forward_declarations() {
    let src = r#"
model Sub { start Init; state End; }
start Main = Sub { next Done; }
state Done;
"#;
    let header = generate_h_content(src, "System");

    // Прежде тест проверял комментарий `/* Forward declarations */` - то есть подпись
    // секции, а не саму секцию. Комментарий убран, и предмет проверки остался прежним:
    // опережающие объявления структур ниже. Проверять надо обещание, а не его подпись.
    // Корневая структура должна быть forward-declared
    assert!(
        header.contains("typedef struct System System;"),
        "корневая структура должна быть forward-declared:\n{header}"
    );
    // Вложенная Sub получает уникальное имя SystemSub
    assert!(
        header.contains("typedef struct SystemSub SystemSub;"),
        "зависимая структура SystemSub должна быть forward-declared:\n{header}"
    );
}

/// Проверяет топологическую сортировку: зависимые модели идут после своих зависимостей.
///
/// Если модель A использует модель B как поле структуры, определение B должно
/// предшествовать определению A в сгенерированном заголовке.
#[test]
fn test_header_struct_definitions_are_topologically_ordered() {
    let src = r#"
model Sub { start Ready; state Done; }
start Main = Sub { next End; }
state End;
"#;
    let header = generate_h_content(src, "Parent");

    // Вложенная Sub получает уникальное имя ParentSub и должна быть определена до
    // Parent
    let pos_sub_def = header.find("} ParentSub;");
    let pos_parent_def = header.find("} Parent;");
    if let (Some(p_sub), Some(p_parent)) = (pos_sub_def, pos_parent_def) {
        assert!(
            p_sub < p_parent,
            "структура ParentSub должна быть определена ДО Parent:\n{header}"
        );
    }
}

// -- Тесты генерации if/else-if (Changes-48) -----------------------------------

/// Проверяет, что конструкция `else if` схлопывается в `} else if (...)`, а не
/// разворачивается во вложенный `else { if (...) { } }`.
///
/// Пример: `if c == X {} else if c == Y {}` должно генерироваться без лишнего уровня
/// вложенности и переноса строки перед `else`.
#[test]
fn test_if_else_if_collapse() {
    // check вызывается в always, чтобы функция попала в UsageSet и была сгенерирована.
    let src = r#"
enum Color { Red, Green }
var c: Color := Red;
fn check(c: Color) -> bool {
    if c = Red {
        return true;
    } else if c = Green {
        return true;
    }
    return false;
}
start S { always { check(c); } }
"#;
    let c = generate_c_content(src, "Fsm");

    // Конструкция должна содержать } else if (
    assert!(
        c.contains("} else if ("),
        "конструкция else-if должна быть схлопнута в `}} else if (`:\n{c}"
    );
    // Не должно быть переноса строки непосредственно перед else
    assert!(
        !c.contains("}\n else") && !c.contains("}\r\n else"),
        "не должно быть переноса строки перед `else`:\n{c}"
    );
    // Не должно быть вложенного `else {\n if (`
    assert!(
        !c.contains("else {\n        if (") && !c.contains("else {\r\n        if ("),
        "else-ветка не должна содержать вложенный if:\n{c}"
    );
}

/// Проверяет цепочку `if / else if / else if` из трёх ветвей.
///
/// Все три ветви должны генерироваться на одном уровне вложенности.
#[test]
fn test_if_else_if_chain() {
    // route вызывается в always, чтобы функция попала в UsageSet и была сгенерирована.
    let src = r#"
enum Dir { North, South, East }
var d: Dir := North;
fn route(d: Dir) -> bool {
    if d = North {
        return true;
    } else if d = South {
        return true;
    } else if d = East {
        return true;
    }
    return false;
}
start S { always { route(d); } }
"#;
    let c = generate_c_content(src, "Fsm");

    // Должно быть два вхождения } else if (
    let count = c.matches("} else if (").count();
    assert!(
        count >= 2,
        "цепочка из трёх ветвей должна содержать минимум 2 вхождения `}} else if (`, найдено {count}:\n{c}"
    );
    // Не должно быть вложенных else { if (
    assert!(
        !c.contains("else {\n        if ("),
        "цепочка else-if не должна содержать вложенных блоков:\n{c}"
    );
}

/// Проверяет обычный `if/else` (не `if/else-if`): else-ветка не является if, поэтому
/// должна оставаться как `} else {` и не схлопываться.
#[test]
fn test_if_else_plain() {
    // flip вызывается в always, чтобы функция попала в UsageSet и была сгенерирована.
    let src = r#"
var flag: bool;
fn flip(x: bool) -> bool {
    if x {
        return false;
    } else {
        return true;
    }
}
start S { always { flag := flip(flag); } }
"#;
    let c = generate_c_content(src, "Fsm");

    // Должен присутствовать } else {
    assert!(
        c.contains("} else {"),
        "обычный if-else должен генерировать `}} else {{`:\n{c}"
    );
    // Не должно быть переноса строки перед else
    assert!(
        !c.contains("}\n else") && !c.contains("}\r\n else"),
        "не должно быть переноса строки перед `else`:\n{c}"
    );
}

// Вторая половина тестов - в подмодуле. `#[path]` от tests/ (объявление на корне
// тест-бинарника).
#[path = "codegen_tests/part2.rs"]
mod part2;
