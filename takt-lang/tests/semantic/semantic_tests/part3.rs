//! Интеграционные тесты семантики, часть 3.
//!
//! Хелперы и импорты - из родителя через `use super::*`.

use super::*;

/// `tests/data/semantic/invalid/bit_value_in_const.takt` - бит-константа с недопустимым
/// значением -> ошибка.
#[test]
fn example_bit_value_in_const_is_error() {
    let result = build_file("tests/data/semantic/invalid/bit_value_in_const.takt");
    assert!(result.is_err(), "bit = 5 должно давать ошибку");
}

/// `tests/data/semantic/invalid/no_start_state.takt` - модель без start -> ошибка.
#[test]
fn example_no_start_state_is_error() {
    let result = build_file("tests/data/semantic/invalid/no_start_state.takt");
    assert!(result.is_err(), "модель без start должна давать ошибку");
}

/// `tests/data/semantic/invalid/unknown_type_in_function.takt` - неизвестный тип
/// параметра -> ошибка.
#[test]
fn example_unknown_type_in_function_is_error() {
    let result = build_file("tests/data/semantic/invalid/unknown_type_in_function.takt");
    assert!(
        result.is_err(),
        "неизвестный тип параметра должен давать ошибку"
    );
}

// --- Тесты Се1: обнаружение циклических импортов ------------------------------
//
// Реализация Се1: семантический анализатор обнаруживает циклические зависимости между
// файлами импорта. При обнаружении цикла возвращается ошибка вида: "Циклический импорт:
// /path/a.takt -> /path/b.takt -> /path/a.takt"
//
// Поддерживаемые сценарии:
//   - прямой цикл между двумя файлами: a -> b -> a
//   - длинная цепочка: a -> b -> c -> a
//   - самоссылающийся файл: a -> a

/// Вспомогательная функция: создаёт временный `.takt`-файл в директории `dir`.
fn write_tmp_in_dir(dir: &tempfile::TempDir, name: &str, content: &str) -> String {
    let path = dir.path().join(name);
    std::fs::write(&path, content).unwrap();
    dir.path().to_string_lossy().into_owned()
}

/// Прямой цикл между двумя файлами: `a.takt` импортирует `b.takt`, `b.takt` - `a.takt`.
///
/// Ожидается ошибка "Циклический импорт" с упоминанием обоих файлов в цепочке.
#[test]
fn circular_import_two_files_is_error() {
    let dir = tempfile::tempdir().unwrap();
    let dir_str = dir.path().to_string_lossy().into_owned();

    // a.takt -> b.takt
    write_tmp_in_dir(
        &dir,
        "a.takt",
        r#"import "b.takt"; start Entry = B { } state Done;"#,
    );
    // b.takt -> a.takt (замыкает цикл)
    write_tmp_in_dir(&dir, "b.takt", r#"import "a.takt"; model B { start S; }"#);

    let src = r#"import "a.takt";"#;
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let result = construct_model(&ast, None, &[dir_str]);

    assert!(result.is_err(), "циклический импорт должен давать ошибку");
    let err = result.unwrap_err();
    assert!(
        err.message.contains("циклический") || err.message.to_lowercase().contains("цикл"),
        "сообщение должно содержать слово о цикле: {}",
        err.message
    );
    assert!(
        err.message.contains("a.takt"),
        "сообщение должно упоминать файл a.takt: {}",
        err.message
    );
    assert!(
        err.message.contains("b.takt"),
        "сообщение должно упоминать файл b.takt: {}",
        err.message
    );
}

/// Длинная цепочка циклического импорта: `a -> b -> c -> a`.
///
/// Ожидается ошибка с цепочкой из трёх файлов.
#[test]
fn circular_import_three_files_is_error() {
    let dir = tempfile::tempdir().unwrap();
    let dir_str = dir.path().to_string_lossy().into_owned();

    write_tmp_in_dir(
        &dir,
        "ca.takt",
        r#"import "cb.takt"; start Entry = Cb { } state Done;"#,
    );
    write_tmp_in_dir(
        &dir,
        "cb.takt",
        r#"import "cc.takt"; model Cb { start S; }"#,
    );
    // cc.takt -> ca.takt (замыкает цикл длиной 3)
    write_tmp_in_dir(
        &dir,
        "cc.takt",
        r#"import "ca.takt"; model Cc { start S; }"#,
    );

    let src = r#"import "ca.takt";"#;
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let result = construct_model(&ast, None, &[dir_str]);

    assert!(
        result.is_err(),
        "трёхзвенный циклический импорт должен давать ошибку"
    );
    let err = result.unwrap_err();
    assert!(
        err.message.contains("циклический") || err.message.to_lowercase().contains("цикл"),
        "сообщение должно содержать слово о цикле: {}",
        err.message
    );
}

/// Самоссылающийся файл: `self.takt` импортирует самого себя.
///
/// Это частный случай прямого цикла длиной 1.
#[test]
fn circular_import_self_reference_is_error() {
    let dir = tempfile::tempdir().unwrap();
    let dir_str = dir.path().to_string_lossy().into_owned();

    // self.takt импортирует себя же
    write_tmp_in_dir(&dir, "self_ref.takt", r#"import "self_ref.takt"; start S;"#);

    let src = r#"import "self_ref.takt";"#;
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let result = construct_model(&ast, None, &[dir_str]);

    assert!(
        result.is_err(),
        "самоссылающийся импорт должен давать ошибку"
    );
    let err = result.unwrap_err();
    assert!(
        err.message.contains("циклический") || err.message.to_lowercase().contains("цикл"),
        "сообщение должно содержать слово о цикле: {}",
        err.message
    );
    assert!(
        err.message.contains("self_ref.takt"),
        "сообщение должно упоминать файл self_ref.takt: {}",
        err.message
    );
}

/// Алмазная зависимость (diamond): `a` импортирует `b` и `c`, оба - `d`.
///
/// Это не цикл: `d` дважды импортируется по разным путям, но каждая ветвь не формирует
/// петлю. Ожидается ошибка "уже объявлена" (повторный импорт), а не ошибка цикла.
#[test]
fn diamond_import_is_not_cycle_error() {
    let dir = tempfile::tempdir().unwrap();
    let dir_str = dir.path().to_string_lossy().into_owned();

    // d.takt - общая зависимость
    write_tmp_in_dir(&dir, "d.takt", r#"model D { start S; }"#);
    // b.takt и c.takt оба импортируют d.takt
    write_tmp_in_dir(&dir, "db.takt", r#"import "d.takt"; model Db { start S; }"#);
    write_tmp_in_dir(&dir, "dc.takt", r#"import "d.takt"; model Dc { start S; }"#);
    // a.takt импортирует оба
    write_tmp_in_dir(
        &dir,
        "da.takt",
        r#"import "db.takt"; import "dc.takt"; start S;"#,
    );

    let src = r#"import "da.takt";"#;
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let result = construct_model(&ast, None, &[dir_str]);

    // Ожидаем ошибку (повторный импорт D), но не "циклический"
    if let Err(err) = &result {
        assert!(
            !err.message.contains("циклический") && !err.message.to_lowercase().contains("цикл"),
            "алмазная зависимость не является циклом: {}",
            err.message
        );
    }
    // Результат может быть как Ok, так и Err (в зависимости от реализации dedup), но не
    // должен быть ошибкой цикла.
}

/// Цикл через `import "..." as Alias` - GlobalSymbol вариант импорта.
///
/// Обнаружение цикла должно работать для всех форм импорта.
#[test]
fn circular_import_via_global_symbol_is_error() {
    let dir = tempfile::tempdir().unwrap();
    let dir_str = dir.path().to_string_lossy().into_owned();

    write_tmp_in_dir(
        &dir,
        "ga.takt",
        r#"import "gb.takt" as Gb; start Entry = Gb { } state Done;"#,
    );
    write_tmp_in_dir(
        &dir,
        "gb.takt",
        r#"import "ga.takt" as Ga; model Gb { start S; }"#,
    );

    let src = r#"import "ga.takt" as Ga;"#;
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let result = construct_model(&ast, None, &[dir_str]);

    assert!(
        result.is_err(),
        "циклический as-импорт должен давать ошибку"
    );
    let err = result.unwrap_err();
    assert!(
        err.message.contains("циклический") || err.message.to_lowercase().contains("цикл"),
        "сообщение должно содержать слово о цикле: {}",
        err.message
    );
}

/// Цикл через `import {{ A }} from "..."` - Rename вариант импорта.
///
/// Обнаружение цикла должно работать для всех форм импорта.
#[test]
fn circular_import_via_rename_is_error() {
    let dir = tempfile::tempdir().unwrap();
    let dir_str = dir.path().to_string_lossy().into_owned();

    write_tmp_in_dir(
        &dir,
        "ra.takt",
        r#"import { Rb } from "rb.takt"; start Entry = Rb { } state Done;"#,
    );
    write_tmp_in_dir(
        &dir,
        "rb.takt",
        r#"import { Ra } from "ra.takt"; model Ra { start S; } model Rb { start S; }"#,
    );

    // Инициируем цикл через Plain-импорт (ra.takt содержит rename-импорт rb.takt,
    // который замкнёт цикл)
    let src = r#"import "ra.takt";"#;
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let result = construct_model(&ast, None, &[dir_str]);

    assert!(
        result.is_err(),
        "циклический rename-импорт должен давать ошибку"
    );
    let err = result.unwrap_err();
    assert!(
        err.message.contains("циклический") || err.message.to_lowercase().contains("цикл"),
        "сообщение должно содержать слово о цикле: {}",
        err.message
    );
}

/// Нециклический линейный импорт `a -> b -> c` (без петли) - должен успешно строиться.
///
/// Проверяет, что детектор не ложно срабатывает на корректные цепочки.
#[test]
fn linear_import_chain_is_valid() {
    let dir = tempfile::tempdir().unwrap();
    let dir_str = dir.path().to_string_lossy().into_owned();

    write_tmp_in_dir(&dir, "lc.takt", r#"model Lc { start S; }"#);
    write_tmp_in_dir(
        &dir,
        "lb.takt",
        r#"import "lc.takt"; model Lb { start S; }"#,
    );
    write_tmp_in_dir(
        &dir,
        "la.takt",
        r#"import "lb.takt"; model La { start S; }"#,
    );

    let src = r#"import "la.takt";"#;
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let result = construct_model(&ast, None, &[dir_str]);

    assert!(
        result.is_ok(),
        "линейная цепочка импортов должна успешно строиться: {:?}",
        result.err()
    );
}

// --- Тесты Се12: документационные комментарии в семантическом дереве ----------
//
// Реализация Се12: `///`-комментарии включаются в семантическое дерево через
// `construct_model_with_docs`. Доступ:
//   - `model_node.own_doc()` -> документация самой модели
//   - `model_node.element_doc("Имя")` -> документация именованного элемента
//   - `model_node.docs["Имя"]` -> то же через HashMap

/// `construct_model_with_docs` - состояние получает свой doc-комментарий.
#[test]
fn doc_comment_for_state() {
    let src = "/// Начальное состояние.\nstart S;";
    let (ast, comments) = parse(src, 0).expect("ошибка разбора");
    let root =
        construct_model_with_docs(&ast, None, &[], &comments).expect("ошибка построения семантики");
    assert_eq!(
        root.borrow().element_doc("S"),
        ["Начальное состояние."],
        "doc-комментарий должен быть привязан к состоянию S"
    );
}

/// Переменная получает свой doc-комментарий.
#[test]
fn doc_comment_for_variable() {
    let src = "/// Счётчик.\nvar counter: bit := false;";
    let (ast, comments) = parse(src, 0).expect("ошибка разбора");
    let root =
        construct_model_with_docs(&ast, None, &[], &comments).expect("ошибка построения семантики");
    assert_eq!(
        root.borrow().element_doc("counter"),
        ["Счётчик."],
        "doc-комментарий должен быть привязан к переменной counter"
    );
}

/// Тип (`type`) получает свой doc-комментарий.
#[test]
fn doc_comment_for_type() {
    let src = "/// Байт.\ntype Byte = [bit;8];\n";
    let (ast, comments) = parse(src, 0).expect("ошибка разбора");
    let root =
        construct_model_with_docs(&ast, None, &[], &comments).expect("ошибка построения семантики");
    assert_eq!(
        root.borrow().element_doc("Byte"),
        ["Байт."],
        "doc-комментарий должен быть привязан к типу Byte"
    );
}

/// Условие (`cond`) получает свой doc-комментарий.
#[test]
fn doc_comment_for_condition() {
    let src = "/// Истинно всегда.\ncond AlwaysTrue = true;";
    let (ast, comments) = parse(src, 0).expect("ошибка разбора");
    let root =
        construct_model_with_docs(&ast, None, &[], &comments).expect("ошибка построения семантики");
    assert_eq!(
        root.borrow().element_doc("AlwaysTrue"),
        ["Истинно всегда."],
        "doc-комментарий должен быть привязан к условию AlwaysTrue"
    );
}

/// Вложенная модель получает doc-комментарий через `root.element_doc("M")`.
#[test]
fn doc_comment_for_nested_model_from_parent() {
    let src = "/// Вложенная модель.\nmodel M { start S; }";
    let (ast, comments) = parse(src, 0).expect("ошибка разбора");
    let root =
        construct_model_with_docs(&ast, None, &[], &comments).expect("ошибка построения семантики");
    assert_eq!(
        root.borrow().element_doc("M"),
        ["Вложенная модель."],
        "doc-комментарий должен быть доступен через родительскую модель"
    );
}

/// Собственный `doc`-поле вложенной модели содержит тот же текст.
#[test]
fn doc_comment_for_nested_model_own_doc() {
    let src = "/// Своя документация.\nmodel Inner { start S; }";
    let (ast, comments) = parse(src, 0).expect("ошибка разбора");
    let root =
        construct_model_with_docs(&ast, None, &[], &comments).expect("ошибка построения семантики");
    let inner = root.borrow().search_model("Inner").unwrap();
    assert_eq!(
        inner.borrow().own_doc(),
        ["Своя документация."],
        "дочерний узел должен хранить свою документацию в поле doc"
    );
}

/// Многострочный doc-блок из нескольких `///` привязывается как список строк.
#[test]
fn multi_line_doc_comment_for_state() {
    let src = "/// Строка 1.\n/// Строка 2.\n/// Строка 3.\nstart S;";
    let (ast, comments) = parse(src, 0).expect("ошибка разбора");
    let root =
        construct_model_with_docs(&ast, None, &[], &comments).expect("ошибка построения семантики");
    let doc = root.borrow().element_doc("S").to_vec();
    assert_eq!(doc.len(), 3, "должно быть три строки документации");
    assert_eq!(doc[0], "Строка 1.");
    assert_eq!(doc[1], "Строка 2.");
    assert_eq!(doc[2], "Строка 3.");
}

/// Обычный `//`-комментарий не попадает в документацию.
#[test]
fn regular_comment_not_in_docs() {
    let src = "// Обычный комментарий.\nstart S;";
    let (ast, comments) = parse(src, 0).expect("ошибка разбора");
    let root =
        construct_model_with_docs(&ast, None, &[], &comments).expect("ошибка построения семантики");
    assert!(
        root.borrow().element_doc("S").is_empty(),
        "обычный // комментарий не должен попасть в документацию"
    );
}

/// Без комментариев - `element_doc` возвращает пустой срез.
#[test]
fn no_doc_comment_returns_empty() {
    let src = "start S; state Done;";
    let (ast, comments) = parse(src, 0).expect("ошибка разбора");
    let root =
        construct_model_with_docs(&ast, None, &[], &comments).expect("ошибка построения семантики");
    assert!(root.borrow().element_doc("S").is_empty());
    assert!(root.borrow().element_doc("Done").is_empty());
    assert!(root.borrow().own_doc().is_empty());
}

/// Каждый элемент получает свой doc-комментарий - не чужой.
#[test]
fn each_element_gets_its_own_doc() {
    let src = concat!(
        "/// Состояние A.\n",
        "start A;\n",
        "/// Состояние B.\n",
        "state B;\n",
    );
    let (ast, comments) = parse(src, 0).expect("ошибка разбора");
    let root =
        construct_model_with_docs(&ast, None, &[], &comments).expect("ошибка построения семантики");
    assert_eq!(root.borrow().element_doc("A"), ["Состояние A."]);
    assert_eq!(root.borrow().element_doc("B"), ["Состояние B."]);
}

/// `construct_model` (без docs) -> поля doc и docs остаются пустыми.
#[test]
fn construct_model_without_docs_leaves_fields_empty() {
    let src = "/// Документация.\nstart S;";
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let root = construct_model(&ast, None, &[]).expect("ошибка построения семантики");
    assert!(
        root.borrow().element_doc("S").is_empty(),
        "construct_model без docs должен оставить поле docs пустым"
    );
    assert!(root.borrow().own_doc().is_empty());
}

/// Документация вложенного состояния в модели (`model M { /// doc\n state S... }`).
#[test]
fn doc_comment_for_state_inside_model() {
    let src = concat!(
        "model M {\n",
        "    /// Документация состояния внутри модели.\n",
        "    start S;\n",
        "    state Done;\n",
        "}\n",
    );
    let (ast, comments) = parse(src, 0).expect("ошибка разбора");
    let root =
        construct_model_with_docs(&ast, None, &[], &comments).expect("ошибка построения семантики");
    let m = root.borrow().search_model("M").unwrap();
    assert_eq!(
        m.borrow().element_doc("S"),
        ["Документация состояния внутри модели."],
    );
    assert!(
        m.borrow().element_doc("Done").is_empty(),
        "Done не имеет doc-комментария"
    );
}

/// `tests/data/semantic/valid/doc_comments.takt` - файл с doc-комментариями строится
/// корректно.
#[test]
fn example_doc_comments_file_is_valid() {
    let src = std::fs::read_to_string("tests/data/semantic/valid/doc_comments.takt")
        .expect("не могу прочитать doc_comments.takt");
    let (ast, comments) = parse(&src, 0).expect("ошибка разбора");
    let root =
        construct_model_with_docs(&ast, None, &[], &comments).expect("ошибка построения семантики");

    // Проверяем документацию на верхнем уровне
    let rb = root.borrow();
    assert!(
        !rb.element_doc("Byte").is_empty(),
        "тип Byte должен иметь doc"
    );
    assert!(
        !rb.element_doc("counter").is_empty(),
        "переменная counter должна иметь doc"
    );
    assert!(
        !rb.element_doc("MaxReached").is_empty(),
        "условие MaxReached должно иметь doc"
    );
    assert!(
        !rb.element_doc("TrafficLight").is_empty(),
        "модель TrafficLight должна иметь doc"
    );

    // Проверяем документацию состояний внутри TrafficLight
    let tl = rb
        .search_model("TrafficLight")
        .expect("TrafficLight не найдена");
    let tl = tl.borrow();
    assert!(
        !tl.own_doc().is_empty(),
        "TrafficLight должна иметь собственную документацию"
    );
    assert!(
        !tl.element_doc("timer").is_empty(),
        "переменная timer должна иметь doc"
    );
    assert!(
        !tl.element_doc("Red").is_empty(),
        "состояние Red должно иметь doc"
    );
    assert!(
        !tl.element_doc("Green").is_empty(),
        "состояние Green должно иметь doc"
    );
    assert!(
        !tl.element_doc("Yellow").is_empty(),
        "состояние Yellow должно иметь doc"
    );
}

/// Документация модели содержит несколько строк из многострочного `///`-блока.
#[test]
fn multi_line_doc_for_model() {
    let src = concat!(
        "/// Первая строка.\n",
        "/// Вторая строка.\n",
        "model M { start S; }\n",
    );
    let (ast, comments) = parse(src, 0).expect("ошибка разбора");
    let root =
        construct_model_with_docs(&ast, None, &[], &comments).expect("ошибка построения семантики");
    let doc = root.borrow().element_doc("M").to_vec();
    assert_eq!(doc.len(), 2, "должно быть две строки документации для M");
    assert_eq!(doc[0], "Первая строка.");
    assert_eq!(doc[1], "Вторая строка.");
    let m = root.borrow().search_model("M").unwrap();
    assert_eq!(
        m.borrow().own_doc().len(),
        2,
        "M.doc тоже должен содержать обе строки"
    );
}

// --- Се11: строгая проверка булевости условий переходов ----------------------

/// Явное сравнение в условии перехода - нет предупреждений.
///
/// # Takt
/// ```but
/// var timer: [bit;8] = 0;
/// start S { ref T: timer != 0; }
/// state T;
/// ```
#[test]
fn se11_explicit_comparison_no_warnings() {
    let src = "var timer: [bit;8] := 0; start S { ref T: timer != 0; } state T;";
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let root = construct_model(&ast, None, &[]).expect("ошибка построения");
    let warnings = implicit_bool_warnings(&root);
    assert!(
        warnings.is_empty(),
        "явное сравнение не должно давать предупреждений"
    );
}

/// Числовая переменная без сравнения - предупреждение Се11.
///
/// # Takt
/// ```but
/// var timer: [bit;8] = 0;
/// start S { ref T: timer; }   // <- Предупреждение
/// state T;
/// ```
#[test]
fn se11_numeric_var_in_ref_gives_warning() {
    let src = "var timer: [bit;8] := 0; start S { ref T: timer; } state T;";
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let root = construct_model(&ast, None, &[]).expect("ошибка построения");
    let warnings = implicit_bool_warnings(&root);
    assert_eq!(
        warnings.len(),
        1,
        "числовая переменная в условии должна давать предупреждение"
    );
    assert!(
        warnings[0].message.contains("timer"),
        "предупреждение должно упоминать имя переменной"
    );
}

/// Числовой литерал в условии перехода - предупреждение Се11.
#[test]
fn se11_number_literal_in_ref_gives_warning() {
    let src = "start S { ref T: 5; } state T;";
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let root = construct_model(&ast, None, &[]).expect("ошибка построения");
    let warnings = implicit_bool_warnings(&root);
    assert_eq!(
        warnings.len(),
        1,
        "числовой литерал в условии должен давать предупреждение"
    );
}

/// Переменная типа `bool` в условии - нет предупреждений.
#[test]
fn se11_bool_var_in_ref_no_warnings() {
    let src = "var flag: bool := false; start S { ref T: flag; } state T;";
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let root = construct_model(&ast, None, &[]).expect("ошибка построения");
    let warnings = implicit_bool_warnings(&root);
    assert!(
        warnings.is_empty(),
        "переменная bool не должна давать предупреждений"
    );
}

/// Переменная типа `bit` (1 бит) в условии - нет предупреждений.
#[test]
fn se11_bit_var_in_ref_no_warnings() {
    let src = "var flag: bit := 0; start S { ref T: flag; } state T;";
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let root = construct_model(&ast, None, &[]).expect("ошибка построения");
    let warnings = implicit_bool_warnings(&root);
    assert!(
        warnings.is_empty(),
        "переменная bit не должна давать предупреждений"
    );
}

/// Булев литерал в условии - нет предупреждений.
#[test]
fn se11_bool_literal_in_ref_no_warnings() {
    let src = "start S { ref T: true; } state T;";
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let root = construct_model(&ast, None, &[]).expect("ошибка построения");
    let warnings = implicit_bool_warnings(&root);
    assert!(
        warnings.is_empty(),
        "булев литерал не должен давать предупреждений"
    );
}

/// Безусловный переход (без условия) - нет предупреждений.
#[test]
fn se11_unconditional_ref_no_warnings() {
    let src = "start S { ref T; } state T;";
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let root = construct_model(&ast, None, &[]).expect("ошибка построения");
    let warnings = implicit_bool_warnings(&root);
    assert!(
        warnings.is_empty(),
        "безусловный переход не должен давать предупреждений"
    );
}

/// Несколько переходов: один числовой, один явный - одно предупреждение.
#[test]
fn se11_one_numeric_one_explicit_ref() {
    let src = concat!(
        "var timer: [bit;8] := 0;\n",
        "var flag: bool := false;\n",
        "start S { ref T: timer; ref U: flag; }\n",
        "state T;\n",
        "state U;\n",
    );
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let root = construct_model(&ast, None, &[]).expect("ошибка построения");
    let warnings = implicit_bool_warnings(&root);
    assert_eq!(warnings.len(), 1, "должно быть ровно одно предупреждение");
}

/// Вложенная модель с числовым условием - предупреждение включает имя модели.
#[test]
fn se11_nested_model_numeric_ref_warning() {
    let src = concat!(
        "model M {\n",
        "    var timer: [bit;8] := 0;\n",
        "    start S { ref T: timer; }\n",
        "    state T;\n",
        "}\n",
    );
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let root = construct_model(&ast, None, &[]).expect("ошибка построения");
    let warnings = implicit_bool_warnings(&root);
    assert_eq!(warnings.len(), 1, "предупреждение из вложенной модели");
    assert!(
        warnings[0].message.contains("M"),
        "предупреждение должно упоминать имя вложенной модели"
    );
}

/// Файл `implicit_bool_warn.takt` из тестовых данных - без предупреждений.
///
/// Все переходы в файле используют явные сравнения или булевы переменные.
#[test]
fn se11_valid_file_no_warnings() {
    let src = std::fs::read_to_string("tests/data/semantic/valid/implicit_bool_warn.takt")
        .expect("не удалось прочитать файл");
    let (ast, _) = parse(&src, 0).expect("ошибка разбора");
    let root = construct_model(&ast, None, &[]).expect("ошибка построения");
    let warnings = implicit_bool_warnings(&root);
    assert!(
        warnings.is_empty(),
        "файл с явными сравнениями не должен давать предупреждений: {:?}",
        warnings
    );
}

/// Файл `implicit_bool_numeric.takt` - одно предупреждение о числовом условии.
#[test]
fn se11_numeric_file_gives_one_warning() {
    let src = std::fs::read_to_string("tests/data/semantic/valid/implicit_bool_numeric.takt")
        .expect("не удалось прочитать файл");
    let (ast, _) = parse(&src, 0).expect("ошибка разбора");
    let root = construct_model(&ast, None, &[]).expect("ошибка построения");
    let warnings = implicit_bool_warnings(&root);
    assert_eq!(
        warnings.len(),
        1,
        "файл с числовым условием должен давать ровно одно предупреждение"
    );
    assert!(
        warnings[0].message.contains("timer"),
        "предупреждение должно упоминать переменную timer"
    );
}

/// Условие сравнения `<` - нет предупреждений Се11.
#[test]
fn se11_less_comparison_no_warnings() {
    let src = "var timer: [bit;8] := 0; start S { ref T: timer < 100; } state T;";
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let root = construct_model(&ast, None, &[]).expect("ошибка построения");
    let warnings = implicit_bool_warnings(&root);
    assert!(
        warnings.is_empty(),
        "условие '<' не должно давать предупреждений"
    );
}

/// Условия `>`, `<=`, `>=` - нет предупреждений Се11.
#[test]
fn se11_other_comparisons_no_warnings() {
    let src = concat!(
        "var a: [bit;8] := 0;\n",
        "var b: [bit;8] := 0;\n",
        "start S { ref T: a > 0; ref U: a <= b; ref V: a >= b; }\n",
        "state T; state U; state V;\n",
    );
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let root = construct_model(&ast, None, &[]).expect("ошибка построения");
    let warnings = implicit_bool_warnings(&root);
    assert!(
        warnings.is_empty(),
        "условия >, <=, >= не должны давать предупреждений"
    );
}

/// Логическое не в условии - нет предупреждений Се11.
#[test]
fn se11_not_condition_no_warnings() {
    let src = "var flag: bool := false; start S { ref T: !flag; } state T;";
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let root = construct_model(&ast, None, &[]).expect("ошибка построения");
    let warnings = implicit_bool_warnings(&root);
    assert!(
        warnings.is_empty(),
        "условие '!' не должно давать предупреждений"
    );
}

/// Именованное условие в ref - нет предупреждений Се11.
#[test]
fn se11_named_cond_in_ref_no_warnings() {
    let src = concat!(
        "var counter: [bit;8] := 0;\n",
        "cond Full = counter = 255;\n",
        "start S { ref T: Full; }\n",
        "state T;\n",
    );
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let root = construct_model(&ast, None, &[]).expect("ошибка построения");
    let warnings = implicit_bool_warnings(&root);
    assert!(
        warnings.is_empty(),
        "именованное условие не должно давать предупреждений"
    );
}

/// Арифметическое выражение в условии - предупреждение Се11.
#[test]
fn se11_arithmetic_in_ref_gives_warning() {
    let src = "var timer: [bit;8] := 0; start S { ref T: timer + 1; } state T;";
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let root = construct_model(&ast, None, &[]).expect("ошибка построения");
    let warnings = implicit_bool_warnings(&root);
    assert_eq!(
        warnings.len(),
        1,
        "арифметическое выражение в условии должно давать предупреждение"
    );
    assert!(
        warnings[0].message.contains("сложение"),
        "предупреждение должно упоминать тип выражения: {}",
        warnings[0].message
    );
}

/// Файл с арифметическим условием - одно предупреждение.
#[test]
fn se11_arithmetic_file_gives_one_warning() {
    let src = std::fs::read_to_string("tests/data/semantic/valid/implicit_bool_arithmetic.takt")
        .expect("не удалось прочитать файл");
    let (ast, _) = parse(&src, 0).expect("ошибка разбора");
    let root = construct_model(&ast, None, &[]).expect("ошибка построения");
    let warnings = implicit_bool_warnings(&root);
    assert_eq!(
        warnings.len(),
        1,
        "файл с арифметическим условием должен давать ровно одно предупреждение"
    );
}

/// Файл с именованными условиями - нет предупреждений Се11.
#[test]
fn se11_named_cond_file_no_warnings() {
    let src = std::fs::read_to_string("tests/data/semantic/valid/implicit_bool_named_cond.takt")
        .expect("не удалось прочитать файл");
    let (ast, _) = parse(&src, 0).expect("ошибка разбора");
    let root = construct_model(&ast, None, &[]).expect("ошибка построения");
    let warnings = implicit_bool_warnings(&root);
    assert!(
        warnings.is_empty(),
        "файл с именованными условиями не должен давать предупреждений: {:?}",
        warnings
    );
}

/// Предупреждение Се11 содержит имя исходного состояния.
#[test]
fn se11_warning_contains_source_state_name() {
    let src = "var x: [bit;8] := 0; start SourceState { ref T: x; } state T;";
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let root = construct_model(&ast, None, &[]).expect("ошибка построения");
    let warnings = implicit_bool_warnings(&root);
    assert_eq!(warnings.len(), 1);
    assert!(
        warnings[0].message.contains("SourceState"),
        "предупреждение должно упоминать состояние-источник: {}",
        warnings[0].message
    );
}

// --- Тесты разыменования condition в ref-переходах (этап 6 конвейера) -------

/// Условие ref с bit-переменной разрешается в `Condition::Variable`, не в
/// `Condition::Unresolved`.
///
/// # Пример (Takt)
/// ```but
/// var flag: bit = false;
/// start A { ref B: flag; }
/// state B;
/// ```
#[test]
fn ref_cond_bit_var_is_resolved() {
    use takt_lang::semantic::ConditionNode;
    let src = "var flag: bit := false; start A { ref B: flag; } state B;";
    let node = build(src);
    let state_a = &node.states["A"];
    if let StateNode::Simple { references, .. } = state_a {
        assert_eq!(references.len(), 1);
        assert!(
            matches!(references[0].cond, ConditionNode::Variable(_, _)),
            "условие должно быть разрешено в Variable, получено: {:?}",
            references[0].cond
        );
    } else {
        panic!("ожидался StateNode::Simple для A");
    }
}

/// Условие ref с bool-переменной разрешается в `Condition::Variable`.
///
/// # Пример (Takt)
/// ```but
/// var done: bool = false;
/// start A { ref B: done; }
/// state B;
/// ```
#[test]
fn ref_cond_bool_var_is_resolved() {
    use takt_lang::semantic::ConditionNode;
    let src = "var done: bool := false; start A { ref B: done; } state B;";
    let node = build(src);
    let state_a = &node.states["A"];
    if let StateNode::Simple { references, .. } = state_a {
        assert!(
            matches!(references[0].cond, ConditionNode::Variable(_, _)),
            "условие должно быть разрешено в Variable"
        );
    } else {
        panic!("ожидался StateNode::Simple для A");
    }
}

/// Именованное условие (`cond`) в ref разрешается до его значения (не `Unresolved`).
///
/// # Пример (Takt)
/// ```but
/// var x: [bit;8] = 0;
/// cond Full = x = 255;
/// start A { ref B: Full; }
/// state B;
/// ```
#[test]
fn ref_cond_named_cond_is_resolved() {
    use takt_lang::semantic::ConditionNode;
    let src = "var x: [bit;8] := 0; cond Full = x = 255; start A { ref B: Full; } state B;";
    let node = build(src);
    let state_a = &node.states["A"];
    if let StateNode::Simple { references, .. } = state_a {
        assert_eq!(references.len(), 1);
        // Именованное условие раскрывается до значения (Equal или аналог)
        assert!(
            !matches!(references[0].cond, ConditionNode::Unresolved(_)),
            "условие не должно оставаться Unresolved после этапа 6"
        );
    } else {
        panic!("ожидался StateNode::Simple для A");
    }
}
