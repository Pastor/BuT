//! Позиции в диагностиках: файл, строка, колонка.
//!
//! Тесты - на то, **что увидит пользователь**: путь файла в диагностике и координаты.

use takt_lang::diagnostics::{FileTable, Location, line_column};

const DIR: &str = "tests/data/diag53";

/// Компилирует фикстуру и возвращает диагностику (фикстуры заведомо ошибочны).
fn error_of(fixture: &str) -> takt_lang::diagnostics::Diagnostic {
    let path = format!("{DIR}/{fixture}");
    let source = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{path}: {e}"));
    let out = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join("takt_diag53_out.c");
    // Каталог процесса создаётся здесь: файл пишет не тест, а инструмент.
    let _ = std::fs::create_dir_all(out.parent().expect("каталог процесса"));
    takt_lang::compile_to_c(
        &path,
        &source,
        out.to_str().expect("путь"),
        &[DIR.to_string()],
        &takt_lang::GenerateOptions::default(),
    )
    .expect_err("фикстура обязана быть ошибочной")
}

// --- Путь файла в диагностике (A1-A3) ----------------------------------------

/// Ошибка своего файла названа своим файлом.
#[test]
fn error_in_own_file_names_own_file() {
    let d = error_of("lib_bad.takt");
    assert_eq!(d.file.as_deref(), Some("tests/data/diag53/lib_bad.takt"));
}

/// Ошибка внутри импортированного файла названа именем библиотеки, а не импортёра.
/// Это и есть суть фичи.
#[test]
fn error_inside_import_names_the_library() {
    let d = error_of("importer.takt");
    assert_eq!(
        d.file.as_deref(),
        Some("tests/data/diag53/lib_bad.takt"),
        "виновник — библиотека; импортёр её не писал и чинить не вправе"
    );
}

/// Вложенный импорт (top -> mid -> deep) называет самый внутренний файл.
///
/// Тест правила "первый проставивший выигрывает": затирание пути на каждом уровне
/// всплытия дало бы имя импортёра вместо имени виновника.
#[test]
fn nested_import_names_the_deepest_file() {
    let d = error_of("top.takt");
    assert_eq!(d.file.as_deref(), Some("tests/data/diag53/deep_bad.takt"));
}

/// Координаты указывают на место ошибки, а не на начало файла.
#[test]
fn position_points_at_the_offending_reference() {
    let d = error_of("lib_bad.takt");
    let Location::Source(_, start, _) = d.loc else {
        panic!("ожидалась файловая позиция, получено {:?}", d.loc);
    };
    let text = std::fs::read_to_string("tests/data/diag53/lib_bad.takt").expect("чтение");
    let (line, column) = line_column(&text, start as usize);
    assert_eq!(line, 4, "ссылка 'Nowhere' — на 4-й строке");
    assert!(
        column > 1,
        "колонка указывает внутрь строки, а не на её начало"
    );
}

/// Настоящий `file_no`: файлы получают разные номера.
#[test]
fn imported_file_gets_its_own_file_no() {
    let d = error_of("importer.takt");
    let Location::Source(file_no, _, _) = d.loc else {
        panic!("ожидалась файловая позиция");
    };
    assert_ne!(file_no, 0, "0 — корневой файл; ошибка пришла из импорта");
}

// --- Реестр файлов -----------------------------------------------------------

#[test]
fn file_table_registers_root_as_zero() {
    let files = FileTable::new("main.takt");
    assert_eq!(files.path(0), Some("main.takt"));
}

/// Один путь - один номер: номер обозначает файл, а не факт загрузки.
#[test]
fn file_table_deduplicates_paths() {
    let mut files = FileTable::new("main.takt");
    let first = files.add("lib.takt");
    let second = files.add("lib.takt");
    assert_eq!(first, second);
    assert_ne!(first, 0);
}

#[test]
fn file_table_returns_none_for_unknown_and_non_source() {
    let files = FileTable::new("main.takt");
    assert_eq!(files.path(42), None);
    assert_eq!(files.path_of(&Location::Codegen), None);
    assert_eq!(files.path_of(&Location::Implicit), None);
}

/// Импорт **никогда** не получает номер корня - при любом способе создания реестра.
///
/// Позиция из импортированного файла становилась неотличима от корневой: дефект,
/// который как раз закрывала.
///
/// []: ../../docs/fixes/-file-table-default-collision.md
#[test]
fn file_table_default_never_gives_import_the_root_number() {
    let mut files = FileTable::default();
    assert_ne!(
        files.add("lib.takt"),
        0,
        "номер 0 означает «корень» и импорту достаться не может"
    );
}

/// Слот `0` реестра без корня занят, но путь честно неизвестен.
///
/// Альтернатива ("корень = первый добавленный") и была дефектом: реестр выдавал бы
/// чужой файл за корневой.
#[test]
fn file_table_default_reports_unknown_root() {
    let files = FileTable::default();
    assert_eq!(
        files.path(0),
        None,
        "корень неизвестен — врать о нём нельзя"
    );
}

/// У реестра с корнем нумерация не изменилась.
#[test]
fn file_table_new_keeps_root_at_zero_and_import_at_one() {
    let mut files = FileTable::new("main.takt");
    assert_eq!(files.path(0), Some("main.takt"));
    assert_eq!(files.add("lib.takt"), 1);
}

// --- Строка и колонка --------------------------------------------------------

/// Нумерация с единицы - как в rustc/gcc (внутри Location смещения с нуля).
#[test]
fn line_column_counts_from_one() {
    assert_eq!(line_column("abc", 0), (1, 1));
    assert_eq!(line_column("abc\ndef", 4), (2, 1));
    assert_eq!(line_column("abc\ndef", 6), (2, 3));
}

/// Колонка - в символах, а не в байтах: в `.takt` есть кириллица (комментарии, строки),
/// и байтовая колонка указывала бы мимо.
#[test]
fn line_column_counts_characters_not_bytes() {
    let text = "// абв\nstart S;";
    let offset = text.find("start").expect("есть");
    assert_eq!(line_column(text, offset), (2, 1));
    // Внутри кириллицы: 'в' - третий символ после "// ".
    let inside = text.find('в').expect("есть");
    assert_eq!(line_column(text, inside), (1, 6));
}

/// Смещение за концом текста не паникует.
#[test]
fn line_column_clamps_offset_past_end() {
    assert_eq!(line_column("ab", 99), (1, 3));
}
