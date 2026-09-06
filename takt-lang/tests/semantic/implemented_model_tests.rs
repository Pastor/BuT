//! Модель без состояний в реализации - `SE-106`.
//!
//! ## Что здесь ловится
//!
//! До фичи такую программу не судил никто, и решение о ней принимал каждый потребитель
//! сам.
//!
//! Худший ответ - не бессодержательный `CC-005`, а **молчаливый успех**. Поэтому тест
//! перечисляет **все** входы генерации и падает списком: цель, пропустившая отказ, не
//! должна пройти незамеченной.

use takt_lang::diagnostics::{Location, line_column};
use takt_lang::{GenerateOptions, collect_compile_diagnostics};

/// Каталог исправлениетур (он же путь поиска импорта).
const DIR: &str = "tests/data/implemented0211";

fn fixture(name: &str) -> (String, String) {
    let path = format!("{DIR}/{name}");
    let source =
        std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("не прочитать {path}: {e}"));
    (path, source)
}

/// Диагностики исправлениетуры: `(код, сообщение, строка, колонка)`.
///
/// Координата - та, что видит пользователь: строка и колонка **в символах** (в `.takt`
/// есть кириллица).
fn diagnostics(name: &str) -> Vec<(String, String, usize, usize)> {
    let (path, source) = fixture(name);
    collect_compile_diagnostics(&path, &source, &[DIR.to_string()], false)
        .iter()
        .map(|d| {
            let (line, column) = match d.loc {
                Location::Source(_, start, _) => line_column(&source, start as usize),
                _ => (0, 0),
            };
            (
                d.code.clone().unwrap_or_default(),
                d.message.clone(),
                line,
                column,
            )
        })
        .collect()
}

/// Возвращает `SE-106` исправлениетуры (ожидая ровно одну).
fn single_se106(name: &str) -> (String, usize, usize) {
    let found = diagnostics(name);
    let mut se106 = found.iter().filter(|(code, ..)| code == "SE-106");
    let first = se106
        .next()
        .unwrap_or_else(|| panic!("{name}: ожидалась SE-106, получено: {found:#?}"));
    assert!(
        se106.next().is_none(),
        "{name}: ожидалась ровно одна SE-106, получено: {found:#?}"
    );
    (first.1.clone(), first.2, first.3)
}

// -- R1/R2: все формы реализации отвечают одинаково ---------------------------

/// Формы реализации перечислены **списком**: исправлениетура, имя пустой модели и позиция её
/// **использования** (координаты сняты зондом `taktc`, а не угаданы - правило проекта о
/// новых тестах).
const FORMS: [(&str, &str, usize, usize); 5] = [
    ("direct.takt", "Empty", 6, 13),
    ("parallel.takt", "Empty", 16, 13),
    ("sequence.takt", "Empty", 16, 20),
    ("model_form.takt", "Empty", 6, 13),
    ("nested.takt", "Inner", 9, 15),
];

#[test]
fn every_implementation_form_is_rejected() {
    let mut silent = Vec::new();
    for (fixture, model, _, _) in FORMS {
        let found = diagnostics(fixture);
        if !found.iter().any(|(code, ..)| code == "SE-106") {
            silent.push(format!("{fixture} (модель '{model}'): {found:#?}"));
        }
    }
    assert!(
        silent.is_empty(),
        "формы реализации, оставшиеся без SE-106:\n{}",
        silent.join("\n")
    );
}

#[test]
fn diagnostic_points_at_the_use_site() {
    for (fixture, _, line, column) in FORMS {
        let (_, got_line, got_column) = single_se106(fixture);
        assert_eq!(
            (got_line, got_column),
            (line, column),
            "{fixture}: SE-106 обязана указывать на МЕСТО ИСПОЛЬЗОВАНИЯ модели \
             (у CC-005 позиции не было вовсе — Location::Codegen)"
        );
    }
}

#[test]
fn diagnostic_names_the_model_and_the_way_out() {
    let (message, _, _) = single_se106("direct.takt");
    assert!(
        message.contains("'Empty'"),
        "SE-106 обязана назвать модель, получено: {message:?}"
    );
    assert!(
        message.contains("start"),
        "SE-106 обязана назвать способ починки, получено: {message:?}"
    );
}

// -- R3: накопление по использованию ---------------------------

#[test]
fn each_use_is_reported_separately() {
    let found = diagnostics("two_empty.takt");
    let se106: Vec<_> = found.iter().filter(|(code, ..)| code == "SE-106").collect();
    assert_eq!(
        se106.len(),
        2,
        "две пустые модели в одной композиции — две диагностики, а не первая \
         попавшаяся; получено: {found:#?}"
    );
    assert!(
        se106.iter().any(|(_, m, ..)| m.contains("'E1'"))
            && se106.iter().any(|(_, m, ..)| m.contains("'E2'")),
        "обе модели обязаны быть названы, получено: {se106:#?}"
    );
}

// -- R4/R5: законное не задето, соседи не перехвачены -------------------------

#[test]
fn declaration_container_without_states_stays_legal() {
    let found = diagnostics("container.takt");
    assert!(
        !found.iter().any(|(code, ..)| code == "SE-106"),
        "модель без состояний, НЕ участвующая в реализации, — законный контейнер \
         объявлений; получено: {found:#?}"
    );
}

#[test]
fn model_with_states_but_no_start_is_still_se011() {
    let found = diagnostics("no_start.takt");
    assert!(
        found.iter().any(|(code, ..)| code == "SE-011"),
        "состояния есть, `start` нет — это забытая пометка (SE-011), а не пустая \
         модель; получено: {found:#?}"
    );
    assert!(
        !found.iter().any(|(code, ..)| code == "SE-106"),
        "SE-106 не имеет права перехватывать случай SE-011; получено: {found:#?}"
    );
}

#[test]
fn library_file_as_entry_is_still_se102() {
    // Позиция "вход исполнения" - свойство вызова, а не модели (`validate/entry.rs`),
    // поэтому спрашиваем цель, а не `validate`: `collect_compile_diagnostics`
    // библиотечный файл законно пропускает.
    let (path, source) = fixture("library.takt");
    let search: [String; 0] = [];
    let error = takt_lang::compile_to_c(
        &path,
        &source,
        &out_dir("library"),
        &search,
        &GenerateOptions::default(),
    )
    .expect_err("библиотека входом быть не может");
    assert_eq!(
        error.code.as_deref(),
        Some("SE-102"),
        "файл без состояний вовсе — это библиотека (SE-102), а не пустая \
         реализация (SE-106)"
    );
}

// -- R7: сноска о ловушке именования при `import` -----------------------------

#[test]
fn note_points_at_the_nested_model_that_has_states() {
    let found = diagnostics("uses_wrapper.takt");
    let (path, source) = fixture("uses_wrapper.takt");
    let diagnostics = collect_compile_diagnostics(&path, &source, &[DIR.to_string()], false);
    let se106 = diagnostics
        .iter()
        .find(|d| d.code.as_deref() == Some("SE-106"))
        .unwrap_or_else(|| panic!("ожидалась SE-106, получено: {found:#?}"));
    let notes: Vec<&str> = se106.notes.iter().map(|n| n.message.as_str()).collect();
    assert!(
        notes.iter().any(|n| n.contains("'Helper'")),
        "сноска обязана назвать вложенную модель, у которой состояния ЕСТЬ: без \
         неё сообщение выглядит ложью — `start` в подключённом файле написан. \
         Получено: {notes:#?}"
    );
}

// -- R6: ни одна цель не пропускает отказ -------------------------------------

/// Каталог вывода, уникальный по имени потока: тесты идут параллельно. Каталог не
/// создаётся - отказ приходит до генерации.
fn out_dir(tag: &str) -> String {
    let thread = std::thread::current()
        .name()
        .unwrap_or("unnamed")
        .replace(':', "_");
    format!(
        "{}/takt0211_{thread}_{tag}",
        std::env::temp_dir()
            .join(format!("takt_pid{}", std::process::id()))
            .display()
    )
}

#[test]
fn every_target_refuses_the_same_input() {
    let (path, source) = fixture("direct.takt");
    let options = GenerateOptions::default();
    let empty: [takt_lang::address_map::AddressMapEntry; 0] = [];
    let env = takt_lang::address_map::AddressEnv::default();
    let search: [String; 0] = [];

    // Список входов генерации - **весь**. Цель, которая перестанет отказывать, обязана
    // попасть в отчёт по имени: молчаливый успех и есть дефект, ради которого фича
    // заведена (цель `plantuml` печатала `[*] --> `).
    let attempts: Vec<(
        &str,
        Result<Vec<takt_lang::diagnostics::Diagnostic>, takt_lang::diagnostics::Diagnostic>,
    )> = vec![
        (
            "c",
            takt_lang::compile_to_c(&path, &source, &out_dir("c"), &search, &options),
        ),
        (
            "c-hal",
            takt_lang::compile_to_c_hal(
                &path,
                &source,
                &out_dir("c_hal"),
                &search,
                &empty,
                &env,
                &options,
            ),
        ),
        (
            "st",
            takt_lang::compile_to_st(&path, &source, &out_dir("st"), &search, &options),
        ),
        (
            "st-at",
            takt_lang::compile_to_st_at(
                &path,
                &source,
                &out_dir("st_at"),
                &search,
                &empty,
                &env,
                &options,
            ),
        ),
        (
            "rust",
            takt_lang::compile_to_rust(&path, &source, &out_dir("rust"), &search, &options),
        ),
        (
            "sv",
            takt_lang::compile_to_sv(&path, &source, &out_dir("sv"), &search, &options),
        ),
        (
            "plantuml",
            takt_lang::compile_to_plantuml(&path, &source, &out_dir("plantuml"), &search),
        ),
    ];

    let silent: Vec<&str> = attempts
        .iter()
        .filter(|(_, result)| !matches!(result, Err(d) if d.code.as_deref() == Some("SE-106")))
        .map(|(target, _)| *target)
        .collect();
    assert!(
        silent.is_empty(),
        "цели, не ответившие SE-106 на модель без состояний: {silent:?}"
    );
}
