//! Проверка охранных формул.
//!
//! ## Что здесь ловится
//!
//! `validate` не обходил формулы вовсе: `: [Guard] levl < 3;` (опечатка в имени)
//! принималось **молча**. Средство безопасности переставало проверять, а автор об этом
//! не узнавал: цель `c` печатала `assert( < 3);` - и отказ приходил от чужого
//! инструмента (`cc: expected expression`), цель `rust` отвечала `RS-011`, цели
//! `st`/`sv` формулу не печатали вовсе, а симулятор падал `SIM-016` **в такте**. Один
//! вход - четыре разных ответа.

use takt_lang::collect_compile_diagnostics;

/// Читает фикстуру каталога `tests/data/formula0203/`.
fn fixture(name: &str) -> (String, String) {
    let path = format!("tests/data/formula0203/{name}");
    let source =
        std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("не прочитать {path}: {e}"));
    (path, source)
}

/// Диагностики фикстуры: пары "код - сообщение".
fn diagnostics(name: &str) -> Vec<(String, String)> {
    let (path, source) = fixture(name);
    collect_compile_diagnostics(&path, &source, &[], false)
        .iter()
        .map(|d| (d.code.clone().unwrap_or_default(), d.message.clone()))
        .collect()
}

/// Требует ровно одну `SE-025`, называющую потерянное имя.
fn expect_single_se025(name: &str, lost: &str) {
    let found = diagnostics(name);
    let se025: Vec<_> = found.iter().filter(|(code, _)| code == "SE-025").collect();
    assert_eq!(
        se025.len(),
        1,
        "{name}: ожидалась ровно одна SE-025, получено: {found:#?}"
    );
    assert!(
        se025[0].1.contains(lost),
        "{name}: SE-025 обязана назвать имя '{lost}' (цитата записи автора), получено: {:?}",
        se025[0].1
    );
}

// -- Шесть мест объявления формулы --------------------------------------------

#[test]
fn unknown_name_in_model_level_formula_is_diagnosed() {
    expect_single_se025("guard_model_unknown.takt", "levl");
}

#[test]
fn unknown_name_in_state_formula_is_diagnosed() {
    expect_single_se025("guard_state_unknown.takt", "levl");
}

#[test]
fn unknown_name_in_model_named_block_formula_is_diagnosed() {
    expect_single_se025("guard_model_block_unknown.takt", "levl");
}

#[test]
fn unknown_name_in_state_named_block_formula_is_diagnosed() {
    expect_single_se025("guard_state_block_unknown.takt", "levl");
}

#[test]
fn unknown_name_in_function_body_formula_is_diagnosed() {
    expect_single_se025("guard_fn_unknown.takt", "levl");
}

#[test]
fn unknown_name_in_nested_statement_formula_is_diagnosed() {
    expect_single_se025("guard_nested_unknown.takt", "levl");
}

/// Обход остаётся **один**, и все шесть мест высказываются.
///
/// Отдельный тест поверх шести - не дубль: он падает **списком**, называя осиротевшее
/// место, и потому читается как контракт обхода. Потеря места в `formula/sites.rs`
/// роняет и его, и соответствующий точечный тест.
#[test]
fn every_declaration_site_of_a_formula_is_checked() {
    let sites = [
        ("тело модели", "guard_model_unknown.takt"),
        ("тело состояния", "guard_state_unknown.takt"),
        ("именованный блок модели", "guard_model_block_unknown.takt"),
        (
            "именованный блок состояния",
            "guard_state_block_unknown.takt",
        ),
        ("тело функции", "guard_fn_unknown.takt"),
        ("вложенный оператор", "guard_nested_unknown.takt"),
    ];
    let silent: Vec<&str> = sites
        .iter()
        .filter(|(_, file)| !diagnostics(file).iter().any(|(code, _)| code == "SE-025"))
        .map(|(site, _)| *site)
        .collect();
    assert!(
        silent.is_empty(),
        "формула молчит в местах: {silent:?} — место выпало из общего обхода \
         `semantic/formula/sites.rs`"
    );
}

// -- Формы записи и накопление  -----------------------------------

#[test]
fn unknown_name_in_short_form_formula_is_diagnosed() {
    // Краткая запись `: c;` - та же формула без `[Guard]`; своей лазейки у неё быть не
    // должно.
    expect_single_se025("guard_short_unknown.takt", "levl");
}

#[test]
fn invariant_with_unknown_name_reports_exactly_once() {
    // `invariant Имя = c;` десахаризуется в `cond Имя = c;` **плюс**
    // `Formula::Guard(<ссылка на Имя>)`. Проверяются оба, но ссылка на условие
    // разрешается, поэтому сообщение остаётся одно. Проверять это надо счётом, а не
    // фактом наличия: изменится порядок регистрации - задвоение появится молча.
    expect_single_se025("invariant_unknown.takt", "levl");
}

#[test]
fn two_broken_formulas_yield_two_diagnostics() {
    let found = diagnostics("guard_two_unknown.takt");
    let se025: Vec<_> = found.iter().filter(|(code, _)| code == "SE-025").collect();
    assert_eq!(
        se025.len(),
        2,
        "накопление (правило 0151): каждая формула высказывается своя, получено: {found:#?}"
    );
}

// -- Границы объёма  --------------------------------------------------

#[test]
fn ltl_formula_with_unknown_atom_is_not_an_error() {
    // `Formula::LTL` в объём не входит: у неё своя проверка и иной режим строгости -
    // предупреждение SE-056, потому что абстракция LTL заведомо сверх-аппроксимирует
    // предупреждение. Ошибкой оно стать не должно.
    let found = diagnostics("ltl_unknown_atom.takt");
    let errors: Vec<_> = found.iter().filter(|(code, _)| code == "SE-025").collect();
    assert!(
        errors.is_empty(),
        "LTL-атом не обязан разрешаться как условие, получено: {found:#?}"
    );
}

#[test]
fn resolvable_formulas_are_accepted_in_all_sites() {
    let found = diagnostics("guard_resolvable.takt");
    assert!(
        found.is_empty(),
        "формулы с разрешимыми именами обязаны приниматься молча, получено: {found:#?}"
    );
}

// -- Попутные проверки судьи --------------------------------------------------

#[test]
fn formula_gets_the_other_condition_checks_too() {
    // Судья у формул тот же, что у `cond` и рёбер, поэтому формулам достаются и прочие
    // его проверки - например состояние, которого в указанной модели нет (SE-033). Ради
    // этого проверка и доставляет условие судье, а не судит сама.
    let found = diagnostics("guard_state_of_unknown_state.takt");
    assert!(
        found.iter().any(|(code, _)| code == "SE-033"),
        "ожидалась SE-033 о состоянии чужой модели, получено: {found:#?}"
    );
}

#[test]
fn bare_model_state_pattern_is_accepted_everywhere() {
    // Краткая форма паттерна (`Модель != Состояние` без `S(...)`) законна наравне с
    // полной - Теперь форму разбирает одна функция на проект -
    // `semantic::condition::state_of`.
    let found = diagnostics("guard_bare_model_state.takt");
    assert!(
        found.is_empty(),
        "краткая форма паттерна обязана приниматься и в формуле, и на ребре, получено: {found:#?}"
    );
}
