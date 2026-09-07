//! Начальное значение порта.
//!
//! ## Что здесь ловится
//!
//! После введения `at` и миграции корпуса `:=` в объявлении порта означает **начальное
//! значение**, а не адрес. Проверяется, что смысл действительно сменился: адрес берётся
//! только из `at`, значение - только из `:=`, а вход с начальным значением отвергается.
//!
//! Здесь же проверялось временное предупреждение `SE-093` ("значение
//! выставляют не все цели"). научила последние четыре цели, и
//! предупреждение снято: теперь порт с начальным значением обязан компилироваться
//! **молча** - тест этого и требует.

use std::rc::Rc;
use takt_lang::address_map::{AddressEnv, resolve_addresses};
use takt_lang::semantic::tree::construct_model;

fn error_codes(source: &str) -> Vec<String> {
    takt_lang::collect_compile_diagnostics("probe.takt", source, &[], false)
        .into_iter()
        .filter(|d| matches!(d.level, takt_lang::diagnostics::Level::Error))
        .filter_map(|d| d.code)
        .collect()
}

/// Предупреждения идут через **единую точку** `collect_model_warnings` - тот же вход,
/// что у `taktc compile`. `collect_compile_diagnostics` их не собирает: он отдаёт
/// ошибки компиляции.
fn warning_codes(source: &str) -> Vec<String> {
    let (ast, _) = takt_lang::parse(source, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    takt_lang::semantic::warnings::collect_model_warnings(&ast, &model)
        .into_iter()
        .filter_map(|d| d.code)
        .collect()
}

/// Адрес порта после разрешения источников.
fn address_of(source: &str, port: &str) -> Option<(i64, Option<i64>)> {
    let (ast, _) = takt_lang::parse(source, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let res = resolve_addresses(Rc::clone(&model), &[], &AddressEnv::default());
    res.map
        .values()
        .find(|r| r.name == port)
        .map(|r| (r.addr, r.bit))
}

#[test]
fn assignment_no_longer_places_the_port() {
    // Главное следствие фичи: `:=` больше не адрес. Порт остаётся без размещения, и
    // слой адресов сообщает об этом (SE-052), а не берёт значение за адрес.
    let source = "out led: bit := 0x40000004;\nvar t: u8 := 0;\n\
                  start S { always { led := 1; t := t + 1; } ref S: t = 9; }\n";
    assert_eq!(
        address_of(source, "led"),
        None,
        "значение из `:=` не имеет права становиться адресом"
    );
}

#[test]
fn placement_comes_only_from_at() {
    let source = "out led: bit at 0x40000004:2 := 1;\nvar t: u8 := 0;\n\
                  start S { always { led := 1; t := t + 1; } ref S: t = 9; }\n";
    assert_eq!(address_of(source, "led"), Some((0x4000_0004, Some(2))));
}

#[test]
fn input_port_with_initial_value_is_rejected() {
    let codes = error_codes(
        "in btn: bit at 0x40000000:0 := 1;\nvar t: u8 := 0;\n\
         start S { always { t := t + 1; } ref S: btn = 1; }\n",
    );
    assert!(
        codes.contains(&"SE-092".to_string()),
        "вход с начальным значением обязан отвергаться: {codes:?}"
    );
}

#[test]
fn output_initial_value_is_silent_now_that_all_targets_emit_it() {
    // Тест проверяет именно снятие: вернувшееся `SE-093` означало бы, что какая-то цель
    // снова теряет значение молча.
    let codes = warning_codes(
        "out led: bit at 0x40000004:2 := 1;\nvar t: u8 := 0;\n\
         start S { always { led := 1; t := t + 1; } ref S: t = 9; }\n",
    );
    assert!(
        !codes.contains(&"SE-093".to_string()),
        "значение выставляют все цели — предупреждение снято: {codes:?}"
    );
}

#[test]
fn port_without_initial_value_is_silent() {
    let codes = warning_codes(
        "out led: bit at 0x40000004:2;\nvar t: u8 := 0;\n\
         start S { always { led := 1; t := t + 1; } ref S: t = 9; }\n",
    );
    assert!(
        codes.is_empty(),
        "порт без начального значения — не повод для шума: {codes:?}"
    );
}
