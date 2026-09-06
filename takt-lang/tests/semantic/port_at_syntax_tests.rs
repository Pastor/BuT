//! Размещение порта ключевым словом `at`.
//!
//! ## Что здесь ловится
//!
//! `at` - **необязательная** часть объявления: адрес может прийти по имени порта
//! (оператор `address` или внешняя карта `--address-map`), поэтому объявление без `at`
//! обязано оставаться законным, а полноту адреса проверяет слой адресов после
//! разрешения всех источников, а не разбор.
//!
//! **Переходное состояние.** Пока адресом считается и `at <адрес>`, и старая форма `:=
//! <адрес>`: корпус переходит на `at`, и лишь затем `:=` меняет смысл на начальное
//! значение.

use std::rc::Rc;
use takt_lang::address_map::{AddressEnv, parse_address_map, resolve_addresses};
use takt_lang::semantic::tree::construct_model;

/// Разрешает адреса модели; `external` - текст внешней карты.
fn resolve(source: &str, external: Option<&str>) -> takt_lang::address_map::AddressResolution {
    let (ast, _) = takt_lang::parse(source, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let entries = match external {
        Some(text) => parse_address_map(text, 0).expect("карта"),
        None => Vec::new(),
    };
    resolve_addresses(Rc::clone(&model), &entries, &AddressEnv::default())
}

/// Адрес порта по имени (карта ключуется квалифицированно - ищем по значению).
fn address_of(
    res: &takt_lang::address_map::AddressResolution,
    port: &str,
) -> Option<(i64, Option<i64>)> {
    res.map
        .values()
        .find(|r| r.name == port)
        .map(|r| (r.addr, r.bit))
}

#[test]
fn at_places_the_port() {
    let res = resolve(
        "in btn: bit at 0x40000000:0;\nvar t: u8 := 0;\nstart S { always { t := t + 1; } ref S: btn = 1; }\n",
        None,
    );
    assert_eq!(address_of(&res, "btn"), Some((0x4000_0000, Some(0))));
}

#[test]
fn declaration_without_at_is_legal_and_address_comes_by_name() {
    // R1a: отсутствие `at` - не отсутствие адреса. Здесь его задаёт оператор `address`
    // по имени порта.
    let res = resolve(
        "in temp: u8;\naddress temp = 0x40000002;\nvar t: u8 := 0;\n\
         start S { always { t := temp; } ref S: t = 9; }\n",
        None,
    );
    assert_eq!(address_of(&res, "temp"), Some((0x4000_0002, None)));
}

#[test]
fn external_map_still_addresses_a_port_declared_without_at() {
    let res = resolve(
        "in temp: u8;\nvar t: u8 := 0;\nstart S { always { t := temp; } ref S: t = 9; }\n",
        Some("temp = 0x50000000;\n"),
    );
    assert_eq!(address_of(&res, "temp"), Some((0x5000_0000, None)));
}

#[test]
fn external_map_overrides_at() {
    // Приоритет источников не меняется: карта бьёт объявление.
    let res = resolve(
        "in btn: bit at 0x40000000:0;\nvar t: u8 := 0;\n\
         start S { always { t := t + 1; } ref S: btn = 1; }\n",
        Some("btn = 0x60000000:3;\n"),
    );
    assert_eq!(address_of(&res, "btn"), Some((0x6000_0000, Some(3))));
}

#[test]
fn port_without_any_address_is_still_reported_after_resolution() {
    // R1b: полноту проверяет слой адресов, а не разбор объявления.
    let res = resolve(
        "in temp: u8;\nvar t: u8 := 0;\nstart S { always { t := temp; } ref S: t = 9; }\n",
        None,
    );
    let codes: Vec<_> = res
        .diagnostics
        .iter()
        .filter_map(|d| d.code.clone())
        .collect();
    assert!(
        codes.contains(&"SE-052".to_string()),
        "порт без единого источника адреса обязан давать SE-052: {codes:?}"
    );
}

#[test]
fn formatter_keeps_the_placement() {
    // Правило форматтера "добавил узел - добавь печать" защищает от новых узлов, а не
    // полей: новое поле компилятор разобрать не потребовал бы (`..` в образце), и адрес
    // молча пропал бы из вывода.
    let source = "in btn: bit at 0x40000000:0;\nvar t: u8 := 0;\nstart S {\n  always { t := t + 1; }\n  ref S: btn = 1;\n}\n";
    let formatted = takt_lang::format::format_source(source).expect("форматирование");
    assert!(
        formatted.contains("in btn: bit at 0x40000000:0;"),
        "размещение обязано печататься обратно:\n{formatted}"
    );
    // Круговой рейс: повторное форматирование ничего не меняет.
    let again = takt_lang::format::format_source(&formatted).expect("повторное форматирование");
    assert_eq!(
        formatted, again,
        "форматирование обязано быть идемпотентным"
    );
}
