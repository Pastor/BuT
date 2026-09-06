//! Интеграционные тесты полноты адресов портов.
//!
//! Порт, достижимый кодогенерацией (используемый в логике), обязан иметь адрес; мёртвый
//! порт без адреса - допустим. Предупреждение - `SE-052`.

use takt_lang::port_address_completeness_warnings;
use takt_lang::semantic::tree::construct_model;

fn model_of(src: &str) -> std::rc::Rc<std::cell::RefCell<takt_lang::semantic::ModelNode>> {
    let (ast, _) = takt_lang::parse(src, 0).expect("ошибка разбора .takt");
    construct_model(&ast, None, &[]).expect("ошибка построения модели")
}

/// Коды предупреждений полноты для модели и списка портов из внешней карты.
fn completeness_codes(src: &str, external: &[&str]) -> Vec<String> {
    let model = model_of(src);
    let ext: Vec<String> = external.iter().map(|s| s.to_string()).collect();
    port_address_completeness_warnings(model, &ext)
        .into_iter()
        .map(|d| d.code.unwrap_or_default())
        .collect()
}

/// Используемый порт без адреса -> SE-052.
#[test]
fn used_port_without_address_warns_se052() {
    let codes = completeness_codes("in BTN: bit; start S { ref T: BTN; } state T;", &[]);
    assert_eq!(codes, vec!["SE-052"]);
}

/// Мёртвый (неиспользуемый) порт без адреса -> без предупреждений.
#[test]
fn dead_port_without_address_is_silent() {
    let codes = completeness_codes("in BTN: bit; start S; state T;", &[]);
    assert!(
        codes.is_empty(),
        "мёртвый порт не требует адреса: {:?}",
        codes
    );
}

/// Используемый порт с inline-адресом -> без предупреждений.
#[test]
fn used_port_with_inline_address_is_silent() {
    let codes = completeness_codes(
        "in BTN: bit at 0x00100000:0; start S { ref T: BTN; } state T;",
        &[],
    );
    assert!(
        codes.is_empty(),
        "inline-адрес закрывает требование: {:?}",
        codes
    );
}

/// Используемый порт с адресом через оператор `address` -> без предупреждений.
#[test]
fn used_port_with_operator_address_is_silent() {
    let codes = completeness_codes(
        "in BTN: bit; address BTN = 0x00100000; start S { ref T: BTN; } state T;",
        &[],
    );
    assert!(
        codes.is_empty(),
        "оператор address закрывает требование: {:?}",
        codes
    );
}

/// Используемый порт, покрытый внешней картой -> без предупреждений.
#[test]
fn used_port_covered_by_external_map_is_silent() {
    let codes = completeness_codes("in BTN: bit; start S { ref T: BTN; } state T;", &["BTN"]);
    assert!(
        codes.is_empty(),
        "внешняя карта закрывает требование: {:?}",
        codes
    );
}
