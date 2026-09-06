//! Имя типа занимается один раз - `SE-107` и `SE-108`.
//!
//! # Что здесь ловится
//!
//! Все четыре формы принимались молча, а компилятор рапортовал об успехе:
//!
//! - `type u8 = i64;` -> цель `c` печатала `int64_t`: встроенный тип подменён и
//!   недоступен во всём файле;
//! - `struct u8 {...}`, `enum u8 {...}` -> то же затенение другими средствами;
//! - `type Level = u8; type Level = i64;` -> побеждало **последнее** объявление;
//! - `struct S {...}` дважды -> принималось, а затем приходила `SE-061`
//!   "структура 'S' не содержит поля 'a'" - диагностика **о следствии**,
//!   уводящая искать опечатку в имени поля.

use takt_lang::semantic::tree::construct_model;

/// Строит дерево из исходника, возвращая диагностику отказа.
fn refuse(src: &str) -> takt_lang::diagnostics::Diagnostic {
    let (ast, _) = takt_lang::parse(src, 0).expect("разбор");
    construct_model(&ast, None, &[]).expect_err("ожидался отказ")
}

/// Строит дерево, ожидая успех.
fn accept(src: &str) {
    let (ast, _) = takt_lang::parse(src, 0).expect("разбор");
    construct_model(&ast, None, &[]).expect("ожидалось построение");
}

// -- SE-107: имя занято языком ------------------------------------------------

/// **T1.** Псевдоним с именем встроенного типа отвергается.
#[test]
fn alias_over_builtin_is_refused() {
    let d = refuse("type u8 = i64; start S;");
    assert_eq!(d.code.as_deref(), Some("SE-107"), "{d:?}");
    assert!(d.message.contains("u8"), "текст обязан называть имя: {d:?}");
}

/// **T2.** Структура с именем встроенного типа отвергается.
#[test]
fn struct_over_builtin_is_refused() {
    let d = refuse("struct u8 { a: u16 } start S;");
    assert_eq!(d.code.as_deref(), Some("SE-107"), "{d:?}");
}

/// **T3.** Перечисление с именем встроенного типа отвергается.
#[test]
fn enum_over_builtin_is_refused() {
    let d = refuse("enum bool { Off, On } start S;");
    assert_eq!(d.code.as_deref(), Some("SE-107"), "{d:?}");
}

/// **T4.** Позиция указывает на **имя** в объявлении, а не на начало файла.
#[test]
fn refusal_carries_position_of_the_name() {
    let d = refuse("type u8 = i64; start S;");
    assert!(
        matches!(d.loc, takt_lang::diagnostics::Location::Source(_, start, _) if start > 0),
        "позиция обязана указывать на имя типа: {:?}",
        d.loc
    );
}

/// **T5.** `u128` встроенным не является - объявлять его законно.
///
/// Контр-пример к запрету: список имён берётся у `builtin_type_by_name`, и своя копия в
/// проверке разошлась бы с ним именно здесь.
#[test]
fn non_builtin_width_is_still_allowed() {
    accept("type u128 = [bit; 128]; start S;");
}

// -- SE-108: имя уже занято ---------------------------------------------------

/// **T6.** Повторный псевдоним отвергается и называет первое объявление.
#[test]
fn duplicate_alias_is_refused_with_first_position() {
    let d = refuse("type Level = u8; type Level = i64; start S;");
    assert_eq!(d.code.as_deref(), Some("SE-108"), "{d:?}");
    assert_eq!(d.notes.len(), 1, "заметка о первом объявлении обязательна");
    assert!(
        d.notes[0].message.contains("Level"),
        "заметка обязана называть имя: {:?}",
        d.notes[0]
    );
}

/// **T7.** Повторная структура отвергается - вместо прежней `SE-061` о поле.
///
/// Именно этот вход давал диагностику о следствии: второе объявление затирало первое, и
/// автор читал "структура 'S' не содержит поля 'a'".
#[test]
fn duplicate_struct_is_refused_instead_of_missing_field() {
    let d = refuse("struct S { a: u8 } struct S { b: u16 } start S2;");
    assert_eq!(d.code.as_deref(), Some("SE-108"), "{d:?}");
}

/// **T8.** Столкновение видов объявления: структура против перечисления.
#[test]
fn duplicate_across_declaration_kinds_is_refused() {
    let d = refuse("struct Mode { a: u8 } enum Mode { Off, On } start S;");
    assert_eq!(d.code.as_deref(), Some("SE-108"), "{d:?}");
}

/// **T9.** Разные имена по-прежнему принимаются.
#[test]
fn distinct_names_are_accepted() {
    accept("type Byte = [bit;8]; struct Point { x: u8 } enum Mode { Off, On } start S;");
}
