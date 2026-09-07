//! `SE-089`: литерал не помещается в тип приёмника.
//!
//! # Что доказывается
//!
//! Диагностика заводится вместе с расширением приёма литерала. Без неё расширение
//! увеличило бы класс "валидный `.takt` -> невалидный вывод": проба (П9) показала, что
//! уже `var a: u8 := 300;` компилировался с рапортом об успехе, а `cc -Wall -Werror`
//! порождённый C отвергал (`-Wconstant-conversion`).
//!
//! Проверки идут через `collect_compile_diagnostics` - тот же вход, которым пользуются
//! CLI и LSP, а не через внутренности прохода.

use takt_lang::collect_compile_diagnostics;

fn codes(source: &str) -> Vec<String> {
    collect_compile_diagnostics("проба.takt", source, &[], false)
        .iter()
        .filter_map(|d| d.code.clone())
        .collect()
}

fn diagnostics_text(source: &str) -> String {
    collect_compile_diagnostics("проба.takt", source, &[], false)
        .iter()
        .map(|d| d.message.clone())
        .collect::<Vec<_>>()
        .join("\n")
}

/// Инициализатор беззнакового типа: `u8 := 300` -> `SE-089`.
#[test]
fn unsigned_initializer_out_of_range_is_se089() {
    let source = "model M { var a: u8 := 300; start S { always { a := a; } } } start Root = M;";
    assert!(
        codes(source).contains(&"SE-089".to_string()),
        "ожидался SE-089, получено: {:?}",
        codes(source)
    );
}

/// Инициализатор знакового типа: `i8 := -200` -> `SE-089`.
#[test]
fn signed_initializer_out_of_range_is_se089() {
    let source = "model M { var b: i8 := -200; start S { always { b := b; } } } start Root = M;";
    assert!(codes(source).contains(&"SE-089".to_string()));
}

/// Присваивание в теле проверяется так же, как инициализатор.
#[test]
fn assignment_in_body_out_of_range_is_se089() {
    let source = "model M { var a: u8 := 0; start S { always { a := 999; } } } start Root = M;";
    assert!(
        codes(source).contains(&"SE-089".to_string()),
        "литерал в присваивании обязан проверяться: цель `c` печатает его \
         буквально, и вывод отвергает гейт"
    );
}

/// Сообщение называет **и** значение, **и** допустимый диапазон.
///
/// Диагностика без границ бесполезна: автор не увидит, на сколько промахнулся.
#[test]
fn message_states_value_and_bounds() {
    let source = "model M { var a: u8 := 300; start S { always { a := a; } } } start Root = M;";
    let text = diagnostics_text(source);
    assert!(text.contains("300"), "нет значения: {text}");
    assert!(text.contains("[0, 255]"), "нет границ диапазона: {text}");
    assert!(text.contains("'u8'"), "нет имени типа: {text}");
}

// -- Контрпримеры: границы обязаны быть точными -------------------------------

/// Крайние значения диапазона **валидны** - тест против "сузили лишнего".
#[test]
fn boundary_values_are_accepted() {
    for source in [
        "model M { var a: u8 := 255; start S { always { a := a; } } } start Root = M;",
        "model M { var a: u8 := 0; start S { always { a := a; } } } start Root = M;",
        "model M { var b: i8 := -128; start S { always { b := b; } } } start Root = M;",
        "model M { var b: i8 := 127; start S { always { b := b; } } } start Root = M;",
        "model M { var u: u64 := 18446744073709551615; start S { always { u := u; } } } start Root = M;",
        "model M { var i: i64 := -9223372036854775808; start S { always { i := i; } } } start Root = M;",
        "model M { var m: [bit;64] := 0xFFFFFFFFFFFFFFFF; start S { always { m := m; } } } start Root = M;",
    ] {
        assert!(
            !codes(source).contains(&"SE-089".to_string()),
            "крайнее значение диапазона обязано приниматься: {source}\n{:?}",
            codes(source)
        );
    }
}

/// Ширина бит-вектора учитывается: `[bit;8] := 256` - вне диапазона.
#[test]
fn bit_vector_width_bounds_the_literal() {
    let ok = "model M { var m: [bit;8] := 255; start S { always { m := m; } } } start Root = M;";
    let bad = "model M { var m: [bit;8] := 256; start S { always { m := m; } } } start Root = M;";
    assert!(!codes(ok).contains(&"SE-089".to_string()));
    assert!(codes(bad).contains(&"SE-089".to_string()));
}

/// Фикстура-контрпример строится и даёт ровно `SE-089` (три раза).
#[test]
fn fixture_reports_every_violation_not_just_first() {
    let source =
        std::fs::read_to_string("tests/data/semantic/invalid/literal_out_of_type_range.takt")
            .expect("чтение фикстуры");
    let found: Vec<String> = codes(&source)
        .into_iter()
        .filter(|c| c == "SE-089")
        .collect();
    assert_eq!(
        found.len(),
        3,
        "проверка накапливает находки (фича 0130): два инициализатора и одно \
         присваивание, а не «первая ошибка на модель»"
    );
}
