//! Структура без полей - `SE-115`.
//!
//! ## Что здесь ловится
//!
//! Запись `struct Empty { }` язык принимал, а вывод не принимали **чужие** инструменты,
//! и каждый по-своему:
//!
//! | Потребитель | Что происходило |
//! |---|---|
//! | эталон | исполнял: переменная получала `Empty{}` |
//! | `c` | `typedef struct Empty { } Empty;` - расширение GNU, `cc -pedantic` даёт `-Wgnu-empty-struct` |
//! | `st`, `st-at` | `iec2c` **отвергает**: "no structure element declared in structure type declaration" |
//!
//! **Контрольный вход отделяет причину от следствия:** непустая структура проходит и
//! `cc -std=c11 -pedantic`, и `iec2c`. Ломается именно пустота, а не структурный тип.
//!
//! **Проверка проекта класс не видел по устройству:** он гоняет `cc` **без** `-pedantic`
//! (там пустая структура законна как расширение), а в корпусе `examples/` пустых
//! структур нет ни одной.
//!
//! Симметрия с `SE-105` (перечисление без вариантов) - та же форма ("агрегат без
//! элементов") и тот же отказ **на объявлении**; причины разные, потому и коды разные.

use takt_lang::collect_compile_diagnostics;
use takt_lang::diagnostics::{Diagnostic, Location, line_column};

const DIR: &str = "tests/data/semantic/invalid";

fn fixture(name: &str) -> (String, String) {
    let path = format!("{DIR}/{name}");
    let source =
        std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("не прочитать {path}: {e}"));
    (path, source)
}

fn diagnostics(source: &str) -> Vec<Diagnostic> {
    collect_compile_diagnostics("model.takt", source, &[], false)
}

fn codes(source: &str) -> Vec<String> {
    diagnostics(source)
        .into_iter()
        .filter_map(|d| d.code)
        .collect()
}

/// **T1: пустая структура - `SE-115`.**
#[test]
fn empty_struct_is_se115() {
    let (_, source) = fixture("empty_struct.takt");
    assert!(
        codes(&source).contains(&"SE-115".to_string()),
        "ожидался SE-115, получено: {:?}",
        codes(&source)
    );
}

/// **T2: отказ указывает на объявление, а не на использование.**
///
/// Ошибка в объявлении, и координата обязана вести туда: у пустой структуры
/// использований может не быть вовсе, а `SE-105` разбирался тем же доводом.
#[test]
fn se115_points_at_the_declaration() {
    let (_, source) = fixture("empty_struct.takt");
    let d = diagnostics(&source)
        .into_iter()
        .find(|d| d.code.as_deref() == Some("SE-115"))
        .expect("SE-115");
    let Location::Source(_, start, _) = d.loc else {
        panic!("позиция обязана быть в исходнике, получено {:?}", d.loc);
    };
    let (line, _) = line_column(&source, start.try_into().expect("смещение"));
    let declaration_line = source
        .lines()
        .position(|l| l.starts_with("struct Empty"))
        .expect("строка объявления")
        + 1;
    assert_eq!(
        line, declaration_line,
        "координата обязана вести на объявление структуры"
    );
}

/// **T3: текст называет правило и выход.**
///
/// Диагностика, которая только запрещает, оставляет автора без ответа "а как тогда";
/// здесь названы и требование, и оба чужих инструмента, из-за которых оно введено.
#[test]
fn se115_text_names_the_rule_and_the_way_out() {
    let (_, source) = fixture("empty_struct.takt");
    let d = diagnostics(&source)
        .into_iter()
        .find(|d| d.code.as_deref() == Some("SE-115"))
        .expect("SE-115");
    for needle in ["Empty", "хотя бы одно поле", "удалите объявление"]
    {
        assert!(
            d.message.contains(needle),
            "текст обязан содержать «{needle}»: {}",
            d.message
        );
    }
}

/// **T4: накопление - одна диагностика на объявление.**
#[test]
fn every_empty_struct_reports_its_own() {
    let (_, source) = fixture("empty_struct_many.takt");
    let got: Vec<String> = codes(&source)
        .into_iter()
        .filter(|c| c == "SE-115")
        .collect();
    assert_eq!(
        got.len(),
        2,
        "каждое объявление обязано высказаться, получено: {:?}",
        codes(&source)
    );
}

/// **T5. Контроль: непустая структура законна.**
///
/// Без этой проверки правило можно было бы "исполнить", запретив структуры вовсе.
/// Контроль важен и по существу: замер показал, что непустая структура проходит
/// настоящие `cc -pedantic` и `iec2c`.
#[test]
fn non_empty_struct_stays_legal() {
    let source = "struct Pt { a: u8, b: u8 }\n\
                  var p: Pt;\n\
                  var n: u8 := 0;\n\
                  start Run { always { n := n + 1; p := p; } ref Run: n < 3; }\n";
    assert!(
        codes(source).is_empty(),
        "непустая структура законна, получено: {:?}",
        codes(source)
    );
}

/// **T6. Граница: пустое перечисление судится своим кодом.**
///
/// Форма та же ("агрегат без элементов"), причины разные: у перечисления выбирать
/// нечего, у структуры вывод не принимают чужие инструменты. Подмена кодов размыла бы
/// обе.
#[test]
fn empty_enum_keeps_its_own_code() {
    let (_, source) = fixture("empty_enum.takt");
    let got = codes(&source);
    assert!(
        got.contains(&"SE-105".to_string()) && !got.contains(&"SE-115".to_string()),
        "перечисление судит SE-105, получено: {got:?}"
    );
}
