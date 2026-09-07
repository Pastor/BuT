//! Левая часть присваивания - место записи: `SE-111` и `SE-112`.
//!
//! ## Что здесь ловится
//!
//! нормировала **позиции** присваивания, но левую часть не судил никто: грамматика
//! принимает слева произвольное выражение.
//!
//! Запись в константу была хуже: там расходились не сообщения, а **значения** - эталон
//! молча исполнял (`n=1`), цель `c` печатала `5 = 1;`.
//!
//! ## Устройство набора
//!
//! Теста двое, и оба падают **списком**:
//!
//! - отрицательный перечисляет незаконные формы и называет ту, что осталась
//!   без отказа;
//! - положительный перечисляет **все** законные места и называет ту, что
//!   отказ получила зря. Без него правило легко "ужесточить" до неработающего
//!   языка: список мест конечен и обязан быть виден целиком.
//!
//! Прогон идёт через `collect_compile_diagnostics` - тот же вход, что у CLI и LSP:
//! важно, что диагностика **доезжает** до пользователя, а не только вырабатывается
//! внутри.

use takt_lang::collect_compile_diagnostics;
use takt_lang::diagnostics::{Diagnostic, Level};

/// Общая обвязка пробы: объявления, при которых форма левой части осмысленна.
const PRELUDE: &str = "struct Inner { v: u8 }\n\
                       struct Pt { x: u8, y: Inner }\n\
                       const K: u8 := 5;\n\
                       var n: u8 := 0;\n\
                       var m: u8 := 0;\n\
                       var b: u8 := 0;\n\
                       var arr: [u8; 4] := { 0, 0, 0, 0 };\n\
                       var p: Pt := { 1, { 2 } };\n\
                       out led: bit at 0x100:3;\n\
                       fn f(x: u8) -> u8 { return x; }\n";

/// Собирает исходник с одним присваиванием в теле `always`.
fn probe(lhs: &str) -> String {
    format!("{PRELUDE}start Run {{ always {{ {lhs} := 1; }} }}\n")
}

/// Ошибки (без предупреждений) для исходника - тем же входом, что у CLI и LSP.
fn errors(source: &str) -> Vec<Diagnostic> {
    collect_compile_diagnostics("probe.takt", source, &[], false)
        .into_iter()
        .filter(|d| matches!(d.level, Level::Error))
        .collect()
}

/// Коды ошибок для исходника.
fn error_codes(source: &str) -> Vec<String> {
    errors(source).into_iter().filter_map(|d| d.code).collect()
}

/// Незаконные левые части: форма и слово, которым её обязан назвать отказ.
///
/// Список - **свидетельство замера**, а не выдумка: каждая форма проверена на цели `c`
/// до фичи и печаталась невалидным C (`5 = 1;`, `model->n + model->m = 1;`, `-model->n
/// = 1;`, `(uint8_t)model->n = 1;`, `!model->n = 1;`).
const NOT_A_PLACE: &[(&str, &str)] = &[
    ("f(n)", "вызов функции"),
    ("(f(n))", "вызов функции"),
    ("5", "литерал"),
    ("n + m", "арифметическое выражение"),
    ("n * 2", "арифметическое выражение"),
    ("n & m", "побитовое выражение"),
    ("n = m", "сравнение"),
    ("-n", "смена знака"),
    ("+n", "унарный плюс"),
    ("!n", "логическое отрицание"),
    ("~n", "побитовое отрицание"),
    ("n as u8", "приведение типа"),
];

/// Законные места записи - **весь** список, каким его знает язык.
const A_PLACE: &[&str] = &[
    "n",        // переменная
    "(n)",      // скобки прозрачны
    "p.x",      // поле структуры
    "p.y.v",    // вложенное поле
    "arr[1]",   // элемент массива
    "arr[n]",   // элемент по вычисленному индексу
    "arr[0:2]", // срез (цели его не переводят - это их дело, не языка)
    "b.2",      // отдельный бит
    "led",      // порт
    "#0x200.5", // ячейка по адресу
];

/// **Незаконная форма левой части - `SE-111`.** Падает списком: молчащая
/// форма названа поимённо.
#[test]
fn every_non_place_left_hand_side_is_rejected() {
    let silent: Vec<&str> = NOT_A_PLACE
        .iter()
        .filter(|(lhs, _)| !error_codes(&probe(lhs)).contains(&"SE-111".to_string()))
        .map(|(lhs, _)| *lhs)
        .collect();
    assert!(
        silent.is_empty(),
        "эти формы левой части остались без SE-111: {silent:?}"
    );
}

/// **Отказ называет вид формы словом, а не дампом узла.**
///
/// Здесь текст сверяется с ожидаемым словом, а имя варианта Rust в нём запрещено.
#[test]
fn refusal_names_the_kind_in_words() {
    let wrong: Vec<String> = NOT_A_PLACE
        .iter()
        .filter_map(|(lhs, kind)| {
            let text = errors(&probe(lhs))
                .into_iter()
                .find(|d| d.code.as_deref() == Some("SE-111"))
                .map(|d| d.message)
                .unwrap_or_default();
            (!text.contains(kind)).then(|| format!("{lhs}: ожидалось «{kind}», получено «{text}»"))
        })
        .collect();
    assert!(wrong.is_empty(), "вид формы назван неверно: {wrong:?}");
}

/// **Запись в константу - `SE-112`, и текст называет её имя.**
///
/// До фичи этот вход давал молчаливое расхождение: эталон исполнял запись (трасса
/// `n=1`), цель `c` печатала `CONST_K = 1;` при `#define CONST_K 5`.
#[test]
fn write_to_constant_is_rejected_by_name() {
    let found = errors(&probe("K"))
        .into_iter()
        .find(|d| d.code.as_deref() == Some("SE-112"))
        .expect("запись в константу обязана отвергаться");
    assert!(
        found.message.contains('K'),
        "отказ обязан называть константу: {}",
        found.message
    );
}

/// **A3б: константа под индексом и полем - тоже отказ.** Носитель ищется по
/// основанию места, а не по верхнему узлу.
#[test]
fn constant_under_selector_is_rejected_too() {
    let source = "const ARR: [u8; 4] := { 0, 0, 0, 0 };\n\
                  start Run { always { ARR[1] := 1; } }\n";
    assert!(
        error_codes(source).contains(&"SE-112".to_string()),
        "запись в элемент константного массива обязана отвергаться: {:?}",
        error_codes(source)
    );
}

/// **Законные места проходят молча.** Падает списком: место, получившее
/// отказ зря, названо поимённо.
///
/// Тест обязателен. Правило легко ужесточить до неработающего языка, а заметить это
/// на корпусе нельзя: записи бита в переменную в `examples/` нет ни одной.
#[test]
fn every_place_of_the_language_is_accepted() {
    let refused: Vec<String> = A_PLACE
        .iter()
        .filter_map(|lhs| {
            let codes: Vec<String> = error_codes(&probe(lhs))
                .into_iter()
                .filter(|c| c == "SE-111" || c == "SE-112")
                .collect();
            (!codes.is_empty()).then(|| format!("{lhs}: {codes:?}"))
        })
        .collect();
    assert!(
        refused.is_empty(),
        "законные места отвергнуты зря: {refused:?}"
    );
}

/// **Накопление - несколько нарушений дают несколько диагностик.**
///
/// Образец `literal_range`: редактор подчёркивает каждое, а не первое.
#[test]
fn violations_accumulate() {
    let source =
        format!("{PRELUDE}start Run {{ always {{ f(n) := 1; 5 := 2; n + m := 3; K := 4; }} }}\n");
    let codes = error_codes(&source);
    let places = codes.iter().filter(|c| *c == "SE-111").count();
    let constants = codes.iter().filter(|c| *c == "SE-112").count();
    assert_eq!(places, 3, "ожидались три SE-111: {codes:?}");
    assert_eq!(constants, 1, "ожидалась одна SE-112: {codes:?}");
}

/// **Правило действует во всех позициях обхода, а не только в `always`.**
///
/// Судья ничего не знает о позициях - их знает обход `validate/bodies.rs`. Пропущенная
/// позиция и есть предмет обхода, ради которого он заведён.
#[test]
fn rule_holds_in_every_body_the_traversal_visits() {
    let cases: &[(&str, String)] = &[
        (
            "тело функции",
            format!("{PRELUDE}fn g() -> u8 {{ f(n) := 1; return 0; }}\nstart Run {{ }}\n"),
        ),
        (
            "вложенный if",
            format!("{PRELUDE}start Run {{ always {{ if n = 0 {{ f(n) := 1; }} }} }}\n"),
        ),
        (
            "тело while",
            format!("{PRELUDE}start Run {{ always {{ while n = 0 {{ f(n) := 1; }} }} }}\n"),
        ),
        (
            "шаг цикла for",
            format!(
                "{PRELUDE}start Run {{ always {{ for var i: u8 := 0; i < 2; f(i) := 1 {{ }} }} }}\n"
            ),
        ),
        (
            "именованный блок enter",
            format!("{PRELUDE}start Run {{ enter {{ f(n) := 1; }} }}\n"),
        ),
    ];
    let silent: Vec<&str> = cases
        .iter()
        .filter(|(_, source)| !error_codes(source).contains(&"SE-111".to_string()))
        .map(|(name, _)| *name)
        .collect();
    assert!(
        silent.is_empty(),
        "в этих позициях правило не сработало: {silent:?}"
    );
}

/// **Под-модель судится наравне с корнем.** Обход спускается в `models`.
#[test]
fn nested_model_is_judged_too() {
    let source = "model Child {\n\
                  \x20   var c: u8 := 0;\n\
                  \x20   fn h(x: u8) -> u8 { return x; }\n\
                  \x20   start Inner { always { h(c) := 1; } }\n\
                  }\n\
                  var n: u8 := 0;\n\
                  start Run { always { n := 1; } }\n";
    assert!(
        error_codes(source).contains(&"SE-111".to_string()),
        "тело под-модели обязано судиться: {:?}",
        error_codes(source)
    );
}

/// **Параметр модели под запрет `SE-112` не попадает.**
///
/// понижает в `Const` только параметр без присваиваний; параметру, которому пишут, флаг
/// `mutated` не даёт понизиться. Риск назван в анализе - тест его закрывает.
#[test]
fn model_parameter_is_writable() {
    let source = "model Tuner {\n\
                  \x20   parameter GAIN: u8 := 2;\n\
                  \x20   start S { always { GAIN := 3; } }\n\
                  }\n\
                  var n: u8 := 0;\n\
                  start Run { always { n := 1; } }\n";
    let codes = error_codes(source);
    assert!(
        !codes.contains(&"SE-112".to_string()),
        "параметр модели — поле экземпляра, писать в него законно: {codes:?}"
    );
}
