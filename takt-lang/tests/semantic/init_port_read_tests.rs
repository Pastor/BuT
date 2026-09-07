//! Чтение порта в инициализаторе объявления - `SE-113`.
//!
//! ## Что здесь ловится
//!
//! При `sensor = 0` ноль эталона и чтение железа совпадают **случайно** - расхождение
//! ничем себя не выдаёт.
//!
//! Тест стоит на **семантике**, а не на целях: отказ приходит до генерации, и потому
//! одинаков у всех девяти потребителей. Проверки идут через
//! `collect_compile_diagnostics` - общий вход CLI и языкового сервера: так
//! доказывается, что диагностика доезжает и до редактора.
//!
//! Правило общее с `SE-099` (ячейка по адресу) и обход у них **один**: A4 проверяет, что
//! общая воронка не потеряла соседа.

use takt_lang::diagnostics::Diagnostic;

/// Диагностики общего входа CLI и LSP.
fn diagnostics(source: &str) -> Vec<Diagnostic> {
    takt_lang::pipeline::collect_compile_diagnostics("model.takt", source, &[], false)
}

/// Коды диагностик по порядку.
fn codes(source: &str) -> Vec<String> {
    diagnostics(source)
        .into_iter()
        .filter_map(|d| d.code)
        .collect()
}

/// Хвост модели: состояние нужно, иначе вход отвергает `SE-102`.
const TAIL: &str = "start Run { }\n";

/// **Чтение порта в инициализаторе - `SE-113`, с именем порта и обходом.**
///
/// Текст обязан назвать **и** порт, **и** способ снять отказ: диагностика, которая
/// только запрещает, оставляет автора без ответа "а как тогда".
#[test]
fn port_read_in_initializer_is_rejected() {
    let found = diagnostics(&format!(
        "in sensor: bit;\n\
         var mirror: u8 := sensor;\n\
         {TAIL}"
    ));
    let d = found
        .iter()
        .find(|d| d.code.as_deref() == Some("SE-113"))
        .unwrap_or_else(|| panic!("ожидался SE-113, получено: {found:?}"));
    assert!(
        d.message.contains("sensor"),
        "сообщение обязано назвать порт: {}",
        d.message
    );
    assert!(
        d.message.contains("mirror"),
        "сообщение обязано назвать объявление: {}",
        d.message
    );
    assert!(
        d.message.contains("always"),
        "сообщение обязано назвать обход — чтение в теле: {}",
        d.message
    );
}

/// **Диагностика накапливается - одна на объявление.**
///
/// Ранний выход внутри одного выражения сохранён, но второе объявление обязано
/// высказаться: иначе автор чинит по одному отказу за прогон.
#[test]
fn every_declaration_reports_its_own_diagnostic() {
    let got = codes(&format!(
        "in a: bit;\n\
         in b: bit;\n\
         var x: u8 := a;\n\
         var y: u8 := b;\n\
         {TAIL}"
    ));
    assert_eq!(
        got,
        vec!["SE-113", "SE-113"],
        "каждое объявление обязано высказаться"
    );
}

/// **Чтение порта в теле остаётся законным.**
///
/// Запрет передвигает чтение туда, где значение определено, а не отнимает его. Без этой
/// проверки правило можно было бы "исполнить" запретом порта вообще.
#[test]
fn port_read_in_body_stays_legal() {
    let got = codes(
        "in sensor: bit;\n\
         var mirror: u8 := 0;\n\
         start Run {\n\
             always { mirror := sensor; }\n\
         }\n",
    );
    assert!(
        got.is_empty(),
        "чтение порта в теле законно, получено: {got:?}"
    );
}

/// **Сосед по воронке цел - `SE-099` по-прежнему выдаётся.**
///
/// Обход у двух правил один; сведя их, легко потерять первое. Мутация "убрать ветвь
/// `AnonPort`" валит именно этот тест.
#[test]
fn cell_read_still_reports_se099() {
    let got = codes(&format!(
        "var flags: u32 := #0x40000010 as u32;\n\
         {TAIL}"
    ));
    assert_eq!(got, vec!["SE-099"], "ячейка по адресу — прежний SE-099");
}

/// **Константа судится наравне с переменной.**
///
/// У `const` тот же инициализатор и то же время вычисления; пропустив её, правило
/// получило бы дыру ровно в том месте, где автор пишет "значение".
#[test]
fn const_initializer_is_judged_too() {
    let got = codes(&format!(
        "in sensor: bit;\n\
         const SNAPSHOT: u8 := sensor;\n\
         {TAIL}"
    ));
    assert_eq!(got, vec!["SE-113"]);
}

/// **Обход спускается во вложенные формы.**
///
/// Проверка, ловящая лишь плоское `:= порт`, молчала бы на арифметике и приведении - то
/// есть на большинстве настоящих записей.
#[test]
fn nested_forms_are_found() {
    for init in [
        "(sensor)",
        "sensor + 1",
        "1 + sensor",
        "sensor as u8",
        "~sensor",
        "-sensor",
        "sensor ? 1 : 2",
    ] {
        let got = codes(&format!(
            "in sensor: bit;\n\
             var mirror: u8 := {init};\n\
             {TAIL}"
        ));
        assert!(
            got.contains(&"SE-113".to_string()),
            "форма '{init}' обязана быть поймана, получено: {got:?}"
        );
    }
}

/// **Выходной порт судится своим правилом, а не этим.**
///
/// Чтение `out`-порта запрещено везде (`SE-027`) и приходит раньше; подменять его на
/// `SE-113` значило бы говорить о времени там, где дело в направлении.
#[test]
fn output_port_keeps_its_own_diagnostic() {
    let got = codes(&format!(
        "out lamp: bit;\n\
         var mirror: u8 := lamp;\n\
         {TAIL}"
    ));
    assert!(
        got.contains(&"SE-027".to_string()),
        "направление судит SE-027, получено: {got:?}"
    );
}
