//! Перечисление без вариантов.
//!
//! ## Что здесь ловится
//!
//! Запись `enum E { }` отвергал **парсер**: правило требовало `CommaOne<EnumVariant>`.
//! То есть правило языка было свойством квантификатора в грамматике, а не решением, и
//! текст сообщения - внутренностью LR-разбора:
//!
//! ```text
//! SY-002: нераспознанный токен '}', ожидалось identifier, "X", "F", "G", "U",
//!         "R", "LTL", "Guard"
//! ```
//!
//! принял решение языка: **у перечисления обязан быть хотя бы один вариант**, и
//! высказывает его семантика - кодом `SE-105`, на **объявлении**.
//!
//! ## Что тесты обязаны различать
//!
//! **Пустоту** (`enum E { }` -> `SE-105`) и **мусор** внутри скобок
//! (`enum E { 42 }` -> `SY-002`): смена квантификатора не имеет права съесть
//! ветвь восстановления парсера.

use takt_lang::collect_compile_diagnostics;
use takt_lang::diagnostics::{Location, line_column};
use takt_lang::format::format_source;

fn diagnostics(source: &str) -> Vec<takt_lang::diagnostics::Diagnostic> {
    collect_compile_diagnostics("probe.takt", source, &[], false)
}

fn codes(source: &str) -> Vec<String> {
    diagnostics(source)
        .into_iter()
        .filter_map(|d| d.code)
        .collect()
}

const TAIL: &str = r#"
var n: u8 := 0;

start Run {
    always { n := n + 1; }
    ref Done: n >= 3;
}

state Done { }
"#;

/// R1: перечисление без вариантов даёт `SE-105`.
#[test]
fn empty_enum_is_se105() {
    let codes = codes(&format!("enum Mode {{ }}{TAIL}"));
    assert!(
        codes.iter().any(|c| c == "SE-105"),
        "пустое перечисление должно давать SE-105, получено: {codes:?}"
    );
    assert!(
        !codes.iter().any(|c| c == "SY-002"),
        "отказ обязан идти от семантики, а не от разбора: {codes:?}"
    );
}

/// R1: позиция - объявление перечисления (его имя), а не закрывающая скобка.
#[test]
fn se105_points_at_the_declaration() {
    let source = format!("enum Mode {{ }}{TAIL}");
    let d = diagnostics(&source)
        .into_iter()
        .find(|d| d.code.as_deref() == Some("SE-105"))
        .expect("ожидалась SE-105");
    let Location::Source(_, start, _) = d.loc else {
        panic!("позиция обязана быть в исходнике, получено: {:?}", d.loc);
    };
    assert_eq!(
        line_column(&source, start as usize),
        (1, 6),
        "SE-105 обязана указывать на имя перечисления"
    );
}

/// R2: текст называет имя, правило и способ исправить.
#[test]
fn se105_text_names_the_rule_and_the_way_out() {
    let d = diagnostics(&format!("enum Mode {{ }}{TAIL}"))
        .into_iter()
        .find(|d| d.code.as_deref() == Some("SE-105"))
        .expect("ожидалась SE-105");
    let text = d.message.clone();
    for part in ["Mode", "хотя бы один вариант", "удалите объявление"]
    {
        assert!(
            text.contains(part),
            "в тексте SE-105 ожидалась подстрока {part:?}, получено: {text}"
        );
    }
}

/// R3: накопление - каждое пустое перечисление высказывается само за себя, включая
/// объявленное во вложенной модели.
#[test]
fn every_empty_enum_speaks_for_itself() {
    let source = std::fs::read_to_string("tests/data/semantic/invalid/empty_enum_many.takt")
        .expect("фикстура накопления");
    let count = collect_compile_diagnostics("empty_enum_many.takt", &source, &[], false)
        .into_iter()
        .filter(|d| d.code.as_deref() == Some("SE-105"))
        .count();
    assert_eq!(
        count, 3,
        "ожидались три SE-105 (два перечисления модели и одно вложенной)"
    );
}

/// R3: при использовании пустого перечисления автор видит **обе** записи - `SE-105`
/// (причина) и `SE-043` (следствие), - и причина идёт первой.
///
/// Первенство обеспечивает **не** порядок проверок в `validate_model_all`, а
/// `diagnostics::normalize`: выдача сортируется по позиции в тексте, а объявление стоит
/// выше использования.
#[test]
fn cause_precedes_consequence() {
    let source = format!("enum Mode {{ }}\nvar m: Mode := 0;{TAIL}");
    let codes = codes(&source);
    let se105 = codes.iter().position(|c| c == "SE-105");
    let se043 = codes.iter().position(|c| c == "SE-043");
    assert!(
        se105.is_some() && se043.is_some(),
        "ожидались обе диагностики, получено: {codes:?}"
    );
    assert!(
        se105 < se043,
        "SE-105 (причина) обязана идти раньше SE-043 (следствие): {codes:?}"
    );
}

/// R4: мусор внутри скобок по-прежнему отвергается **разбором** - ветвь восстановления
/// жива, снятие квантификатора её не съело.
#[test]
fn garbage_inside_braces_is_still_a_parse_error() {
    let codes = codes(&format!("enum Mode {{ 42 }}{TAIL}"));
    assert!(
        codes.iter().any(|c| c == "SY-002"),
        "мусор в теле перечисления должен давать SY-002, получено: {codes:?}"
    );
    assert!(
        !codes.iter().any(|c| c == "SE-105"),
        "до семантики такой вход доходить не должен: {codes:?}"
    );
}

/// R5: форматтер печатает пустую форму и делает это **идемпотентно** - запись доезжает
/// до него, потому что отказ переехал из разбора в семантику, а `fmt` семантику не
/// зовёт.
#[test]
fn formatter_round_trip_is_stable() {
    let printed =
        format_source("enum Mode {   }\n\nvar n: u8 := 0;\n\nstart Run {\n    ref Run;\n}\n")
            .expect("пустое перечисление обязано форматироваться");
    assert!(
        printed.contains("enum Mode {}"),
        "ожидалась однострочная форма, получено:\n{printed}"
    );
    let again = format_source(&printed).expect("повторная печать");
    assert_eq!(printed, again, "печать обязана быть идемпотентной");
}

/// R6: диагностика доезжает до **редактора** - тем же входом `validate_model_all`,
/// которым пользуется командная строка.
#[cfg(feature = "lsp")]
#[test]
fn editor_shows_se105() {
    let diagnostics = takt_lang::lsp::collect_diagnostics(&format!("enum Mode {{ }}{TAIL}"));
    let found: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.code == Some(lsp_types::NumberOrString::String("SE-105".to_string())))
        .collect();
    assert_eq!(found.len(), 1, "редактор обязан показать: {diagnostics:?}");
}

/// R1 (контр-пример): перечисление с единственным вариантом законно, и `SE-105` на нём
/// не возникает.
#[test]
fn single_variant_enum_is_valid() {
    let codes = codes(&format!("enum Mode {{ Idle }}{TAIL}"));
    assert!(
        codes.is_empty(),
        "перечисление с одним вариантом обязано быть валидным, получено: {codes:?}"
    );
}
