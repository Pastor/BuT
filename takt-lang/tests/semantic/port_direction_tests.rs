//! Направление порта проверяется во всех позициях.
//!
//! ## Что здесь ловится
//!
//! Правило "во входной порт не пишут, из выходного не читают" (`SE-026`/ `SE-027`)
//! существовало, но применялось только к условиям переходов. Тела блоков и функций не
//! обходились, поэтому нарушение уезжало в цели, где они расходились: `rust` отказывал,
//! `c`/`st`/симулятор молча исполняли, `sv` печатал присваивание входному порту модуля,
//! а `c-hal` брал индекс из чужой таблицы и писал **по адресу другого порта**.
//!
//! Проверки идут через `collect_compile_diagnostics` - тот же вход, что у CLI и LSP:
//! важно, что диагностика доезжает до пользователя, а не только вырабатывается внутри.

use takt_lang::collect_compile_diagnostics;

/// Коды диагностик уровня ошибки для исходника.
fn error_codes(source: &str) -> Vec<String> {
    collect_compile_diagnostics("probe.takt", source, &[], false)
        .into_iter()
        .filter(|d| matches!(d.level, takt_lang::diagnostics::Level::Error))
        .filter_map(|d| d.code)
        .collect()
}

#[test]
fn write_to_input_port_in_block_body_is_rejected() {
    let codes = error_codes(
        "in A: u8 at 0x100;\nvar t: u8 := 0;\n\
         start S { always { A := 5; } ref S: t = 9; }\n",
    );
    assert!(
        codes.contains(&"SE-026".to_string()),
        "запись во входной порт обязана отвергаться: {codes:?}"
    );
}

#[test]
fn read_from_output_port_in_block_body_is_rejected() {
    let codes = error_codes(
        "out B: u8 at 0x104;\nvar t: u8 := 0;\n\
         start S { always { t := B; } ref S: t = 9; }\n",
    );
    assert!(
        codes.contains(&"SE-027".to_string()),
        "чтение выходного порта обязано отвергаться: {codes:?}"
    );
}

#[test]
fn writing_to_output_port_stays_legal() {
    // Тест направления правки: исключение для левой части присваивания обязано
    // покрывать обе формы цели записи - иначе фича сломала бы каждый пример с выходным
    // портом.
    let codes = error_codes(
        "out B: u8 at 0x104;\nout L: bit at 0x108:0;\nvar t: u8 := 0;\n\
         start S { always { B := 1; L := 1; L.0 := 1; } ref S: t = 9; }\n",
    );
    assert!(
        codes.is_empty(),
        "запись в выходной порт — законная форма: {codes:?}"
    );
}

#[test]
fn reading_input_port_stays_legal() {
    let codes = error_codes(
        "in A: u8 at 0x100;\nvar t: u8 := 0;\n\
         start S { always { t := A; } ref S: t = 9; }\n",
    );
    assert!(codes.is_empty(), "чтение входного порта законно: {codes:?}");
}

#[test]
fn inout_port_is_read_and_written_freely() {
    let codes = error_codes(
        "inout C: u8 at 0x108;\nvar t: u8 := 0;\n\
         start S { always { C := C + 1; t := C; } ref S: t = 9; }\n",
    );
    assert!(
        codes.is_empty(),
        "двунаправленный порт свободен в обе стороны: {codes:?}"
    );
}

#[test]
fn violation_inside_nested_statement_is_caught() {
    // Вложенность - отдельная позиция обхода: `if` внутри `loop` внутри блока.
    let codes = error_codes(
        "in A: u8 at 0x100;\nvar t: u8 := 0;\n\
         start S { always { if t < 3 { loop t < 2 { A := 7; } } } ref S: t = 9; }\n",
    );
    assert!(
        codes.contains(&"SE-026".to_string()),
        "нарушение во вложенном операторе обязано ловиться: {codes:?}"
    );
}

#[test]
fn violation_inside_function_body_is_caught() {
    let codes = error_codes(
        "out B: u8 at 0x104;\nvar t: u8 := 0;\n\
         fn peek() -> u8 { return B; }\n\
         start S { always { t := peek(); } ref S: t = 9; }\n",
    );
    assert!(
        codes.contains(&"SE-027".to_string()),
        "нарушение в теле функции обязано ловиться: {codes:?}"
    );
}

#[test]
fn several_violations_are_reported_together() {
    // Накопление: пользователь видит все нарушения, а не первое.
    let codes = error_codes(
        "in A: u8 at 0x100;\nout B: u8 := 0x104;\nvar t: u8 := 0;\n\
         start S { always { A := 5; t := B; } ref S: t = 9; }\n",
    );
    assert!(
        codes.contains(&"SE-026".to_string()) && codes.contains(&"SE-027".to_string()),
        "обе диагностики обязаны быть выданы за один прогон: {codes:?}"
    );
}
