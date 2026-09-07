//! Тест против возврата рекурсии по числу состояний.
//!
//! Модель-цепочка `S0 -> S1 -> ... -> S ` даёт обход глубиной ровно N. Пока обходы
//! были рекурсивными, это исчерпывало стек - и отказ был **без диагностики**:
//! `SIGABRT`, без строки, позиции и кода возврата. Замеры до правки (основной поток, 8
//! Мб): генерация падала на N ≈ 2800 (debug), проверка свойств - на N ≈ 16000.
//!
//! **Тесты жёстче продуктового сценария, и это намеренно.** Поток теста несёт 2 мб
//! стека против 8 мб у основного, поэтому рекурсия ломалась бы здесь уже на сотнях
//! состояний. Возврат рекурсии в любой обход завалит эти тесты **раньше**, чем
//! пользователь увидит `SIGABRT`.
//!
//! Проверка называет **слой** (генерация / верификация), а не "обходы вообще": потолок
//! жил не там, где его искали: обход верификации назывался первым, а самый низкий
//! потолок был в построении карты - общем для всех целей).

use std::rc::Rc;

/// Исходник модели-цепочки из `n` состояний со свойством `F S `.
fn chain_source(n: usize) -> String {
    let mut src = String::new();
    src.push_str(&format!(": [LTL] F S{};\n", n - 1));
    src.push_str("start S0 { ref S1; }\n");
    for i in 1..n - 1 {
        src.push_str(&format!("state S{i} {{ ref S{}; }}\n", i + 1));
    }
    src.push_str(&format!("state S{};\n", n - 1));
    src
}

/// Глубина, заведомо ломающая рекурсию на 2 мб стека тестового потока.
const DEEP: usize = 5000;

/// Построение карты (общее для всех целей) не рекурсирует по состояниям.
///
/// Это и был самый низкий потолок продукта: карту строит каждый генератор (`c_map`,
/// `puml_map`, `rust_map`, `st_map`, `sv_map`), поэтому падали все пять целей - ещё до
/// печати текста.
#[test]
fn codegen_handles_deep_state_chain() {
    let src = chain_source(DEEP);
    let out = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join("takt_deep_chain.c");
    // Каталог процесса создаётся здесь: файл пишет не тест, а инструмент.
    let _ = std::fs::create_dir_all(out.parent().expect("каталог процесса"));
    takt_lang::compile_to_c(
        "deep.takt",
        &src,
        out.to_str().expect("путь"),
        &[],
        &takt_lang::GenerateOptions::default(),
    )
    .expect("цепочка из 5000 состояний обязана компилироваться");
    let _ = std::fs::remove_file(&out);
}

/// PlantUML - вторая цель поверх той же карты: контроль от правки одной цели.
#[test]
fn plantuml_handles_deep_state_chain() {
    let src = chain_source(DEEP);
    let out = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join("takt_deep_chain.puml");
    // Каталог процесса создаётся здесь: файл пишет не тест, а инструмент.
    let _ = std::fs::create_dir_all(out.parent().expect("каталог процесса"));
    takt_lang::compile_to_plantuml("deep.takt", &src, out.to_str().expect("путь"), &[])
        .expect("цепочка из 5000 состояний обязана давать диаграмму");
    let _ = std::fs::remove_file(&out);
}

/// Верификация (nested-DFS) не рекурсирует по состояниям произведения.
///
/// Проверяется **вердикт**, а не факт возврата: `F S4999` на цепочке достижимо и
/// неизбежно. Иначе тест прошёл бы и на сломанной семантике.
#[test]
fn verification_handles_deep_state_chain() {
    use takt_lang::verification::verify::Verdict;

    let src = chain_source(DEEP);
    let (ast, _) = takt_lang::parse(&src, 0).expect("разбор");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
    let results = takt_lang::verify_all(Rc::clone(&model));
    assert_eq!(results.len(), 1, "в цепочке одна формула");
    assert_eq!(
        results[0].verdict,
        Verdict::Holds,
        "конец цепочки достижим и неизбежен: F S{} обязано держаться",
        DEEP - 1
    );
}
