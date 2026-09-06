//! Ребро после безусловного недостижимо - `SE-116`,.
//!
//! # Что здесь тестытся
//!
//! Переходы состояния проверяются по порядку записи, и первое **безусловное** ребро
//! завершает выбор: всё, что записано после него, не сработает никогда. Правило
//! исполняют все четыре цели и эталон, но автору о нём не говорил **никто**.
//!
//!
//! **Ce14 (`SE-037`) класс не покрывает:** она ищет **несколько** безусловных рёбер, а
//! здесь безусловное одно - лишними оказываются условные за ним.
//!
//! **Корпус класс не покрывает:** записи "ребро после безусловного" в `examples/` нет
//! ни одной (проверено прогоном по всем файлам корпуса), - поэтому тест исправлениетурный.

use takt_lang::semantic::warnings::collect_model_warnings;

/// Предупреждения, которые увидит пользователь (та же точка, что у `taktc`).
fn warnings_of(src: &str) -> Vec<takt_lang::diagnostics::Diagnostic> {
    let (ast, _) = takt_lang::parse(src, 0).expect("разбор");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("дерево");
    collect_model_warnings(&ast, &model)
}

/// Только коды `SE-116` - прочие предупреждения к предмету не относятся.
fn unreachable_codes(src: &str) -> Vec<takt_lang::diagnostics::Diagnostic> {
    warnings_of(src)
        .into_iter()
        .filter(|d| d.code.as_deref() == Some("SE-116"))
        .collect()
}

/// **T1.** Ребро после безусловного получает предупреждение с названными
/// именами.
#[test]
fn edge_after_unconditional_is_reported() {
    let src = "var n: u8 := 0;\n\
               start Run {\n\
               \x20   always { n := n + 1; }\n\
               \x20   ref Done;\n\
               \x20   ref Late: n = 1;\n\
               }\n\
               state Done { always { n := 0; } }\n\
               state Late { always { n := 9; } }\n";
    let found = unreachable_codes(src);
    assert_eq!(found.len(), 1, "ожидалось одно предупреждение: {found:?}");
    let text = &found[0].message;
    assert!(
        text.contains("'Late'") && text.contains("'Done'"),
        "сообщение обязано называть оба ребра:\n{text}"
    );
}

/// **T2.** Высказываются все недостижимые рёбра, а не первое.
///
/// одна диагностика на элемент, все элементы высказываются.
#[test]
fn every_dead_edge_is_reported() {
    let src = "var n: u8 := 0;\n\
               start Run {\n\
               \x20   always { n := n + 1; }\n\
               \x20   ref Done;\n\
               \x20   ref Late: n = 1;\n\
               \x20   ref Later: n = 2;\n\
               }\n\
               state Done { always { n := 0; } }\n\
               state Late { always { n := 9; } }\n\
               state Later { always { n := 8; } }\n";
    assert_eq!(unreachable_codes(src).len(), 2);
}

/// **T3. Контроль: условное ребро перед безусловным законно и молчит.**
///
/// Это доминирующая идиома: сначала особые случаи, затем безусловный выход.
/// Предупреждение здесь было бы шумом на каждом втором состоянии корпуса.
#[test]
fn conditional_before_unconditional_is_silent() {
    let src = "var n: u8 := 0;\n\
               start Run {\n\
               \x20   always { n := n + 1; }\n\
               \x20   ref Late: n = 1;\n\
               \x20   ref Done;\n\
               }\n\
               state Done { always { n := 0; } }\n\
               state Late { always { n := 9; } }\n";
    assert!(
        unreachable_codes(src).is_empty(),
        "законная идиома не должна предупреждаться"
    );
}

/// **T4. Контроль: корпус чист.**
///
/// Записи "ребро после безусловного" в `examples/` нет ни одной - если проверка начнёт
/// срабатывать там, значит она ловит не тот класс.
#[test]
fn corpus_has_no_unreachable_edges() {
    let mut offenders = Vec::new();
    for entry in std::fs::read_dir("../examples").expect("каталог examples") {
        let path = entry.expect("запись каталога").path();
        if path.extension().and_then(|e| e.to_str()) != Some("takt") {
            continue;
        }
        let Ok(source) = std::fs::read_to_string(&path) else {
            continue;
        };
        let Ok((ast, _)) = takt_lang::parse(&source, 0) else {
            continue;
        };
        let dir = path
            .parent()
            .map(|p| p.to_string_lossy().into_owned())
            .unwrap_or_default();
        let Ok(model) = takt_lang::semantic::tree::construct_model(&ast, None, &[dir]) else {
            continue;
        };
        let found = collect_model_warnings(&ast, &model)
            .into_iter()
            .filter(|d| d.code.as_deref() == Some("SE-116"))
            .count();
        if found > 0 {
            offenders.push(format!("{}: {found}", path.display()));
        }
    }
    assert!(
        offenders.is_empty(),
        "корпус этого класса не содержит — срабатывания подозрительны:\n{offenders:#?}"
    );
}

/// **T5.** Ce14 остаётся при своём: два безусловных ребра - её случай, не наш.
#[test]
fn two_unconditional_edges_stay_with_ce14() {
    let src = "var n: u8 := 0;\n\
               start Run {\n\
               \x20   always { n := n + 1; }\n\
               \x20   ref Done;\n\
               \x20   ref Late;\n\
               }\n\
               state Done { always { n := 0; } }\n\
               state Late { always { n := 9; } }\n";
    let all = warnings_of(src);
    assert!(
        all.iter().any(|d| d.code.as_deref() == Some("SE-037")),
        "недетерминизм обязан остаться за Ce14: {all:?}"
    );
    assert_eq!(
        unreachable_codes(src).len(),
        1,
        "второе безусловное ребро тоже недостижимо — о нём говорим"
    );
}
