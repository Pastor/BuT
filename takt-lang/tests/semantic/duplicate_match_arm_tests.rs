//! Повторяющийся образец `match` - `SE-131`,.
//!
//! # Что здесь тестытся
//!
//! `match` берёт **первое** совпадение, поэтому ветвь, чей образец уже стоял выше, не
//! сработает никогда.
//!
//! Проверка недетерминизма (`SE-037`) класс не видит: она о рёбрах состояния, а не о
//! ветвях `match`.
//!
//! Корпус класса не содержит - тест исправлениетурный.

use takt_lang::semantic::warnings::collect_model_warnings;

fn warnings_of(src: &str) -> Vec<takt_lang::diagnostics::Diagnostic> {
    let (ast, _) = takt_lang::parse(src, 0).expect("разбор");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("дерево");
    collect_model_warnings(&ast, &model)
}

/// Только коды `SE-131` - прочие предупреждения к предмету не относятся.
fn duplicate_codes(src: &str) -> Vec<takt_lang::diagnostics::Diagnostic> {
    warnings_of(src)
        .into_iter()
        .filter(|d| d.code.as_deref() == Some("SE-131"))
        .collect()
}

const TWO_SAME: &str = "var op: u8 := 0; var acc: u8 := 0;\n\
                        start Run {\n\
                        \x20   always {\n\
                        \x20       op := op + 1;\n\
                        \x20       match op { 1 => { acc := acc + 1; } 1 => { acc := acc + 10; } }\n\
                        \x20   }\n\
                        \x20   ref Run;\n\
                        }\n";

/// **T1.** Повторный образец получает предупреждение - с позицией ветви.
#[test]
fn duplicate_pattern_is_reported() {
    let found = duplicate_codes(TWO_SAME);
    assert_eq!(found.len(), 1, "ожидалось одно предупреждение: {found:?}");
    assert!(
        found[0].message.contains("ПЕРВОЕ совпадение"),
        "сообщение обязано объяснять правило:\n{}",
        found[0].message
    );
    assert!(
        !matches!(found[0].loc, takt_lang::diagnostics::Location::Builtin),
        "диагностика обязана нести позицию ветви (класс 0471): {:?}",
        found[0].loc
    );
}

/// **T2.** Высказываются все недостижимые ветви, а не первая.
#[test]
fn every_unreachable_arm_is_reported() {
    let src = "var op: u8 := 0; var acc: u8 := 0;\n\
               start Run {\n\
               \x20   always {\n\
               \x20       op := op + 1;\n\
               \x20       match op { 1 => { acc := 1; } 1 => { acc := 2; } 1 => { acc := 3; } }\n\
               \x20   }\n\
               \x20   ref Run;\n\
               }\n";
    assert_eq!(duplicate_codes(src).len(), 2, "недостижимы вторая и третья");
}

/// **T3.** Вторая ветвь `_` - тот же класс.
#[test]
fn second_default_arm_is_reported() {
    let src = "var op: u8 := 0; var acc: u8 := 0;\n\
               start Run {\n\
               \x20   always {\n\
               \x20       op := op + 1;\n\
               \x20       match op { 1 => { acc := 1; } _ => { acc := 2; } _ => { acc := 3; } }\n\
               \x20   }\n\
               \x20   ref Run;\n\
               }\n";
    assert_eq!(duplicate_codes(src).len(), 1, "недостижима вторая `_`");
}

/// **T4. Контроль:** различные образцы предупреждения не дают.
///
/// Без этой проверки правило нельзя отличить от "предупреждать на каждом `match`":
/// ложное срабатывание здесь дороже пропуска - оно шумит на корректных моделях.
#[test]
fn distinct_patterns_are_silent() {
    let src = "var op: u8 := 0; var acc: u8 := 0;\n\
               start Run {\n\
               \x20   always {\n\
               \x20       op := op + 1;\n\
               \x20       match op { 1 => { acc := 1; } 2 => { acc := 2; } _ => { acc := 3; } }\n\
               \x20   }\n\
               \x20   ref Run;\n\
               }\n";
    assert!(duplicate_codes(src).is_empty(), "образцы различны");
}

/// **T5.** Ветвь в теле функции и во вложенной модели тоже обходится.
#[test]
fn nested_bodies_are_walked() {
    let src = "model Inner {\n\
               \x20   var op: u8 := 0; var acc: u8 := 0;\n\
               \x20   fn pick(v: u8) -> u8 {\n\
               \x20       match v { 1 => { return 1; } 1 => { return 2; } }\n\
               \x20       return 0;\n\
               \x20   }\n\
               \x20   start Run { always { acc := pick(op); } ref Run; }\n\
               }\n\
               start Top = Inner;\n";
    assert_eq!(
        duplicate_codes(src).len(),
        1,
        "тело функции вложенной модели обязано обходиться"
    );
}

/// **T6.** Исправлениетура корпуса даёт `SE-131` - код достижим проверками.
///
/// Без входа В корпусе код проверялся бы только строками в тестах, а проверки целей и
/// потактовые сверки его бы не видели: ровно это и объявляет реестр недостижимых кодов.
#[test]
fn corpus_fixture_reports_the_code() {
    let path = concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/data/dupmatch0514/duplicate_arm.takt"
    );
    let src = std::fs::read_to_string(path).expect("фикстура читается");
    let found = duplicate_codes(&src);
    assert_eq!(found.len(), 1, "ожидалось одно предупреждение: {found:?}");
}
