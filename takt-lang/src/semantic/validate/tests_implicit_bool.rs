//! Тесты проверки неявной булевости условий (`SE-037`).
//!
//! Проверяется поведение, а не внутренний предикат: тесты идут через
//! `check_implicit_bool_conditions` и судят вердикт на исходнике.

use super::*;
use crate::parse;
use crate::semantic::tree::construct_model;

fn build_rc(src: &str) -> Rc<RefCell<ModelNode>> {
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    construct_model(&ast, None, &[]).expect("ошибка семантики")
}

// -- Юнит-тесты check_implicit_bool_conditions ----------------------------

/// Безусловный переход (`ref Next;`) - нет предупреждений.
#[test]
fn unconditional_ref_no_warning() {
    let model = build_rc("start S { ref T; } state T;");
    let warnings = check_implicit_bool_conditions(&model);
    assert!(
        warnings.is_empty(),
        "безусловный переход не должен давать предупреждений"
    );
}

/// Булев литерал в условии (`ref Next: true;`) - нет предупреждений.
#[test]
fn bool_literal_cond_no_warning() {
    let model = build_rc("start S { ref T: true; } state T;");
    let warnings = check_implicit_bool_conditions(&model);
    assert!(
        warnings.is_empty(),
        "булев литерал не должен давать предупреждений"
    );
}

/// Переменная типа `bool` в условии - нет предупреждений.
#[test]
fn bool_var_cond_no_warning() {
    let model = build_rc("var flag: bool := false; start S { ref T: flag; } state T;");
    let warnings = check_implicit_bool_conditions(&model);
    assert!(
        warnings.is_empty(),
        "переменная bool не должна давать предупреждений"
    );
}

/// Переменная типа `bit` (один бит) в условии - нет предупреждений.
#[test]
fn bit_var_cond_no_warning() {
    let model = build_rc("var flag: bit := 0; start S { ref T: flag; } state T;");
    let warnings = check_implicit_bool_conditions(&model);
    assert!(
        warnings.is_empty(),
        "переменная bit не должна давать предупреждений"
    );
}

/// Явное сравнение `!= 0` - нет предупреждений.
#[test]
fn explicit_ne_comparison_no_warning() {
    let model = build_rc("var timer: [bit;8] := 0; start S { ref T: timer != 0; } state T;");
    let warnings = check_implicit_bool_conditions(&model);
    assert!(
        warnings.is_empty(),
        "явное != не должно давать предупреждений"
    );
}

/// Явное сравнение `= 100` - нет предупреждений.
#[test]
fn explicit_eq_comparison_no_warning() {
    let model = build_rc("var timer: [bit;8] := 0; start S { ref T: timer = 100; } state T;");
    let warnings = check_implicit_bool_conditions(&model);
    assert!(
        warnings.is_empty(),
        "явное = не должно давать предупреждений"
    );
}

/// Именованное условие в ref - нет предупреждений.
#[test]
fn named_cond_in_ref_no_warning() {
    let model = build_rc(
        "var timer: [bit;8] := 0; \
         cond Full = timer = 255; \
         start S { ref T: Full; } state T;",
    );
    let warnings = check_implicit_bool_conditions(&model);
    assert!(
        warnings.is_empty(),
        "именованное условие не должно давать предупреждений"
    );
}

/// Переменная числового типа `[bit;8]` без сравнения - предупреждение.
#[test]
fn array_var_cond_gives_warning() {
    let model = build_rc("var timer: [bit;8] := 0; start S { ref T: timer; } state T;");
    let warnings = check_implicit_bool_conditions(&model);
    assert_eq!(
        warnings.len(),
        1,
        "переменная [bit;8] должна давать предупреждение"
    );
    assert!(
        warnings[0].message.contains("timer"),
        "сообщение должно упоминать 'timer'"
    );
}

/// Числовой литерал в условии - предупреждение.
#[test]
fn number_literal_cond_gives_warning() {
    let model = build_rc("start S { ref T: 5; } state T;");
    let warnings = check_implicit_bool_conditions(&model);
    assert_eq!(
        warnings.len(),
        1,
        "числовой литерал должен давать предупреждение"
    );
    assert!(
        warnings[0].message.contains('5'),
        "сообщение должно упоминать значение 5"
    );
}

/// Предупреждение содержит имя целевого состояния.
#[test]
fn warning_message_contains_target_state() {
    let model = build_rc("var x: [bit;8] := 0; start S { ref MyTarget: x; } state MyTarget;");
    let warnings = check_implicit_bool_conditions(&model);
    assert_eq!(warnings.len(), 1);
    assert!(
        warnings[0].message.contains("MyTarget"),
        "сообщение должно упоминать состояние-цель: {}",
        warnings[0].message
    );
}

/// Несколько переходов: один числовой, один булев - одно предупреждение.
#[test]
fn mixed_refs_one_warning() {
    let model = build_rc(
        "var timer: [bit;8] := 0; var flag: bool := false; \
         start S { ref T: timer; ref U: flag; } state T; state U;",
    );
    let warnings = check_implicit_bool_conditions(&model);
    assert_eq!(warnings.len(), 1, "должно быть ровно одно предупреждение");
}

/// Два числовых условия - два предупреждения.
#[test]
fn two_numeric_refs_two_warnings() {
    let model = build_rc(
        "var a: [bit;8] := 0; var b: [bit;8] := 0; \
         start S { ref T: a; ref U: b; } state T; state U;",
    );
    let warnings = check_implicit_bool_conditions(&model);
    assert_eq!(
        warnings.len(),
        2,
        "два числовых условия — два предупреждения"
    );
}

/// Вложенная модель с числовым условием - предупреждение упоминает имя модели.
#[test]
fn nested_model_implicit_bool_gives_warning() {
    let model = build_rc("model M { var timer: [bit;8] := 0; start S { ref T: timer; } state T; }");
    let warnings = check_implicit_bool_conditions(&model);
    assert_eq!(
        warnings.len(),
        1,
        "вложенная модель должна давать предупреждение"
    );
    assert!(
        warnings[0].message.contains('M'),
        "сообщение должно упоминать 'M'"
    );
}

/// Модель без состояний - нет предупреждений.
#[test]
fn model_without_states_no_warnings() {
    let model = build_rc("var timer: [bit;8] := 0;");
    let warnings = check_implicit_bool_conditions(&model);
    assert!(
        warnings.is_empty(),
        "модель без состояний не должна давать предупреждений"
    );
}

/// Предупреждение Се11 имеет уровень Warning.
#[test]
fn warning_has_correct_level() {
    use crate::diagnostics::Level;
    let model = build_rc("start S { ref T: 5; } state T;");
    let warnings = check_implicit_bool_conditions(&model);
    assert_eq!(warnings.len(), 1);
    assert_eq!(
        warnings[0].level,
        Level::Warning,
        "уровень должен быть Warning"
    );
}

// -- Таблица правил булевости ---------------------------------
//
// Правило булевости одно, и проверяется оно по вердикту на исходнике, а не по
// внутренней функции: таблица покрывает случаи так, как их видит автор программы.

/// Число предупреждений `SE-037` на исходнике.
fn warnings_for(src: &str) -> usize {
    check_implicit_bool_conditions(&build_rc(src)).len()
}

/// Каждое правило булевости - на исходнике: булево молчит, числовое предупреждает.
#[test]
fn boolean_rules_table() {
    // (исходник условия, ожидается ли предупреждение, чем случай интересен)
    let cases: &[(&str, bool, &str)] = &[
        (
            "var f: bool := false; start S { ref T: f; } state T;",
            false,
            "переменная bool",
        ),
        (
            "var b: bit := 0; start S { ref T: b; } state T;",
            false,
            "переменная bit",
        ),
        (
            "var t: [bit;8] := 0; start S { ref T: t != 0; } state T;",
            false,
            "сравнение",
        ),
        (
            "var t: [bit;8] := 0; start S { ref T: !(t = 0); } state T;",
            false,
            "отрицание",
        ),
        (
            "var t: [bit;8] := 0; start S { ref T: (t = 0); } state T;",
            false,
            "скобки прозрачны",
        ),
        (
            "var t: [bit;8] := 0; cond Full = t = 255; start S { ref T: Full; } state T;",
            false,
            "именованное условие",
        ),
        (
            "in a: bit; in b: bit; start S { ref T: a & b; } state T;",
            false,
            "`&` над булевыми",
        ),
        (
            "in a: bit; in b: bit; start S { ref T: a | b; } state T;",
            false,
            "`|` над булевыми",
        ),
        (
            "var f: [bit;8] := 0; start S { ref T: f.3; } state T;",
            false,
            "доступ к одному биту",
        ),
        ("start S { ref T: after 5s; } state T;", false, "выдержка"),
        // -- числовые: предупреждение обязано быть -------------------------
        (
            "var t: [bit;8] := 0; start S { ref T: t; } state T;",
            true,
            "многобитная переменная",
        ),
        ("start S { ref T: 1; } state T;", true, "числовой литерал"),
        (
            "var t: [bit;8] := 0; start S { ref T: t + 1; } state T;",
            true,
            "арифметика",
        ),
        (
            "in a: bit; var t: [bit;8] := 0; start S { ref T: a & t; } state T;",
            true,
            "числовой операнд `&`",
        ),
    ];

    for (src, expect, what) in cases {
        let got = warnings_for(src) > 0;
        assert_eq!(
            got,
            *expect,
            "{what}: ожидалось {}, получено {} — исходник: {src}",
            if *expect {
                "предупреждение"
            } else {
                "молчание"
            },
            if got {
                "предупреждение"
            } else {
                "молчание"
            },
        );
    }
}

/// Неразрешённое условие предупреждения не даёт.
///
/// Ветвь недостижима через обоих потребителей: неразрешённое условие - это ошибка
/// `SE-025`, а предупреждения считаются только при отсутствии ошибок. Тест держит
/// **решение** молчать: судить о булевости того, чего семантика не поняла, значит
/// гадать.
#[test]
fn unresolved_condition_is_silent() {
    // `S(Модель) = Состояние` - единственная форма, ради которой держали второе
    // правило.
    let src = "model Ping { start Go { ref End; } state End; } \
               model Main2 { start Wait { ref Done: S(Ping) = End; } state Done; } \
               start Root = Ping + Main2;";
    assert_eq!(warnings_for(src), 0, "сравнение состояний булево");
}
