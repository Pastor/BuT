//! Разрешение тел вложенных операторов -.
//!
//! ## Что здесь ловится
//!
//! Ошибка разрешения во **вложенном** теле (`if`/`else`, `while`/`loop`, `for`)
//! глоталась: оператор оставался `StatementNode::Unresolved`, цель `c` печатала
//! его **пустотой**, симулятор - пропускал. То есть терялась не диагностика, а
//! **сам оператор**: `always { if x > 0 { x := неизвестное; } }` давал
//! `if (model->x > 0) { }` и "Скомпилировано".
//!
//! Поэтому тесты идут парами: контрпример обязан дать диагностику, а соседний пример -
//! по-прежнему компилироваться **вместе с телом** (иначе "починка" могла бы выкидывать
//! тела уже громко).
//!
//! Теста стоят на **каждой** из пяти точек глотания: вернуть тишину в одну из них -
//! правка в одну строку, и без пофиточного теста она пройдёт незаметно.

use takt_lang::collect_compile_diagnostics;

/// Читает исправлениетуру каталога `tests/data/nested0155/`.
fn fixture(name: &str) -> (String, String) {
    let path = format!("tests/data/nested0155/{name}");
    let source =
        std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("не прочитать {path}: {e}"));
    (path, source)
}

/// Коды диагностик, выданных на исправлениетуре.
fn codes(name: &str) -> Vec<String> {
    let (path, source) = fixture(name);
    collect_compile_diagnostics(&path, &source, &[], false)
        .iter()
        .map(|d| d.code.clone().unwrap_or_default())
        .collect()
}

/// Проверяет, что исправлениетура даёт `SE-003` с указанием потерянного имени.
fn expect_unknown_identifier(name: &str) {
    let (path, source) = fixture(name);
    let diagnostics = collect_compile_diagnostics(&path, &source, &[], false);
    assert!(
        !diagnostics.is_empty(),
        "{name}: ожидалась диагностика, получена тишина — оператор молча выброшен"
    );
    let found = diagnostics
        .iter()
        .any(|d| d.code.as_deref() == Some("SE-003") && d.message.contains("unknown_var_zzz"));
    assert!(
        found,
        "{name}: ожидался SE-003 про 'unknown_var_zzz', получено: {:#?}",
        diagnostics
            .iter()
            .map(|d| (&d.code, &d.message))
            .collect::<Vec<_>>()
    );
}

// -- Пять точек глотания -------------------------------------------------------

#[test]
fn unknown_identifier_in_if_then_is_diagnosed() {
    expect_unknown_identifier("if_then_unknown.takt");
}

#[test]
fn unknown_identifier_in_if_else_is_diagnosed() {
    expect_unknown_identifier("if_else_unknown.takt");
}

#[test]
fn unknown_identifier_in_while_body_is_diagnosed() {
    expect_unknown_identifier("while_body_unknown.takt");
}

#[test]
fn unknown_identifier_in_loop_body_is_diagnosed() {
    // `loop` и `while` - синонимы (одна ветка АСД), но точка глотания одна на обе: тест
    // исправлениеирует, что синоним не имеет своей лазейки.
    expect_unknown_identifier("loop_body_unknown.takt");
}

#[test]
fn unknown_identifier_in_for_init_is_diagnosed() {
    expect_unknown_identifier("for_init_unknown.takt");
}

#[test]
fn unknown_identifier_in_for_body_is_diagnosed() {
    expect_unknown_identifier("for_body_unknown.takt");
}

// -- Вложенность в других контекстах -------------------------------------------

#[test]
fn unknown_identifier_nested_in_function_body_is_diagnosed() {
    // Тело функции верхним уровнем проверялось и раньше; тишину давала именно
    // вложенность в `if` внутри функции.
    expect_unknown_identifier("fn_nested_unknown.takt");
}

#[test]
fn unknown_identifier_nested_in_match_arm_is_diagnosed() {
    // Само плечо `match` разрешалось через `?` и раньше, но `if` внутри плеча уходил в
    // ту же дыру.
    expect_unknown_identifier("match_arm_nested_unknown.takt");
}

// -- Шестая точка: inline-`Guard` в блоке --------------------------------------

#[test]
fn resolution_error_in_inline_guard_is_diagnosed() {
    // Шестая точка глотания - `filter_map(|c| resolve_condition(c, ...).ok())`:
    // формула, чьё условие не разрешилось, молча выпадала из списка, и тест
    // переставал тестыть, не сказав ни слова.
    //
    // Проверяется ошибка **разрешения**, а не неизвестное имя: для неизвестного имени
    // `resolve_condition` возвращает `Ok(Unresolved(...))` - это инвариант рёбер `ref`, -
    // и молчание там имеет другую причину (`validate` не обходит формулы). Поэтому
    // берём условие глубже предела: оно даёт настоящую `Err` и раньше выпадало из
    // списка молча.
    let depth = 60;
    let cond = format!("{}x{} > 0", "(".repeat(depth), ")".repeat(depth));
    let source = format!(
        "model Probe {{\n    var x: u8 := 0;\n    always {{ : [Guard] {cond}; }}\n    start A {{ ref A: x = 200; }}\n}}\nstart Entry = Probe;\n"
    );
    let diagnostics = collect_compile_diagnostics("inline_guard_deep.takt", &source, &[], false);
    assert!(
        diagnostics
            .iter()
            .any(|d| d.code.as_deref() == Some("SE-062")),
        "ошибка разрешения условия inline-Guard обязана доходить до пользователя, получено: {:#?}",
        diagnostics
            .iter()
            .map(|d| (&d.code, &d.message))
            .collect::<Vec<_>>()
    );
}

// -- Тест направления --------------------------------------------------------

#[test]
fn valid_nested_bodies_are_still_accepted() {
    let diagnostics = codes("nested_valid.takt");
    assert!(
        diagnostics.is_empty(),
        "корректные вложенные тела обязаны компилироваться без диагностик: {diagnostics:?}"
    );
}

#[test]
fn valid_nested_body_is_emitted_into_generated_c() {
    // Ключевой тест: диагностика - половина дела. Вторая половина в том, что тело
    // вложенного оператора **доезжает до порождённого кода**.
    let (_, source) = fixture("nested_valid.takt");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join("takt-0155-nested-valid");
    let _ = std::fs::remove_dir_all(&dir);
    takt_lang::compile_to_c(
        &fixture("nested_valid.takt").0,
        &source,
        dir.to_str().expect("путь"),
        &[],
        &takt_lang::GenerateOptions::default(),
    )
    .expect("корректная модель обязана транслироваться");

    let mut body = String::new();
    for entry in std::fs::read_dir(&dir).expect("каталог вывода") {
        let path = entry.expect("запись каталога").path();
        if path.extension().and_then(|e| e.to_str()) == Some("c") {
            body.push_str(&std::fs::read_to_string(&path).expect("файл .c"));
        }
    }
    let _ = std::fs::remove_dir_all(&dir);

    assert!(
        body.contains("model->x = 5"),
        "тело ветви `then` обязано быть в порождённом C, получено:\n{body}"
    );
    assert!(
        body.contains("model->x = 7"),
        "тело ветви `else` обязано быть в порождённом C, получено:\n{body}"
    );
}

// -- Доставка в редактор ------------------------------------------

#[cfg(feature = "lsp")]
#[test]
fn nested_diagnostic_reaches_lsp() {
    // требует фактической проверки, а не рассуждения "путь общий".
    let (path, source) = fixture("if_then_unknown.takt");
    let diagnostics = takt_lang::lsp::collect_diagnostics_at(&path, &source, &[]);
    assert!(
        diagnostics.iter().any(|d| {
            matches!(&d.code, Some(lsp_types::NumberOrString::String(code)) if code == "SE-003")
        }),
        "диагностика вложенного тела обязана доходить до LSP, получено: {:#?}",
        diagnostics
            .iter()
            .map(|d| (&d.code, &d.message))
            .collect::<Vec<_>>()
    );
}
