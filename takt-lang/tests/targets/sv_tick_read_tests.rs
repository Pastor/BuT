//! Тест семантики такта цели `sv`: чтение внутри такта видит запись того же такта
//! (переехал из юнит-тестов `generator/sv/mod.rs`).
//!
//! ## Что здесь ловится
//!
//! `v := 1; w := v;` обязано дать `w = 1` - так в симуляторе и в цели `c` (там
//! `write(V, 1)`, затем `read(V)` возвращает только что записанное). Значит чтение
//! внутри такта обязано идти из рабочей копии `_next`, а не из регистра: регистр держит
//! значение **предыдущего** такта, и `w` получил бы `0`.
//!
//! Дефект такого рода **не ловится ни одним проверкой цели**: `w_next = v;` даёт валидный
//! синтезируемый модуль, который просто считает не то.

use std::fs;

/// Компилирует исходник целью `sv` и возвращает текст модуля.
fn sv_module(source: &str) -> String {
    let dir = tempfile::tempdir().expect("временный каталог");
    let out = dir.path().to_str().expect("путь");
    takt_lang::compile_to_sv(
        "tick_read.takt",
        source,
        out,
        &[],
        &takt_lang::GenerateOptions::default(),
    )
    .expect("цель sv обязана компилироваться");

    let entry = fs::read_dir(out)
        .expect("каталог вывода")
        .filter_map(Result::ok)
        .find(|e| e.path().extension().is_some_and(|ext| ext == "sv"))
        .expect("файл .sv");
    fs::read_to_string(entry.path()).expect("чтение .sv")
}

#[test]
fn read_inside_tick_sees_write_made_in_same_tick() {
    let sv = sv_module(
        "var v: bit := 0; out w: bit;\n\
         start S { always { v := 1; w := v; } }\n",
    );
    assert!(
        sv.contains("w_next = tick_read_v_next;"),
        "чтение внутри такта обязано видеть запись этого же такта \
         (рабочую копию `_next`), иначе значение будет от предыдущего такта:\n{sv}"
    );
}

#[test]
fn working_copy_is_seeded_from_the_register() {
    // Обратная сторона того же контракта: рабочая копия начинает такт со значения
    // регистра, иначе непризнанное присваивание дало бы защёлку.
    let sv = sv_module(
        "var v: bit := 0; out w: bit;\n\
         start S { always { v := 1; w := v; } }\n",
    );
    assert!(
        sv.contains("tick_read_v_next = tick_read_v;"),
        "рабочая копия обязана начинаться со значения регистра:\n{sv}"
    );
}
