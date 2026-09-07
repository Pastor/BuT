//! Собственное ребро состояния-композиции: эталон == порождённый C.
//!
//! # Что проверяется
//!
//! Состояние может нести реализацию (`= A | B`) и **свои** рёбра:
//!
//! ```text
//! start Entry = Worker | Idler {
//!     ref Finish: flag = 3;
//! }
//! ```
//!
//! Правило языка задано: реализация тикается до проверки переходов, и переход берётся
//! по её завершении. Эталон исполняет это буквально - сначала `ref`-рёбра в порядке
//! объявления, затем `next`. Цели печатали **только** `next`, а рёбра теряли: в
//! `Finish` не шёл никто, а при ложном условии цель уходила в `END` там, где эталон
//! ждёт.
//!
//! # Что наблюдается
//!
//! `mark`: 0 - пока автомат в композиции, 7 - после перехода в `Finish`. Наблюдаемая
//! различает **исход**, а не факт компиляции.

use std::path::{Path, PathBuf};
use std::process::Command;

/// Тактов в трассе.
const TICKS: usize = 6;

/// Имя порождаемой единицы (оно же имя корневой модели: `CompEdge`).
const UNIT: &str = "comp_edge";

/// Условие ребра **истинно** в момент завершения композиции -> переход в `Finish`.
const SRC_FIRES: &str = "var flag: u8 := 0;\n\
    var mark: u8 := 0;\n\n\
    model Worker {\n\
    \x20   var n: u8 := 0;\n\n\
    \x20   start Busy {\n\
    \x20       always { n := n + 1; flag := n; }\n\
    \x20       ref Done: n = 3;\n\
    \x20   }\n\n\
    \x20   state Done;\n\
    }\n\n\
    model Idler {\n\
    \x20   var k: u8 := 0;\n\n\
    \x20   start Loop {\n\
    \x20       always { k := k + 1; }\n\
    \x20       ref Stop: k = 2;\n\
    \x20   }\n\n\
    \x20   state Stop;\n\
    }\n\n\
    start Entry = Worker | Idler {\n\
    \x20   ref Finish: flag = 3;\n\
    }\n\n\
    state Finish {\n\
    \x20   always { mark := 7; }\n\
    }\n";

/// Условие ребра **ложно** всегда: композиция завершилась, но перехода нет - автомат
/// обязан остаться в состоянии, а не уйти в `END`.
const SRC_WAITS: &str = "var flag: u8 := 0;\n\
    var mark: u8 := 0;\n\n\
    model Worker {\n\
    \x20   var n: u8 := 0;\n\n\
    \x20   start Busy {\n\
    \x20       always { n := n + 1; flag := n; }\n\
    \x20       ref Done: n = 2;\n\
    \x20   }\n\n\
    \x20   state Done;\n\
    }\n\n\
    start Entry = Worker {\n\
    \x20   ref Finish: flag = 99;\n\
    }\n\n\
    state Finish {\n\
    \x20   always { mark := 7; }\n\
    }\n";

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Каталог сборки, уникальный по имени потока (0190; двоеточие - ).
fn build_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0303_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог сборки");
    dir
}

/// Потактовая трасса переменной `mark` у эталона.
fn simulator_trace(src: &str) -> Vec<i128> {
    let (ast, _) = takt_lang::parse(src, 0).expect("разбор");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = takt_sim::build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    for _ in 0..TICKS {
        if let takt_sim::TickResult::Failed(why) = unit.tick() {
            panic!("эталон остановился: {why}");
        }
        match unit.variable("mark") {
            Some(takt_sim::Value::Number(v)) => trace.push(v),
            other => panic!("переменная 'mark' обязана быть числом, получено {other:?}"),
        }
    }
    trace
}

/// Потактовая трасса переменной `mark` у порождённой прошивки.
fn generated_c_trace(dir: &Path, src: &str) -> Vec<i128> {
    takt_lang::compile_to_c(
        UNIT,
        src,
        dir.to_str().expect("путь в UTF-8"),
        &[] as &[String],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");

    let harness = format!(
        r#"#include <stdio.h>
#include "{UNIT}.h"

int main(void) {{
    CompEdge m = {{0}};
    CompEdge_init(&m);
    for (int i = 0; i < {TICKS}; i++) {{
        CompEdge_tick(&m);
        printf("%ld\n", (long)m.mark);
    }}
    return 0;
}}
"#
    );
    let harness_path = dir.join("harness_comp_edge.c");
    std::fs::write(&harness_path, harness).expect("запись харнесса");

    let bin = dir.join("comp_edge_bin");
    let compile = Command::new("cc")
        .args(["-std=c11", "-Wall", "-Wextra", "-Werror", "-I"])
        .arg(dir)
        .arg(dir.join(format!("{UNIT}.c")))
        .arg(&harness_path)
        .arg("-o")
        .arg(&bin)
        .output()
        .expect("запуск cc");
    assert!(
        compile.status.success(),
        "порождённый C не компилируется:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let run = Command::new(&bin).output().expect("запуск прошивки");
    assert!(run.status.success(), "прошивка завершилась с ошибкой");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .map(|l| l.trim().parse::<i128>().expect("значение — целое"))
        .collect()
}

/// **Сработавшее ребро композиции ведёт в своё состояние, а не в `END`.**
///
/// Мутация "не печатать рёбра в ветви завершения" возвращает прежний вывод, и прошивка
/// застревает с `mark = 0`.
#[test]
fn composition_edge_fires_like_reference() {
    let expected = simulator_trace(SRC_FIRES);
    assert!(
        expected.contains(&7),
        "эталон обязан дойти до 'Finish' — иначе проба не о том: {expected:?}"
    );
    if !cc_available() {
        eprintln!("cc недоступен — сверка с прошивкой пропущена");
        return;
    }
    let dir = build_dir("fires");
    let actual = generated_c_trace(&dir, SRC_FIRES);
    assert_eq!(
        actual, expected,
        "трасса прошивки разошлась с эталоном:\nC      = {actual:?}\nэталон = {expected:?}"
    );
}

/// **Несработавшее ребро не завершает автомат.**
///
/// У эталона узел завершается лишь при **пустом** списке переходов; если переходы есть,
/// но ни один не сработал, он остаётся в состоянии. Цели уходили в `END` безусловно -
/// прошивка "досрочно готова" там, где модель ждёт.
#[test]
fn composition_without_fired_edge_waits_like_reference() {
    let expected = simulator_trace(SRC_WAITS);
    assert!(
        expected.iter().all(|v| *v == 0),
        "контроль: ребро не должно срабатывать ни на одном такте: {expected:?}"
    );
    if !cc_available() {
        eprintln!("cc недоступен — сверка с прошивкой пропущена");
        return;
    }
    let dir = build_dir("waits");
    let actual = generated_c_trace(&dir, SRC_WAITS);
    assert_eq!(
        actual, expected,
        "трасса прошивки разошлась с эталоном:\nC      = {actual:?}\nэталон = {expected:?}"
    );
}
