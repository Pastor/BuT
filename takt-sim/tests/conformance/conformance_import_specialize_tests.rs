//! Специализация импортированной модели: эталон == цель `c`.
//!
//! # Что доказывается
//!
//! Параметризованная модель, подключённая `import`, под `--parameters=specialize` даёт
//! **ту же** потактовую трассу, что в режиме `assign` и что эталон. До фичи такой вход
//! отвергался `SE-120`, а до неё - порождал C, который `cc` не принимает: тела копии
//! писали в поля исходной модели ("no member named 'tuner'") при **нулевом** коде
//! возврата.
//!
//! # Мягкая деградация
//!
//! Нет `cc` - половина с целью `c` **пропускается с сообщением**; трасса эталона
//! проверяется всегда.

use std::path::{Path, PathBuf};
use std::process::Command;
use takt_lang::semantic::tree::construct_model_with_files;
use takt_sim::{TickResult, Unit, build_unit};

/// Тактов сверки. Пределы 30 и 60 при шаге 5 разводят экземпляры на 6-м такте: первый
/// уже в `Done`, второй ещё считает - то есть трасса различает их не только значением,
/// но и моментом остановки.
const TICKS: usize = 12;

/// Библиотека: параметризованная модель.
const LIB: &str = "model Tuner {\n\
                   \x20   parameter limit: u8 := 100;\n\
                   \x20   var acc: u8 := 0;\n\n\
                   \x20   start Count {\n\
                   \x20       always { acc := acc + 5; }\n\
                   \x20       ref Done: acc >= limit;\n\
                   \x20   }\n\n\
                   \x20   state Done { ref Done; }\n\
                   }\n";

/// Импортёр: два экземпляра с разными аргументами.
const APP: &str = "import { Tuner } from \"tuner_lib.takt\";\n\n\
                   start Main = Tuner(limit := 30) | Tuner(limit := 60);\n";

/// Имена полей экземпляров в порождённом C - свои у каждого режима (сняты пробой
/// заголовка, а не угаданы).
const FIELDS_ASSIGN: (&str, &str) = ("tuner0", "tuner1");
const FIELDS_SPECIALIZE: (&str, &str) = ("tuner_p10", "tuner_p21");

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Каталог теста, уникальный по имени потока.
fn build_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("main")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0395_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание каталога");
    dir
}

/// Кладёт оба файла и возвращает путь входа.
fn fixtures(dir: &Path) -> PathBuf {
    std::fs::write(dir.join("tuner_lib.takt"), LIB).expect("запись библиотеки");
    let app = dir.join("app.takt");
    std::fs::write(&app, APP).expect("запись импортёра");
    app
}

/// Юнит эталона в заданном режиме параметров.
fn unit_of(dir: &Path, specialize: bool) -> Unit {
    let (ast, _) = takt_lang::parse(APP, 0).expect("разбор");
    let mut files = takt_lang::diagnostics::FileTable::default();
    let model = construct_model_with_files(
        &ast,
        None,
        &[dir.to_str().expect("путь").to_string()],
        &mut files,
        specialize,
    )
    .expect("семантика");
    build_unit(model).expect("построение юнита")
}

/// Значения `acc` у обоих экземпляров - структурно, снимком.
///
/// Квалифицированное имя не годится: в режиме `assign` обе под-модели зовутся `Tuner`.
fn accumulators(unit: &Unit) -> Vec<i128> {
    fn walk(snap: &takt_sim::state_io::UnitSnapshot, out: &mut Vec<i128>) {
        match snap {
            takt_sim::state_io::UnitSnapshot::None => {}
            takt_sim::state_io::UnitSnapshot::Node { variables, .. } => {
                if let Some(value) = variables
                    .get("acc")
                    .and_then(|v| v.as_number())
                    .and_then(serde_json::Number::as_i128)
                {
                    out.push(value);
                }
            }
            takt_sim::state_io::UnitSnapshot::Parallel { children }
            | takt_sim::state_io::UnitSnapshot::Sequential { children, .. } => {
                for child in children {
                    walk(child, out);
                }
            }
        }
    }
    let mut out = Vec::new();
    walk(&takt_sim::state_io::snapshot(unit), &mut out);
    out
}

/// Трасса эталона.
fn sim_trace(dir: &Path, specialize: bool) -> Vec<(i128, i128)> {
    let mut unit = unit_of(dir, specialize);
    let mut trace = Vec::new();
    for tick in 1..=TICKS {
        assert!(
            !matches!(unit.tick(), TickResult::Failed(_)),
            "такт {tick}: эталон отказал (specialize = {specialize})"
        );
        let values = accumulators(&unit);
        assert_eq!(
            values.len(),
            2,
            "такт {tick}: ожидались два экземпляра, снимок дал {values:?}"
        );
        trace.push((values[0], values[1]));
    }
    trace
}

/// Трасса порождённого C в заданном режиме.
fn c_trace(dir: &Path, entry: &Path, specialize: bool, fields: (&str, &str)) -> Vec<(i128, i128)> {
    let out = dir.join(if specialize { "spec" } else { "assign" });
    std::fs::create_dir_all(&out).expect("каталог вывода");
    // `GenerateOptions` - `#[non_exhaustive]`: поле правится после `default()`.
    let mut options = takt_lang::generator::GenerateOptions::default();
    options.specialize = specialize;
    takt_lang::compile_to_c(
        entry.to_str().expect("путь входа"),
        APP,
        out.to_str().expect("путь вывода"),
        &[dir.to_str().expect("путь").to_string()],
        &options,
    )
    .expect("порождение C");

    let (first, second) = fields;
    let harness = format!(
        r#"#include <stdio.h>
#include "app.h"

int main(void) {{
    App m;
    App_init(&m);
    for (int i = 0; i < {TICKS}; i++) {{
        App_tick(&m);
        printf("%d %d\n", (int)m.main.{first}.acc, (int)m.main.{second}.acc);
    }}
    return 0;
}}
"#
    );
    let harness_path = out.join("harness.c");
    std::fs::write(&harness_path, &harness).expect("запись харнесса");

    let bin = out.join("app_bin");
    let compile = Command::new("cc")
        .args(["-std=c11", "-Wall", "-Werror", "-o"])
        .arg(&bin)
        .arg(&harness_path)
        .arg(out.join("app.c"))
        .arg("-I")
        .arg(&out)
        .output()
        .expect("запуск cc");
    assert!(
        compile.status.success(),
        "cc не собрал харнесс (specialize = {specialize}):\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let run = Command::new(&bin).output().expect("запуск харнесса");
    assert!(run.status.success(), "харнесс завершился с ошибкой");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .map(|line| {
            let mut parts = line.split_whitespace().map(|p| p.parse::<i128>().unwrap());
            (parts.next().unwrap(), parts.next().unwrap())
        })
        .collect()
}

/// Ожидание считается независимо от обоих исполнителей: совпадение двух реализаций
/// между собой ещё не значит, что они правы.
fn expected() -> Vec<(i128, i128)> {
    let mut trace = Vec::new();
    let (mut fast, mut slow) = (0i128, 0i128);
    for _ in 0..TICKS {
        if fast < 30 {
            fast += 5;
        }
        if slow < 60 {
            slow += 5;
        }
        trace.push((fast, slow));
    }
    trace
}

/// Четыре трассы - эталон и цель `c` в обоих режимах - совпадают потактово.
#[test]
fn imported_model_specialization_agrees_tick_by_tick() {
    let dir = build_dir("modes");
    let entry = fixtures(&dir);

    let want = expected();
    assert_eq!(sim_trace(&dir, false), want, "эталон в режиме assign");
    assert_eq!(sim_trace(&dir, true), want, "эталон в режиме specialize");

    if !cc_available() {
        eprintln!(
            "[ПРОПУСК] сверка с целью `c`: компилятор `cc` не найден; \
             трассы эталона в обоих режимах уже сверены"
        );
        return;
    }
    assert_eq!(
        c_trace(&dir, &entry, false, FIELDS_ASSIGN),
        want,
        "цель c в режиме assign"
    );
    assert_eq!(
        c_trace(&dir, &entry, true, FIELDS_SPECIALIZE),
        want,
        "цель c в режиме specialize — форма вывода другая, поведение то же"
    );
}
