//! Знак объявленного типа в выведенном: эталон == порождённый C -.
//!
//! # Что наблюдается
//!
//! Модель складывает знаковую константу с выведенной и держит результат в переменной
//! `n: i16`. Верная трасса - `−299` на каждом такте; трасса `213` означает, что знак
//! снова потерян.
//!
//! Переменная объявлена **знаковой явно**: иначе `213` и `−299` неразличимы по битам в
//! восьмиразрядном поле, и сверка молчала бы о подмене.
//!
//! # Мягкая деградация
//!
//! Нет `cc` - половина с прошивкой пропускается с сообщением (образец -
//! `conformance_c_unconditional_edge_tests`); трасса эталона проверяется всегда.

use std::path::{Path, PathBuf};
use std::process::Command;

/// Тактов в трассе.
const TICKS: usize = 3;

/// Имя порождаемой единицы (оно же - имя корневой модели: `SignedWiden`).
const UNIT: &str = "signed_widen";

/// Знаковая константа плюс литерал: тип выводится, знак обязан сохраниться.
const SRC: &str = "const A: i16 := -300;\n\
                   const D := A + 1;\n\
                   var n: i16 := 0;\n\
                   start Run { always { n := D; } ref Run; }\n";

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Каталог сборки, уникальный по имени потока (тесты идут параллельно, 0190).
fn build_dir() -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0287_conformance_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог сборки");
    dir
}

/// Потактовая трасса переменной `n` у эталона.
fn simulator_trace() -> Vec<i128> {
    let (ast, _) = takt_lang::parse(SRC, 0).expect("разбор");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = takt_sim::build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    for _ in 0..TICKS {
        let _ = unit.tick();
        match unit.variable("n") {
            Some(takt_sim::Value::Number(v)) => trace.push(v),
            other => panic!("переменная 'n' обязана быть числом, получено {other:?}"),
        }
    }
    trace
}

/// Потактовая трасса переменной `n` у порождённой прошивки.
fn generated_c_trace(dir: &Path) -> Vec<i128> {
    takt_lang::compile_to_c(
        UNIT,
        SRC,
        dir.to_str().expect("путь в UTF-8"),
        &[] as &[String],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");

    let harness = format!(
        r#"#include <stdio.h>
#include "{UNIT}.h"

int main(void) {{
    SignedWiden m = {{0}};
    SignedWiden_init(&m);
    for (int i = 0; i < {TICKS}; i++) {{
        SignedWiden_tick(&m);
        printf("%ld\n", (long)m.n);
    }}
    return 0;
}}
"#
    );
    let harness_path = dir.join("harness_signed.c");
    std::fs::write(&harness_path, harness).expect("запись харнесса");

    let bin = dir.join("signed_bin");
    let compile = Command::new("cc")
        .args(["-std=c11", "-Wall", "-Werror", "-I"])
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
    let run = Command::new(&bin).output().expect("запуск собранного C");
    assert!(run.status.success(), "собранный C завершился с ошибкой");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .map(|l| l.trim().parse::<i128>().expect("число в строке вывода"))
        .collect()
}

/// **A3: знак объявленного типа доезжает до значения - и до прошивки.**
#[test]
fn signed_inferred_const_trace_matches_generated_c() {
    let sim = simulator_trace();
    assert_eq!(
        sim,
        vec![-299; TICKS],
        "эталон: знак объявленного `i16` обязан сохраниться (было 213)"
    );

    if !cc_available() {
        eprintln!("[ПРОПУСК] signed_inferred_const_trace_matches_generated_c: `cc` не найден");
        return;
    }
    let dir = build_dir();
    let c = generated_c_trace(&dir);
    assert_eq!(
        sim, c,
        "потактовые трассы эталона и порождённого C обязаны совпадать:\nsim={sim:?}\nC={c:?}"
    );
}
