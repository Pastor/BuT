//! Бит-вектор шире 64 бит: эталон == порождённый C.
//!
//! # Что наблюдается
//!
//! Модель ставит разряды в **разных** словах (5 - слово 0, 70 - слово 1) и собирает
//! наблюдаемое число из трёх проверок: оба поставленных разряда и соседний, который
//! остаться нулём. Трасса - `7` на каждом такте; любая путаница слов даёт меньшее
//! число.
//!
//! Разряды выбраны в **разных** словах намеренно: с одним словом сверка не отличает
//! верное деление от "всегда слово 0".
//!
//! # Мягкая деградация
//!
//! Нет `cc` - половина с прошивкой пропускается с сообщением; трасса эталона
//! проверяется всегда.

use std::path::{Path, PathBuf};
use std::process::Command;

/// Тактов в трассе.
const TICKS: usize = 3;

/// Имя порождаемой единицы (оно же - имя корневой модели: `WideBits`).
const UNIT: &str = "wide_bits";

/// Разряды в двух разных словах плюс контрольный сосед.
const SRC: &str = "var w: [bit;96] := 0;\n\
                   var n: u8 := 0;\n\
                   start Run {\n\
                       always {\n\
                           w.5 := 1;\n\
                           w.70 := 1;\n\
                           n := 0;\n\
                           if w.5 { n := n + 1; }\n\
                           if w.70 { n := n + 2; }\n\
                           if w.71 { n := n + 8; }\n\
                           n := n + 4;\n\
                       }\n\
                       ref Run;\n\
                   }\n";

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
        .join(format!("takt_0262_conformance_{thread}"));
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
    WideBits m = {{0}};
    WideBits_init(&m);
    for (int i = 0; i < {TICKS}; i++) {{
        WideBits_tick(&m);
        printf("%ld\n", (long)m.n);
    }}
    return 0;
}}
"#
    );
    let harness_path = dir.join("harness_wide.c");
    std::fs::write(&harness_path, harness).expect("запись харнесса");

    let bin = dir.join("wide_bin");
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
    let run = Command::new(&bin).output().expect("запуск собранного C");
    assert!(run.status.success(), "собранный C завершился с ошибкой");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .map(|l| l.trim().parse::<i128>().expect("число в строке вывода"))
        .collect()
}

/// **A3: разряд ложится в своё слово - и у эталона, и в прошивке.**
#[test]
fn wide_bit_vector_trace_matches_generated_c() {
    let sim = simulator_trace();
    assert_eq!(
        sim,
        vec![7; TICKS],
        "эталон: поставлены разряды 5 и 70, разряд 71 остаётся нулём"
    );

    if !cc_available() {
        eprintln!("[ПРОПУСК] wide_bit_vector_trace_matches_generated_c: `cc` не найден");
        return;
    }
    let dir = build_dir();
    let c = generated_c_trace(&dir);
    assert_eq!(
        sim, c,
        "потактовые трассы эталона и порождённого C обязаны совпадать:\nsim={sim:?}\nC={c:?}"
    );
}
