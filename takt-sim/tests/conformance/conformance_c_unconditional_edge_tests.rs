//! Безусловное ребро: эталон == порождённый C -.
//!
//! # Что наблюдается
//!
//! Модель: `Run` крутит `n := n + 1` и несёт **безусловное** ребро в `Done`, а за ним -
//! ребро в `Late` по условию `n = 1`. Хвост недостижим у эталона (симулятор выбирает
//! `Done` уже на первом такте), поэтому верная трасса - `1, 0, 0, ...`: такт в `Run`,
//! дальше `Done` обнуляет `n`. Трасса `1, 9, ...` означала бы, что цель ушла в `Late`,
//! то есть что удалён был не тот код.
//!
//! Условие хвоста выбрано **истинным на том же такте** (`n = 1`, а не `n = 3`)
//! намеренно: иначе тест не отличает верную правку от половинчатой ("`break;` снят,
//! но обход рёбер продолжается") - при ложном условии обе дают одну трассу. Проверено
//! мутацией.
//!
//! Записи "ребро после безусловного" в корпусе `examples/` нет ни одной, так что этот
//! класс держится **только** здесь.
//!
//! # Мягкая деградация
//!
//! Нет `cc` - половина с прошивкой пропускается с сообщением (образец -
//! `conformance_c_import_tests`); трасса эталона проверяется всегда.

use std::path::{Path, PathBuf};
use std::process::Command;

/// Тактов в трассе.
const TICKS: usize = 4;

/// Имя порождаемой единицы (оно же - имя корневой модели: `UncondEdge`).
const UNIT: &str = "uncond_edge";

/// Безусловное ребро с хвостом: `ref Late` недостижим.
const SRC: &str = "var n: u8 := 0;\n\
                   start Run { always { n := n + 1; } ref Done; ref Late: n = 1; }\n\
                   state Done { always { n := 0; } }\n\
                   state Late { always { n := 9; } }\n";

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
        .join(format!("takt_0213_conformance_{thread}"));
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
    UncondEdge m = {{0}};
    UncondEdge_init(&m);
    for (int i = 0; i < {TICKS}; i++) {{
        UncondEdge_tick(&m);
        printf("%ld\n", (long)m.n);
    }}
    return 0;
}}
"#
    );
    let harness_path = dir.join("harness_uncond.c");
    std::fs::write(&harness_path, harness).expect("запись харнесса");

    let bin = dir.join("uncond_bin");
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

/// **A3: изъятие недостижимого кода поведение не меняет.**
#[test]
fn unconditional_edge_trace_matches_generated_c() {
    let sim = simulator_trace();
    assert_eq!(
        sim,
        vec![1, 0, 0, 0],
        "эталон: безусловное ребро уводит в Done уже на первом такте, хвост недостижим"
    );

    if !cc_available() {
        eprintln!("[ПРОПУСК] unconditional_edge_trace_matches_generated_c: `cc` не найден");
        return;
    }
    let dir = build_dir();
    let c = generated_c_trace(&dir);
    assert_eq!(
        sim, c,
        "потактовые трассы эталона и порождённого C обязаны совпадать:\nsim={sim:?}\nC={c:?}"
    );
}
