//! Цепочка функций, читающих состояние: цель `st` против эталона.
//!
//! ## Что доказывает набор
//!
//! Приём `iec2c` доказывает, что вывод **валиден**; он не доказывает, что `VAR_IN_OUT`
//! связал те переменные. Свяжись список не с той - вывод остался бы валидным (типы
//! совпадают), а значение разошлось бы. Поэтому фикстура держит две переменные разной
//! величины, читаемые на **разных** уровнях цепочки, и сверяется трасса `probe`.
//!
//! ## Мягкая деградация
//!
//! Нет `iec2c`/`cc`/заголовков MatIEC -> тест-пропуск, а не красный (как у прочих
//! сверок `st`).

use std::path::{Path, PathBuf};
use std::process::Command;

use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Unit, Value, build_unit};

const FIXTURE: &str = "tests/data/eval/conformance_st_transitive.takt";
const UNIT: &str = "sttrans";
/// Имя корневого POU в символах `iec2c`: идентификаторы IEC регистронезависимы.
const ROOT: &str = "STTRANS";
/// Путь к наблюдаемому порту: под-FB и его выход.
const PORT: &str = "ST_TRANS0.PROBE";
const SCANS: usize = 3;
/// `probe` по сканам: `(ticks + (ticks + 1)) * gain` при `gain = 3`.
const EXPECTED: [i128; SCANS] = [9, 15, 21];

fn temp_dir(tag: &str) -> PathBuf {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!(
            "takt_0505_{tag}_{}",
            std::thread::current()
                .name()
                .unwrap_or("single")
                .replace(':', "_")
        ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    dir
}

fn source() -> String {
    std::fs::read_to_string(FIXTURE).expect("фикстура читается")
}

fn iec2c_available() -> Option<(PathBuf, PathBuf)> {
    let prefix = std::env::var_os("IEC2C_PREFIX")
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from(std::env::var_os("HOME").unwrap()).join(".local"));
    let bin = prefix.join("bin").join("iec2c");
    let lib = prefix.join("share").join("matiec").join("lib");
    (bin.is_file() && lib.join("ieclib.txt").is_file()).then_some((bin, lib))
}

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

fn simulator_trace() -> Vec<i128> {
    let (ast, _) = takt_lang::parse(&source(), 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit: Unit = build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    for _ in 0..SCANS {
        assert!(
            !matches!(unit.tick(), TickResult::Failed(_)),
            "прогон не обрывается"
        );
        match unit.variable("probe") {
            Some(Value::Number(v)) => trace.push(v),
            other => panic!("порт 'probe': {other:?}"),
        }
    }
    trace
}

fn st_trace(dir: &Path, iec2c: &Path, lib: &Path) -> Vec<i128> {
    let st_dir = dir.join("st");
    std::fs::create_dir_all(&st_dir).expect("каталог ST");
    takt_lang::compile_to_st(
        UNIT,
        &source(),
        st_dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение ST");

    let work = dir.join("iec2c");
    std::fs::create_dir_all(&work).expect("рабочий каталог");
    let transpile = Command::new(iec2c)
        .arg("-I")
        .arg(lib)
        .arg(st_dir.join(format!("{UNIT}.st")))
        .current_dir(&work)
        .output()
        .expect("запуск iec2c");
    assert!(
        transpile.status.success() && work.join("POUS.c").is_file(),
        "iec2c не оттранслировал ST (прежде здесь было «Variable not declared \
         in this scope»):\n{}",
        String::from_utf8_lossy(&transpile.stderr)
    );

    let harness = format!(
        r#"#include <stdio.h>
#include "iec_std_lib.h"
TIME __CURRENT_TIME;
BOOL __DEBUG = 0;
#include "POUS.h"
#include "POUS.c"

int main(void) {{
    {ROOT}_data__ fb = {{0}};
    {ROOT}_init__(&fb, __BOOL_LITERAL(FALSE));
    for (int i = 0; i < {SCANS}; i++) {{
        {ROOT}_body__(&fb);
        printf("%u\n", (unsigned)fb.{PORT}.value);
    }}
    return 0;
}}
"#
    );
    std::fs::write(work.join("harness.c"), harness).expect("харнесс");
    let bin = work.join("bin");
    // Заголовки время выполненияа MatIEC лежат в `lib/C`, а не в `lib`: соседние сверки `st`
    // берут именно этот путь.
    let compile = Command::new("cc")
        .args(["-std=c99", "-w", "-I"])
        .arg(lib.join("C"))
        .arg("-I")
        .arg(&work)
        .arg("-o")
        .arg(&bin)
        .arg(work.join("harness.c"))
        .output()
        .expect("запуск cc");
    assert!(
        compile.status.success(),
        "cc не собрал харнесс ST:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск харнесса");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter_map(|line| line.trim().parse::<i128>().ok())
        .collect()
}

/// Эталон даёт ожидаемую трассу - она же ожидается от цели.
#[test]
fn simulator_trace_matches_expectation() {
    assert_eq!(simulator_trace(), EXPECTED.to_vec());
}

/// Цель `st` считает то же: `VAR_IN_OUT` связал те переменные.
#[test]
fn st_target_matches_simulator() {
    let Some((iec2c, lib)) = iec2c_available() else {
        eprintln!("[ПРОПУСК] st_target_matches_simulator: нет iec2c");
        return;
    };
    if !cc_available() {
        eprintln!("[ПРОПУСК] st_target_matches_simulator: нет cc");
        return;
    }
    let dir = temp_dir("st");
    let trace = st_trace(&dir, &iec2c, &lib);
    assert_eq!(
        trace,
        EXPECTED.to_vec(),
        "цель `st` обязана считать то же, что эталон"
    );
    let _ = std::fs::remove_dir_all(&dir);
}
