//! Потактовая сверка локального объявления в теле блока.

use std::path::{Path, PathBuf};
use std::process::Command;

const FIXTURE: &str = "tests/data/eval/conformance_local_decl.takt";
const UNIT: &str = "conformance_local_decl";
const TICKS: usize = 3;

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Каталог сборки уникален по тесту.
fn build_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог сборки");
    dir
}

/// Трасса эталона: `(probe, mirror)` по тактам.
fn simulator_trace() -> Vec<(i128, i128)> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор фикстуры");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = takt_sim::build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    for _ in 0..TICKS {
        let _ = unit.tick();
        let value = |name: &str| match unit.variable(name) {
            Some(takt_sim::Value::Number(v)) => v,
            other => panic!("порт '{name}' обязан быть числом, получено {other:?}"),
        };
        trace.push((value("probe"), value("mirror")));
    }
    trace
}

/// Трасса порождённого C: те же порты по тактам.
fn generated_c_trace(dir: &Path) -> Vec<(i128, i128)> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_c(
        UNIT,
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");

    let harness = format!(
        r#"#include <stdio.h>
#include "{UNIT}.h"

static long probe_value = 0;
static long mirror_value = 0;

static void wr_num(ConformanceLocalDecl_Out_NumericPort port, uint8_t index, int64_t v, void *ud) {{
    (void)index;
    (void)ud;
    if (port == CONFORMANCE_LOCAL_DECL_PORT_PROBE) {{
        probe_value = (long)v;
    }} else {{
        mirror_value = (long)v;
    }}
}}

int main(void) {{
    ConformanceLocalDecl m = {{0}};
    m.write_numeric = wr_num;
    ConformanceLocalDecl_init(&m);
    for (int i = 0; i < {TICKS}; i++) {{
        ConformanceLocalDecl_tick(&m);
        printf("TICK %ld %ld\n", probe_value, mirror_value);
    }}
    return 0;
}}
"#
    );
    let harness_path = dir.join("harness_local_decl.c");
    std::fs::write(&harness_path, harness).expect("запись харнесса");

    let bin = dir.join("local_decl_bin");
    let compile = Command::new("cc")
        .args([
            "-std=c11",
            "-Wall",
            "-Wextra",
            "-Wno-unused-parameter",
            "-Werror",
            "-I",
        ])
        .arg(dir)
        .arg(dir.join(format!("{UNIT}.c")))
        .arg(&harness_path)
        .arg("-o")
        .arg(&bin)
        .output()
        .expect("запуск cc");
    assert!(
        compile.status.success(),
        "порождённый C с локальным объявлением не компилируется:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск собранного C");
    assert!(run.status.success(), "харнесс упал");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter_map(|line| {
            let rest = line.strip_prefix("TICK ")?;
            let mut it = rest.split_whitespace();
            let probe = it.next()?.parse::<i128>().ok()?;
            let mirror = it.next()?.parse::<i128>().ok()?;
            Some((probe, mirror))
        })
        .collect()
}

/// Эталон исполняет локальное объявление и совпадает с порождённым C.
///
/// Ожидание записано **числами**: `g = F + 1 = 6`, тело накапливает, значит `probe`
/// идёт 6, 12, 18; `mirror` - контрольный вход (то же объявление на верхнем уровне) и
/// держится 6.
#[test]
fn local_declaration_matches_generated_c() {
    let sim = simulator_trace();
    assert_eq!(
        sim,
        vec![(6, 6), (12, 6), (18, 6)],
        "эталон обязан исполнять локальное объявление: {sim:?}"
    );

    if !cc_available() {
        eprintln!("[ПРОПУСК] local_declaration_matches_generated_c: cc не найден");
        return;
    }
    let dir = build_dir("local_decl");
    let c = generated_c_trace(&dir);
    assert_eq!(sim, c, "трассы эталона и C разошлись\nsim={sim:?}\nC={c:?}");
}

/// Цель `sv` печатает **валидный** модуль: локальная переменная объявлена.
///
/// Без правки цель молчит об этом дефекте - код возврата `taktc` нулевой, а
/// `verilator` отвечал "Can't find definition of variable". Проверяется линтом, потому
/// что предмет здесь - **валидность вывода**, а не значения (их сверяет тест выше на
/// цели `c`).
#[test]
fn generated_sv_declares_local_variable() {
    let dir = build_dir("local_decl_sv");
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_sv(
        UNIT,
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение SystemVerilog");
    let text = std::fs::read_to_string(dir.join(format!("{UNIT}.sv"))).expect("чтение модуля");
    assert!(
        text.contains("automatic logic"),
        "локальная переменная тела обязана быть объявлена:\n{text}"
    );

    let available = Command::new("verilator")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false);
    if !available {
        eprintln!("[ПРОПУСК] generated_sv_declares_local_variable: verilator не найден");
        return;
    }
    let lint = Command::new("verilator")
        .args(["--lint-only", "-Wall"])
        .arg(dir.join(format!("{UNIT}.sv")))
        .output()
        .expect("запуск verilator");
    assert!(
        lint.status.success(),
        "порождённый SV не проходит линт:\n{}",
        String::from_utf8_lossy(&lint.stderr)
    );
}
