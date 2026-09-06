//! Потактовая сверка целой степени `**`.

use std::path::{Path, PathBuf};
use std::process::Command;

const FIXTURE: &str = "tests/data/eval/conformance_power.takt";
const UNIT: &str = "conformance_power";
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

/// Трасса эталона: `(probe, cube)` по тактам.
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
        trace.push((value("probe"), value("cube")));
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

static unsigned long long probe_value = 0;
static unsigned long long cube_value = 0;

static void wr_num(ConformancePower_Out_NumericPort port, uint8_t index, int64_t v, void *ud) {{
    (void)index;
    (void)ud;
    if (port == CONFORMANCE_POWER_PORT_PROBE) {{
        probe_value = (unsigned long long)v;
    }} else {{
        cube_value = (unsigned long long)v;
    }}
}}

int main(void) {{
    ConformancePower m = {{0}};
    m.write_numeric = wr_num;
    ConformancePower_init(&m);
    for (int i = 0; i < {TICKS}; i++) {{
        ConformancePower_tick(&m);
        printf("TICK %llu %llu\n", probe_value, cube_value);
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
            let cube = it.next()?.parse::<i128>().ok()?;
            Some((probe, cube))
        })
        .collect()
}

/// Значения степени совпадают у эталона и порождённого C.
///
/// Ожидание записано **числами**: `3 ** 40` = 12157665459056928801 (не
/// 12157665459056928768, как считал `double`), `2 ** 3` = 8.
#[test]
fn power_matches_generated_c() {
    let sim = simulator_trace();
    assert_eq!(
        sim,
        vec![(12_157_665_459_056_928_801, 8); 3],
        "эталон обязан считать степень точно: {sim:?}"
    );

    if !cc_available() {
        eprintln!("[ПРОПУСК] local_declaration_matches_generated_c: cc не найден");
        return;
    }
    let dir = build_dir("local_decl");
    let c = generated_c_trace(&dir);
    assert_eq!(sim, c, "трассы эталона и C разошлись\nsim={sim:?}\nC={c:?}");
}

/// Цель `st` печатает степень **умножениями**, и `iec2c` это принимает.
///
/// Прежде печаталось `a ** 2`, и арбитр отвергал файл - при нулевом коде возврата
/// `taktc`. Проверяется текст и прогон настоящего `iec2c`.
#[test]
fn generated_st_expands_power_into_multiplications() {
    let dir = build_dir("power_st");
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_st(
        UNIT,
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение ST");
    let text = std::fs::read_to_string(dir.join(format!("{UNIT}.st"))).expect("чтение вывода");
    assert!(
        !text.contains("**"),
        "оператор `**` в IEC определён над вещественным — его в выводе быть не должно:\n{text}"
    );
    assert!(
        text.contains("narrow * narrow * narrow"),
        "степень обязана разворачиваться в умножения:\n{text}"
    );

    let iec2c =
        std::path::Path::new(&std::env::var("HOME").unwrap_or_default()).join(".local/bin/iec2c");
    if !iec2c.is_file() {
        eprintln!("[ПРОПУСК] generated_st_expands_power_into_multiplications: iec2c не найден");
        return;
    }
    let lib = std::path::Path::new(&std::env::var("HOME").unwrap_or_default())
        .join(".local/share/matiec/lib");
    let out = Command::new(&iec2c)
        .arg("-I")
        .arg(&lib)
        .arg("-T")
        .arg(&dir)
        .arg(dir.join(format!("{UNIT}.st")))
        .output()
        .expect("запуск iec2c");
    let log = String::from_utf8_lossy(&out.stderr);
    assert!(
        !log.contains("error"),
        "порождённый ST не принят арбитром:\n{log}"
    );
}

/// Цели `rust` и `sv` тоже переводят степень.
///
/// `rust` печатает `wrapping_pow`: он даёт **ровно** семантику эталона (обёртка `mod
/// 2ⁿ`), тогда как обычный `pow` паникует при переполнении в отладке - то есть прошивка
/// и прогон разошлись бы падением.
///
/// `sv` разворачивает степень в умножения: синтезатору нужна константа, и при
/// литеральном показателе она есть.
#[test]
fn rust_and_sv_translate_power() {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    let dir = build_dir("power_targets");

    takt_lang::compile_to_rust(
        UNIT,
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение Rust");
    let rust = std::fs::read_to_string(dir.join(format!("{UNIT}.rs"))).expect("чтение Rust");
    assert!(
        rust.contains("wrapping_pow"),
        "цель rust обязана печатать wrapping_pow:\n{rust}"
    );

    takt_lang::compile_to_sv(
        UNIT,
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение SystemVerilog");
    let sv = std::fs::read_to_string(dir.join(format!("{UNIT}.sv"))).expect("чтение SV");
    assert!(
        !sv.contains("**"),
        "оператор `**` синтезатору не годится — его в выводе быть не должно:\n{sv}"
    );
}
