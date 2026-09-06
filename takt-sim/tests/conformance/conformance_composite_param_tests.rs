//! Сверка массива с составным элементом в параметре функции.
//!
//! ## Что доказывает
//!
//! научила цель `sv` передавать массив **скаляров** плоским вектором; массив структур,
//! перечислений и вложенный массив шли прежней формой - распакованным портом функции.
//!
//! - `sv`/`sv-mmio` - "input/output/inout ports cannot have unpacked
//!   dimensions" у **yosys**, тогда как verilator тот же модуль принимает;
//! - `st`/`st-at` (вложенный массив) - имя типа-формы строилось из текста типа
//!   элемента и давало `TAKT_ARR_2_ARRAY_[0..1]_OF_USINT`, который `iec2c`
//!   отвергает.
//!
//! Всё - при **нулевом** коде возврата `taktc`, а эталон, `c` и `rust` те же входы
//! исполняют.

use std::path::{Path, PathBuf};
use std::process::Command;

use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Unit, Value, build_unit};

const FIXTURE: &str = "tests/data/eval/conformance_composite_param.takt";
const UNIT: &str = "compositeparam";
const TICKS: usize = 3;

/// Наблюдаемые: имя порта в эталоне и путь поля в структуре POUS у цели `st`.
///
/// `lo0`/`hi1` - поля разных элементов массива структур, `g00`/`g11` - углы вложенного
/// массива, `m0`/`m1` - позиции массива перечислений.
const OBSERVED: &[(&str, &str)] = &[
    ("lo0", "LO0"),
    ("hi1", "HI1"),
    ("g00", "G00"),
    ("g11", "G11"),
    ("m0", "M0"),
    ("m1", "M1"),
];

/// Имя корневой модели в C-символах `iec2c` (идентификаторы IEC регистронезависимы, и
/// `iec2c` печатает их в верхнем регистре).
const ST_ROOT: &str = "COMPOSITEPARAM";

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

fn verilator_available() -> bool {
    Command::new("verilator")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

fn yosys_available() -> bool {
    Command::new("yosys")
        .arg("-V")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

fn iec2c_available() -> Option<(PathBuf, PathBuf)> {
    let prefix = match std::env::var("IEC2C_PREFIX") {
        Ok(p) => PathBuf::from(p),
        Err(_) => PathBuf::from(std::env::var("HOME").unwrap_or_default()).join(".local"),
    };
    let bin = prefix.join("bin").join("iec2c");
    let lib = prefix.join("share").join("matiec").join("lib");
    (bin.is_file() && lib.join("C").is_dir()).then_some((bin, lib))
}

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

fn sim_value(unit: &Unit, name: &str) -> i128 {
    match unit.variable(name) {
        Some(Value::Number(v)) => v,
        Some(Value::Boolean(b)) => i128::from(b),
        other => panic!("порт '{name}': неожиданное значение {other:?}"),
    }
}

/// Трасса эталона: значения наблюдаемых после каждого такта.
fn simulator_trace() -> Vec<Vec<i128>> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор фикстуры");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    for _ in 0..TICKS {
        let result = unit.tick();
        assert!(
            !matches!(result, TickResult::Failed(_)),
            "симуляция не должна падать: {result:?}"
        );
        trace.push(
            OBSERVED
                .iter()
                .map(|(name, _)| sim_value(&unit, name))
                .collect(),
        );
    }
    trace
}

fn generate_sv(dir: &Path) {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_sv(
        UNIT,
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение SystemVerilog");
}

/// Трасса порождённого RTL.
fn generated_sv_trace(dir: &Path) -> Vec<Vec<i128>> {
    generate_sv(dir);
    let ports = OBSERVED
        .iter()
        .map(|(name, _)| format!(".{name}({name})"))
        .collect::<Vec<_>>()
        .join(", ");
    let decls = OBSERVED
        .iter()
        .map(|(name, _)| format!("    logic [7:0] {name};"))
        .collect::<Vec<_>>()
        .join("\n");
    let args = OBSERVED
        .iter()
        .map(|(name, _)| (*name).to_string())
        .collect::<Vec<_>>()
        .join(", ");
    let fmt = vec!["%0d"; OBSERVED.len()].join(" ");
    let tb = format!(
        r#"module tb;
    logic clk = 0, rst_n = 0;
    logic is_done;
{decls}
    {UNIT} dut (.clk(clk), .rst_n(rst_n), {ports}, .is_done(is_done));
    always #5 clk = ~clk;
    initial begin
        @(posedge clk);
        rst_n <= 1'b1;
        for (int i = 0; i < {TICKS}; i++) begin
            @(posedge clk);
            #1 $display("TICK {fmt}", {args});
        end
        $finish;
    end
endmodule
"#
    );
    std::fs::write(dir.join("tb.sv"), tb).expect("тестбенч");

    let build = Command::new("verilator")
        .current_dir(dir)
        .args([
            "--binary",
            // `-j 4`, а не `-j 0` (все ядра), как в проверке предкоммита: проверка гоняет
            // verilator в одиночку, а тестов с ним - три десятка, и cargo запускает их
            // параллельно. Кратная перегрузка изредка роняла сборку на временных файлах
            // самого verilator - флак.
            "-j",
            "4",
            "--timing",
            "-Wno-fatal",
            "--top-module",
            "tb",
            "tb.sv",
            &format!("{UNIT}.sv"),
            "-o",
            "simtb",
        ])
        .output()
        .expect("запуск verilator");
    assert!(
        build.status.success(),
        "verilator не собрал тестбенч:\n{}",
        String::from_utf8_lossy(&build.stderr)
    );
    let run = Command::new(dir.join("obj_dir").join("simtb"))
        .current_dir(dir)
        .output()
        .expect("запуск симуляции");
    assert!(run.status.success(), "симуляция RTL упала");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter_map(|line| {
            let rest = line.strip_prefix("TICK ")?;
            Some(
                rest.split_whitespace()
                    .map(|v| v.parse().expect("число в трассе"))
                    .collect(),
            )
        })
        .collect()
}

/// Значения совпадают с RTL: структура, вложенный массив и перечисление в параметре
/// передаются в верном порядке разрядов.
#[test]
fn composite_parameter_values_match_generated_sv() {
    let sim = simulator_trace();
    assert_eq!(
        sim,
        vec![
            vec![1, 21, 41, 61, 1, 2],
            vec![2, 22, 42, 62, 1, 2],
            vec![3, 23, 43, 63, 1, 2],
        ],
        "эталон: поля элементов и углы вложенного массива растут, позиции \
         перечисления различны: {sim:?}"
    );

    if !verilator_available() {
        eprintln!("[ПРОПУСК] composite_parameter_values_match_generated_sv: нет verilator");
        return;
    }
    let dir = build_dir("composite_param_sv");
    let rtl = generated_sv_trace(&dir);
    assert_eq!(rtl.len(), TICKS, "тестбенч обязан напечатать каждый такт");
    assert_eq!(sim, rtl, "трассы разошлись\nsim={sim:?}\nRTL={rtl:?}");
    let _ = std::fs::remove_dir_all(&dir);
}

/// Порождённый модуль синтезируется: verilator этого не доказывает.
///
/// Класс видит **один инструмент из трёх**: и `taktc`, и verilator принимали прежний
/// вывод, а yosys отвечал "input/output/inout ports cannot have unpacked dimensions".
#[test]
fn composite_parameter_is_synthesizable() {
    if !yosys_available() {
        eprintln!("[ПРОПУСК] composite_parameter_is_synthesizable: нет yosys");
        return;
    }
    let dir = build_dir("composite_param_synth");
    generate_sv(&dir);
    let synth = Command::new("yosys")
        .current_dir(&dir)
        .args([
            "-q",
            "-p",
            &format!("read_verilog -sv {UNIT}.sv; synth -top {UNIT}"),
        ])
        .output()
        .expect("запуск yosys");
    assert!(
        synth.status.success(),
        "порождённый SystemVerilog не синтезируется:\n{}",
        String::from_utf8_lossy(&synth.stderr)
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// Трасса цели `st`: тот же ST, оттранслированный `iec2c` и исполненный.
fn st_trace(dir: &Path, iec2c: &Path, lib: &Path) -> Vec<Vec<i128>> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    let st_dir = dir.join("st");
    std::fs::create_dir_all(&st_dir).expect("каталог ST");
    takt_lang::compile_to_st(
        "composite_param.takt",
        &source,
        st_dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение ST");

    let work = dir.join("iec2c");
    std::fs::create_dir_all(&work).expect("рабочий каталог iec2c");
    let transpile = Command::new(iec2c)
        .arg("-I")
        .arg(lib)
        .arg(st_dir.join("composite_param.st"))
        .current_dir(&work)
        .output()
        .expect("запуск iec2c");
    assert!(
        transpile.status.success() && work.join("POUS.c").is_file(),
        "iec2c не оттранслировал порождённый ST:\n{}",
        String::from_utf8_lossy(&transpile.stderr)
    );

    let prints = OBSERVED
        .iter()
        .map(|(_, path)| format!(r#"        printf("%lld ", (long long)fb.{path}.value);"#))
        .collect::<Vec<_>>()
        .join("\n");
    let harness = format!(
        r#"#include <stdio.h>
#include "iec_std_lib.h"
TIME __CURRENT_TIME;
BOOL __DEBUG = 0;
#include "POUS.h"
#include "POUS.c"

int main(void) {{
    {ST_ROOT}_data__ fb = {{0}};
    {ST_ROOT}_init__(&fb, __BOOL_LITERAL(FALSE));
    for (int i = 0; i < {TICKS}; i++) {{
        {ST_ROOT}_body__(&fb);
{prints}
        printf("\n");
    }}
    return 0;
}}
"#
    );
    let harness_path = work.join("harness.c");
    std::fs::write(&harness_path, harness).expect("запись драйвера");

    let bin = work.join("composite_param_bin");
    let compile = Command::new("cc")
        .args(["-std=c99", "-w", "-I"])
        .arg(lib.join("C"))
        .arg("-I")
        .arg(&work)
        .arg(&harness_path)
        .arg("-o")
        .arg(&bin)
        .output()
        .expect("запуск cc");
    assert!(
        compile.status.success(),
        "порождённый ST (через iec2c) не собирается:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let run = Command::new(&bin).output().expect("запуск драйвера ST");
    assert!(run.status.success(), "драйвер ST завершился с ошибкой");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter(|l| !l.trim().is_empty())
        .map(|l| {
            l.split_whitespace()
                .map(|v| v.parse().expect("число в трассе"))
                .collect()
        })
        .collect()
}

/// Вложенный массив в параметре у цели `st`: `iec2c` принимает вывод, а значения
/// совпадают с эталоном.
///
/// До тест не доходил до сравнения: имя типа-формы содержало скобки и точки, и `iec2c`
/// отвергал файл на объявлении.
#[test]
fn composite_parameter_st_trace_matches_reference() {
    let Some((iec2c, lib)) = iec2c_available() else {
        eprintln!("iec2c не установлен — сверка пропущена (см. scripts/ensure-iec2c.sh)");
        return;
    };
    if !cc_available() {
        eprintln!("cc недоступен — сверка пропущена");
        return;
    }
    let dir = build_dir("composite_param_st");
    let reference = simulator_trace();
    let target = st_trace(&dir, &iec2c, &lib);
    assert_eq!(
        target.len(),
        reference.len(),
        "длина трасс:\nэталон {reference:?}\nst     {target:?}"
    );
    for (tick, (r, t)) in reference.iter().zip(target.iter()).enumerate() {
        assert_eq!(
            t,
            r,
            "такт {} разошёлся.\nнаблюдаемые: {:?}\nэталон: {reference:?}\nst:     {target:?}",
            tick + 1,
            OBSERVED.iter().map(|(n, _)| *n).collect::<Vec<_>>()
        );
    }
    let _ = std::fs::remove_dir_all(&dir);
}
