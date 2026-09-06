//! Вложенная цепочка `+` внутри параллели - цели `st` и `sv`.
//!
//! # Исправлениетура
//!
//! `A + ((B + C) | D)` - вложенная цепочка стоит внутри параллели, которая
//! сама является шагом другой цепочки. Это разом проверяет адресацию по
//! **месту в дереве** (две цепочки одного состояния не должны делить машину
//! шагов) и терминальное состояние вложенной цепочки: выхода из неё нет,
//! готовность читает вмещающая параллель.

use std::path::{Path, PathBuf};
use std::process::Command;
use takt_lang::semantic::tree::construct_model;
use takt_sim::{Value, build_unit};

const FIXTURE: &str = "tests/data/eval/conformance_nested_chain.takt";
const UNIT: &str = "nestedchain";
/// Тактов в трассе: автомат завершается на девятом - хвост тоже сверяется.
const TICKS: usize = 9;
/// Имя корневой модели в C-символах `iec2c` (идентификаторы IEC печатаются в верхнем
/// регистре).
const ST_ROOT: &str = "NESTEDCHAIN";

/// Наблюдаемые порты и их поля у экземпляров, порождённых целью `st`.
///
/// Приставка `ALL_` - у экземпляров внутри цепочки (форма снята зондом цели `c`); здесь
/// её несут все четыре, потому что параллель `(B + C) | D` сама стоит шагом цепочки.
const ST_FIELDS: [&str; 4] = ["ALL_A0.OA", "ALL_B1.OB", "ALL_C2.OC", "ALL_D3.OD"];

fn tool(bin: &str) -> bool {
    Command::new(bin)
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
        .join(format!("takt_0427_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог сборки");
    dir
}

fn iec2c_paths() -> Option<(PathBuf, PathBuf)> {
    let prefix = std::env::var("IEC2C_PREFIX").map_or_else(
        |_| PathBuf::from(std::env::var("HOME").unwrap_or_default()).join(".local"),
        PathBuf::from,
    );
    let bin = prefix.join("bin").join("iec2c");
    let lib = prefix.join("share").join("matiec").join("lib");
    (bin.is_file() && lib.join("C").is_dir()).then_some((bin, lib))
}

/// Трасса эталона: `(oa, ob, oc, od)` по тактам.
///
/// Наблюдение через регистр, а не прямым чтением: `unit.variable` отдаёт порт только у
/// активной ветви, а завершившаяся из ответа исчезает.
fn simulator_trace() -> Vec<[i128; 4]> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор фикстуры");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    let mut reg = [0i128; 4];
    for _ in 0..TICKS {
        let _ = unit.tick();
        for (idx, name) in ["oa", "ob", "oc", "od"].iter().enumerate() {
            if let Some(Value::Number(v)) = unit.variable(name) {
                reg[idx] = v;
            }
        }
        trace.push(reg);
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
fn sv_trace(dir: &Path) -> Vec<[i128; 4]> {
    generate_sv(dir);
    let tb = format!(
        r#"module tb;
    logic clk = 0, rst_n = 0;
    logic [7:0] oa, ob, oc, od;
    logic is_done;
    {UNIT} dut (.clk(clk), .rst_n(rst_n), .oa(oa), .ob(ob), .oc(oc), .od(od), .is_done(is_done));
    always #5 clk = ~clk;
    initial begin
        @(posedge clk);
        rst_n <= 1'b1;
        for (int i = 0; i < {TICKS}; i++) begin
            @(posedge clk);
            #1 $display("TICK %0d %0d %0d %0d", oa, ob, oc, od);
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
    parse_trace(&String::from_utf8_lossy(&run.stdout), "TICK ")
}

/// Трасса ПЛК: тот же ST, оттранслированный `iec2c` и исполненный.
fn st_trace(dir: &Path, iec2c: &Path, lib: &Path) -> Vec<[i128; 4]> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    let st_dir = dir.join("st");
    std::fs::create_dir_all(&st_dir).expect("каталог ST");
    takt_lang::compile_to_st(
        &format!("{UNIT}.takt"),
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
        .arg(st_dir.join(format!("{UNIT}.st")))
        .current_dir(&work)
        .output()
        .expect("запуск iec2c");
    assert!(
        transpile.status.success() && work.join("POUS.c").is_file(),
        "iec2c не оттранслировал порождённый ST:\n{}",
        String::from_utf8_lossy(&transpile.stderr)
    );

    let prints = ST_FIELDS
        .iter()
        .map(|path| format!(r#"        printf("%lld ", (long long)fb.{path}.value);"#))
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
    std::fs::write(work.join("harness.c"), harness).expect("запись драйвера");
    let bin = work.join("st_bin");
    let compile = Command::new("cc")
        .args(["-std=c99", "-w", "-I"])
        .arg(lib.join("C"))
        .arg("-I")
        .arg(&work)
        .arg(work.join("harness.c"))
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
    parse_trace(&String::from_utf8_lossy(&run.stdout), "")
}

fn parse_trace(text: &str, prefix: &str) -> Vec<[i128; 4]> {
    text.lines()
        .filter_map(|line| {
            let rest = line.strip_prefix(prefix)?;
            let mut it = rest.split_whitespace();
            let mut row = [0i128; 4];
            for cell in &mut row {
                *cell = it.next()?.parse().ok()?;
            }
            Some(row)
        })
        .collect()
}

/// Значения эталона названы числами: A идёт первым, затем B->C параллельно D.
#[test]
fn reference_values_are_named() {
    assert_eq!(
        simulator_trace(),
        vec![
            [1, 0, 0, 0],
            [2, 0, 0, 0],
            [2, 0, 0, 0],
            [2, 2, 0, 4],
            [2, 4, 0, 8],
            [2, 4, 0, 8],
            [2, 4, 3, 8],
            [2, 4, 6, 8],
            [2, 4, 9, 8],
        ],
        "A завершается на такте 2; B и D идут параллельно, C — после B"
    );
}

/// ПЛК цели `st` считает то же, что эталон.
#[test]
fn nested_chain_matches_generated_st() {
    let Some((iec2c, lib)) = iec2c_paths() else {
        eprintln!("[ПРОПУСК] nested_chain_matches_generated_st: нет iec2c");
        return;
    };
    if !tool("cc") {
        eprintln!("[ПРОПУСК] nested_chain_matches_generated_st: нет cc");
        return;
    }
    let dir = build_dir("st");
    let sim = simulator_trace();
    let plc = st_trace(&dir, &iec2c, &lib);
    assert_eq!(sim, plc, "трассы эталона и ПЛК обязаны совпадать");
    let _ = std::fs::remove_dir_all(&dir);
}

/// RTL цели `sv` считает то же, что эталон.
#[test]
fn nested_chain_matches_generated_sv() {
    if !tool("verilator") {
        eprintln!("[ПРОПУСК] nested_chain_matches_generated_sv: нет verilator");
        return;
    }
    let dir = build_dir("sv");
    let sim = simulator_trace();
    let rtl = sv_trace(&dir);
    assert_eq!(sim, rtl, "трассы эталона и RTL обязаны совпадать");
    let _ = std::fs::remove_dir_all(&dir);
}

/// Порождённый модуль проходит линт цели (флаги её проверки).
///
/// Сверка значений этого не видит: тестбенч идёт с `-Wno-fatal`, и предупреждение линта -
/// которое проверка цели считает ошибкой - там молчит.
#[test]
fn nested_chain_sv_passes_target_lint() {
    if !tool("verilator") {
        eprintln!("[ПРОПУСК] nested_chain_sv_passes_target_lint: нет verilator");
        return;
    }
    let dir = build_dir("sv_lint");
    generate_sv(&dir);
    let lint = Command::new("verilator")
        .current_dir(&dir)
        .args([
            "--lint-only",
            "-Wall",
            "--top-module",
            UNIT,
            &format!("{UNIT}.sv"),
        ])
        .output()
        .expect("запуск verilator");
    assert!(
        lint.status.success(),
        "порождённый SystemVerilog не проходит линт цели:\n{}",
        String::from_utf8_lossy(&lint.stderr)
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// Порождённый модуль синтезируется: verilator этого не доказывает.
///
/// Два инструмента обязательны: комбинационную петлю и защёлку yosys синтезирует молча,
/// а verilator их ловит - и наоборот, распакованные размерности портов verilator
/// принимает, yosys нет.
#[test]
fn nested_chain_sv_is_synthesizable() {
    if !tool("yosys") {
        eprintln!("[ПРОПУСК] nested_chain_sv_is_synthesizable: нет yosys");
        return;
    }
    let dir = build_dir("sv_synth");
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
