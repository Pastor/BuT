//! Обращение к члену результата вызова: поле структуры и разряд.
//!
//! # Решение
//!
//! Результат вызова поднимается во временную переменную **в семантике** (приём 0400,
//! общий модуль с 0431): за её границей формы не существует, и печатники целей не
//! трогаются вовсе.

use std::path::{Path, PathBuf};
use std::process::Command;
use takt_lang::semantic::tree::construct_model;
use takt_sim::{Value, build_unit};

const FIXTURE: &str = "tests/data/eval/conformance_call_result_field.takt";
const UNIT: &str = "callfield";
const TICKS: usize = 3;
const EXPECTED: [(i128, i128); TICKS] = [(11, 1), (12, 1), (13, 1)];

fn tool(bin: &str) -> bool {
    Command::new(bin)
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

fn build_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0432_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог сборки");
    dir
}

fn source() -> String {
    std::fs::read_to_string(FIXTURE).expect("фикстура читается")
}

fn simulator_trace() -> Vec<(i128, i128)> {
    let (ast, _) = takt_lang::parse(&source(), 0).expect("разбор фикстуры");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    let mut reg = (0i128, 0i128);
    for _ in 0..TICKS {
        let _ = unit.tick();
        if let Some(Value::Number(v)) = unit.variable("field") {
            reg.0 = v;
        }
        if let Some(Value::Number(v)) = unit.variable("bit0") {
            reg.1 = v;
        }
        trace.push(reg);
    }
    trace
}

fn parse_trace(text: &str, prefix: &str) -> Vec<(i128, i128)> {
    text.lines()
        .filter_map(|line| {
            let mut it = line.strip_prefix(prefix)?.split_whitespace();
            Some((
                it.next()?.parse::<i128>().ok()?,
                it.next()?.parse::<i128>().ok()?,
            ))
        })
        .collect()
}

fn rust_trace(dir: &Path) -> Vec<(i128, i128)> {
    takt_lang::compile_to_rust(
        UNIT,
        &source(),
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение Rust");
    let module = dir.join(format!("{UNIT}.rs"));
    let driver = format!(
        r#"#[path = "{module}"]
mod generated;
use generated::{{Callfield, Hal, OutU8Port}};
use std::cell::RefCell;
use std::rc::Rc;

struct Probe {{ reg: Rc<RefCell<[u8; 2]>> }}

impl Hal for Probe {{
    fn write_u8(&mut self, port: OutU8Port, value: u8) {{
        let idx = match port {{ OutU8Port::Field => 0, OutU8Port::Bit0 => 1 }};
        self.reg.borrow_mut()[idx] = value;
    }}
}}

fn main() {{
    let reg = Rc::new(RefCell::new([0u8; 2]));
    let mut model = Callfield::new(Probe {{ reg: Rc::clone(&reg) }});
    model.init();
    for _ in 0..{TICKS} {{
        model.tick();
        let r = reg.borrow();
        println!("{{}} {{}}", r[0], r[1]);
    }}
}}
"#,
        module = module.display(),
    );
    std::fs::write(dir.join("driver.rs"), driver).expect("драйвер");
    let build = Command::new("rustc")
        .current_dir(dir)
        .args(["--edition", "2021", "driver.rs", "-o", "driver"])
        .output()
        .expect("запуск rustc");
    assert!(
        build.status.success(),
        "rustc не собрал драйвер:\n{}",
        String::from_utf8_lossy(&build.stderr)
    );
    let run = Command::new(dir.join("driver"))
        .current_dir(dir)
        .output()
        .expect("запуск драйвера");
    parse_trace(&String::from_utf8_lossy(&run.stdout), "")
}

fn generate_sv(dir: &Path) {
    takt_lang::compile_to_sv(
        UNIT,
        &source(),
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение SystemVerilog");
}

fn sv_trace(dir: &Path) -> Vec<(i128, i128)> {
    generate_sv(dir);
    let tb = format!(
        r#"module tb;
    logic clk = 0, rst_n = 0;
    logic [7:0] field, bit0;
    logic is_done;
    {UNIT} dut (.clk(clk), .rst_n(rst_n), .field(field), .bit0(bit0), .is_done(is_done));
    always #5 clk = ~clk;
    initial begin
        @(posedge clk);
        rst_n <= 1'b1;
        for (int i = 0; i < {TICKS}; i++) begin
            @(posedge clk);
            #1 $display("TICK %0d %0d", field, bit0);
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
    parse_trace(&String::from_utf8_lossy(&run.stdout), "TICK ")
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

fn st_trace(dir: &Path, iec2c: &Path, lib: &Path) -> Vec<(i128, i128)> {
    let st_dir = dir.join("st");
    std::fs::create_dir_all(&st_dir).expect("каталог ST");
    takt_lang::compile_to_st(
        &format!("{UNIT}.takt"),
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
        "iec2c не оттранслировал порождённый ST:\n{}",
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
    CALLFIELD_data__ fb = {{0}};
    CALLFIELD_init__(&fb, __BOOL_LITERAL(FALSE));
    for (int i = 0; i < {TICKS}; i++) {{
        CALLFIELD_body__(&fb);
        printf("%lld %lld\n", (long long)fb.FIELD.value, (long long)fb.BIT0.value);
    }}
    return 0;
}}
"#
    );
    std::fs::write(work.join("harness.c"), harness).expect("драйвер");
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
        "порождённый ST не собирается:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск драйвера ST");
    parse_trace(&String::from_utf8_lossy(&run.stdout), "")
}

/// Значения эталона названы числами.
#[test]
fn reference_values_are_named() {
    assert_eq!(
        simulator_trace(),
        EXPECTED.to_vec(),
        "читаются оба члена результата: поле структуры и разряд"
    );
}

/// Прошивка цели `rust` считает то же.
#[test]
fn call_result_field_matches_generated_rust() {
    if !tool("rustc") {
        eprintln!("[ПРОПУСК] call_result_field_matches_generated_rust: нет rustc");
        return;
    }
    let dir = build_dir("rust");
    assert_eq!(simulator_trace(), rust_trace(&dir), "эталон против rust");
    let _ = std::fs::remove_dir_all(&dir);
}

/// RTL цели `sv` считает то же - прежде вывод не синтезировался.
#[test]
fn call_result_field_matches_generated_sv() {
    if !tool("verilator") {
        eprintln!("[ПРОПУСК] call_result_field_matches_generated_sv: нет verilator");
        return;
    }
    let dir = build_dir("sv");
    assert_eq!(simulator_trace(), sv_trace(&dir), "эталон против RTL sv");
    let _ = std::fs::remove_dir_all(&dir);
}

/// ПЛК цели `st` считает то же - прежде `iec2c` вывод отвергал.
#[test]
fn call_result_field_matches_generated_st() {
    let Some((iec2c, lib)) = iec2c_paths() else {
        eprintln!("[ПРОПУСК] call_result_field_matches_generated_st: нет iec2c");
        return;
    };
    if !tool("cc") {
        eprintln!("[ПРОПУСК] call_result_field_matches_generated_st: нет cc");
        return;
    }
    let dir = build_dir("st");
    assert_eq!(
        simulator_trace(),
        st_trace(&dir, &iec2c, &lib),
        "эталон против ПЛК st"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// Порождённый модуль синтезируется: индексацию результата вызова ловит yosys.
///
/// Именно здесь класс и жил вторым слоем: `pair(k)[1]` verilator принимает, а yosys
/// отвечает "syntax error, unexpected '['". Разворот во временную сделан в семантике
/// (приём 0400), поэтому форму видят все цели сразу.
#[test]
fn call_result_field_sv_is_synthesizable() {
    if !tool("yosys") {
        eprintln!("[ПРОПУСК] call_result_field_sv_is_synthesizable: нет yosys");
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

/// Цель `c` ту же запись переводит - контроль.
///
/// Он отделяет класс от соседнего (0431, где `c` отказывает `CC-015` на возврате
/// массива): здесь возврат скалярный и структурный, и отказа быть не должно ни у одной
/// цели.
#[test]
fn call_result_field_is_translated_by_c() {
    if !tool("cc") {
        eprintln!("[ПРОПУСК] call_result_field_is_translated_by_c: нет cc");
        return;
    }
    let dir = build_dir("c");
    takt_lang::compile_to_c(
        UNIT,
        &source(),
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("цель `c` обращение к члену результата переводит");
    let compile = Command::new("cc")
        .args([
            "-std=c11",
            "-Wall",
            "-Wextra",
            "-Wno-unused-parameter",
            "-Werror",
            "-c",
        ])
        .arg(dir.join(format!("{UNIT}.c")))
        .arg("-o")
        .arg(dir.join("obj.o"))
        .arg("-I")
        .arg(&dir)
        .output()
        .expect("запуск cc");
    assert!(
        compile.status.success(),
        "cc не принял вывод флагами гейта цели:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let _ = std::fs::remove_dir_all(&dir);
}
