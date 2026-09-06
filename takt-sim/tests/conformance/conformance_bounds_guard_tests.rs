//! Guard границ массива по флагу.
//!
//! #
//!
//! Guard **сообщает наружу** (синтетический выходной порт `bounds_fault`) и операцию не
//! выполняет; флаг **выключен по умолчанию**.
//!
//! # Что доказывает набор
//!
//! При включённом guard все пятеро дают одну трассу: `2 4 6 6 6` со срабатыванием
//! признака с четвёртого такта. Без флага поведение прежнее - на это стоит отдельная
//! проверка (эталон отвечает `SIM-010`).

use std::path::{Path, PathBuf};
use std::process::Command;
use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Value, build_unit};

const FIXTURE: &str = "tests/data/eval/conformance_bounds_guard.takt";
const UNIT: &str = "boundsguard";
const TICKS: usize = 5;
/// `(probe, bounds_fault)` по тактам: доступ за границей не выполняется.
const EXPECTED: [(i128, i128); TICKS] = [(2, 0), (4, 0), (6, 0), (6, 1), (6, 1)];

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
        .join(format!("takt_0433_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог сборки");
    dir
}

fn source() -> String {
    std::fs::read_to_string(FIXTURE).expect("фикстура читается")
}

/// Опции цели с включённым guard.
fn options() -> takt_lang::generator::GenerateOptions {
    let mut opts = takt_lang::generator::GenerateOptions::default();
    opts.bounds_check = true;
    opts
}

/// Трасса эталона с guard.
fn simulator_trace() -> Vec<(i128, i128)> {
    let (ast, _) = takt_lang::parse(&source(), 0).expect("разбор фикстуры");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    takt_lang::semantic::bounds_guard::insert_bounds_guards(&model);
    let mut unit = build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    let mut reg = (0i128, 0i128);
    for _ in 0..TICKS {
        let result = unit.tick();
        assert!(
            !matches!(result, TickResult::Failed(_)),
            "с guard прогон не обрывается: {result:?}"
        );
        if let Some(Value::Number(v)) = unit.variable("probe") {
            reg.0 = v;
        }
        match unit.variable("bounds_fault") {
            Some(Value::Number(v)) => reg.1 = v,
            Some(Value::Boolean(b)) => reg.1 = i128::from(b),
            _ => {}
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

fn c_trace(dir: &Path) -> Vec<(i128, i128)> {
    takt_lang::compile_to_c(
        UNIT,
        &source(),
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &options(),
    )
    .expect("порождение C");
    let harness = format!(
        r#"#include <stdio.h>
#include <stdbool.h>
#include "{UNIT}.h"
static long long num, fault;
static void on_num(Boundsguard_Out_NumericPort p, uint8_t index, int64_t v, void *u) {{
    (void)index;
    (void)p; (void)u; num = (long long)v;
}}
static void on_bit(Boundsguard_Out_BitPort p, uint8_t bit, bool v, void *u) {{
    (void)bit;
    (void)p; (void)u; fault = v ? 1 : 0;
}}
int main(void) {{
    Boundsguard m;
    Boundsguard_init(&m);
    m.write_numeric = on_num;
    m.write_bit = on_bit;
    m.userdata = 0;
    for (int i = 0; i < {TICKS}; i++) {{
        Boundsguard_tick(&m);
        printf("%lld %lld\n", num, fault);
    }}
    return 0;
}}
"#
    );
    std::fs::write(dir.join("harness.c"), harness).expect("харнесс");
    let bin = dir.join("bin");
    let compile = Command::new("cc")
        .args([
            "-std=c11",
            "-Wall",
            "-Wextra",
            "-Wno-unused-parameter",
            "-Werror",
            "-o",
        ])
        .arg(&bin)
        .arg(dir.join("harness.c"))
        .arg(dir.join(format!("{UNIT}.c")))
        .arg("-I")
        .arg(dir)
        .output()
        .expect("запуск cc");
    assert!(
        compile.status.success(),
        "cc не собрал харнесс флагами гейта цели:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск харнесса");
    parse_trace(&String::from_utf8_lossy(&run.stdout), "")
}

fn rust_trace(dir: &Path) -> Vec<(i128, i128)> {
    takt_lang::compile_to_rust(
        UNIT,
        &source(),
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &options(),
    )
    .expect("порождение Rust");
    let module = dir.join(format!("{UNIT}.rs"));
    let driver = format!(
        r#"#[path = "{module}"]
mod generated;
use generated::{{Boundsguard, Hal, OutBitPort, OutU8Port}};
use std::cell::RefCell;
use std::rc::Rc;

struct Probe {{ reg: Rc<RefCell<(u8, u8)>> }}

impl Hal for Probe {{
    fn write_u8(&mut self, _port: OutU8Port, value: u8) {{ self.reg.borrow_mut().0 = value; }}
    fn write_bit(&mut self, _port: OutBitPort, value: bool) {{
        self.reg.borrow_mut().1 = u8::from(value);
    }}
}}

fn main() {{
    let reg = Rc::new(RefCell::new((0u8, 0u8)));
    let mut model = Boundsguard::new(Probe {{ reg: Rc::clone(&reg) }});
    model.init();
    for _ in 0..{TICKS} {{
        model.tick();
        let r = reg.borrow();
        println!("{{}} {{}}", r.0, r.1);
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

fn sv_trace(dir: &Path) -> Vec<(i128, i128)> {
    takt_lang::compile_to_sv(
        UNIT,
        &source(),
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &options(),
    )
    .expect("порождение SystemVerilog");
    let tb = format!(
        r#"module tb;
    logic clk = 0, rst_n = 0;
    logic [7:0] probe;
    logic bounds_fault, is_done;
    {UNIT} dut (.clk(clk), .rst_n(rst_n), .probe(probe),
                .bounds_fault(bounds_fault), .is_done(is_done));
    always #5 clk = ~clk;
    initial begin
        @(posedge clk);
        rst_n <= 1'b1;
        for (int i = 0; i < {TICKS}; i++) begin
            @(posedge clk);
            #1 $display("TICK %0d %0d", probe, bounds_fault);
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
        &options(),
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
    BOUNDSGUARD_data__ fb = {{0}};
    BOUNDSGUARD_init__(&fb, __BOOL_LITERAL(FALSE));
    for (int i = 0; i < {TICKS}; i++) {{
        BOUNDSGUARD_body__(&fb);
        printf("%lld %lld\n", (long long)fb.PROBE.value, (long long)fb.BOUNDS_FAULT.value);
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

/// Значения эталона с guard названы числами.
#[test]
fn reference_values_are_named() {
    assert_eq!(
        simulator_trace(),
        EXPECTED.to_vec(),
        "доступ за границей не выполняется, признак поднимается"
    );
}

/// **Граница:** Без флага поведение прежнее - прогон обрывается `SIM-010`.
///
/// Проверка обязательна: умолчание выбрано, и молчаливое включение guard изменило бы
/// вывод всего корпуса.
#[test]
fn without_the_flag_the_reference_still_stops() {
    let (ast, _) = takt_lang::parse(&source(), 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение Unit");
    let mut failed = None;
    for _ in 0..TICKS {
        if let TickResult::Failed(msg) = unit.tick() {
            failed = Some(msg);
            break;
        }
    }
    let msg = failed.expect("без guard эталон обязан остановиться");
    assert!(msg.contains("SIM-010"), "{msg}");
}

/// Прошивка цели `c` считает то же.
#[test]
fn bounds_guard_matches_generated_c() {
    if !tool("cc") {
        eprintln!("[ПРОПУСК] bounds_guard_matches_generated_c: нет cc");
        return;
    }
    let dir = build_dir("c");
    assert_eq!(simulator_trace(), c_trace(&dir), "эталон против прошивки c");
    let _ = std::fs::remove_dir_all(&dir);
}

/// Прошивка цели `rust` считает то же.
#[test]
fn bounds_guard_matches_generated_rust() {
    if !tool("rustc") {
        eprintln!("[ПРОПУСК] bounds_guard_matches_generated_rust: нет rustc");
        return;
    }
    let dir = build_dir("rust");
    assert_eq!(
        simulator_trace(),
        rust_trace(&dir),
        "эталон против прошивки rust"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// RTL цели `sv` считает то же.
#[test]
fn bounds_guard_matches_generated_sv() {
    if !tool("verilator") {
        eprintln!("[ПРОПУСК] bounds_guard_matches_generated_sv: нет verilator");
        return;
    }
    let dir = build_dir("sv");
    assert_eq!(simulator_trace(), sv_trace(&dir), "эталон против RTL sv");
    let _ = std::fs::remove_dir_all(&dir);
}

/// ПЛК цели `st` считает то же.
#[test]
fn bounds_guard_matches_generated_st() {
    let Some((iec2c, lib)) = iec2c_paths() else {
        eprintln!("[ПРОПУСК] bounds_guard_matches_generated_st: нет iec2c");
        return;
    };
    if !tool("cc") {
        eprintln!("[ПРОПУСК] bounds_guard_matches_generated_st: нет cc");
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

/// **Граница:** без флага порт `bounds_fault` в выводе не появляется.
///
/// Иначе включение фичи меняло бы интерфейс модуля у всех, кто её не просил, - а вывод
/// корпуса обязан остаться прежним байт-в-байт.
#[test]
fn without_the_flag_no_fault_port_is_emitted() {
    let dir = build_dir("noflag");
    takt_lang::compile_to_c(
        UNIT,
        &source(),
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");
    let header = std::fs::read_to_string(dir.join(format!("{UNIT}.h"))).expect("заголовок");
    assert!(
        !header.contains("BOUNDS_FAULT"),
        "без флага порта-признака быть не должно:\n{header}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}
