//! Момент готовности вложенной цепочки - все четыре исполняемые цели.

use std::path::{Path, PathBuf};
use std::process::Command;
use takt_lang::semantic::tree::construct_model;
use takt_sim::{Value, build_unit};

const FIXTURE: &str = "tests/data/eval/conformance_nested_ready.takt";
const UNIT: &str = "nestedready";
/// Тактов в трассе: автомат завершается на десятом - хвост тоже сверяется.
const TICKS: usize = 10;
/// Имя корневой модели в C-символах `iec2c` (идентификаторы IEC печатаются в верхнем
/// регистре).
const ST_ROOT: &str = "NESTEDREADY";
/// Наблюдаемые порты и их поля у экземпляров, порождённых целью `st`.
const ST_FIELDS: [&str; 4] = ["ALL_A0.OA", "ALL_B1.OB", "ALL_C2.OC", "ALL_E3.OE"];
/// Наблюдаемые порты - в одном порядке у всех потребителей.
const PORTS: [&str; 4] = ["oa", "ob", "oc", "oe"];

fn tool(bin: &str) -> bool {
    Command::new(bin)
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Каталог сборки уникален по потоку И процессу.
fn build_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0479_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог сборки");
    dir
}

fn iec2c_paths() -> Option<(PathBuf, PathBuf)> {
    let prefix = std::env::var("IEC2C_PREFIX").map_or_else(
        |_| {
            std::env::var("HOME")
                .map(PathBuf::from)
                .unwrap_or_default()
                .join(".local")
        },
        PathBuf::from,
    );
    let bin = prefix.join("bin").join("iec2c");
    let lib = prefix.join("share").join("matiec").join("lib");
    (bin.is_file() && lib.is_dir()).then_some((bin, lib))
}

/// Трасса эталона: значения портов по тактам.
///
/// Наблюдение через регистр, а не через прямое чтение: завершившаяся ветвь из ответа
/// `unit.variable` исчезает, а на плате регистр держит последнее записанное значение -
/// так же его наблюдают харнессы целей.
fn simulator_trace() -> Vec<[i128; 4]> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор фикстуры");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    let mut reg = [0i128; 4];
    for _ in 0..TICKS {
        let _ = unit.tick();
        for (idx, name) in PORTS.iter().enumerate() {
            if let Some(Value::Number(v)) = unit.variable(name) {
                reg[idx] = v;
            }
        }
        trace.push(reg);
    }
    trace
}

/// Трасса прошивки цели `c`.
fn c_trace(dir: &Path) -> Vec<[i128; 4]> {
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

static long long reg[4] = {{0, 0, 0, 0}};

static void on_write(Nestedready_Out_NumericPort port, uint8_t index, int64_t value, void *ud) {{
    (void)index;
    (void)ud;
    reg[(int)port] = (long long)value;
}}

int main(void) {{
    Nestedready m;
    Nestedready_init(&m);
    m.write_numeric = on_write;
    m.userdata = 0;
    for (int i = 0; i < {TICKS}; i++) {{
        Nestedready_tick(&m);
        printf("%lld %lld %lld %lld\n", reg[0], reg[1], reg[2], reg[3]);
    }}
    return 0;
}}
"#
    );
    std::fs::write(dir.join("harness.c"), &harness).expect("запись харнесса");
    let bin = dir.join("c_bin");
    // Флаги - те же, что у проверки цели `c` в предкоммите.
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
    assert!(run.status.success(), "харнесс завершился с ошибкой");
    parse_trace(&String::from_utf8_lossy(&run.stdout), "")
}

/// Трасса прошивки цели `rust`.
fn rust_trace(dir: &Path) -> Vec<[i128; 4]> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_rust(
        UNIT,
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение Rust");

    let module = dir.join(format!("{UNIT}.rs"));
    let driver = format!(
        r#"#[path = "{module}"]
mod generated;
use generated::{{Hal, Nestedready, OutU8Port}};
use std::cell::RefCell;
use std::rc::Rc;

struct Probe {{ reg: Rc<RefCell<[u8; 4]>> }}

impl Hal for Probe {{
    fn write_u8(&mut self, port: OutU8Port, value: u8) {{
        let idx = match port {{
            OutU8Port::Oa => 0,
            OutU8Port::Ob => 1,
            OutU8Port::Oc => 2,
            OutU8Port::Oe => 3,
        }};
        self.reg.borrow_mut()[idx] = value;
    }}
}}

fn main() {{
    let reg = Rc::new(RefCell::new([0u8; 4]));
    let mut m = Nestedready::new(Probe {{ reg: reg.clone() }});
    m.init();
    for _ in 0..{TICKS} {{
        m.tick();
        let r = reg.borrow();
        println!("{{}} {{}} {{}} {{}}", r[0], r[1], r[2], r[3]);
    }}
}}
"#,
        module = module.display()
    );
    std::fs::write(dir.join("driver.rs"), driver).expect("запись драйвера");
    let bin = dir.join("rust_bin");
    let compile = Command::new("rustc")
        .args(["--edition", "2021", "-A", "warnings", "-o"])
        .arg(&bin)
        .arg(dir.join("driver.rs"))
        .output()
        .expect("запуск rustc");
    assert!(
        compile.status.success(),
        "rustc не собрал драйвер:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск драйвера");
    assert!(run.status.success(), "драйвер завершился с ошибкой");
    parse_trace(&String::from_utf8_lossy(&run.stdout), "")
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
    logic [7:0] oa, ob, oc, oe;
    logic is_done;
    {UNIT} dut (.clk(clk), .rst_n(rst_n), .oa(oa), .ob(ob), .oc(oc), .oe(oe), .is_done(is_done));
    always #5 clk = ~clk;
    initial begin
        @(posedge clk);
        rst_n <= 1'b1;
        for (int i = 0; i < {TICKS}; i++) begin
            @(posedge clk);
            #1 $display("TICK %0d %0d %0d %0d", oa, ob, oc, oe);
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
            // `-j 4`, а не `-j 0`: тестов с verilator три десятка, и cargo гоняет их
            // параллельно (флак ).
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

/// Значения эталона названы числами: `A` идёт первым, `C` - параллельно цепочке, `E`
/// начинает считать только после её готовности.
#[test]
fn reference_values_are_named() {
    assert_eq!(
        simulator_trace(),
        vec![
            [1, 0, 3, 0],
            [2, 0, 6, 0],
            [2, 0, 9, 0],
            [2, 2, 9, 0],
            [2, 4, 9, 0],
            [2, 4, 9, 0],
            [2, 4, 9, 1],
            [2, 4, 9, 2],
            [2, 4, 9, 3],
            [2, 4, 9, 3],
        ],
        "B стартует только после A (такт 3), E — только после готовности параллели"
    );
}

/// Прошивка цели `c` считает то же, что эталон.
#[test]
fn nested_ready_matches_generated_c() {
    if !tool("cc") {
        eprintln!("[ПРОПУСК] nested_ready_matches_generated_c: нет cc");
        return;
    }
    let dir = build_dir("c");
    let sim = simulator_trace();
    let firmware = c_trace(&dir);
    assert_eq!(sim, firmware, "трассы эталона и прошивки обязаны совпадать");
    let _ = std::fs::remove_dir_all(&dir);
}

/// Прошивка цели `rust` считает то же, что эталон.
///
/// Ради этой проверки фича и заведена: до неё цепочка `A + B` тикала параллелью, а
/// `clippy -D warnings` такой вывод принимал.
#[test]
fn nested_ready_matches_generated_rust() {
    if !tool("rustc") {
        eprintln!("[ПРОПУСК] nested_ready_matches_generated_rust: нет rustc");
        return;
    }
    let dir = build_dir("rust");
    let sim = simulator_trace();
    let firmware = rust_trace(&dir);
    assert_eq!(sim, firmware, "трассы эталона и прошивки обязаны совпадать");
    let _ = std::fs::remove_dir_all(&dir);
}

/// ПЛК цели `st` считает то же, что эталон.
#[test]
fn nested_ready_matches_generated_st() {
    let Some((iec2c, lib)) = iec2c_paths() else {
        eprintln!("[ПРОПУСК] nested_ready_matches_generated_st: нет iec2c");
        return;
    };
    if !tool("cc") {
        eprintln!("[ПРОПУСК] nested_ready_matches_generated_st: нет cc");
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
fn nested_ready_matches_generated_sv() {
    if !tool("verilator") {
        eprintln!("[ПРОПУСК] nested_ready_matches_generated_sv: нет verilator");
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
fn nested_ready_sv_passes_target_lint() {
    if !tool("verilator") {
        eprintln!("[ПРОПУСК] nested_ready_sv_passes_target_lint: нет verilator");
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
        "verilator -Wall не принял модуль:\n{}",
        String::from_utf8_lossy(&lint.stderr)
    );
    let _ = std::fs::remove_dir_all(&dir);
}
