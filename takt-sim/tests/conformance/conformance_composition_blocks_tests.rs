//! Блоки состояния-Композиции: `always` и `exit`.

use std::path::{Path, PathBuf};
use std::process::Command;
use takt_lang::semantic::tree::construct_model;
use takt_sim::{Value, build_unit};

const FIXTURE: &str = "tests/data/eval/conformance_composition_blocks.takt";
const UNIT: &str = "compblocks";
const TICKS: usize = 6;
/// Ожидание: `always` даёт 1, 2, 3, 4-й такт - и `exit` добавляет 10 однажды.
// Значения изменились с: у состояния-композиции `All` есть тело, и потому автомат в нём
// Остаётся, когда композиция отработала, - `always` продолжает считать (4, 5, 6), а
// `exit` не наступает вовсе.
const EXPECTED: [i128; TICKS] = [1, 2, 3, 4, 5, 6];

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
        .join(format!("takt_0430_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог сборки");
    dir
}

fn source() -> String {
    std::fs::read_to_string(FIXTURE).expect("фикстура читается")
}

/// Трасса эталона: наблюдаемая - порт, через регистр (последнее записанное).
fn simulator_trace() -> Vec<i128> {
    let (ast, _) = takt_lang::parse(&source(), 0).expect("разбор фикстуры");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    let mut reg = 0i128;
    for _ in 0..TICKS {
        let _ = unit.tick();
        if let Some(Value::Number(v)) = unit.variable("probe") {
            reg = v;
        }
        trace.push(reg);
    }
    trace
}

fn parse_trace(text: &str, prefix: &str) -> Vec<i128> {
    text.lines()
        .filter_map(|line| line.strip_prefix(prefix)?.trim().parse::<i128>().ok())
        .collect()
}

/// Трасса прошивки цели `c`.
fn c_trace(dir: &Path) -> Vec<i128> {
    takt_lang::compile_to_c(
        UNIT,
        &source(),
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");
    let harness = format!(
        r#"#include <stdio.h>
#include "{UNIT}.h"
static long long reg;
static void on_write(Compblocks_Out_NumericPort p, uint8_t index, int64_t v, void *u) {{
    (void)index;
    (void)p; (void)u; reg = (long long)v;
}}
int main(void) {{
    Compblocks m;
    Compblocks_init(&m);
    m.write_numeric = on_write;
    m.userdata = 0;
    for (int i = 0; i < {TICKS}; i++) {{
        Compblocks_tick(&m);
        printf("%lld\n", reg);
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

/// Трасса прошивки цели `rust`.
fn rust_trace(dir: &Path) -> Vec<i128> {
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
use generated::{{Compblocks, Hal, OutU8Port}};
use std::cell::RefCell;
use std::rc::Rc;

struct Probe {{ reg: Rc<RefCell<u8>> }}

impl Hal for Probe {{
    fn write_u8(&mut self, _port: OutU8Port, value: u8) {{
        *self.reg.borrow_mut() = value;
    }}
}}

fn main() {{
    let reg = Rc::new(RefCell::new(0u8));
    let mut model = Compblocks::new(Probe {{ reg: Rc::clone(&reg) }});
    model.init();
    for _ in 0..{TICKS} {{
        model.tick();
        println!("{{}}", reg.borrow());
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

/// Трасса RTL цели `sv`.
fn sv_trace(dir: &Path) -> Vec<i128> {
    takt_lang::compile_to_sv(
        UNIT,
        &source(),
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение SystemVerilog");
    let tb = format!(
        r#"module tb;
    logic clk = 0, rst_n = 0;
    logic [7:0] probe;
    logic is_done;
    {UNIT} dut (.clk(clk), .rst_n(rst_n), .probe(probe), .is_done(is_done));
    always #5 clk = ~clk;
    initial begin
        @(posedge clk);
        rst_n <= 1'b1;
        for (int i = 0; i < {TICKS}; i++) begin
            @(posedge clk);
            #1 $display("TICK %0d", probe);
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

/// Трасса ПЛК цели `st` (через `iec2c`).
fn st_trace(dir: &Path, iec2c: &Path, lib: &Path) -> Vec<i128> {
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
    COMPBLOCKS_data__ fb = {{0}};
    COMPBLOCKS_init__(&fb, __BOOL_LITERAL(FALSE));
    for (int i = 0; i < {TICKS}; i++) {{
        COMPBLOCKS_body__(&fb);
        printf("%lld\n", (long long)fb.PROBE.value);
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

fn iec2c_paths() -> Option<(PathBuf, PathBuf)> {
    let prefix = std::env::var("IEC2C_PREFIX").map_or_else(
        |_| PathBuf::from(std::env::var("HOME").unwrap_or_default()).join(".local"),
        PathBuf::from,
    );
    let bin = prefix.join("bin").join("iec2c");
    let lib = prefix.join("share").join("matiec").join("lib");
    (bin.is_file() && lib.join("C").is_dir()).then_some((bin, lib))
}

/// Значения эталона названы числами.
#[test]
fn reference_values_are_named() {
    assert_eq!(
        simulator_trace(),
        EXPECTED.to_vec(),
        "`always` исполняется каждый такт, и состояние с телом не завершается"
    );
}

/// Прошивка цели `c` считает то же.
#[test]
fn composition_blocks_match_generated_c() {
    if !tool("cc") {
        eprintln!("[ПРОПУСК] composition_blocks_match_generated_c: нет cc");
        return;
    }
    let dir = build_dir("c");
    assert_eq!(simulator_trace(), c_trace(&dir), "эталон против прошивки c");
    let _ = std::fs::remove_dir_all(&dir);
}

/// Прошивка цели `rust` считает то же.
#[test]
fn composition_blocks_match_generated_rust() {
    if !tool("rustc") {
        eprintln!("[ПРОПУСК] composition_blocks_match_generated_rust: нет rustc");
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

/// RTL цели `sv` считает то же - без правки `exit` печатается дважды.
#[test]
fn composition_blocks_match_generated_sv() {
    if !tool("verilator") {
        eprintln!("[ПРОПУСК] composition_blocks_match_generated_sv: нет verilator");
        return;
    }
    let dir = build_dir("sv");
    assert_eq!(simulator_trace(), sv_trace(&dir), "эталон против RTL sv");
    let _ = std::fs::remove_dir_all(&dir);
}

/// ПЛК цели `st` считает то же - без правки `exit` теряется.
#[test]
fn composition_blocks_match_generated_st() {
    let Some((iec2c, lib)) = iec2c_paths() else {
        eprintln!("[ПРОПУСК] composition_blocks_match_generated_st: нет iec2c");
        return;
    };
    if !tool("cc") {
        eprintln!("[ПРОПУСК] composition_blocks_match_generated_st: нет cc");
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

/// Простое терминальное состояние: `exit` исполняется и там - контроль.
///
/// Без него правка выглядела бы "про композицию", тогда как эталон не исполнял `exit`
/// при завершении **нигде**: замер дал `hits = 1` против `11` у всех четырёх целей.
#[test]
fn plain_terminal_state_runs_exit_too() {
    // Терминально состояние без тела: `exit` телом не считается - он про уход, а не про
    // пребывание, и состояние с одним лишь `exit` остаётся концом. Иначе его блок стал
    // бы мёртвым кодом.
    let src = "var hits: u8 := 0;\n\
               out probe: u8 at 0x100;\n\
               start Go {\n\
                   ref Stop: true;\n\
               }\n\
               state Stop {\n\
                   exit { hits := hits + 10; probe := hits; }\n\
               }\n";
    let (ast, _) = takt_lang::parse(src, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение Unit");
    let mut reg = 0i128;
    for _ in 0..3 {
        let _ = unit.tick();
        if let Some(Value::Number(v)) = unit.variable("probe") {
            reg = v;
        }
    }
    assert_eq!(reg, 10, "уход в терминал — тоже выход из состояния");
}

/// Контроль к предыдущему: состояние С телом автомат не завершает, и `exit` в нём не
/// наступает вовсе.
///
/// Без этой проверки правило было бы декоративным: `plain_terminal_state...` один
/// держит только половину - "пустое состояние кончает", - а вторая половина ("состояние
/// с телом работает вечно") осталась бы без свидетеля.
#[test]
fn a_state_with_a_body_keeps_running_and_never_exits() {
    let src = "var hits: u8 := 0;\n\
               out probe: u8 at 0x100;\n\
               start Go {\n\
                   always { hits := hits + 1; probe := hits; }\n\
                   exit { probe := 99; }\n\
               }\n";
    let (ast, _) = takt_lang::parse(src, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение Unit");
    let mut reg = 0i128;
    for _ in 0..4 {
        let _ = unit.tick();
        if let Some(Value::Number(v)) = unit.variable("probe") {
            reg = v;
        }
    }
    assert_eq!(reg, 4, "`always` обязан идти каждый такт, а не однажды");
    assert!(
        !unit.is_terminal(),
        "состояние с телом автомат не завершает"
    );
}
