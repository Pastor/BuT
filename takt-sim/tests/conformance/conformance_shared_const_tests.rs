//! Одноимённые константы разных моделей: эталон == цели `rust` и `sv`.
//!
//! # Что наблюдается
//!
//! Два выходных порта: `a` пишет модель `A` (её `K` = 2), `b` - модель `B` (её `K` =
//! 3). Слияние констант наблюдаемо **сразу**: у `b` появляется значение 2 вместо 3.
//! Мутационная проверка (вернуть голое имя константы в `rust_expr::const_ident` /
//! `sv_expr::const_signal`) роняет обе сверки.
//!
//! # Мягкая деградация
//!
//! Нет `rustc` или `verilator` - соответствующая половина **пропускается с сообщением**
//! (образец - `conformance_rust_tests`, `conformance_sv_tests`). Трасса эталона
//! проверяется всегда.

use std::path::{Path, PathBuf};
use std::process::Command;
use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Unit, Value, build_unit};

/// Тактов в трассе.
const TICKS: usize = 3;

/// Две модели, у каждой своя константа `K`; каждая пишет в свой порт.
const DUP: &str = "out a: u8;\n\
                   out b: u8;\n\
                   model A { const K: u8 := 2; start Run { always { a := K; } ref Run; } }\n\
                   model B { const K: u8 := 3; start Run { always { b := K; } ref Run; } }\n\
                   start Main = A | B;\n";

fn tool_available(tool: &str) -> bool {
    Command::new(tool)
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

fn sim_value(unit: &Unit, name: &str) -> i128 {
    match unit.variable(name) {
        Some(Value::Number(n)) => n,
        Some(Value::Boolean(b)) => i128::from(b),
        other => panic!("порт '{name}': неожиданное значение {other:?}"),
    }
}

/// Каталог сборки, уникальный по тесту.
///
/// Ключ - имя потока: тесты идут параллельно, а каждый прогон начинается с
/// `remove_dir_all`. Общий каталог здесь означал бы, что один тест сносит вывод другого
/// прямо во время сборки.
fn build_dir(tag: &str) -> PathBuf {
    // `:` вычищается - см. имя теста после слияния целей несёт префикс модуля, и
    // двоеточие попало бы в путь.
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0193_conformance_{thread}_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог сборки");
    dir
}

fn fixture(dir: &Path, name: &str, source: &str) -> PathBuf {
    let path = dir.join(format!("{name}.takt"));
    std::fs::write(&path, source).expect("запись фикстуры");
    path
}

/// Потактовая трасса эталона: пара `(a, b)` после каждого такта.
fn simulate_trace(fixture: &Path) -> Vec<(i128, i128)> {
    let source = std::fs::read_to_string(fixture).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение юнита");
    let mut trace = Vec::new();
    for _ in 0..TICKS {
        let result = unit.tick();
        assert!(
            !matches!(result, TickResult::Failed(_)),
            "симуляция не должна падать: {result:?}"
        );
        trace.push((sim_value(&unit, "a"), sim_value(&unit, "b")));
        if result == TickResult::Terminated {
            break;
        }
    }
    trace
}

/// Потактовая трасса порождённой прошивки Rust.
///
/// Драйвер реализует `Hal`, запоминая последнее записанное значение каждого порта, -
/// так прошивку наблюдает и реальная плата. Драйвер принадлежит проверке, а не продукту
/// (то же решение, что у тестбенча цели `sv`).
fn rust_trace(dir: &Path, fixture: &Path, basename: &str) -> Vec<(i128, i128)> {
    let source = std::fs::read_to_string(fixture).expect("фикстура читается");
    takt_lang::compile_to_rust(
        basename,
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение Rust");

    let root = {
        let mut chars = basename.chars();
        let first = chars
            .next()
            .expect("непустое имя")
            .to_uppercase()
            .to_string();
        format!("{first}{}", chars.as_str())
    };
    let driver = format!(
        r#"#[path = "{module}"]
mod generated;
use generated::{{Hal, OutU8Port, {root}}};
use std::cell::RefCell;
use std::rc::Rc;

struct Probe {{ a: Rc<RefCell<u8>>, b: Rc<RefCell<u8>> }}

impl Hal for Probe {{
    fn write_u8(&mut self, port: OutU8Port, value: u8) {{
        match port {{
            OutU8Port::A => *self.a.borrow_mut() = value,
            OutU8Port::B => *self.b.borrow_mut() = value,
        }}
    }}
}}

fn main() {{
    let a = Rc::new(RefCell::new(0u8));
    let b = Rc::new(RefCell::new(0u8));
    let mut model = {root}::new(Probe {{ a: Rc::clone(&a), b: Rc::clone(&b) }});
    model.init();
    for _ in 0..{TICKS} {{
        model.tick();
        println!("TICK {{}} {{}}", a.borrow(), b.borrow());
    }}
}}
"#,
        module = dir.join(format!("{basename}.rs")).display(),
    );
    std::fs::write(dir.join("driver.rs"), driver).expect("запись драйвера");

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
    parse_pairs(&String::from_utf8_lossy(&run.stdout))
}

/// Потактовая трасса порождённого RTL через тестбенч verilator.
fn sv_trace(dir: &Path, fixture: &Path, basename: &str) -> Vec<(i128, i128)> {
    let source = std::fs::read_to_string(fixture).expect("фикстура читается");
    takt_lang::compile_to_sv(
        basename,
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение SystemVerilog");

    // Значения печатаются после фронта (`#1`), иначе читалось бы состояние до
    // защёлкивания.
    let tb = format!(
        r#"module tb;
    logic clk = 0, rst_n = 0;
    logic is_done;
    logic [7:0] a, b;
    {basename} dut (.clk(clk), .rst_n(rst_n), .is_done(is_done), .a(a), .b(b));
    always #5 clk = ~clk;
    initial begin
        @(posedge clk);
        rst_n <= 1'b1;
        repeat ({TICKS}) begin
            @(posedge clk);
            #1 $display("TICK %0d %0d", a, b);
        end
        $finish;
    end
endmodule
"#
    );
    std::fs::write(dir.join("tb.sv"), tb).expect("запись тестбенча");

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
            &format!("{basename}.sv"),
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
        .expect("запуск собранной симуляции");
    parse_pairs(&String::from_utf8_lossy(&run.stdout))
}

fn parse_pairs(stdout: &str) -> Vec<(i128, i128)> {
    stdout
        .lines()
        .filter_map(|line| line.strip_prefix("TICK "))
        .map(|rest| {
            let mut it = rest
                .split_whitespace()
                .map(|v| v.parse::<i128>().expect("значение — целое"));
            (it.next().expect("a"), it.next().expect("b"))
        })
        .collect()
}

/// Каждая модель считает своей константой.
///
/// До порт `b` получал 2 вместо 3: объявление было одно на две одноимённые константы.
/// Мутация "вернуть голое имя" роняет этот тест.
#[test]
fn each_model_uses_its_own_constant_in_rust() {
    let dir = build_dir("rust");
    let path = fixture(&dir, "dupr", DUP);
    let sim = simulate_trace(&path);
    assert_eq!(
        sim.first(),
        Some(&(2, 3)),
        "предусловие: эталон обязан считать разными константами, трасса={sim:?}"
    );

    if !tool_available("rustc") {
        eprintln!("[ПРОПУСК] each_model_uses_its_own_constant_in_rust: rustc не найден");
        return;
    }
    let rs = rust_trace(&dir, &path, "dupr");
    assert_eq!(
        sim, rs,
        "потактовые трассы эталона и порождённого Rust обязаны совпадать: \
         одноимённые константы разных моделей не должны сливаться в одну.\n\
         эталон={sim:?}\nRust={rs:?}"
    );
}

/// То же для уплощённого RTL.
#[test]
fn each_model_uses_its_own_constant_in_sv() {
    let dir = build_dir("sv");
    let path = fixture(&dir, "dups", DUP);
    let sim = simulate_trace(&path);
    assert_eq!(
        sim.first(),
        Some(&(2, 3)),
        "предусловие: эталон обязан считать разными константами, трасса={sim:?}"
    );

    if !tool_available("verilator") {
        eprintln!("[ПРОПУСК] each_model_uses_its_own_constant_in_sv: verilator не найден");
        return;
    }
    let sv = sv_trace(&dir, &path, "dups");
    assert_eq!(
        sim, sv,
        "потактовые трассы эталона и порождённого RTL обязаны совпадать: \
         модуль SV уплощён, и `localparam` в нём — общее пространство имён.\n\
         эталон={sim:?}\nSV={sv:?}"
    );
}
