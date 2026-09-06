//! Потактовая сверка присваивания агрегата массива.

use std::path::{Path, PathBuf};
use std::process::Command;
use takt_lang::semantic::tree::construct_model;
use takt_sim::{Value, build_unit};

const FIXTURE: &str = "tests/data/eval/conformance_array_assign.takt";
const UNIT: &str = "arrassign";
const TICKS: usize = 3;

fn verilator_available() -> bool {
    Command::new("verilator")
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

/// Трасса эталона: `(first, second)` по тактам.
fn simulator_trace() -> Vec<(i128, i128)> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор фикстуры");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    for _ in 0..TICKS {
        let _ = unit.tick();
        let value = |name: &str| match unit.variable(name) {
            Some(Value::Number(v)) => v,
            other => panic!("порт '{name}' обязан быть числом, получено {other:?}"),
        };
        trace.push((value("first"), value("second")));
    }
    trace
}

/// Трасса порождённого RTL: те же порты по тактам.
fn generated_sv_trace(dir: &Path) -> Vec<(i128, i128)> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_sv(
        UNIT,
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение SystemVerilog");

    let tb = format!(
        r#"module tb;
    logic clk = 0, rst_n = 0;
    logic [7:0] first, second;
    logic is_done;
    {UNIT} dut (.clk(clk), .rst_n(rst_n), .first(first), .second(second), .is_done(is_done));
    always #5 clk = ~clk;
    initial begin
        @(posedge clk);
        rst_n <= 1'b1;
        for (int i = 0; i < {TICKS}; i++) begin
            @(posedge clk);
            #1 $display("TICK %0d %0d", first, second);
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
        "verilator не собрал тестбенч массива:\n{}",
        String::from_utf8_lossy(&build.stderr)
    );
    let run = Command::new(dir.join("obj_dir").join("simtb"))
        .current_dir(dir)
        .output()
        .expect("запуск симуляции");
    assert!(run.status.success(), "симуляция RTL массива упала");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter_map(|line| {
            let rest = line.strip_prefix("TICK ")?;
            let mut it = rest.split_whitespace();
            let first = it.next()?.parse::<i128>().ok()?;
            let second = it.next()?.parse::<i128>().ok()?;
            Some((first, second))
        })
        .collect()
}

/// Значения присвоенного агрегата совпадают у эталона и у RTL.
#[test]
fn array_assignment_matches_generated_sv() {
    let sim = simulator_trace();
    assert_eq!(
        sim,
        vec![(1, 11), (2, 12), (3, 13)],
        "эталон обязан присваивать оба элемента: {sim:?}"
    );

    if !verilator_available() {
        eprintln!("[ПРОПУСК] array_values_match_generated_sv: verilator не найден");
        return;
    }
    let dir = build_dir("array_assign");
    let rtl = generated_sv_trace(&dir);
    assert_eq!(rtl.len(), TICKS, "тестбенч обязан напечатать каждый такт");
    assert_eq!(sim, rtl, "трассы разошлись\nsim={sim:?}\nRTL={rtl:?}");
}

/// Цель `st` печатает поэлементные присваивания, и `iec2c` их принимает.
///
/// Проверяется и текст, и прогон арбитра: до фичи цель отказывала, а агрегатной формы
/// значения массива в IEC нет - "просто напечатать" её было нельзя.
#[test]
fn generated_st_assigns_elements_one_by_one() {
    let dir = build_dir("array_assign_st");
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
        text.contains("a[0] :=") && text.contains("a[1] :="),
        "агрегат обязан разворачиваться поэлементно:\n{text}"
    );

    let iec2c =
        std::path::Path::new(&std::env::var("HOME").unwrap_or_default()).join(".local/bin/iec2c");
    if !iec2c.is_file() {
        eprintln!("[ПРОПУСК] generated_st_assigns_elements_one_by_one: iec2c не найден");
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
