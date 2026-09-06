//! Отрицательное значение варианта перечисления.

use std::path::{Path, PathBuf};
use std::process::Command;
use takt_lang::semantic::tree::construct_model;
use takt_sim::{Value, build_unit};

const FIXTURE: &str = "tests/data/eval/conformance_signed_enum.takt";
const UNIT: &str = "conformance_signed_enum";
const TICKS: usize = 4;

fn verilator_available() -> bool {
    Command::new("verilator")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

fn build_dir(tag: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!(
        "takt_0423_{tag}_{}_{}",
        std::process::id(),
        std::thread::current()
            .name()
            .unwrap_or("t")
            .replace(':', "_")
    ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    dir
}

fn simulator_trace() -> Vec<i128> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор фикстуры");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    for _ in 0..TICKS {
        let _ = unit.tick();
        match unit.variable("probe") {
            Some(Value::Number(v)) => trace.push(v),
            other => panic!("порт 'probe' обязан быть числом, получено {other:?}"),
        }
    }
    trace
}

fn generated_sv_trace(dir: &Path) -> Vec<i128> {
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
    logic signed [7:0] probe;
    logic is_done;
    {UNIT} dut (.clk(clk), .rst_n(rst_n), .probe(probe), .is_done(is_done));
    always #5 clk = ~clk;
    initial begin
        @(posedge clk);
        rst_n <= 1'b1;
        for (int i = 0; i < {TICKS}; i++) begin
            @(posedge clk);
            #1 $display("TICK %0d", $signed(probe));
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
        .filter_map(|line| line.strip_prefix("TICK ")?.trim().parse::<i128>().ok())
        .collect()
}

/// Значения эталона названы числами.
#[test]
fn reference_values_are_named() {
    assert_eq!(
        simulator_trace(),
        vec![-1, 2, 12, 22],
        "трасса обязана начинаться с ветви Low"
    );
}

/// Отрицательный вариант печатается знаковой формой литерала.
#[test]
fn negative_variant_uses_signed_literal() {
    let dir = build_dir("form");
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_sv(
        UNIT,
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение SystemVerilog");
    let text = std::fs::read_to_string(dir.join(format!("{UNIT}.sv"))).expect("чтение вывода");
    assert!(
        text.contains("'sd5"),
        "отрицательное значение обязано печататься знаковым литералом:\n{text}"
    );
    assert!(
        !text.contains("'d-"),
        "формы `W'd-N` в выводе быть не должно (verilator её не разбирает):\n{text}"
    );
    // Контроль: положительные значения формы не меняют.
    assert!(
        text.contains("'d7"),
        "положительный вариант печатается прежней формой:\n{text}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// RTL считает то же, что эталон.
#[test]
fn signed_enum_matches_generated_sv() {
    if !verilator_available() {
        eprintln!("[ПРОПУСК] signed_enum_matches_generated_sv: verilator не найден");
        return;
    }
    let dir = build_dir("sv");
    let sim = simulator_trace();
    let rtl = generated_sv_trace(&dir);
    assert_eq!(sim, rtl, "трассы эталона и RTL обязаны совпадать");
    let _ = std::fs::remove_dir_all(&dir);
}
