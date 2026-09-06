//! Потактовая сверка приведения **`as`** цели `sv` с эталоном.

use std::path::{Path, PathBuf};
use std::process::Command;
use takt_lang::semantic::tree::construct_model;
use takt_sim::{Value, build_unit};

const FIXTURE: &str = "tests/data/eval/conformance_sv_cast.takt";
const UNIT: &str = "svcast";
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

/// Трасса эталона: `(probe, cut)` по тактам.
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
        trace.push((value("probe"), value("cut")));
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
    logic [15:0] probe;
    logic [7:0] cut;
    logic is_done;
    {UNIT} dut (.clk(clk), .rst_n(rst_n), .probe(probe), .cut(cut), .is_done(is_done));
    always #5 clk = ~clk;
    initial begin
        @(posedge clk);
        rst_n <= 1'b1;
        for (int i = 0; i < {TICKS}; i++) begin
            @(posedge clk);
            #1 $display("TICK %0d %0d", probe, cut);
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
            let probe = it.next()?.parse::<i128>().ok()?;
            let cut = it.next()?.parse::<i128>().ok()?;
            Some((probe, cut))
        })
        .collect()
}

/// Значения приведения совпадают у эталона и у RTL.
///
/// Ожидание записано **числами**: `200 as u16 + 100 = 300` (расширение), а `300 as u8 =
/// 44` (сужение с обёрткой, правило ). Ошибка в ширине видна **значением**: на одном
/// расширении она была бы незаметна - старшие разряды и так нули.
#[test]
fn casts_match_generated_sv() {
    let sim = simulator_trace();
    assert_eq!(
        sim,
        vec![(300, 44), (300, 44), (300, 44)],
        "эталон обязан расширять до 300 и сужать до 44: {sim:?}"
    );

    if !verilator_available() {
        eprintln!("[ПРОПУСК] array_values_match_generated_sv: verilator не найден");
        return;
    }
    let dir = build_dir("sv_cast");
    let rtl = generated_sv_trace(&dir);
    assert_eq!(rtl.len(), TICKS, "тестбенч обязан напечатать каждый такт");
    assert_eq!(sim, rtl, "трассы разошлись\nsim={sim:?}\nRTL={rtl:?}");
}

/// **Устройство:** знаковая цель печатается через `$signed`.
///
/// Без `$signed` сравнение и арифметический сдвиг работали бы как беззнаковые - молча:
/// оба инструмента SV такой модуль принимают.
#[test]
fn signed_cast_uses_signed_wrapper() {
    let dir = build_dir("sv_cast_signed");
    let source = "var a: u8 := 5;\nvar s: i16 := 0;\nout probe: i16 at 0;\n\
         start Run { always { s := a as i16; probe := s; } ref Run: s < 100; }\n";
    takt_lang::compile_to_sv(
        "svcastsigned",
        source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение SystemVerilog");
    let text = std::fs::read_to_string(dir.join("svcastsigned.sv")).expect("чтение модуля");
    assert!(
        text.contains("$signed(16'("),
        "знаковое приведение обязано печататься через $signed:\n{text}"
    );
}
