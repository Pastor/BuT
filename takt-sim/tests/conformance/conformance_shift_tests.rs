//! Потактовая сверка **сдвигов** `<<`/`>>` цели `sv` с эталоном.

use std::path::{Path, PathBuf};
use std::process::Command;
use takt_lang::semantic::tree::construct_model;
use takt_sim::{Value, build_unit};

const FIXTURE: &str = "tests/data/eval/conformance_shift.takt";
const UNIT: &str = "svshift";
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

/// Трасса эталона: `(right, left)` по тактам.
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
        trace.push((value("right"), value("left")));
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
    logic signed [7:0] right;
    logic [7:0] left;
    logic is_done;
    {UNIT} dut (.clk(clk), .rst_n(rst_n), .right(right), .left(left), .is_done(is_done));
    always #5 clk = ~clk;
    initial begin
        @(posedge clk);
        rst_n <= 1'b1;
        for (int i = 0; i < {TICKS}; i++) begin
            @(posedge clk);
            #1 $display("TICK %0d %0d", right, left);
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
            let right = it.next()?.parse::<i128>().ok()?;
            let left = it.next()?.parse::<i128>().ok()?;
            Some((right, left))
        })
        .collect()
}

/// Значения сдвигов совпадают у эталона и у RTL.
///
/// Ожидание записано **числами**: `-7 >> 1 = -4` (арифметический сдвиг, округление к
/// −∞) и `3 << 2 = 12`. Логический сдвиг дал бы **124** - ошибка видна значением, а не
/// структурой вывода.
///
/// Значение **нечётное** намеренно: на чётном floor и усечение к нулю совпадают, и
/// подмена сдвига делением осталась бы незамеченной.
#[test]
fn shifts_match_generated_sv() {
    let sim = simulator_trace();
    assert_eq!(
        sim,
        vec![(-4, 12), (-4, 12), (-4, 12)],
        "эталон обязан давать арифметический сдвиг −4 и левый 12: {sim:?}"
    );

    if !verilator_available() {
        eprintln!("[ПРОПУСК] array_values_match_generated_sv: verilator не найден");
        return;
    }
    let dir = build_dir("sv_shift");
    let rtl = generated_sv_trace(&dir);
    assert_eq!(rtl.len(), TICKS, "тестбенч обязан напечатать каждый такт");
    assert_eq!(sim, rtl, "трассы разошлись\nsim={sim:?}\nRTL={rtl:?}");
}

/// **Устройство:** знаковый операнд печатается арифметическим `>>>`.
///
/// Проверка текстом нужна потому, что `>>` и `>>>` различаются **значением только на
/// отрицательных**: на положительных модуль вёл бы себя одинаково, и подмена прошла бы
/// мимо сверки, случись она на другой фикстуре.
#[test]
fn signed_shift_uses_arithmetic_operator() {
    let dir = build_dir("sv_shift_signed");
    let source = "var a: i8 := -8;\nvar v: i8 := 0;\nout probe: i8 at 0;\n\
         start Run { always { v := a >> 1; probe := v; } ref Run: v < 100; }\n";
    takt_lang::compile_to_sv(
        "svshiftsigned",
        source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение SystemVerilog");
    let text = std::fs::read_to_string(dir.join("svshiftsigned.sv")).expect("чтение модуля");
    assert!(
        text.contains(">>>"),
        "знаковый сдвиг вправо обязан быть арифметическим:\n{text}"
    );
}
