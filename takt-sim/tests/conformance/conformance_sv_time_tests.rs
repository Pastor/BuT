//! Сверка модели времени цели `sv` (профиль "часы") - вынос из
//! `conformance_sv_tests.rs` по лимиту размера модуля.
//!
//! Выдержка `after Nms` в профиле "часы" реализуется служебным входом `time_ms`
//! (внешний источник времени, как `clk`/`en`): метка входа латчит `time_ms`,
//! условие - разностью. Здесь доказывается, что:
//! - трасса симулятора (эталон) совпадает с трассой RTL, ведомого `time_ms`
//!   (сдвига нет - у sv нет синтетического INIT, в отличие от st);
//! - порождённый RTL никогда не несёт физического времени (`#`-задержки, `$time`)
//!   -- тест A7 (проба П4 ): их yosys/verilator пропускают молча, а в железе
//!   они означают иное.
//!
//! Каждый интеграционный тест - отдельный крейт, поэтому мелкие хелперы
//! (`verilator_available`, `sim_value`, `build_dir`) продублированы из
//! `conformance_sv_tests.rs` - общего слоя `tests/common` файл-корпус не заводил.

use std::path::Path;
use std::process::Command;
use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Unit, Value, build_unit};

const TIME_FIXTURE: &str = "tests/data/eval/conformance_after_sv.takt";
const TIME_TICKS: usize = 8;

fn verilator_available() -> bool {
    Command::new("verilator")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

fn sim_value(unit: &Unit, name: &str) -> i128 {
    match unit.variable(name) {
        Some(Value::Number(n)) => n,
        Some(Value::Boolean(b)) => i128::from(b),
        Some(Value::Fixed { repr, .. }) => i128::from(repr),
        other => panic!("переменная '{name}': неожиданное значение {other:?}"),
    }
}

/// Каталог сборки под конкретный тест.
///
/// Имя потока обязательно: тесты идут параллельно, каждый помощник начинает с
/// `remove_dir_all`, а преисправление `takt_conformance_sv_` тот же, что у
/// `conformance_sv_tests` - совпади теги, тесты сносили бы каталог друг у друга.
fn build_dir(tag: &str) -> std::path::PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_conformance_sv_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог сборки");
    dir
}

/// Трасса симулятора при 1 мс на такт (эталон профиля "часы").
fn simulate_sv_time_trace() -> Vec<i128> {
    let source = std::fs::read_to_string(TIME_FIXTURE).expect("фикстура");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение юнита");
    let mut trace = Vec::new();
    for step in 0..TIME_TICKS {
        unit.set_time_ns(i64::try_from(step).unwrap() * 1_000_000);
        let result = unit.tick();
        assert!(
            !matches!(result, TickResult::Failed(_)),
            "падение: {result:?}"
        );
        trace.push(sim_value(&unit, "level"));
    }
    trace
}

/// Трасса порождённого RTL: тестбенч ведёт `time_ms` модельным временем (1 мс на такт,
/// 0-индексно - как c-time-харнесс `fake_now = tick-1`).
fn sv_time_trace(dir: &Path) -> Vec<i128> {
    let source = std::fs::read_to_string(TIME_FIXTURE).expect("фикстура");
    takt_lang::compile_to_sv(
        "svtime",
        &source,
        dir.to_str().unwrap(),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение SystemVerilog");
    let tb = format!(
        r#"module tb;
    logic clk = 0, rst_n = 0;
    logic is_done;
    logic [31:0] time_ms = 0;
    svtime dut (.clk(clk), .rst_n(rst_n), .time_ms(time_ms[7:0]), .is_done(is_done));
    always #5 clk = ~clk;
    initial begin
        @(posedge clk);
        rst_n <= 1'b1;
        for (int i = 0; i < {TIME_TICKS}; i++) begin
            time_ms = i;              // модельное время такта i+1 = i мс
            @(posedge clk);
            #1 $display("TICK %0d", dut.level);
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
            "svtime.sv",
            "-o",
            "simtb",
        ])
        .output()
        .expect("запуск verilator");
    assert!(
        build.status.success(),
        "verilator не собрал тестбенч времени:\n{}",
        String::from_utf8_lossy(&build.stderr)
    );
    let run = Command::new(dir.join("obj_dir").join("simtb"))
        .current_dir(dir)
        .output()
        .expect("запуск симуляции");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter_map(|l| l.strip_prefix("TICK ")?.trim().parse::<i128>().ok())
        .collect()
}

/// Выдержка `after 5ms` (профиль "часы") срабатывает у симулятора и у порождённого RTL
/// на одном такте: у sv нет синтетического INIT, поэтому сдвига нет (в отличие от st).
/// Мягкая деградация: нет verilator -> пропуск.
#[test]
fn after_clock_profile_matches_generated_sv() {
    let sim = simulate_sv_time_trace();
    // 5 мс при 1 мс/такт - уровень становится 1 на 6-м такте (индекс 5).
    assert_eq!(
        sim,
        vec![0, 0, 0, 0, 0, 1, 1, 1],
        "эталон профиля «часы»: {sim:?}"
    );
    if !verilator_available() {
        eprintln!("[ПРОПУСК] after_clock_profile_matches_generated_sv: verilator не найден");
        return;
    }
    let dir = build_dir("svtime");
    let sv = sv_time_trace(&dir);
    assert_eq!(
        sim, sv,
        "трассы симулятора и RTL (time_ms) обязаны совпадать\nсим={sim:?}\nRTL={sv:?}"
    );
}

// -- Периодический блок `every` ---------------------------------

const EVERY_FIXTURE: &str = "tests/data/eval/conformance_every.takt";
const EVERY_TICKS: usize = 10;

/// Трасса эталона `every`: `led` после каждого такта при 1 мс/такт.
fn simulate_every_trace() -> Vec<i128> {
    let source = std::fs::read_to_string(EVERY_FIXTURE).expect("фикстура");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение юнита");
    let mut trace = Vec::new();
    for step in 0..EVERY_TICKS {
        unit.set_time_ns(i64::try_from(step).unwrap() * 1_000_000);
        let r = unit.tick();
        assert!(!matches!(r, TickResult::Failed(_)), "падение: {r:?}");
        trace.push(sim_value(&unit, "led"));
    }
    trace
}

/// Трасса RTL `every`: тестбенч ведёт `time_ms` модельным временем, читает `dut.led`.
fn sv_every_trace(dir: &Path) -> Vec<i128> {
    let source = std::fs::read_to_string(EVERY_FIXTURE).expect("фикстура");
    takt_lang::compile_to_sv(
        "svevery",
        &source,
        dir.to_str().unwrap(),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение SystemVerilog");
    let tb = format!(
        r#"module tb;
    logic clk = 0, rst_n = 0;
    logic is_done;
    logic [31:0] time_ms = 0;
    logic [7:0] led;
    svevery dut (.clk(clk), .rst_n(rst_n), .time_ms(time_ms[7:0]), .led(led), .is_done(is_done));
    always #5 clk = ~clk;
    initial begin
        @(posedge clk);
        rst_n <= 1'b1;
        for (int i = 0; i < {EVERY_TICKS}; i++) begin
            time_ms = i;
            @(posedge clk);
            #1 $display("TICK %0d", dut.led);
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
            "svevery.sv",
            "-o",
            "simtb",
        ])
        .output()
        .expect("verilator");
    assert!(
        build.status.success(),
        "verilator не собрал тестбенч `every`:\n{}",
        String::from_utf8_lossy(&build.stderr)
    );
    let run = Command::new(dir.join("obj_dir").join("simtb"))
        .current_dir(dir)
        .output()
        .expect("запуск");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter_map(|l| l.strip_prefix("TICK ")?.trim().parse::<i128>().ok())
        .collect()
}

/// Периодический блок `every 3ms` (профиль "часы") срабатывает у симулятора и у RTL на
/// одних тактах (3, 6, 9). Мягкая деградация: нет verilator -> пропуск.
#[test]
fn every_period_matches_generated_sv() {
    let sim = simulate_every_trace();
    assert_eq!(
        sim,
        vec![0, 0, 0, 1, 1, 1, 2, 2, 2, 3],
        "эталон периода `every`: {sim:?}"
    );
    if !verilator_available() {
        eprintln!("[ПРОПУСК] every_period_matches_generated_sv: verilator не найден");
        return;
    }
    let dir = build_dir("svevery");
    let sv = sv_every_trace(&dir);
    assert_eq!(
        sim, sv,
        "трассы симулятора и RTL (`every`) обязаны совпадать\nсим={sim:?}\nRTL={sv:?}"
    );
}

/// Тест A7 (проба П4 ): порождённый RTL никогда не несёт `#`-задержек и `$time` - их
/// yosys/verilator пропускают молча, а в железе они означают иное.
#[test]
fn generated_sv_has_no_physical_time() {
    let source = std::fs::read_to_string(TIME_FIXTURE).expect("фикстура");
    let dir = build_dir("sva7");
    takt_lang::compile_to_sv(
        "svtime",
        &source,
        dir.to_str().unwrap(),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение SystemVerilog");
    let sv = std::fs::read_to_string(dir.join("svtime.sv")).expect(".sv");
    // Строки-комментарии не в счёт - грепаем код. Порт `time_ms` содержит `time`, но не
    // `$time`; проверяем именно `$time` и `#<цифра>`.
    for line in sv.lines() {
        let code = line.split("//").next().unwrap_or("");
        assert!(!code.contains("$time"), "RTL не должен нести $time: {line}");
        assert!(
            !code.contains("#0")
                && !code.contains("#1")
                && !code.contains("#2")
                && !code.contains("#3")
                && !code.contains("#4")
                && !code.contains("#5")
                && !code.contains("#6")
                && !code.contains("#7")
                && !code.contains("#8")
                && !code.contains("#9"),
            "RTL не должен нести #-задержек: {line}"
        );
    }
}
