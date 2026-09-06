//! Сверка сброса перечислимого регистра у цели `sv`.
//!
//! ## Что доказывает
//!
//! Регистр перечислимого типа без инициализатора цель сбрасывала в `'0`, и verilator
//! отвечал **ошибкой** `%Error-ENUMVALUE`: перечисления SystemVerilog строго
//! типизированы. Проверка цели считает предупреждение ошибкой, то есть вывод не проходил
//! проверку своей же цели - при **нулевом** коде возврата `taktc`.

use std::path::{Path, PathBuf};
use std::process::Command;

use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Unit, Value, build_unit};

const FIXTURE: &str = "tests/data/eval/conformance_sv_enum_reset.takt";
const UNIT: &str = "svenumreset";
const TICKS: usize = 2;
const OBSERVED: &[&str] = &["reset_mode", "reset_phase"];

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

fn verilator_available() -> bool {
    Command::new("verilator")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

fn sim_value(unit: &Unit, name: &str) -> i128 {
    match unit.variable(name) {
        Some(Value::Number(v)) => v,
        Some(Value::Boolean(b)) => i128::from(b),
        other => panic!("порт '{name}': неожиданное значение {other:?}"),
    }
}

fn simulator_trace() -> Vec<Vec<i128>> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор фикстуры");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    for _ in 0..TICKS {
        let result = unit.tick();
        assert!(
            !matches!(result, TickResult::Failed(_)),
            "симуляция не должна падать: {result:?}"
        );
        trace.push(OBSERVED.iter().map(|name| sim_value(&unit, name)).collect());
    }
    trace
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

fn generated_sv_trace(dir: &Path) -> Vec<Vec<i128>> {
    generate_sv(dir);
    let decls = OBSERVED
        .iter()
        .map(|name| format!("    logic [7:0] {name};"))
        .collect::<Vec<_>>()
        .join("\n");
    let ports = OBSERVED
        .iter()
        .map(|name| format!(".{name}({name})"))
        .collect::<Vec<_>>()
        .join(", ");
    let args = OBSERVED.join(", ");
    let fmt = vec!["%0d"; OBSERVED.len()].join(" ");
    let tb = format!(
        r#"module tb;
    logic clk = 0, rst_n = 0;
    logic is_done;
{decls}
    {UNIT} dut (.clk(clk), .rst_n(rst_n), {ports}, .is_done(is_done));
    always #5 clk = ~clk;
    initial begin
        @(posedge clk);
        rst_n <= 1'b1;
        for (int i = 0; i < {TICKS}; i++) begin
            @(posedge clk);
            #1 $display("TICK {fmt}", {args});
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
        .filter_map(|line| {
            let rest = line.strip_prefix("TICK ")?;
            Some(
                rest.split_whitespace()
                    .map(|v| v.parse().expect("число в трассе"))
                    .collect(),
            )
        })
        .collect()
}

/// Значение сброса совпадает с эталоном - Первый по тексту вариант.
///
/// Прежде здесь стоял ноль; правило изменено: ноль может не принадлежать набору
/// вариантов, и тогда регистр стартует со значения, о котором не знает ни один `case`.
/// `Mode` начинается с `Idle = 1`, `Phase` - с `Zero = 0`: разные ответы у этих двух и
/// отличают новое правило от прежнего.
#[test]
fn enum_reset_values_match_generated_sv() {
    let sim = simulator_trace();
    assert_eq!(
        sim,
        vec![vec![1, 0], vec![2, 1]],
        "эталон: на первом такте регистры ещё в сбросе (первый вариант): {sim:?}"
    );

    if !verilator_available() {
        eprintln!("[ПРОПУСК] enum_reset_values_match_generated_sv: нет verilator");
        return;
    }
    let dir = build_dir("sv_enum_reset");
    let rtl = generated_sv_trace(&dir);
    assert_eq!(sim, rtl, "трассы разошлись\nsim={sim:?}\nRTL={rtl:?}");
    let _ = std::fs::remove_dir_all(&dir);
}

/// Линт цели принимает вывод, а форма записи - мнемоника первого варианта.
///
/// Именно линт и был арбитром: `m <= '0;` - `%Error-ENUMVALUE`, и проверка цели считает
/// предупреждение ошибкой.
#[test]
fn enum_reset_passes_the_lint_and_uses_the_right_form() {
    let dir = build_dir("sv_enum_reset_lint");
    generate_sv(&dir);
    let sv = std::fs::read_to_string(dir.join(format!("{UNIT}.sv"))).expect("вывод цели");
    assert!(
        sv.contains("<= MODE_IDLE;"),
        "умолчание — первый по тексту вариант (фича 0391):\n{sv}"
    );
    assert!(
        sv.contains("<= PHASE_ZERO;"),
        "у перечисления, чей первый вариант нулевой, — та же мнемоника:\n{sv}"
    );
    assert!(
        !sv.contains("mode_e'(0)"),
        "приведения к нулю быть не должно: ноль вне набора вариантов\n{sv}"
    );
    if !verilator_available() {
        eprintln!("[ПРОПУСК] линт: нет verilator");
        return;
    }
    let out = Command::new("verilator")
        .current_dir(&dir)
        .args(["--lint-only", "-Wall", &format!("{UNIT}.sv")])
        .output()
        .expect("запуск verilator");
    assert!(
        out.status.success(),
        "линт цели обязан принять вывод:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let _ = std::fs::remove_dir_all(&dir);
}
