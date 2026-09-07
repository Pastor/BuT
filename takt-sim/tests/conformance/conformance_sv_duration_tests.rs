//! Потактовая сверка **значений** типа `duration` цели `sv` с эталоном.
//!
//! Это слабее по существу: оба инструмента SV принимают молча неверный модуль - на этом
//! стоял дефект 0045, где `always_comb` читал регистр вместо `_next`, и вердикт дала
//! только потактовая сверка.
//!
//! Граница, ради которой сверка заведена: эталон держит длительность в
//! **наносекундах**, цель - в **миллисекундах**, и ошибка на этом
//! переводе не даёт ни отказа, ни предупреждения.
//!
//! **Фикстура своя**, а не общая с `c`/`rust`/`st`: приведение `as` цель `sv` не
//! транслирует (`SV-002`), поэтому число миллисекунд через порт не выдать. Значение
//! зажимается сравнениями с двух сторон и проверяется на равенство, а сам сигнал
//! `elapsed` читается тестбенчем иерархически - у цели это `logic [31:0]` в
//! миллисекундах.
//!
//! Мягкая деградация: нет `verilator` -> пропуск, не отказ.

use std::path::{Path, PathBuf};
use std::process::Command;
use takt_lang::semantic::tree::construct_model;
use takt_sim::{Unit, Value, build_unit};

const FIXTURE: &str = "tests/data/eval/conformance_duration_value_sv.takt";
/// Имя модуля порождённого SV (оно же имя входа для порождения).
const UNIT: &str = "svdurvalue";
/// Иерархическое имя сигнала длительности внутри модуля: `<модуль>_<модель>_<имя>`.
const ELAPSED_SIGNAL: &str = "svdurvalue_timers_elapsed";
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

fn sim_number(unit: &Unit, name: &str) -> i128 {
    match unit.variable(name) {
        Some(Value::Number(v)) => v,
        // Наносекунды эталона переводятся в миллисекунды здесь: перевод и есть предмет
        // сверки, и делать его в цели значило бы сверять цель с собой.
        Some(Value::Duration(ns)) => i128::from(ns / 1_000_000),
        other => panic!("значение '{name}' не число и не длительность: {other:?}"),
    }
}

/// Значения `(elapsed, late, exact, over)` у эталона после первого такта.
fn simulator_values() -> (i128, i128, i128, i128) {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор фикстуры");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение Unit");
    let _ = unit.tick();
    (
        sim_number(&unit, "elapsed"),
        sim_number(&unit, "late"),
        sim_number(&unit, "exact"),
        sim_number(&unit, "over"),
    )
}

/// Значения `(elapsed, late, exact, over)` порождённого RTL по тактам.
fn generated_sv_values(dir: &Path) -> Vec<(i128, i128, i128, i128)> {
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
    logic late, exact, over, is_done;
    {UNIT} dut (.clk(clk), .rst_n(rst_n), .late(late), .exact(exact), .over(over), .is_done(is_done));
    always #5 clk = ~clk;
    initial begin
        @(posedge clk);
        rst_n <= 1'b1;
        for (int i = 0; i < {TICKS}; i++) begin
            @(posedge clk);
            #1 $display("TICK %0d %0d %0d %0d",
                        dut.{ELAPSED_SIGNAL}, late, exact, over);
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
        "verilator не собрал тестбенч длительностей:\n{}",
        String::from_utf8_lossy(&build.stderr)
    );
    let run = Command::new(dir.join("obj_dir").join("simtb"))
        .current_dir(dir)
        .output()
        .expect("запуск симуляции");
    assert!(run.status.success(), "симуляция RTL длительностей упала");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter_map(|line| {
            let rest = line.strip_prefix("TICK ")?;
            let mut it = rest.split_whitespace();
            let elapsed = it.next()?.parse::<i128>().ok()?;
            let late = it.next()?.parse::<i128>().ok()?;
            let exact = it.next()?.parse::<i128>().ok()?;
            let over = it.next()?.parse::<i128>().ok()?;
            Some((elapsed, late, exact, over))
        })
        .collect()
}

/// Значение длительности и все три сравнения совпадают у эталона и у RTL.
///
/// Сверяются **значения**, а не факт линта: `verilator` и `yosys` принимают модуль,
/// который считает другое.
#[test]
fn duration_values_match_generated_sv() {
    let values = simulator_values();
    // Эталон: 1s + 750ms = 1750 мс; нижняя граница пройдена, равенство точно, верхняя -
    // нет. Ожидание записано числами, а не выведено из цели.
    assert_eq!(
        values,
        (1750, 1, 1, 0),
        "эталон обязан давать 1750 мс и сравнения (1, 1, 0)"
    );

    if !verilator_available() {
        eprintln!("[ПРОПУСК] duration_values_match_generated_sv: verilator не найден");
        return;
    }
    let dir = build_dir("sv_duration");
    let ticks = generated_sv_values(&dir);
    assert_eq!(ticks.len(), TICKS, "тестбенч обязан напечатать каждый такт");
    for (index, got) in ticks.iter().enumerate() {
        assert_eq!(
            *got,
            values,
            "такт {}: RTL разошёлся с эталоном (elapsed, late, exact, over)",
            index + 1
        );
    }
}
