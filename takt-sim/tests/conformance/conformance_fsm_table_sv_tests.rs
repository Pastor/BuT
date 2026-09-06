//! Потактовая сверка табличной формы автомата у цели `sv`.
//!
//! # Что доказывает набор
//!
//! Флаг `--fsm=table` меняет **форму** порождённого RTL, а не поведение: эталон, модуль
//! формы `unique case` и модуль формы `table` дают одну трассу такт в такт - и на
//! простом автомате, и на последовательной композиции.
//!
//! Линт и синтез этого не видят по устройству: `verilator` и `yosys` принимают модуль,
//! который считает другое. Ровно поэтому предмет проверки - **значения** на портах по
//! тактам, а не факт компиляции.
//!
//! Момент перехода у таблицы совпадает с формой `unique case` потому, что
//! готовность композиции читается по `_next`, а готовность цепочки - по
//! **регистру** шага: в комбинационном блоке он не меняется, и диспетчер в
//! конце блока видит то же значение, что ветвь `case`.

use std::path::{Path, PathBuf};
use std::process::Command;

use takt_lang::generator::{FsmForm, GenerateOptions};
use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Value, build_unit};

const SIMPLE_FIXTURE: &str = "tests/data/eval/conformance_fsm_table.takt";
const SIMPLE_UNIT: &str = "conformance_fsm_table";
const SIMPLE_TICKS: usize = 8;

const CHAIN_FIXTURE: &str = "tests/data/eval/conformance_fsm_table_chain.takt";
const CHAIN_UNIT: &str = "conformance_fsm_table_chain";
const CHAIN_TICKS: usize = 9;

fn verilator_available() -> bool {
    Command::new("verilator")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Каталог сборки, уникальный по потоку И процессу.
fn build_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0441_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог сборки");
    dir
}

fn source(fixture: &str) -> String {
    std::fs::read_to_string(fixture).expect("фикстура читается")
}

/// Трасса эталона по наблюдаемым портам.
fn simulator_trace(fixture: &str, ports: &[&str], ticks: usize) -> Vec<Vec<i128>> {
    let (ast, _) = takt_lang::parse(&source(fixture), 0).expect("разбор фикстуры");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    let mut reg = vec![0i128; ports.len()];
    for _ in 0..ticks {
        let result = unit.tick();
        assert!(
            !matches!(result, TickResult::Failed(_)),
            "эталон не обрывает прогон: {result:?}"
        );
        for (slot, port) in reg.iter_mut().zip(ports) {
            match unit.variable(port) {
                Some(Value::Number(v)) => *slot = v,
                Some(Value::Boolean(b)) => *slot = i128::from(b),
                _ => {}
            }
        }
        trace.push(reg.clone());
    }
    trace
}

/// Трасса порождённого RTL заданной формы: тестбенч на `verilator --binary`.
fn sv_trace(
    dir: &Path,
    fixture: &str,
    unit: &str,
    ports: &[&str],
    ticks: usize,
    fsm: FsmForm,
) -> Vec<Vec<i128>> {
    let mut options = GenerateOptions::default();
    options.fsm = fsm;
    takt_lang::compile_to_sv(
        unit,
        &source(fixture),
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &options,
    )
    .expect("порождение SystemVerilog");

    let decls = ports
        .iter()
        .map(|p| format!("    logic [7:0] {p};"))
        .collect::<Vec<_>>()
        .join("\n");
    let conns = ports
        .iter()
        .map(|p| format!(".{p}({p})"))
        .collect::<Vec<_>>()
        .join(", ");
    let fmt = ports.iter().map(|_| "%0d").collect::<Vec<_>>().join(" ");
    let args = ports.to_vec().join(", ");
    let tb = format!(
        r#"module tb;
    logic clk = 0, rst_n = 0;
{decls}
    logic is_done;
    {unit} dut (.clk(clk), .rst_n(rst_n), {conns}, .is_done(is_done));
    always #5 clk = ~clk;
    initial begin
        @(posedge clk);
        rst_n <= 1'b1;
        for (int i = 0; i < {ticks}; i++) begin
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
            &format!("{unit}.sv"),
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

/// Простой автомат: условное ребро, `enter`/`exit`, два конкурирующих ребра.
#[test]
fn sv_table_form_matches_case_form_and_simulator() {
    if !verilator_available() {
        eprintln!("verilator недоступен — сверка пропущена");
        return;
    }
    let ports = ["probe"];
    let expected = simulator_trace(SIMPLE_FIXTURE, &ports, SIMPLE_TICKS);
    let case = sv_trace(
        &build_dir("simple_case"),
        SIMPLE_FIXTURE,
        SIMPLE_UNIT,
        &ports,
        SIMPLE_TICKS,
        FsmForm::Switch,
    );
    let table = sv_trace(
        &build_dir("simple_table"),
        SIMPLE_FIXTURE,
        SIMPLE_UNIT,
        &ports,
        SIMPLE_TICKS,
        FsmForm::Table,
    );
    assert_eq!(case, expected, "форма unique case разошлась с эталоном");
    assert_eq!(table, expected, "форма table разошлась с эталоном");
    // Контроль: трасса наблюдает и вход (`enter` даёт 100), и выход (`exit` - 200).
    let flat: Vec<i128> = expected.iter().flatten().copied().collect();
    assert!(
        flat.contains(&100) && flat.contains(&200),
        "трасса не наблюдает блоки enter/exit: {expected:?}"
    );
}

/// Последовательная композиция: выход из цепочки - строка таблицы.
#[test]
fn sv_table_form_matches_case_form_on_chain() {
    if !verilator_available() {
        eprintln!("verilator недоступен — сверка пропущена");
        return;
    }
    let ports = ["first_probe", "second_probe", "line_probe"];
    let expected = simulator_trace(CHAIN_FIXTURE, &ports, CHAIN_TICKS);
    let case = sv_trace(
        &build_dir("chain_case"),
        CHAIN_FIXTURE,
        CHAIN_UNIT,
        &ports,
        CHAIN_TICKS,
        FsmForm::Switch,
    );
    let table = sv_trace(
        &build_dir("chain_table"),
        CHAIN_FIXTURE,
        CHAIN_UNIT,
        &ports,
        CHAIN_TICKS,
        FsmForm::Table,
    );
    assert_eq!(
        table, case,
        "формы unique case и table разошлись на цепочке"
    );
    assert_eq!(case, expected, "форма unique case разошлась с эталоном");
    // Контроль: видны оба шага цепочки и жизнь после неё.
    let flat: Vec<i128> = case.iter().flatten().copied().collect();
    assert!(
        flat.contains(&12) && flat.contains(&22) && flat.contains(&91),
        "трасса не наблюдает шаги цепочки: {case:?}"
    );
}
