//! Сверка локальной переменной со структурой у цели `sv`.
//!
//! ## Что доказывает
//!
//! Локальные переменные тел цель объявляет `automatic` внутри ветви `case`. Переменной,
//! содержащей структуру, этого мало: тело пишет её поля, а внутри ветви yosys полным
//! присваиванием такую запись не считает - "Latch inferred for signal
//! `$unnamed_block$1.tmp.lo`".

use std::path::{Path, PathBuf};
use std::process::Command;

use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Unit, Value, build_unit};

const FIXTURE: &str = "tests/data/eval/conformance_sv_struct_local.takt";
const UNIT: &str = "svstructlocal";
const TICKS: usize = 3;

/// Наблюдаемые: три формы записи структуры - по полям, целиком и внутри массива.
/// Синтезатор различает их, поэтому фикстура держит все три.
const OBSERVED: &[&str] = &[
    "field_lo",
    "field_hi",
    "whole_lo",
    "whole_hi",
    "nested_first",
    "nested_last",
];

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

fn yosys_available() -> bool {
    Command::new("yosys")
        .arg("-V")
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

/// Значения совпадают с RTL: подъём объявления поведения не изменил.
#[test]
fn struct_local_values_match_generated_sv() {
    let sim = simulator_trace();
    assert_eq!(
        sim,
        vec![
            vec![1, 11, 21, 61, 61, 91],
            vec![2, 12, 22, 62, 62, 92],
            vec![3, 13, 23, 63, 63, 93],
        ],
        "эталон: три формы записи структуры растут по тактам: {sim:?}"
    );

    if !verilator_available() {
        eprintln!("[ПРОПУСК] struct_local_values_match_generated_sv: нет verilator");
        return;
    }
    let dir = build_dir("sv_struct_local");
    let rtl = generated_sv_trace(&dir);
    assert_eq!(rtl.len(), TICKS, "тестбенч обязан напечатать каждый такт");
    assert_eq!(sim, rtl, "трассы разошлись\nsim={sim:?}\nRTL={rtl:?}");
    let _ = std::fs::remove_dir_all(&dir);
}

/// Порождённый модуль синтезируется: verilator этого не доказывает.
///
/// Именно здесь класс и жил: линт цели идёт через verilator, а он объявление внутри
/// ветви принимает - "Latch inferred" отвечает только yosys.
#[test]
fn struct_local_is_synthesizable() {
    if !yosys_available() {
        eprintln!("[ПРОПУСК] struct_local_is_synthesizable: нет yosys");
        return;
    }
    let dir = build_dir("sv_struct_local_synth");
    generate_sv(&dir);
    let synth = Command::new("yosys")
        .current_dir(&dir)
        .args([
            "-q",
            "-p",
            &format!("read_verilog -sv {UNIT}.sv; synth -top {UNIT}"),
        ])
        .output()
        .expect("запуск yosys");
    assert!(
        synth.status.success(),
        "порождённый SystemVerilog не синтезируется:\n{}",
        String::from_utf8_lossy(&synth.stderr)
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// Объявление стоит первым в процессе, а умолчания - и целиком, и по полям.
///
/// Обе формы умолчания обязательны: тело вправе писать структуру целиком (`whole =
/// make(n);`) и по полям (`field.lo = ...`), а yosys считает незаданной ту форму,
/// которой присваивает тело.
#[test]
fn struct_local_declaration_precedes_statements() {
    let dir = build_dir("sv_struct_local_text");
    generate_sv(&dir);
    let sv = std::fs::read_to_string(dir.join(format!("{UNIT}.sv"))).expect("вывод цели");
    let comb = sv
        .split("always_comb begin")
        .nth(1)
        .expect("в выводе есть always_comb");
    let decl = comb
        .find("automatic pair_t field;")
        .expect("объявление field");
    let first_stmt = comb
        .find("state_next = state;")
        .expect("умолчание регистра");
    assert!(
        decl < first_stmt,
        "объявление обязано предшествовать операторам процесса:\n{comb}"
    );
    for whole in ["field = '0;", "whole = '0;"] {
        assert!(
            comb.contains(whole),
            "нет умолчания целиком '{whole}':\n{comb}"
        );
    }
    for leaf in ["field.lo = 8'd0;", "cells[1].hi = 8'd0;"] {
        assert!(
            comb.contains(leaf),
            "нет умолчания по полю '{leaf}':\n{comb}"
        );
    }
    let _ = std::fs::remove_dir_all(&dir);
}

// --: частично использованная локальная структура ------------------

/// Исправлениетура, где поле локальной структуры записано, но не прочитано.
const PARTIAL: &str = "struct Pair { lo: u8, hi: u8 }\n\
                       var n: u8 := 0;\n\
                       out a: u8 at 0;\n\
                       start Run {\n\
                           always {\n\
                               n := n + 1;\n\
                               var tmp: Pair := {n, n + 20};\n\
                               a := tmp.lo;\n\
                           }\n\
                           ref Run: n < 100;\n\
                       }\n";

/// Исправлениетура с локальным массивом структур: поглотитель обязан склеивать его поэлементно -
/// `&{1'b0, tmp}` над распакованным массивом yosys встречает "Invalid array access".
const PARTIAL_ARRAY: &str = "struct Pair { lo: u8, hi: u8 }\n\
                             var n: u8 := 0;\n\
                             out a: u8 at 0;\n\
                             start Run {\n\
                                 always {\n\
                                     n := n + 1;\n\
                                     var cells: [Pair; 2] := {{n, n + 1}, {n + 2, n + 3}};\n\
                                     a := cells[0].lo;\n\
                                 }\n\
                                 ref Run: n < 100;\n\
                             }\n";

fn generate_source(tag: &str, source: &str) -> PathBuf {
    let dir = build_dir(tag);
    takt_lang::compile_to_sv(
        "partial",
        source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение SystemVerilog");
    dir
}

/// Поле, записанное но не прочитанное, поглощается: проверка цели зелён.
///
/// Класс виден **verilator**, а не yosys: `%Warning-UNUSEDSIGNAL: Bits of signal are
/// not used: 'tmp'[15:8]`, и проверка цели считает предупреждение ошибкой - при нулевом
/// коде возврата `taktc`.
#[test]
fn partially_used_struct_local_passes_the_lint() {
    if !verilator_available() {
        eprintln!("[ПРОПУСК] partially_used_struct_local_passes_the_lint: нет verilator");
        return;
    }
    for (tag, source) in [("partial", PARTIAL), ("partial_array", PARTIAL_ARRAY)] {
        let dir = generate_source(tag, source);
        let out = Command::new("verilator")
            .current_dir(&dir)
            .args(["--lint-only", "-Wall", "partial.sv"])
            .output()
            .expect("запуск verilator");
        assert!(
            out.status.success(),
            "линт цели обязан принять вывод ('{tag}'):\n{}",
            String::from_utf8_lossy(&out.stderr)
        );
        let _ = std::fs::remove_dir_all(&dir);
    }
}

/// Тот же вывод синтезируется, и поглотитель массива идёт поэлементно.
#[test]
fn absorber_of_unpacked_array_is_elementwise() {
    let dir = generate_source("partial_array_text", PARTIAL_ARRAY);
    let sv = std::fs::read_to_string(dir.join("partial.sv")).expect("вывод цели");
    assert!(
        sv.contains("_unused_cells = &{1'b0, cells[0].lo, cells[0].hi, cells[1].lo, cells[1].hi};"),
        "поглотитель распакованного массива обязан склеивать ЭЛЕМЕНТЫ:\n{sv}"
    );
    if !yosys_available() {
        eprintln!("[ПРОПУСК] absorber_of_unpacked_array_is_elementwise: нет yosys");
        return;
    }
    let synth = Command::new("yosys")
        .current_dir(&dir)
        .args([
            "-q",
            "-p",
            "read_verilog -sv partial.sv; synth -top partial",
        ])
        .output()
        .expect("запуск yosys");
    assert!(
        synth.status.success(),
        "порождённый SystemVerilog не синтезируется:\n{}",
        String::from_utf8_lossy(&synth.stderr)
    );
    let _ = std::fs::remove_dir_all(&dir);
}
