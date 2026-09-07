//! Потактовая сверка **массива** цели `sv` с эталоном.

use std::path::{Path, PathBuf};
use std::process::Command;
use takt_lang::semantic::tree::construct_model;
use takt_sim::{Value, build_unit};

const FIXTURE: &str = "tests/data/eval/conformance_sv_array.takt";
const UNIT: &str = "svarray";
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

/// Трасса эталона: `(probe, first)` по тактам.
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
        trace.push((value("probe"), value("first")));
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
    logic [7:0] probe, first;
    logic is_done;
    {UNIT} dut (.clk(clk), .rst_n(rst_n), .probe(probe), .first(first), .is_done(is_done));
    always #5 clk = ~clk;
    initial begin
        @(posedge clk);
        rst_n <= 1'b1;
        for (int i = 0; i < {TICKS}; i++) begin
            @(posedge clk);
            #1 $display("TICK %0d %0d", probe, first);
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
            let first = it.next()?.parse::<i128>().ok()?;
            Some((probe, first))
        })
        .collect()
}

/// Значения массива совпадают у эталона и у RTL.
///
/// Ожидание записано **числами**: `arr[1] = 2` не меняется, поэтому `acc` идёт 2, 4, 6,
/// а `arr[0]` получает то же значение в том же такте - это и проверяет, что
/// инициализатор доехал (иначе `arr[1]` был бы нулём) и что индексная запись работает.
#[test]
fn array_values_match_generated_sv() {
    let sim = simulator_trace();
    assert_eq!(
        sim,
        vec![(2, 2), (4, 4), (6, 6)],
        "эталон обязан читать инициализированный массив: {sim:?}"
    );

    if !verilator_available() {
        eprintln!("[ПРОПУСК] array_values_match_generated_sv: verilator не найден");
        return;
    }
    let dir = build_dir("sv_array");
    let rtl = generated_sv_trace(&dir);
    assert_eq!(rtl.len(), TICKS, "тестбенч обязан напечатать каждый такт");
    assert_eq!(sim, rtl, "трассы разошлись\nsim={sim:?}\nRTL={rtl:?}");
}

/// Число значений в агрегате сверяется с объявленным размером.
///
/// Без проверки лишний элемент уехал бы в шаблон присваивания, и `verilator` ответил бы
/// своей ошибкой о ширине - то есть отказ пришёл бы от **чужого** инструмента на
/// порождённом файле.
///
/// С такой вход отсекает **семантика** (`SE-123`) - раньше, чем цель до него доходит:
/// длину агрегата сверяют теперь все девять потребителей, а не одна цель. Проверка цели
/// осталась страховкой и проверяется здесь по коду: `SV-002` она вернуть уже не
/// успевает.
#[test]
fn aggregate_size_mismatch_is_refused() {
    let dir = build_dir("sv_array_bad");
    let source = "var arr: [u8; 3] := {1, 2};\nvar i: u8 := 0;\n\
         out probe: u8 at 0;\n\
         start Run { always { i := i + 1; probe := arr[0]; } ref Run: i < 100; }\n";
    let err = takt_lang::compile_to_sv(
        "svarraybad",
        source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect_err("несовпадение размера обязано отвергаться");
    assert_eq!(err.code.as_deref(), Some("SE-123"), "{err:?}");
}

// --: сброс распакованного массива и переменный индекс -------------

const INDEX_FIXTURE: &str = "tests/data/eval/conformance_sv_array_index.takt";
const INDEX_UNIT: &str = "svarrayindex";

/// Трасса эталона по фикстуре распакованного массива: `(probe, picked, spare)`.
fn index_simulator_trace() -> Vec<(i128, i128, i128)> {
    let source = std::fs::read_to_string(INDEX_FIXTURE).expect("фикстура читается");
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
        trace.push((value("probe"), value("picked"), value("spare")));
    }
    trace
}

/// Трасса порождённого RTL по той же фикстуре.
fn index_generated_sv_trace(dir: &Path) -> Vec<(i128, i128, i128)> {
    let source = std::fs::read_to_string(INDEX_FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_sv(
        INDEX_UNIT,
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение SystemVerilog");

    let tb = format!(
        r#"module tb;
    logic clk = 0, rst_n = 0;
    logic [7:0] probe, picked, spare;
    logic is_done;
    {INDEX_UNIT} dut (.clk(clk), .rst_n(rst_n), .probe(probe), .picked(picked),
                      .spare(spare), .is_done(is_done));
    always #5 clk = ~clk;
    initial begin
        @(posedge clk);
        rst_n <= 1'b1;
        for (int i = 0; i < {TICKS}; i++) begin
            @(posedge clk);
            #1 $display("TICK %0d %0d %0d", probe, picked, spare);
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
            &format!("{INDEX_UNIT}.sv"),
            "-o",
            "simtb",
        ])
        .output()
        .expect("запуск verilator");
    assert!(
        build.status.success(),
        "verilator не собрал тестбенч индексации:\n{}",
        String::from_utf8_lossy(&build.stderr)
    );
    let run = Command::new(dir.join("obj_dir").join("simtb"))
        .current_dir(dir)
        .output()
        .expect("запуск симуляции");
    assert!(run.status.success(), "симуляция RTL индексации упала");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter_map(|line| {
            let rest = line.strip_prefix("TICK ")?;
            let mut it = rest.split_whitespace();
            let probe = it.next()?.parse::<i128>().ok()?;
            let picked = it.next()?.parse::<i128>().ok()?;
            let spare = it.next()?.parse::<i128>().ok()?;
            Some((probe, picked, spare))
        })
        .collect()
}

/// Массив без инициализатора и переменный индекс: значения совпадают с RTL.
///
/// До порождённый модуль **не проходил** проверка собственной цели: сброс печатался `'0`
/// (verilator: "CONST is not an unpacked array"), а индекс - полной шириной
/// (`WIDTHTRUNC`, который проверка считает ошибкой). Оба раза - при **нулевом** коде
/// возврата `taktc`.
///
/// Порт `spare` читает элемент, которого тело не касается: он и проверяет
/// **сброс**. Ошибка в нём дала бы `x` в RTL, а не другое число, - трасса
/// разойдётся.
#[test]
fn unpacked_array_reset_and_variable_index_match_generated_sv() {
    let sim = index_simulator_trace();
    assert_eq!(
        sim,
        vec![(1, 1, 0), (3, 2, 0), (6, 3, 0)],
        "эталон: acc = 1, 3, 6; picked = idx + 1; нетронутый элемент — ноль: {sim:?}"
    );

    if !verilator_available() {
        eprintln!(
            "[ПРОПУСК] unpacked_array_reset_and_variable_index_match_generated_sv: \
             verilator не найден"
        );
        return;
    }
    let dir = build_dir("sv_array_index");
    let rtl = index_generated_sv_trace(&dir);
    assert_eq!(rtl.len(), TICKS, "тестбенч обязан напечатать каждый такт");
    assert_eq!(sim, rtl, "трассы разошлись\nsim={sim:?}\nRTL={rtl:?}");
}

/// Порождённый модуль проходит **линт цели** - те же флаги, что у проверки.
///
/// Сверка значений выше этого не доказывает: тестбенч собирается с `-Wno-fatal`,
/// поэтому `WIDTHTRUNC` её не роняет (проверено мутацией "не сужать индекс" - трасса
/// совпадала). Сужение индекса проверяет только линт, а проверка цели считает предупреждение
/// ошибкой.
#[test]
fn unpacked_array_passes_target_lint() {
    if !verilator_available() {
        eprintln!("[ПРОПУСК] unpacked_array_passes_target_lint: verilator не найден");
        return;
    }
    let dir = build_dir("sv_array_lint");
    let source = std::fs::read_to_string(INDEX_FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_sv(
        INDEX_UNIT,
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение SystemVerilog");

    let lint = Command::new("verilator")
        .current_dir(&dir)
        .args([
            "--lint-only",
            "-Wall",
            "--top-module",
            INDEX_UNIT,
            &format!("{INDEX_UNIT}.sv"),
        ])
        .output()
        .expect("запуск verilator");
    assert!(
        lint.status.success(),
        "порождённый SystemVerilog не проходит линт цели:\n{}",
        String::from_utf8_lossy(&lint.stderr)
    );
}

// --: массив структур ---------------------------------------------

const STRUCT_FIXTURE: &str = "tests/data/eval/conformance_sv_struct_array.takt";
const STRUCT_UNIT: &str = "svstructarray";

/// Трасса эталона по фикстуре массива структур: `(head, tail, spare)`.
fn struct_simulator_trace() -> Vec<(i128, i128, i128)> {
    let source = std::fs::read_to_string(STRUCT_FIXTURE).expect("фикстура читается");
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
        trace.push((value("head"), value("tail"), value("spare")));
    }
    trace
}

/// Трасса порождённого RTL по той же фикстуре.
fn struct_generated_sv_trace(dir: &Path) -> Vec<(i128, i128, i128)> {
    let source = std::fs::read_to_string(STRUCT_FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_sv(
        STRUCT_UNIT,
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение SystemVerilog");

    let tb = format!(
        r#"module tb;
    logic clk = 0, rst_n = 0;
    logic [7:0] head, tail, spare;
    logic is_done;
    {STRUCT_UNIT} dut (.clk(clk), .rst_n(rst_n), .head(head), .tail(tail),
                       .spare(spare), .is_done(is_done));
    always #5 clk = ~clk;
    initial begin
        @(posedge clk);
        rst_n <= 1'b1;
        for (int i = 0; i < {TICKS}; i++) begin
            @(posedge clk);
            #1 $display("TICK %0d %0d %0d", head, tail, spare);
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
            &format!("{STRUCT_UNIT}.sv"),
            "-o",
            "simtb",
        ])
        .output()
        .expect("запуск verilator");
    assert!(
        build.status.success(),
        "verilator не собрал тестбенч массива структур:\n{}",
        String::from_utf8_lossy(&build.stderr)
    );
    let run = Command::new(dir.join("obj_dir").join("simtb"))
        .current_dir(dir)
        .output()
        .expect("запуск симуляции");
    assert!(run.status.success(), "симуляция RTL массива структур упала");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter_map(|line| {
            let rest = line.strip_prefix("TICK ")?;
            let mut it = rest.split_whitespace();
            let head = it.next()?.parse::<i128>().ok()?;
            let tail = it.next()?.parse::<i128>().ok()?;
            let spare = it.next()?.parse::<i128>().ok()?;
            Some((head, tail, spare))
        })
        .collect()
}

/// Массив структур: значения совпадают с RTL.
///
/// Наблюдаемые различают **поле** и **элемент**: `head` - поле первого элемента, `tail` -
/// другое поле второго. `spare` читает нетронутую переменную и проверяет **сброс по
/// полям**.
#[test]
fn struct_array_values_match_generated_sv() {
    let sim = struct_simulator_trace();
    assert_eq!(
        sim,
        vec![(11, 21, 0), (12, 22, 0), (13, 23, 0)],
        "эталон: head = n + 10, tail = n + 20, нетронутая переменная — ноль: {sim:?}"
    );

    if !verilator_available() {
        eprintln!("[ПРОПУСК] struct_array_values_match_generated_sv: verilator не найден");
        return;
    }
    let dir = build_dir("sv_struct_array");
    let rtl = struct_generated_sv_trace(&dir);
    assert_eq!(rtl.len(), TICKS, "тестбенч обязан напечатать каждый такт");
    assert_eq!(sim, rtl, "трассы разошлись\nsim={sim:?}\nRTL={rtl:?}");
}

/// Порождённый модуль синтезируется: линт этого не доказывает.
///
/// Оба инструмента обязательны: verilator принимал вывод, где сброс печатался шаблоном
/// присваивания, а yosys отвергал его - "Assignment pattern is only supported for whole
/// unpacked array assignments". Сверка значений тоже молчала: её тестбенч собирает
/// verilator.
#[test]
fn struct_array_is_synthesizable() {
    if Command::new("yosys")
        .arg("-V")
        .output()
        .map(|o| !o.status.success())
        .unwrap_or(true)
    {
        eprintln!("[ПРОПУСК] struct_array_is_synthesizable: yosys не найден");
        return;
    }
    let dir = build_dir("sv_struct_array_synth");
    let source = std::fs::read_to_string(STRUCT_FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_sv(
        STRUCT_UNIT,
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение SystemVerilog");

    let synth = Command::new("yosys")
        .current_dir(&dir)
        .args([
            "-q",
            "-p",
            &format!("read_verilog -sv {STRUCT_UNIT}.sv; synth -top {STRUCT_UNIT}"),
        ])
        .output()
        .expect("запуск yosys");
    assert!(
        synth.status.success(),
        "порождённый SystemVerilog не синтезируется:\n{}",
        String::from_utf8_lossy(&synth.stderr)
    );
}

// --: массив в параметре функции ----------------------------------

const PARAM_FIXTURE: &str = "tests/data/eval/conformance_sv_array_param.takt";
const PARAM_UNIT: &str = "svarrayparam";

/// Трасса эталона по фикстуре массива в параметре: `(low, high, sum)`.
fn param_simulator_trace() -> Vec<(i128, i128, i128)> {
    let source = std::fs::read_to_string(PARAM_FIXTURE).expect("фикстура читается");
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
        trace.push((value("low"), value("high"), value("sum")));
    }
    trace
}

/// Трасса порождённого RTL по той же фикстуре.
fn param_generated_sv_trace(dir: &Path) -> Vec<(i128, i128, i128)> {
    let source = std::fs::read_to_string(PARAM_FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_sv(
        PARAM_UNIT,
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение SystemVerilog");

    let tb = format!(
        r#"module tb;
    logic clk = 0, rst_n = 0;
    logic [7:0] low, high, sum;
    logic is_done;
    {PARAM_UNIT} dut (.clk(clk), .rst_n(rst_n), .low(low), .high(high),
                      .sum(sum), .is_done(is_done));
    always #5 clk = ~clk;
    initial begin
        @(posedge clk);
        rst_n <= 1'b1;
        for (int i = 0; i < {TICKS}; i++) begin
            @(posedge clk);
            #1 $display("TICK %0d %0d %0d", low, high, sum);
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
            &format!("{PARAM_UNIT}.sv"),
            "-o",
            "simtb",
        ])
        .output()
        .expect("запуск verilator");
    assert!(
        build.status.success(),
        "verilator не собрал тестбенч параметра-массива:\n{}",
        String::from_utf8_lossy(&build.stderr)
    );
    let run = Command::new(dir.join("obj_dir").join("simtb"))
        .current_dir(dir)
        .output()
        .expect("запуск симуляции");
    assert!(
        run.status.success(),
        "симуляция RTL параметра-массива упала"
    );
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter_map(|line| {
            let rest = line.strip_prefix("TICK ")?;
            let mut it = rest.split_whitespace();
            let low = it.next()?.parse::<i128>().ok()?;
            let high = it.next()?.parse::<i128>().ok()?;
            let sum = it.next()?.parse::<i128>().ok()?;
            Some((low, high, sum))
        })
        .collect()
}

/// Массив в параметре функции: значения совпадают с RTL.
///
/// Наблюдаемые различают **края** массива и меняются по тактам: перепутанный порядок
/// конкатенации даёт валидный RTL с зеркальными значениями, а на постоянных числах
/// ошибка неотличима.
#[test]
fn array_parameter_values_match_generated_sv() {
    let sim = param_simulator_trace();
    assert_eq!(
        sim,
        vec![(1, 21, 22), (2, 22, 24), (3, 23, 26)],
        "эталон: low = n, high = n + 20, sum = low + high: {sim:?}"
    );

    if !verilator_available() {
        eprintln!("[ПРОПУСК] array_parameter_values_match_generated_sv: verilator не найден");
        return;
    }
    let dir = build_dir("sv_array_param");
    let rtl = param_generated_sv_trace(&dir);
    assert_eq!(rtl.len(), TICKS, "тестбенч обязан напечатать каждый такт");
    assert_eq!(sim, rtl, "трассы разошлись\nsim={sim:?}\nRTL={rtl:?}");
}

/// Порождённый модуль синтезируется: verilator этого не доказывает.
///
/// Именно здесь класс и жил: `input logic [7:0] a [0:2]` verilator принимает, а yosys
/// отвечает "input/output/inout ports cannot have unpacked dimensions".
#[test]
fn array_parameter_is_synthesizable() {
    if Command::new("yosys")
        .arg("-V")
        .output()
        .map(|o| !o.status.success())
        .unwrap_or(true)
    {
        eprintln!("[ПРОПУСК] array_parameter_is_synthesizable: yosys не найден");
        return;
    }
    let dir = build_dir("sv_array_param_synth");
    let source = std::fs::read_to_string(PARAM_FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_sv(
        PARAM_UNIT,
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение SystemVerilog");

    let synth = Command::new("yosys")
        .current_dir(&dir)
        .args([
            "-q",
            "-p",
            &format!("read_verilog -sv {PARAM_UNIT}.sv; synth -top {PARAM_UNIT}"),
        ])
        .output()
        .expect("запуск yosys");
    assert!(
        synth.status.success(),
        "порождённый SystemVerilog не синтезируется:\n{}",
        String::from_utf8_lossy(&synth.stderr)
    );
}
