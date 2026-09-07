//! Сверка симулятора с RTL цели `taktc -t sv-mmio`.
//!
//! # Что здесь проверяется
//!
//! - **основное:** потактовая трасса `sv-mmio`, снятая **через
//!   регистры**, совпадает с трассой симулятора. Проверка (verilator + yosys)
//!   доказывает валидность и синтезируемость, но не верность (: дефект
//!   "`always_comb` читает регистр вместо `_next`" приняли оба инструмента).
//! - запись шиной в бит `out` игнорируется; чтение бита `in`
//!   возвращает записанное.
//!
//! # Мягкая деградация
//!
//! Нет Verilator -> тест **пропускается с сообщением** (образец `cc_available()` в
//! `conformance_c_tests.rs`). В CI verilator обязателен - там краснеет проверка sv.

use std::path::{Path, PathBuf};
use std::process::Command;
use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Unit, Value, build_unit};

/// Опорная фикстура: выходные порты `counter` (`0x10`) и `flag` (`0x20`) эволюционируют
/// по тактам (автономно, без входов).
const MMIO_FIXTURE: &str = "tests/data/eval/conformance_mmio.takt";

/// Тактов в трассе симулятора - с запасом над её длиной.
const TRACE_TICKS: usize = 6;

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

/// Потактовая трасса симулятора: значения `vars` после каждого такта.
fn simulate_trace(fixture: &str, vars: &[&str]) -> Vec<Vec<i128>> {
    let source = std::fs::read_to_string(fixture).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение юнита");
    let mut trace = Vec::new();
    for _ in 0..TRACE_TICKS {
        let result = unit.tick();
        assert!(
            !matches!(result, TickResult::Failed(_)),
            "симуляция не должна падать: {result:?}"
        );
        trace.push(vars.iter().map(|v| sim_value(&unit, v)).collect());
        if result == TickResult::Terminated {
            break;
        }
    }
    trace
}

/// Каталог сборки под конкретный тест.
fn build_dir(tag: &str) -> PathBuf {
    // Имя потока в имени каталога: тесты идут параллельно, и два набора, собирающие в
    // один каталог, вычищают файлы друг друга - покраснение при этом невоспроизводимо.
    // Соседние `sv`-наборы имя потока несут; здесь его не было, и один прогон
    // 2026-09-02 покраснел без объяснения.
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_conformance_sv_mmio_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог сборки");
    dir
}

/// Компилирует фикстуру целью `sv-mmio` в каталог `dir` (без внешней карты).
fn compile_mmio(dir: &Path, fixture: &str, basename: &str) {
    let source = std::fs::read_to_string(fixture).expect("фикстура читается");
    takt_lang::compile_to_sv_mmio(
        basename,
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &[],
        &takt_lang::parse_defines(&[]).expect("пустой env адресов"),
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение sv-mmio");
}

/// Наблюдаемый регистр: адрес и срез `[bit +: width]` слова данных.
struct RegSpec {
    addr: i64,
    bit: u32,
    width: u32,
}

/// Потактовая трасса RTL цели `sv-mmio`, снятая через регистры.
///
/// На каждом такте тестбенч по очереди выставляет `reg_addr` на адрес наблюдаемого
/// регистра и читает срез `reg_rdata` - смотрит на модуль так же, как шинный мастер.
/// `addr_width`/`data_width` - ширины шин модуля.
fn sv_mmio_trace(
    dir: &Path,
    fixture: &str,
    basename: &str,
    regs: &[RegSpec],
    addr_width: u32,
    data_width: u32,
    ticks: usize,
) -> Vec<Vec<i128>> {
    compile_mmio(dir, fixture, basename);

    let reads: String = regs
        .iter()
        .map(|r| {
            format!(
                "            reg_addr = {aw}'h{addr:x}; #1 $write(\" %0d\", reg_rdata[{bit} +: {width}]);\n",
                aw = addr_width,
                addr = r.addr,
                bit = r.bit,
                width = r.width,
            )
        })
        .collect();
    // у ядра без входных регистров сигналов записи нет, и заводить к ним провода нельзя -
    // verilator отвечает `PINNOTFOUND`. Тестбенч следует за интерфейсом ядра ровно так
    // же, как адаптер шины: смотрит, что модуль объявил, а не что он объявлял раньше.
    let core = std::fs::read_to_string(dir.join(format!("{basename}.sv")))
        .expect("порождённый .sv читается");
    let writable = core.contains("reg_wen");
    let write_decls = if writable {
        format!(
            "    logic [{dw_hi}:0] reg_wdata = 0;\n    logic reg_wen = 0;\n",
            dw_hi = data_width - 1
        )
    } else {
        String::new()
    };
    let write_pins = if writable {
        "\n                    .reg_wdata(reg_wdata), .reg_wen(reg_wen),"
    } else {
        ""
    };
    let tb = format!(
        r#"module tb;
    logic clk = 0, rst_n = 0;
    logic is_done;
    logic [{aw_hi}:0] reg_addr = 0;
{write_decls}    logic [{dw_hi}:0] reg_rdata;
    {basename} dut (.clk(clk), .rst_n(rst_n), .reg_addr(reg_addr),{write_pins}
                    .reg_rdata(reg_rdata), .is_done(is_done));
    always #5 clk = ~clk;
    initial begin
        @(posedge clk);
        rst_n <= 1'b1;
        repeat ({ticks}) begin
            @(posedge clk);
            #1 $write("TICK");
{reads}            $display("");
        end
        $finish;
    end
endmodule
"#,
        aw_hi = addr_width - 1,
        dw_hi = data_width - 1,
    );
    std::fs::write(dir.join("tb.sv"), tb).expect("запись тестбенча");

    build_and_run(dir, basename)
        .lines()
        .filter_map(|line| line.strip_prefix("TICK "))
        .map(|rest| {
            rest.split_whitespace()
                .map(|v| v.parse::<i128>().expect("значение — целое"))
                .collect()
        })
        .collect()
}

/// Собирает тестбенч `tb.sv` + модуль `<basename>.sv` через Verilator (`--binary
/// --timing`), запускает и возвращает stdout.
fn build_and_run(dir: &Path, basename: &str) -> String {
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
            &format!("{}.sv", basename),
            "-o",
            "simtb",
        ])
        .output()
        .expect("запуск verilator");
    assert!(
        build.status.success(),
        "verilator не собрал тестбенч sv-mmio:\n{}",
        String::from_utf8_lossy(&build.stderr)
    );
    let run = Command::new(dir.join("obj_dir").join("simtb"))
        .current_dir(dir)
        .output()
        .expect("запуск собранной симуляции");
    assert!(
        run.status.success(),
        "тестбенч sv-mmio завершился с ошибкой:\n{}\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    String::from_utf8_lossy(&run.stdout).into_owned()
}

/// **Трасса `sv-mmio` через регистры = трасса
/// симулятора на каждом такте.**
#[test]
fn per_tick_trace_matches_generated_sv_mmio() {
    // Симулятор наблюдает порты по именам; адрес - метаданные, значение с 0.
    let sim = simulate_trace(MMIO_FIXTURE, &["counter", "flag"]);
    // Пиннинг: counter 1->2->3, flag встаёт в 1 на терминальном шаге.
    assert_eq!(
        sim,
        vec![
            vec![1, 0],
            vec![2, 0],
            vec![3, 1],
            vec![3, 1],
            vec![3, 1],
            vec![3, 1],
        ],
        "ожидаемая трасса симулятора: (counter, flag)"
    );

    if !verilator_available() {
        eprintln!(
            "[ПРОПУСК] per_tick_trace_matches_generated_sv_mmio: verilator не найден — \
             сверка через регистры не выполнена (трасса симулятора пришпилена выше)"
        );
        return;
    }
    let dir = build_dir("trace");
    // counter - u8 по 0x10 (биты [7:0]); flag - bit по 0x20 (бит 0). Ширина адреса 6
    // (max 0x20), ширина данных 8 (max bit+width) - выведены генератором.
    let regs = [
        RegSpec {
            addr: 0x10,
            bit: 0,
            width: 8,
        },
        RegSpec {
            addr: 0x20,
            bit: 0,
            width: 1,
        },
    ];
    let sv = sv_mmio_trace(
        &dir,
        MMIO_FIXTURE,
        "conformance_mmio",
        &regs,
        6,
        8,
        sim.len(),
    );
    assert_eq!(
        sim, sv,
        "трасса sv-mmio (через регистры) обязана совпасть с симулятором НА КАЖДОМ \
         такте (T4/A4).\nсимулятор={sim:?}\nRTL(шина)={sv:?}"
    );
}

/// **Запись шиной в бит `out` игнорируется; чтение бита `in`
/// возвращает записанное.** Тестбенч самопроверяющийся (`$fatal`).
#[test]
fn bus_write_ignored_for_out_and_readback_for_in() {
    if !verilator_available() {
        eprintln!("[ПРОПУСК] bus_write_ignored_for_out_and_readback_for_in: verilator не найден");
        return;
    }
    let dir = build_dir("io");
    compile_mmio(
        dir.as_path(),
        "tests/data/eval/conformance_mmio_io.takt",
        "conformance_mmio_io",
    );

    // cmd - in по 0x1 (бит 0); echo - out по 0x2 (бит 0). Ширина адреса 2, данных 1.
    let tb = r#"module tb;
    logic clk = 0, rst_n = 0;
    logic is_done;
    logic [1:0] reg_addr = 0;
    logic [0:0] reg_wdata = 0;
    logic reg_wen = 0;
    logic [0:0] reg_rdata;
    conformance_mmio_io dut (.clk(clk), .rst_n(rst_n), .reg_addr(reg_addr),
                             .reg_wdata(reg_wdata), .reg_wen(reg_wen),
                             .reg_rdata(reg_rdata), .is_done(is_done));
    always #5 clk = ~clk;
    // #1 после фронта перед снятием reg_wen: иначе блокирующее reg_wen=0 в этом
    // же слоте конкурирует с always_ff записи (гонка тестбенча, не модуля).
    task automatic bus_write(input [1:0] a, input [0:0] d);
        reg_addr = a; reg_wdata = d; reg_wen = 1'b1;
        @(posedge clk);
        #1;
        reg_wen = 1'b0;
    endtask
    initial begin
        @(posedge clk);
        rst_n <= 1'b1;
        #1;
        // Пишем cmd=1 по адресу 0x1, читаем обратно.
        bus_write(2'h1, 1'b1);
        reg_addr = 2'h1; #1;
        if (reg_rdata[0] !== 1'b1) $fatal(1, "T7: чтение in-бита не вернуло записанное шиной");
        // Вход дошёл до автомата: echo:= cmd (ещё такт).
        @(posedge clk); #1;
        reg_addr = 2'h2; #1;
        if (reg_rdata[0] !== 1'b1) $fatal(1, "вход не дошёл до автомата: echo != cmd");
        // Пишем 0 по адресу out-бита echo - запись обязана игнорироваться.
        bus_write(2'h2, 1'b0);
        reg_addr = 2'h2; #1;
        if (reg_rdata[0] !== 1'b1) $fatal(1, "T6: запись шиной в out-бит не проигнорирована");
        $display("MMIO_IO_OK");
        $finish;
    end
endmodule
"#;
    std::fs::write(dir.join("tb.sv"), tb).expect("запись тестбенча");
    let stdout = build_and_run(dir.as_path(), "conformance_mmio_io");
    assert!(
        stdout.contains("MMIO_IO_OK"),
        "тестбенч T6/T7 не дошёл до конца (см. $fatal):\n{stdout}"
    );
}
