//! Двунаправленный порт: эталон против прошивки цели `c`.

use std::path::{Path, PathBuf};
use std::process::Command;

const FIXTURE: &str = "tests/data/eval/conformance_inout_port.takt";
const UNIT: &str = "conformance_inout_port";
const TICKS: usize = 3;

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

fn build_dir(tag: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!(
        "takt_0421_{tag}_{}_{}",
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

/// Потактовые значения двунаправленного порта у эталона.
fn simulator_trace() -> Vec<i128> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор фикстуры");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = takt_sim::build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    for _ in 0..TICKS {
        let _ = unit.tick();
        match unit.variable("line") {
            Some(takt_sim::Value::Number(v)) => trace.push(v),
            other => panic!("порт 'line' обязан быть числом, получено {other:?}"),
        }
    }
    trace
}

/// Те же значения у прошивки: подставное железо держит один регистр - чтение и запись
/// идут по одному адресу, как на плате.
fn generated_c_trace(dir: &Path) -> Vec<i128> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_c(
        UNIT,
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");

    let harness = format!(
        r#"#include <stdio.h>
#include "{UNIT}.h"

static long long reg = 0;

static int64_t on_read(ConformanceInoutPort_In_NumericPort port, uint8_t index, void *ud) {{
    (void)index;
    (void)ud;
    (void)port;
    return (int64_t)reg;
}}

static void on_write(ConformanceInoutPort_Out_NumericPort port, uint8_t index, int64_t value, void *ud) {{
    (void)index;
    (void)ud;
    (void)port;
    reg = (long long)value;
}}

int main(void) {{
    ConformanceInoutPort m;
    ConformanceInoutPort_init(&m);
    m.read_numeric = on_read;
    m.write_numeric = on_write;
    m.userdata = 0;
    for (int i = 0; i < {TICKS}; i++) {{
        ConformanceInoutPort_tick(&m);
        printf("%lld\n", reg);
    }}
    return 0;
}}
"#
    );
    std::fs::write(dir.join("harness.c"), &harness).expect("запись харнесса");

    let bin = dir.join("inout_bin");
    let compile = Command::new("cc")
        .args([
            "-std=c11",
            "-Wall",
            "-Wextra",
            "-Wno-unused-parameter",
            "-Werror",
            "-o",
        ])
        .arg(&bin)
        .arg(dir.join("harness.c"))
        .arg(dir.join(format!("{UNIT}.c")))
        .arg("-I")
        .arg(dir)
        .output()
        .expect("запуск cc");
    assert!(
        compile.status.success(),
        "cc не собрал харнесс флагами гейта цели:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let run = Command::new(&bin).output().expect("запуск харнесса");
    assert!(run.status.success(), "харнесс завершился с ошибкой");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .map(|line| line.trim().parse::<i128>().expect("число в выводе"))
        .collect()
}

/// Предмет: вывод собирается, и значения совпадают с эталоном.
#[test]
fn inout_port_values_match_the_reference() {
    let sim = simulator_trace();
    assert_eq!(
        sim,
        vec![1, 3, 6],
        "эталон: значение накапливается через чтение того же порта"
    );

    if !cc_available() {
        eprintln!("[ПРОПУСК] inout_port_values_match_the_reference: cc не найден");
        return;
    }
    let dir = build_dir("trace");
    let c = generated_c_trace(&dir);
    assert_eq!(sim, c, "значения двунаправленного порта обязаны совпадать");
    let _ = std::fs::remove_dir_all(&dir);
}

/// Имена перечислителей двунаправленного порта различаются по стороне.
///
/// Сегмент печатается только двунаправленному порту: имена портов видны пользователю
/// (сигнатура HAL-колбэка), и смена формы у однонаправленных была бы ломающей без
/// нужды. На это стоит контрольная проверка.
#[test]
fn inout_enumerators_differ_by_side() {
    let dir = build_dir("names");
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_c(
        UNIT,
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");
    let header = std::fs::read_to_string(dir.join(format!("{UNIT}.h"))).expect("чтение заголовка");
    assert!(
        header.contains("CONFORMANCE_INOUT_PORT_PORT_LINE_IN")
            && header.contains("CONFORMANCE_INOUT_PORT_PORT_LINE_OUT"),
        "двунаправленный порт обязан дать два разных перечислителя:\n{header}"
    );

    // Контроль: однонаправленный порт имя не меняет.
    let plain = "out probe: u8 at 0x100;\nvar e: u8 := 0;\n\
                 start Run { always { e := e + 1; probe := e; } ref Run; }\n";
    let dir2 = build_dir("plain");
    takt_lang::compile_to_c(
        "plain_port",
        plain,
        dir2.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");
    let header2 = std::fs::read_to_string(dir2.join("plain_port.h")).expect("чтение заголовка");
    assert!(
        header2.contains("PLAIN_PORT_PORT_PROBE =") && !header2.contains("PORT_PROBE_OUT"),
        "у однонаправленного порта имя обязано остаться прежним:\n{header2}"
    );
    let _ = std::fs::remove_dir_all(&dir);
    let _ = std::fs::remove_dir_all(&dir2);
}

// --------------------------------------------------------------------------- Цель
// `sv`: три сигнала вместо трёхстабильной шины
// ---------------------------------------------------------------------------
//
// Форму выбрал 2026-08-23: `line_i` (вход), `line_o` (выход) и строб `line_we`. Это
// ровно та механика, что у цели `c` - плата держит ячейку, модуль её читает и пишет.
// Трёхстабильная шина отвергнута: внутри кристалла её нет (yosys: "limited support for
// tri-state logic"), а сигнал разрешения пришлось бы выводить из модели, которая о нём
// молчит.

fn tool(bin: &str) -> bool {
    Command::new(bin)
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
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

/// Трасса RTL: ячейку держит тестбенч - как плата и как харнесс цели `c`.
fn generated_sv_trace(dir: &Path) -> Vec<i128> {
    generate_sv(dir);
    let tb = format!(
        r#"module tb;
    logic clk = 0, rst_n = 0;
    logic [7:0] line_i, line_o;
    logic line_we, is_done;
    // "Плата": ячейка, которую модуль читает и пишет. Значение, записанное в
    // такте N, видно чтению такта N+1 - как у эталона; поэтому чтение идёт
    // через тот же строб, а не через регистр ячейки (регистр обновится лишь на
    // следующем фронте, и трасса сдвинулась бы на такт).
    logic [7:0] mem;
    always_ff @(posedge clk) begin
        if (!rst_n) mem <= '0;
        else if (line_we) mem <= line_o;
    end
    assign line_i = line_we ? line_o : mem;
    {UNIT} dut (.clk(clk), .rst_n(rst_n), .line_i(line_i), .line_o(line_o),
                .line_we(line_we), .is_done(is_done));
    always #5 clk = ~clk;
    initial begin
        @(posedge clk);
        rst_n <= 1'b1;
        for (int i = 0; i < {TICKS}; i++) begin
            @(posedge clk);
            #1 $display("TICK %0d", line_we ? line_o : mem);
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

/// Значения двунаправленного порта в RTL совпадают с эталоном.
///
/// Сверка значений обязательна: перепутанные стороны (`_i` вместо `_o`) дают валидный,
/// синтезируемый модуль - и другой автомат.
#[test]
fn inout_port_values_match_generated_sv() {
    if !tool("verilator") {
        eprintln!("[ПРОПУСК] inout_port_values_match_generated_sv: нет verilator");
        return;
    }
    let dir = build_dir("sv_trace");
    let sim = simulator_trace();
    let rtl = generated_sv_trace(&dir);
    assert_eq!(sim, rtl, "трассы эталона и RTL обязаны совпадать");
    let _ = std::fs::remove_dir_all(&dir);
}

/// Строб поднят только в такте записи: умолчание - ноль, а не "как есть".
///
/// Без этого строб залипает после первой же записи, и плата затирает ячейку каждый такт -
/// модель, записавшая порт однажды, вела бы линию вечно. На значениях исправлениетуры это
/// невидимо (она пишет каждый такт), поэтому проверка смотрит текст.
#[test]
fn inout_strobe_defaults_to_zero() {
    let dir = build_dir("sv_text");
    generate_sv(&dir);
    let sv = std::fs::read_to_string(dir.join(format!("{UNIT}.sv"))).expect("чтение модуля");
    assert!(
        sv.contains("line_we_next = 1'b0;"),
        "умолчание строба обязано быть нулём:\n{sv}"
    );
    assert!(
        sv.contains("line_we_next = 1'b1;"),
        "запись порта обязана поднимать строб:\n{sv}"
    );
    assert!(
        sv.contains("input  logic [7:0] line_i") && sv.contains("output logic [7:0] line_o"),
        "порт обязан развернуться в три сигнала:\n{sv}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// Стороны не перепутаны: чтение идёт с `_i`, запись - в `_o`.
///
/// Проверка смотрит текст, и это названная граница. Мутация подтвердила: сверка
/// значений её пропускает, ловит только линт (`UNUSEDSIGNAL` на неподключённом входе) -
/// а линт молчал бы, начни модель читать порт где-то ещё.
#[test]
fn inout_sides_are_not_swapped() {
    let dir = build_dir("sv_sides");
    generate_sv(&dir);
    let sv = std::fs::read_to_string(dir.join(format!("{UNIT}.sv"))).expect("чтение модуля");
    let body = sv.split("always_comb").nth(1).expect("тело always_comb");
    assert!(
        body.contains("line_i + "),
        "чтение порта обязано идти со стороны входа:\n{body}"
    );
    assert!(
        body.contains("line_o_next = "),
        "запись порта обязана идти в сторону выхода:\n{body}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// Порождённый модуль проходит линт цели и синтезируется.
///
/// Два инструмента обязательны: трёхстабильную форму, которую эта фича отвергла,
/// verilator принимает молча, а yosys встречает "limited support for tri-state logic".
#[test]
fn inout_port_sv_passes_tools() {
    if !tool("verilator") {
        eprintln!("[ПРОПУСК] inout_port_sv_passes_tools: нет verilator");
        return;
    }
    let dir = build_dir("sv_tools");
    generate_sv(&dir);
    let lint = Command::new("verilator")
        .current_dir(&dir)
        .args([
            "--lint-only",
            "-Wall",
            "--top-module",
            UNIT,
            &format!("{UNIT}.sv"),
        ])
        .output()
        .expect("запуск verilator");
    assert!(
        lint.status.success(),
        "линт цели отверг модуль:\n{}",
        String::from_utf8_lossy(&lint.stderr)
    );
    if tool("yosys") {
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
            "модуль не синтезируется:\n{}",
            String::from_utf8_lossy(&synth.stderr)
        );
    }
    let _ = std::fs::remove_dir_all(&dir);
}
