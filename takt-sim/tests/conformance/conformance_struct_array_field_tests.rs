//! Массив как поле структуры: эталон против `c` и RTL.

use std::path::{Path, PathBuf};
use std::process::Command;
use takt_lang::semantic::tree::construct_model;
use takt_sim::{Value, build_unit};

const FIXTURE: &str = "tests/data/eval/conformance_struct_array_field.takt";
const UNIT: &str = "conformance_struct_array_field";
const TICKS: usize = 3;

fn tool_available(bin: &str) -> bool {
    Command::new(bin)
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

fn build_dir(tag: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!(
        "takt_0422_{tag}_{}_{}",
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

/// Трасса эталона: `(probe, first, last)` по тактам.
fn simulator_trace() -> Vec<(i128, i128, i128)> {
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
        trace.push((value("probe"), value("first"), value("last")));
    }
    trace
}

/// Трасса порождённого RTL.
fn generated_sv_trace(dir: &Path) -> Vec<(i128, i128, i128)> {
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
    logic [7:0] probe, first, last;
    logic is_done;
    {UNIT} dut (.clk(clk), .rst_n(rst_n), .probe(probe), .first(first), .last(last), .is_done(is_done));
    always #5 clk = ~clk;
    initial begin
        @(posedge clk);
        rst_n <= 1'b1;
        for (int i = 0; i < {TICKS}; i++) begin
            @(posedge clk);
            #1 $display("TICK %0d %0d %0d", probe, first, last);
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
            let mut it = rest.split_whitespace();
            Some((
                it.next()?.parse::<i128>().ok()?,
                it.next()?.parse::<i128>().ok()?,
                it.next()?.parse::<i128>().ok()?,
            ))
        })
        .collect()
}

/// Эталон даёт ожидаемые числа - иначе "стороны сошлись" ничего не значит.
#[test]
fn reference_values_are_named() {
    assert_eq!(
        simulator_trace(),
        vec![(2, 2, 3), (5, 3, 3), (9, 4, 3)],
        "инициализатор поля-массива обязан доехать: last остаётся 3"
    );
}

/// Порождённый RTL считает то же, что эталон.
///
/// Порядок элементов в упакованной конкатенации обратный, и ошибка в нём зеркалит
/// значения: `first` и `last` в исправлениетуре разные именно ради этого.
#[test]
fn struct_array_field_matches_generated_sv() {
    if !tool_available("verilator") {
        eprintln!("[ПРОПУСК] struct_array_field_matches_generated_sv: verilator не найден");
        return;
    }
    let dir = build_dir("sv");
    let sim = simulator_trace();
    let rtl = generated_sv_trace(&dir);
    assert_eq!(sim, rtl, "трассы эталона и RTL обязаны совпадать");
    let _ = std::fs::remove_dir_all(&dir);
}

/// Цель `st`: вывод принимается `iec2c` - инициализатор ушёл в первый скан.
#[test]
fn struct_array_field_is_accepted_by_iec2c() {
    let iec2c = std::env::var("HOME")
        .map(|home| PathBuf::from(home).join(".local/bin/iec2c"))
        .unwrap_or_default();
    if !iec2c.exists() {
        eprintln!("[ПРОПУСК] struct_array_field_is_accepted_by_iec2c: iec2c не найден");
        return;
    }
    let dir = build_dir("st");
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_st(
        UNIT,
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение ST");
    let text = std::fs::read_to_string(dir.join(format!("{UNIT}.st"))).expect("чтение вывода");
    // Поле-массив исключено из агрегата объявления и кладётся первым сканом.
    assert!(
        !text.contains("data := ["),
        "агрегат объявления не должен нести поле-массив:\n{text}"
    );
    assert!(
        text.contains("b.data[0] := 1;") && text.contains("b.data[2] := 3;"),
        "значения поля-массива обязаны лечь операторами первого скана:\n{text}"
    );

    let lib = std::env::var("HOME")
        .map(|home| PathBuf::from(home).join(".local/share/matiec/lib"))
        .unwrap_or_default();
    let out = Command::new(&iec2c)
        .arg("-I")
        .arg(&lib)
        .arg("-T")
        .arg(&dir)
        .arg(dir.join(format!("{UNIT}.st")))
        .output()
        .expect("запуск iec2c");
    assert!(
        out.status.success(),
        "iec2c обязан принять вывод:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let _ = std::fs::remove_dir_all(&dir);
}
