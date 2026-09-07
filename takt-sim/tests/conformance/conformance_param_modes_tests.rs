//! Сверка режимов генерации параметров, (,   анализа).
//!
//! # Что доказывается
//!
//! Одна и та же программа под `--parameters=assign` и `--parameters=specialize` даёт
//! **одинаковую потактовую трассу**, и обе совпадают с трассой симулятора. Это
//! **главный тест гибрида**: два пути кодогена реализуют один смысл, и разойдясь, они
//! дали бы одной программе два поведения - узнал бы об этом тот, кто сменил флаг (риск,
//! названный первым).
//!
//! Сверяются **четыре** трассы, а не две: симулятор строит дерево в том же режиме, что
//! и цель (`construct_model_with_files(..., specialize)`), поэтому режим проверяется и
//! в эталоне, и в цели:
//!
//! | Трасса | Дерево | Исполнитель |
//! |---|---|---|
//! | 1 | `assign` | симулятор |
//! | 2 | `specialize` | симулятор |
//! | 3 | `assign` | порождённый C |
//! | 4 | `specialize` | порождённый C |
//!
//! Форма вывода при этом **разная**, и именно поэтому сверка нужна: в `assign`
//! экземпляры зовутся `main.tuner0`/`main.tuner1` и настройка лежит в поле, в
//! `specialize` - `main.tuner_p10`/`main.tuner_p21`, и настройка стала константой.
//! Совпадение трасс при разной форме - это ровно то утверждение, которое решение
//! обязано было доказать.

use std::path::Path;
use std::process::Command;
use takt_lang::semantic::tree::construct_model_with_files;
use takt_sim::{TickResult, Unit, build_unit};

/// Два экземпляра одной модели с разными настройками.
///
/// `gain`, а не `step`: `step` занят стандартной библиотекой IEC (`ST-014`).
/// Самопереход `ref Count;` обязателен - состояние без исходящих `ref` цель `c`
/// завершает после первого такта, и трасса застыла бы.
const SRC: &str = "model Tuner {\n\
                   \x20   parameter gain: u8 := 1;\n\
                   \x20   var acc: u8 := 0;\n\
                   \x20   start Count {\n\
                   \x20       always { acc := acc + gain; }\n\
                   \x20       ref Count;\n\
                   \x20   }\n\
                   }\n\
                   \n\
                   start Main = Tuner(gain := 100) | Tuner(gain := 200);\n";

/// Тактов сверки. Настройки 100 и 200 при `u8` дают обёртку уже на третьем такте (300
/// mod 256 = 44): переполнение в сверке намеренно (норма 0127).
const TICKS: usize = 6;

/// Имена полей экземпляров в порождённом C - **свои у каждого режима**.
///
/// В `assign` модель одна и экземпляры нумеруются (`tuner0`/`tuner1`); в `specialize`
/// каждый экземпляр - своя копия модели (`tuner_p10`/`tuner_p21`). Имена сняты
/// **пробой** порождённого заголовка, а не угаданы (правило "сперва зонд, затем
/// проверки").
const FIELDS_ASSIGN: (&str, &str) = ("tuner0", "tuner1");
const FIELDS_SPECIALIZE: (&str, &str) = ("tuner_p10", "tuner_p21");

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Юнит из исходника в заданном режиме параметров.
fn unit_of(src: &str, specialize: bool) -> Unit {
    let (ast, _) = takt_lang::parse(src, 0).expect("разбор");
    let mut files = takt_lang::diagnostics::FileTable::default();
    let model =
        construct_model_with_files(&ast, None, &[], &mut files, specialize).expect("семантика");
    build_unit(model).expect("построение юнита")
}

/// Значения переменной `name` у всех экземпляров-детей, в порядке объявления.
///
/// Квалифицированная адресация (`Модель::имя`) не годится: в режиме `assign` оба
/// экземпляра - `Tuner`. Снимок (`state_io`) различает их структурно и потому работает
/// в **обоих** режимах.
fn variables_named(unit: &Unit, name: &str) -> Vec<i128> {
    fn walk(snap: &takt_sim::state_io::UnitSnapshot, name: &str, out: &mut Vec<i128>) {
        match snap {
            takt_sim::state_io::UnitSnapshot::None => {}
            takt_sim::state_io::UnitSnapshot::Node { variables, .. } => {
                if let Some(value) = variables
                    .get(name)
                    .and_then(|v| v.as_number())
                    .and_then(serde_json::Number::as_i128)
                {
                    out.push(value);
                }
            }
            takt_sim::state_io::UnitSnapshot::Parallel { children }
            | takt_sim::state_io::UnitSnapshot::Sequential { children, .. } => {
                for child in children {
                    walk(child, name, out);
                }
            }
        }
    }
    let mut out = Vec::new();
    walk(&takt_sim::state_io::snapshot(unit), name, &mut out);
    out
}

/// Трасса симулятора: пара накопителей на каждом такте.
fn sim_trace(src: &str, specialize: bool) -> Vec<(i128, i128)> {
    let mut unit = unit_of(src, specialize);
    let mut trace = Vec::new();
    for tick in 1..=TICKS {
        assert!(
            !matches!(unit.tick(), TickResult::Failed(_)),
            "такт {tick}: симулятор отказал (specialize = {specialize})"
        );
        let values = variables_named(&unit, "acc");
        assert_eq!(
            values.len(),
            2,
            "такт {tick}: ожидались два экземпляра, снимок дал {values:?}"
        );
        trace.push((values[0], values[1]));
    }
    trace
}

/// Трасса порождённого C в заданном режиме: порождает, собирает с харнессом, исполняет
/// и читает значения накопителей.
fn c_trace(dir: &Path, specialize: bool, fields: (&str, &str)) -> Vec<(i128, i128)> {
    let _ = std::fs::remove_dir_all(dir);
    std::fs::create_dir_all(dir).expect("каталог");

    // `GenerateOptions` - `#[non_exhaustive]`: поле правится после `default()`.
    let mut options = takt_lang::generator::GenerateOptions::default();
    options.specialize = specialize;
    takt_lang::compile_to_c(
        "modes_conf",
        SRC,
        dir.to_str().expect("путь"),
        &[],
        &options,
    )
    .expect("порождение C");

    let (first, second) = fields;
    let harness = format!(
        r#"#include <stdio.h>
#include "modes_conf.h"

int main(void) {{
    ModesConf m;
    ModesConf_init(&m);
    for (int i = 0; i < {TICKS}; i++) {{
        ModesConf_tick(&m);
        printf("%d %d\n", (int)m.main.{first}.acc, (int)m.main.{second}.acc);
    }}
    return 0;
}}
"#
    );
    let harness_path = dir.join("harness.c");
    std::fs::write(&harness_path, &harness).expect("запись харнесса");

    let bin = dir.join("modes_conf_bin");
    let compile = Command::new("cc")
        .args(["-std=c11", "-Wall", "-Werror", "-o"])
        .arg(&bin)
        .arg(&harness_path)
        .arg(dir.join("modes_conf.c"))
        .arg("-I")
        .arg(dir)
        .output()
        .expect("запуск cc");
    assert!(
        compile.status.success(),
        "cc не собрал харнесс (specialize = {specialize}):\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let run = Command::new(&bin).output().expect("запуск харнесса");
    assert!(run.status.success(), "харнесс завершился с ошибкой");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .map(|line| {
            let mut parts = line.split_whitespace().map(|p| p.parse::<i128>().unwrap());
            (parts.next().unwrap(), parts.next().unwrap())
        })
        .collect()
}

// --- Режимы совпадают по поведению -----------------------------------

/// Четыре трассы - симулятор и цель `c` в обоих режимах - совпадают потактово.
#[test]
fn both_modes_and_simulator_agree_tick_by_tick() {
    let sim_assign = sim_trace(SRC, false);
    let sim_specialize = sim_trace(SRC, true);

    // Ожидание считается независимо от обоих исполнителей: совпадение двух реализаций
    // между собой ещё не значит, что они правы.
    let mut expected = Vec::new();
    let (mut fast, mut slow) = (0i128, 0i128);
    for _ in 0..TICKS {
        fast = (fast + 100) % 256;
        slow = (slow + 200) % 256;
        expected.push((fast, slow));
    }
    assert_eq!(sim_assign, expected, "трасса симулятора в режиме assign");
    assert_eq!(
        sim_specialize, expected,
        "трасса симулятора в режиме specialize"
    );

    if !cc_available() {
        eprintln!(
            "[ПРОПУСК] сверка с целью `c`: компилятор `cc` не найден; \
             трассы симулятора в обоих режимах уже сверены"
        );
        return;
    }
    let base = std::env::temp_dir().join(format!("takt_pid{}", std::process::id()));
    let c_assign = c_trace(&base.join("takt-0185-07-assign"), false, FIELDS_ASSIGN);
    let c_specialize = c_trace(
        &base.join("takt-0185-07-specialize"),
        true,
        FIELDS_SPECIALIZE,
    );
    assert_eq!(c_assign, expected, "трасса цели c в режиме assign");
    assert_eq!(
        c_specialize, expected,
        "трасса цели c в режиме specialize — форма вывода другая, поведение то же"
    );
}

// --- Поведение специализаций в целях `rust` и `sv` ---------------------------

/// Фикстура, в которой **обе** настройки влияют на одно наблюдаемое значение.
///
/// Зачем так: в целях `rust` и `sv` константы живут в **общем** пространстве
/// имён модуля, и их квалифицировала - но проверила это
/// **текстом**. Текст доказывает, что имена разные; что исполнение берёт **свою**
/// константу, доказывает только прогон. Здесь оба экземпляра прибавляют свой
/// `gain` к общей переменной, поэтому подмена одной константы другой сразу видна
/// в значении: 100 + 200 = 300 (44 с обёрткой `u8`), а не 200.
///
/// Порт пишется **внутри** под-модели, а не блоком `always` корня: проба 2026-07-30
/// показала, что model-level `always` у модели, состояние которой - композиция,
/// симулятор **теряет** (в цели `c` он исполняется). Дефект записан находкой в
/// `FEATURES.md`; фикстура на него не опирается.
const SHARED: &str = "out sum: u8;\n\
                      var total: u8 := 0;\n\
                      model Tuner {\n\
                      \x20   parameter gain: u8 := 1;\n\
                      \x20   start Count {\n\
                      \x20       always { total := total + gain; sum := total; }\n\
                      \x20       ref Count;\n\
                      \x20   }\n\
                      }\n\
                      start Main = Tuner(gain := 100) | Tuner(gain := 200);\n";

/// Ожидаемая трасса общей суммы: обе настройки применены, обёртка `u8` - по 0127.
fn shared_expected() -> Vec<i128> {
    let mut total = 0i128;
    (0..TICKS)
        .map(|_| {
            total = (total + 300) % 256;
            total
        })
        .collect()
}

fn tool_available(tool: &str, arg: &str) -> bool {
    Command::new(tool)
        .arg(arg)
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Трасса симулятора по переменной `total` (обе настройки в одном значении).
fn shared_sim_trace(specialize: bool) -> Vec<i128> {
    let mut unit = unit_of(SHARED, specialize);
    (0..TICKS)
        .map(|_| {
            assert!(!matches!(unit.tick(), TickResult::Failed(_)));
            match unit.variable("total") {
                Some(takt_sim::Value::Number(n)) => n,
                other => panic!("переменная 'total': неожиданное значение {other:?}"),
            }
        })
        .collect()
}

/// Цель `rust` в режиме `specialize`: каждая специализация обязана считать со
/// **своей** константой.
#[test]
fn specialized_constants_drive_generated_rust() {
    if !tool_available("rustc", "--version") {
        eprintln!("[ПРОПУСК] specialized_constants_drive_generated_rust: rustc не найден");
        return;
    }
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join("takt-0185-07-rust");
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");

    let mut options = takt_lang::generator::GenerateOptions::default();
    options.specialize = true;
    takt_lang::compile_to_rust(
        "modes_shared",
        SHARED,
        dir.to_str().expect("путь"),
        &[],
        &options,
    )
    .expect("порождение Rust");

    // Драйвер принадлежит проверке, а не продукту: поля модели приватны,
    // и наблюдение идёт через порт - ровно как с платы.
    let module = dir.join("modes_shared.rs");
    let driver = format!(
        r#"#[path = "{module}"]
mod generated;
use generated::{{Hal, ModesShared, OutU8Port}};
use std::cell::RefCell;
use std::rc::Rc;

/// Наблюдение выносится из HAL общей ячейкой: поле `hal` модели приватно и
/// аксессора не имеет - как и должно быть у прошивки.
struct Probe {{ reg: Rc<RefCell<u8>> }}

impl Hal for Probe {{
    fn write_u8(&mut self, port: OutU8Port, value: u8) {{
        assert!(matches!(port, OutU8Port::Sum));
        *self.reg.borrow_mut() = value;
    }}
}}

fn main() {{
    let reg = Rc::new(RefCell::new(0u8));
    let mut model = ModesShared::new(Probe {{ reg: Rc::clone(&reg) }});
    model.init();
    for _ in 0..{TICKS} {{
        model.tick();
        println!("TICK {{}}", reg.borrow());
    }}
}}
"#,
        module = module.display(),
    );
    std::fs::write(dir.join("driver.rs"), driver).expect("запись драйвера");
    let build = Command::new("rustc")
        .current_dir(&dir)
        .args(["--edition", "2021", "driver.rs", "-o", "driver"])
        .output()
        .expect("запуск rustc");
    assert!(
        build.status.success(),
        "rustc не собрал драйвер:\n{}",
        String::from_utf8_lossy(&build.stderr)
    );
    let run = Command::new(dir.join("driver"))
        .current_dir(&dir)
        .output()
        .expect("запуск драйвера");
    let trace: Vec<i128> = String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter_map(|line| line.strip_prefix("TICK "))
        .map(|v| v.trim().parse::<i128>().expect("значение — целое"))
        .collect();
    assert_eq!(
        trace,
        shared_expected(),
        "прошивка rust обязана считать со своими константами специализаций"
    );
    assert_eq!(
        shared_sim_trace(true),
        shared_expected(),
        "симулятор на специализированном дереве"
    );
}

/// Цель `sv` в режиме `specialize`: то же утверждение в RTL.
///
/// Наблюдение - иерархической ссылкой на выходной порт (законный отладочный механизм
/// языка, приём сверки 0045).
#[test]
fn specialized_constants_drive_generated_sv() {
    if !tool_available("verilator", "--version") {
        eprintln!("[ПРОПУСК] specialized_constants_drive_generated_sv: verilator не найден");
        return;
    }
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join("takt-0185-07-sv");
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");

    let mut options = takt_lang::generator::GenerateOptions::default();
    options.specialize = true;
    takt_lang::compile_to_sv(
        "modes_shared",
        SHARED,
        dir.to_str().expect("путь"),
        &[],
        &options,
    )
    .expect("порождение SystemVerilog");

    // Значения печатаются после фронта (`#1`), иначе читалось бы состояние до
    // защёлкивания (артефакт, названный ).
    let tb = format!(
        r#"module tb;
    logic clk = 0, rst_n = 0;
    logic [7:0] sum;
    logic is_done;
    modes_shared dut (.clk(clk), .rst_n(rst_n), .sum(sum), .is_done(is_done));
    always #5 clk = ~clk;
    initial begin
        @(posedge clk);
        rst_n <= 1'b1;
        repeat ({TICKS}) begin
            @(posedge clk);
            #1 $display("TICK %0d", sum);
        end
        $finish;
    end
endmodule
"#
    );
    std::fs::write(dir.join("tb.sv"), tb).expect("запись тестбенча");
    let build = Command::new("verilator")
        .current_dir(&dir)
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
            "modes_shared.sv",
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
        .current_dir(&dir)
        .output()
        .expect("запуск собранной симуляции");
    let trace: Vec<i128> = String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter_map(|line| line.strip_prefix("TICK "))
        .map(|v| v.trim().parse::<i128>().expect("значение — целое"))
        .collect();
    assert_eq!(
        trace,
        shared_expected(),
        "модуль sv обязан считать со своими localparam специализаций"
    );
}

/// "Локальная копия"  держится в **обоих** режимах: экземпляр меняет свой параметр -
/// сосед и объявление этого не видят.
///
/// В `assign` независимость обеспечена принадлежностью поля экземпляру, в `specialize` -
/// тем, что у экземпляров разные модели. Утверждение одно, пути разные, поэтому
/// проверяется дважды.
#[test]
fn local_copy_holds_in_both_modes() {
    const MUTABLE: &str = "model Tuner {\n\
                           \x20   parameter gain: u8 := 1;\n\
                           \x20   var acc: u8 := 0;\n\
                           \x20   var first: u8 := 1;\n\
                           \x20   start Count {\n\
                           \x20       always {\n\
                           \x20           acc := acc + gain;\n\
                           \x20           if first = 1 { gain := gain + 1; first := 0; }\n\
                           \x20       }\n\
                           \x20       ref Count;\n\
                           \x20   }\n\
                           }\n\
                           \n\
                           start Main = Tuner(gain := 10) | Tuner(gain := 20);\n";
    for specialize in [false, true] {
        let mut unit = unit_of(MUTABLE, specialize);
        assert!(!matches!(unit.tick(), TickResult::Failed(_)));
        assert_eq!(
            variables_named(&unit, "acc"),
            vec![10, 20],
            "такт 1, specialize = {specialize}"
        );
        assert_eq!(
            variables_named(&unit, "gain"),
            vec![11, 21],
            "каждый экземпляр поднял СВОЙ параметр, specialize = {specialize}"
        );
        assert!(!matches!(unit.tick(), TickResult::Failed(_)));
        assert_eq!(
            variables_named(&unit, "acc"),
            vec![21, 41],
            "изменение параметра в одном экземпляре не должно влиять на другой, \
             specialize = {specialize}"
        );
    }
}
