//! Потактовая сверка начального значения выходного порта.
//!
//! # Что доказывается
//!
//! Значение из `:=` появляется на порте **до первого такта** - и у эталона
//! (симулятора), и у цели `c`, - а дальше трассы совпадают такт в такт.

use std::path::Path;
use std::process::Command;

const FIXTURE: &str = "tests/data/eval/conformance_port_init.takt";
const TICKS: usize = 6;

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

fn verilator_available() -> bool {
    Command::new("verilator")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Трасса эталона: значение `led` до первого такта и после каждого такта.
fn simulate_trace() -> Vec<i128> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = takt_sim::build_unit(model).expect("построение Unit");

    let mut trace = vec![led_of(&unit)];
    for _ in 0..TICKS {
        let r = unit.tick();
        assert!(
            !matches!(r, takt_sim::TickResult::Failed(_)),
            "падение: {r:?}"
        );
        trace.push(led_of(&unit));
    }
    trace
}

/// Значение порта `led` в текущем состоянии эталона.
fn led_of(unit: &takt_sim::Unit) -> i128 {
    match unit.variable("led") {
        Some(takt_sim::Value::Number(n)) => n,
        other => panic!("led: {other:?}"),
    }
}

/// Трасса порождённого C: колбэк `write_numeric` держит последнее записанное значение,
/// харнесс печатает его после `_init` и после каждого `_tick`.
///
/// Начальное значение колбэком не "подделывается": переменная харнесса стартует с
/// заведомо чужого числа (`-1`), поэтому нулевой элемент трассы равен `7` только если
/// `_init` действительно вызвал запись.
fn generated_c_trace(dir: &Path) -> Vec<i128> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура");
    takt_lang::compile_to_c(
        "conformance_port_init",
        &source,
        dir.to_str().expect("путь"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");

    let harness = format!(
        r#"#include <stdio.h>
#include "conformance_port_init.h"

static int64_t led = -1;
static void wr(ConformancePortInit_Out_NumericPort port, uint8_t index, int64_t v, void *ud) {{
    (void)index;
    (void)port; (void)ud; led = v;
}}

int main(void) {{
    ConformancePortInit m = {{0}};
    /* Колбэки - до `_init`: начальное значение порта уходит наружу уже там
       (наблюдаемое следствие задачи 0187-03). */
    m.write_numeric = wr;
    ConformancePortInit_init(&m);
    printf("TICK %lld\n", (long long)led);
    for (int tick = 1; tick <= {TICKS}; tick++) {{
        ConformancePortInit_tick(&m);
        printf("TICK %lld\n", (long long)led);
    }}
    return 0;
}}
"#
    );
    let harness_path = dir.join("harness_port_init.c");
    std::fs::write(&harness_path, harness).expect("харнесс");

    let bin = dir.join("conformance_port_init_bin");
    let compile = Command::new("cc")
        .args(["-std=c11", "-Wall", "-Werror", "-I"])
        .arg(dir)
        .arg(dir.join("conformance_port_init.c"))
        .arg(&harness_path)
        .arg("-o")
        .arg(&bin)
        .output()
        .expect("cc");
    assert!(
        compile.status.success(),
        "порождённый C не компилируется:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск");
    assert!(run.status.success(), "собранный C упал");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter_map(|l| l.strip_prefix("TICK ")?.trim().parse::<i128>().ok())
        .collect()
}

/// Трасса порождённого RTL: значение выходного порта после снятия сброса ("до первого
/// такта") и после каждого фронта.
///
/// Тестбенч пишется здесь, а не порождается `taktc`: тестбенч - принадлежность
/// проверки, а не продукта (решение ). Наблюдение - сам **порт** модуля, а не
/// внутренний сигнал: у цели `sv` начальное значение живёт в ветви сброса, и увидеть
/// его надо там же, где увидит плата.
fn sv_trace(dir: &Path) -> Vec<i128> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура");
    takt_lang::compile_to_sv(
        "conformance_port_init",
        &source,
        dir.to_str().expect("путь"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение SystemVerilog");

    let tb = format!(
        r#"module tb;
    logic clk = 0, rst_n = 0;
    logic is_done;
    logic [7:0] led;
    conformance_port_init dut (.clk(clk), .rst_n(rst_n), .led(led), .is_done(is_done));
    always #5 clk = ~clk;
    initial begin
        // Первый фронт снимает сброс: в регистрах уже стартовые состояния и
        // начальные значения портов, но такта модели ещё не было - это и есть
        // нулевой элемент трассы.
        @(posedge clk);
        rst_n <= 1'b1;
        #1 $display("TICK %0d", led);
        repeat ({TICKS}) begin
            @(posedge clk);
            #1 $display("TICK %0d", led);
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
            "conformance_port_init.sv",
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
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter_map(|l| l.strip_prefix("TICK ")?.trim().parse::<i128>().ok())
        .collect()
}

/// Директория установки MatIEC (та же, что у `scripts/ensure-iec2c.sh`).
fn iec2c_available() -> Option<(std::path::PathBuf, std::path::PathBuf)> {
    let prefix = std::env::var("IEC2C_PREFIX")
        .map(std::path::PathBuf::from)
        .unwrap_or_else(|_| {
            std::path::PathBuf::from(std::env::var("HOME").unwrap_or_else(|_| ".".to_string()))
                .join(".local")
        });
    let bin = prefix.join("bin").join("iec2c");
    let lib = prefix.join("share").join("matiec").join("lib");
    if bin.is_file() && lib.join("C").join("iec_std_lib.h").is_file() {
        Some((bin, lib))
    } else {
        None
    }
}

/// Трасса **исполненного** ST: значение `led` после `_init__` и после каждого вызова
/// `_body__`.
///
/// Время выполненияа к ST не прилагается, поэтому путь тот же, что у `conformance_st_tests`:
/// `taktc -t st` -> `iec2c` в C -> драйвер. Наблюдение - поле экземпляра
/// `FUNCTION_BLOCK`, то есть оттуда же, откуда его берёт ПЛК.
fn st_trace(dir: &Path, iec2c: &Path, lib: &Path) -> Vec<i128> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура");
    let st_dir = dir.join("st");
    std::fs::create_dir_all(&st_dir).expect("каталог ST");
    takt_lang::compile_to_st(
        "conformance_port_init.takt",
        &source,
        st_dir.to_str().expect("путь"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение ST");

    let work = dir.join("iec2c");
    std::fs::create_dir_all(&work).expect("рабочий каталог");
    let transpile = Command::new(iec2c)
        .arg("-I")
        .arg(lib)
        .arg(st_dir.join("conformance_port_init.st"))
        .current_dir(&work)
        .output()
        .expect("запуск iec2c");
    assert!(
        transpile.status.success() && work.join("POUS.c").is_file(),
        "iec2c не оттранслировал порождённый ST:\n{}",
        String::from_utf8_lossy(&transpile.stderr)
    );

    // Структура обнуляется намеренно: `__INIT_VAR` время выполненияа MatIEC ставит флаги через
    // `|=`, и мусорный `__IEC_FORCE_FLAG` со стека заблокировал бы записи (капкан,
    // снятый спайком ).
    let harness = format!(
        r#"#include <stdio.h>
#include "iec_std_lib.h"
TIME __CURRENT_TIME;
BOOL __DEBUG = 0;
#include "POUS.h"
#include "POUS.c"

int main(void) {{
    CONFORMANCEPORTINIT_data__ fb = {{0}};
    CONFORMANCEPORTINIT_init__(&fb, __BOOL_LITERAL(FALSE));
    printf("TICK %u\n", (unsigned)fb.BLINKER0.LED.value);
    for (int i = 0; i < {TICKS}; i++) {{
        CONFORMANCEPORTINIT_body__(&fb);
        printf("TICK %u\n", (unsigned)fb.BLINKER0.LED.value);
    }}
    return 0;
}}
"#
    );
    let harness_path = work.join("harness.c");
    std::fs::write(&harness_path, harness).expect("драйвер");

    let bin = work.join("st_port_init_bin");
    let compile = Command::new("cc")
        .args(["-std=c99", "-w", "-I"])
        .arg(lib.join("C"))
        .arg("-I")
        .arg(&work)
        .arg(&harness_path)
        .arg("-o")
        .arg(&bin)
        .output()
        .expect("cc");
    assert!(
        compile.status.success(),
        "драйвер ST не собрался:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск");
    assert!(run.status.success(), "исполнение ST упало");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter_map(|l| l.strip_prefix("TICK ")?.trim().parse::<i128>().ok())
        .collect()
}

/// Эталон: порт несёт начальное значение до первого такта, затем - записи тела.
///
/// Ожидание выписано числами намеренно: тест обязан ловить и "значение не выставилось"
/// (первый элемент `0`), и "выставилось не то".
#[test]
fn simulator_starts_output_port_with_initial_value() {
    let sim = simulate_trace();
    assert_eq!(
        sim,
        vec![7, 11, 12, 13, 13, 13, 13],
        "трасса эталона: до такта — 7 из `:=`, затем записи тела, \
         после перехода в Done значение держится: {sim:?}"
    );
}

/// Цель `c` даёт ту же трассу, включая значение **до первого такта**.
#[test]
fn initial_port_value_matches_generated_c() {
    let sim = simulate_trace();
    if !cc_available() {
        eprintln!("[ПРОПУСК] initial_port_value_matches_generated_c: `cc` не найден");
        return;
    }
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join("takt_0187_05_c");
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    let c = generated_c_trace(&dir);
    assert_eq!(
        sim, c,
        "трассы эталона и цели `c` обязаны совпадать такт в такт, \
         включая значение до первого такта"
    );
}

/// Цель `sv` даёт ту же трассу: значение из ветви сброса - это и есть "до первого
/// такта" кристалла.
///
/// Сверка идёт **той же фикстурой**, что и с целью `c`: одна модель, два эталона - в
/// этом и смысл сверки (совпадение с прошивкой совпадения с кристаллом не влечёт, у
/// целей разные генераторы и разная модель исполнения).
#[test]
fn initial_port_value_matches_generated_sv() {
    let sim = simulate_trace();
    if !verilator_available() {
        eprintln!("[ПРОПУСК] initial_port_value_matches_generated_sv: verilator не найден");
        return;
    }
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join("takt_0187_05_sv");
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    let sv = sv_trace(&dir);
    assert_eq!(
        sim, sv,
        "трассы эталона и цели `sv` обязаны совпадать такт в такт, \
         включая значение после снятия сброса"
    );
}

/// Цель `st`: начальное значение стоит на порте **до первого вызова** тела, и записи
/// тела дают ту же последовательность значений.
///
/// Здесь сверяется **не** потактовое совпадение, а начальное значение и порядок
/// значений. Причина названа прямо: цель `st` расходует по такту на синтетическое
/// состояние `INIT` **каждого уровня** - трасса исполненного ST равна `[7, 7, 7, 11,
/// 12, 13, 13]` против `[7, 11, 12, 13, 13, 13, 13]` у эталона, то есть сдвинута на
/// глубину вложенности. Это нарушение контракта
/// выравнивание тактов, обнаруженное этой задачей и
/// записанное кандидатом в `FEATURES.md`; чинить его здесь нельзя - это другая фича,
/// меняющая форму вывода цели `st`.
///
/// Предмет **этой** задачи сдвигом не затронут: значение `:=` появляется до первого
/// вызова тела, и тест проверяет именно его.
#[test]
fn initial_port_value_is_set_before_first_body_call_in_st() {
    let Some((iec2c, lib)) = iec2c_available() else {
        eprintln!("[ПРОПУСК] initial_port_value_is_set_before_first_body_call_in_st: iec2c нет");
        return;
    };
    if !cc_available() {
        eprintln!("[ПРОПУСК] initial_port_value_is_set_before_first_body_call_in_st: `cc` нет");
        return;
    }
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join("takt_0187_05_st");
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    let st = st_trace(&dir, &iec2c, &lib);

    assert_eq!(
        st.first().copied(),
        Some(7),
        "значение из `:=` обязано стоять на порте до первого вызова тела: {st:?}"
    );
    // Последовательность **значений** (без повторов) совпадает с эталоном: сдвиг INIT
    // добавляет повторы, но не меняет порядка и состава.
    let squash = |v: &[i128]| {
        let mut out: Vec<i128> = Vec::new();
        for x in v {
            if out.last() != Some(x) {
                out.push(*x);
            }
        }
        out
    };
    assert_eq!(
        squash(&st),
        squash(&simulate_trace()),
        "порядок значений порта у цели `st` обязан совпадать с эталоном: {st:?}"
    );
}
