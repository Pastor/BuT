//! Сверка симулятора с RTL, порождённым `taktc -t sv`.
//!
//! # Что здесь проверяется, чего C-сверка проверить не может
//!
//! **Сдвиг = 0 достаётся конструктивно.** У цели `c` синтетическое состояние
//! `INIT` стоило по такту на уровень вложенности, и его потребовалось *убирать*
//! правкой. В RTL его нет вовсе: ветвь сброса кладёт стартовые
//! состояния **всех** уровней одним фронтом. Поэтому сдвиг равен нулю на любой
//! глубине **без единой правки** - и тесты `shift_is_zero_at_depth_*` проверяют
//! это на глубинах 1, 2 и 3.
//!
//! **`Bit` включён в область сверки** - в отличие от C-сверки, где он был
//! исключён из-за дефекта 0029 (`bit` -> `int`). В RTL `bit` -> `logic`, то есть
//! один провод: соответствие идеальное, сверять нечему мешать.
//!
//! # Как наблюдается состояние модели
//!
//! Иерархической ссылкой из тестбенча (`dut.<сигнал>`), а не через порты. Переменная
//! модели портом не является, и выводить её наружу ради теста значило бы менять
//! **продукт** ради **проверки**: у кристалла появился бы лишний вывод.
//!
//! Имя сигнала - с префиксом уровня (`conformance_ticks_counter_n`): модуль SV один на
//! корневую модель, композиция уплощается, и переменные всех уровней живут в одном
//! пространстве имён (см.
//!
//! # Мягкая деградация
//!
//! Нет Verilator -> тест **пропускается с сообщением**, а не падает (образец -
//! `cc_available()` в `conformance_c_tests.rs`). Verilator для сборки и тестов `taktc`
//! не нужен. В CI он обязателен - там проверка краснеет сам.

use std::path::{Path, PathBuf};
use std::process::Command;
use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Unit, Value, build_unit};

/// Фикстура потактовой сверки: `n` принимает 1 -> 2 -> 3 по мере переходов.
///
/// Переиспользуется из сверки с C **как есть**: одна и та же модель, сверяемая с двумя
/// эталонами, - это и есть смысл сверки. Модель эволюционирует несколько тактов,
/// поэтому сдвиг на такт (если бы он появился) сместил бы всю трассу.
const TICKS_FIXTURE: &str = "tests/data/eval/conformance_ticks.takt";

/// Фикстура переполнения беззнакового.
const OVERFLOW_FIXTURE: &str = "tests/data/eval/conformance_overflow.takt";

/// Тактов в трассе - с запасом над её длиной.
const TRACE_TICKS: usize = 6;

/// Собирает тестбенч через `verilator --binary` в каталоге `dir`.
///
/// `-j 0` обязателен: порождённый C++ verilator собирает В один поток по
/// умолчанию, и это главная статья времени сверки - замер 2026-08-17 дал
/// 110.6 с против 316 с. Помощник один на три вызова: прежде они
/// повторяли пятнадцать строк флагов дословно, и флаг пришлось бы добавлять
/// трижды.
fn verilate(dir: &std::path::Path, design: &str) -> std::process::Output {
    Command::new("verilator")
        .current_dir(dir)
        // `-j 1`, а не `-j 0`: проверка предкоммита гоняет verilator в одиночку, а тестов с
        // ним три десятка, и cargo запускает их параллельно (флак ).
        .args(["--binary", "-j", "4", "--timing", "-Wno-fatal"])
        .args(["--top-module", "tb", "tb.sv", design, "-o", "simtb"])
        .output()
        .expect("запуск verilator")
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
        Some(Value::Number(n)) => n,
        Some(Value::Boolean(b)) => i128::from(b),
        // q(m, n): наблюдаемое - представление (сигнал `logic signed [W-1:0]` = repr),
        // ровно то, что тестбенч читает через `$signed(dut.<сигнал>)`.
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

/// Потактовая трасса порождённого RTL.
///
/// Порождает `.sv` тем же `taktc`, пишет тестбенч, собирает его настоящим Verilator
/// (`--binary --timing`), запускает и разбирает печать.
///
/// `signals` - имена **сигналов SV** (с префиксом уровня), а не имена Takt.
///
/// **Тестбенч пишется здесь, а не порождается `taktc`.** Тестбенч - принадлежность
/// проверки, а не продукта: генератор не должен уметь то, что нужно только
/// тестам (решение открытого вопроса ).
fn sv_trace(
    dir: &Path,
    fixture: &str,
    basename: &str,
    signals: &[&str],
    ticks: usize,
) -> Vec<Vec<i128>> {
    let source = std::fs::read_to_string(fixture).expect("фикстура читается");
    takt_lang::compile_to_sv(
        basename,
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение SystemVerilog");

    // Печатаются значения после фронта (`#1` после `@(posedge clk)`), иначе читалось бы
    // состояние до защёлкивания - артефакт, о котором предупреждает.
    let prints = signals
        .iter()
        .map(|s| format!("dut.{}", s))
        .collect::<Vec<_>>()
        .join(", ");
    let fmt = vec!["%0d"; signals.len()].join(" ");
    let tb = format!(
        r#"module tb;
    logic clk = 0, rst_n = 0;
    logic is_done;
    {basename} dut (.clk(clk), .rst_n(rst_n), .is_done(is_done));
    always #5 clk = ~clk;
    initial begin
        // Первый фронт снимает сброс: стартовые состояния всех уровней уже в
        // регистрах, поэтому следующий фронт - такт 1 модели.
        @(posedge clk);
        rst_n <= 1'b1;
        repeat ({ticks}) begin
            @(posedge clk);
            #1 $display("TICK {fmt}", {prints});
        end
        $finish;
    end
endmodule
"#
    );
    let tb_path = dir.join("tb.sv");
    std::fs::write(&tb_path, tb).expect("запись тестбенча");

    let build = verilate(dir, &format!("{}.sv", basename));
    assert!(
        build.status.success(),
        "verilator не собрал тестбенч:\n{}",
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(dir.join("obj_dir").join("simtb"))
        .current_dir(dir)
        .output()
        .expect("запуск собранной симуляции");
    let stdout = String::from_utf8_lossy(&run.stdout);
    stdout
        .lines()
        .filter_map(|line| line.strip_prefix("TICK "))
        .map(|rest| {
            rest.split_whitespace()
                .map(|v| v.parse::<i128>().expect("значение — целое"))
                .collect()
        })
        .collect()
}

/// Каталог сборки под конкретный тест.
///
/// Прежде здесь стояло "тесты идут однопоточно" - утверждение, неверное с:
/// `--test-threads=1` снят, и тесты идут параллельно. Ключ уникальности - имя потока
/// (харнесс Rust называет поток именем теста), иначе совпадение тега с
/// `conformance_sv_time_tests` (тот же префикс) означало бы `remove_dir_all` чужого
/// каталога прямо во время сборки.
fn build_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_conformance_sv_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог сборки");
    dir
}

/// Пишет временную фикстуру и возвращает путь к ней.
fn fixture(dir: &Path, name: &str, source: &str) -> PathBuf {
    let path = dir.join(format!("{name}.takt"));
    std::fs::write(&path, source).expect("запись фикстуры");
    path
}

/// **Потактовые трассы симулятора и RTL совпадают.**
///
/// Не "установившиеся значения", а значение на **каждом такте**. C-сверка позволить
/// себе этого долго не могла: синтетический `INIT` сдвигал трассу. Здесь сдвига нет с
/// самого начала - сверять потактово можно сразу.
#[test]
fn per_tick_trace_matches_generated_sv() {
    let vars = ["n"];
    let sim = simulate_trace(TICKS_FIXTURE, &vars);
    // Пиннинг трассы симулятора: если она изменится, тест обязан упасть здесь, а не
    // "подстроиться" под RTL.
    assert_eq!(
        sim,
        held_trace(),
        "ожидаемая потактовая трасса симулятора: n = 1, 2, 3"
    );

    if !verilator_available() {
        eprintln!(
            "[ПРОПУСК] per_tick_trace_matches_generated_sv: verilator не найден — \
             потактовая сверка с RTL не выполнена (трасса симулятора пришпилена выше)"
        );
        return;
    }
    let dir = build_dir("trace");
    let sv = sv_trace(
        &dir,
        TICKS_FIXTURE,
        "conformance_ticks",
        &["conformance_ticks_counter_n"],
        sim.len(),
    );
    assert_eq!(
        sim, sv,
        "потактовые трассы симулятора и порождённого RTL обязаны совпадать НА \
         КАЖДОМ такте (R7/A10).\nсимулятор={sim:?}\nRTL={sv:?}"
    );
}

/// Обёртка `u8` совпадает у симулятора и порождённого RTL.
///
/// В аппаратуре обёртка получается сама - регистр `logic [7:0]` шире не бывает. Сверка
/// нужна как **пиннинг правила**: она фиксирует, что эталон-симулятор именно
/// оборачивает (правило S1), а не диагностирует и не насыщает. Разойдись эталон с
/// аппаратной арифметикой - расхождение поймается здесь.
#[test]
fn unsigned_overflow_wraps_like_generated_sv() {
    let vars = ["t"];
    let sim = simulate_trace(OVERFLOW_FIXTURE, &vars);
    assert_eq!(
        sim,
        vec![vec![254], vec![255], vec![0], vec![1], vec![2], vec![3]],
        "ожидаемая трасса симулятора: 254, 255, 0 (обёртка), 1, 2, 3"
    );

    if !verilator_available() {
        eprintln!(
            "[ПРОПУСК] unsigned_overflow_wraps_like_generated_sv: verilator не найден \
             (трасса симулятора пришпилена выше)"
        );
        return;
    }
    let dir = build_dir("ovf");
    let sv = sv_trace(
        &dir,
        OVERFLOW_FIXTURE,
        "conformance_overflow",
        &["conformance_overflow_wrap_t"],
        sim.len(),
    );
    assert_eq!(
        sim, sv,
        "обёртка беззнакового обязана совпадать.\nсимулятор={sim:?}\nSV={sv:?}"
    );
}

/// Модель глубины 1 (без обёртки): тело стартового состояния - на такте 1.
#[test]
fn shift_is_zero_at_depth_1() {
    check_shift_is_zero(
        "depth1",
        "var n: u8 := 0; \
         start S0 { always { n := 1; } ref S1; } \
         state S1 { always { n := 2; } }",
        &["depth1_n"],
    );
}

/// Глубина 2 (`start E = M;`): лишний уровень трассу не сдвигает.
#[test]
fn shift_is_zero_at_depth_2() {
    check_shift_is_zero(
        "depth2",
        "model M { var n: u8 := 0; start S0 { always { n := 1; } ref S1; } \
         state S1 { always { n := 2; } } } \
         start E = M;",
        &["depth2_m_n"],
    );
}

/// **Глубина 3 - где у цели `c` сдвиг был максимальным.**
///
/// В C уровни входили последовательно, поэтому тело исполнялось на такте, равном
/// глубине. В RTL все `always_ff` сбрасываются **одним фронтом**, и глубина не стоит
/// ничего.
#[test]
fn shift_is_zero_at_depth_3() {
    check_shift_is_zero(
        "depth3",
        "model Inner { var n: u8 := 0; start S0 { always { n := 1; } ref S1; } \
         state S1 { always { n := 2; } } } \
         model Mid { start M = Inner; } \
         start E = Mid;",
        // Имя снято зондом, а не выведено из пути: уникальное имя модели не повторяет
        // промежуточный уровень - сигнал `depth3_inner_n`, а не `depth3_mid_inner_n`.
        &["depth3_inner_n"],
    );
}

/// Общая проверка "сдвиг = 0": на такте 1 переменная уже равна 1.
///
/// Сверяется не только первый такт, но и вся трасса - со стороной симулятора.
fn check_shift_is_zero(tag: &str, source: &str, signals: &[&str]) {
    let dir = build_dir(tag);
    let path = fixture(&dir, tag, source);
    let sim = simulate_trace(path.to_str().expect("путь в UTF-8"), &["n"]);
    assert_eq!(
        sim.first().map(|t| t[0]),
        Some(1),
        "симулятор: тело стартового состояния обязано исполниться на такте 1 \
         (контракт ADR 0033), трасса={sim:?}"
    );

    if !verilator_available() {
        eprintln!("[ПРОПУСК] {tag}: verilator не найден — сверка с RTL не выполнена");
        return;
    }
    let sv = sv_trace(
        &dir,
        path.to_str().expect("путь в UTF-8"),
        tag,
        signals,
        sim.len(),
    );
    assert_eq!(
        sv.first().map(|t| t[0]),
        Some(1),
        "RTL: тело стартового состояния обязано исполниться на ТАКТЕ 1 после \
         снятия rst_n — сдвиг = 0 на любой глубине.\nRTL={sv:?}"
    );
    assert_eq!(
        sim, sv,
        "потактовые трассы обязаны совпадать.\nсимулятор={sim:?}\nRTL={sv:?}"
    );
}

/// **`Bit` входит в область сверки.**
///
/// В C-сверке он был исключён: генератор давал `bit` -> `int` (дефект 0029), и
/// сверяться с дефектным эталоном значило бы узаконить дефект. В RTL `bit` -> `logic` -
/// один провод, соответствие идеальное.
#[test]
fn bit_is_within_conformance_scope() {
    let dir = build_dir("bit");
    let source = "var f: bit := 0; \
                  start S0 { always { f := 1; } ref S1; } \
                  state S1 { always { f := 0; } }";
    let path = fixture(&dir, "bitconf", source);
    let sim = simulate_trace(path.to_str().expect("путь в UTF-8"), &["f"]);
    assert_eq!(
        sim,
        vec![vec![1], vec![0], vec![0], vec![0], vec![0], vec![0]],
        "симулятор: f принимает 1, затем 0"
    );

    if !verilator_available() {
        eprintln!("[ПРОПУСК] bit_is_within_conformance_scope: verilator не найден");
        return;
    }
    let sv = sv_trace(
        &dir,
        path.to_str().expect("путь в UTF-8"),
        "bitconf",
        &["bitconf_f"],
        sim.len(),
    );
    assert_eq!(
        sim, sv,
        "тип `bit` обязан сверяться потактово: в RTL он `logic`, то есть один \
         провод, и дефекта 0029 (`bit` → `int`) здесь нет.\nсимулятор={sim:?}\nRTL={sv:?}"
    );
}

// -- Сверка `+` - Тройная: sim == C == SV (доведена) ------------
//
// называет эталоном поведения `+` **обе** реализации - C (`generate_concat_tick`) и
// симулятор (`Unit::Sequential`). Совпадать они начали не сразу: симулятор **не
// исполнял композицию, несущую переход `next`** (состояние-реализация с `next` уходило
// по нему на такте 1, не тикнув шаги), и потому сверка временно велась только с
// порождённым C.
//
// Дефект заведён исправлением `docs/fixes/0057-01-sim-composition-next.md` (Tier 2) и закрыт
// (реализация состояния - дочерний юнит с общим контекстом; переход берётся по её
// завершении). Теперь обе сверки ниже - тройные: эталон C пришпилен литералом, а SV и
// симулятор обязаны совпасть с ним и между собой. Так расхождение любой из трёх сторон
// становится видно немедленно, а не подменяется молчаливым согласием двух.

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Потактовая трасса порождённого C для корневых переменных (`m.<var>`).
///
/// Собственный минимальный аналог `conformance_c_tests::c_trace` - тот адресует
/// **вложенную** модель (`m.<accessor>.<var>`), а здесь наблюдаемые лежат в
/// корне (shared-переменные, записанные под-моделями композиции).
fn c_root_trace(
    dir: &Path,
    fixture: &str,
    basename: &str,
    root_type: &str,
    vars: &[&str],
) -> Vec<Vec<i128>> {
    let source = std::fs::read_to_string(fixture).expect("фикстура читается");
    takt_lang::compile_to_c(
        basename,
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");
    let prints = vars
        .iter()
        .map(|v| format!(r#"        printf("%d:{v}=%d\n", t, (int)m.{v});"#))
        .collect::<Vec<_>>()
        .join("\n");
    let harness = format!(
        r#"#include <stdio.h>
#include "{basename}.h"
int main(void) {{
    {root_type} m;
    {root_type}_init(&m);
    for (int t = 0; t < {TRACE_TICKS}; t++) {{
        {root_type}_tick(&m);
{prints}
        if ({root_type}_is_done(&m)) break;
    }}
    return 0;
}}
"#
    );
    let harness_path = dir.join("c_root_harness.c");
    std::fs::write(&harness_path, harness).expect("запись харнесса");
    let bin = dir.join("c_root_bin");
    let compile = Command::new("cc")
        .args(["-std=c11", "-I"])
        .arg(dir)
        .arg(dir.join(format!("{basename}.c")))
        .arg(&harness_path)
        .arg("-o")
        .arg(&bin)
        .output()
        .expect("запуск cc");
    assert!(
        compile.status.success(),
        "порождённый C не компилируется:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск собранного C");
    assert!(run.status.success(), "собранный C завершился с ошибкой");
    let out = String::from_utf8_lossy(&run.stdout).into_owned();
    let mut trace: Vec<Vec<i128>> = Vec::new();
    for line in out.lines() {
        let (t_str, rest) = line.split_once(':').expect("формат T:var=val");
        let t: usize = t_str.parse().expect("индекс такта");
        let (_, val) = rest.split_once('=').expect("формат var=val");
        let val: i128 = val.trim().parse().expect("значение");
        if trace.len() <= t {
            trace.resize(t + 1, Vec::new());
        }
        trace[t].push(val);
    }
    trace
}

/// **0057: последовательная композиция `A + B` - sim == C == SV.**
///
/// Каждый шаг под-модели исполняется до завершения, затем шаг-регистр
/// продвигается, и следующий шаг тикается на такте **после** (эталон - `break`
/// в C `generate_concat_tick`). Наблюдаемая - корневая `stage`, которую пишет
/// текущий шаг (shared-переменная). Завершение последнего шага переводит
/// **родительское** состояние в `Done`  - видно по `is_done`.
#[test]
fn sequential_composition_matches_generated_c() {
    let dir = build_dir("concat");
    let source = "var stage: u8 := 0; \
                  model A { start S1 { always { stage := 1; } ref S2: 1 = 1; } state S2; } \
                  model B { start S1 { always { stage := 2; } ref S2: 1 = 1; } state S2; } \
                  start P = A + B { next Done; } state Done;";
    let path = fixture(&dir, "seqc", source);

    // Симулятор - первая сторона тройной сверки. Инструментов не требует, поэтому
    // проверяется до мягких пропусков `cc`/`verilator`: на машине без них сверка
    // сузится, но не исчезнет.
    let sim = simulate_trace(path.to_str().expect("путь в UTF-8"), &["stage"]);
    assert_eq!(
        sim,
        vec![vec![1], vec![1], vec![2], vec![2], vec![2]],
        "симулятор обязан исполнять композицию, объявленную реализацией \
         состояния с переходом `next`: шаг A держит stage=1 два такта, шаг B — \
         stage=2, затем родитель уходит в Done (фикс 0057-01, фича 0181).\n\
         симулятор={sim:?}"
    );

    if !cc_available() {
        eprintln!("[ПРОПУСК] sequential_composition_matches_generated_c: `cc` не найден");
        return;
    }
    let c = c_root_trace(
        &dir,
        path.to_str().expect("путь в UTF-8"),
        "seqc",
        "Seqc",
        &["stage"],
    );
    // Пиннинг C-эталона: `A` пишет 1 (такты 1-2), `B` - 2 (такты 3-4), затем родитель
    // уходит в `Done`. Если обе стороны сломать одинаково - упадёт тут.
    assert_eq!(
        c,
        vec![vec![1], vec![1], vec![2], vec![2], vec![2]],
        "эталон C: шаг A держит stage=1 два такта, шаг B — stage=2, затем Done"
    );
    assert_eq!(
        sim, c,
        "потактовые трассы `+` (симулятор и C) обязаны совпадать — ADR 0057 \
         объявляет эталоном обе реализации.\nсимулятор={sim:?}\nC={c:?}"
    );

    if !verilator_available() {
        eprintln!("[ПРОПУСК] sequential_composition_matches_generated_c: verilator не найден");
        return;
    }
    let sv = sv_trace(
        &dir,
        path.to_str().expect("путь в UTF-8"),
        "seqc",
        &["seqc_stage"],
        c.len(),
    );
    assert_eq!(
        c, sv,
        "потактовые трассы `+` (SV и C) обязаны совпадать: шаг активируется на \
         такте ПОСЛЕ завершения предыдущего (регистр шага разрывает такт, как \
         `break` в C).\nC={c:?}\nRTL={sv:?}"
    );
}

/// **0057: параллельный шаг `(B | C)` внутри цепочки `+` - sim == C == SV.**
///
/// Средний шаг - параллельная композиция: цепочка переходит к следующему шагу, когда
/// завершены **обе** ветви (конъюнкция done). `s1` пишет шаг A, `s2` - обе ветви
/// параллельного шага (идемпотентно, порядок ветвей не важен).
#[test]
fn parallel_step_inside_concatenation_matches_generated_c() {
    let dir = build_dir("concat_par");
    let source = "var s1: u8 := 0; var s2: u8 := 0; \
                  model A { start S1 { always { s1 := 1; } ref S2: 1 = 1; } state S2; } \
                  model B { start S1 { always { s2 := 1; } ref S2: 1 = 1; } state S2; } \
                  model C { start S1 { always { s2 := 1; } ref S2: 1 = 1; } state S2; } \
                  start P = A + (B | C) { next Done; } state Done;";
    let path = fixture(&dir, "parc", source);

    // Симулятор - первая сторона тройной сверки, без инструментов.
    let sim = simulate_trace(path.to_str().expect("путь в UTF-8"), &["s1", "s2"]);
    assert_eq!(
        sim,
        vec![vec![1, 0], vec![1, 0], vec![1, 1], vec![1, 1], vec![1, 1]],
        "симулятор: шаг A даёт s1=1 (такты 1–2), затем параллельный шаг даёт \
         s2=1 по завершении обеих ветвей.\nсимулятор={sim:?}"
    );

    if !cc_available() {
        eprintln!(
            "[ПРОПУСК] parallel_step_inside_concatenation_matches_generated_c: `cc` не найден"
        );
        return;
    }
    let c = c_root_trace(
        &dir,
        path.to_str().expect("путь в UTF-8"),
        "parc",
        "Parc",
        &["s1", "s2"],
    );
    assert_eq!(
        c,
        vec![vec![1, 0], vec![1, 0], vec![1, 1], vec![1, 1], vec![1, 1]],
        "эталон C: A даёт s1=1 (такты 1–2), затем параллельный шаг даёт s2=1 по \
         завершении обеих ветвей"
    );
    assert_eq!(
        sim, c,
        "потактовые трассы `(|)` внутри `+` (симулятор и C) обязаны \
         совпадать.\nсимулятор={sim:?}\nC={c:?}"
    );

    if !verilator_available() {
        eprintln!(
            "[ПРОПУСК] parallel_step_inside_concatenation_matches_generated_c: verilator не найден"
        );
        return;
    }
    let sv = sv_trace(
        &dir,
        path.to_str().expect("путь в UTF-8"),
        "parc",
        &["parc_s1", "parc_s2"],
        c.len(),
    );
    assert_eq!(
        c, sv,
        "потактовые трассы `(|)` внутри `+` (SV и C) обязаны совпадать: \
         параллельный шаг завершается по конъюнкции done обеих ветвей.\nC={c:?}\nRTL={sv:?}"
    );
}

// -----------------------------------------------------------------------------
// Q-арифметика fixed-point: T8/T9/T10 для цели sv
//
// Сигнал q - `logic signed [W-1:0]` = repr; наблюдается `$signed(dut.<сигнал>)`
// (знаковая печать) и сверяется потактово с симулятором. Проверки verilator+yosys
// доказывают синтезируемость, сверка - верность (T10, включая floor T9).

/// Как [`sv_trace`], но наблюдаемые сигналы печатаются **знаково**
/// (`$signed(dut.<сигнал>)`): представление q бывает отрицательным, а `%0d` над `logic`
/// печатает без знака.
fn sv_trace_signed(
    dir: &Path,
    fixture: &str,
    basename: &str,
    signals: &[&str],
    ticks: usize,
    opts: &takt_lang::generator::GenerateOptions,
) -> Vec<Vec<i128>> {
    let source = std::fs::read_to_string(fixture).expect("фикстура читается");
    takt_lang::compile_to_sv(
        basename,
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        opts,
    )
    .expect("порождение SystemVerilog");

    let prints = signals
        .iter()
        .map(|s| format!("$signed(dut.{})", s))
        .collect::<Vec<_>>()
        .join(", ");
    let fmt = vec!["%0d"; signals.len()].join(" ");
    let tb = format!(
        r#"module tb;
    logic clk = 0, rst_n = 0;
    logic is_done;
    {basename} dut (.clk(clk), .rst_n(rst_n), .is_done(is_done));
    always #5 clk = ~clk;
    initial begin
        @(posedge clk);
        rst_n <= 1'b1;
        repeat ({ticks}) begin
            @(posedge clk);
            #1 $display("TICK {fmt}", {prints});
        end
        $finish;
    end
endmodule
"#
    );
    std::fs::write(dir.join("tb.sv"), tb).expect("запись тестбенча");
    let build = verilate(dir, &format!("{}.sv", basename));
    assert!(
        build.status.success(),
        "verilator не собрал Q-тестбенч:\n{}",
        String::from_utf8_lossy(&build.stderr)
    );
    let run = Command::new(dir.join("obj_dir").join("simtb"))
        .current_dir(dir)
        .output()
        .expect("запуск симуляции");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter_map(|line| line.strip_prefix("TICK "))
        .map(|rest| {
            rest.split_whitespace()
                .map(|v| v.parse::<i128>().expect("значение — целое"))
                .collect()
        })
        .collect()
}

/// Побитовая потактовая сверка Q-арифметики с
/// симулятором - включая отрицательные и floor к −∞ у `*` (S2: repr −2, а не −1). Проверки
/// verilator/yosys прогоняются в `precheck.sh`.
#[test]
fn fixed_point_arithmetic_matches_generated_sv() {
    let vars = ["acc"];
    let sim = simulate_trace("tests/data/eval/conformance_fixed.takt", &vars);
    assert_eq!(
        sim,
        q_trace(),
        "трасса представлений q(8,8) — эталон Q-арифметики симулятора"
    );

    if !verilator_available() {
        eprintln!("[ПРОПУСК] fixed_point_arithmetic_matches_generated_sv: verilator не найден");
        return;
    }
    let dir = build_dir("fixed");
    let sv = sv_trace_signed(
        &dir,
        "tests/data/eval/conformance_fixed.takt",
        "conformance_fixed",
        &["conformance_fixed_fixed_acc"],
        sim.len(),
        &takt_lang::generator::GenerateOptions::default(),
    );
    assert_eq!(
        sim, sv,
        "Q-арифметика симулятора и порождённого RTL обязана совпасть ПОБИТОВО на \
         каждом такте (repr q(8,8)).\nсимулятор={sim:?}\nRTL={sv:?}"
    );
}

// -----------------------------------------------------------------------------
// Прозрачный float -> q(m, n): T4/T7/T9 для цели sv
//
// Под флагом `--float-as-q=8.8` цель sv понижает `float` в q(8,8) - SV-003 снимается,
// вещественная модель синтезируется. Трасса `acc` обязана совпасть с q-версией
// (`conformance_fixed`) побитово: float->q(8,8) - это и есть q(8,8).

/// Опции генерации с глобальной Q-точностью `q(m, n)` для `float`.
#[allow(clippy::field_reassign_with_default)] // GenerateOptions - #[non_exhaustive]
fn float_as_q_opts(m: u8, n: u8) -> takt_lang::generator::GenerateOptions {
    let mut o = takt_lang::generator::GenerateOptions::default();
    o.float_as_q = Some((m, n));
    o
}

/// Q-режим эталона: понижает `float -> q(m, n)` в модели симулятора тем же проходом,
/// что и цель.
fn simulate_trace_float_q(fixture: &str, m: u8, n: u8, vars: &[&str]) -> Vec<Vec<i128>> {
    let source = std::fs::read_to_string(fixture).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    takt_lang::semantic::lower_float::lower_float_to_fixed(model.clone(), m, n)
        .expect("float → q(m, n)");
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

/// Под `--float-as-q=8.8` `float` -> q(8,8),
/// `SV-003` снят, RTL синтезируется (verilator) и **побитово** совпадает с Q-эталоном
/// симулятора на каждом такте. Ожидаемая трасса - та же, что у явной q-версии
/// `conformance_fixed` (float->q(8,8) == q(8,8)).
#[test]
fn float_as_q_matches_generated_sv() {
    let sim = simulate_trace_float_q("tests/data/eval/conformance_float_q.takt", 8, 8, &["acc"]);
    assert_eq!(
        sim,
        q_trace(),
        "Q-эталон float→q(8,8) обязан совпасть с трассой явной q-версии (0061)"
    );

    if !verilator_available() {
        eprintln!("[ПРОПУСК] float_as_q_matches_generated_sv: verilator не найден");
        return;
    }
    let dir = build_dir("float_q");
    let sv = sv_trace_signed(
        &dir,
        "tests/data/eval/conformance_float_q.takt",
        "conformance_float_q",
        &["conformance_float_q_float_fixed_acc"],
        sim.len(),
        &float_as_q_opts(8, 8),
    );
    assert_eq!(
        sim, sv,
        "float→q(8,8) в симуляторе и порождённом RTL обязаны совпасть ПОБИТОВО на \
         каждом такте (repr q(8,8)).\nсимулятор={sim:?}\nRTL={sv:?}"
    );
}

/// Без флага `float` в цели sv остаётся `SV-003`. Это и
/// есть "выбор формата автором": флаг - единственное, что снимает запрет. Мутация
/// "убрать флаг у цели" обязана вернуть ошибку, а не молча собрать несопоставимый
/// (native) вывод.
#[test]
fn float_without_flag_is_sv003() {
    let source = std::fs::read_to_string("tests/data/eval/conformance_float_q.takt")
        .expect("фикстура читается");
    let dir = build_dir("float_no_flag");
    let err = takt_lang::compile_to_sv(
        "conformance_float_q",
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect_err("float без --float-as-q обязан дать SV-003");
    assert_eq!(
        err.code.as_deref(),
        Some("SV-003"),
        "без флага float в sv — SV-003 (флаг = выбор формата автором)"
    );
}

/// **0063: `en` ограничивает шаг, но не сброс - доказано симуляцией.**
///
/// Проверка (verilator lint + yosys) доказывает лишь валидность/синтезируемость; что
/// неподключённый `en` **значит `1`** и что `en=0` **замораживает** - доказывает только
/// прогон. Один тестбенч на четыре экземпляра одной модели `conformance_ticks` (счётчик
/// `n`: 1 -> 2 -> 3, сброс `n = 0`):
///
/// - `dut_unc` - `en` **не подключён** (умолчание `1'b1`);
/// - `dut_en1` - `en = 1'b1` явно;
/// - `dut_en0` - `en = 1'b0` (заморожен);
/// - `dut_rst` - `en = 1` три такта (n -> 3), затем `en = 0` **и** `rst_n = 0`
///   одним фронтом (A4: сброс обязан сработать при `en = 0`).
///
/// Утверждения: `unc == en1` **побитово** на каждом такте (A2, умолчание = `1`); `en0 =
/// 0` всё время (A2, заморозка); `rst` всё время (A2, заморозка); `1 2 en` -
/// ).
#[test]
fn clock_enable_gates_step_but_not_reset() {
    if !verilator_available() {
        eprintln!(
            "[ПРОПУСК] clock_enable_gates_step_but_not_reset: verilator не найден — \
             семантика en не проверена (форма выхода закреплена юнит-тестами генератора)"
        );
        return;
    }
    let dir = build_dir("clock_enable");
    let source = std::fs::read_to_string(TICKS_FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_sv(
        "conformance_ticks",
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение SystemVerilog");

    // `en` у `dut_unc` не подключён намеренно - это и есть проверка умолчания (сверка
    // `sv_trace` тоже его не подключает). `dut_rst.en` - регистр `en_rst`.
    let tb = r#"module tb;
    logic clk = 0, rst_n = 0, en_rst;
    logic d0, d1, d2, d3;
    conformance_ticks dut_unc (.clk(clk), .rst_n(rst_n),               .is_done(d0));
    conformance_ticks dut_en1 (.clk(clk), .rst_n(rst_n), .en(1'b1),    .is_done(d1));
    conformance_ticks dut_en0 (.clk(clk), .rst_n(rst_n), .en(1'b0),    .is_done(d2));
    conformance_ticks dut_rst (.clk(clk), .rst_n(rst_n), .en(en_rst),  .is_done(d3));
    always #5 clk = ~clk;
    initial begin
        en_rst = 1'b1;
        @(posedge clk);
        rst_n <= 1'b1;
        repeat (3) begin
            @(posedge clk);
            #1 $display("STEP %0d %0d %0d %0d",
                dut_unc.conformance_ticks_counter_n,
                dut_en1.conformance_ticks_counter_n,
                dut_en0.conformance_ticks_counter_n,
                dut_rst.conformance_ticks_counter_n);
        end
        // Сброс при en=0 - глушим en_rst и одновременно опускаем rst_n.
        en_rst <= 1'b0;
        rst_n  <= 1'b0;
        @(posedge clk);
        #1 $display("RST %0d", dut_rst.conformance_ticks_counter_n);
        $finish;
    end
endmodule
"#;
    std::fs::write(dir.join("tb.sv"), tb).expect("запись тестбенча");
    let build = verilate(&dir, "conformance_ticks.sv");
    assert!(
        build.status.success(),
        "verilator не собрал тестбенч en:\n{}",
        String::from_utf8_lossy(&build.stderr)
    );
    let run = Command::new(dir.join("obj_dir").join("simtb"))
        .current_dir(&dir)
        .output()
        .expect("запуск собранной симуляции");
    let stdout = String::from_utf8_lossy(&run.stdout);

    let steps: Vec<Vec<i128>> = stdout
        .lines()
        .filter_map(|l| l.strip_prefix("STEP "))
        .map(|r| r.split_whitespace().map(|v| v.parse().unwrap()).collect())
        .collect();
    let rst: i64 = stdout
        .lines()
        .find_map(|l| l.strip_prefix("RST "))
        .expect("строка RST")
        .trim()
        .parse()
        .expect("значение RST — целое");

    assert_eq!(steps.len(), 3, "ожидались 3 такта STEP:\n{stdout}");
    for (i, s) in steps.iter().enumerate() {
        let (unc, en1, en0, _) = (s[0], s[1], s[2], s[3]);
        // Неподключённый en побитово тождествен en=1.
        assert_eq!(
            unc, en1,
            "такт {i}: неподключённый en ≠ en=1 (умолчание сломано)"
        );
        // Неподключённый = ожидаемая эволюция счётчика (1,2,3).
        assert_eq!(unc, (i as i128) + 1, "такт {i}: en=1 не даёт n = {}", i + 1);
        // En=0 замораживает автомат на сбросовом значении.
        assert_eq!(en0, 0, "такт {i}: en=0 не заморозил счётчик (n = {en0})");
    }
    // Сброс сработал при en=0 - n вернулся к сбросовому 0.
    assert_eq!(
        rst, 0,
        "сброс при en=0 не сработал: n = {rst} (правило 3 ADR 0063)"
    );
}

// Перенос q к ширине формата - подмодулем: файл упирается в лимит размера модуля, а
// правило требует делить по логике.
#[path = "conformance_sv_tests/fixed_width.rs"]
mod fixed_width;

#[path = "conformance_sv_tests/traces.rs"] // пиннинги трасс - тем же приёмом
mod traces;
use traces::{held_trace, q_trace};

// Насыщение q(m, n) sat - тем же приёмом подмодуля.
#[path = "conformance_sv_tests/fixed_sat.rs"]
mod fixed_sat;

/// Наблюдатель-сосед видит состояние соседа.
mod state_of;

/// Вычислимое приведение в инициализаторе.
mod const_cast_init;

/// Структуры в цели `sv`.
mod struct_init;
