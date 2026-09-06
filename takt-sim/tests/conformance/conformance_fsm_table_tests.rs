//! Потактовая сверка табличной формы автомата.
//!
//! # Что доказывает набор
//!
//! Флаг `--fsm=table` меняет **форму** порождённого C, а не поведение: эталон, прошивка
//! формы `switch` и прошивка формы `table` обязаны давать **одну** трассу такт в такт.
//!
//! Проверка цели этого не видит по устройству: таблица, переставившая две
//! строки или потерявшая блок `exit`, собирается тем же `cc -Wall -Wextra
//! -Werror` без единого замечания - вывод валиден, автомат другой. Ровно
//! поэтому сверка заведена вместе с формой (правило "сверку заводить вместе с
//! генератором", уроки 0045 и 0050).
//!
//! # Исправлениетуры
//!
//! - `conformance_fsm_table.takt` - условное ребро, блоки `enter`/`exit`,
//!   **накапливающее** тело (на идемпотентном пропуск и двойное исполнение
//!   неразличимы), самопереход;
//! - `conformance_fsm_table_parallel.takt` - параллельная композиция: страж
//!   строки есть конъюнкция готовностей ветвей, и берётся она у того же
//!   носителя, что печатает их тик;
//! - `conformance_fsm_table_chain.takt` - последовательная композиция (фича
//!   0438): машина шагов остаётся в теле такта, а наружу состояние уходит при
//!   условии "цепочка на последнем шаге, и он завершён". Фаза каждого шага
//!   наблюдаема своим портом, поэтому пропуск шага, лишний такт и ранний выход
//!   дают разные трассы.

use std::path::{Path, PathBuf};
use std::process::Command;

use takt_lang::generator::{FsmForm, GenerateOptions};
use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Value, build_unit};

const SIMPLE_FIXTURE: &str = "tests/data/eval/conformance_fsm_table.takt";
const SIMPLE_UNIT: &str = "conformance_fsm_table";
const SIMPLE_TICKS: usize = 8;

const PARALLEL_FIXTURE: &str = "tests/data/eval/conformance_fsm_table_parallel.takt";
const PARALLEL_UNIT: &str = "conformance_fsm_table_parallel";
const PARALLEL_TICKS: usize = 6;

const CHAIN_FIXTURE: &str = "tests/data/eval/conformance_fsm_table_chain.takt";
const CHAIN_UNIT: &str = "conformance_fsm_table_chain";
const CHAIN_TICKS: usize = 9;
/// Порты исправлениетуры цепочки: фаза каждого шага наблюдается своим портом.
const CHAIN_PORTS: [&str; 3] = ["first_probe", "second_probe", "line_probe"];

fn tool(bin: &str) -> bool {
    Command::new(bin)
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
        .join(format!("takt_0435_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог сборки");
    dir
}

fn source(fixture: &str) -> String {
    std::fs::read_to_string(fixture).expect("фикстура читается")
}

/// Трасса эталона: значение порта `probe` по тактам.
fn simulator_trace(fixture: &str, ticks: usize) -> Vec<i128> {
    let (ast, _) = takt_lang::parse(&source(fixture), 0).expect("разбор фикстуры");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    let mut probe = 0i128;
    for _ in 0..ticks {
        let result = unit.tick();
        assert!(
            !matches!(result, TickResult::Failed(_)),
            "эталон не обрывает прогон: {result:?}"
        );
        if let Some(Value::Number(v)) = unit.variable("probe") {
            probe = v;
        }
        trace.push(probe);
    }
    trace
}

/// Трасса прошивки заданной формы: тот же порт, те же такты.
fn c_trace(dir: &Path, fixture: &str, unit: &str, ticks: usize, fsm: FsmForm) -> Vec<i128> {
    let mut options = GenerateOptions::default();
    options.fsm = fsm;
    takt_lang::compile_to_c(
        unit,
        &source(fixture),
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &options,
    )
    .expect("порождение C");
    let camel = camel(unit);
    let harness = format!(
        r#"#include <stdio.h>
#include "{unit}.h"
static long long probe;
static void on_num({camel}_Out_NumericPort port, uint8_t index, int64_t value, void *userdata) {{
    (void)index;
    (void)userdata;
    if (port == {upper}_PORT_PROBE) {{ probe = (long long)value; }}
}}
int main(void) {{
    {camel} m;
    {camel}_init(&m);
    m.write_numeric = on_num;
    m.userdata = 0;
    for (int i = 0; i < {ticks}; i++) {{
        {camel}_tick(&m);
        printf("%lld\n", probe);
    }}
    return 0;
}}
"#,
        upper = probe_port_owner(unit),
    );
    std::fs::write(dir.join("harness.c"), harness).expect("харнесс");
    let bin = dir.join("bin");
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
        .arg(dir.join(format!("{unit}.c")))
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
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter_map(|l| l.trim().parse::<i128>().ok())
        .collect()
}

/// Имя структуры корня: `conformance_fsm_table` -> `ConformanceFsmTable`.
fn camel(unit: &str) -> String {
    unit.split('_')
        .map(|part| {
            let mut chars = part.chars();
            match chars.next() {
                Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
                None => String::new(),
            }
        })
        .collect()
}

/// Владелец перечислителя порта `probe` в порождённом C.
///
/// Имя перечислителя квалифицировано моделью-владельцем, и у двух исправлениетур владельцы
/// разные: `Worker` и `Pair`.
fn probe_port_owner(unit: &str) -> String {
    let owner = if unit == PARALLEL_UNIT {
        "PAIR"
    } else {
        "WORKER"
    };
    format!("{}_{owner}", unit.to_uppercase())
}

/// Обе формы и эталон дают одну трассу: условное ребро, `enter`/`exit`, накопление,
/// самопереход.
#[test]
fn table_form_matches_switch_and_simulator() {
    if !tool("cc") {
        eprintln!("cc недоступен — сверка пропущена");
        return;
    }
    let expected = simulator_trace(SIMPLE_FIXTURE, SIMPLE_TICKS);
    let switch_dir = build_dir("simple_switch");
    let table_dir = build_dir("simple_table");
    let switch = c_trace(
        &switch_dir,
        SIMPLE_FIXTURE,
        SIMPLE_UNIT,
        SIMPLE_TICKS,
        FsmForm::Switch,
    );
    let table = c_trace(
        &table_dir,
        SIMPLE_FIXTURE,
        SIMPLE_UNIT,
        SIMPLE_TICKS,
        FsmForm::Table,
    );
    assert_eq!(switch, expected, "форма switch разошлась с эталоном");
    assert_eq!(table, expected, "форма table разошлась с эталоном");
    // Контроль осмысленности трассы: в ней есть и вход (`enter` даёт 100), и выход
    // (`exit` даёт 200). На постоянной трассе сверка ничего не значит.
    assert!(
        expected.contains(&100) && expected.contains(&200),
        "трасса не наблюдает блоки enter/exit: {expected:?}"
    );
}

/// Параллельная композиция: страж строки - конъюнкция готовностей ветвей.
#[test]
fn table_form_matches_switch_on_parallel_composition() {
    if !tool("cc") {
        eprintln!("cc недоступен — сверка пропущена");
        return;
    }
    let expected = simulator_trace(PARALLEL_FIXTURE, PARALLEL_TICKS);
    let switch_dir = build_dir("parallel_switch");
    let table_dir = build_dir("parallel_table");
    let switch = c_trace(
        &switch_dir,
        PARALLEL_FIXTURE,
        PARALLEL_UNIT,
        PARALLEL_TICKS,
        FsmForm::Switch,
    );
    let table = c_trace(
        &table_dir,
        PARALLEL_FIXTURE,
        PARALLEL_UNIT,
        PARALLEL_TICKS,
        FsmForm::Table,
    );
    assert_eq!(switch, expected, "форма switch разошлась с эталоном");
    assert_eq!(table, expected, "форма table разошлась с эталоном");
    // Контроль: трасса меняется по тактам - иначе перестановка строк таблицы была бы
    // незаметна.
    assert!(
        expected.first() != expected.last(),
        "трасса постоянна и сверкой ничего не доказывает: {expected:?}"
    );
}

/// Трасса эталона на цепочке: тройка портов по тактам.
fn chain_simulator_trace() -> Vec<[i128; 3]> {
    let (ast, _) = takt_lang::parse(&source(CHAIN_FIXTURE), 0).expect("разбор фикстуры");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    let mut reg = [0i128; 3];
    for _ in 0..CHAIN_TICKS {
        let result = unit.tick();
        assert!(
            !matches!(result, TickResult::Failed(_)),
            "эталон не обрывает прогон: {result:?}"
        );
        for (slot, port) in reg.iter_mut().zip(CHAIN_PORTS) {
            if let Some(Value::Number(v)) = unit.variable(port) {
                *slot = v;
            }
        }
        trace.push(reg);
    }
    trace
}

/// Трасса прошивки на цепочке: те же три порта, те же такты.
fn chain_c_trace(dir: &Path, fsm: FsmForm) -> Vec<[i128; 3]> {
    let mut options = GenerateOptions::default();
    options.fsm = fsm;
    takt_lang::compile_to_c(
        CHAIN_UNIT,
        &source(CHAIN_FIXTURE),
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &options,
    )
    .expect("порождение C");
    let camel = camel(CHAIN_UNIT);
    let upper = CHAIN_UNIT.to_uppercase();
    let harness = format!(
        r#"#include <stdio.h>
#include "{CHAIN_UNIT}.h"
static long long first, second, line;
static void on_num({camel}_Out_NumericPort port, uint8_t index, int64_t value, void *userdata) {{
    (void)index;
    (void)userdata;
    if (port == {upper}_FIRST_PORT_FIRST_PROBE) {{ first = (long long)value; }}
    else if (port == {upper}_SECOND_PORT_SECOND_PROBE) {{ second = (long long)value; }}
    else {{ line = (long long)value; }}
}}
int main(void) {{
    {camel} m;
    {camel}_init(&m);
    m.write_numeric = on_num;
    m.userdata = 0;
    for (int i = 0; i < {CHAIN_TICKS}; i++) {{
        {camel}_tick(&m);
        printf("%lld %lld %lld\n", first, second, line);
    }}
    return 0;
}}
"#
    );
    std::fs::write(dir.join("harness.c"), harness).expect("харнесс");
    let bin = dir.join("bin");
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
        .arg(dir.join(format!("{CHAIN_UNIT}.c")))
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
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter_map(|line| {
            let mut it = line.split_whitespace();
            Some([
                it.next()?.parse().ok()?,
                it.next()?.parse().ok()?,
                it.next()?.parse().ok()?,
            ])
        })
        .collect()
}

/// Последовательная композиция: машина шагов в теле, выход - строкой таблицы.
#[test]
fn table_form_matches_switch_on_chain_composition() {
    if !tool("cc") {
        eprintln!("cc недоступен — сверка пропущена");
        return;
    }
    let expected = chain_simulator_trace();
    let switch = chain_c_trace(&build_dir("chain_switch"), FsmForm::Switch);
    let table = chain_c_trace(&build_dir("chain_table"), FsmForm::Table);
    assert_eq!(switch, expected, "форма switch разошлась с эталоном");
    assert_eq!(table, expected, "форма table разошлась с эталоном");
    // Контроль: в трассе видно оба шага и жизнь после цепочки. Иначе ранний выход из
    // состояния-цепочки (страж без проверки последнего шага) прошёл бы незамеченным.
    assert!(
        expected.iter().any(|r| r[0] == 12) && expected.iter().any(|r| r[1] == 22),
        "трасса не наблюдает оба шага цепочки: {expected:?}"
    );
    assert!(
        expected.iter().any(|r| r[2] == 91),
        "трасса не доходит до состояния после цепочки: {expected:?}"
    );
    // Контроль второго прохода: автомат возвращается в цепочку, и на этом такте счётчик
    // состояния-хвоста не растёт - значит, повторный вход наблюдаем.
    assert!(
        expected.windows(2).any(|pair| pair[0] == pair[1]),
        "трасса не наблюдает повторный вход в цепочку: {expected:?}"
    );
}
