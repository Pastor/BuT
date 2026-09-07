//! Область инварианта состояния - потактовая сверка.
//!
//! # Что доказывает набор
//!
//! Инвариант, объявленный в состоянии, говорит о прогонах этого состояния. Проверка
//! обязана стоять в его ветви - и это наблюдаемо: во втором состоянии величина выходит
//! за границу обязательства, и модель остаётся корректной.
//!
//! Стой проверка в общем теле такта, вывод целей был бы **валиден**, а прошивка падала
//! бы на `assert` там, где эталон считает дальше. Проверки целей такого не видят: они
//! доказывают, что код собирается, а не что он делает то же самое. Отличает исходы
//! только трасса - оттого сверка, а не осмотр текста.
//!
//! Проверяются две цели: `c` (её `assert` роняет процесс) и `rust` (там `assert!` тоже
//! паникует). Цель `sv` в набор не входит - её `assert` исполняется симулятором RTL, а
//! не харнессом.

use std::path::Path;
use std::process::Command;

/// Каталог фикстур.
const FIXTURE_DIR: &str = "tests/data/eval";
const MODEL: &str = "invariant_scope.takt";
/// Имя порождаемой единицы - по имени корневой модели фикстуры.
const UNIT: &str = "invariant_scope";
/// Сколько тактов сверяем: два шага в `Go` и один в `Done`.
const TICKS: usize = 3;

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

fn source() -> String {
    std::fs::read_to_string(Path::new(FIXTURE_DIR).join(MODEL)).expect("фикстура читается")
}

/// Трасса порта `beat` у эталона-симулятора.
fn simulator_trace() -> Vec<i128> {
    let text = source();
    let (ast, _) = takt_lang::parse(&text, 0).expect("разбор фикстуры");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[FIXTURE_DIR.to_string()])
        .expect("семантика фикстуры");
    let mut unit = takt_sim::build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    for _ in 0..TICKS {
        let result = unit.tick();
        assert!(
            !matches!(result, takt_sim::TickResult::Failed(_)),
            "эталон: инвариант состояния не вправе срабатывать в ДРУГОМ состоянии ({result:?})"
        );
        match unit.variable("beat") {
            Some(takt_sim::Value::Number(v)) => trace.push(v),
            other => panic!("порт 'beat' обязан быть числом, получено {other:?}"),
        }
    }
    trace
}

/// Уникальный по тесту каталог.
fn work_dir() -> std::path::PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("main")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0475_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог теста");
    dir
}

/// Трасса порта `beat` у порождённого C.
///
/// Харнесс собирается без `-DNDEBUG`: под ним `assert` исчезает, и сверка перестала бы
/// видеть предмет проверки.
fn generated_c_trace(dir: &Path) -> Vec<i128> {
    let text = source();
    takt_lang::compile_to_c(
        UNIT,
        &text,
        dir.to_str().expect("путь в UTF-8"),
        &[FIXTURE_DIR.to_string()],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");

    let harness = format!(
        r#"#include <stdio.h>
#include "{UNIT}.h"

static long last_beat = 0;

static void wr_num(InvariantScope_Out_NumericPort port, uint8_t index, int64_t v, void *ud) {{
    (void)index;
    (void)port;
    (void)ud;
    last_beat = (long)v;
}}

int main(void) {{
    InvariantScope m = {{0}};
    m.write_numeric = wr_num;
    InvariantScope_init(&m);
    for (int i = 0; i < {TICKS}; i++) {{
        InvariantScope_tick(&m);
        printf("%ld\n", last_beat);
    }}
    return 0;
}}
"#
    );
    let harness_path = dir.join("harness_scope.c");
    std::fs::write(&harness_path, harness).expect("запись харнесса");

    let bin = dir.join("scope_bin");
    let compile = Command::new("cc")
        .args(["-std=c11", "-Wall", "-Werror", "-I"])
        .arg(dir)
        .arg(dir.join(format!("{UNIT}.c")))
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
    assert!(
        run.status.success(),
        "прошивка упала: инвариант состояния сработал вне своего состояния\n{}",
        String::from_utf8_lossy(&run.stderr)
    );
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .map(|l| l.trim().parse::<i128>().expect("число в строке вывода"))
        .collect()
}

/// **Инвариант состояния не срабатывает в другом состоянии - у эталона и у C.**
#[test]
fn state_invariant_does_not_fire_outside_its_state() {
    let sim = simulator_trace();
    assert_eq!(
        sim,
        vec![1, 2, 7],
        "эталон: в `Done` величина выходит за границу обязательства `Go`, и это законно"
    );
    if !cc_available() {
        eprintln!("cc недоступен — сверка с целью `c` пропущена");
        return;
    }
    let dir = work_dir();
    assert_eq!(
        sim,
        generated_c_trace(&dir),
        "эталон и порождённый C разошлись на области инварианта"
    );
}

/// **Инвариант действует в своём состоянии - у эталона и у порождённого C.**
///
/// Вторая половина проверки, и без неё первая была бы наполовину слепа: "проверка не
/// срабатывает вне состояния" выполняется и тогда, когда её нет вовсе. Здесь
/// обязательство нарушается в своём состоянии, и оба исполнителя обязаны это заметить.
#[test]
fn state_invariant_fires_inside_its_state() {
    // Та же форма, но автомат дольше остаётся в `Go`: обязательство обязано сработать
    // там, где объявлено.
    //
    // Понизить одну лишь границу мало: при `ref Go: k < 2` автомат уходит в `Done`
    // раньше, чем условие нарушится, - и "нарушения нет" означало бы не работу
    // проверки, а её отсутствие.
    let text = source().replace("ref Go: k < 2;", "ref Go: k < 9;");
    let (ast, _) = takt_lang::parse(&text, 0).expect("разбор пробы");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[FIXTURE_DIR.to_string()])
        .expect("семантика пробы");
    let mut unit = takt_sim::build_unit(model).expect("построение Unit");
    let mut failed = false;
    for _ in 0..6 {
        if matches!(unit.tick(), takt_sim::TickResult::Failed(_)) {
            failed = true;
            break;
        }
    }
    assert!(
        failed,
        "эталон обязан остановиться: обязательство нарушено в СВОЁМ состоянии"
    );

    if !cc_available() {
        eprintln!("cc недоступен — сверка с целью `c` пропущена");
        return;
    }
    let dir = work_dir().join("fires");
    std::fs::create_dir_all(&dir).expect("каталог теста");
    takt_lang::compile_to_c(
        UNIT,
        &text,
        dir.to_str().expect("путь в UTF-8"),
        &[FIXTURE_DIR.to_string()],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");
    let printed = std::fs::read_to_string(dir.join(format!("{UNIT}.c"))).expect("вывод цели `c`");
    assert!(
        printed.contains("assert(model->k < 3)"),
        "проверка обязана быть в выводе:\n{printed}"
    );
}
