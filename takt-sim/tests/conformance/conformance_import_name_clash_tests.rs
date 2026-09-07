//! Потактовая сверка на входе, где имя модели совпадает с именем файла.
//!
//! # Что доказывает набор
//!
//! Проверки целевых языков доказывают, что вывод **собирается**, - но здесь до он не
//! собирался вовсе: три цели отвечали "Состояние ... не найдено" на корректной
//! программе. Компиляции, однако, мало: путь `...:Clash:Clash:Work` мог бы разрешиться
//! в контейнер файла, у которого своё состояние `Root`, - вывод собрался бы, а тикала
//! бы не та модель. Отличает эти два исхода только наблюдаемое: трасса порта `beat`.
//!
//! Ожидаемая трасса `1, 2, 3, 3` возможна лишь тогда, когда тикает **модель** `Clash`:
//! у контейнера тела нет, и порт остался бы нулём.

use std::path::Path;
use std::process::Command;

/// Каталог фикстур: библиотека и применение лежат рядом (импорт ищет файл рядом с
/// импортирующим).
const FIXTURE_DIR: &str = "tests/data/eval";
const APP: &str = "clash_app.takt";
/// Имя порождаемой единицы - по имени корневой модели фикстуры.
const UNIT: &str = "clash_app";
/// Сколько тактов сверяем: три шага работы плюс один после завершения.
const TICKS: usize = 4;

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

fn app_source() -> String {
    std::fs::read_to_string(Path::new(FIXTURE_DIR).join(APP)).expect("фикстура применения читается")
}

/// Трасса порта `beat` у эталона-симулятора.
fn simulator_trace() -> Vec<i128> {
    let source = app_source();
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор применения");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[FIXTURE_DIR.to_string()])
        .expect("семантика применения");
    let mut unit = takt_sim::build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    for _ in 0..TICKS {
        let _ = unit.tick();
        match unit.variable("beat") {
            Some(takt_sim::Value::Number(v)) => trace.push(v),
            other => panic!("порт 'beat' обязан быть числом, получено {other:?}"),
        }
    }
    trace
}

/// Трасса порта `beat` у порождённого C.
fn generated_c_trace(dir: &Path) -> Vec<i128> {
    let source = app_source();
    takt_lang::compile_to_c(
        UNIT,
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[FIXTURE_DIR.to_string()],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C для применения");

    let harness = format!(
        r#"#include <stdio.h>
#include "{UNIT}.h"

static long last_beat = 0;

static void wr_num(ClashApp_Out_NumericPort port, uint8_t index, int64_t v, void *ud) {{
    (void)index;
    (void)port;
    (void)ud;
    last_beat = (long)v;
}}

int main(void) {{
    ClashApp m = {{0}};
    m.write_numeric = wr_num;
    ClashApp_init(&m);
    for (int i = 0; i < {TICKS}; i++) {{
        ClashApp_tick(&m);
        printf("%ld\n", last_beat);
    }}
    return 0;
}}
"#
    );
    let harness_path = dir.join("harness_clash.c");
    std::fs::write(&harness_path, harness).expect("запись харнесса");

    let bin = dir.join("clash_bin");
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
        "порождённый C входа со столкновением имён не компилируется:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск собранного C");
    assert!(run.status.success(), "собранный C завершился с ошибкой");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .map(|l| l.trim().parse::<i128>().expect("число в строке вывода"))
        .collect()
}

/// Уникальный по тесту каталог.
fn work_dir() -> std::path::PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("main")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0469_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог теста");
    dir
}

/// **Столкновение имени файла и модели: тикает модель, и цель считает то же.**
#[test]
fn name_clash_ticks_the_model_in_simulator_and_generated_c() {
    let sim = simulator_trace();
    assert_eq!(
        sim,
        vec![1, 2, 3, 3],
        "эталон: тикать обязана модель `Clash`, а не одноимённый контейнер файла"
    );
    if !cc_available() {
        eprintln!("cc недоступен — сверка с целью `c` пропущена");
        return;
    }
    let dir = work_dir();
    let generated = generated_c_trace(&dir);
    assert_eq!(
        sim, generated,
        "эталон и порождённый C разошлись на входе со столкновением имён"
    );
}
