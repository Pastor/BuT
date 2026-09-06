//! Потактовая сверка **значений** на импортирующем входе.
//!
//! Проверки целевых языков доказывают, что порождённый код **собирается**, но не что он
//! считает то же самое. Для усыновления импортированного поддерева это особенно важно:
//! ошибка привязки даёт не отказ, а **разные копии** одной переменной - обе модели
//! пишут каждая в своё, код компилируется, а контур молча разомкнут.
//!
//! Наблюдаемое - выходной порт `lvl`, куда применение выкладывает общую переменную.
//! Ожидаемая трасса `1, 3, 7, 15, 31` возможна только при **одной** переменной на обе
//! модели: при копиях `meas` не рос бы.

use std::path::Path;
use std::process::Command;

/// Каталог исправлениетур (библиотека + применение лежат рядом: импорт ищет файл рядом с
/// импортирующим).
const FIXTURE_DIR: &str = "tests/data/eval";
const APP: &str = "import_pid_app.takt";
/// Имя порождаемой единицы: совпадает с именем корневой модели исправлениетуры.
const UNIT: &str = "import_pid_app";
/// Сколько тактов сверяем.
const TICKS: usize = 5;

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

/// Трасса порта `lvl` у эталона-симулятора.
fn simulator_trace() -> Vec<i128> {
    let source = app_source();
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор применения");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[FIXTURE_DIR.to_string()])
        .expect("семантика применения");
    let mut unit = takt_sim::build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    for _ in 0..TICKS {
        let _ = unit.tick();
        match unit.variable("lvl") {
            Some(takt_sim::Value::Number(v)) => trace.push(v),
            other => panic!("порт 'lvl' обязан быть числом, получено {other:?}"),
        }
    }
    trace
}

/// Трасса порта `lvl` у порождённого C.
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

static long last_lvl = 0;

static void wr_num(ImportPidApp_Out_NumericPort port, uint8_t index, int64_t v, void *ud) {{
    (void)index;
    (void)port;
    (void)ud;
    last_lvl = (long)v;
}}

int main(void) {{
    ImportPidApp m = {{0}};
    m.write_numeric = wr_num;
    ImportPidApp_init(&m);
    for (int i = 0; i < {TICKS}; i++) {{
        ImportPidApp_tick(&m);
        printf("%ld\n", last_lvl);
    }}
    return 0;
}}
"#
    );
    let harness_path = dir.join("harness_import.c");
    std::fs::write(&harness_path, harness).expect("запись харнесса");

    let bin = dir.join("import_bin");
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
        "порождённый C импортирующего входа не компилируется:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск собранного C");
    assert!(run.status.success(), "собранный C завершился с ошибкой");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .map(|l| l.trim().parse::<i128>().expect("число в строке вывода"))
        .collect()
}

/// **A3: импортированная переменная - Одна на всё дерево.**
///
/// Эталон и порождённый C дают одну трассу, и она - та, что возможна лишь при общей
/// переменной. Если привязка импортированного объявления потеряется вновь, упадёт либо
/// компиляция (как было до фичи), либо это равенство.
#[test]
fn imported_shared_variable_matches_simulator_and_generated_c() {
    let sim = simulator_trace();
    assert_eq!(
        sim,
        vec![1, 3, 7, 15, 31],
        "эталон: обе модели пишут в ОДНУ переменную (при копиях трасса была бы иной)"
    );

    if !cc_available() {
        eprintln!(
            "[ПРОПУСК] imported_shared_variable_matches_simulator_and_generated_c: `cc` не найден"
        );
        return;
    }
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join("takt_0184_conformance");
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание каталога сборки");
    let c = generated_c_trace(&dir);
    assert_eq!(
        sim, c,
        "потактовые трассы эталона и порождённого C обязаны совпадать:\nsim={sim:?}\nC={c:?}"
    );
}
