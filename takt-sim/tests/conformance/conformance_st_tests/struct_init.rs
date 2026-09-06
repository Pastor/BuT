//! Агрегатный инициализатор структуры - эталон == `st`.
//!
//! # Что ловится
//!
//! Цель `st` переводила объявление структуры, тип и доступ к полю, но
//! **молча теряла** инициализатор: `g : Gains;` без `:=`. Прошивка считала с
//! нулей, эталон - с заданных значений; `iec2c` такой ST принимает, то есть
//! расхождение видно только по значениям.
//!
//! Форма выбрана **пробой**: `g : Gains := (kp := 2, ki := 3);` - именованная
//! инициализация структурного типа, которую `iec2c` принял (2026-08-19).

use super::*;

/// Исправлениетура: структура из двух полей, наблюдаемая `sum = kp + ki`.
const STRUCT_FIXTURE: &str = "tests/data/eval/conformance_struct_init.takt";

/// Наблюдаемая: `(имя в симуляторе, путь поля в структуре POUS)`.
///
/// `LOOP0` - экземпляр под-модели (имя модели + порядковый номер) в верхнем регистре:
/// идентификаторы IEC регистронезависимы.
const OBSERVED_STRUCT: &[(&str, &str)] = &[("sum", "LOOP0.SUM")];

/// Трасса `sum` у эталона.
fn simulate_struct_trace() -> Vec<i128> {
    let source = std::fs::read_to_string(STRUCT_FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    for _ in 0..MAX_TICKS {
        match unit.tick() {
            TickResult::Failed(why) => panic!("эталон остановился: {why}"),
            TickResult::Terminated => break,
            TickResult::Processing => {}
        }
        trace.push(sim_value(&unit, "sum"));
    }
    trace
}

/// Трасса `sum` у порождённого ST (через `iec2c` и `cc`).
fn run_generated_st_struct(dir: &Path, iec2c: &Path, lib: &Path) -> Vec<i128> {
    let source = std::fs::read_to_string(STRUCT_FIXTURE).expect("фикстура читается");
    let st_dir = dir.join("st");
    std::fs::create_dir_all(&st_dir).expect("каталог ST");
    takt_lang::compile_to_st(
        "structinit.takt",
        &source,
        st_dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение ST");

    let work = dir.join("iec2c");
    std::fs::create_dir_all(&work).expect("рабочий каталог iec2c");
    let transpile = Command::new(iec2c)
        .arg("-I")
        .arg(lib)
        .arg(st_dir.join("structinit.st"))
        .current_dir(&work)
        .output()
        .expect("запуск iec2c");
    assert!(
        transpile.status.success() && work.join("POUS.c").is_file(),
        "iec2c не принял ST с инициализатором структуры:\n{}",
        String::from_utf8_lossy(&transpile.stderr)
    );

    let field = OBSERVED_STRUCT[0].1;
    let harness = format!(
        r#"#include <stdio.h>
#include "iec_std_lib.h"
TIME __CURRENT_TIME;
BOOL __DEBUG = 0;
#include "POUS.h"
#include "POUS.c"

int main(void) {{
    STRUCTINIT_data__ fb = {{0}};
    STRUCTINIT_init__(&fb, __BOOL_LITERAL(FALSE));
    for (int i = 0; i < {MAX_TICKS}; i++) {{
        STRUCTINIT_body__(&fb);
        printf("%d\n", (int)fb.{field}.value);
        if (fb.IS_DONE.value) break;
    }}
    return 0;
}}
"#
    );
    let harness_path = work.join("harness_struct.c");
    std::fs::write(&harness_path, harness).expect("запись драйвера");
    let bin = work.join("structinit_bin");
    let compile = Command::new("cc")
        .args(["-std=c99", "-w", "-I"])
        .arg(lib.join("C"))
        .arg("-I")
        .arg(&work)
        .arg(&harness_path)
        .arg("-o")
        .arg(&bin)
        .output()
        .expect("запуск cc");
    assert!(
        compile.status.success(),
        "ST со структурой (через iec2c) не собирается:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск драйвера ST");
    assert!(run.status.success(), "драйвер ST завершился с ошибкой");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter_map(|l| l.trim().parse::<i128>().ok())
        .collect()
}

/// Инициализатор структуры доезжает до прошивки: трассы совпадают.
#[test]
fn struct_initializer_matches_generated_st() {
    let expected = simulate_struct_trace();
    assert!(
        expected.contains(&5),
        "контроль: эталон обязан дать sum = 5 (2 + 3), получено {expected:?}"
    );
    let Some((iec2c, lib)) = iec2c_available() else {
        eprintln!("iec2c/заголовки MatIEC недоступны — сверка ST пропущена");
        return;
    };
    if !cc_available() {
        eprintln!("cc недоступен — сверка ST пропущена");
        return;
    }
    let dir = std::env::temp_dir().join(format!("st_struct_{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("рабочий каталог");
    let actual = run_generated_st_struct(&dir, &iec2c, &lib);
    // Эталон останавливается по завершении автомата, ПЛК-драйвер делает лишний скан:
    // сравниваются такты, которые прошли оба.
    let common: Vec<i128> = actual.iter().take(expected.len()).copied().collect();
    assert_eq!(
        common, expected,
        "трасса ST разошлась с эталоном: инициализатор структуры потерян?\n\
         ST     = {actual:?}\nэталон = {expected:?}"
    );
}
