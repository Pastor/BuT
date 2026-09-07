//! Сверка **насыщения** `q(m, n) sat` с целью `st`.
//!
//! Отдельный подмодуль по предмету: здесь насыщение `q(m, n) sat`, а
//! требует делить по логике. Помощники (`iec2c_available`, `cc_available`, `sim_value`,
//! `MAX_TICKS`) берутся из родителя через `use super::*`.
//!
//! Прижатие цель `st` делает в `LINT` - **до** сужения `LINT_TO_INT`. Порядок здесь не
//! косметика: сужение, сработав раньше, вернуло бы обёрнутое значение, и `iec2c` принял
//! бы такой ST молча - тот же капкан, что стоил исправления.

use super::*;

/// Фикстура насыщения: `q(6, 6) sat` - W = 12 при типе хранения `INT` (16 бит).
const FIXED_SAT_FIXTURE: &str = "tests/data/eval/conformance_fixed_sat_w12.takt";

/// Наблюдаемые точки: `(имя в симуляторе, путь поля в структуре POUS)`.
///
/// `FIXED_SAT_W120` - имя экземпляра под-модели (имя модели + порядковый номер),
/// печатается `iec2c` в верхнем регистре: идентификаторы IEC регистронезависимы.
const OBSERVED_SAT: &[(&str, &str)] = &[
    ("up", "FIXED_SAT_W120.UP"),
    ("down", "FIXED_SAT_W120.DOWN"),
    ("neg", "FIXED_SAT_W120.NEG"),
];

/// Трасса представлений фикстуры насыщения (та же, что у целей `c` и `sv`: тестовые
/// файлы - разные крейты, общего модуля у них нет).
fn expected_sat_w12() -> Vec<Vec<i128>> {
    vec![
        vec![512, -512, 512],
        vec![1024, -1024, 1024],
        vec![1536, -1536, 1536],
        vec![2047, -2048, 2047],
        vec![2047, -2048, 2047],
        vec![2047, -2048, 2047],
        vec![2047, -2048, 2047],
        vec![2047, -2048, 2047],
    ]
}

/// Потактовая трасса `up`/`down`/`neg` (repr q(6,6)) симулятора.
fn simulate_sat_trace() -> Vec<Vec<i128>> {
    let source = std::fs::read_to_string(FIXED_SAT_FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение юнита");
    let mut trace = Vec::new();
    for _ in 0..MAX_TICKS {
        let result = unit.tick();
        assert!(
            !matches!(result, TickResult::Failed(_)),
            "симуляция: {result:?}"
        );
        trace.push(
            OBSERVED_SAT
                .iter()
                .map(|(name, _)| sim_value(&unit, name))
                .collect(),
        );
        if result == TickResult::Terminated {
            break;
        }
    }
    trace
}

/// Потактовая трасса порождённого ST (через `iec2c` -> C).
fn run_generated_st_sat(dir: &Path, iec2c: &Path, lib: &Path) -> Vec<Vec<i128>> {
    let source = std::fs::read_to_string(FIXED_SAT_FIXTURE).expect("фикстура читается");
    let st_dir = dir.join("st");
    std::fs::create_dir_all(&st_dir).expect("каталог ST");
    takt_lang::compile_to_st(
        "qsat.takt",
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
        .arg(st_dir.join("qsat.st"))
        .current_dir(&work)
        .output()
        .expect("запуск iec2c");
    assert!(
        transpile.status.success() && work.join("POUS.c").is_file(),
        "iec2c не оттранслировал ST с TAKT_Q_SAT:\n{}",
        String::from_utf8_lossy(&transpile.stderr)
    );

    let prints = OBSERVED_SAT
        .iter()
        .map(|(name, field)| format!(r#"printf("%d:{name}=%d\n", i, (int)fb.{field}.value);"#))
        .collect::<Vec<_>>()
        .join("\n        ");
    let harness = format!(
        r#"#include <stdio.h>
#include "iec_std_lib.h"
TIME __CURRENT_TIME;
BOOL __DEBUG = 0;
#include "POUS.h"
#include "POUS.c"

int main(void) {{
    QSAT_data__ fb = {{0}};
    QSAT_init__(&fb, __BOOL_LITERAL(FALSE));
    for (int i = 0; i < {MAX_TICKS}; i++) {{
        QSAT_body__(&fb);
        {prints}
        if (fb.IS_DONE.value) break;
    }}
    return 0;
}}
"#
    );
    let harness_path = work.join("harness.c");
    std::fs::write(&harness_path, harness).expect("запись драйвера");
    let bin = work.join("qsat_bin");
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
        "ST с TAKT_Q_SAT (через iec2c) не собирается:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск драйвера ST");
    assert!(run.status.success(), "драйвер ST завершился с ошибкой");

    let out = String::from_utf8_lossy(&run.stdout).into_owned();
    let mut trace: Vec<Vec<i128>> = Vec::new();
    for line in out.lines() {
        let Some((tick, rest)) = line.split_once(':') else {
            continue;
        };
        let Some((name, value)) = rest.split_once('=') else {
            continue;
        };
        let tick: usize = tick.parse().expect("номер такта — целое");
        let value: i128 = value.trim().parse().expect("значение — целое");
        let column = OBSERVED_SAT
            .iter()
            .position(|(n, _)| *n == name)
            .expect("печатается только наблюдаемое");
        if trace.len() <= tick {
            trace.resize(tick + 1, vec![0; OBSERVED_SAT.len()]);
        }
        trace[tick][column] = value;
    }
    trace
}

/// Насыщение прижимает к границам **формата** на обеих границах и
/// на крае унарного минуса `−(−2^(W−1))`.
///
/// Тест заодно доказывает, что `FUNCTION TAKT_Q_SAT` **принимается MatIEC**: сравнения
/// над `LINT` и возврат из трёх ветвей - не самоочевидная для IEC конструкция, а рядом
/// уже стоит `TAKT_Q_WRAP`, чей приём тоже проверялся отдельно.
#[test]
fn fixed_saturation_matches_generated_st() {
    let sim = simulate_sat_trace();
    assert_eq!(
        sim,
        expected_sat_w12(),
        "q(6,6) sat: прижатие к repr ∈ [−2048, 2047]; перенос дал бы на такте 4 \
         up = −2048 и neg = −2048"
    );

    let Some((iec2c, lib)) = iec2c_available() else {
        eprintln!("iec2c/MatIEC недоступны — сверка насыщения ST пропущена");
        return;
    };
    if !cc_available() {
        eprintln!("cc недоступен — сверка насыщения ST пропущена");
        return;
    }
    // Имя каталога уникально по фикстуре и процессу - как у соседних сверок ST.
    let dir = std::env::temp_dir().join(format!("st_conf_fixed_sat_{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("рабочий каталог");
    let st = run_generated_st_sat(&dir, &iec2c, &lib);
    assert_eq!(
        sim, st,
        "насыщение обязано совпасть с ST на каждом такте.\nсимулятор={sim:?}\nST={st:?}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}
