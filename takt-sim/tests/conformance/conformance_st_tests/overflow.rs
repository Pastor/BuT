//! Обёртка беззнакового целого - эталон == `st`.
//!
//! Вынесено из `conformance_st_tests.rs` по границе **темы**: переполнение -
//! самостоятельный предмет со своей фикстурой, и держать его в общем файле значило лишь
//! набирать строки (правило размера модуля).

use super::*;

/// Потактовая трасса `t` симулятора на фикстуре переполнения.
fn simulate_overflow_trace() -> Vec<i128> {
    let source = std::fs::read_to_string(OVERFLOW_FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение юнита");
    let mut trace = Vec::new();
    for _ in 0..6 {
        let result = unit.tick();
        assert!(
            !matches!(result, TickResult::Failed(_)),
            "симуляция: {result:?}"
        );
        trace.push(sim_value(&unit, "t"));
        if result == TickResult::Terminated {
            break;
        }
    }
    trace
}

/// Потактовая трасса `t` порождённого ST (через `iec2c` -> C).
fn run_generated_st_overflow(dir: &Path, iec2c: &Path, lib: &Path) -> Vec<i128> {
    let source = std::fs::read_to_string(OVERFLOW_FIXTURE).expect("фикстура читается");
    let st_dir = dir.join("st");
    std::fs::create_dir_all(&st_dir).expect("каталог ST");
    takt_lang::compile_to_st(
        "ovf.takt",
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
        .arg(st_dir.join("ovf.st"))
        .current_dir(&work)
        .output()
        .expect("запуск iec2c");
    assert!(
        transpile.status.success() && work.join("POUS.c").is_file(),
        "iec2c не оттранслировал ST переполнения:\n{}",
        String::from_utf8_lossy(&transpile.stderr)
    );

    let harness = r#"#include <stdio.h>
#include "iec_std_lib.h"
TIME __CURRENT_TIME;
BOOL __DEBUG = 0;
#include "POUS.h"
#include "POUS.c"

int main(void) {
    OVF_data__ fb = {0};
    OVF_init__(&fb, __BOOL_LITERAL(FALSE));
    for (int i = 0; i < 6; i++) {
        OVF_body__(&fb);
        printf("%d:t=%d\n", i, (int)fb.WRAP0.T.value);
        if (fb.IS_DONE.value) break;
    }
    return 0;
}
"#;
    let harness_path = work.join("harness.c");
    std::fs::write(&harness_path, harness).expect("запись драйвера");
    let bin = work.join("ovf_bin");
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
        "ST переполнения (через iec2c) не собирается:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск драйвера ST");
    assert!(run.status.success(), "драйвер ST завершился с ошибкой");
    let out = String::from_utf8_lossy(&run.stdout).into_owned();
    let mut trace: Vec<(usize, i128)> = out
        .lines()
        .filter_map(|line| {
            let (t, rest) = line.split_once(':')?;
            let (_, v) = rest.split_once('=')?;
            Some((t.parse().ok()?, v.trim().parse().ok()?))
        })
        .collect();
    trace.sort_by_key(|(t, _)| *t);
    trace.into_iter().map(|(_, v)| v).collect()
}

/// Обёртка `u8` совпадает у симулятора и порождённого ST.
#[test]
fn unsigned_overflow_wraps_like_generated_st() {
    let sim = simulate_overflow_trace();
    // Пиннинг правила S1: обёртка на третьем такте (255 + 1 -> 0).
    assert_eq!(
        sim,
        vec![254, 255, 0, 1, 2, 3],
        "ожидаемая трасса симулятора: 254, 255, 0 (обёртка), 1, 2, 3"
    );

    let Some((iec2c, lib)) = iec2c_available() else {
        eprintln!(
            "[ПРОПУСК] unsigned_overflow_wraps_like_generated_st: iec2c не найден \
             (трасса симулятора пришпилена выше)"
        );
        return;
    };
    if !cc_available() {
        eprintln!("[ПРОПУСК] unsigned_overflow_wraps_like_generated_st: `cc` не найден");
        return;
    }
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join("takt_conformance_0127_st_overflow");
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог сборки");
    let st = run_generated_st_overflow(&dir, &iec2c, &lib);

    // Поправка на INIT-сдвиг снята (см. Q-сверку выше): цель `st` больше не тратит скан
    // на вход в стартовое состояние, поэтому трассы сверяются напрямую, такт в такт.
    assert_eq!(
        st, sim,
        "обёртка беззнакового обязана совпадать такт в такт.\n\
         симулятор={sim:?}\nST={st:?}"
    );

    // Главное утверждение фичи: USINT в ST именно оборачивается (255 -> 0), а не
    // насыщается и не даёт отказ. До 0127 поведение цели `st` при переполнении не
    // проверялось никем.
    assert!(
        st.windows(2).any(|w| w == [255, 0]),
        "в трассе ST обязан быть переход 255 → 0 (обёртка mod 2^8): ST={st:?}"
    );
}
