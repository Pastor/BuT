//! Сверка периодического блока `every` цели `st`.
//!
//! Как у `after`, потактовое равенство с эталоном для `st` не
//! требуется: MatIEC `TON` ловит фронт на 2-м вызове, а ветвь INIT `CASE`
//! расходует скан - сдвиг структурный, к времени отношения не имеющий. Поэтому
//! профиль "часы" проверяется **свойством** (тело `every` срабатывает
//! периодически: `led` растёт многократно и монотонно), профиль "такты" -
//! **валидностью** (iec2c принимает счётчик-аккумулятор). Время выполненияа к ST нет -
//! `iec2c` транслирует в C, драйвер подаёт время в `__CURRENT_TIME` (проба П3).
//!
//! Мягкая деградация: нет `iec2c`/`cc`/заголовков MatIEC -> пропуск, не отказ.

use std::path::{Path, PathBuf};
use std::process::Command;

const FIXTURE: &str = "tests/data/eval/conformance_every.takt";
/// Корень фикстуры в C-символах iec2c (верхний регистр от `stevery`).
const ROOT: &str = "STEVERY";
/// Путь к наблюдаемому порту: под-FB `BLINKER0`, поле `LED`.
const PORT: &str = "BLINKER0.LED";
const SCANS: usize = 20;

fn iec2c_prefix() -> PathBuf {
    std::env::var_os("IEC2C_PREFIX")
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from(std::env::var_os("HOME").unwrap()).join(".local"))
}

fn iec2c_available() -> Option<(PathBuf, PathBuf)> {
    let prefix = iec2c_prefix();
    let bin = prefix.join("bin").join("iec2c");
    let lib = prefix.join("share").join("matiec").join("lib");
    (bin.is_file() && lib.join("ieclib.txt").is_file()).then_some((bin, lib))
}

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Трасса порождённого ST (профиль "часы"): драйвер ведёт `__CURRENT_TIME` 1 мс/скан,
/// печатает `led` после каждого `_body__`.
fn run_st_every_trace(dir: &Path, iec2c: &Path, lib: &Path) -> Vec<i128> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура");
    let st_dir = dir.join("st");
    std::fs::create_dir_all(&st_dir).expect("каталог ST");
    takt_lang::compile_to_st(
        "stevery.takt",
        &source,
        st_dir.to_str().unwrap(),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение ST");
    let work = dir.join("iec2c");
    std::fs::create_dir_all(&work).expect("рабочий каталог");
    let transpile = Command::new(iec2c)
        .arg("-I")
        .arg(lib)
        .arg(st_dir.join("stevery.st"))
        .current_dir(&work)
        .output()
        .expect("запуск iec2c");
    assert!(
        transpile.status.success() && work.join("POUS.c").is_file(),
        "iec2c не оттранслировал ST `every`:\n{}",
        String::from_utf8_lossy(&transpile.stderr)
    );
    let harness = format!(
        r#"#include <stdio.h>
#include "iec_std_lib.h"
TIME __CURRENT_TIME;
BOOL __DEBUG = 0;
#include "POUS.h"
#include "POUS.c"

int main(void) {{
    {ROOT}_data__ fb = {{0}};
    {ROOT}_init__(&fb, __BOOL_LITERAL(FALSE));
    for (int i = 0; i < {SCANS}; i++) {{
        __CURRENT_TIME.tv_sec = 0;
        __CURRENT_TIME.tv_nsec = (long)i * 1000000L;
        {ROOT}_body__(&fb);
        printf("TICK %u\n", (unsigned)fb.{PORT}.value);
    }}
    return 0;
}}
"#
    );
    let harness_path = work.join("harness_every.c");
    std::fs::write(&harness_path, harness).expect("драйвер");
    let bin = work.join("st_every_bin");
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
        "ST `every` (через iec2c) не собирается:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск драйвера");
    assert!(run.status.success(), "драйвер ST `every` упал");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter_map(|l| l.strip_prefix("TICK ")?.trim().parse::<i128>().ok())
        .collect()
}

/// Профиль "часы": самосбрасывающийся `TON` даёт периодическое срабатывание - `led`
/// растёт многократно и монотонно (свойство, не абсолютный такт).
#[test]
fn every_clock_profile_fires_periodically_in_generated_st() {
    let Some((iec2c, lib)) = iec2c_available() else {
        eprintln!(
            "[ПРОПУСК] every_clock_profile_fires_periodically_in_generated_st: iec2c не найден"
        );
        return;
    };
    if !cc_available() {
        eprintln!("[ПРОПУСК] every_clock_profile_fires_periodically_in_generated_st: cc не найден");
        return;
    }
    let dir = tempfile::tempdir().expect("каталог");
    let st = run_st_every_trace(dir.path(), &iec2c, &lib);
    // Монотонность: счётчик не убывает (тело только инкрементирует).
    assert!(
        st.windows(2).all(|w| w[1] >= w[0]),
        "трасса ST `every` обязана быть неубывающей: {st:?}"
    );
    // Периодичность: за 20 сканов при периоде 3 мс тело срабатывает не раз (иначе это
    // `after`, а не `every`).
    let fires = *st.last().expect("непустая трасса");
    assert!(
        fires >= 2,
        "периодический `every` обязан сработать многократно за {SCANS} сканов: {st:?}"
    );
}

/// Профиль "такты" (`--tick-hz` без `clock`): `every` даёт счётчик-аккумулятор
/// `takt_every0` (не `TON`), и iec2c его принимает. Тест валидности профиля.
#[test]
#[allow(clippy::field_reassign_with_default)]
fn every_ticks_profile_generates_valid_st() {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура");
    // Профиль "такты" требует объявленной частоты либо `--tick-hz`; фикстура `clock` не
    // объявляет, поэтому добавляем её к тексту для этого теста.
    let ticks_source = source.replace("model Blinker {", "model Blinker {\n    clock 1kHz;");
    let dir = tempfile::tempdir().expect("каталог");
    let st_dir = dir.path().join("st");
    std::fs::create_dir_all(&st_dir).expect("каталог ST");
    let mut opts = takt_lang::generator::GenerateOptions::default();
    opts.tick_hz = Some(1000);
    takt_lang::compile_to_st(
        "sttick.takt",
        &ticks_source,
        st_dir.to_str().unwrap(),
        &[],
        &opts,
    )
    .expect("порождение ST");
    let st = std::fs::read_to_string(st_dir.join("sttick.st")).expect(".st");
    assert!(
        st.contains("takt_every0"),
        "профиль «такты» — счётчик-аккумулятор `every`:\n{st}"
    );

    let Some((iec2c, lib)) = iec2c_available() else {
        eprintln!("[ПРОПУСК] every_ticks_profile_generates_valid_st: iec2c не найден");
        return;
    };
    let work = dir.path().join("iec2c");
    std::fs::create_dir_all(&work).expect("рабочий каталог");
    let transpile = Command::new(&iec2c)
        .arg("-I")
        .arg(&lib)
        .arg(st_dir.join("sttick.st"))
        .current_dir(&work)
        .output()
        .expect("запуск iec2c");
    assert!(
        transpile.status.success() && work.join("POUS.c").is_file(),
        "iec2c отверг профиль «такты» `every`:\n{}",
        String::from_utf8_lossy(&transpile.stderr)
    );
}
