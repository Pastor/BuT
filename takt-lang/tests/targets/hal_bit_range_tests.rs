//! /: диапазон бита адреса порта (`SE-060`) и безопасный умолчательный HAL цели `c-hal`.
//!
//! Проверяется **на значениях**, а не грепом (критерий 2 исправления ): прежний HAL читал
//! один байт и сдвигал на `b.bit`, поэтому бит 8...31 давал молча ноль, а бит >= 32 -
//! undefined behavior (`int >> 33`). Оба проверки (`cc`) принимали это молча - `b.bit`
//! значение времени выполнения, предупредить компилятор не мог.
//!
//! **Значенческий тест эмулирует MMIO через `mmap(MAP_FIXED)`:** разыменовать
//! абсолютный адрес порта на хосте нельзя, поэтому C-драйвер отображает страницу по
//! адресу порта и исполняет `read_bit`. При недоступности `cc`/`mmap` - явный пропуск
//! (как `cc_available`), а не молчаливый.

use std::collections::HashMap;
use std::path::Path;
use std::process::Command;
use takt_lang::AddressEnv;
use takt_lang::generator::GenerateOptions;

/// Адрес порта в тестовой модели. Выровнен по странице и заведомо свободен и на macOS,
/// и на Linux (16 тб) - `mmap(MAP_FIXED)` на низкие адреса macOS отвергает.
const PROBE_ADDR: &str = "0x100000000000";

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Компилирует исходник в `c-hal` (адреса inline, без внешней карты и define).
fn compile_hal(name: &str, source: &str, dir: &Path) -> Result<(), String> {
    takt_lang::compile_to_c_hal(
        name,
        source,
        dir.to_str().unwrap(),
        &[],
        &[],
        &AddressEnv::new(HashMap::new()),
        &GenerateOptions::default(),
    )
    .map(|_warnings| ())
    .map_err(|d| format!("[{}] {}", d.code.as_deref().unwrap_or("?"), d.message))
}

fn bit_model(bit: &str) -> String {
    format!(
        "in P: bit at 0x1000:{bit};\nvar s: bit := 0;\nstart S {{\n    always {{ s := P; }}\n}}\n"
    )
}

/// Бит 63 валиден, бит 64 -> `SE-060` (вне [0, 63], `uint64_t` - предел).
#[test]
fn bit_in_range_ok_out_of_range_is_se060() {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join("takt_0098_se060");
    std::fs::create_dir_all(&dir).expect("каталог");

    // Верхняя граница диапазона - валидна (читается uint64_t).
    let ok = compile_hal("p63", &bit_model("63"), &dir);
    assert!(ok.is_ok(), "бит 63 обязан быть валиден, получено: {ok:?}");

    // За границей - ошибка, а не тихое чтение нуля / UB.
    let err = compile_hal("p64", &bit_model("64"), &dir).expect_err("бит 64 вне диапазона");
    assert!(err.contains("SE-060"), "ожидалась SE-060, получено: {err}");
    assert!(err.contains("[0, 63]"), "текст называет диапазон: {err}");
}

/// Умолчательный HAL читает бит 33 словом `uint64_t`, а не байтом.
/// Прежний код (`uint8_t >> 33`) дал бы UB - исполнение вернуло бы не "1001". Бит 2
/// остаётся байтовым (младший байт), их независимость и проверяется.
#[test]
fn hal_reads_wide_bit_without_ub() {
    if !cc_available() {
        eprintln!("[ПРОПУСК] hal_reads_wide_bit_without_ub: компилятор `cc` не найден");
        return;
    }
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join("takt_0098_hal_value");
    std::fs::create_dir_all(&dir).expect("каталог");

    // LO - бит 2 (младший байт -> доступ 1 байт), HI - бит 33 (-> доступ 8 байт).
    let source = format!(
        "in LO: bit at {PROBE_ADDR}:2;\nin HI: bit at {PROBE_ADDR}:33;\n\
         var seen: bit := 0;\nstart S {{\n    always {{ seen := LO; }}\n}}\n"
    );
    compile_hal("bitprobe", &source, &dir).expect("c-hal генерируется");

    // Драйвер отображает страницу по адресу порта (эмуляция MMIO) и исполняет
    // сгенерированный `read_bit`. `MAP_ANON`/`MAP_ANONYMOUS` - псевдонимы на разных
    // платформах. При неудаче mmap - печатает SKIP (тест не падает).
    let driver = format!(
        r#"#include "bitprobe.h"
#include <sys/mman.h>
#include <stdio.h>
#include <stdint.h>
#ifndef MAP_ANONYMOUS
#define MAP_ANONYMOUS MAP_ANON
#endif
int main(void) {{
    uintptr_t A = (uintptr_t){PROBE_ADDR}ULL;
    void *p = mmap((void*)A, 4096, PROT_READ|PROT_WRITE,
                   MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED, -1, 0);
    if (p == MAP_FAILED) {{ printf("SKIP\n"); return 0; }}
    volatile uint64_t *w = (volatile uint64_t*)A;
    Bitprobe m; Bitprobe_init(&m); Bitprobe_bind_default_hal(&m);
    *w = ((uint64_t)1 << 33);
    int a = m.read_bit(BITPROBE_PORT_HI, 0, m.userdata) ? 1 : 0;
    int b = m.read_bit(BITPROBE_PORT_LO, 0, m.userdata) ? 1 : 0;
    *w = ((uint64_t)1 << 2);
    int c = m.read_bit(BITPROBE_PORT_HI, 0, m.userdata) ? 1 : 0;
    int d = m.read_bit(BITPROBE_PORT_LO, 0, m.userdata) ? 1 : 0;
    printf("%d%d%d%d\n", a, b, c, d);
    return 0;
}}
"#
    );
    let driver_path = dir.join("driver.c");
    std::fs::write(&driver_path, driver).expect("запись драйвера");

    let bin = dir.join("bitprobe_bin");
    let compile = Command::new("cc")
        .args(["-std=c11", "-I"])
        .arg(&dir)
        .arg(dir.join("bitprobe.c"))
        .arg(&driver_path)
        .arg("-o")
        .arg(&bin)
        .output()
        .expect("запуск cc");
    assert!(
        compile.status.success(),
        "c-hal + драйвер не компилируется:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let run = Command::new(&bin).output().expect("запуск драйвера");
    assert!(run.status.success(), "драйвер завершился с ошибкой");
    let out = String::from_utf8_lossy(&run.stdout);
    let out = out.trim();
    if out == "SKIP" {
        eprintln!(
            "[ПРОПУСК] hal_reads_wide_bit_without_ub: mmap(MAP_FIXED) недоступен на этой платформе"
        );
        return;
    }
    // слово=1<<33: HI(бит33,uint64)=1, LO(бит2,uint8)=0; слово=1<<2: HI=0, LO=1. ->
    // "1001". Прежний UB (`uint8_t >> 33`) этого равенства не даёт.
    assert_eq!(
        out, "1001",
        "чтение бита через слово неверно (UB?): {out:?}"
    );
}
