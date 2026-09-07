//! Потактовая сверка анонимного обращения к ячейке.
//!
//! # Что доказывается
//!
//! Эталон (симулятор) и цель `c-hal` дают **одинаковую трассу** на модели, которая
//! читает и пишет ячейку по адресу. Сверяется значение, а не факт компиляции: проверка цели
//! доказывает, что вывод собирается, но не что он верен - на этом стояли дефекты 0045 и
//! 0050.
//!
//! # Как наблюдается ячейка
//!
//! У эталона ячейка - синтетический порт (решение 5B ), её значение видно как значение
//! с именем `AT_<адрес>_<бит>_<ширина>`. У цели `c-hal` доступ идёт прямым
//! разыменованием `*(volatile uintN_t*)АДРЕС`, поэтому харнесс **подставляет настоящую
//! память** по этому адресу: `mmap(MAP_FIXED)` на выбранный адрес - тот же приём, что у
//! теста битового диапазона (`takt-lang/tests/hal_bit_range_tests.rs`).
//!
//! Адрес - `0x1000_0000_0000` (16 ТиБ), тот же, что у теста 0098: macOS отвергает
//! низкие адреса (проба: `mmap` на 256 МиБ не даёт требуемый адрес), а страничное
//! выравнивание требуется самим `mmap`.

use std::path::Path;
use std::process::Command;

/// Адрес ячейки - он же в фикстуре.
const CELL_ADDR: u64 = 0x1000_0000_0000;
const TICKS: usize = 6;

const FIXTURE: &str = concat!(
    "model Probe {\n",
    "    var seen: u8 := 0;\n",
    "    var n: u8 := 0;\n",
    "    start Run {\n",
    "        always {\n",
    "            #0x100000000000 as u8 := n + 1;\n",
    "            seen := #0x100000000000 as u8;\n",
    "            n := n + 1;\n",
    "        }\n",
    "        ref Run: n < 100;\n",
    "    }\n",
    "}\n",
    "start Main = Probe;\n"
);

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Трасса эталона: значение ячейки после каждого такта.
fn simulate_trace() -> Vec<i128> {
    let (ast, _) = takt_lang::parse(FIXTURE, 0).expect("разбор");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = takt_sim::build_unit(model).expect("построение Unit");

    let mut trace = Vec::new();
    for _ in 0..TICKS {
        let r = unit.tick();
        assert!(
            !matches!(r, takt_sim::TickResult::Failed(_)),
            "падение эталона: {r:?}"
        );
        trace.push(cell_of(&unit));
    }
    trace
}

/// Значение ячейки в эталоне - по имени, которое строит компилятор.
fn cell_of(unit: &takt_sim::Unit) -> i128 {
    let name = format!("AT_{CELL_ADDR:X}_0_8");
    match unit.variable(&name) {
        Some(takt_sim::Value::Number(n)) => n,
        other => panic!("ячейка {name}: {other:?}"),
    }
}

/// Трасса порождённого `c-hal`: настоящая память по адресу ячейки.
fn generated_c_hal_trace(dir: &Path) -> Vec<i128> {
    let env = takt_lang::parse_defines(&[]).expect("среда");
    takt_lang::compile_to_c_hal(
        "conformance_anon",
        FIXTURE,
        dir.to_str().expect("путь"),
        &[],
        &[],
        &env,
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение c-hal");

    let harness = format!(
        r#"#include <stdio.h>
#include <sys/mman.h>
#include "conformance_anon.h"

/* Ячейка живёт в настоящей памяти по своему адресу: порождённый код
   разыменовывает `*(volatile uint8_t*)0x{CELL_ADDR:X}`, и подменять это
   чтение нечем — иначе сверялась бы не та трансляция, что уедет в прошивку. */
int main(void) {{
    void *page = mmap((void *)0x{CELL_ADDR:X}, 4096, PROT_READ | PROT_WRITE,
                      MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED, -1, 0);
    if (page != (void *)0x{CELL_ADDR:X}) {{
        fprintf(stderr, "mmap не дал требуемый адрес\n");
        return 1;
    }}
    ConformanceAnon m = {{0}};
    ConformanceAnon_init(&m);
    for (int tick = 1; tick <= {TICKS}; tick++) {{
        ConformanceAnon_tick(&m);
        printf("TICK %u\n", (unsigned)(*(volatile uint8_t *)0x{CELL_ADDR:X}));
    }}
    return 0;
}}
"#
    );
    let harness_path = dir.join("harness_anon.c");
    std::fs::write(&harness_path, harness).expect("харнесс");

    let bin = dir.join("conformance_anon_bin");
    let compile = Command::new("cc")
        .args(["-std=c11", "-Wall", "-Werror", "-I"])
        .arg(dir)
        .arg(dir.join("conformance_anon.c"))
        .arg(&harness_path)
        .arg("-o")
        .arg(&bin)
        .output()
        .expect("cc");
    assert!(
        compile.status.success(),
        "порождённый c-hal не компилируется:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск");
    assert!(
        run.status.success(),
        "собранный c-hal упал:\n{}",
        String::from_utf8_lossy(&run.stderr)
    );
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter_map(|l| l.strip_prefix("TICK ")?.trim().parse::<i128>().ok())
        .collect()
}

/// Эталон и цель `c-hal` дают одну трассу ячейки.
#[test]
fn anon_cell_trace_matches_c_hal() {
    let expected = simulate_trace();
    assert_eq!(
        expected,
        vec![1, 2, 3, 4, 5, 6],
        "эталон должен писать в ячейку n + 1 каждый такт"
    );

    if !cc_available() {
        eprintln!("cc недоступен — сверка с c-hal пропущена");
        return;
    }
    // `:` из имени потока вычищается: после слияния тестовых целей имя теста несёт
    // префикс модуля (`модуль::тест`), и двоеточие попало бы в путь каталога.
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!(
            "takt_0189_conformance_{}",
            std::thread::current()
                .name()
                .unwrap_or("single")
                .replace(':', "_")
        ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");

    let actual = generated_c_hal_trace(&dir);
    assert_eq!(
        expected, actual,
        "трасса ячейки эталона и цели c-hal обязана совпадать такт в такт"
    );
}
