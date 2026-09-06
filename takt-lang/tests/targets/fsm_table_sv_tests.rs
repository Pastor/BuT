//! Табличная форма автомата у цели `sv`.
//!
//! # Что доказывает набор
//!
//! 1. **Строки - данные**: `localparam`-векторы `..._TRANS_FROM`/`..._TRANS_TO`, а
//!    просмотр - `for` со статической границей (он разворачивается при
//!    элаборации, давая ту же приоритетную цепочку, что форма `unique case`).
//! 2. **Переходы уходят из ветвей** `unique case`.
//! 3. **Умолчание не изменилось** - контроль.
//! 4. **Вывод принимают оба инструмента.** Это не перестраховка: у формы строк
//!    распакованный массив и упакованный двумерный `verilator` принимает, а
//!    `yosys` отвергает синтаксически; признаки диспетчера без умолчаний
//!    `yosys` объявляет защёлкой, а `verilator` молчит. Каждый инструмент видит
//!    свою половину.
//! 5. **Цель `sv-mmio` печатает ту же таблицу** - генератор у них общий.
//!
//! Тождественность поведения этим набором **не** доказывается: её предмет - потактовая
//! сверка `conformance_fsm_table_sv_tests` (крейт `takt-sim`).

use std::path::{Path, PathBuf};
use std::process::Command;

/// Простой автомат: условное ребро, блоки `enter`/`exit`, возврат назад.
///
/// Порт назван `led`, а не `probe`: имя пробы даёт имя модуля, и порт с тем же именем
/// verilator встречает отказом "Variable has same name as CELLINLINE".
const SIMPLE: &str = "\
model Counter {
    var n: u8 := 0;
    out led: u8;

    start Low {
        always {
            n := n + 1;
            led := n;
        }
        ref High: n = 3;
    }

    state High {
        enter {
            led := 100;
        }
        always {
            n := n + 1;
            led := n;
        }
        ref Low: n = 6;
        exit {
            led := 200;
        }
    }
}
start Main = Counter;
";

fn taktc() -> Command {
    Command::new(env!("CARGO_BIN_EXE_taktc"))
}

fn tool(bin: &str) -> bool {
    Command::new(bin)
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Уникальный по тесту каталог.
fn work_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("main")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0441_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог теста");
    dir
}

/// Компилирует пробу целью `sv`; отдаёт `(успех, stderr, текст модуля)`.
fn compile(dir: &Path, target: &str, extra: &[&str]) -> (bool, String, String) {
    let input = dir.join("probe.takt");
    std::fs::write(&input, SIMPLE).expect("запись пробы");
    let out = taktc()
        .arg("compile")
        .args(["-t", target])
        .args(extra)
        .arg(&input)
        .arg("-o")
        .arg(dir.join("out"))
        .output()
        .expect("запуск taktc compile");
    let text = std::fs::read_to_string(dir.join("out").join("probe.sv")).unwrap_or_default();
    (
        out.status.success(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
        text,
    )
}

/// Тело `unique case` уровня модели - до строки, с которой начинается диспетчер.
fn case_bodies(source: &str) -> String {
    let mut inside = false;
    let mut collected = String::new();
    for line in source.lines() {
        if line.contains("unique case (") {
            inside = true;
            continue;
        }
        if inside && line.trim_start().starts_with("takt_fired_") {
            inside = false;
            continue;
        }
        if inside {
            collected.push_str(line);
            collected.push('\n');
        }
    }
    collected
}

#[test]
fn sv_table_form_prints_transitions_as_data() {
    let dir = work_dir("prints");
    let (ok, stderr, text) = compile(&dir, "sv", &["--fsm=table"]);
    assert!(ok, "компиляция табличной формы цели sv: {stderr}");
    assert!(
        text.contains("localparam logic [3:0] PROBE_COUNTER_TRANS_FROM = "),
        "нет вектора состояний-источников:\n{text}"
    );
    assert!(
        text.contains("localparam logic [3:0] PROBE_COUNTER_TRANS_TO = "),
        "нет вектора состояний-приёмников:\n{text}"
    );
    assert!(
        text.contains("for (int unsigned takt_row = 0; takt_row < 2; takt_row++) begin"),
        "нет просмотра строк:\n{text}"
    );
    assert!(
        text.contains("takt_fired_probe_counter") && text.contains("takt_ok_probe_counter"),
        "нет признаков диспетчера:\n{text}"
    );
    // Умолчания признаков обязательны: без них yosys объявляет их защёлкой.
    assert!(
        text.contains("takt_fired_probe_counter = 1'b0;"),
        "нет умолчания признака — yosys объявит его защёлкой:\n{text}"
    );
}

#[test]
fn sv_table_form_empties_case_bodies_of_transitions() {
    let dir = work_dir("bodies");
    let (ok, stderr, text) = compile(&dir, "sv", &["--fsm=table"]);
    assert!(ok, "компиляция табличной формы: {stderr}");
    let bodies = case_bodies(&text);
    assert!(
        bodies.contains("probe_counter_n_next = (probe_counter_n_next + 1);"),
        "тело состояния потеряно:\n{bodies}"
    );
    assert!(
        !bodies.contains("probe_counter_state_next = PROBE_COUNTER_"),
        "переход остался в ветви case:\n{bodies}"
    );
}

#[test]
fn sv_default_form_has_no_table() {
    let dir = work_dir("default");
    let (ok, stderr, text) = compile(&dir, "sv", &[]);
    assert!(ok, "компиляция формы по умолчанию: {stderr}");
    assert!(
        !text.contains("_TRANS_FROM"),
        "умолчание изменилось — появилась таблица:\n{text}"
    );
    assert!(
        text.contains("if ((probe_counter_n_next == 3)) begin"),
        "форма по умолчанию потеряла условие ребра:\n{text}"
    );
}

#[test]
fn sv_table_form_is_accepted_by_verilator() {
    if !tool("verilator") {
        eprintln!("verilator недоступен — шаг пропущен");
        return;
    }
    let dir = work_dir("verilator");
    let (ok, stderr, _) = compile(&dir, "sv", &["--fsm=table"]);
    assert!(ok, "компиляция табличной формы: {stderr}");
    let run = Command::new("verilator")
        .args(["--lint-only", "-Wall"])
        .arg(dir.join("out").join("probe.sv"))
        .output()
        .expect("запуск verilator");
    assert!(
        run.status.success(),
        "verilator отверг табличную форму:\n{}",
        String::from_utf8_lossy(&run.stderr)
    );
}

#[test]
fn sv_table_form_is_synthesized_by_yosys() {
    if !tool("yosys") {
        eprintln!("yosys недоступен — шаг пропущен");
        return;
    }
    let dir = work_dir("yosys");
    let (ok, stderr, _) = compile(&dir, "sv", &["--fsm=table"]);
    assert!(ok, "компиляция табличной формы: {stderr}");
    let script = format!(
        "read_verilog -sv {}; synth -top probe",
        dir.join("out").join("probe.sv").display()
    );
    let run = Command::new("yosys")
        .args(["-q", "-p", &script])
        .output()
        .expect("запуск yosys");
    assert!(
        run.status.success(),
        "yosys не синтезировал табличную форму:\n{}",
        String::from_utf8_lossy(&run.stderr)
    );
}

#[test]
fn sv_mmio_prints_the_same_table() {
    let dir = work_dir("mmio");
    let (ok, stderr, text) = compile(&dir, "sv-mmio", &["--fsm=table"]);
    assert!(ok, "компиляция табличной формы цели sv-mmio: {stderr}");
    assert!(
        text.contains("PROBE_COUNTER_TRANS_FROM") && text.contains("takt_fired_probe_counter"),
        "цель sv-mmio не напечатала таблицу — генератор у них общий:\n{text}"
    );
}
