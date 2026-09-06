//! Табличная форма автомата у целей `rust` и `st`.
//!
//! # Что доказывает набор
//!
//! 1. **Форма печатается идиомой целевого языка**: у `rust` - `static` со
//!    строками и методы-разборщики номера, у `st` - константные массивы и
//!    диспетчер `WHILE`. Указателей на функции нет ни там, ни там, и это не
//!    вкус: в IEC их не существует вовсе, а в Rust тип стража зависит от
//!    параметра метода (`tick<H: Hal>`).
//! 2. **Переходы уходят из тел состояний**: в ветви остаётся только тело такта.
//! 3. **Умолчание не изменилось** - контроль, без которого пункт 1 ничего не
//!    значит.
//! 4. **Вывод принимают инструменты целей** (`clippy -D warnings`, `iec2c`).
//! 5. **Флаг у цели, которая формы не печатает, - ошибка CLI** с перечислением
//!    поддерживающих.
//!
//! Тождественность поведения этим набором **не** доказывается: её предмет - потактовые
//! сверки `conformance_fsm_table_rust_tests` и `conformance_fsm_table_st_tests` (крейт
//! `takt-sim`).

use std::path::{Path, PathBuf};
use std::process::Command;

/// Простой автомат: условное ребро, блоки `enter`/`exit`, возврат назад.
const SIMPLE: &str = "\
model Counter {
    var n: u8 := 0;
    out probe: u8;

    start Low {
        always {
            n := n + 1;
            probe := n;
        }
        ref High: n = 3;
    }

    state High {
        enter {
            probe := 100;
        }
        always {
            n := n + 1;
            probe := n;
        }
        ref Low: n = 6;
        exit {
            probe := 200;
        }
    }
}
start Main = Counter;
";

fn taktc() -> Command {
    Command::new(env!("CARGO_BIN_EXE_taktc"))
}

/// Уникальный по тесту каталог.
fn work_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("main")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0440_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог теста");
    dir
}

/// Компилирует пробу заданной целью; отдаёт `(успех, stderr, текст вывода)`.
fn compile(dir: &Path, target: &str, extension: &str, extra: &[&str]) -> (bool, String, String) {
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
    let text = std::fs::read_to_string(dir.join("out").join(format!("probe.{extension}")))
        .unwrap_or_default();
    (
        out.status.success(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
        text,
    )
}

/// Тело диспетчеризации состояний: от `match self.state` / `CASE state OF` до строки, с
/// которой начинается диспетчер таблицы.
fn state_bodies(source: &str, head: &str, tail: &str) -> String {
    let mut inside = false;
    let mut collected = String::new();
    for line in source.lines() {
        if line.contains(head) {
            inside = true;
            continue;
        }
        if inside && line.contains(tail) {
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
fn rust_table_form_prints_transitions_as_data() {
    let dir = work_dir("rust_prints");
    let (ok, stderr, text) = compile(&dir, "rust", "rs", &["--fsm=table"]);
    assert!(ok, "компиляция табличной формы цели rust: {stderr}");
    assert!(
        text.contains("static PROBE_COUNTER_TRANSITIONS: [ProbeCounterTransition; 2] = ["),
        "нет таблицы переходов:\n{text}"
    );
    assert!(
        text.contains("fn takt_guard") && text.contains("fn takt_action"),
        "нет разборщиков номера стража и действия:\n{text}"
    );
    assert!(
        text.contains("fn takt_dispatch") && text.contains("self.takt_dispatch("),
        "нет диспетчера либо его вызова:\n{text}"
    );
    // Переходы ушли из ветвей `match`: тело состояния на месте, присваивания состояния -
    // нет.
    let bodies = state_bodies(&text, "match self.state {", "self.takt_dispatch(");
    assert!(
        bodies.contains("self.n = self.n.wrapping_add(1);"),
        "тело состояния потеряно:\n{bodies}"
    );
    assert!(
        !bodies.contains("self.state = "),
        "переход остался в ветви match:\n{bodies}"
    );
}

#[test]
fn rust_default_form_has_no_table() {
    let dir = work_dir("rust_default");
    let (ok, stderr, text) = compile(&dir, "rust", "rs", &[]);
    assert!(ok, "компиляция формы по умолчанию: {stderr}");
    assert!(
        !text.contains("_TRANSITIONS"),
        "умолчание изменилось — появилась таблица:\n{text}"
    );
    assert!(
        text.contains("if self.n == 3 {"),
        "форма по умолчанию потеряла условие ребра:\n{text}"
    );
}

#[test]
fn rust_table_form_passes_clippy() {
    let dir = work_dir("rust_clippy");
    let (ok, stderr, _) = compile(&dir, "rust", "rs", &["--fsm=table"]);
    assert!(ok, "компиляция табличной формы: {stderr}");
    let wrapper = dir.join("gate.rs");
    std::fs::write(
        &wrapper,
        format!(
            "#![no_std]\n#[path = \"{}\"]\npub mod generated;\n",
            dir.join("out").join("probe.rs").display()
        ),
    )
    .expect("обёртка гейта");
    let run = Command::new("clippy-driver")
        .args(["--edition", "2021", "--crate-type=lib", "-D", "warnings"])
        .arg(&wrapper)
        .arg("--out-dir")
        .arg(dir.join("gate_out"))
        .output();
    let Ok(run) = run else {
        eprintln!("clippy-driver недоступен — шаг пропущен");
        return;
    };
    assert!(
        run.status.success(),
        "clippy отверг табличную форму цели rust:\n{}",
        String::from_utf8_lossy(&run.stderr)
    );
}

#[test]
fn st_table_form_prints_transitions_as_data() {
    let dir = work_dir("st_prints");
    let (ok, stderr, text) = compile(&dir, "st", "st", &["--fsm=table"]);
    assert!(ok, "компиляция табличной формы цели st: {stderr}");
    assert!(
        text.contains("TAKT_TRANS_FROM : ARRAY [0..1] OF USINT := [1, 2];"),
        "нет массива состояний-источников:\n{text}"
    );
    assert!(
        text.contains("TAKT_TRANS_GUARD : ARRAY") && text.contains("TAKT_TRANS_TO : ARRAY"),
        "таблица неполна:\n{text}"
    );
    assert!(
        text.contains("WHILE (takt_trans_row <= 1) AND (NOT takt_trans_fired) DO"),
        "нет диспетчера:\n{text}"
    );
    // Переходы ушли из ветвей `CASE`: тело состояния на месте, а переход в другое
    // состояние печатает только диспетчер.
    let bodies = state_bodies(&text, "CASE state OF", "takt_trans_row := 0;");
    assert!(
        bodies.contains("n := n + 1;"),
        "тело состояния потеряно:\n{bodies}"
    );
    assert!(
        !bodies.contains("(* High *)\n            probe := 100;"),
        "блок enter остался в ветви CASE:\n{bodies}"
    );
}

#[test]
fn st_default_form_has_no_table() {
    let dir = work_dir("st_default");
    let (ok, stderr, text) = compile(&dir, "st", "st", &[]);
    assert!(ok, "компиляция формы по умолчанию: {stderr}");
    assert!(
        !text.contains("TAKT_TRANS_FROM"),
        "умолчание изменилось — появилась таблица:\n{text}"
    );
    assert!(
        text.contains("IF n = 3 THEN"),
        "форма по умолчанию потеряла условие ребра:\n{text}"
    );
}

#[test]
fn st_table_form_is_accepted_by_iec2c() {
    let prefix = std::env::var("IEC2C_PREFIX")
        .map(PathBuf::from)
        .unwrap_or_else(|_| {
            PathBuf::from(std::env::var("HOME").unwrap_or_default()).join(".local")
        });
    let iec2c = prefix.join("bin").join("iec2c");
    let lib = prefix.join("share").join("matiec").join("lib");
    if !iec2c.is_file() || !lib.is_dir() {
        eprintln!("iec2c недоступен — шаг пропущен");
        return;
    }
    let dir = work_dir("st_iec2c");
    let (ok, stderr, _) = compile(&dir, "st", "st", &["--fsm=table"]);
    assert!(ok, "компиляция табличной формы: {stderr}");
    let work = dir.join("iec2c");
    std::fs::create_dir_all(&work).expect("рабочий каталог");
    let run = Command::new(&iec2c)
        .arg("-I")
        .arg(&lib)
        .arg(dir.join("out").join("probe.st"))
        .current_dir(&work)
        .output()
        .expect("запуск iec2c");
    assert!(
        run.status.success() && work.join("POUS.c").is_file(),
        "iec2c отверг табличную форму цели st:\n{}",
        String::from_utf8_lossy(&run.stderr)
    );
}

#[test]
fn table_flag_names_supporting_targets() {
    let dir = work_dir("targets");
    // `plantuml` - единственная цель без табличной формы: диаграмма переходов и есть
    // отношение переходов, второй его формы у неё быть не может.
    let (ok, stderr, _) = compile(&dir, "plantuml", "puml", &["--fsm=table"]);
    assert!(
        !ok,
        "флаг у цели без табличной формы обязан быть ошибкой:\n{stderr}"
    );
    for target in ["c", "c-hal", "rust", "st", "st-at", "sv", "sv-mmio"] {
        assert!(
            stderr.contains(target),
            "ошибка не называет цель '{target}':\n{stderr}"
        );
    }
}
