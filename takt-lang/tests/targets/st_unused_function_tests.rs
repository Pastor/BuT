//! Неиспользуемая функция у цели `st` не печатается.
//!
//! # Что доказывает набор
//!
//! 1. Функция, которую никто не зовёт, из вывода **исчезает** - так же, как у
//!    целей `c`, `rust` и `sv`: признак у всех один (`UsageSet::functions`).
//! 2. Функция, вызываемая **только из условия ребра**, остаётся: условие
//!    ребра - такое же использование, как вызов в теле.
//! 3. Вызываемая функция остаётся - контроль (иначе "фильтр" означал бы
//!    "печатать нечего").
//! 4. Вывод принимает `iec2c`.
//!
//! Проверка цели класса не видел по устройству: мёртвый POU `iec2c`
//! **принимает**. Цена - лишний код в программе ПЛК; после подстановки тела
//! там оставалось объявление функции, вызовов которой в файле уже
//! нет.

use std::path::{Path, PathBuf};
use std::process::Command;

const PROBE: &str = "\
model Worker {
    var n: u8 := 0;
    out led: u8 at 0x40000100;

    fn dead_helper(v: u8) -> u8 {
        return v * 3;
    }

    fn live_helper(v: u8) -> u8 {
        return v + 1;
    }

    fn edge_ready(v: u8) -> bit {
        return v > 3;
    }

    start Run {
        always {
            n := n + 1;
            led := live_helper(n);
        }
        ref Done: edge_ready(n) = 1;
    }

    state Done {
        always { led := 200; }
        ref Done: n > 0;
    }
}
start Main = Worker;
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
        .join(format!("takt_0445_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог теста");
    dir
}

/// Компилирует пробу целью `st`; отдаёт `(успех, stderr, текст)`.
fn compile(dir: &Path) -> (bool, String, String) {
    let input = dir.join("probe.takt");
    std::fs::write(&input, PROBE).expect("запись пробы");
    let out = taktc()
        .arg("compile")
        .args(["-t", "st"])
        .arg(&input)
        .arg("-o")
        .arg(dir.join("out"))
        .output()
        .expect("запуск taktc compile");
    let text = std::fs::read_to_string(dir.join("out").join("probe.st")).unwrap_or_default();
    (
        out.status.success(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
        text,
    )
}

/// `(бинарник iec2c, каталог lib MatIEC)` - если оба на месте.
fn iec2c_available() -> Option<(PathBuf, PathBuf)> {
    let prefix = std::env::var("IEC2C_PREFIX")
        .map(PathBuf::from)
        .unwrap_or_else(|_| {
            PathBuf::from(std::env::var("HOME").unwrap_or_default()).join(".local")
        });
    let bin = prefix.join("bin").join("iec2c");
    let lib = prefix.join("share").join("matiec").join("lib");
    (bin.is_file() && lib.join("C").is_dir()).then_some((bin, lib))
}

#[test]
fn dead_function_is_not_printed() {
    let dir = work_dir("dead");
    let (ok, stderr, text) = compile(&dir);
    assert!(ok, "компиляция пробы: {stderr}");
    assert!(
        !text.contains("dead_helper"),
        "функция без единого вызова осталась в выводе:\n{text}"
    );
}

#[test]
fn called_functions_stay() {
    let dir = work_dir("live");
    let (ok, stderr, text) = compile(&dir);
    assert!(ok, "компиляция пробы: {stderr}");
    // Контроль: фильтр не должен вычищать всё подряд.
    assert!(
        text.contains("FUNCTION ProbeWorker_live_helper"),
        "вызываемая из тела функция пропала:\n{text}"
    );
    // Условие ребра - такое же использование, как вызов в теле. Без этого фильтр
    // порождал бы ST, ссылающийся на несуществующий POU.
    assert!(
        text.contains("FUNCTION ProbeWorker_edge_ready"),
        "функция, вызываемая только из условия ребра, пропала:\n{text}"
    );
}

#[test]
fn output_is_accepted_by_iec2c() {
    let Some((iec2c, lib)) = iec2c_available() else {
        eprintln!("iec2c недоступен — шаг пропущен");
        return;
    };
    let dir = work_dir("iec2c");
    let (ok, stderr, _) = compile(&dir);
    assert!(ok, "компиляция пробы: {stderr}");
    let work = dir.join("iec");
    std::fs::create_dir_all(&work).expect("рабочий каталог iec2c");
    let run = Command::new(iec2c)
        .arg("-I")
        .arg(lib)
        .arg(dir.join("out").join("probe.st"))
        .current_dir(&work)
        .output()
        .expect("запуск iec2c");
    assert!(
        run.status.success() && work.join("POUS.c").is_file(),
        "iec2c отверг вывод без мёртвого POU:\n{}",
        String::from_utf8_lossy(&run.stderr)
    );
}
