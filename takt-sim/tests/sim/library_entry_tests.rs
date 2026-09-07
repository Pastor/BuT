//! Симулятор отвечает библиотечному файлу тем же, что и цели.
//!
//! Тест не дублирует `takt-lang/tests/library_entry_tests.rs`, а закрывает
//! **вторую** точку входа исполнения. Правило одно (`validate_entry_model`), но
//! позвать его обязаны оба инструмента: без проверки эталон библиотеку принимает и
//! рапортовал "Завершено: модель достигла терминального состояния за 1 шагов" -
//! успешный прогон автомата, которого в файле нет.

use std::path::PathBuf;
use std::process::Command;

/// Библиотека: типы и функции, ни одного состояния.
const LIBRARY: &str = "\
struct Pid {
    kp: float,
    integral: float
}

fn pid_reset(p: Pid) -> Pid {
    var r: Pid := p;
    r.integral := 0.0;
    return r;
}
";

/// Уникальный по тесту каталог.
fn work_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("main")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_sim_0182_02_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог теста");
    dir
}

#[test]
fn simulator_rejects_library_file() {
    let dir = work_dir("entry");
    let path = dir.join("library.takt");
    std::fs::write(&path, LIBRARY).expect("запись библиотеки");

    let result = Command::new(env!("CARGO_BIN_EXE_takt-sim"))
        .arg(&path)
        .arg("-n")
        .arg("3")
        .output()
        .expect("запуск takt-sim");
    let stderr = String::from_utf8_lossy(&result.stderr).into_owned();
    let stdout = String::from_utf8_lossy(&result.stdout).into_owned();

    assert_ne!(
        result.status.code(),
        Some(0),
        "исполнять в библиотеке нечего: {stderr}{stdout}"
    );
    assert!(stderr.contains("[SE-102]"), "ожидался SE-102: {stderr}");
    assert!(
        !stdout.contains("Завершено"),
        "прогон несуществующего автомата: {stdout}"
    );
}

#[test]
fn simulator_still_runs_a_model() {
    // Контрпример: обычный файл симулируется - проверка не должна отвергать
    // то, ради чего инструмент существует.
    let dir = work_dir("model");
    let path = dir.join("model.takt");
    std::fs::write(
        &path,
        "var v: u8 := 0;\nstart Run {\n    always {\n        v := v + 1;\n    }\n}\n",
    )
    .expect("запись модели");

    let result = Command::new(env!("CARGO_BIN_EXE_takt-sim"))
        .arg(&path)
        .arg("-n")
        .arg("3")
        .output()
        .expect("запуск takt-sim");

    assert_eq!(
        result.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&result.stderr)
    );
}
