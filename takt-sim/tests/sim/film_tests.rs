//! GIF прогона ключом `-o`: кадр на такт, схема по файлу раскладки рядом с моделью.
//!
//! Тесты сквозные - гоняют бинарник: решение "писать ли файл" и отказы живут в нём.

use std::path::{Path, PathBuf};
use std::process::{Command, Output};

const DATA: &str = "tests/data/film";

/// Каталог теста: уникален по процессу и потоку, пустой.
fn dir(tag: &str) -> PathBuf {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!(
            "takt_film_{tag}_{}",
            std::thread::current()
                .name()
                .unwrap_or("single")
                .replace(':', "_")
        ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог теста");
    dir
}

fn sim(args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_takt-sim"))
        .args(args)
        .output()
        .expect("запуск симулятора")
}

fn s(path: &Path) -> &str {
    path.to_str().expect("путь")
}

#[test]
fn the_run_gif_has_a_frame_per_tick() {
    let out_dir = dir("gif").join("out");
    let out = sim(&[&format!("{DATA}/line.takt"), "-o", s(&out_dir), "-n", "4"]);
    assert!(
        out.status.success(),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let trace = String::from_utf8_lossy(&out.stdout);
    assert_eq!(
        trace.lines().filter(|l| l.contains("Line")).count(),
        4,
        "четыре такта трассы: {trace}"
    );

    let file = std::fs::File::open(out_dir.join("line.gif")).expect("GIF прогона");
    let mut decoder = gif::DecodeOptions::new()
        .read_info(std::io::BufReader::new(file))
        .expect("GIF читается");
    let mut frames = 0;
    while let Some(frame) = decoder.read_next_frame().expect("кадр") {
        assert_eq!(frame.delay, 50, "пауза по умолчанию - 500 мс");
        frames += 1;
    }
    assert_eq!(frames, 4, "кадр на такт");
    let names: Vec<_> = std::fs::read_dir(&out_dir)
        .expect("каталог вывода")
        .map(|e| e.expect("запись").file_name())
        .collect();
    assert_eq!(names, ["line.gif"], "временного файла не осталось");
}

#[test]
fn without_a_layout_the_run_is_refused_before_it_starts() {
    let work = dir("no_layout");
    let model = work.join("line.takt");
    std::fs::copy(format!("{DATA}/line.takt"), &model).expect("модель");
    let out_dir = work.join("out");
    let out = sim(&[s(&model), "-o", s(&out_dir), "-n", "4"]);
    let err = String::from_utf8_lossy(&out.stderr);
    assert!(!out.status.success(), "без раскладки - отказ");
    assert!(err.contains("раскладки нет"), "причина названа: {err}");
    assert!(
        String::from_utf8_lossy(&out.stdout).trim().is_empty(),
        "отказ до прогона: трассы нет"
    );
    assert!(!out_dir.exists(), "файлов на диске не появилось");

    // Неполный файл: состояния `Finished` в нём нет.
    let layout = std::fs::read_to_string(format!("{DATA}/line.takt-ui")).expect("раскладка");
    let mut value: serde_json::Value = serde_json::from_str(&layout).expect("JSON");
    let nodes = value["sheets"]["/"]["nodes"]
        .as_object_mut()
        .expect("узлы корня");
    assert!(
        nodes.remove("Finished").is_some(),
        "фикстура размещает Finished"
    );
    std::fs::write(work.join("line.takt-ui"), value.to_string()).expect("запись");
    let out = sim(&[s(&model), "-o", s(&out_dir), "-n", "4"]);
    let err = String::from_utf8_lossy(&out.stderr);
    assert!(!out.status.success() && err.contains("Finished"), "{err}");
    assert!(!out_dir.exists(), "файлов на диске не появилось");
}

#[test]
fn the_removed_graphics_config_key_is_refused_in_words() {
    let out = sim(&[
        &format!("{DATA}/line.takt"),
        "--graphics-config",
        "any.json",
    ]);
    let err = String::from_utf8_lossy(&out.stderr);
    assert!(!out.status.success(), "снятый ключ - отказ");
    assert!(
        err.contains("`--graphics-config` снят"),
        "причина названа: {err}"
    );
}
