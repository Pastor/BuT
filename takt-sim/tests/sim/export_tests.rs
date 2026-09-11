//! Подкоманды `takt-sim export` и `takt-sim project` на трёх формах проекта.
//!
//! Тесты сквозные - гоняют бинарник: разбор ключей, каталог вывода и отказы
//! живут в нём.

use std::path::{Path, PathBuf};
use std::process::{Command, Output};

const DATA: &str = "tests/data/export";

/// Каталог теста: уникален по процессу и потоку, пустой.
fn dir(tag: &str) -> PathBuf {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!(
            "takt_export_{tag}_{}",
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

fn names(dir: &Path) -> Vec<String> {
    let mut out: Vec<String> = std::fs::read_dir(dir)
        .expect("каталог вывода")
        .map(|e| {
            e.expect("запись")
                .file_name()
                .to_string_lossy()
                .into_owned()
        })
        .collect();
    out.sort();
    out
}

fn ok(out: &Output) {
    assert!(
        out.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
}

/// Копия каталога фикстуры: вывод по умолчанию ложится рядом с проектом.
fn copy(from: &Path, to: &Path) {
    std::fs::create_dir_all(to).expect("каталог копии");
    for entry in std::fs::read_dir(from).expect("фикстура") {
        let entry = entry.expect("запись");
        std::fs::copy(entry.path(), to.join(entry.file_name())).expect("копия");
    }
}

#[test]
fn a_directory_project_exports_every_view_and_format() {
    let work = dir("directory");
    let project = work.join("project");
    copy(Path::new(&format!("{DATA}/project")), &project);
    // Без `-o` вывод ложится в `export/` проекта; библиотека без состояний
    // (`helper.takt`) рисовать нечего - её в выводе нет.
    ok(&export(&[
        s(&project),
        "--format",
        "svg",
        "--format",
        "png",
    ]));
    assert_eq!(
        names(&project.join("export")),
        ["plant.draft.png", "plant.draft.svg"]
    );
    let out = work.join("run");
    ok(&export(&[
        s(&project),
        "-o",
        s(&out),
        "--view",
        "run",
        "--format",
        "svg",
        "--legend",
        "off",
    ]));
    let svg = std::fs::read_to_string(out.join("plant.run.svg")).expect("SVG");
    assert!(svg.contains("@font-face"), "SVG несёт шрифт");
    assert!(
        svg.contains("#DFE6D5"),
        "цветной вид подсвечивает последний такт"
    );
    assert!(!svg.contains("наполнение"), "легенда выключена");
}

#[test]
fn an_archive_gives_the_same_pictures_as_its_directory() {
    let work = dir("archive");
    let project = takt_project::load(Path::new(&format!("{DATA}/project"))).expect("проект");
    let zip = takt_project::pack(&takt_project::Export {
        manifest: project.manifest.clone(),
        sources: project.files.clone(),
        generated: Vec::new(),
        refusal: None,
    })
    .expect("архив");
    let archive = work.join("plant.zip");
    std::fs::write(&archive, zip).expect("запись архива");
    for (from, to) in [
        (format!("{DATA}/project"), "a"),
        (s(&archive).to_string(), "b"),
    ] {
        ok(&export(&[
            &from,
            "-o",
            s(&work.join(to)),
            "--format",
            "svg",
            "--format",
            "png",
        ]));
    }
    for name in ["plant.draft.svg", "plant.draft.png"] {
        assert_eq!(
            std::fs::read(work.join("a").join(name)).expect("каталог"),
            std::fs::read(work.join("b").join(name)).expect("архив"),
            "{name}: каталог и архив дают одно"
        );
    }
}

#[test]
fn one_model_exports_all_its_sheets() {
    let out = dir("model");
    ok(&export(&[
        &format!("{DATA}/model/line.takt"),
        "-o",
        s(&out),
        "--format",
        "png",
        "--background",
        "none",
    ]));
    assert_eq!(
        names(&out),
        ["Heater.draft.png", "line#Line.draft.png", "line.draft.png"]
    );
    let png = std::fs::read(out.join("line.draft.png")).expect("PNG");
    assert_eq!(&png[..8], b"\x89PNG\r\n\x1a\n", "PNG");
}

#[test]
fn a_video_has_a_frame_per_tick_of_the_run() {
    let out = dir("video");
    ok(&export(&[
        &format!("{DATA}/project"),
        "-o",
        s(&out),
        "--format",
        "gif",
        "--format",
        "mp4",
    ]));
    assert_eq!(names(&out), ["plant.run.gif", "plant.run.mp4"]);
    // Тот же прогон без подкоманды: такт трассы - кадр видео.
    let run = sim(&[
        &format!("{DATA}/project/plant.takt"),
        "-s",
        &format!("{DATA}/project/plant_start.json"),
    ]);
    let ticks = String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter(|l| l.trim_start().starts_with("Шаг"))
        .count();
    let file = std::fs::File::open(out.join("plant.run.gif")).expect("GIF");
    let mut decoder = gif::DecodeOptions::new()
        .read_info(std::io::BufReader::new(file))
        .expect("GIF читается");
    let mut frames = 0;
    while decoder.read_next_frame().expect("кадр").is_some() {
        frames += 1;
    }
    assert!(ticks > 1, "прогон дал такты: {ticks}");
    assert_eq!(frames, ticks, "кадр на такт");
}

#[test]
fn a_missing_layout_refuses_the_whole_export() {
    let work = dir("refusal");
    let project = work.join("project");
    copy(Path::new(&format!("{DATA}/project")), &project);
    std::fs::write(
        project.join("other.takt"),
        "start A {\n    ref B;\n}\nstate B;\n",
    )
    .expect("вторая модель");
    let manifest = std::fs::read_to_string(project.join("takt-project.json")).expect("манифест");
    let manifest = manifest.replace(
        r#"{ "name": "helper.takt", "kind": "takt" },"#,
        r#"{ "name": "helper.takt", "kind": "takt" },
    { "name": "other.takt", "kind": "takt" },"#,
    );
    std::fs::write(project.join("takt-project.json"), manifest).expect("манифест");
    let out = work.join("out");
    let run = export(&[s(&project), "-o", s(&out), "--format", "svg"]);
    let err = String::from_utf8_lossy(&run.stderr);
    assert!(
        !run.status.success(),
        "у одной модели нет раскладки - отказ"
    );
    assert!(
        err.contains("other.takt") && err.contains("раскладки нет"),
        "{err}"
    );
    assert!(!out.exists(), "частичного вывода нет");
}

#[test]
fn contradictory_or_unknown_keys_are_refused() {
    let out = dir("keys");
    let project = format!("{DATA}/project");
    let run = export(&[
        &project,
        "-o",
        s(&out),
        "--view",
        "draft",
        "--format",
        "gif",
    ]);
    assert!(!run.status.success());
    assert!(
        String::from_utf8_lossy(&run.stderr).contains("видео - всегда цветной вид"),
        "причина названа"
    );
    let run = export(&[&project, "--graphics-config", "x.json"]);
    assert!(!run.status.success(), "неизвестный ключ подкоманды - отказ");
    let run = export(&[&project, "-o", s(&out), "--sheet", "Nope"]);
    assert!(String::from_utf8_lossy(&run.stderr).contains("листа `Nope` нет"));
    assert!(names(&out).is_empty(), "после отказов вывода нет");
}

fn export(args: &[&str]) -> Output {
    let mut all = vec!["export"];
    all.extend_from_slice(args);
    sim(&all)
}

#[test]
fn the_project_is_described_for_scripts() {
    let run = sim(&["project", &format!("{DATA}/project")]);
    ok(&run);
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "проект: Бак (каталог)\n\
         активный файл: plant.takt\n\
         активный сценарий: plant_start.json\n\
         цель: st\n\
         ключи: -\n\
         файлы:\n\
         \x20 helper.takt\ttakt\n\
         \x20 notes.md\tmarkdown\n\
         \x20 plant.takt\ttakt\n\
         \x20 plant.takt-ui\tlayout\n\
         \x20 plant_start.json\tscenario\n\
         сценарии:\n\
         \x20 helper.takt\t-\n\
         \x20 plant.takt\tplant_start.json\n"
    );
    let examples = Path::new(env!("CARGO_MANIFEST_DIR")).join("../examples");
    let owner = sim(&[
        "project",
        "--owner",
        "elevator_mini_floor2.json",
        s(&examples.join("elevator.takt")),
        s(&examples.join("elevator_mini.takt")),
    ]);
    ok(&owner);
    assert!(
        String::from_utf8_lossy(&owner.stdout)
            .trim()
            .ends_with("elevator_mini.takt"),
        "самая длинная основа побеждает"
    );
    let none = sim(&[
        "project",
        "--owner",
        "pump_x.json",
        s(&examples.join("elevator.takt")),
    ]);
    assert!(!none.status.success(), "ничей сценарий - код 1");
}
