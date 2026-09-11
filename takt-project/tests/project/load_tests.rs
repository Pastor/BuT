//! Три формы проекта: каталог с манифестом, архив и одна модель.

use std::path::{Path, PathBuf};

use takt_project::{Error, Export, Form, Kind, Limits, load, pack, unpack};

fn data(name: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/data")
        .join(name)
}

/// Каталог с манифестом: состав называет манифест, выбор автора - его поля.
#[test]
fn a_directory_with_a_manifest_is_read_as_the_manifest_says() {
    let project = load(&data("directory")).expect("каталог");
    assert_eq!(project.form, Form::Directory);
    let names: Vec<&str> = project.files.iter().map(|f| f.name.as_str()).collect();
    assert_eq!(
        names,
        [
            "heater.takt",
            "heater.takt-ui",
            "heater_warm.json",
            "lib.takt",
            "notes.md"
        ],
        "файл рядом, которого манифест не называет, в проект не входит"
    );
    assert_eq!(project.manifest.main_file.as_deref(), Some("heater.takt"));
    assert_eq!(
        project.manifest.main_scenario.as_deref(),
        Some("heater_warm.json")
    );
    assert_eq!(project.manifest.build_target, "rust");
    assert_eq!(project.manifest.build_args, "--fsm=table");
    assert_eq!(
        project.file("heater.takt-ui").map(|f| f.kind.as_str()),
        Some("layout")
    );
    assert_eq!(
        project.layout_of("heater.takt").map(|f| f.name.as_str()),
        Some("heater.takt-ui")
    );
    assert!(
        project.layout_of("lib.takt").is_none(),
        "у библиотеки раскладки нет"
    );
    assert_eq!(project.scenarios_of("heater.takt"), ["heater_warm.json"]);
    // Состав для компилятора - модели проекта: импорт разрешается по нему.
    assert_eq!(
        project.models().keys().collect::<Vec<_>>(),
        ["heater.takt", "lib.takt"]
    );
}

/// Архив - та же форма обмена, что у сервиса: круговой рейс через диск.
#[test]
fn an_archive_is_read_back_as_the_same_project() {
    let project = load(&data("directory")).expect("каталог");
    let bytes = pack(&Export {
        manifest: project.manifest.clone(),
        sources: project.files.clone(),
        generated: vec![("heater.h".into(), "// вывод цели".into())],
        refusal: None,
    })
    .expect("архив");
    let dir = tempfile::tempdir().expect("каталог");
    let path = dir.path().join("heater.zip");
    std::fs::write(&path, &bytes).expect("запись");
    let back = load(&path).expect("архив");
    assert_eq!(back.form, Form::Archive);
    assert_eq!(back.manifest, project.manifest);
    assert_eq!(back.files, project.files, "вывод цели в состав не попал");
    assert_eq!(back.root, dir.path());
    // Сервис судит архив своими пределами, командная строка - без них.
    let tight = Limits {
        file_bytes: 1024,
        files: 2,
        project_bytes: 1 << 20,
    };
    assert!(matches!(unpack(&bytes, tight), Err(Error::Limit(_))));
}

/// Одна модель: соседи по соглашению имён, сценарии - по правилу принадлежности.
#[test]
fn a_single_model_takes_its_neighbours_by_names() {
    let project = load(&data("model/elevator_mini.takt")).expect("модель");
    assert_eq!(project.form, Form::Model);
    let names: Vec<&str> = project.files.iter().map(|f| f.name.as_str()).collect();
    assert_eq!(
        names,
        [
            "elevator_mini.md",
            "elevator_mini.takt",
            "elevator_mini.takt-ui",
            "elevator_mini_floor2.json"
        ]
    );
    assert_eq!(project.manifest.name, "elevator_mini");
    assert_eq!(
        project.manifest.main_file.as_deref(),
        Some("elevator_mini.takt")
    );
    assert_eq!(
        project.manifest.main_scenario.as_deref(),
        Some("elevator_mini_floor2.json"),
        "единственный подходящий сценарий - активный"
    );
    assert!(
        project.manifest.build_target.is_empty(),
        "цель задаёт команда"
    );

    // `elevator` соседнего `elevator_mini_floor2.json` не забирает: основа длиннее
    // у `elevator_mini`.
    let other = load(&data("model/elevator.takt")).expect("модель");
    let names: Vec<&str> = other.files.iter().map(|f| f.name.as_str()).collect();
    assert_eq!(names, ["elevator.takt", "elevator_rush.json"]);
    assert_eq!(
        other.of_kind(Kind::Layout).count(),
        0,
        "раскладки у этой модели нет"
    );
}

/// Отказы названы: каталог без манифеста, манифест с отсутствующим файлом, чужой
/// путь.
#[test]
fn refusals_name_the_reason() {
    let error = load(&data("model")).expect_err("каталог без манифеста");
    assert!(error.message().contains("takt-project.json"), "{error}");

    let dir = tempfile::tempdir().expect("каталог");
    std::fs::write(
        dir.path().join("takt-project.json"),
        r#"{"format": 5, "name": "x", "files": [{"name": "gone.takt", "kind": "takt"}]}"#,
    )
    .expect("запись");
    let error = load(dir.path()).expect_err("файла нет");
    assert!(error.message().contains("gone.takt"), "{error}");

    std::fs::write(
        dir.path().join("takt-project.json"),
        r#"{"format": 99, "name": "x"}"#,
    )
    .expect("запись");
    let error = load(dir.path()).expect_err("формат новее");
    assert!(error.message().contains("99"), "{error}");

    let error = load(&data("model/notes.txt")).expect_err("чужой род");
    assert!(matches!(error, Error::Invalid(_)), "{error}");
}
