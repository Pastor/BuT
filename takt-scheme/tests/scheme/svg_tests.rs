//! Чертёж листа: детерминизм, виды, легенда, шрифты, строка трассы, снимки.
//!
//! Снимки лежат в `tests/data/snapshots/` без вшитых шрифтов (иначе каждый файл
//! весил бы сотню килобайт base64); пересборка - `TAKT_SCHEME_UPDATE=1`. Картинки
//! для глаза выгружает `TAKT_SCHEME_OUT=<каталог>`.

use std::path::{Path, PathBuf};

use takt_scheme::run::{Active, Segment, Tick};
use takt_scheme::style::View;
use takt_scheme::svg::{Options, render_all};

fn root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).to_path_buf()
}

fn read(path: &Path) -> String {
    std::fs::read_to_string(path).unwrap_or_else(|e| panic!("{}: {e}", path.display()))
}

fn model(name: &str) -> (String, String) {
    let data = root().join("tests/data");
    let source = if name == "line" {
        read(&data.join("line.takt"))
    } else {
        read(&root().join(format!("../examples/{name}.takt")))
    };
    (source, read(&data.join(format!("{name}.takt-ui"))))
}

fn draft(legend: bool) -> Options {
    Options {
        view: View::Draft,
        legend,
        tick: None,
        trace: None,
        fonts: false,
    }
}

fn render(name: &str, options: &Options) -> Vec<(String, String)> {
    let (source, layout) = model(name);
    let out = render_all(&source, &layout, options).expect("чертёж");
    if let Ok(dir) = std::env::var("TAKT_SCHEME_OUT") {
        let dir = PathBuf::from(dir);
        std::fs::create_dir_all(&dir).expect("каталог");
        let view = match options.view {
            View::Draft => "draft",
            View::Run => "run",
        };
        // Для глаза - со вшитыми шрифтами: без них браузер наберёт чертёж чужим.
        let (source, layout) = model(name);
        let fonted = render_all(
            &source,
            &layout,
            &Options {
                fonts: true,
                ..options.clone()
            },
        )
        .expect("чертёж");
        for (key, svg) in &fonted {
            let stem = takt_scheme::svg::file_stem(key, name);
            std::fs::write(
                dir.join(format!(
                    "{stem}.{view}{}.svg",
                    if options.legend { ".legend" } else { "" }
                )),
                svg,
            )
            .expect("запись");
        }
    }
    out
}

/// Шаг `step` листа `Line` корневой модели.
fn step(step: usize, state: &str, done: bool) -> Active {
    Active {
        path: vec![Segment {
            owner: "Line".into(),
            step,
            model: "Heater".into(),
        }],
        model: Some("Heater".into()),
        state: state.into(),
        done,
    }
}

#[test]
fn the_drawing_is_deterministic_and_has_no_canvas_view() {
    for name in ["elevator", "pid_heater", "line"] {
        let a = render(name, &draft(true));
        let b = render(name, &draft(true));
        assert_eq!(a, b, "{name}: два чертежа - один текст");
        for (key, svg) in &a {
            for canvas in [
                "edge-halo",
                "edge-hit",
                "class=\"pin",
                "node-enter",
                "radial-gradient",
                "cursor",
            ] {
                assert!(
                    !svg.contains(canvas),
                    "{name} {key}: вид холста '{canvas}' попал в чертёж"
                );
            }
        }
    }
}

#[test]
fn every_sheet_is_drawn_including_compositions() {
    let keys: Vec<String> = render("elevator", &draft(false))
        .into_iter()
        .map(|(k, _)| k)
        .collect();
    assert_eq!(keys, ["/", "/#Middle", "Engine"]);
    let line: Vec<String> = render("line", &draft(false))
        .into_iter()
        .map(|(k, _)| k)
        .collect();
    assert_eq!(line, ["/", "/#Line", "Heater"]);
    assert_eq!(takt_scheme::svg::file_stem("/#Line", "line"), "line#Line");
    assert_eq!(
        takt_scheme::svg::file_stem("Plant/Pump", "line"),
        "Plant.Pump"
    );
}

#[test]
fn the_draft_view_has_only_ink() {
    for (key, svg) in render("line", &draft(false)) {
        for colour in [
            "#DFE6D5",
            "#F6EFDC",
            "#F5E7E1",
            "#C6A24A",
            "#5A6154\" stroke-width=\"1.5\"/>",
        ] {
            assert!(
                !svg.contains(colour) || colour.starts_with("#5A6154"),
                "{key}: цвет прогона {colour} в чертёжном виде"
            );
        }
        assert!(svg.contains("stroke=\"#2E332B\""), "{key}: чернила");
    }
}

#[test]
fn the_run_view_lights_the_instance_and_counts_them() {
    let tick = Tick {
        active: vec![step(2, "Heating", false), step(3, "Heating", false)],
        next: vec![],
    };
    let options = Options {
        view: View::Run,
        legend: false,
        tick: Some(tick),
        trace: None,
        fonts: false,
    };
    let out = render("line", &options);
    let sheet = |key: &str| {
        out.iter()
            .find(|(k, _)| k == key)
            .map(|(_, s)| s.clone())
            .expect("лист")
    };
    let composition = sheet("/#Line");
    assert_eq!(
        composition
            .matches("fill=\"#DFE6D5\" stroke=\"#5F6E55\" stroke-width=\"2.5\"")
            .count(),
        2,
        "горят две ветви параллели"
    );
    assert!(
        composition.contains("fill=\"#F6EFDC\""),
        "последний шаг достижим"
    );
    let model = sheet("Heater");
    assert!(
        model.contains(">2</text>"),
        "на листе модели - число экземпляров"
    );
    let root = sheet("/");
    assert!(
        root.contains("Heater#2: Heating, Heater#3: Heating"),
        "плашка квадрата перечисляет шаги"
    );
}

#[test]
fn the_legend_is_by_the_key_and_names_the_marks() {
    let with = render("elevator", &draft(true));
    let engine = with
        .iter()
        .find(|(k, _)| k == "Engine")
        .map(|(_, s)| s)
        .expect("лист");
    assert!(engine.contains("Ожидание"), "подпись автора");
    assert!(
        engine.contains("S1 → S2") || engine.contains("→"),
        "переход условия"
    );
    let without = render("elevator", &draft(false));
    let engine = without
        .iter()
        .find(|(k, _)| k == "Engine")
        .map(|(_, s)| s)
        .expect("лист");
    assert!(!engine.contains("Ожидание"), "без легенды подписи нет");
}

#[test]
fn the_picture_carries_only_the_faces_it_uses() {
    let options = Options {
        fonts: true,
        ..draft(false)
    };
    let (_, svg) = render("line", &options).into_iter().next().expect("лист");
    assert_eq!(svg.matches("@font-face").count(), 1, "только ГОСТ");
    assert!(svg.contains("font-family:\"ГОСТ 2.304-81\";src:url(data:font/ttf;base64,"));
    let framed = Options {
        fonts: true,
        trace: Some("Шаг   1:  [Line, Heating]".into()),
        ..draft(false)
    };
    let (_, svg) = render("line", &framed).into_iter().next().expect("лист");
    assert_eq!(
        svg.matches("@font-face").count(),
        2,
        "строке трассы нужен моноширинный"
    );
    assert!(svg.contains("Шаг   1:  [Line, Heating]"));
}

#[test]
fn snapshots_hold_the_drawing() {
    let dir = root().join("tests/data/snapshots");
    let update = std::env::var("TAKT_SCHEME_UPDATE").is_ok();
    let tick = Tick {
        active: vec![step(1, "Done", true)],
        next: vec![],
    };
    let cases = [
        ("elevator", draft(true), "draft.legend"),
        ("line", draft(false), "draft"),
        (
            "line",
            Options {
                view: View::Run,
                legend: true,
                tick: Some(tick),
                trace: Some("Шаг   7:  [Line, Done]".into()),
                fonts: false,
            },
            "run",
        ),
    ];
    let mut checked = 0;
    for (name, options, view) in cases {
        for (key, svg) in render(name, &options) {
            let path = dir.join(format!(
                "{}.{view}.svg",
                takt_scheme::svg::file_stem(&key, name)
            ));
            if update {
                std::fs::create_dir_all(&dir).expect("каталог");
                std::fs::write(&path, &svg).expect("снимок");
            }
            assert_eq!(
                svg,
                read(&path),
                "{}: чертёж разошёлся со снимком (TAKT_SCHEME_UPDATE=1)",
                path.display()
            );
            checked += 1;
        }
    }
    assert!(checked >= 9, "снимков сверено {checked}");
}
