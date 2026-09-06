//! `SE-102` называет файл, подключающий библиотеку.
//!
//! # Что здесь ловится
//!
//! 1. **Импортёр найден и назван** - в заметке диагностики.
//! 2. **Контрпример:** сосед, который библиотеку **не** подключает, в подсказку
//!    не попадает. Без этого "назвали файл" означало бы "назвали любой файл
//!    рядом".
//! 3. **Нет импортёров - нет заметки.** Молчание честнее пустого списка.
//! 4. **Неразобранный сосед не ломает поиск** и не попадает в список: АСД у
//!    него нет, а значит нет и директив импорта (названная граница).
//! 5. **Все три формы `import`** ведут к находке: `import "p";`,
//!    `import "p" as M;`, `import { a } from "p";`.
//! 6. **Координаты у заметки нет:** она говорит о другом файле, и координата в
//!    своём была бы ложью.

use std::path::PathBuf;
use takt_lang::pipeline::{find_importers, importers_note};

const LIBRARY: &str = "\
struct Gains { kp: u8, ki: u8 }

fn scale(a: u8) -> u8 { return a + 1; }
";

/// Уникальный по тесту каталог.
fn work_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("main")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0294_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание каталога");
    dir
}

/// Раскладывает библиотеку и соседей; возвращает каталог и путь библиотеки.
fn layout(tag: &str, neighbours: &[(&str, &str)]) -> (PathBuf, String) {
    let dir = work_dir(tag);
    let lib = dir.join("helper.takt");
    std::fs::write(&lib, LIBRARY).expect("запись библиотеки");
    for (name, text) in neighbours {
        std::fs::write(dir.join(name), text).expect("запись соседа");
    }
    (dir, lib.to_string_lossy().to_string())
}

/// Импортёр найден и назван.
#[test]
fn importer_is_found_and_named() {
    let (_dir, lib) = layout(
        "found",
        &[(
            "app.takt",
            "import \"helper.takt\";\n\nvar n: u8 := 0;\nstart Run { ref Run: false; }\n",
        )],
    );
    let found = find_importers(&lib, &[]);
    assert_eq!(found.len(), 1, "ожидался ровно один импортёр: {found:?}");
    assert!(found[0].ends_with("app.takt"), "{found:?}");
    let note = importers_note(&lib, &[]).expect("подсказка обязана быть");
    assert!(note.contains("app.takt"), "{note}");
}

/// **Контрпример:** сосед без директивы импорта в подсказку не попадает.
#[test]
fn unrelated_neighbour_is_not_named() {
    let (_dir, lib) = layout(
        "unrelated",
        &[(
            "other.takt",
            "var m: u8 := 0;\nstart Idle { ref Idle: false; }\n",
        )],
    );
    assert!(
        find_importers(&lib, &[]).is_empty(),
        "сосед библиотеку не подключает — называть его не за что"
    );
    assert!(
        importers_note(&lib, &[]).is_none(),
        "без импортёров заметки быть не должно"
    );
}

/// Импортёров нет вовсе - заметки нет (молчание честнее пустого списка).
#[test]
fn no_importers_means_no_note() {
    let (_dir, lib) = layout("alone", &[]);
    assert!(importers_note(&lib, &[]).is_none());
}

/// Неразобранный сосед поиск не ломает и в список не попадает.
///
/// Он **мог бы** импортировать библиотеку, но утверждать этого нельзя: АСД у него нет.
/// Ошибка в сторону "подсказки нет", а не ложной подсказки.
#[test]
fn unparsable_neighbour_is_skipped() {
    let (_dir, lib) = layout(
        "broken",
        &[
            ("broken.takt", "model M { ??? }\n"),
            (
                "app.takt",
                "import \"helper.takt\";\n\nstart Run { ref Run: false; }\n",
            ),
        ],
    );
    let found = find_importers(&lib, &[]);
    assert_eq!(
        found.len(),
        1,
        "неразобранный сосед пропускается: {found:?}"
    );
    assert!(found[0].ends_with("app.takt"), "{found:?}");
}

/// Все три формы директивы `import` ведут к находке.
#[test]
fn every_import_form_is_recognised() {
    for (tag, directive) in [
        ("plain", "import \"helper.takt\";"),
        ("alias", "import \"helper.takt\" as Lib;"),
        ("select", "import { scale } from \"helper.takt\";"),
    ] {
        let (_dir, lib) = layout(
            tag,
            &[(
                "app.takt",
                &format!("{directive}\n\nstart Run {{ ref Run: false; }}\n"),
            )],
        );
        let found = find_importers(&lib, &[]);
        assert_eq!(
            found.len(),
            1,
            "форма `{directive}` не распознана: {found:?}"
        );
    }
}

/// Импортёр из каталога `-I` тоже находится.
#[test]
fn importer_from_search_path_is_found() {
    let (dir, lib) = layout("searchpath", &[]);
    let other = work_dir("searchpath_consumer");
    std::fs::write(
        other.join("app.takt"),
        "import \"helper.takt\";\n\nstart Run { ref Run: false; }\n",
    )
    .expect("запись импортёра");
    let found = find_importers(&lib, &[other.to_string_lossy().to_string()]);
    assert_eq!(
        found.len(),
        1,
        "импортёр из каталога поиска обязан находиться: {found:?} (библиотека в {dir:?})"
    );
}
