//! Порядок стадий построения - один носитель.
//!
//! ## Что здесь ловится
//!
//! Последовательность стадий `0 -> 1 -> 2 -> 3 -> 5 -> 4 -> 6` жила в **двух** местах:
//! `semantic/stages/mod.rs` (объявленный носитель) и
//! `semantic/tree.rs::construct_model_impl` - путь **импорта**. Копии разошлись: у
//! подключаемого файла не выполнялись `collect_clock`, `specialize_instantiations` и
//! `constify_parameters`.
//!
//!
//! | № | Вход | Корневой файл | Через `import` |
//! |---|---|---|---|
//! | 1 | `cond T = after 5s;` | `SE-068` | молчание, код 0 |
//! | 2 | `clock 1kHz;` + цель `c` | `SE-069` | молчание, профиль "часы" |
//! | 3 | две разные частоты | `SE-067` | молчание |
//! | 4 | `specialize`, без аргументов | валидный C | `cc`: no member named ... |
//! | 5 | `specialize`, с аргументами | `#define ..._P1_LIMIT 30` | `cc`: no member named ... |

use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
use std::process::Command;
use takt_lang::generator::GenerateOptions;

/// Временный каталог теста - уникален по имени потока.
///
/// После слияния тестовых целей имя потока несёт `::` - двоеточие вычищается, иначе
/// путь неверен на части файловых систем.
fn temp_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("stage_order")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt-0296-{tag}-{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание временного каталога");
    dir
}

/// Пишет файл в каталог и возвращает путь.
fn write(dir: &Path, name: &str, source: &str) -> PathBuf {
    let path = dir.join(name);
    std::fs::write(&path, source).expect("запись файла пробы");
    path
}

/// Коды диагностик, которые даёт компиляция целью `c`.
fn codes_of_c(dir: &Path, entry: &Path, options: &GenerateOptions) -> Vec<String> {
    let source = std::fs::read_to_string(entry).expect("чтение входа");
    let out = dir.join("out");
    match takt_lang::compile_to_c(
        entry.to_str().unwrap(),
        &source,
        out.to_str().unwrap(),
        &[dir.to_str().unwrap().to_string()],
        options,
    ) {
        Ok(warnings) => warnings.into_iter().filter_map(|d| d.code).collect(),
        Err(d) => vec![d.code.unwrap_or_else(|| "?".to_string())],
    }
}

/// Модель с выдержкой в **именованном условии** - место, где `after` незаконен.
const AFTER_IN_COND: &str = "model Helper {\n\
    \x20   var n: u8 := 0;\n\
    \x20   cond Timeout = after 5s;\n\n\
    \x20   start Idle {\n\
    \x20       always { n := n + 1; }\n\
    \x20       ref Idle: Timeout;\n\
    \x20   }\n\
}\n";

/// Модель, объявляющая частоту тактирования.
const CLOCKED: &str = "model Helper {\n\
    \x20   clock 1kHz;\n\
    \x20   var n: u8 := 0;\n\n\
    \x20   start Idle {\n\
    \x20       always { n := n + 1; }\n\
    \x20       ref Idle: n = 3;\n\
    \x20   }\n\
}\n";

/// Модель с параметром - предмет режима `--parameters=specialize`.
const TUNER: &str = "model Tuner {\n\
    \x20   parameter limit: u8 := 100;\n\
    \x20   var acc: u8 := 0;\n\n\
    \x20   start Count {\n\
    \x20       always { acc := acc + 5; }\n\
    \x20       ref Done: acc >= limit;\n\
    \x20   }\n\n\
    \x20   state Done;\n\
}\n";

/// **T1: перечисление стадий в проекте одно.**
///
/// Признак перечисления - вызовы **чужих** стадий: файл, зовущий две и более стадии,
/// которых сам не определяет, и есть носитель порядка. Самовызов (рекурсия стадии по
/// вложенным моделям) законен и не считается.
///
/// Падение - списком: пропустив одно место, тест вернул бы расхождение.
#[test]
fn stage_order_is_written_once() {
    let mut carriers = Vec::new();
    let mut files = Vec::new();
    collect_rs(Path::new("src"), &mut files);
    assert!(
        !files.is_empty(),
        "исходники не найдены — это дефект сторожа, а не чистота дерева (урок 0230)"
    );
    for path in files {
        let Ok(text) = std::fs::read_to_string(&path) else {
            continue;
        };
        let mut called: BTreeSet<u8> = BTreeSet::new();
        for stage in 0u8..=6 {
            let name = format!("construct_model_stage{stage}");
            let defines = text.contains(&format!("fn {name}"));
            let calls = text.contains(&format!("{name}("));
            if calls && !defines {
                called.insert(stage);
            }
        }
        if called.len() >= 2 {
            carriers.push(format!("{}: стадии {:?}", path.display(), called));
        }
    }
    assert_eq!(
        carriers.len(),
        1,
        "порядок стадий обязан быть описан ровно в одном месте \
         (semantic/stages/mod.rs), найдено:\n{}",
        carriers.join("\n")
    );
    assert!(
        carriers[0].contains("stages"),
        "носителем порядка обязан быть semantic/stages/mod.rs, найдено: {}",
        carriers[0]
    );
}

/// Рекурсивно собирает `.rs`-файлы каталога.
fn collect_rs(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect_rs(&path, out);
        } else if path.extension().is_some_and(|e| e == "rs") {
            out.push(path);
        }
    }
}

/// **T2: `SE-068` не зависит от того, чей это файл.**
///
/// Тот же текст, поданный корнем и подключённый через `import`, обязан дать один
/// вердикт.
#[test]
fn after_outside_edge_is_rejected_through_import() {
    let dir = temp_dir("se068");
    let helper = write(&dir, "helper.takt", AFTER_IN_COND);
    let root = write(
        &dir,
        "root.takt",
        "import \"helper.takt\";\n\n\
         model Main {\n\
         \x20   var k: u8 := 0;\n\n\
         \x20   start Run {\n\
         \x20       always { k := k + 1; }\n\
         \x20       ref Run: k = 200;\n\
         \x20   }\n\
         }\n\n\
         start Boot = Main;\n",
    );
    let options = GenerateOptions::default();
    let direct = codes_of_c(&dir, &helper, &options);
    let imported = codes_of_c(&dir, &root, &options);
    assert!(
        direct.contains(&"SE-068".to_string()),
        "контроль: в корневом файле выдержка вне ребра обязана отвергаться, получено {direct:?}"
    );
    assert!(
        imported.contains(&"SE-068".to_string()),
        "через границу импорта правило обязано действовать так же, получено {imported:?}"
    );
}

/// **T3: `SE-067` ловит конфликт частот через границу импорта.**
///
/// Единица компиляции - файл **вместе** со своими импортами; частота у неё одна.
#[test]
fn conflicting_clock_across_import_is_se067() {
    let dir = temp_dir("se067");
    write(&dir, "helper.takt", CLOCKED);
    let root = write(
        &dir,
        "root.takt",
        "import \"helper.takt\";\n\n\
         model Main {\n\
         \x20   clock 8MHz;\n\
         \x20   var k: u8 := 0;\n\n\
         \x20   start Run {\n\
         \x20       always { k := k + 1; }\n\
         \x20       ref Run: k = 5;\n\
         \x20   }\n\
         }\n\n\
         start Boot = Main;\n",
    );
    let mut options = GenerateOptions::default();
    options.tick_hz = Some(8_000_000);
    let codes = codes_of_c(&dir, &root, &options);
    assert!(
        codes.contains(&"SE-067".to_string()),
        "две разные частоты в одной сборке обязаны отвергаться, получено {codes:?}"
    );
}

/// **T4: `clock` подключённого файла - контракт всей сборки.**
///
/// Без совпадающего `--tick-hz` цель `c` обязана отказать `SE-069`; с ним - перевести.
#[test]
fn imported_clock_binds_the_build() {
    let dir = temp_dir("se069");
    write(&dir, "helper.takt", CLOCKED);
    let root = write(
        &dir,
        "root.takt",
        "import \"helper.takt\";\n\n\
         model Main {\n\
         \x20   var k: u8 := 0;\n\n\
         \x20   start Run {\n\
         \x20       always { k := k + 1; }\n\
         \x20       ref Run: after 5s;\n\
         \x20   }\n\
         }\n\n\
         start Boot = Main;\n",
    );
    let without = codes_of_c(&dir, &root, &GenerateOptions::default());
    assert!(
        without.contains(&"SE-069".to_string()),
        "частота подключённого файла обязана требовать --tick-hz, получено {without:?}"
    );
    let mut ticked = GenerateOptions::default();
    ticked.tick_hz = Some(1000);
    let with = codes_of_c(&dir, &root, &ticked);
    assert!(
        !with.contains(&"SE-069".to_string()),
        "совпадающая частота обязана приниматься, получено {with:?}"
    );
}

/// **T5: `specialize` на импортированной модели без аргументов даёт валидный C.**
///
/// Проверяется не факт компиляции `taktc` (он и прежде возвращал ноль), а приёмка
/// вывода **чужим** инструментом: прежде `cc` отвечал "no member named 'limit' in
/// 'struct ...Tuner'".
#[test]
fn specialize_of_imported_model_emits_valid_c() {
    if !cc_available() {
        eprintln!("cc недоступен — проверка вывода пропущена");
        return;
    }
    let dir = temp_dir("spec-ok");
    write(&dir, "tuner_lib.takt", TUNER);
    let root = write(
        &dir,
        "app.takt",
        "import { Tuner } from \"tuner_lib.takt\";\n\nstart Main = Tuner;\n",
    );
    let source = std::fs::read_to_string(&root).unwrap();
    let out = dir.join("out");
    let mut options = GenerateOptions::default();
    options.specialize = true;
    takt_lang::compile_to_c(
        root.to_str().unwrap(),
        &source,
        out.to_str().unwrap(),
        &[dir.to_str().unwrap().to_string()],
        &options,
    )
    .expect("перевод целью c");
    let compile = Command::new("cc")
        .args([
            "-std=c11",
            "-Wall",
            "-Wextra",
            "-Wno-unused-parameter",
            "-Werror",
            "-c",
        ])
        .arg("-I")
        .arg(&out)
        .arg(out.join("app.c"))
        .arg("-o")
        .arg(dir.join("app.o"))
        .output()
        .expect("запуск cc");
    assert!(
        compile.status.success(),
        "порождённый C не принимается компилятором:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
}

/// **T6: специализация импортированной модели с аргументами работает.**
///
/// Прежде этот тест закреплял отказ `SE-120` - утверждение, переставшее быть верным:
/// сняла границу, перепривязав тела копии к ней самой. Контроль - тот же вход **в одном
/// файле**, где специализация работала всегда.
#[test]
fn specialize_with_arguments_across_import_works() {
    let dir = temp_dir("spec-import");
    write(&dir, "tuner_lib.takt", TUNER);
    let root = write(
        &dir,
        "app.takt",
        "import { Tuner } from \"tuner_lib.takt\";\n\n\
         start Main = Tuner(limit := 30) | Tuner(limit := 60);\n",
    );
    let mut options = GenerateOptions::default();
    options.specialize = true;
    let codes = codes_of_c(&dir, &root, &options);
    assert!(
        codes.is_empty(),
        "специализация модели из другого файла обязана работать, получено {codes:?}"
    );

    // Контроль: без границы импорта тот же вход специализируется - то есть разницы
    // между "своей" и "подключённой" моделью больше нет.
    let single = write(
        &dir,
        "single.takt",
        &format!("{TUNER}\nstart Main = Tuner(limit := 30) | Tuner(limit := 60);\n"),
    );
    let control = codes_of_c(&dir, &single, &options);
    assert!(
        control.is_empty(),
        "в одном файле специализация обязана работать, получено {control:?}"
    );
}

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}
