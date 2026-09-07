//! Импорт переносит функции библиотеки.
//!
//! # Три места, где это ломалось независимо
//!
//! 1. **перенос** - `SE-017` на выборочном импорте;
//! 2. **владелец** - цель `c` строит имя функции из владельца
//!    (`<Модель>_<функция>`); оставив ссылку на корень библиотеки, импорт дал бы
//!    вызов функции, которую этот файл не объявляет;
//! 3. **импорт целиком** - `import "lib.takt";` требовал импортировать `'p'` и
//!    `'r'`, параметр и локальную переменную функции: по владельцу они
//!    неотличимы от глобальной переменной библиотеки.
//!
//! Проверяется вывод, а не карта семантики: перенос в карту прошёл бы и в тот день,
//! когда владельца забыли перепривязать, - а именно это и было первой редакцией исправления
//! (вызов отвечал `SE-004`, потому что карта затиралась в конце обхода).

use std::path::PathBuf;
use std::process::Command;

/// Библиотека: тип, операция над ним и функция с локальными именами.
const LIBRARY: &str = "\
struct Loop {
    gain: float,
    acc: float
}

fn loop_reset(l: Loop) -> Loop {
    var r: Loop := l;
    r.acc := 0.0;
    return r;
}

fn loop_step(l: Loop, x: float) -> Loop {
    var r: Loop := l;
    var delta: float := l.gain * x;
    r.acc := l.acc + delta;
    return r;
}
";

fn work_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("main")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0182_04_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог теста");
    dir
}

/// Кладёт библиотеку и применение, компилирует применение целью `c`.
fn compile_c(tag: &str, application: &str) -> (PathBuf, String, i32) {
    let dir = work_dir(tag);
    std::fs::write(dir.join("library.takt"), LIBRARY).expect("запись библиотеки");
    let app = dir.join("app.takt");
    std::fs::write(&app, application).expect("запись применения");
    let out = dir.join("out");

    let result = Command::new(env!("CARGO_BIN_EXE_taktc"))
        .arg("compile")
        .arg("-t")
        .arg("c")
        .arg(&app)
        .arg("-o")
        .arg(&out)
        .output()
        .expect("запуск taktc");
    (
        out,
        String::from_utf8_lossy(&result.stderr).into_owned(),
        result.status.code().unwrap_or(-1),
    )
}

#[test]
fn selected_function_is_imported_and_callable() {
    let (out, stderr, code) = compile_c(
        "selected",
        "\
import {Loop, loop_reset} from \"library.takt\";

model App {
    var l: Loop := {2.0, 7.0};
    start Run {
        always {
            l := loop_reset(l);
        }
    }
}
start Main = App;
",
    );
    assert_eq!(code, 0, "{stderr}");

    let source = std::fs::read_to_string(out.join("app.c")).expect("порождённый .c");
    // Владелец - Импортёр: имя функции в цели `c` строится из него.
    assert!(
        source.contains("App_loop_reset"),
        "функция обязана принадлежать импортёру:\n{source}"
    );
    assert!(
        !source.contains("Library_loop_reset"),
        "владелец остался у библиотеки — вызов уйдёт в никуда:\n{source}"
    );
}

#[test]
fn imported_function_keeps_its_body() {
    // Тело обязано доехать целиком: перенос "пустой" функции дал бы компилируемый, но
    // неверный вывод - класс исправления.
    let (out, stderr, code) = compile_c(
        "body",
        "\
import {Loop, loop_step} from \"library.takt\";

model App {
    var l: Loop := {2.0, 0.0};
    start Run {
        always {
            l := loop_step(l, 3.0);
        }
    }
}
start Main = App;
",
    );
    assert_eq!(code, 0, "{stderr}");

    let source = std::fs::read_to_string(out.join("app.c")).expect("порождённый .c");
    assert!(
        source.contains("gain") && source.contains("acc"),
        "тело функции обязано доехать до вывода:\n{source}"
    );
}

#[test]
fn aliased_function_is_renamed() {
    let (out, stderr, code) = compile_c(
        "alias",
        "\
import {Loop, loop_reset as clear} from \"library.takt\";

model App {
    var l: Loop := {2.0, 7.0};
    start Run {
        always {
            l := clear(l);
        }
    }
}
start Main = App;
",
    );
    assert_eq!(code, 0, "{stderr}");

    let source = std::fs::read_to_string(out.join("app.c")).expect("порождённый .c");
    assert!(source.contains("App_clear"), "{source}");
}

#[test]
fn whole_file_import_does_not_demand_local_names() {
    // Прежде эта форма отвечала `SE-074`, требуя импортировать 'r' и 'delta' -
    // локальные имена тела функции, которых библиотека не объявляет и импортировать
    // которые нельзя в принципе.
    let (_, stderr, code) = compile_c(
        "whole",
        "\
import \"library.takt\";

model App {
    var l: Loop := {2.0, 0.0};
    start Run {
        always {
            l := loop_step(l, 1.0);
        }
    }
}
start Main = App;
",
    );
    assert_eq!(code, 0, "{stderr}");
    assert!(
        !stderr.contains("SE-074"),
        "локальные имена тела не являются объявлениями библиотеки: {stderr}"
    );
}

#[test]
fn unknown_name_still_reports_se017() {
    // Граница: появление категории `functions` не должно скрыть настоящую ошибку
    // "такого имени в библиотеке нет".
    let (_, stderr, code) = compile_c(
        "unknown",
        "\
import {loop_nope} from \"library.takt\";

model App {
    start Run;
}
start Main = App;
",
    );
    assert_ne!(code, 0, "{stderr}");
    assert!(stderr.contains("SE-017"), "{stderr}");
}
