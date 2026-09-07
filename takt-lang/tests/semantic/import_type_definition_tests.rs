//! Импорт переносит устройство типа, а не одно имя.

use std::path::PathBuf;
use std::process::Command;

/// Библиотека: структура и перечисление, ни одного состояния.
const LIBRARY: &str = "\
struct Pid {
    kp: float,
    i_acc: float
}

enum Mode {
    Manual,
    Auto
}
";

/// Применение: подключает и структуру, и перечисление.
const APPLICATION: &str = "\
import {Pid, Mode} from \"library.takt\";

model App {
    var p: Pid := {1.0, 0.0};
    var m: Mode := Auto;
    start Run {
        always {
            p.i_acc := p.i_acc + 1.0;
        }
    }
}
start Main = App;
";

/// Псевдоним: устройство обязано переехать вместе с новым именем.
const ALIASED: &str = "\
import {Pid as Loop} from \"library.takt\";

model App {
    var p: Loop := {1.0, 0.0};
    start Run {
        always {
            p.kp := 2.0;
        }
    }
}
start Main = App;
";

/// Уникальный по тесту каталог.
fn work_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("main")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0182_03_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог теста");
    dir
}

/// Готовит пару "библиотека + применение" и компилирует применение целью `c`.
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
fn imported_struct_reaches_generated_header() {
    let (out, stderr, code) = compile_c("struct", APPLICATION);
    assert_eq!(code, 0, "{stderr}");

    let header = std::fs::read_to_string(out.join("app.h")).expect("порождённый заголовок");
    assert!(
        header.contains("typedef struct Pid"),
        "устройство импортированной структуры обязано доехать до вывода:\n{header}"
    );
    assert!(
        header.contains("Pid p;"),
        "поле объявлено импортированным типом:\n{header}"
    );
}

#[test]
fn imported_enum_reaches_generated_output() {
    let (out, stderr, code) = compile_c("enum", APPLICATION);
    assert_eq!(code, 0, "{stderr}");

    // Перечисление лежит в той же паре карт (`types` + `enums`), и его вариант обязан
    // быть известен цели - иначе `Auto` не с чем сопоставить. Цель `c` печатает
    // варианты константами в `.c`, а не типом в заголовке.
    //
    // Проверка смотрит на имя варианта, а не на имя типа. Комментарий убран - и тест
    // покраснел, ничего не сломав в выводе.
    let source = std::fs::read_to_string(out.join("app.c")).expect("порождённый исходник");
    assert!(
        source.contains("ENUM_APP_MODE_AUTO") && source.contains("ENUM_APP_MODE_MANUAL"),
        "оба варианта импортированного перечисления обязаны доехать до вывода:\n{source}"
    );
}

#[test]
fn aliased_struct_carries_its_definition() {
    let (out, stderr, code) = compile_c("alias", ALIASED);
    assert_eq!(code, 0, "{stderr}");

    let header = std::fs::read_to_string(out.join("app.h")).expect("порождённый заголовок");
    assert!(
        header.contains("typedef struct Loop"),
        "под псевдонимом устройство обязано переехать вместе с именем:\n{header}"
    );
}
