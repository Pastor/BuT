//! Заметки диагностики доезжают до пользователя эталона -.
//!
//! # Что здесь тестытся
//!
//! Формат диагностики - свойство самой диагностики, а не бинарника. Печать заметок жила
//! **только** в `format_compile_error`, а `takt-sim` строил текст своей функцией, где
//! цикла по заметкам не было.
//!
//! | Потребитель | Что печатал |
//! |---|---|
//! | `taktc` | `SE-106` **и** сноску "состояния есть у вложенной модели 'Helper'" |
//! | `takt-sim` | только `SE-106` |
//!
//! Сноска - единственный указатель выхода из этой ситуации, и именно её пользователь
//! эталона не видел.
//!
//! Тест сквозной - гоняет **бинарник**: печать живёт в `bin/takt_sim.rs`, и проверять
//! надо ровно то, что увидит пользователь (образец - `diagnostics_tests`).

use std::process::Command;

/// Библиотека: состояния объявлены у вложенной модели - обёртка их не наследует.
const LIB: &str = "model Helper {\n\
                   \x20   var n: u8 := 0;\n\
                   \x20   start Idle {\n\
                   \x20       always { n := n + 1; }\n\
                   \x20       ref Idle: n < 3;\n\
                   \x20   }\n\
                   }\n";

/// Приложение подключает файл целиком и ссылается на обёртку.
const APP: &str = "import \"lib.takt\";\n\nstart Main = Lib;\n";

fn workspace() -> std::path::PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0279_sim_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание каталога");
    std::fs::write(dir.join("lib.takt"), LIB).expect("запись библиотеки");
    std::fs::write(dir.join("app.takt"), APP).expect("запись приложения");
    dir
}

/// **A3: эталон печатает заметку так же, как компилятор.**
#[test]
fn simulator_prints_diagnostic_notes() {
    let dir = workspace();
    let out = Command::new(env!("CARGO_BIN_EXE_takt-sim"))
        .args([
            dir.join("app.takt").to_str().expect("путь в UTF-8"),
            "-I",
            dir.to_str().expect("путь в UTF-8"),
            "--steps",
            "1",
        ])
        .output()
        .expect("запуск симулятора");
    assert!(!out.status.success(), "вход обязан отвергаться");
    let err = String::from_utf8_lossy(&out.stderr);
    assert!(err.contains("[SE-106]"), "код диагностики потерян: {err}");
    assert!(
        err.contains("примечание:"),
        "заметка не доехала до пользователя эталона: {err}"
    );
    assert!(
        err.contains("вложенной модели 'Helper'"),
        "заметка обязана называть вложенную модель: {err}"
    );
}
