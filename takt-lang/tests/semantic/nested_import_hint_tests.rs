//! Подсказка о выборочном импорте вложенной модели -.
//!
//! # Что здесь проверяется
//!
//! `import "lib.takt";` вносит к импортёру **обёртку** по имени файла; объявленная
//! внутри неё модель снаружи не видна. Запись `start Main = Helper;` естественна и
//! получала "Модель 'Helper' не найдена" - утверждение, которое автор читает как "такой
//! модели нет".
//!
//!
//! | Запись | Ответ |
//! |---|---|
//! | `import "lib.takt"; start Main = Helper;` | `SE-001` без подсказки |
//! | `import "lib.takt"; start Main = Lib;` | `SE-106` + сноска о вложенной |
//! | `import "lib.takt" as Lib; start Main = Helper;` | `SE-001` |
//! | **`import { Helper } from "lib.takt";`** | **исполняется** |
//!
//! Последняя строка сняла с фичи её первоначальный объём: кандидат утверждал, что
//! ссылка на вложенную модель **невыразима**, и предлагал ввести `Lib::Helper`. Форма
//! уже есть - не хватало не синтаксиса, а **подсказки**.
//!
//! **Контроль обязателен:** обычная опечатка в имени модели подсказки получать не
//! должна, иначе сноска стала бы шумом на каждой `SE-001`.

use std::path::PathBuf;

/// Библиотека: состояния объявлены у вложенной модели.
const LIB: &str = "model Helper {\n\
                   \x20   var n: u8 := 0;\n\
                   \x20   start Idle {\n\
                   \x20       always { n := n + 1; }\n\
                   \x20       ref Idle: n < 3;\n\
                   \x20   }\n\
                   }\n";

fn workspace(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0279_{thread}_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание каталога");
    std::fs::write(dir.join("lib.takt"), LIB).expect("запись библиотеки");
    dir
}

/// Строит дерево приложения; возвращает диагностику отказа.
fn build_error(tag: &str, app: &str) -> takt_lang::diagnostics::Diagnostic {
    let dir = workspace(tag);
    let path = dir.join("app.takt");
    std::fs::write(&path, app).expect("запись приложения");
    let source = std::fs::read_to_string(&path).expect("чтение приложения");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор");
    let search = vec![dir.to_string_lossy().into_owned()];
    takt_lang::semantic::tree::construct_model(&ast, None, &search)
        .expect_err("построение дерева обязано отказать")
}

/// **T1.** Ссылка на вложенную модель получает подсказку о выборочном импорте.
#[test]
fn nested_model_reference_gets_hint() {
    let err = build_error("hint", "import \"lib.takt\";\n\nstart Main = Helper;\n");
    assert_eq!(err.code.as_deref(), Some("SE-001"), "код отказа");
    let note = err
        .notes
        .first()
        .unwrap_or_else(|| panic!("подсказки нет: {}", err.message));
    assert!(
        note.message.contains("import { Helper } from"),
        "подсказка обязана называть форму подключения:\n{}",
        note.message
    );
    assert!(
        note.message.contains("'Lib'"),
        "подсказка обязана называть владельца:\n{}",
        note.message
    );
}

/// **T2. Контроль: опечатка подсказки не получает.**
///
/// Без этой проверки сноска стала бы шумом на каждой `SE-001` - и перестала бы читаться
/// там, где действительно нужна.
#[test]
fn plain_typo_gets_no_hint() {
    let err = build_error("typo", "import \"lib.takt\";\n\nstart Main = Helpr;\n");
    assert_eq!(err.code.as_deref(), Some("SE-001"));
    assert!(
        err.notes.is_empty(),
        "у опечатки подсказки быть не должно: {:?}",
        err.notes
    );
}

/// **T3.** Форма, которую подсказка называет, действительно работает.
///
/// Тест самой подсказки: если бы выборочный импорт вложенной модели не работал, совет
/// вёл бы в тупик - а именно им фича и заменила предложенный кандидатом новый
/// синтаксис.
#[test]
fn selective_import_of_nested_model_works() {
    let dir = workspace("selective");
    let path = dir.join("app.takt");
    let app = "import { Helper } from \"lib.takt\";\n\nstart Main = Helper;\n";
    std::fs::write(&path, app).expect("запись приложения");
    let (ast, _) = takt_lang::parse(app, 0).expect("разбор");
    let search = vec![dir.to_string_lossy().into_owned()];
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &search)
        .expect("выборочный импорт вложенной модели обязан работать");
    assert!(
        model.borrow().search_model("Helper").is_some(),
        "модель 'Helper' обязана быть видна импортёру"
    );
}
