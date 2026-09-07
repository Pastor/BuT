//! отказ `taktc fmt` печатается общим форматом диагностики.
//!
//! До 0202 `format_source` склеивал `Vec<Diagnostic>` в строку **внутри** библиотеки
//! (`format!("{d:?}")`), и пользователь получал `Debug`-дамп: байтовые смещения вместо
//! `строка:колонка`, экранированные кавычки, а несколько ошибок разбора - одной
//! нечитаемой строкой.
//!
//! Раздел документа о диагностике при этом объявляет формат **единым** и прямо называет
//! форматирование среди его источников - то есть дефект нарушал нормативное описание, а
//! не только вкус.
//!
//! **Главная проверка - тождественность вывода `fmt` и `compile`**
//! ([`fmt_error_matches_compile_error`]), а не отсутствие слова `Diagnostic`. Проверка
//! на запрещённую подстроку поймала бы дамп, но пропустила бы любое расхождение формата
//! (скажем, "Ошибка форматирования" вместо "Ошибка компиляции" или потерянный код) - а
//! предмет фичи именно единство формата.
//!
//! Наблюдается так, как это видит пользователь: прогоном бинаря `taktc`
//! (`CARGO_BIN_EXE_taktc`) и перехватом stderr.

use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, Stdio};

/// Файл с **одной** синтаксической ошибкой (`var x u8` - пропущено `:`).
const ONE_ERROR: &str =
    "model M {\n    var x u8 := 1;\n    start S { always { } }\n}\nstart Main = M;\n";

/// Файл с **двумя** синтаксическими ошибками.
const TWO_ERRORS: &str = "model M {\n    var x u8 := 1;\n    var y u8 := 2;\n    start S { always { } }\n}\nstart Main = M;\n";

/// Файл с узлом `formula` - прежний пример непечатаемого узла.
///
/// С форматтер его **печатает**, и это ровно то, что здесь теперь проверяется: отказ на
/// нём был свойством инструмента, а не языка.
const FORMULA_NODE: &str =
    "model M {\n    formula \"ltl\" { }\n    start S { always { } }\n}\nstart Main = M;\n";

fn taktc() -> Command {
    Command::new(env!("CARGO_BIN_EXE_taktc"))
}

/// Уникальный по тесту каталог (: тесты идут параллельно, и общий каталог был
/// источником гонок).
fn work_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("main")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0202_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог теста");
    dir
}

/// Кладёт исходник в уникальный каталог и возвращает путь к нему.
fn fixture(tag: &str, source: &str) -> PathBuf {
    let path = work_dir(tag).join("probe.takt");
    std::fs::write(&path, source).expect("запись фикстуры");
    path
}

/// stderr и код возврата `taktc fmt --check <файл>`.
fn fmt_check(path: &std::path::Path) -> (String, i32) {
    let out = taktc()
        .arg("fmt")
        .arg("--check")
        .arg(path)
        .output()
        .expect("запуск taktc fmt");
    (
        String::from_utf8_lossy(&out.stderr).into_owned(),
        out.status.code().unwrap_or(-1),
    )
}

/// stderr `taktc compile` - эталон формата.
fn compile_stderr(path: &std::path::Path) -> String {
    let out_dir = path.parent().expect("каталог").join("out");
    let out = taktc()
        .arg("compile")
        .arg("-t")
        .arg("c")
        .arg(path)
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("запуск taktc compile");
    String::from_utf8_lossy(&out.stderr).into_owned()
}

/// **A2 (главный критерий).** Отказ `fmt` печатается тем же текстом, что и
/// отказ `compile` на том же входе.
///
/// Сравниваются **строки целиком**: позиция, вид, код, текст.
#[test]
fn fmt_error_matches_compile_error() {
    let path = fixture("match", ONE_ERROR);
    let (fmt, _) = fmt_check(&path);
    let compile = compile_stderr(&path);

    assert_eq!(
        fmt.trim(),
        compile.trim(),
        "вывод `fmt` обязан совпадать с выводом `compile` на одном входе:\n\
         fmt:     {fmt:?}\n\
         compile: {compile:?}"
    );
    assert!(
        fmt.contains("probe.takt:2:11: Ошибка компиляции [SY-002]:"),
        "ожидался общий формат `путь:строка:колонка: вид [код]: текст`, получено: {fmt:?}"
    );
}

/// **A3.** Две ошибки разбора дают **две** строки, а не одну.
///
/// Разбор накапливает диагностики; прежде вектор печатался одним `Debug`-блобом, и
/// накопление до пользователя не доезжало.
#[test]
fn two_parse_errors_print_two_lines() {
    let path = fixture("two", TWO_ERRORS);
    let (fmt, _) = fmt_check(&path);

    let lines: Vec<&str> = fmt
        .lines()
        .filter(|l| l.contains("Ошибка компиляции"))
        .collect();
    assert_eq!(lines.len(), 2, "ожидались две строки диагностики: {fmt:?}");
    assert!(lines[0].contains(":2:11:"), "первая строка: {:?}", lines[0]);
    assert!(lines[1].contains(":3:11:"), "вторая строка: {:?}", lines[1]);
}

/// **A4.** Внутреннее представление наружу не выходит.
///
/// Вспомогательная проверка: сама по себе она слаба (см. докблок модуля), но именно эти
/// подстроки видел пользователь до фичи.
#[test]
fn internal_representation_never_printed() {
    let path = fixture("internal", ONE_ERROR);
    let (fmt, _) = fmt_check(&path);

    for forbidden in ["Diagnostic {", "Source(", "code: Some", "level:", "\\\""] {
        assert!(
            !fmt.contains(forbidden),
            "во выводе осталось внутреннее представление ({forbidden:?}): {fmt:?}"
        );
    }
}

/// **A5.** Коды возврата не изменились: отказ - это `1`.
///
/// Контракт для CI (`--check`) фича не трогает: дефект был чисто в тексте.
#[test]
fn exit_codes_unchanged() {
    let path = fixture("codes", ONE_ERROR);
    let (_, code) = fmt_check(&path);
    assert_eq!(code, 1, "`fmt --check` на битом файле обязан вернуть 1");

    let out = taktc().arg("fmt").arg(&path).output().expect("запуск fmt");
    assert_eq!(out.status.code(), Some(1), "`fmt` на месте — тоже 1");
}

/// **A6.** Узел `formula` печатается, а не отвергается.
///
/// Ожидание менялось дважды. привела отказ к общей форме
/// (`FM-001` с путём, строкой и колонкой вместо своей строки без позиции), а
/// завела **печать** - и отказа не стало вовсе. Проверка осталась
/// здесь как тест второго перехода: `fmt --check` на этом входе сообщает о
/// неканоничности (код 1) либо принимает его, но `FM-001` не печатает никогда.
#[test]
fn unsupported_node_is_a_diagnostic_with_position() {
    let path = fixture("unsupported", FORMULA_NODE);
    let (fmt, _) = fmt_check(&path);

    assert!(
        !fmt.contains("FM-001"),
        "узел `formula` печатается с фичи 0405 — отказа быть не должно: {fmt:?}"
    );
}

/// **A7.** У `--stdin` пути нет, поэтому нет и префикса позиции - но код и
/// текст на месте.
///
/// Отсутствие префикса здесь **правильно**: выдумывать координаты файла,
/// которого не существует, нельзя (докблок `position_prefix`). Проверка стоит
/// затем, чтобы это осталось решением, а не случайностью.
#[test]
fn stdin_has_code_and_text_but_no_path_prefix() {
    let mut child = taktc()
        .arg("fmt")
        .arg("--stdin")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("запуск taktc fmt --stdin");
    child
        .stdin
        .as_mut()
        .expect("stdin")
        .write_all(ONE_ERROR.as_bytes())
        .expect("запись в stdin");
    let out = child.wait_with_output().expect("ожидание taktc");
    let stderr = String::from_utf8_lossy(&out.stderr);

    assert!(
        stderr.contains("Ошибка компиляции [SY-002]:"),
        "код и вид диагностики обязаны быть и без файла: {stderr:?}"
    );
    assert!(
        !stderr.contains(".takt:"),
        "пути у stdin нет — префикс позиции выдумывать нельзя: {stderr:?}"
    );
    assert_eq!(out.status.code(), Some(1));
}
