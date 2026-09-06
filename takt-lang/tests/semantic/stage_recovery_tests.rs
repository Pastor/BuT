//! стадии построения 4-6 накапливают диагностики по соседям.
//!
//! До 0152 стадии построения были терминальны целиком: две независимые ошибки в телах
//! разных состояний показывались по одной, хотя и лексер с разбором, и проверки
//! `validate` уже накапливали. Терминальный слой лежал **между** двумя накапливающими.
//!
//! Граница, которую фича провела:
//!
//! - **соседи** (именованные блоки, тела функций) - накапливаем: проба
//!   показала, что вторая диагностика там самостоятельная причина;
//! - **предпосылки** (имена, типы, переменные, `cond`) - терминально: после них
//!   продолжение даёт каскад следствий.
//!
//! Неполное дерево наружу **не выходит**: при ошибках `construct_stages` отдаёт список
//! диагностик и отбрасывает дерево. Поэтому "частично построенной модели" для семи
//! целей, симулятора и верификации не существует, а отсутствие порождённых артефактов -
//! гарантия по построению, а не проверка.

use std::path::PathBuf;
use std::process::Command;

fn taktc() -> Command {
    Command::new(env!("CARGO_BIN_EXE_taktc"))
}

/// Уникальный по тесту каталог.
fn work_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("main")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0152_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог теста");
    dir
}

/// Компилирует исходник целью `c`; возвращает (stderr, каталог вывода).
fn compile(tag: &str, source: &str) -> (String, PathBuf) {
    let dir = work_dir(tag);
    let src = dir.join("probe.takt");
    std::fs::write(&src, source).expect("запись фикстуры");
    let out = dir.join("out");
    let result = taktc()
        .arg("compile")
        .arg("-t")
        .arg("c")
        .arg(&src)
        .arg("-o")
        .arg(&out)
        .output()
        .expect("запуск taktc");
    (String::from_utf8_lossy(&result.stderr).into_owned(), out)
}

/// Число строк-диагностик в выводе.
fn errors(stderr: &str) -> Vec<&str> {
    stderr
        .lines()
        .filter(|l| l.contains("Ошибка компиляции"))
        .collect()
}

/// **A1.** Две независимые ошибки в телах разных состояний - две диагностики.
///
/// Ключевой замер фичи: `nope1` и `nope2` - самостоятельные причины, ни одна не
/// следствие другой.
#[test]
fn two_state_body_errors_are_both_reported() {
    let (stderr, _) = compile(
        "states",
        "model M {\n    var ok: u8 := 0;\n    start S {\n        always { ok := nope1 + 1; }\n        ref T: ok > 0;\n    }\n    state T {\n        always { ok := nope2 + 2; }\n    }\n}\nstart Main = M;\n",
    );
    let lines = errors(&stderr);
    assert_eq!(lines.len(), 2, "ожидались две диагностики: {stderr:?}");
    assert!(lines[0].contains("nope1"), "первая: {:?}", lines[0]);
    assert!(lines[1].contains("nope2"), "вторая: {:?}", lines[1]);
}

/// **A2.** Две ошибки в телах разных функций - две диагностики.
#[test]
fn two_function_body_errors_are_both_reported() {
    let (stderr, _) = compile(
        "fns",
        "model M {\n    fn f() -> u8 { return aaa; }\n    fn g() -> u8 { return bbb; }\n    var v: u8 := 0;\n    start S { always { v := 1; } }\n}\nstart Main = M;\n",
    );
    let lines = errors(&stderr);
    assert_eq!(lines.len(), 2, "ожидались две диагностики: {stderr:?}");
    assert!(
        lines[0].contains("aaa") && lines[1].contains("bbb"),
        "{stderr:?}"
    );
}

/// **A5.** Диагностики упорядочены **по позиции**, а не по порядку обхода.
///
/// Обход идёт по `BTreeMap` (алфавит имён), поэтому без `normalize` порядок вывода стал
/// бы наблюдаемым свойством имён состояний.
#[test]
fn diagnostics_are_ordered_by_position() {
    // Состояние `Zeta` объявлено первым, но по алфавиту идёт после `Alpha`.
    let (stderr, _) = compile(
        "order",
        "model M {\n    var ok: u8 := 0;\n    start Zeta {\n        always { ok := first + 1; }\n        ref Alpha: ok > 0;\n    }\n    state Alpha {\n        always { ok := second + 2; }\n    }\n}\nstart Main = M;\n",
    );
    let lines = errors(&stderr);
    assert_eq!(lines.len(), 2, "{stderr:?}");
    assert!(
        lines[0].contains("first") && lines[1].contains("second"),
        "порядок обязан идти по тексту, а не по алфавиту имён: {stderr:?}"
    );
}

/// **A7.** Функция, зовущая упавшую, следствия не порождает.
///
/// `g` вызывает `f`; тело `f` не разрешилось. Диагностика по `g` была бы "функция не
/// найдена" - сообщением о следствии.
#[test]
fn caller_of_failed_function_produces_no_cascade() {
    let (stderr, _) = compile(
        "chain",
        "model M {\n    fn f() -> u8 { return aaa; }\n    fn g() -> u8 { return f(); }\n    var v: u8 := 0;\n    start S { always { v := 1; } }\n}\nstart Main = M;\n",
    );
    let lines = errors(&stderr);
    assert_eq!(
        lines.len(),
        1,
        "следствие не должно попасть в вывод: {stderr:?}"
    );
    assert!(lines[0].contains("aaa"), "{stderr:?}");
}

/// **A3.** Стадии предпосылок остались терминальными.
///
/// Ошибка в объявлении типа делает следствием всякое обращение к переменной. Здесь тело
/// `x := x + 1` ссылается на неудавшееся объявление - и второй диагностики быть не
/// должно.
#[test]
fn declaration_stage_stays_terminal() {
    let (stderr, _) = compile(
        "decl",
        "model M {\n    var x: nosuchtype := 1;\n    start S {\n        always { x := x + 1; }\n    }\n}\nstart Main = M;\n",
    );
    let lines = errors(&stderr);
    assert_eq!(lines.len(), 1, "каскада быть не должно: {stderr:?}");
    assert!(lines[0].contains("SE-034"), "{stderr:?}");
}

/// **A4.** При ошибках построения артефактов нет.
///
/// Это и есть исполнение решения "неполная модель не даёт порождённого кода" - но не
/// проверкой, а тем, что неполное дерево не покидает построение.
#[test]
fn no_artifacts_when_stages_fail() {
    let (stderr, out) = compile(
        "artifacts",
        "model M {\n    var ok: u8 := 0;\n    start S { always { ok := nope1 + 1; } }\n}\nstart Main = M;\n",
    );
    assert_eq!(errors(&stderr).len(), 1, "{stderr:?}");
    let produced = std::fs::read_dir(&out)
        .map(|d| d.filter_map(Result::ok).count())
        .unwrap_or(0);
    assert_eq!(produced, 0, "порождённых файлов быть не должно: {out:?}");
}

/// Публичный вход `construct_model_with_files` по-прежнему отдаёт **одну** диагностику:
/// его контракт фича не меняет (**A6**).
#[test]
fn public_entry_still_returns_single_diagnostic() {
    use takt_lang::diagnostics::FileTable;
    use takt_lang::semantic::tree::construct_model_with_files;

    let source = "model M {\n    var ok: u8 := 0;\n    start S {\n        always { ok := nope1 + 1; }\n        ref T: ok > 0;\n    }\n    state T {\n        always { ok := nope2 + 2; }\n    }\n}\nstart Main = M;\n";
    let (ast, _) = takt_lang::parse(source, 0).expect("исходник разбирается");
    let mut files = FileTable::new("probe.takt");
    let err = construct_model_with_files(&ast, None, &[], &mut files, false)
        .expect_err("построение обязано отказать");
    // Тип - одна диагностика, а не список: контракт публичного API сохранён.
    assert!(
        err.message.contains("nope1"),
        "ожидалась самая ранняя: {err:?}"
    );
}
