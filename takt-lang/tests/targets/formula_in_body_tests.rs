//! Охранная формула В теле доезжает до целей.
//!
//! # Что тестытся
//!
//! постановила: охранная формула - это `assert` у целей `c`, `rust` и `sv` и
//! предупреждение `ST-022` у `st`. Правило соблюдалось для формулы, объявленной
//! Элементом (уровень модели или состояния), и не соблюдалось для формулы-Оператора:
//! цель `rust` печатала предупреждение "LTL-формула в теле блока не транслируется"
//! (`RS-010`), а `sv` теряла её молча.
//!
//!
//! | Цель | было | стало |
//! |---|---|---|
//! | `c` | 6 `assert` | 6 |
//! | `rust` | 2 `assert!` + 4 ложных `RS-010` | 6 `assert!` |
//! | `sv` | 2 `assert` (четыре потеряны молча) | 6 |
//! | `st` | 6 `ST-022` + 3 ложных `ST-010` | 6 `ST-022` |
//!
//! Тест считает `assert` в выводе, а не отсутствие предупреждения: именно потеря
//! проверки - предмет фичи. Текст предупреждения проверяется отдельно.

use std::path::PathBuf;

/// Модель с охранной формулой в теле блока и в теле состояния.
const SOURCE: &str = "out o: u8 at 0;\n\
                      var level: u8 := 0;\n\
                      \n\
                      start Run {\n\
                      \x20   : [Guard] level < 9;\n\
                      \x20   always {\n\
                      \x20       : [Guard] level < 9;\n\
                      \x20       level := level + 1;\n\
                      \x20       o := level;\n\
                      \x20   }\n\
                      \x20   ref Run: level < 3;\n\
                      }\n";

/// Уникальный по тесту каталог.
fn work_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("main")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0472_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог теста");
    dir
}

fn options() -> takt_lang::generator::GenerateOptions {
    takt_lang::generator::GenerateOptions::default()
}

/// Обе формулы - и элемента, и тела - печатаются целью `c`.
#[test]
fn target_c_prints_both_guards() {
    let dir = work_dir("c");
    takt_lang::compile_to_c("probe", SOURCE, dir.to_str().unwrap(), &[], &options())
        .expect("цель `c` переводит эту модель");
    let text = std::fs::read_to_string(dir.join("probe.c")).expect("вывод цели `c`");
    let count = text.matches("assert(model->level < 9)").count();
    assert_eq!(
        count, 2,
        "обе охранные формулы обязаны стать `assert`:\n{text}"
    );
}

/// То же у цели `rust`: прежде формула тела давала ложное `RS-010`.
#[test]
fn target_rust_prints_both_guards() {
    let dir = work_dir("rust");
    let warnings =
        takt_lang::compile_to_rust("probe", SOURCE, dir.to_str().unwrap(), &[], &options())
            .expect("цель `rust` переводит эту модель");
    let text = std::fs::read_to_string(dir.join("probe.rs")).expect("вывод цели `rust`");
    assert_eq!(
        text.matches("assert!(self.level < 9)").count(),
        2,
        "обе охранные формулы обязаны стать `assert!`:\n{text}"
    );
    assert!(
        !warnings.iter().any(|d| d.code.as_deref() == Some("RS-010")),
        "об охранной формуле нельзя сообщать как о темпоральной: {warnings:?}"
    );
}

/// То же у цели `sv`: прежде формула тела терялась молча.
#[test]
fn target_sv_prints_both_guards() {
    let dir = work_dir("sv");
    takt_lang::compile_to_sv("probe", SOURCE, dir.to_str().unwrap(), &[], &options())
        .expect("цель `sv` переводит эту модель");
    let text = std::fs::read_to_string(dir.join("probe.sv")).expect("вывод цели `sv`");
    assert_eq!(
        text.matches("assert (").count(),
        2,
        "обе охранные формулы обязаны стать `assert`:\n{text}"
    );
}

/// Цель `st` сообщает о каждой формуле кодом `ST-022`, и только им.
///
/// `ST-010` адресован темпоральной формуле: печатать его на охранную - значит называть
/// LTL там, где её нет.
#[test]
fn target_st_reports_guards_as_st022_only() {
    let dir = work_dir("st");
    let warnings =
        takt_lang::compile_to_st("probe", SOURCE, dir.to_str().unwrap(), &[], &options())
            .expect("цель `st` переводит эту модель");
    let st022 = warnings
        .iter()
        .filter(|d| d.code.as_deref() == Some("ST-022"))
        .count();
    assert_eq!(
        st022, 2,
        "обе формулы обязаны получить `ST-022`: {warnings:?}"
    );
    assert!(
        !warnings.iter().any(|d| d.code.as_deref() == Some("ST-010")),
        "об охранной формуле нельзя сообщать как о темпоральной: {warnings:?}"
    );
}

/// Флаг `--guard-disable` снимает проверки у всех трёх целей.
///
/// Без этой проверки правка "печатать формулу тела всегда" прошла бы мимо флага: у
/// формулы-элемента он спрашивается картой, а тела о нём не знали.
#[test]
fn guard_disable_removes_body_guards() {
    let mut options = options();
    options.guard_enable = false;
    for (target, needle) in [("c", "assert(model->level < 9)"), ("sv", "assert (")] {
        let dir = work_dir(&format!("off_{target}"));
        let path = dir.to_str().unwrap();
        let file = match target {
            "c" => {
                takt_lang::compile_to_c("probe", SOURCE, path, &[], &options).expect("цель `c`");
                dir.join("probe.c")
            }
            _ => {
                takt_lang::compile_to_sv("probe", SOURCE, path, &[], &options).expect("цель `sv`");
                dir.join("probe.sv")
            }
        };
        let text = std::fs::read_to_string(&file).expect("вывод цели");
        assert!(
            !text.contains(needle),
            "под `--guard-disable` проверок быть не должно ({target}):\n{text}"
        );
    }
}

/// **Имя инварианта доезжает до сообщения цели `rust`.**
///
/// У цели `c` имя не печатается, и это решение, а не пробел: `assert()` в C показывает
/// само условие, а строка сообщения потребовала бы своей формы вывода. Проверяются обе
/// стороны - иначе "имя пропало у `rust`" и "имя появилось у `c`" отличить было бы
/// нечем.
#[test]
fn invariant_name_reaches_rust_message_only() {
    const NAMED: &str = "out o: u8 at 0;\n\
                         var level: u8 := 0;\n\
                         \n\
                         start Run {\n\
                         \x20   invariant NamedHere = level < 9;\n\
                         \x20   always {\n\
                         \x20       level := level + 1;\n\
                         \x20       o := level;\n\
                         \x20   }\n\
                         \x20   ref Run: level < 3;\n\
                         }\n";
    let dir = work_dir("named_rust");
    takt_lang::compile_to_rust("probe", NAMED, dir.to_str().unwrap(), &[], &options())
        .expect("цель `rust` переводит эту модель");
    let rust = std::fs::read_to_string(dir.join("probe.rs")).expect("вывод цели `rust`");
    assert!(
        rust.contains("нарушен инвариант 'NamedHere'"),
        "имя обязательства обязано доехать до сообщения:\n{rust}"
    );

    let dir = work_dir("named_c");
    takt_lang::compile_to_c("probe", NAMED, dir.to_str().unwrap(), &[], &options())
        .expect("цель `c` переводит эту модель");
    let c = std::fs::read_to_string(dir.join("probe.c")).expect("вывод цели `c`");
    assert!(
        c.contains("assert(model->level < 9)"),
        "у цели `c` печатается само условие:\n{c}"
    );
    assert!(
        !c.contains("NamedHere"),
        "имя обязательства у цели `c` не печатается — решение 0044:\n{c}"
    );
}
