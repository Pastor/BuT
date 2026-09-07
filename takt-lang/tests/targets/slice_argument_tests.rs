//! Срез массива в аргументе вызова - разворот в семантике.
//!
//! # Что доказывается
//!
//! Форма переводится **всеми** целями, и печатники их не знают о ней вовсе: разворот
//! делает семантика, а за её границей среза в аргументе не существует (приём
//! о ней не знают.
//!
//!
//! **Разворот возможен только после трёх смежных исправлений**, каждое из которых нашлось
//! прогоном инструментов целей, а не чтением кода: локальный массив в аргументе у
//! `st`, лишний `mut` и `needless_late_init` у `rust`, `E0381` на
//! отложенном массиве у `rust`).
//!
//! Значения сверяет `takt-sim` (`conformance_slice_argument_tests`): ошибка в границах
//! даёт валидный вывод с другим числом.

use std::path::PathBuf;
use takt_lang::generator::GenerateOptions;

const SRC: &str = "fn first(a: [u8; 2]) -> u8 {\n    return a[0];\n}\n\
     var src: [u8; 4] := {5, 6, 7, 8};\nvar o: u8 := 0;\nout probe: u8 at 0;\n\
     start Run {\n    always {\n        o := first(src[1:3]);\n        probe := o;\n    }\n\
     \x20   ref Run;\n}\n";

fn out_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0400_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог вывода");
    dir
}

/// Порождает код цели и возвращает его текст.
fn compile(target: &str, tag: &str, src: &str) -> String {
    let dir = out_dir(tag);
    let out = dir.to_str().expect("путь в UTF-8");
    let opts = GenerateOptions::default();
    let result = match target {
        "c" => takt_lang::compile_to_c(tag, src, out, &[], &opts),
        "rust" => takt_lang::compile_to_rust(tag, src, out, &[], &opts),
        "st" => takt_lang::compile_to_st(tag, src, out, &[], &opts),
        "sv" => takt_lang::compile_to_sv(tag, src, out, &[], &opts),
        other => panic!("цель '{other}' в этом тесте не предусмотрена"),
    };
    result.unwrap_or_else(|e| panic!("цель '{target}' отказала: {e:?}"));
    std::fs::read_dir(&dir)
        .expect("чтение каталога")
        .filter_map(|e| e.ok())
        .filter(|e| e.path().is_file())
        .map(|e| std::fs::read_to_string(e.path()).unwrap_or_default())
        .collect::<Vec<_>>()
        .join("\n")
}

/// Предмет: форму переводят все четыре цели, у которых есть тело.
#[test]
fn slice_argument_is_translated_by_every_target() {
    for (i, target) in ["c", "rust", "st", "sv"].into_iter().enumerate() {
        let text = compile(target, &format!("sa{i}"), SRC);
        assert!(
            text.contains("takt_slice_"),
            "цель '{target}': разворот обязан завести временную переменную\n{text}"
        );
    }
}

/// Имя временной переменной - допустимый идентификатор целевых языков.
///
/// Имя вида `#slice1` не годится: `cc` отвечает "expected identifier", а `iec2c` -
/// "invalid variable(s) declaration". Имя, невыразимое в цели, превращает правку в
/// новый отказ.
#[test]
fn temporary_name_is_a_valid_identifier() {
    let text = compile("c", "saname", SRC);
    assert!(
        !text.contains('#') || !text.contains("#slice"),
        "имя временной обязано быть идентификатором цели:\n{text}"
    );
}

/// **Контроль:** срез в присваивании печатается поэлементно.
///
/// Без него правка читалась бы как "срез всегда выносится во временную".
#[test]
fn slice_assignment_is_unchanged() {
    let src = "var src: [u8; 4] := {5, 6, 7, 8};\nvar part: [u8; 2] := {0, 0};\n\
         var o: u8 := 0;\nout probe: u8 at 0;\n\
         start Run {\n    always {\n        part := src[1:3];\n        o := part[0];\n\
         \x20       probe := o;\n    }\n    ref Run;\n}\n";
    let text = compile("c", "saplain", src);
    assert!(
        !text.contains("takt_slice_"),
        "присваивание среза временной переменной не требует:\n{text}"
    );
}

/// Имя временной **не сталкивается** с написанным автором.
///
/// Молчаливое затенение чужого имени - тот же класс, что `SE-086` у специализации:
/// занятое имя обязано быть замечено.
#[test]
fn temporary_name_avoids_the_authors_name() {
    let src = "fn first(a: [u8; 2]) -> u8 {\n    return a[0];\n}\n\
         var src: [u8; 4] := {5, 6, 7, 8};\nvar takt_slice_1: u8 := 42;\nvar o: u8 := 0;\n\
         out probe: u8 at 0;\n\
         start Run {\n    always {\n        o := first(src[1:3]);\n\
         \x20       probe := o + takt_slice_1;\n    }\n    ref Run;\n}\n";
    let text = compile("c", "sataken", src);
    assert!(
        text.contains("takt_slice_2"),
        "занятое имя обязано быть пропущено:\n{text}"
    );
}
