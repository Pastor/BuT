//! Спуск по типу выражения - один носитель на семантику и цели.
//!
//! # Что доказывается
//!
//! Цепочку `переменная(.поле | [индекс])*` разбирает **общий** `validate::base_type`;
//! печатники целей своей копии не держат.
//!
//! # Что не сводится, и это замер
//!
//! Носители целей отвечают на **разные** вопросы, и свести их в один значило бы
//! утверждать обратное:
//!
//! | Вход | `st` | `rust` | `sv` |
//! |---|---|---|---|
//! | литерал `5` | `None` - иначе печатник построит `INT_TO_...` для числа | `i32` | - (возвращает q-формат, а не тип) |
//! | разряд `x.3` | `BOOL` - разряд печатается сравнением | `bool` - `bit` есть `bool` | - |
//! | сравнение `a < b` | `None` | `bool` | - |
//!
//! Общее у них - **ядро** (переменная, скобки, поле, индекс), и вынесено именно оно.

use std::path::PathBuf;
use takt_lang::generator::GenerateOptions;

/// Поле структуры q-типа: тип базы нужен всем трём целям, чтобы напечатать
/// масштабирование.
const SRC: &str = "struct Gains {\n    kp: q(8, 8),\n    ki: q(8, 8)\n}\n\
     var g: Gains := {1.5, 2.5};\nvar one: u8 := 0;\nout probe: u8 at 0;\n\
     start Run {\n    always {\n        one := g.kp as u8;\n        probe := one;\n    }\n\
     \x20   ref Run;\n}\n";

/// Поле структуры внутри массива - цепочка спуска из двух шагов.
const NESTED: &str = "struct Gains {\n    kp: q(8, 8),\n    ki: q(8, 8)\n}\n\
     var gs: [Gains; 2] := {{1.5, 2.5}, {3.5, 4.5}};\nvar one: u8 := 0;\n\
     out probe: u8 at 0;\n\
     start Run {\n    always {\n        one := gs[1].kp as u8;\n        probe := one;\n    }\n\
     \x20   ref Run;\n}\n";

fn out_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0399_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог вывода");
    dir
}

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

/// Приведение поля q-типа масштабируется у **всех** целей.
///
/// Значение проверяет потактовая сверка; здесь - что ни одна цель не потеряла знание о
/// типе базы после переезда на общий носитель.
#[test]
fn field_of_struct_is_scaled_by_every_target() {
    for (i, target) in ["c", "rust", "st", "sv"].into_iter().enumerate() {
        let text = compile(target, &format!("ot{i}"), SRC);
        assert!(
            text.contains(">> 8") || text.to_uppercase().contains("FLOORDIV"),
            "цель '{target}': приведение поля обязано масштабироваться\n{text}"
        );
    }
}

/// Цепочка из двух шагов (`gs[1].kp`) разбирается тем же правилом.
///
/// Контроль спуска: одношаговый случай прошёл бы и на прежнем, частном разборе каждой
/// цели.
#[test]
fn field_inside_array_is_scaled_too() {
    for (i, target) in ["c", "st", "sv"].into_iter().enumerate() {
        let text = compile(target, &format!("otn{i}"), NESTED);
        assert!(
            text.contains(">> 8")
                || text.contains(">>> 8")
                || text.to_uppercase().contains("FLOORDIV"),
            "цель '{target}': цепочка `массив → поле` обязана разбираться\n{text}"
        );
    }
}

/// Печатники целей своего спуска по типу не держат.
///
/// Это тест **устройства**, а не поведения, и другого тут быть не может: рефакторинг
/// вывода не меняет - прежний частный разбор давал те же значения.
/// Отличить "одно знание" от "двух совпадающих" можно только по коду; падает тест
/// **списком** мест (образец 0291, 0203).
#[test]
fn targets_do_not_reimplement_the_descent() {
    let mut offenders = Vec::new();
    for (path, needle) in [
        (
            "src/generator/sv/sv_fixed.rs",
            "ExpressionNode::ArraySubscript(inner, _) => {",
        ),
        ("src/generator/st/st_operand_type.rs", "search_struct"),
    ] {
        let text = std::fs::read_to_string(path).unwrap_or_default();
        if text.contains(needle) {
            offenders.push(format!("{path}: спуск по типу написан заново ({needle})"));
        }
    }
    assert!(
        offenders.is_empty(),
        "цепочку `переменная(.поле | [индекс])*` разбирает общий носитель \
         `validate::base_type` (фича 0399):\n{}",
        offenders.join("\n")
    );
}
