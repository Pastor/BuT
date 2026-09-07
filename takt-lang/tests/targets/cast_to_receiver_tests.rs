//! Явное приведение не отменяет приведения к приёмнику.
//!
//! # Что проверяется
//!
//! Значение печатается приведённым **дважды**: к типу, названному автором, и к типу
//! приёмника. Контроль: когда типы совпадают, второго приведения нет - лишнее `as`
//! отвергает `clippy` (`unnecessary_cast`).

use takt_lang::GenerateOptions;

/// Приведение шире приёмника: `u16 as u32` в порт `u8`.
const WIDER: &str = "model Probe {\n\
     \x20   var wide: u16 := 300;\n\
     \x20   var ticks: u8 := 0;\n\
     \x20   out out_value: u8 at 0x300;\n\
     \x20   start Cycle {\n\
     \x20       always { ticks := ticks + 1; out_value := wide as u32; }\n\
     \x20       ref Cycle: ticks < 200;\n\
     \x20   }\n\
     }\n\
     start Main = Probe;\n";

/// Контроль: приведение В тип приёмника - второго приведения быть не должно.
const EXACT: &str = "model Probe {\n\
     \x20   var wide: u16 := 300;\n\
     \x20   var ticks: u8 := 0;\n\
     \x20   out out_value: u8 at 0x300;\n\
     \x20   start Cycle {\n\
     \x20       always { ticks := ticks + 1; out_value := wide as u8; }\n\
     \x20       ref Cycle: ticks < 200;\n\
     \x20   }\n\
     }\n\
     start Main = Probe;\n";

fn emit(target: &str, source: &str, tag: &str) -> String {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!(
            "takt_0495_{tag}_{}",
            std::thread::current()
                .name()
                .unwrap_or("single")
                .replace(':', "_")
        ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    let path = dir.to_str().expect("путь");
    let options = GenerateOptions::default();
    let file = match target {
        "rust" => {
            takt_lang::compile_to_rust("probe", source, path, &[], &options).expect("перевод");
            "probe.rs"
        }
        "st" => {
            takt_lang::compile_to_st("probe", source, path, &[], &options).expect("перевод");
            "probe.st"
        }
        "sv" => {
            takt_lang::compile_to_sv("probe", source, path, &[], &options).expect("перевод");
            "probe.sv"
        }
        other => panic!("неизвестная цель {other}"),
    };
    let text = std::fs::read_to_string(dir.join(file)).expect("вывод читается");
    let _ = std::fs::remove_dir_all(&dir);
    text
}

/// Цель `rust`: приведение автора обёрнуто приведением к приёмнику.
#[test]
fn rust_casts_to_receiver_after_author_cast() {
    let text = emit("rust", WIDER, "rust_wider");
    assert!(
        text.contains("(self.wide as u32) as u8"),
        "значение обязано быть приведено к типу приёмника:\n{text}"
    );
}

/// Цель `st`: то же - вложенной функцией преобразования IEC.
#[test]
fn st_casts_to_receiver_after_author_cast() {
    let text = emit("st", WIDER, "st_wider");
    assert!(
        text.contains("UDINT_TO_USINT(UINT_TO_UDINT(wide))"),
        "приведение к приёмнику обязано обернуть приведение автора:\n{text}"
    );
}

/// Цель `sv`: размерная форма по ширине приёмника.
#[test]
fn sv_casts_to_receiver_after_author_cast() {
    let text = emit("sv", WIDER, "sv_wider");
    assert!(
        text.contains("8'(32'("),
        "значение обязано быть сужено до ширины приёмника:\n{text}"
    );
}

/// **Контроль:** приведение в тип приёмника второго приведения не получает.
///
/// Без контроля правило читалось бы как "оборачивать всегда", а лишнее `as` у `rust`
/// отвергает `clippy::unnecessary_cast` под `-D warnings`.
#[test]
fn exact_cast_is_not_wrapped_twice() {
    let text = emit("rust", EXACT, "rust_exact");
    assert!(
        !text.contains("as u8) as u8"),
        "второго приведения быть не должно:\n{text}"
    );
}
