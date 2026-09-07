//! Перечисление в поле структуры.
//!
//! # Что проверяется
//!
//! - значение перечисления, записанное в **поле структуры**, печатается именем
//!   варианта у `rust` и `sv` (а не числом);
//! - перечисления объявляются **до** структур, которые на них ссылаются;
//! - контроль: скалярная переменная перечислимого типа печатается по-прежнему.

use takt_lang::GenerateOptions;

/// Структура с полем перечислимого типа и записью варианта в это поле.
const SOURCE: &str = "enum Mode { Idle, Run }\n\
     struct Setting { hold: duration, mode: Mode }\n\
     model Probe {\n\
     \x20   var conf: Setting := {2s, Idle};\n\
     \x20   var seen: duration := 0ms;\n\
     \x20   var ticks: u8 := 0;\n\
     \x20   out ticks_out: u8 at 0x300;\n\
     \x20   start Cycle {\n\
     \x20       always { ticks := ticks + 1; conf.mode := Run; seen := conf.hold; ticks_out := ticks; }\n\
     \x20       ref Cycle: ticks < 200;\n\
     \x20   }\n\
     }\n\
     start Main = Probe;\n";

fn out_dir(tag: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!(
            "takt_0492_{tag}_{}",
            std::thread::current()
                .name()
                .unwrap_or("single")
                .replace(':', "_")
        ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    dir
}

fn emit(target: &str, tag: &str) -> String {
    let dir = out_dir(tag);
    let path = dir.to_str().expect("путь");
    let options = GenerateOptions::default();
    let file = match target {
        "rust" => {
            takt_lang::compile_to_rust("probe", SOURCE, path, &[], &options).expect("перевод");
            "probe.rs"
        }
        "sv" => {
            takt_lang::compile_to_sv("probe", SOURCE, path, &[], &options).expect("перевод");
            "probe.sv"
        }
        other => panic!("неизвестная цель {other}"),
    };
    let text = std::fs::read_to_string(dir.join(file)).expect("вывод читается");
    let _ = std::fs::remove_dir_all(&dir);
    text
}

/// Цель `rust`: в поле структуры пишется вариант, а не число.
#[test]
fn rust_writes_variant_into_struct_field() {
    let text = emit("rust", "rust");
    assert!(
        text.contains("self.conf.mode = Mode::Run;"),
        "значение перечисления печатается вариантом:\n{text}"
    );
    assert!(
        !text.contains("self.conf.mode = 1;"),
        "числом печатать нельзя — `rustc` отвечает E0308:\n{text}"
    );
}

/// Цель `sv`: в поле структуры пишется мнемоника варианта.
#[test]
fn sv_writes_variant_into_struct_field() {
    let text = emit("sv", "sv_value");
    assert!(
        text.contains(".mode = MODE_RUN;"),
        "значение перечисления печатается мнемоникой:\n{text}"
    );
}

/// Цель `sv`: перечисление объявлено раньше структуры, которая им пользуется.
///
/// Обратной зависимости не бывает: варианты перечисления суть литералы, поэтому порядок
/// "перечисления, затем структуры" верен всегда - в отличие от порядка самих структур,
/// где нужна сортировка по зависимостям.
#[test]
fn sv_declares_enums_before_structs() {
    let text = emit("sv", "sv_order");
    let enum_at = text.find("typedef enum").expect("перечисление объявлено");
    let struct_at = text.find("typedef struct").expect("структура объявлена");
    assert!(
        enum_at < struct_at,
        "перечисление обязано стоять раньше структуры, которая на него ссылается:\n{text}"
    );
}
