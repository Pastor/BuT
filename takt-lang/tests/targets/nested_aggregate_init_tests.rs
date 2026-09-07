//! Вложенный агрегат: инициализатор `st` и умолчание `rust`.
//!
//! # Что проверяется
//!
//! - у `st` поле-структура откладывается в первый скан **и раскрывается до
//!   листьев**: одноуровневая печать теряла бы вложенные значения молча;
//! - у `rust` `derive(Default)` выводится, когда каждое поле **имеет**
//!   `Default`, а не когда каждое поле само выводимо: структура получает
//!   `Default` всегда.

use takt_lang::GenerateOptions;

const SOURCE: &str = "enum Mode { Idle, Run }\n\
     struct Inner { mode: Mode, hold: duration }\n\
     struct Outer { head: Inner, slots: [u8; 2] }\n\
     model Probe {\n\
     \x20   var conf: Outer := {{Idle, 2s}, {1, 2}};\n\
     \x20   var seen: u8 := 0;\n\
     \x20   var ticks: u8 := 0;\n\
     \x20   out ticks_out: u8 at 0x300;\n\
     \x20   start Cycle {\n\
     \x20       always { ticks := ticks + 1; conf.head.mode := Run; seen := conf.slots[1]; ticks_out := ticks + seen; }\n\
     \x20       ref Cycle: ticks < 200;\n\
     \x20   }\n\
     }\n\
     start Main = Probe;\n";

fn emit(target: &str, tag: &str) -> String {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!(
            "takt_0496_{tag}_{}",
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
        "st" => {
            takt_lang::compile_to_st("probe", SOURCE, path, &[], &options).expect("перевод");
            "probe.st"
        }
        "rust" => {
            takt_lang::compile_to_rust("probe", SOURCE, path, &[], &options).expect("перевод");
            "probe.rs"
        }
        other => panic!("неизвестная цель {other}"),
    };
    let text = std::fs::read_to_string(dir.join(file)).expect("вывод читается");
    let _ = std::fs::remove_dir_all(&dir);
    text
}

/// Цель `st`: вложенное поле кладётся первым сканом до листьев.
///
/// Проверяются **оба** листа: одноуровневая печать положила бы поле целиком - форму,
/// которой в IEC нет, - либо не положила бы ничего, и прошивка считала бы с нулей.
#[test]
fn st_defers_nested_struct_to_first_scan() {
    let text = emit("st", "st");
    assert!(
        !text.contains("conf : Outer := ("),
        "вложенного инициализатора в объявлении быть не должно — `iec2c` его не принимает:\n{text}"
    );
    assert!(
        text.contains("conf.head.mode := Mode_Idle;") && text.contains("conf.head.hold := 2000;"),
        "значения вложенного поля кладутся первым сканом:\n{text}"
    );
    assert!(
        text.contains("conf.slots[0] := 1;"),
        "поле-массив по-прежнему кладётся первым сканом (0422):\n{text}"
    );
}

/// Цель `rust`: у структуры с полем-структурой `Default` выводится.
///
/// Контроль тут же: у самой `Inner` (поле-перечисление) `derive` невозможен, и ручной
/// `impl` остаётся - иначе `rustc` ответит, что `Default` для `Mode` не выводится.
#[test]
fn rust_derives_default_when_every_field_has_one() {
    let text = emit("rust", "rust");
    let outer = text
        .find("pub struct Outer")
        .expect("структура Outer напечатана");
    let derive_line = text[..outer]
        .rfind("#[derive")
        .expect("атрибут перед объявлением");
    assert!(
        text[derive_line..outer].contains("Default"),
        "у Outer каждое поле имеет Default — derive обязан выводиться:\n{text}"
    );
    assert!(
        text.contains("impl Default for Inner"),
        "у Inner поле-перечисление, и ручной impl остаётся:\n{text}"
    );
}
