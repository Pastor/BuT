//! Частично прочитанный параметр-структура - цель `sv`.
//!
//! # Гранулярность правила измерена
//!
//! | Вход | `verilator` |
//! |---|---|
//! | читается `v.b`, поле `a` - нет | **отвергает** |
//! | вложенная структура, читается `v.tail.y`, поле `head` - нет | **отвергает** |
//! | вложенная, не читается лишь `tail.x` | принимает |
//! | читаются оба поля | принимает |
//!
//! То есть инструмент судит по полям верхнего уровня, и поглотитель печатается ровно по
//! этой границе: гасить лишнее нельзя ( - поглотитель на весь сигнал скрыл бы и
//! настоящий дефект печати).
//!
//! # Что проверяется
//!
//! - поглотитель на непрочитанное поле и его отсутствие у прочитанного;
//! - параметр, упомянутый как целое, поглотителя не получает;
//! - `verilator` принимает вывод - главный тест класса.

use std::path::PathBuf;
use std::process::Command;

use takt_lang::generator::GenerateOptions;

/// Читается только `v.b` - поле `a` обязано быть поглощено.
const PARTIAL: &str = "struct Inner { a: u8, b: u8 }\nout sum: u8 at 0x2000;\n\
     var src: Inner := {1, 2};\nvar ticks: u8 := 0;\n\
     fn pick(v: Inner) -> u8 { return v.b; }\n\
     start Run { always { ticks := ticks + 1; sum := pick(src) + ticks; } ref Run; }\n";

/// **Контроль:** читаются оба поля - поглощать нечего.
const FULL: &str = "struct Inner { a: u8, b: u8 }\nout sum: u8 at 0x2000;\n\
     var src: Inner := {1, 2};\nvar ticks: u8 := 0;\n\
     fn pick(v: Inner) -> u8 { return v.a + v.b; }\n\
     start Run { always { ticks := ticks + 1; sum := pick(src) + ticks; } ref Run; }\n";

/// **Контроль:** параметр упомянут как целое - читаются все разряды.
const WHOLE: &str = "struct Inner { a: u8, b: u8 }\nout sum: u8 at 0x2000;\n\
     var src: Inner := {1, 2};\nvar ticks: u8 := 0;\n\
     fn same(v: Inner) -> Inner { return v; }\n\
     start Run { always { ticks := ticks + 1; sum := same(src).b + ticks; } ref Run; }\n";

fn out_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0506_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог вывода");
    dir
}

fn generate(tag: &str, source: &str) -> (PathBuf, String) {
    let dir = out_dir(tag);
    takt_lang::compile_to_sv(
        "probe",
        source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .unwrap_or_else(|d| panic!("порождение SystemVerilog ({tag}): {}", d.message));
    let text = std::fs::read_to_string(dir.join("probe.sv")).expect("чтение вывода");
    (dir, text)
}

fn verilator() -> bool {
    Command::new("verilator")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Непрочитанное поле поглощается, прочитанное - нет.
#[test]
fn unread_field_is_absorbed() {
    let (dir, text) = generate("partial", PARTIAL);
    assert!(
        text.contains("logic _unused_v_a;") && text.contains("_unused_v_a = &{1'b0, v.a};"),
        "непрочитанное поле обязано быть поглощено:\n{text}"
    );
    assert!(
        !text.contains("_unused_v_b"),
        "прочитанное поле поглощать нечем — иначе гаснут честные предупреждения:\n{text}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// **Контроль:** оба поля прочитаны - поглотителя нет.
#[test]
fn fully_read_parameter_has_no_sink() {
    let (dir, text) = generate("full", FULL);
    assert!(
        !text.contains("_unused_v"),
        "поглотитель печатается ПО НУЖДЕ:\n{text}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// **Контроль:** параметр, упомянутый как целое, поглотителя не получает.
///
/// Без него правка читалась бы как "поглощать всё, что не упомянуто с точкой", и
/// `return v;` получил бы поглотитель на каждое поле.
#[test]
fn whole_value_parameter_has_no_sink() {
    let (dir, text) = generate("whole", WHOLE);
    assert!(
        !text.contains("_unused_v"),
        "имя, прочитанное целиком, поглощать нечем:\n{text}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// `verilator` принимает вывод - главный тест класса.
#[test]
fn generated_sv_passes_the_gate_tool() {
    if !verilator() {
        eprintln!("[ПРОПУСК] verilator не найден; форма вывода проверена отдельно");
        return;
    }
    for (tag, source) in [("lint_partial", PARTIAL), ("lint_full", FULL)] {
        let (dir, _) = generate(tag, source);
        let out = Command::new("verilator")
            .args(["--lint-only", "-Wall"])
            .arg(dir.join("probe.sv"))
            .output()
            .expect("запуск verilator");
        assert!(
            out.status.success(),
            "verilator обязан принять вывод ({tag}):\n{}",
            String::from_utf8_lossy(&out.stderr)
        );
        let _ = std::fs::remove_dir_all(&dir);
    }
}
