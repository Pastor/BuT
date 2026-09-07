//! Сдвиг на величину, не меньшую ширины типа, в цели `rust`.

use std::path::PathBuf;
use takt_lang::generator::GenerateOptions;

const SRC: &str = "var a: i8 := -8;\nvar b: u8 := 200;\nvar v: i8 := 0;\nvar w: u8 := 0;\n\
     out probe: i8 at 0;\n\
     start Run { always { v := a >> 8; w := b >> 8; probe := v; } ref Run: w < 100; }\n";

fn out_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_shift_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог вывода");
    dir
}

fn generate(tag: &str, src: &str) -> String {
    let dir = out_dir(tag);
    takt_lang::compile_to_rust(
        tag,
        src,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение Rust");
    std::fs::read_to_string(dir.join(format!("{tag}.rs"))).expect("чтение вывода")
}

/// Предмет: сдвиг на ширину типа печатается выразимой формой.
///
/// Знаковый - сдвиг на `ширина − 1` (там остаётся только знак, то есть −1 либо 0);
/// беззнаковый - `0`.
#[test]
fn shift_by_type_width_is_printed_as_saturated() {
    let text = generate("shiftw", SRC);
    assert!(
        text.contains(">> 7"),
        "знаковый сдвиг обязан насыщаться до ширины−1:\n{text}"
    );
    assert!(
        !text.contains(">> 8"),
        "сдвиг на всю ширину rustc отвергает — его в выводе быть не должно:\n{text}"
    );
}

/// **Контроль:** обычный сдвиг печатается без насыщения.
///
/// Без него правило читалось бы как "сдвиги переписываются всегда".
#[test]
fn ordinary_shift_is_unchanged() {
    let src = SRC.replace(">> 8", ">> 1");
    let text = generate("shiftn", &src);
    assert!(text.contains(">> 1"), "{text}");
}

/// **Переменная величина насыщается тоже**.
///
/// Обратная проверка "переменная величина печатается как есть"
/// и она **закрепляла бы дефект**: такой вывод паникует в отладке
/// и маскирует величину в релизе (`n & 7`), давая `200` вместо `0`.
#[test]
fn variable_shift_amount_saturates() {
    let src = "var a: i8 := -8;\nvar n: u8 := 1;\nvar v: i8 := 0;\n\
         out probe: i8 at 0;\n\
         start Run { always { v := a >> n; probe := v; } ref Run: v < 100; }\n";
    let text = generate("shiftv", src);
    assert!(
        text.contains(">> ((self.n) as u32).min(7)"),
        "знаковый сдвиг вправо на переменную величину обязан насыщаться:\n{text}"
    );
}

/// **Сдвиг влево** насыщается в обе формы величины.
///
/// Литеральная форма без насыщения не собирается вовсе: `rustc` отвечает "attempt to shift
/// left by `8_i32`, which would overflow" при **нулевом** коде возврата `taktc`.
#[test]
fn left_shift_saturates_in_both_forms() {
    let src = "var a: u8 := 3;\nvar n: u8 := 8;\nvar lit: u8 := 0;\nvar vary: u8 := 0;\n\
         out p1: u8 at 0;\nout p2: u8 at 1;\n\
         start Run { always { lit := a << 8; vary := a << n; p1 := lit; p2 := vary; } \
         ref Run: lit = 0; }\n";
    let text = generate("shiftl", src);
    assert!(
        text.contains("self.lit = 0;"),
        "литеральная величина влево обязана давать 0:\n{text}"
    );
    assert!(
        text.contains("checked_shl((self.n) as u32).unwrap_or(0)"),
        "переменная величина влево обязана насыщаться:\n{text}"
    );
    assert!(
        !text.contains("<< 8"),
        "сдвиг на всю ширину rustc отвергает — его в выводе быть не должно:\n{text}"
    );
}
