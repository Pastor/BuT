//! Сдвиг на величину, не меньшую ширины продвинутого типа, в цели `c`.
//!
//! # Порог принадлежит целевому языку
//!
//! В C операнды сдвига **продвигаются** до `int` (C11 6.5.7p3), поэтому `u8 >> 8`
//! определено и совпадает с эталоном; UB начинается с ширины продвинутого типа. Отсюда
//! порог `max(32, W)` - и оттого вывод корпуса не меняется: сдвигов такой величины в
//! `examples/` нет ни одного.
//!
//! Тесты смотрят на **текст вывода и на прогон настоящего `cc`** с флагами проверки:
//! класс, ради которого фича заведена, - именно отказ чужого инструмента при рапорте об
//! успехе.

use std::path::PathBuf;
use std::process::Command;
use takt_lang::generator::GenerateOptions;

/// Беззнаковый и знаковый сдвиг на ширину продвинутого типа в одной модели.
const SRC: &str = "var w: u32 := 200;\nvar s: i32 := -8;\nvar o: u32 := 1;\nvar q: i32 := 1;\n\
     out probe: u32 at 0;\n\
     start Run { always { o := w >> 32; q := s >> 32; probe := o; } ref Run: o = 0; }\n";

fn out_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_cshift_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог вывода");
    dir
}

/// Порождает C и возвращает пару "каталог, текст `.c`".
fn generate(tag: &str, src: &str) -> (PathBuf, String) {
    let dir = out_dir(tag);
    takt_lang::compile_to_c(
        tag,
        src,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение C");
    let text = std::fs::read_to_string(dir.join(format!("{tag}.c"))).expect("чтение вывода");
    (dir, text)
}

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Предмет: сдвиг на ширину продвинутого типа печатается выразимой формой.
///
/// Беззнаковый - `0` (все разряды ушли); знаковый - сдвиг на `W − 1`, там остаётся
/// только знак, то есть −1 либо 0 - ровно то, что даёт эталон.
#[test]
fn shift_beyond_promoted_width_is_saturated() {
    let (_, text) = generate("cshiftw", SRC);
    assert!(
        text.contains("model->o = 0;"),
        "беззнаковый сдвиг обязан давать 0:\n{text}"
    );
    assert!(
        text.contains("(model->s >> 31)"),
        "знаковый сдвиг обязан насыщаться до ширины−1:\n{text}"
    );
    assert!(
        !text.contains(">> 32"),
        "сдвиг на всю ширину `cc` отвергает — его в выводе быть не должно:\n{text}"
    );
}

/// **Контроль:** сдвиг на законную величину печатается без хелпера.
///
/// Без него правило читалось бы как "сдвиги переписываются всегда", и вывод корпуса
/// поехал бы молча.
#[test]
fn ordinary_shift_is_unchanged() {
    let src = SRC.replace(">> 32", ">> 3");
    let (_, text) = generate("cshiftn", &src);
    assert!(text.contains("model->w >> 3"), "{text}");
    assert!(text.contains("model->s >> 3"), "{text}");
}

/// **Контроль порога:** `u8 >> 8` в C определено (продвижение до `int`) и
/// совпадает с эталоном - трогать его нельзя.
///
/// Это отличие цели `c` от цели `rust`, где та же запись насыщается: порог принадлежит
/// целевому языку, а не правилу.
#[test]
fn shift_within_promoted_width_is_left_alone() {
    let src = "var b: u8 := 200;\nvar c8: u8 := 1;\nout probe: u8 at 0;\n\
         start Run { always { c8 := b >> 8; probe := c8; } ref Run: c8 = 0; }\n";
    let (_, text) = generate("cshiftp", src);
    assert!(
        text.contains("model->b >> 8"),
        "сдвиг в пределах продвинутого типа обязан печататься как есть:\n{text}"
    );
}

/// Величина сдвига **переменная** - считается хелпером.
///
/// Названная граница "печатается как есть" здесь неверна: она была бы
/// **дефектом, закреплённым тестом**: замер 2026-08-23 показал, что прошивка на `n =
/// 32` давала 4294967295 вместо 0 - молча, при зелёном `cc -Werror`. Утверждение снято
/// вместе с границей.
///
/// Хелпер, а не тернарный оператор: тот печатал бы величину дважды, а операнд в Takt
/// бывает с эффектом.
#[test]
fn variable_shift_amount_goes_through_helper() {
    let src = "var a: u32 := 200;\nvar n: u32 := 1;\nvar v: u32 := 0;\n\
         out probe: u32 at 0;\n\
         start Run { always { v := a >> n; probe := v; } ref Run: v < 100; }\n";
    let (_, text) = generate("cshiftv", src);
    assert!(
        text.contains("takt_shr_u((uint64_t)(model->a), (uint64_t)(model->n), 32)"),
        "переменная величина обязана идти через хелпер:\n{text}"
    );
    assert!(
        text.contains("static uint64_t takt_shr_u("),
        "определение хелпера эмитится по факту вызова:\n{text}"
    );
    // Контроль: операнды печатаются по одному разу - ради этого хелпер и заведён.
    assert_eq!(
        text.matches("model->n").count(),
        text.matches("model->a").count(),
        "ни один операнд не должен печататься дважды:\n{text}"
    );
}

/// Порождённый C собирается **флагами проверки цели** - тем самым, что отвергал прежний
/// вывод.
#[test]
fn generated_c_compiles_under_gate_flags() {
    if !cc_available() {
        eprintln!("[ПРОПУСК] компилятор `cc` не найден; текст вывода уже проверен");
        return;
    }
    let (dir, _) = generate("cshiftcc", SRC);
    let out = Command::new("cc")
        .args(["-Wall", "-Wextra", "-Wno-unused-parameter", "-Werror", "-c"])
        .arg("-I")
        .arg(&dir)
        .arg(dir.join("cshiftcc.c"))
        .arg("-o")
        .arg(dir.join("cshiftcc.o"))
        .output()
        .expect("запуск cc");
    assert!(
        out.status.success(),
        "порождённый C обязан собираться флагами гейта цели:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
}
