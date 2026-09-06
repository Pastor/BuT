//! Арифметика печатается в типе приёмника.

use std::process::Command;
use takt_lang::generator::GenerateOptions;

/// Однознаковое расширение: `u8 + u8` в `u16`.
const WIDEN: &str = "var a: u8 := 200; var b: u8 := 100; var r: u16 := 0; \
                     out o: u8 at 0x100; out rr: u16 at 0x104; \
                     start Run { always { r := a + b; o := 1; rr := r; } \
                     ref Done: r > 0; } state Done { }";

/// Смешанная знаковость: `i8 + u8` в `i16`.
const MIXED: &str = "var s: i8 := -1; var u: u8 := 200; var r: i16 := 0; \
                     out o: u8 at 0x100; out rr: u16 at 0x104; \
                     start Run { always { r := s + u; o := 1; rr := r; } \
                     ref Done: r > 0; } state Done { }";

/// Накопление: Левый операнд уже типа приёмника, правый - уже.
const KEEP: &str = "var total: u16 := 0; var step: u8 := 3; \
                    out o: u16 at 0x100; \
                    start Run { always { total := total + step; o := total; } \
                    ref Done: total > 60000; } state Done { }";

/// **Контрпример:** операнды того же типа, что приёмник, - печать прежняя.
const SAME: &str = "var a: u8 := 1; var b: u8 := 2; var r: u8 := 0; \
                    out o: u8 at 0x100; \
                    start Run { always { r := a + b; o := r; } ref Done: r > 0; } \
                    state Done { }";

fn generate(tag: &str, target: &str, source: &str) -> (std::path::PathBuf, String) {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0360_{tag}_{target}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    let path = dir.to_str().expect("путь");
    let opts = GenerateOptions::default();
    match target {
        "c" => takt_lang::compile_to_c("probe", source, path, &[], &opts).map(|_| ()),
        "st" => takt_lang::compile_to_st("probe", source, path, &[], &opts).map(|_| ()),
        "rust" => takt_lang::compile_to_rust("probe", source, path, &[], &opts).map(|_| ()),
        _ => takt_lang::compile_to_sv("probe", source, path, &[], &opts).map(|_| ()),
    }
    .unwrap_or_else(|e| panic!("порождение для '{target}': {e:?}"));
    let ext = match target {
        "c" => "c",
        "st" => "st",
        "rust" => "rs",
        _ => "sv",
    };
    let text = std::fs::read_to_string(dir.join(format!("probe.{ext}"))).expect("чтение");
    (dir, text)
}

/// Однознаковое расширение: операнды приводятся к типу приёмника.
#[test]
fn widening_casts_operands() {
    let (_d, rust) = generate("widen", "rust", WIDEN);
    assert!(
        rust.contains("(self.a as u16).wrapping_add(self.b as u16)"),
        "цель `rust`: `u8 + u8` не присвоить `u16` — нужен приведённый операнд.\n{rust}"
    );
    let (_d, st) = generate("widen", "st", WIDEN);
    assert!(
        st.contains("USINT_TO_UINT"),
        "цель `st`: `iec2c` отвергает присваивание разных типов.\n{st}"
    );
    let (_d, sv) = generate("widen", "sv", WIDEN);
    assert!(
        sv.contains("16'("),
        "цель `sv`: verilator отвечает WIDTHEXPAND, а гейт считает это ошибкой.\n{sv}"
    );
}

/// Смешанная знаковость: расширение идёт со знаком.
#[test]
fn mixed_sign_widening_keeps_sign() {
    let (_d, rust) = generate("mixed", "rust", MIXED);
    assert!(
        rust.contains("as i16"),
        "приёмник знаковый — операнды приводятся к знаковому.\n{rust}"
    );
    let (_d, st) = generate("mixed", "st", MIXED);
    assert!(
        st.contains("SINT_TO_INT") && st.contains("USINT_TO_INT"),
        "оба операнда приводятся к типу приёмника.\n{st}"
    );
    let (_d, sv) = generate("mixed", "sv", MIXED);
    assert!(
        sv.contains("$signed("),
        "цель `sv`: знаковая ширина требует `$signed`, иначе отрицательное \
         станет большим положительным (урок 0323).\n{sv}"
    );
}

/// **Контрпример:** одинаковые типы - вывод прежний.
///
/// Без этой проверки правка добавила бы приведения всюду и изменила вывод всего
/// корпуса.
#[test]
fn same_types_are_untouched() {
    let (_d, rust) = generate("same", "rust", SAME);
    assert!(
        !rust.contains("as u8)"),
        "приведений быть не должно: операнды уже нужного типа.\n{rust}"
    );
    let (_d, st) = generate("same", "st", SAME);
    assert!(!st.contains("USINT_TO_"), "то же у цели `st`.\n{st}");
}

/// **Контрпример:** цель `c` не трогается - продвижение до `int` уже верно.
#[test]
fn c_is_untouched() {
    let (_d, c) = generate("widen", "c", WIDEN);
    assert!(
        c.contains("model->a + model->b"),
        "у цели `c` печать прежняя: иначе изменился бы вывод корпуса.\n{c}"
    );
}

/// Беззнаковая арифметика с расширением сохраняет обёртку.
///
/// Компиляция обе формы принимает - расхождение видно только значением.
#[test]
fn unsigned_widening_keeps_wrapping() {
    let (_d, rust) = generate("widen", "rust", WIDEN);
    assert!(
        rust.contains(".wrapping_add(") && !rust.contains("as u16) + ("),
        "операцию печатает печатник арифметики, а он ставит обёртку.\n{rust}"
    );
}

/// Приведение операнда печатается по нужде.
///
/// `total := total + step;` при `total: u16` и `step: u8` давало `(self.total as u16) +
/// (self.step as u16)` - приведение `u16` -> `u16` есть `clippy::unnecessary_cast`, то
/// есть отказ проверки самой цели при нулевом коде возврата `taktc`.
#[test]
fn operand_of_receiver_type_keeps_no_cast() {
    let (_d, rust) = generate("keep", "rust", KEEP);
    assert!(
        rust.contains("self.total.wrapping_add(self.step as u16)"),
        "левый операнд уже `u16` — приведения ему не нужно, правому нужно.\n{rust}"
    );
    assert!(
        !rust.contains("self.total as u16"),
        "лишнее приведение отвергает `clippy -D warnings`.\n{rust}"
    );
}

/// Знаковый приёмник обёртки не получает - контроль.
///
/// Без этой проверки правка читалась бы как "печатать обёртку всегда", а у знакового
/// переполнения в языке иной смысл: это ошибка программы, а не перенос.
#[test]
fn signed_widening_stays_plain() {
    let (_d, rust) = generate("mixed", "rust", MIXED);
    assert!(
        rust.contains("(self.s as i16) + (self.u as i16)"),
        "знаковая арифметика печатается обычным `+`.\n{rust}"
    );
}

// -- Настоящие инструменты ----------------------------------------------------

fn tool(bin: &str) -> bool {
    Command::new(bin)
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Вывод цели `rust` принимается `clippy -D warnings` на обоих входах.
#[test]
fn rust_output_compiles() {
    if !tool("clippy-driver") {
        eprintln!("[ПРОПУСК] rust_output_compiles: `clippy-driver` не найден");
        return;
    }
    for (tag, source) in [("widen", WIDEN), ("mixed", MIXED), ("keep", KEEP)] {
        let (dir, _) = generate(tag, "rust", source);
        let out = Command::new("clippy-driver")
            .args(["--edition", "2021", "--crate-type=lib", "-D", "warnings"])
            .arg(dir.join("probe.rs"))
            .arg("--out-dir")
            .arg(dir.join("out"))
            .output()
            .expect("запуск clippy-driver");
        assert!(
            out.status.success(),
            "вывод цели `rust` для '{tag}' обязан собираться:\n{}",
            String::from_utf8_lossy(&out.stderr)
        );
    }
}
