//! Печать операндов степени у цели `rust`.

use std::path::PathBuf;
use std::process::Command;
use takt_lang::generator::GenerateOptions;

/// Все четыре формы рядом, включая степень слагаемым (приёмник обязан доехать до неё
/// сквозь арифметику).
const ALL_FORMS: &str = "var b: u32 := 2;\n\
                         var n: u32 := 3;\n\
                         var k: u8 := 3;\n\
                         var v: u32 := 0;\n\
                         var w: u32 := 0;\n\
                         out probe: u32 at 0x100;\n\
                         start Run {\n\
                             always {\n\
                                 v := 2 ** 8;\n\
                                 w := (2 ** 3) + (b ** 2) + (b ** n) + (b ** k);\n\
                                 probe := v + w;\n\
                             }\n\
                             ref Run;\n\
                         }\n";

fn build_dir(tag: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!(
        "takt_0415_{tag}_{}_{}",
        std::process::id(),
        std::thread::current()
            .name()
            .unwrap_or("t")
            .replace(':', "_")
    ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    dir
}

fn generate_rust(tag: &str, source: &str) -> (PathBuf, String) {
    let dir = build_dir(tag);
    takt_lang::compile_to_rust(
        tag,
        source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение Rust");
    let text = std::fs::read_to_string(dir.join(format!("{tag}.rs"))).expect("чтение модуля");
    (dir, text)
}

/// База-литерал получает тип приёмника суффиксом, а не приведением.
///
/// Суффикс, а не `as`: `2 as u32` - это `clippy::unnecessary_cast`, то есть обмен
/// одного отказа проверки на другой.
#[test]
fn literal_base_takes_the_target_type() {
    let (_dir, text) = generate_rust("pow_base", ALL_FORMS);
    assert!(
        text.contains("(2u32).wrapping_pow(8)"),
        "литеральная база печатается с суффиксом типа приёмника:\n{text}"
    );
    assert!(
        !text.contains("(2).wrapping_pow"),
        "базы без типа остаться не должно (E0689):\n{text}"
    );
}

/// Приведение показателя печатается по нужде.
#[test]
fn exponent_cast_is_printed_on_demand() {
    let (_dir, text) = generate_rust("pow_exp", ALL_FORMS);
    assert!(
        text.contains("wrapping_pow(2)") && text.contains("wrapping_pow(self.n)"),
        "литерал и показатель типа u32 идут без приведения:\n{text}"
    );
    // Контроль: показатель иного типа приведение сохраняет - иначе `u8` не подойдёт
    // сигнатуре `wrapping_pow(u32)`.
    assert!(
        text.contains("wrapping_pow((self.k) as u32)"),
        "показатель типа u8 приведение сохраняет:\n{text}"
    );
}

/// Приёмник доезжает до степени сквозь арифметику.
///
/// Без этого правило действовало бы лишь в позиции "степень прямо в приёмнике", а `v :=
/// (2 ** 3) + x;` - обычная запись.
#[test]
fn target_type_reaches_power_through_arithmetic() {
    let (_dir, text) = generate_rust("pow_sum", ALL_FORMS);
    assert!(
        text.contains("(2u32).wrapping_pow(3)"),
        "степень слагаемым тоже получает тип приёмника:\n{text}"
    );
}

/// Порождённый модуль принимается `clippy -D warnings` - как в проверке.
#[test]
fn generated_rust_passes_clippy_gate() {
    let available = Command::new("clippy-driver")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false);
    if !available {
        eprintln!("[ПРОПУСК] generated_rust_passes_clippy_gate: clippy-driver не найден");
        return;
    }
    let (dir, _) = generate_rust("pow_clippy", ALL_FORMS);
    let wrapper = dir.join("gate.rs");
    std::fs::write(
        &wrapper,
        format!(
            "#![no_std]\n#[path = \"{}\"]\npub mod generated;\n",
            dir.join("pow_clippy.rs").display()
        ),
    )
    .expect("запись обёртки");
    let out = Command::new("clippy-driver")
        .args(["--edition", "2021", "--crate-type=lib", "-D", "warnings"])
        .arg(&wrapper)
        .arg("--out-dir")
        .arg(dir.join("out"))
        .output()
        .expect("запуск clippy-driver");
    assert!(
        out.status.success(),
        "порождённый Rust обязан приниматься `clippy -D warnings`:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
}

/// Позиции приёмника перечислены, и каждая даёт базе тип.
///
/// Прежде здесь стояла названная граница 0415 - "в аргументе встроенной функции
/// приёмник неизвестен, и запись отвергается `RS-011`".
///
/// Тест падает **списком**: позиция, переставшая давать тип, называется по имени. Отказ
/// `RS-011` для литеральной базы после этого - защита в глубину: из корректной
/// программы он недостижим, и недостижимость держат ровно эти пять позиций.
#[test]
fn every_receiver_position_gives_the_base_a_type() {
    let cases: [(&str, &str); 5] = [
        (
            "присваивание",
            "var v: u32 := 0;\nout probe: u32 at 0x100;\n\
             start Run { always { v := 2 ** 3; probe := v; } ref Run; }\n",
        ),
        (
            "аргумент встроенной функции",
            "var v: u32 := 0;\nout probe: u32 at 0x100;\n\
             start Run { always { v := min(2 ** 3, 5); probe := v; } ref Run; }\n",
        ),
        (
            "аргумент локальной функции",
            "fn twice(x: u32) -> u32 { return x + x; }\n\
             var v: u32 := 0;\nout probe: u32 at 0x100;\n\
             start Run { always { v := twice(2 ** 3); probe := v; } ref Run; }\n",
        ),
        (
            "аргумент внешней функции",
            "extern fn sink(x: u32) -> u32;\n\
             var v: u32 := 0;\nout probe: u32 at 0x100;\n\
             start Run { always { v := sink(2 ** 3); probe := v; } ref Run; }\n",
        ),
        (
            "возврат функции",
            "fn eight() -> u32 { return 2 ** 3; }\n\
             var v: u32 := 0;\nout probe: u32 at 0x100;\n\
             start Run { always { v := eight(); probe := v; } ref Run; }\n",
        ),
    ];
    let mut broken = Vec::new();
    for (position, src) in cases {
        let dir = build_dir(&format!("pos_{}", position.replace(' ', "_")));
        match takt_lang::compile_to_rust(
            "pos",
            src,
            dir.to_str().expect("путь в UTF-8"),
            &[],
            &GenerateOptions::default(),
        ) {
            Err(diag) => broken.push(format!("{position}: отказ {:?}", diag.code)),
            Ok(_) => {
                let text =
                    std::fs::read_to_string(dir.join("pos.rs")).expect("чтение порождённого Rust");
                if !text.contains("(2u32).wrapping_pow(3)") {
                    broken.push(format!("{position}: база без типа приёмника"));
                }
            }
        }
        let _ = std::fs::remove_dir_all(&dir);
    }
    assert!(
        broken.is_empty(),
        "позиции приёмника, потерявшие тип базы: {broken:?}"
    );
}
