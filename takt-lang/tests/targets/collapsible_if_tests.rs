//! Вложенный `if` у цели `rust`.
//!
//! # Где слияние законно, а где меняет автомат
//!
//! Но у ветви `match` это верно **только для последней**: при истинном образце и ложном
//! внутреннем условии слитая ветвь пропустит управление дальше по цепочке - в `else if`
//! или `else`, - тогда как вложенный `if` просто ничего не делает.
//!
//! Границы замерены прогоном `clippy`: `else` у внешнего, `else` у
//! внутреннего и лишний оператор рядом линт принимает - эти формы не трогаются, иначе
//! вывод менялся бы без повода.

use std::path::PathBuf;
use std::process::Command as Proc;
use takt_lang::generator::GenerateOptions;

/// Предмет: вложенный `if` - единственный оператор тела блока.
const NESTED: &str = "var op: u8 := 0; var acc: u8 := 0;\n\
                      out probe: u8 at 0x100;\n\
                      start Run {\n\
                          always {\n\
                              op := op + 1;\n\
                              if op > 1 { if acc < 100 { acc := acc + 5; } }\n\
                              probe := acc;\n\
                          }\n\
                          ref Run;\n\
                      }\n";

/// **Контроль:** у внешнего есть `else` - `clippy` молчит, форма не меняется.
const OUTER_ELSE: &str = "var op: u8 := 0; var acc: u8 := 0;\n\
                          out probe: u8 at 0x100;\n\
                          start Run {\n\
                              always {\n\
                                  op := op + 1;\n\
                                  if op > 1 { if acc < 100 { acc := acc + 5; } } else { acc := 0; }\n\
                                  probe := acc;\n\
                              }\n\
                              ref Run;\n\
                          }\n";

/// Предмет: вложенный `if` - тело последней ветви `match`.
const LAST_ARM: &str = "var op: u8 := 0; var acc: u8 := 0;\n\
                        out probe: u8 at 0x100;\n\
                        start Run {\n\
                            always {\n\
                                op := op + 1;\n\
                                match op {\n\
                                    1 => { acc := acc + 1; }\n\
                                    2 => { if acc < 100 { acc := acc + 5; } }\n\
                                }\n\
                                probe := acc;\n\
                            }\n\
                            ref Run;\n\
                        }\n";

/// **Контроль:** за ветвью идёт `_` - слияние поменяло бы автомат.
const ARM_WITH_DEFAULT: &str = "var op: u8 := 0; var acc: u8 := 0;\n\
                                out probe: u8 at 0x100;\n\
                                start Run {\n\
                                    always {\n\
                                        op := op + 1;\n\
                                        match op {\n\
                                            1 => { if acc > 100 { acc := 1; } }\n\
                                            _ => { acc := acc + 10; }\n\
                                        }\n\
                                        probe := acc;\n\
                                    }\n\
                                    ref Run;\n\
                                }\n";

fn generate(tag: &str, source: &str) -> (PathBuf, String) {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0510_{thread}_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    takt_lang::compile_to_rust(
        tag,
        source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение Rust");
    let text = std::fs::read_to_string(dir.join(format!("{tag}.rs"))).expect("чтение");
    (dir, text)
}

/// Вложенный `if` сливается с внешним.
#[test]
fn nested_if_is_collapsed() {
    let (_d, text) = generate("nested", NESTED);
    assert!(
        text.contains("if (self.op > 1) && (self.acc < 100) {"),
        "`clippy` требует схлопывания:\n{text}"
    );
}

/// **Контроль:** при `else` у внешнего форма прежняя.
#[test]
fn outer_else_is_left_alone() {
    let (_d, text) = generate("outer_else", OUTER_ELSE);
    assert!(
        text.contains("if self.op > 1 {") && text.contains("if self.acc < 100 {"),
        "линт эту форму принимает — менять её незачем:\n{text}"
    );
}

/// Последняя ветвь `match` сливается со своим вложенным `if`.
#[test]
fn last_match_arm_is_collapsed() {
    let (_d, text) = generate("last_arm", LAST_ARM);
    assert!(
        text.contains("} else if self.op == 2 && (self.acc < 100) {"),
        "после последней ветви `else` нет — слияние законно:\n{text}"
    );
}

/// **Контроль:** ветвь перед `_` не сливается.
///
/// Слияние здесь пропустило бы управление в `_`-ветвь: валидный вывод и другой автомат.
/// Значение этого расхождения проверяет потактовая сверка
/// `conformance_collapsible_if_tests`.
#[test]
fn arm_before_default_is_not_collapsed() {
    let (_d, text) = generate("arm_default", ARM_WITH_DEFAULT);
    assert!(
        text.contains("if self.op == 1 {") && text.contains("if self.acc > 100 {"),
        "ветвь не последняя — сливать нельзя:\n{text}"
    );
}

/// Порождённое принимает `clippy -D warnings`.
#[test]
fn generated_output_passes_clippy() {
    let available = Proc::new("clippy-driver")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false);
    if !available {
        eprintln!("[ПРОПУСК] clippy-driver не найден");
        return;
    }
    for (tag, src) in [
        ("gate_nested", NESTED),
        ("gate_outer", OUTER_ELSE),
        ("gate_last", LAST_ARM),
        ("gate_default", ARM_WITH_DEFAULT),
    ] {
        let (dir, _) = generate(tag, src);
        let wrapper = dir.join("gate.rs");
        std::fs::write(
            &wrapper,
            format!(
                "#![no_std]\n#[path = \"{}\"]\npub mod generated;\n",
                dir.join(format!("{tag}.rs")).display()
            ),
        )
        .expect("запись обёртки");
        let out = Proc::new("clippy-driver")
            .args(["--edition", "2021", "--crate-type=lib", "-D", "warnings"])
            .arg(&wrapper)
            .arg("--out-dir")
            .arg(dir.join("out"))
            .output()
            .expect("запуск clippy-driver");
        assert!(
            out.status.success(),
            "вывод цели `rust` ({tag}) обязан собираться:\n{}",
            String::from_utf8_lossy(&out.stderr)
        );
    }
}
