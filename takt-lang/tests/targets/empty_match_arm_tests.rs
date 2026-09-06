//! Пустая ветвь `match` -.

use std::path::PathBuf;
use std::process::Command as Proc;
use takt_lang::generator::GenerateOptions;

/// Предмет: пустая ветвь по умолчанию.
const EMPTY_DEFAULT: &str = "var op: u8 := 0; var acc: u8 := 0;\n\
                             out probe: u8 at 0x100;\n\
                             start Run {\n\
                                 always {\n\
                                     op := op + 1;\n\
                                     match op { 1 => { acc := acc + 1; } _ => {} }\n\
                                     probe := acc;\n\
                                 }\n\
                                 ref Run;\n\
                             }\n";

/// Предмет: пустая ветвь образца при непустой соседней.
const EMPTY_ARM: &str = "var op: u8 := 0; var acc: u8 := 0;\n\
                         out probe: u8 at 0x100;\n\
                         start Run {\n\
                             always {\n\
                                 op := op + 1;\n\
                                 match op { 1 => {} 2 => { acc := acc + 10; } }\n\
                                 probe := acc;\n\
                             }\n\
                             ref Run;\n\
                         }\n";

/// **Контроль:** непустая ветвь по умолчанию печатается как прежде.
const FULL_DEFAULT: &str = "var op: u8 := 0; var acc: u8 := 0;\n\
                            out probe: u8 at 0x100;\n\
                            start Run {\n\
                                always {\n\
                                    op := op + 1;\n\
                                    match op { 1 => { acc := acc + 1; } _ => { acc := 0; } }\n\
                                    probe := acc;\n\
                                }\n\
                                ref Run;\n\
                            }\n";

/// **Контроль:** дубль образца - пустая ветвь остаётся, иначе поедет автомат.
const DUPLICATE: &str = "var op: u8 := 0; var acc: u8 := 0;\n\
                         out probe: u8 at 0x100;\n\
                         start Run {\n\
                             always {\n\
                                 op := op + 1;\n\
                                 match op { 1 => {} 1 => { acc := acc + 10; } }\n\
                                 probe := acc;\n\
                             }\n\
                             ref Run;\n\
                         }\n";

fn generate(tag: &str, target: &str, source: &str) -> (PathBuf, String) {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0509_{thread}_{tag}_{target}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    let path = dir.to_str().expect("путь в UTF-8");
    let opts = GenerateOptions::default();
    match target {
        "rust" => takt_lang::compile_to_rust(tag, source, path, &[], &opts).map(|_| ()),
        "st" => takt_lang::compile_to_st(tag, source, path, &[], &opts).map(|_| ()),
        "sv" => takt_lang::compile_to_sv(tag, source, path, &[], &opts).map(|_| ()),
        _ => takt_lang::compile_to_c(tag, source, path, &[], &opts).map(|_| ()),
    }
    .unwrap_or_else(|e| panic!("порождение для '{target}': {e:?}"));
    let ext = match target {
        "rust" => "rs",
        "st" => "st",
        "sv" => "sv",
        _ => "c",
    };
    let text = std::fs::read_to_string(dir.join(format!("{tag}.{ext}"))).expect("чтение");
    (dir, text)
}

/// **T1.** Цель `rust`: пустая `_`-ветвь не печатается.
#[test]
fn rust_drops_empty_default_arm() {
    let (_d, text) = generate("empty_def", "rust", EMPTY_DEFAULT);
    assert!(
        !text.contains("} else {"),
        "пустая ветвь `else` — отказ `clippy` под `-D warnings`:\n{text}"
    );
    assert!(
        text.contains("self.acc.wrapping_add(1)"),
        "непустая ветвь обязана остаться:\n{text}"
    );
}

/// **T2.** Цель `st`: пустая `_`-ветвь не даёт `ELSE`.
#[test]
fn st_drops_empty_default_arm() {
    let (_d, text) = generate("empty_def", "st", EMPTY_DEFAULT);
    assert!(
        !text.contains("ELSE"),
        "`iec2c` отвечает «no statement defined after 'ELSE'»:\n{text}"
    );
    assert!(
        text.contains("END_IF;"),
        "цепочка обязана закрыться:\n{text}"
    );
}

/// **T3.** Цель `st`: пустая ветвь образца опускается вместе со своим `IF`.
#[test]
fn st_drops_empty_pattern_arm() {
    let (_d, text) = generate("empty_arm", "st", EMPTY_ARM);
    assert!(
        !text.contains("op = 1"),
        "ветвь пуста — её условие печатать незачем:\n{text}"
    );
    assert!(
        text.contains("op = 2"),
        "непустая ветвь обязана остаться и стать первой:\n{text}"
    );
}

/// **T4. Контроль:** непустая ветвь по умолчанию печатается как прежде.
#[test]
fn non_empty_default_arm_is_kept() {
    let (_d, rust) = generate("full_def", "rust", FULL_DEFAULT);
    assert!(rust.contains("} else {"), "ветвь непуста:\n{rust}");
    let (_d, st) = generate("full_def", "st", FULL_DEFAULT);
    assert!(st.contains("ELSE"), "ветвь непуста:\n{st}");
}

/// **T5.** При дубле образца недостижимая ветвь исчезает вместе с пустой.
///
/// До пустая ветвь при дубле сохранялась: опустить её значило отдать совпадение нижней
/// ветви, то есть поменять автомат. С 0514 нижняя ветвь не печатается вовсе (она
/// недостижима), и оговорка стала не нужна - исчезают обе, а поведение остаётся
/// прежним: первая ветвь пуста.
///
/// Прежняя редакция теста закрепляла форму, которую `iec2c` отвергал ("no statement
/// defined after 'THEN'"), - в миниатюре.
#[test]
fn duplicate_pattern_drops_both_arms() {
    let (_d, text) = generate("dup", "st", DUPLICATE);
    assert_eq!(
        text.matches("op = 1").count(),
        0,
        "первая ветвь пуста, вторая недостижима — печатать нечего:\n{text}"
    );
}

/// **T6. Контроль:** цели `c` и `sv` не затронуты - их вывод принимали и до.
#[test]
fn c_and_sv_are_untouched() {
    let (_d, c) = generate("empty_def", "c", EMPTY_DEFAULT);
    assert!(c.contains("default:"), "в C пустая ветвь законна:\n{c}");
    let (_d, sv) = generate("empty_def", "sv", EMPTY_DEFAULT);
    assert!(
        sv.contains("default:"),
        "у `sv` `default` печатается ВСЕГДА — без него синтезатор выводит защёлку (0322):\n{sv}"
    );
}

/// **T7.** Порождённое принимают инструменты проверок.
#[test]
fn generated_output_passes_target_tools() {
    let clippy = Proc::new("clippy-driver")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false);
    if clippy {
        for (tag, src) in [("gate_def", EMPTY_DEFAULT), ("gate_arm", EMPTY_ARM)] {
            let (dir, _) = generate(tag, "rust", src);
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
    } else {
        eprintln!("[ПРОПУСК] clippy-driver не найден");
    }

    let iec2c = Proc::new("iec2c")
        .arg("-h")
        .output()
        .map(|o| o.status.code().is_some())
        .unwrap_or(false);
    if !iec2c {
        eprintln!("[ПРОПУСК] iec2c не найден");
        return;
    }
    let lib = std::env::var("HOME").map(|h| format!("{h}/.local/share/matiec/lib"));
    let Ok(lib) = lib else {
        eprintln!("[ПРОПУСК] каталог библиотеки MatIEC неизвестен");
        return;
    };
    for (tag, src) in [("gate_def", EMPTY_DEFAULT), ("gate_arm", EMPTY_ARM)] {
        let (dir, _) = generate(tag, "st", src);
        let out_dir = dir.join("iec");
        std::fs::create_dir_all(&out_dir).expect("каталог вывода");
        let out = Proc::new("iec2c")
            .arg("-I")
            .arg(&lib)
            .arg("-T")
            .arg(&out_dir)
            .arg(dir.join(format!("{tag}.st")))
            .output()
            .expect("запуск iec2c");
        assert!(
            out.status.success(),
            "вывод цели `st` ({tag}) обязан приниматься арбитром:\n{}",
            String::from_utf8_lossy(&out.stdout)
        );
    }
}

/// **T8.** Недостижимая ветвь не печатается целями `c`, `rust`, `sv`.
///
/// `match` берёт первое совпадение, поэтому ветвь с повторяющимся образцом не сработает
/// никогда.
#[test]
fn unreachable_arm_is_not_printed() {
    const DUP_VALUE: &str = "var op: u8 := 0; var acc: u8 := 0;\n\
                             out probe: u8 at 0x100;\n\
                             start Run {\n\
                                 always {\n\
                                     op := op + 1;\n\
                                     match op { 1 => { acc := acc + 1; } 1 => { acc := acc + 10; } }\n\
                                     probe := acc;\n\
                                 }\n\
                                 ref Run;\n\
                             }\n";
    const DUP_DEFAULT: &str = "var op: u8 := 0; var acc: u8 := 0;\n\
                               out probe: u8 at 0x100;\n\
                               start Run {\n\
                                   always {\n\
                                       op := op + 1;\n\
                                       match op { 1 => { acc := acc + 1; } _ => { acc := acc + 10; } _ => { acc := acc + 100; } }\n\
                                       probe := acc;\n\
                                   }\n\
                                   ref Run;\n\
                               }\n";

    let (_d, c) = generate("dup_value", "c", DUP_VALUE);
    assert_eq!(
        c.matches("case 1:").count(),
        1,
        "в C две одинаковые метки — ошибка компиляции:\n{c}"
    );
    let (_d, rust) = generate("dup_value", "rust", DUP_VALUE);
    assert_eq!(
        rust.matches("self.op == 1").count(),
        1,
        "вторая ветвь недостижима, а `clippy` её не принимает:\n{rust}"
    );
    let (_d, sv) = generate("dup_default", "sv", DUP_DEFAULT);
    assert_eq!(
        sv.matches("default:").count(),
        1,
        "второй `default` — «Multiple default statements» у verilator:\n{sv}"
    );

    // Печатается первая ветвь: эталон исполняет её, и подмена на последнюю дала бы
    // валидный вывод с другим поведением (тест - потактовая сверка).
    assert!(
        rust.contains("wrapping_add(1)") && !rust.contains("wrapping_add(10)"),
        "остаться обязана первая ветвь:\n{rust}"
    );
    assert!(
        sv.contains("+ 10") && !sv.contains("+ 100"),
        "у `_`-ветвей остаться обязана первая:\n{sv}"
    );
}
