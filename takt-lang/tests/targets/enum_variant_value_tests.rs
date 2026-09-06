//! Имя варианта рядом с целым операндом -.

use std::path::PathBuf;
use std::process::Command as Proc;
use takt_lang::generator::GenerateOptions;

/// Предмет: целая переменная сравнивается с именем варианта.
const INT_VS_NAME: &str = "enum Op { Nop = 0, Hlt = 3 }\n\
                           var op: u8 := 0;\n\
                           out probe: u8 at 0x100;\n\
                           start Run {\n\
                               always { op := op + 1; probe := op; }\n\
                               ref Done: op = Hlt;\n\
                           }\n\
                           state Done { }\n";

/// Тот же предмет с обратным порядком операндов и неравенством.
const NAME_VS_INT: &str = "enum Op { Nop = 0, Hlt = 3 }\n\
                           var op: u8 := 0;\n\
                           out probe: u8 at 0x100;\n\
                           start Run {\n\
                               always { op := op + 1; probe := op; }\n\
                               ref Done: Hlt != op;\n\
                           }\n\
                           state Done { }\n";

/// **Контроль:** перечислимая переменная - печать прежняя.
const ENUM_VS_NAME: &str = "enum Mode { Idle = 0, Run = 1 }\n\
                            var m: Mode := Idle;\n\
                            out probe: u8 at 0x100;\n\
                            start Boot {\n\
                                always { m := Run; probe := 1; }\n\
                                ref Done: m = Run;\n\
                            }\n\
                            state Done { }\n";

fn build_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0508_{thread}_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание каталога");
    dir
}

fn generate(tag: &str, target: &str, source: &str) -> (PathBuf, String) {
    let dir = build_dir(&format!("{tag}_{target}"));
    let path = dir.to_str().expect("путь в UTF-8");
    let opts = GenerateOptions::default();
    match target {
        "rust" => takt_lang::compile_to_rust(tag, source, path, &[], &opts).map(|_| ()),
        "sv" => takt_lang::compile_to_sv(tag, source, path, &[], &opts).map(|_| ()),
        "c" => takt_lang::compile_to_c(tag, source, path, &[], &opts).map(|_| ()),
        _ => takt_lang::compile_to_st(tag, source, path, &[], &opts).map(|_| ()),
    }
    .unwrap_or_else(|e| panic!("порождение для '{target}': {e:?}"));
    let ext = match target {
        "rust" => "rs",
        "sv" => "sv",
        "c" => "c",
        _ => "st",
    };
    let text = std::fs::read_to_string(dir.join(format!("{tag}.{ext}"))).expect("чтение");
    (dir, text)
}

/// **T1.** Цель `rust`: имя варианта печатается значением.
#[test]
fn rust_prints_variant_value_against_integer() {
    let (_d, text) = generate("int_name", "rust", INT_VS_NAME);
    assert!(
        text.contains("self.op == 3"),
        "у `op: u8` перечислимого типа нет — сравнивать надо со значением:\n{text}"
    );
    assert!(
        !text.contains("Op::Hlt"),
        "имя варианта здесь даёт E0308:\n{text}"
    );
}

/// **T2.** Цель `sv`: то же, иначе `WIDTHEXPAND` роняет проверка цели.
#[test]
fn sv_prints_variant_value_against_integer() {
    let (_d, text) = generate("int_name", "sv", INT_VS_NAME);
    assert!(
        text.contains("== 3"),
        "`ENUMITEMREF` уже сигнала — verilator отвечает WIDTHEXPAND:\n{text}"
    );
    assert!(
        !text.contains("== OP_HLT"),
        "имени варианта в СРАВНЕНИИ остаться не должно (в typedef оно на месте):\n{text}"
    );
}

/// **T3.** Обратный порядок операндов и неравенство - та же пара.
#[test]
fn reversed_operands_are_the_same_pair() {
    let (_d, rust) = generate("name_int", "rust", NAME_VS_INT);
    assert!(
        rust.contains("3 != self.op"),
        "имя слева опознаётся так же:\n{rust}"
    );
    let (_d, sv) = generate("name_int", "sv", NAME_VS_INT);
    assert!(sv.contains("(3 !="), "то же у цели `sv`:\n{sv}");
}

/// **T4. Контроль:** перечислимая переменная сравнивается именем.
///
/// Без этой проверки правку нельзя отличить от "печатать значение всегда", а это ровно
/// тот дефект, который чинила 0281, - с обратным знаком.
#[test]
fn enum_variable_still_compares_by_name() {
    let (_d, rust) = generate("enum_name", "rust", ENUM_VS_NAME);
    assert!(
        rust.contains("self.m == Mode::Run"),
        "у перечислимой переменной числового представления в Rust нет:\n{rust}"
    );
    let (_d, sv) = generate("enum_name", "sv", ENUM_VS_NAME);
    assert!(
        sv.contains("MODE_RUN"),
        "цель `sv` печатает мнемонику, иначе теряется тип:\n{sv}"
    );
}

/// **T5. Контроль:** цель `c` не затронута - её вывод принимали и до.
///
/// В C перечисление есть целое, мнемонику читает человек, и объявление константы цель
/// печатает всегда - `cc` такой вход принимает.
#[test]
fn c_is_untouched() {
    let (_d, c) = generate("int_name", "c", INT_VS_NAME);
    assert!(
        c.contains("ENUM_INT_NAME_OP_HLT"),
        "в C перечисление есть целое, и имя константы читает человек (0167):\n{c}"
    );
}

/// **T6.** Порождённое принимают инструменты проверок.
#[test]
fn generated_output_passes_target_tools() {
    let clippy = Proc::new("clippy-driver")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false);
    if clippy {
        for (tag, src) in [("gate_int", INT_VS_NAME), ("gate_rev", NAME_VS_INT)] {
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

    let verilator = Proc::new("verilator")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false);
    if !verilator {
        eprintln!("[ПРОПУСК] verilator не найден");
        return;
    }
    let (dir, _) = generate("gate_sv", "sv", INT_VS_NAME);
    let out = Proc::new("verilator")
        .args(["--lint-only", "-Wall"])
        .arg(dir.join("gate_sv.sv"))
        .output()
        .expect("запуск verilator");
    assert!(
        out.status.success(),
        "гейт цели `sv` считает предупреждение ошибкой:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
}

/// **T7.** Цель `st`: имя варианта рядом с целым - тоже значение.
///
/// У `st` мнемонику объявляет `VAR CONSTANT` того же POU. Но перечисление, пришедшее из
/// библиотеки вместе С моделью и не названное в списке импорта, до дерева импортёра не
/// доезжает: `iec2c` отвечал "Ambiguous enumerate value or Variable not declared in
/// this scope" при нулевом коде возврата `taktc`. Значение лежит в самом узле и
/// доезжает всегда.
#[test]
fn st_prints_variant_value_against_integer() {
    let (_d, text) = generate("int_name", "st", INT_VS_NAME);
    assert!(
        text.contains("op = 3"),
        "у `op: u8` перечислимого типа нет — сравнивать надо со значением:\n{text}"
    );
    assert!(
        !text.contains("= Op_Hlt"),
        "мнемоника требует объявления, которого в этом POU может не быть:\n{text}"
    );
}
