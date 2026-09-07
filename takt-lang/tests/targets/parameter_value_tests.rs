//! Значение параметра модели и константы особых типов.
//!
//! # Что проверяется
//!
//! Оба входа переводятся всеми восемью целями в обоих режимах; значение q-литерала
//! понижено в представление (`2.5` при `q(8, 8)` - это `640`, а не `2`), значение
//! длительности - миллисекунды.
//!
//! Контроль на границу: агрегат массива и структуры в аргументе параметра по-прежнему
//! отвергается целью `st` (`ST-017`) - форму инициализатора экземпляра `iec2c` не
//! принимает, и это названная граница, а не пробел.

use takt_lang::GenerateOptions;

const TARGETS: &[&str] = &[
    "c", "c-hal", "st", "st-at", "rust", "sv", "sv-mmio", "plantuml",
];

/// Модель с параметром типа `q(8, 8)`; аргумент - дробный литерал.
const Q_PARAMETER: &str = "model Worker {\n\
     \x20   parameter setting: q(8, 8) := 1.5;\n\
     \x20   var seen: q(8, 8) := 0.0;\n\
     \x20   var acc: u8 := 0;\n\
     \x20   out out_acc: u8 at 0x300;\n\
     \x20   start Go { always { acc := acc + 1; seen := setting; out_acc := acc; } ref Go: acc < 200; }\n\
     }\n\
     model Probe {\n\
     \x20   var ticks: u8 := 0;\n\
     \x20   out ticks_out: u8 at 0x304;\n\
     \x20   start Run = Worker(setting := 2.5);\n\
     \x20   state Done { always { ticks_out := ticks; } }\n\
     }\n\
     start Main = Probe;\n";

/// Константа типа `duration` - вторая половина замера.
const DURATION_CONST: &str = "model Probe {\n\
     \x20   const HOLD: duration := 2s;\n\
     \x20   var seen: duration := 0ms;\n\
     \x20   var ticks: u8 := 0;\n\
     \x20   out ticks_out: u8 at 0x300;\n\
     \x20   start Cycle { always { ticks := ticks + 1; seen := HOLD; ticks_out := ticks; } ref Cycle: ticks < 200; }\n\
     }\n\
     start Main = Probe;\n";

/// Агрегат в аргументе параметра - названная граница цели `st`.
const ARRAY_PARAMETER: &str = "model Worker {\n\
     \x20   parameter setting: [u8; 2] := {1, 2};\n\
     \x20   var seen: [u8; 2] := {0, 0};\n\
     \x20   var acc: u8 := 0;\n\
     \x20   out out_acc: u8 at 0x300;\n\
     \x20   start Go { always { acc := acc + 1; seen := setting; out_acc := acc; } ref Go: acc < 200; }\n\
     }\n\
     model Probe {\n\
     \x20   var ticks: u8 := 0;\n\
     \x20   out ticks_out: u8 at 0x304;\n\
     \x20   start Run = Worker(setting := {3, 4});\n\
     \x20   state Done { always { ticks_out := ticks; } }\n\
     }\n\
     start Main = Probe;\n";

fn out_dir(tag: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!(
            "takt_0489_{tag}_{}",
            std::thread::current()
                .name()
                .unwrap_or("single")
                .replace(':', "_")
        ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    dir
}

fn emit(
    name: &str,
    source: &str,
    tag: &str,
    specialize: bool,
) -> Result<String, takt_lang::diagnostics::Diagnostic> {
    let dir = out_dir(tag);
    let path = dir.to_str().expect("путь");
    let mut options = GenerateOptions::default();
    options.specialize = specialize;
    let env = takt_lang::address_map::AddressEnv::default();
    let result = match name {
        "c" => takt_lang::compile_to_c("probe", source, path, &[], &options),
        "c-hal" => takt_lang::compile_to_c_hal("probe", source, path, &[], &[], &env, &options),
        "st" => takt_lang::compile_to_st("probe", source, path, &[], &options),
        "st-at" => takt_lang::compile_to_st_at("probe", source, path, &[], &[], &env, &options),
        "rust" => takt_lang::compile_to_rust("probe", source, path, &[], &options),
        "sv" => takt_lang::compile_to_sv("probe", source, path, &[], &options),
        "sv-mmio" => {
            options.hal = true;
            takt_lang::compile_to_sv_mmio("probe", source, path, &[], &[], &env, &options)
        }
        "plantuml" => takt_lang::compile_to_plantuml("probe", source, path, &[]),
        other => panic!("неизвестная цель {other}"),
    };
    result?;
    let mut text = String::new();
    for entry in std::fs::read_dir(&dir).expect("каталог вывода") {
        let file = entry.expect("файл").path();
        if file.is_file() {
            text.push_str(&std::fs::read_to_string(&file).unwrap_or_default());
        }
    }
    let _ = std::fs::remove_dir_all(&dir);
    Ok(text)
}

/// Параметр `q(m, n)` переводится всеми целями в обоих режимах.
#[test]
fn q_parameter_translates_in_both_modes() {
    let mut failed = Vec::new();
    for (mode, label) in [(false, "assign"), (true, "spec")] {
        for target in TARGETS {
            if let Err(err) = emit(target, Q_PARAMETER, &format!("q_{label}_{target}"), mode) {
                failed.push(format!("{target} ({label}): отказ {:?}", err.code));
            }
        }
    }
    assert!(failed.is_empty(), "не переведено:\n{}", failed.join("\n"));
}

/// Значение аргумента понижено в представление `q(8, 8)`: `2.5` -> `640`.
///
/// Проверка **значения**, а не факта компиляции: доедь литерал до цели как написан,
/// вывод остался бы валидным у `c` (там это просто `2.5`), а автомат считал бы другое -
/// ровно класс, ради которого заведены 0381/0382.
#[test]
fn q_argument_is_lowered_to_representation() {
    let text = emit("c", Q_PARAMETER, "q_value", false).expect("цель переводит");
    assert!(
        text.contains("640"),
        "аргумент обязан быть понижен в представление (2.5 · 2⁸ = 640):\n{text}"
    );
    assert!(
        !text.contains("2.5"),
        "дробного литерала за границей семантики быть не должно:\n{text}"
    );
}

/// Константа типа `duration` переводится всеми целями.
#[test]
fn duration_constant_translates_everywhere() {
    let mut failed = Vec::new();
    for target in TARGETS {
        if let Err(err) = emit(target, DURATION_CONST, &format!("dur_{target}"), false) {
            failed.push(format!("{target}: отказ {:?}", err.code));
        }
    }
    assert!(failed.is_empty(), "не переведено:\n{}", failed.join("\n"));
}

/// Значение константы длительности - миллисекунды (`2s` -> `2000`).
#[test]
fn duration_constant_is_printed_in_millis() {
    let text = emit("c", DURATION_CONST, "dur_value", false).expect("цель переводит");
    assert!(
        text.contains("2000"),
        "единица границы — миллисекунда (0183):\n{text}"
    );
}

/// **Контроль:** агрегат в аргументе параметра - названная граница `st`.
///
/// Форму инициализатора экземпляра `iec2c` не принимает, поэтому цель
/// отвечает `ST-017`. Без этого контроля "параметры переводятся" читалось бы как
/// "любые", а прочие цели тот же вход переводят.
#[test]
fn aggregate_argument_is_still_refused_by_st() {
    for target in ["st", "st-at"] {
        let err = emit(target, ARRAY_PARAMETER, &format!("arr_{target}"), false)
            .expect_err("ожидалась названная граница цели");
        assert_eq!(err.code.as_deref(), Some("ST-017"), "{target}");
    }
    for target in ["c", "rust", "sv"] {
        emit(target, ARRAY_PARAMETER, &format!("arr_ok_{target}"), false)
            .unwrap_or_else(|e| panic!("{target}: агрегат обязан переводиться, а получено {e:?}"));
    }
}
