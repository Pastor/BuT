//! Агрегат-литерал в аргументе вызова.
//!
//! # Что проверяется
//!
//! Литерал поднимается во временную переменную **в семантике** - тем же приёмом, что
//! срез и результат вызова, - поэтому цели видят обычную переменную и
//! печатники не трогаются.

use takt_lang::GenerateOptions;

const STRUCT_ARG: &str = "struct Pair { lo: u8, hi: u8 }\n\
     model Probe {\n\
     \x20   var seen: Pair := {0, 0};\n\
     \x20   var ticks: u8 := 0;\n\
     \x20   out ticks_out: u8 at 0x300;\n\
     \x20   fn pick(a: Pair) -> Pair { return a; }\n\
     \x20   start Cycle {\n\
     \x20       always { ticks := ticks + 1; seen := pick({1, 2}); ticks_out := ticks; }\n\
     \x20       ref Cycle: ticks < 200;\n\
     \x20   }\n\
     }\n\
     start Main = Probe;\n";

const ARRAY_ARG: &str = "model Probe {\n\
     \x20   var seen: u8 := 0;\n\
     \x20   var ticks: u8 := 0;\n\
     \x20   out ticks_out: u8 at 0x300;\n\
     \x20   fn take(a: [u8; 2]) -> u8 { return a[0]; }\n\
     \x20   start Cycle {\n\
     \x20       always { ticks := ticks + 1; seen := take({3, 4}); ticks_out := ticks + seen; }\n\
     \x20       ref Cycle: ticks < 200;\n\
     \x20   }\n\
     }\n\
     start Main = Probe;\n";

const TARGETS: &[&str] = &[
    "c", "c-hal", "st", "st-at", "rust", "sv", "sv-mmio", "plantuml",
];

fn out_dir(tag: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!(
            "takt_0493_{tag}_{}",
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
    target: &str,
    source: &str,
    tag: &str,
) -> Result<String, takt_lang::diagnostics::Diagnostic> {
    let dir = out_dir(tag);
    let path = dir.to_str().expect("путь");
    let mut options = GenerateOptions::default();
    let env = takt_lang::address_map::AddressEnv::default();
    let result = match target {
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

/// Агрегат-литерал в аргументе переводят все восемь целей.
#[test]
fn every_target_translates_aggregate_argument() {
    let mut failed = Vec::new();
    for (label, src) in [("структура", STRUCT_ARG), ("массив", ARRAY_ARG)] {
        for target in TARGETS {
            if let Err(err) = emit(target, src, &format!("{label}_{target}")) {
                failed.push(format!("{target} ({label}): отказ {:?}", err.code));
            }
        }
    }
    assert!(failed.is_empty(), "не переведено:\n{}", failed.join("\n"));
}

/// Литерал заменён временной переменной - печатники целей его не видят.
///
/// Проверяется вывод, а не только отсутствие отказа: подъём в семантике и значит, что
/// за её границей агрегата в аргументе не существует.
#[test]
fn aggregate_argument_is_lifted_into_temporary() {
    let text = emit("c", STRUCT_ARG, "lift_c").expect("цель переводит");
    assert!(
        text.contains("takt_slice_"),
        "литерал обязан подниматься во временную:\n{text}"
    );
    assert!(
        !text.contains("pick({1, 2})"),
        "агрегата в позиции аргумента быть не должно:\n{text}"
    );
}
