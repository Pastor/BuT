//! Одноимённые константы разных моделей не сливаются.

use std::process::Command;
use takt_lang::generator::GenerateOptions;

/// Две модели, у каждой своя константа `K`; каждая пишет в свой порт.
const DUP: &str = "out a: u8;\n\
                   out b: u8;\n\
                   model A { const K: u8 := 2; start Run { always { a := K; } ref Run; } }\n\
                   model B { const K: u8 := 3; start Run { always { b := K; } ref Run; } }\n\
                   start Main = A | B;\n";

/// Тот же вход, но константа модели `B` **не используется**.
///
/// До фильтр "константа используется" ключевался голым именем, и
/// запись `K` считалась задействованной обеими моделями. После квалификации
/// имени такая тёзка напечаталась бы объявлением, к которому никто не
/// обращается, - а неиспользуемая `const` в Rust под `-D warnings` это
/// **отказ сборки**, не предупреждение.
const DUP_ONE_UNUSED: &str = "out a: u8;\n\
                              out b: u8;\n\
                              model A { const K: u8 := 2; start Run { always { a := K; } ref Run; } }\n\
                              model B { const K: u8 := 3; start Run { always { b := 7; } ref Run; } }\n\
                              start Main = A | B;\n";

fn build_dir(tag: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0193_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание каталога");
    dir
}

/// Порождает Rust и возвращает каталог с текстом модуля.
fn generate_rust(tag: &str, source: &str) -> (std::path::PathBuf, String) {
    let dir = build_dir(tag);
    takt_lang::compile_to_rust(
        tag,
        source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение Rust");
    let text = std::fs::read_to_string(dir.join(format!("{tag}.rs"))).expect("чтение модуля Rust");
    (dir, text)
}

/// Порождает SystemVerilog и возвращает текст модуля.
fn generate_sv(tag: &str, source: &str) -> String {
    let dir = build_dir(tag);
    takt_lang::compile_to_sv(
        tag,
        source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение SystemVerilog");
    std::fs::read_to_string(dir.join(format!("{tag}.sv"))).expect("чтение модуля SV")
}

// -- Слой 1: состав объявлений ------------------------------------------------

/// **R1 (цель `rust`):** одноимённые константы дают два объявления, и каждая
/// модель читает своё.
#[test]
fn rust_emits_both_constants_and_each_model_reads_its_own() {
    let (_dir, text) = generate_rust("rsdup", DUP);
    assert!(
        text.contains("const RSDUP_A_K: u8 = 2;") && text.contains("const RSDUP_B_K: u8 = 3;"),
        "у каждой модели обязано быть СВОЁ объявление константы:\n{text}"
    );
    assert!(
        !text.contains("\nconst K:"),
        "голого имени `K` быть не должно — по нему объявления и сливались:\n{text}"
    );
    assert!(
        text.contains("OutU8Port::A, RSDUP_A_K") && text.contains("OutU8Port::B, RSDUP_B_K"),
        "каждая модель обязана читать свою константу, а не соседскую:\n{text}"
    );
}

/// **R1 (цель `sv`):** то же для `localparam` уплощённого модуля.
#[test]
fn sv_emits_both_localparams_and_each_model_reads_its_own() {
    let text = generate_sv("svdup", DUP);
    assert!(
        text.contains("localparam logic [7:0] svdup_a_K = 2;")
            && text.contains("localparam logic [7:0] svdup_b_K = 3;"),
        "каждая модель обязана получить свой localparam:\n{text}"
    );
    assert!(
        !text.contains("logic [7:0] K ="),
        "голого имени `K` быть не должно — модуль SV уплощён, это одно \
         пространство имён:\n{text}"
    );
}

/// **R2:** имя объявления и имя в выражении - одно и то же имя.
///
/// Разъехавшись, они дают либо потерянное объявление, либо ссылку в пустоту; именно
/// поэтому печать и дедупликация обязаны идти одной функцией.
#[test]
fn declaration_and_reference_use_the_same_name() {
    let (_dir, rust) = generate_rust("rsname", DUP);
    for name in ["RSNAME_A_K", "RSNAME_B_K"] {
        assert!(
            rust.contains(&format!("const {name}: u8 =")) && rust.matches(name).count() >= 2,
            "имя '{name}' обязано встретиться и в объявлении, и в обращении:\n{rust}"
        );
    }
}

/// **R3:** неиспользуемая тёзка не печатается.
#[test]
fn unused_namesake_constant_is_not_emitted() {
    let (_dir, text) = generate_rust("rsunused", DUP_ONE_UNUSED);
    assert!(
        text.contains("const RSUNUSED_A_K: u8 = 2;"),
        "используемая константа обязана остаться:\n{text}"
    );
    assert!(
        !text.contains("RSUNUSED_B_K"),
        "неиспользуемая константа-тёзка печататься не должна: под `-D warnings` \
         это отказ сборки, а не предупреждение:\n{text}"
    );
}

// -- Слой 2: тот же проверка, что в precheck.sh -----------------------------------

fn clippy_available() -> bool {
    Command::new("clippy-driver")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// **A3:** вывод на входе с неиспользуемой тёзкой принимается `clippy -D warnings`.
///
/// Первый слой ловит регресс дёшево, этот доказывает, что ловим мы **настоящее**
/// правило линта, а не своё представление о нём (образец - 0174).
#[test]
fn generated_module_with_namesake_passes_clippy_gate() {
    if !clippy_available() {
        eprintln!(
            "[ПРОПУСК] generated_module_with_namesake_passes_clippy_gate: \
             clippy-driver не найден"
        );
        return;
    }
    let (dir, _) = generate_rust("rsgate", DUP_ONE_UNUSED);
    let wrapper = dir.join("gate.rs");
    std::fs::write(
        &wrapper,
        format!(
            "#![no_std]\n#[path = \"{}\"]\npub mod generated;\n",
            dir.join("rsgate.rs").display()
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
        "порождённый Rust обязан приниматься `clippy -D warnings` — это тот же \
         гейт, что в `precheck.sh`:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
}
