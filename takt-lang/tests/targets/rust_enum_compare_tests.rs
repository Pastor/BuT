//! Сравнение перечислимой переменной с числом в цели `rust`.
//!
//! # Что здесь проверяется
//!
//! В Takt вариант перечисления - число, и `ref Done: c = 1;` естественно. В Rust у
//! `enum` числового представления в выражении нет: дословный перевод даёт `self.c ==
//! 1`, а `rustc` отвечает **`E0308`** ("expected `Command`, found integer") - при
//! **нулевом** коде возврата `taktc`.
//!
//!
//! | Место | Печаталось | Ответ `rustc` |
//! |---|---|---|
//! | присваивание `c := Go;` | `self.c = Command::Go;` | верно |
//! | сравнение `ref Done: c = 1;` | `if self.c == 1 {` | **`E0308`** |
//!
//! То есть цель восстанавливала вариант **только в одном из двух** мест. Соседние цели
//! (`c`, `st`, `sv`) тот же вход переводят валидно - предмет именно в `rust`.
//!
//! **Проверка цели этого не видел по устройству:** он гоняет только корпус, а сравнения
//! перечисления с числом там нет ни одного. Тот же слепой угол дал дефекты, и 0263.
//!
//! **Контроль обязателен**: сравнение с именем варианта (`c = Go`) печаталось верно
//! всегда - без этой проверки нельзя отличить "починили сравнение с числом" от "сломали
//! сравнение вообще".

use std::path::PathBuf;
use std::process::Command as Proc;
use takt_lang::generator::GenerateOptions;

/// Сравнение с числом - предмет фичи.
const BY_NUMBER: &str = "enum Command { Stop = 0, Go = 1 }\n\
                         var c: Command := Stop;\n\
                         var seen: u8 := 0;\n\
                         start Run {\n\
                             always { c := Go; }\n\
                             ref Done: c = 1;\n\
                         }\n\
                         state Done { always { seen := 1; } }\n";

/// Контроль: сравнение с именем варианта - печаталось верно и до фичи.
const BY_NAME: &str = "enum Command { Stop = 0, Go = 1 }\n\
                       var c: Command := Stop;\n\
                       var seen: u8 := 0;\n\
                       start Run {\n\
                           always { c := Go; }\n\
                           ref Done: c = Go;\n\
                       }\n\
                       state Done { always { seen := 1; } }\n";

/// Неравенство и обратный порядок операндов - та же форма.
const NOT_EQUAL: &str = "enum Command { Stop = 0, Go = 1 }\n\
                         var c: Command := Stop;\n\
                         var seen: u8 := 0;\n\
                         start Run {\n\
                             always { c := Go; }\n\
                             ref Done: 0 != c;\n\
                         }\n\
                         state Done { always { seen := 1; } }\n";

fn build_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0281_{thread}_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание каталога");
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

/// Сравнение с числом печатается вариантом, а не числом.
#[test]
fn comparison_with_number_prints_variant() {
    let (_dir, text) = generate_rust("cmp_num", BY_NUMBER);
    assert!(
        text.contains("self.c == Command::Go"),
        "сравнение обязано восстановить вариант:\n{text}"
    );
    assert!(
        !text.contains("self.c == 1"),
        "числового сравнения остаться не должно:\n{text}"
    );
}

/// Неравенство и обратный порядок операндов - та же форма.
#[test]
fn not_equal_and_reversed_operands_work_too() {
    let (_dir, text) = generate_rust("cmp_ne", NOT_EQUAL);
    assert!(
        text.contains("Command::Stop"),
        "литерал слева обязан распознаваться так же:\n{text}"
    );
    assert!(
        !text.contains("!= 0") && !text.contains("0 !="),
        "числового сравнения остаться не должно:\n{text}"
    );
}

/// **Контроль: сравнение с именем варианта не изменилось.**
#[test]
fn comparison_by_variant_name_is_untouched() {
    let (_dir, text) = generate_rust("cmp_name", BY_NAME);
    assert!(
        text.contains("self.c == Command::Go"),
        "форма с именем варианта работала и до фичи:\n{text}"
    );
}

/// Присваивание и сравнение зовут один носитель имени варианта.
///
/// Разъезд этих двух путей и был дефектом: присваивание вариант восстанавливало,
/// сравнение - нет.
#[test]
fn assignment_and_comparison_agree() {
    let (_dir, text) = generate_rust("cmp_agree", BY_NUMBER);
    assert!(
        text.contains("self.c = Command::Go;") && text.contains("self.c == Command::Go"),
        "оба места обязаны печатать одно и то же имя варианта:\n{text}"
    );
}

/// Порождённый модуль принимается `rustc` и `clippy -D warnings`.
#[test]
fn generated_rust_compiles_and_passes_clippy() {
    let available = Proc::new("clippy-driver")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false);
    if !available {
        eprintln!("[ПРОПУСК] generated_rust_compiles_and_passes_clippy: clippy-driver не найден");
        return;
    }
    for (tag, src) in [
        ("gate_num", BY_NUMBER),
        ("gate_ne", NOT_EQUAL),
        ("gate_name", BY_NAME),
    ] {
        let (dir, _) = generate_rust(tag, src);
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
            "порождённый Rust ({tag}) обязан собираться и проходить clippy:\n{}",
            String::from_utf8_lossy(&out.stderr)
        );
    }
}

/// Значение вне набора вариантов - честный отказ, а не догадка.
#[test]
fn value_outside_variants_is_refused() {
    const OUTSIDE: &str = "enum Command { Stop = 0, Go = 1 }\n\
                           var c: Command := Stop;\n\
                           var seen: u8 := 0;\n\
                           start Run {\n\
                               always { c := Go; }\n\
                               ref Done: c = 7;\n\
                           }\n\
                           state Done { always { seen := 1; } }\n";
    let dir = build_dir("cmp_outside");
    let err = takt_lang::compile_to_rust(
        "cmp_outside",
        OUTSIDE,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .expect_err("значение вне набора вариантов представить нечем");
    assert!(
        err.message.contains("не соответствует ни одному варианту"),
        "отказ обязан называть причину:\n{}",
        err.message
    );
}
