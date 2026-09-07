//! Не-ASCII идентификатор и алфавиты целей.

use std::process::Command;
use takt_lang::generator::GenerateOptions;

/// Кириллица в четырёх видах объявлений сразу: переменная, порт, состояние.
const CYRILLIC: &str = "var счёт: u8 := 0;\n\
                        out Кнопка: bit;\n\
                        \n\
                        start Работа {\n\
                        \x20   always { счёт := счёт + 1; Кнопка := 1; }\n\
                        \x20   ref Работа;\n\
                        }\n";

/// Тот же смысл латиницей - контрольная форма.
const ASCII: &str = "var count: u8 := 0;\n\
                     out Button: bit;\n\
                     \n\
                     start Work {\n\
                     \x20   always { count := count + 1; Button := 1; }\n\
                     \x20   ref Work;\n\
                     }\n";

fn build_dir(tag: &str) -> std::path::PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .to_string();
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0200_{thread}_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог вывода");
    dir
}

fn tool_available(tool: &str) -> bool {
    Command::new(tool)
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Код диагностики цели (или `None` при успехе).
fn target_error(target: &str, tag: &str, source: &str) -> Option<String> {
    let dir = build_dir(tag);
    let path = dir.to_str().expect("путь в UTF-8");
    let opts = GenerateOptions::default();
    let result = match target {
        "c" => takt_lang::compile_to_c(tag, source, path, &[], &opts),
        "rust" => takt_lang::compile_to_rust(tag, source, path, &[], &opts),
        "sv" => takt_lang::compile_to_sv(tag, source, path, &[], &opts),
        "st" => takt_lang::compile_to_st(tag, source, path, &[], &opts),
        other => panic!("неизвестная цель '{other}'"),
    };
    result.err().map(|d| d.code.unwrap_or_else(|| "?".into()))
}

/// Цель `sv` отвергает не-ASCII имя своей диагностикой.
#[test]
fn sv_rejects_non_ascii_identifier() {
    assert_eq!(
        target_error("sv", "sv_cyr", CYRILLIC).as_deref(),
        Some("SV-018"),
        "отказ обязан приходить от компилятора, а не от verilator на .sv"
    );
}

/// Цель `st` - то же, своим кодом.
#[test]
fn st_rejects_non_ascii_identifier() {
    assert_eq!(
        target_error("st", "st_cyr", CYRILLIC).as_deref(),
        Some("ST-020"),
        "отказ обязан приходить от компилятора, а не от iec2c на .st"
    );
}

/// Цели `c` и `rust` тот же вход **переводят**.
///
/// Это и есть причина, по которой отказ живёт в цели, а не в языке: запрет в семантике
/// отнял бы работающую возможность.
#[test]
fn c_and_rust_still_accept_non_ascii_identifier() {
    for target in ["c", "rust"] {
        assert_eq!(
            target_error(target, &format!("ok_{target}"), CYRILLIC),
            None,
            "цель {target} обязана переводить не-ASCII имена"
        );
    }
}

/// Вывод цели `c` на не-ASCII именах принимается настоящим `cc`.
///
/// Проверка **сборкой**, а не строкой: строковая проверка закрепила бы наше
/// представление о правильном, а `cc` проверяет то, что считает правильным компилятор
/// C.
#[test]
fn generated_c_with_non_ascii_names_compiles() {
    if !tool_available("cc") {
        eprintln!("[ПРОПУСК] generated_c_with_non_ascii_names_compiles: `cc` не найден");
        return;
    }
    let dir = build_dir("cc_gate");
    takt_lang::compile_to_c(
        "cyr",
        CYRILLIC,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("цель c принимает не-ASCII имена");
    let out = Command::new("cc")
        .args(["-std=c11", "-Wall", "-Werror", "-I"])
        .arg(&dir)
        .arg("-c")
        .arg(dir.join("cyr.c"))
        .arg("-o")
        .arg(dir.join("cyr.o"))
        .output()
        .expect("запуск cc");
    assert!(
        out.status.success(),
        "порождённый C с не-ASCII именами обязан компилироваться:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
}

/// ASCII-имена не задеты ни одной целью.
#[test]
fn ascii_identifiers_are_unaffected() {
    for target in ["c", "rust", "sv", "st"] {
        assert_eq!(
            target_error(target, &format!("ascii_{target}"), ASCII),
            None,
            "цель {target} обязана переводить обычные имена как прежде"
        );
    }
}

/// Отказ приходит на **каждый** вид объявления, а не только на порт.
///
/// Условие живёт в самой проверке имени, а не в её вызовах - иначе осталась бы дыра,
/// как у направления порта без обхода тел. Тест держит это свойство: каждая фикстура несёт
/// **ровно одно** не-ASCII имя своего вида.
#[test]
fn every_declaration_kind_is_checked() {
    let cases = [
        (
            "переменная",
            "var счёт: u8 := 0;\nstart Work { always { счёт := 1; } ref Work; }\n",
        ),
        (
            "порт",
            "out Кнопка: bit;\nstart Work { always { Кнопка := 1; } ref Work; }\n",
        ),
        (
            "состояние",
            "var n: u8 := 0;\nstart Работа { always { n := 1; } ref Работа; }\n",
        ),
        (
            "константа",
            "const Предел: u8 := 3;\nvar n: u8 := 0;\nstart Work { always { n := Предел; } ref Work; }\n",
        ),
    ];
    for (kind, src) in cases {
        assert_eq!(
            target_error("sv", &format!("kind_sv_{}", kind.len()), src).as_deref(),
            Some("SV-018"),
            "цель sv обязана отвергать не-ASCII имя в позиции «{kind}»"
        );
        assert_eq!(
            target_error("st", &format!("kind_st_{}", kind.len()), src).as_deref(),
            Some("ST-020"),
            "цель st обязана отвергать не-ASCII имя в позиции «{kind}»"
        );
    }
}
