//! Порт составного типа.
//!
//! # Что сделано
//!
//! - **`c`:** порт составного типа отвергается `CC-015` - как у `c-hal`.
//!   Колбэки HAL принимают скаляр, и структура в них не проходит.
//! - **`sv`:** пользовательские типы объявляются **вне модуля**, поэтому порт
//!   структурного типа стал выразим; порт-**массив** отвергается `SV-002`:
//!   распакованный массив в списке портов **yosys не принимает вовсе**
//!   ("syntax error, unexpected '['"), хотя verilator его допускает - форма
//!   выбирается по тому, что принимают **оба**.

use std::path::PathBuf;
use std::process::Command;

const STRUCT_PORT: &str = "struct Pair { a: u8, b: u8 }\nout po: Pair at 0;\n\
     var v: Pair := {1, 2};\nstart Run { always { po := v; } ref Run: v.a = 1; }\n";
const ARRAY_PORT: &str = "out pa: [u8;2] at 4;\nvar v: u8 := 1;\n\
     start Run { always { pa := {3, 4}; } ref Run: v = 1; }\n";

fn build_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0350_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    dir
}

/// Цель `c`: порт структурного типа **переводится** - разворотом на скалярные.
///
/// Отказ `CC-015` (колбэк HAL принимает скаляр) здесь закреплять нельзя - это утверждение,
/// переставшее быть верным: развернула такой порт в семантике по листам структуры, и
/// цель получает уже скаляры. Сам отказ **не снят**: он остаётся защитой в глубину, а
/// его недостижимость держит чужая проверка.
#[test]
fn c_translates_composite_port_by_leaves() {
    let dir = build_dir("c");
    takt_lang::compile_to_c(
        "cp",
        STRUCT_PORT,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порт структурного типа обязан переводиться развортом");
    let text = std::fs::read_to_string(dir.join("cp.c")).expect("чтение");
    let header = std::fs::read_to_string(dir.join("cp.h")).expect("чтение заголовка");
    assert!(
        header.contains("PORT_PO_A") && header.contains("PORT_PO_B"),
        "порт обязан развернуться в порты по полям:\n{header}"
    );
    assert!(
        text.contains("PC_PORT_PO_A") || text.contains("CP_PORT_PO_A"),
        "запись обязана идти по листам:\n{text}"
    );
}

/// Цель `sv`: порт структурного типа переводится, оба инструмента чисты.
///
/// Проверяется **позиция** `typedef`: он обязан стоять до `module`, иначе шапка
/// ссылается на необъявленный тип.
#[test]
fn sv_struct_port_declares_type_before_module() {
    let dir = build_dir("sv");
    takt_lang::compile_to_sv(
        "cp",
        STRUCT_PORT,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение SystemVerilog");
    let text = std::fs::read_to_string(dir.join("cp.sv")).expect("чтение");
    let typedef = text.find("} pair_t;").expect("тип объявлен");
    let module = text.find("module cp").expect("модуль объявлен");
    assert!(
        typedef < module,
        "тип обязан объявляться до шапки модуля:\n{text}"
    );

    let available = Command::new("verilator")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false);
    if !available {
        eprintln!("[ПРОПУСК] sv_struct_port_declares_type_before_module: verilator не найден");
        return;
    }
    let lint = Command::new("verilator")
        .args(["--lint-only", "-Wall"])
        .arg(dir.join("cp.sv"))
        .output()
        .expect("запуск verilator");
    assert!(
        lint.status.success(),
        "verilator не принял модуль:\n{}",
        String::from_utf8_lossy(&lint.stderr)
    );
}

/// Цель `sv`: порт-массив разворачивается по элементам.
///
/// Названная граница "структуру разворачивают все цели" здесь неверна, и
/// она была **дефектом, закреплённым тестом**: замер 2026-08-23 показал, что запись
/// исполняет эталон, а `sv` отвечала `SV-002`. Невыразим распакованный массив **в шапке
/// модуля** - но не сам порт: по элементам он раскладывается в скалярные, и оба
/// инструмента SV вывод принимают.
#[test]
fn sv_splits_array_port() {
    let dir = build_dir("sv_arr");
    takt_lang::compile_to_sv(
        "ap",
        ARRAY_PORT,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порт-массив разворачивается и переводится");
    let text = std::fs::read_to_string(dir.join("ap.sv")).expect("чтение вывода");
    assert!(
        text.contains("pa_0") && text.contains("pa_1"),
        "порт обязан развернуться по элементам:\n{text}"
    );
    assert!(
        !text.contains("pa["),
        "исходного порта-массива в выводе быть не должно:\n{text}"
    );
}
