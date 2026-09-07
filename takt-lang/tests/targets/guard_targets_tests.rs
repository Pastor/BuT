//! Охранная формула в целях `sv` и `st`.
//!
//! ## Что здесь ловится
//!
//! До фичи цели `st` и `sv` не печатали охранную формулу **вовсе** - ни при опечатке в
//! имени, ни при верном. Автор, собирающий ПЛК или FPGA, получал прошивку **без**
//! объявленного им средства безопасности, а компилятор рапортовал об успехе. Цели `c` и
//! `rust` ту же формулу печатали как `assert`, симулятор останавливал прогон `SIM-025` -
//! один вход, четыре ответа.

use std::path::{Path, PathBuf};
use std::process::Command;
use takt_lang::generator::GenerateOptions;

const SOURCE: &str = r#"
var level: u8 := 0;

invariant Safe = level < 3;

start Run {
    : [Guard] level < 3;
    always { level := level + 1; }
    ref Run;
}
"#;

fn tmp(tag: &str) -> PathBuf {
    // Каталог уникален по имени потока: тесты идут параллельно.
    let thread = std::thread::current()
        .name()
        .unwrap_or("main")
        .replace("::", "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_guard_0235_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог теста");
    dir
}

fn compile_sv(dir: &Path, opts: &GenerateOptions) -> String {
    takt_lang::compile_to_sv(
        "guard_model",
        SOURCE,
        dir.to_str().expect("путь"),
        &[],
        opts,
    )
    .expect("порождение SV");
    std::fs::read_to_string(dir.join("guard_model.sv")).expect("порождённый .sv")
}

fn tool_available(tool: &str) -> bool {
    Command::new(tool)
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

// -- Цель sv: проверка печатается  ------------------------------------

#[test]
fn sv_emits_guard_assertion() {
    let dir = tmp("emits");
    let code = compile_sv(&dir, &GenerateOptions::default());
    assert!(
        code.contains("assert ("),
        "цель sv обязана печатать охранную формулу:\n{code}"
    );
    // Две формулы: явная `: [Guard]` и десахаризация `invariant`.
    assert_eq!(
        code.matches("assert (").count(),
        2,
        "обе охранные формулы обязаны попасть в вывод:\n{code}"
    );
}

#[test]
fn sv_guard_has_no_else_branch() {
    // Не косметика: `assert (...) else $error("...");` verilator принимает, а yosys
    // отвергает синтаксически (`unexpected TOK_ELSE`). То есть форма с сообщением
    // прошла бы локальную проверку и уронила проверка. Имя инварианта цель `sv` не печатает
    // именно поэтому, а не по забывчивости.
    let dir = tmp("no_else");
    let code = compile_sv(&dir, &GenerateOptions::default());
    for forbidden in ["else $error", "$error", "$fatal", "Safe"] {
        assert!(
            !code.contains(forbidden),
            "в выводе sv не должно быть '{forbidden}' (yosys не примет):\n{code}"
        );
    }
}

// -- Цель sv: инструменты проверки  ------------------------------------------

#[test]
fn sv_guard_output_is_accepted_by_both_tools() {
    // Два инструмента обязательны: verilator и yosys ловят непересекающиеся классы - и
    // здесь это не теория, а прямое наблюдение: форму с `else` verilator принял, а
    // yosys отверг.
    if !tool_available("verilator") || !tool_available("yosys") {
        eprintln!("[пропуск] verilator или yosys не найдены");
        return;
    }
    let dir = tmp("tools");
    let _ = compile_sv(&dir, &GenerateOptions::default());
    let sv = dir.join("guard_model.sv");

    let lint = Command::new("verilator")
        .args(["--lint-only", "-Wall", "-Wno-DECLFILENAME"])
        .arg(&sv)
        .output()
        .expect("запуск verilator");
    assert!(
        lint.status.success(),
        "verilator обязан принять вывод с assert:\n{}",
        String::from_utf8_lossy(&lint.stderr)
    );

    let synth = Command::new("yosys")
        .args([
            "-q",
            "-p",
            &format!(
                "read_verilog -sv {}; synth -top guard_model",
                sv.to_str().expect("путь")
            ),
        ])
        .output()
        .expect("запуск yosys");
    assert!(
        synth.status.success(),
        "yosys обязан синтезировать вывод с assert:\n{}",
        String::from_utf8_lossy(&synth.stderr)
    );
}

// -- Цель sv: --guard-disable  --------------------------------------------

#[test]
fn sv_guard_disable_suppresses_assertions() {
    // Поле `SvMap::guard_enable` существовало с самого генератора, но ни один печатник его
    // не читал - заготовка без потребителя. Фича делает его живым, и тест проверяет оба
    // состояния: иначе флаг снова станет мёртвым молча.
    let dir = tmp("disabled");
    let code = compile_sv(&dir, &GenerateOptions::new(false));
    assert!(
        !code.contains("assert ("),
        "--guard-disable обязан подавлять проверки в цели sv:\n{code}"
    );
}

// -- Цель st: предупреждение, а не молчание и не отказ (A6-A8) ----------------

#[test]
fn st_warns_about_untranslatable_guard_and_still_compiles() {
    let dir = tmp("st_warn");
    let source_path = dir.join("guard_model.takt");
    std::fs::write(&source_path, SOURCE).expect("запись исходника");

    let out = Command::new(env!("CARGO_BIN_EXE_taktc"))
        .args(["compile", "-t", "st"])
        .arg(&source_path)
        .args(["-o", dir.join("out").to_str().expect("путь")])
        .output()
        .expect("запуск taktc");

    assert!(
        out.status.success(),
        "цель st обязана ПРОДОЛЖИТЬ трансляцию (решение заказчика: предупреждение, \
         а не отказ):\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        dir.join("out/guard_model.st").exists(),
        "вывод ST обязан быть создан несмотря на предупреждение"
    );

    let stderr = String::from_utf8_lossy(&out.stderr);
    let warnings = stderr.matches("ST-022").count();
    // Формул две (явная и десахаризация `invariant`) - предупреждений тоже две. Счёт, а
    // не факт наличия: одно предупреждение на модель означало бы, что вторая формула
    // потеряна молча - тот самый дефект, который фича лечит.
    assert_eq!(
        warnings, 2,
        "предупреждение обязано выдаваться на КАЖДУЮ охранную формулу:\n{stderr}"
    );
    // Текст обязан называть причину: иначе автор решит, что это дефект компилятора.
    assert!(
        stderr.contains("IEC 61131-3") && stderr.contains("assert"),
        "ST-022 обязан называть причину (в IEC 61131-3 нет assert):\n{stderr}"
    );
}

#[test]
fn st_output_without_guards_is_silent() {
    // Контрпример: модель без формул не должна получать ST-022 - иначе предупреждение
    // обесценится и его начнут пролистывать.
    let dir = tmp("st_silent");
    let source_path = dir.join("plain.takt");
    std::fs::write(
        &source_path,
        "var level: u8 := 0;\nstart Run { always { level := level + 1; } ref Run; }\n",
    )
    .expect("запись исходника");

    let out = Command::new(env!("CARGO_BIN_EXE_taktc"))
        .args(["compile", "-t", "st"])
        .arg(&source_path)
        .args(["-o", dir.join("out").to_str().expect("путь")])
        .output()
        .expect("запуск taktc");

    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        !stderr.contains("ST-022"),
        "модель без охранных формул не должна получать ST-022:\n{stderr}"
    );
}
