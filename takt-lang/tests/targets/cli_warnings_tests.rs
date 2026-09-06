//! `taktc compile` печатает предупреждения компилятора.
//!
//! До 0081 `unused_variable_warnings` (`SE-036`) и
//! `nondeterministic_transition_warnings` (`SE-037`) - часть публичного API, но из CLI
//! **не вызывались вовсе**: пользователь не видел ни одного предупреждения.
//! Диагностика, которую никто не печатает, равносильна её отсутствию.
//!
//! Проверяется через прогон **бинаря** `taktc` (`CARGO_BIN_EXE_taktc`): предупреждения
//! идут в stderr, поэтому наблюдаются перехватом, как их видит пользователь.

use std::process::Command;

const VALID: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/data/semantic/valid");

fn taktc() -> Command {
    Command::new(env!("CARGO_BIN_EXE_taktc"))
}

/// Каталог для вывода компиляции (файлы `.c`/`.h` нам не важны - важен stderr).
fn out_dir(tag: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0081_{tag}"));
    std::fs::create_dir_all(&dir).expect("каталог вывода");
    dir
}

fn compile_stderr(fixture: &str, tag: &str, extra: &[&str]) -> String {
    let dir = out_dir(tag);
    let out = taktc()
        .arg("compile")
        .args(extra)
        .arg(format!("{VALID}/{fixture}"))
        .arg("-o")
        .arg(&dir)
        .output()
        .expect("запуск taktc");
    String::from_utf8_lossy(&out.stderr).into_owned()
}

/// T1: неиспользуемая переменная (`Ce13`) доезжает до пользователя как `SE-036`.
#[test]
fn unused_variable_warning_is_printed() {
    let stderr = compile_stderr("unused_variable.takt", "unused", &[]);
    assert!(
        stderr.contains("SE-036"),
        "ожидалось предупреждение SE-036 (неиспользуемая переменная):\n{stderr}"
    );
    assert!(
        stderr.contains("Предупреждение"),
        "формат предупреждения:\n{stderr}"
    );
}

/// T2: недетерминированные переходы (`Ce14`) доезжают как `SE-037`.
#[test]
fn nondeterministic_transition_warning_is_printed() {
    let stderr = compile_stderr("nondeterministic_warn.takt", "nondet", &[]);
    assert!(
        stderr.contains("SE-037"),
        "ожидалось предупреждение SE-037 (недетерминизм переходов):\n{stderr}"
    );
}

/// T3: `--quiet` глушит предупреждения (симметрия с существующим поведением).
#[test]
fn quiet_suppresses_warnings() {
    let stderr = compile_stderr("unused_variable.takt", "quiet", &["--quiet"]);
    assert!(
        !stderr.contains("SE-036") && !stderr.contains("Предупреждение"),
        "--quiet обязан заглушить предупреждения:\n{stderr}"
    );
}

/// T4: чистый файл предупреждений не даёт (нет ложных срабатываний).
#[test]
fn clean_model_has_no_warnings() {
    let stderr = compile_stderr("all_vars_used.takt", "clean", &[]);
    assert!(
        !stderr.contains("Предупреждение"),
        "чистый файл не должен давать предупреждений:\n{stderr}"
    );
}
