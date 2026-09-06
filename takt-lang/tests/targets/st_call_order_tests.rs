//! Порядок функций у цели `st` - по зависимостям вызова.

use std::path::{Path, PathBuf};
use std::process::Command;

const FIXTURE: &str = "../takt-sim/tests/data/eval/conformance_call_order.takt";

/// Порядок печати идёт по вызовам, и `iec2c` вывод принимает.
#[test]
fn callee_is_printed_before_caller() {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!(
            "takt_0344_{}",
            std::thread::current()
                .name()
                .unwrap_or("single")
                .replace(':', "_")
        ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    takt_lang::compile_to_st(
        "co",
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение ST");
    let text = std::fs::read_to_string(dir.join("co.st")).expect("чтение");
    let twice = text.find("FUNCTION Co_twice").expect("twice объявлена");
    let quad = text.find("FUNCTION Co_quad").expect("quad объявлена");
    assert!(
        twice < quad,
        "вызываемая функция обязана печататься раньше вызывающей:\n{text}"
    );

    let Some(iec2c) = iec2c_path() else {
        eprintln!("[ПРОПУСК] callee_is_printed_before_caller: iec2c не найден");
        return;
    };
    let out = dir.join("st_out");
    std::fs::create_dir_all(&out).expect("каталог");
    let lib = iec2c
        .parent()
        .and_then(Path::parent)
        .map_or_else(|| PathBuf::from("/usr/local"), Path::to_path_buf)
        .join("share/matiec/lib");
    let run = Command::new(&iec2c)
        .args(["-I".as_ref(), lib.as_os_str()])
        .arg("-T")
        .arg(&out)
        .arg(dir.join("co.st"))
        .output()
        .expect("запуск iec2c");
    let stderr = String::from_utf8_lossy(&run.stderr);
    assert!(
        !stderr.contains("error"),
        "iec2c отверг порождённый ST:\n{stderr}"
    );
}

fn iec2c_path() -> Option<PathBuf> {
    let home = std::env::var("HOME").ok()?;
    let path = PathBuf::from(home).join(".local/bin/iec2c");
    path.is_file().then_some(path)
}
