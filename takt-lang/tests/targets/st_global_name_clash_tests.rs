//! Имя размещённого порта против имён POU у цели `st-at`.
//!
//! # Что здесь ловится
//!
//! Отказ приходит на каждое из четырёх занятых имён и **не** приходит там, где `iec2c`
//! совпадение принимает: имя конфигурации, имя задачи и **локальная** переменная.
//! Список занятых имён - из замера (карточка фичи), и контрольные входы стоят рядом с
//! отказами именно поэтому.

use std::path::PathBuf;
use std::process::Command;

fn taktc() -> Command {
    Command::new(env!("CARGO_BIN_EXE_taktc"))
}

/// Уникальный по тесту каталог.
fn work_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("main")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0455_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог теста");
    dir
}

/// Модель с адресованным портом заданного имени; файл называется `probe.takt`, поэтому
/// корневая модель - `Probe`.
fn port_source(name: &str) -> String {
    format!(
        "model Wrap {{\n    var k: u8 := 0;\n    out {name}: u8 at 0x40000100;\n    start Go {{\n        always {{\n            k := k + 1;\n            {name} := k;\n        }}\n        next Done;\n    }}\n    state Done;\n}}\nstart Main = Wrap;\n"
    )
}

/// Та же модель, но имя носит **локальная** переменная, а порт зовётся `led`.
fn local_source(name: &str) -> String {
    format!(
        "model Wrap {{\n    var {name}: u8 := 0;\n    out led: u8 at 0x40000100;\n    start Go {{\n        always {{\n            {name} := {name} + 1;\n            led := {name};\n        }}\n        next Done;\n    }}\n    state Done;\n}}\nstart Main = Wrap;\n"
    )
}

/// Компилирует целью `st-at`; отдаёт `(успех, stderr)`.
fn compile(tag: &str, source: &str) -> (bool, String) {
    let dir = work_dir(tag);
    let input = dir.join("probe.takt");
    std::fs::write(&input, source).expect("запись пробы");
    let out = taktc()
        .arg("compile")
        .args(["-t", "st-at"])
        .arg(&input)
        .arg("-o")
        .arg(dir.join("out"))
        .output()
        .expect("запуск taktc compile");
    (
        out.status.success(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
    )
}

/// Каждое занятое имя отвергается `ST-024`.
#[test]
fn occupied_names_are_refused() {
    // `Probe` - корневая модель (из имени файла), `ProbeWrap` - блок под-модели,
    // `ProbeMain` - программа, `Res0` и `Inst0` - ресурс и экземпляр.
    for name in ["probe", "probewrap", "probemain", "res0", "inst0"] {
        let (ok, stderr) = compile(&format!("clash_{name}"), &port_source(name));
        assert!(!ok, "имя '{name}' принято, хотя занято POU конфигурации");
        assert!(
            stderr.contains("ST-024") && stderr.contains(name),
            "отказ на '{name}' не называет ни кода, ни имени: {stderr}"
        );
    }
}

/// Контрольные входы: там, где `iec2c` совпадение принимает, отказа нет.
#[test]
fn allowed_names_stay_allowed() {
    // Имя конфигурации (`ProbeConfig`) и задачи (`Tick`) свободны - замер 2026-08-31;
    // обычное имя тем более.
    for name in ["probeconfig", "tick", "led"] {
        let (ok, stderr) = compile(&format!("free_{name}"), &port_source(name));
        assert!(
            ok,
            "имя '{name}' отвергнуто, хотя iec2c его принимает: {stderr}"
        );
    }
    // Локальную переменную правило не задевает: она живёт внутри блока, и совпадение с
    // именем POU `iec2c` принимает.
    let (ok, stderr) = compile("local_probe", &local_source("probe"));
    assert!(
        ok,
        "правило задело локальную переменную, хотя её iec2c принимает: {stderr}"
    );
}
