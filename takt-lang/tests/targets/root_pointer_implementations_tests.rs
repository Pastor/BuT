//! Указатель на корень нужен и моделям-реализациям состояния.
//!
//! # Что здесь ловится
//!
//! Три формы реализации плюс контроль (обёртка со своим портом - она работала и
//! указателя). Вердикт даёт **прогон настоящего `cc`** флагами проверки цели: код возврата
//! `taktc` о валидности вывода не говорит.

use std::path::PathBuf;
use std::process::Command;

/// Обёртка без собственных обращений к корню; шаг-модель пишет в порт.
fn wrapper(implementation: &str) -> String {
    format!(
        "\
model First {{
    out a: u8;
    start Go {{
        always {{
            a := 1;
        }}
        next Done;
    }}
    state Done;
}}

model Second {{
    out b: u8;
    start Go {{
        always {{
            b := 2;
        }}
        next Done;
    }}
    state Done;
}}

model Wrap {{
    start Only = {implementation};
}}
start Main = Wrap;
"
    )
}

/// Контроль: у обёртки есть собственный порт - указатель нужен ей и без реализации, и
/// такой вход собирался всегда.
const WRAPPER_WITH_PORT: &str = "\
model First {
    out a: u8;
    start Go {
        always {
            a := 1;
        }
        next Done;
    }
    state Done;
}

model Wrap {
    out own: u8;
    var k: u8 := 0;
    start Only = First {
        next Tail;
    }
    state Tail {
        always {
            k := k + 1;
            own := k;
        }
        ref Tail: k > 0;
    }
}
start Main = Wrap;
";

fn work_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("main")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0439_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог теста");
    dir
}

/// Компилирует исходник целью `c` и собирает вывод `cc` флагами проверки цели.
///
/// Возвращает `None`, если `cc` недоступен: шаг тогда пропускается, а не объявляется
/// пройденным.
fn compiles_with_cc(tag: &str, source: &str) -> Option<Result<(), String>> {
    let dir = work_dir(tag);
    let input = dir.join("probe.takt");
    std::fs::write(&input, source).expect("запись пробы");
    let out = Command::new(env!("CARGO_BIN_EXE_taktc"))
        .arg("compile")
        .arg(&input)
        .arg("-o")
        .arg(dir.join("out"))
        .output()
        .expect("запуск taktc compile");
    assert!(
        out.status.success(),
        "цель `c` обязана перевести вход: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    let cc = Command::new("cc")
        .args([
            "-std=c11",
            "-Wall",
            "-Wextra",
            "-Wno-unused-parameter",
            "-Werror",
            "-c",
        ])
        .arg(dir.join("out").join("probe.c"))
        .arg("-I")
        .arg(dir.join("out"))
        .arg("-o")
        .arg(dir.join("probe.o"))
        .output()
        .ok()?;
    if cc.status.success() {
        Some(Ok(()))
    } else {
        Some(Err(String::from_utf8_lossy(&cc.stderr).into_owned()))
    }
}

#[test]
fn single_model_implementation_gets_root_pointer() {
    let Some(result) = compiles_with_cc("single", &wrapper("First")) else {
        eprintln!("cc недоступен — проверка пропущена");
        return;
    };
    assert!(
        result.is_ok(),
        "обёртка над одиночной реализацией порождает несобираемый C:\n{}",
        result.unwrap_err()
    );
}

#[test]
fn parallel_implementation_gets_root_pointer() {
    let Some(result) = compiles_with_cc("parallel", &wrapper("First | Second")) else {
        eprintln!("cc недоступен — проверка пропущена");
        return;
    };
    assert!(
        result.is_ok(),
        "обёртка над параллелью порождает несобираемый C:\n{}",
        result.unwrap_err()
    );
}

#[test]
fn chain_implementation_gets_root_pointer() {
    let Some(result) = compiles_with_cc("chain", &wrapper("First + Second")) else {
        eprintln!("cc недоступен — проверка пропущена");
        return;
    };
    assert!(
        result.is_ok(),
        "обёртка над цепочкой порождает несобираемый C:\n{}",
        result.unwrap_err()
    );
}

#[test]
fn wrapper_with_own_port_still_compiles() {
    let Some(result) = compiles_with_cc("own_port", WRAPPER_WITH_PORT) else {
        eprintln!("cc недоступен — проверка пропущена");
        return;
    };
    assert!(
        result.is_ok(),
        "контрольный вход перестал собираться:\n{}",
        result.unwrap_err()
    );
}
