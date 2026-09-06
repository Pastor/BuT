//! Q-хелперы цели `st` печатаются перед первым POU.

use std::process::Command;
use takt_lang::generator::GenerateOptions;

/// Функция с параметром `q(m, n)`: её тело зовёт `TAKT_Q_FLOORDIV`.
const SOURCE: &str = "var gain: q(8, 8) := 1.5;\n\
                      var n: u8 := 0;\n\
                      out a: u8 at 0x100;\n\
                      fn whole(v: q(8, 8)) -> u8 { return v as u8; }\n\
                      start Run { always { n := n + 1; a := whole(gain); } ref Done: n > 3; }\n\
                      state Done { }\n";

fn generate() -> (std::path::PathBuf, String) {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0380_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    takt_lang::compile_to_st(
        "probe",
        SOURCE,
        dir.to_str().expect("путь"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение ST");
    let text = std::fs::read_to_string(dir.join("probe.st")).expect("чтение");
    (dir, text)
}

/// Объявление хелпера стоит раньше функции, которая его зовёт.
#[test]
fn helper_precedes_the_function_that_calls_it() {
    let (_d, st) = generate();
    let helper = st
        .find("FUNCTION TAKT_Q_FLOORDIV")
        .expect("хелпер обязан быть напечатан");
    let user = st
        .find("FUNCTION Probe_whole")
        .expect("пользовательская функция обязана быть напечатана");
    assert!(
        helper < user,
        "в IEC 61131-3 опережающих ссылок нет: хелпер обязан стоять раньше\n{st}"
    );
}

/// Тот же вывод принимает `iec2c` - арбитр, который и отвергал прежний.
#[test]
fn generated_st_is_accepted_by_iec2c() {
    let prefix = std::env::var("IEC2C_PREFIX")
        .unwrap_or_else(|_| format!("{}/.local", std::env::var("HOME").unwrap_or_default()));
    let iec2c = std::path::Path::new(&prefix).join("bin").join("iec2c");
    let lib = std::path::Path::new(&prefix)
        .join("share")
        .join("matiec")
        .join("lib");
    if !iec2c.is_file() || !lib.join("ieclib.txt").is_file() {
        eprintln!("[ПРОПУСК] generated_st_is_accepted_by_iec2c: нет iec2c");
        return;
    }
    let (dir, _) = generate();
    let out_dir = dir.join("iec");
    std::fs::create_dir_all(&out_dir).expect("каталог iec2c");
    let out = Command::new(&iec2c)
        .arg("-I")
        .arg(&lib)
        .arg("-T")
        .arg(&out_dir)
        .arg(dir.join("probe.st"))
        .output()
        .expect("запуск iec2c");
    assert!(
        out.status.success(),
        "вывод обязан приниматься MatIEC:\n{}",
        String::from_utf8_lossy(&out.stdout)
    );
    let _ = std::fs::remove_dir_all(&dir);
}
