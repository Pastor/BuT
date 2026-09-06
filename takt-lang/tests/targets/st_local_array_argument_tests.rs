//! Локальный массив в аргументе функции у цели `st`.

use std::path::PathBuf;
use std::process::Command;
use takt_lang::generator::GenerateOptions;

/// Локальный массив в теле состояния и в теле функции - обе позиции.
const SRC: &str = "fn first(a: [u8; 2]) -> u8 {\n    return a[0];\n}\n\
     fn wrap() -> u8 {\n    var inner: [u8; 2];\n    inner[0] := 3;\n    inner[1] := 4;\n\
     \x20   return first(inner);\n}\n\
     var o: u8 := 0;\nvar w: u8 := 0;\nout probe: u8 at 0;\n\
     start Run {\n    always {\n        var part: [u8; 2] := {6, 7};\n\
     \x20       o := first(part);\n        w := wrap();\n        probe := o + w;\n    }\n\
     \x20   ref Run;\n}\n";

fn out_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0409_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог вывода");
    dir
}

fn generate(tag: &str, src: &str) -> (PathBuf, String) {
    let dir = out_dir(tag);
    takt_lang::compile_to_st(
        tag,
        src,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение ST");
    let text = std::fs::read_to_string(dir.join(format!("{tag}.st"))).expect("чтение вывода");
    (dir, text)
}

/// Путь к `iec2c`, если он установлен (ставит `scripts/ensure-iec2c.sh`).
fn iec2c() -> Option<PathBuf> {
    let home = std::env::var("HOME").ok()?;
    let path = PathBuf::from(home).join(".local/bin/iec2c");
    path.exists().then_some(path)
}

/// Предмет: локальное объявление получает **именованную** форму.
#[test]
fn local_array_is_declared_with_the_named_form() {
    let (_, text) = generate("st0409", SRC);
    assert!(
        text.contains("part : TAKT_ARR_2_USINT;"),
        "локальный массив тела состояния обязан объявляться формой:\n{text}"
    );
    assert!(
        text.contains("inner : TAKT_ARR_2_USINT;"),
        "локальный массив тела функции обязан объявляться формой:\n{text}"
    );
    assert!(
        !text.contains("part : ARRAY"),
        "анонимный тип MatIEC не примет в вызове:\n{text}"
    );
}

/// **Контроль:** массив, форма которого в параметрах не встречается, остаётся
/// анонимным.
///
/// Без него правка читалась бы как "все локальные массивы именуются", и вывод корпуса
/// поехал бы молча.
#[test]
fn unrelated_local_array_stays_anonymous() {
    let src = "var o: u8 := 0;\nout probe: u8 at 0;\n\
         start Run {\n    always {\n        var scratch: [u8; 3] := {1, 2, 3};\n\
         \x20       o := scratch[0];\n        probe := o;\n    }\n    ref Run;\n}\n";
    let (_, text) = generate("st0409c", src);
    assert!(
        text.contains("scratch : ARRAY [0..2] OF USINT"),
        "форма без параметра-массива обязана остаться анонимной:\n{text}"
    );
}

/// Порождённый ST принимается настоящим `iec2c` - тем самым арбитром, что отвергал
/// прежний вывод.
#[test]
fn generated_st_is_accepted_by_iec2c() {
    let Some(tool) = iec2c() else {
        eprintln!("[ПРОПУСК] `iec2c` не установлен; текст вывода уже проверен");
        return;
    };
    let (dir, _) = generate("st0409t", SRC);
    let home = std::env::var("HOME").expect("HOME");
    let out = Command::new(&tool)
        .arg("-I")
        .arg(format!("{home}/.local/share/matiec/lib"))
        .arg("-T")
        .arg(&dir)
        .arg(dir.join("st0409t.st"))
        .output()
        .expect("запуск iec2c");
    assert!(
        out.status.success(),
        "iec2c обязан принять вывод:\n{}\n{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
}
