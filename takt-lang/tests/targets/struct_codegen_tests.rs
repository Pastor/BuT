//! Дефекты генератора C по структурам.
//!
//! Три дефекта, каждый подтверждён пробой и закрыт:
//! 1. `var p: Point := {1,2}` -> `model->p = {1,2};` (невалидный C) -> составной
//!    литерал `(Point){1,2}`;
//! 2. используемая `const c: Coord := {...}` эмитилась макросом `#define`, и
//!    `c.x` -> `{...}.x` (невалидно) -> `static const Coord ... = {...};`;
//! 3. `p.NOSUCHFIELD` компилировалось молча -> компайл-тайм `SE-061`.
//!
//! Дефекты 1/2 проверяются **компиляцией** порождённого C (`cc`), дефект 3 -
//! семантической диагностикой (строки захвачены из реального вывода).

use std::path::{Path, PathBuf};
use std::process::Command;
use takt_lang::semantic::tree::construct_model;
use takt_lang::{GenerateOptions, compile_to_c};

fn tmp(tag: &str) -> PathBuf {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0080_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    dir
}

fn gen_c(src: &str, name: &str, dir: &Path) -> String {
    compile_to_c(
        name,
        src,
        dir.to_str().unwrap(),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение C");
    std::fs::read_to_string(dir.join(format!("{name}.c"))).expect(".c")
}

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

fn assert_cc_compiles(dir: &Path, name: &str) {
    if !cc_available() {
        eprintln!("[ПРОПУСК] `cc` не найден — {name} не проверен сборкой");
        return;
    }
    let out = Command::new("cc")
        .args(["-std=c11", "-Wall", "-Werror", "-c"])
        .arg(dir.join(format!("{name}.c")))
        .arg("-o")
        .arg(dir.join("out.o"))
        .output()
        .expect("запуск cc");
    assert!(
        out.status.success(),
        "порождённый C не компилируется:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
}

/// Дефект 1: инициализатор структурной переменной - составной литерал.
#[test]
fn struct_var_initializer_is_compound_literal() {
    let src = r#"
struct Point { x: u8, y: u8 }
model M {
    var p: Point := {1, 2};
    start S { always { p.x := 5; } ref Done: p.x > 3; }
    state Done;
}
start Entry = M;
"#;
    let dir = tmp("d1");
    let c = gen_c(src, "d1", &dir);
    assert!(
        c.contains("model->p = (Point){1, 2};"),
        "структура должна присваиваться составным литералом `(Point){{…}}`:\n{c}"
    );
    assert_cc_compiles(&dir, "d1");
}

/// Дефект 2: используемая структурная константа - `static const`, а не `#define`.
#[test]
fn used_struct_const_is_static_const() {
    let src = r#"
struct Coord { x: u8, y: u8, z: u8 }
const origin: Coord := {0, 0, 0};
model M {
    var q: u8 := 0;
    start S { always { q := origin.x; } ref Done: q > 0; }
    state Done;
}
start Entry = M;
"#;
    let dir = tmp("d2");
    let c = gen_c(src, "d2", &dir);
    assert!(
        c.contains("static const Coord CONST_D2_ORIGIN = {0, 0, 0};"),
        "структурная константа должна быть `static const`, а не `#define`:\n{c}"
    );
    assert_cc_compiles(&dir, "d2");
}

/// Дефект 3: доступ к несуществующему полю -> `SE-061` на этапе семантики.
#[test]
fn unknown_struct_field_is_se061() {
    let src = r#"
struct Point { x: u8, y: u8 }
model M {
    var p: Point := {1, 2};
    start S { always { p.x := p.NOSUCHFIELD; } ref Done: p.x > 3; }
    state Done;
}
start Entry = M;
"#;
    let (ast, _) = takt_lang::parse(src, 0).expect("разбор");
    let err = construct_model(&ast, None, &[]).expect_err("ожидался SE-061");
    assert_eq!(err.code.as_deref(), Some("SE-061"), "{err:?}");
    assert!(
        format!("{err:?}").contains("не содержит поля 'NOSUCHFIELD'"),
        "текст должен называть отсутствующее поле: {err:?}"
    );
}

/// Дефект 3, позитив: валидное поле не отвергается (тест против ложных срабатываний) -
/// и в условии, и в теле, и в инициализаторе.
#[test]
fn valid_struct_field_access_is_accepted() {
    let src = r#"
struct Point { x: u8, y: u8 }
model M {
    var p: Point := {1, 2};
    var q: u8 := 0;
    cond Ok = p.y > 0;
    start S { always { q := p.x; } ref Done: p.y > 3; }
    state Done;
}
start Entry = M;
"#;
    let (ast, _) = takt_lang::parse(src, 0).expect("разбор");
    construct_model(&ast, None, &[]).expect("валидный доступ к полю не должен отвергаться");
}
