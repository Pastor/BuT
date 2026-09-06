//! Прерывание вне цикла - `SE-132`.
//!
//! Поэтому проверяется и обратное: внутри цикла запись остаётся законной на любой
//! глубине, иначе отказ сломал бы работающие модели.

use takt_lang::semantic::tree::construct_model;

/// Строит модель и возвращает код отказа (пусто - принято).
fn codes(src: &str) -> Vec<String> {
    let (ast, _) = takt_lang::parse(src, 0).expect("разбор");
    match construct_model(&ast, None, &[]) {
        Ok(_) => Vec::new(),
        Err(diagnostic) => diagnostic.code.clone().into_iter().collect(),
    }
}

/// B1: `break` в теле блока состояния отвергается.
#[test]
fn break_in_block_body_is_rejected() {
    let found = codes(
        r#"
var x: u8 := 0;

start Run {
    always { x := x + 1; break; }
    ref Run;
}
"#,
    );
    assert!(
        found.iter().any(|c| c == "SE-132"),
        "прерывание вне цикла обязано отвергаться: {found:?}"
    );
}

/// B2: `continue` в теле функции отвергается - там цикла тоже нет.
#[test]
fn continue_in_function_body_is_rejected() {
    let found = codes(
        r#"
fn step(v: u8) -> u8 { continue; return v; }

var x: u8 := 0;

start Run {
    always { x := step(x); }
    ref Run;
}
"#,
    );
    assert!(
        found.iter().any(|c| c == "SE-132"),
        "'continue' вне цикла обязан отвергаться: {found:?}"
    );
}

/// B3: внутри цикла запись законна - контроль против слишком широкого отказа.
#[test]
fn break_inside_loop_stays_legal() {
    let found = codes(
        r#"
var x: u8 := 0;

start Run {
    always { loop { x := x + 1; if x > 3 { break; } } }
    ref Run;
}
"#,
    );
    assert!(
        found.is_empty(),
        "break в цикле обязан приниматься: {found:?}"
    );
}

/// B4: глубина не важна - `continue` внутри `if` внутри `for` законен.
#[test]
fn continue_deep_inside_for_stays_legal() {
    let found = codes(
        r#"
var x: u8 := 0;

start Run {
    always {
        for var i: u8 := 0; i < 4; i := i + 1 {
            if i = 2 {
                continue;
            }
            x := x + 1;
        }
    }
    ref Run;
}
"#,
    );
    assert!(
        found.is_empty(),
        "continue в цикле обязан приниматься: {found:?}"
    );
}

/// B5: цикл, закончившийся раньше, признака не оставляет.
///
/// Страж потоковый, и "залипший" счётчик сделал бы запись после цикла законной - то
/// есть отказ пропал бы там, где он и нужен.
#[test]
fn break_after_loop_is_rejected() {
    let found = codes(
        r#"
var x: u8 := 0;

start Run {
    always {
        loop { x := x + 1; if x > 3 { break; } }
        break;
    }
    ref Run;
}
"#,
    );
    assert!(
        found.iter().any(|c| c == "SE-132"),
        "после цикла признак обязан сниматься: {found:?}"
    );
}
