//! `SE-036` видит локальные объявления тел.

use std::cell::RefCell;
use std::rc::Rc;

use takt_lang::semantic::ModelNode;
use takt_lang::semantic::tree::construct_model;

fn warnings(src: &str) -> Vec<String> {
    let (ast, _) = takt_lang::parse(src, 0).expect("разбор");
    let model: Rc<RefCell<ModelNode>> = construct_model(&ast, None, &[]).expect("семантика");
    takt_lang::unused_variable_warnings(model)
        .into_iter()
        .map(|d| {
            let pos = match d.loc {
                takt_lang::diagnostics::Location::Source(_, start, _) => start.to_string(),
                other => format!("{other:?}"),
            };
            format!("{}|{pos}", d.message)
        })
        .collect()
}

const UNUSED: &str = r#"
var n: u8 := 0;
out o: u8 at 0;

start Run {
    always {
        var spare: u8 := n + 5;
        n := n + 1;
        o := n;
    }
    ref Run: n < 3;
}
"#;

const USED: &str = r#"
var n: u8 := 0;
out o: u8 at 0;

start Run {
    always {
        var t: u8 := n + 5;
        n := n + 1;
        o := t;
    }
    ref Run: n < 3;
}
"#;

/// Неиспользуемая локальная получает `SE-036`.
#[test]
fn unused_local_is_reported() {
    let found = warnings(UNUSED);
    assert!(
        found.iter().any(|w| w.contains("'spare'")),
        "локальная 'spare' обязана попасть в SE-036: {found:?}"
    );
}

/// Координата указывает на объявление, а не на начало файла и не на блок.
///
/// Проверяется смещением: `1:1` (позиция по умолчанию) - ложь, а не отсутствие
/// координаты.
#[test]
fn warning_points_at_the_declaration() {
    let offset = UNUSED.find("var spare").expect("объявление в тексте");
    let found = warnings(UNUSED);
    let spare = found
        .iter()
        .find(|w| w.contains("'spare'"))
        .expect("предупреждение найдено");
    let pos: usize = spare
        .rsplit('|')
        .next()
        .and_then(|p| p.parse().ok())
        .expect("позиция — смещение");
    assert_eq!(
        pos, offset,
        "координата обязана указывать на объявление (ожидалось смещение {offset}): {spare}"
    );
}

/// Контроль: используемая локальная предупреждения не получает.
///
/// Без него правка читается как "любое локальное объявление подозрительно".
#[test]
fn used_local_is_silent() {
    let found = warnings(USED);
    assert!(
        !found.iter().any(|w| w.contains("'t'")),
        "используемая локальная не должна предупреждаться: {found:?}"
    );
}

/// Локальная во вложенном блоке тоже проверяется, а видимость соблюдается.
///
/// Второй вход - контроль области видимости: имя, объявленное снаружи и использованное
/// внутри `if`, использовано законно.
#[test]
fn nested_blocks_are_checked_and_scoped() {
    let nested_unused = r#"
var n: u8 := 0;
out o: u8 at 0;

start Run {
    always {
        n := n + 1;
        if n < 2 {
            var deep: u8 := n;
        }
        o := n;
    }
    ref Run: n < 3;
}
"#;
    assert!(
        warnings(nested_unused).iter().any(|w| w.contains("'deep'")),
        "локальная вложенного блока обязана проверяться"
    );

    let outer_used_inside = r#"
var n: u8 := 0;
out o: u8 at 0;

start Run {
    always {
        var outer: u8 := n + 1;
        n := n + 1;
        if n < 2 {
            o := outer;
        }
    }
    ref Run: n < 3;
}
"#;
    assert!(
        !warnings(outer_used_inside)
            .iter()
            .any(|w| w.contains("'outer'")),
        "использование во вложенном блоке — законное использование"
    );
}

/// Тела модели и функции проверяются тоже - не только тела состояний.
///
/// Мест объявления тела три (блок модели, блок состояния, функция), и правка, взявшая
/// теста покрывала одно: мутация "снять обход блоков модели" её не роняла. Вход ниже -
/// ровно та проверка, которой не хватало.
#[test]
fn model_level_blocks_and_functions_are_checked() {
    let model_level = r#"
var n: u8 := 0;
out o: u8 at 0;

always {
    var idle_only: u8 := n + 1;
    n := n + 1;
}

start Run {
    always {
        o := n;
    }
    ref Run: n < 3;
}
"#;
    assert!(
        warnings(model_level)
            .iter()
            .any(|w| w.contains("'idle_only'")),
        "локальная блока УРОВНЯ МОДЕЛИ обязана проверяться"
    );

    let in_function = r#"
var n: u8 := 0;
out o: u8 at 0;

fn step(v: u8) -> u8 {
    var tmp: u8 := v + 1;
    return v + 2;
}

start Run {
    always {
        n := n + 1;
        o := step(n);
    }
    ref Run: n < 3;
}
"#;
    assert!(
        warnings(in_function).iter().any(|w| w.contains("'tmp'")),
        "локальная тела ФУНКЦИИ обязана проверяться"
    );
}
