//! Комментарий перед блочным оператором остаётся перед ним.
//!
//! # Как проверяется
//!
//! **По отступу, а не по порядку строк**: уезжая внутрь тела, комментарии сохраняют
//! взаимный порядок, и проверка порядка дефекта не видит. Уехавший комментарий
//! отличается **отступом** - он на уровень глубже оператора, к которому относится.

use takt_lang::format::format_source;

/// Форматирует исходник, требуя успеха.
fn fmt(source: &str) -> String {
    format_source(source).unwrap_or_else(|e| panic!("форматтер отказал: {e:?}"))
}

/// Отступ строки, содержащей `needle`.
fn indent_of(text: &str, needle: &str) -> usize {
    let line = text
        .lines()
        .find(|line| line.contains(needle))
        .unwrap_or_else(|| panic!("строки с {needle:?} нет:\n{text}"));
    line.len() - line.trim_start().len()
}

/// Собирает модель с телом `body` внутри `always`.
fn model_with(body: &str) -> String {
    format!(
        "model M {{\n    var a: u8 := 0;\n\n    start S {{\n        always {{\n{body}\n        }}\n    }}\n}}\n"
    )
}

/// Комментарий перед оператором с телом стоит на его уровне, а не внутри тела.
#[test]
fn comment_stays_at_the_level_of_the_block_statement() {
    // Тело у каждого своё: у `match` внутрь блока идут ветви, а не операторы.
    for (word, block) in [
        ("if", "if a > 0 {\n                a := 2;\n            }"),
        (
            "while",
            "while a > 0 {\n                a := 2;\n            }",
        ),
        ("loop", "loop {\n                break;\n            }"),
        (
            "for",
            "for var i: u8 := 0; i < 2; i := i + 1 {\n                a := 2;\n            }",
        ),
        (
            "match",
            "match a {\n                _ => { a := 0; }\n            }",
        ),
        (
            "assembly",
            "assembly \"c\" {\n                a := 2;\n            }",
        ),
    ] {
        let source = model_with(&format!(
            "            a := 1;\n\n            // перед {word}\n            {block}"
        ));
        let out = fmt(&source);
        assert_eq!(
            indent_of(&out, &format!("// перед {word}")),
            indent_of(&out, &format!("{word} ")),
            "{word}: комментарий уехал с уровня оператора:\n{out}"
        );
    }
}

/// Блок `formula` - тот же случай: у него свой печатник.
#[test]
fn comment_before_formula_block_stays_outside() {
    let source = model_with(
        "            a := 1;\n\n            // перед formula\n            formula {\n                check(a)\n            }",
    );
    let out = fmt(&source);
    assert_eq!(
        indent_of(&out, "// перед formula"),
        indent_of(&out, "formula {"),
        "комментарий уехал внутрь блока формул:\n{out}"
    );
}

/// Пустая строка перед блочным оператором - часть записи автора.
#[test]
fn blank_line_before_block_statement_survives() {
    let out = fmt(&model_with(
        "            a := 1;\n\n            if a > 0 {\n                a := 2;\n            }",
    ));
    assert!(
        out.contains("a := 1;\n\n            if a > 0 {"),
        "пустая строка перед оператором потеряна:\n{out}"
    );
}

/// Контроль: K&R-раскладка цепочки не разорвана.
///
/// Правка выдаёт комментарии перед оператором - в ветви `else` строка ждёт продолжения
/// (`} else `), и выдача в неё разорвала бы канон.
#[test]
fn else_chain_stays_flat() {
    let out = fmt(&model_with(
        "            if a > 0 {\n                a := 1;\n            } else if a = 0 {\n                a := 2;\n            } else {\n                a := 3;\n            }",
    ));
    assert!(
        out.contains("} else if a = 0 {"),
        "цепочка разорвана:\n{out}"
    );
    assert!(out.contains("} else {"), "простой else разорван:\n{out}");
}

/// Контроль строже: комментарий перед веткой `else` цепочку не разрывает.
///
/// Без этого входа мутация "выдавать ведущие и в ветви `else`" проходит незамеченной: в
/// цепочке без комментариев выдавать нечего, и разрыв не проявляется. Проверено
/// мутацией - она даёт `} else // комментарий` и перенос `if` на следующую строку.
///
/// Названная **граница**: сам комментарий при этом уходит внутрь тела ветки. Поставить
/// его между `}` и `else` нельзя, не разорвав K&R-раскладку, а своей строки у ветки
/// `else` в каноне нет.
#[test]
fn comment_before_else_branch_does_not_break_the_chain() {
    let out = fmt(&model_with(
        "            if a > 0 {\n                a := 1;\n            }\n            // перед веткой else\n            else if a = 0 {\n                a := 2;\n            }",
    ));
    assert!(
        out.contains("} else if a = 0 {"),
        "комментарий разорвал цепочку:\n{out}"
    );
}

/// Комментарий внутри тела остаётся внутри - правка не гонит их наружу.
#[test]
fn comment_inside_the_body_stays_inside() {
    let out = fmt(&model_with(
        "            if a > 0 {\n                // внутри тела\n                a := 2;\n            }",
    ));
    assert!(
        indent_of(&out, "// внутри тела") > indent_of(&out, "if a > 0 {"),
        "комментарий тела вынесло наружу:\n{out}"
    );
}

/// Второй прогон ничего не меняет.
#[test]
fn printing_is_idempotent() {
    let source = model_with(
        "            a := 1;\n\n            // перед if\n            if a > 0 {\n                // внутри\n                a := 2;\n            }",
    );
    let once = fmt(&source);
    assert_eq!(once, fmt(&once), "второй прогон изменил текст");
}
