//! Слово `from` - контекстное ключевое.
//!
//! ## Что доказывает
//!
//! `from` встречается в языке ровно в одной форме - директиве импорта, но было
//! объявлено **жёстким** ключевым словом.
//!
//! Приём тот же, которым уже живут слова LTL (`X`, `F`, `G`, `U`, `R`, `LTL`, `Guard`):
//! продукция в правиле `Identifier`.

use takt_lang::parse;

fn parses(src: &str) -> Result<(), String> {
    match parse(src, 0) {
        Ok(_) => Ok(()),
        Err(diagnostics) => Err(diagnostics
            .iter()
            .map(|d| d.message.clone())
            .collect::<Vec<_>>()
            .join("; ")),
    }
}

/// `from` - имя поля структуры.
#[test]
fn from_is_a_field_name() {
    let src = r#"
struct Line {
    from: u8,
    to: u8
}

var seg: Line := {1, 2};
start Idle;
"#;
    parses(src).expect("поле 'from' обязано разбираться");
}

/// `from` - имя переменной, состояния и функции.
#[test]
fn from_is_a_declaration_name() {
    for src in [
        "var from: u8 := 1;\nstart Idle;",
        "start from {\n}\n",
        "fn from(v: u8) -> u8 {\n    return v + 1;\n}\nstart Idle;",
    ] {
        parses(src).unwrap_or_else(|e| panic!("вход обязан разбираться: {src}\nответ: {e}"));
    }
}

/// Директива импорта работает в обеих формах - контроль на конфликт LR(1).
#[test]
fn import_forms_still_parse() {
    for src in [
        "import { Point, twice } from \"lib.takt\";\nstart Idle;",
        "import * as Lib from \"lib.takt\";\nstart Idle;",
        // Крайний случай: слово стоит и именем, и терминалом в одной строке.
        "import { from } from \"lib.takt\";\nstart Idle;",
        "import { from as near } from \"lib.takt\";\nstart Idle;",
    ] {
        parses(src).unwrap_or_else(|e| panic!("импорт обязан разбираться: {src}\nответ: {e}"));
    }
}

/// Контроль: прочие ключевые слова именами не стали.
///
/// Без этой проверки правка читается как "ключевые слова теперь можно использовать как
/// имена".
#[test]
fn other_keywords_are_still_hard() {
    for word in [
        "state", "model", "next", "at", "after", "every", "in", "out",
    ] {
        let src = format!("var {word}: u8 := 1;\nstart Idle;");
        assert!(
            parses(&src).is_err(),
            "'{word}' обязано оставаться ключевым словом"
        );
    }
}
