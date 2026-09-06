//! Подсветка имён типов: классификация по позиции, а не по имени.
//!
//! Что здесь доказывается (критерии приёмки ):
//! - `q(8, 8)` - тип, хотя `q` не ключевое слово и не значится в таблице имён;
//! - псевдоним `type` - тип и в объявлении, и в ссылке;
//! - переменная, названная именем типа (`var bit: u8 := 1;` - законный вход),
//!   типом не считается: имя совпадает, позиция - нет;
//! - тип параметра функции (грамматика разбирает его выражением, а не `Type`);
//! - на неразобранном файле работает описанный запасной путь по имени.
//!
//! Тесты пишутся на классификацию, а не на факт "не паникует": подсветка начинает врать
//! молча, и сборка при этом зелёная.

#![cfg(feature = "lsp")]

use takt_lang::lsp::{SEMANTIC_TOKEN_TYPES, semantic_tokens};

/// Абсолютный токен, декодированный из дельта-потока LSP.
#[derive(Debug, Clone)]
struct Tok {
    line: u32,
    col: u32,
    ty: String,
    text: String,
}

/// Инверсия дельта-кодирования из `lsp/semantic_tokens.rs`.
fn decode(source: &str) -> Vec<Tok> {
    let tokens = semantic_tokens(source);
    let lines: Vec<&str> = source.split('\n').collect();
    let mut out = Vec::new();
    let (mut line, mut col) = (0u32, 0u32);
    for t in &tokens.data {
        if t.delta_line == 0 {
            col += t.delta_start;
        } else {
            line += t.delta_line;
            col = t.delta_start;
        }
        let text = lines
            .get(line as usize)
            .map(|l| {
                l.chars()
                    .skip(col as usize)
                    .take(t.length as usize)
                    .collect::<String>()
            })
            .unwrap_or_default();
        let ty = SEMANTIC_TOKEN_TYPES
            .get(t.token_type as usize)
            .map(|t| t.as_str().to_string())
            .unwrap_or_else(|| "<?>".to_string());
        out.push(Tok {
            line,
            col,
            ty,
            text,
        });
    }
    out
}

/// Тип токена с текстом `text`, встретившегося `nth`-м по счёту (с нуля).
fn nth_kind(source: &str, text: &str, nth: usize) -> String {
    let toks = decode(source);
    let found: Vec<&Tok> = toks.iter().filter(|t| t.text == text).collect();
    assert!(
        found.len() > nth,
        "токен {text:?} №{nth} не найден; всё, что есть: {:?}",
        toks.iter()
            .map(|t| (t.line, t.col, t.text.clone(), t.ty.clone()))
            .collect::<Vec<_>>()
    );
    found[nth].ty.clone()
}

/// Единственное вхождение - его тип.
fn kind(source: &str, text: &str) -> String {
    nth_kind(source, text, 0)
}

/// Зонд: печатает реальную классификацию (правило проекта - сперва зонд, потом проверки
/// против захваченного).
#[test]
fn probe_type_positions() {
    for src in [FIXED_POINT, ALIAS, SHADOWED_NAME, NESTED, FUNCTION_PARAM] {
        println!("--- {:?}", src.lines().next().unwrap_or(""));
        for t in decode(src) {
            println!("  {:>2}:{:<3} {:<12} {:?}", t.line, t.col, t.ty, t.text);
        }
    }
}

// Fixed-point: `q` - тип, хотя ключевым словом не является ------------

const FIXED_POINT: &str = "\
model M {
    var f: q(8, 8) := 1.5;
    start S { always { f := f; } }
}
start Main = M;
";

#[test]
fn fixed_point_constructor_is_a_type() {
    assert_eq!(
        kind(FIXED_POINT, "q"),
        "type",
        "q(8, 8) — тип; до 0196 он красился переменной, потому что классификация \
         шла по имени, а `q` намеренно не ключевое слово (грамматика: иначе `q` \
         перестал бы работать как имя переменной)"
    );
}

// Псевдоним `type`: тип и в объявлении, и в ссылке --------------------

const ALIAS: &str = "\
type Celsius = u8;
model M {
    var t: Celsius := 20;
    start S { always { t := t + 1; } }
}
start Main = M;
";

#[test]
fn type_alias_is_a_type_in_declaration_and_reference() {
    assert_eq!(kind(ALIAS, "Celsius"), "type", "объявление псевдонима");
    assert_eq!(nth_kind(ALIAS, "Celsius", 1), "type", "ссылка на псевдоним");
    assert_eq!(kind(ALIAS, "u8"), "type", "встроенный тип справа от `=`");
}

// Имя совпадает с именем типа, позиция - нет --------------------------

const SHADOWED_NAME: &str = "\
model M {
    var bit: u8 := 1;
    start S { always { bit := bit + 1; } }
}
start Main = M;
";

#[test]
fn variable_named_like_a_type_is_not_a_type() {
    assert_eq!(
        kind(SHADOWED_NAME, "bit"),
        "variable",
        "`var bit: u8 := 1;` — законный вход (проба: компилируется), и `bit` здесь \
         ИМЯ ПЕРЕМЕННОЙ. До 0196 классификация шла по имени и выдавала его за тип"
    );
    assert_eq!(kind(SHADOWED_NAME, "u8"), "type", "а вот `u8` — тип");
}

// Вложенная форма: тип внутри массива ---------------------------------

const NESTED: &str = "\
model M {
    var m: [q(4, 4); 8];
    start S { always { m[0] := m[0]; } }
}
start Main = M;
";

#[test]
fn type_inside_array_is_a_type() {
    assert_eq!(
        kind(NESTED, "q"),
        "type",
        "обход типов рекурсивен: элемент массива — тоже позиция типа"
    );
}

// Тип параметра функции (в АСД - выражение, а не `Type`) --------------

const FUNCTION_PARAM: &str = "\
model M {
    fn twice(x: u8) -> u8 { return x + x; }
    var v: u8 := 0;
    start S { always { v := twice(2); } }
}
start Main = M;
";

#[test]
fn function_parameter_type_is_a_type() {
    // Первое `u8` - тип параметра, второе - возвращаемый тип, третье - у `var`.
    assert_eq!(kind(FUNCTION_PARAM, "u8"), "type", "тип параметра");
    assert_eq!(nth_kind(FUNCTION_PARAM, "u8", 1), "type", "тип возврата");
}

// Неразобранный файл: запасной путь по имени --------------------------

const BROKEN: &str = "\
model M {
    var t: u8 := ;;;
    start S {
";

#[test]
fn unparseable_file_falls_back_to_name_table() {
    // Дерева нет - позиций тоже; классификация обязана продолжать работать по имени, а
    // не исчезать: при наборе текста файл не разбирается почти всегда.
    let toks = decode(BROKEN);
    assert!(!toks.is_empty(), "на битом входе токены не выдаются вовсе");
    assert_eq!(
        kind(BROKEN, "u8"),
        "type",
        "встроенное имя остаётся типом и без дерева — это и есть запасной путь"
    );
}
