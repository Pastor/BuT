//! Тесты классификации семантических токенов сервера `takt-lsp`: закрытие дыры - до сих
//! пор проверялось лишь "не паникует" и длина UTF-16, но не то, что имя `fn` приходит
//! как `FUNCTION`, тип - как `TYPE` и т. Классификация - то, ради чего фича существует;
//! переставь ветви `match` по `Token::Identifier` (`lsp/semantic_tokens.rs`) -
//! подсветка начнёт врать, а сборка осталась бы зелёной.
//!
//! Продуктивный код не трогается  - только тесты и фикстура.

#![cfg(feature = "lsp")]

use takt_lang::lsp::{SEMANTIC_TOKEN_TYPES, semantic_tokens};

/// Абсолютный токен, декодированный из дельта-потока LSP.
#[derive(Debug, Clone)]
struct Tok {
    line: u32,
    col: u32,
    len: u32,
    ty: u32,
    /// Текст токена (по позиции в исходнике), для читаемости зонда/сверок.
    text: String,
}

/// Декодирует дельта-поток `SemanticTokens` в абсолютные токены и достаёт текст каждого
/// по позиции. Инверсия дельта-кодирования из `lsp/semantic_tokens.rs`.
fn decode(source: &str, tokens: &lsp_types::SemanticTokens) -> Vec<Tok> {
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
        // Текст токена: `len` - в кодовых единицах UTF-16, поэтому берём срез по
        // символам строки (для ASCII фикстуры совпадает с байтами).
        let text = lines
            .get(line as usize)
            .map(|l| {
                l.chars()
                    .skip(col as usize)
                    .take(t.length as usize)
                    .collect::<String>()
            })
            .unwrap_or_default();
        out.push(Tok {
            line,
            col,
            len: t.length,
            ty: t.token_type,
            text,
        });
    }
    out
}

/// Имя типа токена по индексу легенды (для читаемых сообщений).
fn ty_name(ty: u32) -> &'static str {
    SEMANTIC_TOKEN_TYPES
        .get(ty as usize)
        .map(|t| t.as_str())
        .unwrap_or("<?>")
}

const FIXTURE: &str = include_str!("../data/lsp/semantic_tokens.takt");

/// Зонд: печатает реальную классификацию токенов фикстуры (правило проекта - "сперва
/// зонд для захвата вывода, затем assertions против захваченного").
#[test]
fn probe_semantic_tokens() {
    let toks = decode(FIXTURE, &semantic_tokens(FIXTURE));
    let mut buf = String::new();
    for t in &toks {
        buf.push_str(&format!(
            "{:>2}:{:<2} len={:<2} {:<11} {:?}\n",
            t.line,
            t.col,
            t.len,
            ty_name(t.ty),
            t.text
        ));
    }
    print!("{buf}");
    if let Ok(p) = std::env::var("TAKT_PROBE_OUT") {
        let _ = std::fs::write(p, buf);
    }
}

/// Все токены с текстом `text` (может встречаться несколько раз - использование и
/// объявление).
fn by_text<'a>(toks: &'a [Tok], text: &str) -> Vec<&'a Tok> {
    toks.iter().filter(|t| t.text == text).collect()
}

/// Утверждает: **все** вхождения `text` имеют тип `expected` (имя из легенды), и
/// вхождений не ноль.
fn assert_kind(toks: &[Tok], text: &str, expected: &str) {
    let found = by_text(toks, text);
    assert!(!found.is_empty(), "токен '{text}' не найден в потоке");
    for t in &found {
        assert_eq!(
            ty_name(t.ty),
            expected,
            "токен '{text}' @ {}:{} классифицирован как '{}', ожидалось '{expected}'",
            t.line,
            t.col,
            ty_name(t.ty),
        );
    }
}

/// R6/A1 (главная сверка фичи): каждая категория **идентификатора** получает свой
/// семантический тип. Ловит перестановку ветвей `match` по `Token::Identifier`
/// (`lsp/semantic_tokens.rs`) - раньше её не ловил ни один тест (дыра ). Включает члены
/// **под-модели** `Thermostat` (`scale`/`Idle`/`Done`): до него они деградировали в
/// `variable`.
#[test]
fn semantic_tokens_classification() {
    let toks = decode(FIXTURE, &semantic_tokens(FIXTURE));

    // Функция: и объявление, и использование `scale(count)`.
    assert_kind(&toks, "scale", "function");
    // Тип-псевдоним и перечисление (имя типа), включая использование `Level`.
    assert_kind(&toks, "Celsius", "type");
    assert_kind(&toks, "Level", "type");
    // Варианты перечисления: объявление `Low`/`High` и использование `High`.
    assert_kind(&toks, "Low", "enumMember");
    assert_kind(&toks, "High", "enumMember");
    // Состояния и модели -> CLASS: под-модель, старт, состояния, корневой старт.
    assert_kind(&toks, "Thermostat", "class");
    assert_kind(&toks, "Idle", "class");
    assert_kind(&toks, "Done", "class");
    assert_kind(&toks, "Main", "class");
    // Прочие идентификаторы -> переменная.
    for v in ["count", "LIMIT", "lvl", "x"] {
        assert_kind(&toks, v, "variable");
    }
    // Встроенный тип имеет приоритет (TAKT_BUILTIN_TYPES) -> TYPE.
    assert_kind(&toks, "u8", "type");
    assert_kind(&toks, "i16", "type");
}

/// R6/A2: не-идентификаторы получают свои типы - ключевое слово, число, комментарий,
/// оператор.
#[test]
fn semantic_tokens_non_identifier_kinds() {
    let toks = decode(FIXTURE, &semantic_tokens(FIXTURE));
    for kw in [
        "type", "enum", "model", "var", "const", "fn", "start", "state", "ref", "return",
    ] {
        assert_kind(&toks, kw, "keyword");
    }
    for num in ["0", "100", "1"] {
        assert_kind(&toks, num, "number");
    }
    for op in [":=", "=", "<="] {
        assert_kind(&toks, op, "operator");
    }
    // Комментарии всех трёх форм (///, //, /* */) -> COMMENT.
    assert!(
        toks.iter().filter(|t| ty_name(t.ty) == "comment").count() >= 4,
        "ожидались 4 комментария (///, три //, блочный) как COMMENT"
    );
    let block = by_text(&toks, "/* блочный комментарий */");
    assert_eq!(block.len(), 1);
    assert_eq!(ty_name(block[0].ty), "comment");
}

/// R6: строковый литерал -> STRING. В самодостаточной модели строк нет, поэтому
/// проверяем на `import "..."` - STRING эмитится лексером (модель для этого не нужна;
/// классификация строки от семантики не зависит).
#[test]
fn semantic_tokens_string_literal() {
    let src = "import \"lib.takt\";\nstart S;";
    let toks = decode(src, &semantic_tokens(src));
    let strings: Vec<_> = toks.iter().filter(|t| ty_name(t.ty) == "string").collect();
    assert_eq!(
        strings.len(),
        1,
        "ожидался ровно один STRING-токен (путь импорта), получено: {toks:?}"
    );
    assert!(
        strings[0].text.contains("lib.takt"),
        "STRING-токен обязан покрывать литерал пути: {:?}",
        strings[0].text
    );
}

/// R7 (робастность): непарсящийся исходник -> токены отдаются, паники нет,
/// идентификаторы деградируют в `variable` (модель не строится через `.ok()`).
#[test]
fn semantic_tokens_broken_source() {
    // Незакрытая скобка модели - семантическая модель не построится.
    let src = "model M {\n    fn scale(x: u8) -> u8 { return x; }\n";
    let toks = decode(src, &semantic_tokens(src));
    assert!(!toks.is_empty(), "даже на битом вводе токены обязаны быть");
    // Имя `scale` без модели деградирует в переменную (не FUNCTION).
    assert_kind(&toks, "scale", "variable");
    // Ключевые слова/операторы лексического уровня - по-прежнему свои.
    assert_kind(&toks, "fn", "keyword");
    assert_kind(&toks, "model", "keyword");
}

/// R7: пустой ввод -> пустой поток, без паники.
#[test]
fn semantic_tokens_empty_source() {
    assert!(semantic_tokens("").data.is_empty());
}

/// R7: длина кириллического идентификатора - в кодовых единицах UTF-16, не байтах.
#[test]
fn semantic_tokens_cyrillic_utf16_length() {
    let src = "extern fn АБВ() -> [bit;8]; start S;";
    let toks = decode(src, &semantic_tokens(src));
    let f = toks.iter().find(|t| t.text == "АБВ").expect("токен АБВ");
    assert_eq!(f.len, 3, "'АБВ' — 3 кодовые единицы UTF-16, не 6 байт");
    assert_eq!(ty_name(f.ty), "function", "'АБВ' объявлена как extern fn");
}
