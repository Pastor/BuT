//! Редакторский слой для нового синтаксиса портов.
//!
//! # Что здесь ловится
//!
//! свода: изменение лексики, синтаксиса или семантики **обязано** проверить
//! редакторский слой. Фича ввела ключевое слово `at`, сменила смысл `:=` в объявлении
//! порта и завела диагностики `SE-092`/`SE-094`/`SY-006`. Часть держит машина (разбор в
//! `lsp/semantic_tokens.rs` исчерпывающий - новый токен валит сборку), но **данные** ею
//! не защищаются:
//!
//! - список автодополнения `lsp/keywords.rs::TAKT_KEYWORDS` - обычный массив;
//! - подсветка `at` - ветвь `match` рядом с сорока другими токенами;
//! - доставка диагностик в редактор - общий вход `collect_compile_diagnostics`,
//!   и новая проверка попадает туда, только если её туда добавили.
//!
//! Эти три пункта здесь и проверяются - на реальном сервере, а не на замысле.

#![cfg(feature = "lsp")]

use takt_lang::lsp::{completion_items, semantic_tokens};

/// Объявление порта в новой форме: размещение `at`, начальное значение `:=`.
const SRC: &str = "out led: bit at 0x40000004:2 := 1;\n\
                   in btn: bit at 0x40000000:0;\n\
                   var t: u8 := 0;\n\
                   start S { always { led := 1; t := t + 1; } ref S: btn = 1; }\n";

/// Декодирует дельта-поток в пары "текст токена -> тип".
fn tokens_of(source: &str) -> Vec<(String, u32)> {
    let tokens = semantic_tokens(source);
    let lines: Vec<&str> = source.split('\n').collect();
    let (mut line, mut col) = (0u32, 0u32);
    let mut out = Vec::new();
    for t in &tokens.data {
        if t.delta_line == 0 {
            col += t.delta_start;
        } else {
            line += t.delta_line;
            col = t.delta_start;
        }
        let text: String = lines
            .get(line as usize)
            .map(|l| {
                l.chars()
                    .skip(col as usize)
                    .take(t.length as usize)
                    .collect()
            })
            .unwrap_or_default();
        out.push((text, t.token_type));
    }
    out
}

/// Индекс типа токена в объявленной серверу легенде.
fn type_index(name: &str) -> u32 {
    takt_lang::lsp::SEMANTIC_TOKEN_TYPES
        .iter()
        .position(|t| t.as_str() == name)
        .unwrap_or_else(|| panic!("в легенде нет типа {name}")) as u32
}

/// `at` подсвечивается как **ключевое слово**, а не как имя.
#[test]
fn at_is_highlighted_as_keyword() {
    let toks = tokens_of(SRC);
    let keyword = type_index("keyword");
    let at: Vec<&(String, u32)> = toks.iter().filter(|(text, _)| text == "at").collect();
    assert_eq!(
        at.len(),
        2,
        "оба `at` фикстуры обязаны быть в потоке: {toks:?}"
    );
    assert!(
        at.iter().all(|(_, ty)| *ty == keyword),
        "`at` обязан подсвечиваться ключевым словом: {at:?}"
    );
}

/// Автодополнение предлагает `at` и объясняет, что размещение необязательно.
#[test]
fn completion_offers_at_with_explanation() {
    let items = completion_items(SRC);
    let at = items
        .iter()
        .find(|i| i.label == "at")
        .expect("`at` обязан быть в списке автодополнения");
    let has_detail = at.detail.as_deref().is_some_and(|d| !d.is_empty());
    assert!(
        has_detail || at.documentation.is_some(),
        "у ключевого слова обязано быть пояснение: {at:?}"
    );
}

/// Диагностики фичи доезжают до редактора общим входом.
///
/// Проверяются все три: значение у входа (`SE-092`), невычислимое значение
/// (`SE-094`) и присваивание в позиции значения (`SY-006` - с исправления это
/// **синтаксическая** ошибка, прежде была семантическая `SE-095`).
#[test]
fn feature_diagnostics_reach_the_editor() {
    let cases = [
        (
            "SE-092",
            "in btn: bit at 0x40000000:0 := 1;\nvar t: u8 := 0;\nstart S { always { t := t + 1; } ref S: btn = 1; }\n",
        ),
        (
            "SE-094",
            "var n: u8 := 1;\nout level: u8 at 0x40000008 := n;\nstart S { always { n := n + 1; } }\n",
        ),
        (
            "SY-006",
            "var a: u8 := 0;\nvar b: u8 := 0;\nstart S { always { b := (a := 3) + 1; } }\n",
        ),
    ];
    for (code, source) in cases {
        let codes: Vec<String> =
            takt_lang::collect_compile_diagnostics("probe.takt", source, &[], false)
                .into_iter()
                .filter_map(|d| d.code)
                .collect();
        assert!(
            codes.contains(&code.to_string()),
            "редактор обязан показывать {code}: {codes:?}"
        );
    }
}

/// Позиция диагностики указывает на **объявление** нарушившего порта.
///
/// `Location::Source(файл, смещение, ...)` несёт байтовое смещение, поэтому тест
/// проверяет его прямо: срез исходника с этого места обязан начинаться с объявления.
/// Координата-заглушка (`Codegen`, ноль-по-умолчанию у другого узла) такую проверку не
/// пройдёт - а в пачке сообщений она бесполезна для редактора.
#[test]
fn diagnostics_carry_real_positions() {
    let source = "var t: u8 := 0;\nin btn: bit at 0x40000000:0 := 1;\nstart S { always { t := t + 1; } ref S: btn = 1; }\n";
    let found = takt_lang::collect_compile_diagnostics("probe.takt", source, &[], false);
    let se092 = found
        .iter()
        .find(|d| d.code.as_deref() == Some("SE-092"))
        .expect("SE-092 обязан быть");
    let takt_lang::diagnostics::Location::Source(_, offset, _) = se092.loc else {
        panic!(
            "диагностика обязана нести позицию исходника: {:?}",
            se092.loc
        );
    };
    let at = &source[offset as usize..];
    assert!(
        at.starts_with("in btn"),
        "позиция обязана указывать на объявление порта, а указывает на: {:?}",
        &at[..20.min(at.len())]
    );
}

/// Форматирование редактора не теряет ни размещения, ни начального значения.
///
/// Тот же путь, которым идёт `textDocument/formatting`: сервер зовёт `format_source`.
/// Потеря части объявления здесь означала бы, что сохранение файла в редакторе портит
/// модель.
#[test]
fn formatting_preserves_placement_and_initial_value() {
    let formatted = takt_lang::format::format_source(SRC).expect("форматирование");
    assert!(
        formatted.contains("out led: bit at 0x40000004:2 := 1;"),
        "обе части объявления обязаны пережить форматирование:\n{formatted}"
    );
    let again = takt_lang::format::format_source(&formatted).expect("повторное форматирование");
    assert_eq!(
        formatted, again,
        "форматирование обязано быть идемпотентным"
    );
}
