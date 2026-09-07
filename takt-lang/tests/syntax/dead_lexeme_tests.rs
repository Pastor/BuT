//! Тест против мёртвой лексики.
//!
//! Язык описан **двумя** таблицами, и они умеют разойтись молча:
//!
//! - `KEYWORDS` в `takt-lang/src/parser/lexer.rs` - какие слова лексер считает
//!   ключевыми;
//! - extern-блок в `takt-lang/src/grammar.lalrpop` - какие терминалы знает
//!   грамматика.
//!
//! Сверки между ними не было ни одной. Обе существовавшие (`test_completion_
//! covers_lexer_keywords` в LSP и `TaktKeywordSyncTest` плагина IntelliJ) идут
//! **от** `KEYWORDS` наружу, к редакторскому слою, - то есть принимают её за
//! истину и разносят ошибку дальше. Так `string` и `template` попали в списки
//! автодополнения,
//! обещая пользователю конструкции, которых в языке нет.
//!
//! Здесь сверка идёт в обе стороны:
//!
//! 1. **Класс A** - слово в `KEYWORDS`, терминала в грамматике нет. Лексер
//!    выдаст токен, которого парсер не знает: употребление даёт `SY-002`
//!    "нераспознанный токен", хотя слово стоит в списке дополнения.
//! 2. **Класс B** - терминал объявлен в extern-блоке, но не используется ни
//!    одним правилом. Замер показал, что такое объявление не делает
//!    отказ внятнее (сообщение байт-в-байт то же), зато добавляет **каскадную**
//!    вторую диагностику - следствие первой ошибки, а не второй дефект.

use std::collections::BTreeSet;
use takt_lang::parser::lexer::all_keywords;

/// Текст грамматики. `include_str!` привязывает путь на этапе компиляции: переехавший
/// файл валит сборку, а не молча отключает теста.
const GRAMMAR: &str = include_str!("../../src/grammar.lalrpop");

/// Разбирает грамматику на "правила" и "объявления терминалов".
///
/// Возвращает `(тело правил, содержимое extern-блока)`.
fn split_grammar() -> (&'static str, &'static str) {
    let marker = "enum Token<'input> {";
    let start = GRAMMAR
        .find(marker)
        .expect("в грамматике нет extern-блока `enum Token<'input> {`");
    let body_end = GRAMMAR[..start]
        .rfind("extern {")
        .expect("объявление `enum Token` вне блока `extern {`");
    let inner = start + marker.len();
    let end = GRAMMAR[inner..]
        .find("\n    }")
        .expect("extern-блок не закрыт")
        + inner;
    (&GRAMMAR[..body_end], &GRAMMAR[inner..end])
}

/// Терминалы extern-блока: пары "как пишется в правилах" -> строка объявления.
///
/// Квотированные (`"=="`) и именованные (`identifier`, `number`) различаются: в
/// правилах первые встречаются вместе с кавычками, вторые - голым словом.
fn extern_terminals() -> Vec<(String, bool)> {
    let (_, extern_block) = split_grammar();
    let mut out = Vec::new();
    for line in extern_block.lines() {
        let line = strip_comment(line).trim();
        if !line.contains("=>") {
            continue;
        }
        // Левую часть нельзя брать через `split_once("=>")`: сам `"=>"` - терминал
        // языка (ветка `match`), и разрез пришёлся бы внутрь его кавычек. Квотированная
        // часть читается по кавычкам, именованная - до первого пробела.
        if let Some(rest) = line.strip_prefix('"') {
            let Some(close) = rest.find('"') else {
                continue;
            };
            out.push((rest[..close].to_string(), true));
        } else {
            let name = line.split_whitespace().next().unwrap_or_default();
            if !name.is_empty() {
                out.push((name.to_string(), false));
            }
        }
    }
    assert!(
        out.len() > 50,
        "разбор extern-блока дал {} терминалов — похоже, форма файла изменилась",
        out.len()
    );
    out
}

/// Убирает строчный комментарий: терминал, упомянутый **только** в комментарии, живым
/// не считается.
///
/// Не декоративная предосторожность: комментарий, называющий `-->` как эталон
/// подсветки, в проекте уже есть.
fn strip_comment(line: &str) -> &str {
    match line.find("//") {
        Some(i) => &line[..i],
        None => line,
    }
}

/// Тело правил без комментариев.
fn rules_without_comments() -> String {
    let (rules, _) = split_grammar();
    rules
        .lines()
        .map(strip_comment)
        .collect::<Vec<_>>()
        .join("\n")
}

/// **Класс A.** Каждое ключевое слово лексера обязано быть терминалом
/// грамматики.
///
/// Нарушение означает, что слово занято языком, но употребить его нельзя: автор видит
/// его в автодополнении и получает `SY-002`.
#[test]
fn every_keyword_is_a_grammar_terminal() {
    // Только квотированные терминалы. Именованные (`identifier`, `number`, `string`,
    // `duration`, ...) - категории токенов, а не ключевые слова, и совпадение имени
    // обманчиво: терминал `string` в extern-блоке означает `Token::StringLiteral` -
    // строковый литерал, а не слово `string`. Мутационная проверка (критерий A7 анализа
    // 0201) поймала здесь ложное "зелено" именно на этой паре.
    let declared: BTreeSet<String> = extern_terminals()
        .into_iter()
        .filter(|(_, quoted)| *quoted)
        .map(|(t, _)| t)
        .collect();

    let orphans: Vec<&str> = all_keywords()
        .filter(|kw| !declared.contains(*kw))
        .collect();

    assert!(
        orphans.is_empty(),
        "Ключевые слова лексера, которых НЕ ЗНАЕТ грамматика: {orphans:?}\n\
         Такое слово нельзя употребить — оно даёт SY-002 «нераспознанный \
         токен», при этом занимая место в таблице KEYWORDS и во всех списках, \
         которые по ней выравниваются (LSP, плагин IntelliJ, подсветка \
         документа).\n\
         Либо задействуйте слово в grammar.lalrpop, либо изымите его из \
         KEYWORDS: имя типа ключевым словом быть НЕ обязано — `bit`, `bool`, \
         `u8` и `q` им не являются."
    );
}

/// **Класс B.** Каждый терминал extern-блока обязан использоваться хотя бы
/// одним правилом.
///
/// Неиспользуемый терминал - не "надгробие" изъятой конструкции: замер показал, что на
/// текст диагностики он не влияет, а вторую, каскадную диагностику порождает.
#[test]
fn every_grammar_terminal_is_used_by_a_rule() {
    let rules = rules_without_comments();
    // Слова правил - для терминалов, пишущихся без кавычек (`identifier`).
    let words: BTreeSet<&str> = rules
        .split(|c: char| !(c.is_alphanumeric() || c == '_'))
        .filter(|w| !w.is_empty())
        .collect();

    let unused: Vec<String> = extern_terminals()
        .into_iter()
        .filter(|(terminal, quoted)| {
            if *quoted {
                !rules.contains(&format!("\"{terminal}\""))
            } else {
                !words.contains(terminal.as_str())
            }
        })
        .map(|(terminal, _)| terminal)
        .collect();

    assert!(
        unused.is_empty(),
        "Терминалы, объявленные в extern-блоке, но не использованные ни одним \
         правилом: {unused:?}\n\
         Объявление не делает отказ на изъятой конструкции внятнее (замер \
         фичи 0201: сообщение байт-в-байт то же), зато добавляет вторую, \
         КАСКАДНУЮ диагностику — следствие первой ошибки, а не второй дефект.\n\
         Удалите терминал из extern-блока."
    );
}

/// Лексемы, изъятые, не вернулись.
///
/// Сверки выше - общие: они ловят класс. Этот тест держит **конкретный** разбор, чтобы
/// возврат слова читался в диффе как осознанное решение, а не как случайная правка
/// таблицы.
#[test]
fn lexemes_withdrawn_by_0201_stay_withdrawn() {
    let keywords: BTreeSet<&str> = all_keywords().collect();
    for word in ["string", "template", "pragma"] {
        assert!(
            !keywords.contains(word),
            "'{word}' вернулось в KEYWORDS. Грамматика его не использует, то \
             есть употребление даёт SY-002. Имя типа ключевым словом быть не \
             обязано: `string` вводится типом одной строкой в \
             `builtin_type_by_name`, как `duration` в 0134."
        );
    }

    let declared: BTreeSet<String> = extern_terminals().into_iter().map(|(t, _)| t).collect();
    for terminal in ["==", "-->"] {
        assert!(
            !declared.contains(terminal),
            "терминал '{terminal}' вернулся в extern-блок грамматики"
        );
    }
}

/// Строковый **литерал** изъятием слова `string` не задет.
///
/// Соседство `Token::String` (изъят) и `Token::StringLiteral` (живой) - главный
/// источник ошибки при этой правке: литералы несут пути `import`, аргумент `debug` и
/// диалекты `assembly`/`formula`.
#[test]
fn string_literals_survive() {
    let (rules, extern_block) = split_grammar();
    assert!(
        extern_block.contains("Token::StringLiteral"),
        "терминал строкового литерала пропал из extern-блока"
    );
    assert!(
        rules.contains("ImportPath::Filename"),
        "путь `import \"…\"` больше не разбирается строковым литералом"
    );
}
