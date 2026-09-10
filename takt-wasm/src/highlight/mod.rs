//! Подсветка вывода цели: порождённый код красится по правилам своего языка.
//!
//! # Откуда берутся слова
//!
//!   велит брать словари у библиотеки, а не писать руками, и
//! называет цену: список зарезервированных имён IEC, собранный чтением
//! стандарта, отстал на двадцать имён. После решения
//! "фронтенд без зависимостей" библиотеки-редактора у проекта нет - зато есть
//! **свои** списки, которые проект держит зелёными прогоном чужих
//! инструментов:
//!
//! | Язык | Носитель слов | Кто ещё им пользуется |
//! |---|---|---|
//! | Rust | `takt_lang::generator::keywords::RUST` | отказ `RS-004` цели `rust` |
//! | SystemVerilog | `takt_lang::generator::keywords::SV` | отказ `SV-012` цели `sv` |
//! | Structured Text | [`syntax::ST`] | сверяется с `book/st.sublime-syntax` (тест) |
//! | C | [`syntax::C`] | - |
//!
//! У C носителя в проекте нет, и словарь здесь написан руками. Это названная
//! граница, а не недосмотр: цена промаха здесь **другого класса**, чем у списка IEC.
//! Там пропущенное имя означало вывод, отвергнутый `iec2c` при нулевом коде возврата;
//! здесь - слово без цвета. Ни поведение автомата, ни валидность вывода от словаря не
//! зависят.
//!
//! # Координаты
//!
//! Колонки и длины - в **единицах UTF-16**, как во всём слое LSP (`lsp/position.rs`): в
//! комментариях порождённых файлов есть кириллица, и счёт в байтах увёл бы подсветку
//! вправо.

pub mod syntax;

use serde::Serialize;
use takt_lang::compile::Target;
use takt_lang::lsp;

use crate::reply;
use syntax::{Role, Syntax};

/// Ответ подсветки - та же форма, что у `takt_tokens`.
///
/// Одна форма на два источника не случайность: страница разбирает её одним `spans()`, и
/// добавить сюда "своё" поле значило бы завести на странице второй разворот.
#[derive(Debug, Serialize)]
struct HighlightJson {
    /// Язык, по правилам которого покрашен текст (для подписи вкладки).
    language: &'static str,
    token_types: Vec<String>,
    data: Vec<u32>,
}

/// Красит текст `text` по правилам языка, которым печатает цель `target`.
///
/// Цель, а не язык, - потому что странице известна именно она: `c` и `c-hal` печатают
/// C, `st` и `st-at` - Structured Text, и повторять это соответствие в браузере значило
/// бы завести вторую таблицу целей.
pub fn highlight(target: &str, text: &str) -> String {
    let Some(target) = Target::parse(target) else {
        return reply::refused(format!(
            "неизвестная цель '{target}'. Поддерживается: {}",
            Target::ALL
                .iter()
                .map(|t| t.name())
                .collect::<Vec<_>>()
                .join(", ")
        ));
    };
    let Some((language, syntax)) = syntax::of(target.language()) else {
        return reply::refused(format!(
            "цель '{}' печатает язык, описания подсветки для которого нет",
            target.name()
        ));
    };
    reply::ok(HighlightJson {
        language,
        token_types: lsp::SEMANTIC_TOKEN_TYPES
            .iter()
            .map(|t| t.as_str().to_string())
            .collect(),
        data: scan(syntax, text),
    })
}

/// Индекс роли в легенде слоя LSP.
///
/// Спрашивается у легенды, а не пишется числом: порядок задаёт
/// `lsp::SEMANTIC_TOKEN_TYPES`, и вписанная сюда константа разъехалась бы с ним молча -
/// страница красила бы комментарии как строки.
fn role_index(role: Role) -> u32 {
    let name = role.token_type();
    lsp::SEMANTIC_TOKEN_TYPES
        .iter()
        .position(|t| t.as_str() == name)
        .map(|i| i as u32)
        // Недостижимо: тест `every_role_is_in_the_legend` проверяет каждую роль.
        .unwrap_or(0)
}

/// Отрезок текста, которому назначен цвет.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Span {
    line: u32,
    /// Колонка в единицах UTF-16.
    column: u32,
    length: u32,
    role: Role,
}

/// Состояние разбора, переживающее конец строки.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Carry {
    /// Обычный текст.
    None,
    /// Внутри блочного комментария - индекс пары в [`Syntax::block_comments`].
    Block(usize),
}

/// Разбирает текст и отдаёт пятёрки LSP.
fn scan(syntax: &Syntax, text: &str) -> Vec<u32> {
    let mut spans: Vec<Span> = Vec::new();
    let mut carry = Carry::None;
    for (line_no, line) in text.split('\n').enumerate() {
        // `\r` снимается: файлы вывода печатаются с `\n`, но текст мог приехать ссылкой
        // из системы с CRLF, и висячий возврат каретки сдвинул бы последнюю колонку
        // строки.
        let line = line.strip_suffix('\r').unwrap_or(line);
        carry = scan_line(syntax, line, line_no as u32, carry, &mut spans);
    }
    encode(&spans)
}

/// Разбирает одну строку; возвращает состояние для следующей.
///
/// Токен не пересекает конец строки - это правило протокола LSP, и блочный комментарий
/// поэтому печатается по токену на строку.
fn scan_line(
    syntax: &Syntax,
    line: &str,
    line_no: u32,
    carry: Carry,
    out: &mut Vec<Span>,
) -> Carry {
    let bytes = line.as_bytes();
    let mut at = 0usize;
    let mut column = 0u32;
    let mut carry = carry;

    // Хвост незакрытого блочного комментария предыдущей строки.
    if let Carry::Block(index) = carry {
        let (_, close) = syntax.block_comments[index];
        match line.find(close) {
            Some(end) => {
                let stop = end + close.len();
                push(out, line_no, 0, utf16_len(&line[..stop]), Role::Comment);
                at = stop;
                column = utf16_len(&line[..stop]);
                carry = Carry::None;
            }
            None => {
                if !line.is_empty() {
                    push(out, line_no, 0, utf16_len(line), Role::Comment);
                }
                return Carry::Block(index);
            }
        }
    }

    while at < bytes.len() {
        let rest = &line[at..];
        let ch = rest.chars().next().expect("непустой остаток строки");

        // Пробелы цвета не несут.
        if ch.is_whitespace() {
            at += ch.len_utf8();
            column += ch.len_utf16() as u32;
            continue;
        }

        // Строчный комментарий - до конца строки.
        if syntax.line_comments.iter().any(|m| rest.starts_with(m)) {
            push(out, line_no, column, utf16_len(rest), Role::Comment);
            return Carry::None;
        }

        // Блочный комментарий: закрылся здесь же либо переехал на следующую строку.
        if let Some((index, (_, close))) = syntax
            .block_comments
            .iter()
            .enumerate()
            .find(|(_, (open, _))| rest.starts_with(*open))
        {
            let open_len = syntax.block_comments[index].0.len();
            match rest[open_len..].find(close) {
                Some(offset) => {
                    let stop = open_len + offset + close.len();
                    push(
                        out,
                        line_no,
                        column,
                        utf16_len(&rest[..stop]),
                        Role::Comment,
                    );
                    column += utf16_len(&rest[..stop]);
                    at += stop;
                    continue;
                }
                None => {
                    push(out, line_no, column, utf16_len(rest), Role::Comment);
                    return Carry::Block(index);
                }
            }
        }

        // Строка. Незакрытая кавычка гасится концом строки: многострочных литералов ни
        // одна из целей не печатает, а "съеденный" остаток файла был бы заметнее ошибки
        // в самой строке.
        if syntax.strings.contains(&ch) {
            let length = string_len(syntax, rest, ch);
            push(
                out,
                line_no,
                column,
                utf16_len(&rest[..length]),
                Role::String,
            );
            column += utf16_len(&rest[..length]);
            at += length;
            continue;
        }

        // Число - в том числе формы целевых языков: `16#FF`, `T#100ms`, `8'd12`,
        // `0xFFu8`, `1'b1`.
        if ch.is_ascii_digit() {
            let length = number_len(syntax, rest);
            push(
                out,
                line_no,
                column,
                utf16_len(&rest[..length]),
                Role::Number,
            );
            column += utf16_len(&rest[..length]);
            at += length;
            continue;
        }

        // Директива: `#include` у C.
        if Some(ch) == syntax.directive_prefix {
            let length = ch.len_utf8() + word_len(&rest[ch.len_utf8()..]);
            push(
                out,
                line_no,
                column,
                utf16_len(&rest[..length]),
                Role::Keyword,
            );
            column += utf16_len(&rest[..length]);
            at += length;
            continue;
        }

        // Локация прямого доступа IEC: `%QX0.1`, `%MB512` - то, что печатает `st-at`.
        if Some(ch) == syntax.location_prefix {
            let length = ch.len_utf8() + location_len(&rest[ch.len_utf8()..]);
            push(
                out,
                line_no,
                column,
                utf16_len(&rest[..length]),
                Role::Variable,
            );
            column += utf16_len(&rest[..length]);
            at += length;
            continue;
        }

        // Слово: ключевое слово, тип, именованная константа, вызов или имя.
        if is_word_start(ch) {
            let length = word_len(rest);
            let word = &rest[..length];
            // Типизированный литерал IEC (`T#100ms`, `USINT#12`) - число, а не имя: `#`
            // в нём не разделитель, а часть записи литерала.
            if syntax.hash_number && rest[length..].starts_with('#') {
                // Хвост `T#100ms` начинается с цифры: спроси о нём `word_len` - и
                // литерал распадётся надвое, `T#` и `100ms`.
                let full = length + 1 + literal_tail_len(&rest[length + 1..]);
                push(out, line_no, column, utf16_len(&rest[..full]), Role::Number);
                column += utf16_len(&rest[..full]);
                at += full;
                continue;
            }
            if let Some(role) = syntax.role_of(word) {
                push(out, line_no, column, utf16_len(word), role);
            } else if rest[length..].trim_start().starts_with('(') {
                // Имя перед скобкой - вызов. Правило дешёвое и совпадает с тем, как
                // роль `Function` понимает тема документа (`entity.name.function`).
                push(out, line_no, column, utf16_len(word), Role::Function);
            }
            column += utf16_len(word);
            at += length;
            continue;
        }

        // Знаки операций - одной группой: `:=`, `-->`, `<=`. Токен на каждый знак
        // раздул бы ответ вдвое без всякой пользы для глаза.
        if syntax.operators.contains(ch) {
            let mut length = 0usize;
            for c in rest.chars() {
                if !syntax.operators.contains(c) {
                    break;
                }
                length += c.len_utf8();
            }
            push(
                out,
                line_no,
                column,
                utf16_len(&rest[..length]),
                Role::Operator,
            );
            column += utf16_len(&rest[..length]);
            at += length;
            continue;
        }

        at += ch.len_utf8();
        column += ch.len_utf16() as u32;
    }
    carry
}

fn push(out: &mut Vec<Span>, line: u32, column: u32, length: u32, role: Role) {
    if length > 0 {
        out.push(Span {
            line,
            column,
            length,
            role,
        });
    }
}

/// Длина строкового литерала в байтах, включая обе кавычки.
fn string_len(syntax: &Syntax, rest: &str, quote: char) -> usize {
    let mut at = quote.len_utf8();
    let bytes = rest.as_bytes();
    while at < bytes.len() {
        let ch = rest[at..].chars().next().expect("непустой остаток");
        if Some(ch) == syntax.string_escape && at + ch.len_utf8() < bytes.len() {
            let next = rest[at + ch.len_utf8()..]
                .chars()
                .next()
                .expect("непустой остаток");
            at += ch.len_utf8() + next.len_utf8();
            continue;
        }
        at += ch.len_utf8();
        if ch == quote {
            return at;
        }
    }
    bytes.len()
}

/// Длина числового литерала в байтах.
fn number_len(syntax: &Syntax, rest: &str) -> usize {
    let mut at = 0usize;
    for ch in rest.chars() {
        let ok = ch.is_ascii_alphanumeric()
            || ch == '_'
            || ch == '.'
            || (syntax.hash_number && ch == '#')
            || (syntax.tick_number && ch == '\'');
        if !ok {
            break;
        }
        at += ch.len_utf8();
    }
    // Точка в конце - это разделитель (`f(1);` против `1.5`), а не часть числа.
    while rest[..at].ends_with('.') {
        at -= 1;
    }
    at
}

/// Длина слова в байтах.
fn word_len(rest: &str) -> usize {
    let mut at = 0usize;
    for ch in rest.chars() {
        if at == 0 {
            if !is_word_start(ch) {
                break;
            }
        } else if !(ch.is_alphanumeric() || ch == '_' || ch == '$') {
            break;
        }
        at += ch.len_utf8();
    }
    at
}

/// Длина хвоста типизированного литерала IEC после `#`: `100ms`, `FF`, `1010`.
fn literal_tail_len(rest: &str) -> usize {
    let mut at = 0usize;
    for ch in rest.chars() {
        if !(ch.is_ascii_alphanumeric() || ch == '_' || ch == '.') {
            break;
        }
        at += ch.len_utf8();
    }
    at
}

/// Длина локации прямого доступа после `%`: `QX0.1`, `MB512`.
fn location_len(rest: &str) -> usize {
    let mut at = 0usize;
    for ch in rest.chars() {
        if !(ch.is_ascii_alphanumeric() || ch == '.') {
            break;
        }
        at += ch.len_utf8();
    }
    at
}

fn is_word_start(ch: char) -> bool {
    ch.is_alphabetic() || ch == '_' || ch == '$'
}

/// Длина отрезка в единицах UTF-16.
fn utf16_len(s: &str) -> u32 {
    s.chars().map(|c| c.len_utf16() as u32).sum()
}

/// Складывает отрезки в пятёрки LSP: дельты от предыдущего токена.
fn encode(spans: &[Span]) -> Vec<u32> {
    let mut data = Vec::with_capacity(spans.len() * 5);
    let mut line = 0u32;
    let mut column = 0u32;
    for span in spans {
        let delta_line = span.line - line;
        let delta_start = if delta_line == 0 {
            span.column - column
        } else {
            span.column
        };
        data.extend_from_slice(&[
            delta_line,
            delta_start,
            span.length,
            role_index(span.role),
            0,
        ]);
        line = span.line;
        column = span.column;
    }
    data
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Разворачивает пятёрки обратно в отрезки - так же, как это делает страница
    /// (`bridge.spans`).
    fn marks(target: &str, text: &str) -> Vec<(u32, u32, u32, String)> {
        let reply: serde_json::Value =
            serde_json::from_str(&highlight(target, text)).expect("ответ — JSON");
        assert_eq!(reply["ok"], true, "{reply}");
        let types: Vec<String> = reply["token_types"]
            .as_array()
            .expect("легенда")
            .iter()
            .map(|t| t.as_str().expect("имя роли").to_string())
            .collect();
        let data: Vec<u32> = reply["data"]
            .as_array()
            .expect("данные")
            .iter()
            .map(|v| v.as_u64().expect("число") as u32)
            .collect();
        let mut out = Vec::new();
        let (mut line, mut column) = (0u32, 0u32);
        for chunk in data.chunks(5) {
            line += chunk[0];
            column = if chunk[0] == 0 {
                column + chunk[1]
            } else {
                chunk[1]
            };
            out.push((line, column, chunk[2], types[chunk[3] as usize].clone()));
        }
        out
    }

    fn roles(target: &str, text: &str) -> Vec<String> {
        marks(target, text).into_iter().map(|m| m.3).collect()
    }

    #[test]
    fn every_role_is_in_the_legend() {
        // Роль, которой нет в легенде слоя LSP, приехала бы к странице чужим индексом -
        // то есть чужим цветом, и молча.
        for role in Role::ALL {
            assert!(
                lsp::SEMANTIC_TOKEN_TYPES
                    .iter()
                    .any(|t| t.as_str() == role.token_type()),
                "роль {:?} не названа в легенде",
                role
            );
        }
    }

    #[test]
    fn c_output_distinguishes_keyword_number_and_comment() {
        let text = "/* шапка */\nstatic uint8_t x = 0x1F; // хвост\n";
        let got = marks("c", text);
        assert!(got.iter().any(|m| m.3 == "comment" && m.0 == 0));
        assert!(got.iter().any(|m| m.3 == "keyword"));
        assert!(got.iter().any(|m| m.3 == "type"));
        assert!(got.iter().any(|m| m.3 == "number"));
        assert!(got.iter().any(|m| m.3 == "comment" && m.0 == 1));
    }

    #[test]
    fn st_is_case_insensitive_and_knows_iec_literals() {
        // Цель печатает ключевые слова верхним регистром, а рукописный пример может
        // быть строчным: идентификаторы IEC регистронезависимы.
        assert!(roles("st", "IF x THEN").contains(&"keyword".to_string()));
        assert!(roles("st", "if x then").contains(&"keyword".to_string()));
        let got = marks("st-at", "y := T#100ms; z := 16#FF; w AT %QX0.1 : BOOL;");
        assert_eq!(
            got.iter().filter(|m| m.3 == "number").count(),
            2,
            "оба литерала IEC — числа: {got:?}"
        );
        assert!(got.iter().any(|m| m.3 == "variable"), "локация: {got:?}");
    }

    #[test]
    fn st_comment_spans_lines_but_tokens_do_not() {
        // Токен LSP не пересекает конец строки: комментарий из трёх строк - три токена,
        // иначе страница красит первую и теряет остальные.
        let got = marks("st", "(*\n * шапка\n *)\nVAR\n");
        assert_eq!(got.iter().filter(|m| m.3 == "comment").count(), 3);
        assert!(got.iter().any(|m| m.3 == "type" && m.0 == 3), "{got:?}");
    }

    #[test]
    fn sv_sized_literal_is_one_number() {
        let got = marks("sv", "assign a = 8'd12;");
        let numbers: Vec<_> = got.iter().filter(|m| m.3 == "number").collect();
        assert_eq!(numbers.len(), 1, "{got:?}");
        assert_eq!(numbers[0].2, 5, "весь литерал целиком: {got:?}");
    }

    #[test]
    fn rust_words_come_from_the_compiler_list() {
        // Слово из списка, которым цель `rust` судит имена, обязано краситься:
        // разъедься они - подсветка обещала бы другое имя, чем компилятор.
        for word in takt_lang::generator::keywords::RUST.iter().take(8) {
            let line = format!("{word} ");
            assert!(
                roles("rust", &line).contains(&"keyword".to_string()),
                "слово '{word}' не покрашено"
            );
        }
    }

    #[test]
    fn columns_are_counted_in_utf16() {
        // В комментариях порождённых файлов есть кириллица: счёт в байтах увёл бы
        // подсветку вправо на длину комментария.
        let got = marks("c", "// шапка\nint x;\n");
        let word = got.iter().find(|m| m.0 == 1).expect("вторая строка");
        assert_eq!(word.1, 0, "{got:?}");
        let got = marks("rust", "let s = \"тире\"; // хвост");
        let tail = got.iter().find(|m| m.3 == "comment").expect("комментарий");
        assert_eq!(tail.1, 16, "{got:?}");
    }

    #[test]
    fn unknown_target_is_refused_by_name() {
        let reply: serde_json::Value =
            serde_json::from_str(&highlight("verilog", "module m;")).expect("JSON");
        assert_eq!(reply["ok"], false);
        assert!(
            reply["error"]["message"]
                .as_str()
                .expect("текст")
                .contains("verilog"),
            "{reply}"
        );
    }

    #[test]
    fn each_language_distinguishes_keyword_number_and_comment() {
        // Условие приёмки. Проверяется на пробе языка, а не на выводе цели:
        // требование к выводу было бы требованием к фикстуре, а не к подсветке.
        let probes = [
            ("c", "/* шапка */ static int x = 12;"),
            ("c-hal", "// шапка\nstatic int x = 12;"),
            ("st", "(* шапка *) IF x = 12 THEN"),
            ("st-at", "(* шапка *) IF x = 12 THEN"),
            ("rust", "// шапка\nlet x = 12;"),
            ("sv", "// шапка\nassign a = 12;"),
            ("sv-mmio", "// шапка\nassign a = 12;"),
        ];
        for (target, probe) in probes {
            let got = roles(target, probe);
            for role in ["keyword", "number", "comment"] {
                assert!(
                    got.contains(&role.to_string()),
                    "цель {target}: нет роли '{role}' ({got:?})"
                );
            }
        }
        assert_eq!(
            probes.len(),
            Target::ALL.len(),
            "проба обязана быть у каждой цели"
        );
    }

    #[test]
    fn every_target_paints_its_output() {
        // Условие приёмки задачи: у каждой цели разметка непуста. Цель, которую забыли
        // завести в таблице языков, иначе показывала бы чёрный текст - и заметил бы это
        // только человек, открывший её вкладку.
        for target in Target::ALL {
            let text = "// x\nstate 1 (* y *) 12\n";
            let got = marks(target.name(), text);
            assert!(!got.is_empty(), "цель {} не красит вывод", target.name());
        }
    }
}
