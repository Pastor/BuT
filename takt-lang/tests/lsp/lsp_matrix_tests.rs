//! Сплошной перебор LSP по корпусу матрицы.
//!
//! # Что доказывает набор
//!
//! Редакторский слой обязан отвечать на **любом** входе - в том числе на том, который
//! цель переводить отказывается. Проверялся он входами отдельных фич; здесь входы
//! берутся у генератора матрицы: те же формы реализации, объявления, порты, формулы,
//! импорты, параметры, адреса и время, что проверяются у целей.
//!
//! На каждом входе шесть операций: диагностика, семантические токены, символы
//! документа, наведение, переход к объявлению и форматирование.
//!
//! Проверяется **ответ**, а не его содержимое: "hover сказал именно это" - предмет
//! отдельных наборов (`lsp_tests`, `type_highlighting_tests`). Здесь предмет один:
//! сервер **не падает** и остаётся полезным на всяком сочетании, включая семантически
//! некорректное.
//!
//! Набор идёт под `--all-features`: без флага `lsp` он не собирается вовсе.

#![cfg(feature = "lsp")]

use lsp_types::Position;
use takt_lang::lsp::{
    collect_diagnostics, document_symbols, formatting_edits, goto_declaration, hover_info,
    semantic_tokens,
};

// Генератор матрицы общий с перебором целей, и берётся он по пути: интеграционные тесты -
// разные бинарники, модуль между ними иначе не разделить. Копия генератора разошлась бы
// с оригиналом молча. `dead_code` глушится осознанно: генератор общий, а этому набору
// нужна лишь часть его API - файлы библиотек и ключи CLI спрашивает перебор целей.
#[allow(dead_code)]
#[path = "../targets/matrix_kind.rs"]
mod matrix_kind;
#[allow(dead_code)]
#[path = "../targets/matrix_probes.rs"]
mod matrix_probes;

use matrix_probes::{case_name, cases, source};

/// Позиции, на которых спрашивают наведение и переход: начало каждой строки плюс
/// середина - так обходятся и объявления, и тела, и пустые места.
fn probe_positions(text: &str) -> Vec<Position> {
    let mut out = Vec::new();
    for (line, content) in text.lines().enumerate() {
        let line = u32::try_from(line).unwrap_or(0);
        out.push(Position { line, character: 0 });
        let middle = u32::try_from(content.chars().count() / 2).unwrap_or(0);
        out.push(Position {
            line,
            character: middle,
        });
    }
    out
}

/// Сервер отвечает на всяком входе матрицы и не падает.
#[test]
fn lsp_answers_on_every_matrix_case() {
    let all = cases();
    let mut failures: Vec<String> = Vec::new();
    for (shape, touch, kind) in &all {
        let name = case_name(*shape, *touch, *kind);
        let text = source(*shape, *touch, *kind);

        // Диагностика: молчание на корректном входе - тоже ответ, поэтому проверяется
        // не пустота, а отсутствие паники и разумность координат.
        for diagnostic in collect_diagnostics(&text) {
            let range = diagnostic.range;
            if range.start.line > range.end.line {
                failures.push(format!(
                    "{name}: диагностика с перевёрнутым диапазоном: {range:?}"
                ));
            }
        }

        // Семантические токены: разбор исчерпывающий (новый токен валит сборку), здесь
        // проверяется, что они вообще выданы.
        let tokens = semantic_tokens(&text);
        if tokens.data.is_empty() {
            failures.push(format!("{name}: семантических токенов нет вовсе"));
        }

        // Символы документа: у файла с моделью символ обязан быть.
        if document_symbols(&text).is_empty() {
            failures.push(format!("{name}: документ без единого символа"));
        }

        // Наведение и переход: ответ может быть пустым (позиция вне узла), но паники
        // быть не должно ни на одной позиции.
        for position in probe_positions(&text) {
            let _ = hover_info(&text, position);
            let _ = goto_declaration(&text, position);
        }

        // Форматирование через LSP - тот же канон, что у `taktc fmt`. `Ok(None)` значит
        // "файл уже в каноне" - это ответ, а не отказ.
        if let Err(err) = formatting_edits(&text) {
            failures.push(format!("{name}: LSP не смог отформатировать вход: {err:?}"));
        }
    }
    assert!(
        failures.is_empty(),
        "LSP разошёлся с ожиданием в {} случаях из {}:\n{}",
        failures.len(),
        all.len(),
        failures.join("\n")
    );
}

/// Сервер остаётся полезным на **неполном** файле: правило редакторского слоя.
///
/// Редактор зовёт LSP на каждом нажатии, и половина этих вызовов приходится на файл,
/// который ещё не дописан. Диагностика обязана прийти с координатой, а прочие операции -
/// не паниковать.
#[test]
fn lsp_survives_incomplete_files() {
    let broken = [
        "model Wrap {",
        "model Wrap {\n    var k: u8 :=",
        "model Wrap {\n    start Go {\n        always {",
        ": [LTL] G ",
        "import ",
    ];
    for text in broken {
        let diagnostics = collect_diagnostics(text);
        assert!(
            !diagnostics.is_empty(),
            "неполный файл принят молча: {text:?}"
        );
        for diagnostic in &diagnostics {
            assert!(
                diagnostic.range.start.line <= diagnostic.range.end.line,
                "перевёрнутый диапазон на {text:?}"
            );
        }
        // Прочие операции на том же входе обязаны отвечать, а не падать.
        let _ = semantic_tokens(text);
        let _ = document_symbols(text);
        let _ = hover_info(
            text,
            Position {
                line: 0,
                character: 0,
            },
        );
    }
}
