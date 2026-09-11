//! Привязки Takt для браузера: компилятор, слой LSP и эталон одним модулем WebAssembly.
//!
//! # Что это и чего здесь нет
//!
//! Крейт - **мост**, а не вторая реализация. Компиляция идёт через `takt_lang::compile`
//! (та же точка выбора цели, что у `taktc`), операции редактора - через
//! `takt_lang::lsp` (тот же слой, что у `takt-lsp`), прогон - через
//! `takt_sim::runner::SimulationRunner::step` (тот же такт, на котором стоят все
//! потактовые сверки). Своего знания о языке здесь нет ни строки: заведи его - и
//! браузер начнёт расходиться с инструментами молча.
//!
//! # Протокол обмена
//!
//! Плоский C-ABI без `wasm-bindgen`, общий с модулем экспорта (`takt-wasm-export`):
//! буфер ввода-вывода, вызов операции и форма ответа живут в крейте
//! `takt-wasm-io`, здесь - только обёртки `takt_io_*` и операции ядра.
//!
//! # Ядро и модуль экспорта
//!
//! Этот модуль - ядро: то, что страница делает всегда (редактор, сборка,
//! прогон). Растеризатор, шрифты и кодировщики видео нужны одной кнопке
//! "Экспорт" и живут в модуле экспорта, который поток прогона грузит по
//! требованию.
//!
//! Единственный `unsafe` крейта - атрибуты `#[unsafe(no_mangle)]`, без которых символы
//! не экспортируются (edition 2024).

#![allow(unsafe_code)]

pub mod compile;
pub mod editor;
pub mod graph;
pub mod highlight;
pub mod project;
pub mod sim;

/// Форма ответа - общая с модулем экспорта.
pub use takt_wasm_io::reply;
use takt_wasm_io::{call, write_reply};

use serde::Deserialize;

/// Адрес буфера ввода-вывода.
///
/// Перечитывать после каждой операции: буфер растёт под ответ, и адрес меняется вместе
/// с ним.
#[unsafe(no_mangle)]
pub extern "C" fn takt_io_ptr() -> *mut u8 {
    takt_wasm_io::ptr()
}

/// Ёмкость буфера ввода-вывода в байтах.
#[unsafe(no_mangle)]
pub extern "C" fn takt_io_cap() -> u32 {
    takt_wasm_io::cap()
}

/// Гарантирует ёмкость буфера не меньше `len`; возвращает новую ёмкость.
#[unsafe(no_mangle)]
pub extern "C" fn takt_io_reserve(len: u32) -> u32 {
    takt_wasm_io::reserve(len)
}

/// Версия языка и крейтов моста.
#[unsafe(no_mangle)]
pub extern "C" fn takt_version() -> u32 {
    let reply = reply::ok(serde_json::json!({
        "language": takt_lang::LANGUAGE_VERSION,
        "takt_lang": env!("CARGO_PKG_VERSION"),
        "languages": takt_wasm_io::languages(),
        "targets": takt_lang::compile::Target::ALL
            .iter()
            .map(|t| t.name())
            .collect::<Vec<_>>(),
    }));
    write_reply(&reply)
}

/// Запрос компиляции.
#[derive(Debug, Deserialize)]
struct CompileRequest {
    /// Имя цели, как в `taktc compile -t`.
    target: String,
    /// Ключи сборки одной строкой; пусто - умолчания.
    #[serde(default)]
    args: String,
    /// Исходный текст модели.
    source: String,
    /// Имя файла, под которым компилируется исходник; пусто - умолчание моста.
    ///
    /// Имя корневой модели берётся из имени файла, поэтому оно есть **часть
    /// вывода**: у открытого проекта это имя его файла, а не имя буфера.
    #[serde(default)]
    filename: String,
    /// Состав проекта: имя файла - текст. По нему разрешается `import`; пусто -
    /// подключать нечего, и `import` кончается `SE-013`.
    #[serde(default)]
    files: std::collections::BTreeMap<String, String>,
}

/// Компилирует модель: запрос [`CompileRequest`] в буфере.
#[unsafe(no_mangle)]
pub extern "C" fn takt_compile(len: u32) -> u32 {
    call(len, |request: CompileRequest| {
        compile::compile(
            &request.target,
            &request.args,
            &request.source,
            &request.filename,
            request.files,
        )
    })
}

/// Запрос проверки ключей сборки.
#[derive(Debug, Deserialize)]
struct FlagsRequest {
    /// Имя цели, как в `taktc compile -t`.
    target: String,
    /// Ключи сборки одной строкой; пусто - умолчания.
    #[serde(default)]
    args: String,
}

/// Проверяет цель и ключи сборки, ничего не компилируя.
///
/// Операция заведена ради сервера: цель и ключи стали свойством
/// проекта, и негодные отвергаются при записи. Отвечает тот же разбор, что у сборки, -
/// второго списка ключей в проекте нет.
#[unsafe(no_mangle)]
pub extern "C" fn takt_flags(len: u32) -> u32 {
    call(len, |r: FlagsRequest| compile::check(&r.target, &r.args))
}

/// Запрос, которому нужен только текст документа.
#[derive(Debug, Deserialize)]
struct SourceRequest {
    source: String,
}

/// Запрос диагностик: текст документа и состав проекта для `import`.
#[derive(Debug, Deserialize)]
struct DiagnosticsRequest {
    source: String,
    #[serde(default)]
    files: std::collections::BTreeMap<String, String>,
}

/// Запрос подсветки вывода цели.
#[derive(Debug, Deserialize)]
struct HighlightRequest {
    /// Имя цели, как в `taktc compile -t`: язык вывода знает она.
    target: String,
    /// Текст порождённого файла.
    text: String,
}

/// Запрос с позицией курсора (строка и колонка с нуля, как в LSP).
#[derive(Debug, Deserialize)]
struct PositionRequest {
    source: String,
    line: u32,
    character: u32,
}

/// Запрос переименования.
#[derive(Debug, Deserialize)]
struct RenameRequest {
    source: String,
    line: u32,
    character: u32,
    new_name: String,
}

/// Диагностики документа.
#[unsafe(no_mangle)]
pub extern "C" fn takt_diagnostics(len: u32) -> u32 {
    call(len, |r: DiagnosticsRequest| {
        editor::diagnostics(&r.source, r.files)
    })
}

/// Семантические токены (подсветка).
#[unsafe(no_mangle)]
pub extern "C" fn takt_tokens(len: u32) -> u32 {
    call(len, |r: SourceRequest| editor::tokens(&r.source))
}

/// Подсветка вывода цели: те же пятёрки, что у [`takt_tokens`].
#[unsafe(no_mangle)]
pub extern "C" fn takt_highlight(len: u32) -> u32 {
    call(len, |r: HighlightRequest| {
        highlight::highlight(&r.target, &r.text)
    })
}

/// Подсказка при наведении.
#[unsafe(no_mangle)]
pub extern "C" fn takt_hover(len: u32) -> u32 {
    call(len, |r: PositionRequest| {
        editor::hover(&r.source, r.line, r.character)
    })
}

/// Переход к объявлению.
#[unsafe(no_mangle)]
pub extern "C" fn takt_goto(len: u32) -> u32 {
    call(len, |r: PositionRequest| {
        editor::goto(&r.source, r.line, r.character)
    })
}

/// Использования символа.
#[unsafe(no_mangle)]
pub extern "C" fn takt_references(len: u32) -> u32 {
    call(len, |r: PositionRequest| {
        editor::references(&r.source, r.line, r.character)
    })
}

/// Переименование символа.
#[unsafe(no_mangle)]
pub extern "C" fn takt_rename(len: u32) -> u32 {
    call(len, |r: RenameRequest| {
        editor::rename(&r.source, r.line, r.character, &r.new_name)
    })
}

/// Форматирование документа.
#[unsafe(no_mangle)]
pub extern "C" fn takt_format(len: u32) -> u32 {
    call(len, |r: SourceRequest| editor::format(&r.source))
}

/// Структура документа.
#[unsafe(no_mangle)]
pub extern "C" fn takt_symbols(len: u32) -> u32 {
    call(len, |r: SourceRequest| editor::symbols(&r.source))
}

/// Автодополнение.
#[unsafe(no_mangle)]
pub extern "C" fn takt_completion(len: u32) -> u32 {
    call(len, |r: SourceRequest| editor::completion(&r.source))
}

/// Граф модели для схемы: листы, узлы, рёбра, ярусы.
#[unsafe(no_mangle)]
pub extern "C" fn takt_graph(len: u32) -> u32 {
    call(len, |r: SourceRequest| graph::graph(&r.source))
}

/// Запрос открытия прогона.
#[derive(Debug, Deserialize)]
struct SimOpenRequest {
    source: String,
    /// Сценарий входов (JSON той же формы, что файл `-s` у `takt-sim`).
    #[serde(default)]
    scenario: String,
    /// Период модельного такта в миллисекундах; `0` - как в CLI.
    #[serde(default)]
    tick_ms: i64,
    /// Состав проекта для `import`.
    #[serde(default)]
    files: std::collections::BTreeMap<String, String>,
    /// Длина прогона в тактах - ключ `-n` у `takt-sim`; нет - длину задаёт сценарий.
    #[serde(default)]
    steps: Option<usize>,
}

/// Запрос такта прогона.
#[derive(Debug, Deserialize)]
struct SimTickRequest {
    id: u32,
    /// Сколько тактов сделать за вызов - бюджет отзывчивости страницы.
    budget: u32,
}

/// Запрос закрытия прогона.
#[derive(Debug, Deserialize)]
struct SimCloseRequest {
    id: u32,
}

/// Открывает прогон модели.
#[unsafe(no_mangle)]
pub extern "C" fn takt_sim_open(len: u32) -> u32 {
    call(len, |r: SimOpenRequest| {
        sim::open(&r.source, &r.scenario, r.tick_ms, r.files, r.steps)
    })
}

/// Делает такты открытого прогона.
#[unsafe(no_mangle)]
pub extern "C" fn takt_sim_tick(len: u32) -> u32 {
    call(len, |r: SimTickRequest| sim::tick(r.id, r.budget))
}

/// Закрывает прогон.
#[unsafe(no_mangle)]
pub extern "C" fn takt_sim_close(len: u32) -> u32 {
    call(len, |r: SimCloseRequest| sim::close(r.id))
}

/// Сценарии каждой модели проекта по правилу принадлежности крейта проекта.
#[unsafe(no_mangle)]
pub extern "C" fn takt_scenarios(len: u32) -> u32 {
    call(len, |r: project::ScenariosRequest| project::scenarios(&r))
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::Value;

    /// Кладёт запрос в буфер, зовёт операцию, читает ответ - как это делает страница.
    fn round_trip(request: Value, operation: extern "C" fn(u32) -> u32) -> Value {
        let len = takt_wasm_io::put_request(&request.to_string());
        let answer = takt_wasm_io::take_reply(operation(len));
        serde_json::from_str(&answer).expect("ответ — JSON")
    }

    /// Круговой рейс через буфер: запрос JSON -> ответ JSON.
    #[test]
    fn buffer_round_trip_compiles() {
        let reply = round_trip(
            serde_json::json!({
                "target": "c",
                "args": "heater.takt",
                "source": "start S;\n",
            }),
            takt_compile,
        );
        assert_eq!(reply["ok"], Value::Bool(true), "{reply}");
        assert_eq!(reply["files"][0]["name"], Value::String("heater.h".into()));
    }

    /// Ответ больше начальной ёмкости буфер растит, а не обрезает.
    ///
    /// Тест против молчаливой потери хвоста: обрезанный JSON не разберётся у
    /// страницы, а обрезанный вывод цели разберётся - и покажет не тот код.
    #[test]
    fn reply_larger_than_buffer_grows_it() {
        // Модель с сотней состояний: вывод цели `c` заведомо больше 64 КиБ.
        let mut source = String::from(
            "var n: u32 := 0;\n\nstart S0 {\n    always { n := n + 1; }\n    ref S1: 1 = 1;\n}\n",
        );
        for i in 1..400 {
            source.push_str(&format!(
                "state S{i} {{\n    always {{ n := n + {i}; }}\n    ref S{}: 1 = 1;\n}}\n",
                i + 1
            ));
        }
        source.push_str("state S400;\n");
        let reply = round_trip(
            serde_json::json!({"target": "c", "args": "big.takt", "source": source}),
            takt_compile,
        );
        assert_eq!(reply["ok"], Value::Bool(true), "{reply}");
        let text = reply["files"][1]["text"].as_str().unwrap();
        assert!(
            text.len() > takt_wasm_io::IO_INITIAL_CAP,
            "ожидался ответ крупнее начального буфера, получено {} байт",
            text.len()
        );
        assert!(takt_io_cap() as usize >= text.len(), "буфер обязан вырасти");
    }

    /// Битый запрос - отказ вызова, а не паника.
    #[test]
    fn broken_request_is_refused() {
        let len = takt_wasm_io::put_request("{ это не json");
        let answer = takt_wasm_io::take_reply(takt_compile(len));
        let reply: Value = serde_json::from_str(&answer).unwrap();
        assert_eq!(reply["ok"], Value::Bool(false), "{reply}");
    }

    /// Пары "модель - сценарии": самая длинная основа забирает свои сценарии.
    #[test]
    fn scenarios_follow_the_longest_stem() {
        let reply = round_trip(
            serde_json::json!({ "names": [
                "elevator.takt", "elevator_mini.takt", "elevator_rush.json",
                "elevator_mini_floor2.json", "notes.md",
                "model.takt", "model-scenario-1.json"
            ] }),
            takt_scenarios,
        );
        // Сценарий, заведённый на странице через дефис, - тоже сценарий модели.
        assert_eq!(
            reply["scenarios"]["model.takt"],
            serde_json::json!(["model-scenario-1.json"])
        );
        assert_eq!(
            reply["scenarios"]["elevator.takt"],
            serde_json::json!(["elevator_rush.json"])
        );
        assert_eq!(
            reply["scenarios"]["elevator_mini.takt"],
            serde_json::json!(["elevator_mini_floor2.json"])
        );
    }

    /// Версия называет язык, крейт и список целей.
    #[test]
    fn version_names_language_and_targets() {
        let answer = takt_wasm_io::take_reply(takt_version());
        let reply: Value = serde_json::from_str(&answer).unwrap();
        assert_eq!(
            reply["language"],
            Value::String(takt_lang::LANGUAGE_VERSION.to_string())
        );
        assert_eq!(reply["targets"].as_array().unwrap().len(), 7);
        assert_eq!(reply["languages"], serde_json::json!(["en", "ru"]));
    }
}
