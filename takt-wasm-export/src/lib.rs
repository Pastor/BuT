//! Модуль экспорта для браузера: картинки листов схемы и видео прогона.
//!
//! # Предмет
//!
//! Растеризатор, вшитые шрифты и кодировщики видео нужны одной кнопке "Экспорт",
//! а ядро (`takt-wasm`: компилятор, слой LSP, эталон) грузится при каждом открытии
//! страницы. Поэтому экспорт - свой модуль: поток прогона грузит его при первом
//! экспорте по адресу из той же описи выкладки, что и ядро, и держит в кеше, как
//! ядро.
//!
//! # Протокол
//!
//! Тот же, что у ядра, и одной копией (`takt-wasm-io`): буфер ввода-вывода, JSON
//! запроса и ответа, ответ всегда с полем `ok`. Своего знания о языке и рисунке
//! здесь нет: экспорт - `takt_sim::export`, тот же, что у `takt-sim export`.
//!
//! Единственный `unsafe` крейта - атрибуты `#[unsafe(no_mangle)]`, без которых символы
//! не экспортируются (edition 2024).

#![allow(unsafe_code)]

pub mod export;

use serde::Deserialize;
use takt_wasm_io::{call, reply, write_reply};

/// Адрес буфера ввода-вывода; перечитывать после каждой операции.
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

/// Версия языка модуля: модули одной выкладки собраны из одного дерева, и версия у
/// них одна.
#[unsafe(no_mangle)]
pub extern "C" fn takt_version() -> u32 {
    write_reply(&reply::ok(serde_json::json!({
        "language": takt_lang::LANGUAGE_VERSION,
        "languages": takt_wasm_io::languages(),
    })))
}

/// Экспорт проекта: картинки листов и видео прогона; ответ несёт файлы строкой
/// base64.
#[unsafe(no_mangle)]
pub extern "C" fn takt_export(len: u32) -> u32 {
    call(len, |r: export::ExportRequest| export::run(&r))
}

/// Запрос рисунка схемы: текст модели и файла раскладки.
#[derive(Debug, Deserialize)]
struct SchemeRequest {
    source: String,
    layout: String,
    /// Такт прогона: с ним ответ несёт и подсветку каждого листа.
    #[serde(default)]
    tick: Option<takt_scheme::run::Tick>,
}

/// Рисунок всех листов в числах - тот, что берёт чертёж.
///
/// Его зовёт сверка паритета холста с чертежом: геометрия живёт в двух языках, и
/// расхождение иначе дошло бы до картинки молча.
#[unsafe(no_mangle)]
pub extern "C" fn takt_scheme_geometry(len: u32) -> u32 {
    call(
        len,
        |r: SchemeRequest| match takt_scheme::drawn::geometry_json(
            &r.source,
            &r.layout,
            r.tick.as_ref(),
        ) {
            Ok(json) => {
                let sheets: serde_json::Value = serde_json::from_str(&json).unwrap_or_default();
                reply::ok(serde_json::json!({ "sheets": sheets }))
            }
            Err(message) => reply::refused(message),
        },
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::Value;

    fn round_trip(request: Value, operation: extern "C" fn(u32) -> u32) -> Value {
        let len = takt_wasm_io::put_request(&request.to_string());
        serde_json::from_str(&takt_wasm_io::take_reply(operation(len))).expect("ответ — JSON")
    }

    /// Экспорт отдаёт байты тех же картинок, что носитель, архив - только для
    /// нескольких файлов; без раскладки - отказ словами.
    #[test]
    fn export_returns_pictures_and_refuses_without_a_layout() {
        use base64::Engine as _;
        let files = serde_json::json!({
            "m.takt": "start A {\n    ref B;\n}\nstate B;\n",
            "m.takt-ui": "{\"format\":1,\"sheets\":{\"/\":{\"nodes\":{\"A\":{\"x\":72,\"y\":72},\"B\":{\"x\":72,\"y\":240}}}}}",
        });
        let reply = round_trip(
            serde_json::json!({ "files": files, "formats": ["svg", "png"] }),
            takt_export,
        );
        assert_eq!(reply["ok"], Value::Bool(true), "{reply}");
        assert_eq!(
            reply["names"],
            serde_json::json!(["m.draft.svg", "m.draft.png"])
        );
        let svg = base64::engine::general_purpose::STANDARD
            .decode(reply["files"][0]["data"].as_str().unwrap())
            .unwrap();
        assert!(String::from_utf8(svg).unwrap().starts_with("<svg"), "SVG");

        let zipped = round_trip(
            serde_json::json!({ "files": files, "formats": ["svg", "png"], "archive": "m.zip" }),
            takt_export,
        );
        assert_eq!(zipped["files"][0]["name"], Value::String("m.zip".into()));
        let single = round_trip(
            serde_json::json!({ "files": files, "formats": ["svg"], "archive": "m.zip" }),
            takt_export,
        );
        assert_eq!(
            single["files"][0]["name"],
            Value::String("m.draft.svg".into()),
            "один файл - без архива"
        );

        let bare = round_trip(
            serde_json::json!({ "files": { "m.takt": "start A;\n" }, "formats": ["svg"] }),
            takt_export,
        );
        assert_eq!(bare["ok"], Value::Bool(false), "{bare}");
        assert!(bare.to_string().contains("раскладки нет"), "{bare}");
    }

    /// Версия модуля экспорта - та же, что у ядра: оба собраны из одного дерева.
    #[test]
    fn version_names_the_same_language() {
        let reply: Value = serde_json::from_str(&takt_wasm_io::take_reply(takt_version())).unwrap();
        assert_eq!(
            reply["language"],
            Value::String(takt_lang::LANGUAGE_VERSION.to_string())
        );
        assert_eq!(reply["languages"], serde_json::json!(["en", "ru"]));
    }
}
