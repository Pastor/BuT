//! Протокол обмена модулей WebAssembly со страницей: один у всех модулей.
//!
//! Модулей два - ядро (компилятор, слой LSP, эталон) и модуль экспорта, - и
//! говорят они со страницей одинаково:
//!
//! - буфер ввода-вывода: страница пишет туда UTF-8 JSON запроса и зовёт операцию с
//!   его длиной; операция кладёт JSON ответа в **тот же** буфер и возвращает его
//!   длину ([`call`]);
//! - [`reserve`] растит буфер под большой запрос; после него (и после любой
//!   операции) адрес буфера надо перечитать: `Vec` при росте переезжает;
//! - ответ всегда одной формы: `{"ok": true, ...}` либо `{"ok": false, "error":
//!   {...}}` ([`reply`]).
//!
//! Символов модуля здесь нет: каждый модуль объявляет `takt_io_ptr`,
//! `takt_io_cap`, `takt_io_reserve` своими обёртками над [`ptr`], [`cap`] и
//! [`reserve`]. Вторая копия протокола у второго модуля разошлась бы с первой
//! молча - страница говорит с обоими одним мостом.
//!
//! Модуль однопоточен, вызовы не реентерабельны - состояние живёт в
//! `thread_local`.
//!
//! Язык ответа задаёт поле `lang` запроса, одно на все операции обоих модулей
//! ([`call`]); список языков называет [`languages`].

pub mod reply;

use serde::Deserialize;
use std::cell::RefCell;
use takt_lang::diagnostics::lang::{self, Lang, keys};
use takt_lang::msg;

/// Начальная ёмкость буфера ввода-вывода.
///
/// 64 КиБ хватает запросу (исходник модели) и большинству ответов; вывод цели `c` на
/// крупной модели больше - под него страница зовёт `takt_io_reserve`.
pub const IO_INITIAL_CAP: usize = 64 * 1024;

thread_local! {
    /// Буфер ввода-вывода: сюда страница кладёт запрос, отсюда читает ответ.
    static IO: RefCell<Vec<u8>> = RefCell::new(vec![0; IO_INITIAL_CAP]);
}

/// Адрес буфера ввода-вывода.
pub fn ptr() -> *mut u8 {
    IO.with(|io| io.borrow_mut().as_mut_ptr())
}

/// Ёмкость буфера ввода-вывода в байтах.
pub fn cap() -> u32 {
    IO.with(|io| u32::try_from(io.borrow().len()).unwrap_or(u32::MAX))
}

/// Гарантирует ёмкость буфера не меньше `len`; возвращает новую ёмкость.
pub fn reserve(len: u32) -> u32 {
    IO.with(|io| {
        let mut io = io.borrow_mut();
        let need = len as usize;
        if io.len() < need {
            io.resize(need, 0);
        }
        u32::try_from(io.len()).unwrap_or(u32::MAX)
    })
}

/// Разбирает запрос из буфера, зовёт операцию и кладёт ответ обратно.
///
/// Ошибка разбора запроса - отказ **вызова** с текстом, а не паника: паника в модуле
/// есть `abort`, и страница теряет модуль целиком вместе с открытыми прогонами.
pub fn call<T, F>(len: u32, operation: F) -> u32
where
    T: for<'de> Deserialize<'de>,
    F: FnOnce(T) -> String,
{
    // Пока запрос не разобран, язык его неизвестен: отказ разбора идёт на базовом.
    let value = IO.with(|io| {
        let io = io.borrow();
        let len = (len as usize).min(io.len());
        std::str::from_utf8(&io[..len])
            .map_err(|e| unread(keys::BRIDGE_REQUEST_NOT_UTF8, &e))
            .and_then(|text| {
                serde_json::from_str::<serde_json::Value>(text)
                    .map_err(|e| unread(keys::BRIDGE_REQUEST_UNREADABLE, &e))
            })
    });
    let request = value.and_then(|value| {
        choose_language(&value)?;
        serde_json::from_value::<T>(value)
            .map_err(|e| msg!(keys::BRIDGE_REQUEST_UNREADABLE, error = e))
    });
    let answer = match request {
        Ok(request) => operation(request),
        Err(message) => reply::refused(message),
    };
    write_reply(&answer)
}

/// Отказ разбора запроса на базовом языке.
fn unread(key: lang::Key, error: &dyn std::fmt::Display) -> String {
    lang::render_in(Lang::base(), key, &[("error", error)])
}

/// Ставит язык ответа по полю `lang` запроса.
///
/// Язык ставится на каждый вызов и не запоминается: запрос без поля обязан получить
/// умолчание, иначе язык прошлого вызова отвечал бы за чужой запрос. Неизвестный код -
/// отказ вызова с перечислением известных.
fn choose_language(request: &serde_json::Value) -> Result<(), String> {
    match request.get("lang").and_then(serde_json::Value::as_str) {
        Some(code) => lang::activate(lang::parse(code)?),
        None => lang::reset(),
    }
    Ok(())
}

/// Коды языков, у которых есть каталог: их называет ответ `takt_version` каждого
/// модуля, и страница не заводит своего списка.
pub fn languages() -> Vec<&'static str> {
    lang::all().iter().map(Lang::code).collect()
}

/// Кладёт ответ в буфер, расширяя его при необходимости.
pub fn write_reply(answer: &str) -> u32 {
    IO.with(|io| {
        let mut io = io.borrow_mut();
        if io.len() < answer.len() {
            io.resize(answer.len(), 0);
        }
        io[..answer.len()].copy_from_slice(answer.as_bytes());
        u32::try_from(answer.len()).unwrap_or(u32::MAX)
    })
}

/// Кладёт запрос в буфер так, как это делает страница; ответ - длина запроса.
/// Для родных тестов модулей: страницы у них нет.
pub fn put_request(text: &str) -> u32 {
    reserve(u32::try_from(text.len()).unwrap_or(u32::MAX));
    IO.with(|io| io.borrow_mut()[..text.len()].copy_from_slice(text.as_bytes()));
    u32::try_from(text.len()).unwrap_or(u32::MAX)
}

/// Ответ длиной `len` из буфера. Для родных тестов модулей.
pub fn take_reply(len: u32) -> String {
    IO.with(|io| String::from_utf8_lossy(&io.borrow()[..len as usize]).into_owned())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Операция, отвечающая языком, на котором её позвали.
    fn answered_language(request: &str) -> serde_json::Value {
        let len = put_request(request);
        let reply = take_reply(call(len, |_: serde_json::Value| {
            reply::ok(serde_json::json!({ "lang": lang::current().code() }))
        }));
        serde_json::from_str(&reply).expect("ответ - JSON")
    }

    /// Язык задаёт запрос, и следующий запрос без поля его не наследует.
    #[test]
    fn language_is_chosen_per_call() {
        assert_eq!(answered_language(r#"{"lang": "en"}"#)["lang"], "en");
        let base = answered_language("{}");
        assert_eq!(base["lang"], lang::current().code(), "без поля - умолчание");
        assert_ne!(base["lang"], "en", "язык прошлого вызова не наследуется");
    }

    /// Неизвестный язык - отказ вызова, и он перечисляет известные.
    #[test]
    fn unknown_language_is_refused_with_the_known_ones() {
        let reply = answered_language(r#"{"lang": "xx"}"#);
        assert_eq!(reply["ok"], false, "{reply}");
        let text = reply.to_string();
        assert!(text.contains("en") && text.contains("ru"), "{text}");
    }

    /// Список языков - те же каталоги, что у компилятора.
    #[test]
    fn languages_name_the_catalogues() {
        assert_eq!(languages(), ["en", "ru"]);
    }
}
