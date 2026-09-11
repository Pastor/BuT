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

pub mod reply;

use serde::Deserialize;
use std::cell::RefCell;

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
    let request = IO.with(|io| {
        let io = io.borrow();
        let len = (len as usize).min(io.len());
        std::str::from_utf8(&io[..len])
            .map_err(|e| format!("запрос не UTF-8: {e}"))
            .and_then(|text| {
                serde_json::from_str::<T>(text).map_err(|e| format!("запрос не читается: {e}"))
            })
    });
    let answer = match request {
        Ok(request) => operation(request),
        Err(message) => reply::refused(message),
    };
    write_reply(&answer)
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
