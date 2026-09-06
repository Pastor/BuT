//! Сервер проектов онлайн-редактора Takt.
//!
//! # Что это и чего здесь нет
//!
//! Остов: учётные записи, вход, раздача собранной статики и `/health`. Ручки проектов
//! заводит задача `09b`.
//!
//! Знания о языке Takt здесь нет ни строки: компилирует и исполняет модель браузер
//! модулем `takt-wasm`, а сервер только хранит исходники и раздаёт файлы. Заведи здесь
//! компиляцию - и появился бы второй компилятор, который разошёлся бы с браузером
//! молча.

pub mod access;
pub mod archive;
pub mod archive_api;
pub mod auth;
pub mod config;
pub mod db;
pub mod error;
pub mod files;
pub mod grants;
pub mod limits;
pub mod module;
pub mod oauth;
pub mod projects;
pub mod rate;
pub mod retention;
pub mod routes;
pub mod showcase;
pub mod store;
