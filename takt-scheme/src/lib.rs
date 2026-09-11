//! Схема модели вне страницы: файл раскладки, геометрия, чертёж.
//!
//! Чертёж строится по графу модели (`takt_lang::layout`) и файлу раскладки `.takt-ui`,
//! который правит страница; своей раскладки у носителя нет.

pub mod drawn;
pub mod geometry;
pub mod js;
pub mod layout;
pub mod sheet;
