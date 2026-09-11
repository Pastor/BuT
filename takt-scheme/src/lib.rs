//! Схема модели вне страницы: файл раскладки, геометрия, чертёж.
//!
//! Чертёж строится по графу модели (`takt_lang::layout`) и файлу раскладки `.takt-ui`,
//! который правит страница; своей раскладки у носителя нет.

pub mod drawn;
pub mod fonts;
pub mod geometry;
pub mod js;
pub mod layout;
#[cfg(feature = "video")]
pub mod mp4;
#[cfg(feature = "raster")]
pub mod raster;
pub mod run;
pub mod sheet;
pub mod style;
pub mod svg;
#[cfg(feature = "video")]
pub mod video;
