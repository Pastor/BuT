//! Растр чертежа: PNG листа и кадр видео.
//!
//! Масштаб один по обеим осям, размер - из `viewBox` чертежа (пиксель на единицу
//! листа, дробная сторона округляется вверх). Начертания растеризатор получает
//! вшитыми байтами ([`crate::fonts`]), системные шрифты не ищутся: картинка
//! одинакова на любой машине, в том числе там, где ГОСТ не установлен.

use std::sync::{Arc, OnceLock};

use resvg::tiny_skia::{Color, Pixmap, Transform};
use resvg::usvg;

use crate::fonts::Face;
use crate::style::Palette;

/// Фон картинки.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Background {
    /// Заливка цветом поля листа (`--surface-sheet` светлой темы).
    Fill,
    /// Прозрачный: пиксели вне рисунка несут нулевую альфу.
    None,
}

/// База шрифтов из вшитых начертаний; строится один раз на процесс.
fn fonts() -> Arc<usvg::fontdb::Database> {
    static DB: OnceLock<Arc<usvg::fontdb::Database>> = OnceLock::new();
    DB.get_or_init(|| {
        let mut db = usvg::fontdb::Database::new();
        for face in [Face::Gost, Face::Mono] {
            db.load_font_data(face.bytes().to_vec());
        }
        Arc::new(db)
    })
    .clone()
}

/// Разобранный чертёж: дерево растеризатора.
pub struct Tree(usvg::Tree);

impl Tree {
    /// Разбирает SVG-текст чертежа.
    ///
    /// # Ошибки
    /// Текст не является SVG.
    pub fn parse(svg: &str) -> Result<Self, String> {
        let options = usvg::Options {
            fontdb: fonts(),
            // Гарнитура без записи в чертеже - ГОСТ, как у листа по умолчанию, а не
            // системный шрифт растеризатора.
            font_family: Face::Gost.family().to_string(),
            ..usvg::Options::default()
        };
        usvg::Tree::from_str(svg, &options)
            .map(Self)
            .map_err(|e| format!("чертёж не разбирается растеризатором: {e}"))
    }

    /// Размер растра в пикселях: стороны `viewBox`, округлённые вверх.
    pub fn size(&self) -> (u32, u32) {
        let size = self.0.size();
        (
            size.width().ceil().max(1.0) as u32,
            size.height().ceil().max(1.0) as u32,
        )
    }

    /// Растр на холсте `width` на `height`: рисунок в левом верхнем углу, остальное -
    /// фон. Холст больше рисунка нужен кадрам видео: у кадров одна сторона, а
    /// строка трассы переносится по-разному от такта к такту.
    ///
    /// # Ошибки
    /// Холст нулевой либо не помещается в память.
    pub fn render(
        &self,
        width: u32,
        height: u32,
        background: Background,
    ) -> Result<Pixmap, String> {
        let mut pixmap = Pixmap::new(width, height)
            .ok_or_else(|| format!("растр {width} × {height} не создаётся"))?;
        if background == Background::Fill {
            pixmap.fill(sheet_color());
        }
        resvg::render(&self.0, Transform::identity(), &mut pixmap.as_mut());
        Ok(pixmap)
    }
}

/// Цвет поля листа из таблицы оформления.
fn sheet_color() -> Color {
    let hex = Palette::SHEET.trim_start_matches('#');
    let byte = |i: usize| u8::from_str_radix(&hex[i..i + 2], 16).unwrap_or(0xFF);
    Color::from_rgba8(byte(0), byte(2), byte(4), 0xFF)
}

/// PNG чертежа.
///
/// # Ошибки
/// Текст не разбирается растеризатором либо растр не кодируется.
pub fn png(svg: &str, background: Background) -> Result<Vec<u8>, String> {
    let tree = Tree::parse(svg)?;
    let (width, height) = tree.size();
    tree.render(width, height, background)?
        .encode_png()
        .map_err(|e| format!("PNG не кодируется: {e}"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_sheet_color_is_read_from_the_palette() {
        let c = sheet_color().to_color_u8();
        assert_eq!(
            (c.red(), c.green(), c.blue(), c.alpha()),
            (0xF7, 0xF7, 0xF2, 0xFF)
        );
    }

    #[test]
    fn a_fractional_side_is_rounded_up() {
        let tree = Tree::parse(
            r#"<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 10.5 4" width="10.5" height="4"/>"#,
        )
        .expect("svg");
        assert_eq!(tree.size(), (11, 4));
    }
}
