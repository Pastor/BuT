use crate::unit::viewport::Viewport;
use std::path::{Path, PathBuf};
use std::sync::Arc;

// -- GIF-запись ----------------------------------------------------------------

pub(crate) struct FrameTiming {
    pub serial_ms: u128,
    pub parse_ms: u128,
    pub render_ms: u128,
    pub quant_ms: u128,
}

/// Записывает кадры симуляции в анимированный GIF-файл.
///
/// Каждый кадр добавляется как RGB-изображение, растеризованное из SVG-документа через
/// resvg. Для итогового GIF применяется квантизация цвета до 256 оттенков.
pub(crate) struct GifRecorder {
    frames: Vec<RgbFrame>,
    output_path: PathBuf,
    frame_delay: u16,
    /// База шрифтов загружается один раз при создании рекордера и переиспользуется для
    /// всех кадров - повторный вызов load_system_fonts() занимал ~1-1.5 с/кадр.
    fontdb: Arc<resvg::usvg::fontdb::Database>,
}

struct RgbFrame {
    width: u16,
    height: u16,
    data: Vec<u8>,
    delay: u16,
}

impl GifRecorder {
    /// Создаёт новый рекордер и загружает системные шрифты один раз.
    ///
    /// `frame_delay` - задержка между кадрами в единицах 1/100 секунды.
    pub(crate) fn new(output_path: &Path, frame_delay: u16) -> Self {
        let mut fontdb = resvg::usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        Self {
            frames: Vec::new(),
            output_path: output_path.to_path_buf(),
            frame_delay,
            fontdb: Arc::new(fontdb),
        }
    }

    /// Добавляет кадр из SVG-документа Viewport.
    ///
    /// `delay_override` - задержка в единицах 1/100 с; `None` -> использовать
    /// `frame_delay`.
    pub(crate) fn add_frame(
        &mut self,
        viewport: &Viewport,
        width: u32,
        height: u32,
        delay_override: Option<u16>,
    ) -> Result<FrameTiming, String> {
        let t = std::time::Instant::now();
        let svg_str = viewport_to_string(viewport)?;
        let serial_ms = t.elapsed().as_millis();

        let t = std::time::Instant::now();
        let options = resvg::usvg::Options {
            fontdb: Arc::clone(&self.fontdb),
            ..Default::default()
        };
        let tree = resvg::usvg::Tree::from_str(&svg_str, &options)
            .map_err(|e| format!("Ошибка парсинга SVG: {e}"))?;
        let parse_ms = t.elapsed().as_millis();

        let t = std::time::Instant::now();
        let pixmap = render_tree(&tree, width, height)?;
        let render_ms = t.elapsed().as_millis();

        let t = std::time::Instant::now();
        let pixel_count = (width as usize) * (height as usize);
        let mut rgb = Vec::with_capacity(pixel_count * 3);
        for i in 0..pixel_count {
            let base = i * 4;
            if base + 2 < pixmap.len() {
                rgb.push(pixmap[base]);
                rgb.push(pixmap[base + 1]);
                rgb.push(pixmap[base + 2]);
            }
        }
        let quant_ms = t.elapsed().as_millis();

        self.frames.push(RgbFrame {
            width: width as u16,
            height: height as u16,
            data: rgb,
            delay: delay_override.unwrap_or(self.frame_delay),
        });

        Ok(FrameTiming {
            serial_ms,
            parse_ms,
            render_ms,
            quant_ms,
        })
    }

    /// Сохраняет все накопленные кадры в GIF-файл.
    pub(crate) fn save(self) -> Result<(), String> {
        if self.frames.is_empty() {
            return Ok(());
        }
        let file = std::fs::File::create(&self.output_path).map_err(|e| {
            format!(
                "Не удалось создать GIF {}: {}",
                self.output_path.display(),
                e
            )
        })?;
        let w = self.frames[0].width;
        let h = self.frames[0].height;
        let mut encoder = gif::Encoder::new(file, w, h, &[])
            .map_err(|e| format!("Ошибка создания GIF encoder: {e}"))?;
        encoder
            .set_repeat(gif::Repeat::Infinite)
            .map_err(|e| format!("Ошибка настройки GIF repeat: {e}"))?;

        for frame_data in self.frames {
            // speed=10: в 5-7 раз быстрее NeuQuant чем speed=1 при незначительной
            // потере качества.
            let mut frame = gif::Frame::from_rgb_speed(
                frame_data.width,
                frame_data.height,
                &frame_data.data,
                10,
            );
            frame.delay = frame_data.delay;
            encoder
                .write_frame(&frame)
                .map_err(|e| format!("Ошибка записи кадра GIF: {e}"))?;
        }
        Ok(())
    }
}

// -- Вспомогательные функции ---------------------------------------------------

fn viewport_to_string(viewport: &Viewport) -> Result<String, String> {
    match viewport {
        Viewport::SVG(doc) => Ok(doc.to_string()),
    }
}

fn render_tree(tree: &resvg::usvg::Tree, width: u32, height: u32) -> Result<Vec<u8>, String> {
    let mut pixmap = resvg::tiny_skia::Pixmap::new(width, height)
        .ok_or_else(|| "Не удалось создать Pixmap".to_string())?;
    pixmap.fill(resvg::tiny_skia::Color::WHITE);

    let scale_x = width as f32 / tree.size().width();
    let scale_y = height as f32 / tree.size().height();
    let transform = resvg::tiny_skia::Transform::from_scale(scale_x, scale_y);

    resvg::render(tree, transform, &mut pixmap.as_mut());
    Ok(pixmap.data().to_vec())
}

// -- Тесты ---------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gif_recorder_save_empty_is_ok() {
        let tmp = tempfile::NamedTempFile::new().unwrap();
        let recorder = GifRecorder::new(tmp.path(), 50);
        assert!(recorder.save().is_ok());
    }

    #[test]
    fn test_render_tree_produces_rgba() {
        let svg = r#"<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 10 10"><rect width="10" height="10" fill="red"/></svg>"#;
        let fontdb = Arc::new(resvg::usvg::fontdb::Database::new());
        let options = resvg::usvg::Options {
            fontdb,
            ..Default::default()
        };
        let tree = resvg::usvg::Tree::from_str(svg, &options).unwrap();
        let result = render_tree(&tree, 10, 10);
        assert!(result.is_ok());
        let data = result.unwrap();
        assert_eq!(data.len(), 10 * 10 * 4);
    }

    #[test]
    fn test_frame_timing_fields() {
        let t = FrameTiming {
            serial_ms: 1,
            parse_ms: 2,
            render_ms: 3,
            quant_ms: 4,
        };
        assert_eq!(t.serial_ms + t.parse_ms + t.render_ms + t.quant_ms, 10);
    }
}
