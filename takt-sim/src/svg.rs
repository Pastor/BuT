//! Запись кадров симуляции в виде отдельных SVG-файлов.
//!
//! Каждый кадр сохраняется немедленно при вызове [`SvgRecorder::add_frame`]. Имена
//! файлов формируются как `<stem>-<XXXX>.svg`, где XXXX - порядковый номер кадра с
//! ведущими нулями.
//!
//! Шрифты, указанные в конфигурации (например, GOST), встраиваются в каждый SVG-файл
//! как base64-закодированные `@font-face`-правила - это обеспечивает корректное
//! отображение без установки шрифтов на просматривающей системе.

use crate::gif::FrameTiming;
use crate::graphics_config::GraphicsConfig;
use crate::unit::viewport::Viewport;
use base64::Engine as _;
use std::collections::HashSet;
use std::path::{Path, PathBuf};

// -- SVG-запись ----------------------------------------------------------------

/// Записывает кадры симуляции в отдельные SVG-файлы.
pub(crate) struct SvgRecorder {
    output_dir: PathBuf,
    stem: String,
    frame_count: usize,
    /// Предварительно построенный блок `@font-face` CSS для встраивания в каждый кадр.
    font_face_css: String,
}

impl SvgRecorder {
    /// Создаёт рекордер.
    ///
    /// При создании загружает системные шрифты и встраивает данные для всех
    /// нестандартных семейств из `config`.
    pub(crate) fn new(output_dir: PathBuf, stem: String, config: &GraphicsConfig) -> Self {
        let font_face_css = build_font_face_css(config);
        Self {
            output_dir,
            stem,
            frame_count: 0,
            font_face_css,
        }
    }

    /// Сохраняет очередной кадр как SVG-файл с встроенными шрифтами.
    pub(crate) fn add_frame(
        &mut self,
        viewport: &Viewport,
        _w: u32,
        _h: u32,
        _delay: Option<u16>,
    ) -> Result<FrameTiming, String> {
        self.frame_count += 1;
        let filename = format!("{}-{:04}.svg", self.stem, self.frame_count);
        let path = self.output_dir.join(&filename);

        let t = std::time::Instant::now();
        let svg_str = viewport_to_string(viewport)?;
        let svg_str = inject_font_faces(&svg_str, &self.font_face_css);
        let serial_ms = t.elapsed().as_millis();

        std::fs::write(&path, svg_str)
            .map_err(|e| format!("Не удалось записать SVG {}: {e}", path.display()))?;

        Ok(FrameTiming {
            serial_ms,
            parse_ms: 0,
            render_ms: 0,
            quant_ms: 0,
        })
    }

    /// Завершает запись (ничего не делает - кадры уже записаны в `add_frame`).
    pub(crate) fn save(self) -> Result<(), String> {
        Ok(())
    }
}

// -- Встраивание шрифтов -------------------------------------------------------

/// Строит блок `@font-face` CSS из нестандартных семейств конфигурации.
///
/// Шрифты ищутся через системную базу fontdb. Если шрифт не найден - пропускается без
/// ошибки (SVG просто не будет отображать его корректно на сторонних системах).
fn build_font_face_css(config: &GraphicsConfig) -> String {
    let families = collect_custom_families(config);
    if families.is_empty() {
        return String::new();
    }

    let mut db = resvg::usvg::fontdb::Database::new();
    db.load_system_fonts();

    let mut css = String::new();
    // Deduplicate по пути файла - один файл может покрывать несколько семейств.
    let mut embedded_paths: HashSet<PathBuf> = HashSet::new();

    for face in db.faces() {
        for (family, _) in &face.families {
            if families.iter().any(|f| f.eq_ignore_ascii_case(family)) {
                if let Some((data, path_opt, mime, fmt)) = font_data_from_source(&face.source) {
                    let dedup_key = path_opt.unwrap_or_else(|| {
                        // Для бинарных шрифтов используем хэш первых 64 байт как ключ.
                        PathBuf::from(format!(
                            "__binary_{:x}",
                            simple_hash(&data[..data.len().min(64)])
                        ))
                    });
                    if embedded_paths.insert(dedup_key) {
                        let b64 = base64::engine::general_purpose::STANDARD.encode(&data);
                        let style = match face.style {
                            resvg::usvg::fontdb::Style::Normal => "normal",
                            resvg::usvg::fontdb::Style::Italic => "italic",
                            resvg::usvg::fontdb::Style::Oblique => "oblique",
                        };
                        let weight = face.weight.0;
                        css.push_str(&format!(
                            "@font-face {{\n  font-family: \"{family}\";\n  font-style: {style};\n  font-weight: {weight};\n  src: url(\"data:{mime};base64,{b64}\") format('{fmt}');\n}}\n"
                        ));
                    }
                }
                break; // один face -> один раз
            }
        }
    }

    css
}

/// Извлекает байты шрифта, путь к файлу (если есть) и MIME/формат из Source.
fn font_data_from_source(
    source: &resvg::usvg::fontdb::Source,
) -> Option<(Vec<u8>, Option<PathBuf>, &'static str, &'static str)> {
    match source {
        resvg::usvg::fontdb::Source::File(path) => {
            let data = std::fs::read(path).ok()?;
            let mime = mime_for_path(path);
            let fmt = format_hint(path);
            Some((data, Some(path.clone()), mime, fmt))
        }
        resvg::usvg::fontdb::Source::Binary(arc) => {
            let data = arc.as_ref().as_ref().to_vec();
            Some((data, None, "font/ttf", "truetype"))
        }
        // SharedFile и другие варианты - пропускаем.
        #[allow(unreachable_patterns)]
        _ => None,
    }
}

/// Вставляет блок `@font-face` в первый тег `<style>` SVG-документа.
fn inject_font_faces(svg: &str, font_css: &str) -> String {
    if font_css.is_empty() {
        return svg.to_string();
    }
    // Ищем первый закрывающий `>` открывающего тега <svg ...> и вставляем <style> сразу
    // после него.
    if let Some(pos) = svg.find("><style>").or_else(|| svg.find(">\n<style>")) {
        let offset = svg[pos..].find("<style>").unwrap() + pos;
        let insert_at = offset + "<style>".len();
        let mut result = svg[..insert_at].to_string();
        result.push('\n');
        result.push_str(font_css);
        result.push_str(&svg[insert_at..]);
        result
    } else {
        // Нет <style> - вставляем перед первым закрывающим > корня.
        if let Some(pos) = svg.find('>') {
            let mut result = svg[..=pos].to_string();
            result.push_str(&format!("\n<style>\n{font_css}</style>"));
            result.push_str(&svg[pos + 1..]);
            result
        } else {
            svg.to_string()
        }
    }
}

/// Парсит строку CSS font-family и возвращает нестандартные имена.
fn collect_custom_families(config: &GraphicsConfig) -> Vec<String> {
    const GENERIC: &[&str] = &[
        "sans-serif",
        "serif",
        "monospace",
        "cursive",
        "fantasy",
        "system-ui",
        "math",
        "emoji",
    ];

    let sources = [
        config.node.text_font_family.as_str(),
        config.edge_label.font_family.as_str(),
    ];

    let mut seen = HashSet::new();
    let mut result = Vec::new();

    for src in sources {
        for part in src.split(',') {
            let name = part.trim().trim_matches('"').trim_matches('\'').trim();
            if !name.is_empty()
                && !GENERIC.iter().any(|g| g.eq_ignore_ascii_case(name))
                && seen.insert(name.to_lowercase())
            {
                result.push(name.to_string());
            }
        }
    }
    result
}

fn mime_for_path(path: &Path) -> &'static str {
    match path.extension().and_then(|e| e.to_str()) {
        Some(e) if e.eq_ignore_ascii_case("otf") => "font/otf",
        Some(e) if e.eq_ignore_ascii_case("woff") => "font/woff",
        Some(e) if e.eq_ignore_ascii_case("woff2") => "font/woff2",
        _ => "font/ttf",
    }
}

fn format_hint(path: &Path) -> &'static str {
    match path.extension().and_then(|e| e.to_str()) {
        Some(e) if e.eq_ignore_ascii_case("otf") => "opentype",
        Some(e) if e.eq_ignore_ascii_case("woff") => "woff",
        Some(e) if e.eq_ignore_ascii_case("woff2") => "woff2",
        _ => "truetype",
    }
}

fn simple_hash(data: &[u8]) -> u64 {
    let mut h: u64 = 0xcbf29ce484222325;
    for &b in data {
        h ^= b as u64;
        h = h.wrapping_mul(0x100000001b3);
    }
    h
}

// ── Вспомогательные функции ───────────────────────────────────────────────────

fn viewport_to_string(viewport: &Viewport) -> Result<String, String> {
    match viewport {
        Viewport::SVG(doc) => Ok(doc.to_string()),
    }
}

// ── Тесты ─────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_svg_recorder_save_empty_is_ok() {
        let dir = tempfile::tempdir().unwrap();
        let cfg = GraphicsConfig::default();
        let rec = SvgRecorder::new(dir.path().to_path_buf(), "test".into(), &cfg);
        assert!(rec.save().is_ok());
    }

    #[test]
    fn test_svg_recorder_frame_count_increments() {
        let dir = tempfile::tempdir().unwrap();
        let cfg = GraphicsConfig::default();
        let mut rec = SvgRecorder::new(dir.path().to_path_buf(), "model".into(), &cfg);
        assert_eq!(rec.frame_count, 0);
        let doc = svg::Document::new()
            .set("viewBox", (0, 0, 100, 100))
            .set("xmlns", "http://www.w3.org/2000/svg");
        let vp = Viewport::SVG(doc);
        rec.add_frame(&vp, 100, 100, None).unwrap();
        assert_eq!(rec.frame_count, 1);
        assert!(dir.path().join("model-0001.svg").exists());
    }

    #[test]
    fn test_svg_recorder_naming_format() {
        let dir = tempfile::tempdir().unwrap();
        let cfg = GraphicsConfig::default();
        let mut rec = SvgRecorder::new(dir.path().to_path_buf(), "stacker".into(), &cfg);
        let doc = svg::Document::new().set("xmlns", "http://www.w3.org/2000/svg");
        let vp = Viewport::SVG(doc);
        for _ in 0..3 {
            rec.add_frame(&vp, 100, 100, None).unwrap();
        }
        for i in 1..=3u32 {
            assert!(dir.path().join(format!("stacker-{i:04}.svg")).exists());
        }
    }

    #[test]
    fn test_collect_custom_families_excludes_generic() {
        let cfg = GraphicsConfig::default();
        let families = collect_custom_families(&cfg);
        // monospace, sans-serif должны быть исключены
        assert!(
            !families
                .iter()
                .any(|f| f == "monospace" || f == "sans-serif")
        );
    }

    #[test]
    fn test_inject_font_faces_into_style_block() {
        let svg = r#"<svg xmlns="http://www.w3.org/2000/svg"><style>
.foo { color: red; }
</style></svg>"#;
        let css =
            "@font-face { font-family: \"Test\"; src: url(\"data:font/ttf;base64,AA==\"); }\n";
        let result = inject_font_faces(svg, css);
        assert!(result.contains("@font-face"), "должен содержать @font-face");
        assert!(
            result.contains(".foo"),
            "оригинальный CSS должен сохраниться"
        );
    }

    #[test]
    fn test_inject_font_faces_empty_css_noop() {
        let svg = "<svg><style>.x{}</style></svg>";
        let result = inject_font_faces(svg, "");
        assert_eq!(result, svg);
    }
}
