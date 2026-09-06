//! Конфигурация генерации GIF-анимации симуляции.
//!
//! Все параметры вёрстки кадров (размеры, цвета, отступы, шрифты, поведение раскладки
//! графа) собраны в [`GraphicsConfig`]. Конфигурация сериализуется из/в JSON (через
//! `serde`) и загружается из файла методом [`GraphicsConfig::from_file`].
//!
//! Структура разбита на логические группы (canvas/node/edge/legend/...) - при
//! отсутствии группы или поля в JSON используются значения по умолчанию, идентичные
//! "зашитым" в коде до выноса в настройки.

use serde::Deserialize;
use std::path::Path;

// -- Режим сохранения графики --------------------------------------------------

/// Режим сохранения графики симуляции.
#[derive(Clone, Debug, Default, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum OutputMode {
    /// Анимированный GIF-файл (по умолчанию).
    #[default]
    Gif,
    /// Серия SVG-файлов (по одному на кадр).
    Svg,
}

// -- Корневая конфигурация -----------------------------------------------------

/// Полная конфигурация генерации графики симуляции.
///
/// Создаётся через [`GraphicsConfig::default`] либо загружается из JSON-файла методом
/// [`GraphicsConfig::from_file`]. Все вложенные группы помечены `#[serde(default)]`,
/// поэтому в файле допустимо указывать только те поля, которые нужно переопределить.
#[derive(Clone, Debug, Deserialize)]
#[serde(default)]
#[derive(Default)]
pub struct GraphicsConfig {
    /// Режим сохранения: "gif" (по умолчанию) или "svg".
    pub output_mode: OutputMode,
    /// Параметры холста (ширина, высота, тайминги кадров).
    pub canvas: CanvasConfig,
    /// Параметры узлов состояний.
    pub node: NodeConfig,
    /// Параметры рёбер переходов.
    pub edge: EdgeConfig,
    /// Параметры подписи к подсвеченному ребру.
    pub edge_label: EdgeLabelConfig,
    /// Параметры легенды портов/состояний/цветов.
    pub legend: LegendConfig,
    /// Параметры подсветки сработавшего перехода.
    pub highlight: HighlightConfig,
    /// Параметры заголовка с именем модели.
    pub model_name: ModelNameConfig,
    /// Параметры алгоритма раскладки графа.
    pub layout: LayoutConfig,
}

impl GraphicsConfig {
    /// Загружает конфигурацию из JSON-файла.
    ///
    /// Отсутствующие поля заменяются значениями по умолчанию. Возвращает
    /// человекочитаемое сообщение об ошибке при сбое чтения или парсинга.
    pub fn from_file(path: &Path) -> Result<Self, String> {
        let text = std::fs::read_to_string(path)
            .map_err(|e| format!("Не удалось прочитать {}: {e}", path.display()))?;
        serde_json::from_str(&text).map_err(|e| format!("Ошибка парсинга {}: {e}", path.display()))
    }
}

// -- Холст и тайминги ----------------------------------------------------------

fn default_svg_background() -> Option<String> {
    Some("white".to_string())
}

#[derive(Clone, Debug, Deserialize)]
#[serde(default)]
pub struct CanvasConfig {
    pub width: f64,
    pub height: f64,
    /// Задержка между обычными кадрами (1/100 секунды).
    pub frame_delay_cs: u16,
    /// Задержка для highlight-кадра - обычно длиннее, чтобы переход успели разглядеть.
    pub highlight_frame_delay_cs: u16,
    /// Цвет фона SVG-кадра. `None` - фон не рисуется. По умолчанию белый.
    #[serde(default = "default_svg_background")]
    pub svg_background: Option<String>,
}

impl Default for CanvasConfig {
    fn default() -> Self {
        Self {
            width: 800.0,
            height: 600.0,
            frame_delay_cs: 50,
            highlight_frame_delay_cs: 150,
            svg_background: default_svg_background(),
        }
    }
}

// -- Узлы ----------------------------------------------------------------------

#[derive(Clone, Debug, Deserialize)]
#[serde(default)]
pub struct NodeConfig {
    pub radius: f64,
    pub min_distance: f64,
    pub active_fill: String,
    pub inactive_fill: String,
    pub stroke: String,
    pub stroke_width: u32,
    pub stroke_width_active: u32,
    pub text_font_family: String,
    pub text_font_size: f64,
    pub text_color: String,
}

impl Default for NodeConfig {
    fn default() -> Self {
        Self {
            radius: 25.0,
            min_distance: 25.0,
            active_fill: "#FFE066".into(),
            inactive_fill: "#cce5ff".into(),
            stroke: "#333".into(),
            stroke_width: 2,
            stroke_width_active: 3,
            text_font_family:
                "\"GOST 2.304-81 Type A\", \"GOST type A\", \"OpenGOST Type A\", sans-serif".into(),
            text_font_size: 14.0,
            text_color: "black".into(),
        }
    }
}

// -- Рёбра ---------------------------------------------------------------------

#[derive(Clone, Debug, Deserialize)]
#[serde(default)]
pub struct EdgeConfig {
    pub arrow_size: f64,
    pub curve_coefficient: f64,
    pub stroke: String,
    pub stroke_width: f64,
    pub highlight_stroke: String,
    pub highlight_stroke_width: f64,
}

impl Default for EdgeConfig {
    fn default() -> Self {
        Self {
            arrow_size: 4.0,
            curve_coefficient: 0.10,
            stroke: "#444".into(),
            stroke_width: 2.0,
            highlight_stroke: "#FF8C00".into(),
            highlight_stroke_width: 3.5,
        }
    }
}

// -- Подпись к подсвеченному ребру ---------------------------------------------

#[derive(Clone, Debug, Deserialize)]
#[serde(default)]
pub struct EdgeLabelConfig {
    pub font_family: String,
    pub font_size: f64,
    pub char_width: f64,
    pub margin: f64,
    pub text_color: String,
    pub bg_fill: String,
    pub bg_stroke: String,
    pub bg_stroke_width: f64,
    pub bg_radius: f64,
    /// Радиусы поиска свободного места для подписи.
    pub search_radii: Vec<f64>,
    pub search_angles: usize,
    pub leader_color: String,
    pub leader_width: f64,
    pub leader_dasharray: String,
}

impl Default for EdgeLabelConfig {
    fn default() -> Self {
        Self {
            font_family:
                "\"GOST 2.304-81 Type A\", \"GOST type A\", \"OpenGOST Type A\", sans-serif".into(),
            font_size: 11.0,
            char_width: 9.0,
            margin: 3.0,
            text_color: "#222".into(),
            bg_fill: "#FFF3CD".into(),
            bg_stroke: "#FF8C00".into(),
            bg_stroke_width: 1.5,
            bg_radius: 2.0,
            search_radii: vec![0.0, 15.0, 30.0, 50.0, 70.0, 90.0, 115.0, 145.0, 180.0],
            search_angles: 16,
            leader_color: "#FF8C00".into(),
            leader_width: 1.0,
            leader_dasharray: "3,2".into(),
        }
    }
}

// -- Легенда -------------------------------------------------------------------

#[derive(Clone, Debug, Deserialize)]
#[serde(default)]
pub struct LegendConfig {
    pub width: f64,
    pub padding: f64,
    pub line_height: f64,
    pub bg_fill: String,
    pub bg_stroke: String,
    pub bg_stroke_width: f64,
    pub bg_radius: f64,
    pub font_family: String,
    pub font_size: f64,
    pub header_color: String,
    pub value_color: String,
    pub swatch_radius: f64,
    pub swatch_stroke: String,
    pub swatch_stroke_width: f64,
    pub active_label: String,
    pub inactive_label: String,
    pub state_header: String,
    pub colors_header: String,
    pub in_header: String,
    pub out_header: String,
    pub inout_header: String,
    pub vars_header: String,
}

impl Default for LegendConfig {
    fn default() -> Self {
        Self {
            width: 190.0,
            padding: 5.0,
            line_height: 15.0,
            bg_fill: "#f8f8f8".into(),
            bg_stroke: "#bbb".into(),
            bg_stroke_width: 1.0,
            bg_radius: 4.0,
            font_family: "monospace".into(),
            font_size: 10.0,
            header_color: "#555".into(),
            value_color: "#333".into(),
            swatch_radius: 4.0,
            swatch_stroke: "#555".into(),
            swatch_stroke_width: 1.0,
            active_label: "активное".into(),
            inactive_label: "неактивное".into(),
            state_header: "СОСТОЯНИЯ:".into(),
            colors_header: "ЦВЕТА:".into(),
            in_header: "IN:".into(),
            out_header: "OUT:".into(),
            inout_header: "INOUT:".into(),
            vars_header: "VARS:".into(),
        }
    }
}

// -- Подсветка перехода --------------------------------------------------------

#[derive(Clone, Debug, Deserialize)]
#[serde(default)]
pub struct HighlightConfig {
    pub border_color: String,
    pub border_width: f64,
    pub border_inset: f64,
    pub header_height: f64,
    pub header_bg_color: String,
    pub header_text_color: String,
    pub header_font_family: String,
    pub header_font_size: f64,
    pub header_font_weight: String,
}

impl Default for HighlightConfig {
    fn default() -> Self {
        Self {
            border_color: "#FF8C00".into(),
            border_width: 6.0,
            border_inset: 1.0,
            header_height: 30.0,
            header_bg_color: "#FF8C00".into(),
            header_text_color: "white".into(),
            header_font_family: "monospace".into(),
            header_font_size: 14.0,
            header_font_weight: "bold".into(),
        }
    }
}

// -- Заголовок с именем модели -------------------------------------------------

#[derive(Clone, Debug, Deserialize)]
#[serde(default)]
pub struct ModelNameConfig {
    pub font_family: String,
    pub font_size: f64,
    pub font_weight: String,
    pub color: String,
    pub y_offset: f64,
}

impl Default for ModelNameConfig {
    fn default() -> Self {
        Self {
            font_family: "monospace".into(),
            font_size: 14.0,
            font_weight: "bold".into(),
            color: "#333".into(),
            y_offset: 16.0,
        }
    }
}

// -- Алгоритм раскладки --------------------------------------------------------

#[derive(Clone, Debug, Deserialize)]
#[serde(default)]
pub struct LayoutConfig {
    /// Начальная температура имитации отжига.
    pub temperature_start: f64,
    /// Минимальная температура (точка остановки).
    pub temperature_end: f64,
    /// Коэффициент охлаждения (0 < alpha < 1).
    pub cooling_alpha: f64,
    /// Множитель числа итераций на температуру (умножается на N узлов).
    pub iterations_per_temperature_factor: usize,
    /// Вес штрафа за перекрытие узлов.
    pub weight_overlap: f64,
    /// Вес штрафа за длину рёбер.
    pub weight_length: f64,
    /// Вес штрафа за пересечение рёбер.
    pub weight_cross: f64,
    /// Штраф за одно пересечение рёбер.
    pub cross_penalty: f64,
}

impl Default for LayoutConfig {
    fn default() -> Self {
        Self {
            temperature_start: 500.0,
            temperature_end: 0.1,
            cooling_alpha: 0.995,
            iterations_per_temperature_factor: 100,
            weight_overlap: 1e6,
            weight_length: 1.0,
            weight_cross: 1.0,
            cross_penalty: 500.0,
        }
    }
}

// -- Тесты ---------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;

    #[test]
    fn test_default_config_round_trip() {
        let cfg = GraphicsConfig::default();
        // Базовая проверка: значения по умолчанию совпадают с исходными "зашитыми".
        assert_eq!(cfg.canvas.width, 800.0);
        assert_eq!(cfg.canvas.height, 600.0);
        assert_eq!(cfg.legend.width, 190.0);
        assert_eq!(cfg.node.radius, 25.0);
        assert_eq!(cfg.edge.curve_coefficient, 0.10);
    }

    #[test]
    fn test_load_partial_json_uses_defaults() {
        let mut f = tempfile::NamedTempFile::new().unwrap();
        writeln!(f, r#"{{ "canvas": {{ "width": 1024, "height": 768 }} }}"#).unwrap();
        let cfg = GraphicsConfig::from_file(f.path()).unwrap();
        assert_eq!(cfg.canvas.width, 1024.0);
        assert_eq!(cfg.canvas.height, 768.0);
        // Остальные группы получили умолчаниеы.
        assert_eq!(cfg.canvas.frame_delay_cs, 50);
        assert_eq!(cfg.legend.width, 190.0);
    }

    #[test]
    fn test_load_full_override() {
        let mut f = tempfile::NamedTempFile::new().unwrap();
        let json = r##"{
            "canvas": {"width": 640, "height": 480, "frame_delay_cs": 30, "highlight_frame_delay_cs": 200},
            "node":   {"radius": 18, "active_fill": "#abc"},
            "edge":   {"highlight_stroke": "#0f0"},
            "legend": {"width": 220, "active_label": "ON"}
        }"##;
        f.write_all(json.as_bytes()).unwrap();
        let cfg = GraphicsConfig::from_file(f.path()).unwrap();
        assert_eq!(cfg.canvas.width, 640.0);
        assert_eq!(cfg.canvas.frame_delay_cs, 30);
        assert_eq!(cfg.canvas.highlight_frame_delay_cs, 200);
        assert_eq!(cfg.node.radius, 18.0);
        assert_eq!(cfg.node.active_fill, "#abc");
        assert_eq!(cfg.edge.highlight_stroke, "#0f0");
        assert_eq!(cfg.legend.width, 220.0);
        assert_eq!(cfg.legend.active_label, "ON");
        // Остальные значения - по умолчанию.
        assert_eq!(cfg.legend.inactive_label, "неактивное");
    }

    #[test]
    fn test_load_missing_file_returns_err() {
        let res = GraphicsConfig::from_file(Path::new("/nonexistent/path/cfg.json"));
        assert!(res.is_err());
    }

    #[test]
    fn test_load_invalid_json_returns_err() {
        let mut f = tempfile::NamedTempFile::new().unwrap();
        writeln!(f, "{{ not json }}").unwrap();
        let res = GraphicsConfig::from_file(f.path());
        assert!(res.is_err());
    }

    #[test]
    fn test_output_mode_default_is_gif() {
        let cfg = GraphicsConfig::default();
        assert_eq!(cfg.output_mode, OutputMode::Gif);
    }

    #[test]
    fn test_output_mode_svg_from_json() {
        let mut f = tempfile::NamedTempFile::new().unwrap();
        writeln!(f, r#"{{ "output_mode": "svg" }}"#).unwrap();
        let cfg = GraphicsConfig::from_file(f.path()).unwrap();
        assert_eq!(cfg.output_mode, OutputMode::Svg);
        // Остальные поля - умолчание.
        assert_eq!(cfg.canvas.width, 800.0);
    }

    #[test]
    fn test_output_mode_gif_explicit_from_json() {
        let mut f = tempfile::NamedTempFile::new().unwrap();
        writeln!(f, r#"{{ "output_mode": "gif" }}"#).unwrap();
        let cfg = GraphicsConfig::from_file(f.path()).unwrap();
        assert_eq!(cfg.output_mode, OutputMode::Gif);
    }

    #[test]
    fn test_svg_background_default_is_white() {
        let cfg = GraphicsConfig::default();
        assert_eq!(cfg.canvas.svg_background, Some("white".to_string()));
    }

    #[test]
    fn test_svg_background_null_from_json() {
        let mut f = tempfile::NamedTempFile::new().unwrap();
        writeln!(f, r#"{{ "canvas": {{ "svg_background": null }} }}"#).unwrap();
        let cfg = GraphicsConfig::from_file(f.path()).unwrap();
        assert_eq!(cfg.canvas.svg_background, None);
    }

    #[test]
    fn test_svg_background_custom_color_from_json() {
        let mut f = tempfile::NamedTempFile::new().unwrap();
        writeln!(f, r##"{{ "canvas": {{ "svg_background": "#f0f0f0" }} }}"##).unwrap();
        let cfg = GraphicsConfig::from_file(f.path()).unwrap();
        assert_eq!(cfg.canvas.svg_background, Some("#f0f0f0".to_string()));
    }

    #[test]
    fn test_svg_background_absent_from_json_uses_default() {
        let mut f = tempfile::NamedTempFile::new().unwrap();
        writeln!(f, r#"{{ "canvas": {{ "width": 1024 }} }}"#).unwrap();
        let cfg = GraphicsConfig::from_file(f.path()).unwrap();
        // Поле отсутствует -> используется default_svg_background() = Some("white")
        assert_eq!(cfg.canvas.svg_background, Some("white".to_string()));
    }

    /// Все JSON-пресеты в `examples/gif-configs/` должны корректно загружаться.
    /// Запускаем по абсолютному пути от `CARGO_MANIFEST_DIR`, чтобы работало и при
    /// `cargo test` из корня workspace.
    #[test]
    fn test_load_example_presets() {
        let presets_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .join("examples")
            .join("graphics-configs");
        for name in ["default", "dark", "compact", "large", "monochrome"] {
            let path = presets_dir.join(format!("{name}.json"));
            assert!(path.exists(), "пресет не найден: {}", path.display());
            let cfg = GraphicsConfig::from_file(&path)
                .unwrap_or_else(|e| panic!("не удалось загрузить {name}: {e}"));
            // Базовая проверка корректности - все обязательные поля присутствуют.
            assert!(cfg.canvas.width > 0.0);
            assert!(cfg.canvas.height > 0.0);
            assert!(cfg.node.radius > 0.0);
        }
    }
}
