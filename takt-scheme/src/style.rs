//! Оформление чертежа: палитра, ступени толщины и кегля, гаммы.
//!
//! Числа и цвета - те же, что у листа схемы на странице (`web/static/app.css`,
//! светлая тема): чертёж обязан выглядеть как холст. Здесь они таблицей, потому что
//! чертёж строится вне браузера; сверку таблицы с `app.css` держит тест в `node`
//! (`scheme-style-tests.mjs`): смена шкалы правится в двух местах, и расхождение
//! иначе дошло бы до картинки молча.

use crate::layout::Layout;

/// Сырые цвета светлой темы (`--raw-*`).
pub struct Palette;

impl Palette {
    pub const INK: &str = "#2E332B";
    pub const INK_SOFT: &str = "#5A6154";
    pub const INK_OFF: &str = "#B4B9AF";
    pub const PAPER_RAISED: &str = "#FBFBF7";
    pub const LINE: &str = "#D7DACF";
    pub const ACCENT: &str = "#5F6E55";
    pub const ALARM: &str = "#C0705A";
    pub const ALARM_INK: &str = "#8E4A34";
    pub const ALARM_BG: &str = "#F5E7E1";
    pub const WARN: &str = "#C6A24A";
    pub const WARN_INK: &str = "#6F5713";
    pub const WARN_BG: &str = "#F6EFDC";
    pub const YES: &str = "#DFE6D5";
    pub const YES_INK: &str = "#414A39";
    pub const SHEET: &str = "#F7F7F2";
}

/// Корневой кегль страницы (`--text-root`), пикселей.
pub const ROOT_PX: f64 = 16.0;

/// Кегль ступени (`--text-xs` ... `--text-lg`), пикселей.
pub fn text_px(level: &str) -> f64 {
    ROOT_PX
        * match level {
            "xs" => 0.78,
            "sm" => 0.82,
            "lg" => 1.0,
            _ => 0.9,
        }
}

/// Доля кегля у номера нижним индексом (`--text-index`).
pub const INDEX_EM: f64 = 0.6;

/// Толщина ступени линии (`--sheet-edge`, `--sheet-node`).
pub fn stroke_px(level: &str) -> f64 {
    match level {
        "thin" => 1.0,
        "bold" => 2.5,
        _ => 1.5,
    }
}

/// Вид чертежа.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum View {
    /// Чертёжный: одни чернила, без подсветки прогона.
    Draft,
    /// Цветной: гамма холста и подсветка прогона.
    Run,
}

/// Чернила листа в данном виде.
#[derive(Debug, Clone, Copy)]
pub struct Inks {
    /// Линия ребра.
    pub edge: &'static str,
    /// Рамка узла.
    pub node_stroke: &'static str,
    /// Заливка узла.
    pub node_fill: &'static str,
    /// Знаки узла и условия.
    pub node_mark: &'static str,
    pub edge_mark: &'static str,
    /// Наконечники.
    pub arrow: &'static str,
}

impl Inks {
    /// Чернила вида: у чертёжного - одни чернила и поле листа в заливке узла
    /// (`.scheme[data-gamma="draft"]`), у цветного - роли холста.
    pub fn of(view: View) -> Self {
        match view {
            View::Draft => Self {
                edge: Palette::INK,
                node_stroke: Palette::INK,
                node_fill: Palette::SHEET,
                node_mark: Palette::INK,
                edge_mark: Palette::INK,
                arrow: Palette::INK,
            },
            View::Run => Self {
                edge: Palette::INK_SOFT,
                node_stroke: Palette::INK_SOFT,
                node_fill: Palette::PAPER_RAISED,
                node_mark: Palette::INK,
                edge_mark: Palette::INK_SOFT,
                arrow: Palette::INK_SOFT,
            },
        }
    }
}

/// Ступени вида, прочитанные из файла раскладки, - в числах.
#[derive(Debug, Clone, Copy)]
pub struct Levels {
    pub edge: f64,
    pub node: f64,
    pub state_px: f64,
    pub cond_px: f64,
    pub state_face: crate::fonts::Face,
    pub cond_face: crate::fonts::Face,
    /// Форма наконечника: `open`, `solid`, `line`.
    pub arrow: &'static str,
}

impl Levels {
    pub fn of(layout: &Layout) -> Self {
        use crate::fonts::Face;
        Self {
            edge: stroke_px(layout.view_of("edgeWidth")),
            node: stroke_px(layout.view_of("nodeWidth")),
            state_px: text_px(layout.view_of("stateSize")),
            cond_px: text_px(layout.view_of("condSize")),
            state_face: Face::of(layout.view_of("stateFont")),
            cond_face: Face::of(layout.view_of("condFont")),
            arrow: match layout.view_of("arrow") {
                "solid" => "solid",
                "line" => "line",
                _ => "open",
            },
        }
    }
}

/// Форма наконечника: путь и размер маркера (таблица `ARROWS` холста).
pub fn arrow_shape(form: &str) -> (&'static str, &'static str, f64) {
    match form {
        "solid" => ("M1 1L9 5L1 9", "z", 7.0),
        "line" => ("M2 2L9 5L2 8", "", 9.0),
        _ => ("M1 1L9 5L1 9", "", 7.0),
    }
}
