//! Шрифты чертежа: вшитые начертания и ширина текста.
//!
//! Чертёж несёт шрифт с собой: SVG - в `@font-face`, растр - теми же байтами.
//! Системные шрифты не ищутся - иначе картинка зависела бы от машины. Ширина
//! подписи считается по метрикам того же файла: легенда верстается и обрезается
//! по колонке без растеризатора.

/// Гарнитура листа.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Face {
    /// Чертёжный ГОСТ 2.304-81 тип А, наклонный.
    Gost,
    /// Моноширинный Fira Code - тот же, что у кода.
    Mono,
}

/// ГОСТ 2.304-81 тип А, наклонный (`takt-scheme/fonts/`).
pub const GOST: &[u8] = include_bytes!("../fonts/GOST2.304-81TypeA-Slanted.ttf");
/// Fira Code Regular (`takt-scheme/fonts/`).
pub const MONO: &[u8] = include_bytes!("../fonts/FiraCode-Regular.ttf");

impl Face {
    /// Ступень файла раскладки (`gost`, `mono`).
    pub fn of(level: &str) -> Self {
        if level == "mono" {
            Self::Mono
        } else {
            Self::Gost
        }
    }

    /// Байты начертания.
    pub fn bytes(self) -> &'static [u8] {
        match self {
            Self::Gost => GOST,
            Self::Mono => MONO,
        }
    }

    /// Семейство - то же, что записано в файле шрифта: по нему находит начертание
    /// растеризатор, и тем же именем его объявляет `@font-face` для браузера.
    pub fn family(self) -> &'static str {
        match self {
            Self::Gost => "ГОСТ 2.304-81",
            Self::Mono => "Fira Code",
        }
    }

    /// Семейство для атрибута `font-family`: в кавычках - имя «ГОСТ 2.304-81»
    /// идентификатором CSS не является (часть начинается с цифры), и без кавычек
    /// браузер отбросил бы объявление целиком.
    pub fn css(self) -> String {
        format!("'{}'", self.family())
    }

    /// Ширина строки в пикселях при кегле `size`.
    pub fn width(self, text: &str, size: f64) -> f64 {
        let Ok(face) = ttf_parser::Face::parse(self.bytes(), 0) else {
            return 0.0;
        };
        let per_em = f64::from(face.units_per_em());
        let units: f64 = text
            .chars()
            .map(|c| {
                face.glyph_index(c)
                    .and_then(|g| face.glyph_hor_advance(g))
                    .map_or(per_em / 2.0, f64::from)
            })
            .sum();
        units * size / per_em
    }

    /// Строка, обрезанная по ширине `limit` с многоточием; укладывается - как есть.
    pub fn fit(self, text: &str, size: f64, limit: f64) -> String {
        if self.width(text, size) <= limit {
            return text.to_string();
        }
        let mut out = String::new();
        for c in text.chars() {
            let next = format!("{out}{c}…");
            if self.width(&next, size) > limit {
                break;
            }
            out.push(c);
        }
        format!("{out}…")
    }

    /// Правило `@font-face` с начертанием в base64.
    pub fn font_face(self) -> String {
        use base64::Engine as _;
        let data = base64::engine::general_purpose::STANDARD.encode(self.bytes());
        format!(
            "@font-face{{font-family:\"{}\";src:url(data:font/ttf;base64,{data}) format(\"truetype\");}}",
            self.family()
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_embedded_fonts_are_readable_and_measure_text() {
        let mono = Face::Mono.width("S1", 10.0);
        assert!(
            (mono - 2.0 * 10.0 * 1200.0 / 1950.0).abs() < 1e-9,
            "моноширинный: {mono}"
        );
        assert!(
            Face::Gost.width("Ожидание", 14.4) > 0.0,
            "кириллица в ГОСТ есть"
        );
        let cut = Face::Gost.fit("Очень длинная подпись состояния", 14.4, 80.0);
        assert!(
            cut.ends_with('…') && Face::Gost.width(&cut, 14.4) <= 80.0,
            "{cut}"
        );
        assert_eq!(Face::Gost.fit("S1", 14.4, 80.0), "S1");
    }
}
