//! Род файла проекта по расширению и правило имени.

use crate::error::Error;

/// Наибольшая длина имени файла, символов.
pub const NAME_CHARS: usize = 64;

/// Род файла проекта.
///
/// Род выводится из расширения, и правило одно: имя - это и есть личность файла,
/// переименования в проекте не бывает, и род не может разойтись с расширением
/// молча.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Kind {
    /// Модель на Takt.
    Takt,
    /// Сценарий входов и проверок - та же форма, что у файла `-s` эталона (шаги с
    /// `in_ports`, `time_ms`, `extern` и `guard`).
    Scenario,
    /// Пояснение к проекту на Markdown.
    Markdown,
    /// Раскладка схемы модели (`.takt-ui`), парная модели по имени файла.
    /// Компилятор её не читает, сборка от неё не зависит.
    Layout,
    /// Внешняя карта адресов портов (`.takt-map`) - формат ключа `--address-map`.
    AddressMap,
}

/// Расширения по родам. Раскладка стоит раньше модели: `.takt-ui` кончается не на
/// `.takt`, но порядок делает правило независимым от формы расширений.
const EXTENSIONS: [(&str, Kind); 5] = [
    (".takt-ui", Kind::Layout),
    (".takt-map", Kind::AddressMap),
    (".takt", Kind::Takt),
    (".json", Kind::Scenario),
    (".md", Kind::Markdown),
];

impl Kind {
    /// Имя рода в манифесте и в базе сервиса.
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Takt => "takt",
            Self::Scenario => "scenario",
            Self::Markdown => "markdown",
            Self::Layout => "layout",
            Self::AddressMap => "address_map",
        }
    }

    /// Род по имени из манифеста; `None` - имя не рода.
    pub fn parse(name: &str) -> Option<Self> {
        EXTENSIONS
            .iter()
            .map(|(_, kind)| *kind)
            .find(|kind| kind.as_str() == name)
    }

    /// Расширение рода, с точкой.
    pub fn extension(self) -> &'static str {
        EXTENSIONS
            .iter()
            .find(|(_, kind)| *kind == self)
            .map_or("", |(extension, _)| extension)
    }
}

/// Основа имени файла и его род; `None` - расширение не рода проекта.
pub fn stem_of(name: &str) -> Option<(&str, Kind)> {
    EXTENSIONS
        .iter()
        .find_map(|(extension, kind)| name.strip_suffix(extension).map(|stem| (stem, *kind)))
}

/// Проверяет имя файла и определяет его род.
///
/// Алфавит узкий: имя файла становится **именем корневой модели**, а оно попадает в
/// порождённый код - `concat.takt` даёт отказ `iec2c`, пробел или кириллица не пройдут
/// дальше первой цели. Отказать здесь дешевле, чем объяснять потом отказ чужого
/// инструмента. У пояснения и сценария алфавит тот же: имя попадает в путь архива и
/// в адрес ручки, и второе правило имени означало бы, что автор должен помнить,
/// какое из них где.
///
/// # Ошибки
/// Имя длиннее предела, чужое расширение, пустая основа, символ вне алфавита.
pub fn check_file_name(name: &str) -> Result<Kind, Error> {
    let length = name.chars().count();
    if length > NAME_CHARS {
        return Err(Error::exceeded(
            "длина имени файла в символах",
            NAME_CHARS,
            length,
        ));
    }
    let Some((stem, kind)) = stem_of(name) else {
        return Err(Error::Invalid(
            "имя файла: расширение '.takt', '.json', '.md', '.takt-ui' либо '.takt-map'"
                .to_string(),
        ));
    };
    if stem.is_empty() {
        return Err(Error::Invalid("имя файла: пустое".to_string()));
    }
    if !stem
        .chars()
        .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '-')
    {
        return Err(Error::Invalid(
            "имя файла: латинские буквы, цифры, '_' и '-'".to_string(),
        ));
    }
    Ok(kind)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn file_name_becomes_a_model_name_and_is_checked_as_one() {
        assert_eq!(check_file_name("heater.takt"), Ok(Kind::Takt));
        assert_eq!(check_file_name("board.takt-map"), Ok(Kind::AddressMap));
        assert_eq!(check_file_name("run-1.json"), Ok(Kind::Scenario));
        assert_eq!(check_file_name("readme.md"), Ok(Kind::Markdown));
        assert_eq!(
            check_file_name("heater.takt-ui"),
            Ok(Kind::Layout),
            "раскладка - свой род, а не модель"
        );
        for bad in [
            ".takt-ui",
            "модель.takt-ui",
            "модель.takt",
            "два слова.takt",
            "heater.c",
            ".takt",
            "a/b.takt",
        ] {
            assert!(
                matches!(check_file_name(bad), Err(Error::Invalid(_))),
                "{bad}"
            );
        }
        assert!(matches!(
            check_file_name(&format!("{}.takt", "x".repeat(NAME_CHARS))),
            Err(Error::Limit(_))
        ));
    }

    #[test]
    fn every_kind_makes_a_round_trip_through_its_names() {
        for (extension, kind) in EXTENSIONS {
            assert_eq!(Kind::parse(kind.as_str()), Some(kind));
            assert_eq!(kind.extension(), extension);
            assert_eq!(stem_of(&format!("x{extension}")), Some(("x", kind)));
        }
        assert_eq!(Kind::parse("image"), None);
    }
}
