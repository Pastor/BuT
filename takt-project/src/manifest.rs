//! Манифест проекта `takt-project.json`: метаданные и состав.

use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

/// Имя файла манифеста - в архиве и в каталоге проекта.
pub const MANIFEST: &str = "takt-project.json";

/// Каталог исходников внутри архива.
pub const SOURCES: &str = "src/";

/// Каталог порождённого вывода внутри архива.
pub const GENERATED: &str = "generated/";

/// Версия формата манифеста.
///
/// Растёт вместе с формой записи: поле, род файла, смысл поля. Читатель, встретивший
/// бо́льшую версию, **отказывает**: разобрать наполовину значит отдать автору проект,
/// про который он думает, что тот целый. Прежние версии читаются - новые поля
/// приходят пустыми (`serde(default)`), и проект получает умолчания.
///
/// - `2` - цель и ключи сборки;
/// - `3` - активный сценарий и род `markdown`;
/// - `4` - задержки прогона по сценариям;
/// - `5` - род `address_map` (карта адресов `.takt-map`).
pub const FORMAT: u32 = 5;

/// Метаданные проекта.
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct Manifest {
    /// Версия формата.
    pub format: u32,
    pub name: String,
    #[serde(default)]
    pub description: String,
    /// Версия модуля, которой открывается проект.
    #[serde(default)]
    pub takt_lang: String,
    #[serde(default)]
    pub language_version: String,
    /// Активный файл; `null` - не назначен.
    #[serde(default)]
    pub main_file: Option<String>,
    /// Активный сценарий прогона; `null` - не назначен.
    #[serde(default)]
    pub main_scenario: Option<String>,
    /// Состав исходников: имя и род.
    #[serde(default)]
    pub files: Vec<ManifestFile>,
    /// Когда выгружен, Unix-секунды.
    #[serde(default)]
    pub exported_at: i64,
    /// Какой целью собран `generated/` архива; `null` - вывода в архиве нет.
    ///
    /// Это не выбор автора: поле отвечает на вопрос "чем собран каталог
    /// `generated/`", а выбор живёт в [`Manifest::build_target`]. Поля стоят рядом и
    /// легко путаются - оттого смысл каждого назван здесь.
    #[serde(default)]
    pub generated_target: Option<String>,
    /// Цель сборки, выбранная автором; пусто - умолчание.
    #[serde(default)]
    pub build_target: String,
    /// Ключи сборки, выбранные автором; пусто - умолчания.
    #[serde(default)]
    pub build_args: String,
    /// Задержки прогона по сценариям, секунд; пусто - без задержек.
    #[serde(default)]
    pub run_delays: BTreeMap<String, f64>,
}

/// Запись состава.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ManifestFile {
    pub name: String,
    pub kind: String,
}
