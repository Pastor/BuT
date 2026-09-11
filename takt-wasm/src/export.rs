//! Экспорт проекта и пары "модель - сценарии" для страницы.
//!
//! Мост без своего знания: проект из состава запроса строит крейт проекта (род
//! файла по расширению), картинки и видео - `takt_sim::export`, тот же, что зовёт
//! `takt-sim export`. Вывод модуля равен выводу командной строки байт в байт - это
//! сверяет проверка тождественности модуля.
//!
//! Байты картинок и видео едут в JSON строкой base64: протокол моста текстовый, а
//! второй, двоичный, ради одной операции завёл бы второй разбор ответа у страницы.

use std::collections::{BTreeMap, BTreeSet};

use base64::Engine as _;
use serde::Deserialize;
use takt_project::{Form, Manifest, Project, SourceFile, check_file_name};
use takt_scheme::raster::Background;
use takt_scheme::style::View;
use takt_sim::export::{Format, Request, export};

use crate::reply;

/// Запрос экспорта.
#[derive(Debug, Deserialize)]
pub struct ExportRequest {
    /// Состав проекта: имя файла - текст (модели, раскладки, сценарии).
    files: BTreeMap<String, String>,
    /// Активная модель проекта; нет - единственная.
    #[serde(default)]
    main_file: Option<String>,
    /// Активный сценарий проекта.
    #[serde(default)]
    main_scenario: Option<String>,
    /// Файл модели объёма; нет - картинки всех моделей, видео активной.
    #[serde(default)]
    model: Option<String>,
    /// Ключ листа; нет - картинки всех листов, видео корня.
    #[serde(default)]
    sheet: Option<String>,
    /// `draft` либо `run`; нет - чертёжный.
    #[serde(default)]
    view: Option<String>,
    /// Форматы: `svg`, `png`, `gif`, `mp4`.
    formats: Vec<String>,
    /// Фон PNG: `fill` либо `none`.
    #[serde(default)]
    background: Option<String>,
    #[serde(default = "yes")]
    legend: bool,
    /// Пауза между кадрами видео, миллисекунд; нет - умолчание.
    #[serde(default)]
    pause: Option<u32>,
    /// Сценарий прогона - файл состава.
    #[serde(default)]
    scenario: Option<String>,
    #[serde(default)]
    steps: Option<usize>,
    #[serde(default)]
    tick_ms: Option<i64>,
    /// Имя архива `.zip`: вывод из нескольких файлов уходит им одним. Один файл
    /// уходит как есть - архив из одной картинки читателю ни к чему.
    #[serde(default)]
    archive: Option<String>,
}

fn yes() -> bool {
    true
}

/// Проект из состава запроса: форма каталога, импорты - по составу.
fn project_of(request: &ExportRequest) -> Result<Project, String> {
    let mut files = Vec::new();
    for (name, text) in &request.files {
        let kind = check_file_name(name).map_err(|e| format!("'{name}': {}", e.message()))?;
        files.push(SourceFile {
            name: name.clone(),
            kind: kind.as_str().to_string(),
            text: text.clone(),
        });
    }
    Ok(Project {
        form: Form::Directory,
        root: ".".into(),
        manifest: Manifest {
            format: takt_project::FORMAT,
            main_file: request.main_file.clone(),
            main_scenario: request.main_scenario.clone(),
            ..Manifest::default()
        },
        files,
    })
}

fn request_of(r: &ExportRequest) -> Result<Request, String> {
    let view = match r.view.as_deref() {
        None | Some("") => None,
        Some("draft") => Some(View::Draft),
        Some("run") => Some(View::Run),
        Some(other) => return Err(format!("вид '{other}': draft либо run")),
    };
    let mut formats = BTreeSet::new();
    for name in &r.formats {
        formats.insert(
            Format::parse(name)
                .ok_or_else(|| format!("формат '{name}': svg, png, gif либо mp4"))?,
        );
    }
    let background = match r.background.as_deref() {
        None | Some("fill") => Background::Fill,
        Some("none") => Background::None,
        Some(other) => return Err(format!("фон '{other}': fill либо none")),
    };
    Ok(Request {
        view,
        formats,
        background,
        legend: r.legend,
        model: r.model.clone(),
        sheet: r.sheet.clone(),
        pause_ms: r.pause.unwrap_or(takt_sim::film::PAUSE_MS),
        scenario: r.scenario.clone(),
        steps: r.steps,
        tick_ms: r.tick_ms,
        search: Vec::new(),
    })
}

/// Экспорт: файлы строкой base64 либо один архив, и замечания прогона.
pub fn run(request: &ExportRequest) -> String {
    let outcome = project_of(request).and_then(|project| {
        let wanted = request_of(request)?;
        export(&project, &wanted)
    });
    let out = match outcome {
        Ok(out) => out,
        Err(message) => return reply::refused(message),
    };
    let base64 = base64::engine::general_purpose::STANDARD;
    let names: Vec<&str> = out.files.iter().map(|f| f.name.as_str()).collect();
    if let Some(archive) = request.archive.as_ref().filter(|_| out.files.len() > 1) {
        let pairs: Vec<(String, Vec<u8>)> = out
            .files
            .iter()
            .map(|f| (f.name.clone(), f.bytes.clone()))
            .collect();
        return match takt_project::pack_files(&pairs) {
            Ok(bytes) => reply::ok(serde_json::json!({
                "files": [{ "name": archive, "data": base64.encode(bytes) }],
                "names": names,
                "notes": out.notes,
            })),
            Err(e) => reply::refused(e.message().to_string()),
        };
    }
    let files: Vec<serde_json::Value> = out
        .files
        .iter()
        .map(|f| serde_json::json!({ "name": f.name, "data": base64.encode(&f.bytes) }))
        .collect();
    reply::ok(serde_json::json!({ "files": files, "names": names, "notes": out.notes }))
}

/// Запрос пар "модель - сценарии": имена файлов проекта.
#[derive(Debug, Deserialize)]
pub struct ScenariosRequest {
    names: Vec<String>,
}

/// Сценарии каждой модели по правилу принадлежности крейта проекта: из моделей,
/// подходящих по имени, сценарий достаётся самой длинной основе.
pub fn scenarios(request: &ScenariosRequest) -> String {
    let stem = |name: &str| takt_project::stem_of(name).map(|(s, k)| (s.to_string(), k));
    let models: Vec<String> = request
        .names
        .iter()
        .filter_map(|n| match stem(n) {
            Some((s, takt_project::Kind::Takt)) => Some(s),
            _ => None,
        })
        .collect();
    let stems: Vec<&str> = models.iter().map(String::as_str).collect();
    let scenario_names = request
        .names
        .iter()
        .filter(|n| matches!(stem(n), Some((_, takt_project::Kind::Scenario))));
    let pairs: BTreeMap<String, Vec<String>> = models
        .iter()
        .map(|model| {
            (
                format!("{model}.takt"),
                takt_project::scenarios_of(
                    model,
                    scenario_names.clone().map(String::as_str),
                    &stems,
                ),
            )
        })
        .collect();
    reply::ok(serde_json::json!({ "scenarios": pairs }))
}
