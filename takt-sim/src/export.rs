//! Экспорт проекта: картинки листов (SVG, PNG) и видео прогона (GIF, MP4).
//!
//! Проект приходит составом в памяти ([`takt_project::Project`]), вывод уходит
//! парами "имя файла, байты": куда их положить, решает вызывающий - командная
//! строка пишет каталог, страница собирает архив. Рисует крейт рисунка по файлу
//! раскладки `.takt-ui` каждой модели, прогон делает эталон тем же шагом, что
//! трассу.
//!
//! Экспорт всё или ничего: нет раскладки или она неполна хотя бы у одной модели
//! объёма - отказ всего экспорта с перечнем причин. Частичный вывод прошёл бы за
//! полный.

use std::collections::BTreeSet;

use takt_lang::diagnostics::{Diagnostic, FileTable};
use takt_lang::semantic::tree::construct_model_with_files;
use takt_project::{Form, Kind, Project, stem_of};
use takt_scheme::layout::Layout;
use takt_scheme::raster::{self, Background};
use takt_scheme::sheet::DrawSheet;
use takt_scheme::style::View;
use takt_scheme::svg::{Options, file_stem, svg};

use crate::film::{Film, ROOT_SHEET};
use crate::json_input::SimStep;
use crate::port_names::PortNames;
use crate::runner::SimulationRunner;

/// Предел тактов прогона без сценария и без `-n`: модель без терминального
/// состояния иначе шла бы вечно. То же умолчание, что у бюджета прогона страницы.
pub const DEFAULT_STEPS: usize = 200;

/// Формат файла вывода.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Format {
    Svg,
    Png,
    Gif,
    Mp4,
}

impl Format {
    /// Формат по имени (`svg`, `png`, `gif`, `mp4`).
    pub fn parse(name: &str) -> Option<Self> {
        match name {
            "svg" => Some(Self::Svg),
            "png" => Some(Self::Png),
            "gif" => Some(Self::Gif),
            "mp4" => Some(Self::Mp4),
            _ => None,
        }
    }

    /// Расширение файла без точки.
    pub fn extension(self) -> &'static str {
        match self {
            Self::Svg => "svg",
            Self::Png => "png",
            Self::Gif => "gif",
            Self::Mp4 => "mp4",
        }
    }

    /// Видео ли это: видео - всегда цветной вид одного листа.
    pub fn is_video(self) -> bool {
        matches!(self, Self::Gif | Self::Mp4)
    }
}

/// Что выгрузить.
#[derive(Debug, Clone)]
pub struct Request {
    /// Вид картинок; `None` - чертёжный. Видео всегда цветное, и чертёжный вид
    /// рядом с видео - отказ, а не молчаливая подмена.
    pub view: Option<View>,
    pub formats: BTreeSet<Format>,
    /// Фон PNG. SVG фона не несёт, видео залито всегда.
    pub background: Background,
    pub legend: bool,
    /// Файл модели проекта; `None` - картинки всех моделей, видео активной.
    pub model: Option<String>,
    /// Ключ листа (`/`, `/#Line`, `Engine`); `None` - картинки всех листов, видео
    /// корня.
    pub sheet: Option<String>,
    /// Пауза между кадрами видео, миллисекунд.
    pub pause_ms: u32,
    /// Сценарий проекта; `None` - активный либо единственный сценарий модели.
    pub scenario: Option<String>,
    /// Предел тактов; `None` - длина сценария, без сценария [`DEFAULT_STEPS`].
    pub steps: Option<usize>,
    /// Период такта, миллисекунд; `None` - частота модели либо 1 мс.
    pub tick_ms: Option<i64>,
    /// Каталоги поиска импортов одной модели (`-I`); у каталога и архива импорты
    /// разрешаются по составу проекта.
    pub search: Vec<String>,
}

/// Файл вывода.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Output {
    pub name: String,
    pub bytes: Vec<u8>,
}

/// Итог экспорта: файлы и замечания прогона (нарушенная проверка сценария,
/// ошибка вычисления) - картинку они не отменяют, но промолчать о них нельзя.
#[derive(Debug, Clone, Default)]
pub struct Export {
    pub files: Vec<Output>,
    pub notes: Vec<String>,
}

/// Модель объёма: файл, его листы и раскладка.
struct Drawn {
    file: String,
    stem: String,
    sheets: Vec<DrawSheet>,
    layout: Layout,
}

/// Выгружает проект.
///
/// # Ошибки
/// Запрос противоречив (нет форматов, чертёжный вид у видео, модели или листа нет,
/// активной модели для видео не выбрать); раскладки нет либо она неполна хотя бы у
/// одной модели объёма; модель не собирается; два листа дают одно имя файла.
pub fn export(project: &Project, request: &Request) -> Result<Export, String> {
    if request.formats.is_empty() {
        return Err("не назван ни один формат".to_string());
    }
    let (videos, pictures): (BTreeSet<Format>, BTreeSet<Format>) =
        request.formats.iter().partition(|f| f.is_video());
    if !videos.is_empty() && request.view == Some(View::Draft) {
        return Err("видео - всегда цветной вид прогона: чертёжный вид здесь не годится".into());
    }
    // Картинки и видео - разный объём по умолчанию: картинки - все листы всех
    // моделей, видео - корень активной. Обе части считаются в памяти, и отказ любой
    // из них отменяет весь вывод.
    let mut out = Export::default();
    let mut names = BTreeSet::new();
    for (formats, video) in [(pictures, false), (videos, true)] {
        if !formats.is_empty() {
            part(project, request, &formats, video, &mut names, &mut out)?;
        }
    }
    Ok(out)
}

/// Часть экспорта: картинки либо видео.
fn part(
    project: &Project,
    request: &Request,
    formats: &BTreeSet<Format>,
    video: bool,
    names: &mut BTreeSet<String>,
    out: &mut Export,
) -> Result<(), String> {
    let files = scope(project, request, video)?;
    let drawn = draw_all(project, &files, request.model.is_some())?;
    let run_view = video || request.view == Some(View::Run);
    let view = if run_view { "run" } else { "draft" };
    for model in &drawn {
        let keys: Vec<String> = match (&request.sheet, video) {
            (Some(sheet), _) => vec![sheet.clone()],
            (None, true) => vec![ROOT_SHEET.to_string()],
            (None, false) => model.sheets.iter().map(|s| s.key.clone()).collect(),
        };
        for key in &keys {
            if !model.sheets.iter().any(|s| &s.key == key) {
                return Err(format!("`{}`: листа `{key}` нет", model.file));
            }
        }
        let steps = if run_view {
            Some(run(project, &model.file, request, &mut out.notes)?)
        } else {
            None
        };
        for key in &keys {
            let film = match &steps {
                Some(steps) => {
                    let mut film = Film::of_sheets(
                        model.sheets.clone(),
                        model.layout.clone(),
                        key,
                        request.legend,
                    )?;
                    for step in steps {
                        film.record(step);
                    }
                    Some(film)
                }
                None => None,
            };
            let drawing = |fonts: bool| match &film {
                Some(film) => film.last(fonts),
                None => picture(model, key, request.legend, fonts),
            };
            let stem = file_stem(key, &model.stem);
            for format in formats {
                let name = format!("{stem}.{view}.{}", format.extension());
                if !names.insert(name.clone()) {
                    return Err(format!(
                        "два листа дают одно имя файла `{name}`: выгрузите модели по одной (`--model`)"
                    ));
                }
                let bytes = match (format, &film) {
                    (Format::Svg, _) => drawing(true)?.into_bytes(),
                    (Format::Png, _) => raster::png(&drawing(false)?, request.background)?,
                    (Format::Gif, Some(film)) => {
                        let mut bytes = Vec::new();
                        film.gif(request.pause_ms, &mut bytes)?;
                        bytes
                    }
                    (Format::Mp4, Some(film)) => film.mp4(request.pause_ms)?,
                    (Format::Gif | Format::Mp4, None) => {
                        return Err("видео без прогона не пишется".to_string());
                    }
                };
                out.files.push(Output { name, bytes });
            }
        }
    }
    Ok(())
}

/// Файлы моделей объёма: названная, для видео - активная, иначе все.
fn scope(project: &Project, request: &Request, video: bool) -> Result<Vec<String>, String> {
    let models: Vec<String> = project
        .of_kind(Kind::Takt)
        .map(|f| f.name.clone())
        .collect();
    if let Some(model) = &request.model {
        if !models.contains(model) {
            return Err(format!("модели `{model}` в проекте нет"));
        }
        return Ok(vec![model.clone()]);
    }
    if video || request.sheet.is_some() {
        return main_model(project, &models).map(|m| vec![m]);
    }
    Ok(models)
}

/// Активная модель: названная манифестом либо единственная.
fn main_model(project: &Project, models: &[String]) -> Result<String, String> {
    if let Some(main) = &project.manifest.main_file
        && models.contains(main)
    {
        return Ok(main.clone());
    }
    match models {
        [only] => Ok(only.clone()),
        _ => Err("активная модель не выбрана: назовите её ключом `--model`".to_string()),
    }
}

/// Листы и раскладки моделей объёма; отказы собираются по всем моделям разом.
///
/// Модель без единого состояния - библиотека: рисовать в ней нечего, и раскладки
/// у неё нет. Названная явно, она попадает в объём как есть.
fn draw_all(project: &Project, files: &[String], named: bool) -> Result<Vec<Drawn>, String> {
    let mut drawn = Vec::new();
    let mut refusals = Vec::new();
    for file in files {
        let text = &project
            .file(file)
            .map(|f| f.text.clone())
            .unwrap_or_default();
        let graph = match takt_lang::layout::graph_of(text) {
            Ok(graph) => graph,
            Err(d) => {
                refusals.push(format!("`{file}`: {}", d.message));
                continue;
            }
        };
        if !named && graph.sheets.iter().all(|s| s.nodes.is_empty()) {
            continue;
        }
        let Some(layout_file) = project.layout_of(file) else {
            let stem = stem_of(file).map_or(file.as_str(), |(s, _)| s);
            refusals.push(format!(
                "`{file}`: раскладки нет - картинка рисуется по файлу `{stem}.takt-ui`, его пишет редактор схемы"
            ));
            continue;
        };
        let layout = match takt_scheme::layout::parse(&layout_file.text) {
            Ok(layout) => layout,
            Err(e) => {
                refusals.push(format!("`{}`: {}", layout_file.name, e.0));
                continue;
            }
        };
        match takt_scheme::sheet::sheets(&graph, &layout) {
            Ok(sheets) => drawn.push(Drawn {
                stem: stem_of(file).map_or(file.clone(), |(s, _)| s.to_string()),
                file: file.clone(),
                sheets,
                layout,
            }),
            Err(unplaced) => refusals.push(format!("`{}`: {unplaced}", layout_file.name)),
        }
    }
    if refusals.is_empty() {
        Ok(drawn)
    } else {
        Err(refusals.join("\n"))
    }
}

/// Чертёж листа: чертёжный вид без подсветки.
fn picture(model: &Drawn, key: &str, legend: bool, fonts: bool) -> Result<String, String> {
    let options = Options {
        view: View::Draft,
        legend,
        tick: None,
        trace: None,
        fonts,
    };
    svg(key, &model.sheets, &model.layout, &options)
        .ok_or_else(|| format!("`{}`: листа `{key}` нет", model.file))
}

/// Сценарий прогона модели: названный, активный проекта (если он её), либо
/// единственный её. Выбрать из нескольких за автора значило бы прогнать не тот.
fn scenario_of(project: &Project, file: &str, request: &Request) -> Result<Option<String>, String> {
    let own = project.scenarios_of(file);
    if let Some(named) = &request.scenario {
        if project.file(named).is_none() {
            return Err(format!("сценария `{named}` в проекте нет"));
        }
        return Ok(Some(named.clone()));
    }
    if let Some(main) = &project.manifest.main_scenario
        && own.contains(main)
    {
        return Ok(Some(main.clone()));
    }
    Ok((own.len() == 1).then(|| own[0].clone()))
}

/// Прогон модели: шаги тактов, из которых лента берёт подсветку и строку трассы.
fn run(
    project: &Project,
    file: &str,
    request: &Request,
    notes: &mut Vec<String>,
) -> Result<Vec<crate::runner::Step>, String> {
    let source = project
        .file(file)
        .map(|f| f.text.clone())
        .unwrap_or_default();
    let mut table = FileTable::new(file);
    let (ast, _) = takt_lang::parse(&source, 0).map_err(|diagnostics| {
        diagnostics
            .first()
            .map_or_else(|| format!("`{file}` не разбирается"), |d| shown(d, &table))
    })?;
    // Импорты каталога и архива - по составу проекта, как у страницы; одна модель
    // читает соседей с диска, как прогон без подкоманды.
    let built = if project.form == Form::Model {
        let mut search = vec![project.root.to_string_lossy().into_owned()];
        search.extend(request.search.iter().cloned());
        construct_model_with_files(&ast, None, &search, &mut table, false)
    } else {
        let _memory = takt_lang::semantic::import::memory::install(project.models());
        let search = takt_lang::compile::project_search_paths();
        construct_model_with_files(&ast, None, &search, &mut table, false)
    };
    let model = built.map_err(|d| shown(&d, &table))?;
    if let Some(d) = takt_lang::pipeline::validate_entry_model(&model) {
        return Err(shown(&d, &table));
    }
    let port_names = PortNames::from_model(&model.borrow());
    let clock_hz = model.borrow().clock_hz;
    let unit = crate::build_unit(model).map_err(|d| shown(&d, &table))?;

    let scenario = scenario_of(project, file, request)?;
    let steps: Vec<SimStep> = match &scenario {
        Some(name) => {
            let text = project
                .file(name)
                .map(|f| f.text.clone())
                .unwrap_or_default();
            serde_json::from_str(&text).map_err(|e| format!("`{name}` не читается: {e}"))?
        }
        None => Vec::new(),
    };
    let limit = request
        .steps
        .or(scenario.is_none().then_some(DEFAULT_STEPS));
    let mut runner = SimulationRunner::new(unit, steps, limit, port_names);
    if let Some(ms) = request.tick_ms {
        runner.set_tick_period_ns(ms.saturating_mul(1_000_000));
    } else if let Some(hz) = clock_hz.filter(|hz| *hz > 0) {
        runner.set_tick_period_ns(1_000_000_000 / i64::try_from(hz).unwrap_or(i64::MAX));
    }

    let mut out = Vec::new();
    loop {
        let step = runner.step()?;
        if let Some(result) = &step.result {
            for line in crate::trace::result_report(result).errors {
                notes.push(format!("`{file}`: {line}"));
            }
            out.push(step);
            return Ok(out);
        }
        out.push(step);
    }
}

/// Диагностика строкой: позиция, код, сообщение, заметки - как у прогона.
fn shown(diagnostic: &Diagnostic, table: &FileTable) -> String {
    let path = table.path_of(&diagnostic.loc).map(str::to_string);
    let stamped = diagnostic.clone().with_file_if_unset(path.as_deref());
    let code = stamped
        .code
        .as_deref()
        .map(|c| format!("[{c}] "))
        .unwrap_or_default();
    format!(
        "{}{code}{}{}",
        takt_lang::diagnostics::position_prefix(&stamped),
        stamped.message,
        takt_lang::diagnostics::format_notes(&stamped)
    )
}
