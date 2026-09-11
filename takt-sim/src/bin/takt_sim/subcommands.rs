//! Подкоманды `takt-sim export` и `takt-sim project`.
//!
//! Прежний вызов `takt-sim модель [ключи]` остаётся прогоном: подкоманда
//! узнаётся по первому аргументу, и имя файла модели с ней не спутать - у модели
//! есть расширение.

use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use clap::{Parser, ValueEnum};
use takt_project::{Form, Kind, Project, SourceFile};
use takt_scheme::raster::Background;
use takt_scheme::style::View;
use takt_sim::export::{Format, Request, export};
use takt_sim::film::PAUSE_MS;

/// Подкоманда по первому аргументу; `None` - прежний вызов прогона.
pub fn dispatch(args: &[std::ffi::OsString]) -> Option<ExitCode> {
    let name = args.get(1)?.to_str()?;
    let rest = std::iter::once(format!("takt-sim {name}").into()).chain(args[2..].iter().cloned());
    match name {
        "export" => Some(finish(run_export(ExportArgs::parse_from(rest)))),
        "project" => Some(finish(run_project(ProjectArgs::parse_from(rest)))),
        _ => None,
    }
}

fn finish(result: Result<bool, String>) -> ExitCode {
    match result {
        Ok(true) => ExitCode::SUCCESS,
        Ok(false) => ExitCode::FAILURE,
        Err(e) => {
            eprintln!("Ошибка: {e}");
            ExitCode::FAILURE
        }
    }
}

#[derive(Clone, Copy, ValueEnum)]
enum ViewArg {
    /// Чертёжный: одни чернила, без подсветки.
    Draft,
    /// Цветной: подсветка последнего такта прогона.
    Run,
}

#[derive(Clone, Copy, ValueEnum)]
enum FormatArg {
    Svg,
    Png,
    Gif,
    Mp4,
}

#[derive(Clone, Copy, ValueEnum)]
enum BackgroundArg {
    /// Заливка цветом поля листа.
    Fill,
    /// Прозрачный фон.
    None,
}

#[derive(Clone, Copy, ValueEnum)]
enum OnOff {
    On,
    Off,
}

/// Экспорт картинок листов и видео прогона по файлам раскладки `.takt-ui`.
#[derive(Parser)]
#[command(name = "takt-sim export", version)]
struct ExportArgs {
    /// Проект: каталог с `takt-project.json`, архив `.zip` либо модель `.takt`.
    project: PathBuf,

    /// Каталог вывода; умолчание - `export/` в каталоге проекта.
    #[arg(short = 'o', long = "output", value_name = "DIR")]
    output: Option<PathBuf>,

    /// Вид картинок; умолчание - `draft`. Видео всегда цветное.
    #[arg(long, value_enum)]
    view: Option<ViewArg>,

    /// Формат (повторяемый): `svg`, `png` - картинка листа, `gif`, `mp4` - видео
    /// прогона. Умолчание - `svg`.
    #[arg(long = "format", value_enum)]
    formats: Vec<FormatArg>,

    /// Фон PNG: `fill` - поле листа, `none` - прозрачный.
    #[arg(long, value_enum, default_value = "fill")]
    background: BackgroundArg,

    /// Легенда - таблица знаков состояний и условий с подписями автора.
    #[arg(long, value_enum, default_value = "on")]
    legend: OnOff,

    /// Файл модели проекта; умолчание - картинки всех моделей, видео активной.
    #[arg(long, value_name = "FILE")]
    model: Option<String>,

    /// Лист (`/`, `/#Line`, `Engine`); умолчание - картинки всех листов, видео корня.
    #[arg(long, value_name = "KEY")]
    sheet: Option<String>,

    /// Пауза между кадрами видео, миллисекунд.
    #[arg(long, value_name = "MS", default_value_t = PAUSE_MS)]
    pause: u32,

    /// Сценарий прогона: файл проекта; файл вне проекта берётся с диска.
    /// Умолчание - активный либо единственный сценарий модели.
    #[arg(short = 's', long = "sim-file", value_name = "FILE")]
    scenario: Option<PathBuf>,

    /// Предел тактов прогона; умолчание - длина сценария, без сценария 200.
    #[arg(short = 'n', long = "steps", value_name = "N")]
    steps: Option<usize>,

    /// Сколько модельного времени проходит за такт, в миллисекундах.
    #[arg(long = "tick-ms", value_name = "MS")]
    tick_ms: Option<i64>,

    /// Каталоги поиска импортов одной модели (у каталога и архива импорты - по составу).
    #[arg(short = 'I', long = "include", value_name = "DIR")]
    include: Vec<PathBuf>,

    /// Язык сообщений: `ru`, `en`, ...
    #[arg(long = "lang", value_name = "код")]
    lang: Option<String>,
}

/// Состав проекта: файлы и роды, активные файлы, сборка, сценарии моделей.
#[derive(Parser)]
#[command(name = "takt-sim project", version)]
struct ProjectArgs {
    /// Проект: каталог с `takt-project.json`, архив `.zip` либо модель `.takt`.
    /// С `--owner` - файлы моделей, среди которых ищется владелец сценария.
    #[arg(required = true)]
    paths: Vec<PathBuf>,

    /// Модель, которой принадлежит сценарий, по правилу принадлежности: печатается
    /// файл модели, не принадлежит никому - пустая строка и код 1.
    #[arg(long, value_name = "SCENARIO")]
    owner: Option<String>,
}

fn language(code: Option<&str>) -> Result<(), String> {
    if let Some(code) = code {
        takt_lang::diagnostics::lang::activate(takt_lang::diagnostics::lang::parse(code)?);
    }
    Ok(())
}

fn run_export(args: ExportArgs) -> Result<bool, String> {
    language(args.lang.as_deref())?;
    let mut project = takt_project::load(&args.project).map_err(|e| e.message().to_string())?;
    let scenario = match &args.scenario {
        Some(path) => Some(scenario_in(&mut project, path)?),
        None => None,
    };
    let formats: BTreeSet<Format> = if args.formats.is_empty() {
        BTreeSet::from([Format::Svg])
    } else {
        args.formats
            .iter()
            .map(|f| match f {
                FormatArg::Svg => Format::Svg,
                FormatArg::Png => Format::Png,
                FormatArg::Gif => Format::Gif,
                FormatArg::Mp4 => Format::Mp4,
            })
            .collect()
    };
    let request = Request {
        view: args.view.map(|v| match v {
            ViewArg::Draft => View::Draft,
            ViewArg::Run => View::Run,
        }),
        formats,
        background: match args.background {
            BackgroundArg::Fill => Background::Fill,
            BackgroundArg::None => Background::None,
        },
        legend: matches!(args.legend, OnOff::On),
        model: args.model,
        sheet: args.sheet,
        pause_ms: args.pause,
        scenario,
        steps: args.steps,
        tick_ms: args.tick_ms,
        search: args
            .include
            .iter()
            .map(|p| p.to_string_lossy().into_owned())
            .collect(),
    };
    let out = export(&project, &request)?;
    let dir = args.output.unwrap_or_else(|| project.root.join("export"));
    std::fs::create_dir_all(&dir)
        .map_err(|e| format!("Не удалось создать каталог {}: {e}", dir.display()))?;
    for file in &out.files {
        let path = dir.join(&file.name);
        write(&path, &file.bytes)?;
        println!("{}", path.display());
    }
    for note in &out.notes {
        eprintln!("{note}");
    }
    Ok(out.notes.is_empty())
}

/// Сценарий ключа `-s`: файл проекта по имени, файл вне проекта - с диска, и он
/// входит в проект сценарием.
fn scenario_in(project: &mut Project, path: &Path) -> Result<String, String> {
    let name = path
        .file_name()
        .and_then(|n| n.to_str())
        .ok_or_else(|| format!("'{}': не имя файла", path.display()))?
        .to_string();
    if project.file(&name).is_some() && !path.is_file() {
        return Ok(name);
    }
    if !path.is_file() {
        return Err(format!(
            "сценария '{}' нет ни в проекте, ни на диске",
            path.display()
        ));
    }
    let text = std::fs::read_to_string(path).map_err(|e| format!("'{}': {e}", path.display()))?;
    project.files.retain(|f| f.name != name);
    project.files.push(SourceFile {
        name: name.clone(),
        kind: Kind::Scenario.as_str().to_string(),
        text,
    });
    Ok(name)
}

/// Файл появляется целиком либо не появляется: запись во временный и переименование.
fn write(path: &Path, bytes: &[u8]) -> Result<(), String> {
    let mut part = path.as_os_str().to_owned();
    part.push(".part");
    let part = PathBuf::from(part);
    let written = std::fs::write(&part, bytes)
        .map_err(|e| format!("Не удалось записать {}: {e}", part.display()))
        .and_then(|()| {
            std::fs::rename(&part, path)
                .map_err(|e| format!("Не удалось записать {}: {e}", path.display()))
        });
    if written.is_err() {
        let _ = std::fs::remove_file(&part);
    }
    written
}

fn run_project(args: ProjectArgs) -> Result<bool, String> {
    if let Some(scenario) = &args.owner {
        let stems: Vec<(String, &PathBuf)> = args
            .paths
            .iter()
            .filter_map(|p| {
                let name = p.file_name()?.to_str()?;
                match takt_project::stem_of(name) {
                    Some((stem, Kind::Takt)) => Some((stem.to_string(), p)),
                    _ => None,
                }
            })
            .collect();
        let name = Path::new(scenario)
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or(scenario);
        let owner = takt_project::owner_of(name, stems.iter().map(|(s, _)| s.as_str()));
        let path = owner.and_then(|o| stems.iter().find(|(s, _)| s == o).map(|(_, p)| *p));
        println!(
            "{}",
            path.map(|p| p.display().to_string()).unwrap_or_default()
        );
        return Ok(path.is_some());
    }
    let [path] = args.paths.as_slice() else {
        return Err("проект один: каталог, архив либо модель".to_string());
    };
    let project = takt_project::load(path).map_err(|e| e.message().to_string())?;
    for line in describe(&project) {
        println!("{line}");
    }
    Ok(true)
}

/// Состав проекта строками: форма, активные файлы, сборка, файлы с родами,
/// сценарии каждой модели. Строки стабильны - их читают скрипты.
fn describe(project: &Project) -> Vec<String> {
    let manifest = &project.manifest;
    let form = match project.form {
        Form::Directory => "каталог",
        Form::Archive => "архив",
        Form::Model => "модель",
    };
    let or_none = |v: &Option<String>| v.clone().unwrap_or_else(|| "-".to_string());
    let or_dash = |v: &str| {
        if v.is_empty() {
            "-".to_string()
        } else {
            v.to_string()
        }
    };
    let mut out = vec![
        format!("проект: {} ({form})", manifest.name),
        format!("активный файл: {}", or_none(&manifest.main_file)),
        format!("активный сценарий: {}", or_none(&manifest.main_scenario)),
        format!("цель: {}", or_dash(&manifest.build_target)),
        format!("ключи: {}", or_dash(&manifest.build_args)),
        "файлы:".to_string(),
    ];
    for file in &project.files {
        out.push(format!("  {}\t{}", file.name, file.kind));
    }
    out.push("сценарии:".to_string());
    for model in project.of_kind(Kind::Takt) {
        let scenarios = project.scenarios_of(&model.name);
        out.push(format!(
            "  {}\t{}",
            model.name,
            if scenarios.is_empty() {
                "-".to_string()
            } else {
                scenarios.join(", ")
            }
        ));
    }
    out
}
