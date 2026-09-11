//! CLI-симулятор Takt-моделей.
//!
//! Запускает пошаговую симуляцию модели, переданной в аргументах командной строки.
//! Поддерживает: JSON-файл входных данных, проверку guard, GIF прогона.
//! Подкоманды `export` (картинки и видео проекта) и `project` (состав проекта) -
//! в модуле `subcommands`.

// Путь назван явно: файл прямо в `src/bin/` cargo принял бы за отдельный бинарник.
#[path = "takt_sim/subcommands.rs"]
mod subcommands;

use clap::Parser;
use std::path::PathBuf;
use std::process::ExitCode;
use takt_lang::parse;
use takt_lang::semantic::tree::construct_model_with_files;
use takt_sim::build_unit;
use takt_sim::film::{Film, PAUSE_MS, ROOT_SHEET};
use takt_sim::json_input::load_sim_steps;
use takt_sim::runner::{PortNames, RunResult, SimulationRunner};
use takt_sim::state_io;

// -- Аргументы командной строки ------------------------------------------------

#[derive(Parser)]
#[command(
    name = "takt-sim",
    about = "Симуляция Takt-моделей",
    version,
    after_help = "Подкоманды:\n  takt-sim export <проект> [ключи]   картинки листов и видео прогона\n  takt-sim project <проект>          состав проекта\nСправка подкоманды: takt-sim export --help"
)]
struct Args {
    /// Путь к.takt файлу (обязательный)
    model_file: PathBuf,

    /// Директории поиска include (можно указать несколько)
    #[arg(short = 'I', long = "include", value_name = "DIR")]
    include_paths: Vec<PathBuf>,

    /// Язык сообщений: `ru`, `en`, ... - по каталогам в дереве.
    ///
    /// Умолчание - русский; переменная `TAKT_LANG` действует, если ключа нет. Системная
    /// локаль (`LANG`/`LC_MESSAGES`) **не читается**: вывод инструмента не должен
    /// зависеть от машины - на этом стоят потактовые сверки. Ключ тот же, что у
    /// `taktc`, и разбирает его общий носитель.
    #[arg(long = "lang", value_name = "код")]
    lang: Option<String>,

    /// Количество шагов (по умолчанию - до терминального состояния)
    #[arg(short = 'n', long = "steps", value_name = "N")]
    steps: Option<usize>,

    /// Guard границ массива: доступ за границей не выполняется, а признак уходит в
    /// выходной порт `bounds_fault`.
    ///
    /// Без флага эталон отвечает `SIM-010` и останавливает прогон - это его умолчание и
    /// умолчание целей (флаг `--bounds-check` у `taktc`).
    #[arg(long = "bounds-check")]
    bounds_check: bool,

    /// Каталог для GIF прогона: кадр на такт - схема корневого листа цветным видом
    /// и строка трассы. Схема рисуется по файлу раскладки `<модель>.takt-ui` рядом
    /// с моделью; без него - отказ до прогона.
    #[arg(short = 'o', long = "output", value_name = "DIR")]
    output_dir: Option<PathBuf>,

    /// JSON-файл с входными данными и проверками
    #[arg(short = 's', long = "sim-file", value_name = "FILE")]
    sim_file: Option<PathBuf>,

    /// Загрузить состояние модели из JSON-файла перед симуляцией
    #[arg(long = "load-state", value_name = "FILE")]
    load_state: Option<PathBuf>,

    /// Сохранить состояние модели в JSON-файл после симуляции
    #[arg(long = "save-state", value_name = "FILE")]
    save_state: Option<PathBuf>,

    /// Снятый ключ настроек прежней графики. Принимается ради внятного отказа:
    /// неизвестный ключ `clap` отверг бы справкой, не назвав причину.
    #[arg(long = "graphics-config", value_name = "FILE", hide = true)]
    graphics_config: Option<PathBuf>,

    /// Мягкий режим инвариантов: нарушение записывается, и прогон продолжается, вместо
    /// останова. Для отладки - сверки с C у него нет.
    #[arg(long = "invariant-soft")]
    invariant_soft: bool,

    /// Сколько модельного времени проходит за такт, в миллисекундах.
    ///
    /// Умолчание - 1 мс.
    #[arg(long = "tick-ms", value_name = "MS")]
    tick_ms: Option<i64>,
}

// -- Точка входа ---------------------------------------------------------------

fn main() -> ExitCode {
    env_logger::init();
    let argv: Vec<std::ffi::OsString> = std::env::args_os().collect();
    if let Some(code) = subcommands::dispatch(&argv) {
        return code;
    }
    let args = Args::parse_from(argv);

    // Язык - до первого сообщения: диагностика прогона обязана прийти уже на выбранном
    // языке. Разбор общий с `taktc`.
    if let Some(code) = args.lang.as_deref() {
        match takt_lang::diagnostics::lang::parse(code) {
            Ok(lang) => takt_lang::diagnostics::lang::activate(lang),
            Err(e) => {
                eprintln!("Ошибка: {e}");
                return ExitCode::FAILURE;
            }
        }
    }

    match run(args) {
        Ok(result) => {
            print_result(&result);
            match &result {
                // Мягкий режим завершает прогон, но нарушения - находки: не молчим
                // кодом возврата.
                RunResult::GuardFailed { .. }
                | RunResult::EvalFailed { .. }
                | RunResult::CompletedWithInvariantViolations { .. } => ExitCode::FAILURE,
                _ => ExitCode::SUCCESS,
            }
        }
        Err(e) => {
            eprintln!("Ошибка: {e}");
            ExitCode::FAILURE
        }
    }
}

fn run(args: Args) -> Result<RunResult, String> {
    // Снятый ключ - отказ словами, а не молчание: прежний вызов со своими
    // настройками получил бы другую картинку и не узнал бы об этом.
    if args.graphics_config.is_some() {
        return Err(
            "ключ `--graphics-config` снят: кадры прогона рисуются по файлу \
                    раскладки `<модель>.takt-ui` рядом с моделью, вид схемы задаёт он"
                .to_string(),
        );
    }

    // 1. Читаем исходный файл модели
    let source = std::fs::read_to_string(&args.model_file)
        .map_err(|e| format!("Не удалось прочитать {}: {e}", args.model_file.display()))?;

    // Реестр файлов: корневой - номер 0, импортируемые получит проход 0. Нужен, чтобы
    // назвать пользователю файл ошибки.
    let mut files = takt_lang::diagnostics::FileTable::new(&args.model_file.to_string_lossy());

    // 2. Парсинг
    let (ast, _comments) =
        parse(&source, 0).map_err(|diags| format_diagnostics("Ошибки парсинга", &diags, &files))?;

    // 3. Семантический анализ
    let search_paths: Vec<String> = args
        .include_paths
        .iter()
        .map(|p| p.to_string_lossy().into_owned())
        .collect();
    // Позиция - в начале строки, как у `taktc` и `rustc`: так её видит редактор. Слова
    // "Семантическая ошибка" не дублируются - об этом говорит код (`SE-...`).
    let model_rc = construct_model_with_files(&ast, None, &search_paths, &mut files, false)
        .map_err(|d| format_diagnostic(&d, &files))?;

    // Библиотечный файл (без единого состояния) исполнять нечем - `SE-102`. Проверка та
    // же, что у целей компиляции: правило одно, и разойтись двум ответам на один вход не
    // по чему. Без неё эталон такой файл принимает и рапортует "Завершено: модель
    // достигла терминального состояния за 1 шагов" - прогон автомата, которого в файле
    // нет.
    if let Some(mut d) = takt_lang::pipeline::validate_entry_model(&model_rc) {
        // Подсказка "кто эту библиотеку подключает" - та же, что у `taktc`. Два ответа
        // на один вход расходиться не должны: если компилятор называет импортёра, а
        // симулятор молчит, автор получает разную помощь от инструментов одного
        // проекта.
        if let Some(note) =
            takt_lang::pipeline::importers_note(&args.model_file.to_string_lossy(), &search_paths)
        {
            d.notes.push(takt_lang::diagnostics::Note {
                // Позиции нет по существу: заметка говорит о другом файле.
                loc: takt_lang::diagnostics::Location::Codegen,
                message: note,
            });
        }
        return Err(format_diagnostic(&d, &files));
    }

    // 4. Извлекаем имена портов и объявленную частоту
    let port_names = extract_port_names(&model_rc.borrow());
    let clock_hz = model_rc.borrow().clock_hz;

    // 5. Строим Unit
    if args.bounds_check {
        takt_lang::semantic::bounds_guard::insert_bounds_guards(&model_rc);
    }
    let mut unit = build_unit(model_rc).map_err(|d| format!("Ошибка построения: {}", d.message))?;

    // Загружаем сохранённое состояние (если указано)
    if let Some(path) = &args.load_state {
        state_io::load_from_file(&mut unit, path)?;
        println!("Состояние загружено из {}", path.display());
    }

    // 6. Загружаем шаги симуляции (если указан файл)
    let sim_steps = if let Some(path) = &args.sim_file {
        load_sim_steps(path)?
    } else {
        vec![]
    };

    // Лента кадров - до прогона: нет файла раскладки или он неполон - отказ, пока
    // ничего не исполнено и не записано.
    let mut film = match &args.output_dir {
        Some(_) => Some(film_of(&args.model_file, &source)?),
        None => None,
    };

    // 7. Создаём и запускаем runner
    let mut runner = SimulationRunner::new(unit, sim_steps, args.steps, port_names);
    runner.set_invariant_soft(args.invariant_soft);
    // Период такта модельных часов: флаг > частота модели > умолчание 1 мс. Приоритет
    // тот же, что у профиля времени в компиляторе: явно заданное побеждает выведенное.
    if let Some(ms) = args.tick_ms {
        runner.set_tick_period_ns(ms.saturating_mul(1_000_000));
    } else if let Some(hz) = clock_hz.filter(|hz| *hz > 0) {
        runner.set_tick_period_ns(1_000_000_000 / i64::try_from(hz).unwrap_or(i64::MAX));
    }

    let result = runner.run_with(|step| {
        if let Some(film) = &mut film {
            film.record(step);
        }
        Ok(())
    })?;

    // 8. Сохраняем состояние модели (если указано)
    if let Some(path) = &args.save_state {
        state_io::save_to_file(runner.unit(), path)?;
        println!("Состояние сохранено в {}", path.display());
    }

    // 9. GIF прогона: имя - по файлу сценария, без него - по файлу модели.
    if let (Some(dir), Some(film)) = (&args.output_dir, &film) {
        let stem = args
            .sim_file
            .as_ref()
            .unwrap_or(&args.model_file)
            .file_stem()
            .map_or_else(
                || "output".to_string(),
                |s| s.to_string_lossy().into_owned(),
            );
        write_gif(film, &dir.join(format!("{stem}.gif")))?;
    }

    Ok(result)
}

/// Лента корневого листа по файлу раскладки рядом с моделью.
fn film_of(model_file: &std::path::Path, source: &str) -> Result<Film, String> {
    let layout_file = model_file.with_extension("takt-ui");
    let layout = std::fs::read_to_string(&layout_file).map_err(|e| {
        format!(
            "раскладки нет: {} ({e}) - кадры прогона рисуются по файлу раскладки \
             схемы, его пишет редактор",
            layout_file.display()
        )
    })?;
    Film::new(source, &layout, ROOT_SHEET, true)
        .map_err(|e| format!("{}: {e}", layout_file.display()))
}

/// Пишет GIF ленты. Файл появляется целиком либо не появляется вовсе: запись идёт
/// во временный файл рядом и переименовывается по готовности. Прогон без единого
/// такта кадров не даёт, и файла нет - исход прогона уже назван сводкой.
fn write_gif(film: &Film, path: &std::path::Path) -> Result<(), String> {
    if film.frames() == 0 {
        return Ok(());
    }
    if let Some(dir) = path.parent() {
        std::fs::create_dir_all(dir)
            .map_err(|e| format!("Не удалось создать каталог {}: {e}", dir.display()))?;
    }
    let part = path.with_extension("gif.part");
    let written = std::fs::File::create(&part)
        .map_err(|e| format!("Не удалось создать {}: {e}", part.display()))
        .and_then(|file| film.gif(PAUSE_MS, std::io::BufWriter::new(file)))
        .and_then(|()| {
            std::fs::rename(&part, path)
                .map_err(|e| format!("Не удалось записать {}: {e}", path.display()))
        });
    if written.is_err() {
        let _ = std::fs::remove_file(&part);
    }
    written
}

// -- Вспомогательные функции ---------------------------------------------------

fn extract_port_names(model: &takt_lang::semantic::ModelNode) -> PortNames {
    // Сбор рекурсивен, включая под-модели композиции; живёт он в библиотеке
    // (`PortNames::from_model`) ради тестируемости.
    PortNames::from_model(model)
}

/// Печатает диагностики с позицией и кодом.
///
///
/// Печатаются **все** диагностики, а не первая: у разбора их обычно несколько, и каждая -
/// своя подсказка. (`taktc` показывает первую; здесь поведение полезнее и сохранено
/// осознанно.)
fn format_diagnostics(
    prefix: &str,
    diags: &[takt_lang::diagnostics::Diagnostic],
    files: &takt_lang::diagnostics::FileTable,
) -> String {
    let messages: Vec<String> = diags.iter().map(|d| format_diagnostic(d, files)).collect();
    format!("{prefix}:\n{}", messages.join("\n"))
}

/// Одна диагностика: `путь:строка:колонка: [КОД] сообщение` и её заметки.
///
/// Позиция печатается общей для всех бинарников функцией
/// (`takt_lang::diagnostics::position_prefix`) - формат позиции един у `taktc` и
/// симулятора физически, а не по договорённости.
///
/// **Заметки - тем же общим носителем** (`format_notes`). Один вход, два разных объёма
/// сведений.
fn format_diagnostic(
    diag: &takt_lang::diagnostics::Diagnostic,
    files: &takt_lang::diagnostics::FileTable,
) -> String {
    let stamped = stamp_file(diag.clone(), files);
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

/// Разрешает номер файла диагностики в путь (приём ).
///
/// Реестр - деталь загрузки модели: он жив только здесь, а наружу выходит уже
/// разрешённый путь в `Diagnostic::file`.
fn stamp_file(
    diag: takt_lang::diagnostics::Diagnostic,
    files: &takt_lang::diagnostics::FileTable,
) -> takt_lang::diagnostics::Diagnostic {
    let path = files.path_of(&diag.loc).map(str::to_string);
    diag.with_file_if_unset(path.as_deref())
}

fn print_result(result: &RunResult) {
    // Текст сводки строит библиотека (`trace::result_report`): та же сводка нужна
    // потребителю без консоли - модулю WebAssembly. CLI решает только, в какой поток её
    // отдать.
    let report = takt_sim::trace::result_report(result);
    for line in &report.info {
        println!("{line}");
    }
    for line in &report.errors {
        eprintln!("{line}");
    }
}
