//! Компилятор Takt - утилита командной строки.
//!
//! # Использование
//!
//! ```text
//! taktc compile [--target c] [-I dir1:dir2] [--verbose | --quiet] <input.takt> [-o output_dir]
//! taktc compile input.takt           # вывод в ./output
//! taktc --help                      # справка
//! ```
//!
//! # Устройство
//!
//! Бинарник - **тонкий диспетчер**: разбор аргументов и исполнение подкоманд живут в
//! библиотеке `takt_lang`: `compile` - [`takt_lang::compile_cli`], `verify` -
//! [`takt_lang::verification::verify_cli`], `address-map` - [`takt_lang::address_map`].
//! Здесь остаются `fmt`, печать вердиктов `verify`, справка и маршрутизация по
//! `args[1]`.
//!
//! # Уровни диагностики
//!
//! - `--verbose` (`-v`): расширенный вывод - все предупреждения и полные пути к
//!   файлам. Взаимоисключается с `--quiet`.
//! - `--quiet` (`-q`): тихий режим - только ошибки компиляции. Взаимоисключается
//!   с `--verbose`.

use std::env;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::process;
use std::rc::Rc;

// -----------------------------------------------------------------------------
// Подкоманда `fmt` - канонический форматтер
// -----------------------------------------------------------------------------

/// Опции подкоманды `fmt`.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct FmtOptions {
    /// Не писать файлы; вернуть ненулевой код, если ввод отличается от канона.
    ///
    /// Режим для CI: проверяет, отформатирован ли репозиторий.
    pub check: bool,
    /// Читать из stdin, писать в stdout.
    pub stdin: bool,
    /// Файлы и каталоги (каталоги обходятся рекурсивно по `*.takt`).
    pub paths: Vec<String>,
}

/// Разбирает аргументы подкоманды `fmt`.
///
/// Принимает слайс без имени программы и без `"fmt"` в начале.
pub fn parse_fmt_args(args: &[String]) -> Result<FmtOptions, String> {
    let mut options = FmtOptions::default();
    for arg in args {
        match arg.as_str() {
            "--check" => options.check = true,
            "--stdin" => options.stdin = true,
            other if other.starts_with('-') => {
                return Err(format!("неизвестный флаг '{other}'"));
            }
            other => options.paths.push(other.to_string()),
        }
    }
    if options.stdin && !options.paths.is_empty() {
        return Err("--stdin несовместим с указанием файлов".to_string());
    }
    if !options.stdin && options.paths.is_empty() {
        return Err("укажите файлы/каталоги или --stdin".to_string());
    }
    Ok(options)
}

/// Рекурсивно собирает `*.takt` из файла или каталога.
fn collect_takt_files(path: &Path, out: &mut Vec<PathBuf>) -> Result<(), String> {
    if path.is_file() {
        out.push(path.to_path_buf());
        return Ok(());
    }
    if !path.is_dir() {
        return Err(format!("путь не найден: {}", path.display()));
    }
    let entries = fs::read_dir(path).map_err(|e| format!("{}: {e}", path.display()))?;
    for entry in entries.flatten() {
        let child = entry.path();
        if child.is_dir() {
            collect_takt_files(&child, out)?;
        } else if child.extension().is_some_and(|e| e == "takt") {
            out.push(child);
        }
    }
    Ok(())
}

/// Печатает предупреждения о стиле.
///
/// На код возврата они **не влияют** - это: именование советует, а не запрещает, и `fmt
/// --check` по-прежнему падает только из-за неканоничного формата. Иначе предупреждение
/// стало бы ошибкой, а корпус пришлось бы приводить к канону обязательным порядком.
///
/// Формат берётся у общего `format_warning`, а не пишется здесь: вид диагностики - её
/// собственное свойство.
fn report_style_warnings(warnings: &[takt_lang::diagnostics::Diagnostic], path: Option<&str>) {
    for w in warnings {
        let stamped = w.clone().with_file_if_unset(path);
        eprintln!("{}", takt_lang::diagnostics::format_warning(&stamped));
    }
}

/// Печатает отказ форматирования в **общем** формате диагностики.
///
/// `path` - путь к файлу, если он есть (у `--stdin` его нет). Диагностика разбора
/// приходит из библиотеки без файла (`file: None`): его знает только вызывающий, и он
/// же проставляет - тем самым появляется префикс `путь:строка:колонка`, который
/// `position_prefix` иначе выдать не может.
///
/// Формат берётся у [`takt_lang::diagnostics::format_compile_error`], а не пишется
/// здесь заново: вид диагностики - её собственное свойство, и вторая копия формата в
/// `taktc` уже расходилась однажды.
///
/// Диагностики печатаются **построчно**: их бывает несколько, и вывод обязан совпадать
/// с выводом `compile` строка в строку - именно это, а не отсутствие `Debug`-дампа,
/// есть предмет.
fn report_format_error(error: &takt_lang::format::FormatError, path: Option<&str>) {
    match error {
        takt_lang::format::FormatError::Parse(diagnostics) => {
            for diagnostic in diagnostics {
                let stamped = diagnostic.clone().with_file_if_unset(path);
                eprintln!("{}", takt_lang::diagnostics::format_compile_error(&stamped));
            }
        }
        // Отказ печати узла - такая же диагностика, и путь ей ставит тот же вызывающий.
        takt_lang::format::FormatError::Unsupported(diagnostic) => {
            let stamped = diagnostic.clone().with_file_if_unset(path);
            eprintln!("{}", takt_lang::diagnostics::format_compile_error(&stamped));
        }
    }
}

/// Исполняет подкоманду `fmt`; возвращает код завершения процесса.
///
/// Коды: `0` - всё канонично (или файлы отформатированы); `1` - при `--check` найдены
/// отличия либо произошла ошибка. Ненулевой код при `--check` - это и есть контракт для
/// CI.
fn run_fmt(options: &FmtOptions) -> i32 {
    if options.stdin {
        let mut source = String::new();
        if let Err(e) = io::Read::read_to_string(&mut io::stdin(), &mut source) {
            eprintln!("Ошибка чтения stdin: {e}");
            return 1;
        }
        return match takt_lang::format::format_source_with_warnings(&source) {
            Ok((formatted, style)) => {
                report_style_warnings(&style, None);
                if options.check {
                    if formatted == source {
                        0
                    } else {
                        eprintln!("stdin: требуется форматирование");
                        1
                    }
                } else {
                    print!("{formatted}");
                    0
                }
            }
            Err(e) => {
                // Пути нет - префикс позиции будет пуст, и это верно: выдумывать
                // координаты файла, которого не существует, нельзя.
                report_format_error(&e, None);
                1
            }
        };
    }

    let mut files = Vec::new();
    for path in &options.paths {
        if let Err(e) = collect_takt_files(Path::new(path), &mut files) {
            eprintln!("Ошибка: {e}");
            return 1;
        }
    }

    let mut need_format = Vec::new();
    let mut failed = 0usize;
    let mut changed = 0usize;

    for file in &files {
        let source = match fs::read_to_string(file) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("Ошибка чтения '{}': {e}", file.display());
                failed += 1;
                continue;
            }
        };
        let path_text = file.display().to_string();
        let formatted = match takt_lang::format::format_source_with_warnings(&source) {
            Ok((f, style)) => {
                report_style_warnings(&style, Some(&path_text));
                f
            }
            Err(e) => {
                // Отказ форматтера - не "файл канонический". Сообщаем и считаем
                // ошибкой: молча пропустить значило бы соврать в --check.
                let path = file.display().to_string();
                report_format_error(&e, Some(&path));
                failed += 1;
                continue;
            }
        };
        if formatted == source {
            continue;
        }
        if options.check {
            need_format.push(file.clone());
        } else if let Err(e) = fs::write(file, &formatted) {
            eprintln!("Ошибка записи '{}': {e}", file.display());
            failed += 1;
        } else {
            changed += 1;
        }
    }

    if options.check {
        for file in &need_format {
            eprintln!("требуется форматирование: {}", file.display());
        }
        if !need_format.is_empty() {
            eprintln!(
                "\nНе отформатировано файлов: {} из {}",
                need_format.len(),
                files.len()
            );
        }
        if need_format.is_empty() && failed == 0 {
            return 0;
        }
        return 1;
    }

    if changed > 0 {
        eprintln!("Отформатировано файлов: {changed} из {}", files.len());
    }
    if failed > 0 { 1 } else { 0 }
}

// -----------------------------------------------------------------------------
// Подкоманда `verify` - model checking по LTL
// -----------------------------------------------------------------------------

// Разбор аргументов `verify` и тип графа живут в библиотеке
// (`takt_lang::verification::{verify_cli, dot}`) - бинарник тонкий (лимит размера
// `taktc.rs`). Здесь - только диспетчер и печать.
use takt_lang::verification::verify_cli::{VerifyOptions, parse_verify_args};

/// Выполняет подкоманду `verify`; возвращает код возврата процесса.
///
/// Код `0` - все проверенные свойства держатся; `1` - есть нарушение, непроверяемое
/// свойство или ошибка разбора.
fn run_verify(options: &VerifyOptions) -> i32 {
    let source = match fs::read_to_string(&options.input_file) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Ошибка чтения файла '{}': {e}", options.input_file);
            return 1;
        }
    };

    let (ast, _) = match takt_lang::parse(&source, 0) {
        Ok(parsed) => parsed,
        Err(diags) => {
            for d in diags {
                eprintln!(
                    "Ошибка разбора [{}]: {}",
                    d.code.as_deref().unwrap_or("?"),
                    d.message
                );
            }
            return 1;
        }
    };

    let model = match takt_lang::semantic::tree::construct_model(&ast, None, &options.include_dirs)
    {
        Ok(m) => m,
        Err(d) => {
            eprintln!(
                "Семантическая ошибка [{}]: {}",
                d.code.as_deref().unwrap_or("?"),
                d.message
            );
            return 1;
        }
    };

    // Экспорт графа верификации в DOT - вместо проверки. Построение и разбор - в
    // библиотеке (`verification::dot`); бинарник лишь печатает.
    if let Some(kind) = options.emit_graph {
        return match takt_lang::verification::dot::emit_graph_dot(
            &model.borrow(),
            kind,
            options.property.as_deref(),
        ) {
            Ok(dot) => {
                print!("{dot}");
                0
            }
            Err(msg) => {
                eprintln!("{msg}");
                1
            }
        };
    }

    // Свойства: либо одно из --property, либо все объявленные в файле.
    let outcome = match &options.property {
        Some(text) => {
            let phi = match takt_lang::parse_ltl_property(text) {
                Ok(p) => p,
                Err(d) => {
                    eprintln!("Ошибка разбора свойства: {}", d.message);
                    return 1;
                }
            };
            let (verdict, trace) =
                takt_lang::verification::verify::verify_model_traced(&model.borrow(), &phi);
            // Свойство из строки проверяется против корневой модели файла и области не
            // касается: пропускать тут нечего.
            takt_lang::VerifyOutcome {
                results: vec![takt_lang::PropertyResult {
                    model: String::new(),
                    loc: takt_lang::diagnostics::Location::Implicit,
                    verdict,
                    formula: phi,
                    trace: options.trace.then_some(trace),
                }],
                skipped: Vec::new(),
            }
        }
        None => takt_lang::verify_all_scoped(Rc::clone(&model), options.trace, options.scope),
    };

    if outcome.results.is_empty() && outcome.skipped.is_empty() {
        eprintln!(
            "В файле '{}' нет LTL-формул. Объявите свойство как `: [LTL] φ;` \
             или задайте его флагом --property \"φ\".",
            options.input_file
        );
        return 1;
    }

    print_verify_results(&outcome)
}

/// Печатает вердикты и возвращает код возврата процесса.
fn print_verify_results(outcome: &takt_lang::VerifyOutcome) -> i32 {
    use takt_lang::verification::verify::Verdict;

    let results = &outcome.results;
    let mut failures = 0usize;
    for result in results {
        if let Some(trace) = &result.trace {
            print!("{trace}");
        }
        let scope = if result.model.is_empty() {
            String::new()
        } else {
            format!(" [модель {}]", result.model)
        };
        match &result.verdict {
            Verdict::Holds => {
                println!("СВОЙСТВО ДЕРЖИТСЯ{scope}: {}", result.formula);
            }
            Verdict::Violated(cex) => {
                failures += 1;
                println!("СВОЙСТВО НАРУШЕНО{scope}: {}", result.formula);
                println!("  контрпример: {}", cex.trace());
                println!(
                    "  (абстракция управления: условия переходов не учитываются — \
                     контрпример может быть недостижим по данным)"
                );
            }
            Verdict::Unsupported { atoms, reason } => {
                failures += 1;
                println!("СВОЙСТВО НЕ ПРОВЕРЕНО{scope}: {}", result.formula);
                // Печатается одна причина - та, по которой проверка и не выполнена.
                println!("  атом(ы) {}: {}.", atoms.join(", "), reason.text());
                // Охват печатается всегда: он объясняет, что вообще проверяемо, и нужен
                // при любой из причин - в том числе чтобы автор увидел, что предикаты
                // над данными в охвате есть.
                println!(
                    "  В охвате: свойства управления (имя состояния) и предикаты над данными \
                     (`cond`/булев `var` над `bit`/`bool`/целым/`enum`)."
                );
            }
            Verdict::NoStartState => {
                failures += 1;
                println!("СВОЙСТВО НЕ ПРОВЕРЕНО{scope}: {}", result.formula);
                println!("  у модели нет стартового состояния — проверять нечего");
            }
        }
    }

    // Сужение области - не тишина: пропущенное перечисляется, иначе "все держатся"
    // читалось бы как "проверено всё".
    if !outcome.skipped.is_empty() {
        println!(
            "\nНе проверено (вне области): {} — модели из импортов: {}",
            outcome.skipped.len(),
            outcome.skipped.join(", ")
        );
        println!("  --scope all — проверить их тоже.");
    }

    // Итог печатается в stdout вместе с вердиктами: разведи их по потокам - и в
    // терминале итог всплывёт выше вердиктов из-за буферизации.
    if failures > 0 {
        println!(
            "\nПроверено свойств: {}; не держится/не проверено: {failures}",
            results.len()
        );
        return 1;
    }
    println!("\nПроверено свойств: {}; все держатся", results.len());
    0
}

/// Выводит справку по использованию утилиты в stderr.
fn print_usage() {
    eprintln!(
        "Takt — учебный язык автоматных моделей: показывает принципы автоматного подхода к разработке."
    );
    eprintln!();
    eprintln!("Использование: taktc compile [флаги] <input.takt> [-o <output>]");
    eprintln!("               taktc fmt [--check] [--stdin] <файлы/каталоги>");
    eprintln!(
        "               taktc verify [--property \"φ\"] [--scope file|all] [--trace] <input.takt>"
    );
    eprintln!(
        "               taktc address-map [--emit map|json] [--address-map <файл>] [-D N=V] [-o <out>] <input.takt>"
    );
    eprintln!("               taktc version | --version | -V");
    eprintln!("               taktc --help");
    eprintln!();
    eprintln!("Флаги compile:");
    eprintln!("  --target, -t <цель>    Целевой язык (по умолчанию: c) — см. «Целевые платформы»");
    eprintln!("  --output, -o <путь>    Путь к выходному файлу");
    eprintln!("  --include-dirs, -I <dirs>  Пути поиска файлов import, разделённые ':' или ';'");
    eprintln!("                             Можно повторять: -I /a -I /b  или  -I /a:/b");
    eprintln!("  --verbose, -v          Расширенный вывод: все предупреждения и полные пути");
    eprintln!("  --quiet, -q            Тихий режим: только ошибки");
    eprintln!("                         Флаги --verbose и --quiet взаимоисключающие");
    eprintln!("  --guard-enable         Включить генерацию проверок Guard-формул (по умолчанию)");
    eprintln!("  --guard-disable        Выключить генерацию проверок Guard-формул");
    eprintln!("  --fsm=switch|table     Форма автомата (все цели, кроме plantuml):");
    eprintln!(
        "                         switch — по умолчанию; table — переходы данными (таблица + диспетчер)"
    );
    eprintln!("  --bounds-check         Guard границ массива: доступ за границей не выполняется,");
    eprintln!(
        "                         признак уходит в выходной порт bounds_fault (по умолчанию выкл.)"
    );
    eprintln!(
        "  --inline=off|auto      Подстановка тела функции: off — по умолчанию (действует только"
    );
    eprintln!(
        "                         атрибут [inline]); auto — плюс эвристика (тело ≤ 5 операторов)"
    );
    eprintln!(
        "  --parameters=assign|specialize  Параметры модели: assign — поле экземпляра (по умолчанию);"
    );
    eprintln!(
        "                         specialize — копия модели на набор аргументов, параметр константой"
    );
    eprintln!("  --address-map <файл>   Внешняя карта адресов портов (.ld-подобный формат)");
    eprintln!("  -D, --define N=VALUE   Символ платформы для выражений адреса (повторяем);");
    eprintln!("                         слитно: -DN=VALUE. Значение — 0x…/десятичное[:бит].");
    eprintln!(
        "                         Виден ТОЛЬКО выражениям адреса, логику автомата не меняет."
    );
    eprintln!("  --float-width=32|64    Ширина вещественного типа в C: float или double");
    eprintln!(
        "  --float-as-q=m.n       Точность q(m,n) для реализации float (0096): sv → q; c/rust/st → q с --float-embedded"
    );
    eprintln!(
        "  --float-embedded       Реализовать float целочисленным q в c/rust/st (embedded без FPU)"
    );
    eprintln!(
        "  --bus=apb              Цель sv-mmio: адаптер шины рядом с ядром (<модель>_apb.sv)"
    );
    eprintln!(
        "  --tick-hz=<n>          Частота такта устройства (Гц): профиль «такты» (фича 0134)"
    );
    eprintln!("                         Модель с `clock` требует совпадающий флаг (SE-069/SE-070)");
    eprintln!(
        "                         По умолчанию 64 (double) — совпадает с точностью симулятора"
    );
    eprintln!();
    eprintln!("Целевые платформы:");
    eprintln!("  c         Генерация C-заголовочного файла");
    eprintln!("  c-hal     C + таблица адресов портов и дефолтный HAL (фича 0020)");
    eprintln!("  plantuml  Генерация диаграммы состояний PlantUML (.puml)");
    eprintln!("  st        Генерация Structured Text IEC 61131-3 (.st), язык ПЛК (фича 0041)");
    eprintln!("  st-at     ST + размещение портов по карте адресов (AT %...)");
    eprintln!("  rust      Генерация no_std Rust (.rs) — прошивка МК (фича 0050)");
    eprintln!("            Порты через трейт Hal; подключается в крейт через mod");
    eprintln!("  sv        Генерация синтезируемого SystemVerilog (.sv) — FPGA/ASIC (фича 0045)");
    eprintln!("            Такт модели ≡ posedge clk; clk/rst_n — служебные порты модуля");
    eprintln!("  sv-mmio   SV + порты с адресом → регистровый файл на шине (фича 0062)");
    eprintln!("            Порт с адресом = бит регистра; интерфейс reg_addr/wdata/wen/rdata");
    eprintln!();
    eprintln!("Примеры:");
    eprintln!("  taktc compile main.takt");
    eprintln!("  taktc compile -I /lib/lam:/home/user/lam main.takt -o build/");
    eprintln!("  taktc compile -I /lib/lam -I /home/user/lam --target c main.takt");
    eprintln!("  taktc compile --verbose main.takt");
    eprintln!("  taktc compile --quiet main.takt -o dist/");
    eprintln!();
    eprintln!("Подкоманда fmt (канонический форматтер):");
    eprintln!("  --check      Не писать файлы; ненулевой код, если нужен формат (для CI)");
    eprintln!("  --stdin      Читать из stdin, писать в stdout");
    eprintln!("  taktc fmt examples/            # отформатировать каталог на месте");
    eprintln!("  taktc fmt --check examples/    # проверить (CI)");
    eprintln!("  cat a.takt | taktc fmt --stdin  # отформатировать поток");
    eprintln!();
    eprintln!("Подкоманда verify (проверка LTL-свойств, model checking — фича 0049):");
    eprintln!("  --property, -p \"φ\"  Проверить одну формулу из командной строки");
    eprintln!("                      Без флага проверяются все `: [LTL] φ;` файла");
    eprintln!("  --scope file|all    Область проверки (по умолчанию: file — модели своего файла)");
    eprintln!("                      all — проверять и модели, пришедшие через import");
    eprintln!("  --trace             Печатать конвейер (Крипке, автомат !φ, произведение)");
    eprintln!("  -I <dirs>           Пути поиска файлов import");
    eprintln!();
    eprintln!("  Атом формулы — ИМЯ СОСТОЯНИЯ: `S` истинно, когда автомат в состоянии S.");
    eprintln!("  Проверяются свойства управления: достижимость, порядок состояний, живость.");
    eprintln!("  Свойства над данными (`G (temp <= 100)`) в этой абстракции не поддержаны.");
    eprintln!("  Код возврата: 0 — все свойства держатся; 1 — нарушение/не проверено.");
    eprintln!();
    eprintln!("  taktc verify model.takt");
    eprintln!("  taktc verify --property \"F Done\" model.takt       # достижимость");
    eprintln!("  taktc verify -p \"G (Fault -> F Idle)\" model.takt  # живость");
}

fn main() {
    let mut args: Vec<String> = env::args().collect();

    // Язык сообщений выбирается первым: ключ действует на все подкоманды разом, и отказ
    // разбора самих аргументов обязан прийти уже на выбранном языке. Разбор - в
    // библиотеке: ключ общий с `takt-sim`, и вторая копия разошлась бы с первой молча.
    if let Err(e) = takt_lang::diagnostics::lang::take_flag(&mut args) {
        eprintln!("Ошибка разбора аргументов: {e}");
        process::exit(1);
    }

    if args.len() < 2 || args.iter().any(|a| a == "--help" || a == "-h") {
        print_usage();
        process::exit(0);
    }

    // Версия - до разбора прочих подкоманд: `--version`/`-V` пользователь пробует
    // первым, и отвечать на них "неизвестная команда" невежливо. Синоним разрешается
    // Здесь, а не второй реализацией.
    if args[1] == "version" || args[1] == "--version" || args[1] == "-V" {
        process::exit(takt_lang::version::run_version_subcommand());
    }

    if args[1] == "fmt" {
        let options = match parse_fmt_args(&args[2..]) {
            Ok(o) => o,
            Err(e) => {
                eprintln!("Ошибка разбора аргументов: {e}");
                print_usage();
                process::exit(1);
            }
        };
        process::exit(run_fmt(&options));
    }

    if args[1] == "verify" {
        let options = match parse_verify_args(&args[2..]) {
            Ok(o) => o,
            Err(e) => {
                eprintln!("Ошибка разбора аргументов: {e}");
                print_usage();
                process::exit(1);
            }
        };
        process::exit(run_verify(&options));
    }

    if args[1] == "address-map" {
        // Логика - в библиотеке (`bin/taktc.rs` пришпилен к baseline размера).
        process::exit(takt_lang::address_map::run_export_subcommand(&args[2..]));
    }

    if args[1] != "compile" {
        eprintln!(
            "Ошибка: неизвестная команда '{}'. Используйте 'compile', 'fmt', 'verify', \
             'address-map' или 'version'.",
            args[1]
        );
        print_usage();
        process::exit(1);
    }

    // Подкоманда `compile` - вся логика в библиотеке (правило размера модуля).
    process::exit(takt_lang::compile_cli::run_compile(&args[2..]));
}

// --- Тесты --------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use takt_lang::verification::dot::GraphKind; // для тестов разбора --emit-graph

    // -- Подкоманда fmt --------------------------------------

    #[test]
    fn fmt_args_paths() {
        let args = vec!["examples/".to_string(), "a.takt".to_string()];
        let o = parse_fmt_args(&args).unwrap();
        assert!(!o.check);
        assert!(!o.stdin);
        assert_eq!(o.paths, vec!["examples/", "a.takt"]);
    }

    #[test]
    fn fmt_args_check_flag() {
        let args = vec!["--check".to_string(), "a.takt".to_string()];
        let o = parse_fmt_args(&args).unwrap();
        assert!(o.check);
    }

    #[test]
    fn fmt_args_stdin_flag() {
        let args = vec!["--stdin".to_string()];
        let o = parse_fmt_args(&args).unwrap();
        assert!(o.stdin);
        assert!(o.paths.is_empty());
    }

    #[test]
    fn fmt_args_stdin_with_files_is_error() {
        // Контрпример: `--stdin` и файлы одновременно бессмысленны - лучше отказать,
        // чем молча проигнорировать одно из двух.
        let args = vec!["--stdin".to_string(), "a.takt".to_string()];
        assert!(parse_fmt_args(&args).is_err());
    }

    #[test]
    fn fmt_args_without_input_is_error() {
        assert!(parse_fmt_args(&[]).is_err());
    }

    #[test]
    fn fmt_args_unknown_flag_is_error() {
        let args = vec!["--verbose".to_string()];
        assert!(parse_fmt_args(&args).is_err());
    }

    // -- Подкоманда verify -----------------------------------

    fn args(list: &[&str]) -> Vec<String> {
        list.iter().map(|s| s.to_string()).collect()
    }

    #[test]
    fn verify_args_file_only() {
        let o = parse_verify_args(&args(&["model.takt"])).unwrap();
        assert_eq!(o.input_file, "model.takt");
        assert_eq!(o.property, None, "без --property проверяются формулы файла");
        assert!(!o.trace);
    }

    #[test]
    fn verify_args_property_flag() {
        let o = parse_verify_args(&args(&["--property", "G (Fault -> F Idle)", "m.takt"])).unwrap();
        assert_eq!(o.property.as_deref(), Some("G (Fault -> F Idle)"));
        assert_eq!(o.input_file, "m.takt");
    }

    /// Короткая форма `-p` и слитная `--property=` - синонимы длинной.
    #[test]
    fn verify_args_property_short_and_joined_forms() {
        let short = parse_verify_args(&args(&["-p", "F Done", "m.takt"])).unwrap();
        let joined = parse_verify_args(&args(&["--property=F Done", "m.takt"])).unwrap();
        assert_eq!(short.property.as_deref(), Some("F Done"));
        assert_eq!(joined.property.as_deref(), Some("F Done"));
    }

    #[test]
    fn verify_args_trace_flag() {
        let o = parse_verify_args(&args(&["--trace", "m.takt"])).unwrap();
        assert!(o.trace);
        assert_eq!(o.input_file, "m.takt");
    }

    #[test]
    fn verify_args_include_dirs() {
        let o = parse_verify_args(&args(&["-I", "/a", "-I/b", "m.takt"])).unwrap();
        assert_eq!(o.include_dirs, vec!["/a", "/b"]);
    }

    #[test]
    fn verify_args_emit_graph() {
        // --emit-graph в раздельной и слитной форме.
        let k = parse_verify_args(&args(&["--emit-graph", "kripke", "m.takt"])).unwrap();
        assert_eq!(k.emit_graph, Some(GraphKind::Kripke));
        let b = parse_verify_args(&args(&["--emit-graph=buchi", "m.takt"])).unwrap();
        assert_eq!(b.emit_graph, Some(GraphKind::Buchi));
        let p = parse_verify_args(&args(&["--emit-graph", "product", "m.takt"])).unwrap();
        assert_eq!(p.emit_graph, Some(GraphKind::Product));
        // По умолчанию экспорта нет.
        let none = parse_verify_args(&args(&["m.takt"])).unwrap();
        assert_eq!(none.emit_graph, None);
    }

    #[test]
    fn verify_args_emit_graph_bad_value_is_error() {
        // Контрпример: опечатка не должна молча уходить в проверку.
        assert!(parse_verify_args(&args(&["--emit-graph", "kripk", "m.takt"])).is_err());
        assert!(parse_verify_args(&args(&["--emit-graph", "m.takt"])).is_err());
    }

    #[test]
    fn verify_args_without_file_is_error() {
        // Контрпример: проверять нечего - отказ, а не пустой прогон.
        assert!(parse_verify_args(&[]).is_err());
        assert!(parse_verify_args(&args(&["--property", "F Done"])).is_err());
    }

    #[test]
    fn verify_args_property_without_value_is_error() {
        assert!(parse_verify_args(&args(&["m.takt", "--property"])).is_err());
    }

    #[test]
    fn verify_args_duplicate_property_is_error() {
        // Контрпример: второй -p молча затирал бы первый, и отчёт "проверено свойств:
        // 1" умалчивал бы о невыполненной проверке.
        assert!(parse_verify_args(&args(&["-p", "F Done", "-p", "G Idle", "m.takt"])).is_err());
        assert!(
            parse_verify_args(&args(&["-p", "F Done", "--property=G Idle", "m.takt"])).is_err(),
            "смешение форм флага повтором быть не перестаёт"
        );
    }

    #[test]
    fn verify_args_second_file_is_error() {
        // Контрпример: verify работает с одним файлом; второй молча проигнорировать -
        // значит соврать о том, что проверено.
        assert!(parse_verify_args(&args(&["a.takt", "b.takt"])).is_err());
    }

    #[test]
    fn verify_args_unknown_flag_is_error() {
        assert!(parse_verify_args(&args(&["--target", "c", "m.takt"])).is_err());
    }

    // -- Область проверки verify ------------------

    #[test]
    fn verify_scope_defaults_to_file() {
        let o = parse_verify_args(&["m.takt".to_string()]).unwrap();
        assert_eq!(o.scope, takt_lang::VerifyScope::File);
    }

    #[test]
    fn verify_scope_is_parsed_in_both_forms() {
        let split = parse_verify_args(&[
            "--scope".to_string(),
            "all".to_string(),
            "m.takt".to_string(),
        ])
        .unwrap();
        assert_eq!(split.scope, takt_lang::VerifyScope::All);
        let joined = parse_verify_args(&["--scope=all".to_string(), "m.takt".to_string()]).unwrap();
        assert_eq!(joined.scope, takt_lang::VerifyScope::All);
    }

    /// A5: негодная область - отказ, а не молчаливое умолчание.
    #[test]
    fn verify_unknown_scope_is_rejected() {
        let err = parse_verify_args(&["--scope=al".to_string(), "m.takt".to_string()]).unwrap_err();
        assert!(err.contains("al"), "сообщение: {err}");
        assert!(
            err.contains("file"),
            "подсказать допустимые значения: {err}"
        );
    }

    #[test]
    fn verify_scope_requires_value() {
        let err = parse_verify_args(&["--scope".to_string()]).unwrap_err();
        assert!(err.contains("--scope"), "сообщение: {err}");
    }
}
