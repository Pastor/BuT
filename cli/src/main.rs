use std::fs;
use std::path::{Path, PathBuf};

use clap::Parser;

use but_codegen::{AllOutput, CodegenContext, IndentStyle};
use but_grammar::ast::SourceUnit;
use but_middleware::include::IncludeResolver;
use but_simulators::{build_all, Simulator, Value};
use but_visual::visualize_all;

/// Компилятор и симулятор BuT FSM
#[derive(Parser, Debug)]
#[command(name = "but", about = "Инструментарий BuT FSM: симуляция, визуализация, генерация кода")]
struct Args {
    /// Входной файл .but
    #[arg(value_name = "FILE")]
    source: PathBuf,

    /// Запустить симуляцию
    #[arg(long, short = 's')]
    simulate: bool,

    /// Количество шагов симуляции
    #[arg(long, default_value_t = 10)]
    steps: usize,

    /// Установить значения портов (формат: имя=значение), можно повторять
    #[arg(long, value_name = "NAME=VALUE")]
    port: Vec<String>,

    /// Сгенерировать DOT/PNG визуализацию
    #[arg(long, short = 'v')]
    visualize: bool,

    /// Сгенерировать C-код
    #[arg(long)]
    gen_c: bool,

    /// Сгенерировать Verilog-код
    #[arg(long)]
    gen_verilog: bool,

    /// Сгенерировать Structured Text (МЭК 61131-3)
    #[arg(long)]
    gen_st: bool,

    /// Сгенерировать ассемблер LC3
    #[arg(long)]
    gen_lc3: bool,

    /// Сгенерировать ассемблер ARM Thumb
    #[arg(long)]
    gen_thumb: bool,

    /// Сгенерировать все выходные данные (симуляция + визуализация + все генераторы)
    #[arg(long, short = 'a')]
    all: bool,

    /// Выходная директория
    #[arg(long, short = 'o', default_value = "gen")]
    output_dir: PathBuf,

    /// Дополнительные директории для поиска include-файлов (.but).
    /// Можно указывать несколько раз: -I ./lib -I ./shared
    /// Порядок поиска: директория исходного файла → указанные директории (слева направо).
    #[arg(long = "include-dir", short = 'I', value_name = "DIR")]
    include_dirs: Vec<PathBuf>,

    /// Количество пробелов на один уровень отступа при генерации кода (по умолчанию: 4).
    /// Игнорируется при использовании --indent-tab.
    #[arg(long, default_value_t = 4, value_name = "N")]
    indent_size: usize,

    /// Использовать символ табуляции вместо пробелов для отступов в генерируемом коде.
    #[arg(long)]
    indent_tab: bool,
}

fn main() {
    let args = Args::parse();

    // Читаем исходный файл
    let source_text = match fs::read_to_string(&args.source) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Ошибка чтения {}: {}", args.source.display(), e);
            std::process::exit(1);
        }
    };

    // Базовое имя файла для именования выходных файлов (без расширения)
    let base_name = args
        .source
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("model")
        .to_string();

    // Разбор
    let (unit, _comments) = match but_grammar::parse(&source_text, 0) {
        Ok(r) => r,
        Err(diagnostics) => {
            for d in &diagnostics {
                eprintln!("Ошибка разбора: {:?}", d);
            }
            std::process::exit(1);
        }
    };

    // Разрешение директив import (рекурсивное включение файлов)
    let unit = {
        let mut resolver = IncludeResolver::from_file(&args.source);
        for dir in &args.include_dirs {
            if dir.is_dir() {
                resolver.add_search_path(dir.clone());
            } else {
                eprintln!("Предупреждение: директория не найдена: {}", dir.display());
            }
        }
        match resolver.resolve(unit, &args.source) {
            Ok(merged) => merged,
            Err(errs) => {
                for e in &errs {
                    eprintln!("Ошибка импорта: {}", e);
                }
                std::process::exit(1);
            }
        }
    };

    // Семантический анализ (продолжаем даже при предупреждениях)
    let unit = match but_middleware::processing(unit) {
        Ok(unit) => unit,
        Err(diagnostics) => {
            eprintln!("[предупреждение] Семантические диагностики:");
            for d in &diagnostics {
                eprintln!("  {:?}", d);
            }
            std::process::exit(1);
        }
    };

    // Создаём выходную директорию
    if let Err(e) = fs::create_dir_all(&args.output_dir) {
        eprintln!("Не удаётся создать выходную директорию {}: {}", args.output_dir.display(), e);
        std::process::exit(1);
    }

    let do_simulate = args.simulate || args.all;
    let do_visualize = args.visualize || args.all;
    let do_gen_c = args.gen_c || args.all;
    let do_gen_verilog = args.gen_verilog || args.all;
    let do_gen_st = args.gen_st || args.all;
    let do_gen_lc3 = args.gen_lc3 || args.all;
    let do_gen_thumb = args.gen_thumb || args.all;

    // ── Симуляция ─────────────────────────────────────────────────────────────────
    if do_simulate {
        println!("\n=== Симуляция ===");
        match build_all(&unit) {
            Ok(machines) => {
                for machine in machines {
                    let mut sim = Simulator::new(machine);

                    for port_spec in &args.port {
                        if let Some((name, val_str)) = port_spec.split_once('=') {
                            let val = parse_value(val_str);
                            sim.set_port(name, val);
                        }
                    }

                    println!(
                        "Начальное состояние: {}",
                        sim.current_state().unwrap_or("<неизвестно>")
                    );

                    for step in 0..args.steps {
                        let changed = sim.step();
                        let state = sim.current_state().unwrap_or("<завершён>");
                        if changed {
                            println!("  Шаг {}: → {}", step + 1, state);
                        }
                    }

                    println!("Конечное состояние: {}", sim.current_state().unwrap_or("<неизвестно>"));
                    sim.print_ltl();
                    sim.print_state();
                }
            }
            Err(e) => {
                eprintln!("Ошибка симуляции: {}", e);
            }
        }
    }

    // ── Визуализация ──────────────────────────────────────────────────────────────
    if do_visualize {
        println!("\n=== Визуализация ===");
        match visualize_all(&unit, &args.output_dir) {
            Ok(()) => {}
            Err(e) => eprintln!("Ошибка визуализации: {}", e),
        }
    }

    // ── Кодогенерация ────────────────────────────────────────────────────────────
    if do_gen_c || do_gen_verilog || do_gen_st || do_gen_lc3 || do_gen_thumb {
        println!("\n=== Кодогенерация (базовое имя: {}) ===", base_name);
        let indent = if args.indent_tab {
            IndentStyle::Tab
        } else {
            IndentStyle::Spaces(args.indent_size)
        };
        let ctx = CodegenContext::from_source_with_indent(&unit, indent);
        generate_outputs(
            &unit,
            &base_name,
            &ctx,
            &args.output_dir,
            do_gen_c,
            do_gen_verilog,
            do_gen_st,
            do_gen_lc3,
            do_gen_thumb,
        );
    }
}

fn generate_outputs(
    unit: &SourceUnit,
    base_name: &str,
    ctx: &CodegenContext,
    out_dir: &Path,
    gen_c: bool,
    gen_verilog: bool,
    gen_st: bool,
    gen_lc3: bool,
    gen_thumb: bool,
) {
    let output = AllOutput::generate_with_ctx(unit, base_name, ctx);

    if gen_c {
        // Один .h + один .c для всех моделей
        let c_dir = out_dir.join("c");
        let _ = fs::create_dir_all(&c_dir);
        write_file(&c_dir.join(format!("{}.h", base_name)), &output.c.0);
        write_file(&c_dir.join(format!("{}.c", base_name)), &output.c.1);
    }

    if gen_verilog {
        // Один .v файл для всех модулей
        let v_dir = out_dir.join("verilog");
        let _ = fs::create_dir_all(&v_dir);
        write_file(&v_dir.join(format!("{}.v", base_name)), &output.verilog);
    }

    if gen_st {
        // Один .FB.DECL.st + один .FB.PRGS.st для всех моделей
        let st_dir = out_dir.join("st");
        let _ = fs::create_dir_all(&st_dir);
        write_file(&st_dir.join(format!("{}.FB.DECL.st", base_name)), &output.st.0);
        write_file(&st_dir.join(format!("{}.FB.PRGS.st", base_name)), &output.st.1);
    }

    if gen_lc3 {
        // Один .asm файл для всех моделей
        let lc3_dir = out_dir.join("lc3");
        let _ = fs::create_dir_all(&lc3_dir);
        write_file(&lc3_dir.join(format!("{}.asm", base_name)), &output.lc3);
    }

    if gen_thumb {
        // Один .S файл для всех моделей
        let thumb_dir = out_dir.join("thumb");
        let _ = fs::create_dir_all(&thumb_dir);
        write_file(&thumb_dir.join(format!("{}.S", base_name)), &output.thumb);
    }
}

fn write_file(path: &Path, content: &str) {
    match fs::write(path, content) {
        Ok(()) => println!("[codegen] Written: {}", path.display()),
        Err(e) => eprintln!("[codegen] Ошибка записи {}: {}", path.display(), e),
    }
}

fn parse_value(s: &str) -> Value {
    if let Ok(n) = s.parse::<i64>() {
        return Value::Int(n);
    }
    if let Ok(f) = s.parse::<f64>() {
        return Value::Real(f);
    }
    match s {
        "true" | "1" => Value::Bool(true),
        "false" | "0" => Value::Bool(false),
        _ => Value::Str(s.to_string()),
    }
}
