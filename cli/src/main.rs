use std::fs;
use std::path::{Path, PathBuf};

use clap::Parser;

use but_codegen::{AllOutput, CodegenContext, IndentStyle};
use but_grammar::ast::SourceUnit;
use but_middleware::include::IncludeResolver;
use but_simulators::{build_all, Simulator, Value};
use but_visual::visualize_all;

/// BuT FSM compiler and simulator
#[derive(Parser, Debug)]
#[command(name = "but", about = "BuT FSM toolchain: simulation, visualization, code generation")]
struct Args {
    /// Input .but file
    #[arg(value_name = "FILE")]
    source: PathBuf,

    /// Run simulation
    #[arg(long, short = 's')]
    simulate: bool,

    /// Number of simulation steps
    #[arg(long, default_value_t = 10)]
    steps: usize,

    /// Set port values (format: name=value), can be repeated
    #[arg(long, value_name = "NAME=VALUE")]
    port: Vec<String>,

    /// Generate DOT/PNG visualization
    #[arg(long, short = 'v')]
    visualize: bool,

    /// Generate C code
    #[arg(long)]
    gen_c: bool,

    /// Generate Verilog code
    #[arg(long)]
    gen_verilog: bool,

    /// Generate Structured Text (IEC 61131-3)
    #[arg(long)]
    gen_st: bool,

    /// Generate LC3 assembly
    #[arg(long)]
    gen_lc3: bool,

    /// Generate ARM Thumb assembly
    #[arg(long)]
    gen_thumb: bool,

    /// Generate all outputs (simulation + visualization + all generators)
    #[arg(long, short = 'a')]
    all: bool,

    /// Output directory
    #[arg(long, short = 'o', default_value = "gen")]
    output_dir: PathBuf,

    /// Additional directories to search for include files (.but).
    /// Can be specified multiple times: -I ./lib -I ./shared
    /// Search order: source file directory -> specified directories (left to right).
    #[arg(long = "include-dir", short = 'I', value_name = "DIR")]
    include_dirs: Vec<PathBuf>,

    /// Number of spaces per indentation level in generated code (default: 4).
    /// Ignored when --indent-tab is used.
    #[arg(long, default_value_t = 4, value_name = "N")]
    indent_size: usize,

    /// Use a tab character instead of spaces for indentation in generated code.
    #[arg(long)]
    indent_tab: bool,
}

fn main() {
    let args = Args::parse();

    // Read the source file
    let source_text = match fs::read_to_string(&args.source) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading {}: {}", args.source.display(), e);
            std::process::exit(1);
        }
    };

    // Base file name for naming output files (without extension)
    let base_name = args
        .source
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("model")
        .to_string();

    // Parse
    let (unit, _comments) = match but_grammar::parse(&source_text, 0) {
        Ok(r) => r,
        Err(diagnostics) => {
            for d in &diagnostics {
                eprintln!("Parse error: {:?}", d);
            }
            std::process::exit(1);
        }
    };

    // Resolve import directives (recursive file inclusion)
    let unit = {
        let mut resolver = IncludeResolver::from_file(&args.source);
        for dir in &args.include_dirs {
            if dir.is_dir() {
                resolver.add_search_path(dir.clone());
            } else {
                eprintln!("Warning: directory not found: {}", dir.display());
            }
        }
        match resolver.resolve(unit, &args.source) {
            Ok(merged) => merged,
            Err(errs) => {
                for e in &errs {
                    eprintln!("Import error: {}", e);
                }
                std::process::exit(1);
            }
        }
    };

    // Semantic analysis (continue even with warnings)
    let unit = match but_middleware::processing(unit) {
        Ok(unit) => unit,
        Err(diagnostics) => {
            eprintln!("[warning] Semantic diagnostics:");
            for d in &diagnostics {
                eprintln!("  {:?}", d);
            }
            std::process::exit(1);
        }
    };

    // Create the output directory
    if let Err(e) = fs::create_dir_all(&args.output_dir) {
        eprintln!("Cannot create output directory {}: {}", args.output_dir.display(), e);
        std::process::exit(1);
    }

    let do_simulate = args.simulate || args.all;
    let do_visualize = args.visualize || args.all;
    let do_gen_c = args.gen_c || args.all;
    let do_gen_verilog = args.gen_verilog || args.all;
    let do_gen_st = args.gen_st || args.all;
    let do_gen_lc3 = args.gen_lc3 || args.all;
    let do_gen_thumb = args.gen_thumb || args.all;

    // -- Simulation -------------------------------------------------------------------
    if do_simulate {
        println!("\n=== Simulation ===");
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
                        "Initial state: {}",
                        sim.current_state().unwrap_or("<unknown>")
                    );

                    for step in 0..args.steps {
                        let changed = sim.step();
                        let state = sim.current_state().unwrap_or("<done>");
                        if changed {
                            println!("  Step {}: -> {}", step + 1, state);
                        }
                    }

                    println!("Final state: {}", sim.current_state().unwrap_or("<unknown>"));
                    sim.print_ltl();
                    sim.print_state();
                }
            }
            Err(e) => {
                eprintln!("Simulation error: {}", e);
            }
        }
    }

    // -- Visualization ----------------------------------------------------------------
    if do_visualize {
        println!("\n=== Visualization ===");
        match visualize_all(&unit, &args.output_dir) {
            Ok(()) => {}
            Err(e) => eprintln!("Visualization error: {}", e),
        }
    }

    // -- Code generation --------------------------------------------------------------
    if do_gen_c || do_gen_verilog || do_gen_st || do_gen_lc3 || do_gen_thumb {
        println!("\n=== Code generation (base name: {}) ===", base_name);
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
        // One .h + one .c for all models
        let c_dir = out_dir.join("c");
        let _ = fs::create_dir_all(&c_dir);
        write_file(&c_dir.join(format!("{}.h", base_name)), &output.c.0);
        write_file(&c_dir.join(format!("{}.c", base_name)), &output.c.1);
    }

    if gen_verilog {
        // One .v file for all modules
        let v_dir = out_dir.join("verilog");
        let _ = fs::create_dir_all(&v_dir);
        write_file(&v_dir.join(format!("{}.v", base_name)), &output.verilog);
    }

    if gen_st {
        // One .FB.DECL.st + one .FB.PRGS.st for all models
        let st_dir = out_dir.join("st");
        let _ = fs::create_dir_all(&st_dir);
        write_file(&st_dir.join(format!("{}.FB.DECL.st", base_name)), &output.st.0);
        write_file(&st_dir.join(format!("{}.FB.PRGS.st", base_name)), &output.st.1);
    }

    if gen_lc3 {
        // One .asm file for all models
        let lc3_dir = out_dir.join("lc3");
        let _ = fs::create_dir_all(&lc3_dir);
        write_file(&lc3_dir.join(format!("{}.asm", base_name)), &output.lc3);
    }

    if gen_thumb {
        // One .S file for all models
        let thumb_dir = out_dir.join("thumb");
        let _ = fs::create_dir_all(&thumb_dir);
        write_file(&thumb_dir.join(format!("{}.S", base_name)), &output.thumb);
    }
}

fn write_file(path: &Path, content: &str) {
    match fs::write(path, content) {
        Ok(()) => println!("[codegen] Written: {}", path.display()),
        Err(e) => eprintln!("[codegen] Write error {}: {}", path.display(), e),
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
