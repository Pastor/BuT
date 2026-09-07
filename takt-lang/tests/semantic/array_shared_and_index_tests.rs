//! Массив как общая переменная в цели `st` и индекс-выражение.
//!
//! ## Три предмета, связанные причинно
//!
//! 1. **Общий массив давал невалидный ST.** Файл, где под-модели общаются через
//!    `var mem: [u8;4]` корня, компилировался, но `iec2c` его отвергал:
//!    "Data type incompatibility between parameter 'mem' and value being
//!    passed". То есть `taktc` рапортовал об успехе, а арбитром оказывался
//!    чужой инструмент.
//!
//! Причина найдена пробой: MatIEC не принимает **анонимный** `ARRAY [...] OF T` в
//! объявлении параметра. С именованным типом тот же файл принимается. Именованным
//! обязан быть **и параметр, и переменная владельца**: типы сверяются, и половинчатая
//! правка оставляет ту же ошибку.
//!
//! 2. **Индекс был только литералом или именем**: `mem[pc + 1]` -> `SY-002`.
//!    Ограничение жило в грамматике и не следовало ни из семантики, ни из
//!    возможностей целей - все печатники уже рекурсивны.
//!
//! 3. **Переменная-индекс не считалась использованной** (находка фичи):
//!    `got := mem[pc];` давал ложное `SE-036` "переменная 'pc' объявлена, но
//!    нигде не используется". После снятия ограничения (2) это стало бы
//!    случаться чаще - в индексе появляются целые выражения.

use std::path::{Path, PathBuf};
use std::process::Command;
use takt_lang::generator::GenerateOptions;

fn tmp(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("main")
        .replace("::", "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0210_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог теста");
    dir
}

/// Общий массив: корень объявляет, под-модель читает.
const SHARED_ARRAY: &str = r#"
var mem: [u8; 4] := {1, 2, 3, 4};

model Reader {
    var got: u8 := 0;
    start Read {
        always { got := mem[1]; }
        ref Read: got = 0;
    }
}

start Root = Reader;
"#;

/// Индекс-выражение в теле и в условии; `pc` используется **только** индексом.
const INDEX_EXPR: &str = r#"
var mem: [u8; 4] := {10, 20, 30, 40};
var pc: u8 := 0;
var got: u8 := 0;

start Run {
    always { got := mem[pc + 1]; }
    ref Done: mem[pc + 1] > 15;
}

state Done { }
"#;

fn compile_st(tag: &str, name: &str, source: &str) -> (PathBuf, String) {
    let dir = tmp(tag);
    takt_lang::compile_to_st(
        &format!("{name}.takt"),
        source,
        dir.to_str().expect("путь"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("цель st");
    let text = std::fs::read_to_string(dir.join(format!("{name}.st"))).expect("вывод .st");
    (dir, text)
}

fn tool_available(cmd: &str, probe: &str) -> bool {
    Command::new(cmd)
        .arg(probe)
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Прогоняет `iec2c` - **арбитра валидности ST**.
///
/// Отсутствие инструмента - мягкий пропуск, как в предкоммите: локально его может не
/// быть, а в CI проверка строгий.
fn assert_st_valid(dir: &Path, name: &str) {
    let iec2c = std::env::var("HOME")
        .map(|h| PathBuf::from(h).join(".local/bin/iec2c"))
        .unwrap_or_else(|_| PathBuf::from("iec2c"));
    let lib = std::env::var("HOME")
        .map(|h| PathBuf::from(h).join(".local/share/matiec/lib"))
        .unwrap_or_default();
    if !iec2c.exists() && !tool_available("iec2c", "-h") {
        eprintln!("[ПРОПУСК] iec2c недоступен — ST не проверен арбитром");
        return;
    }
    let out = Command::new(&iec2c)
        .arg("-I")
        .arg(&lib)
        .arg("-T")
        .arg(dir)
        .arg(dir.join(format!("{name}.st")))
        .output()
        .expect("запуск iec2c");
    assert!(
        out.status.success(),
        "порождённый ST не принят iec2c:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
}

/// `iec2c` принимает вывод с общим массивом.
///
/// Главная проверка фичи: до неё компилятор рапортовал об успехе, а этот прогон падал.
#[test]
fn shared_array_output_is_accepted_by_iec2c() {
    let (dir, _) = compile_st("shared_iec", "shared_arr", SHARED_ARRAY);
    assert_st_valid(&dir, "shared_arr");
}

/// Массив объявлен **именованным** типом - и у владельца, и в параметре.
#[test]
fn shared_array_uses_a_named_type_on_both_sides() {
    let (_, text) = compile_st("shared_named", "shared_arr", SHARED_ARRAY);
    assert!(
        text.contains("SharedArr_mem_arr : ARRAY [0..3] OF USINT;"),
        "тип массива обязан быть объявлен именованным:\n{text}"
    );
    // Две стороны: параметр под-модели и переменная корня. Половинчатая правка (только
    // параметр) оставляет ту же ошибку iec2c - проба это показала.
    //
    // Совпадение - без завершающей `;`: у переменной корня с печатается ещё и
    // инициализатор (`:= [1, 2, 3, 4]`), и точное совпадение со скобкой считало бы
    // только сторону параметра.
    assert_eq!(
        text.matches("mem : SharedArr_mem_arr").count(),
        2,
        "именованным типом обязаны быть объявлены обе стороны:\n{text}"
    );
    assert!(
        !text.contains("mem : ARRAY [0..3] OF USINT;"),
        "анонимного объявления массива в параметре остаться не должно:\n{text}"
    );
}

/// Индекс-выражение принимается - в теле и в условии.
#[test]
fn index_expression_is_accepted() {
    let dir = tmp("index_c");
    takt_lang::compile_to_c(
        "idx.takt",
        INDEX_EXPR,
        dir.to_str().expect("путь"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("цель c принимает индекс-выражение");
    let text = std::fs::read_to_string(dir.join("idx.c")).expect("вывод .c");
    assert!(
        text.contains("model->got = model->mem[model->pc + 1];"),
        "индекс печатается выражением:\n{text}"
    );
    assert!(
        text.contains("model->mem[model->pc + 1] > 15"),
        "то же в условии перехода:\n{text}"
    );
}

/// И остальные цели переводят его согласованно.
#[test]
fn index_expression_translates_in_every_target() {
    let (_, st) = compile_st("index_st", "idx", INDEX_EXPR);
    assert!(st.contains("got := mem[pc + 1];"), "цель st:\n{st}");

    let dir = tmp("index_rust");
    takt_lang::compile_to_rust(
        "idx.takt",
        INDEX_EXPR,
        dir.to_str().expect("путь"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("цель rust");
    let rs = std::fs::read_to_string(dir.join("idx.rs")).expect("вывод .rs");
    assert!(
        rs.contains("self.mem[self.pc.wrapping_add(1) as usize]"),
        "цель rust:\n{rs}"
    );
}

/// Срез `[l:r]` не задет правкой индекса - **разбирается** по-прежнему.
///
/// Проверяется именно разбор, а не перевод: цель `c` срез не поддерживает
/// вовсе ("ArraySlice не поддерживается в C генераторе", причём диагностика
/// **без кода** - класс ). Это положение дел до, и трогать
/// его она не вправе; здесь важно одно: грамматика среза не сломана соседней
/// правкой индекса.
#[test]
fn slice_is_still_parsed() {
    let source = r#"
var bits: [bit; 8] := {0, 0, 0, 0, 0, 0, 0, 0};
var part: [bit; 4] := {0, 0, 0, 0};
var n: u8 := 0;

start Run {
    always { part := bits[0:3]; n := n + 1; }
    ref Run: n < 3;
}
"#;
    takt_lang::parse(source, 0).expect("срез обязан разбираться по-прежнему");
}

/// Предупреждения компилятора - **той же точкой**, которой пользуется CLI
/// (`collect_model_warnings`).
///
/// `collect_compile_diagnostics` здесь не годится: он собирает **ошибки**, а
/// предупреждения идут отдельным каналом. Тест, зовущий его, проходил бы
/// на пустом списке, то есть не проверяла ничего.
fn warnings_of(source: &str) -> Vec<takt_lang::diagnostics::Diagnostic> {
    let (ast, _) = takt_lang::parse(source, 0).expect("разбор");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("дерево");
    takt_lang::semantic::warnings::collect_model_warnings(&ast, &model)
}

/// Переменная, использованная **только индексом**, не даёт `SE-036`.
///
/// Проверяются **обе** позиции: тело и условие. Сборщиков использований в
/// `semantic/unused.rs` четыре (два по выражениям, два по условиям), и правка одного
/// оставляла ложное предупреждение в другой позиции.
#[test]
fn index_variable_counts_as_used() {
    let warnings = warnings_of(INDEX_EXPR);
    let unused: Vec<_> = warnings
        .iter()
        .filter(|d| d.code.as_deref() == Some("SE-036"))
        .collect();
    assert!(
        unused.is_empty(),
        "переменная-индекс использована — ложного SE-036 быть не должно: {unused:?}"
    );
}

/// По-настоящему неиспользуемая переменная предупреждение
/// по-прежнему даёт - правка не глушит проверку целиком.
#[test]
fn genuinely_unused_variable_still_warns() {
    let source = r#"
var mem: [u8; 4] := {1, 2, 3, 4};
var idle: u8 := 0;
var got: u8 := 0;

start Run {
    always { got := mem[1]; }
    ref Run: got = 0;
}
"#;
    let warnings = warnings_of(source);
    assert!(
        warnings
            .iter()
            .any(|d| d.code.as_deref() == Some("SE-036") && d.message.contains("idle")),
        "неиспользуемая переменная обязана давать SE-036: {warnings:?}"
    );
}

/// Порождённый Rust с переменной-индексом **компилируется**.
///
/// Это не косметика, а исправление другой цели: переменная, использованная только
/// индексом, не считалась использованной, не попадала в общую структуру - и печатник
/// ссылался на `self.i`, которого нет:
///
/// ```text
/// error[E0609]: no field `i` on type `&mut RpM`
/// ```
///
/// Проверка цели `rust` гоняет **только корпус**, где такой формы нет, а текстовый тест
/// печатника это ожидание **закреплял**. Тест - настоящий `rustc`, а не сравнение
/// строк.
#[test]
fn generated_rust_with_index_variable_compiles() {
    let source = "out o: bit; var xs: [u8;4] := {1, 2, 3, 4}; var i: u8 := 0;\n\
                  model M { start S { always { o := 1; } ref T: xs[i] > 0; } state T; }\n\
                  start Main = M;";
    let dir = tmp("rustc_index");
    takt_lang::compile_to_rust(
        "idxvar.takt",
        source,
        dir.to_str().expect("путь"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("цель rust");

    if !tool_available("rustc", "--version") {
        eprintln!("[ПРОПУСК] rustc недоступен — вывод не проверен компилятором");
        return;
    }
    let out = Command::new("rustc")
        .arg("--edition")
        .arg("2021")
        .arg("--crate-type")
        .arg("lib")
        .arg("-o")
        .arg(dir.join("libidxvar.rlib"))
        .arg(dir.join("idxvar.rs"))
        .current_dir(&dir)
        .output()
        .expect("запуск rustc");
    assert!(
        out.status.success(),
        "порождённый Rust обязан компилироваться:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
}
