//! Начальное значение выходного порта в целях `sv`, `sv-mmio`, `st` и `st-at`.
//!
//! # Что доказывается
//!
//! 1. Значение выставляется **до первого такта**: у целей `sv`/`sv-mmio` -
//!    ветвью сброса (иного места нет: там же живут стартовые состояния, контракт
//!    ), у `st` - инициализатором `VAR_OUTPUT` (экземпляр
//!    `FUNCTION_BLOCK` получает его при создании), у `st-at` - инициализатором
//!    размещённой глобальной переменной.
//! 2. Порт **без** `:=` печатается как прежде (`'0` в сбросе, объявление без
//!    инициализатора) - иначе изменился бы вывод всего корпуса.
//! 3. Вход значения не получает ни в одной цели.

use std::path::{Path, PathBuf};
use std::process::Command;
use takt_lang::generator::GenerateOptions;

/// Битовый и числовой выход со значением, вход без значения.
///
/// `level` в теле не пишется намеренно: начальное значение само по себе делает порт
/// задействованным (иначе цель не завела бы для него сигнал).
const WITH_INIT: &str = "in btn: bit at 0x100:0;\n\
                         out ready: bit at 0x200:1 := 1;\n\
                         out level: u8 at 0x201 := 7;\n\
                         var seen: bit := 0;\n\
                         start S { always { seen := btn; ready := seen; } }";

/// Тот же автомат без начальных значений - контрпример "форма прежняя".
const WITHOUT_INIT: &str = "in btn: bit at 0x100:0;\n\
                            out ready: bit at 0x200:1;\n\
                            out level: u8 at 0x201;\n\
                            var seen: bit := 0;\n\
                            start S { always { seen := btn; ready := seen; level := 1; } }";

fn out_dir(tag: &str) -> PathBuf {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0187_04_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    dir
}

/// Компилирует целью `sv` (адреса цель не потребляет - порты остаются выводами).
fn generate_sv(tag: &str, source: &str) -> (PathBuf, String) {
    let dir = out_dir(tag);
    takt_lang::compile_to_sv(
        tag,
        source,
        dir.to_str().expect("путь"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение SV");
    let text = std::fs::read_to_string(dir.join(format!("{tag}.sv"))).expect("чтение .sv");
    (dir, text)
}

/// Компилирует целью `sv-mmio` - адресованные порты становятся битами регистров.
fn generate_sv_mmio(tag: &str, source: &str) -> (PathBuf, String) {
    let dir = out_dir(tag);
    let env = takt_lang::parse_defines(&[]).expect("среда");
    takt_lang::compile_to_sv_mmio(
        tag,
        source,
        dir.to_str().expect("путь"),
        &[],
        &[],
        &env,
        &GenerateOptions::default(),
    )
    .expect("порождение SV (mmio)");
    let text = std::fs::read_to_string(dir.join(format!("{tag}.sv"))).expect("чтение .sv");
    (dir, text)
}

/// Компилирует целью `st` - порты суть входы/выходы `FUNCTION_BLOCK`.
fn generate_st(tag: &str, source: &str) -> (PathBuf, String) {
    let dir = out_dir(tag);
    takt_lang::compile_to_st(
        tag,
        source,
        dir.to_str().expect("путь"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение ST");
    let text = std::fs::read_to_string(dir.join(format!("{tag}.st"))).expect("чтение .st");
    (dir, text)
}

/// Компилирует целью `st-at` - порты суть размещённые глобальные переменные.
fn generate_st_at(tag: &str, source: &str) -> (PathBuf, String) {
    let dir = out_dir(tag);
    let env = takt_lang::parse_defines(&[]).expect("среда");
    takt_lang::compile_to_st_at(
        tag,
        source,
        dir.to_str().expect("путь"),
        &[],
        &[],
        &env,
        &GenerateOptions::default(),
    )
    .expect("порождение ST (at)");
    let text = std::fs::read_to_string(dir.join(format!("{tag}.st"))).expect("чтение .st");
    (dir, text)
}

/// Ветвь сброса порождённого модуля SV.
fn reset_branch(text: &str) -> String {
    let start = text
        .find("if (!rst_n) begin")
        .unwrap_or_else(|| panic!("в выводе нет ветви сброса:\n{text}"));
    let rest = &text[start..];
    let end = rest.find("end else").expect("конец ветви сброса");
    rest[..end].to_string()
}

// -- Цель `sv` ----------------------------------------------------------------

/// Значение выходного порта - в ветви сброса, а не в теле такта.
#[test]
fn sv_resets_output_port_to_initial_value() {
    let (_dir, text) = generate_sv("sv", WITH_INIT);
    let reset = reset_branch(&text);
    assert!(
        reset.contains("ready <= 1;"),
        "битовый выход обязан сбрасываться в своё начальное значение:\n{reset}"
    );
    assert!(
        reset.contains("level <= 7;"),
        "числовой выход обязан сбрасываться в своё начальное значение — \
         в том числе тот, к которому тело автомата не обращается:\n{reset}"
    );
}

/// **Контрпример:** порт без `:=` сбрасывается в ноль, как и прежде.
#[test]
fn sv_port_without_value_resets_to_zero() {
    let (_dir, text) = generate_sv("svnoinit", WITHOUT_INIT);
    let reset = reset_branch(&text);
    assert!(
        reset.contains("ready <= '0;") && reset.contains("level <= '0;"),
        "без начального значения сброс прежний — ноль:\n{reset}"
    );
}

/// Входной порт значения не получает: он вывод модуля, его ведёт окружение.
#[test]
fn sv_input_port_is_not_reset() {
    let (_dir, text) = generate_sv("svin", WITH_INIT);
    let reset = reset_branch(&text);
    assert!(
        !reset.contains("btn <="),
        "входной порт в ветви сброса не пишется:\n{reset}"
    );
}

// -- Цель `sv-mmio` -----------------------------------------------------------

/// Адресованный `out`-порт - бит регистрового файла; сбрасывается в значение.
#[test]
fn sv_mmio_resets_register_bits_to_initial_value() {
    let (_dir, text) = generate_sv_mmio("mmio", WITH_INIT);
    let reset = reset_branch(&text);
    assert!(
        reset.contains("ready <= 1;") && reset.contains("level <= 7;"),
        "биты регистрового файла обязаны сбрасываться в начальные значения:\n{reset}"
    );
}

// -- Цель `st` ----------------------------------------------------------------

/// Значение - инициализатор `VAR_OUTPUT`: экземпляр FB получает его при создании.
#[test]
fn st_declares_output_with_initial_value() {
    let (_dir, text) = generate_st("st", WITH_INIT);
    assert!(
        text.contains("ready : BOOL := TRUE;"),
        "битовый выход обязан объявляться со значением (BOOL — не 0/1):\n{text}"
    );
    assert!(
        text.contains("level : USINT := 7;"),
        "числовой выход обязан объявляться со значением:\n{text}"
    );
}

/// **Контрпример:** без `:=` объявление прежнее - без инициализатора.
#[test]
fn st_port_without_value_has_no_initializer() {
    let (_dir, text) = generate_st("stnoinit", WITHOUT_INIT);
    assert!(
        text.contains("ready : BOOL;") && text.contains("level : USINT;"),
        "без начального значения объявление порта не меняется:\n{text}"
    );
}

/// Входной порт инициализатора не получает.
#[test]
fn st_input_port_has_no_initializer() {
    let (_dir, text) = generate_st("stin", WITH_INIT);
    assert!(
        text.contains("btn : BOOL;"),
        "входной порт объявляется без значения:\n{text}"
    );
}

// -- Цель `st-at` -------------------------------------------------------------

/// Значение стоит на **размещённой глобальной** переменной, а не на `VAR_EXTERNAL`
/// блока: инициализатор внешнего объявления стандарт запрещает.
#[test]
fn st_at_places_initial_value_on_global_not_external() {
    let (_dir, text) = generate_st_at("stat", WITH_INIT);
    assert!(
        text.contains("ready AT %QX512.1 : BOOL := TRUE;"),
        "значение обязано стоять на размещённой глобальной переменной:\n{text}"
    );
    assert!(
        text.contains("level AT %QB513 : USINT := 7;"),
        "словная локация тоже несёт значение:\n{text}"
    );
    let external = text
        .find("VAR_EXTERNAL")
        .map(|i| {
            let rest = &text[i..];
            let end = rest.find("END_VAR").expect("конец VAR_EXTERNAL");
            rest[..end].to_string()
        })
        .expect("в выводе st-at есть VAR_EXTERNAL");
    assert!(
        !external.contains(":="),
        "у `VAR_EXTERNAL` инициализатора быть не должно:\n{external}"
    );
}

// -- Проверки: те же инструменты, что в precheck.sh ------------------------------

fn tool_available(tool: &str, arg: &str) -> bool {
    Command::new(tool)
        .arg(arg)
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Линт verilator: ловит комбинационную петлю и узкую ширину перечисления.
fn assert_verilator_clean(dir: &Path, name: &str) {
    if !tool_available("verilator", "--version") {
        eprintln!("[ПРОПУСК] verilator недоступен — sv не проверен линтом");
        return;
    }
    let out = Command::new("verilator")
        .args(["--lint-only", "-Wall"])
        .arg(dir.join(format!("{name}.sv")))
        .output()
        .expect("запуск verilator");
    assert!(
        out.status.success(),
        "порождённый SV не проходит линт:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
}

/// Синтез yosys: ловит то, что verilator принимает молча (`real`, `return`).
fn assert_yosys_synthesizes(dir: &Path, name: &str) {
    if !tool_available("yosys", "-V") {
        eprintln!("[ПРОПУСК] yosys недоступен — sv не проверен синтезом");
        return;
    }
    let script = format!(
        "read_verilog -sv {}; synth -top {}",
        dir.join(format!("{name}.sv")).display(),
        name
    );
    let out = Command::new("yosys")
        .args(["-q", "-p", &script])
        .output()
        .expect("запуск yosys");
    assert!(
        out.status.success(),
        "порождённый SV не синтезируется:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
}

/// Арбитр валидности ST - `iec2c` (мягкий пропуск, если недоступен).
fn assert_st_valid(dir: &Path, name: &str) {
    let iec2c = std::env::var("HOME")
        .map(|h| PathBuf::from(h).join(".local/bin/iec2c"))
        .unwrap_or_else(|_| PathBuf::from("iec2c"));
    let lib = std::env::var("HOME")
        .map(|h| PathBuf::from(h).join(".local/share/matiec/lib"))
        .unwrap_or_default();
    if !iec2c.exists() && !tool_available("iec2c", "-h") {
        eprintln!("[ПРОПУСК] iec2c недоступен — st не проверен арбитром");
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
        "порождённый ST не принят iec2c:\n{}{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
}

/// Модуль SV с начальными значениями портов проходит **оба** проверки.
#[test]
fn generated_sv_passes_both_gates() {
    let (dir, _) = generate_sv("svgate", WITH_INIT);
    assert_verilator_clean(&dir, "svgate");
    assert_yosys_synthesizes(&dir, "svgate");
}

/// Вывод `st` принимается `iec2c`.
#[test]
fn generated_st_passes_iec2c_gate() {
    let (dir, _) = generate_st("stgate", WITH_INIT);
    assert_st_valid(&dir, "stgate");
}

/// Вывод `st-at` принимается `iec2c` - тот самый риск анализа, снятый пробой:
/// инициализатор на размещённой переменной стандартом допускается, и запасной путь
/// "запись первым сканом" не нужен.
#[test]
fn generated_st_at_passes_iec2c_gate() {
    let (dir, _) = generate_st_at("statgate", WITH_INIT);
    assert_st_valid(&dir, "statgate");
}
