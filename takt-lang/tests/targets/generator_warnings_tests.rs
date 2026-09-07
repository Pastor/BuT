//! Предупреждения генераторов возвращаются вызывающему -.
//!
//! ## Что здесь ловится
//!
//! Три цели (`st`, `rust`, `sv`) заканчивали генерацию собственной копией функции
//! `report`, которая печатала `eprintln!` **прямо из библиотеки**:
//!
//! ```text
//! Предупреждение [ST-022]: охранная формула не транслируется целью 'st'…
//! ```
//!
//! Копий было три потому, что другого выхода наружу у генератора **не было по типу**:
//! `Generator::generate` и публичные входы возвращали `Result<(), Diagnostic>`.
//! Следствия замерены пробой (2026-08-16):
//!
//! - `--quiet` такой вывод **не глушил** (флаг документирован как "только
//!   ошибки");
//! - формат разошёлся с общим: библиотечная копия печатала только код и текст,
//!   тогда как CLI печатает позицию, код и текст;
//! - у цели `sv-mmio` уживались **два канала**: адресные предупреждения
//!   возвращались (и глушились), `SV-009` печаталась (и не глушилась);
//! - тест мог проверить факт предупреждения только перехватом stderr либо
//!   инспекцией внутреннего приёмника.
//!
//! Теперь предупреждения - **часть результата**, и эти тесты читают их как значение.

use std::path::PathBuf;
use takt_lang::diagnostics::Diagnostic;
use takt_lang::generator::GenerateOptions;

/// Каталог теста уникален по имени потока: тесты идут параллельно.
fn tmp(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("main")
        .replace("::", "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0168_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог теста");
    dir
}

fn codes(warnings: &[Diagnostic]) -> Vec<String> {
    warnings
        .iter()
        .filter_map(|w| w.code.clone())
        .collect::<Vec<_>>()
}

/// `ST-022`: охранная формула не переводится в IEC 61131-3.
const GUARD: &str = r#"
var level: u8 := 0;

invariant Safe = level < 3;

start Run {
    always { level := level + 1; }
    ref Run;
}
"#;

/// `ST-009`: тело внешней функции неизвестно - эмитируется заглушка.
const EXTERN_FN: &str = r#"
extern fn sensor() -> u8;

var n: u8 := 0;

start Run {
    always { n := sensor(); }
    ref Run;
}
"#;

/// `ST-010`: LTL-формула в **теле блока** - приёмник, отличный от того, где рождаются
/// `ST-009`/`ST-022`.
///
/// Этот вход обязателен: у цели `st` предупреждения собираются в **трёх** местах, и
/// мутация "не собирать вклад `emit_function_block`" проходила мимо тестов, пока
/// фикстуры задевали лишь два из них.
const LTL_IN_BODY: &str = r#"
var n: u8 := 0;

start Run {
    always {
        : [LTL] G (Run -> F Done);
        n := n + 1;
    }
    ref Done: n >= 3;
}

state Done { }
"#;

/// `SV-009`: деление по переменному делителю.
const VAR_DIVIDER: &str = r#"
var a: u8 := 10;
var b: u8 := 2;
var c: u8 := 0;

start Run {
    always { c := a / b; }
    ref Run;
}
"#;

/// `SV-009` у адрес-потребляющей цели: раньше он шёл мимо возвращаемого списка.
const VAR_DIVIDER_MMIO: &str = r#"
in A: u8 at 0x1000;
var b: u8 := 2;
var c: u8 := 0;

start Run {
    always { c := A / b; }
    ref Run;
}
"#;

/// R1/R7 (`ST-022`): цель `st` **возвращает** предупреждение об охранной формуле.
#[test]
fn st_returns_guard_warning() {
    let dir = tmp("st_guard");
    let warnings = takt_lang::compile_to_st(
        "guard.takt",
        GUARD,
        dir.to_str().expect("путь"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("цель st");
    assert!(
        codes(&warnings).iter().any(|c| c == "ST-022"),
        "ожидалась ST-022, получено: {:?}",
        codes(&warnings)
    );
}

/// R1/R7 (`ST-009`): заглушка внешней функции - тоже возвращается.
#[test]
fn st_returns_extern_stub_warning() {
    let dir = tmp("st_extern");
    let warnings = takt_lang::compile_to_st(
        "ext.takt",
        EXTERN_FN,
        dir.to_str().expect("путь"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("цель st");
    assert!(
        codes(&warnings).iter().any(|c| c == "ST-009"),
        "ожидалась ST-009, получено: {:?}",
        codes(&warnings)
    );
}

/// R7 (`ST-010`): предупреждение **тела блока** тоже доезжает.
///
/// Приёмников у цели `st` три (функции, тела блоков, конфигурация), и вклад каждого
/// обязан быть собран. Мутация "`emit_function_block` не учитывается" падает **только**
/// здесь - прочие фикстуры её не задевают.
#[test]
fn st_returns_body_warning() {
    let dir = tmp("st_body");
    let warnings = takt_lang::compile_to_st(
        "ltl_body.takt",
        LTL_IN_BODY,
        dir.to_str().expect("путь"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("цель st");
    assert!(
        codes(&warnings).iter().any(|c| c == "ST-010"),
        "ожидалась ST-010 из тела блока, получено: {:?}",
        codes(&warnings)
    );
}

/// R1/R7 (`SV-009`): цель `sv` возвращает предупреждение о переменном делителе.
#[test]
fn sv_returns_divider_warning() {
    let dir = tmp("sv_div");
    let warnings = takt_lang::compile_to_sv(
        "div.takt",
        VAR_DIVIDER,
        dir.to_str().expect("путь"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("цель sv");
    assert!(
        codes(&warnings).iter().any(|c| c == "SV-009"),
        "ожидалась SV-009, получено: {:?}",
        codes(&warnings)
    );
}

/// R6: у `sv-mmio` больше **одна** судьба у всей диагностики - предупреждение
/// генератора приходит тем же списком, что и адресные.
#[test]
fn sv_mmio_returns_generator_warning_too() {
    let dir = tmp("mmio_div");
    let warnings = takt_lang::compile_to_sv_mmio(
        "div_mmio.takt",
        VAR_DIVIDER_MMIO,
        dir.to_str().expect("путь"),
        &[],
        &[],
        &Default::default(),
        &GenerateOptions::default(),
    )
    .expect("цель sv-mmio");
    assert!(
        codes(&warnings).iter().any(|c| c == "SV-009"),
        "предупреждение генератора обязано прийти вместе с адресными, получено: {:?}",
        codes(&warnings)
    );
}

/// R1: цели без собственных предупреждений возвращают **пустой** список, а не
/// отсутствие канала. Появится первое предупреждение цели `c` - поедет по нему без
/// смены сигнатуры.
#[test]
fn c_and_plantuml_return_an_empty_channel() {
    let dir = tmp("c_empty");
    let path = dir.to_str().expect("путь");
    let c = takt_lang::compile_to_c(
        "plain.takt",
        VAR_DIVIDER,
        path,
        &[],
        &GenerateOptions::default(),
    )
    .expect("цель c");
    assert!(c.is_empty(), "у цели c предупреждений нет: {:?}", codes(&c));

    let puml = takt_lang::compile_to_plantuml("plain.takt", VAR_DIVIDER, path, &[])
        .expect("цель plantuml");
    assert!(
        puml.is_empty(),
        "у цели plantuml предупреждений нет: {:?}",
        codes(&puml)
    );
}

/// R2: библиотека не разговаривает с пользователем - печати из `generator/` не
/// осталось.
///
/// Греп по исходнику, а не по поведению: перехватить stderr из процесса теста нельзя
/// надёжно, а `report` был именно текстом в трёх модулях. Тест падает **списком** мест,
/// чтобы четвёртая копия не завелась незамеченной.
#[test]
fn generator_does_not_print() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("src/generator");
    let mut offenders = Vec::new();
    let mut stack = vec![root];
    while let Some(dir) = stack.pop() {
        for entry in std::fs::read_dir(&dir).expect("каталог generator/") {
            let path = entry.expect("запись каталога").path();
            if path.is_dir() {
                stack.push(path);
                continue;
            }
            if path.extension().and_then(|e| e.to_str()) != Some("rs") {
                continue;
            }
            let text = std::fs::read_to_string(&path).expect("файл читается");
            // Тестовый код модуля отсекается по `#[cfg(test)]`: в нём `eprintln!` -
            // отладочный вывод пробы, а не канал диагностики.
            let code = text.split("#[cfg(test)]").next().unwrap_or(&text);
            // Комментарии отсекаются тоже - иначе тест ловит **рассказ** о прежней
            // печати.
            let prints = code.lines().any(|line| {
                let trimmed = line.trim_start();
                !trimmed.starts_with("//")
                    && (trimmed.contains("eprintln!") || trimmed.contains("println!"))
            });
            if prints {
                offenders.push(path.display().to_string());
            }
        }
    }
    assert!(
        offenders.is_empty(),
        "генератор печатает из библиотеки — диагностику возвращают вызывающему \
         (фича 0168, правило 29): {offenders:?}"
    );
}

// --- Доставка до пользователя: CLI --------------------------------------------

/// stderr прогона `taktc compile` над временным файлом.
fn compile_stderr(tag: &str, source: &str, target: &str, extra: &[&str]) -> String {
    let dir = tmp(tag);
    let file = dir.join("probe.takt");
    std::fs::write(&file, source).expect("запись пробы");
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_taktc"))
        .arg("compile")
        .arg("-t")
        .arg(target)
        .args(extra)
        .arg(&file)
        .arg("-o")
        .arg(dir.join("out"))
        .output()
        .expect("запуск taktc compile");
    String::from_utf8_lossy(&out.stderr).into_owned()
}

/// R3: `--quiet` глушит предупреждение **цели**, как и предупреждение компилятора.
#[test]
fn quiet_silences_target_warnings() {
    let quiet = compile_stderr("cli_quiet_st", GUARD, "st", &["--quiet"]);
    assert!(
        quiet.trim().is_empty(),
        "в тихом режиме stderr обязан быть пуст, получено:\n{quiet}"
    );

    let quiet_sv = compile_stderr("cli_quiet_sv", VAR_DIVIDER, "sv", &["--quiet"]);
    assert!(
        quiet_sv.trim().is_empty(),
        "цель sv в тихом режиме тоже молчит, получено:\n{quiet_sv}"
    );
}

/// R4/R7: без флага предупреждение печатается - и **общим** форматом, тем же, каким
/// печатаются предупреждения компилятора (позиция, код, текст).
///
/// Проверяется не только факт, но и формат: прежняя библиотечная копия печатала
/// "Предупреждение [ST-022]: ..." без позиции, и именно это расхождение теряло
/// координату.
#[test]
fn without_quiet_target_warning_is_printed_in_the_common_format() {
    let out = compile_stderr("cli_loud_st", GUARD, "st", &[]);
    let line = out
        .lines()
        .find(|l| l.contains("ST-022"))
        .unwrap_or_else(|| panic!("ожидалась строка с ST-022, получено:\n{out}"));
    assert!(
        line.contains("probe.takt:") && line.contains("Предупреждение [ST-022]:"),
        "формат обязан совпадать с общим (путь:строка:колонка: Предупреждение [код]: …), \
         получено: {line}"
    );
}
