//! Анонимные порты: формы, диагностики и поведение целей.
//!
//! # Что проверяется
//!
//! | Что | Как |
//! |---|---|
//! | три формы разбираются в выражении | компиляция целью `c-hal` |
//! | битовая форма разбирается в условии ребра | компиляция целью `c-hal` |
//! | ширина не задана -> `SE-097` | код диагностики |
//! | позиция/ширина поля вне слова -> `SE-098` | код диагностики |
//! | обращение в инициализаторе -> `SE-099` | код диагностики |
//! | запись -> предупреждение `SE-096` с названным выходом | текст диагностики |
//! | голый адресный литерал -> `SY-008` | код диагностики |
//! | оператор без эффекта -> `SY-007` | код диагностики |
//! | `at` и `address` по-прежнему принимают литерал | компиляция |
//! | цели `c`/`rust`/`st`/`sv` отказывают **с причиной** | код диагностики |
//!
//! Проверяется **наличие диагностики**, а не отсутствие вывода: молчаливая потеря
//! оператора - ровно тот дефект, ради которого заведена фича (проба стадии 2: цель `c`
//! теряла `x := 0x105:0;` целиком при рапорте об успехе).

use takt_lang::generator::GenerateOptions;

/// Модель с чтением и записью ячейки.
const READ_WRITE: &str = "model Probe {\n\
                          var seen: u8 := 0;\n\
                          var n: u8 := 0;\n\
                          start Run {\n\
                              always {\n\
                                  #0x2000:0 as u8 := n + 1;\n\
                                  seen := #0x2000:0 as u8;\n\
                                  n := n + 1;\n\
                              }\n\
                              ref Run: n < 10;\n\
                          }\n\
                          }\n\
                          start Main = Probe;\n";

/// Каталог теста уникален по потоку.
fn out_dir(tag: &str) -> std::path::PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0189_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    dir
}

/// Компилирует исходник целью `c-hal`, возвращая порождённый текст.
fn compile_c_hal(tag: &str, src: &str) -> Result<String, Vec<takt_lang::diagnostics::Diagnostic>> {
    let dir = out_dir(tag);
    let env = takt_lang::parse_defines(&[]).expect("среда");
    takt_lang::compile_to_c_hal(
        tag,
        src,
        dir.to_str().expect("путь"),
        &[],
        &[],
        &env,
        &GenerateOptions::default(),
    )
    .map_err(|d| vec![d])?;
    Ok(std::fs::read_to_string(dir.join(format!("{tag}.c"))).expect("порождённый C"))
}

/// Диагностики компиляции (общий вход CLI и LSP - ).
fn diagnostics(src: &str) -> Vec<takt_lang::diagnostics::Diagnostic> {
    takt_lang::collect_compile_diagnostics("probe", src, &[], false)
}

/// Код первой диагностики с указанным префиксом, если она есть.
fn has_code(src: &str, code: &str) -> bool {
    diagnostics(src)
        .iter()
        .any(|d| d.code.as_deref() == Some(code))
}

// --- Формы -------------------------------------------------------------------

/// Три формы обращения разбираются и доезжают до цели `c-hal`.
#[test]
fn three_forms_are_translated() {
    let src = "model Probe {\n\
               var word: u32 := 0;\n\
               var field: u8 := 0;\n\
               var flag: bit := 0;\n\
               start Run {\n\
                   always {\n\
                       word := #0x2000 as u32;\n\
                       field := #0x2004:3 as u8;\n\
                       flag := #0x2008.4;\n\
                   }\n\
                   ref Run: word > 0;\n\
               }\n\
               }\n\
               start Main = Probe;\n";
    let text = compile_c_hal("forms", src).expect("цель c-hal");
    assert!(
        text.contains("volatile uint32_t*)(uintptr_t)0x2000u"),
        "слово читается 32-разрядным доступом: {text}"
    );
    assert!(
        text.contains(">> 3") && text.contains("0x2004"),
        "поле со смещением читается сдвигом: {text}"
    );
    assert!(
        text.contains(">> 4") && text.contains("0x2008"),
        "бит читается сдвигом на свою позицию: {text}"
    );
}

/// Битовая форма законна в условии ребра - грамматика условий своя.
#[test]
fn bit_form_is_allowed_in_edge_condition() {
    let src = "model Probe {\n\
               var n: u8 := 0;\n\
               start Run {\n\
                   always { n := n + 1; }\n\
                   ref Done: #0x2000.3;\n\
               }\n\
               state Done;\n\
               }\n\
               start Main = Probe;\n";
    let text = compile_c_hal("cond", src).expect("цель c-hal");
    assert!(
        text.contains("volatile") && text.contains(">> 3"),
        "условие ребра обязано читать бит ячейки: {text}"
    );
}

// --- Диагностики семантики ---------------------------------------------------

/// Ширина доступа не задана - отказ, а не умолчание.
#[test]
fn width_must_be_explicit() {
    let src = "model Probe {\n\
               var n: u8 := 0;\n\
               start Run { always { n := #0x2000; } ref Run: n < 3; }\n\
               }\n\
               start Main = Probe;\n";
    assert!(has_code(src, "SE-097"), "{:?}", diagnostics(src));
}

/// Поле, выходящее за слово доступа, отвергается.
#[test]
fn field_beyond_word_is_rejected() {
    let src = "model Probe {\n\
               var n: u8 := 0;\n\
               start Run { always { n := #0x2000:60 as u8; } ref Run: n < 3; }\n\
               }\n\
               start Main = Probe;\n";
    assert!(has_code(src, "SE-098"), "{:?}", diagnostics(src));
}

/// Позиция бита, заданная дважды, - ошибка, а не выбор за автора.
#[test]
fn bit_position_twice_is_rejected() {
    let src = "model Probe {\n\
               var b: bit := 0;\n\
               start Run { always { b := #0x2000:3.4; } ref Run: b = 0; }\n\
               }\n\
               start Main = Probe;\n";
    assert!(has_code(src, "SE-098"), "{:?}", diagnostics(src));
}

/// Обращение в инициализаторе объявления - ошибка: содержимое памяти до первого такта
/// неизвестно, и эталон с целью разошлись бы молча.
#[test]
fn access_in_initializer_is_rejected() {
    let src = "model Probe {\n\
               var n: u8 := #0x2000 as u8;\n\
               start Run { ref Run: n < 3; }\n\
               }\n\
               start Main = Probe;\n";
    assert!(has_code(src, "SE-099"), "{:?}", diagnostics(src));
}

/// Запись предупреждает - и **называет способ** предупреждение снять.
#[test]
fn write_warns_and_names_the_way_out() {
    let (ast, _) = takt_lang::parse(READ_WRITE, 0).expect("разбор");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
    let warnings = takt_lang::semantic::warnings::collect_model_warnings(&ast, &model);
    let write = warnings
        .iter()
        .find(|d| d.code.as_deref() == Some("SE-096"))
        .unwrap_or_else(|| panic!("SE-096 не выдано: {warnings:?}"));
    assert!(
        write.message.contains("Объявите именованный порт"),
        "текст обязан называть выход: {}",
        write.message
    );
}

/// Чтение молчит: оно безопаснее записи.
#[test]
fn read_does_not_warn() {
    let src = "model Probe {\n\
               var n: u8 := 0;\n\
               start Run { always { n := #0x2000 as u8; } ref Run: n < 3; }\n\
               }\n\
               start Main = Probe;\n";
    let (ast, _) = takt_lang::parse(src, 0).expect("разбор");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
    let warnings = takt_lang::semantic::warnings::collect_model_warnings(&ast, &model);
    assert!(
        !warnings.iter().any(|d| d.code.as_deref() == Some("SE-096")),
        "чтение предупреждать не должно: {warnings:?}"
    );
}

// --- Ломающие правила --------------------------------------------------------

/// Голый адресный литерал вне позиции размещения - отказ парсера с правилом языка, а не
/// со списком ожидаемых токенов.
#[test]
fn bare_address_literal_in_expression_is_rejected() {
    let src = "model Probe {\n\
               var n: u8 := 0;\n\
               start Run { always { n := 0x2000:0; } ref Run: n < 3; }\n\
               }\n\
               start Main = Probe;\n";
    let found = diagnostics(src);
    let diagnostic = found
        .iter()
        .find(|d| d.code.as_deref() == Some("SY-008"))
        .unwrap_or_else(|| panic!("SY-008 не выдано: {found:?}"));
    assert!(
        diagnostic.message.contains("#0x2000:0 as ТИП"),
        "диагностика обязана называть замену: {}",
        diagnostic.message
    );
}

/// Позиция размещения не задета: `at` и оператор `address` принимают литерал.
#[test]
fn address_placement_still_accepts_literal() {
    let src = "model Probe {\n\
               out led: bit at 0x2000:3;\n\
               in btn: bit;\n\
               address btn = 0x2004:1;\n\
               start Run { always { led := 1; } ref Run: btn = 1; }\n\
               }\n\
               start Main = Probe;\n";
    let found = diagnostics(src);
    assert!(
        found.iter().all(|d| d.code.as_deref() != Some("SY-008")),
        "размещение обязано остаться законным: {found:?}"
    );
}

/// Выражение без эффекта в позиции оператора - отказ парсера (`SY-007`).
#[test]
fn statement_without_effect_is_rejected() {
    let src = "model Probe {\n\
               var n: u8 := 0;\n\
               start Run { always { n + 1; } ref Run: n < 3; }\n\
               }\n\
               start Main = Probe;\n";
    assert!(has_code(src, "SY-007"), "{:?}", diagnostics(src));
}

/// Обособленное обращение к ячейке - тот же отказ: чтение без потребителя эффекта не
/// имеет (буква требования ).
#[test]
fn bare_anon_access_statement_is_rejected() {
    let src = "model Probe {\n\
               var n: u8 := 0;\n\
               start Run { always { #0x2000.4; } ref Run: n < 3; }\n\
               }\n\
               start Main = Probe;\n";
    assert!(has_code(src, "SY-007"), "{:?}", diagnostics(src));
}

/// Вызов функции остаётся законным оператором: у него есть действие.
#[test]
fn function_call_remains_a_statement() {
    let src = "model Probe {\n\
               extern fn beep();\n\
               var n: u8 := 0;\n\
               start Run { always { beep(); n := n + 1; } ref Run: n < 3; }\n\
               }\n\
               start Main = Probe;\n";
    let found = diagnostics(src);
    assert!(
        found.iter().all(|d| d.code.as_deref() != Some("SY-007")),
        "вызов — оператор с эффектом: {found:?}"
    );
}

// --- Поведение целей ---------------------------------------------------------

/// Цель `c` отказывает **с причиной**, а не теряет оператор молча.
#[test]
fn plain_c_refuses_with_reason() {
    let dir = out_dir("plain_c");
    let error = takt_lang::compile_to_c(
        "probe",
        READ_WRITE,
        dir.to_str().expect("путь"),
        &[],
        &GenerateOptions::default(),
    )
    .expect_err("цель c обязана отказать");
    assert_eq!(error.code.as_deref(), Some("CC-021"), "{error:?}");
    assert!(
        error.message.contains("c-hal"),
        "отказ обязан называть, чем собирать: {}",
        error.message
    );
}

/// Цель `rust` отказывает: порт у неё - метод HAL-трейта, адресов она не знает.
#[test]
fn rust_refuses_with_reason() {
    let dir = out_dir("rust");
    let error = takt_lang::compile_to_rust(
        "probe",
        READ_WRITE,
        dir.to_str().expect("путь"),
        &[],
        &GenerateOptions::default(),
    )
    .expect_err("цель rust обязана отказать");
    assert!(
        error.message.contains("c-hal"),
        "отказ обязан называть цели с картой памяти: {}",
        error.message
    );
}

/// Цель `st` (библиотека блоков) отказывает; парная `st-at` - размещает.
#[test]
fn st_refuses_and_st_at_places_the_cell() {
    let dir = out_dir("st");
    let error = takt_lang::compile_to_st(
        "probe",
        READ_WRITE,
        dir.to_str().expect("путь"),
        &[],
        &GenerateOptions::default(),
    )
    .expect_err("цель st обязана отказать");
    assert_eq!(error.code.as_deref(), Some("ST-018"), "{error:?}");

    let dir = out_dir("st_at");
    let env = takt_lang::parse_defines(&[]).expect("среда");
    takt_lang::compile_to_st_at(
        "probe",
        READ_WRITE,
        dir.to_str().expect("путь"),
        &[],
        &[],
        &env,
        &GenerateOptions::default(),
    )
    .expect("цель st-at");
    let text = std::fs::read_to_string(dir.join("probe.st")).expect("порождённый ST");
    assert!(
        text.contains("AT_2000_0_8 AT %MB8192"),
        "ячейка обязана получить локацию памяти: {text}"
    );
    assert!(
        text.contains("VAR_EXTERNAL"),
        "блок видит ячейку через VAR_EXTERNAL: {text}"
    );
}

/// Цель `sv` отказывает (адресного пространства нет); `sv-mmio` заводит регистр.
#[test]
fn sv_refuses_and_sv_mmio_registers_the_cell() {
    let dir = out_dir("sv");
    let error = takt_lang::compile_to_sv(
        "probe",
        READ_WRITE,
        dir.to_str().expect("путь"),
        &[],
        &GenerateOptions::default(),
    )
    .expect_err("цель sv обязана отказать");
    assert_eq!(error.code.as_deref(), Some("SV-017"), "{error:?}");

    let dir = out_dir("sv_mmio");
    let env = takt_lang::parse_defines(&[]).expect("среда");
    takt_lang::compile_to_sv_mmio(
        "probe",
        READ_WRITE,
        dir.to_str().expect("путь"),
        &[],
        &[],
        &env,
        &GenerateOptions::default(),
    )
    .expect("цель sv-mmio");
    let text = std::fs::read_to_string(dir.join("probe.sv")).expect("порождённый SV");
    assert!(
        text.contains("AT_2000_0_8_next"),
        "ячейка обязана стать регистром с комбинационной парой: {text}"
    );
    assert!(
        text.contains("reg_rdata"),
        "ячейка обязана быть видна шине на чтение: {text}"
    );
}
