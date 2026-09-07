//! Вывод константности параметра -,.
//!
//! Проверяется **текст** вывода, а не факт сборки и **обе стороны** вывода (
//! анализа):
//!
//! - параметр без присваиваний под `--parameters=specialize` - **константа**;
//! - параметр с присваиванием - поле, включая присваивание из вложенного
//!   оператора, из функции модели и запись в бит;
//! - под `assign` (умолчание) - поле в **обоих** случаях: константности в этом
//!   режиме нет по решению.
//!
//! Отдельно - R12/A9: параметр там, где нужна величина, известная при генерации (`after
//! PARAM`, адрес порта). Под `specialize` законен, под `assign` - `SE-088`, и
//! проверяется **текст** сообщения: оно обязано называть режим, а не отказывать общими
//! словами.

use takt_lang::GenerateOptions;
use takt_lang::{compile_to_c, compile_to_c_hal, compile_to_rust, compile_to_sv};

/// Опции с включённой специализацией.
fn specialize() -> GenerateOptions {
    // `GenerateOptions` - `#[non_exhaustive]`: поле правится после `default()`.
    let mut options = GenerateOptions::default();
    options.specialize = true;
    options
}

/// Каталог вывода для одного теста.
fn out_dir(tag: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt-0185-06-{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог вывода");
    dir
}

/// Компилирует в C и возвращает `(заголовок, тело)`.
fn compile_c(tag: &str, name: &str, src: &str, options: &GenerateOptions) -> (String, String) {
    let dir = out_dir(tag);
    compile_to_c(
        &format!("{name}.takt"),
        src,
        dir.to_str().expect("путь"),
        &[],
        options,
    )
    .expect("цель c принимает вход");
    (
        std::fs::read_to_string(dir.join(format!("{name}.h"))).expect("заголовок"),
        std::fs::read_to_string(dir.join(format!("{name}.c"))).expect("тело"),
    )
}

/// Число вхождений подстроки.
fn count(haystack: &str, needle: &str) -> usize {
    haystack.matches(needle).count()
}

/// Параметр только читается - константа.
const READ_ONLY: &str = "model Tuner {\n\
                         \x20   parameter gain: u8 := 1;\n\
                         \x20   var acc: u8 := 0;\n\
                         \x20   start Count {\n\
                         \x20       always { acc := acc + gain; }\n\
                         \x20       ref Count;\n\
                         \x20   }\n\
                         }\n\
                         start Main = Tuner(gain := 2) | Tuner(gain := 3);\n";

// --- A7: константа выводится -------------------------------------------------

/// Параметр без присваиваний -> `#define` на специализацию, поля в структуре нет.
#[test]
fn read_only_parameter_becomes_constant() {
    let (header, body) = compile_c("const", "roc", READ_ONLY, &specialize());
    assert!(
        body.contains("#define CONST_ROC_TUNER_P1_GAIN 2")
            && body.contains("#define CONST_ROC_TUNER_P2_GAIN 3"),
        "параметр без присваиваний обязан стать константой специализации:\n{body}"
    );
    assert_eq!(
        count(&header, "uint8_t gain;"),
        0,
        "константа не занимает поле в структуре:\n{header}"
    );
}

// --- A7: контр-примеры (параметр остаётся переменной) ------------------------

/// Присваивание из **вложенного** оператора: параметр - поле, не константа.
///
/// Тот самый пропуск, которого требует контр-пример A7: разбор, не спускающийся в тело
/// `if`, объявил бы изменяемый параметр константой - и вывод стал бы молча неверным.
#[test]
fn assignment_in_nested_statement_keeps_field() {
    let src = "model Tuner {\n\
               \x20   parameter gain: u8 := 1;\n\
               \x20   var acc: u8 := 0;\n\
               \x20   start Count {\n\
               \x20       always { if acc < 10 { gain := gain + 1; } acc := acc + gain; }\n\
               \x20       ref Count;\n\
               \x20   }\n\
               }\n\
               start Main = Tuner(gain := 2) | Tuner(gain := 3);\n";
    let (header, body) = compile_c("nested", "nst", src, &specialize());
    assert_eq!(
        count(&header, "uint8_t gain;"),
        2,
        "изменяемый параметр обязан остаться полем каждой специализации:\n{header}"
    );
    assert!(
        !body.contains("CONST_NST_TUNER_P1_GAIN"),
        "изменяемый параметр константой быть не может:\n{body}"
    );
}

/// Присваивание из **функции модели** - то же: поле, не константа.
#[test]
fn assignment_in_model_function_keeps_field() {
    let src = "model Tuner {\n\
               \x20   parameter gain: u8 := 1;\n\
               \x20   var acc: u8 := 0;\n\
               \x20   fn bump() -> u8 { gain := gain + 1; return gain; }\n\
               \x20   start Count {\n\
               \x20       always { acc := acc + bump(); }\n\
               \x20       ref Count;\n\
               \x20   }\n\
               }\n\
               start Main = Tuner(gain := 2) | Tuner(gain := 3);\n";
    let (header, body) = compile_c("func", "fnc", src, &specialize());
    assert_eq!(
        count(&header, "uint8_t gain;"),
        2,
        "присваивание в теле функции модели обязано считаться:\n{header}"
    );
    assert!(
        !body.contains("CONST_FNC_TUNER_P1_GAIN"),
        "изменяемый параметр константой быть не может:\n{body}"
    );
}

/// Запись в **бит** параметра - изменение объявления, а не чтение.
#[test]
fn bit_assignment_keeps_field() {
    let src = "model Tuner {\n\
               \x20   parameter flags: u8 := 1;\n\
               \x20   var acc: u8 := 0;\n\
               \x20   start Count {\n\
               \x20       always { flags.0 := 1; acc := acc + flags; }\n\
               \x20       ref Count;\n\
               \x20   }\n\
               }\n\
               start Main = Tuner(flags := 2) | Tuner(flags := 3);\n";
    let (header, _) = compile_c("bit", "bta", src, &specialize());
    assert_eq!(
        count(&header, "uint8_t flags;"),
        2,
        "запись в бит параметра обязана считаться изменением:\n{header}"
    );
}

// --- A7: под `assign` константности нет -------------------------------------

/// Тот же вход в режиме по умолчанию: параметр - поле, ни одного `#define`.
#[test]
fn assign_mode_has_no_constants() {
    let (header, body) = compile_c("assign", "asg", READ_ONLY, &GenerateOptions::default());
    assert_eq!(
        count(&header, "uint8_t gain;"),
        1,
        "в режиме assign модель одна и параметр — её поле:\n{header}"
    );
    assert!(
        !body.contains("CONST_ASG_TUNER_GAIN"),
        "в режиме assign константности нет по решению заказчика:\n{body}"
    );
    assert!(
        body.contains("tuner0.gain = 2;") && body.contains("tuner1.gain = 3;"),
        "значения обязаны присваиваться полям экземпляров:\n{body}"
    );
}

// --- Имя константы квалифицировано владельцем (цели rust и sv) --------------

/// В целях `rust` и `sv` константы живут в **общем** пространстве имён модуля.
/// Константа-параметр обязана несли префикс владельца - иначе две специализации дают
/// одно объявление, и вторая молча получает значение первой.
#[test]
fn parameter_constants_are_qualified_in_rust_and_sv() {
    let dir = out_dir("qual");
    compile_to_rust(
        "qr.takt",
        READ_ONLY,
        dir.to_str().expect("путь"),
        &[],
        &specialize(),
    )
    .expect("цель rust принимает вход");
    let rust = std::fs::read_to_string(dir.join("qr.rs")).expect("вывод rust");
    assert!(
        rust.contains("const QR_TUNER_P1_GAIN: u8 = 2;")
            && rust.contains("const QR_TUNER_P2_GAIN: u8 = 3;"),
        "каждая специализация обязана получить свою константу:\n{rust}"
    );

    let dir = out_dir("qual-sv");
    compile_to_sv(
        "qs.takt",
        READ_ONLY,
        dir.to_str().expect("путь"),
        &[],
        &specialize(),
    )
    .expect("цель sv принимает вход");
    let sv = std::fs::read_to_string(dir.join("qs.sv")).expect("вывод sv");
    assert!(
        sv.contains("localparam logic [7:0] qs_tuner_p1_gain = 2;")
            && sv.contains("localparam logic [7:0] qs_tuner_p2_gain = 3;"),
        "каждая специализация обязана получить свой localparam:\n{sv}"
    );
}

/// Обычная константа подчиняется **тому же** правилу, что и константа-параметр: имя
/// квалифицировано владельцем.
///
/// Прежняя редакция теста утверждала обратное ("имени не меняет") - это было требование
/// R7 ("вывод корпуса байт-в-байт"), из-за которого квалификацию тогда получили
/// **только** константы-параметры. Исключение снято фичей [0193] по решению: два
/// правила на один вид объявления и были дефектом - две модели с одноимённой константой
/// давали одно объявление, и вторая молча считала чужим значением. Предмет теста
/// прежний, ожидание обновлено.
///
/// [0193]: ../../docs/features/0193-shared-const-qualified.md
#[test]
fn plain_constants_are_qualified_too() {
    let src = "model Tuner {\n\
               \x20   const gain: u8 := 2;\n\
               \x20   var acc: u8 := 0;\n\
               \x20   start Count {\n\
               \x20       always { acc := acc + gain; }\n\
               \x20       ref Count;\n\
               \x20   }\n\
               }\n\
               start Main = Tuner;\n";
    let dir = out_dir("plain");
    compile_to_rust(
        "pl.takt",
        src,
        dir.to_str().expect("путь"),
        &[],
        &specialize(),
    )
    .expect("цель rust принимает вход");
    let rust = std::fs::read_to_string(dir.join("pl.rs")).expect("вывод rust");
    assert!(
        rust.contains("const PL_TUNER_GAIN: u8 = 2;"),
        "обычная константа обязана быть квалифицирована владельцем (фича 0193):\n{rust}"
    );
}

// --- A9/R12: параметр в позиции compile-time величины -----------------------

/// Модель с выдержкой по параметру.
const DWELL: &str = "model Timer {\n\
                     \x20   parameter dwell: duration := 100ms;\n\
                     \x20   out led: bit;\n\
                     \x20   start Wait {\n\
                     \x20       enter { led := 1; }\n\
                     \x20       ref Done: after dwell;\n\
                     \x20   }\n\
                     \x20   state Done {\n\
                     \x20       enter { led := 0; }\n\
                     \x20       ref Wait;\n\
                     \x20   }\n\
                     }\n\
                     start Main = Timer(dwell := 200ms);\n";

/// Под `specialize` выдержка по параметру - **константная**: в порождённом C стоит
/// число, а не чтение поля.
#[test]
fn after_parameter_is_constant_under_specialize() {
    let (_, body) = compile_c("after-ok", "aok", DWELL, &specialize());
    assert!(
        body.contains(">= 200)"),
        "выдержка обязана стать константой значения аргумента:\n{body}"
    );
    assert!(
        !body.contains("model->dwell"),
        "константная выдержка поля не читает:\n{body}"
    );
}

/// Под `assign` - отказ `SE-088`, и текст **называет режим** (иначе выдержка молча
/// стала бы вычисляемой: другой код, 32-битный счётчик, отказ `ST-016` в цели `st`).
#[test]
fn after_parameter_is_rejected_under_assign() {
    let dir = out_dir("after-err");
    let err = compile_to_c(
        "aerr.takt",
        DWELL,
        dir.to_str().expect("путь"),
        &[],
        &GenerateOptions::default(),
    )
    .expect_err("параметр в позиции compile-time величины обязан отвергаться");
    assert_eq!(
        err.code.as_deref(),
        Some("SE-088"),
        "ожидался SE-088, получено: {:?} ({})",
        err.code,
        err.message
    );
    assert!(
        err.message.contains("--parameters=specialize")
            && err.message.contains("--parameters=assign"),
        "диагностика обязана называть оба режима: {}",
        err.message
    );
}

/// Адрес порта: та же граница. Под `specialize` адрес вычисляется, под `assign` -
/// `SE-088`.
#[test]
fn port_address_parameter_follows_the_same_rule() {
    const SRC: &str = "model Board {\n\
                       \x20   parameter base: u32 := 0x40000000;\n\
                       \x20   in btn: bit at base;\n\
                       \x20   out led: bit at 0x40000004;\n\
                       \x20   start Run {\n\
                       \x20       always { led := btn; }\n\
                       \x20       ref Run;\n\
                       \x20   }\n\
                       }\n\
                       start Main = Board(base := 0x50000000);\n";
    let dir = out_dir("addr-ok");
    compile_to_c_hal(
        "aok.takt",
        SRC,
        dir.to_str().expect("путь"),
        &[],
        &[],
        &Default::default(),
        &specialize(),
    )
    .expect("под specialize адрес обязан вычисляться");
    let header = std::fs::read_to_string(dir.join("aok.h")).expect("заголовок");
    assert!(
        header.contains("0x50000000u"),
        "адрес обязан взяться из аргумента специализации:\n{header}"
    );

    let dir = out_dir("addr-err");
    let err = compile_to_c_hal(
        "aerr.takt",
        SRC,
        dir.to_str().expect("путь"),
        &[],
        &[],
        &Default::default(),
        &GenerateOptions::default(),
    )
    .expect_err("под assign адрес по параметру обязан отвергаться");
    assert_eq!(
        err.code.as_deref(),
        Some("SE-088"),
        "ожидался SE-088, получено: {:?} ({})",
        err.code,
        err.message
    );
    assert!(
        err.message.contains("--parameters=specialize"),
        "диагностика обязана называть режим: {}",
        err.message
    );
}
