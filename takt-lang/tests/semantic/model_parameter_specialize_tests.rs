//! Режим `--parameters=specialize`,.
//!
//! `M(Y := 200)` порождает **копию** модели с подставленным значением; дальше по
//! конвейеру идёт обычная модель. Проверяется **текст** вывода (анализа:
//! число структур, а не их наличие) и границы механизма (SE-086/087).
//!
//! Сверка поведения режимов (`assign` == `specialize`); вывод константности в
//! специализациях.

use takt_lang::GenerateOptions;
use takt_lang::{compile_to_c, compile_to_sv};

/// Опции с включённой специализацией.
fn specialize() -> GenerateOptions {
    let mut options = GenerateOptions::default();
    options.specialize = true;
    options
}

/// Каталог вывода для одного теста.
fn out_dir(tag: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt-0185-05-{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог вывода");
    dir
}

/// Число вхождений подстроки.
fn count(haystack: &str, needle: &str) -> usize {
    haystack.matches(needle).count()
}

/// Модель с параметром; `gain`, не `step` (ST-014).
const TUNER: &str = "model Tuner {\n\
                     \x20   parameter gain: u8 := 1;\n\
                     \x20   var acc: u8 := 0;\n\
                     \x20   start Count {\n\
                     \x20       always { acc := acc + gain; }\n\
                     \x20       ref Count;\n\
                     \x20   }\n\
                     }\n";

// --- Форма вывода цели `c`  ----------------------------------------------

/// Разные значения -> разные структуры со своими `_init`; значение подставлено в
/// инициализатор копии; исходная модель мертва и в вывод не попадает.
#[test]
fn different_values_produce_different_models() {
    let src = format!("{TUNER}\nstart Main = Tuner(gain := 2) | Tuner(gain := 3);\n");
    let dir = out_dir("two");
    compile_to_c(
        "spec.takt",
        &src,
        dir.to_str().expect("путь"),
        &[],
        &specialize(),
    )
    .expect("цель c принимает вход");
    let header = std::fs::read_to_string(dir.join("spec.h")).expect("заголовок");
    let body = std::fs::read_to_string(dir.join("spec.c")).expect("тело");

    for model in ["SpecTunerP1", "SpecTunerP2"] {
        assert_eq!(
            count(&header, &format!("typedef struct {model} {model};")),
            1,
            "специализация {model} обязана быть структурой:\n{header}"
        );
    }
    // Значение места инстанцирования принадлежит копии, а не экземпляру: присваиваний
    // после `_init` нет. `gain` в фикстуре не присваивают, поэтому выводит его
    // константой - макросом на специализацию (а не полем `model->gain =
    // 2;`, то есть инициализатор поля).
    assert!(
        body.contains("#define CONST_SPEC_TUNER_P1_GAIN 2")
            && body.contains("#define CONST_SPEC_TUNER_P2_GAIN 3"),
        "значения обязаны быть константами специализаций:\n{body}"
    );
    assert!(
        !body.contains("gain = 2;") && !body.contains("gain = 3;"),
        "присваиваний полю в режиме specialize быть не должно:\n{body}"
    );
    // Исходная модель не инстанцируется никем - в карту достижимых не попадает.
    assert_eq!(
        count(&header, "typedef struct SpecTuner "),
        0,
        "мёртвая исходная модель не должна попадать в вывод:\n{header}"
    );
}

/// Одинаковые наборы дедуплицируются: в выводе одна структура на набор (проверяется
/// **число**,   - иначе тест прошёл бы и без дедупа).
#[test]
fn equal_value_sets_are_deduplicated() {
    let src =
        format!("{TUNER}\nstart Main = Tuner(gain := 5) | Tuner(gain := 5) | Tuner(gain := 7);\n");
    let dir = out_dir("dedup");
    compile_to_c(
        "dedup.takt",
        &src,
        dir.to_str().expect("путь"),
        &[],
        &specialize(),
    )
    .expect("цель c принимает вход");
    let header = std::fs::read_to_string(dir.join("dedup.h")).expect("заголовок");
    assert_eq!(
        count(&header, "typedef struct DedupTunerP"),
        2,
        "три вызова с двумя различными наборами обязаны дать ровно две \
         специализации:\n{header}"
    );
}

/// Вызов без аргументов сосуществует со специализацией: исходная модель остаётся для
/// него, копия - для настроенного экземпляра.
#[test]
fn plain_call_keeps_the_original_model() {
    let src = format!("{TUNER}\nstart Main = Tuner | Tuner(gain := 9);\n");
    let dir = out_dir("mixed");
    compile_to_c(
        "mixed.takt",
        &src,
        dir.to_str().expect("путь"),
        &[],
        &specialize(),
    )
    .expect("цель c принимает вход");
    let header = std::fs::read_to_string(dir.join("mixed.h")).expect("заголовок");
    assert_eq!(count(&header, "typedef struct MixedTuner MixedTuner;"), 1);
    assert_eq!(
        count(&header, "typedef struct MixedTunerP1 MixedTunerP1;"),
        1
    );
}

// --- Специализация снимает ограничение уплощения `sv` ------------------------

/// Разные наборы у одной модели: в `assign` - отказ `SV-016` (экземпляры делят
/// регистры), в `specialize` - законно: копии дают каждому набору свои регистры.
#[test]
fn sv_accepts_different_sets_via_specialization() {
    let src = format!("{TUNER}\nstart Main = Tuner(gain := 2) | Tuner(gain := 3);\n");
    let dir = out_dir("sv");
    compile_to_sv(
        "svspec.takt",
        &src,
        dir.to_str().expect("путь"),
        &[],
        &specialize(),
    )
    .expect("specialize обязан снимать ограничение уплощения");
    let code = std::fs::read_to_string(dir.join("svspec.sv")).expect("вывод sv");
    // `gain` не присваивают - выводит его константой, и каждая специализация получает
    // **свой** `localparam` (а не регистры со своими ветвями сброса:
    // `tuner_p1_gain <= 2;`). Имя квалифицировано владельцем - иначе уплощение слило бы
    // два объявления в одно и вторая специализация молча получила бы значение первой.
    assert!(
        code.contains("localparam logic [7:0] svspec_tuner_p1_gain = 2;")
            && code.contains("localparam logic [7:0] svspec_tuner_p2_gain = 3;"),
        "каждая специализация обязана получить свою константу:\n{code}"
    );
}

// --- Границы механизма -------------------------------------------------------

/// Специализация модели с собственными вложенными моделями - честный отказ `SE-087`:
/// `copy` разделил бы под-модели по `Rc`, и их уникальные имена разъехались бы с местом
/// в дереве (`CC-004`): владельца под-моделей менять нельзя.
#[test]
fn nested_models_are_rejected_with_se087() {
    let src = "model Outer {\n\
               \x20   parameter gain: u8 := 1;\n\
               \x20   model Inner { var x: u8 := 0; start S { ref S; } }\n\
               \x20   var acc: u8 := 0;\n\
               \x20   start Count {\n\
               \x20       always { acc := acc + gain; }\n\
               \x20       ref Count;\n\
               \x20   }\n\
               }\n\
               start Main = Outer(gain := 2);\n";
    let dir = out_dir("nested");
    let err = compile_to_c(
        "nested.takt",
        src,
        dir.to_str().expect("путь"),
        &[],
        &specialize(),
    )
    .expect_err("вложенные модели обязаны отвергаться");
    assert_eq!(
        err.code.as_deref(),
        Some("SE-087"),
        "ожидался SE-087, получено: {:?} ({})",
        err.code,
        err.message
    );
}

/// Имя специализации занято существующей моделью - `SE-086`, а не молчаливое
/// переименование: пространство имён кодогена проверок не имеет (О6 анализа).
#[test]
fn occupied_specialization_name_is_se086() {
    let src = "model Tuner {\n\
               \x20   parameter gain: u8 := 1;\n\
               \x20   var acc: u8 := 0;\n\
               \x20   start Count {\n\
               \x20       always { acc := acc + gain; }\n\
               \x20       ref Count;\n\
               \x20   }\n\
               }\n\
               model TunerP1 { var y: u8 := 0; start S { ref S; } }\n\
               start Main = Tuner(gain := 2) | TunerP1;\n";
    let dir = out_dir("collision");
    let err = compile_to_c(
        "coll.takt",
        src,
        dir.to_str().expect("путь"),
        &[],
        &specialize(),
    )
    .expect_err("коллизия имени обязана отвергаться");
    assert_eq!(
        err.code.as_deref(),
        Some("SE-086"),
        "ожидался SE-086, получено: {:?} ({})",
        err.code,
        err.message
    );
}

// --- Режим по умолчанию не задет ---------------------------------------------

/// Без включённой специализации тот же вход идёт режимом `assign` - одна структура,
/// присваивания после `_init` (форма ).
#[test]
fn default_mode_is_still_assign() {
    let src = format!("{TUNER}\nstart Main = Tuner(gain := 2) | Tuner(gain := 3);\n");
    let dir = out_dir("default");
    compile_to_c(
        "def.takt",
        &src,
        dir.to_str().expect("путь"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("режим assign принимает вход");
    let header = std::fs::read_to_string(dir.join("def.h")).expect("заголовок");
    let body = std::fs::read_to_string(dir.join("def.c")).expect("тело");
    assert_eq!(
        count(&header, "typedef struct DefTuner"),
        1,
        "в режиме assign модель одна:\n{header}"
    );
    assert!(
        body.contains("tuner0.gain = 2;") && body.contains("tuner1.gain = 3;"),
        "в режиме assign значения — присваиваниями после _init:\n{body}"
    );
}
