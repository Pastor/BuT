//! Применение аргументов инстанцирования целями -, (режим по умолчанию
//! `--parameters=assign`).
//!
//! Проверяется **текст** вывода, а не факт сборки (критерий A4 анализа): молча неверная
//! трансляция компилируется тоже. Форма режима `assign`:
//!
//! - модель **одна** - одна структура, одна `_init` на все экземпляры;
//! - значение аргумента присваивается полю экземпляра **после** вызова его
//!   инициализатора;
//! - ни одного `#define` на параметр (константности в этом режиме нет - цена,
//!   принятая осознанно, Option E).
//!
//! Поведение сверяет `takt-sim/tests/conformance_param_apply_tests.rs` (потактово,
//! симулятор ↔ цель `c`).

use takt_lang::GenerateOptions;
use takt_lang::{compile_to_c, compile_to_rust, compile_to_st, compile_to_sv};

/// Два экземпляра одной модели с разными настройками.
///
/// Параметр назван `gain`, а не `step`: `step` - имя стандартной библиотеки IEC
/// 61131-3, и цель `st` его отвергает (`ST-014`).
const SRC: &str = "model Tuner {\n\
                   \x20   parameter gain: u8 := 1;\n\
                   \x20   var acc: u8 := 0;\n\
                   \x20   start Count {\n\
                   \x20       always { acc := acc + gain; }\n\
                   \x20   }\n\
                   }\n\
                   \n\
                   start Main = Tuner(gain := 100) | Tuner(gain := 200);\n";

/// Каталог вывода для одного теста.
fn out_dir(tag: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt-0185-04-{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог вывода");
    dir
}

/// Число вхождений подстроки.
fn count(haystack: &str, needle: &str) -> usize {
    haystack.matches(needle).count()
}

// --- Цель `c`: форма режима `assign` (A4) ------------------------------------

/// Одна структура и одна `_init`; значения - присваиваниями после `_init` каждого
/// экземпляра; `#define` на параметр нет.
#[test]
fn c_emits_one_init_and_assignments_after_it() {
    let dir = out_dir("c");
    compile_to_c(
        "two_tuners.takt",
        SRC,
        dir.to_str().expect("путь"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("цель c принимает вход");
    let header = std::fs::read_to_string(dir.join("two_tuners.h")).expect("заголовок");
    let body = std::fs::read_to_string(dir.join("two_tuners.c")).expect("тело");

    // Модель одна: одно определение структуры и одной _init под-модели.
    assert_eq!(
        count(&header, "typedef struct TwoTunersTuner TwoTunersTuner;"),
        1,
        "структура под-модели обязана быть одна:\n{header}"
    );
    // `\nvoid` - определение с начала строки; иначе счёт ловил бы и форвард-прототип
    // (`static void ..._init(...);`), в котором та же подстрока.
    assert_eq!(
        count(&body, "\nvoid TwoTunersTuner_init("),
        1,
        "функция _init под-модели обязана быть одна:\n{body}"
    );
    // Значения экземпляров - присваиваниями после _init.
    for assignment in ["tuner0.gain = 100;", "tuner1.gain = 200;"] {
        assert_eq!(
            count(&body, assignment),
            1,
            "ожидалось присваивание '{assignment}':\n{body}"
        );
    }
    let init_pos = body.find("TwoTunersTuner_init(").expect("вызов _init");
    let assign_pos = body.find("tuner0.gain = 100;").expect("присваивание");
    assert!(
        init_pos < assign_pos,
        "настройка обязана идти ПОСЛЕ инициализатора экземпляра"
    );
    // Константности в режиме assign нет.
    assert_eq!(
        count(&header, "#define") + count(&body, "#define"),
        header.matches("#define").count() + body.matches("#define").count(),
    );
    assert!(
        !header.contains("STEP") || !header.contains("#define TWO_TUNERS_TUNER_STEP"),
        "параметр не должен эмитироваться макросом в режиме assign:\n{header}"
    );
}

// --- Цель `rust`: new() и init() согласованы ---------------------------------

/// Значения применяются и в конструкторе, и в `init()`: разойдясь, они дали бы одному
/// экземпляру разные значения в зависимости от того, как его создали.
#[test]
fn rust_applies_arguments_in_both_new_and_init() {
    let dir = out_dir("rust");
    compile_to_rust(
        "two_tuners.takt",
        SRC,
        dir.to_str().expect("путь"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("цель rust принимает вход");
    let code = std::fs::read_to_string(dir.join("two_tuners.rs")).expect("вывод rust");
    for assignment in ["gain = 100;", "gain = 200;"] {
        assert_eq!(
            count(&code, assignment),
            2,
            "значение обязано применяться дважды — в new() и в init():\n{code}"
        );
    }
}

// --- Цель `st`: инициализатор экземпляра FB ----------------------------------

/// Настройка - инициализатором экземпляра (`:= (gain := 100)`), а не присваиванием в
/// теле: тело исполняется каждый скан и перетирало бы значение, которое модель меняет
/// сама.
#[test]
fn st_emits_instance_initializers() {
    let dir = out_dir("st");
    compile_to_st(
        "two_tuners.takt",
        SRC,
        dir.to_str().expect("путь"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("цель st принимает вход");
    let code = std::fs::read_to_string(dir.join("two_tuners.st")).expect("вывод st");
    assert!(
        code.contains(":= (gain := 100)") && code.contains(":= (gain := 200)"),
        "оба экземпляра обязаны получить инициализатор:\n{code}"
    );
    // FUNCTION_BLOCK под-модели один - модель не копируется.
    assert_eq!(
        count(&code, "FUNCTION_BLOCK TwoTunersTuner"),
        1,
        "FB под-модели обязан быть один:\n{code}"
    );
}

// --- Цель `sv`: значение сброса и отказ на конфликте -------------------------

/// Одинаковые наборы аргументов законны: значение уходит в ветвь сброса.
#[test]
fn sv_puts_the_value_into_the_reset_branch() {
    let src = "model Tuner {\n\
               \x20   parameter gain: u8 := 1;\n\
               \x20   var acc: u8 := 0;\n\
               \x20   start Count {\n\
               \x20       always { acc := acc + gain; }\n\
               \x20   }\n\
               }\n\
               \n\
               start Main = Tuner(gain := 100);\n";
    let dir = out_dir("sv");
    compile_to_sv(
        "one_tuner.takt",
        src,
        dir.to_str().expect("путь"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("цель sv принимает вход");
    let code = std::fs::read_to_string(dir.join("one_tuner.sv")).expect("вывод sv");
    assert!(
        code.contains("gain <= 100;"),
        "значение аргумента обязано попасть в ветвь сброса:\n{code}"
    );
}

/// Разные наборы у одной модели в `sv` - громкий отказ `SV-016`: уплощение даёт один
/// набор регистров на модель, экземпляры делят их, и разные настройки невыразимы.
/// Молчаливая победа последнего значения - класс дефекта 0184.
#[test]
fn sv_rejects_conflicting_argument_sets() {
    let dir = out_dir("sv-conflict");
    let err = compile_to_sv(
        "two_tuners.takt",
        SRC,
        dir.to_str().expect("путь"),
        &[],
        &GenerateOptions::default(),
    )
    .expect_err("разные наборы обязаны отвергаться");
    assert_eq!(
        err.code.as_deref(),
        Some("SV-016"),
        "ожидался SV-016, получено: {:?} ({})",
        err.code,
        err.message
    );
}
