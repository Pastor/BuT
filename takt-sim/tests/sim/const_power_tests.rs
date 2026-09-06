//! Целая степень в инициализаторе константы: эталон == цель `c` -.
//!
//! # Что здесь тестытся
//!
//! Оператор `**` эталон исполняет **в теле** с, а в инициализаторе объявления не
//! работал ни у кого: константный вычислитель знака `**` не знал, и свёртка отдавала
//! узел потребителям как есть.
//!
//! | Потребитель | До фичи |
//! |---|---|
//! | эталон | `0` - **молча** |
//! | `c`, `c-hal` | `CC-023` |
//! | `st`, `st-at` | инициализатор потерян **молча** (`iec2c` принимает файл) |
//! | `rust` | вывод отвергает `rustc` (`E0689`) |
//! | `sv`, `sv-mmio` | `SV-002` |
//!
//! Значения `256` не давал **никто**.
//!
//! Сверяются **значения**, а не факт сборки: дефект цели `st` был именно молчаливым -
//! валидный файл с нулём вместо 256.
//!
//! Контр-пример обязателен: обёртка по типу приёмника должна остаться в силе - `2 ** 8`
//! в `u8` даёт `0`, как `200 + 100` даёт `44`. Без него правка читается как "степень
//! считается в объявленном типе".

use takt_lang::generator::GenerateOptions;
use takt_sim::{TickResult, Value};

/// Модель с константой-предметом; `u` читает её, иначе объявление не доживёт до
/// структуры C (штатный фильтр неиспользуемого).
fn model(decl: &str, ty: &str) -> String {
    format!("{decl}\nvar u: {ty} := 0;\nstart Run {{ always {{ u := K; }} ref Run; }}\n")
}

/// Значение `u` у эталона после первого такта.
fn reference_value(src: &str) -> Value {
    let (ast, _) = takt_lang::parse(src, 0).expect("разбор");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = takt_sim::build_unit(model).expect("построение юнита");
    let result = unit.tick();
    assert!(
        !matches!(result, TickResult::Failed(_)),
        "эталон не должен падать: {result:?}"
    );
    unit.variable("u").expect("значение 'u'")
}

/// Значение константы в порождённом C (строка `#define`).
fn generated_c_const(tag: &str, src: &str) -> String {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0407_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание каталога");
    takt_lang::compile_to_c(
        tag,
        src,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение C");
    let text = std::fs::read_to_string(dir.join(format!("{tag}.c"))).expect("чтение .c");
    let _ = std::fs::remove_dir_all(&dir);
    text.lines()
        .find(|l| l.contains("#define") && l.contains("_K"))
        .expect("строка определения константы 'K'")
        .trim()
        .to_string()
}

/// Эталон и цель `c` дают одно значение.
fn check(tag: &str, decl: &str, ty: &str, expected: i128) {
    let src = model(decl, ty);
    assert_eq!(
        reference_value(&src),
        Value::Number(expected),
        "эталон разошёлся с ожиданием на `{decl}`"
    );
    let line = generated_c_const(tag, &src);
    assert!(
        line.ends_with(&format!(" {expected}")),
        "цель c разошлась с эталоном на `{decl}`: {line}"
    );
}

/// Степень в инициализаторе даёт то же, что даёт та же запись в теле.
#[test]
fn power_in_const_initializer_is_computed() {
    check("pow_basic", "const K: u16 := 2 ** 8;", "u16", 256);
}

/// Основание может быть отрицательным - знак сохраняется.
#[test]
fn negative_base_keeps_its_sign() {
    check("pow_negative_base", "const K: i16 := -2 ** 3;", "i16", -8);
}

/// Нулевой показатель даёт единицу, а не ноль.
///
/// Контроль против "свёртка вернула умолчание": до фичи любое значение вырождалось в
/// `0`, и на `2 ** 8` это неотличимо от "показатель потерян".
#[test]
fn zero_exponent_yields_one() {
    check("pow_zero_exp", "const K: u8 := 7 ** 0;", "u8", 1);
}

/// Степень складывается с остальной арифметикой в одном выражении.
#[test]
fn power_composes_with_arithmetic() {
    check("pow_mixed", "const K: u16 := 2 ** 8 + 1;", "u16", 257);
}

/// Обёртка по типу приёмника остаётся в силе.
///
/// Контр-пример: своей нормировки фича не заводит - 256 в `u8` даёт `0`.
#[test]
fn folded_power_wraps_by_the_declared_type() {
    check("pow_wrap_u8", "const K: u8 := 2 ** 8;", "u8", 0);
}
