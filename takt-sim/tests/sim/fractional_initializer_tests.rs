//! Дробная арифметика в инициализаторе объявления -.
//!
//! # Что здесь тестытся
//!
//! До фичи `var f: float := 1.0 + 2.0;` давал **три разных значения** на одном входе:
//! `0.0` у эталона (арифметика над дробными считалась "не константой"), `3.0` у целей
//! `c` и `rust`, молчаливую **потерю** инициализатора у `st`; на `q(4, 4)` цель `c`
//! печатала выражение в поле `int8_t`, то есть `3` - а в q(4, 4) это `0.1875`. Ни одно
//! значение не совпадало с другим, и ни одной диагностики не было.

use takt_lang::generator::GenerateOptions;
use takt_sim::{TickResult, Value};

/// Модель с одним объявлением: `v` используется, иначе не доживёт до структуры
/// порождённого C (неиспользуемая переменная в неё не попадает).
fn model(decl: &str) -> String {
    format!("{decl}\nstart Run {{ always {{ v := v; }} ref Run; }}\n")
}

/// Значение `v` у эталона после первого такта.
fn reference_value(src: &str) -> Value {
    let (ast, _) = takt_lang::parse(src, 0).expect("разбор");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = takt_sim::build_unit(model).expect("построение юнита");
    let result = unit.tick();
    assert!(
        !matches!(result, TickResult::Failed(_)),
        "эталон не должен падать: {result:?}"
    );
    unit.variable("v").expect("значение 'v'")
}

/// Строка инициализации `v` в порождённом C.
fn generated_c_init(tag: &str, src: &str) -> String {
    let thread = std::thread::current()
        .name()
        .unwrap_or("unnamed")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0300_{thread}_{tag}"));
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
    std::fs::read_to_string(dir.join(format!("{tag}.c")))
        .expect("чтение .c")
        .lines()
        .find(|l| l.contains("->v = "))
        .expect("строка инициализации 'v'")
        .trim()
        .to_string()
}

/// Диагностики общего входа CLI и языкового сервера.
fn codes(src: &str) -> Vec<String> {
    takt_lang::pipeline::collect_compile_diagnostics("model.takt", src, &[], false)
        .into_iter()
        .filter_map(|d| d.code)
        .collect()
}

/// Сверяет: эталон даёт `expected`, цель `c` печатает `c_fragment`.
fn check(tag: &str, decl: &str, expected: Value, c_fragment: &str) {
    let src = model(decl);
    assert_eq!(
        reference_value(&src),
        expected,
        "эталон разошёлся с ожиданием на форме `{decl}`"
    );
    let line = generated_c_init(tag, &src);
    assert!(
        line.contains(c_fragment),
        "цель `c` печатает не то, что сверяем на `{decl}`: {line}"
    );
}

/// **T1.** `float`: сумма дробных литералов - у эталона и цели одно значение.
#[test]
fn float_sum_agrees() {
    check(
        "f_sum",
        "var v: float := 1.0 + 2.0;",
        Value::Real(3.0),
        "3.0",
    );
}

/// **T2.** `q(m, n)`: та же сумма - и в целом q-представлении.
///
/// Ни одно из трёх чисел не было верным.
#[test]
fn fixed_sum_agrees_in_repr() {
    check(
        "q_sum",
        "var v: q(4, 4) := 1.0 + 2.0;",
        Value::Fixed {
            repr: 48,
            m: 4,
            n: 4,
            sat: false,
        },
        "48",
    );
}

/// **T3. Контроль: свёрнутая запись равна написанной.**
///
/// Без этой пары "стороны сошлись" означало бы лишь, что они ошибаются одинаково. Здесь
/// доказывается, что `1.0 + 2.0` и `3.0` - одно и то же на обеих сторонах.
#[test]
fn folded_equals_written_literal() {
    let folded = model("var v: q(4, 4) := 1.0 + 2.0;");
    let written = model("var v: q(4, 4) := 3.0;");
    assert_eq!(
        reference_value(&folded),
        reference_value(&written),
        "свёрнутая запись обязана дать то же, что написанный литерал"
    );
    assert_eq!(
        generated_c_init("q_folded", &folded),
        generated_c_init("q_written", &written),
        "цель `c` обязана напечатать то же самое"
    );
}

/// **T4.** Умножение и вычитание точны так же, как сложение.
#[test]
fn product_and_difference_agree() {
    check(
        "f_mul",
        "var v: float := 1.5 * 1.5;",
        Value::Real(2.25),
        "2.25",
    );
    check(
        "f_sub",
        "var v: float := 2.5 - 1.0;",
        Value::Real(1.5),
        "1.5",
    );
}

/// **T5.** Смешение с целым точно так же - отвергать за форму записи не за что.
#[test]
fn mixed_with_integer_agrees() {
    check(
        "f_mix",
        "var v: float := 1 + 3.14;",
        Value::Real(4.14),
        "4.14",
    );
}

/// **T6.** Деление отвергается `SE-114`: округление задано эталоном.
#[test]
fn division_is_rejected() {
    let got = codes(&model("var v: float := 1.0 / 3.0;"));
    assert!(
        got.contains(&"SE-114".to_string()),
        "деление обязано отвергаться, получено: {got:?}"
    );
}

/// **T7. Граница: приведение `as` остаётся законным.**
///
/// Первая редакция отказа была шире и отвергала эту форму - вход, на котором эталон и
/// цель **уже согласны** (работа ). Поймали это её теста; проверка стоит здесь, чтобы
/// граница была видна вместе с правилом.
#[test]
fn cast_initializer_stays_legal() {
    let got = codes(&model("var v := 3 as q(4, 4);"));
    assert!(
        got.is_empty(),
        "приведение в инициализаторе законно (фича 0205), получено: {got:?}"
    );
}

/// **T8. Граница: целых правило не касается.**
///
/// Их свёртка точна всегда, и сужать язык там повода нет.
#[test]
fn integer_arithmetic_is_untouched() {
    check("i_sum", "var v: u8 := 1 + 2;", Value::Number(3), "3");
}
