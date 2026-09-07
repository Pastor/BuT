//! Вычисленный инициализатор нормируется по типу объявления.
//!
//! # Что здесь проверяется
//!
//! Столкнулись два правила языка: свёртка инициализатора считала `~0` в
//! `i128` и давала `-1`, а проверка диапазона отвергала `-1` для
//! беззнакового типа - `var u: u8 := ~0;` становился ошибкой `SE-089`. При этом
//! **та же запись в теле** (`u := ~0;`) законна и даёт `255` у эталона, цели
//! `c` (`~0` -> `uint8_t`) и цели `rust` (`!0`). Одна запись, два ответа -
//! в зависимости от места.
//!
//! **вычисленное** значение нормируется по типу объявления по правилу (беззнаковое -
//! обёртка `mod 2ⁿ`, знаковое - ошибка), а **литерал автора** по-прежнему обязан
//! помещаться в тип.
//!
//! Контр-примеры здесь не формальность: без них правка неотличима от "разрешить всё".
//! `var u: u8 := 300;` обязан остаться ошибкой - автор написал число, которое не
//! помещается.

use takt_lang::generator::GenerateOptions;
use takt_sim::{TickResult, Value};

/// Модель с одним объявлением; `u` используется, чтобы дожить до структуры C.
fn model(decl: &str) -> String {
    format!("{decl}\nstart Run {{ always {{ u := u; }} ref Run; }}\n")
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

/// Строка инициализации `u` в порождённом C.
fn generated_c_init(tag: &str, src: &str) -> String {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0207_{tag}"));
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
        .find(|l| l.contains("->u = "))
        .expect("строка инициализации 'u'")
        .trim()
        .to_string()
}

/// Код диагностики семантики, если объявление отвергнуто.
fn rejection_code(src: &str) -> String {
    let (ast, _) = takt_lang::parse(src, 0).expect("разбор");
    match takt_lang::semantic::tree::construct_model(&ast, None, &[]) {
        Ok(_) => panic!("ожидался отказ, а дерево построилось"),
        Err(d) => d.code.unwrap_or_default(),
    }
}

/// Проверяет, что эталон и цель `c` дают одно значение.
fn check(tag: &str, decl: &str, expected: i128) {
    let src = model(decl);
    assert_eq!(
        reference_value(&src),
        Value::Number(expected),
        "эталон разошёлся с ожиданием на `{decl}`"
    );
    let line = generated_c_init(tag, &src);
    assert!(
        line.contains(&expected.to_string()),
        "цель `c` печатает не то значение: {line}"
    );
}

/// "Все единицы" для беззнакового: `~0` даёт границу типа.
#[test]
fn bitwise_not_zero_fills_unsigned() {
    check("u8", "var u: u8 := ~0;", 255);
}

/// Ширина берётся у типа объявления, а не у литерала.
#[test]
fn width_comes_from_declared_type() {
    check("u16", "var u: u16 := ~0;", 65535);
}

/// Выведенный тип - тот же случай: `[bit;8]` беззнаков.
#[test]
fn inferred_bit_vector_is_unsigned_too() {
    check("inferred", "var u := ~0;", 255);
}

/// `~1` - не частный случай нуля.
#[test]
fn bitwise_not_of_one() {
    check("not_one", "var u: u8 := ~1;", 254);
}

/// Знаковый тип не нормируется: `-1` в него помещается и остаётся собой.
#[test]
fn signed_type_keeps_negative_value() {
    check("i8", "var u: i8 := ~0;", -1);
}

/// Вычисленное переполнение беззнакового - обёртка, как и в теле.
///
/// Это **следствие** решения, а не побочный эффект: `u := 200 + 100;` в теле даёт `44`
/// у эталона и у цели `c`; объявление теперь отвечает так же.
#[test]
fn computed_overflow_wraps_for_unsigned() {
    check("wrap", "var u: u8 := 200 + 100;", 44);
}

/// **Контр-пример.** Литерал автора обязан помещаться в тип.
///
/// Без этой проверки правка была бы неотличима от "разрешить всё": диагностика границ
/// существует ровно затем, чтобы `var u: u8 := 300;` не уезжал в порождённый C, который
/// отвергнет `cc -Werror`.
#[test]
fn authored_literal_out_of_range_is_still_rejected() {
    assert_eq!(rejection_code(&model("var u: u8 := 300;")), "SE-089");
}

/// **Контр-пример.** Вычисленное за границей знакового типа - ошибка.
///
/// Правило: беззнаковое переполнение - обёртка, знаковое - ошибка программы.
/// Нормирование знакового типа было бы отступлением от него.
#[test]
fn computed_overflow_of_signed_is_an_error() {
    assert_eq!(rejection_code(&model("var u: i8 := 200 + 100;")), "SE-089");
}

/// Объявление и тело отвечают одинаково - предмет фичи одной проверкой.
///
/// Слева `~0` в инициализаторе, справа - та же запись в теле; значения обязаны
/// совпасть.
#[test]
fn declaration_and_body_agree() {
    let in_declaration = reference_value(&model("var u: u8 := ~0;"));
    let in_body = {
        let src = "var u: u8 := 0;\nstart Run { always { u := ~0; } ref Run; }\n";
        reference_value(src)
    };
    assert_eq!(
        in_declaration, in_body,
        "объявление и тело обязаны дать одно значение"
    );
}
