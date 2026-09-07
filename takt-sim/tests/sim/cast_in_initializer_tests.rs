//! Приведение `as` в инициализаторе объявления.
//!
//! # Что здесь проверяется
//!
//! До фичи второй вычислитель эталона (`unit/initial.rs`) относил `Cast` к "не
//! константе", и переменная получала **ноль по умолчанию**, тогда как цели
//! печатали настоящее значение. Замер: шесть форм из восьми расходились
//! **молча**, две падали `SIM-006` в такте.

use takt_lang::generator::GenerateOptions;
use takt_sim::{TickResult, Value};

/// Модель с одним объявлением: `v` объявлена и используется, чтобы дожить до структуры
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

/// Строка инициализации `v` в порождённом C - доказательство, что цель считает.
fn generated_c_init(tag: &str, src: &str) -> String {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0205_{tag}"));
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

/// Проверяет форму: эталон даёт `expected`, а цель `c` печатает `c_fragment`.
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
        "цель `c` печатает не то, что сверяем: {line}"
    );
}

/// Целое к более широкому целому.
#[test]
fn int_to_wider_int() {
    check("int_u16", "var v := 5 as u16;", Value::Number(5), "5");
}

/// Обёртка при сужении - правило, а не усечение наугад.
///
/// Вывод цели **изменился**: правило переехало в общий носитель, свёртка
/// инициализаторов вычисляет приведение при компиляции, и печатается готовое `44`
/// вместо `(uint8_t)300`. Значение то же - прежде его считал `cc`, теперь компилятор;
/// выигрыш в том, что теперь его считает и цель `sv`, которая приведение отвергала.
#[test]
fn int_narrowing_wraps_like_c() {
    check("int_u8", "var v := 300 as u8;", Value::Number(44), "= 44;");
}

/// Число к длительности - мост через миллисекунды.
///
/// До фичи эта форма падала `SIM-006` **в такте**: значение оставалось числом, а тип
/// объявления был `duration`.
#[test]
fn int_to_duration() {
    check(
        "int_dur",
        "var v := 250 as duration;",
        Value::Duration(250_000_000),
        "250",
    );
}

/// Длительность к числу - тот же мост в обратную сторону.
#[test]
fn duration_to_int() {
    check(
        "dur_int",
        "var v := 2s as u16;",
        Value::Number(2000),
        "2000",
    );
}

/// Булево к целому.
#[test]
fn bool_to_int() {
    check("bool_u8", "var v := true as u8;", Value::Number(1), "true");
}

/// Целое к `q(m, n)` - масштабирование на 2ⁿ.
///
/// Утверждение "эту форму свёртка не покрыла бы" **устарело** и снято: правило
/// представления переехало в общий носитель, компилятор считает его сам, и цель
/// печатает готовое `48` вместо сдвига. Значение то же - прежде его считал `cc`.
#[test]
fn int_to_fixed_scales() {
    check(
        "int_q",
        "var v := 3 as q(4, 4);",
        Value::Fixed {
            repr: 48,
            m: 4,
            n: 4,
            sat: false,
        },
        "= 48;",
    );
}

/// Дробное к `q(m, n)`.
///
/// Вывод цели **изменился**: печатается готовое представление `384` вместо `floor((1.5) *
/// 256.0)` - прежде цель звала `floor()` в времени выполнения ради константы.
#[test]
fn rational_to_fixed_scales() {
    check(
        "rat_q",
        "var v := 1.5 as q(8, 8);",
        Value::Fixed {
            repr: 384,
            m: 8,
            n: 8,
            sat: false,
        },
        "= 384;",
    );
}

/// Приведение **ссылки на константу** - форма из карточки фичи.
///
/// Значение берётся из **таблицы объявлений**, а не из ячейки ссылки: в ячейке лежит
/// снимок с ещё не понижённым АСД.
///
/// Вывод цели **изменился**: приведение к `duration` сворачивается при компиляции, и
/// печатается готовое число миллисекунд вместо ссылки на константу.
#[test]
fn cast_of_const_reference() {
    check(
        "const_dur",
        "const A := 250;\nvar v := A as duration;",
        Value::Duration(250_000_000),
        "= 250;",
    );
}

/// Взаимные константы не зацикливают построение юнита.
///
/// Значения у такой пары нет - есть предел глубины. Тест проверяет **завершение**: без
/// предела обход по инициализаторам ушёл бы в бесконечную рекурсию.
#[test]
fn mutual_const_reference_terminates() {
    let src = "const A := B;\nconst B := A;\nvar v: u8 := 0;\nstart Run { always { v := v; } ref Run; }\n";
    let (ast, _) = takt_lang::parse(src, 0).expect("разбор");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = takt_sim::build_unit(model).expect("построение юнита");
    let _ = unit.tick();
    assert_eq!(
        unit.variable("v"),
        Some(Value::Number(0)),
        "переменная без ссылки на цикл обязана сохранить своё значение"
    );
}

/// Ссылка на константу **без** приведения тоже даёт значение.
///
/// Ветвь `Variable` чинит не только `as`: до фичи любой инициализатор со ссылкой,
/// переживший свёртку, давал ноль.
#[test]
fn plain_const_reference_carries_value() {
    let src = model("const A := 7;\nvar v := A;");
    assert_eq!(reference_value(&src), Value::Number(7));
}
