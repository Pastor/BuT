//! Сквозные тесты структурных типов в симуляторе.
//!
//! Проверяются **на значения** (`Unit::variable`), а не на факт перехода: имена
//! ожидаемых значений - из живой пробы, а не догадок (инвариант `CLAUDE.md`).

use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Unit, Value, build_unit};

fn unit_from(fixture: &str) -> Unit {
    let path = format!("tests/data/eval/{fixture}");
    let source = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{path}: {e}"));
    let (ast, _) = takt_lang::parse(&source, 0).unwrap_or_else(|e| panic!("{path}: разбор: {e:?}"));
    let model =
        construct_model(&ast, None, &[]).unwrap_or_else(|e| panic!("{path}: семантика: {e:?}"));
    build_unit(model).unwrap_or_else(|e| panic!("{path}: юнит: {e:?}"))
}

/// Прогоняет `steps` тактов (не прерываясь на терминальном - структурам нужен такт
/// `always`).
fn run(fixture: &str, steps: usize) -> (Unit, TickResult) {
    let mut unit = unit_from(fixture);
    let mut last = TickResult::Processing;
    for _ in 0..steps {
        last = unit.tick();
        if matches!(last, TickResult::Failed(_)) {
            break;
        }
    }
    (unit, last)
}

fn num(unit: &Unit, name: &str) -> i128 {
    match unit.variable(name) {
        Some(Value::Number(n)) => n,
        other => panic!("переменная '{name}': ожидалось целое, получено {other:?}"),
    }
}

/// Значение поля структурной переменной по имени.
fn field(unit: &Unit, var: &str, field: &str) -> Value {
    match unit.variable(var) {
        Some(Value::Struct { name, fields }) => fields
            .iter()
            .find(|(f, _)| f == field)
            .map(|(_, v)| v.clone())
            .unwrap_or_else(|| panic!("структура '{name}' не имеет поля '{field}'")),
        other => panic!("переменная '{var}': ожидалась структура, получено {other:?}"),
    }
}

// -- Наблюдаемость структуры ----------------------------------------------

/// Структурная переменная наблюдаема как `Value::Struct` целиком, поля в
/// объявленном порядке.
#[test]
fn struct_variable_is_observable() {
    let (unit, _) = run("struct_var.takt", 1);
    match unit.variable("p") {
        Some(Value::Struct { name, fields }) => {
            assert_eq!(name, "Point");
            // Порядок объявления: x, затем y.
            assert_eq!(fields[0].0, "x");
            assert_eq!(fields[1].0, "y");
        }
        other => panic!("p должна быть структурой, получено {other:?}"),
    }
}

// -- Чтение и запись поля ---------------------------------------

/// Запись в поле `p.x := 7`. Точечность: `p.y` не затёрт записью в `p.x`.
/// `p.y := 300` при `y: u8` усекается до `44` (S9 внутри поля).
#[test]
fn field_write_is_pointwise_and_truncates() {
    let (unit, _) = run("struct_var.takt", 1);
    assert_eq!(field(&unit, "p", "x"), Value::Number(7), "p.x := 7");
    assert_eq!(
        field(&unit, "p", "y"),
        Value::Number(44),
        "T22: p.y := 300 при u8 → 44"
    );
}

/// Чтение поля `rx := p.x`, `ry := p.y` (после записи и усечения).
#[test]
fn field_read_returns_value() {
    let (unit, _) = run("struct_var.takt", 1);
    assert_eq!(num(&unit, "rx"), 7, "rx = p.x = 7");
    assert_eq!(num(&unit, "ry"), 44, "ry = p.y = 44 (усечено)");
}

/// Запись во **вложенное** поле `o.i.v := 5` - путь рекурсивен.
#[test]
fn nested_field_write() {
    let (unit, _) = run("struct_nested.takt", 1);
    match field(&unit, "o", "i") {
        Value::Struct { fields, .. } => {
            assert_eq!(fields[0].0, "v");
            assert_eq!(fields[0].1, Value::Number(5), "o.i.v := 5");
        }
        other => panic!("o.i должна быть структурой, получено {other:?}"),
    }
    assert_eq!(num(&unit, "rv"), 5, "rv = o.i.v = 5");
}

// -- Контрпримеры: громкий отказ вместо тихой порчи ---------------------------

/// Текст ошибки первого падающего такта.
fn failure(fixture: &str) -> String {
    let (_, last) = run(fixture, 1);
    match last {
        TickResult::Failed(msg) => msg,
        other => panic!("{fixture}: ожидался отказ, получено {other:?}"),
    }
}

/// Чтение неизвестного поля `p.z`.
///
/// ** (SE-061):** ошибка теперь ловится на **компиляции**
/// (`construct_model` -> `validate`), а не в времени выполнения симулятора (`SIM-027`) -
/// компайл-тайм диагностика строго лучше: до исполнения дело не доходит. Сам
/// механизм `SIM-027` остаётся страховкой для базы, статически не разрешимой в
/// структуру, и покрыт юнит-тестами `eval/{access,place}.rs`.
#[test]
fn unknown_field_read_is_se061_at_compile_time() {
    let path = "tests/data/eval/struct_unknown_field.takt";
    let source = std::fs::read_to_string(path).expect("фикстура");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор");
    let err = construct_model(&ast, None, &[]).expect_err("ожидался отказ семантики SE-061");
    assert_eq!(err.code.as_deref(), Some("SE-061"), "{err:?}");
    assert!(
        format!("{err:?}").contains("не содержит поля"),
        "текст должен называть отсутствующее поле: {err:?}"
    );
}

/// Обращение к структуре по номеру бита `p.0` -> диагностика (SIM-029).
#[test]
fn bit_index_on_struct_is_diagnostic() {
    let msg = failure("struct_bit_index.takt");
    assert!(msg.contains("SIM-029"), "ожидался SIM-029: {msg}");
    assert!(msg.contains("по номеру бита"), "{msg}");
}

/// Сравнение структур `p = q` не определено (C запрещает `==` на структурах) ->
/// диагностика (SIM-005 TypeMismatch), а не тихое `false`.
#[test]
fn struct_comparison_is_diagnostic_not_false() {
    let msg = failure("struct_compare.takt");
    assert!(
        msg.contains("SIM-005"),
        "ожидался SIM-005 (TypeMismatch): {msg}"
    );
    assert!(msg.contains("структур"), "текст о структурах: {msg}");
}
