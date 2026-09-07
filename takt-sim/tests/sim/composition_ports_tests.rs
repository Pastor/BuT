//! Порты под-модели композиции доступны драйверу/дисплею.
//!
//! Дефект: `PortNames` собирались только с корневой модели, поэтому порты под-моделей
//! композиции (`Cabin | Motor`) не перечислялись - их нельзя было подать из sim-файла и
//! они не отображались. Читать симулятор их мог (`Unit::get_value` обходит все ветви),
//! а подать вход было нечем: модель "не реагировала на датчики" (`elevator_mini`).
//! Симптом `SIM-009` замаскирован (порт без значения -> 0), но порт оставался "немым".

use takt_lang::semantic::tree::construct_model;
use takt_sim::runner::PortNames;
use takt_sim::{Value, build_unit};

/// `Cabin | Motor`: Cabin несёт in-порт `Sensor` и out-порт `Alarm`, Motor - in-порт
/// `Limit`. Все три обязаны попасть в перечисление.
const SRC: &str = r#"
model Cabin {
    in Sensor: bit;
    out Alarm: bit;
    var seen: u8 := 0;
    start Watch {
        always { if Sensor { seen := 1; Alarm := true; } }
        ref Done: seen > 0;
    }
    state Done;
}
model Motor {
    in Limit: bit;
    start Run { ref Stop: Limit; }
    state Stop;
}
start Main = Cabin | Motor;
"#;

fn model() -> std::rc::Rc<std::cell::RefCell<takt_lang::semantic::ModelNode>> {
    let (ast, _) = takt_lang::parse(SRC, 0).expect("разбор");
    construct_model(&ast, None, &[]).expect("семантика")
}

/// Порты под-моделей композиции перечисляются (регрессия дефекта 0079).
#[test]
fn composition_submodel_ports_are_enumerated() {
    let names = PortNames::from_model(&model().borrow());
    assert!(
        names.in_ports.contains(&"Sensor".to_string()),
        "in-порт под-модели Cabin должен перечисляться: {:?}",
        names.in_ports
    );
    assert!(
        names.in_ports.contains(&"Limit".to_string()),
        "in-порт под-модели Motor должен перечисляться: {:?}",
        names.in_ports
    );
    assert!(
        names.out_ports.contains(&"Alarm".to_string()),
        "out-порт под-модели Cabin должен перечисляться: {:?}",
        names.out_ports
    );
}

/// Читаемость порта под-модели композиции (страховка): чтение `Sensor` (значение по
/// умолчанию `0` после 0086) не даёт `SIM-009` - порт в среде. Сквозная реакция на
/// поданный вход проверяется driven-сценарием
/// `examples/simulations/elevator_mini_floor2.json` (`floor_sensor_f2_bottom` ->
/// `current_floor := 2`), прогоняемым `scripts/run_simulations.sh`.
#[test]
fn submodel_port_is_readable_in_composition() {
    let unit = build_unit(model()).expect("построение юнита");
    // Порт `Sensor` под-модели читается через корневой юнит (обход всех ветвей).
    assert_eq!(
        unit.variable("Sensor"),
        Some(Value::Number(0)),
        "порт под-модели композиции должен читаться (0 по умолчанию, не SIM-009)"
    );
}

// -----------------------------------------------------------------------------
// Квалифицированные имена портов
//
// Дефект: пространство имён значений симулятора было плоским. Одноимённые порты разных
// под-моделей композиции делили адрес: чтение находило первую ветвь (порт второй модели
// был ненаблюдаем вовсе), запись расходилась по всем ветвям (задать вход отдельной
// под-модели было нечем). Карта адресов цели `c-hal` квалифицирована, а значения
// симулятора - нет: разные слои.

/// Две под-модели с одноимёнными портами: `Left::val = 1`, `Right::val = 2`.
fn duplicate_ports_model() -> std::rc::Rc<std::cell::RefCell<takt_lang::semantic::ModelNode>> {
    let src = "model Left  { in cmd: u8; out val: u8; start S { always { val := 1; } } }\
               model Right { in cmd: u8; out val: u8; start S { always { val := 2; } } }\
               start Root = Left | Right;";
    let (ast, _) = takt_lang::parse(src, 0).expect("разбор");
    construct_model(&ast, None, &[]).expect("семантика")
}

#[test]
fn qualified_name_reads_the_named_branch() {
    let mut unit = build_unit(duplicate_ports_model()).expect("построение юнита");
    unit.tick();

    // Голое имя по-прежнему находит первую ветвь - совместимость.
    assert_eq!(unit.variable("val"), Some(Value::Number(1)));

    // Квалифицированное - обе ветви различимы. До 0135 значение `Right::val` не
    // читалось никак: оно существовало в снимке, но было недоступно по имени.
    assert_eq!(unit.variable("Left::val"), Some(Value::Number(1)));
    assert_eq!(unit.variable("Right::val"), Some(Value::Number(2)));
}

#[test]
fn qualified_write_touches_only_the_named_branch() {
    let mut unit = build_unit(duplicate_ports_model()).expect("построение юнита");
    unit.set_port("Left::cmd", Value::Number(7));

    assert_eq!(unit.variable("Left::cmd"), Some(Value::Number(7)));
    assert_eq!(
        unit.variable("Right::cmd"),
        Some(Value::Number(0)),
        "запись по квалифицированному имени обязана попасть ровно в одну ветвь"
    );
}

#[test]
fn unknown_qualifier_addresses_nothing() {
    // Опечатка в имени модели не должна молча писать "куда-нибудь".
    let mut unit = build_unit(duplicate_ports_model()).expect("построение юнита");
    unit.set_port("Middle::cmd", Value::Number(9));
    assert_eq!(unit.variable("Left::cmd"), Some(Value::Number(0)));
    assert_eq!(unit.variable("Right::cmd"), Some(Value::Number(0)));
    assert_eq!(unit.variable("Middle::cmd"), None);
}

#[test]
fn duplicate_names_are_reported_as_ambiguous() {
    // Двусмысленность обязана быть видна перечислению: на ней стоят и предупреждение
    // при запуске, и квалифицированный вывод такта.
    let names = PortNames::from_model(&duplicate_ports_model().borrow());
    let ambiguous: Vec<&str> = names.ambiguous.iter().map(|(n, _)| n.as_str()).collect();
    assert_eq!(ambiguous, vec!["cmd", "val"]);

    let val = names
        .ambiguous
        .iter()
        .find(|(n, _)| n == "val")
        .expect("val двусмысленно");
    assert_eq!(val.1, vec!["Left::val", "Right::val"]);
}

#[test]
fn unique_names_are_not_reported_as_ambiguous() {
    // Тест направления: перечисление не должно объявлять двусмысленным то, что
    // объявлено один раз (иначе предупреждение станет шумом и его перестанут читать).
    let names = PortNames::from_model(&model().borrow());
    assert!(
        names.ambiguous.is_empty(),
        "у модели с уникальными именами двусмысленности нет: {:?}",
        names.ambiguous
    );
}
