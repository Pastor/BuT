//! Сохранение и загрузка состояния симуляции в JSON-файл.
//!
//! Снимок описывает дерево [`Unit`]: текущее состояние каждого узла и значения всех
//! переменных/портов.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;

use crate::context::Context;
use crate::eval::value::Value;
use crate::json_input::json_to_value;
use crate::unit::{Unit, UnitKind};

// -- Структуры снимка ----------------------------------------------------------

/// Рекурсивный снимок дерева Unit.
#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "kind")]
pub enum UnitSnapshot {
    None,
    Node {
        current_state: Option<String>,
        variables: HashMap<String, serde_json::Value>,
    },
    Parallel {
        children: Vec<UnitSnapshot>,
    },
    Sequential {
        index: usize,
        children: Vec<UnitSnapshot>,
    },
}

// -- Конвертация Value <--> JSON -------------------------------------------------

fn value_to_json(v: &Value) -> serde_json::Value {
    match v {
        // `i128`: `From` для него у `serde_json::Number` нет - есть явный конструктор,
        // и он принимает весь диапазон носителя.
        Value::Number(n) => serde_json::Number::from_i128(*n)
            .map(serde_json::Value::Number)
            .unwrap_or(serde_json::Value::Null),
        Value::Real(f) => {
            serde_json::Value::Number(serde_json::Number::from_f64(*f).unwrap_or(0.into()))
        }
        Value::Boolean(b) => serde_json::Value::Bool(*b),
        // q(m, n): сохраняем **представление** целым; при загрузке `coerce_to_type`
        // трактует Number как готовый repr -> круг-трип точен.
        Value::Fixed { repr, .. } => serde_json::Value::Number((*repr).into()),
        // Длительность: сохраняем наносекунды целым - как и repr у q, круговой рейс
        // точен, потому что канон языка и есть наносекунды.
        Value::Duration(ns) => serde_json::Value::Number((*ns).into()),
        Value::Array(arr) => serde_json::Value::Array(arr.iter().map(value_to_json).collect()),
        // Структура: объект `{ поле: значение }`. Порядок полей при загрузке
        // восстанавливает `coerce_to_type` по определению структуры, поэтому имена
        // полей достаточно сохранить (значение - рекурсивно).
        Value::Struct { fields, .. } => serde_json::Value::Object(
            fields
                .iter()
                .map(|(k, v)| (k.clone(), value_to_json(v)))
                .collect(),
        ),
    }
}

// -- Снимок Unit ---------------------------------------------------------------

/// Создаёт снимок текущего состояния Unit-дерева.
pub fn snapshot(unit: &Unit) -> UnitSnapshot {
    match unit.kind() {
        UnitKind::None => UnitSnapshot::None,
        // Значения берутся из контекста модели через `Context::dump`: он и есть единый
        // источник истины о них.
        UnitKind::Node { state, .. } => UnitSnapshot::Node {
            current_state: state.clone(),
            variables: unit
                .dump()
                .iter()
                .map(|(k, v)| (k.clone(), value_to_json(v)))
                .collect(),
        },
        UnitKind::Parallel { units, .. } => UnitSnapshot::Parallel {
            children: units.iter().map(|u| snapshot(&u.borrow())).collect(),
        },
        UnitKind::Sequential { units, index, .. } => UnitSnapshot::Sequential {
            index: *index,
            children: units.iter().map(|u| snapshot(&u.borrow())).collect(),
        },
    }
}

/// Восстанавливает состояние Unit-дерева из снимка.
///
/// Несовпадения структуры (например, снимок Parallel для Node) игнорируются.
pub fn restore(unit: &mut Unit, snap: &UnitSnapshot) {
    restore_kind(unit, snap);
    // Общий реестр состояний - третья точка публикации после постройки узла и перехода:
    // возобновлённый прогон обязан отвечать на `S(Модель) = Состояние` тем состоянием,
    // которое пришло из снимка, а не стартовым. Публикуется после восстановления
    // поддерева: у композита публикация спускается к детям.
    publish_tree(unit);
}

/// Публикует состояния всего поддерева в реестр.
fn publish_tree(unit: &Unit) {
    unit.publish_state();
    match unit.kind() {
        UnitKind::Parallel { units, .. } | UnitKind::Sequential { units, .. } => {
            for child in units {
                publish_tree(&child.borrow());
            }
        }
        UnitKind::Node { .. } | UnitKind::None => {}
    }
}

fn restore_kind(unit: &mut Unit, snap: &UnitSnapshot) {
    match (unit.kind_mut(), snap) {
        (
            UnitKind::Node {
                state,
                context,
                entered_initial,
                ..
            },
            UnitSnapshot::Node {
                current_state,
                variables: vars,
            },
        ) => {
            *state = current_state.clone();
            // Возобновление: модель уже находится в этом состоянии, поэтому `enter`
            // повторять нельзя - иначе он затрёт загруженные значения.
            *entered_initial = true;
            // Восстановление идёт тем же путём, что присваивание в модели, - через
            // контекст.
            if let Some(ctx) = context {
                for (k, v) in vars {
                    if let Some(val) = json_to_value(v) {
                        ctx.borrow_mut().set_value(k, val);
                    }
                }
            }
        }
        (UnitKind::Parallel { units, .. }, UnitSnapshot::Parallel { children }) => {
            for (u, snap_child) in units.iter().zip(children.iter()) {
                restore_kind(&mut u.borrow_mut(), snap_child);
            }
        }
        (
            UnitKind::Sequential { units, index, .. },
            UnitSnapshot::Sequential {
                index: snap_idx,
                children,
            },
        ) => {
            *index = *snap_idx;
            for (u, snap_child) in units.iter().zip(children.iter()) {
                restore_kind(&mut u.borrow_mut(), snap_child);
            }
        }
        _ => {} // структура не совпадает - молча пропускаем
    }
}

// -- Файловые операции ---------------------------------------------------------

/// Сохраняет снимок Unit в JSON-файл.
pub fn save_to_file(unit: &Unit, path: &Path) -> Result<(), String> {
    let snap = snapshot(unit);
    let json = serde_json::to_string_pretty(&snap)
        .map_err(|e| format!("Ошибка сериализации состояния: {e}"))?;
    std::fs::write(path, json)
        .map_err(|e| format!("Не удалось записать файл состояния {}: {e}", path.display()))
}

/// Загружает снимок из JSON-файла и восстанавливает состояние Unit.
pub fn load_from_file(unit: &mut Unit, path: &Path) -> Result<(), String> {
    let json = std::fs::read_to_string(path).map_err(|e| {
        format!(
            "Не удалось прочитать файл состояния {}: {e}",
            path.display()
        )
    })?;
    let snap: UnitSnapshot = serde_json::from_str(&json)
        .map_err(|e| format!("Ошибка разбора файла состояния {}: {e}", path.display()))?;
    restore(unit, &snap);
    Ok(())
}

// -- Тесты ---------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::unit::Predicate;
    use std::collections::HashMap;

    // Узлы без контекста хранить значения не могут: единый источник истины - контекст
    // модели. Тесты значений и кругового рейса живут в интеграционном
    // `takt-sim/tests/sim/state_io_tests.rs`, где юниты строятся из фикстур `.takt`
    // через `build_unit`. Здесь остаётся структурная проверка снимка состояния и путей
    // ошибок.
    fn make_node(state: &str) -> Unit {
        let mut st = HashMap::new();
        st.insert(state.to_string(), vec![]);
        Unit::from_kind(UnitKind::Node {
            time_ns: 0,
            ticks_in_state: 0,
            state_entered_ns: 0,
            model_name: None,
            entered_initial: false,
            exited_terminal: false,
            path: Vec::new(),
            context: None,
            executions: HashMap::new(),
            state: Some(state.to_string()),
            state_transitions: st,
            state_executions: HashMap::new(),
            state_every: HashMap::new(),
            state_impls: HashMap::new(),
            every_consumed: Vec::new(),
            guards: Default::default(),
            invariant_violations: Vec::new(),
            last_transition: None,
        })
    }

    fn make_transitioning_node(from: &str, to: &str) -> Unit {
        let pred = Predicate::new("go", |_| Ok(true));
        let mut st = HashMap::new();
        st.insert(from.to_string(), vec![(to.to_string(), pred)]);
        st.insert(to.to_string(), vec![]);
        Unit::from_kind(UnitKind::Node {
            time_ns: 0,
            ticks_in_state: 0,
            state_entered_ns: 0,
            model_name: None,
            entered_initial: false,
            exited_terminal: false,
            path: Vec::new(),
            context: None,
            executions: HashMap::new(),
            state: Some(from.to_string()),
            state_transitions: st,
            state_executions: HashMap::new(),
            state_every: HashMap::new(),
            state_impls: HashMap::new(),
            every_consumed: Vec::new(),
            guards: Default::default(),
            invariant_violations: Vec::new(),
            last_transition: None,
        })
    }

    #[test]
    fn test_snapshot_none_is_none() {
        let snap = snapshot(&Unit::default());
        assert!(matches!(snap, UnitSnapshot::None));
    }

    #[test]
    fn test_snapshot_node_captures_state() {
        let unit = make_node("Active");
        let UnitSnapshot::Node { current_state, .. } = snapshot(&unit) else {
            panic!("ожидался UnitSnapshot::Node");
        };
        assert_eq!(current_state, Some("Active".to_string()));
    }

    #[test]
    fn test_restore_node_state() {
        let mut unit = make_transitioning_node("Idle", "Active");
        unit.tick();
        // После тика - в Active
        assert_eq!(unit.current_state(), Some("Active"));

        // Восстанавливаем обратно в Idle
        let snap = UnitSnapshot::Node {
            current_state: Some("Idle".to_string()),
            variables: HashMap::new(),
        };
        restore(&mut unit, &snap);
        assert_eq!(unit.current_state(), Some("Idle"));
    }

    #[test]
    fn test_save_invalid_path_returns_error() {
        let unit = make_node("S");
        let result = save_to_file(&unit, Path::new("/no/such/dir/state.json"));
        assert!(result.is_err());
    }

    #[test]
    fn test_load_missing_file_returns_error() {
        let mut unit = make_node("S");
        let result = load_from_file(&mut unit, Path::new("/no/such/file.json"));
        assert!(result.is_err());
    }
}
