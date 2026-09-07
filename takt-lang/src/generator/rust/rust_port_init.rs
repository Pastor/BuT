//! Начальные значения выходных портов в цели `rust`.
//!
//! ## Два места, а не одно
//!
//! Записи печатаются и в `new()`, и в `init()`: это разные входы. `new()` строит
//! модель, `init()` возвращает её в начальное состояние, и выставь значение только в
//! одном - сброс либо не дойдёт до порта, либо порт получит значение только после
//! `init()`.

use crate::diagnostics::Diagnostic;
use crate::generator::rust::rust_expr::{Scope, coerce_to};
use crate::generator::rust::rust_map::RustMap;
use crate::generator::rust::rust_name::rust_type_name;
use crate::generator::rust::rust_port::port_class;
use crate::semantic::minimap::Element;
use crate::semantic::{ExpressionNode, ModelNode, PortDirection, VariableNode};
use std::collections::BTreeSet;

/// Собирает записи начальных значений портов всего дерева - по одной строке вида
/// `write_f64(OutF64Port::Temperature, 0.0);` **без** приёмника.
///
/// Приёмник (`self.hal` в `new()`/`init()`) дописывает вызывающий: в `new()` модель ещё
/// строится и обращение идёт к `this`, а не к `self`.
///
/// Порядок обхода задан картой (`BTreeMap` моделей, `using_models`) - детерминизм
/// вывода держится типом контейнера, а не сортировкой здесь.
pub(crate) fn port_initial_writes(
    map: &RustMap,
    root: &ModelNode,
) -> Result<Vec<String>, Diagnostic> {
    let scope = Scope {
        model: root,
        shared: Vec::new(),
        shared_via_self: false,
        locals: Vec::new(),
        // По ссылке в методах модели ничего не приходит: массивы там - поля.
        by_ref: Vec::new(),
        assigned: BTreeSet::new(),
        hal: String::new(),
        has_self: false,
        hal_is_ref: false,
        instances: Vec::new(),
        time_profile: map.time_profile(),
        return_type: None,
        // Подсказка о приёмнике степени ставится в `coerce_to`.
        power_target: None,
        guard_enable: map.guard_enable(),
    };
    let mut seen: BTreeSet<String> = BTreeSet::new();
    let mut out = Vec::new();
    for element in map.using_models() {
        let Element::Model { name, .. } = element else {
            continue;
        };
        let model = map.raw_model_at(name)?;
        collect_from(&model.borrow(), &scope, &mut seen, &mut out)?;
    }
    collect_from(root, &scope, &mut seen, &mut out)?;
    Ok(out)
}

/// Добавляет записи по портам одной модели.
fn collect_from(
    model: &ModelNode,
    scope: &Scope,
    seen: &mut BTreeSet<String>,
    out: &mut Vec<String>,
) -> Result<(), Diagnostic> {
    for var in model.variables.values() {
        let VariableNode::Port {
            name,
            ty,
            init,
            direction,
            loc,
            ..
        } = var
        else {
            continue;
        };
        if matches!(init, ExpressionNode::None) || *direction == PortDirection::In {
            continue;
        }
        // Одно имя - один вариант перечисления (то же правило, что в `collect_ports`):
        // порт, объявленный в двух моделях под одним именем, даёт одну запись, а не две
        // одинаковых.
        if !seen.insert(name.clone()) {
            continue;
        }
        let class = port_class(ty, name, *loc, scope.model)?;
        // Начальное значение перечислимого порта уходит в HAL целым - тем же правилом,
        // что и запись в такте. Путей печати записи два, и правка одного оставляет
        // `rustc: mismatched types` во втором: класс поймала матрица целей.
        let value = match ty {
            crate::semantic::type_node::TypeNode::Enum(_) => {
                format!("{} as {}", coerce_to(init, ty, scope)?, class.value_type())
            }
            _ => coerce_to(init, ty, scope)?,
        };
        out.push(format!(
            "{}({}::{}, {});",
            class.write_fn(),
            class.out_enum(),
            rust_type_name(name, *loc)?,
            value
        ));
    }
    Ok(())
}
