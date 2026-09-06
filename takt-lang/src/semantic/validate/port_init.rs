//! Начальное значение порта: где оно законно.
//!
//! **`SE-092`** - начальное значение у **входного** порта. Значение входа
//! приходит извне: датчик, регистр, соседнее устройство. Задавать его нечем и
//! незачем, а запись `in P: u8 := 5;` почти наверняка означает, что автор принял
//! `:=` за адрес (до так и было).
//!
//! Здесь же жило временное предупреждение **`SE-093`** ("значение выставляют не все
//! цели"). научила последние четыре цели (`sv`, `sv-mmio`, `st`, `st-at`), и
//! предупреждение **снято**: молчаливого пропуска, о котором оно говорило, больше нет.
//! Номер не переиспользуется - он помечен `RETIRED` в реестре диагностик
//! (`docs/diagnostics/README.md`).

use crate::diagnostics::Diagnostic;
use crate::semantic::{ExpressionNode, ModelNode, PortDirection, VariableNode};
use std::cell::RefCell;
use std::rc::Rc;

/// `SE-092`: начальное значение у входного порта - **ошибка**.
pub(super) fn check_port_initializers(model: Rc<RefCell<ModelNode>>) -> Vec<Diagnostic> {
    let mut found = Vec::new();
    collect(&model, &mut found);
    found
}

fn collect(model: &Rc<RefCell<ModelNode>>, out: &mut Vec<Diagnostic>) {
    let (vars, nested) = {
        let b = model.borrow();
        (
            b.variables.values().cloned().collect::<Vec<_>>(),
            b.models.values().map(Rc::clone).collect::<Vec<_>>(),
        )
    };
    for var in &vars {
        let VariableNode::Port {
            init,
            direction,
            name,
            loc,
            ..
        } = var
        else {
            continue;
        };
        if matches!(init, ExpressionNode::None) {
            continue;
        }
        if *direction == PortDirection::In {
            out.push(
                Diagnostic::error(
                    *loc,
                    format!(
                        "входной порт '{name}' не может иметь начального значения: значение \
                         входа приходит извне. Если имелся в виду адрес, укажите его \
                         размещением — `at <адрес>`"
                    ),
                )
                .with_code("SE-092"),
            );
        }
    }
    for child in &nested {
        collect(child, out);
    }
}
