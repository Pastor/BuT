//! Проверка типов портов - цель `c`.
//!
//! Граница модуля - ответственность: `c_header` печатает объявления, а здесь отвечают
//! на вопрос "ложится ли порт на протокол HAL".

use crate::diagnostics::Diagnostic;
use crate::generator::c::PortClass;
use crate::semantic::minimap::Name;
use crate::semantic::{ModelNode, VariableNode};

/// Проверяет, что типы портов модели ложатся на протокол HAL.
///
/// Колбэки HAL принимают **скаляр** (`bool`, `int64_t`, `double`), поэтому структура и
/// массив (кроме упакованного `[bit;N]`) в них не проходят.
pub(in crate::generator::c) fn check_port_types(
    model: &ModelNode,
    model_name: &Name,
) -> Result<(), Diagnostic> {
    for var in model.variables.values() {
        let VariableNode::Port { name, ty, .. } = var else {
            continue;
        };
        if !PortClass::fits_hal(ty) {
            return Err(Diagnostic::error(
                crate::diagnostics::Location::Codegen,
                format!(
                    "порт '{}' модели '{}' имеет составной тип '{}': колбэки \
                     HAL принимают скаляр (bit/целое/вещественное), и \
                     структуру либо массив в них не передать. Разложите \
                     порт на скалярные либо работайте с переменной модели",
                    name,
                    model_name.local(),
                    ty
                ),
            )
            .with_code("CC-015"));
        }
    }
    Ok(())
}
