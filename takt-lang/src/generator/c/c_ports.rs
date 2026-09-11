//! Проверка типов портов - цель `c`.
//!
//! Граница модуля - ответственность: `c_header` печатает объявления, а здесь отвечают
//! на вопрос "ложится ли порт на протокол HAL".

use crate::diagnostics::Diagnostic;
use crate::diagnostics::lang::keys;
use crate::generator::c::PortClass;
use crate::msg;
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
                msg!(
                    keys::CC_015_COMPOSITE_HAL_PORT,
                    name = name,
                    model = model_name.local(),
                    ty = ty
                ),
            )
            .with_code("CC-015"));
        }
    }
    Ok(())
}
