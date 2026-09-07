//! Обращение к элементу порта у цели `c`.

use super::*;

/// Печатает запись в элемент порта; `false` - приёмник не элемент порта.
///
/// # Ошибки
/// Отказ печати индекса либо значения; порт без владельца.
#[allow(clippy::too_many_arguments)]
pub(in crate::generator::c) fn emit_write(
    printer: &mut Printer,
    map: &CMap,
    owner: &Element,
    params: Vec<(String, TypeNode)>,
    expr: &ExpressionNode,
    target: &ExpressionNode,
    value: &ExpressionNode,
    has_model: bool,
) -> Result<bool, Diagnostic> {
    let ExpressionNode::ArraySubscript(base, index) = target else {
        return Ok(false);
    };
    let ExpressionNode::Variable(var_rc) = base.as_ref() else {
        return Ok(false);
    };
    let borrowed = var_rc.borrow();
    let VariableNode::Port {
        direction,
        name,
        ty,
        upper,
        ..
    } = &*borrowed
    else {
        return Ok(false);
    };
    let Some(model_rc) = upper.as_ref().and_then(|w| w.upgrade()) else {
        return Err(crate::generator::c::c_unresolved::refuse(
            expr.loc(),
            crate::generator::c::c_unresolved::UnresolvedNode::PortOwner("запись элемента"),
        ));
    };
    let variant = crate::generator::c::c_names::port_enum_variant(
        &Name::from(model_rc),
        name,
        *direction,
        crate::parser::ast::PortDirection::Out,
    );
    let mut index_str = String::new();
    {
        let mut tmp = Printer::new(4, &mut index_str);
        generate_expr(&mut tmp, map, owner, params.clone(), index, 0, has_model)?;
    }
    let mut value_str = String::new();
    {
        let mut tmp = Printer::new(4, &mut value_str);
        generate_expr(&mut tmp, map, owner, params, value, 0, has_model)?;
    }
    let ptr = if has_model && !owner.name().eq(&map.root_name()) {
        "main"
    } else {
        "model"
    };
    printer.print(&crate::generator::c::c_port_call::write(
        PortClass::from_type(ty),
        ptr,
        &variant,
        &index_str,
        &value_str,
    ));
    Ok(true)
}

/// Печатает запись разряда порта; `false` - база не порт.
///
/// У bit-порта номер разряда несёт само обращение; у числового разряд остаётся
/// частью значения: оно читается, правится и пишется обратно, а элемент при этом
/// нулевой.
///
/// # Ошибки
/// Порт без владельца; разряд вещественного порта (`CC-001`).
#[allow(clippy::too_many_arguments)]
pub(in crate::generator::c) fn emit_bit_write(
    printer: &mut Printer,
    map: &CMap,
    owner: &Element,
    params: Vec<(String, TypeNode)>,
    expr: &ExpressionNode,
    base: &ExpressionNode,
    bit: i128,
    value: &ExpressionNode,
    has_model: bool,
) -> Result<bool, Diagnostic> {
    let ExpressionNode::Variable(var_rc) = base else {
        return Ok(false);
    };
    let borrowed = var_rc.borrow();
    let VariableNode::Port {
        direction,
        name,
        ty,
        upper,
        ..
    } = &*borrowed
    else {
        return Ok(false);
    };
    let Some(model_rc) = upper.as_ref().and_then(|w| w.upgrade()) else {
        return Err(crate::generator::c::c_unresolved::refuse(
            expr.loc(),
            crate::generator::c::c_unresolved::UnresolvedNode::PortOwner("запись бита"),
        ));
    };
    let cls = PortClass::from_type(ty);
    if cls == PortClass::Rational {
        return Err(Diagnostic::error(
            crate::diagnostics::Location::Codegen,
            "BitAccess на float-порт не поддерживается при записи".to_string(),
        )
        .with_code("CC-001"));
    }
    let variant = crate::generator::c::c_names::port_enum_variant(
        &Name::from(model_rc),
        name,
        *direction,
        crate::parser::ast::PortDirection::Out,
    );
    let mut value_str = String::new();
    {
        let mut tmp = Printer::new(4, &mut value_str);
        generate_expr(&mut tmp, map, owner, params, value, 0, has_model)?;
    }
    let ptr = if has_model && !owner.name().eq(&map.root_name()) {
        "main"
    } else {
        "model"
    };
    match cls {
        PortClass::Bit => {
            printer.print(&crate::generator::c::c_port_call::write_bit(
                ptr,
                &variant,
                &bit.to_string(),
                &value_str,
            ));
        }
        PortClass::Numeric => {
            let read = crate::generator::c::c_port_call::read_numeric(
                ptr,
                &variant,
                crate::generator::c::c_port_call::SCALAR_INDEX,
            );
            let folded = format!("({read} & ~(1LL << {bit})) | (({value_str} & 1LL) << {bit})");
            printer.print(&crate::generator::c::c_port_call::write_numeric(
                ptr,
                &variant,
                crate::generator::c::c_port_call::SCALAR_INDEX,
                &folded,
            ));
        }
        PortClass::Rational => unreachable!("отсечён выше"),
    }
    Ok(true)
}

/// Обращение к элементу порта на чтение; `None` - база не порт.
///
/// Носитель один на выражения и условия: порт значением не является, и всякий, кто
/// напечатает индексацию его чтения, получит C, не собирающийся ни одним компилятором, -
/// при нулевом коде возврата `taktc`.
pub(in crate::generator::c) fn read(
    base: &ExpressionNode,
    index: &str,
    map: &CMap,
    owner: &Element,
    has_model: bool,
) -> Result<Option<String>, Diagnostic> {
    let ExpressionNode::Variable(var_rc) = base else {
        return Ok(None);
    };
    let var = var_rc.borrow();
    let VariableNode::Port {
        direction,
        name,
        ty,
        upper,
        ..
    } = &*var
    else {
        return Ok(None);
    };
    let Some(model_rc) = upper.as_ref().and_then(|w| w.upgrade()) else {
        return Err(crate::generator::c::c_unresolved::refuse(
            crate::diagnostics::Location::Codegen,
            crate::generator::c::c_unresolved::UnresolvedNode::PortOwner("чтение элемента"),
        ));
    };
    let variant = crate::generator::c::c_names::port_enum_variant(
        &Name::from(model_rc),
        name,
        *direction,
        crate::parser::ast::PortDirection::In,
    );
    let ptr = if has_model && !owner.name().eq(&map.root_name()) {
        "main"
    } else {
        "model"
    };
    Ok(Some(crate::generator::c::c_port_call::read(
        PortClass::from_type(ty),
        ptr,
        &variant,
        index,
    )))
}
