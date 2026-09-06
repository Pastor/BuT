//! Печать вызова функции и её аргументов.
//!
//! Часть модуля `c_expr`.

use super::*;

/// Генерирует список C-аргументов для вызова функции.
fn generate_args(
    map: &CMap,
    owner: &Element,
    params: &[(String, TypeNode)],
    args: &[ExpressionNode],
    has_model: bool,
) -> Result<Vec<String>, Diagnostic> {
    let mut result = Vec::new();
    for arg in args {
        let mut s = String::new();
        let mut tmp = Printer::new(4, &mut s);
        generate_stmt_expression(&mut tmp, map, owner, params.to_vec(), arg, has_model)?;
        result.push(s);
    }
    Ok(result)
}

/// Генерирует C-вызов функции.
///
/// - `Local` -> `{ModelCamelCase}_{name}(main, args...)`
/// - `External` -> `{name}(args...)`
/// - `Builtin("min"|"max"|"abs"|"clamp")` -> раскрывается как тернарное выражение
/// - `Builtin("debug"|"S")` -> возвращает ошибку (не транслируется в C)
pub(super) fn generate_function_call(
    printer: &mut Printer,
    map: &CMap,
    owner: &Element,
    params: Vec<(String, TypeNode)>,
    fun_def: &FunctionDefinitionNode,
    args: &[ExpressionNode],
    has_model: bool,
) -> Result<(), Diagnostic> {
    match fun_def {
        FunctionDefinitionNode::Local { upper, name, .. } => {
            let owner_rc =
                upper
                    .as_ref()
                    .and_then(|w| w.upgrade())
                    .ok_or_else(|| -> Diagnostic {
                        // Воронка `CC-023`; место пропущено при закрытии той фичи -
                        // найдено замером 0276.
                        crate::generator::c::c_unresolved::refuse(
                            crate::diagnostics::Location::Codegen,
                            crate::generator::c::c_unresolved::UnresolvedNode::Function(Some(
                                name.to_string(),
                            )),
                        )
                    })?;
            let model_name = Name::from(std::rc::Rc::clone(&owner_rc));
            let func_name = format!("{}_{}", model_name.unique_camelcase(), name);
            let arg_strs = generate_args(map, owner, &params, args, has_model)?;
            // В корневой модели (или вне контекста tick/init) первый аргумент -
            // `model`, в подмоделях - `main` (указатель на корневую модель)
            let first_arg = if !has_model || owner.name().eq(&map.root_name()) {
                "model"
            } else {
                "main"
            };
            // Указатель передаётся по нужде - тем же признаком, по которому он попал
            // (или не попал) в сигнатуру: разъехавшись, они дали бы невалидный C, и это
            // громкий отказ `cc`, а не молчание.
            let wants_state =
                crate::generator::c::c_needs::needs_state(fun_def, &owner_rc.borrow())?;
            let mut all_args = if wants_state {
                vec![first_arg.to_string()]
            } else {
                Vec::new()
            };
            all_args.extend(arg_strs);
            printer.print(&format!("{}({})", func_name, all_args.join(", ")));
        }
        FunctionDefinitionNode::External { name, .. } => {
            let arg_strs = generate_args(map, owner, &params, args, has_model)?;
            printer.print(&format!("{}({})", name, arg_strs.join(", ")));
        }
        FunctionDefinitionNode::Builtin(builtin_name, _, _) => match *builtin_name {
            "min" => {
                let arg_strs = generate_args(map, owner, &params, args, has_model)?;
                if arg_strs.len() >= 2 {
                    printer.print(&format!(
                        "((({a}) < ({b})) ? ({a}) : ({b}))",
                        a = arg_strs[0],
                        b = arg_strs[1]
                    ));
                }
            }
            "max" => {
                let arg_strs = generate_args(map, owner, &params, args, has_model)?;
                if arg_strs.len() >= 2 {
                    printer.print(&format!(
                        "((({a}) > ({b})) ? ({a}) : ({b}))",
                        a = arg_strs[0],
                        b = arg_strs[1]
                    ));
                }
            }
            "abs" => {
                let arg_strs = generate_args(map, owner, &params, args, has_model)?;
                if !arg_strs.is_empty() {
                    printer.print(&format!("((({x}) < 0) ? -({x}) : ({x}))", x = arg_strs[0]));
                }
            }
            "clamp" => {
                let arg_strs = generate_args(map, owner, &params, args, has_model)?;
                if arg_strs.len() >= 3 {
                    printer.print(&format!(
                        "((({x}) < ({lo})) ? ({lo}) : ((({x}) > ({hi})) ? ({hi}) : ({x})))",
                        x = arg_strs[0],
                        lo = arg_strs[1],
                        hi = arg_strs[2]
                    ));
                }
            }
            // Отказ здесь - часть штатного пути: печатник операторов (`c_expr::stmt`)
            // ловит его и пропускает вызов молча, потому что `debug`/`S` кода не
            // порождают (решение ). Код и текст всё равно обязаны быть настоящими: в
            // позиции выражения (`v := debug(x);`) отказ доходит до автора.
            "debug" | "S" => {
                return Err(crate::generator::c::c_unsupported::refuse(
                    crate::generator::c::c_unsupported::UnsupportedNode::Builtin(builtin_name),
                    // Координата - у оператора: своей у вызова нет.
                    crate::generator::site::at(crate::diagnostics::Location::Codegen),
                ));
            }
            _ => {
                return Err(crate::generator::c::c_unsupported::refuse(
                    crate::generator::c::c_unsupported::UnsupportedNode::UnknownBuiltin,
                    crate::generator::site::at(crate::diagnostics::Location::Codegen),
                ));
            }
        },
        _ => {
            return Err(crate::generator::c::c_unresolved::refuse(
                crate::diagnostics::Location::Codegen,
                crate::generator::c::c_unresolved::UnresolvedNode::Function(None),
            ));
        }
    }
    Ok(())
}
