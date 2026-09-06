//! Функции модели -> свободные функции модуля.
//!
//! Форвард-прототипов, в отличие от цели `c`, **не требуется**: в Rust порядок
//! объявления функций не значим. Расхождение с C осознанное - заставила C-генератор
//! эмитить прототипы именно потому, что там порядок значим.
//!
//! `extern fn` сюда не попадает: он становится методом трейта `Hal`
//! (`rust_decl::collect_ports`) - решение (а). Вариант `extern "C" { fn ... }`
//! отвергнут: он потребовал бы `unsafe` в порождаемом коде и уничтожил бы главную
//! дельту фичи к цели `c` (R10).

use crate::diagnostics::Diagnostic;
use crate::generator::rust::Printer;
use crate::generator::rust::rust_expr::Scope;
use crate::generator::rust::rust_map::RustMap;
use crate::generator::rust::rust_name::rust_value_name;
use crate::generator::rust::rust_needs::function_needs;
use crate::generator::rust::rust_stmt::{StmtOutput, print_block, print_statement, print_tail};
use crate::generator::rust::rust_type::rust_type;
use crate::semantic::minimap::Name;
use crate::semantic::type_node::TypeNode;
use crate::semantic::{FunctionDefinitionNode, ModelNode, StatementNode};
use std::collections::BTreeSet;

/// Печатает тело функции, заменяя **завершающий** `return x;` на хвостовое `x`.
///
/// В Takt возврат всегда явный, в Rust идиома - хвостовое выражение, и clippy
/// настаивает: `return x;` последним оператором это `needless_return`, то есть отказ
/// проверки под `-D warnings`.
///
/// Заменяется **только последний** оператор: ранние выходы (`if a > b { return a - b;
/// }` в `abs_diff`) остаются `return`'ами - они и в Rust идиоматичны, и clippy на них
/// не ругается.
fn print_body_with_tail(
    body: &StatementNode,
    scope: &mut Scope,
    p: &mut Printer,
    out: &mut StmtOutput,
) -> Result<(), Diagnostic> {
    let StatementNode::Block(items) = body else {
        return print_statement(body, scope, p, out);
    };
    // Тело функции печатается общим печатником блока - иначе оно не получило бы
    // переноса объявлений с мёртвым инициализатором (`rust_live`), и `travel_time` в
    // `stacker.takt` остался бы с `needless_late_init`. Хвостовой `return` (и
    // завершающий `if/else` со сворачиваемыми ветвями) заменяется выражением через
    // `print_tail`.
    print_block(items, Some(&print_tail), scope, p, out)
}

/// Печатает локальные функции всех моделей файла.
pub(crate) fn emit_functions(
    p: &mut Printer,
    map: &RustMap,
    blocks: &[(Name, std::rc::Rc<std::cell::RefCell<ModelNode>>)],
    warnings: &mut Vec<Diagnostic>,
) -> Result<(), Diagnostic> {
    let mut seen: BTreeSet<String> = BTreeSet::new();
    for (_, model_rc) in blocks {
        let model = model_rc.borrow();
        for def in model.functions.values() {
            let FunctionDefinitionNode::Local {
                name,
                params,
                ret,
                body,
                loc,
                ..
            } = def
            else {
                continue;
            };
            if !map.usage().functions.contains(name) {
                continue;
            }
            let ident = rust_value_name(name, *loc)?;
            if !seen.insert(ident.clone()) {
                continue;
            }

            // Что функции нужно сверх объявленных параметров: HAL (порт / `extern fn` /
            // `debug`) и читаемые переменные модели. Считается тем же предикатом, каким
            // `rust_expr::call` строит аргументы, - разойдись они, порождённый код не
            // собрался бы.
            let needs = function_needs(def, &model, &mut BTreeSet::new())?;

            let mut signature = Vec::new();
            for (pname, pty) in params {
                // Массив передаётся по ссылке: по значению это копия на каждый вызов,
                // тогда как цель `c` передаёт указатель, а `st` - `VAR_IN_OUT`. Признак -
                // общий с печатью аргумента: разъехавшись, они дают `E0308`.
                let printed = rust_type(pty, &format!("параметр '{}' функции '{}'", pname, name))?;
                let printed = if crate::generator::rust::rust_byref::is_array_by_reference(pty) {
                    format!("&{printed}")
                } else {
                    printed
                };
                signature.push(format!("{}: {}", rust_value_name(pname, *loc)?, printed));
            }
            // Переменные модели - после объявленных параметров и в порядке `BTreeMap`
            // (детерминизм).
            for (vname, vty) in &needs.vars {
                // Массив передаётся по ссылке: по значению это копия на каждый вызов,
                // тогда как цель `c` передаёт указатель, а `st` - `VAR_IN_OUT`. Признак -
                // общий с печатью аргумента.
                let printed_ty =
                    rust_type(vty, &format!("переменная '{}' в функции '{}'", vname, name))?;
                let printed_ty = if crate::generator::rust::rust_byref::is_array_by_reference(vty) {
                    format!("&{printed_ty}")
                } else {
                    printed_ty
                };
                signature.push(format!("{}: {}", rust_value_name(vname, *loc)?, printed_ty));
            }
            // HAL идёт последним параметром, а не первым. Причина - заимствования:
            // аргумент нередко сам читает порт (`travel_time(pos_stack, ...)` в
            // `stacker.takt`), и вызов `f(&mut hal, hal.read_u8(...))` взял бы `hal`
            // изменяемо дважды (E0499). Аргументы вычисляются слева направо, поэтому
            // чтение успевает отпустить заимствование до того, как его возьмёт `&mut`.
            if needs.hal {
                signature.push("hal: &mut H".to_string());
            }
            let generics = if needs.hal { "<H: Hal>" } else { "" };
            let ret_str = match ret {
                TypeNode::Unit | TypeNode::Inference => String::new(),
                other => format!(
                    " -> {}",
                    rust_type(other, &format!("возврат функции '{}'", name))?
                ),
            };

            // Внутри тела и объявленные параметры, и переменные модели - обычные
            // локальные имена: `x` печатается как `x`, а не `self.x`. `self` у
            // свободной функции нет, поэтому `has_self: false` - и это не ограничение,
            // а следствие: всё нужное уже в параметрах.
            let mut assigned = BTreeSet::new();
            crate::generator::rust::rust_assigned::collect_assigned(body, &mut assigned);
            let mut locals: Vec<String> = params.iter().map(|(pname, _)| pname.clone()).collect();
            locals.extend(needs.vars.keys().cloned());
            // Имена, пришедшие по ссылке: признак - тот же `rust_byref`, которым
            // выбрана форма параметра. Третьего списка не заводится: разъедься он с
            // сигнатурой, вывод не собрался бы.
            let by_ref: Vec<String> = params
                .iter()
                .filter(|(_, pty)| crate::generator::rust::rust_byref::is_array_by_reference(pty))
                .map(|(pname, _)| pname.clone())
                .chain(
                    needs
                        .vars
                        .iter()
                        .filter(|(_, vty)| {
                            crate::generator::rust::rust_byref::is_array_by_reference(vty)
                        })
                        .map(|(vname, _)| vname.clone()),
                )
                .collect();
            let mut scope = Scope {
                model: &model,
                shared: Vec::new(),
                shared_via_self: false,
                locals,
                by_ref,
                assigned,
                hal: if needs.hal {
                    "hal".to_string()
                } else {
                    String::new()
                },
                has_self: false,
                // Свободная функция получает `hal: &mut H` - уже ссылку.
                hal_is_ref: needs.hal,
                instances: Vec::new(),
                time_profile: map.time_profile(),
                // Тело функции - единственное место, где `return` имеет тип приёмника.
                return_type: Some(ret.clone()),
                // Подсказка о приёмнике степени ставится в `coerce_to`.
                power_target: None,
                guard_enable: map.guard_enable(),
            };

            p.ident(&format!(
                "fn {}{}({}){} {{",
                ident,
                generics,
                signature.join(", "),
                ret_str
            ))
            .nl();
            p.up();
            let mut out = StmtOutput::default();
            // Тело печатается в буфер: параметру, которым тело не пользуется, нужна
            // заглушка, а узнать это можно только по напечатанному тексту - тем же
            // признаком, что у цели `c`.
            let mut body_text = String::new();
            {
                // `fork` наследует уровень вложенности - отступы буфера совпадают с
                // прямой печатью (тот же приём, что у цели `c`).
                let mut buffer = p.fork(&mut body_text);
                print_body_with_tail(body, &mut scope, &mut buffer, &mut out)?;
            }
            for (param, _) in params {
                if crate::generator::rust::rust_unused::is_unused(&body_text, param) {
                    p.ident(&crate::generator::rust::rust_unused::guard(param))
                        .nl();
                }
            }
            p.print(&body_text);
            warnings.append(&mut out.warnings);
            p.down();
            p.ident("}").nl().nl();
        }
    }
    Ok(())
}
