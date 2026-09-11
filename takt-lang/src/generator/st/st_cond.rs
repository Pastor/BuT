//! Печать условий цели `st`.
//!
//! Условие и выражение - разные узлы с разной семантикой `=` (инвариант проекта:
//! `Condition` и `Expression` не унифицировать), поэтому и печатники у них разные.
//! Здесь - только условия; выражения печатает `st_expr`.

use super::st_expr::{
    binary_cond, bit_access, bool_literal, unsupported, variable_name, wrap_cond,
};
use crate::diagnostics::lang::keys;
use crate::diagnostics::{Diagnostic, Location};
use crate::generator::st::st_operand_type::inner_cond_type;
use crate::msg;
use crate::semantic::{ConditionNode, ModelNode};

/// Печатает условие Takt в текст ST.
///
/// Отдельный печатник - инвариант: в условии `=` это **равенство**, а не присваивание.
///
/// # Ошибки
/// `ST-011` - узел не имеет представления в ST.
pub(crate) fn print_condition(
    cond: &ConditionNode,
    model: &ModelNode,
) -> Result<String, Diagnostic> {
    match cond {
        // Литерал длительности в условии - миллисекунды, как и значение. Выдержка
        // `after` здесь **не** обрабатывается: её печатает `st_model` (у него есть
        // доступ к полю времени и профилю), и попадание сюда означало бы, что условие
        // разбирают в обход того пути.
        ConditionNode::Duration(nanos) => Ok(crate::semantic::duration::value_millis(
            *nanos,
            Location::Codegen,
            &msg!(keys::GEN_WHAT_DURATION_IN_CONDITION),
        )?
        .to_string()),
        // Выдержка (константная и вычисляемая) печатается `st_model`: только у него
        // есть поле времени и профиль. Попадание сюда означает разбор в обход того
        // пути.
        ConditionNode::After(_) | ConditionNode::AfterTicks(_) | ConditionNode::AfterExpr(_) => {
            Err(
                Diagnostic::error(Location::Codegen, msg!(keys::ST_015_AFTER_AS_CONDITION))
                    .with_code("ST-015"),
            )
        }
        ConditionNode::Number(n) => Ok(n.to_string()),
        ConditionNode::Bool(b) => Ok(bool_literal(*b)),
        ConditionNode::Rational(text, negative) => {
            Ok(format!("{}{}", if *negative { "-" } else { "" }, text))
        }
        ConditionNode::Variable(var, _) => Ok(variable_name(&var.borrow())),
        ConditionNode::Parenthesis(inner) => Ok(format!("({})", print_condition(inner, model)?)),
        ConditionNode::Not(a) => Ok(format!("NOT {}", wrap_cond(a, model)?)),
        ConditionNode::And(a, b) => binary_cond(a, "AND", b, model),
        ConditionNode::Or(a, b) => binary_cond(a, "OR", b, model),
        ConditionNode::Add(a, b) => binary_cond(a, "+", b, model),
        ConditionNode::Subtract(a, b) => binary_cond(a, "-", b, model),
        // Ключевое отличие от цели `c`: там печатается `==`, здесь `=`.
        ConditionNode::Equal(a, b) => crate::generator::st::st_sign::compare_cond(a, "=", b, model),
        ConditionNode::NotEqual(a, b) => {
            crate::generator::st::st_sign::compare_cond(a, "<>", b, model)
        }
        ConditionNode::Less(a, b) => crate::generator::st::st_sign::compare_cond(a, "<", b, model),
        ConditionNode::More(a, b) => crate::generator::st::st_sign::compare_cond(a, ">", b, model),
        ConditionNode::LessEqual(a, b) => {
            crate::generator::st::st_sign::compare_cond(a, "<=", b, model)
        }
        ConditionNode::MoreEqual(a, b) => {
            crate::generator::st::st_sign::compare_cond(a, ">=", b, model)
        }
        ConditionNode::BitAccess(inner, member) => bit_access(
            &|| print_condition(inner, model),
            inner_cond_type(inner),
            member,
            model,
        ),
        // Цепочка индексаций схлопывается в одну - то же правило, что у печатника
        // выражений: печатников два, и правка одного чинит половину входов.
        ConditionNode::ArraySubscript(_, _) => {
            let (root, indices) = super::st_multidim::condition_subscript_chain(cond, model)?;
            Ok(format!("{}[{}]", root, indices.join(", ")))
        }
        // Вариант перечисления -> именованная константа, которую объявляет `st_decl`
        // (откат Option C: перечислимых типов MatIEC не знает).
        ConditionNode::EnumVariant(enum_node, variant, _) => {
            Ok(format!("{}_{}", enum_node.borrow().name, variant))
        }
        // Узлы без представления в ST - поимённо, без ветки `_`.
        ConditionNode::None => Err(unsupported(&msg!(keys::GEN_WHAT_EMPTY_CONDITION))),
        ConditionNode::Unresolved(_) => Err(unsupported(&msg!(keys::ST_WHAT_UNRESOLVED_CONDITION))),
        // Вызов функции в условии - тот же печатник, что и в выражении: аргументы
        // приходят условиями, поэтому печатаются печатником условий.
        ConditionNode::Function(def, args, _) => {
            let mut printed = Vec::new();
            for arg in args {
                printed.push(print_condition(arg, model)?);
            }
            super::st_func::print_call_texts(def, &printed, model)
        }
        ConditionNode::String(_) => Err(unsupported(&msg!(keys::ST_WHAT_STRING))),
        // Форма `S(Модель) = Состояние`. Причина отказа названа: прежний текст "модель
        // как условие" объяснял неверно - цель отвергает запись не потому, что не
        // понимает модель в условии, а потому, что экземпляры под-моделей суть поля
        // Родительского `FUNCTION_BLOCK`, и из соседнего блока доступа к ним нет. Дать
        // его значило бы завести параметр-указатель на корень, которого в цели нет (у
        // `c` это `main`).
        ConditionNode::Model(_, _) => Err(unsupported(&msg!(keys::ST_WHAT_STATE_OF_MODEL))),
        // Ветвь недостижима из корректной программы: форму `S(Модель) = Состояние`
        // перехватывает ветвь выше - с текстом, называющим обход. Отказ оставлен
        // страховкой; прежний текст обещал "", давно закрытую.
        ConditionNode::State(..) => Err(unsupported(&msg!(keys::ST_WHAT_STATE_IN_CONDITION))),
        // Анонимное обращение - см. оговорку у печатника выражений.
        ConditionNode::AnonPort(access) => Ok(access.synthetic_name()),
    }
}
