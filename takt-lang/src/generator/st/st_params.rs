//! Параметры POU цели `st`: что функция берёт у модели.
//!
//! Вынесено из `st_func` по границе ответственности (правило размера модуля,
//! `docs/CODE.md`): там - печать функций и вызовов, здесь - три списка, из которых
//! складывается заголовок POU:
//!
//! - [`params_of`] - параметры автора (`VAR_INPUT`);
//! - [`state_params`] - переменные и порты модели, передаваемые по ссылке
//!   (`VAR_IN_OUT`), - **транзитивно** по вызовам;
//! - [`const_params`] - константы, которые дублируются внутрь функции.
//!
//! Списки - единый источник истины для объявления И для аргументов вызова: разойдись
//! они, ST либо не соберётся, либо свяжет не те переменные.

use crate::semantic::type_node::TypeNode;
use crate::semantic::unused::{UsageSet, usage_from_stmt};
use crate::semantic::{FunctionDefinitionNode, ModelNode, StatementNode, VariableNode};

/// Параметры функции.
pub(crate) fn params_of(def: &FunctionDefinitionNode) -> Vec<(String, TypeNode)> {
    match def {
        FunctionDefinitionNode::Local { params, .. }
        | FunctionDefinitionNode::External { params, .. } => params.clone(),
        FunctionDefinitionNode::Builtin(_, params, _) => params
            .iter()
            .map(|(n, t)| (n.to_string(), t.clone()))
            .collect(),
        FunctionDefinitionNode::None | FunctionDefinitionNode::Unresolved(_) => Vec::new(),
    }
}

/// Переменные модели, которые функция читает или пишет в своём теле.
///
/// **Зачем.** В цели `c` функция получает первым параметром указатель на модель
/// (`static uint8_t Stacker_travel_time(const Stacker *model, ...)`) и читает через
/// него порты и переменные (`stacker.c:29-56`). В IEC 61131-3 `FUNCTION` -
/// **чистая**: она видит только свои `VAR_INPUT`/`VAR_IN_OUT` и к переменным
/// вызывающего `FUNCTION_BLOCK` доступа не имеет. Проверка `iec2c` поймал это на
/// `travel_time`, который читает порт корня `pos_stack`:
/// "Ambiguous enumerate value or Variable not declared in this scope".
///
/// Поэтому такие переменные передаются функции по ссылке - `VAR_IN_OUT`, форма
/// проверена пробой (✅). Список - **единый источник истины** для объявления и для
/// аргументов вызова: разойдись они, ST либо не соберётся, либо свяжет не те
/// переменные.
///
/// Константы сюда **не** попадают: они неизменны и объявляются `VAR CONSTANT` внутри
/// самой функции (форма тоже проверена пробой).
pub(crate) fn state_params(
    def: &FunctionDefinitionNode,
    model: &ModelNode,
) -> Vec<(String, TypeNode)> {
    let mut seen = std::collections::HashSet::new();
    state_params_seen(def, model, &mut seen)
}

/// Тот же список с множеством посещённых функций - защита от цикла обхода.
///
/// Признак **транзитивен**: функция, зовущая функцию, которая читает переменную модели,
/// обязана объявить эту переменную у себя - иначе она печатает `hot(ticks)`, не объявив
/// `ticks`, и `iec2c` отвечает "Variable not declared in this scope" при нулевом коде
/// возврата `taktc`. У целей `c` и `rust` тот же признак транзитивен с 0396.
///
/// Цикл вызовов запрещён семантикой (`SE-053`), и множество посещённых - защита в
/// глубину, а не рабочий случай.
fn state_params_seen(
    def: &FunctionDefinitionNode,
    model: &ModelNode,
    seen: &mut std::collections::HashSet<String>,
) -> Vec<(String, TypeNode)> {
    let FunctionDefinitionNode::Local {
        body, params, name, ..
    } = def
    else {
        return Vec::new();
    };
    if !seen.insert(name.clone()) {
        return Vec::new();
    }
    let mut set = UsageSet::default();
    usage_from_stmt(body, &mut set);

    // Нужды вызываемых функций - часть нужд этой: имена собираются тем же обходом
    // (`UsageSet::functions`), второго знания о вызовах не заводится.
    let mut inherited: Vec<(String, TypeNode)> = Vec::new();
    let mut called: Vec<&String> = set.functions.iter().collect();
    called.sort();
    for callee in called {
        let Some(found) = model.search_func(callee) else {
            continue;
        };
        let borrowed = found.borrow();
        for (name, ty) in state_params_seen(&borrowed, model, seen) {
            if !inherited.iter().any(|(n, _)| *n == name) {
                inherited.push((name, ty));
            }
        }
    }

    // Локальные объявления тела - не состояние модели.
    let mut locals: Vec<String> = params.iter().map(|(n, _)| n.clone()).collect();
    collect_locals(body, &mut locals);

    let mut out = Vec::new();
    let mut names: Vec<&String> = model.variables.keys().collect();
    names.sort();
    for name in names {
        if locals.contains(name) {
            continue;
        }
        if !set.variables.contains(name) && !set.ports.contains(name) {
            continue;
        }
        let (VariableNode::Simple { ty, .. } | VariableNode::Port { ty, .. }) =
            &model.variables[name]
        else {
            continue;
        };
        out.push((name.clone(), ty.clone()));
    }
    // Унаследованное добавляется к собственному, а порядок остаётся алфавитным: список -
    // единый источник истины для объявления и для аргументов вызова, и его порядок
    // обязан быть устойчивым.
    for (name, ty) in inherited {
        if !locals.contains(&name) && !out.iter().any(|(n, _)| *n == name) {
            out.push((name, ty));
        }
    }
    out.sort_by(|a, b| a.0.cmp(&b.0));
    out
}

/// Собирает имена переменных, объявленных внутри тела.
fn collect_locals(stmt: &StatementNode, out: &mut Vec<String>) {
    match stmt {
        StatementNode::Variable(name, _, _, _) => out.push(name.clone()),
        // Вставка: цель `st` печатает тело, только если оно адресовано ей; объявления
        // чужой вставки в вывод не попадают.
        StatementNode::Assembly { target, body, .. } => {
            if crate::semantic::target_block::emits_for(target.as_deref(), "st") {
                collect_locals(body, out);
            }
        }
        StatementNode::Block(items) => items.iter().for_each(|s| collect_locals(s, out)),
        StatementNode::If { then_, else_, .. } => {
            collect_locals(then_, out);
            if let Some(e) = else_ {
                collect_locals(e, out);
            }
        }
        StatementNode::Loop { body, .. } => collect_locals(body, out),
        StatementNode::For { init, body, .. } => {
            if let Some(i) = init {
                collect_locals(i, out);
            }
            collect_locals(body, out);
        }
        StatementNode::Match { arms, .. } => arms.iter().for_each(|a| collect_locals(&a.body, out)),
        StatementNode::None
        | StatementNode::Unresolved(_)
        | StatementNode::Expression(_, _)
        | StatementNode::Return(_, _)
        | StatementNode::Continue(_)
        | StatementNode::Break(_)
        | StatementNode::Formula(_)
        | StatementNode::InlineFormula(_) => {}
    }
}

/// Константы модели, которые функция использует.
///
/// Объявляются `VAR CONSTANT` внутри самой функции: `FUNCTION` чистая, а константа
/// неизменна - дублировать её дешевле, чем плести через параметры.
pub(crate) fn const_params(def: &FunctionDefinitionNode, model: &ModelNode) -> Vec<String> {
    let FunctionDefinitionNode::Local { body, .. } = def else {
        return Vec::new();
    };
    let mut set = UsageSet::default();
    usage_from_stmt(body, &mut set);
    // Множество использованных констант ключуется парой (владелец, имя) -, - поэтому
    // отбор идёт **от объявлений модели**: для каждой её константы строим тот же ключ и
    // спрашиваем множество. Обратный порядок (взять ключи множества и искать их среди
    // имён) сравнивал бы ключ с именем. Заодно правится и старая неточность: по голому
    // имени константа модели-тёзки, использованная в теле, засчитывалась этой модели.
    let mut names: Vec<String> = model
        .variables
        .iter()
        .filter_map(|(name, var)| match var {
            VariableNode::Const { upper, .. }
                if set
                    .constants
                    .contains(&crate::semantic::unused::const_key(upper.as_ref(), name)) =>
            {
                Some(name.clone())
            }
            _ => None,
        })
        .collect();
    names.sort();
    names
}
