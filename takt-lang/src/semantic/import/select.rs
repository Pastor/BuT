//! Выборочный импорт: `import { A, B as C } from "file.takt";`.
//!
//! Экспортирует перечисленные имена подключённого файла в текущий контекст.
//! Поддерживаемые категории и приоритет поиска: **модель -> тип -> переменная ->
//! условие**.
//!
//! Модуль выделен из `semantic/tree.rs`: ветка разрослась
//! усыновлением импортированного поддерева, а `tree.rs` - файл сверх лимита
//! размера, которому расти нельзя. Заодно приём стал видимым: перенос имён и их
//! **привязка** к импортёру - одна операция, и жить ей вместе.

use crate::diagnostics::Diagnostic;
use crate::parser::ast::Identifier;
use crate::semantic::import::adopt;
use crate::semantic::{ConditionDefinitionNode, ModelNode, VariableNode};
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::rc::Rc;

/// Карты строящейся модели-импортёра, в которые попадают импортированные имена.
pub(in crate::semantic) struct Target<'a> {
    pub models: &'a mut BTreeMap<String, Rc<RefCell<ModelNode>>>,
    pub variables: &'a mut BTreeMap<String, VariableNode>,
    pub conditions: &'a mut BTreeMap<String, ConditionDefinitionNode>,
    /// Узел импортёра: владелец усыновлённых объявлений и хозяин карты типов.
    pub model_node: &'a Rc<RefCell<ModelNode>>,
}

/// Переносит перечисленные символы из `imported` в карты импортёра и усыновляет
/// импортированное поддерево.
///
/// # Ошибки
///
/// - `SE-005`/`SE-006`/`SE-007`/`SE-008` - имя уже занято у импортёра;
/// - `SE-017` - запрошенного имени в подключённом файле нет;
/// - `SE-074` - импортированная модель опирается на объявления подключённого
///   файла, не перечисленные в этом импорте (см. [`adopt`]).
pub(in crate::semantic) fn apply(
    target: Target<'_>,
    imported: &Rc<RefCell<ModelNode>>,
    symbols: &[(Identifier, Option<Identifier>)],
    mark_imported: fn(Rc<RefCell<ModelNode>>) -> Rc<RefCell<ModelNode>>,
) -> Result<(), Diagnostic> {
    let Target {
        models,
        variables,
        conditions,
        model_node,
    } = target;
    // Усыновление перепривязывает поддеревья после цикла - пока `Rc` корня
    // подключённого файла жив: по нему опознаются его объявления.
    let mut renames: BTreeMap<String, String> = BTreeMap::new();
    let mut adopted: Vec<(Rc<RefCell<ModelNode>>, String)> = Vec::new();
    {
        let src = imported.borrow();
        for (orig_id, alias_id) in symbols {
            let orig = &orig_id.name;
            // Целевое имя: alias если задан, иначе оригинальное
            let alias = alias_id
                .as_ref()
                .map_or_else(|| orig.clone(), |a| a.name.clone());
            let sym_loc = alias_id.as_ref().map(|a| a.loc).unwrap_or(orig_id.loc);
            if let Some(m) = src.models.get(orig) {
                if models.contains_key(&alias) {
                    return Err(Diagnostic::declaration_error(
                        sym_loc,
                        format!("Модель с именем '{}' уже объявлена", alias),
                    )
                    .with_code("SE-006"));
                }
                adopted.push((Rc::clone(m), alias.clone()));
                models.insert(alias, mark_imported(Rc::clone(m)));
            } else if let Some(t) = src.types.get(orig) {
                if model_node.borrow().types.contains_key(&alias) {
                    return Err(Diagnostic::declaration_error(
                        sym_loc,
                        format!("Тип '{}' уже объявлен", alias),
                    )
                    .with_code("SE-007"));
                }
                model_node
                    .borrow_mut()
                    .types
                    .insert(alias.clone(), t.clone());
                // Структура и перечисление живут в двух картах: имя - в `types`,
                // устройство - в `structs`/`enums`. Перенеся одно имя, импорт отдавал
                // применению тип, о котором никто не знает, что у него внутри: цель `c`
                // печатала `Pid p;` без `typedef` - порождённый файл не компилировался, -
                // а симулятор строил агрегатный инициализатор массивом и отвечал
                // `SIM-012`. Отказа при этом не было ни одного: `taktc` рапортовал об
                // успехе.
                carry_definition(&src, orig, &alias, model_node);
            } else if let Some(v) = src.variables.get(orig) {
                if variables.contains_key(&alias) {
                    return Err(Diagnostic::declaration_error(
                        sym_loc,
                        format!("Переменная '{}' уже объявлена", alias),
                    )
                    .with_code("SE-005"));
                }
                let mut adopted_var = v.clone();
                adopt::adopt_declaration(&mut adopted_var, model_node, &alias);
                renames.insert(orig.clone(), alias.clone());
                variables.insert(alias, adopted_var);
            } else if let Some(f) = src.functions.get(orig) {
                if model_node.borrow().functions.contains_key(&alias) {
                    return Err(Diagnostic::declaration_error(
                        sym_loc,
                        format!("Функция с именем '{}' уже определена", alias),
                    )
                    .with_code("SE-009"));
                }
                let adopted = adopt_function(f, model_node, &alias);
                // Прямо в узел, и это работает лишь потому, что строитель в конце
                // обхода делает `functions.extend(...)`, а не присваивание целиком:
                // присваивание затёрло бы вклад, и вызов ответил бы `SE-004`
                // "неизвестная функция".
                model_node.borrow_mut().functions.insert(alias, adopted);
            } else if let Some(c) = src.conditions.get(orig) {
                if conditions.contains_key(&alias) {
                    return Err(Diagnostic::declaration_error(
                        sym_loc,
                        format!("Условие '{}' уже объявлено", alias),
                    )
                    .with_code("SE-008"));
                }
                conditions.insert(alias, c.clone());
            } else {
                return Err(Diagnostic::declaration_error(
                    orig_id.loc,
                    format!("Идентификатор '{}' не найден в импортируемом файле", orig),
                )
                .with_code("SE-017"));
            }
        }
    }
    for (m, alias) in &adopted {
        adopt::adopt_selected_model(m, imported, model_node, alias, &renames)?;
    }
    Ok(())
}

/// Усыновляет импортированную функцию: **перепривязывает владельца** и берёт целевое
/// имя.
///
/// Оставив ссылку на корень библиотеки, импорт дал бы вызов функции, которую этот файл
/// не объявляет, - тот же класс, что для структуры и для переменной.
///
/// Внешняя функция (`extern fn`) владельца не имеет вовсе: её имя - имя символа
/// компоновки, и переименовывать его псевдонимом импорта нельзя. Поэтому под alias
/// меняется **ключ карты**, а `name` внешней остаётся своим.
fn adopt_function(
    fun: &crate::semantic::FunctionDefinitionNode,
    model_node: &Rc<RefCell<ModelNode>>,
    alias: &str,
) -> crate::semantic::FunctionDefinitionNode {
    use crate::semantic::FunctionDefinitionNode as F;
    let mut adopted = fun.clone();
    if let F::Local { upper, name, .. } = &mut adopted {
        *upper = Some(Rc::downgrade(model_node));
        *name = alias.to_string();
    }
    adopted
}

/// Переносит устройство импортированного типа - поля структуры или варианты
/// перечисления.
///
/// Имя типа лежит в `types`, а его содержимое - в отдельной карте (`structs`, `enums`),
/// и перенос одного лишь имени давал применению тип-пустышку.
///
/// Под псевдонимом (`import { Pid as Loop }`) определение переименовывается
/// **вместе с ключом карты**: потребители - генераторы и симулятор - ищут
/// устройство по тому имени, которое видят в объявлении переменной.
fn carry_definition(
    src: &std::cell::Ref<'_, ModelNode>,
    orig: &str,
    alias: &str,
    model_node: &Rc<RefCell<ModelNode>>,
) {
    if let Some(s) = src.structs.get(orig) {
        let mut adopted = s.clone();
        adopted.name = alias.to_string();
        model_node
            .borrow_mut()
            .structs
            .insert(alias.to_string(), adopted);
    }
    if let Some(e) = src.enums.get(orig) {
        let mut adopted = e.clone();
        adopted.name = alias.to_string();
        model_node
            .borrow_mut()
            .enums
            .insert(alias.to_string(), adopted);
    }
}
