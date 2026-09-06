//! Типы: размер массива, циклы псевдонимов.
//!
//! Часть модуля `validate`.

use super::*;

/// Запускает все семантические проверки для модели и всех вложенных моделей.
///
/// # Ошибки
///
/// Пробрасывает первую найденную [`Diagnostic`]-ошибку. Ce15: максимально допустимый
/// размер массива.
///
/// Массивы размером более 1024 элементов являются подозрительно большими для
/// встраиваемых систем. Тип `element_count` ограничен `u16` (<= 65535), но даже
/// статический C-массив из 65535 `float` занимает ~256 кб и может переполнить стек или
/// BSS-сегмент на микроконтроллере. Значение 1024 (2^10) - разумный предел для
/// Takt-целевых платформ.
pub(super) const MAX_ARRAY_SIZE: u16 = 1024;

/// Ce15: рекурсивно проверяет тип на допустимый размер массива.
///
/// Проверяет как внешний, так и вложенные (многомерные) массивы.
pub(super) fn check_type_array_size(ty: &TypeNode, loc: Location) -> Result<(), Diagnostic> {
    if let TypeNode::Array(size, elem) = ty {
        if *size > MAX_ARRAY_SIZE {
            return Err(Diagnostic::error(
                loc,
                format!(
                    "размер массива {} превышает максимально допустимый {} (2^10). \
                     Используйте динамическую память или разбейте массив на части.",
                    size, MAX_ARRAY_SIZE
                ),
            )
            .with_code("SE-038"));
        }
        // Рекурсивная проверка вложенных массивов (многомерные типы)
        check_type_array_size(elem, loc)?;
    }
    Ok(())
}

/// Ce15: проверяет все переменные модели на допустимый размер массивов.
pub(super) fn check_array_sizes(model: Rc<RefCell<ModelNode>>) -> Vec<Diagnostic> {
    let borrowed = model.borrow();
    // Накопление по объявлениям.
    let mut out = Vec::new();
    for var in borrowed.variables.values() {
        match var {
            VariableNode::Simple { ty, .. }
            | VariableNode::Const { ty, .. }
            | VariableNode::Port { ty, .. } => {
                out.extend(check_type_array_size(ty, var.loc()).err());
            }
            VariableNode::Unresolved => {}
        }
    }
    out
}

/// Возвращает имена псевдонимов типов, на которые прямо ссылается данный AST-тип.
///
/// Рекурсивно обходит `Array` и возвращает все `Alias`-имена, встреченные в типе.
/// Встроенные псевдонимы (`bit`, `bool`, `float`, `unit`) исключаются.
fn collect_type_deps(ty: &ast::Type) -> Vec<String> {
    match ty {
        ast::Type::Alias(id) => match id.name.as_str() {
            "bit" | "bool" | "float" | "unit" => vec![],
            name => vec![name.to_string()],
        },
        ast::Type::Array { element_type, .. } => collect_type_deps(element_type),
        _ => vec![],
    }
}

/// DFS-обход с обнаружением цикла в графе зависимостей псевдонимов.
///
/// `current` - узел, который сейчас посещается. `defs` - карта `имя -> AST-тип` (все
/// псевдонимы в модели). `visited` - уже полностью обработанные узлы (серые/чёрные).
/// `stack` - узлы на текущем пути DFS (для обнаружения цикла).
///
/// Возвращает `Some(имя)` - имя псевдонима, с которого начинается цикл, если цикл
/// найден.
fn dfs_type_cycle(
    current: &str,
    defs: &std::collections::BTreeMap<String, ast::Type>,
    visited: &mut HashSet<String>,
    stack: &mut HashSet<String>,
) -> Option<String> {
    if stack.contains(current) {
        // Узел уже на стеке - найден цикл
        return Some(current.to_string());
    }
    if visited.contains(current) {
        // Узел уже полностью обработан - цикла нет
        return None;
    }

    stack.insert(current.to_string());

    if let Some(ty) = defs.get(current) {
        let deps = collect_type_deps(ty);
        for dep in deps {
            if let Some(cycle_start) = dfs_type_cycle(&dep, defs, visited, stack) {
                return Some(cycle_start);
            }
        }
    }

    stack.remove(current);
    visited.insert(current.to_string());
    None
}

/// Ce16: Проверяет карту сырых АСД-типов на наличие циклических зависимостей.
///
/// Используется как в `validate_model` (через `check_recursive_type_aliases`), так и
/// напрямую в `tree.rs` до вызова `construct_type` - чтобы выдать понятную ошибку Ce16
/// вместо "тип не найден".
///
/// `type_locs` - карта `имя -> позиция` для формирования диагностики с правильным
/// Source.
pub fn check_type_alias_cycles_ast(
    raw_defs: &std::collections::BTreeMap<String, ast::Type>,
    type_locs: &std::collections::BTreeMap<String, crate::diagnostics::Location>,
) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    let mut visited: HashSet<String> = HashSet::new();
    let mut reported: HashSet<String> = HashSet::new();

    for name in raw_defs.keys() {
        if visited.contains(name) {
            continue;
        }
        let mut stack: HashSet<String> = HashSet::new();
        if let Some(cycle_start) = dfs_type_cycle(name, raw_defs, &mut visited, &mut stack)
            && !reported.contains(&cycle_start)
        {
            reported.insert(cycle_start.clone());
            let loc = type_locs
                .get(&cycle_start)
                .copied()
                .unwrap_or(Location::Implicit);
            diags.push(
                Diagnostic::error(
                    loc,
                    format!(
                        "псевдоним типа '{}' образует циклическую зависимость",
                        cycle_start
                    ),
                )
                .with_code("SE-039"),
            );
        }
    }

    diags
}

/// Ce16: Проверяет наличие циклических зависимостей среди псевдонимов типов.
///
/// Строит граф зависимостей псевдонимов из `model.raw_type_defs` и обнаруживает циклы с
/// помощью DFS. Для каждого найденного цикла возвращает [`Diagnostic`] с кодом Ce16.
///
/// ## Примеры
///
/// ```text
/// type A = [A; 8];           // Ce16: прямая рекурсия
/// type A = [B; 4];
/// type B = [A; 2];           // Ce16: взаимная рекурсия
/// type A = [bit; 8];
/// type B = [A; 2];           // OK: нет цикла
/// ```
pub fn check_recursive_type_aliases(model: Rc<RefCell<ModelNode>>) -> Vec<Diagnostic> {
    let raw_defs = model.borrow().raw_type_defs.clone();
    let type_locs = model.borrow().type_locs.clone();

    let mut diags = check_type_alias_cycles_ast(&raw_defs, &type_locs);

    // Рекурсивная проверка вложенных моделей
    let nested: Vec<Rc<RefCell<ModelNode>>> =
        model.borrow().models.values().map(Rc::clone).collect();
    for nested_model in nested {
        diags.extend(check_recursive_type_aliases(nested_model));
    }

    diags
}
