//! Имена C: функции, поля родителя, макросы условий, путь от корня.
//!
//! Часть модуля `c_expr`.

use super::*;

/// Возвращает C-имя функции (camelcase_name или внешнее имя).
pub(in crate::generator::c) fn get_function_name(fun: &FunctionDefinitionNode) -> String {
    match fun {
        FunctionDefinitionNode::Local { upper, name, .. } => {
            let model_name = Name::from(upper.clone().unwrap().upgrade().unwrap());
            format!("{}_{}", model_name.unique_camelcase(), name)
        }
        FunctionDefinitionNode::External { name, .. } => name.clone(),
        FunctionDefinitionNode::Builtin(name, ..) => name.to_string(),
        _ => {
            unreachable!("Unresolved function definition");
        }
    }
}

/// Возвращает имя поля в родительской C-структуре для вложенной модели.
///
/// Ищет в родительской модели состояние, в реализации которого используется эта модель,
/// и возвращает путь к полю в сгенерированной C-структуре.
pub(in crate::generator::c) fn field_name_in_parent(
    model_rc: &Rc<RefCell<ModelNode>>,
) -> Option<String> {
    let parent_rc = model_rc.borrow().upper.as_ref()?.upgrade()?;
    let parent = parent_rc.borrow();
    for (state_name, state_node) in &parent.states {
        if let StateNode::Implement { implements, .. } = state_node
            && let Some(path) = find_in_extend(implements, model_rc, state_name)
        {
            return Some(path);
        }
    }
    None
}

/// Путь к структуре модели **от корня**: `поле.поле...` (для корня - пустая строка).
///
/// Корневая структура владеет всеми под-моделями **по значению** (`Deep2bMid entry;` -
/// проба), а указатель на корень (`main`) доступен в любой порождённой функции. Поэтому
/// любая модель адресуема цепочкой полей от корня - в том числе та, до которой у
/// владельца условия нет прямого пути.
///
/// `None` - цепочку построить не удалось (модель не встроена ни в одно состояние
/// родителя): вызывающий обязан дать диагностику, а не догадку.
pub(super) fn path_from_root(model: &Rc<RefCell<ModelNode>>) -> Option<String> {
    let parent = match model.borrow().upper.as_ref() {
        None => return Some(String::new()), // сам корень
        Some(weak) => weak.upgrade()?,
    };
    let field = field_name_in_parent(model)?;
    let prefix = path_from_root(&parent)?;
    Some(if prefix.is_empty() {
        field
    } else {
        format!("{}.{}", prefix, field)
    })
}
