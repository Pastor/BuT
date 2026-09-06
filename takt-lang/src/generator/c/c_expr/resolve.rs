//! Разрешение переменной модели в C-выражение - поиск по дереву композиции.
//!
//! Часть модуля `c_expr`.

use super::*;

/// Возвращает имя поля в родительской C-структуре для вложенной модели.
///
/// Ищет в родительской модели состояние с `implements = Extend::Model(эта_модель)` и
/// возвращает имя этого состояния в snake_case (именно оно используется как поле в
/// сгенерированной C-структуре). Если не найдено - возвращает `None`.
pub(super) fn find_in_extend(
    extend: &Extend,
    target: &Rc<RefCell<ModelNode>>,
    state_name: &str,
) -> Option<String> {
    match extend {
        Extend::Model(m, _, _) => {
            if Rc::ptr_eq(m, target) {
                Some(normalize_lowercase_snakecase(state_name.to_string()))
            } else {
                None
            }
        }
        Extend::Concatenation(items) => {
            for (idx, item) in items.iter().enumerate() {
                if let Some(path) = find_in_concat(item, target, state_name, idx) {
                    return Some(path);
                }
            }
            None
        }
        Extend::Parallel(items) => {
            for (idx, item) in items.iter().enumerate() {
                if let Some(path) = find_in_parallel(item, target, state_name, idx) {
                    return Some(path);
                }
            }
            None
        }
        Extend::Parentless(inner) => find_in_extend(inner, target, state_name),
        _ => None,
    }
}

fn find_in_concat(
    extend: &Extend,
    target: &Rc<RefCell<ModelNode>>,
    state_name: &str,
    idx: usize,
) -> Option<String> {
    match extend {
        Extend::Model(m, _, _) => {
            if Rc::ptr_eq(m, target) {
                let model_name = m.borrow().name.clone().unwrap_or_default();
                Some(format!(
                    "{}_{}{}",
                    normalize_lowercase_snakecase(state_name.to_string()),
                    normalize_lowercase_snakecase(model_name),
                    idx
                ))
            } else {
                None
            }
        }
        Extend::Parallel(items) => {
            let prefix = format!(
                "{}_parallel{}",
                normalize_lowercase_snakecase(state_name.to_string()),
                idx
            );
            for (inner_idx, item) in items.iter().enumerate() {
                if let Some(path) = find_in_parallel(item, target, &prefix, inner_idx) {
                    return Some(path);
                }
            }
            None
        }
        Extend::Concatenation(items) => {
            for (inner_idx, item) in items.iter().enumerate() {
                if let Some(path) = find_in_concat(item, target, state_name, inner_idx) {
                    return Some(path);
                }
            }
            None
        }
        Extend::Parentless(inner) => find_in_concat(inner, target, state_name, idx),
        _ => None,
    }
}

fn find_in_parallel(
    extend: &Extend,
    target: &Rc<RefCell<ModelNode>>,
    prefix: &str,
    idx: usize,
) -> Option<String> {
    match extend {
        Extend::Model(m, _, _) => {
            if Rc::ptr_eq(m, target) {
                let model_name = m.borrow().name.clone().unwrap_or_default();
                Some(format!(
                    "{}.{}{}",
                    normalize_lowercase_snakecase(prefix.to_string()),
                    normalize_lowercase_snakecase(model_name),
                    idx
                ))
            } else {
                None
            }
        }
        Extend::Parallel(items) => {
            let nested_prefix = format!(
                "{}.parallel{}",
                normalize_lowercase_snakecase(prefix.to_string()),
                idx
            );
            for (inner_idx, item) in items.iter().enumerate() {
                if let Some(path) = find_in_parallel(item, target, &nested_prefix, inner_idx) {
                    return Some(path);
                }
            }
            None
        }
        Extend::Concatenation(items) => {
            for (inner_idx, item) in items.iter().enumerate() {
                if let Some(path) = find_in_parallel(item, target, prefix, inner_idx) {
                    return Some(path);
                }
            }
            None
        }
        Extend::Parentless(inner) => find_in_parallel(inner, target, prefix, idx),
        _ => None,
    }
}

/// Преобразует [`VariableNode`] в C-выражение для чтения.
///
/// - `Simple` с `loc == Implicit` - локальная переменная (stack), доступ по имени.
/// - `Simple` - переменная модели, доступ `main->field` или `main->model.field`.
/// - `Const` - `CONST_{MODEL}_{NAME}`.
/// - `Port` - вызов `(*main->read_bit)(PORT_..., bit, main->userdata)`.
pub(in crate::generator::c) fn resolve_variable_c_expr(
    var: &VariableNode,
    params: &[(String, TypeNode)],
    map: &CMap,
    owner: &Element,
    has_model: bool,
) -> Result<String, Diagnostic> {
    match var {
        VariableNode::Simple {
            name, upper, loc, ..
        } => {
            // Локальная переменная (объявлена через register_local_var) имеет loc ==
            // Implicit
            if matches!(loc, Location::Implicit) {
                return Ok(normalize_lowercase_snakecase(name.clone()));
            }
            // Параметр функции - тоже доступ по имени
            if params.iter().any(|(p, _)| p == name) {
                return Ok(normalize_lowercase_snakecase(name.clone()));
            }
            // Переменная уровня модели -> main->field
            if let Some(model_rc) = upper.as_ref().and_then(|w| w.upgrade()) {
                // Извлекаем имя модели до вызова field_name_in_parent, чтобы избежать
                // двойного заимствования model_rc.
                let model_name_opt = model_rc.borrow().name.clone();
                if model_name_opt.is_some() {
                    // Вложенная модель: поле структуры называется по имени
                    // состояния-контейнера, а не по имени самой модели. Ищем это
                    // состояние в родителе.
                    let field = field_name_in_parent(&model_rc).unwrap_or_else(|| {
                        normalize_lowercase_snakecase(model_name_opt.unwrap_or_default())
                    });
                    Ok(format!(
                        "model->{}.{}",
                        field,
                        normalize_lowercase_snakecase(name.clone())
                    ))
                } else {
                    // Корневая модель - поле напрямую
                    Ok(format!(
                        "model->{}",
                        normalize_lowercase_snakecase(name.clone())
                    ))
                }
            } else {
                Ok(format!(
                    "model->{}",
                    normalize_lowercase_snakecase(name.clone())
                ))
            }
        }
        VariableNode::Const { name, upper, .. } => {
            // CONST_{MODEL_UPPERCASE}_{CONST_UPPERCASE}
            if let Some(model_rc) = upper.as_ref().and_then(|w| w.upgrade()) {
                let model_name = Name::from(model_rc);
                Ok(format!(
                    "CONST_{}_{}",
                    model_name.unique_uppercase_snakecase(),
                    normalize_lowercase_snakecase(name.clone()).to_uppercase()
                ))
            } else {
                Ok(format!(
                    "CONST_{}",
                    normalize_lowercase_snakecase(name.clone()).to_uppercase()
                ))
            }
        }
        VariableNode::Port {
            direction,
            name,
            ty,
            upper,
            ..
        } => {
            let model_name = if let Some(model_rc) = upper.as_ref().and_then(|w| w.upgrade()) {
                Name::from(model_rc)
            } else {
                return Err(crate::generator::c::c_unresolved::refuse(
                    var.loc(),
                    crate::generator::c::c_unresolved::UnresolvedNode::PortOwner("чтение"),
                ));
            };
            let cls = PortClass::from_type(ty);
            let variant = crate::generator::c::c_names::port_enum_variant(
                &model_name,
                name,
                *direction,
                crate::parser::ast::PortDirection::In,
            );
            // В локальных функциях (has_model=false) первый параметр - `const Root
            // *model`. В tick/init корневой модели - тоже `model`. В tick/init
            // подмодели - `main`.
            let ptr = if has_model && !owner.name().eq(&map.root_name()) {
                "main"
            } else {
                "model"
            };
            // Порт целиком - элемент нулевой: контракт один на все порты, и "нет
            // индекса" в нём не бывает.
            Ok(crate::generator::c::c_port_call::read(
                cls,
                ptr,
                &variant,
                crate::generator::c::c_port_call::SCALAR_INDEX,
            ))
        }
        VariableNode::Unresolved => Err(crate::generator::c::c_unresolved::refuse(
            crate::diagnostics::Location::Codegen,
            crate::generator::c::c_unresolved::UnresolvedNode::Variable,
        )),
    }
}

/// Разрешает путь доступа к [`VariableNode::Simple`] с учётом контекста генерации.
///
/// Сигнатуры C-функций:
///   - Tick/init (`has_model = true`): `void SubModel_tick(SubModel *model, Root *main)`
///   - Локальная функция (`has_model = false`): `static T Model_fn(const Root *model, ...)`
///
/// Правила доступа:
/// - Переменная той же модели, что `owner`:
///   - `has_model = true` -> `model->var`
///   - `has_model = false` -> `model->field.var` (через поле дочерней модели в Root)
/// - Переменная корневой модели, `owner` - вложенная:
///   - `has_model = true` -> `main->var`
///   - `has_model = false` -> `model->var` (первый параметр - сама Root)
/// - Иначе -> делегируем в [`resolve_variable_c_expr`]
pub(in crate::generator::c) fn resolve_simple_var_in_context(
    var_name: &str,
    upper: &Option<std::rc::Weak<std::cell::RefCell<ModelNode>>>,
    params: &[(String, TypeNode)],
    owner: &Element,
    map: &CMap,
    has_model: bool,
) -> Option<String> {
    // Параметры функции - доступ по имени, обрабатывается в resolve_variable_c_expr
    if params.iter().any(|(p, _)| p == var_name) {
        return None;
    }
    let var_model_rc = upper.as_ref().and_then(|w| w.upgrade())?;
    let var_model_name = Name::from(var_model_rc.clone());
    let is_same_model = var_model_name.eq(&owner.name());
    let is_root_var = var_model_rc.borrow().upper.is_none();
    let is_root_owner = owner.name().eq(&map.root_name());
    let snake = normalize_lowercase_snakecase(var_name.to_string());
    if is_same_model {
        if has_model {
            // Переменная принадлежит текущей генерируемой модели, `model` доступен
            Some(format!("model->{}", snake))
        } else if is_root_var {
            // Локальная функция корневой модели: `const Root *model` -> прямой доступ
            Some(format!("model->{}", snake))
        } else {
            // Локальная функция вложенной модели: первый параметр - `const Root
            // *model`, доступ через поле-контейнер дочерней модели.
            let field = field_name_in_parent(&var_model_rc)?;
            Some(format!("model->{}.{}", field, snake))
        }
    } else if is_root_var && !is_root_owner {
        // Переменная корневой модели, accessed из вложенной:
        // - tick/init: `main->var` (Root передаётся как `main`)
        // - локальная функция: `model->var` (первый параметр - сама Root)
        if has_model {
            Some(format!("main->{}", snake))
        } else {
            Some(format!("model->{}", snake))
        }
    } else {
        // Родительская модель обращается к переменной дочерней - стандартный путь
        None
    }
}
