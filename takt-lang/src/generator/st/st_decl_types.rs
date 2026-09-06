//! Объявления ТИПОВ цели Structured Text: `TYPE … END_TYPE` (фича 0388).
//!
//! # Почему отдельный модуль
//!
//! Печать объявлений цели `st` состоит из двух пластов, и границы у них разные
//! по существу:
//!
//! - **типы файла** — структуры, именованные типы общих массивов и формы
//!   массивов из параметров функций. Они печатаются ОДИН раз на файл и **до**
//!   первого использования: в IEC 61131-3 порядок объявлений значим;
//! - **секции `VAR…`** внутри `FUNCTION_BLOCK` — они принадлежат конкретной
//!   модели (`st_decl`).
//!
//! Разделение — не косметика: `st_decl.rs` упёрся в предел размера модуля
//! (1000 строк при лимите 1000, замер фичи 0385), и следующая правка печати
//! объявлений уронила бы гейт. Границей выбрана та, что уже описана в шапке
//! `st_decl`: «два пласта вывода».
//!
//! ⚠️ Вывод не меняется ни на байт — фича переносит код, а не правит его;
//! сторож здесь — снапшоты корпуса `examples/generated/st` (гейт 0048/0274).

use crate::diagnostics::Diagnostic;
use crate::generator::indent::Printer;
use crate::generator::st::st_reserved::check_st_field_name;
use crate::generator::st::st_type::{self, get_st_type};
use crate::semantic::FunctionDefinitionNode;
use crate::semantic::minimap::Name;
use crate::semantic::type_node::TypeNode;
use crate::semantic::{ModelNode, VariableNode};
use std::cell::RefCell;
use std::rc::Rc;

/// Имена массивов корня, которые **действительно** передаются под-моделям
/// через `VAR_IN_OUT` (фича 0210).
///
/// ⚠️ Считается по объединению `shared` всех под-моделей, а не «все массивы
/// корня»: иначе локальный массив модели без под-моделей тоже получил бы
/// именованный тип — лишняя сущность и сдвиг вывода там, где ничего не ломалось
/// (проба показала это падением теста `st_tests`).
pub(crate) fn shared_array_names(
    map: &crate::generator::st::st_map::StMap,
    models: &[(Name, Rc<RefCell<ModelNode>>)],
    root_name: &Name,
) -> Vec<String> {
    let Some((_, root_rc)) = models.last() else {
        return Vec::new();
    };
    let root = &*root_rc.borrow();
    let mut out: Vec<String> = Vec::new();
    for (name, _) in models {
        if name.unique() == root_name.unique() {
            continue;
        }
        for (var, ty) in map.shared_variables(name) {
            if st_type::needs_named_array_type(&ty, root) && !out.contains(&var) {
                out.push(var);
            }
        }
    }
    out.sort();
    out
}

/// Печатает объявления структур файла как `TYPE … END_TYPE`.
///
/// Структуры собираются со **всех** моделей снимка и дедуплицируются по имени:
/// одна структура, видимая из нескольких моделей, объявляется однажды (R5.4).
/// Порядок — лексикографический: `structs` — `HashMap`, её обход
/// недетерминирован (та же первопричина, что у порядка `FUNCTION_BLOCK` в
/// `mod.rs`).
///
/// # Ошибки
/// Диагностика от [`get_st_type`], если тип поля не отображается в IEC.
pub(crate) fn emit_struct_types(
    p: &mut Printer,
    models: &[(Name, Rc<RefCell<ModelNode>>)],
    shared_arrays: &[String],
) -> Result<bool, Diagnostic> {
    // Позиция объявления едет вместе с ним: комментарий автора привязан к ней
    // (фича 0535, задача 05), а печать идёт ниже, где узла уже нет.
    let mut declared: Vec<DeclaredStruct> = Vec::new();
    for (_, model_rc) in models {
        let model = &*model_rc.borrow();
        // Порядок — по ЗАВИСИМОСТЯМ (фича 0341), а не алфавитный: вложенная
        // структура обязана быть объявлена раньше вмещающей, иначе `iec2c`
        // отвечает «invalid specification in structure element declaration».
        for node in crate::generator::struct_order::sorted(&model.structs) {
            let name = &node.name;
            if declared.iter().any(|(n, _, _)| n == name) {
                continue;
            }
            let mut fields = Vec::new();
            for (field, ty) in &node.fields {
                fields.push((field.clone(), get_st_type(ty, model)?));
            }
            declared.push((name.clone(), fields, node.loc));
        }
    }
    // Именованные типы массивов, разделяемых через `VAR_IN_OUT` (фича 0210):
    // MatIEC отвергает анонимный `ARRAY […] OF T` в объявлении параметра.
    // Собираются здесь же, чтобы `TYPE … END_TYPE` в файле остался ОДИН: вторая
    // секция типов рядом с первой — лишняя сущность и лишний повод разъехаться.
    let mut arrays = shared_array_types(models, shared_arrays)?;
    // Формы массивов, встречающиеся в ПАРАМЕТРАХ функций (фича 0348): MatIEC
    // не принимает анонимный `ARRAY […] OF T` в `VAR_INPUT`, а типы аргумента и
    // параметра обязаны совпадать — значит именованной должна быть **форма**, а
    // не переменная (в отличие от общих массивов под-моделей, 0210).
    for (name, ty) in function_array_forms(models)? {
        if !arrays.iter().any(|(n, _)| *n == name) {
            arrays.push((name, ty));
        }
    }

    if declared.is_empty() && arrays.is_empty() {
        return Ok(false);
    }
    // ⚠️ Сортировки по имени здесь НЕТ (фича 0341): порядок уже задан
    // зависимостями — вложенная структура собрана раньше вмещающей, — и
    // алфавитная сортировка на выходе его бы разрушила. Детерминированность
    // (инвариант 0048) обеспечивает сам обход: он идёт по `BTreeMap`.
    p.ident("TYPE").nl();
    p.up();
    for (name, fields, loc) in &declared {
        for line in crate::generator::comments::leading(
            *loc,
            crate::generator::header::CommentStyle::IecBlock,
        ) {
            p.ident(&line).nl();
        }
        p.ident(&format!("{} :", name)).nl();
        p.ident("STRUCT").nl();
        p.up();
        for (field, ty) in fields {
            check_st_field_name(field)?; // фича 0385: `from` в IEC ключевое
            p.ident(&format!("{} : {};", field, ty)).nl();
        }
        p.down();
        p.ident("END_STRUCT;").nl();
    }
    for (name, ty) in &arrays {
        p.ident(&format!("{} : {};", name, ty)).nl();
    }
    p.down();
    p.ident("END_TYPE").nl().nl();
    Ok(true)
}

/// Именованные типы массивов, разделяемых корнем через `VAR_IN_OUT` (фича 0210).
///
/// Владелец — **корень**: разделяются именно его переменные. Имя строит
/// `st_type::shared_array_type_name`, та же функция, что зовёт потребитель в
/// `VAR_IN_OUT`; второй формулы здесь быть не должно (урок ADR 0195).
///
/// ⚠️ Тип объявляется по **переменной корня**, а не по под-моделям: под-моделей
/// может быть несколько, и все они видят один и тот же массив.
/// Объявление структуры, готовое к печати: имя, поля и место в исходнике.
///
/// Тип именован ради читаемости: тройка с вложенным вектором пар — как раз тот
/// случай, о котором говорит `clippy::type_complexity`.
type DeclaredStruct = (String, Vec<(String, String)>, crate::diagnostics::Location);

fn shared_array_types(
    models: &[(Name, Rc<RefCell<ModelNode>>)],
    shared_arrays: &[String],
) -> Result<Vec<(String, String)>, Diagnostic> {
    let Some((root_name, root_rc)) = models.last() else {
        return Ok(Vec::new());
    };
    let root = &*root_rc.borrow();
    let mut out: Vec<(String, String)> = Vec::new();
    let mut names: Vec<&String> = root.variables.keys().collect();
    names.sort();
    for name in names {
        let VariableNode::Simple { ty, .. } = &root.variables[name] else {
            continue;
        };
        if !shared_arrays.iter().any(|n| n == name) {
            continue;
        }
        if !st_type::needs_named_array_type(ty, root) {
            continue;
        }
        out.push((
            st_type::shared_array_type_name(root_name.unique(), name),
            get_st_type(ty, root)?,
        ));
    }
    Ok(out)
}

/// Формы массивов из параметров функций — `(имя формы, объявление IEC)`.
///
/// ⚠️ Порядок детерминирован (инвариант 0048): обход идёт по отсортированным
/// именам функций и по порядку параметров.
fn function_array_forms(
    models: &[(Name, Rc<RefCell<ModelNode>>)],
) -> Result<Vec<(String, String)>, Diagnostic> {
    let mut out: Vec<(String, String)> = Vec::new();
    for (_, model_rc) in models {
        let model = &*model_rc.borrow();
        let mut names: Vec<&String> = model.functions.keys().collect();
        names.sort();
        for key in names {
            let (FunctionDefinitionNode::Local { params, ret, .. }
            | FunctionDefinitionNode::External { params, ret, .. }) = &model.functions[key]
            else {
                continue;
            };
            // Тип ВОЗВРАТА берётся наравне с параметрами (фича 0431):
            // анонимный `ARRAY […] OF T` в заголовке `FUNCTION` MatIEC
            // отвергает («invalid return type defined in function
            // declaration»), а именованный принимает — форма та же, что у
            // параметра-массива (0348).
            // Переменная модели, которую тело читает, приходит функции
            // НЕЯВНО — через `VAR_IN_OUT` (`state_params`), и массиву там
            // нужна та же именованная форма (фича 0466): без объявления
            // `iec2c` отвечал «unexpected token after 'VAR_IN_OUT'», а с
            // анонимной формой — «Data type incompatibility».
            let implicit: Vec<TypeNode> =
                crate::generator::st::st_func::state_params(&model.functions[key], model)
                    .into_iter()
                    .map(|(_, ty)| ty)
                    .collect();
            for ty in params
                .iter()
                .map(|(_, ty)| ty)
                .chain(std::iter::once(ret))
                .chain(implicit.iter())
            {
                let Some(form) = st_type::array_form_name(ty, model) else {
                    continue;
                };
                if !out.iter().any(|(n, _)| *n == form) {
                    out.push((form, get_st_type(ty, model)?));
                }
            }
        }
    }
    Ok(out)
}

/// Имена форм массивов из параметров функций (фича 0348).
///
/// Продюсер (`TYPE … END_TYPE`) и потребитель (объявление переменной и
/// параметра) обязаны спрашивать **один** список — разъехавшись, они дадут
/// ссылку в пустоту (урок ADR 0195, тот же довод, что у `named_arrays`).
pub(crate) fn function_array_form_names(models: &[(Name, Rc<RefCell<ModelNode>>)]) -> Vec<String> {
    function_array_forms(models)
        .map(|forms| forms.into_iter().map(|(name, _)| name).collect())
        .unwrap_or_default()
}
