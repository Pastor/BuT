//! Построение дерева **подключаемого** файла.
//!
//! Отдельный модуль по двум причинам. Формальная: `tree.rs` пришпилен реестром размеров
//! и расти не имеет права. Содержательная: "как строится подключаемый файл" - знание об
//! импорте, а не о разборе элементов модели, и живёт оно рядом с усыновлением
//! (`adopt.rs`) и выборкой (`select.rs`).

use crate::diagnostics::{Diagnostic, FileTable, Location};
use crate::parser::ast::Model;
use crate::semantic::ModelNode;
use std::cell::RefCell;
use std::rc::Rc;

/// Строит дерево **подключаемого** файла - в контексте импортёра.
///
/// Своего перечисления стадий не содержит: зовёт
/// [`construct_stages_within`](crate::semantic::stages::construct_stages_within) - тот
/// же носитель порядка, что и корневой файл, с тем же режимом `specialize` и с общим
/// стеком импорта (обнаружение циклов).
///
/// **Прежде здесь стояла вторая копия последовательности**, и она отстала на три
/// прохода: `collect_clock`, `specialize_instantiations`, `constify_parameters`
/// подключённому файлу не доставались вовсе.
///
/// Отдаёт **одну** диагностику: результат встраивается в стадию 0 импортёра, а она
/// терминальна - списку здесь некуда доехать. `normalize` перед взятием первой
/// обязателен: иначе "первой" окажется не самая ранняя по тексту, а первая по порядку
/// обхода.
pub(in crate::semantic) fn build_imported_file(
    model: &Model,
    importer: &Rc<RefCell<ModelNode>>,
    search_paths: &[String],
    import_stack: &mut Vec<String>,
    files: &mut FileTable,
    specialize: bool,
    import_loc: Location,
) -> Result<Rc<RefCell<ModelNode>>, Diagnostic> {
    let first = |ds: Vec<Diagnostic>| {
        crate::diagnostics::normalize(ds)
            .into_iter()
            .next()
            .unwrap_or_else(|| crate::semantic::internal::no_diagnostic("импорт дерева"))
    };
    let model = crate::semantic::stages::construct_stages_within(
        model,
        None,
        search_paths,
        import_stack,
        files,
        specialize,
    )
    .map_err(first)?;
    crate::semantic::validate::validate_model(model.clone())?;
    // Частота подключённого файла - контракт всей сборки, а не его личное дело.
    crate::semantic::time_ast::adopt_clock(importer, &model, import_loc)?;
    Ok(model)
}
