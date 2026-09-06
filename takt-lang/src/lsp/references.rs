//! `textDocument/references` - поиск всех использований символа.
//!
//! Работает на слое использований (`semantic::usages`), а не на `SemanticIndex`: индекс
//! не видит вхождений в телах `enter`/`always` и в телах функций - позиции теряются при
//! семантическом понижении.
//!
//! Часть модуля `lsp`.

use super::*;
use crate::lsp::workspace::Workspace;
use crate::semantic::usages::{self, UsageKind};

/// Вхождение символа в конкретном файле рабочей области.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileReference {
    /// Путь файла (как он найден при обходе области).
    pub path: String,
    /// Диапазон имени в координатах редактора.
    pub range: Range,
}

/// Все вхождения символа под курсором - **по всей рабочей области**.
///
/// `roots` - корни области (`workspaceFolders`/`rootUri`; при их отсутствии вызывающий
/// передаёт каталог документа), `search_paths` - пути поиска импортов, `overlay` -
/// текст открытых документов: у редактора он свежее диска.
///
/// ## Что считается вхождением через границу файла
///
/// Правило целиком живёт в слое рабочей области ([`Workspace::resolve`]):
/// связывается только то, что импорт действительно переносит импортёру, и
/// **не** связывается имя, которое импорт сам же и вводит (`import "файл";`
/// даёт имя по имени файла, `as M` - алиас). Иначе ответ содержал бы вхождения
/// чужого символа - см. предупреждение в шапке `workspace.rs`.
///
/// ## Границы
///
/// Файл вне рабочей области сервером не виден: свойство задачи, а не реализации.
/// Возвращает `None`, если файла нет в области, он не разбирается или под курсором нет
/// имени.
pub fn references_in_workspace(
    path: &str,
    position: Position,
    include_declaration: bool,
    roots: &[String],
    search_paths: &[String],
    overlay: &dyn Fn(&str) -> Option<String>,
) -> Option<Vec<FileReference>> {
    let workspace = Workspace::scan(roots, search_paths, overlay);
    let source = workspace.text_of(path)?.to_string();
    let offset = position_to_offset(&source, position)?;
    let resolution = workspace.resolve(path, offset)?;

    let mut out = Vec::new();
    for occurrence in resolution.occurrences {
        if !include_declaration && occurrence.declaration {
            continue;
        }
        // Диапазон считается по тексту своего файла: смещения принадлежат ему, и
        // перевод в строку-колонку по чужому тексту дал бы верное на вид, но неверное
        // место.
        let text = workspace.text_of(&occurrence.path)?;
        out.push(FileReference {
            path: occurrence.path.clone(),
            range: offset_to_range(text, occurrence.start as usize, occurrence.end as usize),
        });
    }
    Some(out)
}

/// Вхождения символа **в одном тексте** - путь, не требующий рабочей области.
///
/// Оставлен для потребителей, у которых области нет (проверка формы ответа, плагины
/// поверх библиотеки). Кросс-файловый ответ даёт [`references_in_workspace`].
pub fn references_at(
    source: &str,
    position: Position,
    include_declaration: bool,
) -> Option<Vec<Range>> {
    let (ast, _) = crate::parse(source, 0).ok()?;
    let table = usages::collect_usages(&ast);
    let offset = position_to_offset(source, position)?;
    let symbol = table.usage_at(offset)?.symbol;

    let ranges = table
        .occurrences_of(symbol)
        .into_iter()
        .filter(|u| include_declaration || u.kind != UsageKind::Declaration)
        .map(|u| offset_to_range(source, u.start as usize, u.end as usize))
        .collect();
    Some(ranges)
}
