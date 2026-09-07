//! Имя, введённое выборочным импортом, - символ своего вида.
//!
//! Предмет - **согласие двух курсоров**: ответ на имени в директиве `import { A } from
//! "...";` обязан совпадать с ответом на том же имени в теле.

#![cfg(feature = "lsp")]

use lsp_types::Position;
use takt_lang::lsp::{RenameRefusal, references_at, references_in_workspace, rename_in_workspace};

fn fixture(sub: &str) -> String {
    format!("{}/tests/data/ws0153/{sub}", env!("CARGO_MANIFEST_DIR"))
}

fn path_in(sub: &str, file: &str) -> String {
    format!("{}/{file}", fixture(sub))
}

fn no_overlay(_: &str) -> Option<String> {
    None
}

/// Позиция по байтовому смещению в тексте.
fn offset_position(text: &str, at: usize) -> Position {
    let head = &text[..at];
    Position::new(
        head.matches('\n').count() as u32,
        head.rsplit('\n').next().map_or(0, |l| l.chars().count()) as u32,
    )
}

/// Позиция `n`-го вхождения подстроки в тексте (нумерация с нуля).
fn position_of(text: &str, needle: &str, n: usize) -> Position {
    let at = text
        .match_indices(needle)
        .nth(n)
        .unwrap_or_else(|| panic!("нет {}-го вхождения `{needle}`", n + 1))
        .0;
    let head = &text[..at];
    Position::new(
        head.matches('\n').count() as u32,
        head.rsplit('\n').next().map_or(0, |l| l.chars().count()) as u32,
    )
}

/// Файл, где имя `speed` приходит формой `import { speed } from "lib.takt";` и
/// используется в теле.
fn selected_consumer() -> (String, String) {
    let path = path_in("ok", "uses_selected.takt");
    let text = std::fs::read_to_string(&path).expect("фикстура читается");
    (path, text)
}

/// A1: однофайловый ответ одинаков с обоих курсоров - на директиве импорта и на
/// вхождении в теле.
///
/// Проверяется **равенство ответов**, а не число: число зависит от фикстуры и
/// поменяется с её правкой, а равенство есть само свойство фичи.
#[test]
fn both_cursors_agree_in_single_file() {
    let (_, text) = selected_consumer();
    let on_import = references_at(&text, position_of(&text, "speed", 1), true);
    let in_body = references_at(&text, position_of(&text, "speed :=", 0), true);
    assert!(
        on_import.is_some() && in_body.is_some(),
        "оба курсора обязаны находить символ: импорт {on_import:?}, тело {in_body:?}"
    );
    assert_eq!(
        on_import, in_body,
        "ответ не должен зависеть от того, куда поставлен курсор"
    );
}

/// A1: вхождение в теле связано с объявлением-импортом, а не потеряно.
///
/// Контроль к предыдущему тесту: равенство двух `None` тоже было бы равенством.
#[test]
fn body_occurrence_is_bound_to_the_import() {
    let (_, text) = selected_consumer();
    let refs = references_at(&text, position_of(&text, "speed", 1), true)
        .expect("символ на имени в директиве импорта");
    assert!(
        refs.len() >= 2,
        "в файле есть строка импорта и вхождение в теле — найдено {}: {refs:?}",
        refs.len()
    );
}

/// A2: в рабочей области оба курсора тоже дают один ответ - включая объявление в
/// библиотеке и вхождения у прочих потребителей.
#[test]
fn both_cursors_agree_in_workspace() {
    let (path, text) = selected_consumer();
    let roots = [fixture("ok")];
    let on_import = references_in_workspace(
        &path,
        position_of(&text, "speed", 1),
        true,
        &roots,
        &[],
        &no_overlay,
    )
    .expect("символ на директиве импорта");
    let in_body = references_in_workspace(
        &path,
        position_of(&text, "speed :=", 0),
        true,
        &roots,
        &[],
        &no_overlay,
    )
    .expect("символ на вхождении в теле");
    assert_eq!(
        on_import.len(),
        in_body.len(),
        "ответ области не должен зависеть от курсора: {on_import:?} против {in_body:?}"
    );
    assert!(
        on_import.iter().any(|r| r.path.ends_with("lib.takt")),
        "объявление библиотеки обязано быть в ответе: {on_import:?}"
    );
}

/// A3: переименование с курсора на директиве импорта правит и библиотеку, и обоих
/// потребителей - то есть отказ `ModelName` здесь не срабатывает.
#[test]
fn rename_from_import_directive_edits_library() {
    let (path, text) = selected_consumer();
    let edits = rename_in_workspace(
        &path,
        position_of(&text, "speed", 1),
        "velocity",
        &[fixture("ok")],
        &[],
        &no_overlay,
    )
    .expect("переименование с директивы импорта");
    let files: Vec<&str> = edits
        .iter()
        .map(|(p, _)| p.rsplit('/').next().unwrap_or(p.as_str()))
        .collect();
    assert!(
        files.contains(&"lib.takt") && files.contains(&"uses_selected.takt"),
        "правка обязана задеть библиотеку и потребителя: {files:?}"
    );
}

/// A4 (**контроль границы**): имя формы `import "файл" as M;` остаётся моделью.
///
/// Оно введено самим импортом и за границей файла ни с чем не связано (решение ):
/// переименовать его - значит разойтись с алиасом, о котором знает только импортёр. Без
/// этой проверки читалась бы как "связывать всё подряд", и её правка вида символа
/// поехала бы на вторую форму импорта.
#[test]
fn alias_import_is_still_a_model_name() {
    let path = path_in("alias", "uses_alias.takt");
    let text = std::fs::read_to_string(&path).expect("фикстура alias читается");
    // Позиция берётся по `as Lib` со сдвигом: голое `Lib` встречается в фикстуре раньше -
    // в имени файла `lib.takt` поиск нечувствителен к тому, что там строчные буквы
    // только у части подстроки (: проба проверяется, и эта уже ошиблась однажды - отказ
    // приходил `NoSymbol`).
    let alias_at = text.find("as Lib").expect("алиас") + 3;
    let refusal = rename_in_workspace(
        &path,
        offset_position(&text, alias_at),
        "Other",
        &[fixture("alias")],
        &[],
        &no_overlay,
    );
    assert!(
        matches!(refusal, Err(RenameRefusal::ModelName)),
        "алиас импорта переименованию не подлежит: {refusal:?}"
    );
}
