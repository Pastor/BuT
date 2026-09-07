//! Рабочая область: `references` и `rename` между файлами.
//!
//! Тесты идут **на ответ сервера**, а не на устройство слоя: вопрос фичи - "какие
//! вхождения увидит пользователь и какие файлы изменит правка".

#![cfg(feature = "lsp")]

use lsp_types::Position;
use takt_lang::lsp::{
    RenameRefusal, Workspace, prepare_rename_in_workspace, references_in_workspace,
    rename_in_workspace,
};

/// Каталог фикстур фичи.
fn fixture(sub: &str) -> String {
    format!("{}/tests/data/ws0153/{sub}", env!("CARGO_MANIFEST_DIR"))
}

fn path_in(sub: &str, file: &str) -> String {
    format!("{}/{file}", fixture(sub))
}

/// Тексты берутся с диска: открытых документов у тестов нет.
fn no_overlay(_: &str) -> Option<String> {
    None
}

/// Позиция `n`-го вхождения имени в файле (нумерация с нуля).
fn position_of(path: &str, needle: &str, n: usize) -> Position {
    let text = std::fs::read_to_string(path).expect("фикстура читается");
    let mut from = 0usize;
    let mut at = None;
    for _ in 0..=n {
        let found = text[from..].find(needle).expect("вхождение имени") + from;
        at = Some(found);
        from = found + needle.len();
    }
    let at = at.expect("вхождение");
    let before = &text[..at];
    Position::new(
        before.matches('\n').count() as u32,
        (at - before.rfind('\n').map(|i| i + 1).unwrap_or(0)) as u32,
    )
}

/// Сколько вхождений найдено в каждом файле ответа.
fn per_file(refs: &[takt_lang::lsp::FileReference]) -> Vec<(String, usize)> {
    let mut out: Vec<(String, usize)> = Vec::new();
    for r in refs {
        let name = std::path::Path::new(&r.path)
            .file_name()
            .expect("имя файла")
            .to_string_lossy()
            .into_owned();
        match out.iter_mut().find(|(p, _)| *p == name) {
            Some((_, n)) => *n += 1,
            None => out.push((name, 1)),
        }
    }
    out.sort();
    out
}

/// A1: вхождения символа библиотеки видны во всех файлах области, а не только в
/// открытом.
#[test]
fn references_cross_files_from_declaration() {
    let lib = path_in("ok", "lib.takt");
    let refs = references_in_workspace(
        &lib,
        position_of(&lib, "speed", 0),
        true,
        &[fixture("ok")],
        &[],
        &no_overlay,
    )
    .expect("символ найден");
    let counts = per_file(&refs);
    assert_eq!(
        counts,
        vec![
            ("lib.takt".to_string(), 1),
            ("uses_plain.takt".to_string(), 2),
            ("uses_selected.takt".to_string(), 2),
        ],
        "вхождения обязаны собираться по всей области: {counts:?}"
    );
}

/// A1: тот же ответ, если курсор стоит у потребителя, а не в библиотеке.
#[test]
fn references_cross_files_from_consumer() {
    let user = path_in("ok", "uses_plain.takt");
    let refs = references_in_workspace(
        &user,
        // Ищется вхождение В коде: первое `speed` в файле стоит в комментарии, и
        // позиция на нём проверяла бы не то.
        position_of(&user, "speed :=", 0),
        true,
        &[fixture("ok")],
        &[],
        &no_overlay,
    )
    .expect("символ найден");
    assert_eq!(
        per_file(&refs),
        vec![
            ("lib.takt".to_string(), 1),
            ("uses_plain.takt".to_string(), 2),
            ("uses_selected.takt".to_string(), 2),
        ]
    );
}

/// A6: до фичи `references` на импортированном имени отвечал `None` - не полуправдой, а
/// молчанием. Тест на сам факт ответа.
#[test]
fn references_on_imported_name_is_not_silence() {
    let user = path_in("ok", "uses_selected.takt");
    let refs = references_in_workspace(
        &user,
        position_of(&user, "speed :=", 0), // вхождение в теле, не в строке импорта
        true,
        &[fixture("ok")],
        &[],
        &no_overlay,
    )
    .expect("импортированное имя обязано давать ответ, а не None");
    assert!(!refs.is_empty());
}

/// A4: локальное объявление затеняет импортированное - файл `shadow.takt` в ответе не
/// появляется, хотя имя там то же.
#[test]
fn local_declaration_shadows_imported_symbol() {
    let lib = path_in("ok", "lib.takt");
    let refs = references_in_workspace(
        &lib,
        position_of(&lib, "speed", 0),
        true,
        &[fixture("ok")],
        &[],
        &no_overlay,
    )
    .expect("символ найден");
    assert!(
        !refs.iter().any(|r| r.path.ends_with("shadow.takt")),
        "затенённое имя принадлежит своему файлу: {refs:?}"
    );
}

/// A2: `rename` правит все файлы области, где символ употреблён.
#[test]
fn rename_edits_every_consumer() {
    let lib = path_in("ok", "lib.takt");
    let plan = rename_in_workspace(
        &lib,
        position_of(&lib, "speed", 0),
        "velocity",
        &[fixture("ok")],
        &[],
        &no_overlay,
    )
    .expect("переименование по области");
    let mut files: Vec<String> = plan
        .iter()
        .map(|(p, edits)| {
            format!(
                "{}:{}",
                std::path::Path::new(p)
                    .file_name()
                    .unwrap()
                    .to_string_lossy(),
                edits.len()
            )
        })
        .collect();
    files.sort();
    assert_eq!(
        files,
        vec![
            "lib.takt:1".to_string(),
            "uses_plain.takt:2".to_string(),
            "uses_selected.takt:2".to_string()
        ],
        "правки обязаны уйти во все файлы-потребители"
    );
}

/// A2: применённые правки оставляют файлы разбираемыми - правка встала на имена, а не в
/// произвольные места.
#[test]
fn renamed_files_still_parse() {
    let lib = path_in("ok", "lib.takt");
    let plan = rename_in_workspace(
        &lib,
        position_of(&lib, "speed", 0),
        "velocity",
        &[fixture("ok")],
        &[],
        &no_overlay,
    )
    .expect("переименование по области");
    for (path, edits) in plan {
        let text = std::fs::read_to_string(&path).expect("чтение");
        let patched = apply(&text, &edits);
        assert!(
            takt_lang::parse(&patched, 0).is_ok(),
            "после правки файл {path} перестал разбираться:\n{patched}"
        );
        assert_eq!(
            patched.matches("velocity").count(),
            edits.len(),
            "в {path} применились не все правки:\n{patched}"
        );
        // Проверять "старого имени не осталось вовсе" нельзя: имя законно встречается в
        // комментариях, а комментарии переименование не трогает.
        assert!(
            !patched.contains("speed :="),
            "старое имя осталось в коде {path}:\n{patched}"
        );
    }
}

/// A3 (контрпример): имя, введённое `import "файл";`, получено из имени файла и с
/// объявлением внутри библиотеки не связано.
///
/// Исправлениетура `goto56`: файл `helper.takt` объявляет `model Helper`, импортёр пишет
/// `Helper` - но означает он имя файла. Наивное связывание "одинаковое имя ⇒ один
/// символ" правило бы импортёра и ломало его молча.
#[test]
fn file_derived_model_name_is_not_bound_to_library_model() {
    let root = format!("{}/tests/data/goto56", env!("CARGO_MANIFEST_DIR"));
    let helper = format!("{root}/helper.takt");
    let refs = references_in_workspace(
        &helper,
        position_of(&helper, "Helper", 0),
        true,
        std::slice::from_ref(&root),
        &[],
        &no_overlay,
    )
    .expect("имя модели найдено");
    assert!(
        !refs.iter().any(|r| r.path.ends_with("uses_helper.takt")),
        "имя у импортёра получено из имени файла — правка библиотеки его не касается: {refs:?}"
    );
}

/// A3: и обратно - переименовать такое имя у импортёра нельзя: оно ни с чем не связано,
/// а правка одного вхождения оторвала бы его от имени файла.
#[test]
fn renaming_file_derived_name_is_refused() {
    let root = format!("{}/tests/data/goto56", env!("CARGO_MANIFEST_DIR"));
    let user = format!("{root}/uses_helper.takt");
    let refusal = prepare_rename_in_workspace(
        &user,
        position_of(&user, "Helper", 0), // единственное вхождение: `start Main = Helper;`
        &[root],
        &[],
        &no_overlay,
    )
    .expect_err("имя по имени файла переименовывать нельзя");
    assert_eq!(refusal, RenameRefusal::ForeignDeclaration);
}

/// A5: файл области, который не разбирается и потребляет символ, - отказ.
#[test]
fn unparsable_consumer_refuses_rename() {
    let lib = path_in("broken", "lib.takt");
    let refusal = rename_in_workspace(
        &lib,
        position_of(&lib, "flow", 0),
        "rate",
        &[fixture("broken")],
        &[],
        &no_overlay,
    )
    .expect_err("потребитель не разбирается — полнота недостижима");
    assert_eq!(refusal, RenameRefusal::UnparsableConsumer);
}

/// A5: имя, объявленное сразу двумя подключёнными файлами, - отказ.
#[test]
fn ambiguous_import_refuses_rename() {
    let user = path_in("ambig", "user.takt");
    let refusal = rename_in_workspace(
        &user,
        position_of(&user, "level", 1),
        "grade",
        &[fixture("ambig")],
        &[],
        &no_overlay,
    )
    .expect_err("какое из двух объявлений — область не знает");
    assert_eq!(refusal, RenameRefusal::AmbiguousImport);
}

/// A5: новое имя уже занято в затрагиваемом файле - отказ.
///
/// Проверка появилась вместе с кросс-файловой правкой: пока правился один файл,
/// столкновение видел автор.
#[test]
fn taken_name_refuses_rename() {
    let lib = path_in("ok", "lib.takt");
    let refusal = rename_in_workspace(
        &lib,
        position_of(&lib, "speed", 0),
        "doubled", // функция, объявленная тут же
        &[fixture("ok")],
        &[],
        &no_overlay,
    )
    .expect_err("занятое имя завело бы затенение");
    assert_eq!(refusal, RenameRefusal::NameTaken);
}

/// A7: область - одно знание: слой видит ровно `.takt` своих корней, и оба запроса
/// работают поверх него.
#[test]
fn workspace_sees_every_takt_of_its_roots() {
    let ws = Workspace::scan(&[fixture("ok")], &[], &no_overlay);
    let on_disk = std::fs::read_dir(fixture("ok"))
        .expect("каталог фикстур")
        .flatten()
        .filter(|e| e.path().extension().is_some_and(|x| x == "takt"))
        .count();
    assert_eq!(ws.len(), on_disk, "область обязана видеть все файлы корня");
}

/// Открытый документ сильнее диска: правка строится по тексту редактора.
#[test]
fn overlay_text_wins_over_disk() {
    let lib = path_in("ok", "lib.takt");
    let edited = std::fs::read_to_string(&lib).expect("чтение").replace(
        "var speed: u8 := 0;",
        "var speed: u8 := 0; // правка в редакторе",
    );
    let overlay = |p: &str| (p == lib).then(|| edited.clone());
    let ws = Workspace::scan(&[fixture("ok")], &[], &overlay);
    assert!(
        ws.text_of(&lib)
            .expect("файл области")
            .contains("правка в редакторе"),
        "текст открытого документа обязан побеждать диск"
    );
}

/// Применяет правки к тексту (от конца к началу - диапазоны не пересекаются).
fn apply(text: &str, edits: &[lsp_types::TextEdit]) -> String {
    let mut offsets: Vec<(usize, usize, String)> = edits
        .iter()
        .map(|e| {
            let start = offset_of(text, e.range.start);
            let end = offset_of(text, e.range.end);
            (start, end, e.new_text.clone())
        })
        .collect();
    offsets.sort_by_key(|(s, _, _)| std::cmp::Reverse(*s));
    let mut out = text.to_string();
    for (start, end, new_text) in offsets {
        out.replace_range(start..end, &new_text);
    }
    out
}

fn offset_of(text: &str, position: Position) -> usize {
    let mut offset = 0usize;
    for (i, line) in text.split_inclusive('\n').enumerate() {
        if i == position.line as usize {
            return offset + position.character as usize;
        }
        offset += line.len();
    }
    offset
}
