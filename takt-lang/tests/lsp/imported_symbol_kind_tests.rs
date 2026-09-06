//! Вид импортированного символа: замер наблюдаемого следствия.
//!
//! # Вопрос
//!
//! Имя, введённое `import { a } from "...";`, объявляется видом `SymbolKind::Imported`,
//! который подходит **любому** пространству имён: вид объявления живёт в чужом файле, и
//! однофайловому слою `usages` он неизвестен. Комментарий 0256 называет допущение
//! односторонним - "лишняя связь внутри одного файла, где имя и так занято импортом".

#![cfg(feature = "lsp")]

use lsp_types::Position;
use takt_lang::lsp::references_at;

fn fixture(file: &str) -> String {
    format!("{}/tests/data/ws0401/{file}", env!("CARGO_MANIFEST_DIR"))
}

fn offset_position(text: &str, at: usize) -> Position {
    let head = &text[..at];
    Position::new(
        head.matches('\n').count() as u32,
        head.rsplit('\n').next().map_or(0, |l| l.chars().count()) as u32,
    )
}

/// Позиция `n`-го вхождения `needle` (нумерация с нуля).
fn position_of(text: &str, needle: &str, n: usize) -> Position {
    let mut from = 0;
    for _ in 0..n {
        from = text[from..].find(needle).expect("вхождение") + from + needle.len();
    }
    let at = text[from..].find(needle).expect("вхождение") + from;
    offset_position(text, at)
}

/// Имя, занятое импортом, и одноимённое состояние не смешиваются.
///
/// **Замер 2026-08-23 опроверг опасение.** Вид `Imported` подходит любому пространству,
/// и ожидалось, что вхождения состояния попадут в тот же символ, что импортированная
/// переменная. Прогон показал обратное: разрешение идёт по пространству **позиции**, а
/// не только по виду символа, - переменная связывается с директивой импорта и чтением в
/// теле, состояние со своим объявлением и `ref`.
///
/// Тест оставлен **тестом** этого свойства: оно и делает неточность вида
/// ненаблюдаемой, а значит держит решение "оставить как есть".
#[test]
fn imported_name_and_state_are_separate_symbols() {
    let text = std::fs::read_to_string(fixture("app.takt")).expect("фикстура читается");
    let on_import = references_at(&text, position_of(&text, "shared", 0), true)
        .expect("символ на директиве импорта");
    let on_state = references_at(&text, position_of(&text, "start shared", 0), true);

    // Курсор на `start shared` попадает на слово `start`; берём само имя.
    let state_pos = position_of(&text, "shared", 1);
    let on_state = on_state.or_else(|| references_at(&text, state_pos, true));

    let on_state = on_state.expect("имя состояния обязано находиться");
    assert!(
        !on_import.is_empty() && !on_state.is_empty(),
        "оба курсора обязаны находить вхождения: импорт {on_import:?}, состояние {on_state:?}"
    );
    assert_ne!(
        on_import, on_state,
        "вхождения импортированной переменной и одноимённого состояния \
         обязаны принадлежать РАЗНЫМ символам: смешав их, `rename` испортил бы \
         исходник, а он обещает «полнота или отказ» (0153)"
    );
    // Контроль состава: у каждого символа ровно свои места, а не подмножество.
    assert_eq!(on_import.len(), 2, "директива импорта и чтение в теле");
    assert_eq!(on_state.len(), 2, "объявление состояния и `ref`");
}
