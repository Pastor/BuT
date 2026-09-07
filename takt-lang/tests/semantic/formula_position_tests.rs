//! У формулы есть собственная позиция.
//!
//! # Что здесь проверяется
//!
//! Реестр мест объявления формул (`semantic/formula/sites.rs`) клал в диагностику
//! позицию **вместилища** - модели либо состояния, - потому что своей позиции у
//! `Formula` в дереве не было вовсе.
//!
//!
//! **Смежная опасность, ради которой позиция и нужна:** дедупликация
//! `diagnostics::normalize` сравнивает `(позиция, код, текст)`. Две записи с одинаковой
//! координатой и одинаковым текстом схлопнулись бы в одну - ровно та потеря, от которой
//! проверяла **счётом**. Тест T3 проверяет это напрямую.

use std::path::PathBuf;
use takt_lang::diagnostics::Location;
use takt_lang::generator::GenerateOptions;

/// Две формулы уровня модели: разные строки, разные условия.
const TWO_INVARIANTS: &str = "var n: u8 := 0;\n\
                              var m: u8 := 0;\n\
                              \n\
                              invariant Low = n < 10;\n\
                              invariant High = m < 20;\n\
                              \n\
                              start Run {\n\
                              \x20   always { n := n + 1; m := m + 2; }\n\
                              \x20   ref Run: n < 3;\n\
                              }\n";

/// Предупреждения цели `st` - наблюдаемый след позиции формулы.
///
/// Реестр мест (`semantic::formula::sites`) - внутренний слой; проверять его напрямую
/// значило бы проверять устройство вместо ответа пользователю. Цель `st` выдаёт по
/// `ST-022` на каждую охранную формулу, и её координата - та самая, о которой фича.
fn guard_warnings(tag: &str, src: &str) -> Vec<takt_lang::diagnostics::Diagnostic> {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir: PathBuf = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0282_{thread}_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание каталога");
    takt_lang::compile_to_st(
        tag,
        src,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение ST")
    .into_iter()
    .filter(|d| d.code.as_deref() == Some("ST-022"))
    .collect()
}

/// Строка по смещению позиции.
fn line_of(src: &str, loc: Location) -> usize {
    let Location::Source(_, start, _) = loc else {
        panic!("позиция обязана быть в исходнике: {loc:?}");
    };
    src[..start as usize].matches('\n').count() + 1
}

/// У каждой формулы своя позиция - строка её объявления.
#[test]
fn each_formula_has_its_own_position() {
    let found = guard_warnings("two", TWO_INVARIANTS);
    assert_eq!(found.len(), 2, "ожидались две формулы: {found:?}");
    let lines: Vec<usize> = found
        .iter()
        .map(|d| line_of(TWO_INVARIANTS, d.loc))
        .collect();
    assert_eq!(
        lines,
        vec![4, 5],
        "координаты обязаны указывать на строки объявления инвариантов"
    );
}

/// Встроенная формула состояния тоже несёт свою позицию.
#[test]
fn inline_formula_carries_its_position() {
    const INLINE: &str = "var n: u8 := 0;\n\
                          \n\
                          start Run {\n\
                          \x20   always { n := n + 1; }\n\
                          \x20   : n < 10;\n\
                          \x20   ref Run: n < 3;\n\
                          }\n";
    let found = guard_warnings("inline", INLINE);
    assert_eq!(found.len(), 1, "ожидалась одна формула: {found:?}");
    assert_eq!(line_of(INLINE, found[0].loc), 5);
}

/// Диагностики двух формул не схлопываются дедупликацией.
///
/// `diagnostics::normalize` сравнивает `(позиция, код, текст)`. Пока координата у обеих
/// формул была одна, одинаковый текст двух предупреждений означал бы потерю одного из
/// них - и счёт, которым проверяет, это поймал бы, а координата нет. Теперь позиции
/// разные, и проверка идёт прямо.
#[test]
fn two_formula_diagnostics_survive_normalize() {
    let found = guard_warnings("normalize", TWO_INVARIANTS);
    let same_text: Vec<takt_lang::diagnostics::Diagnostic> = found
        .iter()
        .map(|d| {
            takt_lang::diagnostics::Diagnostic::warning(
                d.loc,
                "охранная формула не транслируется".to_string(),
            )
            .with_code("ST-022")
        })
        .collect();
    let after = takt_lang::diagnostics::normalize(same_text);
    assert_eq!(
        after.len(),
        2,
        "две формулы с одинаковым текстом обязаны остаться двумя записями: {after:?}"
    );
}

/// **Контроль: формула, построенная вне разбора, берёт позицию вместилища.**
///
/// `condition_to_formula` строит `Guard` без позиции (`Location::Builtin`) - у неё нет
/// исходного текста. Запасной ход обязан остаться: иначе такая формула потеряла бы
/// координату вовсе.
#[test]
fn formula_without_own_position_falls_back_to_container() {
    use takt_lang::semantic::ConditionNode;
    use takt_lang::semantic::formula::{Formula, condition_to_formula};
    let built = condition_to_formula(&ConditionNode::Bool(true));
    assert!(
        matches!(built, Formula::Guard(_, None, Location::Builtin)),
        "формула вне разбора своей позиции не имеет: {built:?}"
    );
}
