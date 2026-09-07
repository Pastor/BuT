//! Причина отказа проверки названа вердиктом.
//!
//! На входе за потолком первая строка вдобавок утверждала ложное ("атом - не
//! отслеживаемый предикат", хотя атомы там как раз отслеживаемые; сняла ложь, заменив
//! утверждение нейтральным, но причину не назвала).
//!
//! Здесь проверяется **соответствие входа причине**: каждая достижимая причина приходит
//! на своём входе и не приходит на чужом.

use takt_lang::parse;
use takt_lang::semantic::tree::construct_model;
use takt_lang::verification::verify::{UnsupportedReason, Verdict, verify_model};

/// Вердикт свойства `phi` на модели `src`.
fn verdict(src: &str, phi_src: &str) -> Verdict {
    let (ast, _) = parse(src, 0).expect("разбор модели");
    let model = construct_model(&ast, None, &[]).expect("построение дерева");
    let phi = takt_lang::parse_ltl_property(phi_src).expect("разбор формулы");
    let m = model.borrow();
    verify_model(&m, &phi)
}

/// Причина вердикта либо `None`, если проверка выполнена.
fn reason_of(src: &str, phi_src: &str) -> Option<UnsupportedReason> {
    match verdict(src, phi_src) {
        Verdict::Unsupported { reason, .. } => Some(reason),
        _ => None,
    }
}

/// Опечатка в имени атома - `UnknownAtom`.
#[test]
fn typo_in_atom_is_unknown_atom() {
    let r = reason_of("start A { ref A; }", "G nosuch");
    assert_eq!(r, Some(UnsupportedReason::UnknownAtom));
}

/// Предикат с арифметикой - `PredicateOutsideSubset`.
///
/// Тем же путём уходят `float`, `q`, массив и структура: неперечислимый тип отсекается
/// **подмножеством предикатов**, а не проверкой домена. Ветвь
/// `DomainNotEnumerable` сегодня недостижима и защитна - поэтому её здесь нет, и это
/// названо, а не забыто.
#[test]
fn arithmetic_predicate_is_outside_subset() {
    let src = "var t: u8 := 0; cond P = t + 1 > 2; start A { ref A; }";
    assert_eq!(
        reason_of(src, "G P"),
        Some(UnsupportedReason::PredicateOutsideSubset)
    );
}

/// `float` в предикате приходит той же причиной - подмножеством.
#[test]
fn float_predicate_is_outside_subset() {
    let src = "var x: float := 0.0; cond P = x <= 1.0; start A { ref A; }";
    assert_eq!(
        reason_of(src, "G P"),
        Some(UnsupportedReason::PredicateOutsideSubset)
    );
}

/// Три `u8` - `SizeOverLimit` (потолок считается по рёбрам).
///
/// Именно этот вход прежде получал ложную первую строку: атомы здесь отслеживаемые, а
/// сообщение говорило обратное.
#[test]
fn three_u8_is_size_over_limit() {
    let src = "var a: u8 := 0; var b: u8 := 0; var c: u8 := 0; \
               cond P = a <= b & b <= c; start A { ref A; }";
    assert_eq!(
        reason_of(src, "G P"),
        Some(UnsupportedReason::SizeOverLimit)
    );
}

/// `InitialValueUnknown` стала **недостижимой** - и это проверяется.
///
/// отвергает его **семантикой** (`SE-084`): значение `extern` при компиляции неизвестно
/// по определению, и прежде потребители расходились на нём молча (эталон - ноль, `st` -
/// потерянный инициализатор, прочие - отказ).
///
/// Тест проверяет **причину недостижимости**, а не молчит о ней: вход обязан
/// отвергаться на построении дерева, с кодом `SE-084`. Если запрет когда-нибудь
/// ослабят, тест упадёт - и ветвь причины снова станет живой.
#[test]
fn extern_call_initializer_is_rejected_before_verification() {
    let src = "extern fn src() -> u8; var x: u8 := src(); \
               cond P = x = 0; start A { ref A; }";
    let (ast, _) = parse(src, 0).expect("разбор модели");
    let diagnostic = construct_model(&ast, None, &[])
        .expect_err("вызов внешней функции в инициализаторе обязан отвергаться");
    assert_eq!(diagnostic.code.as_deref(), Some("SE-084"), "{diagnostic:?}");
}

/// Проверяемое свойство причины не получает вовсе.
///
/// Без контроля "причина такая-то" доказывало бы лишь, что вердикт всегда
/// `Unsupported`.
#[test]
fn checkable_property_has_no_reason() {
    let src = "var f: bit := 0; cond P = f = 0; start A { ref A; }";
    assert_eq!(reason_of(src, "G P"), None, "свойство над `bit` проверяемо");
}

/// Тексты причин различны и не пусты - иначе называть причину бессмысленно.
#[test]
fn reason_texts_are_distinct_and_nonempty() {
    let all = [
        UnsupportedReason::UnknownAtom,
        UnsupportedReason::PredicateOutsideSubset,
        UnsupportedReason::DomainNotEnumerable,
        UnsupportedReason::SizeOverLimit,
        UnsupportedReason::InitialValueUnknown,
    ];
    let mut seen: Vec<&str> = Vec::new();
    for r in all {
        let text = r.text();
        assert!(!text.trim().is_empty(), "причина {r:?} без текста");
        assert!(
            !text.contains("Unsupported") && !text.contains("Reason"),
            "текст причины не должен нести имя варианта Rust (класс 0231): {text}"
        );
        assert!(
            !seen.contains(&text),
            "две причины с одним текстом — различать их нечем: {text}"
        );
        seen.push(text);
    }
}
