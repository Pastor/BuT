//! Реализация модели `model M = A | B { ... }`.
//!
//! # Как лечится
//!
//! Реализация разворачивается на построении дерева в синтетическое стартовое состояние -
//! то есть форма становится синонимом `model M { start Имя = A | B; }`, которая
//! работала всегда. За границей построения формы не существует, и пять потребителей
//! нового вида узла не видят.

use takt_lang::generator::GenerateOptions;

/// Форма 1: реализация + тело **без** собственных состояний.
const IMPLEMENT_WITH_BODY: &str = "var probe: u8 := 0;\n\
                                   model A { start C1 { ref C1; } }\n\
                                   model B { start C2 { ref C2; } }\n\
                                   model M = A | B { always { probe := probe + 1; } }\n\
                                   start Main = M;\n";

/// Форма 2: реализация **и** собственное состояние - конфликт.
const IMPLEMENT_WITH_OWN_STATE: &str = "var probe: u8 := 0;\n\
                                        model A { start C1 { ref C1; } }\n\
                                        model B { start C2 { ref C2; } }\n\
                                        model M = A | B { start S { ref S; } }\n\
                                        start Main = M;\n";

/// Эквивалент, работавший всегда, - контрольная форма.
const EQUIVALENT: &str = "var probe: u8 := 0;\n\
                          model A { start C1 { ref C1; } }\n\
                          model B { start C2 { ref C2; } }\n\
                          model M { always { probe := probe + 1; } start Inner = A | B; }\n\
                          start Main = M;\n";

fn build_dir(tag: &str) -> std::path::PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .to_string();
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0199_{thread}_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог вывода");
    dir
}

/// Код диагностики цели (или `None` при успехе) и число примечаний.
fn target_result(target: &str, tag: &str, source: &str) -> Option<(String, usize)> {
    let dir = build_dir(tag);
    let path = dir.to_str().expect("путь в UTF-8");
    let opts = GenerateOptions::default();
    let result = match target {
        "c" => takt_lang::compile_to_c(tag, source, path, &[], &opts),
        "rust" => takt_lang::compile_to_rust(tag, source, path, &[], &opts),
        "sv" => takt_lang::compile_to_sv(tag, source, path, &[], &opts),
        "st" => takt_lang::compile_to_st(tag, source, path, &[], &opts),
        other => panic!("неизвестная цель '{other}'"),
    };
    result
        .err()
        .map(|d| (d.code.unwrap_or_else(|| "?".into()), d.notes.len()))
}

/// Форма переводится **всеми четырьмя** целями.
///
/// Проверяются все четыре не для полноты счёта: правка, разрешающая
/// `implements` в поле, и от этого заработал **только эталон** - цели остались
/// красными, потому что карта строит состояния из `model.states`.
#[test]
fn implement_with_body_is_accepted_by_every_target() {
    for target in ["c", "rust", "sv", "st"] {
        assert_eq!(
            target_result(target, &format!("ok_{target}"), IMPLEMENT_WITH_BODY),
            None,
            "цель {target} обязана перевести реализацию модели"
        );
    }
}

/// Реализация вместе с собственным состоянием - `SE-101`.
///
/// Диагностика обязана нести **обе** позиции: саму модель и её состояние. Без второй
/// автор не поймёт, что именно конфликтует.
#[test]
fn implement_with_own_state_is_rejected() {
    for target in ["c", "rust", "sv", "st"] {
        assert_eq!(
            target_result(
                target,
                &format!("conflict_{target}"),
                IMPLEMENT_WITH_OWN_STATE
            ),
            Some(("SE-101".to_string(), 1)),
            "цель {target}: конфликт обязан отвергаться семантикой с примечанием"
        );
    }
}

/// Форма без тела остаётся синтаксической ошибкой.
///
/// Грамматика требует блок после имени модели; объём фичи этого не менял.
#[test]
fn implement_without_body_stays_a_syntax_error() {
    let src = "model A { start C1 { ref C1; } }\n\
               model B { start C2 { ref C2; } }\n\
               model M = A | B;\n\
               start Main = M;\n";
    let got = target_result("c", "nobody", src);
    assert_eq!(
        got.map(|(code, _)| code),
        Some("SY-002".to_string()),
        "форма без тела грамматикой не принимается"
    );
}

/// Эквивалент, работавший всегда, не задет.
#[test]
fn equivalent_form_still_works() {
    for target in ["c", "rust", "sv", "st"] {
        assert_eq!(
            target_result(target, &format!("eq_{target}"), EQUIVALENT),
            None,
            "цель {target} обязана по-прежнему принимать эквивалентную запись"
        );
    }
}

/// Синтетическое состояние видно в выводе - форма развёрнута, а не поддержана особым
/// случаем.
///
/// Проверка **текстом**: именно разворот в состояние делает форму синонимом, и если он
/// исчезнет, пять потребителей снова разойдутся.
#[test]
fn implement_is_expanded_into_a_state() {
    let dir = build_dir("expand");
    takt_lang::compile_to_c(
        "expand",
        IMPLEMENT_WITH_BODY,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение C");
    let header = std::fs::read_to_string(dir.join("expand.h")).expect("заголовок");
    assert!(
        header.contains("EXPAND_M_IMPLEMENT"),
        "реализация обязана разворачиваться в состояние 'Implement':\n{header}"
    );
}
