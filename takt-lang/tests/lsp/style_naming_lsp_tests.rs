//! Проводка `CS-001` в LSP.
//!
//! назвал **двух** потребителей канона: `taktc fmt` и редактор. Первого проверяет
//! `style_naming_fmt_tests.rs`, второго - этот файл.
//!
//! # Что здесь ловится
//!
//! 1. **Критерий приёмки 3:** редактор отдаёт то же предупреждение,
//!    уровень - `WARNING` (не ошибка: имя - совет), с кодом в поле протокола и
//!    диапазоном на самом имени.
//! 2. **"Одна реализация на двоих".** Текст предупреждения у `fmt` и у LSP
//!    сверяется **буквально**: две копии правил - класс, который проект закрывал
//!    в 0084, 0193 и 0195, и здесь он был бы особенно тих (кто сверяет вывод
//!    форматтера с подсказкой редактора?).
//! 3. **Паритет доставки на файле, который не собирается**.
//!    Предупреждение о стиле выдаётся во **всех** ветвях возврата
//!    `collect_diagnostics_at` - рядом с ошибкой построения дерева и рядом с
//!    ошибкой `validate`, - потому что канон именования от смысла не зависит.
//!    Прежде редактор здесь молчал, а `fmt` печатал.
//! 4. **Узость этого исключения.** Предупреждения о **смысле** (`SE-036` и
//!    прочие) при ошибках по-прежнему не показываются: они смотрят на
//!    построенную модель, и на отвергнутой их вердикт может быть ложным. Тест
//!    `lsp_still_hides_semantic_warnings_when_errors_present` держит границу.

#![cfg(feature = "lsp")]

use lsp_types::{DiagnosticSeverity, NumberOrString};
use takt_lang::lsp::collect_diagnostics;

/// Каноничный по формату файл с неканоничным именем порта (`BadPort` на строке 2,
/// колонка 9 - та же фикстура, что у тестов `fmt`).
const BAD_NAME: &str = "\
model M {
    out BadPort: bit;
    start S {
        always {
            BadPort := 1;
        }
    }
}
start Main = M;
";

/// То же, но имя каноничное - контр-пример.
const GOOD_NAME: &str = "\
model M {
    out good_port: bit;
    start S {
        always {
            good_port := 1;
        }
    }
}
start Main = M;
";

/// Семантически некорректный файл (`SE-034`: тип не найден) с неканоничным именем
/// переменной.
const SEMANTICALLY_BROKEN: &str = "\
model M {
    var BadVar: NoSuchType := 1;
    start S {
        always {
            BadVar := 1;
        }
    }
}
start Main = M;
";

/// Дерево строится, но `validate` отвергает модель (`SE-026`: запись во входной порт).
/// Неканоничных имён здесь **два**, и одно из них (`Unused`) заодно даёт `SE-036` - на
/// нём видно, что политика для предупреждений о смысле не менялась.
const VALIDATE_BROKEN: &str = "\
model M {
    in BadPort: bit;
    var Unused: u8 := 0;
    start S {
        always {
            BadPort := 1;
        }
    }
}
start Main = M;
";

/// Файл, который **не разбирается** (пропущено `:` в объявлении), - граница: АСД нет,
/// проверять форму имени нечем.
const PARSE_BROKEN: &str = "\
model M {
    var BadVar u8 := 1;
    start S;
}
start Main = M;
";

/// Только диагностики `CS-001`.
fn style_diagnostics(source: &str) -> Vec<lsp_types::Diagnostic> {
    collect_diagnostics(source)
        .into_iter()
        .filter(|d| d.code == Some(NumberOrString::String("CS-001".to_string())))
        .collect()
}

/// **Критерий 3.** Редактор отдаёт `CS-001` предупреждением, с кодом и
/// источником.
///
/// Уровень проверяется явно: ошибкой канон именования быть не должен - иначе редактор
/// объявил бы отказом то, что инструмент считает советом (и что не меняет код возврата
/// `fmt --check`).
#[test]
fn lsp_reports_cs001_as_warning_with_code() {
    let diags = style_diagnostics(BAD_NAME);
    assert_eq!(diags.len(), 1, "ожидалась одна диагностика: {diags:?}");
    let d = &diags[0];

    assert_eq!(
        d.severity,
        Some(DiagnosticSeverity::WARNING),
        "канон именования — предупреждение, а не ошибка: {d:?}"
    );
    assert_eq!(
        d.source.as_deref(),
        Some("takt-lsp"),
        "источник диагностики: {d:?}"
    );
    assert!(
        d.message.contains("порт 'BadPort'") && d.message.contains("snake_case"),
        "текст обязан называть вид объявления и ожидаемую форму: {:?}",
        d.message
    );
}

/// Диапазон лежит **на имени**, а не в начале файла и не на всём объявлении.
///
/// По нему редактор подчёркивает слово; нулевой диапазон (`0,0`-`0,0`), которым
/// `grammar_diagnostic_to_lsp` спасает диагностики без координат, здесь означал бы
/// подчёркивание не там.
#[test]
fn lsp_range_covers_the_name_itself() {
    let diags = style_diagnostics(BAD_NAME);
    let range = diags[0].range;

    // ` out BadPort: bit;` - вторая строка (индекс 1), имя с 9-й колонки (индекс 8),
    // длина 7.
    assert_eq!(range.start.line, 1, "строка имени: {range:?}");
    assert_eq!(range.start.character, 8, "колонка имени: {range:?}");
    assert_eq!(range.end.line, 1, "имя не переносится: {range:?}");
    assert_eq!(
        range.end.character - range.start.character,
        "BadPort".len() as u32,
        "диапазон обязан покрывать имя целиком: {range:?}"
    );
}

/// **Одна реализация на двоих.** Текст предупреждения у LSP и у `fmt`
/// совпадает буквально.
///
/// Сравнивается не "оба сработали", а сама строка: разъехавшиеся формулировки означали
/// бы две копии правил - то, чего фича избегала выбором Option A.
#[test]
fn lsp_message_is_identical_to_fmt_message() {
    let (_, fmt_warnings) =
        takt_lang::format::format_source_with_warnings(BAD_NAME).expect("фикстура форматируется");
    assert_eq!(fmt_warnings.len(), 1, "у fmt тоже одно: {fmt_warnings:?}");

    let lsp = style_diagnostics(BAD_NAME);
    assert_eq!(
        lsp[0].message, fmt_warnings[0].message,
        "текст предупреждения обязан быть один на обоих потребителей"
    );
    assert_eq!(
        fmt_warnings[0].code.as_deref(),
        Some("CS-001"),
        "код у fmt: {:?}",
        fmt_warnings[0]
    );
}

/// **Контр-пример.** Каноничное имя не даёт ни ошибок, ни предупреждений о
/// стиле.
#[test]
fn lsp_is_silent_on_canonical_name() {
    assert!(
        style_diagnostics(GOOD_NAME).is_empty(),
        "каноничное имя не должно давать CS-001"
    );
    let errors: Vec<_> = collect_diagnostics(GOOD_NAME)
        .into_iter()
        .filter(|d| d.severity == Some(DiagnosticSeverity::ERROR))
        .collect();
    assert!(
        errors.is_empty(),
        "фикстура обязана быть корректной: {errors:?}"
    );
}

/// **Ошибка построения дерева предупреждения о стиле не глушит**.
///
/// Ветвь `construct_stages`: тип не найден, модели нет. Канон именования от смысла не
/// зависит, поэтому `CS-001` выдаётся рядом с ошибкой - как его печатает `fmt` на том
/// же файле (тест:
/// `style_naming_fmt_tests::semantically_broken_file_is_formatted_and_warned`).
///
/// До 0227 редактор здесь молчал, и автор, правящий имя в файле, который ещё не
/// собирается, замечания не видел.
#[test]
fn lsp_shows_style_warning_next_to_construct_error() {
    let all = collect_diagnostics(SEMANTICALLY_BROKEN);
    let errors: Vec<_> = all
        .iter()
        .filter(|d| d.severity == Some(DiagnosticSeverity::ERROR))
        .collect();
    assert!(
        !errors.is_empty(),
        "фикстура обязана быть семантически некорректной: {all:?}"
    );

    let style = style_diagnostics(SEMANTICALLY_BROKEN);
    assert_eq!(
        style.len(),
        1,
        "предупреждение о стиле обязано быть рядом с ошибкой: {all:?}"
    );
    assert_eq!(style[0].severity, Some(DiagnosticSeverity::WARNING));
    assert!(
        style[0].message.contains("переменная 'BadVar'"),
        "текст предупреждения: {:?}",
        style[0].message
    );
}

/// **Ошибка проверки (`validate`) - та же ветвь возврата, то же поведение.**
///
/// Дерево построено, отвергает его `validate` (`SE-026`: запись во входной порт). Ветвь
/// возврата другая, и правка обязана покрывать её тоже: иначе предупреждение появлялось
/// бы "через раз" в зависимости от вида ошибки.
#[test]
fn lsp_shows_style_warning_next_to_validate_error() {
    let all = collect_diagnostics(VALIDATE_BROKEN);
    let has_se026 = all
        .iter()
        .any(|d| d.code == Some(NumberOrString::String("SE-026".to_string())));
    assert!(
        has_se026,
        "фикстура обязана отвергаться проверкой validate: {all:?}"
    );

    let names: Vec<&str> = style_diagnostics(VALIDATE_BROKEN)
        .iter()
        .map(|d| {
            if d.message.contains("'BadPort'") {
                "BadPort"
            } else if d.message.contains("'Unused'") {
                "Unused"
            } else {
                "?"
            }
        })
        .collect();
    assert_eq!(
        names,
        vec!["BadPort", "Unused"],
        "оба неканоничных имени обязаны быть сообщены: {all:?}"
    );
}

/// **Предупреждения о смысле по-прежнему молчат при ошибках** - объём правки
/// узкий, политика "сперва ошибки" для них не менялась.
///
/// Исправлениетура несёт неиспользуемую переменную `Unused` (`SE-036`) **и** ошибку `SE-026`.
/// Стиль показан, `SE-036` - нет: он смотрит на построенную модель, и на отвергнутой
/// его вердикт может быть ложным.
#[test]
fn lsp_still_hides_semantic_warnings_when_errors_present() {
    let all = collect_diagnostics(VALIDATE_BROKEN);
    assert!(
        !style_diagnostics(VALIDATE_BROKEN).is_empty(),
        "стиль показан: {all:?}"
    );
    assert!(
        !all.iter()
            .any(|d| d.code == Some(NumberOrString::String("SE-036".to_string()))),
        "предупреждение о неиспользуемой переменной при ошибках показываться не \
         должно: {all:?}"
    );
}

/// **Граница: разбор не удался - предупреждения о стиле нет.**
///
/// Проверка идёт по АСД, а его в этой ветви не существует: имя может быть недописано, и
/// говорить о его форме нечего. Ровно так же ведёт себя `fmt` - он отказывает, не
/// предупреждая.
#[test]
fn lsp_has_no_style_warning_when_parse_fails() {
    let all = collect_diagnostics(PARSE_BROKEN);
    assert!(
        all.iter()
            .any(|d| d.severity == Some(DiagnosticSeverity::ERROR)),
        "фикстура обязана не разбираться: {all:?}"
    );
    assert!(
        style_diagnostics(PARSE_BROKEN).is_empty(),
        "без АСД проверять форму имени нечем: {all:?}"
    );
}

/// **Паритет с `fmt` на файле, который не собирается** (главный критерий 0227).
///
/// Сравниваются не факты "оба сработали", а **множества текстов**: именно их
/// расхождение и было дефектом - у одного потребителя предупреждение было, у другого
/// нет.
#[test]
fn lsp_and_fmt_agree_on_broken_file() {
    for source in [SEMANTICALLY_BROKEN, VALIDATE_BROKEN] {
        let (_, fmt_warnings) = takt_lang::format::format_source_with_warnings(source)
            .expect("форматтер семантику не запускает");
        let fmt_texts: Vec<String> = fmt_warnings.iter().map(|w| w.message.clone()).collect();
        let lsp_texts: Vec<String> = style_diagnostics(source)
            .iter()
            .map(|d| d.message.clone())
            .collect();
        assert_eq!(
            lsp_texts, fmt_texts,
            "редактор и fmt обязаны говорить об именах одно и то же"
        );
    }
}
