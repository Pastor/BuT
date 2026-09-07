//! Все ошибки за один прогон.
//!
//! Проверяется не "ошибка нашлась", а **сколько** их дошло до пользователя, в каком
//! порядке и не потерялись ли по дороге.

use takt_lang::collect_compile_diagnostics;
use takt_lang::diagnostics::{Diagnostic, Location, normalize};

/// Читает фикстуру каталога `tests/data/diagnostics0130/`.
fn fixture(name: &str) -> (String, String) {
    let path = format!("tests/data/diagnostics0130/{name}");
    let source =
        std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("не прочитать {path}: {e}"));
    (path, source)
}

/// Смещение начала диагностики (для проверок порядка).
fn start_of(diagnostic: &Diagnostic) -> u32 {
    match diagnostic.loc {
        Location::Source(_, start, _) => start,
        _ => u32::MAX,
    }
}

/// A1: файл с тремя ошибками разбора даёт **три** сообщения, а не первое.
#[test]
fn all_parser_errors_are_reported() {
    let (path, source) = fixture("three_syntax_errors.takt");
    let diagnostics = collect_compile_diagnostics(&path, &source, &[], false);
    assert_eq!(
        diagnostics.len(),
        3,
        "ожидались три ошибки разбора, получено {}: {:#?}",
        diagnostics.len(),
        diagnostics
            .iter()
            .map(|d| (&d.code, &d.message))
            .collect::<Vec<_>>()
    );
    for diagnostic in &diagnostics {
        assert!(
            matches!(diagnostic.loc, Location::Source(_, _, _)),
            "у каждой ошибки разбора есть позиция: {diagnostic:?}"
        );
        assert_eq!(
            diagnostic.file.as_deref(),
            Some(path.as_str()),
            "путь файла проставлен (фича 0053)"
        );
    }
}

/// A5: сообщения идут по возрастанию позиции в тексте.
///
/// Обход модели идёт по `BTreeMap`, то есть по алфавиту имён: без сортировки на выдаче
/// пользователь получил бы ошибки вразнобой.
#[test]
fn diagnostics_are_ordered_by_position() {
    let (path, source) = fixture("three_syntax_errors.takt");
    let diagnostics = collect_compile_diagnostics(&path, &source, &[], false);
    let starts: Vec<u32> = diagnostics.iter().map(start_of).collect();
    let mut sorted = starts.clone();
    sorted.sort_unstable();
    assert_eq!(starts, sorted, "порядок обязан быть текстовым: {starts:?}");
}

/// A5: два прогона дают побайтно один и тот же список.
#[test]
fn repeated_runs_agree() {
    let (path, source) = fixture("three_syntax_errors.takt");
    let first = collect_compile_diagnostics(&path, &source, &[], false);
    let second = collect_compile_diagnostics(&path, &source, &[], false);
    assert_eq!(
        format!("{first:?}"),
        format!("{second:?}"),
        "выдача обязана быть воспроизводимой"
    );
}

/// A6: точный повтор не доходит до пользователя дважды.
#[test]
fn exact_duplicates_are_collapsed() {
    let (path, source) = fixture("three_syntax_errors.takt");
    let diagnostics = collect_compile_diagnostics(&path, &source, &[], false);
    let doubled: Vec<Diagnostic> = diagnostics
        .iter()
        .cloned()
        .chain(diagnostics.iter().cloned())
        .collect();
    assert_eq!(
        normalize(doubled).len(),
        diagnostics.len(),
        "дубликаты обязаны схлопываться"
    );
}

/// A3: проверки высказываются все, а не только первая.
///
/// В фикстуре две независимые ошибки: два начальных состояния (`SE-011`) и значение `5`
/// у переменной типа `bit` (`SE-035`).
#[test]
fn all_validation_errors_are_reported() {
    let (path, source) = fixture("two_validate_errors.takt");
    let diagnostics = collect_compile_diagnostics(&path, &source, &[], false);
    let codes: Vec<&str> = diagnostics
        .iter()
        .filter_map(|d| d.code.as_deref())
        .collect();
    assert!(
        codes.contains(&"SE-011") && codes.contains(&"SE-035"),
        "ожидались обе ошибки проверок, получено {codes:?}"
    );
}

/// A4: ошибка вложенной модели попадает в тот же список.
#[test]
fn nested_model_errors_are_collected() {
    let (path, source) = fixture("nested_error.takt");
    let diagnostics = collect_compile_diagnostics(&path, &source, &[], false);
    let messages: Vec<&str> = diagnostics.iter().map(|d| d.message.as_str()).collect();
    assert_eq!(
        diagnostics.len(),
        2,
        "ожидались ошибки внешней и вложенной моделей: {messages:?}"
    );
    assert!(
        messages.iter().any(|m| m.contains("'top'"))
            && messages.iter().any(|m| m.contains("'deep'")),
        "обе модели обязаны высказаться: {messages:?}"
    );
    let starts: Vec<u32> = diagnostics.iter().map(start_of).collect();
    let mut sorted = starts.clone();
    sorted.sort_unstable();
    assert_eq!(starts, sorted, "порядок текстовый: {starts:?}");
}

/// A7: диагностика несёт **реальную** позицию, а не начало файла.
///
/// Пока сообщение было одно, координата `1:1` терпелась: ошибка в файле одна, найдётся.
/// В пачке сообщений позиция становится обязательной - иначе все они указывают в первую
/// строку и искать приходится вручную.
#[test]
fn diagnostics_point_at_real_positions() {
    let (path, source) = fixture("unknown_identifier.takt");
    let diagnostics = collect_compile_diagnostics(&path, &source, &[], false);
    assert_eq!(diagnostics.len(), 1, "{diagnostics:#?}");
    let Location::Source(_, start, end) = diagnostics[0].loc else {
        panic!("ожидалась позиция в исходном тексте: {:?}", diagnostics[0]);
    };
    assert!(
        start > 0,
        "позиция обязана указывать на имя, а не на начало файла"
    );
    assert_eq!(
        &source[start as usize..end as usize],
        "unknown_name",
        "диапазон обязан покрывать сам идентификатор"
    );
}

/// Корректный файл даёт пустой список - "нет ошибок" выражается пустотой, а не
/// отсутствием вызова.
#[test]
fn valid_source_yields_no_diagnostics() {
    let (path, source) = fixture("valid.takt");
    let diagnostics = collect_compile_diagnostics(&path, &source, &[], false);
    assert!(
        diagnostics.is_empty(),
        "корректная модель не должна давать диагностик: {diagnostics:#?}"
    );
}

/// A2: языковой сервер и компилятор сообщают об одних и тех же ошибках.
#[cfg(feature = "lsp")]
#[test]
fn cli_and_language_server_agree() {
    let (path, source) = fixture("three_syntax_errors.takt");
    let compiler = collect_compile_diagnostics(&path, &source, &[], false);
    let server = takt_lang::lsp::collect_diagnostics_at(&path, &source, &[]);

    let compiler_codes: Vec<String> = compiler
        .iter()
        .map(|d| d.code.clone().unwrap_or_default())
        .collect();
    let server_codes: Vec<String> = server
        .iter()
        .filter_map(|d| match &d.code {
            Some(lsp_types::NumberOrString::String(code)) => Some(code.clone()),
            _ => None,
        })
        .collect();
    assert_eq!(
        compiler_codes, server_codes,
        "состав ошибок обязан совпадать: компилятор {compiler_codes:?}, сервер {server_codes:?}"
    );
}
