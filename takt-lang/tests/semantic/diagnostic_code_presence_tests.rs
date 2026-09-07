//! У диагностики семантики есть код и позиция.
//!
//! # Что здесь проверяется
//!
//! Часть отказов строилась конверсией `From<&str> for Diagnostic`: код `None`
//! (печатался `[?]`), позиция `Location::Source(0, 0, 0)` - то есть "начало первого
//! файла". Координата была **ложной**, а не отсутствующей: сообщение указывало на
//! строку 1 любого входа.
//!
//! Из восьми достижимы оказались два - они получили свои коды:
//!
//! | Вход | До | После |
//! |---|---|---|
//! | `ref Done: mem[1] = 0;` (нет `mem`) | `[?]`, `1:1` | `SE-117`, позиция имени |
//! | `fn f(a: u8) -> u8;` (нет тела) | `[?]`, `1:1` | `SE-118`, позиция объявления |
//!
//! Остальные - внутренние инварианты: их недостижимость держат **другие** проверки
//! (`SE-034`, `SE-004`, `SE-081`), и им дан один общий код `SE-119`, называющий вид
//! нарушения словом (образец `CC-023`).
//!
//! **Класс запрещает тип, а не тест:** конверсия `From<&str>` удалена, поэтому
//! `Err("текст".into())` больше не компилируется. Тест ниже проверяет именно это - на
//! случай, если конверсию решат вернуть.

use takt_lang::diagnostics::Location;

/// Диагностика отказа построения дерева.
fn error_of(src: &str) -> takt_lang::diagnostics::Diagnostic {
    let (ast, _) = takt_lang::parse(src, 0).expect("разбор");
    takt_lang::semantic::tree::construct_model(&ast, None, &[])
        .expect_err("вход обязан отвергаться")
}

/// **T1.** Индексация не-массива: код `SE-117` и позиция имени.
#[test]
fn subscript_of_non_array_has_code_and_position() {
    let src = "var n: u8 := 0;\n\
               \n\
               start Run {\n\
               \x20   always { n := n + 1; }\n\
               \x20   ref Done: mem[1] = 0;\n\
               }\n\
               state Done;\n";
    let err = error_of(src);
    assert_eq!(err.code.as_deref(), Some("SE-117"), "код диагностики");
    let Location::Source(_, start, _) = err.loc else {
        panic!("позиция обязана быть в исходнике: {:?}", err.loc);
    };
    assert!(
        start > 0,
        "позиция `Source(0, …)` — это «начало первого файла», а не место ошибки"
    );
    assert!(
        err.message.contains("'mem'"),
        "сообщение обязано называть имя:\n{}",
        err.message
    );
}

/// **T2.** Локальная функция без тела: код `SE-118` и позиция объявления.
#[test]
fn local_function_without_body_has_code_and_position() {
    let src = "var n: u8 := 0;\n\
               \n\
               fn f(a: u8) -> u8;\n\
               \n\
               start Run {\n\
               \x20   always { n := f(1); }\n\
               \x20   ref Run: n < 3;\n\
               }\n";
    let err = error_of(src);
    assert_eq!(err.code.as_deref(), Some("SE-118"), "код диагностики");
    let Location::Source(_, start, _) = err.loc else {
        panic!("позиция обязана быть в исходнике: {:?}", err.loc);
    };
    assert!(start > 0, "позиция указывает в начало первого файла");
}

/// **T3. Устройство: конверсии `From<&str> for Diagnostic` больше нет.**
///
/// Пока она существовала, безликую диагностику можно было построить случайно -
/// `Err("текст".into())` компилировалось. Теперь это запрещает **тип**, и тест
/// смотрит на исходник библиотеки диагностик.
#[test]
fn no_from_str_conversion_for_diagnostic() {
    let source = include_str!("../../src/diagnostics/mod.rs");
    assert!(
        !source.contains("impl From<&str> for Diagnostic"),
        "конверсия `From<&str>` строит диагностику без кода и с позицией \
         «начало первого файла» — она удалена намеренно (фича 0276)"
    );
}

/// **T4. Устройство: в семантике не осталось `Err(\"...\".into())`.**
///
/// Второй слой к T3: конверсию могут вернуть под другим именем, а этот тест смотрит на
/// места употребления и падает **списком**.
#[test]
fn semantic_has_no_faceless_diagnostics() {
    let mut offenders = Vec::new();
    let root = std::path::Path::new("src/semantic");
    let mut stack = vec![root.to_path_buf()];
    while let Some(dir) = stack.pop() {
        let Ok(entries) = std::fs::read_dir(&dir) else {
            continue;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                stack.push(path);
                continue;
            }
            if path.extension().and_then(|e| e.to_str()) != Some("rs") {
                continue;
            }
            let Ok(text) = std::fs::read_to_string(&path) else {
                continue;
            };
            for (i, line) in text.lines().enumerate() {
                let t = line.trim();
                if t.starts_with("Err(\"") && t.contains(".into()") {
                    offenders.push(format!("{}:{}: {t}", path.display(), i + 1));
                }
            }
        }
    }
    assert!(
        offenders.is_empty(),
        "диагностика без кода и позиции — места:\n{offenders:#?}"
    );
}
