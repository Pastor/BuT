//! Вызов функции в именованном условии.
//!
//! # Что тестытся
//!
//! - перебор мест, где вызов стоит в условии: все переводятся;
//! - копии условия - в теле блока, в теле функции, во вложенном условии, в
//!   под-модели: перепривязка обязана дойти до каждой;
//! - контроль: неизвестное имя по-прежнему отвергается `SE-004`.

use std::path::PathBuf;

use takt_lang::generator::GenerateOptions;

/// Шапка проб: функция, читающая переменную модели.
const HEAD: &str = "out sum: u8 at 0x2000;\nvar ticks: u8 := 0;\n\
     fn hot() -> u8 { return ticks + 1; }\n";

/// Места, где вызов стоит в условии. Первое - предмет фичи, прочие - контроль: они
/// работали всегда, и правка не вправе их задеть.
const SITES: &[(&str, &str)] = &[
    (
        "именованное условие",
        "cond warm = hot() > 3;\n\
         start Run { always { ticks := ticks + 1; sum := ticks; } ref Hot: warm; ref Run; }\n\
         state Hot { always { sum := 9; } ref Hot; }",
    ),
    (
        "именованное условие через второе",
        "cond warm = hot() > 3;\ncond both = warm;\n\
         start Run { always { ticks := ticks + 1; sum := ticks; } ref Hot: both; ref Run; }\n\
         state Hot { always { sum := 9; } ref Hot; }",
    ),
    (
        "именованное условие в теле блока",
        "cond warm = hot() > 3;\n\
         start Run { always { ticks := ticks + 1; if warm { sum := 9; } else { sum := ticks; } } \
         ref Run; }",
    ),
    (
        "именованное условие в теле ФУНКЦИИ",
        "cond warm = hot() > 3;\n\
         fn outer(k: u8) -> u8 { if warm { return k + 1; } return k; }\n\
         start Run { always { ticks := ticks + 1; sum := outer(ticks); } ref Run; }",
    ),
    (
        "условие ребра",
        "start Run { always { ticks := ticks + 1; sum := ticks; } ref Hot: hot() > 3; ref Run; }\n\
         state Hot { always { sum := 9; } ref Hot; }",
    ),
    (
        "условие if в теле",
        "start Run { always { ticks := ticks + 1; \
         if hot() > 3 { sum := 9; } else { sum := ticks; } } ref Run; }",
    ),
    (
        "охранная формула",
        "start Run { : hot() < 200; always { ticks := ticks + 1; sum := ticks; } ref Run; }",
    ),
    (
        "инвариант",
        "invariant Safe = hot() < 200;\n\
         start Run { always { ticks := ticks + 1; sum := ticks; } ref Run; }",
    ),
];

fn out_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0503_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог вывода");
    dir
}

/// Код отказа компиляции целью `c` (`None` - приняла).
fn refusal(tag: &str, source: &str) -> Option<String> {
    let dir = out_dir(tag);
    let result = takt_lang::compile_to_c(
        tag,
        source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    );
    let _ = std::fs::remove_dir_all(&dir);
    result.err().and_then(|d| d.code)
}

/// Каждое место, где вызов стоит в условии, переводится.
///
/// Тест падает **списком**: копий условия несколько, и знать надо все непройденные
/// сразу.
#[test]
fn every_condition_site_translates() {
    let mut failed = Vec::new();
    for (index, (name, body)) in SITES.iter().enumerate() {
        let tag = format!("cr0503{index}");
        if let Some(code) = refusal(&tag, &format!("{HEAD}{body}\n")) {
            failed.push(format!("{name} → {code}"));
        }
    }
    assert!(
        failed.is_empty(),
        "вызов функции в условии обязан переводиться: {}",
        failed.join("; ")
    );
}

/// Условие под-Модели перепривязывается так же.
#[test]
fn submodel_condition_translates() {
    let source = "out sum: u8 at 0x2000;\n\
         model Worker {\n    var step: u8 := 0;\n\
         \x20   fn hot() -> u8 { return step + 1; }\n\
         \x20   cond warm = hot() > 2;\n\
         \x20   start Go { always { step := step + 1; sum := step; } ref Done: warm; ref Go; }\n\
         \x20   state Done { always { sum := 9; } ref Done; }\n}\n\
         start Main = Worker;\n";
    assert_eq!(
        refusal("cr0503s", source),
        None,
        "перепривязка обязана обойти вложенные модели"
    );
}

/// **Контроль:** неизвестное имя функции в условии по-прежнему отвергается.
///
/// Без него правка читалась бы как "принимать любой вызов": перепривязка не вправе
/// подменять собой проверку существования.
#[test]
fn unknown_function_is_still_refused() {
    let source = "out sum: u8 at 0x2000;\nvar ticks: u8 := 0;\n\
         cond warm = missing() > 3;\n\
         start Run { always { ticks := ticks + 1; sum := ticks; } ref Hot: warm; ref Run; }\n\
         state Hot { always { sum := 9; } ref Hot; }\n";
    assert_eq!(
        refusal("cr0503u", source).as_deref(),
        Some("SE-004"),
        "имя, которого нет, обязано отвергаться"
    );
}
