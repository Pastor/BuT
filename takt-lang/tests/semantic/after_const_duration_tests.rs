//! Константная выдержка `after DWELL` / `after (BASE + 30s)`.
//!
//! Проверяется **тождественность**: вывод каждой цели обязан совпадать байт-в-байт с
//! выводом того же исходника, где вместо имени (или выражения) стоит литерал. Это
//! сильнее проверки "компилируется": литеральная форма уже покрыта потактовыми сверками
//! (`conformance_{c,rust,st,sv}_*_time_tests`), поэтому равенство текста переносит на
//! новую форму и их доказательства - а вот "собралось" не отличило бы верную трансляцию
//! от молча неверной.
//!
//! Вычисление выражения, диагностика `SE-072` и цепочка констант покрыты юнит-тестами
//! слоя (`takt_lang::semantic::after_const`); здесь - границы: цели, форматтер и
//! редакторский слой.

use takt_lang::GenerateOptions;
use takt_lang::{compile_to_c, compile_to_rust, compile_to_st, compile_to_sv};

/// Вентилятор с выбегом: выдержка задана **именем** константы.
const NAMED: &str = r#"
model Fan {
    const OVERRUN := 3m;
    in light: bit;
    out motor: bit;
    start Idle {
        enter { motor := 0; }
        ref Working: light = 1;
    }
    state Working {
        enter { motor := 1; }
        ref Overrun: light = 0;
    }
    state Overrun {
        ref Working: light = 1;
        ref Idle: after OVERRUN;
    }
}
start Entry = Fan;
"#;

/// Тот же вентилятор с **литералом** - эталон сравнения.
const LITERAL: &str = r#"
model Fan {
    in light: bit;
    out motor: bit;
    start Idle {
        enter { motor := 0; }
        ref Working: light = 1;
    }
    state Working {
        enter { motor := 1; }
        ref Overrun: light = 0;
    }
    state Overrun {
        ref Working: light = 1;
        ref Idle: after 3m;
    }
}
start Entry = Fan;
"#;

fn tmp(tag: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0143_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    dir
}

/// Порождает обе формы в отдельные каталоги под **одним** именем входа и возвращает
/// содержимое файла `<имя>.<ext>` для каждой.
///
/// Имя входа обязано совпадать: у цели `c` имя корневой модели берётся из имени файла,
/// поэтому разные имена дали бы разный вывод по причине, к фиче не относящейся.
/// Сигнатура порождающей функции цели (`compile_to_c` и родственные).
type Generate =
    fn(
        &str,
        &str,
        &str,
        &[String],
        &GenerateOptions,
    ) -> Result<Vec<takt_lang::diagnostics::Diagnostic>, takt_lang::diagnostics::Diagnostic>;

fn both(tag: &str, ext: &str, generate: Generate) -> (String, String) {
    let read = |dir: &std::path::Path| {
        std::fs::read_to_string(dir.join(format!("fan.{ext}"))).expect("порождённый файл")
    };
    let named_dir = tmp(&format!("{tag}_named"));
    let literal_dir = tmp(&format!("{tag}_literal"));
    generate(
        "fan",
        NAMED,
        named_dir.to_str().unwrap(),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение из именной формы");
    generate(
        "fan",
        LITERAL,
        literal_dir.to_str().unwrap(),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение из литеральной формы");
    (read(&named_dir), read(&literal_dir))
}

/// A1: цель `c` - вывод тождествен литеральной форме.
#[test]
fn c_output_identical_to_literal() {
    let (named, literal) = both("c", "c", compile_to_c);
    assert_eq!(
        named, literal,
        "цель c: `after OVERRUN` обязана дать тот же код, что `after 3m`"
    );
    // Тест самого теста: выдержка в выводе действительно есть - иначе тест сравнивал
    // бы два кода без выдержки и был бы тождественно зелёным. Модель без объявления
    // `clock` идёт профилем "часы", где выдержка меряется меткой времени, а не
    // счётчиком тактов, - отсюда `180000` (миллисекунды трёх минут).
    assert!(
        named.contains("takt_entry_ms") && named.contains("180000"),
        "в выводе нет выдержки — сравнение бессмысленно:\n{named}"
    );
}

/// A1: цель `rust` - вывод тождествен литеральной форме.
#[test]
fn rust_output_identical_to_literal() {
    let (named, literal) = both("rust", "rs", compile_to_rust);
    assert_eq!(named, literal, "цель rust: вывод расходится с литеральным");
    assert!(
        named.contains("180000"),
        "в выводе нет выдержки — сравнение бессмысленно:\n{named}"
    );
}

/// A1: цель `st` - вывод тождествен литеральной форме.
#[test]
fn st_output_identical_to_literal() {
    let (named, literal) = both("st", "st", compile_to_st);
    assert_eq!(named, literal, "цель st: вывод расходится с литеральным");
}

/// A1: цель `sv` - вывод тождествен литеральной форме.
#[test]
fn sv_output_identical_to_literal() {
    let (named, literal) = both("sv", "sv", compile_to_sv);
    assert_eq!(named, literal, "цель sv: вывод расходится с литеральным");
}

/// Выдержка **выражением** (`after ((BASE + TRIM) - 30s)`) тоже сводится к литералу:
/// цель `c` даёт тот же код, что для `after 3m`.
///
/// Требование 2026-07-29 (ревизия объёма): `after` принимает константное выражение, а
/// не только имя.
#[test]
fn expression_form_output_identical_to_literal() {
    const EXPR: &str = r#"
model Fan {
    const BASE := 2m;
    const TRIM := 30s;
    in light: bit;
    out motor: bit;
    start Idle {
        enter { motor := 0; }
        ref Working: light = 1;
    }
    state Working {
        enter { motor := 1; }
        ref Overrun: light = 0;
    }
    state Overrun {
        ref Working: light = 1;
        ref Idle: after ((BASE + TRIM) + 30s);
    }
}
start Entry = Fan;
"#;
    let expr_dir = tmp("c_expr");
    let literal_dir = tmp("c_expr_literal");
    compile_to_c(
        "fan",
        EXPR,
        expr_dir.to_str().unwrap(),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение из выражения");
    compile_to_c(
        "fan",
        LITERAL,
        literal_dir.to_str().unwrap(),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение из литерала");
    let expr = std::fs::read_to_string(expr_dir.join("fan.c")).expect(".c");
    let literal = std::fs::read_to_string(literal_dir.join("fan.c")).expect(".c");
    assert_eq!(
        expr, literal,
        "2m + 30s + 30s обязаны дать тот же код, что 3m"
    );
}

/// A5: форматтер печатает именную выдержку и круговой рейс устойчив.
///
/// Печать нового узла АСД - обязанность форматтера (иначе `format_source`
/// **отказывает**, а не печатает приблизительно); проверяется и то, что имя
/// сохранено как написано, и что повторная печать ничего не меняет.
#[test]
fn formatter_prints_named_dwell_and_is_stable() {
    let once = takt_lang::format::format_source(NAMED).expect("печать именной формы");
    assert!(
        once.contains("after OVERRUN"),
        "форматтер потерял имя выдержки:\n{once}"
    );
    let twice = takt_lang::format::format_source(&once).expect("повторная печать");
    assert_eq!(once, twice, "круговой рейс форматтера неустойчив");

    // Скобочная форма обязана вернуться со скобками: без них `after A + 1s` разобралось
    // бы как `(after A) + 1s`, то есть печать испортила бы программу.
    const EXPR_SRC: &str = r#"model M {
    const BASE := 2m;
    start Wait { ref Done: after ((BASE + 30s) - 15s); }
    state Done;
}
"#;
    let printed = takt_lang::format::format_source(EXPR_SRC).expect("печать выражения");
    assert!(
        printed.contains("after ((BASE + 30s) - 15s)"),
        "скобки выражения выдержки потеряны:\n{printed}"
    );
    let again = takt_lang::format::format_source(&printed).expect("повторная печать выражения");
    assert_eq!(printed, again, "круговой рейс на выражении неустойчив");
}

/// A6: имя внутри `after` - использование константы, а не безымянный литерал.
///
/// Без этого переименование константы испортило бы исходник: выдержка осталась бы со
/// старым именем и перестала компилироваться.
#[cfg(feature = "lsp")]
mod editor {
    use lsp_types::Position;
    use takt_lang::lsp::{references_at, rename_at};

    /// Позиция курсора на `n`-м (с нуля) вхождении подстроки.
    fn cursor_on_nth(source: &str, needle: &str, n: usize) -> Position {
        let offset = source
            .match_indices(needle)
            .nth(n)
            .unwrap_or_else(|| panic!("нет {}-го вхождения `{needle}`", n + 1))
            .0;
        let head = &source[..offset];
        let line = head.matches('\n').count() as u32;
        let col = head.rsplit('\n').next().map_or(0, |l| l.chars().count()) as u32;
        Position::new(line, col)
    }

    const SRC: &str = r#"model Fan {
    const OVERRUN := 3m;
    start Idle { ref Done: after OVERRUN; }
    state Done;
}
"#;

    /// `references` от объявления константы находит и вхождение внутри `after`.
    #[test]
    fn references_include_name_inside_after() {
        let position = cursor_on_nth(SRC, "OVERRUN", 0);
        let found = references_at(SRC, position, true).expect("вхождения константы");
        assert_eq!(
            found.len(),
            2,
            "объявление + вхождение в `after`, найдено {}: {found:?}",
            found.len()
        );
        let after_line = SRC
            .lines()
            .position(|l| l.contains("after OVERRUN"))
            .expect("строка с выдержкой") as u32;
        assert!(
            found.iter().any(|r| r.start.line == after_line),
            "вхождение внутри `after` не найдено: {found:?}"
        );
    }

    /// `rename` правит и объявление, и имя внутри `after`.
    #[test]
    fn rename_updates_name_inside_after() {
        let position = cursor_on_nth(SRC, "OVERRUN", 0);
        let edits = rename_at(SRC, position, "DWELL").expect("переименование");
        assert_eq!(
            edits.len(),
            2,
            "правки: объявление + `after`, получено {}: {edits:?}",
            edits.len()
        );
        assert!(
            edits.iter().all(|e| e.new_text == "DWELL"),
            "правки должны подставлять новое имя: {edits:?}"
        );
    }
}
