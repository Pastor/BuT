//! Тесты форматтера: идемпотентность и семантическая нейтральность.
//!
//! # Что здесь важнее отдельных примеров
//!
//! Форматтер печатает **всё** дерево, поэтому его нельзя проверить парой исправлениетур: любой
//! непокрытый узел молча испортил бы исходник пользователя. Поэтому основная проверка -
//! прогон по **всему корпусу** `.takt` репозитория (`examples/`,
//! `takt-lang/tests/data/`, `takt-sim/tests/data/`) с двумя инвариантами:
//!
//! - **A1, идемпотентность:** `fmt(fmt(x)) == fmt(x)`;
//! - **A3, семантическая нейтральность:** `parse(fmt(x))` структурно равен
//!   `parse(x)` - форматтер меняет раскладку, но не смысл.
//!
//! Тест печатает **сводку покрытия**: сколько файлов корпуса форматируется, и
//! **падает при появлении нового непокрытого узла**. Так "не отформатировано" не
//! превращается в тихую норму.
//!
//! **Каталоги перечислены явно, и пропавший каталог валит тест**.
//! Прежде список звал каталоги данных под старыми именами крейтов,
//! переименованных в `takt-lang/` и `takt-sim/` ещё, - а помощник
//! обхода при ошибке чтения **молча возвращался**. Тест оставался зелёным,
//! рапортуя о "всём корпусе", но видел один каталог из трёх: 158 файлов вместо
//! 308. Цена молчания измерена - исправление []: в невидимой половине жила
//! запись `model M = A | B { ... }`, у которой форматтер **терял реализацию**.
//!
//! []: https://github.com/Pastor/Takt/blob/v2/docs/fixes/-fmt-drops-model-implement.md

use std::path::{Path, PathBuf};
use takt_lang::format::{FormatError, format_source};

/// Собирает все `.takt` репозитория.
fn corpus() -> Vec<PathBuf> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("корень репозитория")
        .to_path_buf();
    let mut files = Vec::new();
    for dir in ["examples", "takt-lang/tests/data", "takt-sim/tests/data"] {
        collect(&root.join(dir), &mut files);
    }
    files.sort();
    files
}

/// Обходит каталог рекурсивно, собирая `.takt`.
///
/// **Пропавший каталог - отказ, а не пропуск**: именно молчаливый возврат позволил
/// обходу съёжиться с трёх каталогов до одного и не сообщить об этом. Ошибка чтения
/// **вложенного** каталога - тот же отказ: половина корпуса, исчезнувшая тихо, хуже
/// упавшего теста.
fn collect(dir: &Path, out: &mut Vec<PathBuf>) {
    let entries = std::fs::read_dir(dir)
        .unwrap_or_else(|e| panic!("каталог корпуса {} не читается: {e}", dir.display()));
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect(&path, out);
        } else if path.extension().is_some_and(|e| e == "takt") {
            out.push(path);
        }
    }
}

/// Структурное сравнение АСД без учёта `Location`.
///
/// `Location` намеренно игнорируется: форматтер **обязан** сдвигать позиции - в этом и
/// состоит его работа. Сравнивается всё остальное.
fn ast_eq_ignoring_locations(
    a: &takt_lang::parser::ast::Model,
    b: &takt_lang::parser::ast::Model,
) -> bool {
    // `Debug`-представление содержит `Location`, поэтому вырезаем их текстом: это
    // дёшево и не требует отдельного обхода дерева.
    fn strip(model: &takt_lang::parser::ast::Model) -> String {
        let text = format!("{model:?}");
        let mut out = String::with_capacity(text.len());
        let mut rest = text.as_str();
        while let Some(i) = rest.find("Source(") {
            out.push_str(&rest[..i]);
            let Some(close) = rest[i..].find(')') else {
                break;
            };
            out.push_str("Source(…)");
            rest = &rest[i + close + 1..];
        }
        out.push_str(rest);
        out
    }
    strip(a) == strip(b)
}

#[test]
fn a1_a3_corpus_report() {
    let files = corpus();
    assert!(!files.is_empty(), "корпус .takt не найден");

    let mut formatted = 0usize;
    let mut unsupported: Vec<(String, String)> = Vec::new();
    let mut idempotency_failures: Vec<String> = Vec::new();
    let mut semantic_failures: Vec<String> = Vec::new();
    let mut parse_failures = 0usize;

    for path in &files {
        let name = path
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("?")
            .to_string();
        let Ok(source) = std::fs::read_to_string(path) else {
            continue;
        };
        // Невалидные исправлениетуры (`data/semantic/invalid/`) могут не разбираться - это не
        // дефект форматтера.
        if takt_lang::parse(&source, 0).is_err() {
            parse_failures += 1;
            continue;
        }

        match format_source(&source) {
            // отказ несёт диагностику, а не строку. Вид узла берём из текста - он там в
            // кавычках, и именно он интересен проверке ниже.
            Err(FormatError::Unsupported(diagnostic)) => {
                let kind = diagnostic
                    .message
                    .split('\'')
                    .nth(1)
                    .unwrap_or(&diagnostic.message)
                    .to_string();
                unsupported.push((name, kind));
            }
            Err(FormatError::Parse(_)) => parse_failures += 1,
            Ok(once) => {
                formatted += 1;
                // A1: идемпотентность.
                match format_source(&once) {
                    Ok(twice) if twice == once => {}
                    Ok(_) => idempotency_failures.push(name.clone()),
                    Err(e) => idempotency_failures.push(format!("{name}: повторный прогон: {e}")),
                }
                // A3: семантическая нейтральность.
                match (takt_lang::parse(&source, 0), takt_lang::parse(&once, 0)) {
                    (Ok((before, _)), Ok((after, _))) => {
                        if !ast_eq_ignoring_locations(&before, &after) {
                            semantic_failures.push(name.clone());
                        }
                    }
                    _ => semantic_failures.push(format!("{name}: результат не разбирается")),
                }
            }
        }
    }

    // Сводка покрытия - печатается всегда: это метрика прогресса.
    let total = files.len();
    eprintln!("\n── Форматтер: покрытие корпуса ──");
    eprintln!("  всего .takt:            {total}");
    eprintln!("  отформатировано:       {formatted}");
    eprintln!("  не разбирается:        {parse_failures} (невалидные фикстуры — норма)");
    eprintln!("  узел не поддержан:     {}", unsupported.len());
    let mut kinds: Vec<&str> = unsupported.iter().map(|(_, k)| k.as_str()).collect();
    kinds.sort_unstable();
    kinds.dedup();
    for kind in kinds {
        let count = unsupported.iter().filter(|(_, k)| k == kind).count();
        eprintln!("      {count:3} × {kind}");
    }

    assert!(
        idempotency_failures.is_empty(),
        "A1 (идемпотентность) нарушена: {idempotency_failures:?}"
    );
    assert!(
        semantic_failures.is_empty(),
        "A3 (семантическая нейтральность) нарушена — форматтер изменил смысл: {semantic_failures:?}"
    );
    // Проверка: единственными причинами "не отформатировано" могут быть известные и
    // задокументированные пробелы. Любой новый непокрытый узел валит тест - это и есть
    // защита от тихой потери куска исходника.
    //
    // Список **пуст** - замер (2026-08-05): на 279 файлах корпуса непокрытых узлов
    // ноль. Прежние две записи ("комментарии (печать - )" и `InlineFormula`) обещали
    // пробелы, которых давно нет: обе печати сделаны, а запись о них осталась.
    const KNOWN_GAPS: &[&str] = &[];

    let unexpected: Vec<&(String, String)> = unsupported
        .iter()
        .filter(|(_, kind)| !KNOWN_GAPS.contains(&kind.as_str()))
        .collect();
    assert!(
        unexpected.is_empty(),
        "появился НЕизвестный непокрытый узел (тихая потеря исходника недопустима): {unexpected:?}"
    );

    // Ратчет: запись, которой на корпусе больше не соответствует ни один отказ, обязана
    // быть удалена. Иначе список копит обещания о несуществующих пробелах и потихоньку
    // расширяет допуск: следующий настоящий пробел, случайно совпав именем со старой
    // записью, пройдёт молча. Список о своей ненужности сам не сообщает - поэтому
    // спрашивает тест.
    let stale: Vec<&str> = KNOWN_GAPS
        .iter()
        .copied()
        .filter(|kind| !unsupported.iter().any(|(_, k)| k == kind))
        .collect();
    assert!(
        stale.is_empty(),
        "KNOWN_GAPS обещает пробелы, которых на корпусе нет — удалите записи: {stale:?}"
    );
}

/// Реализация модели с собственным телом печатается целиком.
///
/// `model M = A | B { ... }` - форма. Печатник выбирал между "короткой" записью (`model
/// M = выражение;`, когда тела нет) и "телом" (`model M { ... }`), и во второй ветке
/// **терял** реализацию: композиция исчезала, модель становилась пустым автоматом.
/// Форматтер молча менял смысл программы - ровно то, ради предотвращения чего он
/// отказывается печатать непокрытые узлы.
///
/// Проверка стоит **отдельно** от корпусной: та поймала дефект лишь потому, что обход
/// дошёл до исправлениетур `takt-sim/tests/data/`. Инвариант не должен зависеть от того, лежит
/// ли в корпусе подходящий файл.
#[test]
fn model_implement_with_body_keeps_implementation() {
    let source = "model A {\n    start C1;\n}\n\nmodel B {\n    start C2;\n}\n\n\
                  model Root = A | B {\n    var n: u8 := 0;\n}\n\nstart Entry = Root;\n";
    let out = format_source(source).expect("фикстура форматируется");

    assert!(
        out.contains("model Root = A | B {"),
        "реализация модели обязана уцелеть при печати тела:\n{out}"
    );
    // Пустое тело - тоже тело: `model Root = A { }` печатается с фигурными скобками.
    // Круговой рейс это и ловит.
    let empty_body = "model A {\n    start C1;\n}\n\nmodel Root = A { }\n\nstart Entry = Root;\n";
    let printed = format_source(empty_body).expect("форма с пустым телом форматируется");
    assert!(
        printed.contains("model Root = A {"),
        "реализация с пустым телом печатается со скобками:\n{printed}"
    );
    format_source(&printed).expect("напечатанное обязано разбираться (круговой рейс)");
}

#[test]
fn a3_semantics_preserved_on_operators() {
    // Инвариант: `=` - сравнение, `:=` - присваивание. Подмена одного другим при печати
    // изменила бы смысл программы молча.
    let source = "var x: u8 := 1;\nstate A {\n    ref B: x = 1;\n}\n";
    let out = format_source(source).unwrap();
    assert!(out.contains("x: u8 := 1"), "присваивание: {out}");
    assert!(out.contains("ref B: x = 1"), "сравнение в условии: {out}");
}

#[test]
fn idempotent_on_messy_input() {
    let messy = "start    S ;\n\n\n\nstate   A{ref B:x>1;}";
    let once = format_source(messy).unwrap();
    let twice = format_source(&once).unwrap();
    assert_eq!(once, twice, "fmt(fmt(x)) должно равняться fmt(x)");
}

#[test]
fn a2_comments_of_all_three_kinds_survive() {
    // R2/A2: ни один комментарий не теряется. Три вида: `//`, `///`, `/* */`, в ведущей
    // и хвостовой позиции.
    let source = r#"/// документация модели
// обычный ведущий
var x: u8 := 0; // хвостовой
/* блочный */
start S;
"#;
    let out = format_source(source).unwrap();
    for expected in [
        "/// документация модели",
        "// обычный ведущий",
        "// хвостовой",
        "/* блочный */",
    ] {
        assert!(
            out.contains(expected),
            "потерян комментарий {expected:?}:\n{out}"
        );
    }
    // Хвостовой обязан остаться на строке своего узла, а не уехать отдельно.
    assert!(
        out.contains("var x: u8 := 0; // хвостовой"),
        "хвостовой комментарий оторвался от узла:\n{out}"
    );
}

#[test]
fn a2_comments_are_idempotent() {
    // Комментарии - главный риск идемпотентности: при повторном прогоне они не должны
    // ни дублироваться, ни смещаться.
    let source = "// ведущий\nvar x: u8 := 0; // хвостовой\nstart S;\n";
    let once = format_source(source).unwrap();
    let twice = format_source(&once).unwrap();
    assert_eq!(once, twice);
    assert_eq!(
        once.matches("// ведущий").count(),
        1,
        "дубль комментария:\n{once}"
    );
}

// -- Контракт CLI `taktc fmt` --------------------

/// Прогоняет настоящий бинарник `taktc` - тот же путь, что у пользователя.
fn taktc(args: &[&str]) -> std::process::Output {
    std::process::Command::new(env!("CARGO_BIN_EXE_taktc"))
        .args(args)
        .output()
        .expect("запуск taktc")
}

#[test]
fn a4_fmt_check_exit_codes() {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join("takt_fmt_0024_03");
    std::fs::create_dir_all(&dir).unwrap();

    let messy = dir.join("messy.takt");
    std::fs::write(&messy, "var   x :u8:=0;\nstart   S ;\n").unwrap();
    let out = taktc(&["fmt", "--check", messy.to_str().unwrap()]);
    assert!(
        !out.status.success(),
        "--check на НЕотформатированном обязан вернуть ненулевой код (контракт CI)"
    );

    let canon = dir.join("canon.takt");
    std::fs::write(&canon, "var x: u8 := 0;\nstart S;\n").unwrap();
    let out = taktc(&["fmt", "--check", canon.to_str().unwrap()]);
    assert!(
        out.status.success(),
        "--check на каноническом обязан вернуть 0; stderr: {}",
        String::from_utf8_lossy(&out.stderr)
    );
}

#[test]
fn a4_fmt_check_does_not_write() {
    // `--check` - режим для CI: он обязан быть НЕразрушающим.
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join("takt_fmt_0024_03_nowrite");
    std::fs::create_dir_all(&dir).unwrap();
    let file = dir.join("messy.takt");
    let original = "var   x :u8:=0;\n";
    std::fs::write(&file, original).unwrap();

    let _ = taktc(&["fmt", "--check", file.to_str().unwrap()]);
    assert_eq!(
        std::fs::read_to_string(&file).unwrap(),
        original,
        "--check не должен изменять файл"
    );
}

#[test]
fn fmt_rewrites_file_in_place() {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join("takt_fmt_0024_03_inplace");
    std::fs::create_dir_all(&dir).unwrap();
    let file = dir.join("messy.takt");
    std::fs::write(&file, "var   x :u8:=0;\nstart   S ;\n").unwrap();

    let out = taktc(&["fmt", file.to_str().unwrap()]);
    assert!(
        out.status.success(),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert_eq!(
        std::fs::read_to_string(&file).unwrap(),
        "var x: u8 := 0;\nstart S;\n"
    );

    // Повторный прогон ничего не меняет и остаётся каноничным (A1 через CLI).
    let out = taktc(&["fmt", "--check", file.to_str().unwrap()]);
    assert!(
        out.status.success(),
        "после форматирования --check обязан быть зелёным"
    );
}

#[test]
fn inline_formula_guard_and_ltl_are_printed() {
    // Guard: `: [Guard] conds;` и `: conds;` дают один АСД - канон выбирает короткую
    // форму. Это нормализация раскладки, а не смена смысла: A3 (parse(fmt(x)) ==
    // parse(x)) проверяется на корпусе.
    let out = format_source("state A {\n    :  x > 1 ,  y ;\n}\n").unwrap();
    assert!(out.contains(": x > 1, y;"), "{out}");

    let ltl = format_source("state A {\n    : [LTL]  G  x ->  F y ;\n}\n").unwrap();
    assert!(ltl.contains(": [LTL] G x -> F y;"), "{ltl}");
}

#[test]
fn r5_blank_lines_are_restored_and_normalised() {
    // Пустые строки не хранятся в АСД - восстанавливаются из исходника. Канон: не более
    // одной подряд.
    let out = format_source("var a: u8 := 0;\n\n\n\nvar b: u8 := 0;\nvar c: u8 := 0;\n").unwrap();
    assert_eq!(
        out, "var a: u8 := 0;\n\nvar b: u8 := 0;\nvar c: u8 := 0;\n",
        "три пустые строки обязаны схлопнуться в одну, а между b и c её быть не должно"
    );
}

#[test]
fn r5_no_spurious_blank_after_multiline_function() {
    // Регресс, пойманный harness'ом на type_alias_inference.takt: `fn.loc` покрывает
    // только сигнатуру, поэтому замер "от loc.end" проезжал через тело функции и
    // вставлял пустую строку. Проверяем именно этот случай.
    let source =
        "model M {\n    fn get() -> u8 {\n        return 0;\n    }\n    var x := get();\n}\n";
    let once = format_source(source).unwrap();
    assert!(
        !once.contains("}\n\n    var x"),
        "пустая строка после тела функции появилась на ровном месте:\n{once}"
    );
    assert_eq!(once, format_source(&once).unwrap(), "идемпотентность");
}

#[test]
fn r2_blank_line_between_comment_groups_survives() {
    // Это потеря авторского замысла: шапка файла слипалась с доком следующего элемента.
    // Перечисление печатается по варианту на строке - здесь проверяется пустая строка
    // Между группами комментариев, а не его раскладка.
    let out = format_source("/// шапка файла\n\n/// про enum\nenum M { A = 0 }\n").unwrap();
    assert_eq!(
        out,
        "/// шапка файла\n\n/// про enum\nenum M {\n    A = 0\n}\n"
    );
    assert_eq!(out, format_source(&out).unwrap(), "идемпотентность");
}

#[test]
fn r4_author_keyword_is_preserved_not_canonicalised() {
    // (вариант 2): форматтер печатает слово автора. `while` - синоним `loop`, но
    // переписывать одно в другое форматтер не вправе: он меняет раскладку, а не текст
    // программы.
    let source = "var c: u8 := 0;\nstart S {\n    always {\n        while c < 3 {\n            c := c + 1;\n        }\n        loop c < 9 {\n            c := c + 1;\n        }\n    }\n}\n";
    let out = format_source(source).unwrap();
    assert!(
        out.contains("while c < 3 {"),
        "`while` обязан остаться `while`:\n{out}"
    );
    assert!(
        out.contains("loop c < 9 {"),
        "`loop` обязан остаться `loop`:\n{out}"
    );
    assert_eq!(out, format_source(&out).unwrap(), "идемпотентность");
}

#[test]
fn r4_guard_form_is_preserved() {
    // То же для синонимов `: [Guard] условия;` и `: условия;`.
    let out =
        format_source("state A {\n    : [Guard] x > 1;\n}\nstate B {\n    : y > 2;\n}\n").unwrap();
    assert!(
        out.contains(": [Guard] x > 1;"),
        "явная форма обязана сохраниться:\n{out}"
    );
    assert!(
        out.contains(": y > 2;"),
        "краткая форма обязана сохраниться:\n{out}"
    );
    assert_eq!(out, format_source(&out).unwrap(), "идемпотентность");
}

/// A16/A17: форматтер печатает `invariant` как условие (`=`, не `:=`), сохраняя форму
/// автора (не разворачивает в `cond` + `: [Guard]`), и идемпотентно.
#[test]
fn invariant_is_formatted_as_condition() {
    let out = format_source("invariant   Safe=t<=100 ;\nstart Main { always { } }")
        .expect("форматирование invariant");
    assert!(
        out.contains("invariant Safe = t <= 100;"),
        "инвариант печатается как условие (= , не :=):\n{out}"
    );
    assert!(
        !out.contains(":="),
        "правая часть — условие, не присваивание:\n{out}"
    );
    // Идемпотентность.
    let twice = format_source(&out).expect("повторное форматирование");
    assert_eq!(out, twice, "печать инварианта должна быть идемпотентной");
}
