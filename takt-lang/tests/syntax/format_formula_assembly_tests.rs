//! Печать блоков `formula` и `assembly` форматтером.
//!
//! До фичи `taktc fmt` на обоих узлах **отказывал** (`FM-001`), и это значило больше,
//! чем неудобство: канон корпуса тестыт `fmt --check`, поэтому примера с этими
//! конструкциями в `examples/` быть не могло - проверка покрытия держал одиннадцать видов
//! узлов в реестре долга именно по этой причине.
//!
//! Проверяется не "печать не падает", а **что напечатано**: текст, разбор напечатанного
//! и неподвижность второго прогона.

use takt_lang::format::format_source;

/// Форматирует исходник, требуя успеха.
fn fmt(source: &str) -> String {
    format_source(source).unwrap_or_else(|e| panic!("форматтер отказал: {e:?}"))
}

/// Блок уровня модели: заголовок, вызовы, вложенный блок.
#[test]
fn model_level_formula_is_printed() {
    let out = fmt("model M{var l:u8:=0;formula{assume(l,250){holds(l)}}start S;}");
    assert!(out.contains("    formula {\n"), "{out}");
    assert!(out.contains("        assume(l, 250)\n"), "{out}");
    assert!(
        out.contains("        {\n            holds(l)\n        }\n"),
        "{out}"
    );
}

/// Диалект - часть записи автора и обязан пережить печать.
///
/// Ради него фича правит **АСД**: грамматика диалект разбирала и выбрасывала, то есть
/// печать без правки теряла бы кусок исходника молча - ровно то, от чего форматтер
/// защищает отказом.
#[test]
fn dialect_survives_printing() {
    let out = fmt("model M{formula \"smt\"{assume(x)}start S;}");
    assert!(out.contains("formula \"smt\" {"), "диалект потерян: {out}");
}

/// Пустой блок печатается одной строкой - как пустое тело у соседей.
#[test]
fn empty_formula_block_is_one_line() {
    let out = fmt("model M{formula{}start S;}");
    assert!(out.contains("    formula {}\n"), "{out}");
}

/// Все формы выражения блока формул: литералы с аннотацией и без, строка, переменная,
/// доступ к члену, вложенный вызов.
///
/// Скобок в списке нет намеренно: вариант `FormulaExpression::Parenthesis` в АСД есть,
/// а грамматика его **не строит** (замер ) - печать для него написана, но проверить её
/// нечем, входа не существует.
#[test]
fn every_formula_expression_form_is_printed() {
    let out = fmt("model M{formula{f(true, 42, \"s\", x, u.v, g(1), 7:u8)}start S;}");
    assert!(
        out.contains("f(true, 42, \"s\", x, u.v, g(1), 7:u8)"),
        "{out}"
    );
}

/// `assembly` в теле: диалект, тело обычными операторами Takt.
#[test]
fn assembly_in_body_is_printed() {
    let out = fmt("model M{var a:u8:=0;start S{always{assembly \"c\"{a:=1;}}}}");
    assert!(out.contains("            assembly \"c\" {\n"), "{out}");
    assert!(out.contains("                a := 1;\n"), "{out}");
}

/// `assembly` без диалекта - форма автора сохраняется, кавычки не выдумываются.
#[test]
fn assembly_without_dialect_keeps_the_form() {
    let out = fmt("model M{var a:u8:=0;start S{always{assembly{a:=1;}}}}");
    assert!(out.contains("assembly {\n"), "{out}");
    assert!(
        !out.contains("assembly \"\""),
        "выдуман пустой диалект: {out}"
    );
}

/// Блок формул в теле состояния - тот же печатник, что у уровня модели.
#[test]
fn formula_in_body_is_printed() {
    let out = fmt("model M{var l:u8:=0;start S{always{formula{step(l)}}}}");
    assert!(out.contains("            formula {\n"), "{out}");
    assert!(out.contains("                step(l)\n"), "{out}");
}

/// Второй прогон ничего не меняет: печать - неподвижная точка.
///
/// Без этой проверки канон мог бы "дышать" - файл в каноне после `fmt` оказывался бы не
/// в каноне при следующем `fmt --check`.
#[test]
fn printing_is_idempotent() {
    let source = "model M{var a:u8:=0;formula \"smt\"{q(a){r(a.b, 1:u8)}}\
                  start S{always{assembly \"c\"{a:=1;}formula{s(a)}}}}";
    let once = fmt(source);
    let twice = fmt(&once);
    assert_eq!(once, twice, "второй прогон изменил текст");
}

/// Напечатанное разбирается обратно и даёт **то же дерево**.
///
/// Совпадение текста само по себе не доказывает, что смысл сохранён: печать могла бы
/// потерять аргумент или переставить операторы и остаться стабильной.
#[test]
fn printed_source_parses_to_the_same_tree() {
    let source = "model M{var a:u8:=0;formula \"smt\"{q(a){r(a.b, 1:u8)}}\
                  start S{always{assembly \"c\"{a:=1;}formula{s(a)}}}}";
    let printed = fmt(source);
    let (before, _) = takt_lang::parse(source, 0).expect("исходник разбирается");
    let (after, _) = takt_lang::parse(&printed, 0).expect("напечатанное разбирается");
    assert_eq!(
        without_positions(&format!("{before:?}")),
        without_positions(&format!("{after:?}")),
        "печать изменила дерево"
    );
}

/// Убирает координаты из дампа дерева.
///
/// Смещения после печати меняются по существу - текст стал другим; сравнивать надо
/// структуру. Своего обхода дерева ради этого не заводится: сравнение дампов ловит и
/// потерю аргумента, и перестановку операторов.
fn without_positions(dump: &str) -> String {
    let mut out = String::with_capacity(dump.len());
    let mut rest = dump;
    while let Some(at) = rest.find("Source(") {
        out.push_str(&rest[..at]);
        let tail = &rest[at..];
        let close = tail.find(')').expect("координата закрыта скобкой");
        out.push_str("Source(_)");
        rest = &tail[close + 1..];
    }
    out.push_str(rest);
    out
}
