//! Юнит-тесты слоя использований.
//!
//! Проверяется то, ради чего слой заведён: **полнота** (тела блоков и функций, которых
//! не видит `SemanticIndex`), **раздельность областей** (затенение и одноимённые
//! символы разных моделей) и **точность диапазонов**.

use super::*;
use crate::parse;

/// Строит таблицу вхождений по исходному тексту.
fn table_of(src: &str) -> UsageTable {
    let (ast, _) = parse(src, 0).expect("исходник должен разбираться");
    collect_usages(&ast)
}

/// Смещение `n`-го (с нуля) вхождения подстроки.
fn nth_offset(src: &str, needle: &str, n: usize) -> usize {
    src.match_indices(needle)
        .nth(n)
        .unwrap_or_else(|| panic!("в тексте нет {}-го вхождения `{needle}`", n + 1))
        .0
}

/// Диапазоны всех вхождений символа, найденного по смещению.
fn occurrence_ranges(table: &UsageTable, offset: usize) -> Vec<(u32, u32)> {
    let usage = table
        .usage_at(offset)
        .unwrap_or_else(|| panic!("на смещении {offset} нет вхождения"));
    table
        .occurrences_of(usage.symbol)
        .iter()
        .map(|u| (u.start, u.end))
        .collect()
}

/// Модель, в которой переменная используется во всех местах сразу.
const ALL_PLACES: &str = r#"model M {
    out flag: bit;
    var speed: u8 := 0;
    var mirror: u8 := speed;
    cond Fast = speed > 3;
    fn bump(x: u8) -> u8 { return x + speed; }
    start Idle {
        enter { speed := bump(speed); }
        always { speed := speed + 1; }
        exit { flag := 1; }
        ref Done: speed > 10;
    }
    state Done;
}
"#;

/// Полнота: вхождения в телах блоков и функции - те самые, которых нет в
/// `SemanticIndex` (замер F1 анализа).
#[test]
fn usages_cover_block_and_function_bodies() {
    let table = table_of(ALL_PLACES);
    let decl = nth_offset(ALL_PLACES, "speed", 0);
    let ranges = occurrence_ranges(&table, decl);

    // Ожидаются: объявление, инициализатор `mirror`, `cond`, тело `fn`, `enter`
    // (дважды), `always` (дважды), условие ребра.
    assert_eq!(
        ranges.len(),
        9,
        "найдено {} вхождений: {ranges:?}",
        ranges.len()
    );

    // Каждое вхождение - ровно идентификатор.
    for (start, end) in &ranges {
        assert_eq!(
            &ALL_PLACES[*start as usize..*end as usize],
            "speed",
            "диапазон {start}..{end} покрывает не имя"
        );
    }
}

/// Вхождение в теле `enter` действительно попало в таблицу (точечно - чтобы провал был
/// читаемым, а не "9 != 8").
#[test]
fn usage_inside_enter_block_is_found() {
    let table = table_of(ALL_PLACES);
    let inside_enter = nth_offset(ALL_PLACES, "enter { speed", 0) + "enter { ".len();
    let usage = table
        .usage_at(inside_enter)
        .expect("вхождение в теле enter должно находиться");
    assert_eq!(usage.name, "speed");
    assert_eq!(usage.kind, UsageKind::Reference);
    assert_eq!(usage.symbol_kind, SymbolKind::Variable);
}

/// Затенение: локальная `var x` блока и переменная модели - **разные** символы.
///
/// Слой обязан различать их так же - иначе переименование испортит чужие вхождения
/// молча.
#[test]
fn local_variable_shadows_model_variable() {
    const SRC: &str = r#"model M {
    var x: u8 := 1;
    out y: u8;
    start S {
        always {
            var x: u8 := 2;
            x := x + 1;
            y := x;
        }
    }
}
"#;
    let table = table_of(SRC);

    let model_decl = nth_offset(SRC, "var x: u8 := 1", 0) + "var ".len();
    let model_ranges = occurrence_ranges(&table, model_decl);
    assert_eq!(
        model_ranges.len(),
        1,
        "у затенённой переменной модели остаётся только объявление: {model_ranges:?}"
    );

    let local_decl = nth_offset(SRC, "var x: u8 := 2", 0) + "var ".len();
    let local_ranges = occurrence_ranges(&table, local_decl);
    assert_eq!(
        local_ranges.len(),
        4,
        "локальная: объявление + три вхождения в теле, найдено {local_ranges:?}"
    );
    assert!(
        local_ranges.iter().all(|(s, _)| *s as usize > model_decl),
        "вхождения локальной не должны включать объявление модели"
    );
}

/// Одноимённые переменные разных моделей не смешиваются.
#[test]
fn same_name_in_two_models_are_distinct_symbols() {
    const SRC: &str = r#"model A {
    var speed: u8 := 0;
    start S { always { speed := speed + 1; } }
}

model B {
    var speed: u8 := 0;
    start S { always { speed := speed + 2; } }
}
"#;
    let table = table_of(SRC);
    let first = nth_offset(SRC, "var speed", 0) + "var ".len();
    let second = nth_offset(SRC, "var speed", 1) + "var ".len();

    let first_ranges = occurrence_ranges(&table, first);
    let second_ranges = occurrence_ranges(&table, second);
    assert_eq!(first_ranges.len(), 3, "модель A: {first_ranges:?}");
    assert_eq!(second_ranges.len(), 3, "модель B: {second_ranges:?}");
    assert!(
        first_ranges.iter().all(|(s, _)| (*s as usize) < second),
        "вхождения модели A не должны заходить в модель B"
    );
}

/// Вложенная модель видит переменную внешней - как `search_var` по цепочке `upper`.
#[test]
fn nested_model_sees_outer_variable() {
    const SRC: &str = r#"model Outer {
    var shared: u8 := 0;
    model Inner {
        start S { always { shared := shared + 1; } }
    }
    start Root = Inner;
}
"#;
    let table = table_of(SRC);
    let decl = nth_offset(SRC, "var shared", 0) + "var ".len();
    let ranges = occurrence_ranges(&table, decl);
    assert_eq!(
        ranges.len(),
        3,
        "объявление + два вхождения во вложенной модели: {ranges:?}"
    );
}

/// Имя состояния в `ref` и в `S(Модель) = Состояние` - использования состояния.
#[test]
fn state_names_are_tracked_in_ref_and_state_of_model() {
    const SRC: &str = r#"model Ping {
    start Go { ref End; }
    state End;
}

model Main {
    start Watch { ref Stop: S(Ping) = End; }
    state Stop;
}

start Root = Main;
"#;
    let table = table_of(SRC);
    let decl = nth_offset(SRC, "state End", 0) + "state ".len();
    let ranges = occurrence_ranges(&table, decl);
    assert_eq!(
        ranges.len(),
        3,
        "объявление + `ref End` + правая часть `S(Ping) = End`: {ranges:?}"
    );

    // Имя модели в `S(Ping)` разрешается как модель.
    let model_use = nth_offset(SRC, "S(Ping)", 0) + "S(".len();
    let usage = table.usage_at(model_use).expect("`Ping` в S(...)");
    assert_eq!(usage.symbol_kind, SymbolKind::Model);
}

/// Переменная в формуле LTL - использование ( установила это для `SE-036`; пропустив
/// формулы, переименование оставило бы их со старым именем).
#[test]
fn variable_in_ltl_formula_is_a_usage() {
    const SRC: &str = r#"model M {
    var flag: bit := 0;
    start S {
        always { flag := 1; }
        : [LTL] G flag;
    }
}
"#;
    let table = table_of(SRC);
    let decl = nth_offset(SRC, "var flag", 0) + "var ".len();
    let ranges = occurrence_ranges(&table, decl);
    assert_eq!(
        ranges.len(),
        3,
        "объявление + тело always + атом формулы: {ranges:?}"
    );
    let in_formula = nth_offset(SRC, "G flag", 0) + "G ".len();
    assert!(
        table.usage_at(in_formula).is_some(),
        "атом формулы обязан быть вхождением"
    );
}

/// Имя порта в операторе `address` - использование порта.
#[test]
fn address_operator_references_port() {
    const SRC: &str = r#"model M {
    in sensor: u8;
    address sensor = 0x40001000;
    start S { ref S: sensor > 0; }
}
"#;
    let table = table_of(SRC);
    let decl = nth_offset(SRC, "in sensor", 0) + "in ".len();
    let ranges = occurrence_ranges(&table, decl);
    assert_eq!(
        ranges.len(),
        3,
        "объявление + `address` + условие ребра: {ranges:?}"
    );
}

/// Имя, объявленное вне файла, не связывается - и попадает в список неразрешённых
/// (тест полноты для переименования).
#[test]
fn unknown_name_is_reported_unresolved() {
    const SRC: &str = r#"import "helper.takt" as Helper;

model M {
    start S { ref S: outside > 0; }
}
"#;
    let table = table_of(SRC);
    assert!(
        table.has_unresolved_named("outside"),
        "неизвестное имя обязано попасть в неразрешённые: {:?}",
        table.unresolved()
    );
    // Алиас импорта - объявление вида "модель": переименование ему запрещено.
    let alias = nth_offset(SRC, "as Helper", 0) + "as ".len();
    let usage = table.usage_at(alias).expect("алиас импорта");
    assert_eq!(usage.symbol_kind, SymbolKind::Model);
    assert_eq!(usage.kind, UsageKind::Declaration);
}

/// Корпус разбирается целиком: непокрытых узлов нет.
///
/// Тест правила "полнота или отказ": встреть обход незнакомый узел на реальном
/// примере - переименование в нём откажет, и узнать об этом лучше здесь.
#[test]
fn examples_corpus_is_fully_covered() {
    let dir = std::path::Path::new("../examples");
    let entries = std::fs::read_dir(dir).expect("каталог examples/");
    let mut checked = 0;
    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) != Some("takt") {
            continue;
        }
        let src = std::fs::read_to_string(&path).expect("чтение примера");
        let Ok((ast, _)) = parse(&src, 0) else {
            continue;
        };
        let table = collect_usages(&ast);
        assert!(
            table.is_complete(),
            "{}: непокрытые узлы {:?}",
            path.display(),
            table.unsupported()
        );
        checked += 1;
    }
    assert!(checked > 0, "не проверено ни одного примера");
}
