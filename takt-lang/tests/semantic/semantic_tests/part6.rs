//! Интеграционные тесты семантики, часть 6 (вынос из `semantic_tests.rs`).
//!
//! Хелперы и импорты - из родителя через `use super::*` (приём /08).

use super::*;

/// Блок с опечаткой в имени генерирует предупреждение SE-045.
#[test]
fn test_unknown_named_block_typo_generates_warning() {
    let (ast, _) =
        takt_lang::parse("start S { enteer { } } state Done;", 0).expect("ошибка разбора");
    let warnings = takt_lang::unknown_named_block_warnings(&ast);
    assert_eq!(
        warnings.len(),
        1,
        "ожидалось 1 предупреждение SE-045, получено: {:?}",
        warnings
    );
    assert_eq!(warnings[0].code.as_deref(), Some("SE-045"));
    assert!(warnings[0].message.contains("enteer"));
}

/// Корректные блоки `enter`, `exit`, `always` не генерируют предупреждений.
#[test]
fn test_known_named_blocks_no_warning() {
    let (ast, _) =
        takt_lang::parse("start S { enter { } exit { } always { } }", 0).expect("ошибка разбора");
    let warnings = takt_lang::unknown_named_block_warnings(&ast);
    assert!(
        warnings.is_empty(),
        "известные блоки не должны предупреждать: {:?}",
        warnings
    );
}

/// Неизвестный блок на уровне модели тоже предупреждает.
#[test]
fn test_unknown_named_block_at_model_level_generates_warning() {
    let (ast, _) = takt_lang::parse("tick { } start S;", 0).expect("ошибка разбора");
    let warnings = takt_lang::unknown_named_block_warnings(&ast);
    assert_eq!(
        warnings.len(),
        1,
        "ожидалось 1 предупреждение SE-045 на уровне модели, получено: {:?}",
        warnings
    );
    assert_eq!(warnings[0].code.as_deref(), Some("SE-045"));
}

// ---: предупреждение о лишней точке с запятой ----------------------

/// Двойная точка с запятой `;;` на уровне модели генерирует предупреждение SE-044.
#[test]
fn test_stray_semicolon_at_model_level_generates_warning() {
    let (ast, _) = takt_lang::parse("start S;; state Done;", 0).expect("ошибка разбора");
    let warnings = takt_lang::stray_semicolon_warnings(&ast);
    assert_eq!(
        warnings.len(),
        1,
        "ожидалось 1 предупреждение SE-044, получено: {:?}",
        warnings
    );
    assert_eq!(warnings[0].code.as_deref(), Some("SE-044"));
    assert!(warnings[0].message.contains("точка с запятой"));
}

/// Двойная точка с запятой `;;` внутри состояния генерирует предупреждение SE-044.
#[test]
fn test_stray_semicolon_inside_state_generates_warning() {
    let (ast, _) =
        takt_lang::parse("start S { ref Done;; } state Done;", 0).expect("ошибка разбора");
    let warnings = takt_lang::stray_semicolon_warnings(&ast);
    assert_eq!(
        warnings.len(),
        1,
        "ожидалось 1 предупреждение SE-044 внутри состояния, получено: {:?}",
        warnings
    );
    assert_eq!(warnings[0].code.as_deref(), Some("SE-044"));
}

/// Корректный код без лишних `;` не генерирует предупреждений.
#[test]
fn test_no_stray_semicolon_no_warning() {
    let (ast, _) = takt_lang::parse("cond X = true; start S { ref Done: X; } state Done;", 0)
        .expect("ошибка разбора");
    let warnings = takt_lang::stray_semicolon_warnings(&ast);
    assert!(
        warnings.is_empty(),
        "предупреждений быть не должно, получено: {:?}",
        warnings
    );
}

// --- Задача 1: cond требует завершающего `;` ----------------------------------

/// Объявление `cond` с `;` разбирается как один элемент модели (не порождает
/// StraySemicolon).
#[test]
fn test_cond_define_semicolon_consumed_not_stray() {
    use takt_lang::parser::ast::ModelElement;
    let (ast, errs) = takt_lang::parse("cond X = true; start S;", 0).expect("ошибка разбора");
    assert!(errs.is_empty(), "ошибок разбора быть не должно");
    let cond_count = ast
        .elements
        .iter()
        .filter(|e| matches!(e, ModelElement::Condition(_)))
        .count();
    let stray_count = ast
        .elements
        .iter()
        .filter(|e| matches!(e, ModelElement::StraySemicolon(_)))
        .count();
    assert_eq!(cond_count, 1, "ожидался 1 элемент Condition");
    assert_eq!(
        stray_count, 0,
        "StraySemicolon после cond не должен появляться"
    );
}

/// `match` с числовыми паттернами и wildcard разрешается без ошибок.
#[test]
fn match_statement_is_valid() {
    let src = r#"
        model M {
            var x: bit := 0;
            start S {
                always {
                    match x {
                        0 => { x := 1; }
                        1 | 2 => { x := 0; }
                        _ => { x := 0; }
                    }
                }
            }
        }
    "#;
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let result = construct_model(&ast, None, &[]);
    assert!(
        result.is_ok(),
        "match должен разрешаться без ошибок: {:?}",
        result
    );
}

/// fixture `match_switch.takt` проходит семантический анализ без ошибок.
#[test]
fn match_switch_fixture_is_valid() {
    let src = std::fs::read_to_string("tests/data/semantic/valid/match_switch.takt")
        .expect("файл не найден");
    let (ast, _) = parse(&src, 0).expect("ошибка разбора");
    let result = construct_model(&ast, None, &[]);
    assert!(
        result.is_ok(),
        "match_switch.takt должен проходить анализ: {:?}",
        result
    );
}

// ---: Анализ константных условий ------------------------------------

/// SE-047: переход с `1 = 0` - всегда ложно -> предупреждение.
#[test]
fn constant_condition_always_false_warns() {
    let src = "start S { ref S: 1 = 0; }";
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let model = construct_model(&ast, None, &[]).expect("ошибка семантики");
    let warnings = takt_lang::constant_condition_warnings(&model);
    assert!(
        warnings.iter().any(|d| d.code.as_deref() == Some("SE-047")),
        "ожидалось SE-047 для `1 = 0`, получено: {:?}",
        warnings
    );
}

/// SE-047: переход с `1 = 1` - всегда истинно -> предупреждение.
#[test]
fn constant_condition_always_true_warns() {
    let src = "start S { ref S: 1 = 1; }";
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let model = construct_model(&ast, None, &[]).expect("ошибка семантики");
    let warnings = takt_lang::constant_condition_warnings(&model);
    assert!(
        warnings.iter().any(|d| d.code.as_deref() == Some("SE-047")),
        "ожидалось SE-047 для `1 = 1`, получено: {:?}",
        warnings
    );
}

/// SE-047: переход с переменной в условии - не предупреждение.
#[test]
fn constant_condition_with_variable_no_warn() {
    let src = "var x: bit := 0; start S { ref S: x = 1; }";
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let model = construct_model(&ast, None, &[]).expect("ошибка семантики");
    let warnings = takt_lang::constant_condition_warnings(&model);
    assert!(
        !warnings.iter().any(|d| d.code.as_deref() == Some("SE-047")),
        "SE-047 не должен выдаваться для `x = 1`, получено: {:?}",
        warnings
    );
}

// ---: вызов функции из тела функции ---------------------------------

/// A1 (R1): композиция `f -> g` внутри модели компилируется (было SE-004).
#[test]
fn fn_calls_fn_composition_compiles() {
    let node = build(
        "fn g(x: u8) -> u8 { return x; } fn f(x: u8) -> u8 { return g(x); } start Main { always { } }",
    );
    assert!(node.functions.contains_key("f") && node.functions.contains_key("g"));
}

/// A2 (R1): порядок объявления не важен - `f` вызывает `g`, объявленную ниже.
#[test]
fn fn_calls_fn_declaration_order_independent() {
    // Не должно паниковать/ошибаться: `f` объявлена раньше вызываемой `g`.
    let node = build(
        "fn f(x: u8) -> u8 { return g(x); } fn g(x: u8) -> u8 { return x; } start Main { always { } }",
    );
    assert!(node.functions.contains_key("f"));
}

/// A3 (R2): вызов функции родительской модели из вложенной - регресса нет.
#[test]
fn fn_calls_parent_fn_still_works() {
    let node = build(
        "fn top(x: u8) -> u8 { return x; } \
         model Inner { fn inner_fn(x: u8) -> u8 { return top(x); } start S { always { } } } \
         start Main = Inner;",
    );
    let inner = node.search_model("Inner").expect("Inner найдена");
    assert!(inner.borrow().functions.contains_key("inner_fn"));
}

/// A4 (R3): прямая рекурсия `f -> f` отвергается SE-053 с цепочкой.
#[test]
fn fn_direct_recursion_is_se053() {
    let err = build_err("fn f(x: u8) -> u8 { return f(x); } start Main { always { } }");
    assert_eq!(err.code.as_deref(), Some("SE-053"), "код: {err:?}");
    assert!(
        err.message.contains("f → f"),
        "цепочка в сообщении: {}",
        err.message
    );
}

/// A5 (R3): взаимная рекурсия `f -> g -> f` отвергается SE-053 с полной цепочкой.
#[test]
fn fn_mutual_recursion_is_se053() {
    let err = build_err(
        "fn f(x: u8) -> u8 { return g(x); } fn g(x: u8) -> u8 { return f(x); } start Main { always { } }",
    );
    assert_eq!(err.code.as_deref(), Some("SE-053"), "код: {err:?}");
    assert!(
        err.message.contains("→"),
        "цепочка в сообщении: {}",
        err.message
    );
}

/// A6 (R3): цикл длины 3 `f -> g -> h -> f` отвергается SE-053.
#[test]
fn fn_cycle_three_is_se053() {
    let err = build_err(
        "fn f(x: u8) -> u8 { return g(x); } fn g(x: u8) -> u8 { return h(x); } \
         fn h(x: u8) -> u8 { return f(x); } start Main { always { } }",
    );
    assert_eq!(err.code.as_deref(), Some("SE-053"), "код: {err:?}");
}

/// A7 (R4): вызов необъявленного имени остаётся SE-004, не SE-053.
#[test]
fn fn_unknown_call_is_se004() {
    let err = build_err("fn f(x: u8) -> u8 { return ghost(x); } start Main { always { } }");
    assert_eq!(err.code.as_deref(), Some("SE-004"), "код: {err:?}");
}

/// A8 (R5): встроенные функции (`min`) из тела `fn` работают - рёбер не дают.
#[test]
fn fn_calls_builtin_compiles() {
    let node = build("fn f(x: u8) -> u8 { return min(x, 1); } start Main { always { } }");
    assert!(node.functions.contains_key("f"));
}

/// A10 (R7): дубликат имени функции - SE-009 (прежде принимался молча).
#[test]
fn fn_duplicate_name_is_se009() {
    let err = build_err(
        "fn f(x: u8) -> u8 { return 1; } fn f(x: u8) -> u8 { return 2; } start Main { always { } }",
    );
    assert_eq!(err.code.as_deref(), Some("SE-009"), "код: {err:?}");
}

// ---: инвариант (invariant) -----------------------------------------

/// A1: `invariant P = C;` разбирается как элемент модели.
#[test]
fn invariant_parses_as_model_element() {
    let node = build("var t: u8 := 0; invariant Safe = t <= 100; start Main { always { } }");
    // Десахаризация: имя P попадает в условия (для LTL-атома и ref).
    assert!(
        node.conditions.contains_key("Safe"),
        "инвариант регистрирует cond"
    );
    // И обязательство - Guard-формула в formulas.
    assert!(
        node.formulas.iter().any(
            |f| matches!(f, takt_lang::semantic::formula::Formula::Guard(_, Some(n), _) if n == "Safe")
        ),
        "инвариант даёт именованную Guard-формулу: {:?}",
        node.formulas
    );
}

/// A2: `assert` ключевым словом не стал - `var assert` валиден.
#[test]
fn assert_is_not_a_keyword() {
    let node = build("var assert: u8 := 1; start Main { always { } }");
    assert!(node.variables.contains_key("assert"));
}

/// A5 (ключевой тест фичи): имя инварианта - атом LTL. До 0044 `G(t <= 100)` невыразимо
/// (`LtlPrimary` принимает только идентификатор); инвариант даёт имя.
#[test]
fn invariant_name_is_ltl_atom() {
    // Не должно быть ошибки: G(Safe) ссылается на имя инварианта.
    let node = build(
        "var t: u8 := 0; invariant Safe = t <= 100; : [LTL] G(Safe); start Main { always { } }",
    );
    assert!(
        node.formulas
            .iter()
            .any(|f| matches!(f, takt_lang::semantic::formula::Formula::LTL(_, _))),
        "должна быть LTL-формула, ссылающаяся на инвариант"
    );
}

/// A6: имя инварианта - условие ребра (`ref Next: P;`).
#[test]
fn invariant_name_is_edge_condition() {
    // Не должно паниковать/ошибаться: ref ссылается на инвариант как на условие.
    let _ = build(
        "var t: u8 := 0; invariant Ready = t = 1; \
         start A { ref B: Ready; } state B;",
    );
}

/// A7: коллизия имени инварианта с существующим `cond` -> SE-054.
#[test]
fn invariant_name_clash_is_se054() {
    let err = build_err(
        "var t: u8 := 0; cond Safe = t <= 100; invariant Safe = t <= 50; start Main { always { } }",
    );
    assert_eq!(err.code.as_deref(), Some("SE-054"), "код: {err:?}");
}
