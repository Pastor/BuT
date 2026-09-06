//! Тест предела вложенности выражений и условий.
//!
//! ## Что здесь ловится
//!
//! Построение семантического дерева рекурсивно по структуре текста, поэтому глубоко
//! вложенное выражение съедало стек: инструменты падали с `SIGABRT` (`has overflowed
//! its stack`) **без диагностики**.
//!
//! Теперь глубина ограничена (`validate::depth::MAX_NESTING_DEPTH`), и выход за предел
//! даёт `SE-062`.

use takt_lang::semantic::tree::construct_model;

/// Предел вложенности, продублированный числом намеренно.
///
/// Константа `validate::depth::MAX_NESTING_DEPTH` живёт в `pub(crate)`-модуле и
/// интеграционному тесту недоступна, а делать её публичной значит расширять API ради
/// теста. Синхронность держит парный тест `constant_matches_documented_limit` в самом
/// модуле: изменение константы красит его, и обе стороны правятся осознанно.
const MAX_NESTING_DEPTH: usize = 32;

/// Модель с условием ребра из `depth` вложенных скобок.
fn model_with_nested_parens(depth: usize) -> String {
    format!(
        "var x: u8 := 0;\nstart S {{\n  ref S: {}x{} = 1;\n}}\n",
        "(".repeat(depth),
        ")".repeat(depth)
    )
}

/// Модель с присваиванием, где значение обёрнуто в `depth` скобок.
fn model_with_nested_expression(depth: usize) -> String {
    format!(
        "var x: u8 := 0;\nstart S {{\n  always {{ x := {}1{}; }}\n  ref S: x = 9;\n}}\n",
        "(".repeat(depth),
        ")".repeat(depth)
    )
}

/// Строит модель и отдаёт диагностику в виде `КОД|сообщение`.
///
/// Отказ **разбора** оформляется так же, как отказ семантики: с у глубины появился
/// второй рубеж (`SY-005` в `parse`), и тест обязан различать их по коду, а не по
/// стадии. Иначе "глубина отвергнута" и "глубина отвергнута не тем тестом" выглядели
/// бы одинаково.
fn build(src: &str) -> Result<(), String> {
    let (ast, _) = takt_lang::parse(src, 0).map_err(|diagnostics| {
        diagnostics.first().map_or_else(
            || "разбор: диагностика отсутствует".to_string(),
            |d| format!("{}|{}", d.code.clone().unwrap_or_default(), d.message),
        )
    })?;
    construct_model(&ast, None, &[])
        .map(|_| ())
        .map_err(|d| format!("{}|{}", d.code.clone().unwrap_or_default(), d.message))
}

/// Глубина, заведомо укладывающаяся в предел.
///
/// Счётчик меряет глубину **узлов дерева**, а не число скобок: условие `(((x))) = 1`
/// добавляет к скобкам ещё и узел сравнения, поэтому "ровно `MAX_NESTING_DEPTH` скобок" -
/// уже перебор. Берём запас, чтобы тест проверял правило, а не арифметику обёрток.
const WITHIN_LIMIT: usize = MAX_NESTING_DEPTH - 8;

#[test]
fn nesting_within_limit_is_accepted() {
    // Тест направления: предел не должен "сползти" вниз от правок - обычные выражения
    // обязаны строиться как раньше.
    let err = build(&model_with_nested_parens(WITHIN_LIMIT));
    assert!(
        err.is_ok(),
        "глубина в пределах лимита обязана строиться: {err:?}"
    );
}

#[test]
fn nesting_beyond_limit_is_diagnosed_not_crash() {
    let err = build(&model_with_nested_parens(MAX_NESTING_DEPTH + 1))
        .expect_err("превышение предела обязано давать диагностику");
    assert!(
        err.starts_with("SE-062|"),
        "ожидался SE-062, получено: {err}"
    );
}

#[test]
fn deep_nesting_that_used_to_crash_is_diagnosed() {
    // Именно эта глубина роняла все три инструмента до.
    //
    // С такой ввод не доходит до семантики: предел разбора отвергает его раньше
    // (`SY-005`). Тест остаётся на месте, но проверяет то, ради чего заводился, -
    // **диагностику вместо падения**, а не то, каким из двух рубежей она выдана.
    let err = build(&model_with_nested_parens(300)).expect_err("300 уровней — диагностика");
    assert!(
        err.starts_with("SY-005|"),
        "ожидался SY-005 (предел разбора), получено: {err}"
    );
}

#[test]
fn deep_nesting_in_expression_is_diagnosed_too() {
    // Условие ребра и выражение в теле - разные точки рекурсии (`resolve_condition` и
    // `construct_expression`), прикрыть надо обе. На глубине 300 обе перекрыты пределом
    // разбора.
    let err = build(&model_with_nested_expression(300))
        .expect_err("глубокое выражение в теле — диагностика");
    assert!(
        err.starts_with("SY-005|"),
        "ожидался SY-005 (предел разбора), получено: {err}"
    );
}

#[test]
fn semantic_limit_still_applies_between_the_two_thresholds() {
    // Инвариант двух рубежей: глубина, которую разбор принимает, но семантика - нет,
    // обязана давать **семантическую** диагностику. Иначе `SE-062` стал бы
    // недостижимым, а пользователь вместо разбора причины получал бы отказ разбора.
    let err = build(&model_with_nested_parens(MAX_NESTING_DEPTH + 8))
        .expect_err("глубина между рубежами — диагностика");
    assert!(
        err.starts_with("SE-062|"),
        "ожидался SE-062 (семантический предел), получено: {err}"
    );
}

#[test]
fn diagnostic_names_the_limit() {
    // Сообщение обязано называть предел: без числа пользователь не поймёт, к чему
    // стремиться. Проверяется на семантическом рубеже - там предел и есть
    // `MAX_NESTING_DEPTH`.
    let err = build(&model_with_nested_parens(MAX_NESTING_DEPTH + 8)).expect_err("диагностика");
    assert!(
        err.contains(&MAX_NESTING_DEPTH.to_string()),
        "сообщение обязано называть предел {MAX_NESTING_DEPTH}: {err}"
    );
}

/// Модель с `depth` вложенными `if` в теле `always`.
fn model_with_nested_statements(depth: usize) -> String {
    let mut body = String::from("x := 5;");
    for _ in 0..depth {
        body = format!("if x > 0 {{ {body} }}");
    }
    format!("var x: u8 := 0;\nstart S {{\n  always {{ {body} }}\n  ref S: x = 9;\n}}\n")
}

#[test]
fn deep_statement_nesting_is_diagnosed() {
    // Точка рекурсии по **операторам** (`resolve_ast_statement`) - третья наряду с
    // условием ребра и выражением. До она была прикрыта только на бумаге: тест
    // срабатывал, но его ошибку глотало разрешение вложенного тела, и модель
    // компилировалась молча.
    //
    // Глубина взята **между рубежами**: вложенный `if` даёт два узла дерева (`If` +
    // `Block`), поэтому 60 операторов - это уже за пределом разбора, и диагностику
    // выдал бы `SY-005`. Здесь проверяется именно тест операторов в семантике,
    // поэтому вложенность на треть выше семантического предела и вдвое ниже парсерного.
    let err = build(&model_with_nested_statements(MAX_NESTING_DEPTH + 8))
        .expect_err("вложенные операторы сверх предела — диагностика, а не тишина");
    assert!(
        err.starts_with("SE-062|"),
        "ожидался SE-062, получено: {err}"
    );
}

#[test]
fn statement_nesting_within_limit_is_accepted() {
    // Тест направления: предел не должен сползти вниз - обычная вложенность (в
    // корпусе встречается тройная) обязана строиться.
    let ok = build(&model_with_nested_statements(3));
    assert!(ok.is_ok(), "тройная вложенность обязана строиться: {ok:?}");
}

#[test]
fn counter_does_not_leak_between_builds() {
    // Счётчик потоковый; если бы выход из рекурсии не учитывался, вторая сборка в том
    // же потоке упёрлась бы в остаток от первой. Строим подряд несколько моделей у
    // самого предела - все обязаны пройти.
    for _ in 0..3 {
        assert!(
            build(&model_with_nested_parens(WITHIN_LIMIT)).is_ok(),
            "остаток счётчика от предыдущей сборки"
        );
    }
}
