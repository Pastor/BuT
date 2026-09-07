//! Комментарий сохраняет **место** при форматировании.

use takt_lang::format::format_source;

/// Форматирует исходник и возвращает результат.
fn fmt(source: &str) -> String {
    format_source(source).expect("форматтер обязан принять фикстуру")
}

/// Номер строки, содержащей `needle` (для проверки порядка, а не только отступа).
fn line_no(text: &str, needle: &str) -> usize {
    text.lines()
        .position(|l| l.contains(needle))
        .unwrap_or_else(|| panic!("строка '{needle}' не найдена:\n{text}"))
}

/// Строка с комментарием и её отступ - для проверки не только факта, но и места.
fn comment_line(text: &str, needle: &str) -> String {
    text.lines()
        .find(|l| l.contains(needle))
        .unwrap_or_else(|| panic!("комментарий '{needle}' исчез из вывода:\n{text}"))
        .to_string()
}

/// Комментарий первой строкой тела остаётся в теле.
#[test]
fn comment_first_in_body_stays_inside() {
    let src = "var n: u8 := 0;\n\
               start Run {\n\
               \x20   always {\n\
               \x20       // пояснение к телу\n\
               \x20       n := n + 1;\n\
               \x20   }\n\
               \x20   ref Run;\n\
               }\n";
    let out = fmt(src);
    assert_eq!(
        comment_line(&out, "пояснение к телу"),
        "        // пояснение к телу",
        "комментарий обязан остаться в теле и с отступом тела:\n{out}"
    );
}

/// Комментарий между операторами тела.
#[test]
fn comment_between_statements_stays_inside() {
    let src = "var n: u8 := 0;\n\
               start Run {\n\
               \x20   always {\n\
               \x20       n := n + 1;\n\
               \x20       // между операторами\n\
               \x20       n := n + 2;\n\
               \x20   }\n\
               \x20   ref Run;\n\
               }\n";
    let out = fmt(src);
    assert_eq!(
        comment_line(&out, "между операторами"),
        "        // между операторами",
        "комментарий между операторами обязан остаться на месте:\n{out}"
    );
}

/// Комментарий **последней строкой** тела.
///
/// Отдельный случай: ведущих комментариев ему никто не выдаст - следующего оператора
/// нет. Без явного вызова перед `}` его подхватывал бы `leading()` следующего элемента,
/// то есть привязывал к чужому узлу.
#[test]
fn comment_last_in_body_stays_inside() {
    let src = "var n: u8 := 0;\n\
               start Run {\n\
               \x20   always {\n\
               \x20       n := n + 1;\n\
               \x20       // последней строкой\n\
               \x20   }\n\
               \x20   ref Run;\n\
               }\n";
    let out = fmt(src);
    assert_eq!(
        comment_line(&out, "последней строкой"),
        "        // последней строкой",
        "комментарий последней строкой тела обязан остаться внутри:\n{out}"
    );
}

/// Комментарий во вложенном `if` не всплывает на уровень состояния.
#[test]
fn comment_in_nested_if_keeps_its_depth() {
    let src = "var n: u8 := 0;\n\
               start Run {\n\
               \x20   always {\n\
               \x20       if n < 3 {\n\
               \x20           // внутри if\n\
               \x20           n := n + 1;\n\
               \x20       }\n\
               \x20   }\n\
               \x20   ref Run;\n\
               }\n";
    let out = fmt(src);
    assert_eq!(
        comment_line(&out, "внутри if"),
        "            // внутри if",
        "комментарий обязан остаться на СВОЁЙ глубине, а не всплыть:\n{out}"
    );
}

/// Комментарий в теле функции не уезжает за её пределы.
#[test]
fn comment_in_function_body_stays_inside() {
    let src = "fn f(a: u8) -> u8 {\n\
               \x20   // в теле функции\n\
               \x20   return a + 1;\n\
               }\n\
               start Run { ref Run; }\n";
    let out = fmt(src);
    assert_eq!(
        comment_line(&out, "в теле функции"),
        "    // в теле функции",
        "комментарий обязан остаться в функции, а не уехать к следующему \
         элементу файла:\n{out}"
    );
    // Отступа недостаточно: при печати `return` в обход привязки комментарий всё равно
    // остаётся внутри тела (его подхватывает выдача перед `}`), но оказывается после
    // оператора, к которому относится. Мутационная проверка показала, что тест без
    // этого утверждения дефект пропускает.
    assert!(
        line_no(&out, "в теле функции") < line_no(&out, "return"),
        "комментарий обязан стоять ПЕРЕД оператором, к которому относится:\n{out}"
    );
}

/// Хвостовой комментарий остаётся на строке своего оператора.
#[test]
fn trailing_comment_stays_on_its_statement() {
    let src = "var n: u8 := 0;\n\
               start Run {\n\
               \x20   always {\n\
               \x20       n := n + 1; // хвостовой в теле\n\
               \x20       n := n + 2;\n\
               \x20   }\n\
               \x20   ref Run;\n\
               }\n";
    let out = fmt(src);
    assert_eq!(
        comment_line(&out, "хвостовой в теле"),
        "        n := n + 1; // хвостовой в теле",
        "хвостовой комментарий обязан остаться на строке своего оператора:\n{out}"
    );
}

/// Форматтер идемпотентен.
///
/// До правки он тоже был идемпотентен - комментарий уезжал ровно один раз. Свойство
/// нельзя потерять: иначе повторный прогон стал бы двигать текст дальше с каждым
/// вызовом.
#[test]
fn formatting_is_idempotent() {
    let src = "var n: u8 := 0;\n\
               start Run {\n\
               \x20   always {\n\
               \x20       // первой строкой\n\
               \x20       n := n + 1;\n\
               \x20       // последней строкой\n\
               \x20   }\n\
               \x20   ref Run;\n\
               }\n";
    let once = fmt(src);
    let twice = fmt(&once);
    assert_eq!(once, twice, "повторный прогон не должен ничего двигать");
}
