//! Канон стиля: K&R `else` и перечисление по варианту на строке.
//!
//! # Что изменилось
//!
//! пересмотрел два правила печати: `else` больше не уходит на свою строку (`} else {`,
//! цепочка - `} else if c {`), а перечисление печатается по варианту на строке вместо
//! одной строки.

use takt_lang::format::format_source;

/// Форматирует исходник и возвращает результат.
fn fmt(source: &str) -> String {
    format_source(source).expect("форматтер обязан принять фикстуру")
}

/// Оборачивает тело в минимальную модель - `if` живёт только внутри блока.
fn in_body(body: &str) -> String {
    format!(
        "model P {{\n    var x: u8 := 0;\n    start S {{\n        always {{\n{body}\n        }}\n    }}\n}}\n"
    )
}

/// Разбирается ли напечатанное обратно (круговой рейс).
///
/// Главный вопрос для канона печати: форматтер пишет файлы на месте, и запись, которой
/// грамматика не принимает, уничтожает исходник.
fn reparses(text: &str) -> bool {
    takt_lang::parse(text, 0).is_ok()
}

// ----------------------------- K&R `else` -----------------------------

#[test]
fn simple_else_shares_the_closing_brace_line() {
    let out = fmt(&in_body("if x > 3 {\nx := 1;\n}\nelse {\nx := 2;\n}"));
    assert!(
        out.contains("} else {"),
        "ожидался K&R `}} else {{`:\n{out}"
    );
    assert!(
        !out.lines().any(|l| l.trim() == "else"),
        "`else` не должен занимать отдельную строку:\n{out}"
    );
    assert!(reparses(&out), "напечатанное не разбирается:\n{out}");
}

#[test]
fn else_if_chain_is_flat() {
    let out = fmt(&in_body(
        "if x > 3 {\nx := 1;\n}\nelse if x > 2 {\nx := 2;\n}\nelse if x > 1 {\nx := 3;\n}\nelse {\nx := 4;\n}",
    ));
    assert!(out.contains("} else if x > 2 {"), "{out}");
    assert!(out.contains("} else if x > 1 {"), "{out}");
    assert!(out.contains("} else {"), "{out}");
    // Прежняя печать давала три строки: `}` / `else` / `if ...`.
    assert!(
        !out.lines().any(|l| l.trim() == "else"),
        "цепочка обязана быть плоской:\n{out}"
    );
    assert!(
        !out.lines().any(|l| l.trim().starts_with("if x > 2")),
        "заголовок звена уехал на свою строку:\n{out}"
    );
    assert!(reparses(&out), "{out}");
}

#[test]
fn empty_bodies_stay_on_one_line() {
    let out = fmt(&in_body("if x > 0 {\n}\nelse {\n}"));
    assert!(
        out.contains("            if x > 0 {} else {}\n"),
        "пустые тела обязаны остаться одной строкой:\n{out}"
    );
    // Круговой рейс: `if x {} else {}` - запись, которую грамматика принимает.
    assert!(reparses(&out), "{out}");
}

#[test]
fn else_keeps_the_depth_of_its_if() {
    let out = fmt(&in_body("if x > 3 {\nx := 1;\n}\nelse {\nx := 2;\n}"));
    assert!(
        out.contains("\n            } else {\n"),
        "склейка сбила отступ закрывающей скобки:\n{out}"
    );
}

#[test]
fn comment_between_brace_and_else_survives() {
    let src = in_body("if x > 3 {\nx := 1;\n}\n// пояснение к иначе\nelse {\nx := 2;\n}");
    let once = fmt(&src);
    assert!(
        once.contains("// пояснение к иначе"),
        "комментарий потерян (требование R2):\n{once}"
    );
    // Место комментария наследуется от прежнего поведения; проверяем сохранность и
    // Стабильность - второй прогон обязан дать то же самое.
    assert_eq!(fmt(&once), once, "печать не идемпотентна:\n{once}");
}

#[test]
fn chain_is_idempotent() {
    let once = fmt(&in_body(
        "if x > 3 {\nx := 1;\n}\nelse if x > 2 {\nx := 2;\n}\nelse {\nx := 3;\n}",
    ));
    assert_eq!(fmt(&once), once, "печать цепочки не идемпотентна:\n{once}");
}

#[test]
fn non_block_else_is_wrapped_and_reparses() {
    // Грамматика допускает не-блочную ветку (`else x := 2;`); канон оборачивает её в
    // скобки. Страховка того, что пустой заголовок (`head = ""`) не сломал эту ветку
    // `block_with_head`.
    let out = fmt(&in_body("if x > 3 {\nx := 1;\n}\nelse x := 2;"));
    assert!(out.contains("} else {"), "{out}");
    assert!(reparses(&out), "{out}");
}

// ------------------------- перечисление -------------------------

#[test]
fn enum_prints_one_variant_per_line() {
    let out = fmt("enum Mode { Auto = 0, Manual = 1, Emergency }\n");
    assert_eq!(
        out, "enum Mode {\n    Auto = 0,\n    Manual = 1,\n    Emergency\n}\n",
        "перечисление обязано печататься по варианту на строке"
    );
}

#[test]
fn last_variant_has_no_trailing_comma() {
    let out = fmt("enum Mode { Auto, Manual }\n");
    assert!(
        !out.contains(",\n}"),
        "висячая запятая: грамматика `CommaOne<EnumVariant>` её не принимает:\n{out}"
    );
    // Главная проверка - не косметика, а разбор: напечатав запятую, форматтер сделал бы
    // файл неразбираемым.
    assert!(reparses(&out), "напечатанное не разбирается:\n{out}");
}

#[test]
fn nested_enum_keeps_indent() {
    let out = fmt("model P {\n    enum Inner { A, B = 5 }\n    start S;\n}\n");
    assert!(out.contains("\n    enum Inner {\n"), "{out}");
    assert!(out.contains("\n        A,\n"), "{out}");
    assert!(out.contains("\n        B = 5\n"), "{out}");
    assert!(out.contains("\n    }\n"), "{out}");
}

#[test]
fn trailing_comment_stays_with_the_closing_brace() {
    // Единственный тест против "приклеить комментарий к заголовку": `loc`
    // перечисления покрывает запись целиком, и в однострочном исходнике этот
    // комментарий стоит на той же строке, что и каждый вариант.
    let once = fmt("enum Mode { Auto, Manual } // вид работы\n");
    assert!(
        once.contains("} // вид работы"),
        "комментарий обо всём перечислении уехал внутрь:\n{once}"
    );
    assert_eq!(fmt(&once), once, "печать не идемпотентна:\n{once}");
}

#[test]
fn variant_comments_keep_their_place() {
    let once = fmt("enum Mode {\n    // самоход\n    Auto,\n    Manual // руками\n}\n");
    assert!(once.contains("    // самоход\n    Auto,"), "{once}");
    assert!(once.contains("    Manual // руками"), "{once}");
    assert_eq!(fmt(&once), once, "печать не идемпотентна:\n{once}");
}

#[test]
fn negative_variant_value_survives() {
    // Форма из корпуса: `takt-sim/tests/data/eval/conformance_neg_enum.takt`.
    let out = fmt("enum Level { Low = -5, Mid = 0 }\n");
    assert!(out.contains("    Low = -5,"), "{out}");
    assert!(reparses(&out), "{out}");
}

// ------------------------- структура -------------------------
//
// Тот же класс, что у перечисления: печать шла целиком через `line`, минуя привязку
// комментариев. Теста фикстурные по той же причине - хвостовых комментариев у
// структуры в корпусе из 308 файлов **ноль**.

#[test]
fn struct_prints_one_field_per_line() {
    let out = fmt("struct Trip { origin: u8, dest: u8 }\n");
    assert_eq!(
        out, "struct Trip {\n    origin: u8,\n    dest: u8\n}\n",
        "структура обязана печататься по полю на строке"
    );
}

#[test]
fn struct_trailing_comment_stays_with_the_closing_brace() {
    // Симптом дефекта: комментарий уезжал строкой ниже `}` и на следующем прогоне
    // доставался уже следующему элементу.
    let once = fmt("struct Trip { origin: u8, dest: u8 }   // структура\n");
    assert!(
        once.contains("} // структура"),
        "комментарий обо всей структуре оторвался от `}}`:\n{once}"
    );
    assert!(
        !once.lines().any(|l| l.trim() == "// структура"),
        "комментарий занял отдельную строку — он привяжется к чужому узлу:\n{once}"
    );
    assert_eq!(fmt(&once), once, "печать не идемпотентна:\n{once}");
}

#[test]
fn struct_field_comments_keep_their_place() {
    let once = fmt("struct Trip {\n    // откуда\n    origin: u8,\n    dest: u8 // куда\n}\n");
    assert!(once.contains("    // откуда\n    origin: u8,"), "{once}");
    assert!(once.contains("    dest: u8 // куда"), "{once}");
    assert_eq!(fmt(&once), once, "печать не идемпотентна:\n{once}");
}

#[test]
fn struct_comment_on_the_last_body_line_stays_inside() {
    // Комментарий последней строкой тела: следующего поля нет, выдать его обязана
    // `comments_before` - иначе он вылезет за `}`.
    let once = fmt("struct Trip {\n    origin: u8\n    // сюда добавить назначение\n}\n");
    let inside = once.find("// сюда").expect("комментарий потерян");
    let brace = once.find("\n}").expect("нет закрывающей скобки");
    assert!(inside < brace, "комментарий вылез за `}}`:\n{once}");
    assert_eq!(fmt(&once), once, "печать не идемпотентна:\n{once}");
}

#[test]
fn struct_last_field_has_no_trailing_comma() {
    let out = fmt("struct Trip { origin: u8, dest: u8 }\n");
    assert!(
        !out.contains(",\n}"),
        "висячая запятая: грамматика `Comma<StructField>` её не принимает:\n{out}"
    );
    // Главная проверка - разбор: `fmt` пишет файлы на месте.
    assert!(reparses(&out), "напечатанное не разбирается:\n{out}");
}

#[test]
fn nested_struct_keeps_indent() {
    let out = fmt("model P {\n    struct Point { x: u8, y: u8 }\n    start S;\n}\n");
    assert!(out.contains("\n    struct Point {\n"), "{out}");
    assert!(out.contains("\n        x: u8,\n"), "{out}");
    assert!(out.contains("\n        y: u8\n"), "{out}");
    assert!(out.contains("\n    }\n"), "{out}");
}

#[test]
fn empty_struct_keeps_its_comment() {
    // Пустая запись у структуры законна (`Comma<StructField>`), в отличие от
    // перечисления, - и печатается прежними двумя строками; проверяем ровно то, что
    // комментарий при этом остаётся при `}`.
    let once = fmt("struct Empty {} // задел\n");
    assert!(once.contains("} // задел"), "{once}");
    assert!(reparses(&once), "напечатанное не разбирается:\n{once}");
    assert_eq!(fmt(&once), once, "печать не идемпотентна:\n{once}");
}
