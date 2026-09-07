//! Имя типа видно во всём файле; цикл структур - `SE-124`.

use takt_lang::diagnostics::Diagnostic;
use takt_lang::semantic::tree::construct_model;

/// Строит дерево из исходника: `Ok` - принято, `Err` - диагностика.
fn build(source: &str) -> Result<(), Diagnostic> {
    let (ast, _) = takt_lang::parse(source, 0).expect("разбор");
    construct_model(&ast, None, &[]).map(|_| ())
}

fn code_of(source: &str) -> Option<String> {
    build(source).err().and_then(|d| d.code)
}

// -- Ссылка вперёд: пять форм, все законны ------------------------------------

/// Поле-структура ссылается на структуру, объявленную ниже.
#[test]
fn struct_field_may_reference_struct_declared_below() {
    build("struct Aa { b: Bb, n: u8 } struct Bb { m: u8 } var v: Aa; start Run { always { } }")
        .expect("поле вправе сослаться на структуру, объявленную ниже");
}

/// Поле-перечисление ссылается на перечисление, объявленное ниже.
#[test]
fn struct_field_may_reference_enum_declared_below() {
    build("struct Cfg { m: Mode, n: u8 } enum Mode { Idle, Work } var c: Cfg; start Run { always { } }")
        .expect("поле вправе сослаться на перечисление, объявленное ниже");
}

/// Элемент массива в поле ссылается вперёд.
///
/// Отдельный тест: разрешение спускается в элемент массива, и без спуска форма
/// просочилась бы мимо правки.
#[test]
fn array_field_element_may_reference_type_declared_below() {
    build("struct Holder { cells: [Cell; 2], n: u8 } struct Cell { v: u8 } var h: Holder; start Run { always { } }")
        .expect("элемент массива вправе сослаться на тип, объявленный ниже");
}

/// Псевдоним ссылается на структуру, объявленную ниже.
#[test]
fn alias_may_reference_struct_declared_below() {
    build(
        "type Alias = Point; struct Point { x: u8, y: u8 } var p: Alias; start Run { always { } }",
    )
    .expect("псевдоним вправе сослаться на структуру, объявленную ниже");
}

/// Псевдоним ссылается на псевдоним, объявленный ниже.
///
/// Значение псевдонима - уже разрешённый тип, поэтому одного прохода мало: проверка
/// проверяет разрешение **до неподвижной точки**.
#[test]
fn alias_may_reference_alias_declared_below() {
    build("type A = B; type B = u8; var x: A := 3; start Run { always { } }")
        .expect("псевдоним вправе сослаться на псевдоним, объявленный ниже");
}

/// Цепочка псевдонимов длиной три, объявленная задом наперёд.
///
/// Тест на **две** ссылки вперёд подряд, и он не декоративен: мутация "один виток
/// вместо неподвижной точки" на паре `A -> B` **не ловится** - финальная попытка
/// разрешить остаток служит вторым проходом. Цепочка из трёх требует настоящего
/// повторения.
#[test]
fn alias_chain_of_three_resolves() {
    build("type A = B; type B = C; type C = u8; var x: A := 3; start Run { always { } }")
        .expect("цепочка псевдонимов разрешается до неподвижной точки");
}

// -- Контрпримеры: что осталось ошибкой ---------------------------------------

/// **Контрпример:** несуществующее имя типа по-прежнему `SE-034`.
///
/// Без этой проверки "починка", регистрирующая любое имя, прошла бы тесты выше и молча
/// приняла бы опечатку.
#[test]
fn unknown_type_name_is_still_an_error() {
    assert_eq!(
        code_of("struct Aa { b: Nope, n: u8 } var v: Aa; start Run { always { } }").as_deref(),
        Some("SE-034"),
        "имя, которого нет ни выше, ни ниже, обязано оставаться ошибкой"
    );
}

/// **Контрпример:** дубль имени типа по-прежнему `SE-108`.
///
/// Занятие имени переехало в предпроход, и второй вызов `claim_type_name` в основном
/// цикле дал бы **ложную** `SE-108` на каждой структуре; этот тест вместе с
/// положительными выше отделяет верное поведение от такой поломки.
#[test]
fn duplicate_type_name_is_still_an_error() {
    assert_eq!(
        code_of("struct S { a: u8 } struct S { b: u8 } var v: S; start Run { always { } }")
            .as_deref(),
        Some("SE-108"),
        "имя типа занимается один раз"
    );
}

/// **Контрпример:** имя встроенного типа по-прежнему `SE-107`.
#[test]
fn builtin_type_name_is_still_reserved() {
    assert_eq!(
        code_of("struct u8 { a: u16 } start Run { always { } }").as_deref(),
        Some("SE-107"),
        "встроенное имя занять нельзя"
    );
}

// -- Цикл структур: SE-124 ----------------------------------------------------

/// Взаимный цикл двух структур.
#[test]
fn mutual_struct_cycle_is_rejected() {
    assert_eq!(
        code_of("struct Aa { b: Bb, n: u8 } struct Bb { a: Aa, m: u8 } var v: Aa; start Run { always { } }")
            .as_deref(),
        Some("SE-124"),
        "структура, содержащая себя через цепочку полей, непредставима"
    );
}

/// Самоссылка **через элемент массива** - тот же цикл.
#[test]
fn self_reference_through_array_is_rejected() {
    assert_eq!(
        code_of("struct Node { kids: [Node; 2], n: u8 } var v: Node; start Run { always { } }")
            .as_deref(),
        Some("SE-124"),
        "обход обязан спускаться в элемент массива"
    );
}

/// Цикл, замкнутый **через псевдоним**.
#[test]
fn cycle_through_alias_is_rejected() {
    assert_eq!(
        code_of("struct P { r: Ref, n: u8 } type Ref = Q; struct Q { p: P, m: u8 } var v: P; start Run { always { } }")
            .as_deref(),
        Some("SE-124"),
        "узлами графа служат и псевдонимы: иначе класс просочится"
    );
}

/// **Контрпример:** цепочка без цикла законна.
///
/// Без него `SE-124` могла бы срабатывать на любой вложенности - и правка читалась бы
/// как "вложенные структуры запрещены".
#[test]
fn nested_chain_without_cycle_is_accepted() {
    build("struct C3 { v: u8 } struct C2 { c: C3, n: u8 } struct C1 { c: C2, n: u8 } var v: C1; start Run { always { } }")
        .expect("цепочка вложенных структур без цикла законна");
}

/// **Контрпример:** цикл среди одних псевдонимов судит `SE-039`, а не `SE-124`.
///
/// Эта проверка стоит **раньше** и гарантирует завершение разрешения псевдонимов до
/// неподвижной точки; подмена кода означала бы, что порядок проверок нарушен.
#[test]
fn pure_alias_cycle_is_still_se039() {
    assert_eq!(
        code_of("type A = B; type B = A; var x: A; start Run { always { } }").as_deref(),
        Some("SE-039"),
        "чисто псевдонимный цикл остаётся за Ce16"
    );
}
