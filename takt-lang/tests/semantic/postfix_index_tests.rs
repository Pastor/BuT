//! Индексация и срез применимы к выражению, а не только к имени.
//!
//! # Что проверяем
//!
//! Обе цепочки в обе стороны, чтение и запись, срез над полем - и то, что
//! прежние диагностики (`SE-030`, `SE-117`, `SE-028`) остались на месте и
//! **называют предмет**.

use takt_lang::diagnostics::Diagnostic;
use takt_lang::semantic::tree::construct_model;

fn build(source: &str) -> Result<(), Diagnostic> {
    let (ast, _) = takt_lang::parse(source, 0).expect("разбор");
    construct_model(&ast, None, &[]).map(|_| ())
}

fn parse_fails(source: &str) -> bool {
    takt_lang::parse(source, 0).is_err()
}

fn code_of(source: &str) -> Option<String> {
    build(source).err().and_then(|d| d.code)
}

const STRUCT_WITH_ARRAY: &str = "struct Buf { data: [u8; 2], n: u8 } \
                                 var b: Buf := {{7, 9}, 3}; out o: u8 at 0x100; ";

/// Чтение элемента поля-массива.
#[test]
fn index_of_struct_field_is_parsed() {
    assert!(
        !parse_fails(&format!(
            "{STRUCT_WITH_ARRAY} start Run {{ always {{ o := b.data[1]; }} next Done; }} state Done {{ }}"
        )),
        "`b.data[1]` обязан разбираться: `[` — постфикс над выражением"
    );
    build(&format!(
        "{STRUCT_WITH_ARRAY} start Run {{ always {{ o := b.data[1]; }} next Done; }} state Done {{ }}"
    ))
    .expect("семантика принимает индексацию поля");
}

/// Запись в элемент поля-массива - то же место, что и чтение.
#[test]
fn write_into_struct_field_element_is_parsed() {
    build(&format!(
        "{STRUCT_WITH_ARRAY} start Run {{ always {{ b.data[0] := 5; o := b.data[0]; }} next Done; }} state Done {{ }}"
    ))
    .expect("запись в элемент поля — законное место");
}

/// Индексация поля в условии: у условий своё дерево, и правка нужна там тоже.
#[test]
fn index_of_struct_field_works_in_condition() {
    build(&format!(
        "{STRUCT_WITH_ARRAY} start Run {{ always {{ o := 1; }} ref Done: b.data[1] > 3; }} state Done {{ }}"
    ))
    .expect("условие принимает индексацию поля");
}

/// Срез над полем - тот же постфикс.
///
/// Срез пришлось перевести на выражение **вместе** с индексацией: иначе LR(1) не
/// различает два правила, начинающиеся с `Identifier [`, и грамматика перестаёт
/// строиться.
#[test]
fn slice_of_struct_field_is_parsed() {
    build(&format!(
        "{STRUCT_WITH_ARRAY} var part: [u8; 2] := {{0, 0}}; \
         start Run {{ always {{ part := b.data[0:2]; o := part[1]; }} next Done; }} state Done {{ }}"
    ))
    .expect("срез над полем структуры разбирается и принимается");
}

/// **Контрольный вход:** обратная цепочка работала и раньше - и работает.
#[test]
fn field_of_array_element_still_works() {
    build(
        "struct P { x: u8, y: u8 } var ps: [P; 2] := {{1, 2}, {3, 4}}; out o: u8 at 0x100; \
         start Run { always { o := ps[1].x; } next Done; } state Done { }",
    )
    .expect("`ps[1].x` — прежняя форма, она не должна пострадать");
}

/// Двойная индексация: элемент массива внутри поля, затем поле элемента.
#[test]
fn chained_postfix_operations_compose() {
    build(
        "struct P { x: u8 } struct Holder { items: [P; 2] } \
         var h: Holder := {{{1}, {2}}}; out o: u8 at 0x100; \
         start Run { always { o := h.items[1].x; } next Done; } state Done { }",
    )
    .expect("цепочка `.items[1].x` разбирается слева направо");
}

// -- Контрпримеры: прежние диагностики на месте -------------------------------

/// **К1:** индексация не массива - по-прежнему `SE-030`, и сообщение называет имя.
#[test]
fn index_of_non_array_is_still_se030_with_name() {
    let err = build("var flag: bit := false; var x: bit := flag[0];")
        .expect_err("индексация bit-переменной — ошибка");
    assert_eq!(err.code.as_deref(), Some("SE-030"), "{err:?}");
    assert!(
        err.message.contains("flag"),
        "диагностика обязана называть предмет, а не «значение»: {}",
        err.message
    );
}

/// **К2:** индексация неизвестного имени в условии - `SE-117`.
///
/// База теперь разрешается общим путём, и неразрешённое имя приходит узлом
/// `Unresolved`; без явной ветви на него проверка бы **молчала**.
#[test]
fn index_of_unknown_name_in_condition_is_still_se117() {
    assert_eq!(
        code_of("cond Bad = arr[0]; var v: u8 := 0; start Run { always { v := 1; } }").as_deref(),
        Some("SE-117"),
        "неизвестное имя индексировать нельзя"
    );
}

/// **К3:** выход за границы литеральным индексом - по-прежнему `SE-028`.
#[test]
fn out_of_range_literal_index_is_still_se028() {
    assert_eq!(
        code_of("var a: [u8; 2] := {1, 2}; var x: u8 := a[5]; start Run { always { } }").as_deref(),
        Some("SE-028"),
        "статическая проверка границ осталась"
    );
}

/// **К4:** та же проверка границ работает и для поля структуры.
#[test]
fn out_of_range_index_of_field_is_se028() {
    assert_eq!(
        code_of(&format!(
            "{STRUCT_WITH_ARRAY} start Run {{ always {{ o := b.data[5]; }} next Done; }} state Done {{ }}"
        ))
        .as_deref(),
        Some("SE-028"),
        "тип базы выводится по цепочке, значит и границы проверяются"
    );
}
