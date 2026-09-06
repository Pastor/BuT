//! Индексация параметра-массива внутри функции.

use takt_lang::parse;
use takt_lang::semantic::tree::construct_model;

/// Строит дерево из исходника (или отдаёт код диагностики).
fn build(source: &str) -> Result<(), String> {
    let (ast, _) = parse(source, 0).map_err(|d| format!("{d:?}"))?;
    construct_model(&ast, None, &[])
        .map(|_| ())
        .map_err(|d| d.code.unwrap_or_default())
}

const INDEXED_PARAM: &str = "fn first(a: [u8;2]) -> u8 { return a[0]; }\n\
     var arr: [u8;2] := {7, 1};\nvar got: u8 := 0;\nout o: u8 at 0;\n\
     start Run { always { got := first(arr); o := got; } ref Run: got = 7; }\n";

/// Параметр-массив индексируется - дерево строится.
#[test]
fn indexed_array_parameter_resolves() {
    assert_eq!(build(INDEXED_PARAM), Ok(()));
}

/// Параметр-индекс тоже разрешается: список параметров доезжает и в индекс.
#[test]
fn parameter_used_as_index_resolves() {
    // Имя `at` брать нельзя: это ключевое слово языка (адрес порта), и отказ пришёл бы
    // от парсера, а не от предмета проверки.
    let source = "fn pick_at(a: [u8;2], i: u8) -> u8 { return a[i]; }\n\
         var arr: [u8;2] := {7, 1};\nvar got: u8 := 0;\nout o: u8 at 0;\n\
         start Run { always { got := pick_at(arr, 1); o := got; } ref Run: got = 1; }\n";
    assert_eq!(build(source), Ok(()));
}

/// **Контроль:** неизвестное имя по-прежнему `SE-003`.
///
/// Без него правка читалась бы как "индексация принимает любое имя".
#[test]
fn unknown_name_is_still_se003() {
    let source = "fn first(a: [u8;2]) -> u8 { return b[0]; }\n\
         var arr: [u8;2] := {7, 1};\nvar got: u8 := 0;\nout o: u8 at 0;\n\
         start Run { always { got := first(arr); o := got; } ref Run: got = 7; }\n";
    assert_eq!(build(source), Err("SE-003".to_string()));
}

/// **Контроль:** параметр не массив, а индексируется - `SE-030`.
#[test]
fn non_array_parameter_is_se030() {
    let source = "fn first(a: u8) -> u8 { return a[0]; }\n\
         var got: u8 := 0;\nout o: u8 at 0;\n\
         start Run { always { got := first(3); o := got; } ref Run: got = 3; }\n";
    assert_eq!(build(source), Err("SE-030".to_string()));
}
