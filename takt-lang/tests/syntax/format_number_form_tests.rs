//! Форма записи числа переживает форматирование.
//!
//! # Что здесь ловится
//!
//! Обе формы остаются собой, и `fmt` по-прежнему **идемпотентен**: печать по исходнику
//! не должна была этого нарушить.

use takt_lang::format::format_source;

/// Шестнадцатеричная запись сохраняется во всех позициях.
#[test]
fn hexadecimal_forms_survive_formatting() {
    let source = "\
model Wrap {
    const MASK: u8 := 0xF0;
    const DEC: u8 := 240;
    var k: u8 := 0;
    out led: u8 at 0x40000100;

    start Go {
        always {
            k := k + MASK;
            led := k;
        }
        ref Go: k < 0x30;
    }
}
start Main = Wrap;
";
    let formatted = format_source(source).expect("файл форматируется");
    for written in ["0xF0", "0x40000100", "0x30"] {
        assert!(
            formatted.contains(written),
            "запись '{written}' потеряна:\n{formatted}"
        );
    }
    // Десятичная запись тоже остаётся собой: канон не переводит её в hex.
    assert!(
        formatted.contains("const DEC: u8 := 240;"),
        "десятичная запись изменена:\n{formatted}"
    );
    // Ни одно значение не всплывает в чужой форме: десятичный адрес - верный признак
    // того, что запись потеряна.
    assert!(
        !formatted.contains("1073742080"),
        "адрес напечатан десятичным:\n{formatted}"
    );
}

/// Печать по исходнику не сломала идемпотентность.
#[test]
fn formatting_stays_idempotent() {
    let source = "model Wrap {\n  const MASK: u8 := 0xF0;\n  var k: u8 := 0;\n  out led: u8 at 0x40000100;\n  start Go { always { k := k + MASK; led := k; } ref Go: k < 0x30; }\n}\nstart Main = Wrap;\n";
    let once = format_source(source).expect("первый прогон");
    let twice = format_source(&once).expect("второй прогон");
    assert_eq!(once, twice, "fmt перестал быть идемпотентным");
    assert!(once.contains("0xF0") && once.contains("0x40000100"));
}

/// Разделитель разрядов - часть записи автора и тоже сохраняется.
#[test]
fn digit_separator_survives() {
    let source = "model Wrap {\n    const BIG: u32 := 1_000_000;\n    var k: u8 := 0;\n    out led: u8;\n    start Go {\n        always {\n            k := k + 1;\n            led := k;\n        }\n        ref Go: k < 5;\n    }\n}\nstart Main = Wrap;\n";
    let formatted = format_source(source).expect("файл форматируется");
    assert!(
        formatted.contains("1_000_000"),
        "разделитель разрядов потерян:\n{formatted}"
    );
}
