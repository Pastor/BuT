//! Разряд за объявленной шириной значения - `SE-125`.
//!
//! #
//!
//! Объявленная ширина - **контракт**: разряд за ней есть ошибка, и судит её семантика -
//! то есть до генерации, одинаково для всех восьми целей. Раскладка по словам - деталь
//! представления, о которой автор знать не обязан.
//!
//! **Граница - не "переменный индекс", как предполагала **: такой формы в языке нет
//! (`w.idx` - доступ к полю с именем `idx`, замер 2026-08-23). Пропускается выражение,
//! **чей тип не выводится** (например, результат вызова): проверка консервативна, и там
//! `SIM-011` эталона достижим.

use takt_lang::generator::GenerateOptions;

/// Код диагностики, если модель отвергнута семантикой.
fn code_of(src: &str) -> Option<String> {
    let dir = std::env::temp_dir().join(format!(
        "takt_0394_{}_{}",
        std::process::id(),
        std::thread::current()
            .name()
            .unwrap_or("t")
            .replace(':', "_")
    ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    let result = takt_lang::compile_to_c(
        "bitw",
        src,
        dir.to_str().expect("путь"),
        &[],
        &GenerateOptions::default(),
    );
    let _ = std::fs::remove_dir_all(&dir);
    result.err().and_then(|d| d.code)
}

fn model(body: &str) -> String {
    format!(
        "var w: [bit;96];\nvar n: u8 := 0;\nvar o: u8 := 0;\nout probe: u8 at 0;\n\
         start Run {{\n    always {{\n{body}\n        probe := o;\n    }}\n    ref Run;\n}}\n"
    )
}

/// Предмет: разряд за объявленной шириной бит-вектора отвергается.
#[test]
fn bit_beyond_declared_width_is_refused() {
    assert_eq!(
        code_of(&model("        w.100 := 1;\n        o := 1;")).as_deref(),
        Some("SE-125"),
        "разряд 100 при объявленных 96 обязан отвергаться"
    );
}

/// **Контроль:** последний законный разряд принимается.
///
/// Без него правка читалась бы как "разряды вектора запрещены".
#[test]
fn last_declared_bit_is_accepted() {
    assert_eq!(
        code_of(&model("        w.95 := 1;\n        o := 1;")),
        None,
        "разряд 95 при объявленных 96 законен"
    );
}

/// Правило действует и на **целое**: у `u8` разрядов восемь.
#[test]
fn bit_beyond_integer_width_is_refused() {
    assert_eq!(
        code_of(&model("        n.8 := 1;\n        o := 1;")).as_deref(),
        Some("SE-125"),
        "разряд 8 у `u8` обязан отвергаться"
    );
    assert_eq!(
        code_of(&model("        n.7 := 1;\n        o := 1;")),
        None,
        "разряд 7 у `u8` законен"
    );
}

/// Правило действует **в условии** тоже.
///
/// Печатников у условий и выражений два, значит и проверка обязана стоять в обоих
/// обходах: правка одного чинит половину входов.
#[test]
fn bit_beyond_width_is_refused_in_conditions() {
    let src = "var n: u8 := 0;\nvar o: u8 := 0;\nout probe: u8 at 0;\n\
         start Run {\n    always {{ o := 1; probe := o; }}\n    ref Done: n.8 = 1;\n}\n\
         state Done {{ }}\n"
        .replace("{{", "{")
        .replace("}}", "}");
    assert_eq!(
        code_of(&src).as_deref(),
        Some("SE-125"),
        "разряд за шириной в условии обязан отвергаться"
    );
}

/// **Контроль границы:** переменный индекс остаётся за исполнением.
///
/// Названная граница, а не пропуск: значение индекса при компиляции неизвестно, а
/// проверку в времени выполнения цели не ставят.
#[test]
fn variable_bit_index_is_left_to_runtime() {
    let src = "var n: u8 := 0;\nvar i: u8 := 0;\nvar o: u8 := 0;\nout probe: u8 at 0;\n\
         start Run { always { o := n; i := i + 1; probe := o; } ref Run; }\n";
    assert_eq!(code_of(src), None, "переменный индекс проверке не подлежит");
}

/// Индекс по упакованному вектору - Разряд, и в условии тоже.
///
/// Правило одно с выражениями: условие и тело говорят об одном месте. Разойдись они -
/// `ref Done: src[3];` печаталось бы целями индексацией значения, которое значением не
/// является: `cc` отвечает "subscripted value is not an array", `iec2c` - "Array
/// variable not declared", и обе жалобы приходят при нулевом коде возврата `taktc`.
#[test]
fn a_bit_vector_index_in_a_condition_is_a_bit() {
    let source = "var buf: [bit; 8] := 0;\ncond C = buf[3];\nstart S { always { buf.0 := 1; } }\n";
    let (ast, _) = takt_lang::parse(source, 0).expect("разбор");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("дерево");
    let printed = format!("{:?}", model.borrow().conditions);
    assert!(
        printed.contains("BitAccess"),
        "индекс по бит-вектору в условии обязан сводиться к разряду:\n{printed}"
    );
    assert!(
        !printed.contains("ArraySubscript"),
        "индексного узла в условии остаться не должно:\n{printed}"
    );
}
