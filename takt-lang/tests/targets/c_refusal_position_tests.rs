//! Координата отказа цели `c` - место употребления.
//!
//! # Что здесь проверяется
//!
//! У выражения нет позиции **употребления**: `ExpressionNode::loc()` выводит её из
//! объявлений операндов (решение 0056 - не тащить координату через ~40 вариантов ради
//! одного потребителя). Поэтому отказ цели указывал на объявление:
//!
//! | Вход | Координата до фичи | После |
//! |---|---|---|
//! | `res := mem[1:2];` в строке 7 (`mem` объявлена в строке 1) | **`1:1`** | `7:9` |
//!
//! **Это ложь, а не пустота:** координата выглядит настоящей, и автор идёт читать
//! строку объявления. Тот же класс, что 0264 и 0276.
//!
//! Позицию несёт оператор (`StatementNode::Expression`); цель берёт её из общего
//! носителя `generator::site::StatementSite`, не меняя ни `ExpressionNode`, ни
//! сигнатуры рекурсивных печатников.
//!
//! **Границы объёма названы:** цели `rust`, `st`, `sv` печатают отказ с
//! `Location::Codegen`, то есть **без** координаты - это честная пустота, а не ложь; их
//! случай вынесен кандидатом. Отказ `CC-023` (узел не прошёл понижение) недостижим из
//! корректной программы и координаты тоже не меняет.

use std::path::PathBuf;
use takt_lang::diagnostics::Location;
use takt_lang::generator::GenerateOptions;

/// Срез массива в скалярный приёмник: `res: u8` элементов не имеет, поэтому
/// поэлементная форма неприменима и цель отвечает `CC-022`. Эталон такой вход тоже не
/// исполняет (`SIM-006` "значение массив нельзя привести к типу u8"), то есть пример
/// остаётся непереводимым по существу.
const SLICE: &str = "var mem: [u8; 4] := { 0, 0, 0, 0 };\n\
                     var res: u8 := 0;\n\
                     \n\
                     start Run {\n\
                     \x20   always {\n\
                     \x20       mem[0] := 1;\n\
                     \x20       res := mem[1:2];\n\
                     \x20   }\n\
                     \x20   ref Run: res < 3;\n\
                     }\n";

fn build_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0277_{thread}_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание каталога");
    dir
}

/// Отказ цели `c` на исходнике.
fn refusal(tag: &str, source: &str) -> takt_lang::diagnostics::Diagnostic {
    let dir = build_dir(tag);
    takt_lang::compile_to_c(
        tag,
        source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .expect_err("вход обязан отвергаться")
}

/// Строка по смещению диагностики.
fn line_of(src: &str, diagnostic: &takt_lang::diagnostics::Diagnostic) -> usize {
    let Location::Source(_, start, _) = diagnostic.loc else {
        panic!("у отказа нет позиции в исходнике: {:?}", diagnostic.loc);
    };
    src[..start as usize].matches('\n').count() + 1
}

/// **T1.** Отказ указывает на строку употребления, а не объявления.
#[test]
fn refusal_points_at_the_usage_line() {
    let err = refusal("slice", SLICE);
    assert_eq!(err.code.as_deref(), Some("CC-022"), "код отказа");
    assert_eq!(
        line_of(SLICE, &err),
        7,
        "координата обязана указывать на строку `res := mem[1:2];`, а не на объявление `mem`"
    );
}

/// **T2.** Отказ вне оператора берёт свою координату, а не чужую.
///
/// Инициализатор объявления печатается до тела, оператор ещё не начат. До координаты у
/// такого отказа не было вовсе; теперь её даёт второй слой носителя - Объявление
/// (`site::enter_declaration`), то есть строка самой переменной. Проверка осталась о
/// том же: позиция последнего оператора (строка 5) сюда попасть не вправе - это была бы
/// ложь, от которой ушла 0308.
#[test]
fn refusal_outside_a_statement_takes_its_own_position() {
    const SCALAR_INIT: &str = "var mem: [u8; 4] := 0;\n\
                               var n: u8 := 0;\n\
                               \n\
                               start Run {\n\
                               \x20   always { n := mem[0]; }\n\
                               \x20   ref Run: n < 3;\n\
                               }\n";
    let err = refusal("scalar_init", SCALAR_INIT);
    assert_eq!(err.code.as_deref(), Some("CC-017"));
    assert_eq!(
        line_of(SCALAR_INIT, &err),
        1,
        "координата обязана указывать на объявление `mem`, а не на оператор тела"
    );
}

/// **T3.** Позиция не "залипает": следующий оператор объявляет своё место.
///
/// Носитель - изменяемое состояние на время генерации, и это его главный риск: не
/// обнови печатник операторов позицию, все последующие отказы указывали бы на первый
/// оператор модели.
#[test]
fn position_follows_the_current_statement() {
    const LATER: &str = "var mem: [u8; 4] := { 0, 0, 0, 0 };\n\
                         var res: u8 := 0;\n\
                         \n\
                         start Run {\n\
                         \x20   always {\n\
                         \x20       res := 1;\n\
                         \x20       res := 2;\n\
                         \x20       res := 3;\n\
                         \x20       res := mem[1:2];\n\
                         \x20   }\n\
                         \x20   ref Run: res < 3;\n\
                         }\n";
    let err = refusal("later", LATER);
    assert_eq!(
        line_of(LATER, &err),
        9,
        "координата обязана указывать на последний оператор, а не на первый"
    );
}

/// **T4.** Слой объявления снимается: тело чужой координаты не занимает.
///
/// Второй слой носителя - главный риск: переживи он печать объявлений, отказ в теле
/// получил бы координату последнего объявления, то есть указал бы не туда. Проверяется
/// на входе, где объявление стоит перед телом, а отказ рождается в теле: координата
/// обязана быть строкой оператора, а не строкой объявления.
#[test]
fn declaration_layer_does_not_leak_into_the_body() {
    const AFTER_DECL: &str = "var mem: [u8; 4] := { 0, 0, 0, 0 };\n\
                              var res: u8 := 0;\n\
                              \n\
                              start Run {\n\
                              \x20   always {\n\
                              \x20       res := mem[1:2];\n\
                              \x20   }\n\
                              \x20   ref Run: res < 3;\n\
                              }\n";
    let err = refusal("after_decl", AFTER_DECL);
    assert_eq!(err.code.as_deref(), Some("CC-022"), "код отказа");
    assert_eq!(
        line_of(AFTER_DECL, &err),
        6,
        "координата обязана указывать на строку оператора, а не на объявление `mem`"
    );
}

/// **T5.** Отказ о ячейке называет место обращения.
///
/// Цель здесь - `st`, а не `c`, и это не прихоть: у `c` отказ приходит из печатника
/// условий, где место уже объявлено слоем ребра, - координата была бы верной и без
/// правки. Цель `st` отвергает модель до первой строки вывода, когда носитель позиции
/// пуст: единственный источник координаты - `AnonPortAccess::loc`. Мутация "ячейка без
/// места" роняет именно этот тест.
#[test]
fn anon_cell_refusal_names_the_access_site() {
    const CELL: &str = "var n: u8 := 0;\n\
                        \n\
                        start Run {\n\
                        \x20   always {\n\
                        \x20       n := n + 1;\n\
                        \x20   }\n\
                        \x20   ref Done: #0x300.0 = 1;\n\
                        }\n\
                        state Done;\n";
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!(
            "takt_0470_anon_{}",
            std::thread::current()
                .name()
                .unwrap_or("main")
                .replace(':', "_")
        ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог теста");
    let err = takt_lang::compile_to_st(
        "anon_cell",
        CELL,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect_err("цель `st` обязана отказать: адресного пространства она не знает");
    assert_eq!(err.code.as_deref(), Some("ST-018"), "код отказа");
    assert_eq!(
        line_of(CELL, &err),
        7,
        "координата обязана указывать на строку обращения к ячейке"
    );
}

/// **T6.** Заголовок цикла без инициализатора называет своё место.
///
/// Позиция у `StatementNode::For` появилась своя - прежде её брали у объявления в
/// заголовке, а `for ; i < 3; ...` такого объявления не имеет и координаты не получал
/// вовсе.
#[test]
fn loop_header_without_init_names_its_place() {
    const LOOP: &str = "out o: u8 at 0;\n\
                        var i: u8 := 0;\n\
                        \n\
                        start Run {\n\
                        \x20   always {\n\
                        \x20       for ; i < 3; i := i + 1 {\n\
                        \x20           o := i;\n\
                        \x20       }\n\
                        \x20   }\n\
                        \x20   ref Run: i < 1;\n\
                        }\n";
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!(
            "takt_0471_loop_{}",
            std::thread::current()
                .name()
                .unwrap_or("main")
                .replace(':', "_")
        ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог теста");
    let err = takt_lang::compile_to_sv(
        "loop_probe",
        LOOP,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect_err("цель `sv` разворачивает только статический цикл");
    assert_eq!(err.code.as_deref(), Some("SV-002"), "код отказа");
    assert_eq!(
        line_of(LOOP, &err),
        6,
        "координата обязана указывать на заголовок цикла"
    );
}

/// **T7.** Формула в теле блока называет своё место.
///
/// Координата была в АСД всегда, а при понижении терялась: цели `rust` и `st` печатали
/// предупреждение без места.
///
/// Проверяется темпоральная форма: охранная с предупреждения не вызывает вовсе - она
/// печатается `assert!` (её место проверяет набор `formula_in_body_tests`).
#[test]
fn formula_in_body_names_its_place() {
    const FORMULA: &str = "out o: u8 at 0;\n\
                           var level: u8 := 0;\n\
                           \n\
                           start Run {\n\
                           \x20   always {\n\
                           \x20       : [LTL] F Done;\n\
                           \x20       level := level + 1;\n\
                           \x20       o := level;\n\
                           \x20   }\n\
                           \x20   ref Done: level > 2;\n\
                           }\n\
                           state Done;\n";
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!(
            "takt_0471_formula_{}",
            std::thread::current()
                .name()
                .unwrap_or("main")
                .replace(':', "_")
        ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог теста");
    let warnings = takt_lang::compile_to_rust(
        "formula_probe",
        FORMULA,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("цель `rust` переводит эту модель");
    let warning = warnings
        .iter()
        .find(|d| d.code.as_deref() == Some("RS-010"))
        .expect("предупреждение о формуле в теле");
    assert_eq!(
        line_of(FORMULA, warning),
        6,
        "координата обязана указывать на строку формулы"
    );
}
