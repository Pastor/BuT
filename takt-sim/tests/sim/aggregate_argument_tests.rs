//! Агрегат как аргумент параметра.
//!
//! # Что здесь проверяется
//!
//! `Memory(prog := {9, 8, 7, 6})` отвергалось `SE-083`: константный вычислитель агрегат
//! не сворачивал, и "программа как аргумент" не выражалась вовсе.
//!
//! Снятие отказа **обнажило** неготовность цели `c`: она печатала `model->main.prog =
//! {9, 8, 7, 6};` - не выражение, а инициализатор объявления, и `cc` отвечал "expected
//! expression". Поэтому здесь сверяется **текст** вывода, а не факт компиляции:
//! элементы обязаны печататься поэлементно.

use takt_lang::generator::GenerateOptions;
use takt_sim::{TickResult, Value};

/// Модель с параметром-массивом; аргумент задаётся при инстанцировании.
const AGGREGATE: &str = "\
model Memory {
    parameter prog: [u8; 4] := {0, 0, 0, 0};
    var out_v: u8 := 0;
    start Run { always { out_v := prog[0]; } ref Run; }
}
start Main = Memory(prog := {9, 8, 7, 6});
";

/// Контр-пример: элемент агрегата - переменная, значение которой известно лишь в такте.
const NON_CONSTANT_ELEMENT: &str = "\
model Memory {
    parameter prog: [u8; 2] := {0, 0};
    var out_v: u8 := 0;
    start Run { always { out_v := prog[0]; } ref Run; }
}
var runtime: u8 := 3;
start Main = Memory(prog := {runtime, 1});
";

fn build(
    src: &str,
) -> Result<std::rc::Rc<std::cell::RefCell<takt_lang::semantic::ModelNode>>, String> {
    let (ast, _) = takt_lang::parse(src, 0).expect("разбор");
    takt_lang::semantic::tree::construct_model(&ast, None, &[])
        .map_err(|d| d.code.unwrap_or_default())
}

/// Порождает C и возвращает его текст.
fn generated_c(tag: &str, src: &str) -> String {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0209_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание каталога");
    takt_lang::compile_to_c(
        tag,
        src,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение C");
    std::fs::read_to_string(dir.join(format!("{tag}.c"))).expect("чтение .c")
}

/// **T1.** Семантика принимает агрегат в аргументе.
#[test]
fn aggregate_argument_is_accepted() {
    assert!(
        build(AGGREGATE).is_ok(),
        "агрегат в аргументе параметра обязан вычисляться"
    );
}

/// **T2.** Эталон видит именно заданные значения, а не умолчание модели.
///
/// Проверяется **значение**: потерянный аргумент дал бы `{0, 0, 0, 0}` - и прогон при
/// этом не упал бы.
#[test]
fn reference_sees_the_given_values() {
    let model = build(AGGREGATE).expect("семантика");
    let mut unit = takt_sim::build_unit(model).expect("построение юнита");
    let result = unit.tick();
    assert!(
        !matches!(result, TickResult::Failed(_)),
        "прогон не должен падать: {result:?}"
    );
    assert_eq!(
        unit.variable("out_v"),
        Some(Value::Number(9)),
        "первый элемент программы обязан доехать до эталона"
    );
    assert_eq!(
        unit.variable("prog"),
        Some(Value::Array(vec![
            Value::Number(9),
            Value::Number(8),
            Value::Number(7),
            Value::Number(6),
        ])),
        "аргумент обязан перекрыть умолчание целиком"
    );
}

/// **T3.** Цель `c` печатает агрегат поэлементно.
///
/// Присваивание агрегата в C не выражение: `p.prog = {9, 8, 7, 6};` отвергает `cc`
/// ("expected expression"). Проверяется текст, потому что "скомпилировалось" здесь
/// ничего не значит - вывод отвергает не наш инструмент, а чужой.
#[test]
fn c_target_assigns_aggregate_element_wise() {
    let text = generated_c("agg", AGGREGATE);
    for (i, value) in [9, 8, 7, 6].into_iter().enumerate() {
        let line = format!("prog[{i}] = {value};");
        assert!(
            text.contains(&line),
            "в порождённом C нет строки `{line}`:\n{text}"
        );
    }
    assert!(
        !text.contains("prog = {"),
        "агрегат печатается присваиванием — такой C не компилируется:\n{text}"
    );
}

/// **T4. Контр-пример.** Неконстантный элемент по-прежнему отвергается.
///
/// Без него правка была бы неотличима от "принимать любой агрегат": значение параметра
/// задаётся **при сборке**, и переменная времени работы им быть не может.
#[test]
fn non_constant_element_is_still_refused() {
    assert_eq!(build(NON_CONSTANT_ELEMENT), Err("SE-083".to_string()));
}

/// **T5.** Границы целей названы, а не забыты.
///
/// Тест пришпиливает состояние целей: научившаяся цель обязана покраснеть и потребовать
/// снять запись - это лучше, чем молчаливое расхождение.
///
/// Граница `sv` **сдвинулась**: агрегат массива печатается шаблоном присваивания
/// `'{...}`, и здесь он доезжает до ветви сброса. Поэтому проверяется не отказ, а
/// **значение** в выводе - "переводит" не должно означать "печатает что попало". Цель
/// `st` агрегат в этой позиции по-прежнему не печатает и отвечает своим кодом.
#[test]
fn st_refuses_and_sv_prints_the_aggregate() {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join("takt_0209_targets");
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание каталога");
    let path = dir.to_str().expect("путь в UTF-8");
    let options = GenerateOptions::default();
    let st = takt_lang::compile_to_st("agg", AGGREGATE, path, &[], &options)
        .expect_err("цель st пока не печатает агрегатный аргумент");
    assert_eq!(st.code.as_deref(), Some("ST-017"));

    takt_lang::compile_to_sv("agg", AGGREGATE, path, &[], &options)
        .expect("цель sv печатает агрегат массива с фичи 0309");
    let text = std::fs::read_to_string(dir.join("agg.sv")).expect("чтение модуля");
    assert!(
        text.contains("'{8'd9, 8'd8, 8'd7, 8'd6}"),
        "значения агрегата обязаны доехать до ветви сброса:\n{text}"
    );
}
