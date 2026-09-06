//! Начальное значение составного порта.
//!
//! # Что доказывает набор
//!
//! Записи HAL по тактам совпадают с эталоном - и по значению, и по номеру такта, на
//! котором они происходят. Проверяются все три агрегатные формы: массив, структура
//! (порядок полей значим) и перечисление (значение варианта обязано быть известно при
//! компиляции).

use std::path::{Path, PathBuf};
use std::process::Command;

use takt_lang::generator::GenerateOptions;
use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Value, build_unit};

const FIXTURE: &str = "tests/data/eval/conformance_port_aggregate_init.takt";
const UNIT: &str = "conformance_port_aggregate_init";
const TICKS: usize = 3;

/// Наблюдаемое: имя порта у эталона и перечислитель у цели `c`.
///
/// Составной порт эталон держит **целым**. Цель `c` с держит целым и массив (элемент
/// выбирает индекс обращения), а структуру по-прежнему разворачивает по полям - индекс
/// в HAL один, и второй уровень им не выразить. Поэтому наблюдаемое у массива - пара
/// "перечислитель, индекс", а у структуры и перечисления - перечислитель листа с
/// индексом 0.
const LEAVES: [(&str, usize, &str, usize); 6] = [
    ("bus", 0, "WORKER_PORT_BUS", 0),
    ("bus", 1, "WORKER_PORT_BUS", 1),
    ("bus", 2, "WORKER_PORT_BUS", 2),
    ("pair", 0, "WORKER_PORT_PAIR_LO", 0),
    ("pair", 1, "WORKER_PORT_PAIR_HI", 0),
    ("mode", usize::MAX, "WORKER_PORT_MODE", 0),
];

fn tool(bin: &str) -> bool {
    Command::new(bin)
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Каталог сборки, уникальный по потоку И процессу.
fn build_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0451_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог сборки");
    dir
}

fn source() -> String {
    std::fs::read_to_string(FIXTURE).expect("фикстура читается")
}

/// Значения листьев у эталона на каждом такте.
fn simulator_trace() -> Vec<Vec<i128>> {
    let (ast, _) = takt_lang::parse(&source(), 0).expect("разбор фикстуры");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    for _ in 0..TICKS {
        let result = unit.tick();
        assert!(
            !matches!(result, TickResult::Failed(_)),
            "эталон не обрывает прогон: {result:?}"
        );
        let mut row = Vec::new();
        for (port, index, _, _) in LEAVES {
            let value = unit.variable(port).expect("порт есть у эталона");
            row.push(leaf_value(&value, index));
        }
        trace.push(row);
    }
    trace
}

/// Часть значения эталона: элемент массива, поле структуры либо скаляр.
fn leaf_value(value: &Value, index: usize) -> i128 {
    match value {
        Value::Number(v) => *v,
        Value::Boolean(b) => i128::from(*b),
        Value::Array(items) => match items.get(index) {
            Some(item) => leaf_value(item, 0),
            None => panic!("у массива нет элемента {index}: {value:?}"),
        },
        // Поля - в объявленном порядке, и лист адресуется тем же порядком, каким его
        // строит разворот порта.
        Value::Struct { fields, .. } => match fields.get(index) {
            Some((_, item)) => leaf_value(item, 0),
            None => panic!("у структуры нет поля {index}: {value:?}"),
        },
        other => panic!("непредвиденный вид значения: {other:?}"),
    }
}

/// Значения листьев у прошивки: последняя запись HAL по каждому порту.
fn c_trace(dir: &Path) -> Vec<Vec<i128>> {
    takt_lang::compile_to_c(
        UNIT,
        &source(),
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение C");
    let upper = UNIT.to_uppercase();
    let arms = LEAVES
        .iter()
        .enumerate()
        .map(|(slot, (_, _, variant, index))| {
            // Ветвь различает не только порт, но и индекс: у массива-порта
            // перечислитель один на все элементы, и `case` по одному имени записал бы
            // все три значения в одну ячейку.
            format!(
                "        if (port == {upper}_{variant} && index == {index})                  {{ reg[{slot}] = (long long)value; }}"
            )
        })
        .collect::<Vec<_>>()
        .join("\n");
    let width = LEAVES.len();
    let harness = format!(
        r#"#include <stdio.h>
#include "{UNIT}.h"
static long long reg[{width}];
static void on_num(ConformancePortAggregateInit_Out_NumericPort port, uint8_t index, int64_t value, void *userdata) {{
    (void)userdata;
{arms}
}}
int main(void) {{
    ConformancePortAggregateInit m = {{0}};
    /* Колбэки — ДО `_init`: начальное значение порта уходит наружу уже там
       (правило 0187), и составной порт после разворота — не исключение. */
    m.write_numeric = on_num;
    m.userdata = 0;
    ConformancePortAggregateInit_init(&m);
    for (int i = 0; i < {TICKS}; i++) {{
        ConformancePortAggregateInit_tick(&m);
        for (int j = 0; j < {width}; j++) {{
            printf("%lld ", reg[j]);
        }}
        printf("\n");
    }}
    return 0;
}}
"#
    );
    std::fs::write(dir.join("harness.c"), harness).expect("харнесс");
    let bin = dir.join("bin");
    let compile = Command::new("cc")
        .args([
            "-std=c11",
            "-Wall",
            "-Wextra",
            "-Wno-unused-parameter",
            "-Werror",
            "-o",
        ])
        .arg(&bin)
        .arg(dir.join("harness.c"))
        .arg(dir.join(format!("{UNIT}.c")))
        .arg("-I")
        .arg(dir)
        .output()
        .expect("запуск cc");
    assert!(
        compile.status.success(),
        "cc не собрал харнесс флагами гейта цели:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск харнесса");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter(|l| !l.trim().is_empty())
        .map(|l| {
            l.split_whitespace()
                .map(|v| v.parse().expect("число в трассе"))
                .collect()
        })
        .collect()
}

/// Начальные значения листьев доезжают до прошивки и совпадают с эталоном.
#[test]
fn composite_port_initial_values_match_reference() {
    if !tool("cc") {
        eprintln!("cc недоступен — сверка пропущена");
        return;
    }
    let expected = simulator_trace();
    let actual = c_trace(&build_dir("trace"));
    assert_eq!(actual, expected, "цель c разошлась с эталоном");
    // Контроль: значения ненулевые - иначе "совпало" означало бы, что обе стороны
    // ничего не написали (именно так дефект и выглядел).
    assert!(
        expected[0].iter().all(|v| *v != 0),
        "начальные значения нулевые, сверка ничего не доказывает: {expected:?}"
    );
}
