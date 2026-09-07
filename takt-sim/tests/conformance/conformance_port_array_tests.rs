//! Порт-Массив: эталон против прошивки цели `c`.

use std::path::{Path, PathBuf};
use std::process::Command;

const FIXTURE: &str = "tests/data/eval/conformance_port_array.takt";
const UNIT: &str = "conformance_port_array";
const TICKS: usize = 3;

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

fn build_dir(tag: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!(
        "takt_0417_{tag}_{}_{}",
        std::process::id(),
        std::thread::current()
            .name()
            .unwrap_or("t")
            .replace(':', "_")
    ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    dir
}

/// Потактовые значения обоих элементов порта у эталона.
fn simulator_trace() -> Vec<(i128, i128)> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор фикстуры");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = takt_sim::build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    for _ in 0..TICKS {
        let _ = unit.tick();
        // У эталона порт остаётся массивом: разворот принадлежит целям.
        match unit.variable("bus") {
            Some(takt_sim::Value::Array(items)) => {
                let at = |i: usize| match &items[i] {
                    takt_sim::Value::Number(v) => *v,
                    other => panic!("элемент порта обязан быть числом, получено {other:?}"),
                };
                trace.push((at(0), at(1)));
            }
            other => panic!("порт 'bus' обязан быть массивом, получено {other:?}"),
        }
    }
    trace
}

/// Те же значения у прошивки - прогоном харнесса с колбэком HAL.
fn generated_c_trace(dir: &Path) -> Vec<(i128, i128)> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_c(
        UNIT,
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");

    // Наблюдение идёт через колбэк портов - ровно так прошивку видит плата.
    let harness = format!(
        r#"#include <stdio.h>
#include "{UNIT}.h"

static long long bus0 = -1;
static long long bus1 = -1;

static void on_write(ConformancePortArray_Out_NumericPort port, uint8_t index, int64_t value, void *ud) {{
    (void)index;
    (void)ud;
    /* Порт-массив - один порт, элемент выбирает индекс:
       прежде здесь стояли листья `_0` и `_1`, и разворот по листам не выражал
       переменного индекса. */
    if (port == CONFORMANCE_PORT_ARRAY_PORT_BUS && index == 0) {{ bus0 = (long long)value; }}
    if (port == CONFORMANCE_PORT_ARRAY_PORT_BUS && index == 1) {{ bus1 = (long long)value; }}
}}

int main(void) {{
    ConformancePortArray m;
    ConformancePortArray_init(&m);
    m.write_numeric = on_write;
    m.userdata = 0;
    for (int i = 0; i < {TICKS}; i++) {{
        ConformancePortArray_tick(&m);
        printf("%lld %lld\n", bus0, bus1);
    }}
    return 0;
}}
"#
    );
    let harness_path = dir.join("harness.c");
    std::fs::write(&harness_path, &harness).expect("запись харнесса");

    let bin = dir.join("port_bin");
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
        .arg(&harness_path)
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
    assert!(run.status.success(), "харнесс завершился с ошибкой");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .map(|line| {
            let mut parts = line
                .split_whitespace()
                .map(|p| p.parse::<i128>().expect("число в выводе харнесса"));
            (parts.next().expect("bus_0"), parts.next().expect("bus_1"))
        })
        .collect()
}

/// Значения листов совпадают потактово - и порядок не перепутан.
#[test]
fn port_array_values_match_the_reference() {
    let sim = simulator_trace();
    assert_eq!(
        sim,
        vec![(8, 11), (9, 13), (10, 15)],
        "эталон: элементы различны и растут по-разному"
    );

    if !cc_available() {
        eprintln!("[ПРОПУСК] port_array_values_match_the_reference: cc не найден");
        return;
    }
    let dir = build_dir("trace");
    let c = generated_c_trace(&dir);
    assert_eq!(
        sim, c,
        "значения листов порта обязаны совпадать с эталоном потактово"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// Адрес листа - базовый плюс смещение элемента.
///
/// Раскладка обязана быть предсказуемой: у HAL-целей порт ложится на регистр, и "где
/// второй элемент" - вопрос не вкуса, а стенда.
#[test]
fn leaf_addresses_follow_the_base() {
    let dir = build_dir("addr");
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_c_hal(
        UNIT,
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &[],
        &takt_lang::address_map::AddressEnv::default(),
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение c-hal");
    // Таблица адресов HAL живёт в заголовке - там и проверяется раскладка.
    let text =
        std::fs::read_to_string(dir.join(format!("{UNIT}.h"))).expect("чтение заголовка HAL");
    // С порт-массив не разворачивается по листам: адрес в таблице один - базовый, - а
    // элемент `i` находит реализация HAL шагом в ширину элемента. Предмет проверки
    // прежний ("второй элемент лежит за первым"), свидетель другой: не две строки
    // таблицы, а формула шага и ширина.
    assert!(
        text.contains("0x200"),
        "базовый адрес массива-порта обязан быть в таблице:\n{text}"
    );
    assert!(
        text.contains("b.addr + (uintptr_t)index * b.width"),
        "элемент обязан находиться шагом от базового адреса:\n{text}"
    );
    assert!(
        text.contains("0x200u, -1, 1 }"),
        "ширина доступа — ширина ЭЛЕМЕНТА (u8 → 1 байт), она же шаг:\n{text}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}
