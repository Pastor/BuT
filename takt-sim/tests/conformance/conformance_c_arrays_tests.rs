//! Сверка симулятора с порождённым `taktc -t c` по **массивам**.
//!
//! Вынесено из `conformance_c_tests.rs` (тот упёрся в лимит размера модуля - правило
//! CLAUDE.md; границы модулей = границы ответственности). Здесь - только сверка
//! исполнения массивов симулятором с эталоном C.
//!
//! `Array` **сверяется** с 0076: симулятор исполняет запись в элемент (`data[i] := v`)
//! и список-инициализатор `{...}`, значения совпадают с порождённым C. Тест пришёл на
//! смену тесту `a9_bit_and_array_conformance_gap`, который исправлениеировал препятствие
//! `SIM-017` (записи в элемент массива не было).
//!
//! **Вне сверки остаётся `[bit;N]`** - вопрос семантики языка, а не
//! дефект генератора; скалярный инициализатор массива C сам отвергает (CC-017),
//! эталона у него нет.

use std::path::PathBuf;
use std::process::Command;
use takt_lang::semantic::tree::construct_model;
use takt_sim::{Value, build_unit};

/// Тактов на прогон C до установившегося состояния (с запасом на `INIT`).
const MAX_TICKS: usize = 8;

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// **T18 /.** `Array` теперь **сверяется** с порождённым C.
///
/// До 0076 сверки не было (тест `a9_bit_and_array_conformance_gap` исправлениеировал
/// препятствие `SIM-017`): симулятор не писал элемент массива и не инициализировал
/// массив. Теперь `data[i] := v` исполняется, список `{...}` приводится поэлементно -
/// сверяем значения `data[i]`/`counter` с синтезом C.
///
/// Модель на `[u8;4]` (элемент `u8` = `Integer{8}` - **скаляр**), поэтому к
/// двойственности `[bit;N]` отношения нет. Скалярный инициализатор массива (`[u8;4] :=
/// 0`) и `[bit;N]` - вне объёма 0076; C сам scalar-init отвергает (CC-017), поэтому
/// эталона у него нет.
///
/// Наблюдение: поле-массив C печатается поэлементно (`m.entry.data[i]`), симулятор
/// индексирует `Value::Array` - оба против одного эталона (C).
#[test]
fn array_element_matches_generated_c() {
    if !cc_available() {
        eprintln!(
            "[ПРОПУСК] array_element_matches_generated_c: компилятор `cc` не найден — \
             сверка симулятора с порождённым C по массиву не выполнена"
        );
        return;
    }

    // `data` - список-инициализатор (валидная форма); в теле пишем два элемента.
    let source = "\
model ArrConf {
    var data: [u8;4] := {0, 0, 0, 0};
    var counter: u8 := 0;
    start Idle {
        always {
            data[0] := 7;
            data[1] := 200;
            counter := 1;
        }
    }
}
start Entry = ArrConf;
";
    // Симулятор: один такт исполняет тело `always`.
    let (ast, _) = takt_lang::parse(source, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение юнита");
    let _ = unit.tick();
    let sim_data = match unit.variable("data") {
        Some(Value::Array(items)) => items,
        other => panic!("`data` обязана быть массивом, получено {other:?}"),
    };
    let sim_elem = |i: usize| -> i128 {
        match &sim_data[i] {
            Value::Number(n) => *n,
            other => panic!("data[{i}]: не целое {other:?}"),
        }
    };

    // Порождённый C: собираем харнесс, печатающий data[i] и counter.
    let dir: PathBuf = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join("takt_conformance_0076_array");
    std::fs::create_dir_all(&dir).expect("каталог сборки");
    takt_lang::compile_to_c(
        "arrconf",
        source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");

    let harness = format!(
        r#"#include <stdio.h>
#include "arrconf.h"

int main(void) {{
    Arrconf m;
    Arrconf_init(&m);
    for (int i = 0; i < {MAX_TICKS}; i++) {{
        Arrconf_tick(&m);
        if (Arrconf_is_done(&m)) break;
    }}
    for (int i = 0; i < 4; i++) {{
        printf("data%d=%d\n", i, (int)m.entry.data[i]);
    }}
    printf("counter=%d\n", (int)m.entry.counter);
    return 0;
}}
"#
    );
    let harness_path = dir.join("harness.c");
    std::fs::write(&harness_path, harness).expect("запись харнесса");
    let bin = dir.join("arrconf_bin");
    let compile = Command::new("cc")
        .args(["-std=c11", "-I"])
        .arg(&dir)
        .arg(dir.join("arrconf.c"))
        .arg(&harness_path)
        .arg("-o")
        .arg(&bin)
        .output()
        .expect("запуск cc");
    assert!(
        compile.status.success(),
        "порождённый C не компилируется:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск собранного C");
    assert!(run.status.success(), "собранный C завершился с ошибкой");
    let out = String::from_utf8_lossy(&run.stdout);
    let c_val = |key: &str| -> i128 {
        out.lines()
            .find_map(|l| l.strip_prefix(&format!("{key}="))?.trim().parse().ok())
            .unwrap_or_else(|| panic!("C не напечатал '{key}': {out}"))
    };

    // Сверка: эталон data = [7, 200, 0, 0], counter = 1.
    for i in 0..4 {
        assert_eq!(
            sim_elem(i),
            c_val(&format!("data{i}")),
            "расхождение data[{i}]: симулятор={}, C={}",
            sim_elem(i),
            c_val(&format!("data{i}"))
        );
    }
    assert_eq!(
        sim_elem(0),
        7,
        "эталон data[0] = 7 (запись элемента исполнена)"
    );
    assert_eq!(sim_elem(1), 200, "эталон data[1] = 200");
    assert_eq!(c_val("counter"), 1, "counter = 1 (тело always исполнилось)");
}

/// **.** Вложенный массив `[[u8; 2]; 2]` - значения совпадают с C.
///
///
/// Сверяются значения, а не факт компиляции: перестановка индексов даёт валидный C с
/// другим поведением. Поэтому элементы различны, а один из них перезаписывается в теле.
#[test]
fn nested_array_matches_generated_c() {
    if !cc_available() {
        eprintln!(
            "[ПРОПУСК] nested_array_matches_generated_c: компилятор `cc` не найден — \
             сверка вложенного массива не выполнена"
        );
        return;
    }

    // Размерности разные (2 строки по 3): на квадратной матрице перестановка
    // размерностей в объявлении неразличима - мутация "печатать `[3][2]`" компилируется
    // и даёт те же значения.
    let source = "\
model NestConf {
    var grid: [[u8;3];2] := {{1, 2, 3}, {4, 5, 6}};
    var mirror: [[u8;3];2] := {{0, 0, 0}, {0, 0, 0}};
    var picked: u8 := 0;
    var sum: u8 := 0;
    var copied: u8 := 0;
    start Idle {
        always {
            grid[0][1] := 9;
            picked := grid[1][0];
            sum := grid[0][0] + grid[0][1] + grid[1][2];
            mirror := {{7, 8, 9}, {10, 11, 12}};
            copied := mirror[1][2];
        }
    }
}
start Entry = NestConf;
";
    let (ast, _) = takt_lang::parse(source, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение юнита");
    let _ = unit.tick();
    let sim_scalar = |unit: &takt_sim::Unit, name: &str| -> i128 {
        match unit.variable(name) {
            Some(Value::Number(n)) => n,
            other => panic!("{name}: не целое {other:?}"),
        }
    };
    let sim_picked = sim_scalar(&unit, "picked");
    let sim_sum = sim_scalar(&unit, "sum");
    let sim_copied = sim_scalar(&unit, "copied");

    let dir: PathBuf = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join("takt_conformance_0364_nested");
    std::fs::create_dir_all(&dir).expect("каталог сборки");
    takt_lang::compile_to_c(
        "nestconf",
        source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");

    let harness = format!(
        r#"#include <stdio.h>
#include "nestconf.h"

int main(void) {{
    Nestconf m;
    Nestconf_init(&m);
    for (int i = 0; i < {MAX_TICKS}; i++) {{
        Nestconf_tick(&m);
        if (Nestconf_is_done(&m)) break;
    }}
    printf("picked=%d\n", (int)m.entry.picked);
    printf("sum=%d\n", (int)m.entry.sum);
    printf("copied=%d\n", (int)m.entry.copied);
    for (int r = 0; r < 2; r++) {{
        for (int c = 0; c < 3; c++) {{
            printf("g%d%d=%d\n", r, c, (int)m.entry.grid[r][c]);
        }}
    }}
    return 0;
}}
"#
    );
    let harness_path = dir.join("harness_nested.c");
    std::fs::write(&harness_path, harness).expect("запись харнесса");
    let bin = dir.join("nestconf_bin");
    let compile = Command::new("cc")
        .args(["-std=c11", "-Wall", "-Wextra", "-Werror", "-I"])
        .arg(&dir)
        .arg(dir.join("nestconf.c"))
        .arg(&harness_path)
        .arg("-o")
        .arg(&bin)
        .output()
        .expect("запуск cc");
    assert!(
        compile.status.success(),
        "порождённый C не компилируется:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск собранного C");
    assert!(run.status.success(), "собранный C завершился с ошибкой");
    let out = String::from_utf8_lossy(&run.stdout);
    let c_val = |key: &str| -> i128 {
        out.lines()
            .find_map(|l| l.strip_prefix(&format!("{key}="))?.trim().parse().ok())
            .unwrap_or_else(|| panic!("C не напечатал '{key}': {out}"))
    };

    assert_eq!(
        sim_picked,
        c_val("picked"),
        "расхождение picked: симулятор={sim_picked}, C={}",
        c_val("picked")
    );
    assert_eq!(
        sim_sum,
        c_val("sum"),
        "расхождение sum: симулятор={sim_sum}, C={}",
        c_val("sum")
    );
    assert_eq!(
        sim_copied,
        c_val("copied"),
        "расхождение copied: симулятор={sim_copied}, C={}",
        c_val("copied")
    );
    // Значения эталона: элементы различны, одна ячейка перезаписана в теле, а вторая
    // переменная получает агрегат целиком - на симметричной матрице и на одинаковых
    // значениях перестановка индексов была бы неразличима.
    assert_eq!(
        sim_picked, 4,
        "grid[1][0] = 4 (вторая строка, первый столбец)"
    );
    assert_eq!(sim_sum, 16, "1 + 9 + 6");
    assert_eq!(sim_copied, 12, "mirror[1][2] после присваивания агрегата");
    assert_eq!(c_val("g01"), 9, "запись grid[0][1] дошла до C");
    assert_eq!(c_val("g10"), 4, "grid[1][0] не затронут записью");
}

/// **.** Элемент агрегата печатается по типу элемента.
///
/// Та же запись **скаляром** работает у всех девяти потребителей.
///
/// Сверяются значения: понижение q-литерала - это умножение на 2ⁿ, и ошибка в нём даёт
/// валидный C с другим числом.
#[test]
fn aggregate_element_types_match_generated_c() {
    if !cc_available() {
        eprintln!("[ПРОПУСК] aggregate_element_types_match_generated_c: компилятор `cc` не найден");
        return;
    }

    let source = "\
enum Mode { Idle = 0, Work = 7 }
model AggElem {
    var gains: [q(8,8); 2] := {1.5, 2.5};
    var modes: [Mode; 2] := {Idle, Work};
    var whole: u8 := 0;
    var code: u8 := 0;
    start Idle2 {
        always {
            whole := (gains[0] + gains[1]) as u8;
            modes[0] := Work;
            code := modes[0] as u8;
        }
    }
}
start Entry = AggElem;
";
    let (ast, _) = takt_lang::parse(source, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение юнита");
    let _ = unit.tick();
    let sim = |name: &str| -> i128 {
        match unit.variable(name) {
            Some(Value::Number(n)) => n,
            other => panic!("{name}: не целое {other:?}"),
        }
    };
    let sim_whole = sim("whole");
    let sim_code = sim("code");
    assert_eq!(sim_whole, 4, "1.5 + 2.5 = 4.0");
    assert_eq!(sim_code, 7, "modes[0] := Work → 7");

    let dir: PathBuf = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join("takt_conformance_0368_aggelem");
    std::fs::create_dir_all(&dir).expect("каталог сборки");
    takt_lang::compile_to_c(
        "aggelem",
        source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");

    let harness = format!(
        r#"#include <stdio.h>
#include "aggelem.h"

int main(void) {{
    Aggelem m;
    Aggelem_init(&m);
    for (int i = 0; i < {MAX_TICKS}; i++) {{
        Aggelem_tick(&m);
        if (Aggelem_is_done(&m)) break;
    }}
    printf("whole=%d\n", (int)m.entry.whole);
    printf("code=%d\n", (int)m.entry.code);
    printf("g0=%d\n", (int)m.entry.gains[0]);
    return 0;
}}
"#
    );
    let harness_path = dir.join("harness_aggelem.c");
    std::fs::write(&harness_path, harness).expect("запись харнесса");
    let bin = dir.join("aggelem_bin");
    let compile = Command::new("cc")
        .args(["-std=c11", "-Wall", "-Wextra", "-Werror", "-I"])
        .arg(&dir)
        .arg(dir.join("aggelem.c"))
        .arg(&harness_path)
        .arg("-o")
        .arg(&bin)
        .output()
        .expect("запуск cc");
    assert!(
        compile.status.success(),
        "порождённый C не компилируется:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск собранного C");
    assert!(run.status.success(), "собранный C завершился с ошибкой");
    let out = String::from_utf8_lossy(&run.stdout);
    let c_val = |key: &str| -> i128 {
        out.lines()
            .find_map(|l| l.strip_prefix(&format!("{key}="))?.trim().parse().ok())
            .unwrap_or_else(|| panic!("C не напечатал '{key}': {out}"))
    };

    assert_eq!(sim_whole, c_val("whole"), "расхождение whole");
    assert_eq!(sim_code, c_val("code"), "расхождение code");
    // Понижение q-литерала: 1.5 в q(8, 8) - это 384, а не 1.
    assert_eq!(c_val("g0"), 384, "литерал 1.5 понижен в q-представление");
}

/// **.** Понижение q-литерала доходит до полей структуры.
///
/// Причина: понижение шло по типу объявления, а поля живут в `ModelNode`, которого слой
/// свёртки не видел.
///
/// Сверяются значения: понижение - умножение на 2ⁿ, и ошибка в нём даёт валидный C с
/// другим числом.
#[test]
fn struct_field_fixed_matches_generated_c() {
    if !cc_available() {
        eprintln!("[ПРОПУСК] struct_field_fixed_matches_generated_c: компилятор `cc` не найден");
        return;
    }

    // Наблюдаемые - q-Значения, а не результат приведения `as u8`: приведение из поля
    // структуры цель `c` печатает без деления на 2ⁿ - соседний класс, вынесенный
    // кандидатом; смешав их, тест мерил бы не понижение литерала.
    let source = "\
struct Gains { kp: q(8,8), ki: q(8,8) }
model FieldQ {
    var g: Gains := {1.5, 2.5};
    var loops: [Gains; 2] := {{0.5, 0.25}, {1.0, 2.0}};
    var total: q(8,8) := 0.0;
    var picked: q(8,8) := 0.0;
    start Run {
        always {
            total := g.kp + g.ki;
            picked := loops[0].ki + loops[1].kp;
        }
    }
}
start Entry = FieldQ;
";
    let (ast, _) = takt_lang::parse(source, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение юнита");
    let _ = unit.tick();
    // Значение q эталон хранит представлением: 4.0 в q(8, 8) - это 1024.
    let sim = |name: &str| -> i128 {
        match unit.variable(name) {
            Some(Value::Fixed { repr, .. }) => i128::from(repr),
            other => panic!("{name}: не q-значение {other:?}"),
        }
    };
    let sim_total = sim("total");
    let sim_picked = sim("picked");
    assert_eq!(sim_total, 1024, "1.5 + 2.5 = 4.0 → 1024");
    assert_eq!(sim_picked, 320, "0.25 + 1.0 = 1.25 → 320");

    let dir: PathBuf = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join("takt_conformance_0370_fieldq");
    std::fs::create_dir_all(&dir).expect("каталог сборки");
    takt_lang::compile_to_c(
        "fieldq",
        source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");

    let harness = format!(
        r#"#include <stdio.h>
#include "fieldq.h"

int main(void) {{
    Fieldq m;
    Fieldq_init(&m);
    for (int i = 0; i < {MAX_TICKS}; i++) {{
        Fieldq_tick(&m);
        if (Fieldq_is_done(&m)) break;
    }}
    printf("total=%d\n", (int)m.entry.total);
    printf("picked=%d\n", (int)m.entry.picked);
    printf("kp=%d\n", (int)m.entry.g.kp);
    printf("l1kp=%d\n", (int)m.entry.loops[1].kp);
    return 0;
}}
"#
    );
    let harness_path = dir.join("harness_fieldq.c");
    std::fs::write(&harness_path, harness).expect("запись харнесса");
    let bin = dir.join("fieldq_bin");
    let compile = Command::new("cc")
        .args(["-std=c11", "-Wall", "-Wextra", "-Werror", "-I"])
        .arg(&dir)
        .arg(dir.join("fieldq.c"))
        .arg(&harness_path)
        .arg("-o")
        .arg(&bin)
        .output()
        .expect("запуск cc");
    assert!(
        compile.status.success(),
        "порождённый C не компилируется:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск собранного C");
    assert!(run.status.success(), "собранный C завершился с ошибкой");
    let out = String::from_utf8_lossy(&run.stdout);
    let c_val = |key: &str| -> i128 {
        out.lines()
            .find_map(|l| l.strip_prefix(&format!("{key}="))?.trim().parse().ok())
            .unwrap_or_else(|| panic!("C не напечатал '{key}': {out}"))
    };

    assert_eq!(sim_total, c_val("total"), "расхождение total");
    assert_eq!(sim_picked, c_val("picked"), "расхождение picked");
    // Представление: 1.5 в q(8, 8) - 384, 1.0 - 256.
    assert_eq!(c_val("kp"), 384, "поле структуры понижено");
    assert_eq!(c_val("l1kp"), 256, "поле структуры ВНУТРИ массива понижено");
}

/// **.** Приведение `as` из поля структуры масштабируется.
///
/// Всё **молча** и при нулевом коде возврата `taktc` - класс, ради которого сверки и
/// заведены.
#[test]
fn fixed_cast_from_field_matches_generated_c() {
    if !cc_available() {
        eprintln!("[ПРОПУСК] fixed_cast_from_field_matches_generated_c: компилятор `cc` не найден");
        return;
    }

    let source = "\
struct Gains { kp: q(8,8), ki: q(8,8) }
model CastQ {
    var g: Gains := {1.5, 2.5};
    var cells: [Gains; 2] := {{0.5, 3.25}, {7.75, 1.0}};
    var one: u8 := 0;
    var sum: u8 := 0;
    var elem: u8 := 0;
    start Run {
        always {
            one := g.kp as u8;
            sum := (g.kp + g.ki) as u8;
            elem := cells[1].kp as u8;
        }
    }
}
start Entry = CastQ;
";
    let (ast, _) = takt_lang::parse(source, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение юнита");
    let _ = unit.tick();
    let sim = |name: &str| -> i128 {
        match unit.variable(name) {
            Some(Value::Number(n)) => n,
            other => panic!("{name}: не целое {other:?}"),
        }
    };
    let (sim_one, sim_sum, sim_elem) = (sim("one"), sim("sum"), sim("elem"));
    // Значения разные: одинаковые не отличили бы поле от поля и элемент от элемента.
    assert_eq!(sim_one, 1, "1.5 as u8 = 1 (floor)");
    assert_eq!(sim_sum, 4, "1.5 + 2.5 = 4.0");
    assert_eq!(sim_elem, 7, "7.75 as u8 = 7");

    let dir: PathBuf = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join("takt_conformance_0371_castq");
    std::fs::create_dir_all(&dir).expect("каталог сборки");
    takt_lang::compile_to_c(
        "castq",
        source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");

    let harness = format!(
        r#"#include <stdio.h>
#include "castq.h"

int main(void) {{
    Castq m;
    Castq_init(&m);
    for (int i = 0; i < {MAX_TICKS}; i++) {{
        Castq_tick(&m);
        if (Castq_is_done(&m)) break;
    }}
    printf("one=%d\n", (int)m.entry.one);
    printf("sum=%d\n", (int)m.entry.sum);
    printf("elem=%d\n", (int)m.entry.elem);
    return 0;
}}
"#
    );
    let harness_path = dir.join("harness_castq.c");
    std::fs::write(&harness_path, harness).expect("запись харнесса");
    let bin = dir.join("castq_bin");
    let compile = Command::new("cc")
        .args(["-std=c11", "-Wall", "-Wextra", "-Werror", "-I"])
        .arg(&dir)
        .arg(dir.join("castq.c"))
        .arg(&harness_path)
        .arg("-o")
        .arg(&bin)
        .output()
        .expect("запуск cc");
    assert!(
        compile.status.success(),
        "порождённый C не компилируется:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск собранного C");
    assert!(run.status.success(), "собранный C завершился с ошибкой");
    let out = String::from_utf8_lossy(&run.stdout);
    let c_val = |key: &str| -> i128 {
        out.lines()
            .find_map(|l| l.strip_prefix(&format!("{key}="))?.trim().parse().ok())
            .unwrap_or_else(|| panic!("C не напечатал '{key}': {out}"))
    };

    assert_eq!(sim_one, c_val("one"), "расхождение one");
    assert_eq!(sim_sum, c_val("sum"), "расхождение sum");
    assert_eq!(sim_elem, c_val("elem"), "расхождение elem");
}
