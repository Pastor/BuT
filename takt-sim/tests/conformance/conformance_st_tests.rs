//! Потактовая сверка цели **Structured Text** (IEC 61131-3) с симулятором -, отдача
//! долга.
//!
//! ## Как исполняется порождённый ST
//!
//! Время выполненияа к ST не прилагается, поэтому: `taktc -t st` -> `iec2c` транслирует ST в C
//! (`POUS.c`/`POUS.h`) -> к нему пишется драйвер, который делает такт вызовом
//! `{Root}_body__`, читает наблюдаемые переменные и печатает их. Наблюдение берётся
//! оттуда, откуда его берёт потребитель (правило проекта): у ПЛК - это поля экземпляров
//! `FUNCTION_BLOCK` (аналог `dut.<сигнал>` у цели `sv`).
//!
//! **Капкан время выполненияа MatIEC - ноль-инициализация.** `__INIT_VAR` выставляет флаги через
//! `|=`, не обнуляя их, поэтому по неинициализированной структуре (`{Root}_data__ fb;` -
//! мусор на стеке) мусорный бит `__IEC_FORCE_FLAG` блокирует `__SET_VAR` - автомат
//! стоит на месте. Драйвер обязан обнулить структуру: `{Root}_data__ fb = {0};`. `EN`
//! каждого FB `init` ставит в TRUE сам - руками взводить не нужно (снято спайком ).
//!
//! ## Мягкая деградация
//!
//! `iec2c` собирается `scripts/ensure-iec2c.sh` и пакетом не поставляется. Как и проверка
//! ST в `precheck.sh`, сверка **не валит** сборку при отсутствии инструмента: нет
//! `iec2c`/`cc`/заголовков MatIEC -> тест-пропуск, а не красный.

use std::path::{Path, PathBuf};
use std::process::Command;
use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Unit, Value, build_unit};

/// Исправлениетура исправления: две модели с одноимённой `fn helper` (тела `x+1` и `x+2`),
/// переменные названы по-разному (`wa`/`wb`) ради раздельного наблюдения.
const FIXTURE: &str = "tests/data/eval/st_dup_fn.takt";

/// Имя корневой модели в C-символах `iec2c`. Идентификаторы IEC
/// **регистронезависимы**, и iec2c печатает их в верхнем регистре
/// (`STDUPFN_data__`/`STDUPFN_body__`), поэтому здесь - не `StDupFn`.
const ROOT: &str = "STDUPFN";

/// Тактов на прогон каждой стороны до установившегося состояния (с запасом).
const MAX_TICKS: usize = 8;

/// Наблюдаемые точки: `(имя в симуляторе, путь поля в структуре POUS)`.
///
/// Путь поля задан именами экземпляров MatIEC (`A0`/`B1` - модель + порядковый номер) и
/// полями (`WA`/`WB` - имя переменной в верхнем регистре). Оба детерминированы
/// (генерация 0048 + правила именования iec2c), поэтому для этой фикстуры фиксируются
/// здесь.
const OBSERVED: &[(&str, &str)] = &[("wa", "A0.WA"), ("wb", "B1.WB")];

/// Директория установки MatIEC (та же, что у `scripts/ensure-iec2c.sh`).
fn iec2c_prefix() -> PathBuf {
    if let Ok(p) = std::env::var("IEC2C_PREFIX") {
        return PathBuf::from(p);
    }
    let home = std::env::var("HOME").unwrap_or_else(|_| ".".to_string());
    Path::new(&home).join(".local")
}

/// `(бинарник iec2c, каталог lib MatIEC)` - если оба на месте.
fn iec2c_available() -> Option<(PathBuf, PathBuf)> {
    let prefix = iec2c_prefix();
    let bin = prefix.join("bin").join("iec2c");
    let lib = prefix.join("share").join("matiec").join("lib");
    // `-I lib` обязателен (иначе iec2c падает на любом входе), а `lib/C` - путь для
    // `cc` к время выполнения-заголовкам. Проверяем ключевой заголовок.
    if bin.is_file() && lib.join("C").join("iec_std_lib.h").is_file() {
        Some((bin, lib))
    } else {
        None
    }
}

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Прогоняет симулятор до завершения и возвращает наблюдаемые значения.
fn simulate() -> Vec<(String, i128)> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение юнита");
    for _ in 0..MAX_TICKS {
        let result = unit.tick();
        assert!(
            !matches!(result, TickResult::Failed(_)),
            "симуляция не должна падать: {result:?}"
        );
        if result == TickResult::Terminated {
            break;
        }
    }
    OBSERVED
        .iter()
        .map(|(name, _)| (name.to_string(), sim_value(&unit, name)))
        .collect()
}

fn sim_value(unit: &Unit, name: &str) -> i128 {
    match unit.variable(name) {
        Some(Value::Number(n)) => n,
        Some(Value::Boolean(b)) => i128::from(b),
        // q(m, n): наблюдаемое - представление (INT-поле в POUS = repr).
        Some(Value::Fixed { repr, .. }) => i128::from(repr),
        other => panic!("переменная '{name}': неожиданное значение {other:?}"),
    }
}

/// Порождает ST, транслирует его `iec2c` в C, собирает с драйвером и возвращает
/// значения, напечатанные исполнённым ST.
fn run_generated_st(dir: &Path, iec2c: &Path, lib: &Path) -> Vec<(String, i128)> {
    // 1. Порождение ST - тем же путём, что `taktc compile -t st`.
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    let st_dir = dir.join("st");
    std::fs::create_dir_all(&st_dir).expect("каталог ST");
    takt_lang::compile_to_st(
        "st_dup_fn.takt",
        &source,
        st_dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение ST");
    let st_file = st_dir.join("st_dup_fn.st");

    // 2. Трансляция ST -> C через iec2c (POUS.c/POUS.h кладутся в рабочий каталог).
    let work = dir.join("iec2c");
    std::fs::create_dir_all(&work).expect("рабочий каталог iec2c");
    let transpile = Command::new(iec2c)
        .arg("-I")
        .arg(lib)
        .arg(st_file)
        .current_dir(&work)
        .output()
        .expect("запуск iec2c");
    assert!(
        transpile.status.success() && work.join("POUS.c").is_file(),
        "iec2c не оттранслировал порождённый ST:\n{}",
        String::from_utf8_lossy(&transpile.stderr)
    );

    // 3. Драйвер: обнуляет структуру (см. заголовок про мусорные флаги),
    //    инициализирует, крутит такты до завершения, печатает наблюдаемое.
    let prints = OBSERVED
        .iter()
        .map(|(name, path)| format!(r#"    printf("{name}=%u\n", (unsigned)fb.{path}.value);"#))
        .collect::<Vec<_>>()
        .join("\n");
    let harness = format!(
        r#"#include <stdio.h>
#include "iec_std_lib.h"
TIME __CURRENT_TIME;
BOOL __DEBUG = 0;
#include "POUS.h"
#include "POUS.c"

int main(void) {{
    {ROOT}_data__ fb = {{0}};
    {ROOT}_init__(&fb, __BOOL_LITERAL(FALSE));
    for (int i = 0; i < {MAX_TICKS}; i++) {{
        {ROOT}_body__(&fb);
        if (fb.IS_DONE.value) break;
    }}
{prints}
    return 0;
}}
"#
    );
    let harness_path = work.join("harness.c");
    std::fs::write(&harness_path, harness).expect("запись драйвера");

    // 4. Сборка: `-w` глушит шум заголовков MatIEC (-Wpointer-sign/-Wvarargs) -
    //    он не относится к порождённому нами коду.
    let bin = work.join("st_conformance_bin");
    let compile = Command::new("cc")
        .args(["-std=c99", "-w", "-I"])
        .arg(lib.join("C"))
        .arg("-I")
        .arg(&work)
        .arg(&harness_path)
        .arg("-o")
        .arg(&bin)
        .output()
        .expect("запуск cc");
    assert!(
        compile.status.success(),
        "порождённый ST (через iec2c) не собирается:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    // 5. Запуск и разбор.
    let run = Command::new(&bin).output().expect("запуск драйвера ST");
    assert!(run.status.success(), "драйвер ST завершился с ошибкой");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter_map(|line| {
            let (name, value) = line.split_once('=')?;
            Some((name.to_string(), value.trim().parse().ok()?))
        })
        .collect()
}

/// **T3/A3 - Основная.** Трасса порождённого ST совпадает с симулятором на
/// фикстуре одноимённых `fn`: модель `B` обязана дать `wb = 3` (своей функцией
/// `x+2`), а не `2` (склейка с телом модели `A`).
///
/// **Мутация (T13):** вернуть дедупликацию по голому имени в
/// `st_func.rs::emit_functions` -> `wb` станет `2`, сверка **упадёт**, а проверка `iec2c`
/// останется зелёным. Это и есть смысл теста: он ловит то, чего проверка не видит.
#[test]
fn per_tick_trace_matches_generated_st() {
    let Some((iec2c, lib)) = iec2c_available() else {
        eprintln!("iec2c/заголовки MatIEC недоступны — сверка ST пропущена (мягкая деградация)");
        return;
    };
    if !cc_available() {
        eprintln!("cc недоступен — сверка ST пропущена (мягкая деградация)");
        return;
    }

    let dir = std::env::temp_dir().join(format!("st_conf_{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("рабочий каталог");

    let from_sim = simulate();
    let from_st = run_generated_st(&dir, &iec2c, &lib);

    // Обе стороны обязаны дать эталон исправления: wa = 2, wb = 3.
    assert_eq!(
        from_sim,
        vec![("wa".to_string(), 2), ("wb".to_string(), 3)],
        "эталон симулятора нарушен: ожидались wa=2, wb=3"
    );
    assert_eq!(
        from_st, from_sim,
        "трасса ST разошлась с симулятором — молча-неверная трансляция ST \
         (класс фикса 0041-01)"
    );

    let _ = std::fs::remove_dir_all(&dir);
}

// -----------------------------------------------------------------------------
// Q-арифметика fixed-point: T10 для цели st
//
// В IEC сдвигов над числами нет -> floor у `*` и `q -> int` идут через `FUNCTION
// TAKT_Q_FLOORDIV`. Наблюдаемое - INT-поле `FIXED0.ACC` (= repr q(8,8)), сверяется
// потактово с симулятором.

const FIXED_FIXTURE: &str = "tests/data/eval/conformance_fixed.takt";

/// Исправлениетура переполнения беззнакового.
const OVERFLOW_FIXTURE: &str = "tests/data/eval/conformance_overflow.takt";

/// Потактовая трасса `acc` (repr q(8,8)) симулятора.
fn simulate_fixed_trace() -> Vec<i128> {
    let source = std::fs::read_to_string(FIXED_FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение юнита");
    let mut trace = Vec::new();
    for _ in 0..MAX_TICKS {
        let result = unit.tick();
        assert!(
            !matches!(result, TickResult::Failed(_)),
            "симуляция: {result:?}"
        );
        trace.push(sim_value(&unit, "acc"));
        if result == TickResult::Terminated {
            break;
        }
    }
    trace
}

/// Потактовая трасса `acc` порождённого ST (через iec2c -> C).
fn run_generated_st_fixed(dir: &Path, iec2c: &Path, lib: &Path) -> Vec<i128> {
    let source = std::fs::read_to_string(FIXED_FIXTURE).expect("фикстура читается");
    let st_dir = dir.join("st");
    std::fs::create_dir_all(&st_dir).expect("каталог ST");
    takt_lang::compile_to_st(
        "qfix.takt",
        &source,
        st_dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение ST");

    let work = dir.join("iec2c");
    std::fs::create_dir_all(&work).expect("рабочий каталог iec2c");
    let transpile = Command::new(iec2c)
        .arg("-I")
        .arg(lib)
        .arg(st_dir.join("qfix.st"))
        .current_dir(&work)
        .output()
        .expect("запуск iec2c");
    assert!(
        transpile.status.success() && work.join("POUS.c").is_file(),
        "iec2c не оттранслировал Q-ST:\n{}",
        String::from_utf8_lossy(&transpile.stderr)
    );

    let harness = format!(
        r#"#include <stdio.h>
#include "iec_std_lib.h"
TIME __CURRENT_TIME;
BOOL __DEBUG = 0;
#include "POUS.h"
#include "POUS.c"

int main(void) {{
    QFIX_data__ fb = {{0}};
    QFIX_init__(&fb, __BOOL_LITERAL(FALSE));
    for (int i = 0; i < {MAX_TICKS}; i++) {{
        QFIX_body__(&fb);
        printf("%d:acc=%d\n", i, (int)fb.FIXED0.ACC.value);
        if (fb.IS_DONE.value) break;
    }}
    return 0;
}}
"#
    );
    let harness_path = work.join("harness.c");
    std::fs::write(&harness_path, harness).expect("запись драйвера");
    let bin = work.join("qfix_bin");
    let compile = Command::new("cc")
        .args(["-std=c99", "-w", "-I"])
        .arg(lib.join("C"))
        .arg("-I")
        .arg(&work)
        .arg(&harness_path)
        .arg("-o")
        .arg(&bin)
        .output()
        .expect("запуск cc");
    assert!(
        compile.status.success(),
        "Q-ST (через iec2c) не собирается:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск драйвера ST");
    assert!(run.status.success(), "драйвер Q-ST завершился с ошибкой");
    let out = String::from_utf8_lossy(&run.stdout).into_owned();
    let mut trace: Vec<(usize, i128)> = out
        .lines()
        .filter_map(|line| {
            let (t, rest) = line.split_once(':')?;
            let (_, v) = rest.split_once('=')?;
            Some((t.parse().ok()?, v.trim().parse().ok()?))
        })
        .collect();
    trace.sort_by_key(|(t, _)| *t);
    trace.into_iter().map(|(_, v)| v).collect()
}

/// Исправлениетура переноса к `W = 12` при типе хранения 16 бит.
const FIXED_W12_FIXTURE: &str = "tests/data/eval/conformance_fixed_wrap_w12.takt";

/// Потактовая трасса `acc` (repr q(6,6)) симулятора на w12-фикстуре.
fn simulate_fixed_w12_trace() -> Vec<i128> {
    let source = std::fs::read_to_string(FIXED_W12_FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение юнита");
    let mut trace = Vec::new();
    for _ in 0..MAX_TICKS {
        let result = unit.tick();
        assert!(
            !matches!(result, TickResult::Failed(_)),
            "симуляция: {result:?}"
        );
        trace.push(sim_value(&unit, "acc"));
        if result == TickResult::Terminated {
            break;
        }
    }
    trace
}

/// Потактовая трасса `acc` порождённого ST на w12-фикстуре (через iec2c -> C).
fn run_generated_st_fixed_w12(dir: &Path, iec2c: &Path, lib: &Path) -> Vec<i128> {
    let source = std::fs::read_to_string(FIXED_W12_FIXTURE).expect("фикстура читается");
    let st_dir = dir.join("st");
    std::fs::create_dir_all(&st_dir).expect("каталог ST");
    takt_lang::compile_to_st(
        "qw12.takt",
        &source,
        st_dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение ST");

    let work = dir.join("iec2c");
    std::fs::create_dir_all(&work).expect("рабочий каталог iec2c");
    let transpile = Command::new(iec2c)
        .arg("-I")
        .arg(lib)
        .arg(st_dir.join("qw12.st"))
        .current_dir(&work)
        .output()
        .expect("запуск iec2c");
    assert!(
        transpile.status.success() && work.join("POUS.c").is_file(),
        "iec2c не оттранслировал ST с TAKT_Q_WRAP:\n{}",
        String::from_utf8_lossy(&transpile.stderr)
    );

    let harness = format!(
        r#"#include <stdio.h>
#include "iec_std_lib.h"
TIME __CURRENT_TIME;
BOOL __DEBUG = 0;
#include "POUS.h"
#include "POUS.c"

int main(void) {{
    QW12_data__ fb = {{0}};
    QW12_init__(&fb, __BOOL_LITERAL(FALSE));
    for (int i = 0; i < {MAX_TICKS}; i++) {{
        QW12_body__(&fb);
        printf("%d:acc=%d\n", i, (int)fb.FIXED_WRAP_W120.ACC.value);
        if (fb.IS_DONE.value) break;
    }}
    return 0;
}}
"#
    );
    let harness_path = work.join("harness.c");
    std::fs::write(&harness_path, harness).expect("запись драйвера");
    let bin = work.join("qw12_bin");
    let compile = Command::new("cc")
        .args(["-std=c99", "-w", "-I"])
        .arg(lib.join("C"))
        .arg("-I")
        .arg(&work)
        .arg(&harness_path)
        .arg("-o")
        .arg(&bin)
        .output()
        .expect("запуск cc");
    assert!(
        compile.status.success(),
        "ST с TAKT_Q_WRAP (через iec2c) не собирается:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск драйвера ST");
    assert!(run.status.success(), "драйвер ST завершился с ошибкой");
    let out = String::from_utf8_lossy(&run.stdout).into_owned();
    let mut trace: Vec<(usize, i128)> = out
        .lines()
        .filter_map(|line| {
            let (t, rest) = line.split_once(':')?;
            let (_, v) = rest.split_once('=')?;
            Some((t.parse().ok()?, v.trim().parse().ok()?))
        })
        .collect();
    trace.sort_by_key(|(t, _)| *t);
    trace.into_iter().map(|(_, v)| v).collect()
}

/// (цель st): перенос идёт к **W**, а не к ширине хранения.
///
/// Формат `q(6, 6)`: `W = 12`, хранение `INT` (16 бит). Прежняя сверка шла на `q(8,
/// 8)`, где границы совпадают, - расхождение было ей невидимо. Тест заодно доказывает,
/// что `FUNCTION TAKT_Q_WRAP` **принимается MatIEC**: `MOD` над `LINT` и поправки знака -
/// не самоочевидная для IEC конструкция.
#[test]
fn fixed_wrap_to_width_matches_generated_st() {
    let sim = simulate_fixed_w12_trace();
    assert_eq!(
        sim,
        vec![-1664, -1152, -640, -128, 384, 896, 1408, 1920],
        "q(6,6): перенос к 12 битам (repr ∈ [−2048, 2047]); к 16 битам дал бы 2432"
    );

    let Some((iec2c, lib)) = iec2c_available() else {
        eprintln!("iec2c/MatIEC недоступны — сверка ST(w12) пропущена");
        return;
    };
    if !cc_available() {
        eprintln!("cc недоступен — сверка ST(w12) пропущена");
        return;
    }
    let dir = std::env::temp_dir().join(format!("st_conf_fixed_w12_{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("рабочий каталог");
    let st = run_generated_st_fixed_w12(&dir, &iec2c, &lib);
    assert_eq!(
        sim, st,
        "перенос к W обязан совпасть с ST.\nсимулятор={sim:?}\nST={st:?}"
    );
}

/// T10/A4 (цель st): побитовая потактовая сверка Q-арифметики с симулятором - floor к
/// −∞ у `*` строится `TAKT_Q_FLOORDIV` (сдвигов над числами в IEC нет).
#[test]
fn fixed_point_arithmetic_matches_generated_st() {
    let sim = simulate_fixed_trace();
    assert_eq!(
        sim,
        vec![-768, -384, -2, 510, 1022, 1534, 2046, 2558],
        "трасса представлений q(8,8) — эталон Q-арифметики симулятора"
    );

    let Some((iec2c, lib)) = iec2c_available() else {
        eprintln!("iec2c/MatIEC недоступны — сверка Q-ST пропущена (трасса симулятора пришпилена)");
        return;
    };
    if !cc_available() {
        eprintln!("cc недоступен — сверка Q-ST пропущена");
        return;
    }
    let dir = std::env::temp_dir().join(format!("st_conf_fixed_{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("рабочий каталог");
    let st = run_generated_st_fixed(&dir, &iec2c, &lib);

    // Поправка на INIT-сдвиг снята: цель `st` больше не тратит скан на вход в стартовое
    // состояние (контракт 0033 соблюдён), поэтому трассы обязаны совпадать напрямую.
    //
    // Прежняя редакция сверяла `st.ends_with(&sim)` с нулевым префиксом - то есть
    // **закрепляла дефект как ожидаемое поведение**. Так он и дожил: тест был зелёным
    // именно потому, что описывал неверное.
    assert_eq!(
        st, sim,
        "Q-арифметика ST обязана совпасть с симулятором побитово и такт в такт.\n\
         симулятор={sim:?}\nST={st:?}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

// -----------------------------------------------------------------------------
// Прозрачный float -> q(m, n), embedded-путь
//
// Как и у rust: наблюдение Q-модели без портов затруднено, поэтому сверка -
// **byte-equality**: вывод float-фикстуры под --float-embedded обязан совпасть
// Байт-В-Байт с выводом явного q-двойника (одинаковый basename). Так float->q
// наследует уже проверенную 0061 ST-Q-арифметику (fixed_point_..._st выше).
// -----------------------------------------------------------------------------

const FLOAT_Q_FIXTURE: &str = "tests/data/eval/conformance_float_q.takt";
const FLOAT_Q_TWIN: &str = "tests/data/eval/conformance_float_q_twin.takt";

/// Опции embedded-Q для `float`.
#[allow(clippy::field_reassign_with_default)] // GenerateOptions - #[non_exhaustive]
fn float_embedded_opts(m: u8, n: u8) -> takt_lang::generator::GenerateOptions {
    let mut o = takt_lang::generator::GenerateOptions::default();
    o.float_as_q = Some((m, n));
    o.float_embedded = true;
    o
}

/// T6/A4 (цель st, embedded): `float` под `--float-embedded` даёт байт-В-Байт тот же
/// ST, что явный `q(8, 8)`. Одинаковый basename -> символы совпадают, а содержимое -
/// только если трансформация даёт ровно проверенный q-кодоген.
#[test]
fn float_embedded_matches_explicit_q_st() {
    let dir = std::env::temp_dir().join(format!("st_conf_float_eq_{}", std::process::id()));
    let out_f = dir.join("f");
    let out_q = dir.join("q");
    std::fs::create_dir_all(&out_f).unwrap();
    std::fs::create_dir_all(&out_q).unwrap();
    let float_src = std::fs::read_to_string(FLOAT_Q_FIXTURE).expect("float-фикстура");
    let twin_src = std::fs::read_to_string(FLOAT_Q_TWIN).expect("q-двойник");
    takt_lang::compile_to_st(
        "twin",
        &float_src,
        out_f.to_str().unwrap(),
        &[],
        &float_embedded_opts(8, 8),
    )
    .expect("float → st");
    takt_lang::compile_to_st(
        "twin",
        &twin_src,
        out_q.to_str().unwrap(),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("q → st");
    let st_f = std::fs::read_to_string(out_f.join("twin.st")).expect(".st float");
    let st_q = std::fs::read_to_string(out_q.join("twin.st")).expect(".st q");
    // Сравнивается код: фикстуры - разные файлы, и комментарии авторов в них законно
    // различаются, а с они доезжают до вывода. Предмет проверки - совпадение
    // порождённого кода, а не текста.
    assert_eq!(
        crate::target_code::code_only(&st_f),
        crate::target_code::code_only(&st_q),
        "float→q(8,8) под --float-embedded обязан дать ровно тот же ST, что явный q(8,8)"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// A3/T5 (цель st native по умолчанию): `--float-as-q` без `--float-embedded` оставляет
/// `float` нативным `LREAL`. Проверка переключения: с `--float-embedded` - `INT`.
/// Молчаливого Q быть не должно.
#[test]
#[allow(clippy::field_reassign_with_default)] // GenerateOptions - #[non_exhaustive]
fn float_as_q_without_embedded_is_native_st() {
    let dir = std::env::temp_dir().join(format!("st_conf_float_native_{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let source = std::fs::read_to_string(FLOAT_Q_FIXTURE).expect("фикстура");
    let mut opts = takt_lang::generator::GenerateOptions::default();
    opts.float_as_q = Some((8, 8)); // точность задана, embedded НЕ включён
    takt_lang::compile_to_st("cfq", &source, dir.to_str().unwrap(), &[], &opts)
        .expect("порождение st");
    let st = std::fs::read_to_string(dir.join("cfq.st")).expect(".st");
    assert!(
        st.contains("LREAL"),
        "без --float-embedded float остаётся native LREAL (не INT).\n{st}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

// -----------------------------------------------------------------------------
// Переполнение беззнакового целого: правило S1 - обёртка mod 2^N
//
// Единственная из четырёх целей, чьё поведение при переполнении до 0127 никем не
// проверялось: `iec2c` доказывает, что ST компилируется, но не что USINT оборачивается
// так же, как `uint8_t` в C. Здесь это проверяется исполнением.

// -- Модель времени: профиль "часы" через штатный TON --------------

const TIME_FIXTURE: &str = "tests/data/eval/conformance_after_st.takt";
/// Корень фикстуры времени в C-символах iec2c (верхний регистр от basename).
const TIME_ROOT: &str = "STTIME";
/// Путь к наблюдаемому порту в структуре POUS (под-FB `DWELL0`, поле `LEVEL`).
const TIME_PORT: &str = "DWELL0.LEVEL";
const TIME_TICKS: usize = 8;

/// Трасса симулятора при 1 мс на такт (эталон профиля "часы").
fn simulate_time_trace() -> Vec<i128> {
    let source = std::fs::read_to_string(TIME_FIXTURE).expect("фикстура");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение юнита");
    let mut trace = Vec::new();
    for step in 0..TIME_TICKS {
        unit.set_time_ns(i64::try_from(step).unwrap() * 1_000_000);
        let result = unit.tick();
        assert!(
            !matches!(result, TickResult::Failed(_)),
            "падение: {result:?}"
        );
        trace.push(sim_value(&unit, "level"));
    }
    trace
}

/// Трасса порождённого ST: драйвер подаёт модельное время в `__CURRENT_TIME` (1 мс на
/// такт) перед каждым `_body__` - проба П3. Печатает порт после скана.
fn run_st_time_trace(dir: &Path, iec2c: &Path, lib: &Path) -> Vec<i128> {
    let source = std::fs::read_to_string(TIME_FIXTURE).expect("фикстура");
    let st_dir = dir.join("st");
    std::fs::create_dir_all(&st_dir).expect("каталог ST");
    takt_lang::compile_to_st(
        "sttime.takt",
        &source,
        st_dir.to_str().expect("путь"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение ST");
    let work = dir.join("iec2c");
    std::fs::create_dir_all(&work).expect("рабочий каталог");
    let transpile = Command::new(iec2c)
        .arg("-I")
        .arg(lib)
        .arg(st_dir.join("sttime.st"))
        .current_dir(&work)
        .output()
        .expect("запуск iec2c");
    assert!(
        transpile.status.success() && work.join("POUS.c").is_file(),
        "iec2c не оттранслировал ST времени:\n{}",
        String::from_utf8_lossy(&transpile.stderr)
    );

    let harness = format!(
        r#"#include <stdio.h>
#include "iec_std_lib.h"
TIME __CURRENT_TIME;
BOOL __DEBUG = 0;
#include "POUS.h"
#include "POUS.c"

int main(void) {{
    {TIME_ROOT}_data__ fb = {{0}};
    {TIME_ROOT}_init__(&fb, __BOOL_LITERAL(FALSE));
    for (int i = 0; i < {TIME_TICKS}; i++) {{
        /* 1 мс на такт: модельное время такта i (проба П3). */
        __CURRENT_TIME.tv_sec = 0;
        __CURRENT_TIME.tv_nsec = (long)i * 1000000L;
        {TIME_ROOT}_body__(&fb);
        printf("TICK %u\n", (unsigned)fb.{TIME_PORT}.value);
    }}
    return 0;
}}
"#
    );
    let harness_path = work.join("harness_time.c");
    std::fs::write(&harness_path, harness).expect("драйвер");
    let bin = work.join("st_time_bin");
    let compile = Command::new("cc")
        .args(["-std=c99", "-w", "-I"])
        .arg(lib.join("C"))
        .arg("-I")
        .arg(&work)
        .arg(&harness_path)
        .arg("-o")
        .arg(&bin)
        .output()
        .expect("запуск cc");
    assert!(
        compile.status.success(),
        "ST времени (через iec2c) не собирается:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск драйвера");
    assert!(
        run.status.success(),
        "драйвер ST времени завершился с ошибкой"
    );
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter_map(|l| l.strip_prefix("TICK ")?.trim().parse::<i128>().ok())
        .collect()
}

/// Выдержка `after 5ms` (профиль "часы") через штатный `TON`: цель `st`, как эталон,
/// Задерживает переход и снимает его после накопления 5 мс.
///
/// Сверяется не абсолютный такт, а **свойство выдержки**. `TON` отмеряет длительность
/// верно (проба П3 анализа: ET копит инжектированное время до `PT`), но абсолютный такт
/// срабатывания у `st` смещён относительно симулятора на два скана - по причинам, к
/// времени отношения не имеющим: (1) ветвь INIT `CASE` расходует скан (тело стартового
/// состояния исполняется со следующего - свойство структуры ST, а не 0033), (2) MatIEC
/// `TON` ловит фронт `IN` на втором вызове. Оба - идиосинкразия ST/MatIEC. Поэтому
/// потактовое равенство трасс здесь не требуется; требуется, чтобы выдержка **сработала
/// не сразу и не никогда**, была монотонной и не опередила эталон. Мягкая деградация:
/// нет iec2c/cc -> пропуск.
#[test]
fn after_clock_profile_delays_and_fires_in_generated_st() {
    let sim = simulate_time_trace();
    let sim_fire = sim
        .iter()
        .position(|&v| v == 1)
        .expect("эталон снимает выдержку");
    assert_eq!(
        sim,
        vec![0, 0, 0, 0, 0, 1, 1, 1],
        "эталон профиля «часы»: {sim:?}"
    );
    let Some((iec2c, lib)) = iec2c_available() else {
        eprintln!(
            "[ПРОПУСК] after_clock_profile_delays_and_fires_in_generated_st: iec2c не найден"
        );
        return;
    };
    if !cc_available() {
        eprintln!("[ПРОПУСК] after_clock_profile_delays_and_fires_in_generated_st: cc не найден");
        return;
    }
    let dir = tempfile::tempdir().expect("каталог");
    let st = run_st_time_trace(dir.path(), &iec2c, &lib);
    let st_fire = st
        .iter()
        .position(|&v| v == 1)
        .unwrap_or_else(|| panic!("ST обязан снять выдержку (TON сработал): {st:?}"));
    // Задержка: не мгновенно (не такт 0) - TON реально ждёт.
    assert!(
        st_fire > 0,
        "выдержка не должна срабатывать мгновенно: {st:?}"
    );
    // Не раньше эталона: TON меряет ту же длительность, ST лишь структурно позже.
    assert!(
        st_fire >= sim_fire,
        "ST не может опередить эталон (та же длительность): ST={st:?}, эталон снял на {sim_fire}"
    );
    // Монотонность: 0...0 затем 1...1 (переход единожды, не мигает).
    assert!(
        st.iter().skip(st_fire).all(|&v| v == 1) && st.iter().take(st_fire).all(|&v| v == 0),
        "трасса ST обязана быть монотонной 0→1: {st:?}"
    );
}

/// Профиль "такты" (`--tick-hz` без `clock`) порождает счётчик `takt_dwell`, не `TON`,
/// и `iec2c` его принимает. Тест валидности встречного профиля.
#[test]
#[allow(clippy::field_reassign_with_default)]
fn after_ticks_profile_generates_valid_st() {
    let source = std::fs::read_to_string(TIME_FIXTURE).expect("фикстура");
    let dir = tempfile::tempdir().expect("каталог");
    let st_dir = dir.path().join("st");
    std::fs::create_dir_all(&st_dir).expect("каталог ST");
    let mut opts = takt_lang::generator::GenerateOptions::default();
    opts.tick_hz = Some(1000); // профиль "такты" без объявления clock
    takt_lang::compile_to_st("sttick.takt", &source, st_dir.to_str().unwrap(), &[], &opts)
        .expect("порождение ST");
    let st = std::fs::read_to_string(st_dir.join("sttick.st")).expect(".st");
    assert!(
        st.contains("takt_dwell"),
        "профиль «такты» — счётчик:\n{st}"
    );
    assert!(
        !crate::target_code::code_only(&st).contains("TON"),
        "профиль «такты» не должен эмитить TON:\n{st}"
    );

    let Some((iec2c, lib)) = iec2c_available() else {
        eprintln!("[ПРОПУСК] after_ticks_profile_generates_valid_st: iec2c не найден");
        return;
    };
    let work = dir.path().join("iec2c");
    std::fs::create_dir_all(&work).expect("рабочий каталог");
    let transpile = Command::new(&iec2c)
        .arg("-I")
        .arg(&lib)
        .arg(st_dir.join("sttick.st"))
        .current_dir(&work)
        .output()
        .expect("запуск iec2c");
    assert!(
        transpile.status.success() && work.join("POUS.c").is_file(),
        "iec2c отверг профиль «такты»:\n{}",
        String::from_utf8_lossy(&transpile.stderr)
    );
}

// Насыщение q(m, n) sat - подмодулем: файл упирается в лимит размера модуля, а правило
// требует делить по логике.
#[path = "conformance_st_tests/fixed_sat.rs"]
mod fixed_sat;

/// Агрегатный инициализатор структуры.
mod struct_init;

/// Разряд в позиции числового значения.
mod bit_value;

/// Перечисление внутри функции.
mod enum_in_function;

/// Обёртка беззнакового целого.
mod overflow;

/// Инициализатор массива.
mod array_init;

/// Агрегат в локальном объявлении.
mod local_aggregate;

/// Массив в параметре функции.
mod array_param;
