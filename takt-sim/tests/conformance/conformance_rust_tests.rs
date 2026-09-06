//! Сверка симулятора с прошивкой, порождённой `taktc -t rust` (фича 0050).
//!
//! # Зачем, если гейт `rustc`/`clippy` уже зелёный
//!
//! **Гейт доказывает, что вывод компилируется, но не что он ведёт себя как
//! модель.** Молча неверная трансляция (перепутанный приоритет, потерянный
//! переход, не тот операнд) компилируется тоже — и `-D warnings` её пропустит.
//!
//! Урок не теоретический. Фича 0045 (цель `sv`) поймала ровно такой дефект:
//! чтение внутри такта давало значение предыдущего такта, и **оба** её гейта
//! (verilator и yosys) модуль принимали — он был валиден и синтезируем, просто
//! считал не то. Вывод, записанный в отчёте 0045 и в `CLAUDE.md`: потактовую
//! сверку с симулятором нужно заводить **вместе** с бэкендом. Здесь она
//! заводится — и делает цель `rust` третьей после `c` и `sv`, чьё поведение
//! сверено с эталоном, а не только синтаксис.
//!
//! # Что здесь наблюдается и почему через порты
//!
//! Через **выходные порты**, то есть через HAL. Поля модели в порождённом Rust
//! **приватны** (`n: u8`, не `pub n: u8`), и это правильно: инкапсуляция — часть
//! того, что цель даёт сверх `c`, где структура открыта нараспашку. Делать поля
//! публичными ради теста значило бы менять **продукт** ради **проверки**.
//!
//! У цели `sv` наблюдение идёт иерархической ссылкой (`dut.<сигнал>`) — там это
//! законный отладочный механизм языка. В Rust аналога нет, и порт — честный
//! способ: ровно так прошивку наблюдает и реальная плата.
//!
//! # Мягкая деградация
//!
//! `rustc` — зависимость проекта (это Rust-репозиторий), поэтому пропуск здесь
//! теоретический: проверка оставлена по образцу `cc_available()`, чтобы тест
//! падал по существу, а не по отсутствию инструмента.

use std::path::{Path, PathBuf};
use std::process::Command;
use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Unit, Value, build_unit};

/// Тактов в трассе — с запасом над её длиной.
const TRACE_TICKS: usize = 6;

fn rustc_available() -> bool {
    Command::new("rustc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

fn sim_value(unit: &Unit, name: &str) -> i128 {
    match unit.variable(name) {
        Some(Value::Number(n)) => n,
        Some(Value::Boolean(b)) => i128::from(b),
        other => panic!("порт '{name}': неожиданное значение {other:?}"),
    }
}

/// Потактовая трасса симулятора: значение `port` после каждого такта.
fn simulate_trace(fixture: &Path, port: &str) -> Vec<i128> {
    let source = std::fs::read_to_string(fixture).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение юнита");
    let mut trace = Vec::new();
    for _ in 0..TRACE_TICKS {
        let result = unit.tick();
        assert!(
            !matches!(result, TickResult::Failed(_)),
            "симуляция не должна падать: {result:?}"
        );
        trace.push(sim_value(&unit, port));
        if result == TickResult::Terminated {
            break;
        }
    }
    trace
}

/// Потактовая трасса порождённой прошивки.
///
/// Порождает `.rs` тем же `taktc`, пишет драйвер, компилирует его настоящим
/// `rustc` и запускает. Драйвер реализует `Hal`, запоминая последнее записанное
/// в порт значение, и печатает его после каждого такта — то есть наблюдает
/// прошивку ровно так, как наблюдала бы плата.
///
/// **Драйвер пишется здесь, а не порождается `taktc`.** Он принадлежность
/// проверки, а не продукта (то же решение, что и у тестбенча цели `sv`).
///
/// `root` — имя корневой структуры (CamelCase от имени файла), `variant` — имя
/// варианта порта в `OutU8Port`.
fn rust_trace(
    dir: &Path,
    fixture: &Path,
    basename: &str,
    root: &str,
    variant: &str,
    ticks: usize,
) -> Vec<i128> {
    let source = std::fs::read_to_string(fixture).expect("фикстура читается");
    takt_lang::compile_to_rust(
        basename,
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение Rust");

    let module = dir.join(format!("{basename}.rs"));
    // Наблюдение выносится из HAL общей ячейкой: поле `hal` модели приватно и
    // аксессора не имеет — как и должно быть у прошивки. Ровно так же плата
    // наблюдает не «внутренности прошивки», а состояние регистра, в который та
    // пишет.
    let driver = format!(
        r#"#[path = "{module}"]
mod generated;
use generated::{{Hal, OutU8Port, {root}}};
use std::cell::RefCell;
use std::rc::Rc;

/// Подставное железо: запоминает последнее записанное в порт значение.
struct Probe {{ reg: Rc<RefCell<u8>> }}

impl Hal for Probe {{
    fn write_u8(&mut self, port: OutU8Port, value: u8) {{
        assert!(matches!(port, OutU8Port::{variant}), "неожиданный порт");
        *self.reg.borrow_mut() = value;
    }}
}}

fn main() {{
    let reg = Rc::new(RefCell::new(0u8));
    let mut model = {root}::new(Probe {{ reg: Rc::clone(&reg) }});
    model.init();
    for _ in 0..{ticks} {{
        model.tick();
        println!("TICK {{}}", reg.borrow());
    }}
}}
"#,
        module = module.display(),
    );
    let driver_path = dir.join("driver.rs");
    std::fs::write(&driver_path, driver).expect("запись драйвера");

    let build = Command::new("rustc")
        .current_dir(dir)
        .args(["--edition", "2021", "driver.rs", "-o", "driver"])
        .output()
        .expect("запуск rustc");
    assert!(
        build.status.success(),
        "rustc не собрал драйвер:\n{}",
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(dir.join("driver"))
        .current_dir(dir)
        .output()
        .expect("запуск драйвера");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter_map(|line| line.strip_prefix("TICK "))
        .map(|v| v.trim().parse::<i128>().expect("значение — целое"))
        .collect()
}

fn build_dir(tag: &str) -> PathBuf {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_conformance_rust_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог сборки");
    dir
}

fn fixture(dir: &Path, name: &str, source: &str) -> PathBuf {
    let path = dir.join(format!("{name}.takt"));
    std::fs::write(&path, source).expect("запись фикстуры");
    path
}

/// Общая проверка: трассы совпадают потактово и тело старта — на такте 1.
fn check(tag: &str, root: &str, source: &str) {
    let dir = build_dir(tag);
    let path = fixture(&dir, tag, source);
    let sim = simulate_trace(&path, "n");
    assert_eq!(
        sim.first(),
        Some(&1),
        "симулятор: тело стартового состояния обязано исполниться на такте 1 \
         (контракт ADR 0033), трасса={sim:?}"
    );

    if !rustc_available() {
        eprintln!("[ПРОПУСК] {tag}: rustc не найден — сверка с прошивкой не выполнена");
        return;
    }
    let rs = rust_trace(&dir, &path, tag, root, "N", sim.len());
    assert_eq!(
        sim, rs,
        "потактовые трассы симулятора и порождённого Rust обязаны совпадать НА \
         КАЖДОМ такте: гейт rustc/clippy доказывает лишь, что вывод \
         компилируется.\nсимулятор={sim:?}\nRust={rs:?}"
    );
}

/// **T23 (контракт 0033), глубина 1:** тело стартового состояния — на такте 1.
#[test]
fn shift_is_zero_at_depth_1() {
    check(
        "rsdepth1",
        "Rsdepth1",
        "out n: u8; \
         start S0 { always { n := 1; } ref S1; } \
         state S1 { always { n := 2; } }",
    );
}

/// **T23, глубина 2** (`start E = M;`): лишний уровень трассу не сдвигает.
#[test]
fn shift_is_zero_at_depth_2() {
    check(
        "rsdepth2",
        "Rsdepth2",
        "model M { out n: u8; start S0 { always { n := 1; } ref S1; } \
         state S1 { always { n := 2; } } } \
         start E = M;",
    );
}

/// **T23, глубина 3** — где сдвиг цели `c` был максимальным (3 такта до 0033).
#[test]
fn shift_is_zero_at_depth_3() {
    check(
        "rsdepth3",
        "Rsdepth3",
        "model Inner { out n: u8; start S0 { always { n := 1; } ref S1; } \
         state S1 { always { n := 2; } } } \
         model Mid { start M = Inner; } \
         start E = Mid;",
    );
}

/// Потактовая трасса на модели, эволюционирующей несколько тактов.
///
/// Сдвиг на такт (если бы он вернулся) сместил бы **всю** трассу, а не только
/// первое значение, — и был бы пойман здесь.
#[test]
fn per_tick_trace_matches_generated_rust() {
    let dir = build_dir("rstrace");
    let source = "model Counter { out n: u8; \
                  start S0 { always { n := 1; } ref S1; } \
                  state S1 { always { n := 2; } ref S2; } \
                  state S2 { always { n := 3; } } } \
                  start Entry = Counter;";
    let path = fixture(&dir, "rstrace", source);
    let sim = simulate_trace(&path, "n");
    // Пиннинг: если трасса симулятора изменится, тест упадёт здесь, а не
    // «подстроится» под прошивку.
    assert_eq!(
        sim,
        vec![1, 2, 3, 3, 3, 3],
        "ожидаемая трасса симулятора: n = 1, 2, 3"
    );

    if !rustc_available() {
        eprintln!(
            "[ПРОПУСК] per_tick_trace_matches_generated_rust: rustc не найден \
             (трасса симулятора пришпилена выше)"
        );
        return;
    }
    let rs = rust_trace(&dir, &path, "rstrace", "Rstrace", "N", sim.len());
    assert_eq!(
        sim, rs,
        "потактовые трассы обязаны совпадать.\nсимулятор={sim:?}\nRust={rs:?}"
    );
}

/// Обёртка `u8` совпадает у симулятора и порождённой прошивки Rust.
///
/// **Сторож главного дефекта фичи 0127.** Цель `rust` печатала `n += 1`, и на
/// `255 + 1` прошивка **паниковала** (`attempt to add with overflow`) в
/// debug-профиле, а в release молча оборачивала — то есть поведение зависело от
/// профиля сборки пользователя, и ни один из двух вариантов не совпадал с
/// правилом языка гарантированно. Теперь печатается `wrapping_add`.
///
/// Наблюдение — через выходной порт: поля модели приватны (см. шапку файла).
#[test]
fn unsigned_overflow_wraps_like_generated_rust() {
    let dir = build_dir("rsovf");
    // Счётчик — `var` (выходной порт читать нельзя, SE-027), наружу он
    // зеркалится портом `n`: наблюдение в этой сверке идёт только через HAL.
    let source = "model Wrap { var t: u8 := 253; out n: u8; \
                  start Counting { always { t := t + 1; n := t; } ref Done: t = 3; } \
                  state Done {} } \
                  start Entry = Wrap;";
    let path = fixture(&dir, "rsovf", source);
    let sim = simulate_trace(&path, "n");
    // Пиннинг правила S1: обёртка на третьем такте.
    assert_eq!(
        sim,
        vec![254, 255, 0, 1, 2, 3],
        "ожидаемая трасса симулятора: 254, 255, 0 (обёртка), 1, 2, 3"
    );

    if !rustc_available() {
        eprintln!(
            "[ПРОПУСК] unsigned_overflow_wraps_like_generated_rust: rustc не найден \
             (трасса симулятора пришпилена выше)"
        );
        return;
    }
    let rs = rust_trace(&dir, &path, "rsovf", "Rsovf", "N", sim.len());
    assert_eq!(
        sim, rs,
        "обёртка беззнакового обязана совпадать.\nсимулятор={sim:?}\nRust={rs:?}"
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// Q-арифметика fixed-point (фича 0061, задача 0061-03): T10 для цели rust
//
// Наблюдение — через ВЫХОДНОЙ float-порт `probe := acc as float`: поля модели
// приватны, а `repr·2⁻ⁿ` точно представимо в f64, поэтому сверка идёт по
// **битам** f64 (`to_bits`), то есть по представлению q.
// ─────────────────────────────────────────────────────────────────────────────

/// Значение вещественного порта симулятора в битах f64 (точная сверка).
fn sim_f64_bits(unit: &Unit, name: &str) -> u64 {
    match unit.variable(name) {
        Some(Value::Real(f)) => f.to_bits(),
        other => panic!("порт '{name}': ожидался Real, получено {other:?}"),
    }
}

/// Потактовая трасса `probe` (биты f64) симулятора.
fn simulate_f64_trace(fixture: &Path, port: &str) -> Vec<u64> {
    let source = std::fs::read_to_string(fixture).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение юнита");
    let mut trace = Vec::new();
    for _ in 0..TRACE_TICKS {
        let result = unit.tick();
        assert!(
            !matches!(result, TickResult::Failed(_)),
            "симуляция: {result:?}"
        );
        trace.push(sim_f64_bits(&unit, port));
        if result == TickResult::Terminated {
            break;
        }
    }
    trace
}

/// Потактовая трасса `probe` (биты f64) порождённой прошивки rust.
fn rust_f64_trace(
    dir: &Path,
    fixture: &Path,
    basename: &str,
    root: &str,
    ticks: usize,
) -> Vec<u64> {
    let source = std::fs::read_to_string(fixture).expect("фикстура читается");
    takt_lang::compile_to_rust(
        basename,
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение Rust");

    let module = dir.join(format!("{basename}.rs"));
    let driver = format!(
        r#"#[path = "{module}"]
mod generated;
use generated::{{Hal, OutF64Port, {root}}};
use std::cell::RefCell;
use std::rc::Rc;

struct Probe {{ reg: Rc<RefCell<f64>> }}

impl Hal for Probe {{
    fn write_f64(&mut self, port: OutF64Port, value: f64) {{
        assert!(matches!(port, OutF64Port::Probe), "неожиданный порт");
        *self.reg.borrow_mut() = value;
    }}
}}

fn main() {{
    let reg = Rc::new(RefCell::new(0f64));
    let mut model = {root}::new(Probe {{ reg: Rc::clone(&reg) }});
    model.init();
    for _ in 0..{ticks} {{
        model.tick();
        println!("TICK {{}}", reg.borrow().to_bits());
    }}
}}
"#,
        module = module.display(),
    );
    std::fs::write(dir.join("driver.rs"), driver).expect("запись драйвера");
    let build = Command::new("rustc")
        .current_dir(dir)
        .args(["--edition", "2021", "driver.rs", "-o", "driver"])
        .output()
        .expect("запуск rustc");
    assert!(
        build.status.success(),
        "rustc не собрал драйвер:\n{}",
        String::from_utf8_lossy(&build.stderr)
    );
    let run = Command::new(dir.join("driver"))
        .current_dir(dir)
        .output()
        .expect("запуск драйвера");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter_map(|line| line.strip_prefix("TICK "))
        .map(|v| v.trim().parse::<u64>().expect("биты f64 — целое"))
        .collect()
}

/// Фикс 0061-01 (цель rust): перенос идёт к **W**, а не к ширине хранения.
///
/// ⚠️ Формат `q(6, 6)` выбран намеренно: `W = 12`, хранение — 16 бит. При
/// `q(8, 8)` обе границы совпадают, и прежняя сверка расхождения не видела —
/// это была дыра в **покрытии**, а не в проверке.
#[test]
fn fixed_wrap_to_width_matches_generated_rust() {
    let dir = build_dir("rsfixedw12");
    let fixture = Path::new("tests/data/eval/conformance_fixed_probe_w12.takt");
    let sim = simulate_f64_trace(fixture, "probe");
    assert_eq!(
        sim,
        vec![
            (-26.0f64).to_bits(),
            (-18.0f64).to_bits(),
            (-10.0f64).to_bits(),
            (-2.0f64).to_bits(),
            (6.0f64).to_bits(),
            (14.0f64).to_bits(),
        ],
        "q(6,6): 1920 + 512 = 2432 → перенос к 12 битам даёт −26.0; \
         перенос к 16 битам дал бы 38.0"
    );

    if !rustc_available() {
        eprintln!("[ПРОПУСК] fixed_wrap_to_width_matches_generated_rust: rustc не найден");
        return;
    }
    let rs = rust_f64_trace(&dir, fixture, "rsfixedw12", "Rsfixedw12", sim.len());
    assert_eq!(
        sim, rs,
        "перенос к W (не к ширине хранения) обязан совпасть с Rust.\n\
         симулятор={sim:?}\nRust={rs:?}"
    );
}

/// A4/A5/A6 (цель rust, фича 0170): насыщение `q(6, 6) sat` прижимает к
/// границам **формата** на обеих границах.
///
/// ⚠️ Наблюдаемое — float-порт, а не поле: поля цели `rust` приватны, а q-порта
/// у неё нет (RS-016). Значения кратны 2⁻⁶ и точны в f64, поэтому сверка идёт
/// по битам — то есть по представлению q.
///
/// ⚠️ Прижатие идёт к границам формата (`W = 12`), а не хранения (`i16`):
/// `.clamp(-2048, 2047)` против `as i16`, который вернул бы перенос. Формат
/// `q(8, 8)` этого различия не показывает (урок фикса 0061-01).
#[test]
fn fixed_saturation_matches_generated_rust() {
    let dir = build_dir("rsfixedsatw12");
    let fixture = Path::new("tests/data/eval/conformance_fixed_sat_probe_w12.takt");
    let sim = simulate_f64_trace(fixture, "probe");
    assert_eq!(
        sim,
        vec![
            (24.0f64).to_bits(),
            (31.984375f64).to_bits(),
            (7.984375f64).to_bits(),
            (-16.015625f64).to_bits(),
            (-32.0f64).to_bits(),
            (-32.0f64).to_bits(),
        ],
        "q(6,6) sat: 1536 + 1536 = 3072 → прижато к 2047 (31.984375); перенос дал \
         бы −1024 (−16.0). Снизу: −2561 → −2048 (−32.0)"
    );

    if !rustc_available() {
        eprintln!("[ПРОПУСК] fixed_saturation_matches_generated_rust: rustc не найден");
        return;
    }
    let rs = rust_f64_trace(&dir, fixture, "rsfixedsatw12", "Rsfixedsatw12", sim.len());
    assert_eq!(
        sim, rs,
        "насыщение обязано совпасть с Rust побитово.\nсимулятор={sim:?}\nRust={rs:?}"
    );
}

/// T10/A4 (цель rust): побитовая потактовая сверка Q-арифметики с симулятором —
/// включая отрицательные и floor к −∞ у `*` (S2: repr −2, т.е. −0.0078125).
#[test]
fn fixed_point_arithmetic_matches_generated_rust() {
    let dir = build_dir("rsfixed");
    let fixture = Path::new("tests/data/eval/conformance_fixed_probe.takt");
    let sim = simulate_f64_trace(fixture, "probe");
    // Пиннинг битов: -3.0, -1.5, -0.0078125, 1.9921875 (все точны в f64) — и
    // дальше накопление продолжается: состояние с телом автомат не завершает
    // (фича 0534, правило стабильности состояния).
    assert_eq!(
        sim,
        vec![
            (-3.0f64).to_bits(),
            (-1.5f64).to_bits(),
            (-0.0078125f64).to_bits(),
            (1.9921875f64).to_bits(),
            (3.9921875f64).to_bits(),
            (5.9921875f64).to_bits(),
        ],
        "трасса probe (repr q(8,8) / 256) — эталон Q-арифметики симулятора"
    );

    if !rustc_available() {
        eprintln!("[ПРОПУСК] fixed_point_arithmetic_matches_generated_rust: rustc не найден");
        return;
    }
    let rs = rust_f64_trace(&dir, fixture, "rsfixed", "Rsfixed", sim.len());
    assert_eq!(
        sim, rs,
        "Q-арифметика симулятора и порождённого Rust обязана совпасть ПОБИТОВО \
         (биты f64 наблюдаемого porta).\nсимулятор={sim:?}\nRust={rs:?}"
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// Прозрачный float → q(m, n), embedded-путь (фича 0096, задача 0096-03)
//
// Поля цели rust приватны, а q-выходной порт rust не поддерживает (RS-016 — порт
// только бит/число), поэтому Q-модель без портов рантайм-наблюдать нечем. Сверка —
// **byte-equality**: вывод float-фикстуры под --float-embedded обязан совпасть
// БАЙТ-В-БАЙТ с выводом явного q-двойника (одинаковый basename). Так float→q
// наследует уже проверенную 0061 rust-Q-арифметику (fixed_point_..._rust выше).
// ─────────────────────────────────────────────────────────────────────────────

const FLOAT_Q_FIXTURE: &str = "tests/data/eval/conformance_float_q.takt";
const FLOAT_Q_TWIN: &str = "tests/data/eval/conformance_float_q_twin.takt";

/// Опции embedded-Q для `float` (фича 0096).
#[allow(clippy::field_reassign_with_default)] // GenerateOptions — #[non_exhaustive]
fn float_embedded_opts(m: u8, n: u8) -> takt_lang::generator::GenerateOptions {
    let mut o = takt_lang::generator::GenerateOptions::default();
    o.float_as_q = Some((m, n));
    o.float_embedded = true;
    o
}

/// T6/A4 (цель rust, embedded): `float` под `--float-embedded` даёт БАЙТ-В-БАЙТ
/// тот же rust, что явный `q(8, 8)`. Одинаковый basename → символы совпадают, а
/// содержимое — только если трансформация даёт ровно проверенный q-кодоген.
#[test]
fn float_embedded_matches_explicit_q_rust() {
    let dir = build_dir("float_eq");
    let float_src = std::fs::read_to_string(FLOAT_Q_FIXTURE).expect("float-фикстура");
    let twin_src = std::fs::read_to_string(FLOAT_Q_TWIN).expect("q-двойник");
    let out_f = dir.join("f");
    let out_q = dir.join("q");
    std::fs::create_dir_all(&out_f).unwrap();
    std::fs::create_dir_all(&out_q).unwrap();
    takt_lang::compile_to_rust(
        "twin",
        &float_src,
        out_f.to_str().unwrap(),
        &[],
        &float_embedded_opts(8, 8),
    )
    .expect("float → rust");
    takt_lang::compile_to_rust(
        "twin",
        &twin_src,
        out_q.to_str().unwrap(),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("q → rust");
    let rs_f = std::fs::read_to_string(out_f.join("twin.rs")).expect(".rs float");
    let rs_q = std::fs::read_to_string(out_q.join("twin.rs")).expect(".rs q");
    // ⚠️ Сравнивается КОД: фикстуры — разные файлы, и комментарии авторов в
    // них законно различаются, а с задачи 0535-05 они доезжают до вывода.
    // Предмет проверки — совпадение порождённого кода, а не текста.
    assert_eq!(
        crate::target_code::code_only(&rs_f),
        crate::target_code::code_only(&rs_q),
        "float→q(8,8) под --float-embedded обязан дать ровно тот же rust, что явный q(8,8)"
    );
}

/// A3/T5 (цель rust native по умолчанию): `--float-as-q` без `--float-embedded`
/// оставляет `float` нативным `f64`. Гейт переключения: с `--float-embedded` —
/// `i16`. Молчаливого Q быть не должно.
#[test]
#[allow(clippy::field_reassign_with_default)] // GenerateOptions — #[non_exhaustive]
fn float_as_q_without_embedded_is_native_rust() {
    let dir = build_dir("float_native");
    let source = std::fs::read_to_string(FLOAT_Q_FIXTURE).expect("фикстура");
    let mut opts = takt_lang::generator::GenerateOptions::default();
    opts.float_as_q = Some((8, 8)); // точность задана, embedded НЕ включён
    takt_lang::compile_to_rust("cfq", &source, dir.to_str().unwrap(), &[], &opts)
        .expect("порождение rust");
    let rs = std::fs::read_to_string(dir.join("cfq.rs")).expect(".rs");
    assert!(
        rs.contains("f64"),
        "без --float-embedded float остаётся native f64 (не i16).\n{rs}"
    );
}

// ── Модель времени: профиль «часы» через внешний `now_ms` (фича 0134) ─────────

/// Трасса симулятора при 1 мс на такт (эталон профиля «часы»).
fn simulate_time_trace(source: &str, port: &str, ticks: usize) -> Vec<i128> {
    let (ast, _) = takt_lang::parse(source, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение юнита");
    let mut trace = Vec::new();
    for step in 0..ticks {
        unit.set_time_ns(i64::try_from(step).unwrap() * 1_000_000);
        let result = unit.tick();
        assert!(
            !matches!(result, TickResult::Failed(_)),
            "падение: {result:?}"
        );
        trace.push(sim_value(&unit, port));
    }
    trace
}

/// Трасса порождённой прошивки: фиктивный `now_ms` = модельное время (1 мс/такт).
fn rust_time_trace(dir: &Path, path: &Path, root: &str, variant: &str, ticks: usize) -> Vec<i128> {
    let source = std::fs::read_to_string(path).expect("фикстура");
    takt_lang::compile_to_rust(
        "rstime",
        &source,
        dir.to_str().unwrap(),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение rust");
    let module = dir.join("rstime.rs");
    let driver = format!(
        r#"#[path = "{module}"]
mod generated;
use generated::{{Hal, OutU8Port, {root}}};
use std::cell::RefCell;
use std::rc::Rc;

struct Probe {{ reg: Rc<RefCell<u8>>, now: Rc<RefCell<u64>> }}
impl Hal for Probe {{
    fn write_u8(&mut self, port: OutU8Port, value: u8) {{
        assert!(matches!(port, OutU8Port::{variant}), "неожиданный порт");
        *self.reg.borrow_mut() = value;
    }}
    fn now_ms(&mut self) -> u64 {{ *self.now.borrow() }}
}}

fn main() {{
    let reg = Rc::new(RefCell::new(0u8));
    let now = Rc::new(RefCell::new(0u64));
    let mut model = {root}::new(Probe {{ reg: Rc::clone(&reg), now: Rc::clone(&now) }});
    // Вход стартового состояния «до такта 1»: время такта 1 — ноль.
    model.init();
    for step in 0..{ticks} {{
        *now.borrow_mut() = step as u64; // 1 мс на такт, начиная с нуля
        model.tick();
        println!("TICK {{}}", reg.borrow());
    }}
}}
"#,
        module = module.display(),
    );
    std::fs::write(dir.join("driver.rs"), driver).expect("драйвер");
    let build = Command::new("rustc")
        .current_dir(dir)
        .args(["--edition", "2021", "driver.rs", "-o", "driver"])
        .output()
        .expect("запуск rustc");
    assert!(
        build.status.success(),
        "rustc не собрал драйвер профиля «часы»:\n{}",
        String::from_utf8_lossy(&build.stderr)
    );
    let run = Command::new(dir.join("driver"))
        .current_dir(dir)
        .output()
        .expect("запуск драйвера");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter_map(|l| l.strip_prefix("TICK "))
        .map(|v| v.trim().parse::<i128>().expect("значение"))
        .collect()
}

/// Выдержка `after 5ms` в профиле «часы» срабатывает на том же такте у симулятора
/// и у порождённого Rust (внешний `now_ms`, 1 мс на такт).
#[test]
fn after_clock_profile_matches_generated_rust() {
    const FIXTURE: &str = "tests/data/eval/conformance_after_rust.takt";
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура");
    let ticks = 8usize;
    let sim = simulate_time_trace(&source, "level", ticks);
    // 5 мс при 1 мс/такт — уровень становится 1 на 6-м такте (индекс 5).
    assert_eq!(
        sim,
        vec![0, 0, 0, 0, 0, 1, 1, 1],
        "эталон профиля «часы»: {sim:?}"
    );
    if !rustc_available() {
        eprintln!("[ПРОПУСК] after_clock_profile_matches_generated_rust: rustc не найден");
        return;
    }
    let dir = build_dir("rstime");
    let path = fixture(&dir, "conformance_after_rust", &source);
    let rs = rust_time_trace(&dir, &path, "Rstime", "Level", ticks);
    assert_eq!(
        sim, rs,
        "трассы симулятора и порождённого Rust (профиль «часы») обязаны совпадать\n\
         симулятор={sim:?}\nRust={rs:?}"
    );
}

/// Периодический блок `every 3ms` (профиль «часы») срабатывает у симулятора и у
/// порождённого Rust на одних тактах (3, 6, 9) — счётчик `led` растёт синхронно.
#[test]
fn every_period_matches_generated_rust() {
    const FIXTURE: &str = "tests/data/eval/conformance_every.takt";
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура");
    let ticks = 10usize;
    let sim = simulate_time_trace(&source, "led", ticks);
    assert_eq!(
        sim,
        vec![0, 0, 0, 1, 1, 1, 2, 2, 2, 3],
        "эталон периода `every`: {sim:?}"
    );
    if !rustc_available() {
        eprintln!("[ПРОПУСК] every_period_matches_generated_rust: rustc не найден");
        return;
    }
    let dir = build_dir("rsevery");
    let path = fixture(&dir, "conformance_every", &source);
    let rs = rust_time_trace(&dir, &path, "Rstime", "Led", ticks);
    assert_eq!(
        sim, rs,
        "трассы симулятора и порождённого Rust (`every`) обязаны совпадать\n\
         симулятор={sim:?}\nRust={rs:?}"
    );
}

/// **Структуры в цели `rust` (фича 0293): поля доезжают до прошивки.**
///
/// Прежде цель отвечала `RS-011` («поля структур в цели rust пока не
/// транслируются»), то есть структура не переводилась дальше объявления
/// переменной. Наблюдаемая — `sum = kp + ki`, выведенная в порт: она различает
/// **значение**, а не факт компиляции; порядок полей в литерале структуры
/// `rustc` не проверяет по смыслу — перестановка дала бы собирающийся код с
/// другими значениями.
#[test]
fn struct_fields_match_generated_rust() {
    let tag = "structinit";
    let source = include_str!("../data/eval/conformance_struct_init.takt");
    let dir = build_dir(tag);
    let path = fixture(&dir, tag, source);
    let sim = simulate_trace(&path, "probe");
    assert!(
        sim.contains(&5),
        "контроль: эталон обязан дать probe = 5 (2 + 3), трасса={sim:?}"
    );
    if !rustc_available() {
        eprintln!("[ПРОПУСК] {tag}: rustc не найден — сверка с прошивкой не выполнена");
        return;
    }
    let rs = rust_trace(&dir, &path, tag, "Structinit", "Probe", sim.len());
    assert_eq!(
        sim, rs,
        "потактовые трассы эталона и порождённого Rust обязаны совпадать:\n\
         эталон={sim:?}\nRust={rs:?}"
    );
}
