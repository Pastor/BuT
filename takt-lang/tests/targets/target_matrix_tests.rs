//! Сплошной перебор форм по всем целям и их инструментам.
//!
//! # Что доказывает набор
//!
//! Матрица "вид обращения к корню x форма реализации состояния" (носитель -
//! [`matrix_probes`](super::matrix_probes), 40 случаев) прогоняется через
//! **восемь** целей, и для каждой проверяется не код возврата `taktc`, а
//! **вердикт её инструмента**: `cc` для `c`/`c-hal`, `iec2c` + `cc` для
//! `st`/`st-at`, `clippy -D warnings` для `rust`, `verilator` и `yosys` для
//! `sv`/`sv-mmio`.
//!
//! Мера предосторожности не теоретическая: тем же перебором у цели `c` нашёлся вход,
//! дававший невалидный C при нулевом коде возврата. Прочие цели проверялись входами
//! отдельных фич - то есть выборкой.
//!
//! # Границы целей - часть таблицы, а не исключение из неё
//!
//! Там, где цель отказывает **законно**, ожидается именно её код отказа, и молчаливая
//! смена поведения (перевела то, что не умеет; отказала там, где умела) роняет тест
//! так же, как невалидный вывод:
//!
//! | Цель | Вид | Код | Почему |
//! |---|---|---|---|
//! | `rust` | `var_init` | `RS-017` | функция порождается свободной и состояния модели не видит |
//! | `sv`, `sv-mmio` | `var_init` | `SV-002` | ветвь сброса выражений не вычисляет |
//! | `sv`, `sv-mmio` | `extern_call` | `SV-005` | внешней функции в синтезируемом RTL нет |

use std::path::{Path, PathBuf};
use std::process::Command;

use super::matrix_probes::{
    Kind, Shape, Touch, case_name, cases, extra_flags, library_files, source,
};

/// Ожидание для тройки "цель x вид обращения x форма объявления": перевод либо отказ
/// **названным** кодом.
///
/// Таблица снята прогоном. Границы целей - её часть: цель, которая вдруг перевела то,
/// что не умеет, роняет тест так же, как невалидный вывод.
pub(crate) fn refusal(target: &str, touch: Touch, kind: Kind) -> Option<&'static str> {
    match (target, touch, kind) {
        // Инициализатор от переменной корня: у `rust` функция порождается свободной и
        // состояния модели не видит, у `sv` ветвь сброса выражений не вычисляет.
        ("rust", Touch::VarInit, _) => Some("RS-017"),
        ("sv" | "sv-mmio", Touch::VarInit, _) => Some("SV-002"),
        // Инициализатор массива значением другой переменной: в C массив не
        // присваивается.
        ("c" | "c-hal", Touch::VarInit, Kind::Array) => Some("CC-017"),
        // Внешней функции в синтезируемом RTL нет.
        ("sv" | "sv-mmio", Touch::ExternCall, _) => Some("SV-005"),
        // Цикл в синтезируемом RTL обязан разворачиваться в схему, то есть иметь
        // границы, известные на этапе синтеза. Их даёт только форма `for` с литеральным
        // началом и сравнением переменной цикла; прочие четыре - законная граница цели,
        // а не пробел (замер 0477).
        (
            "sv" | "sv-mmio",
            Touch::LoopForNoInit | Touch::LoopWhile | Touch::LoopBreak | Touch::LoopContinue,
            _,
        ) => Some("SV-002"),
        // В IEC 61131-3 есть `EXIT` (аналог `break`), а продолжения итерации нет вовсе:
        // `continue` целью `st` не выражается (замер 0477).
        ("st" | "st-at", Touch::LoopContinue, _) => Some("ST-011"),
        // Порт перечислимого типа прежде отвергали `rust` (`RS-016`) и `st-at`
        // (`ST-004`) - записи здесь и стояли. обе границы сняла: перечисление - скаляр,
        // его ширину и знак даёт `enum_facts`, и порт ложится на метод HAL-трейта либо
        // на локацию `AT %QB`. Записи удалены, а не помечены пропуском: ожидание
        // отказа, которого больше нет, - то же расхождение с фактом, ради которого
        // заведён этот набор. Двунаправленный порт в регистровом файле: у шины сторона
        // одна, и выразить `inout` ею нельзя.
        ("sv-mmio", Touch::InoutRead | Touch::InoutWrite, _) => Some("SV-006"),
        // Вычисляемая выдержка в профиле "часы": у цели `st` переменный `PT` таймера
        // требует своей обвязки - названная граница.
        ("st" | "st-at", Touch::TimeDurationVar, _) => Some("ST-016"),
        // Полный импорт вносит только модель-Контейнер файла; вложенная модель снаружи
        // не видна, и взять её реализацией нельзя. Отказ приходит из семантики, то есть
        // у всех целей одинаково.
        (_, Touch::ImportNestedModel, _) => Some("SE-106"),
        _ => None,
    }
}

fn tool(bin: &str) -> bool {
    Command::new(bin)
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// `(бинарник iec2c, каталог lib MatIEC)` - если оба на месте.
fn iec2c_available() -> Option<(PathBuf, PathBuf)> {
    let prefix = std::env::var("IEC2C_PREFIX")
        .map(PathBuf::from)
        .unwrap_or_else(|_| {
            PathBuf::from(std::env::var("HOME").unwrap_or_default()).join(".local")
        });
    let bin = prefix.join("bin").join("iec2c");
    let lib = prefix.join("share").join("matiec").join("lib");
    (bin.is_file() && lib.join("C").is_dir()).then_some((bin, lib))
}

/// Уникальный по тесту каталог.
fn work_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("main")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0450_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог теста");
    dir
}

/// Итог компиляции: путь к каталогу вывода либо код отказа цели.
enum Emitted {
    Ok(PathBuf),
    Refused(String),
}

/// Компилирует случай заданной целью.
fn emit(dir: &Path, target: &str, text: &str, touch: Touch, mode: &str) -> Emitted {
    let input = dir.join("probe.takt");
    std::fs::write(&input, text).expect("запись пробы");
    // Подключаемые файлы кладутся рядом: импорт ищется в каталоге импортёра, и пути
    // поиска задавать не нужно.
    for file in library_files(touch) {
        std::fs::write(dir.join(file.name), file.text).expect("запись библиотеки");
    }
    let out_dir = dir.join("out");
    let out = Command::new(env!("CARGO_BIN_EXE_taktc"))
        .arg("compile")
        .args(["-t", target])
        // Режим параметров - часть случая: дефект дубля сигнала у `sv-mmio` жил только
        // в `specialize`.
        .arg(format!("--parameters={mode}"))
        // Ключи, которых требует вид обращения: внешняя карта, `-D`.
        .args(
            extra_flags(touch)
                .into_iter()
                .map(|flag| flag.replace("{dir}", &dir.display().to_string())),
        )
        .arg(&input)
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("запуск taktc compile");
    if out.status.success() {
        return Emitted::Ok(out_dir);
    }
    let stderr = String::from_utf8_lossy(&out.stderr);
    let code = stderr
        .split_once('[')
        .and_then(|(_, rest)| rest.split_once(']'))
        .map(|(code, _)| code.to_string())
        .unwrap_or_else(|| stderr.trim().to_string());
    Emitted::Refused(code)
}

/// Случаи вместе с режимом `--parameters`.
///
/// Второй режим гоняется **только** у видов с параметром: у прочих он вывода не меняет,
/// и удвоение перебора было бы платой ни за что.
fn cases_with_modes() -> Vec<(Shape, Touch, Kind, &'static str)> {
    let mut out = Vec::new();
    for (shape, touch, kind) in cases() {
        out.push((shape, touch, kind, "assign"));
        if matches!(
            touch,
            Touch::ParameterDefault | Touch::ParameterArgument | Touch::ParameterExpression
        ) {
            out.push((shape, touch, kind, "specialize"));
        }
    }
    out
}

/// Прогоняет матрицу через одну цель; `check` судит порождённый каталог.
fn sweep(target: &str, check: &dyn Fn(&Path, &Path, Touch) -> Result<(), String>) -> Vec<String> {
    let mut failures = Vec::new();
    for (shape, touch, kind, mode) in cases_with_modes() {
        let name = case_name(shape, touch, kind);
        let name = if mode == "specialize" {
            format!("{name}_specialize")
        } else {
            name
        };
        let tag = format!("{target}_{name}");
        let dir = work_dir(&tag);
        let text = source(shape, touch, kind);
        match (
            emit(&dir, target, &text, touch, mode),
            refusal(target, touch, kind),
        ) {
            (Emitted::Ok(out), None) => {
                if let Err(err) = check(&dir, &out, touch) {
                    failures.push(format!("{name}: {err}"));
                }
            }
            (Emitted::Ok(_), Some(code)) => failures.push(format!(
                "{name}: цель перевела вход, а ожидался отказ {code} — граница исчезла молча"
            )),
            (Emitted::Refused(code), Some(expected)) if code == expected => {}
            (Emitted::Refused(code), Some(expected)) => {
                failures.push(format!("{name}: отказ {code}, а ожидался {expected}"))
            }
            (Emitted::Refused(code), None) => {
                failures.push(format!("{name}: цель отказала кодом {code}"))
            }
        }
    }
    failures
}

/// Итог перебора: список расхождений обязан быть пуст.
fn verdict(target: &str, failures: Vec<String>) {
    assert!(
        failures.is_empty(),
        "цель `{target}`: {} случаев из {} разошлись с ожиданием:\n{}",
        failures.len(),
        cases_with_modes().len(),
        failures.join("\n")
    );
}

/// Доезжает ли обязательство до цели - контроль осмысленности оси формул.
///
/// Без него перебор был бы зелен и на выводе, из которого формула пропала:
/// охранная формула - это `assert` у трёх целей, а темпоральное
/// свойство до целей не доезжает вовсе (предмет верификации). Проверяется
/// **наличие** обязательства, а не его текст: текст - предмет 0235.
fn assertion_expected(touch: Touch) -> Option<bool> {
    match touch {
        Touch::InvariantModel | Touch::InvariantState | Touch::GuardFormula => Some(true),
        // Формула-Оператор доезжает до целей так же, как формула-объявление: проверка
        // обязана быть в выводе.
        Touch::GuardInBlock | Touch::GuardInFunction | Touch::GuardInNested => Some(true),
        // Темпоральная не доезжает - ни объявлением, ни оператором.
        Touch::LtlFormula | Touch::LtlInBlock => Some(false),
        _ => None,
    }
}

/// Проверяет присутствие `assert` в выводе цели, если ось этого требует.
fn check_assertion(text: &str, touch: Touch, marker: &str) -> Result<(), String> {
    let Some(expected) = assertion_expected(touch) else {
        return Ok(());
    };
    let found = text.contains(marker);
    if found == expected {
        return Ok(());
    }
    Err(if expected {
        format!("обязательство не доехало до цели: '{marker}' в выводе нет")
    } else {
        format!("темпоральное свойство доехало до цели: '{marker}' в выводе есть")
    })
}

/// Обязано ли именованное условие быть раскрыто в выводе.
///
/// Контроль осмысленности оси: без него перебор был бы зелен и на выводе, где условие
/// Потеряно, - а именно так и выглядел у цели `c` (ссылка на неопределённый
/// идентификатор при нулевом коде возврата). Признак прямой: в выводе стоит значение
/// условия (`200`), а имени `Low` нет вовсе - ни одна цель его не печатает (замер
/// 2026-09-01).
fn condition_expanded(touch: Touch) -> bool {
    matches!(
        touch,
        Touch::CondOnEdge | Touch::CondInBody | Touch::CondInGuard | Touch::CondNested
    )
}

/// Проверяет раскрытие именованного условия, если ось этого требует.
///
/// Цель `st` охранную формулу не печатает (`ST-022`), поэтому вид `CondInGuard` у неё
/// раскрытия не даёт - и это не потеря, а известная граница: условие там просто некуда
/// поместить.
fn check_condition(text: &str, touch: Touch, guard_printed: bool) -> Result<(), String> {
    if !condition_expanded(touch) {
        return Ok(());
    }
    if touch == Touch::CondInGuard && !guard_printed {
        return Ok(());
    }
    if !text.contains("200") {
        return Err("именованное условие не раскрыто: значения '200' в выводе нет".to_string());
    }
    if text.contains("Low") || text.contains("Nested") {
        return Err("имя условия попало в вывод: раскрытие не выполнено".to_string());
    }
    Ok(())
}

/// Какой адрес обязан стоять в выводе цели `c-hal` - контроль оси адресации.
///
/// Без него перебор был бы зелен и тогда, когда адрес молча потерян или взят не из того
/// источника: приоритет `inline < address < карта` иначе никем в переборе не
/// проверяется.
fn expected_address(touch: Touch) -> Option<&'static str> {
    match touch {
        Touch::AddressOperator => Some("0x40000200u"),
        // Арифметику адреса вычисляет компилятор: `0x40000000 + 8`.
        Touch::AddressExpression => Some("0x40000008u"),
        // `-DBASE=0x40000000` плюс `+ 4`.
        Touch::AddressDefine => Some("0x40000004u"),
        // Внешняя карта перекрывает inline-адрес объявления (0x40000100).
        Touch::AddressMap => Some("0x200004u"),
        _ => None,
    }
}

/// Проверяет адрес в выводе `c-hal`, если ось этого требует.
fn check_address(text: &str, touch: Touch) -> Result<(), String> {
    let Some(expected) = expected_address(touch) else {
        return Ok(());
    };
    if !text.contains(expected) {
        return Err(format!("в выводе нет ожидаемого адреса '{expected}'"));
    }
    // Карта обязана перекрыть inline: старого адреса в выводе быть не должно.
    if touch == Touch::AddressMap && text.contains("0x40000100u") {
        return Err("внешняя карта не перекрыла inline-адрес объявления".to_string());
    }
    Ok(())
}

/// Собирает порождённый C флагами проверки цели.
fn cc_builds(dir: &Path, out: &Path, touch: Touch) -> Result<(), String> {
    // Наличие `assert` у цели `c` не проверяется: её вывод содержит `assert(0 !=
    // model)` в каждой функции - маркер неразличим. Ось формул сторожится у `rust` и
    // `sv`, где обязательство печатается отдельным оператором.
    let cc = Command::new("cc")
        .args([
            "-std=c11",
            "-Wall",
            "-Wextra",
            "-Wno-unused-parameter",
            "-Werror",
            "-c",
        ])
        .arg(out.join("probe.c"))
        .arg("-I")
        .arg(out)
        .arg("-o")
        .arg(dir.join("probe.o"))
        .output()
        .expect("запуск cc");
    if !cc.status.success() {
        return Err(format!(
            "cc отверг вывод:\n{}",
            String::from_utf8_lossy(&cc.stderr)
        ));
    }
    // Адрес проверяется по заголовку цели `c-hal`: там он печатается таблицей
    // размещений. У цели `c` таблицы нет - контроль ей не адресован.
    let header = std::fs::read_to_string(out.join("probe.h")).unwrap_or_default();
    if header.contains("uintptr_t") {
        check_address(&header, touch)?;
    }
    let body = std::fs::read_to_string(out.join("probe.c")).unwrap_or_default();
    check_condition(&body, touch, true)?;
    Ok(())
}

#[test]
fn target_c_accepts_every_shape() {
    if !tool("cc") {
        eprintln!("cc недоступен — перебор пропущен");
        return;
    }
    verdict("c", sweep("c", &cc_builds));
}

#[test]
fn target_c_hal_accepts_every_shape() {
    if !tool("cc") {
        eprintln!("cc недоступен — перебор пропущен");
        return;
    }
    verdict("c-hal", sweep("c-hal", &cc_builds));
}

/// Цели `st`/`st-at`: вывод транслируется `iec2c`, результат собирается `cc`.
fn st_sweep(target: &str) {
    let Some((iec2c, lib)) = iec2c_available() else {
        eprintln!("iec2c недоступен — перебор пропущен");
        return;
    };
    let check = move |dir: &Path, out: &Path, touch: Touch| -> Result<(), String> {
        let work = dir.join("iec");
        std::fs::create_dir_all(&work).expect("рабочий каталог iec2c");
        let run = Command::new(&iec2c)
            .arg("-I")
            .arg(&lib)
            .arg(out.join("probe.st"))
            .current_dir(&work)
            .output()
            .expect("запуск iec2c");
        if !run.status.success() || !work.join("POUS.c").is_file() {
            return Err(format!(
                "iec2c отверг вывод:\n{}",
                String::from_utf8_lossy(&run.stderr)
            ));
        }
        // У цели `st` охранной формулы в выводе нет (`ST-022`), поэтому вид
        // `CondInGuard` раскрытия не даёт - граница названа в `check_condition`.
        let text = std::fs::read_to_string(out.join("probe.st")).unwrap_or_default();
        check_condition(&text, touch, false)
    };
    verdict(target, sweep(target, &check));
}

#[test]
fn target_st_accepts_every_shape() {
    st_sweep("st");
}

#[test]
fn target_st_at_accepts_every_shape() {
    st_sweep("st-at");
}

/// Цель `rust`: вердикт даёт `clippy` - флаги проверки цели, а не `rustc`.
#[test]
fn target_rust_accepts_every_shape() {
    if !tool("clippy-driver") {
        eprintln!("clippy-driver недоступен — перебор пропущен");
        return;
    }
    let check = |dir: &Path, out: &Path, touch: Touch| -> Result<(), String> {
        // Рабочий каталог - каталог случая: без него `clippy-driver` кладёт
        // `libprobe.rlib` в текущий, то есть в дерево репозитория (проверка 0377 ловит
        // такие артефакты, но лучше их не порождать).
        let run = Command::new("clippy-driver")
            .current_dir(dir)
            .args(["--edition", "2021", "--crate-type", "lib", "-D", "warnings"])
            .arg(out.join("probe.rs"))
            .output()
            .expect("запуск clippy-driver");
        if !run.status.success() {
            return Err(format!(
                "clippy отверг вывод:\n{}",
                String::from_utf8_lossy(&run.stderr)
            ));
        }
        let text = std::fs::read_to_string(out.join("probe.rs")).unwrap_or_default();
        check_assertion(&text, touch, "assert!(")?;
        check_condition(&text, touch, true)
    };
    verdict("rust", sweep("rust", &check));
}

/// Цели `sv`/`sv-mmio`: **оба** инструмента - они видят разные половины картины.
fn sv_sweep(target: &str) {
    if !tool("verilator") {
        eprintln!("verilator недоступен — перебор пропущен");
        return;
    }
    let with_yosys = tool("yosys");
    let check = move |_dir: &Path, out: &Path, touch: Touch| -> Result<(), String> {
        let module = out.join("probe.sv");
        let lint = Command::new("verilator")
            .args(["--lint-only", "-Wall"])
            .arg(&module)
            .output()
            .expect("запуск verilator");
        if !lint.status.success() {
            return Err(format!(
                "verilator отверг модуль:\n{}",
                String::from_utf8_lossy(&lint.stderr)
            ));
        }
        if with_yosys {
            let script = format!("read_verilog -sv {}; synth -top probe", module.display());
            let synth = Command::new("yosys")
                .args(["-q", "-p", &script])
                .output()
                .expect("запуск yosys");
            if !synth.status.success() {
                return Err(format!(
                    "yosys не синтезировал модуль:\n{}",
                    String::from_utf8_lossy(&synth.stderr)
                ));
            }
        }
        let text = std::fs::read_to_string(&module).unwrap_or_default();
        check_assertion(&text, touch, "assert (")?;
        check_condition(&text, touch, true)
    };
    verdict(target, sweep(target, &check));
}

#[test]
fn target_sv_accepts_every_shape() {
    sv_sweep("sv");
}

#[test]
fn target_sv_mmio_accepts_every_shape() {
    sv_sweep("sv-mmio");
}

/// Цель `plantuml`: инструмента нет, вердикт - непустая диаграмма.
#[test]
fn target_plantuml_accepts_every_shape() {
    let check = |_dir: &Path, out: &Path, _touch: Touch| -> Result<(), String> {
        let text = std::fs::read_to_string(out.join("probe.puml"))
            .map_err(|e| format!("диаграмма не читается: {e}"))?;
        if text.contains("@startuml") && text.contains("@enduml") {
            Ok(())
        } else {
            Err(format!("диаграмма пуста либо неполна:\n{text}"))
        }
    };
    verdict("plantuml", sweep("plantuml", &check));
}
