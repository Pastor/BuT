//! Сплошной перебор сценариев сборки по всем целям и их инструментам.
//!
//! # Что доказывает набор
//!
//! Флаги `compile` проверялись входами своих фич - каждый в одиночку. Набор перебирает
//! их **сочетаниями**: шесть проб (по одной на предмет флага) x семь сценариев x восемь
//! целей, и вердикт даёт инструмент цели, а не код возврата `taktc`.
//!
//! Улов первого прогона  - пять классов, каждый с валидным на вид выводом
//! при нулевом коде возврата:
//!
//! | Сочетание | Что было |
//! |---|---|
//! | `--bounds-check` + `return d[i]` | `c`: возврат не по всем путям; `rust`: `E0308` |
//! | `--bounds-check` + `var x := d[i]` | переменная уезжала внутрь `if` вместе с областью видимости |
//! | `--inline=auto` + параметр-массив | `c`: `uint8_t a[4] = model->data;` - так массив не инициализируют |
//! | `--inline=auto` + `--bounds-check` | `rust`: отложенное объявление, `clippy::needless_late_init` |
//! | функция читает массив модели | `rust`: `E0308`; `st`: анонимный `ARRAY` в `VAR_IN_OUT` |
//!
//! Корпус матрицы форм этих классов не видит: его оси - обращения к корню, а флаги
//! смотрят на индексацию, функции и число состояний. Прогон всех 295 случаев корпуса
//! через три флага расхождений не дал - оттого у оси и свои пробы (`build_probes`).
//!
//! # Границы - часть таблицы
//!
//! Цель, отказавшая законно, обязана отказать **названным** кодом; молчаливая смена
//! поведения роняет тест так же, как невалидный вывод.

use std::path::{Path, PathBuf};
use std::process::Command;

use super::build_probes::{PROBES, Probe, SCENARIOS, Scenario, probe_name, source};

/// Законный отказ цели на сочетании "проба x сценарий".
///
/// Таблица снята прогоном.
fn refusal(target: &str, probe: Probe, scenario: &Scenario) -> Option<&'static str> {
    let inlined = scenario.flags.contains(&"--inline=auto");
    match (target, probe) {
        // Досрочный возврат цель `sv` печатать не умеет: возврат там - присваивание
        // имени функции, исполнения оно не прерывает. Под подстановкой функции не
        // остаётся вовсе, и вывод законен - это и проверяет вторая половина условия.
        ("sv" | "sv-mmio", Probe::SmallFunctions) if !inlined => Some("SV-002"),
        _ => None,
    }
}

/// Доступен ли инструмент.
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
        .join(format!("takt_0466_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог теста");
    dir
}

/// Итог компиляции: каталог вывода либо код отказа.
enum Emitted {
    Ok(PathBuf),
    Refused(String),
}

/// Нужна ли цели внешняя карта адресов для синтетического порта.
///
/// `--bounds-check` заводит выходной порт `bounds_fault`, а целям с адресами адрес
/// обязателен каждому порту - автор задаёт его картой, и именно это здесь и проверяется
/// (сам отказ без карты проверяют тесты guard).
fn needs_fault_address(target: &str, scenario: &Scenario) -> bool {
    matches!(target, "c-hal" | "st-at") && scenario.flags.contains(&"--bounds-check")
}

/// Компилирует пробу заданной целью в заданном сценарии.
fn emit(dir: &Path, target: &str, probe: Probe, scenario: &Scenario) -> Emitted {
    let input = dir.join("probe.takt");
    std::fs::write(&input, source(probe)).expect("запись пробы");
    let mut command = Command::new(env!("CARGO_BIN_EXE_taktc"));
    command.arg("compile").args(["-t", target]);
    command.args(scenario.flags);
    if needs_fault_address(target, scenario) {
        let map = dir.join("fault.map");
        std::fs::write(&map, "bounds_fault = 0x40000200:0;\n").expect("запись карты");
        command.arg("--address-map").arg(&map);
    }
    let out_dir = dir.join("out");
    let out = command
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

/// Прогоняет пробы x сценарии через одну цель.
fn sweep(target: &str, check: &dyn Fn(&Path, &Path) -> Result<(), String>) -> Vec<String> {
    let mut failures = Vec::new();
    for probe in PROBES {
        for scenario in SCENARIOS {
            let name = format!("{}_{}", probe_name(*probe), scenario.name);
            let dir = work_dir(&format!("{target}_{name}"));
            match (
                emit(&dir, target, *probe, scenario),
                refusal(target, *probe, scenario),
            ) {
                (Emitted::Ok(out), None) => {
                    if let Err(err) = check(&dir, &out) {
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
    }
    failures
}

/// Итог перебора: список расхождений обязан быть пуст.
fn verdict(target: &str, failures: Vec<String>) {
    let total = PROBES.len() * SCENARIOS.len();
    assert!(
        failures.is_empty(),
        "цель `{target}`: {} случаев из {total} разошлись с ожиданием:\n{}",
        failures.len(),
        failures.join("\n")
    );
}

/// Собирает порождённый C флагами проверки цели.
fn cc_builds(dir: &Path, out: &Path) -> Result<(), String> {
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
    Ok(())
}

#[test]
fn scenarios_hold_for_c() {
    if !tool("cc") {
        eprintln!("cc недоступен — перебор пропущен");
        return;
    }
    verdict("c", sweep("c", &cc_builds));
}

#[test]
fn scenarios_hold_for_c_hal() {
    if !tool("cc") {
        eprintln!("cc недоступен — перебор пропущен");
        return;
    }
    verdict("c-hal", sweep("c-hal", &cc_builds));
}

/// Цели `st`/`st-at`: вердикт даёт `iec2c`.
fn st_sweep(target: &str) {
    let Some((iec2c, lib)) = iec2c_available() else {
        eprintln!("iec2c недоступен — перебор пропущен");
        return;
    };
    let check = move |dir: &Path, out: &Path| -> Result<(), String> {
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
        Ok(())
    };
    verdict(target, sweep(target, &check));
}

#[test]
fn scenarios_hold_for_st() {
    st_sweep("st");
}

#[test]
fn scenarios_hold_for_st_at() {
    st_sweep("st-at");
}

#[test]
fn scenarios_hold_for_rust() {
    if !tool("clippy-driver") {
        eprintln!("clippy-driver недоступен — перебор пропущен");
        return;
    }
    let check = |dir: &Path, out: &Path| -> Result<(), String> {
        // Рабочий каталог - каталог случая: иначе `libprobe.rlib` ложится в дерево
        // репозитория.
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
        Ok(())
    };
    verdict("rust", sweep("rust", &check));
}

/// Цели `sv`/`sv-mmio`: вердикт даёт `verilator` (и `yosys`, если он есть).
fn sv_sweep(target: &str) {
    if !tool("verilator") {
        eprintln!("verilator недоступен — перебор пропущен");
        return;
    }
    let check = move |_dir: &Path, out: &Path| -> Result<(), String> {
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
        Ok(())
    };
    verdict(target, sweep(target, &check));
}

#[test]
fn scenarios_hold_for_sv() {
    sv_sweep("sv");
}

#[test]
fn scenarios_hold_for_sv_mmio() {
    sv_sweep("sv-mmio");
}

/// Цель `plantuml`: инструмента нет - вердикт даёт непустая диаграмма.
///
/// Сценарии с `--fsm=table` сюда не входят: этой цели флаг не адресован, и CLI
/// отказывает **до** компиляции (носитель `compile_cli::target_flags`).
#[test]
fn scenarios_hold_for_plantuml() {
    let mut failures = Vec::new();
    for probe in PROBES {
        for scenario in SCENARIOS {
            if scenario.flags.contains(&"--fsm=table") {
                continue;
            }
            let name = format!("{}_{}", probe_name(*probe), scenario.name);
            let dir = work_dir(&format!("plantuml_{name}"));
            match emit(&dir, "plantuml", *probe, scenario) {
                Emitted::Ok(out) => {
                    let text = std::fs::read_to_string(out.join("probe.puml")).unwrap_or_default();
                    if !text.contains("@startuml") || !text.contains("@enduml") {
                        failures.push(format!("{name}: диаграмма пуста либо неполна"));
                    }
                }
                Emitted::Refused(code) => {
                    failures.push(format!("{name}: цель отказала кодом {code}"))
                }
            }
        }
    }
    assert!(
        failures.is_empty(),
        "цель `plantuml`: {} случаев разошлись с ожиданием:\n{}",
        failures.len(),
        failures.join("\n")
    );
}

/// Флаги подкоманды `compile`, которые разбирает CLI.
///
/// Список - Данные теста, а не выведенный факт: разбор написан кодом, и реестра флагов
/// у него нет. Зато сверка двусторонняя по сути: флаг, пропавший из разбора, роняет
/// вторую половину теста, а флаг, забытый в справке, - первую.
const COMPILE_FLAGS: &[&str] = &[
    "--target",
    "--output",
    "--include-dirs",
    "--verbose",
    "--quiet",
    "--guard-enable",
    "--guard-disable",
    "--fsm",
    "--inline",
    "--parameters",
    "--bounds-check",
    "--address-map",
    "--define",
    "--float-width",
    "--float-as-q",
    "--float-embedded",
    "--bus",
    "--tick-hz",
];

/// Справка `compile` называет каждый флаг, который CLI разбирает.
#[test]
fn help_names_every_compile_flag() {
    let out = Command::new(env!("CARGO_BIN_EXE_taktc"))
        .args(["compile", "--help"])
        .output()
        .expect("запуск taktc compile --help");
    let text = format!(
        "{}{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
    let missing: Vec<&str> = COMPILE_FLAGS
        .iter()
        .copied()
        .filter(|flag| !text.contains(flag))
        .collect();
    assert!(
        missing.is_empty(),
        "справка `taktc compile --help` не называет флаги: {missing:?}"
    );
}

/// Ни один из названных флагов не считается неизвестным.
///
/// Проверяется именно "неизвестный флаг", а не успех: часть флагов требует значения, и
/// без него отказ законен - но он про значение, не про имя.
#[test]
fn every_named_flag_is_known_to_parser() {
    let dir = work_dir("known_flags");
    let input = dir.join("probe.takt");
    std::fs::write(&input, source(Probe::ManyStates)).expect("запись пробы");
    let mut unknown = Vec::new();
    for flag in COMPILE_FLAGS {
        let out = Command::new(env!("CARGO_BIN_EXE_taktc"))
            .arg("compile")
            .arg(flag)
            .arg(&input)
            .arg("-o")
            .arg(dir.join("out"))
            .output()
            .expect("запуск taktc compile");
        let text = String::from_utf8_lossy(&out.stderr).to_string();
        if text.contains("неизвестный") {
            unknown.push(*flag);
        }
    }
    assert!(
        unknown.is_empty(),
        "CLI не знает флаги, названные справкой: {unknown:?}"
    );
}
