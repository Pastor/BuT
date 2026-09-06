//! Сплошной перебор эталона: прогон и сценарии.
//!
//! # Что доказывает набор
//!
//! Эталон - единственный судья поведения в проекте: на нём стоят все потактовые сверки.
//! Его собственные режимы (сценарий именами портов, guard шага, значения `extern`,
//! guard границ, инварианты) проверялись входами отдельных фич; здесь вопрос задаётся
//! сплошь - режим x ожидаемый ответ.
//!
//! Ответы сняты **прогоном**, а не выведены из кода: тест, повторяющий реализацию,
//! доказывает лишь сам себя.
//!
//! Проверяется **ответ инструмента** - значения в трассе и код диагностики, - а не
//! внутреннее состояние `Unit`: последнее меняется вместе с реализацией и тестом быть
//! не может.

use std::path::{Path, PathBuf};
use std::process::Command;

/// Чего ждём от прогона.
enum Expect {
    /// Прогон успешен, и в выводе есть эти фрагменты.
    Trace(&'static [&'static str]),
    /// Прогон остановлен; в выводе есть код диагностики.
    Refused(&'static str),
}

/// Случай перебора.
struct Case {
    name: &'static str,
    model: &'static str,
    /// Сценарий (JSON) - либо `None`, если прогон без него.
    scenario: Option<&'static str>,
    /// Дополнительные ключи `takt-sim`.
    flags: &'static [&'static str],
    expect: Expect,
}

/// Модель с входным портом, выходом и инвариантом.
const COUNTER: &str = "\
model Wrap {
    var k: u8 := 0;
    in sw: u8;
    out led: u8;

    invariant Bounded = k < 100;

    start Go {
        always {
            k := k + sw;
            led := k;
        }
        ref Go: k < 50;
    }
}
start Main = Wrap;
";

/// Модель, чей инвариант нарушается на четвёртом такте.
const BREAKS_INVARIANT: &str = "\
model Wrap {
    var k: u8 := 0;
    out led: u8;

    invariant Small = k < 3;

    start Go {
        always {
            k := k + 1;
            led := k;
        }
        ref Go: k < 50;
    }
}
start Main = Wrap;
";

/// Модель с индексацией за границей массива.
const OUT_OF_BOUNDS: &str = "\
model Wrap {
    var data: [u8; 3] := {1, 2, 3};
    var i: u8 := 0;
    out led: u8;

    start Go {
        always {
            i := i + 1;
            led := data[i];
        }
        ref Go: i < 50;
    }
}
start Main = Wrap;
";

/// Модель, зовущая внешнюю функцию: значение даёт сценарий.
const EXTERNAL: &str = "\
model Wrap {
    var k: u8 := 0;
    extern fn sensor() -> u8;
    out led: u8;

    start Go {
        always {
            k := sensor();
            led := k;
        }
        ref Go: k < 50;
    }
}
start Main = Wrap;
";

fn cases() -> Vec<Case> {
    vec![
        Case {
            name: "plain_run",
            model: COUNTER,
            scenario: None,
            flags: &["-n", "3"],
            // Без сценария вход держит нулевое значение - счётчик стоит.
            expect: Expect::Trace(&["Шаг   3", "vars:k=0"]),
        },
        Case {
            name: "named_ports",
            model: COUNTER,
            scenario: Some(
                "[\n  { \"_step\": \"1\", \"in_ports\": { \"sw\": 2 } },\n  { \"_step\": \"2\", \"in_ports\": { \"sw\": 3 } }\n]\n",
            ),
            flags: &["-n", "2"],
            // Значения портов удерживаются между тактами: второй шаг задаёт 3, и сумма -
            // 5.
            expect: Expect::Trace(&["out:led=5", "vars:k=5"]),
        },
        Case {
            name: "guard_passes",
            model: COUNTER,
            scenario: Some(
                "[\n  { \"_step\": \"1\", \"in_ports\": { \"sw\": 2 }, \"guard\": { \"vars\": { \"k\": 2 } } }\n]\n",
            ),
            flags: &["-n", "1"],
            expect: Expect::Trace(&["vars:k=2"]),
        },
        Case {
            // Guard шага - проверка, а не пожелание: расхождение останавливает прогон и
            // называет обе величины.
            name: "guard_fails",
            model: COUNTER,
            scenario: Some(
                "[\n  { \"_step\": \"1\", \"in_ports\": { \"sw\": 2 }, \"guard\": { \"vars\": { \"k\": 99 } } }\n]\n",
            ),
            flags: &["-n", "1"],
            expect: Expect::Refused("Guard шага 1"),
        },
        Case {
            name: "unknown_port",
            model: COUNTER,
            scenario: Some("[ { \"_step\": \"1\", \"in_ports\": { \"missing\": 2 } } ]\n"),
            flags: &["-n", "1"],
            expect: Expect::Refused("SIM-030"),
        },
        Case {
            // Нарушение инварианта - `Failed`, а не предупреждение.
            name: "invariant_violated",
            model: BREAKS_INVARIANT,
            scenario: None,
            flags: &["-n", "6"],
            expect: Expect::Refused("SIM-025"),
        },
        Case {
            // Умолчание эталона совпадает с умолчанием целей: доступ за границей
            // останавливает прогон.
            name: "bounds_without_flag",
            model: OUT_OF_BOUNDS,
            scenario: None,
            flags: &["-n", "5"],
            expect: Expect::Refused("SIM-010"),
        },
        Case {
            // С флагом доступ не выполняется, а прогон продолжается - и это обязано
            // совпадать с прошивкой, собранной с `--bounds-check`.
            name: "bounds_with_flag",
            model: OUT_OF_BOUNDS,
            scenario: None,
            flags: &["-n", "5", "--bounds-check"],
            expect: Expect::Trace(&["Шаг   5", "vars:data=[1,2,3]"]),
        },
        Case {
            // Значение внешней функции задаёт сценарий: умолчания у неё нет.
            name: "extern_value",
            model: EXTERNAL,
            scenario: Some("[ { \"_step\": \"1\", \"extern\": { \"sensor\": 7 } } ]\n"),
            flags: &["-n", "1"],
            expect: Expect::Trace(&["out:led=7", "vars:k=7"]),
        },
    ]
}

/// Уникальный по тесту каталог.
fn work_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0461_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог теста");
    dir
}

/// Гоняет эталон; отдаёт stdout+stderr.
fn run(dir: &Path, case: &Case) -> String {
    let model = dir.join("probe.takt");
    std::fs::write(&model, case.model).expect("запись модели");
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_takt-sim"));
    cmd.args(case.flags);
    if let Some(scenario) = case.scenario {
        let path = dir.join("scenario.json");
        std::fs::write(&path, scenario).expect("запись сценария");
        cmd.arg("-s").arg(&path);
    }
    let out = cmd.arg(&model).output().expect("запуск takt-sim");
    format!(
        "{}{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    )
}

/// Каждый режим эталона даёт названный ответ.
#[test]
fn every_simulator_mode_answers_as_expected() {
    let all = cases();
    let mut failures: Vec<String> = Vec::new();
    for case in &all {
        let dir = work_dir(case.name);
        let text = run(&dir, case);
        match case.expect {
            Expect::Trace(fragments) => {
                for fragment in fragments {
                    if !text.contains(fragment) {
                        failures.push(format!("{}: в трассе нет '{fragment}':\n{text}", case.name));
                    }
                }
                if text.contains("Симуляция остановлена") {
                    failures.push(format!("{}: прогон оборван:\n{text}", case.name));
                }
            }
            Expect::Refused(code) => {
                if !text.contains(code) {
                    failures.push(format!("{}: в выводе нет '{code}':\n{text}", case.name));
                }
            }
        }
    }
    assert!(
        failures.is_empty(),
        "эталон разошёлся с ожиданием в {} случаях из {}:\n{}",
        failures.len(),
        all.len(),
        failures.join("\n")
    );
}
