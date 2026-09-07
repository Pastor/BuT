//! Сплошной тест признака "нужен ли указатель на корень".
//!
//! # Что именно проверяется
//!
//! 1. **Вывод собирается** `cc` флагами проверки цели - ловит ложное "указатель не
//!    нужен" (громкий класс: отказ чужого инструмента).
//! 2. **Признак точен**: там, где обращений нет, параметра в сигнатуре **тоже**
//!    нет - ловит ложное "нужен" (тихий класс: заглушка `(void)main;` делает
//!    вывод валидным, и `cc` молчит).
//!
//! Ожидания таблицы сняты **прогоном**, а не выведены из кода признака: тест,
//! повторяющий реализацию, доказывает лишь сам себя.

use std::path::{Path, PathBuf};
use std::process::Command;

use super::matrix_probes::{Kind, Touch, case_name, cases, extra_flags, library_files, source};
use super::target_matrix_tests::refusal;

/// Ожидание: печатается ли `main` функциям `(_init, _tick)`.
///
/// Снято прогоном 2026-08-31, а не выведено из кода признака: тест, повторяющий
/// реализацию, доказывает лишь сам себя.
fn expects(touch: Touch, _kind: Kind) -> (bool, bool) {
    match touch {
        // Обязательства читают переменную самой модели: корня им не нужно.
        Touch::None
        | Touch::ExternCall
        | Touch::InvariantModel
        | Touch::InvariantState
        | Touch::GuardFormula
        | Touch::LtlFormula
        // Формула в теле читает переменную самой модели либо параметр функции: корня ей
        // не нужно.
        | Touch::GuardInBlock
        | Touch::GuardInFunction
        | Touch::GuardInNested
        | Touch::LtlInBlock
        | Touch::LtlInModelBlock
        | Touch::LtlInFunction
        | Touch::LtlInState
        | Touch::LtlInNested
        | Touch::InvariantScoped
        | Touch::InvariantNamed
        // Вызов из условия зовёт функцию, состояния не касающуюся: корня ей не нужно ни
        // в сигнатуре, ни в вызове.
        | Touch::FnCallOnEdge
        // Именованное условие раскрывается в условие над переменной модели: корня ему
        // не нужно.
        | Touch::CondOnEdge
        | Touch::CondInBody
        | Touch::CondInGuard
        | Touch::CondNested
        // Цикл считает переменную самой модели: корня ему не нужно.
        | Touch::LoopForStatic
        | Touch::LoopForNoInit
        | Touch::LoopWhile
        | Touch::LoopBreak
        | Touch::LoopNested
        | Touch::LoopContinue
        // `match` разбирает переменную самой модели: корня ему не нужно.
        | Touch::MatchWildcard
        | Touch::MatchNoWildcard
        | Touch::MatchMultiPattern
        | Touch::MatchEnum
        | Touch::MatchNested
        // Импортированное объявление - объявление импортёра: функция, тип и константа
        // приходят к нему, и корня им не нужно.
        | Touch::ImportFunction
        | Touch::ImportSelective
        | Touch::ImportType
        | Touch::ImportTransitive
        // Реализация подключённой моделью: обёртка своих обращений не имеет.
        | Touch::ImportModel
        | Touch::ImportNestedModel => (false, false),
        // Имя модели совпало с именем файла: модель библиотеки пишет В порт (без этого
        // проба не показывает второй слой дефекта), и указатель нужен ей и обёртке,
        // которая её тикает, - как у видов с параметром.
        Touch::ImportNameClash => (false, true),
        // Донор с параметром пишет свой порт - указатель нужен ему и обёртке, которая
        // его тикает.
        Touch::ParameterDefault | Touch::ParameterArgument | Touch::ParameterExpression => {
            (false, true)
        }
        // Порт читается и пишется в такте - через HAL корня; у входного и
        // двунаправленного это так же, как у выходного.
        Touch::PortWrite
        | Touch::SharedRead
        | Touch::Transitive
        | Touch::PortRead
        | Touch::InoutRead
        | Touch::InoutWrite
        | Touch::PortReadPartial
        // Адресованный порт пишется в такте - через HAL корня, как обычный.
        | Touch::AddressOperator
        | Touch::AddressBit
        | Touch::AddressExpression
        | Touch::AddressMap
        | Touch::AddressDefine => (false, true),
        Touch::PortInit | Touch::VarInit => (true, false),
        // Время в профиле "часы" - обращение к корню и в `_init`, и в такте (метка
        // сравнивается с `main->now_ms(...)`).
        Touch::ClockAfter
        | Touch::TimeEvery
        | Touch::TimeDurationVar
        | Touch::TimeComputed => (true, true),
        // Тактовая выдержка `after Nt` и объявленный `clock` времени хоста не требуют:
        // счётчик тактов живёт в самой модели.
        Touch::TimeAfterTicks | Touch::TimeClockDeclared => (false, false),
    }
}

/// Уникальный по тесту каталог.
fn work_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("main")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0449_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог теста");
    dir
}

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Компилирует случай целью `c`; отдаёт текст порождённого файла.
fn compile(dir: &Path, source: &str, touch: Touch) -> String {
    let input = dir.join("probe.takt");
    std::fs::write(&input, source).expect("запись пробы");
    for file in library_files(touch) {
        std::fs::write(dir.join(file.name), file.text).expect("запись библиотеки");
    }
    let out = Command::new(env!("CARGO_BIN_EXE_taktc"))
        .arg("compile")
        .args(["-t", "c"])
        // Ключи вида обращения: карта адресов, `-D`, `--tick-hz` (0458, 0459).
        .args(
            extra_flags(touch)
                .into_iter()
                .map(|flag| flag.replace("{dir}", &dir.display().to_string())),
        )
        .arg(&input)
        .arg("-o")
        .arg(dir.join("out"))
        .output()
        .expect("запуск taktc compile");
    assert!(
        out.status.success(),
        "цель `c` обязана перевести вход:\n{}\n--- исходник ---\n{source}",
        String::from_utf8_lossy(&out.stderr)
    );
    std::fs::read_to_string(dir.join("out").join("probe.c")).expect("порождённый файл читается")
}

/// Собирает порождённый файл флагами проверки цели.
fn build(dir: &Path) -> Result<(), String> {
    let cc = Command::new("cc")
        .args([
            "-std=c11",
            "-Wall",
            "-Wextra",
            "-Wno-unused-parameter",
            "-Werror",
            "-c",
        ])
        .arg(dir.join("out").join("probe.c"))
        .arg("-I")
        .arg(dir.join("out"))
        .arg("-o")
        .arg(dir.join("probe.o"))
        .output()
        .expect("запуск cc");
    if cc.status.success() {
        Ok(())
    } else {
        Err(String::from_utf8_lossy(&cc.stderr).into_owned())
    }
}

/// Есть ли у функции параметр-указатель на корень.
///
/// Ищется **прототип**: он печатается первым и в единственном экземпляре.
fn has_root_parameter(text: &str, function: &str) -> bool {
    let needle = format!("static void {function}(");
    let line = text
        .lines()
        .find(|l| l.contains(&needle))
        .unwrap_or_else(|| panic!("в выводе нет прототипа '{function}':\n{text}"));
    line.contains("Probe *main")
}

/// Сплошной перебор: форма реализации x вид обращения x форма объявления.
#[test]
fn root_pointer_is_exact_for_every_shape_and_touch() {
    if !cc_available() {
        eprintln!("cc недоступен — сплошной сторож пропущен");
        return;
    }
    let all = cases();
    let mut failures: Vec<String> = Vec::new();
    for (shape, touch, kind) in &all {
        let tag = case_name(*shape, *touch, *kind);
        // Законную границу цели судит таблица набора 0450 - здесь она означает
        // "сигнатуры проверять не на чем", а не "тест молчит".
        if let Some(code) = refusal("c", *touch, *kind) {
            let dir = work_dir(&tag);
            let input = dir.join("probe.takt");
            std::fs::write(&input, source(*shape, *touch, *kind)).expect("запись пробы");
            for file in library_files(*touch) {
                std::fs::write(dir.join(file.name), file.text).expect("запись библиотеки");
            }
            let out = Command::new(env!("CARGO_BIN_EXE_taktc"))
                .arg("compile")
                .args(["-t", "c"])
                .arg(&input)
                .arg("-o")
                .arg(dir.join("out"))
                .output()
                .expect("запуск taktc compile");
            let stderr = String::from_utf8_lossy(&out.stderr);
            if out.status.success() || !stderr.contains(code) {
                failures.push(format!(
                    "{tag}: ожидался отказ {code}, получено: {}",
                    if out.status.success() {
                        "перевод".to_string()
                    } else {
                        stderr.trim().to_string()
                    }
                ));
            }
            continue;
        }
        let dir = work_dir(&tag);
        let text = compile(&dir, &source(*shape, *touch, *kind), *touch);
        if let Err(err) = build(&dir) {
            failures.push(format!("{tag}: cc отверг вывод:\n{err}"));
            continue;
        }
        let (init_expected, tick_expected) = expects(*touch, *kind);
        let init_actual = has_root_parameter(&text, "ProbeWrap_init");
        let tick_actual = has_root_parameter(&text, "ProbeWrap_tick");
        if init_actual != init_expected {
            failures.push(format!(
                "{tag}: `_init` {} указатель, ожидалось «{}»",
                if init_actual {
                    "получил"
                } else {
                    "не получил"
                },
                if init_expected {
                    "получит"
                } else {
                    "не получит"
                }
            ));
        }
        if tick_actual != tick_expected {
            failures.push(format!(
                "{tag}: `_tick` {} указатель, ожидалось «{}»",
                if tick_actual {
                    "получил"
                } else {
                    "не получил"
                },
                if tick_expected {
                    "получит"
                } else {
                    "не получит"
                }
            ));
        }
    }
    assert!(
        failures.is_empty(),
        "признак «нужен ли указатель на корень» разошёлся с ожиданием в {} случаях из {}:\n{}",
        failures.len(),
        all.len(),
        failures.join("\n")
    );
}
