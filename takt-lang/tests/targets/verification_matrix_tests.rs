//! Сплошной перебор верификации: LTL и автоматы Бюхи.
//!
//! # Что доказывает набор
//!
//! Верификация (`taktc verify`) отвечает одним из трёх вердиктов:
//! **держится**, **нарушено** (с контрпримером) и **не проверено** (с названной
//! причиной).
//!
//! Проверяется **вердикт**, а не структура автомата: устройство Бюхи - деталь
//! реализации, и тест на неё уже однажды закрепил дефект. Ровно поэтому здесь смотрят
//! на ответ инструмента.
//!
//! Направление ошибки у абстракции **несимметрично**: "держится" надёжно, "нарушено"
//! может быть ложным. Поэтому случаи, где ожидается "держится", ценнее прочих: они
//! ловят потерю рёбер, из-за которой Крипке стала бы недо-аппроксимацией.

use std::path::{Path, PathBuf};
use std::process::Command;

/// Вердикт верификации - то, что печатает инструмент.
#[derive(Clone, Copy, PartialEq)]
enum Verdict {
    /// Свойство держится на всех путях абстракции.
    Holds,
    /// Свойство нарушено; печатается контрпример.
    Violated,
    /// Свойство не проверено; печатается **причина**.
    Unsupported,
}

impl Verdict {
    /// Маркер вердикта в выводе.
    fn marker(self) -> &'static str {
        match self {
            Verdict::Holds => "СВОЙСТВО ДЕРЖИТСЯ",
            Verdict::Violated => "СВОЙСТВО НАРУШЕНО",
            Verdict::Unsupported => "СВОЙСТВО НЕ ПРОВЕРЕНО",
        }
    }
}

/// Случай перебора: имя, исходник и ожидаемый вердикт.
struct Case {
    name: &'static str,
    source: String,
    verdict: Verdict,
    /// Фрагмент, который обязан быть в выводе помимо вердикта: контрпример, причина
    /// отказа. Пусто - не проверяется.
    detail: &'static str,
}

/// Модель со свойством `property` над счётчиком и состояниями `Go`/`Done`.
fn counting_model(property: &str) -> String {
    format!(
        "model Wrap {{\n    var k: u8 := 0;\n    out led: u8 at 0x40000100;\n\n    cond Low = k < 5;\n\n    : [LTL] {property};\n\n    start Go {{\n        always {{\n            k := k + 1;\n            led := k;\n        }}\n        ref Done: k > 3;\n    }}\n    state Done {{\n        always {{\n            led := 200;\n        }}\n        ref Done: k > 0;\n    }}\n}}\nstart Main = Wrap;\n"
    )
}

/// Та же модель, но переход из `Go` **безусловный**: самопетли у состояния тогда нет, и
/// достижимость `Done` абстракция видит.
fn unconditional_model(property: &str) -> String {
    format!(
        "model Wrap {{\n    var k: u8 := 0;\n    out led: u8 at 0x40000100;\n\n    cond Low = k < 5;\n\n    : [LTL] {property};\n\n    start Go {{\n        always {{\n            k := k + 1;\n            led := k;\n        }}\n        ref Done;\n    }}\n    state Done {{\n        always {{\n            led := 200;\n        }}\n        ref Done: k > 0;\n    }}\n}}\nstart Main = Wrap;\n"
    )
}

/// Те же свойства, но модель - параллельная композиция: свойство объявлено в ветви, и
/// область формулы - её модель.
fn composed_model(property: &str) -> String {
    format!(
        "model First {{\n    var a: u8 := 0;\n    cond Alow = a < 5;\n\n    : [LTL] {property};\n\n    start Go {{\n        always {{\n            a := a + 1;\n        }}\n        next Done;\n    }}\n    state Done;\n}}\n\nmodel Second {{\n    var b: u8 := 0;\n    start Work {{\n        always {{\n            b := b + 1;\n        }}\n        next Done;\n    }}\n    state Done;\n}}\n\nmodel Wrap {{\n    start Only = First | Second;\n}}\nstart Main = Wrap;\n"
    )
}

/// Все случаи перебора.
fn cases() -> Vec<Case> {
    vec![
        // -- Свойства управления: атом - имя состояния ----------------------
        Case {
            // Самопетля есть у всякого состояния без безусловного `ref`: путь "остаться
            // в Go навсегда" существует, и достижимости `Done` абстракция не видит.
            // Ошибка здесь - в безопасную сторону: лишний прогон, а не пропущенный
            // дефект.
            name: "eventually_state_conditional",
            source: counting_model("F Done"),
            verdict: Verdict::Violated,
            detail: "контрпример",
        },
        Case {
            // Тот же вход с безусловным переходом: самопетли нет, и свойство держится.
            // Пара случаев показывает, чем именно абстракция платит.
            name: "eventually_state_unconditional",
            source: unconditional_model("F Done"),
            verdict: Verdict::Holds,
            detail: "",
        },
        Case {
            // Самопетля есть у всякого состояния без безусловного `ref`: путь "остаться
            // в Go навсегда" существует, и `G Done` на нём ложно.
            name: "always_state",
            source: counting_model("G Done"),
            verdict: Verdict::Violated,
            detail: "контрпример",
        },
        // -- Свойства над данными: атом - именованное условие ---------------
        Case {
            name: "eventually_data",
            source: counting_model("F Low"),
            verdict: Verdict::Holds,
            detail: "",
        },
        Case {
            name: "always_data",
            source: counting_model("G Low"),
            verdict: Verdict::Violated,
            detail: "контрпример",
        },
        // -- Темпоральные связки --------------------------------------------
        Case {
            // `U` требует достижимости правой части - та же самопетля.
            name: "until_conditional",
            source: counting_model("Low U Done"),
            verdict: Verdict::Violated,
            detail: "контрпример",
        },
        Case {
            name: "until_unconditional",
            source: unconditional_model("Low U Done"),
            verdict: Verdict::Holds,
            detail: "",
        },
        Case {
            name: "implication",
            source: counting_model("G (Done -> F Done)"),
            verdict: Verdict::Holds,
            detail: "",
        },
        // -- Границы: атом вне охвата и потолок задачи ----------------------
        Case {
            name: "unknown_atom",
            source: counting_model("F Missing"),
            verdict: Verdict::Unsupported,
            detail: "не является ни именем состояния",
        },
        Case {
            // Домен `u32` даёт рёбер больше потолка: проверка отвергается до счёта, а
            // не виснет.
            name: "domain_over_budget",
            source: "model Wrap {\n    var k: u8 := 0;\n    var big: u32 := 0;\n    out led: u8 at 0x40000100;\n\n    cond Big = big > 100;\n\n    : [LTL] G Big;\n\n    start Go {\n        always {\n            k := k + 1;\n            big := big + 1;\n            led := k;\n        }\n        ref Go: k > 0;\n    }\n}\nstart Main = Wrap;\n"
                .to_string(),
            verdict: Verdict::Unsupported,
            detail: "потолком",
        },
        // -- Композиция: область формулы - модель, где она объявлена --------
        Case {
            name: "composed_eventually",
            source: composed_model("F Alow"),
            verdict: Verdict::Holds,
            detail: "модель First",
        },
        Case {
            name: "composed_always",
            source: composed_model("G Alow"),
            verdict: Verdict::Violated,
            detail: "модель First",
        },
    ]
}

fn taktc() -> Command {
    Command::new(env!("CARGO_BIN_EXE_taktc"))
}

/// Уникальный по тесту каталог.
fn work_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("main")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0460_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог теста");
    dir
}

/// Запускает `taktc verify`; отдаёт stdout+stderr.
fn verify(dir: &Path, source: &str, extra: &[&str]) -> String {
    let input = dir.join("probe.takt");
    std::fs::write(&input, source).expect("запись пробы");
    let out = taktc()
        .arg("verify")
        .args(extra)
        .arg(&input)
        .output()
        .expect("запуск taktc verify");
    format!(
        "{}{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    )
}

/// Сплошной перебор: каждый случай даёт **названный** вердикт.
#[test]
fn every_property_gets_its_verdict() {
    let all = cases();
    let mut failures: Vec<String> = Vec::new();
    for case in &all {
        let dir = work_dir(case.name);
        let text = verify(&dir, &case.source, &[]);
        if !text.contains(case.verdict.marker()) {
            failures.push(format!(
                "{}: ожидался вердикт «{}», получено:\n{text}",
                case.name,
                case.verdict.marker()
            ));
            continue;
        }
        if !case.detail.is_empty() && !text.contains(case.detail) {
            failures.push(format!(
                "{}: в выводе нет '{}':\n{text}",
                case.name, case.detail
            ));
        }
    }
    assert!(
        failures.is_empty(),
        "верификация разошлась с ожиданием в {} случаях из {}:\n{}",
        failures.len(),
        all.len(),
        failures.join("\n")
    );
}

/// Свойство ключом `--property` проверяется у корневой модели.
///
/// Состояния вложенных моделей в охват не входят, и вердикт здесь - "не проверено" с
/// названной причиной, а не молчание.
#[test]
fn property_flag_scopes_to_root_model() {
    let dir = work_dir("property_flag");
    let text = verify(&dir, &counting_model("F Done"), &["-p", "F Done"]);
    assert!(
        text.contains("СВОЙСТВО НЕ ПРОВЕРЕНО") && text.contains("вложенной модели"),
        "ключ `--property` дал неожиданный вердикт:\n{text}"
    );
}

/// Диаграмма абстракции печатается ключом `--trace` и непуста.
#[test]
fn trace_prints_the_kripke_structure() {
    let dir = work_dir("trace");
    let text = verify(&dir, &counting_model("F Done"), &["--trace"]);
    assert!(
        text.contains("Структура Крипке"),
        "ключ `--trace` не напечатал абстракцию:\n{text}"
    );
}
