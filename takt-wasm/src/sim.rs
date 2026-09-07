//! Прогон модели в браузере: тот же эталон, тот же такт.

use serde::Serialize;
use std::cell::RefCell;
use std::collections::HashMap;
use takt_lang::diagnostics::FileTable;
use takt_lang::semantic::tree::construct_model_with_files;
use takt_sim::json_input::SimStep;
use takt_sim::port_names::PortNames;
use takt_sim::runner::SimulationRunner;
use takt_sim::{build_unit, graphics_config::GraphicsConfig};

use crate::compile::DEFAULT_FILENAME;
use crate::reply::{self, DiagnosticJson};

thread_local! {
    /// Открытые прогоны: `id -> бегун`.
    ///
    /// Модуль исполняется в одном потоке Worker'а, поэтому `thread_local` достаточно и
    /// мьютекс не нужен. Прогон живёт между вызовами - иначе каждый такт начинался бы
    /// заново.
    static SESSIONS: RefCell<HashMap<u32, SimulationRunner>> = RefCell::new(HashMap::new());
    /// Номер следующего прогона.
    static NEXT_ID: RefCell<u32> = const { RefCell::new(1) };
}

/// Открывает прогон: строит модель и эталон, но ни одного такта не делает.
///
/// `scenario` - JSON-сценарий той же формы, что файл `-s` у `takt-sim` (пустая строка -
/// сценария нет). `tick_ms` - период модельных часов; `0` означает "взять из объявления
/// `clock` модели, иначе 1 мс", как в CLI.
/// Предупреждение прогона в форме страницы.
///
/// Код отдельным полем - как у предупреждений компиляции: страница показывает его
/// перед текстом, а не ищет внутри строки. `null` означает, что кода у сообщения нет.
#[derive(Serialize)]
struct RunWarningJson {
    code: Option<String>,
    message: String,
    step: Option<usize>,
}

impl From<&takt_sim::runner::RunWarning> for RunWarningJson {
    fn from(warning: &takt_sim::runner::RunWarning) -> Self {
        Self {
            code: (!warning.code.is_empty()).then(|| warning.code.to_string()),
            message: warning.message.clone(),
            step: warning.step,
        }
    }
}

pub fn open(source: &str, scenario: &str, tick_ms: i64) -> String {
    #[derive(Serialize)]
    struct Reply {
        id: u32,
        warnings: Vec<RunWarningJson>,
    }

    let mut files = FileTable::new(DEFAULT_FILENAME);
    let (ast, _comments) = match takt_lang::parse(source, 0) {
        Ok(parsed) => parsed,
        Err(diagnostics) => {
            let first = diagnostics
                .first()
                .cloned()
                .unwrap_or_else(unreachable_empty);
            return reply::failed(&first, source);
        }
    };
    let model = match construct_model_with_files(&ast, None, &[], &mut files, false) {
        Ok(model) => model,
        Err(diagnostic) => return reply::failed(&diagnostic, source),
    };
    // Библиотечный файл исполнять нечем - `SE-102`. Правило то же, что у CLI симулятора
    // и у целей компиляции: два ответа на один вход расходиться не должны.
    if let Some(diagnostic) = takt_lang::pipeline::validate_entry_model(&model) {
        return reply::failed(&diagnostic, source);
    }

    let port_names = PortNames::from_model(&model.borrow());
    let model_name = model.borrow().name.clone();
    let clock_hz = model.borrow().clock_hz;
    let unit = match build_unit(model) {
        Ok(unit) => unit,
        Err(diagnostic) => return reply::failed(&diagnostic, source),
    };

    let steps: Vec<SimStep> = if scenario.trim().is_empty() {
        Vec::new()
    } else {
        match serde_json::from_str(scenario) {
            Ok(steps) => steps,
            Err(e) => return reply::refused(format!("сценарий не читается: {e}")),
        }
    };

    // Каталога вывода нет - кадры в браузере не пишутся (крейт эталона собран без фичи
    // `graphics`).
    let mut runner = match SimulationRunner::new(
        unit,
        steps,
        None,
        None,
        DEFAULT_FILENAME,
        GraphicsConfig::default().output_mode.clone(),
        port_names,
        model_name,
        GraphicsConfig::default(),
    ) {
        Ok(runner) => runner,
        Err(message) => return reply::refused(message),
    };
    // Период такта: явный аргумент > частота модели > умолчание 1 мс - тот же
    // приоритет, что у CLI.
    if tick_ms > 0 {
        runner.set_tick_period_ns(tick_ms.saturating_mul(1_000_000));
    } else if let Some(hz) = clock_hz.filter(|hz| *hz > 0) {
        runner.set_tick_period_ns(1_000_000_000 / i64::try_from(hz).unwrap_or(i64::MAX));
    }

    let warnings: Vec<RunWarningJson> = runner
        .ambiguous_name_warnings()
        .iter()
        .map(RunWarningJson::from)
        .collect();
    let id = NEXT_ID.with(|next| {
        let mut next = next.borrow_mut();
        let id = *next;
        *next = next.wrapping_add(1).max(1);
        id
    });
    SESSIONS.with(|sessions| sessions.borrow_mut().insert(id, runner));
    reply::ok(Reply { id, warnings })
}

/// Делает не более `budget` тактов открытого прогона.
///
/// Возвращает строки трассы - те же, что печатает `takt-sim`, - и исход, если прогон
/// окончен. Не окончен - страница зовёт снова: так вкладка остаётся отзывчивой на
/// модели, которая не завершается никогда.
pub fn tick(id: u32, budget: u32) -> String {
    #[derive(Serialize)]
    struct Reply {
        lines: Vec<String>,
        done: bool,
        info: Vec<String>,
        errors: Vec<String>,
        /// Предупреждения тактов этой порции - сообщения инструмента о прогоне.
        warnings: Vec<RunWarningJson>,
        /// Вывод программы (`debug`) - строки самой модели, а не замечания к ней.
        output: Vec<String>,
    }

    SESSIONS.with(|sessions| {
        let mut sessions = sessions.borrow_mut();
        let Some(runner) = sessions.get_mut(&id) else {
            return reply::refused(format!("прогон {id} не открыт"));
        };
        let mut lines = Vec::new();
        let mut warnings = Vec::new();
        let mut output = Vec::new();
        for _ in 0..budget {
            match runner.step() {
                Ok(step) => {
                    warnings.extend(step.warnings.iter().map(RunWarningJson::from));
                    output.extend(step.output);
                    if let Some(line) = step.line {
                        lines.push(line);
                    }
                    if let Some(result) = step.result {
                        let report = takt_sim::trace::result_report(&result);
                        return reply::ok(Reply {
                            lines,
                            done: true,
                            info: report.info,
                            errors: report.errors,
                            warnings,
                            output,
                        });
                    }
                }
                // Ошибка прогона (guard, чтение сценария) - отказ с текстом эталона:
                // придумывать свой значило бы показать в браузере не то, что показывает
                // инструмент.
                Err(message) => return reply::refused(message),
            }
        }
        reply::ok(Reply {
            lines,
            done: false,
            info: Vec::new(),
            errors: Vec::new(),
            warnings,
            output,
        })
    })
}

/// Закрывает прогон и освобождает его память.
pub fn close(id: u32) -> String {
    #[derive(Serialize)]
    struct Reply {
        closed: bool,
    }
    let closed = SESSIONS.with(|sessions| sessions.borrow_mut().remove(&id).is_some());
    reply::ok(Reply { closed })
}

/// Диагностика на случай пустого списка ошибок разбора.
///
/// Список пустым не бывает: `parse` отдаёт `Err` только с ошибками. Но `unwrap` здесь
/// стал бы паникой, а паника в модуле - `abort`: страница теряет модуль целиком.
fn unreachable_empty() -> takt_lang::diagnostics::Diagnostic {
    takt_lang::diagnostics::Diagnostic::error(
        takt_lang::diagnostics::Location::Codegen,
        "разбор отказал без диагностики".to_string(),
    )
}

/// Диагностики документа в форме страницы - общая с редакторским слоем.
pub type Diagnostics = Vec<DiagnosticJson>;

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::Value;

    fn json(text: &str) -> Value {
        serde_json::from_str(text).expect("ответ моста — JSON")
    }

    const COUNTER: &str = "var n: u8 := 0;\n\nstart Run {\n    always {\n        n := n + 1;\n    }\n\n    ref Run: 1 = 1;\n}\n";

    /// Прогон идёт по тактам и отдаёт ту же трассу, что печатает `takt-sim`.
    #[test]
    fn ticks_yield_trace_lines() {
        let opened = json(&open(COUNTER, "", 0));
        assert_eq!(opened["ok"], Value::Bool(true), "{opened}");
        let id = opened["id"].as_u64().unwrap() as u32;

        let reply = json(&tick(id, 3));
        let lines: Vec<&str> = reply["lines"]
            .as_array()
            .unwrap()
            .iter()
            .map(|l| l.as_str().unwrap())
            .collect();
        assert_eq!(lines.len(), 3, "запрошено три такта: {reply}");
        assert!(
            lines[0].starts_with("Шаг   1:"),
            "форма строки: {}",
            lines[0]
        );
        assert!(lines[2].contains("n=3"), "счётчик тикает: {}", lines[2]);
        assert_eq!(reply["done"], Value::Bool(false), "модель не завершается");

        assert_eq!(json(&close(id))["closed"], Value::Bool(true));
    }

    /// Бюджет не даёт вкладке замереть: незавершающаяся модель отдаёт управление после
    /// запрошенного числа тактов.
    #[test]
    fn budget_stops_endless_model() {
        let opened = json(&open(COUNTER, "", 0));
        let id = opened["id"].as_u64().unwrap() as u32;
        for _ in 0..3 {
            let reply = json(&tick(id, 5));
            assert_eq!(reply["ok"], Value::Bool(true), "{reply}");
            assert_eq!(reply["done"], Value::Bool(false));
            assert_eq!(reply["lines"].as_array().unwrap().len(), 5);
        }
        json(&close(id));
    }

    /// Завершающаяся модель отдаёт исход и сводку - ту же, что печатает CLI.
    #[test]
    fn terminating_model_reports_outcome() {
        let opened = json(&open("start S;\n", "", 0));
        let id = opened["id"].as_u64().unwrap() as u32;
        let reply = json(&tick(id, 10));
        assert_eq!(reply["done"], Value::Bool(true), "{reply}");
        let info = reply["info"].as_array().unwrap();
        assert!(
            info.iter()
                .any(|l| l.as_str().unwrap().contains("Завершено")),
            "ожидался исход прогона: {reply}"
        );
        json(&close(id));
    }

    /// Сценарий задаёт входы, и они видны в трассе.
    #[test]
    fn scenario_drives_inputs() {
        let model = "in sensor: u8;\nvar seen: u8 := 0;\n\nstart Run {\n    always {\n        seen := sensor;\n    }\n\n    ref Run: 1 = 1;\n}\n";
        let scenario = r#"[{"in_ports": {"sensor": 7}}]"#;
        let opened = json(&open(model, scenario, 0));
        assert_eq!(opened["ok"], Value::Bool(true), "{opened}");
        let id = opened["id"].as_u64().unwrap() as u32;
        let reply = json(&tick(id, 1));
        assert!(
            reply["lines"][0].as_str().unwrap().contains("sensor=7"),
            "вход сценария обязан доехать: {reply}"
        );
        json(&close(id));
    }

    /// Ошибка модели - диагностика с кодом, а не строка без роду.
    #[test]
    fn broken_model_is_refused_with_diagnostic() {
        let reply = json(&open("start S {\n    ref Missing: 1 = 1;\n}\n", "", 0));
        assert_eq!(reply["ok"], Value::Bool(false), "{reply}");
        assert!(reply["error"]["code"].as_str().is_some(), "{reply}");
    }

    /// Неоткрытый прогон - отказ, а не пустая трасса.
    #[test]
    fn unknown_session_is_refused() {
        let reply = json(&tick(4242, 1));
        assert_eq!(reply["ok"], Value::Bool(false), "{reply}");
    }
}
