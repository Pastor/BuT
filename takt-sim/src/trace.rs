//! Трасса прогона: строка шага и итоговая сводка - **строками**, а не печатью.

use crate::context::Context;
use crate::eval::value::Value;
use crate::port_names::PortNames;
use crate::runner::{RunResult, RunWarning};
use crate::unit::Unit;
use takt_lang::diagnostics::lang::keys;
use takt_lang::msg;

/// Строит строку шага трассы - ту же, что печатает `takt-sim`.
///
/// `now_ns` - модельное время прогона: показывается, только когда часы сдвинулись,
/// иначе оно засоряло бы вывод моделям, время не использующим.
pub fn step_line(unit: &Unit, port_names: &PortNames, step_no: usize, now_ns: i64) -> String {
    let states = unit.active_states();
    let states_str = if states.is_empty() {
        "—".to_string()
    } else {
        states.join(", ")
    };

    // Двусмысленное имя печатается квалифицированными формами: показывать `val=1`, пока
    // вторая под-модель держит `val=2`, - значит скрывать половину состояния модели.
    let display_names = |names: &[String]| -> Vec<String> {
        let mut out = Vec::new();
        for n in names {
            match port_names.ambiguous.iter().find(|(bare, _)| bare == n) {
                Some((_, qualified)) => out.extend(qualified.iter().cloned()),
                None => out.push(n.clone()),
            }
        }
        out
    };

    let fmt_group = |names: &[String]| -> String {
        display_names(names)
            .iter()
            .filter_map(|n| {
                unit.get_value(n)
                    .map(|v| format!("{}={}", n, format_value(&v)))
            })
            .collect::<Vec<_>>()
            .join("  ")
    };

    // Трасса печатает и такт, и модельное время: без времени не прочесть, почему
    // выдержка сработала именно здесь, а без такта - не сверить с целью.
    let mut line = if now_ns > 0 {
        format!(
            "Шаг {:3} ({:>8}):  [{}]",
            step_no,
            format_duration(now_ns),
            states_str
        )
    } else {
        format!("Шаг {:3}:  [{}]", step_no, states_str)
    };

    for (label, names) in [
        ("in", port_names.in_ports.as_slice()),
        ("out", port_names.out_ports.as_slice()),
        ("inout", port_names.inout_ports.as_slice()),
        ("vars", port_names.vars.as_slice()),
    ] {
        let s = fmt_group(names);
        if !s.is_empty() {
            line.push_str(&format!("  {}:{}", label, s));
        }
    }
    line
}

/// Печатная форма предупреждения прогона - единственная в проекте.
///
/// Библиотека предупреждения возвращает, печатает вызывающий, но **форма** строки одна
/// на всех: второй носитель формата разошёлся бы с первым молча, и один и тот же код
/// выглядел бы в консоли и на странице по-разному.
///
/// Пустой код означает, что кода у предупреждения нет: таково предупреждение о
/// двусмысленных именах, печатавшееся словом внимания с самого появления.
pub fn warning_line(warning: &RunWarning) -> String {
    if warning.code.is_empty() {
        return msg!(keys::SIM_TRACE_ATTENTION, message = warning.message);
    }
    match warning.step {
        Some(step) => msg!(
            keys::SIM_TRACE_WARNING_AT_STEP,
            code = warning.code,
            step = step,
            message = warning.message
        ),
        None => msg!(
            keys::SIM_TRACE_WARNING,
            code = warning.code,
            message = warning.message
        ),
    }
}

/// Итог прогона: что сказать в обычный поток, а что - в поток ошибок.
///
/// Разделение потоков - свойство **CLI**, но принадлежность строки к тому или другому
/// решает содержание, и потому живёт
/// вместе с текстом. Потребитель без консоли складывает обе половины, не гадая.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ResultReport {
    /// Сообщения об исходе прогона (обычный поток).
    pub info: Vec<String>,
    /// Сообщения об отказах и нарушениях (поток ошибок).
    pub errors: Vec<String>,
}

/// Строит итоговую сводку прогона - ту же, что печатает `takt-sim`.
pub fn result_report(result: &RunResult) -> ResultReport {
    let mut report = ResultReport::default();
    match result {
        RunResult::Terminated { steps } => {
            report
                .info
                .push(msg!(keys::SIM_TRACE_TERMINATED, steps = steps));
        }
        RunResult::StepsReached { steps } => {
            report
                .info
                .push(msg!(keys::SIM_TRACE_STEPS_EXHAUSTED, steps = steps));
        }
        RunResult::GuardFailed { step, details } => {
            report.errors.push(msg!(
                keys::SIM_TRACE_GUARD_FAILED,
                step = step,
                details = details
            ));
        }
        RunResult::EvalFailed { step, details } => {
            report.errors.push(msg!(
                keys::SIM_TRACE_EVAL_FAILED,
                step = step,
                details = details
            ));
            report.errors.push(msg!(keys::SIM_TRACE_UNRELIABLE));
        }
        RunResult::CompletedWithInvariantViolations {
            steps,
            terminated,
            violations,
        } => {
            let how = if *terminated {
                msg!(keys::SIM_TRACE_HOW_TERMINATED)
            } else {
                msg!(keys::SIM_TRACE_HOW_LIMIT)
            };
            report.info.push(msg!(
                keys::SIM_TRACE_SOFT_FINISHED,
                how = how,
                steps = steps
            ));
            report
                .errors
                .push(msg!(keys::SIM_TRACE_VIOLATIONS, count = violations.len()));
            for (step, details) in violations {
                report.errors.push(format!(
                    "  {}",
                    msg!(
                        keys::SIM_TRACE_VIOLATION_STEP,
                        step = step,
                        details = details
                    )
                ));
            }
        }
    }
    report
}

/// Человекочитаемая запись длительности: `999ms`, `1s`, `1s1ms`, `1m30s`.
///
/// Разряды переносятся, как в литерале языка: пока значение укладывается в
/// младшую единицу - печатается ею (`999ms`), при переполнении появляется
/// старшая (`1000ms` -> `1s`), а остаток дописывается справа (`1001ms` ->
/// `1s1ms`). Так запись в трассе читается тем же способом, каким автор её
/// **писал** в исходнике, и `90000ms` не приходится делить в голове.
///
/// Нулевые разряды опускаются (`3600s` -> `1h`, а не `1h0m0s`); нулевая длительность
/// печатается младшей содержательной единицей - `0ms`.
pub fn format_duration(nanos: i64) -> String {
    const UNITS: [(i64, &str); 6] = [
        (3_600_000_000_000, "h"),
        (60_000_000_000, "m"),
        (1_000_000_000, "s"),
        (1_000_000, "ms"),
        (1_000, "us"),
        (1, "ns"),
    ];
    if nanos == 0 {
        return "0ms".to_string();
    }
    let sign = if nanos < 0 { "-" } else { "" };
    // Модуль берётся с защитой от i64::MIN: `abs()` на нём паникует.
    let mut rest = nanos.unsigned_abs();
    let mut out = String::new();
    for (size, name) in UNITS {
        let size = size.unsigned_abs();
        if rest >= size {
            out.push_str(&format!("{}{}", rest / size, name));
            rest %= size;
        }
        if rest == 0 {
            break;
        }
    }
    format!("{sign}{out}")
}

pub(crate) fn format_value(v: &Value) -> String {
    match v {
        Value::Number(n) => n.to_string(),
        Value::Real(f) => format!("{f:.4}"),
        Value::Boolean(b) => b.to_string(),
        // q(m, n): показываем вещественное значение repr·2⁻ⁿ.
        Value::Fixed { repr, n, .. } => format!("{:.4}", *repr as f64 / (1u64 << n) as f64),
        // Длительность печатается человекочитаемо: наносекунды в трассе нечитаемы, а
        // выдержки задаются секундами и миллисекундами.
        Value::Duration(ns) => format_duration(*ns),
        Value::Array(arr) => format!(
            "[{}]",
            arr.iter().map(format_value).collect::<Vec<_>>().join(",")
        ),
        // Структура: `Point{x=7,y=300}` - читаемо и в объявленном порядке полей.
        Value::Struct { name, fields } => format!(
            "{name}{{{}}}",
            fields
                .iter()
                .map(|(f, v)| format!("{f}={}", format_value(v)))
                .collect::<Vec<_>>()
                .join(",")
        ),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Каждый исход прогона высказывается - и в свой поток.
    ///
    /// Тест против молчаливой потери ветви при переносе печати в носитель: текст
    /// сводки читает человек, и "прогон завершён" вместо "нарушен инвариант" выглядело
    /// бы успехом.
    #[test]
    fn every_outcome_is_reported() {
        let terminated = result_report(&RunResult::Terminated { steps: 7 });
        assert_eq!(
            terminated.info,
            vec!["Завершено: модель достигла терминального состояния за 7 шагов."]
        );
        assert!(terminated.errors.is_empty(), "исход успеха молчит в stderr");

        let reached = result_report(&RunResult::StepsReached { steps: 4 });
        assert_eq!(reached.info, vec!["Выполнено 4 шагов (лимит достигнут)."]);

        let guard = result_report(&RunResult::GuardFailed {
            step: 2,
            details: "vars[n]".to_string(),
        });
        assert!(guard.info.is_empty(), "отказ не идёт в обычный поток");
        assert_eq!(guard.errors, vec!["ОШИБКА guard на шаге 2: vars[n]"]);

        let eval = result_report(&RunResult::EvalFailed {
            step: 3,
            details: "деление на ноль".to_string(),
        });
        assert!(eval.info.is_empty());
        assert_eq!(
            eval.errors,
            vec![
                "ОШИБКА вычисления на шаге 3: деление на ноль",
                "Симуляция остановлена: результат недостоверен.",
            ]
        );

        // Мягкий режим: исход - в обычный поток, нарушения - в поток ошибок, каждое
        // своей строкой.
        let soft = result_report(&RunResult::CompletedWithInvariantViolations {
            steps: 5,
            terminated: false,
            violations: vec![(4, "нарушен инвариант 'Small' (SIM-025)".to_string())],
        });
        assert_eq!(
            soft.info,
            vec!["Прогон завершён (лимит шагов достигнут) за 5 шагов; мягкий режим инвариантов."]
        );
        assert_eq!(
            soft.errors,
            vec![
                "Нарушений инвариантов: 1 (режим --invariant-soft — прогон продолжен):",
                "  шаг 4: нарушен инвариант 'Small' (SIM-025)",
            ]
        );
    }
}
