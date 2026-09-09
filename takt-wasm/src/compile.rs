//! Компиляция цели в браузере: те же ключи, тот же вывод.
//!
//! # Что здесь есть и чего нет
//!
//! Есть перевод строки ключей в опции - **тем же** разбором, что у `taktc compile`
//! (`compile_cli::parse_compile_args`), и печать цели в память
//! (`takt_lang::compile::compile_texts`). Нет ничего своего: ни списка целей, ни
//! таблицы применимости флагов, ни второго способа собрать `GenerateOptions`. Это
//! условие критерия приёмки - вывод в браузере обязан совпадать с выводом `taktc`
//! **байт в байт**, а совпадение двух реализаций проверить нечем.
//!
//! Импорт в браузере не работает: файловой системы нет, список путей поиска пуст, и
//! `import "file.takt";` кончается `SE-001`. Это названная граница (фичи), а
//! не недоделка.

use serde::Serialize;
use takt_lang::compile::{CompileInput, Target};
use takt_lang::compile_cli::{CompileOptions, generate_options, parse_compile_args, target_flags};

use crate::reply::{self, DiagnosticJson};

/// Имя файла, под которым модуль компилирует безымянный исходник.
///
/// Имя корневой модели берётся из имени файла, поэтому оно есть часть вывода:
/// `stacker.takt` даёт `stacker.h`, а `playground.takt` - `playground.h`. Отсюда
/// правило: страница, которой важно имя, передаёт его позиционным аргументом - ровно
/// как в командной строке.
pub const DEFAULT_FILENAME: &str = "playground.takt";

/// Один файл вывода.
#[derive(Debug, Serialize)]
struct FileJson {
    name: String,
    text: String,
}

/// Успешный ответ компиляции.
#[derive(Debug, Serialize)]
struct CompiledJson {
    target: &'static str,
    filename: String,
    files: Vec<FileJson>,
    warnings: Vec<DiagnosticJson>,
}

/// Разбирает цель и ключи - тем же разбором, что у `taktc compile`.
///
/// Носитель один на обе операции модуля: компиляцию и проверку ключей ([`check`]).
/// Разойдись они - страница принимала бы ключ, который сборка отвергает, либо наоборот,
/// и увидеть это можно было бы только сборкой.
///
/// # Ошибки
/// Незакрытая кавычка, неизвестная цель, неразбираемый ключ либо ключ,
/// неприменимый к цели (таблица `target_flags`).
fn prepare(target: &str, args: &str, filename: &str) -> Result<(Target, CompileOptions), String> {
    let Some(parsed) = Target::parse(target) else {
        return Err(format!(
            "неизвестная цель '{target}'. Поддерживается: {}",
            Target::ALL
                .iter()
                .map(|t| t.name())
                .collect::<Vec<_>>()
                .join(", ")
        ));
    };

    let mut argv = split_args(args)?;
    // Позиционный аргумент - имя файла: `parse_compile_args` требует его, как и
    // командная строка. Своего значения по умолчанию у разбора нет, и это верно: у CLI
    // файл обязателен.
    if !argv.iter().any(|a| !a.starts_with('-')) {
        // Имя, названное вызывающим, сильнее умолчания: у открытого файла проекта
        // оно своё, и вывод обязан нести его, а не имя безымянного буфера.
        argv.push(if filename.is_empty() {
            DEFAULT_FILENAME.to_string()
        } else {
            filename.to_string()
        });
    }
    argv.push("--target".to_string());
    argv.push(parsed.name().to_string());

    let options =
        parse_compile_args(&argv).map_err(|message| format!("ключи сборки: {message}"))?;

    // Применимость ключа к цели - та же таблица, что у CLI: `--bus=apb` у `rust` обязан
    // отказывать и здесь, а не приниматься молча.
    let mut raised: Vec<&str> = Vec::new();
    if options.fsm == takt_lang::generator::FsmForm::Table {
        raised.push("--fsm=table");
    }
    if options.bus.is_some() {
        raised.push("--bus=apb");
    }
    target_flags::check(parsed.name(), &raised)?;
    Ok((parsed, options))
}

/// Проверяет цель и ключи, ничего не компилируя.
///
/// Нужна серверу: цель и ключи стали свойством проекта, а отвергать
/// негодные при записи. Проверять их своим разбором сервер не может: список ключей и
/// таблица применимости живут в `compile_cli`, и вторая копия разошлась бы с ней молча.
/// Отсюда круговой рейс через модуль - тот же приём, что у сборки архива.
pub fn check(target: &str, args: &str) -> String {
    match prepare(target, args, "") {
        Ok(_) => reply::ok(serde_json::json!({})),
        Err(message) => reply::refused(message),
    }
}

/// Компилирует исходник целью `target` с ключами `args`.
///
/// `args` - строка ключей `taktc compile` (`--fsm=table -I lib`, имя файла позиционным
/// аргументом). Ключ `-o` разбирается, но не действует: писать некуда.
pub fn compile(target: &str, args: &str, source: &str, filename: &str) -> String {
    let (target, options) = match prepare(target, args, filename) {
        Ok(prepared) => prepared,
        Err(message) => return reply::refused(message),
    };

    let generate = generate_options(&options);
    let input = CompileInput::new(&options.input_file, source, &[], &generate);
    match takt_lang::compile::compile_texts(target, &input) {
        Ok(output) => reply::ok(CompiledJson {
            target: target.name(),
            filename: options.input_file.clone(),
            files: output
                .files
                .into_iter()
                .map(|f| FileJson {
                    name: f.name,
                    text: f.text,
                })
                .collect(),
            warnings: output
                .warnings
                .iter()
                .map(|w| DiagnosticJson::of(w, source))
                .collect(),
        }),
        Err(diagnostic) => reply::failed(&diagnostic, source),
    }
}

/// Разбивает строку ключей на аргументы, уважая кавычки.
///
/// Разбиение по пробелам - не каприз: командную строку страница пишет текстом, а путь
/// `-I my lib` без кавычек означал бы два аргумента и там, и в терминале. Незакрытая
/// кавычка - отказ, а не молчаливое склеивание.
fn split_args(args: &str) -> Result<Vec<String>, String> {
    let mut out = Vec::new();
    let mut current = String::new();
    let mut quote: Option<char> = None;
    let mut has_current = false;
    for ch in args.chars() {
        match quote {
            Some(q) if ch == q => quote = None,
            Some(_) => current.push(ch),
            None if ch == '"' || ch == '\'' => {
                quote = Some(ch);
                has_current = true;
            }
            None if ch.is_whitespace() => {
                if has_current {
                    out.push(std::mem::take(&mut current));
                    has_current = false;
                }
            }
            None => {
                current.push(ch);
                has_current = true;
            }
        }
    }
    if quote.is_some() {
        return Err("незакрытая кавычка в ключах сборки".to_string());
    }
    if has_current {
        out.push(current);
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::Value;

    fn json(text: &str) -> Value {
        serde_json::from_str(text).expect("ответ моста — JSON")
    }

    const MODEL: &str = "var x: u8 := 0;\n\nstart S {\n    always {\n        x := 1;\n    }\n}\n";

    /// Цель `c` отдаёт два файла, названных по имени входа.
    #[test]
    fn compiles_c_into_two_files() {
        let reply = json(&compile("c", "stacker.takt", MODEL, ""));
        assert_eq!(reply["ok"], Value::Bool(true), "{reply}");
        let names: Vec<&str> = reply["files"]
            .as_array()
            .unwrap()
            .iter()
            .map(|f| f["name"].as_str().unwrap())
            .collect();
        assert_eq!(names, vec!["stacker.h", "stacker.c"]);
        assert_eq!(reply["filename"], Value::String("stacker.takt".into()));
    }

    /// Без имени входа берётся [`DEFAULT_FILENAME`].
    #[test]
    fn nameless_source_gets_default_filename() {
        let reply = json(&compile("rust", "", MODEL, ""));
        assert_eq!(reply["ok"], Value::Bool(true), "{reply}");
        assert_eq!(
            reply["files"][0]["name"],
            Value::String("playground.rs".into())
        );
    }

    /// Неизвестная цель - отказ вызова со списком поддерживаемых.
    #[test]
    fn unknown_target_is_refused_with_list() {
        let reply = json(&compile("verilog", "", MODEL, ""));
        assert_eq!(reply["ok"], Value::Bool(false));
        let message = reply["error"]["message"].as_str().unwrap();
        assert!(message.contains("sv-mmio"), "нет списка целей: {message}");
        assert!(reply["error"]["code"].is_null(), "у отказа вызова кода нет");
    }

    /// Ошибка модели - диагностика цели: код и позиция, а не строка без роду.
    #[test]
    fn model_error_carries_code_and_position() {
        let reply = json(&compile(
            "c",
            "",
            "start S {\n    ref Missing: 1 = 1;\n}\n",
            "",
        ));
        assert_eq!(reply["ok"], Value::Bool(false), "{reply}");
        assert!(
            reply["error"]["code"].as_str().is_some(),
            "у диагностики обязан быть код: {reply}"
        );
        assert_eq!(
            reply["error"]["line"].as_u64(),
            Some(2),
            "позиция считается по тексту в памяти: {reply}"
        );
    }

    /// Ключ, неприменимый к цели, отвергается - той же таблицей, что у CLI.
    #[test]
    fn flag_not_applicable_to_target_is_refused() {
        let reply = json(&compile("rust", "--bus=apb", MODEL, ""));
        assert_eq!(reply["ok"], Value::Bool(false), "{reply}");
        let message = reply["error"]["message"].as_str().unwrap();
        assert!(
            message.contains("--bus"),
            "отказ обязан назвать ключ: {message}"
        );
    }

    /// Импорт - названная граница: файловой системы нет.
    #[test]
    fn import_is_refused_with_diagnostic() {
        let reply = json(&compile("c", "", "import \"lib.takt\";\nstart S;\n", ""));
        assert_eq!(reply["ok"], Value::Bool(false), "{reply}");
        assert!(
            !reply["error"]["message"].as_str().unwrap().is_empty(),
            "отказ обязан быть назван"
        );
    }

    /// Годные ключи проверка принимает, ничего не компилируя.
    #[test]
    fn check_accepts_applicable_flags() {
        let reply = json(&check("sv-mmio", "--bus=apb --fsm=table"));
        assert_eq!(reply["ok"], Value::Bool(true), "{reply}");
    }

    /// Ключ, неприменимый к цели, отвергается - той же таблицей, что у сборки.
    ///
    /// Контроль обязателен: `--bus=apb` у `sv-mmio` принимается (проверка
    /// выше), и без него правило "отвергать всё" выглядело бы работающим.
    #[test]
    fn check_refuses_flag_not_for_target() {
        let reply = json(&check("rust", "--bus=apb"));
        assert_eq!(reply["ok"], Value::Bool(false), "{reply}");
        let message = reply["error"]["message"].as_str().unwrap();
        assert!(message.contains("--bus"), "причина не названа: {message}");
    }

    /// Неизвестный ключ и неизвестная цель - отказ с причиной.
    #[test]
    fn check_refuses_unknown_flag_and_target() {
        let unknown = json(&check("c", "--нет-такого"));
        assert_eq!(unknown["ok"], Value::Bool(false), "{unknown}");
        let target = json(&check("verilog", ""));
        assert_eq!(target["ok"], Value::Bool(false), "{target}");
        assert!(
            target["error"]["message"]
                .as_str()
                .unwrap()
                .contains("sv-mmio"),
            "отказ цели обязан назвать список"
        );
    }

    /// Кавычки в ключах: путь с пробелом остаётся одним аргументом.
    #[test]
    fn quoted_argument_stays_whole() {
        assert_eq!(
            split_args("-I \"my lib\" model.takt").unwrap(),
            vec!["-I", "my lib", "model.takt"]
        );
        assert!(
            split_args("-I \"unclosed").is_err(),
            "незакрытая кавычка — отказ"
        );
    }
}
