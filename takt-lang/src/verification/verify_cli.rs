//! Разбор аргументов подкоманды `taktc verify`.
//!
//! Разбор живёт в библиотеке, а бинарник держит тонкий диспетчер. Тесты разбора - при
//! потребителе.

use crate::VerifyScope;
use crate::address_map::split_include_dirs;
use crate::verification::dot::{GraphKind, parse_graph_kind};

/// Опции подкоманды `verify`.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct VerifyOptions {
    /// Путь к проверяемому `.takt`-файлу.
    pub input_file: String,
    /// Директории поиска файлов `import`.
    pub include_dirs: Vec<String>,
    /// Свойство из командной строки (`--property "G (Fault -> F Idle)"`).
    ///
    /// `None` - проверяются все формулы `: [LTL] φ;`, объявленные в файле.
    pub property: Option<String>,
    /// Печатать трассу конвейера (Крипке, автомат `¬φ`, произведение).
    pub trace: bool,
    /// Область проверки: `file` (умолчание) - модели своего файла, `all` - включая
    /// импортированные.
    pub scope: VerifyScope,
    /// Выгрузить граф верификации в DOT вместо проверки.
    pub emit_graph: Option<GraphKind>,
}

/// Разбирает значение флага `--scope`.
///
/// Негодное значение - отказ, а не молчаливое умолчание: `--scope al` иначе проверял бы
/// свой файл, отчитавшись "все держатся", и пользователь считал бы, что импорты тоже
/// проверены.
fn parse_scope(value: &str) -> Result<VerifyScope, String> {
    match value {
        "file" => Ok(VerifyScope::File),
        "all" => Ok(VerifyScope::All),
        other => Err(format!(
            "неизвестная область '{other}'; допустимо: file (модели своего файла) \
             или all (включая импортированные)"
        )),
    }
}

/// Задаёт проверяемое свойство, отвергая повтор флага.
///
/// Второй `--property` молча затирал бы первый, и `taktc verify -p "F Done" -p "G Idle"
/// m.takt` отчитался бы "проверено свойств: 1; все держатся" - про первую формулу
/// пользователь узнал бы только из исходников. Отказ по тому же правилу, что и для
/// второго файла.
fn set_property(options: &mut VerifyOptions, value: &str) -> Result<(), String> {
    if let Some(first) = &options.property {
        return Err(format!(
            "свойство задано дважды ('{first}' и '{value}'); \
             verify проверяет одно свойство за вызов"
        ));
    }
    options.property = Some(value.to_string());
    Ok(())
}

/// Разбирает аргументы подкоманды `verify`.
///
/// Принимает слайс без имени программы и без `"verify"` в начале.
pub fn parse_verify_args(args: &[String]) -> Result<VerifyOptions, String> {
    let mut options = VerifyOptions::default();
    let mut i = 0;
    while i < args.len() {
        let arg = args[i].as_str();
        match arg {
            "--property" | "-p" => {
                i += 1;
                let value = args
                    .get(i)
                    .ok_or_else(|| format!("флаг '{arg}' требует значение — LTL-формулу"))?;
                set_property(&mut options, value)?;
            }
            "--trace" => options.trace = true,
            "--emit-graph" => {
                i += 1;
                let value = args.get(i).ok_or_else(|| {
                    format!("флаг '{arg}' требует значение: kripke, buchi или product")
                })?;
                options.emit_graph = Some(parse_graph_kind(value)?);
            }
            "--scope" => {
                i += 1;
                let value = args
                    .get(i)
                    .ok_or_else(|| format!("флаг '{arg}' требует значение: file или all"))?;
                options.scope = parse_scope(value)?;
            }
            "--include-dirs" | "-I" => {
                i += 1;
                let value = args
                    .get(i)
                    .ok_or_else(|| format!("флаг '{arg}' требует значение"))?;
                options.include_dirs.extend(split_include_dirs(value));
            }
            other if other.starts_with("--property=") => {
                set_property(&mut options, &other["--property=".len()..])?;
            }
            other if other.starts_with("--scope=") => {
                options.scope = parse_scope(&other["--scope=".len()..])?;
            }
            other if other.starts_with("--emit-graph=") => {
                options.emit_graph = Some(parse_graph_kind(&other["--emit-graph=".len()..])?);
            }
            // Слитная форма `-I/путь` - как в подкоманде compile.
            other if other.starts_with("-I") && other.len() > 2 => {
                options.include_dirs.extend(split_include_dirs(&other[2..]));
            }
            other if other.starts_with('-') => {
                return Err(format!("неизвестный флаг '{other}'"));
            }
            other => {
                if !options.input_file.is_empty() {
                    return Err(format!(
                        "verify принимает один файл; лишний аргумент '{other}'"
                    ));
                }
                options.input_file = other.to_string();
            }
        }
        i += 1;
    }
    if options.input_file.is_empty() {
        return Err("укажите .takt-файл для проверки".to_string());
    }
    Ok(options)
}
