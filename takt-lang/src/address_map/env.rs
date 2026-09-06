//! Среда символов адреса из `--define` и её разбор.
//!
//! Тема самостоятельна: описывает **источник значений** для вычислителя
//! ([`eval`](super::eval)), но арифметику и разрешение имён не выполняет.

use super::parse::{is_name_cont, is_name_start, parse_address_token};
use crate::diagnostics::{Diagnostic, Location};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};

/// Среда символов адреса: имя -> значение.
#[derive(Debug, Default, Clone)]
pub struct AddressEnv {
    /// Символ -> `(адрес, бит)`; значение разбирает `parse_address_token` - та же
    /// грамматика, что у записи внешней карты (решение C1).
    symbols: HashMap<String, (i64, Option<i64>)>,
    /// Имена, к которым обратилось хотя бы одно выражение адреса.
    ///
    /// Нужны для `DF-004`: define, которого никто не спросил, - почти всегда опечатка
    /// (симметрия с `SE-051` - висячей записью карты).
    used: RefCell<HashSet<String>>,
    /// Имена, где define перекрыл одноимённую `const` модели, и позиция `const`.
    ///
    /// Копится по ходу вычисления, а печатается вызывающим: `SE-053` обязана указывать
    /// на **объявление `const`** - то, что перекрыто.
    overridden: RefCell<Vec<(String, Location)>>,
}

impl AddressEnv {
    /// Собирает среду из разобранных пар `(имя, значение)`.
    pub fn new(symbols: HashMap<String, (i64, Option<i64>)>) -> Self {
        AddressEnv {
            symbols,
            used: RefCell::new(HashSet::new()),
            overridden: RefCell::new(Vec::new()),
        }
    }

    /// Значение символа; обращение запоминается (для `DF-004`).
    pub(super) fn lookup(&self, name: &str) -> Option<(i64, Option<i64>)> {
        let found = self.symbols.get(name).copied();
        if found.is_some() {
            self.used.borrow_mut().insert(name.to_string());
        }
        found
    }

    /// Имена define'ов, к которым не обратилось ни одно выражение адреса.
    ///
    /// Порядок - алфавитный: диагностики обязаны быть детерминированными.
    pub fn unused(&self) -> Vec<String> {
        let used = self.used.borrow();
        let mut out: Vec<String> = self
            .symbols
            .keys()
            .filter(|n| !used.contains(*n))
            .cloned()
            .collect();
        out.sort();
        out
    }

    /// Пуста ли среда (define'ов не передавали).
    pub fn is_empty(&self) -> bool {
        self.symbols.is_empty()
    }

    /// Запоминает, что define перекрыл `const` с этим именем.
    pub(super) fn note_override(&self, name: &str, const_loc: Location) {
        let mut o = self.overridden.borrow_mut();
        if !o.iter().any(|(n, _)| n == name) {
            o.push((name.to_string(), const_loc));
        }
    }

    /// Перекрытия `const` define'ами: `(имя, позиция const)`, по алфавиту.
    pub(super) fn overrides(&self) -> Vec<(String, Location)> {
        let mut out = self.overridden.borrow().clone();
        out.sort_by(|a, b| a.0.cmp(&b.0));
        out
    }
}

/// Разбирает аргументы `--define NAME=VALUE` в среду символов.
///
/// Грамматика значения - **та же**, что у записи внешней карты
/// (`parse_address_token`): `0x...` / десятичное / необязательный `:bit`. Это
/// решение C1: третий мини-язык не заводится, а арифметика остаётся
/// **в модели** (`address BTN = BASE + 4;`), где её видит ревьюер и форматтер,
/// а не в командной строке.
///
/// # Коды диагностик
///
/// - `DF-001` - нет `=`, пустое или некорректное имя (симметрия с `AM-001/002`);
/// - `DF-002` - некорректный литерал значения (симметрия с `AM-005`);
/// - `DF-003` - повторный `--define` для одного имени (симметрия с `AM-006`).
///
/// У аргумента командной строки нет позиции в файле, поэтому диагностики несут
/// [`Location::CommandLine`].
pub fn parse_defines(args: &[String]) -> Result<AddressEnv, Vec<Diagnostic>> {
    let mut symbols: HashMap<String, (i64, Option<i64>)> = HashMap::new();
    let mut diags: Vec<Diagnostic> = Vec::new();

    for arg in args {
        let Some((name, value)) = arg.split_once('=') else {
            diags.push(
                Diagnostic::error(
                    Location::CommandLine,
                    format!("--define '{}': ожидалось NAME=VALUE (нет '=')", arg),
                )
                .with_code("DF-001"),
            );
            continue;
        };
        if !is_valid_symbol(name) {
            diags.push(
                Diagnostic::error(
                    Location::CommandLine,
                    format!(
                        "--define '{}': некорректное имя символа '{}' \
                         (ожидается [A-Za-z_][A-Za-z0-9_]*)",
                        arg, name
                    ),
                )
                .with_code("DF-001"),
            );
            continue;
        }
        let parsed = match parse_address_token(value) {
            Ok(v) => v,
            Err(e) => {
                diags.push(
                    Diagnostic::error(Location::CommandLine, format!("--define '{}': {}", arg, e))
                        .with_code("DF-002"),
                );
                continue;
            }
        };
        // Повтор - ошибка, а не "побеждает последний": молчаливое затирание сделало бы
        // адрес зависящим от порядка флагов (симметрия с `AM-006`).
        if symbols.contains_key(name) {
            diags.push(
                Diagnostic::error(
                    Location::CommandLine,
                    format!("--define: символ '{}' задан дважды", name),
                )
                .with_code("DF-003"),
            );
            continue;
        }
        symbols.insert(name.to_string(), parsed);
    }

    if diags.is_empty() {
        Ok(AddressEnv::new(symbols))
    } else {
        Err(diags)
    }
}

/// Имя символа: `[A-Za-z_][A-Za-z0-9_]*` - как имя порта в карте.
fn is_valid_symbol(name: &str) -> bool {
    let mut chars = name.chars();
    match chars.next() {
        Some(c) if is_name_start(c) => {}
        _ => return false,
    }
    chars.all(is_name_cont)
}
