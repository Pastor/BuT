//! Печать **вызова функции** в цели `sv`.
//!
//! Граница по ответственности: печать выражения отвечает "как выглядит операция", этот
//! модуль - "как выглядит вызов". Поводом был проверка размера модуля, границей - смысл.

use crate::diagnostics::{Diagnostic, Location};
use crate::generator::sv::sv_expr::sv002;
use crate::generator::sv::sv_expr::sv005;
use crate::semantic::FunctionDefinitionNode;

/// Возвращает имя вызываемой функции, отвергая невыразимые случаи.
///
/// # Ошибки
/// - [`SV-005`](sv005) - `extern fn`: в синтезируемом RTL вызова внешнего кода
///   не существует;
/// - [`SV-002`](sv002) - неразрешённое определение функции.
pub(in crate::generator::sv) fn local_function_name(
    func: &FunctionDefinitionNode,
    loc: Location,
) -> Result<String, Diagnostic> {
    match func {
        FunctionDefinitionNode::Local { name, .. } => Ok(name.clone()),
        FunctionDefinitionNode::External { name, .. } => Err(sv005(name, loc)),
        // Встроенные (`min`/`max`/`abs`/`debug`) требуют каждая своего разворачивания и
        // разбираются отдельно (`print_builtin`); сюда попасть не должны.
        FunctionDefinitionNode::Builtin(name, _, _) => Err(sv002(&format!(
            "встроенная функция '{}' в этой позиции",
            name
        ))),
        FunctionDefinitionNode::None | FunctionDefinitionNode::Unresolved(_) => {
            Err(sv002("неразрешённый вызов функции"))
        }
    }
}

/// Печатает вызов функции по уже напечатанным аргументам.
///
/// Общий хвост обоих печатающих путей (условие и выражение): грамматики разные, а
/// правила вызова - одни.
///
/// # Ошибки
/// [`SV-005`](sv005) на `extern fn`, [`SV-002`](sv002) на непереводимой
/// встроенной функции.
pub(in crate::generator::sv) fn print_call(
    func: &FunctionDefinitionNode,
    args: &[String],
    loc: Location,
) -> Result<String, Diagnostic> {
    if let FunctionDefinitionNode::Builtin(name, _, _) = func {
        return print_builtin(name, args, loc);
    }
    Ok(format!(
        "{}({})",
        local_function_name(func, loc)?,
        args.join(", ")
    ))
}

/// Разворачивает встроенную функцию языка в выражение SystemVerilog.
///
/// Функции языка (`min`/`max`/`abs`) в SV не существуют - там они
/// **разворачиваются** в тернарный оператор, то есть в мультиплексор. Это не
/// обход, а прямое соответствие: в RTL выбор меньшего из двух и есть
/// мультиплексор со сравнителем.
///
/// # Ошибки
/// [`SV-002`](sv002) на `debug` и на неизвестной встроенной функции.
fn print_builtin(name: &str, args: &[String], _loc: Location) -> Result<String, Diagnostic> {
    match (name, args) {
        ("min", [a, b]) => Ok(format!("(({} < {}) ? {} : {})", a, b, a, b)),
        ("max", [a, b]) => Ok(format!("(({} > {}) ? {} : {})", a, b, a, b)),
        ("abs", [a]) => Ok(format!("(({} < 0) ? -{} : {})", a, a, a)),
        // Молчаливо отбросить нельзя: ровно эту тихую потерю закрыла.
        ("debug", _) => Err(sv002(
            "встроенная функция 'debug': в синтезируемом RTL вывода текста не \
             существует — печатать некуда и нечем. Отладка RTL ведётся \
             осциллограммой сигналов, а не печатью; используйте цель \
             'c'/'rust', если нужен вывод",
        )),
        (other, _) => Err(sv002(&format!(
            "встроенная функция '{}' с таким числом аргументов",
            other
        ))),
    }
}
