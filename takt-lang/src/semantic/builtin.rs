//! Встроенные функции языка Takt.
//!
//! Встроенные функции не объявляются пользователем явно - они всегда доступны в любом
//! контексте программы Takt. Реестр определён как статическая PHF-таблица
//! [`BUILTIN_FUNCTIONS`].
//!
//! ## Текущий список встроенных функций
//!
//! | Имя | Параметры | Возвращаемый тип | Описание |
//! |---------|----------------------------------------|-------------------|---------------------------------------|
//! | `debug` | `text: BuiltinString` | `Unit` | Вывод отладочного сообщения |
//! | `S` | `model: BuiltinModel` | `BuiltinState` | **Текущее** состояние модели |
//! | `min` | `a: BuiltinNumeric, b: BuiltinNumeric` | `BuiltinNumeric` | Минимум из двух числовых значений |
//! | `max` | `a: BuiltinNumeric, b: BuiltinNumeric` | `BuiltinNumeric` | Максимум из двух числовых значений |
//! | `abs` | `x: BuiltinNumeric` | `BuiltinNumeric` | Абсолютное значение числа |
//! | `clamp` | `x, lo, hi: BuiltinNumeric` | `BuiltinNumeric` | Ограничение значения в диапазон |

use crate::diagnostics::Diagnostic;
use crate::diagnostics::lang::keys;
use crate::msg;
use crate::semantic::FunctionDefinitionNode;
use crate::semantic::type_node::TypeNode;
use phf::phf_map;

/// Статическая таблица встроенных функций языка Takt.
///
/// Ключ - имя функции, значение - [`FunctionDefinitionNode::Builtin`].
const BUILTIN_FUNCTIONS: phf::Map<&'static str, FunctionDefinitionNode> = phf_map! {
    "debug" => FunctionDefinitionNode::Builtin("debug", &[("text", TypeNode::BuiltinString)], TypeNode::Unit),
    "S" => FunctionDefinitionNode::Builtin("S", &[("model", TypeNode::BuiltinModel)], TypeNode::BuiltinState),
    "min" => FunctionDefinitionNode::Builtin("min", &[("a", TypeNode::BuiltinNumeric), ("b", TypeNode::BuiltinNumeric)], TypeNode::BuiltinNumeric),
    "max" => FunctionDefinitionNode::Builtin("max", &[("a", TypeNode::BuiltinNumeric), ("b", TypeNode::BuiltinNumeric)], TypeNode::BuiltinNumeric),
    "abs" => FunctionDefinitionNode::Builtin("abs", &[("x", TypeNode::BuiltinNumeric)], TypeNode::BuiltinNumeric),
    "clamp" => FunctionDefinitionNode::Builtin("clamp", &[("x", TypeNode::BuiltinNumeric), ("lo", TypeNode::BuiltinNumeric), ("hi", TypeNode::BuiltinNumeric)], TypeNode::BuiltinNumeric),
};

/// Возвращает [`FunctionDefinitionNode`] встроенной функции по имени.
///
/// # Ошибки
///
/// Возвращает [`Diagnostic`], если функция с указанным именем не найдена в реестре
/// встроенных функций.
///
/// # Примеры
///
/// ```but
/// debug("значение x");   // встроенная функция debug
/// start A = S(M) { }    // встроенная функция S
/// ```
pub fn builtin_function(name: &str) -> Result<&FunctionDefinitionNode, Diagnostic> {
    BUILTIN_FUNCTIONS.get(name).ok_or_else(|| {
        // Позиции у имени здесь нет: реестр встроенных функций спрашивают по строке.
        // Координату ставит вызывающий - `resolve_*` знает узел. Своя подстановка дала
        // бы `Source(0, 0, 0)`, то есть "начало первого файла".
        Diagnostic::error(
            crate::diagnostics::Location::Builtin,
            msg!(keys::SE_004_UNKNOWN_FUNCTION, name = name),
        )
        .with_code("SE-004")
    })
}

// -- Тесты ---------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic::type_node::TypeNode;

    /// `builtin_function("debug")` возвращает корректный узел.
    #[test]
    fn builtin_debug_exists() {
        let func = builtin_function("debug").unwrap();
        assert!(
            matches!(
                func,
                FunctionDefinitionNode::Builtin("debug", _, TypeNode::Unit)
            ),
            "debug должна быть Builtin с возвратом Unit"
        );
    }

    /// `builtin_function("S")` возвращает корректный узел.
    #[test]
    fn builtin_s_exists() {
        let func = builtin_function("S").unwrap();
        assert!(
            matches!(
                func,
                FunctionDefinitionNode::Builtin("S", _, TypeNode::BuiltinState)
            ),
            "S должна быть Builtin с возвратом BuiltinState"
        );
    }

    /// Контрпример: неизвестная встроенная функция -> ошибка.
    ///
    /// # Контрпример (Takt)
    /// ```but
    /// ghost();   // ошибка: функция ghost не определена
    /// ```
    #[test]
    fn unknown_builtin_is_error() {
        let result = builtin_function("ghost");
        assert!(result.is_err(), "неизвестная функция должна давать ошибку");
        let err = result.unwrap_err();
        assert!(
            err.message.contains("ghost"),
            "сообщение должно содержать имя функции: {}",
            err.message
        );
    }

    /// Параметры `debug`: один параметр `text: BuiltinString`.
    #[test]
    fn builtin_debug_has_correct_params() {
        if let FunctionDefinitionNode::Builtin(_, params, _) = builtin_function("debug").unwrap() {
            assert_eq!(params.len(), 1);
            assert_eq!(params[0].0, "text");
            assert_eq!(params[0].1, TypeNode::BuiltinString);
        } else {
            panic!("ожидался FunctionNode::Builtin");
        }
    }

    /// Параметры `S`: один параметр `model: BuiltinModel`.
    #[test]
    fn builtin_s_has_correct_params() {
        if let FunctionDefinitionNode::Builtin(_, params, ret) = builtin_function("S").unwrap() {
            assert_eq!(params.len(), 1);
            assert_eq!(params[0].0, "model");
            assert_eq!(params[0].1, TypeNode::BuiltinModel);
            assert_eq!(*ret, TypeNode::BuiltinState);
        } else {
            panic!("ожидался FunctionNode::Builtin");
        }
    }

    // -- Тесты min -------------------------------------------------------------

    /// `builtin_function("min")` возвращает корректный узел с двумя числовыми
    /// параметрами.
    ///
    /// # Пример (Takt):
    /// ```but
    /// var result: bit = min(a, b);   // минимум двух значений
    /// ```
    #[test]
    fn builtin_min_exists() {
        let func = builtin_function("min").unwrap();
        assert!(
            matches!(
                func,
                FunctionDefinitionNode::Builtin("min", _, TypeNode::BuiltinNumeric)
            ),
            "min должна быть Builtin с возвратом BuiltinNumeric"
        );
    }

    /// Параметры `min`: два параметра `a: BuiltinNumeric`, `b: BuiltinNumeric`.
    #[test]
    fn builtin_min_has_two_numeric_params() {
        if let FunctionDefinitionNode::Builtin(_, params, ret) = builtin_function("min").unwrap() {
            assert_eq!(params.len(), 2, "min должна принимать 2 параметра");
            assert_eq!(params[0].0, "a");
            assert_eq!(params[0].1, TypeNode::BuiltinNumeric);
            assert_eq!(params[1].0, "b");
            assert_eq!(params[1].1, TypeNode::BuiltinNumeric);
            assert_eq!(*ret, TypeNode::BuiltinNumeric);
        } else {
            panic!("ожидался FunctionNode::Builtin");
        }
    }

    // -- Тесты max -------------------------------------------------------------

    /// `builtin_function("max")` возвращает корректный узел с двумя числовыми
    /// параметрами.
    ///
    /// # Пример (Takt):
    /// ```but
    /// var result: bit = max(a, b);   // максимум двух значений
    /// ```
    #[test]
    fn builtin_max_exists() {
        let func = builtin_function("max").unwrap();
        assert!(
            matches!(
                func,
                FunctionDefinitionNode::Builtin("max", _, TypeNode::BuiltinNumeric)
            ),
            "max должна быть Builtin с возвратом BuiltinNumeric"
        );
    }

    /// Параметры `max`: два параметра `a: BuiltinNumeric`, `b: BuiltinNumeric`.
    #[test]
    fn builtin_max_has_two_numeric_params() {
        if let FunctionDefinitionNode::Builtin(_, params, ret) = builtin_function("max").unwrap() {
            assert_eq!(params.len(), 2, "max должна принимать 2 параметра");
            assert_eq!(params[0].0, "a");
            assert_eq!(params[0].1, TypeNode::BuiltinNumeric);
            assert_eq!(params[1].0, "b");
            assert_eq!(params[1].1, TypeNode::BuiltinNumeric);
            assert_eq!(*ret, TypeNode::BuiltinNumeric);
        } else {
            panic!("ожидался FunctionNode::Builtin");
        }
    }

    // -- Тесты abs -------------------------------------------------------------

    /// `builtin_function("abs")` возвращает корректный узел с одним числовым
    /// параметром.
    ///
    /// # Пример (Takt):
    /// ```but
    /// var result: bit = abs(x);   // абсолютное значение
    /// ```
    #[test]
    fn builtin_abs_exists() {
        let func = builtin_function("abs").unwrap();
        assert!(
            matches!(
                func,
                FunctionDefinitionNode::Builtin("abs", _, TypeNode::BuiltinNumeric)
            ),
            "abs должна быть Builtin с возвратом BuiltinNumeric"
        );
    }

    /// Параметры `abs`: один параметр `x: BuiltinNumeric`.
    #[test]
    fn builtin_abs_has_one_numeric_param() {
        if let FunctionDefinitionNode::Builtin(_, params, ret) = builtin_function("abs").unwrap() {
            assert_eq!(params.len(), 1, "abs должна принимать 1 параметр");
            assert_eq!(params[0].0, "x");
            assert_eq!(params[0].1, TypeNode::BuiltinNumeric);
            assert_eq!(*ret, TypeNode::BuiltinNumeric);
        } else {
            panic!("ожидался FunctionNode::Builtin");
        }
    }

    /// Контрпример: `builtin_function("sin")` - неизвестная функция -> ошибка.
    ///
    /// # Контрпример (Takt):
    /// ```but
    /// sin(3.14);   // ошибка: функция sin не определена
    /// ```
    #[test]
    fn unknown_math_builtin_is_error() {
        let result = builtin_function("sin");
        assert!(result.is_err(), "sin должна быть неизвестной");
    }

    /// Все встроенные математические функции доступны через `builtin_function`.
    #[test]
    fn all_math_builtins_registered() {
        for name in &["min", "max", "abs", "clamp"] {
            assert!(
                builtin_function(name).is_ok(),
                "Встроенная функция '{}' должна быть зарегистрирована",
                name
            );
        }
    }

    // -- Тесты clamp -----------------------------------------------------------

    /// `builtin_function("clamp")` возвращает корректный узел с тремя параметрами.
    ///
    /// # Пример (Takt):
    /// ```but
    /// var r: bit = clamp(x, lo, hi);   // ограничение x в [lo, hi]
    /// ```
    #[test]
    fn builtin_clamp_exists() {
        let func = builtin_function("clamp").unwrap();
        assert!(
            matches!(
                func,
                FunctionDefinitionNode::Builtin("clamp", _, TypeNode::BuiltinNumeric)
            ),
            "clamp должна быть Builtin с возвратом BuiltinNumeric"
        );
    }

    /// Параметры `clamp`: три параметра `x`, `lo`, `hi` типа `BuiltinNumeric`.
    #[test]
    fn builtin_clamp_has_three_params() {
        if let FunctionDefinitionNode::Builtin(_, params, ret) = builtin_function("clamp").unwrap()
        {
            assert_eq!(params.len(), 3, "clamp должна принимать 3 параметра");
            assert_eq!(params[0].0, "x");
            assert_eq!(params[1].0, "lo");
            assert_eq!(params[2].0, "hi");
            for (_, ty) in *params {
                assert_eq!(*ty, TypeNode::BuiltinNumeric);
            }
            assert_eq!(*ret, TypeNode::BuiltinNumeric);
        } else {
            panic!("ожидался FunctionNode::Builtin");
        }
    }
}
