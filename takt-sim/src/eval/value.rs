/// Значение переменной или порта в симуляторе.
///
/// `PartialEq` (а не `Eq`) - из-за [`Value::Real`]: `f64` не даёт полного отношения
/// эквивалентности (`NaN != NaN`). Сравнение значений в языке идёт через
/// [`crate::eval::ops::apply_binary`], где несравнимость `NaN` - явная ошибка, а не
/// выдуманный порядок; `PartialEq` здесь нужен тестам и служебным сверкам.
///
/// `pub` (а не `pub(crate)`), поскольку значение наблюдаемо извне через
/// [`crate::unit::Unit::variable`] и уже возвращалось публичной
/// [`crate::json_input::json_to_value`] (что и давало предупреждение
/// `private_interfaces` - пункт бэклога, закрыт ).
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// Целое значение.
    ///
    /// Носитель - `i128`: `u64` - тип языка, и его верхняя половина в знаковый
    /// 64-битный не укладывается. Ширина **типа** значения этим не задаётся: усечение и
    /// обёртку по объявленному типу делает [`crate::eval::coerce_to_type`], а `i128`
    /// лишь даёт запас, чтобы промежуточный результат не переполнялся там, где итог
    /// законен.
    Number(i128),
    Real(f64),
    Boolean(bool),
    Array(Vec<Value>),
    /// Fixed-point `q(m, n)`: `repr` - знаковое представление `intW` (`W = m + n`),
    /// значение = `repr · 2⁻ⁿ`. Формат путешествует **со значением** (`m`, `n`),
    /// поэтому Q-арифметика (`*`/`/` - сдвиг на `n`) самоописательна и не требует
    /// протаскивать тип в [`crate::eval::ops`].
    Fixed {
        repr: i64,
        m: u8,
        n: u8,
        /// Насыщение вместо переноса.
        ///
        /// Признак путешествует **со значением** по той же причине, что `m` и `n`:
        /// арифметика самоописательна, и без него операция не знала бы, прижимать
        /// результат или переносить. Зеркально полю `sat` в `TypeNode::Fixed`
        /// компилятора.
        sat: bool,
    },
    /// Структурное значение: имя типа + поля в **объявленном** порядке. `Vec`, а не
    /// `BTreeMap`: инициализатор `{1, 2}` позиционный, а карта упорядочила бы поля по
    /// имени и молча перепутала их. Имя типа хранится в значении - нужно диагностике
    /// "структура 'Point'" и проверке совместимости при `q := p` без внешнего
    /// контекста.
    Struct {
        name: String,
        fields: Vec<(String, Value)>,
    },
    /// Длительность в **наносекундах** - каноническое представление языка. Отдельный
    /// вариант, а не `Number`: иначе `t + 1` прошло бы молча, и запрет смешения
    /// (`SE-065`) обходился бы через симулятор.
    ///
    /// Эталон меряет время наносекундами, а не единицами профиля: профиль - свойство
    /// **генерации** (целям нужен целый счётчик), а не модели.
    Duration(i64),
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::f64::consts::PI;

    #[test]
    fn test_number_positive() {
        assert!(matches!(Value::Number(42), Value::Number(42)));
    }

    #[test]
    fn test_number_negative() {
        assert!(matches!(Value::Number(-1), Value::Number(-1)));
    }

    #[test]
    fn test_number_zero() {
        assert!(matches!(Value::Number(0), Value::Number(0)));
    }

    #[test]
    fn test_real_value() {
        let Value::Real(x) = Value::Real(PI) else {
            panic!("ожидалось Real");
        };
        assert!((x - PI).abs() < 1e-9);
    }

    #[test]
    fn test_real_negative() {
        let Value::Real(x) = Value::Real(-0.5) else {
            panic!("ожидалось Real");
        };
        assert!((x + 0.5).abs() < 1e-9);
    }

    #[test]
    fn test_boolean_true() {
        assert!(matches!(Value::Boolean(true), Value::Boolean(true)));
    }

    #[test]
    fn test_boolean_false() {
        assert!(matches!(Value::Boolean(false), Value::Boolean(false)));
    }

    #[test]
    fn test_array_empty() {
        let Value::Array(items) = Value::Array(vec![]) else {
            panic!("ожидалось Array");
        };
        assert!(items.is_empty());
    }

    #[test]
    fn test_array_nested() {
        let v = Value::Array(vec![
            Value::Number(1),
            Value::Boolean(true),
            Value::Array(vec![Value::Real(0.5)]),
        ]);
        let Value::Array(items) = &v else {
            panic!("ожидалось Array");
        };
        assert_eq!(items.len(), 3);
        assert!(matches!(items[0], Value::Number(1)));
        assert!(matches!(items[1], Value::Boolean(true)));
        assert!(matches!(items[2], Value::Array(_)));
    }

    #[test]
    fn test_clone_number() {
        let original = Value::Number(100);
        let cloned = original.clone();
        assert!(matches!(cloned, Value::Number(100)));
    }

    #[test]
    fn test_clone_array_is_deep() {
        let original = Value::Array(vec![Value::Number(1), Value::Number(2)]);
        let Value::Array(items) = original.clone() else {
            panic!("ожидалось Array");
        };
        // Клон содержит те же элементы
        assert_eq!(items.len(), 2);
        assert!(matches!(items[0], Value::Number(1)));
    }

    #[test]
    fn test_debug_contains_value() {
        assert!(format!("{:?}", Value::Number(7)).contains('7'));
        assert!(format!("{:?}", Value::Boolean(true)).contains("true"));
        assert!(format!("{:?}", Value::Array(vec![])).contains("Array"));
    }
}
