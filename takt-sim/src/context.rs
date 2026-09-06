use crate::eval::error::EvalError;
use crate::eval::value::Value;
use std::collections::HashMap;
use takt_lang::semantic::StructDefinitionNode;
use takt_lang::semantic::type_node::TypeNode;

/// Контекст выполнения: предоставляет доступ к переменным текущей области видимости.
/// Значения, которыми стенд подменяет вызовы `extern fn`.
#[derive(Debug, Clone, Default)]
pub(crate) struct ExternStubs {
    /// Имя функции -> значения. Ключ таблицы - **первый аргумент** вызова; `None` -
    /// значение на любой вызов в этом такте.
    by_name: HashMap<String, ExternStub>,
}

/// Чем подменяется одна внешняя функция.
#[derive(Debug, Clone)]
pub(crate) enum ExternStub {
    /// Одно значение на любой вызов в этом такте.
    Any(Value),
    /// Значение по первому аргументу вызова.
    ByArgument(HashMap<i128, Value>),
}

impl ExternStubs {
    /// Объявляет подмену для функции.
    pub(crate) fn declare(&mut self, name: &str, stub: ExternStub) {
        self.by_name.insert(name.to_string(), stub);
    }

    /// Значение для вызова, если стенд его задал.
    ///
    /// Таблица ищет по **первому** аргументу: у вызова без аргументов её ключа нет, и
    /// такая подмена не срабатывает - это названная граница, а не молчание (для вызова
    /// без аргументов есть форма "одно значение").
    pub(crate) fn result(&self, name: &str, args: &[Value]) -> Option<Value> {
        match self.by_name.get(name)? {
            ExternStub::Any(value) => Some(value.clone()),
            ExternStub::ByArgument(table) => match args.first()? {
                Value::Number(key) => table.get(key).cloned(),
                Value::Boolean(flag) => table.get(&i128::from(*flag)).cloned(),
                _ => None,
            },
        }
    }

    /// Пуст ли стенд (значит, спрашивать нечего).
    pub(crate) fn is_empty(&self) -> bool {
        self.by_name.is_empty()
    }
}

pub(crate) trait Context {
    /// Возвращает клонированное значение переменной или `None`, если переменная не
    /// найдена.
    fn get_value(&self, name: &str) -> Option<Value>;
    /// Устанавливает значение переменной в текущей области видимости.
    fn set_value(&mut self, name: &str, value: Value);
    /// Определение структурного типа по имени - для приведения инициализатора `{...}` к
    /// `Value::Struct`. Умолчание - `None` (контекст без модели, напр. мок в тестах):
    /// структур нет. Контексты над моделью переопределяют, делегируя
    /// `ModelNode::search_struct` (учитывает родителей), а вложенные области - своему
    /// `outer`.
    fn find_struct(&self, _name: &str) -> Option<StructDefinitionNode> {
        None
    }
    /// Значение, которым стенд подменяет вызов `extern fn`.
    ///
    /// Умолчание - `None`: стенда нет, и вызов отвечает прежним `SIM-019`.
    fn extern_result(&self, _name: &str, _args: &[Value]) -> Option<Value> {
        None
    }

    /// Ставит стенд внешних функций.
    ///
    /// Зовётся `Unit::set_extern_stubs` перед тактом - так же, как `runner` ставит
    /// модельное время: значения принадлежат **шагу сценария**, а не модели.
    fn set_extern_stubs(&mut self, _stubs: ExternStubs) {}

    /// Перечисляет значения, составляющие состояние модели (для снимка).
    ///
    /// Включает значения родительских контекстов: для параллельных подмоделей родитель
    /// общий, и его переменные - часть их состояния. Константы и локальные переменные
    /// не включаются (см. анализ 0032). Реализация по умолчанию пуста - её достаточно
    /// для контекстов без собственного состояния.
    fn dump(&self) -> HashMap<String, Value> {
        HashMap::new()
    }
    /// Сколько модельного времени (нс) прошло с входа в текущее состояние.
    ///
    /// Отсюда `after` берёт отсчёт. Умолчание - `0`: контекст без модели (мок в тестах)
    /// времени не ведёт, и выдержка в нём никогда не истекает - это честнее, чем
    /// "истекла сразу".
    fn since_state_entry_ns(&self) -> i64 {
        0
    }
    /// Сколько **тактов** прошло с входа в текущее состояние.
    ///
    /// Отдельно от модельного времени: выдержка `after 3t` меряется шагами логики и
    /// частоты не требует.
    fn ticks_in_state(&self) -> u64 {
        0
    }

    /// Текущее состояние **другой** модели прогона - для проверки `S(Модель) =
    /// Состояние` и её краткой формы.
    ///
    /// `None` - модель в прогоне не запущена (или контекст её не знает); вызывающий
    /// отвечает диагностикой `SIM-036`, а не "условие ложно": цель `c` такую модель
    /// тоже не компилирует (`CC-012`), и молчаливое `false` дало бы расхождение эталона
    /// с прошивкой.
    ///
    /// Адресация - **по имени модели**, как у квалифицированного порта: две под-модели
    /// с одинаковым именем неразличимы. Компромисс принят осознанно и повторяет уже
    /// действующее правило.
    fn model_state(&self, _model: &str) -> Option<String> {
        None
    }

    /// Записывает текущее состояние модели в общий реестр прогона.
    ///
    /// Зовётся при постройке узла (стартовое состояние), при каждом переходе и при
    /// восстановлении из снимка. Берёт `&self`: карта живёт за `RefCell`, как кэш
    /// значений, - иначе запись во время такта требовала бы изменяемого заимствования
    /// родительского контекста и роняла бы прогон.
    fn set_model_state(&self, _model: &str, _state: &str) {}
}

/// Приводит значение к типу цели, используя реестр структур из контекста. Мост между
/// слоем адаптеров (у которых есть `Context`) и ядром
/// [`crate::eval::coerce_to_type_with`] (которому нужен `StructRegistry`).
pub(crate) fn coerce_via(
    ctx: &dyn Context,
    value: Value,
    ty: &TypeNode,
) -> Result<Value, EvalError> {
    struct Reg<'a>(&'a dyn Context);
    impl crate::eval::StructRegistry for Reg<'_> {
        fn find_struct(&self, name: &str) -> Option<StructDefinitionNode> {
            self.0.find_struct(name)
        }
    }
    crate::eval::coerce_to_type_with(value, ty, &Reg(ctx))
}

/// Обновляет значение по пути сегментов (`p.x := ...`, `data[i] := ...`), используя
/// реестр структур из контекста. `ty` - объявленный тип корневой переменной (для
/// приведения листа к типу поля/элемента). Мост к ядру [`crate::eval::place::update`].
pub(crate) fn update_place_via(
    ctx: &dyn Context,
    value: Value,
    ty: Option<&TypeNode>,
    path: &[crate::eval::place::PlaceSegment],
    new: Value,
) -> Result<Value, EvalError> {
    struct Reg<'a>(&'a dyn Context);
    impl crate::eval::StructRegistry for Reg<'_> {
        fn find_struct(&self, name: &str) -> Option<StructDefinitionNode> {
            self.0.find_struct(name)
        }
    }
    crate::eval::place::update(value, ty, path, new, &Reg(ctx))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::value::Value;
    use std::collections::HashMap;

    struct MockContext {
        vars: HashMap<String, Value>,
    }

    impl Context for MockContext {
        fn get_value(&self, name: &str) -> Option<Value> {
            self.vars.get(name).cloned()
        }

        fn set_value(&mut self, name: &str, value: Value) {
            self.vars.insert(name.to_string(), value);
        }
    }

    #[test]
    fn test_context_not_found() {
        let ctx = MockContext {
            vars: HashMap::new(),
        };
        assert!(ctx.get_value("S").is_none());
    }

    #[test]
    fn test_context_found_number() {
        let mut vars = HashMap::new();
        vars.insert("S".to_string(), Value::Number(5));
        let ctx = MockContext { vars };
        assert!(matches!(ctx.get_value("S"), Some(Value::Number(5))));
    }

    #[test]
    fn test_context_found_boolean() {
        let mut vars = HashMap::new();
        vars.insert("S".to_string(), Value::Boolean(false));
        let ctx = MockContext { vars };
        assert!(matches!(ctx.get_value("S"), Some(Value::Boolean(false))));
    }

    #[test]
    fn test_context_different_key_not_found() {
        // Контрпример: запрашиваем имя T, а сохранено S
        let mut vars = HashMap::new();
        vars.insert("S".to_string(), Value::Number(1));
        let ctx = MockContext { vars };
        assert!(ctx.get_value("T").is_none());
    }

    #[test]
    fn test_context_via_dyn() {
        // Трейт-объект: dyn Context диспетчеризуется корректно
        let mut vars = HashMap::new();
        vars.insert("S".to_string(), Value::Number(99));
        let ctx: Box<dyn Context> = Box::new(MockContext { vars });
        assert!(matches!(ctx.get_value("S"), Some(Value::Number(99))));
    }
}
