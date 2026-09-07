use crate::eval::value::Value;
use serde::Deserialize;
use std::collections::{BTreeMap, HashMap};
use std::path::Path;

// -- Структуры шага симуляции -------------------------------------------------

/// Значения портов шага: **позиционные** либо **именованные**.
///
/// ```json
/// { "in_ports": [0, 1, 0] }                       // позиционная форма
/// { "in_ports": {"floor_sensor_f2_bottom": 1} }   // именованная форма
/// ```
///
/// Позиционная форма хрупка не только для чтения: индекс - это место имени в
/// **алфавитном** списке портов модели и её под-моделей, поэтому добавление или
/// переименование порта сдвигает весь массив, и шаг начинает описывать другое
/// событие - молча. Именованная форма от этого защищена:
/// несуществующее имя становится ошибкой.
///
/// Формы не пересекаются по представлению (массив против объекта), поэтому `untagged`
/// разбирает их однозначно.
#[derive(Deserialize, Clone, Debug)]
#[serde(untagged)]
pub enum PortValues {
    /// Значения по порядку портов (историческая форма).
    Positional(Vec<serde_json::Value>),
    /// Значения по именам портов; имя может быть квалифицированным (`Модель::порт`).
    ///
    /// `BTreeMap`, а не `HashMap`: порядок обхода решает, **какая** ошибка будет
    /// названа первой, и он обязан быть воспроизводимым.
    Named(BTreeMap<String, serde_json::Value>),
}

/// Один шаг симуляции из JSON-файла.
///
/// ```json
/// { "in_ports": {"btn": 1}, "inout": [8], "guard": { "out": {"lamp": 0}, "vars": {"M::x": 1} } }
/// ```
#[derive(Deserialize, Default, Clone)]
pub struct SimStep {
    #[serde(rename = "in_ports")]
    pub in_ports: Option<PortValues>,
    /// На сколько продвинуть модельные часы перед этим тактом, в миллисекундах. `None` -
    /// на период такта прогона (умолчание 1 мс).
    ///
    /// Поле необязательно, поэтому все существующие сценарии читаются без правки: время
    /// появляется только там, где о нём попросили.
    pub time_ms: Option<i64>,
    pub inout: Option<PortValues>,
    /// Стенд внешних функций на этот такт.
    ///
    /// Две формы на функцию: одно значение (`"mem_read": 42`) либо таблица по
    /// **первому** аргументу (`"mem_read": {"0": 9, "1": 8}`) - так задаётся
    /// "память", из которой читает модель.
    ///
    /// Поле необязательно, и умолчания у него нет: не задали - вызов `extern fn` со
    /// значением отвечает прежним `SIM-019`. Ноль по умолчанию сделал бы прогон зелёным
    /// там, где эталон и прошивка расходятся.
    #[serde(rename = "extern")]
    pub extern_stubs: Option<BTreeMap<String, ExternValue>>,
    pub guard: Option<Guard>,
}

/// Чем сценарий подменяет одну внешнюю функцию.
#[derive(Deserialize, Clone)]
#[serde(untagged)]
pub enum ExternValue {
    /// Таблица "первый аргумент -> значение". Ключ - строка: в JSON ключом объекта
    /// иначе не бывает.
    ByArgument(BTreeMap<String, serde_json::Value>),
    /// Одно значение на любой вызов в этом такте.
    Any(serde_json::Value),
}

/// Проверка состояния модели после шага.
#[derive(Deserialize, Clone)]
pub struct Guard {
    pub out: Option<PortValues>,
    pub inout: Option<PortValues>,
    pub vars: Option<HashMap<String, serde_json::Value>>,
}

// -- Чтение файла -------------------------------------------------------------

/// Читает JSON-файл симуляции и возвращает вектор шагов.
pub fn load_sim_steps(path: &Path) -> Result<Vec<SimStep>, String> {
    let content = std::fs::read_to_string(path)
        .map_err(|e| format!("Ошибка чтения {}: {}", path.display(), e))?;
    serde_json::from_str(&content)
        .map_err(|e| format!("Ошибка парсинга JSON {}: {}", path.display(), e))
}

// -- Конвертация значений ------------------------------------------------------

/// Конвертирует JSON-значение в симуляционное Value.
pub fn json_to_value(v: &serde_json::Value) -> Option<Value> {
    match v {
        serde_json::Value::Bool(b) => Some(Value::Boolean(*b)),
        serde_json::Value::Number(n) => {
            if let Some(i) = n.as_i128() {
                Some(Value::Number(i))
            } else {
                n.as_f64().map(Value::Real)
            }
        }
        serde_json::Value::Array(arr) => {
            let items: Option<Vec<Value>> = arr.iter().map(json_to_value).collect();
            Some(Value::Array(items?))
        }
        serde_json::Value::Null => None,
        _ => None,
    }
}

// -- Тесты ---------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_json_to_value_bool_true() {
        let v = serde_json::Value::Bool(true);
        assert!(matches!(json_to_value(&v), Some(Value::Boolean(true))));
    }

    #[test]
    fn test_json_to_value_bool_false() {
        let v = serde_json::Value::Bool(false);
        assert!(matches!(json_to_value(&v), Some(Value::Boolean(false))));
    }

    #[test]
    fn test_json_to_value_integer() {
        let v = serde_json::json!(42i64);
        assert!(matches!(json_to_value(&v), Some(Value::Number(42))));
    }

    #[test]
    fn test_json_to_value_negative_integer() {
        let v = serde_json::json!(-7i64);
        assert!(matches!(json_to_value(&v), Some(Value::Number(-7))));
    }

    #[test]
    fn test_json_to_value_float() {
        let v = serde_json::json!(2.5f64);
        let result = json_to_value(&v);
        let Some(Value::Real(f)) = result else {
            panic!("ожидался Real");
        };
        assert!((f - 2.5).abs() < 1e-9);
    }

    #[test]
    fn test_json_to_value_null_is_none() {
        let v = serde_json::Value::Null;
        assert!(
            json_to_value(&v).is_none(),
            "null должен означать 'не проверять'"
        );
    }

    #[test]
    fn test_json_to_value_array() {
        let v = serde_json::json!([1, true, 2.5]);
        let Some(Value::Array(arr)) = json_to_value(&v) else {
            panic!("ожидался Array");
        };
        assert_eq!(arr.len(), 3);
        assert!(matches!(arr[0], Value::Number(1)));
        assert!(matches!(arr[1], Value::Boolean(true)));
    }

    #[test]
    fn test_json_to_value_string_is_none() {
        let v = serde_json::Value::String("x".to_string());
        assert!(json_to_value(&v).is_none());
    }

    #[test]
    fn test_load_sim_steps_valid_json() {
        use std::io::Write;
        let mut f = tempfile::NamedTempFile::new().unwrap();
        write!(f, r#"[{{}}, {{"in_ports": [1, 2]}}]"#).unwrap();
        let steps = load_sim_steps(f.path()).unwrap();
        assert_eq!(steps.len(), 2);
        assert!(steps[0].in_ports.is_none());
        match steps[1].in_ports.as_ref().expect("шаг 2 задаёт входы") {
            PortValues::Positional(list) => assert_eq!(list.len(), 2),
            PortValues::Named(_) => panic!("массив обязан разбираться позиционной формой"),
        }
    }

    /// Именованная форма разбирается объектом, а не позиционным массивом: формы не
    /// должны путаться местами.
    #[test]
    fn named_form_is_parsed_as_map() {
        use std::io::Write;
        let mut f = tempfile::NamedTempFile::new().unwrap();
        write!(f, r#"[{{"in_ports": {{"btn": 1}}}}]"#).unwrap();
        let steps = load_sim_steps(f.path()).unwrap();
        match steps[0].in_ports.as_ref().expect("входы заданы") {
            PortValues::Named(map) => {
                assert_eq!(map.len(), 1);
                assert!(map.contains_key("btn"));
            }
            PortValues::Positional(_) => panic!("объект обязан разбираться именованной формой"),
        }
    }

    #[test]
    fn test_load_sim_steps_invalid_json() {
        use std::io::Write;
        let mut f = tempfile::NamedTempFile::new().unwrap();
        write!(f, "not json").unwrap();
        assert!(load_sim_steps(f.path()).is_err());
    }

    #[test]
    fn test_load_sim_steps_missing_file() {
        let path = std::path::Path::new("/no/such/file.json");
        assert!(load_sim_steps(path).is_err());
    }
}
