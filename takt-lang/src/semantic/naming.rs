/// Нормализует имя файла или идентификатора в CamelCase.
///
/// Преобразует `my_model`, `mein-leib`, `Mein_Leib` -> `MyModel`, `MeinLeib`.
/// Небуквенно-цифровые символы (`_`, `-`, `#` и т.д.) используются как разделители
/// слов.
///
/// **Регистр приводится по Unicode, а не по ASCII**. Прежний `to_ascii_uppercase`
/// не-ASCII букву оставлял как есть, и `out кнопка` давало цели `rust` вариант
/// перечисления `кнопка`, который `clippy -D warnings` отвергает: "variant should have
/// an upper camel case name". Имена языка не-ASCII быть могут, то есть правило регистра
/// обязано работать на том же алфавите, что и сам язык.
///
/// Слипание имён после приведения регистра (`кнопка` и `Кнопка` -> одно `Кнопка`) ловит
/// существующая диагностика `RS-005` - тот же механизм, что для ASCII
/// (`button`/`Button`); нового класса коллизий правка не заводит.
pub fn normalize_camelcase_name(name: &str) -> String {
    let mut result = String::new();
    let mut upper = true;
    for ch in name.chars() {
        if ch.is_alphabetic() && upper {
            result.extend(ch.to_uppercase());
        } else if !ch.is_alphanumeric() {
            upper = true;
            continue;
        } else {
            result.push(ch);
        }
        upper = false;
    }
    result
}

/// Нормализует уникальное имя (с разделителями `:`): заменяет `:` на `_`, затем
/// применяет `normalize_lowercase_snakecase`.
#[allow(dead_code)]
pub fn normalize_unique_name(name: &str) -> String {
    normalize_lowercase_snakecase(name.replace(":", "_"))
}

pub fn normalize_lowercase_snakecase(name: String) -> String {
    let mut result = String::new();
    let mut prev_was_lower = false;
    for ch in name.chars() {
        if ch.is_alphabetic() && ch.is_uppercase() {
            if prev_was_lower {
                result.push('_');
            }
            result.push(ch.to_ascii_lowercase());
            prev_was_lower = false;
        } else {
            result.push(ch);
            prev_was_lower = ch.is_alphabetic() && ch.is_lowercase();
        }
    }
    result
}

#[cfg(test)]
mod tests {
    const NAMES: &[(&str, &str)] = &[
        ("mein_leib", "MeinLeib"),
        ("mein-leib", "MeinLeib"),
        ("Mein_Leib", "MeinLeib"),
        ("mein_Leib", "MeinLeib"),
        ("Mein#Leib", "MeinLeib"),
    ];

    #[test]
    fn normalize_model_name() {
        use super::normalize_camelcase_name;
        for (name, expected) in NAMES {
            let normalized = normalize_camelcase_name(name);
            assert_eq!(&normalized, expected);
        }
    }

    // -- Дополнительные тесты нормализации имён --------------------------------

    /// Пустая строка остаётся пустой.
    #[test]
    fn normalize_model_name_empty() {
        use super::normalize_camelcase_name;
        assert_eq!(normalize_camelcase_name(""), "");
    }

    /// Строка из одних цифр не изменяется.
    #[test]
    fn normalize_model_name_digits_only() {
        use super::normalize_camelcase_name;
        assert_eq!(normalize_camelcase_name("123"), "123");
    }

    /// Одно слово: первая буква становится заглавной.
    #[test]
    fn normalize_model_name_single_word() {
        use super::normalize_camelcase_name;
        assert_eq!(normalize_camelcase_name("hello"), "Hello");
    }

    /// Не-ASCII буква тоже поднимается в верхний регистр.
    ///
    /// Прежний `to_ascii_uppercase` оставлял её как есть, и цель `rust` печатала
    /// вариант перечисления `кнопка` - `clippy -D warnings` отвечает "variant should
    /// have an upper camel case name". Имена языка не-ASCII быть могут, значит и
    /// правило регистра обязано работать на том же алфавите.
    #[test]
    fn normalize_model_name_non_ascii() {
        use super::normalize_camelcase_name;
        assert_eq!(normalize_camelcase_name("кнопка"), "Кнопка");
        assert_eq!(normalize_camelcase_name("кнопка_пуска"), "КнопкаПуска");
        assert_eq!(normalize_camelcase_name("Кнопка"), "Кнопка");
        // Смешанный алфавит: правило одно на оба.
        assert_eq!(normalize_camelcase_name("пуск_button"), "ПускButton");
    }

    // -- Тесты normalize_lowercase_snakecase -----------------------------------

    /// CamelCase -> snake_case: граница нижний->верхний регистр.
    #[test]
    fn snakecase_camel_case() {
        use super::normalize_lowercase_snakecase;
        assert_eq!(
            normalize_lowercase_snakecase("MyModel".to_string()),
            "my_model"
        );
        assert_eq!(
            normalize_lowercase_snakecase("ThisIsMyModel".to_string()),
            "this_is_my_model"
        );
        assert_eq!(
            normalize_lowercase_snakecase("isReady".to_string()),
            "is_ready"
        );
    }

    /// ALL_CAPS-имя (уже разделено `_`) не разбивается на символы.
    #[test]
    fn snakecase_all_caps_with_underscore() {
        use super::normalize_lowercase_snakecase;
        assert_eq!(
            normalize_lowercase_snakecase("IS_EMPTY".to_string()),
            "is_empty"
        );
        assert_eq!(
            normalize_lowercase_snakecase("AT_FLOOR".to_string()),
            "at_floor"
        );
    }

    /// Сплошные заглавные без разделителя не разбиваются посимвольно.
    #[test]
    fn snakecase_solid_all_caps() {
        use super::normalize_lowercase_snakecase;
        assert_eq!(
            normalize_lowercase_snakecase("MATRIX".to_string()),
            "matrix"
        );
        assert_eq!(normalize_lowercase_snakecase("NUMB".to_string()), "numb");
    }

    /// Цифры не вызывают вставку `_`.
    #[test]
    fn snakecase_with_digits() {
        use super::normalize_lowercase_snakecase;
        assert_eq!(normalize_lowercase_snakecase("B1".to_string()), "b1");
        assert_eq!(
            normalize_lowercase_snakecase("sensors1".to_string()),
            "sensors1"
        );
    }

    /// Пустая строка остаётся пустой.
    #[test]
    fn snakecase_empty() {
        use super::normalize_lowercase_snakecase;
        assert_eq!(normalize_lowercase_snakecase(String::new()), "");
    }
}
