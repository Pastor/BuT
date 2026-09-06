//! Уровни доступа к проекту.
//!
//! # Один носитель на все ручки
//!
//! "Что этот человек может с этим проектом" - вопрос, который задают девять
//! обработчиков. Пока каждый отвечал сам, ответов было два - владелец и никто. С
//! выданными правами вариантов стало пять, и второй носитель разошёлся бы с первым
//! молча: расхождение здесь проявляется не отказом, а **чужой записью в чужой проект**.
//!
//! # Лестница
//!
//! | Уровень | Может | Не может |
//! |---|---|---|
//! | `none` | ничего; проекта для него **не существует** (`404`) | - |
//! | `view` | читать проект и файлы, брать снимок | сохранять, копировать себе |
//! | `fork` | всё `view` + копировать себе | править |
//! | `edit` | всё `fork` + писать и удалять файлы | менять видимость, права, имя, версию; удалять проект |
//! | владелец | всё | - |
//!
//! Уровни **упорядочены**, и проверка всюду одна: "не ниже требуемого". Перечисление
//! операций списком (`может ли `edit` удалить файл") развело бы таблицу и код.
//!
//! # Откуда берётся уровень
//!
//! Наивысший из применимых: выданное право, видимость, владение.
//!
//! - **`public` даёт `fork`, а не `view`** (проработка §4): текст уже в
//!   браузере читателя, и запрет копировать неисполним - право, которое нельзя
//!   обеспечить, не право.
//! - **`link` даёт `view`.** Проработка о нём не высказалась; выбрано
//!   отестное: `link` - это "показать человеку", а не "выложить", и копия в
//!   чужом списке проектов - уже другое действие. Решение обратимо и названо
//!   здесь: вправе поднять его до `fork` одной строкой.
//! - **Выданное право сильнее видимости в обе стороны**: `edit` на закрытом
//!   проекте работает, а `view` на открытом **не понижает** его до `view` -
//!   берётся наивысший, иначе выдача права выглядела бы наказанием.

use serde::{Deserialize, Serialize};

/// Что спрашивающий может с проектом.
///
/// Порядок вариантов - часть смысла: сравнение уровней и есть проверка права. Переставь
/// их - и `view` станет сильнее `edit`, а компилятор промолчит.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Level {
    /// Проекта для него не существует.
    None,
    /// Только чтение.
    View,
    /// Чтение и копирование себе.
    Fork,
    /// Чтение, копирование и правка файлов.
    Edit,
    /// Владелец: сверх `edit` - метаданные, права и удаление.
    Owner,
}

impl Level {
    /// Имя уровня в API.
    pub fn as_str(self) -> &'static str {
        match self {
            Self::None => "none",
            Self::View => "view",
            Self::Fork => "fork",
            Self::Edit => "edit",
            Self::Owner => "owner",
        }
    }

    /// Разбирает **выдаваемый** уровень; `None` - такого выдать нельзя.
    ///
    /// `owner` и `none` не выдаются: владелец один на проект (передача владения - не
    /// право, а другое действие), а "выдать none" - это удалить право, и для него есть
    /// `DELETE`.
    pub fn grantable(text: &str) -> Option<Self> {
        match text {
            "view" => Some(Self::View),
            "fork" => Some(Self::Fork),
            "edit" => Some(Self::Edit),
            _ => None,
        }
    }
}

/// Считает уровень спрашивающего.
///
/// # Параметры
/// - `owner` - владеет ли спрашивающий проектом;
/// - `visibility` - видимость проекта;
/// - `granted` - выданное ему право, если есть.
pub fn effective(owner: bool, visibility: &str, granted: Option<Level>) -> Level {
    if owner {
        return Level::Owner;
    }
    let by_visibility = match visibility {
        // Текст открытого проекта уже в браузере читателя: запрет копировать
        // неисполним, а право, которое нельзя обеспечить, не право.
        "public" => Level::Fork,
        // Показан человеку, а не выложен: копия в чужом списке - другое действие
        // (решение d, обратимо).
        "link" => Level::View,
        _ => Level::None,
    };
    by_visibility.max(granted.unwrap_or(Level::None))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_ladder_is_ordered() {
        // Проверка права - это сравнение уровней, и порядок вариантов есть само
        // правило. Переставь их - `view` станет сильнее `edit`, вывод соберётся, а
        // чужой человек получит запись.
        assert!(Level::None < Level::View);
        assert!(Level::View < Level::Fork);
        assert!(Level::Fork < Level::Edit);
        assert!(Level::Edit < Level::Owner);
    }

    #[test]
    fn visibility_gives_what_it_can_enforce() {
        assert_eq!(effective(false, "private", None), Level::None);
        assert_eq!(effective(false, "link", None), Level::View);
        // Открытый даёт `fork`: текст уже у читателя.
        assert_eq!(effective(false, "public", None), Level::Fork);
        assert_eq!(effective(true, "private", None), Level::Owner);
    }

    #[test]
    fn a_grant_never_lowers_what_visibility_already_gave() {
        // Берётся наивысший: иначе выдача `view` на открытом проекте отняла бы у
        // человека копирование, то есть выглядела бы наказанием.
        assert_eq!(effective(false, "public", Some(Level::View)), Level::Fork);
        assert_eq!(effective(false, "public", Some(Level::Edit)), Level::Edit);
        // И поднимает на закрытом.
        assert_eq!(effective(false, "private", Some(Level::Edit)), Level::Edit);
        assert_eq!(effective(false, "private", Some(Level::View)), Level::View);
        // Владение сильнее всего: своё право себе не выдают.
        assert_eq!(effective(true, "public", Some(Level::View)), Level::Owner);
    }

    #[test]
    fn only_three_levels_are_grantable() {
        assert_eq!(Level::grantable("view"), Some(Level::View));
        assert_eq!(Level::grantable("fork"), Some(Level::Fork));
        assert_eq!(Level::grantable("edit"), Some(Level::Edit));
        // Владелец один на проект: передача владения - не право, а другое действие;
        // "выдать none" - это удаление права, и для него есть DELETE.
        assert_eq!(Level::grantable("owner"), None);
        assert_eq!(Level::grantable("none"), None);
        assert_eq!(Level::grantable("EDIT"), None, "регистр не угадываем");
    }
}
