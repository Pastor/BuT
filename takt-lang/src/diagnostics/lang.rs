//! Язык сообщений инструментов: носитель, каталог и подстановка.
//!
//! # Устройство
//!
//! - Каталоги - `takt-lang/messages/<код>.txt`, по строке на ключ; в код их
//!   вносит `build.rs` (там же строятся [`keys`] - константы ключей).
//! - **Список языков открыт**: третий язык - это
//!   третий файл, без правки кода. Отсюда [`Lang`] - код, проверенный по
//!   каталогам, а не перечисление вариантов.
//! - Порядок выбора: параметр API ([`activate`]) -> ключ `--lang` у `taktc` и
//!   `takt-sim` -> переменная `TAKT_LANG` -> [`BASE`] (`ru`).
//!
//! **Системная локаль не читается**: `LANG`/`LC_MESSAGES` на выбор не влияют. Вывод
//! инструмента не должен зависеть от машины - на этом стоят потактовые сверки и проверки
//! корпуса, а пересъём эталонов из-за чужого окружения был бы молчаливым.
//!
//! Носитель **потоковый**: язык, выбранный в одном потоке, в другой не переезжает. Для
//! CLI и моста этого достаточно (вход выбирает язык один раз), а многопоточному
//! потребителю язык передаётся параметром входа.

use std::cell::RefCell;
use std::fmt::Display;

include!(concat!(env!("OUT_DIR"), "/messages.rs"));

/// Базовый язык: его каталог полон по построению и служит запасным.
pub const BASE: &str = "ru";

/// Ключ сообщения. Значения - только константы [`keys`], построенные `build.rs`.
///
/// Обёртка над строкой существует ради одного: ключ, написанный литералом, ошибался бы
/// **молча** - сообщение просто не нашлось бы, а вместо текста поехал бы запасной.
/// Константа делает опечатку отказом компиляции.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Key(pub &'static str);

impl Key {
    /// Строковое имя ключа - для проверок и сообщений об ошибке.
    pub fn as_str(&self) -> &'static str {
        self.0
    }
}

/// Язык сообщений: код, для которого в дереве есть каталог.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Lang(&'static str);

impl Lang {
    /// Код языка (`ru`, `en`, ...).
    pub fn code(&self) -> &'static str {
        self.0
    }

    /// Базовый язык.
    pub fn base() -> Self {
        Self(BASE)
    }
}

impl Display for Lang {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0)
    }
}

impl Default for Lang {
    fn default() -> Self {
        Self::base()
    }
}

/// Все языки, у которых есть каталог, в порядке кода.
pub fn all() -> Vec<Lang> {
    LANGS.iter().map(|code| Lang(code)).collect()
}

/// Разбирает код языка; неизвестный - отказ с **перечислением** известных.
///
/// Перечисление обязательно: список открыт, и автор, промахнувшийся мимо `en`, иначе не
/// узнает, чем дерево располагает.
pub fn parse(code: &str) -> Result<Lang, String> {
    let wanted = code.trim().to_ascii_lowercase();
    if let Some(found) = LANGS.iter().find(|known| **known == wanted) {
        return Ok(Lang(found));
    }
    let known = LANGS.join(", ");
    // Текст отказа берётся из каталога базового языка: активный язык в этот момент ещё
    // не выбран - его как раз и не удалось разобрать.
    Err(render_in(
        Lang::base(),
        keys::LANG_UNKNOWN,
        &[("name", &code), ("known", &known)],
    ))
}

/// Изымает ключ `--lang <код>` (или `--lang=<код>`) из аргументов и активирует язык.
///
/// Живёт в библиотеке, а не в бинарнике, по той же причине, что и разбор подкоманд:
/// ключ общий для `taktc` и `takt-sim`, а вторая копия разбора разошлась бы с первой
/// молча.
///
/// Ключ изымается **до** разбора подкоманды и потому действует на все из них: язык -
/// свойство прогона, а не подкоманды. Отказ возвращается словами и перечисляет
/// известные языки; печатает его вызывающий (библиотека, пишущая в `stderr`, лишает
/// вызывающего выбора - ).
pub fn take_flag(args: &mut Vec<String>) -> Result<(), String> {
    let mut chosen = None;
    let mut rest = Vec::with_capacity(args.len());
    let mut iter = std::mem::take(args).into_iter();
    while let Some(arg) = iter.next() {
        if let Some(code) = arg.strip_prefix("--lang=") {
            chosen = Some(code.to_string());
        } else if arg == "--lang" {
            let Some(code) = iter.next() else {
                // Значения нет - значит `--lang` шёл последним, и в `iter` ничего не
                // осталось: аргументы вызывающего целы, отдаём их обратно и только
                // потом отказываем.
                *args = rest;
                return Err(render(keys::LANG_MISSING_VALUE, &[]));
            };
            chosen = Some(code);
        } else {
            rest.push(arg);
        }
    }
    *args = rest;
    if let Some(code) = chosen {
        activate(parse(&code)?);
    }
    Ok(())
}

thread_local! {
    /// Активный язык потока. `None` - "выбор не делался, спросить окружение".
    static ACTIVE: RefCell<Option<Lang>> = const { RefCell::new(None) };
}

/// Делает язык активным в текущем потоке.
pub fn activate(lang: Lang) {
    ACTIVE.with(|cell| *cell.borrow_mut() = Some(lang));
}

/// Возвращает поток к умолчанию (окружение, затем базовый). Для тестов.
pub fn reset() {
    ACTIVE.with(|cell| *cell.borrow_mut() = None);
}

/// Активный язык: явный выбор -> `TAKT_LANG` -> [`BASE`].
///
/// Негодное значение `TAKT_LANG` **не отказ**: переменная окружения не место разговора
/// с автором, и падать из-за неё посреди сборки хуже, чем ответить по-русски. Отказ с
/// перечислением даёт ключ `--lang`, где промах виден автору.
pub fn current() -> Lang {
    if let Some(chosen) = ACTIVE.with(|cell| *cell.borrow()) {
        return chosen;
    }
    std::env::var("TAKT_LANG")
        .ok()
        .and_then(|code| parse(&code).ok())
        .unwrap_or_else(Lang::base)
}

/// Текст ключа на активном языке с подстановкой `{имя}`.
///
/// Значения приходят через [`Display`], а не [`std::fmt::Debug`]: дамп узла в тексте
/// диагностики - известный класс, и здесь он невозможен по построению.
pub fn render(key: Key, params: &[(&str, &dyn Display)]) -> String {
    render_in(current(), key, params)
}

/// Тот же текст, но на названном языке - для тестов и для отказа разбора языка.
pub fn render_in(lang: Lang, key: Key, params: &[(&str, &dyn Display)]) -> String {
    let template = lookup(lang, key)
        // Запасной путь: ключ есть в базе, но не в каталоге языка. При зелёном проверке
        // паритета случай недостижим - потому здесь и стоит база, а не паника:
        // сломанный перевод не повод не сообщить о дефекте модели.
        .or_else(|| lookup(Lang::base(), key))
        .unwrap_or(key.0);
    substitute(template, params)
}

/// Ищет ключ в каталоге языка.
fn lookup(lang: Lang, key: Key) -> Option<&'static str> {
    let entries = CATALOGUES
        .iter()
        .find(|(code, _)| *code == lang.0)
        .map(|(_, entries)| *entries)?;
    entries
        .binary_search_by(|(k, _)| (*k).cmp(key.0))
        .ok()
        .map(|idx| entries[idx].1)
}

/// Подставляет `{имя}` значениями; неизвестное имя остаётся как написано.
///
/// Оставить неизвестное имя - намеренно: проверка паритета сверяет наборы имён и ловит
/// расхождение списком, а сообщение с видимым `{name}` читается как дефект каталога,
/// тогда как пустота выглядела бы дефектом модели.
///
/// Удвоенная скобка - одна скобка, как у `format!`: сообщению бывают нужны фигурные
/// скобки (`always { x := f(); }`), и шаблон пишет их `{{` и `}}`, а имя внутри
/// подставляется как обычно.
fn substitute(template: &str, params: &[(&str, &dyn Display)]) -> String {
    if !template.contains(['{', '}']) {
        return template.to_string();
    }
    let mut out = String::with_capacity(template.len());
    let mut rest = template;
    while let Some(at) = rest.find(['{', '}']) {
        out.push_str(&rest[..at]);
        let tail = &rest[at..];
        if tail.starts_with("{{") || tail.starts_with("}}") {
            out.push_str(&tail[..1]);
            rest = &tail[2..];
            continue;
        }
        if let Some(after) = tail.strip_prefix('}') {
            out.push('}');
            rest = after;
            continue;
        }
        let inner = &tail[1..];
        let Some(close) = inner.find('}') else {
            out.push('{');
            rest = inner;
            continue;
        };
        let name = &inner[..close];
        match params.iter().find(|(n, _)| *n == name) {
            Some((_, value)) => out.push_str(&value.to_string()),
            None => {
                out.push('{');
                out.push_str(name);
                out.push('}');
            }
        }
        rest = &inner[close + 1..];
    }
    out.push_str(rest);
    out
}

/// Текст ключа на активном языке с именованными подстановками.
///
/// ```ignore
/// msg!(keys::SE_034_LOCAL_TYPE_NOT_FOUND, name = def.name)
/// ```
#[macro_export]
macro_rules! msg {
    ($key:expr $(, $name:ident = $value:expr)* $(,)?) => {
        $crate::diagnostics::lang::render(
            $key,
            &[$((stringify!($name), &$value as &dyn ::std::fmt::Display)),*],
        )
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Дерево несёт базовый каталог и английский; список строится из файлов.
    #[test]
    fn catalogues_are_discovered_from_files() {
        let codes: Vec<&str> = all().iter().map(|l| l.code()).collect();
        assert!(codes.contains(&"ru"), "{codes:?}");
        assert!(codes.contains(&"en"), "{codes:?}");
    }

    /// Удвоенная скобка - одна скобка, и подстановка внутри неё работает: так
    /// печатались сообщения с примером кода, пока текст жил в `format!`.
    #[test]
    fn doubled_braces_are_literal_braces() {
        let name = "x";
        let func = "now";
        let out = substitute(
            "'always {{ {name} := {func}(); }}'",
            &[("name", &name as &dyn Display), ("func", &func)],
        );
        assert_eq!(out, "'always { x := now(); }'");
        assert_eq!(
            substitute("('start Имя {{ … }}')", &[]),
            "('start Имя { … }')"
        );
        assert_eq!(substitute("{unknown} и {{", &[]), "{unknown} и {");
    }

    /// Неизвестный язык - отказ, и он перечисляет известные.
    #[test]
    fn unknown_language_is_refused_with_the_known_ones_listed() {
        let err = parse("de").expect_err("язык 'de' в дереве отсутствует");
        assert!(err.contains("de"), "{err}");
        assert!(err.contains("ru") && err.contains("en"), "{err}");
    }

    /// Регистр и пробелы кода языка значения не имеют.
    #[test]
    fn language_code_is_case_insensitive() {
        assert_eq!(parse(" EN ").unwrap().code(), "en");
    }

    /// Явный выбор сильнее окружения; `reset` возвращает поток к умолчанию.
    ///
    /// Половина про умолчание проверяется, только если `TAKT_LANG` не задана:
    /// переменная процесса общая для всех потоков, и тест, требующий её отсутствия, был
    /// бы флаки на машине, где она выставлена.
    #[test]
    fn explicit_choice_wins_over_environment() {
        reset();
        if std::env::var("TAKT_LANG").is_err() {
            assert_eq!(current().code(), BASE);
        }
        activate(parse("en").unwrap());
        assert_eq!(current().code(), "en");
        reset();
        if std::env::var("TAKT_LANG").is_err() {
            assert_eq!(current().code(), BASE);
        }
    }

    /// Ключ `--lang` изымается из аргументов в обеих формах и выбирает язык.
    #[test]
    fn flag_is_taken_out_of_arguments() {
        for form in [vec!["--lang", "en"], vec!["--lang=en"]] {
            reset();
            let mut args: Vec<String> = ["taktc", "compile"]
                .into_iter()
                .chain(form)
                .chain(["model.takt"])
                .map(str::to_string)
                .collect();
            take_flag(&mut args).expect("язык 'en' известен");
            assert_eq!(args, ["taktc", "compile", "model.takt"], "{args:?}");
            assert_eq!(current().code(), "en");
        }
        reset();
    }

    /// Без ключа аргументы не меняются и выбор не делается.
    #[test]
    fn absent_flag_changes_nothing() {
        let mut args: Vec<String> = ["taktc", "fmt", "--check"]
            .into_iter()
            .map(str::to_string)
            .collect();
        take_flag(&mut args).expect("ключа нет — отказывать не за что");
        assert_eq!(args, ["taktc", "fmt", "--check"]);
    }

    /// Неизвестный язык у ключа - отказ; аргументы вызывающего целы.
    #[test]
    fn unknown_language_in_flag_is_refused() {
        let mut args: Vec<String> = ["taktc", "--lang", "de", "compile"]
            .into_iter()
            .map(str::to_string)
            .collect();
        let err = take_flag(&mut args).expect_err("языка 'de' в дереве нет");
        assert!(err.contains("de") && err.contains("en"), "{err}");
    }

    /// `--lang` без значения - отказ словами, а не паника или тихий пропуск.
    #[test]
    fn flag_without_value_is_refused() {
        let mut args: Vec<String> = ["taktc", "compile", "--lang"]
            .into_iter()
            .map(str::to_string)
            .collect();
        let err = take_flag(&mut args).expect_err("значения нет");
        assert!(!err.is_empty(), "отказ обязан быть со словами");
        assert_eq!(args, ["taktc", "compile"], "аргументы вызывающего целы");
    }

    /// Системная локаль не читается - это решение, а не забывчивость.
    ///
    /// Тест текстовый: чтение `LANG`/`LC_MESSAGES` сделало бы вывод инструмента
    /// зависимым от машины, а на нём стоят потактовые сверки и проверки корпуса.
    /// Проверяется исходник самого носителя - иначе правило живёт только в док-строке.
    #[test]
    fn system_locale_is_not_consulted() {
        let source = include_str!("lang.rs");
        for name in ["\"LANG\"", "\"LC_MESSAGES\"", "\"LC_ALL\""] {
            assert!(
                !source.contains(&format!("var({name})")),
                "носитель читает {name} — вывод стал бы зависеть от машины"
            );
        }
    }

    /// Подстановка идёт по имени, а не по порядку.
    #[test]
    fn substitution_is_by_name() {
        let text = render_in(
            Lang::base(),
            keys::DIAG_COMPILE_ERROR,
            &[("message", &"нет типа"), ("code", &"SE-034")],
        );
        assert_eq!(text, "Ошибка компиляции [SE-034]: нет типа");
    }

    /// Неизвестное имя подстановки остаётся видимым - это дефект каталога.
    #[test]
    fn unknown_placeholder_stays_visible() {
        let text = render_in(Lang::base(), keys::LANG_UNKNOWN, &[("name", &"de")]);
        assert!(text.contains("{known}"), "{text}");
    }

    /// Английский каталог отдаёт английский текст того же ключа.
    #[test]
    fn english_catalogue_answers_in_english() {
        let text = render_in(
            parse("en").unwrap(),
            keys::SE_034_LOCAL_TYPE_NOT_FOUND,
            &[("name", &"u12")],
        );
        assert_eq!(text, "local type 'u12' not found");
        assert!(!text.chars().any(|c| ('а'..='я').contains(&c)), "{text}");
    }

    /// Каталог без запрошенного языка отвечает базовым текстом, а не пустотой.
    #[test]
    fn missing_translation_falls_back_to_base() {
        let absent = Lang("xx");
        let text = render_in(
            absent,
            keys::SE_034_LOCAL_TYPE_NOT_FOUND,
            &[("name", &"u12")],
        );
        assert_eq!(text, "Локальный тип 'u12' не найден");
    }
}
