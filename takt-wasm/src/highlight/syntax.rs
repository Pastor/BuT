//! Описания целевых языков для подсветки.
//!
//! Описание - это **данные**, а не код: набор слов по ролям плюс правила комментариев,
//! строк и литералов. Разбор один на все языки ([`super::scan`]), и добавление языка
//! сюда не заводит второго разборщика.
//!
//! # Роли, а не цвета
//!
//! Роль - то же имя, что в легенде слоя LSP (`keyword`, `type`, `number`, ...), и цвет
//! ей назначает страница (`--tok-*` в `web/static/app.css`). Реестр ролей - тема
//! документа `book/takt.tmTheme`: блоки кода в PDF и вкладка цели в редакторе обязаны
//! красить одни И те же виды токенов, иначе документ и редактор разойдутся глазами.
//! Сверку держит тест страницы ("роли кода ↔ `takt.tmTheme`").

use takt_lang::generator::Language;
use takt_lang::generator::keywords;

/// Роль отрезка: имя из легенды слоя LSP.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Role {
    Keyword,
    Variable,
    Function,
    Type,
    /// Именованная константа: `TRUE`/`FALSE` у ST, `NULL` у C.
    Constant,
    String,
    Number,
    Comment,
    Operator,
}

impl Role {
    /// Все роли - для теста "каждая названа в легенде".
    pub const ALL: [Role; 9] = [
        Role::Keyword,
        Role::Variable,
        Role::Function,
        Role::Type,
        Role::Constant,
        Role::String,
        Role::Number,
        Role::Comment,
        Role::Operator,
    ];

    /// Имя роли в легенде `lsp::SEMANTIC_TOKEN_TYPES`.
    pub fn token_type(self) -> &'static str {
        match self {
            Role::Keyword => "keyword",
            Role::Variable => "variable",
            Role::Function => "function",
            Role::Type => "type",
            // Вариант перечисления - та же роль, что именованная константа целевого
            // языка: тема документа зовёт её `constant.language`.
            Role::Constant => "enumMember",
            Role::String => "string",
            Role::Number => "number",
            Role::Comment => "comment",
            Role::Operator => "operator",
        }
    }
}

/// Описание языка для разбора.
pub struct Syntax {
    /// Группы слов с ролью каждой группы.
    pub words: &'static [(&'static [&'static str], Role)],
    /// Идентификаторы языка регистронезависимы (Structured Text).
    pub case_insensitive: bool,
    /// Начала строчных комментариев.
    pub line_comments: &'static [&'static str],
    /// Пары блочных комментариев.
    pub block_comments: &'static [(&'static str, &'static str)],
    /// Кавычки строковых литералов.
    pub strings: &'static [char],
    /// Знак экранирования внутри строки.
    pub string_escape: Option<char>,
    /// Знаки операций - красятся одной группой подряд.
    pub operators: &'static str,
    /// Начало директивы: `#` у C, `@` у PlantUML.
    pub directive_prefix: Option<char>,
    /// Начало локации прямого доступа: `%` у Structured Text.
    pub location_prefix: Option<char>,
    /// `#` внутри литерала: `16#FF`, `T#100ms`, `USINT#12` (IEC 61131-3).
    pub hash_number: bool,
    /// Апостроф внутри литерала: `8'd12` (SystemVerilog).
    pub tick_number: bool,
}

impl Syntax {
    /// Роль слова, если оно есть в словаре.
    pub fn role_of(&self, word: &str) -> Option<Role> {
        for (group, role) in self.words {
            let hit = if self.case_insensitive {
                group.iter().any(|w| w.eq_ignore_ascii_case(word))
            } else {
                group.contains(&word)
            };
            if hit {
                return Some(*role);
            }
        }
        None
    }
}

/// Название языка и его описание; `None` - описания нет.
///
/// `Language` помечен `#[non_exhaustive]`, и ветви `_` здесь не избежать. Она отвечает
/// **отказом с названной причиной**, а не подсветкой наугад: покрась новый язык
/// правилами C - вкладка выглядела бы рабочей и врала бы. Сегодня ветвь недостижима, и
/// это проверяет тест `every_target_paints_its_output`.
pub fn of(language: Language) -> Option<(&'static str, &'static Syntax)> {
    match language {
        Language::C => Some(("C", &C)),
        Language::ST => Some(("Structured Text", &ST)),
        Language::Rust => Some(("Rust", &RUST)),
        Language::SV | Language::SvMmio => Some(("SystemVerilog", &SV)),
        Language::PlantUML => Some(("PlantUML", &PLANTUML)),
        _ => None,
    }
}

// -- C (цели `c`, `c-hal`) ----------------------------------------------------

/// Управляющие слова C.
///
/// Носителя в проекте нет: цель `c` имён не судит (в C нет плоского пространства имён
/// IEC и raw-идентификаторов Rust), поэтому список написан здесь. Граница названа в
/// заголовке [`super`]: пропущенное слово стоит цвета, а не валидности вывода.
const C_KEYWORDS: &[&str] = &[
    "alignas",
    "alignof",
    "auto",
    "break",
    "case",
    "const",
    "constexpr",
    "continue",
    "default",
    "do",
    "else",
    "enum",
    "extern",
    "for",
    "goto",
    "if",
    "inline",
    "register",
    "restrict",
    "return",
    "sizeof",
    "static",
    "static_assert",
    "struct",
    "switch",
    "typedef",
    "typeof",
    "union",
    "volatile",
    "while",
    "_Alignas",
    "_Alignof",
    "_Atomic",
    "_Generic",
    "_Noreturn",
    "_Static_assert",
    "_Thread_local",
];

/// Имена типов, которые печатает цель `c` (`generator/c/c_type.rs`).
const C_TYPES: &[&str] = &[
    "bool",
    "char",
    "double",
    "float",
    "int",
    "long",
    "short",
    "signed",
    "unsigned",
    "void",
    "_Bool",
    "size_t",
    "ptrdiff_t",
    "intptr_t",
    "uintptr_t",
    "int8_t",
    "int16_t",
    "int32_t",
    "int64_t",
    "uint8_t",
    "uint16_t",
    "uint32_t",
    "uint64_t",
];

const C_CONSTANTS: &[&str] = &["NULL", "true", "false"];

pub static C: Syntax = Syntax {
    words: &[
        (C_KEYWORDS, Role::Keyword),
        (C_TYPES, Role::Type),
        (C_CONSTANTS, Role::Constant),
    ],
    case_insensitive: false,
    line_comments: &["//"],
    block_comments: &[("/*", "*/")],
    strings: &['"', '\''],
    string_escape: Some('\\'),
    operators: "+-*/%=<>!&|^~?:;,.()[]{}",
    directive_prefix: Some('#'),
    location_prefix: None,
    hash_number: false,
    tick_number: false,
};

// -- Structured Text (цели `st`, `st-at`) -------------------------------------

/// Объявления POU и секций переменных.
///
/// Группы ST **сверяются** с `book/st.sublime-syntax` тестом
/// [`tests::st_dictionary_matches_the_book`]: два описания одного языка в одном
/// дереве разъехались бы молча, а именно этим описанием документ
/// красит вывод целей `st`/`st-at`.
const ST_POU: &[&str] = &[
    "FUNCTION_BLOCK",
    "END_FUNCTION_BLOCK",
    "FUNCTION",
    "END_FUNCTION",
    "PROGRAM",
    "END_PROGRAM",
    "CONFIGURATION",
    "END_CONFIGURATION",
    "RESOURCE",
    "END_RESOURCE",
    "TASK",
    "TYPE",
    "END_TYPE",
    "STRUCT",
    "END_STRUCT",
    "VAR_INPUT",
    "VAR_OUTPUT",
    "VAR_IN_OUT",
    "VAR_GLOBAL",
    "VAR_EXTERNAL",
    "VAR_TEMP",
    "VAR",
    "END_VAR",
    "CONSTANT",
    "RETAIN",
    "AT",
];

/// Управляющие конструкции IEC 61131-3.
const ST_CONTROL: &[&str] = &[
    "IF",
    "THEN",
    "ELSIF",
    "ELSE",
    "END_IF",
    "CASE",
    "OF",
    "END_CASE",
    "FOR",
    "TO",
    "BY",
    "DO",
    "END_FOR",
    "WHILE",
    "END_WHILE",
    "REPEAT",
    "UNTIL",
    "END_REPEAT",
    "EXIT",
    "CONTINUE",
    "RETURN",
];

/// Операторы-слова: в IEC булева алгебра записывается словами.
const ST_WORD_OPERATORS: &[&str] = &["AND", "OR", "XOR", "NOT", "MOD"];

/// Элементарные типы стандарта плюс те, что печатают цели `st`/`st-at`.
const ST_TYPES: &[&str] = &[
    "BOOL", "BYTE", "WORD", "DWORD", "LWORD", "SINT", "USINT", "INT", "UINT", "DINT", "UDINT",
    "LINT", "ULINT", "REAL", "LREAL", "TIME", "DATE", "STRING", "WSTRING", "ARRAY", "POINTER",
];

const ST_CONSTANTS: &[&str] = &["TRUE", "FALSE"];

pub static ST: Syntax = Syntax {
    words: &[
        (ST_POU, Role::Type),
        (ST_CONTROL, Role::Keyword),
        (ST_WORD_OPERATORS, Role::Operator),
        (ST_TYPES, Role::Type),
        (ST_CONSTANTS, Role::Constant),
    ],
    // Идентификаторы IEC регистронезависимы, и цель печатает ключевые слова
    // Верхним регистром; рукописный пример строчными обязан краситься так же.
    case_insensitive: true,
    line_comments: &["//"],
    block_comments: &[("(*", "*)")],
    strings: &['\'', '"'],
    // Экранирование IEC - `$`, а не обратная косая (`$'`, `$N`).
    string_escape: Some('$'),
    operators: "+-*/=<>&:;,.()[]",
    directive_prefix: None,
    location_prefix: Some('%'),
    hash_number: true,
    tick_number: false,
};

// -- Rust (цель `rust`) -------------------------------------------------------

/// Примитивные типы Rust - то, что печатает цель (`generator/rust/rust_type.rs`).
const RUST_TYPES: &[&str] = &[
    "bool", "char", "str", "f32", "f64", "i8", "i16", "i32", "i64", "i128", "isize", "u8", "u16",
    "u32", "u64", "u128", "usize", "Option", "Some", "None", "Result", "Ok", "Err",
];

pub static RUST: Syntax = Syntax {
    words: &[
        // Слова берутся у компилятора: тем же списком цель `rust` судит имена
        // (`RS-004`), и второй копии здесь нет.
        (keywords::RUST, Role::Keyword),
        (keywords::RUST_NOT_RAW, Role::Keyword),
        (RUST_TYPES, Role::Type),
    ],
    case_insensitive: false,
    line_comments: &["//"],
    block_comments: &[("/*", "*/")],
    strings: &['"'],
    string_escape: Some('\\'),
    operators: "+-*/%=<>!&|^~?:;,.()[]{}#",
    directive_prefix: None,
    location_prefix: None,
    hash_number: false,
    tick_number: false,
};

// -- SystemVerilog (цели `sv`, `sv-mmio`) -------------------------------------

pub static SV: Syntax = Syntax {
    // Тот же список, которым цель `sv` отказывает на имени-ключевом слове
    // (`SV-012`): 248 слов IEEE 1800-2017, и второй копии здесь нет.
    words: &[(keywords::SV, Role::Keyword)],
    case_insensitive: false,
    line_comments: &["//"],
    block_comments: &[("/*", "*/")],
    strings: &['"'],
    string_escape: Some('\\'),
    operators: "+-*/%=<>!&|^~?:;,.()[]{}@#",
    directive_prefix: Some('`'),
    location_prefix: None,
    hash_number: false,
    // Размерный литерал `8'd12` - одно число: апостроф в SV не кавычка.
    tick_number: true,
};

// -- PlantUML (цель `plantuml`) -----------------------------------------------

/// Слова диаграммы состояний PlantUML.
///
/// Носителя в проекте нет - как и у C. Набор покрывает то, что печатает цель
/// (`@startuml`, `title`, `state`, `[*]`, `-->`), плюс обиходные слова диаграммы
/// состояний: вкладку читает человек, и дописанная им строка тоже должна краситься.
const PLANTUML_WORDS: &[&str] = &[
    "state",
    "title",
    "note",
    "end",
    "as",
    "hide",
    "show",
    "skinparam",
    "left",
    "right",
    "of",
    "top",
    "bottom",
    "direction",
    "fork",
    "join",
    "choice",
    "history",
    "scale",
    "caption",
    "header",
    "footer",
    "legend",
    "newpage",
];

pub static PLANTUML: Syntax = Syntax {
    words: &[(PLANTUML_WORDS, Role::Keyword)],
    case_insensitive: false,
    // Строчный комментарий PlantUML - апостроф; блочный - `/' ... '/`.
    line_comments: &["'"],
    block_comments: &[("/'", "'/")],
    strings: &['"'],
    string_escape: None,
    operators: "-<>*[]{}:|",
    directive_prefix: Some('@'),
    location_prefix: None,
    hash_number: false,
    tick_number: false,
};

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeSet;

    /// Описание ST для документа - второе описание того же языка в дереве.
    ///
    /// Читается на сборке теста: разъедься словари, документ и вкладка цели
    /// покрасили бы вывод `st` по-разному, и заметил бы это только глаз.
    const BOOK_ST: &str = include_str!("../../../book/st.sublime-syntax");

    /// Достаёт слова группы по имени области видимости `.sublime-syntax`.
    fn book_group(scope: &str) -> BTreeSet<String> {
        let mut out = BTreeSet::new();
        let mut previous: Option<&str> = None;
        for line in BOOK_ST.lines() {
            let line = line.trim();
            if line.strip_prefix("scope: ").map(str::trim) == Some(scope) {
                let source = previous.expect("у области видимости есть правило match");
                // Слова стоят между последней открывающей скобкой и первой
                // закрывающей за ней: запись правила - `(?i:\b(A|B)\b)`, и
                // наружные скобки к словам не относятся.
                let start = source.rfind('(').expect("группа слов в скобках");
                let end = source[start..].find(')').expect("группа слов закрыта") + start;
                for word in source[start + 1..end].split('|') {
                    let word = word.trim();
                    assert!(
                        !word.is_empty()
                            && word
                                .chars()
                                .all(|c| c.is_ascii_uppercase() || c == '_' || c.is_ascii_digit()),
                        "в группе {scope} слово '{word}' не похоже на ключевое: \
                         разбор правила разошёлся с записью"
                    );
                    out.insert(word.to_string());
                }
            }
            if line.starts_with("- match:") {
                previous = Some(line);
            }
        }
        assert!(
            !out.is_empty(),
            "в book/st.sublime-syntax нет группы {scope}"
        );
        out
    }

    fn module_group(group: &[&str]) -> BTreeSet<String> {
        group.iter().map(|w| w.to_string()).collect()
    }

    #[test]
    fn st_dictionary_matches_the_book() {
        // Соответствие "группа модуля ↔ область видимости документа" - предмет
        // проверки: имя области задаёт роль, а роль задаёт цвет и в PDF, и на
        // странице.
        for (group, scope) in [
            (ST_POU, "storage.type.st"),
            (ST_CONTROL, "keyword.control.st"),
            (ST_WORD_OPERATORS, "keyword.operator.word.st"),
            (ST_TYPES, "storage.type.primitive.st"),
            (ST_CONSTANTS, "constant.language.st"),
        ] {
            assert_eq!(
                module_group(group),
                book_group(scope),
                "словарь ST разошёлся с book/st.sublime-syntax в группе {scope}"
            );
        }
    }

    #[test]
    fn word_lookup_respects_case_policy() {
        assert_eq!(ST.role_of("if"), Some(Role::Keyword));
        assert_eq!(ST.role_of("IF"), Some(Role::Keyword));
        // Rust регистр различает: `If` - это имя, а не ключевое слово.
        assert_eq!(RUST.role_of("if"), Some(Role::Keyword));
        assert_eq!(RUST.role_of("If"), None);
    }

    #[test]
    fn rust_and_sv_take_words_from_the_compiler() {
        // Предмет проверки - что здесь не появилось второго списка: словарь
        // подсветки обязан быть тем же, которым цель судит имена (`RS-004`,
        // `SV-012`). Сверка идёт по содержимому, а не по адресу: адрес
        // статики в двух крейтах компоновщик вправе размножить (замер
        // 2026-09-04 - два разных адреса у одного списка), и равенство
        // указателей доказывало бы не то.
        assert_eq!(RUST.words[0].0, keywords::RUST);
        assert_eq!(RUST.words[1].0, keywords::RUST_NOT_RAW);
        assert_eq!(SV.words[0].0, keywords::SV);
        // Списки не выродились: пустой словарь прошёл бы равенство и не
        // покрасил бы ничего.
        assert!(keywords::SV.len() > 200, "слов SV: {}", keywords::SV.len());
        assert!(
            keywords::RUST.len() > 40,
            "слов Rust: {}",
            keywords::RUST.len()
        );
    }
}
