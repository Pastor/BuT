//! Терминальные токены грамматики Takt ([`Token`]).
//!
//! Вынесено из `lexer.rs`: чистое перемещение. `Token` - выход лексера и вход парсера
//! (lalrpop `extern { enum Token }`); путь `parser::lexer::Token` сохранён реэкспортом
//! в `lexer.rs`.

use std::fmt;

/// Все лексемы (токены) языка Takt.
///
/// Каждый вариант соответствует одному терминальному символу грамматики.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
#[allow(missing_docs)]
pub enum Token<'input> {
    /// Произвольный идентификатор, не являющийся ключевым словом.
    Identifier(&'input str),
    /// Строковый литерал: `(unicode, содержимое)`.
    StringLiteral(bool, &'input str),
    /// Адресный литерал (числовой адрес порта).
    AddressLiteral(&'input str),
    /// Целочисленный литерал.
    ///
    /// Носитель - `i128`, а не `i64`: язык объявляет тип `u64` (и `[bit;64]` как
    /// упакованный скаляр, 0078), верхняя половина диапазона которого в знаковый
    /// 64-битный носитель не влезает - маску "все единицы" записать было нельзя вовсе.
    /// Принимаемый диапазон - `[i64::MIN, u64::MAX]` (см.
    /// `LexicalError::NumberOutOfRange`); `i128` шире него намеренно, чтобы
    /// промежуточный результат константного вычисления не переполнялся там, где итог по
    /// типу законен.
    Number(i128),
    /// Рациональный (плавающий) литерал: `(строка, отрицательный)`.
    RationalNumber(&'input str, bool),
    /// Литерал длительности: `(наносекунды, как записано)` -.
    ///
    /// Исходный текст нужен форматтеру: `1m30s` не канонизируется в `90s` (тот же
    /// приём, что у [`RationalNumber`](Token::RationalNumber)).
    Duration(i64, &'input str),
    /// Литерал частоты: `(герцы, как записано)` -.
    Frequency(u64, &'input str),
    /// Литерал выдержки в **тактах**: `(число тактов, как записано)` - `3t`.
    ///
    /// Отдельный токен, а не длительность: такт в наносекундах не выражается без
    /// частоты, и тактовая выдержка её не требует.
    Ticks(i64, &'input str),
    /// Оператор деления `/`.
    Divide,
    /// Ключевое слово `fn`.
    Function,
    /// Ключевое слово `import`.
    Import,
    /// Ключевое слово `type`.
    Type,
    /// Ключевое слово `loop`.
    Loop,
    /// Ключевое слово `while`.
    While,
    /// Ключевое слово `continue`.
    Continue,
    /// Ключевое слово `break`.
    Break,
    /// Ключевое слово `return`.
    Return,
    /// Символ `#`.
    Sharp,
    /// Символ `;`.
    Semicolon,
    /// Символ `,`.
    Comma,
    /// Символ `(`.
    OpenParenthesis,
    /// Символ `)`.
    CloseParenthesis,
    /// Символ `{`.
    OpenCurlyBrace,
    /// Символ `}`.
    CloseCurlyBrace,

    /// Оператор побитового или `|`.
    BitwiseOr,
    /// Оператор побитового исключающего или `^`.
    BitwiseXor,
    /// Логический оператор или `||`.
    Or,

    /// Оператор побитового И `&`.
    BitwiseAnd,
    /// Оператор побитового не `~`.
    BitwiseNot,
    /// Логический оператор И `&&`.
    And,
    /// Оператор сложения `+`.
    Add,
    /// Оператор вычитания `-`.
    Subtract,
    /// Оператор умножения `*`.
    Mul,
    /// Оператор возведения в степень `**`.
    Power,
    /// Оператор взятия остатка `%`.
    Modulo,

    /// Оператор `==` - **выведен** из языка: равенство пишется одним `=`. В грамматике
    /// терминала нет, употребление даёт `SY-002`.
    ///
    /// **Лексема сохранена намеренно, терминал - нет**. Пока `==`
    /// распознаётся лексером, диагностика называет его целиком: "нераспознанный
    /// токен `==`". Изъятие лексемы дало бы разбор на `=` и `=` - сообщение о
    /// следствии вместо причины.
    ///
    /// Прежний комментарий обещал "понятную диагностику „использовать `=`"" - замер
    /// 0201 это **не подтвердил**: текст диагностики от объявления терминала не зависел
    /// вовсе (байт-в-байт тот же), а само объявление добавляло вторую, **каскадную**
    /// ошибку. Поэтому терминал изъят.
    Equal,
    /// Оператор `=` - **сравнение на равенство** в выражениях и условиях; в декларациях
    /// (`type`/`enum`/`model`/`cond`) - определение имени. Присваивание значения -
    /// отдельный токен [`Token::ColonAssign`].
    Assign,
    /// Оператор `:=` - **присваивание/инициализация значения**: в выражениях
    /// (`Expression::Assign`) и инициализаторах `var`/`const`/`in`/`out`/`inout`.
    ColonAssign,

    /// Оператор неравенства `!=`.
    NotEqual,
    /// Логическое не `!`.
    Not,

    /// Логическое значение `true`.
    True,
    /// Логическое значение `false`.
    False,
    /// Ключевое слово `else`.
    Else,
    /// Ключевое слово `for`.
    For,
    /// Ключевое слово `if`.
    If,
    /// Ключевое слово `match`.
    Match,
    /// Подстановочный образец `_` в ветке `match`.
    Wildcard,
    /// Толстая стрелка `=>` (разделитель образца и тела в `match`).
    FatArrow,

    /// Оператор сдвига вправо `>>`.
    ShiftRight,
    /// Оператор "меньше" `<`.
    Less,
    /// Оператор "меньше или равно" `<=`.
    LessEqual,

    /// Оператор сдвига влево `<<`.
    ShiftLeft,
    /// Оператор "больше" `>`.
    More,
    /// Оператор "больше или равно" `>=`.
    MoreEqual,

    /// Оператор доступа к члену `.`.
    Member,
    /// Двоеточие `:`.
    Colon,
    /// Символ вопроса `?` (тернарный оператор).
    Question,
    /// Символ `[`.
    OpenBracket,
    /// Символ `]`.
    CloseBracket,

    /// Ключевое слово `as` (приведение типов).
    As,

    /// Ключевое слово `assembly`.
    Assembly,
    /// Ключевое слово `formula`.
    Formula,

    /// Ключевое слово `const`.
    Constant,
    /// Ключевое слово `parameter` (параметр модели).
    ///
    /// Третья форма объявления наряду с `var` и `const`: значение задаётся в месте
    /// инстанцирования модели, а объявление несёт значение по умолчанию.
    Parameter,
    /// Ключевое слово `in` (входной порт).
    PortIn,
    /// Ключевое слово `out` (выходной порт).
    PortOut,
    /// Ключевое слово `inout` (двунаправленный порт).
    PortInOut,
    /// Ключевое слово `address` (оператор задания адреса порта).
    Address,
    /// Ключевое слово `at` - размещение порта в объявлении.
    ///
    /// Отделяет **адрес** от значения: `out led: bit at 0x40:2 := 0;`. Часть
    /// необязательная - адрес может прийти по имени порта (оператор `address` или
    /// внешняя карта), поэтому объявление без `at` законно.
    At,
    /// Ключевое слово `clock` (частота тактирования модели).
    Clock,
    /// Ключевое слово `after` (выдержка на ребре).
    After,
    /// Ключевое слово `every` (периодическое действие).
    Every,

    /// Стрелка `-->` - грамматикой не используется (LTL-импликация пишется `->`).
    /// Терминал изъят из extern-блока; лексема сохранена по той же причине, что у
    /// [`Token::Equal`](Token::Equal) - чтобы отказ назывался целиком, а не распадался
    /// на `-` и `->`.
    PeirceArrow,

    /// Ключевое слово `model`.
    Model,
    /// Ключевое слово `state`.
    State,
    /// Ключевое слово `start`.
    Start,
    /// Ключевое слово `ref`.
    Reference,
    /// Ключевое слово `cond`.
    Condition,
    /// Ключевое слово `invariant`.
    Invariant,
    /// Ключевое слово `var`.
    Variable,
    /// Ключевое слово `next`.
    Next,
    /// Ключевое слово `extern`.
    Extern,
    /// Ключевое слово `enum`.
    Enum,
    /// Ключевое слово `struct`.
    Struct,
    /// Ключевое слово `from`.
    From,
    /// LTL оператор Next (X)
    LtlNext,
    /// LTL оператор Finally (F)
    LtlFinally,
    /// LTL оператор Globally (G)
    LtlGlobally,
    /// LTL оператор Until (U)
    LtlUntil,
    /// LTL оператор Release (R)
    LtlRelease,
    /// Тип формулы LTL
    TypeLtl,
    /// Тип формулы Guard
    TypeGuard,
    /// Оператор `->`
    Arrow,
}

impl<'input> fmt::Display for Token<'input> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Identifier(id) => write!(f, "{id}"),
            Token::StringLiteral(false, s) => write!(f, "\"{s}\""),
            Token::StringLiteral(true, s) => write!(f, "unicode\"{s}\""),
            Token::AddressLiteral(address) => write!(f, "{address}"),
            Token::Number(n) => write!(f, "{n}"),
            Token::RationalNumber(n, d) => {
                if *d {
                    write!(f, "-")?;
                }
                write!(f, "{n}")
            }
            // печатается авторская запись (`1m30s`), а не канон.
            Token::Duration(_, text) => write!(f, "{text}"),
            Token::Frequency(_, text) => write!(f, "{text}"),
            Token::Ticks(_, text) => write!(f, "{text}"),
            Token::Semicolon => write!(f, ";"),
            Token::Comma => write!(f, ","),
            Token::Sharp => write!(f, "#"),
            Token::OpenParenthesis => write!(f, "("),
            Token::CloseParenthesis => write!(f, ")"),
            Token::OpenCurlyBrace => write!(f, "{{"),
            Token::CloseCurlyBrace => write!(f, "}}"),
            Token::BitwiseOr => write!(f, "|"),
            Token::BitwiseXor => write!(f, "^"),
            Token::Or => write!(f, "||"),
            Token::BitwiseAnd => write!(f, "&"),
            Token::BitwiseNot => write!(f, "~"),
            Token::And => write!(f, "&&"),
            Token::Add => write!(f, "+"),
            Token::Subtract => write!(f, "-"),
            Token::Mul => write!(f, "*"),
            Token::Power => write!(f, "**"),
            Token::Divide => write!(f, "/"),
            Token::Modulo => write!(f, "%"),
            Token::Equal => write!(f, "=="),
            Token::Assign => write!(f, "="),
            Token::ColonAssign => write!(f, ":="),
            Token::NotEqual => write!(f, "!="),
            Token::Not => write!(f, "!"),
            Token::ShiftLeft => write!(f, "<<"),
            Token::More => write!(f, ">"),
            Token::MoreEqual => write!(f, ">="),
            Token::Member => write!(f, "."),
            Token::Colon => write!(f, ":"),
            Token::Question => write!(f, "?"),
            Token::OpenBracket => write!(f, "["),
            Token::CloseBracket => write!(f, "]"),
            Token::ShiftRight => write!(f, ">>"),
            Token::Less => write!(f, "<"),
            Token::LessEqual => write!(f, "<="),
            Token::Function => write!(f, "fn"),
            Token::Import => write!(f, "import"),
            Token::Type => write!(f, "type"),
            Token::Constant => write!(f, "const"),
            Token::Parameter => write!(f, "parameter"),
            Token::Loop => write!(f, "loop"),
            Token::While => write!(f, "while"),
            Token::Continue => write!(f, "continue"),
            Token::Break => write!(f, "break"),
            Token::Return => write!(f, "return"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::Else => write!(f, "else"),
            Token::For => write!(f, "for"),
            Token::If => write!(f, "if"),
            Token::Match => write!(f, "match"),
            Token::Wildcard => write!(f, "_"),
            Token::FatArrow => write!(f, "=>"),
            Token::As => write!(f, "as"),
            Token::Assembly => write!(f, "assembly"),
            Token::Formula => write!(f, "formula"),
            Token::PeirceArrow => write!(f, "-->"),
            Token::Model => write!(f, "model"),
            Token::State => write!(f, "state"),
            Token::Start => write!(f, "start"),
            Token::Reference => write!(f, "ref"),
            Token::Condition => write!(f, "cond"),
            Token::Invariant => write!(f, "invariant"),
            Token::PortIn => write!(f, "in"),
            Token::PortOut => write!(f, "out"),
            Token::PortInOut => write!(f, "inout"),
            Token::Address => write!(f, "address"),
            Token::At => write!(f, "at"),
            Token::Clock => write!(f, "clock"),
            Token::After => write!(f, "after"),
            Token::Every => write!(f, "every"),
            Token::Variable => write!(f, "var"),
            Token::Next => write!(f, "next"),
            Token::Extern => write!(f, "extern"),
            Token::Enum => write!(f, "enum"),
            Token::Struct => write!(f, "struct"),
            Token::From => write!(f, "from"),
            Token::LtlNext => write!(f, "X"),
            Token::LtlFinally => write!(f, "F"),
            Token::LtlGlobally => write!(f, "G"),
            Token::LtlUntil => write!(f, "U"),
            Token::LtlRelease => write!(f, "R"),
            Token::TypeLtl => write!(f, "LTL"),
            Token::TypeGuard => write!(f, "Guard"),
            Token::Arrow => write!(f, "->"),
        }
    }
}
