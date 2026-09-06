//! Продолжение `lexer_tests`: вторая половина тестов лексера, вынесенная в подмодуль.
//! Helpers и импорты - из родителя (`use super::*`); чистое перемещение тестов,
//! утверждения не меняются.

use super::*;

// --------------------------- Тесты Display токенов ---------------------------

/// Display токенов ключевых слов совпадает с исходным кодом.
#[test]
fn token_display_keywords() {
    let cases: &[(&str, Token)] = &[
        ("model", Token::Model),
        ("state", Token::State),
        ("start", Token::Start),
        ("ref", Token::Reference),
        ("cond", Token::Condition),
        ("var", Token::Variable),
        ("const", Token::Constant),
        ("in", Token::PortIn),
        ("out", Token::PortOut),
        ("fn", Token::Function),
        ("true", Token::True),
        ("false", Token::False),
        ("next", Token::Next),
        ("extern", Token::Extern),
        ("import", Token::Import),
        ("type", Token::Type),
        ("loop", Token::Loop),
        ("continue", Token::Continue),
        ("break", Token::Break),
        ("return", Token::Return),
        ("else", Token::Else),
        ("for", Token::For),
        ("if", Token::If),
        ("as", Token::As),
        ("assembly", Token::Assembly),
        ("formula", Token::Formula),
        ("X", Token::LtlNext),
        ("F", Token::LtlFinally),
        ("G", Token::LtlGlobally),
        ("U", Token::LtlUntil),
        ("R", Token::LtlRelease),
        ("LTL", Token::TypeLtl),
        ("Guard", Token::TypeGuard),
    ];
    for (expected, token) in cases {
        assert_eq!(
            &token.to_string(),
            expected,
            "Display для {:?} должен быть '{}'",
            token,
            expected
        );
    }
}

/// Display токенов операторов и знаков пунктуации.
#[test]
fn token_display_operators() {
    let cases: &[(&str, Token)] = &[
        (";", Token::Semicolon),
        (",", Token::Comma),
        ("#", Token::Sharp),
        ("(", Token::OpenParenthesis),
        (")", Token::CloseParenthesis),
        ("{", Token::OpenCurlyBrace), // отображается как "{"
        ("}", Token::CloseCurlyBrace),
        ("|", Token::BitwiseOr),
        ("^", Token::BitwiseXor),
        ("||", Token::Or),
        ("&", Token::BitwiseAnd),
        ("~", Token::BitwiseNot),
        ("&&", Token::And),
        ("+", Token::Add),
        ("-", Token::Subtract),
        ("*", Token::Mul),
        ("**", Token::Power),
        ("/", Token::Divide),
        ("%", Token::Modulo),
        ("==", Token::Equal),
        ("=", Token::Assign),
        ("!=", Token::NotEqual),
        ("!", Token::Not),
        ("<<", Token::ShiftLeft),
        (">", Token::More),
        (">=", Token::MoreEqual),
        (".", Token::Member),
        (":", Token::Colon),
        ("[", Token::OpenBracket),
        ("]", Token::CloseBracket),
        (">>", Token::ShiftRight),
        ("<", Token::Less),
        ("<=", Token::LessEqual),
        ("->", Token::Arrow),
        ("-->", Token::PeirceArrow),
    ];
    for (expected, token) in cases {
        assert_eq!(
            &token.to_string(),
            expected,
            "Display для {:?} должен быть '{}'",
            token,
            expected
        );
    }
}

/// Display для литеральных токенов.
#[test]
fn token_display_literals() {
    // Числовой
    assert_eq!(Token::Number(42).to_string(), "42");
    assert_eq!(Token::Number(-1).to_string(), "-1");

    // Рациональный - без знака
    assert_eq!(Token::RationalNumber("3.14", false).to_string(), "3.14");
    // Рациональный - отрицательный
    assert_eq!(Token::RationalNumber("3.14", true).to_string(), "-3.14");

    // Строковый - обычный
    assert_eq!(Token::StringLiteral(false, "hi").to_string(), "\"hi\"");
    // Строковый - unicode
    assert_eq!(
        Token::StringLiteral(true, "hi").to_string(),
        "unicode\"hi\""
    );

    // Адресный
    assert_eq!(Token::AddressLiteral("0x1234").to_string(), "0x1234");

    // Идентификатор
    assert_eq!(Token::Identifier("myVar").to_string(), "myVar");
}

// --------------------------- Тесты ошибок (Display и loc) --------------------

/// `LexicalError::loc()` возвращает корректное местоположение для каждого варианта.
#[test]
fn lexical_error_loc_for_all_variants() {
    let loc = Location::Source(0, 5, 10);

    let cases = vec![
        LexicalError::EndOfFileInComment(loc),
        LexicalError::EndOfFileInString(loc),
        LexicalError::EndOfFileInHex(loc),
        LexicalError::MissingNumber(loc),
        LexicalError::InvalidCharacterInHexLiteral(loc, 'Z'),
        LexicalError::UnrecognisedToken(loc, "@".to_string()),
        LexicalError::MissingExponent(loc),
        LexicalError::ExpectedFrom(loc, "foo".to_string()),
    ];

    for err in &cases {
        assert_eq!(
            err.loc(),
            Location::Source(0, 5, 10),
            "Неверный loc для {:?}",
            err
        );
    }
}

/// Текстовые сообщения `LexicalError` содержат нужные подстроки.
#[test]
fn lexical_error_display_messages() {
    let loc = Location::Source(0, 0, 1);

    let cases: Vec<(LexicalError, &str)> = vec![
        (
            LexicalError::EndOfFileInComment(loc),
            "неожиданный конец файла внутри комментария",
        ),
        (
            LexicalError::EndOfFileInString(loc),
            "неожиданный конец файла внутри строкового литерала",
        ),
        (
            LexicalError::EndOfFileInHex(loc),
            "неожиданный конец файла внутри шестнадцатеричного литерала",
        ),
        (
            LexicalError::MissingNumber(loc),
            "отсутствует число после '0x'",
        ),
        (
            LexicalError::InvalidCharacterInHexLiteral(loc, 'Z'),
            "недопустимый символ 'Z'",
        ),
        (
            LexicalError::UnrecognisedToken(loc, "@".into()),
            "нераспознанный токен '@'",
        ),
        (
            LexicalError::MissingExponent(loc),
            "отсутствует показатель степени",
        ),
        (
            LexicalError::ExpectedFrom(loc, "bar".into()),
            "ожидалось ключевое слово 'from', но найдено 'bar'",
        ),
    ];

    for (err, expected_substr) in &cases {
        let msg = err.to_string();
        assert!(
            msg.contains(expected_substr),
            "Сообщение {:?} не содержит '{}'",
            msg,
            expected_substr
        );
    }
}

// --------------------------- Тесты комментариев ------------------------------

/// Обычный комментарий `//` не производит токены, но собирается.
#[test]
fn line_comment_produces_no_tokens() {
    let src = "// это комментарий\nmodel";
    assert_eq!(token_count(src), 1, "После комментария — только 'model'");
    assert_eq!(comment_count(src), 1, "Ожидался один комментарий");
    assert!(!first_comment_is_doc(src), "// — не документационный");
}

/// Документационный комментарий `///` помечается флагом.
#[test]
fn doc_comment_is_distinguished_from_line_comment() {
    let src = "/// документация\nmodel";
    assert_eq!(token_count(src), 1);
    assert_eq!(comment_count(src), 1);
    assert!(first_comment_is_doc(src), "/// — документационный");
}

/// `////` и далее - обычный (не документационный) комментарий.
#[test]
fn four_slash_comment_is_not_doc() {
    let src = "//// not doc\nmodel";
    assert_eq!(comment_count(src), 1);
    assert!(!first_comment_is_doc(src), "//// не документационный");
}

/// Пустой комментарий `//` (без текста) разбирается без ошибок.
#[test]
fn empty_line_comment_lexes_ok() {
    let errors = collect_errors("//\nmodel");
    assert!(
        errors.is_empty(),
        "Пустой комментарий вызвал ошибку: {:?}",
        errors
    );
}

/// Несколько комментариев - все собраны.
#[test]
fn multiple_comments_are_all_collected() {
    let src = "// первый\n// второй\n/// третий\nmodel";
    assert_eq!(comment_count(src), 3, "Ожидались 3 комментария");
    assert_eq!(token_count(src), 1, "После комментариев — только 'model'");
}

/// Комментарий в конце файла без перевода строки не вызывает ошибок.
#[test]
fn comment_at_eof_without_newline() {
    let src = "model // в конце";
    assert_eq!(token_count(src), 1);
    assert_eq!(comment_count(src), 1);
    let errors = collect_errors(src);
    assert!(errors.is_empty());
}

// ----------------------- Тесты блочных комментариев --------------------------

/// Блочный комментарий `/* */` не производит токены, но собирается.
#[test]
fn block_comment_produces_no_tokens() {
    let src = "/* блочный комментарий */\nmodel";
    assert_eq!(
        token_count(src),
        1,
        "После блочного комментария — только 'model'"
    );
    assert_eq!(comment_count(src), 1, "Ожидался один блочный комментарий");
}

/// Блочный комментарий помечается флагом `is_block()`.
#[test]
fn block_comment_is_detected_as_block() {
    use takt_lang::parser::ast::Comment;
    let src = "/* блок */\nmodel";
    let mut comments = Vec::new();
    let mut errors = Vec::new();
    let _: Vec<_> = Lexer::new(src, 0, &mut comments, &mut errors).collect();
    assert_eq!(comments.len(), 1);
    assert!(
        matches!(&comments[0], Comment::Block(..)),
        "Ожидался Comment::Block, получено: {:?}",
        comments[0]
    );
    assert!(comments[0].is_block(), "is_block() должен возвращать true");
    assert!(!comments[0].is_doc(), "is_doc() должен возвращать false");
    assert!(!comments[0].is_line(), "is_line() должен возвращать false");
}

/// Блочный комментарий может содержать несколько строк.
#[test]
fn multiline_block_comment_lexes_ok() {
    let src = "/*\n строка 1\n строка 2\n*/\nvar x = 1;";
    let errors = collect_errors(src);
    assert!(
        errors.is_empty(),
        "Многострочный блочный комментарий вызвал ошибку: {:?}",
        errors
    );
    assert_eq!(comment_count(src), 1, "Ожидался один комментарий");
    assert_eq!(token_count(src), 5, "Ожидались токены: var, x, =, 1, ;");
}

/// Блочный комментарий может содержать `//` внутри.
#[test]
fn block_comment_containing_line_comment_syntax() {
    let src = "/* содержит // строчный синтаксис */\nmodel";
    assert_eq!(comment_count(src), 1);
    assert_eq!(token_count(src), 1);
    let errors = collect_errors(src);
    assert!(errors.is_empty());
}

/// Несколько блочных комментариев - все собраны.
#[test]
fn multiple_block_comments_collected() {
    let src = "/* первый */ var /* второй */ x = 1;";
    assert_eq!(comment_count(src), 2, "Ожидались 2 блочных комментария");
    assert_eq!(token_count(src), 5, "var, x, =, 1, ;");
    let errors = collect_errors(src);
    assert!(errors.is_empty());
}

/// Смешанные строчные и блочные комментарии - все собраны.
#[test]
fn mixed_line_and_block_comments_collected() {
    let src = "// строчный\n/* блочный */\n/// документация\nmodel";
    assert_eq!(comment_count(src), 3, "Ожидались 3 комментария");
    assert_eq!(token_count(src), 1, "После комментариев — только 'model'");
}

/// Незакрытый блочный комментарий порождает `EndOfFileInComment`.
#[test]
fn unclosed_block_comment_produces_eof_error() {
    let src = "/* незакрытый комментарий";
    let errors = collect_errors(src);
    assert!(
        !errors.is_empty(),
        "Незакрытый блочный комментарий должен давать ошибку"
    );
    assert!(
        matches!(errors[0], LexicalError::EndOfFileInComment(_)),
        "Ожидалась EndOfFileInComment, получено: {:?}",
        errors[0]
    );
}

/// Пустой блочный комментарий `/**/` разбирается без ошибок.
#[test]
fn empty_block_comment_lexes_ok() {
    let src = "/**/ model";
    let errors = collect_errors(src);
    assert!(
        errors.is_empty(),
        "Пустой блочный комментарий вызвал ошибку: {:?}",
        errors
    );
    assert_eq!(comment_count(src), 1);
    assert_eq!(token_count(src), 1);
}

/// Деление `/` после блочного комментария разбирается корректно.
#[test]
fn divide_after_block_comment_is_divide_token() {
    let src = "/* комментарий */ 10 / 2";
    assert_eq!(comment_count(src), 1);
    assert_eq!(token_count(src), 3, "10, /, 2");
    let errors = collect_errors(src);
    assert!(errors.is_empty());
}

/// Значение блочного комментария содержит исходный текст включая `/*` и `*/`.
#[test]
fn block_comment_value_contains_full_text() {
    let src = "/* содержимое */";
    let mut comments = Vec::new();
    let mut errors = Vec::new();
    let _: Vec<_> = Lexer::new(src, 0, &mut comments, &mut errors).collect();
    assert_eq!(comments.len(), 1);
    let val = comments[0].value();
    assert!(
        val.contains("содержимое"),
        "Значение должно содержать текст комментария: {}",
        val
    );
}

// ----------------------- Тесты ошибочных конструкций -------------------------

/// Неизвестный символ `@` порождает `UnrecognisedToken`.
#[test]
fn unknown_character_at_produces_error() {
    let errors = collect_errors("@");
    assert!(!errors.is_empty(), "Символ '@' должен давать ошибку");
    assert!(
        matches!(errors[0], LexicalError::UnrecognisedToken(_, _)),
        "Ожидалась UnrecognisedToken, получено: {:?}",
        errors[0]
    );
}

/// Ошибка лексера содержит корректное местоположение.
#[test]
fn lexical_error_has_source_location() {
    let errors = collect_errors("@");
    assert!(!errors.is_empty());
    let loc = errors[0].loc();
    assert_eq!(loc.start(), 0, "Ошибка должна начинаться с позиции 0");
}

/// Несколько неизвестных символов - ошибка для каждого.
#[test]
fn multiple_unknown_chars_produce_multiple_errors() {
    let errors = collect_errors("@ @");
    assert!(
        errors.len() >= 2,
        "Два символа '@' должны дать не менее 2 ошибок"
    );
}

// ----------------------- Тесты позиций токенов -------------------------------

/// Позиции токенов соответствуют их расположению в исходной строке.
#[test]
fn token_positions_are_correct() {
    let src = "model M".to_string();
    let mut comments = Vec::new();
    let mut errors = Vec::new();
    let spanned: Vec<_> = Lexer::new(&src, 0, &mut comments, &mut errors).collect();

    assert_eq!(spanned.len(), 2, "Ожидалось 2 токена");
    let (start, _, end) = spanned[0];
    assert_eq!(start, 0, "'model' начинается с 0");
    assert_eq!(end, 5, "'model' заканчивается на 5");

    let (start, _, end) = spanned[1];
    assert_eq!(start, 6, "'M' начинается с 6");
    assert_eq!(end, 7, "'M' заканчивается на 7");
}

// ---------------------------- Тесты идентификаторов --------------------------

/// Идентификаторы разбираются без ошибок.
#[test]
fn identifiers_lex_without_errors() {
    let ids = [
        "abc",
        "ABC",
        "_private",
        "$dollar",
        "CamelCase",
        "x1",
        "_123",
    ];
    for id in ids {
        let errors = collect_errors(id);
        assert!(
            errors.is_empty(),
            "Ошибка при идентификаторе '{}': {:?}",
            id,
            errors
        );
        assert_eq!(token_count(id), 1, "'{}' должен давать 1 токен", id);
    }
}

/// Идентификатор, совпадающий с ключевым словом, распознаётся как ключевое слово.
#[test]
fn keyword_is_not_identifier() {
    let src = "model".to_string();
    let errors = collect_errors(&src);
    assert!(errors.is_empty());
    let strings = collect_token_strings(&src);
    assert_eq!(strings.len(), 1);
    assert_eq!(strings[0], "model", "model должен быть Token::Model");
}

/// Пробелы и переносы строк игнорируются.
#[test]
fn whitespace_is_skipped() {
    assert_eq!(token_count("  \t  model  \n  state  "), 2);
    assert_eq!(collect_errors("  \t  model  \n  state  ").len(), 0);
}

/// Пустая строка не даёт токенов и не даёт ошибок.
#[test]
fn empty_input_produces_nothing() {
    assert_eq!(token_count(""), 0);
    assert!(collect_errors("").is_empty());
}

// ---------------------------- Тест `pragma` как идентификатора ---------------

/// `pragma` не является ключевым словом в Takt - распознаётся как идентификатор.
#[test]
fn pragma_is_not_a_keyword_and_lexes_as_identifier() {
    let strings = collect_token_strings("pragma");
    assert_eq!(strings.len(), 1);
    assert_eq!(strings[0], "pragma", "pragma должен быть идентификатором");
    assert_eq!(collect_errors("pragma").len(), 0);
}

// ------------------- Тест полного набора Takt-конструкций ---------------------

/// Полный пример программы лексируется без ошибок.
#[test]
fn complete_but_program_lexes_without_errors() {
    let src = r#"
/// Пример полной Takt-программы
const MAX: u8 = 0xFF;
out LED: u8 = 00100000;
var counter: u8 = 0;
cond IsMax = counter = MAX;
model Blinker {
    start On {
        ref Off: IsMax;
        enter { LED = 0xFF; }
        exit  { LED = 0x00; }
        always { counter = counter + 1; }
    }
    state Off {
        ref On: true;
    }
}
start Main = Blinker;
    "#;
    let errors = collect_errors(src);
    assert!(
        errors.is_empty(),
        "Полная программа вызвала ошибки: {:?}",
        errors
    );
}

/// `extern fn` лексируется как два токена: `Extern` и `Function`.
#[test]
fn extern_fn_lexes_as_two_tokens() {
    let strings = collect_token_strings("extern fn");
    assert_eq!(strings.len(), 2);
    assert_eq!(strings[0], "extern", "Первый токен — extern");
    assert_eq!(strings[1], "fn", "Второй токен — fn");
}
