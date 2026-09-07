//! Вход разбора: построение АСД и перевод ошибок парсера в диагностики.
//!
//! Здесь же стоит проверка предела глубины дерева: разбор отдаёт наружу только
//! дерево, уложившееся в предел.

use lalrpop_util::ParseError;

use crate::diagnostics::{Diagnostic, Location};
use crate::parser::ast;
use crate::parser::lexer::{self, LexicalError, Token};
use crate::{grammar, parser};

/// Разбирает исходник **без** проверки предела глубины.
///
/// Внутренняя половина [`parse`]: наружу дерево произвольной глубины отдавать нельзя
/// (рекурсивные потребители), но самому измерению глубины и его тестам нужен именно
/// неограниченный разбор.
pub(crate) fn parse_without_depth_limit(
    src: &str,
    file_no: u64,
) -> Result<(ast::Model, Vec<ast::Comment>), Vec<Diagnostic>> {
    let mut comments = Vec::new();
    let mut lexer_errors = Vec::new();
    let mut lex = lexer::Lexer::new(src, file_no, &mut comments, &mut lexer_errors);

    let mut parser_errors = Vec::new();
    let res = grammar::SourceUnitParser::new().parse(src, file_no, &mut parser_errors, &mut lex);

    let mut diagnostics = Vec::with_capacity(lex.errors.len() + parser_errors.len());
    for lexical_error in lex.errors {
        diagnostics.push(
            Diagnostic::parser_error(lexical_error.loc(), lexical_error.to_string())
                .with_code(lexical_error.code()),
        );
    }

    for e in parser_errors {
        diagnostics.push(parser_error_to_diagnostic(&e.error, file_no));
    }

    match res {
        Err(e) => {
            diagnostics.push(parser_error_to_diagnostic(&e, file_no));
            Err(diagnostics)
        }
        Ok(model) if !diagnostics.is_empty() => {
            // Дерево наружу не идёт, но уничтожать его рекурсивным `Drop` нельзя:
            // глубокий файл с синтаксической ошибкой уронил бы процесс ровно в момент
            // отказа.
            parser::depth::dismantle(model);
            Err(diagnostics)
        }
        Ok(model) => Ok((model, comments)),
    }
}

/// Преобразует ошибку LALRPOP-парсера в [`Diagnostic`].
fn parser_error_to_diagnostic(
    error: &ParseError<usize, Token, LexicalError>,
    file_no: u64,
) -> Diagnostic {
    match error {
        ParseError::InvalidToken { location } => Diagnostic::parser_error(
            Location::source(file_no, *location, *location),
            "недопустимый токен".to_string(),
        )
        .with_code("SY-001"),
        // Присваивание в позиции значения - своя диагностика.
        //
        // Грамматика допускает `:=` ровно в трёх местах: оператор тела, шаг цикла `for`
        // и именованный аргумент вызова. Встретив токен где-то ещё, LALRPOP сообщил бы
        // "нераспознанный токен ':=', ожидалось: ..." со списком из двадцати пяти
        // операторов - сообщение о механике разбора вместо правила языка. Здесь оно
        // заменяется на само правило.
        ParseError::UnrecognizedToken {
            token: (l, Token::ColonAssign, r),
            ..
        } => Diagnostic::parser_error(
            Location::source(file_no, *l, *r),
            "присваивание стоит там, где вычисляется значение: в языке \
             присваивание — оператор, а не выражение. Запись обязана быть \
             отдельным оператором (либо шагом цикла `for`): сперва \
             `цель := значение;`, затем использование цели"
                .to_string(),
        )
        .with_code("SY-006"),
        // Адресный литерал вне позиции размещения - своя диагностика.
        //
        // Грамматика принимает `0xАДРЕС:бит` только там, где адрес осмыслен: после `at`
        // и в операторе `address`. В выражении тот же токен даёт "нераспознанный токен"
        // со списком из двух десятков ожидаемых - здесь это сообщение заменяется
        // правилом языка.
        ParseError::UnrecognizedToken {
            token: (l, Token::AddressLiteral(text), r),
            ..
        } => Diagnostic::parser_error(
            Location::source(file_no, *l, *r),
            format!(
                "адресный литерал '{text}' стоит там, где вычисляется значение: \
                 адрес есть свойство размещения ('at {text}', оператор 'address', \
                 внешняя карта). Чтобы обратиться к ячейке в выражении, напишите \
                 '#{text} as ТИП' либо '#0xАДРЕС.БИТ'"
            ),
        )
        .with_code("SY-008"),
        ParseError::UnrecognizedToken {
            token: (l, token, r),
            expected,
        } => Diagnostic::parser_error(
            Location::source(file_no, *l, *r),
            format!(
                "нераспознанный токен '{}', ожидалось {}",
                token,
                expected.join(", ")
            ),
        )
        .with_code("SY-002"),
        ParseError::User { error } => {
            Diagnostic::parser_error(error.loc(), error.to_string()).with_code(error.code())
        }
        ParseError::ExtraToken { token } => Diagnostic::parser_error(
            Location::source(file_no, token.0, token.2),
            format!("лишний токен '{}'", token.1),
        )
        .with_code("SY-003"),
        ParseError::UnrecognizedEof { expected, location } => Diagnostic::parser_error(
            Location::source(file_no, *location, *location),
            format!("неожиданный конец файла, ожидалось {}", expected.join(", ")),
        )
        .with_code("SY-004"),
    }
}
