//! Объявление параметра модели,.
//!
//! Проверяется только **объявление**: лексика (`parameter` - ключевое слово),
//! грамматика (инициализатор обязателен, форма живёт лишь на уровне модели), АСД-узел,
//! построение семантического дерева и печать форматтером. Аргументы инстанцирования
//! (`M(Y := 200)`) и режимы генерации (`--parameters=assign|specialize`).
//!
//! Ключевая проверка здесь - `parameter_is_a_plain_variable_in_the_tree`: в дереве
//! параметр обязан быть обычной переменной. На этом стоит режим по умолчанию
//! (`assign`): потребитель, ничего не знающий о параметрах, обращается с ним верно, а
//! не теряет молча ( - механизм, требующий правки в каждом из пяти потребителей,
//! расходится молча).

use takt_lang::format::format_source;
use takt_lang::parser::ast;
use takt_lang::parser::lexer::Lexer;
use takt_lang::parser::token::Token;
use takt_lang::semantic::VariableNode;
use takt_lang::semantic::tree::construct_model;
use takt_lang::{GenerateOptions, compile_to_c, parse};

/// Исходник примера: две настройки и накопитель.
const SRC: &str = "model Tuner {
    parameter limit: u8 := 100;
    parameter step: u8 := 5;
    var acc: u8 := 0;
    start Count {
        always {
            acc := acc + step;
        }
        ref Done: acc >= limit;
    }
    state Done;
}

start Main = Tuner;
";

/// Строит семантическое дерево из исходника.
fn build(src: &str) -> Result<takt_lang::semantic::ModelNode, takt_lang::diagnostics::Diagnostic> {
    let (tree, _) = parse(src, 0).expect("исходник должен разбираться");
    construct_model(&tree, None, &[]).map(|m| m.take())
}

/// Читает фикстуру и строит дерево.
fn build_file(
    path: &str,
) -> Result<takt_lang::semantic::ModelNode, takt_lang::diagnostics::Diagnostic> {
    let src = std::fs::read_to_string(path).unwrap_or_else(|e| panic!("не читается {path}: {e}"));
    build(&src)
}

// --- Лексика -----------------------------------------------------------------

/// `parameter` - ключевое слово, а не идентификатор.
#[test]
fn parameter_is_a_keyword() {
    let mut comments = Vec::new();
    let mut errors = Vec::new();
    let tokens: Vec<Token> = Lexer::new("parameter x", 0, &mut comments, &mut errors)
        .map(|(_, t, _)| t)
        .collect();
    assert_eq!(
        tokens.first(),
        Some(&Token::Parameter),
        "слово 'parameter' должно давать Token::Parameter, получено: {tokens:?}"
    );
}

/// Ломающее лексическое изменение зафиксировано: `parameter` больше не годится в
/// качестве имени. Проверка нужна, чтобы отказ был **осознанным** контрактом, а не
/// побочным следствием, замеченным пользователем.
#[test]
fn parameter_is_no_longer_a_valid_identifier() {
    assert!(
        parse("model M { var parameter: u8 := 1; start S; }", 0).is_err(),
        "'parameter' как имя переменной обязан отвергаться разбором"
    );
}

// --- Грамматика и АСД --------------------------------------------------------

/// Объявление даёт узел `VariableDefine::Parameter` с типом и инициализатором.
#[test]
fn declaration_builds_parameter_ast_node() {
    let (tree, _) = parse(SRC, 0).expect("исходник должен разбираться");
    let model = tree
        .elements
        .iter()
        .find_map(|e| match e {
            ast::ModelElement::Model(m)
                if m.name.as_ref().map(|i| i.name.as_str()) == Some("Tuner") =>
            {
                Some(m)
            }
            _ => None,
        })
        .expect("модель Tuner должна быть в дереве");

    let names: Vec<String> = model
        .elements
        .iter()
        .filter_map(|e| match e {
            ast::ModelElement::Variable(v) => match v.as_ref() {
                ast::VariableDefine::Parameter { name, .. } => {
                    name.as_ref().map(|i| i.name.clone())
                }
                _ => None,
            },
            _ => None,
        })
        .collect();

    assert_eq!(
        names,
        vec!["limit".to_string(), "step".to_string()],
        "оба параметра должны стать узлами Parameter в порядке объявления"
    );
}

/// Инициализатор обязателен: он и есть значение по умолчанию. Форма без него означала
/// бы обязательный параметр - он вынесен за границу фичи.
#[test]
fn parameter_without_initializer_is_a_parse_error() {
    assert!(
        parse("model M { parameter limit: u8; start S; }", 0).is_err(),
        "объявление параметра без ':= значение' обязано отвергаться разбором"
    );
}

/// Параметр объявляется только на уровне модели: внутри блока грамматика его не
/// принимает (инстанцировать локальное объявление нечем).
#[test]
fn parameter_inside_block_is_a_parse_error() {
    assert!(
        parse("model M { start S { parameter limit: u8 := 1; } }", 0).is_err(),
        "объявление параметра внутри блока обязано отвергаться разбором"
    );
}

// --- Семантика ---------------------------------------------------------------

/// В дереве параметр - **обычная переменная** с начальным значением, а порядок
/// объявления сохранён отдельно. На этом стоит режим генерации по умолчанию.
#[test]
fn parameter_is_a_plain_variable_in_the_tree() {
    let root = build(SRC).expect("модель с параметрами должна строиться");
    let tuner = root
        .models
        .get("Tuner")
        .expect("под-модель Tuner должна быть в дереве")
        .borrow();

    for name in ["limit", "step"] {
        match tuner.variables.get(name) {
            Some(VariableNode::Simple { .. }) => {}
            other => panic!("параметр '{name}' должен быть Simple-переменной, получено: {other:?}"),
        }
    }

    let declared: Vec<&str> = tuner.parameters.iter().map(|p| p.name.as_str()).collect();
    assert_eq!(
        declared,
        vec!["limit", "step"],
        "параметры обязаны храниться в порядке объявления: по нему строится ключ \
         дедупликации специализаций (ADR 0185)"
    );
    assert!(
        tuner
            .parameters
            .iter()
            .all(|p| p.loc != takt_lang::diagnostics::Location::Implicit),
        "у параметра обязана быть позиция объявления: без неё диагностика 0185-02 \
         укажет в 1:1"
    );
}

/// Обычные `var`/`const` параметрами не становятся.
#[test]
fn plain_variables_are_not_parameters() {
    let root = build(SRC).expect("модель с параметрами должна строиться");
    let tuner = root.models.get("Tuner").expect("Tuner").borrow();
    assert!(
        !tuner.parameters.iter().any(|p| p.name == "acc"),
        "переменная 'acc' не параметр — иначе граница «настройка vs величина такта» \
         теряется"
    );
}

/// Фикстура валидного объявления строится.
#[test]
fn valid_fixture_builds() {
    let root = build_file("tests/data/semantic/valid/parameter_declaration.takt")
        .expect("фикстура объявления параметра должна строиться");
    assert_eq!(
        root.models
            .get("Tuner")
            .expect("Tuner")
            .borrow()
            .parameters
            .len(),
        2,
        "в фикстуре объявлены два параметра"
    );
}

/// Параметр верхнего уровня файла - `SE-075`, а не молчаливое превращение в переменную:
/// анонимный корень в выражении реализации по имени не появляется.
#[test]
fn parameter_at_top_level_is_se075() {
    let err = build_file("tests/data/semantic/invalid/parameter_at_top_level.takt")
        .expect_err("параметр вне модели обязан отвергаться");
    assert_eq!(
        err.code.as_deref(),
        Some("SE-075"),
        "ожидался SE-075, получено: {:?} ({})",
        err.code,
        err.message
    );
}

// --- Форматтер ---------------------------------------------------------------

/// Форматтер печатает объявление, а не отказывает: непокрытый узел АСД валит
/// `format_source` по замыслу (молча потерять кусок исходника хуже).
#[test]
fn formatter_prints_parameter_declaration() {
    let out = format_source(SRC).expect("форматтер обязан печатать объявление параметра");
    assert!(
        out.contains("parameter limit: u8 := 100;"),
        "ожидалось объявление параметра в выводе форматтера, получено:\n{out}"
    );
    let again = format_source(&out).expect("повторная печать");
    assert_eq!(out, again, "печать обязана быть идемпотентной");
}

// --- Цель `c` (режим по умолчанию `assign`) ----------------------------------

/// Параметр доезжает до цели `c` полем структуры, инициализируемым значением по
/// умолчанию, - то есть ведёт себя как переменная. Это и есть форма режима `assign` без
/// аргументов инстанцирования.
#[test]
fn parameter_reaches_c_as_a_field() {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join("takt-0185-01-c");
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог для вывода");
    compile_to_c(
        "tuner.takt",
        SRC,
        dir.to_str().expect("путь"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("цель c должна принять параметр");

    let header =
        std::fs::read_to_string(dir.join("tuner.h")).expect("заголовок должен быть создан");
    assert!(
        header.contains("uint8_t limit;") && header.contains("uint8_t step;"),
        "оба параметра обязаны быть полями структуры, получено:\n{header}"
    );

    let body = std::fs::read_to_string(dir.join("tuner.c")).expect("тело должно быть создано");
    assert!(
        body.contains("model->limit = 100;") && body.contains("model->step = 5;"),
        "значение по умолчанию обязано попадать в инициализатор, получено:\n{body}"
    );
}
