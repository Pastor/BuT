use std::sync::mpsc;
use std::time::Duration;
use std::{fs, path::Path, thread};

use pretty_assertions::assert_eq;
use walkdir::WalkDir;

use crate::ast::*;
use crate::ast::Annotation::Function;
use crate::ast::Expression::{AddressLiteral, BitwiseAnd, BitwiseOr, Initializer, NumberLiteral, Parenthesis, RationalNumberLiteral, Variable};
use crate::ast::Type::{Alias, Array};
use crate::ast::VariableAttribute::{Constant, Portable, Readable, Writable};
use crate::but;
use crate::diagnostics::{Diagnostic, ErrorType::ParserError, Level::Error};
use crate::lexer::Lexer;
use crate::Loc::Source;

#[test]
fn parser_error_recovery() {
    let src = r#"import * as sesa frum "sesa";
    struct None {
        if (true)
    }
    "#;

    if let Err(errors) = crate::parse(src, 0) {
        assert_eq!(
            errors,
            vec![
                Diagnostic { loc: Source(0, 17, 21), level: Error, ty: ParserError,
                    message: "'frum' found where 'from' expected".to_string(), notes: vec![] },
                Diagnostic { loc: Source(0, 56, 58), level: Error, ty: ParserError,
                    message: "unrecognised token 'if', expected \"#\", \"const\", \"external\", \"let\", \"mut\", \"pio\", \"}\", identifier".to_string(), notes: vec![] },
            ]
        )
    }
}

#[test]
fn parse_function_assembly() {
    let src = r#"
        #[unused, always_inline, align(4)]
        fn try_recover() {
            assembly {

            }
        }
        "#;

    let (actual_parse_tree, _) = crate::parse(src, 0).unwrap();
    assert_eq!(actual_parse_tree.0.len(), 1);
}

#[test]
fn parse_random_doc_comment() {
    let src = r#"
 const /** x */ dev : /** x */ usize  = /** x */1 /** x */ + /** x */2/** x */;
    "#;

    let (actual_parse_tree, _) = crate::parse(src, 0).unwrap();
    assert_eq!(actual_parse_tree.0.len(), 1);
}

#[test]
fn test_lib_but() {
    fn timeout_after<T, F>(d: Duration, f: F) -> Result<T, String>
    where
        T: Send + 'static,
        F: FnOnce() -> T,
        F: Send + 'static,
    {
        let (done_tx, done_rx) = mpsc::channel();
        let handle = thread::spawn(move || {
            let val = f();
            done_tx.send(()).expect("Unable to send completion signal");
            val
        });

        match done_rx.recv_timeout(d) {
            Ok(_) => Ok(handle.join().expect("Thread panicked")),
            Err(_) => Err(format!("Thread timeout-ed after {d:?}")),
        }
    }

    let source_delimiter = regex::Regex::new(r"====.*====").unwrap();
    let error_matcher = regex::Regex::new(r"// ----\r?\n// \w+( \d+)?:").unwrap();

    let semantic_tests =
        WalkDir::new(Path::new(env!("CARGO_MANIFEST_DIR")).join("../tests_data/BuT/semantic"))
            .into_iter()
            .collect::<Result<Vec<_>, _>>()
            .unwrap()
            .into_iter()
            .map(|entry| (false, entry));

    let syntax_tests =
        WalkDir::new(Path::new(env!("CARGO_MANIFEST_DIR")).join("../tests_data/BuT/syntax"))
            .into_iter()
            .collect::<Result<Vec<_>, _>>()
            .unwrap()
            .into_iter()
            .map(|entry| (true, entry));

    let errors = semantic_tests
        .into_iter()
        .chain(syntax_tests)
        .map::<Result<_, String>, _>(|(syntax_test, entry)| {
            if entry.file_name().to_string_lossy().ends_with(".but") {
                let source = match fs::read_to_string(entry.path()) {
                    Ok(source) => source,
                    Err(err) if matches!(err.kind(), std::io::ErrorKind::InvalidData) => {
                        return Ok(vec![]);
                    }
                    Err(err) => return Err(err.to_string()),
                };

                let expect_error = syntax_test && error_matcher.is_match(&source);

                Ok(source_delimiter
                    .split(&source)
                    .filter(|source_part| !source_part.is_empty())
                    .map(|part| {
                        (
                            entry.path().to_string_lossy().to_string(),
                            expect_error,
                            part.to_string(),
                        )
                    })
                    .collect::<Vec<_>>())
            } else {
                Ok(vec![])
            }
        })
        .collect::<Result<Vec<_>, _>>()
        .unwrap()
        .into_iter()
        .flatten()
        .filter_map(|(path, expect_error, source_part)| {
            let result = match timeout_after(Duration::from_secs(5), move || {
                crate::parse(&source_part, 0)
            }) {
                Ok(result) => result,
                Err(err) => return Some(format!("{path:?}: \n\t{err}")),
            };

            if let (Err(err), false) = (
                result.map_err(|diags| {
                    format!(
                        "{:?}:\n\t{}",
                        path,
                        diags
                            .iter()
                            .map(|diag| format!("{diag:?}"))
                            .collect::<Vec<_>>()
                            .join("\n\t")
                    )
                }),
                expect_error,
            ) {
                return Some(err);
            }

            None
        })
        .collect::<Vec<_>>();

    assert!(errors.is_empty(), "{}", errors.join("\n"));
}

#[test]
fn parse_const_variable() {
    let src = r#"const PI: float = 3.1415;"#;
    let (actual_parse_tree, _) = crate::parse(src, 0).unwrap();
    std::assert_eq!(actual_parse_tree.0.len(), 1);
    let expected_parse_tree = SourceUnit(vec![SourceUnitPart::VariableDefinition(Box::new(
        VariableDefinition {
            loc: Source(0, 0, 24),
            ty: Alias(Identifier {
                loc: Source(0, 10, 15),
                name: "float".to_string(),
            }),
            annotations: vec![],
            attrs: vec![Constant(Source(0, 0, 5))],
            name: Some(Identifier {
                loc: Source(0, 6, 8),
                name: "PI".to_string(),
            }),
            initializer: Some(RationalNumberLiteral(
                Source(0, 18, 24),
                "3.1415".to_string(),
                false,
            )),
        },
    ))]);
    std::assert_eq!(actual_parse_tree, expected_parse_tree);
}

#[test]
fn parse_port_with_address() {
    let src = r#"pio  port5: bit = 0xFFB0:2;"#;
    let (actual_parse_tree, _) = crate::parse(src, 0).unwrap();
    std::assert_eq!(actual_parse_tree.0.len(), 1);
    let expected_parse_tree = SourceUnit(vec![SourceUnitPart::VariableDefinition(Box::new(
        VariableDefinition {
            loc: Source(0, 0, 26),
            ty: Alias(Identifier {
                loc: Source(0, 12, 15),
                name: "bit".to_string(),
            }),
            annotations: vec![],
            attrs: vec![Portable(Source(0, 0, 3))],
            name: Some(Identifier {
                loc: Source(0, 5, 10),
                name: "port5".to_string(),
            }),
            initializer: Some(AddressLiteral(Source(0, 18, 26), 65456, 2)),
        },
    ))]);
    std::assert_eq!(actual_parse_tree, expected_parse_tree);

    let src = r#"pio port6: [8: bit] = 0xFFC0; "#;
    let (actual_parse_tree, _) = crate::parse(src, 0).unwrap();
    std::assert_eq!(actual_parse_tree.0.len(), 1);
    let expected_parse_tree = SourceUnit(vec![SourceUnitPart::VariableDefinition(Box::new(
        VariableDefinition {
            loc: Source(0, 0, 28),
            ty: Array {
                loc: Source(0, 11, 19),
                element_count: 8,
                element_type: Box::new(Alias(Identifier {
                    loc: Source(0, 15, 18),
                    name: "bit".to_string(),
                })),
            },
            annotations: vec![],
            attrs: vec![Portable(Source(0, 0, 3))],
            name: Some(Identifier {
                loc: Source(0, 4, 9),
                name: "port6".to_string(),
            }),
            initializer: Some(NumberLiteral(Source(0, 22, 28), 65472)),
        },
    ))]);
    std::assert_eq!(actual_parse_tree, expected_parse_tree);
}

#[test]
fn parse_variable_with_initializer() {
    let src = r#"let mut  var6: [ 3: [3: u8] ] = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}};"#;
    let (actual_parse_tree, _) = crate::parse(src, 0).unwrap();
    std::assert_eq!(actual_parse_tree.0.len(), 1);
    let expected_parse_tree = SourceUnit(vec![SourceUnitPart::VariableDefinition(Box::new(
        VariableDefinition {
            loc: Source(0, 0, 65),
            ty: Array {
                loc: Source(0, 15, 29),
                element_count: 3,
                element_type: Box::new(Array {
                    loc: Source(0, 20, 27),
                    element_count: 3,
                    element_type: Box::new(Alias(Identifier {
                        loc: Source(0, 24, 26),
                        name: "u8".to_string(),
                    })),
                }),
            },
            annotations: vec![],
            attrs: vec![Readable(Source(0, 0, 3)), Writable(Source(0, 4, 7))],
            name: Some(Identifier {
                loc: Source(0, 9, 13),
                name: "var6".to_string(),
            }),
            initializer: Some(Initializer(
                Source(0, 32, 65),
                vec![
                    Initializer(
                        Source(0, 33, 42),
                        vec![
                            NumberLiteral(Source(0, 34, 35), 1),
                            NumberLiteral(Source(0, 37, 38), 2),
                            NumberLiteral(Source(0, 40, 41), 3),
                        ],
                    ),
                    Initializer(
                        Source(0, 44, 53),
                        vec![
                            NumberLiteral(Source(0, 45, 46), 4),
                            NumberLiteral(Source(0, 48, 49), 5),
                            NumberLiteral(Source(0, 51, 52), 6),
                        ],
                    ),
                    Initializer(
                        Source(0, 55, 64),
                        vec![
                            NumberLiteral(Source(0, 56, 57), 7),
                            NumberLiteral(Source(0, 59, 60), 8),
                            NumberLiteral(Source(0, 62, 63), 9),
                        ],
                    ),
                ],
            )),
        },
    ))]);
    std::assert_eq!(actual_parse_tree, expected_parse_tree);
}

#[test]
fn parse_alias_type() {
    let src = r#"type bool  = bit;"#;
    let (actual_parse_tree, _) = crate::parse(src, 0).unwrap();
    std::assert_eq!(actual_parse_tree.0.len(), 1);
    let expected_parse_tree = SourceUnit(vec![SourceUnitPart::TypeDefinition(Box::new(
        TypeDefinition {
            loc: Loc::Source(0, 0, 16),
            name: Identifier {
                loc: Loc::Source(0, 5, 9),
                name: "bool".to_string(),
            },
            ty: Type::Alias(Identifier {
                loc: Loc::Source(0, 13, 16),
                name: "bit".to_string(),
            }),
        },
    ))]);
    std::assert_eq!(actual_parse_tree, expected_parse_tree);
}

#[test]
fn parse_simple_array_type() {
    let src = r#"type u8 = [8   : bit];"#;
    let (actual_parse_tree, _) = crate::parse(src, 0).unwrap();
    std::assert_eq!(actual_parse_tree.0.len(), 1);
    let expected_parse_tree = SourceUnit(vec![SourceUnitPart::TypeDefinition(Box::new(
        TypeDefinition {
            loc: Loc::Source(0, 0, 21),
            name: Identifier {
                loc: Loc::Source(0, 5, 7),
                name: "u8".to_string(),
            },
            ty: Type::Array {
                loc: Loc::Source(0, 10, 21),
                element_count: 8,
                element_type: Box::new(Type::Alias(Identifier {
                    loc: Loc::Source(0, 17, 20),
                    name: "bit".to_string(),
                })),
            },
        },
    ))]);
    std::assert_eq!(actual_parse_tree, expected_parse_tree);
}

#[test]
fn parse_nested_array_type() {
    let src = r#"type block = [2 : [2: bit]];"#;
    let (actual_parse_tree, _) = crate::parse(src, 0).unwrap();
    std::assert_eq!(actual_parse_tree.0.len(), 1);
    let expected_parse_tree = SourceUnit(vec![SourceUnitPart::TypeDefinition(Box::new(
        TypeDefinition {
            loc: Loc::Source(0, 0, 27),
            name: Identifier {
                loc: Loc::Source(0, 5, 10),
                name: "block".to_string(),
            },
            ty: Type::Array {
                loc: Loc::Source(0, 13, 27),
                element_count: 2,
                element_type: Box::new(Type::Array {
                    loc: Loc::Source(0, 18, 26),
                    element_count: 2,
                    element_type: Box::new(Type::Alias(Identifier {
                        loc: Loc::Source(0, 22, 25),
                        name: "bit".to_string(),
                    })),
                }),
            },
        },
    ))]);
    std::assert_eq!(actual_parse_tree, expected_parse_tree);
}

#[test]
fn parse_variable_annotations() {
    let src = r#"
        #[extern(C), unused, inline(never)]
        let  var1: bit = 1;
        "#;
    let (actual_parse_tree, _) = crate::parse(src, 0).unwrap();
    std::assert_eq!(actual_parse_tree.0.len(), 1);
    let expected_parse_tree = SourceUnit(vec![SourceUnitPart::VariableDefinition(Box::new(
        VariableDefinition {
            loc: Source(0, 9, 71),
            ty: Alias(Identifier {
                loc: Source(0, 64, 67),
                name: "bit".to_string(),
            }),
            attrs: vec![Readable(Source(0, 53, 56))],
            name: Some(Identifier {
                loc: Source(0, 58, 62),
                name: "var1".to_string(),
            }),
            initializer: Some(NumberLiteral(Source(0, 70, 71), 1)),
            annotations: vec![AnnotationDefinition {
                loc: Source(0, 9, 44),
                args: vec![
                    Function {
                        loc: Source(0, 11, 20),
                        name: Identifier {
                            loc: Source(0, 11, 17),
                            name: "extern".to_string(),
                        },
                        args: vec![crate::ast::Annotation::Identifier(
                            Source(0, 18, 19),
                            IdentifierPath {
                                loc: Source(0, 18, 19),
                                identifiers: vec![Identifier {
                                    loc: Source(0, 18, 19),
                                    name: "C".to_string(),
                                }],
                            },
                        )],
                    },
                    crate::ast::Annotation::Identifier(
                        Source(0, 22, 28),
                        IdentifierPath {
                            loc: Source(0, 22, 28),
                            identifiers: vec![Identifier {
                                loc: Source(0, 22, 28),
                                name: "unused".to_string(),
                            }],
                        },
                    ),
                    Function {
                        loc: Source(0, 30, 43),
                        name: Identifier {
                            loc: Source(0, 30, 36),
                            name: "inline".to_string(),
                        },
                        args: vec![crate::ast::Annotation::Identifier(
                            Source(0, 37, 42),
                            IdentifierPath {
                                loc: Source(0, 37, 42),
                                identifiers: vec![Identifier {
                                    loc: Source(0, 37, 42),
                                    name: "never".to_string(),
                                }],
                            },
                        )],
                    },
                ],
                glob: false,
            }],
        },
    ))]);
    std::assert_eq!(actual_parse_tree, expected_parse_tree);
}

#[test]
fn parse_global_annotations() {
    let src = r#"#![fsm(Table)]"#;
    let (actual_parse_tree, _) = crate::parse(src, 0).unwrap();
    std::assert_eq!(actual_parse_tree.0.len(), 1);
    let expected_parse_tree = SourceUnit(vec![SourceUnitPart::AnnotationDefinition(Box::new(
        AnnotationDefinition {
            loc: Source(0, 0, 14),
            args: vec![Annotation::Function {
                loc: Source(0, 3, 13),
                name: Identifier {
                    loc: Source(0, 3, 6),
                    name: "fsm".to_string(),
                },
                args: vec![Annotation::Identifier(
                    Source(0, 7, 12),
                    IdentifierPath {
                        loc: Source(0, 7, 12),
                        identifiers: vec![Identifier {
                            loc: Source(0, 7, 12),
                            name: "Table".to_string(),
                        }],
                    },
                )],
            }],
            glob: true,
        },
    ))]);
    std::assert_eq!(actual_parse_tree, expected_parse_tree);

    let src = r#"#![guard = "timer >= 0"]"#;
    let (actual_parse_tree, _) = crate::parse(src, 0).unwrap();
    std::assert_eq!(actual_parse_tree.0.len(), 1);
    let expected_parse_tree = SourceUnit(vec![SourceUnitPart::AnnotationDefinition(Box::new(
        AnnotationDefinition {
            loc: Source(0, 0, 24),
            args: vec![Annotation::Assign {
                loc: Source(0, 3, 23),
                name: IdentifierPath {
                    loc: Source(0, 3, 8),
                    identifiers: vec![Identifier {
                        loc: Source(0, 3, 8),
                        name: "guard".to_string(),
                    }],
                },
                value: Expression::StringLiteral(vec![StringLiteral {
                    loc: Source(0, 11, 23),
                    unicode: false,
                    string: "timer >= 0".to_string(),
                }]),
            }],
            glob: true,
        },
    ))]);
    std::assert_eq!(actual_parse_tree, expected_parse_tree);
}

#[test]
fn parse_simple_expression() {
    let src = r#"behavior  -> (None | One) & (Three | Mark);"#;
    let (actual_parse_tree, _) = crate::parse(src, 0).unwrap();
    std::assert_eq!(actual_parse_tree.0.len(), 1);
    let expected_parse_tree = SourceUnit(vec![SourceUnitPart::PropertyDefinition(Box::new(
        PropertyDefinition {
            loc: Source(0, 0, 42),
            name: Some(Identifier {
                loc: Source(0, 0, 8),
                name: "behavior".to_string(),
            }),
            value: Some(BitwiseAnd(
                Source(0, 13, 42),
                Box::new(Parenthesis(
                    Source(0, 14, 24),
                    Box::new(BitwiseOr(
                        Source(0, 14, 24),
                        Box::new(Variable(Identifier {
                            loc: Source(0, 14, 18),
                            name: "None".to_string(),
                        })),
                        Box::new(Variable(Identifier {
                            loc: Source(0, 21, 24),
                            name: "One".to_string(),
                        })),
                    )),
                )),
                Box::new(Parenthesis(
                    Source(0, 29, 41),
                    Box::new(BitwiseOr(
                        Source(0, 29, 41),
                        Box::new(Variable(Identifier {
                            loc: Source(0, 29, 34),
                            name: "Three".to_string(),
                        })),
                        Box::new(Variable(Identifier {
                            loc: Source(0, 37, 41),
                            name: "Mark".to_string(),
                        })),
                    )),
                )),
            )),
            annotations: vec![],
        },
    ))]);
    std::assert_eq!(actual_parse_tree, expected_parse_tree);
}
