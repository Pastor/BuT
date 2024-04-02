use std::sync::mpsc;
use std::time::Duration;
use std::{fs, path::Path, thread};

use pretty_assertions::assert_eq;
use walkdir::WalkDir;

use crate::ast::Annotation::Function;
use crate::ast::Expression::{
    Add, AddressLiteral, BitwiseAnd, BitwiseOr, Cast, Initializer, NumberLiteral, Parenthesis,
    RationalNumberLiteral, Variable,
};
use crate::ast::FormulaExpression::BoolLiteral;
use crate::ast::FormulaStatement::FunctionCall;
use crate::ast::Statement::{Block, Formula, Return};
use crate::ast::Type::{Alias, Array};
use crate::ast::VariableAttribute::{Constant, Portable, Readable, Writable};
use crate::ast::*;
use crate::diagnostics::{Diagnostic, ErrorType::ParserError, Level::Error};
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
                Diagnostic {
                    loc: Source(0, 17, 21),
                    level: Error,
                    ty: ParserError,
                    message: "'frum' found where 'from' expected".to_string(),
                    notes: vec![],
                },
                Diagnostic {
                    loc: Source(0, 56, 58),
                    level: Error,
                    ty: ParserError,
                    message: "unrecognised token 'if', expected \"#\", \"const\", \"external\", \"let\", \"mut\", \"port\", \"private\", \"}\", identifier".to_string(),
                    notes: vec![],
                },
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
fn parse_function_formula() {
    let src = r#"
        #[unused]
        fn with_formula(input1: u8, input2: bit) -> u16 {
            formula "LTL" {
            }
            formula {
                None(true)
                None(false)
                Always(input1)
            }
            return (input1 + input2) as u16;
        }
        "#;

    let (actual_parse_tree, _) = crate::parse(src, 0).unwrap();
    assert_eq!(actual_parse_tree.0.len(), 1);
    let expected_parse_tree = SourceUnit(vec![SourceUnitPart::FunctionDefinition(Box::new(
        FunctionDefinition {
            loc: Source(0, 9, 74),
            ty: FunctionTy::Function,
            name: Some(Identifier {
                loc: Source(0, 30, 42),
                name: "with_formula".to_string(),
            }),
            name_loc: Source(0, 30, 42),
            params: vec![
                (
                    Source(0, 43, 53),
                    Some(Parameter {
                        loc: Source(0, 43, 53),
                        annotation: None,
                        ty: Variable(Identifier {
                            loc: Source(0, 51, 53),
                            name: "u8".to_string(),
                        }),
                        storage: None,
                        name: Some(Identifier {
                            loc: Source(0, 43, 49),
                            name: "input1".to_string(),
                        }),
                    }),
                ),
                (
                    Source(0, 55, 66),
                    Some(Parameter {
                        loc: Source(0, 55, 66),
                        annotation: None,
                        ty: Variable(Identifier {
                            loc: Source(0, 63, 66),
                            name: "bit".to_string(),
                        }),
                        storage: None,
                        name: Some(Identifier {
                            loc: Source(0, 55, 61),
                            name: "input2".to_string(),
                        }),
                    }),
                ),
            ],
            annotations: vec![AnnotationDefinition {
                loc: Source(0, 9, 18),
                args: vec![Annotation::Identifier(
                    Source(0, 11, 17),
                    IdentifierPath {
                        loc: Source(0, 11, 17),
                        identifiers: vec![Identifier {
                            loc: Source(0, 11, 17),
                            name: "unused".to_string(),
                        }],
                    },
                )],
                glob: false,
            }],
            attributes: vec![],
            return_type: Some(Alias(Identifier {
                loc: Source(0, 71, 74),
                name: "u16".to_string(),
            })),
            body: Some(Block {
                loc: Source(0, 75, 295),
                unchecked: false,
                statements: vec![
                    Formula {
                        loc: Source(0, 89, 118),
                        dialect: Some(StringLiteral {
                            loc: Source(0, 97, 102),
                            unicode: false,
                            string: "LTL".to_string(),
                        }),
                        flags: None,
                        block: Box::new(FormulaBlock {
                            loc: Source(0, 103, 118),
                            statements: vec![],
                        }),
                    },
                    Formula {
                        loc: Source(0, 131, 240),
                        dialect: None,
                        flags: None,
                        block: Box::new(FormulaBlock {
                            loc: Source(0, 139, 240),
                            statements: vec![
                                FunctionCall(Box::new(FormulaFunctionCall {
                                    loc: Source(0, 157, 167),
                                    id: Identifier {
                                        loc: Source(0, 157, 161),
                                        name: "None".to_string(),
                                    },
                                    arguments: vec![BoolLiteral(Source(0, 162, 166), true, None)],
                                })),
                                FunctionCall(Box::new(FormulaFunctionCall {
                                    loc: Source(0, 184, 195),
                                    id: Identifier {
                                        loc: Source(0, 184, 188),
                                        name: "None".to_string(),
                                    },
                                    arguments: vec![BoolLiteral(Source(0, 189, 194), false, None)],
                                })),
                                FunctionCall(Box::new(FormulaFunctionCall {
                                    loc: Source(0, 212, 226),
                                    id: Identifier {
                                        loc: Source(0, 212, 218),
                                        name: "Always".to_string(),
                                    },
                                    arguments: vec![FormulaExpression::Variable(Identifier {
                                        loc: Source(0, 219, 225),
                                        name: "input1".to_string(),
                                    })],
                                })),
                            ],
                        }),
                    },
                    Return(
                        Source(0, 253, 284),
                        Some(Cast(
                            Source(0, 260, 284),
                            Box::new(Parenthesis(
                                Source(0, 261, 276),
                                Box::new(Add(
                                    Source(0, 261, 276),
                                    Box::new(Variable(Identifier {
                                        loc: Source(0, 261, 267),
                                        name: "input1".to_string(),
                                    })),
                                    Box::new(Variable(Identifier {
                                        loc: Source(0, 270, 276),
                                        name: "input2".to_string(),
                                    })),
                                )),
                            )),
                            Alias(Identifier {
                                loc: Source(0, 281, 284),
                                name: "u16".to_string(),
                            }),
                        )),
                    ),
                ],
            }),
        },
    ))]);
    std::assert_eq!(actual_parse_tree, expected_parse_tree);
}

#[test]
fn parse_struct() {
    let src = r#"
        #[repr(C)]
        struct None {
        }
        "#;

    let (actual_parse_tree, _) = crate::parse(src, 0).unwrap();
    assert_eq!(actual_parse_tree.0.len(), 1);
}

#[test]
fn parse_enum() {
    let src = r#"
        #[repr(C)]
        enum None {
           First, Second
        }
        "#;

    let (actual_parse_tree, _) = crate::parse(src, 0).unwrap();
    assert_eq!(actual_parse_tree.0.len(), 1);
}

#[test]
fn parse_function() {
    let src = r#"
        #[extern(C)]
        #[unused]
        fn none(args: str) {
            return none("");
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
    let examples_tests =
        WalkDir::new(Path::new(env!("CARGO_MANIFEST_DIR")).join("../tests_data/BuT/examples"))
            .into_iter()
            .collect::<Result<Vec<_>, _>>()
            .unwrap()
            .into_iter()
            .map(|entry| (true, entry));

    let errors = semantic_tests
        .into_iter()
        .chain(syntax_tests)
        .chain(examples_tests)
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
            let src = source_part.to_string();
            let result = match timeout_after(Duration::from_secs(5), move || {
                crate::parse(&source_part, 0)
            }) {
                Ok(result) => result,
                Err(err) => return Some(format!("{path:?}: \n\t{err}")),
            };

            if let (Err(err), false) = (
                result.map_err(|diags| {
                    format!(
                        "{:?}:\n\t{}\n\t{}",
                        path,
                        diags
                            .iter()
                            .map(|diag| format!("{diag:?}"))
                            .collect::<Vec<_>>()
                            .join("\n\t"),
                        diags
                            .iter()
                            .map(|d| {
                                let loc = d.loc;
                                let mut chars = src.chars();
                                let mut start_line = loc.start();
                                let mut end_line = loc.end();
                                'position: loop {
                                    if start_line <= 0 {
                                        start_line = 0;
                                        break;
                                    }
                                    if end_line >= src.len() {
                                        end_line = src.len() - 1;
                                        break;
                                    }
                                    let option = chars.nth(start_line);
                                    match option {
                                        None => break 'position,
                                        Some(c) if c == '\n' => {
                                            start_line += 1;
                                            break 'position;
                                        }
                                        _ => {}
                                    }

                                    let option = chars.nth(end_line);
                                    match option {
                                        None => break 'position,
                                        Some(c) if c == '\n' => {
                                            end_line -= 1;
                                            break 'position;
                                        }
                                        _ => {}
                                    }
                                    start_line -= 1;
                                    end_line += 1;
                                }
                                format!("{}", &src[start_line..end_line])
                            })
                            .collect::<Vec<_>>()
                            .join("\n\t"),
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
    let src = r#"port  port5: bit = 0xFFB0:2;"#;
    let (actual_parse_tree, _) = crate::parse(src, 0).unwrap();
    std::assert_eq!(actual_parse_tree.0.len(), 1);
    let expected_parse_tree = SourceUnit(vec![SourceUnitPart::VariableDefinition(Box::new(
        VariableDefinition {
            loc: Source(0, 0, 27),
            ty: Alias(Identifier {
                loc: Source(0, 13, 16),
                name: "bit".to_string(),
            }),
            annotations: vec![],
            attrs: vec![Portable(Source(0, 0, 4))],
            name: Some(Identifier {
                loc: Source(0, 6, 11),
                name: "port5".to_string(),
            }),
            initializer: Some(AddressLiteral(Source(0, 19, 27), 65456, 2)),
        },
    ))]);
    std::assert_eq!(actual_parse_tree, expected_parse_tree);

    let src = r#"port port6: [8: bit] = 0xFFC0; "#;
    let (actual_parse_tree, _) = crate::parse(src, 0).unwrap();
    std::assert_eq!(actual_parse_tree.0.len(), 1);
    let expected_parse_tree = SourceUnit(vec![SourceUnitPart::VariableDefinition(Box::new(
        VariableDefinition {
            loc: Source(0, 0, 29),
            ty: Array {
                loc: Source(0, 12, 20),
                element_count: 8,
                element_type: Box::new(Alias(Identifier {
                    loc: Source(0, 16, 19),
                    name: "bit".to_string(),
                })),
            },
            annotations: vec![],
            attrs: vec![Portable(Source(0, 0, 4))],
            name: Some(Identifier {
                loc: Source(0, 5, 10),
                name: "port6".to_string(),
            }),
            initializer: Some(NumberLiteral(Source(0, 23, 29), 65472)),
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
            loc: Source(0, 0, 16),
            name: Identifier {
                loc: Source(0, 5, 9),
                name: "bool".to_string(),
            },
            ty: Alias(Identifier {
                loc: Source(0, 13, 16),
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
            loc: Source(0, 0, 21),
            name: Identifier {
                loc: Source(0, 5, 7),
                name: "u8".to_string(),
            },
            ty: Array {
                loc: Source(0, 10, 21),
                element_count: 8,
                element_type: Box::new(Alias(Identifier {
                    loc: Source(0, 17, 20),
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
            loc: Source(0, 0, 27),
            name: Identifier {
                loc: Source(0, 5, 10),
                name: "block".to_string(),
            },
            ty: Array {
                loc: Source(0, 13, 27),
                element_count: 2,
                element_type: Box::new(Array {
                    loc: Source(0, 18, 26),
                    element_count: 2,
                    element_type: Box::new(Alias(Identifier {
                        loc: Source(0, 22, 25),
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
                        args: vec![Annotation::Identifier(
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
                    Annotation::Identifier(
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
                        args: vec![Annotation::Identifier(
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
            args: vec![Function {
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
            loc: Source(0, 0, 43),
            name: Some(Identifier {
                loc: Source(0, 0, 8),
                name: "behavior".to_string(),
            }),
            value: Property::Expression(BitwiseAnd(
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
