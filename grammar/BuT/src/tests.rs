use std::{fs, path::Path, thread};
use std::sync::mpsc;
use std::time::Duration;

use pretty_assertions::assert_eq;
use walkdir::WalkDir;

use crate::ast::*;
use crate::but;
use crate::diagnostics::{Diagnostic, ErrorType::ParserError, Level::Error};
use crate::lexer::Lexer;
use crate::Loc::Source;

#[test]
fn parser_error_recovery() {
    let src = r#"import * as sesa frum "sesa";
pragma sesa_pragma;
usingg sesa for *;
contract 9c {
    uint256 0sesa_glb = 90;
    9uint256 sesa_glb = 90;
    uint256 sesa_glb = 90id;

    event 1sesa_event(uint 0invalid_param_id);
    event sesa_event(3uint invalid_param_type);

    error 1sesa_error(uint 0invalid_param_id);
    error sesa_error(3uint invalid_param_type);

    struct 2sesa_struct {
        uint256 3sesa_struct_mem;
    }

    function 4sesa_func() public! pure {
        uint 3sesa_var = 3sesa_id + id;
        9uint sesa= 4b;
        if (true)
    }
}
"#;

    if let Err(errors) = crate::parse(src, 0) {
        assert_eq!(
            errors,
            vec![
                Diagnostic { loc: Source(0, 17, 21), level: Error, ty: ParserError, message: "'frum' found where 'from' expected".to_string(), notes: vec![] },
                Diagnostic { loc: Source(0, 48, 49), level: Error, ty: ParserError, message: r#"unrecognised token ';', expected string"#.to_string(), notes: vec![] },
                Diagnostic { loc: Source(0, 62, 65), level: Error, ty: ParserError, message: r#"unrecognised token 'for', expected "(", ";", "=""#.to_string(), notes: vec![] },
                Diagnostic { loc: Source(0, 78, 79), level: Error, ty: ParserError, message: r#"unrecognised token '9', expected "case", "default", "leave", "revert", "switch", identifier"#.to_string(), notes: vec![] },
                Diagnostic { loc: Source(0, 95, 96), level: Error, ty: ParserError, message: "unrecognised token '0', expected \"(\", \"++\", \"--\", \".\", \"[\", \"case\", \"constant\", \"default\", \"external\", \"immutable\", \"internal\", \"leave\", \"override\", \"private\", \"public\", \"revert\", \"switch\", \"{\", identifier".to_string(), notes: vec![] },
                Diagnostic { loc: Source(0, 116, 123), level: Error, ty: ParserError, message: "unrecognised token 'uint256', expected \"++\", \"--\", \".\", \"[\", \"case\", \"default\", \"leave\", \"switch\", identifier".to_string(), notes: vec![] },
                Diagnostic { loc: Source(0, 403, 404), level: Error, ty: ParserError, message: "unrecognised token '3', expected \"(\", \"++\", \"--\", \".\", \"[\", \"case\", \"constant\", \"default\", \"external\", \"immutable\", \"internal\", \"leave\", \"override\", \"private\", \"public\", \"revert\", \"switch\", \"{\", identifier".to_string(), notes: vec![] },
                Diagnostic { loc: Source(0, 441, 442), level: Error, ty: ParserError, message: r#"unrecognised token '4', expected "(", "case", "default", "leave", "revert", "switch", identifier"#.to_string(), notes: vec![] },
                Diagnostic { loc: Source(0, 460, 461), level: Error, ty: ParserError, message: "unrecognised token '!', expected \";\", \"case\", \"constant\", \"default\", \"external\", \"immutable\", \"internal\", \"leave\", \"override\", \"payable\", \"private\", \"public\", \"pure\", \"return\", \"returns\", \"revert\", \"switch\", \"view\", \"virtual\", \"{\", identifier".to_string(), notes: vec![] },
                Diagnostic { loc: Source(0, 482, 483), level: Error, ty: ParserError, message: "unrecognised token '3', expected \"!=\", \"%\", \"%=\", \"&\", \"&&\", \"&=\", \"(\", \"*\", \"**\", \"*=\", \"+\", \"++\", \"+=\", \"-\", \"--\", \"-=\", \".\", \"/\", \"/=\", \";\", \"<\", \"<<\", \"<<=\", \"<=\", \"=\", \"==\", \">\", \">=\", \">>\", \">>=\", \"?\", \"[\", \"^\", \"^=\", \"calldata\", \"case\", \"default\", \"leave\", \"memory\", \"revert\", \"storage\", \"switch\", \"{\", \"|\", \"|=\", \"||\", identifier".to_string(), notes: vec![] },
                Diagnostic { loc: Source(0, 518, 522), level: Error, ty: ParserError, message: "unrecognised token 'uint256', expected \"!=\", \"%\", \"%=\", \"&\", \"&&\", \"&=\", \"*\", \"**\", \"*=\", \"+\", \"++\", \"+=\", \"-\", \"--\", \"-=\", \".\", \"/\", \"/=\", \";\", \"<\", \"<<\", \"<<=\", \"<=\", \"=\", \"==\", \">\", \">=\", \">>\", \">>=\", \"?\", \"[\", \"^\", \"^=\", \"case\", \"default\", \"leave\", \"switch\", \"|\", \"|=\", \"||\", identifier".to_string(), notes: vec![] },
                Diagnostic { loc: Source(0, 555, 556), level: Error, ty: ParserError, message: "unrecognised token '}', expected \"!\", \"(\", \"+\", \"++\", \"-\", \"--\", \"[\", \"address\", \"assembly\", \"bool\", \"break\", \"byte\", \"bytes\", \"case\", \"continue\", \"default\", \"delete\", \"do\", \"emit\", \"false\", \"for\", \"function\", \"if\", \"leave\", \"mapping\", \"new\", \"payable\", \"return\", \"revert\", \"string\", \"switch\", \"true\", \"try\", \"type\", \"unchecked\", \"while\", \"{\", \"~\", Bytes, Int, Uint, address, hexnumber, hexstring, identifier, number, rational, string".to_string(), notes: vec![] },
                Diagnostic { loc: Source(0, 557, 558), level: Error, ty: ParserError, message: "unrecognised token '}', expected \"(\", \";\", \"[\", \"abstract\", \"address\", \"bool\", \"byte\", \"bytes\", \"case\", \"contract\", \"default\", \"enum\", \"event\", \"false\", \"function\", \"import\", \"interface\", \"leave\", \"library\", \"mapping\", \"payable\", \"pragma\", \"string\", \"struct\", \"switch\", \"true\", \"type\", \"using\", Bytes, Int, Uint, address, annotation, hexnumber, hexstring, identifier, number, rational, string".to_string(), notes: vec![] },
            ]
        )
    }
}

#[test]
fn parse_test() {
    let src = r#"/// @title Foo
                /// @description Foo
                /// Bar
                contract foo {
                    /**
                    @title Jurisdiction
                    */
                    /// @author Anon
                    /**
                    @description Data for
                    jurisdiction
                    @dev It's a struct
                    */
                    struct Jurisdiction {
                        bool exists;
                        uint keyIdx;
                        bytes2 country;
                        bytes32 region;
                    }
                    string __abba_$;
                    int64 $thing_102;
                }

                function bar() {
                    try sum(1, 1) returns (uint sum) {
                        assert(sum == 2);
                    } catch (bytes memory b) {
                        revert(unicode'très');
                    } catch Error(string memory error) {
                        revert(error);
                    } catch Panic(uint x) {
                        revert('feh');
                    }
                }"#;

    let mut comments = Vec::new();
    let mut errors = Vec::new();
    let lex = Lexer::new(src, 0, &mut comments, &mut errors);

    let my_errs = &mut Vec::new();
    let actual_parse_tree = but::SourceUnitParser::new()
        .parse(src, 0, my_errs, lex)
        .unwrap();
    let expected_parse_tree = SourceUnit(vec![SourceUnitPart::FunctionDefinition(Box::new(
        FunctionDefinition {
            loc: Source(0, 720, 735),
            ty: FunctionTy::Function,
            name: Some(Identifier {
                loc: Source(0, 729, 732),
                name: "bar".to_string(),
            }),
            name_loc: Source(0, 729, 732),
            params: vec![],
            attributes: vec![],
            return_not_returns: None,
            returns: vec![],
            body: Some(Statement::Block {
                loc: Source(0, 735, 1147),
                unchecked: false,
                statements: vec![Statement::Try(
                    Source(0, 757, 1129),
                    Expression::FunctionCall(
                        Source(0, 761, 770),
                        Box::new(Expression::Variable(Identifier {
                            loc: Source(0, 761, 764),
                            name: "sum".to_string(),
                        })),
                        vec![
                            Expression::NumberLiteral(
                                Source(0, 765, 766),
                                1,
                            ),
                            Expression::NumberLiteral(
                                Source(0, 768, 769),
                                1,
                            ),
                        ],
                    ),
                    Some((
                        vec![(
                            Source(0, 780, 788),
                            Some(Parameter {
                                loc: Source(0, 780, 788),
                                ty: Expression::Type(Source(0, 780, 784), Type::Uint(256)),
                                storage: None,
                                name: Some(Identifier {
                                    loc: Source(0, 785, 788),
                                    name: "sum".to_string(),
                                }),
                                annotation: None,
                            }),
                        )],
                        Box::new(Statement::Block {
                            loc: Source(0, 790, 855),
                            unchecked: false,
                            statements: vec![Statement::Expression(
                                Source(0, 816, 832),
                                Expression::FunctionCall(
                                    Source(0, 816, 832),
                                    Box::new(Expression::Variable(Identifier {
                                        loc: Source(0, 816, 822),
                                        name: "assert".to_string(),
                                    })),
                                    vec![Expression::Equal(
                                        Source(0, 823, 831),
                                        Box::new(Expression::Variable(Identifier {
                                            loc: Source(0, 823, 826),
                                            name: "sum".to_string(),
                                        })),
                                        Box::new(Expression::NumberLiteral(
                                            Source(0, 830, 831),
                                            2,
                                        )),
                                    )],
                                ),
                            )],
                        }),
                    )),
                    vec![
                        CatchClause::Simple(
                            Source(0, 856, 950),
                            Some(Parameter {
                                loc: Source(0, 863, 877),
                                ty: Expression::Type(Source(0, 863, 868), Type::DynamicBytes),
                                storage: Some(StorageLocation::Memory(Source(0, 869, 875))),
                                name: Some(Identifier {
                                    loc: Source(0, 876, 877),
                                    name: "b".to_string(),
                                }),
                                annotation: None,
                            }),
                            Statement::Block {
                                loc: Source(0, 879, 950),
                                unchecked: false,
                                statements: vec![Statement::Revert(
                                    Source(0, 905, 927),
                                    None,
                                    vec![Expression::StringLiteral(vec![StringLiteral {
                                        loc: Source(0, 912, 926),
                                        unicode: true,
                                        string: "très".to_string(),
                                    }])],
                                )],
                            },
                        ),
                        CatchClause::Named(
                            Source(0, 951, 1046),
                            Identifier {
                                loc: Source(0, 957, 962),
                                name: "Error".to_string(),
                            },
                            Parameter {
                                loc: Source(0, 963, 982),
                                ty: Expression::Type(Source(0, 963, 969), Type::String),
                                storage: Some(StorageLocation::Memory(Source(0, 970, 976))),
                                name: Some(Identifier {
                                    loc: Source(0, 977, 982),
                                    name: "error".to_string(),
                                }),
                                annotation: None,
                            },
                            Statement::Block {
                                loc: Source(0, 984, 1046),
                                unchecked: false,
                                statements: vec![Statement::Revert(
                                    Source(0, 1010, 1023),
                                    None,
                                    vec![Expression::Variable(Identifier {
                                        loc: Source(0, 1017, 1022),
                                        name: "error".to_string(),
                                    })],
                                )],
                            },
                        ),
                        CatchClause::Named(
                            Source(0, 1047, 1129),
                            Identifier {
                                loc: Source(0, 1053, 1058),
                                name: "Panic".to_string(),
                            },
                            Parameter {
                                loc: Source(0, 1059, 1065),
                                ty: Expression::Type(Source(0, 1059, 1063), Type::Uint(256)),
                                storage: None,
                                name: Some(Identifier {
                                    loc: Source(0, 1064, 1065),
                                    name: "x".to_string(),
                                }),
                                annotation: None,
                            },
                            Statement::Block {
                                loc: Source(0, 1067, 1129),
                                unchecked: false,
                                statements: vec![Statement::Revert(
                                    Source(0, 1093, 1106),
                                    None,
                                    vec![Expression::StringLiteral(vec![StringLiteral {
                                        loc: Source(0, 1100, 1105),
                                        unicode: false,
                                        string: "feh".to_string(),
                                    }])],
                                )],
                            },
                        ),
                    ],
                )],
            }),
        },
    ))]);

    assert_eq!(actual_parse_tree, expected_parse_tree);

    assert_eq!(
        comments,
        vec![
            Comment::DocLine(
                Source(
                    0,
                    0,
                    14,
                ),
                "/// @title Foo".to_string(),
            ),
            Comment::DocLine(
                Source(
                    0,
                    31,
                    51,
                ),
                "/// @description Foo".to_string(),
            ),
            Comment::DocLine(
                Source(
                    0,
                    68,
                    75,
                ),
                "/// Bar".to_string(),
            ),
            Comment::DocBlock(
                Source(
                    0,
                    127,
                    193,
                ),
                "/**\n                    @title Jurisdiction\n                    */".to_string(),
            ),
            Comment::DocLine(
                Source(
                    0,
                    214,
                    230,
                ),
                "/// @author Anon".to_string(),
            ),
            Comment::DocBlock(
                Source(
                    0,
                    251,
                    391,
                ),
                "/**\n                    @description Data for\n                    jurisdiction\n                    @dev It's a struct\n                    */".to_string(),
            ),
        ]
    );
}

#[test]
fn parse_error_test() {
    let src = r#"

        error Outer(uint256 available, uint256 required);

        contract TestToken {
            error NotPending();
            /// Insufficient balance for transfer. Needed `required` but only
            /// `available` available.
            /// @param available balance available.
            /// @param required requested amount to transfer.
            error InsufficientBalance(uint256 available, uint256 required);
        }
        "#;

    let (actual_parse_tree, comments) = crate::parse(src, 0).unwrap();
    assert_eq!(actual_parse_tree.0.len(), 2);

    let expected_parse_tree = SourceUnit(vec![SourceUnitPart::ErrorDefinition(Box::new(
        ErrorDefinition {
            loc: Source(0, 10, 58),
            keyword: Expression::Variable(Identifier {
                loc: Source(0, 10, 15),
                name: "error".to_string(),
            }),
            name: Some(Identifier {
                loc: Source(0, 16, 21),
                name: "Outer".to_string(),
            }),
            fields: vec![
                ErrorParameter {
                    ty: Expression::Type(Source(0, 22, 29), Type::Uint(256)),
                    loc: Source(0, 22, 39),
                    name: Some(Identifier {
                        loc: Source(0, 30, 39),
                        name: "available".to_string(),
                    }),
                },
                ErrorParameter {
                    ty: Expression::Type(Source(0, 41, 48), Type::Uint(256)),
                    loc: Source(0, 41, 57),
                    name: Some(Identifier {
                        loc: Source(0, 49, 57),
                        name: "required".to_string(),
                    }),
                },
            ],
        },
    ))]);

    assert_eq!(actual_parse_tree, expected_parse_tree);

    assert_eq!(
        comments,
        vec![
            Comment::DocLine(
                Source(0, 134, 199),
                "/// Insufficient balance for transfer. Needed `required` but only".to_owned(),
            ),
            Comment::DocLine(
                Source(0, 212, 238),
                "/// `available` available.".to_owned(),
            ),
            Comment::DocLine(
                Source(0, 251, 290),
                "/// @param available balance available.".to_owned(),
            ),
            Comment::DocLine(
                Source(0, 303, 352),
                "/// @param required requested amount to transfer.".to_owned(),
            ),
        ]
    );
}

#[test]
fn test_assembly_parser() {
    let src = r#"
                function bar() {
                    assembly "evmasm" {
                        let x := 0
                        for { let i := 0 } lt(i, 0x100) { i := add(i, 0x20) } {
                            x := /* meh */ add(x, mload(i))

                            if gt(i, 0x10) {
                                break
                            }
                        }

                        let h : u32, y, z : u16 := funcCall()

                        switch x
                        case 0 {
                            revert(0, 0)
                            // feh
                        }
                        default {
                            leave
                        }
                    }

                    assembly {

                        function power(base : u256, exponent) -> result
                        {
                            let y := and("abc":u32, add(3:u256, 2:u256))
                            let result
                        }
                    }
                }"#;

    let mut comments = Vec::new();
    let mut errors = Vec::new();
    let lex = Lexer::new(src, 0, &mut comments, &mut errors);

    let my_errs = &mut Vec::new();
    let actual_parse_tree = but::SourceUnitParser::new()
        .parse(src, 0, my_errs, lex)
        .unwrap();

    let expected_parse_tree = SourceUnit(vec![SourceUnitPart::FunctionDefinition(Box::new(
        FunctionDefinition {
            loc: Source(0, 17, 32),
            ty: FunctionTy::Function,
            name: Some(Identifier {
                loc: Source(0, 26, 29),
                name: "bar".to_string(),
            }),
            name_loc: Source(0, 26, 29),
            params: vec![],
            attributes: vec![],
            return_not_returns: None,
            returns: vec![],
            body: Some(Statement::Block {
                loc: Source(0, 32, 1045),
                unchecked: false,
                statements: vec![
                    Statement::Assembly {
                        loc: Source(0, 54, 736),
                        block: YulBlock {
                            loc: Source(0, 72, 736),
                            statements: vec![
                                YulStatement::VariableDeclaration(
                                    Source(0, 98, 108),
                                    vec![YulTypedIdentifier {
                                        loc: Source(0, 102, 103),
                                        id: Identifier {
                                            loc: Source(0, 102, 103),
                                            name: "x".to_string(),
                                        },
                                        ty: None,
                                    }],
                                    Some(YulExpression::NumberLiteral(
                                        Source(0, 107, 108),
                                        "0".to_string(), "".to_string(),
                                        None,
                                    )),
                                ),
                                YulStatement::For(YulFor {
                                    loc: Source(0, 133, 388),
                                    init_block: YulBlock {
                                        loc: Source(0, 137, 151),
                                        statements: vec![YulStatement::VariableDeclaration(
                                            Source(0, 139, 149),
                                            vec![YulTypedIdentifier {
                                                loc: Source(0, 143, 144),
                                                id: Identifier {
                                                    loc: Source(0, 143, 144),
                                                    name: "i".to_string(),
                                                },
                                                ty: None,
                                            }],
                                            Some(YulExpression::NumberLiteral(
                                                Source(0, 148, 149),
                                                "0".to_string(), "".to_string(),
                                                None,
                                            )),
                                        )],
                                    },
                                    condition: YulExpression::FunctionCall(Box::new(YulFunctionCall {
                                        loc: Source(0, 152, 164),
                                        id: Identifier {
                                            loc: Source(0, 152, 154),
                                            name: "lt".to_string(),
                                        },
                                        arguments: vec![
                                            YulExpression::Variable(Identifier {
                                                loc: Source(0, 155, 156),
                                                name: "i".to_string(),
                                            }),
                                            YulExpression::HexNumberLiteral(
                                                Source(0, 158, 163),
                                                "0x100".to_string(),
                                                None,
                                            ),
                                        ],
                                    })),
                                    post_block: YulBlock {
                                        loc: Source(0, 165, 186),
                                        statements: vec![YulStatement::Assign(
                                            Source(0, 167, 184),
                                            vec![YulExpression::Variable(Identifier {
                                                loc: Source(0, 167, 168),
                                                name: "i".to_string(),
                                            })],
                                            YulExpression::FunctionCall(Box::new(
                                                YulFunctionCall {
                                                    loc: Source(0, 172, 184),
                                                    id: Identifier {
                                                        loc: Source(0, 172, 175),
                                                        name: "add".to_string(),
                                                    },
                                                    arguments: vec![
                                                        YulExpression::Variable(Identifier {
                                                            loc: Source(0, 176, 177),
                                                            name: "i".to_string(),
                                                        }),
                                                        YulExpression::HexNumberLiteral(
                                                            Source(0, 179, 183),
                                                            "0x20".to_string(),
                                                            None,
                                                        ),
                                                    ],
                                                },
                                            )),
                                        )],
                                    },
                                    execution_block: YulBlock {
                                        loc: Source(0, 187, 388),
                                        statements: vec![
                                            YulStatement::Assign(
                                                Source(0, 217, 248),
                                                vec![YulExpression::Variable(Identifier {
                                                    loc: Source(0, 217, 218),
                                                    name: "x".to_string(),
                                                })],
                                                YulExpression::FunctionCall(Box::new(
                                                    YulFunctionCall {
                                                        loc: Source(0, 232, 248),
                                                        id: Identifier {
                                                            loc: Source(0, 232, 235),
                                                            name: "add".to_string(),
                                                        },
                                                        arguments: vec![
                                                            YulExpression::Variable(Identifier {
                                                                loc: Source(0, 236, 237),
                                                                name: "x".to_string(),
                                                            }),
                                                            YulExpression::FunctionCall(Box::new(
                                                                YulFunctionCall {
                                                                    loc: Source(0, 239, 247),
                                                                    id: Identifier {
                                                                        loc: Source(0, 239, 244),
                                                                        name: "mload".to_string(),
                                                                    },
                                                                    arguments: vec![
                                                                        YulExpression::Variable(
                                                                            Identifier {
                                                                                loc: Source(0, 245, 246),
                                                                                name: "i".to_string(),
                                                                            },
                                                                        ),
                                                                    ],
                                                                },
                                                            )),
                                                        ],
                                                    },
                                                )),
                                            ),
                                            YulStatement::If(
                                                Source(0, 278, 362),
                                                YulExpression::FunctionCall(Box::new(
                                                    YulFunctionCall {
                                                        loc: Source(0, 281, 292),
                                                        id: Identifier {
                                                            loc: Source(0, 281, 283),
                                                            name: "gt".to_string(),
                                                        },
                                                        arguments: vec![
                                                            YulExpression::Variable(Identifier {
                                                                loc: Source(0, 284, 285),
                                                                name: "i".to_string(),
                                                            }),
                                                            YulExpression::HexNumberLiteral(
                                                                Source(0, 287, 291),
                                                                "0x10".to_string(),
                                                                None,
                                                            ),
                                                        ],
                                                    },
                                                )),
                                                YulBlock {
                                                    loc: Source(0, 293, 362),
                                                    statements: vec![YulStatement::Break(Source(0, 327, 332))],
                                                },
                                            ),
                                        ],
                                    },
                                }),
                                YulStatement::VariableDeclaration(
                                    Source(0, 414, 451),
                                    vec![
                                        YulTypedIdentifier {
                                            loc: Source(0, 418, 425),
                                            id: Identifier {
                                                loc: Source(0, 418, 419),
                                                name: "h".to_string(),
                                            },
                                            ty: Some(Identifier {
                                                loc: Source(0, 422, 425),
                                                name: "u32".to_string(),
                                            }),
                                        },
                                        YulTypedIdentifier {
                                            loc: Source(0, 427, 428),
                                            id: Identifier {
                                                loc: Source(0, 427, 428),
                                                name: "y".to_string(),
                                            },
                                            ty: None,
                                        },
                                        YulTypedIdentifier {
                                            loc: Source(0, 430, 437),
                                            id: Identifier {
                                                loc: Source(0, 430, 431),
                                                name: "z".to_string(),
                                            },
                                            ty: Some(Identifier {
                                                loc: Source(0, 434, 437),
                                                name: "u16".to_string(),
                                            }),
                                        },
                                    ],
                                    Some(YulExpression::FunctionCall(Box::new(
                                        YulFunctionCall {
                                            loc: Source(0, 441, 451),
                                            id: Identifier {
                                                loc: Source(0, 441, 449),
                                                name: "funcCall".to_string(),
                                            },
                                            arguments: vec![],
                                        },
                                    ))),
                                ),
                                YulStatement::Switch(YulSwitch {
                                    loc: Source(0, 477, 714),
                                    condition: YulExpression::Variable(Identifier {
                                        loc: Source(0, 484, 485),
                                        name: "x".to_string(),
                                    }),
                                    cases: vec![YulSwitchOptions::Case(
                                        Source(0, 510, 620),
                                        YulExpression::NumberLiteral(
                                            Source(0, 515, 516),
                                            "0".to_string(), "".to_string(),
                                            None,
                                        ),
                                        YulBlock {
                                            loc: Source(0, 517, 620),
                                            statements: vec![YulStatement::FunctionCall(Box::new(
                                                YulFunctionCall {
                                                    loc: Source(0, 547, 559),
                                                    id: Identifier {
                                                        loc: Source(0, 547, 553),
                                                        name: "revert".to_string(),
                                                    },
                                                    arguments: vec![
                                                        YulExpression::NumberLiteral(
                                                            Source(0, 554, 555),
                                                            "0".to_string(), "".to_string(),
                                                            None,
                                                        ),
                                                        YulExpression::NumberLiteral(
                                                            Source(0, 557, 558),
                                                            "0".to_string(), "".to_string(),
                                                            None,
                                                        ),
                                                    ],
                                                },
                                            ))],
                                        },
                                    )],
                                    default: Some(YulSwitchOptions::Default(
                                        Source(0, 645, 714),
                                        YulBlock {
                                            loc: Source(0, 653, 714),
                                            statements: vec![YulStatement::Leave(Source(0, 683, 688))],
                                        },
                                    )),
                                }),
                            ],
                        },
                        dialect: Some(StringLiteral {
                            loc: Source(0, 63, 71),
                            unicode: false,
                            string: "evmasm".to_string(),
                        }),
                        flags: None,
                    },
                    Statement::Assembly {
                        loc: Source(0, 758, 1027),
                        block: YulBlock {
                            loc: Source(0, 767, 1027),
                            statements: vec![YulStatement::FunctionDefinition(Box::new(
                                YulFunctionDefinition {
                                    loc: Source(0, 794, 1005),
                                    id: Identifier {
                                        loc: Source(0, 803, 808),
                                        name: "power".to_string(),
                                    },
                                    params: vec![
                                        YulTypedIdentifier {
                                            loc: Source(0, 809, 820),
                                            id: Identifier {
                                                loc: Source(0, 809, 813),
                                                name: "base".to_string(),
                                            },
                                            ty: Some(Identifier {
                                                loc: Source(0, 816, 820),
                                                name: "u256".to_string(),
                                            }),
                                        },
                                        YulTypedIdentifier {
                                            loc: Source(0, 822, 830),
                                            id: Identifier {
                                                loc: Source(0, 822, 830),
                                                name: "exponent".to_string(),
                                            },
                                            ty: None,
                                        },
                                    ],
                                    returns: vec![YulTypedIdentifier {
                                        loc: Source(0, 835, 841),
                                        id: Identifier {
                                            loc: Source(0, 835, 841),
                                            name: "result".to_string(),
                                        },
                                        ty: None,
                                    }],
                                    body: YulBlock {
                                        loc: Source(0, 866, 1005),
                                        statements: vec![
                                            YulStatement::VariableDeclaration(
                                                Source(0, 896, 940),
                                                vec![YulTypedIdentifier {
                                                    loc: Source(0, 900, 901),
                                                    id: Identifier {
                                                        loc: Source(0, 900, 901),
                                                        name: "y".to_string(),
                                                    },
                                                    ty: None,
                                                }],
                                                Some(YulExpression::FunctionCall(Box::new(
                                                    YulFunctionCall {
                                                        loc: Source(0, 905, 940),
                                                        id: Identifier {
                                                            loc: Source(0, 905, 908),
                                                            name: "and".to_string(),
                                                        },
                                                        arguments: vec![
                                                            YulExpression::StringLiteral(
                                                                StringLiteral {
                                                                    loc: Source(0, 909, 914),
                                                                    unicode: false,
                                                                    string: "abc".to_string(),
                                                                },
                                                                Some(Identifier {
                                                                    loc: Source(0, 915, 918),
                                                                    name: "u32".to_string(),
                                                                }),
                                                            ),
                                                            YulExpression::FunctionCall(Box::new(
                                                                YulFunctionCall {
                                                                    loc: Source(0, 920, 939),
                                                                    id: Identifier {
                                                                        loc: Source(0, 920, 923),
                                                                        name: "add".to_string(),
                                                                    },
                                                                    arguments: vec![
                                                                        YulExpression::NumberLiteral(
                                                                            Source(0, 924, 930),
                                                                            "3".to_string(), "".to_string(),
                                                                            Some(Identifier {
                                                                                loc: Source(0, 926, 930),
                                                                                name: "u256".to_string(),
                                                                            }),
                                                                        ),
                                                                        YulExpression::NumberLiteral(
                                                                            Source(0, 932, 938),
                                                                            "2".to_string(), "".to_string(),
                                                                            Some(Identifier {
                                                                                loc: Source(0, 934, 938),
                                                                                name: "u256".to_string(),
                                                                            }),
                                                                        ),
                                                                    ],
                                                                },
                                                            )),
                                                        ],
                                                    },
                                                ))),
                                            ),
                                            YulStatement::VariableDeclaration(
                                                Source(0, 969, 979),
                                                vec![YulTypedIdentifier {
                                                    loc: Source(0, 973, 979),
                                                    id: Identifier {
                                                        loc: Source(0, 973, 979),
                                                        name: "result".to_string(),
                                                    },
                                                    ty: None,
                                                }],
                                                None,
                                            ),
                                        ],
                                    },
                                },
                            ))],
                        },
                        dialect: None,
                        flags: None,
                    },
                ],
            }),
        },
    ))]);

    assert_eq!(expected_parse_tree, actual_parse_tree);

    assert_eq!(
        comments,
        vec![
            Comment::Block(Source(0, 222, 231), "/* meh */".to_string()),
            Comment::Line(Source(0, 588, 594), "// feh".to_string()),
        ]
    );
}

#[test]
fn parse_byte_function_assembly() {
    let src = r#"
    contract ECDSA {
        function tryRecover() internal pure {
            assembly {
                v := byte(0, mload(add(signature, 0x60)))
            }
        }
    }
        "#;

    let (actual_parse_tree, _) = crate::parse(src, 0).unwrap();
    assert_eq!(actual_parse_tree.0.len(), 1);
}

#[test]
fn parse_user_defined_value_type() {
    let src = r#"
        type Uint256 is uint256;
        contract TestToken {
            type Bytes32 is bytes32;
        }
        "#;

    let (actual_parse_tree, _) = crate::parse(src, 0).unwrap();
    assert_eq!(actual_parse_tree.0.len(), 2);

    let expected_parse_tree =
        SourceUnit(vec![SourceUnitPart::TypeDefinition(Box::new(
            TypeDefinition {
                loc: Source(0, 9, 32),
                name: Identifier {
                    loc: Source(0, 14, 21),
                    name: "Uint256".to_string(),
                },
                ty: Type::Uint(256),
            },
        ))]);

    assert_eq!(actual_parse_tree, expected_parse_tree);
}

#[test]
fn parse_no_parameters_yul_function() {
    let src = r#"
contract C {
	function testing() pure public {
		assembly {
			function multiple() -> ret1, ret2 {
				ret1 := 1
				ret2 := 2
			}
		}
	}
}
    "#;

    let (actual_parse_tree, _) = crate::parse(src, 0).unwrap();
    assert_eq!(actual_parse_tree.0.len(), 1);
}

#[test]
fn parse_random_doc_comment() {
    let src = r#"
int  /** x */ constant /** x */ y/** dev:  */ = /** x */1 /** x */ + /** x */2/** x */;
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

    let errors =
        semantic_tests
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
fn parameter_annotation() {
    let src = r#"
contract MyTest {
    constructor(@seed bytes mySeed) {}
}
    "#;

    // let (actual_parse_tree, _) = crate::parse(src, 0).unwrap();
    //
    // let expected_tree = SourceUnit(vec![SourceUnitPart::ContractDefinition(
    //     ContractDefinition {
    //         loc: Source(0, 1, 59),
    //         ty: ContractTy::Contract(Source(0, 1, 9)),
    //         name: Some(Identifier {
    //             loc: Source(0, 10, 16),
    //             name: "MyTest".to_string(),
    //         }),
    //         base: vec![],
    //         parts: vec![ContractPart::FunctionDefinition(
    //             FunctionDefinition {
    //                 loc: Source(0, 23, 55),
    //                 ty: FunctionTy::Constructor,
    //                 name: None,
    //                 name_loc: Source(0, 34, 34),
    //                 params: (vec![(
    //                     Source(0, 35, 53),
    //                     Some(Parameter {
    //                         loc: Source(0, 35, 53),
    //                         ty: Expression::Type(Source(0, 41, 46), Type::DynamicBytes),
    //                         storage: None,
    //                         name: Some(Identifier {
    //                             loc: Source(0, 47, 53),
    //                             name: "mySeed".to_string(),
    //                         }),
    //                         annotation: Some(Annotation {
    //                             loc: Source(0, 35, 40),
    //                             id: Identifier {
    //                                 loc: Source(0, 35, 40),
    //                                 name: "seed".to_string(),
    //                             },
    //                             value: None,
    //                         }),
    //                     }),
    //                 )]),
    //                 attributes: vec![],
    //                 return_not_returns: None,
    //                 returns: vec![],
    //                 body: Some(Statement::Block {
    //                     loc: Source(0, 55, 57),
    //                     unchecked: false,
    //                     statements: vec![],
    //                 }),
    //             }
    //             .into(),
    //         )],
    //     }
    //     .into(),
    // )]);
    //
    // assert_eq!(expected_tree, actual_parse_tree);
}
