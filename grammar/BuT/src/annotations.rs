#[cfg(test)]
mod tests {
    use crate::ast::Annotation::Function;
    use crate::ast::Annotation::Identifier as AnnotationIdentifier;
    use crate::ast::Expression::NumberLiteral;
    use crate::ast::Loc::Source;
    use crate::ast::Type::Alias;
    use crate::ast::VariableAttribute::Readable;
    use crate::ast::{
        Annotation, AnnotationDefinition, Expression, Identifier, IdentifierPath, SourceUnit,
        SourceUnitPart, StringLiteral, VariableDefinition,
    };

    #[test]
    fn parse_variable_annotations() {
        let src = r#"
        #[extern(C), unused, inline(never)]
        let  var1: bit = 1;
        "#;
        let (actual_parse_tree, _) = crate::parse(src, 0).unwrap();
        assert_eq!(actual_parse_tree.0.len(), 1);
        let expected_parse_tree = SourceUnit(vec![SourceUnitPart::VariableDefinition(Box::new(
            VariableDefinition {
                loc: Source(0, 9, 71),
                ty: Some(Alias(Identifier {
                    loc: Source(0, 64, 67),
                    name: "bit".to_string(),
                })),
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
                            args: vec![AnnotationIdentifier(
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
                        AnnotationIdentifier(
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
                            args: vec![AnnotationIdentifier(
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
        assert_eq!(actual_parse_tree, expected_parse_tree);
    }

    #[test]
    fn parse_global_annotations() {
        let src = r#"#![fsm(Table)]"#;
        let (actual_parse_tree, _) = crate::parse(src, 0).unwrap();
        assert_eq!(actual_parse_tree.0.len(), 1);
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
        assert_eq!(actual_parse_tree, expected_parse_tree);

        let src = r#"#![guard = "timer >= 0"]"#;
        let (actual_parse_tree, _) = crate::parse(src, 0).unwrap();
        assert_eq!(actual_parse_tree.0.len(), 1);
        let expected_parse_tree = SourceUnit(vec![SourceUnitPart::AnnotationDefinition(Box::new(
            AnnotationDefinition {
                loc: Source(0, 0, 24),
                args: vec![Annotation::Assign {
                    loc: Source(0, 3, 23),
                    name: IdentifierPath {
                        loc: Source(0, 3, 23),
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
        assert_eq!(actual_parse_tree, expected_parse_tree);
    }
}
