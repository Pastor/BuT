#[cfg(test)]
mod tests {
    use crate::ast::Loc::Source;
    use crate::ast::{
        Annotation, AnnotationDefinition, Expression, Identifier, SourceUnit, SourceUnitPart,
        StringLiteral,
    };

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
                        Identifier {
                            loc: Source(0, 7, 12),
                            name: "Table".to_string(),
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
                    name: Identifier {
                        loc: Source(0, 3, 8),
                        name: "guard".to_string(),
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
