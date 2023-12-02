#[cfg(test)]
mod tests {
    use crate::ast::Expression::{BitwiseAnd, BitwiseOr, Parenthesis, Variable};
    use crate::ast::Loc::Source;
    use crate::ast::{Identifier, PropertyDefinition, SourceUnit, SourceUnitPart};

    #[test]
    fn parse_simple_expression() {
        let src = r#"behavior  -> (None | One) & (Three | Mark);"#;
        let (actual_parse_tree, _) = crate::parse(src, 0).unwrap();
        assert_eq!(actual_parse_tree.0.len(), 1);
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
        assert_eq!(actual_parse_tree, expected_parse_tree);
    }
}
