#[cfg(test)]
mod tests {
    use crate::ast::{Identifier, Loc, SourceUnit, SourceUnitPart, Type, TypeDefinition};

    #[test]
    fn parse_alias_type() {
        let src = r#"type bool  = bit;"#;
        let (actual_parse_tree, _) = crate::parse(src, 0).unwrap();
        assert_eq!(actual_parse_tree.0.len(), 1);
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
        assert_eq!(actual_parse_tree, expected_parse_tree);
    }

    #[test]
    fn parse_simple_array_type() {
        let src = r#"type u8 = [8   : bit];"#;
        let (actual_parse_tree, _) = crate::parse(src, 0).unwrap();
        assert_eq!(actual_parse_tree.0.len(), 1);
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
        assert_eq!(actual_parse_tree, expected_parse_tree);
    }

    #[test]
    fn parse_nested_array_type() {
        let src = r#"type block = [2 : [2: bit]];"#;
        let (actual_parse_tree, _) = crate::parse(src, 0).unwrap();
        assert_eq!(actual_parse_tree.0.len(), 1);
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
        assert_eq!(actual_parse_tree, expected_parse_tree);
    }
}
