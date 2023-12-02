#[cfg(test)]
mod tests {
    use crate::ast::Expression::{
        AddressLiteral, Initializer, NumberLiteral, RationalNumberLiteral,
    };
    use crate::ast::Loc::Source;
    use crate::ast::Type::{Alias, Array};
    use crate::ast::VariableAttribute::{Constant, Portable, Readable, Writable};
    use crate::ast::{Identifier, SourceUnit, SourceUnitPart, VariableDefinition};

    #[test]
    fn parse_const_variable() {
        let src = r#"const PI: float = 3.1415;"#;
        let (actual_parse_tree, _) = crate::parse(src, 0).unwrap();
        assert_eq!(actual_parse_tree.0.len(), 1);
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
        assert_eq!(actual_parse_tree, expected_parse_tree);
    }

    #[test]
    fn parse_port_with_address() {
        let src = r#"pio  port5: bit = 0xFFB0:2;"#;
        let (actual_parse_tree, _) = crate::parse(src, 0).unwrap();
        assert_eq!(actual_parse_tree.0.len(), 1);
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
        assert_eq!(actual_parse_tree, expected_parse_tree);

        let src = r#"pio port6: [8: bit] = 0xFFC0; "#;
        let (actual_parse_tree, _) = crate::parse(src, 0).unwrap();
        assert_eq!(actual_parse_tree.0.len(), 1);
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
        assert_eq!(actual_parse_tree, expected_parse_tree);
    }

    #[test]
    fn parse_variable_with_initializer() {
        let src = r#"let mut  var6: [ 3: [3: u8] ] = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}};"#;
        let (actual_parse_tree, _) = crate::parse(src, 0).unwrap();
        assert_eq!(actual_parse_tree.0.len(), 1);
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
        assert_eq!(actual_parse_tree, expected_parse_tree);
    }
}
