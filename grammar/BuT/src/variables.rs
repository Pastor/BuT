#[cfg(test)]
mod tests {
    use crate::ast::{
        Identifier, SourceUnit, SourceUnitPart, VariableDefinition,
    };
    use crate::ast::Expression::{AddressLiteral, NumberLiteral, RationalNumberLiteral};
    use crate::ast::Loc::Source;
    use crate::ast::Type::{Alias, Array};
    use crate::ast::VariableAttribute::{Constant, Portable};

    #[test]
    fn parse_const_variable() {
        let src = r#"const PI = 3.1415;"#;
        let (actual_parse_tree, _) = crate::parse(src, 0).unwrap();
        assert_eq!(actual_parse_tree.0.len(), 1);
        let expected_parse_tree = SourceUnit(vec![SourceUnitPart::VariableDefinition(Box::new(
            VariableDefinition {
                loc: Source(0, 0, 17),
                ty: None,
                attrs: vec![Constant(Source(0, 0, 5))],
                name: Some(Identifier {
                    loc: Source(0, 6, 8),
                    name: "PI".to_string(),
                }),
                initializer: Some(RationalNumberLiteral(
                    Source(0, 11, 17),
                    "3.1415".to_string(),
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
                ty: Some(Alias(Identifier {
                    loc: Source(0, 12, 15),
                    name: "bit".to_string(),
                })),
                attrs: vec![Portable(Source(0, 0, 3))],
                name: Some(Identifier {
                    loc: Source(0, 5, 10),
                    name: "port5".to_string(),
                }),
                initializer: Some(AddressLiteral(
                    Source(0, 18, 26),
                    65456, 2,
                )),
            },
        ))]);
        assert_eq!(actual_parse_tree, expected_parse_tree);

        let src = r#"pio port6: [8: bit] = 0xFFC0; "#;
        let (actual_parse_tree, _) = crate::parse(src, 0).unwrap();
        assert_eq!(actual_parse_tree.0.len(), 1);
        let expected_parse_tree = SourceUnit(vec![SourceUnitPart::VariableDefinition(Box::new(
            VariableDefinition {
                loc: Source(0, 0, 28),
                ty: Some(Array {
                    loc: Source(0, 11, 19),
                    element_count: 8,
                    element_type: Box::new(Alias(Identifier {
                        loc: Source(0, 15, 18),
                        name: "bit".to_string(),
                    })),
                }),
                attrs: vec![Portable(Source(0, 0, 3))],
                name: Some(Identifier { loc: Source(0, 4, 9), name: "port6".to_string() }),
                initializer: Some(NumberLiteral(Source(0, 22, 28), 65472)),
            },
        ))]);
        assert_eq!(actual_parse_tree, expected_parse_tree);
    }
}
