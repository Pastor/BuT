#[cfg(test)]
mod tests {
    use crate::ast::Expression::RationalNumberLiteral;
    use crate::ast::VariableAttribute::Constant;
    use crate::ast::{
        Identifier, Loc, SourceUnit, SourceUnitPart, Type, TypeDefinition, VariableDefinition,
    };

    #[test]
    fn parse_const_variable() {
        let src = r#"const PI = 3.1415;"#;
        let (actual_parse_tree, _) = crate::parse(src, 0).unwrap();
        assert_eq!(actual_parse_tree.0.len(), 1);
        let expected_parse_tree = SourceUnit(vec![SourceUnitPart::VariableDefinition(Box::new(
            VariableDefinition {
                loc: Loc::Source(0, 0, 17),
                ty: None,
                attrs: vec![Constant(Loc::Source(0, 0, 5))],
                name: Some(Identifier {
                    loc: Loc::Source(0, 6, 8),
                    name: "PI".to_string(),
                }),
                initializer: Some(RationalNumberLiteral(
                    Loc::Source(0, 11, 17),
                    "3".to_string(),
                    "1415".to_string(),
                    "".to_string(),
                    None,
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
                loc: Loc::Source(0, 0, 17),
                ty: None,
                attrs: vec![Constant(Loc::Source(0, 0, 5))],
                name: Some(Identifier {
                    loc: Loc::Source(0, 6, 8),
                    name: "PI".to_string(),
                }),
                initializer: Some(RationalNumberLiteral(
                    Loc::Source(0, 11, 17),
                    "3".to_string(),
                    "1415".to_string(),
                    "".to_string(),
                    None,
                )),
            },
        ))]);
        assert_eq!(actual_parse_tree, expected_parse_tree);
    }
}
