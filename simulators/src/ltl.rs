//! Utilities for extracting and displaying LTL formulas from the AST.
//!
//! Supports two sources of LTL formulas:
//! 1. Annotations `#![ltl = "formula"]` and `#[ltl = "formula"]`
//! 2. `formula "LTL" { ... }` blocks (function calls inside)

use but_grammar::ast::{
    Annotation, AnnotationDefinition, Expression, FormulaStatement, ModelDefinition, ModelPart,
};

/// Extract all LTL formulas from a model.
///
/// Returns formula strings in their original form.
pub fn extract_ltl_formulas(model: &ModelDefinition) -> Vec<String> {
    let mut result = vec![];

    // Extract from model annotations (#![ltl = "..."] and #[ltl = "..."])
    result.extend(extract_from_annotations(&model.annotations));

    // Extract from annotations inside the model body (ModelPart::AnnotationDefinition)
    for part in &model.parts {
        if let ModelPart::AnnotationDefinition(ann_def) = part {
            result.extend(extract_from_annotations(std::slice::from_ref(ann_def)));
        }
    }

    // Extract from formula { ... } blocks
    for part in &model.parts {
        if let ModelPart::FormulaDefinition(fd) = part {
            for stmt in &fd.formula.statements {
                if let FormulaStatement::FunctionCall(fc) = stmt {
                    let args_str = fc
                        .arguments
                        .iter()
                        .map(formula_expr_to_str)
                        .collect::<Vec<_>>()
                        .join(", ");
                    result.push(format!("{}({})", fc.id.name, args_str));
                }
            }
        }
    }

    result
}

/// Print LTL formulas for a model during simulation.
pub fn print_ltl_info(name: &str, formulas: &[String]) {
    if formulas.is_empty() {
        return;
    }
    println!("[ltl] Specifications for model '{}':", name);
    for f in formulas {
        println!("  {}", f);
    }
}

/// Extract LTL formulas from annotations (#![ltl = "..."], #[ltl = "..."]).
fn extract_from_annotations(annotations: &[AnnotationDefinition]) -> Vec<String> {
    let mut result = vec![];
    for ann_def in annotations {
        for ann in &ann_def.args {
            if let Annotation::Assign { name, value, .. } = ann {
                let attr_name = name
                    .identifiers
                    .last()
                    .map(|id| id.name.as_str())
                    .unwrap_or("");
                if attr_name.eq_ignore_ascii_case("ltl") {
                    if let Expression::StringLiteral(lits) = value {
                        let formula =
                            lits.iter().map(|l| l.string.as_str()).collect::<String>();
                        result.push(formula);
                    }
                }
            }
        }
    }
    result
}

/// Convert a formula expression to a string.
fn formula_expr_to_str(expr: &but_grammar::ast::FormulaExpression) -> String {
    use but_grammar::ast::FormulaExpression;
    match expr {
        FormulaExpression::Variable(id) => id.name.clone(),
        FormulaExpression::BoolLiteral(_, b, _) => b.to_string(),
        FormulaExpression::NumberLiteral(_, n, _) => n.to_string(),
        FormulaExpression::StringLiteral(lit, _) => lit.string.clone(),
        FormulaExpression::FunctionCall(fc) => {
            let args = fc
                .arguments
                .iter()
                .map(formula_expr_to_str)
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}({})", fc.id.name, args)
        }
        _ => "...".to_string(),
    }
}
