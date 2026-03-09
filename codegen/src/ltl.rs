//! Утилиты для извлечения LTL-формул из AST и встраивания в генерируемый код.
//!
//! Поддерживает два источника LTL-формул:
//! 1. Аннотации `#![ltl = "формула"]` и `#[ltl = "формула"]`
//! 2. Блоки `formula "LTL" { ... }` (функциональные вызовы внутри)

use but_grammar::ast::{
    Annotation, AnnotationDefinition, Expression, FormulaStatement, ModelDefinition, ModelPart,
};

/// Извлечь все LTL-формулы из модели.
pub fn extract_ltl_formulas(model: &ModelDefinition) -> Vec<String> {
    let mut result = vec![];

    // Аннотации модели (#![ltl = "..."] / #[ltl = "..."])
    result.extend(extract_from_annotations(&model.annotations));

    // Аннотации внутри тела модели (ModelPart::AnnotationDefinition)
    for part in &model.parts {
        if let ModelPart::AnnotationDefinition(ann_def) = part {
            result.extend(extract_from_annotations(std::slice::from_ref(ann_def)));
        }
    }

    // Блоки formula { ... }
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

/// Сгенерировать блок LTL-комментариев в стиле C (/* ... */).
pub fn ltl_comments_c(formulas: &[String]) -> String {
    if formulas.is_empty() {
        return String::new();
    }
    let mut out = String::from("/* LTL-спецификации:\n");
    for f in formulas {
        out.push_str(&format!(" *   {}\n", f));
    }
    out.push_str(" */\n");
    out
}

/// Сгенерировать блок LTL-комментариев в стиле Verilog (// ...).
pub fn ltl_comments_verilog(formulas: &[String]) -> String {
    if formulas.is_empty() {
        return String::new();
    }
    let mut out = String::from("// LTL-спецификации:\n");
    for f in formulas {
        out.push_str(&format!("//   {}\n", f));
    }
    out
}

/// Сгенерировать блок LTL-комментариев в стиле ST ((* ... *)).
pub fn ltl_comments_st(formulas: &[String]) -> String {
    if formulas.is_empty() {
        return String::new();
    }
    let mut out = String::from("(* LTL-спецификации:\n");
    for f in formulas {
        out.push_str(&format!(" *   {}\n", f));
    }
    out.push_str(" *)\n");
    out
}

/// Сгенерировать блок LTL-комментариев в стиле ассемблера (; ...).
pub fn ltl_comments_asm(formulas: &[String]) -> String {
    if formulas.is_empty() {
        return String::new();
    }
    let mut out = String::from("; LTL-спецификации:\n");
    for f in formulas {
        out.push_str(&format!(";   {}\n", f));
    }
    out
}

/// Сгенерировать блок LTL-комментариев в стиле C (/* ... */) для ARM Thumb.
pub fn ltl_comments_thumb(formulas: &[String]) -> String {
    if formulas.is_empty() {
        return String::new();
    }
    let mut out = String::from("/* LTL-спецификации:\n");
    for f in formulas {
        out.push_str(&format!(" *   {}\n", f));
    }
    out.push_str(" */\n");
    out
}

/// Извлечь LTL-формулы из аннотаций.
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

/// Преобразовать выражение формулы в строку.
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
