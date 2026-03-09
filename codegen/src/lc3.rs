/// Генератор кода LC3 Assembly.
use but_grammar::ast::{
    Condition, ModelDefinition, ModelPart, Property, SourceUnit, SourceUnitPart, StatePart,
};

use crate::condition::condition_to_c;
use crate::ltl::{extract_ltl_formulas, ltl_comments_asm};
use crate::CodegenContext;

/// Сгенерировать ассемблер LC3 для модели.
pub fn generate_lc3(model: &ModelDefinition, _ctx: &CodegenContext) -> String {
    let name = model_name(model).to_uppercase();
    let states = collect_states(model);
    let start = find_start(model).unwrap_or_else(|| states.first().cloned().unwrap_or_default());
    let start_idx = states.iter().position(|s| *s == start).unwrap_or(0) as i64;

    let mut out = String::new();
    let ltl_formulas = extract_ltl_formulas(model);
    out.push_str("; Сгенерировано but-codegen (ассемблер LC3)\n");
    out.push_str("; Модель: ");
    out.push_str(&model_name(model));
    out.push('\n');
    if !ltl_formulas.is_empty() {
        out.push_str(&ltl_comments_asm(&ltl_formulas));
    }
    out.push_str(".ORIG x3000\n\n");

    // Initialize state
    out.push_str("; Инициализация конечного автомата\n");
    out.push_str("INIT\n");
    out.push_str(&format!("    LD R0, STATE_{}_INIT\n", name));
    out.push_str(&format!("    ST R0, {}_STATE\n\n", name));

    // Main loop
    out.push_str("; Главный цикл выполнения\n");
    out.push_str(&format!("{}_LOOP\n", name));
    out.push_str(&format!("    LD R0, {}_STATE\n", name));

    // State dispatch
    for (idx, sname) in states.iter().enumerate() {
        out.push_str(&format!("    ; Проверка состояния {}\n", sname));
        out.push_str(&format!("    LD R1, CONST_{}\n", idx));
        out.push_str("    NOT R2, R1\n");
        out.push_str("    ADD R2, R2, #1\n");
        out.push_str("    ADD R2, R0, R2\n");
        out.push_str(&format!("    BRz {}_STATE_{}\n", name, sname.to_uppercase()));
    }
    out.push_str(&format!("    BRnzp {}_LOOP\n\n", name));

    // State handlers
    for (idx, sname) in states.iter().enumerate() {
        out.push_str(&format!("; Состояние: {}\n", sname));
        out.push_str(&format!("{}_STATE_{}\n", name, sname.to_uppercase()));

        // Find transitions for this state
        if let Some(ModelPart::StateDefinition(sd)) = model.parts.iter().find(|p| {
            if let ModelPart::StateDefinition(sd) = p {
                sd.name.as_ref().map(|n| n.name.as_str()) == Some(sname.as_str())
            } else {
                false
            }
        }) {
            for sp in &sd.parts {
                if let StatePart::Reference(_, target, cond) = sp {
                    let cond_str = cond
                        .as_ref()
                        .map(|c| format!("; условие: {}", condition_to_c(c)))
                        .unwrap_or_else(|| "; безусловный".to_string());
                    out.push_str(&format!("    {} \n", cond_str));
                    let target_idx = states.iter().position(|s| s == &target.name).unwrap_or(0);
                    out.push_str(&format!("    LD R3, CONST_{}\n", target_idx));
                    out.push_str(&format!("    ST R3, {}_STATE\n", name));
                    out.push_str(&format!("    BRnzp {}_LOOP\n", name));
                }
            }
        }

        let _ = idx;
        out.push_str(&format!("    BRnzp {}_LOOP\n\n", name));
    }

    // Data section
    out.push_str("; Данные\n");
    out.push_str(&format!("{}_STATE .BLKW 1\n", name));
    out.push_str(&format!("STATE_{}_INIT .FILL #{}\n", name, start_idx));

    for (i, _) in states.iter().enumerate() {
        out.push_str(&format!("CONST_{} .FILL #{}\n", i, i));
    }

    out.push_str("\n.END\n");
    out
}

/// Сгенерировать ассемблер LC3 для всех моделей из SourceUnit.
pub fn generate_lc3_all(
    source: &SourceUnit,
    ctx: &CodegenContext,
) -> Vec<(String, String)> {
    let mut result = vec![];
    for part in &source.0 {
        if let SourceUnitPart::ModelDefinition(md) = part {
            let name = model_name(md).to_lowercase();
            let asm = generate_lc3(md, ctx);
            result.push((name, asm));
        }
    }
    result
}

// ── Вспомогательные функции ─────────────────────────────────────────────────────

fn model_name(model: &ModelDefinition) -> String {
    model.name.as_ref().map(|n| n.name.clone()).unwrap_or_else(|| "MODEL".to_string())
}

fn collect_states(model: &ModelDefinition) -> Vec<String> {
    model.parts.iter().filter_map(|p| {
        if let ModelPart::StateDefinition(sd) = p {
            sd.name.as_ref().map(|n| n.name.clone())
        } else {
            None
        }
    }).collect()
}

fn find_start(model: &ModelDefinition) -> Option<String> {
    for part in &model.parts {
        match part {
            ModelPart::PropertyDefinition(pd) => {
                if pd.name.as_ref().map(|n| n.name.as_str()) == Some("start") {
                    if let Property::Expression(but_grammar::ast::Expression::Variable(id)) = &pd.value {
                        return Some(id.name.clone());
                    }
                }
            }
            ModelPart::ConditionDefinition(cd) => {
                if cd.name.as_ref().map(|n| n.name.as_str()) == Some("start") {
                    if let Condition::Variable(id) = &cd.value {
                        return Some(id.name.clone());
                    }
                }
            }
            _ => {}
        }
    }
    None
}
