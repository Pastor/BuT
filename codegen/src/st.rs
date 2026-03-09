/// Генератор кода Structured Text (IEC 61131-3 / МЭК 61131-3).
use but_grammar::ast::{
    Condition, ModelDefinition, ModelPart, Property, SourceUnit, SourceUnitPart, StatePart,
    VariableAttribute,
};

use crate::condition::{condition_to_c, expr_to_c, type_to_st};
use crate::ltl::{extract_ltl_formulas, ltl_comments_st};
use crate::CodegenContext;

/// Сгенерировать файл объявлений (.FB.DECL.st) для модели.
pub fn generate_st_decl(model: &ModelDefinition, ctx: &CodegenContext) -> String {
    let name = model_name(model).to_uppercase();
    let mut out = String::new();

    let ltl_formulas = extract_ltl_formulas(model);
    out.push_str(&format!("(* Сгенерировано but-codegen *)\n"));
    if !ltl_formulas.is_empty() {
        out.push_str(&ltl_comments_st(&ltl_formulas));
    }
    out.push_str(&format!("FUNCTION_BLOCK {}\n", name));
    out.push_str("VAR\n");
    out.push_str("    state : INT;\n");

    // Переменные портов
    for vd in &ctx.global_vars {
        if vd.attrs.iter().any(|a| matches!(a, VariableAttribute::Portable(_))) {
            if let Some(vname) = &vd.name {
                let ty = type_to_st(&vd.ty);
                let init = vd
                    .initializer
                    .as_ref()
                    .map(|e| expr_to_c(e))
                    .unwrap_or_else(|| "0".to_string());
                let dir = if vd.attrs.iter().any(|a| matches!(a, VariableAttribute::Readable(_))) {
                    "AT %I"
                } else {
                    "AT %Q"
                };
                out.push_str(&format!(
                    "    {} {} : {} := {};\n",
                    vname.name, dir, ty, init
                ));
            }
        }
    }

    // Переменные модели
    for part in &model.parts {
        if let ModelPart::VariableDefinition(vd) = part {
            if vd.attrs.iter().any(|a| matches!(a, VariableAttribute::Portable(_))) {
                continue;
            }
            if let Some(vname) = &vd.name {
                let ty = type_to_st(&vd.ty);
                let init = vd
                    .initializer
                    .as_ref()
                    .map(|e| expr_to_c(e))
                    .unwrap_or_else(|| "0".to_string());
                out.push_str(&format!("    {} : {} := {};\n", vname.name, ty, init));
            }
        }
    }

    out.push_str("END_VAR\n");
    out
}

/// Сгенерировать файл программы (.FB.PRGS.st) для модели.
pub fn generate_st_program(model: &ModelDefinition, _ctx: &CodegenContext) -> String {
    let name = model_name(model).to_uppercase();
    let states = collect_states(model);
    let start = find_start(model).unwrap_or_else(|| states.first().cloned().unwrap_or_default());

    let mut out = String::new();
    out.push_str(&format!("(* Сгенерировано but-codegen *)\n"));
    out.push_str(&format!("(* Программа: {} *)\n\n", name));

    // Константы состояний в виде комментариев
    for (i, s) in states.iter().enumerate() {
        out.push_str(&format!("(* {}_STATE_{} = {} *)\n", name, s.to_uppercase(), i));
    }

    // Найти индекс начального состояния
    let start_idx = states.iter().position(|s| *s == start).unwrap_or(0);
    out.push_str(&format!("\n(* Initialize *)\n"));
    out.push_str(&format!("IF state = 0 AND state < 0 THEN\n"));
    out.push_str(&format!("    state := {};\n", start_idx));
    out.push_str("END_IF;\n\n");

    // Глобальные обработчики enter (каждый такт)
    for part in &model.parts {
        if let ModelPart::PropertyDefinition(pd) = part {
            if pd.name.as_ref().map(|n| n.name.as_str()) == Some("enter") {
                match &pd.value {
                    Property::Expression(e) => {
                        out.push_str(&format!("(* глобальный enter *) {} ;\n", expr_to_c(e)));
                    }
                    Property::Function(_) => {
                        out.push_str("(* глобальный блок enter *)\n");
                    }
                }
            }
        }
    }

    out.push_str("\n(* State machine *)\n");
    out.push_str("CASE state OF\n");

    for (idx, state_name) in states.iter().enumerate() {
        out.push_str(&format!("    {} : (* {} *)\n", idx, state_name));

        // Find state in model
        if let Some(ModelPart::StateDefinition(sd)) = model.parts.iter().find(|p| {
            if let ModelPart::StateDefinition(sd) = p {
                sd.name.as_ref().map(|n| n.name.as_str()) == Some(state_name.as_str())
            } else {
                false
            }
        }) {
            // Обработчики enter
            for sp in &sd.parts {
                if let StatePart::PropertyDefinition(pd) = sp {
                    if pd.name.as_ref().map(|n| n.name.as_str()) == Some("enter") {
                        match &pd.value {
                            Property::Expression(e) => {
                                out.push_str(&format!("        {} ;\n", expr_to_c(e)));
                            }
                            Property::Function(_) => {
                                out.push_str("        (* блок enter *)\n");
                            }
                        }
                    }
                }
            }

            // Переходы
            for sp in &sd.parts {
                if let StatePart::Reference(_, target, cond) = sp {
                    let cond_str = cond
                        .as_ref()
                        .map(|c| condition_to_c(c))
                        .unwrap_or_else(|| "TRUE".to_string());
                    let target_idx = states
                        .iter()
                        .position(|s| s == &target.name)
                        .unwrap_or(states.len()); // unknown state → end state

                    out.push_str(&format!("        IF {} THEN\n", cond_str));

                    // Обработчики exit
                    for sp2 in &sd.parts {
                        if let StatePart::PropertyDefinition(pd2) = sp2 {
                            if pd2.name.as_ref().map(|n| n.name.as_str()) == Some("exit") {
                                match &pd2.value {
                                    Property::Expression(e) => {
                                        out.push_str(&format!("            {} ;\n", expr_to_c(e)));
                                    }
                                    _ => {}
                                }
                            }
                        }
                    }

                    out.push_str(&format!("            state := {};\n", target_idx));
                    out.push_str("        END_IF;\n");
                }
            }
        }

        out.push('\n');
    }

    out.push_str("END_CASE;\n");
    out
}

/// Сгенерировать ST-файлы для всех моделей из SourceUnit.
pub fn generate_st_all(
    source: &SourceUnit,
    ctx: &CodegenContext,
) -> Vec<(String, String, String)> {
    let mut result = vec![];
    for part in &source.0 {
        if let SourceUnitPart::ModelDefinition(md) = part {
            let name = model_name(md).to_uppercase();
            let decl = generate_st_decl(md, ctx);
            let prog = generate_st_program(md, ctx);
            result.push((name, decl, prog));
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
