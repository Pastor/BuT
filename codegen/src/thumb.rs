/// Генератор кода ARM Thumb Assembly.
use but_grammar::ast::{
    Condition, ModelDefinition, ModelPart, Property, SourceUnit, SourceUnitPart, StatePart,
};

use crate::condition::condition_to_c;
use crate::ltl::{extract_ltl_formulas, ltl_comments_thumb};
use crate::CodegenContext;

/// Сгенерировать ассемблер ARM Thumb для модели.
pub fn generate_thumb(model: &ModelDefinition, _ctx: &CodegenContext) -> String {
    let name = model_name(model).to_lowercase();
    let name_up = name.to_uppercase();
    let states = collect_states(model);
    let start = find_start(model).unwrap_or_else(|| states.first().cloned().unwrap_or_default());
    let start_idx = states.iter().position(|s| *s == start).unwrap_or(0);

    let mut out = String::new();
    let ltl_formulas = extract_ltl_formulas(model);
    out.push_str("/* Сгенерировано but-codegen (ARM Thumb) */\n");
    out.push_str(&format!("/* Модель: {} */\n", model_name(model)));
    if !ltl_formulas.is_empty() {
        out.push_str(&ltl_comments_thumb(&ltl_formulas));
    }
    out.push('\n');
    out.push_str(".syntax unified\n");
    out.push_str(".thumb\n");
    out.push_str(".text\n\n");

    // Константы состояний
    for (i, s) in states.iter().enumerate() {
        out.push_str(&format!(
            ".equ {}_STATE_{}, {}\n",
            name_up,
            s.to_uppercase(),
            i
        ));
    }
    out.push('\n');

    // Переменная состояния в BSS
    out.push_str(".section .bss\n");
    out.push_str(&format!("_{}_state:\n", name));
    out.push_str("    .space 4\n\n");

    out.push_str(".section .text\n\n");

    // Функция инициализации
    out.push_str(&format!(".global {}_init\n", name));
    out.push_str(&format!(".thumb_func\n"));
    out.push_str(&format!("{}_init:\n", name));
    out.push_str(&format!("    ldr r0, =_{}_state\n", name));
    out.push_str(&format!("    movs r1, #{}\n", start_idx));
    out.push_str("    str r1, [r0]\n");
    out.push_str("    bx lr\n\n");

    // Функция шага
    out.push_str(&format!(".global {}_step\n", name));
    out.push_str(".thumb_func\n");
    out.push_str(&format!("{}_step:\n", name));
    out.push_str("    push {lr}\n");
    out.push_str(&format!("    ldr r0, =_{}_state\n", name));
    out.push_str("    ldr r1, [r0]  /* r1 = текущее состояние */\n\n");

    // Таблица диспетчеризации состояний
    for (idx, sname) in states.iter().enumerate() {
        out.push_str(&format!(
            "    /* Check state {} (idx={}) */\n",
            sname, idx
        ));
        out.push_str(&format!("    movs r2, #{}\n", idx));
        out.push_str("    cmp r1, r2\n");
        out.push_str(&format!(
            "    beq {}_state_{}\n",
            name,
            sname.to_lowercase()
        ));
    }
    out.push_str(&format!("    b {}_done\n\n", name));

    // Обработчики состояний
    for (idx, sname) in states.iter().enumerate() {
        out.push_str(&format!(
            "{}_state_{}:  /* Состояние: {} */\n",
            name,
            sname.to_lowercase(),
            sname
        ));

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
                        .map(|c| condition_to_c(c))
                        .unwrap_or_else(|| "1".to_string());
                    let target_idx = states
                        .iter()
                        .position(|s| s == &target.name)
                        .unwrap_or(0);

                    out.push_str(&format!("    /* если ({}) → {} */\n", cond_str, target.name));
                    // Load condition-related port/var into r3 if it's a variable
                    if let Some(Condition::Variable(id)) = cond.as_ref() {
                        out.push_str(&format!("    ldr r3, ={}\n", id.name));
                        out.push_str("    ldr r3, [r3]\n");
                    }
                    // Simple condition check (best-effort for common cases)
                    match cond.as_ref() {
                        Some(Condition::Equal(_, _, rhs)) => {
                            if let Condition::NumberLiteral(_, n) = rhs.as_ref() {
                                out.push_str(&format!("    movs r2, #{}\n", n));
                                out.push_str("    cmp r3, r2\n");
                                out.push_str(&format!("    bne {}_state_{}_skip_{}\n", name, sname.to_lowercase(), idx));
                            }
                        }
                        Some(Condition::BoolLiteral(_, true)) | None => {
                            // unconditional: always transition
                        }
                        _ => {
                            out.push_str("    /* сложное условие — вычислите вручную */\n");
                        }
                    }

                    out.push_str(&format!("    movs r1, #{}\n", target_idx));
                    out.push_str(&format!("    ldr r0, =_{}_state\n", name));
                    out.push_str("    str r1, [r0]\n");
                    out.push_str(&format!("    b {}_done\n", name));
                    out.push_str(&format!("{}_state_{}_skip_{}:\n", name, sname.to_lowercase(), idx));
                }
            }
        }

        out.push_str(&format!("    b {}_done\n\n", name));
        let _ = idx;
    }

    out.push_str(&format!("{}_done:\n", name));
    out.push_str("    pop {pc}\n\n");

    out
}

/// Сгенерировать ассемблер ARM Thumb для всех моделей из SourceUnit.
pub fn generate_thumb_all(
    source: &SourceUnit,
    ctx: &CodegenContext,
) -> Vec<(String, String)> {
    let mut result = vec![];
    for part in &source.0 {
        if let SourceUnitPart::ModelDefinition(md) = part {
            let name = model_name(md).to_lowercase();
            let asm = generate_thumb(md, ctx);
            result.push((name, asm));
        }
    }
    result
}

// ── Вспомогательные функции ─────────────────────────────────────────────────────

fn model_name(model: &ModelDefinition) -> String {
    model.name.as_ref().map(|n| n.name.clone()).unwrap_or_else(|| "model".to_string())
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
