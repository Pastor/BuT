/// Генератор кода LC3 Assembly.
use but_grammar::ast::{
    Condition, ModelDefinition, ModelPart, Property, SourceUnit, SourceUnitPart, StatePart,
};

use crate::behavior::{find_behavior, find_end_property, find_terminal_states, BehaviorKind};
use crate::condition::condition_to_c;
use crate::ltl::{extract_ltl_formulas, ltl_comments_asm};
use crate::CodegenContext;

/// Сгенерировать объединённый ассемблер LC3 для всех моделей из SourceUnit.
pub fn generate_lc3_all(source: &SourceUnit, ctx: &CodegenContext) -> String {
    let mut out = String::new();
    out.push_str("; Сгенерировано but-codegen (ассемблер LC3)\n");
    out.push_str("; Все модели объединены в одном файле\n\n");
    out.push_str(".ORIG x3000\n\n");

    let mut has_models = false;
    for part in &source.0 {
        if let SourceUnitPart::ModelDefinition(md) = part {
            out.push_str(&generate_lc3_model_body(md, ctx));
            out.push('\n');
            has_models = true;
        }
    }

    if !has_models {
        out.push_str("; (нет моделей)\n");
    }

    out.push_str("\n.END\n");
    out
}

/// Сгенерировать ассемблер LC3 для одной модели.
pub fn generate_lc3(model: &ModelDefinition, ctx: &CodegenContext) -> String {
    let mut out = String::new();
    out.push_str("; Сгенерировано but-codegen (ассемблер LC3)\n");
    let ltl_formulas = extract_ltl_formulas(model);
    if !ltl_formulas.is_empty() {
        out.push_str(&ltl_comments_asm(&ltl_formulas));
    }
    out.push_str(".ORIG x3000\n\n");
    out.push_str(&generate_lc3_model_body(model, ctx));
    out.push_str("\n.END\n");
    out
}

// ── Внутренняя генерация тела одной модели ────────────────────────────────────────

fn generate_lc3_model_body(model: &ModelDefinition, ctx: &CodegenContext) -> String {
    let ltl_formulas = extract_ltl_formulas(model);
    if let Some(bk) = find_behavior(model) {
        generate_lc3_behavior(model, &bk, ctx, &ltl_formulas)
    } else {
        generate_lc3_fsm(model, ctx, &ltl_formulas)
    }
}

fn generate_lc3_fsm(
    model: &ModelDefinition,
    _ctx: &CodegenContext,
    ltl_formulas: &[String],
) -> String {
    let name = model_name(model).to_uppercase();
    let states = collect_states(model);
    let start = find_start(model).unwrap_or_else(|| states.first().cloned().unwrap_or_default());
    let start_idx = states.iter().position(|s| *s == start).unwrap_or(0) as i64;
    let terminal_states = find_terminal_states(model);
    let end_prop = find_end_property(model);

    let mut out = String::new();
    out.push_str(&format!("; === Модель: {} ===\n", name));
    if !ltl_formulas.is_empty() {
        out.push_str(&ltl_comments_asm(ltl_formulas));
    }

    // Инициализация
    out.push_str(&format!("; Инициализация: {}\n", name));
    out.push_str(&format!("{}_INIT\n", name));
    out.push_str(&format!("    LD R0, {}_STATE_INIT\n", name));
    out.push_str(&format!("    ST R0, {}_STATE\n\n", name));

    // is_done флаг инициализация
    if !terminal_states.is_empty() {
        out.push_str(&format!("    AND R0, R0, #0\n"));
        out.push_str(&format!("    ST R0, {}_DONE\n\n", name));
    }

    // Главный цикл
    out.push_str(&format!("; Главный цикл: {}\n", name));
    out.push_str(&format!("{}_LOOP\n", name));
    out.push_str(&format!("    LD R0, {}_STATE\n", name));

    for (idx, sname) in states.iter().enumerate() {
        out.push_str(&format!("    ; Проверка состояния {}\n", sname));
        out.push_str(&format!("    LD R1, {}_CONST_{}\n", name, idx));
        out.push_str("    NOT R2, R1\n");
        out.push_str("    ADD R2, R2, #1\n");
        out.push_str("    ADD R2, R0, R2\n");
        out.push_str(&format!("    BRz {}_STATE_{}\n", name, sname.to_uppercase()));
    }
    out.push_str(&format!("    BRnzp {}_LOOP\n\n", name));

    // Обработчики состояний
    for (idx, sname) in states.iter().enumerate() {
        let is_terminal = terminal_states.iter().any(|t| t == sname);
        out.push_str(&format!("; Состояние: {} (idx={})\n", sname, idx));
        out.push_str(&format!("{}_STATE_{}\n", name, sname.to_uppercase()));

        if let Some(ModelPart::StateDefinition(sd)) = model.parts.iter().find(|p| {
            if let ModelPart::StateDefinition(sd) = p {
                sd.name.as_ref().map(|n| n.name.as_str()) == Some(sname.as_str())
            } else {
                false
            }
        }) {
            for sp in &sd.parts {
                if let StatePart::Reference(_, target, cond) = sp {
                    let cond_comment = cond
                        .as_ref()
                        .map(|c| format!("условие: {}", condition_to_c(c)))
                        .unwrap_or_else(|| "безусловный".to_string());
                    out.push_str(&format!("    ; {}\n", cond_comment));
                    let target_idx = states.iter().position(|s| s == &target.name).unwrap_or(0);
                    out.push_str(&format!("    LD R3, {}_CONST_{}\n", name, target_idx));
                    out.push_str(&format!("    ST R3, {}_STATE\n", name));
                    out.push_str(&format!("    BRnzp {}_LOOP\n", name));
                }
            }
        }

        // Терминальное состояние: is_done = 1 + end handler
        if is_terminal {
            out.push_str(&format!("    ; Терминальное состояние — установка флага done\n"));
            out.push_str(&format!("    AND R0, R0, #0\n"));
            out.push_str(&format!("    ADD R0, R0, #1\n"));
            out.push_str(&format!("    ST R0, {}_DONE\n", name));
            if end_prop.is_some() {
                out.push_str(&format!("    ; end handler — см. {}_END_HANDLER\n", name));
                out.push_str(&format!("    JSR {}_END_HANDLER\n", name));
            }
        }

        out.push_str(&format!("    BRnzp {}_LOOP\n\n", name));
    }

    // End handler subroutine
    if let Some(ep) = end_prop {
        out.push_str(&format!("; Подпрограмма end-обработчика: {}\n", name));
        out.push_str(&format!("{}_END_HANDLER\n", name));
        match ep {
            Property::Expression(e) => {
                out.push_str(&format!(
                    "    ; end: {}\n",
                    crate::condition::expr_to_c(e)
                ));
            }
            Property::Function(_) => {
                out.push_str("    ; end block (реализуйте вручную)\n");
            }
        }
        out.push_str("    RET\n\n");
    }

    // Данные
    out.push_str(&format!("; Данные: {}\n", name));
    out.push_str(&format!("{}_STATE .BLKW 1\n", name));
    out.push_str(&format!("{}_STATE_INIT .FILL #{}\n", name, start_idx));
    if !terminal_states.is_empty() {
        out.push_str(&format!("{}_DONE .BLKW 1\n", name));
    }
    for (i, _) in states.iter().enumerate() {
        out.push_str(&format!("{}_CONST_{} .FILL #{}\n", name, i, i));
    }
    out
}

fn generate_lc3_behavior(
    model: &ModelDefinition,
    bk: &BehaviorKind,
    _ctx: &CodegenContext,
    ltl_formulas: &[String],
) -> String {
    let name = model_name(model).to_uppercase();
    let models = bk.models();
    let end_prop = find_end_property(model);
    let mut out = String::new();

    out.push_str(&format!("; === Компоновочная модель: {} ===\n", name));
    if !ltl_formulas.is_empty() {
        out.push_str(&ltl_comments_asm(ltl_formulas));
    }

    let kind_str = match bk {
        BehaviorKind::Sequential(_) => "sequential",
        BehaviorKind::Parallel(_) => "parallel",
        BehaviorKind::Choice(_) => "choice",
    };
    out.push_str(&format!("; Тип компоновки: {} ({})\n", kind_str, models.join(", ")));
    out.push_str(&format!("; Подмодели: {}\n\n", models.join(", ")));

    // Инициализация
    out.push_str(&format!("{}_COMP_INIT\n", name));
    for m in models {
        out.push_str(&format!("    JSR {}_INIT\n", m.to_uppercase()));
    }
    out.push_str(&format!("    AND R0, R0, #0\n"));
    out.push_str(&format!("    ST R0, {}_PHASE\n\n", name));

    // Шаг
    out.push_str(&format!("{}_COMP_STEP\n", name));
    match bk {
        BehaviorKind::Sequential(ms) => {
            for (i, m) in ms.iter().enumerate() {
                out.push_str(&format!(
                    "    ; Фаза {}: {}\n    LD R0, {}_PHASE\n    LD R1, {}_CONST_{}\n    NOT R2, R1\n    ADD R2, R2, #1\n    ADD R2, R0, R2\n    BRz {}_DO_{}\n",
                    i, m, name, name, i, name, m.to_uppercase()
                ));
            }
            for (i, m) in ms.iter().enumerate() {
                out.push_str(&format!("{}_DO_{}\n", name, m.to_uppercase()));
                out.push_str(&format!("    JSR {}_STEP\n", m.to_uppercase()));
                out.push_str(&format!("    LD R0, {}_DONE\n", m.to_uppercase()));
                out.push_str(&format!("    BRz {}_STEP_DONE\n", name));
                let next_phase = i + 1;
                out.push_str(&format!("    LD R0, {}_CONST_{}\n", name, next_phase));
                out.push_str(&format!("    ST R0, {}_PHASE\n", name));
                if i + 1 == ms.len() {
                    if end_prop.is_some() {
                        out.push_str(&format!("    JSR {}_END_HANDLER\n", name));
                    }
                }
                out.push_str(&format!("    BRnzp {}_STEP_DONE\n", name));
            }
        }
        BehaviorKind::Parallel(ms) => {
            for m in ms {
                out.push_str(&format!("    LD R0, {}_DONE\n", m.to_uppercase()));
                out.push_str(&format!("    BRnz {}_SKIP_{}\n", name, m.to_uppercase()));
                out.push_str(&format!("    JSR {}_STEP\n", m.to_uppercase()));
                out.push_str(&format!("{}_SKIP_{}\n", name, m.to_uppercase()));
            }
            if end_prop.is_some() {
                out.push_str(&format!("    ; Проверка завершения всех подмоделей\n"));
                for m in ms {
                    out.push_str(&format!("    LD R0, {}_DONE\n    BRz {}_STEP_DONE\n", m.to_uppercase(), name));
                }
                out.push_str(&format!("    JSR {}_END_HANDLER\n", name));
            }
        }
        BehaviorKind::Choice(ms) => {
            if let Some(m) = ms.first() {
                out.push_str(&format!("    JSR {}_STEP\n", m.to_uppercase()));
                out.push_str(&format!("    LD R0, {}_DONE\n", m.to_uppercase()));
                out.push_str(&format!("    BRz {}_STEP_DONE\n", name));
                if end_prop.is_some() {
                    out.push_str(&format!("    JSR {}_END_HANDLER\n", name));
                }
            }
        }
    }
    out.push_str(&format!("{}_STEP_DONE\n    RET\n\n", name));

    // End handler
    if let Some(ep) = end_prop {
        out.push_str(&format!("; End-обработчик: {}\n", name));
        out.push_str(&format!("{}_END_HANDLER\n", name));
        match ep {
            Property::Expression(e) => {
                out.push_str(&format!(
                    "    ; end: {}\n",
                    crate::condition::expr_to_c(e)
                ));
            }
            Property::Function(_) => {
                out.push_str("    ; end block (реализуйте вручную)\n");
            }
        }
        out.push_str("    RET\n\n");
    }

    // Данные
    out.push_str(&format!("; Данные: {}\n", name));
    out.push_str(&format!("{}_PHASE .BLKW 1\n", name));
    for (i, _) in models.iter().enumerate() {
        out.push_str(&format!("{}_CONST_{} .FILL #{}\n", name, i, i));
    }
    out.push_str(&format!(
        "{}_CONST_{} .FILL #{}\n",
        name,
        models.len(),
        models.len()
    ));
    out
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
