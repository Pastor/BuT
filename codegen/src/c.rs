use but_grammar::ast::{
    Condition, ModelDefinition, ModelPart, Property, SourceUnit, SourceUnitPart,
    StatePart, VariableAttribute,
};

use crate::condition::{condition_to_c, stmt_to_c, type_to_c};
use crate::ltl::{extract_ltl_formulas, ltl_comments_c};
use crate::CodegenContext;

/// Сгенерировать заголовочный C-файл для модели.
pub fn generate_c_header(model: &ModelDefinition, ctx: &CodegenContext) -> String {
    let name = model_name(model);
    let upper = name.to_uppercase();

    let mut out = String::new();
    out.push_str(&format!("#ifndef __{}_H__\n", upper));
    out.push_str(&format!("#define __{}_H__\n\n", upper));
    out.push_str("#include <stdint.h>\n\n");

    // LTL-спецификации в виде комментария
    let ltl_formulas = extract_ltl_formulas(model);
    if !ltl_formulas.is_empty() {
        out.push_str(&ltl_comments_c(&ltl_formulas));
        out.push('\n');
    }

    // Перечисление состояний
    out.push_str(&format!("typedef enum {{\n"));
    let states = collect_states(model);
    for (i, s) in states.iter().enumerate() {
        let comma = if i + 1 < states.len() { "," } else { "" };
        out.push_str(&format!("    {}_{}{}\n", upper, s.to_uppercase(), comma));
    }
    out.push_str(&format!("}} {}_State_t;\n\n", name));

    // Внешние объявления портов
    for vd in &ctx.global_vars {
        if vd.attrs.iter().any(|a| matches!(a, VariableAttribute::Portable(_))) {
            if let Some(vname) = &vd.name {
                let ty = type_to_c(&vd.ty);
                out.push_str(&format!("extern {} {};\n", ty, vname.name));
            }
        }
    }

    out.push_str("\n");
    out.push_str(&format!("void {}_init(void);\n", name));
    out.push_str(&format!("void {}_step(void);\n", name));
    out.push_str(&format!("{}_State_t {}_state(void);\n\n", name, name));

    out.push_str(&format!("#endif /* __{}_H__ */\n", upper));
    out
}

/// Сгенерировать исходный C-файл для модели.
pub fn generate_c_source(model: &ModelDefinition, ctx: &CodegenContext) -> String {
    let name = model_name(model);
    let upper = name.to_uppercase();

    let mut out = String::new();
    out.push_str(&format!("#include \"{}.h\"\n\n", name.to_lowercase()));

    // LTL-спецификации в виде комментария
    let ltl_formulas = extract_ltl_formulas(model);
    if !ltl_formulas.is_empty() {
        out.push_str(&ltl_comments_c(&ltl_formulas));
        out.push('\n');
    }

    // Переменная состояния
    out.push_str(&format!("static {}_State_t _state;\n\n", name));

    // Переменные портов
    for vd in &ctx.global_vars {
        if vd.attrs.iter().any(|a| matches!(a, VariableAttribute::Portable(_))) {
            if let Some(vname) = &vd.name {
                let ty = type_to_c(&vd.ty);
                let init = vd
                    .initializer
                    .as_ref()
                    .map(crate::condition::expr_to_c)
                    .unwrap_or_else(|| "0".to_string());
                out.push_str(&format!("{} {} = {};\n", ty, vname.name, init));
            }
        }
    }

    // Переменные модели
    for part in &model.parts {
        if let ModelPart::VariableDefinition(vd) = part {
            if vd.attrs.iter().any(|a| matches!(a, VariableAttribute::Portable(_))) {
                continue; // ports handled above
            }
            if let Some(vname) = &vd.name {
                let ty = type_to_c(&vd.ty);
                let init = vd
                    .initializer
                    .as_ref()
                    .map(crate::condition::expr_to_c)
                    .unwrap_or_else(|| "0".to_string());
                out.push_str(&format!("static {} {} = {};\n", ty, vname.name, init));
            }
        }
    }
    out.push('\n');

    // Найти начальное состояние
    let start = find_start(model).unwrap_or_default();

    // Функция инициализации
    out.push_str(&format!("void {}_init(void) {{\n", name));
    out.push_str(&format!("    _state = {}_{};\n", upper, start.to_uppercase()));
    // Обработчик enter уровня модели
    for part in &model.parts {
        if let ModelPart::PropertyDefinition(pd) = part {
            if pd.name.as_ref().map(|n| n.name.as_str()) == Some("enter") {
                let stmt = match &pd.value {
                    Property::Function(s) => stmt_to_c(s, 4),
                    Property::Expression(e) => format!("    {};\n", crate::condition::expr_to_c(e)),
                };
                out.push_str(&stmt);
            }
        }
    }
    out.push_str("}\n\n");

    // Запрос состояния
    out.push_str(&format!("{}_State_t {}_state(void) {{\n", name, name));
    out.push_str("    return _state;\n");
    out.push_str("}\n\n");

    // Функция шага
    out.push_str(&format!("void {}_step(void) {{\n", name));

    // Enter уровня модели (каждый такт)
    for part in &model.parts {
        if let ModelPart::PropertyDefinition(pd) = part {
            if pd.name.as_ref().map(|n| n.name.as_str()) == Some("enter") {
                let stmt = match &pd.value {
                    Property::Function(s) => stmt_to_c(s, 4),
                    Property::Expression(e) => format!("    {};\n", crate::condition::expr_to_c(e)),
                };
                out.push_str(&stmt);
            }
        }
    }

    out.push_str(&format!("    switch (_state) {{\n"));

    for part in &model.parts {
        if let ModelPart::StateDefinition(sd) = part {
            let sname = sd.name.as_ref().map(|n| n.name.as_str()).unwrap_or("?");

            out.push_str(&format!(
                "        case {}_{}: {{\n",
                upper,
                sname.to_uppercase()
            ));

            // Обработчик enter состояния (каждый такт)
            for sp in &sd.parts {
                if let StatePart::PropertyDefinition(pd) = sp {
                    if pd.name.as_ref().map(|n| n.name.as_str()) == Some("enter") {
                        let stmt = match &pd.value {
                            Property::Function(s) => stmt_to_c(s, 12),
                            Property::Expression(e) => {
                                format!("            {};\n", crate::condition::expr_to_c(e))
                            }
                        };
                        out.push_str(&stmt);
                    }
                }
            }

            // Переходы
            for sp in &sd.parts {
                if let StatePart::Reference(_, target, cond) = sp {
                    let cond_str = cond
                        .as_ref()
                        .map(|c| condition_to_c(c))
                        .unwrap_or_else(|| "1".to_string());

                    out.push_str(&format!("            if ({}) {{\n", cond_str));

                    // Обработчики exit
                    for sp2 in &sd.parts {
                        if let StatePart::PropertyDefinition(pd2) = sp2 {
                            if pd2.name.as_ref().map(|n| n.name.as_str()) == Some("exit") {
                                let stmt = match &pd2.value {
                                    Property::Function(s) => stmt_to_c(s, 16),
                                    Property::Expression(e) => {
                                        format!("                {};\n", crate::condition::expr_to_c(e))
                                    }
                                };
                                out.push_str(&stmt);
                            }
                        }
                    }

                    // Обработчики before на целевом состоянии
                    if let Some(target_part) = model.parts.iter().find(|p| {
                        if let ModelPart::StateDefinition(ts) = p {
                            ts.name.as_ref().map(|n| n.name.as_str()) == Some(target.name.as_str())
                        } else {
                            false
                        }
                    }) {
                        if let ModelPart::StateDefinition(ts) = target_part {
                            for tsp in &ts.parts {
                                if let StatePart::PropertyDefinition(pd3) = tsp {
                                    if pd3.name.as_ref().map(|n| n.name.as_str()) == Some("before") {
                                        let stmt = match &pd3.value {
                                            Property::Function(s) => stmt_to_c(s, 16),
                                            Property::Expression(e) => {
                                                format!("                {};\n", crate::condition::expr_to_c(e))
                                            }
                                        };
                                        out.push_str(&stmt);
                                    }
                                }
                            }
                        }
                    }

                    out.push_str(&format!(
                        "                _state = {}_{};\n",
                        upper,
                        target.name.to_uppercase()
                    ));
                    out.push_str("            }\n");
                }
            }

            out.push_str("            break;\n");
            out.push_str("        }\n");
        }
    }

    out.push_str("    }\n");
    out.push_str("}\n");
    out
}

/// Сгенерировать C-заголовок и исходник для всех моделей из SourceUnit.
pub fn generate_c_all(
    source: &SourceUnit,
    ctx: &CodegenContext,
) -> Vec<(String, String, String)> {
    let mut result = vec![];
    for part in &source.0 {
        if let SourceUnitPart::ModelDefinition(md) = part {
            let name = model_name(md).to_lowercase();
            let header = generate_c_header(md, ctx);
            let src = generate_c_source(md, ctx);
            result.push((name, header, src));
        }
    }
    result
}

// ── Вспомогательные функции ─────────────────────────────────────────────────────

fn model_name(model: &ModelDefinition) -> String {
    model
        .name
        .as_ref()
        .map(|n| n.name.clone())
        .unwrap_or_else(|| "Model".to_string())
}

fn collect_states(model: &ModelDefinition) -> Vec<String> {
    model
        .parts
        .iter()
        .filter_map(|p| {
            if let ModelPart::StateDefinition(sd) = p {
                sd.name.as_ref().map(|n| n.name.clone())
            } else {
                None
            }
        })
        .collect()
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
