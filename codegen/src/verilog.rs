use but_grammar::ast::{
    Condition, ModelDefinition, ModelPart, Property, SourceUnit, SourceUnitPart, StatePart,
    VariableAttribute,
};

use crate::condition::condition_to_verilog;
use crate::ltl::{extract_ltl_formulas, ltl_comments_verilog};
use crate::CodegenContext;

/// Сгенерировать Verilog-модуль для модели.
pub fn generate_verilog(model: &ModelDefinition, ctx: &CodegenContext) -> String {
    let name = model_name(model).to_lowercase();
    let states = collect_states(model);
    let state_bits = bits_needed(states.len());

    let mut out = String::new();
    let ltl_formulas = extract_ltl_formulas(model);
    out.push_str("// Сгенерировано but-codegen\n");
    if !ltl_formulas.is_empty() {
        out.push_str(&ltl_comments_verilog(&ltl_formulas));
    }
    out.push_str(&format!("module {} (\n", name));
    out.push_str("    input wire clk,\n");
    out.push_str("    input wire rst");

    // Добавить сигналы портов
    let mut port_list = vec![];
    for vd in &ctx.global_vars {
        if vd.attrs.iter().any(|a| matches!(a, VariableAttribute::Portable(_))) {
            if let Some(vname) = &vd.name {
                let is_readable = vd.attrs.iter().any(|a| matches!(a, VariableAttribute::Readable(_)));
                let is_writable = vd.attrs.iter().any(|a| matches!(a, VariableAttribute::Writable(_)));
                let dir = if is_writable { "output reg" } else { "input wire" };
                port_list.push(format!("    {} {}", dir, vname.name));
            }
        }
    }
    for p in &port_list {
        out.push_str(",\n");
        out.push_str(p);
    }
    out.push_str("\n);\n\n");

    // Параметры состояний
    for (i, s) in states.iter().enumerate() {
        out.push_str(&format!(
            "    parameter STATE_{} = {}'d{};\n",
            s.to_uppercase(),
            state_bits,
            i
        ));
    }
    out.push('\n');

    // Регистр состояния
    out.push_str(&format!(
        "    reg [{}-1:0] state;\n\n",
        state_bits
    ));

    // Найти начальное состояние
    let start = find_start(model).unwrap_or_else(|| states.first().cloned().unwrap_or_default());

    // Блок always
    out.push_str("    always @(posedge clk or posedge rst) begin\n");
    out.push_str(&format!(
        "        if (rst) state <= STATE_{};\n",
        start.to_uppercase()
    ));
    out.push_str("        else begin\n");
    out.push_str("            case (state)\n");

    for part in &model.parts {
        if let ModelPart::StateDefinition(sd) = part {
            let sname = sd.name.as_ref().map(|n| n.name.as_str()).unwrap_or("?");
            out.push_str(&format!(
                "                STATE_{}: begin\n",
                sname.to_uppercase()
            ));

            for sp in &sd.parts {
                if let StatePart::Reference(_, target, cond) = sp {
                    let cond_str = cond
                        .as_ref()
                        .map(|c| condition_to_verilog(c))
                        .unwrap_or_else(|| "1'b1".to_string());
                    out.push_str(&format!(
                        "                    if ({}) state <= STATE_{};\n",
                        cond_str,
                        target.name.to_uppercase()
                    ));
                }
            }

            out.push_str("                end\n");
        }
    }

    out.push_str("            endcase\n");
    out.push_str("        end\n");
    out.push_str("    end\n\n");

    // Логика выходов (комбинаторная — на основе обработчиков enter)
    out.push_str("    // Логика выходов\n");
    out.push_str("    always @(*) begin\n");
    out.push_str("        case (state)\n");

    for part in &model.parts {
        if let ModelPart::StateDefinition(sd) = part {
            let sname = sd.name.as_ref().map(|n| n.name.as_str()).unwrap_or("?");
            let has_actions = sd.parts.iter().any(|p| {
                if let StatePart::PropertyDefinition(pd) = p {
                    pd.name.as_ref().map(|n| n.name.as_str()) == Some("enter")
                } else {
                    false
                }
            });

            if has_actions {
                out.push_str(&format!(
                    "            STATE_{}: begin\n",
                    sname.to_uppercase()
                ));
                for sp in &sd.parts {
                    if let StatePart::PropertyDefinition(pd) = sp {
                        if pd.name.as_ref().map(|n| n.name.as_str()) == Some("enter") {
                            match &pd.value {
                                Property::Expression(e) => {
                                    out.push_str(&format!(
                                        "                {};\n",
                                        crate::condition::expr_to_c(e)
                                    ));
                                }
                                Property::Function(stmt) => {
                                    // Развернуть блочные присваивания
                                    out.push_str(&format!(
                                        "                /* {} */\n",
                                        "enter handler"
                                    ));
                                    let _ = stmt;
                                }
                            }
                        }
                    }
                }
                out.push_str("            end\n");
            }
        }
    }

    out.push_str("        endcase\n");
    out.push_str("    end\n\n");

    out.push_str("endmodule\n");
    out
}

/// Сгенерировать Verilog для всех моделей из SourceUnit.
pub fn generate_verilog_all(
    source: &SourceUnit,
    ctx: &CodegenContext,
) -> Vec<(String, String)> {
    let mut result = vec![];
    for part in &source.0 {
        if let SourceUnitPart::ModelDefinition(md) = part {
            let name = model_name(md).to_lowercase();
            let verilog = generate_verilog(md, ctx);
            result.push((name, verilog));
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

fn bits_needed(n: usize) -> usize {
    if n <= 1 { 1 } else { (n as f64).log2().ceil() as usize }
}
