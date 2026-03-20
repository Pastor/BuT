use crate::CodegenContext;
use crate::behavior::{
    CompositionTree, find_end_property, find_terminal_states, model_composition_tree,
};
use crate::condition::{condition_to_c, expr_to_c, type_to_c_ctx};
use crate::ltl::{extract_ltl_formulas, ltl_comments_c};
use but_grammar::ast::{
    Annotation, Condition, Expression, ModelDefinition, ModelPart, Property, SourceUnit,
    SourceUnitPart, StatePart, VariableAttribute,
};
use std::collections::{HashMap, HashSet};

// ── Public API ────────────────────────────────────────────────────────────────

/// Generate a combined C header file for all models from a SourceUnit.
pub fn generate_c_header_all(source: &SourceUnit, base_name: &str, ctx: &CodegenContext) -> String {
    let guard = base_name.to_uppercase().replace('-', "_").replace('.', "_");
    let mut out = String::new();
    out.push_str("#pragma once\n");
    out.push_str(&format!("#ifndef {}_H__\n", guard));
    out.push_str(&format!("#define {}_H__\n\n", guard));
    out.push_str("#include <stdbool.h>\n");
    out.push_str("#include <stdint.h>\n\n");

    // Type alias typedefs
    let mut alias_names: Vec<(&String, &but_grammar::ast::Type)> =
        ctx.type_aliases.iter().collect();
    alias_names.sort_by_key(|(k, _)| k.as_str());
    for (alias, ty) in &alias_names {
        let resolved = type_to_c_ctx(ty, &ctx.type_aliases);
        // Skip if resolved type contains array suffix (can't make a clean typedef)
        if !resolved.contains('[') {
            out.push_str(&format!("typedef {} {};\n", resolved, alias));
        }
    }
    if !alias_names.is_empty() {
        out.push('\n');
    }

    // Global port extern declarations
    for vd in &ctx.global_vars {
        if vd
            .attrs
            .iter()
            .any(|a| matches!(a, VariableAttribute::Portable(_)))
        {
            if let Some(vname) = &vd.name {
                let ty = type_to_c_ctx(&vd.ty, &ctx.type_aliases);
                out.push_str(&format!(
                    "extern {};\n",
                    format_field_decl(&vname.name, &ty)
                ));
            }
        }
    }
    if !ctx.global_vars.is_empty() {
        out.push('\n');
    }

    // Find which models are sub-models (embedded, not top-level)
    let sub_model_names = collect_sub_model_names(source);

    for part in &source.0 {
        if let SourceUnitPart::ModelDefinition(md) = part {
            let name = model_name(md);
            if sub_model_names.contains(&name) {
                continue; // embedded in a composition struct
            }
            out.push_str(&gen_model_struct(md, source, ctx));
            out.push('\n');
            out.push_str(&gen_model_declarations(md, ctx));
            out.push('\n');
        }
    }

    out.push_str(&format!("#endif /* {}_H__ */\n", guard));
    out
}

/// Generate a combined C source file for all models from a SourceUnit.
pub fn generate_c_source_all(source: &SourceUnit, base_name: &str, ctx: &CodegenContext) -> String {
    let mut out = String::new();
    out.push_str("#include <assert.h>\n");
    out.push_str(&format!("#include \"{}.h\"\n\n", base_name.to_lowercase()));

    // Global port definitions
    for vd in &ctx.global_vars {
        if vd
            .attrs
            .iter()
            .any(|a| matches!(a, VariableAttribute::Portable(_)))
        {
            if let Some(vname) = &vd.name {
                let ty = type_to_c_ctx(&vd.ty, &ctx.type_aliases);
                let init = vd
                    .initializer
                    .as_ref()
                    .map(expr_to_c)
                    .unwrap_or_else(|| "0".to_string());
                out.push_str(&format!(
                    "{} = {};\n",
                    format_field_decl(&vname.name, &ty),
                    init
                ));
            }
        }
    }
    if !ctx.global_vars.is_empty() {
        out.push('\n');
    }

    let sub_model_names = collect_sub_model_names(source);

    for part in &source.0 {
        if let SourceUnitPart::ModelDefinition(md) = part {
            let name = model_name(md);
            if sub_model_names.contains(&name) {
                continue;
            }
            out.push_str(&gen_model_source(md, source, ctx));
            out.push('\n');
        }
    }
    out
}

/// Generate a C header for a single model.
pub fn generate_c_header(model: &ModelDefinition, ctx: &CodegenContext) -> String {
    // Use an empty source for single-model generation
    let empty_src = SourceUnit(vec![]);
    let name = model_name(model);
    let upper = name.to_uppercase();
    let mut out = String::new();
    out.push_str(&format!("#ifndef __{}_H__\n", upper));
    out.push_str(&format!("#define __{}_H__\n\n", upper));
    out.push_str("#include <stdio.h>\n");
    out.push_str("#include <stdbool.h>\n");
    out.push_str("#include <stdint.h>\n\n");
    let ltl = extract_ltl_formulas(model);
    if !ltl.is_empty() {
        out.push_str(&ltl_comments_c(&ltl));
        out.push('\n');
    }
    for vd in &ctx.global_vars {
        if vd
            .attrs
            .iter()
            .any(|a| matches!(a, VariableAttribute::Portable(_)))
        {
            if let Some(vname) = &vd.name {
                let ty = type_to_c_ctx(&vd.ty, &ctx.type_aliases);
                out.push_str(&format!(
                    "extern {};\n",
                    format_field_decl(&vname.name, &ty)
                ));
            }
        }
    }
    if !ctx.global_vars.is_empty() {
        out.push('\n');
    }
    out.push_str(&gen_model_struct(model, &empty_src, ctx));
    out.push('\n');
    out.push_str(&gen_model_declarations(model, ctx));
    out.push_str(&format!("#endif /* __{}_H__ */\n", upper));
    out
}

/// Generate a C source for a single model.
pub fn generate_c_source(model: &ModelDefinition, ctx: &CodegenContext) -> String {
    let empty_src = SourceUnit(vec![]);
    gen_model_source(model, &empty_src, ctx)
}

/// Generate C header and source for all models.
pub fn generate_c_all(source: &SourceUnit, ctx: &CodegenContext) -> (String, String) {
    generate_c_all_named(source, "model", ctx)
}

/// Generate combined C output with a specified base file name.
pub fn generate_c_all_named(
    source: &SourceUnit,
    base_name: &str,
    ctx: &CodegenContext,
) -> (String, String) {
    (
        generate_c_header_all(source, base_name, ctx),
        generate_c_source_all(source, base_name, ctx),
    )
}

// ── Struct generation ─────────────────────────────────────────────────────────

/// Generate `struct Model { ... };` for a model.
fn gen_model_struct(model: &ModelDefinition, source: &SourceUnit, ctx: &CodegenContext) -> String {
    let name = model_name(model);
    let upper = name.to_uppercase();
    let i1 = ctx.indent.level(1);
    let i2 = ctx.indent.level(2);
    let i3 = ctx.indent.level(3);
    let mut out = String::new();

    let ltl = extract_ltl_formulas(model);
    if !ltl.is_empty() {
        out.push_str(&ltl_comments_c(&ltl));
        out.push('\n');
    }

    out.push_str(&format!("struct {} {{\n", name));

    if let Some(tree) = model_composition_tree(model) {
        // Composition model: embed sub-model structs
        let leaves = tree.leaves();
        for leaf in &leaves {
            let field = leaf.to_lowercase();
            let leaf_upper = leaf.to_uppercase();
            // Find the sub-model definition to get its states and vars
            if let Some(sub_md) = find_model_in_source(source, leaf) {
                out.push_str(&format!("{}struct {{\n", i1));
                // State enum
                out.push_str(&format!("{}enum {{\n", i2));
                out.push_str(&format!("{}{}_{}_INIT,\n", i3, upper, leaf_upper));
                for s in collect_state_names(sub_md) {
                    out.push_str(&format!(
                        "{}{}_{}_{},\n",
                        i3,
                        upper,
                        leaf_upper,
                        s.to_uppercase()
                    ));
                }
                out.push_str(&format!("{}}} state;\n", i2));
                // Local variables
                for part in &sub_md.parts {
                    if let ModelPart::VariableDefinition(vd) = part {
                        if vd
                            .attrs
                            .iter()
                            .any(|a| matches!(a, VariableAttribute::Portable(_)))
                        {
                            continue;
                        }
                        if let Some(vname) = &vd.name {
                            let ty = type_to_c_ctx(&vd.ty, &ctx.type_aliases);
                            out.push_str(&format!(
                                "{}{}{};\n",
                                i1,
                                i1,
                                format_field_decl(&vname.name, &ty)
                            ));
                        }
                    }
                }
                out.push_str(&format!("{}}} {};\n", i1, field));
            } else {
                // Sub-model not in source — generate minimal embedded struct
                out.push_str(&format!("{}struct {{\n", i1));
                out.push_str(&format!("{}enum {{\n", i2));
                out.push_str(&format!("{}{}_{}_INIT,\n", i3, upper, leaf_upper));
                out.push_str(&format!("{}{}_{}_DONE\n", i3, upper, leaf_upper));
                out.push_str(&format!("{}}} state;\n", i2));
                out.push_str(&format!("{}}} {};\n", i1, field));
            }
        }
        // Composition phase state
        out.push_str(&format!("{}enum {{\n", i1));
        out.push_str(&format!("{}{}_IMPLEMENT_INIT,\n", i2, upper));
        out.push_str(&format!("{}{}_IMPLEMENT_TICK,\n", i2, upper));
        out.push_str(&format!("{}{}_IMPLEMENT_END\n", i2, upper));
        out.push_str(&format!("{}}} state;\n", i1));
        // Own local variables of the composition model
        for part in &model.parts {
            if let ModelPart::VariableDefinition(vd) = part {
                if vd
                    .attrs
                    .iter()
                    .any(|a| matches!(a, VariableAttribute::Portable(_)))
                {
                    continue;
                }
                if let Some(vname) = &vd.name {
                    let ty = type_to_c_ctx(&vd.ty, &ctx.type_aliases);
                    out.push_str(&format!("{}{};\n", i1, format_field_decl(&vname.name, &ty)));
                }
            }
        }
    } else {
        // FSM model: state enum + local vars
        out.push_str(&format!("{}enum {{\n", i1));
        out.push_str(&format!("{}{}_INIT,\n", i2, upper));
        for s in collect_state_names(model) {
            out.push_str(&format!("{}{}_{}", i2, upper, s.to_uppercase()));
            out.push_str(",\n");
        }
        out.push_str(&format!("{}}} state;\n", i1));
        for part in &model.parts {
            if let ModelPart::VariableDefinition(vd) = part {
                if vd
                    .attrs
                    .iter()
                    .any(|a| matches!(a, VariableAttribute::Portable(_)))
                {
                    continue;
                }
                if let Some(vname) = &vd.name {
                    let ty = type_to_c_ctx(&vd.ty, &ctx.type_aliases);
                    out.push_str(&format!("{}{};\n", i1, format_field_decl(&vname.name, &ty)));
                }
            }
        }
    }

    out.push_str("};\n");
    out
}

/// Generate function declarations for a model.
fn gen_model_declarations(model: &ModelDefinition, ctx: &CodegenContext) -> String {
    let name = model_name(model);
    let param = format!("struct {} *{}", name, name.to_lowercase());
    let mut out = String::new();
    out.push_str(&format!("void {}_init({});\n", name, param));
    out.push_str(&format!("void {}_tick({});\n", name, param));
    let terminals = find_terminal_states(model);
    let is_composition = model_composition_tree(model).is_some();
    if !terminals.is_empty() || is_composition {
        out.push_str(&format!("bool {}_finished({});\n", name, param));
    }
    out.push_str(&format!("void {}_reset({});\n", name, param));
    out
}

// ── Source generation ─────────────────────────────────────────────────────────

/// Generate the C source for a model (composition or FSM).
fn gen_model_source(model: &ModelDefinition, source: &SourceUnit, ctx: &CodegenContext) -> String {
    let name = model_name(model);
    let upper = name.to_uppercase();
    let param = name.to_lowercase();
    let i1 = ctx.indent.level(1);
    let i2 = ctx.indent.level(2);
    let i3 = ctx.indent.level(3);

    let mut out = String::new();

    if let Some(tree) = model_composition_tree(model) {
        // --- Composition model ---
        let leaves = tree.leaves();

        // Build peer map: model_name → field_name
        let peer_map: HashMap<String, String> = leaves
            .iter()
            .map(|n| (n.clone(), n.to_lowercase()))
            .collect();

        // Sub-model static inline functions
        for leaf in &leaves {
            let field = leaf.to_lowercase();
            let leaf_upper = leaf.to_uppercase();
            if let Some(sub_md) = find_model_in_source(source, leaf) {
                out.push_str(&gen_sub_model_funcs(
                    sub_md,
                    &name,
                    &upper,
                    &field,
                    leaf,
                    &leaf_upper,
                    &param,
                    &peer_map,
                    ctx,
                ));
            }
        }

        // --- Main composition functions ---
        out.push_str(&format!("//model: {}\n", name));

        // reset
        out.push_str(&format!(
            "void {}_reset(struct {} *{}) {{\n",
            name, name, param
        ));
        out.push_str(&format!("{}assert({} != 0);\n", i1, param));
        out.push_str(&format!(
            "{}{}->{} = {}_IMPLEMENT_INIT;\n",
            i1, param, "state", upper
        ));
        for leaf in &leaves {
            out.push_str(&format!("{}{}_{}_reset({});\n", i1, name, leaf, param));
        }
        // Own local var init
        for part in &model.parts {
            if let ModelPart::VariableDefinition(vd) = part {
                if vd
                    .attrs
                    .iter()
                    .any(|a| matches!(a, VariableAttribute::Portable(_)))
                {
                    continue;
                }
                if let Some(vname) = &vd.name {
                    if let Some(init) = &vd.initializer {
                        match init {
                            Expression::Initializer(_, vals) => {
                                for (i, v) in vals.iter().enumerate() {
                                    out.push_str(&format!(
                                        "{}{}->{}[{}] = {};\n",
                                        i1,
                                        param,
                                        vname.name,
                                        i,
                                        expr_to_c(v)
                                    ));
                                }
                            }
                            _ => {
                                out.push_str(&format!(
                                    "{}{}->{} = {};\n",
                                    i1,
                                    param,
                                    vname.name,
                                    expr_to_c(init)
                                ));
                            }
                        }
                    }
                }
            }
        }
        out.push_str("}\n\n");

        // init
        out.push_str(&format!(
            "void {}_init(struct {} *{}) {{\n",
            name, name, param
        ));
        out.push_str(&format!("{}assert({} != 0);\n", i1, param));
        out.push_str(&format!("{}{}_reset({});\n", i1, name, param));
        out.push_str("}\n\n");

        // tick
        out.push_str(&format!(
            "void {}_tick(struct {} *{}) {{\n",
            name, name, param
        ));
        out.push_str(&format!("{}assert({} != 0);\n", i1, param));
        out.push_str(&format!("{}switch ({}->{}) {{\n", i1, param, "state"));

        // IMPLEMENT_INIT case
        out.push_str(&format!("{}case {}_IMPLEMENT_INIT: {{\n", i2, upper));
        for leaf in &leaves {
            out.push_str(&format!("{}{}_{}_init({});\n", i3, name, leaf, param));
        }
        out.push_str(&format!(
            "{}{}->{} = {}_IMPLEMENT_TICK;\n",
            i3, param, "state", upper
        ));
        out.push_str(&format!("{}break;\n", i3));
        out.push_str(&format!("{}}}\n", i2));

        // IMPLEMENT_TICK case
        out.push_str(&format!("{}case {}_IMPLEMENT_TICK: {{\n", i2, upper));
        out.push_str(&gen_composition_tick(&tree, &name, &upper, &param, ctx));
        out.push_str(&format!("{}break;\n", i3));
        out.push_str(&format!("{}}}\n", i2));

        // IMPLEMENT_END case
        out.push_str(&format!("{}case {}_IMPLEMENT_END: {{\n", i2, upper));
        if let Some(ep) = find_end_property(model) {
            out.push_str(&property_with_ctx(ep, &param, "", &[], ctx, 3));
        }
        out.push_str(&format!("{}break;\n", i3));
        out.push_str(&format!("{}}}\n", i2));

        out.push_str(&format!("{}}}\n", i1));
        out.push_str("}\n\n");

        // finished
        out.push_str(&format!(
            "bool {}_finished(struct {} *{}) {{\n",
            name, name, param
        ));
        out.push_str(&format!("{}assert({} != 0);\n", i1, param));
        out.push_str(&format!(
            "{}return {}->{} == {}_IMPLEMENT_END;\n",
            i1, param, "state", upper
        ));
        out.push_str("}\n");
    } else {
        // --- FSM model ---
        out.push_str(&gen_fsm_source(model, ctx));
    }

    out
}

/// Generate static inline sub-model functions for a composition model.
fn gen_sub_model_funcs(
    sub_md: &ModelDefinition,
    parent_name: &str,
    parent_upper: &str,
    field: &str,
    field_name: &str,
    field_upper: &str,
    param: &str,
    peer_map: &HashMap<String, String>,
    ctx: &CodegenContext,
) -> String {
    let i1 = ctx.indent.level(1);
    let i2 = ctx.indent.level(2);
    let i3 = ctx.indent.level(3);
    let i4 = ctx.indent.level(4);
    let inline = "__attribute__((always_inline)) static ";
    let struct_param = format!("struct {} *{}", parent_name, param);
    let mut out = String::new();

    let local_vars = collect_local_var_names(sub_md);
    let states = collect_state_names(sub_md);
    let start_state = find_start_state(sub_md);
    let terminal_states = find_terminal_states(sub_md);

    out.push_str(&format!("//model: {}\n", field_name));

    // reset
    out.push_str(&format!(
        "{}void {}_{}_reset({}) {{\n",
        inline, parent_name, field_name, struct_param
    ));
    out.push_str(&format!("{}assert({} != 0);\n", i1, param));
    out.push_str(&format!(
        "{}{}->{}.state = {}_{}_INIT;\n",
        i1, param, field, parent_upper, field_upper
    ));
    out.push_str("}\n");

    // init
    out.push_str(&format!(
        "{}void {}_{}_init({}) {{\n",
        inline, parent_name, field_name, struct_param
    ));
    out.push_str(&format!("{}assert({} != 0);\n", i1, param));
    out.push_str(&format!(
        "{}{}_{}_reset({});\n",
        i1, parent_name, field_name, param
    ));
    out.push_str("}\n");

    // tick
    out.push_str(&format!(
        "{}void {}_{}_tick({}) {{\n",
        inline, parent_name, field_name, struct_param
    ));
    out.push_str(&format!("{}assert({} != 0);\n", i1, param));
    out.push_str(&format!("{}switch ({}->{}.state) {{\n", i1, param, field));

    // INIT case: set initial values, transition to start
    out.push_str(&format!(
        "{}case {}_{}_INIT: {{\n",
        i2, parent_upper, field_upper
    ));
    for part in &sub_md.parts {
        if let ModelPart::VariableDefinition(vd) = part {
            if vd
                .attrs
                .iter()
                .any(|a| matches!(a, VariableAttribute::Portable(_)))
            {
                continue;
            }
            if let Some(vname) = &vd.name {
                if let Some(init) = &vd.initializer {
                    match init {
                        Expression::Initializer(_, vals) => {
                            for (i, v) in vals.iter().enumerate() {
                                out.push_str(&format!(
                                    "{}{}->{}.{}[{}] = {};\n",
                                    i3,
                                    param,
                                    field,
                                    vname.name,
                                    i,
                                    expr_to_c(v)
                                ));
                            }
                        }
                        _ => {
                            out.push_str(&format!(
                                "{}{}->{}.{} = {};\n",
                                i3,
                                param,
                                field,
                                vname.name,
                                expr_to_c(init)
                            ));
                        }
                    }
                }
            }
        }
    }
    if let Some(start) = &start_state {
        out.push_str(&format!(
            "{}{}->{}.state = {}_{}_{} ;\n",
            i3,
            param,
            field,
            parent_upper,
            field_upper,
            start.to_uppercase()
        ));
    } else {
        // fallback: no start state found
        out.push_str(&format!("{}/* no start state */\n", i3));
    }
    out.push_str(&format!("{}break;\n", i3));
    out.push_str(&format!("{}}}\n", i2));

    // Regular state cases
    for part in &sub_md.parts {
        if let ModelPart::StateDefinition(sd) = part {
            let sname = sd.name.as_ref().map(|n| n.name.as_str()).unwrap_or("?");
            let sname_up = sname.to_uppercase();
            let is_terminal = terminal_states.iter().any(|t| t == sname);

            out.push_str(&format!(
                "{}case {}_{}_{}:  {{\n",
                i2, parent_upper, field_upper, sname_up
            ));

            // Transitions
            for sp in &sd.parts {
                if let StatePart::Reference(_, target, cond_opt) = sp {
                    let cond_str = if let Some(cond) = cond_opt {
                        cond_with_ctx(
                            cond,
                            param,
                            field,
                            &local_vars,
                            parent_upper,
                            field_upper,
                            peer_map,
                        )
                    } else {
                        "1".to_string()
                    };
                    out.push_str(&format!("{}if ({}) {{\n", i3, cond_str));

                    // Enter handler of target state
                    if let Some(target_sd) = find_state_in_model(sub_md, &target.name) {
                        for tsp in &target_sd.parts {
                            if let StatePart::PropertyDefinition(pd) = tsp {
                                if pd.name.as_ref().map(|n| n.name.as_str()) == Some("enter") {
                                    out.push_str(&property_with_ctx(
                                        &pd.value,
                                        param,
                                        field,
                                        &local_vars,
                                        ctx,
                                        4,
                                    ));
                                }
                            }
                        }
                    }

                    out.push_str(&format!(
                        "{}{}->{}.state = {}_{}_{};\n",
                        i4,
                        param,
                        field,
                        parent_upper,
                        field_upper,
                        target.name.to_uppercase()
                    ));
                    out.push_str(&format!("{}}}\n", i3));
                }
            }

            out.push_str(&format!("{}break;\n", i3));
            out.push_str(&format!("{}}}\n", i2));
        }
    }

    out.push_str(&format!("{}}}\n", i1));
    out.push_str("}\n");

    // finished
    out.push_str(&format!(
        "{}bool {}_{}_finished({}) {{\n",
        inline, parent_name, field_name, struct_param
    ));
    out.push_str(&format!("{}assert({} != 0);\n", i1, param));
    if terminal_states.is_empty() {
        out.push_str(&format!("{}return false;\n", i1));
    } else {
        let conds: Vec<String> = terminal_states
            .iter()
            .map(|s| {
                format!(
                    "{}->{}.state == {}_{}_{}",
                    param,
                    field,
                    parent_upper,
                    field_upper,
                    s.to_uppercase()
                )
            })
            .collect();
        out.push_str(&format!("{}return {};\n", i1, conds.join(" || ")));
    }
    out.push_str("}\n");

    out
}

/// Generate the source for a standalone FSM model.
fn gen_fsm_source(model: &ModelDefinition, ctx: &CodegenContext) -> String {
    let name = model_name(model);
    let upper = name.to_uppercase();
    let param = name.to_lowercase();
    let i1 = ctx.indent.level(1);
    let i2 = ctx.indent.level(2);
    let i3 = ctx.indent.level(3);
    let i4 = ctx.indent.level(4);
    let struct_param = format!("struct {} *{}", name, param);

    let local_vars = collect_local_var_names(model);
    let start_state = find_start_state(model);
    let terminal_states = find_terminal_states(model);

    let mut out = String::new();

    let ltl = extract_ltl_formulas(model);
    if !ltl.is_empty() {
        out.push_str(&ltl_comments_c(&ltl));
        out.push('\n');
    }

    // reset
    out.push_str(&format!("void {}_reset({}) {{\n", name, struct_param));
    out.push_str(&format!("{}assert({} != 0);\n", i1, param));
    out.push_str(&format!("{}{}->{} = {}_INIT;\n", i1, param, "state", upper));
    out.push_str("}\n\n");

    // init
    out.push_str(&format!("void {}_init({}) {{\n", name, struct_param));
    out.push_str(&format!("{}assert({} != 0);\n", i1, param));
    out.push_str(&format!("{}{}_reset({});\n", i1, name, param));
    out.push_str("}\n\n");

    // tick
    out.push_str(&format!("void {}_tick({}) {{\n", name, struct_param));
    out.push_str(&format!("{}assert({} != 0);\n", i1, param));
    out.push_str(&format!("{}switch ({}->{}) {{\n", i1, param, "state"));

    // INIT case
    out.push_str(&format!("{}case {}_INIT: {{\n", i2, upper));
    // Local var initializations
    for part in &model.parts {
        if let ModelPart::VariableDefinition(vd) = part {
            if vd
                .attrs
                .iter()
                .any(|a| matches!(a, VariableAttribute::Portable(_)))
            {
                continue;
            }
            if let Some(vname) = &vd.name {
                if let Some(init) = &vd.initializer {
                    match init {
                        Expression::Initializer(_, vals) => {
                            for (i, v) in vals.iter().enumerate() {
                                out.push_str(&format!(
                                    "{}{}->{}[{}] = {};\n",
                                    i3,
                                    param,
                                    vname.name,
                                    i,
                                    expr_with_ctx(v, &param, "", &local_vars)
                                ));
                            }
                        }
                        _ => {
                            out.push_str(&format!(
                                "{}{}->{} = {};\n",
                                i3,
                                param,
                                vname.name,
                                expr_with_ctx(init, &param, "", &local_vars)
                            ));
                        }
                    }
                }
            }
        }
    }
    if let Some(start) = &start_state {
        // Enter handler of start state
        if let Some(start_sd) = find_state_in_model(model, start) {
            for sp in &start_sd.parts {
                if let StatePart::PropertyDefinition(pd) = sp {
                    if pd.name.as_ref().map(|n| n.name.as_str()) == Some("enter") {
                        out.push_str(&property_with_ctx(
                            &pd.value,
                            &param,
                            "",
                            &local_vars,
                            ctx,
                            3,
                        ));
                    }
                }
            }
        }
        out.push_str(&format!(
            "{}{}->{} = {}_{};\n",
            i3,
            param,
            "state",
            upper,
            start.to_uppercase()
        ));
    }
    out.push_str(&format!("{}break;\n", i3));
    out.push_str(&format!("{}}}\n", i2));

    // Regular state cases
    for part in &model.parts {
        if let ModelPart::StateDefinition(sd) = part {
            let sname = sd.name.as_ref().map(|n| n.name.as_str()).unwrap_or("?");
            let sname_up = sname.to_uppercase();
            let is_terminal = terminal_states.iter().any(|t| t == sname);

            out.push_str(&format!("{}case {}_{}: {{\n", i2, upper, sname_up));

            // Transitions
            for sp in &sd.parts {
                if let StatePart::Reference(_, target, cond_opt) = sp {
                    let peer_map = HashMap::new(); // standalone model has no peers
                    let cond_str = if let Some(cond) = cond_opt {
                        cond_with_ctx(cond, &param, "", &local_vars, &upper, "", &peer_map)
                    } else {
                        "1".to_string()
                    };
                    out.push_str(&format!("{}if ({}) {{\n", i3, cond_str));

                    // Enter handler of target state
                    if let Some(target_sd) = find_state_in_model(model, &target.name) {
                        for tsp in &target_sd.parts {
                            if let StatePart::PropertyDefinition(pd) = tsp {
                                if pd.name.as_ref().map(|n| n.name.as_str()) == Some("enter") {
                                    out.push_str(&property_with_ctx(
                                        &pd.value,
                                        &param,
                                        "",
                                        &local_vars,
                                        ctx,
                                        4,
                                    ));
                                }
                            }
                        }
                    }

                    out.push_str(&format!(
                        "{}{}->{} = {}_{};\n",
                        i4,
                        param,
                        "state",
                        upper,
                        target.name.to_uppercase()
                    ));
                    out.push_str(&format!("{}}}\n", i3));
                }
            }

            // End handler for terminal states
            if is_terminal {
                if let Some(ep) = find_end_property(model) {
                    out.push_str(&format!("{}/* end handler */\n", i3));
                    out.push_str(&property_with_ctx(ep, &param, "", &local_vars, ctx, 3));
                }
            }

            out.push_str(&format!("{}break;\n", i3));
            out.push_str(&format!("{}}}\n", i2));
        }
    }

    out.push_str(&format!("{}}}\n", i1));
    out.push_str("}\n\n");

    // finished
    if !terminal_states.is_empty() {
        out.push_str(&format!("bool {}_finished({}) {{\n", name, struct_param));
        out.push_str(&format!("{}assert({} != 0);\n", i1, param));
        let conds: Vec<String> = terminal_states
            .iter()
            .map(|s| format!("{}->{} == {}_{}", param, "state", upper, s.to_uppercase()))
            .collect();
        out.push_str(&format!("{}return {};\n", i1, conds.join(" || ")));
        out.push_str("}\n");
    }

    out
}

// ── Composition tick logic ────────────────────────────────────────────────────

fn gen_composition_tick(
    tree: &CompositionTree,
    parent_name: &str,
    parent_upper: &str,
    param: &str,
    ctx: &CodegenContext,
) -> String {
    let i3 = ctx.indent.level(3);
    let i4 = ctx.indent.level(4);
    let mut out = String::new();

    match tree {
        CompositionTree::Sequential(steps) => {
            let mut first = true;
            for step in steps {
                let not_done = gen_not_done_cond(step, parent_name, param);
                let actions = gen_tick_actions(step, parent_name, param, &i4);
                if first {
                    out.push_str(&format!("{}if ({}) {{\n", i3, not_done));
                    first = false;
                } else {
                    out.push_str(&format!("{}}} else if ({}) {{\n", i3, not_done));
                }
                out.push_str(&actions);
            }
            out.push_str(&format!("{}}} else {{\n", i3));
            out.push_str(&format!(
                "{}{}->{} = {}_IMPLEMENT_END;\n",
                i4, param, "state", parent_upper
            ));
            out.push_str(&format!("{}}}\n", i3));
        }
        CompositionTree::Parallel(children) => {
            // Tick all children
            for child in children {
                out.push_str(&gen_tick_actions(child, parent_name, param, &i3));
            }
            // Check if all done
            let all_done: Vec<String> = children
                .iter()
                .flat_map(|c| c.leaves())
                .map(|n| format!("{}_{}_finished({})", parent_name, n, param))
                .collect();
            out.push_str(&format!("{}if ({}) {{\n", i3, all_done.join(" && ")));
            out.push_str(&format!(
                "{}{}->{} = {}_IMPLEMENT_END;\n",
                i4, param, "state", parent_upper
            ));
            out.push_str(&format!("{}}}\n", i3));
        }
        CompositionTree::Single(name) => {
            let not_done = format!("!{}_{}_finished({})", parent_name, name, param);
            out.push_str(&format!("{}if ({}) {{\n", i3, not_done));
            out.push_str(&format!(
                "{}{}_{}_tick({});\n",
                i4, parent_name, name, param
            ));
            out.push_str(&format!("{}}} else {{\n", i3));
            out.push_str(&format!(
                "{}{}->{} = {}_IMPLEMENT_END;\n",
                i4, param, "state", parent_upper
            ));
            out.push_str(&format!("{}}}\n", i3));
        }
    }
    out
}

fn gen_not_done_cond(step: &CompositionTree, parent_name: &str, param: &str) -> String {
    match step {
        CompositionTree::Single(name) => {
            format!("!{}_{}_finished({})", parent_name, name, param)
        }
        CompositionTree::Parallel(children) | CompositionTree::Sequential(children) => {
            let leaves: Vec<String> = children.iter().flat_map(|c| c.leaves()).collect();
            let conds: Vec<String> = leaves
                .iter()
                .map(|n| format!("{}_{}_finished({})", parent_name, n, param))
                .collect();
            format!("!({})", conds.join(" && "))
        }
    }
}

fn gen_tick_actions(
    step: &CompositionTree,
    parent_name: &str,
    param: &str,
    indent: &str,
) -> String {
    match step {
        CompositionTree::Single(name) => {
            format!("{}{}_{}_tick({});\n", indent, parent_name, name, param)
        }
        CompositionTree::Parallel(children) | CompositionTree::Sequential(children) => children
            .iter()
            .flat_map(|c| c.leaves())
            .map(|n| format!("{}{}_{}_tick({});\n", indent, parent_name, n, param))
            .collect(),
    }
}

// ── Context-aware expression/condition translation ────────────────────────────

/// Translate a condition with struct-field context.
/// `field` is the sub-model field (e.g., "consumer"); empty for main model.
/// `local_vars` are names of local let-variables of the current sub-model.
/// `peer_map` maps model names to their field names for S(X) resolution.
fn cond_with_ctx(
    cond: &Condition,
    param: &str,
    field: &str,
    local_vars: &[String],
    parent_upper: &str,
    current_upper: &str,
    peer_map: &HashMap<String, String>,
) -> String {
    match cond {
        Condition::Variable(id) => {
            if local_vars.contains(&id.name) {
                if field.is_empty() {
                    format!("{}->{}", param, id.name)
                } else {
                    format!("{}->{}.{}", param, field, id.name)
                }
            } else {
                id.name.clone()
            }
        }
        Condition::ArraySubscript(_, id, idx) => {
            if local_vars.contains(&id.name) {
                if field.is_empty() {
                    format!("{}->{}[{}]", param, id.name, idx)
                } else {
                    format!("{}->{}.{}[{}]", param, field, id.name, idx)
                }
            } else {
                format!("{}[{}]", id.name, idx)
            }
        }
        Condition::Equal(_, l, r) => {
            // Detect S(Model) = State pattern
            if let (Condition::FunctionCall(_, s_id, s_args), Condition::Variable(state_var)) =
                (l.as_ref(), r.as_ref())
            {
                if s_id.name == "S" && s_args.len() == 1 {
                    if let Condition::Variable(model_var) = &s_args[0] {
                        let mname = &model_var.name;
                        if let Some(peer_field) = peer_map.get(mname) {
                            let mu = mname.to_uppercase();
                            let su = state_var.name.to_uppercase();
                            return format!(
                                "{}->{}.state == {}_{}_{}",
                                param, peer_field, parent_upper, mu, su
                            );
                        }
                    }
                }
            }
            format!(
                "{} == {}",
                cond_with_ctx(
                    l,
                    param,
                    field,
                    local_vars,
                    parent_upper,
                    current_upper,
                    peer_map
                ),
                cond_with_ctx(
                    r,
                    param,
                    field,
                    local_vars,
                    parent_upper,
                    current_upper,
                    peer_map
                )
            )
        }
        Condition::NotEqual(_, l, r) => format!(
            "{} != {}",
            cond_with_ctx(
                l,
                param,
                field,
                local_vars,
                parent_upper,
                current_upper,
                peer_map
            ),
            cond_with_ctx(
                r,
                param,
                field,
                local_vars,
                parent_upper,
                current_upper,
                peer_map
            )
        ),
        Condition::Less(_, l, r) => format!(
            "{} < {}",
            cond_with_ctx(
                l,
                param,
                field,
                local_vars,
                parent_upper,
                current_upper,
                peer_map
            ),
            cond_with_ctx(
                r,
                param,
                field,
                local_vars,
                parent_upper,
                current_upper,
                peer_map
            )
        ),
        Condition::LessEqual(_, l, r) => format!(
            "{} <= {}",
            cond_with_ctx(
                l,
                param,
                field,
                local_vars,
                parent_upper,
                current_upper,
                peer_map
            ),
            cond_with_ctx(
                r,
                param,
                field,
                local_vars,
                parent_upper,
                current_upper,
                peer_map
            )
        ),
        Condition::More(_, l, r) => format!(
            "{} > {}",
            cond_with_ctx(
                l,
                param,
                field,
                local_vars,
                parent_upper,
                current_upper,
                peer_map
            ),
            cond_with_ctx(
                r,
                param,
                field,
                local_vars,
                parent_upper,
                current_upper,
                peer_map
            )
        ),
        Condition::MoreEqual(_, l, r) => format!(
            "{} >= {}",
            cond_with_ctx(
                l,
                param,
                field,
                local_vars,
                parent_upper,
                current_upper,
                peer_map
            ),
            cond_with_ctx(
                r,
                param,
                field,
                local_vars,
                parent_upper,
                current_upper,
                peer_map
            )
        ),
        Condition::And(_, l, r) => format!(
            "({} && {})",
            cond_with_ctx(
                l,
                param,
                field,
                local_vars,
                parent_upper,
                current_upper,
                peer_map
            ),
            cond_with_ctx(
                r,
                param,
                field,
                local_vars,
                parent_upper,
                current_upper,
                peer_map
            )
        ),
        Condition::Or(_, l, r) => format!(
            "({} || {})",
            cond_with_ctx(
                l,
                param,
                field,
                local_vars,
                parent_upper,
                current_upper,
                peer_map
            ),
            cond_with_ctx(
                r,
                param,
                field,
                local_vars,
                parent_upper,
                current_upper,
                peer_map
            )
        ),
        Condition::Not(_, inner) => format!(
            "(!{})",
            cond_with_ctx(
                inner,
                param,
                field,
                local_vars,
                parent_upper,
                current_upper,
                peer_map
            )
        ),
        Condition::Parenthesis(_, inner) => format!(
            "({})",
            cond_with_ctx(
                inner,
                param,
                field,
                local_vars,
                parent_upper,
                current_upper,
                peer_map
            )
        ),
        Condition::BoolLiteral(_, b) => {
            if *b {
                "true".to_string()
            } else {
                "false".to_string()
            }
        }
        // Fallback to generic translation
        _ => condition_to_c(cond),
    }
}

/// Translate an expression with struct-field context.
fn expr_with_ctx(expr: &Expression, param: &str, field: &str, local_vars: &[String]) -> String {
    match expr {
        Expression::Variable(id) => {
            if local_vars.contains(&id.name) {
                if field.is_empty() {
                    format!("{}->{}", param, id.name)
                } else {
                    format!("{}->{}.{}", param, field, id.name)
                }
            } else {
                id.name.clone()
            }
        }
        Expression::ArraySubscript(_, id, idx) => {
            if local_vars.contains(&id.name) {
                if field.is_empty() {
                    format!("{}->{}[{}]", param, id.name, idx)
                } else {
                    format!("{}->{}.{}[{}]", param, field, id.name, idx)
                }
            } else {
                format!("{}[{}]", id.name, idx)
            }
        }
        Expression::Assign(_, l, r) => {
            // Handle bit-write: a.N = expr
            if let Expression::MemberAccess(_, base, but_grammar::ast::Member::Number(n)) =
                l.as_ref()
            {
                let base_str = expr_with_ctx(base, param, field, local_vars);
                let rhs_str = expr_with_ctx(r, param, field, local_vars);
                format!(
                    "{0} = (({0}) & ~(1UL << {1})) | (({2} & 1) << {1})",
                    base_str, n, rhs_str
                )
            } else {
                format!(
                    "{} = {}",
                    expr_with_ctx(l, param, field, local_vars),
                    expr_with_ctx(r, param, field, local_vars)
                )
            }
        }
        Expression::BoolLiteral(_, b) => {
            if *b {
                "true".to_string()
            } else {
                "false".to_string()
            }
        }
        _ => expr_to_c(expr),
    }
}

/// Convert a Property to C code with struct context.
fn property_with_ctx(
    prop: &Property,
    param: &str,
    field: &str,
    local_vars: &[String],
    ctx: &CodegenContext,
    level: usize,
) -> String {
    let pad = ctx.indent.level(level);
    match prop {
        Property::Expression(e) => {
            format!("{}{};\n", pad, expr_with_ctx(e, param, field, local_vars))
        }
        Property::Function(stmt) => stmt_with_ctx(stmt, param, field, local_vars, ctx, level),
    }
}

/// Convert a Statement to C code with struct context.
fn stmt_with_ctx(
    stmt: &but_grammar::ast::Statement,
    param: &str,
    field: &str,
    local_vars: &[String],
    ctx: &CodegenContext,
    level: usize,
) -> String {
    use but_grammar::ast::Statement;
    let pad = ctx.indent.level(level);
    match stmt {
        Statement::Block { statements, .. } => {
            let inner: String = statements
                .iter()
                .map(|s| stmt_with_ctx(s, param, field, local_vars, ctx, level + 1))
                .collect();
            format!("{}{{\n{}{}}}\n", pad, inner, pad)
        }
        Statement::Expression(_, e) => {
            format!("{}{};\n", pad, expr_with_ctx(e, param, field, local_vars))
        }
        Statement::If(_, cond, then_s, else_s) => {
            let peer_map = HashMap::new();
            let cond_str = cond_with_ctx(
                &cond_expr_to_cond(cond),
                param,
                field,
                local_vars,
                "",
                "",
                &peer_map,
            );
            let mut out = format!("{}if ({}) ", pad, cond_str);
            out.push_str(&stmt_with_ctx(then_s, param, field, local_vars, ctx, level));
            if let Some(else_body) = else_s {
                out.push_str(&format!("{}else ", pad));
                out.push_str(&stmt_with_ctx(
                    else_body, param, field, local_vars, ctx, level,
                ));
            }
            out
        }
        // Fall back to the standard stmt_to_c for other statement types
        _ => crate::condition::stmt_to_c(stmt, &ctx.indent, level),
    }
}

/// Hack: convert an Expression used as a condition into a Condition node for cond_with_ctx.
/// For simple cases (variable, comparison), this works. Otherwise falls back to expr_to_c.
fn cond_expr_to_cond(expr: &Expression) -> Condition {
    // Best effort: wrap in a "truthy" check
    // In practice, if statements in BuT use Conditions not Expressions for the predicate,
    // so this fallback may not be reached often.
    Condition::Variable(but_grammar::ast::Identifier {
        loc: but_grammar::ast::Loc::Implicit,
        name: expr_to_c(expr),
    })
}

// ── Helper functions ──────────────────────────────────────────────────────────

/// Format a C field declaration, handling array types: "uint8_t[2]" → "uint8_t name[2]".
fn format_field_decl(name: &str, ty: &str) -> String {
    if let Some(idx) = ty.find('[') {
        let base = &ty[..idx];
        let suffix = &ty[idx..];
        format!("{} {}{}", base, name, suffix)
    } else {
        format!("{} {}", ty, name)
    }
}

/// Collect all model names that appear as sub-models in any composition.
fn collect_sub_model_names(source: &SourceUnit) -> HashSet<String> {
    let mut set = HashSet::new();
    for part in &source.0 {
        if let SourceUnitPart::ModelDefinition(md) = part {
            if let Some(tree) = model_composition_tree(md) {
                for name in tree.leaves() {
                    set.insert(name);
                }
            }
        }
    }
    set
}

/// Find a model definition by name in the source unit.
fn find_model_in_source<'a>(source: &'a SourceUnit, name: &str) -> Option<&'a ModelDefinition> {
    for part in &source.0 {
        if let SourceUnitPart::ModelDefinition(md) = part {
            if md.name.as_ref().map(|n| n.name.as_str()) == Some(name) {
                return Some(md);
            }
        }
    }
    None
}

/// Find the start state name in a model (looks for StateKind::Start).
fn find_start_state(model: &ModelDefinition) -> Option<String> {
    for part in &model.parts {
        if let ModelPart::StateDefinition(sd) = part {
            if sd.kind == Some(but_grammar::ast::StateKind::Start) {
                return sd.name.as_ref().map(|n| n.name.clone());
            }
        }
    }
    // Fallback: look for `start -> StateName;` property syntax
    for part in &model.parts {
        if let ModelPart::PropertyDefinition(pd) = part {
            if pd.name.as_ref().map(|n| n.name.as_str()) == Some("start") {
                if let Property::Expression(Expression::Variable(id)) = &pd.value {
                    return Some(id.name.clone());
                }
            }
        }
    }
    None
}

/// Collect all state names from a model (all StateDefinitions, in order).
fn collect_state_names(model: &ModelDefinition) -> Vec<String> {
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

/// Collect local variable names (non-port let declarations) from a model.
fn collect_local_var_names(model: &ModelDefinition) -> Vec<String> {
    model
        .parts
        .iter()
        .filter_map(|p| {
            if let ModelPart::VariableDefinition(vd) = p {
                if vd
                    .attrs
                    .iter()
                    .any(|a| matches!(a, VariableAttribute::Portable(_)))
                {
                    return None;
                }
                vd.name.as_ref().map(|n| n.name.clone())
            } else {
                None
            }
        })
        .collect()
}

/// Find a state definition by name in a model.
fn find_state_in_model<'a>(
    model: &'a ModelDefinition,
    name: &str,
) -> Option<&'a but_grammar::ast::StateDefinition> {
    for part in &model.parts {
        if let ModelPart::StateDefinition(sd) = part {
            if sd.name.as_ref().map(|n| n.name.as_str()) == Some(name) {
                return Some(sd);
            }
        }
    }
    None
}

// ── Kept for compatibility with other modules ─────────────────────────────────

pub(crate) fn model_name(model: &ModelDefinition) -> String {
    let named: Option<String> = model
        .annotations
        .iter()
        .filter(|a| !a.glob)
        .flat_map(|a| a.args.iter().collect::<Vec<_>>())
        .filter(|a| matches!(a, Annotation::Function { .. }))
        .filter_map(|a| {
            if let Annotation::Function { name, args, .. } = a
                && name.name == "name"
                && !args.is_empty()
            {
                args.iter().find_map(|a| {
                    if let Annotation::String(s) = a {
                        Some(s.string.clone())
                    } else {
                        None
                    }
                })
            } else {
                None
            }
        })
        .next();
    if let Some(n) = named {
        return n;
    }
    model
        .name
        .as_ref()
        .map(|n| n.name.clone())
        .unwrap_or_else(|| "Model".to_string())
}

pub(crate) fn collect_states(model: &ModelDefinition) -> Vec<String> {
    collect_state_names(model)
}

pub(crate) fn find_start(model: &ModelDefinition) -> Option<String> {
    find_start_state(model)
}
