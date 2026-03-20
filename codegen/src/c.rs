use crate::CodegenContext;
use crate::behavior::{
    CompositionTree, find_end_property, find_terminal_states, model_composition_tree,
};
use crate::condition::{condition_to_c, expr_to_c, resolve_alias, type_to_c_ctx};
use crate::ltl::{extract_ltl_formulas, ltl_comments_c};
use but_grammar::ast::{
    Annotation, Condition, Expression, ModelDefinition, ModelPart, Property, SourceUnit,
    SourceUnitPart, StatePart, Type, VariableAttribute,
};
use std::collections::{HashMap, HashSet};

// ── Port kind classification ──────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
enum PortKind {
    Bit,
    Int,
    Real,
}

#[derive(Debug, Clone)]
struct PortInfo {
    address: i64,
    kind: PortKind,
}

/// Classify a type into PortKind (Bit/Int/Real).
fn classify_port_kind(ty: &Type, aliases: &HashMap<String, Type>) -> PortKind {
    use but_grammar::ast::Type;
    let resolved = resolve_alias(ty, aliases);
    match &resolved {
        // 1-bit types → Bit
        Type::Alias(id) if id.name == "bit" => PortKind::Bit,
        // Arrays of bits
        Type::Array { element_type, element_count, .. } => {
            if is_bit_type(element_type) {
                if *element_count == 1 {
                    PortKind::Bit
                } else {
                    PortKind::Int
                }
            } else {
                PortKind::Int
            }
        }
        // Rational / float → Real
        Type::Rational => PortKind::Real,
        Type::Alias(id) => match id.name.as_str() {
            "real" | "f32" | "f64" | "float" => PortKind::Real,
            "bool" => PortKind::Bit,
            "bit" => PortKind::Bit,
            _ => PortKind::Int,
        },
        Type::Bool => PortKind::Bit,
        _ => PortKind::Int,
    }
}

fn is_bit_type(ty: &Type) -> bool {
    match ty {
        Type::Alias(id) => id.name == "bit",
        _ => false,
    }
}

/// Extract integer address from initializer expression.
fn extract_port_address(expr: &Expression) -> Option<i64> {
    match expr {
        Expression::NumberLiteral(_, n) => Some(*n),
        Expression::HexNumberLiteral(_, s, _) => {
            i64::from_str_radix(s.trim_start_matches("0x").trim_start_matches("0X"), 16).ok()
        }
        _ => None,
    }
}

/// Build port_map from global_vars: port_name → PortInfo.
fn build_port_map(ctx: &CodegenContext) -> HashMap<String, PortInfo> {
    let mut map = HashMap::new();
    for vd in &ctx.global_vars {
        let is_port = vd.attrs.iter().any(|a| matches!(a, VariableAttribute::Portable(_)));
        if !is_port {
            continue;
        }
        let name = match &vd.name {
            Some(n) => n.name.clone(),
            None => continue,
        };
        let address = match &vd.initializer {
            Some(e) => match extract_port_address(e) {
                Some(a) => a,
                None => continue,
            },
            None => continue,
        };
        let kind = classify_port_kind(&vd.ty, &ctx.type_aliases);
        map.insert(name, PortInfo { address, kind });
    }
    map
}

/// Determine if the source has any port (global_vars with Portable attribute).
fn has_ports(ctx: &CodegenContext) -> bool {
    ctx.global_vars
        .iter()
        .any(|vd| vd.attrs.iter().any(|a| matches!(a, VariableAttribute::Portable(_))))
}

/// Convert port name to PORT_ADDRESS_XXX constant name.
fn port_address_name(port_name: &str) -> String {
    format!("PORT_ADDRESS_{}", port_name.to_uppercase())
}

/// C type string for port reads (used as const local var type in tick functions).
fn port_read_c_type(kind: &PortKind) -> &'static str {
    match kind {
        PortKind::Bit => "bool",
        PortKind::Int => "int",
        PortKind::Real => "float",
    }
}

/// Default value for port reads.
fn port_read_default(kind: &PortKind) -> &'static str {
    match kind {
        PortKind::Bit => "false",
        PortKind::Int => "0",
        PortKind::Real => "0.",
    }
}

/// Macro name for reading a port.
fn port_read_macro(kind: &PortKind) -> &'static str {
    match kind {
        PortKind::Bit => "read_bit_port",
        PortKind::Int => "read_int_port",
        PortKind::Real => "read_real_port",
    }
}

// ── Public API ────────────────────────────────────────────────────────────────

/// Generate a combined C header file for all models from a SourceUnit.
pub fn generate_c_header_all(source: &SourceUnit, base_name: &str, ctx: &CodegenContext) -> String {
    let guard = base_name.to_uppercase().replace('-', "_").replace('.', "_");
    let mut out = String::new();
    out.push_str("#pragma once\n");
    out.push_str(&format!("#ifndef {}_H__\n", guard));
    out.push_str(&format!("#define {}_H__\n\n", guard));
    out.push_str("#include <stdbool.h>\n");
    out.push_str("#include <stdint.h>\n");

    let with_ports = has_ports(ctx);
    if with_ports {
        out.push_str("#include <stdio.h>\n");
    }
    out.push('\n');

    // Type alias typedefs — skip reserved C keywords and types that produce arrays
    let mut alias_names: Vec<(&String, &but_grammar::ast::Type)> =
        ctx.type_aliases.iter().collect();
    alias_names.sort_by_key(|(k, _)| k.as_str());
    for (alias, ty) in &alias_names {
        let resolved = type_to_c_ctx(ty, &ctx.type_aliases);
        // Skip reserved C keywords
        if is_reserved_c_keyword(alias) {
            continue;
        }
        // Skip if resolved type contains array suffix (can't make a clean typedef)
        if !resolved.contains('[') {
            out.push_str(&format!("typedef {} {};\n", resolved, alias));
        }
    }
    let has_aliases = alias_names.iter().any(|(alias, ty)| {
        let resolved = type_to_c_ctx(ty, &ctx.type_aliases);
        !is_reserved_c_keyword(alias) && !resolved.contains('[')
    });
    if has_aliases {
        out.push('\n');
    }

    // Port-callback struct and macros (only if source has port declarations)
    if with_ports {
        out.push_str("struct Port {\n");
        out.push_str("  void *userdata;\n");
        out.push_str("  void (*port_bit_write)(int address, int bit, bool value, void *userdata);\n");
        out.push_str("  bool (*port_bit_read)(int address, int bit, void *userdata);\n");
        out.push_str("  void (*port_int_write)(int address, int value, void *userdata);\n");
        out.push_str("  int (*port_int_read)(int address, void *userdata);\n");
        out.push_str("  void (*port_real_write)(int address, float value, void *userdata);\n");
        out.push_str("  float (*port_real_read)(int address, void *userdata);\n");
        out.push_str("};\n\n");

        out.push_str("#define write_bit_port(port, address, bit, value)                              \\\n");
        out.push_str("  {                                                                            \\\n");
        out.push_str("    if ((port).port_bit_write != 0)                                            \\\n");
        out.push_str("      (port).port_bit_write(address, bit, value, (port).userdata);             \\\n");
        out.push_str("  }\n");
        out.push_str("#define write_int_port(port, address, value)                                   \\\n");
        out.push_str("  {                                                                            \\\n");
        out.push_str("    if ((port).port_int_write != 0)                                            \\\n");
        out.push_str("      (port).port_int_write(address, value, (port).userdata);                  \\\n");
        out.push_str("  }\n");
        out.push('\n');
        out.push_str("#define debug(message) printf(\"%s\\n\", message)\n");
        out.push('\n');
        out.push_str("#define read_int_port(port, address, def)                                      \\\n");
        out.push_str("  (((port).port_int_read != 0)                                                 \\\n");
        out.push_str("       ? (port).port_int_read(address, (port).userdata)                        \\\n");
        out.push_str("       : def)\n");
        out.push_str("#define read_real_port(port, address, def)                                     \\\n");
        out.push_str("  (((port).port_real_read != 0)                                                \\\n");
        out.push_str("       ? (port).port_real_read(address, (port).userdata)                       \\\n");
        out.push_str("       : def)\n");
        out.push_str("#define read_bit_port(port, address, bit, def)                                 \\\n");
        out.push_str("  (((port).port_bit_read != 0)                                                 \\\n");
        out.push_str("       ? (port).port_bit_read(address, bit, (port).userdata)                   \\\n");
        out.push_str("       : def)\n");
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
    out.push_str(&format!("#include \"{}.h\"\n", base_name.to_lowercase()));

    let port_map = build_port_map(ctx);

    // Port address defines
    if !port_map.is_empty() {
        out.push('\n');
        // Output in address order for determinism
        let mut ports_sorted: Vec<(&String, &PortInfo)> = port_map.iter().collect();
        ports_sorted.sort_by(|a, b| a.1.address.cmp(&b.1.address).then(a.0.cmp(b.0)));
        for (pname, pinfo) in &ports_sorted {
            out.push_str(&format!("#define {} {}\n", port_address_name(pname), pinfo.address));
        }
    }

    let sub_model_names = collect_sub_model_names(source);

    for part in &source.0 {
        if let SourceUnitPart::ModelDefinition(md) = part {
            let name = model_name(md);
            if sub_model_names.contains(&name) {
                continue;
            }
            out.push('\n');
            out.push_str(&gen_model_source(md, source, ctx, &port_map));
        }
    }
    out
}

/// Generate a C header for a single model.
pub fn generate_c_header(model: &ModelDefinition, ctx: &CodegenContext) -> String {
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
    out.push_str(&gen_model_struct(model, &empty_src, ctx));
    out.push('\n');
    out.push_str(&gen_model_declarations(model, ctx));
    out.push_str(&format!("#endif /* __{}_H__ */\n", upper));
    out
}

/// Generate a C source for a single model.
pub fn generate_c_source(model: &ModelDefinition, ctx: &CodegenContext) -> String {
    let empty_src = SourceUnit(vec![]);
    let port_map = build_port_map(ctx);
    gen_model_source(model, &empty_src, ctx, &port_map)
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
    let i4 = ctx.indent.level(4);
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
            if let Some(sub_md) = find_model_in_source(source, leaf) {
                // Check if the sub-model has delegating states that need sub-sub-model structs
                let delegating_states = collect_delegating_states(sub_md);

                out.push_str(&format!("{}struct {{\n", i1));

                // Sub-sub-model structs for delegating states
                for (del_state_name, del_impl_name) in &delegating_states {
                    let del_field = del_state_name.to_lowercase();
                    let del_upper = del_impl_name.to_uppercase();
                    // Try to find the delegated sub-model in source
                    let del_upper_prefix = format!("{}_{}", upper, leaf_upper);
                    out.push_str(&format!("{}struct {{\n", i2));
                    out.push_str(&format!("{}enum {{\n", i3));
                    out.push_str(&format!(
                        "{}{}_{}_{}_INIT,\n",
                        i4, upper, leaf_upper, del_upper
                    ));
                    // Find the delegated model states (skip "End" since we add _END explicitly)
                    if let Some(del_md) = find_model_in_source(source, del_impl_name) {
                        for s in collect_state_names(del_md) {
                            if s == "End" { continue; } // _END is added explicitly below
                            out.push_str(&format!(
                                "{}{}_{}_{}_{}",
                                i4, upper, leaf_upper, del_upper,
                                s.to_uppercase()
                            ));
                            out.push_str(",\n");
                        }
                    }
                    out.push_str(&format!(
                        "{}{}_{}_{}_END,\n",
                        i4, upper, leaf_upper, del_upper
                    ));
                    let _ = del_upper_prefix;
                    out.push_str(&format!("{}}} state;\n", i3));
                    // Local vars of the delegated model
                    if let Some(del_md) = find_model_in_source(source, del_impl_name) {
                        for part in &del_md.parts {
                            if let ModelPart::VariableDefinition(vd) = part {
                                if vd.attrs.iter().any(|a| matches!(a, VariableAttribute::Portable(_))) {
                                    continue;
                                }
                                if let Some(vname) = &vd.name {
                                    let ty = type_to_struct_field_c(&vd.ty, &ctx.type_aliases);
                                    out.push_str(&format!(
                                        "{}{};\n",
                                        i3,
                                        format_field_decl(&vname.name, &ty)
                                    ));
                                }
                            }
                        }
                    }
                    out.push_str(&format!("{}}} {};\n", i2, del_field));
                }

                // State enum for the sub-model (skip "End" since _END is added explicitly)
                out.push_str(&format!("{}enum {{\n", i2));
                out.push_str(&format!("{}{}_{}_INIT,\n", i3, upper, leaf_upper));
                for s in collect_state_names(sub_md) {
                    if s == "End" { continue; }
                    out.push_str(&format!(
                        "{}{}_{}_{},\n",
                        i3, upper, leaf_upper, s.to_uppercase()
                    ));
                }
                out.push_str(&format!("{}{}_{}_END,\n", i3, upper, leaf_upper));
                out.push_str(&format!("{}}} state;\n", i2));
                // Local variables of the sub-model (non-port)
                for part in &sub_md.parts {
                    if let ModelPart::VariableDefinition(vd) = part {
                        if vd.attrs.iter().any(|a| matches!(a, VariableAttribute::Portable(_))) {
                            continue;
                        }
                        if let Some(vname) = &vd.name {
                            let ty = type_to_struct_field_c(&vd.ty, &ctx.type_aliases);
                            out.push_str(&format!(
                                "{}{};\n",
                                i2,
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
        // Global vars embedded in the top-level struct (non-port, non-const)
        for vd in &ctx.global_vars {
            let is_port = vd.attrs.iter().any(|a| matches!(a, VariableAttribute::Portable(_)));
            let is_const = vd.attrs.iter().any(|a| matches!(a, VariableAttribute::Constant(_)));
            if is_port || is_const {
                continue;
            }
            if let Some(vname) = &vd.name {
                // Use base C type (not struct field type) — int/bool/etc
                let ty = type_to_global_c_type(&vd.ty, &ctx.type_aliases);
                out.push_str(&format!("{}{};\n", i1, format_field_decl(&vname.name, &ty)));
            }
        }
        // Composition phase state
        out.push_str(&format!("{}enum {{\n", i1));
        out.push_str(&format!("{}{}_IMPLEMENT_INIT, {}_IMPLEMENT_TICK, {}_IMPLEMENT_END", i2, upper, upper, upper));
        out.push_str(&format!(" }} state;\n"));
        // Port struct if ports exist
        if has_ports(ctx) {
            out.push_str(&format!("{}struct Port port;\n", i1));
        }
        // Own local variables of the composition model (non-port)
        for part in &model.parts {
            if let ModelPart::VariableDefinition(vd) = part {
                if vd.attrs.iter().any(|a| matches!(a, VariableAttribute::Portable(_))) {
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
                if vd.attrs.iter().any(|a| matches!(a, VariableAttribute::Portable(_))) {
                    continue;
                }
                if let Some(vname) = &vd.name {
                    let ty = type_to_struct_field_c(&vd.ty, &ctx.type_aliases);
                    out.push_str(&format!("{}{};\n", i1, format_field_decl(&vname.name, &ty)));
                }
            }
        }
    }

    out.push_str("};\n");
    out
}

/// Determine global-scope variable C type (not typedef-resolved struct field types).
/// For `int = [32: bit]` → `int`, `bool = bit` → `bool`.
fn type_to_global_c_type(ty: &Type, aliases: &HashMap<String, Type>) -> String {
    use but_grammar::ast::Type;
    let resolved = resolve_alias(ty, aliases);
    match &resolved {
        Type::Bool => "bool".to_string(),
        Type::Rational => "double".to_string(),
        Type::Alias(id) => match id.name.as_str() {
            "bit" => "uint8_t".to_string(),
            "bool" => "bool".to_string(),
            "int" | "i32" => "int".to_string(),
            "real" | "f32" | "f64" => "double".to_string(),
            "float" => "float".to_string(),
            "u8" | "byte" => "uint8_t".to_string(),
            "u16" => "uint16_t".to_string(),
            "u32" => "uint32_t".to_string(),
            "u64" => "uint64_t".to_string(),
            _ => type_to_c_ctx(ty, aliases),
        },
        // A [32: bit] array → int (if it came from `type int = [32: bit]`)
        Type::Array { element_type, element_count, .. } => {
            if is_bit_type(element_type) {
                match element_count {
                    1 => "bool".to_string(),
                    8 => "uint8_t".to_string(),
                    16 => "uint16_t".to_string(),
                    32 => "int".to_string(),
                    64 => "uint64_t".to_string(),
                    _ => type_to_c_ctx(ty, aliases),
                }
            } else {
                type_to_c_ctx(ty, aliases)
            }
        }
        _ => type_to_c_ctx(ty, aliases),
    }
}

/// Convert a type to a C type for use in struct fields.
/// Preserves sign information: `int`/`i32` → `int32_t`, not `uint32_t`.
/// `bool` → `bool`, `real` → `double`.
fn type_to_struct_field_c(ty: &Type, aliases: &HashMap<String, Type>) -> String {
    use but_grammar::ast::Type;
    // First check the original type before alias resolution for semantic names
    if let Type::Alias(id) = ty {
        match id.name.as_str() {
            "int" | "i32" => return "int32_t".to_string(),
            "bool" => return "bool".to_string(),
            "real" | "f64" => return "double".to_string(),
            "f32" | "float" => return "float".to_string(),
            _ => {}
        }
    }
    let resolved = resolve_alias(ty, aliases);
    match &resolved {
        Type::Bool => "bool".to_string(),
        Type::Rational => "double".to_string(),
        Type::Alias(id) => match id.name.as_str() {
            "bit" => "bool".to_string(),
            "bool" => "bool".to_string(),
            "int" | "i32" => "int32_t".to_string(),
            "real" | "f64" => "double".to_string(),
            "f32" | "float" => "float".to_string(),
            "u8" | "byte" => "uint8_t".to_string(),
            "u16" => "uint16_t".to_string(),
            "u32" => "uint32_t".to_string(),
            "u64" => "uint64_t".to_string(),
            _ => type_to_c_ctx(ty, aliases),
        },
        Type::Array { element_type, element_count, .. } => {
            if is_bit_type(element_type) {
                match element_count {
                    1 => "bool".to_string(),
                    8 => "uint8_t".to_string(),
                    16 => "uint16_t".to_string(),
                    32 => "int32_t".to_string(),
                    64 => "uint64_t".to_string(),
                    _ => type_to_c_ctx(ty, aliases),
                }
            } else {
                type_to_c_ctx(ty, aliases)
            }
        }
        _ => type_to_c_ctx(ty, aliases),
    }
}

/// Collect global (non-port, non-const) variable names from the context.
fn collect_global_var_names(ctx: &CodegenContext) -> Vec<String> {
    ctx.global_vars
        .iter()
        .filter(|vd| {
            !vd.attrs.iter().any(|a| matches!(a, VariableAttribute::Portable(_)))
                && !vd.attrs.iter().any(|a| matches!(a, VariableAttribute::Constant(_)))
        })
        .filter_map(|vd| vd.name.as_ref().map(|n| n.name.clone()))
        .collect()
}

/// Check if a type alias name is a reserved C keyword or primitive type we don't typedef.
fn is_reserved_c_keyword(name: &str) -> bool {
    matches!(
        name,
        "int" | "bool" | "float" | "double" | "char" | "long" | "short"
            | "void" | "unsigned" | "signed" | "real"
    )
}

/// Generate function declarations for a model.
fn gen_model_declarations(model: &ModelDefinition, ctx: &CodegenContext) -> String {
    let name = model_name(model);
    let param = format!("struct {} *{}", name, name.to_lowercase());
    let with_ports = has_ports(ctx);
    let mut out = String::new();
    out.push_str(&format!("void {}_init({});\n", name, param));
    out.push_str(&format!("void {}_tick({});\n", name, param));
    let terminals = find_terminal_states(model);
    let is_composition = model_composition_tree(model).is_some();
    if !terminals.is_empty() || is_composition {
        if with_ports {
            out.push_str(&format!(
                "bool {}_finished(const struct {} *{});\n",
                name,
                name,
                name.to_lowercase()
            ));
        } else {
            out.push_str(&format!("bool {}_finished({});\n", name, param));
        }
    }
    out.push_str(&format!("void {}_reset({});\n", name, param));
    out
}

// ── Source generation ─────────────────────────────────────────────────────────

/// Generate the C source for a model (composition or FSM).
fn gen_model_source(
    model: &ModelDefinition,
    source: &SourceUnit,
    ctx: &CodegenContext,
    port_map: &HashMap<String, PortInfo>,
) -> String {
    let name = model_name(model);
    let upper = name.to_uppercase();
    let param = name.to_lowercase();
    let i1 = ctx.indent.level(1);
    let i2 = ctx.indent.level(2);
    let i3 = ctx.indent.level(3);
    let with_ports = has_ports(ctx);

    let mut out = String::new();

    if let Some(tree) = model_composition_tree(model) {
        // --- Composition model ---
        let leaves = tree.leaves();

        // Build peer map: model_name → field_name
        let peer_map: HashMap<String, String> = leaves
            .iter()
            .map(|n| (n.clone(), n.to_lowercase()))
            .collect();

        // Sub-model static inline functions:
        // First pass — emit sub-sub-models (delegated implementations) in source definition order.
        // Collect all delegated impl names referenced by any leaf's delegating states.
        let mut all_del_impl_names: Vec<String> = Vec::new();
        let mut del_impl_to_leaf: HashMap<String, (String, String, String)> = HashMap::new(); // impl_name → (leaf, leaf_upper, field)
        for leaf in &leaves {
            let field = leaf.to_lowercase();
            let leaf_upper = leaf.to_uppercase();
            if let Some(sub_md) = find_model_in_source(source, leaf) {
                for (_del_state_name, del_impl_name) in collect_delegating_states(sub_md) {
                    if !del_impl_to_leaf.contains_key(&del_impl_name) {
                        all_del_impl_names.push(del_impl_name.clone());
                        del_impl_to_leaf.insert(del_impl_name, (leaf.clone(), leaf_upper.clone(), field.clone()));
                    }
                }
            }
        }
        // Emit sub-sub-model functions in source definition order (order models appear in source).
        for part in &source.0 {
            if let SourceUnitPart::ModelDefinition(del_md) = part {
                let del_impl_name = model_name(del_md);
                if let Some((leaf, leaf_upper, field)) = del_impl_to_leaf.get(&del_impl_name) {
                    let del_field = del_impl_name.to_lowercase();
                    let del_upper = del_impl_name.to_uppercase();
                    let del_path = format!("{}.{}", field, del_field);
                    let enum_prefix = format!("{}_{}", upper, leaf_upper);
                    out.push_str(&gen_delegated_sub_model_funcs(
                        del_md,
                        &name,
                        &upper,
                        leaf,
                        leaf_upper,
                        &del_field,
                        &del_impl_name,
                        &del_upper,
                        &del_path,
                        &enum_prefix,
                        &param,
                        port_map,
                        ctx,
                    ));
                }
            }
        }
        let _ = all_del_impl_names;

        // Second pass — emit leaf model functions (simple FSM or delegating FSM).
        for leaf in &leaves {
            let field = leaf.to_lowercase();
            let leaf_upper = leaf.to_uppercase();
            if let Some(sub_md) = find_model_in_source(source, leaf) {
                let delegating_states = collect_delegating_states(sub_md);
                if !delegating_states.is_empty() {
                    // Generate the FSM sub-model with delegating state logic
                    out.push_str(&gen_delegating_fsm_funcs(
                        sub_md,
                        &name,
                        &upper,
                        &field,
                        leaf,
                        &leaf_upper,
                        &param,
                        &peer_map,
                        port_map,
                        ctx,
                        with_ports,
                    ));
                } else {
                    out.push_str(&gen_sub_model_funcs(
                        sub_md,
                        &name,
                        &upper,
                        &field,
                        leaf,
                        &leaf_upper,
                        &param,
                        &peer_map,
                        port_map,
                        ctx,
                        with_ports,
                    ));
                }
            }
        }

        // --- Main composition functions ---
        out.push_str(&format!("// model: {}\n", name));

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
            out.push_str(&property_with_ctx(ep, &param, "", &[], port_map, ctx, 3));
        }
        out.push_str(&format!("{}break;\n", i3));
        out.push_str(&format!("{}}}\n", i2));

        out.push_str(&format!("{}}}\n", i1));
        out.push_str("}\n\n");

        // finished
        if with_ports {
            out.push_str(&format!(
                "bool {}_finished(const struct {} *{}) {{\n",
                name, name, param
            ));
        } else {
            out.push_str(&format!(
                "bool {}_finished(struct {} *{}) {{\n",
                name, name, param
            ));
        }
        out.push_str(&format!("{}assert({} != 0);\n", i1, param));
        out.push_str(&format!(
            "{}return {}->{} == {}_IMPLEMENT_END;\n",
            i1, param, "state", upper
        ));
        out.push_str("}\n");
    } else {
        // --- FSM model ---
        out.push_str(&gen_fsm_source(model, ctx, port_map));
    }

    out
}

// ── Sub-model funcs (for delegating states: sub-sub-models) ──────────────────

/// Generate sub-sub-model functions (e.g., Main_Controller_Detect_Angle_*).
#[allow(clippy::too_many_arguments)]
fn gen_delegated_sub_model_funcs(
    del_md: &ModelDefinition,
    parent_name: &str,
    parent_upper: &str,
    leaf_name: &str,
    leaf_upper: &str,
    _del_field: &str,
    del_name: &str,
    del_upper: &str,
    del_path: &str,  // e.g. "controller.detect_angle"
    _enum_prefix: &str,
    param: &str,
    port_map: &HashMap<String, PortInfo>,
    ctx: &CodegenContext,
) -> String {
    let i1 = ctx.indent.level(1);
    let inline = "__attribute__((always_inline)) static ";
    let struct_param = format!("struct {} *{}", parent_name, param);
    let mut out = String::new();

    let terminal_states = find_terminal_states(del_md);
    let func_prefix = format!("{}_{}_{}", parent_name, leaf_name, del_name);
    let enum_prefix = format!("{}_{}_{}", parent_upper, leaf_upper, del_upper);

    out.push_str(&format!("// model: {}\n", del_name));

    // reset
    out.push_str(&format!(
        "{}void {}_reset({}) {{\n",
        inline, func_prefix, struct_param
    ));
    out.push_str(&format!("{}assert({} != 0);\n", i1, param));
    out.push_str(&format!(
        "{}{}->{}.state = {}_INIT;\n",
        i1, param, del_path, enum_prefix
    ));
    out.push_str("}\n\n");

    // init
    out.push_str(&format!(
        "{}void {}_init({}) {{\n",
        inline, func_prefix, struct_param
    ));
    out.push_str(&format!("{}assert({} != 0);\n", i1, param));
    out.push_str(&format!("{}{}_reset({});\n", i1, func_prefix, param));
    out.push_str("}\n\n");

    // tick: emit port reads as const locals
    out.push_str(&format!(
        "{}void {}_tick({}) {{\n",
        inline, func_prefix, struct_param
    ));
    out.push_str(&format!("{}assert({} != 0);\n", i1, param));
    // Port reads for ports used in this model
    let used_ports = collect_used_ports_in_model(del_md, port_map);
    for pname in &used_ports {
        if let Some(pinfo) = port_map.get(pname) {
            out.push_str(&gen_port_read_local(pname, pinfo, param, &i1));
        }
    }
    // Switch on del_path.state
    out.push_str(&format!("{}switch ({}->{}.state) {{\n", i1, param, del_path));

    // INIT case
    out.push_str(&format!("{}case {}_INIT: {{\n", ctx.indent.level(1), enum_prefix));
    // Var initializations
    let del_local_vars = collect_local_var_names(del_md);
    for part in &del_md.parts {
        if let ModelPart::VariableDefinition(vd) = part {
            if vd.attrs.iter().any(|a| matches!(a, VariableAttribute::Portable(_))) {
                continue;
            }
            if let Some(vname) = &vd.name {
                if let Some(init) = &vd.initializer {
                    match init {
                        Expression::Initializer(_, vals) => {
                            for (i, v) in vals.iter().enumerate() {
                                out.push_str(&format!(
                                    "{}{}->{}.{}[{}] = {};\n",
                                    ctx.indent.level(2), param, del_path, vname.name, i,
                                    expr_with_ctx_ports(v, param, del_path, &del_local_vars, port_map, ctx)
                                ));
                            }
                        }
                        _ => {
                            out.push_str(&format!(
                                "{}{}->{}.{} = {};\n",
                                ctx.indent.level(2), param, del_path, vname.name,
                                expr_with_ctx_ports(init, param, del_path, &del_local_vars, port_map, ctx)
                            ));
                        }
                    }
                }
            }
        }
    }
    // Transition to start state
    if let Some(start) = find_start_state(del_md) {
        out.push_str(&format!(
            "{}{}->{}.state = {}_{};\n",
            ctx.indent.level(2), param, del_path, enum_prefix, start.to_uppercase()
        ));
    }
    out.push_str(&format!("{}break;\n", ctx.indent.level(2)));
    out.push_str(&format!("{}}}\n", ctx.indent.level(1)));

    // State cases
    let empty_peer_map: HashMap<String, String> = HashMap::new();
    for part in &del_md.parts {
        if let ModelPart::StateDefinition(sd) = part {
            let sname = sd.name.as_ref().map(|n| n.name.as_str()).unwrap_or("?");
            let sname_up = sname.to_uppercase();
            let is_terminal = terminal_states.iter().any(|t| t == sname);

            out.push_str(&format!(
                "{}case {}_{}: {{\n",
                ctx.indent.level(1), enum_prefix, sname_up
            ));

            // Transitions
            for sp in &sd.parts {
                if let StatePart::Reference(_, target, cond_opt) = sp {
                    let global_var_names = collect_global_var_names(ctx);
                    let cond_str = if let Some(cond) = cond_opt {
                        cond_with_ctx_ports(
                            cond,
                            param,
                            del_path,
                            &del_local_vars,
                            parent_upper,
                            &format!("{}_{}", leaf_upper, del_upper),
                            &empty_peer_map,
                            port_map,
                            &global_var_names,
                        )
                    } else {
                        "1".to_string()
                    };
                    out.push_str(&format!("{}if ({}) {{\n", ctx.indent.level(2), cond_str));

                    // Enter handler of target
                    if let Some(target_sd) = find_state_in_model(del_md, &target.name) {
                        for tsp in &target_sd.parts {
                            if let StatePart::PropertyDefinition(pd) = tsp {
                                if pd.name.as_ref().map(|n| n.name.as_str()) == Some("enter") {
                                    out.push_str(&property_with_ctx(
                                        &pd.value,
                                        param,
                                        del_path,
                                        &del_local_vars,
                                        port_map,
                                        ctx,
                                        3,
                                    ));
                                }
                            }
                        }
                    }

                    out.push_str(&format!(
                        "{}{}->{}.state = {}_{};\n",
                        ctx.indent.level(3), param, del_path, enum_prefix,
                        target.name.to_uppercase()
                    ));
                    out.push_str(&format!("{}}}\n", ctx.indent.level(2)));
                }
            }

            let _ = is_terminal;
            out.push_str(&format!("{}break;\n", ctx.indent.level(2)));
            out.push_str(&format!("{}}}\n", ctx.indent.level(1)));
        }
    }

    out.push_str(&format!("{}}}\n", i1));
    out.push_str("}\n");

    // finished (const if ports)
    if !terminal_states.is_empty() {
        out.push_str(&format!(
            "{}bool {}_finished(const struct {} *{}) {{\n",
            inline, func_prefix, parent_name, param
        ));
        out.push_str(&format!("{}assert({} != 0);\n", i1, param));
        let conds: Vec<String> = terminal_states
            .iter()
            .map(|s| {
                format!(
                    "{}->{}.state == {}_{}",
                    param, del_path, enum_prefix, s.to_uppercase()
                )
            })
            .collect();
        out.push_str(&format!("{}return {};\n", i1, conds.join(" || ")));
        out.push_str("}\n");
    }
    out.push('\n');

    out
}

/// Generate a sub-model (FSM) that has delegating states — the FSM level, not sub-sub-model.
/// e.g. Main_Controller_* where Controller has `state Detect_Angle = Detect_Angle { ... }`
#[allow(clippy::too_many_arguments)]
fn gen_delegating_fsm_funcs(
    sub_md: &ModelDefinition,
    parent_name: &str,
    parent_upper: &str,
    field: &str,
    field_name: &str,
    field_upper: &str,
    param: &str,
    peer_map: &HashMap<String, String>,
    port_map: &HashMap<String, PortInfo>,
    ctx: &CodegenContext,
    with_ports: bool,
) -> String {
    let i1 = ctx.indent.level(1);
    let inline = "__attribute__((always_inline)) static ";
    let struct_param = format!("struct {} *{}", parent_name, param);
    let mut out = String::new();

    let terminal_states = find_terminal_states(sub_md);
    let local_vars = collect_local_var_names(sub_md);
    let delegating_states = collect_delegating_states(sub_md);
    // Map: state_name → delegated_model_name
    let del_state_map: HashMap<String, String> = delegating_states.into_iter().collect();

    out.push_str(&format!("// model: {}\n", field_name));

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
    out.push_str(&format!("{}{}_{}_reset({});\n", i1, parent_name, field_name, param));
    out.push_str("}\n");

    // tick: port reads
    out.push_str(&format!(
        "{}void {}_{}_tick({}) {{\n",
        inline, parent_name, field_name, struct_param
    ));
    out.push_str(&format!("{}assert({} != 0);\n", i1, param));
    // Port reads for ports used in this FSM (not in delegated sub-models)
    let used_ports = collect_used_ports_in_model(sub_md, port_map);
    for pname in &used_ports {
        if let Some(pinfo) = port_map.get(pname) {
            out.push_str(&gen_port_read_local(pname, pinfo, param, &i1));
        }
    }

    out.push_str(&format!("{}switch ({}->{}.state) {{\n", i1, param, field));

    // INIT case
    out.push_str(&format!("{}case {}_{}_INIT: {{\n", ctx.indent.level(1), parent_upper, field_upper));
    // Var initializations
    for part in &sub_md.parts {
        if let ModelPart::VariableDefinition(vd) = part {
            if vd.attrs.iter().any(|a| matches!(a, VariableAttribute::Portable(_))) {
                continue;
            }
            if let Some(vname) = &vd.name {
                if let Some(init) = &vd.initializer {
                    match init {
                        Expression::Initializer(_, vals) => {
                            for (i, v) in vals.iter().enumerate() {
                                out.push_str(&format!(
                                    "{}{}->{}.{}[{}] = {};\n",
                                    ctx.indent.level(2), param, field, vname.name, i,
                                    expr_with_ctx_ports(v, param, field, &local_vars, port_map, ctx)
                                ));
                            }
                        }
                        _ => {
                            out.push_str(&format!(
                                "{}{}->{}.{} = {};\n",
                                ctx.indent.level(2), param, field, vname.name,
                                expr_with_ctx_ports(init, param, field, &local_vars, port_map, ctx)
                            ));
                        }
                    }
                }
            }
        }
    }
    // Transition to start
    if let Some(start) = find_start_state(sub_md) {
        out.push_str(&format!(
            "{}{}->{}.state = {}_{}_{};\n",
            ctx.indent.level(2), param, field, parent_upper, field_upper, start.to_uppercase()
        ));
    }
    out.push_str(&format!("{}break;\n", ctx.indent.level(2)));
    out.push_str(&format!("{}}}\n", ctx.indent.level(1)));

    // State cases
    for part in &sub_md.parts {
        if let ModelPart::StateDefinition(sd) = part {
            let sname = sd.name.as_ref().map(|n| n.name.as_str()).unwrap_or("?");
            let sname_up = sname.to_uppercase();

            out.push_str(&format!(
                "{}case {}_{}_{}: {{\n",
                ctx.indent.level(1), parent_upper, field_upper, sname_up
            ));

            // Check if this is a delegating state
            if let Some(del_impl_name) = del_state_map.get(sname) {
                let del_field = del_impl_name.to_lowercase();
                let del_upper = del_impl_name.to_uppercase();
                let del_path = format!("{}.{}", field, del_field);
                let del_func_prefix = format!("{}_{}_{}", parent_name, field_name, del_impl_name);
                let del_enum_prefix = format!("{}_{}_{}", parent_upper, field_upper, del_upper);

                // Tick the delegated sub-model
                out.push_str(&format!("{}{}_tick({});\n", ctx.indent.level(2), del_func_prefix, param));

                // Transitions from this delegating state
                for sp in &sd.parts {
                    if let StatePart::Reference(_, target, cond_opt) = sp {
                        let global_var_names = collect_global_var_names(ctx);
                        let cond_str = if let Some(cond) = cond_opt {
                            // The condition may reference S(SubModel) = End | reset pattern
                            cond_with_ctx_delegating(
                                cond,
                                param,
                                field,
                                &local_vars,
                                parent_upper,
                                field_upper,
                                peer_map,
                                port_map,
                                // For S(Detect_Angle) = End, we need the del sub-model state path
                                &del_path,
                                &del_enum_prefix,
                                sname, // current delegating state name
                                del_impl_name,
                                &global_var_names,
                            )
                        } else {
                            "1".to_string()
                        };

                        out.push_str(&format!("{}if ({}) {{\n", ctx.indent.level(2), cond_str));

                        // Enter handler of target (inline)
                        if let Some(target_sd) = find_state_in_model(sub_md, &target.name) {
                            for tsp in &target_sd.parts {
                                if let StatePart::PropertyDefinition(pd) = tsp {
                                    if pd.name.as_ref().map(|n| n.name.as_str()) == Some("enter") {
                                        out.push_str(&property_with_ctx(
                                            &pd.value,
                                            param,
                                            field,
                                            &local_vars,
                                            port_map,
                                            ctx,
                                            3,
                                        ));
                                    }
                                }
                            }
                        }
                        // If transitioning to another delegating state, also init that state's sub-model
                        if let Some(next_del_impl) = del_state_map.get(&target.name) {
                            let next_del_func = format!("{}_{}_{}", parent_name, field_name, next_del_impl);
                            out.push_str(&format!(
                                "{}{}_init({});\n",
                                ctx.indent.level(3), next_del_func, param
                            ));
                        }

                        out.push_str(&format!(
                            "{}{}->{}.state = {}_{}_{} ;\n",
                            ctx.indent.level(3), param, field, parent_upper, field_upper,
                            target.name.to_uppercase()
                        ));
                        out.push_str(&format!("{}}}\n", ctx.indent.level(2)));
                    }
                }
                let _ = del_func_prefix;
                let _ = del_enum_prefix;
            } else {
                // Normal (non-delegating) state
                for sp in &sd.parts {
                    if let StatePart::Reference(_, target, cond_opt) = sp {
                        let global_var_names = collect_global_var_names(ctx);
                        let cond_str = if let Some(cond) = cond_opt {
                            cond_with_ctx_ports(
                                cond,
                                param,
                                field,
                                &local_vars,
                                parent_upper,
                                field_upper,
                                peer_map,
                                port_map,
                                &global_var_names,
                            )
                        } else {
                            "1".to_string()
                        };

                        out.push_str(&format!("{}if ({}) {{\n", ctx.indent.level(2), cond_str));

                        // Enter handler of target
                        if let Some(target_sd) = find_state_in_model(sub_md, &target.name) {
                            for tsp in &target_sd.parts {
                                if let StatePart::PropertyDefinition(pd) = tsp {
                                    if pd.name.as_ref().map(|n| n.name.as_str()) == Some("enter") {
                                        out.push_str(&property_with_ctx(
                                            &pd.value,
                                            param,
                                            field,
                                            &local_vars,
                                            port_map,
                                            ctx,
                                            3,
                                        ));
                                    }
                                }
                            }
                        }
                        // If target is a delegating state, init its sub-model
                        if let Some(next_del_impl) = del_state_map.get(&target.name) {
                            let next_del_func = format!("{}_{}_{}", parent_name, field_name, next_del_impl);
                            out.push_str(&format!(
                                "{}{}_init({});\n",
                                ctx.indent.level(3), next_del_func, param
                            ));
                        }

                        out.push_str(&format!(
                            "{}{}->{}.state = {}_{}_{} ;\n",
                            ctx.indent.level(3), param, field, parent_upper, field_upper,
                            target.name.to_uppercase()
                        ));
                        out.push_str(&format!("{}}}\n", ctx.indent.level(2)));
                    }
                }
            }

            out.push_str(&format!("{}break;\n", ctx.indent.level(2)));
            out.push_str(&format!("{}}}\n", ctx.indent.level(1)));
        }
    }

    out.push_str(&format!("{}}}\n", i1));
    out.push_str("}\n");

    // finished
    let const_qualifier = if with_ports { "const " } else { "" };
    out.push_str(&format!(
        "{}bool {}_{}_finished({}struct {} *{}) {{\n",
        inline, parent_name, field_name, const_qualifier, parent_name, param
    ));
    out.push_str(&format!("{}assert({} != 0);\n", i1, param));
    if terminal_states.is_empty() {
        out.push_str(&format!("{}return false;\n", i1));
    } else {
        let conds: Vec<String> = terminal_states
            .iter()
            .map(|s| {
                format!(
                    "{}->{}.state == {}_{}_{} ",
                    param, field, parent_upper, field_upper, s.to_uppercase()
                )
            })
            .collect();
        out.push_str(&format!("{}return {};\n", i1, conds.join(" || ")));
    }
    out.push_str("}\n");

    out
}

/// Generate static inline sub-model functions for a composition model.
#[allow(clippy::too_many_arguments)]
fn gen_sub_model_funcs(
    sub_md: &ModelDefinition,
    parent_name: &str,
    parent_upper: &str,
    field: &str,
    field_name: &str,
    field_upper: &str,
    param: &str,
    peer_map: &HashMap<String, String>,
    port_map: &HashMap<String, PortInfo>,
    ctx: &CodegenContext,
    with_ports: bool,
) -> String {
    let i1 = ctx.indent.level(1);
    let inline = "__attribute__((always_inline)) static ";
    let struct_param = format!("struct {} *{}", parent_name, param);
    let mut out = String::new();

    let local_vars = collect_local_var_names(sub_md);
    let terminal_states = find_terminal_states(sub_md);

    out.push_str(&format!("// model: {}\n", field_name));

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
    out.push_str(&format!("{}{}_{}_reset({});\n", i1, parent_name, field_name, param));
    out.push_str("}\n");

    // tick
    out.push_str(&format!(
        "{}void {}_{}_tick({}) {{\n",
        inline, parent_name, field_name, struct_param
    ));
    out.push_str(&format!("{}assert({} != 0);\n", i1, param));
    // Emit port reads as const locals
    let used_ports = collect_used_ports_in_model(sub_md, port_map);
    for pname in &used_ports {
        if let Some(pinfo) = port_map.get(pname) {
            out.push_str(&gen_port_read_local(pname, pinfo, param, &i1));
        }
    }
    out.push_str(&format!("{}switch ({}->{}.state) {{\n", i1, param, field));

    // INIT case: set initial values, transition to start
    out.push_str(&format!(
        "{}case {}_{}_INIT: {{\n",
        ctx.indent.level(1), parent_upper, field_upper
    ));
    for part in &sub_md.parts {
        if let ModelPart::VariableDefinition(vd) = part {
            if vd.attrs.iter().any(|a| matches!(a, VariableAttribute::Portable(_))) {
                continue;
            }
            if let Some(vname) = &vd.name {
                if let Some(init) = &vd.initializer {
                    match init {
                        Expression::Initializer(_, vals) => {
                            for (i, v) in vals.iter().enumerate() {
                                out.push_str(&format!(
                                    "{}{}->{}.{}[{}] = {};\n",
                                    ctx.indent.level(2), param, field, vname.name, i,
                                    expr_with_ctx_ports(v, param, field, &local_vars, port_map, ctx)
                                ));
                            }
                        }
                        _ => {
                            out.push_str(&format!(
                                "{}{}->{}.{} = {};\n",
                                ctx.indent.level(2), param, field, vname.name,
                                expr_with_ctx_ports(init, param, field, &local_vars, port_map, ctx)
                            ));
                        }
                    }
                }
            }
        }
    }
    if let Some(start) = find_start_state(sub_md) {
        out.push_str(&format!(
            "{}{}->{}.state = {}_{}_{} ;\n",
            ctx.indent.level(2), param, field, parent_upper, field_upper, start.to_uppercase()
        ));
    }
    out.push_str(&format!("{}break;\n", ctx.indent.level(2)));
    out.push_str(&format!("{}}}\n", ctx.indent.level(1)));

    // Regular state cases
    for part in &sub_md.parts {
        if let ModelPart::StateDefinition(sd) = part {
            let sname = sd.name.as_ref().map(|n| n.name.as_str()).unwrap_or("?");
            let sname_up = sname.to_uppercase();
            let is_terminal = terminal_states.iter().any(|t| t == sname);

            out.push_str(&format!(
                "{}case {}_{}_{}: {{\n",
                ctx.indent.level(1), parent_upper, field_upper, sname_up
            ));

            // Transitions
            for sp in &sd.parts {
                if let StatePart::Reference(_, target, cond_opt) = sp {
                    let global_var_names = collect_global_var_names(ctx);
                    let cond_str = if let Some(cond) = cond_opt {
                        cond_with_ctx_ports(
                            cond,
                            param,
                            field,
                            &local_vars,
                            parent_upper,
                            field_upper,
                            peer_map,
                            port_map,
                            &global_var_names,
                        )
                    } else {
                        "1".to_string()
                    };
                    out.push_str(&format!("{}if ({}) {{\n", ctx.indent.level(2), cond_str));

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
                                        port_map,
                                        ctx,
                                        3,
                                    ));
                                }
                            }
                        }
                    }

                    out.push_str(&format!(
                        "{}{}->{}.state = {}_{}_{} ;\n",
                        ctx.indent.level(3),
                        param,
                        field,
                        parent_upper,
                        field_upper,
                        target.name.to_uppercase()
                    ));
                    out.push_str(&format!("{}}}\n", ctx.indent.level(2)));
                }
            }

            let _ = is_terminal;
            out.push_str(&format!("{}break;\n", ctx.indent.level(2)));
            out.push_str(&format!("{}}}\n", ctx.indent.level(1)));
        }
    }

    out.push_str(&format!("{}}}\n", i1));
    out.push_str("}\n");

    // finished
    let const_qualifier = if with_ports { "const " } else { "" };
    out.push_str(&format!(
        "{}bool {}_{}_finished({}struct {} *{}) {{\n",
        inline, parent_name, field_name, const_qualifier, parent_name, param
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
fn gen_fsm_source(
    model: &ModelDefinition,
    ctx: &CodegenContext,
    port_map: &HashMap<String, PortInfo>,
) -> String {
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
    let with_ports = !port_map.is_empty();

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
    // Port reads
    let used_ports = collect_used_ports_in_model(model, port_map);
    for pname in &used_ports {
        if let Some(pinfo) = port_map.get(pname) {
            out.push_str(&gen_port_read_local(pname, pinfo, &param, &i1));
        }
    }
    out.push_str(&format!("{}switch ({}->{}) {{\n", i1, param, "state"));

    // INIT case
    out.push_str(&format!("{}case {}_INIT: {{\n", i2, upper));
    for part in &model.parts {
        if let ModelPart::VariableDefinition(vd) = part {
            if vd.attrs.iter().any(|a| matches!(a, VariableAttribute::Portable(_))) {
                continue;
            }
            if let Some(vname) = &vd.name {
                if let Some(init) = &vd.initializer {
                    match init {
                        Expression::Initializer(_, vals) => {
                            for (i, v) in vals.iter().enumerate() {
                                out.push_str(&format!(
                                    "{}{}->{}[{}] = {};\n",
                                    i3, param, vname.name, i,
                                    expr_with_ctx(v, &param, "", &local_vars)
                                ));
                            }
                        }
                        _ => {
                            out.push_str(&format!(
                                "{}{}->{} = {};\n",
                                i3, param, vname.name,
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
                            port_map,
                            ctx,
                            3,
                        ));
                    }
                }
            }
        }
        out.push_str(&format!(
            "{}{}->{} = {}_{};\n",
            i3, param, "state", upper, start.to_uppercase()
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

            let empty_peer_map: HashMap<String, String> = HashMap::new();
            let global_var_names = collect_global_var_names(ctx);
            // Transitions
            for sp in &sd.parts {
                if let StatePart::Reference(_, target, cond_opt) = sp {
                    let cond_str = if let Some(cond) = cond_opt {
                        cond_with_ctx_ports(cond, &param, "", &local_vars, &upper, "", &empty_peer_map, port_map, &global_var_names)
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
                                        port_map,
                                        ctx,
                                        4,
                                    ));
                                }
                            }
                        }
                    }

                    out.push_str(&format!(
                        "{}{}->{} = {}_{};\n",
                        i4, param, "state", upper, target.name.to_uppercase()
                    ));
                    out.push_str(&format!("{}}}\n", i3));
                }
            }

            // End handler for terminal states
            if is_terminal {
                if let Some(ep) = find_end_property(model) {
                    out.push_str(&format!("{}/* end handler */\n", i3));
                    out.push_str(&property_with_ctx(ep, &param, "", &local_vars, port_map, ctx, 3));
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
        let const_qualifier = if with_ports { "const " } else { "" };
        out.push_str(&format!("bool {}_finished({}struct {} *{}) {{\n", name, const_qualifier, name, param));
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

// ── Port read helpers ─────────────────────────────────────────────────────────

/// Generate a const port read local variable declaration.
/// `const float angle = read_real_port(main->port, PORT_ADDRESS_ANGLE, 0.);`
fn gen_port_read_local(pname: &str, pinfo: &PortInfo, param: &str, pad: &str) -> String {
    let c_type = port_read_c_type(&pinfo.kind);
    let def = port_read_default(&pinfo.kind);
    let macro_name = port_read_macro(&pinfo.kind);
    let addr_const = port_address_name(pname);
    match pinfo.kind {
        PortKind::Bit => format!(
            "{}const {} {} = {}({}->{}, {}, 0, {});\n",
            pad, c_type, pname, macro_name, param, "port", addr_const, def
        ),
        PortKind::Int => format!(
            "{}const {} {} = {}({}->{}, {}, {});\n",
            pad, c_type, pname, macro_name, param, "port", addr_const, def
        ),
        PortKind::Real => format!(
            "{}const {} {} = {}({}->{}, {}, {});\n",
            pad, c_type, pname, macro_name, param, "port", addr_const, def
        ),
    }
}

/// Generate a port write expression string.
fn gen_port_write(pname: &str, pinfo: &PortInfo, param: &str, value_str: &str) -> String {
    let addr_const = port_address_name(pname);
    match pinfo.kind {
        PortKind::Bit => format!(
            "write_bit_port({}->{}, 0, {}, {})",
            param, "port", addr_const, value_str
        ),
        PortKind::Int | PortKind::Real => format!(
            "write_int_port({}->{}, {}, {})",
            param, "port", addr_const, value_str
        ),
    }
}

/// Collect the set of port names used in a model's conditions and expressions.
fn collect_used_ports_in_model(model: &ModelDefinition, port_map: &HashMap<String, PortInfo>) -> Vec<String> {
    let mut used: Vec<String> = Vec::new();
    let mut seen: HashSet<String> = HashSet::new();
    for (pname, _) in port_map {
        if is_port_used_in_model(model, pname) {
            if seen.insert(pname.clone()) {
                used.push(pname.clone());
            }
        }
    }
    // Sort by address for deterministic output
    used.sort_by_key(|n| port_map.get(n).map(|p| p.address).unwrap_or(0));
    used
}

fn is_port_used_in_model(model: &ModelDefinition, pname: &str) -> bool {
    for part in &model.parts {
        match part {
            ModelPart::StateDefinition(sd) => {
                for sp in &sd.parts {
                    match sp {
                        StatePart::Reference(_, _, Some(cond)) => {
                            if cond_contains_var(cond, pname) {
                                return true;
                            }
                        }
                        StatePart::PropertyDefinition(pd) => {
                            if property_contains_var(&pd.value, pname) {
                                return true;
                            }
                        }
                        _ => {}
                    }
                }
            }
            ModelPart::PropertyDefinition(pd) => {
                if property_contains_var(&pd.value, pname) {
                    return true;
                }
            }
            _ => {}
        }
    }
    false
}

fn cond_contains_var(cond: &Condition, name: &str) -> bool {
    match cond {
        Condition::Variable(id) => id.name == name,
        Condition::Equal(_, l, r)
        | Condition::NotEqual(_, l, r)
        | Condition::Less(_, l, r)
        | Condition::LessEqual(_, l, r)
        | Condition::More(_, l, r)
        | Condition::MoreEqual(_, l, r)
        | Condition::And(_, l, r)
        | Condition::Or(_, l, r)
        | Condition::Add(_, l, r)
        | Condition::Subtract(_, l, r) => cond_contains_var(l, name) || cond_contains_var(r, name),
        Condition::Not(_, inner) | Condition::Parenthesis(_, inner) => {
            cond_contains_var(inner, name)
        }
        Condition::FunctionCall(_, _, args) => args.iter().any(|a| cond_contains_var(a, name)),
        _ => false,
    }
}

fn property_contains_var(prop: &Property, name: &str) -> bool {
    match prop {
        Property::Expression(e) => expr_contains_var(e, name),
        Property::Function(s) => stmt_contains_var(s, name),
    }
}

fn expr_contains_var(expr: &Expression, name: &str) -> bool {
    match expr {
        Expression::Variable(id) => id.name == name,
        Expression::Assign(_, l, r) => expr_contains_var(l, name) || expr_contains_var(r, name),
        Expression::Add(_, l, r)
        | Expression::Subtract(_, l, r)
        | Expression::Multiply(_, l, r)
        | Expression::Divide(_, l, r) => {
            expr_contains_var(l, name) || expr_contains_var(r, name)
        }
        _ => false,
    }
}

fn stmt_contains_var(stmt: &but_grammar::ast::Statement, name: &str) -> bool {
    use but_grammar::ast::Statement;
    match stmt {
        Statement::Block { statements, .. } => {
            statements.iter().any(|s| stmt_contains_var(s, name))
        }
        Statement::Expression(_, e) => expr_contains_var(e, name),
        Statement::If(_, _, then_s, else_s) => {
            stmt_contains_var(then_s, name)
                || else_s
                    .as_ref()
                    .map(|s| stmt_contains_var(s, name))
                    .unwrap_or(false)
        }
        _ => false,
    }
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
            if !all_done.is_empty() {
                out.push_str(&format!("{}if ({}) {{\n", i3, all_done.join(" && ")));
                out.push_str(&format!(
                    "{}{}->{} = {}_IMPLEMENT_END;\n",
                    i4, param, "state", parent_upper
                ));
                out.push_str(&format!("{}}}\n", i3));
            }
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

// ── Context-aware condition translation (with port support) ──────────────────

/// Translate condition with struct context, resolving port names to local const vars.
#[allow(clippy::too_many_arguments)]
fn cond_with_ctx_ports(
    cond: &Condition,
    param: &str,
    field: &str,
    local_vars: &[String],
    parent_upper: &str,
    current_upper: &str,
    peer_map: &HashMap<String, String>,
    port_map: &HashMap<String, PortInfo>,
    global_var_names: &[String],
) -> String {
    match cond {
        Condition::Variable(id) => {
            let name = &id.name;
            // Port names are used as const local vars (no prefix needed)
            if port_map.contains_key(name.as_str()) {
                return name.clone();
            }
            if local_vars.contains(name) {
                if field.is_empty() {
                    format!("{}->{}", param, name)
                } else {
                    format!("{}->{}.{}", param, field, name)
                }
            } else if global_var_names.contains(name) {
                format!("{}->{}", param, name)
            } else {
                name.clone()
            }
        }
        Condition::ArraySubscript(_, id, idx) => {
            if port_map.contains_key(id.name.as_str()) {
                return format!("{}[{}]", id.name, idx);
            }
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
            let ls = cond_with_ctx_ports(l, param, field, local_vars, parent_upper, current_upper, peer_map, port_map, global_var_names);
            let rs = cond_with_ctx_ports(r, param, field, local_vars, parent_upper, current_upper, peer_map, port_map, global_var_names);
            format!("{} == {}", ls, rs)
        }
        Condition::NotEqual(_, l, r) => format!(
            "{} != {}",
            cond_with_ctx_ports(l, param, field, local_vars, parent_upper, current_upper, peer_map, port_map, global_var_names),
            cond_with_ctx_ports(r, param, field, local_vars, parent_upper, current_upper, peer_map, port_map, global_var_names)
        ),
        Condition::Less(_, l, r) => format!(
            "{} < {}",
            cond_with_ctx_ports(l, param, field, local_vars, parent_upper, current_upper, peer_map, port_map, global_var_names),
            cond_with_ctx_ports(r, param, field, local_vars, parent_upper, current_upper, peer_map, port_map, global_var_names)
        ),
        Condition::LessEqual(_, l, r) => format!(
            "{} <= {}",
            cond_with_ctx_ports(l, param, field, local_vars, parent_upper, current_upper, peer_map, port_map, global_var_names),
            cond_with_ctx_ports(r, param, field, local_vars, parent_upper, current_upper, peer_map, port_map, global_var_names)
        ),
        Condition::More(_, l, r) => format!(
            "{} > {}",
            cond_with_ctx_ports(l, param, field, local_vars, parent_upper, current_upper, peer_map, port_map, global_var_names),
            cond_with_ctx_ports(r, param, field, local_vars, parent_upper, current_upper, peer_map, port_map, global_var_names)
        ),
        Condition::MoreEqual(_, l, r) => format!(
            "{} >= {}",
            cond_with_ctx_ports(l, param, field, local_vars, parent_upper, current_upper, peer_map, port_map, global_var_names),
            cond_with_ctx_ports(r, param, field, local_vars, parent_upper, current_upper, peer_map, port_map, global_var_names)
        ),
        Condition::And(_, l, r) => format!(
            "({} && {})",
            cond_with_ctx_ports(l, param, field, local_vars, parent_upper, current_upper, peer_map, port_map, global_var_names),
            cond_with_ctx_ports(r, param, field, local_vars, parent_upper, current_upper, peer_map, port_map, global_var_names)
        ),
        Condition::Or(_, l, r) => format!(
            "({} || {})",
            cond_with_ctx_ports(l, param, field, local_vars, parent_upper, current_upper, peer_map, port_map, global_var_names),
            cond_with_ctx_ports(r, param, field, local_vars, parent_upper, current_upper, peer_map, port_map, global_var_names)
        ),
        Condition::Not(_, inner) => format!(
            "(!{})",
            cond_with_ctx_ports(inner, param, field, local_vars, parent_upper, current_upper, peer_map, port_map, global_var_names)
        ),
        Condition::Parenthesis(_, inner) => format!(
            "({})",
            cond_with_ctx_ports(inner, param, field, local_vars, parent_upper, current_upper, peer_map, port_map, global_var_names)
        ),
        Condition::BoolLiteral(_, b) => {
            if *b {
                "true".to_string()
            } else {
                "false".to_string()
            }
        }
        Condition::Add(_, l, r) => format!(
            "({} + {})",
            cond_with_ctx_ports(l, param, field, local_vars, parent_upper, current_upper, peer_map, port_map, global_var_names),
            cond_with_ctx_ports(r, param, field, local_vars, parent_upper, current_upper, peer_map, port_map, global_var_names)
        ),
        Condition::Subtract(_, l, r) => format!(
            "({} - {})",
            cond_with_ctx_ports(l, param, field, local_vars, parent_upper, current_upper, peer_map, port_map, global_var_names),
            cond_with_ctx_ports(r, param, field, local_vars, parent_upper, current_upper, peer_map, port_map, global_var_names)
        ),
        // Fallback to generic translation
        _ => condition_to_c(cond),
    }
}

/// Translate condition for delegating state context.
/// Handles `S(SubModelName) = State | reset` pattern.
#[allow(clippy::too_many_arguments)]
fn cond_with_ctx_delegating(
    cond: &Condition,
    param: &str,
    field: &str,
    local_vars: &[String],
    parent_upper: &str,
    field_upper: &str,
    peer_map: &HashMap<String, String>,
    port_map: &HashMap<String, PortInfo>,
    del_path: &str,
    del_enum_prefix: &str,
    _delegating_state_name: &str,
    del_impl_name: &str,
    global_var_names: &[String],
) -> String {
    match cond {
        Condition::Variable(id) => {
            let name = &id.name;
            // Port names → const local vars (already hoisted)
            if port_map.contains_key(name.as_str()) {
                return name.clone();
            }
            // local_vars → param->field.varname
            if local_vars.contains(name) {
                if field.is_empty() {
                    return format!("{}->{}", param, name);
                } else {
                    return format!("{}->{}.{}", param, field, name);
                }
            }
            // Global vars (watchdog, reset etc.) → param->varname
            if global_var_names.contains(name) {
                return format!("{}->{}", param, name);
            }
            name.clone()
        }
        Condition::Equal(_, l, r) => {
            if let Condition::FunctionCall(_, s_id, s_args) = l.as_ref() {
                if s_id.name == "S" && s_args.len() == 1 {
                    if let Condition::Variable(model_var) = &s_args[0] {
                        if model_var.name == del_impl_name {
                            // Detect S(SubModel) = State | extra_cond pattern
                            // The DSL `S(X) = End | reset` parses as Equal(S(X), Or(End, reset))
                            // We expand it to `state == END || extra_cond`
                            if let Condition::Or(_, or_l, or_r) = r.as_ref() {
                                if let Condition::Variable(state_var) = or_l.as_ref() {
                                    let su = state_var.name.to_uppercase();
                                    let state_check = format!(
                                        "{}->{}.state == {}_{}",
                                        param, del_path, del_enum_prefix, su
                                    );
                                    let extra = cond_with_ctx_delegating(or_r, param, field, local_vars, parent_upper, field_upper, peer_map, port_map, del_path, del_enum_prefix, _delegating_state_name, del_impl_name, global_var_names);
                                    return format!("{} || {}", state_check, extra);
                                }
                            }
                            // Detect S(SubModel) = StateEnum simple pattern
                            if let Condition::Variable(state_var) = r.as_ref() {
                                let su = state_var.name.to_uppercase();
                                return format!(
                                    "{}->{}.state == {}_{}",
                                    param, del_path, del_enum_prefix, su
                                );
                            }
                        }
                    }
                }
            }
            let ls = cond_with_ctx_delegating(cond_inner_l(cond), param, field, local_vars, parent_upper, field_upper, peer_map, port_map, del_path, del_enum_prefix, _delegating_state_name, del_impl_name, global_var_names);
            let rs = cond_with_ctx_delegating(cond_inner_r(cond), param, field, local_vars, parent_upper, field_upper, peer_map, port_map, del_path, del_enum_prefix, _delegating_state_name, del_impl_name, global_var_names);
            format!("{} == {}", ls, rs)
        }
        Condition::Or(_, l, r) => format!(
            "({} || {})",
            cond_with_ctx_delegating(l, param, field, local_vars, parent_upper, field_upper, peer_map, port_map, del_path, del_enum_prefix, _delegating_state_name, del_impl_name, global_var_names),
            cond_with_ctx_delegating(r, param, field, local_vars, parent_upper, field_upper, peer_map, port_map, del_path, del_enum_prefix, _delegating_state_name, del_impl_name, global_var_names)
        ),
        Condition::And(_, l, r) => format!(
            "({} && {})",
            cond_with_ctx_delegating(l, param, field, local_vars, parent_upper, field_upper, peer_map, port_map, del_path, del_enum_prefix, _delegating_state_name, del_impl_name, global_var_names),
            cond_with_ctx_delegating(r, param, field, local_vars, parent_upper, field_upper, peer_map, port_map, del_path, del_enum_prefix, _delegating_state_name, del_impl_name, global_var_names)
        ),
        Condition::Not(_, inner) => format!(
            "(!{})",
            cond_with_ctx_delegating(inner, param, field, local_vars, parent_upper, field_upper, peer_map, port_map, del_path, del_enum_prefix, _delegating_state_name, del_impl_name, global_var_names)
        ),
        Condition::BoolLiteral(_, b) => if *b { "true".to_string() } else { "false".to_string() },
        _ => cond_with_ctx_ports(cond, param, field, local_vars, parent_upper, field_upper, peer_map, port_map, global_var_names),
    }
}

fn cond_inner_l(cond: &Condition) -> &Condition {
    match cond {
        Condition::Equal(_, l, _) | Condition::NotEqual(_, l, _) => l,
        _ => cond,
    }
}

fn cond_inner_r(cond: &Condition) -> &Condition {
    match cond {
        Condition::Equal(_, _, r) | Condition::NotEqual(_, _, r) => r,
        _ => cond,
    }
}

/// Translate an expression with struct-field context (port-aware version).
fn expr_with_ctx_ports(
    expr: &Expression,
    param: &str,
    field: &str,
    local_vars: &[String],
    port_map: &HashMap<String, PortInfo>,
    ctx: &CodegenContext,
) -> String {
    let global_var_names = collect_global_var_names(ctx);
    match expr {
        Expression::Variable(id) => {
            let name = &id.name;
            if port_map.contains_key(name.as_str()) {
                // Port name — used as const local or written via macro
                return name.clone();
            }
            if local_vars.contains(name) {
                if field.is_empty() {
                    format!("{}->{}", param, name)
                } else {
                    format!("{}->{}.{}", param, field, name)
                }
            } else if global_var_names.contains(name) {
                format!("{}->{}", param, name)
            } else {
                name.clone()
            }
        }
        Expression::ArraySubscript(_, id, idx) => {
            if port_map.contains_key(id.name.as_str()) {
                return format!("{}[{}]", id.name, idx);
            }
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
            // Port assignment → write_xxx_port macro
            if let Expression::Variable(id) = l.as_ref() {
                if let Some(pinfo) = port_map.get(id.name.as_str()) {
                    let rhs = expr_with_ctx_ports(r, param, field, local_vars, port_map, ctx);
                    return gen_port_write(&id.name, pinfo, param, &rhs);
                }
            }
            // Check global vars (not in local_vars, not ports) → param->varname
            if let Expression::Variable(id) = l.as_ref() {
                // If it looks like a global var (starts lowercase, is not in local_vars)
                let rhs = expr_with_ctx_ports(r, param, field, local_vars, port_map, ctx);
                if !local_vars.contains(&id.name) {
                    // Could be a global var from the context (watchdog, reset, etc.)
                    // We check by seeing if it might be a global: we'll try to disambiguate
                    // For now, pass through as normal expr_with_ctx behavior
                }
                let lhs = expr_with_ctx_ports(l, param, field, local_vars, port_map, ctx);
                return format!("{} = {}", lhs, rhs);
            }
            // Handle bit-write: a.N = expr
            if let Expression::MemberAccess(_, base, but_grammar::ast::Member::Number(n)) =
                l.as_ref()
            {
                let base_str = expr_with_ctx_ports(base, param, field, local_vars, port_map, ctx);
                let rhs_str = expr_with_ctx_ports(r, param, field, local_vars, port_map, ctx);
                return format!(
                    "{0} = (({0}) & ~(1UL << {1})) | (({2} & 1) << {1})",
                    base_str, n, rhs_str
                );
            }
            let lhs = expr_with_ctx_ports(l, param, field, local_vars, port_map, ctx);
            let rhs = expr_with_ctx_ports(r, param, field, local_vars, port_map, ctx);
            format!("{} = {}", lhs, rhs)
        }
        Expression::BoolLiteral(_, b) => {
            if *b {
                "true".to_string()
            } else {
                "false".to_string()
            }
        }
        Expression::Add(_, l, r) => format!(
            "{} + {}",
            expr_with_ctx_ports(l, param, field, local_vars, port_map, ctx),
            expr_with_ctx_ports(r, param, field, local_vars, port_map, ctx)
        ),
        Expression::Subtract(_, l, r) => format!(
            "{} - {}",
            expr_with_ctx_ports(l, param, field, local_vars, port_map, ctx),
            expr_with_ctx_ports(r, param, field, local_vars, port_map, ctx)
        ),
        Expression::Multiply(_, l, r) => format!(
            "{} * {}",
            expr_with_ctx_ports(l, param, field, local_vars, port_map, ctx),
            expr_with_ctx_ports(r, param, field, local_vars, port_map, ctx)
        ),
        Expression::Divide(_, l, r) => format!(
            "{} / {}",
            expr_with_ctx_ports(l, param, field, local_vars, port_map, ctx),
            expr_with_ctx_ports(r, param, field, local_vars, port_map, ctx)
        ),
        Expression::Equal(_, l, r) => format!(
            "{} == {}",
            expr_with_ctx_ports(l, param, field, local_vars, port_map, ctx),
            expr_with_ctx_ports(r, param, field, local_vars, port_map, ctx)
        ),
        Expression::NotEqual(_, l, r) => format!(
            "{} != {}",
            expr_with_ctx_ports(l, param, field, local_vars, port_map, ctx),
            expr_with_ctx_ports(r, param, field, local_vars, port_map, ctx)
        ),
        Expression::Less(_, l, r) => format!(
            "{} < {}",
            expr_with_ctx_ports(l, param, field, local_vars, port_map, ctx),
            expr_with_ctx_ports(r, param, field, local_vars, port_map, ctx)
        ),
        Expression::LessEqual(_, l, r) => format!(
            "{} <= {}",
            expr_with_ctx_ports(l, param, field, local_vars, port_map, ctx),
            expr_with_ctx_ports(r, param, field, local_vars, port_map, ctx)
        ),
        Expression::More(_, l, r) => format!(
            "{} > {}",
            expr_with_ctx_ports(l, param, field, local_vars, port_map, ctx),
            expr_with_ctx_ports(r, param, field, local_vars, port_map, ctx)
        ),
        Expression::MoreEqual(_, l, r) => format!(
            "{} >= {}",
            expr_with_ctx_ports(l, param, field, local_vars, port_map, ctx),
            expr_with_ctx_ports(r, param, field, local_vars, port_map, ctx)
        ),
        Expression::And(_, l, r) => format!(
            "({} && {})",
            expr_with_ctx_ports(l, param, field, local_vars, port_map, ctx),
            expr_with_ctx_ports(r, param, field, local_vars, port_map, ctx)
        ),
        Expression::Or(_, l, r) => format!(
            "({} || {})",
            expr_with_ctx_ports(l, param, field, local_vars, port_map, ctx),
            expr_with_ctx_ports(r, param, field, local_vars, port_map, ctx)
        ),
        Expression::Not(_, e) => format!(
            "!{}",
            expr_with_ctx_ports(e, param, field, local_vars, port_map, ctx)
        ),
        Expression::Negate(_, e) => format!(
            "-{}",
            expr_with_ctx_ports(e, param, field, local_vars, port_map, ctx)
        ),
        Expression::Parenthesis(_, e) => format!(
            "({})",
            expr_with_ctx_ports(e, param, field, local_vars, port_map, ctx)
        ),
        _ => expr_to_c(expr),
    }
}

/// Translate an expression with struct-field context (original, non-port-aware version).
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

/// Convert a Property to C code with struct context and port-write support.
fn property_with_ctx(
    prop: &Property,
    param: &str,
    field: &str,
    local_vars: &[String],
    port_map: &HashMap<String, PortInfo>,
    ctx: &CodegenContext,
    level: usize,
) -> String {
    let pad = ctx.indent.level(level);
    match prop {
        Property::Expression(e) => {
            let expr_str = expr_with_ctx_ports(e, param, field, local_vars, port_map, ctx);
            format!("{}{};\n", pad, expr_str)
        }
        Property::Function(stmt) => {
            stmt_with_ctx(stmt, param, field, local_vars, port_map, ctx, level)
        }
    }
}

/// Convert a Statement to C code with struct context.
fn stmt_with_ctx(
    stmt: &but_grammar::ast::Statement,
    param: &str,
    field: &str,
    local_vars: &[String],
    port_map: &HashMap<String, PortInfo>,
    ctx: &CodegenContext,
    level: usize,
) -> String {
    use but_grammar::ast::Statement;
    let pad = ctx.indent.level(level);
    match stmt {
        Statement::Block { statements, .. } => {
            let inner: String = statements
                .iter()
                .map(|s| stmt_with_ctx(s, param, field, local_vars, port_map, ctx, level + 1))
                .collect();
            format!("{}{{\n{}{}}}\n", pad, inner, pad)
        }
        Statement::Expression(_, e) => {
            let expr_str = expr_with_ctx_ports(e, param, field, local_vars, port_map, ctx);
            format!("{}{};\n", pad, expr_str)
        }
        Statement::If(_, cond, then_s, else_s) => {
            let cond_str = expr_with_ctx_ports(cond, param, field, local_vars, port_map, ctx);
            let mut out = format!("{}if ({}) ", pad, cond_str);
            out.push_str(&stmt_with_ctx(then_s, param, field, local_vars, port_map, ctx, level));
            if let Some(else_body) = else_s {
                out.push_str(&format!("{}else ", pad));
                out.push_str(&stmt_with_ctx(
                    else_body, param, field, local_vars, port_map, ctx, level,
                ));
            }
            out
        }
        // Fall back to the standard stmt_to_c for other statement types
        _ => crate::condition::stmt_to_c(stmt, &ctx.indent, level),
    }
}

/// Hack: convert an Expression used as a condition into a Condition node.
fn cond_expr_to_cond(expr: &Expression) -> Condition {
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

/// Collect all model names that appear as sub-models in any composition,
/// AND as delegating state implementations.
fn collect_sub_model_names(source: &SourceUnit) -> HashSet<String> {
    let mut set = HashSet::new();
    for part in &source.0 {
        if let SourceUnitPart::ModelDefinition(md) = part {
            if let Some(tree) = model_composition_tree(md) {
                for name in tree.leaves() {
                    set.insert(name);
                }
            }
            // Also collect models used as delegating state implementations
            for mp in &md.parts {
                if let ModelPart::StateDefinition(sd) = mp {
                    if let Some(Expression::Variable(id)) = &sd.implements {
                        set.insert(id.name.clone());
                    }
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

/// Find the start state name in a model.
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

/// Collect delegating states: states with `implements = Some(Variable(name))`.
/// Returns vec of (state_name, impl_model_name).
fn collect_delegating_states(model: &ModelDefinition) -> Vec<(String, String)> {
    let mut result = Vec::new();
    for part in &model.parts {
        if let ModelPart::StateDefinition(sd) = part {
            if let Some(Expression::Variable(id)) = &sd.implements {
                if let Some(sname) = &sd.name {
                    result.push((sname.name.clone(), id.name.clone()));
                }
            }
        }
    }
    result
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
