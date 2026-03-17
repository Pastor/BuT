use crate::CodegenContext;
use crate::behavior::{BehaviorKind, find_behavior, find_end_property, find_terminal_states};
use crate::condition::{condition_to_c, expr_to_c, stmt_to_c, type_to_c_ctx};
use crate::ltl::{extract_ltl_formulas, ltl_comments_c};
use but_grammar::ast::{
    Annotation, Condition, ModelDefinition, ModelPart, Property, SourceUnit, SourceUnitPart,
    StatePart, VariableAttribute,
};

// ── Публичный API ────────────────────────────────────────────────────────────────

/// Сгенерировать объединённый C-заголовочный файл для всех моделей из SourceUnit.
pub fn generate_c_header_all(source: &SourceUnit, base_name: &str, ctx: &CodegenContext) -> String {
    let guard = base_name.to_uppercase().replace('-', "_").replace('.', "_");
    let mut out = String::new();
    out.push_str(&format!("#ifndef __{}_H__\n", guard));
    out.push_str(&format!("#define __{}_H__\n\n", guard));
    out.push_str("#include <stdint.h>\n\n");

    // Внешние объявления глобальных портов
    for vd in &ctx.global_vars {
        if vd
            .attrs
            .iter()
            .any(|a| matches!(a, VariableAttribute::Portable(_)))
        {
            if let Some(vname) = &vd.name {
                let ty = type_to_c_ctx(&vd.ty, &ctx.type_aliases);
                out.push_str(&format!("extern {} {};\n", ty, vname.name));
            }
        }
    }
    if !ctx.global_vars.is_empty() {
        out.push('\n');
    }

    for part in &source.0 {
        if let SourceUnitPart::ModelDefinition(md) = part {
            out.push_str(&generate_c_model_header(md, ctx));
            out.push('\n');
        }
    }

    out.push_str(&format!("#endif /* __{}_H__ */\n", guard));
    out
}

/// Сгенерировать объединённый C-исходный файл для всех моделей из SourceUnit.
pub fn generate_c_source_all(source: &SourceUnit, base_name: &str, ctx: &CodegenContext) -> String {
    let mut out = String::new();
    out.push_str(&format!("#include \"{}.h\"\n\n", base_name.to_lowercase()));

    // Определения глобальных портов
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
                out.push_str(&format!("{} {} = {};\n", ty, vname.name, init));
            }
        }
    }
    if !ctx.global_vars.is_empty() {
        out.push('\n');
    }

    for part in &source.0 {
        if let SourceUnitPart::ModelDefinition(md) = part {
            out.push_str(&generate_c_model_source(md, ctx));
            out.push('\n');
        }
    }
    out
}

/// Сгенерировать заголовочный C-файл для одной модели (для прямого использования в тестах).
pub fn generate_c_header(model: &ModelDefinition, ctx: &CodegenContext) -> String {
    let name = model_name(model);
    let upper = name.to_uppercase();
    let mut out = String::new();
    out.push_str(&format!("#ifndef __{}_H__\n", upper));
    out.push_str(&format!("#define __{}_H__\n\n", upper));
    out.push_str("#include <stdint.h>\n\n");
    let ltl_formulas = extract_ltl_formulas(model);
    if !ltl_formulas.is_empty() {
        out.push_str(&ltl_comments_c(&ltl_formulas));
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
                out.push_str(&format!("extern {} {};\n", ty, vname.name));
            }
        }
    }
    out.push('\n');
    out.push_str(&model_declarations(&name, &upper, model));
    out.push_str(&format!("#endif /* __{}_H__ */\n", upper));
    out
}

/// Сгенерировать исходный C-файл для одной модели (для прямого использования в тестах).
pub fn generate_c_source(model: &ModelDefinition, ctx: &CodegenContext) -> String {
    generate_c_model_source(model, ctx)
}

/// Сгенерировать C-заголовок и исходник для всех моделей (объединённый вывод).
///
/// Возвращает `(header_content, source_content)` — содержимое одного `.h` и одного `.c` файла.
pub fn generate_c_all(source: &SourceUnit, ctx: &CodegenContext) -> (String, String) {
    generate_c_all_named(source, "model", ctx)
}

/// Сгенерировать объединённый C-вывод с заданным базовым именем файла.
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

// ── Внутренняя генерация по одной модели ─────────────────────────────────────────

/// Блок объявлений (прототипы + enum состояний) для одной модели — без include-guard.
fn generate_c_model_header(model: &ModelDefinition, ctx: &CodegenContext) -> String {
    let name = model_name(model);
    let upper = name.to_uppercase();
    let mut out = String::new();

    let ltl_formulas = extract_ltl_formulas(model);
    if !ltl_formulas.is_empty() {
        out.push_str(&ltl_comments_c(&ltl_formulas));
        out.push('\n');
    }

    let behavior = find_behavior(model);
    if let Some(bk) = &behavior {
        // Для компоновочных моделей — enum фаз вместо состояний
        out.push_str(&generate_c_behavior_enum(
            &name,
            &upper,
            bk,
            ctx.indent.level(1).as_str(),
        ));
    } else {
        // Перечисление состояний FSM
        let i1 = ctx.indent.level(1);
        out.push_str(&format!("typedef enum {{\n"));
        let states = collect_states(model);
        for (i, s) in states.iter().enumerate() {
            let comma = if i + 1 < states.len() { "," } else { "" };
            out.push_str(&format!("{}{}_{}{}\n", i1, upper, s.to_uppercase(), comma));
        }
        out.push_str(&format!("}} {}_State_t;\n\n", name));
    }

    out.push_str(&model_declarations(&name, &upper, model));
    out
}

/// Генерация enum фаз для компоновочной модели.
fn generate_c_behavior_enum(name: &str, upper: &str, bk: &BehaviorKind, i1: &str) -> String {
    let mut out = String::new();
    out.push_str(&format!("typedef enum {{\n"));
    for (i, m) in bk.models().iter().enumerate() {
        out.push_str(&format!("{}{}_PHASE_{},\n", i1, upper, m.to_uppercase()));
        let _ = i;
    }
    out.push_str(&format!("{}{}_DONE,\n", i1, upper));
    out.push_str(&format!("}} {}_Phase_t;\n\n", name));
    out
}

/// Объявления функций для модели (общие для FSM и компоновки).
fn model_declarations(name: &str, upper: &str, model: &ModelDefinition) -> String {
    let mut out = String::new();
    let terminals = find_terminal_states(model);
    let has_behavior = find_behavior(model).is_some();

    if has_behavior {
        out.push_str(&format!("void {}_init(void);\n", name));
        out.push_str(&format!("void {}_step(void);\n", name));
        out.push_str(&format!("int  {}_is_done(void);\n\n", name));
    } else {
        out.push_str(&format!("void {}_init(void);\n", name));
        out.push_str(&format!("void {}_step(void);\n", name));
        out.push_str(&format!("{}_State_t {}_state(void);\n", name, name));
        if !terminals.is_empty() {
            out.push_str(&format!("int  {}_is_done(void);\n", name));
        }
        out.push('\n');
    }
    let _ = upper;
    out
}

/// Сгенерировать C-реализацию для одной модели.
fn generate_c_model_source(model: &ModelDefinition, ctx: &CodegenContext) -> String {
    if let Some(bk) = find_behavior(model) {
        generate_c_behavior_source(model, &bk, ctx)
    } else {
        generate_c_fsm_source(model, ctx)
    }
}

/// Исходник для обычного FSM-автомата.
fn generate_c_fsm_source(model: &ModelDefinition, ctx: &CodegenContext) -> String {
    let name = model_name(model);
    let upper = name.to_uppercase();
    let i1 = ctx.indent.level(1);
    let i2 = ctx.indent.level(2);
    let i3 = ctx.indent.level(3);
    let i4 = ctx.indent.level(4);

    let mut out = String::new();

    let ltl_formulas = extract_ltl_formulas(model);
    if !ltl_formulas.is_empty() {
        out.push_str(&ltl_comments_c(&ltl_formulas));
        out.push('\n');
    }

    out.push_str(&format!("static {}_State_t _{}_state;\n", name, name));

    // Переменные модели
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
                let init = vd
                    .initializer
                    .as_ref()
                    .map(expr_to_c)
                    .unwrap_or_else(|| "0".to_string());
                out.push_str(&format!("static {} {} = {};\n", ty, vname.name, init));
            }
        }
    }
    out.push('\n');

    let start = find_start(model).unwrap_or_default();
    let terminal_states = find_terminal_states(model);
    let end_prop = find_end_property(model);

    // Функция инициализации
    out.push_str(&format!("void {}_init(void) {{\n", name));
    out.push_str(&format!(
        "{}_{}_state = {}_{};\n",
        i1,
        name,
        upper,
        start.to_uppercase()
    ));
    for part in &model.parts {
        if let ModelPart::PropertyDefinition(pd) = part {
            if pd.name.as_ref().map(|n| n.name.as_str()) == Some("enter") {
                let stmt = property_to_c(&pd.value, ctx, 1);
                out.push_str(&stmt);
            }
        }
    }
    out.push_str("}\n\n");

    // Запрос текущего состояния
    out.push_str(&format!("{}_State_t {}_state(void) {{\n", name, name));
    out.push_str(&format!("{}return _{}_state;\n", i1, name));
    out.push_str("}\n\n");

    // is_done() — если есть терминальные состояния
    if !terminal_states.is_empty() {
        out.push_str(&format!("int {}_is_done(void) {{\n", name));
        let conditions: Vec<String> = terminal_states
            .iter()
            .map(|s| format!("_{}_state == {}_{}", name, upper, s.to_uppercase()))
            .collect();
        out.push_str(&format!("{}return {};\n", i1, conditions.join(" || ")));
        out.push_str("}\n\n");
    }

    // Функция шага
    out.push_str(&format!("void {}_step(void) {{\n", name));

    // Enter уровня модели (каждый такт)
    for part in &model.parts {
        if let ModelPart::PropertyDefinition(pd) = part {
            if pd.name.as_ref().map(|n| n.name.as_str()) == Some("enter") {
                out.push_str(&property_to_c(&pd.value, ctx, 1));
            }
        }
    }

    out.push_str(&format!("{}switch (_{}_state) {{\n", i1, name));

    for part in &model.parts {
        if let ModelPart::StateDefinition(sd) = part {
            let sname = sd.name.as_ref().map(|n| n.name.as_str()).unwrap_or("?");
            let is_terminal = terminal_states.iter().any(|t| t == sname);

            out.push_str(&format!(
                "{}case {}_{}: {{\n",
                i2,
                upper,
                sname.to_uppercase()
            ));

            // Обработчик enter состояния
            for sp in &sd.parts {
                if let StatePart::PropertyDefinition(pd) = sp {
                    if pd.name.as_ref().map(|n| n.name.as_str()) == Some("enter") {
                        out.push_str(&property_to_c(&pd.value, ctx, 3));
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

                    out.push_str(&format!("{}if ({}) {{\n", i3, cond_str));

                    // Обработчики exit
                    for sp2 in &sd.parts {
                        if let StatePart::PropertyDefinition(pd2) = sp2 {
                            if pd2.name.as_ref().map(|n| n.name.as_str()) == Some("exit") {
                                out.push_str(&property_to_c(&pd2.value, ctx, 4));
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
                                    if pd3.name.as_ref().map(|n| n.name.as_str()) == Some("before")
                                    {
                                        out.push_str(&property_to_c(&pd3.value, ctx, 4));
                                    }
                                }
                            }
                        }
                    }

                    out.push_str(&format!(
                        "{}_{}_state = {}_{};\n",
                        i4,
                        name,
                        upper,
                        target.name.to_uppercase()
                    ));
                    out.push_str(&format!("{}}}\n", i3));
                }
            }

            // Терминальное состояние: обработчик end
            if is_terminal {
                if let Some(ep) = end_prop {
                    out.push_str(&format!("{}/* end handler */\n", i3));
                    out.push_str(&property_to_c(ep, ctx, 3));
                }
            }

            out.push_str(&format!("{}break;\n", i3));
            out.push_str(&format!("{}}}\n", i2));
        }
    }

    out.push_str(&format!("{}}}\n", i1));
    out.push_str("}\n");
    out
}

/// Исходник для компоновочной модели (behavior).
fn generate_c_behavior_source(
    model: &ModelDefinition,
    bk: &BehaviorKind,
    ctx: &CodegenContext,
) -> String {
    let name = model_name(model);
    let upper = name.to_uppercase();
    let i1 = ctx.indent.level(1);
    let i2 = ctx.indent.level(2);
    let end_prop = find_end_property(model);

    let models = bk.models();
    let mut out = String::new();

    match bk {
        BehaviorKind::Sequential(_) => {
            out.push_str(&format!(
                "/* Последовательная компоновка: {} */\n",
                models.join(" → ")
            ));
            out.push_str(&format!("static {}_Phase_t _{}_phase;\n\n", name, name));

            // init
            out.push_str(&format!("void {}_init(void) {{\n", name));
            out.push_str(&format!(
                "{}_{}_phase = {}_PHASE_{};\n",
                i1,
                name,
                upper,
                models.first().map(|s| s.to_uppercase()).unwrap_or_default()
            ));
            if let Some(m0) = models.first() {
                out.push_str(&format!("{}{}_init();\n", i1, m0.to_lowercase()));
            }
            out.push_str("}\n\n");

            // step
            out.push_str(&format!("void {}_step(void) {{\n", name));
            out.push_str(&format!("{}switch (_{}_phase) {{\n", i1, name));
            for (idx, m) in models.iter().enumerate() {
                let mlo = m.to_lowercase();
                let mup = m.to_uppercase();
                out.push_str(&format!("{}case {}_PHASE_{}: {{\n", i2, upper, mup));
                out.push_str(&format!("{}{}_step();\n", ctx.indent.level(3), mlo));
                out.push_str(&format!(
                    "{}if ({}_is_done()) {{\n",
                    ctx.indent.level(3),
                    mlo
                ));
                if idx + 1 < models.len() {
                    let next_mup = models[idx + 1].to_uppercase();
                    let next_mlo = models[idx + 1].to_lowercase();
                    out.push_str(&format!(
                        "{}_{}_phase = {}_PHASE_{};\n",
                        ctx.indent.level(4),
                        name,
                        upper,
                        next_mup
                    ));
                    out.push_str(&format!("{}{}_init();\n", ctx.indent.level(4), next_mlo));
                } else {
                    out.push_str(&format!(
                        "{}_{}_phase = {}_DONE;\n",
                        ctx.indent.level(4),
                        name,
                        upper
                    ));
                    if let Some(ep) = end_prop {
                        out.push_str(&format!("{}/* end handler */\n", ctx.indent.level(4)));
                        out.push_str(&property_to_c(ep, ctx, 4));
                    }
                }
                out.push_str(&format!("{}}}\n", ctx.indent.level(3)));
                out.push_str(&format!("{}break;\n", ctx.indent.level(3)));
                out.push_str(&format!("{}}}\n", i2));
            }
            out.push_str(&format!("{}case {}_DONE:\n", i2, upper));
            out.push_str(&format!("{}break;\n", ctx.indent.level(3)));
            out.push_str(&format!("{}}}\n", i1));
            out.push_str("}\n\n");

            // is_done
            out.push_str(&format!("int {}_is_done(void) {{\n", name));
            out.push_str(&format!(
                "{}return _{}_phase == {}_DONE;\n",
                i1, name, upper
            ));
            out.push_str("}\n");
        }

        BehaviorKind::Parallel(_) => {
            out.push_str(&format!(
                "/* Параллельная компоновка: {} */\n",
                models.join(" ∥ ")
            ));
            out.push_str(&format!("static int _{}_done;\n\n", name));

            // init
            out.push_str(&format!("void {}_init(void) {{\n", name));
            out.push_str(&format!("{}_{}_done = 0;\n", i1, name));
            for m in models {
                out.push_str(&format!("{}{}_init();\n", i1, m.to_lowercase()));
            }
            out.push_str("}\n\n");

            // step
            out.push_str(&format!("void {}_step(void) {{\n", name));
            for m in models {
                let mlo = m.to_lowercase();
                out.push_str(&format!("{}if (!{}_is_done()) {}_step();\n", i1, mlo, mlo));
            }
            let all_done: Vec<String> = models
                .iter()
                .map(|m| format!("{}_is_done()", m.to_lowercase()))
                .collect();
            out.push_str(&format!(
                "{}if (!_{}_done && {}) {{\n",
                i1,
                name,
                all_done.join(" && ")
            ));
            out.push_str(&format!("{}_{}_done = 1;\n", i2, name));
            if let Some(ep) = end_prop {
                out.push_str(&format!("{}/* end handler */\n", i2));
                out.push_str(&property_to_c(ep, ctx, 2));
            }
            out.push_str(&format!("{}}}\n", i1));
            out.push_str("}\n\n");

            // is_done
            out.push_str(&format!("int {}_is_done(void) {{\n", name));
            out.push_str(&format!("{}return _{}_done;\n", i1, name));
            out.push_str("}\n");
        }

        BehaviorKind::Choice(_) => {
            out.push_str(&format!(
                "/* Компоновка выбора: {} */\n",
                models.join(" | ")
            ));
            out.push_str(&format!("static int _{}_active_idx;\n", name));
            out.push_str(&format!("static int _{}_done;\n\n", name));

            // init — запускаем первую готовую (здесь просто первую)
            out.push_str(&format!("void {}_init(void) {{\n", name));
            out.push_str(&format!("{}_{}_active_idx = 0;\n", i1, name));
            out.push_str(&format!("{}_{}_done = 0;\n", i1, name));
            if let Some(m0) = models.first() {
                out.push_str(&format!("{}{}_init();\n", i1, m0.to_lowercase()));
            }
            out.push_str("}\n\n");

            // step
            out.push_str(&format!("void {}_step(void) {{\n", name));
            out.push_str(&format!("{}switch (_{}_active_idx) {{\n", i1, name));
            for (idx, m) in models.iter().enumerate() {
                let mlo = m.to_lowercase();
                out.push_str(&format!(
                    "{}case {}: {}_step(); if ({}_is_done()) {{ _{}_done = 1;",
                    i2, idx, mlo, mlo, name
                ));
                if let Some(ep) = end_prop {
                    let ep_str = match ep {
                        Property::Expression(e) => format!(" {}; ", expr_to_c(e)),
                        Property::Function(_) => " /* end block */ ".to_string(),
                    };
                    out.push_str(&ep_str);
                }
                out.push_str(&format!("}} break;\n"));
            }
            out.push_str(&format!("{}}}\n", i1));
            out.push_str("}\n\n");

            // is_done
            out.push_str(&format!("int {}_is_done(void) {{\n", name));
            out.push_str(&format!("{}return _{}_done;\n", i1, name));
            out.push_str("}\n");
        }
    }

    out
}

// ── Вспомогательные функции ───────────────────────────────────────────────────────

fn property_to_c(prop: &Property, ctx: &CodegenContext, level: usize) -> String {
    match prop {
        Property::Function(s) => stmt_to_c(s, &ctx.indent, level),
        Property::Expression(e) => {
            format!("{}{};\n", ctx.indent.level(level), expr_to_c(e))
        }
    }
}

pub(crate) fn model_name(model: &ModelDefinition) -> String {
    let named: Option<String> = model
        .annotations
        .iter()
        .filter(|a| !a.glob)
        .flat_map(|a| a.args.iter().collect::<Vec<_>>())
        .filter(|a| {
            if let Annotation::String(s) = a {
                true
            } else {
                false
            }
        })
        .map(|a| {
            if let Annotation::Function { name, args, .. } = a
                && name.name == "name"
                && !args.is_empty()
            {
                let name = args
                    .iter()
                    .filter(|a| {
                        if let Annotation::String(s) = a {
                            true
                        } else {
                            false
                        }
                    })
                    .map(|a| {
                        if let Annotation::String(s) = a {
                            s.string.clone()
                        } else {
                            unreachable!()
                        }
                    })
                    .collect::<Vec<String>>()
                    .first()
                    .map(|s| s.clone());
                return name.unwrap_or_else(|| "Model".to_string());
            } else {
                panic!("Annotation is not a string")
            }
        })
        .collect::<Vec<String>>()
        .first()
        .map(|s| s.clone());
    if let Some(named) = named {
        return named.clone();
    }
    model
        .name
        .as_ref()
        .map(|n| n.name.clone())
        .unwrap_or_else(|| "Model".to_string())
}

pub(crate) fn collect_states(model: &ModelDefinition) -> Vec<String> {
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

pub(crate) fn find_start(model: &ModelDefinition) -> Option<String> {
    for part in &model.parts {
        match part {
            ModelPart::PropertyDefinition(pd) => {
                if pd.name.as_ref().map(|n| n.name.as_str()) == Some("start") {
                    if let Property::Expression(but_grammar::ast::Expression::Variable(id)) =
                        &pd.value
                    {
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
