use but_grammar::ast::{
    Annotation, Condition, Expression, FormulaStatement,
    ModelDefinition, ModelPart, Property, SourceUnit, SourceUnitPart, StatePart,
};

/// Сгенерировать DOT-граф для одной модели.
pub fn model_to_dot(model: &ModelDefinition, current_state: Option<&str>) -> String {
    let name = model
        .name
        .as_ref()
        .map(|n| n.name.as_str())
        .unwrap_or("FSM");

    let mut out = String::new();
    out.push_str(&format!("digraph {} {{\n", sanitize(name)));
    out.push_str("  rankdir=LR;\n");
    out.push_str("  node [shape=circle, fontname=\"DejaVu Sans\", fontsize=11];\n");
    out.push_str("  edge [fontname=\"DejaVu Sans\", fontsize=9];\n");

    // Стартовый узел
    out.push_str("  __start__ [shape=point, width=0.2];\n");

    // Найти начальное состояние
    let start_state = find_start_state(model);

    // Стартовый переход
    if let Some(ref s) = start_state {
        out.push_str(&format!("  __start__ -> {};\n", sanitize(s)));
    }

    // Вывести узлы состояний
    let mut state_names: Vec<String> = vec![];
    for part in &model.parts {
        if let ModelPart::StateDefinition(sd) = part {
            let sname = sd
                .name
                .as_ref()
                .map(|n| n.name.as_str())
                .unwrap_or("?");
            state_names.push(sname.to_string());

            let is_current = current_state.map(|c| c == sname).unwrap_or(false);
            let is_start = start_state.as_deref() == Some(sname);

            let shape = if sd.parts.iter().any(|p| {
                matches!(p, StatePart::Reference(..))
            }) || is_start {
                "circle"
            } else {
                "doublecircle" // терминальные состояния
            };

            let attrs = if is_current {
                format!(
                    "shape={}, label=\"{}\", color=red, penwidth=2, style=bold",
                    shape, sname
                )
            } else {
                format!("shape={}, label=\"{}\"", shape, sname)
            };
            out.push_str(&format!("  {} [{}];\n", sanitize(sname), attrs));
        }
    }

    // Вывести переходы (рёбра)
    for part in &model.parts {
        if let ModelPart::StateDefinition(sd) = part {
            let from = sd
                .name
                .as_ref()
                .map(|n| n.name.as_str())
                .unwrap_or("?");

            for sp in &sd.parts {
                if let StatePart::Reference(_, target, cond) = sp {
                    let label = cond
                        .as_ref()
                        .map(|c| condition_to_label(c))
                        .unwrap_or_default();
                    out.push_str(&format!(
                        "  {} -> {} [label=\"{}\"];\n",
                        sanitize(from),
                        sanitize(&target.name),
                        escape_dot(&label)
                    ));
                }
            }
        }
    }

    // LTL-спецификации: добавить в граф в виде узла-заметки
    let ltl_formulas = extract_ltl_formulas_dot(model);
    if !ltl_formulas.is_empty() {
        let ltl_label = ltl_formulas.join("\n");
        out.push_str(&format!(
            "  __ltl__ [shape=note, style=filled, fillcolor=lightyellow, fontsize=8, label=\"LTL:\n{}\"]; \n",
            escape_dot(&ltl_label)
        ));
    }

    out.push_str("}\n");
    out
}

/// Сгенерировать DOT для всех моделей из SourceUnit.
pub fn source_to_dots(source: &SourceUnit) -> Vec<(String, String)> {
    let mut result = vec![];
    for part in &source.0 {
        if let SourceUnitPart::ModelDefinition(md) = part {
            let name = md
                .name
                .as_ref()
                .map(|n| n.name.clone())
                .unwrap_or_else(|| "unnamed".to_string());
            let dot = model_to_dot(md, None);
            result.push((name, dot));
        }
    }
    result
}

/// Преобразовать условие Condition в читаемую строку для метки.
pub fn condition_to_label(cond: &Condition) -> String {
    match cond {
        Condition::Variable(id) => id.name.clone(),
        Condition::BoolLiteral(_, b) => b.to_string(),
        Condition::NumberLiteral(_, n) => n.to_string(),
        Condition::RationalNumberLiteral(_, s, neg) => {
            if *neg {
                format!("-{}", s)
            } else {
                s.clone()
            }
        }
        Condition::HexNumberLiteral(_, s, _) => s.clone(),
        Condition::Equal(_, l, r) => {
            format!("{} = {}", condition_to_label(l), condition_to_label(r))
        }
        Condition::NotEqual(_, l, r) => {
            format!("{} != {}", condition_to_label(l), condition_to_label(r))
        }
        Condition::Less(_, l, r) => {
            format!("{} < {}", condition_to_label(l), condition_to_label(r))
        }
        Condition::LessEqual(_, l, r) => {
            format!("{} <= {}", condition_to_label(l), condition_to_label(r))
        }
        Condition::More(_, l, r) => {
            format!("{} > {}", condition_to_label(l), condition_to_label(r))
        }
        Condition::MoreEqual(_, l, r) => {
            format!("{} >= {}", condition_to_label(l), condition_to_label(r))
        }
        Condition::And(_, l, r) => {
            format!("{} & {}", condition_to_label(l), condition_to_label(r))
        }
        Condition::Or(_, l, r) => {
            format!("{} | {}", condition_to_label(l), condition_to_label(r))
        }
        Condition::Not(_, inner) => format!("!{}", condition_to_label(inner)),
        Condition::Add(_, l, r) => {
            format!("{} + {}", condition_to_label(l), condition_to_label(r))
        }
        Condition::Subtract(_, l, r) => {
            format!("{} - {}", condition_to_label(l), condition_to_label(r))
        }
        Condition::Parenthesis(_, inner) => {
            format!("({})", condition_to_label(inner))
        }
        Condition::FunctionCall(_, id, args) => {
            let arg_str = args
                .iter()
                .map(condition_to_label)
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}({})", id.name, arg_str)
        }
        Condition::MemberAccess(_, base, _member) => condition_to_label(base),
        Condition::ArraySubscript(_, id, idx) => format!("{}[{}]", id.name, idx),
        Condition::StringLiteral(lits) => {
            lits.iter().map(|l| l.string.as_str()).collect::<String>()
        }
        Condition::HexLiteral(lits) => lits.first().map(|l| l.hex.clone()).unwrap_or_default(),
    }
}

/// Извлечь LTL-формулы из аннотаций и блоков formula модели (для DOT).
fn extract_ltl_formulas_dot(model: &ModelDefinition) -> Vec<String> {
    let mut result = vec![];

    // Из аннотаций (#![ltl = "..."])
    for ann_def in &model.annotations {
        for ann in &ann_def.args {
            if let Annotation::Assign { name, value, .. } = ann {
                let attr_name = name.identifiers.last()
                    .map(|id| id.name.as_str()).unwrap_or("");
                if attr_name.eq_ignore_ascii_case("ltl") {
                    if let Expression::StringLiteral(lits) = value {
                        let formula = lits.iter().map(|l| l.string.as_str()).collect::<String>();
                        result.push(formula);
                    }
                }
            }
        }
    }

    // Из аннотаций внутри тела модели (ModelPart::AnnotationDefinition)
    for part in &model.parts {
        if let ModelPart::AnnotationDefinition(ann_def) = part {
            for ann in &ann_def.args {
                if let Annotation::Assign { name, value, .. } = ann {
                    let attr_name = name.identifiers.last()
                        .map(|id| id.name.as_str()).unwrap_or("");
                    if attr_name.eq_ignore_ascii_case("ltl") {
                        if let Expression::StringLiteral(lits) = value {
                            let formula = lits.iter().map(|l| l.string.as_str()).collect::<String>();
                            result.push(formula);
                        }
                    }
                }
            }
        }
    }

    // Из блоков formula { ... }
    for part in &model.parts {
        if let ModelPart::FormulaDefinition(fd) = part {
            for stmt in &fd.formula.statements {
                if let FormulaStatement::FunctionCall(fc) = stmt {
                    let args_str = fc.arguments.iter()
                        .map(|e| {
                            use but_grammar::ast::FormulaExpression;
                            match e {
                                FormulaExpression::Variable(id) => id.name.clone(),
                                FormulaExpression::BoolLiteral(_, b, _) => b.to_string(),
                                FormulaExpression::NumberLiteral(_, n, _) => n.to_string(),
                                _ => "...".to_string(),
                            }
                        })
                        .collect::<Vec<_>>()
                        .join(", ");
                    result.push(format!("{}({})", fc.id.name, args_str));
                }
            }
        }
    }

    result
}

/// Найти начальное состояние из объявлений property/condition уровня модели.
fn find_start_state(model: &ModelDefinition) -> Option<String> {
    for part in &model.parts {
        match part {
            ModelPart::PropertyDefinition(pd) => {
                if pd.name.as_ref().map(|n| n.name.as_str()) == Some("start") {
                    if let Property::Expression(expr) = &pd.value {
                        if let but_grammar::ast::Expression::Variable(id) = expr {
                            return Some(id.name.clone());
                        }
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

/// Очистить имя для использования как идентификатора узла DOT.
fn sanitize(name: &str) -> String {
    // Заменить пробелы и спецсимволы на подчёркивания
    name.chars()
        .map(|c| if c.is_alphanumeric() || c == '_' { c } else { '_' })
        .collect()
}

/// Экранировать спецсимволы в строках метки DOT.
fn escape_dot(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
}
