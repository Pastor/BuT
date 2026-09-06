//! Генерация деклараций: константы, порты, перечисления, функции.
//!
//! Содержит [`generate_constants_and_ports_and_enums`] для генерации `#define`-макросов
//! и [`generate_functions`] для генерации extern/static функций.

use super::c_expr::{generate_code_block, get_function_name};
use super::{c_type_or_diagnostic, typed_variable_or_diagnostic};
use crate::diagnostics::Diagnostic;
use crate::generator::c::c_map::CMap;
use crate::generator::indent::Printer;
use crate::semantic::minimap::Element;
use crate::semantic::naming::normalize_lowercase_snakecase;
use crate::semantic::type_node::TypeNode;
use crate::semantic::{ExpressionNode, FunctionDefinitionNode, VariableNode};

fn const_expr_string(expr: &ExpressionNode, name: &str) -> Result<String, Diagnostic> {
    Ok(if let ExpressionNode::Number(value) = expr {
        super::c_literal::c_int_literal(*value)
    } else if let ExpressionNode::Bool(value) = expr {
        if *value {
            "true".to_string()
        } else {
            "false".to_string()
        }
    } else if let ExpressionNode::String(value) = expr {
        format!("\"{}\"", value.join(""))
    } else if let ExpressionNode::Rational(value, _) = expr {
        value.clone()
    } else if let ExpressionNode::Duration(nanos) = expr {
        // Длительность — целое в МИЛЛИСЕКУНДАХ (0183), как и её тип
        // (`uint32_t`). ⚠️ Без этой ветви `const HOLD: duration := 2s;`
        // отвергался воронкой недостижимости (`CC-023` «невычисленное значение
        // константы») — при том, что шесть прочих потребителей константу
        // переводят, а эталон исполняет (замер 0489). Пересчёт делает общий
        // носитель, а не своя формула.
        crate::semantic::duration::value_millis(
            *nanos,
            crate::diagnostics::Location::Codegen,
            &format!("значение константы '{name}'"),
        )?
        .to_string()
    } else if let ExpressionNode::Initializer(value) = expr {
        let mut parts = Vec::new();
        for v in value.iter() {
            parts.push(const_expr_string(v, name)?);
        }
        format!("{{{}}}", parts.join(", "))
    } else {
        // ⚠️ Без `Debug`-дампа выражения (фича 0231): сообщение читает автор
        // программы. Ветвь защитная — до неё доходит только значение, не
        // свёрнутое семантикой, а такое отвергается раньше (`SE-003`).
        return Err(crate::generator::c::c_unresolved::refuse(
            crate::diagnostics::Location::Codegen,
            crate::generator::c::c_unresolved::UnresolvedNode::ConstantValue(name.to_string()),
        ));
    })
}

/// Генерирует `#define`-макросы для констант, портов и перечислений всех моделей.
pub(super) fn generate_constants_and_ports_and_enums(
    printer: &mut Printer,
    map: &CMap,
) -> Result<(), Diagnostic> {
    let mut models = map.using_models();
    models.insert(
        0,
        Element::Model {
            name: map.root_name().clone(),
            states: map.states().clone(),
            start: map.start().clone(),
        },
    );
    for model in models {
        let model_name = model.name();
        let model = map.raw_model_at(model.name())?;
        let model = &*model.borrow();
        let variables = model.variables.clone().into_values();
        let mut lines = Vec::new();
        for var in variables {
            match var {
                VariableNode::Unresolved | VariableNode::Simple { .. } => {}
                VariableNode::Port { .. } => {
                    // Порты генерируются как enum в заголовочном файле (ModelNamePorts).
                }
                VariableNode::Const {
                    ref upper,
                    name,
                    ref expr,
                    ref ty,
                    ..
                } => {
                    // Пропускаем неиспользуемые константы. Ключ — пара
                    // (владелец, имя) (фича 0193): цель `c` квалифицирует имена
                    // с самого начала, поэтому голый ключ давал ей лишний
                    // `#define` неиспользуемой тёзки — безвредный, но неверный.
                    if !map
                        .usage()
                        .constants
                        .contains(&crate::semantic::unused::const_key(upper.as_ref(), &name))
                    {
                        continue;
                    }
                    let name = model_name.unique_uppercase_snakecase()
                        + "_"
                        + normalize_lowercase_snakecase(name.clone())
                            .to_uppercase()
                            .as_str();
                    let value = const_expr_string(expr, &name)?;
                    // 0080-02: структурная константа — `static const`, а НЕ
                    // `#define`. Макрос `#define X {…}` при доступе `X.field`
                    // разворачивается в `{…}.field` — невалидный C (фигурный
                    // литерал вне объявления). `static const Type X = {…};`
                    // делает `X.field` корректным. Скаляр/массив — прежним
                    // `#define` (массив с полевым доступом — территория 0078).
                    // Массив — то же самое и по той же причине (фича 0490):
                    // `#define X {1, 2}` при `a[0] = X[0];` разворачивается в
                    // `{1, 2}[0]`, и `cc` отвечает «expected expression» при
                    // НУЛЕВОМ коде возврата `taktc`. Замер 2026-09-02: тот же
                    // вход отвергают `st`/`st-at` и не синтезирует `yosys`.
                    //
                    // ⚠️ Бит-вектор `[bit;N≤64]` — упакованный СКАЛЯР (0078):
                    // ему `#define` верен, и трогать его нельзя.
                    let array_const = matches!(ty, TypeNode::Array(..))
                        && crate::semantic::bit_vector::is_bit_vector(ty).is_none();
                    if let TypeNode::Struct(struct_name) = ty {
                        lines.push(format!(
                            "static const {} CONST_{} = {};",
                            struct_name, name, value
                        ));
                    } else if array_const {
                        let (len, elem) = match ty {
                            TypeNode::Array(len, elem) => (*len, elem.as_ref()),
                            _ => unreachable!("проверено `array_const`"),
                        };
                        let elem_ty = c_type_or_diagnostic(
                            elem,
                            model,
                            map.float_width(),
                            &format!("константа '{name}'"),
                        )?;
                        lines.push(format!(
                            "static const {elem_ty} CONST_{name}[{len}] = {value};"
                        ));
                    } else {
                        lines.push(format!("#define CONST_{} {}", name, value));
                    }
                }
            }
        }
        if !lines.is_empty() {
            lines.sort();
            printer.print(lines.join("\n").as_str()).nl();
        }

        // Имя строит `c_names::enum_constant` — ТА ЖЕ функция, которой
        // печатается значение (`c_enum::constant_of`, фича 0167). Прежде здесь
        // жила своя формула без сегмента перечисления, и два перечисления одной
        // модели с одноимённым вариантом давали дубль `#define` с разными
        // значениями — `cc -Werror` такой файл отвергает.
        let enums = model.enums.clone().into_values();
        let mut lines = Vec::new();
        for en in enums {
            for (name, value) in &en.variants {
                lines.push(format!(
                    "#define {} {}",
                    crate::generator::c::c_names::enum_constant(&model_name, &en.name, name),
                    value
                ));
            }
        }
        if !lines.is_empty() {
            lines.sort();
            printer.print(lines.join("\n").as_str()).nl();
        }
    }
    Ok(())
}

/// Генерирует extern-декларации и static-определения функций всех моделей.
pub(super) fn generate_functions(printer: &mut Printer, map: &CMap) -> Result<(), Diagnostic> {
    let mut models = map.using_models();
    models.insert(
        0,
        Element::Model {
            name: map.root_name().clone(),
            states: map.states().clone(),
            start: map.start().clone(),
        },
    );
    for model in models {
        let element = model.clone();
        let model = map.raw_model_at(model.name())?;
        let model = &*model.borrow();
        let mut external_funcs = Vec::new();
        let mut local_funcs = Vec::new();
        // 0031: форвард-прототипы локальных функций. Композиция `f → g` (фича
        // 0031) делает порядок определений значимым: без прототипа `Model_f`,
        // напечатанная раньше `Model_g`, вызвала бы необъявленную функцию
        // (`cc -std=c99`: implicit-function-declaration). Прототипы печатаются
        // ДО определений, поэтому порядок определений (алфавитный) уже не важен.
        let mut local_protos = Vec::new();
        for ref fun in model.functions.clone().into_values() {
            // Пропускаем функции, которые нигде не вызываются
            if !fun.name().is_empty() && !map.usage().functions.contains(fun.name()) {
                continue;
            }
            match fun {
                FunctionDefinitionNode::Local {
                    params, body, ret, ..
                } => {
                    let mut definition = String::new();
                    // 0029-01: было `.unwrap()` — невыразимый тип параметра ронял
                    // `taktc` паникой (проба: `fn pick(data: [u8;4])`). Параметр
                    // печатается формой объявления: тип массива в C неотделим от
                    // имени (`uint8_t data[4]`).
                    let mut tiny_params = params
                        .iter()
                        .map(|(name, typ)| {
                            typed_variable_or_diagnostic(
                                typ,
                                name,
                                model,
                                map.float_width(),
                                &format!("параметр '{}' функции '{}'", name, fun.name()),
                            )
                        })
                        .collect::<Result<Vec<String>, Diagnostic>>()?;
                    // Указатель на состояние печатается ПО НУЖДЕ (фича 0396):
                    // прежде он стоял в сигнатуре всегда, а тело пользовалось
                    // им не везде — 53 заглушки `(void)model;` в корпусе.
                    // Признак берётся у общего носителя (`c_needs`), который
                    // спрашивает тот же `rust_needs`, что и цель `rust`.
                    let wants_state = crate::generator::c::c_needs::needs_state(&fun, model)?;
                    if wants_state {
                        tiny_params.insert(
                            0,
                            format!("const {} *model", map.root_name().unique_camelcase()),
                        );
                    }
                    let ret_type = c_type_or_diagnostic(
                        ret,
                        model,
                        map.float_width(),
                        &format!("возвращаемое значение функции '{}'", fun.name()),
                    )?;
                    // ⚠️ Пустой список параметров печатается `void`, а не
                    // пустотой: `f()` в C означает «список НЕИЗВЕСТЕН» (K&R), и
                    // `cc -Wstrict-prototypes` отвечает «a function
                    // declaration without a prototype is deprecated». До фичи
                    // 0396 случай не возникал — указатель стоял всегда.
                    let param_list = if tiny_params.is_empty() {
                        "void".to_string()
                    } else {
                        tiny_params.join(", ")
                    };
                    local_protos.push(format!(
                        "static {} {}({});",
                        ret_type.as_str(),
                        get_function_name(&fun),
                        param_list
                    ));
                    // Комментарий автора перед объявлением функции (фича
                    // 0535, задача 04): в корпусе таких 23, и объясняют они
                    // именно то, что делает функция.
                    for line in crate::generator::comments::leading(
                        fun.loc(),
                        crate::generator::header::CommentStyle::Slashes,
                    ) {
                        definition.push_str(&line);
                        definition.push('\n');
                    }
                    definition.push_str(
                        format!(
                            "static {} {}({}) {{\n",
                            ret_type.as_str(),
                            get_function_name(&fun),
                            param_list
                        )
                        .as_str(),
                    );
                    let mut code_block = String::new();
                    {
                        let mut tmp_printer = Printer::new(4, &mut code_block);
                        tmp_printer.up();
                        generate_code_block(
                            &mut tmp_printer,
                            map,
                            &element,
                            params.clone(),
                            body,
                            false,
                        )?;
                        tmp_printer.down();
                    }
                    // Указатель на состояние требует протокол вызова, но тело
                    // не всегда им пользуется: без заглушки `cc -Wall -Wextra`
                    // отвечает `-Wunused-parameter` (фича 0260).
                    // Тот же вопрос задаётся и ОБЪЯВЛЕННЫМ параметрам (фича
                    // 0337): `fn constant(v: u8) -> u8 { return 7; }` давало
                    // `-Wunused-parameter`, то есть отказ гейта под `-Werror`,
                    // при нулевом коде возврата `taktc`.
                    // ⚠️ Заглушка `model` осталась защитой в глубину: признак
                    // считает по семантике, а `is_unused` — по напечатанному
                    // тексту, и расхождение двух взглядов даст отказ `cc`, а не
                    // молчание. Когда параметра нет, вопрос не задаётся.
                    let mut guards: Vec<String> = if wants_state {
                        vec!["model".to_string()]
                    } else {
                        Vec::new()
                    };
                    guards.extend(params.iter().map(|(name, _)| name.clone()));
                    for guard in guards {
                        if crate::generator::c::c_params::is_unused(&code_block, &guard) {
                            definition.push_str("    ");
                            definition
                                .push_str(&crate::generator::c::c_params::unused_guard(&guard));
                            definition.push('\n');
                        }
                    }
                    definition.push_str(&code_block);
                    definition.push_str("}\n");
                    local_funcs.push(definition);
                }
                FunctionDefinitionNode::External { params, ret, .. } => {
                    let params = params
                        .iter()
                        .map(|(name, typ)| {
                            typed_variable_or_diagnostic(
                                typ,
                                name,
                                model,
                                map.float_width(),
                                &format!("параметр '{}' функции '{}'", name, fun.name()),
                            )
                        })
                        .collect::<Result<Vec<String>, Diagnostic>>()?;
                    let ret_type = c_type_or_diagnostic(
                        ret,
                        model,
                        map.float_width(),
                        &format!("возвращаемое значение функции '{}'", fun.name()),
                    )?;
                    external_funcs.push(format!(
                        "extern {} {}({});",
                        ret_type.as_str(),
                        get_function_name(&fun),
                        params.join(", ").as_str()
                    ));
                }
                // ⚠️ Текст был по-английски и без кода — класс фичи 0212.
                _ => {
                    return Err(crate::generator::c::c_unresolved::refuse(
                        fun.loc(),
                        crate::generator::c::c_unresolved::UnresolvedNode::Function(Some(
                            fun.name().to_string(),
                        )),
                    ));
                }
            }
        }

        if !external_funcs.is_empty() {
            external_funcs.sort();
            for func in external_funcs {
                printer.print(func.as_str()).nl();
            }
        }
        if !local_funcs.is_empty() {
            // Прототипы — до определений (0031): порядок определений не важен.
            local_protos.sort();
            for proto in &local_protos {
                printer.print(proto.as_str()).nl();
            }
            local_funcs.sort();
            for func in local_funcs {
                printer.print(func.as_str()).nl();
            }
        }
    }
    Ok(())
}
