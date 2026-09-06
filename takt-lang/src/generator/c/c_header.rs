use crate::diagnostics::Diagnostic;
use crate::generator::c;
use crate::generator::c::c_map::CMap;
use crate::generator::c::{
    FUNCTION_PORT_READ_BIT, FUNCTION_PORT_READ_FLOAT, FUNCTION_PORT_READ_NUMERIC,
    FUNCTION_PORT_WRITE_BIT, FUNCTION_PORT_WRITE_FLOAT, FUNCTION_PORT_WRITE_NUMERIC, PortClass,
    typed_variable_or_diagnostic,
};
use crate::generator::header::{CommentStyle, file_header};
use crate::generator::indent::Printer;
use crate::semantic::minimap::{Element, Name, StateExtend};
use crate::semantic::naming::normalize_lowercase_snakecase;
use crate::semantic::{PortDirection, VariableNode};
use log::warn;

/// Словарь портов: `(класс порта, направление)` -> список `(имя модели, имя порта)`.
use crate::generator::c::c_port_enums::{collect_ports_by_class, generate_port_enums};

/// Генерирует поля структуры C для extend состояния. Единичный Model -> `{state}`,
/// составной -> делегирует в build_concat_item.
fn build_extend_header(
    printer: &mut Printer,
    state_name: &Name,
    extend: &StateExtend,
) -> Result<(), Diagnostic> {
    match extend {
        StateExtend::None => {}
        StateExtend::Model(model, _) => {
            printer
                .ident(&format!(
                    "{} {};",
                    model.unique_camelcase(),
                    state_name.local_lowercase_snakecase(),
                ))
                .nl();
        }
        StateExtend::Concatenation(items) => {
            for (idx, item) in items.iter().enumerate() {
                build_concat_item(printer, state_name, item, idx)?;
            }
            build_concat_state_enum(printer, state_name, items)?;
        }
        StateExtend::Parallel(items) => {
            printer.ident("struct {").nl().up();
            for (idx, item) in items.iter().enumerate() {
                build_parallel_item(printer, item, idx, &state_name.unique_lowercase_snakecase())?;
            }
            printer.ident("enum {").up().nl();
            printer
                .ident(&*(state_name.unique_uppercase_snakecase() + "_INIT,"))
                .nl();
            printer
                .ident(&*(state_name.unique_uppercase_snakecase() + "_TICK,"))
                .nl();
            printer
                .ident(&*(state_name.unique_uppercase_snakecase() + "_END"))
                .nl();
            printer.down().ident("} state;").nl();
            printer
                .down()
                .ident(&format!("}} {};", state_name.local_lowercase_snakecase()))
                .nl();
        }
    }
    Ok(())
}

/// Генерирует поле для элемента конкатенации. Model -> `{state}_{model}{idx}`, Parallel ->
/// struct `{state}_parallel{idx}`.
fn build_concat_item(
    printer: &mut Printer,
    state_name: &Name,
    extend: &StateExtend,
    idx: usize,
) -> Result<(), Diagnostic> {
    match extend {
        StateExtend::None => {}
        StateExtend::Model(model, _) => {
            printer
                .ident(&format!(
                    "{} {}_{}{};",
                    model.unique_camelcase(),
                    state_name.local_lowercase_snakecase(),
                    model.local_lowercase_snakecase(),
                    idx,
                ))
                .nl();
        }
        StateExtend::Parallel(items) => {
            let local_partial_name =
                format!("{}_parallel{}", state_name.local_lowercase_snakecase(), idx);
            let unique_partial_name = format!(
                "{}_parallel{}",
                state_name.unique_lowercase_snakecase(),
                idx
            );
            printer.ident("struct {").nl().up();
            for (inner_idx, item) in items.iter().enumerate() {
                build_parallel_item(printer, item, inner_idx, &unique_partial_name)?;
            }
            printer.ident("enum {").up().nl();
            printer
                .ident(&*(unique_partial_name.to_uppercase() + "_INIT,"))
                .nl();
            printer
                .ident(&*(unique_partial_name.to_uppercase() + "_TICK,"))
                .nl();
            printer
                .ident(&*(unique_partial_name.to_uppercase() + "_END"))
                .nl();
            printer.down().ident("} state;").nl();
            printer
                .down()
                .ident(&format!("}} {};", local_partial_name))
                .nl();
        }
        StateExtend::Concatenation(items) => {
            for (inner_idx, item) in items.iter().enumerate() {
                build_concat_item(printer, state_name, item, inner_idx)?;
            }
        }
    }
    Ok(())
}

/// Генерирует поле для элемента параллельного блока. Model -> `{model}{idx}`, вложенный
/// Parallel -> struct `parallel{idx}` с enum state.
fn build_parallel_item(
    printer: &mut Printer,
    extend: &StateExtend,
    idx: usize,
    unique_prefix: &str,
) -> Result<(), Diagnostic> {
    match extend {
        StateExtend::None => {}
        StateExtend::Model(model, _) => {
            printer
                .ident(&format!(
                    "{} {}{};",
                    model.unique_camelcase(),
                    model.local_lowercase_snakecase(),
                    idx,
                ))
                .nl();
        }
        StateExtend::Parallel(items) => {
            let nested_prefix = format!("{}_parallel{}", unique_prefix, idx);
            printer.ident("struct {").nl().up();
            for (inner_idx, item) in items.iter().enumerate() {
                build_parallel_item(printer, item, inner_idx, &nested_prefix)?;
            }
            printer.ident("enum {").up().nl();
            printer
                .ident(&*(nested_prefix.to_uppercase() + "_INIT,"))
                .nl();
            printer
                .ident(&*(nested_prefix.to_uppercase() + "_TICK,"))
                .nl();
            printer
                .ident(&*(nested_prefix.to_uppercase() + "_END"))
                .nl();
            printer.down().ident("} state;").nl();
            printer.down().ident(&format!("}} parallel{};", idx)).nl();
        }
        // Вложенная последовательность внутри параллели.
        //
        // Прежде она раскладывалась плоско, то есть без собственной машины шагов:
        // печать тика её пропускала молча (`_ => {}`), и ветвь `A + B` в прошивке не
        // исполнялась вовсе.
        //
        // Форма - та же группа, что у вложенной параллели: поля элементов плюс поле
        // `state` с шагами.
        StateExtend::Concatenation(items) => {
            let nested_prefix = format!("{}_concat{}", unique_prefix, idx);
            printer.ident("struct {").nl().up();
            for (inner_idx, item) in items.iter().enumerate() {
                build_parallel_item(printer, item, inner_idx, &nested_prefix)?;
            }
            build_step_state_enum(printer, &nested_prefix, items)?;
            printer.down().ident(&format!("}} concat{};", idx)).nl();
        }
    }
    Ok(())
}

/// Перечисление шагов вложенной последовательности.
///
/// Варианты именуются по элементу, а не по номеру: так же, как у последовательности
/// верхнего уровня (`build_concat_state_enum`), - иначе два способа назвать один шаг
/// разошлись бы при первой же правке.
fn build_step_state_enum(
    printer: &mut Printer,
    unique_prefix: &str,
    items: &[StateExtend],
) -> Result<(), Diagnostic> {
    let prefix = unique_prefix.to_uppercase();
    printer.ident("enum {").up().nl();
    printer.ident(&format!("{prefix}_INIT,")).nl();
    for (idx, item) in items.iter().enumerate() {
        let variant = match item {
            StateExtend::Model(model, _) => {
                format!("{}_{}{}", prefix, model.unique_uppercase_snakecase(), idx)
            }
            StateExtend::Parallel(_) => format!("{prefix}_PARALLEL{idx}"),
            StateExtend::Concatenation(_) => format!("{prefix}_CONCAT{idx}"),
            StateExtend::None => continue,
        };
        printer.ident(&format!("{variant},")).nl();
    }
    printer.ident(&format!("{prefix}_END")).nl();
    printer.down().ident("} state;").nl();
    Ok(())
}

/// Генерирует enum поля состояния для конкатенации. Вариант INIT + по одному варианту
/// на элемент: Model -> {STATE}_{MODEL}{idx}, Parallel -> {STATE}_PARALLEL{idx}.
fn build_concat_state_enum(
    printer: &mut Printer,
    state_name: &Name,
    items: &[StateExtend],
) -> Result<(), Diagnostic> {
    let prefix = state_name.unique_uppercase_snakecase();
    let mut variants: Vec<String> = vec![format!("{}_INIT", prefix)];
    for (idx, item) in items.iter().enumerate() {
        let variant = match item {
            StateExtend::None => continue,
            StateExtend::Model(model, _) => {
                format!(
                    "{}_{}{}",
                    prefix,
                    model.local_lowercase_snakecase().to_uppercase(),
                    idx,
                )
            }
            StateExtend::Parallel(_) | StateExtend::Concatenation(_) => {
                format!("{}_PARALLEL{}", prefix, idx)
            }
        };
        variants.push(variant);
    }
    variants.push(format!("{}_END", prefix));
    printer.ident("enum {").up().nl();
    let last = variants.len() - 1;
    for (i, variant) in variants.iter().enumerate() {
        if i < last {
            printer.ident(&format!("{},", variant)).nl();
        } else {
            printer.ident(variant).nl();
        }
    }
    printer
        .down()
        .ident(&format!(
            "}} {}_state;",
            state_name.local_lowercase_snakecase()
        ))
        .nl();
    Ok(())
}

fn generate_model_header(
    printer: &mut Printer,
    map: &CMap,
    name: Name,
    states: Vec<Name>,
    num: Option<usize>,
    main: bool,
) -> Result<usize, Diagnostic> {
    let num = num.unwrap_or(0);
    let model = map.raw_model_at(name.clone())?;
    let struct_name = name.unique_camelcase();
    // Комментарий автора перед объявлением модели. Их в корпусе больше всего - 103 из
    // 469: перед `model` автор пишет, что за установка описана и как она работает. У
    // корневой модели это вводный блок файла, и он же - самое ценное, что есть в
    // исходнике для читателя вывода.
    let model_loc = model.borrow().loc;
    for line in decl_comments(model_loc) {
        printer.print(&line).nl();
    }
    printer
        .print(format!("struct {} {{", struct_name).as_str())
        .nl();
    printer.up();
    for var in model.borrow().variables.clone().into_values() {
        match var {
            VariableNode::Unresolved => {}
            VariableNode::Simple { name, ty, loc, .. } => {
                // Пропускаем переменные, которые нигде не используются
                if !map.usage().variables.contains(&name) {
                    continue;
                }
                // Комментарий автора перед объявлением переменной модели.
                for line in decl_comments(loc) {
                    printer.ident(&line).nl();
                }
                // прежде отказ отображения давал CC-009 "Variable not found" - ошибку
                // не по адресу: переменная найдена, невыразим её тип. Теперь причина
                // доходит до пользователя (CC-014/015).
                let tv = typed_variable_or_diagnostic(
                    &ty,
                    &name,
                    &*model.borrow(),
                    map.float_width(),
                    &format!("переменная '{}'", name),
                )?;
                printer.ident(&tv).print(";").nl();
            }
            VariableNode::Port { .. } => {}
            VariableNode::Const { .. } => {}
        }
    }
    // Генерируем enum состояний модели
    let end_constant = name.unique_uppercase_snakecase() + "_END";
    printer.ident("enum {").up().nl();
    printer.ident(&*(name.unique_uppercase_snakecase() + "_INIT"));
    let mut end_already_generated = false;
    for state_name in states.clone() {
        if map.state_at(state_name.clone()).is_none() {
            continue;
        }
        printer.print(",").nl();
        let constant = state_name.unique_uppercase_snakecase();
        if constant == end_constant {
            end_already_generated = true;
        }
        printer.ident(&constant);
    }
    // Добавляем _END только если ни одно состояние не сгенерировало ту же константу
    if !end_already_generated {
        printer.print(",").nl();
        printer.ident(&end_constant);
    }
    printer.down().nl().ident("} state;").nl();
    // Механизмы времени: поля счётчика/метки эмитятся только при использовании `after`
    // (модель без времени - прежний вывод байт-в-байт). Логика в `c_time` (лимит
    // размера `c_header`).
    crate::generator::c::c_time::emit_state_time_fields(printer, map, &model.borrow())?;
    // Аккумуляторы периодических блоков `every`.
    crate::generator::c::c_every::emit_fields(printer, map, &model.borrow())?;
    // Генерируем поля extend-состояний
    let mut is_extend = false;
    for state_name in states {
        let Some(state) = map.state_at(state_name.clone()) else {
            warn!("State {} not used", state_name);
            continue;
        };
        if state.is_state()
            && let Element::StateExtend { extend, .. } = state
        {
            if !is_extend {
                is_extend = true;
            }
            build_extend_header(printer, &state_name, &extend)?;
        }
    }
    if main {
        let root_camelcase = map.root_name().unique_camelcase();
        let by_class = collect_ports_by_class(map)?;
        let has_in_bit = by_class.contains_key(&(PortClass::Bit, PortDirection::In));
        let has_out_bit = by_class.contains_key(&(PortClass::Bit, PortDirection::Out));
        let has_in_rational = by_class.contains_key(&(PortClass::Rational, PortDirection::In));
        let has_out_rational = by_class.contains_key(&(PortClass::Rational, PortDirection::Out));
        let has_in_numeric = by_class.contains_key(&(PortClass::Numeric, PortDirection::In));
        let has_out_numeric = by_class.contains_key(&(PortClass::Numeric, PortDirection::Out));
        let has_any = has_in_bit
            || has_out_bit
            || has_in_rational
            || has_out_rational
            || has_in_numeric
            || has_out_numeric;
        // Источник времени `now_ms` (профиль "часы") встаёт рядом с портовыми колбэками -
        // и требует `userdata`, даже если портов нет.
        let needs_now_ms = crate::generator::c::c_time::needs_now_ms(map, &model.borrow());
        if has_any || needs_now_ms {
            printer.ident("void  *userdata;").nl();
            if has_out_bit {
                let bit_out = PortClass::Bit
                    .qualified_enum_name_with_dir(&root_camelcase, PortDirection::Out);
                printer
                    .ident(&format!(
                        "void  (*{write_bit})({bit_out} port, uint8_t bit, bool val, void *userdata);",
                        write_bit = FUNCTION_PORT_WRITE_BIT
                    ))
                    .nl();
            }
            if has_in_bit {
                let bit_in =
                    PortClass::Bit.qualified_enum_name_with_dir(&root_camelcase, PortDirection::In);
                printer
                    .ident(&format!(
                        "bool  (*{read_bit} )({bit_in} port, uint8_t bit, void *userdata);",
                        read_bit = FUNCTION_PORT_READ_BIT
                    ))
                    .nl();
            }
            if has_out_rational {
                let rat_out = PortClass::Rational
                    .qualified_enum_name_with_dir(&root_camelcase, PortDirection::Out);
                printer
                    .ident(&format!(
                        "void  (*{write_float})({rat_out} port, uint8_t index, float val, void *userdata);",
                        write_float = FUNCTION_PORT_WRITE_FLOAT
                    ))
                    .nl();
            }
            if has_in_rational {
                let rat_in = PortClass::Rational
                    .qualified_enum_name_with_dir(&root_camelcase, PortDirection::In);
                printer
                    .ident(&format!(
                        "float (*{read_float} )({rat_in} port, uint8_t index, void *userdata);",
                        read_float = FUNCTION_PORT_READ_FLOAT
                    ))
                    .nl();
            }
            if has_out_numeric {
                let num_out = PortClass::Numeric
                    .qualified_enum_name_with_dir(&root_camelcase, PortDirection::Out);
                printer
                    .ident(&format!(
                        "void    (*{write_numeric})({num_out} port, uint8_t index, int64_t val, void *userdata);",
                        write_numeric = FUNCTION_PORT_WRITE_NUMERIC
                    ))
                    .nl();
            }
            if has_in_numeric {
                let num_in = PortClass::Numeric
                    .qualified_enum_name_with_dir(&root_camelcase, PortDirection::In);
                printer
                    .ident(&format!(
                        "int64_t (*{read_numeric} )({num_in} port, uint8_t index, void *userdata);",
                        read_numeric = FUNCTION_PORT_READ_NUMERIC
                    ))
                    .nl();
            }
            if needs_now_ms {
                printer
                    .ident(&format!(
                        "uint64_t (*{now_ms})(void *userdata);",
                        now_ms = crate::generator::c::FUNCTION_TIME_NOW_MS
                    ))
                    .nl();
            }
        }
    }

    printer.down();
    // Корректное закрытие typedef struct: } TypeName;
    printer.print("};".to_string().as_str()).nl();
    printer.nl();
    Ok(num)
}

/// Как цель называет себя в шапке: профиль HAL - часть имени.
pub(super) fn c_target_name(hal: bool) -> &'static str {
    if hal { "C (HAL profile)" } else { "C" }
}

/// Ведущие комментарии объявления в форме комментария C.
fn decl_comments(loc: crate::diagnostics::Location) -> Vec<String> {
    crate::generator::comments::leading(loc, crate::generator::header::CommentStyle::Slashes)
}

pub fn generate_header(
    filename: &str,
    map: &CMap,
    options: &crate::generator::GenerateOptions,
) -> Result<String, Diagnostic> {
    let mut header = String::new();
    let mut printer = Printer::new(4, &mut header);
    for line in file_header(c_target_name(options.hal), CommentStyle::Slashes) {
        printer.print(&line).nl();
    }
    // Умолчательный `now_ms` цели `c-hal` зовёт `clock_gettime(CLOCK_MONOTONIC)`, а на
    // строгом glibc под `-std=c11` этот символ скрыт без `_POSIX_C_SOURCE`. Объявляем у
    // самого верха - До любого системного заголовка, иначе feature-тест glibc уже
    // отработал. Только для c-hal с профилем "часы": прочий вывод неизменен.
    if options.hal
        && map
            .root_model_node()
            .is_some_and(|m| crate::generator::c::c_time::needs_now_ms(map, &m.borrow()))
    {
        printer
            .print("#ifndef _POSIX_C_SOURCE")
            .nl()
            .print("#define _POSIX_C_SOURCE 199309L")
            .nl()
            .print("#endif")
            .nl();
    }
    // Исправлено: заменяем '.' (не '\.') для корректного C-идентификатора #ifndef guard
    let id = normalize_lowercase_snakecase(filename.to_string())
        .replace(".", "_")
        .to_uppercase()
        + "_H__";
    printer.print("#ifndef ").print(&id).nl();
    printer.print("#define ").print(&id).nl();
    printer.print("#include <stdint.h>").nl();
    printer.print("#include <stdbool.h>").nl();
    printer.nl();

    // Отпечаток контракта частоты: готовый блок из `c_time`, если модель объявила
    // `clock` (иначе закреплять нечего). Печатается сырьём.
    if let Some(block) = c::c_time::clock_contract_block(map) {
        printer.print(&block);
    }

    // Топологически сортируем зависимые модели - зависимости идут первыми
    let sorted_models = c::topological_sort_models(map, map.using_models());

    // Forward declarations всех структур: позволяют компилятору C знать о типах раньше
    // их полного определения (важно при взаимных ссылках).
    //
    // Typedef корня эмитится безусловно - от наличия под-моделей он не зависит.
    // Структура печатается тегом (`struct {Root} { ... };`), а прототипы объявлены
    // через голое имя (`void {Root}_init({Root} *main);`), поэтому без typedef голое
    // имя типом не становится и порождённый C не компилируется: "must use 'struct' tag
    // to refer to type".
    //
    // Ветку `c-hal` (правка ) с ней слили в один путь: расхождение целей и было
    // причиной того, что `c-hal` работал, а `c` - нет. Единый путь не даёт этому
    // воспроизвестись.
    if !sorted_models.is_empty() {
        for element in &sorted_models {
            let Element::Model { name, .. } = element else {
                continue;
            };
            let s = name.unique_camelcase();
            printer.print(&format!("typedef struct {0} {0};", s)).nl();
        }
    }
    let root_struct = map.root_name().unique_camelcase();
    printer
        .print(&format!("typedef struct {0} {0};", root_struct))
        .nl();
    printer.nl();

    // Генерируем typedef struct для пользовательских структур
    if let Some(model_rc) = map.root_model_node() {
        let model = model_rc.borrow();
        // Порядок - по зависимостям: вложенная структура обязана быть объявлена раньше
        // вмещающей. Алфавитный порядок давал `cc` "unknown type name", потому что
        // `Line` стоит в алфавите раньше `Point`. Правило общее у трёх целей - носитель
        // один.
        let structs = crate::generator::struct_order::sorted(&model.structs);
        let structs: Vec<_> = structs.iter().collect();
        if !structs.is_empty() {
            for s in structs {
                // Комментарий автора перед объявлением типа: в корпусе таких - 45 из
                // 469, больше только у самой модели.
                for line in decl_comments(s.loc) {
                    printer.print(&line).nl();
                }
                printer
                    .print(&format!("typedef struct {} {{", s.name))
                    .nl()
                    .up();
                for (field_name, field_ty) in &s.fields {
                    // было `/* unsupported */ имя` - поле без типа, то есть невалидный
                    // C, выданный молча и с кодом 0.
                    let c_decl = c::typed_variable_or_diagnostic(
                        field_ty,
                        field_name,
                        &*model,
                        map.float_width(),
                        &format!("поле '{}' структуры '{}'", field_name, s.name),
                    )?;
                    printer.ident(&format!("{};", c_decl)).nl();
                }
                printer.down().print(&format!("}} {};", s.name)).nl();
            }
            printer.nl();
        }
    }

    // Генерируем enum типы для портов - до struct, чтобы можно было использовать в
    // сигнатурах
    generate_port_enums(&mut printer, map)?;

    let mut num = 0;
    for element in sorted_models {
        let Element::Model { name, states, .. } = element else {
            continue;
        };
        generate_model_header(&mut printer, map, name, states, Some(num), false)?;
        num += 1;
    }
    generate_model_header(
        &mut printer,
        map,
        map.root_name(),
        map.states(),
        Some(num),
        true,
    )?;
    let struct_name = map.root_name().unique_camelcase();
    printer
        .print("void ")
        .print(&struct_name)
        .print("_init(")
        .print(&struct_name)
        .print(" *main);")
        .nl();
    printer
        .print("void ")
        .print(&struct_name)
        .print("_tick(")
        .print(&struct_name)
        .print(" *main);")
        .nl();
    printer
        .print("void ")
        .print(&struct_name)
        .print("_reset(")
        .print(&struct_name)
        .print(" *main);")
        .nl();
    printer
        .print("bool ")
        .print(&struct_name)
        .print("_is_done(const ")
        .print(&struct_name)
        .print(" *main);")
        .nl();

    // в режиме `c-hal` - таблица адресов портов и умолчательный HAL (вынесены в `c_hal.rs`,
    // / - лимит размера модуля).
    if options.hal {
        super::c_hal::generate_hal(&mut printer, map, options)?;
    }

    printer.print("#endif").nl();
    Ok(header)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::generator::c::c_map::CMap;
    use crate::{parse, semantic};

    /// Строит `.h` для исходника с именем корневой модели.
    fn generate_h_content(src: &str, name: &str) -> String {
        let (model_ast, _) = parse(src, 0).unwrap();
        let model = semantic::tree::construct_model(&model_ast, None, &[]).unwrap();
        model.borrow_mut().name = Some(name.to_string());
        let model = model.borrow();
        let map = CMap::new(model.name(), &*model, true).unwrap();
        generate_header(
            map.get_filename(),
            &map,
            &crate::generator::GenerateOptions::default(),
        )
        .unwrap()
    }

    /// **T1.** Одиночная модель без под-моделей получает typedef корня.
    ///
    /// Дефект: typedef эмитился только при наличии под-моделей либо в цели `c-hal`, а
    /// ветка "под-моделей нет, цель `c`" не печатала ничего. Структура печатается тегом
    /// (`struct Demo { ... };`), а прототипы - через голое имя (`void Demo_init(Demo
    /// *main);`), поэтому без typedef имя типом не становится.
    #[test]
    fn test_single_model_without_submodels_gets_root_typedef() {
        let header = generate_h_content(
            "var n: u8 := 0;\nstart S { always { n := n + 1; } }",
            "Demo",
        );
        assert_eq!(
            header.matches("typedef struct Demo Demo;").count(),
            1,
            "typedef корня обязан быть ровно один:\n{header}"
        );
    }

    /// **T1.** Модель С под-моделями typedef корня не теряет и не дублирует.
    ///
    /// Тест слияния веток: раньше эту ветку правка не трогала, и она обязана остаться
    /// прежней.
    #[test]
    fn test_model_with_submodels_still_has_exactly_one_root_typedef() {
        let header = generate_h_content(
            "model A { start Q { } }\nvar n: u8 := 0;\nstart Entry = A;",
            "Demo",
        );
        assert_eq!(
            header.matches("typedef struct Demo Demo;").count(),
            1,
            "typedef корня обязан быть ровно один:\n{header}"
        );
        assert!(
            header.contains("typedef struct DemoA DemoA;"),
            "forward-декларация под-модели потеряна:\n{header}"
        );
    }

    /// **T3.** Заголовок одиночной модели самодостаточен: голое имя - тип.
    #[test]
    fn test_root_name_is_usable_as_type_in_prototypes() {
        let header = generate_h_content(
            "var n: u8 := 0;\nstart S { always { n := n + 1; } }",
            "Demo",
        );
        let typedef = header
            .find("typedef struct Demo Demo;")
            .expect("нет typedef корня");
        let proto = header.find("Demo_init(Demo *").expect("нет прототипа init");
        assert!(
            typedef < proto,
            "typedef обязан предшествовать прототипу, иначе имя ещё не тип:\n{header}"
        );
    }

    /// Enum выбирает минимальный беззнаковый тип по максимальному значению варианта.
    ///
    /// Граница: u8::MAX = 255.
    /// - max <= 255 -> uint8_t
    /// - 255 < max <= 65535 -> uint16_t
    /// - max > 65535 -> uint32_t (и далее uint64_t)
    #[test]
    fn enum_type_sized_by_maximum_variant() {
        // High=300 > u8::MAX(255) -> uint16_t p используется в always, чтобы попасть в
        // UsageSet.
        let src = r#"
enum Priority { Low = 0, Medium = 5, High = 300 }
var p: Priority := Low;
start Main { always { p := High; } }
        "#;
        let (model_ast, _) = parse(src, 0).unwrap();
        let model = semantic::tree::construct_model(&model_ast, None, &[]).unwrap();
        model.borrow_mut().name = Some("Test".to_string());
        let model = model.borrow();
        let map = CMap::new(model.name(), &*model, true).unwrap();
        let header = generate_header(
            map.get_filename(),
            &map,
            &crate::generator::GenerateOptions::default(),
        )
        .unwrap();
        assert!(
            header.contains("uint16_t"),
            "ожидался uint16_t для max=300, получено:\n{header}"
        );
        assert!(
            !header.contains("uint8_t p"),
            "uint8_t слишком мал для max=300:\n{header}"
        );

        // High=200 <= u8::MAX(255) -> uint8_t (ранее ошибочно давал uint16_t) lv
        // используется в always, чтобы попасть в UsageSet.
        let src2 = r#"
enum Levels { Low = 0, High = 200 }
var lv: Levels := Low;
start Main { always { lv := High; } }
        "#;
        let (model_ast2, _) = parse(src2, 0).unwrap();
        let model2 = semantic::tree::construct_model(&model_ast2, None, &[]).unwrap();
        model2.borrow_mut().name = Some("Test2".to_string());
        let model2 = model2.borrow();
        let map2 = CMap::new(model2.name(), &*model2, true).unwrap();
        let header2 = generate_header(
            map2.get_filename(),
            &map2,
            &crate::generator::GenerateOptions::default(),
        )
        .unwrap();
        assert!(
            header2.contains("uint8_t lv"),
            "ожидался uint8_t для max=200 (≤ u8::MAX=255), получено:\n{header2}"
        );
    }

    #[test]
    fn test_parallel_extend_numbering_starts_from_zero() {
        // state Par = Eng | Eng: внутри struct поля eng0, eng1 (по имени модели), сам
        // struct: `} par;` (по имени состояния, без номера).
        let src = r#"
model Eng { start S; }
start Main = Eng {
    next Par;
}
state Par = Eng | Eng;
"#;
        let (model_ast, _) = parse(src, 0).unwrap();
        let model = semantic::tree::construct_model(&model_ast, None, &[]).unwrap();
        model.borrow_mut().name = Some("Test".to_string());
        let model = model.borrow();
        let map = CMap::new(model.name(), &*model, true).unwrap();
        let header = generate_header(
            map.get_filename(),
            &map,
            &crate::generator::GenerateOptions::default(),
        )
        .unwrap();
        // Внутри параллельного блока поля нумеруются с 0 по имени модели
        assert!(
            header.contains("eng0;"),
            "первый элемент parallel должен быть eng0:\n{header}"
        );
        assert!(
            header.contains("eng1;"),
            "второй элемент parallel должен быть eng1:\n{header}"
        );
        // Сам struct закрывается именем состояния без номера
        assert!(
            header.contains("} par;"),
            "struct parallel должен закрываться как `}} par;`:\n{header}"
        );
        // Поля eng0/eng1 должны быть перед `} state;` внутри parallel struct Ищем
        // последний `} state;` до `} par;`, чтобы взять state именно parallel struct
        let pos_par_close = header.find("} par;").expect("} par; не найден");
        let pos_state = header[..pos_par_close]
            .rfind("} state;")
            .expect("} state; не найден до } par;");
        let pos_eng0 = header[..pos_par_close]
            .rfind("eng0;")
            .expect("eng0 не найден до } par;");
        assert!(
            pos_eng0 < pos_state,
            "поля должны быть перед state enum:\n{header}"
        );
    }

    #[test]
    fn test_concatenation_with_parallel_no_gaps() {
        // state Mid = Eng + (Eng | Eng) + Eng: idx 0: mid_eng0, idx 1: struct{ eng0,
        // eng1 } mid_parallel1, idx 2: mid_eng2.
        let src = r#"
model Eng { start S; }
start Main = Eng {
    next Mid;
}
state Mid = Eng + (Eng | Eng) + Eng;
"#;
        let (model_ast, _) = parse(src, 0).unwrap();
        let model = semantic::tree::construct_model(&model_ast, None, &[]).unwrap();
        model.borrow_mut().name = Some("Test".to_string());
        let model = model.borrow();
        let map = CMap::new(model.name(), &*model, true).unwrap();
        let header = generate_header(
            map.get_filename(),
            &map,
            &crate::generator::GenerateOptions::default(),
        )
        .unwrap();
        // Первый элемент конкатенации: {state}_{model}{idx}
        assert!(
            header.contains("mid_eng0;"),
            "первый элемент конкатенации должен быть mid_eng0:\n{header}"
        );
        // Параллельный блок под индексом 1
        assert!(
            header.contains("} mid_parallel1;"),
            "параллельный блок должен закрываться как `}} mid_parallel1;`:\n{header}"
        );
        // Последний элемент конкатенации под индексом 2
        assert!(
            header.contains("mid_eng2;"),
            "третий элемент конкатенации должен быть mid_eng2:\n{header}"
        );
        // Внутри parallel struct поля должны быть перед state enum Ищем последний `}
        // state;` до `} mid_parallel1;`
        let pos_par1_close = header
            .find("} mid_parallel1;")
            .expect("} mid_parallel1; не найден");
        let pos_state = header[..pos_par1_close]
            .rfind("} state;")
            .expect("} state; не найден до } mid_parallel1;");
        let pos_eng0 = header[..pos_par1_close]
            .rfind("eng0;")
            .expect("eng0 не найден до } mid_parallel1;");
        assert!(
            pos_eng0 < pos_state,
            "поля parallel должны быть перед state enum:\n{header}"
        );
        // concat state enum должен присутствовать
        assert!(
            header.contains("mid_state;"),
            "concat state enum mid_state должен быть сгенерирован:\n{header}"
        );
    }

    #[test]
    fn test_concatenation_generates_state_enum() {
        // state Mid = Eng + (Eng | Eng) + Eng: ожидается mid_state с вариантами INIT,
        // ENG0, PARALLEL1, ENG2.
        let src = r#"
model Eng { start S; }
start Main = Eng {
    next Mid;
}
state Mid = Eng + (Eng | Eng) + Eng;
"#;
        let (model_ast, _) = parse(src, 0).unwrap();
        let model = semantic::tree::construct_model(&model_ast, None, &[]).unwrap();
        model.borrow_mut().name = Some("Test".to_string());
        let model = model.borrow();
        let map = CMap::new(model.name(), &*model, true).unwrap();
        let header = generate_header(
            map.get_filename(),
            &map,
            &crate::generator::GenerateOptions::default(),
        )
        .unwrap();
        // enum поле состояния конкатенации
        assert!(
            header.contains("mid_state;"),
            "ожидался mid_state enum:\n{header}"
        );
        // вариант INIT
        assert!(
            header.contains("TEST_MID_INIT"),
            "ожидался TEST_MID_INIT:\n{header}"
        );
        // вариант для первого Model-элемента (idx=0)
        assert!(
            header.contains("TEST_MID_ENG0"),
            "ожидался TEST_MID_ENG0:\n{header}"
        );
        // вариант для Parallel-элемента (idx=1)
        assert!(
            header.contains("TEST_MID_PARALLEL1"),
            "ожидался TEST_MID_PARALLEL1:\n{header}"
        );
        // вариант для последнего Model-элемента (idx=2)
        assert!(
            header.contains("TEST_MID_ENG2"),
            "ожидался TEST_MID_ENG2:\n{header}"
        );
    }

    /// Struct с полями генерирует `typedef struct Name { ... } Name;` в заголовке.
    #[test]
    fn struct_typedef_in_header() {
        let src = r#"
struct Point { x: [bit;16], y: [bit;16] }
var pos: Point := 0;
start Main { always { pos.x := 1; } }
        "#;
        let (model_ast, _) = parse(src, 0).unwrap();
        let model = semantic::tree::construct_model(&model_ast, None, &[]).unwrap();
        model.borrow_mut().name = Some("Test".to_string());
        let model = model.borrow();
        let map = CMap::new(model.name(), &*model, true).unwrap();
        let header = generate_header(
            map.get_filename(),
            &map,
            &crate::generator::GenerateOptions::default(),
        )
        .unwrap();
        assert!(
            header.contains("typedef struct Point"),
            "ожидался typedef struct Point:\n{header}"
        );
        assert!(
            header.contains("} Point;"),
            "ожидалось закрытие }} Point;:\n{header}"
        );
    }
}
