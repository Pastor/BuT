//! Инициализация модели в цель C: `_init`-функции.
//!
//! Чистое перемещение: вывод байт-в-байт неизменен. Здесь - инициализация стартового
//! состояния и вложенных элементов в `_init`: только память (вызовы `_init` вложенных,
//! стартовые служебные состояния композитов); блоки `enter` - поведение, они остаются в
//! `_tick` (`c_model.rs`).

use super::c_blocks::generate_scalar_init;
use super::c_expr::generate_expr;
use crate::diagnostics::{Diagnostic, Location};
use crate::generator::c::PortClass;
use crate::generator::c::c_map::CMap;
use crate::generator::indent::Printer;
use crate::semantic::extend::ParameterArgument;
use crate::semantic::minimap::{Element, Name, StateExtend};
use crate::semantic::type_node::TypeNode;
use crate::semantic::{ExpressionNode, PortDirection, VariableNode};

pub(super) fn generate_model_init(
    printer: &mut Printer,
    model: &Element,
    map: &CMap,
) -> Result<(), Diagnostic> {
    let Element::Model {
        start,
        states: _,
        name,
    } = model
    else {
        return Err(Diagnostic::error(
            crate::generator::site::at(Location::Codegen),
            "Элемент не является моделью".to_string(),
        )
        .with_code("CC-006"));
    };
    let raw_rc = map.raw_model_at(name.clone())?;
    let raw = &*raw_rc.borrow();
    printer
        .ident("model->state = ")
        .print(&name.unique_uppercase_snakecase())
        .print("_INIT;")
        .nl();
    // Счётчик/метка времени: счётчик тактов = 0 (на первом такте с входа не прошло ни
    // одного такта), метка "часы" латчит `now_ms`. `takt_prev_state` совпадает с
    // начальным состоянием, поэтому вход в стартовое распознаётся сменой состояния.
    // Логика в `c_time` (лимит размера).
    let init_const = format!("{}_INIT", name.unique_uppercase_snakecase());
    // HAL-указатель: `model` у корня, `main` у под-модели (как порты).
    let hal_ptr = if name.eq(&map.root_name()) {
        "model"
    } else {
        "main"
    };
    crate::generator::c::c_time::emit_state_time_init(printer, map, raw, &init_const, hal_ptr)?;
    // 0033 (R6): инициализация вложенных элементов стартового состояния в `_init`, а не
    // в `_tick` - память приводится в определённое состояние до первого `_tick`,
    // поэтому чтение полей между `_init` и `_tick` перестаёт быть UB. Рекурсивно по
    // уровням: `_init` вложенной модели инициализирует свои вложенные. Блоки `enter`
    // сюда не входят - они поведение и остаются в `_tick`.
    let is_main = name.eq(&map.root_name());
    generate_start_state_init(printer, map, model, start, is_main)?;
    for var in raw.variables.values() {
        let VariableNode::Simple {
            name: var_name,
            ty,
            expr,
            loc,
            ..
        } = var
        else {
            continue;
        };
        // Объявление объявляет своё место: отказ об инициализаторе рождается вне
        // операторов и печатался без координаты.
        crate::generator::site::enter_declaration(*loc);
        // Пропускаем неиспользуемые переменные - они не попадают в struct
        if !map.usage().variables.contains(var_name) {
            continue;
        }
        // Переменная без инициализатора получает ноль.
        if let ExpressionNode::None = expr {
            crate::generator::c::c_zero_init::emit_zero_init(printer, &var.name(), ty, raw)?;
            continue;
        }
        // массив в C **не присваивается** - ни скаляром, ни агрегатом. Общая ветка ниже
        // печатала `model->x = ...` для любого типа, что на настоящем массиве даёт
        // "array type is not assignable".
        if is_real_array(ty) {
            generate_array_init(printer, map, model, &var.name(), ty, expr)?;
            continue;
        }
        // Бит-вектор шире 64 бит - массив слов, а массив в C не присваивается:
        // `model->w = 0;` отвергает `cc` ("array type is not assignable"). Заполняем по
        // словам.
        if let Some(count) = crate::generator::c::c_bits::words_of_type(ty) {
            generate_wide_bits_init(printer, map, model, &var.name(), count, expr)?;
            continue;
        }
        generate_scalar_init(printer, map, model, &var.name(), ty, expr, &raw_rc)?;
    }
    // Слой объявления снимается парно входу: переживи он цикл, отказ в теле получил бы
    // координату последней переменной.
    crate::generator::site::leave_declaration();
    generate_port_initial_values(printer, map, model, raw, name, hal_ptr)?;
    Ok(())
}

/// Выставляет начальные значения выходных портов модели.
///
/// # Где именно
///
/// В `_init`, **последним** действием: к этому моменту память модели уже приведена в
/// определённое состояние, и запись наружу - единственный шаг, который виден за
/// пределами структуры. Так выполняется R5: значение выставлено **до первого такта**, а
/// не в такте 1 (там живут блоки `enter`).
fn generate_port_initial_values(
    printer: &mut Printer,
    map: &CMap,
    model: &Element,
    raw: &crate::semantic::ModelNode,
    model_name: &Name,
    hal_ptr: &str,
) -> Result<(), Diagnostic> {
    for var in raw.variables.values() {
        let VariableNode::Port {
            name: port_name,
            ty,
            init,
            direction,
            ..
        } = var
        else {
            continue;
        };
        if matches!(init, ExpressionNode::None) || *direction == PortDirection::In {
            continue;
        }
        let variant = crate::generator::c::c_names::port_enum_variant(
            model_name,
            port_name,
            *direction,
            crate::parser::ast::PortDirection::Out,
        );
        let cls = PortClass::from_type(ty);
        // Начальное значение массива-порта раздаётся по элементам: агрегат `{4, 5, 6}`
        // целиком означал бы `write_numeric(PORT, 0, {4, 5, 6}, ud)` - "expected
        // expression" у `cc` при нулевом коде возврата.
        if let ExpressionNode::Array(items) | ExpressionNode::Initializer(items) = init
            && matches!(ty, crate::semantic::type_node::TypeNode::Array(..))
            && crate::semantic::bit_vector::is_bit_vector(ty).is_none()
        {
            for (index, item) in items.iter().enumerate() {
                let mut element = String::new();
                {
                    let mut tmp = Printer::new(0, &mut element);
                    generate_expr(&mut tmp, map, model, vec![], item, 0, true)?;
                }
                printer
                    .ident(&format!(
                        "{};",
                        crate::generator::c::c_port_call::write(
                            cls,
                            hal_ptr,
                            &variant,
                            &index.to_string(),
                            &element,
                        )
                    ))
                    .nl();
            }
            continue;
        }
        // Начальное значение печатается через тот же носитель, что и запись в такте:
        // разойдись они - старт писал бы порт вызовом с другим числом аргументов, и
        // `cc` отвергал бы файл при нулевом коде возврата.
        let mut value = String::new();
        {
            let mut tmp = Printer::new(0, &mut value);
            generate_expr(&mut tmp, map, model, vec![], init, 0, true)?;
        }
        printer
            .ident(&format!(
                "{};",
                crate::generator::c::c_port_call::write(
                    cls,
                    hal_ptr,
                    &variant,
                    crate::generator::c::c_port_call::SCALAR_INDEX,
                    &value,
                )
            ))
            .nl();
    }
    Ok(())
}

/// Присваивает значения аргументов инстанцирования полям экземпляра - режим
/// `--parameters=assign`.
///
/// Печатается **сразу после** вызова `_init` этого экземпляра: `_init` кладёт значения
/// по умолчанию, а настройка места инстанцирования их перекрывает. Одна `_init` на все
/// экземпляры - в этом и состоит выбор умолчания: копий модели не возникает.
fn generate_argument_assignments(
    printer: &mut Printer,
    map: &CMap,
    model: &Element,
    access: &str,
    args: &[ParameterArgument],
) -> Result<(), Diagnostic> {
    for arg in args {
        // Агрегат в C **не присваивается**: `p.prog = {9, 8, 7, 6};` - не выражение, а
        // инициализатор объявления, и `cc` отвергает такую строку ("expected
        // expression"). Пишем поэлементно - той же формой, какой `_init` кладёт
        // значения по умолчанию.
        if let ExpressionNode::Initializer(elems) | ExpressionNode::Array(elems) = &arg.value {
            for (i, elem) in elems.iter().enumerate() {
                printer.ident(&format!("{}.{}[{}] = ", access, arg.name, i));
                generate_expr(printer, map, model, vec![], elem, 0, true)?;
                printer.print(";").nl();
            }
            continue;
        }
        printer.ident(&format!("{}.{} = ", access, arg.name));
        generate_expr(printer, map, model, vec![], &arg.value, 0, true)?;
        printer.print(";").nl();
    }
    Ok(())
}

/// Эмитит инициализацию вложенных элементов стартового состояния в `_init`. Только
/// память (вызовы `_init` вложенных, установка стартовых служебных состояний
/// композитов) - блоки `enter` сюда не входят: это поведение, оно остаётся в `_tick`.
/// Форма зеркальна прежней INIT-диспетчеризации, но без `enter` и без установки
/// состояния самой модели.
fn generate_start_state_init(
    printer: &mut Printer,
    map: &CMap,
    model_element: &Element,
    start: &Name,
    is_main: bool,
) -> Result<(), Diagnostic> {
    let append = if !is_main { ", main" } else { ", model" };
    if let Some(Element::StateExtend {
        name: state_name,
        extend,
        ..
    }) = map.state_at(start.clone())
    {
        match extend {
            StateExtend::Model(name, args) => {
                // Имя поля печатается той же функцией, что и его объявление
                // (`c_header.rs`): `local().to_lowercase()` даёт `twowords` там, где
                // объявлено `two_words`, и вывод не компилируется. Односложное имя
                // дефект скрывает - обе нормализации на нём совпадают.
                let access = format!("model->{}", state_name.local_lowercase_snakecase());
                printer
                    .ident(&format!("{}_init(&{}", name.unique_camelcase(), access))
                    .print(map.root_arg(
                        &name,
                        append == ", model",
                        crate::generator::c::c_needs::ModelFn::Init,
                    ))
                    .print(");")
                    .nl();
                generate_argument_assignments(printer, map, model_element, &access, &args)?;
            }
            StateExtend::Parallel(steps) => {
                let local = state_name.local_lowercase_snakecase();
                let unique_upper = state_name.unique_uppercase_snakecase();
                let access = format!("model->{}", local);
                generate_parallel_items_init(
                    printer,
                    map,
                    model_element,
                    &access,
                    &unique_upper,
                    &steps,
                    append,
                )?;
                printer
                    .ident(&format!("model->{}.state = {}_INIT;", local, unique_upper))
                    .nl();
            }
            StateExtend::Concatenation(steps) => {
                let local = state_name.local_lowercase_snakecase();
                let unique_upper = state_name.unique_uppercase_snakecase();
                if let Some(first) = steps.first() {
                    let variant = generate_concat_item_init(
                        printer,
                        map,
                        model_element,
                        (&local, &unique_upper),
                        first,
                        0,
                        append,
                    )?;
                    printer
                        .ident(&format!("model->{}_state = {};", local, variant))
                        .nl();
                }
            }
            StateExtend::None => {}
        }
    }
    Ok(())
}

/// Настоящий ли это массив C (`elem name[N]`), а не бит-вектор.
///
/// `[bit;N]` при N ∈ {8,16,32,64} - **скаляр** `uint{N}_t` (доминирующая идиома
/// корпуса), и присваивание ему законно. Прочие массивы - агрегаты.
fn is_real_array(ty: &TypeNode) -> bool {
    matches!(ty, TypeNode::Array(_, elem) if !matches!(**elem, TypeNode::Bit))
}

/// Инициализирует настоящий массив в `_init` - поэлементно.
///
/// # Скалярный инициализатор массива не поддерживается намеренно
///
/// `var data: [u8;4] := 0;` - что это значит, язык **не определяет**: обнулить весь
/// массив? записать 0 в первый элемент? Три ответа расходятся уже сегодня (кандидат
/// "Семантика `[bit;N]` расходится втрое" в `FEATURES.md`): цель `st` инициализатор
/// молча отбрасывает, полагаясь на обнуление по правилам IEC; симулятор кладёт в
/// переменную **скаляр** `Number(0)`, после чего чтение `data[0]` даёт `SIM-010`
/// ("переменная не является массивом" - проба). Выбрать один ответ здесь - значит
/// решить вопрос семантики языка, на что не уполномочена (см. её карточку). Поэтому -
/// `CC-017`, а не догадка.
fn generate_array_init(
    printer: &mut Printer,
    map: &CMap,
    model: &Element,
    field: &str,
    ty: &TypeNode,
    expr: &ExpressionNode,
) -> Result<(), Diagnostic> {
    let TypeNode::Array(size, _) = ty else {
        return Err(Diagnostic::error(
            crate::generator::site::at(Location::Codegen),
            format!("переменная '{}': ожидался массив", field),
        )
        .with_code("CC-017"));
    };
    // Агрегат записывается двумя формами: `:= {0, 0}` (`Initializer`) и `:= [0, 0]`
    // (`Array`). Для C они неразличимы - обе дают поэлементную запись; принимать только
    // одну значило бы отвергать корректный исходник.
    let (ExpressionNode::Initializer(elems) | ExpressionNode::Array(elems)) = expr else {
        return Err(Diagnostic::error(
            crate::generator::site::at(Location::Codegen),
            format!(
                "переменная '{}': скалярный инициализатор массива не выразим в C — \
                 массив в C не присваивается; используйте агрегат вида ':= {{0, 0, …}}'",
                field
            ),
        )
        .with_code("CC-017"));
    };
    if elems.len() != usize::from(*size) {
        return Err(Diagnostic::error(
            crate::generator::site::at(Location::Codegen),
            format!(
                "переменная '{}': инициализатор из {} элементов не соответствует массиву [{}]",
                field,
                elems.len(),
                size
            ),
        )
        .with_code("CC-017"));
    }
    // Элемент-Структура печатается по полям: агрегатной формы присваивания в C нет
    // вовсе, и `model->cells[0] = {1, 2};` давало `cc` "expected expression" при
    // нулевом коде возврата `taktc`.
    //
    // Агрегат раскрывается до листьев общим носителем: своя рекурсия здесь была копией
    // той, что живёт в печатнике операторов, - а правило одно.
    let fields_of = |name: &str| {
        map.root_model_node()
            .and_then(|root| root.borrow().search_struct(name))
            .map(|def| def.fields)
    };
    for leaf in crate::generator::aggregate::leaves(Some(ty), elems, &fields_of) {
        let suffix = crate::generator::aggregate::c_like_suffix(&leaf.path);
        printer.ident(&format!("model->{field}{suffix} = "));
        generate_expr(printer, map, model, vec![], leaf.value, 0, true)?;
        printer.print(";").nl();
    }
    Ok(())
}

/// Инициализирует бит-вектор шире 64 бит - по словам.
///
/// Литерал шире 64 бит язык не принимает (`LE-009`), поэтому значение достаётся
/// младшему слову, а прочие обнуляются. Форма, не сводимая к числу (например копия
/// другого вектора в инициализаторе), отвергается `CC-022`: печатать вместо неё
/// присваивание массиву значило бы вернуть тот самый невалидный C, ради которого фича и
/// заведена.
fn generate_wide_bits_init(
    printer: &mut Printer,
    map: &CMap,
    model: &Element,
    field: &str,
    count: u16,
    expr: &ExpressionNode,
) -> Result<(), Diagnostic> {
    let ExpressionNode::Number(_) = expr else {
        return Err(crate::generator::c::c_unsupported::refuse(
            crate::generator::c::c_unsupported::UnsupportedNode::WideBitVector("инициализатор"),
            expr.loc(),
        ));
    };
    let mut value = String::new();
    {
        let mut tmp = Printer::new(4, &mut value);
        generate_expr(&mut tmp, map, model, vec![], expr, 0, true)?;
    }
    printer
        .ident(&crate::generator::c::c_bits::fill_words(
            &format!("model->{field}"),
            count,
            &value,
        ))
        .print(";")
        .nl();
    Ok(())
}

/// Генерирует вызовы `_init` для элементов параллельного блока (рекурсивно).
///
/// * `parent_access` - путь к полю-структуре параллели (например, `"model->start"`).
/// * `parent_unique_upper` - уникальный преисправление enum в UPPER_SNAKE_CASE
///   (например, `"EXTEND_COMPLEX_C_START"`), используется для формирования имён
///   enum-вариантов вложенных параллелей.
fn generate_parallel_items_init(
    printer: &mut Printer,
    map: &CMap,
    model_element: &Element,
    parent_access: &str,
    parent_unique_upper: &str,
    items: &[StateExtend],
    append: &str,
) -> Result<(), Diagnostic> {
    for (idx, item) in items.iter().enumerate() {
        match item {
            StateExtend::Model(name, args) => {
                let access = format!(
                    "{}.{}{}",
                    parent_access,
                    name.local_lowercase_snakecase(),
                    idx
                );
                printer
                    .ident(&format!(
                        "{}_init(&{}{});",
                        name.unique_camelcase(),
                        access,
                        map.root_arg(
                            name,
                            append == ", model",
                            crate::generator::c::c_needs::ModelFn::Init
                        ),
                    ))
                    .nl();
                generate_argument_assignments(printer, map, model_element, &access, args)?;
            }
            StateExtend::Parallel(inner) => {
                let nested_access = format!("{}.parallel{}", parent_access, idx);
                let nested_upper = format!("{}_PARALLEL{}", parent_unique_upper, idx);
                generate_parallel_items_init(
                    printer,
                    map,
                    model_element,
                    &nested_access,
                    &nested_upper,
                    inner,
                    append,
                )?;
                printer
                    .ident(&format!("{}.state = {}_INIT;", nested_access, nested_upper))
                    .nl();
            }
            // Вложенная последовательность: инициализируется первый шаг и состояние
            // цепочки. Остальные шаги не трогаются: их `_init` зовёт такт по завершении
            // предыдущего - так же, как у последовательности верхнего уровня, иначе шаг
            // начал бы отсчёт времени раньше, чем до него дошла очередь.
            StateExtend::Concatenation(inner) => {
                let nested_access = format!("{}.concat{}", parent_access, idx);
                let nested_upper = format!("{}_CONCAT{}", parent_unique_upper, idx);
                if let Some((first_idx, StateExtend::Model(first, args))) = inner
                    .iter()
                    .enumerate()
                    .find(|(_, item)| matches!(item, StateExtend::Model(..)))
                {
                    let access = format!(
                        "{}.{}{}",
                        nested_access,
                        first.local_lowercase_snakecase(),
                        first_idx
                    );
                    printer
                        .ident(&format!(
                            "{}_init(&{}{});",
                            first.unique_camelcase(),
                            access,
                            map.root_arg(
                                first,
                                append == ", model",
                                crate::generator::c::c_needs::ModelFn::Init
                            ),
                        ))
                        .nl();
                    generate_argument_assignments(printer, map, model_element, &access, args)?;
                    printer
                        .ident(&format!(
                            "{}.state = {}_{}{};",
                            nested_access,
                            nested_upper,
                            first.unique_uppercase_snakecase(),
                            first_idx
                        ))
                        .nl();
                } else {
                    printer
                        .ident(&format!("{}.state = {}_END;", nested_access, nested_upper))
                        .nl();
                }
            }
            StateExtend::None => {}
        }
    }
    Ok(())
}

/// Перечислитель шага - у общего носителя `c_chain`.
///
/// Форма, которую печать такта не ведёт (вложенная цепочка), доходить сюда не должна: у
/// неё нет ни варианта, ни тика, - и она получает `CC-007`.
fn variant_of(
    state_unique_upper: &str,
    item: &StateExtend,
    idx: usize,
) -> Result<String, Diagnostic> {
    crate::generator::c::c_chain::step_variant(state_unique_upper, item, idx).ok_or_else(|| {
        Diagnostic::error(
            crate::generator::site::at(Location::Codegen),
            "Неподдерживаемый тип элемента конкатенации".to_string(),
        )
        .with_code("CC-007")
    })
}

/// Генерирует вызов `_init` для одного элемента конкатенации и возвращает
/// соответствующий вариант enum `{state_local}_state`.
///
/// * `state_local` - локальное имя состояния в lowercase_snake_case (например, `"start"`).
/// * `state_unique_upper` - уникальный преисправление enum в UPPER_SNAKE_CASE
///   (например, `"EXTEND_COMPLEX_START"`).
pub(super) fn generate_concat_item_init(
    printer: &mut Printer,
    map: &CMap,
    model_element: &Element,
    // Локальное и UPPER-имя несущего состояния - парой: это одно имя в двух регистрах,
    // а не два независимых параметра (и лимит аргументов clippy).
    (state_local, state_unique_upper): (&str, &str),
    item: &StateExtend,
    idx: usize,
    append: &str,
) -> Result<String, Diagnostic> {
    match item {
        StateExtend::Model(name, args) => {
            // Имена машины шагов - у общего носителя.
            let access = crate::generator::c::c_chain::model_access(state_local, name, idx);
            printer
                .ident(&format!(
                    "{}_init(&{}{});",
                    name.unique_camelcase(),
                    access,
                    map.root_arg(
                        name,
                        append == ", model",
                        crate::generator::c::c_needs::ModelFn::Init
                    ),
                ))
                .nl();
            generate_argument_assignments(printer, map, model_element, &access, args)?;
            variant_of(state_unique_upper, item, idx)
        }
        StateExtend::Parallel(inner) => {
            let access = crate::generator::c::c_chain::parallel_access(state_local, idx);
            let nested_upper =
                crate::generator::c::c_chain::parallel_upper(state_unique_upper, idx);
            generate_parallel_items_init(
                printer,
                map,
                model_element,
                &access,
                &nested_upper,
                inner,
                append,
            )?;
            printer
                .ident(&format!("{}.state = {}_INIT;", access, nested_upper))
                .nl();
            variant_of(state_unique_upper, item, idx)
        }
        _ => Err(Diagnostic::error(
            crate::generator::site::at(Location::Codegen),
            "Неподдерживаемый тип элемента конкатенации".to_string(),
        )
        .with_code("CC-007")),
    }
}
