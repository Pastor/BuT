//! Объявления переменных и типов `FUNCTION_BLOCK` для цели Structured Text.
//!
//! Модуль печатает два пласта вывода:
//!
//! - **`TYPE ... END_TYPE`** - объявления структур, общие для файла и печатаемые
//!   **до** первого использования (в IEC 61131-3 порядок объявлений значим).
//!   Пласт новый: ни цель `c` (там `struct` печатается по месту), ни `plantuml`
//!   (типы не печатаются вовсе) аналога не имеют.
//! - **`VAR_INPUT` / `VAR_OUTPUT` / `VAR_IN_OUT` / `VAR` / `VAR CONSTANT`** -
//!   секции объявлений внутри `FUNCTION_BLOCK`.
//!
//! ## Перечисления: константы вместо перечислимого типа
//!
//! Перечисление Takt не становится `TYPE ... : (...); END_TYPE` - MatIEC отвергает
//! явные значения вариантов. Действует откат Option C
//! тип варианта считает [`get_st_type`], а сами варианты объявляются
//! именованными константами `<Перечисление>_<Вариант>` в секции `VAR CONSTANT`
//! **внутри** блока. Не `VAR_GLOBAL CONSTANT`, как предполагал: `VAR_GLOBAL`
//! вне `CONFIGURATION` недопустим, а цель `st` `CONFIGURATION` не
//! эмитит.

use crate::diagnostics::Diagnostic;
use crate::generator::indent::Printer;
use crate::generator::st::st_reserved::{check_st_declaration, check_st_local_clash};
use crate::generator::st::st_type::{self, get_st_type};
use crate::semantic::type_node::TypeNode;
use crate::semantic::unused::UsageSet;
use crate::semantic::{ExpressionNode, ModelNode, PortDirection, VariableNode};
use std::fmt::Write as _;

/// Одно объявление вида `имя : ТИП := значение;` внутри секции `VAR...`.
struct Declaration {
    name: String,
    ty: String,
    init: Option<String>,
}

impl Declaration {
    /// Печатает объявление одной строкой.
    fn write(&self, p: &mut Printer) {
        let mut line = String::new();
        let _ = write!(line, "{} : {}", self.name, self.ty);
        if let Some(init) = &self.init {
            let _ = write!(line, " := {}", init);
        }
        line.push(';');
        p.ident(&line).nl();
    }
}
/// Дополнения к объявлениям, известные только вызывающему.
///
/// Секции `VAR` печатаются один раз, поэтому всё, что рождается при печати тела
/// (поднятые объявления, экземпляры под-FB) и всё, что синтезирует генератор (`state`,
/// `is_done`, `VAR_IN_OUT`), приходит сюда, а не печатается отдельно: вторая секция
/// `VAR` в одном POU недопустима.
#[derive(Default)]
pub(crate) struct Extras {
    /// Эмитить `state : USINT := 0;` - переменную автомата.
    pub state_var: bool,
    /// Эмитить `is_done : BOOL;` в `VAR_OUTPUT` - признак завершения.
    pub is_done: bool,
    /// Переменные корня, разделяемые через `VAR_IN_OUT` (О1-в).
    pub shared: Vec<(String, TypeNode)>,
    /// Имя модели-владельца разделяемых переменных (корня) - им квалифицируется имя
    /// именованного типа массива. Пусто, когда `shared` пуст.
    pub shared_owner: String,
    /// То же имя, когда печатается **сам корень**: его собственные массивы тоже
    /// объявляются именованным типом, иначе MatIEC сочтёт типы параметра и значения
    /// несовместимыми.
    pub root_owner: Option<String>,
    /// Имена массивов, которым нужен именованный тип, - те, что **фактически**
    /// передаются под-моделям.
    ///
    /// Список общий у продюсера (`TYPE ... END_TYPE`) и потребителя (объявление
    /// переменной): без него они разъезжаются - проба дала объявление `data :
    /// ArrayVar_data_arr;` при **отсутствующем** типе, то есть ссылку в пустоту.
    pub named_arrays: Vec<String>,
    /// Формы массивов, объявленные именованным типом ради параметров функций:
    /// `TAKT_ARR_2_USINT` и подобные.
    ///
    /// Список нужен, чтобы имя получали **только** те массивы, чья форма действительно
    /// передаётся в функцию: иначе именованным стал бы каждый массив вывода - правка
    /// формы там, где ничего не ломалось.
    pub array_forms: Vec<String>,
    /// Константные массивы таблицы переходов: имя, тип, значение.
    ///
    /// Печатаются в `VAR CONSTANT` наравне с прочими константами: строки таблицы -
    /// данные, и место им в объявлениях, а не в теле.
    pub table_constants: Vec<(String, String, String)>,
    /// Экземпляры под-FB: `(имя, тип)`. Экземпляры под-FB: имя, тип, инициализатор
    /// экземпляра.
    ///
    /// Инициализатор - уже напечатанная строка вида `(step := 5)` либо `None`: печатает
    /// её st/mod.rs, которому доступны типы параметров целевой модели.
    pub instances: Vec<(String, String, Option<String>)>,
    /// Объявления, поднятые из тела (`st_stmt`).
    pub hoisted: Vec<(String, TypeNode)>,
    /// Цель `st-at`: порты размещены глобально, поэтому блок видит их через
    /// `VAR_EXTERNAL`, а не объявляет своими входами/выходами.
    pub external_ports: bool,
}

/// Печатает все секции объявлений одного `FUNCTION_BLOCK`.
///
/// Возвращает `true`, если напечатана хотя бы одна секция.
///
/// # Фильтр неиспользуемых
///
/// Неиспользуемые переменные, порты и константы не объявляются - так же поступает цель
/// `c` (`c_header.rs:344`). Это **не** тихая потеря класса Д1b: о неиспользуемом имени
/// уже сообщает семантика (Ce13, [`crate::unused_variable_warnings`]) - то есть
/// диагностика есть, просто не здесь. Потеря без диагностики была бы у **используемой**
/// переменной; такой исход исключён сигнатурой [`get_st_type`] (`Result`, а не
/// `Option`).
pub(crate) fn emit_declarations(
    p: &mut Printer,
    model: &ModelNode,
    usage: &UsageSet,
    extras: &Extras,
) -> Result<bool, Diagnostic> {
    let mut inputs = Vec::new();
    let mut outputs = Vec::new();
    let mut in_outs = Vec::new();
    let mut externals = Vec::new();
    let mut locals = Vec::new();
    let mut constants = enum_constants(model)?;
    // Владелец именованных типов массивов. У корня это он сам - разделяются его
    // переменные; у под-модели имя приходит вместе со списком `shared`. Пусто - типов
    // нет и объявление обычное.
    let array_owner: Option<&str> = if extras.shared_owner.is_empty() {
        extras.root_owner.as_deref()
    } else {
        Some(extras.shared_owner.as_str())
    };
    let named_arrays = extras.named_arrays.as_slice();

    // Признак завершения - выход FB: по нему родитель узнаёт об окончании.
    if extras.is_done {
        outputs.push(Declaration {
            name: "is_done".to_string(),
            ty: "BOOL".to_string(),
            init: None,
        });
    }
    // Переменная автомата. Ноль - это `INIT`: холодный старт ПЛК обнуляет `VAR`,
    // поэтому отдельная инициализация не нужна.
    if extras.state_var {
        locals.push(Declaration {
            name: "state".to_string(),
            ty: "USINT".to_string(),
            init: Some("0".to_string()),
        });
    }
    for (name, ty) in &extras.shared {
        // Массив в параметре объявляется именованным типом: MatIEC отвергает анонимный
        // `ARRAY [...] OF T` в `VAR_IN_OUT` ("Data type incompatibility ... when
        // invoking FB"), а до фичи цель печатала именно его и рапортовала об успехе -
        // арбитром был чужой инструмент.
        let ty_text = if named_arrays.iter().any(|n| n == name) {
            st_type::shared_array_type_name(&extras.shared_owner, name)
        } else {
            get_st_type(ty, model)?
        };
        in_outs.push(Declaration {
            name: name.clone(),
            ty: ty_text,
            init: None,
        });
    }
    for (name, fb_type, init) in &extras.instances {
        locals.push(Declaration {
            name: name.clone(),
            ty: fb_type.clone(),
            init: init.clone(),
        });
    }
    for (name, ty) in &extras.hoisted {
        if locals.iter().any(|d| &d.name == name) {
            continue;
        }
        locals.push(Declaration {
            name: name.clone(),
            // Локальный массив, чья форма встречается в параметре функции, объявляется
            // Той же формой - иначе `iec2c` отвергает вызов: типы аргумента и параметра
            // он сверяет буквально.
            ty: st_type::local_declaration_type(ty, model, &extras.array_forms)?,
            init: None,
        });
    }

    // Занятые имена POU. Набор строится из того, что цель уже напечатала: служебные
    // `state`/`is_done`, разделяемые переменные, экземпляры под-моделей, поднятые
    // локальные и константы перечислений. Отдельного списка служебных имён не заводится -
    // он разошёлся бы с печатью при первом же новом служебном объявлении.
    let mut occupied: Vec<String> = inputs
        .iter()
        .chain(outputs.iter())
        .chain(in_outs.iter())
        .chain(externals.iter())
        .chain(locals.iter())
        .chain(constants.iter())
        .map(|d: &Declaration| d.name.clone())
        .collect();

    let mut names: Vec<&String> = model.variables.keys().collect();
    names.sort();
    for key in names {
        match &model.variables[key] {
            VariableNode::Unresolved => {}
            VariableNode::Simple {
                name,
                ty,
                expr,
                loc,
                ..
            } => {
                if !usage.variables.contains(name) {
                    continue;
                }
                // Проверка стоит после фильтра использования: неиспользуемую переменную
                // генератор не эмитит, `iec2c` её не увидит - значит и ST-014 на неё
                // срабатывать не должна (иначе `var action: Action` из elevator.takt,
                // объявленный, но не используемый, сломал бы сборку). Столкновение
                // проверяется на самом эмитируемом имени.
                check_st_declaration(name, model, *loc)?;
                // Разделяемая переменная уже объявлена в `VAR_IN_OUT`: повторное
                // объявление в `VAR` сделало бы у под-FB две разных переменных с одним
                // именем - то есть тихо разорвало бы связь с корнем.
                if extras.shared.iter().any(|(n, _)| n == name) {
                    continue;
                }
                // Столкновение имён внутри POU - `ST-025`. Проверка стоит здесь, после
                // фильтра использования и после разделяемых: разделяемая уже объявлена
                // в `VAR_IN_OUT` этим же именем, и проверка выше дала бы ложный отказ
                // на ней самой.
                check_st_local_clash(name, &occupied, *loc)?;
                occupied.push(name.clone());
                locals.push(declaration(
                    name,
                    ty,
                    expr,
                    model,
                    array_owner,
                    named_arrays,
                    &extras.array_forms,
                )?);
            }
            VariableNode::Port {
                name,
                ty,
                direction,
                loc,
                init,
                ..
            } => {
                if !usage.ports.contains(name) {
                    continue;
                }
                check_st_declaration(name, model, *loc)?;
                if extras.shared.iter().any(|(n, _)| n == name) {
                    continue;
                }
                check_st_local_clash(name, &occupied, *loc)?;
                occupied.push(name.clone());
                // Начальное значение порта - инициализатор объявления `VAR_OUTPUT`:
                // экземпляр `FUNCTION_BLOCK` получает его при создании, то есть до
                // первого вызова. Это и есть "до первого такта"; запасного пути -
                // записи первым сканом - не нужно: `iec2c` принимает инициализатор и на
                // выходе блока, и на размещённой глобальной переменной.
                //
                // Входной порт значения не получает (`SE-092` его отвергает), а в цели
                // `st-at` порт виден блоку через `VAR_EXTERNAL`, где инициализатор
                // недопустим по стандарту: значение там ставится на `VAR_GLOBAL`
                // (`st/mod.rs::emit_configuration`).
                let mut decl = declaration(name, ty, init, model, None, &[], &extras.array_forms)?;
                // Цель `st-at`: порт - размещённая глобальная переменная (`VAR_GLOBAL
                // ... AT %...` внутри `CONFIGURATION`), и блок видит её через
                // `VAR_EXTERNAL`. Цель `st` адрес не потребляет: порт остаётся
                // входом/выходом блока.
                if extras.external_ports {
                    decl.init = None;
                    externals.push(decl);
                    continue;
                }
                match direction {
                    PortDirection::In => inputs.push(decl),
                    PortDirection::Out => outputs.push(decl),
                    PortDirection::InOut => in_outs.push(decl),
                }
            }
            VariableNode::Const {
                upper,
                name,
                ty,
                expr,
                loc,
            } => {
                // Ключ - пара (владелец, имя),: голым именем константа модели-тёзки
                // считалась бы использованной здесь.
                if !usage
                    .constants
                    .contains(&crate::semantic::unused::const_key(upper.as_ref(), name))
                {
                    continue;
                }
                check_st_declaration(name, model, *loc)?;
                check_st_local_clash(name, &occupied, *loc)?;
                occupied.push(name.clone());
                constants.push(declaration(
                    name,
                    ty,
                    expr,
                    model,
                    None,
                    &[],
                    &extras.array_forms,
                )?);
            }
        }
    }

    // Константы предков: FB в IEC замкнут и области видимости Takt не наследует.
    for (name, var) in inherited_constants(model, usage) {
        let VariableNode::Const { ty, expr, .. } = &var else {
            continue;
        };
        constants.push(declaration(
            &name,
            ty,
            expr,
            model,
            None,
            &[],
            &extras.array_forms,
        )?);
    }

    // Анонимные ячейки: в цели `st-at` они объявлены глобально с
    // локацией, а блок видит их через `VAR_EXTERNAL` - как и порты. Собираются
    // **только свои**: у под-модели свой блок и свой список.
    if extras.external_ports {
        for cell in crate::semantic::collect_anon_ports_local_node(model) {
            externals.push(Declaration {
                name: cell.synthetic_name(),
                ty: get_st_type(&cell.ty, model)?,
                init: None,
            });
        }
    }

    for (name, ty, init) in &extras.table_constants {
        constants.push(Declaration {
            name: name.clone(),
            ty: ty.clone(),
            init: Some(init.clone()),
        });
    }

    // В `VAR_IN_OUT` инициализатора быть не может: секция передаёт ссылку на чужую
    // переменную, и MatIEC отвечает "';' missing at end of variable(s) declaration"
    // . Значение появлялось само - у перечислимого типа умолчание есть
    // всегда (первый вариант).
    let in_outs: Vec<Declaration> = in_outs
        .into_iter()
        .map(|decl| Declaration { init: None, ..decl })
        .collect();
    let sections = [
        ("VAR_INPUT", inputs),
        ("VAR_OUTPUT", outputs),
        ("VAR_IN_OUT", in_outs),
        ("VAR_EXTERNAL", externals),
        ("VAR", locals),
        ("VAR CONSTANT", constants),
    ];
    let mut printed = false;
    for (keyword, decls) in sections {
        if decls.is_empty() {
            continue;
        }
        printed = true;
        p.ident(keyword).nl();
        p.up();
        for decl in &decls {
            decl.write(p);
        }
        p.down();
        p.ident("END_VAR").nl();
    }
    Ok(printed)
}

/// Строит объявления констант-вариантов перечислений модели (откат Option C).
///
/// Имя константы - `<Перечисление>_<Вариант>`: пространство имён констант в IEC 61131-3
/// плоское, а одноимённые варианты разных перечислений в Takt допустимы.
fn enum_constants(model: &ModelNode) -> Result<Vec<Declaration>, Diagnostic> {
    let mut out = Vec::new();
    // Перечисления собираются с модели И её предков. Причина: в Takt область видимости
    // вложенная (под-модель видит `enum Command` корня), а в IEC 61131-3
    // `FUNCTION_BLOCK` - замкнутая единица: он видит только то, что объявлено в нём
    // самом. Проверка поймал это на `elevator_mini`: под-модель `Motor` пишет `command =
    // Command_Stop`, а константа жила лишь в корне -> "Variable not declared in this
    // scope".
    let enums = visible_enums(model);
    let mut names: Vec<&String> = enums.keys().collect();
    names.sort();
    for enum_name in names {
        let node = &enums[enum_name];
        // Разрядность типа выбрана по фактическому диапазону вариантов
        // (`st_type::enum_type`), поэтому усечения значения здесь быть не может.
        let ty = get_st_type(&TypeNode::Enum(enum_name.clone()), model)?;
        for (variant, value) in &node.variants {
            out.push(Declaration {
                name: format!("{}_{}", enum_name, variant),
                ty: ty.clone(),
                init: Some(value.to_string()),
            });
        }
    }
    Ok(out)
}

/// Собирает перечисления, видимые модели: её собственные плюс предков.
pub(in crate::generator::st) fn visible_enums(
    model: &ModelNode,
) -> std::collections::HashMap<String, crate::semantic::EnumDefinitionNode> {
    let mut out = std::collections::HashMap::new();
    // Свои - в первую очередь: ближняя область видимости перекрывает дальнюю.
    for (k, v) in &model.enums {
        out.insert(k.clone(), v.clone());
    }
    let mut current = model.upper.as_ref().and_then(|w| w.upgrade());
    while let Some(parent_rc) = current {
        let parent = parent_rc.borrow();
        for (k, v) in &parent.enums {
            out.entry(k.clone()).or_insert_with(|| v.clone());
        }
        current = parent.upper.as_ref().and_then(|w| w.upgrade());
    }
    out
}

/// Собирает константы, видимые модели, но объявленные у предков.
///
/// В Takt под-модель видит `const CHARGE_STACK` корня; в IEC - нет. Константа
/// неизменна, поэтому дешевле продублировать её в `VAR CONSTANT` каждого FB, чем плести
/// через `VAR_IN_OUT`.
fn inherited_constants(model: &ModelNode, usage: &UsageSet) -> Vec<(String, VariableNode)> {
    let mut out: Vec<(String, VariableNode)> = Vec::new();
    let mut current = model.upper.as_ref().and_then(|w| w.upgrade());
    while let Some(parent_rc) = current {
        let parent = parent_rc.borrow();
        let mut names: Vec<&String> = parent.variables.keys().collect();
        names.sort();
        for name in names {
            let var = &parent.variables[name];
            let VariableNode::Const { upper, .. } = var else {
                continue;
            };
            // Ключ - пара (владелец, имя),: владельцем здесь выступает предок, чью
            // константу мы наследуем, а не модель, которую печатаем.
            if !usage
                .constants
                .contains(&crate::semantic::unused::const_key(upper.as_ref(), name))
            {
                continue;
            }
            if model.variables.contains_key(name) || out.iter().any(|(n, _)| n == name) {
                continue;
            }
            out.push((name.clone(), var.clone()));
        }
        current = parent.upper.as_ref().and_then(|w| w.upgrade());
    }
    out
}

/// Строит одно объявление: имя, тип IEC и - если он литерал - инициализатор.
fn declaration(
    name: &str,
    ty: &TypeNode,
    expr: &ExpressionNode,
    model: &ModelNode,
    array_owner: Option<&str>,
    named_arrays: &[String],
    array_forms: &[String],
) -> Result<Declaration, Diagnostic> {
    // Массив, разделяемый через `VAR_IN_OUT`, объявляется именованным типом - и у
    // владельца тоже. Именованным должен быть **и параметр, и сама переменная**: MatIEC
    // сверяет типы, и `mem : ARRAY [...]` против `mem : Root_mem_arr` для него
    // по-прежнему несовместимы.
    let ty_text = match array_owner {
        Some(owner) if named_arrays.iter().any(|n| n == name) => {
            st_type::shared_array_type_name(owner, name)
        }
        // Массив, чья форма встречается в параметре функции, объявляется той же формой:
        // типы аргумента и параметра обязаны совпадать, и анонимный `ARRAY [...]`
        // против именованного MatIEC не принимает.
        _ => match st_type::array_form_name(ty, model) {
            Some(form) if array_forms.contains(&form) => form,
            _ => get_st_type(ty, model)?,
        },
    };
    Ok(Declaration {
        name: name.to_string(),
        ty: ty_text,
        init: literal_init(expr, ty, Some(model)),
    })
}

/// Инициализатор структуры в форме IEC: `(поле := значение, ...)`.
///
/// `None`, если структура не объявлена, число значений не совпало с числом полей либо
/// значение поля не литерал: тогда объявление остаётся без инициализатора - прежнее
/// поведение.
///
/// Порядок берётся у **объявления** структуры (`Vec` полей): инициализатор языка
/// позиционный, и вторая раскладка разошлась бы с эталоном.
fn struct_init(items: &[ExpressionNode], name: &str, model: &ModelNode) -> Option<String> {
    let def = model.search_struct(name)?;
    if def.fields.len() != items.len() {
        return None;
    }
    let mut parts = Vec::with_capacity(items.len());
    for ((field, field_ty), value) in def.fields.iter().zip(items) {
        // Поле-Массив из агрегата исключается: `iec2c` не принимает `(data := [1, 2,
        // 3], n := 0)` - "Initialization element identifier (data) is not declared in
        // referenced structure/FB scope". Значение такого поля кладут операторы первого
        // скана, как у массива структур. Частичный инициализатор законен - проверено
        // пробой.
        if field_is_deferred(field_ty) {
            continue;
        }
        let printed = literal_init(value, field_ty, Some(model))?;
        parts.push(format!("{field} := {printed}"));
    }
    // Все поля отложены - инициализатора у объявления нет вовсе.
    if parts.is_empty() {
        return None;
    }
    Some(format!("({})", parts.join(", ")))
}

/// Поле структуры, которое объявление IEC выразить не может.
///
/// `[bit;N<=64]` сюда не входит: это упакованный скаляр, и печатается он числом -
/// признак берётся из того же слоя, что и печать типа.
///
/// Поле-Структура отложено по той же причине, что поле-массив: `conf :
/// Outer := (head := (mode := 0, hold := 2000));` `iec2c` отвергает - "Initialization
/// element identifier (mode) is not declared in referenced structure/FB scope" - при
/// Нулевом коде возврата `taktc`, тогда как эталон и остальные семь потребителей вход
/// исполняют.
pub(crate) fn field_is_deferred(ty: &TypeNode) -> bool {
    match ty {
        TypeNode::Array(..) => crate::semantic::bit_vector::is_bit_vector(ty).is_none(),
        TypeNode::Struct(_) => true,
        _ => false,
    }
}

/// Возвращает инициализатор, если выражение - литерал, а тип - скалярный.
///
/// Переводятся только литералы: трансляция произвольных выражений. Пропуск
/// нелитерального инициализатора **безопасен**: переменная объявляется без него и
/// получает нулевое значение по умолчанию IEC, а не исчезает. Полную форму (включая
/// вычислимые инициализаторы) даёт.
///
/// **Составные типы инициализатор не получают.** Takt разрешает скалярный `0` для
/// массива (`var data: [u8; 4] := 0;` - так объявлены переменные корпуса), но в
/// IEC это ошибка: `iec2c` на `ARRAY [0..3] OF USINT := 0` отвечает "invalid
/// initial value in array specification with initialization". Агрегатная форма
/// (`:= [0, 0, 0, 0]`) - вместе с остальными выражениями; до неё
/// массив объявляется без инициализатора и обнуляется правилами IEC по
/// умолчанию, что совпадает с намерением `:= 0`.
pub(crate) fn literal_init(
    expr: &ExpressionNode,
    ty: &TypeNode,
    model: Option<&ModelNode>,
) -> Option<String> {
    // Агрегат структуры: `var g: Gains := {2, 3};` печатается именованной формой IEC -
    // `(kp := 2, ki := 3)`.
    if let (TypeNode::Struct(struct_name), Some(owner)) = (ty, model)
        && let ExpressionNode::Initializer(items) | ExpressionNode::Array(items) = expr
    {
        return struct_init(items, struct_name, owner);
    }
    // `[bit;N<=64]` составным типом не является: по это упакованный скаляр, и
    // `get_st_type` печатает его как `USINT`/`UINT`/`UDINT`/`ULINT`. Признак берётся из
    // того же слоя, что и печать типа, - второе правило упаковки разъехалось бы с
    // первым и дало значение не той ширины.
    //
    // До проверка ниже глушил и его: `var small: [bit;8] := 255;` объявлялся `small :
    // USINT;` - без значения. Эталон и цель `c` давали 255, цель `st` - 0, и
    // расхождение было молчаливым: `iec2c` вывод принимает.
    let packed_bits = crate::semantic::bit_vector::is_bit_vector(ty).is_some_and(|nbits| {
        matches!(
            crate::semantic::bit_vector::layout(nbits),
            crate::semantic::bit_vector::BitVectorLayout::Scalar { .. }
        )
    });
    // Агрегат массива: `var a: [u8;2] := {1, 2};` печатается формой `[1, 2]` - проба
    // `iec2c` 2026-08-20 её принимает.
    //
    // Массив структур сюда не подпадает: ни одна из трёх проверенных форм (`[(1, 2),
    // ...]`, `[(v := 1, ...), ...]`, `((v := 1, ...), ...)`) `iec2c` не принимается -
    // такой инициализатор печатается операторами первого скана.
    if let TypeNode::Array(_, elem) = ty
        && !matches!(**elem, TypeNode::Struct(_))
        && let ExpressionNode::Initializer(items) | ExpressionNode::Array(items) = expr
    {
        return Some(format!(
            "[{}]",
            super::st_multidim::flat_array_items(items, elem, model)?.join(", ")
        ));
    }
    if !packed_bits && matches!(ty, TypeNode::Array(_, _) | TypeNode::Struct(_)) {
        return None;
    }
    // Переменная перечислимого типа без инициализатора получает первый по тексту
    // вариант. Правило одно на эталон и три цели: носитель `semantic::enum_default`.
    if let (TypeNode::Enum(enum_name), ExpressionNode::None, Some(owner)) = (ty, expr, model)
        && let Some(def) = owner.search_enum(enum_name)
        && let Some((_, value)) = crate::semantic::enum_default(&def.variants)
    {
        return Some(value.to_string());
    }
    match expr {
        // `bit`/`bool` в IEC - `BOOL`: числовой литерал 0/1 ему не присвоить, нужны
        // `FALSE`/`TRUE`.
        ExpressionNode::Number(n) if matches!(ty, TypeNode::Bit | TypeNode::Bool) => {
            Some(if *n == 0 { "FALSE" } else { "TRUE" }.to_string())
        }
        // Булев литерал (`const ENABLED: bool := true;`). Без этой ветви константа
        // теряла инициализатор и `iec2c` отвергал объявление: `VAR CONSTANT` без
        // значения - "invalid specification in variable declaration".
        ExpressionNode::Bool(b) => Some(if *b { "TRUE" } else { "FALSE" }.to_string()),
        ExpressionNode::Number(n) => Some(n.to_string()),
        // Длительность - целое в миллисекундах, как и её тип (`UDINT`). Без этой ветви
        // инициализатор терялся **молча**: `var pause: duration := 1s;` объявлялся
        // нулём, тогда как эталон давал 1000 мс, - расхождение, которое не увидел бы ни
        // `iec2c`, ни проверка.
        ExpressionNode::Duration(nanos) => crate::semantic::duration::value_millis(
            *nanos,
            crate::diagnostics::Location::Codegen,
            "инициализатор длительности",
        )
        .ok()
        .map(|millis| millis.to_string()),
        ExpressionNode::Rational(text, negative) => {
            Some(format!("{}{}", if *negative { "-" } else { "" }, text))
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic::tree::construct_model;

    /// Печатает секции объявлений корневой модели исходника.
    fn declarations_of(src: &str) -> String {
        let (ast, _) = crate::parse(src, 0).unwrap();
        let rc = construct_model(&ast, None, &[]).unwrap();
        let usage = crate::semantic::unused::compute_usage(std::rc::Rc::clone(&rc));
        let model = rc.borrow();
        let mut out = String::new();
        let mut p = Printer::new(4, &mut out);
        emit_declarations(&mut p, &model, &usage, &Extras::default())
            .expect("объявления должны печататься");
        out
    }

    /// Используемая переменная-массив объявляется настоящим `ARRAY`.
    ///
    /// **Прямой контрпример дефекту Д1b **: на этом же входе цель `c`
    /// даёт `uint4_t` (несуществующий тип) - размерность теряется целиком.
    #[test]
    fn test_emit_declarations_array_variable_is_declared_not_lost() {
        let src = "var data: [u8; 4] := 0;\ncond C = data[0] = 1;\nstart S { ref Done: C; }\nstate Done {}";
        let st = declarations_of(src);
        assert!(
            st.contains("data : ARRAY [0..3] OF USINT"),
            "переменная-массив обязана быть объявлена:\n{st}"
        );
    }

    /// Скалярный инициализатор массива не переносится в ST.
    ///
    /// В Takt `var data: [u8; 4] := 0;` - обычная форма (так объявлен весь корпус), но
    /// `iec2c` отвергает `ARRAY [0..3] OF USINT := 0` ("invalid initial value in array
    /// specification with initialization"). Тест против возврата: без него вывод
    /// невалиден, а тест на присутствие `data` этого не ловит.
    #[test]
    fn test_emit_declarations_array_gets_no_scalar_initializer() {
        let src = "var data: [u8; 4] := 0;\ncond C = data[0] = 1;\nstart S { ref Done: C; }\nstate Done {}";
        let st = declarations_of(src);
        assert!(
            st.contains("data : ARRAY [0..3] OF USINT;"),
            "у массива не должно быть скалярного инициализатора:\n{st}"
        );
    }

    /// Входные и выходные порты попадают в разные секции.
    #[test]
    fn test_emit_declarations_ports_split_by_direction() {
        let src = "in btn: bit at 0x100:0;\nout lamp: bit at 0x200:0;\nstart S { always { lamp := btn; } }";
        let st = declarations_of(src);
        let inputs = st.find("VAR_INPUT").expect("нет VAR_INPUT");
        let outputs = st.find("VAR_OUTPUT").expect("нет VAR_OUTPUT");
        assert!(
            st[inputs..outputs].contains("btn : BOOL;"),
            "btn не входной:\n{st}"
        );
        assert!(
            st[outputs..].contains("lamp : BOOL;"),
            "lamp не выходной:\n{st}"
        );
    }

    /// Каждая открытая секция закрыта `END_VAR`.
    #[test]
    fn test_emit_declarations_every_section_is_closed() {
        let src = "in btn: bit at 0x100:0;\nout lamp: bit at 0x200:0;\nvar n: u8 := 0;\nstart S { always { lamp := btn; n := n + 1; } }";
        let st = declarations_of(src);
        assert_eq!(
            st.matches("END_VAR").count(),
            3,
            "ожидались VAR_INPUT, VAR_OUTPUT и VAR:\n{st}"
        );
    }

    /// Варианты перечисления становятся именованными константами.
    ///
    /// Значения - из зонда по `examples/elevator.takt:117`: `Floor { Bottom = 80, Top
    /// }` даёт `[("Bottom", 80), ("Top", 81)]`.
    #[test]
    fn test_emit_declarations_enum_variants_become_named_constants() {
        let src =
            "enum Floor { Bottom = 80, Top }\nvar f: u8 := 0;\nstart S { always { f := f + 1; } }";
        let st = declarations_of(src);
        assert!(st.contains("VAR CONSTANT"), "нет секции констант:\n{st}");
        assert!(
            st.contains("Floor_Bottom : USINT := 80;"),
            "нет константы Bottom:\n{st}"
        );
        assert!(
            st.contains("Floor_Top : USINT := 81;"),
            "Top обязан наследовать 81:\n{st}"
        );
    }

    /// Перечисление предка объявляется в под-модели: FB в IEC замкнут.
    ///
    /// Тест против регресса, который поймал проверка, а юнит-тесты - нет: `elevator_mini`
    /// пишет в под-модели `command = Command_Stop`, а `enum Command` объявлен в корне.
    /// В Takt область видимости вложенная, в IEC 61131-3 - нет: `FUNCTION_BLOCK` видит
    /// только объявленное в нём самом.
    #[test]
    fn test_enum_of_ancestor_is_declared_in_submodel_block() {
        let src = "enum Command { Up, Stop }\n\
                   model Motor {\n\
                     var c: u8 := 0;\n\
                     start S { always { c := Stop; } }\n\
                   }\n\
                   start Main = Motor;";
        let (ast, _) = crate::parse(src, 0).unwrap();
        let rc = construct_model(&ast, None, &[]).unwrap();
        let usage = crate::semantic::unused::compute_usage(std::rc::Rc::clone(&rc));
        let sub = rc.borrow().models.get("Motor").cloned();
        let Some(sub) = sub else {
            panic!("под-модель Motor не найдена");
        };
        let model = sub.borrow();
        let mut out = String::new();
        let mut p = Printer::new(4, &mut out);
        emit_declarations(&mut p, &model, &usage, &Extras::default()).unwrap();
        assert!(
            out.contains("Command_Stop"),
            "перечисление корня обязано объявляться в под-модели:\n{out}"
        );
    }

    /// Перечисление шире байта не усекается - тип константы расширяется.
    ///
    /// Вход из `examples/elevator.takt:121`: `Action { Idle = 670, Closing }`.
    #[test]
    fn test_emit_declarations_wide_enum_constant_is_not_truncated() {
        let src = "enum Action { Idle = 670, Closing }\nvar a: u8 := 0;\nstart S { always { a := a + 1; } }";
        let st = declarations_of(src);
        assert!(
            st.contains("Action_Idle : UINT := 670;"),
            "670 не помещается в USINT — константа обязана быть шире:\n{st}"
        );
    }

    /// Литеральный инициализатор `bit`-переменной - `FALSE`/`TRUE`, не 0/1: числовой
    /// литерал в IEC несовместим с `BOOL`.
    #[test]
    fn test_emit_declarations_bool_initializer_is_keyword_not_number() {
        let src = "var flag: bit := 1;\nstart S { always { flag := flag; } }";
        let st = declarations_of(src);
        assert!(
            st.contains("flag : BOOL := TRUE;"),
            "инициализатор BOOL обязан быть TRUE/FALSE:\n{st}"
        );
    }

    /// Неиспользуемая переменная не объявляется - как в цели `c`.
    ///
    /// Это не дефект Д1b: о неиспользуемом имени сообщает семантика (Ce13), диагностика
    /// есть. Тест закрепляет намеренность поведения.
    #[test]
    fn test_emit_declarations_unused_variable_is_filtered_like_c_target() {
        let src =
            "var used: u8 := 0;\nvar unused: u8 := 0;\nstart S { always { used := used + 1; } }";
        let st = declarations_of(src);
        assert!(
            st.contains("used : USINT"),
            "используемая обязана быть:\n{st}"
        );
        assert!(
            !st.contains("unused :"),
            "неиспользуемая фильтруется (паритет с целью c):\n{st}"
        );
    }

    /// Модель без объявлений сообщает об этом вызывающему.
    ///
    /// `iec2c` отвергает `FUNCTION_BLOCK` без объявлений и тела, поэтому пустота
    /// обязана быть видна снаружи, а не "пустой строкой".
    #[test]
    fn test_emit_declarations_reports_empty_model() {
        let (ast, _) = crate::parse("start S;", 0).unwrap();
        let rc = construct_model(&ast, None, &[]).unwrap();
        let usage = crate::semantic::unused::compute_usage(std::rc::Rc::clone(&rc));
        let model = rc.borrow();
        let mut out = String::new();
        let mut p = Printer::new(4, &mut out);
        let printed = emit_declarations(&mut p, &model, &usage, &Extras::default()).unwrap();
        assert!(!printed, "модель без переменных не имеет секций");
        assert!(
            out.is_empty(),
            "пустая модель не должна печатать секции:\n{out}"
        );
    }

    /// Неотображаемый тип используемой переменной обязан завалить генерацию, а не убрать
    /// переменную из вывода.
    ///
    /// Тип портится после разбора: исходника, дающего `Unsupported` у используемой
    /// переменной, в языке нет - узел служебный.
    #[test]
    fn test_emit_declarations_unmappable_type_is_error_not_silent_skip() {
        let (ast, _) = crate::parse(
            "var bad: u8 := 0;\nstart S { always { bad := bad + 1; } }",
            0,
        )
        .unwrap();
        let rc = construct_model(&ast, None, &[]).unwrap();
        let usage = crate::semantic::unused::compute_usage(std::rc::Rc::clone(&rc));
        assert!(
            usage.variables.contains("bad"),
            "переменная обязана считаться используемой — иначе тест проверял бы фильтр"
        );
        if let Some(VariableNode::Simple { ty, .. }) = rc.borrow_mut().variables.get_mut("bad") {
            *ty = TypeNode::Unsupported;
        }
        let model = rc.borrow();
        let mut out = String::new();
        let mut p = Printer::new(4, &mut out);
        let err = emit_declarations(&mut p, &model, &usage, &Extras::default())
            .expect_err("ожидалась диагностика");
        assert_eq!(err.code.as_deref(), Some("ST-002"));
    }
}
