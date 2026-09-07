//! Объявления констант и восстановление enum-литералов цели `sv` (вынос из `sv_fsm` по
//! лимиту размера - /0134). Ответственность узкая: `localparam`-константы модуля и
//! печать значений, пригодных для цепи сброса (`enter` стартового состояния,
//! диагностика `SV-008`).

use crate::diagnostics::{Diagnostic, Location};
use crate::generator::indent::Printer;
use crate::generator::sv::sv_expr::Scope;
use crate::generator::sv::sv_expr::sv002;
use crate::generator::sv::sv_fsm::Block;
use crate::generator::sv::sv_map::SvMap;
use crate::generator::sv::sv_module::check_sv_name;
use crate::generator::sv::sv_names::sv_enum_variant_name;
use crate::generator::sv::sv_type::sv_type;
use crate::semantic::type_node::TypeNode;
use crate::semantic::{ExpressionNode, StatementNode, VariableNode};
use std::collections::{BTreeMap, BTreeSet};

/// Строит диагностику `SV-008` - `enter` стартового состояния неконстантен.
fn sv008(state: &str, loc: Location) -> Diagnostic {
    Diagnostic::error(
        loc,
        format!(
            "блок 'enter' стартового состояния '{}' вычисляет значение, а не \
             присваивает константу. Целью 'sv' стартовое состояние помещается в \
             ветвь сброса (синтетического INIT-состояния в RTL нет), а ветвь \
             сброса синтезируется в цепь сброса триггеров и выражений не \
             вычисляет — вычислять там нечем. Перенесите вычисление в блок \
             'always' того же состояния: вход в стартовое состояние такта не \
             расходует (контракт ADR 0033), поэтому поведение не изменится",
            state
        ),
    )
    .with_code("SV-008")
}

/// Возвращает тип переменной, которой присваивают.
fn target_var_type(var: &std::rc::Rc<std::cell::RefCell<VariableNode>>) -> Option<TypeNode> {
    match &*var.borrow() {
        VariableNode::Simple { ty, .. }
        | VariableNode::Port { ty, .. }
        | VariableNode::Const { ty, .. } => Some(ty.clone()),
        VariableNode::Unresolved => None,
    }
}

/// Восстанавливает имя варианта перечисления по его значению.
///
/// Возвращает `None`, если тип не перечисление либо варианта с таким значением нет -
/// тогда печатается само число.
///
/// **Зачем.** Узла варианта перечисления `ExpressionNode` не имеет вовсе:
/// `command := Up` приходит как `Number(2)` (та же ловушка описана для цели
/// `rust` в `CLAUDE.md`). Перечисления SystemVerilog при этом **строго
/// типизированы** - проба 2026-07-16: `command <= 2;` даёт
/// `%Error-ENUMVALUE: Implicit conversion to enum`.
pub(crate) fn enum_literal(
    ty: &TypeNode,
    value: i128,
    enums: &BTreeMap<String, Vec<(String, i128)>>,
) -> Option<String> {
    let TypeNode::Enum(enum_name) = ty else {
        return None;
    };
    let variants = enums.get(enum_name)?;
    let (variant, _) = variants.iter().find(|(_, v)| *v == value)?;
    Some(sv_enum_variant_name(enum_name, variant))
}

/// Печатает значение, пригодное для **цепи сброса**.
///
/// "Константа" здесь - не только литерал. Именованная константа (`const CHARGE_STACK:
/// u8 := 0;`) становится `localparam`, то есть значением времени компиляции, и цепь
/// сброса выражает её так же свободно, как число; то же касается варианта перечисления.
///
/// Первая редакция этой проверки принимала **только** литералы и отвергала `enter {
/// cmd_target_stack := CHARGE_STACK; }` (`stacker.takt:214`) - то есть **флагманский
/// пример цели**. Карточка фичи утверждала, что цена `SV-008` для корпуса нулевая, и
/// была права: ошибалась проверка, а не модель.
///
/// # Ошибки
/// [`SV-008`](sv008), если значение вычисляется, а не известно на этапе
/// компиляции.
fn constant_value(
    value: &ExpressionNode,
    state: &str,
    loc: Location,
) -> Result<String, Diagnostic> {
    match value {
        ExpressionNode::Number(n) => Ok(n.to_string()),
        ExpressionNode::Bool(b) => Ok(if *b { "1'b1" } else { "1'b0" }.to_string()),
        // Литерал длительности - такая же константа, как число: печатается
        // миллисекундами, потому что тип `duration` в целях есть беззнаковый вектор
        // миллисекунд.
        ExpressionNode::Duration(nanos) => {
            crate::semantic::duration::value_millis(*nanos, loc, "инициализатор длительности")
                .map(|millis| millis.to_string())
        }
        // Константа модели - `localparam`. Переменная и порт сюда не проходят: их
        // значение к моменту сброса не определено.
        ExpressionNode::Variable(var) => match &*var.borrow() {
            // Имя печатается тем же правилом, что и в выражениях
            // (`sv_expr::const_signal`): константа-параметр несёт префикс владельца,
            // иначе ветвь сброса ссылалась бы на localparam соседа.
            VariableNode::Const { upper, name, .. } => Ok(
                crate::generator::sv::sv_names::const_signal(upper.as_ref(), name),
            ),
            _ => Err(sv008(state, loc)),
        },
        _ => Err(sv008(state, loc)),
    }
}

/// Проверяет, что блок `enter` стартового состояния присваивает только константы.
///
/// Проверка **консервативна**: всё, что не является присваиванием литерала в
/// переменную, считается вычислением. Ошибка в эту сторону даёт понятный отказ
/// (`SV-008` с обходом в тексте), ошибка в другую - несинтезируемый модуль.
pub(crate) fn constant_enter_assignments(
    stmt: &StatementNode,
    state: &str,
    loc: Location,
    scope: &Scope,
    out: &mut Vec<(String, String)>,
) -> Result<(), Diagnostic> {
    match stmt {
        StatementNode::None => Ok(()),
        StatementNode::Block(stmts) => {
            for s in stmts {
                constant_enter_assignments(s, state, loc, scope, out)?;
            }
            Ok(())
        }
        // Формула - свойство для верификации, а не поведение: цель `c` её тоже не
        // эмитит (`taktc verify`).
        StatementNode::InlineFormula(_) => Ok(()),
        StatementNode::Expression(expr, _) => match &**expr {
            ExpressionNode::Assign(target, value) => {
                let ExpressionNode::Variable(var) = &**target else {
                    return Err(sv008(state, loc));
                };
                let name = crate::generator::sv::sv_names::signal_of(var)
                    .ok_or_else(|| sv008(state, loc))?;
                // Значение печатается по типу цели: для перечисления число
                // восстанавливается в имя варианта, иначе ветвь сброса даст
                // `%Error-ENUMVALUE` (перечисления SV строго типизированы).
                let printed = match (target_var_type(var), &**value) {
                    (Some(ty), ExpressionNode::Number(n)) => enum_literal(&ty, *n, scope.enums)
                        .map(Ok)
                        .unwrap_or_else(|| constant_value(value, state, loc))?,
                    _ => constant_value(value, state, loc)?,
                };
                out.push((name, printed));
                Ok(())
            }
            _ => Err(sv008(state, loc)),
        },
        _ => Err(sv008(state, loc)),
    }
}

/// Печатает `localparam`-константы модуля (по всем уровням), пригодные для синтеза:
/// значение обязано быть известно на этапе компиляции. Печатает `localparam`-константы
/// и возвращает сбросы констант-массивов.
///
/// Массив-константа `localparam`ом быть не может: `localparam logic [7:0] X [0:1] =
/// '{...}` не принимает **yosys** ("syntax error, unexpected '['"), хотя verilator
/// принимает. Форма, которую принимают оба, - сигнал, заполняемый в цепи
/// сброса: значение постоянно, и синтезатор сводит его к константам сам. Пары `(имя,
/// значение)` уезжают в [`emit_ff`], где стоит эта цепь.
///
/// [`emit_ff`]: crate::generator::sv::sv_fsm::emit_ff
pub(crate) fn emit_constants(
    p: &mut Printer,
    map: &SvMap,
    blocks: &[Block],
) -> Result<Vec<(String, String)>, Diagnostic> {
    let mut array_resets: Vec<(String, String)> = Vec::new();
    let mut seen: BTreeSet<String> = BTreeSet::new();
    let mut emitted = false;
    for (_, model_rc) in blocks {
        let model = model_rc.borrow();
        for var in model.variables.values() {
            let VariableNode::Const {
                upper,
                name,
                ty,
                expr,
                loc,
            } = var
            else {
                continue;
            };
            // Объявление печатается тем именем, которым к нему обращаются выражения:
            // оно квалифицировано владельцем (`sv_expr::const_signal`). Ключ
            // дедупликации - **это** имя: по голому две модели с одноимённой константой
            // слились бы в одно объявление, и вторая молча получила бы значение первой.
            // Ключ фильтра "используется" - та же пара (владелец, имя),: голым именем
            // неиспользуемая тёзка проходила бы вслед за используемой.
            let signal = crate::generator::sv::sv_names::const_signal(upper.as_ref(), name);
            let used = crate::semantic::unused::const_key(upper.as_ref(), name);
            if !map.usage().constants.contains(&used) || !seen.insert(signal.clone()) {
                continue;
            }
            // Константа перечислимого типа объявления не получает: типизированный
            // `localparam mode_e X` не принимает **yosys**, а нетипизированный -
            // verilator (приёмник строго типизирован). Обращения к ней печатаются
            // именем варианта - `sv_names`.
            if crate::generator::sv::sv_names::enum_constant_literal(upper.as_ref(), ty, expr)
                .is_some()
            {
                continue;
            }
            check_sv_name(&signal, *loc)?;
            let decl = sv_type(ty, &format!("константа '{}'", name))?;
            // Значение печатается тем же носителем, что и цепь сброса: он знает
            // агрегаты - структуру печатает именованным литералом, массив шаблоном
            // присваивания.
            let value = reset_value(
                expr,
                ty,
                &enums_of(blocks),
                &format!("константы '{name}'"),
                *loc,
                Some(model_rc),
            )
            .map_err(|_| {
                sv002(&format!(
                    "инициализатор константы '{}': значение обязано быть известно \
                     на этапе компиляции — localparam вычисляется синтезатором, \
                     а не схемой",
                    name
                ))
            })?;
            // Массив - сигналом (см. шапку функции): `localparam` такого вида не
            // синтезируется. Значение уходит в цепь сброса.
            //
            // Бит-вектор `[bit;N<=64]` - упакованный скаляр, ему `localparam` верен и
            // трогать его нельзя.
            let array_const = matches!(ty, TypeNode::Array(..))
                && crate::semantic::bit_vector::is_bit_vector(ty).is_none();
            if array_const {
                p.ident(&format!("{};", decl.declare(&signal))).nl();
                array_resets.push((signal.clone(), value));
            } else {
                p.ident(&format!(
                    "localparam {} = {};",
                    decl.declare(&signal),
                    value
                ))
                .nl();
            }
            emitted = true;
        }
    }
    if emitted {
        p.nl();
    }
    Ok(array_resets)
}

/// Отказ `SV-002`: инициализатор в цепи сброса невычислим.
pub(in crate::generator::sv) fn unresolvable_reset(what: &str) -> Diagnostic {
    sv002(&format!(
        "инициализатор {}: ветвь сброса синтезируется в цепь сброса триггеров и \
         выражений не вычисляет — допустима константа либо выражение, значение \
         которого известно при компиляции",
        what
    ))
}

/// Значение сигнала в ветви сброса по инициализирующему выражению.
///
/// Единая точка для **переменной модели** и для **начального значения выходного
/// порта**: и то и другое ложится в одну и ту же цепь сброса, поэтому правила печати
/// литерала обязаны совпадать. Разъехавшись, они дали бы порту и переменной разное
/// значение при одном и том же тексте инициализатора - расхождение, которое ни
/// verilator, ни yosys не заметят.
///
/// `what` - родительный падеж описания места (`переменной 'x'`, `порта 'led'`):
/// подставляется в диагностику `SV-002`.
///
/// # Ошибки
/// [`SV-002`](sv002) - выражение не является константой: ветвь сброса
/// синтезируется в цепь сброса триггеров и выражений не вычисляет.
pub(in crate::generator::sv) fn reset_value(
    expr: &ExpressionNode,
    ty: &TypeNode,
    enums: &BTreeMap<String, Vec<(String, i128)>>,
    what: &str,
    loc: Location,
    scope: Option<&std::rc::Rc<std::cell::RefCell<crate::semantic::ModelNode>>>,
) -> Result<String, Diagnostic> {
    Ok(match expr {
        // Значение перечисления приходит числом (`command := Up` - это `Number(2)`), а
        // перечисления SV строго типизированы: без восстановления варианта ветвь сброса
        // дала бы `%Error-ENUMVALUE`. Та же ловушка описана для цели `rust`.
        ExpressionNode::Number(n) => enum_literal(ty, *n, enums)
            // Широкое значение сброса - размерной формой по ширине регистра: голое
            // десятичное больше `i32::MAX` даёт `WIDTHEXPAND`.
            .or_else(|| crate::generator::sv::sv_type::sized_literal(*n, ty))
            .unwrap_or_else(|| n.to_string()),
        ExpressionNode::Bool(b) => if *b { "1'b1" } else { "1'b0" }.to_string(),
        // Литерал длительности - константа в **миллисекундах**: тип `duration` в целях
        // есть беззнаковый вектор миллисекунд, поэтому и значение сброса такое же.
        ExpressionNode::Duration(nanos) => {
            crate::semantic::duration::value_millis(*nanos, loc, &format!("инициализатор {what}"))?
                .to_string()
        }
        // Умолчание без инициализатора: регистр обязан иметь значение сброса -
        // "неинициализированного" триггера не бывает.
        //
        // Распакованному массиву `'0` не присваивается: verilator отвечает ошибкой
        // "CONST is not an unpacked array". Форма сброса у него - агрегат нулей, тот
        // же, каким печатается инициализатор; носитель один.
        ExpressionNode::None => {
            // Перечисление сбрасывается своим значением, а не `'0`: перечисления SV
            // строго типизированы, и `m <= '0;` над регистром типа `mode_e` verilator
            // встречает **ошибкой** `%Error-ENUMVALUE` ("Implicit conversion to enum
            // ... from 'bit'"), а проверка цели считает предупреждение ошибкой. Значение
            // остаётся нулём - тем же, что даёт эталон переменной без инициализатора;
            // меняется только форма записи.
            if let TypeNode::Enum(enum_name) = ty {
                // Умолчание перечисления - Первый по тексту вариант: ноль может не
                // принадлежать набору, и тогда регистр стартует со значения, о котором
                // не знает ни один `case`. Правило одно на эталон и три цели - носитель
                // `semantic::enum_default`.
                let default = enums
                    .get(enum_name)
                    .and_then(|variants| crate::semantic::enum_default(variants))
                    .map(|(variant, _)| sv_enum_variant_name(enum_name, &variant));
                return Ok(default.unwrap_or_else(|| {
                    // Перечисления без вариантов не бывает (`SE-105`, 0172) - ветвь
                    // защитная, приведение здесь прежнее поведение.
                    format!(
                        "{}'(0)",
                        crate::generator::sv::sv_type::sv_enum_type_name(enum_name)
                    )
                }));
            }
            crate::generator::sv::sv_array::reset_literal(ty).unwrap_or_else(|| "'0".to_string())
        }
        // Агрегат структуры: `var g: Gains := {2, 3};`. В цепи сброса печатается
        // упакованным литералом `'{поле: значение, ...}` - именованная форма, потому
        // что позиционная у `struct packed` требует совпадения ширин и молча слипается
        // при ошибке в порядке.
        //
        // Порядок берётся у объявления структуры (`Vec` полей, не карта): инициализатор
        // позиционный, и вторая раскладка разошлась бы с эталоном.
        ExpressionNode::Initializer(items) | ExpressionNode::Array(items)
            if matches!(ty, TypeNode::Struct(_)) =>
        {
            return struct_reset(items, ty, enums, what, loc, scope);
        }
        // Агрегат массива: `var arr: [u8; 3] := {1, 2, 3};`. Печатается **шаблоном
        // присваивания** `'{...}`, а не конкатенацией: массив здесь распакованный
        // (`logic [7:0] a [0:2]`), и `{...}` для него не форма значения, а склейка
        // разрядов.
        //
        // Бит-вектор `[bit;N]` сюда не попадает - он скаляр (`logic [N-1:0]`), и его
        // агрегат ложится обычным литералом.
        ExpressionNode::Initializer(items) | ExpressionNode::Array(items)
            if matches!(ty, TypeNode::Array(_, _))
                && crate::semantic::bit_vector::is_bit_vector(ty).is_none() =>
        {
            return array_reset(items, ty, enums, what, loc, scope);
        }
        // Не литерал - спрашиваем вычислимость у общего слоя, а не судим по виду узла.
        other => {
            let value = scope
                .ok_or_else(|| unresolvable_reset(what))
                .and_then(|model| crate::semantic::const_eval::eval_node_public(other, loc, model))
                .map_err(|_| unresolvable_reset(what))?;
            let crate::semantic::const_eval::ConstValue::Int(n) = value else {
                return Err(unresolvable_reset(what));
            };
            enum_literal(ty, n, enums)
                .or_else(|| crate::generator::sv::sv_type::sized_literal(n, ty))
                .unwrap_or_else(|| n.to_string())
        }
    })
}

/// Значение поля-массива внутри `struct packed` - конкатенацией.
///
/// `None` - поле массивом не является (или это упакованный бит-вектор: он печатается
/// скаляром).
#[allow(clippy::too_many_arguments)]
fn packed_field_array(
    value: &ExpressionNode,
    field_ty: &TypeNode,
    enums: &BTreeMap<String, Vec<(String, i128)>>,
    field: &str,
    struct_name: &str,
    loc: Location,
    scope: Option<&std::rc::Rc<std::cell::RefCell<crate::semantic::ModelNode>>>,
) -> Result<Option<String>, Diagnostic> {
    let TypeNode::Array(_, elem) = field_ty else {
        return Ok(None);
    };
    if crate::semantic::bit_vector::is_bit_vector(field_ty).is_some() {
        return Ok(None);
    }
    let (ExpressionNode::Initializer(items) | ExpressionNode::Array(items)) = value else {
        return Ok(None);
    };
    let mut parts = Vec::with_capacity(items.len());
    for item in items.iter().rev() {
        let printed = reset_value(
            item,
            elem,
            enums,
            &format!("элемента поля '{field}' структуры '{struct_name}'"),
            loc,
            scope,
        )?;
        // Размерная форма обязательна: безразмерное число внутри `{...}` даёт
        // `WIDTHCONCAT`, а проверка цели считает предупреждение ошибкой.
        parts.push(match crate::generator::sv::sv_type::scalar_width(elem) {
            Some(width) if printed.chars().all(|c| c.is_ascii_digit()) => {
                format!("{width}'d{printed}")
            }
            _ => printed,
        });
    }
    Ok(Some(format!("{{{}}}", parts.join(", "))))
}

/// Значение сброса для агрегата структуры.
///
/// `'{kp: 8'd2, ki: 8'd3}` - именованная форма литерала упакованной структуры.
///
/// # Ошибки
/// [`SV-002`](crate::generator::sv::sv_fsm::sv002) - структура не объявлена,
/// число значений не совпадает с числом полей либо значение поля невычислимо.
/// Значение сброса для агрегата **массива**.
///
/// # Ошибки
///
/// [`SV-002`](sv002) - число значений не совпало с объявленным размером либо элемент
/// невычислим.
fn array_reset(
    items: &[ExpressionNode],
    ty: &TypeNode,
    enums: &BTreeMap<String, Vec<(String, i128)>>,
    what: &str,
    loc: Location,
    scope: Option<&std::rc::Rc<std::cell::RefCell<crate::semantic::ModelNode>>>,
) -> Result<String, Diagnostic> {
    let TypeNode::Array(size, elem) = ty else {
        return Err(unresolvable_reset(what));
    };
    if usize::from(*size) != items.len() {
        return Err(sv002(&format!(
            "инициализатор {what}: массив объявлен на {size} элементов, а значений {}",
            items.len()
        )));
    }
    let mut parts = Vec::with_capacity(items.len());
    for (index, value) in items.iter().enumerate() {
        let printed = reset_value(
            value,
            elem,
            enums,
            &format!("элемента {index} массива в {what}"),
            loc,
            scope,
        )?;
        // Размерная форма обязательна, как и в агрегате структуры: безразмерное число в
        // шаблоне присваивания даёт `verilator -Wall` предупреждение о ширине, а проверка
        // цели идёт без исключений.
        let sized = match (value, crate::generator::sv::sv_type::scalar_width(elem)) {
            (ExpressionNode::Number(n), Some(width)) => format!("{width}'d{n}"),
            _ => printed,
        };
        parts.push(sized);
    }
    Ok(format!("'{{{}}}", parts.join(", ")))
}

fn struct_reset(
    items: &[ExpressionNode],
    ty: &TypeNode,
    enums: &BTreeMap<String, Vec<(String, i128)>>,
    what: &str,
    loc: Location,
    scope: Option<&std::rc::Rc<std::cell::RefCell<crate::semantic::ModelNode>>>,
) -> Result<String, Diagnostic> {
    let TypeNode::Struct(name) = ty else {
        return Err(unresolvable_reset(what));
    };
    let model = scope.ok_or_else(|| unresolvable_reset(what))?;
    let def = model
        .borrow()
        .search_struct(name)
        .ok_or_else(|| unresolvable_reset(what))?;
    if def.fields.len() != items.len() {
        return Err(sv002(&format!(
            "инициализатор {what}: структура '{name}' объявляет {} полей, а значений {}",
            def.fields.len(),
            items.len()
        )));
    }
    let mut parts = Vec::with_capacity(items.len());
    for ((field, field_ty), value) in def.fields.iter().zip(items) {
        // Поле-Массив внутри `struct packed` объявлено упакованным, и его значение -
        // конкатенация, а не шаблон `'{...}`: `verilator` отвечает "Assignment pattern
        // member not underneath a supported construct: CONCAT". Порядок обратный: у
        // `logic [N-1:0]` старший разряд слева, то есть первым печатается последний
        // элемент.
        if let Some(text) = packed_field_array(value, field_ty, enums, field, name, loc, scope)? {
            // Форма позиционная, как и у прочих полей (yosys не принимает именованную -
            // проба 0293).
            parts.push(text);
            continue;
        }
        let printed = reset_value(
            value,
            field_ty,
            enums,
            &format!("поля '{field}' структуры '{name}'"),
            loc,
            scope,
        )?;
        // Размерная форма обязательна всегда, а не по нужде (в отличие от печати
        // литерала, 0157): `verilator -Wall` отвечает `WIDTHCONCAT` "Unsized numbers
        // not allowed in concatenations" на любое безразмерное число внутри `{...}`.
        //
        // Литерал длительности (`5ms`) - тоже число: он приходит узлом `Duration`,
        // печатается миллисекундами и прежде оставался безразмерным, из-за чего
        // `verilator` отвечал `WIDTHCONCAT` на структуру с полем `duration`. Признак
        // берётся у **напечатанного** текста: он уже приведён к единицам цели.
        let sized = match crate::generator::sv::sv_type::scalar_width(field_ty) {
            Some(width) if printed.chars().all(|c| c.is_ascii_digit()) => {
                format!("{width}'d{printed}")
            }
            _ => printed,
        };
        parts.push(sized);
    }
    // Форма позиционная, а не именованная (`'{kp: 2}`): именованную `verilator`
    // принимает, а **yosys отвергает** (`syntax error, unexpected ':'`) - проба
    // 2026-08-19. Тот же урок, что у `assert ... else`: форма выбирается по тому, что
    // принимают оба инструмента.
    //
    // Порядок - объявленный: в `struct packed` первое поле занимает старшие разряды, и
    // перестановка молча изменила бы значение.
    Ok(format!("{{{}}}", parts.join(", ")))
}

/// Перечисления всех уровней снимка - для восстановления варианта по значению.
///
/// Собирается тем же обходом, что у `Fsm`: второй способ узнать состав перечислений
/// разошёлся бы с первым.
fn enums_of(blocks: &[Block]) -> BTreeMap<String, Vec<(String, i128)>> {
    let mut out = BTreeMap::new();
    for (_, model_rc) in blocks {
        for def in model_rc.borrow().enums.values() {
            out.entry(def.name.clone())
                .or_insert_with(|| def.variants.clone());
        }
    }
    out
}
