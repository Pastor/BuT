//! Печать функций Takt как `FUNCTION` IEC 61131-3.
//!
//! часть 3. Дополняет `st_expr.rs` (выражения) и `st_stmt.rs` (операторы).
//!
//! ## Возврат - присваивание имени функции
//!
//! В ST нет `return <значение>`: результат возвращается присваиванием **имени
//! функции**, а `RETURN;` лишь досрочно выходит. Поэтому `return a - b;` Takt ->
//! `abs_diff := a - b; RETURN;` (форма проверена пробой на раннем возврате `abs_diff`,
//! `stacker.takt:100-103`).
//!
//! ## Три синтетические подпорки - и почему они неизбежны
//!
//! Пробы MatIEC вскрыли, что **`extern fn` Takt в стандартном ST невыразим сразу по
//! трём осям**. Все три бьют по `elevator.takt:93-115` (восемь `extern fn` вида
//! `motor_up();`). Ограничения - в **стандарте**, а не в инструменте: `iec2c -h` сам
//! называет послабления "a non-standard extension".
//!
//! | Препятствие | Факт | Подпорка |
//! |---|---|---|
//! | Функция без параметров | `-i : allow POUs with no in out and inout parameters (a non-standard extension!)`; пустой `VAR_INPUT` роняет `iec2c` **segfault**'ом | синтетический параметр [`SYNTHETIC_PARAM`]; вызов передаёт `0` |
//! | Функция, возвращающая `VOID` | `-b : allow functions returning VOID (a non-standard extension!)` | синтетический тип `USINT`, тело присваивает `0` |
//! | Вызов функции как оператор | `error: Function invocation in ST code is not allowed outside an expression` | присваивание в переменную-приёмник (`st_stmt`) |
//!
//! Подпорки выбраны так, чтобы вывод остался **стандартным ST**: альтернатива -
//! требовать от `iec2c` флагов `-i`/`-b`, но тогда порождённое перестанет приниматься
//! настоящим ПЛК, ради которого фича и делается.
//!
//! ## `extern fn` -> заглушка + `ST-009`
//!
//! Тело внешней функции неизвестно, а `FUNCTION` без тела `iec2c` отвергает ("no body
//! defined in function declaration", проба П9). Эмитится заглушка, возвращающая
//! нейтральное значение, **плюс предупреждение `ST-009`**: молчание здесь дало бы
//! ПЛК-код, который тихо ничего не делает вместо того, чтобы крутить двигатель, - ровно
//! класс дефекта.

use crate::diagnostics::{Diagnostic, Location};
use crate::generator::indent::Printer;
use crate::generator::st::st_expr::print_expression;
use crate::generator::st::st_expr::unsupported;
use crate::generator::st::st_reserved::check_st_name;

// Списки параметров POU живут рядом: контракт держит реэкспорт - пути потребителей
// (`st_func::state_params`) не меняются.
pub(crate) use crate::generator::st::st_params::{const_params, params_of, state_params};
use crate::generator::st::st_stmt::{Hoisted, StmtOutput, print_statement};
use crate::generator::st::st_type::get_st_type;
use crate::semantic::minimap::Name;
use crate::semantic::type_node::TypeNode;
use crate::semantic::{ExpressionNode, FunctionDefinitionNode, ModelNode, VariableNode};
use std::cell::RefCell;
use std::rc::Rc;

/// Имя синтетического параметра у функции, объявленной без параметров.
const SYNTHETIC_PARAM: &str = "unused";

/// Тип, подставляемый вместо отсутствующего возвращаемого значения (`unit`) и
/// синтетическому параметру.
fn synthetic_type() -> TypeNode {
    TypeNode::Integer {
        bits: 8,
        signed: false,
    }
}

/// Возвращаемый тип функции для ST.
///
/// `unit` Takt -> синтетический `USINT`: функции, возвращающие `VOID`, - не стандарт
/// (`iec2c -b`).
pub(crate) fn return_type_of(def: &FunctionDefinitionNode) -> TypeNode {
    match def {
        FunctionDefinitionNode::Local { ret, .. }
        | FunctionDefinitionNode::External { ret, .. } => {
            if matches!(ret, TypeNode::Unit) {
                synthetic_type()
            } else {
                ret.clone()
            }
        }
        FunctionDefinitionNode::Builtin(_, _, ret) => ret.clone(),
        FunctionDefinitionNode::None | FunctionDefinitionNode::Unresolved(_) => synthetic_type(),
    }
}

/// Итоговое имя POU в порождённом ST.
///
/// **Локальная** функция получает **префикс уникального имени модели-владельца** -
/// как в цели `c` (`get_function_name`: `Stacker_travel_time`). Пространство имён
/// `FUNCTION`/`FUNCTION_BLOCK` в IEC 61131-3 **плоское**, поэтому без префикса
/// одноимённые `fn` разных моделей склеились бы в одну - а разошедшиеся тела
/// давали бы **молчаливо неверный** автомат ([](../../../../docs/fixes/-st-fn-dedup-silent.md),
/// Tier 1). Префикс берётся из **`upper`** (модель, где функция объявлена), а не
/// из вызывающей: у вызова из под-модели вызывающая и владелец различаются, и
/// объявление с вызовом обязаны построить **одно и то же** имя.
///
/// **Внешняя** (`extern`) и **встроенная** функции остаются с голым именем - как
/// и в `c`: их имена согласованы между целями (`T4`), а extern к тому же
/// именует реальный символ платформы, префиксовать его нельзя.
fn pou_name(def: &FunctionDefinitionNode) -> Option<String> {
    match def {
        FunctionDefinitionNode::Local { upper, name, .. } => {
            let model = upper.as_ref().and_then(|w| w.upgrade())?;
            Some(format!("{}_{}", Name::from(model).unique_camelcase(), name))
        }
        FunctionDefinitionNode::External { name, .. } => Some(name.clone()),
        FunctionDefinitionNode::Builtin(name, _, _) => Some(name.to_string()),
        FunctionDefinitionNode::None | FunctionDefinitionNode::Unresolved(_) => None,
    }
}

/// Печатает вызов функции как выражение ST.
///
/// У функции, объявленной без параметров, есть синтетический параметр, поэтому вызов
/// передаёт `0` - иначе `iec2c` ответит "no parameter defined in function invocation".
///
/// # Ошибки
/// `ST-011` - функция не разрешена либо аргумент не транслируется.
pub(crate) fn print_call(
    def: &Rc<RefCell<FunctionDefinitionNode>>,
    args: &[ExpressionNode],
    model: &ModelNode,
) -> Result<String, Diagnostic> {
    // Аргумент - позиция приёмника с известным типом: разряд `x.N` печатается булевым
    // выражением, и `iec2c` отвечал "Data type incompatibility for value passed in
    // position 1" при нулевом коде возврата `taktc`. Приведение делает та же воронка,
    // что у присваивания.
    let params: Vec<Option<TypeNode>> = match &*def.borrow() {
        FunctionDefinitionNode::Local { params, .. } => {
            params.iter().map(|(_, ty)| Some(ty.clone())).collect()
        }
        _ => Vec::new(),
    };
    let mut printed = Vec::new();
    for (i, arg) in args.iter().enumerate() {
        printed.push(match params.get(i).and_then(Option::as_ref) {
            Some(ty) => crate::generator::st::st_expr::coerce_to(arg, ty, model)?,
            None => print_expression(arg, model)?,
        });
    }
    print_call_in(def, &printed, model)
}

/// Печатает вызов по уже напечатанным аргументам.
///
/// Отдельный вход нужен печатнику условий: у `ConditionNode` своя грамматика, и её
/// аргументы печатает он сам.
pub(crate) fn print_call_texts(
    def: &Rc<RefCell<FunctionDefinitionNode>>,
    args: &[String],
    model: &ModelNode,
) -> Result<String, Diagnostic> {
    print_call_in(def, args, model)
}

/// Общая печать вызова: объявленные аргументы плюс переменные состояния.
fn print_call_in(
    def_rc: &Rc<RefCell<FunctionDefinitionNode>>,
    args: &[String],
    model: &ModelNode,
) -> Result<String, Diagnostic> {
    let def = def_rc.borrow();
    // Имя вызова строит `pou_name` - та же функция, что и объявление: локальная функция
    // получает префикс модели-владельца, поэтому вызов и `FUNCTION` совпадут даже если
    // вызов идёт из другой модели.
    let name = pou_name(&def)
        .ok_or_else(|| unsupported("вызов неразрешённой функции (определение отсутствует)"))?;
    // Порядок аргументов повторяет порядок объявления: параметры массивов печатаются в
    // `VAR_IN_OUT`, то есть после скалярных, - и вызов обязан следовать той же
    // раскладке, иначе `iec2c` отвечает "Data type incompatibility ... position N".
    let mut printed: Vec<String> = reorder_by_sections(&def, args, model);
    if printed.is_empty() && state_params(&def, model).is_empty() {
        // Синтетический параметр требует синтетического аргумента.
        printed.push("0".to_string());
    }
    // Переменные состояния идут после объявленных - тем же порядком, что в `VAR_IN_OUT`
    // функции: список общий (единый источник истины).
    for (var, _) in state_params(&def, model) {
        printed.push(var);
    }
    Ok(format!("{}({})", name, printed.join(", ")))
}

/// Печатает все функции моделей как `FUNCTION ... END_FUNCTION`.
///
/// Функции печатаются **до** `FUNCTION_BLOCK`, которые их вызывают: опережающие ссылки
/// в ST - нестандартное расширение (`iec2c -p`).
///
/// Возвращает предупреждения `ST-009` по каждой `extern fn`.
///
/// # Ошибки
/// `ST-011`/`ST-002` - тело или тип функции не транслируются.
pub(crate) fn emit_functions(
    p: &mut Printer,
    models: &[(crate::semantic::minimap::Name, Rc<RefCell<ModelNode>>)],
    usage: &crate::semantic::unused::UsageSet,
) -> Result<Vec<Diagnostic>, Diagnostic> {
    let mut warnings = Vec::new();
    // Формы массивов из параметров - тот же список, что у продюсера `TYPE ...
    // END_TYPE`: по нему локальные объявления выбирают именованный тип.
    let array_forms = crate::generator::st::st_decl_types::function_array_form_names(models);
    // POU именуются с префиксом модели-владельца (`pou_name`), поэтому одноимённые
    // **локальные** функции разных моделей больше не склеиваются: их итоговые
    // имена различны ([](../../../../docs/fixes/-st-fn-dedup-silent.md),
    // Tier 1 - прежде склейка по голому имени молча теряла тело второй функции).
    // Дедупликация оставлена, но ключ теперь - **итоговое** имя: единственный
    // случай совпадения после префиксации - одноимённые `extern fn` разных
    // моделей (голое имя, как в `c`); их заглушки идентичны, поэтому склейка
    // безопасна и лишь не даёт `iec2c` "duplicate" на корректной модели.
    let mut emitted: Vec<String> = Vec::new();

    for (_, model_rc) in models {
        let model = &*model_rc.borrow();
        // Порядок - по зависимостям вызова: в IEC 61131-3 опережающих ссылок нет, и
        // функция, вызывающая другую, обязана стоять после неё. Алфавитный порядок
        // давал `iec2c` "';' missing at the end of statement" - сообщение, по которому
        // причину не опознать.
        //
        // Граф ацикличен: рекурсию запрещает семантика (`SE-053`).
        for key in crate::generator::call_order::sorted(&model.functions) {
            let Some(def) = model.functions.get(&key) else {
                continue;
            };
            let Some(name) = pou_name(def) else {
                continue;
            };
            // Функция, которую никто не зовёт, не печатается - так же, как у целей `c`,
            // `rust` и `sv`. Признак общий - `UsageSet::functions`; спрашивается голое
            // имя, потому что `pou_name` уже несёт префикс модели-владельца.
            //
            // `iec2c` мёртвый POU принимает, поэтому проверка цели класса не видел: цена -
            // лишний код в программе ПЛК, а после подстановки тела там оставалось
            // объявление функции, вызовов которой в файле уже нет.
            if !def.name().is_empty() && !usage.functions.contains(def.name()) {
                continue;
            }
            if emitted.contains(&name) {
                continue;
            }
            emitted.push(name);
            emit_function(p, def, model, &mut warnings, &array_forms)?;
        }
    }
    Ok(warnings)
}

/// Печатает одну функцию.
fn emit_function(
    p: &mut Printer,
    def: &FunctionDefinitionNode,
    model: &ModelNode,
    warnings: &mut Vec<Diagnostic>,
    array_forms: &[String],
) -> Result<(), Diagnostic> {
    let Some(name) = pou_name(def) else {
        return Ok(());
    };
    // Встроенные функции языка предоставляет сам компилятор ST - не эмитим.
    if matches!(def, FunctionDefinitionNode::Builtin(_, _, _)) {
        return Ok(());
    }
    // Столкновение имени POU со стандартной библиотекой IEC (ST-014). У локальной
    // функции имя префиксовано (`Stacker_travel_time`) и совпасть не может; у `extern`
    // оно голое (`abs`, `concat`...) - вот здесь `ST-014` и сработает.
    check_st_name(&name, def.loc())?;
    // Объявление функции объявляет своё место: предупреждение о заглушке `extern`
    // рождается вне операторов и печаталось без координаты - автор не знал, какую
    // функцию заменять вручную.
    crate::generator::site::enter_declaration(def.loc());
    // Снятие - сразу после печати тела функции (ниже по этой же функции).
    // Возврат-Массив печатается именованным типом: анонимную форму MatIEC отвергает в
    // заголовке `FUNCTION`, хотя в объявлении переменной принимает. Имя строит тот же
    // носитель, что у параметра-массива, - значит объявление типа и ссылка на него
    // совпадают по построению.
    let ret_node = return_type_of(def);
    let ret_ty = match crate::generator::st::st_type::array_form_name(&ret_node, model) {
        Some(form) => form,
        None => get_st_type(&ret_node, model)?,
    };
    p.ident(&format!("FUNCTION {} : {}", name, ret_ty)).nl();

    // Параметры. Пустой `VAR_INPUT ... END_VAR` недопустим (и роняет iec2c
    // segfault'ом), поэтому у беспараметрической функции - синтетический вход.
    let declared = params_of(def);
    // Параметр-Массив уходит в `VAR_IN_OUT`: `VAR_INPUT` с массивом `iec2c`
    // **разбирает**, но порождает C, который не компилируется ("operand of type
    // '__ARRAY_OF_USINT_2' where arithmetic or pointer type is required").
    let (array_params, mut params): (Vec<_>, Vec<_>) = declared
        .into_iter()
        .partition(|(_, ty)| crate::generator::st::st_type::array_form_name(ty, model).is_some());
    if params.is_empty() && array_params.is_empty() && state_params(def, model).is_empty() {
        params.push((SYNTHETIC_PARAM.to_string(), synthetic_type()));
    }
    // Пустой `VAR_INPUT ... END_VAR` недопустим (и роняет iec2c segfault'ом): если
    // параметров нет, но есть состояние, секция просто не печатается.
    if !params.is_empty() {
        p.ident("VAR_INPUT").nl();
        p.up();
        for (pname, pty) in &params {
            // Имя параметра - тоже идентификатор IEC (проба 3 про `left`
            // распространяется и на параметры). Позиции у параметра нет - берётся
            // позиция функции.
            check_st_name(pname, def.loc())?;
            // Массив объявляется именованной формой: анонимный `ARRAY [...] OF T` в
            // `VAR_INPUT` MatIEC не принимает - "Data type incompatibility for value
            // passed in position 1".
            let ty = match crate::generator::st::st_type::array_form_name(pty, model) {
                Some(form) => form,
                None => get_st_type(pty, model)?,
            };
            p.ident(&format!("{} : {};", pname, ty)).nl();
        }
        p.down();
        p.ident("END_VAR").nl();
    }

    // Переменные модели, которые тело трогает: `FUNCTION` в IEC чистая, поэтому они
    // передаются по ссылке (см.
    let state = state_params(def, model);
    if !state.is_empty() || !array_params.is_empty() {
        p.ident("VAR_IN_OUT").nl();
        p.up();
        // Параметры-массивы идут первыми: их порядок в объявлении обязан совпадать с
        // порядком аргументов вызова (`st_func::print_call_in` печатает объявленные
        // аргументы, затем переменные состояния).
        for (pname, pty) in &array_params {
            check_st_name(pname, def.loc())?;
            let ty = crate::generator::st::st_type::array_form_name(pty, model)
                .ok_or_else(|| unsupported("параметр-массив без именованной формы"))?;
            p.ident(&format!("{} : {};", pname, ty)).nl();
        }
        for (vname, vty) in &state {
            // Переменная-Массив объявляется именованной формой, как и параметр-массив:
            // анонимный `ARRAY [...] OF T` в `VAR_IN_OUT` `iec2c` встречает отказом
            // "Data type incompatibility for value passed in position N".
            let ty = match crate::generator::st::st_type::array_form_name(vty, model) {
                Some(form) => form,
                None => get_st_type(vty, model)?,
            };
            p.ident(&format!("{} : {};", vname, ty)).nl();
        }
        p.down();
        p.ident("END_VAR").nl();
    }
    // Локальные объявления тела. Печатаются до `VAR CONSTANT` - и это не стиль: MatIEC
    // протаскивает квалификатор `CONSTANT` предыдущей секции на следующий за ней `VAR`,
    // из-за чего присваивание локальной переменной отвергается ("Assignment to CONSTANT
    // variables is not allowed"). Проверено пробой: порядок `VAR CONSTANT` -> `VAR`
    // невалиден, `VAR` -> `VAR CONSTANT` валиден. Дефект вскрыт: до неё ни одна функция
    // корпуса не имела локальных переменных, и порядок ничего не ломал.
    let (hoisted, body_text) = match def {
        FunctionDefinitionNode::Local { body, ret, .. } => {
            collect_hoisted(body, &name, ret, model)?
        }
        // Тела нет: у `extern fn` печатается заглушка (ниже), у остальных - печатать
        // нечего, поднимать тоже нечего.
        FunctionDefinitionNode::External { .. }
        | FunctionDefinitionNode::Builtin(_, _, _)
        | FunctionDefinitionNode::None
        | FunctionDefinitionNode::Unresolved(_) => (Vec::new(), String::new()),
    };
    // Локальная переменная тела функции - тоже идентификатор IEC. Позиции у поднятой
    // переменной нет - берётся позиция функции.
    for h in &hoisted {
        check_st_name(&h.name, def.loc())?;
    }
    emit_hoisted_var(p, &hoisted, model, array_forms)?;

    // Константы модели дублируются внутрь функции.
    let consts = const_params(def, model);
    // Константы перечислений - тоже: `FUNCTION` в IEC замкнута, и `Mode_Idle`,
    // объявленная в `FUNCTION_BLOCK`, внутри неё не видна.
    let enum_consts = enum_constants_used(&body_text, model)?;
    if !consts.is_empty() || !enum_consts.is_empty() {
        p.ident("VAR CONSTANT").nl();
        p.up();
        for (cname, ty_name, value) in &enum_consts {
            p.ident(&format!("{cname} : {ty_name} := {value};")).nl();
        }
        for cname in &consts {
            let VariableNode::Const { ty, expr, .. } = &model.variables[cname] else {
                continue;
            };
            let ty_name = get_st_type(ty, model)?;
            let init = crate::generator::st::st_decl::literal_init(expr, ty, None);
            match init {
                Some(v) => p.ident(&format!("{} : {} := {};", cname, ty_name, v)).nl(),
                None => {
                    return Err(unsupported(&format!(
                        "константа '{}' с невычислимым инициализатором внутри функции",
                        cname
                    )));
                }
            };
        }
        p.down();
        p.ident("END_VAR").nl();
    }

    match def {
        FunctionDefinitionNode::External { .. } => {
            warnings.push(
                Diagnostic::warning(
                    crate::generator::site::at(Location::Codegen),
                    format!(
                        "Внешняя функция '{}': тело неизвестно, а IEC 61131-3 требует \
                         его от FUNCTION. Эмитирована заглушка, возвращающая {} — в \
                         ПЛК она НИЧЕГО НЕ СДЕЛАЕТ. Замените её реализацией вручную",
                        name,
                        neutral_value(&return_type_of(def), model)?
                    ),
                )
                .with_code("ST-009"),
            );
            p.up();
            let neutral = neutral_value(&return_type_of(def), model)?;
            p.ident(&format!("{} := {};", name, neutral)).nl();
            p.down();
        }
        FunctionDefinitionNode::Local { body, ret, .. } => {
            emit_local_body(p, &name, body, ret, model)?;
        }
        FunctionDefinitionNode::Builtin(_, _, _)
        | FunctionDefinitionNode::None
        | FunctionDefinitionNode::Unresolved(_) => {}
    }
    p.ident("END_FUNCTION").nl().nl();
    // Слой объявления снимается парно входу: переживи он печать функции, отказ в теле
    // состояния получил бы её координату.
    crate::generator::site::leave_declaration();
    Ok(())
}

/// Собирает объявления, поднимаемые из тела функции в её секцию `VAR`.
///
/// Тело печатается "вхолостую": в ST объявления обязаны стоять до тела, поэтому узнать
/// их состав можно только пройдя тело заранее.
fn collect_hoisted(
    body: &crate::semantic::StatementNode,
    name: &str,
    ret: &TypeNode,
    model: &ModelNode,
) -> Result<(Vec<Hoisted>, String), Diagnostic> {
    let mut probe = String::new();
    let mut out = StmtOutput::default();
    {
        let mut probe_p = Printer::new(4, &mut probe);
        print_statement(body, model, &mut probe_p, &mut out, Some((name, ret)))?;
    }
    // Текст тела нужен ещё и для отбора констант перечислений: `FUNCTION` в IEC -
    // замкнутая единица, и `Mode_Idle`, объявленная в `FUNCTION_BLOCK`, внутри неё не
    // видна ("Variable not declared in this scope"). Признак тот же, что у заглушки
    // параметра (0260/0337): вопрос задаётся напечатанному тексту.
    Ok((out.hoisted, probe))
}

/// Печатает секцию `VAR` поднятых объявлений (пустую - не печатает: пустой `VAR ...
/// END_VAR` для `iec2c` невалиден).
fn emit_hoisted_var(
    p: &mut Printer,
    hoisted: &[Hoisted],
    model: &ModelNode,
    array_forms: &[String],
) -> Result<(), Diagnostic> {
    if hoisted.is_empty() {
        return Ok(());
    }
    p.ident("VAR").nl();
    p.up();
    let mut seen: Vec<&str> = Vec::new();
    for h in hoisted {
        if seen.contains(&h.name.as_str()) {
            continue;
        }
        seen.push(&h.name);
        // Локальный массив, чья форма встречается в параметре функции, объявляется той
        // Же формой: типы аргумента и параметра MatIEC сверяет буквально.
        let ty = crate::generator::st::st_type::local_declaration_type(&h.ty, model, array_forms)?;
        p.ident(&format!("{} : {};", h.name, ty)).nl();
    }
    p.down();
    p.ident("END_VAR").nl();
    Ok(())
}

/// Печатает тело локальной функции. Секцию `VAR` печатает вызывающий - до `VAR
/// CONSTANT` (см.
fn emit_local_body(
    p: &mut Printer,
    name: &str,
    body: &crate::semantic::StatementNode,
    ret: &TypeNode,
    model: &ModelNode,
) -> Result<(), Diagnostic> {
    p.up();
    let mut out2 = StmtOutput::default();
    print_statement(body, model, p, &mut out2, Some((name, ret)))?;
    // Функция, объявленная как `unit`, значение не возвращает - но в ST тип
    // синтетический, поэтому имя обязано быть присвоено хотя бы раз.
    if matches!(ret, TypeNode::Unit) {
        let neutral = neutral_value(&synthetic_type(), model)?;
        p.ident(&format!("{} := {};", name, neutral)).nl();
    }
    p.down();
    Ok(())
}

/// Нейтральное значение типа - для заглушек и синтетических возвратов.
fn neutral_value(ty: &TypeNode, model: &ModelNode) -> Result<String, Diagnostic> {
    Ok(match ty {
        TypeNode::Bit | TypeNode::Bool => "FALSE".to_string(),
        TypeNode::Rational => "0.0".to_string(),
        TypeNode::Integer { .. } => "0".to_string(),
        // Для прочих типов нейтральное значение не очевидно - лучше отказ, чем выдумка:
        // заглушка и так подменяет поведение, молча угадывать нельзя.
        _ => {
            return Err(unsupported(&format!(
                "нейтральное значение для типа '{}' (заглушка внешней функции)",
                get_st_type(ty, model).unwrap_or_else(|_| ty.to_string())
            )));
        }
    })
}

/// Строит диагностику `ST-011`. Константы перечислений, упомянутые в напечатанном теле
/// функции.
///
/// `FUNCTION` в IEC 61131-3 - замкнутая единица: она видит только объявленное в ней
/// самой. Константа `Mode_Idle` живёт в `VAR CONSTANT` функционального блока, и
/// обращение к ней из функции `iec2c` отвергает ("Ambiguous enumerate value or Variable
/// not declared in this scope") - при **нулевом** коде возврата `taktc`. Это тот же
/// довод, по которому дублирует внутрь функции константы модели.
///
/// Отбор идёт по **тексту** тела, а не по обходу дерева: имя константы строит печатник
/// (`st_expr::coerce_to`), и второй способ узнать, какое имя он напечатал, разошёлся бы
/// с первым.
fn enum_constants_used(
    body: &str,
    model: &ModelNode,
) -> Result<Vec<(String, String, String)>, Diagnostic> {
    let mut out = Vec::new();
    for (enum_name, node) in crate::generator::st::st_decl::visible_enums(model) {
        let ty = get_st_type(&TypeNode::Enum(enum_name.clone()), model)?;
        for (variant, value) in &node.variants {
            let name = format!("{enum_name}_{variant}");
            if body.lines().any(|line| mentions_ident(line, &name)) {
                out.push((name, ty.clone(), value.to_string()));
            }
        }
    }
    out.sort();
    Ok(out)
}

/// Встречается ли `ident` в строке **как отдельный идентификатор**.
///
/// Границы обязательны: `Mode_Idle` - префикс `Mode_IdleLong`.
fn mentions_ident(line: &str, ident: &str) -> bool {
    let bytes = line.as_bytes();
    let mut from = 0;
    while let Some(pos) = line[from..].find(ident) {
        let start = from + pos;
        let end = start + ident.len();
        let before_ok = start == 0 || !is_ident_byte(bytes[start - 1]);
        let after_ok = end == bytes.len() || !is_ident_byte(bytes[end]);
        if before_ok && after_ok {
            return true;
        }
        from = end;
    }
    false
}

/// Байт, который может входить в идентификатор IEC.
fn is_ident_byte(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_'
}

/// Переставляет аргументы в порядок секций объявления.
///
/// Скалярные параметры объявлены в `VAR_INPUT`, массивы - в `VAR_IN_OUT`; позиционный
/// вызов IEC следует этому порядку, а не порядку в исходнике Takt.
fn reorder_by_sections(
    def: &FunctionDefinitionNode,
    args: &[String],
    model: &ModelNode,
) -> Vec<String> {
    let params = params_of(def);
    if params.len() != args.len() {
        // Число аргументов сверяет семантика (`SE-122`); здесь расхождение означало бы
        // синтетический аргумент - порядок не трогаем.
        return args.to_vec();
    }
    let mut scalars = Vec::new();
    let mut arrays = Vec::new();
    for ((_, ty), arg) in params.iter().zip(args) {
        if crate::generator::st::st_type::array_form_name(ty, model).is_some() {
            arrays.push(arg.clone());
        } else {
            scalars.push(arg.clone());
        }
    }
    scalars.extend(arrays);
    scalars
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic::tree::construct_model;

    /// Печатает все функции модели.
    ///
    /// Корню даётся имя `M` (как в C-тестах, `c_source.rs`): у файла имя корня берётся
    /// из имени файла, а у анонимной модели теста оно пусто - тогда префикс POU
    /// выродился бы в `_add1`. Имя делает префикс осмысленным (`M_add1`) и совпадающим
    /// с тем, что даёт реальный конвейер (`Stacker_...`).
    fn functions_of(src: &str) -> (String, Vec<Diagnostic>) {
        let (ast, _) = crate::parse(src, 0).unwrap();
        let rc = construct_model(&ast, None, &[]).unwrap();
        rc.borrow_mut().name = Some("M".to_string());
        let name = crate::semantic::minimap::Map::create(Rc::clone(&rc))
            .unwrap()
            .root_name();
        let models = vec![(name, Rc::clone(&rc))];
        // Признак использования - тот же, что у конвейера: без него юнит-тест печатал
        // бы то, чего цель уже не печатает.
        let usage = crate::semantic::unused::compute_usage(Rc::clone(&rc));
        let mut text = String::new();
        let warnings = {
            let mut p = Printer::new(4, &mut text);
            emit_functions(&mut p, &models, &usage).expect("функции должны печататься")
        };
        (text, warnings)
    }

    /// Локальная функция -> `FUNCTION имя : ТИП` ... `END_FUNCTION`.
    #[test]
    fn test_local_function_emits_function_pou() {
        let (st, _) = functions_of(
            "fn add1(n: u8) -> u8 { return n + 1; }\nvar x: u8 := 0;\n\
             start S { always { x := add1(x); } }",
        );
        // Локальная функция префиксуется именем модели-владельца (`M`) - как в цели
        // `c`: без префикса одноимённые `fn` разных моделей склеились бы.
        assert!(
            st.contains("FUNCTION M_add1 : USINT"),
            "нет FUNCTION с префиксом модели:\n{st}"
        );
        assert!(st.contains("n : USINT;"), "нет параметра:\n{st}");
        assert!(st.contains("END_FUNCTION"), "нет END_FUNCTION:\n{st}");
    }

    /// Возврат - присваивание имени функции плюс `RETURN;`: `return <знач>` в ST нет.
    #[test]
    fn test_return_becomes_assignment_to_function_name() {
        let (st, _) = functions_of(
            "fn add1(n: u8) -> u8 { return n + 1; }\nvar x: u8 := 0;\n\
             start S { always { x := add1(x); } }",
        );
        assert!(
            st.contains("M_add1 := n + 1;"),
            "возврат обязан быть присваиванием (префиксованному) имени функции:\n{st}"
        );
        assert!(st.contains("RETURN;"), "нет RETURN:\n{st}");
    }

    /// Ранний возврат внутри `if` (форма `abs_diff`, `stacker.takt:100-103`).
    #[test]
    fn test_early_return_inside_if() {
        let (st, _) = functions_of(
            "fn abs_diff(a: u8, b: u8) -> u8 { if a > b { return a - b; } return b - a; }\n\
             var x: u8 := 0;\nstart S { always { x := abs_diff(x, 1); } }",
        );
        assert!(st.contains("IF a > b THEN"), "нет ветвления:\n{st}");
        assert!(
            st.contains("M_abs_diff := a - b;"),
            "нет раннего возврата:\n{st}"
        );
        assert!(
            st.contains("M_abs_diff := b - a;"),
            "нет позднего возврата:\n{st}"
        );
    }

    /// Локальные переменные функции поднимаются в её `VAR` - до тела.
    #[test]
    fn test_function_locals_are_hoisted_before_body() {
        let (st, _) = functions_of(
            "fn f(n: u8) -> u8 { var t: u8 := 0; t := t + n; return t; }\n\
             var x: u8 := 0;\nstart S { always { x := f(x); } }",
        );
        let var_pos = st.find("\nVAR\n").expect("нет секции VAR функции");
        let body_pos = st.find("t := 0;").expect("нет тела");
        assert!(var_pos < body_pos, "VAR обязан идти до тела:\n{st}");
        assert!(st.contains("t : USINT;"), "локальная не поднята:\n{st}");
    }

    /// Локальный `VAR` обязан идти до `VAR CONSTANT`.
    ///
    /// Не стиль, а условие валидности: MatIEC протаскивает квалификатор `CONSTANT`
    /// предыдущей секции на следующий за ней `VAR`, и присваивание локальной переменной
    /// отвергается ("Assignment to CONSTANT variables is not allowed"). Проверено
    /// пробой на `iec2c`: порядок `VAR CONSTANT` -> `VAR` невалиден, обратный -
    /// валиден.
    ///
    /// Тест нужен потому, что **проверка этот дефект не ловил**: до 0030 ни одна функция
    /// корпуса не имела локальных переменных одновременно с константой, то есть дефект
    /// был латентным при зелёном `iec2c`.
    #[test]
    fn test_function_var_precedes_var_constant() {
        let (st, _) = functions_of(
            "const LIM: u8 := 3;\n\
             fn f(n: u8) -> u8 { var t: u8 := 0; t := n + LIM; return t; }\n\
             var x: u8 := 0;\nstart S { always { x := f(x); } }",
        );
        let var_pos = st.find("\nVAR\n").expect("нет секции VAR функции");
        let const_pos = st
            .find("VAR CONSTANT")
            .expect("нет секции VAR CONSTANT функции");
        assert!(
            var_pos < const_pos,
            "локальный VAR обязан идти до VAR CONSTANT — иначе iec2c считает \
             локальные переменные константами:\n{st}"
        );
    }

    /// `extern fn` -> заглушка с телом **плюс** предупреждение `ST-009`.
    ///
    /// Тела у внешней функции нет, а IEC требует его от `FUNCTION`. Молчание
    /// дало бы ПЛК-код, который тихо ничего не делает.
    #[test]
    fn test_extern_fn_emits_stub_and_warns_st009() {
        let (st, warnings) = functions_of(
            "extern fn log_it(v: u8);\nvar x: u8 := 0;\n\
             start S { always { log_it(x); } }",
        );
        assert!(
            st.contains("FUNCTION log_it : USINT"),
            "нет заглушки:\n{st}"
        );
        assert!(
            st.contains("log_it := 0;"),
            "заглушка обязана иметь тело:\n{st}"
        );
        assert_eq!(warnings.len(), 1, "extern fn обязана предупреждать");
        assert_eq!(warnings[0].code.as_deref(), Some("ST-009"));
    }

    /// Функция без параметров получает синтетический вход.
    ///
    /// Вход из `elevator.takt:93`: `extern fn motor_up();`. Пустой `VAR_INPUT ...
    /// END_VAR` не только недопустим, но и **роняет `iec2c` segfault'ом**, а
    /// беспараметрический POU - нестандартное расширение (`iec2c -i`).
    #[test]
    fn test_parameterless_function_gets_synthetic_input() {
        let (st, _) = functions_of(
            "extern fn motor_up();\nvar x: u8 := 0;\nstart S { always { motor_up(); } }",
        );
        assert!(
            st.contains("unused : USINT;"),
            "беспараметрическая функция обязана получить синтетический вход:\n{st}"
        );
        assert!(
            !st.contains("VAR_INPUT\nEND_VAR"),
            "пустой VAR_INPUT роняет iec2c segfault'ом:\n{st}"
        );
    }

    /// Функция, возвращающая `unit`, получает синтетический тип: `VOID` в ST -
    /// нестандартное расширение (`iec2c -b`).
    #[test]
    fn test_unit_returning_function_gets_synthetic_return_type() {
        let (st, _) = functions_of(
            "extern fn motor_up();\nvar x: u8 := 0;\nstart S { always { motor_up(); } }",
        );
        assert!(
            st.contains("FUNCTION motor_up : USINT"),
            "unit обязан стать синтетическим USINT:\n{st}"
        );
    }

    /// Вызов беспараметрической функции передаёт синтетический аргумент.
    #[test]
    fn test_call_of_parameterless_function_passes_synthetic_argument() {
        let (ast, _) = crate::parse(
            "extern fn motor_up();\nvar x: u8 := 0;\nstart S { always { motor_up(); } }",
            0,
        )
        .unwrap();
        let rc = construct_model(&ast, None, &[]).unwrap();
        let model = rc.borrow();
        let def = model.search_func("motor_up").expect("нет функции");
        let text = print_call(&def, &[], &model).unwrap();
        assert_eq!(
            text, "motor_up(0)",
            "синтетический параметр требует аргумента"
        );
    }
}
