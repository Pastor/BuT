//! Разбор объявления значения модели: `var`, порт, `const`, `parameter`.
//!
//! "Построить узел объявления" - самостоятельная ответственность, отделимая от обхода
//! элементов модели.

use crate::diagnostics::lang::keys;
use crate::msg;
use crate::diagnostics::{Diagnostic, Location};
use crate::parser::ast::{Identifier, VariableDefine};
mod init_refusal;

use crate::semantic::declaration::init_refusal::{
    forward_reference, initializer_calls_extern, initializer_calls_function, unfoldable_call,
    unfoldable_fractional,
};
use crate::semantic::type_node::{TypeNode, construct_type};
use crate::semantic::{
    ExpressionNode, ModelNode, ParameterNode, PortDirection, VariableNode, const_eval,
};
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::rc::Rc;

/// Строит узел объявления и кладёт его в карту переменных модели.
///
/// Параметр дополнительно попадает в `parameters` - в порядке объявления: по нему
/// строится ключ дедупликации специализаций (`--parameters=specialize`), а детерминизм
/// вывода требует, чтобы порядок зависел только от входа.
pub(super) fn construct_declaration(
    def: &VariableDefine,
    model_node: Rc<RefCell<ModelNode>>,
    variables: &mut BTreeMap<String, VariableNode>,
    parameters: &mut Vec<ParameterNode>,
) -> Result<(), Diagnostic> {
    // Пока тип определяется только из явной аннотации.
    match def.clone() {
        VariableDefine::Variable {
            loc,
            typ,
            name,
            initializer,
        } => {
            let name = extract_name(name.clone(), loc)?;
            variables.insert(
                name.clone(),
                VariableNode::Simple {
                    upper: Some(Rc::downgrade(&model_node)),
                    loc,
                    name: name.clone(),
                    ty: construct_type(typ, Rc::clone(&model_node))?,
                    expr: initializer
                        .map(ExpressionNode::Unresolved)
                        .unwrap_or(ExpressionNode::None),
                },
            )
        }
        VariableDefine::Port {
            loc,
            typ,
            name,
            address,
            initializer,
            direction,
        } => {
            let name = extract_name(name.clone(), loc)?;
            let type_node = construct_type(typ, Rc::clone(&model_node))?;
            if type_node == TypeNode::Inference {
                return Err(
                    Diagnostic::error(loc, msg!(keys::SE_023_PORT_NEEDS_TYPE))
                        .with_code("SE-023"),
                );
            }
            // Два независимых выражения: размещение `at <адрес>` и начальное значение
            // `:=`. Каждое необязательно - адрес может прийти по имени порта (оператор
            // `address`, внешняя карта), и полноту проверяет слой адресов, а не
            // объявление.
            let address_node = address
                .clone()
                .map(ExpressionNode::Unresolved)
                .unwrap_or(ExpressionNode::None);
            let init_node = initializer
                .clone()
                .map(ExpressionNode::Unresolved)
                .unwrap_or(ExpressionNode::None);
            variables.insert(
                name.clone(),
                VariableNode::Port {
                    upper: Some(Rc::downgrade(&model_node)),
                    loc,
                    name: name.clone(),
                    ty: type_node,
                    address: address_node,
                    init: init_node,
                    direction,
                },
            )
        }
        VariableDefine::Constant {
            loc,
            typ,
            name,
            initializer,
        } => {
            let name = extract_name(name.clone(), loc)?;
            variables.insert(
                name.clone(),
                VariableNode::Const {
                    upper: Some(Rc::downgrade(&model_node)),
                    loc,
                    name: name.clone(),
                    ty: construct_type(typ, Rc::clone(&model_node))?,
                    expr: ExpressionNode::Unresolved(initializer),
                },
            )
        }
        // Параметр модели. В дереве он **обычная переменная** с начальным значением: в
        // режиме генерации по умолчанию (`--parameters=assign`) параметр и есть поле
        // экземпляра, поэтому потребитель, ничего не знающий о параметрах, обращается с
        // ним верно. Отличие хранится отдельно - в `ModelNode::parameters` (имя,
        // позиция, порядок).
        VariableDefine::Parameter {
            loc,
            typ,
            name,
            initializer,
        } => {
            let name = extract_name(name.clone(), loc)?;
            // Параметр верхнего уровня файла инстанцировать нечем: анонимный корень в
            // выражении реализации по имени не появляется (п. Отказ здесь - вместо
            // объявления, которое молча вело бы себя как `var`.
            if model_node.borrow().upper.is_none() && model_node.borrow().name.is_none() {
                return Err(Diagnostic::error(
                    loc,
                    msg!(keys::SE_075_PARAMETER_OUTSIDE_MODEL, name = name),
                )
                .with_code("SE-075"));
            }
            parameters.push(ParameterNode {
                name: name.clone(),
                loc,
                // "Изменяемый", пока анализ изменяемости не сказал иное: неразмеченный
                // параметр обязан вести себя как переменная.
                mutated: true,
            });
            variables.insert(
                name.clone(),
                VariableNode::Simple {
                    upper: Some(Rc::downgrade(&model_node)),
                    loc,
                    name: name.clone(),
                    ty: construct_type(typ, Rc::clone(&model_node))?,
                    expr: ExpressionNode::Unresolved(initializer),
                },
            )
        }
    };
    Ok(())
}

/// Разбирает объявление **внутри блока оператора**: имя, тип, инициализатор.
///
/// Отличается от [`construct_declaration`] тем, что локальное объявление не становится
/// членом модели: узел строит вызывающий
/// ([`StatementNode::Variable`](crate::semantic::StatementNode::Variable)).
pub(super) fn local_declaration(
    def: &VariableDefine,
    loc: Location,
    model: Rc<RefCell<ModelNode>>,
) -> Result<(String, TypeNode, Option<crate::parser::ast::Expression>), Diagnostic> {
    let named =
        |name: &Option<Identifier>| name.as_ref().map(|i| i.name.clone()).unwrap_or_default();
    match def {
        VariableDefine::Variable {
            name,
            typ,
            initializer,
            ..
        }
        | VariableDefine::Port {
            name,
            typ,
            initializer,
            ..
        } => Ok((
            named(name),
            construct_type(typ.clone(), model)?,
            initializer.clone(),
        )),
        VariableDefine::Constant {
            name,
            typ,
            initializer,
            ..
        } => Ok((
            named(name),
            construct_type(typ.clone(), model)?,
            Some(initializer.clone()),
        )),
        // Параметр в теле блока грамматикой не порождается (`LocalVariableDefine` слова
        // `parameter` не знает), но ветвь обязана быть: расширив грамматику,
        // разработчик получит здесь явный отказ, а не молчаливое превращение параметра
        // в локальную переменную.
        VariableDefine::Parameter { name, .. } => Err(Diagnostic::error(
            loc,
            msg!(keys::SE_075_PARAMETER_INSIDE_BLOCK, name = named(name)),
        )
        .with_code("SE-075")),
    }
}

/// Имя объявления либо отказ: безымянное объявление разбором не отсеивается.
fn extract_name(id: Option<Identifier>, loc: Location) -> Result<String, Diagnostic> {
    match id {
        Some(id) => Ok(id.name.clone()),
        None => {
            Err(Diagnostic::error(loc, msg!(keys::SE_021_IDENTIFIER_MISSING)).with_code("SE-021"))
        }
    }
}

/// Разрешает выражение объявления, если оно ещё "сырое" (`Unresolved`).
///
/// Вынесено сюда: у порта таких выражений **два** - размещение и начальное значение, -
/// и повтор `match` для каждого раздул бы `tree.rs`, уже стоящий в реестре размера.
pub(crate) fn resolve_declaration_expression(
    expr: ExpressionNode,
    model: &Rc<RefCell<ModelNode>>,
) -> Result<ExpressionNode, Diagnostic> {
    match expr {
        ExpressionNode::Unresolved(raw) => {
            crate::semantic::expression::construct_expression(raw, vec![], Rc::clone(model))
        }
        other => Ok(other),
    }
}

/// Разрешает "сырые" выражения объявления переменной (`Unresolved` -> дерево).
///
/// У порта выражений два - размещение и начальное значение; узел без "сырых" выражений
/// возвращается как есть.
pub(crate) fn resolve_variable_expressions(
    var: VariableNode,
    model: &Rc<RefCell<ModelNode>>,
) -> Result<VariableNode, Diagnostic> {
    Ok(match var {
        VariableNode::Simple {
            upper,
            loc,
            name,
            ty,
            expr,
        } => VariableNode::Simple {
            upper,
            loc,
            name,
            ty,
            expr: resolve_declaration_expression(expr, model)?,
        },
        VariableNode::Const {
            upper,
            loc,
            name,
            ty,
            expr,
        } => VariableNode::Const {
            upper,
            loc,
            name,
            ty,
            expr: resolve_declaration_expression(expr, model)?,
        },
        VariableNode::Port {
            upper,
            loc,
            name,
            ty,
            address,
            init,
            direction,
        } => VariableNode::Port {
            upper,
            loc,
            address: resolve_declaration_expression(address, model)?,
            init: resolve_port_init(init, &name, direction, loc, model)?,
            name,
            ty,
            direction,
        },
        VariableNode::Unresolved => VariableNode::Unresolved,
    })
}

/// Разрешает **начальное значение** порта, сворачивая его в литерал.
///
/// # Что принимается
///
/// Всё, что вычисляет [`const_eval`]: литералы, константы модели (в том числе цепочкой)
/// и арифметика над ними. Прочее - **`SE-094`** с названной причиной: молчаливая потеря
/// значения здесь дороже отказа.
///
/// У **входного** порта значение не сворачивается: его там не бывает вовсе (`SE-092`),
/// и свёртка перехватила бы диагностику, подменив её жалобой на невычислимость.
fn resolve_port_init(
    init: ExpressionNode,
    name: &str,
    direction: PortDirection,
    loc: Location,
    model: &Rc<RefCell<ModelNode>>,
) -> Result<ExpressionNode, Diagnostic> {
    if direction == PortDirection::In {
        return resolve_declaration_expression(init, model);
    }
    let ExpressionNode::Unresolved(raw) = &init else {
        return Ok(init);
    };
    let literal = const_eval::fold_to_literal(raw, model).map_err(|cause| {
        Diagnostic::error(
            loc,
            msg!(
                keys::SE_094_PORT_INITIAL_VALUE_NOT_CONSTANT,
                name = name,
                cause = cause.message
            ),
        )
        .with_code("SE-094")
    })?;
    resolve_declaration_expression(ExpressionNode::Unresolved(literal), model)
}

/// Сворачивает инициализаторы `var`/`const` в литералы - **в порядке текста**.
///
/// # Порядок и ссылки на имена
///
/// Имя в позиции инициализатора означает **начальное значение** переменной, а не
/// значение в такте, и ссылаться можно только **назад по тексту**. Поэтому объявления
/// сортируются по позиции в исходнике: `variables` - `BTreeMap`, её обход
/// **алфавитный**, и порядок объявлений в нём не сохранён.
///
/// Послабление "имя переменной вычислимо" живёт **здесь**, в наполнении
/// [`const_eval::Locals`], а не в `resolve_name` общего вычислителя: тот же вычислитель
/// обслуживает выдержку `after`, параметры моделей и порты, где "значение переменной
/// известно только в такте" - верное правило.
///
/// # Ошибки
///
/// `SE-083` от вычислителя - с названной причиной (ссылка вперёд, цикл, порт,
/// невычислимая форма). Молчаливый ноль здесь дороже отказа: именно он и был дефектом.
pub(crate) fn fold_variable_initializers(
    variables: &BTreeMap<String, VariableNode>,
    raw: &BTreeMap<String, crate::parser::ast::Expression>,
    model: &Rc<RefCell<ModelNode>>,
    untyped: &std::collections::BTreeSet<String>,
) -> Result<BTreeMap<String, VariableNode>, Diagnostic> {
    let mut order: Vec<&String> = variables.keys().collect();
    order.sort_by_key(|name| declaration_position(&variables[*name]));

    let mut known = const_eval::Locals::default();
    let mut folded = variables.clone();
    for name in order {
        let loc = match &variables[name] {
            VariableNode::Simple { loc, .. } | VariableNode::Const { loc, .. } => *loc,
            // Порт сворачивается своим путём: у него другое правило и другая
            // диагностика. Прочее значений не несёт.
            VariableNode::Port { .. } | VariableNode::Unresolved => continue,
        };
        let Some(source) = raw.get(name) else {
            continue;
        };
        // Ссылка вперёд на переменную - ошибка. Проверка стоит до общего "невычислимое
        // оставляем как есть": иначе она растворяется в нём, и запись даёт разные
        // значения у эталона (0) и в прошивке (1, потому что цель `c` печатает
        // присваивания в порядке объявления, а поле к этому моменту обнулено). Пять
        // потребителей отвечали по-разному - замер.
        if let Some(diagnostic) = forward_reference(source, name, variables) {
            return Err(diagnostic);
        }
        let literal = match const_eval::fold_to_literal_in(source, model, &known) {
            Ok(literal) => literal,
            Err(cause) => {
                // Диагностика, которая не означает "не константа", терминальна:
                // вычислитель отвечает двумя родами сообщений, и ошибку самой записи
                // проглатывать нельзя - иначе вход уедет к потребителям, где они
                // разойдутся молча. Так `300 as i8` в объявлении давал `0` у эталона,
                // `44` у `c`/`rust` и потерю инициализатора у `st`.
                if !const_eval::is_not_constant(&cause) {
                    return Err(cause);
                }
                // Дробное объявление, инициализатор которого не свернулся, - ошибка
                // `SE-114`. Здесь молчание стоит дороже всего: эталон оставляет ноль, а
                // цель печатает выражение и считает его сама, то есть прогон и прошивка
                // расходятся без единого слова. Точную арифметику свёртка уже выполнила
                // (`decimal.rs`); сюда доходит лишь то, что требует округления, а оно
                // задано эталоном.
                if let Some(diagnostic) =
                    unfoldable_fractional(source, name, declared_type(&variables[name]), loc)
                {
                    return Err(diagnostic);
                }
                // Вызов внешней функции в инициализаторе - `SE-084`: её значение при
                // компиляции неизвестно по определению, а молчание здесь дороже всего -
                // цель `st` теряла инициализатор без единого слова, тогда как `c`,
                // `rust` и `sv` отказывали.
                if let Some(func) = initializer_calls_extern(source, model) {
                    return Err(Diagnostic::error(
                        loc,
                        msg!(keys::SE_084_INITIALIZER_CALLS_EXTERN, name = name, func = func),
                    )
                    .with_code("SE-084"));
                }
                // Невычислимый вызов локальной функции - `SE-084` с причиной.
                // Вычислитель уже назвал, почему тело не исполняется (чтение
                // переменной, порт, неподдержанный оператор, исчерпанный бюджет), и до
                // этой фичи его слова выбрасывались вместе с ошибкой: эталон оставлял
                // ноль молча, `st` теряла инициализатор, а `c`, `rust` и `sv` отвечали
                // внутренними кодами (`CC-023`, `RS-011`, `SV-002`), которые называют
                // дефект инструмента, а не запись автора.
                if initializer_calls_function(source) {
                    return Err(unfoldable_call(name, &cause, loc));
                }
                // Прочее невычислимое оставляем как есть: диагностику о нём (если она
                // нужна) поднимает разрешение выражения ниже по конвейеру. Отвергать
                // всякую невычислимую форму значило бы ломать входы, которых нет в
                // корпусе (: Option D расширяет язык, а не ужесточает).
                continue;
            }
        };
        // Значение запоминается всегда - в том числе у литерала: на него могут
        // сослаться объявления ниже (`var base := 5; var probe := base + 1;`).
        // Пропустив этот шаг для литералов, ссылку сломаешь молча: проба давала `probe
        // = 0`.
        if let Ok(value) = const_eval::eval(&literal, model, &mut const_eval::Budget::new()) {
            known.declare(name, value);
        }
        // А вот подменять литерал нечем и вредно: дробный литерал к этому моменту уже
        // понижен в q-представление, и подстановка "свёрнутого" `0.0` обратно отменила
        // бы понижение.
        if is_literal(source) {
            continue;
        }
        // Вычисленное значение нормируется по типу объявления. Литерал автора сюда не
        // доходит - он отсечён строкой выше, и его выход за границы судит `SE-089`.
        // Ширина выведенного типа берётся у результата, а не у операндов: тип выводится
        // до свёртки, и `const K := 1 + 255;` получил бы ширину левого литерала (8 бит),
        // а нормирование завернуло бы вычисленные 256 в ноль - молча и одинаково у всех
        // потребителей. Автор ширины не выбирал: её выбрал операнд.
        //
        // Явно объявленный тип не трогается: `var u: u8 := 200 + 100;` обязан остаться
        // `44` - там ширину выбрал автор, и обёртка совпадает с тем, что даёт то же
        // выражение в теле.
        //
        // Расширяется только значение, не помещающееся в выведенный тип, а не всякая
        // свёртка. Переопределение типа на каждой свёртке ломает вывод из сигнатуры
        // функции: `fn get32() -> [bit;32]` с `var val := get32();` даёт `[bit;8]`,
        // потому что вычислитель сворачивает вызов в `0`. Там ширину выбрал не операнд,
        // а объявленный возвращаемый тип, и трогать её нельзя.
        //
        // Расширяется только превышение верхней границы. Значение ниже нижней - домен:
        // `var u := ~0;` даёт `-1`, и беззнаковый тип обязан завернуть его в `255`, а
        // не расшириться до знакового.
        if untyped.contains(name)
            && let crate::parser::ast::Expression::Number(_, value) = &literal
            && exceeds_declared_upper(*value, declared_type(&folded[name]))
        {
            let widened = crate::semantic::type_inference::infer_int_type(*value);
            retype_declaration(folded.get_mut(name).expect("имя из этой же карты"), widened);
        }
        let literal = normalize_computed(literal, declared_type(&folded[name]));
        // Дробный результат, свёрнутый над `q(m, n)`, обязан быть понижен в целое
        // q-представление. Понижение литералов идёт при выводе типов, то есть до
        // свёртки, и `1.0 + 2.0` ему не литерал - оно проходит мимо. Без этого шага цель
        // `c` печатает `model->s = 3.0;` в поле `int8_t`, то есть 3, что в q(4, 4) значит
        // 0.1875, тогда как эталон даёт 3.0. Понижение делает тот же носитель, что и для
        // написанного автором литерала: своя копия разошлась бы с ним в округлении.
        let literal = lower_folded_fixed(literal, declared_type(&folded[name]), model)?;
        // Литерал обязан быть разрешён здесь: свёртка идёт последней, и разрешать
        // `Unresolved` после неё уже некому - потребители получили бы неразрешённый
        // узел, а он для них "не константа" (то есть ноль).
        let resolved = resolve_declaration_expression(ExpressionNode::Unresolved(literal), model)?;
        let slot = folded.get_mut(name).expect("имя взято из этой же карты");
        set_initializer(slot, resolved, loc);
    }
    Ok(folded)
}

/// Превышает ли вычисленное значение верхнюю границу выведенного типа.
///
/// Границы берутся у **единственного** их носителя
/// (`validate::literal_range::type_range`): своя копия разошлась бы с проверкой
/// `SE-089`, и расширение шло бы к одним границам, а отказ судил по другим. Тип без
/// границ (например `bit`) расширению не подлежит - его значения судит своя проверка.
///
/// Нижняя граница не проверяется намеренно: значение ниже неё - домен (беззнаковое
/// заворачивается `mod 2ⁿ`, знаковое остаётся ошибкой), и "расширение" подменило бы там
/// принятое решение.
fn exceeds_declared_upper(value: i128, ty: Option<&crate::semantic::type_node::TypeNode>) -> bool {
    match ty.and_then(crate::semantic::validate::literal_range::type_range) {
        Some((_, hi)) => value > hi,
        None => false,
    }
}

/// Задаёт тип объявления - для переменных, объявленных без типа.
///
/// Правится только запись в карте объявлений. Ячейка тела
/// (`Rc<RefCell<VariableNode>>`) к этому моменту ещё не построена - тела разрешаются
/// позже, - поэтому второго представления не существует и синхронизировать нечего.
fn retype_declaration(var: &mut VariableNode, ty: crate::semantic::type_node::TypeNode) {
    match var {
        VariableNode::Simple { ty: slot, .. } | VariableNode::Const { ty: slot, .. } => *slot = ty,
        VariableNode::Port { .. } | VariableNode::Unresolved => {}
    }
}

/// Понижает **свёрнутый** дробный литерал в целое q-представление.
///
/// Возвращает выражение как есть, если тип объявления не `q(m, n)` либо литерал не
/// дробный: понижать нечего.
///
/// Зовёт `lower_fixed_literal` - **тот же** носитель округления, которым понижается
/// литерал, написанный автором. Своя копия разошлась бы с ним, и `var s: q(4,4) := 1.0
/// + 2.0;` дало бы не то, что `var s: q(4,4) := 3.0;`.
fn lower_folded_fixed(
    literal: crate::parser::ast::Expression,
    ty: Option<&crate::semantic::type_node::TypeNode>,
    model: &Rc<RefCell<ModelNode>>,
) -> Result<crate::parser::ast::Expression, Diagnostic> {
    use crate::parser::ast::Expression as E;
    use crate::semantic::type_node::TypeNode;
    // Агрегат понижается поэлементно по типу элемента. Смотри правило только на
    // скалярный тип - и `var gains: [q(8, 8); 2] := {1.5, 2.5};` доедет до целей дробным
    // литералом: `cc -Werror` ответит "implicit conversion from 'double' to 'int16_t'
    // changes value from 1.5 to 1", `rustc` - `E0308`, `sv` - `SV-002`, при том что та
    // же запись скаляром работает у всех потребителей.
    if let (Some(TypeNode::Array(_, elem)), E::Initializer(loc, items) | E::Array(loc, items)) =
        (ty, &literal)
    {
        let mut lowered = Vec::with_capacity(items.len());
        for item in items {
            lowered.push(lower_folded_fixed(item.clone(), Some(elem), model)?);
        }
        return Ok(E::Initializer(*loc, lowered));
    }
    // Поля структуры понижаются по своим типам: объявление полей живёт в `ModelNode`, и
    // без него правило до них не доходило - `var g: Gains := {1.5, 2.5};` при `struct
    // Gains { kp: q(8, 8), ... }` отвергали ВСЕ цели, кроме диаграммы (`cc`, `iec2c`,
    // `rustc` - на порождённом файле, `sv` - своим `SV-002`), тогда как эталон запись
    // исполняет.
    if let (Some(TypeNode::Struct(struct_name)), E::Initializer(loc, items) | E::Array(loc, items)) =
        (ty, &literal)
        && let Some(def) = model.borrow().search_struct(struct_name)
    {
        let mut lowered = Vec::with_capacity(items.len());
        for (item, (_, field_ty)) in items.iter().zip(def.fields.iter()) {
            lowered.push(lower_folded_fixed(item.clone(), Some(field_ty), model)?);
        }
        // Полей может быть больше, чем значений: длину сверяет `SE-123`.
        for item in items.iter().skip(def.fields.len()) {
            lowered.push(item.clone());
        }
        return Ok(E::Initializer(*loc, lowered));
    }
    let (Some(TypeNode::Fixed { m, n, .. }), E::Rational(loc, text, negative)) = (ty, &literal)
    else {
        return Ok(literal);
    };
    let node = ExpressionNode::Rational(text.clone(), *negative);
    match crate::semantic::type_node::type_fixed::lower_fixed_literal(&node, *m, *n, *loc)? {
        Some(repr) => Ok(E::Number(*loc, repr)),
        None => Ok(literal),
    }
}

/// Тип объявления, если он у него есть.
fn declared_type(var: &VariableNode) -> Option<&crate::semantic::type_node::TypeNode> {
    match var {
        VariableNode::Simple { ty, .. } | VariableNode::Const { ty, .. } => Some(ty),
        VariableNode::Port { .. } | VariableNode::Unresolved => None,
    }
}

/// Нормирует **вычисленное** значение по типу объявления.
fn normalize_computed(
    literal: crate::parser::ast::Expression,
    ty: Option<&crate::semantic::type_node::TypeNode>,
) -> crate::parser::ast::Expression {
    use crate::parser::ast::Expression as E;
    let (E::Number(loc, value), Some(ty)) = (&literal, ty) else {
        return literal;
    };
    let Some((lo, hi)) = crate::semantic::validate::literal_range::type_range(ty) else {
        return literal;
    };
    // Беззнаковый тип узнаём по нижней границе: маска - сама верхняя граница (`2ⁿ -
    // 1`), поэтому ширину пересчитывать не нужно.
    if lo == 0 && (*value < lo || *value > hi) {
        return E::Number(*loc, *value & hi);
    }
    literal
}

/// Литерал ли выражение: сворачивать такое нечего.
///
/// Дробный литерал к моменту свёртки уже понижен в q-представление, поэтому "свёртка"
/// вернула бы его в дробный вид и отменила понижение - сверка Q-арифметики с целью `c`
/// это ловит.
pub(super) fn is_literal(expr: &crate::parser::ast::Expression) -> bool {
    use crate::parser::ast::Expression as E;
    matches!(
        expr,
        E::Number(..) | E::Bool(..) | E::Rational(..) | E::Duration(..) | E::String(..)
    )
}

/// Позиция объявления в исходнике - ключ сортировки "по тексту".
///
/// Синтезированные узлы (без позиции) идут последними: ссылаться на них инициализатору
/// всё равно нечем.
pub(super) fn declaration_position(var: &VariableNode) -> (u32, u32) {
    let loc = match var {
        VariableNode::Simple { loc, .. }
        | VariableNode::Const { loc, .. }
        | VariableNode::Port { loc, .. } => *loc,
        VariableNode::Unresolved => Location::Codegen,
    };
    match loc {
        Location::Source(file, start, _) => (file, start),
        _ => (u32::MAX, u32::MAX),
    }
}

/// Собирает идентификаторы выражения вместе с их позициями.
///
/// Разбор намеренно **не** исчерпывающий по `Expression`: интересны только имена, а
/// формы, их не содержащие, к делу не относятся. Пропущенная форма даёт прежнее
/// поведение (молчание), а не ложный отказ.
pub(super) fn collect_identifiers(
    expr: &crate::parser::ast::Expression,
    out: &mut Vec<(String, Location)>,
) {
    use crate::parser::ast::Expression;
    match expr {
        Expression::Variable(id) => out.push((id.name.clone(), id.loc)),
        Expression::Parenthesis(_, inner)
        | Expression::Not(_, inner)
        | Expression::BitwiseNot(_, inner)
        | Expression::UnaryPlus(_, inner)
        | Expression::Negate(_, inner)
        | Expression::Cast(_, inner, _) => collect_identifiers(inner, out),
        Expression::Power(_, l, r)
        | Expression::Multiply(_, l, r)
        | Expression::Divide(_, l, r)
        | Expression::Modulo(_, l, r)
        | Expression::Add(_, l, r)
        | Expression::Subtract(_, l, r)
        | Expression::ShiftLeft(_, l, r)
        | Expression::ShiftRight(_, l, r)
        | Expression::BitwiseAnd(_, l, r)
        | Expression::BitwiseXor(_, l, r)
        | Expression::BitwiseOr(_, l, r) => {
            collect_identifiers(l, out);
            collect_identifiers(r, out);
        }
        _ => {}
    }
}

/// Заменяет инициализатор узла, сохраняя всё остальное.
fn set_initializer(var: &mut VariableNode, init: ExpressionNode, _loc: Location) {
    match var {
        VariableNode::Simple { expr, .. } | VariableNode::Const { expr, .. } => *expr = init,
        VariableNode::Port { .. } | VariableNode::Unresolved => {}
    }
}

/// Готовит переменные модели: разрешение "сырых" выражений -> вывод типов -> свёртка
/// инициализаторов в литералы.
///
/// # Порядок обязателен, и он не очевиден
///
/// - **свёртка работает с сырым АСД**, поэтому исходные выражения запоминаются
///   до разрешения (разрешение их заменяет);
/// - **свёртка идёт последней**, уже после вывода типов: сверни раньше, и на
///   `var b: bit := false; var a := b;` вывод типов увидит булев литерал и даст
///   `a` тип `bool` вместо `bit`. Значение при этом верное, а тип - нет.
///
/// # Ошибки
///
/// Пробрасывает диагностику построения выражения (имя в инициализаторе не найдено в
/// области видимости), вывода типов и свёртки начального значения порта (`SE-094`).
pub(crate) fn prepare_variables(
    variables: &BTreeMap<String, VariableNode>,
    model: &Rc<RefCell<ModelNode>>,
) -> Result<BTreeMap<String, VariableNode>, Diagnostic> {
    let raw: BTreeMap<String, crate::parser::ast::Expression> = variables
        .iter()
        .filter_map(|(name, var)| match var {
            VariableNode::Simple { expr, .. } | VariableNode::Const { expr, .. } => match expr {
                ExpressionNode::Unresolved(source) => Some((name.clone(), source.clone())),
                _ => None,
            },
            VariableNode::Port { .. } | VariableNode::Unresolved => None,
        })
        .collect();

    let mut resolved = BTreeMap::new();
    for (name, var) in variables {
        resolved.insert(
            name.clone(),
            resolve_variable_expressions(var.clone(), model)?,
        );
    }

    // Имена, у которых тип не объявлен, снимаются до вывода типов: после него
    // `Inference` заменён выведенным, и отличить "автор выбрал ширину" от "ширину
    // выбрал литерал" уже нечем.
    let untyped: std::collections::BTreeSet<String> = resolved
        .iter()
        .filter(|(_, var)| {
            matches!(
                declared_type(var),
                Some(crate::semantic::type_node::TypeNode::Inference)
            )
        })
        .map(|(name, _)| name.clone())
        .collect();

    let inferred =
        crate::semantic::type_inference::type_inference(&mut resolved, Rc::clone(model))?;
    fold_variable_initializers(&inferred, &raw, model, &untyped)
}
