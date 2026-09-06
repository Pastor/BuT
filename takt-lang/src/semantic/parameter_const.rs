//! Вывод константности параметра модели -,.
//!
//! Параметр, которому в теле модели **ни разу не присваивают**, есть константа
//! (уточнение 2: константность **выводится**, а не объявляется). Выразить её можно
//! только там, где на каждую настройку приходится своя модель, - то есть в режиме
//! `--parameters=specialize` (п. 7): константа цели `c` - макрос **на модель**, и одно
//! объявление с двумя значениями невыразимо. В умолчании `assign` параметр остаётся
//! полем экземпляра, и проход не применяется.
//!
//! ## Две половины прохода
//!
//! 1. [`mark_mutated`] - **анализ изменяемости** по сырому АСД, стадия 0 (там,
//!    где АСД модели ещё под рукой). Результат - флаг
//!    [`ParameterNode::mutated`](crate::semantic::ParameterNode::mutated).
//! 2. [`constify_parameters`] - **применение**: `VariableNode::Simple` ->
//!    [`VariableNode::Const`] у параметров без присваиваний. Зовётся из
//!    `construct_stages` сразу за специализацией, **до стадии 2**.
//!
//! **Порядок "до стадии 2" несущий.** Тела разрешаются стадиями 2-6, и
//! ссылка на переменную в теле - **своя** `Rc`-ячейка, склонированная из карты
//! `model.variables` в момент разрешения (`semantic/expression/mod.rs`). Пока
//! стадия 2 не прошла, ячеек не существует: правки карты достаточно. Флип
//! **после** разрешения тел потребовал бы мутировать оба представления - ровно
//! засада 0096 (`float` объявлен `q`, а арифметика `float`). Второй довод той же
//! силы: `after PARAM` и адрес порта спрашивают у объявления
//! "константа ли", и спрашивают **на стадиях 2-3** - опоздав, проход дал бы
//! вычисляемую выдержку вместо константной.
//!
//! ## Направление ошибки выбрано осознанно
//!
//! Анализ сопоставляет **имена**, а не объявления: локальная `var gain` в блоке
//! затеняет параметр `gain`, и присваивание затеняющей переменной пометит параметр
//! изменяемым. Это ошибка в **безопасную** сторону: параметр останется полем - форма
//! вывода проще, чем могла быть, поведение прежнее. Обратная ошибка (изменяемый
//! параметр объявлен константой) сделала бы вывод **молча неверным** - класс дефекта
//! 0184, и ради его невозможности точностью здесь пренебрегаем. По той же причине не
//! взят слой [`usages`](crate::semantic::usages): он различает затенение (символ =
//! позиция объявляющего имени), но **не обходит импорты**, и полагаться на его полноту
//! там, где цена ошибки - неверный вывод, оснований нет.
//!
//! Разбор АСД здесь **исчерпывающий** (`deny(clippy::wildcard_enum_match_arm)`): новая
//! форма присваивания, не разобранная проходом, сделала бы изменяемый параметр
//! константой. Пропуск обязан валить сборку, а не вывод.
#![deny(clippy::wildcard_enum_match_arm)]

use crate::diagnostics::{Diagnostic, Location};
use crate::parser::ast;
use crate::semantic::{ModelNode, ParameterNode, VariableNode};
use std::cell::RefCell;
use std::collections::BTreeSet;
use std::rc::Rc;

/// Результат анализа изменяемости одной модели.
struct Scan {
    /// Имена, которым в поддереве модели присваивают.
    assigned: BTreeSet<String>,
    /// Разобран ли АСД целиком. `false` - встретился узел ошибки разбора
    /// (восстановление парсера, 0152): полноту обещать нельзя, и все параметры
    /// объявляются изменяемыми.
    complete: bool,
}

/// Отмечает параметры, которым в теле модели присваивают (стадия 0).
///
/// Обходится **поддерево** АСД модели - её собственные тела и тела вложенных моделей:
/// видимость имён в языке направлена вверх по цепочке `upper`, поэтому вложенная модель
/// до параметра родителя дотянуться может, а посторонняя - нет.
pub(crate) fn mark_mutated(model: &ast::Model, parameters: &mut [ParameterNode]) {
    if parameters.is_empty() {
        return;
    }
    let mut scan = Scan {
        assigned: BTreeSet::new(),
        complete: true,
    };
    scan_model(model, &mut scan);
    for parameter in parameters {
        parameter.mutated = !scan.complete || scan.assigned.contains(&parameter.name);
    }
}

/// Заменяет объявление параметра без присваиваний на константу - режим
/// `--parameters=specialize`.
///
/// Обходит **всё** дерево: константность выводится и у специализаций, и у исходных
/// моделей, инстанцированных без аргументов (у такой параметр держит значение по
/// умолчанию, и оно тоже compile-time).
pub(crate) fn constify_parameters(root: &Rc<RefCell<ModelNode>>) {
    let names: Vec<String> = root
        .borrow()
        .parameters
        .iter()
        .filter(|p| !p.mutated)
        .map(|p| p.name.clone())
        .collect();
    {
        let mut model = root.borrow_mut();
        for name in names {
            let Some(var) = model.variables.get(&name) else {
                continue;
            };
            let VariableNode::Simple {
                upper,
                loc,
                name: var_name,
                ty,
                expr,
            } = var.clone()
            else {
                // Параметр в дереве - всегда `Simple` (`declaration.rs`). Иное
                // означает, что объявление уже кем-то заменено: молча перезаписывать
                // чужую работу нельзя.
                continue;
            };
            model.variables.insert(
                name,
                VariableNode::Const {
                    upper,
                    loc,
                    name: var_name,
                    ty,
                    expr,
                },
            );
        }
    }
    let nested: Vec<Rc<RefCell<ModelNode>>> =
        root.borrow().models.values().map(Rc::clone).collect();
    for sub in nested {
        constify_parameters(&sub);
    }
}

/// Является ли объявление параметром своей модели.
///
/// Спрашивается у **самого объявления** (`upper` -> список параметров модели), а не
/// поиском по области видимости: одноимённые переменные разных моделей - норма, и поиск
/// по имени ответил бы про чужую.
pub(crate) fn is_parameter(var: &VariableNode) -> bool {
    let Some(owner) = var.upper() else {
        return false;
    };
    let name = var.name().to_string();
    owner.borrow().parameters.iter().any(|p| p.name == name)
}

/// `SE-088` - параметр там, где нужна величина, известная при генерации.
///
/// Сообщение **называет режим** (п. 8): в умолчании `assign` параметр - поле
/// экземпляра, то есть величина такта, и молчаливая деградация (вычисляемая выдержка
/// 0183, усечение ширины, потеря адреса) запрещена -, где потерянный адрес
/// диагностировался следствием (`SE-052`), а не причиной.
pub(crate) fn compile_time_parameter(loc: Location, name: &str, position: &str) -> Diagnostic {
    Diagnostic::error(loc, compile_time_parameter_text(name, position)).with_code("SE-088")
}

/// Текст `SE-088` отдельно от диагностики: потребитель, у которого своя воронка причин
/// (`after_const`), собирает сообщение сам, но **этим** текстом - одна формулировка на
/// все позиции.
pub(crate) fn compile_time_parameter_text(name: &str, position: &str) -> String {
    format!(
        "{position}: '{name}' — параметр модели, а здесь нужна величина, \
         известная при генерации; параметр является константой только при сборке \
         с '--parameters=specialize' (в режиме по умолчанию '--parameters=assign' \
         параметр — поле экземпляра)"
    )
}

/// Код диагностики `SE-088` - для потребителя, собирающего `Diagnostic` сам.
pub(crate) const COMPILE_TIME_PARAMETER_CODE: &str = "SE-088";

// --- Обход АСД ---------------------------------------------------------------

/// Модель: свои элементы и вложенные модели.
fn scan_model(model: &ast::Model, scan: &mut Scan) {
    for element in &model.elements {
        scan_element(element, scan);
    }
}

fn scan_element(element: &ast::ModelElement, scan: &mut Scan) {
    match element {
        // Вставка уровня модели - обычные операторы Takt: присваивание в ней делает
        // параметр изменяемым так же, как в любом теле.
        ast::ModelElement::Assembly(block) => scan_statement(block, scan),
        ast::ModelElement::Variable(def) => {
            if let Some(init) = variable_initializer(def) {
                scan_expression(init, scan);
            }
        }
        ast::ModelElement::Function(def) => {
            // Тело функции обходится независимо от того, зовут ли её: неверный ответ
            // "константа" дороже лишней отестности.
            if let Some(body) = &def.body {
                scan_statement(body, scan);
            }
        }
        ast::ModelElement::State(def) => scan_state(def, scan),
        ast::ModelElement::Model(nested) => scan_model(nested, scan),
        ast::ModelElement::NamedBlockCode(def) => scan_statement(&def.statement, scan),
        ast::ModelElement::Address(def) => scan_expression(&def.value, scan),
        // Условия, формулы и инварианты присваивания не содержат: `=` в условии -
        // равенство. Вызов функции в условии тоже безопасен: её тело разобрано как
        // элемент модели.
        ast::ModelElement::Condition(_)
        | ast::ModelElement::Invariant(_)
        | ast::ModelElement::Formula(_)
        | ast::ModelElement::InlineFormula(_)
        | ast::ModelElement::Type(_)
        | ast::ModelElement::Enum(_)
        | ast::ModelElement::Struct(_)
        | ast::ModelElement::Clock(_)
        | ast::ModelElement::StraySemicolon(_) => {}
        // Импортированный файл не обходится: его модели остаются в своей цепочке
        // `upper`, и до параметров импортёра их тела не дотягиваются. Параметры самого
        // импортированного файла размечаются его собственной стадией 0.
        ast::ModelElement::Import(_) => {}
    }
}

fn scan_state(state: &ast::StateDefine, scan: &mut Scan) {
    if let Some(implements) = &state.implements {
        scan_expression(implements, scan);
    }
    for element in &state.elements {
        match element {
            ast::StateElement::NamedBlockCode(def) => scan_statement(&def.statement, scan),
            ast::StateElement::Every(def) => scan_statement(&def.body, scan),
            // Вставка уровня состояния - те же операторы Takt.
            ast::StateElement::Assembly(block) => scan_statement(block, scan),
            ast::StateElement::Next(_)
            | ast::StateElement::Reference(_, _, _)
            | ast::StateElement::InlineFormula(_)
            | ast::StateElement::Invariant(_)
            // Формула - обязательство внешнему анализатору: операторов в ней нет, и
            // параметра она не меняет.
            | ast::StateElement::Formula(_)
            | ast::StateElement::StraySemicolon(_) => {}
        }
    }
}

fn scan_statement(stmt: &ast::Statement, scan: &mut Scan) {
    match stmt {
        ast::Statement::Block { statements, .. } => {
            for s in statements {
                scan_statement(s, scan);
            }
        }
        ast::Statement::If(_, cond, then_, else_) => {
            scan_expression(cond, scan);
            scan_statement(then_, scan);
            if let Some(else_) = else_ {
                scan_statement(else_, scan);
            }
        }
        ast::Statement::Loop(_, cond, body, _) => {
            if let Some(cond) = cond {
                scan_expression(cond, scan);
            }
            scan_statement(body, scan);
        }
        ast::Statement::For(_, init, cond, step, body) => {
            if let Some(init) = init {
                scan_statement(init, scan);
            }
            if let Some(cond) = cond {
                scan_expression(cond, scan);
            }
            if let Some(step) = step {
                scan_expression(step, scan);
            }
            if let Some(body) = body {
                scan_statement(body, scan);
            }
        }
        ast::Statement::Expression(_, expr) => scan_expression(expr, scan),
        ast::Statement::Variable(_, def, init) => {
            if let Some(expr) = variable_initializer(def) {
                scan_expression(expr, scan);
            }
            if let Some(expr) = init {
                scan_expression(expr, scan);
            }
        }
        ast::Statement::Return(_, expr) => {
            if let Some(expr) = expr {
                scan_expression(expr, scan);
            }
        }
        ast::Statement::Match(_, subject, arms) => {
            scan_expression(subject, scan);
            for arm in arms {
                for pattern in &arm.patterns {
                    match pattern {
                        ast::MatchPattern::Value(expr) => scan_expression(expr, scan),
                        ast::MatchPattern::Wildcard(_) => {}
                    }
                }
                scan_statement(&arm.body, scan);
            }
        }
        ast::Statement::Args(_, args) => {
            for arg in args {
                scan_expression(&arg.expr, scan);
            }
        }
        // Ассемблерная вставка непрозрачна для языка - но именно поэтому она и опасна:
        // что она делает с памятью, проход не знает. Пока вставка есть, константности
        // не выводим вовсе.
        ast::Statement::Assembly { .. } => scan.complete = false,
        ast::Statement::Formula { .. }
        | ast::Statement::InlineFormula(_)
        | ast::Statement::Continue(_)
        | ast::Statement::Break(_)
        | ast::Statement::StraySemicolon(_) => {}
        // Узел восстановления парсера: текст разобран не весь.
        ast::Statement::Error(_) => scan.complete = false,
    }
}

fn scan_expression(expr: &ast::Expression, scan: &mut Scan) {
    match expr {
        ast::Expression::Assign(_, target, value) => {
            if let Some(name) = assigned_name(target) {
                scan.assigned.insert(name);
            } else {
                // Цель присваивания - форма, у которой корневого имени нет (например,
                // результат вызова). Разобрать её проход не умеет.
                scan.complete = false;
            }
            scan_expression(target, scan);
            scan_expression(value, scan);
        }
        ast::Expression::Function(_, _, args) => {
            for arg in args {
                scan_expression(arg, scan);
            }
        }
        ast::Expression::ArraySubscript(_, _, index) => scan_expression(index, scan),
        ast::Expression::NamedFunction(_, callee, args) => {
            scan_expression(callee, scan);
            for arg in args {
                scan_expression(&arg.expr, scan);
            }
        }
        ast::Expression::CodeBlock(_, inner, body) => {
            scan_expression(inner, scan);
            scan_statement(body, scan);
        }
        ast::Expression::BitAccess(_, base, _) => scan_expression(base, scan),
        ast::Expression::Cast(_, inner, _) => scan_expression(inner, scan),
        ast::Expression::Parenthesis(_, inner)
        | ast::Expression::Not(_, inner)
        | ast::Expression::BitwiseNot(_, inner)
        | ast::Expression::UnaryPlus(_, inner)
        | ast::Expression::Negate(_, inner) => scan_expression(inner, scan),
        ast::Expression::Power(_, lhs, rhs)
        | ast::Expression::Multiply(_, lhs, rhs)
        | ast::Expression::Divide(_, lhs, rhs)
        | ast::Expression::Modulo(_, lhs, rhs)
        | ast::Expression::Add(_, lhs, rhs)
        | ast::Expression::Subtract(_, lhs, rhs)
        | ast::Expression::ShiftLeft(_, lhs, rhs)
        | ast::Expression::ShiftRight(_, lhs, rhs)
        | ast::Expression::BitwiseAnd(_, lhs, rhs)
        | ast::Expression::BitwiseXor(_, lhs, rhs)
        | ast::Expression::BitwiseOr(_, lhs, rhs)
        | ast::Expression::Less(_, lhs, rhs)
        | ast::Expression::More(_, lhs, rhs)
        | ast::Expression::LessEqual(_, lhs, rhs)
        | ast::Expression::MoreEqual(_, lhs, rhs)
        | ast::Expression::Equal(_, lhs, rhs)
        | ast::Expression::NotEqual(_, lhs, rhs)
        | ast::Expression::And(_, lhs, rhs)
        | ast::Expression::Or(_, lhs, rhs) => {
            scan_expression(lhs, scan);
            scan_expression(rhs, scan);
        }
        ast::Expression::ConditionalOperator(_, cond, then_, else_) => {
            scan_expression(cond, scan);
            scan_expression(then_, scan);
            scan_expression(else_, scan);
        }
        ast::Expression::Array(_, items) | ast::Expression::Initializer(_, items) => {
            for item in items {
                scan_expression(item, scan);
            }
        }
        // Имена и литералы присваивания не содержат.
        ast::Expression::Variable(_)
        | ast::Expression::ArraySlice(_, _, _, _)
        | ast::Expression::List(_, _)
        | ast::Expression::Type(_, _)
        | ast::Expression::Number(_, _)
        | ast::Expression::Duration(_, _, _)
        | ast::Expression::Rational(_, _, _)
        | ast::Expression::String(_)
        | ast::Expression::Address(_, _, _)
        // Обращение по адресу имён не содержит: параметра в нём нет.
        | ast::Expression::AnonAddress(_, _, _)
        | ast::Expression::Bool(_, _) => {}
    }
}

/// Корневое имя цели присваивания.
///
/// `x := 1`, `x.0 := 1`, `x[i] := 1`, `(x) := 1` - все они меняют `x`: запись в бит или
/// элемент есть изменение объявления, и параметр после неё константой быть не может.
fn assigned_name(target: &ast::Expression) -> Option<String> {
    match target {
        ast::Expression::Variable(id) => Some(id.name.clone()),
        // База - выражение: имя ищется в ней рекурсивно.
        ast::Expression::ArraySubscript(_, base, _) | ast::Expression::ArraySlice(_, base, _, _) => {
            assigned_name(base)
        }
        ast::Expression::Parenthesis(_, inner) | ast::Expression::BitAccess(_, inner, _) => {
            assigned_name(inner)
        }
        // Прочие формы целью присваивания быть не могут (проверяет семантика); "имени
        // нет" здесь означает "разобрать не сумели" - вызывающий отказывается от вывода
        // константности.
        ast::Expression::Assign(_, _, _)
        | ast::Expression::Function(_, _, _)
        | ast::Expression::CodeBlock(_, _, _)
        | ast::Expression::NamedFunction(_, _, _)
        | ast::Expression::Not(_, _)
        | ast::Expression::BitwiseNot(_, _)
        | ast::Expression::UnaryPlus(_, _)
        | ast::Expression::Negate(_, _)
        | ast::Expression::Power(_, _, _)
        | ast::Expression::Multiply(_, _, _)
        | ast::Expression::Divide(_, _, _)
        | ast::Expression::Modulo(_, _, _)
        | ast::Expression::Add(_, _, _)
        | ast::Expression::Subtract(_, _, _)
        | ast::Expression::ShiftLeft(_, _, _)
        | ast::Expression::ShiftRight(_, _, _)
        | ast::Expression::BitwiseAnd(_, _, _)
        | ast::Expression::BitwiseXor(_, _, _)
        | ast::Expression::BitwiseOr(_, _, _)
        | ast::Expression::Less(_, _, _)
        | ast::Expression::More(_, _, _)
        | ast::Expression::LessEqual(_, _, _)
        | ast::Expression::MoreEqual(_, _, _)
        | ast::Expression::Equal(_, _, _)
        | ast::Expression::NotEqual(_, _, _)
        | ast::Expression::And(_, _, _)
        | ast::Expression::Or(_, _, _)
        | ast::Expression::ConditionalOperator(_, _, _, _)
        | ast::Expression::Cast(_, _, _)
        | ast::Expression::Array(_, _)
        | ast::Expression::Initializer(_, _)
        | ast::Expression::List(_, _)
        | ast::Expression::Type(_, _)
        | ast::Expression::Number(_, _)
        | ast::Expression::Duration(_, _, _)
        | ast::Expression::Rational(_, _, _)
        | ast::Expression::String(_)
        | ast::Expression::Address(_, _, _)
        // Цель записи - ячейка, а не имя: корневого имени у неё нет.
        | ast::Expression::AnonAddress(_, _, _)
        | ast::Expression::Bool(_, _) => None,
    }
}

/// Инициализатор объявления значения (`var`/`const`/`parameter`/порт).
fn variable_initializer(def: &ast::VariableDefine) -> Option<&ast::Expression> {
    match def {
        ast::VariableDefine::Variable { initializer, .. } => initializer.as_ref(),
        ast::VariableDefine::Constant { initializer, .. } => Some(initializer),
        ast::VariableDefine::Parameter { initializer, .. } => Some(initializer),
        ast::VariableDefine::Port { initializer, .. } => initializer.as_ref(),
    }
}
