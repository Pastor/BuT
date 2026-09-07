//! Обход АСД: сбор вхождений имён.
//!
//! ## Правило модуля: никаких `_ =>` по узлам языка
//!
//! Разбор узлов АСД **исчерпывающий**. Новый узел языка обязан валить сборку этого
//! модуля, а не молча выпадать из покрытия: пропущенное вхождение - это испорченный
//! переименованием исходник. Тот же приём охраняет вычислитель симулятора
//! (`takt-sim/src/eval/`) и печать форматтера.
//!
//! Ветки, дописанные ради узлов **без имён** (литералы, `break`, `continue`),
//! перечисляются явно - так видно, что узел рассмотрен, а не забыт.
#![deny(clippy::wildcard_enum_match_arm)]

use super::scope::{Namespace, Scopes, Symbol, SymbolKind};
use super::{UnresolvedName, Usage, UsageKind, UsageTable, name_range};
use crate::parser::ast;

/// Порядок разрешения имени в позиции значения.
///
/// Переменная имеет преимущество над именованным условием - тот же порядок, что у
/// перехода к декларации; вариант перечисления живёт в том же пространстве.
const VALUE_SPACES: &[Namespace] = &[Namespace::Value, Namespace::Condition];

/// Обходит корневую модель файла.
pub(super) fn walk_root(root: &ast::Model, table: &mut UsageTable) {
    let mut scopes = Scopes::default();
    // Предпроход: члены каждой модели - для формы `S(Ping) = End`, которая адресует
    // состояние соседней модели (в стеке областей её нет).
    register_members(root, &mut scopes);
    walk_model(root, &mut scopes, table);
}

/// Предпроход: регистрирует члены каждой именованной модели файла.
fn register_members(model: &ast::Model, scopes: &mut Scopes) {
    if let Some(model_name) = &model.name
        && let Some((file_no, start, _)) = name_range(model_name.loc)
    {
        let owner = super::SymbolId { file_no, start };
        each_declaration(model, &mut |name, kind| {
            if let Some((file_no, start, _)) = name_range(name.loc) {
                scopes.declare_member(
                    owner,
                    &name.name,
                    Symbol {
                        id: super::SymbolId { file_no, start },
                        kind,
                    },
                );
            }
        });
    }
    for element in &model.elements {
        if let ast::ModelElement::Model(nested) = element {
            register_members(nested, scopes);
        }
    }
}

/// Обходит модель: сперва объявляет её элементы, затем разбирает тела.
///
/// Два прохода нужны потому, что внутри модели порядок объявлений значения не имеет:
/// `always { speed := 1; }` может стоять выше `var speed`.
fn walk_model(model: &ast::Model, scopes: &mut Scopes, table: &mut UsageTable) {
    scopes.push_model();
    declare_elements(model, scopes, table);
    for element in &model.elements {
        walk_element(element, scopes, table);
    }
    if let Some(implements) = &model.implements {
        // `model M = A | B;` - имена под-моделей.
        walk_expression(implements, scopes, table);
    }
    scopes.pop_model();
}

/// Первый проход: объявления модели.
fn declare_elements(model: &ast::Model, scopes: &mut Scopes, table: &mut UsageTable) {
    if let Some(name) = &model.name {
        declare(name, SymbolKind::Model, scopes, table, DeclareIn::Model);
    }
    // Типы объявлений перечисляются в одном месте (`each_declaration`): тот же перечень
    // нужен предпроходу членов модели, и разъехавшись, они дали бы разный ответ на "что
    // объявлено в модели".
    let mut declared: Vec<(&ast::Identifier, SymbolKind)> = Vec::new();
    each_declaration(model, &mut |name, kind| declared.push((name, kind)));
    for (name, kind) in declared {
        declare(name, kind, scopes, table, DeclareIn::Model);
    }
    // Типы объявлений переменных обходятся отдельно: у них есть ещё и ссылка на тип
    // (`var x: MyAlias`), а это использование имени.
    for element in &model.elements {
        if let ast::ModelElement::Variable(def) = element {
            walk_variable_type(def, scopes, table);
        }
    }
}

/// Перечисляет объявления модели (без вложенных моделей).
///
/// Единый источник ответа на вопрос "какие имена объявляет эта модель": используется и
/// при построении области видимости, и при регистрации членов для формы `S(Модель) =
/// Состояние`.
fn each_declaration<'a>(
    model: &'a ast::Model,
    f: &mut impl FnMut(&'a ast::Identifier, SymbolKind),
) {
    for element in &model.elements {
        match element {
            ast::ModelElement::Variable(def) => {
                if let (Some(name), kind) = variable_name_and_kind(def, DeclareIn::Model) {
                    f(name, kind);
                }
            }
            ast::ModelElement::Function(def) => {
                if let Some(name) = &def.name {
                    f(name, SymbolKind::Function);
                }
            }
            ast::ModelElement::Condition(def) => {
                if let Some(name) = &def.name {
                    f(name, SymbolKind::Condition);
                }
            }
            ast::ModelElement::Invariant(def) => {
                if let Some(name) = &def.name {
                    f(name, SymbolKind::Condition);
                }
            }
            ast::ModelElement::Type(def) => f(&def.name, SymbolKind::TypeAlias),
            ast::ModelElement::Enum(def) => {
                if let Some(name) = &def.name {
                    f(name, SymbolKind::Enum);
                }
                for variant in &def.variants {
                    f(&variant.name, SymbolKind::EnumVariant);
                }
            }
            ast::ModelElement::Struct(def) => {
                if let Some(name) = &def.name {
                    f(name, SymbolKind::Struct);
                }
            }
            ast::ModelElement::State(def) => {
                if let Some(name) = &def.name {
                    f(name, SymbolKind::State);
                }
                // Инвариант состояния объявляет имя на уровне модели (десахаризация
                // `invariant` в пару `cond` + `Guard`).
                for state_element in &def.elements {
                    if let ast::StateElement::Invariant(inv) = state_element
                        && let Some(name) = &inv.name
                    {
                        f(name, SymbolKind::Condition);
                    }
                }
            }
            ast::ModelElement::Model(nested) => {
                if let Some(name) = &nested.name {
                    f(name, SymbolKind::Model);
                }
            }
            ast::ModelElement::Import(def) => each_import_binding(def, f),
            // Объявлений не вводят - разбираются вторым проходом.
            ast::ModelElement::Assembly(_)
            | ast::ModelElement::Formula(_)
            | ast::ModelElement::NamedBlockCode(_)
            | ast::ModelElement::InlineFormula(_)
            | ast::ModelElement::Address(_)
            | ast::ModelElement::Clock(_)
            | ast::ModelElement::StraySemicolon(_) => {}
        }
    }
}

/// Второй проход: тела и ссылки.
fn walk_element(element: &ast::ModelElement, scopes: &mut Scopes, table: &mut UsageTable) {
    match element {
        ast::ModelElement::Variable(def) => {
            for expr in variable_value_expressions(def) {
                walk_expression(expr, scopes, table);
            }
        }
        ast::ModelElement::Function(def) => walk_function(def, scopes, table),
        // Вставка уровня модели: её операторы - обычные тела, и имена в них обязаны
        // попасть в слой использований (иначе поедет `rename`).
        ast::ModelElement::Assembly(block) => walk_statement(block, scopes, table),
        ast::ModelElement::Condition(def) => walk_condition(&def.value, scopes, table),
        ast::ModelElement::Invariant(def) => walk_condition(&def.value, scopes, table),
        ast::ModelElement::State(def) => walk_state(def, scopes, table),
        ast::ModelElement::Model(nested) => walk_model(nested, scopes, table),
        ast::ModelElement::NamedBlockCode(def) => {
            scopes.push_local();
            walk_statement(&def.statement, scopes, table);
            scopes.pop_local();
        }
        ast::ModelElement::Address(def) => {
            // `address PORT = 0x...;` - ссылка на порт по имени.
            if let Some(name) = &def.name {
                reference(name, &[Namespace::Value], scopes, table);
            }
            walk_expression(&def.value, scopes, table);
        }
        ast::ModelElement::Formula(def) => walk_formula_block(&def.formula, scopes, table),
        ast::ModelElement::InlineFormula(def) => walk_inline_formula(def, scopes, table),
        // Типы и перечисления ссылок на имена не содержат, кроме псевдонима на другой
        // тип - он разбирается ниже. Само объявляемое имя тоже стоит в позиции типа:
        // иначе `type Celsius = u8;` красил бы тип только справа от `=`.
        ast::ModelElement::Type(def) => {
            mark_declared_type_name(Some(&def.name), table);
            walk_type(&def.ty, scopes, table);
        }
        ast::ModelElement::Struct(def) => {
            mark_declared_type_name(def.name.as_ref(), table);
            for field in &def.fields {
                walk_type(&field.ty, scopes, table);
            }
        }
        ast::ModelElement::Import(def) => note_import_originals(def, table),
        // Имя перечисления - тоже имя типа; варианты именами типов не являются и
        // остаются `enumMember`.
        ast::ModelElement::Enum(def) => mark_declared_type_name(def.name.as_ref(), table),
        // `clock 1kHz;` - литерал частоты, имён не содержит.
        ast::ModelElement::Clock(_) | ast::ModelElement::StraySemicolon(_) => {}
    }
}

/// Состояние: имя, рёбра, вложенные блоки, формулы.
fn walk_state(state: &ast::StateDefine, scopes: &mut Scopes, table: &mut UsageTable) {
    if let Some(implements) = &state.implements {
        walk_expression(implements, scopes, table);
    }
    for element in &state.elements {
        match element {
            ast::StateElement::Next(name) => {
                reference(name, &[Namespace::State], scopes, table);
            }
            ast::StateElement::Reference(_, name, cond) => {
                reference(name, &[Namespace::State], scopes, table);
                if let Some(cond) = cond {
                    walk_condition(cond, scopes, table);
                }
            }
            ast::StateElement::NamedBlockCode(def) => {
                scopes.push_local();
                walk_statement(&def.statement, scopes, table);
                scopes.pop_local();
            }
            ast::StateElement::InlineFormula(def) => walk_inline_formula(def, scopes, table),
            ast::StateElement::Invariant(def) => {
                if let Some(name) = &def.name {
                    declare(name, SymbolKind::Condition, scopes, table, DeclareIn::Model);
                }
                walk_condition(&def.value, scopes, table);
            }
            // `every 100ms { ... }` - тело обходится как у именованного блока:
            // пропустить его значило бы потерять использования имён внутри.
            ast::StateElement::Every(def) => {
                scopes.push_local();
                walk_statement(&def.body, scopes, table);
                scopes.pop_local();
            }
            // Вставка уровня состояния - тело со своей областью, как у именованного
            // блока: имена в ней обязаны попасть в слой использований, иначе `rename`
            // их не увидит.
            ast::StateElement::Assembly(block) => {
                scopes.push_local();
                walk_statement(block, scopes, table);
                scopes.pop_local();
            }
            // Формула - обязательство внешнему анализатору: её текст компилятор не
            // разбирает и имён в нём не связывает.
            ast::StateElement::Formula(_) | ast::StateElement::StraySemicolon(_) => {}
        }
    }
}

/// Функция: параметры объявляются в своей локальной области, затем тело.
fn walk_function(def: &ast::FunctionDefine, scopes: &mut Scopes, table: &mut UsageTable) {
    scopes.push_local();
    for (_, param) in &def.params {
        let Some(param) = param else { continue };
        if let Some(name) = &param.name {
            declare(name, SymbolKind::Parameter, scopes, table, DeclareIn::Local);
        }
        // Тип параметра грамматика разбирает как выражение (`ParameterTypeExpr`), а не
        // как `Type`, поэтому обход типов сюда не доходит. Позицию типа отмечаем здесь;
        // вхождением имя не делаем - это изменило бы поведение `rename`/`references`, а
        // предмет фичи только подсветка.
        mark_type_expression(&param.ty, table);
    }
    if let Some(ty) = &def.return_type {
        walk_type(ty, scopes, table);
    }
    if let Some(body) = &def.body {
        walk_statement(body, scopes, table);
    }
    scopes.pop_local();
}

/// Оператор.
fn walk_statement(stmt: &ast::Statement, scopes: &mut Scopes, table: &mut UsageTable) {
    match stmt {
        ast::Statement::Block { statements, .. } => {
            scopes.push_local();
            for s in statements {
                walk_statement(s, scopes, table);
            }
            scopes.pop_local();
        }
        ast::Statement::If(_, cond, then_, else_) => {
            walk_expression(cond, scopes, table);
            walk_statement(then_, scopes, table);
            if let Some(else_) = else_ {
                walk_statement(else_, scopes, table);
            }
        }
        ast::Statement::Loop(_, cond, body, _) => {
            if let Some(cond) = cond {
                walk_expression(cond, scopes, table);
            }
            walk_statement(body, scopes, table);
        }
        ast::Statement::For(_, init, cond, step, body) => {
            scopes.push_local();
            if let Some(init) = init {
                walk_statement(init, scopes, table);
            }
            if let Some(cond) = cond {
                walk_expression(cond, scopes, table);
            }
            if let Some(step) = step {
                walk_expression(step, scopes, table);
            }
            if let Some(body) = body {
                walk_statement(body, scopes, table);
            }
            scopes.pop_local();
        }
        ast::Statement::Expression(_, expr) => walk_expression(expr, scopes, table),
        ast::Statement::Variable(_, def, init) => {
            // Инициализатор разбирается до объявления: `var x := x;` справа - ещё
            // внешнее `x` (объявление начинает действовать после оператора).
            for expr in variable_value_expressions(def) {
                walk_expression(expr, scopes, table);
            }
            if let Some(init) = init {
                walk_expression(init, scopes, table);
            }
            declare_variable(def, scopes, table, DeclareIn::Local);
        }
        ast::Statement::Return(_, expr) => {
            if let Some(expr) = expr {
                walk_expression(expr, scopes, table);
            }
        }
        ast::Statement::Match(_, subject, arms) => {
            walk_expression(subject, scopes, table);
            for arm in arms {
                for pattern in &arm.patterns {
                    match pattern {
                        ast::MatchPattern::Value(expr) => walk_expression(expr, scopes, table),
                        ast::MatchPattern::Wildcard(_) => {}
                    }
                }
                walk_statement(&arm.body, scopes, table);
            }
        }
        ast::Statement::Args(_, args) => {
            for arg in args {
                walk_expression(&arg.expr, scopes, table);
            }
        }
        ast::Statement::Formula { block, .. } => walk_formula_block(block, scopes, table),
        ast::Statement::InlineFormula(def) => walk_inline_formula(def, scopes, table),
        // Ассемблерная вставка непрозрачна для языка: имён Takt в ней нет.
        ast::Statement::Assembly { .. } => {}
        ast::Statement::Continue(_)
        | ast::Statement::Break(_)
        | ast::Statement::StraySemicolon(_) => {}
        // Узел ошибки разбора: текст неполон, полноту обещать нельзя.
        ast::Statement::Error(loc) => table.push_unsupported(*loc),
    }
}

/// Выражение.
fn walk_expression(expr: &ast::Expression, scopes: &mut Scopes, table: &mut UsageTable) {
    match expr {
        ast::Expression::Variable(name) => reference(name, VALUE_SPACES, scopes, table),
        ast::Expression::Function(_, name, args) => {
            reference(name, &[Namespace::Callable], scopes, table);
            for arg in args {
                walk_expression(arg, scopes, table);
            }
        }
        // База - выражение: вхождения в ней ищет тот же обход, а не `reference` по
        // имени.
        ast::Expression::ArraySubscript(_, base, index) => {
            walk_expression(base, scopes, table);
            walk_expression(index, scopes, table);
        }
        ast::Expression::ArraySlice(_, base, _, _) => walk_expression(base, scopes, table),
        ast::Expression::BitAccess(_, base, member) => {
            walk_expression(base, scopes, table);
            // Член - это поле структуры или номер бита, а не самостоятельное имя
            // области видимости: разрешать его нечем и не нужно.
            match member {
                ast::Member::Identifier(_) | ast::Member::Number(_) => {}
            }
        }
        ast::Expression::NamedFunction(_, callee, args) => {
            walk_expression(callee, scopes, table);
            for arg in args {
                walk_expression(&arg.expr, scopes, table);
            }
        }
        ast::Expression::CodeBlock(_, expr, body) => {
            walk_expression(expr, scopes, table);
            walk_statement(body, scopes, table);
        }
        ast::Expression::Cast(_, expr, ty) => {
            walk_expression(expr, scopes, table);
            walk_type(ty, scopes, table);
        }
        ast::Expression::Type(_, ty) => walk_type(ty, scopes, table),
        ast::Expression::Parenthesis(_, inner)
        | ast::Expression::Not(_, inner)
        | ast::Expression::BitwiseNot(_, inner)
        | ast::Expression::UnaryPlus(_, inner)
        | ast::Expression::Negate(_, inner) => walk_expression(inner, scopes, table),
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
        | ast::Expression::Or(_, lhs, rhs)
        | ast::Expression::Assign(_, lhs, rhs) => {
            walk_expression(lhs, scopes, table);
            walk_expression(rhs, scopes, table);
        }
        ast::Expression::ConditionalOperator(_, cond, then_, else_) => {
            walk_expression(cond, scopes, table);
            walk_expression(then_, scopes, table);
            walk_expression(else_, scopes, table);
        }
        ast::Expression::Array(_, items) | ast::Expression::Initializer(_, items) => {
            for item in items {
                walk_expression(item, scopes, table);
            }
        }
        ast::Expression::List(_, params) => {
            for (_, param) in params {
                if let Some(param) = param {
                    walk_expression(&param.ty, scopes, table);
                }
            }
        }
        // Литералы имён не содержат.
        ast::Expression::Number(_, _)
        | ast::Expression::Duration(_, _, _)
        | ast::Expression::Rational(_, _, _)
        | ast::Expression::String(_)
        | ast::Expression::Address(_, _, _)
        // Анонимное обращение имён не содержит: у ячейки их нет - ни объявления, ни
        // ссылки, поэтому переименовывать в нём нечего.
        | ast::Expression::AnonAddress(_, _, _)
        | ast::Expression::Bool(_, _) => {}
    }
}

/// Условие перехода.
///
/// Отличается от выражения одним местом: `S(Модель) = Состояние` - встроенная
/// форма, в которой аргумент `S` есть **модель**, а правая часть равенства -
/// **состояние**. Разрешать их как значения бессмысленно (их там нет), а молча
/// пропускать нельзя: это настоящие использования имён.
fn walk_condition(cond: &ast::Condition, scopes: &mut Scopes, table: &mut UsageTable) {
    match cond {
        ast::Condition::Equal(_, lhs, rhs) | ast::Condition::NotEqual(_, lhs, rhs)
            if state_of_model_arg(lhs).is_some() =>
        {
            // Левая часть - `S(Модель)`; правая - состояние этой модели, а не текущей.
            // Искать его в стеке областей бессмысленно: модель соседняя, поэтому
            // спрашиваем реестр её членов.
            let model_arg = state_of_model_arg(lhs).expect("проверено охраной ветки");
            walk_condition(lhs, scopes, table);
            match (
                scopes.resolve(&model_arg.name, Namespace::Model),
                rhs.as_ref(),
            ) {
                (Some(model), ast::Condition::Variable(state)) => {
                    reference_member(model.id, state, Namespace::State, scopes, table);
                }
                (_, rhs) => walk_condition(rhs, scopes, table),
            }
        }
        ast::Condition::Variable(name) => reference(name, VALUE_SPACES, scopes, table),
        ast::Condition::Function(_, name, args) => {
            if name.name == STATE_OF_MODEL {
                // `S(Ping)` - аргумент есть имя модели; сама `S` встроена.
                for arg in args {
                    if let ast::Condition::Variable(model) = arg {
                        reference(model, &[Namespace::Model], scopes, table);
                    } else {
                        walk_condition(arg, scopes, table);
                    }
                }
                return;
            }
            reference(name, &[Namespace::Callable], scopes, table);
            for arg in args {
                walk_condition(arg, scopes, table);
            }
        }
        ast::Condition::ArraySubscript(_, base, index) => {
            walk_condition(base, scopes, table);
            walk_condition(index, scopes, table);
        }
        ast::Condition::BitAccess(_, base, member) => {
            walk_condition(base, scopes, table);
            match member {
                ast::Member::Identifier(_) | ast::Member::Number(_) => {}
            }
        }
        ast::Condition::Parenthesis(_, inner) | ast::Condition::Not(_, inner) => {
            walk_condition(inner, scopes, table)
        }
        ast::Condition::Add(_, lhs, rhs)
        | ast::Condition::Subtract(_, lhs, rhs)
        | ast::Condition::And(_, lhs, rhs)
        | ast::Condition::Or(_, lhs, rhs)
        | ast::Condition::Less(_, lhs, rhs)
        | ast::Condition::More(_, lhs, rhs)
        | ast::Condition::LessEqual(_, lhs, rhs)
        | ast::Condition::MoreEqual(_, lhs, rhs)
        | ast::Condition::Equal(_, lhs, rhs)
        | ast::Condition::NotEqual(_, lhs, rhs) => {
            walk_condition(lhs, scopes, table);
            walk_condition(rhs, scopes, table);
        }
        // Константная выдержка: внутри - имена констант, то есть настоящие
        // **использования**. Обходятся тем же рекурсивным путём, что и прочие условия:
        // пропуск испортил бы исходник переименованием константы.
        ast::Condition::AfterExpr(_, inner) => walk_condition(inner, scopes, table),
        ast::Condition::Number(_, _)
        | ast::Condition::Duration(_, _, _)
        | ast::Condition::After(_, _, _)
        | ast::Condition::AfterTicks(_, _, _)
        | ast::Condition::Rational(_, _, _)
        | ast::Condition::String(_)
        | ast::Condition::AnonAddress(_, _, _)
        | ast::Condition::Bool(_, _) => {}
    }
}

/// Имя встроенной формы "состояние модели".
const STATE_OF_MODEL: &str = "S";

/// Имя модели из формы `S(Модель)` - с учётом прозрачных скобок.
///
/// Написано на `if let`, а не на `match`: здесь распознаётся **одна** форма, и ветка
/// "всё остальное" - не пропуск узла, а её отсутствие. `match` с `_` тут правило модуля
/// нарушил бы по букве, ничего не охраняя по сути.
fn state_of_model_arg(cond: &ast::Condition) -> Option<&ast::Identifier> {
    let cond = unwrap_parens(cond);
    let ast::Condition::Function(_, name, args) = cond else {
        return None;
    };
    if name.name != STATE_OF_MODEL {
        return None;
    }
    let ast::Condition::Variable(model) = unwrap_parens(args.first()?) else {
        return None;
    };
    Some(model)
}

/// Снимает обёртки `Parenthesis` - скобки паттерна `S(...)` прозрачны.
fn unwrap_parens(cond: &ast::Condition) -> &ast::Condition {
    let mut current = cond;
    while let ast::Condition::Parenthesis(_, inner) = current {
        current = inner;
    }
    current
}

/// Формула LTL/Guard: атом - использование переменной или имени состояния.
///
/// уже установила, что имя в формуле есть **использование** (иначе `SE-036` даёт ложное
/// предупреждение). Пропустив формулы, переименование оставило бы их со старым именем.
fn walk_ltl(expr: &ast::LtlExpr, scopes: &mut Scopes, table: &mut UsageTable) {
    match expr {
        ast::LtlExpr::Atom(name) => reference(
            name,
            &[Namespace::Value, Namespace::Condition, Namespace::State],
            scopes,
            table,
        ),
        ast::LtlExpr::Not(_, inner)
        | ast::LtlExpr::Next(_, inner)
        | ast::LtlExpr::Finally(_, inner)
        | ast::LtlExpr::Globally(_, inner)
        | ast::LtlExpr::Parenthesis(_, inner) => walk_ltl(inner, scopes, table),
        ast::LtlExpr::And(_, lhs, rhs)
        | ast::LtlExpr::Or(_, lhs, rhs)
        | ast::LtlExpr::Until(_, lhs, rhs)
        | ast::LtlExpr::Release(_, lhs, rhs)
        | ast::LtlExpr::Implies(_, lhs, rhs) => {
            walk_ltl(lhs, scopes, table);
            walk_ltl(rhs, scopes, table);
        }
        ast::LtlExpr::True(_) | ast::LtlExpr::False(_) => {}
    }
}

/// Встроенная формула `: [LTL] φ;` / `: [Guard] φ;`.
fn walk_inline_formula(
    def: &ast::InlineFormulaDefine,
    scopes: &mut Scopes,
    table: &mut UsageTable,
) {
    match def {
        // `: условия;` и `: [Guard] условия;` - обычные условия перехода.
        ast::InlineFormulaDefine::Guard { conditions, .. } => {
            for cond in conditions {
                walk_condition(cond, scopes, table);
            }
        }
        ast::InlineFormulaDefine::Ltl { formulas, .. } => {
            for formula in formulas {
                walk_ltl(formula, scopes, table);
            }
        }
    }
}

/// Блок `formula { ... }`.
fn walk_formula_block(block: &ast::FormulaBlock, scopes: &mut Scopes, table: &mut UsageTable) {
    for stmt in &block.statements {
        match stmt {
            ast::FormulaStatement::Expression(_, expr) => walk_formula_expr(expr, scopes, table),
            ast::FormulaStatement::Block(inner) => walk_formula_block(inner, scopes, table),
            ast::FormulaStatement::Function(f) => {
                for arg in &f.arguments {
                    walk_formula_expr(arg, scopes, table);
                }
            }
            ast::FormulaStatement::Error(loc) => table.push_unsupported(*loc),
        }
    }
}

/// Выражение внутри блока формулы.
fn walk_formula_expr(expr: &ast::FormulaExpression, scopes: &mut Scopes, table: &mut UsageTable) {
    match expr {
        ast::FormulaExpression::Variable(name) => reference(name, VALUE_SPACES, scopes, table),
        ast::FormulaExpression::Function(f) => {
            for arg in &f.arguments {
                walk_formula_expr(arg, scopes, table);
            }
        }
        ast::FormulaExpression::SuffixAccess(_, base, _) => walk_formula_expr(base, scopes, table),
        ast::FormulaExpression::Parenthesis(_, inner) => walk_formula_expr(inner, scopes, table),
        ast::FormulaExpression::Bool(_, _, _)
        | ast::FormulaExpression::Number(_, _, _)
        | ast::FormulaExpression::String(_, _) => {}
    }
}

/// Тип: ссылка на псевдоним/структуру/перечисление - тоже использование имени. Отмечает
/// позицией типа **объявляемое** имя типа (`type X = ...`, `struct X { ... }`, `enum X
/// { ... }`).
fn mark_declared_type_name(name: Option<&ast::Identifier>, table: &mut UsageTable) {
    if let Some(name) = name
        && let Some((_, start, end)) = name_range(name.loc)
    {
        table.push_type_ref(start, end);
    }
}

/// Отмечает позицией типа имя, стоящее в позиции типа, но пришедшее
/// **выражением** (тип параметра функции - `ParameterTypeExpr` грамматики).
///
/// Разбирается только идентификатор: прочие формы выражения в позиции типа (вызов,
/// скобки) типом не являются либо не несут одного имени, и красить их нечем.
/// Исчерпаемостью эта ветвь **не** защищена - узел `Expression` разбирается частично
/// намеренно, поэтому новая форма типа-выражения потребует правки здесь и покрыта
/// тестом.
fn mark_type_expression(expr: &ast::Expression, table: &mut UsageTable) {
    if let ast::Expression::Variable(name) = expr
        && let Some((_, start, end)) = name_range(name.loc)
    {
        table.push_type_ref(start, end);
    }
}

fn walk_type(ty: &ast::Type, scopes: &mut Scopes, table: &mut UsageTable) {
    match ty {
        ast::Type::Alias(name) => {
            // Позиция типа отмечается до разрешения: `u8`, `bit`, `duration` символов
            // этого файла не имеют и в `usages` не попадут, но типами быть не
            // перестают.
            if let Some((_, start, end)) = name_range(name.loc) {
                table.push_type_ref(start, end);
            }
            reference(name, &[Namespace::Type], scopes, table);
        }
        ast::Type::Array { element_type, .. } => walk_type(element_type, scopes, table),
        ast::Type::Function { params, returns } => {
            for (_, param) in params.iter().chain(returns.iter().flatten()) {
                if let Some(param) = param {
                    walk_expression(&param.ty, scopes, table);
                }
            }
        }
        // Fixed-point `q(m, n)`: имя конструктора - обычный идентификатор (ключевым
        // словом `q` намеренно не сделан), позиции отдельной у него нет. Диапазон имени -
        // начало `Location` плюс длина имени: конструктор стоит первым в записи `q(m,
        // n)`, что задано грамматикой.
        ast::Type::Fixed(loc, ctor, _, _, _) => {
            if let Some((_, start, end)) = name_range(*loc) {
                let name_end = start.saturating_add(ctor.chars().count() as u32);
                if name_end <= end {
                    table.push_type_ref(start, name_end);
                }
            }
        }
        // `Enum`/`Struct` в АСД несут имя строкой без позиции - ни вхождением, ни
        // позицией типа их не сделать; сами объявления обходятся отдельно. Остальные
        // варианты грамматикой не порождаются (тип приходит псевдонимом) и позиции не
        // несут.
        ast::Type::Enum(_)
        | ast::Type::Struct(_)
        | ast::Type::Address { .. }
        | ast::Type::Bit
        | ast::Type::Bool
        | ast::Type::Rational
        | ast::Type::Duration
        | ast::Type::Unit => {}
    }
}

/// Куда объявлять символ: в модель или в текущую локальную область.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DeclareIn {
    Model,
    Local,
}

/// Имя и вид объявляемой переменной/константы/порта.
fn variable_name_and_kind(
    def: &ast::VariableDefine,
    where_: DeclareIn,
) -> (Option<&ast::Identifier>, SymbolKind) {
    match def {
        ast::VariableDefine::Variable { name, .. } => (
            name.as_ref(),
            if where_ == DeclareIn::Local {
                SymbolKind::Local
            } else {
                SymbolKind::Variable
            },
        ),
        ast::VariableDefine::Port { name, .. } => (name.as_ref(), SymbolKind::Port),
        ast::VariableDefine::Constant { name, .. } => (name.as_ref(), SymbolKind::Const),
        ast::VariableDefine::Parameter { name, .. } => (name.as_ref(), SymbolKind::ModelParameter),
    }
}

/// Ссылка на тип в объявлении (`var x: MyAlias`).
fn walk_variable_type(def: &ast::VariableDefine, scopes: &mut Scopes, table: &mut UsageTable) {
    let ty = match def {
        ast::VariableDefine::Variable { typ, .. }
        | ast::VariableDefine::Port { typ, .. }
        | ast::VariableDefine::Constant { typ, .. }
        | ast::VariableDefine::Parameter { typ, .. } => typ,
    };
    if let Some(ty) = ty {
        walk_type(ty, scopes, table);
    }
}

/// Объявление переменной/константы/порта - в модели или локально.
fn declare_variable(
    def: &ast::VariableDefine,
    scopes: &mut Scopes,
    table: &mut UsageTable,
    where_: DeclareIn,
) {
    walk_variable_type(def, scopes, table);
    let (name, kind) = variable_name_and_kind(def, where_);
    if let Some(name) = name {
        declare(name, kind, scopes, table, where_);
    }
}

/// Выражения объявления, в которых могут стоять имена.
///
/// У порта их **два**: размещение `at <адрес>` и инициализатор. Адрес - не обязательно
/// литерал: `at BASE + 4` ссылается на константу, и пропустив это выражение,
/// переименование испортило бы исходник (ради чего слой использований и заведён).
fn variable_value_expressions(def: &ast::VariableDefine) -> Vec<&ast::Expression> {
    match def {
        ast::VariableDefine::Variable { initializer, .. } => initializer.iter().collect(),
        ast::VariableDefine::Port {
            address,
            initializer,
            ..
        } => address.iter().chain(initializer.iter()).collect(),
        ast::VariableDefine::Constant { initializer, .. }
        | ast::VariableDefine::Parameter { initializer, .. } => vec![initializer],
    }
}

/// Имена, вводимые директивой `import`.
///
/// Целевой символ живёт в **другом** файле, поэтому объявлением здесь считается само
/// вводимое имя (алиас либо исходное имя при `import { A }`). Так `rename` имени модели
/// получит отказ по виду `Model`, а `references` всё же покажет вхождения в этом файле.
fn each_import_binding<'a>(
    def: &'a ast::ImportDefine,
    f: &mut impl FnMut(&'a ast::Identifier, SymbolKind),
) {
    match def {
        // `import "путь" as Имя;` - имя действительно называет модель файла.
        ast::ImportDefine::GlobalSymbol(_, alias, _) => f(alias, SymbolKind::Model),
        // `import { a, b as c } from "путь";` - переносится объявление соседнего файла,
        // и вид у него тот же, что там: переменная, порт, функция, тип. Здесь он
        // неизвестен (слой однофайловый), поэтому вид `Imported` - он отвечает на
        // ссылку в любом пространстве.
        ast::ImportDefine::Rename(_, names, _) => {
            for (original, alias) in names {
                f(alias.as_ref().unwrap_or(original), SymbolKind::Imported);
            }
        }
        // `import "путь";` вводит имя по имени файла - идентификатора в тексте нет,
        // вхождением он быть не может.
        ast::ImportDefine::Plain(_, _) => {}
    }
}

/// Исходные имена в `import { A as B }` - ссылки на символы **чужого** файла.
///
/// Связать их не с чем, но и потерять нельзя: тест полноты должен знать, что имя `A`
/// в файле встречается.
fn note_import_originals(def: &ast::ImportDefine, table: &mut UsageTable) {
    if let ast::ImportDefine::Rename(_, names, _) = def {
        for (original, alias) in names {
            if alias.is_some() {
                push_unresolved(original, table);
            }
        }
    }
}

/// Записывает объявление символа и его вхождение-декларацию.
fn declare(
    name: &ast::Identifier,
    kind: SymbolKind,
    scopes: &mut Scopes,
    table: &mut UsageTable,
    where_: DeclareIn,
) {
    let Some((file_no, start, end)) = name_range(name.loc) else {
        // Объявление без позиции в тексте (порождённое) - вхождением не является.
        return;
    };
    let symbol = Symbol {
        id: super::SymbolId { file_no, start },
        kind,
    };
    match where_ {
        DeclareIn::Model => scopes.declare_in_model(&name.name, symbol),
        DeclareIn::Local => scopes.declare_local(&name.name, symbol),
    }
    table.push(Usage {
        name: name.name.clone(),
        start,
        end,
        symbol: symbol.id,
        symbol_kind: kind,
        kind: UsageKind::Declaration,
    });
}

/// Записывает использование имени, разрешая его в перечисленных пространствах.
fn reference(
    name: &ast::Identifier,
    spaces: &[Namespace],
    scopes: &mut Scopes,
    table: &mut UsageTable,
) {
    let Some((_, start, end)) = name_range(name.loc) else {
        return;
    };
    match scopes.resolve_any(&name.name, spaces) {
        Some(symbol) => table.push(Usage {
            name: name.name.clone(),
            start,
            end,
            symbol: symbol.id,
            symbol_kind: symbol.kind,
            kind: UsageKind::Reference,
        }),
        None => push_unresolved(name, table),
    }
}

/// Записывает использование имени как **члена заданной модели**.
fn reference_member(
    model: super::SymbolId,
    name: &ast::Identifier,
    ns: Namespace,
    scopes: &mut Scopes,
    table: &mut UsageTable,
) {
    let Some((_, start, end)) = name_range(name.loc) else {
        return;
    };
    match scopes.resolve_member(model, &name.name, ns) {
        Some(symbol) => table.push(Usage {
            name: name.name.clone(),
            start,
            end,
            symbol: symbol.id,
            symbol_kind: symbol.kind,
            kind: UsageKind::Reference,
        }),
        None => push_unresolved(name, table),
    }
}

/// Имя, которое не удалось связать с объявлением этого файла.
fn push_unresolved(name: &ast::Identifier, table: &mut UsageTable) {
    let Some((_, start, end)) = name_range(name.loc) else {
        return;
    };
    table.push_unresolved(UnresolvedName {
        name: name.name.clone(),
        start,
        end,
    });
}
