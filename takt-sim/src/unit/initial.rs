//! Вычислитель **начальных значений** переменных и портов.

#![deny(clippy::wildcard_enum_match_arm)]

use crate::eval::value::Value;
use takt_lang::semantic::{ExpressionNode, ModelNode, VariableNode};

/// Предел вложенности ссылок на константы.
///
/// Цепочка `const A := B; const B := A;` разрешается семантикой, но **значения** у неё
/// нет, и обход по инициализаторам ушёл бы в бесконечную рекурсию. Предел той же
/// природы, что `MAX_NESTING_DEPTH` семантики: не "правильное число", а граница, за
/// которой ответа всё равно нет.
const MAX_REF_DEPTH: usize = 32;

/// Вычисляет простое константное выражение в [`Value`].
///
/// `None` - "константой не является": значение будет взято из умолчания по типу
/// (`default_field` в `builder.rs`) либо вычислено в такте.
pub(super) fn eval_expr(expr: &ExpressionNode) -> Option<Value> {
    eval_expr_at(expr, None, 0)
}

/// То же, но с моделью - областью видимости имён.
///
/// Модель нужна ради **ссылки на константу**: спрашивать надо таблицу объявлений, а не
/// ячейку ссылки. Ячейка - снимок, снятый при разрешении имени, и лежит в ней ещё не
/// понижённый АСД, тогда как в таблице - уже свёрнутый литерал.
pub(super) fn eval_expr_in(expr: &ExpressionNode, model: &ModelNode) -> Option<Value> {
    eval_expr_at(expr, Some(model), 0)
}

/// То же с учётом области видимости и глубины разбора ссылок на константы.
fn eval_expr_at(expr: &ExpressionNode, model: Option<&ModelNode>, depth: usize) -> Option<Value> {
    let eval_expr = |e: &ExpressionNode| eval_expr_at(e, model, depth);
    match expr {
        // Приведение `as`: считается **тем же** `cast_to_type`, что и в такте.
        //
        // Своей арифметики приведения здесь быть не должно.
        ExpressionNode::Cast(inner, ty) => {
            let value = eval_expr_at(inner, model, depth)?;
            crate::eval::cast_to_type(value, ty).ok()
        }
        // Ссылка на константу: её значение известно до такта.
        //
        // Ячейка ссылки - снимок объявления (см. 0204), и читается из неё именно
        // инициализатор: разрешение имени здесь недоступно. Обычная переменная и порт
        // остаются "не константой" - их значение приходит в такте.
        ExpressionNode::Variable(cell) => {
            if depth >= MAX_REF_DEPTH {
                return None;
            }
            let name = cell.borrow().name().to_string();
            // Сначала таблица объявлений: там инициализатор уже свёрнут в литерал. В
            // ячейке лежит снимок с сырым АСД, и по нему значение не восстановить.
            if let Some(model) = model
                && let Some(VariableNode::Const { expr, .. }) = model.search_var(&name)
            {
                return eval_expr_at(&expr, Some(model), depth + 1);
            }
            match &*cell.borrow() {
                VariableNode::Const { expr, .. } => eval_expr_at(expr, model, depth + 1),
                VariableNode::Simple { .. }
                | VariableNode::Port { .. }
                | VariableNode::Unresolved => None,
            }
        }
        ExpressionNode::Number(n) => Some(Value::Number(*n)),
        // Длительность обязана быть здесь: её отсутствие молча превращало `var left:
        // duration := 1m30s;` в "значения нет" (тест поймал ровно это).
        ExpressionNode::Duration(ns) => Some(Value::Duration(*ns)),
        ExpressionNode::Bool(b) => Some(Value::Boolean(*b)),
        ExpressionNode::Rational(s, neg) => {
            let v: f64 = s.parse().ok()?;
            Some(Value::Real(if *neg { -v } else { v }))
        }
        ExpressionNode::Negate(inner) => match eval_expr(inner)? {
            Value::Number(n) => Some(Value::Number(-n)),
            Value::Real(f) => Some(Value::Real(-f)),
            // Отрицание прочих значений смысла не имеет. Перечислено явно, хотя `Value` -
            // тип крейта, а не узел языка: модульный `deny` не различает их, и это к
            // лучшему. Появится новый род значения (например, ещё одно числовое
            // представление) - компилятор потребует решить, отрицаемо ли оно, вместо
            // молчаливого `None`.
            Value::Boolean(_)
            | Value::Array(_)
            | Value::Fixed { .. }
            | Value::Struct { .. }
            | Value::Duration(_) => None,
        },
        ExpressionNode::Parenthesis(inner) => eval_expr(inner),
        // Инициализатор `{...}` структуры и массивный литерал `[...]` - оба дают список
        // значений; тип цели (структура/массив) различит `coerce_initial` по
        // объявленному типу переменной.
        ExpressionNode::Array(items) | ExpressionNode::Initializer(items) => {
            let values: Option<Vec<Value>> = items.iter().map(eval_expr).collect();
            Some(Value::Array(values?))
        }
        // Адресные порты (`bit := 0x600:0`) инициализируются нулём по умолчанию.
        ExpressionNode::Address(_, _) => Some(Value::Number(0)),

        // -- Не константа ----------------------------------------------------
        //
        // Перечислено явно, а не сведено в `_ =>`, и в этом весь смысл модуля: добавив
        // вариант языка, автор обязан вписать его сюда - то есть ответить, константа он
        // или нет. Умолчания у нового узла больше нет.
        //
        // Здесь собраны узлы, значение которых до первого такта неизвестно: они читают
        // переменные и порты, зовут функции, ветвятся или вовсе не являются
        // выражением-значением. Обращение к ячейке - чтение памяти: до первого такта её
        // содержимое неизвестно. Значение отсюда наружу не уходит: инициализатор с
        // обращением отвергает семантика (`SE-099`), иначе эталон дал бы ноль, а цель
        // `c-hal` - настоящее чтение регистра.
        ExpressionNode::AnonPort(..)
        | ExpressionNode::None
        | ExpressionNode::Unresolved(..)
        | ExpressionNode::Model(..)
        | ExpressionNode::Condition(..)
        | ExpressionNode::Type(..)
        | ExpressionNode::String(..)
        | ExpressionNode::List(..)
        | ExpressionNode::CodeBlock(..)
        | ExpressionNode::NamedFunctionBox(..)
        | ExpressionNode::Function(..)
        | ExpressionNode::ArraySubscript(..)
        | ExpressionNode::ArraySlice(..)
        | ExpressionNode::BitAccess(..)
        | ExpressionNode::Assign(..)
        | ExpressionNode::ConditionalOperator(..)
        // Унарные и бинарные операции: свёртки констант этот вычислитель не делает
        // намеренно - она принадлежит семантике, а не построению юнита.
        | ExpressionNode::Not(..)
        | ExpressionNode::BitwiseNot(..)
        | ExpressionNode::UnaryPlus(..)
        | ExpressionNode::Power(..)
        | ExpressionNode::Multiply(..)
        | ExpressionNode::Divide(..)
        | ExpressionNode::Modulo(..)
        | ExpressionNode::Add(..)
        | ExpressionNode::Subtract(..)
        | ExpressionNode::ShiftLeft(..)
        | ExpressionNode::ShiftRight(..)
        | ExpressionNode::BitwiseAnd(..)
        | ExpressionNode::BitwiseXor(..)
        | ExpressionNode::BitwiseOr(..)
        | ExpressionNode::Less(..)
        | ExpressionNode::More(..)
        | ExpressionNode::LessEqual(..)
        | ExpressionNode::MoreEqual(..)
        | ExpressionNode::Equal(..)
        | ExpressionNode::NotEqual(..)
        | ExpressionNode::And(..)
        | ExpressionNode::Or(..) => None,
    }
}
