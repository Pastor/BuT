//! Присваивание - **оператор**, а не выражение: остаточная проверка.
//!
//! ## Что нормируется
//!
//! Чтение порта - выражение, запись - оператор, а как это отображается, задаёт цель по
//! единой таблице. Правило записано здесь, потому что в языке порт и переменная в этом
//! смысле неразличимы: запись - действие, а не вычисляемое значение.
//!
//! Позиций записи в языке три: выражение-оператор тела (`led := 1;`), шаг цикла (`for
//! var i: u8 := 0; ...; i := i + 1`) и именованный аргумент вызова (`Pid(kp := 0.25)`).
//! Первые две отсекает грамматика (`SY-006`), третью - этот модуль (`SE-095`).
//!
//! ## Чем платит язык без правила
//!
//! Вход `seen := (value := 3) + 1;` при `out value: u8` даёт у шести потребителей три
//! поведения и ни одного внятного отказа:
//!
//! | Потребитель | Что происходит |
//! |---|---|
//! | `c` | `model->seen = ((*main->write_numeric)(...)) + 1;` - `void + int`, `cc` отвергает |
//! | `rust` | `self.seen = hal.write_u8(...) + 1;` - `() + {integer}`, `rustc` отвергает (E0369) |
//! | `st` | `seen := (value := 3) + 1;` - в IEC присваивание не выражение, `iec2c` отвергает |
//! | `sv` | честный отказ `SV-002` при генерации |
//! | эталон | отказ `SIM-014` в такте, то есть уже на исполнении |
//!
//! То есть язык позволял бы написать то, чего не умеет ни один потребитель, а
//! диагностику давали бы чужие инструменты - на порождённом файле и с координатами
//! этого файла, а не исходника.
//!
//! На обычных переменных цена выше, чем на портах: `seen := (a := 3) + 1;` цель `c`
//! печатает валидным C (`(model->a = 3) + 1`) и считает `4`, тогда как эталон ту же
//! модель не исполняет вовсе (`SIM-014`).
//!
//! ## Роль модуля
//!
//! Главный носитель правила - грамматика (`SY-006`): `:=` разбирается ровно в трёх
//! позициях - оператор тела, шаг цикла `for` и именованный аргумент вызова
//! (`Pid(kp := 0.25)`).
//!
//! Последняя позиция и оставляет работу этому судье: на уровне АСД вызов функции и
//! инстанцирование модели - один узел `Function`, различает их только семантика по
//! месту. Поэтому `twice(a := 3)` в теле грамматика принимает, а `SE-095` отвергает: у
//! функции именованных аргументов нет.
//!
//! Формы, отсекаемые грамматикой (`seen := (value := 3) + 1`, `if (a := 1)`,
//! `a := b := c`), сюда не доходят - они не разбираются. Судья остаётся страховкой на
//! достижимом остатке.

use crate::diagnostics::lang::keys;
use crate::msg;
use crate::diagnostics::{Diagnostic, Location};
use crate::semantic::validate::bodies::Position;
use crate::semantic::{ExpressionNode, VariableNode};

/// Проверяет позицию присваиваний в выражении.
///
/// Возвращает **все** нарушения (накопительно, как `literal_range`): редактор
/// подчёркивает каждое, а не первое.
pub(super) fn check_expression(
    expr: &ExpressionNode,
    position: Position,
    stmt_loc: Location,
) -> Vec<Diagnostic> {
    let mut found = Vec::new();
    match position {
        // Верхний уровень оператора: само присваивание законно, но его части - уже
        // значения (`led := (x := 1);` - правая часть вычисляется).
        Position::Statement => match expr {
            ExpressionNode::Assign(left, right) => {
                collect(left, stmt_loc, &mut found);
                collect(right, stmt_loc, &mut found);
            }
            // Скобки прозрачны: `(led := 1);` - то же действие.
            ExpressionNode::Parenthesis(inner) => {
                found.extend(check_expression(inner, Position::Statement, stmt_loc))
            }
            other => collect(other, stmt_loc, &mut found),
        },
        Position::Value => collect(expr, stmt_loc, &mut found),
    }
    found
}

/// `SE-095`: присваивание стоит там, где вычисляется значение.
fn diagnostic(loc: Location, target: Option<&str>) -> Diagnostic {
    let what = match target {
        Some(name) => msg!(keys::WHAT_ASSIGNMENT_NAMED, name = name),
        None => msg!(keys::WHAT_ASSIGNMENT),
    };
    Diagnostic::error(
        loc,
        msg!(keys::SE_095_ASSIGNMENT_IN_VALUE_POSITION, what = what),
    )
    .with_code("SE-095")
}

/// Имя и позиция цели записи - для диагностики с координатами исходника.
fn target_of(expr: &ExpressionNode) -> (Location, Option<String>) {
    let var = match expr {
        ExpressionNode::Variable(v) => Some(v),
        ExpressionNode::BitAccess(inner, _) => match inner.as_ref() {
            ExpressionNode::Variable(v) => Some(v),
            _ => None,
        },
        // База индексации - выражение: спускаемся по ней.
        ExpressionNode::ArraySubscript(base, _) => return target_of(base),
        _ => None,
    };
    match var {
        Some(v) => {
            let borrowed = v.borrow();
            match &*borrowed {
                VariableNode::Simple { name, loc, .. }
                | VariableNode::Port { name, loc, .. }
                | VariableNode::Const { name, loc, .. } => (*loc, Some(name.clone())),
                VariableNode::Unresolved => (Location::Codegen, None),
            }
        }
        None => (Location::Codegen, None),
    }
}

/// Ищет присваивания во **всех** подвыражениях: здесь любое из них незаконно.
///
/// Разбор исчерпывающий (ветки `_` нет) намеренно: новый узел языка, способный нести
/// выражение, обязан **завалить сборку** этого модуля, а не молча вывести своё
/// содержимое из-под правила. Тот же приём, что у `semantic/usages/walk.rs` и
/// `parser/depth`.
fn collect(expr: &ExpressionNode, stmt_loc: Location, found: &mut Vec<Diagnostic>) {
    match expr {
        ExpressionNode::Assign(left, right) => {
            let (loc, name) = target_of(left);
            // Позиция оператора точнее: `loc` у цели - координата её объявления, а при
            // неопознанной цели её нет вовсе.
            let loc = if matches!(stmt_loc, Location::Builtin) {
                loc
            } else {
                stmt_loc
            };
            found.push(diagnostic(loc, name.as_deref()));
            // Внутрь тоже: `a := (b := (c := 1));` - три нарушения, не одно.
            collect(left, stmt_loc, found);
            collect(right, stmt_loc, found);
        }
        ExpressionNode::Parenthesis(inner)
        | ExpressionNode::BitAccess(inner, _)
        | ExpressionNode::CodeBlock(inner, _)
        | ExpressionNode::NamedFunctionBox(inner, _)
        | ExpressionNode::Not(inner)
        | ExpressionNode::BitwiseNot(inner)
        | ExpressionNode::UnaryPlus(inner)
        | ExpressionNode::Negate(inner)
        | ExpressionNode::Cast(inner, _)
        | ExpressionNode::ArraySubscript(_, inner) => collect(inner, stmt_loc, found),
        ExpressionNode::Power(left, right)
        | ExpressionNode::Multiply(left, right)
        | ExpressionNode::Divide(left, right)
        | ExpressionNode::Modulo(left, right)
        | ExpressionNode::Add(left, right)
        | ExpressionNode::Subtract(left, right)
        | ExpressionNode::ShiftLeft(left, right)
        | ExpressionNode::ShiftRight(left, right)
        | ExpressionNode::BitwiseAnd(left, right)
        | ExpressionNode::BitwiseXor(left, right)
        | ExpressionNode::BitwiseOr(left, right)
        | ExpressionNode::Less(left, right)
        | ExpressionNode::More(left, right)
        | ExpressionNode::LessEqual(left, right)
        | ExpressionNode::MoreEqual(left, right)
        | ExpressionNode::Equal(left, right)
        | ExpressionNode::NotEqual(left, right)
        | ExpressionNode::And(left, right) => {
            collect(left, stmt_loc, found);
            collect(right, stmt_loc, found);
        }
        ExpressionNode::Or(left, right) => {
            collect(left, stmt_loc, found);
            collect(right, stmt_loc, found);
        }
        ExpressionNode::ConditionalOperator(cond, then_, else_) => {
            collect(cond, stmt_loc, found);
            collect(then_, stmt_loc, found);
            collect(else_, stmt_loc, found);
        }
        ExpressionNode::Function(_, args)
        | ExpressionNode::Array(args)
        | ExpressionNode::Initializer(args) => {
            for arg in args {
                collect(arg, stmt_loc, found);
            }
        }
        // Листья: значения, ссылки и формы, выражений внутри не несущие.
        //
        // `Unresolved` - "сырой" АСД: до понижения узла присваивания в нём не видно, и
        // проверять его здесь нечем. Понижение делает стадия 5, после неё выражение
        // приходит сюда уже разрешённым.
        ExpressionNode::None
        | ExpressionNode::Unresolved(_)
        | ExpressionNode::ArraySlice(_, _, _)
        | ExpressionNode::Number(_)
        | ExpressionNode::Duration(_)
        | ExpressionNode::Rational(_, _)
        | ExpressionNode::String(_)
        | ExpressionNode::Type(_)
        | ExpressionNode::Address(_, _)
        // Обращение к ячейке выражений внутри не несёт: адрес - литерал.
        | ExpressionNode::AnonPort(_)
        | ExpressionNode::Bool(_)
        | ExpressionNode::Variable(_)
        | ExpressionNode::Model(_)
        | ExpressionNode::Condition(_)
        | ExpressionNode::List(_) => {}
    }
}
