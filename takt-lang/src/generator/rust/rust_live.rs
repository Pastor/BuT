//! Живость инициализатора локальной переменной.
//!
//! ## Задача
//!
//! Takt объявляет локальную переменную со значением, даже когда оно тут же затирается -
//! это обычная идиома корпуса (`stacker.takt`, `travel_time`):
//!
//! ```lam
//! var ds: u8 := 0;
//! if pos_stack > to_stack { ds := pos_stack - to_stack; }
//! else                    { ds := to_stack - pos_stack; }
//! ```
//!
//! Дословный перевод даёт `let mut ds: u8 = 0;`, и rustc прав: значение `0`
//! **не читается никогда** (`unused_assignments`), то есть под `-D warnings`
//! проверка краснеет. Цель `c` того же не замечает - там такого линта нет.
//!
//! ## Решение: не эмитить мёртвый инициализатор
//!
//! Это решение R9 (вариант "а", не эмитить лишнее), применённое к телу функции.
//! Альтернатива - `#[allow(unused_assignments)]` - заглушила бы линт на всей функции:
//! настоящая ошибка эмиссии (переменная, которую генератор забыл записать) прошла бы
//! молча.

use crate::semantic::{ExpressionNode, StatementNode};

/// Мёртв ли инициализатор переменной `name`, объявленной перед `rest`.
///
/// `rest` - операторы, идущие **после** объявления в том же блоке.
pub(crate) fn initializer_is_dead(name: &str, rest: &[StatementNode]) -> bool {
    matches!(scan(name, rest), Verdict::Overwritten)
}

/// Нужен ли `mut` объявлению с **отложенной** инициализацией (`let x: T;`).
///
/// Отложенное объявление меняет правила: `let ds: u8; if c { ds = a } else { ds = b }` -
/// это **инициализация** на каждом пути, а не изменение, и `mut` там лишний
/// (`unused_mut` -> отказ проверки). `mut` нужен, только если переменной присваивают
/// **дважды на одном пути**: сначала инициализация, затем правка.
///
/// Оценка сверху: при сомнении `mut` ставится - лишний `mut` ловится линтом (красный
/// проверка), а недостающий даёт ошибку компиляции. Оба исхода громкие.
pub(crate) fn deferred_needs_mut(name: &str, rest: &[StatementNode]) -> bool {
    // Ищем оператор, давший заведомую перезапись: до него присваиваний нет.
    let Some(idx) = rest
        .iter()
        .position(|s| verdict_of(name, s) == Verdict::Overwritten)
    else {
        return true;
    };
    // Внутри самого перезаписывающего оператора путь может присвоить дважды (`{ x := 1;
    // x := 2; }`), и тогда `mut` нужен.
    if max_path_assigns(name, &rest[idx]) > 1 {
        return true;
    }
    // Любое присваивание после инициализации - уже изменение.
    rest[idx + 1..]
        .iter()
        .any(|s| max_path_assigns(name, s) > 0)
}

/// Наибольшее число присваиваний переменной на одном пути внутри оператора.
fn max_path_assigns(name: &str, stmt: &StatementNode) -> usize {
    match stmt {
        StatementNode::Block(items) => items.iter().map(|s| max_path_assigns(name, s)).sum(),
        StatementNode::Expression(expr, _) => match &**expr {
            ExpressionNode::Assign(target, _) => match &**target {
                ExpressionNode::Variable(var) if var.borrow().name() == name => 1,
                _ => 0,
            },
            _ => 0,
        },
        // Ветки исключают друг друга - берётся максимум, а не сумма.
        StatementNode::If { then_, else_, .. } => max_path_assigns(name, then_).max(
            else_
                .as_ref()
                .map(|a| max_path_assigns(name, a))
                .unwrap_or(0),
        ),
        // Тело цикла может исполниться много раз: одно присваивание в нём - это уже
        // изменение.
        StatementNode::Loop { body, .. } => {
            if max_path_assigns(name, body) > 0 {
                2
            } else {
                0
            }
        }
        StatementNode::For {
            body, step, init, ..
        } => {
            // `init` исполняется один раз, тело и шаг - сколько угодно; счёт ведётся по
            // наибольшему пути, поэтому присваивание в `init` даёт единицу, а любое
            // присваивание в теле или шаге - сразу два.
            let in_init = init
                .as_ref()
                .map(|i| max_path_assigns(name, i))
                .unwrap_or(0);
            let in_step = step
                .as_ref()
                .map(|s| {
                    matches!(&**s, ExpressionNode::Assign(t, _)
                        if matches!(&**t, ExpressionNode::Variable(v) if v.borrow().name() == name))
                })
                .unwrap_or(false);
            if max_path_assigns(name, body) > 0 || in_step {
                2
            } else {
                in_init
            }
        }
        StatementNode::Match { arms, .. } => arms
            .iter()
            .map(|a| max_path_assigns(name, &a.body))
            .max()
            .unwrap_or(0),
        _ => 0,
    }
}

/// Форма, в которую сворачивается объявление с мёртвым инициализатором.
///
/// Отложенное объявление (`let ds: u8;` + присваивание) clippy тоже не принимает:
/// `needless_late_init` требует свернуть его в `let ds = ...`. И это справедливо - так
/// код и читается лучше. Свернуть можно всегда, потому что вердикт "мёртв" выдаётся
/// ровно на двух формах (безусловное присваивание либо `if/else`, пишущий обе ветки), и
/// обе выражаются значением.
pub(crate) enum Folded<'a> {
    /// `let x: T = <expr>;` - из безусловного присваивания.
    Value(&'a ExpressionNode),
    /// `let x: T = if <cond> { ... } else { ... };` - из `if/else`.
    Branch {
        /// Условие.
        cond: &'a ExpressionNode,
        /// Значение ветки "да".
        then_: Box<Folded<'a>>,
        /// Значение ветки "нет".
        else_: Box<Folded<'a>>,
    },
    /// `let x: T = if s == p { ... } else if s == q { ... } else { ... };` - из `match`
    /// с ветвью `_`.
    ///
    /// Именно цепочка сравнений, а не `match` языка Rust: образцы Takt - произвольные
    /// выражения, и `match s { p => ... }` **связал бы** `p` как новое имя вместо
    /// сравнения с ним (то же решение, что у печати оператора `match`).
    Chain {
        /// Разбираемое выражение.
        subject: &'a ExpressionNode,
        /// Ветви с образцами: каждая - список образцов и её значение.
        arms: Vec<(Vec<&'a ExpressionNode>, Folded<'a>)>,
        /// Значение ветви `_`.
        otherwise: Box<Folded<'a>>,
    },
}

/// Индекс в `rest` того оператора, в который сворачивается объявление `name`.
///
/// Возвращает `None`, если сворачивать нечего или форма не распознана.
pub(crate) fn fold_target(name: &str, rest: &[StatementNode]) -> Option<usize> {
    let idx = rest
        .iter()
        .position(|s| verdict_of(name, s) == Verdict::Overwritten)?;
    fold_assignment(name, &rest[idx]).map(|_| idx)
}

/// Пытается свернуть присваивания к `name` из оператора в одно значение.
///
/// Возвращает `None`, если форма не распознана - тогда печатается обычное отложенное
/// объявление.
pub(crate) fn fold_assignment<'a>(name: &str, stmt: &'a StatementNode) -> Option<Folded<'a>> {
    match stmt {
        // Блок из одного оператора - прозрачная обёртка (тело ветки `if`).
        StatementNode::Block(items) => match items.as_slice() {
            [single] => fold_assignment(name, single),
            _ => None,
        },
        StatementNode::Expression(expr, _) => match &**expr {
            ExpressionNode::Assign(target, value) => match &**target {
                ExpressionNode::Variable(var) if var.borrow().name() == name => {
                    // `x := x + 1` свернуть нельзя: значение читает само себя.
                    if reads_expr(name, value) {
                        return None;
                    }
                    // Срез свернуть нельзя тоже: он печатается поэлементно, выражения у
                    // него нет - свёртка дала бы `RS-011` там, где присваивание
                    // переводится.
                    if matches!(&**value, ExpressionNode::ArraySlice(_, _, _)) {
                        return None;
                    }
                    Some(Folded::Value(value))
                }
                _ => None,
            },
            _ => None,
        },
        // `match` с ветвью `_` сворачивается в цепочку сравнений. Отложенная форма `let
        // t: T;` перед развёрнутым `if/else` не годится: это
        // `clippy::needless_late_init`, то есть замена одного отказа проверки другим
        // (проверено пробой).
        StatementNode::Match { expr, arms, .. } => {
            if reads_expr(name, expr) {
                return None;
            }
            let mut tested = Vec::new();
            let mut otherwise = None;
            for arm in arms {
                let value = fold_assignment(name, &arm.body)?;
                if arm
                    .patterns
                    .iter()
                    .any(|p| matches!(p, crate::semantic::MatchPatternNode::Wildcard))
                {
                    // Ветвь `_` одна: вторая недостижима, и разбор её значения означал
                    // бы печать мёртвого кода.
                    if otherwise.is_some() {
                        return None;
                    }
                    otherwise = Some(Box::new(value));
                    continue;
                }
                let mut patterns = Vec::new();
                for pattern in &arm.patterns {
                    let crate::semantic::MatchPatternNode::Value(item) = pattern else {
                        return None;
                    };
                    if reads_expr(name, item) {
                        return None;
                    }
                    patterns.push(&**item);
                }
                if patterns.is_empty() {
                    return None;
                }
                tested.push((patterns, value));
            }
            Some(Folded::Chain {
                subject: expr,
                arms: tested,
                otherwise: otherwise?,
            })
        }
        // `if/else` разворачивается рекурсивно - это покрывает и цепочки `else if`,
        // которые clippy сворачивает точно так же.
        StatementNode::If {
            cond,
            then_,
            else_: Some(alt),
            ..
        } => {
            if reads_expr(name, cond) {
                return None;
            }
            Some(Folded::Branch {
                cond,
                then_: Box::new(fold_assignment(name, then_)?),
                else_: Box::new(fold_assignment(name, alt)?),
            })
        }
        _ => None,
    }
}

/// Что произошло с переменной раньше: чтение или перезапись.
#[derive(PartialEq, Eq, Debug)]
enum Verdict {
    /// Переменная прочитана - инициализатор жив.
    Read,
    /// Переменная заведомо перезаписана до чтения - инициализатор мёртв.
    Overwritten,
    /// Ни того, ни другого не доказано.
    Unknown,
}

/// Линейно просматривает операторы до первого чтения либо заведомой перезаписи.
fn scan(name: &str, stmts: &[StatementNode]) -> Verdict {
    for stmt in stmts {
        match verdict_of(name, stmt) {
            Verdict::Unknown => continue,
            decided => return decided,
        }
    }
    Verdict::Unknown
}

/// Вердикт одного оператора.
fn verdict_of(name: &str, stmt: &StatementNode) -> Verdict {
    match stmt {
        StatementNode::Block(items) => scan(name, items),
        StatementNode::Expression(expr, _) => verdict_of_expr(name, expr),
        StatementNode::Variable(decl, _, init, _) => {
            if init.as_ref().is_some_and(|i| reads_expr(name, i)) {
                return Verdict::Read;
            }
            // Объявление с тем же именем затеняет внешнее: дальше речь уже не о нашей
            // переменной.
            if decl == name {
                return Verdict::Overwritten;
            }
            Verdict::Unknown
        }
        // `if` считается заведомой перезаписью, только если обе ветки переписывают
        // переменную и ни условие, ни сами ветки её раньше не читают. `if` без `else`
        // перезаписи не даёт: путь мимо него оставит инициализатор живым.
        StatementNode::If {
            cond, then_, else_, ..
        } => {
            if reads_expr(name, cond) {
                return Verdict::Read;
            }
            let Some(alt) = else_ else {
                return match verdict_of(name, then_) {
                    Verdict::Read => Verdict::Read,
                    _ => Verdict::Unknown,
                };
            };
            match (verdict_of(name, then_), verdict_of(name, alt)) {
                (Verdict::Read, _) | (_, Verdict::Read) => Verdict::Read,
                (Verdict::Overwritten, Verdict::Overwritten) => Verdict::Overwritten,
                _ => Verdict::Unknown,
            }
        }
        // Тело цикла может не исполниться ни разу - перезаписи оно не гарантирует.
        StatementNode::Loop { cond, body, .. } => {
            if cond.as_ref().is_some_and(|c| reads_expr(name, c)) {
                return Verdict::Read;
            }
            match verdict_of(name, body) {
                Verdict::Read => Verdict::Read,
                _ => Verdict::Unknown,
            }
        }
        // `init` цикла исполняется всегда и раньше условия, шага и тела: присваивание в
        // нём - заведомая перезапись.
        StatementNode::For {
            init,
            cond,
            step,
            body,
            ..
        } => {
            if let Some(init) = init {
                match verdict_of(name, init) {
                    Verdict::Read => return Verdict::Read,
                    Verdict::Overwritten => return Verdict::Overwritten,
                    Verdict::Unknown => {}
                }
            }
            if cond.as_ref().is_some_and(|c| reads_expr(name, c))
                || step.as_ref().is_some_and(|s| reads_expr(name, s))
                || verdict_of(name, body) == Verdict::Read
            {
                return Verdict::Read;
            }
            Verdict::Unknown
        }
        // `match` - заведомая перезапись, когда есть ветвь `_` и каждая ветвь
        // переписывает переменную. Без `_` разбор не исчерпан: путь мимо всех образцов
        // оставляет инициализатор живым.
        //
        // Прежде вердикта не было вовсе, а печатник разворачивает `match` в цепочку
        // `if/else` - на входе с `_`-ветвью получался `let mut t: u8 = 0;` с мёртвым
        // значением, то есть отказ `clippy -D warnings` при нулевом коде возврата
        // `taktc`.
        StatementNode::Match { expr, arms, .. } => {
            if reads_expr(name, expr) {
                return Verdict::Read;
            }
            let verdicts: Vec<Verdict> = arms.iter().map(|a| verdict_of(name, &a.body)).collect();
            if verdicts.contains(&Verdict::Read) {
                return Verdict::Read;
            }
            let exhaustive = arms.iter().any(|a| {
                a.patterns
                    .iter()
                    .any(|p| matches!(p, crate::semantic::MatchPatternNode::Wildcard))
            });
            if exhaustive && !arms.is_empty() && verdicts.iter().all(|v| *v == Verdict::Overwritten)
            {
                return Verdict::Overwritten;
            }
            Verdict::Unknown
        }
        StatementNode::Return(Some(expr), _) => {
            if reads_expr(name, expr) {
                Verdict::Read
            } else {
                Verdict::Unknown
            }
        }
        // Вставка: вердикт даёт её тело, но лишь когда эта цель его печатает. Тело,
        // адресованное другой цели, в вывод не попадает - значит и переменную оно здесь
        // не читает.
        StatementNode::Assembly { target, body, .. } => {
            if crate::semantic::target_block::emits_for(target.as_deref(), "rust") {
                verdict_of(name, body)
            } else {
                Verdict::Unknown
            }
        }
        StatementNode::Return(None, _)
        | StatementNode::Continue(_)
        | StatementNode::Break(_)
        | StatementNode::Formula(_)
        | StatementNode::InlineFormula(_)
        | StatementNode::None
        | StatementNode::Unresolved(_) => Verdict::Unknown,
    }
}

/// Вердикт выражения-оператора: присваивание в переменную - перезапись.
fn verdict_of_expr(name: &str, expr: &ExpressionNode) -> Verdict {
    if let ExpressionNode::Assign(target, value) = expr {
        // `x := x + 1` читает `x` - инициализатор жив.
        if reads_expr(name, value) {
            return Verdict::Read;
        }
        if let ExpressionNode::Variable(var) = &**target
            && var.borrow().name() == name
        {
            return Verdict::Overwritten;
        }
        return Verdict::Unknown;
    }
    if reads_expr(name, expr) {
        return Verdict::Read;
    }
    Verdict::Unknown
}

/// Читает ли выражение переменную `name`.
///
/// Цель присваивания чтением не считается, всё остальное - считается. Оценка
/// **сверху**: при сомнении лучше признать чтением (инициализатор останется).
fn reads_expr(name: &str, expr: &ExpressionNode) -> bool {
    match expr {
        ExpressionNode::Variable(var) => var.borrow().name() == name,
        ExpressionNode::Assign(target, value) => {
            // В цели читается только индекс (`a[i] := ...` читает `i`), само имя - нет.
            let target_reads = match &**target {
                ExpressionNode::Variable(_) => false,
                other => reads_expr(name, other),
            };
            target_reads || reads_expr(name, value)
        }
        // База - выражение: чтение ищется в ней тем же обходом.
        ExpressionNode::ArraySubscript(base, index) => {
            reads_expr(name, base) || reads_expr(name, index)
        }
        ExpressionNode::ArraySlice(base, _, _) => reads_expr(name, base),
        ExpressionNode::Parenthesis(a)
        | ExpressionNode::Not(a)
        | ExpressionNode::BitwiseNot(a)
        | ExpressionNode::UnaryPlus(a)
        | ExpressionNode::Negate(a)
        | ExpressionNode::BitAccess(a, _)
        | ExpressionNode::Cast(a, _)
        | ExpressionNode::NamedFunctionBox(a, _) => reads_expr(name, a),
        ExpressionNode::CodeBlock(a, _) => reads_expr(name, a),
        ExpressionNode::Function(_, args)
        | ExpressionNode::Array(args)
        | ExpressionNode::Initializer(args) => args.iter().any(|a| reads_expr(name, a)),
        ExpressionNode::Power(a, b)
        | ExpressionNode::Multiply(a, b)
        | ExpressionNode::Divide(a, b)
        | ExpressionNode::Modulo(a, b)
        | ExpressionNode::Add(a, b)
        | ExpressionNode::Subtract(a, b)
        | ExpressionNode::ShiftLeft(a, b)
        | ExpressionNode::ShiftRight(a, b)
        | ExpressionNode::BitwiseAnd(a, b)
        | ExpressionNode::BitwiseXor(a, b)
        | ExpressionNode::BitwiseOr(a, b)
        | ExpressionNode::Less(a, b)
        | ExpressionNode::More(a, b)
        | ExpressionNode::LessEqual(a, b)
        | ExpressionNode::MoreEqual(a, b)
        | ExpressionNode::Equal(a, b)
        | ExpressionNode::NotEqual(a, b)
        | ExpressionNode::And(a, b)
        | ExpressionNode::Or(a, b) => reads_expr(name, a) || reads_expr(name, b),
        ExpressionNode::ConditionalOperator(a, b, c) => {
            reads_expr(name, a) || reads_expr(name, b) || reads_expr(name, c)
        }
        ExpressionNode::None
        | ExpressionNode::Unresolved(_)
        | ExpressionNode::Number(_)
        | ExpressionNode::Duration(_)
        | ExpressionNode::Rational(_, _)
        | ExpressionNode::String(_)
        | ExpressionNode::Type(_)
        | ExpressionNode::Address(_, _)
        // Обращение по адресу переменных не читает: адрес - литерал.
        | ExpressionNode::AnonPort(_)
        | ExpressionNode::Bool(_)
        | ExpressionNode::Model(_)
        | ExpressionNode::Condition(_)
        | ExpressionNode::List(_) => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic::tree::construct_model;

    /// Разбирает тело функции `f` и отвечает, мёртв ли инициализатор `name`.
    ///
    /// Первый оператор тела - объявление; проверяется остаток.
    fn dead_in(src: &str, name: &str) -> bool {
        let (ast, _) = crate::parse(src, 0).unwrap();
        let model_rc = construct_model(&ast, None, &[]).unwrap();
        let model = model_rc.borrow();
        let def = model.search_func("f").expect("нет функции f");
        let borrowed = def.borrow();
        let crate::semantic::FunctionDefinitionNode::Local { body, .. } = &*borrowed else {
            panic!("f должна быть локальной");
        };
        let StatementNode::Block(items) = body else {
            panic!("тело должно быть блоком");
        };
        // Ищем объявление и проверяем остаток блока.
        let idx = items
            .iter()
            .position(|s| matches!(s, StatementNode::Variable(n, _, _, _) if n == name))
            .expect("нет объявления");
        initializer_is_dead(name, &items[idx + 1..])
    }

    /// **Ключевой случай корпуса** (`stacker.takt`, `travel_time`): обе ветки
    /// `if/else` переписывают переменную -> инициализатор мёртв.
    #[test]
    fn both_branches_assign_kills_initializer() {
        let src = "fn f(a: u8, b: u8) -> u8 { \
                   var ds: u8 := 0; \
                   if a > b { ds := a - b; } else { ds := b - a; } \
                   return ds; } \
                   start S;";
        assert!(dead_in(src, "ds"), "обе ветки пишут — инициализатор мёртв");
    }

    /// Безусловное присваивание убивает инициализатор.
    #[test]
    fn unconditional_assign_kills_initializer() {
        let src = "fn f(a: u8) -> u8 { var t: u8 := 0; t := a; return t; } start S;";
        assert!(dead_in(src, "t"));
    }

    /// **Контрпример:** `if` без `else` перезаписи не гарантирует.
    ///
    /// Путь мимо `if` оставляет инициализатор живым - убрать его значило бы получить
    /// чтение неинициализированной переменной.
    #[test]
    fn if_without_else_keeps_initializer() {
        let src = "fn f(a: u8) -> u8 { \
                   var t: u8 := 0; \
                   if a > 1 { t := a; } \
                   return t; } \
                   start S;";
        assert!(!dead_in(src, "t"), "путь мимо if оставляет 0 живым");
    }

    /// **Контрпример:** чтение до перезаписи оставляет инициализатор живым.
    #[test]
    fn read_before_assign_keeps_initializer() {
        let src = "fn f(a: u8) -> u8 { \
                   var t: u8 := 0; \
                   var s: u8 := t; \
                   t := a; \
                   return s + t; } \
                   start S;";
        assert!(!dead_in(src, "t"), "t прочитана до перезаписи");
    }

    /// **Контрпример:** `t := t + 1` читает `t` - инициализатор жив.
    #[test]
    fn self_referencing_assign_keeps_initializer() {
        let src = "fn f(a: u8) -> u8 { var t: u8 := 0; t := t + a; return t; } start S;";
        assert!(!dead_in(src, "t"), "правая часть читает t");
    }

    /// **Контрпример:** тело цикла может не исполниться - перезаписи нет.
    #[test]
    fn loop_body_does_not_guarantee_overwrite() {
        let src = "fn f(a: u8) -> u8 { \
                   var t: u8 := 0; \
                   while a > 1 { t := a; } \
                   return t; } \
                   start S;";
        assert!(!dead_in(src, "t"), "цикл может не исполниться ни разу");
    }

    /// **Контрпример:** одна из веток читает переменную - инициализатор жив.
    #[test]
    fn branch_reading_variable_keeps_initializer() {
        let src = "fn f(a: u8, b: u8) -> u8 { \
                   var t: u8 := 0; \
                   if a > b { t := a; } else { t := t + b; } \
                   return t; } \
                   start S;";
        assert!(!dead_in(src, "t"), "ветка else читает t");
    }

    /// Условие, читающее переменную, оставляет инициализатор живым.
    #[test]
    fn condition_reading_variable_keeps_initializer() {
        let src = "fn f(a: u8) -> u8 { \
                   var t: u8 := 0; \
                   if t > a { t := a; } else { t := 1; } \
                   return t; } \
                   start S;";
        assert!(!dead_in(src, "t"), "условие читает t");
    }
}
