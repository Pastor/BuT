//! Константный вычислитель: свёртка выражения в литерал на этапе компиляции.
//!
//! Заведён для аргументов инстанцирования модели (`M(X := Y + 1, C := 5s, D :=
//! calculate_parameter(U + 67))`), но тема самостоятельна: "какое значение у этого
//! выражения, если оно вычислимо сейчас".
//!
//! ## Свёртка в литерал, а не в свой тип значения
//!
//! Вход - [`ast::Expression`], выход - снова [`ast::Expression`], но литеральный
//! ([`fold_to_literal`]). Это не стилистика: подставив литерал обратно в дерево, мы
//! отдаём значение **существующему** конвейеру - вывод типов, понижение `float` -> `q`,
//! печать шестью целями. Свой тип значения пришлось бы учить всему этому заново, и он
//! разошёлся бы с эталоном.
//!
//! ## Что вычисляется
//!
//! - целые: литералы, `+ - * / %`, сдвиги, побитовые, унарные, сравнения;
//! - булевы: `true`/`false`, сравнения, `&& || !`;
//! - длительности: литералы (`5s`), `+`/`-` между длительностями, константы
//!   типа `duration` (наносекунды - как у [`ConditionNode::After`](crate::semantic::ConditionNode::After));
//! - имена **констант** модели и её объемлющих (цепочкой);
//! - вызовы **константных функций** (модуль [`call`]).
//!
//! ## Чего не вычисляется - и почему это решение, а не пробел
//!
//! **Арифметика над дробными.** Литерал `0.8` проходит насквозь (значение
//! доносится до объявления как есть), но `0.8 * 2` отвергается. Причина
//! содержательная: представление дробного выбирается **флагами сборки**
//! (`--float-as-q=m.n` / `--float-embedded`), а q-арифметика имеет
//! свою семантику округления (floor к −∞ у `*`, к нулю у `/`), и её
//! эталон - `takt-sim/src/eval/fixed.rs`. Посчитав здесь "как в `f64`", мы
//! получили бы значение, которого симулятор никогда не вычислит, - молча.
//! Отказ с названной причиной честнее.
//!
//! ## Переполнение
//!
//! Внутри вычислителя арифметика идёт в `i64` с обёрткой, как у вычислителя адреса.
//! Проверка "влезает ли в тип параметра" - забота применения значения: только там
//! известен целевой тип, а нормы переполнения (беззнаковое - обёртка `mod 2ⁿ`,
//! знаковое - ошибка) сформулированы про тип.
//!
//! ## Соседние арифметики
//!
//! Их в компиляторе три: адрес, выдержка `after` и эта. Правила между ними не
//! размножаются: длительности здесь хранятся в наносекундах, как в `after_const`, и
//! пересчёт в миллисекунды делает единственный `semantic::duration::value_millis`.

mod call;
mod decimal;
// Таблица целочисленных операций - одна на константное вычисление: её зовёт и этот
// вычислитель, и выражение адреса.
pub(crate) mod fixed_literal;
pub mod fixed_repr;
pub mod int_cast;
pub(crate) mod int_ops;

use crate::diagnostics::lang::keys;
use crate::diagnostics::{Diagnostic, Location};
use crate::msg;
use crate::parser::ast;
use crate::semantic::type_node::TypeNode;
use crate::semantic::{ExpressionNode, ModelNode, VariableNode};
use std::cell::RefCell;
use std::rc::Rc;

/// Предел глубины: вложенность выражения и длина цепочки констант.
///
/// То же число, что у теста глубины вложенности (`validate::depth`, `SE-062`) и у
/// вычислителя выдержки - намеренно: пределы языка не должны расходиться между собой.
const MAX_DEPTH: usize = 32;

/// Предел шагов интерпретации тела константной функции.
///
/// Тест против незавершаемости: рекурсия и долгий цикл повесили бы **и компилятор, и
/// LSP** (сервер зовёт ту же семантику при каждом нажатии).
const MAX_STEPS: usize = 100_000;

/// Значение константного выражения.
#[derive(Debug, Clone, PartialEq)]
pub enum ConstValue {
    /// Целое (в том числе `bit`: 0/1).
    Int(i128),
    /// Булево.
    Bool(bool),
    /// Длительность в наносекундах.
    Duration(i64),
    /// Агрегат: массивный литерал либо инициализатор структуры.
    ///
    /// Элементы - те же `ConstValue`, поэтому вложенность работает сама собой.
    /// Арифметики над агрегатом **нет**: смешение видов отвергает общая ветвь, и
    /// заводить для списка отдельные правила незачем.
    List(Vec<ConstValue>),
    /// Дробный литерал - **как записан** (текст, знак).
    ///
    /// Не `f64`: представление выбирают флаги сборки, и приводить к двоичной плавающей
    /// точке здесь значило бы решать за них.
    Rational(String, bool),
}

impl ConstValue {
    /// Целое значение, если оно целое - для сверки с эталоном.
    pub fn as_int(&self) -> Option<i128> {
        match self {
            ConstValue::Int(v) => Some(*v),
            _ => None,
        }
    }

    /// Длительность в наносекундах, если значение - длительность.
    pub fn as_nanos(&self) -> Option<i64> {
        match self {
            ConstValue::Duration(v) => Some(*v),
            _ => None,
        }
    }

    /// Имя вида значения - для текста диагностики.
    fn kind(&self) -> String {
        match self {
            ConstValue::Int(_) => msg!(keys::KIND_INTEGER),
            ConstValue::Bool(_) => msg!(keys::KIND_BOOLEAN),
            ConstValue::Duration(_) => msg!(keys::KIND_DURATION),
            ConstValue::Rational(_, _) => msg!(keys::KIND_RATIONAL),
            ConstValue::List(_) => msg!(keys::KIND_AGGREGATE),
        }
    }

    /// Обратно в литеральное выражение - то, что подставляется в дерево.
    pub fn to_literal(&self, loc: Location) -> ast::Expression {
        match self {
            ConstValue::Int(v) => ast::Expression::Number(loc, *v),
            ConstValue::Bool(v) => ast::Expression::Bool(loc, *v),
            // Запись синтезируется каноничной: до пользователя она не доезжает
            // (форматтер печатает исходный текст автора), нужна лишь диагностике.
            ConstValue::Duration(ns) => ast::Expression::Duration(loc, *ns, format!("{ns}ns")),
            ConstValue::Rational(text, negative) => {
                ast::Expression::Rational(loc, text.clone(), *negative)
            }
            // Агрегат печатается формой `{...}` - той же, какой его пишет автор (массив
            // и инициализатор структуры в языке записываются одинаково).
            ConstValue::List(items) => ast::Expression::Initializer(
                loc,
                items.iter().map(|item| item.to_literal(loc)).collect(),
            ),
        }
    }
}

/// Бюджет вычисления: глубина и шаги, общие на весь вызов.
#[derive(Debug, Default)]
pub struct Budget {
    depth: usize,
    steps: usize,
}

impl Budget {
    /// Новый бюджет для одного вычисления.
    pub fn new() -> Self {
        Budget { depth: 0, steps: 0 }
    }

    /// Учитывает шаг; отказывает, когда бюджет исчерпан.
    fn step(&mut self, loc: Location) -> Result<(), Diagnostic> {
        self.steps += 1;
        if self.steps > MAX_STEPS {
            return Err(limit_exceeded(loc, msg!(keys::CONST_STEPS_EXHAUSTED)));
        }
        Ok(())
    }

    /// Входит на уровень глубже; отказывает при переполнении глубины.
    fn deeper(&mut self, loc: Location) -> Result<(), Diagnostic> {
        self.depth += 1;
        if self.depth > MAX_DEPTH {
            return Err(limit_exceeded(loc, msg!(keys::CONST_DEPTH_EXHAUSTED)));
        }
        Ok(())
    }

    /// Возвращается на уровень выше.
    fn shallower(&mut self) {
        self.depth = self.depth.saturating_sub(1);
    }
}

/// `SE-085` - предел вычисления исчерпан.
fn limit_exceeded(loc: Location, what: impl AsRef<str>) -> Diagnostic {
    Diagnostic::error(loc, what.as_ref().to_string()).with_code("SE-085")
}

/// `SE-083` - выражение не сворачивается в константу; причина **названа**. Означает ли
/// диагностика вычислителя "это просто не константа".
///
/// Вычислитель отвечает **двумя родами** сообщений, и путать их нельзя:
///
/// - "не константа" (`SE-083`), "функция не вычисляется" (`SE-084`) и
///   "исчерпан бюджет вычисления" (`SE-085`) - это ответ "значение при
///   компиляции неизвестно"; потребитель вправе оставить запись как есть
///
/// - всё прочее - **ошибка самой записи** (например `SE-121`, знаковое
///   переполнение приведения), и проглотить её значит отдать потребителям
///   вход, на котором они разойдутся молча.
///
/// Различение по коду, а не по типу: у вычислителя один канал ошибок, и заводить второй
/// значило бы менять сигнатуру всех его функций ради двух случаев.
///
/// **`SE-085` (бюджет) числится "не константой", и это замер:** цикл определений `const
/// A := B; const B := A;` вычислитель обрывает пределом глубины, а автору о нём говорит
/// `SE-072` - своей диагностикой и в своём месте. Сочтя бюджет ошибкой записи, мы
/// подменили бы её сообщением о пределе (тест `const_cycle_is_se072_not_hang` это
/// поймал).
pub fn is_not_constant(diagnostic: &Diagnostic) -> bool {
    matches!(
        diagnostic.code.as_deref(),
        Some("SE-083") | Some("SE-084") | Some("SE-085")
    )
}

/// Строит диагностику `SE-083` - "выражение не вычисляется при компиляции".
pub fn not_constant(loc: Location, reason: impl AsRef<str>) -> Diagnostic {
    Diagnostic::error(
        loc,
        msg!(
            keys::SE_083_NOT_A_CONSTANT_EXPRESSION,
            reason = reason.as_ref()
        ),
    )
    .with_code("SE-083")
}

/// Сворачивает выражение в литерал либо объясняет, почему не может.
pub fn fold_to_literal(
    expr: &ast::Expression,
    scope: &Rc<RefCell<ModelNode>>,
) -> Result<ast::Expression, Diagnostic> {
    fold_to_literal_in(expr, scope, &Locals::default())
}

/// То же, но с заранее известными значениями имён.
///
/// Нужна свёртке инициализаторов объявлений: там имя переменной, объявленной
/// **выше**, означает её начальное значение. Послабление живёт **здесь**, в
/// содержимом [`Locals`], а не в [`resolve_name`]: тот же вычислитель
/// обслуживает выдержку `after`, параметры моделей и порты
/// где правило "значение переменной известно только в такте" верно и
/// менять его нельзя.
pub fn fold_to_literal_in(
    expr: &ast::Expression,
    scope: &Rc<RefCell<ModelNode>>,
    locals: &Locals,
) -> Result<ast::Expression, Diagnostic> {
    let mut budget = Budget::new();
    let loc = expr_loc(expr);
    let value = eval_in(expr, scope, locals, &mut budget)?;
    Ok(value.to_literal(loc))
}

/// Вычисляет выражение.
pub fn eval(
    expr: &ast::Expression,
    scope: &Rc<RefCell<ModelNode>>,
    budget: &mut Budget,
) -> Result<ConstValue, Diagnostic> {
    eval_in(expr, scope, &Locals::default(), budget)
}

/// Локальные значения интерпретации тела функции: параметры и `var`.
///
/// Список, а не карта: областей мало, а порядок нужен для затенения - последнее
/// объявление имени побеждает.
#[derive(Debug, Default, Clone)]
pub struct Locals {
    values: Vec<(String, ConstValue)>,
}

impl Locals {
    /// Значение имени, если оно локальное.
    fn get(&self, name: &str) -> Option<&ConstValue> {
        self.values
            .iter()
            .rev()
            .find(|(n, _)| n == name)
            .map(|(_, v)| v)
    }

    /// Объявляет (или затеняет) имя.
    pub fn declare(&mut self, name: &str, value: ConstValue) {
        self.values.push((name.to_string(), value));
    }

    /// Присваивает уже объявленному имени; `false` - имени нет.
    pub fn assign(&mut self, name: &str, value: ConstValue) -> bool {
        match self.values.iter_mut().rev().find(|(n, _)| n == name) {
            Some(slot) => {
                slot.1 = value;
                true
            }
            None => false,
        }
    }
}

/// Вычисляет выражение в контексте локальных значений.
pub fn eval_in(
    expr: &ast::Expression,
    scope: &Rc<RefCell<ModelNode>>,
    locals: &Locals,
    budget: &mut Budget,
) -> Result<ConstValue, Diagnostic> {
    use ast::Expression as E;
    let loc = expr_loc(expr);
    budget.step(loc)?;
    budget.deeper(loc)?;
    let result = (|| match expr {
        E::Number(_, v) => Ok(ConstValue::Int(*v)),
        E::Bool(_, v) => Ok(ConstValue::Bool(*v)),
        E::Duration(_, ns, _) => Ok(ConstValue::Duration(*ns)),
        E::Rational(_, text, negative) => Ok(ConstValue::Rational(text.clone(), *negative)),
        E::Parenthesis(_, inner) | E::UnaryPlus(_, inner) => eval_in(inner, scope, locals, budget),
        // Приведение вычисляется, только когда оно **тождественно**: значение целое,
        // цель - целочисленный тип, и значение в него помещается.
        //
        // Правила изменения значения (обёртка беззнакового, `SIM-003` для знакового,
        // масштаб `q`) принадлежат эталону (`takt-sim::eval`), и копии их здесь быть не
        // должно: две реализации одного правила дали бы разные значения у эталона и
        // целей. Поэтому вычислитель берётся лишь за случай, где знать эти правила не
        // нужно, а остальное честно отвергает - с названной причиной.
        E::Cast(cast_loc, inner, ty) => {
            let value = eval_in(inner, scope, locals, budget)?;
            cast_identity(&value, ty, *cast_loc, scope)
        }
        // Агрегат: `{9, 8, 7, 6}` - массив либо инициализатор структуры. Обе формы
        // записываются одинаково и вычисляются поэлементно; невычислимый элемент
        // отвергает сам себя, называя своё место.
        E::Array(_, items) | E::Initializer(_, items) => {
            let values: Result<Vec<ConstValue>, Diagnostic> = items
                .iter()
                .map(|item| eval_in(item, scope, locals, budget))
                .collect();
            Ok(ConstValue::List(values?))
        }
        E::Negate(loc, inner) => match eval_in(inner, scope, locals, budget)? {
            ConstValue::Int(v) => Ok(ConstValue::Int(v.wrapping_neg())),
            ConstValue::Duration(ns) => Ok(ConstValue::Duration(ns.wrapping_neg())),
            ConstValue::Rational(text, negative) => Ok(ConstValue::Rational(text, !negative)),
            other => Err(not_constant(
                *loc,
                msg!(keys::CONST_NEGATE_NOT_APPLICABLE, kind = other.kind()),
            )),
        },
        E::BitwiseNot(loc, inner) => match eval_in(inner, scope, locals, budget)? {
            ConstValue::Int(v) => Ok(ConstValue::Int(!v)),
            other => Err(not_constant(
                *loc,
                msg!(keys::CONST_BITWISE_NOT_NOT_APPLICABLE, kind = other.kind()),
            )),
        },
        E::Not(loc, inner) => match eval_in(inner, scope, locals, budget)? {
            ConstValue::Bool(v) => Ok(ConstValue::Bool(!v)),
            ConstValue::Int(v) => Ok(ConstValue::Bool(v == 0)),
            other => Err(not_constant(
                *loc,
                msg!(keys::CONST_NOT_NOT_APPLICABLE, kind = other.kind()),
            )),
        },
        E::Variable(id) => match locals.get(&id.name) {
            Some(value) => Ok(value.clone()),
            None => resolve_name(&id.name, id.loc, scope, budget),
        },
        E::Function(loc, id, args) => call::eval_call(id, args, *loc, scope, locals, budget),
        // Бинарные операции: единственное место арифметики этого вычислителя.
        E::Add(loc, l, r) => binary("+", l, r, *loc, scope, locals, budget),
        E::Subtract(loc, l, r) => binary("-", l, r, *loc, scope, locals, budget),
        E::Multiply(loc, l, r) => binary("*", l, r, *loc, scope, locals, budget),
        E::Divide(loc, l, r) => binary("/", l, r, *loc, scope, locals, budget),
        E::Modulo(loc, l, r) => binary("%", l, r, *loc, scope, locals, budget),
        // Целая степень.
        E::Power(loc, l, r) => binary("**", l, r, *loc, scope, locals, budget),
        E::ShiftLeft(loc, l, r) => binary("<<", l, r, *loc, scope, locals, budget),
        E::ShiftRight(loc, l, r) => binary(">>", l, r, *loc, scope, locals, budget),
        E::BitwiseAnd(loc, l, r) => binary("&", l, r, *loc, scope, locals, budget),
        E::BitwiseOr(loc, l, r) => binary("|", l, r, *loc, scope, locals, budget),
        E::BitwiseXor(loc, l, r) => binary("^", l, r, *loc, scope, locals, budget),
        E::Equal(loc, l, r) => binary("=", l, r, *loc, scope, locals, budget),
        E::NotEqual(loc, l, r) => binary("!=", l, r, *loc, scope, locals, budget),
        E::Less(loc, l, r) => binary("<", l, r, *loc, scope, locals, budget),
        E::LessEqual(loc, l, r) => binary("<=", l, r, *loc, scope, locals, budget),
        E::More(loc, l, r) => binary(">", l, r, *loc, scope, locals, budget),
        E::MoreEqual(loc, l, r) => binary(">=", l, r, *loc, scope, locals, budget),
        E::And(loc, l, r) => binary("&&", l, r, *loc, scope, locals, budget),
        E::Or(loc, l, r) => binary("||", l, r, *loc, scope, locals, budget),
        // Прочее константным не бывает: обращения к памяти, приведения, строки,
        // присваивания. Причина называется формой, а не "не годится".
        other => Err(not_constant(
            expr_loc(other),
            msg!(keys::CONST_EXPRESSION_FORM),
        )),
    })();
    budget.shallower();
    result
}

/// Применяет бинарную операцию - **единственное** место арифметики модуля.
///
/// Разъехавшись на два места (как когда-то арифметика адреса), она дала бы разное
/// значение для одного текста в зависимости от пути вычисления.
fn binary(
    op: &str,
    left: &ast::Expression,
    right: &ast::Expression,
    loc: Location,
    scope: &Rc<RefCell<ModelNode>>,
    locals: &Locals,
    budget: &mut Budget,
) -> Result<ConstValue, Diagnostic> {
    let l = eval_in(left, scope, locals, budget)?;
    let r = eval_in(right, scope, locals, budget)?;
    apply_binary(op, l, r, loc)
}

/// Арифметика, сравнения и логика над вычисленными операндами.
fn apply_binary(
    op: &str,
    left: ConstValue,
    right: ConstValue,
    loc: Location,
) -> Result<ConstValue, Diagnostic> {
    use ConstValue as V;
    match (&left, &right) {
        // -- Целые -------------------------------------------------------------
        (V::Int(a), V::Int(b)) => int_op(op, *a, *b, loc),
        // -- Длительности ------------------------------------------------------
        //
        // Только `+`/`-` и сравнения, и только между длительностями: смешение с числом
        // запрещено в языке (`SE-065`), и молча приравнять `1s` к `1` значило бы
        // завести здесь свою систему типов.
        (V::Duration(a), V::Duration(b)) => match op {
            "+" => Ok(V::Duration(a.wrapping_add(*b))),
            "-" => Ok(V::Duration(a.wrapping_sub(*b))),
            "=" => Ok(V::Bool(a == b)),
            "!=" => Ok(V::Bool(a != b)),
            "<" => Ok(V::Bool(a < b)),
            "<=" => Ok(V::Bool(a <= b)),
            ">" => Ok(V::Bool(a > b)),
            ">=" => Ok(V::Bool(a >= b)),
            _ => Err(not_constant(
                loc,
                msg!(keys::CONST_OP_ON_DURATIONS, op = op),
            )),
        },
        // -- Булевы ------------------------------------------------------------
        (V::Bool(a), V::Bool(b)) => match op {
            "&&" => Ok(V::Bool(*a && *b)),
            "||" => Ok(V::Bool(*a || *b)),
            "=" => Ok(V::Bool(a == b)),
            "!=" => Ok(V::Bool(a != b)),
            _ => Err(not_constant(loc, msg!(keys::CONST_OP_ON_BOOLEANS, op = op))),
        },
        // -- Дробные: считается точное, отвергается округляемое --
        //
        // Прежний отказ был общим, и довод верен лишь наполовину: "представление
        // выбирают флаги, округление q задано эталоном" - это про округление. Сложение,
        // вычитание и умножение десятичных литералов округления не требуют: `1.0 + 2.0`
        // есть ровно `3.0` в любом представлении, и свёрнутый литерал идёт дальше тем
        // же путём, каким уже идёт написанный автором (проверено: `:= 3.0` даёт 3.0 у
        // эталона и `48` в поле q(4, 4) цели `c`).
        //
        // Цена прежнего умолчания измерена: один вход давал ноль у
        // эталона, 3.0 у целей `c`/`rust`, молчаливую потерю у `st` и отказ у
        // `sv`; на q(4, 4) цель `c` печатала выражение в целое поле, то есть
        // 0.1875 вместо 3.0.
        // Пара "дробное с дробным" и смешанная "дробное с целым": точность у
        // них одна, и отвергать `1 + 3.14` за форму записи было бы наказанием
        // без причины (вывод типов и так даёт `Rational`).
        (V::Rational(_, _), V::Rational(_, _))
        | (V::Rational(_, _), V::Int(_))
        | (V::Int(_), V::Rational(_, _)) => {
            let as_decimal = |v: &V| match v {
                V::Rational(text, negative) => decimal::Decimal::parse(text, *negative),
                V::Int(k) => Some(decimal::Decimal::from_int(*k)),
                _ => None,
            };
            let folded = as_decimal(&left)
                .zip(as_decimal(&right))
                .and_then(|(l, r)| match op {
                    "+" => l.add(r),
                    "-" => l.sub(r),
                    "*" => l.mul(r),
                    // Деление в десятичной записи не представимо, а выбор округления -
                    // та самая часть, что задана эталоном.
                    _ => None,
                });
            match folded {
                Some(value) => {
                    let (text, negative) = value.to_text();
                    Ok(V::Rational(text, negative))
                }
                None => Err(not_constant(
                    loc,
                    msg!(keys::CONST_RATIONAL_OP_INEXACT, op = op),
                )),
            }
        }
        // Дробное со смешанным операндом (целым, булевым) - не наш случай: такие пары
        // отвергает вывод типов (`SE-059`, `SE-065`) раньше.
        (V::Rational(_, _), _) | (_, V::Rational(_, _)) => Err(not_constant(
            loc,
            msg!(
                keys::CONST_RATIONAL_MIXED,
                op = op,
                kind = if matches!(left, V::Rational(_, _)) {
                    right.kind()
                } else {
                    left.kind()
                }
            ),
        )),
        // -- Смешение видов ----------------------------------------------------
        _ => Err(not_constant(
            loc,
            msg!(
                keys::CONST_MIXED_KINDS,
                op = op,
                left = left.kind(),
                right = right.kind()
            ),
        )),
    }
}

/// Целочисленная операция.
fn int_op(op: &str, a: i128, b: i128, loc: Location) -> Result<ConstValue, Diagnostic> {
    use int_ops::{IntOpError, IntOutcome};
    match int_ops::int_binary(op, a, b) {
        Ok(IntOutcome::Int(v)) => Ok(ConstValue::Int(v)),
        Ok(IntOutcome::Bool(v)) => Ok(ConstValue::Bool(v)),
        Err(IntOpError::DivisionByZero) => {
            Err(not_constant(loc, msg!(keys::CONST_DIVISION_BY_ZERO)))
        }
        Err(IntOpError::RemainderByZero) => {
            Err(not_constant(loc, msg!(keys::CONST_REMAINDER_BY_ZERO)))
        }
        // Та же граница, что у выражения адреса и у нормы переполнения.
        Err(IntOpError::ShiftOutOfRange) => {
            Err(not_constant(loc, msg!(keys::CONST_SHIFT_OUT_OF_RANGE)))
        }
        // Показатель степени вне `u32`: значение остаётся невычисленным - то есть
        // поведение прежнее, а не новый отказ.
        Err(IntOpError::ExponentOutOfRange) => {
            Err(not_constant(loc, msg!(keys::CONST_EXPONENT_OUT_OF_RANGE)))
        }
        Err(IntOpError::UnsupportedOperator) => Err(not_constant(
            loc,
            msg!(keys::CONST_OP_NOT_EVALUATED, op = op),
        )),
    }
}

/// Разрешает имя: только **константа** модели или её объемлющих.
///
/// Переменная и порт отвергаются с прямым указанием на причину: их значение известно
/// лишь в такте, и подставить его при сборке нельзя.
fn resolve_name(
    name: &str,
    loc: Location,
    scope: &Rc<RefCell<ModelNode>>,
    budget: &mut Budget,
) -> Result<ConstValue, Diagnostic> {
    // Вариант перечисления - значение, известное при компиляции: имя занято объявлением
    // `enum`, и в такте оно не меняется.
    //
    // Перечисление спрашивается после переменных: объявленная переменная затеняет
    // одноимённый вариант - тот же порядок, что у разрешения выражений
    // (`expression::resolve_expr`).
    let Some(found) = scope.borrow().search_var(name) else {
        if let Some((_, value)) = scope.borrow().search_enum_variant(name) {
            return Ok(ConstValue::Int(value));
        }
        return Err(not_constant(
            loc,
            msg!(keys::CONST_NAME_NOT_DECLARED, name = name),
        ));
    };
    match found {
        VariableNode::Const { expr, .. } => eval_node(&expr, loc, scope, budget),
        VariableNode::Simple { .. } => Err(not_constant(
            loc,
            msg!(keys::CONST_NAME_IS_A_VARIABLE, name = name),
        )),
        VariableNode::Port { .. } => Err(not_constant(
            loc,
            msg!(keys::CONST_NAME_IS_A_PORT, name = name),
        )),
        VariableNode::Unresolved => Err(not_constant(
            loc,
            msg!(keys::CONST_DECLARATION_UNRESOLVED, name = name),
        )),
    }
}

/// Вычисляет значение объявления константы.
///
/// Значение приходит и сырым АСД (`Unresolved`, порядок объявлений), и уже понижённым
/// узлом - оба пути штатны. Вычисляет понижённый узел выражения, в отличие от [`eval`],
/// который принимает сырое АСД.
///
/// Нужна потребителям за пределами семантики: цель `sv` спрашивает вычислимость
/// инициализатора вместо того, чтобы судить по виду узла.
pub fn eval_node_public(
    node: &ExpressionNode,
    loc: Location,
    scope: &Rc<RefCell<ModelNode>>,
) -> Result<ConstValue, Diagnostic> {
    let mut budget = Budget::default();
    eval_node(node, loc, scope, &mut budget)
}

fn eval_node(
    node: &ExpressionNode,
    loc: Location,
    scope: &Rc<RefCell<ModelNode>>,
    budget: &mut Budget,
) -> Result<ConstValue, Diagnostic> {
    budget.deeper(loc)?;
    let result = match node {
        ExpressionNode::Unresolved(expr) => eval(expr, scope, budget),
        ExpressionNode::Number(v) => Ok(ConstValue::Int(*v)),
        ExpressionNode::Duration(ns) => Ok(ConstValue::Duration(*ns)),
        ExpressionNode::Parenthesis(inner) => eval_node(inner, loc, scope, budget),
        ExpressionNode::Variable(cell) => {
            let var = cell.borrow().clone();
            match var {
                VariableNode::Const { expr, .. } => eval_node(&expr, loc, scope, budget),
                other => Err(not_constant(
                    loc,
                    msg!(keys::CONST_NOT_A_CONSTANT_NAME, name = other.name()),
                )),
            }
        }
        _ => Err(not_constant(
            loc,
            msg!(keys::CONST_CONSTANT_VALUE_NOT_EVALUATED),
        )),
    };
    budget.shallower();
    result
}

/// Значение приведения целого.
///
/// Границы берутся у единственного носителя
/// [`type_range`](crate::semantic::validate::literal_range::type_range) - того же,
/// которым судит `SE-089`: второй список границ разъехался бы с первым.
///
/// # Ошибки
///
/// "Не константа" - с причиной: тип не целочисленный либо значение приведением
/// изменится (усечение, обёртка, масштаб `q`). Разрешает АСД-тип цели приведения в узел
/// семантики.
///
/// Имя встроенного типа спрашивается у `builtin_type_by_name` - носителя списка:
/// `ast_type_to_node_ctx` разрешает лишь `bit`/`bool`/`float` и пользовательские
/// псевдонимы, а `u8`...`i64` для него - `Unsupported`.
fn target_of(ty: &ast::Type, scope: &Rc<RefCell<ModelNode>>) -> TypeNode {
    match ty {
        ast::Type::Alias(id) => crate::semantic::type_node::builtin_type_by_name(&id.name)
            .unwrap_or_else(|| {
                crate::semantic::type_inference::ast_type_to_node_ctx(ty, Rc::clone(scope))
            }),
        other => crate::semantic::type_inference::ast_type_to_node_ctx(other, Rc::clone(scope)),
    }
}

/// Значение приведения к `q(m, n)` - точным счётом.
///
/// Возвращает дробный литерал, а не готовое представление: дальше он идёт тем же путём,
/// что литерал автора - понижением и свёрткой, - и второй ветки для уже понижённого
/// значения не заводится.
///
/// Текст точен всегда: знаменатель представления - степень двойки, поэтому `repr · 2⁻ⁿ`
/// конечен в десятичной записи. Именно поэтому `SE-058` ("литерал не представим точно")
/// на результат не срабатывает - он представим по построению.
///
/// `None` - форма, которую носитель не считает (булево, длительность, агрегат,
/// переполнение промежутка): пусть отвечает прежняя ветвь.
fn fixed_cast(value: &ConstValue, m: u8, n: u8, sat: bool) -> Option<ConstValue> {
    let repr = match value {
        ConstValue::Int(v) => fixed_repr::from_int(*v, n),
        ConstValue::Rational(text, negative) => {
            let decimal = decimal::Decimal::parse(text, *negative)?;
            let (mantissa, scale) = decimal.parts();
            fixed_repr::from_decimal(mantissa, scale, n)?
        }
        ConstValue::Bool(_) | ConstValue::Duration(_) | ConstValue::List(_) => return None,
    };
    let normalized = fixed_repr::normalize(repr, m, n, sat);
    let (text, negative) = fixed_repr::to_decimal_text(normalized, n);
    Some(ConstValue::Rational(text, negative))
}

/// Значение приведения агрегата к массиву.
///
/// Длина обязана совпасть с объявленной; элементы приводятся к типу элемента
/// **тем же** правилом целого - второго знания о переносе не заводится.
///
/// `None` - если тип элемента не целочисленный (структура, вложенный массив, `q`): их
/// представление завязано на `Value` эталона, и прежняя ветвь отвечает за них
/// по-прежнему.
fn array_cast(items: &[ConstValue], size: u16, elem: &TypeNode) -> Option<ConstValue> {
    if items.len() != usize::from(size) {
        return None;
    }
    let TypeNode::Integer { bits, signed } = elem else {
        return None;
    };
    let mut folded = Vec::with_capacity(items.len());
    for item in items {
        let ConstValue::Int(value) = item else {
            return None;
        };
        folded.push(ConstValue::Int(
            int_cast::integer(*value, *bits, *signed).ok()?,
        ));
    }
    Some(ConstValue::List(folded))
}

fn cast_identity(
    value: &ConstValue,
    ty: &ast::Type,
    loc: Location,
    scope: &Rc<RefCell<ModelNode>>,
) -> Result<ConstValue, Diagnostic> {
    // Дробная цель считается общим носителем представления: масштаб на 2ⁿ, округление
    // floor к −∞, перенос либо насыщение по `W = m + n`.
    if let TypeNode::Fixed { m, n, sat } = target_of(ty, scope)
        && let Some(text) = fixed_cast(value, m, n, sat)
    {
        return Ok(text);
    }
    // Приведение к `duration` - мост через миллисекунды (решение ), и пересчёт делает
    // единственный носитель `semantic::duration`.
    if matches!(target_of(ty, scope), TypeNode::Duration)
        && let ConstValue::Int(millis) = value
        && let Ok(millis) = i64::try_from(*millis)
        && let Some(nanos) = crate::semantic::duration::from_millis(millis)
    {
        return Ok(ConstValue::Duration(nanos));
    }
    // Обратное направление - `duration as ЦЕЛОЕ` - тот же мост и тот же носитель.
    //
    // Дальше значение идёт общим путём целочисленного приведения: миллисекунды могут не
    // поместиться в узкий тип, и правило переноса тут ровно то же.
    if let ConstValue::Duration(nanos) = value
        && let TypeNode::Integer { bits, signed } = target_of(ty, scope)
    {
        let millis = i128::from(crate::semantic::duration::to_millis(*nanos));
        return match int_cast::integer(millis, bits, signed) {
            Ok(value) => Ok(ConstValue::Int(value)),
            Err(overflow) => Err(Diagnostic::error(
                loc,
                msg!(
                    keys::SE_121_DURATION_CAST_OVERFLOW,
                    bits = overflow.bits,
                    value = overflow.value
                ),
            )
            .with_code("SE-121")),
        };
    }
    // Агрегат к массиву: элементы приводятся к типу элемента тем же правилом целого,
    // длина обязана совпасть с объявленной.
    if let ast::Type::Array {
        element_count,
        element_type,
        ..
    } = ty
        && let ConstValue::List(items) = value
        // Тип элемента разрешается тем же `target_of`, а не общим обходом:
        // `ast_type_to_node_ctx` не знает имён `u8`...`i64` (для него это
        // `Unsupported`), и рекурсия внутри него дала бы для `[u8; 2]` массив
        // неподдержанных элементов - ветвь не сработала бы ни на одном входе.
        && let Some(folded) = array_cast(items, *element_count, &target_of(element_type, scope))
    {
        return Ok(folded);
    }
    let ConstValue::Int(n) = value else {
        return Err(not_constant(loc, msg!(keys::CONST_CAST_NEEDS_INTEGER)));
    };
    let target = target_of(ty, scope);
    // Целочисленная цель считается общим носителем правила: беззнаковое оборачивается
    // `mod 2ⁿ`, знаковое вне диапазона - ошибка.
    if let TypeNode::Integer { bits, signed } = target {
        return match int_cast::integer(*n, bits, signed) {
            Ok(value) => Ok(ConstValue::Int(value)),
            // Это ошибка, а не "не константа": знаковое переполнение есть ошибка
            // программы, и оставить запись невычислимой значило бы отдать её
            // потребителям - эталон дал бы `0` молча, цели
            // `c`/`rust` - `44`, `st` потеряла бы инициализатор.
            Err(overflow) => Err(Diagnostic::error(
                loc,
                msg!(
                    keys::SE_121_CAST_OVERFLOW,
                    bits = overflow.bits,
                    value = overflow.value
                ),
            )
            .with_code("SE-121")),
        };
    }
    // Прочие цели (`bit`, `bool`, длительность, массив) вычисляются, лишь если
    // приведение ничего не меняет: правила их изменения завязаны на представление
    // значения эталона, и копия здесь разошлась бы значениями (довод - он в силе).
    let Some((min, max)) = crate::semantic::validate::literal_range::type_range(&target) else {
        return Err(not_constant(loc, msg!(keys::CONST_CAST_TYPE_NOT_EVALUATED)));
    };
    if *n < min || *n > max {
        return Err(not_constant(
            loc,
            msg!(
                keys::CONST_CAST_CHANGES_VALUE,
                value = n,
                min = min,
                max = max
            ),
        ));
    }
    Ok(ConstValue::Int(*n))
}

/// Имя типа для диагностики - как его написал бы автор.
#[allow(dead_code)]
fn type_name(ty: &TypeNode) -> String {
    ty.to_string()
}

/// Позиция выражения - для диагностики о нём.
pub fn expr_loc(expr: &ast::Expression) -> Location {
    expr.loc()
}
