//! Вычисление константной выдержки `after ...` в наносекунды.
//!
//! ## Что делает
//!
//! Сводит АСД-узел [`ast::Condition::AfterExpr`] - `after DWELL`, `after (BASE + 30s)` -
//! к **тому же** [`ConditionNode::After(нс)`](ConditionNode::After), который даёт
//! литеральная форма `after 3m`. За границей семантики этой формы не существует: ни
//! один генератор и симулятор о ней не знают, поэтому шесть целей не могут по ней
//! разойтись.
//!
//! ## Что принимается
//!
//! Арифметика **над длительностями**, вычислимая компилятором:
//!
//! - литерал длительности (`3m`, `1m30s`);
//! - имя константы (`const`) типа `duration`, в том числе цепочкой
//!   (`const A := 3m; const B := A;`);
//! - скобки и операторы `+` / `-` между длительностями.
//!
//! Всё прочее - `SE-072` с **названной причиной**: молчаливая выдержка "ноль" здесь
//! дороже отказа. В частности отвергаются:
//!
//! - **вызов функции** (`after (f(1))`) - требование 2026-07-29:
//!   значение вызова компилятору неизвестно;
//! - **голое число** (`after (v + 1)`) - длительность сочетается только с
//!   длительностью, как и везде в языке (`SE-065`); пишется `v + 1s`;
//! - **переменная** (`after (v + 1s)` при `var v: duration`) - значение известно
//!   лишь в такте. в объёме 0143 - только
//!   константы, вычисляемая выдержка заводится отдельной фичей. Причина
//!   технически жёсткая: тип `duration` **не поддерживает ни одна цель**
//!   генерации (`CC-020`, `RS-023`, `ST-...`, `SV-...`), то есть выражение с
//!   переменной было бы исполнимо симулятором и невыразимо в прошивке.
//!
//! **Приведения `as duration` здесь нет** - не по решению этой фичи, а потому что
//! оператор `as` живёт только в грамматике **выражений**, тогда как аргумент `after` -
//! условие. Само приведение в языке есть (`left := 250 as duration;` даёт `250ms`), но
//! внутри выдержки его не записать.
//!
//! **Умножения и деления в выражении выдержки нет:** грамматика условий знает только
//! `+` и `-`. Это ограничение формы, а не решение о семантике.
//!
//! **Константы в тактах не бывает.** Литерал `3t` существует только внутри `after`, в
//! выражениях его нет, поэтому `const D := 3t;` не объявить и константная тактовая
//! выдержка недостижима. Именно поэтому `time_ast::raw_has_after_kind` относит эту
//! форму к **длительностным**.

use crate::diagnostics::{Diagnostic, Location};
use crate::parser::ast;
use crate::semantic::type_node::TypeNode;
use crate::semantic::{ConditionNode, ExpressionNode, ModelNode, VariableNode};
use std::cell::RefCell;
use std::rc::Rc;

/// Предел глубины вычисления: длина цепочки констант и вложенность выражения.
///
/// Тест против **цикла** (`const A := B; const B := A;`): без предела разбор
/// зациклился бы и инструмент завис бы без диагностики - тот же класс, который снимала
/// в обходах верификации. То же число, что у теста глубины вложенности
/// (`validate::depth`), - намеренно, чтобы пределы языка не расходились.
const MAX_DEPTH: usize = 32;

/// Почему выражение выдержки не годится. Текст диагностики называет причину, потому что
/// "ожидалось duration" (прежняя `SY-002`) не говорит автору, что делать.
enum Cause {
    /// Имени нет в области видимости (ни своей, ни родительских).
    Undeclared(String),
    /// Имя есть, но это переменная, порт или неразрешённое объявление.
    NotConst(String, &'static str),
    /// Имя есть, и это **параметр модели**.
    ///
    /// Отдельная причина, а не разновидность [`Cause::NotConst`], по двум доводам.
    /// Во-первых, ответ автору другой: параметр становится константой сменой режима
    /// генерации, а переменная - нет, поэтому диагностика **называет режим** (п.
    /// Во-вторых - и это важнее - `NotConst` уводит разбор в **вычисляемую** выдержку,
    /// то есть параметр молча получил бы 32-битный счётчик, другой код и отказ `ST-016`
    /// в цели `st`; ровно та молчаливая деградация, которую решение запрещает.
    Parameter(String),
    /// Константа есть, но её тип - не `duration`.
    WrongType(String, String),
    /// Значение константы не сводится к литералу длительности.
    NotLiteral(String),
    /// Голое число: длительность сочетается только с длительностью.
    BareNumber(i128),
    /// Вызов функции - значение компилятору неизвестно.
    FunctionCall(String),
    /// Форма, которой в константной выдержке быть не может (сравнение, логика, строка,
    /// доступ к биту и т.
    NotConstantForm(&'static str),
    /// Результат отрицателен - выдержки "минус две секунды" не бывает.
    Negative(i64),
    /// Переполнение при сложении/вычитании наносекунд.
    Overflow,
    /// Цепочка/вложенность глубже предела - практически цикл.
    Cycle,
}

impl Cause {
    fn message(&self) -> String {
        match self {
            Cause::Undeclared(name) => format!(
                "выдержка 'after': константа '{name}' не объявлена — 'after' принимает \
                 литерал длительности (after 3m), литерал тактов (after 3t), имя \
                 константы типа duration либо константное выражение над \
                 длительностями (after (BASE + 30s))"
            ),
            Cause::Parameter(name) => {
                crate::semantic::parameter_const::compile_time_parameter_text(
                    name,
                    "выдержка 'after'",
                )
            }
            Cause::NotConst(name, kind) => format!(
                "выдержка 'after': '{name}' — это {kind}, а выдержке нужна константа \
                 типа duration (const {name} := 3m;); значение, известное только в \
                 такте, компилятор подставить не может"
            ),
            Cause::WrongType(name, ty) => format!(
                "выдержка 'after': константа '{name}' имеет тип {ty}, а выдержке нужен \
                 duration"
            ),
            Cause::NotLiteral(name) => format!(
                "выдержка 'after': значение константы '{name}' не сводится к литералу \
                 длительности — допустимы литерал (const {name} := 3m;), имя другой \
                 такой константы либо их сумма и разность"
            ),
            Cause::BareNumber(n) => format!(
                "выдержка 'after': число {n} без единицы времени — длительность \
                 сочетается только с длительностью (напишите {n}s, {n}ms или {n}us)"
            ),
            Cause::FunctionCall(name) => format!(
                "выдержка 'after': вызов функции '{name}' в выражении выдержки \
                 недопустим — её значение компилятору неизвестно"
            ),
            Cause::NotConstantForm(what) => format!(
                "выдержка 'after': {what} в выражении выдержки недопустимо — \
                 допустимы литералы длительности, константы типа duration, скобки \
                 и операторы '+' и '-'"
            ),
            Cause::Negative(nanos) => format!(
                "выдержка 'after': выражение даёт {nanos} нс — отрицательной выдержки \
                 не бывает"
            ),
            Cause::Overflow => "выдержка 'after': переполнение при вычислении \
                 длительности (наносекунды не укладываются в 64-битное целое)"
                .to_string(),
            Cause::Cycle => format!(
                "выдержка 'after': вычисление глубже {MAX_DEPTH} уровней не \
                 заканчивается литералом длительности — вероятно, константы \
                 ссылаются друг на друга"
            ),
        }
    }
}

/// Вычисляет `after <константное выражение>` в [`ConditionNode::After`].
///
/// Позиция диагностики - узел, на котором вычисление остановилось: автор правит именно
/// его, а не всю выдержку.
///
/// # Ошибки
///
/// `SE-072` - выражение не сводится к длительности: неизвестное имя, не константа, не
/// тот тип, голое число, вызов функции, неподходящая форма, отрицательный результат,
/// переполнение или цикл в цепочке констант.
pub(super) fn resolve_after_expr(
    cond: &ast::Condition,
    model: Rc<RefCell<ModelNode>>,
) -> Result<ConditionNode, Diagnostic> {
    match nanos_of_cond(cond, model.clone(), 0) {
        Ok(nanos) if nanos < 0 => Err(fail(cond.loc(), &Cause::Negative(nanos))),
        Ok(nanos) => Ok(ConditionNode::After(nanos)),
        // Значение известно лишь в такте (переменная или порт) - выдержка становится
        // **вычисляемой**. Прочие причины остаются ошибками: вызов функции компилятору
        // неизвестен, голое число не имеет единицы времени, сравнение - не
        // длительность.
        Err((_, Cause::NotConst(..))) => resolve_dynamic(cond, model),
        Err((loc, cause)) => Err(fail(loc, &cause)),
    }
}

/// Строит **вычисляемую** выдержку [`ConditionNode::AfterExpr`].
///
/// Значение считает исполнитель: симулятор в наносекундах, цели в миллисекундах.
/// Поэтому здесь проверяется не значение, а **форма и типы**: допустимы литералы
/// длительности, переменные, порты и константы типа `duration`, скобки, `+`, `-`.
/// Смешение с числом запрещено языком (`SE-065`), и выдержка исключения не делает.
fn resolve_dynamic(
    cond: &ast::Condition,
    model: Rc<RefCell<ModelNode>>,
) -> Result<ConditionNode, Diagnostic> {
    check_dynamic_form(cond, model.clone(), 0).map_err(|(loc, cause)| fail(loc, &cause))?;
    let inner = crate::semantic::condition::resolve_condition(cond, model)?;
    Ok(ConditionNode::AfterExpr(Box::new(inner)))
}

/// Проверяет, что выражение вычисляемой выдержки состоит только из длительностей.
///
/// Разбор исчерпывающий по узлу условия: новая форма языка обязана получить здесь
/// решение, а не унаследовать чужое умолчание.
fn check_dynamic_form(
    cond: &ast::Condition,
    model: Rc<RefCell<ModelNode>>,
    depth: usize,
) -> Result<(), Failure> {
    if depth > MAX_DEPTH {
        return Err((cond.loc(), Cause::Cycle));
    }
    match cond {
        ast::Condition::Duration(_, _, _) => Ok(()),
        ast::Condition::Parenthesis(_, inner) => check_dynamic_form(inner, model, depth + 1),
        ast::Condition::Add(_, left, right) | ast::Condition::Subtract(_, left, right) => {
            check_dynamic_form(left, model.clone(), depth + 1)?;
            check_dynamic_form(right, model, depth + 1)
        }
        ast::Condition::Variable(id) => check_duration_operand(id, model),
        // Всё прочее - те же причины, что у константной формы: сообщение и код общие,
        // поэтому решение принимает один разбор.
        other => match nanos_of_cond(other, model, depth) {
            Ok(_) => Ok(()),
            // Переменная внутри выражения законна - проверяется своей ветвью.
            Err((_, Cause::NotConst(..))) => Ok(()),
            Err(failure) => Err(failure),
        },
    }
}

/// Операнд-имя вычисляемой выдержки обязан быть значением типа `duration`.
fn check_duration_operand(
    id: &ast::Identifier,
    model: Rc<RefCell<ModelNode>>,
) -> Result<(), Failure> {
    let var = model
        .borrow()
        .search_var(&id.name)
        .ok_or_else(|| (id.loc, Cause::Undeclared(id.name.clone())))?;
    let ty = match &var {
        // Параметр внутри смешанного выражения (`after (PARAM + 1s)`) - та же
        // диагностика, что и у одиночного: иначе выдержка стала бы вычисляемой молча, и
        // режим остался бы неназванным.
        VariableNode::Simple { .. } if crate::semantic::parameter_const::is_parameter(&var) => {
            return Err((id.loc, Cause::Parameter(id.name.clone())));
        }
        VariableNode::Simple { ty, .. } | VariableNode::Port { ty, .. } => ty.clone(),
        // Константа сюда доходит только внутри смешанного выражения (`v + DWELL`): её
        // значение известно, но всё выражение - нет.
        VariableNode::Const { ty, .. } => ty.clone(),
        VariableNode::Unresolved => {
            return Err((
                id.loc,
                Cause::NotConst(id.name.clone(), "неразрешённое объявление"),
            ));
        }
    };
    // `Inference`/`Unsupported` - "вывод типов не дошёл"; у **переменной** такой тип
    // означает, что длительностью её объявить не удалось, и принимать её здесь значило
    // бы гадать. Требуем явного `duration`.
    if matches!(ty, TypeNode::Duration) {
        Ok(())
    } else {
        Err((id.loc, Cause::WrongType(id.name.clone(), type_name(&ty))))
    }
}

fn fail(loc: Location, cause: &Cause) -> Diagnostic {
    // Параметр в позиции compile-time величины - свой код (`SE-088`, R12 ): он о режиме
    // генерации, а не о форме выдержки, и тест обязан отличать одно от другого.
    let code = if matches!(cause, Cause::Parameter(_)) {
        crate::semantic::parameter_const::COMPILE_TIME_PARAMETER_CODE
    } else {
        "SE-072"
    };
    Diagnostic::error(loc, cause.message()).with_code(code)
}

/// Ошибка вычисления: позиция виновного узла и причина.
type Failure = (Location, Cause);

/// Наносекунды условия-выражения.
///
/// Вычисление идёт **по сырому АСД**, а не по понижённому узлу: значение нужно
/// раньше, чем условие станет `ConditionNode`, - оно и определяет, каким узлом
/// условие станет. Тот же приём, что у вычислителя адреса, но
/// **не** его дубль: там целые адреса без типов, здесь типизированные
/// длительности с запретом смешения.
fn nanos_of_cond(
    cond: &ast::Condition,
    model: Rc<RefCell<ModelNode>>,
    depth: usize,
) -> Result<i64, Failure> {
    if depth > MAX_DEPTH {
        return Err((cond.loc(), Cause::Cycle));
    }
    match cond {
        ast::Condition::Duration(_, nanos, _) => Ok(*nanos),
        ast::Condition::Parenthesis(_, inner) => nanos_of_cond(inner, model, depth + 1),
        ast::Condition::Variable(id) => nanos_of_name(id, model, depth + 1),
        ast::Condition::Add(loc, left, right) => {
            let left = nanos_of_cond(left, model.clone(), depth + 1)?;
            let right = nanos_of_cond(right, model, depth + 1)?;
            left.checked_add(right).ok_or((*loc, Cause::Overflow))
        }
        ast::Condition::Subtract(loc, left, right) => {
            let left = nanos_of_cond(left, model.clone(), depth + 1)?;
            let right = nanos_of_cond(right, model, depth + 1)?;
            left.checked_sub(right).ok_or((*loc, Cause::Overflow))
        }
        // Голое число - самая частая ошибка автора (пример `after ((v + 1) - f)`):
        // называем единицу времени прямо в сообщении.
        ast::Condition::Number(loc, n) => Err((*loc, Cause::BareNumber(*n))),
        ast::Condition::Function(loc, id, _) => Err((*loc, Cause::FunctionCall(id.name.clone()))),
        // Формы, которых в константной длительности быть не может. Перечислены явно (а
        // не `_ =>`): новый узел условия обязан получить решение здесь, а не
        // унаследовать чужое умолчание.
        ast::Condition::After(loc, _, _)
        | ast::Condition::AfterTicks(loc, _, _)
        | ast::Condition::AfterExpr(loc, _) => {
            Err((*loc, Cause::NotConstantForm("вложенная выдержка 'after'")))
        }
        ast::Condition::Not(loc, _) => Err((*loc, Cause::NotConstantForm("логическое отрицание"))),
        ast::Condition::And(loc, _, _) | ast::Condition::Or(loc, _, _) => {
            Err((*loc, Cause::NotConstantForm("побитовая операция")))
        }
        ast::Condition::Less(loc, _, _)
        | ast::Condition::More(loc, _, _)
        | ast::Condition::LessEqual(loc, _, _)
        | ast::Condition::MoreEqual(loc, _, _)
        | ast::Condition::Equal(loc, _, _)
        | ast::Condition::NotEqual(loc, _, _) => Err((*loc, Cause::NotConstantForm("сравнение"))),
        ast::Condition::BitAccess(loc, _, _) => {
            Err((*loc, Cause::NotConstantForm("обращение к биту")))
        }
        ast::Condition::ArraySubscript(loc, _, _) => {
            Err((*loc, Cause::NotConstantForm("обращение к элементу массива")))
        }
        ast::Condition::Rational(loc, _, _) => {
            Err((*loc, Cause::NotConstantForm("вещественное число")))
        }
        ast::Condition::Bool(loc, _) => Err((*loc, Cause::NotConstantForm("булев литерал"))),
        // Значение ячейки известно только во время работы автомата - выдержка,
        // вычисляемая компилятором, из него сложиться не может.
        ast::Condition::AnonAddress(loc, _, _) => {
            Err((*loc, Cause::NotConstantForm("обращение к ячейке по адресу")))
        }
        ast::Condition::String(parts) => Err((
            parts.first().map(|p| p.loc).unwrap_or(Location::Implicit),
            Cause::NotConstantForm("строка"),
        )),
    }
}

/// Наносекунды константы с именем `id` (с обходом цепочки `upper`).
fn nanos_of_name(
    id: &ast::Identifier,
    model: Rc<RefCell<ModelNode>>,
    depth: usize,
) -> Result<i64, Failure> {
    if depth > MAX_DEPTH {
        return Err((id.loc, Cause::Cycle));
    }
    let var = model
        .borrow()
        .search_var(&id.name)
        .ok_or_else(|| (id.loc, Cause::Undeclared(id.name.clone())))?;
    nanos_of_var(&var, &id.name, id.loc, model, depth)
}

/// Наносекунды объявления: константа типа `duration` со значением-длительностью.
fn nanos_of_var(
    var: &VariableNode,
    name: &str,
    loc: Location,
    model: Rc<RefCell<ModelNode>>,
    depth: usize,
) -> Result<i64, Failure> {
    let (ty, expr) = match var {
        VariableNode::Const { ty, expr, .. } => (ty, expr),
        // Изменяемая переменная и порт негодны **по сути**, а не по типу: их значение
        // известно лишь в такте, а выдержка нужна компилятору ( - см. шапку модуля).
        VariableNode::Simple { .. } => {
            // Параметр модели - не "просто переменная": он станет константой в режиме
            // `--parameters=specialize`, и об этом сказано прямо, а не молчаливым
            // уходом в вычисляемую выдержку (R12 ).
            if crate::semantic::parameter_const::is_parameter(var) {
                return Err((loc, Cause::Parameter(name.to_string())));
            }
            return Err((loc, Cause::NotConst(name.to_string(), "переменная")));
        }
        VariableNode::Port { .. } => {
            return Err((loc, Cause::NotConst(name.to_string(), "порт")));
        }
        VariableNode::Unresolved => {
            return Err((
                loc,
                Cause::NotConst(name.to_string(), "неразрешённое объявление"),
            ));
        }
    };
    // `Inference` и `Unsupported` - не "другой тип", а "вывод типа сюда не дошёл": так
    // выглядят звено цепочки `const DWELL := BASE;` и арифметика `const DWELL := BASE +
    // 1s;` (обе формы сняты пробой 0143). Арбитром в этих случаях становится
    // **значение**: сводится к длительности - принимаем, иначе автор получит
    // `NotLiteral`. Отказ "тип _" сообщал бы автору о слабости вывода типов, а не о его
    // записи.
    if !matches!(
        ty,
        TypeNode::Duration | TypeNode::Inference | TypeNode::Unsupported
    ) {
        return Err((loc, Cause::WrongType(name.to_string(), type_name(ty))));
    }
    nanos_of_expr(expr, name, loc, model, depth)
}

/// Наносекунды значения константы: литерал, скобки, сумма/разность или имя другой такой
/// константы.
fn nanos_of_expr(
    expr: &ExpressionNode,
    name: &str,
    loc: Location,
    model: Rc<RefCell<ModelNode>>,
    depth: usize,
) -> Result<i64, Failure> {
    if depth > MAX_DEPTH {
        return Err((loc, Cause::Cycle));
    }
    match expr {
        ExpressionNode::Duration(nanos) => Ok(*nanos),
        // Тот же литерал, не дошедший до понижения.
        ExpressionNode::Unresolved(ast::Expression::Duration(_, nanos, _)) => Ok(*nanos),
        ExpressionNode::Parenthesis(inner) => nanos_of_expr(inner, name, loc, model, depth + 1),
        ExpressionNode::Add(left, right) => {
            let left = nanos_of_expr(left, name, loc, model.clone(), depth + 1)?;
            let right = nanos_of_expr(right, name, loc, model, depth + 1)?;
            left.checked_add(right).ok_or((loc, Cause::Overflow))
        }
        ExpressionNode::Subtract(left, right) => {
            let left = nanos_of_expr(left, name, loc, model.clone(), depth + 1)?;
            let right = nanos_of_expr(right, name, loc, model, depth + 1)?;
            left.checked_sub(right).ok_or((loc, Cause::Overflow))
        }
        // Звено цепочки: значение уже разрешено в объявление.
        ExpressionNode::Variable(next) => {
            let next_var = next.borrow().clone();
            let next_name = next_var.name().to_string();
            nanos_of_var(&next_var, &next_name, loc, model, depth + 1)
        }
        // Звено цепочки, не дошедшее до разрешения (порядок объявлений): имя ищется в
        // области видимости так же, как в исходной точке входа.
        ExpressionNode::Unresolved(ast::Expression::Variable(id)) => {
            let var = model
                .borrow()
                .search_var(&id.name)
                .ok_or_else(|| (loc, Cause::Undeclared(id.name.clone())))?;
            nanos_of_var(&var, &id.name, loc, model, depth + 1)
        }
        _ => Err((loc, Cause::NotLiteral(name.to_string()))),
    }
}

/// Имя типа для текста диагностики - **как его написал бы автор**.
///
/// Через `Display`, а не `Debug`: сообщение читает автор программы на Takt, и
/// `Bit`/`Array(Bit, 8)` из отладочной печати ему ни о чём не говорят.
fn type_name(ty: &TypeNode) -> String {
    ty.to_string()
}

#[cfg(test)]
mod tests {
    use crate::parse;
    use crate::semantic::tree::construct_model;
    use crate::semantic::{ConditionNode, ModelNode};

    /// Разбирает исходник и возвращает разрешённое условие первого `ref`-ребра
    /// состояния `Wait` модели `M`.
    fn ref_cond(src: &str) -> Result<ConditionNode, String> {
        let (ast, _) = parse(src, 0).map_err(|e| format!("разбор: {e:?}"))?;
        let node: ModelNode = construct_model(&ast, None, &[])
            .map_err(|d| format!("{}|{}", d.code.clone().unwrap_or_default(), d.message))?
            .take();
        let model = node.search_model("M").expect("модель M");
        let model = model.borrow();
        let state = model.states.get("Wait").expect("состояние Wait");
        Ok(state.references()[0].cond.clone())
    }

    /// Каркас модели: `%DECL%` - объявления, `%COND%` - условие ребра.
    fn src(decl: &str, cond: &str) -> String {
        format!(
            r#"
model M {{
    {decl}
    start Wait {{ ref Done: {cond}; }}
    state Done;
}}
start Main = M;
"#
        )
    }

    /// Код диагностики из результата (или пустая строка).
    fn code(err: &str) -> &str {
        err.split('|').next().unwrap_or("")
    }

    // --- позитивные -------------------------------------------------------

    /// `after DWELL` даёт ровно то же, что `after 3m`: 180 000 000 000 нс.
    ///
    /// # Пример (Takt)
    /// ```but
    /// const DWELL := 3m;
    /// ref Done: after DWELL;
    /// ```
    #[test]
    fn named_dwell_equals_literal() {
        let named = ref_cond(&src("const DWELL := 3m;", "after DWELL")).unwrap();
        let literal = ref_cond(&src("", "after 3m")).unwrap();
        assert_eq!(named, ConditionNode::After(180_000_000_000));
        assert_eq!(
            named, literal,
            "именная выдержка обязана дать тот же узел, что литерал"
        );
    }

    /// Явный тип константы работает так же, как выведенный.
    #[test]
    fn explicit_duration_type_works() {
        let cond = ref_cond(&src("const DWELL: duration := 250ms;", "after DWELL")).unwrap();
        assert_eq!(cond, ConditionNode::After(250_000_000));
    }

    /// Цепочка констант разрешается до литерала.
    ///
    /// # Пример (Takt)
    /// ```but
    /// const BASE := 2s;
    /// const DWELL := BASE;
    /// ref Done: after DWELL;
    /// ```
    #[test]
    fn const_chain_resolves() {
        let cond = ref_cond(&src(
            "const BASE := 2s; const DWELL := BASE;",
            "after DWELL",
        ))
        .unwrap();
        assert_eq!(cond, ConditionNode::After(2_000_000_000));
    }

    /// Константа объявлена в **родительской** области - общее правило видимости языка
    /// действует и здесь.
    #[test]
    fn const_from_outer_scope_resolves() {
        let source = r#"
const DWELL := 30s;
model M {
    start Wait { ref Done: after DWELL; }
    state Done;
}
start Main = M;
"#;
        let (ast, _) = parse(source, 0).expect("разбор");
        let node = construct_model(&ast, None, &[]).expect("семантика").take();
        let model = node.search_model("M").expect("модель M");
        let model = model.borrow();
        let cond = model.states["Wait"].references()[0].cond.clone();
        assert_eq!(cond, ConditionNode::After(30_000_000_000));
    }

    /// Составное условие с именной выдержкой: `(after DWELL) & flag`.
    #[test]
    fn named_dwell_inside_compound_condition() {
        let cond = ref_cond(&src(
            "const DWELL := 1s; var flag: bit := 0;",
            "(after DWELL) & flag",
        ))
        .unwrap();
        assert!(
            matches!(cond, ConditionNode::And(_, _)),
            "ожидалось And, получено {cond:?}"
        );
    }

    // --- выражение в скобках (требование 2026-07-29) -------------

    /// Сумма константы и литерала: `after (BASE + 30s)`.
    ///
    /// # Пример (Takt)
    /// ```but
    /// const BASE := 2m;
    /// ref Done: after (BASE + 30s);
    /// ```
    #[test]
    fn sum_of_const_and_literal() {
        let cond = ref_cond(&src("const BASE := 2m;", "after (BASE + 30s)")).unwrap();
        assert_eq!(cond, ConditionNode::After(150_000_000_000));
    }

    /// Вложенные скобки и вычитание - форма примера (`after ((v + 1s) - f)`), с
    /// константами вместо переменных.
    #[test]
    fn nested_parens_with_subtraction() {
        let cond = ref_cond(&src(
            "const V := 10s; const F := 3s;",
            "after ((V + 1s) - F)",
        ))
        .unwrap();
        assert_eq!(cond, ConditionNode::After(8_000_000_000));
    }

    /// Выражение из одних литералов вычисляется так же, как их сумма.
    #[test]
    fn literals_only_expression() {
        let cond = ref_cond(&src("", "after (1m + 30s)")).unwrap();
        assert_eq!(cond, ConditionNode::After(90_000_000_000));
    }

    /// Скобки вокруг одиночного значения ничего не меняют.
    #[test]
    fn redundant_parens_are_transparent() {
        let cond = ref_cond(&src("const DWELL := 5s;", "after (DWELL)")).unwrap();
        assert_eq!(cond, ConditionNode::After(5_000_000_000));
    }

    // --- контрпримеры (SE-072) --------------------------------------------

    /// Имя не объявлено вовсе.
    ///
    /// # Контрпример (Takt)
    /// ```but
    /// ref Done: after DWELL;   // DWELL нигде не объявлена
    /// ```
    #[test]
    fn undeclared_name_is_se072() {
        let err = ref_cond(&src("", "after DWELL")).unwrap_err();
        assert_eq!(code(&err), "SE-072", "получено: {err}");
        assert!(err.contains("не объявлена"), "получено: {err}");
    }

    /// Имя - **переменная** типа `duration`: выдержка становится вычисляемой.
    ///
    /// # Пример (Takt)
    /// ```but
    /// var DWELL: duration := 3m;
    /// ref Done: after DWELL;      // значение берётся в такте
    /// ```
    #[test]
    fn variable_gives_dynamic_dwell() {
        let cond = ref_cond(&src("var DWELL: duration := 3m;", "after DWELL")).unwrap();
        assert!(
            matches!(cond, ConditionNode::AfterExpr(_)),
            "ожидалась вычисляемая выдержка, получено {cond:?}"
        );
    }

    /// Переменная **внутри выражения** - тоже вычисляемая выдержка.
    #[test]
    fn variable_inside_expression_gives_dynamic_dwell() {
        let cond = ref_cond(&src("var v: duration := 10s;", "after (v + 1s)")).unwrap();
        assert!(
            matches!(cond, ConditionNode::AfterExpr(_)),
            "ожидалась вычисляемая выдержка, получено {cond:?}"
        );
    }

    /// Входной порт типа `duration` годится так же, как переменная.
    #[test]
    fn duration_port_gives_dynamic_dwell() {
        let cond = ref_cond(&src("in DWELL: duration;", "after DWELL")).unwrap();
        assert!(
            matches!(cond, ConditionNode::AfterExpr(_)),
            "ожидалась вычисляемая выдержка, получено {cond:?}"
        );
    }

    /// Порт **не той** типизации отвергается: выдержке нужна длительность.
    ///
    /// # Контрпример (Takt)
    /// ```but
    /// in DWELL: bit;
    /// ref Done: after DWELL;   // бит - не длительность
    /// ```
    #[test]
    fn non_duration_port_is_se072() {
        let err = ref_cond(&src("in DWELL: bit;", "after DWELL")).unwrap_err();
        assert_eq!(code(&err), "SE-072", "получено: {err}");
        assert!(err.contains("duration"), "получено: {err}");
    }

    /// Переменная не той типизации - тоже отказ.
    #[test]
    fn non_duration_variable_is_se072() {
        let err = ref_cond(&src("var n: u32 := 5;", "after (n + 1s)")).unwrap_err();
        assert_eq!(code(&err), "SE-072", "получено: {err}");
        assert!(err.contains("duration"), "получено: {err}");
    }

    /// Константа есть, но тип не `duration`.
    ///
    /// # Контрпример (Takt)
    /// ```but
    /// const DWELL := 180;      // число тактов? компилятор не угадывает
    /// ref Done: after DWELL;
    /// ```
    #[test]
    fn wrong_type_const_is_se072() {
        let err = ref_cond(&src("const DWELL := 180;", "after DWELL")).unwrap_err();
        assert_eq!(code(&err), "SE-072", "получено: {err}");
        assert!(err.contains("duration"), "получено: {err}");
    }

    /// Голое число в выражении: длительность сочетается только с длительностью - то же
    /// правило, что `SE-065` в остальном языке.
    ///
    /// # Контрпример (Takt)
    /// ```but
    /// const V := 10s;
    /// ref Done: after (V + 1);   // 1 чего? - напишите 1s
    /// ```
    #[test]
    fn bare_number_in_expression_is_se072() {
        let err = ref_cond(&src("const V := 10s;", "after (V + 1)")).unwrap_err();
        assert_eq!(code(&err), "SE-072", "получено: {err}");
        assert!(err.contains("без единицы времени"), "получено: {err}");
    }

    /// Вызов функции в выражении выдержки недопустим (требование ).
    ///
    /// # Контрпример (Takt)
    /// ```but
    /// fn base() -> u8 { return 1; }
    /// ref Done: after (base() + 1s);
    /// ```
    #[test]
    fn function_call_in_expression_is_se072() {
        let err =
            ref_cond(&src("fn base() -> u8 { return 1; }", "after (base() + 1s)")).unwrap_err();
        assert_eq!(code(&err), "SE-072", "получено: {err}");
        assert!(err.contains("вызов функции"), "получено: {err}");
    }

    /// Сравнение в выражении выдержки - не длительность.
    #[test]
    fn comparison_in_expression_is_se072() {
        let err = ref_cond(&src("const V := 10s;", "after (V > 1s)")).unwrap_err();
        assert_eq!(code(&err), "SE-072", "получено: {err}");
        assert!(err.contains("сравнение"), "получено: {err}");
    }

    /// Отрицательный результат отвергается: выдержки "минус две секунды" нет.
    ///
    /// # Контрпример (Takt)
    /// ```but
    /// ref Done: after (1s - 3s);
    /// ```
    #[test]
    fn negative_result_is_se072() {
        let err = ref_cond(&src("", "after (1s - 3s)")).unwrap_err();
        assert_eq!(code(&err), "SE-072", "получено: {err}");
        assert!(err.contains("отрицательной"), "получено: {err}");
    }

    /// Значение константы - не длительность и не имя (арифметика с числом).
    #[test]
    fn arithmetic_value_is_se072() {
        let err = ref_cond(&src(
            "const BASE := 2s; const DWELL := BASE + 1;",
            "after DWELL",
        ))
        .unwrap_err();
        assert_eq!(code(&err), "SE-072", "получено: {err}");
    }

    /// Цикл в цепочке констант даёт диагностику, а не зависание инструмента.
    ///
    /// Тест класса, стоившего проекту падений без диагностики: обход обязан иметь
    /// предел, а не надеяться на разумность входа.
    ///
    /// # Контрпример (Takt)
    /// ```but
    /// const A := B;
    /// const B := A;    // цепочка не заканчивается литералом
    /// ref Done: after A;
    /// ```
    #[test]
    fn const_cycle_is_se072_not_hang() {
        let err = ref_cond(&src("const A := B; const B := A;", "after A")).unwrap_err();
        assert_eq!(code(&err), "SE-072", "получено: {err}");
        assert!(err.contains("ссылаются друг на друга"), "получено: {err}");
    }

    /// Одноимённое **состояние** константу не подменяет: поиск идёт только по
    /// объявлениям значений.
    #[test]
    fn state_with_same_name_does_not_satisfy_after() {
        let source = r#"
model M {
    start Wait { ref Done: after Done; }
    state Done;
}
start Main = M;
"#;
        let (ast, _) = parse(source, 0).expect("разбор");
        let err = construct_model(&ast, None, &[]).expect_err("ожидалась ошибка");
        assert_eq!(err.code.as_deref(), Some("SE-072"), "получено: {err:?}");
    }

    /// Выдержка вне ребра остаётся `SE-068` - константное выражение место не меняет.
    ///
    /// # Контрпример (Takt)
    /// ```but
    /// const DWELL := 3m;
    /// cond Timeout = after DWELL;   // у выдержки нет состояния-источника
    /// ```
    #[test]
    fn named_dwell_outside_reference_is_se068() {
        let source = r#"
model M {
    const DWELL := 3m;
    cond Timeout = after DWELL;
    start Wait { ref Done: true; }
    state Done;
}
start Main = M;
"#;
        let (ast, _) = parse(source, 0).expect("разбор");
        let err = construct_model(&ast, None, &[]).expect_err("ожидалась ошибка");
        assert_eq!(err.code.as_deref(), Some("SE-068"), "получено: {err:?}");
    }
}
