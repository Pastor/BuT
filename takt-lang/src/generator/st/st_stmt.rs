//! Печать операторов Takt ([`StatementNode`]) в Structured Text (IEC 61131-3).
//!
//! часть 2. Дополняет `st_expr.rs`. Функции
//! (`FUNCTION`/`RETURN`, `extern fn`) - часть 3.
//!
//! ## Подъём объявлений (главное отличие от C)
//!
//! В Takt переменная объявляется по месту: `enter { var boost: u8 := 5; ... }`
//! (`comprehensive.takt:58`). В IEC 61131-3 объявления живут **только в шапке POU**, а
//! не в теле. Поэтому [`print_statement`] **поднимает** объявление в [`Hoisted`], а на
//! его месте оставляет присваивание инициализатора.
//!
//! Разделение принципиально для семантики: поднимается **объявление**, а инициализатор
//! остаётся на исходном месте. Иначе `var i := 0` внутри цикла инициализировался бы
//! однажды, а не на каждом входе в блок.
//!
//! > MatIEC **принимает** и `VAR ... END_VAR` посреди тела (проверено пробой), но
//! > это его послабление, а не стандарт: `iec2c` тут не судья - он одинаково
//! > принимает обе формы. Выбор в пользу подъёма сделан по **стандарту**, потому
//! > что цель фичи - настоящий ПЛК, а не транспилятор.
//!
//! ## Циклы
//!
//! `loop`/`while` Takt -> `WHILE ... DO ... END_WHILE;`. `for` Takt - **си-образный**
//! (`init; cond; step`), а `FOR` в IEC - **счётный** (`FOR i := 0 TO 3 BY 1`), то есть
//! прямого соответствия нет: си-образный `for` разворачивается в `WHILE` с шагом в
//! конце тела.

use crate::diagnostics::{Diagnostic, Location};
use crate::generator::indent::Printer;
use crate::generator::st::st_expr::unsupported;
use crate::generator::st::st_expr::{
    assign_target_type, bit_string_of_type, coerce_to, inner_expr_type, print_expression,
    variable_ident,
};
use crate::parser::ast::Member;
use crate::semantic::type_node::TypeNode;
use crate::semantic::{ExpressionNode, MatchPatternNode, ModelNode, StatementNode};

/// Объявление, поднятое из тела в шапку POU.
#[derive(Debug)]
pub(crate) struct Hoisted {
    /// Имя переменной.
    pub name: String,
    /// Тип переменной.
    pub ty: TypeNode,
}

/// Побочные результаты печати тела: поднятые объявления и предупреждения.
#[derive(Default, Debug)]
pub(crate) struct StmtOutput {
    /// Объявления, которые вызывающий обязан напечатать в шапке POU.
    pub hoisted: Vec<Hoisted>,
    /// Предупреждения (`ST-010`), которые вызывающий обязан показать.
    pub warnings: Vec<Diagnostic>,
}

/// Печатает оператор Takt в текст ST.
///
/// Контекст функции, чьё тело печатается: имя POU и **тип возврата**.
///
/// Тип нужен ветви `Return`: возврат в ST - присваивание имени функции, то есть позиция
/// приёмника с известным типом. Без него разряд `x.N` печатался булевым выражением, и
/// `iec2c` отвечал "Incompatible data types for ':=' operation" при нулевом коде
/// возврата `taktc`.
pub(crate) type FnContext<'a> = Option<(&'a str, &'a TypeNode)>;

/// Объявления переменных **поднимаются** в `out.hoisted` (см. шапку модуля), а на их
/// месте печатается присваивание инициализатора.
///
/// # Ошибки
/// `ST-011` - узел не имеет представления в ST (R4: никакого тихого пропуска).
pub(crate) fn print_statement(
    stmt: &StatementNode,
    model: &ModelNode,
    p: &mut Printer,
    out: &mut StmtOutput,
    fn_name: FnContext<'_>,
) -> Result<(), Diagnostic> {
    match stmt {
        // Пустой оператор ничего не печатает: в ST лишняя `;` - синтаксическая ошибка
        // (проверено пробой: голая `;` в теле FB не разбирается).
        StatementNode::None => Ok(()),
        // Блок формул адресован внешнему анализатору: печатать нечего. Это не тихий
        // пропуск узла: молчание - семантика блока, а неразрешённый узел
        // по-прежнему даёт `ST-011`.
        StatementNode::Formula(_) => Ok(()),
        // Вставка печатается той целью, чьё имя названо; без имени - всеми. Язык вывода
        // у `st` и `st-at` один, поэтому метка у них общая.
        StatementNode::Assembly { target, body, .. } => {
            if crate::semantic::target_block::emits_for(target.as_deref(), "st") {
                print_statement(body, model, p, out, fn_name)?;
            }
            Ok(())
        }
        StatementNode::Block(items) => {
            for item in items {
                // Комментарий автора обрамляет оператор. Форма - блочная IEC: `(* ...
                // *)`, и закрывающая последовательность в тексте автора обезврежена
                // носителем.
                crate::generator::comments::emit_around(
                    p,
                    item.loc(),
                    crate::generator::header::CommentStyle::IecBlock,
                    |p| print_statement(item, model, p, out, fn_name),
                )?;
            }
            Ok(())
        }
        // Голый вызов функции оператором быть не может: "Function invocation in ST code
        // is not allowed outside an expression". Takt так вызывает
        // (`log_temp(temperature);`, `motor_up();`), поэтому результат уходит в
        // переменную-приёмник, которую вызывающий объявит в шапке POU.
        StatementNode::Expression(expr, loc) => {
            // Место оператора - для отказов печати выражений: своей позиции у них нет
            // (решение 0056), и до этой фичи цель печатала `ST-011` без координаты
            // вовсе.
            crate::generator::site::enter(*loc);
            if let ExpressionNode::Function(def, args) = expr.as_ref() {
                let call = crate::generator::st::st_func::print_call(def, args, model)?;
                let ret = crate::generator::st::st_func::return_type_of(&def.borrow());
                let sink = sink_name(&ret, model)?;
                out.hoisted.push(Hoisted {
                    name: sink.clone(),
                    ty: ret,
                });
                p.ident(&format!("{} := {};", sink, call)).nl();
                return Ok(());
            }
            // Запись одного разряда: `flags.3 := v`.
            if let ExpressionNode::Assign(lhs, rhs) = expr.as_ref()
                && let ExpressionNode::BitAccess(inner, Member::Number(bit)) = lhs.as_ref()
            {
                p.ident(&print_bit_write(inner, *bit, rhs, model)?).nl();
                return Ok(());
            }
            // Присваивание среза печатается поэлементно: формы "взять кусок массива" в
            // IEC 61131-3 нет, а эталон вход исполняет. Границы - литералы, проверенные
            // `SE-029`, поэтому длина известна и цикла не требуется.
            if let ExpressionNode::Assign(lhs, rhs) = expr.as_ref()
                && let ExpressionNode::ArraySlice(src, from, to) = rhs.as_ref()
            {
                // База - выражение: печатается тем же печатником.
                let dst = print_expression(lhs, model)?;
                let src_name = print_expression(src, model)?;
                // Срез над бит-вектором поэлементно не выразим - такой вход уходит
                // прежним путём, к отказу цели, как и у эталона. Пригодны оба операнда:
                // приёмник тоже обязан быть настоящим массивом (`res := mem[1:2];` при
                // `res: u8` эталон не исполняет - `SIM-006`). Тип базы даёт общий
                // носитель.
                let dst_ok = crate::generator::slice::elementwise_len_of(lhs, model).is_some();
                let src_len = if dst_ok {
                    crate::generator::slice::elementwise_len_of(src, model)
                } else {
                    None
                };
                let Some(src_len) = src_len else {
                    let text = print_expression(expr, model)?;
                    p.ident(&format!("{text};")).nl();
                    return Ok(());
                };
                let (start, len) = crate::generator::slice::bounds(*from, *to, src_len);
                for k in 0..len {
                    p.ident(&format!("{dst}[{k}] := {src_name}[{}];", start + k))
                        .nl();
                }
                return Ok(());
            }
            // Присваивание агрегата печатается поэлементно: агрегатной формы значения
            // массива в IEC 61131-3 нет, и прежде цель отвечала `ST-011` с текстом,
            // обещавшим "часть 2 ", тогда как эталон, `c` и `rust` вход исполняли.
            if let ExpressionNode::Assign(lhs, rhs) = expr.as_ref()
                && let ExpressionNode::Variable(var) = lhs.as_ref()
                && let ExpressionNode::Array(items) | ExpressionNode::Initializer(items) =
                    rhs.as_ref()
            {
                let name = variable_ident(&var.borrow());
                // Место записи выбирает общий носитель: у массива это индекс, у
                // структуры - имя поля.
                let target_ty = assign_target_type(&var.borrow());
                // Агрегат раскрывается до листьев общим носителем: элемент, который сам
                // является агрегатом, раскрывается дальше.
                let fields_of = |sname: &str| model.search_struct(sname).map(|d| d.fields);
                for leaf in
                    crate::generator::aggregate::leaves(target_ty.as_ref(), items, &fields_of)
                {
                    let value = match &leaf.ty {
                        Some(ty) => coerce_to(leaf.value, ty, model)?,
                        None => print_expression(leaf.value, model)?,
                    };
                    let suffix = crate::generator::st::st_multidim::iec_suffix(&leaf.path);
                    p.ident(&format!("{name}{suffix} := {value};")).nl();
                }
                return Ok(());
            }
            // Массив копируется поэлементно и тогда, когда справа не литерал: `seen :=
            // src;` печаталось как есть, и `iec2c` отвечал "Incompatible data types for
            // ':=' operation" - массив в IEC целиком не присваивается.
            //
            // Структура из объёма исключена: её IEC присваивает целиком (замер: `iec2c`
            // принимает), и разворот был бы лишней работой.
            if let ExpressionNode::Assign(lhs, rhs) = expr.as_ref()
                && let ExpressionNode::Variable(var) = lhs.as_ref()
                && let Some(ty @ TypeNode::Array(count, _)) = assign_target_type(&var.borrow())
                && crate::semantic::bit_vector::is_bit_vector(&ty).is_none()
                // Справа обязана стоять переменная (или константа): только её можно
                // индексировать. Результат вызова индексации в IEC не допускает - его
                // поднимает во временную свой проход, и разворот здесь ломал бы уже
                // починенное (замер: `iec2c` отвечал "';' missing" на `got[0] :=
                // pair(k)[0];`).
                && matches!(rhs.as_ref(), ExpressionNode::Variable(_))
            {
                let name = variable_ident(&var.borrow());
                let source = print_expression(rhs, model)?;
                for index in 0..count {
                    p.ident(&format!("{name}[{index}] := {source}[{index}];"))
                        .nl();
                }
                return Ok(());
            }
            // Присваивание печатается по целевому типу: литерал bool/enum
            // восстанавливается в `FALSE`/`TRUE` / имя константы. Покрывает и тела
            // `enter`/`exit`/`always` - они идут сюда же через `print_statement` (site
            // "enter" ).
            if let ExpressionNode::Assign(lhs, rhs) = expr.as_ref()
                && let ExpressionNode::Variable(var) = lhs.as_ref()
                && let Some(ty) = assign_target_type(&var.borrow())
            {
                let value = coerce_to(rhs, &ty, model)?;
                p.ident(&format!("{} := {};", variable_ident(&var.borrow()), value))
                    .nl();
                return Ok(());
            }
            let text = print_expression(expr, model)?;
            p.ident(&format!("{};", text)).nl();
            Ok(())
        }
        StatementNode::If {
            cond, then_, else_, ..
        } => {
            // Ветви печатаются В буфер: тело, состоящее только из формул, в ST не
            // транслируется (`ST-022`), и на выходе получался `IF ... THEN END_IF;` -
            // "no statement defined after THEN", то есть отказ `iec2c` при нулевом коде
            // возврата `taktc`.
            //
            // Пустого оператора у MatIEC нет: `;` он отвергает.
            // Поэтому пустая конструкция не печатается вовсе - условие языка побочных
            // эффектов не имеет (присваивание в Takt оператор, а не выражение), и
            // потерять нечего.
            let mut then_text = String::new();
            {
                let mut buffer = p.fork(&mut then_text);
                buffer.up();
                print_statement(then_, model, &mut buffer, out, fn_name)?;
                buffer.down();
            }
            let mut else_text = String::new();
            if let Some(else_) = else_ {
                let mut buffer = p.fork(&mut else_text);
                buffer.up();
                print_statement(else_, model, &mut buffer, out, fn_name)?;
                buffer.down();
            }
            if then_text.trim().is_empty() && else_text.trim().is_empty() {
                return Ok(());
            }
            p.ident(&format!("IF {} THEN", print_expression(cond, model)?))
                .nl();
            // Пустая ветвь `THEN` при непустом `ELSE` невозможна: в ST она обязана
            // нести оператор, и условие печатается отрицанием.
            if then_text.trim().is_empty() {
                p.up();
                p.down();
            } else {
                p.print(&then_text);
            }
            if !else_text.trim().is_empty() {
                p.ident("ELSE").nl();
                p.print(&else_text);
            }
            p.ident("END_IF;").nl();
            Ok(())
        }
        // Бесконечный цикл (`cond: None`) - `WHILE TRUE DO`: в ПЛК он завесит
        // скан-цикл, но это свойство модели, а не трансляции; молча менять семантику
        // нельзя.
        StatementNode::Loop { cond, body, .. } => {
            let guard = match cond {
                Some(c) => print_expression(c, model)?,
                None => "TRUE".to_string(),
            };
            p.ident(&format!("WHILE {} DO", guard)).nl();
            p.up();
            print_statement(body, model, p, out, fn_name)?;
            p.down();
            p.ident("END_WHILE;").nl();
            Ok(())
        }
        StatementNode::For {
            init,
            cond,
            step,
            body,
            ..
        } => print_for(init, cond, step, body, model, p, out, fn_name),
        // Объявление: тип уезжает в шапку POU, инициализатор остаётся здесь.
        StatementNode::Variable(name, ty, init, loc) => {
            // Объявление тела объявляет своё место: позиция у него есть с 0386, а отказ
            // печати типа или инициализатора приходил без координаты.
            crate::generator::site::enter(*loc);
            out.hoisted.push(Hoisted {
                name: name.clone(),
                ty: ty.clone(),
            });
            if let Some(init) = init {
                // Агрегат печатается поэлементно: агрегатной формы значения в IEC
                // 61131-3 нет, и `var p: Point := {0, 0};` в теле функции давал
                // `ST-011` - отказ на записи, которую эталон, `c` и `rust` исполняют.
                // Место записи выбирает общий носитель.
                if let ExpressionNode::Initializer(items) | ExpressionNode::Array(items) = &**init {
                    let fields_of = |sname: &str| model.search_struct(sname).map(|d| d.fields);
                    for leaf in crate::generator::aggregate::leaves(Some(ty), items, &fields_of) {
                        let value = match &leaf.ty {
                            Some(elem) => coerce_to(leaf.value, elem, model)?,
                            None => print_expression(leaf.value, model)?,
                        };
                        let suffix = crate::generator::st::st_multidim::iec_suffix(&leaf.path);
                        p.ident(&format!("{name}{suffix} := {value};")).nl();
                    }
                    return Ok(());
                }
                // Инициализатор локального объявления - тоже по целевому типу.
                let text = coerce_to(init, ty, model)?;
                p.ident(&format!("{} := {};", name, text)).nl();
            }
            Ok(())
        }
        // Возврат значения - присваивание имени функции; его подставляет печатник
        // функций, поэтому здесь допустим только голый `RETURN`.
        StatementNode::Return(None, _) => {
            p.ident("RETURN;").nl();
            Ok(())
        }
        // В ST нет `return <значение>`: результат возвращается присваиванием имени
        // функции, а `RETURN;` лишь досрочно выходит.
        StatementNode::Return(Some(value), _) => {
            let (name, ret) = fn_name.ok_or_else(|| {
                unsupported(
                    "return со значением вне функции: присваивать нечему — имя \
                     функции неизвестно",
                )
            })?;
            let text = crate::generator::st::st_expr::coerce_to(value, ret, model)?;
            p.ident(&format!("{} := {};", name, text)).nl();
            p.ident("RETURN;").nl();
            Ok(())
        }
        // В ST выход из цикла - `EXIT`, а не `break`.
        StatementNode::Break(_) => {
            p.ident("EXIT;").nl();
            Ok(())
        }
        StatementNode::Continue(_) => {
            p.ident("CONTINUE;").nl();
            Ok(())
        }
        // `match` -> цепочка `IF/ELSIF`, а не `CASE OF`. Причина: метки `CASE` в IEC -
        // литералы и диапазоны, а образцы Takt могут быть произвольными выражениями
        // (включая варианты перечислений, которые у нас стали именованными константами,
        // а не литералами). Цепочка сравнений семантически тождественна и заведомо
        // выразима.
        StatementNode::Match { expr, arms, .. } => print_match(expr, arms, model, p, out, fn_name),
        // LTL-формулы в ST не транслируются. Предупреждение, а не тихий пропуск:
        // молчание здесь - ровно класс дефекта (ср. где формулы теряются молча уже в
        // семантике).
        StatementNode::InlineFormula(formulas) => {
            // Предупреждение адресовано темпоральной формуле.
            if formulas.iter().any(has_temporal) {
                out.warnings.push(
                    Diagnostic::warning(
                        // Позиция самой формулы.
                        crate::semantic::formula::first_location(formulas)
                            .unwrap_or(Location::Codegen),
                        format!(
                            "LTL-формул ({}) в блоке кода: в Structured Text они не \
                             транслируются и в порождённый ПЛК-код не попадут",
                            formulas.len()
                        ),
                    )
                    .with_code("ST-010"),
                );
            }
            Ok(())
        }
        StatementNode::Unresolved(_) => Err(unsupported(
            "оператор не прошёл семантическое понижение (Unresolved)",
        )),
    }
}

/// Печатает си-образный `for` Takt как `WHILE` со счётчиком.
///
/// `FOR` в IEC - **счётный** (`FOR i := 0 TO 3 BY 1 DO`), а `for` Takt несёт
/// произвольные `cond` и `step`, поэтому прямого соответствия нет.
///
/// # Ошибки
/// `ST-011`, если тело содержит `continue`: в си-образном `for` шаг выполняется и
/// после `continue`, а в `WHILE` - нет. Развернуть такой цикл, не изменив
/// семантику, нельзя, поэтому отказ громкий, а не тихое расхождение.
#[allow(clippy::ref_option, clippy::too_many_arguments)]
fn print_for(
    init: &Option<Box<StatementNode>>,
    cond: &Option<Box<ExpressionNode>>,
    step: &Option<Box<ExpressionNode>>,
    body: &StatementNode,
    model: &ModelNode,
    p: &mut Printer,
    out: &mut StmtOutput,
    fn_name: FnContext<'_>,
) -> Result<(), Diagnostic> {
    if step.is_some() && contains_continue(body) {
        return Err(unsupported(
            "continue внутри for с шагом: в Takt шаг выполняется и после continue, \
             а в WHILE-развёртке ST — нет; тождественной развёртки не существует",
        ));
    }
    if let Some(init) = init {
        print_statement(init, model, p, out, fn_name)?;
    }
    let guard = match cond {
        Some(c) => print_expression(c, model)?,
        None => "TRUE".to_string(),
    };
    p.ident(&format!("WHILE {} DO", guard)).nl();
    p.up();
    print_statement(body, model, p, out, fn_name)?;
    if let Some(step) = step {
        let text = print_expression(step, model)?;
        p.ident(&format!("{};", text)).nl();
    }
    p.down();
    p.ident("END_WHILE;").nl();
    Ok(())
}

/// Печатает `match` как цепочку `IF/ELSIF/ELSE`.
fn print_match(
    expr: &ExpressionNode,
    arms: &[crate::semantic::MatchArmNode],
    model: &ModelNode,
    p: &mut Printer,
    out: &mut StmtOutput,
    fn_name: FnContext<'_>,
) -> Result<(), Diagnostic> {
    let subject = print_expression(expr, model)?;
    let mut printed_if = false;
    let mut wildcard: Option<&StatementNode> = None;

    // Тело ветви печатается В буфер: пустого оператора в IEC нет вовсе, и `IF c THEN
    // END_IF` арбитр отвергает ("no statement defined after 'THEN'"). Пустая ветвь
    // Опускается - тот же приём, что у пустого `IF` и у пустой ветви цели `rust`.
    let body_text = |body: &StatementNode,
                     p: &mut Printer,
                     out: &mut StmtOutput|
     -> Result<String, Diagnostic> {
        let mut text = String::new();
        {
            let mut buffer = p.fork(&mut text);
            buffer.up();
            print_statement(body, model, &mut buffer, out, fn_name)?;
            buffer.down();
        }
        Ok(text)
    };

    for (index, arm) in arms.iter().enumerate() {
        // Ветвь, чей образец повторяет более ранний, недостижима: `match` берёт первое
        // совпадение. Печатать её незачем, а её отсутствие снимает и оговорку о пустой
        // ветви ниже.
        if crate::semantic::match_arms::pattern_repeats_above(arms, index) {
            continue;
        }
        // Ветка `_` печатается последней как `ELSE`, где бы она ни стояла.
        if arm
            .patterns
            .iter()
            .any(|p| matches!(p, MatchPatternNode::Wildcard))
        {
            if wildcard.is_none() {
                wildcard = Some(&arm.body);
            }
            continue;
        }
        let mut tests = Vec::new();
        for pattern in &arm.patterns {
            let MatchPatternNode::Value(value) = pattern else {
                continue;
            };
            tests.push(format!("{} = {}", subject, print_expression(value, model)?));
        }
        if tests.is_empty() {
            continue;
        }
        let text = body_text(&arm.body, p, out)?;
        // Пустая ветвь не печатается: пустого оператора в IEC нет вовсе. Прежде
        // опущение проверял признак "образец повторяется ниже" - при дубле пустая ветвь
        // поглощала совпадение. С дубли до печати не доходят вовсе, и оговорка стала не
        // нужна.
        if text.trim().is_empty() {
            continue;
        }
        let guard = tests.join(" OR ");
        p.ident(&format!(
            "{} {} THEN",
            if printed_if { "ELSIF" } else { "IF" },
            guard
        ))
        .nl();
        p.print(&text);
        printed_if = true;
    }

    let wildcard_text = match wildcard {
        Some(body) => body_text(body, p, out)?,
        None => String::new(),
    };
    match (printed_if, wildcard_text.trim().is_empty()) {
        // Есть ветви и есть непустая `_` -> обычный ELSE.
        (true, false) => {
            p.ident("ELSE").nl();
            p.print(&wildcard_text);
            p.ident("END_IF;").nl();
        }
        (true, true) => {
            p.ident("END_IF;").nl();
        }
        // Только `_` -> тело исполняется безусловно; `IF` не нужен.
        (false, false) => {
            p.print(&wildcard_text);
        }
        (false, true) => {}
    }
    Ok(())
}

/// Есть ли `continue` в теле (не заходя во вложенные циклы - там он свой).
fn contains_continue(stmt: &StatementNode) -> bool {
    match stmt {
        StatementNode::Continue(_) => true,
        StatementNode::Block(items) => items.iter().any(contains_continue),
        StatementNode::If { then_, else_, .. } => {
            contains_continue(then_) || else_.as_ref().is_some_and(|e| contains_continue(e))
        }
        StatementNode::Match { arms, .. } => arms.iter().any(|a| contains_continue(&a.body)),
        // Вставка: `continue` внутри неё считается, если тело печатает эта цель.
        StatementNode::Assembly { target, body, .. } => {
            crate::semantic::target_block::emits_for(target.as_deref(), "st")
                && contains_continue(body)
        }
        // Вложенные циклы перехватывают свой `continue` - дальше не смотрим.
        StatementNode::Loop { .. } | StatementNode::For { .. } => false,
        StatementNode::Formula(_)
        | StatementNode::None
        | StatementNode::Unresolved(_)
        | StatementNode::Expression(_, _)
        | StatementNode::Variable(_, _, _, _)
        | StatementNode::Return(_, _)
        | StatementNode::Break(_)
        | StatementNode::InlineFormula(_) => false,
    }
}

/// Имя переменной-приёмника для результата вызова-оператора.
///
/// Приёмник свой на каждый тип: в ST у переменной один тип, а вызовы в теле могут
/// возвращать разное.
fn sink_name(ty: &TypeNode, model: &ModelNode) -> Result<String, Diagnostic> {
    let st = crate::generator::st::st_type::get_st_type(ty, model)?;
    Ok(format!("_st_discard_{}", st.to_lowercase()))
}

/// Строит диагностику `ST-011` - узел без представления в ST. Печатает запись одного
/// разряда `x.N := v`.
fn print_bit_write(
    inner: &ExpressionNode,
    bit: i128,
    rhs: &ExpressionNode,
    model: &ModelNode,
) -> Result<String, Diagnostic> {
    let base = print_expression(inner, model)?;
    let ty = inner_expr_type(inner).ok_or_else(|| {
        unsupported(&format!(
            "запись разряда {bit}: тип носителя не определяется статически, \
             а разрядность нужна, чтобы построить маску"
        ))
    })?;
    // У однобитного значения разряд ровно один, и он - само значение.
    if matches!(ty, TypeNode::Bit | TypeNode::Bool) {
        if bit != 0 {
            return Err(unsupported(&format!(
                "разряд {bit} у однобитного значения: в IEC 61131-3 у BOOL нет \
                 разрядов, кроме нулевого"
            )));
        }
        return Ok(format!(
            "{base} := {};",
            coerce_to(rhs, &TypeNode::Bool, model)?
        ));
    }
    let bs = bit_string_of_type(&ty).ok_or_else(|| {
        unsupported(&format!(
            "запись разряда в тип '{ty}': маска строится только для целых типов \
             IEC (8/16/32/64 бита)"
        ))
    })?;
    if bit < 0 || bit >= i128::from(bs.bits) {
        return Err(unsupported(&format!(
            "разряд {bit} вне разрядности типа '{ty}' ({} бит)",
            bs.bits
        )));
    }
    let width = bs.hex_digits;
    let set_mask = 1u128 << bit;
    let clear_mask = mask_all(bs.bits) & !set_mask;
    let set = format!("{}({base}) OR 16#{set_mask:0width$X}", bs.to_fn);
    let clear = format!("{}({base}) AND 16#{clear_mask:0width$X}", bs.to_fn);
    let body = match rhs {
        ExpressionNode::Number(n) if n & 1 == 1 => set,
        ExpressionNode::Number(_) => clear,
        // `SEL(G, IN0, IN1)`: при `G = FALSE` берётся `IN0`. Значит "сбросить" идёт
        // вторым аргументом, "установить" - третьим.
        other => format!("SEL({}, {clear}, {set})", low_bit_as_bool(other, model)?),
    };
    Ok(format!("{base} := {}({body});", bs.from_fn))
}

/// Маска "все разряды" для ширины `bits`.
fn mask_all(bits: u8) -> u128 {
    if bits >= 128 {
        u128::MAX
    } else {
        (1u128 << bits) - 1
    }
}

/// Печатает младший бит значения как `BOOL` - правило целей `c` и `sv`.
///
/// `MOD 2`, а не `AND 1`: в IEC `AND` определён только на битовых строках, и смешивать
/// миры чисел и строк в одном выражении нельзя.
fn low_bit_as_bool(rhs: &ExpressionNode, model: &ModelNode) -> Result<String, Diagnostic> {
    let printed = print_expression(rhs, model)?;
    match inner_expr_type(rhs) {
        Some(TypeNode::Bit | TypeNode::Bool) => Ok(printed),
        Some(TypeNode::Integer { .. }) => Ok(format!("({printed} MOD 2) <> 0")),
        _ => Err(unsupported(&format!(
            "значение '{printed}' в записи разряда: тип не определяется статически, \
             и привести его к BOOL нечем"
        ))),
    }
}

/// Есть ли в формуле темпоральная часть.
fn has_temporal(formula: &crate::semantic::formula::Formula) -> bool {
    use crate::semantic::formula::Formula;
    match formula {
        Formula::LTL(_, _) => true,
        Formula::Formulas(items) => items.iter().any(has_temporal),
        Formula::Guard(_, _, _) | Formula::None => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic::tree::construct_model;
    use crate::semantic::{NamedCodeBlockDefinitionNode, StateNode};

    /// Печатает тело блока `always` стартового состояния.
    fn always_of(body: &str) -> (String, StmtOutput) {
        let src = format!(
            "var n: u8 := 0;\nvar m: u8 := 0;\nvar b: bit := 0;\n\
             start S {{ always {{ {} }} }}",
            body
        );
        let (ast, _) = crate::parse(&src, 0).unwrap();
        let rc = construct_model(&ast, None, &[]).unwrap();
        let model = rc.borrow();
        let block = always_body(&model, "S");
        let mut text = String::new();
        let mut out = StmtOutput::default();
        {
            let mut p = Printer::new(4, &mut text);
            print_statement(&block, &model, &mut p, &mut out, None).expect("должно печататься");
        }
        (text, out)
    }

    /// Достаёт тело блока `always` состояния.
    fn always_body(model: &ModelNode, state: &str) -> StatementNode {
        let node = model.states.get(state).expect("нет состояния");
        let (StateNode::Simple { named_blocks, .. } | StateNode::Implement { named_blocks, .. }) =
            node
        else {
            panic!("состояние не разрешено");
        };
        named_blocks
            .iter()
            .find_map(|b| match b {
                NamedCodeBlockDefinitionNode::Always { body, .. } => Some(body.clone()),
                _ => None,
            })
            .expect("нет блока always")
    }

    /// Печатает тело `always` стартового состояния модели с перечислением `Command` и
    /// переменной `command`.
    fn enum_always(body: &str) -> String {
        let src = format!(
            "enum Command {{ Up, Down, Stop }}\nvar command: Command := Up;\n\
             start S {{ always {{ {} }} }}",
            body
        );
        let (ast, _) = crate::parse(&src, 0).unwrap();
        let rc = construct_model(&ast, None, &[]).unwrap();
        let model = rc.borrow();
        let block = always_body(&model, "S");
        let mut text = String::new();
        let mut out = StmtOutput::default();
        {
            let mut p = Printer::new(4, &mut text);
            print_statement(&block, &model, &mut p, &mut out, None).expect("должно печататься");
        }
        text
    }

    /// **0066: `bit`/`BOOL` литерал в присваивании -> `FALSE`/`TRUE`.**
    #[test]
    fn bool_literal_coerced_to_false_true() {
        let (st, _) = always_of("b := 0; b := 1;");
        assert!(st.contains("b := FALSE;"), "0 обязан стать FALSE:\n{st}");
        assert!(st.contains("b := TRUE;"), "1 обязан стать TRUE:\n{st}");
        assert!(
            !st.contains("b := 0;") && !st.contains("b := 1;"),
            "число осталось:\n{st}"
        );
    }

    /// **0066: значение перечисления в присваивании -> имя константы.**
    ///
    /// `command := Stop` приходит числом (`Number(2)`), но печатается `command :=
    /// Command_Stop` - совпадает с объявлением константы.
    #[test]
    fn enum_value_coerced_to_constant_name() {
        let st = enum_always("command := Stop;");
        assert!(
            st.contains("command := Command_Stop;"),
            "значение перечисления обязано стать именем константы:\n{st}"
        );
        assert!(
            !st.contains("command := 2;"),
            "число не должно остаться:\n{st}"
        );
    }

    /// **0066 (A4/T13, негативный тест): значение без варианта -> число.**
    ///
    /// Перечислимой переменной можно присвоить произвольное число; подмена его именем
    /// "похожего" варианта - тихая ложь (). Мутация "подобрать ближайший вариант"
    /// обязана валить этот тест.
    #[test]
    fn enum_value_without_variant_stays_number() {
        let src = "enum Command { Up, Down, Stop }\nvar command: Command := Up;\nstart S;";
        let (ast, _) = crate::parse(src, 0).unwrap();
        let rc = construct_model(&ast, None, &[]).unwrap();
        let model = rc.borrow();
        let ty = TypeNode::Enum("Command".to_string());
        // 7 не соответствует ни одному варианту (0/1/2) -> печатается числом.
        let orphan =
            crate::generator::st::st_expr::coerce_to(&ExpressionNode::Number(7), &ty, &model)
                .unwrap();
        assert_eq!(
            orphan, "7",
            "значение без варианта обязано печататься числом"
        );
        // 2 соответствует Stop -> имя константы.
        let named =
            crate::generator::st::st_expr::coerce_to(&ExpressionNode::Number(2), &ty, &model)
                .unwrap();
        assert_eq!(named, "Command_Stop");
    }

    /// `if` -> `IF ... THEN ... END_IF;` - с обязательным закрытием.
    #[test]
    fn test_if_is_closed_with_end_if() {
        let (st, _) = always_of("if n > 1 { n := 1; }");
        assert!(st.contains("IF n > 1 THEN"), "нет IF:\n{st}");
        assert!(st.contains("n := 1;"), "нет тела:\n{st}");
        assert!(st.contains("END_IF;"), "IF обязан закрываться:\n{st}");
    }

    /// `if/else` -> `IF ... ELSE ... END_IF;`.
    #[test]
    fn test_if_else_prints_else_branch() {
        let (st, _) = always_of("if n > 1 { n := 1; } else { n := 2; }");
        assert!(st.contains("ELSE"), "нет ветки ELSE:\n{st}");
        assert_eq!(st.matches("END_IF;").count(), 1, "лишние END_IF:\n{st}");
    }

    /// `while` -> `WHILE ... DO ... END_WHILE;`.
    #[test]
    fn test_while_becomes_while_do() {
        let (st, _) = always_of("while n < 3 { n := n + 1; }");
        assert!(st.contains("WHILE n < 3 DO"), "нет WHILE:\n{st}");
        assert!(st.contains("END_WHILE;"), "WHILE обязан закрываться:\n{st}");
    }

    /// Си-образный `for` разворачивается в `WHILE`: `FOR` в IEC - счётный, прямого
    /// соответствия произвольным `cond`/`step` у него нет.
    ///
    /// Шаг обязан печататься **в конце тела**, иначе цикл не сойдётся.
    #[test]
    fn test_c_style_for_unrolls_into_while_with_step_at_end() {
        let (st, out) = always_of("for var i: u8 := 0; i < 3; i := i + 1 { n := n + 1; }");
        assert!(st.contains("i := 0;"), "нет инициализации:\n{st}");
        assert!(st.contains("WHILE i < 3 DO"), "нет WHILE:\n{st}");
        let body = st.find("n := n + 1;").expect("нет тела");
        let step = st.find("i := i + 1;").expect("нет шага");
        assert!(step > body, "шаг обязан идти после тела:\n{st}");
        assert!(
            out.hoisted.iter().any(|h| h.name == "i"),
            "счётчик обязан подниматься в шапку POU"
        );
    }

    /// Объявление в теле поднимается, а инициализатор остаётся на месте.
    ///
    /// Вход из `comprehensive.takt:58`: `enter { var boost: u8 := 5; ... }`. В IEC
    /// объявления живут только в шапке POU.
    #[test]
    fn test_local_variable_declaration_is_hoisted_but_initializer_stays() {
        let (st, out) = always_of("var boost: u8 := 5; n := n + boost;");
        assert!(
            !st.contains("VAR"),
            "объявление не должно печататься в теле:\n{st}"
        );
        assert!(st.contains("boost := 5;"), "инициализатор остаётся:\n{st}");
        assert_eq!(out.hoisted.len(), 1, "объявление обязано подняться");
        assert_eq!(out.hoisted[0].name, "boost");
    }

    /// `match` -> цепочка `IF/ELSIF/ELSE`, а не `CASE`: метки `CASE` в IEC - литералы,
    /// а образцы Takt могут быть выражениями.
    #[test]
    fn test_match_becomes_if_elsif_chain() {
        let (st, _) = always_of("match n { 1 => { m := 1; } 2 => { m := 2; } _ => { m := 0; } }");
        assert!(st.contains("IF n = 1 THEN"), "нет первой ветви:\n{st}");
        assert!(st.contains("ELSIF n = 2 THEN"), "нет второй ветви:\n{st}");
        assert!(st.contains("ELSE"), "нет ветви _:\n{st}");
        assert!(st.contains("END_IF;"), "цепочка обязана закрываться:\n{st}");
    }

    /// `break` в ST - `EXIT`.
    #[test]
    fn test_break_is_exit_keyword() {
        let (st, _) = always_of("while n < 3 { break; }");
        assert!(st.contains("EXIT;"), "break обязан стать EXIT:\n{st}");
    }

    /// `continue` внутри `for` с шагом - отказ, а не тихое расхождение.
    ///
    /// В Takt шаг выполняется и после `continue`; в `WHILE`-развёртке - нет.
    /// Тождественной развёртки не существует, поэтому `ST-011`.
    #[test]
    fn test_continue_inside_for_with_step_is_rejected_not_silently_wrong() {
        let src = "var n: u8 := 0;\nstart S { always { \
                   for var i: u8 := 0; i < 3; i := i + 1 { continue; } } }";
        let (ast, _) = crate::parse(src, 0).unwrap();
        let rc = construct_model(&ast, None, &[]).unwrap();
        let model = rc.borrow();
        let block = always_body(&model, "S");
        let mut text = String::new();
        let mut out = StmtOutput::default();
        let mut p = Printer::new(4, &mut text);
        let err = print_statement(&block, &model, &mut p, &mut out, None)
            .expect_err("continue в for с шагом обязан отвергаться");
        assert_eq!(err.code.as_deref(), Some("ST-011"));
    }

    /// LTL-формула даёт предупреждение `ST-010`, а не тихий пропуск.
    #[test]
    fn test_inline_formula_warns_st010() {
        use crate::semantic::Formula;
        let rc = {
            let (ast, _) =
                crate::parse("var n: u8 := 0;\nstart S { always { n := n; } }", 0).unwrap();
            construct_model(&ast, None, &[]).unwrap()
        };
        let model = rc.borrow();
        // Формула темпоральная: с `ST-010` адресован только ей, а охранная получает
        // `ST-022` (обход мест 0203) и печатается целями `c`, `rust`, `sv`.
        let stmt = StatementNode::InlineFormula(vec![Formula::LTL(
            crate::verification::ltl::Ltl::True,
            crate::diagnostics::Location::Codegen,
        )]);
        let mut text = String::new();
        let mut out = StmtOutput::default();
        {
            let mut p = Printer::new(4, &mut text);
            print_statement(&stmt, &model, &mut p, &mut out, None).unwrap();
        }
        assert_eq!(out.warnings.len(), 1, "формула обязана дать предупреждение");
        assert_eq!(out.warnings[0].code.as_deref(), Some("ST-010"));
        assert!(text.is_empty(), "формула ничего не печатает в ST");
    }

    /// Пустая `InlineFormula` предупреждения не даёт: терять нечего.
    #[test]
    fn test_empty_inline_formula_is_silent() {
        let rc = {
            let (ast, _) =
                crate::parse("var n: u8 := 0;\nstart S { always { n := n; } }", 0).unwrap();
            construct_model(&ast, None, &[]).unwrap()
        };
        let model = rc.borrow();
        let mut text = String::new();
        let mut out = StmtOutput::default();
        let mut p = Printer::new(4, &mut text);
        print_statement(
            &StatementNode::InlineFormula(Vec::new()),
            &model,
            &mut p,
            &mut out,
            None,
        )
        .unwrap();
        assert!(out.warnings.is_empty(), "пустая формула не теряет ничего");
    }
}
