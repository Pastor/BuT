//! Печать операторов, блоков кода и проверок формул.
//!
//! Часть модуля `c_expr` (фича 0027: деление по логике).

use super::*;

/// Генерирует C-выражение из семантического узла выражения.
///
/// Функция пишет в `printer` без начального отступа и без завершающего `;\n`.
/// Отступ и разделители добавляет вызывающий код.
/// Является обёрткой над [`generate_expr`] с `min_prec = 0`.
pub(in crate::generator::c) fn generate_stmt_expression(
    printer: &mut Printer,
    map: &CMap,
    owner: &Element,
    params: Vec<(String, TypeNode)>,
    expr: &ExpressionNode,
    has_model: bool,
) -> Result<(), Diagnostic> {
    generate_expr(printer, map, owner, params, expr, 0, has_model)
}

pub(in crate::generator::c) fn generate_formula_check(
    printer: &mut Printer,
    map: &CMap,
    owner: &Element,
    formula: &Formula,
) -> Result<(), Diagnostic> {
    match formula {
        Formula::None => {}
        Formula::Formulas(formulas) => {
            for f in formulas {
                generate_formula_check(printer, map, owner, f)?;
            }
        }
        // Имя инварианта (0044) на эмиссию C не влияет — `assert()` тот же.
        Formula::Guard(cond, _, _) => {
            let cond_expr = generate_condition_expr(cond, map, owner)?;
            if !cond_expr.is_empty() {
                printer.ident(&format!("assert({});", cond_expr)).nl();
            }
        }
        Formula::LTL(_, _) => {
            // 0035: цель `c` LTL не верифицирует (эмиссия не меняется, R6). Это
            // не тихая потеря: предупреждение SE-055 выдаёт `takt_lang::ltl_warnings`
            // (`semantic/ltl_check.rs`) на каждую LTL-формулу любого уровня.
        }
    }
    Ok(())
}

/// Предупреждение `CC-024`: вызов встроенной функции выброшен (фича 0314).
///
/// Имя функции берётся из узла: сообщение без него заставило бы автора искать
/// выброшенный вызов самому. Позиция — у оператора (фича 0264).
fn builtin_dropped(expr: &ExpressionNode, loc: crate::diagnostics::Location) -> Diagnostic {
    let name = builtin_name(expr).unwrap_or("встроенная функция");
    Diagnostic::warning(
        loc,
        format!(
            "вызов '{name}' в порождённый C не попадает: печать из прошивки не \
             подразумевается, и кода у этой функции нет. Прежде цель выбрасывала \
             его молча, тогда как 'st' и 'rust' на том же входе отказывают"
        ),
    )
    .with_code("CC-024")
}

/// Имя встроенной функции, если выражение — её вызов.
fn builtin_name(expr: &ExpressionNode) -> Option<&'static str> {
    match expr {
        ExpressionNode::Function(def, _) => match &*def.borrow() {
            FunctionDefinitionNode::Builtin(name, _, _) => Some(name),
            _ => None,
        },
        ExpressionNode::Parenthesis(inner) => builtin_name(inner),
        _ => None,
    }
}

/// Является ли выражение вызовом **встроенной** функции языка.
///
/// Единственный класс операторов, который цель `c` вправе пропустить: `debug(…)`
/// и `S(…)` — средства отладки и запроса состояния, кода не порождающие. Всё
/// прочее, что печатник не умеет, обязано дойти до автора диагностикой (иначе
/// оператор исчезает при рапорте об успехе).
///
/// ⚠️ «Пропустить» больше не значит «молча»: с фичи 0314 цель возвращает
/// предупреждение `CC-024`.
fn is_builtin_call(expr: &ExpressionNode) -> bool {
    match expr {
        ExpressionNode::Function(def, _) => {
            matches!(&*def.borrow(), FunctionDefinitionNode::Builtin(..))
        }
        // Скобки прозрачны: `(debug("x"));` — тот же вызов.
        ExpressionNode::Parenthesis(inner) => is_builtin_call(inner),
        _ => false,
    }
}

/// Генерирует C-оператор из семантического узла.
///
/// Для `Block` рекурсивно генерирует все вложенные операторы.
/// Для `Expression` генерирует выражение с отступом и `;`.
/// Поддерживает `If`, `Loop`, `For`, `Variable`, `Return`, `Continue`, `Break`.
pub(in crate::generator::c) fn generate_code_block(
    printer: &mut Printer,
    map: &CMap,
    owner: &Element,
    params: Vec<(String, TypeNode)>,
    body: &StatementNode,
    has_model: bool,
) -> Result<(), Diagnostic> {
    match body {
        // Блок формул адресован ВНЕШНЕМУ анализатору (0484): цель его не
        // переводит и не проверяет — печатать нечего. ⚠️ Это не пропуск
        // неразрешённого узла (ниже): у законного блока свой узел, и молчание
        // здесь — его семантика, а не потеря оператора.
        StatementNode::Formula(_) => {}
        // Вставка печатается той целью, чьё имя названо (0484); без имени —
        // всеми. Язык вывода у `c` и `c-hal` один, поэтому метка у них общая.
        StatementNode::Assembly { target, body, .. } => {
            if crate::semantic::target_block::emits_for(target.as_deref(), "c") {
                generate_code_block(printer, map, owner, params, body, has_model)?;
            }
        }
        StatementNode::None => {}
        // ⚠️ Неразрешённый оператор — отказ, а не пропуск (фича 0236). Прежде
        // ветвь была пуста, и оператор исчезал из вывода при рапорте об успехе:
        // тот же класс, что фикс 0155 (печать `Unresolved` пустотой) и фича 0189
        // (`Err(_) => {}` глотал любую ошибку печати).
        StatementNode::Unresolved(raw) => {
            return Err(crate::generator::c::c_unresolved::refuse(
                raw.loc(),
                crate::generator::c::c_unresolved::UnresolvedNode::Statement,
            ));
        }

        StatementNode::Block(block) => {
            for stmt in block {
                // Комментарии автора обрамляют оператор ровно здесь — в обходе
                // тела (фича 0535, задача 04). Место одно на цель: печатай их
                // каждый вид оператора сам, ветви разошлись бы молча.
                let params = params.clone();
                crate::generator::comments::emit_around(
                    printer,
                    stmt.loc(),
                    crate::generator::header::CommentStyle::Slashes,
                    |p| generate_code_block(p, map, owner, params, stmt, has_model),
                )?;
            }
            // Неиспользуемая локальная гасится заглушкой (фича 0376): без неё
            // `cc -Wall -Wextra -Werror` отвечает «unused variable», то есть
            // вывод не собирается под флагами гейта этой же цели при нулевом
            // коде возврата `taktc`. Идиома та же, что у структурного
            // параметра (0260); место — конец блока, где переменная ещё в
            // области видимости.
            for name in crate::generator::local_stub::unused_locals(block) {
                printer
                    .ident(&format!("(void){};", normalize_lowercase_snakecase(name)))
                    .nl();
            }
        }

        StatementNode::Expression(expr, loc) => {
            // Присваивание АГРЕГАТА печатается поэлементно (фича 0340): в C
            // формы `x = {3, 4};` нет вовсе (`cc`: «expected expression»), а
            // массив не присваивается даже составным литералом. Место записи
            // выбирает общий носитель: у массива индекс, у структуры — имя
            // поля.
            crate::generator::site::enter(*loc);
            if super::aggregate::emit(printer, map, owner, params.clone(), expr, has_model)? {
                return Ok(());
            }
            // Объявляем место оператора: отказы печати выражений своей позиции
            // не имеют (решение 0056) и берут её отсюда (фича 0277).
            crate::generator::site::enter(*loc);
            // Генерируем во временный буфер, чтобы пропустить встроенные
            // функции отладки (`debug`, `S`) без порчи вывода.
            //
            // ⚠️ Пропускается **только** этот класс. Прежде здесь стояло
            // `Err(_) => {}` — глоталась любая ошибка печати, и оператор,
            // который цель не умеет, исчезал молча при рапорте об успехе:
            // проба фичи 0189 показала это на `x := 0x105:0;` (эталон считал
            // адрес числом, цель `c` не печатала ничего) и на самом анонимном
            // обращении. Класс тот же, что у фикса 0155: невыразимый узел
            // печатался пустотой вместо диагностики.
            let mut expr_buf = String::new();
            let result = {
                let mut tmp = Printer::new(4, &mut expr_buf);
                generate_stmt_expression(&mut tmp, map, owner, params, expr, has_model)
            };
            match result {
                Ok(()) if !expr_buf.is_empty() => {
                    printer.ident(&expr_buf).print(";").nl();
                }
                Ok(()) => {}
                Err(diagnostic) => {
                    if !is_builtin_call(expr) {
                        return Err(diagnostic);
                    }
                    // Вызов выброшен — и об этом ГОВОРИМ (фича 0314). Прежде
                    // цель молчала: `debug("…")` исчезал из вывода без строки,
                    // комментария и предупреждения, тогда как `st` и `rust` на
                    // том же входе отказывают (`ST-011`, `RS-011`) — три цели
                    // отвечали тремя разными способами.
                    map.warn(builtin_dropped(expr, *loc));
                }
            }
        }

        StatementNode::If {
            cond, then_, else_, ..
        } => {
            // Печатаем первый if
            printer.ident("if (");
            generate_stmt_expression(printer, map, owner, params.clone(), cond, has_model)?;
            printer.print(") {").up().nl();
            generate_code_block(printer, map, owner, params.clone(), then_, has_model)?;

            // Обходим цепочку else/else-if: если else-ветка — одиночный if,
            // схлопываем в `} else if (...)`, чтобы не создавать лишней вложенности
            let mut current_else = else_.as_deref();
            loop {
                match current_else {
                    None => {
                        // Нет else — закрываем последний блок
                        printer.down().ident("}").nl();
                        break;
                    }
                    Some(StatementNode::If {
                        cond: ec,
                        then_: et,
                        else_: ee,
                        ..
                    }) => {
                        // else-ветка — одиночный if: схлопываем в else if
                        printer.down().ident("} else if (");
                        generate_stmt_expression(
                            printer,
                            map,
                            owner,
                            params.clone(),
                            ec,
                            has_model,
                        )?;
                        printer.print(") {").up().nl();
                        generate_code_block(printer, map, owner, params.clone(), et, has_model)?;
                        current_else = ee.as_deref();
                    }
                    Some(else_stmt) => {
                        // else-ветка — произвольный блок
                        printer.down().ident("} else {").up().nl();
                        generate_code_block(
                            printer,
                            map,
                            owner,
                            params.clone(),
                            else_stmt,
                            has_model,
                        )?;
                        printer.down().ident("}").nl();
                        break;
                    }
                }
            }
        }

        StatementNode::Loop { cond, body, .. } => {
            match cond {
                None => {
                    // Бесконечный цикл
                    printer.ident("while (true) {").up().nl();
                }
                Some(cond_expr) => {
                    // Цикл с условием
                    printer.ident("while (");
                    generate_stmt_expression(
                        printer,
                        map,
                        owner,
                        params.clone(),
                        cond_expr,
                        has_model,
                    )?;
                    printer.print(") {").up().nl();
                }
            }
            generate_code_block(printer, map, owner, params.clone(), body, has_model)?;
            printer.down().ident("}").nl();
        }

        StatementNode::For {
            init,
            cond,
            step,
            body,
            ..
        } => {
            let has_var_init = matches!(
                init.as_ref().map(|b| b.as_ref()),
                Some(StatementNode::Variable(..))
            );

            if has_var_init {
                // Объявление переменной выносим перед `for` в обёртку `{}`
                printer.ident("{").nl();
                printer.up();
                if let Some(init_stmt) = init {
                    generate_code_block(printer, map, owner, params.clone(), init_stmt, has_model)?;
                }
                printer.ident("for (;");
                if let Some(cond_expr) = cond {
                    printer.print(" ");
                    generate_stmt_expression(
                        printer,
                        map,
                        owner,
                        params.clone(),
                        cond_expr,
                        has_model,
                    )?;
                }
                printer.print(";");
                if let Some(step_expr) = step {
                    printer.print(" ");
                    generate_stmt_expression(
                        printer,
                        map,
                        owner,
                        params.clone(),
                        step_expr,
                        has_model,
                    )?;
                }
                printer.print(") {").up().nl();
                generate_code_block(printer, map, owner, params.clone(), body, has_model)?;
                printer.down().ident("}").nl();
                printer.down();
                printer.ident("}").nl();
            } else {
                printer.ident("for (");
                if let Some(init_stmt) = init {
                    // Инициализация — только выражение (без отступа и точки с запятой)
                    if let StatementNode::Expression(expr, loc) = init_stmt.as_ref() {
                        crate::generator::site::enter(*loc);
                        generate_stmt_expression(
                            printer,
                            map,
                            owner,
                            params.clone(),
                            expr,
                            has_model,
                        )?;
                    }
                }
                printer.print(";");
                if let Some(cond_expr) = cond {
                    printer.print(" ");
                    generate_stmt_expression(
                        printer,
                        map,
                        owner,
                        params.clone(),
                        cond_expr,
                        has_model,
                    )?;
                }
                printer.print(";");
                if let Some(step_expr) = step {
                    printer.print(" ");
                    generate_stmt_expression(
                        printer,
                        map,
                        owner,
                        params.clone(),
                        step_expr,
                        has_model,
                    )?;
                }
                printer.print(") {").up().nl();
                generate_code_block(printer, map, owner, params.clone(), body, has_model)?;
                printer.down().ident("}").nl();
            }
        }

        StatementNode::Variable(name, ty, init, loc) => {
            // Объявление тела объявляет своё место (фича 0468): позиция у него
            // есть с 0386, а отказ печати типа или инициализатора приходил без
            // координаты.
            crate::generator::site::enter(*loc);
            let model = map.raw_model_at(owner.name())?;
            let model_ref = model.borrow();
            let snake_name = normalize_lowercase_snakecase(name.clone());
            // 0029-01: было `unwrap_or_else(|| format!("int {}"))` — локальная
            // переменная невыразимого типа молча объявлялась как `int`.
            let decl = typed_variable_or_diagnostic(
                ty,
                &snake_name,
                &*model_ref,
                map.float_width(),
                &format!("локальная переменная '{}'", name),
            )?;
            // ⚠️ Локальный МАССИВ с инициализатором-выражением объявляется и
            // копируется ПОЭЛЕМЕНТНО (фича 0466): в C массив не инициализируется
            // другим массивом (`uint8_t a[4] = model->data;` — «array initializer
            // must be an initializer list»). Класс жил под `--inline=auto`:
            // подстановка заводит копию параметра, и у параметра-массива вывод
            // не собирался при нулевом коде возврата `taktc`. Агрегатный литерал
            // ветвь не трогает — его C принимает списком.
            let copy_elementwise = matches!(ty, TypeNode::Array(_, _))
                && crate::semantic::bit_vector::is_bit_vector(ty).is_none()
                && matches!(
                    init.as_deref(),
                    Some(e) if !matches!(
                        e,
                        ExpressionNode::Initializer(_) | ExpressionNode::Array(_)
                    )
                );
            printer.ident(&decl);
            if let Some(init_expr) = init {
                if copy_elementwise {
                    printer.print(";").nl();
                    let count = match ty {
                        TypeNode::Array(len, _) => *len as usize,
                        _ => 0,
                    };
                    for place in crate::generator::aggregate::places(None, Some(ty), count) {
                        printer.ident(&format!("{snake_name}{}", place.suffix));
                        printer.print(" = ");
                        generate_stmt_expression(
                            printer,
                            map,
                            owner,
                            params.clone(),
                            init_expr,
                            has_model,
                        )?;
                        printer.print(&format!("{};", place.suffix)).nl();
                    }
                    return Ok(());
                }
                printer.print(" = ");
                generate_stmt_expression(printer, map, owner, params, init_expr, has_model)?;
            }
            printer.print(";").nl();
        }

        StatementNode::Return(ret, _) => {
            printer.ident("return");
            if let Some(expr) = ret {
                printer.print(" ");
                generate_stmt_expression(printer, map, owner, params, expr, has_model)?;
            }
            printer.print(";").nl();
        }

        StatementNode::Continue(_) => {
            printer.ident("continue;").nl();
        }

        StatementNode::Break(_) => {
            printer.ident("break;").nl();
        }

        StatementNode::InlineFormula(formulas) => {
            if map.guard_enable() {
                // Параметры функции объявляются на время печати условия
                // (фича 0473): иначе `: [Guard] v < 200;` в теле `fn bump(v)`
                // печаталось как `model->v` — обращение к полю, которого нет.
                crate::generator::c::c_expr::condition::enter_function_params(params.clone());
                let result = formulas
                    .iter()
                    .try_for_each(|formula| generate_formula_check(printer, map, owner, formula));
                crate::generator::c::c_expr::condition::leave_function_params();
                result?;
            }
        }

        StatementNode::Match { expr, arms, .. } => {
            printer.ident("switch (");
            generate_stmt_expression(printer, map, owner, params.clone(), expr, has_model)?;
            printer.print(") {").nl();
            for (index, MatchArmNode { patterns, body, .. }) in arms.iter().enumerate() {
                // Ветвь, чей образец уже встречался выше, НЕДОСТИЖИМА: `match`
                // берёт первое совпадение. В C две одинаковые метки — ошибка
                // компиляции («duplicate case value»), то есть невалидный вывод
                // при нулевом коде возврата `taktc` (фича 0514). Автор об этом
                // узнаёт из `SE-131`.
                if crate::semantic::match_arms::pattern_repeats_above(arms, index) {
                    continue;
                }
                let has_wildcard = patterns
                    .iter()
                    .any(|p| matches!(p, MatchPatternNode::Wildcard));
                if has_wildcard {
                    printer.ident("default:").nl();
                } else {
                    for pat in patterns {
                        if let MatchPatternNode::Value(val_expr) = pat {
                            printer.ident("case ");
                            generate_stmt_expression(
                                printer,
                                map,
                                owner,
                                params.clone(),
                                val_expr,
                                has_model,
                            )?;
                            printer.print(":").nl();
                        }
                    }
                }
                printer.ident("{").nl().up();
                generate_code_block(printer, map, owner, params.clone(), body, has_model)?;
                printer.ident("break;").nl();
                printer.down().ident("}").nl();
            }
            printer.ident("}").nl();
        }
    }
    Ok(())
}
