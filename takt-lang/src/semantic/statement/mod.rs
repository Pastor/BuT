//! Разрешение семантических операторов языка Takt.
//!
//! Основная функция [`resolve_statement`] преобразует "сырые" (`Unresolved`)
//! АСД-операторы в полностью разрешённые семантические варианты.
//!
//! ## Алгоритм
//!
//! Для каждого варианта `ast::Statement`:
//! 1. Рекурсивно разрешаются вложенные операторы.
//! 2. Выражения преобразуются через [`construct_expression`].
//! 3. Типы переменных разрешаются через [`construct_type`].
//!
//! ## Локальные переменные в блоках (С4)
//!
//! Объявления `var`/`const` внутри блоков (`if`, `loop`, `for`, `always` и др.)
//! разрешаются в [`StatementNode::Variable`] и временно регистрируются в
//! `model.variables` через [`register_local_var`]. Это позволяет последующим операторам
//! того же блока ссылаться на локальную переменную через обычный механизм
//! [`construct_expression`]. При выходе из блока [`unregister_local_vars`]
//! восстанавливает исходное состояние модели (с поддержкой затенения).
//!
//! ## Ошибка разрешения не зависит от глубины
//!
//! Оператор, который не удалось разрешить, даёт **ошибку** - на любом уровне
//! вложенности одинаково. Никаких `.unwrap_or_else(|_| Unresolved(...))` во вложенных
//! телах (`if`/`else`, `while`/`loop`, `for`: инициализатор и тело) и никаких
//! `filter_map(...ok())` у inline-`Guard`: пробрасывать `?`, и только.
//!
//! **Так было не всегда, и цена ошибки здесь - не "нет диагностики".** Прежде ошибка
//! вложенного тела глоталась, оператор оставался [`StatementNode::Unresolved`], а цель
//! `c` печатала его **пустотой** (`generator/c/c_expr/stmt.rs`), симулятор - пропускал
//! (`takt-sim/src/unit/statement.rs`). То есть `always { if x > 0 { x :=
//! неизвестное_имя; } }` компилировался в `if (model->x > 0) { }`: программа принята, а
//! написанный оператор **исчез**. Заодно не работал тест глубины `SE-062` - его `Err`
//! глотался тем же механизмом.
//!
//! Прежнее обоснование глотания ("позволяет обрабатывать `debug`, `S` без регистрации
//! встроенных символов") **мертво**: встроенные функции зарегистрированы в
//! [`crate::semantic::builtin`] и разрешаются штатно. Именно этот комментарий и
//! удерживал дефект - его читали вместо кода.
//!
//! Законные источники [`StatementNode::Unresolved`] остаются: ветка `_ =>` в
//! [`resolve_ast_statement`] (`Args`, `Error`, `StraySemicolon`) отдаёт его через `Ok`,
//! а не `Err`. `Assembly` и `Formula` из этого списка **ушли**: у них появились свои
//! узлы, потому что законный вход, приходящий целям неразрешённым, они отвергают как
//! дефект.

pub mod loop_context;

use crate::diagnostics::Diagnostic;
use crate::parser::ast;
use crate::semantic::condition::resolve_condition;
use crate::semantic::declaration;
use crate::semantic::expression::construct_expression;
use crate::semantic::type_node::TypeNode;
use crate::semantic::{
    ExpressionNode, Formula, MatchArmNode, MatchPatternNode, ModelNode, StatementNode, VariableNode,
};
use std::cell::RefCell;
use std::rc::Rc;

/// Разрешает семантический оператор [`StatementNode`].
///
/// Для `Unresolved` вызывает [`resolve_ast_statement`]. Для `Block` рекурсивно
/// разрешает каждый вложенный оператор. Остальные варианты возвращаются без изменений.
///
/// При ошибке разрешения выражения оператор сохраняется как `Unresolved`.
pub fn resolve_statement(
    statement: &StatementNode,
    params: Vec<(String, TypeNode)>,
    model: Rc<RefCell<ModelNode>>,
) -> Result<StatementNode, Diagnostic> {
    match statement {
        StatementNode::Unresolved(stmt) => Ok(resolve_ast_statement(stmt, params.clone(), model)?),
        StatementNode::None => Ok(StatementNode::None),
        StatementNode::Block(stmts) => {
            let mut resolved = Vec::with_capacity(stmts.len());
            let mut locals: Vec<(String, Option<VariableNode>)> = Vec::new();
            for s in stmts {
                let r = resolve_statement(s, params.clone(), model.clone())?;
                if let Some(entry) = register_local_var(&r, &model) {
                    locals.push(entry);
                }
                resolved.push(r);
            }
            unregister_local_vars(locals, &model);
            Ok(StatementNode::Block(resolved))
        }
        other => Ok(other.clone()),
    }
}

/// Преобразует `ast::Statement` в разрешённый [`StatementNode`].
///
/// При ошибке разрешения выражения возвращает `Err` (вызывающий код может обернуть в
/// `Unresolved`).
fn resolve_ast_statement(
    stmt: &ast::Statement,
    params: Vec<(String, TypeNode)>,
    model: Rc<RefCell<ModelNode>>,
) -> Result<StatementNode, Diagnostic> {
    // Тест глубины. Позиция не передаётся: `ast::Statement` её хранит не во всех
    // вариантах, а выдумывать координаты нельзя.
    let _depth = crate::semantic::validate::depth::enter(None)?;
    match stmt {
        // -- Блок операторов ----------------------------------------------------
        //
        // С4: после разрешения каждого оператора, если он объявляет локальную
        // переменную, та временно регистрируется в модели, чтобы последующие операторы
        // блока могли её использовать. При выходе из блока все локальные переменные
        // удаляются (затенённые - восстанавливаются).
        ast::Statement::Block { statements, .. } => {
            let mut resolved = Vec::with_capacity(statements.len());
            let mut locals: Vec<(String, Option<VariableNode>)> = Vec::new();
            for s in statements {
                let r = resolve_ast_statement(s, params.clone(), model.clone())?;
                if let Some(entry) = register_local_var(&r, &model) {
                    locals.push(entry);
                }
                resolved.push(r);
            }
            unregister_local_vars(locals, &model);
            Ok(StatementNode::Block(resolved))
        }

        // -- Оператор-выражение (присваивание, вызов функции и т.п.) -----------
        ast::Statement::Expression(loc, expr) => {
            let resolved = construct_expression(expr.clone(), params.clone(), model)?;
            // Позиция оператора - единственная координата употребления, которую
            // понижение может сохранить. У понижённого выражения своей позиции нет:
            // `ExpressionNode::loc()` выводит её из объявлений операндов, поэтому `f(n)
            // := 1;` указывал на строку, где объявлена `f`, а `5 := 2;` - никуда.
            // Позиции вхождений живут отдельным слоем `semantic::usages` и сюда не
            // доходят.
            Ok(StatementNode::Expression(Box::new(resolved), *loc))
        }

        // -- Условный оператор if -----------------------------------------------
        ast::Statement::If(loc, cond, then_, else_) => {
            let cond = construct_expression(cond.clone(), params.clone(), model.clone())?;
            let then_ = resolve_ast_statement(then_, params.clone(), model.clone())?;
            let else_ = else_
                .as_ref()
                .map(|e| resolve_ast_statement(e, params.clone(), model.clone()))
                .transpose()?
                .map(Box::new);
            Ok(StatementNode::If {
                cond: Box::new(cond),
                then_: Box::new(then_),
                else_,
                loc: *loc,
            })
        }

        // -- Цикл loop -------------------------------------------------------- `loop {
        // тело }` - бесконечный цикл (cond = None) `loop условие { тело }` -
        // продолжается, пока условие истинно Ключевое слово (`loop`/`while`) на
        // семантику не влияет - синонимы.
        ast::Statement::Loop(loc, cond, body, _) => {
            let cond = cond
                .as_ref()
                .map(|c| construct_expression(c.clone(), params.clone(), model.clone()))
                .transpose()?
                .map(Box::new);
            let body = {
                let _inside = loop_context::enter();
                resolve_ast_statement(body, params.clone(), model)?
            };
            Ok(StatementNode::Loop {
                cond,
                body: Box::new(body),
                loc: *loc,
            })
        }

        // -- Цикл for ----------------------------------------------------------
        //
        // С4: если инициализация объявляет переменную (`for var i: bit = 0; ...`), та
        // регистрируется в модели до разрешения условия, шага и тела цикла, чтобы они
        // могли на неё ссылаться. После разрешения всей конструкции переменная
        // удаляется из модели.
        ast::Statement::For(loc, init, cond, step, body) => {
            let init_resolved = init
                .as_ref()
                .map(|s| resolve_ast_statement(s, params.clone(), model.clone()))
                .transpose()?
                .map(Box::new);

            // Регистрируем переменную из init для cond/step/body
            let mut for_locals: Vec<(String, Option<VariableNode>)> = Vec::new();
            if let Some(init_stmt) = &init_resolved
                && let Some(entry) = register_local_var(init_stmt, &model)
            {
                for_locals.push(entry);
            }

            // Разрешение cond/step/body отделено от снятия регистрации: любая из трёх
            // частей может завершиться ошибкой (0155 - ошибка тела больше не
            // глотается), а переменная цикла обязана быть снята с регистрации в любом
            // исходе. Иначе неудачная сборка оставила бы `i` в `model.variables` -
            // фантомное имя, видимое последующим проверкам.
            let parts = (|| {
                let cond = cond
                    .as_ref()
                    .map(|e| construct_expression(*e.clone(), params.clone(), model.clone()))
                    .transpose()?
                    .map(Box::new);
                let step = step
                    .as_ref()
                    .map(|e| construct_expression(*e.clone(), params.clone(), model.clone()))
                    .transpose()?
                    .map(Box::new);
                let body = {
                    let _inside = loop_context::enter();
                    body.as_ref()
                        .map(|s| resolve_ast_statement(s, params.clone(), model.clone()))
                        .transpose()?
                        .map(Box::new)
                        .unwrap_or_else(|| Box::new(StatementNode::None))
                };
                Ok::<_, Diagnostic>((cond, step, body))
            })();

            unregister_local_vars(for_locals, &model);
            let (cond, step, body) = parts?;

            Ok(StatementNode::For {
                init: init_resolved,
                cond,
                step,
                body,
                // Позиция заголовка приходит из АСД: там она была всегда, а при
                // понижении терялась.
                loc: *loc,
            })
        }

        // -- Объявление локальной переменной ------------------------------------
        //
        // С4: инициализатор берётся из def.initializer (поле внутри VariableDefine),
        // поскольку после исправления грамматики LocalVariableDefine передаёт
        // инициализатор именно туда, а третье поле Statement::Variable всегда None.
        ast::Statement::Variable(loc, def, _extra_init) => {
            // Перечень форм объявления живёт в одном месте - `declaration.rs` (там же
            // строятся объявления уровня модели).
            let (name, ty, def_init) = declaration::local_declaration(def, *loc, model.clone())?;
            let init = def_init
                .map(|e| construct_expression(e, params.clone(), model.clone()))
                .transpose()?
                .map(Box::new);
            // Тип локального объявления выводится из инициализатора.
            //
            // Один такой вход давал три разных ответа: эталон - `SIM-007` в такте,
            // `c`/`c-hal`/`st`/ `st-at`/`rust` - честный отказ, а `sv`/`sv-mmio`
            // печатали невалидный модуль при нулевом коде возврата (`Can't find
            // definition of variable`), потому что объявление без типа они просто не
            // эмитили.
            //
            // Выводить надо здесь, а не общим проходом: локальное объявление живёт в
            // теле блока, а не в таблице модели, и к моменту `type_inference` тела ещё
            // не построены.
            let ty = match (&ty, &init) {
                (TypeNode::Inference, Some(expr)) => {
                    crate::semantic::type_inference::extract_type(expr, model.clone())?
                }
                _ => ty,
            };
            // Позиция объявления: без неё предупреждение о неиспользуемой локальной
            // печаталось бы с координатой `1:1` - ложью вместо отсутствия.
            Ok(StatementNode::Variable(name, ty, init, *loc))
        }

        // -- Оператор return ----------------------------------------------------
        ast::Statement::Return(loc, expr) => {
            let expr = expr
                .as_ref()
                .map(|e| construct_expression(e.clone(), params.clone(), model))
                .transpose()?
                .map(Box::new);
            Ok(StatementNode::Return(expr, *loc))
        }

        // -- Простые операторы без выражений -----------------------------------
        // Прерывание вне цикла - отказ `SE-132`: эталон и цель `c` расходились на нём
        // Молча, а поведения у такой записи язык не обещает. Признак ведёт
        // `loop_context`, страж которого живёт ровно на построении тела цикла.
        ast::Statement::Continue(loc) => {
            if !loop_context::inside() {
                return Err(loop_context::refuse("continue", *loc));
            }
            Ok(StatementNode::Continue(*loc))
        }
        ast::Statement::Break(loc) => {
            if !loop_context::inside() {
                return Err(loop_context::refuse("break", *loc));
            }
            Ok(StatementNode::Break(*loc))
        }

        // -- Встроенная формула -------------------------------------------------
        ast::Statement::InlineFormula(inline) => {
            match &**inline {
                ast::InlineFormulaDefine::Guard {
                    conditions, loc, ..
                } => {
                    // Параметры функции видны и условию формулы: разрешение условий
                    // спрашивает только модель, и `: [Guard] v < 200;` в теле `fn
                    // bump(v: u8)` давало `SE-025` "неразрешённое условие" - тот же
                    // класс, что 0346 у индексации. Приём - временная регистрация, тот
                    // же, каким видны локальные переменные блока.
                    let registered = register_params(&params, &model);
                    let resolved = conditions
                        .iter()
                        .map(|c| resolve_condition(c, model.clone()))
                        .collect::<Result<Vec<_>, _>>();
                    unregister_local_vars(registered, &model);
                    let resolved: Vec<Formula> = resolved?
                        .iter()
                        // Позиция объявления формулы: прежде охранная формула тела
                        // строилась без места.
                        .map(|c| crate::semantic::formula::condition_to_formula_at(c, *loc))
                        .collect();
                    Ok(StatementNode::InlineFormula(resolved))
                }
                ast::InlineFormulaDefine::Ltl { formulas, loc } => {
                    // 0035: LTL в блоке разбирается той же тотальной функцией, что на
                    // уровнях модели и состояния (`tree.rs`), - паритет уровней.
                    let resolved: Vec<Formula> = formulas
                        .iter()
                        // Позиция приходит из АСД: там она есть у объявления формулы.
                        .map(|f| {
                            Formula::LTL(crate::semantic::formula::ltl_ast_to_semantic(f), *loc)
                        })
                        .collect();
                    Ok(StatementNode::InlineFormula(resolved))
                }
            }
        }

        // -- Оператор match -----------------------------------------------------
        ast::Statement::Match(loc, expr, ast_arms) => {
            let resolved_expr = construct_expression(*expr.clone(), params.clone(), model.clone())?;
            let mut arms: Vec<MatchArmNode> = Vec::new();
            for arm in ast_arms {
                let mut patterns: Vec<MatchPatternNode> = Vec::new();
                for pat in &arm.patterns {
                    let pat_node = match pat {
                        ast::MatchPattern::Wildcard(_) => MatchPatternNode::Wildcard,
                        ast::MatchPattern::Value(e) => {
                            let pexpr =
                                construct_expression(e.clone(), params.clone(), model.clone())?;
                            MatchPatternNode::Value(Box::new(pexpr))
                        }
                    };
                    patterns.push(pat_node);
                }
                let body_node = resolve_statement(
                    &StatementNode::Unresolved(*arm.body.clone()),
                    params.clone(),
                    model.clone(),
                )?;
                arms.push(MatchArmNode {
                    patterns,
                    body: Box::new(body_node),
                    loc: arm.loc,
                });
            }
            Ok(StatementNode::Match {
                expr: Box::new(resolved_expr),
                arms,
                loc: *loc,
            })
        }

        // -- Блок формул внешнего анализатора --------------------------
        //
        // Обязательство адресовано внешнему инструменту: компилятор его не переводит и
        // не проверяет, цели и эталон пропускают. Собственный узел здесь обязателен:
        // оставь мы `Unresolved`, печатники целей отвергли бы законный вход воронкой
        // недостижимости, а разреши мы им пропускать `Unresolved` - вернулся бы
        // (оператор исчезает из вывода при рапорте об успехе).
        ast::Statement::Formula { block, .. } => Ok(StatementNode::Formula(block.clone())),

        // -- Вставка операторов для одной цели -------------------------
        //
        // Тело - обычные операторы Takt, и понижение у него обычное: имена в нём
        // разрешаются, ошибки диагностируются. Метка - Язык вывода, её проверяет
        // `target_block::check_target` (`SE-129`).
        ast::Statement::Assembly {
            loc,
            dialect,
            block,
            ..
        } => {
            let target = dialect
                .as_ref()
                .map(crate::semantic::target_block::check_target)
                .transpose()?;
            let body = resolve_ast_statement(block, params.clone(), model.clone())?;
            Ok(StatementNode::Assembly {
                target,
                body: Box::new(body),
                loc: *loc,
            })
        }

        // -- Прочие варианты: оставляем как Unresolved -------------------------
        //
        // Args, Error, StraySemicolon - служебные варианты, не требуют разрешения.
        _ => Ok(StatementNode::Unresolved(stmt.clone())),
    }
}

// -- Вспомогательные функции для управления областью видимости (С4) ------------

/// Временно регистрирует локальную переменную из разрешённого оператора в модели.
///
/// Если оператор - [`StatementNode::Variable`], вставляет [`VariableNode::Simple`] в
/// `model.variables` и возвращает имя вместе с предыдущим значением (если имя уже было
/// занято - для поддержки затенения). Для остальных операторов возвращает `None`.
///
/// Регистрация происходит после разрешения оператора, поэтому самоссылающийся
/// инициализатор (`var x = x`) корректно завершится ошибкой поиска переменной при
/// разрешении выражения.
fn register_local_var(
    stmt: &StatementNode,
    model: &Rc<RefCell<ModelNode>>,
) -> Option<(String, Option<VariableNode>)> {
    if let StatementNode::Variable(name, ty, _, _) = stmt {
        if name.is_empty() {
            return None;
        }
        // Сохраняем предыдущее значение для восстановления при выходе из блока
        let prev = model.borrow().variables.get(name).cloned();
        let node = VariableNode::Simple {
            upper: Some(Rc::downgrade(model)),
            loc: crate::diagnostics::Location::Implicit,
            name: name.clone(),
            ty: ty.clone(),
            // Expression::None - заглушка; инициализатор уже сохранён в
            // Statement::Variable и не нужен в узле переменной для разрешения выражений
            expr: ExpressionNode::None,
        };
        model.borrow_mut().variables.insert(name.clone(), node);
        Some((name.clone(), prev))
    } else {
        None
    }
}

/// Временно вносит параметры функции в область видимости модели.
///
/// Возвращает список для [`unregister_local_vars`] - восстановление обязано идти тем же
/// путём, что у локальных переменных блока.
///
/// Нужно только разрешению условий: выражения получают параметры отдельным аргументом
/// (`construct_expression`), а условия спрашивают лишь модель.
fn register_params(
    params: &[(String, TypeNode)],
    model: &Rc<RefCell<ModelNode>>,
) -> Vec<(String, Option<VariableNode>)> {
    params
        .iter()
        .filter(|(name, _)| !name.is_empty())
        .map(|(name, ty)| {
            let prev = model.borrow().variables.get(name).cloned();
            let node = VariableNode::Simple {
                upper: Some(Rc::downgrade(model)),
                loc: crate::diagnostics::Location::Implicit,
                name: name.clone(),
                ty: ty.clone(),
                expr: ExpressionNode::None,
            };
            model.borrow_mut().variables.insert(name.clone(), node);
            (name.clone(), prev)
        })
        .collect()
}

/// Восстанавливает состояние модели после выхода из блока.
///
/// Для каждой записи из `locals`:
/// - Если до регистрации была переменная с тем же именем - восстанавливает её.
/// - Если не было - удаляет имя из `model.variables`.
///
/// Порядок восстановления не важен, поскольку имена уникальны внутри одного блока.
fn unregister_local_vars(
    locals: Vec<(String, Option<VariableNode>)>,
    model: &Rc<RefCell<ModelNode>>,
) {
    for (name, prev) in locals {
        match prev {
            Some(node) => {
                model.borrow_mut().variables.insert(name, node);
            }
            None => {
                model.borrow_mut().variables.remove(&name);
            }
        }
    }
}

// -- Тесты ---------------------------------------------------------------------

#[cfg(test)]
mod tests;
