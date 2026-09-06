//! Операторы тела состояния и тела функции (задачи 0045-05, 0045-06).
//!
//! ## Только блокирующие присваивания — и это меняет модель, а не стиль
//!
//! Тело состояния Takt — **императивная последовательность**: `v := 1; w := v;`
//! даёт `w = 1` (так в симуляторе, так в цели `c`). Блокирующее присваивание
//! (`=`) в `always_comb` воспроизводит это буквально; неблокирующее (`<=`) дало
//! бы `w` = **старое** `v` — другую модель. Инлайн блока `enter` целевого
//! состояния прямо в переход требует видимости записи **внутри такта**, что в
//! неблокирующей семантике невозможно в принципе.
//!
//! Неблокирующие живут ровно в одном месте — `always_ff` (`sv_fsm`), где
//! защёлкивают вычисленное. Разделение и делает такт тактом.
//!
//! ## Куда пишется присваивание
//!
//! `v := 1;` печатается как `v_next = 1;`, а не `v = 1;`: в `always_comb`
//! **чтение** идёт из регистра, **запись** — в комбинационную пару, которую
//! `always_ff` защёлкнет по фронту. Отображение имени делает
//! [`Scope`](super::sv_expr::Scope) — здесь только выбор стороны.

use std::collections::{BTreeMap, BTreeSet};

use crate::diagnostics::Diagnostic;
use crate::generator::indent::Printer;
use crate::generator::sv::sv_expr::sv002;
use crate::generator::sv::sv_expr::{Scope, print_expression};
use crate::generator::sv::sv_names::signal_of_in;

use crate::semantic::type_node::TypeNode;
use crate::semantic::{ExpressionNode, MatchPatternNode, StatementNode};

/// Печатает оператор в теле `always_comb` либо `function automatic`.
///
/// # Ошибки
/// [`SV-002`](sv002) на непокрытом операторе; диагностики печатника выражений
/// всплывают наружу.
pub(crate) fn print_statement(
    p: &mut Printer,
    stmt: &StatementNode,
    scope: &Scope,
) -> Result<(), Diagnostic> {
    match stmt {
        // Пустой оператор — не ошибка: `enter { }` законен и означает «ничего».
        StatementNode::None => Ok(()),
        // Блок формул адресован внешнему анализатору (0484): печатать нечего.
        StatementNode::Formula(_) => Ok(()),
        // Вставка печатается той целью, чьё имя названо; без имени — всеми.
        // Язык вывода у `sv` и `sv-mmio` один, поэтому метка у них общая.
        StatementNode::Assembly { target, body, .. } => {
            if crate::semantic::target_block::emits_for(target.as_deref(), "sv") {
                print_statement(p, body, scope)?;
            }
            Ok(())
        }
        StatementNode::Block(stmts) => {
            for s in stmts {
                print_statement(p, s, scope)?;
            }
            Ok(())
        }
        StatementNode::Expression(expr, loc) => {
            // Место оператора — для отказов печати выражений (фича 0308):
            // своей позиции у них нет (решение 0056), и до этой фичи цель
            // печатала отказ без координаты вовсе.
            crate::generator::site::enter(*loc);
            print_expression_statement(p, expr, scope)
        }
        StatementNode::If {
            cond, then_, else_, ..
        } => {
            p.ident(&format!("if ({}) begin", print_expression(cond, scope)?))
                .nl();
            p.up();
            print_statement(p, then_, scope)?;
            p.down();
            if let Some(else_branch) = else_ {
                p.ident("end else begin").nl();
                p.up();
                print_statement(p, else_branch, scope)?;
                p.down();
            }
            p.ident("end").nl();
            Ok(())
        }
        // Возврат значения — присваивание ИМЕНИ ФУНКЦИИ, а не `return`.
        // Ключевое слово `return` в SystemVerilog есть, но **yosys его не
        // принимает** (проба: `ERROR: syntax error, unexpected TOK_ID`), тогда
        // как Verilator принимает молча. Форма выбрана по тому, что принимают
        // оба инструмента.
        StatementNode::Return(Some(expr), _) => {
            let name = scope
                .function
                .ok_or_else(|| sv002("возврат значения вне тела функции"))?;
            let value = match scope.function_ret {
                Some(ty) => scope.coerce(ty, expr)?,
                None => print_expression(expr, scope)?,
            };
            p.ident(&format!("{name} = {value};")).nl();
            Ok(())
        }
        // Пустой `return` возвращать нечего: у функции Takt всегда есть значение.
        // В `void`-функции он означал бы «выйти», то есть досрочный возврат, —
        // а его цель отвергает (см. `has_early_return`).
        StatementNode::Return(None, _) => Ok(()),
        // Локальная переменная: печатается ТОЛЬКО инициализатор — объявление уже
        // вынесено в начало тела (`hoist_locals`). В SystemVerilog объявления
        // обязаны предшествовать операторам, а Takt разрешает объявить переменную
        // посреди блока (`stacker.takt:117-120`), поэтому порядок исходника здесь
        // воспроизвести нельзя — только сохранить смысл.
        //
        // Регистровой пары у локальной переменной нет: она живёт внутри одного
        // вычисления, а не между тактами.
        StatementNode::Variable(name, ty, init, loc) => {
            // Объявление тела объявляет своё место (фича 0468): позиция у него
            // есть с 0386, а отказ печати типа или инициализатора приходил без
            // координаты.
            crate::generator::site::enter(*loc);
            if let Some(expr) = init {
                // Инициализатор — позиция приёмника с известным типом (фича
                // 0338): `var res: Mode := Idle;` приходит сюда числом (узла
                // варианта у `ExpressionNode` нет вовсе), и `res = 0;`
                // verilator отвергает **ошибкой** `ENUMVALUE` — при нулевом
                // коде возврата `taktc`.
                // Агрегат — поэлементно (фича 0345): `'{…}` в `always_comb`
                // пришлось бы согласовывать по ширине, а печатник выражений
                // отвечал `SV-002` «инициализатор структуры» — отказ на записи,
                // которую эталон, `c` и `rust` исполняют.
                if let ExpressionNode::Initializer(items) | ExpressionNode::Array(items) = &**expr {
                    // Агрегат раскрывается ДО ЛИСТЬЕВ общим носителем (фича
                    // 0366): прежде раскрытие было одноуровневым, и
                    // `pts := {{1, 2}, {3, 4}};` давало `SV-002` на записи,
                    // которую исполняют эталон, `rust` и `c`.
                    let fields_of = |sname: &str| scope.structs.get(sname).cloned();
                    for leaf in crate::generator::aggregate::leaves(Some(ty), items, &fields_of) {
                        let value = match &leaf.ty {
                            Some(elem) => scope.coerce(elem, leaf.value)?,
                            None => print_expression(leaf.value, scope)?,
                        };
                        let suffix = crate::generator::aggregate::c_like_suffix(&leaf.path);
                        p.ident(&format!("{name}{suffix} = {value};")).nl();
                    }
                    return Ok(());
                }
                let value = scope.coerce(ty, expr)?;
                p.ident(&format!("{name} = {value};")).nl();
            }
            Ok(())
        }
        // Ветки `_` нет намеренно: добавление оператора обязано валить сборку,
        // а не проваливаться молча (R4).
        //
        // ⚠️ Циклы отвергаются НЕ «пока не сделано»: в `always_comb` цикл обязан
        // разворачиваться в схему, то есть иметь границы, известные на этапе
        // синтеза. Условие Takt их не гарантирует, а неразворачиваемый цикл
        // синтезатор отвергает. Транслировать его молча значило бы обещать
        // схему, которой не существует. В корпусе циклов нет (проверено).
        StatementNode::Loop { .. } => Err(sv002(
            "цикл (loop/while): в синтезируемом RTL цикл обязан разворачиваться \
             в схему, то есть иметь границы, известные на этапе синтеза",
        )),
        // Цикл `for` РАЗВОРАЧИВАЕТСЯ, если его границы известны при
        // компиляции (фича 0321): в RTL цикла нет, есть схема. Прежде цель
        // отказывала на любом `for` — в том числе на статическом, который
        // исполняют остальные семь потребителей.
        StatementNode::For {
            init,
            cond,
            step,
            body,
            loc,
        } => {
            // Заголовок цикла объявляет СВОЁ место (фича 0471): позиция есть
            // у самого узла, и брать её у инициализатора больше не нужно —
            // заголовок без объявления (`for ; i < 3; i := i + 1`) прежде
            // координаты не получал вовсе.
            crate::generator::site::enter(*loc);
            let Some(unrolled) = crate::generator::sv::sv_unroll::unroll(
                init.as_deref(),
                cond.as_deref(),
                step.as_deref(),
            ) else {
                return Err(sv002(&format!(
                    "цикл for: в синтезируемом RTL цикл обязан разворачиваться в схему, \
                     то есть иметь границы, известные на этапе синтеза. Развернуть \
                     удаётся объявление с литеральным началом, сравнение переменной цикла \
                     с литералом и шаг '+='/'-=' на литерал, не более {} итераций",
                    crate::generator::sv::sv_unroll::MAX_ITERATIONS
                )));
            };
            // Имя печатается как есть — так же, как его печатает объявление
            // локальной переменной выше по этому же match.
            let ident = &unrolled.name;
            for value in &unrolled.values {
                // Переменная цикла объявлена как локальная (`automatic`,
                // фича 0304); перед каждой копией тела ей присваивается
                // очередное значение — так тело читает её тем же именем, что и
                // в модели.
                p.ident(&format!("{ident} = {value};")).nl();
                print_statement(p, body, scope)?;
            }
            Ok(())
        }
        StatementNode::Continue(_) => Err(sv002("оператор continue (циклов нет)")),
        StatementNode::Break(_) => Err(sv002("оператор break (циклов нет)")),
        // `match` переводится в `case` (фича 0322): в SystemVerilog это прямой
        // аналог, и отказ здесь был пробелом, а не решением — остальные семь
        // потребителей вход исполняли.
        //
        // ⚠️ Ветвь `default` печатается **всегда**: `case` без неё в
        // `always_comb` оставляет сигнал без значения на непокрытом входе, и
        // синтезатор выводит ЗАЩЁЛКУ — то же, чем обернулась необъявленная
        // переменная цикла в 0321. Молчаливая защёлка хуже отказа.
        StatementNode::Match { expr, arms, .. } => {
            p.ident(&format!("case ({})", print_expression(expr, scope)?))
                .nl();
            p.up();
            let mut has_default = false;
            for (index, arm) in arms.iter().enumerate() {
                // Ветвь, чей образец повторяет более ранний, НЕДОСТИЖИМА:
                // `match` берёт первое совпадение (фича 0514). Вторая ветвь по
                // умолчанию даёт у `verilator` ошибку «Multiple default
                // statements in case statement» — невалидный RTL при нулевом
                // коде возврата `taktc`.
                if crate::semantic::match_arms::pattern_repeats_above(arms, index) {
                    continue;
                }
                let wildcard = arm
                    .patterns
                    .iter()
                    .any(|pattern| matches!(pattern, MatchPatternNode::Wildcard));
                if wildcard {
                    has_default = true;
                    p.ident("default: begin").nl();
                } else {
                    let mut labels = Vec::new();
                    for pattern in &arm.patterns {
                        let MatchPatternNode::Value(value) = pattern else {
                            continue;
                        };
                        labels.push(print_expression(value, scope)?);
                    }
                    if labels.is_empty() {
                        return Err(sv002("ветка match без образцов"));
                    }
                    p.ident(&format!("{}: begin", labels.join(", "))).nl();
                }
                p.up();
                print_statement(p, &arm.body, scope)?;
                p.down();
                p.ident("end").nl();
            }
            if !has_default {
                // Пустая ветвь по умолчанию: сигналы уже получили значение
                // умолчанием `name_next = name` в начале `always_comb`, и
                // трогать их незачем — нужна только полнота `case`.
                p.ident("default: begin end").nl();
            }
            p.down();
            p.ident("endcase").nl();
            Ok(())
        }
        // Формула в теле: ОХРАННАЯ печатается `assert`, темпоральная — предмет
        // `taktc verify` (фича 0472).
        //
        // ⚠️ Прежде ветвь молчала на обеих, хотя охранную цель печатает на
        // уровне модели и состояния с фичи 0235: обязательство автора теряЛОСЬ
        // молча — тот же класс, что закрыла 0035 у семантики. Носитель печати
        // один с формулой-элементом (`sv_blocks::emit_guard`).
        //
        // ⚠️ Отказ печати условия не роняет компиляцию: формула — обязательство,
        // а не поведение, и вход, прежде переводившийся, обязан переводиться.
        StatementNode::InlineFormula(formulas) => {
            if scope.guard_enable {
                for formula in formulas {
                    let _ = crate::generator::sv::sv_blocks::emit_guard(p, formula, scope);
                }
            }
            Ok(())
        }
        StatementNode::Unresolved(_) => Err(sv002("неразрешённый оператор")),
    }
}

/// Собирает объявления локальных переменных тела — рекурсивно.
///
/// **Зачем вынос.** В SystemVerilog объявления обязаны предшествовать
/// операторам блока, а Takt разрешает объявить переменную посреди тела
/// (`stacker.takt:117-120`: `var ds := 0;` идёт после других операторов). Проба
/// 2026-07-16 подтвердила: `logic [7:0] dr;` после присваивания даёт
/// `%Error: syntax error, unexpected IDENTIFIER, expecting "'{"`.
///
/// Вынос смысла не меняет: объявление в Takt видимо до конца блока, а
/// инициализатор остаётся на своём месте отдельным присваиванием.
pub(crate) fn hoist_locals<'a>(stmt: &'a StatementNode, out: &mut Vec<(&'a str, &'a TypeNode)>) {
    match stmt {
        StatementNode::Variable(name, ty, _, _) => out.push((name, ty)),
        StatementNode::Block(stmts) => {
            for s in stmts {
                hoist_locals(s, out);
            }
        }
        StatementNode::If { then_, else_, .. } => {
            hoist_locals(then_, out);
            if let Some(e) = else_ {
                hoist_locals(e, out);
            }
        }
        // Цикл `for` разворачивается (фича 0321), поэтому его переменная и
        // объявления тела — обычные локальные: без спуска сюда развёрнутое
        // тело ссылалось бы на необъявленное имя, и `verilator` отвечал бы
        // «Can't find definition of variable», а `yosys` выводил защёлку.
        StatementNode::For { init, body, .. } => {
            if let Some(init) = init {
                hoist_locals(init, out);
            }
            hoist_locals(body, out);
        }
        // Тела веток `match` — обычные блоки (фича 0322): их объявления
        // обязаны быть подняты, иначе развёрнутое тело сошлётся на
        // необъявленное имя (тот же класс, что у цикла выше).
        StatementNode::Match { arms, .. } => {
            for arm in arms {
                hoist_locals(&arm.body, out);
            }
        }
        // Прочие узлы объявлений не несут: `loop` цель отвергает (`SV-002`), а
        // выражения переменных не заводят.
        _ => {}
    }
}

/// Проверяет, есть ли в теле функции **досрочный** возврат.
///
/// Возврат печатается присваиванием имени функции (`f = t;`), и исполнение
/// после него **продолжается** — в отличие от `return`, который выходит.
/// Поэтому возврат допустим только последним оператором тела: возврат из ветви
/// `if` был бы затёрт тем, что идёт следом, и результат разошёлся бы с моделью
/// **молча**.
///
/// Ключевое слово `return` эту проблему решило бы, но **yosys его не
/// принимает** (проба 2026-07-16), а Verilator принимает — то есть один линтер
/// пропустил бы несинтезируемый модуль. Отказ честнее.
pub(crate) fn has_early_return(stmt: &StatementNode) -> bool {
    /// Есть ли возврат где-то внутри поддерева.
    fn contains_return(s: &StatementNode) -> bool {
        match s {
            StatementNode::Return(_, _) => true,
            StatementNode::Block(stmts) => stmts.iter().any(contains_return),
            StatementNode::If { then_, else_, .. } => {
                contains_return(then_) || else_.as_deref().is_some_and(contains_return)
            }
            StatementNode::Loop { body, .. } | StatementNode::For { body, .. } => {
                contains_return(body)
            }
            _ => false,
        }
    }
    match stmt {
        // Возврат допустим ровно в хвосте верхнего блока.
        StatementNode::Block(stmts) => match stmts.split_last() {
            Some((_, head)) => head.iter().any(contains_return),
            None => false,
        },
        StatementNode::Return(_, _) => false,
        other => contains_return(other),
    }
}

/// Печатает объявления вынесенных локальных переменных.
pub(crate) fn emit_hoisted_locals(
    p: &mut Printer,
    locals: &[(&str, &TypeNode)],
    unread: &[String],
) -> Result<(), Diagnostic> {
    for (name, ty) in locals {
        let decl = super::sv_type::sv_type(ty, &format!("переменная '{}'", name))?;
        p.ident(&format!("{};", decl.declare(name))).nl();
        emit_sink_declaration(p, name, unread);
    }
    Ok(())
}

/// Объявление ПОГЛОТИТЕЛЯ для локальной, значение которой нигде не читается
/// (фича 0387).
///
/// ⚠️ `verilator -Wall` отвечает `UNUSEDSIGNAL`, а гейт цели считает
/// предупреждение ошибкой — при нулевом коде возврата `taktc` и при том, что
/// эталон и цели `st` вход исполняют. Идиома та же, что у неиспользуемого
/// параметра (0337) и у поднятых локальных со структурой (0375): редукция с
/// константой, которую синтезатор выбрасывает сам.
fn emit_sink_declaration(p: &mut Printer, name: &str, unread: &[String]) {
    if unread.iter().any(|n| n == name) {
        p.ident(&format!("logic _unused_{name};")).nl();
    }
}

/// Имена переменных ЦИКЛОВ в теле (фича 0425).
///
/// ⚠️ Переменная цикла читается **частично**: индекс сужается до ширины
/// массива (0365), и `verilator` отвечает `UNUSEDSIGNAL` на старшие разряды
/// («Bits of signal are not used: 'i'[7:2]»), а гейт цели считает
/// предупреждение ошибкой — при нулевом коде возврата `taktc`.
///
/// ⚠️ Тип переменной менять нельзя: его задал автор, и он определяет
/// семантику переполнения (правило 0127). Поэтому гасятся разряды, а не
/// сужается объявление.
pub(crate) fn loop_variables(stmt: &StatementNode, out: &mut Vec<String>) {
    match stmt {
        StatementNode::For { init, body, .. } => {
            if let Some(init) = init
                && let StatementNode::Variable(name, ..) = &**init
                && !out.contains(name)
            {
                out.push(name.clone());
            }
            loop_variables(body, out);
        }
        StatementNode::Block(items) => {
            for item in items {
                loop_variables(item, out);
            }
        }
        StatementNode::If { then_, else_, .. } => {
            loop_variables(then_, out);
            if let Some(alt) = else_ {
                loop_variables(alt, out);
            }
        }
        StatementNode::Loop { body, .. } => loop_variables(body, out),
        StatementNode::Match { arms, .. } => {
            for arm in arms.iter() {
                loop_variables(&arm.body, out);
            }
        }
        // Обход намеренно НЕ исчерпывающий: пропущенная форма даёт прежний
        // отказ инструмента, а не порчу вывода (приём 0246).
        _ => {}
    }
}

/// Присваивание поглотителя — печатается после объявлений (фича 0387).
/// Имена, которые тело читает ТОЛЬКО как индекс массива (фича 0466).
///
/// Индекс печатается сужением по размеру массива (`a[2'(i)]`, фича 0365), и
/// старшие разряды имени не читает никто: `verilator` отвечает
/// `UNUSEDSIGNAL` — «Bits of … are not used: 'i'[7:2]». Гасит их тот же
/// поглотитель, что и вовсе непрочитанную локальную (0387) и переменную цикла
/// (0425), поэтому признак и отдаёт имена **в тот же список**.
///
/// ⚠️ Признак по ДЕРЕВУ, а не по напечатанному тексту (в отличие от заглушки
/// 0337): объявление поглотителя обязано встать до операторов, то есть до
/// того, как тело напечатано.
///
/// ⚠️ Индекс-ВЫРАЖЕНИЕ (`a[i + 1]`) под правило не подпадает: там читается всё
/// имя целиком. Граница названа — лишний поглотитель гасил бы честное
/// предупреждение.
pub(crate) fn index_only_variables(body: &StatementNode, names: &[String], out: &mut Vec<String>) {
    if names.is_empty() {
        return;
    }
    // Обход идёт по КОПИИ: изменяемый обход — единственный, который спускается
    // внутрь выражения (носитель `semantic::walk`), а второй такой заводить
    // нельзя (класс 0084/0193/0195).
    let mut copy = body.clone();
    let mut total: BTreeMap<String, usize> = BTreeMap::new();
    let mut under: BTreeMap<String, usize> = BTreeMap::new();
    // ⚠️ `walk_stmt_exprs_mut` САМ спускается в подвыражения — второй
    // `walk_expr_mut` внутри дал бы двойной счёт (замер: `i` считался дважды,
    // и признак молчал).
    crate::semantic::walk::walk_stmt_exprs_mut(&mut copy, &mut |node| match node {
        ExpressionNode::Variable(cell) => {
            let name = cell.borrow().name().to_string();
            if names.contains(&name) {
                *total.entry(name).or_default() += 1;
            }
        }
        ExpressionNode::ArraySubscript(_, index) => {
            if let ExpressionNode::Variable(cell) = &**index {
                let name = cell.borrow().name().to_string();
                if names.contains(&name) {
                    *under.entry(name).or_default() += 1;
                }
            }
        }
        _ => {}
    });
    for name in names {
        let seen = total.get(name).copied().unwrap_or(0);
        if seen > 0 && under.get(name).copied().unwrap_or(0) == seen && !out.contains(name) {
            out.push(name.clone());
        }
    }
}

/// Поля СТРУКТУРНОГО имени, которые тело читает, — если оно читает его
/// только по полям (фича 0506).
///
/// `None` — имя упомянуто как ЦЕЛОЕ (передано дальше, возвращено, присвоено):
/// тогда читаются все разряды, и поглощать нечего.
///
/// ⚠️ Признак по ДЕРЕВУ, как у `index_only_variables` (0466) и по той же
/// причине: объявление поглотителя обязано встать до операторов, то есть до
/// того, как тело напечатано.
pub(crate) fn field_only_reads(body: &StatementNode, name: &str) -> Option<BTreeSet<String>> {
    // Обход идёт по КОПИИ: изменяемый обход — единственный, который спускается
    // внутрь выражения (носитель `semantic::walk`).
    let mut copy = body.clone();
    let mut total = 0usize;
    let mut under = 0usize;
    let mut fields: BTreeSet<String> = BTreeSet::new();
    crate::semantic::walk::walk_stmt_exprs_mut(&mut copy, &mut |node| match node {
        ExpressionNode::Variable(cell) => {
            if cell.borrow().name() == name {
                total += 1;
            }
        }
        ExpressionNode::BitAccess(base, member) => {
            let ExpressionNode::Variable(cell) = &**base else {
                return;
            };
            if cell.borrow().name() != name {
                return;
            }
            if let crate::parser::ast::Member::Identifier(field) = member {
                under += 1;
                fields.insert(field.name.clone());
            }
        }
        _ => {}
    });
    (total > 0 && total == under).then_some(fields)
}

pub(crate) fn emit_local_sinks(p: &mut Printer, locals: &[(&str, &TypeNode)], unread: &[String]) {
    for (name, _) in locals {
        if unread.iter().any(|n| n == name) {
            p.ident(&format!("_unused_{name} = &{{1'b0, {name}}};"))
                .nl();
        }
    }
}

/// То же для тела блока состояния или модели — с `automatic` (фича 0304).
///
/// # Почему `automatic`, и почему это вообще понадобилось
///
/// Локальные переменные тел состояний **не объявлялись вовсе**: `hoist_locals`
/// звался только для тел функций. Цель печатала `g = (F + 1);` при нулевом коде
/// возврата `taktc`, а `verilator` отвечал «Can't find definition of variable:
/// 'g'». ⚠️ Дефект не зависел от вывода типов: он воспроизводится и на явном
/// `var g: u8 := …` (контрольная проба).
///
/// Тела состояний печатаются внутри ветви `unique case` в `always_comb`, а там
/// объявление обязано быть `automatic` — иначе это статическая переменная в
/// процедурном блоке. Форма проверена **обоими** инструментами (урок 0235):
/// `verilator --lint-only -Wall` и `yosys -p synth` принимают её.
pub(crate) fn emit_hoisted_locals_auto(
    p: &mut Printer,
    locals: &[(&str, &TypeNode)],
    unread: &[String],
) -> Result<(), Diagnostic> {
    for (name, ty) in locals {
        let decl = super::sv_type::sv_type(ty, &format!("переменная '{}'", name))?;
        p.ident(&format!("automatic {};", decl.declare(name))).nl();
        emit_sink_declaration(p, name, unread);
    }
    Ok(())
}

/// Печатает оператор-выражение: присваивание либо вызов.
fn print_expression_statement(
    p: &mut Printer,
    expr: &ExpressionNode,
    scope: &Scope,
) -> Result<(), Diagnostic> {
    match expr {
        // Присваивание СРЕЗА печатается тем же поэлементным путём (фича 0355):
        // массив здесь РАСПАКОВАННЫЙ, и `{…}` для него есть склейка разрядов, а
        // не список элементов (урок 0309). Границы — литералы, проверенные
        // `SE-029`.
        ExpressionNode::Assign(target, value)
            if matches!(value.as_ref(), ExpressionNode::ArraySlice(..)) =>
        {
            let ExpressionNode::ArraySlice(src, from, to) = value.as_ref() else {
                unreachable!("охрана ветви проверила вид узла");
            };
            // База — выражение (фича 0358): печатается тем же печатником.
            let lhs = print_assign_target(target, scope)?;
            let rhs_base = print_expression(src, scope)?;
            // Пригодны ОБА операнда: приёмник тоже обязан быть настоящим
            // массивом (`res := mem[1:2];` при `res: u8` эталон не исполняет —
            // `SIM-006`). У цели `sv` тип берётся её же функцией: модели здесь
            // нет, карта хранит снимок.
            let dst_ok = target_type(target)
                .as_ref()
                .and_then(crate::generator::slice::elementwise_len)
                .is_some();
            let src_len = if dst_ok {
                target_type(src)
                    .as_ref()
                    .and_then(crate::generator::slice::elementwise_len)
            } else {
                None
            };
            // Непригодный операнд отдаётся ПРЕЖНЕМУ пути: отказ `SV-002` строит
            // общий печатник выражений, и координату оператора ему даёт
            // `site::at` (фича 0308). Свой отказ пришёл бы без координаты.
            let Some(src_len) = src_len else {
                let rhs = print_expression(value, scope)?;
                p.ident(&format!("{lhs} = {rhs};")).nl();
                return Ok(());
            };
            let (start, len) = crate::generator::slice::bounds(*from, *to, src_len);
            for k in 0..len {
                p.ident(&format!("{lhs}[{k}] = {rhs_base}[{}];", start + k))
                    .nl();
            }
            Ok(())
        }
        // Присваивание АГРЕГАТА (фичи 0330, 0340) печатается поэлементно:
        // `'{…}` в `always_comb` пришлось бы согласовывать по ширине с каждым
        // элементом, а поэлементная форма выразима всегда.
        //
        // ⚠️ Прежняя редакция этого комментария добавляла «…и совпадает с тем,
        // что печатает цель `c`» — замер 0340 это опроверг: цель `c` печатала
        // агрегат как есть, и её вывод не собирался вовсе (класс 0292).
        ExpressionNode::Assign(target, value)
            if matches!(
                value.as_ref(),
                ExpressionNode::Array(_) | ExpressionNode::Initializer(_)
            ) =>
        {
            let (ExpressionNode::Array(items) | ExpressionNode::Initializer(items)) =
                value.as_ref()
            else {
                unreachable!("охрана ветви проверила вид узла");
            };
            let lhs = print_assign_target(target, scope)?;
            // Место записи выбирает ОБЩИЙ носитель (фича 0340): у массива это
            // индекс, у структуры — имя поля. Прежде индекс печатался всегда,
            // и структура адресовалась как массив.
            let target_ty = target_type(target);
            let fields_of = |sname: &str| scope.structs.get(sname).cloned();
            // Раскрытие до листьев — общий носитель (фича 0366).
            for leaf in crate::generator::aggregate::leaves(target_ty.as_ref(), items, &fields_of) {
                let rhs = match &leaf.ty {
                    Some(ty) => scope.coerce(ty, leaf.value)?,
                    None => print_expression(leaf.value, scope)?,
                };
                let suffix = crate::generator::aggregate::c_like_suffix(&leaf.path);
                p.ident(&format!("{lhs}{suffix} = {rhs};")).nl();
            }
            Ok(())
        }
        ExpressionNode::Assign(target, value) => {
            let lhs = print_assign_target(target, scope)?;
            // Значение печатается ПО ЦЕЛЕВОМУ ТИПУ, а не само по себе: для
            // перечисления число восстанавливается в имя варианта. Узла варианта
            // `ExpressionNode` не имеет — `command := Up` приходит как
            // `Number(2)`, — а перечисления SV строго типизированы.
            let rhs = match target_type_in(target, Some(scope.structs)) {
                Some(ty) => scope.coerce(&ty, value)?,
                None => print_expression(value, scope)?,
            };
            p.ident(&format!("{} = {};", lhs, rhs)).nl();
            // Строб записи двунаправленного порта (фича 0428): плата защёлкивает
            // сторону `_o` только в такте, где строб поднят, — иначе внешние
            // изменения ячейки затирались бы каждым тактом (умолчание выхода —
            // «как есть»).
            if let Some(we) = inout_strobe(target, scope) {
                p.ident(&format!("{}_next = 1'b1;", we)).nl();
            }
            Ok(())
        }
        // Вызов как оператор. В SystemVerilog функция обязана использовать
        // результат, поэтому голый `f(x);` невалиден — отбрасывается `void'(…)`.
        // Практически сюда попадает лишь `debug(…)`, которую печатник выражений
        // и отвергает (`SV-002`): в синтезируемом RTL печатать некуда.
        ExpressionNode::Function(_, _) => {
            p.ident(&format!("void'({});", print_expression(expr, scope)?))
                .nl();
            Ok(())
        }
        other => Err(sv002(&format!(
            "выражение '{}' в позиции оператора",
            match other {
                ExpressionNode::None => "пустое",
                _ => "без побочного эффекта",
            }
        ))),
    }
}

/// Возвращает тип элемента, которому присваивают.
///
/// Нужен для восстановления варианта перечисления по значению: без целевого типа
/// `Number(2)` неотличимо от обычного числа.
fn target_type(target: &ExpressionNode) -> Option<TypeNode> {
    target_type_in(target, None)
}

/// Тип приёмника с учётом ПОЛЕЙ структуры (фича 0492).
///
/// ⚠️ Поле перечислимого типа — тот же класс, что элемент массива (0368):
/// `conf.mode := Run;` печаталось `conf_next.mode = 1;`, и verilator отвечал
/// **ошибкой** `ENUMVALUE`, хотя та же запись скаляром работает у всех
/// потребителей. Объявления структур приходят снимком карты: носитель типа
/// сам их не ищет.
fn target_type_in(
    target: &ExpressionNode,
    structs: Option<&std::collections::BTreeMap<String, Vec<(String, TypeNode)>>>,
) -> Option<TypeNode> {
    if let ExpressionNode::BitAccess(base, crate::parser::ast::Member::Identifier(field)) = target
        && let Some(structs) = structs
        && let Some(TypeNode::Struct(name)) = target_type_in(base, Some(structs))
        && let Some(fields) = structs.get(&name)
    {
        return fields
            .iter()
            .find(|(fname, _)| fname.as_str() == field.name)
            .map(|(_, ty)| ty.clone());
    }
    target_type_scalar(target)
}

/// Тип приёмника без учёта полей — прежнее правило.
fn target_type_scalar(target: &ExpressionNode) -> Option<TypeNode> {
    // Элемент массива несёт ТИП ЭЛЕМЕНТА (фича 0368): прежде здесь стоял
    // `None`, и `modes[0] := Work;` печаталось `modes_next[0] = 1;` —
    // verilator отвечает **ошибкой** `ENUMVALUE` («Implicit conversion to enum
    // … Suggest use enum's mnemonic»), при том что та же запись СКАЛЯРОМ
    // работает у всех девяти потребителей.
    if let ExpressionNode::ArraySubscript(base, _) = target {
        return match crate::generator::sv::sv_array::array_type_expr(base)? {
            TypeNode::Array(_, elem) => Some(*elem),
            _ => None,
        };
    }
    let ExpressionNode::Variable(var) = target else {
        // Доступ к биту даёт бит, а не перечисление.
        return None;
    };
    match &*var.borrow() {
        crate::semantic::VariableNode::Simple { ty, .. }
        | crate::semantic::VariableNode::Port { ty, .. }
        | crate::semantic::VariableNode::Const { ty, .. } => Some(ty.clone()),
        crate::semantic::VariableNode::Unresolved => None,
    }
}

/// Имя строба записи, если цель присваивания — двунаправленный порт (0428).
///
/// Разбирается **та же цепочка**, что и в [`print_assign_target`]: запись в
/// разряд или элемент порта (`line.3 := 1;`) — тоже запись порта, и строб ей
/// нужен так же, как записи целиком.
fn inout_strobe(target: &ExpressionNode, scope: &Scope) -> Option<String> {
    match target {
        ExpressionNode::Variable(var) => {
            let name = signal_of_in(var, scope)?;
            scope
                .inouts
                .contains(&name)
                .then(|| crate::generator::sv::sv_module::inout_we(&name))
        }
        ExpressionNode::ArraySubscript(base, _) | ExpressionNode::BitAccess(base, _) => {
            inout_strobe(base, scope)
        }
        _ => None,
    }
}

/// Печатает левую часть присваивания.
///
/// Отличается от печати выражения ровно одним: имя сигнала берётся **для
/// записи** (`v` → `v_next`), а не для чтения.
fn print_assign_target(target: &ExpressionNode, scope: &Scope) -> Result<String, Diagnostic> {
    match target {
        ExpressionNode::Variable(var) => signal_of_in(var, scope)
            .map(|name| scope.write(&name))
            .ok_or_else(|| sv002("неразрешённая переменная в левой части присваивания")),
        // База — выражение (фича 0358): она сама печатается КАК ЦЕЛЬ ЗАПИСИ,
        // поэтому `b.data[1] := …` даёт `b_next.data[1] = …`.
        //
        // ⚠️ Индекс сужается тем же носителем, что и при чтении (фича 0365):
        // печатников индексации ТРИ — выражения, условия и цель записи, — и
        // правка двух оставляла `WIDTHTRUNC` на записи (замер: линт цели
        // отвергал модуль, а сверка значений этого не видела, потому что
        // тестбенч собирается с `-Wno-fatal`).
        ExpressionNode::ArraySubscript(base, index) => Ok(format!(
            "{}[{}]",
            print_assign_target(base, scope)?,
            crate::generator::sv::sv_array::index_text(
                crate::generator::sv::sv_array::array_type_expr(base).as_ref(),
                crate::generator::mixed_sign::operand_type_expr(index).as_ref(),
                print_expression(index, scope)?,
            )
        )),
        // Запись в бит/поле: `x.0 := 1;` → `x_next[0] = 1;`. Основание — то же,
        // что и при чтении: в SV вектор индексируется как массив.
        ExpressionNode::BitAccess(inner, member) => {
            let base = print_assign_target(inner, scope)?;
            Ok(match member {
                crate::parser::ast::Member::Number(i) => format!("{}[{}]", base, i),
                crate::parser::ast::Member::Identifier(id) => format!("{}.{}", base, id.name),
            })
        }
        // Запись в ячейку (фича 0189): та же комбинационная пара `_next`, что у
        // переменной модели, — защёлкивание делает `always_ff`.
        ExpressionNode::AnonPort(access) => Ok(scope.write(&access.synthetic_name())),
        _ => Err(sv002("сложная левая часть присваивания")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diagnostics::Location;

    fn empty_registered() -> std::collections::BTreeSet<String> {
        std::collections::BTreeSet::new()
    }

    /// `if` печатается блоками `begin`/`end`.
    ///
    /// Скобки обязательны даже вокруг одного оператора: без них добавление
    /// второго оператора тихо вынесло бы его из-под условия.
    #[test]
    fn if_is_printed_with_begin_end() {
        let set = empty_registered();
        let enums = std::collections::BTreeMap::new();
        let structs = std::collections::BTreeMap::new();
        let warnings = std::cell::RefCell::new(Vec::new());
        let scope = Scope {
            registered: &set,
            guard_enable: true,
            inouts: crate::generator::sv::sv_scope::no_inouts(),
            function: None,
            function_ret: None,
            locals: crate::generator::sv::sv_scope::no_locals(),
            enums: &enums,
            structs: &structs,
            warnings: &warnings,
        };
        let mut out = String::new();
        let mut p = Printer::new(4, &mut out);
        let stmt = StatementNode::If {
            cond: Box::new(ExpressionNode::Bool(true)),
            then_: Box::new(StatementNode::None),
            else_: None,
            loc: Location::Codegen,
        };
        print_statement(&mut p, &stmt, &scope).unwrap();
        assert!(out.contains("if (1'b1) begin"), "нет begin:\n{out}");
        assert!(out.contains("end"), "нет end:\n{out}");
    }

    /// **Контрпример:** цикл → `SV-002`, а не молчаливая трансляция.
    ///
    /// В синтезируемом RTL цикл обязан разворачиваться в схему; условие Takt
    /// границ не гарантирует.
    #[test]
    fn loop_is_sv002() {
        let set = empty_registered();
        let enums = std::collections::BTreeMap::new();
        let structs = std::collections::BTreeMap::new();
        let warnings = std::cell::RefCell::new(Vec::new());
        let scope = Scope {
            registered: &set,
            guard_enable: true,
            inouts: crate::generator::sv::sv_scope::no_inouts(),
            function: None,
            function_ret: None,
            locals: crate::generator::sv::sv_scope::no_locals(),
            enums: &enums,
            structs: &structs,
            warnings: &warnings,
        };
        let mut out = String::new();
        let mut p = Printer::new(4, &mut out);
        let stmt = StatementNode::Loop {
            cond: None,
            body: Box::new(StatementNode::None),
            loc: Location::Codegen,
        };
        let err = print_statement(&mut p, &stmt, &scope).unwrap_err();
        assert_eq!(err.code.as_deref(), Some("SV-002"));
    }

    /// LTL-формула в теле блока пропускается молча — и это не потеря.
    ///
    /// Формула есть свойство для верификации (`taktc verify`, фича 0049), а не
    /// поведение; цель `c` поступает так же.
    #[test]
    fn inline_formula_is_skipped() {
        let set = empty_registered();
        let enums = std::collections::BTreeMap::new();
        let structs = std::collections::BTreeMap::new();
        let warnings = std::cell::RefCell::new(Vec::new());
        let scope = Scope {
            registered: &set,
            guard_enable: true,
            inouts: crate::generator::sv::sv_scope::no_inouts(),
            function: None,
            function_ret: None,
            locals: crate::generator::sv::sv_scope::no_locals(),
            enums: &enums,
            structs: &structs,
            warnings: &warnings,
        };
        let mut out = String::new();
        let mut p = Printer::new(4, &mut out);
        let stmt = StatementNode::InlineFormula(vec![]);
        assert!(print_statement(&mut p, &stmt, &scope).is_ok());
        assert!(out.is_empty(), "формула не должна ничего печатать: {out}");
    }
}
