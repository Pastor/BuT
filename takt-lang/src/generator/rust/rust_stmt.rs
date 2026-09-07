//! Трансляция операторов Takt в Rust.
//!
//! ## Чем Rust проще ST
//!
//! Цель `st` вынуждена выкручиваться: объявление переменной **поднимается** в шапку POU
//! (в теле его быть не может), вызов функции в позиции оператора требует изобретения
//! переменной-приёмника, а `match` разворачивается в `IF/ELSIF`, потому что образцы
//! Takt - произвольные выражения. В Rust всё это ложится один в один: `let` живёт в
//! теле, вызов - законный оператор, `match` есть в языке.
//!
//! Единственная тонкость - `match`: образцы Takt произвольны, а образцы Rust - нет
//! (`match x { y => ... }` связал бы `y` как **новое имя**, а не сравнил с ним).
//! Поэтому `match` транслируется в цепочку `if`/`else if` - так же, как в ST, но по
//! другой причине: там не было конструкции, здесь она есть, но с другой семантикой.

use crate::diagnostics::Diagnostic;
use crate::generator::indent::Printer;
use crate::generator::rust::rust_expr::{
    Scope, print_as_bool, print_expression, unsupported, unwrap_outer,
};
use crate::generator::rust::rust_live::{
    Folded, deferred_needs_mut, fold_assignment, fold_target, initializer_is_dead,
};
use crate::generator::rust::rust_name::rust_value_name;
use crate::generator::rust::rust_type::rust_type;
use crate::semantic::{ExpressionNode, StatementNode};
use std::collections::{BTreeMap, BTreeSet};

/// Побочный результат печати тела: предупреждения, которые обязан увидеть автор.
#[derive(Default)]
pub(crate) struct StmtOutput {
    /// Предупреждения (`RS-010`).
    pub(crate) warnings: Vec<Diagnostic>,
}

/// Разыменовывает имя, пришедшее по ссылке, в позиции значения.
///
/// Только имя целиком: `a[0]` над ссылкой работает само (автоматическое разыменование),
/// а лишняя звёздочка дала бы `(*a)[0]` - форму, которую `clippy` отвергает как
/// избыточную.
fn deref_if_by_ref(expr: &ExpressionNode, printed: &str, scope: &Scope) -> String {
    let ExpressionNode::Variable(var) = expr else {
        return printed.to_string();
    };
    let name = var.borrow().name().to_string();
    if scope.by_ref.contains(&name) {
        return format!("*{printed}");
    }
    printed.to_string()
}

/// Печатает свёрнутое значение объявления (`rust_live::Folded`).
///
/// Ветки печатаются как **выражения** (`if c { a } else { b }`), а не как присваивания:
/// в этом и смысл свёртки.
fn print_folded(
    folded: &Folded,
    ty: &crate::semantic::type_node::TypeNode,
    scope: &Scope,
) -> Result<String, Diagnostic> {
    match folded {
        Folded::Value(expr) => Ok(unwrap_outer(&crate::generator::rust::rust_expr::coerce_to(
            expr, ty, scope,
        )?)
        .to_string()),
        // Цепочка из `match`: образец сравнивается с типом разбираемого выражения - тем
        // же приёмом, что и в печати оператора `match`. Без обратного отображения
        // `match mode { 0 => ... }` при `mode : Mode` дало бы сравнение перечисления с
        // целым.
        Folded::Chain {
            subject,
            arms,
            otherwise,
        } => {
            let printed_subject = print_expression(subject, scope)?;
            let subject_type = crate::generator::rust::rust_expr::expression_type(subject);
            let mut text = String::new();
            for (patterns, value) in arms {
                let mut tests = Vec::new();
                for pattern in patterns {
                    let printed = match &subject_type {
                        Some(ty) => {
                            crate::generator::rust::rust_expr::coerce_to(pattern, ty, scope)?
                        }
                        None => print_expression(pattern, scope)?,
                    };
                    tests.push(format!("{printed_subject} == {printed}"));
                }
                let head = if text.is_empty() { "if" } else { " else if" };
                text.push_str(&format!(
                    "{head} {} {{ {} }}",
                    tests.join(" || "),
                    print_folded(value, ty, scope)?
                ));
            }
            let tail = print_folded(otherwise, ty, scope)?;
            // Ветвей с образцами может не быть вовсе (`match x { _ => ... }`) - тогда
            // цепочки нет и печатается одно значение.
            if text.is_empty() {
                return Ok(tail);
            }
            text.push_str(&format!(" else {{ {tail} }}"));
            Ok(text)
        }
        Folded::Branch { cond, then_, else_ } => Ok(format!(
            "if {} {{ {} }} else {{ {} }}",
            unwrap_outer(&print_as_bool(cond, scope)?),
            print_folded(then_, ty, scope)?,
            print_folded(else_, ty, scope)?
        )),
    }
}

/// Печатает объявление, переехавшее к своему затирающему оператору.
///
/// `stmt` - тот самый оператор (безусловное присваивание либо `if/else`); `rest` - что
/// идёт за ним (нужно, чтобы решить вопрос о `mut`).
fn emit_folded_declaration(
    name: &str,
    ty: &crate::semantic::type_node::TypeNode,
    stmt: &StatementNode,
    rest: &[StatementNode],
    scope: &mut Scope,
    p: &mut Printer,
) -> Result<(), Diagnostic> {
    let folded = fold_assignment(name, stmt).ok_or_else(|| {
        unsupported(&format!(
            "объявление '{}': форма присваивания не сворачивается",
            name
        ))
    })?;
    let ident = rust_value_name(name, crate::diagnostics::Location::Codegen)?;
    let ty_name = rust_type(ty, &format!("переменная '{}'", name))?;
    let value = print_folded(&folded, ty, scope)?;
    // `mut` считается по остатку после точки инициализации: присваивание на каждом пути
    // здесь - инициализация, а не изменение.
    let mutable = if rest.iter().any(|s| assigns_later(name, s)) {
        "mut "
    } else {
        ""
    };
    p.ident(&format!(
        "let {}{}: {} = {};",
        mutable, ident, ty_name, value
    ))
    .nl();
    scope.locals.push(name.to_string());
    Ok(())
}

/// Нужно ли отложенному объявлению умолчание.
///
/// Агрегат - массив или структура - в Rust обязан быть инициализирован
/// **целиком** до записи по индексу либо в поле: `let part: [u8; 2]; part[0] =
/// ...;` даёт `E0381` ("used binding `part` isn't initialized"), тогда как
/// эталон, `c`, `st` и `sv` тот же вход исполняют.
///
/// Скаляру умолчание не нужно и **вредно**: там отложенная форма законна, а лишнее
/// значение дало бы `unused_assignments` - отказ проверки под `-D warnings` (тот самый
/// класс, ради которого заведена 0216).
///
/// Бит-вектор `[bit; N <= 64]` - упакованный **скаляр**, и умолчания ему не нужно; при
/// `N > 64` он массив слов, и нужно.
fn needs_deferred_default(ty: &crate::semantic::type_node::TypeNode) -> bool {
    use crate::semantic::type_node::TypeNode;
    match ty {
        TypeNode::Array(..) => {
            crate::semantic::bit_vector::is_bit_vector(ty).is_none_or(|bits| bits > 64)
        }
        TypeNode::Struct(_) => true,
        _ => false,
    }
}

/// Присваивают ли переменной в этом операторе (для решения о `mut`).
fn assigns_later(name: &str, stmt: &StatementNode) -> bool {
    let mut assigned = BTreeSet::new();
    crate::generator::rust::rust_assigned::collect_assigned(stmt, &mut assigned);
    assigned.contains(name)
}

/// Печатник хвостового оператора блока.
///
/// Возвращает `true`, если оператор напечатан им, и `false` - если печатать его надо
/// обычным путём. Существует ради тела функции: там завершающий `return x;` обязан
/// стать хвостовым выражением `x` (`needless_return`).
pub(crate) type TailPrinter<'a> = &'a dyn Fn(
    &StatementNode,
    &mut Scope,
    &mut Printer,
    &mut StmtOutput,
) -> Result<bool, Diagnostic>;

/// Печатает блок операторов.
///
/// Общая точка для тела блока и тела функции: обе обязаны одинаково переносить
/// объявления с мёртвым инициализатором (`rust_live`).
///
/// `tail` - если задан, последний оператор печатается этой функцией (у тела функции это
/// хвостовое выражение вместо `return`).
pub(crate) fn print_block(
    items: &[StatementNode],
    tail: Option<TailPrinter>,
    scope: &mut Scope,
    p: &mut Printer,
    out: &mut StmtOutput,
) -> Result<(), Diagnostic> {
    // Объявление с мёртвым инициализатором переезжает к оператору, который его
    // затирает, и сворачивается в `let x = ...`. Иначе остаётся отложенная форма (`let
    // x: T;`), которую clippy не принимает (`needless_late_init`). Корпус требует
    // именно переезда: `travel_time` в `stacker.takt` объявляет `ds`/`dr`/`dy`/`t`
    // подряд, а затирает их ниже - по одному `if/else`.
    //
    // Безопасность переезда обосновывает `rust_live::fold_target`: между старым и новым
    // местом переменная не упоминается.
    let mut folded_at: BTreeMap<usize, (String, crate::semantic::type_node::TypeNode)> =
        BTreeMap::new();
    let mut moved: BTreeSet<usize> = BTreeSet::new();
    for (i, item) in items.iter().enumerate() {
        let StatementNode::Variable(name, ty, Some(_), _) = item else {
            continue;
        };
        let rest_of = &items[i + 1..];
        if !initializer_is_dead(name, rest_of) {
            continue;
        }
        let Some(offset) = fold_target(name, rest_of) else {
            continue;
        };
        let target = i + 1 + offset;
        // На один оператор - одно объявление: `if/else`, затирающий две переменные
        // сразу, свернуть в одно `let` нельзя.
        if folded_at.contains_key(&target) {
            continue;
        }
        folded_at.insert(target, (name.clone(), ty.clone()));
        moved.insert(i);
    }

    let last_idx = items.len().saturating_sub(1);
    let mut idx = 0;
    while idx < items.len() {
        // Объявление уехало вниз - здесь не печатаем ничего.
        if moved.contains(&idx) {
            idx += 1;
            continue;
        }
        if let Some((name, ty)) = folded_at.get(&idx) {
            emit_folded_declaration(name, ty, &items[idx], &items[idx + 1..], scope, p)?;
            idx += 1;
            continue;
        }
        // Хвост печатается особым образом только если он и правда последний и его не
        // поглотила свёртка.
        if let Some(tail) = tail
            && idx == last_idx
            && tail(&items[idx], scope, p, out)?
        {
            idx += 1;
            continue;
        }
        // Комментарий автора обрамляет оператор. Ставится только на общем пути печати:
        // у свёрнутого объявления (`folded_at`) и у хвоста свои печатники, и
        // комментарий, поставленный здесь, уехал бы к чужой строке.
        let mut eaten = 0;
        crate::generator::comments::emit_around(
            p,
            items[idx].loc(),
            crate::generator::header::CommentStyle::Slashes,
            |p| {
                eaten = print_statement_ctx(&items[idx], &items[idx + 1..], scope, p, out)?;
                Ok(())
            },
        )?;
        idx += 1 + eaten;
    }
    // Неиспользуемая локальная гасится заглушкой: без неё `rustc -D warnings` отвечает
    // "unused variable", то есть вывод не собирается под флагами проверки этой же цели при
    // нулевом коде возврата `taktc`. Идиома - та же, что у неиспользуемого параметра;
    // место - конец блока, где переменная ещё в области видимости.
    for name in crate::generator::local_stub::unused_locals(items) {
        p.ident(&format!(
            "let _ = {};",
            crate::semantic::naming::normalize_lowercase_snakecase(name)
        ))
        .nl();
    }
    Ok(())
}

/// Сворачиваемо ли последнее выражение хвостовой позиции в значение.
///
/// **Единственный источник истины** правила "всё или ничего":
/// печатник [`print_tail`] вызывается только после `true` и форму узла повторно
/// не разбирает. Разойдись предикат и печатник - тот же класс дефекта, что
/// `function_needs` / `rust_expr::call` ("разойдись они, код не собрался бы").
pub(crate) fn tail_foldable(stmt: &StatementNode) -> bool {
    match stmt {
        // `return e;` в хвосте -> `e`.
        StatementNode::Return(Some(_), _) => true,
        // Блок сворачиваем, если сворачиваем его последний оператор (операторы перед
        // ним печатаются как есть - R8).
        StatementNode::Block(items) => items.last().is_some_and(tail_foldable),
        // `if/else` сворачиваем ⟺ сворачиваемы обе ветки (рекурсивно).
        StatementNode::If {
            then_,
            else_: Some(else_),
            ..
        } => tail_foldable(then_) && tail_foldable(else_),
        // `if` без `else` (значения ветки-пропуска нет) и всё прочее - нет.
        _ => false,
    }
}

/// Печатает хвостовую позицию тела/ветки как **выражение**.
///
/// `return e;` -> `e`; завершающий `if/else` со сворачиваемыми ветвями -> `if c {
/// <хвост> } else { <хвост> }` (рекурсивно, цепочка `else if` - та же рекурсия по
/// `else_ = If`). Возвращает `true`, если оператор напечатан как хвост; `false` -
/// печатать обычным путём (`return` остаётся, поведение как сегодня;, R5).
///
/// Годится как [`TailPrinter`] для [`print_block`] - сигнатура совпадает.
pub(crate) fn print_tail(
    stmt: &StatementNode,
    scope: &mut Scope,
    p: &mut Printer,
    out: &mut StmtOutput,
) -> Result<bool, Diagnostic> {
    match stmt {
        StatementNode::Return(Some(expr), _) => {
            // Хвостовое выражение - тот же приёмник, что и `return`: путей печати
            // возврата два, и правило обязано стоять в обоих - иначе оно действует
            // через раз, в зависимости от того, последний ли это оператор тела.
            let text = match &scope.return_type {
                Some(ty) => crate::generator::rust::rust_expr::coerce_to(expr, ty, scope)?,
                None => print_expression(expr, scope)?,
            };
            // Хвост - та же позиция значения, что `return`: имя, пришедшее по ссылке,
            // разыменовывается. Печатников возврата два (хвост и явный `return`), и
            // правка одного оставляет второй -.
            let text = deref_if_by_ref(expr, &text, scope);
            p.ident(unwrap_outer(&text)).nl();
            Ok(true)
        }
        // Хвостовой `if/else` - только когда сворачиваемы обе ветки (предикат -
        // единственное место решения). Условие через `unwrap_outer`: иначе `if (c)
        // {...}` словил бы `unused_parens` (A-3 ).
        StatementNode::If {
            cond,
            then_,
            else_: Some(else_),
            ..
        } if tail_foldable(stmt) => {
            p.ident(&format!(
                "if {} {{",
                unwrap_outer(&print_as_bool(cond, scope)?)
            ))
            .nl();
            p.up();
            print_tail_branch(then_, scope, p, out)?;
            p.down();
            p.ident("} else {").nl();
            p.up();
            print_tail_branch(else_, scope, p, out)?;
            p.down();
            p.ident("}").nl();
            Ok(true)
        }
        _ => Ok(false),
    }
}

/// Печатает ветку хвостового `if/else` - **через `print_block`** с тем же печатником
/// хвоста.
///
/// Мимо `print_block` нельзя: он несёт свёртку мёртвого инициализатора
/// (`rust_live`); ветка в обход потеряла бы перенос объявлений и словила
/// `needless_late_init`. Это же сохраняет операторы перед хвостом ветки.
fn print_tail_branch(
    stmt: &StatementNode,
    scope: &mut Scope,
    p: &mut Printer,
    out: &mut StmtOutput,
) -> Result<(), Diagnostic> {
    match stmt {
        StatementNode::Block(items) => print_block(items, Some(&print_tail), scope, p, out),
        // Не-Block сворачиваемый хвост (голый `return e` или вложенный `if/else`):
        // предикат уже гарантировал сворачиваемость, print_tail его напечатает.
        other => print_tail(other, scope, p, out).map(|_| ()),
    }
}

/// Печатает оператор Takt в Rust.
///
/// # Ошибки
/// [`RS-011`] на непереводимой конструкции - не тихий пропуск.
pub(crate) fn print_statement(
    stmt: &StatementNode,
    scope: &mut Scope,
    p: &mut Printer,
    out: &mut StmtOutput,
) -> Result<(), Diagnostic> {
    print_statement_ctx(stmt, &[], scope, p, out).map(|_| ())
}

/// Печатает оператор, зная **остаток блока**.
///
/// Остаток нужен ровно одному узлу - объявлению переменной: только по нему видно,
/// затирается ли инициализатор до первого чтения (`rust_live`). Печать потоковая,
/// поэтому "посмотреть вперёд" иначе нельзя. Возвращает число операторов из `rest`,
/// **поглощённых** этим вызовом.
///
/// Ненулевым бывает только у объявления с мёртвым инициализатором: оно сворачивает
/// следующее присваивание в своё значение (`let x = if ... {...} else {...};`), и
/// печатать его второй раз нельзя.
pub(crate) fn print_statement_ctx(
    stmt: &StatementNode,
    rest: &[StatementNode],
    scope: &mut Scope,
    p: &mut Printer,
    out: &mut StmtOutput,
) -> Result<usize, Diagnostic> {
    match stmt {
        StatementNode::None => Ok(0),
        // Блок формул адресован внешнему анализатору: печатать нечего; вставка
        // печатается целью, чьё имя названо (без имени - всеми).
        StatementNode::Formula(_) => Ok(0),
        StatementNode::Assembly { target, body, .. } => {
            if crate::semantic::target_block::emits_for(target.as_deref(), "rust") {
                print_statement_ctx(body, &[], scope, p, out)?;
            }
            Ok(0)
        }
        StatementNode::Block(items) => print_block(items, None, scope, p, out).map(|_| 0),
        StatementNode::Expression(expr, loc) => {
            // Место оператора - для отказов печати выражений: своей позиции у них нет
            // (решение 0056), и до этой фичи цель печатала отказ без координаты вовсе.
            crate::generator::site::enter(*loc);
            // Присваивание среза печатается поэлементно - так же, как у `c`, `st` и
            // `sv`. Выражением его не напечатать: `&self.src[0..2]` имеет тип `[u8]`,
            // не `Sized`, а `copy_from_slice` - оператор, не выражение. Границы -
            // литералы, проверенные `SE-029`.
            if let ExpressionNode::Assign(target, value) = expr.as_ref()
                && let ExpressionNode::ArraySlice(src, from, to) = value.as_ref()
            {
                // База - выражение: печатается тем же печатником.
                let dst = print_expression(target, scope)?;
                let base = print_expression(src, scope)?;
                // Пригодны оба операнда: приёмник тоже обязан быть настоящим массивом
                // (`res := mem[1:2];` при `res: u8` эталон не исполняет - `SIM-006`).
                // Тип базы даёт общий носитель.
                let dst_ok =
                    crate::generator::slice::elementwise_len_of(target, scope.model).is_some();
                let src_len = if dst_ok {
                    crate::generator::slice::elementwise_len_of(src, scope.model)
                } else {
                    None
                };
                // Срез над бит-вектором поэлементно не выразим: вход уходит прежним
                // путём - к отказу `RS-011`, как у эталона.
                if let Some(src_len) = src_len {
                    let (start, len) = crate::generator::slice::bounds(*from, *to, src_len);
                    for k in 0..len {
                        p.ident(&format!("{dst}[{k}] = {base}[{}];", start + k))
                            .nl();
                    }
                    return Ok(0);
                }
            }
            p.ident(&format!("{};", print_expression(expr, scope)?))
                .nl();
            Ok(0)
        }
        StatementNode::Variable(name, ty, init, loc) => {
            // Объявление тела объявляет своё место: позиция у него есть с 0386, а отказ
            // печати типа или инициализатора приходил без координаты.
            crate::generator::site::enter(*loc);
            let ident = rust_value_name(name, crate::diagnostics::Location::Codegen)?;
            let ty_name = rust_type(ty, &format!("переменная '{}'", name))?;
            // `mut` ставится по факту присваивания, а не по объявлению: в Takt `var`
            // изменяем всегда, в Rust лишний `mut` - это `unused_mut`.
            let mutable = if scope.assigned.contains(name) {
                "mut "
            } else {
                ""
            };
            // Инициализатор, затираемый до первого чтения, не печатается: `let mut ds:
            // u8 = 0;` перед `if/else`, пишущим обе ветки, даёт `unused_assignments` -
            // отказ проверки. Анализ консервативен, а ошибка в сторону "мёртв" даёт ошибку
            // компиляции, а не тихо неверный код (см.
            let dead = init.is_some() && initializer_is_dead(name, rest);
            match init {
                // Мёртвый инициализатор: печатается не он, а то значение, что его
                // затирает. `mut` тут считается иначе - присваивание на каждом пути
                // является инициализацией, а не изменением (см.
                Some(_) if dead => {
                    let deferred_mut = if deferred_needs_mut(name, rest) {
                        "mut "
                    } else {
                        ""
                    };
                    // Свёртка в `let x = ...`: отложенное объявление clippy тоже не
                    // принимает (`needless_late_init`), и справедливо - так читается
                    // лучше. Свернуть удаётся всегда, когда вердикт "мёртв" дало
                    // распознанное присваивание; иначе печатается отложенная форма.
                    match rest.first().and_then(|next| fold_assignment(name, next)) {
                        Some(folded) => {
                            let value = print_folded(&folded, ty, scope)?;
                            p.ident(&format!(
                                "let {}{}: {} = {};",
                                deferred_mut, ident, ty_name, value
                            ))
                            .nl();
                            scope.locals.push(name.clone());
                            // Присваивание поглощено - печатать его второй раз значило
                            // бы присвоить дважды.
                            return Ok(1);
                        }
                        None => {
                            p.ident(&format!("let {}{}: {};", deferred_mut, ident, ty_name))
                                .nl();
                        }
                    }
                }
                Some(expr) => {
                    p.ident(&format!(
                        "let {}{}: {} = {};",
                        mutable,
                        ident,
                        ty_name,
                        unwrap_outer(&crate::generator::rust::rust_expr::coerce_to(
                            expr, ty, scope
                        )?)
                    ))
                    .nl();
                }
                // Без инициализатора: первое присваивание - это инициализация, а не
                // изменение, и `mut` ей не нужен.
                //
                // Признак - тот же `deferred_needs_mut`, которым уже живёт соседняя
                // ветвь мёртвого инициализатора: второе знание о том, "что считать
                // изменением", разошлось бы с первым.
                None => {
                    let deferred_mut = if deferred_needs_mut(name, rest) {
                        "mut "
                    } else {
                        ""
                    };
                    // Свёртка в `let x: T = ...` - та же, что у ветви мёртвого
                    // инициализатора: отложенное объявление clippy не принимает
                    // (`needless_late_init`), и это **отказ** проверки цели под `-D
                    // warnings`.
                    match rest.first().and_then(|next| fold_assignment(name, next)) {
                        Some(folded) => {
                            let value = print_folded(&folded, ty, scope)?;
                            p.ident(&format!(
                                "let {}{}: {} = {};",
                                deferred_mut, ident, ty_name, value
                            ))
                            .nl();
                            scope.locals.push(name.clone());
                            // Присваивание поглощено - печатать его второй раз значило
                            // бы присвоить дважды.
                            return Ok(1);
                        }
                        None => {
                            // Агрегат обязан получить умолчание: Rust требует
                            // инициализировать массив (и структуру) целиком **до**
                            // записи по индексу или в поле - иначе `E0381` "used
                            // binding isn't initialized" при нулевом коде возврата
                            // `taktc`. Скаляру умолчание не нужно и вредно: там
                            // отложенная форма законна, а лишнее значение дало бы
                            // `unused_assignments`.
                            if needs_deferred_default(ty) {
                                let value = crate::generator::rust::rust_decl::default_value(
                                    ty,
                                    scope.model,
                                )?;
                                p.ident(&format!("let mut {}: {} = {};", ident, ty_name, value))
                                    .nl();
                            } else {
                                p.ident(&format!("let {}{}: {};", deferred_mut, ident, ty_name))
                                    .nl();
                            }
                        }
                    }
                }
            }
            // Имя объявлено в теле и полем модели не является. Без этой записи печатник
            // обратился бы к нему как `self.i` - то есть искал бы поле, которого нет
            // ("no field `i` on type ..."). Регистрация идёт после печати
            // инициализатора: `var x := x;` в Takt читает внешний `x`.
            scope.locals.push(name.clone());
            Ok(0)
        }
        StatementNode::If {
            cond, then_, else_, ..
        } => {
            // Вложенный `if`, составляющий всё тело, сливается с внешним: `clippy` под
            // `-D warnings` отвечает "this `if` statement can be collapsed" - отказ
            // проверки самой цели при нулевом коде возврата `taktc`. Условие языка
            // побочных эффектов не имеет (присваивание - оператор), поэтому слияние
            // поведение не меняет; тот же приём применён к обёртке раннего возврата.
            //
            // Правило узкое, и границы замерены: `else` у внешнего либо у
            // внутреннего, а также лишний оператор рядом - `clippy` принимает, и такие
            // формы не трогаются.
            let (conds, then_, else_) =
                crate::generator::rust::rust_match::collapse_nested_if(cond, then_, else_);

            // Ветви печатаются В буфер: тело, состоящее только из темпоральной формулы,
            // до цели не доезжает по существу (её место - `taktc verify`), и оставался
            // `if ... { }` - "this `if` branch is empty", то есть отказ `clippy` под
            // `-D warnings` при нулевом коде возврата `taktc`.
            //
            // Условие языка побочных эффектов не имеет (присваивание - оператор),
            // поэтому пустая конструкция опускается целиком. Тот же приём, что у
            // пустого `IF` цели `st`.
            let mut then_text = String::new();
            {
                let mut buffer = p.fork(&mut then_text);
                buffer.up();
                print_statement(then_, scope, &mut buffer, out)?;
                buffer.down();
            }
            let mut else_text = String::new();
            if let Some(alt) = else_ {
                let mut buffer = p.fork(&mut else_text);
                buffer.up();
                print_statement(alt, scope, &mut buffer, out)?;
                buffer.down();
            }
            if then_text.trim().is_empty() && else_text.trim().is_empty() {
                return Ok(0);
            }
            // Одно условие печатается как прежде (внешние скобки снимаются - `if (x) {`
            // это `unused_parens`); слитые - конъюнкцией, где скобки подвыражений
            // обязаны остаться.
            let head = if conds.len() == 1 {
                unwrap_outer(&print_as_bool(conds[0], scope)?).to_string()
            } else {
                let mut parts = Vec::with_capacity(conds.len());
                for c in &conds {
                    parts.push(print_as_bool(c, scope)?);
                }
                parts.join(" && ")
            };
            p.ident(&format!("if {head} {{")).nl();
            if then_text.trim().is_empty() {
                // Пустая ветвь `then` при непустом `else` в Rust законна, но `clippy`
                // её не принимает - печатаем комментарий-заполнитель.
                p.up();
                p.down();
            } else {
                p.print(&then_text);
            }
            if !else_text.trim().is_empty() {
                p.ident("} else {").nl();
                p.print(&else_text);
            }
            p.ident("}").nl();
            Ok(0)
        }
        StatementNode::Loop { cond, body, .. } => {
            match cond {
                Some(c) => {
                    p.ident(&format!(
                        "while {} {{",
                        unwrap_outer(&print_as_bool(c, scope)?)
                    ))
                    .nl();
                }
                None => {
                    p.ident("loop {").nl();
                }
            }
            p.up();
            print_statement(body, scope, p, out)?;
            p.down();
            p.ident("}").nl();
            Ok(0)
        }
        // `for` Takt - это C-подобный `for(init; cond; step)`, а не итератор.
        // Разворачивается в блок с `while`: иначе `step` пришлось бы дублировать перед
        // каждым `continue`.
        StatementNode::For {
            init,
            cond,
            step,
            body,
            ..
        } => {
            p.ident("{").nl();
            p.up();
            if let Some(init) = init {
                print_statement(init, scope, p, out)?;
            }
            match cond {
                Some(c) => {
                    p.ident(&format!(
                        "while {} {{",
                        unwrap_outer(&print_as_bool(c, scope)?)
                    ))
                    .nl();
                }
                None => {
                    p.ident("loop {").nl();
                }
            }
            p.up();
            print_statement(body, scope, p, out)?;
            if let Some(step) = step {
                p.ident(&format!("{};", print_expression(step, scope)?))
                    .nl();
            }
            p.down();
            p.ident("}").nl();
            p.down();
            p.ident("}").nl();
            Ok(0)
        }
        StatementNode::Return(value, _) => {
            match value {
                // `return (x);` - ещё одна позиция, где скобки лишние.
                Some(expr) => {
                    // `return` - позиция приёмника с известным типом: `return 1;` при
                    // `-> bit` обязано печататься `true`, вариант перечисления -
                    // именем, разряд - числом.
                    let printed = match &scope.return_type {
                        Some(ty) => crate::generator::rust::rust_expr::coerce_to(expr, ty, scope)?,
                        None => print_expression(expr, scope)?,
                    };
                    // Имя, пришедшее по ссылке (`&[T; N]`), в позиции значения
                    // разыменовывается: `return a;` при `fn pick(a: [u8; 2]) -> [u8;
                    // 2]` давало `E0308` при НУЛЕВОМ коде возврата `taktc`. Массив
                    // здесь `Copy`, поэтому `*a` - копия, а не перемещение.
                    let printed = deref_if_by_ref(expr, &printed, scope);
                    p.ident(&format!("return {};", unwrap_outer(&printed))).nl();
                }
                None => {
                    p.ident("return;").nl();
                }
            }
            Ok(0)
        }
        StatementNode::Continue(_) => {
            p.ident("continue;").nl();
            Ok(0)
        }
        StatementNode::Break(_) => {
            p.ident("break;").nl();
            Ok(0)
        }
        // Образцы Takt - произвольные выражения, а `match x { y => ... }` в Rust связал
        // Бы `y` как новое имя вместо сравнения с ним (печать - свой модуль
        // `rust_match`).
        StatementNode::Match { expr, arms, .. } => {
            crate::generator::rust::rust_match::print_match(expr, arms, scope, p, out)
        }
        // Формула в теле блока: Охранная печатается `assert!`, темпоральная - предмет
        // `taktc verify`.
        //
        // Прежде обе давали `RS-010` "LTL-формула не транслируется", хотя охранную цель
        // переводит на уровне модели и состояния с: сообщение говорило о
        // непереводимости того, что переводится, и называло LTL, которого во входе не
        // было вовсе. Формула-Оператор и формула-Элемент печатаются теперь одним
        // носителем (`emit_guard`) - второй ответ на один вопрос и был дефектом.
        StatementNode::InlineFormula(formulas) => {
            if scope.guard_enable {
                for formula in formulas {
                    // Отказ печати условия не роняет компиляцию: формула -
                    // обязательство, а не поведение, и вход, который прежде переводился
                    // (формула молча терялась), обязан переводиться и теперь.
                    // Непереводимое условие становится предупреждением С причиной -
                    // например обращение к переменной модели из тела функции
                    // (`RS-017`): у цели `rust` функция свободна.
                    if let Err(why) =
                        crate::generator::rust::rust_tick::emit_guard(p, formula, scope)
                    {
                        out.warnings.push(
                            Diagnostic::warning(
                                why.loc,
                                format!(
                                    "охранная формула в теле не транслируется в Rust: {}. \
                                     Порождённый код проверки не содержит",
                                    why.message
                                ),
                            )
                            .with_code("RS-010"),
                        );
                    }
                }
            }
            // Предупреждение - только о темпоральной: она в прошивку не попадает по
            // существу, и молчать о ней нельзя.
            if formulas.iter().any(has_temporal) {
                out.warnings.push(
                    Diagnostic::warning(
                        crate::semantic::formula::first_location(formulas)
                            .unwrap_or(crate::diagnostics::Location::Codegen),
                        "LTL-формула в теле блока не транслируется в Rust: \
                         проверяйте её через 'taktc verify'. Порождённый код \
                         формулу не содержит"
                            .to_string(),
                    )
                    .with_code("RS-010"),
                );
            }
            Ok(0)
        }
        StatementNode::Unresolved(_) => Err(unsupported("неразрешённый оператор")),
    }
}

/// Есть ли в формуле темпоральная часть.
///
/// Предупреждение `RS-010` адресовано только ей: охранная форма печатается `assert!` и
/// молчания не требует.
fn has_temporal(formula: &crate::semantic::formula::Formula) -> bool {
    use crate::semantic::formula::Formula;
    match formula {
        Formula::LTL(_, _) => true,
        Formula::Formulas(items) => items.iter().any(has_temporal),
        Formula::Guard(_, _, _) | Formula::None => false,
    }
}

#[cfg(test)]
mod tail_tests {
    use super::tail_foldable;
    use crate::diagnostics::Location;
    use crate::semantic::{ExpressionNode, StatementNode};

    fn ret(n: i128) -> StatementNode {
        StatementNode::Return(Some(Box::new(ExpressionNode::Number(n))), Location::Codegen)
    }
    fn if_(then_: StatementNode, else_: Option<StatementNode>) -> StatementNode {
        StatementNode::If {
            cond: Box::new(ExpressionNode::Bool(true)),
            then_: Box::new(then_),
            else_: else_.map(Box::new),
            loc: Location::Codegen,
        }
    }

    /// `return e;` в хвосте - сворачиваем.
    #[test]
    fn return_is_foldable() {
        assert!(tail_foldable(&ret(1)));
    }

    /// `if/else` со сворачиваемыми ветвями - сворачиваем ().
    #[test]
    fn if_else_both_return_is_foldable() {
        assert!(tail_foldable(&if_(ret(1), Some(ret(2)))));
    }

    /// Блок сворачиваем по своему последнему оператору (операторы до него - как есть,
    /// R8).
    #[test]
    fn block_folds_on_last_statement() {
        let block = StatementNode::Block(vec![
            StatementNode::Expression(
                Box::new(ExpressionNode::Number(0)),
                crate::diagnostics::Location::default(),
            ),
            ret(1),
        ]);
        assert!(tail_foldable(&block));
    }

    /// **T11 (негативный тест):** `if` без `else` не сворачиваем (значения
    /// ветки-пропуска нет). Мутация "сворачиваем" обязана валить тест.
    #[test]
    fn if_without_else_is_not_foldable() {
        assert!(!tail_foldable(&if_(ret(1), None)));
    }

    /// Смешанные ветки (одна не завершается сворачиваемым хвостом) - не сворачиваем (:
    /// "всё или ничего").
    #[test]
    fn mixed_branches_are_not_foldable() {
        let else_no_return = StatementNode::Expression(
            Box::new(ExpressionNode::Number(5)),
            crate::diagnostics::Location::default(),
        );
        assert!(!tail_foldable(&if_(ret(1), Some(else_no_return))));
    }

    /// Цепочка `else if` (вложенный `if/else` в `else_`) - сворачиваема целиком.
    #[test]
    fn else_if_chain_is_foldable() {
        let inner = if_(ret(2), Some(ret(3)));
        assert!(tail_foldable(&if_(ret(1), Some(inner))));
    }
}
