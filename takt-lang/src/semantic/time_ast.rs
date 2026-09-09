//! Проход по АСД для конструкций времени: частота и `every`.
//!
//! Отдельная стадия, а не ветка в `tree.rs`, по двум причинам. Первая формальная:
//! `tree.rs` пришпилен реестром размеров и расти не имеет права. Вторая содержательная:
//! частота - **свойство единицы компиляции**, а не элемента модели, и собирать её
//! обходом всего дерева честнее, чем накапливать в разборе элементов.
//!
//! ## Что делает
//!
//! 1. Собирает объявления `clock` (включая вложенные модели). Все они обязаны
//!    называть **одну** частоту: разные значения - ошибка автора (`SE-067`), а
//!    не "побеждает последнее". Итог кладётся в
//!    [`ModelNode::clock_hz`](super::ModelNode::clock_hz) корня.
//! 2. Следит, что `after` стоит **только** в условии перехода `ref` (`SE-068`);
//!    `every 100ms { ... }` - разворачивается семантикой в блок.

use crate::diagnostics::Diagnostic;
use crate::diagnostics::lang::keys;
use crate::msg;
use crate::parser::ast::{Condition, InlineFormulaDefine, Model, ModelElement, StateElement};
use crate::semantic::ModelNode;
use std::cell::RefCell;
use std::rc::Rc;

/// Обходит АСД: собирает частоту тактирования и отвергает нереализованное.
///
/// # Ошибки
///
/// - `SE-067` - две несовпадающие частоты в одной единице компиляции;
/// - `SE-068` - `after` вне условия перехода `ref`.
pub(crate) fn collect_clock(ast: &Model, model: &Rc<RefCell<ModelNode>>) -> Result<(), Diagnostic> {
    // Обход начинается с частоты, уже поднятой из подключённых файлов: единица
    // компиляции - это файл **вместе** со своими импортами, поэтому конфликт "в
    // библиотеке 1 кГц, в корне 8 МГц" судит та же проверка, что и два объявления в
    // одном файле.
    let mut found: Option<u64> = model.borrow().clock_hz;
    walk(ast, &mut found)?;
    if let Some(hertz) = found {
        model.borrow_mut().clock_hz = Some(hertz);
    }
    Ok(())
}

/// Поднимает частоту подключённого файла к импортёру.
///
/// Объявление `clock` - контракт тактирования всей сборки (`SE-069` и `SE-070` требуют
/// от цели `c` совпадающего `--tick-hz`), и граница импорта его не разрывает:
/// библиотека, объявившая частоту, обязывает импортёра так же, как объявление в его
/// собственном файле.
///
/// Без подъёма `clock` подключённого файла теряется, профиль остаётся "часы", и прошивка
/// собирается с частотой, отличной от объявленной, без единого слова.
///
/// # Ошибки
///
/// - `SE-067` - подключённый файл называет частоту, отличную от уже известной.
pub(crate) fn adopt_clock(
    importer: &Rc<RefCell<ModelNode>>,
    imported: &Rc<RefCell<ModelNode>>,
    loc: crate::diagnostics::Location,
) -> Result<(), Diagnostic> {
    let Some(hertz) = imported.borrow().clock_hz else {
        return Ok(());
    };
    let known = importer.borrow().clock_hz;
    match known {
        Some(previous) if previous != hertz => Err(Diagnostic::error(
            loc,
            msg!(
                keys::SE_067_CLOCK_DECLARED_TWICE_IMPORTED,
                previous = previous,
                hertz = hertz
            ),
        )
        .with_code("SE-067")),
        Some(_) => Ok(()),
        None => {
            importer.borrow_mut().clock_hz = Some(hertz);
            Ok(())
        }
    }
}

/// Использует ли модель выдержку `after` хотя бы на одном ребре.
///
/// Нужно генераторам: поле-счётчик времени эмитится **только** при использовании ( -
/// модель без времени даёт прежний вывод байт-в-байт). Условия рёбер живут как
/// `Unresolved(ast)` (инвариант проекта), поэтому проверяются оба представления: и
/// сырое АСД, и разрешённый узел.
pub fn model_uses_after(model: &ModelNode) -> bool {
    model.states.values().any(|state| {
        state
            .references()
            .iter()
            .any(|reference| cond_uses_after(&reference.cond))
    })
}

/// Есть ли `after` в условии перехода (в любом из двух представлений).
fn cond_uses_after(cond: &crate::semantic::ConditionNode) -> bool {
    match cond {
        crate::semantic::ConditionNode::After(_)
        | crate::semantic::ConditionNode::AfterTicks(_)
        // Вычисляемая выдержка - тоже выдержка: без этой ветви генераторы не завели бы
        // поле времени, и условие сравнивало бы несуществующий счётчик. Компилятор
        // здесь **не помогает**: разбор заканчивается `_ => false`, поэтому ветвь
        // добавляется руками.
        | crate::semantic::ConditionNode::AfterExpr(_) => true,
        crate::semantic::ConditionNode::Unresolved(raw) => find_after(raw).is_some(),
        _ => false,
    }
}

/// Отвергает `after` в условии, стоящем не на ребре перехода (`SE-068`).
fn reject_after(cond: &Condition, place: &str) -> Result<(), Diagnostic> {
    if let Some(loc) = find_after(cond) {
        return Err(
            Diagnostic::error(loc, msg!(keys::SE_068_AFTER_OUTSIDE_REF, place = place))
                .with_code("SE-068"),
        );
    }
    Ok(())
}

/// Отвергает `after` в Guard-формуле (`: условие;`).
///
/// В LTL-формуле условие не хранится обычным `Condition` (там свой узел `LtlExpr`), а
/// лексема `after` в атом LTL не входит - поэтому проверять там нечего: до семантики
/// такая запись не доходит.
fn reject_after_in_formula(def: &InlineFormulaDefine) -> Result<(), Diagnostic> {
    match def {
        InlineFormulaDefine::Guard { conditions, .. } => {
            for cond in conditions {
                reject_after(cond, &msg!(keys::AFTER_PLACE_GUARD_FORMULA))?;
            }
            Ok(())
        }
        InlineFormulaDefine::Ltl { .. } => Ok(()),
    }
}

/// Ищет `after` в дереве условия; возвращает его позицию.
///
/// Обход исчерпывающий по построению: новый узел условия сюда не попадёт молча -
/// компилятор потребует ветку (в отличие от `_ =>`, который проглотил бы её).
fn find_after(cond: &Condition) -> Option<crate::diagnostics::Location> {
    match cond {
        Condition::After(loc, _, _)
        | Condition::AfterTicks(loc, _, _)
        // Именная форма - та же выдержка, то же ограничение места: `cond X = after
        // DWELL;` обязан отвергаться `SE-068`, как и литеральная.
        | Condition::AfterExpr(loc, _) => Some(*loc),
        Condition::Parenthesis(_, inner)
        | Condition::Not(_, inner)
        | Condition::BitAccess(_, inner, _)
        | Condition::ArraySubscript(_, _, inner) => find_after(inner),
        Condition::Add(_, l, r)
        | Condition::Subtract(_, l, r)
        | Condition::And(_, l, r)
        | Condition::Or(_, l, r)
        | Condition::Less(_, l, r)
        | Condition::More(_, l, r)
        | Condition::LessEqual(_, l, r)
        | Condition::MoreEqual(_, l, r)
        | Condition::Equal(_, l, r)
        | Condition::NotEqual(_, l, r) => find_after(l).or_else(|| find_after(r)),
        Condition::Function(_, _, args) => args.iter().find_map(find_after),
        Condition::Duration(_, _, _)
        | Condition::Number(_, _)
        | Condition::Rational(_, _, _)
        | Condition::String(_)
        | Condition::Bool(_, _)
        // Анонимное обращение к ячейке выдержки не содержит: адрес - литерал, вложенных
        // условий у него нет.
        | Condition::AnonAddress(_, _, _)
        | Condition::Variable(_) => None,
    }
}

/// Использует ли модель длительностную выдержку (`after 3s`, `after 500ms`).
///
/// В профиле "часы" такая выдержка меряется меткой времени `now_ms`, а не счётчиком
/// тактов - отсюда нужен отдельный от `after Nt` предикат: поля структуры у профилей
/// разные.
pub fn model_uses_duration_after(model: &ModelNode) -> bool {
    model_uses_after_kind(model, false)
}

/// Использует ли модель тактовую выдержку (`after 3t`) - считается `takt_dwell` в любом
/// профиле (такт - шаг логики, частота не нужна).
pub fn model_uses_tick_after(model: &ModelNode) -> bool {
    model_uses_after_kind(model, true)
}

/// Использует ли **дерево** модели (сама + вложенные) длительностную выдержку.
///
/// Колбэк `now_ms` профиля "часы" живёт на корневой структуре, а длительностная
/// выдержка бывает во вложенной под-модели композиции - поэтому решение "нужен ли
/// `now_ms` корню" принимается по всему дереву, а не по корню.
pub fn model_tree_uses_duration_after(model: &ModelNode) -> bool {
    model_uses_duration_after(model)
        || model
            .models
            .values()
            .any(|nested| model_tree_uses_duration_after(&nested.borrow()))
}

/// Использует ли модель периодический блок `every` хотя бы в одном состоянии. Период
/// `every` - всегда длительность, поэтому в профиле "часы" он меряется меткой `now_ms`
/// (как длительностный `after`).
pub fn model_uses_every(model: &ModelNode) -> bool {
    model.states.values().any(|state| {
        state
            .named_blocks()
            .iter()
            .any(|block| block.every_period().is_some())
    })
}

/// Использует ли **дерево** модели (сама + вложенные) периодический `every`.
///
/// Как `model_tree_uses_duration_after`: колбэк/вход времени профиля "часы" живёт на
/// корне, а `every` бывает во вложенной под-модели композиции.
pub fn model_tree_uses_every(model: &ModelNode) -> bool {
    model_uses_every(model)
        || model
            .models
            .values()
            .any(|nested| model_tree_uses_every(&nested.borrow()))
}

fn model_uses_after_kind(model: &ModelNode, ticks: bool) -> bool {
    model.states.values().any(|state| {
        state
            .references()
            .iter()
            .any(|reference| cond_has_after_kind(&reference.cond, ticks))
    })
}

fn cond_has_after_kind(cond: &crate::semantic::ConditionNode, ticks: bool) -> bool {
    match cond {
        // Вычисляемая выдержка длительностная: её операнды - значения типа `duration`,
        // тактовых значений в языке нет.
        crate::semantic::ConditionNode::After(_) | crate::semantic::ConditionNode::AfterExpr(_) => {
            !ticks
        }
        crate::semantic::ConditionNode::AfterTicks(_) => ticks,
        crate::semantic::ConditionNode::Unresolved(raw) => raw_has_after_kind(raw, ticks),
        _ => false,
    }
}

/// Ищет выдержку нужного вида в сыром дереве условия (для `Unresolved`-рёбер, напр.
/// составное `(after 10s) & x`). Обход исчерпывающий, как `find_after`.
fn raw_has_after_kind(cond: &Condition, ticks: bool) -> bool {
    match cond {
        // Константа именуется типом `duration`, поэтому именная выдержка - всегда
        // длительностная: тактового литерала в выражениях нет, значит и константы в
        // тактах не существует (ограничение объёма ).
        Condition::After(..) | Condition::AfterExpr(..) => !ticks,
        Condition::AfterTicks(..) => ticks,
        Condition::Parenthesis(_, inner)
        | Condition::Not(_, inner)
        | Condition::BitAccess(_, inner, _)
        | Condition::ArraySubscript(_, _, inner) => raw_has_after_kind(inner, ticks),
        Condition::Add(_, l, r)
        | Condition::Subtract(_, l, r)
        | Condition::And(_, l, r)
        | Condition::Or(_, l, r)
        | Condition::Less(_, l, r)
        | Condition::More(_, l, r)
        | Condition::LessEqual(_, l, r)
        | Condition::MoreEqual(_, l, r)
        | Condition::Equal(_, l, r)
        | Condition::NotEqual(_, l, r) => {
            raw_has_after_kind(l, ticks) || raw_has_after_kind(r, ticks)
        }
        Condition::Function(_, _, args) => args.iter().any(|a| raw_has_after_kind(a, ticks)),
        Condition::Duration(_, _, _)
        | Condition::Number(_, _)
        | Condition::Rational(_, _, _)
        | Condition::String(_)
        | Condition::Bool(_, _)
        | Condition::AnonAddress(_, _, _)
        | Condition::Variable(_) => false,
    }
}

/// Рекурсивный обход АСД: объявления `clock` текущей модели и вложенных.
fn walk(ast: &Model, found: &mut Option<u64>) -> Result<(), Diagnostic> {
    for element in &ast.elements {
        match element {
            ModelElement::Clock(def) => match *found {
                Some(previous) if previous != def.hertz => {
                    return Err(Diagnostic::error(
                        def.loc,
                        msg!(
                            keys::SE_067_CLOCK_DECLARED_TWICE,
                            previous = previous,
                            hertz = def.hertz
                        ),
                    )
                    .with_code("SE-067"));
                }
                _ => *found = Some(def.hertz),
            },
            // `after` вне ребра - ошибка (см. шапку модуля).
            ModelElement::Condition(def) => {
                reject_after(&def.value, &msg!(keys::AFTER_PLACE_NAMED_CONDITION))?
            }
            ModelElement::Invariant(def) => {
                reject_after(&def.value, &msg!(keys::AFTER_PLACE_INVARIANT))?
            }
            ModelElement::InlineFormula(def) => reject_after_in_formula(def)?,
            ModelElement::Model(nested) => walk(nested, found)?,
            ModelElement::State(state) => {
                for element in &state.elements {
                    match element {
                        // `every` исполняется всеми целями и симулятором -
                        // разворачивается семантикой в блок
                        // `NamedCodeBlockDefinitionNode::Every`.
                        StateElement::Every(_) => {}
                        // Условие ребра - **единственное** законное место `after`.
                        StateElement::Reference(_, _, _) => {}
                        StateElement::Invariant(def) => {
                            reject_after(&def.value, &msg!(keys::AFTER_PLACE_STATE_INVARIANT))?;
                        }
                        StateElement::InlineFormula(def) => reject_after_in_formula(def)?,
                        // Вставка уровня состояния - тело, а не условие: `after` в нём
                        // судится общим правилом тел.
                        StateElement::Assembly(_)
                        // Формула - обязательство внешнему анализатору, компилятор её
                        // не переводит.
                        | StateElement::Formula(_)
                        | StateElement::Next(_)
                        | StateElement::NamedBlockCode(_)
                        | StateElement::StraySemicolon(_) => {}
                    }
                }
            }
            _ => {}
        }
    }
    Ok(())
}
