//! Семантическое представление формул языка Takt.
//!
//! Содержит тип [`Formula`] и функцию преобразования [`condition_to_formula`], которая
//! переводит [`ConditionNode`] в [`Formula`].

/// Общий обход мест объявления формулы: шесть мест, два потребителя - диагностики LTL и
/// проверка охранных формул.
///
/// Подмодуль, а не сосед по `semantic/`: `semantic/mod.rs` пришпилен реестром размеров
/// и расти не имеет права (тот же довод, что у `condition::after_const`), а предмет
/// обхода - где живут формулы, то есть законное место рядом с самим типом [`Formula`].
pub(crate) mod sites;

use crate::diagnostics::Location;
use crate::parser::ast::LtlExpr;
use crate::semantic::ConditionNode;
use crate::verification::ltl::Ltl;
use std::rc::Rc;

/// Семантическое представление формулы состояния в языке Takt.
///
/// Используется для аннотации состояний и переходов в семантическом дереве.
#[derive(Debug, Clone)]
pub enum Formula {
    /// Формула отсутствует (нет аннотации).
    None,
    /// Последовательность формул, разделённые запятой.
    Formulas(Vec<Formula>),
    /// LTL-формула из синтаксического дерева и её позиция.
    ///
    /// Координата была в АСД с самого начала (`ast::Formula::Ltl { loc, ... }`), а при
    /// понижении терялась: цели `rust` и `st` печатали предупреждение "темпоральная
    /// формула в теле не транслируется" без места, и автор искал формулу глазами.
    /// Охранная форма (`Guard`) позицию несла всегда - это и было расхождением между
    /// двумя видами одной сущности.
    LTL(Ltl, Location),
    /// Охранное условие перехода (`assert` языка Takt).
    ///
    /// Второе поле - **имя инварианта**, если формула получена десахаризацией
    /// `invariant Имя = C;`. Для анонимной формы `: [Guard] c;` - `None`. Имя несёт
    /// диагностику симулятора (SIM-025) и на эмиссию C **не влияет** - генератор его
    /// игнорирует (регресс C = 0).
    ///
    /// Третье поле - **позиция самой формулы**. Прежде её не было,
    /// и реестр мест (`formula::sites`) клал в диагностику позицию
    /// **вместилища**: два `invariant` в одном файле давали два предупреждения
    /// `ST-022` с одинаковой координатой `1:1`, и автор не знал, о какой из
    /// формул речь.
    Guard(ConditionNode, Option<String>, Location),
}

/// Кладёт темпоральные формулы АСД в список семантических.
///
/// Носитель один: позиция берётся у объявления, и повторять это в трёх местах
/// построения дерева незачем.
pub fn push_ltl(target: &mut Vec<Formula>, formulas: &[LtlExpr], loc: Location) {
    for f in formulas {
        target.push(Formula::LTL(ltl_ast_to_semantic(f), loc));
    }
}

/// Позиция первой формулы списка - для диагностики о теле блока.
///
/// `None` - список пуст либо ни одна формула позиции не несёт. Носитель один: цели
/// `rust` и `st` печатают об одном и том же, и вторая формула выбора разошлась бы с
/// первой молча.
pub fn first_location(formulas: &[Formula]) -> Option<Location> {
    formulas.iter().find_map(|f| match f {
        Formula::LTL(_, loc) => Some(*loc),
        Formula::Guard(_, _, loc) => Some(*loc),
        Formula::Formulas(inner) => first_location(inner),
        Formula::None => None,
    })
}

impl PartialEq for Formula {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // Позиция в равенство не входит - тот же довод, что у охранной формы ниже и
            // у `PartialEq for Extend`.
            (Self::LTL(a, _), Self::LTL(b, _)) => a == b,
            (Self::Formulas(a), Self::Formulas(b)) => a == b,
            // Имя и позиция - метаданные диагностики; равенство определяется условием
            // (позиция исключена по тому же доводу, что и в `PartialEq for Extend`,:
            // иначе две записи одной формулы стали бы разными узлами).
            (Self::Guard(a, _, _), Self::Guard(b, _, _)) => a == b,
            _ => false,
        }
    }
}

impl Eq for Formula {}

/// Преобразует [`ConditionNode`] в [`Formula`].
///
/// [`ConditionNode::None`] отображается в [`Formula::None`]; все остальные условия - в
/// [`Formula::Guard`].
pub fn condition_to_formula(cond: &ConditionNode) -> Formula {
    condition_to_formula_at(cond, Location::Builtin)
}

/// То же с известной позицией объявления.
///
/// Охранная формула В теле блока строилась с `Location::Builtin`, хотя её объявление в
/// АСД координату несёт: диагностика о таком теле печаталась без места наравне с
/// темпоральной.
pub fn condition_to_formula_at(cond: &ConditionNode, loc: Location) -> Formula {
    match cond {
        ConditionNode::None => Formula::None,
        cond => Formula::Guard(cond.clone(), None, loc),
    }
}

/// Рекурсивно преобразует LTL-узел АСД [`LtlExpr`] в семантическое представление
/// [`Ltl`].
pub fn ltl_ast_to_semantic(expr: &LtlExpr) -> Ltl {
    match expr {
        LtlExpr::True(_) => Ltl::True,
        LtlExpr::False(_) => Ltl::False,
        LtlExpr::Atom(id) => Ltl::Atom(id.name.clone()),
        LtlExpr::Not(_, inner) => Ltl::Not(Rc::new(ltl_ast_to_semantic(inner))),
        LtlExpr::Next(_, inner) => Ltl::Next(Rc::new(ltl_ast_to_semantic(inner))),
        LtlExpr::Finally(_, inner) => Ltl::Finally(Rc::new(ltl_ast_to_semantic(inner))),
        LtlExpr::Globally(_, inner) => Ltl::Globally(Rc::new(ltl_ast_to_semantic(inner))),
        LtlExpr::And(_, l, r) => Ltl::And(
            Rc::new(ltl_ast_to_semantic(l)),
            Rc::new(ltl_ast_to_semantic(r)),
        ),
        LtlExpr::Or(_, l, r) => Ltl::Or(
            Rc::new(ltl_ast_to_semantic(l)),
            Rc::new(ltl_ast_to_semantic(r)),
        ),
        LtlExpr::Until(_, l, r) => Ltl::Until(
            Rc::new(ltl_ast_to_semantic(l)),
            Rc::new(ltl_ast_to_semantic(r)),
        ),
        LtlExpr::Release(_, l, r) => Ltl::Release(
            Rc::new(ltl_ast_to_semantic(l)),
            Rc::new(ltl_ast_to_semantic(r)),
        ),
        LtlExpr::Implies(_, l, r) => Ltl::Implies(
            Rc::new(ltl_ast_to_semantic(l)),
            Rc::new(ltl_ast_to_semantic(r)),
        ),
        LtlExpr::Parenthesis(_, inner) => ltl_ast_to_semantic(inner),
    }
}

/// Позиция встроенной формулы (`: [Guard] c;`, `: [LTL] φ;`) в исходнике.
///
/// Живёт здесь, а не в `tree.rs`: разбор варианта прямо в месте построения стоил бы
/// четырёх строк на каждый из двух вызовов, а `tree.rs` состоит в реестре узаконенного
/// долга по размеру - расти ему нельзя.
pub fn inline_formula_loc(inline: &crate::parser::ast::InlineFormulaDefine) -> Location {
    match inline {
        crate::parser::ast::InlineFormulaDefine::Guard { loc, .. }
        | crate::parser::ast::InlineFormulaDefine::Ltl { loc, .. } => *loc,
    }
}
