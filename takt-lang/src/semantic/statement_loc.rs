//! Позиция оператора и равенство операторов (фича 0535, задача 03).
//!
//! # Зачем оператору позиция
//!
//! Комментарий автора модели привязан к смещению в исходнике (`format::comments`
//! сшивает его с узлом АСД по координате), а генераторы печатают из
//! **семантических** узлов. Пока у семантического оператора координаты не было,
//! перенести комментарий в вывод было не к чему привязать — позиция теряется
//! при понижении, хотя в АСД она есть всегда (урок 0471).
//!
//! # Позицию несёт не всякий оператор
//!
//! `Block`, `InlineFormula` и `Formula` своей координаты не хранят: у первого
//! её роль играет позиция первого элемента, у остальных — позиция вместилища.
//! Это названная граница, а не пропуск: поле стоило бы правки всех
//! сопоставлений, а печатать перед ними авторский комментарий (задача
//! `0535-04`) незачем — он привязан к оператору внутри тела.
//!
//! # Модуль отдельный — по требованию гейта размера
//!
//! `semantic/mod.rs` стоит сверх предела (реестр долга
//! `scripts/module-size-baseline.txt`), и расти ему нельзя: новое выносится.

use super::StatementNode;
use crate::diagnostics::Location;

/// Равенство операторов **позицию игнорирует** (урок 0056, прецедент
/// [`MatchArmNode`](super::MatchArmNode)).
///
/// Войди позиция в равенство — два одинаковых по смыслу оператора из разных
/// мест перестали бы быть равными. Сравнение узлов используют подстановка
/// функций (0444), свёртка и тесты, и расхождение пришло бы не отказом, а
/// другим выводом. Ровно этот класс закреплён для `Extend` (0056) и для ветви
/// `match` (0478).
///
/// ⚠️ Поэтому `PartialEq` написан руками, а не выведен: производный сравнивал
/// бы и `loc`. Замер задачи 0535-03: как только позиция появилась у `Continue`
/// и `Break`, производное равенство сломало тест `continue_break_resolve` —
/// узлы, отличавшиеся только координатой, перестали быть равными.
impl PartialEq for StatementNode {
    fn eq(&self, other: &Self) -> bool {
        use StatementNode as S;
        match (self, other) {
            (S::None, S::None) | (S::Continue(_), S::Continue(_)) | (S::Break(_), S::Break(_)) => {
                true
            }
            (S::Unresolved(a), S::Unresolved(b)) => a == b,
            (S::Block(a), S::Block(b)) => a == b,
            (S::Expression(a, _), S::Expression(b, _)) => a == b,
            (
                S::If {
                    cond: c1,
                    then_: t1,
                    else_: e1,
                    ..
                },
                S::If {
                    cond: c2,
                    then_: t2,
                    else_: e2,
                    ..
                },
            ) => c1 == c2 && t1 == t2 && e1 == e2,
            (
                S::Loop {
                    cond: c1, body: b1, ..
                },
                S::Loop {
                    cond: c2, body: b2, ..
                },
            ) => c1 == c2 && b1 == b2,
            (
                S::For {
                    init: i1,
                    cond: c1,
                    step: s1,
                    body: b1,
                    ..
                },
                S::For {
                    init: i2,
                    cond: c2,
                    step: s2,
                    body: b2,
                    ..
                },
            ) => i1 == i2 && c1 == c2 && s1 == s2 && b1 == b2,
            (S::Variable(n1, t1, i1, _), S::Variable(n2, t2, i2, _)) => {
                n1 == n2 && t1 == t2 && i1 == i2
            }
            (S::Return(a, _), S::Return(b, _)) => a == b,
            (S::InlineFormula(a), S::InlineFormula(b)) => a == b,
            (S::Formula(a), S::Formula(b)) => a == b,
            (
                S::Assembly {
                    target: t1,
                    body: b1,
                    ..
                },
                S::Assembly {
                    target: t2,
                    body: b2,
                    ..
                },
            ) => t1 == t2 && b1 == b2,
            (
                S::Match {
                    expr: e1, arms: a1, ..
                },
                S::Match {
                    expr: e2, arms: a2, ..
                },
            ) => e1 == e2 && a1 == a2,
            _ => false,
        }
    }
}

impl Eq for StatementNode {}

impl StatementNode {
    /// Позиция оператора в исходнике; [`Location::Codegen`] — узел синтетический.
    ///
    /// Ветвь `Block` отвечает позицией **первого элемента**: своей координаты у
    /// блока нет, а его началом читатель считает первую строку тела.
    pub fn loc(&self) -> Location {
        match self {
            Self::Expression(_, loc) => *loc,
            Self::Variable(_, _, _, loc) => *loc,
            Self::For { loc, .. } => *loc,
            Self::If { loc, .. } => *loc,
            Self::Loop { loc, .. } => *loc,
            Self::Assembly { loc, .. } => *loc,
            Self::Match { loc, .. } => *loc,
            Self::Return(_, loc) | Self::Continue(loc) | Self::Break(loc) => *loc,
            Self::Block(items) => items.first().map_or(Location::Codegen, Self::loc),
            Self::None | Self::Unresolved(_) | Self::InlineFormula(_) | Self::Formula(_) => {
                Location::Codegen
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic::ExpressionNode;

    fn ret(at: u32) -> StatementNode {
        StatementNode::Return(
            Some(Box::new(ExpressionNode::Number(1))),
            Location::Source(0, at, at + 1),
        )
    }

    /// Позиция НЕ расщепляет одинаковые операторы — главный инвариант задачи.
    ///
    /// ⚠️ Проверяются все формы, получившие координату: одной ветви мало —
    /// равенство написано руками, и забытая ветвь вернула бы производное
    /// поведение только для неё.
    #[test]
    fn positions_do_not_split_identical_statements() {
        assert_eq!(ret(10), ret(99), "return");
        assert_eq!(
            StatementNode::Break(Location::Source(0, 1, 2)),
            StatementNode::Break(Location::Codegen),
            "break"
        );
        assert_eq!(
            StatementNode::Continue(Location::Source(0, 1, 2)),
            StatementNode::Continue(Location::Codegen),
            "continue"
        );

        let if_at = |at: u32| StatementNode::If {
            cond: Box::new(ExpressionNode::Bool(true)),
            then_: Box::new(StatementNode::None),
            else_: None,
            loc: Location::Source(0, at, at + 1),
        };
        assert_eq!(if_at(5), if_at(500), "if");

        let loop_at = |at: u32| StatementNode::Loop {
            cond: None,
            body: Box::new(StatementNode::None),
            loc: Location::Source(0, at, at + 1),
        };
        assert_eq!(loop_at(5), loop_at(500), "loop");

        let match_at = |at: u32| StatementNode::Match {
            expr: Box::new(ExpressionNode::Number(0)),
            arms: Vec::new(),
            loc: Location::Source(0, at, at + 1),
        };
        assert_eq!(match_at(5), match_at(500), "match");

        let asm_at = |at: u32| StatementNode::Assembly {
            target: None,
            body: Box::new(StatementNode::None),
            loc: Location::Source(0, at, at + 1),
        };
        assert_eq!(asm_at(5), asm_at(500), "assembly");
    }

    /// Разные по смыслу операторы остаются разными — контроль к предыдущему.
    ///
    /// Без него тест доказывал бы лишь, что равенство всегда истинно.
    #[test]
    fn different_statements_stay_different() {
        assert_ne!(ret(10), StatementNode::Return(None, Location::Codegen));
        assert_ne!(
            StatementNode::Break(Location::Codegen),
            StatementNode::Continue(Location::Codegen)
        );
    }

    /// Позиция доезжает до потребителя — иначе перенос комментария не к чему
    /// привязать (задача `0535-04`).
    #[test]
    fn statement_reports_its_position() {
        assert_eq!(ret(42).loc(), Location::Source(0, 42, 43));
    }

    /// Блок отвечает позицией первого элемента, пустой — `Codegen`.
    #[test]
    fn block_reports_position_of_its_first_item() {
        let block = StatementNode::Block(vec![ret(7), ret(9)]);
        assert_eq!(block.loc(), Location::Source(0, 7, 8));
        assert_eq!(StatementNode::Block(Vec::new()).loc(), Location::Codegen);
    }
}
