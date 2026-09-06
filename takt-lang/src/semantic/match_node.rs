//! Узлы оператора `match` - ветка и образец.
//!
//! Вынесены из `semantic/mod.rs` по границе ответственности (правило размера модуля):
//! там объявлены дерево и его корень, здесь - две самостоятельные структуры, которыми
//! пользуются понижение, обходы и все восемь печатников.

use crate::diagnostics::Location;
use crate::semantic::{ExpressionNode, StatementNode};

/// Семантическая ветка оператора `match`.
///
/// Позиция ветви нужна диагностике `SE-131`: она указывает на недостижимую ветвь, а без
/// координаты сообщение о повторе образца пришлось бы читать глазами по всему файлу. В
/// АСД позиция была всегда - терялась она при понижении.
#[derive(Default, Debug, Clone)]
pub struct MatchArmNode {
    /// Образцы (хотя бы один).
    pub patterns: Vec<MatchPatternNode>,
    /// Тело ветки.
    pub body: Box<StatementNode>,
    /// Место ветви в исходнике.
    ///
    /// В равенство узлов **не входит** (как у `Extend`): две одинаковые по смыслу ветви
    /// в разных местах остаются равными.
    pub loc: Location,
}

impl PartialEq for MatchArmNode {
    fn eq(&self, other: &Self) -> bool {
        self.patterns == other.patterns && self.body == other.body
    }
}

impl Eq for MatchArmNode {}

/// Семантический образец ветки `match`.
#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub enum MatchPatternNode {
    /// Конкретное значение.
    Value(Box<ExpressionNode>),
    /// Подстановочный образец `_`.
    #[default]
    Wildcard,
}
