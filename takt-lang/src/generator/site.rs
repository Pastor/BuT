//! Позиция текущего оператора при генерации - общий носитель.
//!
//! # Как
//!
//! Печатник операторов объявляет место (`enter`), печатник выражений спрашивает его
//! (`current`) - но **только** когда своей позиции у отказа нет. Носитель живёт в карте
//! цели, которая и так доходит до каждого печатника.
//!
//! **Это изменяемое состояние на время генерации**, и потому оно названо и ограничено:
//! `enter` зовёт **только** печать операторов, а `current` возвращает
//! `Location::Builtin`, пока ни один оператор не начат (генерация объявлений,
//! инициализаторов, заголовков).
//!
//! **Носитель один на все цели и потоковый.** Потоковость не украшение: тесты идут
//! параллельно, а генерация внутри потока однопоточна по построению - одна цель, один
//! проход.
//!
//! # Второй слой: объявление
//!
//! Отказ, рождённый вне операторов, координаты не имеет вовсе: печать объявлений
//! (портов, переменных, типов) идёт до всякого тела, и `current` отвечает `Builtin`.
//! Узел, о котором они говорят, свою позицию знает всегда.
//!
//! Поэтому у носителя два слоя: печатник объявлений объявляет своё место
//! ([`enter_declaration`]) и снимает его по выходе ([`leave_declaration`]), а [`at`]
//! спрашивает сперва оператор, затем объявление, и лишь потом берёт свою.
//!
//! Порядок слоёв именно такой: оператор точнее объявления. Печать тела идёт внутри
//! модели, у которой объявления уже напечатаны, а слой объявления к тому времени снят:
//! переживи он печать, отказ в теле получил бы координату последнего объявления, то есть
//! указал бы не туда.
//!
//! **Снятие обязательно**, и оно парное входу: незакрытый слой пережил бы печать
//! объявлений и достался бы телам. Отсюда и форма - `leave_declaration` зовётся сразу
//! после печати, а не "когда-нибудь".
//!
//! **Сброс обязателен на входе в генерацию** ([`reset`]): без него позиция последнего
//! оператора переживёт вызов и достанется следующей генерации в том же потоке - отказ
//! вне операторов получит координату из чужого файла. Чужая верная координата хуже
//! отсутствующей.

use crate::diagnostics::Location;
use std::cell::Cell;

thread_local! {
    /// Позиция оператора, который печатается сейчас, - одна на поток.
    static SITE: Cell<Location> = const { Cell::new(Location::Builtin) };
    /// Позиция объявления, которое печатается сейчас.
    static DECLARATION: Cell<Location> = const { Cell::new(Location::Builtin) };
}

/// Объявляет начало печати оператора. Зовёт **только** печатник операторов.
pub(crate) fn enter(loc: Location) {
    SITE.with(|site| site.set(loc));
}

/// Позиция текущего оператора; `Location::Builtin`, если оператор не начат.
pub(crate) fn current() -> Location {
    SITE.with(Cell::get)
}

/// Объявляет начало печати объявления (порта, переменной, типа).
///
/// Зовёт печатник объявлений цели; снимается [`leave_declaration`].
pub(crate) fn enter_declaration(loc: Location) {
    DECLARATION.with(|site| site.set(loc));
}

/// Снимает слой объявления. Парен [`enter_declaration`].
pub(crate) fn leave_declaration() {
    DECLARATION.with(|site| site.set(Location::Builtin));
}

/// Позиция текущего объявления; `Location::Builtin`, если оно не начато.
pub(crate) fn current_declaration() -> Location {
    DECLARATION.with(Cell::get)
}

/// Позиция для отказа цели: **оператор**, если он начат, иначе - своя.
///
/// Порядок именно такой, и это следствие замера. У выражения "своей"
/// позиции не бывает: `ExpressionNode::loc()` выводит её из **объявлений**
/// операндов, поэтому `res := mem[1:2];` в строке 10 давал координату строки
/// 1 - места, где объявлена `mem`. Такая координата не "менее точна", она
/// **указывает не туда**, и предпочитать её позиции оператора нельзя.
pub(crate) fn at(own: Location) -> Location {
    match (current(), current_declaration()) {
        (Location::Builtin, Location::Builtin) => own,
        // Оператор точнее объявления: тело печатается позже, и слой объявления к тому
        // времени снят (см. врезку о втором слое).
        (Location::Builtin, declaration) => declaration,
        (statement, _) => statement,
    }
}

/// Забывает начатый оператор. Зовётся на входе в генерацию.
///
/// Без сброса позиция пережила бы вызов: следующая генерация в том же потоке начиналась
/// бы с координаты чужого файла, и отказ вне операторов указывал бы туда - ложь
/// достовернее молчания.
pub(crate) fn reset() {
    SITE.with(|site| site.set(Location::Builtin));
    DECLARATION.with(|site| site.set(Location::Builtin));
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Тесты трогают **потоковый** носитель, поэтому каждый начинается со сброса:
    /// харнесс Rust гоняет тесты одного файла в разных потоках, но порядок внутри
    /// потока не задан, и оставленная позиция "протекла" бы в соседний тест.
    fn fresh() {
        reset();
    }

    #[test]
    fn empty_site_reports_builtin() {
        fresh();
        assert!(matches!(current(), Location::Builtin));
    }

    /// Позиция оператора вытесняет выведенную из объявления - в этом суть.
    #[test]
    fn statement_position_wins_inside_a_statement() {
        fresh();
        enter(Location::Source(0, 100, 110));
        for own in [
            Location::Codegen,
            Location::Builtin,
            // Позиция объявления операнда: выглядит настоящей и указывает не туда.
            Location::Source(0, 5, 7),
        ] {
            assert!(
                matches!(at(own), Location::Source(0, 100, 110)),
                "внутри оператора координата берётся у него, а не у {own:?}"
            );
        }
    }

    /// Вне оператора остаётся своя позиция - там она настоящая.
    #[test]
    fn outside_a_statement_own_position_remains() {
        fresh();
        assert!(matches!(
            at(Location::Source(0, 5, 7)),
            Location::Source(0, 5, 7)
        ));
        assert!(matches!(at(Location::Codegen), Location::Codegen));
    }

    /// Сброс забывает начатый оператор: иначе координата протекла бы в следующую
    /// генерацию того же потока.
    #[test]
    fn reset_forgets_the_statement() {
        fresh();
        enter(Location::Source(0, 100, 110));
        reset();
        assert!(matches!(at(Location::Codegen), Location::Codegen));
    }

    /// Слой объявления даёт координату, когда оператор не начат.
    #[test]
    fn declaration_gives_position_outside_statements() {
        reset();
        enter_declaration(Location::Source(1, 42, 45));
        assert_eq!(at(Location::Codegen), Location::Source(1, 42, 45));
        reset();
    }

    /// Оператор точнее объявления: начатый оператор побеждает.
    #[test]
    fn statement_wins_over_declaration() {
        reset();
        enter_declaration(Location::Source(1, 42, 45));
        enter(Location::Source(1, 100, 110));
        assert_eq!(at(Location::Codegen), Location::Source(1, 100, 110));
        reset();
    }

    /// Снятие слоя обязано возвращать "координаты нет".
    ///
    /// Это главный риск второго слоя: переживи он печать объявлений, отказ в теле
    /// получил бы координату последнего объявления.
    #[test]
    fn leaving_the_declaration_forgets_it() {
        reset();
        enter_declaration(Location::Source(1, 42, 45));
        leave_declaration();
        assert_eq!(at(Location::Codegen), Location::Codegen);
        reset();
    }
}
