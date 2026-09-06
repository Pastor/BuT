//! Канон именования: предупреждение `CS-001`.
//!
//! Канон описан разделом "Стиль кода" документа и принят:
//!
//! | Что | Форма |
//! |---|---|
//! | модели, состояния, типы, перечисления, структуры, варианты | `UpperCamelCase` |
//! | условия (`cond`), инварианты (`invariant`) | `UpperCamelCase` |
//! | переменные, порты, функции, параметры | `snake_case` |
//! | константы | `UPPER_SNAKE_CASE` |

use crate::diagnostics::{Diagnostic, Location};
use crate::parser::ast;

/// Требуемая форма имени.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Case {
    /// `Heater`, `Idle`, `Command`.
    UpperCamel,
    /// `target_floor`, `read_sensor`.
    Snake,
    /// `MAX_LEVEL`, `LIMIT`.
    UpperSnake,
}

impl Case {
    /// Как форма называется в тексте предупреждения.
    fn title(self) -> &'static str {
        match self {
            Case::UpperCamel => "UpperCamelCase",
            Case::Snake => "snake_case",
            Case::UpperSnake => "UPPER_SNAKE_CASE",
        }
    }

    /// Соответствует ли имя форме.
    ///
    /// Регистр берётся у Unicode, а не у ASCII: язык допускает кириллицу, и `Счёт` для
    /// переменной - нарушение так же, как `Count`.
    ///
    /// Имя **без букв с регистром** (только цифры и `_`) считается соответствующим:
    /// правило о регистре к нему неприменимо, а выдумывать для него отдельное значило
    /// бы спорить с автором о том, чего канон не описывает.
    fn matches(self, name: &str) -> bool {
        if name.is_empty() {
            return true;
        }
        let has_cased = name.chars().any(|c| c.is_lowercase() || c.is_uppercase());
        if !has_cased {
            return true;
        }
        match self {
            // Первая буква заглавная, подчёркиваний нет.
            Case::UpperCamel => {
                !name.contains('_')
                    && name
                        .chars()
                        .next()
                        .is_some_and(|c| c.is_uppercase() || !c.is_alphabetic())
                    && name.chars().next().is_some_and(char::is_uppercase)
            }
            // Строчные буквы, цифры и подчёркивания.
            Case::Snake => !name.chars().any(char::is_uppercase),
            // Заглавные буквы, цифры и подчёркивания.
            Case::UpperSnake => !name.chars().any(char::is_lowercase),
        }
    }
}

/// Строит предупреждение `CS-001`.
fn warn(loc: Location, kind: &str, name: &str, case: Case) -> Diagnostic {
    Diagnostic::warning(
        loc,
        format!(
            "{kind} '{name}' не соответствует канону именования: ожидается {}",
            case.title()
        ),
    )
    .with_code("CS-001")
}

/// Проверяет имя, если оно разобрано.
///
/// `IdentifierOrError` кладёт `None`, когда имя не разобралось (набор текста в
/// редакторе). Тогда проверять нечего - молчим, а не паникуем.
fn check(name: &Option<ast::Identifier>, kind: &str, case: Case, out: &mut Vec<Diagnostic>) {
    if let Some(id) = name
        && !case.matches(&id.name)
    {
        out.push(warn(id.loc, kind, &id.name, case));
    }
}

/// Собирает предупреждения о неканоничных именах во всей единице компиляции.
///
/// Обходит АСД рекурсивно, включая вложенные модели.
pub fn naming_warnings(model: &ast::Model) -> Vec<Diagnostic> {
    let mut out = Vec::new();
    collect_model(model, &mut out);
    out
}

fn collect_model(model: &ast::Model, out: &mut Vec<Diagnostic>) {
    check(&model.name, "модель", Case::UpperCamel, out);
    for element in &model.elements {
        collect_element(element, out);
    }
}

fn collect_element(element: &ast::ModelElement, out: &mut Vec<Diagnostic>) {
    match element {
        ast::ModelElement::Model(m) => collect_model(m, out),
        ast::ModelElement::State(s) => {
            check(&s.name, "состояние", Case::UpperCamel, out);
            // Тело состояния тоже объявляет имена: `invariant` живёт и здесь
            // (`StateElement::Invariant`). Без спуска канон действовал бы через раз -
            // правило, работающее наполовину, хуже отсутствующего: его перестают
            // читать.
            for element in &s.elements {
                collect_state_element(element, out);
            }
        }
        ast::ModelElement::Variable(v) => collect_variable(v, out),
        ast::ModelElement::Function(f) => {
            check(&f.name, "функция", Case::Snake, out);
            for (_, param) in &f.params {
                if let Some(param) = param {
                    check(&param.name, "параметр", Case::Snake, out);
                }
            }
        }
        ast::ModelElement::Type(t) => check(&Some(t.name.clone()), "тип", Case::UpperCamel, out),
        ast::ModelElement::Enum(e) => {
            check(&e.name, "перечисление", Case::UpperCamel, out);
            for v in &e.variants {
                check(&Some(v.name.clone()), "вариант", Case::UpperCamel, out);
            }
        }
        ast::ModelElement::Struct(s) => check(&s.name, "структура", Case::UpperCamel, out),
        // `cond Имя = условие;` и `invariant Имя = условие;` - именованные объявления,
        // и канон на них распространяется наравне с типами.
        ast::ModelElement::Condition(c) => check(&c.name, "условие", Case::UpperCamel, out),
        ast::ModelElement::Invariant(i) => check(&i.name, "инвариант", Case::UpperCamel, out),
        _ => {}
    }
}

/// Имена, объявляемые в теле состояния.
///
/// Сегодня оно ровно одно - `invariant`. Остальные элементы тела (`next`, `ref`,
/// именованные блоки, `every`, встроенные формулы) имён не вводят, а ссылаются на чужие -
/// переименовывать их канон не вправе.
///
/// `match` исчерпывающий, без `_`: новый вид элемента состояния обязан заставить
/// принять решение, а не молча выпасть из проверки - так молча выпадали `always` уровня
/// модели и реализация модели.
fn collect_state_element(element: &ast::StateElement, out: &mut Vec<Diagnostic>) {
    match element {
        ast::StateElement::Invariant(i) => check(&i.name, "инвариант", Case::UpperCamel, out),
        ast::StateElement::Next(_)
        | ast::StateElement::Reference(..)
        | ast::StateElement::NamedBlockCode(_)
        | ast::StateElement::StraySemicolon(_)
        | ast::StateElement::InlineFormula(_)
        // Обязательство и вставка уровня состояния имён не объявляют.
        | ast::StateElement::Formula(_)
        | ast::StateElement::Assembly(_)
        | ast::StateElement::Every(_) => {}
    }
}

fn collect_variable(define: &ast::VariableDefine, out: &mut Vec<Diagnostic>) {
    match define {
        ast::VariableDefine::Variable { name, .. } => check(name, "переменная", Case::Snake, out),
        ast::VariableDefine::Port { name, .. } => check(name, "порт", Case::Snake, out),
        ast::VariableDefine::Constant { name, .. } => {
            check(name, "константа", Case::UpperSnake, out)
        }
        ast::VariableDefine::Parameter { name, .. } => check(name, "параметр", Case::Snake, out),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn upper_camel_accepts_and_rejects() {
        assert!(Case::UpperCamel.matches("Heater"));
        assert!(Case::UpperCamel.matches("Idle2"));
        assert!(!Case::UpperCamel.matches("heater"));
        // Подчёркивание в UpperCamelCase недопустимо - на этом и попался
        // `ElevatorMotor_Up` корпуса (переименован в ).
        assert!(!Case::UpperCamel.matches("ElevatorMotor_Up"));
    }

    #[test]
    fn snake_accepts_and_rejects() {
        assert!(Case::Snake.matches("target_floor"));
        assert!(Case::Snake.matches("x2"));
        assert!(!Case::Snake.matches("targetFloor"));
        assert!(!Case::Snake.matches("MAX"));
    }

    #[test]
    fn upper_snake_accepts_and_rejects() {
        assert!(Case::UpperSnake.matches("MAX_LEVEL"));
        assert!(!Case::UpperSnake.matches("Max_Level"));
        // Одиночная строчная - нарушение; на нём попался `const c` из корпуса.
        assert!(!Case::UpperSnake.matches("c"));
    }

    /// Кириллица проверяется теми же правилами.
    #[test]
    fn cyrillic_is_checked_by_the_same_rules() {
        assert!(Case::Snake.matches("счёт"));
        assert!(!Case::Snake.matches("Счёт"));
        assert!(Case::UpperCamel.matches("Счётчик"));
        assert!(!Case::UpperCamel.matches("счётчик"));
    }

    /// Имя без букв с регистром правилу о регистре не подчиняется.
    #[test]
    fn caseless_name_is_accepted_by_every_form() {
        for case in [Case::UpperCamel, Case::Snake, Case::UpperSnake] {
            assert!(case.matches("_"));
            assert!(case.matches("_1"));
        }
    }
}
