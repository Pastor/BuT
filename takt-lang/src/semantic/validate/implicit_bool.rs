//! Неявная булевость условий перехода (Ce11 / SE-037).
//!
//! Часть модуля `validate`.
//!
//! # Правило булевости - одно
//!
//! И не может: условие, которое не разрешилось, - это ошибка `SE-025`, а предупреждения
//! считаются только когда ошибок нет (так устроены оба потребителя - `taktc compile` и
//! языковой сервер).
//!
//! Цена второго правила измерена: правила разошлись на `after` (АСД знало, что выдержка
//! булева, семантическое - нет), и проверка стала неотдаваемой - 16 ложных срабатываний
//! на примерах документа. Теперь расходиться нечему.

use super::*;

/// Добавляет предупреждение Се11 в `out`.
///
/// Выносит форматирование сообщения в отдельную функцию, чтобы не дублировать его.
/// `loc` - координаты перехода в исходном файле (берётся из
/// [`ReferenceNode::location`]).
#[inline]
fn emit_implicit_bool_warning(
    loc: Location,
    prefix: &str,
    target_name: &str,
    summary: &str,
    is_next: bool,
    out: &mut Vec<Diagnostic>,
) {
    let verb = if is_next { "next к" } else { "к" };
    out.push(
        Diagnostic::warning(
            loc,
            format!(
                "{}: условие перехода {} '{}' содержит {} — \
                 рекомендуется явное сравнение (например, '!= 0')",
                prefix, verb, target_name, summary
            ),
        )
        .with_code("SE-037"),
    );
}

/// Проверяет, является ли разрешённое семантическое условие гарантированно булевым.
///
/// Применяется для условий на рёбрах `ref`, разрешённых на этапе 6 конвейера.
///
/// ## Правила классификации
///
/// | Условие | Результат |
/// |--------------------------------------------------|------------|
/// | Безусловный переход (`None`) | булево |
/// | Булев литерал (`true`, `false`) | булево |
/// | Операции сравнения (`=`, `!=`, `<`, `>`, ...) | булево |
/// | Логическое не (`!x`) | булево |
/// | Скобки (`(...)`) | рекурсия |
/// | Вызов функции - тип возврата неизвестен | булево |
/// | Переменная типа `bool` или `bit` | булево |
/// | Переменная числового типа (`[bit;N]`) | числовое |
/// | Числовой / вещественный / строковый литерал | числовое |
/// | Арифметика (`+`, `-`) | числовое |
/// | Побитовые операции (`&`, `\|`) | числовое |
/// | Элемент массива (`arr[n]`) | числовое |
/// | Доступ к битовому полю (`.n`) | числовое |
fn is_boolean_semantic_condition(cond: &ConditionNode) -> bool {
    match cond {
        ConditionNode::None => true,
        ConditionNode::Bool(_) => true,
        // Неразрешённое условие - **молчим**. Ветвь недостижима через обоих
        // потребителей: неразрешённое условие есть ошибка `SE-025`, а предупреждения
        // считаются только при её отсутствии (замер: 433 проверки на корпусе, исправлениетурах
        // и примерах документа - ни одной `Unresolved`, включая пробу на `S(Модель) =
        // Состояние`). Решение молчать записано здесь, а не в вызывающем: судить о
        // булевости того, чего семантика не поняла, значит гадать.
        ConditionNode::Unresolved(_) => true,
        ConditionNode::Equal(_, _)
        | ConditionNode::NotEqual(_, _)
        | ConditionNode::Less(_, _)
        | ConditionNode::More(_, _)
        | ConditionNode::LessEqual(_, _)
        | ConditionNode::MoreEqual(_, _) => true,
        ConditionNode::Not(_) => true,
        ConditionNode::Parenthesis(inner) => is_boolean_semantic_condition(inner),
        // Тип возврата функции неизвестен - не предупреждаем
        ConditionNode::Function(_, _, _) => true,
        ConditionNode::Variable(v, _) => {
            let borrowed = v.borrow();
            match &*borrowed {
                VariableNode::Simple { ty, .. }
                | VariableNode::Port { ty, .. }
                | VariableNode::Const { ty, .. } => matches!(ty, TypeNode::Bool | TypeNode::Bit),
                VariableNode::Unresolved => true, // тип неизвестен - не предупреждаем
            }
        }
        // Ниже - четыре ветви, которых семантическому предикату не хватало. Их
        // отсутствие делало проверку неотдаваемой: она горела на 51 законной записи
        // корпуса, документа и исправлениетур. АСД-предикат про `after` знал всегда -
        // разошлись два предиката одного правила.
        //
        // `&`/`|` над булевыми операндами: логических `&&`/`||` условная грамматика не
        // принимает, это единственная форма конъюнкции.
        ConditionNode::And(l, r) | ConditionNode::Or(l, r) => {
            is_boolean_semantic_condition(l) && is_boolean_semantic_condition(r)
        }
        // Выдержка - "истекла ли": булево по смыслу.
        ConditionNode::After(_) | ConditionNode::AfterTicks(_) => true,
        ConditionNode::AfterExpr(_) => true,
        // Доступ к одному биту (`x.3`) - булев сигнал.
        ConditionNode::BitAccess(_, _) => true,
        _ => false,
    }
}

/// Возвращает краткое описание небулевого разрешённого семантического условия.
///
/// Вызывается только когда [`is_boolean_semantic_condition`] вернул `false`, поэтому
/// покрывает только "числовые" ветви.
fn semantic_condition_summary(cond: &ConditionNode) -> String {
    match cond {
        ConditionNode::Number(n) => format!("числовой литерал {}", n),
        ConditionNode::Rational(s, neg) => {
            format!("вещественный литерал {}{}", if *neg { "-" } else { "" }, s)
        }
        ConditionNode::String(_) => "строковый литерал".to_string(),
        ConditionNode::Variable(v, _) => {
            let borrowed = v.borrow();
            let (name_str, ty) = match &*borrowed {
                VariableNode::Simple { name, ty, .. }
                | VariableNode::Port { name, ty, .. }
                | VariableNode::Const { name, ty, .. } => (name.clone(), ty.clone()),
                VariableNode::Unresolved => return "переменная (неизвестный тип)".to_string(),
            };
            format!("переменная '{}' типа {}", name_str, ty)
        }
        ConditionNode::Add(_, _) => "арифметическое сложение".to_string(),
        ConditionNode::Subtract(_, _) => "арифметическое вычитание".to_string(),
        ConditionNode::And(_, _) => "побитовое И".to_string(),
        ConditionNode::Or(_, _) => "побитовое ИЛИ".to_string(),
        // База - выражение: имя берётся у неё, когда оно есть.
        ConditionNode::ArraySubscript(base, idx) => {
            let name = match base.as_ref() {
                ConditionNode::Variable(var, _) => match &*var.borrow() {
                    VariableNode::Simple { name, .. }
                    | VariableNode::Port { name, .. }
                    | VariableNode::Const { name, .. } => name.clone(),
                    VariableNode::Unresolved => "?".to_string(),
                },
                _ => "выражение".to_string(),
            };
            let idx_str = match idx.as_ref() {
                ConditionNode::Number(n) => n.to_string(),
                _ => "expr".to_string(),
            };
            format!("элемент массива '{}[{}]'", name, idx_str)
        }
        ConditionNode::BitAccess(_, _) => "доступ к битовому полю".to_string(),
        _ => "числовое выражение".to_string(),
    }
}

/// Проверяет условие одного перехода и при необходимости добавляет предупреждение Се11.
///
/// Основной путь - условие уже разрешено на этапе 6 конвейера
/// ([`crate::semantic::tree`]). Неразрешённый вариант [`ConditionNode::Unresolved`]
/// используется только как запасной (для паттернов вида `S(Model).StateName`, которые
/// не могут быть разрешены в текущем контексте).
///
/// Описание условия для сообщения вычисляется **лениво** - только при наличии реального
/// нарушения.
fn check_one_ref(
    loc: Location,
    prefix: &str,
    target_name: &str,
    cond: &ConditionNode,
    is_next: bool,
    out: &mut Vec<Diagnostic>,
) {
    // Одно правило на все случаи: решение о булевости принимает
    // `is_boolean_semantic_condition` - включая неразрешённое условие.
    if !is_boolean_semantic_condition(cond) {
        let summary = semantic_condition_summary(cond);
        emit_implicit_bool_warning(loc, prefix, target_name, &summary, is_next, out);
    }
}

/// Рекурсивно собирает предупреждения Се11 для всех состояний модели.
///
/// Обходит все состояния текущей модели и вложенных моделей. Для каждого перехода
/// вызывает [`check_one_ref`].
fn collect_implicit_bool_warnings(model: &Rc<RefCell<ModelNode>>, out: &mut Vec<Diagnostic>) {
    let borrowed = model.borrow();
    let model_name = borrowed.name.clone().unwrap_or_default();

    // Строит преисправление диагностического сообщения вида "состояние 'S'" или "модель 'M',
    // состояние 'S'".
    let prefix_for = |state_name: &str| -> String {
        if model_name.is_empty() {
            format!("состояние '{}'", state_name)
        } else {
            format!("модель '{}', состояние '{}'", model_name, state_name)
        }
    };

    for state in borrowed.states.values() {
        match state {
            StateNode::Simple {
                name, references, ..
            } => {
                let prefix = prefix_for(name);
                for r in references {
                    check_one_ref(r.location, &prefix, &r.name, &r.cond, false, out);
                }
            }
            StateNode::Implement {
                name,
                references,
                next,
                ..
            } => {
                let prefix = prefix_for(name);
                for r in references {
                    check_one_ref(r.location, &prefix, &r.name, &r.cond, false, out);
                }
                if let Some(nr) = next {
                    check_one_ref(nr.location, &prefix, &nr.name, &nr.cond, true, out);
                }
            }
            StateNode::Unresolved => {}
        }
    }

    // Рекурсивный спуск во вложенные модели
    let nested: Vec<Rc<RefCell<ModelNode>>> = borrowed.models.values().map(Rc::clone).collect();
    drop(borrowed); // освобождаем заимствование перед рекурсией

    for nested_model in nested {
        collect_implicit_bool_warnings(&nested_model, out);
    }
}

/// Проверяет условия переходов в модели и возвращает предупреждения о неявном
/// приведении числового типа к булевому.
///
/// Предупреждение выдаётся, когда условие перехода (`ref`/`next`) содержит выражение
/// числового типа, используемое как булево без явного сравнения.
///
/// # Примеры (Takt)
///
/// ```but
/// var timer: [bit;8] = 0;
/// start Red {
///     ref Green: timer;       // Предупреждение: timer - числовой тип [bit;8]
///     ref Blue:  timer != 0;  // Нет предупреждения: явное сравнение
/// }
/// ```
///
/// # Возвращаемое значение
///
/// Вектор [`Diagnostic`] уровня `Warning` для каждого обнаруженного случая. Пустой
/// вектор означает, что числовых условий не найдено.
pub fn check_implicit_bool_conditions(model: &Rc<RefCell<ModelNode>>) -> Vec<Diagnostic> {
    let mut warnings = Vec::new();
    collect_implicit_bool_warnings(model, &mut warnings);
    warnings
}
