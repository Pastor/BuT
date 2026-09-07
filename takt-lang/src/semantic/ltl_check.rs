//! Диагностики LTL-формул: предупреждения о непроверяемых при компиляции формулах и
//! неизвестных атомах.
//!
//! LTL-формула `: [LTL] φ;` разбирается на всех трёх уровнях: модель, состояние, блок
//! кода. Компиляция её не проверяет - ни одна цель генерации LTL не печатает. Чтобы ни
//! один путь LTL не заканчивался тишиной, здесь собираются предупреждения:
//!
//! - **SE-055** - на каждую LTL-формулу: разобрана и сохранена, но при компиляции
//!   не проверяется. Проверяет её `taktc verify` (управляющие свойства, атом -
//!   имя состояния), на него и указывает текст предупреждения.
//! - **SE-056** - на атом формулы, не совпадающий ни с одним именем модели
//!   (переменная/порт/константа, состояние, `cond`). Это **проверка имени**, а
//!   не связывание; связывание атома с семантикой (атом = имя состояния)
//!   делает верификатор - [`verify_model`](crate::verify_model), и он же
//!   отказывает на атоме-переменной (`Verdict::Unsupported`). Литералы
//!   `true`/`false` не проверяются.
//!
//! Функция [`ltl_warnings`] - публичный API наравне с
//! [`unused_variable_warnings`](crate::unused_variable_warnings).

use crate::diagnostics::{Diagnostic, Location};
use crate::semantic::ModelNode;
use crate::semantic::formula::sites::{FormulaLeaf, model_formula_sites};
use crate::verification::ltl::Ltl;
use std::cell::RefCell;
use std::collections::BTreeSet;
use std::rc::Rc;

/// Собирает предупреждения по LTL-формулам модели и её вложенных моделей.
pub fn ltl_warnings(model: Rc<RefCell<ModelNode>>) -> Vec<Diagnostic> {
    let mut out = Vec::new();
    collect_model(&model, &mut out);
    out
}

fn collect_model(model: &Rc<RefCell<ModelNode>>, out: &mut Vec<Diagnostic>) {
    let borrowed = model.borrow();
    // Известные имена модели: переменные/порты/константы, состояния, условия.
    let known: BTreeSet<String> = borrowed
        .variables
        .keys()
        .chain(borrowed.states.keys())
        .chain(borrowed.conditions.keys())
        .cloned()
        .collect();

    // Диагностика - на авторскую формулу (`site.formula`), а не на её десахаризацию
    // областью: SE-056 обязан говорить об атоме, который автор написал, иначе
    // предупреждение указывало бы на имя, добавленное нами.
    for site in model_ltl_formulas(&borrowed) {
        let (ltl, loc) = (site.formula, site.loc);
        out.push(
            Diagnostic::warning(
                loc,
                // Номера фич из текста убраны: автор модели о них не знает, а ссылка на
                // задачу в диагностике читается как обещание работы. Инструмент назван -
                // этого довольно.
                "LTL-формула разобрана и сохранена, но при компиляции не проверяется: \
                 свойства проверяет `taktc verify` — он умеет и управляющие свойства, и \
                 предикаты над данными"
                    .to_string(),
            )
            .with_code("SE-055"),
        );
        let mut atoms = BTreeSet::new();
        collect_atoms(&ltl, &mut atoms);
        for atom in atoms {
            if !known.contains(&atom) {
                out.push(
                    Diagnostic::warning(
                        loc,
                        format!(
                            "атом '{}' LTL-формулы не соответствует ни одной переменной, \
                             состоянию или именованному условию модели",
                            atom
                        ),
                    )
                    .with_code("SE-056"),
                );
            }
        }
    }

    let nested: Vec<Rc<RefCell<ModelNode>>> = borrowed.models.values().map(Rc::clone).collect();
    drop(borrowed);
    for nested_model in nested {
        collect_model(&nested_model, out);
    }
}

/// Место объявления LTL-формулы: сама формула, её позиция и **область**.
///
/// Область нужна потому, что формула, объявленная в теле состояния, говорит о прогонах
/// **из этого состояния**, а не от старта. Десахаризацию делает потребитель-верификатор
/// ([`verify_all`](crate::verify_all)), а не этот сборщик: диагностикам (SE-055/SE-056)
/// нужна **авторская** формула - иначе предупреждение указывало бы на атом, которого
/// автор не писал.
#[derive(Debug, Clone)]
pub struct LtlSite {
    /// Формула, как её написал автор (без десахаризации области).
    pub formula: Ltl,
    /// Позиция объявления в исходном тексте.
    pub loc: Location,
    /// Имя состояния, в теле которого объявлена формула; `None` - уровень модели (тело
    /// модели, её именованные блоки, тела функций).
    pub state: Option<String>,
}

/// Собирает LTL-формулы, объявленные **непосредственно** в этой модели, вместе с их
/// позициями и областями: уровень модели, состояний, именованных блоков и тел функций.
///
/// Вложенные модели **не обходятся**: их формулы говорят о состояниях своей модели и
/// проверяются против её же графа.
///
/// Обход здесь **не написан**: это фильтр над общим сбором сайтов
/// ([`model_formula_sites`]). Список мест объявления формулы живёт в одном месте -
/// [`formula::sites`](crate::semantic::formula::sites), - потому что второй его
/// экземпляр разъехался бы с первым при появлении нового места. Потребителей у сбора
/// два: эти диагностики вместе с верификацией ([`verify_all`](crate::verify_all)) и
/// проверка охранных формул в `validate`.
pub(crate) fn model_ltl_formulas(model: &ModelNode) -> Vec<LtlSite> {
    model_formula_sites(model)
        .into_iter()
        .filter_map(|site| match site.formula {
            FormulaLeaf::Ltl(formula) => Some(LtlSite {
                formula,
                loc: site.loc,
                state: site.state,
            }),
            // Охранные формулы - предмет `validate` (SE-025 и прочие проверки условия),
            // а не этих предупреждений: режим строгости у них разный, и это решение, а
            // не пропуск.
            FormulaLeaf::Guard(_) => None,
        })
        .collect()
}

/// Собирает имена атомов LTL-формулы (без `true`/`false`).
///
/// Общий источник истины: используется проверкой имён (SE-056) и связыванием атомов при
/// верификации).
pub(crate) fn collect_atoms(ltl: &Ltl, out: &mut BTreeSet<String>) {
    match ltl {
        Ltl::Atom(name) => {
            out.insert(name.clone());
        }
        Ltl::True | Ltl::False => {}
        Ltl::Not(a) | Ltl::Next(a) | Ltl::Finally(a) | Ltl::Globally(a) => collect_atoms(a, out),
        Ltl::And(l, r)
        | Ltl::Or(l, r)
        | Ltl::Implies(l, r)
        | Ltl::Until(l, r)
        | Ltl::Release(l, r) => {
            collect_atoms(l, out);
            collect_atoms(r, out);
        }
    }
}
