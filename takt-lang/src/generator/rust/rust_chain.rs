//! Последовательные композиции цели `rust` - шаги, их имена и сбор.
//!
//! Выделено из `rust_model` по границе ответственности: модель отвечает за структуру и
//! её методы, а этот модуль - за один вопрос, "какие цепочки есть в состоянии и как
//! называются их счётчики". Цепочек стало несколько на состояние (вложенные в
//! параллель), и знание о них лучше держать вместе.
//!
//! Адресация цепочки - **место в дереве** композиции, общий носитель
//! [`chain_site`](crate::generator::chain_site), тот же, что у целей `c`, `st` и `sv`.
//! Вывод при этом валиден и `clippy -D warnings` его принимает - расхождение
//! молчаливое.

use crate::diagnostics::{Diagnostic, Location};
use crate::generator::chain_site;
use crate::generator::rust::rust_map::RustMap;
use crate::generator::rust::rust_model::{Instance, collect_instances};
use crate::generator::rust::rust_name::{rust_type_name, rust_value_name};
use crate::semantic::minimap::{Element, Name, StateExtend};

/// Один шаг последовательной композиции (`A + B + (C | D) + E`).
///
/// Шаг - это либо одна под-модель, либо параллельная группа: внутри шага всё тикает
/// **одновременно**, а сами шаги идут **по очереди**.
pub(crate) struct ConcatStep {
    /// Имя варианта перечисления шага (`A0`, `Group2`).
    pub(crate) variant: String,
    /// Экземпляры, принадлежащие шагу (у параллельной группы - все её ветви).
    pub(crate) instances: Vec<Instance>,
    /// Узел шага - по нему печать спускается вглубь (шаг сам может нести параллель со
    /// своей цепочкой).
    pub(crate) node: StateExtend,
    /// Префикс полей шага - та же формула, что в [`collect_instances`].
    pub(crate) prefix: String,
}

/// Имя варианта перечисления для элемента композиции.
///
/// Правило имени одно на объявление и печать: разъедься они - порождённый код перестал
/// бы собираться (`no variant named ...`).
pub(crate) fn step_variant(step: &StateExtend, idx: usize) -> Result<String, Diagnostic> {
    Ok(match step {
        StateExtend::Model(name, _) => format!(
            "{}{}",
            rust_type_name(name.local(), Location::Codegen)?,
            idx
        ),
        _ => format!("Group{}", idx),
    })
}

/// Префикс полей элемента композиции - та же формула, что в [`collect_instances`]: поля
/// `struct` и печать такта обязаны смотреть на одни и те же имена.
pub(crate) fn step_prefix(prefix: &str, step: &StateExtend, idx: usize) -> String {
    match step {
        StateExtend::Model(name, _) => {
            format!("{}_{}{}", prefix, name.local_lowercase_snakecase(), idx)
        }
        _ => format!("{}_group{}", prefix, idx),
    }
}

/// Разбирает последовательную композицию на шаги.
///
/// Префикса полей строятся **той же** формулой, что и в [`collect_instances`], через
/// общий [`step_prefix`].
///
/// # Ошибки
/// [`RS-021`] на вложенной последовательной композиции внутри шага. Цель `c`
/// такой случай **молча пропускает** (`_ => {}` в `generate_concat_tick`), то
/// есть шаг просто не тикает и автомат встаёт. Повторять это нельзя: тихо
/// вставший автомат - ровно тот дефект, ради отсутствия которого заведена цель.
pub(crate) fn concat_steps(
    steps: &[StateExtend],
    prefix: &str,
    state: &Name,
) -> Result<Vec<ConcatStep>, Diagnostic> {
    let mut out = Vec::new();
    for (idx, step) in steps.iter().enumerate() {
        if matches!(step, StateExtend::None) {
            continue;
        }
        if matches!(step, StateExtend::Concatenation(_)) {
            return Err(Diagnostic::error(
                Location::Codegen,
                format!(
                    "Состояние '{}': последовательная композиция вложена в шаг \
                     другой последовательной композиции — это не транслируется \
                     в Rust. Разнесите шаги по отдельным состояниям",
                    state.local()
                ),
            )
            .with_code("RS-021"));
        }
        let sub = step_prefix(prefix, step, idx);
        let mut instances = Vec::new();
        collect_instances(step, &sub, &mut instances)?;
        if instances.is_empty() {
            continue;
        }
        out.push(ConcatStep {
            variant: step_variant(step, idx)?,
            instances,
            node: step.clone(),
            prefix: sub,
        });
    }
    Ok(out)
}

/// Хвост имени по месту цепочки в дереве: пустой путь - пустая строка.
fn path_camel(path: &[usize]) -> String {
    path.iter().map(|i| format!("C{}", i)).collect()
}

/// Имя перечисления шага последовательной композиции (`RootStartSeq`).
pub(crate) fn seq_enum_name(
    model: &Name,
    state: &Name,
    path: &[usize],
) -> Result<String, Diagnostic> {
    // Хвост отличает вложенную цепочку от цепочки состояния: внутри одного состояния их
    // может быть несколько, и различает их место в дереве, а не имя состояния.
    Ok(format!(
        "{}{}{}Seq",
        model.unique_camelcase(),
        rust_type_name(state.local(), Location::Codegen)?,
        path_camel(path)
    ))
}

/// Имя поля-счётчика шага (`start_seq`, `start_c0_seq`).
pub(crate) fn seq_field_name(state: &Name, path: &[usize]) -> Result<String, Diagnostic> {
    let base = format!(
        "{}{}_seq",
        state.local_lowercase_snakecase(),
        chain_site::suffix(path)
    );
    rust_value_name(&base, Location::Codegen)
}

/// Последовательная композиция модели: состояние, место в дереве и шаги.
///
/// `path` - путь от корня `extend` состояния (носитель [`chain_site`]): пустой у
/// цепочки самого состояния, непустой у вложенной. Цепочек в одном состоянии бывает
/// несколько, и на любой глубине.
pub(crate) struct Chain {
    pub(crate) state: Name,
    pub(crate) path: Vec<usize>,
    pub(crate) steps: Vec<ConcatStep>,
}

impl Chain {
    /// Цепочка вложена (то есть не является `extend` самого состояния).
    pub(crate) fn nested(&self) -> bool {
        !self.path.is_empty()
    }
}

/// Все последовательные композиции модели - включая вложенные, любой глубины.
///
/// Обход - тот же, что у [`chain_site::chains`]: сначала сам узел, затем его элементы
/// слева направо. Здесь он идёт по дереву **вместе с префиксом** полей, потому что
/// шагам нужны имена экземпляров.
pub(crate) fn model_concats(map: &RustMap, states: &[Name]) -> Result<Vec<Chain>, Diagnostic> {
    let mut out = Vec::new();
    for state in states {
        let Some(Element::StateExtend { extend, .. }) = map.state_at(state.clone()) else {
            continue;
        };
        let prefix = state.local_lowercase_snakecase();
        walk(&extend, state, &mut Vec::new(), &prefix, &mut out)?;
    }
    Ok(out)
}

fn walk(
    extend: &StateExtend,
    state: &Name,
    path: &mut Vec<usize>,
    prefix: &str,
    out: &mut Vec<Chain>,
) -> Result<(), Diagnostic> {
    match extend {
        StateExtend::Concatenation(items) => {
            let steps = concat_steps(items, prefix, state)?;
            if !steps.is_empty() {
                out.push(Chain {
                    state: state.clone(),
                    path: path.clone(),
                    steps,
                });
            }
            descend(items, state, path, prefix, out)
        }
        StateExtend::Parallel(items) => descend(items, state, path, prefix, out),
        StateExtend::Model(_, _) | StateExtend::None => Ok(()),
    }
}

fn descend(
    items: &[StateExtend],
    state: &Name,
    path: &mut Vec<usize>,
    prefix: &str,
    out: &mut Vec<Chain>,
) -> Result<(), Diagnostic> {
    for (idx, item) in items.iter().enumerate() {
        let sub = step_prefix(prefix, item, idx);
        path.push(idx);
        let result = walk(item, state, path, &sub, out);
        path.pop();
        result?;
    }
    Ok(())
}

/// Условия готовности узла композиции - по одному правилу на все места, где спрашивают
/// "эта ветвь закончила?".
///
/// Вложенная цепочка отвечает своим счётчиком, а не конъюнкцией `is_done()` своих
/// моделей: пока цепочка не дошла до последнего шага, её поздние шаги не запускались, и
/// их `is_done()` о ней ничего не говорит.
pub(crate) fn node_done(
    model: &Name,
    state: &Name,
    node: &StateExtend,
    path: &mut Vec<usize>,
    prefix: &str,
) -> Result<Vec<String>, Diagnostic> {
    match node {
        StateExtend::None => Ok(Vec::new()),
        StateExtend::Model(_, _) => Ok(vec![format!(
            "self.{}.is_done()",
            rust_value_name(prefix, Location::Codegen)?
        )]),
        StateExtend::Concatenation(_) => Ok(vec![format!(
            "self.{} == {}::Done",
            seq_field_name(state, path)?,
            seq_enum_name(model, state, path)?
        )]),
        StateExtend::Parallel(items) => {
            let mut out = Vec::new();
            for (idx, item) in items.iter().enumerate() {
                let sub = step_prefix(prefix, item, idx);
                path.push(idx);
                let result = node_done(model, state, item, path, &sub);
                path.pop();
                out.extend(result?);
            }
            Ok(out)
        }
    }
}
