//! Специализация моделей по аргументам инстанцирования, режим
//! `--parameters=specialize`.
//!
//! `M(Y := 200)` порождает **копию** `ModelNode` с подставленным значением; ссылка на
//! месте инстанцирования заменяется копией, и дальше по конвейеру идёт **обычная**
//! модель - стадии 2-6, проверки, цели и симулятор о специализации не знают. Копии с
//! одинаковым набором значений дедуплицируются.
//!
//! ## Имя специализации
//!
//! `{Имя}P{n}` - порядковый номер различного набора значений в порядке обхода
//! (детерминированного: `BTreeMap` моделей и состояний, слева направо по композиции).
//! Полагаться на конкретный вид имени не вправе никто, кроме проверки
//! воспроизводимости. Занятое имя - `SE-086`, а не молчаливое переименование:
//! пространство имён порождённого кода проверок не имеет, и молчание здесь стоило бы
//! коллизии в выводе.

use crate::diagnostics::{Diagnostic, Location};
use crate::semantic::extend::{Extend, ParameterArgument};
use crate::semantic::import::adopt::adopt_declaration;
use crate::semantic::{ExpressionNode, ModelNode, StateNode, VariableNode};
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::rc::Rc;

/// Ключ дедупликации: уникальное имя исходной модели + значения всех параметров в
/// объявленном порядке (незаданный - его значение по умолчанию).
type DedupKey = String;

/// Состояние прохода: дедупликация и счётчики имён.
struct Specialization {
    /// Ключ набора значений -> готовая специализация.
    by_values: BTreeMap<DedupKey, Rc<RefCell<ModelNode>>>,
    /// Число специализаций каждой исходной модели (для порядкового номера).
    counters: BTreeMap<String, usize>,
}

/// Специализирует все инстанцирования с аргументами в дереве `root`.
///
/// Вызывается из [`construct_stages`](crate::semantic::stages::construct_stages) между
/// стадиями 1 и 2 - только в режиме `--parameters=specialize`.
pub(crate) fn specialize_instantiations(root: &Rc<RefCell<ModelNode>>) -> Result<(), Diagnostic> {
    let mut ctx = Specialization {
        by_values: BTreeMap::new(),
        counters: BTreeMap::new(),
    };
    walk_model(&mut ctx, root)
}

/// Обходит модель: реализации её состояний, затем вложенные модели.
fn walk_model(ctx: &mut Specialization, model: &Rc<RefCell<ModelNode>>) -> Result<(), Diagnostic> {
    // Состояния клонируются на время обхода: замена ссылки мутирует implements.
    let mut states = model.borrow().states.clone();
    let mut changed = false;
    for state in states.values_mut() {
        if let StateNode::Implement { implements, .. } = state {
            changed |= walk_extend(ctx, implements)?;
        }
    }
    if changed {
        model.borrow_mut().states = states;
    }

    let nested: Vec<Rc<RefCell<ModelNode>>> =
        model.borrow().models.values().map(Rc::clone).collect();
    for sub in nested {
        walk_model(ctx, &sub)?;
    }
    Ok(())
}

/// Обходит реализацию; возвращает `true`, если ссылка заменена специализацией.
fn walk_extend(ctx: &mut Specialization, extend: &mut Extend) -> Result<bool, Diagnostic> {
    match extend {
        Extend::None | Extend::Unresolved(_) => Ok(false),
        Extend::Model(source, loc, args) => {
            if args.is_empty() {
                return Ok(false);
            }
            let specialized = specialize_one(ctx, source, *loc, args)?;
            *extend = Extend::Model(specialized, *loc, Vec::new());
            Ok(true)
        }
        Extend::Parentless(inner) => walk_extend(ctx, inner),
        Extend::Concatenation(items) | Extend::Parallel(items) => {
            let mut changed = false;
            for item in items {
                changed |= walk_extend(ctx, item)?;
            }
            Ok(changed)
        }
    }
}

/// Возвращает специализацию модели `source` под набор `args` (создаёт либо берёт
/// готовую по дедупликации).
fn specialize_one(
    ctx: &mut Specialization,
    source: &Rc<RefCell<ModelNode>>,
    call_loc: Location,
    args: &[ParameterArgument],
) -> Result<Rc<RefCell<ModelNode>>, Diagnostic> {
    let source_unique = unique_name(source);
    let key = dedup_key(&source_unique, source, args);
    if let Some(ready) = ctx.by_values.get(&key) {
        return Ok(Rc::clone(ready));
    }

    // Специализация модели с собственными вложенными моделями - названная граница:
    // `copy` разделил бы под-модели по `Rc`, и их владелец, а с ним уникальное имя пути,
    // разъехался бы с местом в дереве (`CC-004`). Отказ честнее.
    if !source.borrow().models.is_empty() {
        return Err(Diagnostic::error(
            call_loc,
            format!(
                "Специализация модели '{}' с вложенными моделями не поддерживается: \
                 задайте значения параметров вложенных моделей в их собственных \
                 инстанцированиях либо соберите в режиме --parameters=assign",
                source.borrow().name.clone().unwrap_or_default()
            ),
        )
        .with_code("SE-087"));
    }

    let parent = source
        .borrow()
        .upper
        .as_ref()
        .and_then(|weak| weak.upgrade())
        .ok_or_else(|| {
            Diagnostic::error(
                call_loc,
                "Специализируемая модель не имеет родителя в дереве".to_string(),
            )
            .with_code("SE-086")
        })?;

    // Порядковый номер различного набора - имя специализации.
    let counter = ctx.counters.entry(source_unique).or_insert(0);
    *counter += 1;
    let base = source.borrow().name.clone().unwrap_or_default();
    let new_name = format!("{}P{}", base, counter);
    if parent.borrow().models.contains_key(&new_name) {
        return Err(Diagnostic::error(
            call_loc,
            format!(
                "Имя специализации '{new_name}' уже занято моделью — переименуйте её: \
                 пространство имён кодогена общее, и молчаливое переименование \
                 специализации скрыло бы коллизию в выводе"
            ),
        )
        .with_code("SE-086"));
    }

    // Копия под новым именем, в карте моделей родителя исходной.
    let copy = Rc::new(RefCell::new(
        source
            .borrow()
            .copy(Some(new_name.clone()), Some(Rc::clone(&parent))),
    ));
    // `copy` клонирует переменные вместе с их `upper` - Weak на исходную модель.
    // Перепривязка обязательна: по `upper` строится доступ в генераторах. Приём -
    // adopt_declaration, имя остаётся своим.
    {
        let mut model = copy.borrow_mut();
        let mut rebound = BTreeMap::new();
        for (name, mut var) in std::mem::take(&mut model.variables) {
            adopt_declaration(&mut var, &copy, &name);
            rebound.insert(name, var);
        }
        model.variables = rebound;
    }
    adopt_own_nodes(&copy);
    // Тела копии. У модели, объявленной в этом файле, тел ещё нет - их разрешат стадии
    // 2-6 уже на переменные копии. У модели, пришедшей импортом, они разрешены (весь
    // конвейер прошёл внутри стадии 0 импортёра), и ячейки-снимки в них указывают на
    // исходную модель: без перепривязки цель `c` печатает доступ к чужому полю. Обход
    // безвреден и в первом случае - перепривязывать там нечего.
    crate::semantic::import::adopt::adopt_specialized_copy(&copy, source);
    // Значения аргументов - инициализаторами параметров копии. Узел уже понижен
    // (`const_eval` и `construct_expression`): стадия 2 разрешать его не будет, а вывод
    // типов возьмёт тип из аннотации.
    for arg in args {
        set_initializer(&copy, &arg.name, arg.value.clone(), arg.loc)?;
    }
    parent
        .borrow_mut()
        .models
        .insert(new_name, Rc::clone(&copy));
    ctx.by_values.insert(key, Rc::clone(&copy));
    Ok(copy)
}

/// Перепривязывает к копии её **собственные** узлы, склонированные вместе с `upper` на
/// исходную модель.
///
/// **Без этого специализация неверна молча.** Условия рёбер `ref` разрешаются стадией 6
/// в области видимости, взятой **из узла состояния**
/// (`reference.rs::resolve_references` -> `state.upper()`), а `copy` клонирует
/// состояния как есть. Значит `ref Done: after dwell;` копии разрешался бы в исходной
/// модели и брал **её** значение параметра - то есть значение по умолчанию вместо
/// аргумента (обнаружено: под `specialize` выдержка выходила 100 мс при `dwell :=
/// 200ms`). До дефект был незаметен: параметр в обеих моделях был полем с одним именем,
/// и доступ "случайно" указывал на верное поле своего экземпляра.
///
/// Именованные блоки в перепривязке не нуждаются: стадия 4 **создаёт** их заново
/// с `upper` содержащей модели (`named_block.rs`). Вложенные модели трогать
/// **нельзя** - специализация модели с ними отвергается (`SE-087`).
fn adopt_own_nodes(copy: &Rc<RefCell<ModelNode>>) {
    let weak = Rc::downgrade(copy);
    let mut model = copy.borrow_mut();
    let mut states = std::mem::take(&mut model.states);
    for state in states.values_mut() {
        match state {
            StateNode::Simple { upper, .. } | StateNode::Implement { upper, .. } => {
                *upper = Some(weak.clone());
            }
            StateNode::Unresolved => {}
        }
    }
    model.states = states;
    for cond in model.conditions.values_mut() {
        cond.upper = Some(weak.clone());
    }
}

/// Подставляет значение в инициализатор параметра копии.
fn set_initializer(
    copy: &Rc<RefCell<ModelNode>>,
    param: &str,
    value: ExpressionNode,
    loc: Location,
) -> Result<(), Diagnostic> {
    let mut model = copy.borrow_mut();
    match model.variables.get_mut(param) {
        // Формы объявления параметра две, и вторая - у импортированной модели:
        // подключаемый файл проходит весь конвейер внутри стадии 0 импортёра, а он
        // включает `constify_parameters`, - значит к моменту специализации параметр уже
        // `Const`. Приняв только `Simple`, специализация отвечала бы `SE-086` о
        // "ненайденном" параметре, который объявлен рядом.
        Some(VariableNode::Simple { expr, .. } | VariableNode::Const { expr, .. }) => {
            *expr = value;
            Ok(())
        }
        // Проверка (`SE-077...079`) гарантирует, что имя - параметр; иное здесь
        // означает регресс конвейера.
        _ => Err(Diagnostic::error(
            loc,
            format!("Параметр '{param}' не найден в специализируемой модели"),
        )
        .with_code("SE-086")),
    }
}

/// Уникальное имя модели - путь по цепочке `upper`, как у продюсера карты адресов:
/// правя одно, правь другое.
fn unique_name(model: &Rc<RefCell<ModelNode>>) -> String {
    let mut parts = vec![model.borrow().name.clone().unwrap_or_default()];
    let mut current = model.borrow().upper.as_ref().and_then(|w| w.upgrade());
    while let Some(node) = current {
        parts.push(node.borrow().name.clone().unwrap_or_default());
        current = node.borrow().upper.as_ref().and_then(|w| w.upgrade());
    }
    parts.reverse();
    parts.join(":")
}

/// Ключ дедупликации: значения **всех** параметров в объявленном порядке - заданное
/// аргументом либо значение по умолчанию из объявления (п. 6: "исходная модель +
/// значения в объявленном порядке").
fn dedup_key(
    source_unique: &str,
    source: &Rc<RefCell<ModelNode>>,
    args: &[ParameterArgument],
) -> String {
    let model = source.borrow();
    let mut parts = vec![source_unique.to_string()];
    for param in &model.parameters {
        let value = args
            .iter()
            .find(|a| a.name == param.name)
            .map(|a| format!("{:?}", a.value))
            .unwrap_or_else(|| "default".to_string());
        parts.push(format!("{}={}", param.name, value));
    }
    parts.join(";")
}
