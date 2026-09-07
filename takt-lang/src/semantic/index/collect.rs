//! Сбор записей индекса из семантического дерева и сырого АСД.
//!
//! Свободные функции обхода: `collect_*`, `binding_name_of`, разбор
//! `S(Модель) = Состояние`. Контракт держит `mod.rs`: он реэкспортирует их
//! (`use collect::*;`).

use super::*;

/// Имя, под которым `target` связан в области видимости `scope`.
pub(super) fn binding_name_of(
    scope: &Rc<RefCell<ModelNode>>,
    target: &Rc<RefCell<ModelNode>>,
) -> String {
    let mut current = Some(Rc::clone(scope));
    while let Some(node) = current {
        let borrowed = node.borrow();
        for (key, candidate) in &borrowed.models {
            if Rc::ptr_eq(candidate, target) {
                return key.clone();
            }
        }
        current = borrowed.upper.as_ref().and_then(|w| w.upgrade());
    }
    // Синтетическая модель (`M1 + M2`) в `models` не лежит - имени у неё нет.
    target.borrow().name.clone().unwrap_or_default()
}

/// Собирает записи для ссылок на модели внутри реализации состояния.
///
/// `A + B`, `(C | D)` и прочие композиции - дерево [`Extend`], в листьях которого лежат
/// ссылки на модели вместе с позицией использования.
pub(super) fn collect_extend_entries(
    extend: &ModelExtend,
    model: &Rc<RefCell<ModelNode>>,
    entries: &mut Vec<IndexEntry>,
) {
    match extend {
        ModelExtend::Model(target, loc, _) => {
            // Позиции нет у синтетической модели композиции (`Location::Codegen`) и у
            // реализации, разрешённой не из АСД - индексировать нечего.
            if let Location::Source(_, start, end) = loc {
                entries.push(IndexEntry {
                    start: *start as usize,
                    end: *end as usize,
                    node_ref: SemanticNodeRef {
                        name: binding_name_of(model, target),
                        kind: SemanticNodeKind::ReferenceModel,
                        // Позиция использования: по ней запись находит курсор, и по ней
                        // же считается "свой ли это файл".
                        loc: *loc,
                        model: Some(model.clone()),
                    },
                });
            }
        }
        ModelExtend::Parentless(inner) => collect_extend_entries(inner, model, entries),
        ModelExtend::Concatenation(items) | ModelExtend::Parallel(items) => {
            for item in items {
                collect_extend_entries(item, model, entries);
            }
        }
        ModelExtend::None | ModelExtend::Unresolved(_) => {}
    }
}

/// Рекурсивно собирает записи из модели и всех вложенных моделей.
pub(super) fn collect_model_entries(model: &Rc<RefCell<ModelNode>>, entries: &mut Vec<IndexEntry>) {
    let borrowed = model.borrow();

    // Сама модель (только именованные; корневая анонимная модель пропускается)
    if let Some(ref name) = borrowed.name
        && let Location::Source(_, start, end) = borrowed.loc
    {
        entries.push(IndexEntry {
            start: start as usize,
            end: end as usize,
            node_ref: SemanticNodeRef {
                name: name.clone(),
                kind: SemanticNodeKind::Model,
                loc: borrowed.loc,
                model: Some(model.clone()),
            },
        });
    }

    // Переменные (var, port, const)
    for (name, var) in &borrowed.variables {
        let (loc, kind) = match var {
            VariableNode::Simple { loc, .. } => (*loc, SemanticNodeKind::Variable),
            VariableNode::Port { loc, .. } => (*loc, SemanticNodeKind::Port),
            VariableNode::Const { loc, .. } => (*loc, SemanticNodeKind::Const),
            VariableNode::Unresolved => continue,
        };
        if let Location::Source(_, start, end) = loc {
            entries.push(IndexEntry {
                start: start as usize,
                end: end as usize,
                node_ref: SemanticNodeRef {
                    name: name.clone(),
                    kind,
                    loc,
                    model: Some(model.clone()),
                },
            });
        }
    }

    // Функции (fn, extern fn; встроенные не индексируются - нет позиции в коде)
    for (name, func) in &borrowed.functions {
        let (loc, kind) = match func {
            FunctionDefinitionNode::Local { loc, .. } => (*loc, SemanticNodeKind::Function),
            FunctionDefinitionNode::External { loc, .. } => {
                (*loc, SemanticNodeKind::ExternFunction)
            }
            _ => continue,
        };
        if let Location::Source(_, start, end) = loc {
            entries.push(IndexEntry {
                start: start as usize,
                end: end as usize,
                node_ref: SemanticNodeRef {
                    name: name.clone(),
                    kind,
                    loc,
                    model: Some(model.clone()),
                },
            });
        }
    }

    // Состояния (state / start / end)
    for (name, state) in &borrowed.states {
        let loc = state.loc();
        let kind = match state {
            StateNode::Simple { kind, .. } | StateNode::Implement { kind, .. } => match kind {
                StateNodeKind::Start => SemanticNodeKind::StartState,
                StateNodeKind::End => SemanticNodeKind::EndState,
                StateNodeKind::Simple => SemanticNodeKind::State,
            },
            StateNode::Unresolved => continue,
        };
        if let Location::Source(_, start, end) = loc {
            entries.push(IndexEntry {
                start: start as usize,
                end: end as usize,
                node_ref: SemanticNodeRef {
                    name: name.clone(),
                    kind,
                    loc,
                    model: Some(model.clone()),
                },
            });
        }

        // Реализация состояния: `start Main = Helper;`. Имя `Helper` - ссылка на
        // модель, и это единственное место, откуда переход может увести в другой файл:
        // `import` связывает имя с корнем импортированного файла.
        if let StateNode::Implement { implements, .. } = state {
            collect_extend_entries(implements, model, entries);
        }

        for reference in state.references() {
            let loc = reference.location;
            if let Location::Source(_, start, end) = loc {
                entries.push(IndexEntry {
                    start: start as usize,
                    end: end as usize,
                    node_ref: SemanticNodeRef {
                        name: reference.name.clone(),
                        kind: SemanticNodeKind::Reference,
                        loc,
                        model: Some(model.clone()),
                    },
                });
            }
            // Добавляем записи для идентификаторов, встретившихся в условии перехода.
            // Для разрешённых условий позиции использования теряются при семантическом
            // понижении; функция добавляет записи только для Condition::Unresolved
            // (ситуация неудавшегося разрешения ссылки).
            collect_condition_entries(&reference.cond, model, entries);
        }
        // Именованные блоки состояния (enter, exit, always, ...)
        for nb in state.named_blocks() {
            collect_named_block_entries(nb, model, entries);
        }
    }

    // Псевдонимы типов: позиции хранятся в type_locs (отдельно от types). Перечисления
    // (enum) тоже добавляются в type_locs при построении модели, поэтому пропускаем
    // записи, для которых уже есть соответствующий EnumNode.
    for (name, loc) in &borrowed.type_locs {
        if borrowed.enums.contains_key(name.as_str()) {
            // Этот идентификатор - перечисление, оно будет добавлено в секции enums
            continue;
        }
        if let Location::Source(_, start, end) = loc {
            entries.push(IndexEntry {
                start: *start as usize,
                end: *end as usize,
                node_ref: SemanticNodeRef {
                    name: name.clone(),
                    kind: SemanticNodeKind::TypeAlias,
                    loc: *loc,
                    model: Some(model.clone()),
                },
            });
        }
    }

    // Именованные условия переходов (cond)
    for (name, cond) in &borrowed.conditions {
        if let Location::Source(_, start, end) = cond.loc {
            entries.push(IndexEntry {
                start: start as usize,
                end: end as usize,
                node_ref: SemanticNodeRef {
                    name: name.clone(),
                    kind: SemanticNodeKind::Condition,
                    loc: cond.loc,
                    model: Some(model.clone()),
                },
            });
        }
    }

    // Перечисления (enum)
    for (name, enum_node) in &borrowed.enums {
        if let Location::Source(_, start, end) = enum_node.loc {
            entries.push(IndexEntry {
                start: start as usize,
                end: end as usize,
                node_ref: SemanticNodeRef {
                    name: name.clone(),
                    kind: SemanticNodeKind::Enum,
                    loc: enum_node.loc,
                    model: Some(model.clone()),
                },
            });
        }
    }

    // Именованные блоки модели (always, enter, exit, ...)
    for nb in &borrowed.named_blocks {
        collect_named_block_entries(nb, model, entries);
    }

    // I8: индексируем локальные переменные из сырых АСД-операторов именованных блоков.
    // Сырые операторы сохраняют байтовые позиции, которые теряются при семантическом
    // разрешении (StatementNode::Variable не несёт Location).
    for (_, raw_stmt) in &borrowed.named_block_raw {
        collect_ast_statement_entries(raw_stmt, model, entries);
    }

    // Рекурсивно обходим вложенные именованные модели
    for nested in borrowed.models.values() {
        collect_model_entries(nested, entries);
    }
}

// --- Вспомогательные функции: условия переходов ------------------------------

/// Рекурсивно обходит семантическое условие перехода и собирает записи для
/// идентификаторов, позиция которых ещё сохранена в дереве.
///
/// Добавляет записи только для [`ConditionNode::Unresolved`], когда АСД-узел сохраняет
/// исходные байтовые позиции. Разрешённые варианты (`Variable`, `Function`, ...) не
/// несут позиции *использования*: в них хранится ссылка на узел *объявления*, а не на
/// место употребления, поэтому добавлять их в индекс по позиции объявления было бы
/// ошибкой.
///
/// ## Когда имеет эффект
///
/// Функция добавляет записи, только если условие не было разрешено в ходе
/// семантического анализа - например, при ссылке на несуществующий идентификатор. В
/// успешно построенной модели условия разрешены, и функция не добавляет ни одной записи
/// (но рекурсивно обходит составные условия).
///
/// ## Пример
///
/// ```text
/// // Условие разрешено -> записей нет
/// Condition::Variable(var_rc)  ->  (нет записей)
///
/// // Условие не разрешено -> запись добавляется
/// Condition::Unresolved(ast::Variable(id@"x", loc=5..6))  ->  IndexEntry("x", 5, 6)
/// ```
pub(super) fn collect_condition_entries(
    cond: &ConditionNode,
    model: &Rc<RefCell<ModelNode>>,
    entries: &mut Vec<IndexEntry>,
) {
    match cond {
        // Единственный случай, когда позиция использования сохранена - АСД-форма
        ConditionNode::Unresolved(ast_cond) => {
            collect_ast_condition_entries(ast_cond, model, entries);
        }
        // Сравнение текущего состояния модели с её состоянием: `S(Ping) = End` / `!=`.
        // Имя состояния приходит **неразрешённым** (`End` - состояние модели-аргумента
        // `Ping`, а не той, где записано условие; резолвер её не видит и не должен -
        // инвариант "`ref` не разрешается"). Поэтому оно не становится
        // `ConditionNode::State`, а остаётся `Unresolved(Variable)` и без спецразбора
        // индексировалось бы как рядовая `ReferenceCondition` - goto вёл бы в никуда.
        // Резолвим имя в области модели из `S(...)` (как C-генератор,
        // `c_expr::condition:: state_of_model`) и кладём `ReferenceState` с этой
        // моделью-контекстом.
        ConditionNode::Equal(l, r) | ConditionNode::NotEqual(l, r) => {
            if try_collect_state_of_model(l, r, model, entries)
                || try_collect_state_of_model(r, l, model, entries)
            {
                // Сторона `S(...)` уже проиндексирована внутри помощника; имя состояния -
                // как `ReferenceState`. Рядовая рекурсия не нужна: она заново дала бы
                // `ReferenceCondition` поверх имени состояния.
                return;
            }
            collect_condition_entries(l, model, entries);
            collect_condition_entries(r, model, entries);
        }
        // Прочие бинарные операторы: обходим оба операнда
        ConditionNode::And(l, r)
        | ConditionNode::Or(l, r)
        | ConditionNode::Add(l, r)
        | ConditionNode::Subtract(l, r)
        | ConditionNode::Less(l, r)
        | ConditionNode::More(l, r)
        | ConditionNode::LessEqual(l, r)
        | ConditionNode::MoreEqual(l, r) => {
            collect_condition_entries(l, model, entries);
            collect_condition_entries(r, model, entries);
        }
        // Унарные операторы
        ConditionNode::Not(c) | ConditionNode::Parenthesis(c) => {
            collect_condition_entries(c, model, entries);
        }
        ConditionNode::BitAccess(c, _) => {
            collect_condition_entries(c, model, entries);
        }
        ConditionNode::Function(func_rc, args, loc) => {
            // Позиция имени функции сохранена - добавляем запись
            if let Location::Source(_, start, end) = loc {
                let name = func_rc.borrow().name().to_string();
                entries.push(IndexEntry {
                    start: *start as usize,
                    end: *end as usize,
                    node_ref: SemanticNodeRef {
                        name,
                        kind: SemanticNodeKind::ReferenceCondition,
                        loc: *loc,
                        model: Some(model.clone()),
                    },
                });
            }
            for arg in args {
                collect_condition_entries(arg, model, entries);
            }
        }
        // Ссылка на модель в условии: `S(Helper)`. Вторая (после реализации состояния)
        // форма, способная увести переход в другой файл.
        ConditionNode::Model(target, loc) => {
            if let Location::Source(_, start, end) = loc {
                entries.push(IndexEntry {
                    start: *start as usize,
                    end: *end as usize,
                    node_ref: SemanticNodeRef {
                        name: binding_name_of(model, target),
                        kind: SemanticNodeKind::ReferenceModel,
                        loc: *loc,
                        model: Some(model.clone()),
                    },
                });
            }
        }
        // Ссылка на состояние в условии: `End` в `S(Ping) = End`. Use-site позиция
        // сохранена - добавляем запись.
        ConditionNode::State(target, loc) => {
            if let Location::Source(_, start, end) = loc {
                entries.push(IndexEntry {
                    start: *start as usize,
                    end: *end as usize,
                    node_ref: SemanticNodeRef {
                        name: target.borrow().name().to_string(),
                        kind: SemanticNodeKind::ReferenceState,
                        loc: *loc,
                        model: Some(model.clone()),
                    },
                });
            }
        }
        // Позиция использования переменной сохранена - добавляем запись
        ConditionNode::Variable(var_rc, loc) => {
            if let Location::Source(_, start, end) = loc {
                let name = var_rc.borrow().name().to_string();
                entries.push(IndexEntry {
                    start: *start as usize,
                    end: *end as usize,
                    node_ref: SemanticNodeRef {
                        name,
                        kind: SemanticNodeKind::ReferenceCondition,
                        loc: *loc,
                        model: Some(model.clone()),
                    },
                });
            }
        }
        // Прочие терминальные варианты (None, Number, Bool, Rational, ...) - не
        // индексируются
        _ => {}
    }
}

/// Рекурсивно извлекает записи [`SemanticNodeKind::ReferenceCondition`] из АСД-условия.
///
/// Находит [`ast::Condition::Variable`] и [`ast::Condition::Function`] с
/// [`Location::Source`] и добавляет `IndexEntry` для каждого.
///
/// ## Примеры
///
/// ```text
/// // Переменная в условии -> запись с именем и позицией
/// ast::Condition::Variable(id@"flag", loc=Source(0, 10, 14))
///     -> IndexEntry { start:10, end:14, name:"flag", kind:ReferenceCondition }
///
/// // Вызов функции -> запись для имени функции + рекурсивно по аргументам
/// ast::Condition::Function(_, id@"check", [Variable("x")])
///     -> IndexEntry("check"), IndexEntry("x")
/// ```
///
/// ## Контрпримеры
///
/// ```text
/// // Переменная с Builtin-позицией -> запись не добавляется
/// ast::Condition::Variable(Identifier { loc: Builtin, name: "built_in" })
///     -> (нет записей)
///
/// // Числовой литерал -> запись не добавляется
/// ast::Condition::Number(_, 42)  ->  (нет записей)
/// ```
/// Модель, о **текущем состоянии** которой говорит сторона сравнения: `S(Модель)`
/// (встроенная `S`) или краткая форма `Модель`. Зеркало
/// `c_expr::condition::state_of_model` - один разбор на обе цели (goto и C).
///
/// Резолюция уже произошла (условие ребра - `ConditionNode`, не сырой АСД), поэтому
/// `S(Ping)` приходит как `Function(Builtin("S"), [Model(Ping)])`, а не как сырой
/// идентификатор.
pub(super) fn state_of_model_cond(cond: &ConditionNode) -> Option<Rc<RefCell<ModelNode>>> {
    match cond {
        ConditionNode::Model(model, _) => Some(model.clone()),
        ConditionNode::Function(fun, args, _) => {
            if !matches!(&*fun.borrow(), FunctionDefinitionNode::Builtin("S", ..)) {
                return None;
            }
            match args.first().map(|a| a.as_ref())? {
                ConditionNode::Model(model, _) => Some(model.clone()),
                _ => None,
            }
        }
        _ => None,
    }
}

/// Имя состояния и его use-site позиция из правой части `S(...) = <имя>`.
///
/// Три формы (как у C-генератора `generate_state_comparison`): неразрешённый
/// идентификатор - штатный кросс-модельный случай (`End` в чужой области); `State` -
/// имя случайно совпало с состоянием объемлющей модели и разрешилось в её области
/// (искать всё равно в модели-аргументе); `Variable` - то же, но имя совпало с
/// переменной. Позиция берётся из use-site (не из декларации).
pub(super) fn state_name_use_site(cond: &ConditionNode) -> Option<(String, Location)> {
    match cond {
        ConditionNode::Unresolved(ast::Condition::Variable(id)) => Some((id.name.clone(), id.loc)),
        ConditionNode::State(state, loc) => Some((state.borrow().name().to_string(), *loc)),
        ConditionNode::Variable(var, loc) => Some((var.borrow().name().to_string(), *loc)),
        _ => None,
    }
}

/// Распознаёт `s_side = state_side` как `S(Модель) = Состояние`: если `s_side` - про
/// состояние модели-аргумента, а `state_side` несёт имя, объявленное **в этой модели**, -
/// кладёт `ReferenceState` (контекст поиска = модель-аргумент, не текущая) и
/// индексирует сторону `S(...)` рекурсией. Иначе ничего не делает и возвращает `false`
/// (вызывающий пойдёт обычным путём).
pub(super) fn try_collect_state_of_model(
    s_side: &ConditionNode,
    state_side: &ConditionNode,
    model: &Rc<RefCell<ModelNode>>,
    entries: &mut Vec<IndexEntry>,
) -> bool {
    let Some(target) = state_of_model_cond(s_side) else {
        return false;
    };
    let Some((name, loc)) = state_name_use_site(state_side) else {
        return false;
    };
    let Location::Source(_, start, end) = loc else {
        return false;
    };
    if target.borrow().search_state(&name).is_none() {
        return false;
    }
    entries.push(IndexEntry {
        start: start as usize,
        end: end as usize,
        node_ref: SemanticNodeRef {
            name,
            kind: SemanticNodeKind::ReferenceState,
            loc,
            model: Some(target.clone()),
        },
    });
    // Сторона `S(Модель)` индексируется как обычно (имя `S` -> ReferenceCondition, имя
    // модели -> ReferenceModel) - контекст поиска у неё **текущая** модель, где имя
    // `Модель` разрешается связыванием (`binding_name_of`).
    collect_condition_entries(s_side, model, entries);
    true
}

/// Рекурсивно извлекает записи [`SemanticNodeKind::ReferenceCondition`] из АСД-условия.
///
/// Находит [`ast::Condition::Variable`] и [`ast::Condition::Function`] с
/// [`Location::Source`] и добавляет `IndexEntry` для каждого.
pub(super) fn collect_ast_condition_entries(
    cond: &ast::Condition,
    model: &Rc<RefCell<ModelNode>>,
    entries: &mut Vec<IndexEntry>,
) {
    match cond {
        ast::Condition::Variable(id) => {
            if let Location::Source(_, start, end) = id.loc {
                entries.push(IndexEntry {
                    start: start as usize,
                    end: end as usize,
                    node_ref: SemanticNodeRef {
                        name: id.name.clone(),
                        kind: SemanticNodeKind::ReferenceCondition,
                        loc: id.loc,
                        model: Some(model.clone()),
                    },
                });
            }
        }
        ast::Condition::Function(_, id, args) => {
            if let Location::Source(_, start, end) = id.loc {
                entries.push(IndexEntry {
                    start: start as usize,
                    end: end as usize,
                    node_ref: SemanticNodeRef {
                        name: id.name.clone(),
                        kind: SemanticNodeKind::ReferenceCondition,
                        loc: id.loc,
                        model: Some(model.clone()),
                    },
                });
            }
            for arg in args {
                collect_ast_condition_entries(arg, model, entries);
            }
        }
        // База - выражение: записи собирает тот же обход, а не отдельная запись по
        // имени.
        ast::Condition::ArraySubscript(_, base, index) => {
            collect_ast_condition_entries(base, model, entries);
            collect_ast_condition_entries(index, model, entries);
        }
        // Бинарные операторы - рекурсивный обход обеих сторон.
        //
        // `Equal`/`NotEqual` со случаем `S(Ping) = End` сюда **не доходят**: условие
        // ребра резолвится в `ConditionNode::Equal` (левая часть `S(...)` - встроенная
        // функция, всегда разрешима), а неразрешённой остаётся лишь правая
        // часть-состояние. Разбор `S(Модель) = Состояние` живёт на уровне
        // `ConditionNode` (`try_collect_state_of_model`), а не сырого АСД - сюда
        // попадает только имя-лист внутри `Unresolved`, уже без контекста `S(...)`.
        ast::Condition::And(_, l, r)
        | ast::Condition::Or(_, l, r)
        | ast::Condition::Add(_, l, r)
        | ast::Condition::Subtract(_, l, r)
        | ast::Condition::Less(_, l, r)
        | ast::Condition::More(_, l, r)
        | ast::Condition::LessEqual(_, l, r)
        | ast::Condition::MoreEqual(_, l, r)
        | ast::Condition::Equal(_, l, r)
        | ast::Condition::NotEqual(_, l, r) => {
            collect_ast_condition_entries(l, model, entries);
            collect_ast_condition_entries(r, model, entries);
        }
        // Унарные операторы
        ast::Condition::Not(_, c) | ast::Condition::Parenthesis(_, c) => {
            collect_ast_condition_entries(c, model, entries);
        }
        ast::Condition::BitAccess(_, c, _) => {
            collect_ast_condition_entries(c, model, entries);
        }
        // Литералы (Number, Rational, String, Bool) - не индексируются
        _ => {}
    }
}

// --- Вспомогательные функции: именованные блоки кода -------------------------

/// Обходит тело именованного блока кода и добавляет записи для идентификаторов, чьи
/// позиции сохранились в семантическом дереве.
///
/// ## Ограничение
///
/// Семантический [`NamedCodeBlockDefinitionNode`] **не хранит позицию объявления
/// блока** (`loc`): ключевые слова `enter`, `exit`, `always`, `<custom>` не могут быть
/// найдены через индекс. Для устранения ограничения необходимо добавить поле `loc:
/// Location` в [`NamedCodeBlockDefinitionNode`].
///
/// В успешно построенных моделях тело полностью разрешено и позиции использования
/// переменных/функций не сохраняются; функция добавляет записи только для неразрешённых
/// подвыражений (`Statement::Unresolved` / `Expression::Unresolved`).
pub(super) fn collect_named_block_entries(
    nb: &NamedCodeBlockDefinitionNode,
    model: &Rc<RefCell<ModelNode>>,
    entries: &mut Vec<IndexEntry>,
) {
    let body = match nb {
        NamedCodeBlockDefinitionNode::Enter { body, .. }
        | NamedCodeBlockDefinitionNode::Exit { body, .. }
        | NamedCodeBlockDefinitionNode::Always { body, .. }
        | NamedCodeBlockDefinitionNode::Unknown { body, .. }
        | NamedCodeBlockDefinitionNode::Every { body, .. } => body,
        // None/Unresolved - тело отсутствует или ещё не прикреплено
        NamedCodeBlockDefinitionNode::None | NamedCodeBlockDefinitionNode::Unresolved(..) => return,
    };
    collect_statement_entries(body, model, entries);
}

/// Рекурсивно обходит семантический оператор, собирая записи из неразрешённых
/// подвыражений.
///
/// Для разрешённых операторов рекурсивно обходит вложенные блоки (`Block`, `If`,
/// `Loop`, `For`), чтобы добраться до возможных `Statement::Unresolved` или
/// `Expression::Unresolved` вглубь дерева.
pub(super) fn collect_statement_entries(
    stmt: &StatementNode,
    model: &Rc<RefCell<ModelNode>>,
    entries: &mut Vec<IndexEntry>,
) {
    match stmt {
        // АСД-оператор, ещё не прошедший семантическое понижение
        StatementNode::Unresolved(ast_stmt) => {
            collect_ast_statement_entries(ast_stmt, model, entries);
        }
        StatementNode::Block(stmts) => {
            for s in stmts {
                collect_statement_entries(s, model, entries);
            }
        }
        StatementNode::Expression(expr, _) => {
            collect_semantic_expression_entries(expr, model, entries);
        }
        StatementNode::If { then_, else_, .. } => {
            collect_statement_entries(then_, model, entries);
            if let Some(e) = else_ {
                collect_statement_entries(e, model, entries);
            }
        }
        StatementNode::Loop { body, .. } => {
            collect_statement_entries(body, model, entries);
        }
        StatementNode::For { init, body, .. } => {
            if let Some(i) = init {
                collect_statement_entries(i, model, entries);
            }
            collect_statement_entries(body, model, entries);
        }
        // Return, Variable, Continue, Break, None - нет вложенных подвыражений с
        // отслеживаемыми позициями
        _ => {}
    }
}

/// Обрабатывает семантическое выражение: добавляет записи только для
/// [`ExpressionNode::Unresolved`], где АСД-форма сохраняет позиции идентификаторов.
///
/// Для всех разрешённых вариантов позиция использования потеряна в ходе семантического
/// понижения - они пропускаются.
pub(super) fn collect_semantic_expression_entries(
    expr: &ExpressionNode,
    model: &Rc<RefCell<ModelNode>>,
    entries: &mut Vec<IndexEntry>,
) {
    if let ExpressionNode::Unresolved(ast_expr) = expr {
        collect_ast_expression_entries(ast_expr, model, entries);
    }
}

/// Рекурсивно обходит АСД-оператор и добавляет записи для переменных и функций.
///
/// Используется, когда оператор не был разрешён в ходе семантического анализа
/// (например, при частичном построении модели или ошибке разрешения).
///
/// ## Примеры
///
/// ```text
/// // Блок с присваиванием -> рекурсивный обход
/// Block { stmts: [Expression(_, Assign(_, Variable("x"), Number(1)))] }
///     -> IndexEntry("x", ...)
///
/// // Оператор Return с выражением -> рекурсивный обход
/// Return(_, Some(Variable("result")))
///     -> IndexEntry("result", ...)
/// ```
pub(super) fn collect_ast_statement_entries(
    stmt: &ast::Statement,
    model: &Rc<RefCell<ModelNode>>,
    entries: &mut Vec<IndexEntry>,
) {
    match stmt {
        ast::Statement::Block { statements, .. } => {
            for s in statements {
                collect_ast_statement_entries(s, model, entries);
            }
        }
        ast::Statement::Expression(_, expr) => {
            collect_ast_expression_entries(expr, model, entries);
        }
        ast::Statement::If(_, cond, then, else_opt) => {
            collect_ast_expression_entries(cond, model, entries);
            collect_ast_statement_entries(then, model, entries);
            if let Some(e) = else_opt {
                collect_ast_statement_entries(e, model, entries);
            }
        }
        // Ключевое слово (`loop`/`while`) на индекс не влияет - синонимы.
        ast::Statement::Loop(_, cond_opt, body, _) => {
            if let Some(c) = cond_opt {
                collect_ast_expression_entries(c, model, entries);
            }
            collect_ast_statement_entries(body, model, entries);
        }
        ast::Statement::For(_, init_opt, cond_opt, step_opt, body_opt) => {
            if let Some(i) = init_opt {
                collect_ast_statement_entries(i, model, entries);
            }
            if let Some(c) = cond_opt {
                collect_ast_expression_entries(c, model, entries);
            }
            if let Some(s) = step_opt {
                collect_ast_expression_entries(s, model, entries);
            }
            if let Some(b) = body_opt {
                collect_ast_statement_entries(b, model, entries);
            }
        }
        ast::Statement::Return(_, Some(expr)) => {
            collect_ast_expression_entries(expr, model, entries);
        }
        // Объявление локальной переменной внутри блока (I8)
        ast::Statement::Variable(_stmt_loc, var_def, init_expr) => {
            // Извлекаем имя и локацию идентификатора объявляемой переменной
            let (name, id_loc) = match var_def.as_ref() {
                ast::VariableDefine::Variable { name: Some(id), .. }
                | ast::VariableDefine::Port { name: Some(id), .. }
                | ast::VariableDefine::Constant { name: Some(id), .. } => (id.name.clone(), id.loc),
                _ => return,
            };
            if let Location::Source(_, start, end) = id_loc {
                entries.push(IndexEntry {
                    start: start as usize,
                    end: end as usize,
                    node_ref: SemanticNodeRef {
                        name: name.clone(),
                        kind: SemanticNodeKind::LocalVar,
                        loc: id_loc,
                        model: Some(model.clone()),
                    },
                });
            }
            // Индексируем инициализатор (если есть переменные-ссылки в нём)
            if let Some(init) = init_expr {
                collect_ast_expression_entries(init, model, entries);
            }
        }
        // Continue, Break, Return(None), StraySemicolon, Error, Assembly, Formula, Args -
        // либо нет идентификаторов, либо не применимы
        _ => {}
    }
}

/// Рекурсивно обходит АСД-выражение и добавляет записи
/// [`SemanticNodeKind::ReferenceCondition`] для переменных и функций с байтовыми
/// позициями из исходного текста.
///
/// ## Примеры
///
/// ```text
/// // Переменная
/// ast::Expression::Variable(Identifier { loc: Source(0, 8, 12), name: "flag" })
///     -> IndexEntry { start:8, end:12, name:"flag", kind:ReferenceCondition }
///
/// // Присваивание: рекурсивно обходим левую и правую части
/// Assign(_, Variable("x"), Variable("y"))
///     -> IndexEntry("x", ...), IndexEntry("y", ...)
///
/// // Вызов функции: запись для имени + рекурсивно по аргументам
/// Function(_, id@"log", [Variable("msg")])
///     -> IndexEntry("log", ...), IndexEntry("msg", ...)
/// ```
///
/// ## Контрпримеры
///
/// ```text
/// // Литерал -> запись не добавляется
/// ast::Expression::Number(_, 42)  ->  (нет записей)
///
/// // Переменная с Implicit/Builtin-позицией -> запись не добавляется
/// ast::Expression::Variable(Identifier { loc: Implicit, name: "x" })  ->  (нет записей)
/// ```
pub(super) fn collect_ast_expression_entries(
    expr: &ast::Expression,
    model: &Rc<RefCell<ModelNode>>,
    entries: &mut Vec<IndexEntry>,
) {
    match expr {
        ast::Expression::Variable(id) => {
            if let Location::Source(_, start, end) = id.loc {
                entries.push(IndexEntry {
                    start: start as usize,
                    end: end as usize,
                    node_ref: SemanticNodeRef {
                        name: id.name.clone(),
                        kind: SemanticNodeKind::ReferenceCondition,
                        loc: id.loc,
                        model: Some(model.clone()),
                    },
                });
            }
        }
        ast::Expression::Function(_, id, args) => {
            if let Location::Source(_, start, end) = id.loc {
                entries.push(IndexEntry {
                    start: start as usize,
                    end: end as usize,
                    node_ref: SemanticNodeRef {
                        name: id.name.clone(),
                        kind: SemanticNodeKind::ReferenceCondition,
                        loc: id.loc,
                        model: Some(model.clone()),
                    },
                });
            }
            for arg in args {
                collect_ast_expression_entries(arg, model, entries);
            }
        }
        // База - выражение: записи собирает тот же обход.
        ast::Expression::ArraySubscript(_, base, index) => {
            collect_ast_expression_entries(base, model, entries);
            collect_ast_expression_entries(index, model, entries);
        }
        ast::Expression::ArraySlice(_, base, _, _) => {
            collect_ast_expression_entries(base, model, entries);
        }
        // Бинарные операторы
        ast::Expression::Power(_, l, r)
        | ast::Expression::Multiply(_, l, r)
        | ast::Expression::Divide(_, l, r)
        | ast::Expression::Modulo(_, l, r)
        | ast::Expression::Add(_, l, r)
        | ast::Expression::Subtract(_, l, r)
        | ast::Expression::ShiftLeft(_, l, r)
        | ast::Expression::ShiftRight(_, l, r)
        | ast::Expression::BitwiseAnd(_, l, r)
        | ast::Expression::BitwiseXor(_, l, r)
        | ast::Expression::BitwiseOr(_, l, r)
        | ast::Expression::Less(_, l, r)
        | ast::Expression::More(_, l, r)
        | ast::Expression::LessEqual(_, l, r)
        | ast::Expression::MoreEqual(_, l, r)
        | ast::Expression::Equal(_, l, r)
        | ast::Expression::NotEqual(_, l, r)
        | ast::Expression::And(_, l, r)
        | ast::Expression::Or(_, l, r)
        | ast::Expression::Assign(_, l, r) => {
            collect_ast_expression_entries(l, model, entries);
            collect_ast_expression_entries(r, model, entries);
        }
        // Унарные операторы
        ast::Expression::Not(_, c)
        | ast::Expression::BitwiseNot(_, c)
        | ast::Expression::UnaryPlus(_, c)
        | ast::Expression::Negate(_, c)
        | ast::Expression::Parenthesis(_, c) => {
            collect_ast_expression_entries(c, model, entries);
        }
        ast::Expression::BitAccess(_, c, _) | ast::Expression::Cast(_, c, _) => {
            collect_ast_expression_entries(c, model, entries);
        }
        ast::Expression::Array(_, items) | ast::Expression::Initializer(_, items) => {
            for item in items {
                collect_ast_expression_entries(item, model, entries);
            }
        }
        ast::Expression::ConditionalOperator(_, cond, then, else_) => {
            collect_ast_expression_entries(cond, model, entries);
            collect_ast_expression_entries(then, model, entries);
            collect_ast_expression_entries(else_, model, entries);
        }
        ast::Expression::CodeBlock(_, expr, stmt) => {
            collect_ast_expression_entries(expr, model, entries);
            collect_ast_statement_entries(stmt, model, entries);
        }
        ast::Expression::NamedFunction(_, expr, _) => {
            collect_ast_expression_entries(expr, model, entries);
        }
        // Литералы (Number, Rational, String, Bool, Type, Address, List) - не
        // индексируются
        _ => {}
    }
}
