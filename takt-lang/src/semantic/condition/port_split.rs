//! Порт составного типа разворачивается в скалярные.
//!
//! # Правило
//!
//! Порт структурного типа заменяется портами по **листам** структуры: имя -
//! `<порт>_<поле>` (рекурсивно), тип - тип листа, направление - прежнее. Обращения
//! переписываются: агрегатное присваивание - по листам, доступ к полю - прямым
//! обращением к порту листа.
//!
//! За границей семантики составного порта не существует, и печатники целей о нём не
//! знают.
//!
//! **Разворот зовут только те цели, что порт не умеют** (`c`, `c-hal`, `rust`, `st-at`,
//! `sv-mmio`): у `st` и `sv` он работает как есть, и общий разворот изменил бы их вывод
//! без нужды.
//!
//! **Адрес листа - базовый плюс смещение поля в байтах**: у HAL-целей порт ложится на
//! регистр, и раскладка обязана быть предсказуемой. Знание о размере типа берётся у
//! общего носителя `type_size::bytes_of`.

use crate::diagnostics::lang::keys;
use crate::msg;
use std::cell::RefCell;
use std::collections::{BTreeMap, HashSet};
use std::rc::Rc;

use crate::diagnostics::{Diagnostic, Location};
use crate::parser::ast::Member;
use crate::semantic::formula::Formula;
use crate::semantic::type_node::TypeNode;
use crate::semantic::{
    ConditionNode, ExpressionNode, ModelNode, NamedCodeBlockDefinitionNode, StateNode,
    StatementNode, VariableNode,
};

use super::port_subtree;

/// Шаг пути к листу: поле структуры либо элемент массива.
///
/// Путь шагами, а не готовой строкой: форма обращения у поля и у элемента разная
/// (`po.lo` против `bus[0]`), и различать их обязан разворот - тот же приём, что у
/// носителя вложенных агрегатов.
#[derive(Clone, PartialEq)]
pub(super) enum Step {
    Field(String),
    Index(i128),
}

/// Лист в карте разворота: путь, позиции пути и ячейка порта.
pub(super) struct LeafRef {
    pub(super) path: Vec<Step>,
    /// Позиции шагов в позиционном агрегате - ими значение агрегата раздаётся листьям.
    pub(super) positions: Vec<usize>,
    pub(super) cell: Rc<RefCell<VariableNode>>,
}

/// Разворот одного порта: листья и типы всех узлов пути.
pub(super) struct Split {
    /// Листья порта.
    pub(super) leaves: Vec<LeafRef>,
    /// Тип каждого узла пути, включая промежуточные.
    ///
    /// Листьев для подъёма узла-поддерева мало: временная объявляется типом самого узла
    /// (`Inner`, `[u8;2]`), а по набору листьев имя структуры не восстановить. Тип
    /// запоминается при сборе - там он уже в руках.
    pub(super) nodes: Vec<(Vec<Step>, TypeNode)>,
}

/// Карта разворота: имя исходного порта -> его разворот.
pub(super) type LeafCells = BTreeMap<String, Split>;

/// Собранное обходом объявления: листья и типы всех узлов пути.
///
/// Одной структурой, а не парой выходных параметров: у сборщика их стало бы восемь, и
/// `clippy` справедливо считает это перебором.
#[derive(Default)]
struct Collected {
    leaves: Vec<Leaf>,
    nodes: Vec<(Vec<Step>, TypeNode)>,
}

/// Лист развёрнутого порта: имя, путь, тип и смещение адреса в байтах.
struct Leaf {
    name: String,
    path: Vec<Step>,
    /// Позиции шагов в позиционном агрегате: индекс элемента массива либо номер поля
    /// структуры в порядке объявления.
    ///
    /// Ведётся параллельно пути: по самому пути позицию поля не восстановить - тип
    /// структуры лежит в модели, а начальное значение раздаётся листам уже без неё.
    positions: Vec<usize>,
    ty: TypeNode,
    offset: i128,
}

/// Что разворачивать.
///
/// Вид составного типа значим: порт-Структуру цели `st` и `sv` печатают сами, а
/// порт-Массив не умеет никто - у `st` вывод отвергает `iec2c` при нулевом коде
/// возврата, у `sv` цель отказывает `SV-002`. Поэтому массив разворачивается всем, а
/// структура - только тем, кто её не умеет.
#[derive(Clone, Copy, PartialEq)]
pub(crate) enum PortSplit {
    /// Только массивы (цели `st` и `sv`).
    ArraysOnly,
    /// Только структуры (цель `c` с ): массив-порт остаётся одним портом, а его элемент
    /// адресуется индексом в обращении HAL. Разворот массива по листам не выражает
    /// переменного индекса: лист выбирается только по литералу, а `bus[i]` печатался
    /// индексацией имени, которого в выводе нет вовсе.
    StructsOnly,
    /// Массивы и структуры (цели, у которых составного порта нет вовсе).
    All,
}

/// Разворачивает составные порты по всему дереву.
pub(crate) fn split_composite_ports(
    root: &Rc<RefCell<ModelNode>>,
    what: PortSplit,
) -> Result<(), Diagnostic> {
    let mut visited = HashSet::new();
    // Занятые имена собираются по всему дереву и один раз: временная узла не вправе
    // затенить авторское имя, а второй сборщик разошёлся бы с первым молча. Носитель
    // общий - `semantic::fresh`.
    let taken = crate::semantic::fresh::taken_names(root);
    let mut lift = port_subtree::Lift::new(&taken, root);
    split_model(root, what, &mut visited, &mut lift)
}

fn split_model(
    model: &Rc<RefCell<ModelNode>>,
    what: PortSplit,
    visited: &mut HashSet<*const RefCell<ModelNode>>,
    lift: &mut port_subtree::Lift<'_>,
) -> Result<(), Diagnostic> {
    if !visited.insert(Rc::as_ptr(model)) {
        return Ok(());
    }
    let nested: Vec<Rc<RefCell<ModelNode>>> = model.borrow().models.values().cloned().collect();
    split_here(model, what, lift)?;
    for child in &nested {
        split_model(child, what, visited, lift)?;
    }
    Ok(())
}

/// Разворачивает порты одной модели и переписывает её тела.
fn split_here(
    model: &Rc<RefCell<ModelNode>>,
    what: PortSplit,
    lift: &mut port_subtree::Lift<'_>,
) -> Result<(), Diagnostic> {
    // Какие порты разворачивать - решается до правки: обход тел спрашивает готовую
    // карту, а не ищет объявление заново.
    let targets: Vec<(String, VariableNode)> = model
        .borrow()
        .variables
        .iter()
        .filter(|(_, var)| matches!(var, VariableNode::Port { ty, .. } if is_composite(ty, what)))
        .map(|(name, var)| (name.clone(), var.clone()))
        .collect();
    if targets.is_empty() {
        return Ok(());
    }

    let mut cells: LeafCells = BTreeMap::new();
    for (name, var) in &targets {
        let VariableNode::Port {
            ty,
            address,
            direction,
            loc,
            init,
            ..
        } = var
        else {
            continue;
        };
        let mut collected = Collected::default();
        collect_leaves(name, &[], &[], ty, 0, &model.borrow(), &mut collected)?;
        let Collected { leaves, nodes } = collected;
        let base = literal_address(address);
        let mut made = Vec::new();
        for leaf in &leaves {
            let port = VariableNode::Port {
                upper: Some(Rc::downgrade(model)),
                loc: *loc,
                name: leaf.name.clone(),
                ty: leaf.ty.clone(),
                // Адрес листа - базовый плюс смещение поля, числом: форма `Address`
                // несёт ещё и позицию бита, а у поля структуры её нет - `SE-060`
                // отвергал бы любой лист.
                address: match base {
                    Some(value) => ExpressionNode::Number(value + leaf.offset),
                    None => ExpressionNode::None,
                },
                // Начальное значение листа берётся из агрегата исходного порта.
                init: leaf_initializer(init, &leaf.positions),
                direction: *direction,
            };
            model
                .borrow_mut()
                .variables
                .insert(leaf.name.clone(), port.clone());
            made.push(LeafRef {
                path: leaf.path.clone(),
                positions: leaf.positions.clone(),
                cell: Rc::new(RefCell::new(port)),
            });
        }
        cells.insert(
            name.clone(),
            Split {
                leaves: made,
                nodes,
            },
        );
        model.borrow_mut().variables.remove(name);
    }

    // Владелец временных - та модель, чьи тела переписываются.
    lift.set_owner(model);
    rewrite_bodies(model, &cells, lift)
}

/// Составной ли тип порта: структура или массив.
///
/// Бит-вектор `[bit; N <= 64]` - **скаляр**, а не массив: разворачивать его значило бы
/// превратить упакованное слово в набор портов.
fn is_composite(ty: &TypeNode, what: PortSplit) -> bool {
    match ty {
        TypeNode::Struct(_) => matches!(what, PortSplit::All | PortSplit::StructsOnly),
        // Бит-вектор `[bit; N <= 64]` - **скаляр**, а не массив: разворачивать его
        // значило бы превратить упакованное слово в набор портов. У цели, адресующей
        // элемент индексом (`StructsOnly`), не разворачивается и обычный массив.
        TypeNode::Array(_, elem) => {
            if crate::semantic::bit_vector::is_bit_vector(ty).is_some() {
                return false;
            }
            match what {
                // У цели, адресующей элемент индексом, массив скаляров остаётся одним
                // портом, а массив составного разворачивается: индекс в обращении HAL
                // один, и второй уровень им не выразить.
                PortSplit::StructsOnly => {
                    matches!(**elem, TypeNode::Array(..) | TypeNode::Struct(_))
                        && crate::semantic::bit_vector::is_bit_vector(elem).is_none()
                }
                _ => true,
            }
        }
        _ => false,
    }
}

/// Листья структуры: имя `<порт>_<поле>`, тип и смещение в байтах.
///
/// Попутно записывает тип каждого пройденного узла (`nodes`): узел ветвления нужен
/// подъёму во временную, а знать его тип можно только здесь.
fn collect_leaves(
    prefix: &str,
    path: &[Step],
    positions: &[usize],
    ty: &TypeNode,
    offset: i128,
    model: &ModelNode,
    out: &mut Collected,
) -> Result<(), Diagnostic> {
    out.nodes.push((path.to_vec(), ty.clone()));
    // Массив раскрывается по элементам: имя `<порт>_<индекс>`. Бит-вектор сюда не
    // попадает - он скаляр.
    if let TypeNode::Array(size, elem) = ty
        && crate::semantic::bit_vector::is_bit_vector(ty).is_none()
    {
        let step = size_of(elem, model);
        for index in 0..i128::from(*size) {
            let mut next = path.to_vec();
            next.push(Step::Index(index));
            let mut next_positions = positions.to_vec();
            next_positions.push(usize::try_from(index).unwrap_or(0));
            collect_leaves(
                &format!("{prefix}_{index}"),
                &next,
                &next_positions,
                elem,
                offset + index * step,
                model,
                out,
            )?;
        }
        return Ok(());
    }
    let TypeNode::Struct(name) = ty else {
        out.leaves.push(Leaf {
            name: prefix.to_string(),
            path: path.to_vec(),
            positions: positions.to_vec(),
            ty: ty.clone(),
            offset,
        });
        return Ok(());
    };
    let def = model.search_struct(name).ok_or_else(|| {
        Diagnostic::error(
            Location::Codegen,
            msg!(keys::SE_119_STRUCT_NOT_DECLARED, name = name),
        )
        .with_code("SE-119")
    })?;
    let mut at = offset;
    for (position, (field, field_ty)) in def.fields.iter().enumerate() {
        let mut next = path.to_vec();
        next.push(Step::Field(field.clone()));
        let mut next_positions = positions.to_vec();
        next_positions.push(position);
        collect_leaves(
            &format!("{prefix}_{field}"),
            &next,
            &next_positions,
            field_ty,
            at,
            model,
            out,
        )?;
        at += size_of(field_ty, model);
    }
    Ok(())
}

/// Размер типа в байтах - для смещения адреса листа.
///
/// Раскладка **плотная**, без выравнивания: у регистрового файла и MMIO поля идут
/// подряд, а выравнивание - свойство хоста, о котором модель не говорит.
fn size_of(ty: &TypeNode, model: &ModelNode) -> i128 {
    match ty {
        TypeNode::Bit | TypeNode::Bool => 1,
        TypeNode::Integer { bits, .. } => i128::from(bits.div_ceil(8)),
        TypeNode::Duration => 4,
        TypeNode::Fixed { m, n, .. } => i128::from((m + n).div_ceil(8)),
        TypeNode::Enum(_) => 1,
        TypeNode::Array(size, elem) => i128::from(*size) * size_of(elem, model),
        TypeNode::Struct(name) => model.search_struct(name).map_or(1, |def| {
            def.fields.iter().map(|(_, t)| size_of(t, model)).sum()
        }),
        _ => 1,
    }
}

/// Базовый адрес порта, если он задан литералом.
fn literal_address(expr: &ExpressionNode) -> Option<i128> {
    match expr {
        ExpressionNode::Address(value, _) => Some(i128::from(*value)),
        ExpressionNode::Number(value) => Some(*value),
        _ => None,
    }
}

/// Переписывает все места обращения модели: агрегат - по листам, часть порта - прямым
/// обращением к листу.
///
/// Список мест - **один**. Появится новое место обращения - добавлять его нужно здесь,
/// иначе оно молча выпадет.
fn rewrite_bodies(
    model: &Rc<RefCell<ModelNode>>,
    cells: &LeafCells,
    lift: &mut port_subtree::Lift<'_>,
) -> Result<(), Diagnostic> {
    let (mut functions, mut named_blocks, mut states, mut conditions, mut formulas) = {
        let mut b = model.borrow_mut();
        (
            std::mem::take(&mut b.functions),
            std::mem::take(&mut b.named_blocks),
            std::mem::take(&mut b.states),
            std::mem::take(&mut b.conditions),
            std::mem::take(&mut b.formulas),
        )
    };
    // Тела возвращаются на место в любом случае: при отказе дерево обязано остаться
    // целым - его ещё читает печать диагностики.
    let outcome = (|| -> Result<(), Diagnostic> {
        for func in functions.values_mut() {
            if let crate::semantic::FunctionDefinitionNode::Local { body, .. } = func {
                rewrite_stmt(body, cells, lift, None)?;
            }
        }
        for blk in named_blocks.iter_mut() {
            rewrite_block(blk, cells, lift)?;
        }
        for cond in conditions.values_mut() {
            rewrite_cond(&mut cond.value, cells)?;
        }
        for formula in formulas.iter_mut() {
            rewrite_formula(formula, cells)?;
        }
        for state in states.values_mut() {
            rewrite_state(state, cells, lift)?;
        }
        Ok(())
    })();
    let mut b = model.borrow_mut();
    b.functions = functions;
    b.named_blocks = named_blocks;
    b.states = states;
    b.conditions = conditions;
    b.formulas = formulas;
    outcome
}

/// Места обращения одного состояния: блоки, условия рёбер, `next`, формулы.
fn rewrite_state(
    state: &mut StateNode,
    cells: &LeafCells,
    lift: &mut port_subtree::Lift<'_>,
) -> Result<(), Diagnostic> {
    // Поля берутся у варианта напрямую: методов-доступов у `StateNode` нет.
    let (named_blocks, references, formulas, next) = match state {
        StateNode::Simple {
            named_blocks,
            references,
            formulas,
            ..
        } => (named_blocks, references, formulas, None),
        StateNode::Implement {
            named_blocks,
            references,
            formulas,
            next,
            ..
        } => (named_blocks, references, formulas, next.as_mut()),
        StateNode::Unresolved => return Ok(()),
    };
    for blk in named_blocks.iter_mut() {
        rewrite_block(blk, cells, lift)?;
    }
    for reference in references.iter_mut() {
        rewrite_cond(&mut reference.cond, cells)?;
    }
    if let Some(reference) = next {
        rewrite_cond(&mut reference.cond, cells)?;
    }
    for formula in formulas.iter_mut() {
        rewrite_formula(formula, cells)?;
    }
    Ok(())
}

/// Формула: охранная несёт условие, темпоральная говорит о состояниях.
///
/// `Formula::LTL` в объём не входит: её атомы - имена состояний и предикаты
/// верификатора, до целей она не доезжает вовсе.
fn rewrite_formula(formula: &mut Formula, cells: &LeafCells) -> Result<(), Diagnostic> {
    match formula {
        Formula::Guard(cond, _, _) => rewrite_cond(cond, cells)?,
        Formula::Formulas(items) => {
            for item in items.iter_mut() {
                rewrite_formula(item, cells)?;
            }
        }
        Formula::None | Formula::LTL(_, _) => {}
    }
    Ok(())
}

/// `cfg.tail.b` в условии -> порт листа; прочее - рекурсия.
///
/// Узел-Поддерево здесь отвергается `SE-130`: условию неоткуда взять временную, из
/// которой узел собирается, а печать обращения к исчезнувшему порту дала бы невалидный
/// вывод при нулевом коде возврата.
fn rewrite_cond(cond: &mut ConditionNode, cells: &LeafCells) -> Result<(), Diagnostic> {
    // Замена вычисляется отдельно: заимствование `cond` живо, пока в нём ищут лист, и
    // присвоение внутри `if let` компилятор не пропустит.
    if let Some((name, path, loc)) = cond_path(cond) {
        if let Some(cell) = leaf_cell(cells, &name, &path) {
            *cond = ConditionNode::Variable(cell, loc);
            return Ok(());
        }
        if port_subtree::subtree_type(cells, &name, &path).is_some() {
            return Err(port_subtree::refuse(&name, &path, loc));
        }
    }
    match cond {
        ConditionNode::Parenthesis(inner)
        | ConditionNode::Not(inner)
        | ConditionNode::AfterExpr(inner)
        | ConditionNode::BitAccess(inner, _) => rewrite_cond(inner, cells)?,
        ConditionNode::ArraySubscript(l, r)
        | ConditionNode::Add(l, r)
        | ConditionNode::Subtract(l, r)
        | ConditionNode::And(l, r)
        | ConditionNode::Or(l, r)
        | ConditionNode::Less(l, r)
        | ConditionNode::More(l, r)
        | ConditionNode::LessEqual(l, r)
        | ConditionNode::MoreEqual(l, r)
        | ConditionNode::Equal(l, r)
        | ConditionNode::NotEqual(l, r) => {
            rewrite_cond(l, cells)?;
            rewrite_cond(r, cells)?;
        }
        ConditionNode::Function(_, args, _) => {
            for arg in args.iter_mut() {
                rewrite_cond(arg, cells)?;
            }
        }
        _ => {}
    }
    Ok(())
}

fn rewrite_block(
    blk: &mut NamedCodeBlockDefinitionNode,
    cells: &LeafCells,
    lift: &mut port_subtree::Lift<'_>,
) -> Result<(), Diagnostic> {
    match blk {
        NamedCodeBlockDefinitionNode::Enter { body, .. }
        | NamedCodeBlockDefinitionNode::Exit { body, .. }
        | NamedCodeBlockDefinitionNode::Always { body, .. }
        | NamedCodeBlockDefinitionNode::Unknown { body, .. }
        | NamedCodeBlockDefinitionNode::Every { body, .. } => rewrite_stmt(body, cells, lift, None),
        NamedCodeBlockDefinitionNode::None | NamedCodeBlockDefinitionNode::Unresolved(_, _) => {
            Ok(())
        }
    }
}

/// Обходит оператор.
///
/// `prelude` - куда класть объявление и присваивания временной при подъёме
/// узла-поддерева. `None` означает "объявить негде": такие позиции отвечают `SE-130`, а
/// не печатают обращение к исчезнувшему порту.
fn rewrite_stmt(
    stmt: &mut StatementNode,
    cells: &LeafCells,
    lift: &mut port_subtree::Lift<'_>,
    mut prelude: Option<&mut Vec<StatementNode>>,
) -> Result<(), Diagnostic> {
    match stmt {
        StatementNode::Block(items) => {
            let mut out = Vec::with_capacity(items.len());
            for mut item in std::mem::take(items) {
                // Пролог принадлежит оператору и встаёт перед ним: объявление временной
                // живёт в том же блоке, что и её употребление.
                let mut own: Vec<StatementNode> = Vec::new();
                rewrite_stmt(&mut item, cells, lift, Some(&mut own))?;
                let parts = split_aggregate_assign(&item, cells, lift, Some(&mut own))?;
                out.extend(own);
                match parts {
                    Some(parts) => out.extend(parts),
                    None => out.push(item),
                }
            }
            *items = out;
        }
        StatementNode::Expression(..) => {
            // Агрегатное присваивание порту разворачивается в блок по листам; прочее -
            // обычный обход выражения.
            if let Some(parts) = split_aggregate_assign(stmt, cells, lift, prelude.as_deref_mut())?
            {
                *stmt = StatementNode::Block(parts);
                return Ok(());
            }
            if let StatementNode::Expression(expr, loc) = stmt {
                let at = *loc;
                rewrite_expr(expr, cells, lift, prelude.as_deref_mut(), at)?;
            }
        }
        // Условие оператора - тоже место обращения: обойди проход одно тело, и
        // `if cfg.lo > 1 { ... }` доедет до цели с обращением к развёрнутому порту.
        StatementNode::If {
            cond, then_, else_, ..
        } => {
            // Условие `if` вычисляется один раз, поэтому пролог перед оператором точен -
            // в отличие от условия цикла (см. ниже).
            rewrite_expr(
                cond,
                cells,
                lift,
                prelude.as_deref_mut(),
                Location::Implicit,
            )?;
            rewrite_stmt(then_, cells, lift, None)?;
            if let Some(alt) = else_ {
                rewrite_stmt(alt, cells, lift, None)?;
            }
        }
        // У цикла пролога нет и быть не может: он встал бы перед циклом, и значение
        // узла перестало бы обновляться по итерациям - молчаливое расхождение вместо
        // громкого отказа.
        StatementNode::Loop { cond, body, .. } => {
            if let Some(c) = cond {
                rewrite_expr(c, cells, lift, None, Location::Implicit)?;
            }
            rewrite_stmt(body, cells, lift, None)?;
        }
        StatementNode::For {
            init,
            cond,
            step,
            body,
            loc,
        } => {
            let at = *loc;
            if let Some(i) = init {
                rewrite_stmt(i, cells, lift, None)?;
            }
            if let Some(c) = cond {
                rewrite_expr(c, cells, lift, None, at)?;
            }
            if let Some(st) = step {
                rewrite_expr(st, cells, lift, None, at)?;
            }
            rewrite_stmt(body, cells, lift, None)?;
        }
        StatementNode::Match { expr, arms, .. } => {
            rewrite_expr(
                expr,
                cells,
                lift,
                prelude.as_deref_mut(),
                Location::Implicit,
            )?;
            for arm in arms.iter_mut() {
                for pattern in arm.patterns.iter_mut() {
                    if let crate::semantic::MatchPatternNode::Value(value) = pattern {
                        rewrite_expr(value, cells, lift, None, Location::Implicit)?;
                    }
                }
                rewrite_stmt(&mut arm.body, cells, lift, None)?;
            }
        }
        // Вставка для одной цели - обычные операторы Takt: обращение к порту в ней
        // такое же, как в любом теле.
        StatementNode::Assembly { body, .. } => rewrite_stmt(body, cells, lift, None)?,
        StatementNode::InlineFormula(formulas) => {
            for formula in formulas.iter_mut() {
                rewrite_formula(formula, cells)?;
            }
        }
        StatementNode::Return(Some(expr), _) => {
            rewrite_expr(
                expr,
                cells,
                lift,
                prelude.as_deref_mut(),
                Location::Implicit,
            )?;
        }
        StatementNode::Variable(_, _, Some(init), loc) => {
            let at = *loc;
            rewrite_expr(init, cells, lift, prelude, at)?;
        }
        _ => {}
    }
    Ok(())
}

/// `порт.узел := значение;` -> по присваиванию на каждый лист под узлом.
///
/// `None` - не тот случай: цель записи не ведёт к развёрнутому порту, адресует сам лист
/// (его переписывает `rewrite_expr`) либо агрегат справа до листьев не раздаётся (длину
/// судит `SE-123` - второй проверки здесь нет).
///
/// Узлом записи бывает не только порт целиком: `res.tail := v;` и `res := {1, {2, 3}};`
/// разворачиваются так же, иначе они доедут до целей нетронутыми и `cc` ответит "use of
/// undeclared identifier" при нулевом коде возврата. Правая часть раздаётся листьям по
/// позициям (агрегат) либо доступом по остатку пути (значение), и вложенность обеим
/// формам безразлична.
fn split_aggregate_assign(
    stmt: &StatementNode,
    cells: &LeafCells,
    lift: &mut port_subtree::Lift<'_>,
    mut prelude: Option<&mut Vec<StatementNode>>,
) -> Result<Option<Vec<StatementNode>>, Diagnostic> {
    let StatementNode::Expression(expr, loc) = stmt else {
        return Ok(None);
    };
    let ExpressionNode::Assign(target, value) = &**expr else {
        return Ok(None);
    };
    let Some((name, path)) = expr_path(target) else {
        return Ok(None);
    };
    let Some(split) = cells.get(&name) else {
        return Ok(None);
    };
    if split.leaves.iter().any(|leaf| leaf.path == path) {
        return Ok(None); // адресован сам лист - обычная запись в скалярный порт
    }
    let under: Vec<&LeafRef> = split
        .leaves
        .iter()
        .filter(|leaf| leaf.path.starts_with(&path))
        .collect();
    if under.is_empty() {
        return Ok(None);
    }
    // Правая часть бывает двух видов, и оба штатны: агрегат (`po := {1, 2};`) и
    // значение составного типа (`po := v;`). Второй раскладывается доступом к части -
    // иначе цель получала бы структуру в скалярном колбэке.
    let aggregate = matches!(
        &**value,
        ExpressionNode::Initializer(_) | ExpressionNode::Array(_)
    );
    let parts: Vec<ExpressionNode> = under
        .iter()
        .map(|leaf| {
            if aggregate {
                leaf_initializer(value, &leaf.positions[path.len()..])
            } else {
                access_by_path(value, &leaf.path[path.len()..], *loc)
            }
        })
        .collect();
    if parts
        .iter()
        .any(|part| matches!(part, ExpressionNode::None))
    {
        return Ok(None);
    }
    let mut out = Vec::with_capacity(under.len());
    for (leaf, item) in under.iter().zip(parts) {
        let mut value = item;
        rewrite_expr(&mut value, cells, lift, prelude.as_deref_mut(), *loc)?;
        out.push(StatementNode::Expression(
            Box::new(ExpressionNode::Assign(
                Box::new(ExpressionNode::Variable(Rc::clone(&leaf.cell))),
                Box::new(value),
            )),
            *loc,
        ));
    }
    Ok(Some(out))
}

/// Ячейка листа по полному пути от имени порта.
///
/// Путь сопоставляется целиком. Цели печатали ссылку на несуществующее имя при нулевом
/// коде возврата (`cc`, `iec2c` и `verilator` отвергали вывод).
fn leaf_cell(cells: &LeafCells, name: &str, path: &[Step]) -> Option<Rc<RefCell<VariableNode>>> {
    if path.is_empty() {
        return None;
    }
    cells
        .get(name)
        .and_then(|split| {
            split
                .leaves
                .iter()
                .find(|leaf| leaf.path.as_slice() == path)
        })
        .map(|leaf| Rc::clone(&leaf.cell))
}

/// Путь обращения к части порта: имя порта и шаги от него.
///
/// Индекс обязан быть литералом: при переменном лист неизвестен, и такой вход уходит
/// прежним путём - к отказу цели, а не к молчаливо неверному обращению.
fn expr_path(expr: &ExpressionNode) -> Option<(String, Vec<Step>)> {
    match expr {
        ExpressionNode::Variable(var) => Some((var.borrow().name().to_string(), Vec::new())),
        ExpressionNode::BitAccess(base, Member::Identifier(field)) => {
            let (name, mut path) = expr_path(base)?;
            path.push(Step::Field(field.name.clone()));
            Some((name, path))
        }
        ExpressionNode::ArraySubscript(base, index) => {
            let ExpressionNode::Number(value) = &**index else {
                return None;
            };
            let (name, mut path) = expr_path(base)?;
            path.push(Step::Index(*value));
            Some((name, path))
        }
        _ => None,
    }
}

/// Путь обращения к части порта в условии: имя, шаги и позиция использования.
///
/// Условие - своё дерево (инвариант "`Condition` и `Expression` не унифицировать"),
/// поэтому сбор пути здесь свой. Позиция берётся у базы: у листа своей позиции
/// использования нет, а терять её нельзя - по ней индекс LSP находит узел под курсором.
fn cond_path(cond: &ConditionNode) -> Option<(String, Vec<Step>, Location)> {
    match cond {
        ConditionNode::Variable(var, loc) => {
            Some((var.borrow().name().to_string(), Vec::new(), *loc))
        }
        ConditionNode::BitAccess(base, Member::Identifier(field)) => {
            let (name, mut path, loc) = cond_path(base)?;
            path.push(Step::Field(field.name.clone()));
            Some((name, path, loc))
        }
        ConditionNode::ArraySubscript(base, index) => {
            let ConditionNode::Number(value) = &**index else {
                return None;
            };
            let (name, mut path, loc) = cond_path(base)?;
            path.push(Step::Index(*value));
            Some((name, path, loc))
        }
        _ => None,
    }
}

/// Начальное значение листа: часть агрегата по позициям пути.
///
/// `ExpressionNode::None` - начального значения не было либо оно не агрегат:
/// подставлять целое значение каждому листу нельзя, а гадать не о чем.
///
/// Разбирается **значение**, а не форма записи: агрегат массива и инициализатор
/// структуры - разные узлы, а позиционный доступ у них общий (порядок элементов значим -
/// ).
fn leaf_initializer(init: &ExpressionNode, positions: &[usize]) -> ExpressionNode {
    let mut value = init;
    for position in positions {
        while let ExpressionNode::Parenthesis(inner) = value {
            value = inner;
        }
        let items = match value {
            ExpressionNode::Array(items) | ExpressionNode::Initializer(items) => items,
            _ => return ExpressionNode::None,
        };
        match items.get(*position) {
            Some(next) => value = next,
            None => return ExpressionNode::None,
        }
    }
    value.clone()
}

/// Обращение к части значения по пути листа: поле - точкой, элемент - индексом.
pub(super) fn access_by_path(
    value: &ExpressionNode,
    path: &[Step],
    loc: Location,
) -> ExpressionNode {
    let mut expr = value.clone();
    for step in path {
        expr = match step {
            Step::Field(name) => ExpressionNode::BitAccess(
                Box::new(expr),
                Member::Identifier(crate::parser::ast::Identifier {
                    loc,
                    name: name.clone(),
                }),
            ),
            Step::Index(index) => ExpressionNode::ArraySubscript(
                Box::new(expr),
                Box::new(ExpressionNode::Number(*index)),
            ),
        };
    }
    expr
}

/// `po.lo` -> порт листа; прочее - рекурсия. `cfg.tail.b` -> порт листа; узел-поддерево ->
/// временная; прочее - рекурсия.
fn rewrite_expr(
    expr: &mut ExpressionNode,
    cells: &LeafCells,
    lift: &mut port_subtree::Lift<'_>,
    mut prelude: Option<&mut Vec<StatementNode>>,
    loc: Location,
) -> Result<(), Diagnostic> {
    // Замена вычисляется отдельно: заимствование `expr` живо, пока в нём ищут лист, и
    // присвоение внутри `if let` компилятор не пропустит.
    if let Some((name, path)) = expr_path(expr) {
        if let Some(cell) = leaf_cell(cells, &name, &path) {
            *expr = ExpressionNode::Variable(cell);
            return Ok(());
        }
        // Узел ветвления собирается из листьев во временную. Там, где её объявить
        // негде, отказ называет обход - печать обращения к исчезнувшему порту дала бы
        // невалидный вывод при нулевом коде возврата.
        if let Some(ty) = port_subtree::subtree_type(cells, &name, &path) {
            let Some(prelude) = prelude.as_deref_mut() else {
                return Err(port_subtree::refuse(&name, &path, loc));
            };
            *expr = port_subtree::lift(cells, &name, &path, ty, lift, prelude, loc);
            return Ok(());
        }
    }
    match expr {
        ExpressionNode::Assign(l, r) => {
            // Левая часть - Место записи: узел там раздаётся листьям
            // (`split_aggregate_assign`), а не поднимается во временную.
            rewrite_expr(l, cells, lift, None, loc)?;
            rewrite_expr(r, cells, lift, prelude.as_deref_mut(), loc)?;
        }
        ExpressionNode::Parenthesis(inner)
        | ExpressionNode::Not(inner)
        | ExpressionNode::Negate(inner)
        | ExpressionNode::BitwiseNot(inner)
        | ExpressionNode::UnaryPlus(inner)
        | ExpressionNode::Cast(inner, _) => {
            rewrite_expr(inner, cells, lift, prelude.as_deref_mut(), loc)?;
        }
        ExpressionNode::Add(l, r)
        | ExpressionNode::Subtract(l, r)
        | ExpressionNode::Multiply(l, r)
        | ExpressionNode::Divide(l, r)
        | ExpressionNode::Modulo(l, r)
        | ExpressionNode::Power(l, r)
        | ExpressionNode::ShiftLeft(l, r)
        | ExpressionNode::ShiftRight(l, r)
        | ExpressionNode::BitwiseAnd(l, r)
        | ExpressionNode::BitwiseOr(l, r)
        | ExpressionNode::BitwiseXor(l, r)
        | ExpressionNode::Equal(l, r)
        | ExpressionNode::NotEqual(l, r)
        | ExpressionNode::Less(l, r)
        | ExpressionNode::LessEqual(l, r)
        | ExpressionNode::More(l, r)
        | ExpressionNode::MoreEqual(l, r)
        | ExpressionNode::And(l, r)
        | ExpressionNode::Or(l, r) => {
            rewrite_expr(l, cells, lift, prelude.as_deref_mut(), loc)?;
            rewrite_expr(r, cells, lift, prelude.as_deref_mut(), loc)?;
        }
        ExpressionNode::Function(_, args)
        | ExpressionNode::Array(args)
        | ExpressionNode::Initializer(args) => {
            for a in args.iter_mut() {
                rewrite_expr(a, cells, lift, prelude.as_deref_mut(), loc)?;
            }
        }
        // Именованное условие живёт в двух представлениях: значением в карте модели и
        // разделяемой ячейкой в теле. Правки карты мало - печатник берёт ячейку, и `if
        // hot { ... }` доезжал до цели с обращением к развёрнутому порту.
        ExpressionNode::Condition(cell) => rewrite_cond(&mut cell.borrow_mut().value, cells)?,
        _ => {}
    }
    Ok(())
}
