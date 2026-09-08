//! Граф модели для схемы: листы по моделям, узлы-состояния, рёбра, ярусы.
//!
//! # Что это
//!
//! Схема состояний в редакторе показывает **что** рисовать по ответу компилятора, а
//! **где** - по файлу раскладки автора. Этот модуль отвечает на первый вопрос: из
//! разобранного текста строится [`Graph`] - список листов ([`Sheet`]) по моделям
//! файла, у каждого листа узлы ([`Node`]) с видом и позициями в тексте, рёбра
//! ([`Edge`]) с цитатой условия и ярусная раскладка ([`rank`]) для начального
//! размещения.
//!
//! # Граф строится по АСД
//!
//! Схема - вид текста автора и обязана отвечать, пока модель дописывается: неизвестное
//! имя в условии ребра или несходящийся тип не отменяют состояний и переходов. Дерево
//! на таком тексте не строится, АСД - строится. Правила при этом те же, что у семантики:
//! вид "конец" судит [`semantic::terminal`](crate::semantic::terminal), выражение
//! реализации читается в тех же формах (имя, инстанцирование, `+`, `|`, скобки), имя
//! модели ищется вверх по вложенности.
//!
//! Геометрии здесь нет: размеры карточек, шаг сетки и трассировка рёбер принадлежат
//! оформлению, и считает их страница.

pub mod rank;
#[cfg(test)]
mod tests;

use crate::diagnostics::{Diagnostic, Location};
use crate::parser::ast::{self, Expression, ModelElement, StateElement, StateKind};
use std::collections::BTreeMap;

/// Ключ корневого листа.
pub const ROOT_PATH: &str = "/";

/// Разделитель сегментов пути модели.
pub const PATH_SEPARATOR: char = '/';

/// Граф модели: листы в порядке объявления, корень первым.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Graph {
    /// Листы: корень первым, далее вложенные модели в порядке объявления.
    pub sheets: Vec<Sheet>,
}

/// Лист - одна модель файла.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Sheet {
    /// Путь модели: [`ROOT_PATH`] у корня, `Engine`, `Outer/Inner` у вложенных.
    pub path: String,
    /// Имя модели; пусто у корня.
    pub name: String,
    /// Позиция объявления модели; у корня - весь файл.
    pub loc: Location,
    /// Позиция имени модели; у корня нет.
    pub name_loc: Option<Location>,
    /// Имя стартового состояния, если оно объявлено.
    pub start: Option<String>,
    /// Реализация самой модели (`model M = A | B { ... }`).
    pub implements: Option<Implement>,
    /// Узлы в порядке объявления.
    pub nodes: Vec<Node>,
    /// Рёбра в порядке объявления.
    pub edges: Vec<Edge>,
}

/// Вид узла.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NodeKind {
    /// Стартовое состояние.
    Start,
    /// Обычное состояние.
    State,
    /// Конечное состояние: без рёбер и без тела, исполняемого каждый такт.
    End,
    /// Состояние с реализацией (`= Модель`, `= A + B`).
    Composition,
}

impl NodeKind {
    /// Имя вида для ответа наружу.
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Start => "start",
            Self::State => "state",
            Self::End => "end",
            Self::Composition => "composition",
        }
    }
}

/// Узел - состояние модели.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Node {
    /// Имя состояния.
    pub name: String,
    /// Вид узла.
    pub kind: NodeKind,
    /// Стартовое ли состояние; у композиции вид этого не говорит.
    pub start: bool,
    /// Позиция объявления состояния целиком.
    pub loc: Location,
    /// Позиция имени состояния.
    pub name_loc: Location,
    /// Дерево реализации у композиции.
    pub implements: Option<Implement>,
    /// Ярус: расстояние от стартового состояния по рёбрам; недостижимые - ниже всех.
    pub rank: u32,
    /// Порядок внутри яруса.
    pub order: u32,
}

/// Дерево реализации состояния или модели.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Implement {
    /// Ссылка на модель по имени; `path` - её лист в этом файле, `None` - модель в файле
    /// не объявлена (пришла через `import`).
    Model {
        /// Имя модели, как написано в выражении.
        name: String,
        /// Путь листа модели в этом файле.
        path: Option<String>,
        /// Позиция имени в выражении.
        loc: Location,
    },
    /// Цепочка `A + B`.
    Chain(Vec<Implement>),
    /// Параллель `A | B`.
    Parallel(Vec<Implement>),
    /// Скобки.
    Group(Box<Implement>),
}

/// Вид ребра.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EdgeKind {
    /// `ref Имя [: Условие];`
    Ref,
    /// `next Имя;`
    Next,
}

impl EdgeKind {
    /// Имя вида для ответа наружу.
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Ref => "ref",
            Self::Next => "next",
        }
    }
}

/// Ребро - переход между состояниями листа.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Edge {
    /// Состояние-источник.
    pub from: String,
    /// Состояние-цель; может отсутствовать на листе, пока текст дописывается.
    pub to: String,
    /// Порядковый номер среди рёбер той же пары `from -> to`, с нуля.
    pub ordinal: u32,
    /// Вид перехода.
    pub kind: EdgeKind,
    /// Цитата условия из исходника; `None` - безусловный переход.
    pub condition: Option<String>,
    /// Позиция записи перехода.
    pub loc: Location,
}

/// Строит граф по тексту модели.
///
/// # Ошибки
/// Текст не разбирается: первая диагностика разбора.
pub fn graph_of(source: &str) -> Result<Graph, Diagnostic> {
    let (model, _comments) = crate::parse(source, 0).map_err(|diagnostics| {
        diagnostics
            .into_iter()
            .next()
            .unwrap_or_else(|| Diagnostic::error(Location::Implicit, "разбор не удался".into()))
    })?;
    Ok(graph(&model, source))
}

/// Строит граф по разобранной модели; `source` нужен для цитат условий.
pub fn graph(model: &ast::Model, source: &str) -> Graph {
    let index = ModelIndex::of(model);
    let mut sheets = Vec::new();
    collect_sheets(model, ROOT_PATH, &index, source, &mut sheets);
    for sheet in &mut sheets {
        rank::assign(sheet);
    }
    Graph { sheets }
}

/// Указатель моделей файла: путь -> имена вложенных моделей и их пути.
///
/// Нужен разрешению имени в выражении реализации: имя ищется в текущей модели и выше
/// по вложенности, как это делает поиск модели в семантике.
struct ModelIndex {
    children: BTreeMap<String, BTreeMap<String, String>>,
}

impl ModelIndex {
    fn of(model: &ast::Model) -> Self {
        let mut index = Self {
            children: BTreeMap::new(),
        };
        index.walk(model, ROOT_PATH);
        index
    }

    fn walk(&mut self, model: &ast::Model, path: &str) {
        let mut own = BTreeMap::new();
        for element in &model.elements {
            if let ModelElement::Model(nested) = element
                && let Some(name) = nested.name.as_ref()
            {
                let nested_path = join_path(path, &name.name);
                own.insert(name.name.clone(), nested_path.clone());
                self.walk(nested, &nested_path);
            }
        }
        self.children.insert(path.to_string(), own);
    }

    /// Ищет модель по имени от `path` вверх; `None` - в файле не объявлена.
    fn resolve(&self, path: &str, name: &str) -> Option<String> {
        let mut current = Some(path.to_string());
        while let Some(at) = current {
            if let Some(found) = self.children.get(&at).and_then(|own| own.get(name)) {
                return Some(found.clone());
            }
            current = parent_path(&at);
        }
        None
    }
}

/// Путь вложенной модели: у корня - имя, иначе `родитель/имя`.
fn join_path(parent: &str, name: &str) -> String {
    if parent == ROOT_PATH {
        name.to_string()
    } else {
        format!("{parent}{PATH_SEPARATOR}{name}")
    }
}

/// Путь родителя: у корня нет, у верхней модели - корень.
fn parent_path(path: &str) -> Option<String> {
    if path == ROOT_PATH {
        return None;
    }
    Some(
        path.rfind(PATH_SEPARATOR)
            .map(|at| path[..at].to_string())
            .unwrap_or_else(|| ROOT_PATH.to_string()),
    )
}

/// Собирает листы модели и вложенных моделей в порядке объявления.
fn collect_sheets(
    model: &ast::Model,
    path: &str,
    index: &ModelIndex,
    source: &str,
    out: &mut Vec<Sheet>,
) {
    let mut sheet = Sheet {
        path: path.to_string(),
        name: model
            .name
            .as_ref()
            .map(|n| n.name.clone())
            .unwrap_or_default(),
        loc: model.loc,
        name_loc: model.name.as_ref().map(|n| n.loc),
        start: None,
        implements: model
            .implements
            .as_ref()
            .map(|expression| implement_of(expression, path, index)),
        nodes: Vec::new(),
        edges: Vec::new(),
    };
    let mut nested = Vec::new();
    for element in &model.elements {
        match element {
            ModelElement::State(state) => add_state(state, &mut sheet, path, index, source),
            ModelElement::Model(child) => nested.push(child),
            _ => {}
        }
    }
    number_edges(&mut sheet.edges);
    out.push(sheet);
    for child in nested {
        if let Some(name) = child.name.as_ref() {
            collect_sheets(child, &join_path(path, &name.name), index, source, out);
        }
    }
}

/// Добавляет состояние узлом и его переходы рёбрами.
fn add_state(
    state: &ast::StateDefine,
    sheet: &mut Sheet,
    path: &str,
    index: &ModelIndex,
    source: &str,
) {
    let Some(name) = state.name.as_ref() else {
        return;
    };
    let mut edges = Vec::new();
    for element in &state.elements {
        match element {
            StateElement::Reference(loc, target, condition) => edges.push(Edge {
                from: name.name.clone(),
                to: target.name.clone(),
                ordinal: 0,
                kind: EdgeKind::Ref,
                condition: condition
                    .as_ref()
                    .map(|_| condition_quote(source, target.loc, *loc)),
                loc: *loc,
            }),
            StateElement::Next(target) => edges.push(Edge {
                from: name.name.clone(),
                to: target.name.clone(),
                ordinal: 0,
                kind: EdgeKind::Next,
                condition: None,
                loc: target.loc,
            }),
            _ => {}
        }
    }
    let implements = state
        .implements
        .as_ref()
        .map(|expression| implement_of(expression, path, index));
    let start = matches!(state.kind, Some(StateKind::Start));
    let kind = if implements.is_some() {
        NodeKind::Composition
    } else if start {
        NodeKind::Start
    } else if matches!(state.kind, Some(StateKind::End))
        || crate::semantic::terminal::is_end(&edges, &state.elements)
    {
        NodeKind::End
    } else {
        NodeKind::State
    };
    if start {
        sheet.start.get_or_insert_with(|| name.name.clone());
    }
    sheet.nodes.push(Node {
        name: name.name.clone(),
        kind,
        start,
        loc: state.loc,
        name_loc: name.loc,
        implements,
        rank: 0,
        order: 0,
    });
    sheet.edges.extend(edges);
}

/// Порядковый номер ребра внутри пары `from -> to`.
fn number_edges(edges: &mut [Edge]) {
    let mut seen: BTreeMap<(String, String), u32> = BTreeMap::new();
    for edge in edges {
        let count = seen
            .entry((edge.from.clone(), edge.to.clone()))
            .or_insert(0);
        edge.ordinal = *count;
        *count += 1;
    }
}

/// Цитата условия: текст между именем цели и концом записи `ref`, без `:` и `;`.
///
/// Пробелы и переводы строк внутри схлопываются: на схеме условие показывается одной
/// строкой.
fn condition_quote(source: &str, target: Location, reference: Location) -> String {
    let (Location::Source(_, _, after_name), Location::Source(_, _, end)) = (target, reference)
    else {
        return String::new();
    };
    let (after_name, end) = (after_name as usize, end as usize);
    if after_name > end || end > source.len() {
        return String::new();
    }
    let raw = source.get(after_name..end).unwrap_or_default();
    let raw = raw.trim().trim_end_matches(';').trim();
    let raw = raw.strip_prefix(':').unwrap_or(raw);
    raw.split_whitespace().collect::<Vec<_>>().join(" ")
}

/// Дерево реализации по выражению - в тех же формах, что читает семантика.
///
/// Прочие формы выражения - ошибка автора, которую назовёт семантика (`SE-081`); здесь
/// они дают пустую цепочку, чтобы схема не пропала вместе с узлом.
fn implement_of(expression: &Expression, path: &str, index: &ModelIndex) -> Implement {
    match expression {
        Expression::Variable(id) | Expression::Function(_, id, _) => Implement::Model {
            name: id.name.clone(),
            path: index.resolve(path, &id.name),
            loc: id.loc,
        },
        Expression::Parenthesis(_, inner) => {
            Implement::Group(Box::new(implement_of(inner, path, index)))
        }
        Expression::Add(_, left, right) => {
            let mut items = Vec::new();
            push_flat(implement_of(left, path, index), &mut items, true);
            push_flat(implement_of(right, path, index), &mut items, true);
            Implement::Chain(items)
        }
        Expression::BitwiseOr(_, left, right) => {
            let mut items = Vec::new();
            push_flat(implement_of(left, path, index), &mut items, false);
            push_flat(implement_of(right, path, index), &mut items, false);
            Implement::Parallel(items)
        }
        _ => Implement::Chain(Vec::new()),
    }
}

/// Кладёт элемент в список композиции, сливая цепочку с цепочкой и параллель с
/// параллелью: `A + B + C` - одна цепочка из трёх, а не вложенные пары.
fn push_flat(item: Implement, items: &mut Vec<Implement>, chain: bool) {
    match item {
        Implement::Chain(inner) if chain => items.extend(inner),
        Implement::Parallel(inner) if !chain => items.extend(inner),
        other => items.push(other),
    }
}
