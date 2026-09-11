//! Подсветка прогона на чертеже: что горит на листе по адресам экземпляров.
//!
//! Правило - то же, что у холста (`web/static/scheme-run.js`), и записано там же
//! словами; сверку держит тест паритета в `node`. Коротко:
//!
//! - лист модели `M` горит состоянием `S`, если есть адрес с моделью `M` и
//!   состоянием `S`; число экземпляров - число таких адресов; корневой лист - по
//!   адресам с пустым путём;
//! - лист композиции состояния `owner` модели `M` горит шагом `k`, если в пути
//!   адреса стоит сегмент `{ owner, step: k }`, а экземпляр до него - модели `M`
//!   (у корневого листа сегмент первый);
//! - следующий шаг ожидается, когда все шаги, откуда в него ведут рёбра, горят и
//!   завершены;
//! - достижимые - цели рёбер из горящих узлов.

use std::collections::{BTreeMap, BTreeSet};

use serde::Deserialize;

use crate::sheet::DrawSheet;

/// Сегмент адреса экземпляра.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct Segment {
    pub owner: String,
    pub step: usize,
    pub model: String,
}

/// Активное состояние такта с адресом экземпляра.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct Active {
    #[serde(default)]
    pub path: Vec<Segment>,
    #[serde(default)]
    pub model: Option<String>,
    pub state: String,
    #[serde(default)]
    pub done: bool,
}

/// Последний такт: адреса и ожидаемые переходы парами "из, в".
#[derive(Debug, Clone, Default, PartialEq, Eq, Deserialize)]
pub struct Tick {
    #[serde(default)]
    pub active: Vec<Active>,
    #[serde(default)]
    pub next: Vec<(String, String)>,
}

/// Чей лист и где он стоит.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Place {
    pub root: bool,
    pub model: String,
    pub owner: Option<String>,
}

impl Place {
    pub fn of(sheet: &DrawSheet) -> Self {
        Self {
            root: sheet.owner_path == "/",
            model: sheet.model_name.clone(),
            owner: sheet.owner.clone(),
        }
    }
}

/// Экземпляр листа выражения: адреса самого узла листа и все адреса под ним.
#[derive(Debug, Clone, Default)]
pub struct Group<'a> {
    pub own: Vec<&'a Active>,
    pub all: Vec<&'a Active>,
}

impl Group<'_> {
    /// Состояния экземпляра: его узла, у свёрнутой модели - вложенных.
    pub fn states(&self) -> BTreeSet<String> {
        let from = if self.own.is_empty() {
            &self.all
        } else {
            &self.own
        };
        from.iter().map(|a| a.state.clone()).collect()
    }

    fn done(&self) -> bool {
        let from = if self.own.is_empty() {
            &self.all
        } else {
            &self.own
        };
        !from.is_empty() && from.iter().all(|a| a.done)
    }
}

/// Экземпляры листов выражения владельца: номер листа -> адреса.
pub fn steps_of<'a>(
    active: &'a [Active],
    place: &Place,
    owner: &str,
) -> BTreeMap<usize, Group<'a>> {
    let mut out: BTreeMap<usize, Group<'a>> = BTreeMap::new();
    for address in active {
        let path = &address.path;
        let at = path.iter().enumerate().position(|(i, s)| {
            s.owner == owner
                && (if place.root {
                    i == 0
                } else {
                    i > 0 && path[i - 1].model == place.model
                })
        });
        let Some(at) = at else {
            continue;
        };
        let group = out.entry(path[at].step).or_default();
        group.all.push(address);
        if path.len() == at + 1 {
            group.own.push(address);
        }
    }
    out
}

/// Что горит на листе.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Lit {
    pub running: BTreeSet<String>,
    pub expected: BTreeSet<String>,
    pub reachable: BTreeSet<String>,
    pub next_edges: BTreeSet<String>,
    pub counts: BTreeMap<String, usize>,
}

/// Подсветка листа по такту.
pub fn sheet_run(sheet: &DrawSheet, tick: &Tick) -> Lit {
    let place = Place::of(sheet);
    let mut lit = Lit::default();
    if sheet.composition {
        let owner = place.owner.clone().unwrap_or_default();
        let steps = steps_of(&tick.active, &place, &owner);
        let mut done = BTreeSet::new();
        for (step, group) in &steps {
            let Some(node) = sheet.nodes.get(step - 1) else {
                continue;
            };
            lit.counts.insert(node.name.clone(), group.own.len().max(1));
            if group.done() {
                done.insert(node.name.clone());
            }
        }
        for node in &sheet.nodes {
            if lit.counts.contains_key(&node.name) {
                continue;
            }
            let into: Vec<_> = sheet.edges.iter().filter(|e| e.to == node.name).collect();
            if !into.is_empty()
                && into
                    .iter()
                    .all(|e| lit.counts.contains_key(&e.from) && done.contains(&e.from))
            {
                lit.expected.insert(node.name.clone());
                lit.next_edges.extend(into.iter().map(|e| e.key.clone()));
            }
        }
    } else {
        for address in &tick.active {
            let mine = if place.root {
                address.path.is_empty()
            } else {
                address.model.as_deref() == Some(place.model.as_str())
            };
            if mine {
                *lit.counts.entry(address.state.clone()).or_default() += 1;
            }
        }
        let pairs: Vec<&(String, String)> = tick
            .next
            .iter()
            .filter(|p| lit.counts.contains_key(&p.0))
            .collect();
        lit.expected = pairs
            .iter()
            .map(|p| p.1.clone())
            .filter(|n| !lit.counts.contains_key(n))
            .collect();
        lit.next_edges = sheet
            .edges
            .iter()
            .filter(|e| pairs.iter().any(|p| p.0 == e.from && p.1 == e.to))
            .map(|e| e.key.clone())
            .collect();
    }
    lit.running = lit.counts.keys().cloned().collect();
    lit.reachable = sheet
        .edges
        .iter()
        .filter(|e| lit.running.contains(&e.from))
        .map(|e| e.to.clone())
        .filter(|n| !lit.running.contains(n) && !lit.expected.contains(n))
        .collect();
    lit
}

/// Текст плашки: горящие внутренние узлы подписью автора, иначе моделью либо
/// именем, через запятую.
pub fn inner_label(inner: &DrawSheet, lit: &BTreeSet<String>) -> String {
    inner
        .nodes
        .iter()
        .filter(|n| lit.contains(&n.name))
        .map(|n| {
            if !n.alias.is_empty() {
                n.alias.clone()
            } else {
                n.model.clone().unwrap_or_else(|| n.name.clone())
            }
        })
        .collect::<Vec<_>>()
        .join(", ")
}

/// Плашка квадрата композиции: идущие шаги с состояниями; один шаг - без имени.
pub fn steps_label(
    steps: &DrawSheet,
    groups: &BTreeMap<usize, Group<'_>>,
    state_label: impl Fn(usize, &Group<'_>) -> String,
) -> String {
    let shown: Vec<(String, String)> = groups
        .iter()
        .filter_map(|(step, group)| {
            let node = steps.nodes.get(step - 1)?;
            let text = state_label(*step, group);
            let name = if node.alias.is_empty() {
                node.name.clone()
            } else {
                node.alias.clone()
            };
            (!text.is_empty()).then_some((name, text))
        })
        .collect();
    if shown.len() == 1 {
        return shown[0].1.clone();
    }
    shown
        .iter()
        .map(|(n, t)| format!("{n}: {t}"))
        .collect::<Vec<_>>()
        .join(", ")
}
