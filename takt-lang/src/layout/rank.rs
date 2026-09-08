//! Ярусная раскладка листа: ярус и порядок узла без случайности.
//!
//! Ярус - расстояние от стартового состояния по рёбрам (поиск в ширину); узлы, до
//! которых от старта не дойти, идут ниже всех достижимых. Порядок внутри яруса
//! уменьшает пересечения барицентром: узел встаёт под средним порядком своих
//! предшественников из верхних ярусов, ничьи разрешаются именем. Два вызова на одном
//! листе дают одно и то же: ни зерна, ни обхода по хеш-таблице здесь нет.

use super::Sheet;
use std::collections::{BTreeMap, BTreeSet, VecDeque};

/// Проставляет `rank` и `order` узлам листа.
pub fn assign(sheet: &mut Sheet) {
    let names: Vec<String> = sheet.nodes.iter().map(|n| n.name.clone()).collect();
    let mut successors: BTreeMap<&str, BTreeSet<&str>> = BTreeMap::new();
    let mut predecessors: BTreeMap<&str, BTreeSet<&str>> = BTreeMap::new();
    for edge in &sheet.edges {
        // Ребро в неизвестное состояние узла не порождает: его имя покажет сверка.
        if !names.iter().any(|n| n == &edge.to) || !names.iter().any(|n| n == &edge.from) {
            continue;
        }
        successors
            .entry(edge.from.as_str())
            .or_default()
            .insert(edge.to.as_str());
        predecessors
            .entry(edge.to.as_str())
            .or_default()
            .insert(edge.from.as_str());
    }

    let mut ranks: BTreeMap<&str, u32> = BTreeMap::new();
    if let Some(start) = sheet.start.as_deref() {
        let mut queue = VecDeque::from([start]);
        ranks.insert(start, 0);
        while let Some(at) = queue.pop_front() {
            let next_rank = ranks[at] + 1;
            for &to in successors.get(at).into_iter().flatten() {
                if !ranks.contains_key(to) {
                    ranks.insert(to, next_rank);
                    queue.push_back(to);
                }
            }
        }
    }
    let below = ranks.values().max().map_or(0, |max| max + 1);
    for node in &mut sheet.nodes {
        node.rank = ranks.get(node.name.as_str()).copied().unwrap_or(below);
    }

    // Порядок: ярусы снизу вверх по номеру, внутри - барицентр предшественников из
    // верхних ярусов; без предшественников - после размещённых, по имени.
    let mut order_of: BTreeMap<String, u32> = BTreeMap::new();
    let mut by_rank: BTreeMap<u32, Vec<&str>> = BTreeMap::new();
    for node in &sheet.nodes {
        by_rank.entry(node.rank).or_default().push(&node.name);
    }
    for (rank, members) in by_rank {
        let mut keyed: Vec<(Option<u64>, &str)> = members
            .iter()
            .map(|&name| {
                let upstream: Vec<u32> = predecessors
                    .get(name)
                    .into_iter()
                    .flatten()
                    .filter_map(|&from| {
                        let from_rank = sheet.nodes.iter().find(|n| n.name == from)?.rank;
                        (from_rank < rank)
                            .then(|| order_of.get(from).copied())
                            .flatten()
                    })
                    .collect();
                let barycenter = (!upstream.is_empty()).then(|| {
                    // Среднее в тысячных, чтобы сравнивать целыми и без плавающей точки.
                    u64::from(upstream.iter().sum::<u32>()) * 1000 / upstream.len() as u64
                });
                (barycenter, name)
            })
            .collect();
        keyed.sort_by(|a, b| match (a.0, b.0) {
            (Some(x), Some(y)) => x.cmp(&y).then_with(|| a.1.cmp(b.1)),
            (Some(_), None) => std::cmp::Ordering::Less,
            (None, Some(_)) => std::cmp::Ordering::Greater,
            (None, None) => a.1.cmp(b.1),
        });
        for (position, (_, name)) in keyed.into_iter().enumerate() {
            order_of.insert(name.to_string(), position as u32);
        }
    }
    for node in &mut sheet.nodes {
        node.order = order_of.get(&node.name).copied().unwrap_or(0);
    }
}
