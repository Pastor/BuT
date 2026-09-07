//! Реестр имён портов и переменных модели.
//!
//! Граница модуля - ответственность: здесь "какие имена есть у модели и её
//! под-моделей", а в `runner.rs` - "как идёт прогон". Сценарий адресует порты именами, и
//! реестр нужен вне прогона: его зовут бинарник и тесты.
//!
//! Двусмысленные и квалифицированные имена строятся **одним** обходом из общего списка
//! владельцев: два реестра из разных источников разошлись бы, и сценарий получал бы
//! "имя не найдено" на имя, которое симулятор знает.
//!
//! Направление порта (`PortDirectionKind`) живёт здесь же: оно есть свойство имени в
//! реестре, и разлучать их значило бы спрашивать "какие имена" в одном месте, а "какого
//! они направления" - в другом.

/// Упорядоченные имена портов модели (по направлению).
pub struct PortNames {
    pub in_ports: Vec<String>,
    pub out_ports: Vec<String>,
    pub inout_ports: Vec<String>,
    pub vars: Vec<String>,
    /// Имена, объявленные более чем одной моделью: голое имя -> квалифицированные
    /// (`Модель::имя`),.
    ///
    /// Плоское пространство имён делает такие значения неразличимыми: чтение по голому
    /// имени находит первую ветвь, запись расходится по всем. Поле существует, чтобы
    /// двусмысленность была **видна** - в предупреждении при запуске и в потактовом
    /// выводе, - а не молчала.
    pub ambiguous: Vec<(String, Vec<String>)>,
    /// Все квалифицированные имена (`Модель::имя`).
    ///
    /// Строится тем же обходом, что и [`ambiguous`](Self::ambiguous), из одного списка
    /// владельцев: два реестра из разных источников разошлись бы, и сценарий получал бы
    /// "имя не найдено" на имя, которое симулятор знает.
    pub qualified: std::collections::BTreeSet<String>,
    /// Имена значений типа `duration`.
    ///
    /// Сценарий пишет числа (`"program": 20`), а тип значения знает только модель. Без
    /// этого реестра число попадало на порт как `Number`, и первое же `program + 30s`
    /// давало `SIM-005` - то есть вычисляемая выдержка на входном порте была
    /// невыполнима эталоном, тогда как цель `c` её печатает.
    ///
    /// Единица числа - **миллисекунды**, как у приведения `as duration`: другая единица
    /// дала бы сценарию свой язык времени.
    pub durations: std::collections::BTreeSet<String>,
}

impl PortNames {
    /// Собирает имена портов/переменных модели и всех её под-моделей композиции.
    ///
    /// Обход рекурсивный: перечисляй он один корень, порты под-моделей композиции
    /// (`Cabin | Motor`) не попадут ни в драйвер, ни в вывод и не подадутся из
    /// сценария, и модель перестанет реагировать на датчики. Читать их эталон умеет и
    /// так (`Unit::get_value` обходит все ветви), а подать вход было бы нечем.
    /// Одноимённые порты разных под-моделей делят значение: пространство имён плоское,
    /// и имена дедуплицируются.
    pub fn from_model(model: &takt_lang::semantic::ModelNode) -> Self {
        let mut names = Self {
            in_ports: Vec::new(),
            out_ports: Vec::new(),
            inout_ports: Vec::new(),
            vars: Vec::new(),
            ambiguous: Vec::new(),
            qualified: std::collections::BTreeSet::new(),
            durations: std::collections::BTreeSet::new(),
        };
        let mut owners: Vec<(String, String)> = Vec::new();
        names.collect_recursive(model, &mut owners);
        names.ambiguous = ambiguous_names(&owners);
        names.qualified = owners
            .iter()
            .map(|(name, owner)| format!("{owner}::{name}"))
            .collect();
        for v in [
            &mut names.in_ports,
            &mut names.out_ports,
            &mut names.inout_ports,
            &mut names.vars,
        ] {
            v.sort();
            v.dedup();
        }
        names
    }

    /// `owners` копит пары "имя значения -> модель, его объявившая": по ним вычисляется
    /// двусмысленность.
    fn collect_recursive(
        &mut self,
        model: &takt_lang::semantic::ModelNode,
        owners: &mut Vec<(String, String)>,
    ) {
        use takt_lang::parser::ast::PortDirection;
        use takt_lang::semantic::VariableNode;
        let owner = model.name.clone();
        for (name, var) in &model.variables {
            // Тип значения нужен сценарию: число в JSON приводится к длительности по
            // имени, потому что JSON типов Takt не знает.
            if matches!(var.ty(), takt_lang::semantic::type_node::TypeNode::Duration) {
                self.durations.insert(name.clone());
            }
            let mine = match var {
                VariableNode::Port { direction, .. } => {
                    match direction {
                        PortDirection::In => self.in_ports.push(name.clone()),
                        PortDirection::Out => self.out_ports.push(name.clone()),
                        PortDirection::InOut => self.inout_ports.push(name.clone()),
                    }
                    true
                }
                VariableNode::Simple { .. } => {
                    self.vars.push(name.clone());
                    true
                }
                _ => false,
            };
            if mine && let Some(owner) = &owner {
                owners.push((name.clone(), owner.clone()));
            }
        }
        for sub in model.models.values() {
            self.collect_recursive(&sub.borrow(), owners);
        }
    }
}

/// Имена, объявленные более чем одной моделью.
///
/// Одна и та же модель, встреченная дважды при обходе, двусмысленности не даёт -
/// поэтому владельцы дедуплицируются перед подсчётом.
fn ambiguous_names(owners: &[(String, String)]) -> Vec<(String, Vec<String>)> {
    let mut by_name: std::collections::BTreeMap<&str, Vec<&str>> =
        std::collections::BTreeMap::new();
    for (name, owner) in owners {
        let slot = by_name.entry(name.as_str()).or_default();
        if !slot.contains(&owner.as_str()) {
            slot.push(owner.as_str());
        }
    }
    by_name
        .into_iter()
        .filter(|(_, models)| models.len() > 1)
        .map(|(name, models)| {
            let mut qualified: Vec<String> =
                models.iter().map(|m| format!("{m}::{name}")).collect();
            qualified.sort();
            (name.to_string(), qualified)
        })
        .collect()
}

/// Направление портов, к которому относятся значения шага.
///
/// Нужен, чтобы одна воронка разрешения имён обслуживала и входы, и `guard`:
/// направление - единственное, чем эти случаи различаются.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PortDirectionKind {
    In,
    Out,
    InOut,
}

impl PortDirectionKind {
    /// Имя поля в файле сценария - для текста диагностики.
    pub(crate) fn field(self) -> &'static str {
        match self {
            Self::In => "in_ports",
            Self::Out => "out",
            Self::InOut => "inout",
        }
    }
}
