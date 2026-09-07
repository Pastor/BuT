//! Крипке с отслеживанием данных - **абстракция по формуле**.
//!
//! Чистая абстракция управления абстрагирует **все** данные: атом, не являющийся именем
//! состояния (`cond`, булев `var`), даёт `Verdict::Unsupported`. Здесь атом-предикат
//! получает **вердикт**: вершина Крипке становится парой `(состояние, оценка
//! отслеживаемых переменных)`.
//!
//! # Что "отслеживается"
//!
//! Только переменные, **встречающиеся в φ**: переменные `cond`- атомов и булевы
//! `var`-атомы. Всё прочее - переменные вне φ, входные порты, арифметика над ними -
//! остаётся абстрагированным.
//!
//! # Как эволюционируют данные (консервативное ядро)
//!
//! **Максимальная сверх-аппроксимация:** отслеживаемая переменная на каждом
//! такте вольна принять **любое** значение своего домена (переход по данным -
//! полностью недетерминированный). Разметка атомов при этом **точна**: в каждой
//! вершине предикат вычисляется по её оценке.
//!
//! Так держится главная гарантия абстракции: реальный прогон присутствует в ней с
//! точными метками (полный недетерминизм включает ход реальных данных), поэтому
//! `Holds` надёжен. Точное вычисление хода данных - доказывающее `G (temp <= 100)`,
//! когда логика ограничивает `temp`, - намеренно не реализовано: неверный ход данных
//! сломал бы `Holds`, то есть ровно ту гарантию, ради которой абстракция и заведена.
//!
//! # Потолок
//!
//! Размер считается до построения; превышение [`EDGE_LIMIT`] даёт `Unsupported`, а не
//! долгий счёт. `float`, `q` и прочие неперечислимые типы - тоже `Unsupported`.
//!
//! Меряются **рёбра**, а не вершины:
//!
//! ```text
//! вершин: V = V_упр x D          - линейно по D
//! рёбер:  E = E_упр x D²         - квадратично по D
//! ```
//!
//! где `D` - произведение доменов отслеживаемых переменных. Квадрат берётся оттуда,
//! что управляющее ребро скрещивается с любой оценкой: ход данных недетерминирован.
//! Работа (произведение с автоматом Бюхи и nested DFS) идёт по рёбрам, поэтому потолок
//! по вершинам стоимости не предсказывает - два входа одинакового числа вершин
//! различаются по времени на три порядка.

use super::kripke::Kripke;
use super::verify::UnsupportedReason;
use crate::semantic::type_node::TypeNode;
use crate::semantic::{ConditionNode, ExpressionNode, ModelNode, VariableNode};
use std::collections::{BTreeMap, BTreeSet};

/// Потолок числа рёбер Крипке с данными.
///
/// Ребро стоит доли микросекунды - разброс задаёт размер автомата Бюхи, - и порог
/// держит проверку в пределах пары секунд. Для сравнения: `comprehensive`/`Controller`
/// с одной переменной `u8` даёт 8 x 256² = 524 288 рёбер, а формула над двумя `u8` -
/// больше 8.6 x 10⁹, то есть `Unsupported`. Значение инженерное, не физическое:
/// увеличивать его можно вместе с замером времени полной проверки, а не одного
/// построения.
pub const EDGE_LIMIT: u128 = 1_000_000;

/// Строит Крипке с отслеживанием данных для формулы с атомами `atoms`, взяв управляющий
/// граф из уже построенной `control`.
///
/// Вызывается только когда среди атомов есть не-имя-состояния; иначе работает путь
/// абстракции управляющего графа. Возвращает:
///
/// - `Ok(kripke)` - построена Крипке `(состояние x оценка)` с точной разметкой;
/// - `Err((atoms, reason))` - отслеживание невозможно, и причина названа: без неё CLI
///   печатал бы все пять возможных причин разом, предлагая пользователю выбрать свою.
pub fn build_data_kripke(
    model: &ModelNode,
    control: &Kripke,
    atoms: &BTreeSet<String>,
) -> Result<Kripke, (Vec<String>, UnsupportedReason)> {
    // 1. Разбор атомов: имя состояния | предикат над данными | неизвестный.
    let mut data_atoms: Vec<DataAtom> = Vec::new();
    let mut unsupported: BTreeSet<String> = BTreeSet::new();
    for atom in atoms {
        if control.states.iter().any(|s| s == atom) {
            continue; // атом-имя состояния - размечается именем вершины
        }
        match classify_data_atom(model, atom) {
            Some(da) => data_atoms.push(da),
            None => {
                unsupported.insert(atom.clone());
            }
        }
    }
    // Атом, который не состояние и не поддержанный предикат, - честный отказ (как
    // чистый путь управления на атоме-переменной).
    if !unsupported.is_empty() {
        return Err((
            unsupported.into_iter().collect(),
            UnsupportedReason::UnknownAtom,
        ));
    }

    // 2. Сбор отслеживаемых переменных + валидация подмножества предикатов.
    let mut tracked: BTreeMap<String, TrackedDecl> = BTreeMap::new();
    for da in &data_atoms {
        if !collect_tracked(model, &da.expr, &mut tracked) {
            // Предикат вне подмножества (арифметика, функция, порт, битдоступ...).
            return Err((
                vec![da.name.clone()],
                UnsupportedReason::PredicateOutsideSubset,
            ));
        }
    }

    // 3. Домены из типов + потолок до построения.
    let mut domain = 1u128; // D = Π|домен|
    for decl in tracked.values() {
        // Постоянная величина даёт домен из одного значения - она не растит задачу и не
        // порождает контрпримеров "параметр вдруг стал другим".
        if decl.fixed {
            continue;
        }
        let Some(size) = super::domain::of(&decl.ty, model).map(|d| d.size()) else {
            // float/q/массив/структура - домен не перечислим.
            return Err((
                data_atom_names(&data_atoms),
                UnsupportedReason::DomainNotEnumerable,
            ));
        };
        // Переполнение равносильно превышению потолка: считать нечего.
        let Some(d) = domain.checked_mul(size) else {
            return Err((
                data_atom_names(&data_atoms),
                UnsupportedReason::SizeOverLimit,
            ));
        };
        domain = d;
    }
    if edges_exceed_limit(control, domain) {
        return Err((
            data_atom_names(&data_atoms),
            UnsupportedReason::SizeOverLimit,
        ));
    }

    // 4. Материализация доменов (только после прохождения потолка).
    let mut order: Vec<TrackedVar> = Vec::new();
    for (name, decl) in &tracked {
        let values = if decl.fixed {
            // Домен постоянной величины - её собственное значение. Если оно не
            // сворачивается, ниже придёт `InitialValueUnknown`, как у переменной.
            fold_expr(&decl.init).map(|v| vec![v]).unwrap_or_default()
        } else {
            super::domain::of(&decl.ty, model)
                .map(|d| d.values())
                .unwrap_or_default()
        };
        let Some(init) = fold_expr(&decl.init) else {
            // Инициализатор не сворачивается в константу.
            return Err((
                data_atom_names(&data_atoms),
                UnsupportedReason::InitialValueUnknown,
            ));
        };
        let Some(init_idx) = values.iter().position(|v| *v == init) else {
            // Начальное значение вне домена своего типа.
            return Err((
                data_atom_names(&data_atoms),
                UnsupportedReason::InitialValueUnknown,
            ));
        };
        order.push(TrackedVar {
            name: name.clone(),
            values,
            init_idx,
        });
    }

    // 5. Перечисление оценок (одометр) и точная разметка предикатов по оценке.
    // Истинность предиката зависит только от оценки, не от состояния, - считаем
    // один раз на оценку.
    let num_val: usize = order
        .iter()
        .map(|v| v.values.len())
        .product::<usize>()
        .max(1);
    let mut data_true: Vec<BTreeSet<String>> = Vec::with_capacity(num_val);
    for j in 0..num_val {
        let valuation = valuation_at(&order, j);
        let mut here = BTreeSet::new();
        for da in &data_atoms {
            match eval_atom(&da.expr, &valuation) {
                Some(true) => {
                    here.insert(da.name.clone());
                }
                Some(false) => {}
                // Валидация п.2 гарантирует тотальность; None - страховка.
                None => {
                    return Err((
                        vec![da.name.clone()],
                        UnsupportedReason::PredicateOutsideSubset,
                    ));
                }
            }
        }
        data_true.push(here);
    }

    // 6. Полное произведение: вершина = состояние k x оценка j -> id = k*num_val+j.
    let num_states = control.states.len();
    let total_vertices = num_states * num_val;
    let mut states: Vec<String> = Vec::with_capacity(total_vertices);
    let mut labels: Vec<BTreeSet<String>> = Vec::with_capacity(total_vertices);
    for state_name in &control.states {
        for dt in &data_true {
            states.push(state_name.clone());
            // Разметка: имя состояния (для атомов-состояний) + истинные предикаты.
            let mut lab = dt.clone();
            lab.insert(state_name.clone());
            labels.push(lab);
        }
    }

    // Переходы: управляющее ребро k->k2 крестится с любой оценкой j2 (: ход данных
    // полностью недетерминирован - сверх-аппроксимация).
    let mut transitions: BTreeMap<usize, BTreeSet<usize>> = BTreeMap::new();
    for k in 0..num_states {
        let succ_states = control.successors(k);
        for j in 0..num_val {
            let id = k * num_val + j;
            let mut succ = BTreeSet::new();
            for &k2 in succ_states {
                for j2 in 0..num_val {
                    succ.insert(k2 * num_val + j2);
                }
            }
            transitions.insert(id, succ);
        }
    }

    // Начальная вершина: (стартовое состояние, начальная оценка из инициализаторов).
    let init_val = order
        .iter()
        .enumerate()
        .fold(0usize, |acc, (i, v)| acc + v.init_idx * stride(&order, i));
    let initial = control.initial * num_val + init_val;

    Ok(Kripke {
        states,
        initial,
        transitions,
        labels,
    })
}

/// Вид предиката-атома φ.
enum AtomExpr {
    /// Именованное условие (`cond Safe = temp <= 100;`).
    Cond(ConditionNode),
    /// Булев `var`/`const`-атом: истинен ⟺ значение переменной != 0.
    BoolVar(String),
}

/// Предикат-атом φ: имя атома и его вид.
struct DataAtom {
    name: String,
    expr: AtomExpr,
}

/// Объявление отслеживаемой переменной: тип и инициализатор.
struct TrackedDecl {
    ty: TypeNode,
    init: ExpressionNode,
    /// Значение постоянно на всём прогоне - домен из одного значения.
    ///
    /// Так трактуется **параметр модели**, которому не присваивают в теле: параметр
    /// есть величина сборки, от такта к такту он не меняется, а абстракция позволяла
    /// ему скакать каждый такт - отсюда квадрат в размере задачи и лишние контрпримеры
    /// ( проверять значение по умолчанию модели).
    fixed: bool,
}

/// Отслеживаемая переменная с материализованным доменом и индексом нач. значения.
struct TrackedVar {
    name: String,
    values: Vec<i128>,
    init_idx: usize,
}

/// Превысит ли Крипке с данными потолок [`EDGE_LIMIT`]? Считается **до** построения: `E
/// = max(E_упр, V_упр) x D²`.
///
/// `max` - не украшение. Он делает свойством **кода** то, что иначе держится
/// рассуждением: у каждого состояния валидной модели есть преемник (безусловный `ref`
/// либо самопетля `may_stutter`), поэтому `E_упр >= V_упр`, и рёберный потолок
/// мажорирует вершинный. Без `max` вырожденный граф без рёбер прошёл бы потолок с любым
/// числом вершин - и отдельный потолок по вершинам пришлось бы держать вторым знанием
/// об одном предмете.
///
/// Переполнение `u128` - тоже превышение: величина, не влезающая в 128 бит, заведомо за
/// порогом.
fn edges_exceed_limit(control: &Kripke, domain: u128) -> bool {
    let control_edges = (control.edge_count() as u128).max(control.states.len() as u128);
    let Some(square) = domain.checked_mul(domain) else {
        return true;
    };
    match control_edges.checked_mul(square) {
        Some(edges) => edges > EDGE_LIMIT,
        None => true,
    }
}

fn data_atom_names(atoms: &[DataAtom]) -> Vec<String> {
    atoms.iter().map(|a| a.name.clone()).collect()
}

/// Классифицирует не-состояние-атом: `cond` или булев `var`. Иначе `None`.
fn classify_data_atom(model: &ModelNode, atom: &str) -> Option<DataAtom> {
    if let Some(def) = model.conditions.get(atom) {
        return Some(DataAtom {
            name: atom.to_string(),
            expr: AtomExpr::Cond(def.value.clone()),
        });
    }
    // Булев `var`/`const`: атом истинен ⟺ значение != 0.
    if let Some(var) = model.variables.get(atom) {
        let ty = match var {
            VariableNode::Simple { ty, .. } | VariableNode::Const { ty, .. } => Some(ty),
            _ => None,
        };
        if matches!(ty, Some(TypeNode::Bit | TypeNode::Bool)) {
            return Some(DataAtom {
                name: atom.to_string(),
                expr: AtomExpr::BoolVar(atom.to_string()),
            });
        }
    }
    None
}

/// Собирает отслеживаемые (`Simple`) переменные атома и проверяет, что он
/// **целиком** в поддержанном подмножестве. `false` -> атом не поддержан.
fn collect_tracked(
    model: &ModelNode,
    expr: &AtomExpr,
    tracked: &mut BTreeMap<String, TrackedDecl>,
) -> bool {
    match expr {
        AtomExpr::BoolVar(name) => match model.variables.get(name) {
            Some(VariableNode::Simple { ty, expr, .. }) => {
                let fixed = is_fixed_parameter(model, name);
                tracked.entry(name.clone()).or_insert_with(|| TrackedDecl {
                    ty: ty.clone(),
                    init: expr.clone(),
                    fixed,
                });
                true
            }
            // Булева `const` - не отслеживается, но обязана сворачиваться.
            Some(VariableNode::Const { expr, .. }) => fold_expr(expr).is_some(),
            _ => false,
        },
        AtomExpr::Cond(cond) => collect_tracked_cond(cond, model, tracked),
    }
}

/// Постоянен ли `name` на прогоне: параметр модели, которому не присваивают.
///
/// Признак `mutated` заполняет анализ изменяемости, и его умолчание - "изменяемый": не
/// размеченный параметр обязан вести себя как переменная. Здесь то же самое -
/// постоянным считается только явно неизменяемый.
fn is_fixed_parameter(model: &ModelNode, name: &str) -> bool {
    model
        .parameters
        .iter()
        .any(|p| p.name == name && !p.mutated)
}

/// Обход условия: сбор `Simple`-переменных + проверка поддержанности подмножества.
fn collect_tracked_cond(
    cond: &ConditionNode,
    model: &ModelNode,
    tracked: &mut BTreeMap<String, TrackedDecl>,
) -> bool {
    match cond {
        ConditionNode::Bool(_) | ConditionNode::Number(_) | ConditionNode::EnumVariant(..) => true,
        ConditionNode::Parenthesis(c) | ConditionNode::Not(c) => {
            collect_tracked_cond(c, model, tracked)
        }
        ConditionNode::And(l, r)
        | ConditionNode::Or(l, r)
        | ConditionNode::Less(l, r)
        | ConditionNode::More(l, r)
        | ConditionNode::LessEqual(l, r)
        | ConditionNode::MoreEqual(l, r)
        | ConditionNode::Equal(l, r)
        | ConditionNode::NotEqual(l, r) => {
            collect_tracked_cond(l, model, tracked) && collect_tracked_cond(r, model, tracked)
        }
        ConditionNode::Variable(rc, _) => {
            let borrowed = rc.borrow();
            match &*borrowed {
                VariableNode::Simple { name, ty, expr, .. } => {
                    // Отслеживаем ровно тот тип, значения которого можно перебрать:
                    // вопрос один, и носитель у него один.
                    if super::domain::of(ty, model).is_none() {
                        return false;
                    }
                    let fixed = is_fixed_parameter(model, name);
                    tracked.entry(name.clone()).or_insert_with(|| TrackedDecl {
                        ty: ty.clone(),
                        init: expr.clone(),
                        fixed,
                    });
                    true
                }
                // Константа - не отслеживается, но обязана сворачиваться.
                VariableNode::Const { expr, .. } => fold_expr(expr).is_some(),
                // Порт - среда, не данные состояния.
                _ => false,
            }
        }
        // Арифметика, функции, битдоступ, срезы, Unresolved - вне подмножества.
        _ => false,
    }
}

/// Оценка `j` как отображение имя -> значение (одометр по [`TrackedVar`]).
fn valuation_at(order: &[TrackedVar], j: usize) -> BTreeMap<String, i128> {
    let mut out = BTreeMap::new();
    for (i, v) in order.iter().enumerate() {
        let idx = (j / stride(order, i)) % v.values.len();
        out.insert(v.name.clone(), v.values[idx]);
    }
    out
}

/// Шаг одометра для переменной `i` (произведение размеров правее неё).
fn stride(order: &[TrackedVar], i: usize) -> usize {
    order[i + 1..]
        .iter()
        .map(|v| v.values.len())
        .product::<usize>()
        .max(1)
}

/// Свёртка выражения-инициализатора/константы в целое. `None` - не константа.
fn fold_expr(expr: &ExpressionNode) -> Option<i128> {
    match expr {
        ExpressionNode::Number(n) => Some(*n),
        ExpressionNode::Bool(b) => Some(i128::from(*b)),
        ExpressionNode::Parenthesis(inner) | ExpressionNode::UnaryPlus(inner) => fold_expr(inner),
        ExpressionNode::Negate(inner) => fold_expr(inner).map(|v| v.wrapping_neg()),
        _ => None,
    }
}

/// Булева оценка атома по конкретной оценке переменных.
fn eval_atom(expr: &AtomExpr, val: &BTreeMap<String, i128>) -> Option<bool> {
    match expr {
        AtomExpr::BoolVar(name) => val.get(name).map(|v| *v != 0),
        AtomExpr::Cond(cond) => eval_bool(cond, val),
    }
}

/// Булева оценка условия по конкретной оценке переменных. `None` - предикат вне
/// поддержанного подмножества (страховка; валидация п.2 это исключает).
fn eval_bool(cond: &ConditionNode, val: &BTreeMap<String, i128>) -> Option<bool> {
    match cond {
        ConditionNode::Bool(b) => Some(*b),
        ConditionNode::Not(c) => eval_bool(c, val).map(|x| !x),
        ConditionNode::Parenthesis(c) => eval_bool(c, val),
        // `&`/`|` над булевыми операндами (0/1) - побитовое совпадает с логическим.
        ConditionNode::And(l, r) => Some(eval_bool(l, val)? && eval_bool(r, val)?),
        ConditionNode::Or(l, r) => Some(eval_bool(l, val)? || eval_bool(r, val)?),
        ConditionNode::Less(l, r) => Some(eval_num(l, val)? < eval_num(r, val)?),
        ConditionNode::More(l, r) => Some(eval_num(l, val)? > eval_num(r, val)?),
        ConditionNode::LessEqual(l, r) => Some(eval_num(l, val)? <= eval_num(r, val)?),
        ConditionNode::MoreEqual(l, r) => Some(eval_num(l, val)? >= eval_num(r, val)?),
        ConditionNode::Equal(l, r) => Some(eval_num(l, val)? == eval_num(r, val)?),
        ConditionNode::NotEqual(l, r) => Some(eval_num(l, val)? != eval_num(r, val)?),
        // Булева переменная как самостоятельный операнд: != 0.
        ConditionNode::Variable(rc, _) => {
            let borrowed = rc.borrow();
            match &*borrowed {
                VariableNode::Simple { name, .. } => val.get(name).map(|v| *v != 0),
                VariableNode::Const { expr, .. } => fold_expr(expr).map(|v| v != 0),
                _ => None,
            }
        }
        _ => None,
    }
}

/// Числовая оценка терма по оценке переменных. `None` - вне подмножества.
fn eval_num(cond: &ConditionNode, val: &BTreeMap<String, i128>) -> Option<i128> {
    match cond {
        ConditionNode::Number(n) => Some(*n),
        ConditionNode::Bool(b) => Some(i128::from(*b)),
        ConditionNode::EnumVariant(_, _, v) => Some(*v),
        ConditionNode::Parenthesis(c) => eval_num(c, val),
        ConditionNode::Variable(rc, _) => {
            let borrowed = rc.borrow();
            match &*borrowed {
                VariableNode::Simple { name, .. } => val.get(name).copied(),
                VariableNode::Const { expr, .. } => fold_expr(expr),
                _ => None,
            }
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic::ltl_check::collect_atoms;
    use crate::semantic::tree::construct_model;
    use crate::verification::verify::{Verdict, verify_model};
    use crate::{parse, parse_ltl_property};

    /// Строит Крипке данных для φ над моделью `src` (или `Err`, если не вышло).
    fn data_kripke_of(
        src: &str,
        phi_src: &str,
    ) -> Result<Kripke, (Vec<String>, UnsupportedReason)> {
        let (ast, _) = parse(src, 0).unwrap();
        let model = construct_model(&ast, None, &[]).unwrap();
        let m = model.borrow();
        let control = super::super::kripke::build_kripke(&m).unwrap();
        let phi = parse_ltl_property(phi_src).unwrap();
        let mut atoms = BTreeSet::new();
        collect_atoms(&phi, &mut atoms);
        build_data_kripke(&m, &control, &atoms)
    }

    fn verdict(src: &str, phi_src: &str) -> Verdict {
        let (ast, _) = parse(src, 0).unwrap();
        let model = construct_model(&ast, None, &[]).unwrap();
        let phi = parse_ltl_property(phi_src).unwrap();
        verify_model(&model.borrow(), &phi)
    }

    /// Предикат над `u8` даёт вершины, а не `Unsupported`: размер - |состояний| x 256.
    #[test]
    fn u8_predicate_yields_state_times_256_vertices() {
        // 2 состояния, один отслеживаемый `u8` -> 2 x 256 = 512 вершин.
        let src = "var temp: u8 := 0; \
                   cond Hot = temp >= 100; \
                   start A { ref B; } state B { ref A; }";
        let k = data_kripke_of(src, "G !Hot").expect("предикат над u8 отслеживается");
        assert_eq!(k.states.len(), 2 * 256, "вершин = состояний × домен(u8)");
        assert!(!k.labels.is_empty(), "путь данных: разметка непуста");
    }

    /// Свойство над данными получает вердикт, а не `Unsupported`.
    #[test]
    fn data_property_gets_a_verdict_not_unsupported() {
        let src = "var temp: u8 := 0; \
                   cond Hot = temp >= 100; \
                   start A { ref B; } state B { ref A; }";
        let v = verdict(src, "G !Hot");
        assert!(
            !matches!(v, Verdict::Unsupported { .. }),
            "предикат над данными обязан дать вердикт, получено {v:?}"
        );
    }

    /// Гарантия "держится" сохранена: тавтология над доменом `u8` (`temp <= 255`
    /// истинно при всех 256 значениях) даёт `Holds`, несмотря на полный недетерминизм
    /// данных. Проверяется вердикт, а не форма автомата.
    #[test]
    fn tautology_over_domain_holds() {
        let src = "var temp: u8 := 0; \
                   cond InRange = temp <= 255; \
                   start A { ref A; }";
        assert_eq!(
            verdict(src, "G InRange"),
            Verdict::Holds,
            "temp<=255 истинно при всех значениях u8 — свойство держится"
        );
    }

    /// Булева переменная тоже ходит по всему домену, но тавтология `G (Safe | !Safe)`
    /// держится.
    #[test]
    fn excluded_middle_holds() {
        let src = "var temp: u8 := 5; \
                   cond Hi = temp >= 200; \
                   start A { ref A; }";
        assert_eq!(verdict(src, "G (Hi | !Hi)"), Verdict::Holds);
    }

    /// Свойство, ложное при некоторой достижимой оценке, даёт `Violated` - возможно,
    /// ложный: это цена сверх-аппроксимации. Здесь `temp` ходит по всему домену, значит
    /// `temp >= 200` достижимо.
    #[test]
    fn reachable_false_valuation_violates() {
        let src = "var temp: u8 := 0; \
                   cond Hi = temp >= 200; \
                   start A { ref A; }";
        let v = verdict(src, "G !Hi");
        assert!(
            matches!(v, Verdict::Violated(_)),
            "temp вольна достичь 200 (недетерминизм данных) → нарушение, получено {v:?}"
        );
    }

    /// Направление ошибки: переход или значение из абстрагированного источника даёт
    /// лишний прогон, а не пропущенный. Модель, где `temp` меняется только по входу
    /// (мы его не отслеживаем), обязана допускать оценку, при которой предикат ложен ->
    /// `Violated`. Мутация ("невычислимое - ложно/заморожено") сделала бы `temp`
    /// неизменным (=0) и дала бы ложный `Holds` - то есть тест валился бы при мутации.
    #[test]
    fn abstracted_source_yields_extra_run_not_missed() {
        // temp инициализирована 0; при "заморозке" temp=0 всегда, `!Hot` держится
        // ложно. При верном направлении ошибки temp роумит -> Hot достижим -> Violated.
        let src = "in sensor: u8; \
                   var temp: u8 := 0; \
                   cond Hot = temp >= 100; \
                   start A { ref A; }";
        let v = verdict(src, "G !Hot");
        assert!(
            matches!(v, Verdict::Violated(_)),
            "лишний прогон (temp достигает 100) обязан быть, получено {v:?}"
        );
    }

    /// Потолок: φ над тремя `u8` (рёбер `1 x (256³)² ~ 2.8 x 10^14`) даёт
    /// `Unsupported` сразу, потому что размер считается до построения.
    #[test]
    fn three_u8_exceeds_ceiling_unsupported() {
        let src = "var a: u8 := 0; var b: u8 := 0; var c: u8 := 0; \
                   cond P = a <= b & b <= c; \
                   start A { ref A; }";
        let v = verdict(src, "G P");
        // Причина названа вердиктом: без неё "не проверено" приходило одним сообщением
        // со списком всех причин, а на этом самом входе первая строка утверждала "атом
        // не отслеживаемый" - ложь.
        assert!(
            matches!(
                v,
                Verdict::Unsupported {
                    reason: UnsupportedReason::SizeOverLimit,
                    ..
                }
            ),
            "три u8 превышают потолок — ожидался SizeOverLimit, получено {v:?}"
        );
    }

    /// Вход, дешёвый по вершинам и неподъёмный по рёбрам, отвергается потолком.
    ///
    /// Двенадцать `bit`-переменных при трёх состояниях: вершин `3 x 4096 = 12 288` -
    /// вчетверо ниже миллиона, а рёбер `3 x 4096^2 ~ 5 x 10^7`, и прогон на них не
    /// заканчивается за минуту. Потолок по вершинам такой вход пропускает.
    ///
    /// Тест обязан быть именно таким входом. На трёх `u8` (соседний тест) отказом
    /// отвечают обе метрики, и вход, взявший их, ничего бы не различал.
    ///
    /// При регрессе этот тест **не падает быстро - он виснет** (проверено мутацией
    /// "считать `D` вместо `D²`": прогон шёл более 10 минут в debug). Быстрый сигнал
    /// даёт соседний `ceiling_counts_edges_not_vertices`, и удалять его как
    /// "дублирующий" нельзя: он проверяет ту же формулу, но падением, а не терпением.
    #[test]
    fn low_vertex_count_but_edge_explosion_is_unsupported() {
        let mut src = String::new();
        for i in 0..12 {
            src.push_str(&format!("var b{i}: bit := 0; cond H{i} = b{i} = 1; "));
        }
        src.push_str("start A { ref B; } state B { ref C; } state C { ref A; }");
        let atoms: Vec<String> = (0..12).map(|i| format!("H{i}")).collect();
        let phi = format!("G ({})", atoms.join(" & "));

        // Вершин мало - прежняя метрика этот вход принимала.
        let vertices = 3u128 * 4096;
        assert!(
            vertices < 1_000_000,
            "вход обязан проходить вершинный потолок"
        );

        let started = std::time::Instant::now();
        let v = verdict(&src, &phi);
        assert!(
            matches!(v, Verdict::Unsupported { .. }),
            "рёбер ≈ 5 × 10⁷ — ожидался Unsupported, получено {v:?}"
        );
        // Грубый предохранитель, а не порог: отказ обязан прийти до построения, а
        // построение этого графа занимает минуты.
        assert!(
            started.elapsed() < std::time::Duration::from_secs(5),
            "отказ пришёл не до построения: {:?}",
            started.elapsed()
        );
    }

    /// Потолок считает `max(E_упр, V_упр) x D²`.
    ///
    /// Формула проверяется на границе - по обе её стороны при одном и том же графе
    /// управления: домен `2^k` подобран так, что `E x D²` перескакивает `EDGE_LIMIT`.
    #[test]
    fn ceiling_counts_edges_not_vertices() {
        let control = super::super::kripke::build_kripke(
            &construct_model(
                &parse("start A { ref B; } state B { ref A; }", 0).unwrap().0,
                None,
                &[],
            )
            .unwrap()
            .borrow(),
        )
        .unwrap();
        assert_eq!(control.edge_count(), 2, "A → B, B → A");

        // 2 x D² <= 10⁶ ⟺ D <= 707: домен 512 проходит, 1024 - нет.
        assert!(!edges_exceed_limit(&control, 512));
        assert!(edges_exceed_limit(&control, 1024));
        // Переполнение u128 - тоже превышение, а не паника.
        assert!(edges_exceed_limit(&control, u128::MAX));
    }

    /// Корпусной вход остаётся проверяемым: 8 x 256² = 524 288 рёбер против порога
    /// в миллион.
    #[test]
    fn corpus_controller_still_fits_the_ceiling() {
        let path = concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../examples/comprehensive.takt"
        );
        let src = std::fs::read_to_string(path).expect("examples/comprehensive.takt");
        let (ast, _) = parse(&src, 0).unwrap();
        let root = construct_model(&ast, None, &[]).unwrap();
        let controller = root.borrow().models.get("Controller").unwrap().clone();
        let c = controller.borrow();
        let control = super::super::kripke::build_kripke(&c).unwrap();
        assert!(
            !edges_exceed_limit(&control, 256),
            "рёбер {} × 256² — корпусной вход обязан проходить",
            control.edge_count()
        );
    }

    /// `float` в предикате даёт `Unsupported`.
    ///
    /// Причина - `PredicateOutsideSubset`, а не `DomainNotEnumerable`: неперечислимый
    /// тип отсекается раньше, на шаге 2, - `collect_tracked` не пускает такой предикат
    /// в отслеживаемые. Тем же путём уходят `q`, массив и структура.
    #[test]
    fn float_predicate_is_unsupported() {
        let src = "var x: float := 0.0; \
                   cond P = x <= 1.0; \
                   start A { ref A; }";
        let v = verdict(src, "G P");
        assert!(
            matches!(
                v,
                Verdict::Unsupported {
                    reason: UnsupportedReason::PredicateOutsideSubset,
                    ..
                }
            ),
            "float отсекается подмножеством предикатов — ожидался \
             PredicateOutsideSubset, получено {v:?}"
        );
    }

    /// Атом-переменная типа `bit` даёт домен из двух значений: |состояний| x 2 вершин.
    #[test]
    fn bit_variable_atom_domain_is_two() {
        // 3 состояния, один `bit` -> 3 x 2 = 6 вершин.
        let src = "var flag: bit := false; \
                   start A { ref B; } state B { ref C; } state C { ref A; }";
        let k = data_kripke_of(src, "G flag").expect("булев var-атом отслеживается");
        assert_eq!(k.states.len(), 3 * 2, "вершин = состояний × домен(bit)");
    }

    /// Неизвестный атом (не состояние, не cond, не булев var) -> `Unsupported` с его
    /// именем - как в чистом пути управления.
    #[test]
    fn unknown_atom_is_unsupported() {
        let src = "start A { ref A; }";
        let v = verdict(src, "G nosuch");
        assert!(
            matches!(
                v,
                Verdict::Unsupported { ref atoms, reason: UnsupportedReason::UnknownAtom }
                    if *atoms == vec!["nosuch".to_string()]
            ),
            "опечатка в имени атома — ожидался UnknownAtom, получено {v:?}"
        );
    }

    /// Начальная вершина несёт оценку инициализаторов: `temp := 7` -> в стартовой
    /// вершине `temp = 7`, поэтому `Hot7` (temp = 7) там истинно.
    #[test]
    fn initial_vertex_carries_initializer_valuation() {
        let src = "var temp: u8 := 7; \
                   cond Is7 = temp = 7; \
                   start A { ref A; }";
        let k = data_kripke_of(src, "G Is7").expect("отслеживается");
        assert!(
            k.labels[k.initial].contains("Is7"),
            "в стартовой вершине temp=7, значит Is7 истинно: {:?}",
            k.labels[k.initial]
        );
    }

    /// Корпусной пример: `cond Overheated = temperature >= MAX_TEMP` (`u8` и
    /// константа) над моделью `Controller` даёт вершины - состояний x 256, а не
    /// `Unsupported`. У `Controller` четыре состояния, то есть 1024 вершины.
    #[test]
    fn comprehensive_controller_u8_predicate_is_tracked() {
        let path = concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../examples/comprehensive.takt"
        );
        let src = std::fs::read_to_string(path).expect("examples/comprehensive.takt");
        let (ast, _) = parse(&src, 0).unwrap();
        let root = construct_model(&ast, None, &[]).unwrap();
        let controller = root.borrow().models.get("Controller").unwrap().clone();
        let c = controller.borrow();
        let control = super::super::kripke::build_kripke(&c).unwrap();
        let n_states = control.states.len();
        let phi = parse_ltl_property("G !Overheated").unwrap();
        let mut atoms = BTreeSet::new();
        collect_atoms(&phi, &mut atoms);
        let k = build_data_kripke(&c, &control, &atoms)
            .expect("предикат над u8 корпусной модели отслеживается");
        assert_eq!(
            k.states.len(),
            n_states * 256,
            "вершин = состояний Controller ({n_states}) × домен(u8)"
        );
    }
}
