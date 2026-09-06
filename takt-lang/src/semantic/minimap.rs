use crate::diagnostics::Diagnostic;
use crate::semantic::extend::{Extend, ParameterArgument};
use crate::semantic::naming::{normalize_camelcase_name, normalize_lowercase_snakecase};
use crate::semantic::{ModelNode, StateNode};
use itertools::Itertools;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;

/// Имя элемента модели с локальной и уникальной формами.
///
/// - `local` - имя без пути родителей (например, `"State"`).
/// - `unique` - полный путь с разделителем `':'` (например, `"Root:Child:State"`).
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Name {
    local: String,
    unique: String,
}

impl Name {
    /// Конструктор имени (`pub(crate)` - нужен тестам генератора).
    pub(crate) fn new(local: String, unique: String) -> Self {
        Name { local, unique }
    }

    /// Возвращает локальное имя (без пути родителей).
    #[allow(dead_code)]
    pub fn local(&self) -> &str {
        &self.local
    }
    /// Возвращает локальное имя в `snake_case` (например, `"my_state"`).
    pub fn local_lowercase_snakecase(&self) -> String {
        normalize_lowercase_snakecase(self.local.clone())
    }

    /// Возвращает уникальное имя с разделителем `':'` (например, `"Root:Child:State"`).
    pub fn unique(&self) -> &str {
        &self.unique
    }

    /// Возвращает уникальное имя в `snake_case` с `'_'` вместо `':'`.
    pub fn unique_lowercase_snakecase(&self) -> String {
        normalize_lowercase_snakecase(self.unique.replace(":", "_"))
    }

    /// Возвращает уникальное имя в `UPPER_SNAKE_CASE`.
    pub fn unique_uppercase_snakecase(&self) -> String {
        self.unique_lowercase_snakecase().to_uppercase()
    }

    /// Возвращает уникальное имя в `CamelCase`.
    pub fn unique_camelcase(&self) -> String {
        normalize_camelcase_name(&self.unique.replace(":", "_"))
    }
}

/// Порядок имён - по паре `(unique, local)`, первичный ключ `unique`.
///
/// Ручная реализация (а не `derive`): вывод `derive` шёл бы по `local` первым - по
/// порядку объявления полей, - что разошлось бы с конвенцией остального кода,
/// сортирующего по `unique()` (`st/mod.rs`, `st_model.rs`). Сравнение обоих полей
/// согласовано с `Eq` - требование `BTreeMap`. Первичный ключ `unique` (полный путь
/// `Root:Child:State`) группирует элементы по родителям.
impl Ord for Name {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.unique
            .cmp(&other.unique)
            .then_with(|| self.local.cmp(&other.local))
    }
}

impl PartialOrd for Name {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Display for Name {
    /// Отображает имя в формате `"Local (Parent:Child)"`.
    ///
    /// Каждый сегмент уникального пути приводится к CamelCase через
    /// [`normalize_camelcase_name`], чтобы `extend_complex:C` отображалось как
    /// `ExtendComplex:C`, а не `Extend_complex:C`.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} ({})",
            self.local,
            self.unique
                .split(':')
                .map(normalize_camelcase_name)
                .join(":")
        )
    }
}

impl Debug for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

impl From<Rc<RefCell<ModelNode>>> for Name {
    fn from(model: Rc<RefCell<ModelNode>>) -> Self {
        let local_name = model.borrow().name.clone().unwrap_or_default();
        Name::new(local_name, unique_model_name(model.clone()))
    }
}

/// Структура реализации состояния (`= Expr`), представленная в плоском виде.
// `Eq` снят: аргументы инстанцирования несут дробное значение (`ExpressionNode` с `f64`
// внутри), для которого полного равенства не существует.
#[derive(Debug, Clone, PartialEq)]
pub enum StateExtend {
    /// Реализация отсутствует или не разрешена.
    None,
    /// Ссылка на модель с указанным именем и **аргументами инстанцирования**: значения
    /// параметров этого экземпляра, уже вычисленные константным вычислителем. Пустой
    /// вектор - вызов без аргументов.
    Model(Name, Vec<ParameterArgument>),
    /// Последовательная конкатенация нескольких реализаций (`A ; B`).
    Concatenation(Vec<StateExtend>),
    /// Параллельная композиция нескольких реализаций (`A | B`).
    Parallel(Vec<StateExtend>),
}

/// Элемент карты модели: модель, состояние с extend или чистое состояние.
///
/// Поля `start`, `name`, `next`, `references` хранятся для будущей генерации
/// `.c`-источника (I1-I4) и не читаются напрямую в текущей реализации.
#[allow(dead_code)]
// `Eq` снят вслед за `StateExtend` (см. выше): элемент карты несёт реализацию.
#[derive(Debug, Clone, PartialEq)]
pub enum Element {
    /// Вложенная модель со списком состояний и именем стартового состояния.
    Model {
        /// Имя модели.
        name: Name,
        /// Все состояния модели.
        states: Vec<Name>,
        /// Стартовое состояние.
        start: Name,
    },
    /// Состояние с объявлением extend (`=`) и ссылкой на следующее состояние.
    StateExtend {
        /// Имя состояния.
        name: Name,
        /// Дерево реализации состояния.
        extend: StateExtend,
        /// Следующее состояние после завершения реализации.
        next: Name,
    },
    /// Простое состояние со списком переходов.
    State {
        /// Имя состояния.
        name: Name,
        /// Список имён состояний, в которые ведут исходящие переходы.
        references: Vec<Name>,
        /// Завершает ли состояние автомат (`semantic::terminal`).
        ///
        /// Хранится признаком, а не выводится из пустоты списка: состояние с телом
        /// переходов не требует и автомат не завершает, а карта тела уже не помнит - на
        /// ней видны только имена.
        terminal: bool,
    },
}

impl Element {
    pub(crate) fn is_state(&self) -> bool {
        matches!(self, Element::State { .. } | Element::StateExtend { .. })
    }

    pub(crate) fn name(&self) -> Name {
        match self {
            Element::Model { name, .. }
            | Element::StateExtend { name, .. }
            | Element::State { name, .. } => name.clone(),
        }
    }
}

impl Display for Element {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Element::Model { name, .. }
                | Element::StateExtend { name, .. }
                | Element::State { name, .. } => name,
            }
        )
    }
}

/// Снимок семантической карты модели: все достижимые элементы, имена состояний.
///
/// Поле `start` и методы `start()`/`own()` зарезервированы для генератора `.c`-файла
/// (I1-I4).
#[derive(Debug)]
pub struct Map {
    root: Rc<RefCell<ModelNode>>,
    elements: BTreeMap<Name, Element>,
    model: Element,
}

impl Map {
    /// Строит снимок модели: находит стартовое состояние и рекурсивно обходит все
    /// достижимые из него состояния и реализации.
    ///
    /// Должна вызываться после полного семантического разрешения модели.
    ///
    /// # Ошибки
    /// Возвращает [`Diagnostic`], если у модели нет стартового состояния.
    pub fn create(model: Rc<RefCell<ModelNode>>) -> Result<Self, Diagnostic> {
        let mut used_elements = BTreeMap::new();
        let Some(start) = model.borrow().get_start_state() else {
            return Err(Diagnostic::error(
                model.borrow().loc,
                "Модель должна содержать начальное состояние".to_string(),
            )
            .with_code("SE-011"));
        };
        let local_name = start.name().to_string();
        // Рекурсивный спуск: обходим все достижимые состояния начиная со стартового
        visit_state(&start, model.clone(), &mut used_elements);
        let mut states = Vec::new();
        model.borrow().states.iter().for_each(|state| {
            states.push(Name::new(
                state.0.clone(),
                unique_state_name(state.0, model.clone()),
            ));
        });
        let model_name = Name::from(model.clone());
        let start_state = Name::new(
            local_name.clone(),
            unique_state_name(&local_name, model.clone()),
        );

        Ok(Map {
            root: model.clone(),
            elements: used_elements,
            model: Element::Model {
                name: model_name,
                states,
                start: start_state,
            },
        })
    }

    /// Возвращает элемент корневой модели (вариант [`Element::Model`]).
    pub fn model(&self) -> Element {
        self.model.clone()
    }

    /// Возвращает [`ModelNode`] по уникальному имени.
    ///
    /// Если `name` равно `None`, возвращает корневую модель.
    #[inline]
    pub fn model_at(&self, name: Option<String>) -> Option<Rc<RefCell<ModelNode>>> {
        if let Some(name) = name {
            return model_by_unique_name(&name, self.root.clone());
        }
        Some(self.root.clone())
    }

    /// Возвращает [`StateNode`] по уникальному имени вида `"Model:State"`.
    ///
    /// Возвращает `None` если имя не передано или состояние не найдено.
    #[inline]
    pub fn state_at(&self, name: Option<String>) -> Option<Rc<RefCell<StateNode>>> {
        if let Some(name) = name {
            return state_by_unique_name(&name, self.root.clone());
        }
        None
    }

    /// Возвращает все элементы карты, являющиеся вложенными моделями.
    #[inline]
    pub fn used_models(&self) -> Vec<Element> {
        self.elements
            .values()
            .filter(|e| matches!(e, Element::Model { .. }))
            .cloned()
            .collect::<Vec<Element>>()
    }

    /// Возвращает элемент карты по имени или `None`, если он не найден.
    pub fn element_at(&self, name: Name) -> Option<Element> {
        self.elements.get(&name).cloned()
    }

    /// Возвращает элемент корневой модели. Зарезервировано для генератора `.c` (I1).
    #[allow(dead_code)]
    pub(crate) fn own(&self) -> Option<Element> {
        self.elements.get(&self.model.name()).cloned()
    }

    pub(crate) fn root_name(&self) -> Name {
        self.model.name().clone()
    }

    /// Возвращает список имён всех состояний корневой модели.
    pub fn states(&self) -> Vec<Name> {
        let Element::Model { states, .. } = self.model.clone() else {
            unreachable!()
        };
        states
    }
}

fn unique_model_name(model: Rc<RefCell<ModelNode>>) -> String {
    let name = model.borrow().name.clone().unwrap_or_default();
    if let Some(upper) = model.borrow().upper.as_ref() {
        // Weak может быть невалиден, если родительский Rc уже дропнут (например, для
        // моделей из импортированных файлов). В этом случае используем локальное имя
        // без преисправления.
        if let Some(parent) = upper.upgrade() {
            let model_name = unique_model_name(parent);
            if model_name.is_empty() {
                return name;
            }
            return format!("{}:{}", model_name, name);
        }
    }
    name
}

fn unique_state_name(local_name: &str, model: Rc<RefCell<ModelNode>>) -> String {
    let name = unique_model_name(model);
    if name.is_empty() {
        return local_name.to_string();
    }
    format!("{}:{}", name, local_name)
}

/// Ищет модель по уникальному имени вида `"Root:Child:Grandchild"`.
///
/// Рекурсивно отсекает первый сегмент, разделённый `':'`:
/// - если текущая модель совпадает с преисправлением - углубляемся внутрь неё;
/// - иначе - ищем дочернюю модель с именем-преисправлением и рекурсируем в неё.
///
/// Работает с именами произвольной длины.
fn model_by_unique_name(
    model_name: &str,
    owned: Rc<RefCell<ModelNode>>,
) -> Option<Rc<RefCell<ModelNode>>> {
    if let Some(index) = model_name.find(':') {
        let (prefix, rest) = model_name.split_at(index);
        let rest = &rest[1..]; // Отсекаем разделитель ':'
        if owned.borrow().name() == prefix {
            // Текущий узел совпадает с преисправлением - рекурсируем внутрь
            return model_by_unique_name(rest, owned);
        } else if let Some(child) = owned.borrow().search_model(prefix) {
            // Нашли дочернюю модель - рекурсируем в неё
            return model_by_unique_name(rest, child);
        }
    } else {
        // Последний сегмент без ':'.
        //
        // Порядок: сперва прямой ребёнок, и лишь затем "это сам узел". Обратный порядок
        // ломался ровно на совпадении имён: файл `helper.takt` вносится импортёром как
        // модель `Helper`, а внутри него объявлена модель с тем же именем - путь
        // `App:Helper:Helper` возвращал обёртку, у которой искомого состояния нет, и
        // три цели отвечали "Состояние ... не найдено" на корректной программе.
        //
        // `search_model` поднимается по `upper`, поэтому им нельзя подменить проверку
        // прямого ребёнка: у обёртки он нашёл бы её саму в словаре родителя - тот же
        // неверный ответ.
        if let Some(child) = owned.borrow().models.get(model_name) {
            return Some(Rc::clone(child));
        }
        if owned.borrow().name() == model_name {
            return Some(owned);
        }
        if let Some(model) = owned.borrow().search_model(model_name) {
            return Some(model);
        }
    }
    None
}

fn state_by_unique_name(
    state_name: &str,
    owned: Rc<RefCell<ModelNode>>,
) -> Option<Rc<RefCell<StateNode>>> {
    if let Some(index) = state_name.rfind(':') {
        let (model_name, rest) = state_name.split_at(index);
        let state_name = &rest[1..];
        let model = model_by_unique_name(model_name, owned.clone())?;
        return model.borrow().search_state(state_name);
    } else if let Some(state) = owned.borrow().search_state(state_name) {
        return Some(state);
    }
    None
}

/// Обходит состояние и все достижимые из него по ссылкам и `next`-переходу. Добавляет
/// найденные элементы в `used`.
///
/// # Обход итеративный, а не рекурсивный
///
/// Глубина обхода равна **числу состояний**: цепочка `S0 -> S1 -> ... -> S(N-1)` даёт N
/// вложенных шагов. Рекурсия исчерпывала стек на N ≈ 2500 (debug) - а карту строит
/// **каждый** генератор (`c_map`, `puml_map`, `rust_map`, `st_map`, `sv_map`), поэтому
/// падала компиляция во все пять целей, ещё до печати текста. Отказ был без диагностики -
/// `SIGABRT`, без строки и кода.
///
/// Перевод дался механически: после шага "вглубь" работы не остаётся, возврата никто не
/// читает - то есть это предпорядок, и стек кадров заменяется списком задач в куче.
/// Заодно исчез толстый кадр: рекурсия держала в нём `StateNode::clone()` - полную
/// копию узла.
///
/// # Защита от циклов
/// Перед обходом ссылок в `used` вставляется заглушка с ключом состояния;
/// повторный вход отсекает проверка `used.contains_key(&key)`. Она же - страж от
/// самоссылок. Глубину она **не** ограничивает: на ациклической цепочке каждый
/// ключ новый.
fn visit_state(
    state: &StateNode,
    model: Rc<RefCell<ModelNode>>,
    used: &mut BTreeMap<Name, Element>,
) {
    // Список задач вместо стека кадров. `pop()` берёт с конца, поэтому потомки кладутся
    // в обратном порядке - обход идёт ровно в том же порядке, что и прежняя рекурсия.
    // По существу порядок не наблюдаем (`used` - BTreeMap, а элемент определяется самим
    // состоянием), но так правка проверяется диффом вывода, а не рассуждением о нём.
    let mut worklist: Vec<StateNode> = vec![state.clone()];

    while let Some(state) = worklist.pop() {
        let name_str = state.name().to_string();
        let key = Name::new(
            name_str.clone(),
            unique_state_name(&name_str, model.clone()),
        );
        if used.contains_key(&key) {
            continue; // Уже обработано - прерываем возможный цикл
        }
        match &state {
            StateNode::Simple { references, .. } => {
                let ref_names: Vec<Name> = references
                    .iter()
                    .map(|r| Name::new(r.name.clone(), unique_state_name(&r.name, model.clone())))
                    .collect();
                // Регистрируем состояние до обхода ссылок (защита от самоссылок)
                used.insert(
                    key.clone(),
                    Element::State {
                        name: key,
                        references: ref_names.clone(),
                        terminal: state.is_terminated(),
                    },
                );
                // Все исходящие переходы - в список задач (в обратном порядке).
                for ref_name in ref_names.iter().rev() {
                    let next_opt = model.borrow().search_state(&ref_name.local);
                    if let Some(rc) = next_opt {
                        let next = rc.borrow().clone();
                        worklist.push(next);
                    }
                }
            }
            StateNode::Implement {
                implements, next, ..
            } => {
                let next_name = next
                    .as_ref()
                    .map(|n| Name::new(n.name.clone(), unique_state_name(&n.name, model.clone())))
                    .unwrap_or_else(|| Name::new(String::new(), String::new()));
                // Вставляем заглушку до обхода реализации (защита от циклов)
                used.insert(
                    key.clone(),
                    Element::StateExtend {
                        name: key.clone(),
                        extend: StateExtend::None,
                        next: next_name.clone(),
                    },
                );
                // `visit_extend` рекурсивен по дереву выражения `Extend` (`A | B + C`),
                // а не по числу состояний: его глубина - по вложенности, которую пишет
                // человек. Стек ему не грозит.
                visit_extend(implements, model.clone(), used);
                // Обновляем запись с реальным содержимым после обхода
                used.insert(
                    key.clone(),
                    Element::StateExtend {
                        name: key,
                        extend: build_extend(implements, model.clone()),
                        next: next_name.clone(),
                    },
                );
                // next-переход - в список задач
                if !next_name.local.is_empty() {
                    let next_opt = model.borrow().search_state(&next_name.local);
                    if let Some(rc) = next_opt {
                        let next_state = rc.borrow().clone();
                        worklist.push(next_state);
                    }
                }
                // Собственные рёбра состояния-реализации - тоже переходы. Правило языка
                // задано: по завершении реализации проверяются переходы состояния -
                // сначала `ref`, затем `next`.
                for reference in state.references().iter().rev() {
                    let ref_opt = model.borrow().search_state(&reference.name);
                    if let Some(rc) = ref_opt {
                        let ref_state = rc.borrow().clone();
                        worklist.push(ref_state);
                    }
                }
            }
            StateNode::Unresolved => {}
        }
    }
}

fn build_extend(extend: &Extend, model: Rc<RefCell<ModelNode>>) -> StateExtend {
    match extend {
        Extend::None => StateExtend::None,
        Extend::Unresolved(_) => StateExtend::None,
        Extend::Model(model, _, args) => {
            StateExtend::Model(Name::from(Rc::clone(model)), args.clone())
        }
        Extend::Parentless(extend) => build_extend(extend, model),
        Extend::Concatenation(extends) => StateExtend::Concatenation(
            extends
                .iter()
                .map(|extend| build_extend(extend, model.clone()))
                .collect(),
        ),
        Extend::Parallel(extends) => StateExtend::Parallel(
            extends
                .iter()
                .map(|extend| build_extend(extend, model.clone()))
                .collect(),
        ),
    }
}

/// Рекурсивно обходит выражение реализации [`Extend`], регистрирует используемые модели
/// в `used` и возвращает плоский список вложенных элементов.
///
/// - [`Extend::Model`] - регистрирует модель и запускает обход её состояний.
/// - [`Extend::Concatenation`] / [`Extend::Parallel`] - рекурсирует в каждый операнд.
/// - [`Extend::Parentless`] - прозрачная обёртка, делегирует внутрь.
/// - [`Extend::None`] / [`Extend::Unresolved`] - пропускаются.
fn visit_extend(
    extend: &Extend,
    model: Rc<RefCell<ModelNode>>,
    used: &mut BTreeMap<Name, Element>,
) {
    match extend {
        Extend::Model(m_rc, _, _) => {
            let unique = unique_model_name(m_rc.clone());
            let local = m_rc.borrow().name.clone().unwrap_or_default();
            let key = Name::new(local, unique);
            if !used.contains_key(&key) {
                // Собираем имена состояний отдельным borrow, чтобы не удерживать его
                // при последующих вызовах
                let state_keys: Vec<String> = m_rc.borrow().states.keys().cloned().collect();
                let start_opt = m_rc.borrow().get_start_state();
                let states: Vec<Name> = state_keys
                    .iter()
                    .map(|s| Name::new(s.clone(), unique_state_name(s, m_rc.clone())))
                    .collect();
                let start_name = start_opt
                    .as_ref()
                    .map(|s| {
                        Name::new(
                            s.name().to_string(),
                            unique_state_name(s.name(), m_rc.clone()),
                        )
                    })
                    .unwrap_or_else(|| Name::new(String::new(), String::new()));
                used.insert(
                    key.clone(),
                    Element::Model {
                        name: key,
                        states,
                        start: start_name,
                    },
                );
                // Рекурсивно обходим состояния модели
                if let Some(start) = start_opt {
                    visit_state(&start, m_rc.clone(), used);
                }
            }
        }
        Extend::Parentless(inner) => visit_extend(inner, model, used),
        Extend::Concatenation(items) | Extend::Parallel(items) => {
            for item in items {
                visit_extend(item, model.clone(), used)
            }
        }
        Extend::None | Extend::Unresolved(_) => (),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse;
    use crate::semantic::test_constants::tests::SRC;
    use crate::semantic::test_constants::tests::model_node;
    use crate::semantic::tree::construct_model;

    #[test]
    fn test_unique_model_name() {
        let model = model_node("A", None);
        assert_eq!(unique_model_name(model.clone()), "A");

        let model = model_node("B", Some(model.clone()));
        assert_eq!(unique_model_name(model.clone()), "A:B");
    }

    /// Тест с однобуквенными именами (базовые случаи).
    #[test]
    fn test_model_by_unique_name() {
        let global = model_node("A", None);
        let model = model_node("B", Some(global.clone()));
        let _ = model_node("C", Some(model.clone()));
        assert_eq!(
            model_by_unique_name("A", global.clone())
                .unwrap()
                .borrow()
                .name(),
            "A"
        );
        assert_eq!(
            model_by_unique_name("A:B", global.clone())
                .unwrap()
                .borrow()
                .name(),
            "B"
        );
        assert_eq!(
            model_by_unique_name("A:B:C", global.clone())
                .unwrap()
                .borrow()
                .name(),
            "C"
        );
    }

    /// Тест с многосимвольными именами - проверяет корректность отсечения ':'.
    #[test]
    fn test_model_by_unique_name_multichar() {
        let global = model_node("Root", None);
        let child = model_node("Child", Some(global.clone()));
        let _ = model_node("Leaf", Some(child.clone()));
        assert_eq!(
            model_by_unique_name("Root", global.clone())
                .unwrap()
                .borrow()
                .name(),
            "Root"
        );
        assert_eq!(
            model_by_unique_name("Root:Child", global.clone())
                .unwrap()
                .borrow()
                .name(),
            "Child"
        );
        assert_eq!(
            model_by_unique_name("Root:Child:Leaf", global.clone())
                .unwrap()
                .borrow()
                .name(),
            "Leaf"
        );
        // Контрпример: несуществующая модель возвращает None
        assert!(model_by_unique_name("Root:Ghost", global.clone()).is_none());
        assert!(model_by_unique_name("Ghost", global.clone()).is_none());
    }

    /// Snapshot::create возвращает ошибку, если нет стартового состояния.
    #[test]
    fn test_snapshot_create_no_start() {
        // Создаём пустую модель без состояний напрямую, обходя валидацию
        // construct_model
        let model_rc = model_node("Root", None);
        let result = Map::create(model_rc);
        assert!(result.is_err());
    }

    /// Snapshot::create обходит простые состояния.
    #[test]
    fn test_snapshot_create_simple_states() {
        let (ast, _) = parse("start S; state T;", 0).unwrap();
        let model_rc = construct_model(&ast, None, &[]).unwrap();
        let snap = Map::create(model_rc).unwrap();
        // Стартовое состояние найдено
        let Element::Model { start, .. } = snap.model else {
            unreachable!()
        };
        assert_eq!(start.local, "S");
        // S зарегистрировано среди используемых элементов
        assert!(
            snap.elements
                .values()
                .any(|e| matches!(e, Element::State { name, .. } if name.local == "S"))
        );
    }

    /// Snapshot::create регистрирует модели из выражений реализации.
    ///
    /// После compact_extend модель A регистрируется под именем "EntryA" (преисправление
    /// состояния + исходное имя модели).
    #[test]
    fn test_snapshot_create_with_extend() {
        let src = "model A { start S; }\nstart Entry = A;";
        let (ast, _) = parse(src, 0).unwrap();
        let model_rc = construct_model(&ast, None, &[]).unwrap();
        let snap = Map::create(model_rc).unwrap();
        // Стартовое состояние корневой модели - Entry
        let Element::Model { start, .. } = snap.model.clone() else {
            unreachable!()
        };
        assert_eq!(start.local, "Entry");
        // compact_extend именует копию как "Entry" + "A" = "EntryA"
        assert!(
            snap.used_models()
                .iter()
                .any(|e| matches!(e, Element::Model { name, .. } if name.local == "A"))
        );
    }

    /// Snapshot::create не дублирует элементы при параллельных ссылках на одну модель.
    ///
    /// compact_extend кэширует уже созданную копию ("EntryA"), поэтому оба операнда `A |
    /// A` указывают на один и тот же Rc.
    #[test]
    fn test_snapshot_create_dedup() {
        let src = "model A { start S; }\nstart Entry = A | A;";
        let (ast, _) = parse(src, 0).unwrap();
        let model_rc = construct_model(&ast, None, &[]).unwrap();
        let snap = Map::create(model_rc).unwrap();
        // Модель EntryA должна быть зарегистрирована ровно один раз
        let model_count = snap
            .used_models()
            .iter()
            .filter(|e| matches!(e, Element::Model { name, .. } if name.local == "A"))
            .count();
        assert_eq!(model_count, 1);
    }

    #[test]
    fn test_snapshot_create_complex() {
        let (ast, _) = parse(SRC, 0).unwrap();
        let model_rc = construct_model(&ast, None, &[]).unwrap();
        let snap = Map::create(model_rc).unwrap();
        let Element::Model { start, .. } = snap.model else {
            unreachable!()
        };
        assert_eq!(start.local, "Entry");
    }
}
