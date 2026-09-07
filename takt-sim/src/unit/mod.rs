mod blocks;
pub(crate) mod builder;
#[path = "clock.rs"]
mod clock;
/// Контекст модели: где живёт значение переменной.
mod context_model;
mod every;
mod initial;
pub(crate) mod statement;
mod tick;
#[cfg(feature = "graphics")]
pub(crate) mod viewport;

use crate::context::Context;
use crate::eval::value::Value;
use std::cell::RefCell;
use std::cmp::PartialEq;
use std::collections::HashMap;
use std::rc::Rc;
use takt_lang::diagnostics::Diagnostic;

/// Человекочитаемое описание диагностики для `TickResult::Failed`.
fn describe(diagnostic: &Diagnostic) -> String {
    match &diagnostic.code {
        Some(code) => format!("{} ({code})", diagnostic.message),
        None => diagnostic.message.clone(),
    }
}

/// Предикат перехода: именованное условие с функцией-проверкой.
///
/// `name` - отображается как метка ребра в SVG-графе. Клонирование дёшево (`Rc` под
/// капотом).
#[derive(Clone)]
// Тип `func` - замыкание-предикат за `Rc<dyn Fn>`; это и есть суть Predicate, вынос в
// псевдоним лишь спрятал бы её (сигнатура отличается от `Execution` возвращаемым
// `bool`, не `Flow`).
#[allow(clippy::type_complexity)]
pub(crate) struct Predicate {
    pub(crate) name: String,
    func: Rc<dyn Fn(&mut dyn Context) -> Result<bool, Diagnostic>>,
}

impl Predicate {
    pub(crate) fn new(
        name: impl Into<String>,
        f: impl Fn(&mut dyn Context) -> Result<bool, Diagnostic> + 'static,
    ) -> Self {
        Self {
            name: name.into(),
            func: Rc::new(f),
        }
    }

    pub(crate) fn evaluate(&self, ctx: &mut dyn Context) -> Result<bool, Diagnostic> {
        (self.func)(ctx)
    }
}

/// Поток управления после исполнения оператора.
///
/// До -2 исполнитель ничего не возвращал, поэтому `return`/`break`/`continue` были
/// невыразимы и молча ронялись.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Flow {
    /// Исполнение продолжается со следующего оператора.
    Normal,
    /// `break` - выйти из ближайшего цикла.
    Break,
    /// `continue` - перейти к следующей итерации ближайшего цикла.
    Continue,
    /// `return [значение]` - выйти из тела функции.
    Return(Option<Value>),
}

/// Исполнитель. `Err` - ошибка вычисления: она **обязана** дойти до вызывающего, а не
/// быть напечатанной и забытой.
pub(crate) type Execution = Rc<dyn Fn(&mut dyn Context) -> Result<Flow, Diagnostic>>;
type Executions = HashMap<String, Vec<Execution>>;

/// Проверяемое обязательство: предикат условия и опциональное имя инварианта для
/// диагностики SIM-025.
pub(crate) type Guard = (Predicate, Option<String>);

/// Набор инвариантов узла: формулы модели (проверяются каждый такт) и формулы по
/// состояниям (проверяются, пока автомат в этом состоянии). Точки проверки - эталон
/// порождённого C: модель до `always`, состояние до `always`.
#[derive(Clone, Default)]
pub(crate) struct Guards {
    /// Инварианты уровня модели.
    pub(crate) model: Vec<Guard>,
    /// Инварианты по имени состояния.
    pub(crate) per_state: HashMap<String, Vec<Guard>>,
}

/// Результат шага симуляции.
///
/// `pub` (а не `pub(crate)`), поскольку возвращается публичным [`Unit::tick`] - иначе
/// `private_interfaces` (пункт бэклога, закрыт попутно ).
#[derive(Eq, PartialEq, Clone, Debug)]
pub enum TickResult {
    Processing,
    Terminated,
    /// Ошибка вычисления: симуляция недостоверна, продолжать нельзя.
    ///
    /// Именно этот вариант делает ошибку **отличимой** от честно ложного условия
    /// : раньше и то и другое давало `false`.
    Failed(String),
}

/// Исполняемый узел автомата - **непрозрачная** обёртка над приватной формой
/// [`UnitKind`]. Форма узла (варианты, поля) - деталь крейта: наружу видны только
/// методы-аксессоры (`tick`, `variable`, `current_state`, ...). Так внутренние типы
/// (`Context`/`Flow`/`Predicate`/`Guards`) честно остаются `pub(crate)`, а публичный
/// API крейта не рассогласован (`private_interfaces` держится линтом в `lib.rs`).
#[derive(Clone, Default)]
pub struct Unit(UnitKind);

/// Внутренняя форма [`Unit`]. `pub(crate)`: имя доступно потребителям крейта
/// (`state_io`, `builder`, `viewport`), но наружу не реэкспортируется.
// `Node` - доминирующий вариант (реальные автоматы), `None`/композиты редки:
// боксировать `Node` ради выравнивания размера значило бы платить за общий случай.
// Осознанный компромисс (как было и у прежнего `enum Unit`).
#[allow(clippy::large_enum_variant)]
#[derive(Clone, Default)]
pub(crate) enum UnitKind {
    #[default]
    None,
    Node {
        context: Option<Rc<RefCell<dyn Context>>>,

        /// Имя модели этого узла - квалификатор для адресации порта.
        ///
        /// Без него пространство имён значений плоское: одноимённые порты разных
        /// под-моделей композиции неразличимы, чтение находит первую ветвь, а запись
        /// расходится по всем. Имя берётся из `ModelNode::name`; у анонимной модели его
        /// нет - тогда квалифицированная адресация к этому узлу невозможна, а голая
        /// работает.
        model_name: Option<String>,

        state_transitions: HashMap<String, Vec<(String, Predicate)>>,
        state_executions: HashMap<String, Executions>,
        /// Периодические блоки `every` состояния: по имени состояния - список
        /// `(период_нс, тело)`. Период всегда в наносекундах (литерал `every` -
        /// длительность), эталон меряет `since_state_entry_ns`.
        state_every: HashMap<String, Vec<(i64, Vec<Execution>)>>,
        /// Реализации состояний-реализаций (`state P = A + B { ... }`) - по имени
        /// состояния.
        ///
        /// Без этого поля композиция, объявленная реализацией состояния, при построении
        /// теряется: узел остаётся с одним безусловным `next` и уходит по нему на такте
        /// 1, не тикнув ни одного шага.
        ///
        /// Реализация строится с контекстом этого узла в качестве общего родителя -
        /// поэтому шаги `+` делят переменные между собой и с наблюдателем, как общие
        /// переменные у цели `c`.
        state_impls: HashMap<String, Rc<RefCell<Unit>>>,
        /// Поглощённое срабатываниями `every` время (нс) - по одному счётчику на блок
        /// `every` **текущего** состояния. Сбрасывается при входе в состояние
        /// (`mark_state_entry`); скрытое состояние сахара, видимое в трассе.
        every_consumed: Vec<i64>,
        state: Option<String>,

        executions: Executions,
        /// Инварианты модели и состояний, проверяются каждый такт.
        guards: Guards,
        /// Нарушения инвариантов, записанные в **мягком** режиме.
        ///
        /// В жёстком режиме (умолчание) нарушение останавливает прогон и сюда не
        /// пишется. В мягком (`tick_soft`) оно накапливается за такт и забирается
        /// прогоном (`take_invariant_violations`, зеркало `take_last_transitions`),
        /// который метит его номером шага. Каждый такт поле опустошается.
        invariant_violations: Vec<String>,
        /// Последний сработавший переход: (из, в, имя_предиката).
        last_transition: Option<(String, String, String)>,
        /// Модельное время (наносекунды) - виртуальные часы.
        ///
        /// Ставит `runner` перед каждым тактом (`set_time_ns`), поэтому часов реального
        /// мира в эталоне нет ни при каких условиях: иначе трасса перестала бы
        /// воспроизводиться, а все сверки стали бы мигающими.
        time_ns: i64,
        /// Тактов, прошедших с входа в текущее состояние.
        ///
        /// Отдельно от модельного времени: выдержка `after 3t` меряется шагами логики и
        /// частоты **не требует** - такт физической длительности не имеет, пока её не
        /// объявили.
        ticks_in_state: u64,
        /// Модельное время входа в **текущее** состояние.
        ///
        /// От него отсчитывается `after`. Ставится при входе в стартовое состояние и
        /// при каждом переходе - то есть выдержка меряется от момента входа, а не от
        /// начала прогона.
        state_entered_ns: i64,
        /// Исполнен ли `enter` **стартового** состояния (Д5).
        ///
        /// `enter` вызывался только в ветке перехода, поэтому начальная инициализация
        /// модели терялась. Флаг гарантирует "ровно один раз": при возобновлении из
        /// сохранённого состояния (`state_io::restore`) он выставляется в `true` -
        /// модель уже находится в состоянии, входить в него повторно нельзя.
        entered_initial: bool,
        /// Исполнен ли `exit` при завершении автомата.
        ///
        /// Уход в терминал - тоже выход из состояния: цели `c`, `rust`, `st` и `sv`
        /// исполняют там `exit`, а эталон не исполнял его вовсе - один вход давал `hits
        /// = 1` против `11` у всех четырёх. Флаг держит "ровно один раз": терминальный
        /// узел тикается и дальше, а выходят из состояния однажды.
        exited_terminal: bool,
    },
    Parallel {
        units: Vec<Rc<RefCell<Unit>>>,
        executions: Executions,
    },
    Sequential {
        units: Vec<Rc<RefCell<Unit>>>,
        index: usize,
        executions: Executions,
    },
}

impl Unit {
    /// Конструктор из внутренней формы - для потребителей крейта вне модуля `unit`
    /// (`state_io`), которым приватное поле недоступно напрямую.
    pub(crate) fn from_kind(kind: UnitKind) -> Self {
        Unit(kind)
    }

    /// Заимствование внутренней формы (чтение) - для тех же потребителей.
    pub(crate) fn kind(&self) -> &UnitKind {
        &self.0
    }

    /// Заимствование внутренней формы (запись).
    pub(crate) fn kind_mut(&mut self) -> &mut UnitKind {
        &mut self.0
    }
}

impl Context for Unit {
    /// Стенд внешних функций - у контекста узла.
    ///
    /// Такт исполняется с `Unit` в роли контекста, поэтому без этой ветви подмена,
    /// поставленная `set_extern_stubs`, до вызова не доезжает и прогон отвечает
    /// `SIM-019` при заданном стенде.
    fn extern_result(&self, name: &str, args: &[Value]) -> Option<Value> {
        match &self.0 {
            UnitKind::Node { context, .. } => context
                .as_ref()
                .and_then(|ctx| ctx.borrow().extern_result(name, args)),
            UnitKind::Parallel { units, .. } | UnitKind::Sequential { units, .. } => units
                .iter()
                .find_map(|unit| unit.borrow().extern_result(name, args)),
            UnitKind::None => None,
        }
    }

    fn since_state_entry_ns(&self) -> i64 {
        Unit::since_state_entry_ns(self)
    }

    /// Состояние другой модели прогона - у контекста узла: реестр общий и лежит в корне
    /// цепочки контекстов.
    ///
    /// Композиты спрашивают детей: у самих `Parallel`/`Sequential` контекста нет, а
    /// условие может вычисляться и в их поддереве.
    fn model_state(&self, model: &str) -> Option<String> {
        match &self.0 {
            UnitKind::Node { context, .. } => context
                .as_ref()
                .and_then(|ctx| ctx.borrow().model_state(model)),
            UnitKind::Parallel { units, .. } | UnitKind::Sequential { units, .. } => units
                .iter()
                .find_map(|unit| unit.borrow().model_state(model)),
            UnitKind::None => None,
        }
    }

    fn set_model_state(&self, model: &str, state: &str) {
        match &self.0 {
            UnitKind::Node { context, .. } => {
                if let Some(ctx) = context.as_ref() {
                    ctx.borrow().set_model_state(model, state);
                }
            }
            UnitKind::Parallel { units, .. } | UnitKind::Sequential { units, .. } => {
                if let Some(unit) = units.first() {
                    unit.borrow().set_model_state(model, state);
                }
            }
            UnitKind::None => {}
        }
    }

    fn ticks_in_state(&self) -> u64 {
        Unit::ticks_in_state(self)
    }

    fn get_value(&self, name: &str) -> Option<Value> {
        // Квалифицированное имя `Модель::порт` адресует одну ветвь. Голое имя берёт
        // первую нашедшуюся: сценарии, состояния и тесты на голых именах работают
        // по-прежнему.
        if let Some((model, port)) = split_qualified(name) {
            return self.get_qualified(model, port);
        }
        match &self.0 {
            UnitKind::None => None,
            // Единственный источник истины - контекст модели: своей карты значений у
            // узла нет, и читается он оттуда же, куда пишется.
            UnitKind::Node {
                context,
                state,
                state_impls,
                ..
            } => context
                .as_ref()
                .and_then(|ctx| ctx.borrow().get_value(name))
                // Реализация состояния опрашивается после собственного контекста:
                // shared-переменные лежат в контексте узла, и обратный порядок отдал бы
                // наблюдателю копию из ветви вместо общего значения. Здесь находятся
                // только локальные переменные шагов композиции.
                .or_else(|| {
                    state
                        .as_ref()
                        .and_then(|s| state_impls.get(s))
                        .and_then(|inner| inner.borrow().get_value(name))
                }),
            UnitKind::Parallel { units, .. } => {
                units.iter().find_map(|unit| unit.borrow().get_value(name))
            }
            // Завершившаяся цепочка (`index == units.len()`) наблюдается по последнему
            // шагу: ответь чтение `None`, и значение, доведённое композицией до конца,
            // пропадёт из трассы.
            UnitKind::Sequential { units, index, .. } => units
                .get(*index)
                .or_else(|| units.last())
                .and_then(|u| u.borrow().get_value(name)),
        }
    }

    fn set_value(&mut self, name: &str, value: Value) {
        // Квалифицированная запись идёт ровно в одну ветвь - иначе задать вход
        // отдельной под-модели композиции нечем: голое имя рассылается всем ветвям.
        if let Some((model, port)) = split_qualified(name) {
            self.set_qualified(model, port, value);
            return;
        }
        match &mut self.0 {
            UnitKind::None => {}
            // Запись идёт в контекст модели тем же путём, что присваивание в теле
            // блока. Общие переменные уходят по цепочке `parent` в родительский
            // контекст.
            UnitKind::Node {
                context,
                state,
                state_impls,
                ..
            } => {
                if let Some(ctx) = context {
                    ctx.borrow_mut().set_value(name, value.clone());
                }
                // Реализация состояния получает запись так же, как ветви `Parallel`:
                // иначе порт под-модели композиции задать нечем. Повторная запись
                // идемпотентна - shared-имя обе стороны маршрутизируют в один
                // родительский контекст.
                if let Some(inner) = state.as_ref().and_then(|s| state_impls.get(s)) {
                    inner.borrow_mut().set_value(name, value);
                }
            }
            UnitKind::Parallel { units, .. } => {
                // Запись в параллельную композицию адресуется всем ветвям; каждая
                // маршрутизирует shared-имя в общий родительский контекст, поэтому
                // повторная запись идемпотентна (одно значение, один источник).
                for unit in units.iter() {
                    unit.borrow_mut().set_value(name, value.clone());
                }
            }
            UnitKind::Sequential { units, index, .. } => {
                if let Some(u) = units.get(*index) {
                    u.borrow_mut().set_value(name, value);
                }
            }
        }
    }

    fn dump(&self) -> HashMap<String, Value> {
        match &self.0 {
            // Снимок узла - состояние его модели (и родителей) из контекста.
            UnitKind::Node { context, .. } => context
                .as_ref()
                .map(|ctx| ctx.borrow().dump())
                .unwrap_or_default(),
            // Композиты снимаются рекурсивно по детям (см. `state_io::snapshot`),
            // собственного состояния у них нет.
            _ => HashMap::new(),
        }
    }
}

impl Unit {
    /// Извлекает и сбрасывает последний сработавший переход: (из, в, имя_предиката).
    /// Для составных Unit (Parallel/Sequential) рекурсивно собирает из всех дочерних.
    pub fn take_last_transition(&mut self) -> Option<(String, String, String)> {
        self.take_last_transitions().into_iter().next()
    }

    /// Рекурсивно извлекает все сработавшие переходы из этого узла и его потомков.
    pub fn take_last_transitions(&mut self) -> Vec<(String, String, String)> {
        match &mut self.0 {
            UnitKind::Node {
                last_transition,
                state,
                state_impls,
                ..
            } => {
                let mut out: Vec<_> = last_transition.take().into_iter().collect();
                // Переходы внутри реализации состояния - тоже переходы автомата: без
                // них трасса и SVG-граф показывали бы состояние-реализацию неподвижным.
                if let Some(inner) = state.as_ref().and_then(|s| state_impls.get(s)) {
                    out.extend(inner.borrow_mut().take_last_transitions());
                }
                out
            }
            UnitKind::Parallel { units, .. } | UnitKind::Sequential { units, .. } => units
                .iter()
                .flat_map(|u| u.borrow_mut().take_last_transitions())
                .collect(),
            UnitKind::None => vec![],
        }
    }

    /// Сливает нарушения инвариантов, записанные мягким режимом, из всего дерева `Unit` -
    /// рекурсивно, как [`take_last_transitions`]. `runner` зовёт после каждого такта и
    /// тегирует нарушения номером шага (у `Unit` номера шага нет - его ведёт `runner`).
    /// Опустошает поля узлов.
    pub fn take_invariant_violations(&mut self) -> Vec<String> {
        match &mut self.0 {
            UnitKind::Node {
                invariant_violations,
                state,
                state_impls,
                ..
            } => {
                let mut out = std::mem::take(invariant_violations);
                // Инвариант, нарушенный внутри реализации состояния, обязан дойти до
                // `runner` - иначе мягкий режим о нём промолчит.
                if let Some(inner) = state.as_ref().and_then(|s| state_impls.get(s)) {
                    out.extend(inner.borrow_mut().take_invariant_violations());
                }
                out
            }
            UnitKind::Parallel { units, .. } | UnitKind::Sequential { units, .. } => units
                .iter()
                .flat_map(|u| u.borrow_mut().take_invariant_violations())
                .collect(),
            UnitKind::None => vec![],
        }
    }

    /// Возвращает имена состояний, достижимых из активных за один переход.
    pub fn reachable_from_active(&self) -> Vec<String> {
        match &self.0 {
            UnitKind::Node {
                state,
                state_transitions,
                ..
            } => {
                let current = match state {
                    Some(s) => s,
                    None => return vec![],
                };
                state_transitions
                    .get(current)
                    .map(|ts| ts.iter().map(|(to, _)| to.clone()).collect())
                    .unwrap_or_default()
            }
            UnitKind::Parallel { units, .. } | UnitKind::Sequential { units, .. } => units
                .iter()
                .flat_map(|u| u.borrow().reachable_from_active())
                .collect(),
            UnitKind::None => vec![],
        }
    }

    pub fn execution(&mut self, name: &str) -> Result<(), Diagnostic> {
        // Шаг 1: клонируем Rc-ссылки на функции уровня unit, не удерживая заимствование
        // self
        let unit_fns: Vec<Execution> = match &self.0 {
            UnitKind::Node { executions, .. } => executions.get(name).cloned().unwrap_or_default(),
            UnitKind::Parallel { executions, .. } | UnitKind::Sequential { executions, .. } => {
                executions.get(name).cloned().unwrap_or_default()
            }
            UnitKind::None => vec![],
        };
        // Шаг 2: вызываем - self свободен от заимствования
        for f in &unit_fns {
            f(self)?;
        }
        // Шаг 3: для Node - функции уровня текущего состояния.
        //
        // После выхода из состояния они не исполняются: узел завершён, и его блоки
        // молчат - ровно как ветвь `END` в порождённом C, где `switch` уже ничего не
        // выбирает. Блоки уровня модели (шаг 1) при этом исполняются по-прежнему - они
        // и в C печатаются до `switch`.
        let state_fns: Vec<Execution> = match &self.0 {
            UnitKind::Node {
                state: Some(s),
                state_executions,
                exited_terminal: false,
                ..
            } => state_executions
                .get(s.as_str())
                .and_then(|m| m.get(name))
                .cloned()
                .unwrap_or_default(),
            _ => vec![],
        };
        for f in &state_fns {
            f(self)?;
        }
        // Рекурсии в дочерние здесь нет - и это существенно.
        //
        // На идемпотентном присваивании (`x := <выражение>`) разницы не видно - весь
        // корпус написан именно так, - а накапливающее (`n := n + 1`) давало удвоение:
        // симулятор 2, цель `c` 1.
        //
        // Ребёнок исполняет свои блоки сам, в собственном `tick_body`, и множество
        // исполняемых детей от снятия рекурсии не изменилось: у `Parallel` тикаются
        // все, у `Sequential` - активный шаг, ровно те же, в которые спуск и шёл.
        Ok(())
    }

    /// Читает значение переменной или порта - **публичная точка наблюдения**.
    ///
    /// Нужна, чтобы тесты и внешние инструменты могли сверять **вычисленные значения**,
    /// а не только факт перехода. Отсутствие такого слоя и позволило восьми дефектам
    /// прожить при зелёных тестах: проверялись переходы, а значения - нет.
    ///
    /// Читает по той же цепочке, что и вычислитель ([`Context::get_value`]): сначала
    /// собственные переменные юнита, затем контекст модели. Чтение по
    /// квалифицированному имени: спуск до узла модели `model`.
    fn get_qualified(&self, model: &str, port: &str) -> Option<Value> {
        match &self.0 {
            UnitKind::None => None,
            UnitKind::Node {
                context,
                model_name,
                state,
                state_impls,
                ..
            } => {
                if model_name.as_deref() == Some(model) {
                    context
                        .as_ref()
                        .and_then(|ctx| ctx.borrow().get_value(port))
                } else {
                    // Под-модели реализации состояния адресуются квалифицированно так
                    // же, как ветви композиции: спуск идёт в реализацию текущего
                    // состояния.
                    state
                        .as_ref()
                        .and_then(|s| state_impls.get(s))
                        .and_then(|inner| inner.borrow().get_qualified(model, port))
                }
            }
            // Композиция сама модели не имеет: спрашиваем ветви.
            UnitKind::Parallel { units, .. } => units
                .iter()
                .find_map(|unit| unit.borrow().get_qualified(model, port)),
            // У последовательной композиции опрашиваются все шаги, а не только
            // активный: наблюдение за уже отработавшим шагом законно.
            UnitKind::Sequential { units, .. } => units
                .iter()
                .find_map(|unit| unit.borrow().get_qualified(model, port)),
        }
    }

    /// Запись по квалифицированному имени: только в узел модели `model`.
    fn set_qualified(&mut self, model: &str, port: &str, value: Value) {
        match &mut self.0 {
            UnitKind::None => {}
            UnitKind::Node {
                context,
                model_name,
                state,
                state_impls,
                ..
            } => {
                if model_name.as_deref() == Some(model) {
                    if let Some(ctx) = context {
                        ctx.borrow_mut().set_value(port, value);
                    }
                } else if let Some(inner) = state.as_ref().and_then(|s| state_impls.get(s)) {
                    // Симметрично чтению: вход под-модели реализации задаётся
                    // квалифицированным именем.
                    inner.borrow_mut().set_qualified(model, port, value);
                }
            }
            UnitKind::Parallel { units, .. } | UnitKind::Sequential { units, .. } => {
                for unit in units.iter() {
                    unit.borrow_mut().set_qualified(model, port, value.clone());
                }
            }
        }
    }

    /// Записывает значение по имени - публичный вход для драйверов и тестов.
    ///
    /// Симметричен [`variable`](Self::variable): без него запись доступна только через
    /// трейт `Context`, то есть внутренний трейт приходится тащить в каждый вызывающий
    /// модуль. Имя может быть квалифицированным (`Модель::порт`) - тогда запись идёт
    /// ровно в одну ветвь композиции; голое имя рассылается всем ветвям.
    pub fn set_port(&mut self, name: &str, value: Value) {
        self.set_value(name, value);
    }

    pub fn variable(&self, name: &str) -> Option<Value> {
        self.get_value(name)
    }

    /// Возвращает имя текущего активного состояния (только для Unit::Node). Кладёт
    /// текущее состояние узла в общий реестр прогона.
    ///
    /// Зовётся из трёх мест: постройка узла (стартовое состояние - там запись идёт
    /// прямо в контекст), переход (`tick_node`) и восстановление из снимка
    /// (`state_io::restore`). Новая точка смены состояния обязана звать её тоже: реестр
    /// и поле `state` - два места одной истины, и расхождение проявится трассой сверки
    /// sim == c, а не отказом сборки.
    pub(crate) fn publish_state(&self) {
        let UnitKind::Node {
            model_name: Some(model),
            state: Some(state),
            ..
        } = &self.0
        else {
            return;
        };
        Context::set_model_state(self, model, state);
    }

    pub fn current_state(&self) -> Option<&str> {
        match &self.0 {
            UnitKind::Node { state, .. } => state.as_deref(),
            _ => None,
        }
    }

    /// Рекурсивно собирает имена всех активных состояний по дереву Unit.
    ///
    /// Для Sequential возвращает состояние текущего активного дочернего Unit. Для
    /// Parallel - состояния всех дочерних Units.
    pub fn active_states(&self) -> Vec<String> {
        let mut out = Vec::new();
        collect_active_states(self, &mut out);
        out
    }

    /// Есть ли у состояния тело: именованный блок либо периодическое действие.
    ///
    /// Признак нужен двоим - [`Unit::is_terminal`] и уходу в терминал на такте, - и
    /// обязан быть у них общим: разойдись они, автомат объявлялся бы завершённым в
    /// одном месте и работающим в другом.
    pub(crate) fn state_has_body(&self, state: &str) -> bool {
        match &self.0 {
            UnitKind::Node {
                state_executions,
                state_every,
                ..
            } => {
                state_executions.get(state).is_some_and(|blocks| {
                    blocks
                        .iter()
                        // Удерживает то, что исполняется каждый такт: `enter` и `exit`
                        // одноразовы - первый про вход, второй про уход. Правило общее
                        // с компилятором
                        // (`takt_lang::semantic::terminal::holds_machine`), и разойтись
                        // им нельзя.
                        .any(|(name, fns)| {
                            !matches!(name.as_str(), "enter" | "exit") && !fns.is_empty()
                        })
                }) || state_every
                    .get(state)
                    .is_some_and(|every| !every.is_empty())
            }
            _ => false,
        }
    }

    pub fn is_terminal(&self) -> bool {
        match &self.0 {
            UnitKind::None => true,
            UnitKind::Node {
                state: current_state,
                state_transitions,
                state_impls,
                ..
            } => {
                let Some(state_name) = current_state else {
                    // Нет активного состояния - терминально если нет возможных
                    // переходов
                    return state_transitions.is_empty();
                };
                // Состояние-реализация не терминально, пока его композиция не
                // отработала.
                if let Some(inner) = state_impls.get(state_name)
                    && !inner.borrow().is_terminal()
                {
                    return false;
                }
                // Тело считается наравне с переходами: состояние с `always` работает
                // вечно - автор написал "всегда", а не "однажды". Правило то же, что у
                // компилятора (`semantic::terminal`), и разойтись им нельзя:
                // расхождение эталона с целями - худший класс проекта.
                let has_edges = state_transitions
                    .get(state_name)
                    .is_some_and(|t| !t.is_empty());
                !has_edges && !self.state_has_body(state_name)
            }
            UnitKind::Parallel { units, .. } | UnitKind::Sequential { units, .. } => {
                units.iter().all(|u| u.borrow().is_terminal())
            }
        }
    }

    pub fn union(&self, other: &Self) -> Self {
        match self.clone().0 {
            UnitKind::None => other.clone(),
            UnitKind::Node { .. } => self.union_parallel(other),
            UnitKind::Parallel {
                mut units,
                mut executions,
            } => {
                if let UnitKind::Parallel {
                    units: other_units,
                    executions: other_executions,
                    ..
                } = &other.0
                {
                    units.append(&mut other_units.clone());
                    for (k, v) in other_executions.clone() {
                        executions.entry(k).or_default().extend(v);
                    }
                    Unit(UnitKind::Parallel { units, executions })
                } else {
                    units.push(Rc::new(RefCell::new(other.clone())));
                    Unit(UnitKind::Parallel { units, executions })
                }
            }
            UnitKind::Sequential { .. } => self.union_parallel(other),
        }
    }

    fn union_parallel(&self, other: &Unit) -> Unit {
        if let UnitKind::Parallel {
            units: other_units,
            executions,
            ..
        } = &other.0
        {
            let mut units = other_units.clone();
            units.insert(0, Rc::new(RefCell::new(self.clone())));
            Unit(UnitKind::Parallel {
                units,
                executions: executions.clone(),
            })
        } else {
            Unit(UnitKind::Parallel {
                units: vec![
                    Rc::new(RefCell::new(self.clone())),
                    Rc::new(RefCell::new(other.clone())),
                ],
                executions: HashMap::new(),
            })
        }
    }

    pub fn add(&self, other: &Self) -> Self {
        match self.clone().0 {
            UnitKind::None => other.clone(),
            UnitKind::Node { .. } => {
                let (mut units, executions) =
                    if let UnitKind::Sequential {
                        units, executions, ..
                    } = other.clone().0
                    {
                        (units, executions)
                    } else {
                        (vec![Rc::new(RefCell::new(other.clone()))], HashMap::new())
                    };
                units.insert(0, Rc::new(RefCell::new(self.clone())));
                Unit(UnitKind::Sequential {
                    units,
                    index: 0,
                    executions,
                })
            }
            UnitKind::Parallel { .. } => {
                let units = vec![
                    Rc::new(RefCell::new(self.clone())),
                    Rc::new(RefCell::new(other.clone())),
                ];
                Unit(UnitKind::Sequential {
                    units,
                    index: 0,
                    executions: HashMap::new(),
                })
            }
            UnitKind::Sequential {
                mut units,
                mut executions,
                ..
            } => {
                if let UnitKind::Sequential {
                    units: mut other_units,
                    executions: other_executions,
                    ..
                } = other.clone().0
                {
                    units.append(&mut other_units);
                    other_executions.into_iter().for_each(|(k, v)| {
                        executions.entry(k).or_default().extend(v);
                    });
                } else {
                    units.push(Rc::new(RefCell::new(other.clone())));
                }
                Unit(UnitKind::Sequential {
                    units,
                    index: 0,
                    executions,
                })
            }
        }
    }
}

/// Разбирает квалифицированное имя значения `Модель::порт`.
///
/// Разделитель - `::`, как у квалифицированного ключа карты адресов: две подсистемы
/// адресуют одно и то же, и разъехавшиеся формы записи стоили бы пользователю догадок.
/// Имя без разделителя - голое, обрабатывается прежним путём.
fn split_qualified(name: &str) -> Option<(&str, &str)> {
    let (model, port) = name.split_once("::")?;
    if model.is_empty() || port.is_empty() || port.contains("::") {
        return None;
    }
    Some((model, port))
}

fn collect_active_states(unit: &Unit, out: &mut Vec<String>) {
    match &unit.0 {
        UnitKind::None => {}
        UnitKind::Node {
            state, state_impls, ..
        } => {
            if let Some(s) = state {
                out.push(s.clone());
                // Состояния внутри реализации - часть активной конфигурации автомата, а
                // не деталь: без них трасса `state P = A + B` показывала бы одно
                // неподвижное `P`, тогда как у формы `start P = A | B` (та же
                // композиция без переходов) сообщаются состояния ветвей. Две записи
                // одной конструкции обязаны наблюдаться одинаково.
                if let Some(inner) = state_impls.get(s) {
                    collect_active_states(&inner.borrow(), out);
                }
            }
        }
        UnitKind::Parallel { units, .. } => {
            for u in units {
                collect_active_states(&u.borrow(), out);
            }
        }
        UnitKind::Sequential { units, index, .. } => {
            if let Some(u) = units.get(*index) {
                collect_active_states(&u.borrow(), out);
            }
        }
    }
}

#[cfg(test)]
mod tests;
