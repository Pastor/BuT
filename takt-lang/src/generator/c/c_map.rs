use crate::diagnostics::{Diagnostic, Location};
use crate::generator::FloatWidth;
use crate::semantic::minimap::{Element, Map, Name};
use crate::semantic::naming::normalize_camelcase_name;
use crate::semantic::unused::UsageSet;
use crate::semantic::{ModelNode, StateNode};
use std::cell::RefCell;
use std::rc::Rc;

pub struct CMap {
    filename: String,
    map: Map,
    /// Множество используемых имён модели (для фильтрации неиспользуемых элементов).
    usage: UsageSet,
    /// Флаг включения генерации проверок Guard-формул.
    guard_enable: bool,
    /// Форма печати автомата (фича 0435): `switch` либо таблица переходов.
    fsm: crate::generator::FsmForm,
    /// Ширина вещественного типа (фича 0029): `double` при `W64`, `float` при `W32`.
    float_width: FloatWidth,
    /// Профиль времени (фича 0134): в какие единицы пересчитывается длительность.
    time_profile: crate::semantic::duration::TimeProfile,
    /// Режим `c-hal` (фича 0020-05): цель знает адреса и вправе обращаться к
    /// памяти напрямую.
    ///
    /// Нужен печати анонимного обращения `#0x…` (фича 0189): у цели `c` доступ
    /// идёт **только** через колбэки HAL, поэтому обращение по адресу она
    /// отвергает (`CC-021`), а `c-hal` печатает `*(volatile uintN_t*)`.
    hal: bool,
    /// Предупреждения цели, накопленные за проход (фича 0314).
    ///
    /// Канал у цели `c` существует с фичи 0168 (`Generator::generate` возвращает
    /// `Vec<Diagnostic>`), но говорить по нему было нечем. Первое, что по нему
    /// поехало, — `CC-024`: вызов `debug(…)` цель **выбрасывает**, и молчать
    /// об этом нельзя (соседние `st` и `rust` на том же входе отказывают).
    ///
    /// ⚠️ `RefCell`, потому что печатники получают карту по `&self`: заводить
    /// `&mut` через полтора десятка сигнатур ради накопителя — цена выше
    /// пользы. Генерация однопоточна по построению.
    warnings: RefCell<Vec<Diagnostic>>,
}

impl CMap {
    /// Аргумент-указатель на корень для вызова функций модели `child`
    /// (фича 0396): `", main"`, `", model"` либо пусто.
    ///
    /// `caller_is_main` — печатается ли вызов из тела корневой модели: там
    /// указатель на корень зовётся `model`, в под-модели — `main`.
    ///
    /// `which` — какая функция ребёнка вызывается (фича 0419): нужда считается
    /// на функцию, и `X_init` может обойтись без указателя там, где `X_tick`
    /// без него не может.
    ///
    /// ⚠️ Признак спрашивается у **одного** носителя (`c_needs`) и здесь, и в
    /// сигнатуре: разъехавшись, они дали бы «too many arguments» — отказ `cc`,
    /// то есть громкий, но всё же отказ.
    pub(crate) fn root_arg(
        &self,
        child: &Name,
        caller_is_main: bool,
        which: crate::generator::c::c_needs::ModelFn,
    ) -> &'static str {
        let needed = self.raw_model_at(child.clone()).is_ok_and(|rc| {
            crate::generator::c::c_needs::model_fn_needs_root(
                &rc,
                which,
                crate::generator::c::c_time::is_clock_profile(self),
            )
        });
        if !needed {
            return "";
        }
        if caller_is_main { ", model" } else { ", main" }
    }

    pub(crate) fn raw_model_at(&self, name: Name) -> Result<Rc<RefCell<ModelNode>>, Diagnostic> {
        self.map
            .model_at(Some(name.unique().to_string()))
            .ok_or_else(|| {
                Diagnostic::error(
                    Location::Codegen,
                    // Язык сообщений — свойство инструмента, а не автора строки
                    // (фича 0467): две диагностики цели `c` оставались
                    // английскими, тогда как парные им `RS-013` и `SV-011`
                    // давно по-русски.
                    format!("Модель '{}' не найдена", name),
                )
                .with_code("CC-004")
            })
    }

    pub(crate) fn raw_state_at(&self, name: Name) -> Result<Rc<RefCell<StateNode>>, Diagnostic> {
        self.map
            .state_at(Some(name.unique().to_string()))
            .ok_or_else(|| {
                // Позицию даёт носитель (0308, 0468): своей у отказа нет —
                // состояния, о котором он говорит, не существует, зато место
                // ССЫЛКИ на него знает печатник переходов.
                Diagnostic::error(
                    crate::generator::site::at(Location::Codegen),
                    format!("Состояние '{}' не найдено", name),
                )
                .with_code("CC-005")
            })
    }
}

impl CMap {
    /// Запоминает предупреждение цели (фича 0314).
    pub(crate) fn warn(&self, diagnostic: Diagnostic) {
        self.warnings.borrow_mut().push(diagnostic);
    }

    /// Забирает накопленные предупреждения — зовёт генератор в конце прохода.
    pub(crate) fn take_warnings(&self) -> Vec<Diagnostic> {
        std::mem::take(&mut self.warnings.borrow_mut())
    }

    pub fn new(filename: &str, model: &ModelNode, guard_enable: bool) -> Result<Self, Diagnostic> {
        let model_rc = Rc::new(RefCell::new(model.copy(None, None)));
        let usage = crate::semantic::unused::compute_usage(Rc::clone(&model_rc));
        Ok(Self {
            filename: filename.to_string(),
            map: Map::create(model_rc)?,
            usage,
            guard_enable,
            float_width: FloatWidth::default(),
            time_profile: crate::semantic::duration::TimeProfile::default(),
            hal: false,
            fsm: crate::generator::FsmForm::default(),
            warnings: RefCell::new(Vec::new()),
        })
    }

    /// Задаёт форму печати автомата (фича 0435).
    ///
    /// Отдельным методом по той же причине, что профиль времени и режим HAL:
    /// умолчание — `switch`, и существующие вызовы `new` его не повторяют.
    pub fn with_fsm(mut self, fsm: crate::generator::FsmForm) -> Self {
        self.fsm = fsm;
        self
    }

    /// Печатается ли автомат таблицей переходов (`--fsm=table`).
    pub(crate) fn fsm_table(&self) -> bool {
        self.fsm == crate::generator::FsmForm::Table
    }

    /// Включает режим `c-hal` (фича 0020-05, потребитель — фича 0189).
    ///
    /// Отдельным методом по той же причине, что профиль времени и ширина
    /// вещественного: умолчание — цель `c`, и существующие вызовы `new` его не
    /// повторяют. Умолчание при этом **безопасное**: не зная адресов, цель
    /// отказывает, а не печатает доступ наугад.
    pub fn with_hal(mut self, hal: bool) -> Self {
        self.hal = hal;
        self
    }

    /// Знает ли цель адреса (режим `c-hal`).
    pub(crate) fn hal(&self) -> bool {
        self.hal
    }

    /// Задаёт профиль времени (фича 0134): «часы» либо «такты».
    ///
    /// Отдельным методом по той же причине, что и ширина вещественного:
    /// умолчание — профиль «часы», и существующие вызовы `new` его не повторяют.
    pub fn with_time_profile(mut self, profile: crate::semantic::duration::TimeProfile) -> Self {
        self.time_profile = profile;
        self
    }

    /// Профиль времени, выбранный для этой сборки (фича 0134).
    pub(crate) fn time_profile(&self) -> crate::semantic::duration::TimeProfile {
        self.time_profile
    }

    /// Задаёт ширину вещественного типа (фича 0029).
    ///
    /// Отдельным методом, а не параметром `new`: умолчание `W64` — поведение по
    /// умолчанию генератора, и 26 существующих вызовов `new` (в основном тесты)
    /// не должны повторять его дословно.
    pub fn with_float_width(mut self, float_width: FloatWidth) -> Self {
        self.float_width = float_width;
        self
    }

    /// Ширина вещественного типа в порождаемом C.
    pub(crate) fn float_width(&self) -> FloatWidth {
        self.float_width
    }

    pub fn guard_enable(&self) -> bool {
        self.guard_enable
    }

    /// Возвращает ссылку на множество используемых имён модели.
    pub fn usage(&self) -> &UsageSet {
        &self.usage
    }

    pub fn get_filename(&self) -> &str {
        &self.filename
    }

    /// Возвращает имя корневой структуры в PascalCase (например, `ElevatorEngine`).
    #[allow(dead_code)]
    pub fn get_struct_name(&self) -> String {
        let name = self
            .map
            .model_at(None)
            .unwrap()
            .borrow()
            .name
            .clone()
            .unwrap();
        normalize_camelcase_name(&name)
    }

    pub fn using_models(&self) -> Vec<Element> {
        self.map.used_models()
    }

    /// Возвращает элемент корневой модели из карты (если есть).
    #[allow(dead_code)]
    pub fn own_model(&self) -> Option<Element> {
        self.map.own()
    }

    /// Возвращает имя стартового состояния корневой модели.
    #[allow(dead_code)]
    pub fn start(&self) -> Name {
        let Element::Model { start, .. } = self.map.model().clone() else {
            unreachable!()
        };
        start
    }

    pub(crate) fn model(&self) -> Element {
        self.map.model()
    }

    pub fn state_at(&self, name: Name) -> Option<Element> {
        if let Some(element) = self.map.element_at(name)
            && element.is_state()
        {
            Some(element)
        } else {
            None
        }
    }

    pub fn root_name(&self) -> Name {
        self.map.root_name()
    }

    pub fn states(&self) -> Vec<Name> {
        self.map.states()
    }

    /// Возвращает корневую модель (только для чтения, для генерации типов).
    pub(crate) fn root_model_node(&self) -> Option<Rc<RefCell<crate::semantic::ModelNode>>> {
        self.map.model_at(None)
    }
}

/// Карта цели `c` — источник состояний для общего носителя строк таблицы
/// (фича 0440): вопросы к карте одни у всех целей, а сама карта у каждой своя.
impl crate::generator::table::StateSource for CMap {
    fn state_element(&self, name: Name) -> Option<Element> {
        self.state_at(name)
    }

    fn state_node(&self, name: Name) -> Result<Rc<RefCell<StateNode>>, Diagnostic> {
        self.raw_state_at(name)
    }
}
