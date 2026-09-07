#[cfg(feature = "graphics")]
mod graphics;

use crate::context::Context;
use crate::eval::value::Value;
use crate::graphics_config::{GraphicsConfig, OutputMode};
use crate::json_input::{Guard, PortValues, SimStep, json_to_value};
// Реестр имён вынесен в свой модуль, но потребители зовут его прежним путём
// `takt_sim::runner::PortNames` - реэкспорт держит контракт.
pub use crate::port_names::{PortDirectionKind, PortNames};
// Человекочитаемая длительность переехала в носитель трассы; прежний путь
// `takt_sim::runner::format_duration` держит реэкспорт.
pub use crate::trace::format_duration;
#[cfg(feature = "graphics")]
use crate::unit::viewport::CachedLayout;
use crate::unit::{TickResult, Unit};
#[cfg(feature = "graphics")]
use graphics::GraphicsRecorder;
use std::path::PathBuf;

// -- Результат симуляции ------------------------------------------------------

#[derive(Debug)]
pub enum RunResult {
    /// Модель достигла терминального состояния.
    Terminated { steps: usize },
    /// Выполнено заданное количество шагов.
    StepsReached { steps: usize },
    /// Guard не выполнен на шаге `step` (нумерация с 1).
    GuardFailed { step: usize, details: String },
    /// Ошибка вычисления на шаге `step` (нумерация с 1): симуляция недостоверна.
    ///
    /// Отличает сломанную модель от честно неактивного перехода: сведись ошибка
    /// вычисления к `false`, одно стало бы неотличимо от другого.
    EvalFailed { step: usize, details: String },
    /// Прогон в **мягком** режиме инвариантов завершился, но по ходу были нарушения -
    /// записаны, а не прерваны. `terminated` = дошёл ли автомат до терминального
    /// состояния (иначе - исчерпал бюджет шагов). `violations` - пары `(шаг, детали)` в
    /// порядке возникновения.
    CompletedWithInvariantViolations {
        steps: usize,
        terminated: bool,
        violations: Vec<(usize, String)>,
    },
}

/// Итог одного такта прогона.
///
/// Строка и исход **вместе**: такт, на котором автомат пришёл в терминальное состояние,
/// и трассу даёт, и заканчивает прогон. Разведи их по разным ответам - и последняя
/// строка трассы потерялась бы у того потребителя, который читает исход первым.
#[derive(Debug)]
pub struct Step {
    /// Строка трассы этого такта; `None` - такта не было (прогон уже окончен либо
    /// оборван ошибкой вычисления).
    pub line: Option<String>,
    /// Исход прогона, если он окончен на этом такте.
    pub result: Option<RunResult>,
}

// -- Бегун симуляции ----------------------------------------------------------

pub struct SimulationRunner {
    unit: Unit,
    sim_steps: Vec<SimStep>,
    max_steps: Option<usize>,
    #[cfg(feature = "graphics")]
    graphics_recorder: Option<GraphicsRecorder>,
    #[cfg(feature = "graphics")]
    gif_frame_size: Option<(u32, u32)>,
    port_names: PortNames,
    // Имя модели и настройки холста нужны только кадрам: без фичи `graphics` они не
    // поля, а мусор - и компилятор об этом честно говорит.
    #[cfg(feature = "graphics")]
    model_name: Option<String>,
    #[cfg(feature = "graphics")]
    gif_config: GraphicsConfig,
    // Раскладка графа вычисляется один раз перед первым кадром.
    #[cfg(feature = "graphics")]
    cached_layout: Option<CachedLayout>,
    /// Мягкий режим инвариантов: нарушение записывается и прогон продолжается, вместо
    /// останова. Умолчание - `false`, то есть жёсткий режим.
    soft_invariants: bool,
    /// Модельное время прогона (наносекунды) - виртуальные часы.
    ///
    /// Часов реального мира в эталоне нет ни при каких условиях: трасса обязана
    /// воспроизводиться, иначе все сверки станут мигающими.
    now_ns: i64,
    /// На сколько продвигать часы за такт, если шаг сценария не сказал иного.
    ///
    /// Умолчание - **1 мс**: прогон без указания времени должен оставаться возможным, а
    /// неявной частоты здесь не появляется - это свойство прогона, а не модели.
    /// Объявленная моделью частота (`clock`) задаёт период такта.
    tick_period_ns: i64,
    /// Сколько тактов уже выполнено.
    completed: usize,
    /// Накопленные нарушения инвариантов мягкого режима, с номером шага.
    soft_violations: Vec<(usize, String)>,
    /// Сказано ли уже, что сценарий пользуется устаревшей позиционной формой.
    ///
    /// Предупреждение печатается **один раз за прогон**, а не на каждый шаг: сценарий в
    /// сотню шагов дал бы сотню одинаковых строк, и следующее - настоящее -
    /// предупреждение потерялось бы среди повторов. `Cell`, потому что разбор значений
    /// идёт по `&self`.
    positional_form_warned: std::cell::Cell<bool>,
}

impl SimulationRunner {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        unit: Unit,
        sim_steps: Vec<SimStep>,
        max_steps: Option<usize>,
        output_dir: Option<&PathBuf>,
        input_stem: &str,
        output_mode: OutputMode,
        port_names: PortNames,
        model_name: Option<String>,
        gif_config: GraphicsConfig,
    ) -> Result<Self, String> {
        #[cfg(feature = "graphics")]
        let (graphics_recorder, gif_frame_size) =
            graphics::recorder_of(output_dir, input_stem, output_mode, &gif_config)?;
        // Без графики запрос на кадры - отказ, а не молчаливый пропуск: автор просил
        // картинку, и рапорт об успехе без неё был бы ложью.
        #[cfg(not(feature = "graphics"))]
        if output_dir.is_some() {
            let _ = (input_stem, output_mode, &model_name, &gif_config);
            return Err("запись кадров недоступна: крейт собран без фичи `graphics`".to_string());
        }

        Ok(Self {
            unit,
            sim_steps,
            max_steps,
            #[cfg(feature = "graphics")]
            graphics_recorder,
            #[cfg(feature = "graphics")]
            gif_frame_size,
            port_names,
            #[cfg(feature = "graphics")]
            model_name,
            #[cfg(feature = "graphics")]
            gif_config,
            #[cfg(feature = "graphics")]
            cached_layout: None,
            soft_invariants: false,
            completed: 0,
            soft_violations: Vec::new(),
            positional_form_warned: std::cell::Cell::new(false),
            now_ns: 0,
            tick_period_ns: 1_000_000,
        })
    }

    /// Включает и выключает мягкий режим инвариантов. По умолчанию выключен: жёсткий
    /// режим совпадает с `assert()` у цели `c`.
    pub fn set_invariant_soft(&mut self, on: bool) {
        self.soft_invariants = on;
    }

    /// Задаёт период такта модельных часов, в наносекундах.
    ///
    /// Источники, в порядке приоритета: поле шага сценария (`time_ms`) -> это значение ->
    /// умолчание 1 мс. Объявленная моделью частота (`clock`) переводится в период
    /// вызывающим: `1 с / f`.
    pub fn set_tick_period_ns(&mut self, period_ns: i64) {
        self.tick_period_ns = period_ns.max(0);
    }

    /// Текущее модельное время прогона (наносекунды).
    pub fn now_ns(&self) -> i64 {
        self.now_ns
    }

    /// Запускает главный цикл симуляции, печатая трассу.
    ///
    /// Такт делает [`SimulationRunner::step`]; здесь - только печать и обход. Второго
    /// цикла исполнения в проекте быть не должно: потребитель без консоли (модуль
    /// WebAssembly) тикает тем же `step`, иначе две реализации прогона разошлись бы
    /// молча - и сверки перестали бы что-либо доказывать.
    pub fn run(&mut self) -> Result<RunResult, String> {
        for warning in self.ambiguous_name_warnings() {
            eprintln!("{warning}");
        }
        loop {
            let step = self.step()?;
            if let Some(line) = step.line {
                println!("{line}");
            }
            if let Some(result) = step.result {
                return Ok(result);
            }
        }
    }

    /// Выполняет один такт прогона.
    ///
    /// Возвращает строку трассы этого такта и - когда прогон окончен - его исход. Оба
    /// поля вместе: такт, на котором автомат пришёл в терминальное состояние, и строку
    /// даёт, и заканчивает прогон.
    pub fn step(&mut self) -> Result<Step, String> {
        // Длину прогона задаёт `-n`, а сценарий задаёт входы. Когда шаги сценария
        // кончились, прогон продолжается: значения входных портов удерживаются - ровно
        // как они удерживаются между тактами внутри сценария. Без `-n` длину
        // по-прежнему задаёт сценарий, а без сценария - приход в терминальное
        // состояние.
        let sim_len = self.sim_steps.len();
        let limit = self
            .max_steps
            .unwrap_or(if sim_len > 0 { sim_len } else { usize::MAX });
        let step_no = self.completed;
        if step_no >= limit {
            return Ok(Step {
                line: None,
                result: Some(self.outcome(false)),
            });
        }

        let sim_step: Option<SimStep> = self.sim_steps.get(step_no).cloned();

        // Модельное время ставится до такта: показания часов на такте N обязаны быть
        // видны телу, исполняемому на такте N. Иначе выдержка сдвинулась бы на такт
        // относительно целей - а такой сдвиг компилируется молча (тот же класс, что
        // вход в стартовое состояние). Первый такт идёт при t = 0: часы двигаются перед
        // каждым тактом, кроме первого. Иначе модель входила бы в стартовое состояние
        // уже "спустя период", и выдержка отсчитывалась бы от чужого момента.
        if step_no > 0 {
            let advance_ns = sim_step
                .as_ref()
                .and_then(|step| step.time_ms)
                .map_or(self.tick_period_ns, |ms| ms.saturating_mul(1_000_000));
            self.now_ns = self.now_ns.saturating_add(advance_ns);
        }
        self.unit.set_time_ns(self.now_ns);

        // Применяем входные порты и стенд внешних функций: и то, и другое - вход шага
        // сценария, и ставится оно перед тактом.
        if let Some(step) = &sim_step {
            self.apply_step_inputs(step, step_no + 1)?;
            self.unit
                .set_extern_stubs(extern_stubs_of(step, step_no + 1)?);
        }

        // Выполняем шаг. В мягком режиме нарушения инвариантов не прерывают такт, а
        // записываются - сливаем их и тегируем шагом.
        let tick_result = if self.soft_invariants {
            let r = self.unit.tick_soft();
            for details in self.unit.take_invariant_violations() {
                self.soft_violations.push((self.completed + 1, details));
            }
            r
        } else {
            self.unit.tick()
        };
        if let TickResult::Failed(details) = &tick_result {
            return Ok(Step {
                line: None,
                result: Some(RunResult::EvalFailed {
                    step: self.completed + 1,
                    details: details.clone(),
                }),
            });
        }
        self.completed += 1;

        // Строку трассы строит библиотека (`trace::step_line`): печатает её CLI, а
        // модуль в браузере показывает.
        let line =
            crate::trace::step_line(&self.unit, &self.port_names, self.completed, self.now_ns);

        // Записываем кадры в графику (если нужно)
        #[cfg(feature = "graphics")]
        if self.graphics_recorder.is_some() {
            // Highlight-кадры для каждого сработавшего перехода (включая параллельные)
            let transitions = self.unit.take_last_transitions();
            for (from, to, _pred) in &transitions {
                self.capture_frame_with_highlight(Some((from.as_str(), to.as_str())))?;
            }
            // Обычный кадр с новым активным состоянием
            self.capture_frame()?;
        }

        // Проверяем guard
        if let Some(step) = &sim_step
            && let Some(guard) = &step.guard
        {
            let guard = guard.clone();
            self.check_guard(&guard, step_no + 1)?;
        }

        // Проверяем терминальность
        let result = (tick_result == TickResult::Terminated).then(|| self.outcome(true));
        Ok(Step {
            line: Some(line),
            result,
        })
    }

    /// Исход законченного прогона: `terminated` - дошёл ли автомат до терминального
    /// состояния или исчерпал бюджет шагов.
    fn outcome(&mut self, terminated: bool) -> RunResult {
        if !self.soft_violations.is_empty() {
            return RunResult::CompletedWithInvariantViolations {
                steps: self.completed,
                terminated,
                violations: std::mem::take(&mut self.soft_violations),
            };
        }
        if terminated {
            RunResult::Terminated {
                steps: self.completed,
            }
        } else {
            RunResult::StepsReached {
                steps: self.completed,
            }
        }
    }

    /// Сохраняет результат записи (вызывается после завершения run).
    pub fn save_output(self) -> Result<(), String> {
        #[cfg(feature = "graphics")]
        if let Some(recorder) = self.graphics_recorder {
            recorder.save()?;
        }
        Ok(())
    }

    /// Возвращает ссылку на Unit для чтения состояния после завершения симуляции.
    pub fn unit(&self) -> &Unit {
        &self.unit
    }

    // -- Вспомогательные методы ------------------------------------------------

    /// Предупреждения об именах, объявленных несколькими моделями.
    ///
    /// Пространство имён значений плоское: по голому имени читается первая нашедшаяся
    /// ветвь, а запись расходится по всем. Теперь двусмысленность названа, и рядом
    /// показано, как адресовать точно.
    pub fn ambiguous_name_warnings(&self) -> Vec<String> {
        self.port_names
            .ambiguous
            .iter()
            .map(|(bare, qualified)| {
                format!(
                    "ВНИМАНИЕ: имя '{bare}' объявлено несколькими моделями ({}). \
                     По голому имени адресуется первая из них; для точного обращения \
                     используйте квалифицированное имя.",
                    qualified.join(", ")
                )
            })
            .collect()
    }

    /// Применяет входы шага: позиционно (историческая форма) либо по именам.
    ///
    /// Возвращает ошибку, если сценарий назвал порт, которого нет, либо имя
    /// двусмысленно.
    fn apply_step_inputs(&mut self, step: &SimStep, step_no: usize) -> Result<(), String> {
        for (values, direction) in [
            (&step.in_ports, PortDirectionKind::In),
            (&step.inout, PortDirectionKind::InOut),
        ] {
            let Some(values) = values else { continue };
            for (name, value) in self.resolve_values(values, direction, step_no)? {
                self.unit.set_port(&name, value);
            }
        }
        Ok(())
    }

    /// Переводит значения шага в пары "имя порта -> значение".
    ///
    /// Общая воронка для входов и для `guard`: разойдясь, они принимали бы разные
    /// имена, и сценарий вёл бы себя по-разному в зависимости от того, в какой половине
    /// шага написано имя. Говорит один раз за прогон, что сценарий пользуется
    /// устаревшей формой.
    ///
    /// Это **не** `SIM-032`: тот о несовпадении **длины** массива с числом портов, а
    /// этот - о самой форме, даже когда длина верна. Слить их значило бы потерять
    /// различие "массив не той длины" и "форма устарела"; на входе с коротким массивом
    /// печатаются оба.
    fn warn_positional_form_once(&self) {
        if self.positional_form_warned.replace(true) {
            return;
        }
        // Код - отдельным литералом, а не внутри текста: проверка
        // `scripts/check-diagnostic-codes.sh` ищет коды именно строковыми литералами
        // вида `"XX-NNN"`, и код, вплавленный в сообщение, для неё невидим - то есть
        // выпадает и из реестра диагностик.
        const CODE: &str = "SIM-037";
        eprintln!(
            "Предупреждение [{CODE}]: сценарий задаёт значения портов позиционным массивом — \
             форма устарела. Индекс в массиве привязан к месту имени в АЛФАВИТНОМ списке портов \
             модели и её под-моделей, поэтому добавление или переименование порта сдвигает весь \
             массив, и шаг начинает описывать другое событие — молча. Пользуйтесь именами: \
             `\"in_ports\": {{\"имя_порта\": значение}}`; при тёзках из разных моделей имя \
             уточняется как `Модель::порт`."
        );
    }

    fn resolve_values(
        &self,
        values: &PortValues,
        direction: PortDirectionKind,
        step_no: usize,
    ) -> Result<Vec<(String, Value)>, String> {
        let names = self.names_of(direction);
        let mut resolved = Vec::new();
        match values {
            PortValues::Positional(list) => {
                self.warn_positional_form_once();
                if list.len() != names.len() {
                    // Предупреждение, а не ошибка: корпус мог опираться на неполные
                    // массивы, и ломать его фича не должна. Код - отдельным литералом
                    // (см.
                    const CODE: &str = "SIM-032";
                    eprintln!(
                        "Предупреждение [{CODE}]: шаг {step_no}: {} значений в позиционном \
                         массиве `{}`, а портов {} — лишние игнорируются, недостающие не задаются",
                        list.len(),
                        direction.field(),
                        names.len()
                    );
                }
                for (i, json_val) in list.iter().enumerate() {
                    if let (Some(name), Some(value)) = (names.get(i), json_to_value(json_val)) {
                        let value = self.as_port_value(name, value);
                        resolved.push((name.clone(), value));
                    }
                }
            }
            PortValues::Named(map) => {
                for (name, json_val) in map {
                    self.check_port_name(name, direction, step_no)?;
                    if let Some(value) = json_to_value(json_val) {
                        resolved.push((name.clone(), self.as_port_value(name, value)));
                    }
                }
            }
        }
        Ok(resolved)
    }

    /// Приводит значение сценария к типу значения модели.
    ///
    /// Сегодня приведение одно: число на значении типа `duration` трактуется как
    /// **миллисекунды** - та же единица, что у `as duration`. Прочие значения
    /// проходят как есть: JSON и так даёт числа, логические и вещественные.
    ///
    /// Имя ищется и в квалифицированной форме (`Модель::имя`): реестр типов собран по
    /// голым именам, поэтому квалификатор снимается.
    fn as_port_value(&self, name: &str, value: crate::Value) -> crate::Value {
        let bare = name.rsplit("::").next().unwrap_or(name);
        match value {
            crate::Value::Number(millis) if self.port_names.durations.contains(bare) => {
                match i64::try_from(millis)
                    .ok()
                    .and_then(takt_lang::semantic::duration::from_millis)
                {
                    Some(ns) => crate::Value::Duration(ns),
                    // Переполнение наносекунд: оставляем число - ошибку даст
                    // вычисление, и она назовёт место, а молчаливой подмены нет.
                    None => crate::Value::Number(millis),
                }
            }
            other => other,
        }
    }

    /// Имена портов заданного направления.
    fn names_of(&self, direction: PortDirectionKind) -> &[String] {
        match direction {
            PortDirectionKind::In => &self.port_names.in_ports,
            PortDirectionKind::Out => &self.port_names.out_ports,
            PortDirectionKind::InOut => &self.port_names.inout_ports,
        }
    }

    /// Проверяет, что имя из сценария адресует ровно один порт нужного направления.
    ///
    /// Направление проверяется намеренно: `in_ports: {"lamp": 1}` при выходном `lamp` -
    /// почти наверняка опечатка, а не задумка.
    fn check_port_name(
        &self,
        name: &str,
        direction: PortDirectionKind,
        step_no: usize,
    ) -> Result<(), String> {
        // Коды - отдельными литералами, а не внутри текста сообщения: проверка
        // `scripts/check-diagnostic-codes.sh` собирает печатаемые коды именно строковыми
        // литералами вида `"XX-NNN"`, и вплавленный в текст код для неё не существует -
        // то есть выпадает и из реестра диагностик, причём при зелёном прогоне: с обеих
        // сторон пусто. Возврат к вплавленной форме ловит та же проверка.
        const NOT_FOUND: &str = "SIM-030";
        const AMBIGUOUS: &str = "SIM-031";
        if name.contains("::") {
            // Квалифицированное имя: проверяем существование пары "модель::имя".
            // Направление здесь не сужается - квалификация уже однозначна.
            if !self.port_names.qualified.contains(name) {
                return Err(format!(
                    "Ошибка [{NOT_FOUND}]: шаг {step_no}: порт `{name}` не найден в модели"
                ));
            }
            return Ok(());
        }
        if let Some((_, variants)) = self
            .port_names
            .ambiguous
            .iter()
            .find(|(bare, _)| bare == name)
        {
            return Err(format!(
                "Ошибка [{AMBIGUOUS}]: шаг {step_no}: имя `{name}` объявлено несколькими моделями \
                 ({}) — укажите квалифицированное имя",
                variants.join(", ")
            ));
        }
        if !self.names_of(direction).iter().any(|n| n == name) {
            return Err(format!(
                "Ошибка [{NOT_FOUND}]: шаг {step_no}: порт `{name}` не найден среди портов \
                 направления `{}`",
                direction.field()
            ));
        }
        Ok(())
    }

    fn check_guard(&self, guard: &Guard, step_no: usize) -> Result<(), String> {
        // Порты guard разрешаются той же воронкой, что и входы шага: иначе именованная
        // форма работала бы в одной половине файла и не работала в другой.
        for (values, direction) in [
            (&guard.out, PortDirectionKind::Out),
            (&guard.inout, PortDirectionKind::InOut),
        ] {
            let Some(values) = values else { continue };
            for (name, expected) in self.resolve_values(values, direction, step_no)? {
                let actual = self.unit.get_value(&name);
                if !values_match(&actual, &expected) {
                    return Err(format!(
                        "Guard шага {step_no}: {} ({name}): ожидалось {:?}, получено {:?}",
                        direction.field(),
                        expected,
                        actual
                    ));
                }
            }
        }
        if let Some(vars) = &guard.vars {
            for (var_name, expected_json) in vars {
                let Some(expected) = json_to_value(expected_json) else {
                    continue;
                };
                let actual = self.unit.get_value(var_name);
                if !values_match(&actual, &expected) {
                    return Err(format!(
                        "Guard шага {step_no}: vars[{var_name}]: ожидалось {:?}, получено {:?}",
                        expected, actual
                    ));
                }
            }
        }
        Ok(())
    }
}

// -- Вспомогательные функции ---------------------------------------------------

fn values_match(actual: &Option<Value>, expected: &Value) -> bool {
    match actual {
        None => false,
        Some(v) => match (v, expected) {
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::Real(a), Value::Real(b)) => (a - b).abs() < 1e-9,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Number(a), Value::Real(b)) => (*a as f64 - b).abs() < 1e-9,
            (Value::Real(a), Value::Number(b)) => (a - *b as f64).abs() < 1e-9,
            _ => false,
        },
    }
}

/// Переводит секцию `extern` шага сценария в стенд эталона.
///
/// Значение, которое не переводится в величину симулятора (строка, объект в позиции
/// значения), - **ошибка сценария**, а не молчаливый пропуск: автор написал подмену, и
/// она обязана сработать.
fn extern_stubs_of(
    step: &crate::json_input::SimStep,
    step_no: usize,
) -> Result<crate::context::ExternStubs, String> {
    use crate::json_input::ExternValue;
    let mut stubs = crate::context::ExternStubs::default();
    let Some(declared) = &step.extern_stubs else {
        return Ok(stubs);
    };
    for (name, value) in declared {
        match value {
            ExternValue::Any(raw) => {
                let value = crate::json_input::json_to_value(raw).ok_or_else(|| {
                    format!("шаг {step_no}: значение extern-функции '{name}' не читается")
                })?;
                stubs.declare(name, crate::context::ExternStub::Any(value));
            }
            ExternValue::ByArgument(table) => {
                let mut by_arg = std::collections::HashMap::new();
                for (key, raw) in table {
                    let key: i128 = key.parse().map_err(|_| {
                        format!(
                            "шаг {step_no}: ключ '{key}' таблицы extern-функции '{name}' \
                             не число — таблица ищет по значению первого аргумента"
                        )
                    })?;
                    let value = crate::json_input::json_to_value(raw).ok_or_else(|| {
                        format!(
                            "шаг {step_no}: значение extern-функции '{name}' при аргументе \
                             {key} не читается"
                        )
                    })?;
                    by_arg.insert(key, value);
                }
                stubs.declare(name, crate::context::ExternStub::ByArgument(by_arg));
            }
        }
    }
    Ok(stubs)
}
