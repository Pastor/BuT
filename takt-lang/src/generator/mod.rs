pub(crate) mod aggregate;
mod c;
mod call_order;
mod chain_site;
pub(crate) mod enum_compare;
// Шапка порождённого файла (фича 0535): текст один на все цели, обрамление
// накладывает целевой язык.
pub(crate) mod header;
mod indent;
pub mod keywords;
pub(crate) mod local_stub;
pub(crate) mod mixed_sign;
mod plantuml;
mod rust;
mod shift_width;
mod site;
// Границы среза живут в семантике (фича 0400): это свойство языка, и
// разворот среза в аргументе вызова спрашивает те же значения.
pub(crate) use crate::semantic::slice;
mod st;
mod struct_order;
mod sv;
pub(crate) mod table;

use crate::diagnostics::Diagnostic;
use crate::semantic::ModelNode;

/// Поддерживаемые языки генерации кода.
///
/// Помечен `#[non_exhaustive]`: список целевых языков будет расширяться, и
/// добавление вариантов не должно ломать обратную совместимость (правило 11).
#[derive(Debug)]
#[non_exhaustive]
pub enum Language {
    /// Генерация C-кода.
    C,
    /// Генерация диаграммы состояний PlantUML.
    PlantUML,
    /// Генерация Structured Text (IEC 61131-3) — язык ПЛК (фича 0041).
    ///
    /// Модель → `FUNCTION_BLOCK`, состояния → `CASE state OF` (ADR 0041,
    /// Option A). Потребление карты адресов (`AT %…`) включается флагом
    /// [`GenerateOptions::hal`] — тем же, что и для режима `c-hal`.
    ST,
    /// Генерация `no_std` Rust — прошивка микроконтроллера (фича 0050).
    ///
    /// Модель → `struct`, состояния → `enum` + `match`, порты → трейт `Hal`
    /// вместо пары указателей на функции и `void *userdata` цели `c`
    /// (ADR 0050, Option A по обеим развилкам). Вывод — один `.rs`-файл,
    /// подключаемый пользователем через `mod`; `Cargo.toml` генератор не
    /// порождает и им не владеет.
    ///
    /// Карта адресов ([`GenerateOptions::address_map`]) **не потребляется**:
    /// порты идут через HAL — это аналог режима `c`, а не `c-hal`.
    Rust,
    /// Генерация синтезируемого SystemVerilog (IEEE 1800) — FPGA/ASIC (фича 0045).
    ///
    /// Первая **аппаратная** цель: у `C`/`ST`/`Rust` такт — итерация цикла
    /// сканирования, здесь такт Takt ≡ **фронт тактового сигнала** `posedge clk`.
    /// Модель → `module`, состояния → `typedef enum` + `unique case`, порты
    /// `in`/`out` → `input`/`output logic` (ADR 0045).
    ///
    /// Сброс синхронный, активный низкий (`rst_n`); стартовое состояние стоит в
    /// ветви сброса, синтетического `INIT` нет — контракт
    /// [ADR 0033](../../../../../../docs/features/0033-init-tick-alignment.md#архитектура-adr) выполняется
    /// конструктивно, а не правкой.
    ///
    /// Карта адресов ([`GenerateOptions::address_map`]) **не потребляется**:
    /// MMIO-адрес для RTL бессмыслен — сигнал приходит на вывод кристалла, а не
    /// по адресу. Парная цель — [`SvMmio`](Language::SvMmio).
    SV,
    /// Генерация синтезируемого SystemVerilog с **регистровым файлом** — порты с
    /// адресом становятся битами регистров на шинно-агностичном интерфейсе (фича
    /// 0062).
    ///
    /// Парная к [`SV`](Language::SV), как `c-hal` парная к `c` (прецедент
    /// 0020-05). В отличие от `sv`, карта адресов
    /// ([`GenerateOptions::address_map`]) **потребляется**: порт **с** адресом →
    /// бит регистра (направление принадлежит биту, ADR 0062, правило 4); порт
    /// **без** адреса → порт модуля. Модуль получает синхронный регистровый
    /// интерфейс (`reg_addr`/`reg_wdata`/`reg_wen`/`reg_rdata`) **без протокола**:
    /// адаптеры APB/AXI-Lite/Wishbone — отдельные фичи по требованию (ADR 0062,
    /// Option B). Автомат, композиция и сброс — те же, что у `sv`.
    SvMmio,
}

/// Ширина вещественного типа в порождаемом C (фича 0029, ADR Option R-C).
///
/// Умолчание — [`W64`](FloatWidth::W64) (`double`): симулятор считает в f64
/// (`eval::Value::Real`), и без совпадения точности сверка модели с
/// синтезированным кодом по `float` недостижима. [`W32`](FloatWidth::W32)
/// (`float`) остаётся для платформ, где 8-байтное чтение недопустимо: цена
/// умолчания — ширина вещественного порта `c-hal` 4 → 8 байт.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum FloatWidth {
    /// `float` — 4 байта (f32).
    W32,
    /// `double` — 8 байт (f64), совпадает с точностью симулятора.
    #[default]
    W64,
}

/// Опции генерации кода.
///
/// Заменяет «голый» булев флаг `guard_enable` на именованную структуру опций —
/// вызов `generate(model, path, &options)` читается лучше, чем `generate(..., true)`.
/// Помечена `#[non_exhaustive]`: набор опций будет расширяться без слома обратной
/// совместимости (правило 11); конструирование — через [`GenerateOptions::new`]
/// либо [`Default`] с последующей правкой полей.
#[derive(Debug, Clone)]
#[non_exhaustive]
pub struct GenerateOptions {
    /// Генерировать guard-проверки в целевом коде.
    pub guard_enable: bool,
    /// Режим `c-hal` (фича 0020-05): эмитить таблицу адресов портов и дефолтную
    /// реализацию HAL (`*(volatile T*)addr`). В обычном режиме `c` — `false`,
    /// вывод не меняется.
    pub hal: bool,
    /// Разрешённые адреса портов (`имя порта → адрес`) для режима [`hal`].
    ///
    /// Заполняется из [`resolve_addresses`](crate::address_map::resolve_addresses)
    /// (приоритет inline < `address` < внешняя карта). В обычном режиме пуста.
    pub address_map: std::collections::HashMap<String, crate::address_map::ResolvedAddress>,
    /// Ширина вещественного типа в порождаемом C (фича 0029). Умолчание —
    /// [`FloatWidth::W64`]; CLI-флаг `--float-width=32|64`.
    pub float_width: FloatWidth,
    /// Глобальная точность `q(m, n)`, которой реализуется `float` (фича 0096).
    ///
    /// `None` (умолчание) — прежнее поведение: `float` нативен (`c`/`rust`/`st`)
    /// либо `SV-003` (`sv`). `Some((m, n))` (CLI-флаг `--float-as-q=m.n`) —
    /// глобальная точность подстановки `float → q(m, n)`: цель `sv` применяет её
    /// всегда (снимая `SV-003`), цели `c`/`rust`/`st` — только при
    /// [`float_embedded`](Self::float_embedded). Границы — правило 1 ADR 0061.
    pub float_as_q: Option<(u8, u8)>,
    /// Частота тактирования для профиля «такты» (фича 0134), в герцах.
    ///
    /// `None` (умолчание) — профиль **«часы»**: длительность меряется
    /// миллисекундами внешнего источника времени, частота не нужна.
    /// `Some(hz)` (CLI-флаг `--tick-hz`) — профиль **«такты»**: длительность
    /// пересчитывается в число тактов. Флаг **переопределяет** объявление
    /// `clock` в модели; приоритет разрешает
    /// [`duration::resolve_profile`](crate::semantic::duration::resolve_profile).
    ///
    /// ⚠️ Это **второе осознанное исключение** из принципа «CLI не меняет
    /// логику» (правило 13 ADR 0134): первое — `--float-as-q`/`--float-embedded`
    /// (действие A-7 фичи 0096). Исключение уже, чем у 0096: в профиле «часы»
    /// флага нет вовсе.
    pub tick_hz: Option<u64>,
    /// Режим `--parameters=specialize` (фича 0185): инстанцирования с
    /// аргументами заменяются копиями моделей с подставленными значениями —
    /// между стадиями 1 и 2 семантики (`semantic/specialize.rs`). Умолчание
    /// `false` — режим `assign`: модель одна, значения присваиваются полям
    /// экземпляров. ⚠️ Оба режима обязаны давать одинаковое поведение
    /// (потактовая сверка — сторож гибрида, ADR 0185 Option E).
    pub specialize: bool,
    /// Guard границ массива в порождённом коде (фича 0433), CLI-флаг
    /// `--bounds-check`. Умолчание — `false`.
    ///
    /// При включении проход `semantic::bounds_guard` оборачивает операторы с
    /// индексацией **переменным** индексом проверкой и заводит синтетический
    /// выходной порт `bounds_fault`: доступ за границей не выполняется, а
    /// признак уходит наружу (решение заказчика 2026-08-23). Литеральный и
    /// константный индекс судит семантика (`SE-028`, фичи 0028 и 0434).
    ///
    /// ⚠️ Умолчание `false` — тоже решение заказчика: guard стоит тактов и
    /// вентилей, а включение изменило бы вывод всего корпуса.
    pub bounds_check: bool,
    /// Для целей `c`/`rust`/`st`: реализовать `float` целочисленным Q-путём
    /// (embedded без FPU) вместо нативного (фича 0096, CLI-флаг
    /// `--float-embedded`). Действует только вместе с [`float_as_q`](Self::float_as_q);
    /// на `sv` не влияет (там `float` всегда `q`).
    pub float_embedded: bool,
    /// Адаптер шины для цели `sv-mmio` (фича 0169, CLI-флаг `--bus`).
    ///
    /// `None` — адаптера нет, вывод прежний байт-в-байт. `Some(Bus::Apb)` —
    /// рядом с ядром порождается обёртка `<name>_apb.sv`, транслирующая APB в
    /// шинно-агностичный регистровый интерфейс ядра.
    ///
    /// ⚠️ Ядро при этом **не меняется**: на нём стоят потактовые сверки
    /// регистров и гейт двух инструментов SV (ADR 0169, Option C).
    pub bus: Option<Bus>,
    /// Форма печати автомата (фича 0435), CLI-флаг `--fsm=switch|table`.
    ///
    /// Потребляет её цель `c` (и `c-hal` — генератор у них общий); прочие цели
    /// табличную форму не печатают, и CLI отвергает флаг у них **с названием
    /// поддерживающих целей**, а не молча.
    pub fsm: FsmForm,
    /// Подстановка тела функции в место вызова (фича 0444), CLI-флаг
    /// `--inline=off|auto`. Умолчание — [`InlinePolicy::Off`].
    ///
    /// Флаг управляет только **эвристикой** («тело не длиннее пяти операторов
    /// и 1…3 вызова»): атрибут `[inline]` автора действует всегда, а
    /// `[noinline]` всегда подавляет. Умолчание `off` — решение заказчика: у
    /// эвристики нет адресата, который бы её просил, а включение изменило бы
    /// вывод всего корпуса разом (тот же порядок у `--bounds-check` 0433 и
    /// `--fsm` 0435).
    pub inline: InlinePolicy,
}

/// Режим эвристики подстановки (фича 0444).
///
/// ⚠️ Подстановка меняет **форму** вывода, а не поведение: сторож — потактовая
/// сверка эталона с прошивкой, а эталон проход не зовёт (иначе сверка перестала
/// бы видеть дефект подстановки).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum InlinePolicy {
    /// Подставляются только функции с атрибутом `[inline]`.
    #[default]
    Off,
    /// Плюс эвристика: небольшое тело и 1…3 вызова.
    Auto,
}

/// Форма, которой цель печатает автомат (фича 0435).
///
/// Умолчание — [`Switch`](FsmForm::Switch): переход вкраплён в тело `case`.
/// [`Table`](FsmForm::Table) печатает **отношение переходов данными** —
/// таблицу «откуда → страж → действие → куда» и общий диспетчер; так автоматный
/// подход виден в самом порождённом коде, а таблицу можно прочитать отдельно от
/// тел состояний.
///
/// ⚠️ Флаг меняет **форму** вывода, а не поведение: обе формы обязаны давать
/// одну потактовую трассу, и сторож этому — сверка, а не факт компиляции
/// (`conformance_fsm_table_tests`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum FsmForm {
    /// `switch` по состоянию; переходы печатаются внутри ветвей.
    #[default]
    Switch,
    /// Таблица переходов + диспетчер (`--fsm=table`).
    Table,
}

/// Протокол шины для адаптера цели `sv-mmio` (фича 0169).
///
/// Зонт: протокол берётся **по требованию заказчика**, а не «на всякий случай»
/// (карточка фичи). Первым выбран APB — экосистема ARM/AMBA.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Bus {
    /// AMBA APB3, slave: `psel`/`penable`/`pwrite`/`paddr`/`pwdata`/`prdata`.
    Apb,
}

impl GenerateOptions {
    /// Создаёт опции с указанным режимом guard-проверок (режим `c`, без HAL).
    pub fn new(guard_enable: bool) -> Self {
        Self {
            guard_enable,
            hal: false,
            address_map: std::collections::HashMap::new(),
            float_width: FloatWidth::default(),
            float_as_q: None,
            float_embedded: false,
            tick_hz: None,
            specialize: false,
            bounds_check: false,
            bus: None,
            fsm: FsmForm::default(),
            inline: InlinePolicy::default(),
        }
    }
}

impl Default for GenerateOptions {
    /// По умолчанию guard-проверки включены, режим HAL выключен.
    fn default() -> Self {
        Self {
            guard_enable: true,
            hal: false,
            address_map: std::collections::HashMap::new(),
            float_width: FloatWidth::default(),
            float_as_q: None,
            float_embedded: false,
            tick_hz: None,
            specialize: false,
            bounds_check: false,
            bus: None,
            fsm: FsmForm::default(),
            inline: InlinePolicy::default(),
        }
    }
}

/// Один файл вывода цели: имя (с расширением) и текст (фича 0531).
///
/// Имя — **имя файла**, а не путь: куда его положить, решает тот, кто пишет на
/// диск, а в браузере диска нет вовсе.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GeneratedFile {
    /// Имя файла вместе с расширением (`heater.h`, `heater_apb.sv`).
    pub name: String,
    /// Текст файла.
    pub text: String,
}

/// Вывод цели без диска: файлы и предупреждения цели (фича 0531).
///
/// # Зачем тип, а не пара
///
/// Предупреждения цели уже ездят отдельным каналом (фича 0168), и файлов у
/// цели бывает больше одного (`c` — заголовок и исходник, `sv` с `--bus` —
/// ядро и адаптер). Пара `(Vec<_>, Vec<_>)` на месте вызова читается как
/// загадка, а порядок её половин ничем не закреплён.
#[derive(Debug, Clone, Default)]
pub struct Output {
    /// Файлы вывода в том порядке, в каком цель их печатает.
    pub files: Vec<GeneratedFile>,
    /// Предупреждения цели (фича 0168) — см. [`Generator::generate_texts`].
    pub warnings: Vec<Diagnostic>,
}

/// Интерфейс генератора кода для языка Takt.
pub trait Generator {
    /// Печатает вывод цели **в память**: файлы и предупреждения (фича 0531).
    ///
    /// Возвращает **предупреждения цели** (фича 0168) — то, что цель хочет
    /// сказать автору, но что не мешает выпустить код: `ST-009` (тело внешней
    /// функции подменено заглушкой), `ST-022` (охранная формула не переводится
    /// в IEC), `RS-010`/`ST-010` (LTL-формула в вывод не попала), `SV-009`
    /// (переменный делитель).
    ///
    /// ⚠️ Печатать их генератор **не имеет права**. Прежде каждая из трёх целей
    /// заканчивала работу своей копией `report`, делавшей `eprintln!` прямо из
    /// библиотеки: `--quiet` такой вывод не глушил, формат разошёлся с общим
    /// (терялась позиция), а тест мог проверить факт предупреждения только
    /// перехватом потока. Копий было три потому, что другого выхода наружу у
    /// генератора **не было по типу** — этот тип и есть выход.
    ///
    /// ⚠️ Диска цель не касается **по типу**: запись делает [`write_output`],
    /// один раз на все цели. Прежде `fs::write` стоял у каждой — семь мест в
    /// пяти модулях, — и потребитель без файловой системы (модуль WebAssembly,
    /// фича 0531) вывода цели получить не мог вовсе.
    fn generate_texts(
        &self,
        model: &ModelNode,
        options: &GenerateOptions,
    ) -> Result<Output, Diagnostic>;

    /// Диагностика неудачной записи файла на диск.
    ///
    /// Код принадлежит **цели** (`CC-010`, `ST-001`, `RS-001`, `SV-001`,
    /// `PU-001`), поэтому общий носитель записи спрашивает его здесь, а не
    /// печатает свой: сообщение об отказе диска обязано быть узнаваемым по
    /// коду так же, как прежде.
    fn write_failure(&self, error: &std::io::Error) -> Diagnostic;
}

/// Возвращает генератор языка.
///
/// Точка выбора — одна: и печать в память, и печать с записью на диск идут
/// через неё, иначе списки целей разошлись бы (класс 0084).
fn generator_of(l: &Language) -> Box<dyn Generator> {
    match l {
        Language::C => Box::new(c::Generator {}),
        Language::PlantUML => Box::new(plantuml::Generator {}),
        Language::ST => Box::new(st::Generator {}),
        Language::Rust => Box::new(rust::Generator {}),
        Language::SV => Box::new(sv::Generator { mmio: false }),
        Language::SvMmio => Box::new(sv::Generator { mmio: true }),
    }
}

/// Печатает вывод цели **в память** (фича 0531).
///
/// Вход потребителя, у которого нет файловой системы: модуля WebAssembly,
/// теста, сверки двух прогонов. Запись на диск — [`generate`], та же печать
/// плюс [`write_output`].
pub fn generate_texts(
    l: Language,
    model: &ModelNode,
    options: &GenerateOptions,
) -> Result<Output, Diagnostic> {
    // Позиция оператора — потоковое состояние (фича 0308): без сброса
    // координата последнего оператора пережила бы вызов и досталась бы
    // следующей генерации в том же потоке.
    site::reset();
    generator_of(&l).generate_texts(model, options)
}

/// Запускает генератор кода для заданного языка и пишет файлы на диск.
///
/// Возвращает предупреждения цели (фича 0168) — см. [`Generator::generate_texts`].
pub fn generate(
    l: Language,
    model: &ModelNode,
    output_path: &str,
    options: &GenerateOptions,
) -> Result<Vec<Diagnostic>, Diagnostic> {
    site::reset();
    let generator = generator_of(&l);
    let output = generator.generate_texts(model, options)?;
    write_output(&output.files, output_path, generator.as_ref())?;
    Ok(output.warnings)
}

/// Кладёт готовые файлы цели в каталог `output_path` (фича 0531).
///
/// Вход для того, кто уже получил вывод [`generate_texts`] и решил его
/// сохранить: цель нужна, чтобы отказ диска пришёл с её кодом.
pub fn write_files(
    l: Language,
    files: &[GeneratedFile],
    output_path: &str,
) -> Result<(), Diagnostic> {
    write_output(files, output_path, generator_of(&l).as_ref())
}

/// Кладёт файлы вывода в каталог `output_path`.
///
/// Каталог создаётся молча — как и прежде: отсутствие права на создание
/// увидит сама запись, а существующий каталог ошибкой не является.
fn write_output(
    files: &[GeneratedFile],
    output_path: &str,
    generator: &dyn Generator,
) -> Result<(), Diagnostic> {
    let dir = std::path::Path::new(output_path);
    let _ = std::fs::create_dir(dir);
    for file in files {
        std::fs::write(dir.join(&file.name), &file.text)
            .map_err(|e| generator.write_failure(&e))?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic::tree::construct_model;

    /// Каталог теста уникален по ПОТОКУ И ПРОЦЕССУ (фичи 0190, 0429).
    fn tmp(tag: &str) -> std::path::PathBuf {
        let thread = std::thread::current()
            .name()
            .unwrap_or("main")
            .replace("::", "_");
        let dir = std::env::temp_dir()
            .join(format!("takt_pid{}", std::process::id()))
            .join(format!("takt_0531_{tag}_{thread}"));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).expect("каталог теста");
        dir
    }

    /// Строит модель с именем `Root` — как это делает `compile_to_*`.
    fn model_of(src: &str) -> std::rc::Rc<std::cell::RefCell<ModelNode>> {
        let (ast, _) = crate::parse(src, 0).unwrap();
        let model = construct_model(&ast, None, &[]).unwrap();
        model.borrow_mut().name = Some("Root".to_string());
        model
    }

    /// Цель `c` отдаёт **два** файла — заголовок и исходник.
    ///
    /// Прежде их имена существовали только внутри `fs::write`, и потребитель без
    /// файловой системы (модуль WebAssembly, фича 0531) узнать их не мог.
    #[test]
    fn c_output_carries_header_and_source() {
        let model = model_of("start S;");
        let output = generate_texts(Language::C, &model.borrow(), &GenerateOptions::default())
            .expect("генерация c");
        let names: Vec<&str> = output.files.iter().map(|f| f.name.as_str()).collect();
        assert_eq!(names, vec!["root.h", "root.c"], "имена файлов цели `c`");
        assert!(
            output.files[0].text.contains("#ifndef"),
            "первый файл — заголовок:\n{}",
            output.files[0].text
        );
    }

    /// Записанное на диск **совпадает байт в байт** с напечатанным в память.
    ///
    /// Сторож против расхождения двух путей вывода: печать одна, запись —
    /// обёртка над ней, и никакая цель не вправе печатать на диск иначе, чем
    /// отдаёт наружу.
    #[test]
    fn written_files_match_texts() {
        let src = "var x: u8 := 0; start S { always { x := 1; } }";
        // Язык берётся функцией, а не значением: `Language` намеренно не
        // `Clone` (`#[non_exhaustive]`, расширяемый список), а нужен он дважды
        // — печати в память и печати с записью.
        fn language_of(tag: &str) -> Language {
            match tag {
                "c" => Language::C,
                "plantuml" => Language::PlantUML,
                "st" => Language::ST,
                "rust" => Language::Rust,
                _ => Language::SV,
            }
        }
        for tag in ["c", "plantuml", "st", "rust", "sv"] {
            let dir = tmp(tag);
            let path = dir.to_str().expect("путь каталога");
            let model = model_of(src);
            let options = GenerateOptions::default();
            let output = generate_texts(language_of(tag), &model.borrow(), &options)
                .unwrap_or_else(|d| panic!("генерация {tag}: {}", d.message));

            let model = model_of(src);
            generate(language_of(tag), &model.borrow(), path, &options)
                .unwrap_or_else(|d| panic!("запись {tag}: {}", d.message));

            for file in &output.files {
                let written = std::fs::read_to_string(dir.join(&file.name))
                    .unwrap_or_else(|e| panic!("файл {} цели {tag}: {e}", file.name));
                assert_eq!(written, file.text, "цель {tag}, файл {}", file.name);
            }
            let count = std::fs::read_dir(&dir).expect("чтение каталога").count();
            assert_eq!(
                count,
                output.files.len(),
                "цель {tag} записала не те файлы, что напечатала"
            );
            let _ = std::fs::remove_dir_all(&dir);
        }
    }
}
