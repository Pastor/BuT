pub(crate) mod aggregate;
mod c;
mod call_order;
mod chain_site;
pub(crate) mod enum_compare;
// Перенос комментариев автора модели в вывод.
pub mod comments;
// Шапка порождённого файла: текст один на все цели, обрамление накладывает целевой
// язык.
pub(crate) mod header;
mod indent;
pub mod keywords;
pub(crate) mod local_stub;
pub(crate) mod mixed_sign;
mod rust;
mod shift_width;
mod site;
// Границы среза живут в семантике: это свойство языка, и разворот среза в аргументе
// вызова спрашивает те же значения.
pub(crate) use crate::semantic::slice;
mod st;
mod struct_order;
mod sv;
pub(crate) mod table;

use crate::diagnostics::Diagnostic;
use crate::semantic::ModelNode;

/// Поддерживаемые языки генерации кода.
///
/// Помечен `#[non_exhaustive]`: список целевых языков будет расширяться, и добавление
/// вариантов не должно ломать обратную совместимость.
#[derive(Debug)]
#[non_exhaustive]
pub enum Language {
    /// Генерация C-кода.
    C,
    /// Генерация Structured Text (IEC 61131-3) - язык ПЛК.
    ///
    /// Модель -> `FUNCTION_BLOCK`, состояния -> `CASE state OF`. Потребление карты
    /// адресов (`AT %...`) включается флагом [`GenerateOptions::hal`] - тем же, что и
    /// для режима `c-hal`.
    ST,
    /// Генерация `no_std` Rust - прошивка микроконтроллера.
    ///
    /// Модель -> `struct`, состояния -> `enum` + `match`, порты -> трейт `Hal` вместо
    /// пары указателей на функции и `void *userdata` цели `c`. Вывод - один `.rs`-файл,
    /// подключаемый пользователем через `mod`; `Cargo.toml` генератор не порождает и им
    /// не владеет.
    ///
    /// Карта адресов ([`GenerateOptions::address_map`]) **не потребляется**: порты идут
    /// через HAL - это аналог режима `c`, а не `c-hal`.
    Rust,
    /// Генерация синтезируемого SystemVerilog (IEEE 1800) - FPGA/ASIC.
    ///
    /// Первая **аппаратная** цель: у `C`/`ST`/`Rust` такт - итерация цикла
    /// сканирования, здесь такт Takt == **фронт тактового сигнала** `posedge clk`.
    /// Модель -> `module`, состояния -> `typedef enum` + `unique case`, порты
    /// `in`/`out` -> `input`/`output logic`.
    ///
    /// Сброс синхронный, активный низкий (`rst_n`); стартовое состояние стоит в ветви
    /// сброса, синтетического `INIT` нет - контракт
    /// [](../../../../../../docs/features/0033-init-tick-alignment.md#архитектура-adr)
    /// выполняется конструктивно, а не правкой.
    ///
    /// Карта адресов ([`GenerateOptions::address_map`]) **не потребляется**: MMIO-адрес
    /// для RTL бессмыслен - сигнал приходит на вывод кристалла, а не по адресу. Парная
    /// цель - [`SvMmio`](Language::SvMmio).
    SV,
    /// Генерация синтезируемого SystemVerilog с **регистровым файлом** - порты с
    /// адресом становятся битами регистров на шинно-агностичном интерфейсе.
    ///
    /// Парная к [`SV`](Language::SV), как `c-hal` парная к `c` (прецедент
    /// ). В отличие от `sv`, карта адресов
    /// ([`GenerateOptions::address_map`]) **потребляется**: порт **с** адресом ->
    /// бит регистра (направление принадлежит биту, ); порт
    /// **без** адреса -> порт модуля. Модуль получает синхронный регистровый
    /// интерфейс (`reg_addr`/`reg_wdata`/`reg_wen`/`reg_rdata`) **без протокола**:
    /// адаптеры APB/AXI-Lite/Wishbone - отдельные фичи по требованию (
    /// Option B). Автомат, композиция и сброс - те же, что у `sv`.
    SvMmio,
}

/// Ширина вещественного типа в порождаемом C.
///
/// Умолчание - [`W64`](FloatWidth::W64) (`double`): симулятор считает в f64
/// (`eval::Value::Real`), и без совпадения точности сверка модели с синтезированным
/// кодом по `float` недостижима. [`W32`](FloatWidth::W32) (`float`) остаётся для
/// платформ, где 8-байтное чтение недопустимо: цена умолчания - ширина вещественного
/// порта `c-hal` 4 -> 8 байт.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum FloatWidth {
    /// `float` - 4 байта (f32).
    W32,
    /// `double` - 8 байт (f64), совпадает с точностью симулятора.
    #[default]
    W64,
}

/// Опции генерации кода.
///
/// Заменяет "голый" булев флаг `guard_enable` на именованную структуру опций - вызов
/// `generate(model, path, &options)` читается лучше, чем `generate(..., true)`.
/// Помечена `#[non_exhaustive]`: набор опций будет расширяться без слома обратной
/// совместимости; конструирование - через [`GenerateOptions::new`] либо [`Default`] с
/// последующей правкой полей.
#[derive(Debug, Clone)]
#[non_exhaustive]
pub struct GenerateOptions {
    /// Генерировать guard-проверки в целевом коде.
    pub guard_enable: bool,
    /// Комментарии автора модели для переноса в вывод.
    ///
    /// `None` - переносить нечего: так работают потребители, у которых нет исходника
    /// (библиотечный API поверх готового дерева). Заполняет конвейер, который
    /// единственный видит и текст, и разбор.
    pub comments: Option<std::rc::Rc<comments::SourceComments>>,
    /// Режим `c-hal`: эмитить таблицу адресов портов и принятую по умолчанию реализацию HAL
    /// (`*(volatile T*)addr`). В обычном режиме `c` - `false`, вывод не меняется.
    pub hal: bool,
    /// Разрешённые адреса портов (`имя порта -> адрес`) для режима [`hal`].
    ///
    /// Заполняется из [`resolve_addresses`](crate::address_map::resolve_addresses)
    /// (приоритет inline < `address` < внешняя карта). В обычном режиме пуста.
    pub address_map: std::collections::HashMap<String, crate::address_map::ResolvedAddress>,
    /// Ширина вещественного типа в порождаемом C. Умолчание - [`FloatWidth::W64`];
    /// CLI-флаг `--float-width=32|64`.
    pub float_width: FloatWidth,
    /// Глобальная точность `q(m, n)`, которой реализуется `float`.
    ///
    /// `None` (умолчание) - прежнее поведение: `float` нативен (`c`/`rust`/`st`) либо
    /// `SV-003` (`sv`). `Some((m, n))` (CLI-флаг `--float-as-q=m.n`) - глобальная
    /// точность подстановки `float -> q(m, n)`: цель `sv` применяет её всегда (снимая
    /// `SV-003`), цели `c`/`rust`/`st` - только при
    /// [`float_embedded`](Self::float_embedded).
    pub float_as_q: Option<(u8, u8)>,
    /// Частота тактирования для профиля "такты", в герцах.
    ///
    /// `None` (умолчание) - профиль **"часы"**: длительность меряется миллисекундами
    /// внешнего источника времени, частота не нужна. `Some(hz)` (CLI-флаг `--tick-hz`) -
    /// профиль **"такты"**: длительность пересчитывается в число тактов. Флаг
    /// **переопределяет** объявление `clock` в модели; приоритет разрешает
    /// [`duration::resolve_profile`](crate::semantic::duration::resolve_profile).
    ///
    /// Это второе осознанное исключение из правила "ключ сборки не меняет логику";
    /// первое - `--float-as-q` с `--float-embedded`. Здесь исключение уже: в профиле
    /// "часы" флага нет вовсе.
    pub tick_hz: Option<u64>,
    /// Режим `--parameters=specialize`: инстанцирования с аргументами заменяются
    /// копиями моделей с подставленными значениями - между стадиями 1 и 2 семантики
    /// (`semantic/specialize.rs`). Умолчание `false` - режим `assign`: модель одна,
    /// значения присваиваются полям экземпляров. Оба режима обязаны давать одинаковое
    /// поведение: тождественность держит потактовая сверка.
    pub specialize: bool,
    /// Guard границ массива в порождённом коде, CLI-флаг `--bounds-check`. Умолчание -
    /// `false`.
    ///
    /// При включении проход `semantic::bounds_guard` оборачивает операторы с
    /// индексацией **переменным** индексом проверкой и заводит синтетический выходной
    /// порт `bounds_fault`: доступ за границей не выполняется, а признак уходит наружу.
    /// Литеральный и константный индекс судит семантика (`SE-028`).
    ///
    /// Умолчание `false` - тоже: guard стоит тактов и вентилей, а включение изменило бы
    /// вывод всего корпуса.
    pub bounds_check: bool,
    /// Для целей `c`/`rust`/`st`: реализовать `float` целочисленным Q-путём (embedded
    /// без FPU) вместо нативного. Действует только вместе с
    /// [`float_as_q`](Self::float_as_q); на `sv` не влияет (там `float` всегда `q`).
    pub float_embedded: bool,
    /// Адаптер шины для цели `sv-mmio`.
    ///
    /// `None` - адаптера нет, вывод прежний байт-в-байт. `Some(Bus::Apb)` - рядом с
    /// ядром порождается обёртка `<name>_apb.sv`, транслирующая APB в шинно-агностичный
    /// регистровый интерфейс ядра.
    ///
    /// Ядро при этом **не меняется**: на нём стоят потактовые сверки регистров и проверка
    /// двух инструментов SV.
    pub bus: Option<Bus>,
    /// Форма печати автомата, CLI-флаг `--fsm=switch|table`.
    ///
    /// Потребляет её цель `c` (и `c-hal` - генератор у них общий); прочие цели
    /// табличную форму не печатают, и CLI отвергает флаг у них **с названием
    /// поддерживающих целей**, а не молча.
    pub fsm: FsmForm,
    /// Подстановка тела функции в место вызова, CLI-флаг `--inline=off|auto`. Умолчание -
    /// [`InlinePolicy::Off`].
    ///
    /// Флаг управляет только эвристикой ("тело не длиннее пяти операторов и от одного
    /// до трёх вызовов"): атрибут `[inline]` автора действует всегда, а `[noinline]`
    /// всегда подавляет. Умолчание `off`: включение изменило бы вывод всего корпуса
    /// разом - так же устроены `--bounds-check` и `--fsm`.
    pub inline: InlinePolicy,
}

/// Режим эвристики подстановки.
///
/// Подстановка меняет **форму** вывода, а не поведение: тест - потактовая сверка
/// эталона с прошивкой, а эталон проход не зовёт (иначе сверка перестала бы видеть
/// дефект подстановки).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum InlinePolicy {
    /// Подставляются только функции с атрибутом `[inline]`.
    #[default]
    Off,
    /// Плюс эвристика: небольшое тело и 1...3 вызова.
    Auto,
}

/// Форма, которой цель печатает автомат.
///
/// Умолчание - [`Switch`](FsmForm::Switch): переход вкраплён в тело `case`.
/// [`Table`](FsmForm::Table) печатает **отношение переходов данными** - таблицу "откуда ->
/// страж -> действие -> куда" и общий диспетчер; так автоматный подход виден в самом
/// порождённом коде, а таблицу можно прочитать отдельно от тел состояний.
///
/// Флаг меняет **форму** вывода, а не поведение: обе формы обязаны давать одну
/// потактовую трассу, и тест этому - сверка, а не факт компиляции
/// (`conformance_fsm_table_tests`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum FsmForm {
    /// `switch` по состоянию; переходы печатаются внутри ветвей.
    #[default]
    Switch,
    /// Таблица переходов + диспетчер (`--fsm=table`).
    Table,
}

/// Протокол шины для адаптера цели `sv-mmio`.
///
/// Зонт: протокол берётся **по требованию **, а не "на всякий случай" (карточка фичи).
/// Первым выбран APB - экосистема ARM/AMBA.
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
            comments: None,
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
            comments: None,
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

/// Один файл вывода цели: имя (с расширением) и текст.
///
/// Имя - **имя файла**, а не путь: куда его положить, решает тот, кто пишет на диск, а
/// в браузере диска нет вовсе.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GeneratedFile {
    /// Имя файла вместе с расширением (`heater.h`, `heater_apb.sv`).
    pub name: String,
    /// Текст файла.
    pub text: String,
}

/// Вывод цели без диска: файлы и предупреждения цели.
#[derive(Debug, Clone, Default)]
pub struct Output {
    /// Файлы вывода в том порядке, в каком цель их печатает.
    pub files: Vec<GeneratedFile>,
    /// Предупреждения цели - см.
    pub warnings: Vec<Diagnostic>,
}

/// Интерфейс генератора кода для языка Takt.
pub trait Generator {
    /// Печатает вывод цели **в память**: файлы и предупреждения.
    ///
    /// Возвращает **предупреждения цели** - то, что цель хочет сказать автору, но что
    /// не мешает выпустить код: `ST-009` (тело внешней функции подменено заглушкой),
    /// `ST-022` (охранная формула не переводится в IEC), `RS-010`/`ST-010` (LTL-формула
    /// в вывод не попала), `SV-009` (переменный делитель).
    ///
    /// Печатать их генератор **не имеет права**. Копий было три потому, что другого
    /// выхода наружу у генератора **не было по типу** - этот тип и есть выход.
    ///
    /// Диска цель не касается **по типу**: запись делает [`write_output`], один раз на
    /// все цели.
    fn generate_texts(
        &self,
        model: &ModelNode,
        options: &GenerateOptions,
    ) -> Result<Output, Diagnostic>;

    /// Диагностика неудачной записи файла на диск.
    ///
    /// Код принадлежит цели (`CC-010`, `ST-001`, `RS-001`, `SV-001`), поэтому
    /// общий носитель записи спрашивает его здесь, а не печатает свой: сообщение об
    /// отказе диска обязано быть узнаваемым по коду.
    fn write_failure(&self, error: &std::io::Error) -> Diagnostic;
}

/// Возвращает генератор языка.
///
/// Точка выбора - одна: и печать в память, и печать с записью на диск идут через неё,
/// иначе списки целей разошлись бы.
fn generator_of(l: &Language) -> Box<dyn Generator> {
    match l {
        Language::C => Box::new(c::Generator {}),
        Language::ST => Box::new(st::Generator {}),
        Language::Rust => Box::new(rust::Generator {}),
        Language::SV => Box::new(sv::Generator { mmio: false }),
        Language::SvMmio => Box::new(sv::Generator { mmio: true }),
    }
}

/// Печатает вывод цели **в память**.
///
/// Вход потребителя, у которого нет файловой системы: модуля WebAssembly, теста, сверки
/// двух прогонов. Запись на диск - [`generate`], та же печать плюс [`write_output`].
pub fn generate_texts(
    l: Language,
    model: &ModelNode,
    options: &GenerateOptions,
) -> Result<Output, Diagnostic> {
    // Позиция оператора - потоковое состояние: без сброса координата последнего
    // оператора пережила бы вызов и досталась бы следующей генерации в том же потоке.
    site::reset();
    // Комментарии автора - тоже потоковое состояние, и по той же причине: печатники
    // четырёх целей принимают три разных контекста, и протаскивать носитель через все
    // сигнатуры дороже, чем держать его рядом с позицией оператора.
    comments::activate(options.comments.clone());
    let result = generator_of(&l).generate_texts(model, options);
    // Снимается на всех путях выхода, включая отказ: иначе следующая генерация в том же
    // потоке взяла бы комментарии чужого исходника.
    comments::reset();
    result
}

/// Запускает генератор кода для заданного языка и пишет файлы на диск.
///
/// Возвращает предупреждения цели - см.
pub fn generate(
    l: Language,
    model: &ModelNode,
    output_path: &str,
    options: &GenerateOptions,
) -> Result<Vec<Diagnostic>, Diagnostic> {
    site::reset();
    comments::activate(options.comments.clone());
    let generator = generator_of(&l);
    let output = generator.generate_texts(model, options);
    comments::reset();
    let output = output?;
    write_output(&output.files, output_path, generator.as_ref())?;
    Ok(output.warnings)
}

/// Кладёт готовые файлы цели в каталог `output_path`.
///
/// Вход для того, кто уже получил вывод [`generate_texts`] и решил его сохранить: цель
/// нужна, чтобы отказ диска пришёл с её кодом.
pub fn write_files(
    l: Language,
    files: &[GeneratedFile],
    output_path: &str,
) -> Result<(), Diagnostic> {
    write_output(files, output_path, generator_of(&l).as_ref())
}

/// Кладёт файлы вывода в каталог `output_path`.
///
/// Каталог создаётся молча: отсутствие права на создание увидит сама запись, а
/// существующий каталог ошибкой не является.
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

    /// Каталог теста уникален по потоку И процессу.
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

    /// Строит модель с именем `Root` - как это делает `compile_to_*`.
    fn model_of(src: &str) -> std::rc::Rc<std::cell::RefCell<ModelNode>> {
        let (ast, _) = crate::parse(src, 0).unwrap();
        let model = construct_model(&ast, None, &[]).unwrap();
        model.borrow_mut().name = Some("Root".to_string());
        model
    }

    /// Цель `c` отдаёт **два** файла - заголовок и исходник.
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
    /// Тест против расхождения двух путей вывода: печать одна, запись - обёртка над
    /// ней, и никакая цель не вправе печатать на диск иначе, чем отдаёт наружу.
    #[test]
    fn written_files_match_texts() {
        let src = "var x: u8 := 0; start S { always { x := 1; } }";
        // Язык берётся функцией, а не значением: `Language` намеренно не `Clone`
        // (`#[non_exhaustive]`, расширяемый список), а нужен он дважды - печати в
        // память и печати с записью.
        fn language_of(tag: &str) -> Language {
            match tag {
                "c" => Language::C,
                "st" => Language::ST,
                "rust" => Language::Rust,
                _ => Language::SV,
            }
        }
        for tag in ["c", "st", "rust", "sv"] {
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
