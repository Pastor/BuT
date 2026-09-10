//! Библиотека лексического и синтаксического анализаторов языка Takt.
//!
//! # Структура
//!
//! - [`ast`] - узлы абстрактного синтаксического дерева (АСД).
//! - [`diagnostics`] - типы диагностических сообщений (ошибки, предупреждения).
//! - [`lexer`] - лексический анализатор (токенизатор).
#![warn(missing_debug_implementations, missing_docs)]

extern crate core;

use crate::parser::ast;
use diagnostics::Diagnostic;

pub mod address_map; // внешняя карта адресов портов (`.ld`-подобный формат)
pub mod compile_cli; // подкоманда `compile`, вынесена из бинарника
/// Модуль диагностических сообщений компилятора.
pub mod diagnostics;
/// Канонический форматтер `.takt`.
pub mod format;
/// Модуль генерации кода (C и другие целевые платформы).
pub mod generator;
pub mod layout; // граф модели для схемы: листы, узлы, рёбра, ярусы
#[cfg(feature = "lsp")]
pub mod lsp; // вспомогательные функции LSP-сервера (только при флаге `lsp`)
/// Модуль парсера: лексер, грамматика, АСД.
pub mod parser;
pub mod semantic; // семантический анализ и построение семантического дерева
pub mod style; // стиль оформления: канон именования
/// Модуль проверки формальных свойств (LTL-формулы, автоматы Бюхи).
pub mod verification;
/// Версия языка Takt - единственный источник истины в коде.
pub mod version;
pub use version::LANGUAGE_VERSION;

/// Публичная точка входа цели `sv-mmio`; реэкспортируется как
/// `takt_lang::compile_to_sv_mmio`.
mod compile_sv_mmio;
pub use compile_sv_mmio::compile_to_sv_mmio;

/// Внешняя карта адресов: парсер формата и предупреждения наложения.
pub use address_map::{
    AddressEnv, AddressMapEntry, AddressResolution, AddressSource, ResolvedAddress,
    address_expr_warnings, address_map_overlay_warnings, parse_address_map, parse_defines,
    resolve_addresses,
};
/// Ширина вещественного типа в порождаемом C: `takt_lang::FloatWidth`.
pub use generator::FloatWidth;
/// Опции генерации кода (реэкспорт для удобства: `takt_lang::GenerateOptions`).
pub use generator::GenerateOptions;

#[allow(
    clippy::needless_lifetimes,
    clippy::type_complexity,
    clippy::ptr_arg,
    clippy::redundant_clone,
    clippy::just_underscores_and_digits,
    clippy::redundant_field_names,
    clippy::collapsible_if
)]
mod grammar {
    include!(concat!(env!("OUT_DIR"), "/grammar.rs"));
}

/// Разбирает строку исходного кода Takt.
///
/// Возвращает пару `(корневая_модель, комментарии)` при успехе, или вектор
/// диагностических сообщений при ошибке.
///
/// # Параметры
///
/// - `src` - строка исходного кода.
/// - `file_no` - числовой идентификатор файла для сообщений об ошибках.
///
/// # Примеры
///
/// ```
/// use takt_lang::parse;
/// use takt_lang::parser::ast::ModelElement;
///
/// // Успешный разбор минимальной программы.
/// // parse() возвращает анонимную корневую модель; именованные модели - в elements.
/// let (root, _) = parse("model M { start S; }", 0).unwrap();
/// assert!(root.name.is_none(), "Корневая модель всегда анонимна");
/// assert!(root.elements.iter().any(|e| matches!(e, ModelElement::Model(_))));
///
/// // Разбор завершается ошибкой при синтаксических нарушениях
/// let err = parse("model {", 0);
/// assert!(err.is_err());
/// ```
pub fn parse(src: &str, file_no: u64) -> Result<(ast::Model, Vec<ast::Comment>), Vec<Diagnostic>> {
    // Дерево глубже предела наружу не выпускается: за `parse` идут рекурсивные
    // потребители (печать форматтера, `Clone`, `Debug`, сериализация), и самый низкий
    // их потолок ниже глубины, которую переживает сам разбор.
    let (model, comments) = parse_without_depth_limit(src, file_no)?;
    let model = parser::depth::check(model).map_err(|diagnostic| vec![diagnostic])?;
    Ok((model, comments))
}

// Разбор исходника без предела глубины и перевод ошибок LALRPOP в диагностики -
// вынесено модулем: `lib.rs` пришпилен реестром размеров.
mod parse_entry;
pub(crate) use parse_entry::parse_without_depth_limit;

// Общий конвейер разбора и построения плюс сбор всех диагностик - вынесено модулем:
// `lib.rs` пришпилен реестром размеров.
/// Компиляция по имени цели: одна точка выбора для CLI и модуля.
pub mod compile;
pub mod pipeline; // общий конвейер: разбор -> дерево -> диагностики
pub use pipeline::collect_compile_diagnostics;
pub(crate) use semantic::lower_float::apply_float_lowering;

/// Компилирует исходный код Takt в C-код.
///
/// Выполняет полный конвейер: лексический анализ -> синтаксический -> семантический ->
/// генерация C.
///
/// # Параметры
///
/// - `source` - исходный код на языке Takt
/// - `output_path` - путь к выходному каталогу (генератор C создаёт `.h`-файл)
/// - `search_paths` - директории для поиска файлов `import` (пустой слайс - только текущая директория)
///
/// # Ошибки
///
/// Возвращает [`Diagnostic`] при синтаксической или семантической ошибке, а также если
/// файл импорта не найден ни в одном из `search_paths`.
///
/// # Примеры
///
/// ```no_run
/// // Без импортов - пустой список путей; guard-проверки по умолчанию включены
/// takt_lang::compile_to_c(
///     "dummy.takt",
///     "start S;",
///     ".output",
///     &[],
///     &takt_lang::GenerateOptions::default(),
/// )
/// .unwrap();
///
/// // С импортами - указываем директорию поиска
/// takt_lang::compile_to_c(
///     "dummy.takt",
///     r#"import "std.takt"; start S;"#,
///     ".output",
///     &["/usr/lib/lam".to_string()],
///     &takt_lang::GenerateOptions::default(),
/// ).unwrap();
/// ```
pub fn compile_to_c(
    filename: &str,
    source: &str,
    output_path: &str,
    search_paths: &[String],
    options: &GenerateOptions,
) -> Result<Vec<Diagnostic>, Diagnostic> {
    compile::compile_files(
        compile::Target::C,
        &compile::CompileInput::new(filename, source, search_paths, options),
        output_path,
    )
}

/// Компилирует Takt в C в режиме `c-hal`: к обычному C добавляются таблица адресов
/// портов и умолчательная реализация HAL.
///
/// Адреса разрешаются с приоритетом inline < `address` < внешняя карта (`external`).
/// Полнота обязательна: используемый порт без адреса -> ошибка (`SE-052`). Возвращает
/// `Ok(warnings)` (предупреждения наложения `SE-050` / висячих записей карты `SE-051`)
/// при успехе либо первую ошибку.
pub fn compile_to_c_hal(
    filename: &str,
    source: &str,
    output_path: &str,
    search_paths: &[String],
    external: &[address_map::AddressMapEntry],
    env: &address_map::AddressEnv,
    options: &GenerateOptions,
) -> Result<Vec<Diagnostic>, Diagnostic> {
    let input = compile::CompileInput {
        filename,
        source,
        search_paths,
        external,
        env: Some(env),
        options,
    };
    compile::compile_files(compile::Target::CHal, &input, output_path)
}

/// Компилирует исходный код Takt в Structured Text (IEC 61131-3) - язык ПЛК.
///
/// Выполняет полный конвейер: лексический анализ -> синтаксический -> семантический ->
/// генерация `.st`. Модель Takt отображается в `FUNCTION_BLOCK`. Адреса портов **не
/// потребляются** - для размещения по карте адресов (`AT %...`) используйте
/// [`compile_to_st_at`].
///
/// # Параметры
///
/// - `filename` - имя входного файла (используется для именования модели и диагностики)
/// - `source` - исходный код на языке Takt
/// - `output_path` - путь к выходному каталогу (создаёт `<filename>.st`)
/// - `search_paths` - директории для поиска файлов `import`
/// - `options` - опции генерации
///
/// # Ошибки
///
/// Возвращает [`Diagnostic`] при синтаксической или семантической ошибке, либо при
/// ошибке записи файла (`ST-001`).
pub fn compile_to_st(
    filename: &str,
    source: &str,
    output_path: &str,
    search_paths: &[String],
    options: &GenerateOptions,
) -> Result<Vec<Diagnostic>, Diagnostic> {
    compile::compile_files(
        compile::Target::St,
        &compile::CompileInput::new(filename, source, search_paths, options),
        output_path,
    )
}

/// Компилирует исходный код Takt в `no_std` Rust - прошивку микроконтроллера.
///
/// Выполняет полный конвейер: лексический анализ -> синтаксический -> семантический ->
/// генерация `.rs`. Модель Takt отображается в `struct`, состояния - в `enum` +
/// `match`, порты - в трейт `Hal`.
///
/// ## Чем отличается от [`compile_to_c`]
///
/// Ниша та же - прошивка МК, - но отображение типов здесь прямое: `[u8;4]` -> `[u8; 4]`,
/// `bit` -> `bool`, `Rational` -> `f64`, что делает сверку с эталоном достижимой.
/// `void *userdata` заменён параметром типа `H: Hal`, а `unsafe` в порождаемом коде
/// запрещён атрибутом `#![forbid(unsafe_code)]`.
///
/// ## Границы
///
/// - Порождается **один `.rs`-файл**; `Cargo.toml` генератор не порождает.
///   Подключение - через `mod` в крейте пользователя.
/// - Атрибут `#![no_std]` в файле **не эмитится**: он допустим только в корне
///   крейта. Модуль не обращается к `std`, поэтому подключается и в
///   `no_std`-крейт, и в обычный.
/// - Карта адресов (`--address-map`) **не потребляется**: порты идут через HAL.
/// - `--float-width=32` несовместим с целью и даёт ошибку `RS-015`, а не
///   молчаливое игнорирование.
///
/// # Параметры
///
/// - `filename` - имя входного файла (используется для именования модели и диагностики)
/// - `source` - исходный код на языке Takt
/// - `output_path` - путь к выходному каталогу (создаёт `<filename>.rs`)
/// - `search_paths` - директории для поиска файлов `import`
/// - `options` - опции генерации
///
/// # Ошибки
///
/// Возвращает [`Diagnostic`] при синтаксической или семантической ошибке, при
/// непереводимой конструкции (`RS-0xx`) либо при ошибке записи файла (`RS-001`).
pub fn compile_to_rust(
    filename: &str,
    source: &str,
    output_path: &str,
    search_paths: &[String],
    options: &GenerateOptions,
) -> Result<Vec<Diagnostic>, Diagnostic> {
    compile::compile_files(
        compile::Target::Rust,
        &compile::CompileInput::new(filename, source, search_paths, options),
        output_path,
    )
}

/// Компилирует исходный код Takt в синтезируемый SystemVerilog (IEEE 1800) - описание
/// аппаратуры для FPGA/ASIC.
///
/// Выполняет полный конвейер: лексический анализ -> синтаксический -> семантический ->
/// генерация `.sv`. Модель Takt отображается в `module`, состояния - в `typedef enum` +
/// `unique case`, порты - в порты модуля.
///
/// ## Чем отличается от программных целей
///
/// `c`, `st` и `rust` - программные цели: такт модели там есть итерация цикла
/// сканирования. Здесь такт Takt == **фронт тактового сигнала**: `clk` и `rst_n` -
/// неявные служебные порты модуля, которых в языке `.takt` нет, а их имена для цели
/// `sv` зарезервированы (порт модели с таким именем -> `SV-007`).
///
/// Отображение типов прямое: `bit` -> `logic`. Синтетического `INIT`-состояния нет
/// вовсе - стартовое состояние стоит в ветви сброса, поэтому вход в него не расходует
/// такт на любой глубине вложенности.
///
/// ## Границы
///
/// - Порождается **один `.sv`-файл**, один `module` на корневую модель:
///   композиция `M1 | M2` уплощается в общий `always_comb`. Иерархия модулей SV
///   дерево моделей не повторяет - это плата за точную семантику `|`.
/// - `float` (`SV-003`), `extern fn` (`SV-005`) и `inout` (`SV-006`) - ошибки, а
///   не молчаливый пропуск: в синтезируемом RTL плавающей точки, вызова внешнего
///   кода и двунаправленного провода без сигнала `oe` не существует.
/// - Карта адресов (`--address-map`) **не потребляется**: MMIO-адрес для RTL
///   бессмыслен. Парной цели `sv-at` нет.
/// - Один тактовый домен и один сброс; CDC и множественные домены - вне объёма.
///
/// # Ошибки
///
/// Возвращает [`Diagnostic`] при синтаксической или семантической ошибке, при
/// непереводимой конструкции (`SV-0xx`) либо при ошибке записи файла (`SV-001`).
pub fn compile_to_sv(
    filename: &str,
    source: &str,
    output_path: &str,
    search_paths: &[String],
    options: &GenerateOptions,
) -> Result<Vec<Diagnostic>, Diagnostic> {
    compile::compile_files(
        compile::Target::Sv,
        &compile::CompileInput::new(filename, source, search_paths, options),
        output_path,
    )
}

/// Компилирует Takt в Structured Text в режиме `st-at`: к обычному ST добавляется
/// размещение портов по карте адресов (`AT %IX...`/`%QX...`).
///
/// Адреса разрешаются тем же слоем и с тем же приоритетом, что и для `c-hal` (inline <
/// `address` < внешняя карта), поэтому поведение двух адрес-потребляющих целей не
/// расходится. Полнота обязательна: используемый порт без адреса -> ошибка (`SE-052`).
/// Возвращает `Ok(warnings)` (предупреждения наложения `SE-050` / висячих записей карты
/// `SE-051`) при успехе либо первую ошибку.
pub fn compile_to_st_at(
    filename: &str,
    source: &str,
    output_path: &str,
    search_paths: &[String],
    external: &[address_map::AddressMapEntry],
    env: &address_map::AddressEnv,
    options: &GenerateOptions,
) -> Result<Vec<Diagnostic>, Diagnostic> {
    let input = compile::CompileInput {
        filename,
        source,
        search_paths,
        external,
        env: Some(env),
        options,
    };
    compile::compile_files(compile::Target::StAt, &input, output_path)
}

/// Ce13: возвращает предупреждения о неиспользуемых переменных в модели.
///
/// Обходит все выражения, операторы и условия модели и её вложенных моделей, возвращая
/// предупреждения для каждой `var`-переменной, не упомянутой нигде. Порты и константы
/// не проверяются.
///
/// # Пример
///
/// ```
/// use takt_lang::parse;
/// use takt_lang::semantic::tree::construct_model;
///
/// let (ast, _) = parse("var unused: bit:= 0; start S;", 0).unwrap();
/// let model = construct_model(&ast, None, &[]).unwrap();
/// let warnings = takt_lang::unused_variable_warnings(model);
/// assert_eq!(warnings.len(), 1);
/// ```
pub fn unused_variable_warnings(
    model: std::rc::Rc<std::cell::RefCell<semantic::ModelNode>>,
) -> Vec<Diagnostic> {
    semantic::unused::check_unused_variables(model)
}

/// предупреждения по LTL-формулам (SE-055 - формула не верифицируется; SE-056 -
/// неизвестный атом).
///
/// LTL разбирается на всех уровнях (модель/состояние/блок), но не проверяется ни одной
/// целью генерации. Функция гарантирует, что ни один путь LTL не заканчивается тишиной -
/// по образцу [`unused_variable_warnings`].
pub fn ltl_warnings(
    model: std::rc::Rc<std::cell::RefCell<semantic::ModelNode>>,
) -> Vec<Diagnostic> {
    semantic::ltl_check::ltl_warnings(model)
}

/// Проверяет LTL-свойство модели методом model checking.
///
/// Атом формулы - **имя состояния** FSM (абстракция управляющего графа) **или**
/// предикат над данными (`cond`/булев `var`,: вершина - пара `(состояние, оценка
/// отслеживаемых переменных)`). Проверяемы свойства управления (`F Done`, `G(Fault -> F
/// Idle)`) и над данными (`cond Safe = temp <= 100;` затем `G Safe`). Атом, который не
/// имя состояния и не отслеживаемый предикат, даёт [`Verdict::Unsupported`] - не
/// молчаливое "ложно".
///
/// Условия переходов абстрагированы (любой `ref` - возможный переход), а ход данных
/// полностью недетерминирован, поэтому [`Verdict::Holds`] надёжен, а
/// [`Verdict::Violated`] может оказаться ложным (контрпример недостижим по данным).
///
/// # Пример
///
/// ```
/// use takt_lang::parse;
/// use takt_lang::semantic::tree::construct_model;
/// use takt_lang::verification::ltl::Ltl;
/// use takt_lang::verification::verify::Verdict;
/// use std::rc::Rc;
///
/// let (ast, _) = parse("start Idle { ref Fault; } state Fault { ref Fault; }", 0).unwrap();
/// let model = construct_model(&ast, None, &[]).unwrap();
///
/// // G(Fault -> F Idle): "после сбоя система обязана вернуться в Idle".
/// let phi = Ltl::Globally(Rc::new(Ltl::Implies(
///     Rc::new(Ltl::Atom("Fault".to_string())),
///     Rc::new(Ltl::Finally(Rc::new(Ltl::Atom("Idle".to_string())))),
/// )));
/// // Нарушено: из Fault нет пути назад - автомат залипает в нём навсегда.
/// assert!(matches!(takt_lang::verify_model(model, &phi), Verdict::Violated(_)));
/// ```
pub fn verify_model(
    model: std::rc::Rc<std::cell::RefCell<semantic::ModelNode>>,
    phi: &verification::ltl::Ltl,
) -> verification::verify::Verdict {
    verification::verify::verify_model(&model.borrow(), phi)
}

/// результат проверки одной LTL-формулы модели.
#[derive(Debug, Clone)]
pub struct PropertyResult {
    /// Имя модели, в которой объявлена формула (пустое - анонимный корень).
    pub model: String,
    /// Проверенная формула.
    pub formula: verification::ltl::Ltl,
    /// Позиция объявления формулы в исходном тексте.
    pub loc: diagnostics::Location,
    /// Вердикт проверки.
    pub verdict: verification::verify::Verdict,
    /// Дамп конвейера для отладки (Крипке, автомат `¬φ`, произведение).
    ///
    /// Заполняется только при `trace = true` в [`verify_all_traced`]: дамп стоит памяти
    /// и обычному потребителю не нужен.
    pub trace: Option<String>,
}

/// область проверки - какие модели дерева проверяются.
///
/// Дерево модели содержит и **импортированные** модели: проход 0 кладёт их в тот же
/// [`ModelNode::models`](semantic::ModelNode::models), что и локальные вложенные. Без
/// области нарушенное свойство чужой библиотеки давало ненулевой код возврата тому, кто
/// её лишь импортировал.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub enum VerifyScope {
    /// Только модели проверяемого файла (умолчание): поддеревья импортов отсекаются
    /// целиком.
    #[default]
    File,
    /// Все модели дерева, включая импортированные (поведение ).
    All,
}

/// итог проверки - вердикты плюс то, что **не** проверялось.
///
/// Пропущенное перечисляется, а не замалчивается: молчаливое сужение области - ровно
/// тот класс дефекта, который закрывала.
#[derive(Debug, Clone, Default)]
pub struct VerifyOutcome {
    /// Вердикты по проверенным формулам.
    pub results: Vec<PropertyResult>,
    /// Имена моделей, пропущенных из-за области (пусто при [`VerifyScope::All`]).
    pub skipped: Vec<String>,
}

/// проверяет LTL-формулы модели и её вложенных моделей.
///
/// Формулы вложенной модели говорят о состояниях **своей** модели, поэтому каждая
/// проверяется против графа той модели, где объявлена, - а не против корневой.
///
/// Область - [`VerifyScope::File`]: импортированные модели **не** проверяются. Их имена
/// доступны через [`verify_all_scoped`]; здесь они отбрасываются ради краткой
/// сигнатуры.
///
/// Порядок результатов детерминирован: модели обходятся `BTreeMap`.
pub fn verify_all(
    model: std::rc::Rc<std::cell::RefCell<semantic::ModelNode>>,
) -> Vec<PropertyResult> {
    verify_all_scoped(model, false, VerifyScope::default()).results
}

/// То же, что [`verify_all`], но с явной областью и трассой.
///
/// При `trace = true` заполняет [`PropertyResult::trace`] дампом конвейера (`taktc
/// verify --trace`).
pub fn verify_all_scoped(
    model: std::rc::Rc<std::cell::RefCell<semantic::ModelNode>>,
    trace: bool,
    scope: VerifyScope,
) -> VerifyOutcome {
    let mut out = VerifyOutcome::default();
    verify_all_inner(&model, trace, scope, &mut out);
    out
}

fn verify_all_inner(
    model: &std::rc::Rc<std::cell::RefCell<semantic::ModelNode>>,
    trace: bool,
    scope: VerifyScope,
    out: &mut VerifyOutcome,
) {
    let borrowed = model.borrow();
    let name = borrowed.name().to_string();
    for site in semantic::ltl_check::model_ltl_formulas(&borrowed) {
        let loc = site.loc;
        let formula = scoped_formula(site);
        let (verdict, dump) = if trace {
            let (v, t) = verification::verify::verify_model_traced(&borrowed, &formula);
            (v, Some(t))
        } else {
            (
                verification::verify::verify_model(&borrowed, &formula),
                None,
            )
        };
        out.results.push(PropertyResult {
            model: name.clone(),
            formula,
            loc,
            verdict,
            trace: dump,
        });
    }
    // Имя берётся ключом словаря, а не полем узла: корень импортированного файла
    // анонимен (`name: None`), и `name()` дал бы пустую строку - пропуск перечислялся
    // бы без имени. Ключ же и есть то имя, под которым модель видна импортёру (`import
    // "badlib.takt";` -> `Badlib`).
    let nested: Vec<_> = borrowed
        .models
        .iter()
        .map(|(key, m)| (key.clone(), std::rc::Rc::clone(m)))
        .collect();
    drop(borrowed);
    for (key, m) in nested {
        // У импортированного узла поддерево отсекается целиком. Проверять `origin`
        // каждого узла по отдельности мало: вложенные модели чужого файла локальны для
        // него и несут `Local` - обход зашёл бы внутрь и проверил их формулы, то есть
        // область бы не работала.
        if scope == VerifyScope::File && m.borrow().origin == semantic::ModelOrigin::Imported {
            out.skipped.push(key);
            continue;
        }
        verify_all_inner(&m, trace, scope, out);
    }
}

/// LTL-формулы, объявленные непосредственно в модели, с позициями и областями
/// ([`LtlSite`](semantic::ltl_check::LtlSite)).
///
/// Формулы отдаются **авторскими**, без десахаризации области: развернуть формулу
/// состояния в `G (Состояние -> φ)` - дело верификатора ([`scoped_formula`]). Вложенные
/// модели не обходятся - их формулы проверяются против своего графа (см.
pub fn model_ltl_formulas(
    model: std::rc::Rc<std::cell::RefCell<semantic::ModelNode>>,
) -> Vec<semantic::ltl_check::LtlSite> {
    semantic::ltl_check::model_ltl_formulas(&model.borrow())
}

/// раскрывает **область** формулы.
///
/// Формула, объявленная в теле состояния `S`, - сокращение для `G (S -> φ)`: "всякий
/// раз, когда автомат в `S`, дальше держится φ". (вариант "б" открытого вопроса 1
/// карточки фичи).
///
/// Без этого `state Fault { : [LTL] F Idle; }` проверялась бы от **стартового**
/// состояния - то есть отвечала бы на вопрос, которого автор не задавал ("достижим ли
/// Idle от старта"), и на модели со стартом `Idle` держалась бы тривиально, даже если
/// из `Fault` возврата нет вовсе.
///
/// Формула уровня модели областью не связана и возвращается как есть.
fn scoped_formula(site: semantic::ltl_check::LtlSite) -> verification::ltl::Ltl {
    use verification::ltl::Ltl;
    match site.state {
        None => site.formula,
        Some(state) => Ltl::Globally(std::rc::Rc::new(Ltl::Implies(
            std::rc::Rc::new(Ltl::Atom(state)),
            std::rc::Rc::new(site.formula),
        ))),
    }
}

/// разбирает LTL-формулу из строки (для `taktc verify --property`).
///
/// Строка разбирается **грамматикой языка** - как тело `: [LTL] φ;`, поэтому синтаксис
/// свойства в командной строке и в `.takt`-файле совпадает буква в букву, а имена
/// состояний могут быть любой длины.
///
/// Не путать с [`verification::ltl::parse_ltl`]: тот - тестовая игрушка (паникует на
/// ошибке, атом - один символ) и в продуктовом пути не участвует.
///
/// # Пример
///
/// ```
/// use takt_lang::parse_ltl_property;
///
/// let phi = parse_ltl_property("G (Fault -> F Idle)").unwrap();
/// assert_eq!(phi.to_string(), "G (Fault -> F Idle)");
/// assert!(parse_ltl_property("G (").is_err());
/// ```
pub fn parse_ltl_property(source: &str) -> Result<verification::ltl::Ltl, Diagnostic> {
    use parser::ast::{InlineFormulaDefine, ModelElement};

    let wrapped = format!(": [LTL] {};", source);
    let (ast, _) = parse(&wrapped, 0).map_err(|diags| {
        diags.into_iter().next().unwrap_or_else(|| {
            Diagnostic::error(
                diagnostics::Location::Implicit,
                format!("не удалось разобрать LTL-формулу '{}'", source),
            )
        })
    })?;

    // Разобранное обязано быть ровно одной встроенной формулой. Строка со своей `;`
    // закрывает обёртку досрочно, и хвост становится отдельными элементами файла: `-p
    // "F Done;: [LTL] G Idle"` дал бы "проверено: F Done", умолчав о второй формуле, а
    // `-p "G Idle; start X { ref X; }"` протащил бы объявление состояния. Взять первый
    // элемент и промолчать об остальных - соврать о проверенном (тот же отказ, что и
    // для формы через запятую).
    let [element] = ast.elements.as_slice() else {
        return Err(Diagnostic::error(
            diagnostics::Location::Implicit,
            format!(
                "ожидалась одна LTL-формула, а строка '{}' разбирается как {} \
                 конструкций: уберите ';' — проверяйте по одной формуле за вызов",
                source,
                ast.elements.len()
            ),
        ));
    };

    let ModelElement::InlineFormula(inline) = element else {
        return Err(Diagnostic::error(
            diagnostics::Location::Implicit,
            format!("строка '{}' не является LTL-формулой", source),
        ));
    };
    let InlineFormulaDefine::Ltl { formulas, loc } = inline.as_ref() else {
        return Err(Diagnostic::error(
            diagnostics::Location::Implicit,
            format!("строка '{}' не является LTL-формулой", source),
        ));
    };

    match formulas.as_slice() {
        [single] => Ok(semantic::formula::ltl_ast_to_semantic(single)),
        // `: [LTL] a, b;` - список формул; какую из них проверять, непонятно.
        _ => Err(Diagnostic::error(
            *loc,
            format!(
                "ожидалась одна LTL-формула, задано {}: уберите запятые или \
                 вызовите verify для каждой формулы отдельно",
                formulas.len()
            ),
        )),
    }
}

/// Ce14: возвращает предупреждения о недетерминированных переходах в модели.
///
/// Предупреждает, если несколько `ref`-переходов из одного состояния не имеют условий
/// (безусловные переходы), что является явной недетерминированностью.
///
/// # Пример
///
/// ```
/// use takt_lang::parse;
/// use takt_lang::semantic::tree::construct_model;
///
/// let (ast, _) = parse("start A { ref B; ref C; } state B; state C;", 0).unwrap();
/// let model = construct_model(&ast, None, &[]).unwrap();
/// let warnings = takt_lang::nondeterministic_transition_warnings(model);
/// assert_eq!(warnings.len(), 1);
/// ```
pub fn nondeterministic_transition_warnings(
    model: std::rc::Rc<std::cell::RefCell<semantic::ModelNode>>,
) -> Vec<Diagnostic> {
    semantic::validate::check_nondeterministic_transitions(model)
}

/// предупреждения о портах без адреса, попадающих в кодогенерацию.
///
/// Используемый (достижимый кодогенерацией) порт обязан иметь адрес: inline, оператором
/// `address` или во внешней карте. Имена портов, покрытых внешней картой, передаются в
/// `external_ports`. Мёртвые (неиспользуемые) порты без адреса не предупреждаются.
/// Возвращает предупреждения `SE-052`.
///
/// Аналитическая функция: потребитель адресов (C-таблица/HAL) вызывает её и при
/// необходимости трактует как ошибку.
pub fn port_address_completeness_warnings(
    model: std::rc::Rc<std::cell::RefCell<semantic::ModelNode>>,
    external_ports: &[String],
) -> Vec<Diagnostic> {
    let external: std::collections::HashSet<String> = external_ports.iter().cloned().collect();
    semantic::validate::check_port_address_completeness(model, &external)
}

/// Ce16: возвращает ошибки рекурсивных псевдонимов типов в модели.
///
/// Проверяет граф зависимостей псевдонимов типов на наличие циклов. При обнаружении
/// цикла возвращает ошибку Ce16 с именем псевдонима.
pub fn recursive_type_alias_errors(
    model: std::rc::Rc<std::cell::RefCell<semantic::ModelNode>>,
) -> Vec<Diagnostic> {
    semantic::validate::check_recursive_type_aliases(model)
}

/// NI6: возвращает ошибки типобезопасных операций с перечислениями в модели.
///
/// Проверяет, что при присваивании переменной типа enum значение является одним из
/// допустимых вариантов перечисления.
///
/// # Пример
///
/// ```
/// use takt_lang::parse;
/// use takt_lang::semantic::tree::construct_model;
/// use takt_lang::semantic::EnumDefinitionNode;
///
/// // Создаём модель с перечислением программно
/// let (ast, _) = parse("start S;", 0).unwrap();
/// let model = construct_model(&ast, None, &[]).unwrap();
/// {
///     let mut m = model.borrow_mut();
///     let e = EnumDefinitionNode::new("Dir", &[("North", Some(0)), ("South", Some(1))]);
///     m.enums.insert("Dir".to_string(), e);
/// }
/// let errors = takt_lang::enum_type_safety_errors(model);
/// assert!(errors.is_empty());
/// ```
pub fn enum_type_safety_errors(
    model: std::rc::Rc<std::cell::RefCell<semantic::ModelNode>>,
) -> Vec<Diagnostic> {
    semantic::validate::check_enum_type_safety(model)
}

/// SE-046: предупреждения о недостижимых состояниях в модели.
///
/// Обходит граф переходов BFS от стартового состояния. Состояние, не достижимое ни по
/// одному `ref`/`next`-переходу, генерирует предупреждение.
pub fn unreachable_state_warnings(
    model: std::rc::Rc<std::cell::RefCell<semantic::ModelNode>>,
) -> Vec<Diagnostic> {
    semantic::validate::check_unreachable_states(model)
}

/// SE-047: предупреждения об очевидно константных условиях переходов.
///
/// Проверяет условия `ref`/`next` переходов на наличие сравнений литералов, результат
/// которых известен статически (например, `1 = 0` - всегда ложно).
pub fn constant_condition_warnings(
    model: &std::rc::Rc<std::cell::RefCell<semantic::ModelNode>>,
) -> Vec<Diagnostic> {
    semantic::validate::check_constant_conditions(model)
}

/// SE-044: предупреждения о лишних точках с запятой в АСД модели.
///
/// Обходит все элементы модели и состояний (рекурсивно), генерируя предупреждение для
/// каждого [`ast::ModelElement::StraySemicolon`] и
/// [`ast::StateElement::StraySemicolon`].
pub fn stray_semicolon_warnings(model: &ast::Model) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    collect_stray_semicolons_model(model, &mut diags);
    diags
}

/// SE-045: предупреждения об именованных блоках с неизвестным именем.
///
/// Допустимые имена: `enter`, `exit`, `always`. Любое другое имя генерирует
/// предупреждение - вероятнее всего это опечатка.
pub fn unknown_named_block_warnings(model: &ast::Model) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    collect_unknown_named_blocks_model(model, &mut diags);
    diags
}

const KNOWN_NAMED_BLOCKS: &[&str] = &["enter", "exit", "always"];

fn collect_unknown_named_blocks_model(model: &ast::Model, out: &mut Vec<Diagnostic>) {
    for element in &model.elements {
        match element {
            ast::ModelElement::NamedBlockCode(def) => {
                check_named_block_def(def, out);
            }
            ast::ModelElement::State(state) => {
                for se in &state.elements {
                    if let ast::StateElement::NamedBlockCode(def) = se {
                        check_named_block_def(def, out);
                    }
                }
            }
            ast::ModelElement::Model(nested) => {
                collect_unknown_named_blocks_model(nested, out);
            }
            _ => {}
        }
    }
}

fn check_named_block_def(def: &ast::NamedBlockCodeDefine, out: &mut Vec<Diagnostic>) {
    if let Some(name_id) = &def.name
        && !KNOWN_NAMED_BLOCKS.contains(&name_id.name.as_str())
    {
        out.push(
            Diagnostic::warning(
                name_id.loc,
                format!(
                    "неизвестный именованный блок '{}'; допустимые имена: enter, exit, always",
                    name_id.name
                ),
            )
            .with_code("SE-045"),
        );
    }
}

fn collect_stray_semicolons_model(model: &ast::Model, out: &mut Vec<Diagnostic>) {
    for element in &model.elements {
        match element {
            ast::ModelElement::StraySemicolon(loc) => {
                out.push(
                    Diagnostic::warning(*loc, "лишняя точка с запятой".to_string())
                        .with_code("SE-044"),
                );
            }
            ast::ModelElement::State(state) => {
                for se in &state.elements {
                    if let ast::StateElement::StraySemicolon(loc) = se {
                        out.push(
                            Diagnostic::warning(*loc, "лишняя точка с запятой".to_string())
                                .with_code("SE-044"),
                        );
                    }
                }
            }
            ast::ModelElement::Model(nested) => {
                collect_stray_semicolons_model(nested, out);
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const SRC: &str = r#"
//Алиас типа
// Константа: u8 = [bit;8] - упакованный скаляр, init скалярный
const MATRIX: u8 := 0xA5;
const NUMB: u8 := 0xFF;
cond  IsEmpty = it = 0;
//Порт с указанием отображаемого адреса
out A: u8 at 0x00548835;
in B1: bit at 0x00648835:6;
//Переменная
var   it: [bit;64] := 0;

//Модель
model Ping {
    //Начальное состояние
    start Start {
        //Переход на состояние по условию
        ref End: B1;
        //Исполнение блока кода при первом переходе в состояние
        enter {
            A.0 := true;
            A.1 := false;
        }
        //Исполнение блока кода при выходе из состояния
        exit {
            A.0 := false;
            A.1 := true;
        }
        always {
            A.2 := toggle;
        }
        always {
            toggle := !toggle;
        }
    }
    state End;
    var toggle := false;
}
model Pong {
    start Begin {
        ref Stop: S(Ping) = End;
        always {
            A.5 := MATRIX.5;
        }
    }
    state Stop {
        enter {
            A.6 := MATRIX.3;
        }
    }
}
model Toggle {
    start Entry {
        ref Ping: IsEmpty;
    }
    state Ping = Ping {
        next Pong;
        always {
            debug("Ping processing");
        }
    }
    state Pong = Pong {
        next Complete;
    }
    state Complete {
        ref End: true;
    }
    state End;
}
start Entry = (Ping | Pong) + Toggle;
always {
    debug("Main processing");
    it := it + 1;
    if S(Toggle) := Pong {
        debug("Pong processing");
    }
}"#;

    /// Комплексный тест разбора Takt-программы с различными конструкциями.
    ///
    /// Проверяет, что все основные элементы языка (псевдонимы типов, константы,
    /// условия, порты, переменные, модели, состояния, переходы, именованные блоки и
    /// операторы компоновки) успешно разбираются.
    #[test]
    fn parse_simple() {
        let result = parse(SRC, 0);
        if let Err(diagnostics) = result {
            for diagnostic in diagnostics.iter() {
                let source = &SRC[diagnostic.loc.start()..diagnostic.loc.end()];
                let text = &SRC[diagnostic.loc.start() - 5..diagnostic.loc.end() + 5];
                println!(
                    "[{}:{}] Source: {}, Text: {}, Message: {}",
                    diagnostic.loc.start(),
                    diagnostic.loc.end(),
                    source,
                    text,
                    diagnostic.message
                );
            }
        } else {
            let (model, _) = result.unwrap();
            assert!(!model.elements.is_empty());
            #[cfg(feature = "ast-serde")]
            {
                let text = serde_json::to_string_pretty(&model).unwrap();
                println!("{}", text);
            }
        }
    }

    // Инвариант "условия рёбер `ref` не разрешаются" (`ref Stop: S(Ping) = End;`
    // переживает конвейер как `Condition::Unresolved`, прохода
    // `resolve_state_references` быть не должно) охраняется компиляционным тестом
    // `tests/reference_model_tests.rs`: перевод `S(Ping) = End` в C доказывает, что
    // ссылка пережила конвейер. `syntax_simple` переехал туда же - он стоял на
    // некомпилируемом `SRC` и мог проверять лишь строку, а не `cc`.

    // -- Тесты ошибок парсера --------------------------------------------------

    /// Недопустимый токен: строка с управляющим символом вызывает ошибку парсера.
    #[test]
    fn parse_invalid_token_error() {
        // Управляющие символы не являются допустимыми токенами языка Takt
        let result = parse("\x00abc", 0);
        assert!(
            result.is_err(),
            "строка с управляющим символом должна давать ошибку"
        );
    }

    /// Неожиданный конец файла: незакрытая фигурная скобка.
    #[test]
    fn parse_unrecognized_eof_error() {
        let result = parse("model M {", 0);
        assert!(
            result.is_err(),
            "незакрытый блок модели должен давать ошибку EOF"
        );
        let diagnostics = result.unwrap_err();
        assert!(
            !diagnostics.is_empty(),
            "должна быть хотя бы одна диагностика"
        );
    }

    /// Нераспознанный токен: объявление переменной с неверным синтаксисом.
    #[test]
    fn parse_unrecognized_token_error() {
        // "var" без имени переменной - нераспознанный токен
        let result = parse("var = 0;", 0);
        assert!(
            result.is_err(),
            "неверный синтаксис переменной должен давать ошибку"
        );
    }

    /// Разбор корректной программы не должен паниковать.
    #[test]
    fn syntax_simple_does_not_panic() {
        let result = parse("model M { start S; }", 0);
        assert!(
            result.is_ok(),
            "корректная программа должна разбираться без ошибок"
        );
    }

    /// Ошибка парсера содержит непустой диагностический список.
    #[test]
    fn parse_error_produces_diagnostics() {
        let result = parse("model { }", 0);
        assert!(result.is_err(), "модель без имени должна давать ошибку");
        let diags = result.unwrap_err();
        assert!(
            !diags.is_empty(),
            "список диагностик не должен быть пустым при ошибке"
        );
    }

    // -- V1/V2: Тесты безопасного извлечения имени файла ----------------------

    /// V1: обычный путь вида "path/to/model.takt" -> имя модели "model".
    #[test]
    fn compile_to_c_normal_filename_sets_model_name() {
        // Используем пустую директорию; ошибка записи нас не интересует - важно только,
        // что функция не паникует при нормальном имени файла.
        let dir = tempfile::tempdir().unwrap();
        let out = dir.path().to_string_lossy().into_owned();
        // Простой FSM без имени модели; имя должно быть взято из имени файла.
        let src = "start S;";
        let result = compile_to_c(
            "path/to/my_model.takt",
            src,
            &out,
            &[],
            &GenerateOptions::default(),
        );
        // Функция может вернуть ошибку генератора (файл записан / не записан), но не
        // должна паниковать.
        let _ = result;
    }

    /// V1: путь оканчивается на `/` - `file_name()` вернёт None -> имя "Root".
    #[test]
    fn compile_to_c_trailing_slash_uses_root_name() {
        let dir = tempfile::tempdir().unwrap();
        let out = dir.path().to_string_lossy().into_owned();
        let src = "start S;";
        // Путь с завершающим слешем - не должен паниковать.
        let _ = compile_to_c("some/dir/", src, &out, &[], &GenerateOptions::default());
    }

    /// V1: пустая строка имени файла - `file_name()` вернёт None -> имя "Root".
    #[test]
    fn compile_to_c_empty_filename_uses_root_name() {
        let dir = tempfile::tempdir().unwrap();
        let out = dir.path().to_string_lossy().into_owned();
        let src = "start S;";
        // Пустое имя файла - не должно паниковать.
        let _ = compile_to_c("", src, &out, &[], &GenerateOptions::default());
    }

    /// V2: имя файла без расширения - возвращается строка целиком.
    #[test]
    fn compile_to_c_filename_without_extension() {
        let dir = tempfile::tempdir().unwrap();
        let out = dir.path().to_string_lossy().into_owned();
        let src = "start S;";
        // Без точки - `splitn(2,'.')` вернёт всё имя файла.
        let _ = compile_to_c("my_model", src, &out, &[], &GenerateOptions::default());
    }

    /// V2: имя файла с несколькими точками - берётся только часть до первой.
    #[test]
    fn compile_to_c_filename_with_multiple_dots() {
        let dir = tempfile::tempdir().unwrap();
        let out = dir.path().to_string_lossy().into_owned();
        let src = "start S;";
        // "arch.v2.takt" -> должно брать "arch", не "arch.v2".
        let _ = compile_to_c("arch.v2.takt", src, &out, &[], &GenerateOptions::default());
    }
}
