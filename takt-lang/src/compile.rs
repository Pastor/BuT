//! Компиляция по имени цели: одна точка выбора для всех потребителей.

use crate::address_map::{AddressEnv, AddressMapEntry};
use crate::diagnostics::Diagnostic;
use crate::generator::{GenerateOptions, Language, Output};
use crate::pipeline::{self, Compilation};
use crate::semantic::condition::port_split::PortSplit;
use crate::semantic::lower_float::apply_float_lowering;
use std::path::Path;

/// Цель компиляции - то, что CLI принимает ключом `-t`.
///
/// Не то же, что [`Language`]: `c` и `c-hal` печатает один генератор, но готовятся они
/// по-разному (вторая потребляет адреса). Имя цели - контракт с пользователем,
/// `Language` - с генератором.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Target {
    /// `c` - прошивка МК, порты через пару указателей на функции.
    C,
    /// `c-hal` - то же плюс таблица адресов и умолчательная реализация HAL.
    CHal,
    /// `st` - Structured Text (IEC 61131-3).
    St,
    /// `st-at` - ST с размещением портов по карте адресов (`AT %...`).
    StAt,
    /// `rust` - прошивка `no_std`.
    Rust,
    /// `sv` - синтезируемый SystemVerilog.
    Sv,
    /// `sv-mmio` - SystemVerilog с регистровым файлом.
    SvMmio,
}

impl Target {
    /// Все цели в порядке, в каком их перечисляет справка CLI.
    pub const ALL: [Target; 7] = [
        Target::C,
        Target::CHal,
        Target::St,
        Target::StAt,
        Target::Rust,
        Target::Sv,
        Target::SvMmio,
    ];

    /// Разбирает имя цели; `None` - имени нет в списке.
    ///
    /// Ответ на неизвестную цель - дело вызывающего: CLI печатает подсказку со списком,
    /// модуль в браузере отвечает своим сообщением. Диагностики с кодом здесь нет
    /// намеренно - это ошибка **вызова**, а не программы.
    pub fn parse(name: &str) -> Option<Self> {
        Target::ALL.into_iter().find(|t| t.name() == name)
    }

    /// Имя цели - то, что пишут в `-t`.
    pub fn name(self) -> &'static str {
        match self {
            Target::C => "c",
            Target::CHal => "c-hal",
            Target::St => "st",
            Target::StAt => "st-at",
            Target::Rust => "rust",
            Target::Sv => "sv",
            Target::SvMmio => "sv-mmio",
        }
    }

    /// Язык генерации, которым цель печатает вывод.
    pub fn language(self) -> Language {
        match self {
            Target::C | Target::CHal => Language::C,
            Target::St | Target::StAt => Language::ST,
            Target::Rust => Language::Rust,
            Target::Sv => Language::SV,
            Target::SvMmio => Language::SvMmio,
        }
    }

    /// Потребляет ли цель адреса портов.
    pub fn consumes_addresses(self) -> bool {
        matches!(self, Target::CHal | Target::StAt | Target::SvMmio)
    }
}

/// Вход компиляции: исходник и всё, что нужно любой из целей.
///
/// Структура, а не восемь аргументов: список параметров у целей разный только тем, что
/// часть из них не смотрит на адреса, - а вызывающему удобнее собрать вход один раз и
/// выбрать цель отдельно.
#[derive(Debug, Clone, Copy)]
pub struct CompileInput<'a> {
    /// Имя входного файла: даёт имя корневой модели и путь диагностике.
    pub filename: &'a str,
    /// Исходный текст модели.
    pub source: &'a str,
    /// Каталоги поиска `import`.
    pub search_paths: &'a [String],
    /// Записи внешней карты адресов (`--address-map`); для целей без адресов пуст.
    pub external: &'a [AddressMapEntry],
    /// Символы платформы для выражений адреса (`--define`).
    ///
    /// `None` - среды нет: так вход строит цель, которая адреса не потребляет. Ссылка,
    /// а не значение: `AddressEnv` копит обращения в `RefCell`, и общей "пустой"
    /// константы у него быть не может.
    pub env: Option<&'a AddressEnv>,
    /// Опции генерации.
    pub options: &'a GenerateOptions,
}

impl<'a> CompileInput<'a> {
    /// Вход без адресной части - для целей, которые адреса не потребляют.
    pub fn new(
        filename: &'a str,
        source: &'a str,
        search_paths: &'a [String],
        options: &'a GenerateOptions,
    ) -> Self {
        Self {
            filename,
            source,
            search_paths,
            external: &[],
            env: None,
            options,
        }
    }
}

/// Компилирует цель, отдавая файлы **в память**.
///
/// Вход потребителя без файловой системы: модуля WebAssembly, теста, сверки. Запись на
/// диск - [`compile_files`].
pub fn compile_texts(target: Target, input: &CompileInput<'_>) -> Result<Output, Diagnostic> {
    match target {
        // Массив-порт цель `c` не разворачивает: элемент адресуется индексом в
        // обращении HAL, и переменный индекс выразим.
        Target::C => plain(input, PortSplit::StructsOnly, false, true, Language::C),
        Target::CHal => with_addresses(input, PortSplit::StructsOnly, Language::C),
        Target::St => plain(input, PortSplit::ArraysOnly, true, true, Language::ST),
        Target::StAt => with_addresses(input, PortSplit::All, Language::ST),
        Target::Rust => plain(input, PortSplit::All, true, true, Language::Rust),
        Target::Sv => sv(input),
        Target::SvMmio => crate::compile_sv_mmio::compile_sv_mmio_texts(input),
    }
}

/// Компилирует цель и кладёт её файлы в каталог `output_path`.
///
/// Возвращает предупреждения цели - печатает их вызывающий.
pub fn compile_files(
    target: Target,
    input: &CompileInput<'_>,
    output_path: &str,
) -> Result<Vec<Diagnostic>, Diagnostic> {
    let output = compile_texts(target, input)?;
    crate::generator::write_files(target.language(), &output.files, output_path)?;
    Ok(output.warnings)
}

// -- Подготовка по целям ---------------------------------------------------

/// Разбор, построение дерева и имя корневой модели.
///
/// Корневая (файловая) модель всегда анонимна - имя берётся из имени файла; запасное
/// имя `Root` нужно там, где имени файла нет (путь оканчивается на `/`, не-UTF-8 путь).
pub(crate) fn named_unit(input: &CompileInput<'_>) -> Result<Compilation, Diagnostic> {
    let unit = pipeline::parse_and_construct(
        input.filename,
        input.source,
        input.search_paths,
        input.options,
    )?;
    if unit.model.borrow().name.is_none() {
        let stem = Path::new(input.filename)
            .file_name()
            .and_then(|s| s.to_str())
            .map(|s| s.split('.').next().unwrap_or(s).to_owned())
            .unwrap_or_else(|| "Root".to_owned());
        unit.model.borrow_mut().name = Some(stem);
    }
    Ok(unit)
}

/// Цель без адресов: понижение под цель, представление `float`, печать.
///
/// `fold_state_observe` - разворачивать ли чтение состояния соседа: `c` читает его
/// напрямую, `st`/`rust` получают переменную и запись в `enter`.
fn plain(
    input: &CompileInput<'_>,
    split: PortSplit,
    fold_state_observe: bool,
    embedded_float: bool,
    language: Language,
) -> Result<Output, Diagnostic> {
    let unit = named_unit(input)?;
    unit.lower_for_target(split, fold_state_observe)?;
    apply_float_lowering(&unit.model, input.options, embedded_float)?;
    unit.emit_texts(language, input.options)
}

/// Цель с адресами (`c-hal`, `st-at`): к обычному пути добавляется разрешение адресов и
/// режим HAL.
///
/// Предупреждения адресного слоя присоединяются к предупреждениям цели: у одного вызова
/// обязана быть одна судьба, иначе часть диагностики глушится `--quiet`, а часть - нет.
fn with_addresses(
    input: &CompileInput<'_>,
    split: PortSplit,
    language: Language,
) -> Result<Output, Diagnostic> {
    let unit = named_unit(input)?;
    unit.lower_for_target(split, !matches!(language, Language::C))?;

    let default_env = AddressEnv::default();
    let resolution = crate::address_map::resolve_addresses(
        std::rc::Rc::clone(&unit.model),
        input.external,
        input.env.unwrap_or(&default_env),
    );
    // Путь файла ставит тип, а не вызов: иначе `SE-052` придёт без координаты.
    if let Some(err) = pipeline::first_error(&resolution.diagnostics) {
        return Err(unit.stamp(err));
    }

    let mut hal_options = input.options.clone();
    hal_options.hal = true;
    hal_options.address_map = resolution.map;

    // embedded-путь `float -> q(m, n)` при `--float-embedded`.
    apply_float_lowering(&unit.model, input.options, true)?;

    let mut output = unit.emit_texts(language, &hal_options)?;
    let mut warnings = resolution.diagnostics;
    warnings.append(&mut output.warnings);
    output.warnings = warnings;
    Ok(output)
}

/// Цель `sv`: `float` понижается до понижения портов и всегда, без `--float-embedded`
/// (нативного float в синтезируемом RTL нет).
fn sv(input: &CompileInput<'_>) -> Result<Output, Diagnostic> {
    let unit = named_unit(input)?;
    apply_float_lowering(&unit.model, input.options, false)?;
    unit.lower_for_target(PortSplit::ArraysOnly, false)?; // 0417: порт-массив
    unit.emit_texts(Language::SV, input.options)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Имена целей и их разбор - взаимно обратны.
    ///
    /// Тест против расхождения списка: имя, которое `parse` не принимает, делает цель
    /// недоступной CLI, а имя без цели - обещанием пустоты.
    #[test]
    fn target_names_round_trip() {
        for target in Target::ALL {
            assert_eq!(
                Target::parse(target.name()),
                Some(target),
                "цель {:?} не разбирается по своему имени",
                target
            );
        }
        assert_eq!(Target::ALL.len(), 7, "целей семь");
        assert!(
            Target::parse("plantuml").is_none(),
            "цели диаграмм среди целей нет"
        );
        assert!(Target::parse("verilog").is_none(), "чужое имя не цель");
    }

    /// Адреса потребляют ровно три цели.
    #[test]
    fn three_targets_consume_addresses() {
        let consuming: Vec<&str> = Target::ALL
            .into_iter()
            .filter(|t| t.consumes_addresses())
            .map(Target::name)
            .collect();
        assert_eq!(consuming, vec!["c-hal", "st-at", "sv-mmio"]);
    }
}
