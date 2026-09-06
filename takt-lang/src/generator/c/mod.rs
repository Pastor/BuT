//! Генератор C-кода из семантического дерева Takt.
//!
//! Модуль реализует трансляцию семантического дерева [`ModelNode`] в пару файлов:
//! заголовочный (`.h`) и исходный (`.c`).
//!
//! ## Алгоритм трансляции
//!
//! 1. **Заголовок** (`generate_header`): генерирует `#ifndef`-защиту, структуру
//!    модели (рекурсивно для вложенных моделей) и прототипы функций `_init`, `_tick`, `_reset`.
//! 2. **Источник** (`generate_source`): генерирует `#include` и раскрывает
//!    константы, порты и перечисления через `#define`.
//! 3. **Вспомогательные функции**: `unroll_model`, `unroll_variable`, `unroll_cond`,
//!    `unroll_expression` рекурсивно преобразуют семантические узлы в C-выражения.
//!
//! ## Именование
//!
//! - Структура модели: `<PascalCase>` (например, `MainRobot`).
//! - Поля структуры: snake_case (например, `main->robot.idle`).
//! - Порты: варианты `BitPort`, `RationalPort`, `NumericPort` (например, `BitPort_MAIN_SENSORS_1`).
//! - Константы: `CONST_<UPPER_SNAKE>`.
//! - Условия: `COND_<UPPER_SNAKE>`.
//! - Перечисления: `ENUM_<UPPER_SNAKE>_<VARIANT>`.
//!
//! ## Интерфейс портов
//!
//! Сгенерированный код обращается к портам через указатели на функции `write_bit`,
//! `read_bit`, `write_float`, `read_float`, которые должны быть предоставлены
//! платформенным слоем.

#![allow(clippy::needless_borrow)]
#![allow(clippy::explicit_auto_deref)]

mod c_anon;
mod c_bits;
mod c_blocks;
mod c_chain;
mod c_decl;
// Значение перечислимого типа печатается именем константы.
mod c_compose;
mod c_enum;
mod c_every;
mod c_expr;
mod c_hal;
mod c_header;
mod c_literal;
mod c_map;
mod c_model;
mod c_model_init;
mod c_names;
mod c_needs;
mod c_params;
mod c_port_call;
mod c_port_enums;
mod c_ports;
mod c_source;
mod c_table;
/// Механизм времени цели `c`.
mod c_time;
mod c_unresolved;
mod c_unsupported;
mod c_zero_init;

use crate::diagnostics::{Diagnostic, Location};
use crate::generator::Generator as AsGenerator;
use crate::generator::c::c_header::generate_header;
use crate::generator::c::c_map::CMap;
use crate::generator::c::c_source::generate_source;
use crate::generator::{FloatWidth, GenerateOptions, GeneratedFile, Output};
use crate::semantic::ModelNode;
use crate::semantic::PortDirection;
use crate::semantic::bit_vector::{self, BitVectorLayout};
use crate::semantic::enum_facts;
use crate::semantic::minimap::{Element, StateExtend};
use crate::semantic::naming::normalize_lowercase_snakecase;
use crate::semantic::type_node::TypeNode;
use std::collections::{BTreeMap, BTreeSet};

pub(super) const FUNCTION_PORT_WRITE_BIT: &str = "write_bit";
pub(super) const FUNCTION_PORT_READ_BIT: &str = "read_bit";
pub(super) const FUNCTION_PORT_WRITE_FLOAT: &str = "write_float";
pub(super) const FUNCTION_PORT_READ_FLOAT: &str = "read_float";
pub(super) const FUNCTION_PORT_WRITE_NUMERIC: &str = "write_numeric";
pub(super) const FUNCTION_PORT_READ_NUMERIC: &str = "read_numeric";
/// Колбэк источника времени профиля "часы": `uint64_t now_ms(void*)`. Встаёт рядом с
/// портовыми колбэками в структуре модели; умолчание - только у `c-hal`.
pub(super) const FUNCTION_TIME_NOW_MS: &str = "now_ms";

/// Категория типа порта - определяет имя перечисления и набор функций.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum PortClass {
    /// Однобитовый порт (`bit`, `bool`).
    Bit,
    /// Порт с плавающей точкой (`rational`).
    Rational,
    /// Числовой порт (`u8`, `u16`, массив битов и т.
    Numeric,
}

impl PortClass {
    /// Суфисправление C-перечисления для этой категории.
    pub(super) fn enum_name(self) -> &'static str {
        match self {
            PortClass::Bit => "BitPort",
            PortClass::Rational => "RationalPort",
            PortClass::Numeric => "NumericPort",
        }
    }

    /// Полное имя C-перечисления с направлением порта. Например:
    /// `ElevatorMini_In_BitPort`, `ElevatorMini_Out_NumericPort`.
    pub(super) fn qualified_enum_name_with_dir(
        self,
        root_camelcase: &str,
        dir: PortDirection,
    ) -> String {
        let dir_str = match dir {
            PortDirection::In => "In",
            PortDirection::Out => "Out",
            PortDirection::InOut => "InOut",
        };
        format!("{}_{}_{}", root_camelcase, dir_str, self.enum_name())
    }

    /// Определяет категорию по [`TypeNode`].
    ///
    /// **Массив спрашивает категорию у элемента**: порт-массив с контракта 2026-09-04 -
    /// один порт, адресуемый индексом, а не набор портов.
    pub(super) fn from_type(ty: &TypeNode) -> Self {
        match ty {
            TypeNode::Bit | TypeNode::Bool => PortClass::Bit,
            TypeNode::Rational => PortClass::Rational,
            // Бит-вектор `[bit;N]` - порт из N разрядов: категория та же, что у одного
            // разряда, а число разрядов адресуется индексом.
            TypeNode::Array(_, elem) => match crate::semantic::bit_vector::is_bit_vector(ty) {
                Some(_) => PortClass::Bit,
                None => PortClass::from_type(elem),
            },
            _ => PortClass::Numeric,
        }
    }

    /// Ложится ли тип порта на протокол HAL.
    ///
    /// Колбэки HAL принимают **скаляр** (`bool`, `int64_t`, `double`), поэтому
    /// структура и массив (кроме упакованного `[bit;N]`) в него не ложатся.
    pub(super) fn fits_hal(ty: &TypeNode) -> bool {
        match ty {
            TypeNode::Struct(_) => false,
            // Массив ложится на HAL с: обращение несёт индекс, и колбэк по-прежнему
            // принимает скаляр - элемент, а не массив целиком. Не ложится массив, чей
            // Элемент не ложится (массив структур, массив массивов), и широкий
            // бит-вектор: у него носитель - набор слов, а не одно значение.
            TypeNode::Array(_, elem) => match crate::semantic::bit_vector::is_bit_vector(ty) {
                Some(bits) => matches!(
                    crate::semantic::bit_vector::layout(bits),
                    crate::semantic::bit_vector::BitVectorLayout::Scalar { .. }
                ),
                None => PortClass::fits_hal(elem),
            },
            _ => true,
        }
    }
}

/// Генератор C-кода для модели Takt.
///
/// Реализует трейт [`Generator`](crate::generator::Generator): принимает корневой
/// [`ModelNode`] и записывает пару файлов `.h`/`.c` по заданному пути.
pub struct Generator {}

impl AsGenerator for Generator {
    fn generate_texts(
        &self,
        model: &ModelNode,
        options: &GenerateOptions,
    ) -> Result<Output, Diagnostic> {
        // Профиль времени: `clock` модели - контракт, флаг обязан подтвердить. Сверка
        // живёт в общем слое; здесь - единый чекпойнт-энфорсмент: несовпадение ->
        // `SE-069`/`SE-070` из `?`, покрывает все пути кодогенерации (CLI, публичный
        // API, тесты).
        let profile = crate::semantic::duration::resolve_profile(model.clock_hz, options.tick_hz)?;
        // TODO: При генерации следует работать с примитивным слепком модели
        let map = CMap::new(
            &*normalize_lowercase_snakecase(model.name().to_string()),
            model,
            options.guard_enable,
        )?
        .with_float_width(options.float_width)
        .with_time_profile(profile)
        .with_fsm(options.fsm)
        .with_hal(options.hal);
        let header = generate_header(map.get_filename(), &map, options)?;
        let source = generate_source(map.get_filename(), &map)?;
        let filename = map.get_filename();
        Ok(Output {
            files: vec![
                GeneratedFile {
                    name: filename.to_owned() + ".h",
                    text: header,
                },
                GeneratedFile {
                    name: filename.to_owned() + ".c",
                    text: source,
                },
            ],
            // Предупреждения цели (канал заведён, первым по нему поехал `CC-024` - ):
            // накопитель живёт в карте, потому что печатники получают её по `&self` и
            // доходят до каждого оператора.
            //
            // Забирать надо после печати обоих файлов: заголовок и исходник печатаются
            // разными проходами, и вызов, выброшенный во втором, иначе потерялся бы.
            warnings: map.take_warnings(),
        })
    }

    fn write_failure(&self, error: &std::io::Error) -> Diagnostic {
        Diagnostic::warning(Location::Codegen, format!("{error}")).with_code("CC-010")
    }
}

/// Причина, по которой тип Takt не выразим в C.
///
/// Существует, чтобы вызывающий мог дать **точную** диагностику вместо тихого `None`:
/// без причины `[bit;128]` и `BuiltinModel` неразличимы, и оба вырождались либо в
/// молчание, либо в чужую по смыслу ошибку ("переменная не найдена" - при том что
/// переменная найдена, невыразим её тип).
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum CTypeError {
    /// Тип (или тип элемента массива) представления в C не имеет вовсе.
    ///
    /// Код `CC-020` в реестре диагностик помечен снятым - возвращать его под другим
    /// смыслом нельзя.
    Unrepresentable,
}

impl CTypeError {
    /// Оборачивает причину в диагностику. `what` - что именно объявлялось (`переменная
    /// 'x'`, `параметр 'p'`), чтобы сообщение указывало на место.
    fn into_diagnostic(self, what: &str) -> Diagnostic {
        match self {
            Self::Unrepresentable => Diagnostic::error(
                Location::Codegen,
                format!("{}: тип не представим в C", what),
            )
            .with_code("CC-015"),
        }
    }
}

/// Отображает тип Takt в тип C, сохраняя причину отказа.
///
/// Ядро отображения; [`get_c_type`] - его обёртка, теряющая причину.
pub(super) fn map_c_type(
    typ: &TypeNode,
    model: &ModelNode,
    float_width: FloatWidth,
) -> Result<String, CTypeError> {
    match typ {
        // Д2: было `int` - 32-битный знаковый для однобитной семантики. `uint8_t` -
        // наименьший адресуемый беззнаковый тип C. Тип `duration`: целое без знака в
        // **миллисекундах** - та же единица, что у приведения `as` и у профиля "часы",
        // поэтому граница "длительность ↔ число" не порождает арифметики и не может
        // разойтись с эталоном. Ширина - `duration::VALUE_BITS`.
        TypeNode::Duration => Ok(format!("uint{}_t", crate::semantic::duration::VALUE_BITS)),
        TypeNode::Bit => Ok("uint8_t".to_string()),
        TypeNode::Bool => Ok("bool".to_string()),
        // Д3: было `float` (f32) безусловно, тогда как симулятор считает в f64
        // (`eval::Value::Real`) - расхождение точности между эталоном и моделью.
        // Умолчание `--float-width=64` подтверждено; `W32` - осознанный выбор платформы
        // без f64.
        TypeNode::Rational => Ok(match float_width {
            FloatWidth::W64 => "double".to_string(),
            FloatWidth::W32 => "float".to_string(),
        }),
        // Д1: было `uint{size}_t`, где `size` - Число элементов, а не разрядность:
        // `[u8;4]` давало невалидный `uint4_t`. Тип массива без имени переменной в C не
        // выражается (`elem name[N]` - объявитель, а не тип), поэтому здесь остаётся
        // только скалярный бит-вектор (N <= 64); настоящий массив и бит-вектор из слов
        // (N > 64) печатает `map_typed_variable`.
        TypeNode::Array(..) => match bit_vector::is_bit_vector(typ) {
            Some(n) => bit_vector_type(n),
            None => Err(CTypeError::Unrepresentable),
        },
        TypeNode::Unit => Ok("void".to_string()),
        TypeNode::BuiltinString => Ok("char *".to_string()),
        TypeNode::Struct(struct_name) => Ok(struct_name.to_string()),
        TypeNode::Enum(enum_name) => {
            let enum_node = model
                .search_enum(enum_name)
                .ok_or(CTypeError::Unrepresentable)?;
            // (Tier 1): прежде тип брался только по `max` и был всегда беззнаковым -
            // отрицательный вариант молча становился `uint8_t` (`-5` -> `251`, переход
            // `== -5` тождественно ложен, автомат стоял). Теперь знак и ширина берутся
            // из общего факта: цель лишь отображает факт в имя типа C.
            match enum_facts(&enum_node.variants) {
                Some(f) => {
                    let bits = f.machine_bits();
                    if f.signed {
                        Ok(format!("int{}_t", bits))
                    } else {
                        Ok(format!("uint{}_t", bits))
                    }
                }
                // Пустое перечисление - поведение сохраняется сегодняшним (`uint8_t`);
                // унификация с прочими целями - вопрос семантики языка, вынесен
                // кандидатом.
                None => Ok("uint8_t".to_string()),
            }
        }
        TypeNode::Integer { bits, signed } => {
            if *signed {
                Ok(format!("int{}_t", bits))
            } else {
                Ok(format!("uint{}_t", bits))
            }
        }
        // Fixed-point q(m, n): знаковое целое, вмещающее W = m + n
        // бит (`int{8,16,32,64}_t`). Масштабирование при `*`/`/` и ловушка C11
        // 6.5.7p5 (`>>` знакового отрицательного) -.
        TypeNode::Fixed { m, n, .. } => Ok(format!(
            "int{}_t",
            crate::semantic::type_node::type_fixed::fixed_storage_bits(m + n)
        )),
        TypeNode::BuiltinModel
        | TypeNode::BuiltinState
        | TypeNode::BuiltinNumeric
        | TypeNode::Unsupported
        | TypeNode::Inference
        | TypeNode::Address(_, _) => Err(CTypeError::Unrepresentable),
    }
}

/// Отображает тип Takt в тип C, теряя причину отказа.
///
/// Обёртка над [`map_c_type`] для мест, где причина не нужна (`port_ctype` - ширина
/// доступа к MMIO). **Для объявлений использовать [`c_type_or_diagnostic`]**: там
/// потеря причины оборачивается либо молчанием, либо ошибкой не по адресу.
pub fn get_c_type(typ: &TypeNode, model: &ModelNode, float_width: FloatWidth) -> Option<String> {
    map_c_type(typ, model, float_width).ok()
}

/// Тип C для объявления: при отказе - диагностика `CC-014`/`CC-015`.
///
/// `what` описывает объявляемое (`переменная 'x'`) - попадает в сообщение.
pub(super) fn c_type_or_diagnostic(
    typ: &TypeNode,
    model: &ModelNode,
    float_width: FloatWidth,
    what: &str,
) -> Result<String, Diagnostic> {
    map_c_type(typ, model, float_width).map_err(|e| e.into_diagnostic(what))
}

/// Объявление переменной (тип + имя): при отказе - диагностика `CC-014`/`CC-015`.
pub(super) fn typed_variable_or_diagnostic(
    typ: &TypeNode,
    name: &str,
    model: &ModelNode,
    float_width: FloatWidth,
    what: &str,
) -> Result<String, Diagnostic> {
    map_typed_variable(typ, name, model, float_width).map_err(|e| e.into_diagnostic(what))
}

/// Отображает бит-вектор `[bit; N]` в **скалярный** целый тип C.
///
/// `[bit;N]` - упакованный N-битный вектор (единый слой `semantic::bit_vector`). При **N
/// <= 64** - один скаляр `uint{round_up(N)}_t` (`[bit;8]`->`uint8_t`,
/// `[bit;12]`->`uint16_t`; так в Takt записывают `u8`/`u16`/...). Это тип **без
/// имени**, поэтому здесь возвращается только скалярный случай; **N > 64** - это массив
/// слов `uint64_t[⌈N/64⌉]`, а тип массива в C неотделим от имени, поэтому он даёт
/// [`CTypeError::Unrepresentable`] - печатает его [`map_typed_variable`].
fn bit_vector_type(size: u16) -> Result<String, CTypeError> {
    match bit_vector::layout(size) {
        BitVectorLayout::Scalar { width } => Ok(format!("uint{}_t", width)),
        BitVectorLayout::Words { .. } => Err(CTypeError::Unrepresentable),
    }
}

/// Печатает объявление переменной: тип вместе с именем.
///
/// Отдельная функция от [`get_c_type`] не по стилю, а по устройству C: тип массива
/// **неотделим** от имени (`uint8_t data[4]`, а не `uint8_t[4] data`), поэтому
/// объявление массива строится только здесь.
pub(super) fn map_typed_variable(
    typ: &TypeNode,
    name: &str,
    model: &ModelNode,
    float_width: FloatWidth,
) -> Result<String, CTypeError> {
    match typ {
        TypeNode::Array(size, elem) => {
            // Бит-вектор: N <= 64 - скаляр `uint{W}_t name`; N > 64 - массив слов
            // `uint64_t name[⌈N/64⌉]`.
            if let Some(n) = bit_vector::is_bit_vector(typ) {
                return Ok(match bit_vector::layout(n) {
                    BitVectorLayout::Scalar { width } => format!("uint{}_t {}", width, name),
                    BitVectorLayout::Words { count } => {
                        format!("uint{}_t {}[{}]", bit_vector::WORD_BITS, name, count)
                    }
                });
            }
            // Д1: настоящий массив. Теперь она общая для всех.
            //
            // Вложенный массив - Несколько размерностей: `[[u8; 2]; 2]` -> `uint8_t
            // grid[2][2]`. Спуск останавливается на бит-векторе: `[bit;N]` -
            // упакованное значение, а не размерность.
            let mut dims = vec![*size];
            let mut current: &TypeNode = elem;
            while let TypeNode::Array(inner_size, inner_elem) = current {
                if bit_vector::is_bit_vector(current).is_some() {
                    break;
                }
                dims.push(*inner_size);
                current = inner_elem;
            }
            let elem_type = map_c_type(current, model, float_width)?;
            let suffix: String = dims.iter().map(|d| format!("[{}]", d)).collect();
            Ok(format!("{} {}{}", elem_type, name, suffix))
        }
        _t => map_c_type(typ, model, float_width).map(|c_type| format!("{} {}", c_type, name)),
    }
}

/// Собирает имена моделей-зависимостей из элемента StateExtend.
pub fn collect_extend_model_deps(extend: &StateExtend, deps: &mut Vec<String>) {
    match extend {
        StateExtend::None => {}
        StateExtend::Model(name, _) => deps.push(name.unique().to_string()),
        StateExtend::Concatenation(items) | StateExtend::Parallel(items) => {
            for item in items {
                collect_extend_model_deps(item, deps);
            }
        }
    }
}

/// Рекурсивный DFS для топологической сортировки моделей.
pub fn topo_dfs(
    key: &str,
    by_name: &BTreeMap<String, Element>,
    deps_map: &BTreeMap<String, Vec<String>>,
    visited: &mut BTreeSet<String>,
    result: &mut Vec<Element>,
) {
    if visited.contains(key) {
        return;
    }
    visited.insert(key.to_string());
    // Сначала рекурсивно обрабатываем зависимости
    if let Some(deps) = deps_map.get(key) {
        for dep in deps.clone() {
            topo_dfs(&dep, by_name, deps_map, visited, result);
        }
    }
    // Затем добавляем текущую модель
    if let Some(elem) = by_name.get(key) {
        result.push(elem.clone());
    }
}

/// Топологически сортирует список моделей так, чтобы зависимости шли первыми.
///
/// Модель A зависит от B, если одно из её состояний расширяет B (`StateExtend::Model`).
/// Алгоритм: обход в глубину (DFS) с постисправлениеным добавлением в результат.
///
/// Частичного порядка достаточно для *корректности* (зависимость печатается раньше
/// зависимого), но **не** для *воспроизводимости*: DFS со случайным порядком стартовых
/// вершин даёт всякий раз корректный, но случайный выход. Поэтому `by_name`/`deps_map` -
/// `BTreeMap`, а `visited` - `BTreeSet`: порядок одноуровневых вершин задан
/// лексикографикой ключей. Образец - `st/mod.rs`, где вход сортируется перед
/// топологическим обходом.
pub fn topological_sort_models(map: &CMap, models: Vec<Element>) -> Vec<Element> {
    // Фаза 1: строим карту unique_name -> Element
    let mut by_name: BTreeMap<String, Element> = BTreeMap::new();
    for elem in models {
        if let Element::Model { name, .. } = &elem {
            by_name.insert(name.unique().to_string(), elem);
        }
    }

    // Фаза 2: строим граф зависимостей (только зависимости из нашего набора моделей)
    let mut deps_map: BTreeMap<String, Vec<String>> = BTreeMap::new();
    let keys: Vec<String> = by_name.keys().cloned().collect();
    for key in &keys {
        if let Some(Element::Model { states, .. }) = by_name.get(key) {
            let mut deps = Vec::new();
            for state_name in states.clone() {
                if let Some(Element::StateExtend { extend, .. }) = map.state_at(state_name) {
                    collect_extend_model_deps(&extend, &mut deps);
                }
            }
            // Отбрасываем зависимости, которых нет в нашем наборе
            deps.retain(|d| by_name.contains_key(d.as_str()));
            deps_map.insert(key.clone(), deps);
        }
    }

    // Фаза 3: топологический обход (DFS)
    let mut visited = BTreeSet::new();
    let mut result = Vec::new();
    for key in &keys {
        topo_dfs(key, &by_name, &deps_map, &mut visited, &mut result);
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic::tree::construct_model;

    /// Пустая модель - для типов, не требующих разрешения имён.
    fn empty_model() -> std::rc::Rc<std::cell::RefCell<ModelNode>> {
        let (ast, _) = crate::parse("start S;", 0).unwrap();
        construct_model(&ast, None, &[]).unwrap()
    }

    /// Объявление при умолчании `--float-width=64`.
    fn typed(ty: &TypeNode, name: &str) -> Option<String> {
        map_typed_variable(ty, name, &empty_model().borrow(), FloatWidth::default()).ok()
    }

    /// Объявление при заданной ширине вещественного типа.
    fn typed_w(ty: &TypeNode, name: &str, float_width: FloatWidth) -> Option<String> {
        map_typed_variable(ty, name, &empty_model().borrow(), float_width).ok()
    }

    /// Причина отказа отображения - для проверки кода диагностики.
    fn typed_err(ty: &TypeNode, name: &str) -> CTypeError {
        map_typed_variable(ty, name, &empty_model().borrow(), FloatWidth::default())
            .expect_err("тип не должен быть представим в C")
    }

    /// **Д1.** `[u8;4]` -> настоящий массив, а не `uint4_t`.
    ///
    /// Было: `size` (число элементов) подставлялось как разрядность, давая
    /// **несуществующий** тип `uint4_t` - порождённый C не компилировался, а
    /// размерность терялась (при том что тело эмитило `data[0] = 7`).
    #[test]
    fn test_array_of_u8_is_real_array_not_uint4_t() {
        let ty = TypeNode::Array(
            4,
            Box::new(TypeNode::Integer {
                bits: 8,
                signed: false,
            }),
        );
        assert_eq!(typed(&ty, "data").as_deref(), Some("uint8_t data[4]"));
    }

    /// **Д1 /.** Бит-вектор `[bit;8]` -> `uint8_t`; промежуточная
    /// ширина округляется вверх (`[bit;12]`->`uint16_t`).
    #[test]
    fn test_bit_vector_stays_integer_type() {
        let ty = TypeNode::Array(8, Box::new(TypeNode::Bit));
        assert_eq!(typed(&ty, "b").as_deref(), Some("uint8_t b"));
        let ty32 = TypeNode::Array(32, Box::new(TypeNode::Bit));
        assert_eq!(typed(&ty32, "w").as_deref(), Some("uint32_t w"));
        // Округление вверх до родной ширины.
        let ty12 = TypeNode::Array(12, Box::new(TypeNode::Bit));
        assert_eq!(typed(&ty12, "d").as_deref(), Some("uint16_t d"));
        let ty3 = TypeNode::Array(3, Box::new(TypeNode::Bit));
        assert_eq!(typed(&ty3, "t").as_deref(), Some("uint8_t t"));
    }

    /// **.** Бит-вектор `N > 64` -> массив слов `uint64_t name[⌈N/64⌉]`
    /// (прежде `[bit;128]` давал `CC-014` - невыразим). Тип-без-имени (для скаляра)
    /// у него нет, но объявление с именем - есть.
    #[test]
    fn test_bit_vector_over_64_is_word_array() {
        let ty128 = TypeNode::Array(128, Box::new(TypeNode::Bit));
        assert_eq!(typed(&ty128, "big").as_deref(), Some("uint64_t big[2]"));
        let ty100 = TypeNode::Array(100, Box::new(TypeNode::Bit));
        assert_eq!(typed(&ty100, "m").as_deref(), Some("uint64_t m[2]"));
        let ty129 = TypeNode::Array(129, Box::new(TypeNode::Bit));
        assert_eq!(typed(&ty129, "x").as_deref(), Some("uint64_t x[3]"));
        // Как тип-без-имени бит-вектор из слов не выразим (это массив).
        assert_eq!(
            get_c_type(&ty128, &empty_model().borrow(), FloatWidth::default()),
            None
        );
    }

    /// **.** Вложенный массив - несколько размерностей.
    ///
    /// Отказ был **закреплён тестом**, тогда как в C форма выразима объявителем и её
    /// переводят эталон, `rust`, `sv` и `st`.
    #[test]
    fn test_nested_array_is_printed_with_two_dimensions() {
        let elem = TypeNode::Array(
            2,
            Box::new(TypeNode::Integer {
                bits: 8,
                signed: false,
            }),
        );
        assert_eq!(
            typed(&TypeNode::Array(2, Box::new(elem)), "grid").as_deref(),
            Some("uint8_t grid[2][2]")
        );
    }

    /// **.** Массив широких бит-векторов остаётся невыразимым.
    ///
    /// `[bit;128]` - массив слов, и его тип неотделим от имени:
    /// как элемент другого массива он представления не имеет. Замер
    /// 2026-08-21: эталон такой вход исполняет, `st` отказывает `ST-011`, а
    /// цель `c` обязана дать `CC-015` - то есть отказ здесь остаётся
    /// **границей**, а не пробелом печати.
    #[test]
    fn test_array_of_wide_bit_vectors_is_unrepresentable() {
        let rows = TypeNode::Array(2, Box::new(TypeNode::Array(128, Box::new(TypeNode::Bit))));
        let err = typed_err(&rows, "rows");
        assert_eq!(err, CTypeError::Unrepresentable);
        assert_eq!(
            err.into_diagnostic("переменная 'rows'").code.as_deref(),
            Some("CC-015")
        );
    }

    /// **.** Спуск по размерностям останавливается на бит-векторе:
    /// `[bit;N<=64]` - упакованный скаляр, а не размерность.
    #[test]
    fn test_array_of_bit_vectors_keeps_one_dimension() {
        let rows = TypeNode::Array(2, Box::new(TypeNode::Array(8, Box::new(TypeNode::Bit))));
        assert_eq!(typed(&rows, "rows").as_deref(), Some("uint8_t rows[2]"));
    }

    /// **.** Массив как параметр функции печатается объявителем.
    ///
    /// Тип массива в C неотделим от имени, поэтому параметр обязан печататься `uint8_t
    /// data[4]`.
    #[test]
    fn test_array_parameter_is_printed_as_declarator_not_panic() {
        let ty = TypeNode::Array(
            4,
            Box::new(TypeNode::Integer {
                bits: 8,
                signed: false,
            }),
        );
        assert_eq!(typed(&ty, "data").as_deref(), Some("uint8_t data[4]"));
    }

    /// **Д1.** `[float;4]` -> `double fr[4]`: правильная форма была, но
    /// только для `Rational`; теперь она общая для всех типов элемента.
    #[test]
    fn test_array_of_float_is_array_of_double() {
        let ty = TypeNode::Array(4, Box::new(TypeNode::Rational));
        assert_eq!(typed(&ty, "fr").as_deref(), Some("double fr[4]"));
    }

    /// **Д2.** `bit` -> `uint8_t`, а не `int` (32-битный знаковый).
    #[test]
    fn test_bit_is_unsigned_byte_not_signed_int() {
        assert_eq!(
            get_c_type(
                &TypeNode::Bit,
                &empty_model().borrow(),
                FloatWidth::default()
            )
            .as_deref(),
            Some("uint8_t")
        );
    }

    /// **Д3.** `float` -> `double`: симулятор считает в f64
    /// (`eval::Value::Real`), и эталон C обязан совпадать по точности.
    #[test]
    fn test_rational_is_double_to_match_simulator_f64() {
        assert_eq!(
            get_c_type(
                &TypeNode::Rational,
                &empty_model().borrow(),
                FloatWidth::default()
            )
            .as_deref(),
            Some("double")
        );
    }

    /// **.** Умолчание - `W64`: без явного выбора эталон C совпадает с
    /// f64 симулятора. Проверяется именно `Default`, а не `W64` дословно -
    /// смена умолчания обязана уронить этот тест.
    #[test]
    fn test_default_float_width_is_w64() {
        assert_eq!(FloatWidth::default(), FloatWidth::W64);
    }

    /// **.** `--float-width=32` -> `float`: платформа без f64 выбирает
    /// точность осознанно, флагом.
    #[test]
    fn test_float_width_32_gives_float() {
        assert_eq!(
            get_c_type(
                &TypeNode::Rational,
                &empty_model().borrow(),
                FloatWidth::W32
            )
            .as_deref(),
            Some("float")
        );
        // Форма массива обязана следовать за шириной элемента.
        let arr = TypeNode::Array(4, Box::new(TypeNode::Rational));
        assert_eq!(
            typed_w(&arr, "fr", FloatWidth::W32).as_deref(),
            Some("float fr[4]")
        );
    }
    use crate::generator::c::c_map::CMap;
    use crate::generator::c::c_source::generate_source;
    use crate::{parse, semantic};

    const SRC: &str = r#"

in sensors_1: u8 at 0x100000000;
in sensors_2: u8 at 0x200000000;
cond AtFloor8 = sensors_1.0 & sensors_1.1;
cond AtFloor9 = sensors_2.0 & sensors_2.1;

enum Direction { North, South, East, West }
enum Priority { Low = 0, Medium = 5, High = 10 }
var heading: Direction := 0;
model Robot {
    var speed: u8 := 0;
    var active: bit := false;

    model Idle {
        start Start {
                enter {
                speed := 0;
                active := false;
                heading := North;
            }
            ref End: active;
        }
        state End;
    }

    start Rest = Idle {
        next Moving;
    }

    state Moving {
        always {
            heading := West;
            speed := 100;
            debug("Moving");
        }
        ref Rest: AtFloor8 & heading = West;
    }
}

start Main = Robot;
    "#;

    #[test]
    fn test_unroll_model() {
        let (model_ast, _) = parse(SRC, 0)
            .map_err(|d| d.into_iter().next().unwrap())
            .unwrap();
        let _model = semantic::tree::construct_model(&model_ast, None, &[]).unwrap();
    }

    // -- V6: Тесты безопасности resolve_model_name -----------------------------

    /// V6: get_upper_name не паникует для модели с явным именем.
    #[test]
    fn v6_get_upper_name_with_named_model_does_not_panic() {
        let (model_ast, _) = parse("model Named { start S; }", 0).unwrap();
        let model_rc = semantic::tree::construct_model(&model_ast, None, &[]).unwrap();
        let model = model_rc.borrow();
        let inner = model.search_model("Named").unwrap();
        let inner = inner.borrow();
        let name = inner.name();
        assert!(!name.is_empty(), "имя не должно быть пустым");
    }

    /// V6: get_model_name_struct не паникует для модели с явным именем.
    #[test]
    fn v6_get_model_name_struct_with_named_model_does_not_panic() {
        let (model_ast, _) = parse("model MyModel { start S; }", 0).unwrap();
        let model_rc = semantic::tree::construct_model(&model_ast, None, &[]).unwrap();
        let model = model_rc.borrow();
        let inner = model.search_model("MyModel").unwrap();
        let inner = inner.borrow();
        let name = inner.name();
        assert!(!name.is_empty(), "структурное имя не должно быть пустым");
    }

    // -- Тесты имён констант и портов -----------------------------------------

    /// Константы и порты с ALL_CAPS-именами не разбиваются посимвольно.
    ///
    /// Регрессия: `normalize_lowercase_snakecase("MATRIX")` ранее давала `m_a_t_r_i_x`,
    /// что приводило к `CONST_..._M_A_T_R_I_X` вместо `CONST_..._MATRIX`.
    #[test]
    fn const_port_names_are_not_char_split() {
        // Константы используются в always, чтобы попасть в UsageSet.
        let src = r#"
const MATRIX: u8 := 0;
const NUMB: u8 := 255;
in SENSOR: u8 at 0x100000;
var v: u8 := 0;
start Main { always { v := MATRIX; v := NUMB; } }
        "#;
        let (model_ast, _) = parse(src, 0).unwrap();
        let model = semantic::tree::construct_model(&model_ast, None, &[]).unwrap();
        model.borrow_mut().name = Some("Main".to_string());
        let model = model.borrow();
        let map = CMap::new(model.name(), &*model, true).unwrap();
        let source = generate_source(map.get_filename(), &map).unwrap();
        assert!(
            source.contains("CONST_MAIN_MATRIX"),
            "ожидалось CONST_MAIN_MATRIX, получено:\n{source}"
        );
        assert!(
            source.contains("CONST_MAIN_NUMB"),
            "ожидалось CONST_MAIN_NUMB, получено:\n{source}"
        );
        // Порт теперь генерируется как enum в заголовочном файле - в.c его нет.
        assert!(
            !source.contains("PORT_MAIN_SENSOR"),
            "PORT_MAIN_SENSOR не должен присутствовать в .c-файле:\n{source}"
        );
        assert!(
            !source.contains("M_A_T_R_I_X"),
            "имя не должно разбиваться посимвольно:\n{source}"
        );
    }

    /// include в.c-файле не содержит лишнего пробела перед закрывающей кавычкой.
    #[test]
    fn include_directive_has_no_trailing_space() {
        let src = r#"start Main { always { } }"#;
        let (model_ast, _) = parse(src, 0).unwrap();
        let model = semantic::tree::construct_model(&model_ast, None, &[]).unwrap();
        let model = model.borrow();
        let map = CMap::new(model.name(), &*model, true).unwrap();
        let source = generate_source(map.get_filename(), &map).unwrap();
        assert!(
            !source.contains(".h\" "),
            "#include не должен содержать пробел после кавычки:\n{source}"
        );
    }

    /// Вызов extern функции в блоке always генерируется в C-коде.
    #[test]
    fn test_extern_fn_call_in_always() {
        let src = r#"
extern fn log_val(v: u8);
model Counter {
    var x: u8 := 0;
    start Running {
        always { x := x + 1; log_val(x); }
    }
}
start Root = Counter;
"#;
        let (model_ast, _) = parse(src, 0).unwrap();
        let model_rc = semantic::tree::construct_model(&model_ast, None, &[]).unwrap();
        model_rc.borrow_mut().name = Some("main".to_string());
        let model = model_rc.borrow();
        let map = CMap::new(model.name(), &*model, true).unwrap();
        let source = generate_source(map.get_filename(), &map).unwrap();
        // Ищем именно вызов, а не декларацию - вызов не содержит "extern"
        let call_present = source
            .lines()
            .filter(|l| !l.contains("extern "))
            .any(|l| l.contains("log_val("));
        assert!(
            call_present,
            "вызов extern функции должен быть в генерированном коде (без 'extern'):\n{source}"
        );
    }

    /// Вызов extern функции после локальной переменной в блоке always генерируется.
    #[test]
    fn test_extern_fn_call_after_local_var_in_always() {
        let src = r#"
extern fn log_val(v: u8);
model Counter {
    var x: u8 := 0;
    start Running {
        always {
            var delta: u8 := 1;
            x := x + delta;
            log_val(x);
        }
    }
}
start Root = Counter;
"#;
        let (model_ast, _) = parse(src, 0).unwrap();
        let model_rc = semantic::tree::construct_model(&model_ast, None, &[]).unwrap();
        model_rc.borrow_mut().name = Some("main".to_string());
        let model = model_rc.borrow();
        let map = CMap::new(model.name(), &*model, true).unwrap();
        let source = generate_source(map.get_filename(), &map).unwrap();
        eprintln!("=== GENERATED ===\n{source}\n=== END ===");
        let call_present = source
            .lines()
            .filter(|l| !l.contains("extern "))
            .any(|l| l.contains("log_val("));
        assert!(
            call_present,
            "вызов extern функции после local var должен быть:\n{source}"
        );
    }

    #[test]
    fn test_guard_formula_codegen() {
        let src = r#"
                        var x: u8 := 0;
            :[Guard] x < 100;
            start Running {
                :[Guard] x >= 0;
                always {
                    x := x + 1;
                    :[Guard] x > 0;
                }
            }
        "#;
        let (model_ast, _) = parse(src, 0).unwrap();
        let model_rc = semantic::tree::construct_model(&model_ast, None, &[]).unwrap();
        model_rc.borrow_mut().name = Some("Main".to_string());
        let model = model_rc.borrow();

        // С включенными Guard-проверками
        let map_enabled = CMap::new(model.name(), &*model, true).unwrap();
        let source_enabled = generate_source(map_enabled.get_filename(), &map_enabled).unwrap();

        assert!(
            source_enabled.contains("assert(model->x < 100);"),
            "Отсутствует проверка формулы модели:\n{}",
            source_enabled
        );
        assert!(
            source_enabled.contains("assert(model->x >= 0);"),
            "Отсутствует проверка формулы состояния:\n{}",
            source_enabled
        );
        assert!(
            source_enabled.contains("assert(model->x > 0);"),
            "Отсутствует проверка встроенной формулы:\n{}",
            source_enabled
        );

        // С выключенными Guard-проверками
        let map_disabled = CMap::new(model.name(), &*model, false).unwrap();
        let source_disabled = generate_source(map_disabled.get_filename(), &map_disabled).unwrap();

        assert!(!source_disabled.contains("assert(model->x < 100);"));
        assert!(!source_disabled.contains("assert(model->x >= 0);"));
        assert!(!source_disabled.contains("assert(model->x > 0);"));
    }
}
