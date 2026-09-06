//! Семантический узел перечисления (`enum`) языка Takt.
//!
//! Модуль реализует [`EnumDefinitionNode`] - структуру данных, описывающую именованное
//! перечисление с вариантами и опциональными числовыми значениями.
//!
//! Перечисления создаются в ходе семантического анализа при обработке объявлений `enum`
//! в исходном тексте.

use crate::diagnostics::Location;
use crate::parser::ast;
use crate::semantic::ModelNode;
use crate::semantic::type_node::TypeNode;
use std::cell::RefCell;
use std::rc::Rc;
use std::rc::Weak;

/// Семантический узел перечисления (Ce4).
///
/// Описывает именованное перечисление: набор вариантов с опциональными числовыми
/// значениями. Если значение не задано, предполагается автоинкремент от 0.
///
/// # Пример Takt (концептуально, поддержка на семантическом уровне)
///
/// ```text
/// // enum Color { Red = 0, Green = 1, Blue = 2 }
/// ```
///
/// Фактически перечисления сейчас не имеют синтаксиса в грамматике Takt, поэтому
/// `EnumNode` создаётся программно через API.
#[derive(Default, Debug, Clone)]
pub struct EnumDefinitionNode {
    /// Слабая ссылка на родительскую модель (для разрешения имён при генерации кода).
    pub upper: Option<Weak<RefCell<ModelNode>>>,
    /// Имя перечисления.
    pub name: String,
    /// Варианты перечисления: `(имя_варианта, числовое_значение)`. Если значение не
    /// задано при создании, оно равно индексу варианта.
    pub variants: Vec<(String, i128)>,
    /// Позиция объявления перечисления в исходном тексте.
    pub loc: Location,
}

impl EnumDefinitionNode {
    /// Возвращает имя перечисления.
    #[allow(dead_code)]
    pub(crate) fn name(&self) -> &str {
        &self.name
    }
}

impl PartialEq for EnumDefinitionNode {
    fn eq(&self, other: &Self) -> bool {
        // loc игнорируется: не является частью семантической идентичности перечисления
        self.name == other.name && self.variants == other.variants
    }
}

impl Eq for EnumDefinitionNode {}

impl EnumDefinitionNode {
    /// Создаёт новый `EnumNode` с именем и именованными вариантами.
    ///
    /// Если значение варианта задано явно через `(имя, Some(значение))`, используется
    /// это значение. Иначе вариант получает порядковый индекс.
    ///
    /// # Пример
    ///
    /// ```
    /// use takt_lang::semantic::EnumDefinitionNode;
    ///
    /// let e = EnumDefinitionNode::new("Color", &[("Red", None), ("Green", None), ("Blue", Some(5))]);
    /// assert_eq!(e.variants[0], ("Red".to_string(), 0));
    /// assert_eq!(e.variants[1], ("Green".to_string(), 1));
    /// assert_eq!(e.variants[2], ("Blue".to_string(), 5));
    /// ```
    pub fn new(name: &str, variants: &[(&str, Option<i128>)]) -> Self {
        let resolved: Vec<(String, i128)> = variants
            .iter()
            .enumerate()
            .map(|(i, (vname, val))| (vname.to_string(), val.unwrap_or(i as i128)))
            .collect();
        EnumDefinitionNode {
            upper: None,
            name: name.to_string(),
            variants: resolved,
            loc: Location::Implicit,
        }
    }

    /// Ищет числовое значение варианта по имени.
    ///
    /// Возвращает `Some(значение)`, если вариант найден.
    ///
    /// # Пример
    ///
    /// ```
    /// use takt_lang::semantic::EnumDefinitionNode;
    ///
    /// let e = EnumDefinitionNode::new("Color", &[("Red", None), ("Green", None)]);
    /// assert_eq!(e.find_variant("Red"), Some(0));
    /// assert_eq!(e.find_variant("Blue"), None);
    /// ```
    pub fn find_variant(&self, variant_name: &str) -> Option<i128> {
        self.variants
            .iter()
            .find(|(name, _)| name == variant_name)
            .map(|(_, val)| *val)
    }

    /// Возвращает `true`, если вариант с данным именем существует.
    pub fn has_variant(&self, variant_name: &str) -> bool {
        self.find_variant(variant_name).is_some()
    }

    /// Факт о диапазоне перечисления - [`enum_facts`] по вариантам узла.
    pub fn facts(&self) -> Option<EnumFacts> {
        enum_facts(&self.variants)
    }
}

/// Факт о перечислении, общий для всех целей генерации: диапазон, знак и точная
/// минимальная ширина.
///
/// Считается один раз в семантическом слое, а не каждым генератором заново: четыре
/// независимых извлечения диапазона уже разошлись (пустое перечисление давало
/// `uint8_t`/`USINT`/`u8`/`SV-004`, а цель `c` **теряла знак молча** -, Tier 1). Факт
/// **не знает ни одной цели**: имена типов (`uint8_t`/`USINT`/`u8`/`logic`) строит сама
/// цель по этому факту.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct EnumFacts {
    /// Наименьшее значение варианта.
    pub min: i128,
    /// Наибольшее значение варианта.
    pub max: i128,
    /// Знаковый ли диапазон (`min < 0`) - **единая** формула на все цели.
    pub signed: bool,
    /// Точная минимальная ширина в битах, вмещающая диапазон.
    ///
    /// - Беззнаковое: число бит для `max` (`max == 0` -> **1**; ширины 0 не бывает).
    /// - Знаковое: наименьшая ширина дополнительного кода `w >= 2`, для которой
    ///   `-2^(w-1) <= min` и `max <= 2^(w-1)-1` (однобитного знакового не бывает).
    ///
    /// Цель `sv` берёт её **напрямую** (аппаратная ширина точна); `c`/`st`/`rust`
    /// округляют до машинной ([`EnumFacts::machine_bits`]).
    pub min_bits: u32,
}

impl EnumFacts {
    /// Машинная ширина: `min_bits`, округлённая вверх до {8, 16, 32, 64}.
    ///
    /// Нужна целям `c`/`st`/`rust` - у них тип из набора машинных ширин. Округление
    /// одно, а не три расходящихся каскада `max <= u8::MAX ...`.
    pub fn machine_bits(&self) -> u32 {
        match self.min_bits {
            0..=8 => 8,
            9..=16 => 16,
            17..=32 => 32,
            _ => 64,
        }
    }
}

/// Умолчание переменной перечислимого типа - **первый по тексту** вариант.
pub fn enum_default(variants: &[(String, i128)]) -> Option<(String, i128)> {
    variants.first().cloned()
}

/// Считает [`EnumFacts`] по вариантам перечисления.
///
/// `None` - перечисление **без вариантов**: диапазона нет, и трактовку выбирает цель (-
/// поведение сохраняется сегодняшним: `c`/`st`/`rust` -> 8 бит без знака, `sv` ->
/// `SV-004`). Факт возвращает "пусто" честно, а не подставляет молчаливое умолчание.
pub fn enum_facts(variants: &[(String, i128)]) -> Option<EnumFacts> {
    if variants.is_empty() {
        return None;
    }
    let min = variants.iter().map(|(_, v)| *v).min().unwrap_or(0);
    let max = variants.iter().map(|(_, v)| *v).max().unwrap_or(0);
    let signed = min < 0;
    let min_bits = if signed {
        // Наименьшая ширина доп. кода w >= 2, вмещающая весь диапазон.
        (2..=64u32)
            .find(|&w| {
                let lo = -(1i128 << (w - 1));
                let hi = (1i128 << (w - 1)) - 1;
                min >= lo && max <= hi
            })
            .unwrap_or(64)
    } else if max == 0 {
        1
    } else {
        128 - (max as u128).leading_zeros()
    };
    Some(EnumFacts {
        min,
        max,
        signed,
        min_bits,
    })
}

/// Построение узла перечисления из АСД.
///
/// Живёт рядом с самим узлом, а не в `tree.rs`: тот давно сверх лимита размера
/// (`docs/CODE.md`), и правило велит выносить новое, а не дописывать туда. Строит узел
/// перечисления и регистрирует его в модели.
///
/// # Регистрация идёт в две таблицы (Ce4)
///
/// 1. `enums` - для поиска через `search_enum` / `search_enum_variant`;
/// 2. `types` - для разрешения аннотации `var x: Color := 0;`: парсер создаёт
///    `Type::Alias("Color")`, а `construct_type` ищет псевдоним именно там.
///
/// Ограничение прежнее: перечисление должно быть объявлено **до** переменных,
/// использующих его как тип (как и псевдонимы `type`); иначе тип переменной станет
/// `Unsupported`.
pub(crate) fn build_enum(model_node: &Rc<RefCell<ModelNode>>, e: &ast::EnumDefine) {
    let enum_name = e
        .name
        .as_ref()
        .map(|id| id.name.clone())
        .unwrap_or_default();

    // Вариант без явного значения продолжает нумерацию от предыдущего (первый - 0).
    let mut next_val: i128 = 0;
    let mut variant_pairs = Vec::new();
    for variant in &e.variants {
        let val = variant.value.unwrap_or(next_val);
        next_val = val + 1;
        variant_pairs.push((variant.name.name.clone(), val));
    }

    let enum_loc = e.name.as_ref().map(|id| id.loc).unwrap_or(e.loc);
    let mut enum_node = EnumDefinitionNode::new(
        &enum_name,
        &variant_pairs
            .iter()
            .map(|(n, v)| (n.as_str(), Some(*v)))
            .collect::<Vec<_>>(),
    );
    enum_node.loc = enum_loc;
    // Владелец. Поле существовало с самого начала и док-строкой обещало "разрешение
    // имён при генерации кода", но не заполнялось: потребителю оставалось строить имя
    // из "модели, которую печатаем", а не из места объявления - тот же класс
    // расхождения, что закрывала 0193 для констант.
    //
    // Взять владельца иначе нельзя: `search_enum` идёт по цепочке `upper` модели и
    // отдаёт узел, поэтому унаследованное перечисление неотличимо от собственного.
    enum_node.upper = Some(Rc::downgrade(model_node));

    model_node
        .borrow_mut()
        .enums
        .insert(enum_name.clone(), enum_node);
    if !enum_name.is_empty() {
        model_node
            .borrow_mut()
            .types
            .insert(enum_name.clone(), TypeNode::Enum(enum_name.clone()));
        model_node
            .borrow_mut()
            .type_locs
            .insert(enum_name.clone(), enum_loc);
    }
}

#[cfg(test)]
mod facts_tests {
    use super::*;

    fn vs(values: &[i128]) -> Vec<(String, i128)> {
        values
            .iter()
            .enumerate()
            .map(|(i, v)| (format!("V{i}"), *v))
            .collect()
    }

    #[test]
    fn empty_enum_has_no_facts() {
        assert_eq!(enum_facts(&[]), None);
    }

    #[test]
    fn negative_range_is_signed_min_four_bits() {
        // {-5, 0, 5} влезает в диапазон −8...7 доп. кода -> 4 бита, машинно 8.
        let f = enum_facts(&vs(&[-5, 0, 5])).unwrap();
        assert!(f.signed);
        assert_eq!(f.min_bits, 4);
        assert_eq!(f.machine_bits(), 8);
    }

    #[test]
    fn single_negative_one_needs_two_bits_not_one() {
        // Однобитного знакового не бывает: −1 -> 2 бита (тест тонкости sv, T12).
        let f = enum_facts(&vs(&[-1])).unwrap();
        assert!(f.signed);
        assert_eq!(f.min_bits, 2);
    }

    #[test]
    fn value_670_needs_ten_bits_machine_sixteen() {
        let f = enum_facts(&vs(&[670, 671])).unwrap();
        assert!(!f.signed);
        assert_eq!(f.min_bits, 10);
        assert_eq!(f.machine_bits(), 16);
    }

    #[test]
    fn zero_only_is_one_bit_unsigned() {
        // Ширины 0 не бывает: {0} -> 1 бит без знака (тест тонкости sv).
        let f = enum_facts(&vs(&[0])).unwrap();
        assert!(!f.signed);
        assert_eq!(f.min_bits, 1);
        assert_eq!(f.machine_bits(), 8);
    }

    #[test]
    fn unsigned_machine_boundaries() {
        assert_eq!(enum_facts(&vs(&[255])).unwrap().machine_bits(), 8);
        assert_eq!(enum_facts(&vs(&[256])).unwrap().machine_bits(), 16);
        assert_eq!(enum_facts(&vs(&[65536])).unwrap().machine_bits(), 32);
    }
}
