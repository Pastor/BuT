//! Отображение типов Takt ([`TypeNode`]) в типы IEC 61131-3.
//!
//! нормативная таблица - [`analyze/0041-02`], решение - (вопрос 2, Option B с откатом
//! Option C для перечислений).
//!
//! ## Факты, снятые пробой MatIEC `iec2c`
//!
//! Две нормы таблицы исправлены **по факту**, а не по ожиданию:
//!
//! - **Массивы не вкладываются.** `ARRAY [0..2] OF ARRAY [0..1] OF USINT` -
//!   `error: invalid item data type in array specification`. Принятая форма -
//!   многомерная: `ARRAY [0..2, 0..1] OF USINT`. Поэтому
//!   [`get_st_type`] **уплощает** вложенные [`TypeNode::Array`] в список
//!   размерностей, а не рекурсирует в текст типа.
//! - **Перечисления не отображаются напрямую.** `TYPE F : (A := 80); END_TYPE` -
//!   `error: ')' missing at the end of enumerated specification` (явные значения
//!   вариантов - 3-я редакция IEC, MatIEC её не знает). Действует откат Option C:
//!   целочисленный тип + именованные константы (их печатает `st_decl`).
//!
//! [`analyze/0041-02`]: ../../../../../../../../docs/features/0041-st-backend.md#анализ

use crate::diagnostics::lang::keys;
use crate::diagnostics::{Diagnostic, Location};
use crate::msg;
use crate::semantic::ModelNode;
use crate::semantic::enum_facts;
use crate::semantic::naming::normalize_camelcase_name;
use crate::semantic::type_node::TypeNode;

/// Отображает тип Takt в имя типа IEC 61131-3.
///
/// `model` нужен для разрешения именованных типов (`enum`/`struct`): имя ищется в
/// модели и родительских (`search_enum`/`search_struct`).
///
/// # Ошибки
///
/// - `ST-002` - тип не имеет представления в IEC (`Inference`, `Unsupported`,
///   `Address`, `Unit`, `Builtin*`, нестандартная разрядность `Integer`).
/// - `ST-007` - массив нулевого размера (`ARRAY [0..-1]` невыразим).
/// - `ST-008` - `Enum`/`Struct` не разрешается в модели.
///
/// Кода `ST-003` ("неизвестный вариант `TypeNode`") **нет**: разбор исчерпывающий, и
/// новый вариант ловит компилятор - см. комментарий в конце `match`.
pub(crate) fn get_st_type(typ: &TypeNode, model: &ModelNode) -> Result<String, Diagnostic> {
    match typ {
        // В Takt `bit` и `bool` - разные узлы, в IEC оба суть `BOOL`. Различие теряется
        // безвредно: обратного преобразования нет, а семантика одного бита совпадает.
        // Тип `duration` - целое без знака в миллисекундах.
        //
        // `TIME` намеренно не выбран, хотя IEC его имеет: арифметика над `TIME` в MatIEC
        // ограничена (сложение и вычитание есть, сравнение с числом - нет), а язык
        // требует ещё и бесплатного приведения между длительностью и числом. Целое
        // `UDINT` даёт и то и другое, а единица - та же миллисекунда, что у остальных
        // целей, поэтому потактовая сверка сравнивает одинаковые числа. Литерал `TIME`
        // (`time_literal`) остаётся для тех мест, где IEC ждёт именно время (`TON.PT`).
        TypeNode::Duration => integer_type(crate::semantic::duration::VALUE_BITS, false),
        TypeNode::Bit | TypeNode::Bool => Ok("BOOL".to_string()),
        TypeNode::Integer { bits, signed } => integer_type(*bits, *signed),
        // Fixed-point q(m, n): знаковое целое IEC, вмещающее W = m+n бит
        // (SINT/INT/DINT/LINT). Масштабирование при `*`/`/` - (сдвиг в ST через
        // преобразования, `<<` над числами нет).
        TypeNode::Fixed { m, n, .. } => integer_type(
            crate::semantic::type_node::type_fixed::fixed_storage_bits(m + n),
            true,
        ),
        // LREAL - 64-битное вещественное, совпадает с f64 симулятора
        // (`takt-sim/src/eval/`). `REAL` (f32) повторил бы дефект Д3.
        TypeNode::Rational => Ok("LREAL".to_string()),
        // Бит-вектор `[bit;N]` - упакованный скаляр `USINT/UINT/UDINT/ULINT` (round_up,
        // N <= 64) либо массив слов `ARRAY [0..count-1] OF ULINT` (N > 64). Иначе -
        // настоящий массив.
        TypeNode::Array(_, _) => {
            if let Some(nbits) = crate::semantic::bit_vector::is_bit_vector(typ) {
                use crate::semantic::bit_vector::{self, BitVectorLayout};
                return match bit_vector::layout(nbits) {
                    BitVectorLayout::Scalar { width } => integer_type(width as u8, false),
                    BitVectorLayout::Words { count } => {
                        Ok(format!("ARRAY [0..{}] OF ULINT", count - 1))
                    }
                };
            }
            array_type(typ, model)
        }
        // Откат Option C: перечислимый тип MatIEC не принимает.
        TypeNode::Enum(name) => enum_type(name, model),
        // Ссылка на объявление `TYPE ... STRUCT ... END_STRUCT; END_TYPE`, которое
        // печатает `st_decl`.
        TypeNode::Struct(name) => struct_type(name, model),
        // Служебные узлы: типом переменной быть не могут. Цель `c` отдаёт здесь `None` -
        // тихий отказ (дефект Д4).
        TypeNode::Unit => Err(unmapped("unit", &msg!(keys::ST_002_WHY_UNIT))),
        TypeNode::Inference => Err(unmapped(
            &msg!(keys::ST_002_SHOWN_INFERENCE),
            &msg!(keys::ST_002_WHY_INFERENCE),
        )),
        TypeNode::Unsupported => Err(unmapped(
            &msg!(keys::ST_002_SHOWN_UNSUPPORTED),
            &msg!(keys::ST_002_WHY_UNSUPPORTED),
        )),
        TypeNode::Address(addr, _) => Err(unmapped(
            &format!("0x{:X}", addr),
            &msg!(keys::ST_002_WHY_ADDRESS),
        )),
        TypeNode::BuiltinString => Err(unmapped("string", &msg!(keys::ST_002_WHY_STRING))),
        TypeNode::BuiltinModel => Err(unmapped(
            &msg!(keys::ST_002_SHOWN_MODEL),
            &msg!(keys::ST_002_WHY_BUILTIN),
        )),
        TypeNode::BuiltinState => Err(unmapped(
            &msg!(keys::ST_002_SHOWN_STATE),
            &msg!(keys::ST_002_WHY_BUILTIN),
        )),
        TypeNode::BuiltinNumeric => Err(unmapped(
            &msg!(keys::ST_002_SHOWN_NUMERIC),
            &msg!(keys::ST_002_WHY_BUILTIN_NUMERIC),
        )),
        // Ветки `_` здесь нет - и это проверенный факт, а не недосмотр.
        //
        // Анализ предполагал её вынужденной: `TypeNode` помечен
        // `#[non_exhaustive]` (`type_node.rs:495`), значит исчерпывающий разбор
        // якобы невозможен, и ветка `_` обязана вернуть `ST-003`. Предположение
        // **опроверг компилятор**: `#[non_exhaustive]` ограничивает только
        // **внешние** крейты, а `generator/st` живёт в том же крейте, что и
        // `semantic` - здесь атрибут не действует, и ветка `_` помечается
        // `unreachable_patterns`.
        //
        // Следствие сильнее, чем задумывалось: новый вариант `TypeNode` **завалит
        // сборку** этого разбора, а не всплывёт диагностикой `ST-003` в времени выполнения.
        // Компилятор - гарантия строже теста, поэтому `ST-003` не занят и остаётся
        // свободным. Оговорка анализа ("новый вариант не завалит сборку") к
        // внутрикрейтовому коду не относится.
    }
}

/// Строит диагностику `ST-002` - тип без представления в IEC 61131-3.
fn unmapped(shown: &str, why: &str) -> Diagnostic {
    Diagnostic::error(
        crate::generator::site::at(Location::Codegen),
        msg!(keys::ST_002_UNMAPPED_TYPE, shown = shown, why = why),
    )
    .with_code("ST-002")
}

/// Имя целого типа IEC по разрядности и знаковости - публично для правила смешанного
/// сравнения: преобразование `X_TO_Y` строится из имён обоих типов, и второго списка
/// имён быть не должно.
pub(in crate::generator::st) fn iec_integer_name(bits: u8, signed: bool) -> Option<&'static str> {
    match (bits, signed) {
        (8, false) => Some("USINT"),
        (16, false) => Some("UINT"),
        (32, false) => Some("UDINT"),
        (64, false) => Some("ULINT"),
        (8, true) => Some("SINT"),
        (16, true) => Some("INT"),
        (32, true) => Some("DINT"),
        (64, true) => Some("LINT"),
        _ => None,
    }
}

/// Целые Takt отображаются в целые IEC.
///
/// `INT` в IEC - 16-битный знаковый, а не 32-битный, как в C.
fn integer_type(bits: u8, signed: bool) -> Result<String, Diagnostic> {
    let name = match (bits, signed) {
        (8, false) => "USINT",
        (16, false) => "UINT",
        (32, false) => "UDINT",
        (64, false) => "ULINT",
        (8, true) => "SINT",
        (16, true) => "INT",
        (32, true) => "DINT",
        (64, true) => "LINT",
        _ => {
            return Err(unmapped(
                &format!("{}{}", if signed { "i" } else { "u" }, bits),
                &msg!(keys::ST_002_WHY_INTEGER_WIDTH),
            ));
        }
    };
    Ok(name.to_string())
}

/// `Array(N, T)` отображается в `ARRAY [0..N-1] OF <T>`; вложенные массивы уплощаются в
/// многомерный `ARRAY [0..N-1, 0..M-1] OF <T>`.
///
/// Уплощение - не стилистика: `iec2c` **отвергает** `ARRAY OF ARRAY` (`invalid item
/// data type in array specification`), но принимает многомерную форму. Порядок
/// размерностей - от внешней к внутренней, как в индексации Takt: `[[u8; 2]; 3]` =
/// `Array(3, Array(2, u8))` -> `ARRAY [0..2, 0..1] OF USINT`.
fn array_type(typ: &TypeNode, model: &ModelNode) -> Result<String, Diagnostic> {
    let (dims, base) = array_dims_and_base(typ, model)?;
    let ranges: Vec<String> = dims.iter().map(|size| format!("0..{}", size - 1)).collect();
    Ok(format!("ARRAY [{}] OF {}", ranges.join(", "), base))
}

/// Размерности массива и его базовый тип: один разбор на объявление и на имя формы.
///
/// В IEC 61131-3 массивы не вкладываются, поэтому `[[u8; 2]; 2]` объявляется
/// многомерной формой `ARRAY [0..1, 0..1] OF USINT`. Имя типа формы (`array_form_name`)
/// обязано следовать тому же разбору: построенное из текста типа элемента, оно дало бы
/// для вложенного массива `TAKT_ARR_2_ARRAY_[0..1]_OF_USINT` - идентификатор со скобками
/// и точками, который `iec2c` не принимает ("unexpected token after 'TYPE'") при нулевом
/// коде возврата `taktc`.
fn array_dims_and_base(
    typ: &TypeNode,
    model: &ModelNode,
) -> Result<(Vec<u16>, String), Diagnostic> {
    let mut dims: Vec<u16> = Vec::new();
    let mut current = typ;
    while let TypeNode::Array(size, elem) = current {
        // Спуск останавливается на бит-Векторе: `[bit;N<=64]` - упакованный скаляр, а
        // не размерность. Признак берётся у того же слоя, что и печать самого
        // бит-вектора выше, - второе правило упаковки разъехалось бы с первым.
        if !std::ptr::eq(current, typ)
            && crate::semantic::bit_vector::is_bit_vector(current).is_some_and(|nbits| {
                matches!(
                    crate::semantic::bit_vector::layout(nbits),
                    crate::semantic::bit_vector::BitVectorLayout::Scalar { .. }
                )
            })
        {
            break;
        }
        if *size == 0 {
            return Err(Diagnostic::error(
                crate::generator::site::at(Location::Codegen),
                msg!(keys::ST_007_ZERO_SIZE_ARRAY, ty = typ),
            )
            .with_code("ST-007"));
        }
        dims.push(*size);
        current = elem;
    }
    // Базовый тип разбирается тем же отображением: массив структур или перечислений
    // обязан пройти проверку разрешимости имени.
    Ok((dims, get_st_type(current, model)?))
}

/// `Enum(name)` отображается в целочисленный тип, вмещающий все варианты.
///
/// **Почему не перечислимый `TYPE`.** MatIEC отвергает `TYPE F : (A := 80); END_TYPE` -
/// явные значения вариантов появились в третьей редакции IEC 61131-3, - а перечисления
/// Takt значения имеют (`enum Floor { Bottom = 80, Top }`), поэтому прямое отображение
/// непригодно.
///
/// **Почему разрядность считается, а не берётся `USINT`.** Плоский `USINT` корпуса не
/// выдерживает: `enum Action { Idle = 670, Closing }` в него не помещается, и вывод
/// молча усёк бы значение. Разрядность выбирается по фактическому диапазону вариантов -
/// так же, как это делает цель `c`.
fn enum_type(name: &str, model: &ModelNode) -> Result<String, Diagnostic> {
    let node = model
        .search_enum(name)
        .ok_or_else(|| unresolved(&msg!(keys::ST_KIND_ENUM), name))?;
    // Знак и ширина - из общего факта: цель лишь отображает его в имя типа IEC через
    // `integer_type`. Свой каскад извлечения диапазона удалён (в `generator/` не
    // остаётся ни одного - ).
    match enum_facts(&node.variants) {
        Some(f) => integer_type(f.machine_bits() as u8, f.signed),
        // Пустое перечисление - поведение сохраняется сегодняшним (`USINT`).
        None => integer_type(8, false),
    }
}

/// `Struct(name)` отображается в ссылку на объявленный `TYPE`; само объявление печатает
/// `st_decl`.
fn struct_type(name: &str, model: &ModelNode) -> Result<String, Diagnostic> {
    if model.search_struct(name).is_none() {
        return Err(unresolved(&msg!(keys::ST_KIND_STRUCT), name));
    }
    Ok(name.to_string())
}

/// Строит диагностику `ST-008` - именованный тип не разрешается в модели.
fn unresolved(kind: &str, name: &str) -> Diagnostic {
    Diagnostic::error(
        crate::generator::site::at(Location::Codegen),
        msg!(
            keys::ST_008_UNRESOLVED_DECLARATION,
            kind = kind,
            name = name
        ),
    )
    .with_code("ST-008")
}

/// Имя именованного типа для массива, разделяемого через `VAR_IN_OUT`.
pub(crate) fn shared_array_type_name(owner: &str, var: &str) -> String {
    format!("{}_{}_arr", normalize_camelcase_name(owner), var)
}

/// Имя формы массива для объявления в секции `TYPE`.
///
/// MatIEC не принимает анонимный `ARRAY [...] OF T` в параметре `FUNCTION` ("Data type
/// incompatibility for value passed in position 1"), а типы аргумента и параметра
/// обязаны **совпадать** - проверено пробой. Поэтому имя строится по самой форме
/// (размер и тип элемента), а не по владельцу: все переменные и параметры одной формы
/// получают один тип.
///
/// `[bit; N <= 64]` сюда не попадает: это упакованный
/// **скаляр**, и MatIEC принимает его как есть.
pub(crate) fn array_form_name(ty: &TypeNode, model: &ModelNode) -> Option<String> {
    if !needs_named_array_type(ty, model) {
        return None;
    }
    if !matches!(ty, TypeNode::Array(_, _)) {
        return None;
    }
    // Размерности и база берутся тем же разбором, что и объявление: имя
    // `TAKT_ARR_2_2_USINT` отвечает `ARRAY [0..1, 0..1] OF USINT`, и формы совпадают по
    // построению, а не по дисциплине.
    let (dims, base) = array_dims_and_base(ty, model).ok()?;
    let sizes: Vec<String> = dims.iter().map(u16::to_string).collect();
    Some(format!(
        "TAKT_ARR_{}_{}",
        sizes.join("_"),
        base.replace(' ', "_").to_uppercase()
    ))
}

/// Тип локального объявления тела: именованная форма, если она объявлена.
///
/// Локальная переменная-массив попадает в аргумент вызова так же, как переменная
/// модели, а MatIEC сверяет типы **буквально**: анонимный `ARRAY [...] OF T` против
/// именованного `TAKT_ARR_2_USINT` для него несовместимы ("Data type incompatibility
/// for value passed in position 1").
///
/// Правило действует и на локальной переменной, и на переменной модели: пропусти оно
/// локальную - и `var part: [u8; 2] := {6, 7}; o := first(part);` даст вывод,
/// отвергаемый `iec2c`, при нулевом коде возврата `taktc`.
///
/// Список форм спрашивается тот же, что у продюсера `TYPE ... END_TYPE`
/// (`function_array_form_names`): разъехавшись, они дали бы ссылку в пустоту.
pub(crate) fn local_declaration_type(
    ty: &TypeNode,
    model: &ModelNode,
    array_forms: &[String],
) -> Result<String, Diagnostic> {
    match array_form_name(ty, model) {
        Some(form) if array_forms.contains(&form) => Ok(form),
        _ => get_st_type(ty, model),
    }
}

/// Требует ли тип именованного объявления при передаче через `VAR_IN_OUT`.
///
/// Только настоящий массив: `[bit; N <= 64]` - упакованный **скаляр** (`bit_vector`), и
/// его MatIEC принимает как есть.
pub(crate) fn needs_named_array_type(ty: &TypeNode, model: &ModelNode) -> bool {
    matches!(ty, TypeNode::Array(_, _))
        && crate::semantic::bit_vector::is_bit_vector(ty).is_none()
        && get_st_type(ty, model).is_ok_and(|t| t.starts_with("ARRAY "))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic::tree::construct_model;
    use crate::semantic::{EnumDefinitionNode, StructDefinitionNode};

    /// Пустая модель - для типов, не требующих разрешения имён.
    ///
    /// `ModelNode` не реализует `Clone`, поэтому модель живёт в `Rc<RefCell<...>>` и
    /// заимствуется по месту.
    fn empty_model() -> std::rc::Rc<std::cell::RefCell<ModelNode>> {
        let (ast, _) = crate::parse("start S;", 0).unwrap();
        construct_model(&ast, None, &[]).unwrap()
    }

    fn st_of(typ: &TypeNode) -> String {
        get_st_type(typ, &empty_model().borrow()).expect("тип должен отображаться")
    }

    fn code_of(typ: &TypeNode, model: &ModelNode) -> String {
        get_st_type(typ, model)
            .expect_err("ожидалась диагностика")
            .code
            .expect("диагностика обязана иметь код")
    }

    /// `bit` отображается в `BOOL`, а не в целое.
    #[test]
    fn test_get_st_type_bit_is_bool_not_int() {
        assert_eq!(st_of(&TypeNode::Bit), "BOOL");
    }

    /// `bool` отображается в `BOOL`.
    #[test]
    fn test_get_st_type_bool() {
        assert_eq!(st_of(&TypeNode::Bool), "BOOL");
    }

    /// Целые следуют ширинам IEC: `INT` там 16-битный знаковый, это не опечатка.
    #[test]
    fn test_get_st_type_integers_follow_iec_widths() {
        let cases = [
            (8, false, "USINT"),
            (16, false, "UINT"),
            (32, false, "UDINT"),
            (64, false, "ULINT"),
            (8, true, "SINT"),
            (16, true, "INT"),
            (32, true, "DINT"),
            (64, true, "LINT"),
        ];
        for (bits, signed, expected) in cases {
            assert_eq!(
                st_of(&TypeNode::Integer { bits, signed }),
                expected,
                "разрядность {bits}, знаковый={signed}"
            );
        }
    }

    /// Нестандартная разрядность - ошибка, а не молчаливая подстановка.
    #[test]
    fn test_get_st_type_odd_integer_width_is_error() {
        assert_eq!(
            code_of(
                &TypeNode::Integer {
                    bits: 24,
                    signed: false
                },
                &empty_model().borrow()
            ),
            "ST-002"
        );
    }

    /// `float` отображается в `LREAL`, а не в `REAL`: ширина та же, что у эталона.
    #[test]
    fn test_get_st_type_rational_is_lreal_not_real() {
        assert_eq!(st_of(&TypeNode::Rational), "LREAL");
    }

    /// Массив отображается в настоящий `ARRAY`, а не в тип по числу элементов.
    #[test]
    fn test_get_st_type_array_uses_element_count_as_range() {
        assert_eq!(
            st_of(&TypeNode::Array(
                4,
                Box::new(TypeNode::Integer {
                    bits: 8,
                    signed: false
                })
            )),
            "ARRAY [0..3] OF USINT"
        );
    }

    /// Вложенный массив уплощается в многомерный.
    ///
    /// Форма продиктована фактом, а не вкусом: `iec2c` отвергает `ARRAY [0..2] OF ARRAY
    /// [0..1] OF USINT` ("invalid item data type in array specification"), но принимает
    /// `ARRAY [0..2, 0..1] OF USINT`. Порядок размерностей - внешняя первой, как в
    /// индексации Takt.
    #[test]
    fn test_get_st_type_nested_array_is_flattened_to_multidim() {
        let inner = TypeNode::Array(
            2,
            Box::new(TypeNode::Integer {
                bits: 8,
                signed: false,
            }),
        );
        assert_eq!(
            st_of(&TypeNode::Array(3, Box::new(inner))),
            "ARRAY [0..2, 0..1] OF USINT"
        );
    }

    /// Массив нулевого размера невыразим -> `ST-007`, а не пустой диапазон.
    #[test]
    fn test_get_st_type_zero_sized_array_is_st007() {
        assert_eq!(
            code_of(
                &TypeNode::Array(0, Box::new(TypeNode::Bit)),
                &empty_model().borrow()
            ),
            "ST-007"
        );
    }

    /// Перечисление отображается в целое, вмещающее его варианты.
    ///
    /// `Floor { Bottom = 80, Top }` даёт варианты `[("Bottom", 80), ("Top", 81)]`: `Top`
    /// наследует 81.
    #[test]
    fn test_get_st_type_enum_fits_in_usint() {
        let rc = empty_model();
        rc.borrow_mut().enums.insert(
            "Floor".to_string(),
            EnumDefinitionNode::new("Floor", &[("Bottom", Some(80)), ("Top", None)]),
        );
        assert_eq!(
            get_st_type(&TypeNode::Enum("Floor".to_string()), &rc.borrow()).unwrap(),
            "USINT"
        );
    }

    /// Перечисление шире байта получает более широкий тип, а не усекается.
    ///
    /// Вход не гипотетический: `enum Action { Idle = 670, Closing }` -
    /// `examples/elevator.takt:121`. Плоский `USINT` (как предполагалось) усёк бы 670
    /// молча.
    #[test]
    fn test_get_st_type_enum_wider_than_byte_is_widened_not_truncated() {
        let rc = empty_model();
        rc.borrow_mut().enums.insert(
            "Action".to_string(),
            EnumDefinitionNode::new("Action", &[("Idle", Some(670)), ("Closing", None)]),
        );
        assert_eq!(
            get_st_type(&TypeNode::Enum("Action".to_string()), &rc.borrow()).unwrap(),
            "UINT",
            "670 не помещается в USINT — тип обязан расшириться"
        );
    }

    /// Отрицательные варианты требуют знакового типа.
    #[test]
    fn test_get_st_type_enum_with_negative_variant_is_signed() {
        let rc = empty_model();
        rc.borrow_mut().enums.insert(
            "Dir".to_string(),
            EnumDefinitionNode::new("Dir", &[("Down", Some(-1)), ("Up", Some(1))]),
        );
        assert_eq!(
            get_st_type(&TypeNode::Enum("Dir".to_string()), &rc.borrow()).unwrap(),
            "SINT"
        );
    }

    /// Неразрешимое перечисление -> `ST-008`, а не тихий пропуск.
    #[test]
    fn test_get_st_type_unresolved_enum_is_st008() {
        assert_eq!(
            code_of(
                &TypeNode::Enum("Missing".to_string()),
                &empty_model().borrow()
            ),
            "ST-008"
        );
    }

    /// Структура отображается в ссылку на имя объявленного `TYPE`.
    #[test]
    fn test_get_st_type_struct_refers_to_declared_type() {
        let rc = empty_model();
        rc.borrow_mut().structs.insert(
            "Point".to_string(),
            StructDefinitionNode::new("Point", &[("x", TypeNode::Bit)]),
        );
        assert_eq!(
            get_st_type(&TypeNode::Struct("Point".to_string()), &rc.borrow()).unwrap(),
            "Point"
        );
    }

    /// Неразрешимая структура -> `ST-008`.
    #[test]
    fn test_get_st_type_unresolved_struct_is_st008() {
        assert_eq!(
            code_of(
                &TypeNode::Struct("Missing".to_string()),
                &empty_model().borrow()
            ),
            "ST-008"
        );
    }

    /// Служебные типы дают ошибку `ST-002`, а не молчаливое отсутствие типа.
    #[test]
    fn test_get_st_type_unmappable_types_are_st002() {
        let cases = [
            TypeNode::Inference,
            TypeNode::Unsupported,
            TypeNode::Unit,
            TypeNode::Address(0x100, Some(0)),
            TypeNode::BuiltinString,
            TypeNode::BuiltinModel,
            TypeNode::BuiltinState,
            TypeNode::BuiltinNumeric,
        ];
        let rc = empty_model();
        for typ in cases {
            assert_eq!(code_of(&typ, &rc.borrow()), "ST-002", "тип {:?}", typ);
        }
    }

    /// Полнота разбора: каждый вариант `TypeNode` даёт либо тип, либо ошибку с кодом, но
    /// никогда - тихий исход без ответа.
    ///
    /// `TypeNode` помечен `#[non_exhaustive]`, поэтому исчерпывающий `match` в
    /// `get_st_type` невозможен и компилятор о новом варианте не предупредит. Этот тест
    /// заменяет такое предупреждение: список ведётся вручную, и его расхождение с
    /// перечислением видит читатель.
    #[test]
    fn test_get_st_type_covers_all_variants() {
        let rc = empty_model();
        rc.borrow_mut().enums.insert(
            "E".to_string(),
            EnumDefinitionNode::new("E", &[("A", None)]),
        );
        rc.borrow_mut().structs.insert(
            "S".to_string(),
            StructDefinitionNode::new("S", &[("f", TypeNode::Bit)]),
        );
        let all = [
            TypeNode::Inference,
            TypeNode::Address(0, None),
            TypeNode::Bit,
            TypeNode::Bool,
            TypeNode::Rational,
            TypeNode::Array(1, Box::new(TypeNode::Bit)),
            TypeNode::Enum("E".to_string()),
            TypeNode::Unsupported,
            TypeNode::Unit,
            TypeNode::BuiltinString,
            TypeNode::BuiltinModel,
            TypeNode::BuiltinState,
            TypeNode::BuiltinNumeric,
            TypeNode::Struct("S".to_string()),
            TypeNode::Integer {
                bits: 8,
                signed: false,
            },
        ];
        assert_eq!(
            all.len(),
            15,
            "список вариантов TypeNode разошёлся с тестом"
        );
        for typ in all {
            match get_st_type(&typ, &rc.borrow()) {
                Ok(name) => assert!(!name.is_empty(), "пустое имя типа для {:?}", typ),
                Err(d) => assert!(
                    d.code.is_some(),
                    "отказ без кода диагностики для {:?} — это тихий пропуск",
                    typ
                ),
            }
        }
    }

    /// Имя формы вложенного массива следует объявлению.
    #[test]
    fn nested_array_form_name_follows_the_declaration() {
        let rc = empty_model();
        let model = rc.borrow();
        let inner = TypeNode::Array(
            2,
            Box::new(TypeNode::Integer {
                bits: 8,
                signed: false,
            }),
        );
        let nested = TypeNode::Array(2, Box::new(inner.clone()));
        assert_eq!(
            get_st_type(&nested, &model).unwrap(),
            "ARRAY [0..1, 0..1] OF USINT"
        );
        assert_eq!(
            array_form_name(&nested, &model).as_deref(),
            Some("TAKT_ARR_2_2_USINT")
        );
        // Контроль: одномерная форма не изменилась - вывод корпуса на месте.
        assert_eq!(
            array_form_name(&inner, &model).as_deref(),
            Some("TAKT_ARR_2_USINT")
        );
    }

    /// Спуск останавливается на бит-векторе: `[bit;8]` - упакованный скаляр, и
    /// объявление у него одномерное.
    #[test]
    fn bit_vector_element_is_not_a_dimension_in_the_form_name() {
        let rc = empty_model();
        let model = rc.borrow();
        let ty = TypeNode::Array(2, Box::new(TypeNode::Array(8, Box::new(TypeNode::Bit))));
        assert_eq!(get_st_type(&ty, &model).unwrap(), "ARRAY [0..1] OF USINT");
        assert_eq!(
            array_form_name(&ty, &model).as_deref(),
            Some("TAKT_ARR_2_USINT")
        );
    }
}
