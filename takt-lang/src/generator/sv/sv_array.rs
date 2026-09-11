//! Распакованный массив у цели `sv`: сброс и индекс.
//!
//! Массив скаляров печатается **распакованным** (`logic [7:0] a [0:1]`), и две операции
//! над ним требуют своих форм - иначе вывод не проходит проверка собственной цели при
//! нулевом коде возврата `taktc`:
//!
//! - **сброс без инициализатора.** `a <= '0;` verilator встречает **ошибкой**
//!   "CONST '8'h0' is not an unpacked array, but is in an unpacked array
//!   context". Форма `'{default: '0}` не годится: её **не принимает yosys**
//!   ("syntax error, unexpected TOK_DEFAULT"), а форма выбирается по тому, что
//!   принимают **оба** инструмента. Годная - тот же агрегат, каким
//!   печатается инициализатор: `'{8'd0, 8'd0}`;
//! - **переменный индекс.** `a[i]` при `i: u8` и массиве из двух элементов даёт
//!   `%Warning-WIDTHTRUNC: Bit extraction of array[1:0] requires 1 bit index,
//!   not 8 bits`, а проверка цели считает предупреждение ошибкой. Лечится явным
//!   сужением `a[1'(i)]`.
//!
//! Бит-вектор `[bit;N<=64]` под правило **не подпадает**: это упакованный скаляр, у
//! него и сброс `'0`, и индексация разряда законны.

use std::collections::BTreeMap;

use crate::diagnostics::Diagnostic;
use crate::semantic::type_node::TypeNode;
use crate::semantic::{ConditionNode, ExpressionNode};

use super::sv_type::scalar_width;
use crate::diagnostics::lang::keys;
use crate::msg;

/// Значение сброса распакованного массива - агрегат нулей.
///
/// `None`, если тип массивом не является либо его элемент не имеет одной скалярной
/// ширины (массив структур: их сброс печатает свой путь).
pub(crate) fn reset_literal(ty: &TypeNode) -> Option<String> {
    let TypeNode::Array(size, elem) = ty else {
        return None;
    };
    if crate::semantic::bit_vector::is_bit_vector(ty).is_some() {
        return None;
    }
    // Вложенный массив - тот же агрегат уровнем ниже: правило рекурсивно, а второго
    // знания о раскладке заводить нельзя.
    let zero = match &**elem {
        TypeNode::Array(_, _) => reset_literal(elem)?,
        other => format!("{}'d0", scalar_width(other)?),
    };
    let items = vec![zero; usize::from(*size)];
    Some(format!("'{{{}}}", items.join(", ")))
}

/// Печатает индекс массива, сужая его до ширины, которую требует размер.
///
/// `base_ty` - тип **базы** индексации (если известен), `index_ty` - тип индекса
/// (только именованное значение: у литерала ширины нет, он подстраивается под
/// контекст). Приведение печатается **по нужде** - когда ширина индекса заведомо больше
/// требуемой; лишнее приведение сузило бы вывод там, где предупреждения нет.
pub(crate) fn index_text(
    base_ty: Option<&TypeNode>,
    index_ty: Option<&TypeNode>,
    printed: String,
) -> String {
    let Some(TypeNode::Array(size, _)) = base_ty else {
        return printed;
    };
    if base_ty.is_some_and(|ty| crate::semantic::bit_vector::is_bit_vector(ty).is_some()) {
        return printed;
    }
    let Some(index_width) = index_ty.and_then(scalar_width) else {
        return printed;
    };
    let needed = index_width_for(*size);
    if index_width <= needed {
        return printed;
    }
    format!("{needed}'({printed})")
}

/// Тип массива-базы индексации - для выражений.
///
/// Разбирается цепочка `переменная([индекс])*`: у вложенной индексации база сама
/// является индексацией, и без спуска `cells[i][j]` сузил бы только внешний индекс
///
///
/// Опирается на "именованное значение" (`mixed_sign::operand_type_expr`): у литерала и
/// произвольного выражения типа здесь нет, и догадываться о нём нельзя.
pub(crate) fn array_type_expr(expr: &ExpressionNode) -> Option<TypeNode> {
    match expr {
        ExpressionNode::Parenthesis(inner) => array_type_expr(inner),
        ExpressionNode::ArraySubscript(base, _) => match array_type_expr(base)? {
            TypeNode::Array(_, elem) => Some(*elem),
            _ => None,
        },
        other => crate::generator::mixed_sign::operand_type_expr(other),
    }
}

/// То же для условий: у них своё дерево.
pub(crate) fn array_type_cond(cond: &ConditionNode) -> Option<TypeNode> {
    match cond {
        ConditionNode::Parenthesis(inner) => array_type_cond(inner),
        ConditionNode::ArraySubscript(base, _) => match array_type_cond(base)? {
            TypeNode::Array(_, elem) => Some(*elem),
            _ => None,
        },
        other => crate::generator::mixed_sign::operand_type_cond(other),
    }
}

/// Ширина индекса, которую требует массив из `size` элементов.
///
/// Диапазон `[0:size-1]` адресуется `ceil(log2(size))` разрядами, а массив из одного
/// элемента - одним (нулевой ширины в SystemVerilog не бывает).
fn index_width_for(size: u16) -> u32 {
    let last = u32::from(size.saturating_sub(1));
    if last == 0 {
        1
    } else {
        u32::BITS - last.leading_zeros()
    }
}

/// Листья типа: суффикс и тип каждого скалярного места.
///
/// `None` - тип листьями не раскладывается, то есть присваивается целиком. Раскладка
/// нужна там, где внутри распакованного массива лежит структура: yosys принимает шаблон
/// присваивания только для массива целиком, а частичную запись поля элемента при
/// умолчании массива целиком объявляет защёлкой.
/// Значит и умолчание, и сброс такого регистра печатаются **по полям**.
pub(crate) fn type_leaves(
    ty: &TypeNode,
    fields_of: &crate::generator::aggregate::FieldsOf<'_>,
) -> Option<Vec<(String, TypeNode)>> {
    if !needs_leafwise(ty, fields_of) {
        return None;
    }
    let mut out = Vec::new();
    walk_type(ty, fields_of, &mut String::new(), &mut out);
    Some(out)
}

/// Лежит ли структура внутри распакованного массива.
///
/// Только этот случай синтезатор не принимает целиком: массив скаляров присваивается
/// whole-array, а структура вне массива - упакованная, и её
/// присваивание тоже законно.
pub(crate) fn needs_leafwise(
    ty: &TypeNode,
    fields_of: &crate::generator::aggregate::FieldsOf<'_>,
) -> bool {
    match ty {
        TypeNode::Array(_, elem) if crate::semantic::bit_vector::is_bit_vector(ty).is_none() => {
            contains_struct(elem, fields_of)
        }
        _ => false,
    }
}

/// Нулевые умолчания по листьям для поднятой локальной переменной.
///
/// Возвращает пары `(суффикс пути, нулевое значение)`. Умолчание печатается безусловно
/// в начале `always_comb`: без него yosys объявляет защёлку, потому что тело пишет поля
/// лишь в своей ветви `case`.
pub(crate) fn leaf_zero_defaults(
    ty: &TypeNode,
    fields_of: &crate::generator::aggregate::FieldsOf<'_>,
) -> Vec<(String, String)> {
    // Упакованное значение (структура вне массива) сбрасывается целиком: тело
    // присваивает его целиком (`made = make(n);`), и умолчание по полям с таким
    // присваиванием не сходится - yosys объявляет защёлку. Правило
    // то же, что у регистров: по листьям идёт только структура внутри распакованного
    // массива.
    let mut out = Vec::new();
    if !needs_leafwise(ty, fields_of) {
        // Упакованное значение (структура вне массива) сбрасывается и целиком, и по
        // полям - потому что тело вправе писать его обоими способами: `made = make(n);`
        // либо `tmp.lo = ...`. Умолчание, не совпавшее по форме с записью, yosys
        // защёлкой не считает - он видит незаданной ту "форму", которой присваивает
        // тело. Значение одно и то же, поэтому лишним присваиванием
        // смысл не меняется.
        out.push((String::new(), "'0".to_string()));
    }
    let mut nodes = Vec::new();
    walk_type(ty, fields_of, &mut String::new(), &mut nodes);
    out.extend(nodes.into_iter().map(|(suffix, leaf_ty)| {
        // Ширина листа берётся у `scalar_width`; если её нет (лист сам структура, чьи
        // поля неизвестны), печатается `'0` - форма, законная для любого упакованного
        // значения.
        let zero = match scalar_width(&leaf_ty) {
            Some(width) => format!("{width}'d0"),
            None => "'0".to_string(),
        };
        (suffix, zero)
    }));
    out
}

/// Содержит ли тип структуру - сам ею будучи либо элементом массива.
///
/// Признак нужен и локальным объявлениям цели: запись в поле yosys полным присваиванием
/// не считает.
pub(crate) fn contains_struct(
    ty: &TypeNode,
    fields_of: &crate::generator::aggregate::FieldsOf<'_>,
) -> bool {
    match ty {
        TypeNode::Struct(name) => fields_of(name).is_some(),
        TypeNode::Array(_, elem) if crate::semantic::bit_vector::is_bit_vector(ty).is_none() => {
            contains_struct(elem, fields_of)
        }
        _ => false,
    }
}

fn walk_type(
    ty: &TypeNode,
    fields_of: &crate::generator::aggregate::FieldsOf<'_>,
    prefix: &mut String,
    out: &mut Vec<(String, TypeNode)>,
) {
    match ty {
        TypeNode::Array(size, elem) if crate::semantic::bit_vector::is_bit_vector(ty).is_none() => {
            for index in 0..usize::from(*size) {
                let saved = prefix.len();
                prefix.push_str(&format!("[{index}]"));
                walk_type(elem, fields_of, prefix, out);
                prefix.truncate(saved);
            }
        }
        TypeNode::Struct(name) => match fields_of(name) {
            Some(fields) => {
                for (field, field_ty) in fields {
                    let saved = prefix.len();
                    prefix.push('.');
                    prefix.push_str(&field);
                    walk_type(&field_ty, fields_of, prefix, out);
                    prefix.truncate(saved);
                }
            }
            None => out.push((prefix.clone(), ty.clone())),
        },
        other => out.push((prefix.clone(), other.clone())),
    }
}

/// Сброс по листьям, если тип того требует.
///
/// Пустой вектор означает сброс регистра целиком. Значения листьев берутся у
/// инициализатора, когда он агрегат, и у умолчания типа - когда его нет.
pub(crate) fn leafwise_reset(
    expr: &crate::semantic::ExpressionNode,
    ty: &crate::semantic::type_node::TypeNode,
    enums: &BTreeMap<String, Vec<(String, i128)>>,
    structs: &BTreeMap<String, Vec<(String, crate::semantic::type_node::TypeNode)>>,
    loc: crate::diagnostics::Location,
    what: &str,
) -> Result<Vec<(String, String)>, Diagnostic> {
    use crate::semantic::ExpressionNode;

    let fields_of = |name: &str| structs.get(name).cloned();
    let Some(type_leaves) = type_leaves(ty, &fields_of) else {
        return Ok(Vec::new());
    };
    if let ExpressionNode::Initializer(items) | ExpressionNode::Array(items) = expr {
        let mut out = Vec::new();
        for leaf in crate::generator::aggregate::leaves(Some(ty), items, &fields_of) {
            let suffix = crate::generator::aggregate::c_like_suffix(&leaf.path);
            let leaf_ty = leaf.ty.clone().unwrap_or_else(|| ty.clone());
            let value = super::sv_const::reset_value(leaf.value, &leaf_ty, enums, what, loc, None)?;
            out.push((suffix, value));
        }
        return Ok(out);
    }
    // Инициализатора нет: каждый лист получает умолчание своего типа.
    let mut out = Vec::new();
    for (suffix, leaf_ty) in type_leaves {
        let value =
            super::sv_const::reset_value(&ExpressionNode::None, &leaf_ty, enums, what, loc, None)?;
        out.push((suffix, value));
    }
    Ok(out)
}

/// Имя плоского параметра, которым передаётся массив.
pub(crate) fn flat_param_name(param: &str) -> String {
    format!("{param}_flat")
}

/// Раскладка параметра-массива по плоскому вектору.
///
/// Части - в порядке разрядов: первая лежит в **младших**. Спуск идёт только по
/// Распакованным размерностям, и частью становится элемент: у массива скаляров это
/// `[0]`, `[1]`, ..., у массива структур - сами структуры (они упакованы), у вложенного
/// массива - элементы внутреннего.
pub(crate) struct FlatParam {
    /// Ширина плоского вектора: сумма ширин частей.
    pub(crate) width: u32,
    /// Суффикс пути к части, её ширина и тип - от младших разрядов к старшим.
    pub(crate) parts: Vec<(String, u32, TypeNode)>,
}

/// Раскладка массива в плоский вектор, если она у типа есть.
///
/// `None` - тип не распакованный массив (бит-вектор передаётся как есть) либо у
/// какой-то части нет ширины: угадывать её нельзя, и такой параметр печатается обычным
/// путём.
///
/// **Спуск останавливается на упакованном типе, и это замер, а не вкус.** Разложи
/// носитель структуру по полям - и yosys ответит "Latch inferred for signal
/// `...a[0].lo`": запись полей элемента внутри `always_comb` он полным присваиванием не
/// считает. Присваивание структуры целиком принимают оба инструмента.
pub(crate) fn flat_param(
    ty: &TypeNode,
    fields_of: &crate::generator::aggregate::FieldsOf<'_>,
    enums: &BTreeMap<String, Vec<(String, i128)>>,
) -> Option<FlatParam> {
    if !matches!(ty, TypeNode::Array(_, _))
        || crate::semantic::bit_vector::is_bit_vector(ty).is_some()
    {
        return None;
    }
    let mut parts = Vec::new();
    walk_dimensions(ty, &mut String::new(), &mut parts);
    let mut out = Vec::new();
    let mut width = 0;
    for (suffix, part_ty) in parts {
        let part_width = packed_width(&part_ty, fields_of, enums)?;
        width += part_width;
        out.push((suffix, part_width, part_ty));
    }
    (!out.is_empty()).then_some(FlatParam { width, parts: out })
}

/// Спуск по распакованным размерностям: элемент - часть, дальше не идём.
fn walk_dimensions(ty: &TypeNode, prefix: &mut String, out: &mut Vec<(String, TypeNode)>) {
    match ty {
        TypeNode::Array(size, elem) if crate::semantic::bit_vector::is_bit_vector(ty).is_none() => {
            for index in 0..usize::from(*size) {
                let saved = prefix.len();
                prefix.push_str(&format!("[{index}]"));
                walk_dimensions(elem, prefix, out);
                prefix.truncate(saved);
            }
        }
        other => out.push((prefix.clone(), other.clone())),
    }
}

/// Ширина упакованного типа в разрядах.
///
/// Перечисление спрашивается у `enum_width`, структура складывается по полям - у цели
/// `sv` она `struct packed`, то есть вектор своей ширины. Второго знания о ширинах не
/// заводится.
fn packed_width(
    ty: &TypeNode,
    fields_of: &crate::generator::aggregate::FieldsOf<'_>,
    enums: &BTreeMap<String, Vec<(String, i128)>>,
) -> Option<u32> {
    match ty {
        TypeNode::Enum(name) => {
            let variants = enums.get(name)?;
            super::sv_type::enum_width(variants, &msg!(keys::SV_WHAT_ARRAY_PARAMETER))
                .ok()
                .map(|(width, _)| width)
        }
        TypeNode::Struct(name) => {
            let fields = fields_of(name)?;
            let mut width = 0;
            for (_, field_ty) in fields {
                width += packed_width(&field_ty, fields_of, enums)?;
            }
            Some(width)
        }
        other => scalar_width(other),
    }
}

/// Печатает пролог распаковки параметра-массива.
///
/// Плоский вектор раскладывается в переменную **с именем параметра**, и тело печатается
/// дальше прежними печатниками - второго знания об адресации не заводится.
///
/// Часть-Перечисление приводится к своему типу: перечисления в
/// SystemVerilog строго типизированы, и без приведения verilator отвечает
/// **ошибкой** `ENUMVALUE` ("Implicit conversion to enum ... from logic[1:0]"),
/// а проверка цели считает предупреждение ошибкой.
pub(crate) fn emit_unpack_prologue(
    p: &mut crate::generator::indent::Printer,
    param: &str,
    ty: &TypeNode,
    flat: &FlatParam,
    function: &str,
) -> Result<(), Diagnostic> {
    let decl = super::sv_type::sv_type(
        ty,
        &msg!(
            keys::GEN_WHAT_FUNCTION_PARAM,
            name = param,
            function = function
        ),
    )?;
    p.ident(&format!("{};", decl.declare(param))).nl();
    let flat_name = flat_param_name(param);
    let mut low = 0;
    for (suffix, part_width, part_ty) in &flat.parts {
        let slice = format!("{flat_name}[{}:{}]", low + part_width - 1, low);
        let value = match part_ty {
            TypeNode::Enum(enum_name) => {
                format!(
                    "{}'({})",
                    super::sv_type::sv_enum_type_name(enum_name),
                    slice
                )
            }
            _ => slice,
        };
        p.ident(&format!("{param}{suffix} = {value};")).nl();
        low += part_width;
    }
    Ok(())
}

/// Аргумент-массив печатается конкатенацией частей.
///
/// Порядок обратный: первая часть ложится в **младшие** разряды, и распаковка в прологе
/// функции читает те же разряды. Один порядок на сигнатуру, пролог и аргумент:
/// разойдясь, они дали бы валидный RTL с другими значениями.
pub(crate) fn flatten_argument(base: &str, flat: &FlatParam) -> String {
    let items: Vec<String> = flat
        .parts
        .iter()
        .rev()
        .map(|(suffix, _, _)| format!("{base}{suffix}"))
        .collect();
    format!("{{{}}}", items.join(", "))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn u(bits: u8) -> TypeNode {
        TypeNode::Integer {
            bits,
            signed: false,
        }
    }

    /// Структур в модели нет: имя не разрешается ни во что.
    const NO_FIELDS: &crate::generator::aggregate::FieldsOf<'static> = &|_: &str| None;

    #[test]
    fn reset_of_unpacked_array_is_aggregate_of_zeros() {
        let ty = TypeNode::Array(2, Box::new(u(8)));
        assert_eq!(reset_literal(&ty).as_deref(), Some("'{8'd0, 8'd0}"));
    }

    /// Вложенный массив - агрегат агрегатов: правило рекурсивно.
    #[test]
    fn reset_of_nested_array_is_nested_aggregate() {
        let inner = TypeNode::Array(2, Box::new(u(8)));
        let ty = TypeNode::Array(2, Box::new(inner));
        assert_eq!(
            reset_literal(&ty).as_deref(),
            Some("'{'{8'd0, 8'd0}, '{8'd0, 8'd0}}")
        );
    }

    /// Бит-вектор `[bit;8]` - упакованный скаляр: его сброс печатает прежний путь
    /// (`'0`), и агрегата здесь быть не должно.
    #[test]
    fn packed_bit_vector_is_not_an_unpacked_array() {
        let ty = TypeNode::Array(8, Box::new(TypeNode::Bit));
        assert_eq!(reset_literal(&ty), None);
    }

    #[test]
    fn wide_index_is_narrowed_to_the_width_the_size_requires() {
        let base = TypeNode::Array(2, Box::new(u(8)));
        assert_eq!(
            index_text(Some(&base), Some(&u(8)), "i".to_string()),
            "1'(i)"
        );
        let base4 = TypeNode::Array(4, Box::new(u(8)));
        assert_eq!(
            index_text(Some(&base4), Some(&u(8)), "i".to_string()),
            "2'(i)"
        );
    }

    /// Приведение печатается по нужде: индекс, чья ширина уже не больше требуемой,
    /// остаётся как есть - лишнее приведение сужало бы вывод там, где предупреждения
    /// нет.
    #[test]
    fn narrow_index_is_printed_as_is() {
        let base = TypeNode::Array(2, Box::new(u(8)));
        assert_eq!(
            index_text(Some(&base), Some(&TypeNode::Bit), "b".to_string()),
            "b"
        );
        // Тип индекса неизвестен (литерал, выражение) - не трогаем.
        assert_eq!(index_text(Some(&base), None, "1".to_string()), "1");
        // База не массив - не трогаем.
        assert_eq!(index_text(None, Some(&u(8)), "i".to_string()), "i");
    }

    /// Массив структур раскладывается по полям: whole-array присваивание синтезатор
    /// принимает, а частичную запись поля при нём - нет.
    #[test]
    fn array_of_structs_is_split_into_field_leaves() {
        let ty = TypeNode::Array(2, Box::new(TypeNode::Struct("Point".to_string())));
        let fields = |name: &str| {
            (name == "Point").then(|| vec![("x".to_string(), u(8)), ("y".to_string(), u(8))])
        };
        let leaves = type_leaves(&ty, &fields).expect("массив структур раскладывается");
        let names: Vec<_> = leaves.iter().map(|(s, _)| s.clone()).collect();
        assert_eq!(names, vec!["[0].x", "[0].y", "[1].x", "[1].y"]);
    }

    /// Массив скаляров присваивается целиком - раскладка не нужна.
    #[test]
    fn array_of_scalars_is_assigned_as_a_whole() {
        let ty = TypeNode::Array(2, Box::new(u(8)));
        let no_fields = |_: &str| None;
        assert!(type_leaves(&ty, &no_fields).is_none());
    }

    /// Аргумент-массив печатается конкатенацией в обратном порядке: `a[0]` обязан лечь
    /// в младшие разряды, иначе распаковка вернёт другой элемент.
    #[test]
    fn array_argument_is_concatenated_low_element_last() {
        let ty = TypeNode::Array(3, Box::new(u(8)));
        let flat = flat_param(&ty, NO_FIELDS, &BTreeMap::new()).expect("раскладка есть");
        assert_eq!(
            flatten_argument("data", &flat),
            "{data[2], data[1], data[0]}"
        );
    }

    /// Плоский вектор шире элемента ровно во столько раз, сколько элементов.
    #[test]
    fn flat_width_is_size_times_element_width() {
        let ty = TypeNode::Array(3, Box::new(u(8)));
        let flat = flat_param(&ty, NO_FIELDS, &BTreeMap::new()).expect("раскладка есть");
        assert_eq!(flat.width, 24);
        assert_eq!(
            flat.parts
                .iter()
                .map(|(suffix, width, _)| (suffix.as_str(), *width))
                .collect::<Vec<_>>(),
            vec![("[0]", 8), ("[1]", 8), ("[2]", 8)]
        );
        // Бит-вектор передаётся как есть - он упакованный скаляр.
        let bits = TypeNode::Array(8, Box::new(TypeNode::Bit));
        assert!(flat_param(&bits, NO_FIELDS, &BTreeMap::new()).is_none());
    }

    /// Элемент-Структура остаётся одной частью: у цели `sv` она `struct packed`, и
    /// присваивание её целиком принимают оба инструмента, тогда как запись по полям
    /// даёт у yosys "Latch inferred".
    #[test]
    fn struct_element_stays_one_part() {
        let ty = TypeNode::Array(2, Box::new(TypeNode::Struct("Cell".to_string())));
        let fields = |name: &str| {
            (name == "Cell").then(|| vec![("lo".to_string(), u(8)), ("hi".to_string(), u(8))])
        };
        let flat = flat_param(&ty, &fields, &BTreeMap::new()).expect("раскладка есть");
        assert_eq!(flat.width, 32);
        assert_eq!(
            flat.parts
                .iter()
                .map(|(suffix, width, _)| (suffix.as_str(), *width))
                .collect::<Vec<_>>(),
            vec![("[0]", 16), ("[1]", 16)]
        );
    }

    /// Вложенный массив раскладывается до элементов внутреннего: он распакован, и
    /// присвоить ему срез вектора нельзя.
    #[test]
    fn nested_array_is_split_down_to_inner_elements() {
        let inner = TypeNode::Array(2, Box::new(u(8)));
        let ty = TypeNode::Array(2, Box::new(inner));
        let flat = flat_param(&ty, NO_FIELDS, &BTreeMap::new()).expect("раскладка есть");
        assert_eq!(flat.width, 32);
        assert_eq!(
            flat.parts
                .iter()
                .map(|(suffix, _, _)| suffix.as_str())
                .collect::<Vec<_>>(),
            vec!["[0][0]", "[0][1]", "[1][0]", "[1][1]"]
        );
    }

    /// Ширина элемента-Перечисления берётся у `enum_width` - того же знания, которым
    /// печатается сам тип.
    #[test]
    fn enum_element_width_comes_from_enum_facts() {
        let ty = TypeNode::Array(2, Box::new(TypeNode::Enum("Mode".to_string())));
        let mut enums = BTreeMap::new();
        enums.insert(
            "Mode".to_string(),
            vec![("Idle".to_string(), 1), ("Work".to_string(), 2)],
        );
        let flat = flat_param(&ty, NO_FIELDS, &enums).expect("раскладка есть");
        assert_eq!(flat.width, 4);
        // Перечисление неизвестно - раскладки нет, поведение прежнее.
        assert!(flat_param(&ty, NO_FIELDS, &BTreeMap::new()).is_none());
    }

    #[test]
    fn index_width_covers_the_last_element() {
        assert_eq!(index_width_for(1), 1);
        assert_eq!(index_width_for(2), 1);
        assert_eq!(index_width_for(3), 2);
        assert_eq!(index_width_for(4), 2);
        assert_eq!(index_width_for(5), 3);
        assert_eq!(index_width_for(256), 8);
    }
}
