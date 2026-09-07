//! Битовый доступ цели `rust`: чтение `x.N` и запись `x.N := v`.

use crate::diagnostics::{Diagnostic, Location};
use crate::generator::rust::rust_expr::{
    Scope, coerce_to, print_as_bool, print_expression, unsupported, unwrap_outer, write_port,
};
use crate::parser::ast::Member;
use crate::semantic::type_node::TypeNode;
use crate::semantic::{ExpressionNode, PortDirection, VariableNode};

/// Число слов носителя, если бит-вектор представлен массивом слов.
///
/// `None` - либо не бит-вектор, либо `N <= 64` (скаляр): печать прежняя.
pub(crate) fn words_of_type(ty: &TypeNode) -> Option<u16> {
    use crate::semantic::bit_vector::{self, BitVectorLayout};
    let n = bit_vector::is_bit_vector(ty)?;
    match bit_vector::layout(n) {
        BitVectorLayout::Words { count } => Some(count),
        BitVectorLayout::Scalar { .. } => None,
    }
}

/// Число слов носителя выражения - по объявлению переменной.
///
/// Тип берётся из ячейки `ExpressionNode::Variable` - снимка, снятого при разрешении
/// имени. Для объявленного типа он верен; при `Inference` печать остаётся скалярной, а
/// не становится отказом.
pub(crate) fn words_of(expr: &ExpressionNode) -> Option<u16> {
    let ExpressionNode::Variable(var_rc) = expr else {
        return None;
    };
    let var = var_rc.borrow();
    let (VariableNode::Simple { ty, .. }
    | VariableNode::Const { ty, .. }
    | VariableNode::Port { ty, .. }) = &*var
    else {
        return None;
    };
    words_of_type(ty)
}

/// Литерал массива слов: значение достаётся младшему слову, прочие - нули.
pub(crate) fn word_literal(value: i128, count: u16) -> String {
    if value == 0 {
        return format!("[0u64; {count}]");
    }
    let mut words: Vec<String> = vec![format!("{value}u64")];
    words.extend((1..count).map(|_| "0u64".to_string()));
    format!("[{}]", words.join(", "))
}

/// Носитель разряда: сам скаляр либо своё слово массива.
///
/// Позиция берётся у `bit_vector::bit_slot` - общего носителя с эталоном
/// (`eval/access.rs`) и целью `c`. `None` означает разряд за пределом вектора: печатать
/// доступ за границу массива нельзя, это паника в прошивке.
fn carrier_word(base: &str, words: Option<u16>, bit: u64) -> Option<String> {
    let Some(count) = words else {
        return Some(base.to_string());
    };
    let (w, _) = crate::semantic::bit_vector::bit_slot(u32::try_from(bit).unwrap_or(u32::MAX));
    if w >= count {
        return None;
    }
    Some(format!("{base}[{w}]"))
}

/// Смещение разряда внутри носителя: сам номер у скаляра, остаток у слова.
fn carrier_offset(words: Option<u16>, bit: u64) -> u64 {
    if words.is_none() {
        return bit;
    }
    let (_, off) = crate::semantic::bit_vector::bit_slot(u32::try_from(bit).unwrap_or(u32::MAX));
    u64::from(off)
}

/// Печатает битовый доступ `x.N` как маску.
///
/// В MatIEC битового доступа нет вовсе (ни `x.0`, ни `%X0`), и цель `st` разворачивает
/// его в маску по нужде. Здесь маска - тоже единственная форма, но по другой причине: у
/// чисел Rust битового синтаксиса просто нет.
pub(crate) fn bit_access(
    inner: &ExpressionNode,
    member: &Member,
    scope: &Scope,
) -> Result<String, Diagnostic> {
    let base = print_expression(inner, scope)?;
    // Доступ к полю структуры: `g.kp` - обычное поле, а не разряд.
    if let Member::Identifier(name) = member {
        return Ok(field_access(&base, &name.name));
    }
    let bit = member_index(member)?;
    // Носитель может быть массивом слов (`[bit;N > 64]`): сдвигается своё слово.
    let words = words_of(inner);
    let Some(carrier) = carrier_word(&base, words, bit) else {
        // Разряд за объявленной шириной: читается ноль. Доступ за границу массива в
        // Rust - паника, а не "случайный бит".
        return Ok("false".to_string());
    };
    Ok(bit_mask(&carrier, carrier_offset(words, bit)))
}

/// Строит маску битового доступа `x.N`.
///
/// Узел заключается в скобки целиком - как и любой бинарный (см. Без внешних скобок
/// `x.1 | flag` дало бы `(x >> 1) & 1 != 0 | flag`, что в Rust читается как `... != (0 |
/// flag)`: `|` сильнее `!=`.
///
/// Сдвиг на 0 не эмитится: `x >> 0` - операция без эффекта (`clippy::identity_op`), то
/// есть отказ проверки. Нулевой бит в корпусе обычен (`SENSORS_CAB.0`), поэтому случай не
/// теоретический.
pub(crate) fn bit_mask(base: &str, bit: u64) -> String {
    if bit == 0 {
        return format!("(({} & 1) != 0)", base);
    }
    format!("((({} >> {}) & 1) != 0)", base, bit)
}

/// Печатает доступ к полю структуры: `<база>.<поле>`.
///
/// Имя поля нормируется тем же правилом, что и объявление в `rust_decl`: разъехавшись,
/// они дали бы обращение к несуществующему полю.
pub(crate) fn field_access(base: &str, field: &str) -> String {
    format!(
        "{base}.{}",
        crate::semantic::naming::normalize_lowercase_snakecase(field.to_string())
    )
}

/// Извлекает номер бита из члена `x.N`.
///
/// Доступ по имени (`x.field`) битовым не является: это обращение к полю структуры, и
/// молча выдать за него маску значило бы породить тихо неверный код.
pub(crate) fn member_index(member: &Member) -> Result<u64, Diagnostic> {
    match member {
        Member::Number(index) if *index >= 0 => Ok(*index as u64),
        Member::Number(index) => Err(unsupported(&format!(
            "битовый доступ с отрицательным индексом '{}'",
            index
        ))),
        Member::Identifier(name) => Err(unsupported(&format!(
            "доступ к члену '.{}': поля структур в цели rust пока не транслируются",
            name.name
        ))),
    }
}

/// Печатает запись одного разряда `x.N := v`.
///
/// ## Правило
///
/// В разряд кладётся **младший бит значения** - то же, что печатают цели `c` (`(rhs &
/// 1u) << N`) и `sv` (присваивание разряду есть усечение). Литералы `1` и `0` дают
/// идиоматичные `|=` и `&=`; прочее - развилку по значению, приведённому к `bool`.
///
/// **`1 << 0` не эмитится** - но по другой причине, чем у чтения, и это
/// **замер, а не перенос довода**. Чтение не печатает `x >> 0`, потому что это
/// `clippy::identity_op` (проверено: `((self.b >> 0) & 1) != 0` валит
/// `clippy -D warnings`). Сдвиг **литерала** - `1 << 0` - тот же линт
/// пропускает (проверено там же), так что проверка здесь ни при чём. Форма всё
/// равно короткая: маска нулевого разряда есть просто `1`, и печатать вместо
/// неё сдвиг значило бы засорять вывод ради единообразия с самим собой.
pub(crate) fn assign_bit(
    inner: &ExpressionNode,
    bit: i128,
    value: &ExpressionNode,
    scope: &Scope,
) -> Result<String, Diagnostic> {
    if let ExpressionNode::Variable(var) = inner {
        let borrowed = var.borrow();
        if let VariableNode::Port {
            name,
            ty,
            direction,
            loc,
            ..
        } = &*borrowed
        {
            return assign_port_bit(name, ty, *direction, bit, value, scope, *loc);
        }
    }
    let base = print_expression(inner, scope)?;
    let words = words_of(inner);
    let bit_u64 = u64::try_from(bit).unwrap_or(u64::MAX);
    let Some(carrier) = carrier_word(&base, words, bit_u64) else {
        return Err(unsupported(&format!(
            "разряд {bit} за пределом бит-вектора: разрядов за объявленной шириной \
             нет, а доступ за границу массива слов — паника в прошивке"
        )));
    };
    let (bare, grouped) = bit_masks(i128::from(carrier_offset(words, bit_u64)));
    match value {
        ExpressionNode::Number(1) => Ok(format!("{carrier} |= {bare}")),
        ExpressionNode::Number(0) => Ok(format!("{carrier} &= !{grouped}")),
        other => {
            let cond = print_as_bool(other, scope)?;
            Ok(format!(
                "{carrier} = if {} {{ {carrier} | {grouped} }} \
                 else {{ {carrier} & !{grouped} }}",
                unwrap_outer(&cond)
            ))
        }
    }
}

/// Две формы маски разряда: голая и в скобках.
///
/// Голая идёт правой частью составного присваивания (`x |= 1 << 2`) - там скобки
/// лишние, и `rustc` говорит об этом `unused_parens`, то есть под `-D warnings` это
/// отказ проверки. В скобках - везде, где маска операнд: `!(1 << 2)` без них разобралось
/// бы как `(!1) << 2`.
///
/// При `N = 0` сдвига нет вовсе: маска нулевого разряда есть просто `1`.
fn bit_masks(bit: i128) -> (String, String) {
    if bit == 0 {
        ("1".to_string(), "1".to_string())
    } else {
        (format!("1 << {bit}"), format!("(1 << {bit})"))
    }
}

/// Запись разряда порта.
///
/// У однобитного порта разряд ровно один, и запись в него есть запись самого
/// порта - чтения не требуется. У числового выходного порта разряд без чтения
/// не установить, а HAL-трейт даёт выходу **только** запись (в отличие от цели
/// `c`, где `read_numeric` есть у любого порта). Отсюда честный отказ
/// **`RS-025`** с названным обходом.
fn assign_port_bit(
    name: &str,
    ty: &TypeNode,
    direction: PortDirection,
    bit: i128,
    value: &ExpressionNode,
    scope: &Scope,
    loc: Location,
) -> Result<String, Diagnostic> {
    if matches!(ty, TypeNode::Bit | TypeNode::Bool) {
        if bit != 0 {
            return Err(Diagnostic::error(
                loc,
                format!(
                    "разряд {bit} у однобитного порта '{name}': у значения шириной в бит \
                     иных разрядов нет"
                ),
            )
            .with_code("RS-025"));
        }
        let printed = coerce_to(value, ty, scope)?;
        return write_port(name, ty, direction, unwrap_outer(&printed), scope, loc);
    }
    Err(Diagnostic::error(
        loc,
        format!(
            "запись разряда {bit} порта '{name}' не транслируется в Rust: установка \
             одного разряда требует прочитать остальные, а HAL-трейт даёт выходному \
             порту только запись. Держите значение в переменной модели и пишите порт \
             целиком ('var shadow: …; shadow.{bit} := …; {name} := shadow;')"
        ),
    )
    .with_code("RS-025"))
}
