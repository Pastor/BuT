//! Сдвиг на величину, не меньшую ширины типа.
//!
//! # Что делается
//!
//! При **литеральной** величине сдвига, не меньшей ширины типа, печатается то же
//! значение, что даёт эталон, но выразимой формой:
//!
//! - беззнаковый тип - `0`: все разряды ушли;
//! - знаковый - сдвиг на `ширина − 1`: там остаётся только знак, то есть −1
//!   для отрицательного и 0 для неотрицательного.
//!
//! достроила правило до **обоих** направлений и **переменной**
//! величины. Довод "`checked_shr` в каждом выражении стоил бы дороже пользы"
//! опровергнут замером: цена умолчания - не лишняя инструкция, а **другой
//! автомат**. При `n = 8` и `a: u8` эталон даёт `0`, а порождённый Rust
//! **паникует** в отладке и даёт `200` в релизе (величина маскируется до
//! `n & 7 = 0`) - то есть прошивку собирают именно в том режиме, где значение
//! молча неверно. Сдвиг **влево** на литеральную величину при этом вовсе не
//! собирался: `rustc` отвечает "attempt to shift left by `8_i32`, which would
//! overflow" при **нулевом** коде возврата `taktc`.
//!
//! **Отрицательная величина сдвига под правило не подпадает**: эталон отвечает
//! `SIM-002` и останавливает прогон, то есть у записи нет верного значения вовсе.
//! Прошивка считает её молча - это описанное разделение обязанностей, а не расхождение.
//!
//! Здесь же живёт печать **целой степени**: у неё та же природа - операция языка,
//! которую целевой язык выражает не тем оператором, каким её записал автор.

use crate::diagnostics::Diagnostic;
use crate::generator::rust::rust_expr::{Scope, print_expression};
use crate::generator::shift_width::{self, Saturation};
use crate::semantic::ExpressionNode;
use crate::semantic::type_node::TypeNode;

pub(crate) use crate::generator::shift_width::Direction;

/// Печать сдвига, если результат может не помещаться в `<<`/`>>` языка Rust.
///
/// `Ok(None)` - обычный случай: печатает вызывающий обычным оператором.
///
/// # Что печатается
///
/// | Форма | Направление | Печать |
/// |---|---|---|
/// | литерал `>= ширины` | влево | `0` |
/// | литерал `>= ширины` | вправо, беззнаковый | `0` |
/// | литерал `>= ширины` | вправо, знаковый | `v >> (ширина − 1)` |
/// | переменная | влево | `v.checked_shl(n).unwrap_or(0)` |
/// | переменная | вправо, беззнаковый | `v.checked_shr(n).unwrap_or(0)` |
/// | переменная | вправо, знаковый | `v >> n.min(ширина − 1)` |
///
/// Знаковый сдвиг вправо выражен **`min`**, а не `checked_shr`: у него насыщение
/// зависит от самого значения (`v >> (ширина − 1)` есть знак), и `unwrap_or` напечатал
/// бы `v` **дважды** - а вычисление операнда в языке Takt бывает с эффектом (вызов
/// функции пишет в переменные модели).
pub(crate) fn guarded(
    direction: Direction,
    value: &ExpressionNode,
    amount: &ExpressionNode,
    scope: &Scope,
) -> Result<Option<String>, Diagnostic> {
    let Some(bits) = shift_width::width_of(value) else {
        return Ok(None);
    };
    // Порог у Rust - ширина самого типа: сдвиг на неё там ошибка компиляции (литерал)
    // либо паника (переменная). Признак общий с целью `c`, порог свой - см. шапку
    // `generator/shift_width.rs`.
    match shift_width::literal_saturation(direction, value, amount, bits) {
        Saturation::Zero => return Ok(Some(String::from("0"))),
        Saturation::SignOnly(by) => {
            let printed = print_expression(value, scope)?;
            return Ok(Some(format!("({printed} >> {by})")));
        }
        Saturation::AsIs => {}
    }
    if shift_width::literal(amount).is_some() {
        return Ok(None);
    }

    let signed = shift_width::signed_of(value);
    let printed = print_expression(value, scope)?;
    let shift = shift_amount(amount, scope)?;
    Ok(Some(match (direction, signed) {
        (Direction::Right, true) => {
            format!("({printed} >> ({shift}).min({}))", u32::from(bits) - 1)
        }
        (Direction::Left, _) => format!("({printed}).checked_shl({shift}).unwrap_or(0)"),
        (Direction::Right, false) => format!("({printed}).checked_shr({shift}).unwrap_or(0)"),
    }))
}

/// Величина сдвига как `u32` - тип, которого требуют `checked_sh*`.
///
/// Приведение печатается **по нужде**: у величины, уже имеющей тип `u32`, `x as u32` -
/// это `clippy::unnecessary_cast`, то есть **отказ** сборки под `-D warnings` (тот же
/// класс, что ).
fn shift_amount(amount: &ExpressionNode, scope: &Scope) -> Result<String, Diagnostic> {
    let printed = print_expression(amount, scope)?;
    if matches!(
        shift_width::type_of(amount),
        Some(TypeNode::Integer {
            bits: 32,
            signed: false
        })
    ) {
        return Ok(printed);
    }
    Ok(format!("({printed}) as u32"))
}

/// Целая степень - `wrapping_pow`.
///
/// # Операнды
///
/// `target` - тип приёмника, если он известен (`rust_coerce`). Он нужен базе: вывод
/// типов Rust не идёт снаружи внутрь через вызов метода, и у литерала в этой позиции
/// типа нет - `(2).wrapping_pow(8)` есть **`E0689`**.
///
/// # Ошибки
///
/// - `RS-011` - показатель отрицателен: у целой степени его быть не может, а
///   `wrapping_pow` принимает `u32`.
/// - `RS-011` - база-литерал в позиции без известного приёмника: печать дала бы
///   `E0689`, то есть невалидный вывод при нулевом коде возврата `taktc`.
///   Громкий отказ здесь дешевле молчания.
pub(crate) fn power(
    base: &ExpressionNode,
    exp: &ExpressionNode,
    scope: &Scope,
    target: Option<&TypeNode>,
) -> Result<String, Diagnostic> {
    if let Some(value) = shift_width::literal(exp)
        && value < 0
    {
        return Err(crate::generator::rust::rust_expr::unsupported(
            "возведение в ОТРИЦАТЕЛЬНУЮ степень: результат дробный, а целая \
             степень в Rust принимает беззнаковый показатель",
        ));
    }
    Ok(format!(
        "({}).wrapping_pow({})",
        power_base(base, scope, target)?,
        power_exponent(exp, scope)?
    ))
}

/// База степени: литералу приписывается тип приёмника суффиксом.
///
/// Суффикс, а не приведение: `2 as u32` - это `clippy::unnecessary_cast`, то есть отказ
/// сборки под `-D warnings`. Скобки вокруг базы ставит вызывающий - без них унарный
/// минус применился бы после метода (`-2i32.wrapping_pow(2)` = −4 вместо 4).
fn power_base(
    base: &ExpressionNode,
    scope: &Scope,
    target: Option<&TypeNode>,
) -> Result<String, Diagnostic> {
    let printed = print_expression(base, scope)?;
    // У базы уже есть свой тип - приписывать нечего.
    if shift_width::type_of(base).is_some() {
        return Ok(printed);
    }
    let Some(literal) = shift_width::literal(base) else {
        // Не литерал и без своего типа - выражение, чей тип печатнику неизвестен; Rust
        // выведет его из места вызова сам.
        return Ok(printed);
    };
    match target {
        Some(ty @ TypeNode::Integer { .. }) => {
            let name = crate::generator::rust::rust_type::rust_type(ty, "приёмник степени")?;
            Ok(format!("{literal}{name}"))
        }
        _ => Err(crate::generator::rust::rust_expr::unsupported(
            "степень с ЛИТЕРАЛЬНОЙ базой в позиции, где тип приёмника \
             неизвестен: в Rust вывод типа не проходит сквозь вызов метода, и \
             такая запись не компилируется (E0689)",
        )),
    }
}

/// Показатель как `u32` - приведение печатается по нужде.
///
/// Три формы, и две из них приведения не терпят: у литерала тип выводится сам
/// (`x.wrapping_pow(8)`), а `u32 as u32` - это `clippy::unnecessary_cast`.
fn power_exponent(exp: &ExpressionNode, scope: &Scope) -> Result<String, Diagnostic> {
    let printed = print_expression(exp, scope)?;
    if shift_width::literal(exp).is_some() {
        return Ok(printed);
    }
    if matches!(
        shift_width::type_of(exp),
        Some(TypeNode::Integer {
            bits: 32,
            signed: false
        })
    ) {
        return Ok(printed);
    }
    Ok(format!("({printed}) as u32"))
}
