//! Умолчание переменной у цели `c`.

use crate::diagnostics::{Diagnostic, Location};
use crate::generator::indent::Printer;
use crate::semantic::ModelNode;
use crate::semantic::type_node::TypeNode;

/// Печатает нулевое значение переменной `field` типа `ty` в `_init`.
///
/// Форма зависит от типа, потому что в C **не всё присваивается**: массив не
/// присваивается вовсе, структура - не поэлементно, а целиком только из другой
/// структуры. Поэтому раскладка повторяет ту, которой уже пользуются печатники
/// инициализаторов: массив - по элементам, бит-вектор шире 64 бит - по словам,
/// структура - по полям, рекурсивно.
///
/// # Ошибки
/// [`Diagnostic`], если структура типа не объявлена: печатать нечего, а
/// молчаливый пропуск вернул бы исходный дефект.
pub(super) fn emit_zero_init(
    printer: &mut Printer,
    field: &str,
    ty: &TypeNode,
    model: &ModelNode,
) -> Result<(), Diagnostic> {
    // Бит-вектор шире 64 бит - массив слов: проверяется раньше массива, иначе он ушёл
    // бы в общую ветвь и получил ⌈N/64⌉ != N элементов.
    if let Some(count) = crate::generator::c::c_bits::words_of_type(ty) {
        for i in 0..count {
            printer.ident(&format!("model->{field}[{i}] = 0;")).nl();
        }
        return Ok(());
    }
    // Бит-вектор `[bit; N <= 64]` - Скаляр, и обнуляется он одним присваиванием.
    if crate::semantic::bit_vector::is_bit_vector(ty).is_some() {
        printer.ident(&format!("model->{field} = 0;")).nl();
        return Ok(());
    }
    match ty {
        TypeNode::Array(size, elem) => {
            for i in 0..*size {
                emit_zero_init(printer, &format!("{field}[{i}]"), elem, model)?;
            }
            Ok(())
        }
        TypeNode::Struct(name) => {
            let def = model.search_struct(name).ok_or_else(|| {
                Diagnostic::error(
                    Location::Codegen,
                    format!(
                        "структура '{name}' не объявлена: умолчание поля '{field}' не строится"
                    ),
                )
                .with_code("CC-023")
            })?;
            for (sub, sub_ty) in &def.fields {
                emit_zero_init(printer, &format!("{field}.{sub}"), sub_ty, model)?;
            }
            Ok(())
        }
        // Перечисление - первый по тексту вариант. Значение печатается именованной
        // константой: голое число разошлось бы с формой, которой цель печатает варианты
        // в теле.
        TypeNode::Enum(name) => {
            // Имя константы строится той же функцией, что и объявление `#define`, а
            // владелец берётся у самого узла: перечисление могло быть унаследовано от
            // родителя.
            let named = model.search_enum(name).and_then(|def| {
                let (variant, _) = crate::semantic::enum_default(&def.variants)?;
                let owner = def.upper.as_ref().and_then(|w| w.upgrade())?;
                Some(crate::generator::c::c_names::enum_constant(
                    &crate::semantic::minimap::Name::from(owner),
                    name,
                    &variant,
                ))
            });
            // Перечисления без вариантов не бывает (`SE-105`), и владелец у узла есть
            // всегда: ветвь `None` защитная, и ноль в ней - умолчание.
            let value = named.unwrap_or_else(|| "0".to_string());
            printer.ident(&format!("model->{field} = {value};")).nl();
            Ok(())
        }
        // Скаляр - включая `duration` (целое мс) и `q(m, n)` (целый код): нулевой код
        // означает ноль величины у обоих.
        _ => {
            printer.ident(&format!("model->{field} = 0;")).nl();
            Ok(())
        }
    }
}
