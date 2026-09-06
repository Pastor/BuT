//! Перечисления состояний и шагов у цели `sv` (вынесено ).
//!
//! Печать `typedef enum` для состояний каждого уровня и для ступеней последовательной
//! композиции. Вынесено из `sv_fsm.rs` по правилу размера модуля: границы модулей -
//! границы ответственности, и печать перечислений от сборки автомата не зависит.

use crate::diagnostics::Diagnostic;
use crate::generator::indent::Printer;
use crate::generator::sv::sv_fsm::{Block, Fsm, state_enum_name, state_variants};
use crate::generator::sv::sv_map::SvMap;
use crate::generator::sv::sv_names::{self, step_enum_name, step_variant};
use crate::generator::sv::sv_type::enum_width;
use crate::semantic::minimap::Element;
use std::collections::BTreeSet;

/// Печатает перечисления состояний всех уровней.
pub(crate) fn emit_state_enums(
    p: &mut Printer,
    map: &SvMap,
    blocks: &[Block],
) -> Result<(), Diagnostic> {
    for (name, _) in blocks {
        let Some(Element::Model { states, .. }) = map.model_element_of(name) else {
            continue;
        };
        // Алфавит имени состояния: перечислитель печатается здесь, и без этой проверки
        // не-ASCII имя доехало бы до `verilator`. Дыру нашёл тест по видам объявлений,
        // а не чтение: у переменных, портов и функций проверка была, у состояний - нет.
        sv_names::check_state_names(map, name)?;
        let variants = state_variants(name, &states);
        // Ширина - по диапазону значений. Значения назначает генератор (0..n-1),
        // поэтому формула вырождается в ⌈log₂(n)⌉ - то есть совпадает с формулой именно
        // здесь, где та была верна.
        let numbered: Vec<(String, i128)> = variants
            .iter()
            .enumerate()
            .map(|(i, v)| (v.clone(), i as i128))
            .collect();
        let (width, _) = enum_width(&numbered, &format!("состояния модели '{}'", name))?;
        p.ident(&format!("typedef enum logic [{}:0] {{", width - 1))
            .nl();
        p.up();
        for (i, (variant, value)) in numbered.iter().enumerate() {
            let comma = if i + 1 == numbered.len() { "" } else { "," };
            p.ident(&format!("{} = {}'d{}{}", variant, width, value, comma))
                .nl();
        }
        p.down();
        p.ident(&format!("}} {};", state_enum_name(name))).nl().nl();
    }
    Ok(())
}

/// Печатает перечисления шага для цепочек `+`.
///
/// Значения назначает генератор (0..n-1), поэтому ширина - ⌈log₂(n)⌉, как у
/// перечислений состояний. Порядок - обхода `Fsm::build` (детерминизм 0048).
pub(crate) fn emit_step_enums(p: &mut Printer, fsm: &Fsm) -> Result<(), Diagnostic> {
    for chain in &fsm.step_enums {
        let state = &chain.state;
        let mut numbered: Vec<(String, i128)> = (0..chain.count)
            .map(|i| (step_variant(state, &chain.path, i), i as i128))
            .collect();
        // Терминальный вариант - только у вложенной цепочки: у цепочки верхнего уровня
        // последний шаг уводит родительское состояние, и вариант остался бы
        // недостижимым.
        if chain.done {
            numbered.push((
                sv_names::step_done_variant(state, &chain.path),
                chain.count as i128,
            ));
        }
        let (width, _) = enum_width(&numbered, &format!("шаг цепочки '{}'", state))?;
        p.ident(&format!("typedef enum logic [{}:0] {{", width - 1))
            .nl();
        p.up();
        for (i, (variant, value)) in numbered.iter().enumerate() {
            let comma = if i + 1 == numbered.len() { "" } else { "," };
            p.ident(&format!("{} = {}'d{}{}", variant, width, value, comma))
                .nl();
        }
        p.down();
        p.ident(&format!("}} {};", step_enum_name(state, &chain.path)))
            .nl()
            .nl();
    }
    Ok(())
}

/// Печатает пользовательские перечисления модели.
///
/// Переехало сюда из `sv_fsm` по границе ответственности: рядом уже живут перечисления
/// состояний и шагов, а FSM отвечает за автомат.
pub(crate) fn emit_enums(p: &mut Printer, blocks: &[Block]) -> Result<(), Diagnostic> {
    let mut seen: BTreeSet<String> = BTreeSet::new();
    for (_, model_rc) in blocks {
        let model = model_rc.borrow();
        for def in model.enums.values() {
            if !seen.insert(def.name.clone()) {
                continue;
            }
            // Ширина - по диапазону значений. Формула (по числу вариантов) на `Idle =
            // 670` дала бы `logic [0:0]` и `%Error-ENUMITEMWIDTH`.
            let (width, signed) =
                enum_width(&def.variants, &format!("перечисление '{}'", def.name))?;
            let sign = if signed { "signed " } else { "" };
            p.ident(&format!("typedef enum logic {}[{}:0] {{", sign, width - 1))
                .nl();
            p.up();
            for (i, (variant, value)) in def.variants.iter().enumerate() {
                let comma = if i + 1 == def.variants.len() { "" } else { "," };
                // Отрицательное значение печатается `-W'sdN`, а не `W'd-5`: последнее
                // синтаксически неверно - `verilator` отвечает "Number is missing value
                // digits", причём при нулевом коде возврата `taktc`, тогда как эталон и
                // остальные семь целей вход исполняют. Знак в объявлении был учтён
                // (`logic signed`), а в значении - нет.
                let literal = if *value < 0 {
                    format!("-{width}'sd{}", value.unsigned_abs())
                } else {
                    format!("{width}'d{value}")
                };
                p.ident(&format!(
                    "{} = {}{}",
                    crate::generator::sv::sv_names::sv_enum_variant_name(&def.name, variant),
                    literal,
                    comma
                ))
                .nl();
            }
            p.down();
            p.ident(&format!(
                "}} {};",
                crate::generator::sv::sv_type::sv_enum_type_name(&def.name)
            ))
            .nl()
            .nl();
        }
    }
    Ok(())
}
