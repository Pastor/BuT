//! Локальные переменные тел, содержащие СТРУКТУРУ, у цели `sv` (фича 0373).
//!
//! # Зачем отдельное правило
//!
//! Локальные переменные тел цель объявляет `automatic` прямо в ветви `case`
//! (фича 0304). Для переменной, содержащей структуру, этого мало: тело пишет её
//! поля, а внутри ветви yosys полным присваиванием такую запись не считает —
//!
//! ```text
//! ERROR: Latch inferred for signal `\m.$unnamed_block$1.tmp.lo'
//!        from always_comb process ...
//! ```
//!
//! — при том что `verilator --lint-only -Wall` модуль **принимает**, а `taktc`
//! возвращает ноль. Класс видит **один инструмент из трёх**.
//!
//! # Форма и как она выбрана
//!
//! Прогон обоих инструментов 2026-08-21 (по **коду возврата**: yosys печатает
//! `путь:строка: ERROR: …`, и греп `'^ERROR'` объявляет годными негодные
//! формы):
//!
//! | Форма | verilator | yosys |
//! |---|---|---|
//! | `automatic pair_t tmp;` в ветви (было) | принял | отверг |
//! | `automatic pair_t tmp = '0;` в ветви | принял | отверг |
//! | `begin : ярлык` вокруг блока | принял | отверг на массиве структур |
//! | объявление на уровне модуля без умолчаний | принял | отверг |
//! | **`automatic` в начале `always_comb` + умолчания** | принял | **принял** |
//!
//! ⚠️ **Умолчаний ДВА — целиком и по листьям.** Тело вправе писать структуру
//! обоими способами (`whole = make(n);` против `field.lo = …`), а синтезатор
//! считает незаданной ту форму, которой присваивает тело. Значение одно и то
//! же, поэтому лишнее присваивание нуля смысла не меняет.
//!
//! ⚠️ **Список заполняет сам печатник тел** (`sv_blocks`), а не отдельный
//! обход: второй список того же набора разошёлся бы с первым (класс
//! 0084/0193/0195). Отсюда `RefCell` — тела печатаются по `&Fsm`.

use std::cell::RefCell;
use std::collections::BTreeMap;

use crate::diagnostics::Diagnostic;
use crate::generator::indent::Printer;
use crate::semantic::type_node::TypeNode;

use super::sv_expr::sv002;
use super::sv_type::sv_type;

/// Накопитель поднятых локальных: заполняется печатником, читается эмиссией.
pub(crate) type HoistedLocals = RefCell<Vec<HoistedLocal>>;

/// Локальная переменная тела, поднятая в начало `always_comb`.
pub(crate) struct HoistedLocal {
    /// Имя — то же, что в теле: второго знания об адресации не заводится.
    pub(crate) name: String,
    /// Объявление типа, уже отображённое в SystemVerilog.
    pub(crate) decl: String,
    /// Нулевые умолчания: `(суффикс пути, значение)`; пустой суффикс — целиком.
    pub(crate) defaults: Vec<(String, String)>,
}

/// Поднимает локальную переменную со структурой.
///
/// Дедупликация — по имени: одно и то же тело печатается в нескольких местах
/// (`exit` — у каждого перехода), да и разные блоки вправе назвать временную
/// одинаково. Подъём это допускает, потому что умолчания печатаются
/// **безусловно** в начале процесса: каждый блок видит нули, как и предписывает
/// объявление в теле.
///
/// # Ошибки
///
/// [`SV-002`](sv002), если одно имя объявлено с **разными** типами:
/// молча склеив их, цель подменила бы тип временной. Обход — переименование в
/// исходнике, и текст отказа его называет.
pub(crate) fn hoist(
    hoisted: &HoistedLocals,
    structs: &BTreeMap<String, Vec<(String, TypeNode)>>,
    name: &str,
    ty: &TypeNode,
) -> Result<(), Diagnostic> {
    let decl = sv_type(ty, &format!("локальная переменная '{}'", name))?
        .declare(name)
        .to_string();
    if let Some(seen) = hoisted.borrow().iter().find(|l| l.name == name) {
        if seen.decl == decl {
            return Ok(());
        }
        return Err(sv002(&format!(
            "локальная переменная '{name}' объявлена с разными типами ('{}' и \
             '{}'): переменная со структурой поднимается в начало always_comb, \
             и одно имя не может нести два типа. Переименуйте одну из них",
            seen.decl, decl
        )));
    }
    let fields_of = |sname: &str| structs.get(sname).cloned();
    let defaults = super::sv_array::leaf_zero_defaults(ty, &fields_of);
    hoisted.borrow_mut().push(HoistedLocal {
        name: name.to_string(),
        decl,
        defaults,
    });
    Ok(())
}

/// Печатает объявления поднятых — ПЕРВЫМИ в процессе.
///
/// В SystemVerilog объявления обязаны предшествовать операторам блока: иначе
/// verilator отвечает `syntax error, unexpected automatic`.
pub(crate) fn emit_declarations(p: &mut Printer, hoisted: &HoistedLocals) {
    let hoisted = hoisted.borrow();
    if hoisted.is_empty() {
        return;
    }
    for local in hoisted.iter() {
        p.ident(&format!("automatic {};", local.decl)).nl();
        // Объявление поглотителя (фича 0375) — здесь же: объявления обязаны
        // предшествовать операторам блока.
        p.ident(&format!("logic _unused_{};", local.name)).nl();
    }
}

/// Печатает нулевые умолчания поднятых — рядом с умолчаниями регистров.
///
/// ⚠️ **Следом печатается ПОГЛОТИТЕЛЬ** (фича 0375): поле структуры, которое
/// тело пишет, но не читает, verilator объявляет неиспользованным
/// (`%Warning-UNUSEDSIGNAL: Bits of signal are not used: 'tmp'[15:8]`), а гейт
/// цели считает предупреждение ошибкой — при нулевом коде возврата `taktc` и
/// при том, что эталон, `c`, `st` и `rust` тот же вход исполняют. Идиома та
/// же, что у неиспользуемого параметра (0337) и у сигналов записи обёртки APB
/// (0169): редукция с константой, которую синтезатор выбрасывает сам.
///
/// ⚠️ Поглотитель печатается **безусловно**, и это осознанно: «прочитано ли
/// поле» по напечатанному тексту не спросишь — запись `tmp.hi = …` упоминает
/// поле ровно так же, как чтение. Признак, ошибающийся в сторону «поглотитель
/// не нужен», давал бы отказ гейта у пользователя, а корпус такого входа не
/// содержит и не поймал бы его. Цена — две строки на переменную, которые
/// синтезатор удаляет.
pub(crate) fn emit_defaults(p: &mut Printer, hoisted: &HoistedLocals) {
    for local in hoisted.borrow().iter() {
        for (suffix, zero) in &local.defaults {
            p.ident(&format!("{}{} = {};", local.name, suffix, zero))
                .nl();
        }
        // ⚠️ Операнды редукции — то, что в SystemVerilog можно склеить:
        // упакованное значение целиком, а РАСПАКОВАННЫЙ массив — поэлементно.
        // `&{1'b0, tmp}` над `tmp [0:1]` yosys встречает «Invalid array
        // access» (прогон 2026-08-21).
        let packed = local.defaults.iter().any(|(suffix, _)| suffix.is_empty());
        let operands: Vec<String> = if packed {
            vec![local.name.clone()]
        } else {
            local
                .defaults
                .iter()
                .map(|(suffix, _)| format!("{}{}", local.name, suffix))
                .collect()
        };
        p.ident(&format!(
            "_unused_{} = &{{1'b0, {}}};",
            local.name,
            operands.join(", ")
        ))
        .nl();
    }
}
