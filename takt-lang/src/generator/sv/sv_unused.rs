//! Заглушка неиспользуемого параметра функции - цель `sv`.
//!
//! `fn constant(v: u8) -> u8 { return 7; }` даёт `UNUSEDSIGNAL` ("Function variable is
//! not used"), а проверка цели считает предупреждение **ошибкой** - то есть порождённый
//! модуль проверки не прошёл бы, при **нулевом** коде возврата `taktc`.
//!
//! Форма выбрана **пробой обоих инструментов** (2026-08-20): локальная переменная,
//! поглощающая параметр редукцией.
//!
//! ```systemverilog
//! logic _unused_v;
//! _unused_v = &{1'b0, v};
//! ```
//!
//! **`lint_off` запрещён правилом проекта**: прагма гасит теста, а не причину. Здесь
//! параметр честно **используется** - и синтезатор выбрасывает эту логику сам
//! (константа `1'b0` в редукции даёт ноль).
//!
//! **Параметр бывает использован частично**: индекс печатается сужением по размеру
//! массива (`a[2'(i)]`), и старшие разряды не читает никто - verilator отвечает тем же
//! `UNUSEDSIGNAL`, но уже про биты. Тот случай судит признак по дереву
//! (`sv_stmt::index_only_variables`): объявление поглотителя обязано встать до
//! операторов, то есть до того, как тело напечатано.
//!
//! Приём не новый: тем же способом обёртка APB поглощает сигналы записи
//! (`_unused_write`).

use crate::generator::indent::Printer;

/// Печатается ли заглушка для параметра `param`.
///
/// `body` - уже напечатанное тело функции.
pub(in crate::generator::sv) fn is_unused(body: &str, param: &str) -> bool {
    !body.lines().any(|line| mentions(line, param))
}

/// Печатает объявление и присваивание заглушки.
pub(in crate::generator::sv) fn emit_guard(p: &mut Printer, param: &str) {
    p.ident(&format!("logic _unused_{param};")).nl();
    p.ident(&format!("_unused_{param} = &{{1'b0, {param}}};"))
        .nl();
}

/// Встречается ли `ident` в строке **как отдельный идентификатор**.
fn mentions(line: &str, ident: &str) -> bool {
    let bytes = line.as_bytes();
    let mut from = 0;
    while let Some(pos) = line[from..].find(ident) {
        let start = from + pos;
        let end = start + ident.len();
        let before_ok = start == 0 || !is_ident_byte(bytes[start - 1]);
        let after_ok = end == bytes.len() || !is_ident_byte(bytes[end]);
        if before_ok && after_ok {
            return true;
        }
        from = end;
    }
    false
}

/// Байт, который может входить в идентификатор SystemVerilog.
fn is_ident_byte(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_' || b == b'$'
}
