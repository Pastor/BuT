//! Единая точка сбора предупреждений компилятора.
//!
//! Предупреждение доезжает до пользователя всеми целями `taktc`, только если добавлено
//! в [`collect_model_warnings`]: диагностика, которую никто не печатает, равносильна её
//! отсутствию.
//!
//! Адресные предупреждения (`address_expr_warnings`, `address_map_overlay_warnings`)
//! сюда не входят: они зависят от цели - у потребляющих адрес `c-hal` и `st-at` те же
//! ситуации дают ошибки, - и собираются у вызывающего отдельно.

use crate::diagnostics::Diagnostic;
use crate::parser::ast;
use crate::semantic::ModelNode;
use std::cell::RefCell;
use std::rc::Rc;

/// Собирает **все** предупреждения над построенной моделью.
///
/// Порядок вызова - фиксированный (детерминизм вывода); входы смешаны намеренно:
/// большинство проверок берут семантическую модель, `stray_semicolon` и
/// `unknown_named_block` - АСД.
pub fn collect_model_warnings(ast: &ast::Model, model: &Rc<RefCell<ModelNode>>) -> Vec<Diagnostic> {
    let mut warnings = Vec::new();
    warnings.extend(crate::unused_variable_warnings(Rc::clone(model)));
    warnings.extend(crate::nondeterministic_transition_warnings(Rc::clone(
        model,
    )));
    // SE-037 "неявная булевость". Проверка существовала с Ce11, была покрыта
    // юнит-тестами - и никуда не подключена: ни `taktc compile`, ни редактор её не
    // печатали, то есть она считалась и выбрасывалась. Включена после того, как замер
    // снял с неё 51 ложное срабатывание на законных записях корпуса.
    warnings.extend(crate::semantic::tree::implicit_bool_warnings(model));
    warnings.extend(crate::unreachable_state_warnings(Rc::clone(model)));
    // SE-116: ребро после безусловного недостижимо. Ce14 этот класс не видит - она ищет
    // Несколько безусловных рёбер, а здесь безусловное одно.
    warnings.extend(crate::semantic::validate::check_unreachable_edges(
        Rc::clone(model),
    ));
    // SE-131: ветвь `match` с повторяющимся образцом недостижима - `match` берёт первое
    // совпадение. Целям `c` и `rust` такой вход давал невалидный вывод, автору не
    // говорил никто.
    warnings.extend(crate::semantic::validate::check_duplicate_match_arms(
        Rc::clone(model),
    ));
    warnings.extend(crate::constant_condition_warnings(model));
    warnings.extend(crate::ltl_warnings(Rc::clone(model)));
    warnings.extend(crate::stray_semicolon_warnings(ast));
    warnings.extend(crate::unknown_named_block_warnings(ast));
    // SE-096: запись по анонимному адресу. Направление у ячейки не объявлено, поэтому
    // законность записи компилятор проверить не может.
    warnings.extend(crate::semantic::anon_collect::anon_write_warnings(model));
    warnings
}
