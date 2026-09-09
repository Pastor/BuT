//! Порты: направление, адреса, полнота адресации.
//!
//! Часть модуля `validate`.

use crate::diagnostics::lang::keys;
use crate::msg;
use super::*;

pub(super) fn validate_variables(model: Rc<RefCell<ModelNode>>) -> Vec<Diagnostic> {
    let borrowed = model.borrow();
    // Накопление по объявлениям: внутри одного выражения остаётся первая ошибка -
    // дальше пошли бы следствия, - но соседнее объявление высказывается своим
    // сообщением.
    let mut out = Vec::new();
    for variable in borrowed.variables.values() {
        match variable {
            VariableNode::Unresolved => {}
            VariableNode::Simple { expr, .. } | VariableNode::Const { expr, .. } => {
                out.extend(validate_expression(expr, model.clone()).err());
            }
            // У порта выражений два: размещение и начальное значение. Проверять надо
            // оба - имена в адресе (`at BASE + 4`) такие же настоящие, как в значении.
            VariableNode::Port { address, init, .. } => {
                out.extend(validate_expression(address, model.clone()).err());
                out.extend(validate_expression(init, model.clone()).err());
            }
        }
    }
    out
}

/// предупреждения о портах без адреса, попадающих в кодогенерацию.
///
/// Порт считается **достижимым кодогенерацией**, если он используется в логике
/// модели (условиях, блоках, функциях) - критерий переиспользуется из
/// [`compute_usage`](crate::semantic::unused::compute_usage). Для такого порта адрес
/// обязателен; источником может быть inline-инициализатор, оператор `address`
/// или внешняя карта (`external_ports` - имена портов, покрытых картой).
/// **Мёртвые** (неиспользуемые) порты без адреса **не** предупреждаются.
///
/// Возвращает предупреждения **SE-052**, отсортированные по позиции (для детерминизма).
/// Функция аналитическая: она не вызывается конвейером `validate_model` по умолчанию (в
/// текущей C-модели адрес не эмитится) - её подключает потребитель адресов
/// (C-таблица/HAL).
pub fn check_port_address_completeness(
    model: Rc<RefCell<ModelNode>>,
    external_ports: &HashSet<String>,
) -> Vec<Diagnostic> {
    let usage = crate::semantic::unused::compute_usage(Rc::clone(&model));
    let mut out = Vec::new();
    collect_incomplete_addresses(&model, &usage.ports, external_ports, &mut out);
    out.sort_by_key(|d| d.loc.start());
    out
}

/// Рекурсивно собирает используемые порты без адреса по дереву моделей.
fn collect_incomplete_addresses(
    model: &Rc<RefCell<ModelNode>>,
    used_ports: &HashSet<String>,
    external_ports: &HashSet<String>,
    out: &mut Vec<Diagnostic>,
) {
    let borrowed = model.borrow();
    for var in borrowed.variables.values() {
        let VariableNode::Port {
            address, loc, name, ..
        } = var
        else {
            continue;
        };
        if !used_ports.contains(name) {
            continue; // мёртвый порт - адрес не требуется
        }
        let has_inline = !matches!(address, ExpressionNode::None);
        let has_operator = borrowed.address_defs.iter().any(|d| &d.port == name);
        let has_external = external_ports.contains(name);
        if !has_inline && !has_operator && !has_external {
            let mut diagnostic = Diagnostic::warning(
                *loc,
                msg!(keys::SE_052_PORT_WITHOUT_ADDRESS, name = name),
            )
            .with_code("SE-052");
            // Порт, заведённый компилятором, автор в тексте не найдёт: отказ без этой
            // заметки читался как "исправьте то, чего вы не писали".
            if let Some(note) = crate::semantic::bounds_guard::synthetic_port_note(name) {
                diagnostic.notes.push(note);
            }
            out.push(diagnostic);
        }
    }
    let nested: Vec<Rc<RefCell<ModelNode>>> = borrowed.models.values().map(Rc::clone).collect();
    drop(borrowed);
    for nested_model in nested {
        collect_incomplete_addresses(&nested_model, used_ports, external_ports, out);
    }
}

/// Проверки оператора `address` для одной модели.
///
/// Наполнение [`address_defs`](ModelNode::address_defs) выполняет
/// [`construct_model`](super::tree::construct_model); здесь эти привязки сверяются с
/// объявленными портами:
///
/// - **Висячая привязка (`SE-048`).** `address` ссылается на имя, которого нет
///   среди портов модели.
/// - **Конфликт источников (`SE-049`).** Адрес порта задан одновременно inline
///   (`in P: T := <addr>;`) и оператором `address`, либо несколькими операторами
///   `address` для одного порта.
///
/// Приоритет источников (inline < `address` < внешняя карта) и построение `AddressMap`
/// для потребителей живут в слое адресов. Здесь достаточно гарантировать однозначность
/// источника адреса внутри модели.
pub(super) fn check_port_addresses(model: Rc<RefCell<ModelNode>>) -> Vec<Diagnostic> {
    let borrowed = model.borrow();
    let mut bound_by_address: HashSet<&str> = HashSet::new();
    // Накопление по операторам `address`: каждая привязка самостоятельна, и вторая
    // ошибочная не является следствием первой.
    let mut out = Vec::new();
    for def in &borrowed.address_defs {
        // Адрес обязан ссылаться на существующий порт.
        let Some(VariableNode::Port { address, .. }) = borrowed.variables.get(&def.port) else {
            out.push(
                Diagnostic::error(
                    def.loc,
                    msg!(keys::SE_048_ADDRESS_PORT_NOT_FOUND, name = def.port),
                )
                .with_code("SE-048"),
            );
            continue;
        };
        // Несколько операторов `address` для одного порта.
        if !bound_by_address.insert(def.port.as_str()) {
            out.push(
                Diagnostic::error(
                    def.loc,
                    msg!(keys::SE_049_ADDRESS_GIVEN_TWICE, name = def.port),
                )
                .with_code("SE-049"),
            );
            continue;
        }
        // Адрес задан и inline-инициализатором, и оператором `address`.
        if !matches!(address, ExpressionNode::None) {
            out.push(
                Diagnostic::error(
                    def.loc,
                    msg!(keys::SE_049_ADDRESS_INLINE_AND_OPERATOR, name = def.port),
                )
                .with_code("SE-049"),
            );
        }
    }
    out
}

/// Возвращает предупреждения о портах, объявленных во вложенных (не корневых) моделях.
///
/// Порты во вложенных моделях видны всем моделям в системе: они попадают в общие
/// перечисления `BitPort`, `RationalPort`, `NumericPort` и доступны через колбэки
/// корневой модели. Пользователи должны учитывать это при именовании портов.
///
/// Функция рекурсивно обходит все вложенные модели.
pub fn warn_nested_model_ports(model: Rc<RefCell<ModelNode>>) -> Vec<Diagnostic> {
    let mut result = Vec::new();
    let nested: Vec<Rc<RefCell<ModelNode>>> = model.borrow().models.values().cloned().collect();
    for nested_model in nested {
        let borrowed = nested_model.borrow();
        if borrowed.upper.is_some() {
            for var in borrowed.variables.values() {
                if let VariableNode::Port { name, loc, .. } = var {
                    result.push(Diagnostic::warning(
                        *loc,
                        msg!(
                            keys::PORT_NESTED_MODEL_VISIBLE,
                            name = name,
                            model = borrowed.name()
                        ),
                    ));
                }
            }
        }
        drop(borrowed);
        result.extend(warn_nested_model_ports(nested_model));
    }
    result
}
