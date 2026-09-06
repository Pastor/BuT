//! Имена сигналов цели `sv` и проверки имён, требующие доступа к модели.

use crate::diagnostics::Diagnostic;
use crate::generator::sv::sv_map::SvMap;
use crate::generator::sv::sv_module::check_sv_name;
use crate::semantic::minimap::Name;

/// Проверяет имена состояний модели на пригодность для SystemVerilog.
///
/// Позиция берётся из семантической модели: перечислитель печатается по `Name`, у
/// которого позиции нет, а диагностика обязана указывать на объявление автора.
pub(crate) fn check_state_names(map: &SvMap, model: &Name) -> Result<(), Diagnostic> {
    let raw = map.raw_model_at(model.clone())?;
    let borrowed = raw.borrow();
    for state in borrowed.states.values() {
        let (name, loc) = match state {
            crate::semantic::StateNode::Simple { name, loc, .. }
            | crate::semantic::StateNode::Implement { name, loc, .. } => (name, *loc),
            crate::semantic::StateNode::Unresolved => continue,
        };
        check_sv_name(name, loc)?;
    }
    Ok(())
}

// -- Имена сигналов ---------------------------------------------------------
//
// Переехало из `sv_expr` по границе ответственности: печать выражения отвечает на
// вопрос "как выглядит значение", а эти функции - на вопрос "как зовётся сигнал", и он
// один на все три печатника (выражения, условия, операторы).

use crate::generator::sv::sv_expr::Scope;
use crate::parser::ast::Member;
use crate::semantic::VariableNode;

// -- Имена машины шагов цепочки `+` --------------
//
// Вынесены из `sv_fsm` по границе ответственности: тот модуль отвечает за автомат, а
// имя сигнала - правило именования, и оно живёт здесь вместе с прочими (`signal_of`,
// `const_signal`). Повод к выносу - предел размера модуля
// (`scripts/check-module-size.sh`).

/// Имя регистра шага цепочки `+`, несомой состоянием `state`.
///
/// Ключ - уникальное имя несущего состояния **и место цепочки в дереве композиции**
/// (`path`, носитель [`chain_site`](crate::generator::chain_site)): цепочек в одном
/// состоянии бывает несколько (`(A + B) | C`), и по одному имени состояния они делили
/// бы регистр - то есть шли бы одной машиной шагов.
pub(crate) fn step_reg_name(state: &Name, path: &[usize]) -> String {
    format!(
        "{}_step{}",
        state.unique_lowercase_snakecase(),
        crate::generator::chain_site::suffix(path)
    )
}

/// Имя типа-перечисления шага цепочки `+`.
pub(crate) fn step_enum_name(state: &Name, path: &[usize]) -> String {
    format!(
        "{}_step{}_e",
        state.unique_lowercase_snakecase(),
        crate::generator::chain_site::suffix(path)
    )
}

/// Имя варианта `STEP_i` шага цепочки `+`.
pub(crate) fn step_variant(state: &Name, path: &[usize], i: usize) -> String {
    format!(
        "{}_STEP{}_{}",
        state.unique_uppercase_snakecase(),
        crate::generator::chain_site::suffix(path).to_uppercase(),
        i
    )
}

/// Имя терминального варианта вложенной цепочки.
///
/// У цепочки верхнего уровня его нет: пройдя последний шаг, она уводит
/// **родительское состояние**, и лишний вариант остался бы недостижимым. У
/// вложенной выхода из состояния нет - её завершение читает вмещающая
/// композиция, и читать его надо по собственному состоянию цепочки, а не по
/// "все шаги готовы": шаг, до которого очередь не дошла, готовности не
/// выставлял ни разу.
pub(crate) fn step_done_variant(state: &Name, path: &[usize]) -> String {
    format!(
        "{}_STEP{}_DONE",
        state.unique_uppercase_snakecase(),
        crate::generator::chain_site::suffix(path).to_uppercase()
    )
}

/// Имя варианта перечисления в SV: `Action`/`Idle` -> `ACTION_IDLE`.
///
/// Преисправление именем перечисления обязателен: метки `enum` в SystemVerilog живут в
/// **общем пространстве имён модуля**, а не внутри своего типа (в отличие от
/// Rust, где `Action::Idle` квалифицировано). Два перечисления модели с
/// одноимённым вариантом (`Idle` у `Action` и у `Mode`) без преисправления дали бы
/// повторное объявление.
pub(crate) fn sv_enum_variant_name(enum_name: &str, variant: &str) -> String {
    format!(
        "{}_{}",
        crate::generator::sv::sv_type::sv_enum_type_name(enum_name)
            .trim_end_matches("_e")
            .to_uppercase(),
        crate::semantic::naming::normalize_lowercase_snakecase(variant.to_string()).to_uppercase()
    )
}

/// Имя сигнала с учётом локальных имён печатаемой функции.
///
/// Признак "локальная переменная функции" в [`signal_of`] строится как "имя не
/// объявлено в модели" - и при совпадении имён он ложен: локальная `s` функции получала
/// преисправление модели и писала в её сигнал. Список локальных имён снимает двусмысленность
/// там, где узел её не различает.
pub(crate) fn signal_of_in(
    var: &std::rc::Rc<std::cell::RefCell<VariableNode>>,
    scope: &Scope,
) -> Option<String> {
    if let VariableNode::Simple { name, .. } = &*var.borrow()
        && scope.locals.contains(name)
    {
        return Some(name.clone());
    }
    signal_of(var)
}

/// Извлекает имя элемента Takt из узла переменной.
///
/// Возвращает **имя Takt**, а не имя сигнала: отображение в сигнал делает [`Scope`],
/// который один знает про преисправления уплощения.
pub(crate) fn signal_of(var: &std::rc::Rc<std::cell::RefCell<VariableNode>>) -> Option<String> {
    match &*var.borrow() {
        // Переменная модели получает преисправление владельца, а не "модели, которую сейчас
        // печатаем": имя владельца берётся из самой переменной (`upper`), поэтому
        // одноимённые переменные разных под-моделей расходятся сами собой, без карты
        // имён и без риска, что карта отобразит не ту.
        VariableNode::Simple { upper, name, .. } => {
            let Some(owner) = upper.as_ref().and_then(|u| u.upgrade()) else {
                // Владельца нет - это не переменная модели, а локальная переменная
                // функции: у неё нет ни регистра, ни преисправления.
                return Some(name.clone());
            };
            // Параметры и локальные переменные функции - Тоже `Simple` и тоже ссылаются
            // на модель. Отличить их можно ровно одним: они не объявлены в самой
            // модели. Без этой проверки `travel_time(to_stack)` печатал бы
            // `stacker_to_stack` - сигнал, которого не существует (проба 2026-07-16:
            // `Can't find definition of variable`).
            if !owner.borrow().variables.contains_key(name) {
                return Some(name.clone());
            }
            let model: crate::semantic::minimap::Name = owner.into();
            Some(format!("{}_{}", model.unique_lowercase_snakecase(), name))
        }
        // Порт - вывод кристалла, его имя задал автор и оно уникально по модулю
        // (`collect_ports` дедуплицирует). Константа - `localparam` уровня модуля:
        // преисправление ей не нужен и только мешал бы читать - **кроме** константы,
        // выведенной из параметра модели. Константа перечислимого типа печатается
        // именем варианта, а не именем `localparam`: типизированный `localparam mode_e
        // X` не принимает **yosys** ("Non-constant width range on parameter decl"), а
        // нетипизированный ломает verilator - приёмник строго типизирован
        // (`ENUMVALUE`). Имя варианта верно обоим: оно само несёт свой тип.
        VariableNode::Const {
            upper,
            name,
            ty,
            expr,
            ..
        } => enum_constant_literal(upper.as_ref(), ty, expr)
            .or_else(|| Some(const_signal(upper.as_ref(), name))),
        VariableNode::Port { name, .. } => Some(name.clone()),
        VariableNode::Unresolved => None,
    }
}

/// Имя перечислителя для константы перечислимого типа.
///
/// `None`, если тип не перечислимый либо значение не совпало ни с одним вариантом:
/// тогда обращение печатается прежним именем `localparam`.
pub(crate) fn enum_constant_literal(
    upper: Option<&std::rc::Weak<std::cell::RefCell<crate::semantic::ModelNode>>>,
    ty: &crate::semantic::type_node::TypeNode,
    expr: &crate::semantic::ExpressionNode,
) -> Option<String> {
    let crate::semantic::type_node::TypeNode::Enum(enum_name) = ty else {
        return None;
    };
    let crate::semantic::ExpressionNode::Number(value) = expr else {
        return None;
    };
    let owner = upper?.upgrade()?;
    let def = owner.borrow().search_enum(enum_name)?;
    let (variant, _) = def.variants.iter().find(|(_, v)| v == value)?;
    Some(sv_enum_variant_name(enum_name, variant))
}

/// Имя `localparam` константы - **с преисправлением владельца**.
///
/// Композиция в цели `sv` **уплощается**, и `localparam` живёт на уровне
/// модуля: две модели с одноимённой константой разных значений давали **одно**
/// объявление, и вторая молча получала значение первой (проба: `model A { const
/// K:= 2; } model B { const K:= 3; }` давала один `localparam K = 2`).
/// Поэтому преисправление несут **все** константы, а не только выведенные из параметра
/// модели - правило то же, что у регистров (`Simple` выше), и берётся оно у
/// **владельца**, а не у "модели, которую печатаем". Исключение для обычных
/// констант, заведённое ради неизменного вывода корпуса, снято.
///
/// Тем же именем идёт дедупликация объявлений (`sv_const::emit_constants`) и
/// согласуется ключ "константа используется" ([`crate::semantic::unused::const_key`]):
/// печать и фильтрация - одно правило.
pub(crate) fn const_signal(
    upper: Option<&std::rc::Weak<std::cell::RefCell<crate::semantic::ModelNode>>>,
    name: &str,
) -> String {
    let Some(owner) = upper.and_then(|u| u.upgrade()) else {
        return name.to_string();
    };
    let model: crate::semantic::minimap::Name = owner.into();
    format!("{}_{}", model.unique_lowercase_snakecase(), name)
}

/// Печатает доступ к члену (`x.0` -> `x[0]`, `p.field` -> `p.field`).
///
/// **Битового доступа как отдельной конструкции в SV нет и не нужно:** вектор
/// индексируется тем же `[]`, что и массив. Это заметно проще цели `st`, где
/// `x.0` разворачивается в маску `(USINT_TO_BYTE(x) AND 16#01) <> 16#00` -
/// MatIEC не знает ни `x.0`, ни `%X0` (`CLAUDE.md`).
pub(crate) fn print_member(base: &str, member: &Member) -> String {
    match member {
        Member::Number(index) => format!("{}[{}]", base, index),
        Member::Identifier(id) => format!("{}.{}", base, id.name),
    }
}
