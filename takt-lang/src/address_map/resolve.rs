//! Разрешение адресов портов модели с учётом приоритета источников (inline < `address`
//! < внешняя карта) -. Тема самостоятельна: ходит по дереву модели и сводит вычисленные
//! значения в итоговую карту.

use super::env::AddressEnv;
use super::eval::eval_addr_expr;
use super::parse::AddressMapEntry;
use crate::diagnostics::{Diagnostic, Location};
use crate::semantic::type_node::TypeNode;
use crate::semantic::{ExpressionNode, ModelNode, PortDirection, VariableNode};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// Источник, из которого получен адрес порта (по возрастанию приоритета).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AddressSource {
    /// Inline-инициализатор объявления (`in P: T := <addr>;`).
    Inline,
    /// Оператор `address P = <addr>;`.
    Operator,
    /// Внешняя карта адресов (`--address-map`).
    External,
}

/// Разрешённый адрес порта: числовое значение, бит и источник-победитель.
///
/// Поля `ty`/`direction` заполняются "бесплатно" тем же обходом, что и адрес
/// ([`resolve_model`] уже деструктурирует `VariableNode::Port`), и нужны
/// **выгрузке** карты наружу: формат `map` их не печатает, но
/// `json` эмитит тип и направление порта для генераторов HAL. Формат `map`
/// потребляет только `addr`/`bit`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedAddress {
    /// Числовой адрес.
    pub addr: i64,
    /// Битовая позиция (`0xADDR:bit`), если задана.
    pub bit: Option<i64>,
    /// Источник, из которого взят адрес (после разрешения приоритета).
    pub source: AddressSource,
    /// Тип порта (для выгрузки `json` - ).
    pub ty: TypeNode,
    /// Направление порта (`in`/`out`/`inout`) - для выгрузки `json`.
    pub direction: PortDirection,
    /// **Голое** имя порта (без квалификации моделью) -.
    ///
    /// Ключ [`AddressResolution::map`] с 0084 **квалифицирован** моделью
    /// ([`qualified_port_key`]), чтобы одноимённые порты разных под-моделей не затирали
    /// друг друга. Но публичные форматы (регистры `sv-mmio`, выгрузка `map`/`json` )
    /// показывают порт **пользовательским** именем - его и несёт это поле. Так
    /// квалификация ключа не протекает в выгрузку (аддитивность корпуса).
    pub name: String,
}

/// Квалифицированный ключ порта в [`AddressResolution::map`].
///
/// `model_unique` - уникальный путь модели (`Root:Child`, из `Name::unique()` у
/// потребителей и `unique_model_name` у продюсера - обе строки идентичны), `port` -
/// голое имя порта. Разделитель `\u{1}` в идентификаторах невозможен, поэтому ключ
/// однозначен без разбора (это ключ `HashMap`, парсить его не нужно). Продюсер и
/// **все** потребители обязаны строить ключ **этим** хелвером - иначе lookup
/// промахнётся молча (драйвер 3 ).
pub fn qualified_port_key(model_unique: &str, port: &str) -> String {
    format!("{model_unique}\u{1}{port}")
}

/// Метаданные порта **без** разрешённого адреса - для выгрузки `json`: она перечисляет
/// и мёртвые порты, помечая отсутствие адреса **явно** (не `0x0`, R8). Заполняется тем
/// же обходом, что и [`AddressResolution::map`] (второго прохода по модели нет).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PortMeta {
    /// Имя порта.
    pub name: String,
    /// Тип порта.
    pub ty: TypeNode,
    /// Направление (`in`/`out`/`inout`).
    pub direction: PortDirection,
}

/// Результат разрешения адресов модели.
#[derive(Debug, Default)]
pub struct AddressResolution {
    /// Итоговая карта: имя порта -> разрешённый адрес.
    pub map: HashMap<String, ResolvedAddress>,
    /// Порты **без** адреса ни из одного источника (для выгрузки `json`). Достижимые из
    /// них дают `SE-052` (ошибка); недостижимые - просто мёртвые. Экспорт `json`
    /// перечисляет их с `"address": null`.
    pub address_less: Vec<PortMeta>,
    /// Диагностики: ошибки полноты (SE-052) и предупреждения наложения (SE-050) / висячих
    /// записей карты (SE-051).
    pub diagnostics: Vec<Diagnostic>,
}

/// Диагностики выражений адреса для целей, которые адрес **не потребляют**.
pub fn address_expr_warnings(model: Rc<RefCell<ModelNode>>, env: &AddressEnv) -> Vec<Diagnostic> {
    let resolution = resolve_addresses(model, &[], env);
    let mut out: Vec<Diagnostic> = resolution
        .diagnostics
        .into_iter()
        .filter(|d| matches!(d.code.as_deref(), Some("SE-054") | Some("SE-055")))
        .map(|d| Diagnostic {
            level: crate::diagnostics::Level::Warning,
            ..d
        })
        .collect();
    out.sort_by_key(|d| d.loc.start());
    out
}

/// Разрешает адреса всех портов модели с учётом приоритета источников (inline <
/// `address` < внешняя карта) -.
///
/// Строит консолидированную карту `имя порта -> ResolvedAddress` и собирает диагностики
/// для адрес-потребляющего режима (`c-hal`):
///
/// - **SE-052** - используемый (достижимый кодогенерацией) порт без адреса ни из
///   одного источника (ошибка полноты);
/// - **SE-050** - внешняя карта переопределяет адрес, заданный в модели
///   (предупреждение наложения);
/// - **SE-051** - запись внешней карты для несуществующего порта.
///
/// Конфликт inline + `address` внутри модели уже исключён семантикой (SE-049 на этапе
/// `construct_model`), поэтому здесь не проверяется.
pub fn resolve_addresses(
    model: Rc<RefCell<ModelNode>>,
    external: &[AddressMapEntry],
    env: &AddressEnv,
) -> AddressResolution {
    let usage = crate::semantic::unused::compute_usage(Rc::clone(&model));
    let external_by_name: HashMap<&str, &AddressMapEntry> =
        external.iter().map(|e| (e.name.as_str(), e)).collect();

    let mut result = AddressResolution::default();
    resolve_model(&model, &usage.ports, &external_by_name, env, &mut result);

    // SE-053: define перекрыл `const` модели. Позиция - у объявления `const`: показать
    // надо то, что перекрыто (симметрия с `SE-050`).
    for (name, const_loc) in env.overrides() {
        result.diagnostics.push(
            Diagnostic::warning(
                const_loc,
                format!(
                    "--define '{}' перекрывает одноимённую `const` модели в выражении адреса; \
                     в логике автомата `const` сохраняет своё значение",
                    name
                ),
            )
            .with_code("SE-053"),
        );
    }

    // DF-004: define, которого не спросило ни одно выражение адреса, - почти всегда
    // опечатка в имени (симметрия с `SE-051`).
    for name in env.unused() {
        result.diagnostics.push(
            Diagnostic::warning(
                Location::CommandLine,
                format!(
                    "--define '{}' не использован: ни одно выражение адреса к нему не обращается",
                    name
                ),
            )
            .with_code("DF-004"),
        );
    }

    // SE-051: записи карты без соответствующего порта. Ключ карты с 0084 квалифицирован
    // моделью, поэтому наличие порта проверяется по **голому** имени среди значений
    // (`ResolvedAddress::name`), а не `contains_key` (внешняя `.ld` адресует по голому
    // имени - драйвер 4 ).
    for e in external {
        if !result.map.values().any(|r| r.name == e.name) {
            result.diagnostics.push(
                Diagnostic::warning(
                    e.loc,
                    format!(
                        "внешняя карта задаёт адрес для несуществующего порта '{}'",
                        e.name
                    ),
                )
                .with_code("SE-051"),
            );
        }
    }
    // Порядок детерминирован. Диагностики без файловой позиции (`DF-004` - у аргумента
    // CLI её нет) идут последними: привязать их к месту в тексте не к чему, а
    // `Location::start()` на них паникует.
    result.diagnostics.sort_by_key(|d| match d.loc {
        Location::Source(_, start, _) => (0usize, start),
        _ => (1usize, 0),
    });
    result
}

/// Рекурсивный обход дерева моделей для разрешения адресов портов. Уникальный путь
/// модели (`Root:Child`) - совпадает с `minimap::Name::unique()`. Реплика
/// `minimap::unique_model_name`: обход `upper` вверх до корня, разделитель `:`.
/// Согласие с потребителем `c-hal` (он берёт `Name::unique()`) - критический инвариант
/// ключа карты (драйвер 3 ): продюсер и потребитель строят одну и ту же строку.
fn model_unique_name(model: &Rc<RefCell<ModelNode>>) -> String {
    let name = model.borrow().name.clone().unwrap_or_default();
    if let Some(upper) = model.borrow().upper.as_ref()
        && let Some(parent) = upper.upgrade()
    {
        let parent_name = model_unique_name(&parent);
        if parent_name.is_empty() {
            return name;
        }
        return format!("{parent_name}:{name}");
    }
    name
}

fn resolve_model(
    model: &Rc<RefCell<ModelNode>>,
    used_ports: &std::collections::HashSet<String>,
    external_by_name: &HashMap<&str, &AddressMapEntry>,
    env: &AddressEnv,
    out: &mut AddressResolution,
) {
    let borrowed = model.borrow();
    for var in borrowed.variables.values() {
        let VariableNode::Port {
            address,
            loc,
            name,
            ty,
            direction,
            ..
        } = var
        else {
            continue;
        };

        // Выражения вычисляются до выбора слоя-победителя, но в цепочку приоритета это
        // ничего не добавляет.
        //
        // `failed` отличает "источника адреса нет" от "источник есть, но его выражение
        // сломано". Без этого различия рядом с точной причиной (`SE-054`/`SE-055`)
        // вылезал бы и `SE-052` "нет адреса" - то есть пользователь снова получал бы
        // диагностику о следствии, ради ухода от которой фича и делается.
        let mut failed = false;
        let mut eval = |expr: &ExpressionNode, out: &mut AddressResolution| match eval_addr_expr(
            expr, model, env,
        ) {
            Ok(v) => v,
            Err(d) => {
                out.diagnostics.push(d);
                failed = true;
                None
            }
        };
        // Inline-источник - теперь **только** `at <адрес>`: `:=` в объявлении порта
        // означает начальное значение и адресом больше не является.
        let inline = eval(address, out);
        let operator = match borrowed.address_defs.iter().find(|d| &d.port == name) {
            Some(d) => eval(&d.value, out),
            None => None,
        };
        let external = external_by_name.get(name.as_str());

        // Приоритет: внешняя карта > оператор > inline.
        let resolved = if let Some(e) = external {
            // Наложение поверх адреса модели - предупреждение SE-050.
            if inline.is_some() || operator.is_some() {
                out.diagnostics.push(
                    Diagnostic::warning(
                        e.loc,
                        format!(
                            "внешняя карта переопределяет адрес порта '{}', заданный в модели",
                            name
                        ),
                    )
                    .with_code("SE-050"),
                );
            }
            Some(ResolvedAddress {
                addr: e.addr,
                bit: e.bit,
                source: AddressSource::External,
                ty: ty.clone(),
                direction: *direction,
                name: name.clone(),
            })
        } else if let Some((addr, bit)) = operator {
            Some(ResolvedAddress {
                addr,
                bit,
                source: AddressSource::Operator,
                ty: ty.clone(),
                direction: *direction,
                name: name.clone(),
            })
        } else {
            inline.map(|(addr, bit)| ResolvedAddress {
                addr,
                bit,
                source: AddressSource::Inline,
                ty: ty.clone(),
                direction: *direction,
                name: name.clone(),
            })
        };

        match resolved {
            Some(mut r) => {
                // SE-090: у однобитного порта адрес без позиции бита неполон - HAL
                // читает бит по позиции. Умолчание "бит 0" существовало и раньше, но
                // принималось тремя потребителями порознь: `c-hal` подставлял его в
                // самом C-коде (`b.bit < 0 ? 0 : b.bit`), `st-at` - предупреждением
                // `ST-005`, `sv-mmio` - молчаливым `unwrap_or(0)`; выгрузка `json` не
                // говорила о нём вовсе (`"bit": null`). Решение языка не имеет права
                // приниматься сгенерированным кодом, поэтому нормируется здесь - до
                // всех потребителей.
                //
                // Тип проверяется `Bit | Bool` (а не один `Bit`): именно такой предикат
                // `is_bool` у цели `st-at`, и разойдись они - умолчание снова оказалось
                // бы в двух местах. Не-битовому порту ноль подставлять нельзя: у слова
                // позиции бита нет.
                if r.bit.is_none() && matches!(r.ty, TypeNode::Bit | TypeNode::Bool) {
                    r.bit = Some(0);
                    out.diagnostics.push(
                        Diagnostic::warning(
                            *loc,
                            format!(
                                "порт '{}' однобитный, но в адресе не задана позиция бита: \
                                 принят бит 0. Если подразумевался другой бит, укажите его \
                                 явно — `0xADDR:бит`",
                                name
                            ),
                        )
                        .with_code("SE-090"),
                    );
                }
                // SE-060: бит адреса вне диапазона [0, 63]. Проверяется
                // финализированный бит (после выбора источника- победителя), а не
                // inline: адрес мог прийти оператором или картой. Диапазон - предел
                // `uint64_t`: слова шире умолчательный HAL не читает, а `>> >=64` - UB.
                // Живёт в адрес-потребляющем слое, поэтому цель `c` (бит игнорирует) не
                // задета.
                if let Some(b) = r.bit
                    && !(0..64).contains(&b)
                {
                    out.diagnostics.push(
                        Diagnostic::error(
                            *loc,
                            format!(
                                "бит {} адреса порта '{}' вне диапазона [0, 63]: дефолтный HAL \
                                 читает слово шириной до 64 бит (uint64_t), а сдвиг на большую \
                                 величину — неопределённое поведение. Укажите бит 0…63",
                                b, name
                            ),
                        )
                        .with_code("SE-060"),
                    );
                }
                // ключ квалифицирован моделью, иначе одноимённые порты разных
                // под-моделей затирали бы друг друга.
                out.map
                    .insert(qualified_port_key(&model_unique_name(model), name), r);
            }
            None => {
                // Порт без адреса - записать для выгрузки json (мёртвый порт помечается
                // явным отсутствием адреса, R8), а не только диагностировать. Тот же
                // обход, второго прохода нет.
                out.address_less.push(PortMeta {
                    name: name.clone(),
                    ty: ty.clone(),
                    direction: *direction,
                });
                // SE-052: используемый порт без адреса ни из одного источника. Если
                // источник был, но его выражение не вычислилось, причина уже названа
                // (`SE-054`/`SE-055`) - второй диагностики не надо.
                if used_ports.contains(name) && !failed {
                    let mut diagnostic = Diagnostic::error(
                        *loc,
                        format!(
                            "порт '{}' используется в кодогенерации, но не имеет адреса \
                             (ни inline, ни оператором `address`, ни во внешней карте)",
                            name
                        ),
                    )
                    .with_code("SE-052");
                    // Порт, заведённый флагом, автор в тексте не найдёт - заметку даёт
                    // тот проход, который его завёл.
                    if let Some(note) = crate::semantic::bounds_guard::synthetic_port_note(name) {
                        diagnostic.notes.push(note);
                    }
                    out.diagnostics.push(diagnostic);
                }
            }
        }
    }
    let nested: Vec<Rc<RefCell<ModelNode>>> = borrowed.models.values().map(Rc::clone).collect();
    drop(borrowed);
    for nested_model in nested {
        resolve_model(&nested_model, used_ports, external_by_name, env, out);
    }
}
