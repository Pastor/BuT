//! Объявления порождаемого модуля: перечисления, константы, порты, HAL-трейт, `struct`
//! моделей.

use crate::diagnostics::{Diagnostic, Location};
use crate::generator::indent::Printer;
use crate::generator::rust::rust_expr::{Scope, const_ident};
use crate::generator::rust::rust_map::RustMap;
use crate::generator::rust::rust_name::{check_name_collisions, rust_type_name, rust_value_name};
use crate::generator::rust::rust_port::{PortClass, port_class};
use crate::generator::rust::rust_type::{enum_repr, rust_type};
use crate::semantic::minimap::Name;
use crate::semantic::type_node::TypeNode;
use crate::semantic::{ModelNode, PortDirection, VariableNode};
use std::collections::{BTreeMap, BTreeSet};

/// Один порт модели, подготовленный к эмиссии.
///
/// Категория и направление в поля не кладутся: порт уже разложен по нужному
/// перечислению [`PortSet`], то есть они выражены **местом** записи, а не значением.
/// Дублировать их полями значило бы завести второй источник истины.
pub(crate) struct Port {
    /// Исходное имя порта (ключ в `usage`).
    pub(crate) raw: String,
    /// Имя варианта перечисления (CamelCase).
    pub(crate) variant: String,
}

/// Сводка портов всех моделей файла: состав перечислений и трейта.
#[derive(Default)]
pub(crate) struct PortSet {
    /// Входные порты по категориям.
    pub(crate) inputs: BTreeMap<String, Vec<Port>>,
    /// Выходные порты по категориям.
    pub(crate) outputs: BTreeMap<String, Vec<Port>>,
    /// Категории, встретившиеся хоть раз (для состава трейта).
    pub(crate) classes: BTreeMap<String, PortClass>,
    /// Требуется ли метод `debug` (встроенная функция в профиле `no_std`).
    pub(crate) needs_debug: bool,
    /// Требуется ли метод `now_ms` - внешний источник времени (профиль "часы"). Как и
    /// `debug`, это метод трейта без тела: `no_std`-часов нет.
    pub(crate) needs_now_ms: bool,
    /// Внешние функции (`extern fn`) -> методы трейта.
    pub(crate) externals: BTreeMap<String, String>,
}

impl PortSet {
    /// Пуст ли трейт: нет ни портов, ни `debug`, ни `extern fn`.
    ///
    /// Модель без портов **не эмитит ни трейта, ни параметра типа `H`**. Ограничивает
    /// эмиссию не "неиспользуемый параметр типа", а `dead_code` на поле - ещё один
    /// случай общего правила R9.
    pub(crate) fn is_empty(&self) -> bool {
        self.inputs.is_empty()
            && self.outputs.is_empty()
            && !self.needs_debug
            && !self.needs_now_ms
            && self.externals.is_empty()
    }
}

/// Собирает порты всех моделей файла в единую сводку.
///
/// Порты собираются со **всех** моделей, а не только с корня: в `elevator_mini.takt`
/// они объявлены внутри под-моделей (`out elevator_motor_up: bit;` в `Motor`).
/// Перечисления портов - общие для файла, как и в цели `c`.
pub(crate) fn collect_ports(
    map: &RustMap,
    blocks: &[(Name, std::rc::Rc<std::cell::RefCell<ModelNode>>)],
) -> Result<PortSet, Diagnostic> {
    let mut set = PortSet::default();
    let mut seen: BTreeSet<String> = BTreeSet::new();
    for (_, model_rc) in blocks {
        let model = model_rc.borrow();
        for (raw, var) in &model.variables {
            let VariableNode::Port {
                name,
                ty,
                direction,
                loc,
                ..
            } = var
            else {
                continue;
            };
            if !map.usage().ports.contains(name) || !seen.insert(name.clone()) {
                continue;
            }
            let class = port_class(ty, name, *loc, &model)?;
            let port = Port {
                raw: raw.clone(),
                variant: rust_type_name(name, *loc)?,
            };
            set.classes.insert(class.in_enum(), class.clone());
            match direction {
                PortDirection::In => set.inputs.entry(class.in_enum()).or_default().push(port),
                PortDirection::Out => set.outputs.entry(class.out_enum()).or_default().push(port),
                // `inout` поддержан: в Rust он ложится на пару методов трейта без
                // натяжки - порт попадает и во входное, и в выходное перечисление. Цель
                // `sv` его запретила (`SV-006`), но там причина физическая:
                // двунаправленной линии нужен сигнал `oe`, которого Takt не выражает.
                // Здесь такой причины нет, поэтому запрещать нечего - а тихо
                // игнорировать нельзя.
                PortDirection::InOut => {
                    set.inputs.entry(class.in_enum()).or_default().push(Port {
                        raw: raw.clone(),
                        variant: rust_type_name(name, *loc)?,
                    });
                    set.outputs.entry(class.out_enum()).or_default().push(port);
                }
            }
        }
        // `extern fn` -> метод HAL (решение (а) ): единообразно с портами,
        // типобезопасно и без `unsafe`. Вариант `extern "C"` отвергнут именно потому,
        // что внёс бы `unsafe` в порождаемый код и тем самым уничтожил бы главную
        // дельту фичи к цели `c`.
        for def in model.functions.values() {
            if let crate::semantic::FunctionDefinitionNode::External {
                name,
                params,
                ret,
                loc,
                ..
            } = def
            {
                let mut signature = String::new();
                for (pname, pty) in params {
                    signature.push_str(&format!(
                        ", {}: {}",
                        rust_value_name(pname, *loc)?,
                        rust_type(
                            pty,
                            &format!("параметр '{}' внешней функции '{}'", pname, name)
                        )?
                    ));
                }
                let ret_str = match ret {
                    TypeNode::Unit => String::new(),
                    other => format!(
                        " -> {}",
                        rust_type(other, &format!("возврат внешней функции '{}'", name))?
                    ),
                };
                set.externals.insert(
                    rust_value_name(name, *loc)?,
                    format!("(&mut self{}){}", signature, ret_str),
                );
            }
        }
    }
    Ok(set)
}

/// Печатает перечисления портов и трейт `Hal`.
pub(crate) fn emit_hal(p: &mut Printer, set: &PortSet) -> Result<(), Diagnostic> {
    if set.is_empty() {
        return Ok(());
    }

    // Перечисления портов публичны: они стоят в сигнатуре публичного трейта, и
    // приватными быть не могут (`private_interfaces`).
    for (enum_name, ports) in set.inputs.iter().chain(set.outputs.iter()) {
        let names: Vec<(String, String)> = ports
            .iter()
            .map(|port| (port.raw.clone(), port.variant.clone()))
            .collect();
        check_name_collisions(
            &names,
            &format!("порты перечисления {}", enum_name),
            Location::Codegen,
        )?;

        p.ident("#[derive(Debug, Clone, Copy, PartialEq, Eq)]").nl();
        p.ident(&format!("pub enum {} {{", enum_name)).nl();
        p.up();
        for port in ports {
            p.ident(&format!("{},", port.variant)).nl();
        }
        p.down();
        p.ident("}").nl().nl();
    }

    p.ident("pub trait Hal {").nl();
    p.up();
    for enum_name in set.inputs.keys() {
        let class = &set.classes[enum_name];
        p.ident(&format!(
            "fn {}(&mut self, port: {}) -> {};",
            class.read_fn(),
            class.in_enum(),
            class.value_type()
        ))
        .nl();
    }
    for enum_name in set.outputs.keys() {
        let class = set
            .classes
            .values()
            .find(|c| &c.out_enum() == enum_name)
            .ok_or_else(|| {
                Diagnostic::error(
                    Location::Codegen,
                    format!("Категория порта для '{}' не найдена", enum_name),
                )
                .with_code("RS-012")
            })?;
        p.ident(&format!(
            "fn {}(&mut self, port: {}, value: {});",
            class.write_fn(),
            class.out_enum(),
            class.value_type()
        ))
        .nl();
    }
    if set.needs_debug {
        p.ident("fn debug(&mut self, message: &str);").nl();
    }
    if set.needs_now_ms {
        p.ident("fn now_ms(&mut self) -> u64;").nl();
    }
    for (name, signature) in &set.externals {
        p.ident(&format!("fn {}{};", name, signature)).nl();
    }
    p.down();
    p.ident("}").nl().nl();
    Ok(())
}

/// Печатает пользовательские перечисления модели.
///
/// Перечисления **публичны**: их варианты объявил автор `.takt`, они - экспортируемый
/// словарь модуля (цель `c` раскрывает их как `#define`). Публичность здесь не глушит
/// `dead_code`, а честно отражает, что вариант существует по воле автора, а не по
/// решению генератора. Перечисления состояний, наоборот, приватны - их придумывает
/// генератор, и там `dead_code` остаётся тестом (см. Имя функции "значение с порта ->
/// вариант".
pub(crate) const FROM_REPR: &str = "from_repr";

/// Перечисления, значения которых модель читает с портов.
///
/// Признак - **факт чтения**, а не направление порта (тот же носитель, что у сторон
/// `inout` в ). Направление обмануло: `inout`, который модель только пишет, объявлен
/// читаемым, и [`FROM_REPR`] печаталась впустую - `clippy` под `-D warnings` отвечает
/// "associated function is never used".
fn enums_read_from_ports(
    blocks: &[(Name, std::rc::Rc<std::cell::RefCell<ModelNode>>)],
) -> BTreeSet<String> {
    let mut out = BTreeSet::new();
    for (_, model_rc) in blocks {
        let reads = crate::semantic::usage_tree::reads_with_implementations(model_rc);
        let model = model_rc.borrow();
        for var in model.variables.values() {
            let VariableNode::Port {
                name,
                ty,
                direction,
                ..
            } = var
            else {
                continue;
            };
            if matches!(direction, PortDirection::Out) || !reads.ports.contains(name) {
                continue;
            }
            if let TypeNode::Enum(enum_name) = ty {
                out.insert(enum_name.clone());
            }
        }
    }
    out
}

pub(crate) fn emit_enums(
    p: &mut Printer,
    blocks: &[(Name, std::rc::Rc<std::cell::RefCell<ModelNode>>)],
) -> Result<(), Diagnostic> {
    let read_from_ports = enums_read_from_ports(blocks);
    let mut seen: BTreeSet<String> = BTreeSet::new();
    for (_, model_rc) in blocks {
        let model = model_rc.borrow();
        for def in model.enums.values() {
            let name = rust_type_name(&def.name, def.loc)?;
            if !seen.insert(name.clone()) {
                continue;
            }
            let variants: Vec<(String, String)> = def
                .variants
                .iter()
                .map(|(v, _)| Ok((v.clone(), rust_type_name(v, def.loc)?)))
                .collect::<Result<Vec<_>, Diagnostic>>()?;
            check_name_collisions(
                &variants,
                &format!("варианты перечисления '{}'", def.name),
                def.loc,
            )?;

            // Разрядность - По диапазону вариантов. `#[repr(u8)]` по умолчанию отверг
            // бы `Idle = 670` из `elevator.takt:121`.
            p.ident("#[derive(Debug, Clone, Copy, PartialEq, Eq)]").nl();
            p.ident(&format!("#[repr({})]", enum_repr(&def.variants)))
                .nl();
            p.ident(&format!("pub enum {} {{", name)).nl();
            p.up();
            for ((_, value), (_, variant)) in def.variants.iter().zip(variants.iter()) {
                p.ident(&format!("{} = {},", variant, value)).nl();
            }
            p.down();
            p.ident("}").nl().nl();

            if read_from_ports.contains(&def.name) {
                emit_from_repr(p, &name, def, &variants)?;
            }
        }
    }
    Ok(())
}

/// Печатает восстановление варианта из значения порта.
///
/// Функция **тотальна**: значение вне набора даёт умолчание перечисления - первый по
/// тексту вариант. Паника здесь недопустима: число на входном порту приходит от железа,
/// и модель обязана продолжать работу.
fn emit_from_repr(
    p: &mut Printer,
    name: &str,
    def: &crate::semantic::EnumDefinitionNode,
    variants: &[(String, String)],
) -> Result<(), Diagnostic> {
    let repr = enum_repr(&def.variants);
    let default = variants.first().map(|(_, v)| v.clone()).ok_or_else(|| {
        Diagnostic::error(def.loc, "перечисление без вариантов".to_string()).with_code("RS-016")
    })?;
    p.ident(&format!("impl {name} {{")).nl();
    p.up();
    p.ident(&format!("fn {FROM_REPR}(value: {repr}) -> Self {{"))
        .nl();
    p.up();
    p.ident("match value {").nl();
    p.up();
    for ((_, value), (_, variant)) in def.variants.iter().zip(variants.iter()) {
        p.ident(&format!("{value} => Self::{variant},")).nl();
    }
    p.ident(&format!("_ => Self::{default},")).nl();
    p.down();
    p.ident("}").nl();
    p.down();
    p.ident("}").nl();
    p.down();
    p.ident("}").nl().nl();
    Ok(())
}

/// Печатает объявления структур модели.
///
/// **Зачем.** `rust_type` отображает `TypeNode::Struct` в имя типа, но самого
/// объявления цель не эмитила, а доступ к полю отвергала `RS-011` - то есть
/// структуры не переводились дальше объявления переменной.
///
/// `Default` обязателен: поля модели инициализируются `Default::default()` в `new`, и
/// без него порождённый код не собрался бы. `Copy` избавляет от заимствований при
/// чтении поля.
pub(crate) fn emit_structs(
    p: &mut Printer,
    blocks: &[(Name, std::rc::Rc<std::cell::RefCell<ModelNode>>)],
) -> Result<(), Diagnostic> {
    let mut seen: BTreeSet<String> = BTreeSet::new();
    for (_, model_rc) in blocks {
        let model = model_rc.borrow();
        for def in model.structs.values() {
            let name = rust_type_name(&def.name, def.loc)?;
            if !seen.insert(name.clone()) {
                continue;
            }
            // `Eq` не выводится: у поля `float` (`f64`) его нет, и вывод корпуса
            // перестал бы компилироваться (`the trait Eq is not implemented for f64`).
            // Сравнение структур язык и так запрещает (`SE-059`), поэтому `PartialEq`
            // достаточно. `Default` выводится не всегда: у перечисления его нет вовсе,
            // а `impl Default for [T; N]` в стандартной библиотеке существует лишь до N
            // = 32.
            let derived = crate::generator::rust::rust_struct::derives_default(
                &TypeNode::Struct(def.name.clone()),
                &model,
            );
            // Комментарий автора перед объявлением типа. Печатается до атрибутов: между
            // `#[derive]` и `struct` комментарий разорвал бы объявление, а
            // Rust-читатель ждёт его сверху.
            for line in crate::generator::comments::leading(
                def.loc,
                crate::generator::header::CommentStyle::Slashes,
            ) {
                p.ident(&line).nl();
            }
            if derived {
                p.ident("#[derive(Debug, Clone, Copy, PartialEq, Default)]")
                    .nl();
            } else {
                p.ident("#[derive(Debug, Clone, Copy, PartialEq)]").nl();
            }
            p.ident(&format!("pub struct {} {{", name)).nl();
            p.up();
            for (field, ty) in &def.fields {
                let rust_ty = crate::generator::rust::rust_type::rust_type(
                    ty,
                    &format!("поле '{}' структуры '{}'", field, def.name),
                )?;
                p.ident(&format!(
                    "pub {}: {},",
                    crate::semantic::naming::normalize_lowercase_snakecase(field.clone()),
                    rust_ty
                ))
                .nl();
            }
            p.down();
            p.ident("}").nl().nl();
            // Там, где `derive` не выводится, умолчание печатается вручную - тем же
            // носителем, что и литерал инициализатора. Печатать импл всегда нельзя:
            // `clippy::derivable_impls` под `-D warnings` - отказ сборки, а проверка цели
            // гоняет ровно эти флаги.
            if !derived {
                let body =
                    crate::generator::rust::rust_struct::default_literal(&def.name, def, &model)?;
                p.ident(&format!("impl Default for {} {{", name)).nl();
                p.up();
                p.ident("fn default() -> Self {").nl();
                p.up();
                p.ident(&body).nl();
                p.down();
                p.ident("}").nl();
                p.down();
                p.ident("}").nl().nl();
            }
        }
    }
    Ok(())
}

/// Печатает константы уровня модуля.
pub(crate) fn emit_constants(
    p: &mut Printer,
    map: &RustMap,
    blocks: &[(Name, std::rc::Rc<std::cell::RefCell<ModelNode>>)],
) -> Result<(), Diagnostic> {
    let mut seen: BTreeSet<String> = BTreeSet::new();
    for (_, model_rc) in blocks {
        let model = model_rc.borrow();
        let scope = Scope {
            model: &model,
            shared: Vec::new(),
            shared_via_self: false,
            locals: Vec::new(),
            // По ссылке в методах модели ничего не приходит: массивы там - поля.
            by_ref: Vec::new(),
            assigned: BTreeSet::new(),
            hal: String::new(),
            has_self: false,
            hal_is_ref: false,
            instances: Vec::new(),
            time_profile: map.time_profile(),
            return_type: None,
            // Подсказка о приёмнике степени ставится в `coerce_to`.
            power_target: None,
            guard_enable: map.guard_enable(),
        };
        for var in model.variables.values() {
            let VariableNode::Const {
                upper,
                name,
                ty,
                expr,
                loc,
            } = var
            else {
                continue;
            };
            // Ключ фильтра - пара (владелец, имя), а не голое имя: по голому `A::K` и
            // `B::K` делят запись, и неиспользуемая тёзка печаталась бы вслед за
            // используемой - а неиспользуемая `const` в Rust под `-D warnings` это
            // Отказ сборки, не предупреждение.
            if !map
                .usage()
                .constants
                .contains(&crate::semantic::unused::const_key(upper.as_ref(), name))
            {
                continue;
            }
            // Имя объявления - то же, которым к константе обращаются выражения
            // (`rust_expr::const_ident`): оно квалифицировано владельцем, иначе две
            // модели с одноимённой константой слились бы в одно `const`, и вторая молча
            // получила бы значение первой.
            let ident = const_ident(upper.as_ref(), name, *loc)?;
            if !seen.insert(ident.clone()) {
                continue;
            }
            let ty_name = rust_type(ty, &format!("константа '{}'", name))?;
            // Значение печатается по типу приёмника: у константы структурного типа
            // агрегат `{3, 4}` обязан стать литералом структуры.
            let value = crate::generator::rust::rust_expr::coerce_to(expr, ty, &scope)?;
            p.ident(&format!("const {}: {} = {};", ident, ty_name, value))
                .nl();
        }
    }
    if !seen.is_empty() {
        p.nl();
    }
    Ok(())
}

/// Значение поля по умолчанию - когда инициализатора в `.takt` нет.
///
/// Rust требует инициализировать **каждое** поле в конструкторе, поэтому умолчание
/// обязано существовать для любого представимого типа.
pub(crate) fn default_value(ty: &TypeNode, model: &ModelNode) -> Result<String, Diagnostic> {
    match ty {
        TypeNode::Bit | TypeNode::Bool => Ok("false".to_string()),
        TypeNode::Rational => Ok("0.0".to_string()),
        TypeNode::Integer { .. } => Ok("0".to_string()),
        // Бит-вектор шире 64 бит хранится массивом слов, а не массивом бит: умолчание
        // обязано совпадать с объявленным типом `[u64; K]`, иначе конструктор не
        // компилируется.
        TypeNode::Array(..) if crate::generator::rust::rust_bit::words_of_type(ty).is_some() => {
            let count = crate::generator::rust::rust_bit::words_of_type(ty)
                .expect("проверено охраной ветви");
            Ok(format!("[0u64; {count}]"))
        }
        TypeNode::Array(n, elem) => Ok(format!("[{}; {}]", default_value(elem, model)?, n)),
        // Длительность печатается как `u32` миллисекунд, формат `q(m, n)` - как `i{W}`:
        // нулевой код в обоих означает ноль величины, поэтому умолчание общее и никакой
        // арифметики не требует.
        TypeNode::Duration | TypeNode::Fixed { .. } => Ok("0".to_string()),
        // Структура умолчание получает своим `Default`: тело печатает
        // `rust_struct::default_literal` - там же, где живёт литерал инициализатора,
        // чтобы форма агрегата была одна.
        TypeNode::Struct(name) => Ok(format!(
            "{}::default()",
            crate::generator::rust::rust_name::rust_type_name(name, Location::Codegen)?
        )),
        // Умолчание перечисления - его первый вариант. Нуля у перечисления может не
        // быть вовсе (`enum Action { Idle = 670 }`), поэтому `0 as Action` было бы
        // невалидным значением, а не умолчанием.
        TypeNode::Enum(name) => {
            let def = model.search_enum(name).ok_or_else(|| {
                Diagnostic::error(
                    Location::Codegen,
                    format!("Перечисление '{}' не найдено", name),
                )
                .with_code("RS-012")
            })?;
            let first = def.variants.first().ok_or_else(|| {
                Diagnostic::error(
                    def.loc,
                    format!("Перечисление '{}' не имеет вариантов", name),
                )
                .with_code("RS-014")
            })?;
            Ok(format!(
                "{}::{}",
                rust_type_name(name, def.loc)?,
                rust_type_name(&first.0, def.loc)?
            ))
        }
        other => Err(Diagnostic::error(
            Location::Codegen,
            format!("Значение по умолчанию для типа '{}' не строится", other),
        )
        .with_code("RS-014")),
    }
}

/// Возвращает переменные модели, попадающие в её `struct` (в порядке `BTreeMap`).
///
/// Фильтр по фактическому использованию - как в цели `c`. Но если там это оптимизация,
/// то здесь **условие прохождения проверки**: приватное поле, которое никто не читает,
/// валит `-D warnings` ("field is never read").
pub(crate) fn model_fields<'a>(
    model: &'a ModelNode,
    map: &RustMap,
) -> Vec<(&'a String, &'a VariableNode)> {
    model
        .variables
        .iter()
        .filter(|(_, var)| matches!(var, VariableNode::Simple { .. }))
        .filter(|(_, var)| map.usage().variables.contains(var.name()))
        .collect()
}
