//! Автомат: `enum` состояний, `struct` модели, такт.
//!
//! ## Контракт такта - воспроизводится, а не изобретается
//!
//! **Вход в стартовое состояние не расходует такт.** Цель `c` добивается этого
//! диспетчеризацией `INIT` через `if (model->state == {PREFIX}_INIT) { ... }`
//! **до** `switch` и **без `break`** - тело стартового состояния исполняется в
//! том же такте. В Rust провала из `if` в `match` нет, но он и не нужен: `match`
//! читает **свежезаписанное** `self.state`, что даёт ровно тот же порядок.
//!
//! Контракт объявлен обязательным для будущих генераторов (`CLAUDE.md`): "вход не стоит
//! такта; не воспроизводить чужие `INIT`-такты".
//!
//! ## Где Rust расходится с C - и почему это правильно
//!
//! | C | Rust | Причина |
//! |---|---|---|
//! | `if (c1) {...break;} if (c2) {...break;}` | `if c1 {...} else if c2 {...}` | `break` в C = "такт окончен", то есть второй `if` при сработавшем первом недостижим. `else if` выражает то же, но **без** недостижимого кода, который валит `-D warnings`  |
//! | безусловный переход + следующие `ref` за ним | эмиссия рёбер прекращается | там же: код за безусловным переходом недостижим |
//! | `_TICK`, `_END` у составных состояний | не эмитятся | в C они мертвы и молча (поле пишется, но не читается); в Rust `dead_code` это ловит., вариант (а) |
//! | под-модель получает указатель `main` | корневые переменные - параметры `&mut` | `self.cabin.tick(&mut self)` заимствовал бы `self` дважды. Заимствования непересекающихся **полей** законны, поэтому `self.cabin.tick(&mut self.hal, &mut self.command)` собирается |

use crate::diagnostics::{Diagnostic, Location};
use crate::generator::indent::Printer;
use crate::generator::rust::rust_chain::{Chain, model_concats, seq_enum_name, seq_field_name};
use crate::generator::rust::rust_ctx::ModelEmit;
use crate::generator::rust::rust_decl::{PortSet, default_value, model_fields};
use crate::generator::rust::rust_expr::{Scope, coerce_to};
use crate::generator::rust::rust_fields;
use crate::generator::rust::rust_map::RustMap;
use crate::generator::rust::rust_name::{check_name_collisions, rust_type_name, rust_value_name};
use crate::generator::rust::rust_port_init;
use crate::generator::rust::rust_tick::emit_tick;
use crate::generator::rust::rust_time;
use crate::generator::rust::rust_type::rust_type;
use crate::semantic::minimap::{Element, Name, StateExtend};
use crate::semantic::{ModelNode, VariableNode};
use std::collections::{BTreeMap, BTreeSet};

/// Экземпляр под-модели, лежащий полем в `struct` родителя.
pub(crate) struct Instance {
    /// Имя поля (`main_cabin0`).
    pub(crate) field: String,
    /// Имя типа под-модели (`ElevatorMiniCabin`).
    pub(crate) ty: String,
    /// Уникальное имя модели в карте - для поиска её общих переменных.
    pub(crate) unique: String,
    /// Аргументы инстанцирования этого экземпляра.
    ///
    /// Применяются **дважды** - в `new()` и в `init()`: цель `c` присваивает их после
    /// `_init`, а у `rust` конструктор и сброс - разные функции, и расхождение между
    /// ними дало бы разные значения у одного экземпляра в зависимости от того, как его
    /// создали.
    pub(crate) args: Vec<crate::semantic::extend::ParameterArgument>,
}

/// Таблица состояний модели: имя варианта `enum` для каждого состояния.
pub(crate) struct StateTable {
    /// Имя типа перечисления состояний (`ElevatorMiniCabinState`).
    pub(crate) enum_name: String,
    /// Достижимые состояния: уникальное имя -> имя варианта.
    variants: Vec<(Name, String)>,
    /// Нужно ли эмитить собственный вариант `End`.
    ///
    /// Если у автора есть состояние `End`, оно даёт вариант `End` само - второй был бы
    /// дубликатом (та же развилка, что `end_already_generated` в `c_header.rs`).
    pub(crate) emit_end: bool,
}

impl StateTable {
    /// Строит таблицу по достижимым состояниям модели.
    pub(crate) fn build(map: &RustMap, name: &Name, states: &[Name]) -> Result<Self, Diagnostic> {
        let mut variants = Vec::new();
        for state in states {
            if map.state_at(state.clone()).is_none() {
                // Недостижимое состояние варианта не получает: неконструируемый вариант
                // валит `-D warnings` (dead_code)., вариант (а).
                continue;
            }
            variants.push((
                state.clone(),
                rust_type_name(state.local(), Location::Codegen)?,
            ));
        }
        let named: Vec<(String, String)> = variants
            .iter()
            .map(|(n, v)| (n.local().to_string(), v.clone()))
            .collect();
        check_name_collisions(&named, "состояния модели", Location::Codegen)?;

        let emit_end = !variants.iter().any(|(_, v)| v == "End");
        Ok(Self {
            enum_name: format!("{}State", name.unique_camelcase()),
            variants,
            emit_end,
        })
    }

    /// Имя варианта для состояния.
    pub(crate) fn variant_of(&self, state: &Name) -> Result<String, Diagnostic> {
        self.variants
            .iter()
            .find(|(n, _)| n.unique() == state.unique())
            .map(|(_, v)| v.clone())
            .ok_or_else(|| {
                Diagnostic::error(
                    Location::Codegen,
                    format!("Состояние '{}' недостижимо и варианта не имеет", state),
                )
                .with_code("RS-013")
            })
    }

    /// Полный путь к варианту (`ElevatorMiniCabinState::Idle`).
    pub(crate) fn path_of(&self, state: &Name) -> Result<String, Diagnostic> {
        Ok(format!("{}::{}", self.enum_name, self.variant_of(state)?))
    }

    /// Путь к терминальному варианту.
    pub(crate) fn end_path(&self) -> String {
        format!("{}::End", self.enum_name)
    }
}

/// Печатает перечисление состояний модели.
///
/// Перечисление **приватно** - в отличие от пользовательских перечислений. Это не
/// мелочь: проба 2026-07-16 показала, что `pub enum` с неконструируемым вариантом `-D
/// warnings` **проходит**, а приватный - нет. То есть публичность здесь была бы
/// вариантом (б)   ("заглушить линт") в маскировке. Состояния придумывает
/// генератор, поэтому тест `dead_code` над ними должен остаться живым:
/// неконструируемый вариант = дефект эмиссии.
pub(crate) fn emit_state_enum(p: &mut Printer, table: &StateTable) -> Result<(), Diagnostic> {
    p.ident("#[derive(Debug, Clone, Copy, PartialEq, Eq)]").nl();
    p.ident(&format!("enum {} {{", table.enum_name)).nl();
    p.up();
    p.ident("Init,").nl();
    for (_, variant) in &table.variants {
        p.ident(&format!("{},", variant)).nl();
    }
    if table.emit_end {
        p.ident("End,").nl();
    }
    p.down();
    p.ident("}").nl().nl();
    Ok(())
}

/// Печатает перечисления шагов последовательных композиций модели.
///
/// Приватны - как и перечисление состояний, и по той же причине: их придумывает
/// генератор, поэтому `dead_code` над ними обязан остаться тестом.
///
/// Варианты `Init`/`End`, которые цель `c` эмитит у составного состояния
/// (`{STATE}_INIT`, `{STATE}_END`), здесь **не эмитятся**: в C они мертвы - `_init`
/// сразу ставит вариант первого шага, а `End` не пишется никогда. В C это молча, в Rust
/// `dead_code` поймал бы (, вариант (а)).
fn emit_seq_enums(p: &mut Printer, model: &Name, concats: &[Chain]) -> Result<(), Diagnostic> {
    for chain in concats {
        p.ident("#[derive(Debug, Clone, Copy, PartialEq, Eq)]").nl();
        p.ident(&format!(
            "enum {} {{",
            seq_enum_name(model, &chain.state, &chain.path)?
        ))
        .nl();
        p.up();
        for step in &chain.steps {
            p.ident(&format!("{},", step.variant)).nl();
        }
        // У вложенной цепочки есть терминальный вариант: параллель обязана знать, что
        // ветвь кончилась, - у цепочки состояния роль признака играет выход из самого
        // состояния.
        if chain.nested() {
            p.ident("Done,").nl();
        }
        p.down();
        p.ident("}").nl().nl();
    }
    Ok(())
}

/// Собирает экземпляры под-моделей состояния в плоский список полей.
///
/// Цель `c` строит вложенные анонимные структуры (`main.cabin0`), потому что ей нужно
/// место под enum составного состояния. Здесь поля плоские: enum последовательной
/// композиции живёт отдельным полем, а параллельной - не нужен вовсе (в C он мёртв:
/// пишется в `_init` и не читается никогда).
pub(crate) fn collect_instances(
    extend: &StateExtend,
    prefix: &str,
    out: &mut Vec<Instance>,
) -> Result<(), Diagnostic> {
    match extend {
        StateExtend::None => Ok(()),
        StateExtend::Model(name, args) => {
            out.push(Instance {
                field: rust_value_name(prefix, Location::Codegen)?,
                ty: name.unique_camelcase(),
                unique: name.unique().to_string(),
                args: args.clone(),
            });
            Ok(())
        }
        StateExtend::Parallel(steps) | StateExtend::Concatenation(steps) => {
            for (idx, step) in steps.iter().enumerate() {
                let sub = match step {
                    StateExtend::Model(name, _) => {
                        format!("{}_{}{}", prefix, name.local_lowercase_snakecase(), idx)
                    }
                    _ => format!("{}_group{}", prefix, idx),
                };
                collect_instances(step, &sub, out)?;
            }
            Ok(())
        }
    }
}

/// Все экземпляры под-моделей модели - по одному на элемент композиции.
pub(crate) fn model_instances(
    map: &RustMap,
    states: &[Name],
) -> Result<Vec<(Name, Vec<Instance>)>, Diagnostic> {
    let mut out = Vec::new();
    for state in states {
        let Some(Element::StateExtend { extend, .. }) = map.state_at(state.clone()) else {
            continue;
        };
        let mut instances = Vec::new();
        collect_instances(&extend, &state.local_lowercase_snakecase(), &mut instances)?;
        if !instances.is_empty() {
            out.push((state.clone(), instances));
        }
    }
    Ok(out)
}

use crate::generator::rust::rust_shared::{
    emit_shared_new_block, emit_shared_struct, shared_type_name, shared_union, shared_variables,
    union_names as shared_union_names,
};

/// Печатает `struct` модели и её `impl`.
pub(crate) fn emit_model(
    p: &mut Printer,
    map: &RustMap,
    name: &Name,
    model: &ModelNode,
    is_root: bool,
    ports: &PortSet,
    warnings: &mut Vec<Diagnostic>,
) -> Result<(), Diagnostic> {
    let element = if is_root {
        map.model()
    } else {
        map.element_of(name).ok_or_else(|| {
            Diagnostic::error(
                Location::Codegen,
                format!("Модель '{}' отсутствует в снимке карты", name),
            )
            .with_code("RS-012")
        })?
    };
    let Element::Model { states, start, .. } = &element else {
        return Err(Diagnostic::error(
            Location::Codegen,
            format!("Элемент '{}' не является моделью", name),
        )
        .with_code("RS-012"));
    };

    let table = StateTable::build(map, name, states)?;
    let instances = model_instances(map, states)?;
    let concats = model_concats(map, states)?;
    // Корень владеет структурой `Shared` (объединение нужд под-моделей); его
    // scope.shared - весь союз (для доступа `self.shared.x`). Под-модель получает `&mut
    // Shared` и разделяет лишь свою часть.
    let shared = if is_root {
        shared_union(map)
    } else {
        shared_variables(map, name)
    };
    let union_names = shared_union_names(map, is_root);
    // HAL нужен модели, только если она к нему обращается - сама либо через под-модель.
    // Класть `hal: H` в модель, которая его не читает, нельзя: `field 'hal' is never
    // read` валит проверка.
    let uses_hal = !ports.is_empty() && needs_hal(map, name, is_root, &mut BTreeSet::new());
    let struct_name = name.unique_camelcase();

    emit_state_enum(p, &table)?;
    emit_seq_enums(p, name, &concats)?;

    // -- struct Shared ---------------------------------------------- Эмиссия - в
    // `rust_shared` (приватная структура); у корня и только если под-моделям есть что
    // разделять ().
    if is_root {
        emit_shared_struct(p, map, &shared)?;
    }

    // -- struct ---------------------------------------------------------------
    // Объявление параметра и его подстановка - Разные строки: граница пишется один раз
    // (`impl<H: Hal>`), а в позиции типа стоит голое имя (`ElevatorMini<H>`). Повторить
    // границу в аргументах - ошибка E0229.
    let generics = if is_root && uses_hal { "<H: Hal>" } else { "" };
    let type_args = if is_root && uses_hal { "<H>" } else { "" };
    // Комментарий автора перед объявлением модели: их в корпусе больше всего - 103 из
    // 469.
    if let Ok(model_rc) = map.raw_model_at(name.clone()) {
        let loc = model_rc.borrow().loc;
        for line in crate::generator::comments::leading(
            loc,
            crate::generator::header::CommentStyle::Slashes,
        ) {
            p.ident(&line).nl();
        }
    }
    p.ident(&format!("pub struct {}{} {{", struct_name, generics))
        .nl();
    let _ = &type_args;
    p.up();
    // Имена, которые цель напечатает в эту структуру сама. Набор строится до печати
    // переменных: отказ обязан нести координату объявления, а у служебного поля позиции
    // нет. Порядок печати не меняется - снимки `examples/generated/` сверяются
    // побайтно.
    let mut fields = rust_fields::Fields::service(
        map,
        model,
        &instances
            .iter()
            .flat_map(|(_, list)| list)
            .collect::<Vec<_>>(),
        &concats,
        is_root && !shared.is_empty(),
        is_root && uses_hal,
    )?;
    for (_, var) in model_fields(model, map) {
        let VariableNode::Simple {
            name: vname,
            ty,
            loc,
            ..
        } = var
        else {
            continue;
        };
        // Общая переменная уезжает в поле `shared` - прямым полем не остаётся.
        if union_names.contains(vname) {
            continue;
        }
        let field = rust_value_name(vname, *loc)?;
        fields.claim(vname, &field, *loc)?;
        p.ident(&format!(
            "{}: {},",
            field,
            rust_type(ty, &format!("переменная '{}'", vname))?
        ))
        .nl();
    }
    if is_root && !shared.is_empty() {
        p.ident(&format!("shared: {},", shared_type_name(map))).nl();
    }
    p.ident(&format!("state: {},", table.enum_name)).nl();
    // Поля механизма времени: счётчик тактов / метка `now_ms` / предыдущее состояние -
    // только при использовании `after` (иначе `-D warnings` упадёт на неиспользуемом
    // поле). Логика в `rust_time`.
    rust_time::emit_struct_fields(p, map, model, &table.enum_name)?;
    crate::generator::rust::rust_every::emit_struct_fields(p, map, model)?;
    for chain in &concats {
        p.ident(&format!(
            "{}: {},",
            seq_field_name(&chain.state, &chain.path)?,
            seq_enum_name(name, &chain.state, &chain.path)?
        ))
        .nl();
    }
    for (_, list) in &instances {
        for instance in list {
            p.ident(&format!("{}: {},", instance.field, instance.ty))
                .nl();
        }
    }
    if is_root && uses_hal {
        p.ident("hal: H,").nl();
    }
    p.down();
    p.ident("}").nl().nl();

    // -- impl -----------------------------------------------------------------
    p.ident(&format!("impl{} {}{} {{", generics, struct_name, type_args))
        .nl();
    p.up();
    // Снимок "что печатаем": набор назван один раз, и печатники берут его целиком.
    let ctx = ModelEmit {
        map,
        name,
        model,
        element: &element,
        table: &table,
        instances: &instances,
        concats: &concats,
        states,
        start,
        shared: &shared,
        is_root,
        uses_hal,
        ports,
    };
    emit_new(p, &ctx)?;
    emit_init(p, &ctx)?;
    emit_tick(p, &ctx, warnings)?;
    emit_reset(p, is_root)?;
    emit_is_done(p, &table, is_root)?;
    p.down();
    p.ident("}").nl().nl();
    // Данные таблицы живут на уровне модуля: тип строки не зависит от HAL, а `static`
    // не пересобирается на каждом такте.
    if map.fsm_table() {
        crate::generator::rust::rust_table::emit_data(p, &ctx)?;
    }
    emit_default_impl(p, &struct_name, is_root, uses_hal);
    Ok(())
}

/// Нужен ли модели доступ к HAL - **транзитивно**.
///
/// Транзитивность здесь обязательна, а не желательна. Модель может не иметь ни одного
/// порта и всё же нуждаться в `hal`: если её под-модель к железу обращается, родитель
/// обязан HAL **пронести**. Ровно так устроен `elevator_mini`, где все порты объявлены
/// в `Cabin`/`Motor`, а корень - только композиция.
///
/// Обратное так же важно: дать `hal` модели, которая его не трогает, нельзя - у корня
/// это `field 'hal' is never read`, у под-модели `unused variable: hal`. И то и другое
/// валит проверка. То есть точность этого предиката - условие прохождения `-D warnings`, а
/// не аккуратность.
pub(crate) fn needs_hal(
    map: &RustMap,
    name: &Name,
    is_root: bool,
    seen: &mut BTreeSet<String>,
) -> bool {
    if !seen.insert(name.unique().to_string()) {
        // Цикл невозможен (модель не содержит саму себя), но защита дешевле
        // доказательства.
        return false;
    }
    // Собственные порты и вызовы, требующие HAL, - прямая нужда.
    if let Ok(model_rc) = map.raw_model_at(name.clone()) {
        let model = model_rc.borrow();
        let usage_of_model = crate::semantic::unused::compute_usage(std::rc::Rc::clone(&model_rc));
        // Порт, которого модель касается, а не который она объявляет. Разница не
        // теоретическая: `stacker` пишет `cmd_fork`, объявленный в корне, из
        // под-модели. Проверка по объявлениям давала бы "HAL не нужен", и запись
        // печаталась бы в пустоту - `RS-022` на ровном месте.
        let has_ports = !usage_of_model.ports.is_empty();
        // Вызовы считаются по фактическому использованию именно этой модели, а не по
        // всему файлу: иначе `debug` в одной модели потянул бы `hal` во все.
        //
        // Важно: имя ищется через `search_func`, который поднимается по цепочке
        // родителей. `extern fn` объявлен в корне, а вызывает его под-модель (так
        // устроен `comprehensive.takt`) - проверка только собственной таблицы функций
        // дала бы "HAL не нужен", и вызов напечатался бы в пустоту (`.log_temp(x)` без
        // получателя).
        let usage = &usage_of_model;
        // Нужда вызываемых функций - тоже нужда модели: если `travel_time` читает порт,
        // то вызывающая её модель обязана иметь `hal`, чтобы было что передать.
        // Считается тем же предикатом, что и сигнатура функции.
        let needs_call = usage.functions.iter().any(|fname| {
            crate::generator::rust::rust_needs::needs_of_call(fname, &model, &mut BTreeSet::new())
                .map(|needs| needs.hal)
                .unwrap_or(false)
        });
        // Выдержка `after Nms` в профиле "часы" зовёт `now_ms` - метод HAL.
        let needs_time = rust_time::needs_entry_ms(map, &model);
        if has_ports || needs_call || needs_time {
            return true;
        }
    }
    // Нужда под-моделей - тоже нужда: HAL придётся пронести через себя.
    let element = if is_root {
        map.model()
    } else {
        match map.element_of(name) {
            Some(element) => element,
            None => return false,
        }
    };
    let Element::Model { states, .. } = &element else {
        return false;
    };
    let Ok(instances) = model_instances(map, states) else {
        return false;
    };
    for (_, list) in instances {
        for instance in list {
            let Some(sub) = submodel_name(map, &instance.unique) else {
                continue;
            };
            if needs_hal(map, &sub, false, seen) {
                return true;
            }
        }
    }
    false
}

/// Нужен ли HAL именно такту модели.
///
/// Отличие от [`needs_hal`] - в объёме: здесь считаются только **тела** (блоки
/// состояний, условия рёбер, именованные условия, функции), без инициализаторов
/// объявлений. Начальное значение выходного порта пишет конструктор корня, и параметр
/// `hal` в `tick` под-модели после этого не используется - `rustc` под `-D warnings`
/// отвечает "unused variable: `hal`" при нулевом коде возврата `taktc`.
///
/// Признак **на функцию**, а не на модель - тот же приём, что у цели `c`: сигнатуры
/// `init` и `tick` после этого расходятся, и это законно.
pub(crate) fn needs_hal_in_tick(map: &RustMap, name: &Name, seen: &mut BTreeSet<String>) -> bool {
    if !seen.insert(name.unique().to_string()) {
        return false;
    }
    if let Ok(model_rc) = map.raw_model_at(name.clone()) {
        let model = model_rc.borrow();
        let usage = crate::semantic::unused::body_usage(&model_rc);
        let needs_call = usage.functions.iter().any(|fname| {
            crate::generator::rust::rust_needs::needs_of_call(fname, &model, &mut BTreeSet::new())
                .map(|needs| needs.hal)
                .unwrap_or(false)
        });
        if !usage.ports.is_empty() || needs_call || rust_time::needs_entry_ms(map, &model) {
            return true;
        }
        // Дети по вызову: и объявленные внутри, и те, которыми реализованы состояния
        // (`= M`, `A | B`, `A + B`) - их `tick` зовёт этот же `tick` и передаёт им HAL.
        let mut children: Vec<std::rc::Rc<std::cell::RefCell<crate::semantic::ModelNode>>> =
            model.models.values().cloned().collect();
        children.extend(crate::semantic::extend::implementation_children(&model));
        drop(model);
        for child in children {
            let child_name = child.borrow().name.clone().unwrap_or_default();
            // Сперва по уникальному имени, и лишь потом по локальному (фича
            // 0469): одноимённых моделей в карте бывает несколько - файл
            // `helper.takt` вносится как модель `Helper`, а внутри него
            // объявлена модель с тем же именем. Поиск по локальному имени
            // находил обёртку, её `unique` уже лежал в `seen`, и признак
            // отвечал "HAL не нужен": цель печатала `self.root.tick(&mut
            // *hal)` в функции без параметра `hal` - `E0425` при нулевом коде
            // возврата `taktc`.
            //
            // Запасной путь по локальному имени оставлен: ребёнок по вызову (`= M`, `A |
            // B`) живёт в карте под своим уникальным именем, а не под
            // `<родитель>:<имя>`.
            let exact = submodel_name(map, &format!("{}:{}", name.unique(), child_name));
            let found = exact.or_else(|| {
                map.using_models()
                    .into_iter()
                    .find_map(|element| match element {
                        Element::Model { name, .. } if name.local() == child_name => Some(name),
                        _ => None,
                    })
            });
            if let Some(sub) = found
                && needs_hal_in_tick(map, &sub, seen)
            {
                return true;
            }
        }
    }
    false
}

/// Ищет имя под-модели в карте по её уникальному имени.
pub(crate) fn submodel_name(map: &RustMap, unique: &str) -> Option<Name> {
    map.using_models()
        .into_iter()
        .find_map(|element| match element {
            Element::Model { name, .. } if name.unique() == unique => Some(name),
            _ => None,
        })
}

/// Печатает конструктор.
fn emit_new(p: &mut Printer, ctx: &ModelEmit) -> Result<(), Diagnostic> {
    let (map, model, table, instances, concats, model_name, is_root, uses_hal) = (
        ctx.map,
        ctx.model,
        ctx.table,
        ctx.instances,
        ctx.concats,
        ctx.name,
        ctx.is_root,
        ctx.uses_hal,
    );
    let scope = Scope {
        model,
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
    let args = if is_root && uses_hal { "hal: H" } else { "" };
    let vis = if is_root { "pub " } else { "" };
    // Общие переменные корня инициализируются внутри блока `shared { ... }`. Собираем
    // их значения, прямые поля печатаем сразу.
    let union = if is_root {
        shared_union(map)
    } else {
        Vec::new()
    };
    let union_names: BTreeSet<String> = union.iter().map(|(n, _)| n.clone()).collect();
    let mut shared_inits: BTreeMap<String, String> = BTreeMap::new();
    // Начальные значения портов: выставляет корень - у под-модели доступа к HAL в
    // конструкторе нет. Пусто у всех, кроме моделей с `:=` в объявлении порта, поэтому
    // форма `new()` в корпусе не меняется.
    let port_writes = if is_root {
        rust_port_init::port_initial_writes(map, model)?
    } else {
        Vec::new()
    };
    p.ident(&format!("{}fn new({}) -> Self {{", vis, args)).nl();
    p.up();
    // Значение уходит наружу через HAL, а он попадает в структуру только вместе со
    // `Self`: пишем после конструирования, по временной привязке.
    p.ident(if port_writes.is_empty() {
        "Self {"
    } else {
        "let mut this = Self {"
    })
    .nl();
    p.up();
    for (_, var) in model_fields(model, map) {
        let VariableNode::Simple {
            name: vname,
            ty,
            expr,
            loc,
            ..
        } = var
        else {
            continue;
        };
        // Объявление объявляет своё место: отказ печати инициализатора рождается вне
        // операторов и печатался без координаты.
        crate::generator::site::enter_declaration(*loc);
        let value = match expr {
            crate::semantic::ExpressionNode::None => default_value(ty, model)?,
            other => coerce_to(other, ty, &scope)?,
        };
        if union_names.contains(vname) {
            shared_inits.insert(vname.clone(), value);
        } else {
            p.ident(&format!("{}: {},", rust_value_name(vname, *loc)?, value))
                .nl();
        }
    }
    // Слой объявления снимается парно входу.
    crate::generator::site::leave_declaration();
    if is_root {
        emit_shared_new_block(p, map, &union, &shared_inits)?;
    }
    p.ident(&format!("state: {}::Init,", table.enum_name)).nl();
    // Начальные значения полей времени: метку латчим не здесь, а в INIT-диспетчере
    // такта (в конструкторе HAL под-модели недоступен).
    rust_time::emit_new_fields(p, map, model, &table.enum_name)?;
    crate::generator::rust::rust_every::emit_new_fields(p, model);
    // Счётчик шага стартует с первого шага, а не с "Init": так же поступает `_init`
    // цели `c` (её варианты `{STATE}_INIT`/`_END` не пишутся никогда).
    for chain in concats {
        let first = chain.steps.first().ok_or_else(|| {
            Diagnostic::error(
                Location::Codegen,
                format!("Состояние '{}': композиция без шагов", chain.state.local()),
            )
            .with_code("RS-021")
        })?;
        p.ident(&format!(
            "{}: {}::{},",
            seq_field_name(&chain.state, &chain.path)?,
            seq_enum_name(model_name, &chain.state, &chain.path)?,
            first.variant
        ))
        .nl();
    }
    for (_, list) in instances {
        for instance in list {
            let assignments = argument_assignments(map, instance, &scope)?;
            if assignments.is_empty() {
                p.ident(&format!("{}: {}::new(),", instance.field, instance.ty))
                    .nl();
                continue;
            }
            // Аргументы применяются и здесь: `new()` и `init()` - разные входы, и
            // разойдясь, они дали бы одному экземпляру разные значения в зависимости от
            // того, как его создали.
            p.ident(&format!("{}: {{", instance.field)).up().nl();
            p.ident(&format!("let mut instance = {}::new();", instance.ty))
                .nl();
            for assignment in assignments {
                p.ident(&format!("instance.{}", assignment)).nl();
            }
            p.ident("instance").nl();
            p.down().ident("},").nl();
        }
    }
    if is_root && uses_hal {
        p.ident("hal,").nl();
    }
    p.down();
    if port_writes.is_empty() {
        p.ident("}").nl();
    } else {
        p.ident("};").nl();
        for write in &port_writes {
            p.ident(&format!("this.hal.{}", write)).nl();
        }
        p.ident("this").nl();
    }
    p.down();
    p.ident("}").nl().nl();
    Ok(())
}

/// Печатает `impl Default` рядом с публичным `new()` **без аргументов**.
///
/// Условие ровно то же, при котором срабатывает `clippy::new_without_default`:
/// конструктор **публичен** (то есть корневой) и аргументов не имеет (то есть HAL
/// модели транзитивно не нужен - ни портов, ни `extern fn`). Под `-D warnings` проверки
/// цели `rust` такой вывод иначе красный, и пользователь с той же политикой линтов не
/// собрал бы порождённый код вовсе.
///
/// При `uses_hal` печатать нельзя и не нужно: `new(hal: H)` аргумент имеет - линт
/// молчит, - а `Default::default()` и не построить, значение `H` взять неоткуда.
///
/// Выбран `impl Default`, а не `#[allow(clippy::new_without_default)]`: политика R9 -
/// "не эмитить то, на что линт ругается", тест обязан остаться живым. Заглушив линт
/// атрибутом, мы спрятали бы и будущие срабатывания.
fn emit_default_impl(p: &mut Printer, struct_name: &str, is_root: bool, uses_hal: bool) {
    if !is_root || uses_hal {
        return;
    }
    p.ident(&format!("impl Default for {struct_name} {{")).nl();
    p.up();
    p.ident("fn default() -> Self {").nl();
    p.up();
    p.ident("Self::new()").nl();
    p.down();
    p.ident("}").nl();
    p.down();
    p.ident("}").nl().nl();
}

/// Печатает `init` - приведение памяти в начальное состояние.
///
/// Блоков `enter` здесь нет **намеренно**: по  в `_init` живёт только память, а
/// поведение входа - в такте. Иначе вход в стартовое состояние стоил бы такта, и трасса
/// разошлась бы с симулятором.
fn emit_init(p: &mut Printer, ctx: &ModelEmit) -> Result<(), Diagnostic> {
    let (map, model, table, instances, concats, model_name, is_root) = (
        ctx.map,
        ctx.model,
        ctx.table,
        ctx.instances,
        ctx.concats,
        ctx.name,
        ctx.is_root,
    );
    let scope = Scope {
        model,
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
    let vis = if is_root { "pub " } else { "" };
    let union_names = shared_union_names(map, is_root);
    p.ident(&format!("{}fn init(&mut self) {{", vis)).nl();
    p.up();
    for (_, var) in model_fields(model, map) {
        let VariableNode::Simple {
            name: vname,
            ty,
            expr,
            loc,
            ..
        } = var
        else {
            continue;
        };
        let value = match expr {
            crate::semantic::ExpressionNode::None => default_value(ty, model)?,
            other => coerce_to(other, ty, &scope)?,
        };
        // Общая переменная живёт в `self.shared`.
        let target = if union_names.contains(vname) {
            format!("self.shared.{}", rust_value_name(vname, *loc)?)
        } else {
            format!("self.{}", rust_value_name(vname, *loc)?)
        };
        p.ident(&format!("{} = {};", target, value)).nl();
    }
    p.ident(&format!("self.state = {}::Init;", table.enum_name))
        .nl();
    // Сброс полей времени: в 0 / `Init`. Метку латчит INIT-диспетчер такта - `init(&mut
    // self)` под-модели HAL не имеет.
    rust_time::emit_init(p, map, model, &table.enum_name);
    crate::generator::rust::rust_every::emit_reset(p, model);
    for chain in concats {
        let first = chain.steps.first().ok_or_else(|| {
            Diagnostic::error(
                Location::Codegen,
                format!("Состояние '{}': композиция без шагов", chain.state.local()),
            )
            .with_code("RS-021")
        })?;
        p.ident(&format!(
            "self.{} = {}::{};",
            seq_field_name(&chain.state, &chain.path)?,
            seq_enum_name(model_name, &chain.state, &chain.path)?,
            first.variant
        ))
        .nl();
    }
    // Инициализация вложенных - здесь (0033, R6): чтение поля до первого `tick` не
    // должно давать мусор.
    //
    // Инициализируются все экземпляры, включая шаги композиции, которые ещё не
    // наступили. Цель `c` в `_init` трогает только первый шаг, но расхождения нет: шаг
    // не тикает до своей очереди, а при передаче хода его `init` вызывается заново -
    // как и в C.
    for (_, list) in instances {
        for instance in list {
            p.ident(&format!("self.{}.init();", instance.field)).nl();
            // Настройка места инстанцирования - после `init()` экземпляра: та же
            // последовательность, что у цели `c` (присваивание после `_init`).
            for assignment in argument_assignments(map, instance, &scope)? {
                p.ident(&format!("self.{}.{}", instance.field, assignment))
                    .nl();
            }
        }
    }
    // Начальные значения портов - и здесь тоже: `new()` и `init()` разные входы, и
    // порт, выставленный лишь в одном, при сбросе разошёлся бы с целью `c` (там
    // `_reset` зовёт `_init`).
    if is_root {
        for write in rust_port_init::port_initial_writes(map, model)? {
            p.ident(&format!("self.hal.{}", write)).nl();
        }
    }
    p.down();
    p.ident("}").nl().nl();
    Ok(())
}

/// Присваивания аргументов инстанцирования полям экземпляра.
///
/// Значение приводится к типу параметра тем же `coerce_to`, что и инициализатор
/// объявления: печатать "как есть" значило бы завести вторую трактовку типа.
fn argument_assignments(
    map: &RustMap,
    instance: &Instance,
    scope: &Scope,
) -> Result<Vec<String>, Diagnostic> {
    let mut out = Vec::new();
    if instance.args.is_empty() {
        return Ok(out);
    }
    // Тип параметра берётся у целевой модели: значение приводится к нему, а не
    // печатается "как есть" - иначе у аргумента завелась бы вторая трактовка типа.
    let target = map.model_node_by_unique(&instance.unique);
    for arg in &instance.args {
        let ty = target
            .as_ref()
            .and_then(|m| m.borrow().variables.get(&arg.name).map(|v| v.ty().clone()))
            .ok_or_else(|| {
                Diagnostic::error(
                    arg.loc,
                    format!(
                        "Параметр '{}' модели '{}' не найден при печати аргумента",
                        arg.name, instance.unique
                    ),
                )
                .with_code("RS-024")
            })?;
        let value = coerce_to(&arg.value, &ty, scope)?;
        out.push(format!(
            "{} = {};",
            rust_value_name(&arg.name, Location::Codegen)?,
            value
        ));
    }
    Ok(out)
}

/// Печатает `reset` - паритет с целью `c`, где `_reset` вызывает `_init`.
///
/// Эмитится **только корню**. Цель `c` заводит `_reset` каждой модели, но сбрасывать
/// под-модель по отдельности никто не может и не должен: сброс корня и так доходит до
/// вложенных через `init`. В C такой `_reset` - просто мёртвая функция, и это молча; в
/// Rust `dead_code` ловит её и валит проверка ("method `reset` is never used").
///
/// Ещё один случай, вариант (а): не эмитить то, чего не бывает. Заглушить
/// линт было бы дешевле на одну строку - и на этом закончился бы тест, который эту
/// находку и принёс.
fn emit_reset(p: &mut Printer, is_root: bool) -> Result<(), Diagnostic> {
    if !is_root {
        return Ok(());
    }
    p.ident("pub fn reset(&mut self) {").nl();
    p.up();
    p.ident("self.init();").nl();
    p.down();
    p.ident("}").nl().nl();
    Ok(())
}

/// Печатает `is_done`.
///
/// Обращение `self.state == ...::End` - единственное место, где вариант `End`
/// упоминается у модели без терминальных состояний. Этого достаточно: проба 2026-07-16
/// показала, что упоминание варианта в сравнении считается его конструированием,
/// поэтому `dead_code` на `End` не срабатывает никогда, и специально "оживлять" его не
/// требуется.
fn emit_is_done(p: &mut Printer, table: &StateTable, is_root: bool) -> Result<(), Diagnostic> {
    let vis = if is_root { "pub " } else { "" };
    p.ident(&format!("{}fn is_done(&self) -> bool {{", vis))
        .nl();
    p.up();
    p.ident(&format!("self.state == {}", table.end_path())).nl();
    p.down();
    p.ident("}").nl().nl();
    Ok(())
}
