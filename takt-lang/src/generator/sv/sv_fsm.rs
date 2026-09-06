//! Автомат: состояния, переходы, сброс, композиция.
//!
//! ## Two-process, и это решение про семантику, а не про стиль
//!
//! `always_comb` считает следующее состояние и значения (**блокирующие** присваивания),
//! `always_ff` защёлкивает их по фронту (**неблокирующие**). Разделение воспроизводит
//! императивность тела состояния: `v := 1; w := v;` даёт `w = 1`, как в симуляторе и в
//! цели `c`. One-process дал бы `w` = старое `v` - другую модель; three-process (Мур)
//! отвергнут, потому что **модель Takt не Мур**: `enter`/`always`/`exit` суть действия
//! с побочными эффектами, привязанные к переходу.
//!
//! ## Сброс и контракт 0033: сдвиг = 0 достаётся конструктивно
//!
//! Синтетического `INIT`-состояния **нет вовсе** - его роль исполняет цепь сброса:
//! ветвь `if (!rst_n)` кладёт в регистры стартовые состояния всех уровней **одним
//! фронтом**. Поэтому первый же такт после снятия `rst_n` исполняет тело стартового
//! состояния, и сдвиг равен нулю **на любой глубине** (в цели `c` уровни входят
//! последовательно, отсюда сдвиг = глубине, и его потребовалось *убирать* правкой - ).
//!
//! Отсюда же [`SV-008`]: `enter` стартового состояния обязан быть
//! **константным**. Ветвь сброса синтезируется в цепь сброса триггеров и
//! выражений не вычисляет - вычислять там нечем.
//!
//! ## Композиция: инлайн внутрь ветви `case` родителя
//!
//! Формулировка - "логика под-моделей инлайнится в общий `always_comb` в
//! порядке вызова `_tick`" - верна по **порядку**, но умалчивает о
//! **вложенности**. Эталон (`stacker.c:425-433`) зовёт `_tick` под-моделей
//! **внутри** `case STACKER_STACKER`, то есть они работают, только пока родитель
//! в этом состоянии. Вынос наружу изменил бы модель: под-модели продолжали бы
//! работать после выхода родителя.
//!
//! ## `is_done` под-модели читает `_next`, а не регистр
//!
//! В C `_is_done` вызывается **после** `_tick` и читает `model->state` - то есть
//! значение, которое тик только что записал (присваивание в C немедленно
//! видимо). В `always_comb` рабочая копия - `state_next`, поэтому эквивалент -
//! `(<sub>_state_next == <SUB>_END)`. Чтение регистра дало бы значение
//! **предыдущего** такта, то есть ровно тот сдвиг, который осуждает.

use crate::diagnostics::Diagnostic;
use crate::generator::indent::Printer;
use crate::generator::sv::sv_blocks::{emit_model_prelude, emit_named_blocks, emit_state_prelude};
use crate::generator::sv::sv_const;
use crate::generator::sv::sv_expr::sv002;
use crate::generator::sv::sv_expr::{Scope, print_condition};
use crate::generator::sv::sv_map::SvMap;
use crate::generator::sv::sv_module::{SvPorts, check_sv_name};
use crate::generator::sv::sv_names::{step_enum_name, step_reg_name, step_variant};
use crate::generator::sv::sv_time;
use crate::generator::sv::sv_type::sv_type;
use crate::semantic::minimap::{Element, Name, StateExtend};
use crate::semantic::{ModelNode, StateNode, VariableNode};
use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet};
use std::rc::Rc;

/// Блок модели: имя в карте и её узел.
pub(crate) type Block = (Name, Rc<RefCell<ModelNode>>);

/// Регистровый сигнал модуля: объявление и сброс.
pub(crate) struct Reg {
    /// Имя сигнала (без `_next`).
    pub(crate) name: String,
    /// Часть объявления до имени (`logic [7:0]`, `stacker_state_e`).
    pub(crate) prefix: String,
    /// Распакованная размерность после имени (пусто либо `[0:3]`).
    pub(crate) suffix: String,
    /// Значение в ветви сброса.
    pub(crate) reset: String,
    /// Сброс и умолчание по листьям: `[(суффикс, значение)]`.
    ///
    /// Непусто там, где внутри распакованного массива лежит структура: синтезатор
    /// принимает шаблон присваивания только для массива целиком, а частичную запись
    /// поля элемента при умолчании массива целиком объявляет защёлкой. Тогда и сброс, и
    /// умолчание печатаются по полям.
    pub(crate) leaves: Vec<(String, String)>,
    /// Объявлять ли сам регистр: у выходного порта он уже объявлен в заголовке.
    pub(crate) declare_reg: bool,
    /// Умолчание в `always_comb`, если оно не "остаться как есть".
    ///
    /// Такое умолчание есть у строба записи двунаправленного порта: `_we` поднят ровно
    /// в том такте, где модель писала порт, - "как есть" оставило бы его поднятым
    /// навсегда, и плата затирала бы ячейку каждый такт.
    pub(crate) default: Option<String>,
}

/// Сигналы и отображения модуля, собранные по всем уровням. Цепочка `+`, которой нужен
/// свой тип-перечисление шага.
#[derive(Debug)]
pub(crate) struct StepEnum {
    /// Несущее состояние.
    pub state: Name,
    /// Место цепочки в дереве композиции (пустое - цепочка верхнего уровня).
    pub path: Vec<usize>,
    /// Число шагов.
    pub count: usize,
    /// Нужен ли терминальный вариант (есть только у вложенной цепочки).
    pub done: bool,
}

pub(crate) struct Fsm {
    /// Печатать ли охранные формулы (`--guard-enable`).
    pub(crate) guard_enable: bool,
    /// Регистры: состояния уровней, переменные, выходные порты.
    regs: Vec<Reg>,
    /// Уровни с механизмом времени: перекрытие `_next` в `always_comb`.
    pub(crate) time_levels: Vec<sv_time::TimeLevel>,
    /// Имена регистровых сигналов (ключи для `Scope::registered`).
    pub(crate) registered: BTreeSet<String>,
    /// Имена двунаправленных портов: у них две стороны, `_i` и `_o`.
    pub(crate) inouts: BTreeSet<String>,
    /// Имя регистра состояния по уникальному имени модели.
    pub(crate) state_reg: BTreeMap<String, String>,
    /// Варианты перечислений модели - для восстановления варианта по значению.
    pub(crate) enums: BTreeMap<String, Vec<(String, i128)>>,
    /// Поля структур: `имя -> [(поле, тип)]`.
    pub(crate) structs: BTreeMap<String, Vec<(String, crate::semantic::type_node::TypeNode)>>,
    /// Цепочки `+`: место и число шагов (для эмиссии enum шага). Порядок - обхода
    /// `build`, значит детерминирован.
    pub(crate) step_enums: Vec<StepEnum>,
    /// Предупреждения генератора, собранные при печати выражений.
    ///
    /// Ячейка, а не поле-вектор: печать выражения идёт через `&Scope` (неизменяемая
    /// ссылка), а `SV-009` (переменный делитель) обнаруживается именно там - в
    /// единственной точке трансляции всех выражений. Отсюда интерьерная мутабельность:
    /// `print_expression` дописывает сюда через `scope.warnings`, а `generate_program`
    /// отдаёт их **вызывающему** - печатает CLI. Прежде здесь был `report` с
    /// `eprintln!` из библиотеки, воспроизведённый по образцу `rust`/`st` вместе с их
    /// дефектом: `--quiet` такой вывод не глушил, а формат расходился с общим.
    pub(crate) warnings: std::cell::RefCell<Vec<Diagnostic>>,
    /// Локальные переменные тел, содержащие структуру.
    ///
    /// Заполняется **печатником тел** по ходу печати (отсюда `RefCell`); правило и его
    /// причина - в [`sv_locals`](crate::generator::sv::sv_locals).
    pub(crate) hoisted_locals: crate::generator::sv::sv_locals::HoistedLocals,
}

/// Собирает аргументы инстанцирования по всем реализациям карты.
///
/// Ключ - уникальное имя модели. Уплощение цели `sv` даёт **один** набор регистров на
/// модель: два экземпляра одной модели делят их. Поэтому два разных набора аргументов у
/// одной модели невыразимы - громкий отказ `SV-016` вместо молчаливой победы
/// последнего. Одинаковые наборы законны.
fn collect_instantiation_args(
    map: &SvMap,
    blocks: &[Block],
) -> Result<BTreeMap<String, Vec<crate::semantic::extend::ParameterArgument>>, Diagnostic> {
    let mut by_model: BTreeMap<String, Vec<crate::semantic::extend::ParameterArgument>> =
        BTreeMap::new();
    let mut walk = |extend: &StateExtend| -> Result<(), Diagnostic> {
        collect_from_extend(extend, &mut by_model)
    };
    for (name, _) in blocks {
        let Some(Element::Model { states, .. }) = map.model_element_of(name) else {
            continue;
        };
        for state_name in &states {
            if let Some(Element::StateExtend { extend, .. }) = map.state_at(state_name.clone()) {
                walk(&extend)?;
            }
        }
    }
    Ok(by_model)
}

/// Рекурсивный обход реализации для [`collect_instantiation_args`].
fn collect_from_extend(
    extend: &StateExtend,
    by_model: &mut BTreeMap<String, Vec<crate::semantic::extend::ParameterArgument>>,
) -> Result<(), Diagnostic> {
    match extend {
        StateExtend::None => Ok(()),
        StateExtend::Model(name, args) => {
            match by_model.get(name.unique()) {
                None => {
                    by_model.insert(name.unique().to_string(), args.clone());
                }
                Some(existing) if existing == args => {}
                Some(_) => {
                    let loc = args
                        .first()
                        .map(|a| a.loc)
                        .unwrap_or(crate::diagnostics::Location::Codegen);
                    return Err(Diagnostic::error(
                        loc,
                        format!(
                            "Модель '{}' инстанцирована с РАЗНЫМИ наборами аргументов: \
                             цель sv уплощает композицию, и экземпляры одной модели \
                             делят одни регистры — разные настройки невыразимы. \
                             Дайте копиям разные имена моделей либо используйте \
                             другую цель",
                            name.local()
                        ),
                    )
                    .with_code("SV-016"));
                }
            }
            Ok(())
        }
        StateExtend::Parallel(items) | StateExtend::Concatenation(items) => {
            for item in items {
                collect_from_extend(item, by_model)?;
            }
            Ok(())
        }
    }
}

/// Имя регистра состояния модели.
///
/// У корня - просто `state` (форма ); у под-модели - с преисправлением её уникального имени,
/// иначе при уплощении все уровни делили бы один регистр.
fn state_reg_name(name: &Name, root: &Name) -> String {
    if name.unique() == root.unique() {
        "state".to_string()
    } else {
        format!("{}_state", name.unique_lowercase_snakecase())
    }
}

/// Имя типа-перечисления состояний модели.
pub(crate) fn state_enum_name(name: &Name) -> String {
    format!("{}_state_e", name.unique_lowercase_snakecase())
}

/// Имя варианта терминального состояния модели.
pub(crate) fn end_variant(name: &Name) -> String {
    format!("{}_END", name.unique_uppercase_snakecase())
}

/// Имя сигнала переменной модели.
///
/// Преисправление обязателен: модуль один на корневую модель, поэтому переменные всех
/// под-моделей попадают в одно пространство имён. У цели `c` они лежат по своим
/// структурам (`model->stacker.command_receiver0.counter`), и две под-модели вправе
/// иметь переменную `counter`; здесь они слиплись бы **молча**.
fn var_signal_name(model: &Name, var: &str) -> String {
    format!("{}_{}", model.unique_lowercase_snakecase(), var)
}

/// Варианты перечисления состояний модели: сами состояния плюс `END`.
///
/// `INIT` не добавляется - его в цели `sv` не существует (см. шапку модуля). Если
/// состояние с именем `End` объявлено автором, второй `END` не заводится:
/// `unique_uppercase_snakecase` даёт для него ровно `<MODEL>_END`. Ширина перечисления
/// состояний модели - тем же счётом, что у его печати.
///
/// Носитель один: вектор строк таблицы и `typedef enum` обязаны сойтись по ширине,
/// иначе частичный выбор `[i*W +: W]` возьмёт чужие биты, а оба инструмента SV такой
/// модуль примут.
pub(crate) fn state_width(model: &Name, variants: &[String]) -> Result<usize, Diagnostic> {
    let numbered: Vec<(String, i128)> = variants
        .iter()
        .enumerate()
        .map(|(i, v)| (v.clone(), i as i128))
        .collect();
    let (width, _) =
        super::sv_type::enum_width(&numbered, &format!("состояния модели '{}'", model))?;
    Ok(width as usize)
}

pub(crate) fn state_variants(model: &Name, states: &[Name]) -> Vec<String> {
    let mut variants: Vec<String> = states
        .iter()
        .map(|s| s.unique_uppercase_snakecase())
        .collect();
    let end = end_variant(model);
    if !variants.contains(&end) {
        variants.push(end);
    }
    variants
}

impl Fsm {
    /// Собирает сигналы модуля со всех уровней.
    ///
    /// # Ошибки
    /// Диагностики отображения типов (`SV-002`...`SV-004`) и имён
    /// (`SV-007`/`SV-012`).
    pub(crate) fn build(
        map: &SvMap,
        blocks: &[Block],
        root: &Name,
        ports: &SvPorts,
        mmio: Option<&crate::generator::sv::sv_mmio::Mmio>,
    ) -> Result<Self, Diagnostic> {
        let mut fsm = Fsm {
            guard_enable: map.guard_enable(),
            regs: Vec::new(),
            time_levels: Vec::new(),
            registered: BTreeSet::new(),
            inouts: BTreeSet::new(),
            state_reg: BTreeMap::new(),
            enums: BTreeMap::new(),
            structs: BTreeMap::new(),
            step_enums: Vec::new(),
            warnings: std::cell::RefCell::new(Vec::new()),
            hoisted_locals: crate::generator::sv::sv_locals::HoistedLocals::default(),
        };

        // Аргументы инстанцирования: значение сброса регистра параметра берётся из
        // места инстанцирования, а не из объявления.
        let instantiation_args = collect_instantiation_args(map, blocks)?;

        // Перечисления собираются со всех уровней: `command := Up` в АСД - это
        // `Number(2)`, и без списка вариантов имя `COMMAND_UP` не построить.
        for (_, model_rc) in blocks {
            for def in model_rc.borrow().enums.values() {
                fsm.enums
                    .entry(def.name.clone())
                    .or_insert_with(|| def.variants.clone());
            }
            // Поля структур - тем же обходом: присваивание агрегата адресует структуру
            // по имени поля.
            for def in model_rc.borrow().structs.values() {
                fsm.structs
                    .entry(def.name.clone())
                    .or_insert_with(|| def.fields.clone());
            }
        }

        for (name, model_rc) in blocks {
            let Some(Element::Model { start, states, .. }) = map.model_element_of(name) else {
                continue;
            };
            // Регистр состояния уровня. Сбрасывается в стартовое состояние - никакого
            // INIT: все уровни сбрасываются одним фронтом, отсюда сдвиг = 0 на любой
            // глубине.
            let reg = state_reg_name(name, root);
            fsm.state_reg.insert(name.unique().to_string(), reg.clone());
            fsm.registered.insert(reg.clone());
            fsm.regs.push(Reg {
                name: reg,
                prefix: state_enum_name(name),
                suffix: String::new(),
                reset: start.unique_uppercase_snakecase(),
                declare_reg: true,
                leaves: Vec::new(),
                default: None,
            });

            // Переменные модели: свой регистр на каждую, с преисправлением уровня.
            let model = model_rc.borrow();
            for var in model.variables.values() {
                let VariableNode::Simple {
                    name: var_name,
                    ty,
                    expr,
                    loc,
                    ..
                } = var
                else {
                    continue;
                };
                if !map.usage().variables.contains(var_name) {
                    continue;
                }
                check_sv_name(var_name, *loc)?;
                // Объявление объявляет своё место.
                crate::generator::site::enter_declaration(*loc);
                let signal = var_signal_name(name, var_name);
                let decl = sv_type(ty, &format!("переменная '{}'", var_name))?;
                // Параметр, заданный при инстанцировании: значение места
                // инстанцирования перекрывает инициализатор объявления - тот же
                // приоритет, что у цели `c` (присваивание после `_init`).
                let arg_expr = instantiation_args
                    .get(name.unique())
                    .and_then(|args| args.iter().find(|a| a.name == *var_name))
                    .map(|a| a.value.clone());
                let expr = arg_expr.as_ref().unwrap_or(expr);
                let reset = sv_const::reset_value(
                    expr,
                    ty,
                    &fsm.enums,
                    &format!("переменной '{var_name}'"),
                    *loc,
                    map.root_model_node().as_ref(),
                )?;
                // Массив, внутри которого лежит структура, сбрасывается и получает
                // умолчание по полям: синтезатор принимает шаблон присваивания только
                // для массива целиком, а частичную запись поля элемента при умолчании
                // массива целиком объявляет защёлкой.
                let leaves = crate::generator::sv::sv_array::leafwise_reset(
                    expr,
                    ty,
                    &fsm.enums,
                    &fsm.structs,
                    *loc,
                    &format!("переменной '{var_name}'"),
                )?;
                fsm.registered.insert(signal.clone());
                fsm.regs.push(Reg {
                    name: signal,
                    prefix: decl.prefix,
                    suffix: decl.suffix,
                    reset,
                    declare_reg: true,
                    leaves,
                    default: None,
                });
            }
            // Слой объявления снимается парно входу.
            crate::generator::site::leave_declaration();
            drop(model);

            // Регистр шага на каждую цепочку `+`. Служебное состояние вне модели: в
            // `is_done` и в выходные порты не попадает - это отдельный `Reg`, а не
            // порт.
            for state_name in &states {
                let Some(Element::StateExtend { extend, .. }) = map.state_at(state_name.clone())
                else {
                    continue;
                };
                // Цепочек в состоянии бывает несколько: своя машина шагов нужна и
                // вложенной, иначе она делит регистр с несущей.
                for chain in crate::generator::chain_site::chains(&extend) {
                    let signal = step_reg_name(state_name, &chain.path);
                    fsm.registered.insert(signal.clone());
                    fsm.regs.push(Reg {
                        name: signal,
                        prefix: step_enum_name(state_name, &chain.path),
                        suffix: String::new(),
                        reset: step_variant(state_name, &chain.path, 0),
                        declare_reg: true,
                        leaves: Vec::new(),
                        default: None,
                    });
                    fsm.step_enums.push(StepEnum {
                        state: state_name.clone(),
                        done: chain.nested(),
                        path: chain.path,
                        count: chain.len,
                    });
                }
            }

            // Регистры механизма времени уровня. Логика - в `sv_time`.
            sv_time::push_time_regs(
                &mut fsm.regs,
                &mut fsm.registered,
                &mut fsm.time_levels,
                map,
                name,
                &model_rc.borrow(),
                &state_enum_name(name),
                &end_variant(name),
                &state_reg_name(name, root),
            )?;
        }

        // Выходные порты: регистр уже объявлен в заголовке модуля, нужна только
        // комбинационная пара. Значение сброса - начальное значение порта (`:=`) либо
        // ноль: вывод кристалла обязан иметь определённое значение с первого же фронта.
        // Ветвь сброса - то самое "до первого такта" контракта R5: у цели `sv` иного
        // места нет, там же живут стартовые состояния.
        for port in &ports.outputs {
            fsm.registered.insert(port.name.clone());
            fsm.regs.push(Reg {
                name: port.name.clone(),
                prefix: port.ty.prefix.clone(),
                suffix: port.ty.suffix.clone(),
                reset: sv_const::reset_value(
                    &port.init,
                    &port.ty_node,
                    &fsm.enums,
                    &format!("порта '{}'", port.name),
                    port.loc,
                    map.root_model_node().as_ref(),
                )?,
                declare_reg: false,
                leaves: Vec::new(),
                default: None,
            });
        }

        // Двунаправленные порты: сторона записи и строб. Сторона чтения регистром не
        // становится - это вход модуля, его ведёт плата.
        for port in &ports.inouts {
            fsm.inouts.insert(port.name.clone());
            let out = crate::generator::sv::sv_module::inout_out(&port.name);
            let we = crate::generator::sv::sv_module::inout_we(&port.name);
            fsm.registered.insert(out.clone());
            fsm.registered.insert(we.clone());
            fsm.regs.push(Reg {
                name: out,
                prefix: port.ty.prefix.clone(),
                suffix: port.ty.suffix.clone(),
                reset: sv_const::reset_value(
                    &port.init,
                    &port.ty_node,
                    &fsm.enums,
                    &format!("порта '{}'", port.name),
                    port.loc,
                    map.root_model_node().as_ref(),
                )?,
                declare_reg: false,
                leaves: Vec::new(),
                default: None,
            });
            fsm.regs.push(Reg {
                name: we,
                prefix: "logic".to_string(),
                suffix: String::new(),
                reset: "1'b0".to_string(),
                declare_reg: false,
                leaves: Vec::new(),
                // Умолчание - Ноль, а не "как есть": строб поднят ровно в том такте,
                // где модель писала порт. "Как есть" оставило бы его поднятым навсегда,
                // и плата затирала бы ячейку каждый такт - модель, которая пишет
                // однажды, вела бы линию вечно.
                default: Some("1'b0".to_string()),
            });
        }

        // Адресованные `out`-порты цели `sv-mmio`: в отличие от портов модуля,
        // объявляются внутри (`declare_reg: true`) - это биты регистрового файла, а не
        // выводы кристалла. Запись `_next` автоматом и защёлкивание - те же, что у
        // выходного порта; шина их только читает (мультиплексор `reg_rdata` в
        // `sv_mmio`). `in`-биты регистрами автомата не становятся: их пишет шина, и
        // обслуживает их отдельный `always_ff` в `sv_mmio`.
        if let Some(m) = mmio {
            for port in m.outputs() {
                let ty = crate::generator::sv::sv_mmio::port_sv_type(port)?;
                let name = crate::generator::sv::sv_mmio::port_signal_name(port).to_string();
                fsm.registered.insert(name.clone());
                let reset = sv_const::reset_value(
                    &port.init,
                    &port.ty,
                    &fsm.enums,
                    &format!("порта '{}'", port.name),
                    port.loc,
                    map.root_model_node().as_ref(),
                )?;
                fsm.regs.push(Reg {
                    name,
                    prefix: ty.prefix,
                    suffix: ty.suffix,
                    reset,
                    declare_reg: true,
                    leaves: Vec::new(),
                    default: None,
                });
            }
        }
        Ok(fsm)
    }

    /// Контекст печати выражений с собранными отображениями.
    pub(crate) fn scope(&self) -> Scope<'_> {
        Scope {
            registered: &self.registered,
            inouts: &self.inouts,
            guard_enable: self.guard_enable,
            function: None,
            function_ret: None,
            locals: crate::generator::sv::sv_scope::no_locals(),
            enums: &self.enums,
            structs: &self.structs,
            warnings: &self.warnings,
        }
    }
}

/// Печатает объявления регистров и их комбинационных пар.
pub(crate) fn emit_signals(p: &mut Printer, fsm: &Fsm) {
    for reg in &fsm.regs {
        let decl = |name: &str| -> String {
            if reg.suffix.is_empty() {
                format!("{} {};", reg.prefix, name)
            } else {
                format!("{} {} {};", reg.prefix, name, reg.suffix)
            }
        };
        if reg.declare_reg {
            p.ident(&decl(&reg.name)).nl();
        }
        p.ident(&decl(&format!("{}_next", reg.name))).nl();
    }
    p.nl();
}

/// Печатает `always_comb`: умолчания и тела уровней.
pub(crate) fn emit_comb(
    p: &mut Printer,
    map: &SvMap,
    fsm: &Fsm,
    root: &Name,
    models: &[Name],
) -> Result<(), Diagnostic> {
    p.ident("always_comb begin").nl();
    p.up();
    // Тело печатается в буфер: пока оно печатается, печатник тел складывает в
    // `fsm.hoisted_locals` локальные переменные со структурой - их объявления обязаны
    // стоять в начале процесса, а собрать их отдельным обходом значило бы завести
    // второй список того же набора.
    let mut body_text = String::new();
    {
        let mut buffer = p.fork(&mut body_text);
        sv_time::emit_time_updates(&mut buffer, &fsm.time_levels)?;
        emit_model_body(&mut buffer, map, fsm, root)?;
    }
    // Объявления первыми: в SystemVerilog они обязаны предшествовать операторам блока.
    // Локальные тел, содержащие структуру, объявляются здесь: внутри ветви `case` yosys
    // объявляет такую переменную защёлкой, тогда как verilator модуль принимает.
    crate::generator::sv::sv_locals::emit_declarations(p, &fsm.hoisted_locals);
    // Умолчания обязательны: неполное присваивание даёт защёлку, а `verilator -Wall` -
    // LATCH. Это условие проверки, а не стиль.
    for reg in &fsm.regs {
        // Массив со структурой внутри - по полям: whole-array умолчание синтезатор
        // считает защёлкой, когда тело пишет поле элемента.
        if let Some(default) = &reg.default {
            p.ident(&format!("{}_next = {};", reg.name, default)).nl();
        } else if reg.leaves.is_empty() {
            p.ident(&format!("{}_next = {};", reg.name, reg.name)).nl();
        } else {
            for (suffix, _) in &reg.leaves {
                p.ident(&format!(
                    "{name}_next{suffix} = {name}{suffix};",
                    name = reg.name
                ))
                .nl();
            }
        }
    }
    crate::generator::sv::sv_locals::emit_defaults(p, &fsm.hoisted_locals);
    // Умолчания признаков диспетчера таблицы - по той же причине, что у регистров:
    // диспетчер под-модели стоит внутри ветви родителя, и на ветвях, где он не
    // исполняется, признак остался бы без присваивания - yosys отвечает "Latch
    // inferred", а verilator такой модуль принимает.
    if map.fsm_table() {
        super::sv_table::emit_defaults(p, map, models)?;
    }
    p.nl();
    // Перекрытие умолчаний регистров времени и тело - уже напечатаны в буфер выше.
    p.print(&body_text);
    p.down();
    p.ident("end").nl().nl();
    Ok(())
}

/// Печатает тело одного уровня: `unique case` по его состояниям.
pub(crate) fn emit_model_body(
    p: &mut Printer,
    map: &SvMap,
    fsm: &Fsm,
    model: &Name,
) -> Result<(), Diagnostic> {
    let Some(Element::Model { states, .. }) = map.model_element_of(model) else {
        return Err(sv002(&format!(
            "модель '{}' отсутствует в снимке карты",
            model
        )));
    };
    let reg = fsm
        .state_reg
        .get(model.unique())
        .ok_or_else(|| sv002(&format!("регистр состояния модели '{}'", model)))?;

    // model-level `always` (вне состояния) - каждый такт до `unique case`, безусловно
    // по состоянию (эталон - шаг 2 `execution("always")` симулятора). В `always_comb`
    // работает над `_next` (умолчания уже заданы).
    let raw_model = map.raw_model_at(model.clone())?;
    emit_model_prelude(p, map, &raw_model.borrow(), fsm)?;

    p.ident(&format!("unique case ({})", reg)).nl();
    p.up();
    // Имена напечатанных ветвей - чтобы терминальная не задвоилась: состояние,
    // названное автором `End`, даёт тот же вариант, что синтетический терминальный, и
    // `verilator` отвечает `CASEOVERLAP` ("Case conditions overlap") при нулевом коде
    // возврата `taktc`. Цель `c` на том же входе печатает **одну** ветвь - расходилась
    // одна цель.
    let terminal = end_variant(model);
    let mut printed_terminal = false;
    // Предикаты готовности реализаций: собираются при печати тел и читаются диспетчером
    // таблицы после `endcase`.
    let mut ready: Vec<(String, String)> = Vec::new();
    for state_name in &states {
        let Some(element) = map.state_at(state_name.clone()) else {
            continue; // недостижимое состояние - ветви не получает
        };
        let raw = map.raw_state_at(state_name.clone())?;
        let raw = &*raw.borrow();
        let variant = state_name.unique_uppercase_snakecase();
        printed_terminal |= variant == terminal;
        p.ident(&format!("{}: begin", variant)).nl();
        p.up();
        emit_state_prelude(p, map, raw, fsm)?;
        // Периодические блоки `every` - после `always`, как в симуляторе. Проверка читает
        // `_next` метки/счётчика (учёт текущего такта).
        {
            let rm = raw_model.borrow();
            for e in sv_time::model_every(&rm) {
                if e.state != state_name.local() {
                    continue;
                }
                let body = e.body;
                sv_time::emit_every_gate(
                    p,
                    &fsm.time_levels,
                    map,
                    model,
                    e.idx,
                    e.period_nanos,
                    |p| super::sv_stmt::print_statement(p, body, &fsm.scope()),
                )?;
            }
        }
        match element {
            // Табличная форма: переходы простого состояния - строки таблицы, и в ветви
            // остаётся только тело такта.
            Element::State { .. } if map.fsm_table() => {}
            Element::State { .. } => {
                emit_transitions(p, map, fsm, raw, model, &states)?;
            }
            Element::StateExtend { extend, next, .. } => {
                if let Some(predicate) = super::sv_compose::emit_extend(
                    p, map, fsm, state_name, raw, model, &extend, &next, &states,
                )? {
                    // Готовность реализации вычисляется по ходу инлайна композиции -
                    // здесь она запоминается для стража строки (второго знания о
                    // раскладке её регистров не заводится).
                    ready.push((state_name.unique().to_string(), predicate));
                }
            }
            Element::Model { .. } => {
                return Err(sv002("модель в позиции состояния"));
            }
        }
        p.down();
        p.ident("end").nl();
    }
    // Терминальная ветвь: `unique case` требует покрытия всех значений, иначе Verilator
    // даёт CASEINCOMPLETE.
    //
    // Печатается **только если её ещё нет**: состояние с именем `End` даёт тот же
    // вариант, и вторая ветвь - это `CASEOVERLAP`, то есть отказ проверки цели при нулевом
    // коде возврата `taktc`.
    if !printed_terminal {
        p.ident(&format!("{}: begin end", terminal)).nl();
    }
    p.down();
    p.ident("endcase").nl();
    // Табличная форма: переходы просматривает диспетчер - После тел состояний, ровно
    // там, где стоял переход внутри ветви.
    if map.fsm_table() {
        super::sv_table::emit_dispatcher(p, map, fsm, model, &ready)?;
    }
    Ok(())
}

/// Печатает переходы состояния - цепочкой `if / else if`.
///
/// Цепочка, а не независимые `if`: в C каждый переход завершается `break`, то есть
/// **первый сработавший выигрывает**. Независимые `if` дали бы срабатывание всех
/// подходящих подряд, и последний затёр бы предыдущие - порядок рёбер значим.
/// Возвращает `true`, если напечатано **безусловное** ребро: цепочка на нём
/// заканчивается, и вызывающий не печатает переход по `next`/`END`.
pub(crate) fn emit_transitions(
    p: &mut Printer,
    map: &SvMap,
    fsm: &Fsm,
    state: &StateNode,
    model: &Name,
    states: &[Name],
) -> Result<bool, Diagnostic> {
    let mut printed = 0usize;
    for reference in state.references() {
        let Some(target) = states.iter().find(|n| n.local() == reference.name).cloned() else {
            continue; // цель вне достижимых состояний
        };
        // Решение "ребро безусловно" - у одного носителя; см. Переход объявляет своё
        // место.
        crate::generator::site::enter(reference.location);
        let unconditional = reference.cond.is_unconditional();
        if unconditional {
            // Безусловное ребро: всё, что ниже, недостижимо, - и это верно, так как в C
            // оно тоже завершается `break`.
            if printed > 0 {
                p.ident("else begin").nl();
            } else {
                p.ident("begin").nl();
            }
        } else {
            // Выдержка `after` - через `sv_time`; прочие - общий печатник.
            let scope = fsm.scope();
            let cond =
                match sv_time::after_guard(&fsm.time_levels, map, model, &reference.cond, &scope) {
                    Some(guard) => guard?,
                    None => print_condition(&reference.cond, &scope)?,
                };
            let keyword = if printed == 0 { "if" } else { "else if" };
            p.ident(&format!("{} ({}) begin", keyword, cond)).nl();
        }
        p.up();
        emit_named_blocks(p, state, fsm, "exit")?;
        let target_rc = map.raw_state_at(target.clone())?;
        emit_named_blocks(p, &target_rc.borrow(), fsm, "enter")?;
        let reg = fsm
            .state_reg
            .get(model.unique())
            .ok_or_else(|| sv002(&format!("регистр состояния модели '{}'", model)))?;
        p.ident(&format!(
            "{}_next = {};",
            reg,
            target.unique_uppercase_snakecase()
        ))
        .nl();
        p.down();
        p.ident("end").nl();
        printed += 1;
        if unconditional {
            return Ok(true);
        }
    }
    // Терминальное состояние: переходов нет - уходим в END, как это делает C.
    //
    // Возврат здесь `true`: переход напечатан.
    if state.is_terminated() {
        emit_named_blocks(p, state, fsm, "exit")?;
        let reg = fsm
            .state_reg
            .get(model.unique())
            .ok_or_else(|| sv002(&format!("регистр состояния модели '{}'", model)))?;
        p.ident(&format!("{}_next = {};", reg, end_variant(model)))
            .nl();
        return Ok(true);
    }
    Ok(false)
}

/// Печатает `always_ff`: ветвь сброса и защёлкивание.
pub(crate) fn emit_ff(
    p: &mut Printer,
    map: &SvMap,
    fsm: &Fsm,
    blocks: &[Block],
    array_consts: &[(String, String)],
) -> Result<(), Diagnostic> {
    // Блоки `enter` стартовых состояний исполняются цепью сброса - поэтому обязаны быть
    // константными (`SV-008`).
    let mut enter_resets: Vec<(String, String)> = Vec::new();
    for (name, _) in blocks {
        let Some(Element::Model { start, .. }) = map.model_element_of(name) else {
            continue;
        };
        let raw = map.raw_state_at(start.clone())?;
        let raw = &*raw.borrow();
        for b in raw.get_named_blocks("enter") {
            if let Some(stmt) = b.statement() {
                sv_const::constant_enter_assignments(
                    stmt,
                    start.local(),
                    raw.loc(),
                    &fsm.scope(),
                    &mut enter_resets,
                )?;
            }
        }
    }

    p.ident("always_ff @(posedge clk) begin").nl();
    p.up();
    p.ident("if (!rst_n) begin").nl();
    p.up();
    for reg in &fsm.regs {
        if reg.leaves.is_empty() {
            p.ident(&format!("{} <= {};", reg.name, reg.reset)).nl();
        } else {
            // Сброс по полям - по той же причине, что и умолчание.
            for (suffix, value) in &reg.leaves {
                p.ident(&format!("{}{suffix} <= {value};", reg.name)).nl();
            }
        }
    }
    // Константы-массивы: значение постоянно, но задаётся здесь - `localparam`
    // распакованного массива не синтезируется.
    for (signal, value) in array_consts {
        p.ident(&format!("{signal} <= {value};")).nl();
    }
    // Блоки `enter` стартовых состояний - после умолчаний: они их уточняют.
    for (signal, value) in &enter_resets {
        p.ident(&format!("{} <= {};", signal, value)).nl();
    }
    p.down();
    // `en` проверкаит только защёлкивание: сброс выше проверяется первым и от `en` не
    // зависит - иначе модуль нельзя сбросить, пока он не разрешён. `en == 1` при
    // неподключённом порте (умолчание) -> шаг каждый такт, как прежде.
    p.ident("end else if (en) begin").nl();
    p.up();
    for reg in &fsm.regs {
        p.ident(&format!("{} <= {}_next;", reg.name, reg.name)).nl();
    }
    p.down();
    p.ident("end").nl();
    p.down();
    p.ident("end").nl().nl();
    Ok(())
}

/// Печатает выход терминальности корневой модели.
pub(crate) fn emit_is_done(p: &mut Printer, root: &Name) {
    p.ident(&format!(
        "assign is_done = (state == {});",
        end_variant(root)
    ))
    .nl();
}
