//! Генератор Structured Text (IEC 61131-3) из семантического дерева Takt.
//!
//! Модуль транслирует семантическое дерево [`ModelNode`] в файл `.st` - текстовый язык
//! программируемых логических контроллеров (ПЛК), стандарт IEC 61131-3. архитектурное
//! решение - (Option A).
//!
//! ## Схема отображения
//!
//! - Модель Takt -> `FUNCTION_BLOCK` (единственная конструкция IEC с сохраняемым
//!   между вызовами состоянием - прямой аналог `struct` + `_tick()` цели `c`).
//! - Состояния -> `CASE state OF` по переменной состояния.
//! - Тело `FUNCTION_BLOCK` = один цикл сканирования ПЛК = один такт Takt.
//!
//! ## Цели
//!
//! - `st` - чистый IEC 61131-3, адреса портов не потребляются.
//! - `st-at` - плюс размещение портов по карте адресов (`AT %IX...`/`%QX...`),
//!   включается [`GenerateOptions::hal`]; реализуется.
//!
//! ## Состояние реализации
//!
//! Задача **** закрыла **каркас и диспетчеризацию**: цели, публичный API, снимок карты
//! и скелет `FUNCTION_BLOCK` на каждую модель. Задача **** добавила отображение типов
//! (`st_type.rs`) и секции объявлений (`st_decl.rs`). Остаток:
//!
//! | Задача | Что добавляет |
//! |---|---|
//! | | `st_model.rs` - `CASE state OF`, переходы, композиция |
//! | | `st_expr.rs` - выражения, условия, операторы |
//! | | `AT %...` - потребление карты адресов (цель `st-at`) |

mod st_arith;
mod st_at;
mod st_compose;
mod st_cond;
mod st_decl;
mod st_decl_types;
mod st_edges;
mod st_expr;
mod st_fixed;
mod st_func;
mod st_map;
mod st_model;
mod st_multidim;
mod st_operand_type;
mod st_params;
mod st_reserved;
mod st_sign;
mod st_stmt;
mod st_table;
mod st_time;
mod st_type;

use crate::address_map::{AddressSource, ResolvedAddress};
use crate::diagnostics::{Diagnostic, Location};
use crate::generator::Generator as AsGenerator;
use crate::generator::header::{CommentStyle, file_header};
use crate::generator::indent::Printer;
use crate::generator::{GenerateOptions, GeneratedFile, Output};
use crate::semantic::minimap::{Element, Name};
use crate::semantic::naming::normalize_lowercase_snakecase;
use crate::semantic::{ModelNode, PortDirection, VariableNode};
use st_map::StMap;
use std::cell::RefCell;
use std::fmt::Write as _;
use std::rc::Rc;

/// Размер одного уровня отступа в порождаемом ST.
const INDENT: usize = 4;

/// Генератор Structured Text для модели Takt.
pub struct Generator {}

impl AsGenerator for Generator {
    fn generate_texts(
        &self,
        model: &ModelNode,
        options: &GenerateOptions,
    ) -> Result<Output, Diagnostic> {
        // Профиль времени: `clock` модели - контракт, флаг обязан подтвердить. Единый
        // чекпойнт-энфорсмент `SE-069`/`SE-070`.
        let profile = crate::semantic::duration::resolve_profile(model.clock_hz, options.tick_hz)?;
        let map = StMap::new(
            &normalize_lowercase_snakecase(model.name().to_string()),
            model,
            options.hal,
            options.address_map.clone(),
        )?
        .with_time_profile(profile)
        .with_fsm(options.fsm)
        .with_comments(options.comments.clone());
        let (program, warnings) = generate_program(&map)?;
        let filename = map.get_filename();
        Ok(Output {
            files: vec![GeneratedFile {
                name: filename.to_owned() + ".st",
                text: program,
            }],
            warnings,
        })
    }

    fn write_failure(&self, error: &std::io::Error) -> Diagnostic {
        Diagnostic::error(Location::Codegen, format!("{error}")).with_code("ST-001")
    }
}

/// Строит текст ST-программы из снимка модели.
///
/// Эмитится заголовок файла, общие объявления `TYPE ... END_TYPE` и по одному
/// `FUNCTION_BLOCK ... END_FUNCTION_BLOCK` на корневую модель и на каждую используемую
/// подмодель - с секциями объявлений. Тело блоков наполняют .... Возвращает текст
/// программы **и предупреждения цели** (`ST-009`, `ST-010`, `ST-022`, размещение
/// портов). Приёмников три - функции, тела блоков и конфигурация, - и все они сходятся
/// здесь: печатать их генератор не вправе.
fn generate_program(map: &StMap) -> Result<(String, Vec<Diagnostic>), Diagnostic> {
    let Element::Model { .. } = map.model() else {
        return Err(Diagnostic::error(
            crate::generator::site::at(Location::Codegen),
            "Корневой элемент карты не является моделью".to_string(),
        )
        .with_code("ST-012"));
    };

    // Анонимное обращение к ячейке требует **локации**, а её знает только цель `st-at`:
    // цель `st` - библиотека блоков, где порт есть `VAR_INPUT`/`VAR_OUTPUT`, а
    // адресного пространства нет вовсе.
    //
    // Проверка стоит здесь, в точке входа, а не в печатнике: так режим цели не
    // приходится тянуть через все печатники выражений и условий, а отказ приходит
    // **до** первой строки вывода, то есть невалидного файла не возникает даже
    // частично.
    if !map.at_addresses()
        && let Some(root) = map.root_model_node()
        && let Some(cell) = crate::semantic::collect_anon_ports(&root).first()
    {
        return Err(Diagnostic::error(
            // Место обращения к ячейке: отказ приходит до первой строки вывода, когда
            // носитель позиции ещё пуст, - координату несёт сама ячейка.
            crate::generator::site::at(cell.loc),
            format!(
                "обращение к ячейке по адресу ('#0x{:X}') требует размещения, \
                 которого цель 'st' не знает: она порождает библиотеку блоков. \
                 Соберите целью 'st-at'",
                cell.addr as u64
            ),
        )
        .with_code("ST-018"));
    }

    let mut out = String::new();
    let mut p = Printer::new(INDENT, &mut out);

    let target = if map.at_addresses() {
        "Structured Text (IEC 61131-3, AT profile)"
    } else {
        "Structured Text (IEC 61131-3)"
    };
    for line in file_header(target, CommentStyle::IecBlock) {
        p.ident(&line).nl();
    }
    p.nl();

    // Подмодели объявляются раньше корня: FUNCTION_BLOCK, используемый как тип
    // экземпляра, должен быть известен к моменту объявления экземпляра.
    //
    // Порядок фиксируется сортировкой по уникальному имени. В IEC 61131-3 порядок
    // объявлений **значим** - тип экземпляра обязан быть объявлен раньше использования, -
    // поэтому случайный порядок здесь дороже, чем в C: он делает вывод то валидным, то
    // нет. Сортировка даёт воспроизводимую сборку и устойчивый вход для проверки
    // MatIEC.
    //
    // Первопричина - `HashMap` в семантическом слое (`semantic/mod.rs`), она общая для
    // целей `c`/`plantuml` и чинится отдельным кандидатом "Генерация C
    // недетерминирована" (`FEATURES.md`); здесь снимается только следствие в своём
    // генераторе.
    let mut submodels: Vec<_> = map
        .using_models()
        .into_iter()
        .filter_map(|element| match element {
            Element::Model { name, .. } => Some(name),
            _ => None,
        })
        .collect();
    // Модель обязана быть объявлена раньше той, что заводит её экземпляр: опережающие
    // ссылки в ST - нестандартное расширение (`iec2c -p`). Порядок топологический, а не
    // по глубине вложенности: зависимость бывает и между соседями (`E` содержит `F`,
    // оба - подмодели корня), и одной лишь "глубже - раньше" тут мало (поймано проверкой
    // на `extend_complex`).
    submodels.sort_by(|a, b| a.unique().cmp(b.unique()));
    submodels = topological_order(map, submodels);

    // Блоки строятся в порядке "подмодели -> корень"; корень идёт последним по той же
    // причине, что и сортировка выше.
    let mut blocks: Vec<(Name, Rc<RefCell<ModelNode>>)> = Vec::new();
    for name in submodels {
        let model = map.raw_model_at(name.clone())?;
        blocks.push((name, model));
    }
    let root = map
        .root_model_node()
        .ok_or_else(|| root_missing(map.root_name()))?;
    blocks.push((map.root_name(), root));

    // Массивы, которые действительно передаются в под-модели: им нужен именованный тип,
    // потому что MatIEC отвергает анонимный `ARRAY [...]` в объявлении параметра.
    // Считается по объединению `shared` всех под-моделей, а не "все массивы корня":
    // иначе локальный массив без единой под-модели тоже получил бы тип - лишняя
    // сущность и сдвиг вывода корпуса.
    let shared_arrays = st_decl_types::shared_array_names(map, &blocks, &map.root_name());

    // Объявления структур - общие для файла и печатаются раньше всех блоков: в IEC
    // 61131-3 тип обязан быть известен к моменту использования.
    st_decl_types::emit_struct_types(&mut p, &blocks, &shared_arrays)?;
    // Функции - тоже раньше: опережающие ссылки в ST нестандартны (`iec2c -p`).
    let mut warnings = st_func::emit_functions(&mut p, &blocks, map.usage())?;

    // Формы массивов из параметров функций считаются один раз: список общий у продюсера
    // типов и у каждого объявления.
    let array_forms = st_decl_types::function_array_form_names(&blocks);

    let root_name = map.root_name();
    for (name, model) in &blocks {
        let is_root = name.unique() == root_name.unique();
        warnings.extend(emit_function_block(
            &mut p,
            map,
            name,
            &model.borrow(),
            is_root,
            &shared_arrays,
            &array_forms,
        )?);
    }

    // Цель `st-at` порождает программу для ПЛК целиком, а не библиотеку блоков:
    // размещённые порты живут в `VAR_GLOBAL`, а он вне `CONFIGURATION` недопустим
    // (проба П8). Цель `st` обёртки не требует (П2) - цели асимметричны намеренно.
    if map.at_addresses() {
        warnings.extend(emit_configuration(&mut p, map, &root_name, &blocks)?);
    }

    // ST-022: охранная формула в IEC 61131-3 невыразима - конструкции `assert` там нет
    // вовсе, а ближайший аналог (булев флаг нарушения) ввёл бы в вывод сущность,
    // которой нет в модели, и семантику "нарушено, но работаем дальше", расходящуюся с
    // эталоном (`SIM-025` прогон останавливает).
    //
    // Поэтому цель предупреждает и продолжает трансляцию: отказ лишил бы автора рабочей
    // прошивки ПЛК из-за конструкции, которая для этой цели лишь неприменима. Молчать
    // нельзя: до 0235 охрана исчезала бесследно, и автор об этом не узнавал (находка ).
    //
    // Формулы берутся из общего сбора мест (`semantic/formula/sites.rs`), а не из
    // собственного обхода: второй список мест разъехался бы с первым при появлении
    // нового места объявления формулы.
    for (_, model) in &blocks {
        for site in crate::semantic::formula::sites::model_formula_sites(&model.borrow()) {
            if let crate::semantic::formula::sites::FormulaLeaf::Guard(_) = site.formula {
                warnings.push(
                    Diagnostic::warning(
                        site.loc,
                        "охранная формула не транслируется целью 'st': в IEC 61131-3 \
                         конструкции assert не существует, а выразить проверку иначе \
                         значило бы добавить в вывод переменную, которой нет в модели. \
                         Охрана остаётся действующей в симуляторе (SIM-025) и в целях \
                         'c', 'rust', 'sv'"
                            .to_string(),
                    )
                    .with_code("ST-022"),
                );
            }
        }
    }

    // Q-хелпер TAKT_Q_FLOORDIV вставляется перед первым POU по факту вызова.
    Ok((st_fixed::insert_helper(out), warnings))
}

/// Строка объявления анонимной ячейки в `VAR_GLOBAL`.
///
/// Тот же выбор, что у порта `inout` (`st_at::location_of`).
///
/// # Ошибки
///
/// `ST-019` - поле **со смещением** (`#0x100:3 as u8`): локация IEC выражает либо бит
/// (`%MX512.3`), либо целое слово (`%MB512`), а поля с произвольного разряда - нет.
/// Молча выронить смещение нельзя: получилось бы обращение к другому месту памяти.
fn anon_global(cell: &crate::semantic::AnonPortAccess) -> Result<String, Diagnostic> {
    let is_bit = matches!(
        cell.ty,
        crate::semantic::type_node::TypeNode::Bit | crate::semantic::type_node::TypeNode::Bool
    );
    if !is_bit && cell.bit != 0 {
        return Err(Diagnostic::error(
            crate::generator::site::at(Location::Codegen),
            format!(
                "обращение '#0x{:X}:{} as {}' задаёт поле со смещением, а локация \
                 IEC 61131-3 такого не выражает: допустимы либо один бит \
                 ('#0x{:X}.N'), либо целое слово ('#0x{:X} as ТИП')",
                cell.addr as u64, cell.bit, cell.ty, cell.addr as u64, cell.addr as u64
            ),
        )
        .with_code("ST-019"));
    }
    let resolved = ResolvedAddress {
        addr: cell.addr,
        bit: if is_bit { Some(cell.bit) } else { None },
        source: AddressSource::Inline,
        ty: cell.ty.clone(),
        direction: PortDirection::InOut,
        name: cell.synthetic_name(),
    };
    let name = cell.synthetic_name();
    let (location, comment, _warnings) = st_at::location_of(
        &name,
        &cell.ty,
        PortDirection::InOut,
        &resolved,
        &ModelNode::default(),
    )?;
    let ty_name = st_type::get_st_type(&cell.ty, &ModelNode::default())?;
    Ok(format!(
        "{} AT {} : {}; {}",
        name, location, ty_name, comment
    ))
}

/// Печатает `PROGRAM` и `CONFIGURATION` с размещёнными портами (цель `st-at`).
///
/// Форма проверена пробами П3 (обёртка) и П8 (`VAR_GLOBAL ... AT %...` внутри
/// `CONFIGURATION`). Позиция объявления порта - для диагностики.
fn port_loc(var: &VariableNode) -> Location {
    match var {
        VariableNode::Port { loc, .. }
        | VariableNode::Simple { loc, .. }
        | VariableNode::Const { loc, .. } => *loc,
        VariableNode::Unresolved => Location::Codegen,
    }
}

fn emit_configuration(
    p: &mut Printer,
    map: &StMap,
    root_name: &Name,
    blocks: &[(Name, Rc<RefCell<ModelNode>>)],
) -> Result<Vec<Diagnostic>, Diagnostic> {
    let fb = root_name.unique_camelcase();
    p.ident(&format!("PROGRAM {}Main", fb)).nl();
    p.ident("VAR").nl();
    p.up();
    p.ident(&format!("inst : {};", fb)).nl();
    p.down();
    p.ident("END_VAR").nl();
    p.up();
    p.ident("inst();").nl();
    p.down();
    p.ident("END_PROGRAM").nl().nl();

    // Размещённые порты собираются заранее: пустой `VAR_GLOBAL ... END_VAR` недопустим
    // ("no variable declared in global variable(s) declaration"), а модель без портов -
    // не ошибка (например, `comprehensive.takt`).
    let mut placed: Vec<String> = Vec::new();
    let mut warnings = Vec::new();
    let mut seen: Vec<String> = Vec::new();
    // Имена, которые цель печатает рядом с глобальными переменными: блоки, программа,
    // ресурс, экземпляр.
    let block_names: Vec<String> = blocks
        .iter()
        .map(|(name, _)| name.unique_camelcase())
        .collect();
    let occupied = st_reserved::global_pou_names(&fb, &block_names);
    // Порты собираются со всех моделей, а не только с корня: в `elevator_mini` они
    // объявлены внутри под-моделей (`out elevator_motor_up: bit;` в `Motor`).
    // Пропустить их значило бы оставить `VAR_EXTERNAL` без глобала - "the external
    // variable does not match with any global variable".
    for (model_name, model_rc) in blocks {
        let root = &*model_rc.borrow();
        let mut names: Vec<&String> = root.variables.keys().collect();
        names.sort();
        for name in names {
            let VariableNode::Port {
                name: pname,
                ty,
                direction,
                init,
                ..
            } = &root.variables[name]
            else {
                continue;
            };
            if !map.usage().ports.contains(pname) || seen.contains(pname) {
                continue;
            }
            // Порт без адреса при `st-at` - уже ошибка слоя 0020 (SE-052), сюда он не
            // доходит; но если карта пуста, размещать нечего. карта ключуется
            // квалифицированно (модель+порт) - тот же ключ, что строит продюсер
            // `resolve_model`.
            let key = crate::address_map::qualified_port_key(model_name.unique(), pname);
            let Some(resolved) = map.address_of(&key) else {
                continue;
            };
            seen.push(pname.clone());
            // Имя глобальной переменной против имён POU: у MatIEC это одно
            // пространство, и совпадение даёт "invalid global variable(s) declaration"
            // при нулевом коде возврата.
            st_reserved::check_st_global_clash(pname, &occupied, port_loc(&root.variables[name]))?;
            // Объявление объявляет своё место: отказ и предупреждение размещения
            // рождаются вне операторов, и без слоя печатались без координаты - автор не
            // знал, какой порт назван.
            crate::generator::site::enter_declaration(port_loc(&root.variables[name]));
            let (location, comment, mut w) =
                st_at::location_of(pname, ty, *direction, resolved, root)?;
            warnings.append(&mut w);
            let ty_name = st_type::get_st_type(ty, root)?;
            // Начальное значение порта - на размещённой глобальной переменной, а не на
            // `VAR_EXTERNAL` блока: инициализатор внешнего объявления стандарт
            // запрещает. Запасной путь "запись первым сканом", заложенный в анализе как
            // риск, не нужен.
            let init_text = st_decl::literal_init(init, ty, None)
                .map(|v| format!(" := {}", v))
                .unwrap_or_default();
            placed.push(format!(
                "{} AT {} : {}{}; {}",
                pname, location, ty_name, init_text, comment
            ));
        }
        // Слой объявления снимается парно входу.
        crate::generator::site::leave_declaration();
    }
    // Анонимные ячейки: у них нет объявления в исходнике, но локация в IEC принадлежит
    // **объявлению**, поэтому цель заводит размещённую глобальную переменную с именем,
    // которое строит компилятор. Имя - общее с эталоном и с целью `sv-mmio`
    // (`synthetic_name`), иначе сверка трасс сравнивала бы разные величины.
    if let Some(root) = map.root_model_node() {
        for cell in crate::semantic::collect_anon_ports(&root) {
            placed.push(anon_global(&cell)?);
        }
    }

    // Порядок объявления глобалов на семантику не влияет, но должен быть
    // воспроизводимым: `variables` - HashMap.
    placed.sort();

    p.ident(&format!("CONFIGURATION {}Config", fb)).nl();
    p.up();
    if !placed.is_empty() {
        p.ident("VAR_GLOBAL").nl();
        p.up();
        for line in &placed {
            p.ident(line).nl();
        }
        p.down();
        p.ident("END_VAR").nl();
    }
    p.ident("RESOURCE Res0 ON PLC").nl();
    p.up();
    p.ident("TASK Tick(INTERVAL := T#100ms, PRIORITY := 0);")
        .nl();
    p.ident(&format!("PROGRAM Inst0 WITH Tick : {}Main;", fb))
        .nl();
    p.down();
    p.ident("END_RESOURCE").nl();
    p.down();
    p.ident("END_CONFIGURATION").nl().nl();
    Ok(warnings)
}

/// Упорядочивает подмодели так, чтобы каждая шла после тех, кого использует.
///
/// Алгоритм Кана по отношению "заводит экземпляр". Вход отсортирован лексикографически,
/// поэтому и результат воспроизводим (`used_models()` обходит `HashMap`, то есть отдаёт
/// модели в разном порядке от запуска к запуску).
///
/// Цикл в зависимостях невозможен: модель не может содержать саму себя. Но если он
/// вдруг возникнет, остаток дописывается как есть - тогда `iec2c` пожалуется на
/// опережающую ссылку, то есть **громко**, а не молча.
fn topological_order(map: &StMap, models: Vec<Name>) -> Vec<Name> {
    let deps: Vec<(Name, Vec<String>)> = models
        .iter()
        .map(|n| (n.clone(), map.instantiated_by(n)))
        .collect();
    let mut placed: Vec<Name> = Vec::new();
    let mut rest: Vec<(Name, Vec<String>)> = deps;

    while !rest.is_empty() {
        let ready: Vec<Name> = rest
            .iter()
            .filter(|(_, d)| d.iter().all(|dep| placed.iter().any(|p| p.unique() == dep)))
            .map(|(n, _)| n.clone())
            .collect();
        if ready.is_empty() {
            // Цикл: дописываем остаток, не теряя ни одной модели.
            placed.extend(rest.into_iter().map(|(n, _)| n));
            break;
        }
        for name in &ready {
            placed.push(name.clone());
        }
        rest.retain(|(n, _)| !ready.iter().any(|r| r.unique() == n.unique()));
    }
    placed
}

/// Строит диагностику `ST-012` - снимок карты не содержит корневой модели.
fn root_missing(name: Name) -> Diagnostic {
    Diagnostic::error(
        crate::generator::site::at(Location::Codegen),
        format!("Корневая модель '{}' отсутствует в снимке карты", name),
    )
    .with_code("ST-012")
}

/// Печатает один `FUNCTION_BLOCK`: заголовок, секции объявлений, тело.
///
/// **Тело печатается дважды.** Сначала "вхолостую", в отдельный буфер: только
/// напечатав его, генератор узнаёт, что нужно объявить - поднятые из тела
/// переменные (`st_stmt`) и экземпляры под-FB (`st_model`). А объявления в POU
/// обязаны стоять **до** тела, и второй секции `VAR` быть не может. Поэтому
/// сперва буфер, затем шапка, затем готовый текст тела.
fn emit_function_block(
    p: &mut Printer,
    map: &StMap,
    name: &Name,
    model: &ModelNode,
    is_root: bool,
    named_arrays: &[String],
    array_forms: &[String],
) -> Result<Vec<Diagnostic>, Diagnostic> {
    let element = if is_root {
        map.model()
    } else {
        map.element_of(name)
            .ok_or_else(|| root_missing(name.clone()))?
    };
    let Element::Model { states, .. } = &element else {
        return Err(root_missing(name.clone()));
    };
    let table = st_model::StateTable::build(states);

    // Переменные корня под-модель видит через `VAR_IN_OUT` (О1-в): указателей, как
    // `main->` в цели `c`, в переносимом ST нет.
    let shared = if is_root {
        Vec::new()
    } else {
        map.shared_variables(name)
    };
    // Владелец разделяемых переменных - корень: им квалифицируется имя именованного
    // типа массива, и продюсер типа зовёт ту же функцию, что потребитель.
    let shared_owner = map.root_name().unique().to_string();

    let mut body = String::new();
    let out = {
        let mut bp = Printer::new(INDENT, &mut body);
        bp.up();
        st_model::emit_body(&mut bp, map, &element, model, &table)?
    };

    // Константы таблицы переходов считаются по тем же строкам, что печатает диспетчер:
    // носитель строк один (`generator::table`).
    let table_constants = if map.fsm_table() {
        st_table::constants(map, &element, model, &table)?
    } else {
        Vec::new()
    };
    let extras = st_decl::Extras {
        table_constants,
        state_var: true,
        is_done: true,
        external_ports: map.at_addresses(),
        shared,
        shared_owner,
        // Корень объявляет свои массивы тем же именованным типом, что и под-модели в
        // параметрах, - иначе типы не совпадут.
        root_owner: is_root.then(|| map.root_name().unique().to_string()),
        named_arrays: named_arrays.to_vec(),
        // Формы массивов из параметров функций: именованный тип получают только они,
        // иначе правка задела бы каждый массив вывода.
        array_forms: array_forms.to_vec(),
        instances: out
            .instances
            .iter()
            .map(|i| (i.name.clone(), i.fb_type.clone(), i.init.clone()))
            .collect(),
        hoisted: out
            .stmt
            .hoisted
            .iter()
            .map(|h| (h.name.clone(), h.ty.clone()))
            .collect(),
    };

    // Имя FB совпадает с эмитируемым идентификатором. У корня оно берётся из имени
    // файла (проба 2: `concat.takt` -> `FUNCTION_BLOCK Concat`, а `CONCAT` -
    // стандартная функция IEC), поэтому проверять надо именно эту строку.
    let fb_name = name.unique_camelcase();
    st_reserved::check_st_name(&fb_name, model.loc)?;
    // Алфавит имени состояния: имя попадает в комментарий и в разбор `CASE`, и без
    // проверки не-ASCII доехало бы до `iec2c`. Дыру нашёл тест по видам объявлений: у
    // переменных и портов проверка была, у состояний - нет.
    for state in model.states.values() {
        let (state_name, loc) = match state {
            crate::semantic::StateNode::Simple { name, loc, .. }
            | crate::semantic::StateNode::Implement { name, loc, .. } => (name, *loc),
            crate::semantic::StateNode::Unresolved => continue,
        };
        st_reserved::check_st_state_name(state_name, loc)?;
    }
    // Комментарий автора перед объявлением модели.
    for line in crate::generator::comments::leading(
        model.loc,
        crate::generator::header::CommentStyle::IecBlock,
    ) {
        p.ident(&line).nl();
    }
    let mut header = String::new();
    let _ = write!(header, "FUNCTION_BLOCK {}", fb_name);
    p.ident(&header).nl();
    st_decl::emit_declarations(p, model, map.usage(), &extras)?;
    p.print(&body);
    p.ident("END_FUNCTION_BLOCK").nl().nl();

    Ok(out.stmt.warnings)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic::tree::construct_model;

    /// Строит снимок ST-карты из исходника Takt (по образцу
    /// `plantuml::tests::make_map`).
    fn make_map(src: &str, name: &str) -> StMap {
        let (ast, _) = crate::parse(src, 0).unwrap();
        let model_rc = construct_model(&ast, None, &[]).unwrap();
        model_rc.borrow_mut().name = Some(name.to_string());
        let model = model_rc.borrow();
        StMap::new(name, &model, false, Default::default()).unwrap()
    }

    fn program_of(src: &str, name: &str) -> String {
        generate_program(&make_map(src, name)).unwrap().0
    }

    /// Корневая модель должна порождать `FUNCTION_BLOCK` - несущую конструкцию ST.
    #[test]
    fn test_generate_program_root_model_emits_function_block() {
        let st = program_of("start S;", "Root");
        assert!(
            st.contains("FUNCTION_BLOCK Root"),
            "отсутствует FUNCTION_BLOCK корневой модели:\n{st}"
        );
        assert!(
            st.contains("END_FUNCTION_BLOCK"),
            "отсутствует END_FUNCTION_BLOCK:\n{st}"
        );
    }

    /// Комментарий в IEC 61131-3 - `(* ... *)`; C-формы недопустимы.
    ///
    /// Проверка не косметическая: `//` и `/* */` - синтаксическая ошибка для
    /// компилятора ST, то есть порождённый файл не приняла бы ни одна среда ПЛК.
    #[test]
    fn test_generate_program_uses_iec_comments_not_c_style() {
        let st = program_of("start S;", "Root");
        assert!(st.starts_with("(*"), "ожидался IEC-комментарий:\n{st}");
        assert!(!st.contains("/*"), "C-комментарий недопустим в ST:\n{st}");
        assert!(!st.contains("//"), "C-комментарий недопустим в ST:\n{st}");
    }

    /// Каждый открытый `FUNCTION_BLOCK` должен быть закрыт.
    #[test]
    fn test_generate_program_every_function_block_is_closed() {
        let src = "model A { start S; } start E = A;";
        let st = program_of(src, "Root");
        assert_eq!(
            st.matches("FUNCTION_BLOCK ").count(),
            st.matches("END_FUNCTION_BLOCK").count(),
            "число открытий и закрытий FUNCTION_BLOCK должно совпадать:\n{st}"
        );
    }

    /// Подмодель должна порождать собственный `FUNCTION_BLOCK`.
    ///
    /// Имя - **уникальное** (с путём родителей), а не локальное: подмодель `A` модели
    /// `Root` даёт `RootA`. Так же именует цель `c` (`STACKER_LIFT_CONTROLLER`), и того
    /// же требует (`FUNCTION_BLOCK StackerLiftController`). Причина - в IEC 61131-3
    /// пространство имён `FUNCTION_BLOCK` **плоское**: одноимённые подмодели разных
    /// родителей столкнулись бы.
    #[test]
    fn test_generate_program_submodel_emits_own_function_block() {
        let src = "model A { start S; } start E = A;";
        let st = program_of(src, "Root");
        assert!(
            st.contains("FUNCTION_BLOCK RootA"),
            "отсутствует FUNCTION_BLOCK подмодели:\n{st}"
        );
    }

    /// Подмодель объявляется раньше корня: в IEC 61131-3 тип экземпляра должен быть
    /// известен к моменту объявления экземпляра.
    #[test]
    fn test_generate_program_submodel_declared_before_root() {
        let src = "model A { start S; } start E = A;";
        let st = program_of(src, "Root");
        let sub = st.find("FUNCTION_BLOCK RootA").expect("нет подмодели");
        let root = st.find("FUNCTION_BLOCK Root\n").expect("нет корня");
        assert!(
            sub < root,
            "подмодель должна объявляться раньше корня:\n{st}"
        );
    }

    /// Вывод ST должен быть **воспроизводимым**: одна модель - один и тот же файл.
    ///
    /// Тест против регресса недетерминизма. `used_models()` отдаёт модели в порядке
    /// обхода `HashMap`, поэтому без явной сортировки пять прогонов `taktc -t st
    /// examples/stacker.takt` давали **четыре разных** файла. Для ST это не косметика:
    /// порядок объявлений в IEC 61131-3 значим.
    ///
    /// Тест строит карту заново на каждой итерации - иначе он проверял бы кэш, а не
    /// обход.
    #[test]
    fn test_generate_program_output_is_deterministic() {
        let src = "model A { start S; } model B { start T; } model C { start U; } \
                   start E = A | B | C;";
        let first = program_of(src, "Root");
        for i in 1..8 {
            assert_eq!(
                first,
                program_of(src, "Root"),
                "прогон {i} дал другой вывод — вернулся недетерминизм порядка"
            );
        }
    }

    /// Подмодели печатаются в устойчивом (лексикографическом) порядке.
    #[test]
    fn test_generate_program_submodels_in_stable_order() {
        let src = "model B { start T; } model A { start S; } start E = A | B;";
        let st = program_of(src, "Root");
        let a = st.find("FUNCTION_BLOCK RootA").expect("нет блока A");
        let b = st.find("FUNCTION_BLOCK RootB").expect("нет блока B");
        assert!(
            a < b,
            "порядок подмоделей должен быть устойчивым, а не зависеть от обхода HashMap:\n{st}"
        );
    }

    /// Параллельная композиция даёт по `FUNCTION_BLOCK` на каждую подмодель.
    #[test]
    fn test_generate_program_parallel_composition_emits_block_per_submodel() {
        let src = "model A { start S; } model B { start T; } start E = A | B;";
        let st = program_of(src, "Root");
        assert!(st.contains("FUNCTION_BLOCK RootA"), "нет блока A:\n{st}");
        assert!(st.contains("FUNCTION_BLOCK RootB"), "нет блока B:\n{st}");
        assert_eq!(
            st.matches("END_FUNCTION_BLOCK").count(),
            3,
            "ожидались блоки A, B и корня:\n{st}"
        );
    }
}
