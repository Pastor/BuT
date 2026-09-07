//! Усыновление импортированного поддерева.
//!
//! `import` строит дерево подключаемого файла **отдельным** вызовом
//! `construct_model_impl`, поэтому у всего, что оттуда пришло, родитель (`upper`)
//! указывает на корень **библиотеки**, а не на модель-импортёра. Корень библиотеки
//! после импорта не жив (`Rc` на него уходит вместе с локальной переменной ветки
//! импорта), и дальше каждый потребитель семантического дерева обходится с повисшей
//! привязкой по-своему:
//!
//! - цели `c` и `sv` спрашивают у переменной владельца, не получают его и
//!   печатают доступ "как к своей" - вывод не принимают `cc` и `verilator`,
//!   **при том что `taktc` рапортует успех**;
//! - цели `rust` и `st` строят доступ по имени из списков корня и потому
//!   случайно правы;
//! - симулятор строит цепочку контекстов по `upper` модели и обрывается на
//!   мёртвой ссылке - `SIM-009`.
//!
//! Модуль устраняет причину, а не пять её проявлений: импортированное объявление
//! **становится** объявлением импортёра.
//!
//! # Главная засада: переменная существует в двух представлениях
//!
//! [`VariableNode`] лежит owned в [`ModelNode::variables`] **и** за `Rc<RefCell<...>>`
//! в [`ExpressionNode::Variable`]/[`ConditionNode::Variable`]. Перепривязка одной
//! только карты чинит симулятор (он ищет по цепочке моделей), но **не** цели `c`/`sv` -
//! те спрашивают владельца у ячейки. Поэтому обход идёт по телам, ровно как в
//! [`super::lower_float`], откуда взят и его состав.
//!
//! # Момент вызова
//!
//! Усыновление обязано выполняться **внутри** ветки импорта, пока `Rc` корня библиотеки
//! жив: признак "эта ячейка пришла из библиотеки" - [`Rc::ptr_eq`] владельца с этим
//! корнем. После выхода из ветки отличить такую ячейку от любой другой с мёртвым `Weak`
//! уже нельзя.

use crate::diagnostics::{Diagnostic, Location};
use crate::semantic::formula::Formula;
use crate::semantic::{
    ConditionNode, ExpressionNode, FunctionDefinitionNode, MatchPatternNode, ModelNode,
    NamedCodeBlockDefinitionNode, StateNode, StatementNode, VariableNode,
};
use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::rc::Rc;

/// Контекст усыновления: чьи ячейки перепривязываем, к кому и под какими именами.
struct Adoption {
    /// Корень импортируемого файла - признак принадлежности ячейки библиотеке.
    library: Rc<RefCell<ModelNode>>,
    /// Новый владелец импортированных объявлений.
    importer: Rc<RefCell<ModelNode>>,
    /// Импортированные объявления: имя в библиотеке -> имя в импортёре (`as`).
    renames: BTreeMap<String, String>,
    /// Ссылки на объявления библиотеки, которые импортированы **не были**: имя ->
    /// позиция первой встречи. Основание для `SE-074`.
    missing: BTreeMap<String, Location>,
    /// Уже обойдённые модели (композиция разделяет под-модели по `Rc`).
    visited: HashSet<*const RefCell<ModelNode>>,
    /// Имена, локальные для обходимой функции: параметры и объявления её тела.
    ///
    /// Их ячейки созданы при разрешении в контексте библиотеки и выглядят её
    /// объявлениями, хотя объявлениями не являются: без этого списка обход тела
    /// перенесённой функции докладывает о них как о "неимпортированных объявлениях"
    /// (`SE-074`) - и требует импортировать то, чего библиотека не объявляет вовсе.
    locals: BTreeSet<String>,
    /// Заменять ячейку новой, а не править её на месте.
    ///
    /// У импорта правка на месте верна: ячейка принадлежит библиотеке, которая после
    /// импорта не жива, и делить её не с кем. У **копии- специализации** делить есть с
    /// кем: `ModelNode::copy` клонирует узлы тел, но `Rc`-ячейки внутри них разделяет с
    /// исходной моделью, - правка на месте испортила бы исходную (замер 0395: вторая
    /// специализация печатала доступ через поле первой, `cc`: "no member named 'run'").
    fresh_cells: bool,
}

/// Усыновляет **выборочно импортированную** модель: `import { M } from "lib";`.
///
/// Перепривязывает саму модель к импортёру (и даёт ей имя `alias`, под которым она
/// внесена в список моделей импортёра) и обходит её поддерево, заменяя владельца у
/// ячеек, пришедших из корня библиотеки. `renames` перечисляет объявления,
/// импортированные вместе с моделью (имя в библиотеке -> имя в импортёре); ссылка на
/// объявление библиотеки **вне** этого списка - ошибка `SE-074`, а не повисшая
/// привязка.
///
/// # Ошибки
///
/// `SE-074`, если тело импортированной модели ссылается на объявление подключаемого
/// файла, не импортированное вместе с ней.
pub(in crate::semantic) fn adopt_selected_model(
    model: &Rc<RefCell<ModelNode>>,
    library: &Rc<RefCell<ModelNode>>,
    importer: &Rc<RefCell<ModelNode>>,
    alias: &str,
    renames: &BTreeMap<String, String>,
) -> Result<(), Diagnostic> {
    let mut ctx = Adoption {
        library: Rc::clone(library),
        importer: Rc::clone(importer),
        renames: renames.clone(),
        missing: BTreeMap::new(),
        visited: HashSet::new(),
        locals: BTreeSet::new(),
        fresh_cells: false,
    };
    // Выбранная модель вносится в дерево импортёра - значит её владелец теперь
    // импортёр. Её собственные под-модели остаются при ней (см.
    //
    // Имя узла меняется на то, под которым модель внесена в список импортёра (`import {
    // Worker as W1 }`): уникальное имя строится обходом `upper` от имени узла, и без
    // переименования цель `c` ищет `Файл:Worker`, а зарегистрирована `Файл:W1` -
    // `CC-004` (дефект существовал и до фичи).
    {
        let mut b = model.borrow_mut();
        b.upper = Some(Rc::downgrade(importer));
        b.name = Some(alias.to_string());
    }
    adopt_model(&mut ctx, model);
    ctx.report()
}

/// Перепривязывает тела **копии-специализации** к ней самой.
///
/// Модель, пришедшая из другого файла, к моменту специализации уже прошла весь
/// конвейер: её тела разрешены, и в них лежат ячейки-снимки
/// (`Rc<RefCell<VariableNode>>`) с владельцем - **исходной** моделью. `copy` клонирует
/// узлы тел, но владельца ячеек не меняет, и цель `c` печатала доступ через поле
/// исходной модели - `model->tuner.out_value = CONST_APP_TUNER_GAIN;` в теле копии, то
/// есть `cc`: "no member named 'tuner'" при **нулевом** коде возврата `taktc`. Ровно
/// поэтому закрыла вход отказом `SE-120`.
///
/// Приём - тот же, что у импорта: признак "ячейка чужая" это `Rc::ptr_eq` владельца с
/// исходной моделью, и после смены владельца доступ строится по копии. Имена не
/// меняются (`renames` тождественны), поэтому `SE-074` недостижим: карта имён взята у
/// самой исходной модели.
///
/// **Вызывать обязательно до того, как копия внесена в дерево**: признак принадлежности -
/// живой `Rc` исходной модели, ровно как у импорта.
///
/// Вложенных моделей у специализируемой модели не бывает (`SE-087`), поэтому обход
/// поддерева упирается в один узел.
pub(in crate::semantic) fn adopt_specialized_copy(
    copy: &Rc<RefCell<ModelNode>>,
    source: &Rc<RefCell<ModelNode>>,
) {
    // Тождественные имена: копия несёт ту же карту объявлений, что исходная, -
    // переименования здесь нет по существу, а без записи в `renames` ячейка попала бы в
    // `missing` и обход счёл бы её незаимпортированной.
    let renames: BTreeMap<String, String> = source
        .borrow()
        .variables
        .keys()
        .map(|name| (name.clone(), name.clone()))
        .collect();
    let mut ctx = Adoption {
        library: Rc::clone(source),
        importer: Rc::clone(copy),
        renames,
        missing: BTreeMap::new(),
        visited: HashSet::new(),
        locals: BTreeSet::new(),
        fresh_cells: true,
    };
    adopt_subtree(&mut ctx, copy);
}

/// Усыновляет **весь импортированный файл**: `import "lib.takt";` (и форма `as Имя`).
/// Корень библиотеки становится под-моделью импортёра и получает имя, под которым
/// внесён в список моделей - без имени цель `c` отказывает `CC-004` на пустом имени.
///
/// Верхнеуровневые объявления подключённого файла **переносятся к импортёру**, как и
/// при выборочном импорте. Оставить их у корня библиотеки нельзя: он стал промежуточной
/// моделью, а обращение к переменной **промежуточной** модели цели генерации не
/// выражают - цель `c` печатает доступ через несуществующее поле (проба 0184:
/// `model->app.pidlib0.z` изнутри вложенной модели). Перенос делает такое объявление
/// обычной переменной корня, видимой всему поддереву.
///
/// # Ошибки
///
/// `SE-005`, если имя объявления подключённого файла уже занято у импортёра: молча
/// затенять чужое объявление нельзя.
pub(in crate::semantic) fn adopt_whole_file(
    library: &Rc<RefCell<ModelNode>>,
    importer: &Rc<RefCell<ModelNode>>,
    name: &str,
    variables: &mut BTreeMap<String, VariableNode>,
) -> Result<(), Diagnostic> {
    {
        let mut b = library.borrow_mut();
        b.upper = Some(Rc::downgrade(importer));
        if b.name.is_none() {
            b.name = Some(name.to_string());
        }
    }
    let moved: Vec<(String, VariableNode)> =
        library.borrow().variables.clone().into_iter().collect();
    let mut renames = BTreeMap::new();
    for (var_name, mut var) in moved {
        if let Some(existing) = variables.get(&var_name) {
            return Err(Diagnostic::declaration_error(
                existing.loc(),
                format!(
                    "Переменная '{var_name}' уже объявлена (подключённый файл '{name}' \
                     объявляет её же)"
                ),
            )
            .with_code("SE-005"));
        }
        adopt_declaration(&mut var, importer, &var_name);
        renames.insert(var_name.clone(), var_name.clone());
        variables.insert(var_name, var);
    }
    library.borrow_mut().variables.clear();

    // Типы, их устройство и функции переносятся по той же причине, что и переменные:
    // корень библиотеки стал промежуточной моделью, а обращение к её объявлениям цели
    // генерации не выражают.
    let (types, structs, enums, fns, type_locs) = {
        let b = library.borrow();
        (
            b.types.clone(),
            b.structs.clone(),
            b.enums.clone(),
            b.functions.clone(),
            b.type_locs.clone(),
        )
    };
    // имя типа занимается один раз, и граница файла этого не меняет. Столкновение
    // возможно в обе стороны, поэтому проверяются обе:
    //
    // - импорт идёт после локального объявления - ловится здесь (иначе
    //   `extend` молча заменил бы локальный тип библиотечным);
    // - импорт идёт до - ловится воронкой `claim_type_name` на локальном
    //   объявлении, потому что позиции библиотеки перенесены строкой ниже.
    //
    // Позиция заметки принадлежит своему файлу (у `Location` есть `file_no`), поэтому
    // автор видит обе стороны столкновения, а не только свою.
    for (type_name, first_loc) in &type_locs {
        let clash = importer.borrow().type_locs.get(type_name).copied();
        if let Some(local_loc) = clash {
            return Err(Diagnostic::error_with_note(
                local_loc,
                format!(
                    "тип с именем '{type_name}' уже объявлен: подключённый файл                      '{name}' объявляет его же"
                ),
                *first_loc,
                format!("объявление типа '{type_name}' в подключённом файле — здесь"),
            )
            .with_code("SE-108"));
        }
    }
    {
        let mut imp = importer.borrow_mut();
        imp.types.extend(types);
        imp.structs.extend(structs);
        imp.enums.extend(enums);
        // Позиции переносятся вместе с типами: без них воронка 0243 не сможет назвать
        // первое объявление, а `SE-108` без второй позиции - половина сообщения.
        imp.type_locs.extend(type_locs);
    }
    let mut moved: Vec<String> = Vec::new();
    for (fn_name, mut f) in fns {
        if importer.borrow().functions.contains_key(&fn_name) {
            return Err(Diagnostic::declaration_error(
                Location::default(),
                format!(
                    "Функция '{fn_name}' уже определена (подключённый файл '{name}' \
                     определяет её же)"
                ),
            )
            .with_code("SE-009"));
        }
        // Владелец - импортёр: цель `c` строит имя функции из него.
        if let FunctionDefinitionNode::Local { upper, .. } = &mut f {
            *upper = Some(Rc::downgrade(importer));
        }
        moved.push(fn_name.clone());
        importer.borrow_mut().functions.insert(fn_name, f);
    }
    library.borrow_mut().functions.clear();

    let mut ctx = Adoption {
        library: Rc::clone(library),
        importer: Rc::clone(importer),
        renames,
        missing: BTreeMap::new(),
        visited: HashSet::new(),
        locals: BTreeSet::new(),
        fresh_cells: false,
    };
    // Тела перенесённых функций обходятся отдельно: к этому моменту они уже изъяты из
    // библиотеки (`functions.clear()`), и обход её поддерева их не видит. Без этого
    // ячейки вызовов внутри них оставались привязанными к прежнему владельцу, и при
    // Транзитивном импорте цели `c` и `st` печатали определение и вызов с разными
    // префиксами - "call to undeclared function" при нулевом коде возврата `taktc`.
    for name in &moved {
        let mut taken = importer.borrow_mut().functions.remove(name);
        if let Some(f) = taken.as_mut() {
            adopt_function(&mut ctx, f);
        }
        if let Some(f) = taken {
            importer.borrow_mut().functions.insert(name.clone(), f);
        }
    }
    // Обход начинается с самого корня библиотеки: его состояния и блоки тоже ссылаются
    // на перенесённые объявления.
    adopt_subtree(&mut ctx, library);
    ctx.report()
}

impl Adoption {
    /// Принадлежит ли объявление корню импортируемого файла.
    fn is_library_owned(&self, var: &VariableNode) -> bool {
        var.upper()
            .is_some_and(|owner| Rc::ptr_eq(&owner, &self.library))
    }

    /// Перепривязывает ячейку переменной, если она пришла из библиотеки.
    ///
    /// Имя меняется вместе с владельцем: объявление могло быть импортировано под
    /// псевдонимом (`import { meas as pv }`), и тело обязано ссылаться на то имя, под
    /// которым объявление живёт у импортёра, - иначе генератор напечатает доступ к
    /// несуществующему полю.
    fn adopt_var_cell(&mut self, cell: &mut Rc<RefCell<VariableNode>>) {
        if self.locals.contains(cell.borrow().name()) {
            return; // параметр либо локальная тела - не объявление библиотеки
        }
        let (owned, name, loc) = {
            let b = cell.borrow();
            (self.is_library_owned(&b), b.name().to_string(), b.loc())
        };
        if !owned {
            return;
        }
        let Some(alias) = self.renames.get(&name).cloned() else {
            self.missing.entry(name).or_insert(loc);
            return;
        };
        if self.fresh_cells {
            // Ячейка разделена с исходной моделью - правка на месте испортила бы её.
            // Снимок берётся с той же ячейки, поэтому значение и тип сохраняются, а
            // владелец становится своим.
            let mut copy = cell.borrow().clone();
            set_upper(&mut copy, &self.importer);
            set_name(&mut copy, alias);
            *cell = Rc::new(RefCell::new(copy));
            return;
        }
        let mut b = cell.borrow_mut();
        set_upper(&mut b, &self.importer);
        set_name(&mut b, alias);
    }

    /// Итог обхода: либо всё усыновлено, либо `SE-074` о первой пропущенной зависимости
    /// (перечисляя остальные - диагностика должна называть все, а не заставлять чинить
    /// их по одной).
    fn report(self) -> Result<(), Diagnostic> {
        let mut it = self.missing.into_iter();
        let Some((first, loc)) = it.next() else {
            return Ok(());
        };
        let mut names = vec![first];
        names.extend(it.map(|(n, _)| n));
        let list = names
            .iter()
            .map(|n| format!("'{n}'"))
            .collect::<Vec<_>>()
            .join(", ");
        Err(Diagnostic::error(
            loc,
            format!(
                "импортированная модель использует объявления подключаемого файла, \
                 не импортированные вместе с ней: {list}. Добавьте их в список импорта"
            ),
        )
        .with_code("SE-074"))
    }
}

fn set_upper(var: &mut VariableNode, importer: &Rc<RefCell<ModelNode>>) {
    match var {
        VariableNode::Simple { upper, .. }
        | VariableNode::Port { upper, .. }
        | VariableNode::Const { upper, .. } => *upper = Some(Rc::downgrade(importer)),
        VariableNode::Unresolved => {}
    }
}

fn set_name(var: &mut VariableNode, alias: String) {
    match var {
        VariableNode::Simple { name, .. }
        | VariableNode::Port { name, .. }
        | VariableNode::Const { name, .. } => *name = alias,
        VariableNode::Unresolved => {}
    }
}

/// Перепривязывает объявление, взятое из библиотеки, к импортёру: вызывается для копии,
/// которую ветка импорта кладёт в карту переменных импортёра.
pub(in crate::semantic) fn adopt_declaration(
    var: &mut VariableNode,
    importer: &Rc<RefCell<ModelNode>>,
    alias: &str,
) {
    set_upper(var, importer);
    set_name(var, alias.to_string());
}

/// Обходит модель и её поддерево, перепривязывая ячейки библиотеки. Сам корень
/// библиотеки пропускается: при выборочном импорте он не входит в дерево импортёра (в
/// него приходят только выбранные модели).
fn adopt_model(ctx: &mut Adoption, model: &Rc<RefCell<ModelNode>>) {
    if Rc::ptr_eq(model, &ctx.library) {
        return;
    }
    adopt_subtree(ctx, model);
}

/// Как [`adopt_model`], но обходит и сам переданный узел - вход для импорта файла
/// целиком, где корень библиотеки становится под-моделью импортёра.
fn adopt_subtree(ctx: &mut Adoption, model: &Rc<RefCell<ModelNode>>) {
    if !ctx.visited.insert(Rc::as_ptr(model)) {
        return; // разделяемая под-модель уже обойдена
    }
    // Владельца самих моделей поддерева обход не меняет: под-модель библиотеки остаётся
    // под-моделью своей модели, иначе её уникальное имя (цепочка `upper`) разъедется с
    // местом в дереве - цель `c` ищет `App:Inner`, а зарегистрирована
    // `App:Pidlib:Inner` (`CC-004`). Перепривязывается только тот узел, который
    // вносится в дерево импортёра: выбранная модель ([`adopt_selected_model`]) либо
    // корень подключённого файла ([`adopt_whole_file`]).
    let nested: Vec<Rc<RefCell<ModelNode>>> = model.borrow().models.values().cloned().collect();
    {
        let mut b = model.borrow_mut();
        let fnames: Vec<String> = b.functions.keys().cloned().collect();
        for fname in fnames {
            let mut f = b.functions.get(&fname).cloned().unwrap();
            adopt_function(ctx, &mut f);
            b.functions.insert(fname, f);
        }
        for blk in b.named_blocks.iter_mut() {
            adopt_block(ctx, blk);
        }
        for c in b.conditions.values_mut() {
            adopt_cond(ctx, &mut c.value);
        }
        for st in b.states.values_mut() {
            adopt_state(ctx, st);
        }
    }
    for child in &nested {
        adopt_model(ctx, child);
    }
}

/// Перепривязывает ячейку вызова функции, если её владелец - библиотека.
///
/// Ячейка **заменяется**, а не правится на месте: `Rc` разделяется с другими
/// употреблениями, и правка задела бы их все (тот же приём, что у копии- специализации,
/// 0395).
fn adopt_function_cell(ctx: &mut Adoption, cell: &mut Rc<RefCell<FunctionDefinitionNode>>) {
    let owned = match &*cell.borrow() {
        FunctionDefinitionNode::Local { upper, .. }
        | FunctionDefinitionNode::External { upper, .. } => upper
            .as_ref()
            .and_then(|w| w.upgrade())
            .is_some_and(|owner| Rc::ptr_eq(&owner, &ctx.library)),
        _ => false,
    };
    if !owned {
        return;
    }
    let mut copy = cell.borrow().clone();
    match &mut copy {
        FunctionDefinitionNode::Local { upper, .. }
        | FunctionDefinitionNode::External { upper, .. } => {
            *upper = Some(Rc::downgrade(&ctx.importer));
        }
        _ => {}
    }
    *cell = Rc::new(RefCell::new(copy));
}

fn adopt_function(ctx: &mut Adoption, f: &mut FunctionDefinitionNode) {
    if let FunctionDefinitionNode::Local { body, params, .. } = f {
        // Локальные имена функции: параметры плюс объявления тела. Список собирает
        // общий носитель - второй обход разошёлся бы с первым.
        let mut locals: BTreeSet<String> = params.iter().map(|(name, _)| name.clone()).collect();
        let mut declared = std::collections::HashSet::new();
        crate::semantic::fresh::collect_locals(body, &mut declared);
        locals.extend(declared);
        let outer = std::mem::replace(&mut ctx.locals, locals);
        adopt_stmt(ctx, body);
        ctx.locals = outer;
    }
}

fn adopt_block(ctx: &mut Adoption, blk: &mut NamedCodeBlockDefinitionNode) {
    match blk {
        NamedCodeBlockDefinitionNode::Enter { body, .. }
        | NamedCodeBlockDefinitionNode::Exit { body, .. }
        | NamedCodeBlockDefinitionNode::Always { body, .. }
        | NamedCodeBlockDefinitionNode::Unknown { body, .. }
        | NamedCodeBlockDefinitionNode::Every { body, .. } => adopt_stmt(ctx, body),
        NamedCodeBlockDefinitionNode::None | NamedCodeBlockDefinitionNode::Unresolved(_, _) => {}
    }
}

/// Обход состояния: тела именованных блоков, условия рёбер и формулы.
///
/// **Условия рёбер и формулы обходятся с.** Прежде обход брал только `named_blocks`, и
/// для импорта дыра не проявлялась: ячейки условий импортированной модели принадлежат
/// ей самой, а перепривязки требуют лишь пришедшие от корня библиотеки. Специализация
/// же меняет владельца **всех** ячеек модели, и без этих полей условие `ref Idle:
/// out_value > 0;` копии читало поле исходной модели - `model->tuner.out_value` в теле
/// `TunerP1`.
///
/// `next` - **отдельное поле**, а не элемент `references`: обход, взявший только
/// список, теряет безусловный переход.
fn adopt_state(ctx: &mut Adoption, st: &mut StateNode) {
    match st {
        StateNode::Simple {
            named_blocks,
            references,
            formulas,
            ..
        } => {
            for blk in named_blocks.iter_mut() {
                adopt_block(ctx, blk);
            }
            for r in references.iter_mut() {
                adopt_cond(ctx, &mut r.cond);
            }
            for f in formulas.iter_mut() {
                adopt_formula(ctx, f);
            }
        }
        StateNode::Implement {
            named_blocks,
            references,
            next,
            formulas,
            ..
        } => {
            for blk in named_blocks.iter_mut() {
                adopt_block(ctx, blk);
            }
            for r in references.iter_mut() {
                adopt_cond(ctx, &mut r.cond);
            }
            if let Some(n) = next.as_mut() {
                adopt_cond(ctx, &mut n.cond);
            }
            for f in formulas.iter_mut() {
                adopt_formula(ctx, f);
            }
        }
        StateNode::Unresolved => {}
    }
}

/// Обход формулы: охранное условие несёт ячейки так же, как условие ребра.
///
/// `Formula::LTL` ячеек не содержит - её атомы это имена состояний и предикаты по
/// сырому АСД (`verification/ltl.rs`), разбираемые отдельно.
fn adopt_formula(ctx: &mut Adoption, f: &mut Formula) {
    match f {
        Formula::Guard(cond, _, _) => adopt_cond(ctx, cond),
        Formula::Formulas(items) => {
            for item in items.iter_mut() {
                adopt_formula(ctx, item);
            }
        }
        Formula::None | Formula::LTL(_, _) => {}
    }
}

fn adopt_stmt(ctx: &mut Adoption, stmt: &mut StatementNode) {
    match stmt {
        StatementNode::Block(stmts) => {
            for s in stmts.iter_mut() {
                adopt_stmt(ctx, s);
            }
        }
        StatementNode::Expression(e, _) => adopt_expr(ctx, e),
        StatementNode::If {
            cond, then_, else_, ..
        } => {
            adopt_expr(ctx, cond);
            adopt_stmt(ctx, then_);
            if let Some(e) = else_ {
                adopt_stmt(ctx, e);
            }
        }
        StatementNode::Loop { cond, body, .. } => {
            if let Some(c) = cond {
                adopt_expr(ctx, c);
            }
            adopt_stmt(ctx, body);
        }
        StatementNode::For {
            init,
            cond,
            step,
            body,
            ..
        } => {
            if let Some(s) = init {
                adopt_stmt(ctx, s);
            }
            if let Some(c) = cond {
                adopt_expr(ctx, c);
            }
            if let Some(s) = step {
                adopt_expr(ctx, s);
            }
            adopt_stmt(ctx, body);
        }
        StatementNode::Variable(_, _, init, _) => {
            if let Some(e) = init {
                adopt_expr(ctx, e);
            }
        }
        StatementNode::Return(Some(e), _) => adopt_expr(ctx, e),
        StatementNode::Match { expr, arms, .. } => {
            adopt_expr(ctx, expr);
            for arm in arms.iter_mut() {
                for p in arm.patterns.iter_mut() {
                    if let MatchPatternNode::Value(e) = p {
                        adopt_expr(ctx, e);
                    }
                }
                adopt_stmt(ctx, &mut arm.body);
            }
        }
        // Вставка для цели - операторы Takt: обход спускается в тело. Блок формул
        // адресован внешнему анализатору, операторов Takt в нём нет.
        StatementNode::Assembly { body, .. } => adopt_stmt(ctx, body),
        StatementNode::None
        | StatementNode::Unresolved(_)
        | StatementNode::Return(None, _)
        | StatementNode::Continue(_)
        | StatementNode::Break(_)
        | StatementNode::Formula(_)
        | StatementNode::InlineFormula(_) => {}
    }
}

fn adopt_expr(ctx: &mut Adoption, expr: &mut ExpressionNode) {
    match expr {
        ExpressionNode::Add(l, r)
        | ExpressionNode::Subtract(l, r)
        | ExpressionNode::Multiply(l, r)
        | ExpressionNode::Divide(l, r)
        | ExpressionNode::Modulo(l, r)
        | ExpressionNode::Power(l, r)
        | ExpressionNode::ShiftLeft(l, r)
        | ExpressionNode::ShiftRight(l, r)
        | ExpressionNode::BitwiseAnd(l, r)
        | ExpressionNode::BitwiseXor(l, r)
        | ExpressionNode::BitwiseOr(l, r)
        | ExpressionNode::Less(l, r)
        | ExpressionNode::More(l, r)
        | ExpressionNode::LessEqual(l, r)
        | ExpressionNode::MoreEqual(l, r)
        | ExpressionNode::Equal(l, r)
        | ExpressionNode::NotEqual(l, r)
        | ExpressionNode::And(l, r)
        | ExpressionNode::Or(l, r)
        | ExpressionNode::Assign(l, r) => {
            adopt_expr(ctx, l);
            adopt_expr(ctx, r);
        }
        ExpressionNode::Parenthesis(e)
        | ExpressionNode::BitAccess(e, _)
        | ExpressionNode::NamedFunctionBox(e, _)
        | ExpressionNode::Not(e)
        | ExpressionNode::BitwiseNot(e)
        | ExpressionNode::UnaryPlus(e)
        | ExpressionNode::Negate(e)
        | ExpressionNode::Cast(e, _) => adopt_expr(ctx, e),
        ExpressionNode::ConditionalOperator(c, t, e) => {
            adopt_expr(ctx, c);
            adopt_expr(ctx, t);
            adopt_expr(ctx, e);
        }
        ExpressionNode::CodeBlock(e, stmt) => {
            adopt_expr(ctx, e);
            adopt_stmt(ctx, stmt);
        }
        ExpressionNode::Function(def, args) => {
            // Ячейка вызова перепривязывается наравне с объявлением. Объявление
            // усыновляет импортёр (`upper = importer`), а в теле стоит своя `Rc` со
            // старым владельцем - при транзитивном импорте цели `c` и `st` печатали
            // определение `Probe_base_value`, а вызов `ProbeMid_base_value`: `cc`
            // отвечал "call to undeclared function" при нулевом коде возврата `taktc`.
            adopt_function_cell(ctx, def);
            for a in args.iter_mut() {
                adopt_expr(ctx, a);
            }
        }
        ExpressionNode::Array(args) | ExpressionNode::Initializer(args) => {
            for a in args.iter_mut() {
                adopt_expr(ctx, a);
            }
        }
        // База - выражение: усыновляется тем же обходом.
        ExpressionNode::ArraySubscript(base, idx) => {
            adopt_expr(ctx, base);
            adopt_expr(ctx, idx);
        }
        ExpressionNode::ArraySlice(base, _, _) => adopt_expr(ctx, base),
        ExpressionNode::Variable(v) => ctx.adopt_var_cell(v),
        ExpressionNode::None
        | ExpressionNode::Unresolved(_)
        | ExpressionNode::Number(_)
        | ExpressionNode::Duration(_)
        | ExpressionNode::Rational(_, _)
        | ExpressionNode::String(_)
        | ExpressionNode::Type(_)
        | ExpressionNode::Address(_, _)
        // Обращение по адресу объявления не имеет: усыновлять нечего.
        | ExpressionNode::AnonPort(_)
        | ExpressionNode::Bool(_)
        | ExpressionNode::Model(_)
        | ExpressionNode::Condition(_)
        | ExpressionNode::List(_) => {}
    }
}

fn adopt_cond(ctx: &mut Adoption, cond: &mut ConditionNode) {
    match cond {
        ConditionNode::Add(l, r)
        | ConditionNode::Subtract(l, r)
        | ConditionNode::And(l, r)
        | ConditionNode::Or(l, r)
        | ConditionNode::Less(l, r)
        | ConditionNode::More(l, r)
        | ConditionNode::LessEqual(l, r)
        | ConditionNode::MoreEqual(l, r)
        | ConditionNode::Equal(l, r)
        | ConditionNode::NotEqual(l, r) => {
            adopt_cond(ctx, l);
            adopt_cond(ctx, r);
        }
        ConditionNode::Parenthesis(e) | ConditionNode::Not(e) | ConditionNode::BitAccess(e, _) => {
            adopt_cond(ctx, e)
        }
        ConditionNode::ArraySubscript(base, idx) => {
            adopt_cond(ctx, base);
            adopt_cond(ctx, idx);
        }
        ConditionNode::Function(_, args, _) => {
            for a in args.iter_mut() {
                adopt_cond(ctx, a);
            }
        }
        ConditionNode::Variable(v, _) => ctx.adopt_var_cell(v),
        ConditionNode::AfterExpr(inner) => adopt_cond(ctx, inner),
        ConditionNode::None
        | ConditionNode::Unresolved(_)
        | ConditionNode::Number(_)
        | ConditionNode::Duration(_)
        | ConditionNode::After(_)
        | ConditionNode::AfterTicks(_)
        | ConditionNode::Rational(_, _)
        | ConditionNode::String(_)
        | ConditionNode::Bool(_)
        | ConditionNode::AnonPort(_)
        | ConditionNode::Model(_, _)
        | ConditionNode::State(..)
        | ConditionNode::EnumVariant(_, _, _) => {}
    }
}
