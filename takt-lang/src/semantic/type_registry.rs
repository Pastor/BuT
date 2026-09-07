//! Имя типа занимается один раз - воронка регистрации.
//!
//! # Что здесь решается
//!
//! Объявление типа (`type`, `struct`, `enum`) кладёт имя в `types`/`type_locs` модели.
//!
//! - `type u8 = i64;` - встроенный тип языка **затенялся**, и с этого места
//!   `u8` в файле означал 64-битное знаковое целое; встроенный становился
//!   недостижим;
//! - `struct S { a: u8 } struct S { b: u16 }` - второе объявление **затирало**
//!   первое, а автор узнавал об этом из `SE-061` "структура 'S' не содержит
//!   поля 'a'" - диагностики **о следствии**, уводящей искать опечатку в имени
//!   поля.
//!
//! Соседи по языку так себя не ведут: дубль модели отвергается, коллизия имени условия
//! с переменной - `SE-054`, повторная функция - отказ. Не проверялись ровно типы.
//!
//! # Один носитель списка встроенных имён
//!
//! Список спрашивается у [`builtin_type_by_name`], а не переписывается здесь: своя
//! копия разошлась бы с ним при первом же новом типе. Следствие: `u128` под запрет **не
//! попадает** - встроенным он не является, и `type u128 = [bit; 128];` остаётся
//! законным.

use crate::diagnostics::{Diagnostic, Location};
use crate::semantic::ModelNode;
use crate::semantic::type_node::builtin_type_by_name;
use std::cell::RefCell;
use std::rc::Rc;

/// Занимает имя типа за объявлением в позиции `loc`.
///
/// Отвечает `Err`, если имя принадлежит языку (`SE-107`) либо уже занято другим
/// объявлением в этой модели (`SE-108`, с позицией первого объявления заметкой).
///
/// Вызывается **до** записи в `types`/`type_locs` - из всех трёх мест, где объявляется
/// тип: псевдоним, перечисление, структура.
pub(crate) fn claim_type_name(
    model: &Rc<RefCell<ModelNode>>,
    name: &str,
    loc: Location,
) -> Result<(), Diagnostic> {
    // Пустое имя приходит от неразобранного объявления: его судит парсер, а не эта
    // проверка (иначе на один дефект пришли бы две диагностики).
    if name.is_empty() {
        return Ok(());
    }

    if builtin_type_by_name(name).is_some() {
        return Err(Diagnostic::declaration_error(
            loc,
            format!(
                "имя '{name}' принадлежит встроенному типу языка и не может быть \
                 объявлено заново: выберите другое имя (встроенный тип иначе \
                 станет недоступен во всём файле)"
            ),
        )
        .with_code("SE-107"));
    }

    let first = model.borrow().type_locs.get(name).copied();
    if let Some(first_loc) = first {
        // Первое объявление может прийти из подключённого файла: `import` переносит
        // типы к импортёру вместе с их позициями. Различаем по номеру файла - иначе
        // сообщение отправит автора искать объявление там, где его нет.
        let same_file = matches!(
            (loc, first_loc),
            (Location::Source(a, _, _), Location::Source(b, _, _)) if a == b
        );
        let message = if same_file {
            format!("тип с именем '{name}' уже объявлен")
        } else {
            format!("тип с именем '{name}' уже объявлен — он пришёл из подключённого файла")
        };
        return Err(Diagnostic::error_with_note(
            loc,
            message,
            first_loc,
            format!("первое объявление типа '{name}' — здесь"),
        )
        .with_code("SE-108"));
    }

    Ok(())
}

/// Предпроход: занимает имена всех типов модели до разбора их устройства.
///
/// # Что делает
///
/// 1. Занимает имена `struct`, `enum` и `type` - **единственная** воронка
///    занятия; прежние вызовы из основного цикла сняты, иначе второй
///    вызов на том же имени дал бы ложную `SE-108`.
/// 2. Кладёт в `types` те типы, чьё значение известно **по имени**:
///    `TypeNode::Struct` и `TypeNode::Enum`. Псевдоним значения по имени не
///    имеет - его разрешает [`resolve_aliases`].
///
/// Обход идёт в порядке текста: `SE-108` о дубле обязана называть первым верное
/// объявление.
///
/// # Ошибки
/// `SE-107` (имя принадлежит языку) либо `SE-108` (имя уже занято).
pub(crate) fn predeclare_named_types(
    model: &Rc<RefCell<ModelNode>>,
    elements: &[crate::parser::ast::ModelElement],
) -> Result<(), Diagnostic> {
    use crate::parser::ast::ModelElement;
    for element in elements {
        let (name, loc, ty) = match element {
            ModelElement::Struct(s) => {
                let Some(id) = s.name.as_ref() else { continue };
                (
                    id.name.clone(),
                    id.loc,
                    Some(crate::semantic::type_node::TypeNode::Struct(
                        id.name.clone(),
                    )),
                )
            }
            ModelElement::Enum(e) => {
                let Some(id) = e.name.as_ref() else { continue };
                (
                    id.name.clone(),
                    id.loc,
                    Some(crate::semantic::type_node::TypeNode::Enum(id.name.clone())),
                )
            }
            // Псевдоним занимает имя здесь, а значение получает в `resolve_aliases`:
            // оно есть уже разрешённый тип, и знать его до регистрации соседей нельзя.
            ModelElement::Type(def) => (def.name.name.clone(), def.name.loc, None),
            _ => continue,
        };
        claim_type_name(model, &name, loc)?;
        if name.is_empty() {
            continue;
        }
        let mut bm = model.borrow_mut();
        if let Some(ty) = ty {
            bm.types.insert(name.clone(), ty);
        }
        bm.type_locs.insert(name, loc);
    }
    Ok(())
}

/// Разрешает псевдонимы типов модели до неподвижной точки.
///
/// Значение псевдонима - уже **разрешённый** тип, поэтому одного прохода мало: `type A
/// = B;` выше `type B = u8;` требует второго. Проход повторяется, пока карта растёт.
///
/// **Завершение держит `SE-039`** (Ce16): циклические псевдонимы отсекаются
/// **раньше**, до вызова этой функции, поэтому "до неподвижной точки" не
/// зациклится. Зависимость по порядку проверок, в коде она не видна.
///
/// # Ошибки
/// Диагностика первого псевдонима, не разрешившегося после стабилизации, -
/// как правило `SE-034`: имя действительно не объявлено.
pub(crate) fn resolve_aliases(
    model: &Rc<RefCell<ModelNode>>,
    elements: &[crate::parser::ast::ModelElement],
) -> Result<(), Diagnostic> {
    use crate::parser::ast::ModelElement;
    let mut pending: Vec<&crate::parser::ast::TypeDefine> = elements
        .iter()
        .filter_map(|e| match e {
            ModelElement::Type(def) => Some(&**def),
            _ => None,
        })
        .collect();
    if pending.is_empty() {
        return Ok(());
    }
    loop {
        let before = pending.len();
        let mut rest = Vec::with_capacity(before);
        for def in pending {
            match resolve_alias_value(model, def) {
                Ok(()) => {}
                Err(_) => rest.push(def),
            }
        }
        if rest.is_empty() {
            return Ok(());
        }
        if rest.len() == before {
            // Карта больше не растёт: отдаём настоящую диагностику первого
            // неразрешённого - имя действительно не объявлено.
            let def = rest[0];
            resolve_alias_value(model, def)?;
            return Ok(());
        }
        pending = rest;
    }
}

/// Вычисляет значение одного псевдонима и кладёт его в модель.
///
/// Имя уже занято предпроходом ([`predeclare_named_types`]), поэтому `claim_type_name`
/// здесь **не** зовётся: второй вызов дал бы `SE-108`.
fn resolve_alias_value(
    model: &Rc<RefCell<ModelNode>>,
    def: &crate::parser::ast::TypeDefine,
) -> Result<(), Diagnostic> {
    let name = def.name.name.clone();
    let typ = def.ty.clone();
    // Тип вычисляется до `borrow_mut`: `construct_type` зовёт `borrow` (`search_type`),
    // и одновременный `borrow_mut` был бы паникой.
    let resolved = crate::semantic::type_node::construct_type(Some(typ.clone()), Rc::clone(model))?;
    let mut bm = model.borrow_mut();
    // Сырой АСД-тип нужен проверке циклических псевдонимов (Ce16).
    bm.raw_type_defs.insert(name.clone(), typ);
    bm.types.insert(name.clone(), resolved);
    bm.type_locs.insert(name, def.name.loc);
    Ok(())
}

/// Готовит типы модели: цикл структур, занятие имён, разрешение псевдонимов.
///
/// Порядок шагов значим и держится на нём **две** гарантии:
///
/// 1. `SE-124` проверяется по **сырому** АСД, до регистрации имён: тип
///    бесконечного размера не должен доехать до потребителей;
/// 2. [`resolve_aliases`] завершается потому, что циклы среди псевдонимов
///    отсекает `SE-039` (Ce16) - она проверяется **раньше**, в вызывающем.
///
/// Шаг живёт здесь, а не в `tree.rs`: тот сверх лимита размера, и правило велит
/// выносить новое, а не дописывать туда (тот же довод, что у 0167).
pub(crate) fn prepare_types(
    model: &Rc<RefCell<ModelNode>>,
    elements: &[crate::parser::ast::ModelElement],
) -> Result<(), Diagnostic> {
    if let Some(diag) = crate::semantic::validate::struct_cycle::check_struct_cycles(elements) {
        return Err(diag);
    }
    predeclare_named_types(model, elements)?;
    resolve_aliases(model, elements)
}
