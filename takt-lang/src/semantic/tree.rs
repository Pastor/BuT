//! Построение семантического дерева из АСД языка Takt.
//!
//! Основные функции модуля:
//! - [`construct_model`] - главная точка входа, строит [`ModelNode`] из [`Model`].
//! - [`construct_states`] - извлекает состояния и разрешает ссылки между ними.
//! - [`construct_context_model`] - строит контекст (вложенные модели) для модели.
//! - [`construct_context_state`] - строит контекст для состояния (заглушка).
//! - [`construct_condition`] - преобразует условие АСД в семантическое условие.

use crate::diagnostics::{Diagnostic, FileTable, Location};
use crate::parse;
use crate::parser::ast;
use crate::parser::ast::{Identifier, ImportDefine, Model, ModelElement, StateElement, StateKind};
use crate::semantic::condition::{extract_conditions, resolve_condition};
use crate::semantic::declaration;
use crate::semantic::extend::Extend;
use crate::semantic::formula;
use crate::semantic::import::adopt as import_adopt;
use crate::semantic::import::build as import_build;
use crate::semantic::import::read_import_file;
use crate::semantic::import::select as import_select;
use crate::semantic::named_block::resolve_named_blocks;
use crate::semantic::naming::normalize_camelcase_name;
use crate::semantic::type_node::{TypeNode, construct_type};
use crate::semantic::validate::{
    check_implicit_bool_conditions, check_transition_completeness, check_type_alias_cycles_ast,
    validate_model, warn_nested_model_ports,
};
use crate::semantic::{
    ConditionDefinitionNode, ConditionNode, ExpressionNode, Formula, FunctionDefinitionNode,
    ModelNode, ModelOrigin, NamedCodeBlockDefinitionNode, ParameterNode, ReferenceNode, StateNode,
    StateNodeKind, StatementNode, extend,
};
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::rc::Rc;

/// Извлекает имя из опционального [`Identifier`].
///
/// Возвращает [`Diagnostic`]-ошибку с указанной позицией, если идентификатор
/// отсутствует.
#[inline]
fn extract_name(id: Option<Identifier>, loc: Location) -> Result<String, Diagnostic> {
    if let Some(id) = id {
        Ok(id.name.clone())
    } else {
        Err(Diagnostic::error(loc, "Идентификатор не задан".to_string()).with_code("SE-021"))
    }
}

/// Проверяет, не создаёт ли импорт файла `new_file` цикл в текущем стеке обработки.
///
/// Если `new_file` уже присутствует в `import_stack`, значит мы столкнулись с
/// циклической зависимостью. В этом случае возвращается [`Diagnostic`]-ошибка с
/// цепочкой вида `a.takt -> b.takt -> a.takt`.
///
/// # Примеры цикла
///
/// ```text
/// Циклический импорт: /src/a.takt -> /src/b.takt -> /src/a.takt
/// ```
fn check_import_cycle(
    import_stack: &[String],
    new_file: &str,
    loc: Location,
) -> Result<(), Diagnostic> {
    if let Some(pos) = import_stack.iter().position(|f| f == new_file) {
        // Строим цепочку начиная с точки входа цикла
        let mut chain: Vec<&str> = import_stack[pos..].iter().map(|s| s.as_str()).collect();
        chain.push(new_file);
        return Err(
            Diagnostic::error(loc, format!("Циклический импорт: {}", chain.join(" → ")))
                .with_code("SE-013"),
        );
    }
    Ok(())
}

/// Путь файла, чьи импорты сейчас обрабатываются.
///
/// Вершина стека импортов, а если он пуст (импорт из корневого файла) - корень из
/// реестра. Реестр-однодневка (`construct_model` без путей) корня не знает - тогда
/// `None`.
fn importer_path<'a>(import_stack: &'a [String], files: &'a FileTable) -> Option<&'a str> {
    import_stack
        .last()
        .map(String::as_str)
        .or_else(|| files.path(0))
}

/// Пути поиска импорта с добавленным каталогом **импортирующего файла**.
///
/// `import "lib.takt";` в `main.takt` требовал `-I <каталог main.takt>`, иначе
/// `SE-013`. Это расходилось с интуицией (`#include "x.h"`, Python, JS ищут рядом) и
/// делало `import` непригодным без настройки.
fn search_paths_with_importer_dir(
    search_paths: &[String],
    import_stack: &[String],
    files: &FileTable,
) -> Vec<String> {
    let mut paths = search_paths.to_vec();
    if let Some(parent) = importer_path(import_stack, files)
        .map(std::path::Path::new)
        .and_then(std::path::Path::parent)
    {
        // Путь без каталога (`main.takt`) даёт пустого родителя - это "здесь".
        let dir = if parent.as_os_str().is_empty() {
            ".".to_string()
        } else {
            parent.to_string_lossy().into_owned()
        };
        if !paths.contains(&dir) {
            paths.push(dir);
        }
    }
    paths
}

/// Отмечает диагностику из импортированного файла местом её `import`.
///
/// Заметка ставится **на каждом** уровне всплытия, поэтому у ошибки из `top -> mid ->
/// deep` их две: `deep` импортирован в `mid`, `mid` - в `top`. Это и есть цепочка
/// импорта - то, что показывает `rustc`, и то, по чему редактор находит место в
/// **открытом** документе: собственные координаты ошибки указывают в текст чужого
/// файла.
fn note_imported_here(
    d: Diagnostic,
    import_loc: Location,
    filename: &str,
    importer: Option<&str>,
) -> Diagnostic {
    let what = crate::semantic::import::short_name(filename);
    let message = match importer.map(crate::semantic::import::short_name) {
        // Сообщение самодостаточно: `taktc` печатает текст заметки, но не её позицию,
        // поэтому "импортировано здесь" без имён не сказало бы ничего.
        Some(where_) => format!("'{what}' импортирован в '{where_}'"),
        None => format!("'{what}' импортирован здесь"),
    };
    d.with_note(import_loc, message)
}

/// Помечает узел как пришедший через `import`.
///
/// Вызывается в **каждой** из трёх точек вставки импорта - `Plain`, `GlobalSymbol` и
/// `Rename`: иначе импортированная модель неотличима от локальной вложенной (обе живут
/// в одном [`ModelNode::models`]), и область проверки `taktc verify --scope file` молча
/// пропустит форму, которую забыли.
///
/// Для `Rename` узел приходит `Rc::clone`-ом чужого дерева. Гонки за него нет:
/// дерево-источник дропается сразу после блока, а один и тот же файл при повторном
/// импорте разбирается заново (кэша импортов нет).
fn mark_imported(model: Rc<RefCell<ModelNode>>) -> Rc<RefCell<ModelNode>> {
    model.borrow_mut().origin = ModelOrigin::Imported;
    model
}

/// `specialize` - режим `--parameters=specialize`. Стадия сама им не пользуется, но
/// **передаёт** подключаемым файлам: они строятся тем же конвейером, и режим сборки
/// обязан быть у них тот же, иначе специализация обходит библиотеку стороной.
pub(super) fn construct_model_stage0(
    model: &Model,
    upper: Option<Rc<RefCell<ModelNode>>>,
    search_paths: &[String],
    import_stack: &mut Vec<String>,
    files: &mut FileTable,
    specialize: bool,
) -> Result<Rc<RefCell<ModelNode>>, Diagnostic> {
    let name = model.name.clone();

    let model_node = ModelNode {
        upper: upper.as_ref().map(Rc::downgrade),
        loc: model.loc,
        name: name.map(|i| i.name.clone()),
        implements: model
            .implements
            .clone()
            .map(Extend::Unresolved)
            .unwrap_or(Extend::None),
        ..Default::default()
    };
    let model_node = Rc::new(RefCell::new(model_node));
    let mut models = BTreeMap::new();
    let mut variables = BTreeMap::new();
    // Параметры модели - в порядке объявления.
    let mut parameters: Vec<ParameterNode> = Vec::new();
    let mut conditions = BTreeMap::new();
    let mut named_blocks = Vec::new();
    let mut functions = BTreeMap::new();

    // Ce16: предварительная проверка циклических псевдонимов до вызова construct_type.
    // Собираем все AST-определения типов из текущего уровня модели.
    {
        let mut raw_defs: BTreeMap<String, ast::Type> = BTreeMap::new();
        let mut type_locs_pre: BTreeMap<String, Location> = BTreeMap::new();
        for element in model.elements.iter() {
            if let ModelElement::Type(def) = element {
                raw_defs.insert(def.name.name.clone(), def.ty.clone());
                type_locs_pre.insert(def.name.name.clone(), def.name.loc);
            }
        }
        if !raw_defs.is_empty() {
            let cycle_diags = check_type_alias_cycles_ast(&raw_defs, &type_locs_pre);
            if let Some(first) = cycle_diags.into_iter().next() {
                return Err(first);
            }
        }
    }

    // Типы модели готовятся одним шагом: цикл структур, занятие имён и разрешение
    // псевдонимов - см.
    crate::semantic::type_registry::prepare_types(&model_node, &model.elements)?;

    // Каталог импортирующего файла - неявный путь поиска.
    let import_paths = search_paths_with_importer_dir(search_paths, import_stack, files);
    let importer: Option<String> = importer_path(import_stack, files).map(str::to_string);
    let importer = importer.as_deref();

    for element in model.elements.iter() {
        if let ModelElement::Model(model) = element {
            let model = construct_model_stage0(
                model,
                Some(Rc::clone(&model_node)),
                search_paths,
                import_stack,
                files,
                specialize,
            )?;
            let model_name = model.borrow().name.clone().unwrap();
            if models.contains_key(&model_name) {
                return Err(Diagnostic::declaration_error(
                    model.borrow().loc,
                    format!("Модель с именем '{}' уже объявлена", model_name),
                )
                .with_code("SE-006"));
            }
            models.insert(model_name, model);
        } else if let ModelElement::Import(def) = element {
            match def {
                ImportDefine::Plain(path, import_loc) => {
                    let (content, filename) = read_import_file(&import_paths, path)?;
                    // Проверяем цикл до рекурсивной обработки файла
                    check_import_cycle(import_stack, &filename, *import_loc)?;
                    // Извлекаем только имя файла (без директории и расширения), затем
                    // нормализуем в CamelCase: "my_model.takt" -> "MyModel".
                    let stem = std::path::Path::new(&filename)
                        .file_stem()
                        .ok_or_else(|| {
                            Diagnostic::error(
                                *import_loc,
                                format!("Неверный путь к файлу импорта: «{}»", filename),
                            )
                            .with_code("SE-013")
                        })?
                        .to_string_lossy();
                    let model_name = normalize_camelcase_name(&stem);
                    if models.contains_key(&model_name) {
                        return Err(Diagnostic::declaration_error(
                            *import_loc,
                            format!("Модель с именем '{}' уже объявлена", model_name),
                        )
                        .with_code("SE-006"));
                    }
                    match parse(&content, files.add(&filename)) {
                        Ok((model, _)) => {
                            // Добавляем файл в стек, обрабатываем, убираем
                            import_stack.push(filename.clone());
                            let result = import_build::build_imported_file(
                                &model,
                                &model_node,
                                search_paths,
                                import_stack,
                                files,
                                specialize,
                                *import_loc,
                            );
                            import_stack.pop();
                            let node = result.map_err(|d| {
                                note_imported_here(d, *import_loc, &filename, importer)
                            })?;
                            // Усыновление: корень подключённого файла становится
                            // под-моделью импортёра и получает имя, под которым внесён
                            // в список. Без имени цель `c` отказывает `CC-004` на
                            // пустом имени модели.
                            import_adopt::adopt_whole_file(
                                &node,
                                &model_node,
                                &model_name,
                                &mut variables,
                            )
                            .map_err(|d| note_imported_here(d, *import_loc, &filename, importer))?;
                            models.insert(model_name, mark_imported(node));
                        }
                        Err(d) => {
                            return Err(note_imported_here(
                                d.first().unwrap().clone(),
                                *import_loc,
                                &filename,
                                importer,
                            ));
                        }
                    }
                }
                ImportDefine::GlobalSymbol(path, id, import_loc) => {
                    let (content, filename) = read_import_file(&import_paths, path)?;
                    // Проверяем цикл до рекурсивной обработки файла
                    check_import_cycle(import_stack, &filename, *import_loc)?;
                    let model_name = id.name.clone();
                    if models.contains_key(&model_name) {
                        return Err(Diagnostic::declaration_error(
                            id.loc,
                            format!("Модель с именем '{}' уже объявлена", model_name),
                        )
                        .with_code("SE-006"));
                    }
                    match parse(&content, files.add(&filename)) {
                        Ok((model, _)) => {
                            // Добавляем файл в стек, обрабатываем, убираем
                            import_stack.push(filename.clone());
                            let result = import_build::build_imported_file(
                                &model,
                                &model_node,
                                search_paths,
                                import_stack,
                                files,
                                specialize,
                                *import_loc,
                            );
                            import_stack.pop();
                            let node = result.map_err(|d| {
                                note_imported_here(d, *import_loc, &filename, importer)
                            })?;
                            // Усыновление: корень подключённого файла становится
                            // под-моделью импортёра и получает имя, под которым внесён
                            // в список. Без имени цель `c` отказывает `CC-004` на
                            // пустом имени модели.
                            import_adopt::adopt_whole_file(
                                &node,
                                &model_node,
                                &model_name,
                                &mut variables,
                            )
                            .map_err(|d| note_imported_here(d, *import_loc, &filename, importer))?;
                            models.insert(model_name, mark_imported(node));
                        }
                        Err(d) => {
                            return Err(note_imported_here(
                                d.first().unwrap().clone(),
                                *import_loc,
                                &filename,
                                importer,
                            ));
                        }
                    }
                }
                // `import { A, B as C } from "file.takt";`
                //
                // Загружает файл, строит его семантическую модель, затем выборочно
                // экспортирует указанные имена в текущий контекст.
                //
                // Поддерживаемые категории: модели, псевдонимы типов, переменные,
                // условия. Приоритет поиска: модель -> тип -> переменная -> условие.
                ImportDefine::Rename(path, symbols, import_loc) => {
                    let (content, filename) = read_import_file(&import_paths, path)?;
                    // Проверяем цикл до рекурсивной обработки файла
                    check_import_cycle(import_stack, &filename, *import_loc)?;
                    import_stack.push(filename.clone());
                    let result = match parse(&content, files.add(&filename)) {
                        Ok((ast_model, _)) => import_build::build_imported_file(
                            &ast_model,
                            &model_node,
                            search_paths,
                            import_stack,
                            files,
                            specialize,
                            *import_loc,
                        ),
                        Err(d) => {
                            import_stack.pop();
                            return Err(note_imported_here(
                                d.first().unwrap().clone(),
                                *import_loc,
                                &filename,
                                importer,
                            ));
                        }
                    };
                    import_stack.pop();
                    let imported = result
                        .map_err(|d| note_imported_here(d, *import_loc, &filename, importer))?;
                    import_select::apply(
                        import_select::Target {
                            models: &mut models,
                            variables: &mut variables,
                            conditions: &mut conditions,
                            model_node: &model_node,
                        },
                        &imported,
                        symbols,
                        mark_imported,
                    )
                    .map_err(|d| note_imported_here(d, *import_loc, &filename, importer))?;
                }
            }
        } else if let ModelElement::Variable(def) = element {
            // Разбор объявления значения - модуль `declaration.rs`.
            declaration::construct_declaration(
                def,
                Rc::clone(&model_node),
                &mut variables,
                &mut parameters,
            )?;
        } else if let ModelElement::Type(_) = element {
            // Псевдоним занят и разрешён предпроходом: здесь делать нечего. Занятие
            // имени по-прежнему живёт в одной воронке - она просто переехала выше по
            // конвейеру.
        } else if let ModelElement::Condition(def) = element {
            let def_loc = def
                .as_ref()
                .name
                .as_ref()
                .map(|id| id.loc)
                .unwrap_or(Location::Implicit);
            let name = def
                .clone()
                .name
                .ok_or_else(|| {
                    Diagnostic::error(
                        def_loc,
                        "Условие при определении должно иметь имя".to_string(),
                    )
                    .with_code("SE-019")
                })?
                .name
                .clone();
            conditions.insert(
                name.clone(),
                ConditionDefinitionNode {
                    name: name.clone(),
                    loc: def_loc,
                    value: ConditionNode::Unresolved(def.value.clone()),
                    upper: Some(Rc::downgrade(&model_node)),
                },
            );
        } else if let ModelElement::Invariant(inv) = element {
            // 0044: `invariant P = C;` в модели == `cond P = C;` + `: [Guard] P;`
            // (десахаризация, Option C). Имя P регистрируется как условие (-> атом LTL
            // `G(P)` и условие ребра `ref: P`), а обязательство - Guard-формула с
            // именем инварианта (проверяется каждый такт до `switch`, эталон C:
            // c_model.rs:549). АСД не переписывается - форматтер печатает `invariant`.
            let inv_loc = inv
                .name
                .as_ref()
                .map(|id| id.loc)
                .unwrap_or(Location::Implicit);
            let name = inv
                .clone()
                .name
                .ok_or_else(|| {
                    Diagnostic::error(inv_loc, "Инвариант должен иметь имя".to_string())
                        .with_code("SE-019")
                })?
                .name;
            // SE-054: коллизия имени с уже объявленным условием или переменной. Тихая
            // перезапись (HashMap::insert без проверки) недопустима в языке
            // спецификации промышленных систем.
            if conditions.contains_key(&name) || variables.contains_key(&name) {
                return Err(Diagnostic::error(
                    inv_loc,
                    format!(
                        "Имя инварианта '{}' конфликтует с существующим условием или переменной",
                        name
                    ),
                )
                .with_code("SE-054"));
            }
            conditions.insert(
                name.clone(),
                ConditionDefinitionNode {
                    name: name.clone(),
                    loc: inv_loc,
                    value: ConditionNode::Unresolved(inv.value.clone()),
                    upper: Some(Rc::downgrade(&model_node)),
                },
            );
            let guard = ConditionNode::Unresolved(inv.value.clone());
            let formula = Formula::Guard(guard, Some(name), inv_loc);
            model_node.borrow_mut().formulas.push(formula);
        } else if let ModelElement::NamedBlockCode(def) = element {
            let name = def
                .clone()
                .name
                .ok_or_else(|| {
                    Diagnostic::error(
                        def.loc,
                        "Именованный блок кода при определении должен иметь имя".to_string(),
                    )
                    .with_code("SE-018")
                })?
                .name
                .clone();
            // I8: сохраняем сырой АСД-оператор для последующей индексации локальных
            // переменных
            model_node
                .borrow_mut()
                .named_block_raw
                .push((name.clone(), def.statement.clone()));
            let block = match name.as_str() {
                "enter" => NamedCodeBlockDefinitionNode::Enter {
                    upper: Some(Rc::downgrade(&model_node)),
                    body: StatementNode::Unresolved(def.statement.clone()),
                },
                "exit" => NamedCodeBlockDefinitionNode::Exit {
                    upper: Some(Rc::downgrade(&model_node)),
                    body: StatementNode::Unresolved(def.statement.clone()),
                },
                "always" => NamedCodeBlockDefinitionNode::Always {
                    upper: Some(Rc::downgrade(&model_node)),
                    body: StatementNode::Unresolved(def.statement.clone()),
                },
                name => NamedCodeBlockDefinitionNode::Unknown {
                    upper: Some(Rc::downgrade(&model_node)),
                    name: name.to_string(),
                    body: StatementNode::Unresolved(def.statement.clone()),
                },
            };
            named_blocks.push(block);
        } else if let ModelElement::Assembly(block) = element {
            // Вставка уровня модели разворачивается в блок `always` уровня модели, то
            // есть исполняется каждый такт до диспетчеризации состояния. Сырой оператор
            // сохраняется рядом - он нужен индексации локальных переменных (I8), как у
            // обычного именованного блока.
            model_node
                .borrow_mut()
                .named_block_raw
                .push(("always".to_string(), (**block).clone()));
            named_blocks.push(NamedCodeBlockDefinitionNode::Always {
                upper: Some(Rc::downgrade(&model_node)),
                body: StatementNode::Unresolved((**block).clone()),
            });
        } else if let ModelElement::Function(def) = element {
            let name = def
                .clone()
                .name
                .ok_or_else(|| {
                    Diagnostic::error(
                        def.loc,
                        "При определении функция должна иметь имя".to_string(),
                    )
                    .with_code("SE-022")
                })?
                .name
                .clone();
            // 0031: дубликат имени функции - ошибка SE-009 (прежде `HashMap:: insert`
            // молча перетирал, побеждала последняя). Проверка живёт здесь, а не в
            // `construct_function`: после устранения `mem::take` карта на время
            // разрешения тел непуста, и проверка в точке разрешения ловила бы каждую
            // функцию как "уже определённую".
            if functions.contains_key(&name) {
                return Err(Diagnostic::error(
                    def.loc,
                    format!("Функция с именем '{}' уже определена", name),
                )
                .with_code("SE-009"));
            }
            functions.insert(
                name.clone(),
                FunctionDefinitionNode::Unresolved(*def.clone()),
            );
        } else if let ModelElement::Enum(e) = element {
            // Имя уже занято предпроходом `predeclare_named_types`: повторный
            // `claim_type_name` дал бы ложную `SE-108`. FE1: узел перечисления строит
            // `enum_build` ( - вынос из этого файла: он сверх лимита размера, и правило
            // велит выносить новое, а не дописывать сюда).
            crate::semantic::enum_node::build_enum(&model_node, e);
        } else if let ModelElement::Struct(s) = element {
            // NI3: Обработка структурных типов.
            let struct_name = s
                .name
                .as_ref()
                .map(|id| id.name.clone())
                .unwrap_or_default();
            let struct_loc = s.name.as_ref().map(|id| id.loc).unwrap_or(s.loc);
            // имя занимается до разбора полей. Имя занято предпроходом
            // `predeclare_named_types`.

            // Типы полей разрешаются при полной карте имён, поэтому поле вправе
            // сослаться на тип, объявленный ниже.
            //
            // Ошибка `construct_type` возвращается вызывающему, а не подменяется
            // `TypeNode::Unsupported`: подмена глотала диагностику, и эталон исполнял
            // модель с полем-структурой, ставшей числом, - молча.
            let mut field_pairs: Vec<(String, TypeNode)> = Vec::new();
            for field in &s.fields {
                let field_ty = construct_type(Some(field.ty.clone()), Rc::clone(&model_node))?;
                field_pairs.push((field.name.name.clone(), field_ty));
            }

            let mut struct_node = crate::semantic::StructDefinitionNode {
                name: struct_name.clone(),
                fields: field_pairs,
                loc: struct_loc,
            };
            struct_node.loc = struct_loc;

            model_node
                .borrow_mut()
                .structs
                .insert(struct_name.clone(), struct_node);
            // Регистрация в `types`/`type_locs` сделана предпроходом: второе место
            // записи разъехалось бы с первым.
        } else if let ModelElement::Address(def) = element {
            // оператор `address Имя = <выражение>;`. Захватываем сырую привязку;
            // проверка (существование порта, конфликт источников) -
            // `check_port_addresses` в validate.rs (порты могут объявляться позже).
            let port = extract_name(def.name.clone(), def.loc)?;
            model_node
                .borrow_mut()
                .address_defs
                .push(crate::semantic::AddressBindingNode {
                    port,
                    loc: def.loc,
                    value: ExpressionNode::Unresolved(def.value.clone()),
                });
        } else if let ModelElement::InlineFormula(inline) = element {
            match &**inline {
                ast::InlineFormulaDefine::Guard { conditions, .. } => {
                    let at = formula::inline_formula_loc(inline);
                    for cond in conditions {
                        let guard = ConditionNode::Unresolved(cond.clone());
                        model_node
                            .borrow_mut()
                            .formulas
                            .push(Formula::Guard(guard, None, at));
                    }
                }
                ast::InlineFormulaDefine::Ltl { formulas, loc } => {
                    formula::push_ltl(&mut model_node.borrow_mut().formulas, formulas, *loc);
                }
            }
        }
    }
    model_node.borrow_mut().models = models;
    model_node.borrow_mut().states = construct_states(model, Rc::clone(&model_node))?;
    model_node.borrow_mut().variables = variables;
    // Анализ изменяемости параметров - здесь, пока АСД модели под рукой: он ищет
    // присваивания в **сыром** тексте тел, а стадии 2-6 их ещё не разрешили. Результат -
    // флаг `ParameterNode::mutated`; применяет его (только в режиме
    // `--parameters=specialize`) `parameter_const::constify_parameters`.
    crate::semantic::parameter_const::mark_mutated(model, &mut parameters);
    model_node.borrow_mut().parameters = parameters;
    model_node.borrow_mut().conditions = conditions;
    model_node.borrow_mut().named_blocks = named_blocks;
    model_node.borrow_mut().functions.extend(functions); // extend: см. import/select.rs
    Ok(Rc::clone(&model_node))
}

pub(super) fn construct_model_stage1(
    model: Rc<RefCell<ModelNode>>,
) -> Result<Rc<RefCell<ModelNode>>, Diagnostic> {
    extend::expand_model_implement(&model)?; // форма `model M = A|B {...}`, 0199
    // Клонируем состояния до займа: construct_implement берёт заём сам
    let states = model.borrow().states.clone();

    let mut prepared_states = BTreeMap::new();
    for (name, state) in states.iter() {
        if let StateNode::Implement {
            upper,
            loc,
            implements: Extend::Unresolved(implement_expression),
            named_blocks,
            references,
            next,
            name,
            kind,
            formulas,
        } = state.clone()
        {
            let implements = extend::unroll_extend_expression(
                ExpressionNode::Unresolved(implement_expression),
                Rc::clone(&model),
            )?;
            prepared_states.insert(
                name.clone(),
                StateNode::Implement {
                    upper: upper.clone(),
                    loc,
                    named_blocks,
                    name: name.clone(),
                    references,
                    implements,
                    next,
                    kind,
                    formulas,
                },
            );
        } else {
            prepared_states.insert(name.clone(), state.clone());
        }
    }
    model.borrow_mut().states = prepared_states;

    // Клонируем список вложенных моделей до рекурсивного вызова
    let nested: Vec<(String, Rc<RefCell<ModelNode>>)> = model
        .borrow()
        .models
        .iter()
        .map(|(k, v)| (k.clone(), Rc::clone(v)))
        .collect();

    let mut models = BTreeMap::new();
    for (name, nested_model) in nested {
        models.insert(name, construct_model_stage1(Rc::clone(&nested_model))?);
    }
    model.borrow_mut().models = models;

    Ok(Rc::clone(&model))
}

pub(super) fn construct_model_stage2(
    model: Rc<RefCell<ModelNode>>,
) -> Result<Rc<RefCell<ModelNode>>, Diagnostic> {
    // Шаг 2: разрешение инициализаторов, вывод типов и свёртка в литералы. Порядок этих
    // трёх шагов неочевиден и потому собран в одном месте -
    // `declaration::prepare_variables`.
    let variables = model.borrow().variables.clone();
    let variables = declaration::prepare_variables(&variables, &model)?;
    model.borrow_mut().variables = variables;
    // Рекурсивно обрабатываем вложенные модели с их собственным контекстом
    let nested: Vec<(String, Rc<RefCell<ModelNode>>)> = model
        .borrow()
        .models
        .iter()
        .map(|(k, v)| (k.clone(), Rc::clone(v)))
        .collect();
    let mut models = BTreeMap::new();
    for (name, nested_model) in nested {
        models.insert(name, construct_model_stage2(nested_model)?);
    }
    model.borrow_mut().models = models;
    Ok(Rc::clone(&model))
}

pub(super) fn construct_model_stage3(
    model: Rc<RefCell<ModelNode>>,
) -> Result<Rc<RefCell<ModelNode>>, Diagnostic> {
    let mut conditions = model.borrow().conditions.clone();
    conditions = extract_conditions(&conditions, model.clone())?;
    conditions = extract_conditions(&conditions, model.clone())?;
    model.borrow_mut().conditions = conditions;
    // Рекурсивно обрабатываем вложенные модели с их собственным контекстом
    let nested: Vec<(String, Rc<RefCell<ModelNode>>)> = model
        .borrow()
        .models
        .iter()
        .map(|(k, v)| (k.clone(), Rc::clone(v)))
        .collect();
    let mut models = BTreeMap::new();
    for (name, nested_model) in nested {
        models.insert(name, construct_model_stage3(nested_model)?);
    }
    model.borrow_mut().models = models;
    Ok(Rc::clone(&model))
}

fn resolve_formula(formula: Formula, model: Rc<RefCell<ModelNode>>) -> Result<Formula, Diagnostic> {
    match formula {
        Formula::None => Ok(Formula::None),
        Formula::Formulas(formulas) => {
            let mut resolved = Vec::with_capacity(formulas.len());
            for f in formulas {
                resolved.push(resolve_formula(f, model.clone())?);
            }
            Ok(Formula::Formulas(resolved))
        }
        Formula::Guard(cond, name, loc) => match cond {
            ConditionNode::Unresolved(ast_cond) => Ok(Formula::Guard(
                resolve_condition(&ast_cond, model)?,
                name,
                loc,
            )),
            other => Ok(Formula::Guard(other, name, loc)),
        },
        Formula::LTL(ltl, loc) => Ok(Formula::LTL(ltl, loc)),
    }
}

pub(super) fn resolve_formulas(
    formulas: Vec<Formula>,
    model: Rc<RefCell<ModelNode>>,
) -> Result<Vec<Formula>, Diagnostic> {
    let mut resolved = Vec::with_capacity(formulas.len());
    for f in formulas {
        resolved.push(resolve_formula(f, model.clone())?);
    }
    Ok(resolved)
}

/// Разрешает именованные блоки кода внутри одного состояния.
///
/// Ошибки разрешения подавляются - оператор сохраняется как `Unresolved`.
pub(super) fn resolve_state_named_blocks(
    state: StateNode,
    model: Rc<RefCell<ModelNode>>,
) -> Result<StateNode, Diagnostic> {
    match state {
        StateNode::Simple {
            upper,
            loc,
            name,
            references,
            named_blocks,
            kind,
            formulas,
        } => Ok(StateNode::Simple {
            upper: upper.clone(),
            loc,
            name,
            references,
            kind,
            formulas: resolve_formulas(formulas, model.clone())?,
            named_blocks: resolve_named_blocks(named_blocks, model)?,
        }),
        StateNode::Implement {
            upper,
            loc,
            name,
            references,
            implements,
            next,
            named_blocks,
            kind,
            formulas,
        } => Ok(StateNode::Implement {
            upper: upper.clone(),
            loc,
            name,
            references,
            implements,
            next,
            kind,
            formulas: resolve_formulas(formulas, model.clone())?,
            named_blocks: resolve_named_blocks(named_blocks, model)?,
        }),
        other => Ok(other),
    }
}

/// Строит семантический узел модели из АСД-узла [`Model`].
///
/// Собирает контекст верхнего уровня (вложенные модели), а также словарь состояний с
/// разрешёнными ссылками между ними.
///
/// Обнаруживает циклические зависимости между файлами импорта: при наличии цикла
/// `a.takt -> b.takt -> a.takt` возвращает [`Diagnostic`]-ошибку с полным описанием
/// цепочки.
///
/// # Ошибки
///
/// Возвращает [`Diagnostic`], если:
/// - у состояния нет имени,
/// - ссылка `ref` указывает на несуществующее состояние,
/// - `next` встречается в одном состоянии дважды,
/// - обнаружен циклический импорт.
pub fn construct_model(
    model: &Model,
    upper: Option<Rc<RefCell<ModelNode>>>,
    search_paths: &[String],
) -> Result<Rc<RefCell<ModelNode>>, Diagnostic> {
    // Реестр-однодневка: пути файлов вызывающему не нужны.
    let mut files = FileTable::default();
    construct_model_with_files(model, upper, search_paths, &mut files, false)
}

/// То же, что [`construct_model`], но с реестром файлов.
///
/// Реестр раздаёт `file_no` разбираемым файлам и позволяет вызывающему разрешить номер
/// из [`Location`] обратно в путь - чтобы назвать пользователю, **в каком файле**
/// ошибка. Корневой файл регистрирует вызывающий ([`FileTable::new`]); импортируемые
/// регистрирует проход 0 по мере загрузки.
pub fn construct_model_with_files(
    model: &Model,
    upper: Option<Rc<RefCell<ModelNode>>>,
    search_paths: &[String],
    files: &mut FileTable,
    specialize: bool,
) -> Result<Rc<RefCell<ModelNode>>, Diagnostic> {
    // Контракт этого входа - **одна** диагностика; он таким и остаётся. Стадии
    // построения с 0152 накапливают внутри себя, но потребителям вроде `takt-sim` нужна
    // первая ошибка, а не список, и менять публичную сигнатуру ради этого незачем. Кому
    // нужны все - зовёт `stages::construct_stages` напрямую (так делают `pipeline` и
    // LSP).
    //
    // `normalize` перед взятием первой - не украшение: без неё "первой" оказалась бы не
    // самая ранняя по тексту, а первая по порядку обхода.
    let model = super::stages::construct_stages(model, upper, search_paths, files, specialize)
        .map_err(|ds| {
            crate::diagnostics::normalize(ds)
                .into_iter()
                .next()
                .unwrap_or_else(|| super::internal::no_diagnostic("построение дерева"))
        })?;
    validate_model(model.clone())?;
    Ok(model)
}

/// Строит семантическое дерево модели и привязывает `///`-комментарии.
///
/// Расширенный вариант [`construct_model`]: после построения дерева заполняет поля
/// [`ModelNode::doc`](crate::semantic::ModelNode::doc) и
/// [`ModelNode::docs`](crate::semantic::ModelNode::docs) на основе `///`-комментариев
/// из исходного текста.
///
/// # Параметры
///
/// - `model` - корневой узел АСД, результат [`parse`](parse).
/// - `upper` - родительская модель (`None` для корня).
/// - `search_paths` - пути поиска для файлов импорта.
/// - `comments` - комментарии из [`parse`](parse) (второй элемент кортежа).
///
/// # Алгоритм привязки
///
/// Для каждого именованного объявления (состояния, переменной, функции и т.д.) ищутся
/// `///`-комментарии, ближайшим следующим элементом которых является данное объявление.
/// Подробнее - в [`crate::semantic::docs`].
///
/// # Примеры
///
/// ```
/// use takt_lang::{parse, semantic::tree::construct_model_with_docs};
///
/// let src = "/// Документация состояния.\nstart S;";
/// let (ast, comments) = parse(src, 0).unwrap();
/// let root = construct_model_with_docs(&ast, None, &[], &comments).unwrap();
/// assert_eq!(root.borrow().element_doc("S"), ["Документация состояния."]);
/// ```
pub fn construct_model_with_docs(
    model: &Model,
    upper: Option<Rc<RefCell<ModelNode>>>,
    search_paths: &[String],
    comments: &[ast::Comment],
) -> Result<Rc<RefCell<ModelNode>>, Diagnostic> {
    // Строим семантическое дерево (без документации)
    let root = construct_model(model, upper, search_paths)?;
    // Привязываем doc-комментарии к узлам дерева
    crate::semantic::docs::attach_docs(&root, model, comments);
    Ok(root)
}

/// Проверяет условия переходов в семантическом дереве и возвращает предупреждения о
/// неявном приведении числового типа к булевому.
///
/// Функция обходит все состояния модели (рекурсивно) и проверяет, содержат ли условия
/// переходов (`ref`/`next`) выражения числового типа (например, переменная типа
/// `[bit;8]`, числовой литерал, арифметика), используемые как булевые без явного
/// сравнения.
///
/// # Примеры
///
/// ```rust,ignore
/// // Takt-код с числовым условием -> предупреждение
/// let src = "var timer: [bit;8] = 0; start S { ref T: timer; } state T;";
/// let (ast, _) = parse(src, 0)?;
/// let root = construct_model(&ast, None, &[])?;
/// let warnings = implicit_bool_warnings(&root);
/// assert!(!warnings.is_empty());
///
/// // Takt-код с явным сравнением -> без предупреждений
/// let src = "var timer: [bit;8] = 0; start S { ref T: timer != 0; } state T;";
/// let (ast, _) = parse(src, 0)?;
/// let root = construct_model(&ast, None, &[])?;
/// let warnings = implicit_bool_warnings(&root);
/// assert!(warnings.is_empty());
/// ```
pub fn implicit_bool_warnings(model: &Rc<RefCell<ModelNode>>) -> Vec<Diagnostic> {
    check_implicit_bool_conditions(model)
}

/// Проверяет полноту и достижимость переходов в семантическом дереве.
///
/// Возвращает предупреждения и ошибки Ce5:
/// - отсутствие терминальных состояний в модели;
/// - состояния без пути к терминальному;
/// - совместное использование `ref` и `next` в одном состоянии.
///
/// Функция обходит всё дерево моделей рекурсивно.
///
/// # Примеры
///
/// ```rust,ignore
/// use takt_lang::{parse, semantic::tree::{construct_model, transition_completeness_warnings}};
///
/// let src = "start A { ref B: true; } state B { ref A: true; }";
/// let (ast, _) = parse(src, 0).unwrap();
/// let root = construct_model(&ast, None, &[]).unwrap();
/// let warnings = transition_completeness_warnings(&root);
/// // Предупреждение: нет терминальных состояний
/// assert!(!warnings.is_empty());
/// ```
pub fn transition_completeness_warnings(model: &Rc<RefCell<ModelNode>>) -> Vec<Diagnostic> {
    check_transition_completeness(Rc::clone(model))
}

/// Возвращает предупреждения о портах, объявленных во вложенных моделях.
///
/// Порты в дочерних моделях попадают в общесистемные перечисления портов и доступны
/// через колбэки корневой модели из любого места.
pub fn nested_port_warnings(model: &Rc<RefCell<ModelNode>>) -> Vec<Diagnostic> {
    warn_nested_model_ports(Rc::clone(model))
}

/// Извлекает все состояния из модели и разрешает ссылки между ними.
///
/// Алгоритм:
/// 1. Первый проход - создаём [`StateNode`] для каждого `state`/`start` с
///    [`StateNode::Unresolved`] в качестве заглушки для целей ссылок.
/// 2. Второй проход - заменяем заглушки фактическими [`StateNode`].
///
/// # Ошибки
///
/// Возвращает [`Diagnostic`], если состояние без имени, ссылка не найдена, или `next`
/// объявлен дважды в одном состоянии.
pub fn construct_states(
    model: &Model,
    upper: Rc<RefCell<ModelNode>>,
) -> Result<BTreeMap<String, StateNode>, Diagnostic> {
    // Первый проход: создаём узлы с незаполненными ссылками (заглушки Unresolved).
    let mut states: BTreeMap<String, Box<StateNode>> = BTreeMap::new();
    for element in model.elements.iter() {
        if let ModelElement::State(def) = element {
            let name = def
                .clone()
                .name
                .ok_or_else(|| {
                    Diagnostic::error(def.loc, "Имя состояния не задано".to_string())
                        .with_code("SE-020")
                })?
                .name;
            let implements = def.implements.clone();
            let kind = def.kind.clone();
            let mut references = Vec::new();
            let mut next: Option<String> = None;
            let mut state_formulas: Vec<Formula> = Vec::new();
            for element in def.elements.iter() {
                if let StateElement::Reference(_, id, cond) = element {
                    let cond = if let Some(cond) = cond {
                        ConditionNode::Unresolved(cond.clone())
                    } else {
                        ConditionNode::None
                    };
                    references.push(ReferenceNode {
                        location: id.loc,
                        name: id.name.clone(),
                        cond,
                        object: Box::new(StateNode::Unresolved),
                    });
                } else if let StateElement::Next(id) = element {
                    if next.is_some() {
                        return Err(Diagnostic::error(
                            id.loc,
                            format!("Состояние '{}' уже содержит оператор next", id.name),
                        )
                        .with_code("SE-012"));
                    }
                    next = Some(id.name.clone());
                } else if let StateElement::InlineFormula(inline) = element {
                    match &**inline {
                        ast::InlineFormulaDefine::Guard { conditions, .. } => {
                            let at = formula::inline_formula_loc(inline);
                            for cond in conditions {
                                let guard = ConditionNode::Unresolved(cond.clone());
                                state_formulas.push(Formula::Guard(guard, None, at));
                            }
                        }
                        ast::InlineFormulaDefine::Ltl { formulas, loc } => {
                            formula::push_ltl(&mut state_formulas, formulas, *loc);
                        }
                    }
                } else if let StateElement::Invariant(inv) = element {
                    // 0044: инвариант состояния = Guard-формула с именем инварианта
                    // (десахаризация). Условие C проверяется каждый такт, пока автомат
                    // в этом состоянии (эталон C: c_model.rs:667).
                    let inv_name = inv.name.as_ref().map(|id| id.name.clone());
                    let guard = ConditionNode::Unresolved(inv.value.clone());
                    state_formulas.push(Formula::Guard(guard, inv_name, inv.loc));
                }
            }
            // Если состояние не имеет реализации (= Expr), но имеет `next`,
            // конвертируем `next` в безусловный переход (ref с ConditionNode::None).
            if implements.is_none()
                && let Some(ref next_name) = next
            {
                references.push(ReferenceNode {
                    location: def.loc,
                    name: next_name.clone(),
                    cond: ConditionNode::None,
                    object: Box::new(StateNode::Unresolved),
                });
                next = None;
            }
            let kind = match kind {
                None => {
                    if crate::semantic::terminal::is_end(&references, &def.elements) {
                        StateNodeKind::End
                    } else {
                        StateNodeKind::Simple
                    }
                }
                Some(kind) => match kind {
                    StateKind::Start => StateNodeKind::Start,
                    StateKind::End => StateNodeKind::End,
                    StateKind::Next => {
                        return Err(Diagnostic::error(
                            def.loc,
                            "Состояние с типом next не поддерживается в качестве определения"
                                .to_string(),
                        )
                        .with_code("SE-021"));
                    }
                },
            };
            // Определяем вид узла: Implement (есть `= Выражение`) или Simple.
            let state_loc = def.loc;
            let state = if let Some(expr) = implements {
                let next = next.map(|n| ReferenceNode {
                    location: state_loc,
                    name: n,
                    cond: ConditionNode::None,
                    object: Box::new(StateNode::Unresolved),
                });
                StateNode::Implement {
                    upper: Some(Rc::downgrade(&upper)),
                    loc: state_loc,
                    named_blocks: crate::semantic::named_blocks::construct_named_blocks(
                        def,
                        Some(Rc::downgrade(&upper)),
                    )?,
                    name: name.clone(),
                    references,
                    implements: Extend::Unresolved(expr),
                    next,
                    kind,
                    formulas: state_formulas,
                }
            } else {
                StateNode::Simple {
                    upper: Some(Rc::downgrade(&upper)),
                    loc: state_loc,
                    named_blocks: crate::semantic::named_blocks::construct_named_blocks(
                        def,
                        Some(Rc::downgrade(&upper)),
                    )?,
                    name: name.clone(),
                    references,
                    kind,
                    formulas: state_formulas,
                }
            };
            states.insert(name, Box::new(state));
        }
    }

    // Второй проход: заменяем Unresolved-заглушки реальными узлами.
    let mut new_states: BTreeMap<String, StateNode> = BTreeMap::new();
    for state in states.values() {
        match *state.clone() {
            StateNode::Simple {
                upper,
                loc,
                name,
                references,
                named_blocks,
                kind,
                formulas,
            } => {
                let resolved = resolve_references(references, &states)?;
                new_states.insert(
                    name.clone(),
                    StateNode::Simple {
                        upper: upper.clone(),
                        loc,
                        named_blocks,
                        name,
                        references: resolved,
                        kind,
                        formulas,
                    },
                );
            }
            StateNode::Implement {
                upper,
                loc,
                named_blocks,
                name,
                references,
                implements,
                next,
                kind,
                formulas,
            } => {
                let resolved = resolve_references(references, &states)?;
                // Разрешаем next-ссылку отдельно (это одиночный Reference, не список).
                let next = next
                    .map(|r| -> Result<ReferenceNode<StateNode>, Diagnostic> {
                        if let StateNode::Unresolved = *r.object {
                            let target = states.get(&r.name).ok_or_else(|| {
                                Diagnostic::error(
                                    r.location,
                                    format!("Ссылка '{}' не найдена", r.name),
                                )
                                .with_code("SE-002")
                            })?;
                            Ok(ReferenceNode {
                                location: r.location,
                                name: r.name,
                                cond: r.cond,
                                object: target.clone(),
                            })
                        } else {
                            Ok(r)
                        }
                    })
                    .transpose()?;
                new_states.insert(
                    name.clone(),
                    StateNode::Implement {
                        upper: upper.clone(),
                        loc,
                        named_blocks,
                        name,
                        references: resolved,
                        implements,
                        next,
                        kind,
                        formulas,
                    },
                );
            }
            _ => {} // StateNode::Unresolved пропускаем
        }
    }
    Ok(new_states)
}

/// Разрешает список `ref`-ссылок, заменяя [`StateNode::Unresolved`]-заглушки реальными
/// узлами из таблицы первого прохода `states`.
///
/// # Ошибки
///
/// Возвращает [`Diagnostic`], если ссылка указывает на несуществующее состояние.
fn resolve_references(
    references: Vec<ReferenceNode<StateNode>>,
    states: &BTreeMap<String, Box<StateNode>>,
) -> Result<Vec<ReferenceNode<StateNode>>, Diagnostic> {
    references
        .into_iter()
        .map(|r| {
            if let StateNode::Unresolved = *r.object {
                let target = states.get(&r.name).ok_or_else(|| {
                    Diagnostic::error(r.location, format!("Ссылка '{}' не найдена", r.name))
                        .with_code("SE-002")
                })?;
                Ok(ReferenceNode {
                    location: r.location,
                    name: r.name,
                    cond: r.cond,
                    object: target.clone(),
                })
            } else {
                Ok(r)
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse;

    // --- вспомогательная функция --------------------------------------------

    /// Разбирает Takt-программу и строит семантическую модель.
    fn build(src: &str) -> Result<ModelNode, Diagnostic> {
        let (ast, _) = parse(src, 0).expect("parse error");
        construct_model(&ast, None, &[]).map(|model| model.take())
    }

    // --- construct_model ---------------------------------------------------

    /// Пустая программа (нет состояний): `has_states()` должен вернуть `false`.
    #[test]
    fn empty_program_has_no_states() {
        let node = build("").unwrap();
        assert!(!node.has_states());
    }

    /// Программа без состояний, но с типом: `has_states()` -> false.
    #[test]
    fn program_with_only_type_has_no_states() {
        let node = build("type Byte = [bit;8];").unwrap();
        assert!(!node.has_states());
    }

    /// Одна именованная модель с двумя состояниями.
    #[test]
    fn model_with_states_returns_true() {
        let node = build("model M { start S; state E; }").unwrap();
        // Корневая модель содержит модель M, а не состояния напрямую
        assert!(!node.has_states()); // корень анонимен и без состояний
    }

    /// Глобальные состояния верхнего уровня.
    #[test]
    fn top_level_states_are_found() {
        let node = build("start A; state B;").unwrap();
        assert!(node.has_states());
    }

    /// Имя корневой модели всегда `None`.
    #[test]
    fn root_model_name_is_none() {
        let node = build("start S;").unwrap();
        assert_eq!(node.name, None);
    }

    /// Именованная вложенная модель получает корректное имя.
    #[test]
    fn nested_model_name_is_set() {
        let (ast, _) = parse("model Foo { start S; }", 0).unwrap();
        // Ищем вложенную модель в elements
        if let ModelElement::Model(m) = &ast.elements[0] {
            let node = construct_model(m, None, &[]).unwrap();
            assert_eq!(node.take().name, Some("Foo".to_string()));
        } else {
            panic!("ожидался ModelElement::Model");
        }
    }

    // --- construct_states -------------------------------------------------

    /// Состояние без `ref` - SimpleNode, ссылки пустые.
    #[test]
    fn simple_state_no_refs() {
        let node = build("start S;").unwrap();
        assert!(node.states.contains_key("S"));
        if let StateNode::Simple { references, .. } = &node.states["S"] {
            assert!(references.is_empty());
        } else {
            panic!("ожидался StateNode::Simple");
        }
    }

    /// Состояние с корректной `ref`-ссылкой на другое состояние.
    #[test]
    fn ref_to_existing_state_resolves() {
        let node = build("start A { ref B; } state B;").unwrap();
        assert!(node.states.contains_key("A"));
        assert!(node.states.contains_key("B"));
        if let StateNode::Simple { references, .. } = &node.states["A"] {
            assert_eq!(references.len(), 1);
            assert_eq!(references[0].name, "B");
        } else {
            panic!("ожидался StateNode::Simple для A");
        }
    }

    /// Ссылка `ref` на несуществующее состояние - ошибка.
    #[test]
    fn ref_to_missing_state_is_error() {
        // Ghost не существует
        let result = build("start A { ref Ghost; }");
        assert!(result.is_err(), "ожидалась ошибка при неизвестной ссылке");
    }

    /// Два `next` в одном состоянии - ошибка.
    #[test]
    fn double_next_in_state_is_error() {
        // Два next в одном Implement-состоянии
        let result =
            build("start A = M { next B; next C; } state B; state C; model M { start S; }");
        assert!(result.is_err(), "ожидалась ошибка при двойном next");
    }

    /// Implement-состояние с `next` разрешается корректно.
    #[test]
    fn implement_state_with_next_resolves() {
        let node = build("start A = M { next B; } state B; model M { start S; }").unwrap();
        assert!(node.states.contains_key("A"));
        if let StateNode::Implement { next, .. } = &node.states["A"] {
            assert!(next.is_some(), "ожидался Some(next)");
        } else {
            panic!("ожидался StateNode::Implement для A");
        }
    }

    /// Implement-состояние без `next`.
    #[test]
    fn implement_state_without_next() {
        let node = build("start A = M { } state B; model M { start S; }").unwrap();
        if let StateNode::Implement { next, .. } = &node.states["A"] {
            assert!(next.is_none(), "next должен быть None");
        } else {
            panic!("ожидался StateNode::Implement для A");
        }
    }

    /// Несколько состояний с взаимными ссылками.
    #[test]
    fn multiple_states_with_cross_refs() {
        let node = build("start A { ref B; } state B { ref A; }").unwrap();
        assert_eq!(node.states.len(), 2);
    }

    /// `ref` с булевым условием разрешается без ошибок.
    #[test]
    fn ref_with_bool_condition_resolves() {
        let node = build("start A { ref B: true; } state B;").unwrap();
        if let StateNode::Simple { references, .. } = &node.states["A"] {
            assert_eq!(references.len(), 1);
        } else {
            panic!("ожидался StateNode::Simple");
        }
    }

    // --- construct_context_model ------------------------------------------

    /// Вложенная модель попадает в контекст.
    #[test]
    fn nested_model_in_context() {
        let (ast, _) = parse("model Outer { model Inner { start S; } start A; }", 0).unwrap();
        let node = construct_model(&ast, None, &[]).unwrap();
        // Inner - вложен в Outer, который в корневом контексте
        assert!(!node.take().has_states()); // корень не содержит состояний напрямую
    }
}
