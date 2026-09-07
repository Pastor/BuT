//! Построение семантических узлов функций языка Takt.
//!
//! Основная функция [`construct_function`] преобразует [`FunctionDefinitionNode::Unresolved`]
//! (необработанное AST-определение) в:
//! - [`FunctionDefinitionNode::Local`] - локальная функция с телом;
//! - [`FunctionDefinitionNode::External`] - внешняя функция (`extern fn`).
//!
//! Уже разрешённые функции ([`FunctionDefinitionNode::Local`],
//! [`FunctionDefinitionNode::External`], [`FunctionDefinitionNode::Builtin`])
//! возвращаются без изменений.

use crate::diagnostics::Diagnostic;
use crate::diagnostics::lang::keys;
use crate::msg;
use crate::parser::ast;
use crate::semantic::internal::internal;
use crate::semantic::statement::resolve_statement;
use crate::semantic::type_node::{TypeNode, construct_type};
use crate::semantic::{FunctionDefinitionNode, ModelNode, StatementNode};
use std::cell::RefCell;
use std::rc::Rc;

/// Режим подстановки функции, заданный атрибутом объявления.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum InlineMode {
    /// Атрибута нет: решает эвристика (`--inline=auto`), а по умолчанию - подстановки
    /// не происходит.
    #[default]
    Auto,
    /// `[inline]` - подставлять всегда.
    Always,
    /// `[noinline]` - не подставлять никогда.
    Never,
}

/// Разбирает атрибут объявления функции.
///
/// Имя атрибута - обычный идентификатор (грамматика о наборе не знает), поэтому
/// неизвестное имя судит семантика: `SE-126` с перечислением допустимых.
pub fn inline_mode(def: &ast::FunctionDefine) -> InlineMode {
    match def.attribute.as_ref().map(|a| a.name.as_str()) {
        Some("inline") => InlineMode::Always,
        Some("noinline") => InlineMode::Never,
        _ => InlineMode::Auto,
    }
}

/// Проверяет атрибут объявления функции.
fn check_attribute(def: &ast::FunctionDefine) -> Result<(), Diagnostic> {
    let Some(attribute) = def.attribute.as_ref() else {
        return Ok(());
    };
    if !matches!(attribute.name.as_str(), "inline" | "noinline") {
        return Err(Diagnostic::error(
            attribute.loc,
            format!(
                "неизвестный атрибут функции '{}': допустимы 'inline' (подставлять тело \
                 в место вызова) и 'noinline' (не подставлять)",
                attribute.name
            ),
        )
        .with_code("SE-126"));
    }
    if def.external && attribute.name == "inline" {
        return Err(Diagnostic::error(
            attribute.loc,
            "атрибут 'inline' на внешней функции: тела у неё нет, подставлять нечего — \
             её вызов остаётся вызовом в порождённом коде"
                .to_string(),
        )
        .with_code("SE-127"));
    }
    Ok(())
}

/// Строит разрешённый семантический узел функции из [`FunctionDefinitionNode`].
///
/// Обрабатывает только `Unresolved`-вариант; остальные возвращаются без изменений.
///
/// # Ошибки
///
/// Возвращает [`Diagnostic`], если:
/// - функция без имени;
/// - функция с таким именем уже объявлена в текущей модели;
/// - параметр не имеет объявления типа;
/// - локальная функция объявлена без тела.
pub fn construct_function(
    func: FunctionDefinitionNode,
    model: Rc<RefCell<ModelNode>>,
) -> Result<FunctionDefinitionNode, Diagnostic> {
    if let FunctionDefinitionNode::Unresolved(def) = func {
        // Исходное определение сохраняется целиком: его интерпретирует константный
        // вычислитель.
        let raw = Box::new(def.clone());
        let name = def
            .clone()
            .name
            .ok_or_else(|| {
                Diagnostic::error(
                    def.loc,
                    "при определении функция должна иметь имя".to_string(),
                )
                .with_code("SE-022")
            })?
            .name
            .clone();
        // 0031: проверка дубликата (SE-009) перенесена в точку вставки (`tree.rs`,
        // проход сбора функций). Здесь её быть не должно: после устранения `mem::take`
        // карта модели на время разрешения тел непуста, и `contains_key` срабатывал бы
        // на каждой функции (её имя уже в карте).
        {
            let mut params = Vec::new();
            for (_, param) in def.params.iter() {
                if let Some(param) = param {
                    // В грамматике тип параметра хранится как `ast::Expression`.
                    // Возможные варианты:
                    //   - `Expression::Type(_, typ)` - явный тип (редко используется).
                    //   - `Expression::Variable(id)` - идентификатор типа (`bit`, `u8`, ...).
                    //   - `Expression::Array(_, items)` - массивный тип (не поддерживается как параметр).
                    let param_type = match param.clone().ty {
                        ast::Expression::Type(_, typ) => construct_type(Some(typ), model.clone())?,
                        ast::Expression::Variable(id) => {
                            // Преобразуем идентификатор в псевдоним типа и разрешаем.
                            construct_type(Some(ast::Type::Alias(id)), model.clone())?
                        }
                        // Fixed-point `q(m, n)` в параметре. Тип параметра грамматика
                        // хранит выражением, а `q(8, 8)` как выражение есть **вызов**:
                        // `q` не ключевое слово (решение 0061), и правило `Type` сюда
                        // не подставить - оно даёт конфликт с вызовом функции. Поэтому
                        // форму распознаёт семантика, как и имя `q`
                        // (`construct_fixed`).
                        ast::Expression::Function(loc, ctor, args) => {
                            let fixed = fixed_from_call(loc, &ctor, &args).ok_or_else(|| {
                                Diagnostic::error(
                                    param.loc,
                                    msg!(
                                        keys::SE_034_PARAMETER_TYPE_UNRECOGNISED,
                                        name = param
                                            .name
                                            .as_ref()
                                            .map(|t| t.name.as_str())
                                            .unwrap_or("?")
                                    ),
                                )
                                .with_code("SE-034")
                            })?;
                            construct_type(Some(fixed), model.clone())?
                        }
                        // Внутренний инвариант: форму параметра проверяет `SE-034`
                        // раньше, сюда приходит только то, что она
                        // пропустила.
                        _ => return Err(internal("параметр функции без типа")),
                    };
                    let param_name = param
                        .clone()
                        .name
                        .map(|t| t.name.clone())
                        .unwrap_or_default();
                    params.push((param_name, param_type));
                }
            }
            // Атрибут объявления судится здесь: у внешней функции сырого АСД в узле не
            // остаётся, а сказать об ошибке надо и о ней.
            check_attribute(&def)?;
            // Режим и позиция атрибута снимаются до разбора типа возврата: дальше `def`
            // частично перемещается в конструкторы узлов.
            let attribute_mode = inline_mode(&def);
            let attribute_loc = def.attribute.as_ref().map_or(def.loc, |a| a.loc);
            let rett = match def.return_type {
                Some(t) => construct_type(Some(t), model.clone())?,
                None => TypeNode::Unit,
            };
            if def.external {
                Ok(FunctionDefinitionNode::External {
                    upper: Some(Rc::downgrade(&model)),
                    loc: def.loc,
                    name: name.clone(),
                    params,
                    ret: rett,
                })
            } else {
                let statement = if let Some(body) = def.body {
                    resolve_statement(
                        &StatementNode::Unresolved(body),
                        params.clone(),
                        model.clone(),
                    )?
                } else {
                    // SE-118: код и позиция объявления.
                    return Err(Diagnostic::error(
                        def.loc,
                        format!(
                            "локальная функция '{name}' объявлена без тела: тело обязательно \
                             (внешнюю функцию объявляют как 'extern fn')"
                        ),
                    )
                    .with_code("SE-118"));
                };
                // Атрибут `[inline]` обязывает подстановку, а она выражается только для
                // тела с единственным хвостовым возвратом. Молча оставить вызов значило
                // бы не исполнить написанное автором, поэтому - `SE-128` с названным
                // обходом.
                if matches!(attribute_mode, InlineMode::Always)
                    && let Some(why) =
                        crate::semantic::inline::inline_obstacle(&statement, &rett, &model.borrow())
                {
                    return Err(crate::semantic::inline::inline_refusal(
                        attribute_loc,
                        &name,
                        why,
                    ));
                }
                Ok(FunctionDefinitionNode::Local {
                    upper: Some(Rc::downgrade(&model)),
                    loc: def.loc,
                    name: name.clone(),
                    params,
                    ret: rett,
                    body: statement,
                    raw,
                })
            }
        }
    } else if let FunctionDefinitionNode::None = func {
        // Внутренний инвариант: неизвестное имя отсекает `SE-004` раньше (проба
        // 2026-08-19).
        Err(internal("узел функции не определён"))
    } else {
        Ok(func)
    }
}

// -- Тесты ---------------------------------------------------------------------

/// Разбирает `q(m, n)` в параметре как тип, а не вызов.
///
/// Тип параметра грамматика хранит выражением, и форма `q(8, 8)` приходит сюда вызовом
/// функции: `q` - обычный идентификатор (решение 0061), а правило `Type` в позицию
/// параметра не подставить - оно даёт конфликт с вызовом. Поэтому форму опознаёт
/// семантика: два **числовых** аргумента и имя, которое дальше проверит
/// `construct_fixed` (`SE-057` на чужое имя).
///
/// `None` - форма не похожа на конструктор типа: тогда вызывающий отвечает `SE-034`,
/// называя, что ожидалось.
///
/// Модификатор `sat` в параметре **невыразим**: `q(8, 8) sat` в позиции выражения -
/// идентификатор за вызовом, и разбор отвергает его `SY-002`. Граница названа, замер её
/// подтверждает.
fn fixed_from_call(
    loc: crate::diagnostics::Location,
    ctor: &ast::Identifier,
    args: &[ast::Expression],
) -> Option<ast::Type> {
    let [ast::Expression::Number(_, m), ast::Expression::Number(_, n)] = args else {
        return None;
    };
    Some(ast::Type::Fixed(loc, ctor.name.clone(), *m, *n, None))
}

#[cfg(test)]
mod tests {
    use crate::parse;
    use crate::semantic::FunctionDefinitionNode;
    use crate::semantic::tree::construct_model;
    use crate::semantic::type_node::TypeNode;

    /// Строит модель и возвращает корневой ModelNode.
    fn build(src: &str) -> Result<crate::semantic::ModelNode, crate::diagnostics::Diagnostic> {
        let (ast, _) = parse(src, 0).expect("ошибка разбора");
        construct_model(&ast, None, &[]).map(|m| m.take())
    }

    // -- Внешние функции -------------------------------------------------------

    /// `extern fn foo(x: bit);` разрешается в `FunctionNode::External`.
    ///
    /// # Пример (Takt)
    /// ```but
    /// extern fn send(data: bit);
    /// ```
    #[test]
    fn extern_fn_no_params_resolves_to_external() {
        let node = build("extern fn foo(x: bit);").unwrap();
        match node.functions.get("foo").expect("функция foo не найдена") {
            FunctionDefinitionNode::External {
                name, params, ret, ..
            } => {
                assert_eq!(name, "foo");
                assert_eq!(params.len(), 1);
                assert_eq!(params[0].0, "x");
                assert_eq!(params[0].1, TypeNode::Bit);
                assert_eq!(*ret, TypeNode::Unit);
            }
            other => panic!("ожидался External, получен {:?}", other),
        }
    }

    /// `extern fn foo() -> bit;` - внешняя функция с возвращаемым типом.
    #[test]
    fn extern_fn_with_return_type() {
        let node = build("extern fn status() -> bit;").unwrap();
        match node.functions.get("status").expect("status не найдена") {
            FunctionDefinitionNode::External { ret, .. } => assert_eq!(*ret, TypeNode::Bit),
            other => panic!("ожидался External, получен {:?}", other),
        }
    }

    /// `extern fn` без параметров - пустой список параметров.
    #[test]
    fn extern_fn_no_return_type_defaults_to_unit() {
        let node = build("extern fn noop();").unwrap();
        match node.functions.get("noop").expect("noop не найдена") {
            FunctionDefinitionNode::External { params, ret, .. } => {
                assert!(params.is_empty(), "параметров быть не должно");
                assert_eq!(*ret, TypeNode::Unit);
            }
            other => panic!("ожидался External, получен {:?}", other),
        }
    }

    // -- Локальные функции -----------------------------------------------------

    /// `fn id(x: bit) -> bit { return true; }` разрешается в `FunctionNode::Local`.
    ///
    /// Параметры функции (`x`) сейчас не добавляются в область видимости модели,
    /// поэтому тело использует литерал `true` вместо параметра `x`.
    ///
    /// # Пример (Takt)
    /// ```but
    /// fn ready() -> bit {
    ///     return true;
    /// }
    /// ```
    #[test]
    fn local_fn_resolves_to_local() {
        let node = build("fn id(x: bit) -> bit { return true; }").unwrap();
        match node.functions.get("id").expect("id не найдена") {
            FunctionDefinitionNode::Local {
                name, params, ret, ..
            } => {
                assert_eq!(name, "id");
                assert_eq!(params.len(), 1);
                assert_eq!(params[0].0, "x");
                assert_eq!(params[0].1, TypeNode::Bit);
                assert_eq!(*ret, TypeNode::Bit);
            }
            other => panic!("ожидался Local, получен {:?}", other),
        }
    }

    /// `fn noop() { }` - функция без параметров и без возвращаемого типа.
    #[test]
    fn local_fn_no_params_no_return() {
        let node = build("fn noop() { }").unwrap();
        match node.functions.get("noop").expect("noop не найдена") {
            FunctionDefinitionNode::Local { params, ret, .. } => {
                assert!(params.is_empty());
                assert_eq!(*ret, TypeNode::Unit);
            }
            other => panic!("ожидался Local, получен {:?}", other),
        }
    }

    /// `fn add(a: bit, b: bit) -> bit { return true; }` - несколько параметров.
    ///
    /// Тело использует литерал, т.к. параметры не в области видимости модели.
    #[test]
    fn local_fn_multiple_params() {
        let node = build("fn add(a: bit, b: bit) -> bit { return true; }").unwrap();
        match node.functions.get("add").expect("add не найдена") {
            FunctionDefinitionNode::Local { params, .. } => {
                assert_eq!(params.len(), 2);
                assert_eq!(params[0].0, "a");
                assert_eq!(params[0].1, TypeNode::Bit);
                assert_eq!(params[1].0, "b");
                assert_eq!(params[1].1, TypeNode::Bit);
            }
            other => panic!("ожидался Local, получен {:?}", other),
        }
    }

    /// Функция с псевдонимом типа в параметре: `extern fn foo(x: u8);` - u8 встроенный
    /// тип.
    #[test]
    fn extern_fn_alias_param_resolves() {
        let node = build("extern fn foo(x: u8);").unwrap();
        match node.functions.get("foo").expect("foo не найдена") {
            FunctionDefinitionNode::External { params, .. } => {
                assert_eq!(params.len(), 1);
                assert_eq!(
                    params[0].1,
                    TypeNode::Integer {
                        bits: 8,
                        signed: false
                    }
                );
            }
            other => panic!("ожидался External, получен {:?}", other),
        }
    }

    // -- Контрпримеры ----------------------------------------------------------

    /// Контрпример: нельзя объявить функцию без имени.
    ///
    /// Примечание: такой синтаксис не проходит парсер, поэтому проверяем напрямую через
    /// `construct_function` с вручную созданным узлом.
    #[test]
    fn function_none_node_is_error() {
        use super::*;
        use std::cell::RefCell;
        use std::rc::Rc;
        let model = Rc::new(RefCell::new(ModelNode::default()));
        let result = construct_function(FunctionDefinitionNode::None, model);
        assert!(result.is_err(), "FunctionNode::None должен давать ошибку");
    }

    /// Контрпример: `FunctionNode::Local` уже разрешён - возвращается без изменений.
    #[test]
    fn already_resolved_local_passthrough() {
        use super::*;
        use crate::semantic::StatementNode;
        use std::cell::RefCell;
        use std::rc::Rc;
        let local = FunctionDefinitionNode::Local {
            upper: None,
            loc: Default::default(),
            name: "f".into(),
            params: vec![],
            ret: TypeNode::Unit,
            body: StatementNode::None,
            raw: Box::default(),
        };
        let model = Rc::new(RefCell::new(ModelNode::default()));
        let result = construct_function(local.clone(), model).unwrap();
        assert_eq!(result, local);
    }
}
