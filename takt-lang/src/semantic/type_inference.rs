//! Вывод типов для переменных языка Takt.
//!
//! Основная функция [`type_inference`] обходит таблицу переменных и для каждой
//! переменной с [`TypeNode::Inference`] (тип не задан явно) вызывает [`extract_type`],
//! чтобы определить тип из инициализирующего выражения.
//!
//! ## Алгоритм вывода
//!
//! 1. Числовой литерал (Number(n)) -> минимальный целочисленный тип [bit;8/16/32/64]
//! 2. Литерал `Bool` -> `Bool`; `Rational` -> `Rational`; `Duration` -> `Duration`
//! 3. Переменная -> тип referenced переменной
//! 4. Условие -> `Bit`
//! 5. Арифметика -> наиболее "широкий" тип из операндов (`Bit` < `Rational`)
//! 6. Логика/сравнение -> `Bit`
//! 7. Скобки -> тип внутреннего выражения
//! 8. Приведение типа (`as T`) -> `T`
//! 9. Перечисление (`Type::Enum(name)`) -> `TypeNode::Enum(name)`
//! 10. Прочее -> `Unsupported`
//!
//! ## Расширение типов (`wider_type`)
//!
//! При выводе типа бинарных выражений выбирается "более широкий" тип:
//!
//! | Операнды | Результат |
//! |---------------------------------|------------------------|
//! | `Enum("X")` + `Enum("X")` | `Enum("X")` |
//! | `Enum("X")` + `Enum("Y")` | `Unsupported` |
//! | `Enum(_)` + любой не-Enum | `Unsupported` |
//! | `Rational` + любой | `Rational` |
//! | `u16` + `u8` (именованные целые)| `u16` (шире; знак - от знакового) |
//! | `u16` + `[bit;8]` (литерал) | `u16` (тип источника, не литерала) |
//! | `u8` + `bit` / `bool` | `u8` |
//! | `Array(N)` + `Array(M)` | `Array(max(N,M), ...)` |
//! | `Bool` + `Bit` | `Bit` |

use crate::diagnostics::Diagnostic;
use crate::parser::ast::Type;
use crate::semantic::type_node::TypeNode;
use crate::semantic::{ExpressionNode, ModelNode, VariableNode};
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::rc::Rc;

/// Запускает вывод типов для всех переменных с незаданным типом.
///
/// Для каждой переменной [`VariableNode::Simple`] или [`VariableNode::Const`] с типом
/// [`TypeNode::Inference`] вызывает [`extract_type`] и заменяет `Inference` на
/// выведенный тип.
///
/// # Ошибки
///
/// Пробрасывает [`Diagnostic`] из [`extract_type`].
pub fn type_inference(
    variables: &mut BTreeMap<String, VariableNode>,
    model: Rc<RefCell<ModelNode>>,
) -> Result<BTreeMap<String, VariableNode>, Diagnostic> {
    // Первый проход дополнительно понижает q(m, n); последующие только выводят типы
    // (см. заголовок функции о неподвижной точке).
    let mut first_pass = true;
    loop {
        let mut progress = false;
        for (name, var) in variables.clone() {
            match var {
                VariableNode::Simple {
                    upper,
                    loc,
                    ty: TypeNode::Inference,
                    ref expr,
                    ..
                } => {
                    let typ = extract_type_known(expr, model.clone(), Some(variables))?;
                    progress |= !matches!(typ, TypeNode::Inference);
                    variables.insert(
                        name.clone(),
                        VariableNode::Simple {
                            upper,
                            loc,
                            name: name.clone(),
                            ty: typ,
                            expr: expr.clone(),
                        },
                    );
                }
                VariableNode::Const {
                    upper,
                    loc,
                    ty: TypeNode::Inference,
                    ref expr,
                    ..
                } => {
                    let typ = extract_type_known(expr, model.clone(), Some(variables))?;
                    progress |= !matches!(typ, TypeNode::Inference);
                    variables.insert(
                        name.clone(),
                        VariableNode::Const {
                            upper,
                            loc,
                            name: name.clone(),
                            ty: typ,
                            expr: expr.clone(),
                        },
                    );
                }
                // q(m,n): понижение литерала в представление v - см. type_node.
                ref other => {
                    if first_pass
                        && let Some(nv) =
                            crate::semantic::type_node::type_fixed::lower_fixed_var(other)?
                    {
                        variables.insert(name.clone(), nv);
                    }
                }
            }
        }
        first_pass = false;
        if !progress {
            break;
        }
    }
    Ok(variables.clone())
}

// -- Вспомогательные функции ----------------------------------------------------

/// Возвращает тип переменной из её узла.
#[inline]
fn type_of_var(var: &VariableNode) -> TypeNode {
    match var {
        VariableNode::Simple { ty, .. }
        | VariableNode::Port { ty, .. }
        | VariableNode::Const { ty, .. } => ty.clone(),
        VariableNode::Unresolved => TypeNode::Inference,
    }
}

/// Возвращает наиболее "широкий" тип из двух.
///
/// ## Правила расширения
///
/// | Пара типов | Результат |
/// |-------------------------------------|----------------------------|
/// | `Enum("X")` + `Enum("X")` | `Enum("X")` (одинаковые) |
/// | `Enum("X")` + `Enum("Y")` | `Unsupported` (разные) |
/// | `Enum(_)` + любой не-Enum | `Unsupported` |
/// | `Rational` + любой | `Rational` |
/// | `Integer(b1,s1)` + `Integer(b2,s2)` | `Integer(max(b1,b2), s1∨s2)` |
/// | `Integer(b,s)` + `Array(n, Bit)`, `n <= b` | `Integer(b, s)` |
/// | `Integer(b,s)` + `Bit` / `Bool` | `Integer(b, s)` |
/// | `Array(N, T)` + `Array(M, T)` | `Array(max(N,M), T)` |
/// | `Array(N, T)` + скаляр | `Array(N, T)` |
/// | `Bool` + `Bit` | `Bit` |
/// | `Bool` + `Bool` | `Bool` |
/// | `Bit` + `Bit` | `Bit` |
/// | иначе | `Unsupported` |
///
/// `wider_type` - `pub(crate)`; тесты - в модуле `tests` ниже.
#[inline]
pub(crate) fn wider_type(a: TypeNode, b: TypeNode) -> TypeNode {
    match (&a, &b) {
        // Ce4: два одинаковых перечисления -> сохраняем тип перечисления
        (TypeNode::Enum(na), TypeNode::Enum(nb)) if na == nb => TypeNode::Enum(na.clone()),
        // Ce4: разные перечисления или перечисление с не-перечислением -> несовместимо
        (TypeNode::Enum(_), _) | (_, TypeNode::Enum(_)) => TypeNode::Unsupported,
        (TypeNode::Rational, _) | (_, TypeNode::Rational) => TypeNode::Rational,
        // Именованные целые `u8`...`i64`. Правила не было вовсе, и объявленный тип
        // источника проигрывал типу литерала: `[bit;N]` от `infer_int_type`
        // перехватывался ветвями `Array` ниже.
        //
        // Ширина - наибольшая из двух, знак - знаковый, если знаков хотя бы один.
        // Случай "результат не влезает в наибольшую ширину" (`i8 + u8` со значением
        // 200) лечит - расширение по результату; второго знания о границах типа здесь
        // не заводится.
        (
            TypeNode::Integer {
                bits: ba,
                signed: sa,
            },
            TypeNode::Integer {
                bits: bb,
                signed: sb,
            },
        ) => TypeNode::Integer {
            bits: *ba.max(bb),
            signed: *sa || *sb,
        },
        // Граница: именованное целое побеждает битовый вектор, только если вектор в
        // него влезает (`n <= bits`). Вектор шире - это уже не "литерал уточняет тип
        // источника", и решение остаётся за ветвями `Array` ниже: `Integer::bits` шире
        // 64 не бывает по построению.
        (TypeNode::Integer { bits, signed }, TypeNode::Array(n, t))
        | (TypeNode::Array(n, t), TypeNode::Integer { bits, signed })
            if matches!(**t, TypeNode::Bit) && u16::from(*bits) >= *n =>
        {
            TypeNode::Integer {
                bits: *bits,
                signed: *signed,
            }
        }
        // Бит и логическое значение уточняют тип целого, как `Bool` уточняет `Bit`
        // ниже: `var g := F + flag;` (`u8` + `bit`) прежде давал `Unsupported`, то есть
        // `SIM-007` у эталона и `RS-014` у цели `rust` на записи, вычислимой без
        // затруднений.
        (TypeNode::Integer { bits, signed }, TypeNode::Bit | TypeNode::Bool)
        | (TypeNode::Bit | TypeNode::Bool, TypeNode::Integer { bits, signed }) => {
            TypeNode::Integer {
                bits: *bits,
                signed: *signed,
            }
        }
        // Из двух массивов выбирается наибольший по размеру
        (TypeNode::Array(n, t), TypeNode::Array(m, s)) => {
            if n >= m {
                TypeNode::Array(*n, t.clone())
            } else {
                TypeNode::Array(*m, s.clone())
            }
        }
        (TypeNode::Array(n, t), _) => TypeNode::Array(*n, t.clone()),
        (_, TypeNode::Array(n, t)) => TypeNode::Array(*n, t.clone()),
        // Длительность сочетается только с длительностью. Правила не было вовсе, и
        // `const S := 2s + 1s;` давал `Unsupported` - тип, который не переводит ни одна
        // цель. Смешение с числом сюда не доходит: его отвергает `SE-065` (проба
        // 2026-08-16), поэтому прочие пары длительности остаются `Unsupported`
        // осознанно.
        (TypeNode::Duration, TypeNode::Duration) => TypeNode::Duration,
        (TypeNode::Bit, TypeNode::Bit) => TypeNode::Bit,
        (TypeNode::Bool, TypeNode::Bool) => TypeNode::Bool,
        (TypeNode::Bool, TypeNode::Bit) | (TypeNode::Bit, TypeNode::Bool) => TypeNode::Bit,
        // q(m,n): два одинаковых fixed-point -> тот же тип; смешение с любым другим
        // типом (иное q, целое, float, ...) даёт `Unsupported` и ловится стражем T6
        // (`validate::fixed`) - см. Признак `sat` входит в формат: `q(8,8)` и `q(8,8)
        // sat` - РАЗНЫЕ типы, их смешение даёт `Unsupported` и ловится стражем
        // (`SE-103`). Иначе выражение унаследовало бы семантику переполнения случайного
        // операнда - молча.
        (
            TypeNode::Fixed {
                m: ma,
                n: na,
                sat: sa,
            },
            TypeNode::Fixed {
                m: mb,
                n: nb,
                sat: sb,
            },
        ) if ma == mb && na == nb && sa == sb => TypeNode::Fixed {
            m: *ma,
            n: *na,
            sat: *sa,
        },
        _ => TypeNode::Unsupported,
    }
}

/// Определяет минимальный целочисленный тип для числового литерала.
///
/// Начинает с 8 бит и удваивает размер, пока значение не вмещается:
/// - `0..=255` -> `[bit;8]`
/// - `256..=65535` -> `[bit;16]`
/// - `65536..=4294967295` -> `[bit;32]`
/// - иначе (или отрицательное) -> `[bit;64]`
pub(crate) fn infer_int_type(n: i128) -> TypeNode {
    let arr = |size| TypeNode::Array(size, Box::new(TypeNode::Bit));
    if n >= 0 && n <= i128::from(u8::MAX) {
        arr(8)
    } else if n >= 0 && n <= i128::from(u16::MAX) {
        arr(16)
    } else if n >= 0 && n <= i128::from(u32::MAX) {
        arr(32)
    } else {
        arr(64)
    }
}

/// Преобразует АСД-тип [`Type`] в семантический [`TypeNode`] без контекста модели.
///
/// Используется при выводе типа для выражений `as T` (приведение типа) и при Ce6-выводе
/// из возвращаемого типа функции.
///
/// Псевдонимы встроенных типов (`bool`, `bit`, `float`, `unit`) разрешаются.
/// Пользовательские псевдонимы (`Type::Alias`) возвращают `Unsupported` - для их
/// разрешения используйте [`ast_type_to_node_ctx`]. Перечисления (`Type::Enum`)
/// возвращают `TypeNode::Enum(name)` - факт того, что перечисление объявлено,
/// проверяется в `validate_model`.
///
/// | АСД-тип | Результат |
/// |----------------------|-------------------------------|
/// | `Type::Enum("X")` | `TypeNode::Enum("X")` |
/// | `Type::Alias(local)` | `TypeNode::Unsupported` |
/// | `Type::Function` | `TypeNode::Unsupported` |
/// | `Type::Address` | `TypeNode::Unsupported` |
/// Строит [`TypeNode::Fixed`] из конструктора `q(m, n)` без диагностики (для
/// цели приведения `as`, где путь инфраллибелен). Конструктор не `q` или
/// границы вне (`m >= 1`, `n >= 1`, `m + n <= 64`) ->
/// [`TypeNode::Unsupported`]; объявление типа тот же случай ловит `SE-057`
/// (`construct_fixed`).
fn fixed_node_or_unsupported(ctor: &str, m: i128, n: i128, modifier: Option<&str>) -> TypeNode {
    // Модификатор допустим только `sat`; прочее - `Unsupported`, а называющую
    // диагностику `SE-104` даёт объявление (`construct_fixed`).
    let sat = match modifier {
        None => Some(false),
        Some("sat") => Some(true),
        Some(_) => None,
    };
    if let (true, Some(sat)) = (ctor == "q" && m >= 1 && n >= 1 && m + n <= 64, sat) {
        TypeNode::Fixed {
            m: m as u8,
            n: n as u8,
            sat,
        }
    } else {
        TypeNode::Unsupported
    }
}

pub(crate) fn ast_type_to_node(ty: &Type) -> TypeNode {
    match ty {
        Type::Bit => TypeNode::Bit,
        Type::Bool => TypeNode::Bool,
        Type::Rational => TypeNode::Rational,
        Type::Unit => TypeNode::Unit,
        Type::Array {
            element_count,
            element_type,
            ..
        } => TypeNode::Array(*element_count, Box::new(ast_type_to_node(element_type))),
        // q(m, n): цель приведения `x as q(m, n)`. Границы - те же, что у объявления;
        // нарушение даёт `Unsupported` (страж T6/T7).
        Type::Fixed(_, ctor, m, n, modifier) => {
            fixed_node_or_unsupported(ctor, *m, *n, modifier.as_deref())
        }
        // Ce4: перечисление по имени - без проверки существования (нет контекста)
        Type::Enum(name) => TypeNode::Enum(name.clone()),
        // Ce6: встроенные имена - через единый разбор. Пользовательский псевдоним без
        // контекста модели по-прежнему неизвестен.
        Type::Alias(id) => crate::semantic::type_node::builtin_type_by_name(&id.name)
            .unwrap_or(TypeNode::Unsupported),
        // Type::Function, Type::Address - не поддерживаются при выводе типа
        _ => TypeNode::Unsupported,
    }
}

/// Преобразует АСД-тип в семантический с разрешением пользовательских псевдонимов и
/// перечислений через контекст модели.
///
/// FE2/Ce6/Ce4: В отличие от [`ast_type_to_node`], эта функция ищет:
/// - псевдонимы типов в `ModelNode::types` (`type Byte = [bit;8]` -> `Array(8, Bit)`)
/// - перечисления в `ModelNode::enums` (`Color` -> `Enum("Color")` если объявлено)
///
/// Если перечисление не найдено - возвращает `TypeNode::Unsupported`; ошибка будет
/// диагностирована при `validate_model`.
///
/// | АСД-тип | Результат |
/// |----------------------|---------------------------------------------------|
/// | `Type::Enum("X")` | `TypeNode::Enum("X")` если X объявлен |
/// | `Type::Enum("X")` | `TypeNode::Unsupported` если X не найден |
/// | `Type::Alias(local)` | из `types` или `TypeNode::Unsupported` |
pub(crate) fn ast_type_to_node_ctx(ty: &Type, model: Rc<RefCell<ModelNode>>) -> TypeNode {
    match ty {
        Type::Bit => TypeNode::Bit,
        Type::Bool => TypeNode::Bool,
        Type::Rational => TypeNode::Rational,
        Type::Unit => TypeNode::Unit,
        Type::Array {
            element_count,
            element_type,
            ..
        } => TypeNode::Array(
            *element_count,
            Box::new(ast_type_to_node_ctx(element_type, model)),
        ),
        // q(m, n): цель приведения `x as q(m, n)` (см.
        Type::Fixed(_, ctor, m, n, modifier) => {
            fixed_node_or_unsupported(ctor, *m, *n, modifier.as_deref())
        }
        // Ce4: перечисление по имени - проверяем наличие в контексте модели
        Type::Enum(name) => {
            let borrowed = model.borrow();
            if borrowed.search_enum(name).is_some() {
                TypeNode::Enum(name.clone())
            } else {
                // Перечисление не найдено; validate_model сообщит об ошибке
                TypeNode::Unsupported
            }
        }
        Type::Alias(id) => match id.name.as_str() {
            "bit" => TypeNode::Bit,
            "bool" => TypeNode::Bool,
            "float" => TypeNode::Rational,
            "unit" => TypeNode::Unit,
            name => {
                // FE2: ищем пользовательский псевдоним типа в модели
                let borrowed = model.borrow();
                if let Some(alias_type) = borrowed.types.get(name) {
                    alias_type.clone()
                } else {
                    TypeNode::Unsupported
                }
            }
        },
        _ => TypeNode::Unsupported,
    }
}

/// Выводит тип семантического выражения.
///
/// Рекурсивно обходит выражение и возвращает [`TypeNode`] в соответствии с описанными в
/// модуле правилами вывода.
///
/// # Ошибки
///
/// В текущей реализации всегда успешен. Возвращает `Unsupported` для выражений, тип
/// которых не поддаётся автоматическому выводу (например, вызовы функций с неизвестной
/// сигнатурой).
pub(crate) fn extract_type(
    expr: &ExpressionNode,
    model: Rc<RefCell<ModelNode>>,
) -> Result<TypeNode, Diagnostic> {
    extract_type_known(expr, model, None)
}

/// Тип выражения с оглядкой на **уже выведенные** объявления.
///
/// `known` - таблица объявлений в том состоянии, в каком её видит вывод типов; `None` у
/// внешних вызовов (генераторы, `validate`), где вывод давно завершён.
fn extract_type_known(
    expr: &ExpressionNode,
    model: Rc<RefCell<ModelNode>>,
    known: Option<&BTreeMap<String, VariableNode>>,
) -> Result<TypeNode, Diagnostic> {
    match expr {
        // -- Литералы ----------------------------------------------------------
        ExpressionNode::Bool(_) => Ok(TypeNode::Bool),
        ExpressionNode::Number(n) => Ok(infer_int_type(*n)),
        ExpressionNode::Duration(_) => Ok(TypeNode::Duration),
        ExpressionNode::Rational(_, _) => Ok(TypeNode::Rational),
        // Тип обращения по адресу - тип доступа, заданный автором.
        ExpressionNode::AnonPort(access) => Ok(access.ty.clone()),

        // -- Идентификаторы ----------------------------------------------------
        //
        // Ячейка ссылки - снимок до вывода типов (см. заголовок функции), поэтому
        // `Inference` в ней означает "здесь может быть уже известно": спрашиваем
        // таблицу объявлений по имени.
        ExpressionNode::Variable(var_rc) => {
            let var = var_rc.borrow();
            let ty = type_of_var(&var);
            if matches!(ty, TypeNode::Inference)
                && let Some(table) = known
                && let Some(declared) = table.get(var.name())
            {
                return Ok(type_of_var(declared));
            }
            Ok(ty)
        }
        // Условия всегда вычисляются в булев (1-битный) результат.
        ExpressionNode::Condition(_) => Ok(TypeNode::Bit),

        // -- Скобки ------------------------------------------------------------
        ExpressionNode::Parenthesis(inner) => extract_type_known(inner, model, known),

        // -- Логические операции и сравнения -> Bit -----------------------------
        ExpressionNode::Not(_)
        | ExpressionNode::And(_, _)
        | ExpressionNode::Or(_, _)
        | ExpressionNode::Equal(_, _)
        | ExpressionNode::NotEqual(_, _)
        | ExpressionNode::Less(_, _)
        | ExpressionNode::More(_, _)
        | ExpressionNode::LessEqual(_, _)
        | ExpressionNode::MoreEqual(_, _) => Ok(TypeNode::Bit),

        // -- Унарные операции -> тип операнда ----------------------------------
        ExpressionNode::BitwiseNot(e)
        | ExpressionNode::UnaryPlus(e)
        | ExpressionNode::Negate(e) => extract_type_known(e, model, known),

        // -- Арифметические бинарные операции -> наиболее широкий тип ----------
        ExpressionNode::Add(l, r)
        | ExpressionNode::Subtract(l, r)
        | ExpressionNode::Multiply(l, r)
        | ExpressionNode::Divide(l, r)
        | ExpressionNode::Modulo(l, r)
        | ExpressionNode::Power(l, r) => {
            let lt = extract_type_known(l, model.clone(), known)?;
            let rt = extract_type_known(r, model, known)?;
            Ok(wider_type(lt, rt))
        }

        // -- Побитовые бинарные операции -> наиболее широкий тип ---------------
        ExpressionNode::BitwiseAnd(l, r)
        | ExpressionNode::BitwiseXor(l, r)
        | ExpressionNode::BitwiseOr(l, r)
        | ExpressionNode::ShiftLeft(l, r)
        | ExpressionNode::ShiftRight(l, r) => {
            let lt = extract_type_known(l, model.clone(), known)?;
            let rt = extract_type_known(r, model, known)?;
            Ok(wider_type(lt, rt))
        }

        // -- Тернарный оператор -> тип наиболее широкой ветви ------------------
        ExpressionNode::ConditionalOperator(_, then_e, else_e) => {
            let tt = extract_type_known(then_e, model.clone(), known)?;
            let et = extract_type_known(else_e, model, known)?;
            Ok(wider_type(tt, et))
        }

        // -- Присваивание -> тип правой части ----------------------------------
        ExpressionNode::Assign(_, r) => extract_type_known(r, model, known),

        // -- Обращение к массиву -> тип элемента ------------------------------- База -
        // выражение: её тип выводится тем же проходом.
        ExpressionNode::ArraySubscript(base, _) => match extract_type_known(base, model, known)? {
            TypeNode::Array(_, elem_type) => Ok(*elem_type),
            other => Ok(other),
        },

        // -- Срез массива -> тип элемента --------------------------------------
        ExpressionNode::ArraySlice(base, _, _) => match extract_type_known(base, model, known)? {
            TypeNode::Array(_, elem_type) => Ok(*elem_type),
            other => Ok(other),
        },

        // -- Массивный литерал -> Array(N, тип_элемента) -----------------------
        ExpressionNode::Array(items) => {
            let n = items.len() as u16;
            if items.is_empty() {
                return Ok(TypeNode::Array(0, Box::new(TypeNode::Bit)));
            }
            let elem_type = extract_type_known(&items[0], model, known)?;
            Ok(TypeNode::Array(n, Box::new(elem_type)))
        }

        // -- Инициализатор структуры -> тип первого элемента -------------------
        ExpressionNode::Initializer(items) => {
            if items.is_empty() {
                return Ok(TypeNode::Unsupported);
            }
            let n = items.len() as u16;
            let elem_type = extract_type_known(&items[0], model, known)?;
            Ok(TypeNode::Array(n, Box::new(elem_type)))
        }

        // -- Приведение типа -> результирующий тип (FE2: с разрешением псевдонимов) --
        ExpressionNode::Cast(_, ty) => Ok(ty.clone()),
        ExpressionNode::Type(ty) => Ok(ast_type_to_node(ty)),

        // -- Ce6: Вывод типа из возвращаемого типа функции --------------------
        //
        // Если инициализирующее выражение - вызов известной функции, тип переменной
        // выводится из возвращаемого типа функции. Это реализует двунаправленный вывод:
        // `var result = add(1, 2);` -> тип `result` = возвращаемый тип `add`.
        //
        // Поддерживаются разрешённые функции (Local, External, Builtin) и неразрешённые
        // (Unresolved), для которых тип берётся из AST-определения.
        ExpressionNode::Function(func_rc, _args) => {
            let func = func_rc.borrow();
            let ret_type = match &*func {
                crate::semantic::FunctionDefinitionNode::Local { ret, .. } => ret.clone(),
                crate::semantic::FunctionDefinitionNode::External { ret, .. } => ret.clone(),
                crate::semantic::FunctionDefinitionNode::Builtin(_, _, ret) => ret.clone(),
                // Ce6+FE2: функция ещё не разрешена - читаем return_type из AST, с
                // разрешением пользовательских псевдонимов через контекст модели
                crate::semantic::FunctionDefinitionNode::Unresolved(def) => {
                    if let Some(ret_ast) = &def.return_type {
                        ast_type_to_node_ctx(ret_ast, model.clone())
                    } else {
                        TypeNode::Unit // функция без return_type -> void/Unit
                    }
                }
                crate::semantic::FunctionDefinitionNode::None => TypeNode::Unsupported,
            };
            Ok(ret_type)
        }

        // -- Поле структуры -> тип поля ----------------------------
        //
        //
        // Доступ к разряду (`x.3`, `Member::Number`) сюда не входит: его тип - вопрос,
        // и трогать его этой фичей нельзя.
        ExpressionNode::BitAccess(inner, crate::parser::ast::Member::Identifier(field)) => {
            let base = extract_type_known(inner, model.clone(), known)?;
            let TypeNode::Struct(struct_name) = base else {
                return Ok(TypeNode::Unsupported);
            };
            let found = model.borrow().search_struct(&struct_name).and_then(|def| {
                def.fields
                    .iter()
                    .find(|(name, _)| *name == field.name)
                    .map(|(_, ty)| ty.clone())
            });
            Ok(found.unwrap_or(TypeNode::Unsupported))
        }

        // -- Выражения без выводимого типа ------------------------------------
        ExpressionNode::String(_)
        | ExpressionNode::Address(_, _)
        | ExpressionNode::Model(_)
        | ExpressionNode::BitAccess(_, _)
        | ExpressionNode::CodeBlock(_, _)
        | ExpressionNode::NamedFunctionBox(_, _)
        | ExpressionNode::List(_)
        | ExpressionNode::None
        | ExpressionNode::Unresolved(_) => Ok(TypeNode::Unsupported),
    }
}

// -- Тесты ---------------------------------------------------------------------

/// Тесты вынесены в подмодуль-файл.
#[cfg(test)]
#[path = "type_inference/tests.rs"]
mod tests;
