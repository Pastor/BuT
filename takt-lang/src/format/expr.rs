//! Печать выражений, условий, типов и объявлений переменных.
//!
//! Скобки **не пересчитываются**: `Parenthesis` - явный узел АСД, поэтому авторская
//! расстановка сохраняется, а печать остаётся семантически нейтральной
//! без анализа приоритетов.

use super::FormatError;
use crate::parser::ast;

/// Печатает выражение.
pub(crate) fn expression(expr: &ast::Expression) -> Result<String, FormatError> {
    use ast::Expression as E;
    Ok(match expr {
        // -- Литералы и имена ------------------------------------------------- Число
        // печатается как написано: `0xF0` не превращается в `240`. Тот же принцип, что
        // у литерала длительности ниже.
        E::Number(loc, n) => super::literal::number(*loc, *n),
        // Литерал длительности печатается как написан: `1m30s` не канонизируется в
        // `90s` - АСД хранит выбор автора.
        E::Duration(_, _, text) => text.clone(),
        E::Rational(_, s, negative) => {
            if *negative {
                format!("-{s}")
            } else {
                s.clone()
            }
        }
        E::Bool(_, b) => b.to_string(),
        E::String(parts) => string_literal(parts),
        E::Variable(id) => id.name.clone(),
        E::Type(_, t) => ty(t)?,
        E::Address(_, addr, bit) => format!("0x{addr:X}:{bit}"),
        // Анонимное обращение: позиция бита печатается, только если автор её записал.
        // `#0x100` и `#0x100:0` означают одно и то же, но канонизировать одну форму в
        // другую нельзя - форматтер сохраняет выбор автора (то же правило, что у
        // `while`/`loop`).
        E::AnonAddress(_, addr, bit) => anon_address(*addr, *bit),
        E::List(_, params) => parameter_list(params)?,
        E::Array(_, items) | E::Initializer(_, items) => {
            let items = items
                .iter()
                .map(expression)
                .collect::<Result<Vec<_>, _>>()?
                .join(", ");
            format!("{{{items}}}")
        }

        // -- Доступ -----------------------------------------------------------
        E::Parenthesis(_, inner) => format!("({})", expression(inner)?),
        // База - выражение: печатается тем же печатником.
        E::ArraySubscript(_, base, index) => {
            format!("{}[{}]", expression(base)?, expression(index)?)
        }
        E::ArraySlice(_, base, from, to) => format!(
            "{}[{}:{}]",
            expression(base)?,
            from.map(|v| v.to_string()).unwrap_or_default(),
            to.map(|v| v.to_string()).unwrap_or_default()
        ),
        E::BitAccess(_, base, member) => format!("{}.{}", expression(base)?, self::member(member)),
        E::Function(_, id, args) => {
            let args = args
                .iter()
                .map(expression)
                .collect::<Result<Vec<_>, _>>()?
                .join(", ");
            format!("{}({args})", id.name)
        }

        // -- Унарные ----------------------------------------------------------
        E::Not(_, e) => format!("!{}", expression(e)?),
        E::BitwiseNot(_, e) => format!("~{}", expression(e)?),
        E::UnaryPlus(_, e) => format!("+{}", expression(e)?),
        E::Negate(_, e) => format!("-{}", expression(e)?),

        // -- Бинарные ---------------------------------------------------------
        E::Power(_, l, r) => binary(l, "**", r)?,
        E::Multiply(_, l, r) => binary(l, "*", r)?,
        E::Divide(_, l, r) => binary(l, "/", r)?,
        E::Modulo(_, l, r) => binary(l, "%", r)?,
        E::Add(_, l, r) => binary(l, "+", r)?,
        E::Subtract(_, l, r) => binary(l, "-", r)?,
        E::ShiftLeft(_, l, r) => binary(l, "<<", r)?,
        E::ShiftRight(_, l, r) => binary(l, ">>", r)?,
        E::BitwiseAnd(_, l, r) => binary(l, "&", r)?,
        E::BitwiseXor(_, l, r) => binary(l, "^", r)?,
        E::BitwiseOr(_, l, r) => binary(l, "|", r)?,
        E::Less(_, l, r) => binary(l, "<", r)?,
        E::More(_, l, r) => binary(l, ">", r)?,
        E::LessEqual(_, l, r) => binary(l, "<=", r)?,
        E::MoreEqual(_, l, r) => binary(l, ">=", r)?,
        // Инвариант: `=` - сравнение, `:=` - присваивание. Подменять их печатью нельзя
        // ни при каких обстоятельствах.
        E::Equal(_, l, r) => binary(l, "=", r)?,
        E::NotEqual(_, l, r) => binary(l, "!=", r)?,
        E::And(_, l, r) => binary(l, "&&", r)?,
        E::Or(_, l, r) => binary(l, "||", r)?,
        E::Assign(_, l, r) => binary(l, ":=", r)?,
        E::Cast(_, e, t) => format!("{} as {}", expression(e)?, ty(t)?),

        E::ConditionalOperator(_, c, t, e) => format!(
            "{} ? {} : {}",
            expression(c)?,
            expression(t)?,
            expression(e)?
        ),

        // -- Пока не поддержаны - отказ, а не порча исходника -----------------
        E::CodeBlock(loc, _, _) => return Err(super::unsupported(*loc, "блок кода")),
        E::NamedFunction(loc, _, _) => {
            return Err(super::unsupported(*loc, "именованная функция"));
        }
    })
}

fn binary(
    left: &ast::Expression,
    op: &str,
    right: &ast::Expression,
) -> Result<String, FormatError> {
    Ok(format!("{} {op} {}", expression(left)?, expression(right)?))
}

pub(crate) fn member(m: &ast::Member) -> String {
    match m {
        ast::Member::Identifier(id) => id.name.clone(),
        ast::Member::Number(n) => n.to_string(),
    }
}

/// Печатает анонимное обращение к ячейке: `#0x100` либо `#0x100:3`.
///
/// Одна функция на выражения и условия: форма записи у них общая, и разойтись печать не
/// имеет права - иначе одно и то же обращение печаталось бы в ребре иначе, чем в теле.
fn anon_address(addr: i128, bit: Option<i64>) -> String {
    match bit {
        Some(bit) => format!("#0x{addr:X}:{bit}"),
        None => format!("#0x{addr:X}"),
    }
}

/// Печатает условие.
///
/// Отдельная функция, а не переиспользование [`expression`]: `Condition` - своя
/// грамматика с **другой** семантикой `=` (равенство, а не присваивание). Разделение
/// намеренное - сливать печать нельзя.
pub(crate) fn condition(cond: &ast::Condition) -> Result<String, FormatError> {
    use ast::Condition as C;
    Ok(match cond {
        C::Number(loc, n) => super::literal::number(*loc, *n),
        C::Duration(_, _, text) => text.clone(),
        C::After(_, _, text) | C::AfterTicks(_, _, text) => format!("after {text}"),
        // Константная выдержка: печатается вложенное условие как написано - скобки
        // живут в дереве (`Parenthesis`), поэтому `after (BASE + 30s)`
        // восстанавливается вместе с ними.
        C::AfterExpr(_, inner) => format!("after {}", condition(inner)?),
        C::Rational(_, s, negative) => {
            if *negative {
                format!("-{s}")
            } else {
                s.clone()
            }
        }
        C::Bool(_, b) => b.to_string(),
        C::String(parts) => string_literal(parts),
        C::Variable(id) => id.name.clone(),
        C::AnonAddress(_, addr, bit) => anon_address(*addr, *bit),
        C::Parenthesis(_, inner) => format!("({})", condition(inner)?),
        C::ArraySubscript(_, base, index) => {
            format!("{}[{}]", condition(base)?, condition(index)?)
        }
        C::BitAccess(_, base, m) => format!("{}.{}", condition(base)?, member(m)),
        C::Function(_, id, args) => {
            let args = args
                .iter()
                .map(condition)
                .collect::<Result<Vec<_>, _>>()?
                .join(", ");
            format!("{}({args})", id.name)
        }
        C::Not(_, c) => format!("!{}", condition(c)?),
        C::Add(_, l, r) => cond_binary(l, "+", r)?,
        C::Subtract(_, l, r) => cond_binary(l, "-", r)?,
        C::And(_, l, r) => cond_binary(l, "&", r)?,
        C::Or(_, l, r) => cond_binary(l, "|", r)?,
        C::Less(_, l, r) => cond_binary(l, "<", r)?,
        C::More(_, l, r) => cond_binary(l, ">", r)?,
        C::LessEqual(_, l, r) => cond_binary(l, "<=", r)?,
        C::MoreEqual(_, l, r) => cond_binary(l, ">=", r)?,
        // `=` в условии - Равенство.
        C::Equal(_, l, r) => cond_binary(l, "=", r)?,
        C::NotEqual(_, l, r) => cond_binary(l, "!=", r)?,
    })
}

fn cond_binary(
    left: &ast::Condition,
    op: &str,
    right: &ast::Condition,
) -> Result<String, FormatError> {
    Ok(format!("{} {op} {}", condition(left)?, condition(right)?))
}

/// Печатает тип.
pub(crate) fn ty(t: &ast::Type) -> Result<String, FormatError> {
    use ast::Type as T;
    Ok(match t {
        T::Bit => "bit".to_string(),
        T::Bool => "bool".to_string(),
        T::Rational => "float".to_string(),
        T::Duration => "duration".to_string(),
        // Fixed-point q(m, n): печатаем как объявлено, канон - с пробелом после запятой
        // (как в аргументах). Модификатор `sat` печатается через пробел - иначе
        // форматтер потерял бы семантику переполнения, а это не оформление, а смысл
        // программы.
        T::Fixed(_, ctor, m, n, modifier) => match modifier {
            Some(word) => format!("{ctor}({m}, {n}) {word}"),
            None => format!("{ctor}({m}, {n})"),
        },
        T::Unit => "()".to_string(),
        T::Alias(id) => id.name.clone(),
        T::Enum(name) | T::Struct(name) => name.clone(),
        T::Address { address, bit, .. } => match bit {
            Some(bit) => format!("0x{address:X}:{bit}"),
            None => format!("0x{address:X}"),
        },
        T::Array {
            element_count,
            element_type,
            ..
        } => format!("[{}; {element_count}]", ty(element_type)?),
        // Позиции у `Type::Function` в АСД нет, и взять её неоткуда: узел грамматикой
        // **не строится** (мёртвый узел, класс ). Появится правило - вместе с ним
        // появится и `loc`, который сюда протянут.
        T::Function { .. } => {
            return Err(super::unsupported(
                crate::diagnostics::Location::Implicit,
                "тип-функция",
            ));
        }
    })
}

/// Печатает объявление переменной/порта/константы (без завершающей `;`).
///
/// Тип опционален (`typ: Option<Type>`) - выводится семантикой; форматтер печатает то,
/// что написал автор, и ничего не додумывает.
pub(crate) fn variable_define(v: &ast::VariableDefine) -> Result<String, FormatError> {
    use ast::VariableDefine as V;
    Ok(match v {
        V::Variable {
            name,
            typ,
            initializer,
            ..
        } => with_init(head("var", name, typ.as_ref())?, initializer.as_ref())?,
        V::Constant {
            name,
            typ,
            initializer,
            ..
        } => with_init(head("const", name, typ.as_ref())?, Some(initializer))?,
        // Параметр модели: инициализатор обязателен грамматикой, печатается всегда -
        // как у `const`.
        V::Parameter {
            name,
            typ,
            initializer,
            ..
        } => with_init(head("parameter", name, typ.as_ref())?, Some(initializer))?,
        V::Port {
            name,
            typ,
            direction,
            address,
            initializer,
            ..
        } => {
            let keyword = match direction {
                ast::PortDirection::In => "in",
                ast::PortDirection::Out => "out",
                ast::PortDirection::InOut => "inout",
            };
            // Размещение `at <адрес>` печатается **между** типом и инициализатором - в
            // том же порядке, в каком стоит в исходнике. Поле необязательно: адрес
            // может приходить оператором `address` или внешней картой, и тогда его
            // здесь просто нет.
            //
            // Правило форматтера "добавил узел - добавь печать" защищает от новых
            // **узлов**, а не полей: новое поле компилятор не потребовал бы разобрать
            // (`..` в образце), и адрес молча пропал бы из вывода.
            let head = match address {
                Some(addr) => format!(
                    "{} at {}",
                    head(keyword, name, typ.as_ref())?,
                    expression(addr)?
                ),
                None => head(keyword, name, typ.as_ref())?,
            };
            with_init(head, initializer.as_ref())?
        }
    })
}

fn head(
    keyword: &str,
    name: &Option<ast::Identifier>,
    typ: Option<&ast::Type>,
) -> Result<String, FormatError> {
    Ok(match typ {
        Some(t) => format!("{keyword} {}: {}", ident(name), ty(t)?),
        None => format!("{keyword} {}", ident(name)),
    })
}

fn with_init(head: String, init: Option<&ast::Expression>) -> Result<String, FormatError> {
    Ok(match init {
        Some(init) => format!("{head} := {}", expression(init)?),
        None => head,
    })
}

/// Строковый литерал печатается в исходном виде, включая кавычки.
fn string_literal(parts: &[ast::StringLiteral]) -> String {
    parts.iter().map(one_string).collect::<Vec<_>>().join(" ")
}

/// Один строковый литерал в кавычках.
///
/// Отдельная точка нужна печати блока формул: там литерал стоит поодиночке - диалектом
/// заголовка и аргументом вызова. Своя копия кавычек разошлась бы с этой.
pub(crate) fn one_string(part: &ast::StringLiteral) -> String {
    format!("\"{}\"", part.string)
}

fn ident(name: &Option<ast::Identifier>) -> String {
    name.as_ref().map(|n| n.name.clone()).unwrap_or_default()
}

/// Печатает список параметров функции.
pub(crate) fn parameter_list(params: &ast::ParameterList) -> Result<String, FormatError> {
    params
        .iter()
        .filter_map(|(_, p)| p.as_ref())
        .map(|p| {
            // `Parameter::ty` - это Expression (тип как выражение), а не Type.
            let ty_text = expression(&p.ty)?;
            Ok(match &p.name {
                Some(name) => format!("{}: {ty_text}", name.name),
                None => ty_text,
            })
        })
        .collect::<Result<Vec<_>, FormatError>>()
        .map(|v| v.join(", "))
}

/// Печатает директиву импорта (с завершающей `;`).
pub(crate) fn import(i: &ast::ImportDefine) -> Result<String, FormatError> {
    Ok(match i {
        ast::ImportDefine::Plain(path, _) => format!("import {};", import_path(path)),
        ast::ImportDefine::GlobalSymbol(path, id, _) => {
            format!("import * as {} from {};", id.name, import_path(path))
        }
        ast::ImportDefine::Rename(path, names, _) => {
            let list = names
                .iter()
                .map(|(from, to)| match to {
                    Some(to) => format!("{} as {}", from.name, to.name),
                    None => from.name.clone(),
                })
                .collect::<Vec<_>>()
                .join(", ");
            format!("import {{{list}}} from {};", import_path(path))
        }
    })
}

fn import_path(p: &ast::ImportPath) -> String {
    match p {
        ast::ImportPath::Filename(s) => format!("\"{}\"", s.string),
        ast::ImportPath::Path(path) => path
            .identifiers
            .iter()
            .map(|id| id.name.clone())
            .collect::<Vec<_>>()
            .join("."),
    }
}

/// Печатает встроенную формулу (`: условия;` / `: [LTL] формулы;`).
///
/// # Форма автора сохраняется
///
/// `: conds;` и `: [Guard] conds;` - синонимы с одной семантикой, но АСД хранит признак
/// `explicit`, поэтому печатается **та форма, которую написал автор**. Канонизировать
/// синонимы форматтер не вправе: он меняет раскладку, а не текст программы.
pub(crate) fn inline_formula(f: &ast::InlineFormulaDefine) -> Result<String, FormatError> {
    use ast::InlineFormulaDefine as F;
    Ok(match f {
        F::Guard {
            conditions,
            explicit,
            ..
        } => {
            let list = conditions
                .iter()
                .map(condition)
                .collect::<Result<Vec<_>, _>>()?
                .join(", ");
            // Печатаем форму автора: `: [Guard] ...` и `: ...` - синонимы.
            if *explicit {
                format!(": [Guard] {list};")
            } else {
                format!(": {list};")
            }
        }
        F::Ltl { formulas, .. } => {
            let list = formulas
                .iter()
                .map(ltl)
                .collect::<Result<Vec<_>, _>>()?
                .join(", ");
            format!(": [LTL] {list};")
        }
    })
}

/// Позиция встроенной формулы - для привязки комментариев.
pub(crate) fn inline_formula_loc(f: &ast::InlineFormulaDefine) -> crate::diagnostics::Location {
    use ast::InlineFormulaDefine as F;
    match f {
        F::Guard { loc, .. } | F::Ltl { loc, .. } => *loc,
    }
}

/// Печатает LTL-формулу.
fn ltl(e: &ast::LtlExpr) -> Result<String, FormatError> {
    use ast::LtlExpr as L;
    Ok(match e {
        L::True(_) => "true".to_string(),
        L::False(_) => "false".to_string(),
        L::Atom(id) => id.name.clone(),
        L::Parenthesis(_, inner) => format!("({})", ltl(inner)?),
        L::Not(_, inner) => format!("!{}", ltl(inner)?),
        L::Next(_, inner) => format!("X {}", ltl(inner)?),
        L::Finally(_, inner) => format!("F {}", ltl(inner)?),
        L::Globally(_, inner) => format!("G {}", ltl(inner)?),
        L::And(_, l, r) => format!("{} & {}", ltl(l)?, ltl(r)?),
        L::Or(_, l, r) => format!("{} | {}", ltl(l)?, ltl(r)?),
        L::Until(_, l, r) => format!("{} U {}", ltl(l)?, ltl(r)?),
        L::Release(_, l, r) => format!("{} R {}", ltl(l)?, ltl(r)?),
        L::Implies(_, l, r) => format!("{} -> {}", ltl(l)?, ltl(r)?),
    })
}
