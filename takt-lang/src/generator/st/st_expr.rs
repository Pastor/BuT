//! Печать выражений и условий Takt в Structured Text (IEC 61131-3).
//!
//! (часть 1: выражения и условия; операторы, функции и `extern fn` - часть 2). Аналог
//! для цели `c` - `c_expr.rs`, 1736 строк, самый крупный файл C-генератора и кандидат на
//! дробление. Этот модуль обязан не повторить его судьбу: при выходе за ~1000 строк -
//! делить.
//!
//! ## Два печатника, а не один
//!
//! [`ConditionNode`] и [`ExpressionNode`] печатаются **раздельно** - это инвариант
//! языка: у них разная семантика `=` (в условии равенство, в выражении присваивание), и
//! **отверг** их слияние. Целевой синтаксис у обоих общий, но входные грамматики -
//! разные.
//!
//! ## Удачное совпадение с ST
//!
//! Takt и IEC 61131-3 используют **одни и те же** `:=` (присваивание) и `=`
//! (равенство), поэтому отображение почти тождественно - в отличие от цели `c`, которая
//! вынуждена печатать `==` (`stacker.c:146`).
//!
//! ## Факты MatIEC, определившие форму (пробы, 2026-07-15)
//!
//! Три нормы плана опровергнуты проверкой; форма ниже - следствие фактов:
//!
//! - **Побитовые операции не работают на целых.** `n AND m` при `n : USINT` ->
//!   `error: Data type mismatch for 'AND' expression`. В IEC `AND`/`OR`/`XOR`/`NOT`
//!   определены на **битовых строках** (`BYTE`/`WORD`/`DWORD`/`LWORD`), а не на
//!   числах. Поэтому побитовые операции идут через преобразование
//!   `USINT_TO_BYTE(...) AND USINT_TO_BYTE(...)` и обратно `BYTE_TO_USINT(...)`.
//! - **Битового доступа `x.0` нет вовсе.** Ни `n.0`, ни `w.0`, ни `w.%X0` MatIEC
//!   не принимает (`invalid expression after ':='`). Форма 3-й редакции ему
//!   неизвестна. Битовый доступ разворачивается в маску:
//!   `(USINT_TO_BYTE(x) AND 16#01) <> 16#00`.
//! - **Сдвигов-операторов нет.** `n << 1` - синтаксическая ошибка; `SHL`/`SHR` -
//!   функции, и тоже требуют битовой строки.
//!
//! Арифметика на битовых строках **запрещена** (`y + 1` при `y : BYTE` -> `Data type
//! mismatch for '+'`), поэтому переменные остаются числовыми (`USINT`), а
//! преобразование делается **в месте операции**, а не в объявлении.

// Тип операнда живёт своим модулем; имена доступны отсюда - их зовут печатники
// выражений, операторов и фиксированной точки.
use crate::generator::st::st_operand_type::variable_type;
pub(crate) use crate::generator::st::st_operand_type::{inner_expr_type, inner_expr_type_in};

use crate::diagnostics::{Diagnostic, Location};
use crate::generator::st::st_fixed;
use crate::parser::ast::Member;
use crate::semantic::type_node::TypeNode;
use crate::semantic::{ConditionNode, ExpressionNode, ModelNode, VariableNode};

/// Печатает выражение Takt в текст ST.
///
/// # Ошибки
/// `ST-011` - узел не имеет представления в ST (R4: никакого тихого пропуска).
pub(crate) fn print_expression(
    expr: &ExpressionNode,
    model: &ModelNode,
) -> Result<String, Diagnostic> {
    match expr {
        // Длительность печатается **миллисекундами** - целым, как и её тип (`UDINT`);
        // пересчёт зовёт общий слой.
        ExpressionNode::Duration(nanos) => Ok(crate::semantic::duration::value_millis(
            *nanos,
            Location::Codegen,
            "литерал длительности",
        )?
        .to_string()),
        ExpressionNode::Number(n) => Ok(n.to_string()),
        ExpressionNode::Bool(b) => Ok(bool_literal(*b)),
        ExpressionNode::Rational(text, negative) => {
            Ok(format!("{}{}", if *negative { "-" } else { "" }, text))
        }
        ExpressionNode::Variable(var) => Ok(variable_name(&var.borrow())),
        ExpressionNode::Parenthesis(inner) => Ok(format!("({})", print_expression(inner, model)?)),
        // Логические операции: в ST те же слова, что в IEC-условиях.
        ExpressionNode::Not(a) => Ok(format!("NOT {}", wrap_expr(a, model)?)),
        ExpressionNode::And(a, b) => binary(a, "AND", b, model),
        ExpressionNode::Or(a, b) => binary(a, "OR", b, model),
        // Арифметика: синтаксис совпадает, кроме остатка (`%` -> `MOD`). Над q(m, n) -
        // масштабирующая Q-арифметика через LINT-хелперы.
        ExpressionNode::Add(a, b) => fixed_binary(expr, st_fixed::FixedOp::Add, a, b, model)
            .unwrap_or_else(|| binary(a, "+", b, model)),
        ExpressionNode::Subtract(a, b) => {
            fixed_binary(expr, st_fixed::FixedOp::Subtract, a, b, model)
                .unwrap_or_else(|| binary(a, "-", b, model))
        }
        ExpressionNode::Multiply(a, b) => {
            fixed_binary(expr, st_fixed::FixedOp::Multiply, a, b, model)
                .unwrap_or_else(|| binary(a, "*", b, model))
        }
        ExpressionNode::Divide(a, b) => fixed_binary(expr, st_fixed::FixedOp::Divide, a, b, model)
            .unwrap_or_else(|| binary(a, "/", b, model)),
        ExpressionNode::Modulo(a, b) => binary(a, "MOD", b, model),
        // Степень разворачивается в умножения: оператор `**` в IEC определён над
        // Вещественным, и `iec2c` отвергал порождённый код ("Data type mismatch for
        // '**' expression") при нулевом коде возврата `taktc` - то есть цель молча
        // печатала невалидный ST.
        ExpressionNode::Power(a, b) => super::st_arith::power(a, b, model),
        ExpressionNode::UnaryPlus(a) => Ok(format!("+{}", wrap_expr(a, model)?)),
        ExpressionNode::Negate(a) => match st_fixed::fixed_format(expr, model) {
            Some((m, n, sat)) => st_fixed::negate(a, model, m, n, sat),
            None => Ok(format!("-{}", wrap_expr(a, model)?)),
        },
        // Сравнения: `!=` в ST записывается `<>`, остальные совпадают.
        ExpressionNode::Equal(a, b) => {
            crate::generator::st::st_sign::expr_compare(a, "=", b, model)
        }
        ExpressionNode::NotEqual(a, b) => {
            crate::generator::st::st_sign::expr_compare(a, "<>", b, model)
        }
        ExpressionNode::Less(a, b) => crate::generator::st::st_sign::expr_compare(a, "<", b, model),
        ExpressionNode::More(a, b) => crate::generator::st::st_sign::expr_compare(a, ">", b, model),
        ExpressionNode::LessEqual(a, b) => {
            crate::generator::st::st_sign::expr_compare(a, "<=", b, model)
        }
        ExpressionNode::MoreEqual(a, b) => {
            crate::generator::st::st_sign::expr_compare(a, ">=", b, model)
        }
        // Побитовые операции - только через битовую строку (см. шапку модуля).
        ExpressionNode::BitwiseAnd(a, b) => bitwise(a, "AND", b, model),
        ExpressionNode::BitwiseOr(a, b) => bitwise(a, "OR", b, model),
        ExpressionNode::BitwiseXor(a, b) => bitwise(a, "XOR", b, model),
        ExpressionNode::BitwiseNot(a) => {
            let bs = bit_string_of_expr(a, model)?;
            let inner = print_expression(a, model)?;
            Ok(format!("{}(NOT {}({}))", bs.from_fn, bs.to_fn, inner))
        }
        ExpressionNode::ShiftLeft(a, b) => shift(a, "SHL", b, model),
        ExpressionNode::ShiftRight(a, b) => shift(a, "SHR", b, model),
        ExpressionNode::BitAccess(inner, member) => bit_access(
            &|| print_expression(inner, model),
            inner_expr_type(inner),
            member,
            model,
        ),
        // База - выражение: печатается тем же печатником.
        //
        // Цепочка индексаций схлопывается В одну: в IEC 61131-3 массивы не
        // вкладываются, и `st_type` печатает `[[u8; 2]; 2]` многомерной формой `ARRAY
        // [0..1, 0..1] OF USINT` (T12 ). Индексация обязана следовать объявлению:
        // `grid[1, 0]`, а не форма C `grid[1][0]` - на второй `iec2c` отвечает "Number
        // of subscripts/indexes does not match ... (array has 0 indexes)".
        ExpressionNode::ArraySubscript(_, _) => {
            let (root, indices) = super::st_multidim::expression_subscript_chain(expr, model)?;
            Ok(format!("{}[{}]", root, indices.join(", ")))
        }
        // Присваивание - оператор ST, а не выражение; точку с запятой ставит вызывающий
        // (печатник операторов, часть 2 задачи).
        ExpressionNode::Assign(lhs, rhs) => Ok(format!(
            "{} := {}",
            print_expression(lhs, model)?,
            print_expression(rhs, model)?
        )),
        // Тернарный оператор Takt `c ? a : b` -> SEL(G, IN0, IN1): при G=FALSE берётся
        // IN0, при TRUE - IN1, поэтому ветви идут в обратном порядке.
        ExpressionNode::ConditionalOperator(cond, then_, else_) => Ok(format!(
            "SEL({}, {}, {})",
            print_expression(cond, model)?,
            print_expression(else_, model)?,
            print_expression(then_, model)?
        )),
        ExpressionNode::Cast(inner, ty) => {
            // Fixed-point: масштабирующее приведение, когда источник либо цель - q(m,
            // n); иначе обычный `cast`.
            if matches!(ty, TypeNode::Fixed { .. })
                || st_fixed::fixed_format(inner, model).is_some()
            {
                st_fixed::cast(inner, ty, model)
            } else {
                cast(inner, ty, model)
            }
        }
        // Узлы без представления в ST. Каждый назван поимённо - ветки `_` здесь нет:
        // `ExpressionNode` не помечен `#[non_exhaustive]`, поэтому новый вариант
        // Завалит сборку (гарантия ), а не проскочит молча.
        ExpressionNode::None => Err(unsupported("пустое выражение")),
        ExpressionNode::Unresolved(_) => Err(unsupported(
            "выражение не прошло семантическое понижение (Unresolved)",
        )),
        ExpressionNode::ArraySlice(_, _, _) => Err(unsupported(
            "срез массива: в IEC 61131-3 нет операции среза",
        )),
        // Вызов функции печатает `st_func` (часть 3): у беспараметрических функций есть
        // синтетический параметр, и аргумент к нему добавляется там.
        ExpressionNode::Function(def, args) => super::st_func::print_call(def, args, model),
        ExpressionNode::CodeBlock(_, _) => {
            Err(unsupported("блок кода как выражение не выразим в ST"))
        }
        ExpressionNode::NamedFunctionBox(_, _) => Err(unsupported(
            "вызов с именованными аргументами не выразим в ST",
        )),
        ExpressionNode::String(_) => Err(unsupported(
            "строковый литерал: цель ST строк не поддерживает",
        )),
        ExpressionNode::Type(_) => Err(unsupported("тип как выражение")),
        // Ветвь **недостижима** из корректной программы: голый адресный литерал в
        // позиции значения отвергает семантика (`SY-008`) - адрес есть свойство
        // размещения, а не число. Отказ оставлен страховкой; прежний текст обещал "",
        // то есть работу, которой не будет.
        ExpressionNode::Address(_, _) => Err(unsupported(
            "адресный литерал в позиции значения: адрес есть свойство размещения \
             (`at`, оператор `address`, карта), а не величина",
        )),
        // Анонимное обращение: ячейка - размещённая глобальная переменная (`VAR_GLOBAL
        // ... AT %M...`), блок видит её через `VAR_EXTERNAL`, поэтому здесь печатается
        // **имя**.
        //
        // До печатника доходит только цель `st-at`: цель `st` (библиотека блоков,
        // локаций не знающая) отвергает такую модель целиком в точке входа генератора
        // (`st::generate`) - одной проверкой вместо флага, протянутого через все
        // печатники.
        ExpressionNode::AnonPort(access) => Ok(access.synthetic_name()),
        ExpressionNode::Model(_) => Err(unsupported("модель как выражение")),
        // Именованное условие печатается печатником условий. Прежний текст обещал
        // "часть 2 " - работу, которой нет.
        ExpressionNode::Condition(cond) => {
            crate::generator::st::st_cond::print_condition(&cond.borrow().value, model)
        }
        ExpressionNode::List(_) => Err(unsupported("список параметров как выражение")),
        // Агрегат в позиции значения. Присваивание агрегата печатается поэлементно
        // печатником операторов, а сюда доходит то, что значением быть не может:
        // возврат массива из функции (`return {1, 2};`). В IEC 61131-3 функция массива
        // не возвращает - причина в целевом языке, а не в недоделке.
        ExpressionNode::Array(_) | ExpressionNode::Initializer(_) => Err(unsupported(
            "агрегат в позиции значения: в IEC 61131-3 значения-массива нет — \
             присваивайте элементы по одному либо передавайте массив параметром \
             `VAR_IN_OUT`",
        )),
    }
}

/// Печатает значение по целевому типу присваивания.
///
/// Тела цели `st` печатали `cmd_fork := 0` и `command := 2`, тогда как объявления
/// канонично дают `FALSE` и объявляют константы `Command_Stop` - вывод противоречил сам
/// себе. Причина: `print_expression` целевого типа не видит. Здесь он известен, поэтому
/// литерал восстанавливается:
///
/// - `BOOL`/`bit` + `0`/`1` -> `FALSE`/`TRUE`;
/// - перечисление + число -> **имя константы** (`Command_Stop`), совпадающее с её
///   объявлением (`st_decl::enum_constants` печатает `{enum}_{variant}`).
///
/// Приём - тот же, что у целей `rust` и `sv` (`coerce_to` по целевому типу).
/// `print_expression` **не меняется**: она зовётся из всех узлов, и протаскивать тип
/// через каждый - цена, несоизмеримая с задачей.
///
/// **Догадываться нельзя** (): значение без соответствующего варианта печатается
/// **числом** - перечислимой переменной можно присвоить произвольное число, и подмена
/// его именем "похожего" варианта была бы тихой ложью.
pub(crate) fn coerce_to(
    value: &ExpressionNode,
    target: &TypeNode,
    model: &ModelNode,
) -> Result<String, Diagnostic> {
    match (target, value) {
        (TypeNode::Enum(enum_name), ExpressionNode::Number(n)) => {
            if let Some(def) = model.search_enum(enum_name)
                && let Some((variant, _)) = def.variants.iter().find(|(_, v)| v == n)
            {
                // Совпадает с именем константы из `st_decl::enum_constants`.
                return Ok(format!("{}_{}", enum_name, variant));
            }
            Ok(n.to_string())
        }
        (TypeNode::Bool | TypeNode::Bit, ExpressionNode::Number(n)) => match n {
            0 => Ok(bool_literal(false)),
            1 => Ok(bool_literal(true)),
            // Прочие числа для BOOL MatIEC и так отвергает (`b := 2` -> ошибка);
            // печатаем как есть, не выдумывая.
            _ => print_expression(value, model),
        },
        // Разряд `x.N` в позиции числового значения. Битового доступа в MatIEC нет
        // вовсе, поэтому печатник строит **булево** выражение над маской - верное в
        // условии и отвергаемое `iec2c` ("Incompatible data types for ':=' operation")
        // в присваивании числу, при нулевом коде возврата `taktc`.
        (_, ExpressionNode::BitAccess(_, Member::Number(_)))
            if !matches!(target, TypeNode::Bool | TypeNode::Bit) =>
        {
            let printed = print_expression(value, model)?;
            match crate::generator::st::st_type::get_st_type(target, model) {
                // Форма проверена пробой `iec2c` 2026-08-20: стандартная функция
                // преобразования принимается для любого целого типа.
                Ok(name) => Ok(format!("BOOL_TO_{name}({printed})")),
                // Тип, который цель печатать не умеет, уже отвергнут своим отказом там,
                // где объявляется приёмник; здесь ничего не выдумываем.
                Err(_) => Ok(printed),
            }
        }
        // Арифметика печатается В типе приёмника: `r := a + b;` при `a, b: u8` и `r:
        // u16` давало отказ `iec2c` ("Incompatible data types for ':=' operation") при
        // нулевом коде возврата `taktc`, тогда как эталон и цель `c` вход считают.
        //
        // Приводятся операнды, а не результат: сложение в `USINT` обернулось бы по
        // модулю 256 **до** расширения - 300 стало бы 44.
        (
            TypeNode::Integer { .. },
            ExpressionNode::Add(l, r)
            | ExpressionNode::Subtract(l, r)
            | ExpressionNode::Multiply(l, r)
            | ExpressionNode::Divide(l, r)
            | ExpressionNode::Modulo(l, r),
        ) if crate::generator::st::st_sign::operands_need_cast(l, r, target) => {
            crate::generator::st::st_sign::arith_in_target(value, target, model)
        }
        // Именованное значение иного целого типа приводится к приёмнику: `iec2c`
        // отвергает присваивание разных типов.
        (TypeNode::Integer { .. }, ExpressionNode::Variable(_))
            if crate::generator::mixed_sign::operand_type_expr(value)
                .is_some_and(|ty| matches!(ty, TypeNode::Integer { .. }) && ty != *target) =>
        {
            crate::generator::st::st_sign::value_in_target(value, target, model)
        }
        // Явное приведение автора приёмника не отменяет: `probe := wide as u32;` при
        // `out probe: u8` печаталось `UINT_TO_UDINT(wide)` в приёмник `USINT`, и
        // `iec2c` отвечал "Incompatible data types for ':=' operation" при нулевом коде
        // возврата `taktc`. Эталон запись исполняет: приведение автора даёт
        // промежуточное значение, присваивание усекает его по приёмнику.
        (TypeNode::Integer { bits, signed }, ExpressionNode::Cast(_, cast_ty))
            if matches!(cast_ty, TypeNode::Integer { .. }) && *cast_ty != *target =>
        {
            let printed = print_expression(value, model)?;
            let TypeNode::Integer {
                bits: from_bits,
                signed: from_signed,
            } = cast_ty
            else {
                return Ok(printed);
            };
            match (
                crate::generator::st::st_type::iec_integer_name(*bits, *signed),
                crate::generator::st::st_type::iec_integer_name(*from_bits, *from_signed),
            ) {
                (Some(to), Some(from)) => Ok(format!("{from}_TO_{to}({printed})")),
                // Тип, который цель не печатает, отвергается своим отказом там, где
                // объявляется приёмник; здесь ничего не выдумываем.
                _ => Ok(printed),
            }
        }
        _ => print_expression(value, model),
    }
}

/// Тип переменной, которой присваивают (для [`coerce_to`]).
pub(crate) fn assign_target_type(var: &VariableNode) -> Option<TypeNode> {
    variable_type(var)
}

/// Имя переменной для печати присваивания (реэкспорт для `st_stmt`).
pub(crate) fn variable_ident(var: &VariableNode) -> String {
    variable_name(var)
}

/// префикса - автор искал место сам.
pub(crate) fn unsupported(what: &str) -> Diagnostic {
    Diagnostic::error(
        crate::generator::site::at(Location::Codegen),
        format!("Не транслируется в Structured Text: {}", what),
    )
    .with_code("ST-011")
}

/// Литерал `BOOL`.
///
/// MatIEC принимает и числовые `0`/`1` для `BOOL` (проверено пробой - вопреки ожиданию
/// плана), но `2` уже отвергает. Печатаем всегда `FALSE`/`TRUE`: это стандартная форма
/// и она читается однозначно.
pub(super) fn bool_literal(value: bool) -> String {
    if value { "TRUE" } else { "FALSE" }.to_string()
}

/// Возвращает имя переменной для ST.
///
/// В ST порт - **обычная переменная**, поэтому слой косвенности цели `c`
/// (`(*main->read_numeric)(...)`) здесь исчезает: печатается просто имя.
pub(super) fn variable_name(var: &VariableNode) -> String {
    match var {
        VariableNode::Simple { name, .. }
        | VariableNode::Port { name, .. }
        | VariableNode::Const { name, .. } => name.clone(),
        VariableNode::Unresolved => "(*неразрешённая переменная*)".to_string(),
    }
}

/// Печатает бинарную операцию выражения, скобкуя составные операнды.
///
/// **Скобки обязательны, а не косметика.** Приоритеты Takt и ST не совпадают:
/// `!(a = b)` Takt при наивной печати даёт `NOT a = b`, а в ST `NOT` связывает
/// сильнее `=`, то есть читается как `(NOT a) = b` - другое выражение. Проверка
/// поймал это на `elevator` ("Invalid data type for 'NOT' expression"), но
/// страшнее случай, когда типы совпадут и разница пройдёт **молча**.
pub(super) fn binary(
    a: &ExpressionNode,
    op: &str,
    b: &ExpressionNode,
    model: &ModelNode,
) -> Result<String, Diagnostic> {
    Ok(format!(
        "{} {} {}",
        wrap_expr(a, model)?,
        op,
        wrap_expr(b, model)?
    ))
}

/// Q-путь бинарной операции: `Some` тогда и только тогда, когда `expr` имеет тип `q(m,
/// n)` - иначе вызывающий печатает обычную арифметику.
pub(super) fn fixed_binary(
    expr: &ExpressionNode,
    op: st_fixed::FixedOp,
    a: &ExpressionNode,
    b: &ExpressionNode,
    model: &ModelNode,
) -> Option<Result<String, Diagnostic>> {
    st_fixed::fixed_format(expr, model)
        .map(|(m, n, sat)| st_fixed::binary(op, a, b, model, m, n, sat))
}

/// Печатает операнд, заключая составное выражение в скобки.
pub(super) fn wrap_expr(expr: &ExpressionNode, model: &ModelNode) -> Result<String, Diagnostic> {
    let text = print_expression(expr, model)?;
    Ok(if is_atom_expr(expr) {
        text
    } else {
        format!("({})", text)
    })
}

/// Атом - то, чей разбор не зависит от окружения: литерал, имя, вызов, скобки.
pub(super) fn is_atom_expr(expr: &ExpressionNode) -> bool {
    matches!(
        expr,
        ExpressionNode::Number(_)
            | ExpressionNode::Bool(_)
            | ExpressionNode::Rational(_, _)
            | ExpressionNode::Variable(_)
            | ExpressionNode::Parenthesis(_)
            | ExpressionNode::Function(_, _)
            | ExpressionNode::ArraySubscript(_, _)
    )
}

/// Печатает бинарную операцию условия, скобкуя составные операнды (см.
pub(super) fn binary_cond(
    a: &ConditionNode,
    op: &str,
    b: &ConditionNode,
    model: &ModelNode,
) -> Result<String, Diagnostic> {
    Ok(format!(
        "{} {} {}",
        wrap_cond(a, model)?,
        op,
        wrap_cond(b, model)?
    ))
}

/// Печатает операнд-условие, заключая составное в скобки.
pub(super) fn wrap_cond(cond: &ConditionNode, model: &ModelNode) -> Result<String, Diagnostic> {
    let text = crate::generator::st::st_cond::print_condition(cond, model)?;
    Ok(if is_atom_cond(cond) {
        text
    } else {
        format!("({})", text)
    })
}

/// Атом-условие: литерал, имя, вызов, скобки, вариант перечисления.
pub(super) fn is_atom_cond(cond: &ConditionNode) -> bool {
    matches!(
        cond,
        ConditionNode::Number(_)
            | ConditionNode::Bool(_)
            | ConditionNode::Rational(_, _)
            | ConditionNode::Variable(_, _)
            | ConditionNode::Parenthesis(_)
            | ConditionNode::Function(_, _, _)
            | ConditionNode::ArraySubscript(_, _)
            | ConditionNode::EnumVariant(_, _, _)
    )
}

/// Битовая строка, соответствующая целому типу: имя и функции преобразования.
///
/// Имена проверены пробой на всех восьми целых типах (`USINT`...`LINT`).
pub(crate) struct BitString {
    /// Функция "целое -> битовая строка" (например, `USINT_TO_BYTE`).
    pub(crate) to_fn: String,
    /// Функция "битовая строка -> целое" (например, `BYTE_TO_USINT`).
    pub(crate) from_fn: String,
    /// Число шестнадцатеричных цифр в литерале маски (2 для `BYTE`, 4 для `WORD`...).
    pub(crate) hex_digits: usize,
    /// Разрядность типа.
    pub(crate) bits: u8,
}

/// Подбирает битовую строку для целого типа Takt.
pub(crate) fn bit_string_of_type(ty: &TypeNode) -> Option<BitString> {
    // Бит-вектор `[bit;N <= 64]` - упакованный скаляр, по представлению равный `uN`;
    // признак берётся у того же слоя, каким цель печатает сам тип.
    let normalized;
    let ty = match crate::semantic::bit_vector::is_bit_vector(ty)
        .map(crate::semantic::bit_vector::layout)
    {
        Some(crate::semantic::bit_vector::BitVectorLayout::Scalar { width }) => {
            normalized = TypeNode::Integer {
                bits: u8::try_from(width).unwrap_or(64),
                signed: false,
            };
            &normalized
        }
        // N > 64 - массив слов: битовой строкой IEC он не является.
        _ => ty,
    };
    let TypeNode::Integer { bits, signed } = ty else {
        return None;
    };
    let (bs, int_name, digits) = match bits {
        8 => ("BYTE", if *signed { "SINT" } else { "USINT" }, 2),
        16 => ("WORD", if *signed { "INT" } else { "UINT" }, 4),
        32 => ("DWORD", if *signed { "DINT" } else { "UDINT" }, 8),
        64 => ("LWORD", if *signed { "LINT" } else { "ULINT" }, 16),
        _ => return None,
    };
    Some(BitString {
        to_fn: format!("{}_TO_{}", int_name, bs),
        from_fn: format!("{}_TO_{}", bs, int_name),
        hex_digits: digits,
        bits: *bits,
    })
}

/// Подбирает битовую строку для операнда выражения.
pub(super) fn bit_string_of_expr(
    expr: &ExpressionNode,
    _model: &ModelNode,
) -> Result<BitString, Diagnostic> {
    inner_expr_type(expr)
        .as_ref()
        .and_then(bit_string_of_type)
        .ok_or_else(|| {
            unsupported(
                "побитовая операция над операндом, чей целый тип не определяется \
                 статически: в IEC 61131-3 такие операции требуют битовой строки \
                 (BYTE/WORD/DWORD/LWORD), и разрядность обязана быть известна",
            )
        })
}

/// Печатает побитовую операцию через преобразование в битовую строку.
///
/// `n & m` -> `BYTE_TO_USINT(USINT_TO_BYTE(n) AND USINT_TO_BYTE(m))`: прямое `n AND m`
/// MatIEC отвергает ("Data type mismatch for 'AND' expression").
pub(super) fn bitwise(
    a: &ExpressionNode,
    op: &str,
    b: &ExpressionNode,
    model: &ModelNode,
) -> Result<String, Diagnostic> {
    let bs = bit_string_of_expr(a, model)?;
    Ok(format!(
        "{}({}({}) {} {}({}))",
        bs.from_fn,
        bs.to_fn,
        print_expression(a, model)?,
        op,
        bs.to_fn,
        print_expression(b, model)?
    ))
}

/// Печатает сдвиг через `SHL`/`SHR` над битовой строкой.
///
/// Оператора `<<` в ST **нет** (синтаксическая ошибка), а `SHL` на числовом типе
/// отвергается - нужна битовая строка.
fn shift(
    a: &ExpressionNode,
    func: &str,
    b: &ExpressionNode,
    model: &ModelNode,
) -> Result<String, Diagnostic> {
    // Сдвиг вправо знакового обязан быть арифметическим: `SHR`
    // работает над битовой строкой, то есть логически, и `-8 >> 1` давал
    // **124** вместо −4 - молча, при том что эталон, `c` и `rust` дают −4.
    if func == "SHR" && is_signed_expr(a) {
        return super::st_arith::arithmetic_shift_right(a, b, model);
    }
    let bs = bit_string_of_expr(a, model)?;
    Ok(format!(
        "{}({}({}({}), {}))",
        bs.from_fn,
        func,
        bs.to_fn,
        print_expression(a, model)?,
        print_expression(b, model)?
    ))
}

/// Знаково ли выражение - по объявленному типу либо явному приведению.
///
/// Признак осторожен: не узнав знака, отвечает `false`, и печатается прежняя форма
/// через битовую строку. Ошибка - в сторону прежнего поведения.
fn is_signed_expr(expr: &ExpressionNode) -> bool {
    matches!(
        inner_expr_type(expr),
        Some(TypeNode::Integer { signed: true, .. })
    )
}

/// Печатает доступ к члену: бит числа либо поле структуры.
///
/// Битовый доступ разворачивается в маску - формы `x.0` в MatIEC **нет вовсе** (ни
/// `n.0`, ни `w.0`, ни `w.%X0`): `sensors_cab.0` -> `(USINT_TO_BYTE(sensors_cab) AND
/// 16#01) <> 16#00`.
pub(super) fn bit_access(
    print_inner: &dyn Fn() -> Result<String, Diagnostic>,
    inner_ty: Option<TypeNode>,
    member: &Member,
    _model: &ModelNode,
) -> Result<String, Diagnostic> {
    match member {
        // Поле структуры: синтаксис ST совпадает с Takt.
        Member::Identifier(id) => Ok(format!("{}.{}", print_inner()?, id.name)),
        Member::Number(n) => {
            let inner = print_inner()?;
            let ty = inner_ty.ok_or_else(|| {
                unsupported(
                    "битовый доступ к операнду, чей тип не определяется статически: \
                     разрядность нужна, чтобы построить маску",
                )
            })?;
            // Бит 0 булева значения - оно само; иных битов у BOOL нет.
            if matches!(ty, TypeNode::Bit | TypeNode::Bool) {
                return if *n == 0 {
                    Ok(inner)
                } else {
                    Err(unsupported(&format!(
                        "бит {} у однобитного значения: в IEC 61131-3 у BOOL нет битов, \
                         кроме нулевого",
                        n
                    )))
                };
            }
            let bs = bit_string_of_type(&ty).ok_or_else(|| {
                unsupported(&format!(
                    "битовый доступ к типу '{}': маска строится только для целых \
                     типов IEC (8/16/32/64 бита)",
                    ty
                ))
            })?;
            if *n < 0 || *n >= i128::from(bs.bits) {
                return Err(unsupported(&format!(
                    "бит {} вне разрядности типа '{}' ({} бит)",
                    n, ty, bs.bits
                )));
            }
            let mask = 1u128 << n;
            Ok(format!(
                "({}({}) AND 16#{:0width$X}) <> 16#{:0width$X}",
                bs.to_fn,
                inner,
                mask,
                0,
                width = bs.hex_digits
            ))
        }
    }
}

/// Печатает приведение типа через функцию преобразования IEC (`<ИЗ>_TO_<В>`).
fn cast(inner: &ExpressionNode, ty: &TypeNode, model: &ModelNode) -> Result<String, Diagnostic> {
    let to = super::st_type::get_st_type(ty, model)?;
    let from_ty = inner_expr_type_in(inner, model).ok_or_else(|| {
        unsupported(
            "приведение операнда, чей тип не определяется статически: имя функции \
             преобразования IEC строится из ОБОИХ типов (<ИЗ>_TO_<В>)",
        )
    })?;
    let from = super::st_type::get_st_type(&from_ty, model)?;
    if from == to {
        return print_expression(inner, model);
    }
    Ok(format!(
        "{}_TO_{}({})",
        from,
        to,
        print_expression(inner, model)?
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic::tree::construct_model;
    use std::cell::RefCell;
    use std::rc::Rc;

    /// Строит модель из исходника Takt.
    fn model_of(src: &str) -> Rc<RefCell<ModelNode>> {
        let (ast, _) = crate::parse(src, 0).unwrap();
        construct_model(&ast, None, &[]).unwrap()
    }

    /// Печатает условие `cond C = <текст>;` из модели.
    fn cond_of(src_cond: &str) -> String {
        let src = format!(
            "var n: u8 := 0;\nvar m: u8 := 0;\nvar b: bit := 0;\n\
             cond C = {};\nstart S {{ ref D: C; }}\nstate D {{}}",
            src_cond
        );
        let rc = model_of(&src);
        let model = rc.borrow();
        let node = model.conditions.get("C").expect("нет условия C").clone();
        let value = node.value.clone();
        crate::generator::st::st_cond::print_condition(&value, &model)
            .expect("условие должно печататься")
    }

    /// Печатает выражение, записанное **в теле блока**.
    ///
    /// Площадка перенесена из инициализатора переменной: инициализаторы сворачиваются в
    /// литерал на семантике, и выражения там больше нет - `var x: u8 := n + m * 2;`
    /// печаталось бы как `0`. В теле блока выражение живёт как прежде, поэтому печать
    /// проверяется здесь.
    ///
    /// Вход прежний - объявление вида `var x: <тип> := <выражение>;`: его правая часть
    /// и становится телом.
    fn expr_of(decl: &str) -> String {
        let expr = decl
            .split_once(":=")
            .map(|(_, rhs)| rhs.trim().trim_end_matches(';').trim())
            .unwrap_or(decl);
        let src = format!(
            "var n: u8 := 0;\nvar m: u8 := 0;\nvar b: bit := 0;\nvar x: u8 := 0;\n\
             start S {{ always {{ x := {expr}; }} }}"
        );
        let rc = model_of(&src);
        let model = rc.borrow();
        let state = model.states.values().next().expect("нет состояния");
        let crate::semantic::StateNode::Simple { named_blocks, .. } = state else {
            panic!("ожидалось простое состояние");
        };
        let rhs = named_blocks
            .iter()
            .find_map(|b| match b {
                crate::semantic::NamedCodeBlockDefinitionNode::Always { body, .. } => {
                    assignment_rhs(body)
                }
                _ => None,
            })
            .expect("в теле нет присваивания");
        print_expression(&rhs, &model).expect("выражение должно печататься")
    }

    /// Правая часть первого присваивания в теле блока.
    fn assignment_rhs(stmt: &crate::semantic::StatementNode) -> Option<ExpressionNode> {
        use crate::semantic::StatementNode as S;
        match stmt {
            S::Block(items) => items.iter().find_map(assignment_rhs),
            S::Expression(expr, _) => match &**expr {
                ExpressionNode::Assign(_, rhs) => Some((**rhs).clone()),
                _ => None,
            },
            _ => None,
        }
    }

    /// `=` в условии - равенство. Цель `c` печатает здесь `==` (`stacker.c:146`).
    #[test]
    fn test_condition_equal_prints_single_equals_not_double() {
        assert_eq!(cond_of("n = m"), "n = m");
    }

    /// `&`/`|`/`!` над булевыми - словами IEC.
    ///
    /// Сверка с `stacker.c:83`: `lift_request && !(lift_op) && ...` -> в ST
    /// `lift_request AND NOT lift_op AND ...`.
    #[test]
    fn test_condition_logical_operators_use_iec_words() {
        assert_eq!(cond_of("b & !b"), "b AND (NOT b)");
        assert_eq!(cond_of("b | !b"), "b OR (NOT b)");
    }

    /// Составные операнды скобкуются: приоритеты Takt и ST не совпадают.
    ///
    /// Тест против регресса, который поймал проверка, а юнит-тесты - нет: `!(a = b)` при
    /// наивной печати даёт `NOT a = b`, а в ST `NOT` связывает сильнее `=` - читается
    /// как `(NOT a) = b`, другое выражение. На `elevator` это дало "Invalid data type
    /// for 'NOT' expression", но опаснее случай, где типы совпадут и подмена пройдёт
    /// **молча**.
    #[test]
    fn test_not_of_comparison_is_parenthesised_because_iec_binds_not_tighter() {
        assert_eq!(cond_of("!(n = m)"), "NOT (n = m)");
    }

    /// `!=` в ST записывается `<>`.
    #[test]
    fn test_condition_not_equal_is_angle_brackets() {
        assert_eq!(cond_of("n != m"), "n <> m");
    }

    /// Реляционные операторы совпадают с Takt.
    #[test]
    fn test_condition_relational_operators_match_lam() {
        assert_eq!(cond_of("n < m"), "n < m");
        assert_eq!(cond_of("n <= m"), "n <= m");
        assert_eq!(cond_of("n > m"), "n > m");
        assert_eq!(cond_of("n >= m"), "n >= m");
    }

    /// Битовый доступ разворачивается в маску: формы `x.0` в MatIEC нет вовсе.
    ///
    /// Форма проверена пробой: `(USINT_TO_BYTE(n) AND 16#01) <> 16#00` - код 0.
    #[test]
    fn test_condition_bit_access_expands_to_mask() {
        assert_eq!(
            cond_of("n.0"),
            "(USINT_TO_BYTE(n) AND 16#01) <> 16#00",
            "битового доступа x.0 в IEC 61131-3 (диалект MatIEC) нет"
        );
    }

    /// Маска старшего бита учитывает номер бита, а не только тип.
    #[test]
    fn test_condition_bit_access_uses_correct_mask_for_high_bit() {
        assert_eq!(cond_of("n.7"), "(USINT_TO_BYTE(n) AND 16#80) <> 16#00");
    }

    /// Бит 0 однобитного значения - само значение; маска ему не нужна.
    #[test]
    fn test_condition_bit_zero_of_bool_is_the_value_itself() {
        assert_eq!(cond_of("b.0"), "b");
    }

    /// Остаток от деления в ST - `MOD`, а не `%`.
    #[test]
    fn test_expression_modulo_is_mod_keyword() {
        assert_eq!(expr_of("var x: u8 := n % m;"), "n MOD m");
    }

    /// Арифметика и сравнения переносятся тождественно.
    #[test]
    fn test_expression_arithmetic_is_identical() {
        assert_eq!(expr_of("var x: u8 := n + m * 2;"), "n + (m * 2)");
    }

    /// Побитовое И идёт через битовую строку: `n AND m` на USINT MatIEC отвергает.
    ///
    /// Форма проверена пробой: `BYTE_TO_USINT(USINT_TO_BYTE(n) AND USINT_TO_BYTE(m))`.
    #[test]
    fn test_expression_bitwise_and_goes_through_bit_string() {
        assert_eq!(
            expr_of("var x: u8 := n & m;"),
            "BYTE_TO_USINT(USINT_TO_BYTE(n) AND USINT_TO_BYTE(m))",
            "побитовые операции в IEC определены на битовых строках, не на числах"
        );
    }

    /// Сдвиг - функция `SHL` над битовой строкой: оператора `<<` в ST нет.
    #[test]
    fn test_expression_shift_left_is_shl_over_bit_string() {
        assert_eq!(
            expr_of("var x: u8 := n << 1;"),
            "BYTE_TO_USINT(SHL(USINT_TO_BYTE(n), 1))"
        );
    }

    /// Булев литерал печатается словом, а не числом.
    #[test]
    fn test_expression_bool_literal_is_keyword() {
        assert_eq!(expr_of("var x: bit := true;"), "TRUE");
    }

    /// Непечатаемый узел даёт `ST-011`, а не тихий пропуск (R4).
    #[test]
    fn test_unsupported_node_is_st011_error() {
        let rc = model_of("var n: u8 := 0;\nstart S { always { n := n; } }");
        let model = rc.borrow();
        let err = print_expression(&ExpressionNode::None, &model)
            .expect_err("пустое выражение обязано отвергаться");
        assert_eq!(err.code.as_deref(), Some("ST-011"));
    }

    /// Строковый литерал не транслируется - с кодом, а не молча.
    #[test]
    fn test_string_literal_is_st011_error() {
        let rc = model_of("var n: u8 := 0;\nstart S { always { n := n; } }");
        let model = rc.borrow();
        let err = print_expression(&ExpressionNode::String(vec!["s".into()]), &model)
            .expect_err("строка обязана отвергаться");
        assert_eq!(err.code.as_deref(), Some("ST-011"));
    }
}
