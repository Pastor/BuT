//! Выражения и условия в SystemVerilog.
//!
//! ## Две грамматики, а не одна
//!
//! `ConditionNode` (условие ребра `ref S: cond;`) и `ExpressionNode` (тело блока) -
//! **две разные грамматики с разной семантикой `=`**: в условии это равенство, в
//! выражении - присваивание. Поэтому здесь два печатающих пути, как и в цели `c`.
//!
//! ## Скобки: печатается дерево, а не текст
//!
//! Каждый бинарный узел заключается в скобки. Это не стиль: приоритеты исходного
//! `.takt` уже разобраны парсером, и печать плоского текста полагалась бы на то, что
//! приоритеты SV совпадают с приоритетами Takt. У SystemVerilog они совпадают с C (`==`
//! связывает сильнее `|`), но полагаться на это - ровно та ошибка, которая в цели
//! `rust` дала **молчащий** дефект: там `a == b | c` при `a=2, b=2, c=1` даёт в C `1`,
//! а в Rust `false` (`CLAUDE.md`).
//!
//! ## `&`/`|` условий - Логические, потому что так у эталона
//!
//! оставлял открытым вопрос "`&&` или `&` - решать по типу операнда". Вопрос закрыт
//! чтением эталона: цель `c` печатает `ConditionNode::And` как `&&`, а `Or` - как `||`
//! **безусловно** (`c_expr.rs:656`), независимо от ширины операнда. Сверка модели
//! ведётся против C (`conformance_c_tests.rs`), поэтому расхождение с ним и было бы
//! дефектом: на многобитном операнде `a & b` и `a && b` дают разное. Совпадение с C
//! здесь достигается даром - в SV `&&` над `logic [7:0]` тоже даёт один бит 0/1.
//!
//! ## Литералы печатаются без явной ширины
//!
//! требовал ширину у каждого литерала (`8'd7`), обосновывая это тем, что иначе
//! `verilator -Wall` даёт `WIDTHEXPAND`. **Проба 2026-07-16 это опровергла:** модуль с
//! голыми `7`, `0`, `1` в сравнениях и присваиваниях `verilator --lint-only -Wall`
//! принимает **чисто, код 0**. Десятичный литерал в SV самоопределяем и подстраивается
//! под контекст.
//!
//! Граница найдена той же пробой: `x_next = 300;` при `x : logic [7:0]` даёт
//! `%Warning-WIDTHTRUNC` - но это **дефект модели** (значение не влезает в объявленный
//! тип), а не форматирования, и краснеющий на нём проверка прав.
//!
//! Следствие крупное: печатнику **не нужен вывод типа в каждом узле**. У цели `st` он
//! нужен (`ST-011`: без типа операнда не построить имя функции преобразования -
//! `BYTE_TO_USINT(...)`), и это делает её трансляцию типозависимой. Здесь - нет.

// Контекст печати живёт своим модулем; имя доступно отсюда - им пользуются семь модулей
// цели.
pub(crate) use crate::generator::sv::sv_scope::Scope;

use crate::diagnostics::{Diagnostic, Location};
use crate::generator::sv::sv_names::{print_member, signal_of_in, sv_enum_variant_name};
use crate::generator::sv::sv_state_of;
use crate::semantic::type_node::TypeNode;
use crate::semantic::{ConditionNode, ExpressionNode};

/// Строит диагностику `SV-002` - узел АСД не покрыт печатью.
pub(crate) fn sv002(what: &str) -> Diagnostic {
    Diagnostic::error(
        crate::generator::site::at(Location::Codegen),
        format!("Не транслируется в SystemVerilog: {what}"),
    )
    .with_code("SV-002")
    .with_note(
        Location::Codegen,
        "молчаливо пропустить конструкцию нельзя: порождённый модуль вёл бы \
         себя иначе, чем модель"
            .to_string(),
    )
}

/// Строит диагностику `SV-005` - `extern fn` в синтезируемом RTL невыразима.
pub(in crate::generator::sv) fn sv005(name: &str, loc: Location) -> Diagnostic {
    Diagnostic::error(
        loc,
        format!(
            "внешняя функция '{}' (extern fn) целью 'sv' не поддерживается: в \
             синтезируемом RTL вызова внешнего кода не существует. DPI-C — \
             механизм симуляции, синтезатору он недоступен; аппаратный аналог \
             «внешней функции» — отдельный модуль со своим интерфейсом, а \
             extern fn языка Takt не описывает ни портов, ни тактов, ни \
             латентности, и вывести их нельзя. Используйте цель 'c'/'st'/'rust' \
             либо выразите логику на Takt",
            name
        ),
    )
    .with_code("SV-005")
}

/// Строит предупреждение `SV-009` - переменный делитель.
///
/// **Предупреждение, а не ошибка**: деление синтезируется
/// корректно, и отказывать в трансляции рабочего кода цель не вправе.
///
/// Текст называет **причину** (в RTL нет аппаратного делителя) и **следствие** (длинный
/// комбинационный путь -> потолок частоты), но **без** конкретных LUT: они технологичны
/// (`synth_ice40` != `synth_xilinx`) и устареют (action A-1). Про `*` умолчание
/// обосновано замером (DSP, дешевле сложения), про делитель-константу - тоже (17...106
/// LUT); срабатывает лишь переменный.
fn sv009() -> Diagnostic {
    Diagnostic::warning(
        crate::generator::site::at(Location::Codegen),
        "деление или остаток по переменному делителю: в синтезируемом RTL нет \
         аппаратного делителя, поэтому '/' и '%' разворачиваются в крупный \
         комбинационный блок (на порядок больше сложения) с длинным путём — это \
         снижает достижимый потолок тактовой частоты. Делитель-константа так не \
         стоит (сворачивается в сдвиги/умножение на обратное); если делитель по \
         существу постоянен, вынесите его в константу"
            .to_string(),
    )
    .with_code("SV-009")
}

/// Делитель - константа времени компиляции (числовой литерал)?
///
/// -3: константный делитель молчит (замер: `a / 2` -> 17 LUT, `a / 3` -> 106 LUT -
/// дёшево), переменный -> `SV-009` (558 LUT, ноль DSP). Проверка **синтаксическая** и
/// **консервативная** (action A-2): `b := 3; a / b` предупредит, хотя синтезатор
/// свернул бы, - лишнее предупреждение честнее пропущенного. Скобки снимаются: `a /
/// (2)` - та же константа.
fn is_constant_divisor(node: &ExpressionNode) -> bool {
    match node {
        ExpressionNode::Number(_) => true,
        ExpressionNode::Parenthesis(inner) => is_constant_divisor(inner),
        _ => false,
    }
}

/// Дописывает `SV-009` в приёмник, если делитель не константа.
fn warn_variable_divisor(scope: &Scope, divisor: &ExpressionNode) {
    if !is_constant_divisor(divisor) {
        scope.warnings.borrow_mut().push(sv009());
    }
}

/// Печатает условие ребра (`ref S: cond;`) в SystemVerilog.
///
/// # Ошибки
/// [`SV-002`](sv002) на непокрытом узле, [`SV-005`](sv005) на `extern fn`.
pub(crate) fn print_condition(node: &ConditionNode, scope: &Scope) -> Result<String, Diagnostic> {
    // Форма `S(Модель) = Состояние`: правая часть приходит неразрешённой (инвариант
    // проекта), и общий разбор отверг бы её.
    if let Some(text) = sv_state_of::print(node, scope) {
        return Ok(text);
    }
    // Каждый бинарный узел - в скобках: печатается дерево, а не текст.
    let bin = |l: &ConditionNode, op: &str, r: &ConditionNode| -> Result<String, Diagnostic> {
        Ok(format!(
            "({} {} {})",
            print_condition(l, scope)?,
            op,
            print_condition(r, scope)?
        ))
    };
    // Сравнение операндов разной знаковости. В SystemVerilog оно
    // приводит оба к **беззнаковым**, поэтому `-1 < 200` при `i8`/`u8` давало
    // **ложь** - молча, verilator такой модуль принимает. Формы выбраны
    // прогоном: расширение `$signed(W'(x))` и раскрытие проверкой знака.
    let cmp = |l: &ConditionNode, op: &str, r: &ConditionNode| -> Result<String, Diagnostic> {
        // Имя варианта рядом с целым операндом печатается значением: `ENUMITEMREF`
        // шириной 2 бита против 8-битного сигнала даёт `WIDTHEXPAND`, а проверка цели
        // считает предупреждение ошибкой.
        if let Some(low) = crate::generator::enum_compare::lowered(l, r) {
            let (lt, rt) = if low.variant_is_left {
                (low.value.to_string(), print_condition(r, scope)?)
            } else {
                (print_condition(l, scope)?, low.value.to_string())
            };
            return Ok(format!("({lt} {op} {rt})"));
        }
        match crate::generator::mixed_sign::plan(
            crate::generator::mixed_sign::operand_type_cond(l).as_ref(),
            crate::generator::mixed_sign::operand_type_cond(r).as_ref(),
        ) {
            crate::generator::mixed_sign::Plan::AsIs => bin(l, op, r),
            crate::generator::mixed_sign::Plan::Widen { bits } => Ok(format!(
                "($signed({bits}'({})) {op} $signed({bits}'({})))",
                print_condition(l, scope)?,
                print_condition(r, scope)?
            )),
            crate::generator::mixed_sign::Plan::SignGuard { signed_is_left } => {
                let (lt, rt) = (print_condition(l, scope)?, print_condition(r, scope)?);
                Ok(sign_guard(&lt, op, &rt, signed_is_left))
            }
        }
    };
    match node {
        // Безусловное ребро: `if (1'b1)` не печатается - вызывающий код (`sv_fsm`)
        // обязан различать условный и безусловный переход, потому что безусловный
        // делает всё, что ниже него, недостижимым.
        ConditionNode::None => Ok("1'b1".to_string()),
        // Литерал длительности в условии - миллисекунды, как и значение. Выдержка
        // `after` здесь **не** обрабатывается: её печатает `sv_time` (у него есть
        // доступ к счётчику и профилю), и попадание сюда означало бы разбор в обход
        // того пути.
        ConditionNode::Duration(nanos) => Ok(crate::semantic::duration::value_millis(
            *nanos,
            crate::generator::site::at(Location::Codegen),
            "литерал длительности в условии",
        )?
        .to_string()),
        // Выдержка (константная и вычисляемая) печатается `sv_time`: только у него есть
        // счётчик и профиль.
        ConditionNode::After(_) | ConditionNode::AfterTicks(_) | ConditionNode::AfterExpr(_) => {
            Err(Diagnostic::error(
                crate::generator::site::at(Location::Codegen),
                "выдержка 'after' обязана печататься через sv_time, а не как условие".to_string(),
            )
            .with_code("SV-015"))
        }
        ConditionNode::Bool(v) => Ok(if *v { "1'b1" } else { "1'b0" }.to_string()),
        ConditionNode::Number(n) => Ok(n.to_string()),
        ConditionNode::Parenthesis(inner) => Ok(format!("({})", print_condition(inner, scope)?)),
        ConditionNode::Not(inner) => Ok(format!("(!{})", print_condition(inner, scope)?)),
        // Логические, а не побитовые: так у эталона (`c_expr.rs:656`).
        ConditionNode::And(l, r) => bin(l, "&&", r),
        ConditionNode::Or(l, r) => bin(l, "||", r),
        ConditionNode::Add(l, r) => bin(l, "+", r),
        ConditionNode::Subtract(l, r) => bin(l, "-", r),
        ConditionNode::Less(l, r) => cmp(l, "<", r),
        ConditionNode::More(l, r) => cmp(l, ">", r),
        ConditionNode::LessEqual(l, r) => cmp(l, "<=", r),
        ConditionNode::MoreEqual(l, r) => cmp(l, ">=", r),
        ConditionNode::Equal(l, r) => cmp(l, "==", r),
        ConditionNode::NotEqual(l, r) => cmp(l, "!=", r),
        ConditionNode::Variable(var, _) => signal_of_in(var, scope)
            .map(|name| scope.read(&name))
            .ok_or_else(|| sv002("неразрешённая переменная в условии")),
        // База - выражение: печатается тем же печатником условий, поэтому `b.data[1]`
        // выходит через ту же форму доступа к полю. Печатников два, и сужение индекса
        // нужно обоим.
        ConditionNode::ArraySubscript(base, index) => Ok(format!(
            "{}[{}]",
            print_condition(base, scope)?,
            crate::generator::sv::sv_array::index_text(
                crate::generator::sv::sv_array::array_type_cond(base).as_ref(),
                crate::generator::mixed_sign::operand_type_cond(index).as_ref(),
                print_condition(index, scope)?,
            )
        )),
        ConditionNode::BitAccess(inner, member) => {
            Ok(print_member(&print_condition(inner, scope)?, member))
        }
        ConditionNode::EnumVariant(def, variant, _) => {
            Ok(sv_enum_variant_name(&def.borrow().name, variant))
        }
        ConditionNode::Function(func, args, loc) => {
            let printed: Result<Vec<String>, Diagnostic> =
                args.iter().map(|a| print_condition(a, scope)).collect();
            super::sv_call::print_call(&func.borrow(), &printed?, *loc)
        }
        // Ветки `_` нет намеренно: `ConditionNode` объявлен в этом же крейте, поэтому
        // исчерпывающий разбор возможен - и обязан валить сборку при добавлении
        // варианта, а не проглатывать его молча.
        ConditionNode::Unresolved(_) => Err(sv002("неразрешённое условие")),
        ConditionNode::Rational(_, _) => Err(sv002(
            "вещественный литерал: в синтезируемом RTL плавающей точки нет (см. SV-003)",
        )),
        ConditionNode::String(_) => Err(sv002("строковый литерал")),
        ConditionNode::Model(_, _) => Err(sv002("ссылка на модель в условии")),
        ConditionNode::State(..) => Err(sv002("ссылка на состояние в условии")),
        // Анонимное обращение - см. оговорку у печатника выражений.
        ConditionNode::AnonPort(access) => Ok(scope.read(&access.synthetic_name())),
    }
}

/// Печатает выражение (тело блока) в SystemVerilog.
///
/// # Ошибки
/// [`SV-002`](sv002) на непокрытом узле, [`SV-005`](sv005) на `extern fn`.
pub(crate) fn print_expression(node: &ExpressionNode, scope: &Scope) -> Result<String, Diagnostic> {
    // Сравнение операндов разной знаковости: правило одно с печатником условий; здесь -
    // путь тела, где условие приходит выражением.
    let expr_cmp =
        |l: &ExpressionNode, op: &str, r: &ExpressionNode| -> Result<String, Diagnostic> {
            match crate::generator::mixed_sign::plan(
                crate::generator::mixed_sign::operand_type_expr(l).as_ref(),
                crate::generator::mixed_sign::operand_type_expr(r).as_ref(),
            ) {
                crate::generator::mixed_sign::Plan::AsIs => Ok(format!(
                    "({} {} {})",
                    print_expression(l, scope)?,
                    op,
                    print_expression(r, scope)?
                )),
                crate::generator::mixed_sign::Plan::Widen { bits } => Ok(format!(
                    "($signed({bits}'({})) {op} $signed({bits}'({})))",
                    print_expression(l, scope)?,
                    print_expression(r, scope)?
                )),
                crate::generator::mixed_sign::Plan::SignGuard { signed_is_left } => {
                    let (lt, rt) = (print_expression(l, scope)?, print_expression(r, scope)?);
                    Ok(sign_guard(&lt, op, &rt, signed_is_left))
                }
            }
        };
    let bin = |l: &ExpressionNode, op: &str, r: &ExpressionNode| -> Result<String, Diagnostic> {
        Ok(format!(
            "({} {} {})",
            print_expression(l, scope)?,
            op,
            print_expression(r, scope)?
        ))
    };
    // Q-путь бинарной операции: `Some` ⇔ узел имеет тип q(m, n).
    let fixed_bin = |node: &ExpressionNode,
                     op: super::sv_fixed::FixedOp,
                     l: &ExpressionNode,
                     r: &ExpressionNode|
     -> Option<Result<String, Diagnostic>> {
        super::sv_fixed::fixed_format_in(node, scope.structs)
            .map(|(m, n, sat)| super::sv_fixed::binary(op, l, r, scope, m, n, sat))
    };
    match node {
        // Длительность печатается **миллисекундами**; пересчёт зовёт общий слой - своей
        // арифметики времени генератор не заводит.
        ExpressionNode::Duration(nanos) => Ok(crate::semantic::duration::value_millis(
            *nanos,
            crate::generator::site::at(Location::Codegen),
            "литерал длительности",
        )?
        .to_string()),
        ExpressionNode::Number(n) => Ok(n.to_string()),
        ExpressionNode::Bool(v) => Ok(if *v { "1'b1" } else { "1'b0" }.to_string()),
        ExpressionNode::Parenthesis(inner) => Ok(format!("({})", print_expression(inner, scope)?)),
        ExpressionNode::Not(inner) => Ok(format!("(!{})", print_expression(inner, scope)?)),
        ExpressionNode::BitwiseNot(inner) => Ok(format!("(~{})", print_expression(inner, scope)?)),
        ExpressionNode::Negate(inner) => {
            match super::sv_fixed::fixed_format_in(node, scope.structs) {
                Some((m, n, sat)) => super::sv_fixed::negate(inner, scope, m, n, sat),
                None => Ok(format!("(-{})", print_expression(inner, scope)?)),
            }
        }
        ExpressionNode::UnaryPlus(inner) => Ok(format!("(+{})", print_expression(inner, scope)?)),
        // Над q(m, n) - масштабирующая Q-арифметика; иначе прямая.
        ExpressionNode::Multiply(l, r) => fixed_bin(node, super::sv_fixed::FixedOp::Multiply, l, r)
            .unwrap_or_else(|| bin(l, "*", r)),
        // `/` и `%` синтезируются в аппаратный делитель - крупный и медленный блок.
        // Трансляция прямая (и для q(m, n) через `fixed_bin`, action A-3), но
        // переменный делитель порождает `SV-009`: автор `.takt` цену потолка частоты из
        // текста модели иначе не увидит. Константа - молча.
        ExpressionNode::Divide(l, r) => {
            warn_variable_divisor(scope, r);
            fixed_bin(node, super::sv_fixed::FixedOp::Divide, l, r)
                .unwrap_or_else(|| bin(l, "/", r))
        }
        ExpressionNode::Modulo(l, r) => {
            warn_variable_divisor(scope, r);
            bin(l, "%", r)
        }
        ExpressionNode::Add(l, r) => {
            fixed_bin(node, super::sv_fixed::FixedOp::Add, l, r).unwrap_or_else(|| bin(l, "+", r))
        }
        ExpressionNode::Subtract(l, r) => fixed_bin(node, super::sv_fixed::FixedOp::Subtract, l, r)
            .unwrap_or_else(|| bin(l, "-", r)),
        ExpressionNode::ShiftLeft(l, r) => bin(l, "<<", r),
        // Знаковый операнд требует арифметического сдвига (`>>>`); выбор оператора - в
        // `sv_cast`, рядом со знаковостью.
        ExpressionNode::ShiftRight(l, r) => bin(l, super::sv_cast::shift_right_operator(l), r),
        ExpressionNode::BitwiseAnd(l, r) => bin(l, "&", r),
        ExpressionNode::BitwiseOr(l, r) => bin(l, "|", r),
        ExpressionNode::BitwiseXor(l, r) => bin(l, "^", r),
        ExpressionNode::And(l, r) => bin(l, "&&", r),
        ExpressionNode::Or(l, r) => bin(l, "||", r),
        ExpressionNode::Less(l, r) => expr_cmp(l, "<", r),
        ExpressionNode::More(l, r) => expr_cmp(l, ">", r),
        ExpressionNode::LessEqual(l, r) => expr_cmp(l, "<=", r),
        ExpressionNode::MoreEqual(l, r) => expr_cmp(l, ">=", r),
        ExpressionNode::Equal(l, r) => expr_cmp(l, "==", r),
        ExpressionNode::NotEqual(l, r) => expr_cmp(l, "!=", r),
        ExpressionNode::ConditionalOperator(c, t, f) => Ok(format!(
            "({} ? {} : {})",
            print_expression(c, scope)?,
            print_expression(t, scope)?,
            print_expression(f, scope)?
        )),
        ExpressionNode::Variable(var) => signal_of_in(var, scope)
            .map(|name| scope.read(&name))
            .ok_or_else(|| sv002("неразрешённая переменная")),
        // Индекс сужается до ширины, которую требует размер массива: иначе verilator
        // отвечает `WIDTHTRUNC`, а проверка цели считает предупреждение ошибкой. Правило -
        // общий носитель `sv_array`.
        ExpressionNode::ArraySubscript(base, index) => Ok(format!(
            "{}[{}]",
            print_expression(base, scope)?,
            crate::generator::sv::sv_array::index_text(
                crate::generator::sv::sv_array::array_type_expr(base).as_ref(),
                crate::generator::mixed_sign::operand_type_expr(index).as_ref(),
                print_expression(index, scope)?,
            )
        )),
        ExpressionNode::BitAccess(inner, member) => {
            Ok(print_member(&print_expression(inner, scope)?, member))
        }
        ExpressionNode::Function(func, args) => {
            let f = func.borrow();
            // Аргумент - позиция приёмника с известным типом: разряд `x.N` даёт один
            // бит, и verilator отвечал `WIDTHEXPAND` ("Operator FUNCREF expects 8
            // bits"), а проверка цели считает предупреждение ошибкой. Аргумент-Массив
            // печатается конкатенацией: параметр передаётся плоским вектором, потому
            // что распакованную размерность у порта функции yosys не принимает вовсе.
            let printed: Result<Vec<String>, Diagnostic> = args
                .iter()
                .enumerate()
                .map(|(i, a)| match param_type(&f, i) {
                    Some(ty) => {
                        let fields_of = |name: &str| scope.structs.get(name).cloned();
                        match crate::generator::sv::sv_array::flat_param(
                            &ty,
                            &fields_of,
                            scope.enums,
                        ) {
                            Some(flat) => Ok(crate::generator::sv::sv_array::flatten_argument(
                                &print_expression(a, scope)?,
                                &flat,
                            )),
                            None => scope.coerce(&ty, a),
                        }
                    }
                    None => print_expression(a, scope),
                })
                .collect();
            let loc = f.loc();
            super::sv_call::print_call(&f, &printed?, loc)
        }
        // Присваивание - оператор, а не выражение: печатается в `sv_stmt`. Здесь оно
        // означало бы `x = (y = 1)`, чего Takt не строит.
        ExpressionNode::Assign(_, _) => Err(sv002(
            "присваивание внутри выражения (в SystemVerilog присваивание — оператор)",
        )),
        // Степень с литеральным показателем разворачивается в умножения - синтезатору
        // нужна константа, и она здесь есть.
        ExpressionNode::Power(base, exp) => super::sv_cast::power(base, exp, scope),
        ExpressionNode::Rational(_, _) => Err(sv002(
            "вещественный литерал: в синтезируемом RTL плавающей точки нет (см. SV-003)",
        )),
        ExpressionNode::None => Err(sv002("пустое выражение")),
        ExpressionNode::Unresolved(_) => Err(sv002("неразрешённое выражение")),
        ExpressionNode::ArraySlice(_, _, _) => Err(sv002("срез массива")),
        ExpressionNode::CodeBlock(_, _) => Err(sv002("блок кода в выражении")),
        ExpressionNode::NamedFunctionBox(_, _) => Err(sv002("вызов с именованными аргументами")),
        ExpressionNode::String(_) => Err(sv002("строковый литерал")),
        ExpressionNode::Type(_) => Err(sv002("тип в позиции выражения")),
        ExpressionNode::Address(_, _) => Err(sv002(
            "адрес порта: для RTL адрес бессмыслен — сигнал приходит на вывод \
             кристалла, а не по адресу",
        )),
        // Ячейка по адресу - сигнал регистрового файла; `read` даёт `_next` (капкан ).
        // Сюда доходит только `sv-mmio`: цель `sv` отвергает такую модель в точке входа
        // (`SV-017`).
        ExpressionNode::AnonPort(access) => Ok(scope.read(&access.synthetic_name())),
        ExpressionNode::Model(_) => Err(sv002("ссылка на модель в выражении")),
        // Именованное условие печатается своим печатником условий: своего разбора здесь
        // нет - второе знание об условии разошлось бы с первым.
        ExpressionNode::Condition(cond) => print_condition(&cond.borrow().value, scope),
        ExpressionNode::List(_) => Err(sv002("список параметров в позиции выражения")),
        ExpressionNode::Array(_) => Err(sv002("литерал массива")),
        ExpressionNode::Initializer(_) => Err(sv002("инициализатор структуры")),
        // Fixed-point: масштабирующее приведение, когда источник либо цель - q(m, n).
        // Прочие `as` целью sv по-прежнему не транслируются.
        ExpressionNode::Cast(inner, ty) => {
            if matches!(ty, TypeNode::Fixed { .. })
                || super::sv_fixed::fixed_format_in(inner, scope.structs).is_some()
            {
                super::sv_fixed::cast(inner, ty, scope)
            } else if same_printed_type(inner, ty) {
                // Приведение к тому же типу опускается: форма `32'(x)` при `x: u32`
                // валидна, но это лишний код в RTL, а правило у трёх целей должно быть
                // одно.
                print_expression(inner, scope)
            } else {
                crate::generator::sv::sv_cast::integer_cast(inner, ty, scope)
            }
        }
    }
}

/// Тип `i`-го параметра функции, если он объявлен.
///
/// Внешняя функция типов параметров не объявляет - там приведения нет и быть не может:
/// приёмник неизвестен.
fn param_type(
    func: &crate::semantic::FunctionDefinitionNode,
    index: usize,
) -> Option<crate::semantic::type_node::TypeNode> {
    match func {
        crate::semantic::FunctionDefinitionNode::Local { params, .. } => {
            params.get(index).map(|(_, ty)| ty.clone())
        }
        _ => None,
    }
}

/// Раскрытие сравнения проверкой знака.
///
/// Общего типа нет (`u64` против знакового), поэтому правило записывается явно:
/// отрицательное меньше любого беззнакового. Операнд печатается дважды - в условии Takt
/// эффектов не бывает.
fn sign_guard(lhs: &str, op: &str, rhs: &str, signed_is_left: bool) -> String {
    let (signed, unsigned) = if signed_is_left {
        (lhs, rhs)
    } else {
        (rhs, lhs)
    };
    let neg = format!("({signed} < 0)");
    let same = if signed_is_left {
        format!("($unsigned({signed}) {op} {unsigned})")
    } else {
        format!("({unsigned} {op} $unsigned({signed}))")
    };
    let negative_wins = crate::generator::mixed_sign::negative_wins(op, signed_is_left);
    if negative_wins {
        format!("({neg} || {same})")
    } else {
        format!("(!{neg} && {same})")
    }
}

/// Совпадают ли тип операнда и цель приведения после отображения в SV.
///
/// Сравниваются **напечатанные** типы, а не типы Takt: `duration` отображается в `logic
/// [31:0]`, и типы Takt при этом различны - признак 0361 такую запись не ловил, хотя
/// печатал `32'(x)` над 32-битным значением.
///
/// Тип операнда берётся у **именованного значения**: у литерала и выражения он
/// печатнику неизвестен.
fn same_printed_type(inner: &ExpressionNode, ty: &crate::semantic::type_node::TypeNode) -> bool {
    let Some(from) = crate::generator::mixed_sign::operand_type_expr(inner) else {
        return false;
    };
    match (
        super::sv_type::sv_type(&from, "приведение типа"),
        super::sv_type::sv_type(ty, "приведение типа"),
    ) {
        // Сравнение по объявлению одного и того же имени: так в одну строку сходятся и
        // ширина, и знаковость, и упакованность.
        (Ok(from), Ok(to)) => from.declare("x") == to.declare("x"),
        // Неотобразимый тип судит печать самого приведения - здесь молчим.
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::ast::Member;
    use crate::semantic::VariableNode;
    use std::collections::BTreeSet;

    /// Пустой контекст: регистровых пар нет.
    fn empty_scope() -> BTreeSet<String> {
        BTreeSet::new()
    }

    fn num(n: i128) -> Box<ConditionNode> {
        Box::new(ConditionNode::Number(n))
    }

    /// **Ключевой тест: печатается дерево, а не текст.**
    ///
    /// `a == b | c` - узел `Or(Equal(a, b), c)`. Скобки вокруг каждого бинарного узла
    /// делают структуру явной и не дают приоритетам целевого языка переопределить
    /// разбор. Именно отсутствие этой дисциплины дало в цели `rust` молчащий дефект
    /// (`CLAUDE.md`).
    #[test]
    fn binary_nodes_are_fully_parenthesized() {
        let set = empty_scope();
        let enums = std::collections::BTreeMap::new();
        let structs = std::collections::BTreeMap::new();
        let warnings = std::cell::RefCell::new(Vec::new());
        let scope = Scope {
            registered: &set,
            guard_enable: true,
            inouts: crate::generator::sv::sv_scope::no_inouts(),
            function: None,
            function_ret: None,
            locals: crate::generator::sv::sv_scope::no_locals(),
            enums: &enums,
            structs: &structs,
            warnings: &warnings,
        };
        let node = ConditionNode::Or(Box::new(ConditionNode::Equal(num(2), num(2))), num(1));
        assert_eq!(print_condition(&node, &scope).unwrap(), "((2 == 2) || 1)");
    }

    /// `&`/`|` условий печатаются логическими - как у эталона (цель `c`).
    ///
    /// Сверка модели ведётся против C, поэтому расхождение с ним было бы дефектом: на
    /// многобитном операнде `a & b` и `a && b` дают разное.
    #[test]
    fn condition_and_or_are_logical_like_c() {
        let set = empty_scope();
        let enums = std::collections::BTreeMap::new();
        let structs = std::collections::BTreeMap::new();
        let warnings = std::cell::RefCell::new(Vec::new());
        let scope = Scope {
            registered: &set,
            guard_enable: true,
            inouts: crate::generator::sv::sv_scope::no_inouts(),
            function: None,
            function_ret: None,
            locals: crate::generator::sv::sv_scope::no_locals(),
            enums: &enums,
            structs: &structs,
            warnings: &warnings,
        };
        let and = ConditionNode::And(num(1), num(0));
        let or = ConditionNode::Or(num(1), num(0));
        assert_eq!(print_condition(&and, &scope).unwrap(), "(1 && 0)");
        assert_eq!(print_condition(&or, &scope).unwrap(), "(1 || 0)");
    }

    /// Литералы печатаются без явной ширины.
    ///
    /// Тест против возврата к букве: проба 2026-07-16 показала, что `verilator -Wall`
    /// принимает голые литералы чисто, а требование ширины было основано на
    /// несостоявшемся `WIDTHEXPAND`.
    #[test]
    fn literals_are_printed_unsized() {
        let set = empty_scope();
        let enums = std::collections::BTreeMap::new();
        let structs = std::collections::BTreeMap::new();
        let warnings = std::cell::RefCell::new(Vec::new());
        let scope = Scope {
            registered: &set,
            guard_enable: true,
            inouts: crate::generator::sv::sv_scope::no_inouts(),
            function: None,
            function_ret: None,
            locals: crate::generator::sv::sv_scope::no_locals(),
            enums: &enums,
            structs: &structs,
            warnings: &warnings,
        };
        assert_eq!(
            print_condition(&ConditionNode::Number(7), &scope).unwrap(),
            "7"
        );
        assert_eq!(
            print_expression(&ExpressionNode::Number(300), &scope).unwrap(),
            "300"
        );
    }

    /// Булев литерал печатается однобитным: `1'b1`/`1'b0`.
    #[test]
    fn bool_literals_are_one_bit() {
        let set = empty_scope();
        let enums = std::collections::BTreeMap::new();
        let structs = std::collections::BTreeMap::new();
        let warnings = std::cell::RefCell::new(Vec::new());
        let scope = Scope {
            registered: &set,
            guard_enable: true,
            inouts: crate::generator::sv::sv_scope::no_inouts(),
            function: None,
            function_ret: None,
            locals: crate::generator::sv::sv_scope::no_locals(),
            enums: &enums,
            structs: &structs,
            warnings: &warnings,
        };
        assert_eq!(
            print_condition(&ConditionNode::Bool(true), &scope).unwrap(),
            "1'b1"
        );
        assert_eq!(
            print_expression(&ExpressionNode::Bool(false), &scope).unwrap(),
            "1'b0"
        );
    }

    /// Запись в регистровый сигнал идёт в комбинационную пару `_next`.
    #[test]
    fn write_targets_next_signal() {
        let mut set = BTreeSet::new();
        set.insert("cmd_fork".to_string());
        let enums = std::collections::BTreeMap::new();
        let structs = std::collections::BTreeMap::new();
        let warnings = std::cell::RefCell::new(Vec::new());
        let scope = Scope {
            registered: &set,
            guard_enable: true,
            inouts: crate::generator::sv::sv_scope::no_inouts(),
            function: None,
            function_ret: None,
            locals: crate::generator::sv::sv_scope::no_locals(),
            enums: &enums,
            structs: &structs,
            warnings: &warnings,
        };
        assert_eq!(scope.write("cmd_fork"), "cmd_fork_next");
        // У локальной переменной функции пары нет.
        assert_eq!(scope.write("tmp"), "tmp");
    }

    /// **Уплощение:** префиксованный сигнал под-модели получает свою пару `_next`.
    ///
    /// У цели `c` две под-модели вправе иметь переменную `counter` - они лежат по своим
    /// структурам. В одном модуле SV они слиплись бы, поэтому имя сигнала строит
    /// `signal_of` по владельцу переменной, а `_next` - от уже готового имени сигнала.
    #[test]
    fn submodel_signal_gets_next_pair() {
        let mut set = BTreeSet::new();
        set.insert("cabin_counter".to_string());
        let enums = std::collections::BTreeMap::new();
        let structs = std::collections::BTreeMap::new();
        let warnings = std::cell::RefCell::new(Vec::new());
        let scope = Scope {
            registered: &set,
            guard_enable: true,
            inouts: crate::generator::sv::sv_scope::no_inouts(),
            function: None,
            function_ret: None,
            locals: crate::generator::sv::sv_scope::no_locals(),
            enums: &enums,
            structs: &structs,
            warnings: &warnings,
        };
        assert_eq!(scope.write("cabin_counter"), "cabin_counter_next");
    }

    /// Доступ к биту `x.0` -> `x[0]`: в SV вектор индексируется как массив.
    ///
    /// Заметно проще цели `st`, где это разворачивается в маску: MatIEC не знает ни
    /// `x.0`, ни `%X0`.
    #[test]
    fn bit_access_is_plain_indexing() {
        let set = empty_scope();
        let enums = std::collections::BTreeMap::new();
        let structs = std::collections::BTreeMap::new();
        let warnings = std::cell::RefCell::new(Vec::new());
        let scope = Scope {
            registered: &set,
            guard_enable: true,
            inouts: crate::generator::sv::sv_scope::no_inouts(),
            function: None,
            function_ret: None,
            locals: crate::generator::sv::sv_scope::no_locals(),
            enums: &enums,
            structs: &structs,
            warnings: &warnings,
        };
        let node = ConditionNode::BitAccess(num(5), Member::Number(0));
        assert_eq!(print_condition(&node, &scope).unwrap(), "5[0]");
    }

    /// Вариант перечисления получает префикс имени перечисления.
    ///
    /// Метки `enum` в SV живут в общем пространстве имён модуля: два перечисления с
    /// вариантом `Idle` без префикса дали бы повторное объявление.
    #[test]
    fn enum_variant_is_prefixed_by_enum_name() {
        assert_eq!(sv_enum_variant_name("Action", "Idle"), "ACTION_IDLE");
        assert_eq!(sv_enum_variant_name("Mode", "Idle"), "MODE_IDLE");
    }

    /// **Контрпример:** вещественный литерал в условии -> `SV-002`, а не молчание.
    #[test]
    fn rational_literal_is_sv002() {
        let set = empty_scope();
        let enums = std::collections::BTreeMap::new();
        let structs = std::collections::BTreeMap::new();
        let warnings = std::cell::RefCell::new(Vec::new());
        let scope = Scope {
            registered: &set,
            guard_enable: true,
            inouts: crate::generator::sv::sv_scope::no_inouts(),
            function: None,
            function_ret: None,
            locals: crate::generator::sv::sv_scope::no_locals(),
            enums: &enums,
            structs: &structs,
            warnings: &warnings,
        };
        let err = print_condition(&ConditionNode::Rational("1.5".to_string(), false), &scope)
            .unwrap_err();
        assert_eq!(err.code.as_deref(), Some("SV-002"));
    }

    /// **Контрпример:** непереводимые узлы выражения дают `SV-002`.
    #[test]
    fn untranslatable_expression_nodes_are_sv002() {
        let set = empty_scope();
        let enums = std::collections::BTreeMap::new();
        let structs = std::collections::BTreeMap::new();
        let warnings = std::cell::RefCell::new(Vec::new());
        let scope = Scope {
            registered: &set,
            guard_enable: true,
            inouts: crate::generator::sv::sv_scope::no_inouts(),
            function: None,
            function_ret: None,
            locals: crate::generator::sv::sv_scope::no_locals(),
            enums: &enums,
            structs: &structs,
            warnings: &warnings,
        };
        for node in [
            ExpressionNode::None,
            ExpressionNode::String(vec!["s".to_string()]),
            ExpressionNode::Array(vec![]),
            ExpressionNode::Address(0x100, 0),
        ] {
            let err = print_expression(&node, &scope).unwrap_err();
            assert_eq!(err.code.as_deref(), Some("SV-002"), "узел {:?}", node);
        }
    }

    // ---: предупреждение `SV-009` о переменном делителе ---

    /// Строит `ExpressionNode::Variable` без владельца (сигнал = имя).
    fn var(name: &str) -> Box<ExpressionNode> {
        Box::new(ExpressionNode::Variable(std::rc::Rc::new(
            std::cell::RefCell::new(VariableNode::Simple {
                upper: None,
                loc: Location::Implicit,
                name: name.to_string(),
                ty: TypeNode::Integer {
                    bits: 8,
                    signed: false,
                },
                expr: ExpressionNode::None,
            }),
        )))
    }

    /// Печатает выражение и возвращает вместе с собранными предупреждениями.
    fn print_with_warnings(node: &ExpressionNode) -> (String, Vec<Diagnostic>) {
        let set = empty_scope();
        let enums = std::collections::BTreeMap::new();
        let structs = std::collections::BTreeMap::new();
        let warnings = std::cell::RefCell::new(Vec::new());
        let scope = Scope {
            registered: &set,
            guard_enable: true,
            inouts: crate::generator::sv::sv_scope::no_inouts(),
            function: None,
            function_ret: None,
            locals: crate::generator::sv::sv_scope::no_locals(),
            enums: &enums,
            structs: &structs,
            warnings: &warnings,
        };
        let out = print_expression(node, &scope).unwrap();
        (out, warnings.into_inner())
    }

    fn codes(warnings: &[Diagnostic]) -> Vec<&str> {
        warnings.iter().filter_map(|w| w.code.as_deref()).collect()
    }

    /// T1/A1: `a / b` (переменный делитель) -> `SV-009`; трансляция успешна.
    #[test]
    fn variable_divide_warns_sv009() {
        let node = ExpressionNode::Divide(var("a"), var("b"));
        let (out, warnings) = print_with_warnings(&node);
        assert_eq!(out, "(a / b)", "трансляция обязана состояться");
        assert_eq!(codes(&warnings), ["SV-009"]);
    }

    /// T3/A3: `a % b` (переменный делитель) -> `SV-009`.
    #[test]
    fn variable_modulo_warns_sv009() {
        let node = ExpressionNode::Modulo(var("a"), var("b"));
        let (out, warnings) = print_with_warnings(&node);
        assert_eq!(out, "(a % b)");
        assert_eq!(codes(&warnings), ["SV-009"]);
    }

    /// T2/A2: `a / 2` (константа - степень двойки) -> **молчание**.
    #[test]
    fn constant_power_of_two_divide_is_silent() {
        let node = ExpressionNode::Divide(var("a"), Box::new(ExpressionNode::Number(2)));
        let (_, warnings) = print_with_warnings(&node);
        assert!(
            warnings.is_empty(),
            "константа-делитель обязана молчать: {warnings:?}"
        );
    }

    /// T4/A2: `a / 3` (константа - не степень двойки) -> **молчание** (≈106 LUT).
    #[test]
    fn constant_non_power_of_two_divide_is_silent() {
        let node = ExpressionNode::Divide(var("a"), Box::new(ExpressionNode::Number(3)));
        let (_, warnings) = print_with_warnings(&node);
        assert!(warnings.is_empty(), "{warnings:?}");
    }

    /// T6/A3: `a % 4` (константа) -> **молчание**.
    #[test]
    fn constant_modulo_is_silent() {
        let node = ExpressionNode::Modulo(var("a"), Box::new(ExpressionNode::Number(4)));
        let (_, warnings) = print_with_warnings(&node);
        assert!(warnings.is_empty(), "{warnings:?}");
    }

    /// Скобки вокруг константы не превращают её в переменную: `a / (2)` - молчит.
    #[test]
    fn parenthesised_constant_divisor_is_silent() {
        let inner = Box::new(ExpressionNode::Parenthesis(Box::new(
            ExpressionNode::Number(2),
        )));
        let node = ExpressionNode::Divide(var("a"), inner);
        let (_, warnings) = print_with_warnings(&node);
        assert!(warnings.is_empty(), "{warnings:?}");
    }

    /// T5/A4: `a * b` -> **молчание**.
    #[test]
    fn multiply_is_silent() {
        let node = ExpressionNode::Multiply(var("a"), var("b"));
        let (out, warnings) = print_with_warnings(&node);
        assert_eq!(out, "(a * b)");
        assert!(warnings.is_empty(), "умножение молчит (DSP): {warnings:?}");
    }

    /// T8/A5: текст называет причину (нет аппаратного делителя) и следствие (потолок
    /// частоты), **без** конкретных LUT (они технологичны, action A-1).
    #[test]
    fn sv009_text_names_cause_and_consequence_without_luts() {
        let msg = &sv009().message;
        assert!(msg.contains("аппаратного делителя"), "нет причины:\n{msg}");
        assert!(
            msg.contains("частот"),
            "нет следствия (потолок частоты):\n{msg}"
        );
        assert!(
            !msg.contains("LUT") && !msg.contains("558"),
            "текст не должен называть конкретные LUT (устареют):\n{msg}"
        );
    }
}
