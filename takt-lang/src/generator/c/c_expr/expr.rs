//! Печать выражений.
//!
//! Часть модуля `c_expr`.

use super::*;
use crate::generator::c::c_unsupported::{self, UnsupportedNode};
use crate::generator::shift_width::{self, Direction};

/// Отказ на конструкции, которую цель `c` не переводит.
///
/// Позиция берётся у **самого узла** ([`ExpressionNode::loc`]): в пачке диагностик
/// сообщение без координаты бесполезно. Там, где узел позиции не несёт (ссылка на
/// модель, тип, литерал), ответом будет `Location::Builtin` - граница названная, а не
/// забытая.
fn unsupported(node: UnsupportedNode, expr: &ExpressionNode) -> Diagnostic {
    // Координата - у оператора: позиции употребления у выражения нет,
    // `ExpressionNode::loc()` выводит её из объявлений операндов, и `res := mem[1:2];`
    // в строке 7 указывал на строку 1, где объявлена `mem`.
    c_unsupported::refuse(node, crate::generator::site::at(expr.loc()))
}

/// Печать сдвига, величина которого не помещается в **продвинутый** тип C.
///
/// Возвращает `true`, если напечатала - тогда вызывающий обычный оператор не печатает.
///
/// # Порог
///
/// В C операнды сдвига **продвигаются** до `int` (C11 6.5.7p3), поэтому `u8 >> 8`
/// определено и совпадает с эталоном, а UB начинается с ширины продвинутого типа.
/// Отсюда порог `max(32, W)`: у типов уже `int` он равен самой ширине. Именно из-за
/// порога вывод корпуса не меняется - сдвигов такой величины в `examples/` нет ни
/// одного.
///
/// Порог принадлежит **целевому языку**; сам признак и значение насыщения - общему
/// носителю `generator::shift_width`, чтобы цели `c` и `rust` не разошлись на одном
/// входе.
fn shift_saturated(
    printer: &mut Printer,
    map: &CMap,
    owner: &Element,
    params: Vec<(String, TypeNode)>,
    direction: Direction,
    value: &ExpressionNode,
    amount: &ExpressionNode,
) -> Result<bool, Diagnostic> {
    let Some(bits) = shift_width::width_of(value) else {
        return Ok(false);
    };
    let threshold = bits.max(32);
    match shift_width::literal_saturation(direction, value, amount, threshold) {
        // Величина не литерал - насыщение считает хелпер: при
        // компиляции она неизвестна, а `v >> n` при `n >= ширины` в C есть UB.
        // Замер 2026-08-23 (`u32`, значение `0xFFFFFFFF`): при `n = 32`
        // прошивка давала **4294967295**, при `n = 48` - **65535** (сдвиг по
        // модулю 32), тогда как эталон, `rust`, `st` и `sv` дают 0; `cc -Wall
        // -Wextra -Werror` при этом молчит, а результат одинаков на `-O0` и
        // `-O2`. То есть расхождение значений, невидимое инструментам.
        shift_width::Saturation::AsIs if shift_width::literal(amount).is_none() => {
            variable_shift(
                printer, map, owner, params, direction, value, amount, threshold,
            )?;
            Ok(true)
        }
        shift_width::Saturation::AsIs => Ok(false),
        shift_width::Saturation::Zero => {
            printer.print("0");
            Ok(true)
        }
        shift_width::Saturation::SignOnly(by) => {
            // Скобки обязательны: сдвиг стоит в позиции операнда, и приоритет соседа
            // может быть выше (`a >> 32 & 1`).
            printer.print("(");
            generate_expr(printer, map, owner, params, value, 10, false)?;
            printer.print(&format!(" >> {by})"));
            Ok(true)
        }
    }
}

/// Сдвиг на переменную величину - через хелпер.
///
/// Тернарным оператором на месте это не выразить: `(n >= 32 ? 0 : (v >> n))` печатает
/// величину **дважды**, а вычисление операнда в языке Takt бывает с эффектом (вызов
/// функции пишет в переменные модели). Тот же довод, по которому цель `rust` выражает
/// знаковый сдвиг через `min`.
///
/// Порог - ширина **продвинутого** типа (`max(32, W)`, C11 6.5.7p3), как у литеральной
/// величины: на узких типах продвижение до `int` уже даёт ответ эталона, и хелпер там
/// менял бы вывод корпуса без нужды.
#[allow(clippy::too_many_arguments)]
fn variable_shift(
    printer: &mut Printer,
    map: &CMap,
    owner: &Element,
    params: Vec<(String, TypeNode)>,
    direction: Direction,
    value: &ExpressionNode,
    amount: &ExpressionNode,
    threshold: u8,
) -> Result<(), Diagnostic> {
    let signed = shift_width::signed_of(value);
    let (name, cast) = match (direction, signed) {
        (Direction::Left, _) => ("takt_shl", "(uint64_t)"),
        (Direction::Right, false) => ("takt_shr_u", "(uint64_t)"),
        (Direction::Right, true) => ("takt_shr_i", "(int64_t)"),
    };
    printer.print(&format!("{name}({cast}("));
    generate_expr(printer, map, owner, params.clone(), value, 0, false)?;
    printer.print("), (uint64_t)(");
    generate_expr(printer, map, owner, params, amount, 0, false)?;
    printer.print(&format!("), {threshold})"));
    Ok(())
}

/// Генерирует C-выражение из семантического узла с учётом приоритета операторов.
///
/// Скобки добавляются автоматически только там, где это необходимо для сохранения
/// семантики: если `expr_precedence(expr) < min_prec`.
///
/// Используйте `min_prec = 0` для выражений верхнего уровня.
pub(in crate::generator::c) fn generate_expr(
    printer: &mut Printer,
    map: &CMap,
    owner: &Element,
    params: Vec<(String, TypeNode)>,
    expr: &ExpressionNode,
    min_prec: u8,
    has_model: bool,
) -> Result<(), Diagnostic> {
    // Операция над бит-вектором шире 64 бит невыразима: носитель - массив слов, и в C
    // такое выражение означало бы арифметику указателя. Её не поддерживает и эталон
    // (`SIM-005` в такте), поэтому отказ приходит свой, с причиной, а не от `cc` на
    // порождённом файле.
    if let Some(op) = crate::generator::c::c_bits::wide_operand(expr) {
        return Err(unsupported(UnsupportedNode::WideBitVector(op), expr));
    }
    let my_prec = expr_precedence(expr);
    let wrap = my_prec < min_prec;
    if wrap {
        printer.print("(");
    }
    match expr {
        // Длительность печатается **миллисекундами** - единицей представления значения
        // в целях. Пересчёт зовёт общий слой: своей арифметики времени генератор не
        // заводит.
        ExpressionNode::Duration(nanos) => {
            let millis = crate::semantic::duration::value_millis(
                *nanos,
                crate::diagnostics::Location::Codegen,
                "литерал длительности",
            )?;
            printer.print(&millis.to_string());
        }
        // Выражения нет вовсе: полезной нагрузки у ветви тоже нет, позицию взять негде -
        // отказ остаётся безликим. Это предмет ("диагностика цели `c` без кода"), а не
        // забывчивость.
        ExpressionNode::None => {
            return Err(crate::generator::c::c_unresolved::refuse(
                Location::Codegen,
                crate::generator::c::c_unresolved::UnresolvedNode::EmptyExpression,
            ));
        }
        // Неразрешённое выражение отделено от отсутствующего: узел несёт АСД, а значит
        // и позицию, и отказ обязан её нести.
        ExpressionNode::Unresolved(raw) => {
            return Err(crate::generator::c::c_unresolved::refuse(
                raw.loc(),
                crate::generator::c::c_unresolved::UnresolvedNode::Expression,
            ));
        }

        // -- Литералы ----------------------------------------------------------
        ExpressionNode::Number(n) => {
            printer.print(&crate::generator::c::c_literal::c_int_literal(*n));
        }
        ExpressionNode::Bool(value) => {
            printer.print(if *value { "true" } else { "false" });
        }
        ExpressionNode::String(v) => {
            printer.print("\"").print(&v.join("")).print("\"");
        }
        ExpressionNode::Rational(s, neg) => {
            if *neg {
                printer.print("-");
            }
            printer.print(s);
        }

        // -- Унарные операторы --------------------------------------------------
        // min_prec=14 для операнда: бинарные выражения (prec<=13) будут обёрнуты; также
        // исключает двусмысленные `--x` и `++x` (унарный + унарный).
        ExpressionNode::Not(e) => {
            printer.print("!");
            generate_expr(printer, map, owner, params, e, 14, has_model)?;
        }
        ExpressionNode::BitwiseNot(e) => {
            printer.print("~");
            generate_expr(printer, map, owner, params, e, 14, has_model)?;
        }
        ExpressionNode::UnaryPlus(e) => {
            printer.print("+");
            generate_expr(printer, map, owner, params, e, 14, has_model)?;
        }
        ExpressionNode::Negate(e) => {
            // Унарный минус над q(m, n): −repr с wraparound к W.
            if let Some((m, n, sat)) = super::fixed::fixed_of(map, owner, expr) {
                super::fixed::negate(printer, map, owner, params, e, m, n, sat, has_model)?;
            } else {
                printer.print("-");
                generate_expr(printer, map, owner, params, e, 14, has_model)?;
            }
        }

        // -- Степень -> целочисленный хелпер -------------------------
        //
        // Прежде печаталось `pow((double)a, (double)b)`: у `double` 53 разряда
        // мантиссы, и `3 ** 40` давало 12157665459056928768 вместо 12157665459056928801 -
        // прошивка расходилась с эталоном молча. Заодно исчезла зависимость от `libm`
        // ради целой арифметики.
        ExpressionNode::Power(l, r) => {
            printer.print("takt_ipow((int64_t)(");
            generate_expr(printer, map, owner, params.clone(), l, 0, has_model)?;
            printer.print("), (int64_t)(");
            generate_expr(printer, map, owner, params, r, 0, has_model)?;
            printer.print("))");
        }

        // -- Бинарные арифметические -------------------------------------------- Левый
        // операнд: допускается тот же приоритет (левоассоциативность). Правый операнд:
        // требует более высокого приоритета (wrap при равном).
        ExpressionNode::Multiply(l, r) => {
            if let Some((m, n, sat)) = super::fixed::fixed_of(map, owner, expr) {
                super::fixed::binary(
                    printer,
                    map,
                    owner,
                    params,
                    super::fixed::FixedOp::Multiply,
                    l,
                    r,
                    m,
                    n,
                    sat,
                    has_model,
                )?;
            } else {
                generate_expr(printer, map, owner, params.clone(), l, 12, has_model)?;
                printer.print(" * ");
                generate_expr(printer, map, owner, params, r, 13, has_model)?;
            }
        }
        ExpressionNode::Divide(l, r) => {
            if let Some((m, n, sat)) = super::fixed::fixed_of(map, owner, expr) {
                super::fixed::binary(
                    printer,
                    map,
                    owner,
                    params,
                    super::fixed::FixedOp::Divide,
                    l,
                    r,
                    m,
                    n,
                    sat,
                    has_model,
                )?;
            } else {
                generate_expr(printer, map, owner, params.clone(), l, 12, has_model)?;
                printer.print(" / ");
                generate_expr(printer, map, owner, params, r, 13, has_model)?;
            }
        }
        ExpressionNode::Modulo(l, r) => {
            generate_expr(printer, map, owner, params.clone(), l, 12, has_model)?;
            printer.print(" % ");
            generate_expr(printer, map, owner, params, r, 13, has_model)?;
        }
        ExpressionNode::Add(l, r) => {
            if let Some((m, n, sat)) = super::fixed::fixed_of(map, owner, expr) {
                super::fixed::binary(
                    printer,
                    map,
                    owner,
                    params,
                    super::fixed::FixedOp::Add,
                    l,
                    r,
                    m,
                    n,
                    sat,
                    has_model,
                )?;
            } else {
                generate_expr(printer, map, owner, params.clone(), l, 11, has_model)?;
                printer.print(" + ");
                generate_expr(printer, map, owner, params, r, 12, has_model)?;
            }
        }
        ExpressionNode::Subtract(l, r) => {
            if let Some((m, n, sat)) = super::fixed::fixed_of(map, owner, expr) {
                super::fixed::binary(
                    printer,
                    map,
                    owner,
                    params,
                    super::fixed::FixedOp::Subtract,
                    l,
                    r,
                    m,
                    n,
                    sat,
                    has_model,
                )?;
            } else {
                generate_expr(printer, map, owner, params.clone(), l, 11, has_model)?;
                printer.print(" - ");
                generate_expr(printer, map, owner, params, r, 12, has_model)?;
            }
        }

        // -- Битовые сдвиги ----------------------------------------------------
        //
        // Величина, не меньшая ширины **продвинутого** типа, - UB в C, и `cc -Werror`
        // такой вывод отвергает. Признак и значение берутся у общего носителя
        // `generator::shift_width` - того же, каким живёт цель `rust`.
        ExpressionNode::ShiftLeft(l, r) => {
            if shift_saturated(printer, map, owner, params.clone(), Direction::Left, l, r)? {
                return Ok(());
            }
            generate_expr(printer, map, owner, params.clone(), l, 10, has_model)?;
            printer.print(" << ");
            generate_expr(printer, map, owner, params, r, 11, has_model)?;
        }
        ExpressionNode::ShiftRight(l, r) => {
            if shift_saturated(printer, map, owner, params.clone(), Direction::Right, l, r)? {
                return Ok(());
            }
            generate_expr(printer, map, owner, params.clone(), l, 10, has_model)?;
            printer.print(" >> ");
            generate_expr(printer, map, owner, params, r, 11, has_model)?;
        }

        // -- Побитовые операторы ------------------------------------------------
        ExpressionNode::BitwiseAnd(l, r) => {
            generate_expr(printer, map, owner, params.clone(), l, 7, has_model)?;
            printer.print(" & ");
            generate_expr(printer, map, owner, params, r, 8, has_model)?;
        }
        ExpressionNode::BitwiseXor(l, r) => {
            generate_expr(printer, map, owner, params.clone(), l, 6, has_model)?;
            printer.print(" ^ ");
            generate_expr(printer, map, owner, params, r, 7, has_model)?;
        }
        ExpressionNode::BitwiseOr(l, r) => {
            generate_expr(printer, map, owner, params.clone(), l, 5, has_model)?;
            printer.print(" | ");
            generate_expr(printer, map, owner, params, r, 6, has_model)?;
        }

        // -- Сравнение ---------------------------------------------------------
        ExpressionNode::Less(l, r) => {
            // Смешанная знаковость: на 64 битах C сравнивает беззнаково, и `-1 < 200`
            // давало ложь. Правило одно с печатником условий; здесь - путь тела (`if s
            // < u { ... }`).
            if let Some(text) = mixed_sign_compare(l, "<", r, map, owner, &params, has_model)? {
                printer.print(&text);
            } else {
                generate_expr(printer, map, owner, params.clone(), l, 9, has_model)?;
                printer.print(" < ");
                generate_expr(printer, map, owner, params, r, 10, has_model)?;
            }
        }
        ExpressionNode::More(l, r) => {
            // Смешанная знаковость: на 64 битах C сравнивает беззнаково, и `-1 < 200`
            // давало ложь. Правило одно с печатником условий; здесь - путь тела (`if s
            // < u { ... }`).
            if let Some(text) = mixed_sign_compare(l, ">", r, map, owner, &params, has_model)? {
                printer.print(&text);
            } else {
                generate_expr(printer, map, owner, params.clone(), l, 9, has_model)?;
                printer.print(" > ");
                generate_expr(printer, map, owner, params, r, 10, has_model)?;
            }
        }
        ExpressionNode::LessEqual(l, r) => {
            // Смешанная знаковость: на 64 битах C сравнивает беззнаково, и `-1 < 200`
            // давало ложь. Правило одно с печатником условий; здесь - путь тела (`if s
            // < u { ... }`).
            if let Some(text) = mixed_sign_compare(l, "<=", r, map, owner, &params, has_model)? {
                printer.print(&text);
            } else {
                generate_expr(printer, map, owner, params.clone(), l, 9, has_model)?;
                printer.print(" <= ");
                generate_expr(printer, map, owner, params, r, 10, has_model)?;
            }
        }
        ExpressionNode::MoreEqual(l, r) => {
            // Смешанная знаковость: на 64 битах C сравнивает беззнаково, и `-1 < 200`
            // давало ложь. Правило одно с печатником условий; здесь - путь тела (`if s
            // < u { ... }`).
            if let Some(text) = mixed_sign_compare(l, ">=", r, map, owner, &params, has_model)? {
                printer.print(&text);
            } else {
                generate_expr(printer, map, owner, params.clone(), l, 9, has_model)?;
                printer.print(" >= ");
                generate_expr(printer, map, owner, params, r, 10, has_model)?;
            }
        }
        ExpressionNode::Equal(l, r) => {
            // Смешанная знаковость: равенство ломается на 64 битах так же, как `<` -
            // первая редакция его не покрыла.
            if let Some(text) = mixed_sign_compare(l, "==", r, map, owner, &params, has_model)? {
                printer.print(&text);
            } else {
                generate_expr(printer, map, owner, params.clone(), l, 8, has_model)?;
                printer.print(" == ");
                generate_expr(printer, map, owner, params, r, 9, has_model)?;
            }
        }
        ExpressionNode::NotEqual(l, r) => {
            // Смешанная знаковость: равенство ломается на 64 битах так же, как `<` -
            // первая редакция его не покрыла.
            if let Some(text) = mixed_sign_compare(l, "!=", r, map, owner, &params, has_model)? {
                printer.print(&text);
            } else {
                generate_expr(printer, map, owner, params.clone(), l, 8, has_model)?;
                printer.print(" != ");
                generate_expr(printer, map, owner, params, r, 9, has_model)?;
            }
        }

        // -- Логические --------------------------------------------------------
        ExpressionNode::And(l, r) => {
            generate_expr(printer, map, owner, params.clone(), l, 4, has_model)?;
            printer.print(" && ");
            generate_expr(printer, map, owner, params, r, 5, has_model)?;
        }
        ExpressionNode::Or(l, r) => {
            generate_expr(printer, map, owner, params.clone(), l, 3, has_model)?;
            printer.print(" || ");
            generate_expr(printer, map, owner, params, r, 4, has_model)?;
        }

        // -- Специальные ------------------------------------------------------- Явные
        // скобки из исходного кода - всегда генерируем как есть.
        ExpressionNode::Parenthesis(e) => {
            printer.print("(");
            generate_expr(printer, map, owner, params, e, 0, has_model)?;
            printer.print(")");
        }

        // Тернарный оператор: условие обёртывается при prec <= ||, чтобы присваивание
        // или вложенный тернарный в условии был явно выделен.
        ExpressionNode::ConditionalOperator(cond, then_, else_) => {
            generate_expr(printer, map, owner, params.clone(), cond, 4, has_model)?;
            printer.print(" ? ");
            generate_expr(printer, map, owner, params.clone(), then_, 0, has_model)?;
            printer.print(" : ");
            generate_expr(printer, map, owner, params, else_, 0, has_model)?;
        }

        ExpressionNode::Assign(l, r) => {
            // Запись по анонимному адресу: поле уже слова пишется
            // чтением-изменением-записью, целое слово - прямым присваиванием.
            if let ExpressionNode::AnonPort(access) = l.as_ref() {
                if !map.hal() {
                    return Err(crate::generator::c::c_anon::refuse_plain_c());
                }
                let mut rhs_str = String::new();
                {
                    let mut tmp = Printer::new(4, &mut rhs_str);
                    generate_expr(&mut tmp, map, owner, params, r, 0, has_model)?;
                }
                printer.print(&crate::generator::c::c_anon::write(access, &rhs_str));
                return Ok(());
            }
            // Запись в элемент порта (`bus[i] := v`) - своим носителем: порт значением
            // не является, и место записи у него есть только в обращении к HAL.
            if super::port_element::emit_write(
                printer,
                map,
                owner,
                params.clone(),
                expr,
                l,
                r,
                has_model,
            )? {
                return Ok(());
            }
            // Запись в порт -> write_bit / write_float
            if let ExpressionNode::Variable(var_rc) = l.as_ref() {
                let var = var_rc.borrow();
                if let VariableNode::Port {
                    direction,
                    name,
                    ty,
                    upper,
                    ..
                } = &*var
                {
                    let model_name = if let Some(model_rc) =
                        upper.as_ref().and_then(|w| w.upgrade())
                    {
                        Name::from(model_rc)
                    } else {
                        return Err(crate::generator::c::c_unresolved::refuse(
                            expr.loc(),
                            crate::generator::c::c_unresolved::UnresolvedNode::PortOwner("запись"),
                        ));
                    };
                    let cls = PortClass::from_type(ty);
                    let variant = crate::generator::c::c_names::port_enum_variant(
                        &model_name,
                        name,
                        *direction,
                        crate::parser::ast::PortDirection::Out,
                    );
                    let mut rhs_str = String::new();
                    {
                        let mut tmp = Printer::new(4, &mut rhs_str);
                        generate_expr(&mut tmp, map, owner, params, r, 0, has_model)?;
                    }
                    let ptr = if has_model && !owner.name().eq(&map.root_name()) {
                        "main"
                    } else {
                        "model"
                    };
                    // Порт целиком - элемент нулевой: контракт один на все порты, и
                    // "нет индекса" в нём не бывает. Порт целиком - первый (и у скаляра
                    // единственный) элемент; у bit-порта это разряд ноль.
                    let element = if cls == PortClass::Bit {
                        crate::generator::c::c_port_call::SCALAR_BIT
                    } else {
                        crate::generator::c::c_port_call::SCALAR_INDEX
                    };
                    printer.print(&crate::generator::c::c_port_call::write(
                        cls, ptr, &variant, element, &rhs_str,
                    ));
                    return Ok(());
                }
            }
            // BitAccess как lvalue: `порт.N := v` - своим носителем: номер разряда
            // несёт само обращение к HAL.
            if let ExpressionNode::BitAccess(inner_expr, Member::Number(n)) = l.as_ref() {
                if super::port_element::emit_bit_write(
                    printer,
                    map,
                    owner,
                    params.clone(),
                    expr,
                    inner_expr,
                    *n,
                    r,
                    has_model,
                )? {
                    return Ok(());
                }
                // Обычная переменная.бит = val x = (x & ~(1u << N)) | ((val & 1u) << N)
                let mut lhs_str = String::new();
                {
                    let mut tmp = Printer::new(4, &mut lhs_str);
                    generate_expr(
                        &mut tmp,
                        map,
                        owner,
                        params.clone(),
                        inner_expr,
                        0,
                        has_model,
                    )?;
                }
                let mut rhs_str = String::new();
                {
                    let mut tmp = Printer::new(4, &mut rhs_str);
                    generate_expr(&mut tmp, map, owner, params, r, 0, has_model)?;
                }
                // Носитель может быть массивом слов (`[bit;N > 64]`): тогда пишется
                // Своё слово, а не весь вектор. Позиция берётся у
                // `bit_vector::bit_slot` - общего носителя с эталоном.
                let words = crate::generator::c::c_bits::words_of(inner_expr);
                let Some(text) = crate::generator::c::c_bits::write_bit(
                    &lhs_str,
                    words,
                    u64::try_from(*n).unwrap_or(u64::MAX),
                    &rhs_str,
                ) else {
                    return Err(unsupported(UnsupportedNode::BitBeyondVector, expr));
                };
                printer.print(&text);
                return Ok(());
            }
            // Бит-вектор шире 64 бит - массив слов, а массив в C не является изменяемым
            // lvalue: копирование и заполнение идут по словам.
            if let Some(count) = crate::generator::c::c_bits::words_of(l) {
                let mut lhs_str = String::new();
                {
                    let mut tmp = Printer::new(4, &mut lhs_str);
                    generate_expr(&mut tmp, map, owner, params.clone(), l, 0, has_model)?;
                }
                if crate::generator::c::c_bits::words_of(r) == Some(count) {
                    let mut rhs_str = String::new();
                    {
                        let mut tmp = Printer::new(4, &mut rhs_str);
                        generate_expr(&mut tmp, map, owner, params, r, 0, has_model)?;
                    }
                    printer.print(&crate::generator::c::c_bits::copy_words(
                        &lhs_str, &rhs_str, count,
                    ));
                    return Ok(());
                }
                if matches!(r.as_ref(), ExpressionNode::Number(_)) {
                    let mut rhs_str = String::new();
                    {
                        let mut tmp = Printer::new(4, &mut rhs_str);
                        generate_expr(&mut tmp, map, owner, params, r, 0, has_model)?;
                    }
                    printer.print(&crate::generator::c::c_bits::fill_words(
                        &lhs_str, count, &rhs_str,
                    ));
                    return Ok(());
                }
                return Err(unsupported(UnsupportedNode::WideBitVector(":="), expr));
            }
            // Обычное присваивание (право-ассоциативно: тот же prec не оборачивается)
            generate_expr(printer, map, owner, params.clone(), l, 1, has_model)?;
            printer.print(" = ");
            // Значение перечислимого типа печатается именем константы: здесь целевой
            // тип известен - он у переменной слева. Приём тот же, что у `st`
            // (`coerce_to`).
            if let Some(name) = enum_constant_for_assignment(l, r) {
                printer.print(&name);
                return Ok(());
            }
            generate_expr(printer, map, owner, params, r, 1, has_model)?;
        }

        // База - Выражение: печатается тем же печатником, что и прочие выражения,
        // поэтому `b.data[1]` выходит как `model->b.data[1]` без второго знания о
        // выборе базы.
        ExpressionNode::ArraySubscript(base, idx) => {
            let render = |node: &ExpressionNode| -> Result<String, Diagnostic> {
                let mut buf = String::new();
                let mut p = Printer::new(0, &mut buf);
                generate_expr(&mut p, map, owner, params.clone(), node, 0, has_model)?;
                Ok(buf)
            };
            let idx_str = render(idx)?;
            // У порта индекс - часть обращения к HAL, а не индексация значения: порт
            // значением не является, и печать `read_numeric(PORT, ud)[i]` не собирается
            // ни одним компилятором C.
            if let Some(call) = super::port_element::read(base, &idx_str, map, owner, has_model)? {
                printer.print(&call);
                return Ok(());
            }
            let base_str = render(base)?;
            printer.print(&format!("{base_str}[{idx_str}]"));
        }

        ExpressionNode::Variable(var_rc) => {
            let var = var_rc.borrow();
            let var_expr = if let VariableNode::Simple { upper, loc, .. } = &*var {
                // Локальные переменные (loc == Implicit) доступны по имени напрямую, а
                // не через model->name, даже если они принадлежат той же модели.
                if matches!(loc, crate::diagnostics::Location::Implicit) {
                    normalize_lowercase_snakecase(var.name().to_string())
                } else {
                    resolve_simple_var_in_context(var.name(), upper, &params, owner, map, has_model)
                        .map_or_else(
                            || resolve_variable_c_expr(&*var, &params, map, owner, has_model),
                            Ok,
                        )?
                }
            } else {
                resolve_variable_c_expr(&*var, &params, map, owner, has_model)?
            };
            printer.print(&var_expr);
        }

        // Именованное условие подставляется, как на ребре.
        //
        // Прежде печаталось имя макроса `COND_...`, которого цель **нигде не
        // определяет**: порождённый C не собирался при нулевом коде возврата `taktc`.
        // На ребре то же условие подставлялось выражением - то есть один и тот же
        // `cond` печатался двумя способами.
        ExpressionNode::Condition(cond_rc) => {
            let cond = cond_rc.borrow();
            let printed = crate::generator::c::c_expr::condition::generate_condition_expr(
                &cond.value,
                map,
                owner,
            )?;
            printer.print(&printed);
        }

        ExpressionNode::Function(fun_rc, args) => {
            let fun = fun_rc.borrow();
            generate_function_call(printer, map, owner, params, &*fun, args, has_model)?;
        }

        ExpressionNode::Initializer(elems) => {
            printer.print("{");
            for (i, elem) in elems.iter().enumerate() {
                if i > 0 {
                    printer.print(", ");
                }
                generate_expr(printer, map, owner, params.clone(), elem, 0, has_model)?;
            }
            printer.print("}");
        }

        ExpressionNode::Array(elems) => {
            printer.print("{");
            for (i, elem) in elems.iter().enumerate() {
                if i > 0 {
                    printer.print(", ");
                }
                generate_expr(printer, map, owner, params.clone(), elem, 0, has_model)?;
            }
            printer.print("}");
        }

        ExpressionNode::Cast(expr, typ) => {
            let model = map.raw_model_at(owner.name())?;
            let model = &*model.borrow();
            // было `unwrap_or_else(|| "int")` - невыразимый тип приведения молча
            // превращался в `(int)`, то есть приведение к другому типу, принятое
            // C-компилятором без замечаний.
            let type_c = c_type_or_diagnostic(typ, model, map.float_width(), "приведение типа")?;
            // Fixed-point: масштабирующее приведение, когда источник либо цель - q(m,
            // n). Сдвиги не используются (ловушка C11, UB `<<`).
            if matches!(typ, TypeNode::Fixed { .. })
                || super::fixed::fixed_of(map, owner, expr).is_some()
            {
                super::fixed::cast(printer, map, owner, params, expr, typ, &type_c, has_model)?;
            } else if crate::generator::mixed_sign::operand_type_expr(expr).is_some_and(|from| {
                // Сравниваются напечатанные типы: `duration` отображается в `uint32_t`,
                // и типы Takt при этом различны - признак 0361 такую запись не ловил. В
                // C лишнее приведение безвредно, но правило у трёх целей одно: у `rust`
                // та же печать есть `clippy::unnecessary_cast`, то есть отказ проверки.
                c_type_or_diagnostic(&from, model, map.float_width(), "приведение типа")
                    .is_ok_and(|from_c| from_c == type_c)
            }) {
                // Приведение к тому же типу опускается.
                generate_expr(printer, map, owner, params, expr, 13, has_model)?;
            } else {
                // Приводимое выражение оборачивается при prec < UNARY (13), то есть при
                // наличии бинарных операторов: (int)(a + b).
                printer.print("(").print(&type_c).print(")");
                generate_expr(printer, map, owner, params, expr, 13, has_model)?;
            }
        }

        // -- Неподдерживаемые --------------------------------------------------
        ExpressionNode::ArraySlice(_, _, _) => {
            return Err(unsupported(UnsupportedNode::ArraySlice, expr));
        }
        ExpressionNode::BitAccess(inner, member) => {
            match member {
                Member::Identifier(id) => {
                    // Доступ к полю структуры: inner.field - используем максимальный
                    // приоритет
                    generate_expr(printer, map, owner, params, inner, 15, has_model)?;
                    printer.print(&format!(".{}", id.name));
                }
                Member::Number(n) => {
                    // Битовый доступ к порту: (*main->read_bit)(PORT_X, N,
                    // main->userdata)
                    if let ExpressionNode::Variable(var_rc) = inner.as_ref() {
                        let var = var_rc.borrow();
                        if let VariableNode::Port {
                            direction,
                            name,
                            ty,
                            upper,
                            ..
                        } = &*var
                        {
                            let model_name =
                                if let Some(rc) = upper.as_ref().and_then(|w| w.upgrade()) {
                                    Name::from(rc)
                                } else {
                                    return Err(crate::generator::c::c_unresolved::refuse(
                                    expr.loc(),
                                    crate::generator::c::c_unresolved::UnresolvedNode::PortOwner(
                                        "чтение бита",
                                    ),
                                ));
                                };
                            let cls = PortClass::from_type(ty);
                            let variant = crate::generator::c::c_names::port_enum_variant(
                                &model_name,
                                name,
                                *direction,
                                crate::parser::ast::PortDirection::In,
                            );
                            let ptr = if has_model && !owner.name().eq(&map.root_name()) {
                                "main"
                            } else {
                                "model"
                            };
                            match cls {
                                // Разряд bit-порта адресуется самим вызовом (контракт
                                // 0533): прежде номер терялся, и `src.3` читал порт
                                // целиком.
                                PortClass::Bit => {
                                    printer.print(&crate::generator::c::c_port_call::read_bit(
                                        ptr,
                                        &variant,
                                        &n.to_string(),
                                    ));
                                }
                                PortClass::Numeric => {
                                    let read = crate::generator::c::c_port_call::read_numeric(
                                        ptr,
                                        &variant,
                                        crate::generator::c::c_port_call::SCALAR_INDEX,
                                    );
                                    printer.print(&format!("(({read} >> {n}) & 1u)"));
                                }
                                PortClass::Rational => {
                                    return Err(Diagnostic::error(
                                        Location::Codegen,
                                        "BitAccess на float-порт не поддерживается".to_string(),
                                    )
                                    .with_code("CC-001"));
                                }
                            }
                            return Ok(());
                        }
                    }
                    // Обычная переменная/выражение: `((inner >> N) & 1ull)`, а у
                    // массива слов (`[bit;N > 64]`) - сдвиг своего слова.
                    let mut base = String::new();
                    {
                        let mut tmp = Printer::new(4, &mut base);
                        generate_expr(&mut tmp, map, owner, params, inner, 0, has_model)?;
                    }
                    let words = crate::generator::c::c_bits::words_of(inner);
                    printer.print(&crate::generator::c::c_bits::read_bit(
                        &base,
                        words,
                        u64::try_from(*n).unwrap_or(u64::MAX),
                    ));
                }
            }
        }
        ExpressionNode::CodeBlock(_, _) => {
            return Err(unsupported(UnsupportedNode::CodeBlock, expr));
        }
        ExpressionNode::NamedFunctionBox(_, _) => {
            return Err(unsupported(UnsupportedNode::NamedFunction, expr));
        }
        ExpressionNode::List(_) => {
            return Err(unsupported(UnsupportedNode::ParameterList, expr));
        }
        ExpressionNode::Type(_) => {
            return Err(unsupported(UnsupportedNode::Type, expr));
        }
        ExpressionNode::Address(_, _) => {
            return Err(unsupported(UnsupportedNode::Address, expr));
        }
        // Анонимное обращение к ячейке: печатает только `c-hal` - цель `c` адресов не
        // знает по устройству.
        ExpressionNode::AnonPort(access) => {
            if !map.hal() {
                return Err(crate::generator::c::c_anon::refuse_plain_c());
            }
            printer.print(&crate::generator::c::c_anon::read(access));
        }
        ExpressionNode::Model(_) => {
            return Err(unsupported(UnsupportedNode::Model, expr));
        }
    }
    if wrap {
        printer.print(")");
    }
    Ok(())
}

/// Имя константы перечисления для присваивания `переменная := литерал`.
///
/// `None` - печатать правую часть обычным путём: слева не переменная перечислимого
/// типа, справа не число, значение не совпадает ни с одним вариантом либо владелец
/// перечисления недоступен.
///
/// Тип берётся у **переменной слева**, а перечисление ищется от её модели-владельца: у
/// неё же спрашивают тип и прочие печатники цели.
fn enum_constant_for_assignment(left: &ExpressionNode, right: &ExpressionNode) -> Option<String> {
    let ExpressionNode::Variable(var_rc) = left else {
        return None;
    };
    let ExpressionNode::Number(value) = right else {
        return None;
    };
    let var = var_rc.borrow();
    let (VariableNode::Simple { ty, upper, .. }
    | VariableNode::Const { ty, upper, .. }
    | VariableNode::Port { ty, upper, .. }) = &*var
    else {
        return None;
    };
    let scope = upper.as_ref().and_then(|w| w.upgrade())?;
    crate::generator::c::c_enum::constant_of(ty, *value, &scope)
}

/// Сравнение операндов разной знаковости в выражении.
///
/// `None` - печать прежняя. Раскрытие нужно только там, где общего типа нет (`u64`
/// против знакового): на 8/16/32 битах операнды продвигаются до `int`, и печать "как
/// есть" верна, а лишнее приведение изменило бы вывод корпуса.
fn mixed_sign_compare(
    l: &ExpressionNode,
    op: &str,
    r: &ExpressionNode,
    map: &CMap,
    owner: &Element,
    params: &[(String, TypeNode)],
    has_model: bool,
) -> Result<Option<String>, Diagnostic> {
    let crate::generator::mixed_sign::Plan::SignGuard { signed_is_left } =
        crate::generator::mixed_sign::plan(
            crate::generator::mixed_sign::operand_type_expr(l).as_ref(),
            crate::generator::mixed_sign::operand_type_expr(r).as_ref(),
        )
    else {
        return Ok(None);
    };
    let render = |node: &ExpressionNode| -> Result<String, Diagnostic> {
        let mut buf = String::new();
        let mut p = Printer::new(0, &mut buf);
        generate_expr(&mut p, map, owner, params.to_vec(), node, 0, has_model)?;
        Ok(buf)
    };
    let (lt, rt) = (render(l)?, render(r)?);
    let (signed, unsigned) = if signed_is_left {
        (lt.as_str(), rt.as_str())
    } else {
        (rt.as_str(), lt.as_str())
    };
    let neg = format!("({signed} < 0)");
    let same = if signed_is_left {
        format!("((uint64_t)({signed}) {op} ({unsigned}))")
    } else {
        format!("(({unsigned}) {op} (uint64_t)({signed}))")
    };
    let negative_wins = crate::generator::mixed_sign::negative_wins(op, signed_is_left);
    Ok(Some(if negative_wins {
        format!("({neg} || {same})")
    } else {
        format!("(!{neg} && {same})")
    }))
}
