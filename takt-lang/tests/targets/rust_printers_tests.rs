//! Печатники цели `rust`: сверка **текста** вывода по ветвям -.
//!
//! # Мера - ветвь, а не процент
//!
//! Каждый тест ниже закрывает ветвь печатника, дающую **иной текст**, и сверяет именно
//! текст. Проверять "собралось" здесь бесполезно: расхождение приоритетов C и Rust (`a
//! == b | c` - `1` в C, `false` в Rust) собирается прекрасно.
//!
//! Ожидания сняты **зондом** с фактического вывода, а не выведены из чтения кода.

use takt_lang::generator::GenerateOptions;

/// Порождает модуль Rust и возвращает его текст.
fn emit(tag: &str, source: &str) -> String {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0148_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание каталога");
    takt_lang::compile_to_rust(
        tag,
        source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .unwrap_or_else(|e| panic!("порождение Rust ({tag}): {} [{:?}]", e.message, e.code));
    std::fs::read_to_string(dir.join(format!("{tag}.rs"))).expect("чтение модуля")
}

/// Порождает и ожидает **отказ**; возвращает код диагностики.
fn emit_err(tag: &str, source: &str) -> String {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0148_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание каталога");
    match takt_lang::compile_to_rust(
        tag,
        source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    ) {
        Ok(_) => panic!("{tag}: ожидался отказ, а трансляция удалась"),
        Err(e) => e.code.unwrap_or_default(),
    }
}

/// Модель-обёртка: тело `always` и условие ребра задаются параметрами.
fn model(decls: &str, body: &str, guard: &str) -> String {
    format!(
        "out o: bit; {decls}
model M {{ start S {{ always {{ o := 1; {body} }} ref T: {guard}; }} state T; }}
start Main = M;"
    )
}

/// Проверяет, что вывод содержит строку, и печатает её окрестность при отказе.
#[track_caller]
fn assert_has(text: &str, needle: &str, why: &str) {
    assert!(
        text.contains(needle),
        "{why}\nожидалось вхождение: {needle}\nфактический вывод:\n{}",
        text.lines()
            .filter(|l| !l.trim_start().starts_with("//") && !l.trim().is_empty())
            .collect::<Vec<_>>()
            .join("\n")
    );
}

// -- rust_cond: сравнения и логика --------------------------------------------

/// **Каждый** бинарный узел условия заключён в скобки.
///
/// Это не стиль, а защита от **тихого** дефекта: приоритеты C и Rust расходятся, `a ==
/// b | c` даёт в C `1`, а в Rust `false`. На целых операндах расхождение не даёт ни
/// отказа, ни предупреждения - только другой результат.
#[test]
fn every_binary_condition_node_is_parenthesised() {
    let text = emit(
        "parens",
        &model(
            "var a: u8 := 0; var b: u8 := 0;",
            "",
            "a < b & a >= b | !(a = b)",
        ),
    );
    assert_has(
        &text,
        "((shared.a < shared.b) & (shared.a >= shared.b)) | (!(shared.a == shared.b))",
        "каждый бинарный узел обязан быть в скобках: приоритеты C и Rust \
         расходятся МОЛЧА",
    );
}

/// Реляционные и арифметические операции в условии переводятся один в один.
#[test]
fn relational_and_arithmetic_operators_map_directly() {
    let text = emit(
        "relops",
        &model("var a: u8 := 0; var b: u8 := 0;", "", "a + b > a - b"),
    );
    assert_has(
        &text,
        "(shared.a + shared.b) > (shared.a - shared.b)",
        "`+`/`-`/`>` в условии печатаются как есть",
    );
}

/// `=` в условии - **равенство** (`==`), а не присваивание.
#[test]
fn equality_in_condition_is_not_assignment() {
    let text = emit("eqcond", &model("var a: u8 := 0;", "", "a = 3"));
    assert_has(
        &text,
        "shared.a == 3",
        "`=` в УСЛОВИИ — равенство: ради этого различия печатник условий и \
         отделён от печатника выражений",
    );
}

/// Вариант перечисления печатается как `Тип::Вариант`.
#[test]
fn enum_variant_prints_qualified() {
    let text = emit(
        "enumv",
        &model(
            "enum Mode { Idle, Run } var m: Mode := Idle;",
            "",
            "m = Run",
        ),
    );
    assert_has(&text, "shared.m == Mode::Run", "вариант — `Тип::Вариант`");
}

/// Индексация массива получает `as usize`: в Rust индекс - `usize`.
///
/// Ожидание исправлено: было `shared.xs[self.i as usize]`, то есть
/// **невалидный** Rust - `rustc` отвечал `E0609: no field 'i' on type '&mut ...'`.
/// Причина: переменная, использованная только индексом, не считалась
/// использованной, поэтому не попадала в общую структуру, а печатник ссылался на
/// `self`. Тест это закреплял: он сверял текст, а проверка цели гоняет только корпус,
/// где такой формы нет.
#[test]
fn array_subscript_casts_index_to_usize() {
    let text = emit(
        "arridx",
        &model(
            "var xs: [u8;4] := {1, 2, 3, 4}; var i: u8 := 0;",
            "",
            "xs[i] > 0",
        ),
    );
    assert_has(
        &text,
        "shared.xs[shared.i as usize] > 0",
        "индекс массива обязан приводиться к `usize` и браться из общей структуры",
    );
}

/// Доступ к биту разворачивается в маску, а не в `.N`.
#[test]
fn bit_access_expands_to_mask() {
    let text = emit("bitacc", &model("var f: u8 := 0;", "", "f.2 = 1"));
    assert_has(
        &text,
        "((shared.f >> 2) & 1) != 0",
        "битового доступа `x.N` в Rust нет — печатается маска",
    );
}

/// Вызов локальной функции в условии печатается прямым вызовом.
#[test]
fn local_function_call_in_condition() {
    let text = emit(
        "fncall",
        "out o: bit; var a: u8 := 0;
fn twice(x: u8) -> u8 { return x + x; }
model M { start S { always { o := 1; } ref T: twice(a) > 3; } state T; }
start Main = M;",
    );
    assert_has(
        &text,
        "twice(shared.a) > 3",
        "локальная функция — прямой вызов",
    );
}

/// Внешняя функция идёт через HAL: аппаратного аналога у неё нет.
#[test]
fn external_function_call_goes_through_hal() {
    let text = emit(
        "extfn",
        "out o: bit; extern fn sense() -> u8; var a: u8 := 0;
model M { start S { always { o := 1; a := sense(); } ref T: a > 3; } state T; }
start Main = M;",
    );
    assert_has(&text, "hal.sense()", "`extern fn` — метод HAL");
}

// -- rust_cond: булев операнд ----------------------------------

/// `bit` сравнивается с числом - и это **не** `bool == 1`.
///
/// Регресс исправления. В Takt `bit` - целое-однобитное, и `btn = 1` - естественная запись; в
/// Rust `bit` отображается на `bool`, поэтому дословный перевод давал
/// `hal.read_bit(...) == 1` - **ошибку типов**: модуль не компилировался вовсе.
#[test]
fn bit_port_compared_with_number_yields_plain_bool() {
    let text = emit(
        "bitnum",
        "in btn: bit; out o: bit;
model M { start S { always { o := 1; } ref T: btn = 1; } state T; }
start Main = M;",
    );
    assert_has(
        &text,
        "if hal.read_bit(InBitPort::Btn) {",
        "`btn = 1` обязан дать сам операнд: `bool == 1` — ошибка типов",
    );
    assert!(
        !text.contains("read_bit(InBitPort::Btn) == 1"),
        "дословный перевод `bool == 1` не компилируется:\n{text}"
    );
}

/// `bit = 0` даёт отрицание, а не `== 0`.
#[test]
fn bit_compared_with_zero_yields_negation() {
    let text = emit(
        "bitzero",
        "in btn: bit; out o: bit;
model M { start S { always { o := 1; } ref T: btn = 0; } state T; }
start Main = M;",
    );
    assert_has(
        &text,
        "if !hal.read_bit(InBitPort::Btn) {",
        "`= 0` → отрицание",
    );
}

/// `bit != 1` - тоже отрицание: `!=` переворачивает знак.
#[test]
fn bit_not_equal_flips_the_sign() {
    let text = emit(
        "bitne",
        "in btn: bit; out o: bit;
model M { start S { always { o := 1; } ref T: btn != 1; } state T; }
start Main = M;",
    );
    assert_has(
        &text,
        "if !hal.read_bit(InBitPort::Btn) {",
        "`!= 1` → отрицание",
    );
}

/// Сравнение с булевым литералом не даёт `x == true`.
///
/// Форма компилируется, но валит `clippy::bool_comparison`, то есть не проходит
/// политику `-D warnings` проверки цели.
#[test]
fn bit_compared_with_bool_literal_avoids_bool_comparison_lint() {
    let text = emit(
        "bitbool",
        "in btn: bit; out o: bit;
model M { start S { always { o := 1; } ref T: btn = true; } state T; }
start Main = M;",
    );
    assert!(
        !text.contains("== true") && !text.contains("== false"),
        "`x == true` валит clippy::bool_comparison под -D warnings:\n{text}"
    );
    assert_has(
        &text,
        "if hal.read_bit(InBitPort::Btn) {",
        "`= true` → сам операнд",
    );
}

/// Литерал слева работает так же: `1 = btn`.
#[test]
fn literal_on_the_left_is_handled_too() {
    let text = emit(
        "bitleft",
        "in btn: bit; out o: bit;
model M { start S { always { o := 1; } ref T: 1 = btn; } state T; }
start Main = M;",
    );
    assert_has(
        &text,
        "if hal.read_bit(InBitPort::Btn) {",
        "литерал слева — та же форма",
    );
}

/// **Контрпример:** у целого операнда сравнение с числом остаётся сравнением.
///
/// Без него "починка", сводящая к операнду всё подряд, прошла бы тесты выше и сломала
/// бы обычные сравнения.
#[test]
fn integer_compared_with_number_stays_a_comparison() {
    let text = emit("intcmp", &model("var a: u8 := 0;", "", "a = 1"));
    assert_has(
        &text,
        "shared.a == 1",
        "у ЦЕЛОГО операнда сравнение с числом обязано остаться сравнением",
    );
}

/// Голый `bit` в условии печатается как есть - приведения не нужно.
#[test]
fn bare_bit_condition_needs_no_coercion() {
    let text = emit(
        "bitbare",
        "in btn: bit; out o: bit;
model M { start S { always { o := 1; } ref T: btn; } state T; }
start Main = M;",
    );
    assert_has(
        &text,
        "if hal.read_bit(InBitPort::Btn) {",
        "голый `bit` — уже bool",
    );
}

/// Целое в позиции условия приводится к `bool` через `!= 0`.
#[test]
fn integer_condition_is_coerced_to_bool() {
    let text = emit("intbool", &model("var a: u8 := 0;", "", "a & a"));
    assert_has(
        &text,
        "(shared.a != 0) & (shared.a != 0)",
        "в Rust `u8` условием не является: нужно `!= 0`",
    );
}

// -- rust_stmt: операторы -----------------------------------------------------

/// `if`/`else` печатается ветвлением Rust.
#[test]
fn if_else_prints_branches() {
    let text = emit(
        "ifelse",
        &model(
            "var a: u8 := 0;",
            "if a > 1 { a := 1; } else { a := 2; }",
            "a = 1",
        ),
    );
    assert_has(&text, "if shared.a > 1 {", "условие `if`");
    assert_has(&text, "} else {", "ветвь `else`");
}

/// `while` печатается циклом Rust.
#[test]
fn while_prints_loop_with_condition() {
    let text = emit(
        "whilec",
        &model("var a: u8 := 0;", "while a < 3 { a := a + 1; }", "a = 3"),
    );
    assert_has(&text, "while shared.a < 3 {", "`while` — цикл с условием");
}

/// `loop` печатается безусловным циклом, `break` - выходом.
#[test]
fn loop_and_break_print_directly() {
    let text = emit(
        "loopc",
        &model(
            "var a: u8 := 0;",
            "loop { a := a + 1; if a > 2 { break; } }",
            "a > 2",
        ),
    );
    assert_has(&text, "loop {", "`loop` — безусловный цикл");
    assert_has(&text, "break;", "`break` — выход");
}

/// C-подобный `for` понижается в `while`.
///
/// Пиннинг **формы**: в Rust нет трёхчастного `for`, и понижение - решение печатника, а
/// не свойство языка. Инициализатор выносится перед циклом, шаг - в конец тела.
#[test]
fn c_style_for_lowers_to_while() {
    let text = emit(
        "forc",
        &model(
            "var a: u8 := 0;",
            "for var i: u8 := 0; i < 3; i := i + 1 { a := a + 1; }",
            "a >= 3",
        ),
    );
    assert_has(
        &text,
        "while i < 3 {",
        "трёхчастный `for` понижается в `while`",
    );
}

// -- rust_stmt: присваивания и переполнение -----------------------------------

/// Арифметика целых печатается `wrapping_*`, а не операторами.
///
/// Семантика переполнения нормирована: беззнаковое - обёртка `mod 2ⁿ`. Свёртка в `+=`
/// для таких узлов **отключена**: `+=` паникует в debug-сборке, то есть дала бы иную
/// семантику, чем эталон.
#[test]
fn integer_arithmetic_uses_wrapping_and_not_compound_assignment() {
    let text = emit(
        "wrap",
        &model(
            "var a: u8 := 0;",
            "a := a + 1; a := a - 1; a := a * 2;",
            "a > 100",
        ),
    );
    for (needle, why) in [
        (
            "shared.a = shared.a.wrapping_add(1)",
            "сложение — wrapping_add",
        ),
        (
            "shared.a = shared.a.wrapping_sub(1)",
            "вычитание — wrapping_sub",
        ),
        (
            "shared.a = shared.a.wrapping_mul(2)",
            "умножение — wrapping_mul",
        ),
    ] {
        assert_has(&text, needle, why);
    }
    assert!(
        !text.contains("+= 1"),
        "свёртка в `+=` для wrapping-узлов отключена: `+=` паникует в debug:\n{text}"
    );
}

/// Умножение fixed-point печатается сдвигом через расширенный тип.
///
/// Арифметика `q(m, n)` побитово едина у симулятора и всех целей: произведение
/// считается в широком типе и сдвигается на дробную часть.
#[test]
fn fixed_point_multiplication_widens_then_shifts() {
    let text = emit(
        "qmul",
        &model(
            "var q1: q(8, 8) := 1.0; var done: bit := 0;",
            "q1 := q1 * q1;",
            "done = 1",
        ),
    );
    assert_has(
        &text,
        "as i128 * ",
        "произведение q считается в расширенном типе",
    );
    assert_has(&text, ">> 8", "и сдвигается на число дробных бит");
}

/// Запись в элемент массива идёт по индексу с `as usize`.
#[test]
fn array_element_assignment_indexes_with_usize() {
    let text = emit(
        "arrset",
        &model(
            "var xs: [u8;4] := {1, 2, 3, 4}; var i: u8 := 0; var done: bit := 0;",
            "xs[i] := 7;",
            "done = 1",
        ),
    );
    assert_has(
        &text,
        "shared.xs[shared.i as usize] = 7",
        "запись элемента массива: индекс — из общей структуры (см. соседний тест)",
    );
}

/// Приведение `as` печатается приведением Rust.
#[test]
fn cast_prints_as_expression() {
    let text = emit(
        "castc",
        &model(
            "var a: u8 := 0; var b: u16 := 0;",
            "b := a as u16;",
            "b > 3",
        ),
    );
    assert_has(&text, "as u16", "приведение печатается `as`");
}

// -- Порты --------------------------------------------------------------------

/// Чтение входного порта - метод HAL, запись выходного - тоже.
///
/// Присваивание `o := 1` порту типа `bit` **приводит** литерал к `bool`. Именно эта
/// готовая коэрция и была образцом для исправления: сравнение обязано вести себя так же, а не
/// печатать `bool == 1`.
#[test]
fn ports_are_read_and_written_through_hal() {
    let text = emit(
        "portio",
        "in btn: bit; out led: bit; var a: u8 := 0;
model M { start S { always { led := 1; a := a + 1; } ref T: btn; } state T; }
start Main = M;",
    );
    assert_has(
        &text,
        "hal.write_bit(OutBitPort::Led, true)",
        "запись порта — метод HAL; литерал приведён к bool",
    );
    assert_has(
        &text,
        "hal.read_bit(InBitPort::Btn)",
        "чтение порта — метод HAL",
    );
}

// -- Диагностики: границы цели ------------------------------------------------

/// Поле структуры целью `rust` **транслируется**.
///
/// Прежде этот тест утверждал обратное - "поля структур не транслируются, отказ
/// `RS-011`", - и **закреплял дефект**: структуры цель переводит с, а отказ приходил от
/// `expression_type`, считавшего **любой** доступ через точку логическим (`x.7` и
/// `s.field` неразличимо).
#[test]
fn struct_member_access_is_translated() {
    let text = emit(
        "structm",
        "out o: bit; struct P { x: u8, y: u8 } var p: P := {1, 2};
model M { start S { always { o := 1; p.x := 5; } ref T: p.y > 0; } state T; }
start Main = M;",
    );
    assert_has(
        &text,
        "p.x = 5;",
        "запись в поле структуры печатается доступом через точку",
    );
    assert_has(&text, "p.y > 0", "чтение поля — тоже доступ через точку");
}

/// Встроенная функция в условии перехода не транслируется - тоже громко.
#[test]
fn builtin_function_in_condition_is_refused_loudly() {
    let code = emit_err(
        "builtinc",
        "out o: bit; var a: u8 := 0;
model M { start S { always { o := 1; } ref T: min(a, 1) > 0; } state T; }
start Main = M;",
    );
    // Код - `RS-020`, а не `RS-011`, как гласит комментарий у ветви
    // `FunctionDefinitionNode::Builtin` в `rust_cond.rs`: до печатника условие с
    // встроенной функцией не доходит - его отвергает более ранняя проверка. Пиннинг
    // снят зондом, а не выведен из чтения кода: расхождение комментария с поведением -
    // ровно то, что тест обязан исправлениеировать.
    assert_eq!(
        code, "RS-020",
        "встроенная функция в условии отвергается громко"
    );
}
