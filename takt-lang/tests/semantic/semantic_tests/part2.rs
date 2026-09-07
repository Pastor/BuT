//! Интеграционные тесты семантики, часть 2.
//!
//! Хелперы и импорты - из родителя через `use super::*`.

use super::*;

/// `tests/data/semantic/valid/type_aliases.takt` - псевдонимы типов разрешаются.
#[test]
fn example_type_aliases_is_valid() {
    let node = build_file("tests/data/semantic/valid/type_aliases.takt").unwrap();
    assert!(
        node.types.contains_key("Byte"),
        "тип Byte должен быть объявлен"
    );
    assert!(
        node.types.contains_key("Word"),
        "тип Word должен быть объявлен"
    );
    assert!(
        node.search_var("counter").is_some(),
        "переменная counter должна быть найдена"
    );
    assert!(
        node.search_var("STATUS").is_some(),
        "порт STATUS должен быть найден"
    );
}

/// `tests/data/semantic/valid/conditions.takt` - все условия разрешаются.
#[test]
fn example_conditions_is_valid() {
    let node = build_file("tests/data/semantic/valid/conditions.takt").unwrap();
    assert!(
        node.conditions.contains_key("AlwaysTrue"),
        "условие AlwaysTrue должно быть"
    );
    assert!(
        node.conditions.contains_key("AlwaysFalse"),
        "условие AlwaysFalse должно быть"
    );
    assert!(
        node.conditions.contains_key("IsFlagSet"),
        "условие IsFlagSet должно быть"
    );
    assert!(
        node.conditions.contains_key("Negated"),
        "условие negated должно быть"
    );
    assert!(
        node.conditions.contains_key("Grouped"),
        "условие grouped должно быть"
    );
}

/// `tests/data/semantic/valid/composition.takt` - компоновка моделей корректна.
#[test]
fn example_composition_is_valid() {
    let node = build_file("tests/data/semantic/valid/composition.takt").unwrap();
    // Модели Step1, Step2, Step3 должны быть в контексте
    assert!(
        node.search_model("Step1").is_some(),
        "Step1 должна быть найдена"
    );
    assert!(
        node.search_model("Step2").is_some(),
        "Step2 должна быть найдена"
    );
    assert!(
        node.search_model("Step3").is_some(),
        "Step3 должна быть найдена"
    );
    // Состояния Sequential, Parallel, Combined должны быть Implement-узлами
    assert!(
        node.states.contains_key("Sequential"),
        "состояние Sequential должно быть"
    );
    assert!(
        node.states.contains_key("Parallel"),
        "состояние Parallel должно быть"
    );
    assert!(
        node.states.contains_key("Combined"),
        "состояние Combined должно быть"
    );
}

/// `tests/data/semantic/invalid/missing_var.takt` - должна возникнуть ошибка.
#[test]
fn example_missing_var_is_error() {
    let result = build_file("tests/data/semantic/invalid/missing_var.takt");
    assert!(
        result.is_err(),
        "missing_var.takt должен давать ошибку семантики"
    );
}

/// `tests/data/semantic/invalid/unknown_model.takt` - должна возникнуть ошибка.
#[test]
fn example_unknown_model_is_error() {
    let result = build_file("tests/data/semantic/invalid/unknown_model.takt");
    assert!(
        result.is_err(),
        "unknown_model.takt должен давать ошибку семантики"
    );
}

/// `tests/data/semantic/invalid/double_next.takt` - должна возникнуть ошибка.
#[test]
fn example_double_next_is_error() {
    let result = build_file("tests/data/semantic/invalid/double_next.takt");
    assert!(
        result.is_err(),
        "double_next.takt должен давать ошибку семантики"
    );
}

/// `tests/data/semantic/invalid/dangling_ref.takt` - должна возникнуть ошибка.
#[test]
fn example_dangling_ref_is_error() {
    let result = build_file("tests/data/semantic/invalid/dangling_ref.takt");
    assert!(
        result.is_err(),
        "dangling_ref.takt должен давать ошибку семантики"
    );
}

// --- Тесты импорта std.takt ----------------------------------------------------

/// `import "std.takt"` из стандартной библиотеки подключается без ошибок.
#[test]
fn std_but_import_works() {
    let src = r#"import "std.takt";"#;
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let root = construct_model(&ast, None, &["tests/data/include".to_string()]);
    assert!(
        root.is_ok(),
        "импорт std.takt должен завершаться без ошибок"
    );
    let root = root.unwrap();
    // Нормализованное имя файла std.takt -> Std
    assert!(
        root.borrow().search_model("Std").is_some(),
        "модель Std должна быть зарегистрирована после импорта std.takt"
    );
}

// --- Тесты выборочного импорта (ImportDefine::Rename) ------------------------

/// Вспомогательная функция: строит модель из inline-кода с путём поиска shared.takt.
fn build_with_includes(
    src: &str,
) -> Result<takt_lang::semantic::ModelNode, takt_lang::diagnostics::Diagnostic> {
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    construct_model(&ast, None, &["tests/data/include".to_string()]).map(|m| m.take())
}

/// `import { SharedModel } from "shared.takt"` - модель доступна под оригинальным
/// именем.
#[test]
fn rename_import_model_no_alias() {
    let node = build_with_includes(
        r#"import { SharedModel } from "shared.takt"; start E = SharedModel { }"#,
    )
    .unwrap();
    assert!(
        node.search_model("SharedModel").is_some(),
        "SharedModel должна быть доступна после импорта"
    );
}

/// `import { SharedModel as M } from "shared.takt"` - модель доступна под псевдонимом
/// M.
#[test]
fn rename_import_model_with_alias() {
    let node =
        build_with_includes(r#"import { SharedModel as M } from "shared.takt"; start E = M { }"#)
            .unwrap();
    assert!(
        node.search_model("M").is_some(),
        "модель должна быть доступна под псевдонимом M"
    );
    assert!(
        node.search_model("SharedModel").is_none(),
        "оригинальное имя SharedModel не должно быть видно"
    );
}

/// `import { SharedType } from "shared.takt"` - тип-псевдоним импортируется в контекст.
#[test]
fn rename_import_type() {
    let node = build_with_includes(
        r#"import { SharedType } from "shared.takt"; var x: SharedType := 0; start S;"#,
    )
    .unwrap();
    assert!(
        node.types.contains_key("SharedType"),
        "тип SharedType должен быть в контексте после импорта"
    );
    assert!(
        node.search_var("x").is_some(),
        "переменная x должна быть объявлена"
    );
}

/// `import { SharedType as ST }` - тип импортируется под псевдонимом.
#[test]
fn rename_import_type_with_alias() {
    let node = build_with_includes(
        r#"import { SharedType as ST } from "shared.takt"; var x: ST := 0; start S;"#,
    )
    .unwrap();
    assert!(
        node.types.contains_key("ST"),
        "псевдоним ST должен быть в контексте"
    );
    assert!(
        !node.types.contains_key("SharedType"),
        "оригинальное имя SharedType не должно быть видно"
    );
}

/// `import { shared_var }` - переменная импортируется в контекст.
#[test]
fn rename_import_variable() {
    let node =
        build_with_includes(r#"import { shared_var } from "shared.takt"; start S;"#).unwrap();
    assert!(
        node.search_var("shared_var").is_some(),
        "переменная shared_var должна быть в контексте после импорта"
    );
}

/// `import { shared_var as sv }` - переменная импортируется под псевдонимом.
#[test]
fn rename_import_variable_with_alias() {
    let node =
        build_with_includes(r#"import { shared_var as sv } from "shared.takt"; start S;"#).unwrap();
    assert!(
        node.search_var("sv").is_some(),
        "переменная должна быть видна под псевдонимом sv"
    );
    assert!(
        node.search_var("shared_var").is_none(),
        "оригинальное имя shared_var не должно быть видно"
    );
}

/// `import { SharedCond }` - условие импортируется в контекст.
#[test]
fn rename_import_condition() {
    let node = build_with_includes(
        r#"import { SharedCond } from "shared.takt"; start S { ref E: SharedCond; } state E;"#,
    )
    .unwrap();
    assert!(
        node.conditions.contains_key("SharedCond"),
        "условие SharedCond должно быть в контексте"
    );
}

/// Импорт нескольких символов в одном выражении.
#[test]
fn rename_import_multiple_symbols() {
    let node = build_with_includes(
        r#"import { SharedModel as M, SharedType as ST, shared_var as sv } from "shared.takt"; start E = M { }"#,
    ).unwrap();
    assert!(node.search_model("M").is_some(), "M должна быть видна");
    assert!(node.types.contains_key("ST"), "ST должен быть виден");
    assert!(node.search_var("sv").is_some(), "sv должна быть видна");
}

/// Импорт несуществующего символа - ошибка.
#[test]
fn rename_import_missing_symbol_is_error() {
    let result = build_with_includes(r#"import { NonExistent } from "shared.takt"; start S;"#);
    assert!(
        result.is_err(),
        "импорт несуществующего символа должен давать ошибку"
    );
    let err = result.unwrap_err();
    assert!(
        err.message.contains("NonExistent"),
        "сообщение должно содержать имя символа: {}",
        err.message
    );
}

/// Дублирующееся имя при импорте с псевдонимом - ошибка.
#[test]
fn rename_import_duplicate_alias_is_error() {
    // Объявляем модель M локально, затем пробуем импортировать SharedModel as M
    let result = build_with_includes(
        r#"model M { start S; } import { SharedModel as M } from "shared.takt"; start E = M { }"#,
    );
    assert!(result.is_err(), "дублирующееся имя M должно давать ошибку");
}

/// `example_rename_import.takt` - файл-пример строится без ошибок.
#[test]
fn example_rename_import_is_valid() {
    let src = std::fs::read_to_string("tests/data/semantic/valid/rename_import.takt")
        .expect("файл rename_import.takt не найден");
    let (ast, _) = parse(&src, 0).expect("ошибка разбора файла");
    let node = construct_model(&ast, None, &["tests/data/include".to_string()])
        .map(|m| m.take())
        .unwrap();
    // ST - псевдоним SharedType, M - псевдоним SharedModel
    assert!(
        node.types.contains_key("ST"),
        "тип ST должен быть импортирован"
    );
    assert!(
        node.search_model("M").is_some(),
        "модель M должна быть импортирована"
    );
}

// --- Тесты проверки типа и границ массива -------------------------------------

/// ArraySubscript на переменной с корректным индексом - строится без ошибок.
#[test]
fn array_subscript_valid_index() {
    let node = build("var buf: [bit;8] := 0; var x: bit := buf[0];");
    assert!(node.search_var("x").is_some());
}

/// ArraySubscript: последний допустимый индекс (size-1) - ок.
#[test]
fn array_subscript_last_valid_index() {
    let node = build("var buf: [bit;8] := 0; var x: bit := buf[7];");
    assert!(node.search_var("x").is_some());
}

/// ArraySubscript: индекс равный размеру массива - ошибка (out of bounds).
#[test]
fn array_subscript_out_of_bounds_is_error() {
    let (ast, _) = parse("var buf: [bit;8] := 0; var x: bit := buf[8]; start S;", 0).unwrap();
    let result = construct_model(&ast, None, &[]);
    assert!(
        result.is_err(),
        "индекс buf[8] должен давать ошибку для массива размером 8"
    );
}

/// ArraySubscript: отрицательный индекс - ошибка.
#[test]
fn array_subscript_negative_index_is_error() {
    // Отрицательные индексы не поддерживаются
    let (ast, _) = parse("var buf: [bit;8] := 0; var x: bit := buf[-1]; start S;", 0).unwrap();
    let result = construct_model(&ast, None, &[]);
    assert!(result.is_err(), "отрицательный индекс должен давать ошибку");
}

/// ArraySubscript на переменной с типом Bit - ошибка (не массив).
#[test]
fn array_subscript_on_bit_is_error() {
    let (ast, _) = parse("var flag: bit := false; var x: bit := flag[0]; start S;", 0).unwrap();
    let result = construct_model(&ast, None, &[]);
    assert!(
        result.is_err(),
        "индексирование Bit-переменной должно давать ошибку"
    );
    let err = result.unwrap_err();
    assert!(
        err.message.contains("flag"),
        "сообщение должно содержать имя переменной: {}",
        err.message
    );
}

/// `example_array_access.takt` - файл с корректными операциями над массивом строится
/// без ошибок.
#[test]
fn example_array_access_is_valid() {
    let result = build_file("tests/data/semantic/valid/array_access.takt").unwrap();
    assert!(result.search_var("bit0").is_some());
    assert!(result.search_var("bit7").is_some());
}

/// `example_array_out_of_bounds.takt` - должна возникнуть ошибка.
#[test]
fn example_array_out_of_bounds_is_error() {
    let result = build_file("tests/data/semantic/invalid/array_out_of_bounds.takt");
    assert!(
        result.is_err(),
        "array_out_of_bounds.takt должен давать ошибку"
    );
}

/// `example_non_array_subscript.takt` - должна возникнуть ошибка.
#[test]
fn example_non_array_subscript_is_error() {
    let result = build_file("tests/data/semantic/invalid/non_array_subscript.takt");
    assert!(
        result.is_err(),
        "non_array_subscript.takt должен давать ошибку"
    );
}

/// ArraySubscript с индексом-переменной - строится без ошибок.
#[test]
fn array_subscript_variable_index() {
    let node = build("var buf: [bit;8] := 0; var i: bit := 0; var x: bit := buf[i];");
    assert!(node.search_var("x").is_some());
}

/// ArraySubscript с индексом-переменной в условии cond - строится без ошибок.
#[test]
fn array_subscript_variable_index_in_cond() {
    let node = build("var buf: [bit;8] := 0; var i: bit := 0; cond C = buf[i];");
    assert!(node.search_cond("C").is_some());
}

/// `inout` порт объявляется без ошибок и виден в семантическом дереве.
#[test]
fn inout_port_is_valid() {
    let node = build("inout sensor: bit at 0x100:0; start S;");
    assert!(node.search_var("sensor").is_some());
}

/// `while cond { body }` - синоним loop, строится без ошибок.
#[test]
fn while_loop_is_valid() {
    let node = build("var x: bit := 0; start S { always { while x { x := 0; } } }");
    assert!(node.search_var("x").is_some());
}

/// `example_rename_import_missing.takt` - должна возникнуть ошибка.
#[test]
fn example_rename_import_missing_is_error() {
    let src = std::fs::read_to_string("tests/data/semantic/invalid/rename_import_missing.takt")
        .expect("файл не найден");
    let (ast, _) = parse(&src, 0).expect("ошибка разбора");
    let result = construct_model(&ast, None, &["tests/data/include".to_string()]).map(|m| m.take());
    assert!(
        result.is_err(),
        "импорт несуществующего символа должен давать ошибку"
    );
}

/// После импорта `std.takt` объявленные ею типы доступны внутри импортированной модели.
///
/// Типы `u8` и `u16` здесь не годятся: библиотека объявляла их псевдонимами `[bit; N]` -
/// наследие эпохи до встроенных целых. С занять имя встроенного типа нельзя (`SE-107`),
/// объявления сняты, и осталось `u128` - встроенным он не является, поэтому объявлять
/// его законно.
#[test]
fn std_takt_contains_declared_types() {
    let src = r#"import "std.takt";"#;
    let (ast, _) = parse(src, 0).unwrap();
    let root = construct_model(&ast, None, &["tests/data/include".to_string()]).unwrap();
    let std_model = root.borrow().search_model("Std").unwrap();
    assert!(
        std_model.borrow().types.contains_key("u128"),
        "std.takt должен содержать тип u128"
    );
    assert!(
        !std_model.borrow().types.contains_key("u8"),
        "u8 — встроенный тип: библиотека объявлять его не вправе (SE-107)"
    );
}

// --- Тесты resolve_statement и named blocks ----------------------------------

/// Model-level `always` block с известной переменной -> блок разрешается.
#[test]
fn model_always_block_with_known_var_resolves() {
    let node = build("var led: bit := false; always { led := led; } start S;");
    let nb = node.get_named_block("always").expect("always должен быть");
    let stmt = nb.statement().expect("оператор должен быть");
    assert!(
        !matches!(stmt, StatementNode::Unresolved(_)),
        "always должен быть разрешён: {:?}",
        stmt
    );
}

/// State-level `enter` block -> присутствует в state.named_blocks.
#[test]
fn state_enter_block_is_populated() {
    let node = build("var x: bit := false; start S { enter { x := x; } }");
    let state = node.states.get("S").unwrap();
    assert!(
        state.get_named_block("enter").is_some(),
        "enter должен быть в state.named_blocks"
    );
}

/// State-level `enter` с известной переменной -> разрешается (не Unresolved).
#[test]
fn state_enter_block_resolves() {
    let node = build("var x: bit := false; start S { enter { x := x; } }");
    let state = node.states.get("S").unwrap();
    let enter = state.get_named_block("enter").expect("enter не найден");
    let stmt = enter.statement().expect("оператор должен быть");
    assert!(
        !matches!(stmt, StatementNode::Unresolved(_)),
        "enter должен быть разрешён: {:?}",
        stmt
    );
}

/// State-level `enter` + `exit` -> оба присутствуют в named_blocks состояния.
#[test]
fn state_enter_exit_blocks_both_present() {
    let node = build("var x: bit := false; start S { enter { x := x; } exit { x := x; } }");
    let state = node.states.get("S").unwrap();
    assert!(
        state.get_named_block("enter").is_some(),
        "enter отсутствует"
    );
    assert!(state.get_named_block("exit").is_some(), "exit отсутствует");
}

/// `if cond { ... }` в named block разрешается через Statement::Block.
#[test]
fn state_named_block_if_resolves() {
    let node = build("var f: bit := false; start S { always { if f { f := f; } } }");
    let state = node.states.get("S").unwrap();
    let always = state.get_named_block("always").expect("always не найден");
    let stmt = always.statement().expect("оператор должен быть");
    // Блок разрешён - не остаётся как Unresolved на верхнем уровне
    assert!(
        !matches!(stmt, StatementNode::Unresolved(_)),
        "always должен быть разрешён: {:?}",
        stmt
    );
}

/// Named blocks вложенной модели разрешаются в контексте вложенной модели.
#[test]
fn nested_model_named_blocks_resolve_with_own_context() {
    let node = build(
        "model Inner { var t: bit := false; start On { enter { t := t; } } state Off; } \
         start Root = Inner { }",
    );
    // Находим вложенную модель Inner
    let inner = node.search_model("Inner").expect("Inner не найдена");
    let inner = inner.borrow();
    let state = inner.states.get("On").expect("состояние On не найдено");
    let enter = state
        .get_named_block("enter")
        .expect("enter не найден в On");
    let stmt = enter.statement().expect("оператор должен быть");
    assert!(
        !matches!(stmt, StatementNode::Unresolved(_)),
        "enter во Inner::On должен быть разрешён"
    );
}

/// `return x;` в always block разрешается в Statement::Block([Return(...)]).
#[test]
fn return_statement_in_named_block_resolves() {
    let node = build("var x: bit := false; always { return x; } start S;");
    let nb = node.get_named_block("always").expect("always не найден");
    let stmt = nb.statement().expect("оператор должен быть");
    assert!(
        !matches!(stmt, StatementNode::Unresolved(_)),
        "return должен быть разрешён: {:?}",
        stmt
    );
}

/// `always { debug(\"msg\"); }` с вызовом необъявленной встроенной функции - строится
/// без ошибок, блок разрешается (через заглушку FunctionNode).
#[test]
fn named_block_with_builtin_func_call_does_not_error() {
    let node = build(r#"always { debug("msg"); } start S;"#);
    let nb = node.get_named_block("always").expect("always не найден");
    let stmt = nb.statement().expect("оператор должен быть");
    // С заглушкой FunctionNode разрешение успешно
    assert!(
        !matches!(stmt, StatementNode::Unresolved(_)),
        "always с встроенной функцией должен быть разрешён: {:?}",
        stmt
    );
}

/// `syntax_simple` регрессионный тест: сложный SRC со всеми конструкциями строится без
/// паники (todo!() устранён).
#[test]
fn syntax_simple_does_not_panic() {
    // Копия SRC из lib.rs - проверяем что construct_model успешен
    let src = r#"
const MATRIX: u8 := { 0, 0, 0, 0, 0, 0, 0, 0 };
const NUMB: u8 := 0xFF;
cond IsEmpty = it = 0;
out A: u8 at 0x00548835;
in B1: bit at 0x00648835:6;
var it: [bit;64] := 0;
model Ping {
    start Start {
        ref End: B1;
        enter { A.0 := true; }
        exit  { A.0 := false; }
        always { A.2 := toggle; }
        always { toggle := !toggle; }
    }
    state End;
    var toggle := false;
}
model Pong {
    start Begin {
        ref Stop: S(Ping) = End;
        always { A.5 := MATRIX.5; }
    }
    state Stop {
        enter { A.6 := MATRIX.3; }
    }
}
start Entry = (Ping | Pong) + Ping;
always {
    it := it + 1;
}
"#;
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    construct_model(&ast, None, &[]).expect("construct_model не должен паниковать");
}

/// Файл named_blocks.takt строится без ошибок, named_blocks заполнены.
#[test]
fn example_named_blocks_is_valid() {
    let node = build_file("tests/data/semantic/valid/named_blocks.takt").unwrap();
    assert!(
        node.has_states(),
        "named_blocks.takt должен иметь состояния"
    );
    let active = node.states.get("Active").expect("Active не найдено");
    assert!(
        active.get_named_block("enter").is_some(),
        "enter должен быть в Active"
    );
    assert!(
        active.get_named_block("exit").is_some(),
        "exit должен быть в Active"
    );
    assert!(
        active.get_named_block("always").is_some(),
        "always должен быть в Active"
    );
}

/// Файл if_while_for.takt строится без ошибок.
#[test]
fn example_if_while_for_is_valid() {
    build_file("tests/data/semantic/valid/if_while_for.takt").unwrap();
}

/// Файл nested_model_blocks.takt строится без ошибок, enter разрешён.
#[test]
fn example_nested_model_blocks_is_valid() {
    let node = build_file("tests/data/semantic/valid/nested_model_blocks.takt").unwrap();
    let inner = node.search_model("Inner").expect("Inner не найдена");
    let inner = inner.borrow();
    let on = inner.states.get("On").expect("On не найдено");
    assert!(
        on.get_named_block("enter").is_some(),
        "enter должен быть в On"
    );
}

/// named_block_undeclared_var.takt (порт без адреса) -> теперь корректен, адрес
/// опционален.
#[test]
fn example_named_block_port_without_address_is_valid() {
    let result = build_file("tests/data/semantic/invalid/named_block_undeclared_var.takt");
    assert!(
        result.is_ok(),
        "порт без адреса должен быть принят (адрес опционален): {:?}",
        result.err()
    );
}

/// Несколько именованных блоков с одним и тем же именем (например, два `enter`)
/// корректно сохраняются и разрешаются.
#[test]
fn multiple_named_blocks_with_same_name_resolve() {
    let node =
        build("var a: bit := 0; var b: bit := 0; start S { enter { a := 1; } enter { b := 1; } }");
    let state = node.states.get("S").expect("S не найден");
    let blocks = state.get_named_blocks("enter");
    assert_eq!(blocks.len(), 2, "Должно быть два блока enter");

    // Проверяем, что оба разрешены
    for block in blocks {
        let stmt = block.statement().expect("оператор должен быть");
        assert!(
            !matches!(stmt, StatementNode::Unresolved(_)),
            "блок должен быть разрешён"
        );
    }
}

/// Несколько `always` блоков на уровне модели.
#[test]
fn multiple_model_level_always_blocks() {
    let node =
        build("var a: bit := 0; var b: bit := 0; always { a := 1; } always { b := 1; } start S;");
    let blocks = node.get_named_blocks("always");
    assert_eq!(blocks.len(), 2, "Должно быть два блока always");

    for block in blocks {
        let stmt = block.statement().expect("оператор должен быть");
        assert!(
            !matches!(stmt, StatementNode::Unresolved(_)),
            "блок должен быть разрешён"
        );
    }
}

/// Файл multiple_named_blocks.takt строится без ошибок, блоки извлекаются.
#[test]
fn example_multiple_named_blocks_is_valid() {
    let node = build_file("tests/data/semantic/valid/multiple_named_blocks.takt").unwrap();
    let initial = node.states.get("Initial").expect("Initial не найдено");
    assert_eq!(
        initial.get_named_blocks("enter").len(),
        2,
        "Должно быть два enter в Initial"
    );
    assert_eq!(
        initial.get_named_blocks("exit").len(),
        2,
        "Должно быть два exit в Initial"
    );
    assert_eq!(
        node.get_named_blocks("always").len(),
        3,
        "Должно быть три always на уровне модели"
    );
}

// --- Тесты корректности значений типа bit --------------------------------------

/// `tests/data/semantic/valid/bit_values.takt` - допустимые значения bit строятся без
/// ошибок.
///
/// Проверяет: 0, 1, true, false, ссылка на переменную, константы, массив [bit;N].
#[test]
fn example_bit_values_valid_is_valid() {
    let node = build_file("tests/data/semantic/valid/bit_values.takt").unwrap();
    assert!(
        node.search_var("a").is_some(),
        "переменная a должна быть найдена"
    );
    assert!(
        node.search_var("b").is_some(),
        "переменная b должна быть найдена"
    );
    assert!(
        node.search_var("c").is_some(),
        "переменная c должна быть найдена"
    );
    assert!(
        node.search_var("d").is_some(),
        "переменная d должна быть найдена"
    );
}

/// `tests/data/semantic/invalid/bit_out_of_range.takt` - недопустимое bit-значение ->
/// ошибка.
///
/// Тип `bit` принимает только 0, 1, true, false. Значение 2 - ошибка.
#[test]
fn example_bit_out_of_range_is_error() {
    let result = build_file("tests/data/semantic/invalid/bit_out_of_range.takt");
    assert!(
        result.is_err(),
        "bit_out_of_range.takt должен давать ошибку семантики"
    );
    let err = result.unwrap_err();
    assert!(
        err.message.contains("bit"),
        "сообщение об ошибке должно упоминать тип bit: {}",
        err.message
    );
}

/// `tests/data/semantic/valid/type_inference_numbers.takt` - вывод целочисленных типов.
///
/// 0..=255 -> `[bit;8]`, 256..=65535 -> `[bit;16]`, 65536..= -> `[bit;32]`.
#[test]
fn example_type_inference_numbers_is_valid() {
    let node = build_file("tests/data/semantic/valid/type_inference_numbers.takt").unwrap();
    // 8-битные
    if let Some(VariableNode::Simple { ty, .. }) = node.search_var("a") {
        assert_eq!(
            ty,
            TypeNode::Array(8, Box::new(TypeNode::Bit)),
            "a=0 → [bit;8]"
        );
    }
    if let Some(VariableNode::Simple { ty, .. }) = node.search_var("c") {
        assert_eq!(
            ty,
            TypeNode::Array(8, Box::new(TypeNode::Bit)),
            "c=255 → [bit;8]"
        );
    }
    // 16-битные
    if let Some(VariableNode::Simple { ty, .. }) = node.search_var("d") {
        assert_eq!(
            ty,
            TypeNode::Array(16, Box::new(TypeNode::Bit)),
            "d=256 → [bit;16]"
        );
    }
    if let Some(VariableNode::Simple { ty, .. }) = node.search_var("f") {
        assert_eq!(
            ty,
            TypeNode::Array(16, Box::new(TypeNode::Bit)),
            "f=65535 → [bit;16]"
        );
    }
    // 32-битные
    if let Some(VariableNode::Simple { ty, .. }) = node.search_var("g") {
        assert_eq!(
            ty,
            TypeNode::Array(32, Box::new(TypeNode::Bit)),
            "g=65536 → [bit;32]"
        );
    }
}

/// `tests/data/semantic/valid/type_inference_bool.takt` - вывод типа bool из литерала.
///
/// `true`/`false` без аннотации -> `TypeNode::Bool`. Явная аннотация `: bool` ->
/// `TypeNode::Bool`. Явная аннотация `: bit` -> `TypeNode::Bit`.
#[test]
fn example_type_inference_bool_is_valid() {
    let node = build_file("tests/data/semantic/valid/type_inference_bool.takt").unwrap();
    // Вывод из литерала
    if let Some(VariableNode::Simple { ty, .. }) = node.search_var("flag") {
        assert_eq!(ty, TypeNode::Bool, "flag=true → Bool");
    }
    if let Some(VariableNode::Simple { ty, .. }) = node.search_var("done") {
        assert_eq!(ty, TypeNode::Bool, "done=false → Bool");
    }
    // Явная аннотация bool
    if let Some(VariableNode::Simple { ty, .. }) = node.search_var("ready") {
        assert_eq!(ty, TypeNode::Bool, "ready: bool → Bool");
    }
    // Явная аннотация bit
    if let Some(VariableNode::Simple { ty, .. }) = node.search_var("signal") {
        assert_eq!(ty, TypeNode::Bit, "signal: bit → Bit");
    }
}

// --- Тесты новых файлов-примеров ---------------------------------------------

/// `tests/data/semantic/valid/functions.takt` - локальные и внешние функции.
#[test]
fn example_functions_is_valid() {
    let node = build_file("tests/data/semantic/valid/functions.takt").unwrap();
    assert!(node.functions.contains_key("send"), "внешняя функция send");
    assert!(node.functions.contains_key("recv"), "внешняя функция recv");
    assert!(node.functions.contains_key("noop"), "внешняя функция noop");
    assert!(
        node.functions.contains_key("identity"),
        "локальная функция identity"
    );
    assert!(
        node.functions.contains_key("init"),
        "локальная функция init"
    );
}

/// `tests/data/semantic/valid/bool_type.takt` - переменные типа bool.
#[test]
fn example_bool_type_is_valid() {
    let node = build_file("tests/data/semantic/valid/bool_type.takt").unwrap();
    if let Some(VariableNode::Simple { ty, .. }) = node.search_var("ready") {
        assert_eq!(ty, TypeNode::Bool, "ready: bool → TypeNode::Bool");
    } else {
        panic!("переменная ready не найдена");
    }
    if let Some(VariableNode::Simple { ty, .. }) = node.search_var("inferred_true") {
        assert_eq!(ty, TypeNode::Bool, "inferred_true = true → TypeNode::Bool");
    } else {
        panic!("переменная inferred_true не найдена");
    }
}

/// `tests/data/semantic/valid/integer_types.takt` - числовые псевдонимы типов.
#[test]
fn example_integer_types_is_valid() {
    let node = build_file("tests/data/semantic/valid/integer_types.takt").unwrap();
    assert!(
        node.types.contains_key("Byte"),
        "тип Byte должен быть объявлен"
    );
    assert!(
        node.types.contains_key("Word"),
        "тип Word должен быть объявлен"
    );
    assert!(
        node.types.contains_key("DWord"),
        "тип DWord должен быть объявлен"
    );
    // Проверяем вывод типа из числовых литералов
    if let Some(VariableNode::Simple { ty, .. }) = node.search_var("small") {
        assert_eq!(
            ty,
            TypeNode::Array(8, Box::new(TypeNode::Bit)),
            "small=42 → [bit;8]"
        );
    }
    if let Some(VariableNode::Simple { ty, .. }) = node.search_var("medium") {
        assert_eq!(
            ty,
            TypeNode::Array(16, Box::new(TypeNode::Bit)),
            "medium=300 → [bit;16]"
        );
    }
    if let Some(VariableNode::Simple { ty, .. }) = node.search_var("large") {
        assert_eq!(
            ty,
            TypeNode::Array(32, Box::new(TypeNode::Bit)),
            "large=70000 → [bit;32]"
        );
    }
}

/// `tests/data/semantic/valid/state_machine_full.takt` - полный автомат светофора.
#[test]
fn example_state_machine_full_is_valid() {
    let node = build_file("tests/data/semantic/valid/state_machine_full.takt").unwrap();
    let tl = node
        .search_model("TrafficLight")
        .expect("модель TrafficLight не найдена");
    let tl = tl.borrow();
    assert!(tl.states.contains_key("Red"), "состояние Red");
    assert!(tl.states.contains_key("Green"), "состояние Green");
    assert!(tl.states.contains_key("Yellow"), "состояние Yellow");
}

/// `tests/data/semantic/invalid/duplicate_model.takt` - дублирующееся имя модели ->
/// ошибка.
#[test]
fn example_duplicate_model_is_error() {
    let result = build_file("tests/data/semantic/invalid/duplicate_model.takt");
    assert!(
        result.is_err(),
        "дублирующееся имя модели должно давать ошибку"
    );
}
