//! Тесты разрешения семантических операторов (вынесены из `statement.rs`
//! фичей 0225).
//!
//! Причина выноса — правило размера модуля: тесты занимали 599 строк из 976,
//! то есть 61 % файла, и запаса до предела оставалось 24 строки. Разделение
//! «логика / тесты» — приём фичи 0088 (директория-подмодуль + `use super::*`),
//! тот же, которым фича 0129 разделила `semantic/expression.rs`.

use super::*;
use crate::diagnostics::Location;
use crate::parse;
use crate::semantic::tree::construct_model;

/// Строит модель и возвращает корневой ModelNode.
fn build(src: &str) -> ModelNode {
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    construct_model(&ast, None, &[])
        .map(|m| m.take())
        .expect("ошибка построения")
}

// ─── Тесты resolve_statement ────────────────────────────────────────────

/// `Statement::None` → возвращается без изменений.
#[test]
fn resolve_none_returns_none() {
    let m = Rc::new(RefCell::new(ModelNode::default()));
    let result = resolve_statement(&StatementNode::None, vec![], m).unwrap();
    assert_eq!(result, StatementNode::None);
}

/// Уже разрешённый `Statement::Continue` → возвращается без изменений.
#[test]
fn resolve_already_resolved_passthrough() {
    let m = Rc::new(RefCell::new(ModelNode::default()));
    let stmt = StatementNode::Continue(Location::Codegen);
    let result = resolve_statement(&stmt, vec![], m).unwrap();
    assert_eq!(result, StatementNode::Continue(Location::Codegen));
}

/// `Statement::Block([Continue, Break])` рекурсивно разрешается.
#[test]
fn resolve_block_recursively() {
    let m = Rc::new(RefCell::new(ModelNode::default()));
    let stmt = StatementNode::Block(vec![
        StatementNode::Continue(Location::Codegen),
        StatementNode::Break(Location::Codegen),
    ]);
    let result = resolve_statement(&stmt, vec![], m).unwrap();
    assert_eq!(
        result,
        StatementNode::Block(vec![
            StatementNode::Continue(Location::Codegen),
            StatementNode::Break(Location::Codegen)
        ])
    );
}

// ─── Интеграционные тесты через construct_model ──────────────────────────

/// `always { it = it; }` с объявленной переменной `it` — разрешается в Block.
#[test]
fn model_level_always_block_with_known_var_resolves() {
    let node = build("var it: bit := 0; always { it := it; } start S;");
    let nb = node
        .get_named_block("always")
        .expect("блок always должен быть");
    let stmt = nb.statement().expect("оператор должен быть");
    // Должен быть разрешён (не Unresolved)
    assert!(
        !matches!(stmt, StatementNode::Unresolved(_)),
        "блок always должен быть разрешён, получен: {:?}",
        stmt
    );
}

/// `always { debug("msg"); }` с необъявленной функцией — хранится как Unresolved (без паники).
#[test]
fn model_level_always_block_with_unknown_func_does_not_panic() {
    // `debug` не объявлена — после изменения expression.rs создаётся заглушка,
    // поэтому оператор может быть разрешён или нет, но паники быть не должно
    let node = build(r#"always { debug("msg"); } start S;"#);
    let nb = node
        .get_named_block("always")
        .expect("блок always должен быть");
    let _ = nb.statement(); // просто доступ без паники
}

/// `enter { A = 0; }` внутри состояния — блок хранится в state.named_blocks.
#[test]
fn state_level_named_block_is_populated() {
    let node = build("var A: bit := false; start S { enter { A := A; } }");
    let state = node.states.get("S").expect("состояние S не найдено");
    assert!(
        state.get_named_block("enter").is_some(),
        "enter должен быть в named_blocks состояния"
    );
}

/// `enter { A = A; }` с известной переменной — разрешается.
#[test]
fn state_level_named_block_resolves_known_var() {
    let node = build("var A: bit := false; start S { enter { A := A; } }");
    let state = node.states.get("S").expect("состояние S не найдено");
    let enter = state.get_named_block("enter").expect("enter не найден");
    let stmt = enter.statement().expect("оператор должен быть");
    assert!(
        !matches!(stmt, StatementNode::Unresolved(_)),
        "enter должен быть разрешён"
    );
}

/// Несколько named blocks в одном состоянии: enter и exit.
#[test]
fn state_level_multiple_named_blocks() {
    let node = build("var A: bit := false; start S { enter { A := A; } exit { A := A; } }");
    let state = node.states.get("S").unwrap();
    assert!(
        state.get_named_block("enter").is_some(),
        "enter должен быть"
    );
    assert!(state.get_named_block("exit").is_some(), "exit должен быть");
}

/// Разрешение оператора `if` в блоке состояния.
#[test]
fn state_named_block_if_statement_resolves() {
    let node = build("var x: bit := false; start S { always { if x { x := x; } } }");
    let state = node.states.get("S").unwrap();
    let always = state.get_named_block("always").expect("always не найден");
    let stmt = always.statement().expect("оператор должен быть");
    assert!(
        matches!(stmt, StatementNode::Block(_) | StatementNode::If { .. }),
        "оператор if должен быть разрешён: {:?}",
        stmt
    );
}

/// Оператор `return` в named block разрешается.
#[test]
fn return_statement_resolves() {
    let node = build("var x: bit := false; always { return x; } start S;");
    let nb = node.get_named_block("always").expect("always не найден");
    let stmt = nb.statement().expect("оператор должен быть");
    assert!(
        !matches!(stmt, StatementNode::Unresolved(_)),
        "return должен быть разрешён: {:?}",
        stmt
    );
}

/// `continue` и `break` разрешаются в соответствующие варианты — В ЦИКЛЕ.
///
/// Вне цикла оба отвергаются `SE-132` (фича 0530), поэтому признак взводится
/// стражем: тест проверяет понижение, а не место, где оно разрешено.
#[test]
fn continue_break_resolve() {
    let _inside = crate::semantic::statement::loop_context::enter();
    let ast_continue = ast::Statement::Continue(crate::diagnostics::Location::default());
    let ast_break = ast::Statement::Break(crate::diagnostics::Location::default());
    let m = Rc::new(RefCell::new(ModelNode::default()));
    let r1 =
        resolve_statement(&StatementNode::Unresolved(ast_continue), vec![], m.clone()).unwrap();
    let r2 = resolve_statement(&StatementNode::Unresolved(ast_break), vec![], m).unwrap();
    assert_eq!(r1, StatementNode::Continue(Location::Codegen));
    assert_eq!(r2, StatementNode::Break(Location::Codegen));
}

// ─── Циклы ────────────────────────────────────────────────────────────────

// ── Вспомогательная функция ───────────────────────────────────────────────

/// Возвращает первый реальный оператор из блока (вложенного в always-блок).
///
/// `always { <stmt> }` оборачивает оператор в `Block([<stmt>])`.
/// Эта функция раскрывает один уровень вложенности.
fn first_in_block(stmt: &StatementNode) -> &StatementNode {
    match stmt {
        StatementNode::Block(stmts) => stmts.first().expect("блок пуст"),
        other => other,
    }
}

// ── Циклы ─────────────────────────────────────────────────────────────────

/// `loop условие { }` разрешается в `Statement::Loop` с условием.
///
/// # Пример (Takt)
/// ```but
/// var flag: bit = false;
/// always {
///     loop flag { flag = flag; }
/// }
/// ```
#[test]
fn loop_with_condition_resolves() {
    let node = build(
        // loop с условием — аналог while
        "var flag: bit := false; always { loop flag { flag := flag; } } start S;",
    );
    let nb = node.get_named_block("always").expect("always не найден");
    let stmt = first_in_block(nb.statement().expect("оператор должен быть"));
    assert!(
        matches!(stmt, StatementNode::Loop { cond: Some(_), .. }),
        "ожидался Loop с условием, получен: {:?}",
        stmt
    );
}

/// `loop { }` без условия разрешается в `Statement::Loop` с `cond = None`.
///
/// # Пример (Takt)
/// ```but
/// always {
///     loop { break; }
/// }
/// ```
#[test]
fn loop_without_condition_resolves() {
    let node = build(
        // loop без условия — бесконечный цикл
        "always { loop { break; } } start S;",
    );
    let nb = node.get_named_block("always").expect("always не найден");
    let stmt = first_in_block(nb.statement().expect("оператор должен быть"));
    assert!(
        matches!(stmt, StatementNode::Loop { cond: None, .. }),
        "ожидался Loop без условия, получен: {:?}",
        stmt
    );
}

/// `for` с инициализацией, условием и шагом разрешается.
///
/// # Пример (Takt)
/// ```but
/// var i: bit = false;
/// always {
///     // С1 (вариант A): for без скобок вокруг заголовка
///     for i = false; i; i = false { }
/// }
/// ```
#[test]
fn for_loop_resolves() {
    let node = build(
        // С1 (вариант A): for без скобок вокруг заголовка
        "var i: bit := false; always { for i := false; i; i := false { } } start S;",
    );
    let nb = node.get_named_block("always").expect("always не найден");
    let stmt = first_in_block(nb.statement().expect("оператор должен быть"));
    assert!(
        matches!(stmt, StatementNode::For { .. }),
        "ожидался For, получен: {:?}",
        stmt
    );
}

/// `for` без инициализации и шага разрешается.
///
/// # Пример (Takt)
/// ```but
/// // С1 (вариант A): for без скобок, все части пусты
/// for ;; { }
/// ```
#[test]
fn for_loop_empty_parts_resolves() {
    // С1 (вариант A): for без скобок вокруг заголовка
    let node = build("var i: bit := false; always { for ;; { } } start S;");
    let nb = node.get_named_block("always").expect("always не найден");
    let stmt = first_in_block(nb.statement().expect("оператор должен быть"));
    assert!(
        matches!(
            stmt,
            StatementNode::For {
                init: None,
                cond: None,
                step: None,
                ..
            }
        ),
        "ожидался For{{None, None, None}}, получен: {:?}",
        stmt
    );
}

/// Оператор `return` без значения разрешается.
#[test]
fn return_without_value_resolves() {
    let node = build("always { return; } start S;");
    let nb = node.get_named_block("always").expect("always не найден");
    let stmt = first_in_block(nb.statement().expect("оператор должен быть"));
    assert!(
        matches!(stmt, StatementNode::Return(None, _)),
        "ожидался Return(None), получен: {:?}",
        stmt
    );
}

/// Оператор `if` с `else`-веткой разрешается.
///
/// # Пример (Takt)
/// ```but
/// var x: bit = false;
/// always {
///     if x { x = x; } else { x = false; }
/// }
/// ```
#[test]
fn if_else_resolves() {
    let node =
        build("var x: bit := false; always { if x { x := x; } else { x := false; } } start S;");
    let nb = node.get_named_block("always").expect("always не найден");
    let stmt = first_in_block(nb.statement().expect("оператор должен быть"));
    assert!(
        matches!(stmt, StatementNode::If { else_: Some(_), .. }),
        "ожидался If{{else_: Some}}, получен: {:?}",
        stmt
    );
}

/// `if` без `else` имеет `else_ = None`.
#[test]
fn if_without_else_has_none_else() {
    let node = build("var x: bit := false; always { if x { x := x; } } start S;");
    let nb = node.get_named_block("always").expect("always не найден");
    let stmt = first_in_block(nb.statement().expect("оператор должен быть"));
    assert!(
        matches!(stmt, StatementNode::If { else_: None, .. }),
        "ожидался If{{else_: None}}, получен: {:?}",
        stmt
    );
}

// ─── С4: локальные переменные в блоках ────────────────────────────────────

/// `always { var x: bit = false; x = true; }` — локальная переменная
/// объявляется и используется в том же блоке.
///
/// # Пример (Takt)
/// ```but
/// always { var x: bit = false; x = true; }
/// start S;
/// ```
#[test]
fn local_var_in_block_resolves() {
    let node = build("always { var x: bit := false; x := true; } start S;");
    let nb = node.get_named_block("always").expect("always не найден");
    let stmt = nb.statement().expect("оператор должен быть");
    // Блок должен быть разрешён
    assert!(
        !matches!(stmt, StatementNode::Unresolved(_)),
        "блок с локальной var должен быть разрешён: {:?}",
        stmt
    );
    // Первый оператор — Statement::Variable(x, ...)
    if let StatementNode::Block(stmts) = stmt {
        assert!(
            matches!(stmts.first(), Some(StatementNode::Variable(n, _, _, _)) if n == "x"),
            "первый оператор должен быть Variable(x): {:?}",
            stmts.first()
        );
    } else {
        panic!("ожидался Statement::Block, получен: {:?}", stmt);
    }
}

/// `always { var x: bit = false; x = true; }` — инициализатор `false`
/// сохраняется в `Statement::Variable`.
///
/// # Пример (Takt)
/// ```but
/// always { var x: bit = false; x = true; }
/// start S;
/// ```
#[test]
fn local_var_initializer_is_preserved() {
    let node = build("always { var x: bit := false; x := true; } start S;");
    let nb = node.get_named_block("always").expect("always не найден");
    let stmt = nb.statement().expect("оператор должен быть");
    if let StatementNode::Block(stmts) = stmt {
        // Инициализатор должен быть Some(Bool(false))
        assert!(
            matches!(
                stmts.first(),
                Some(StatementNode::Variable(_, _, Some(_), _))
            ),
            "у переменной x должен быть инициализатор: {:?}",
            stmts.first()
        );
    }
}

/// `always { const C: bit = true; C; }` — константа внутри блока разрешается.
///
/// # Пример (Takt)
/// ```but
/// always { const C: bit := true; var seen: bit := C; }
/// start S;
/// ```
///
/// ⚠️ Прежде вторым оператором стояло голое `C;`. Фича 0189 (решение 6A)
/// такую запись отвергает: выражение в позиции оператора обязано иметь
/// эффект. Предмет теста не изменился — локальная `const` по-прежнему
/// должна разрешаться, — но её использование записано законной формой.
#[test]
fn local_const_in_block_resolves() {
    let node = build("always { const C: bit := true; var seen: bit := C; } start S;");
    let nb = node.get_named_block("always").expect("always не найден");
    let stmt = nb.statement().expect("оператор должен быть");
    assert!(
        !matches!(stmt, StatementNode::Unresolved(_)),
        "блок с локальной const должен быть разрешён: {:?}",
        stmt
    );
    if let StatementNode::Block(stmts) = stmt {
        assert!(
            matches!(stmts.first(), Some(StatementNode::Variable(n, _, _, _)) if n == "C"),
            "первый оператор должен быть Variable(C): {:?}",
            stmts.first()
        );
    }
}

/// Локальная переменная затеняет переменную уровня модели.
///
/// Внутри блока `x` ссылается на локальную переменную, после выхода — на model-level.
///
/// # Пример (Takt)
/// ```but
/// var x: bit = true;
/// always { var x: bit = false; x = false; }
/// start S;
/// ```
#[test]
fn local_var_shadows_model_var() {
    let node = build("var x: bit := true; always { var x: bit := false; x := false; } start S;");
    let nb = node.get_named_block("always").expect("always не найден");
    let stmt = nb.statement().expect("оператор должен быть");
    // Блок разрешается (затенение работает)
    assert!(
        !matches!(stmt, StatementNode::Unresolved(_)),
        "блок с затенённой переменной должен быть разрешён: {:?}",
        stmt
    );
    // После выхода из блока model-level x должна остаться в модели
    assert!(
        node.search_var("x").is_some(),
        "model-level переменная x должна сохраниться после выхода из блока"
    );
}

/// После выхода из блока локальная переменная удаляется из модели.
///
/// Проверяет механизм [`unregister_local_vars`] напрямую: после разрешения
/// `{ var inner: bit = false; }` переменная `inner` не должна находиться
/// в `model.variables`.
///
/// # Контрпример (Takt)
/// ```but
/// { var inner: bit = false; }
/// inner = true;   // ошибка: inner вне области видимости
/// ```
#[test]
fn local_var_scope_exits_block() {
    use crate::diagnostics::Location;
    use crate::parser::ast::{Identifier, VariableDefine};

    let loc = Location::default();
    let def = Box::new(VariableDefine::Variable {
        loc,
        typ: Some(crate::parser::ast::Type::Bit),
        name: Some(Identifier {
            loc,
            name: "inner".into(),
        }),
        initializer: None,
    });
    let var_stmt = ast::Statement::Variable(loc, def, None);
    let block = ast::Statement::Block {
        loc,
        unchecked: false,
        statements: vec![var_stmt],
    };

    let m = Rc::new(RefCell::new(ModelNode::default()));
    let resolved = resolve_ast_statement(&block, vec![], m.clone()).unwrap();

    // После разрешения блока `inner` не должна оставаться в модели
    assert!(
        !m.borrow().variables.contains_key("inner"),
        "inner не должна быть в модели после выхода из блока"
    );
    // Но Statement::Variable(inner) должен быть внутри блока
    if let StatementNode::Block(stmts) = resolved {
        assert!(
            matches!(stmts.first(), Some(StatementNode::Variable(n, _, _, _)) if n == "inner"),
            "разрешённый блок должен содержать Variable(inner): {:?}",
            stmts.first()
        );
    } else {
        panic!("ожидался Statement::Block");
    }
}

/// `for var i: bit = false; i; i = false { }` — переменная из init for
/// видна в условии и шаге цикла.
///
/// # Пример (Takt)
/// ```but
/// always { for var i: bit = false; i; i = false { } }
/// start S;
/// ```
#[test]
fn local_var_in_for_init_resolves() {
    let node = build("always { for var i: bit := false; i; i := false { } } start S;");
    let nb = node.get_named_block("always").expect("always не найден");
    let stmt = first_in_block(nb.statement().expect("оператор должен быть"));
    assert!(
        matches!(
            stmt,
            StatementNode::For {
                init: Some(_),
                cond: Some(_),
                ..
            }
        ),
        "ожидался For{{init: Some, cond: Some}}: {:?}",
        stmt
    );
    if let StatementNode::For {
        init: Some(init_box),
        ..
    } = stmt
    {
        assert!(
            matches!(init_box.as_ref(), StatementNode::Variable(n, _, _, _) if n == "i"),
            "init for должен быть Variable(i): {:?}",
            init_box
        );
    }
}

/// Параметр функции виден в операторе-выражении (`Statement::Expression`).
///
/// Регрессионный тест для ошибки «Идентификатор 'value' не найден в области видимости».
/// До исправления `construct_expression` в ветке `Expression` вызывалась с `vec![]`
/// вместо `params.clone()`, из-за чего параметры функции были невидимы в операторах вида
/// `value > 100` или `out = value`.
///
/// # Пример (Takt)
/// ```but
/// fn clamp_temp(value: u8): u8 {
///     if value > 100 { return 100; }
///     return value;
/// }
/// start S;
/// ```
#[test]
fn function_param_visible_in_expression_statement() {
    use crate::semantic::FunctionDefinitionNode;
    // `result = value` — оператор-выражение (присваивание), где `value` — параметр функции.
    // До исправления это приводило к ошибке LSP «Идентификатор 'value' не найден».
    let node = build(concat!(
        "",
        "var result: u8 := 0; ",
        "fn clamp(value: u8) -> u8 { result := value; return value; } ",
        "start S;"
    ));
    // Проверяем, что модель успешно построена и функция clamp присутствует
    assert!(
        node.functions.contains_key("clamp"),
        "функция clamp должна быть в модели"
    );
    // Проверяем, что тело функции разрешено (нет Unresolved на верхнем уровне)
    let func = node
        .functions
        .get("clamp")
        .expect("функция clamp не найдена");
    if let FunctionDefinitionNode::Local { body, .. } = func {
        assert!(
            !matches!(body, StatementNode::Unresolved(_)),
            "тело функции clamp должно быть разрешено, получен: {:?}",
            body
        );
    } else {
        panic!("функция clamp должна быть Local, получен: {:?}", func);
    }
}

/// Объявление переменной в операторе `var` разрешается в `Statement::Variable`.
///
/// Тест проверяет семантический уровень напрямую (через `ast::Statement::Variable`).
#[test]
fn variable_statement_resolves() {
    use crate::diagnostics::Location;
    use crate::parser::ast::{Identifier, VariableDefine};
    let loc = Location::default();
    let def = Box::new(VariableDefine::Variable {
        loc,
        typ: Some(crate::parser::ast::Type::Bit),
        name: Some(Identifier {
            loc,
            name: "tmp".into(),
        }),
        initializer: None,
    });
    let stmt = ast::Statement::Variable(loc, def, None);
    let m = Rc::new(RefCell::new(ModelNode::default()));
    let result = resolve_statement(&StatementNode::Unresolved(stmt), vec![], m).unwrap();
    assert!(
        matches!(result, StatementNode::Variable(ref n, _, None, _) if n == "tmp"),
        "ожидался Variable(tmp, _, None), получен: {:?}",
        result
    );
}
