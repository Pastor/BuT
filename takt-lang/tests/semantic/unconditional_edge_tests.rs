//! Решение "ребро безусловно" - у одного носителя.
//!
//! ## Что здесь ловится
//!
//! Правило "ребро без условия завершает цепочку" принимали **пять** потребителей
//! порознь: цели `c`, `st`, `rust`, `sv` и эталон. Они разошлись - `rust`
//! (`rust_tick.rs`) и `sv` (`sv_fsm.rs`) считали безусловным ещё и
//! `ConditionNode::Unresolved`, то есть **условное** ребро с неразрешённым условием
//! срабатывало у них всегда: вывод валиден, автомат другой, и молча. изъяла ту же ветвь
//! у цели `c`, но её объёмом была одна цель.

use takt_lang::parser::ast;
use takt_lang::semantic::ConditionNode;

/// **Отсутствие условия - безусловное ребро.**
#[test]
fn absent_condition_is_unconditional() {
    assert!(ConditionNode::None.is_unconditional());
}

/// **Неразрешённое условие безусловным не считается.**
///
/// Мутация "вернуть `Unresolved` в предикат" валит именно этот тест.
#[test]
fn unresolved_condition_is_not_unconditional() {
    let node = ConditionNode::Unresolved(ast::Condition::Number(
        takt_lang::diagnostics::Location::Builtin,
        1,
    ));
    assert!(
        !node.is_unconditional(),
        "неразрешённое условие значит «имя не найдено», а не «условия нет»"
    );
}

/// **Настоящее условие безусловным не считается.**
///
/// Тест от обратной мутации - предиката, отвечающего `true` всегда.
#[test]
fn real_condition_is_not_unconditional() {
    assert!(!ConditionNode::Bool(true).is_unconditional());
    assert!(!ConditionNode::Number(1).is_unconditional());
}

/// **Решение принимает один носитель - обхода нет ни у кого.**
///
/// Ищется форма `matches!(..., ConditionNode::None...)` - именно она и есть "решение",
/// в отличие от ветви исчерпывающего `match`, которая законна. Падение - **списком**:
/// пропустив одно место, тест вернул бы расхождение.
#[test]
fn nobody_decides_unconditional_on_its_own() {
    let mut found = Vec::new();
    for root in ["src", "../takt-sim/src"] {
        collect(std::path::Path::new(root), &mut found);
    }
    assert!(
        found.is_empty(),
        "решение «ребро безусловно» принимается мимо ConditionNode::is_unconditional():\n{}",
        found.join("\n")
    );
}

/// Рекурсивно собирает места, решающие "безусловно" своим `matches!`.
fn collect(dir: &std::path::Path, out: &mut Vec<String>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        // Каталога нет - это дефект теста, а не чистота дерева.
        out.push(format!("{}: каталог не читается", dir.display()));
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect(&path, out);
            continue;
        }
        if path.extension().is_none_or(|e| e != "rs") {
            continue;
        }
        // Носитель правила и его собственные тесты - законное место.
        if path.ends_with("condition_node.rs") {
            continue;
        }
        let Ok(text) = std::fs::read_to_string(&path) else {
            continue;
        };
        for (no, line) in text.lines().enumerate() {
            if line.contains("matches!") && line.contains("ConditionNode::None") {
                out.push(format!("{}:{}: {}", path.display(), no + 1, line.trim()));
            }
        }
    }
}

/// **Замер достижимости - неразрешённое условие отсекает `SE-025`.**
///
/// Проверка не "на всякий случай": она фиксирует, **чем** держится недостижимость. Если
/// однажды этот тест перестанет видеть `SE-025`, ветвь предиката станет достижимой - и
/// тест T2 окажется единственным, что отделяет вход от молчаливой подмены автомата.
#[test]
fn unresolved_condition_is_rejected_before_codegen() {
    let codes: Vec<String> = takt_lang::pipeline::collect_compile_diagnostics(
        "model.takt",
        "var n: u8 := 0;\n\
         start Run {\n\
             always { n := n + 1; }\n\
             ref Done: nosuchname;\n\
         }\n\
         state Done;\n",
        &[],
        false,
    )
    .into_iter()
    .filter_map(|d| d.code)
    .collect();
    assert!(
        codes.contains(&"SE-025".to_string()),
        "неразрешённое условие обязано отсекаться семантикой, получено: {codes:?}"
    );
}
