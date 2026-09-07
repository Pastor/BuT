//! Интеграционные тесты верификации LTL: вердикт `taktc verify` через публичный API на
//! фикстурах `tests/data/verify/`.
//!
//! Тесты - **на вердикт и контрпример**, а не на структуру автомата/произведения
//! (капкан: зелёные тесты на структуру при неверной семантике; риск Р7 анализа).
//! Проверяется то, что увидит пользователь.
//!
//! Фикстуры - примеры и контрпримеры: к каждому свойству, которое держится, есть парная
//! модель, где оно нарушено.

use std::cell::RefCell;
use std::rc::Rc;
use takt_lang::parse;
use takt_lang::semantic::tree::construct_model;
use takt_lang::verification::verify::{UnsupportedReason, Verdict};

fn model_of(fixture: &str) -> Rc<RefCell<takt_lang::semantic::ModelNode>> {
    let path = format!("tests/data/verify/{fixture}");
    let source = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{path}: {e}"));
    let (ast, _) = parse(&source, 0).unwrap_or_else(|d| panic!("{path}: разбор — {d:?}"));
    construct_model(&ast, None, &[]).unwrap_or_else(|d| panic!("{path}: семантика — {d:?}"))
}

/// Единственный вердикт фикстуры (в каждой ровно одна `: [LTL] φ;`).
fn verdict_of(fixture: &str) -> Verdict {
    let results = takt_lang::verify_all(model_of(fixture));
    assert_eq!(
        results.len(),
        1,
        "{fixture}: ожидалась ровно одна LTL-формула, найдено {}",
        results.len()
    );
    results.into_iter().next().unwrap().verdict
}

// --- Живость "после сбоя - возврат в Idle" --------------------------------

/// Пример: из Fault все пути ведут в Idle - свойство держится.
#[test]
fn response_property_holds() {
    assert_eq!(
        verdict_of("holds.takt"),
        Verdict::Holds,
        "G (Fault -> F Idle) обязано держаться: из Fault выход только через Recovery в Idle"
    );
}

/// Контрпример: самопетля Fault позволяет залипнуть в сбое навсегда.
#[test]
fn response_property_violated_with_lasso() {
    let Verdict::Violated(cex) = verdict_of("fails.takt") else {
        panic!("fails.takt: `ref Fault` в самом Fault даёт вечное залипание — ожидалось нарушение");
    };
    assert_eq!(
        cex.trace(),
        "Idle -> [ Fault ]*",
        "контрпример — вечное залипание в Fault"
    );
}

// --- Достижимость ---------------------------------------------------------

/// Пример: Done достижимо и неизбежно - `F Done` держится.
#[test]
fn reachability_holds() {
    assert_eq!(verdict_of("reachable.takt"), Verdict::Holds);
}

/// Контрпример: в Done не ведёт ни один `ref` - `F Done` нарушено.
#[test]
fn reachability_violated_when_state_is_unreachable() {
    let Verdict::Violated(cex) = verdict_of("unreachable.takt") else {
        panic!("unreachable.takt: в Done нет ни одного перехода — ожидалось нарушение");
    };
    assert!(
        !cex.cycle.contains(&"Done".to_string()) && !cex.prefix.contains(&"Done".to_string()),
        "контрпример обязан обходить Done стороной: {}",
        cex.trace()
    );
}

// --- Честная граница абстракции -------------------------------------------

/// Атом-переменная не проверяется молча: Unsupported с именем атома.
#[test]
fn data_atom_is_reported_as_unsupported() {
    assert_eq!(
        verdict_of("atom_not_state.takt"),
        Verdict::Unsupported {
            atoms: vec!["temp".to_string()],
            reason: UnsupportedReason::UnknownAtom,
        },
        "`temp` — переменная, а не состояние: в абстракции управления не проверяется"
    );
}

// --- Детерминизм вердикта (проверка 0048) -------------------------------------

#[test]
fn verdicts_are_deterministic() {
    for fixture in [
        "holds.takt",
        "fails.takt",
        "reachable.takt",
        "unreachable.takt",
        "atom_not_state.takt",
    ] {
        let first = verdict_of(fixture);
        for _ in 0..10 {
            assert_eq!(
                verdict_of(fixture),
                first,
                "{fixture}: вердикт не воспроизводим"
            );
        }
    }
}

// --- Разбор свойства из командной строки (--property) -------------------------

/// Свойство из строки разбирается грамматикой языка: имена состояний - не по одному
/// символу (в отличие от тестового `parse_ltl`, A6).
#[test]
fn property_string_parses_multi_char_atoms() {
    let phi = takt_lang::parse_ltl_property("G (Fault -> F Idle)").unwrap();
    assert_eq!(phi.to_string(), "G (Fault -> F Idle)");
}

/// Свойство из строки проверяется наравне с объявленным в файле.
#[test]
fn property_from_string_is_verified() {
    let phi = takt_lang::parse_ltl_property("F Done").unwrap();
    assert_eq!(
        takt_lang::verify_model(model_of("reachable.takt"), &phi),
        Verdict::Holds
    );

    let phi = takt_lang::parse_ltl_property("F Done").unwrap();
    assert!(matches!(
        takt_lang::verify_model(model_of("unreachable.takt"), &phi),
        Verdict::Violated(_)
    ));
}

/// Синтаксически негодное свойство - ошибка, а не паника (`parse_ltl` паникует).
#[test]
fn malformed_property_is_an_error_not_a_panic() {
    assert!(takt_lang::parse_ltl_property("G (").is_err());
    assert!(takt_lang::parse_ltl_property("").is_err());
    assert!(takt_lang::parse_ltl_property("&& F").is_err());
}

/// Список формул через запятую - отказ: непонятно, какую проверять.
#[test]
fn property_list_is_rejected() {
    assert!(
        takt_lang::parse_ltl_property("F Done, G Idle").is_err(),
        "две формулы в одном --property: молча взять первую — соврать о проверенном"
    );
}

/// Строка со своей `;` - отказ, а не молчаливое усечение до первой формулы.
///
/// Свойство оборачивается в `: [LTL] {строка};`, поэтому `;` внутри строки закрывает
/// обёртку досрочно, а хвост становится отдельными конструкциями файла. Взять первую и
/// промолчать - та же ложь, что и с запятой.
#[test]
fn property_with_semicolon_is_rejected_not_truncated() {
    for property in [
        "F Done; : [LTL] G Idle",     // вторая формула потерялась бы молча
        "G Idle; start X { ref X; }", // протащили бы объявление состояния
        "F Done;",                    // хвост пуст, но `;` всё равно лишняя
    ] {
        assert!(
            takt_lang::parse_ltl_property(property).is_err(),
            "'{property}': ожидался отказ — иначе проверено будет не то, что задано"
        );
    }
}

// --- Паритет с README ---------
//
// Каждое свойство из таблицы "что проверяется" README подтверждено парой "модель, где
// держится" + "модель, где нарушено". Таблица обещает пользователю работающие формулы -
// эти тесты и есть доказательство обещания.

fn verdict_str(src: &str, property: &str) -> Verdict {
    let (ast, _) = parse(src, 0).unwrap_or_else(|d| panic!("разбор модели: {d:?}"));
    let model = construct_model(&ast, None, &[]).unwrap_or_else(|d| panic!("семантика: {d:?}"));
    let phi = takt_lang::parse_ltl_property(property).expect("разбор свойства");
    takt_lang::verify_model(model, &phi)
}

/// README: инвариант порядка `G (Open -> X Closing)` - оператор `X`.
#[test]
fn readme_order_invariant() {
    assert_eq!(
        verdict_str(
            "start Open { ref Closing; } state Closing { ref Open; }",
            "G (Open -> X Closing)"
        ),
        Verdict::Holds,
        "за Open всегда следует Closing"
    );
    assert!(
        matches!(
            verdict_str(
                "start Open { ref Closing; ref Open; } state Closing { ref Open; }",
                "G (Open -> X Closing)"
            ),
            Verdict::Violated(_)
        ),
        "`ref Open` в самом Open позволяет Open -> Open, что нарушает X Closing"
    );
}

/// README: отсутствие залипания `G F Idle` - бесконечно частый возврат.
#[test]
fn readme_no_starvation() {
    assert_eq!(
        verdict_str(
            "start Idle { ref Work; } state Work { ref Idle; }",
            "G F Idle"
        ),
        Verdict::Holds
    );
    let violated = verdict_str(
        "start Idle { ref Work; } state Work { ref Work; ref Idle; }",
        "G F Idle",
    );
    let Verdict::Violated(cex) = violated else {
        panic!("самопетля Work позволяет застрять навсегда: ожидалось нарушение");
    };
    assert_eq!(cex.trace(), "Idle -> [ Work ]*");
}

// --- Сверка с оракулом (тест против дефекта класса) ----------------
//
// Для формул `F X` и `G X` ответ считается вторым способом - прямым перебором графа,
// без автоматов Бюхи, произведения и проверки пустоты. Расхождение = дефект в конвейере
// автоматов. Зачем: дефект [](../../docs/fixes/-buchi-acceptance.md) (автомат принимал
// любой прогон) прожил в зелёном CI, потому что тесты проверяли структуру автомата, а
// не его ответы.
//
// **Границы оракула - что он не проверяет.** Он берёт на вход ту же `build_kripke`, из
// которой ответ выводит и верификатор, поэтому независим он от **звена автоматов**, а
// не от абстракции. Ошибку в самой Крипке (например, пропущенную самопетлю у состояния
// с одними условными выходами) оракул повторит вместе с верификатором и промолчит - так
// и вышло на ревью. Верность Крипке эталону (порождённому C) проверяют юнит-тесты
// `kripke.rs`, а не этот оракул; не путать одно с другим и не считать его тестом "на
// всё".

/// `G X` держится ⟺ все достижимые из старта вершины - это X.
fn oracle_globally(kripke: &takt_lang::verification::kripke::Kripke, atom: &str) -> bool {
    reachable_from_start(kripke, |_| true)
        .iter()
        .all(|&s| kripke.states[s] == atom)
}

/// `F X` держится ⟺ нет достижимого прогона, минующего X навсегда.
///
/// Граф конечен и тотален (у каждой вершины есть преемник - тупики имеют самопетлю),
/// поэтому "прогон, минующий X навсегда" существует ⟺ в подграфе вершин != X,
/// достижимом из старта, есть цикл.
fn oracle_finally(kripke: &takt_lang::verification::kripke::Kripke, atom: &str) -> bool {
    let ok = |s: usize| kripke.states[s] != atom;
    if !ok(kripke.initial) {
        return true; // старт - уже X
    }
    let sub = reachable_from_start(kripke, ok);
    // Итеративно отбрасываем вершины без преемников внутри подграфа; остаток - вершины
    // на циклах и ведущие к ним. Пусто ⟹ цикла нет ⟹ X неизбежно.
    let mut alive = sub;
    loop {
        let before = alive.len();
        let snapshot = alive.clone();
        alive.retain(|&s| {
            kripke
                .successors(s)
                .iter()
                .any(|t| ok(*t) && snapshot.contains(t))
        });
        if alive.len() == before {
            break;
        }
    }
    alive.is_empty()
}

/// Вершины, достижимые из стартовой по вершинам, удовлетворяющим `allow`.
fn reachable_from_start(
    kripke: &takt_lang::verification::kripke::Kripke,
    allow: impl Fn(usize) -> bool,
) -> Vec<usize> {
    let mut seen = vec![kripke.initial];
    let mut queue = vec![kripke.initial];
    while let Some(s) = queue.pop() {
        for &t in kripke.successors(s) {
            if allow(t) && !seen.contains(&t) {
                seen.push(t);
                queue.push(t);
            }
        }
    }
    seen
}

/// Вердикт `F X` / `G X` совпадает с прямым перебором на корпусе топологий.
#[test]
fn verdicts_agree_with_brute_force_oracle() {
    // Корпус топологий: самопетля, цепочка, цикл, ветвление, ловушка, недостижимая
    // вершина, цикл мимо цели - плюс модели с условными выходами (у таких состояний
    // Крипке добавляет самопетлю "guard не сработал").
    let corpus = [
        "start A { ref A; }",
        "start A { ref B; } state B;",
        "start A { ref B; } state B { ref A; }",
        "start A { ref B; ref C; } state B { ref A; } state C;",
        "start A { ref B; ref C; } state B { ref C; } state C { ref C; }",
        "start A { ref A; ref B; } state B { ref B; }",
        "start A { ref B; } state B { ref C; } state C { ref A; }",
        "start A { ref B; ref A; } state B { ref C; } state C;",
        "start A { ref C; } state B { ref A; } state C { ref C; }",
        // Условные выходы: прогон "стоим на месте" реален и обязан учитываться обеими
        // сторонами сверки.
        "var x: bit := false; start A { ref B: x = 1; } state B;",
        "var x: bit := false; start A { ref B: x = 1; ref C; } state B; state C;",
        "var x: bit := false; start A { ref B: x = 1; } state B { ref A: x = 0; }",
    ];

    let mut checked = 0usize;
    for src in corpus {
        let (ast, _) = parse(src, 0).unwrap();
        let model = construct_model(&ast, None, &[]).unwrap();
        let kripke = takt_lang::verification::kripke::build_kripke(&model.borrow())
            .expect("модель со стартовым состоянием");

        for atom in ["A", "B", "C"] {
            if !kripke.states.iter().any(|s| s == atom) {
                continue;
            }
            for (property, expected) in [
                (format!("F {atom}"), oracle_finally(&kripke, atom)),
                (format!("G {atom}"), oracle_globally(&kripke, atom)),
            ] {
                let phi = takt_lang::parse_ltl_property(&property).unwrap();
                let holds = matches!(
                    takt_lang::verify_model(Rc::clone(&model), &phi),
                    Verdict::Holds
                );
                assert_eq!(
                    holds, expected,
                    "{property} на `{src}`: перебор говорит {expected}, верификатор — {holds}"
                );
                checked += 1;
            }
        }
    }
    assert!(
        checked >= 40,
        "оракул сверил слишком мало вердиктов: {checked}"
    );
}

// --- Обход моделей ------------------------------------------------------------

/// Формулы вложенной модели проверяются против её графа, а не корневого.
#[test]
fn nested_model_formula_is_verified_against_its_own_states() {
    // В модели Engine состояние Stop достижимо, в корне такого состояния нет.
    let src = "model Engine { : [LTL] F Stop; start Run { ref Stop; } state Stop; } \
               start Main = Engine;";
    let (ast, _) = parse(src, 0).unwrap();
    let model = construct_model(&ast, None, &[]).unwrap();

    let results = takt_lang::verify_all(model);
    assert_eq!(results.len(), 1);
    assert_eq!(
        results[0].model, "Engine",
        "формула принадлежит вложенной модели"
    );
    assert_eq!(
        results[0].verdict,
        Verdict::Holds,
        "F Stop проверяется по состояниям Engine: {:?}",
        results[0].verdict
    );
}

/// Модель без LTL-формул даёт пустой список (а не вердикт "держится").
#[test]
fn model_without_formulas_yields_no_results() {
    let (ast, _) = parse("start A { ref B; } state B;", 0).unwrap();
    let model = construct_model(&ast, None, &[]).unwrap();
    assert!(takt_lang::verify_all(model).is_empty());
}

// --- Проверка завершается на корпусе examples/ ----------------------------

/// Верификация завершается на всех примерах корпуса (конечность Крипке).
///
/// Тест против незавершения: структура Крипке конечна by construction (|K| = число
/// состояний), поэтому проверка обязана заканчиваться на любой модели корпуса. Свойство
/// берётся одно и то же - достижимость стартового состояния; важен факт завершения и
/// вердикта, а не его значение.
#[test]
fn verification_terminates_on_examples_corpus() {
    let mut checked = 0usize;
    for entry in std::fs::read_dir("../examples").expect("каталог examples/") {
        let path = entry.expect("запись каталога").path();
        if path.extension().is_none_or(|e| e != "takt") {
            continue;
        }
        let source = std::fs::read_to_string(&path).expect("чтение примера");
        let Ok((ast, _)) = parse(&source, 0) else {
            continue; // синтаксис примера - забота других тестов
        };
        let Ok(model) = construct_model(&ast, None, &["../examples".to_string()]) else {
            continue; // семантика примера - забота других тестов
        };

        // Свойство над реальным именем состояния корневой модели.
        let start_name = {
            let borrowed = model.borrow();
            borrowed.states.keys().next().cloned()
        };
        let Some(state) = start_name else { continue };
        let phi = takt_lang::parse_ltl_property(&format!("F {state}")).expect("F <состояние>");

        // Сам факт возврата - и есть проверка завершаемости.
        let _verdict = takt_lang::verify_model(Rc::clone(&model), &phi);
        checked += 1;
    }
    assert!(
        checked > 0,
        "корпус examples/ пуст — тест ничего не проверил"
    );
}

// --- Область формулы состояния ------------------------------
//
// Формула в теле состояния `S` - сокращение для `G (S -> φ)`. Тесты - на вердикт:
// именно он отличает новую семантику от прежней "проверять от старта".

/// Пример: `: [LTL] F Idle;` внутри Fault держится - возврат гарантирован.
#[test]
fn state_scoped_formula_holds_when_return_is_guaranteed() {
    assert_eq!(verdict_of("state_scope_holds.takt"), Verdict::Holds);
}

/// Контрпример и **тест семантики области**: залипание в Fault обязано быть
/// нарушением.
///
/// До формула проверялась от стартового состояния, и этот тест краснел бы вердиктом
/// `Holds`: старт - Idle, поэтому `F Idle` истинно тривиально, а самопетля Fault
/// оставалась незамеченной. То есть тест ловит ровно тот вопрос, которого автор не
/// задавал.
#[test]
fn state_scoped_formula_violated_when_state_can_stick() {
    let v = verdict_of("state_scope_fails.takt");
    let Verdict::Violated(cex) = v else {
        panic!("залипание в Fault нарушает «из Fault вернёмся в Idle»: получено {v:?}");
    };
    assert!(
        cex.cycle.iter().all(|s| s == "Fault"),
        "контрпример — вечное залипание в Fault: {}",
        cex.trace()
    );
}

/// Десахаризация видна пользователю: печатается проверенная формула `G (Fault -> F
/// Idle)`, а не авторское `F Idle`.
///
/// Иначе вывод `taktc verify` врал бы об области: пользователь читал бы "свойство
/// Нарушено: F Idle" и искал недостижимость Idle от старта.
#[test]
fn state_scoped_formula_is_reported_desugared() {
    let results = takt_lang::verify_all(model_of("state_scope_fails.takt"));
    assert_eq!(results.len(), 1);
    assert_eq!(results[0].formula.to_string(), "G (Fault -> F Idle)");
}

/// Область - свойство состояния, а не текста: формула уровня модели не связывается
/// ничем и проверяется от старта, как и прежде.
#[test]
fn model_level_formula_is_not_scoped() {
    let results = takt_lang::verify_all(model_of("fails.takt"));
    assert_eq!(results.len(), 1);
    assert_eq!(results[0].formula.to_string(), "G (Fault -> F Idle)");
    assert!(matches!(results[0].verdict, Verdict::Violated(_)));
}

/// Формула в именованном блоке состояния (`enter`/`always`) - та же область: блок
/// исполняется, лишь когда автомат в состоянии.
#[test]
fn formula_in_state_named_block_is_scoped_to_the_state() {
    let src = "start Idle { ref Fault; } \
               state Fault { enter { : [LTL] F Idle; } ref Fault; ref Idle; }";
    let (ast, _) = parse(src, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let results = takt_lang::verify_all(model);
    assert_eq!(results.len(), 1);
    assert_eq!(results[0].formula.to_string(), "G (Fault -> F Idle)");
}

// --- Область проверки: импорты -----------------------------------
//
// Тесты - на вердикт и на состав пропущенного, а не на поле `origin`: признак в дереве -
// средство, а пользователь видит код возврата.

/// Модель из `import` с фикстурами-импортами (нужны search_paths).
fn model_with_imports(fixture: &str) -> Rc<RefCell<takt_lang::semantic::ModelNode>> {
    let path = format!("tests/data/verify/{fixture}");
    let source = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{path}: {e}"));
    let (ast, _) = parse(&source, 0).unwrap_or_else(|d| panic!("{path}: разбор — {d:?}"));
    construct_model(&ast, None, &["tests/data/verify".to_string()])
        .unwrap_or_else(|d| panic!("{path}: семантика — {d:?}"))
}

/// `import "файл";` - нарушитель из импорта не попадает в вердикт файла.
#[test]
fn plain_import_is_out_of_scope_by_default() {
    let outcome = takt_lang::verify_all_scoped(
        model_with_imports("scope_plain.takt"),
        false,
        takt_lang::VerifyScope::File,
    );
    assert!(
        outcome.results.iter().all(|r| r.verdict == Verdict::Holds),
        "свойства своего файла держатся; чужие не проверяются: {:?}",
        outcome.results
    );
    assert_eq!(outcome.skipped, vec!["Badlib".to_string()]);
}

/// `--scope all` возвращает поведение 0049 дословно.
#[test]
fn plain_import_is_verified_with_scope_all() {
    let outcome = takt_lang::verify_all_scoped(
        model_with_imports("scope_plain.takt"),
        false,
        takt_lang::VerifyScope::All,
    );
    assert!(
        outcome
            .results
            .iter()
            .any(|r| matches!(r.verdict, Verdict::Violated(_))),
        "badlib.takt нарушает свои свойства — при scope=all это обязано всплыть"
    );
    assert!(
        outcome.skipped.is_empty(),
        "при scope=all пропускать нечего"
    );
}

/// A3/Р1: форма `import { A as B }` - узел приходит `Rc::clone`-ом чужого дерева и его
/// собственный `origin` - `Local`; пометить обязан импортёр.
///
/// Забудь пометку - и область молча не сработает именно на этой форме.
#[test]
fn rename_import_is_out_of_scope_by_default() {
    let outcome = takt_lang::verify_all_scoped(
        model_with_imports("scope_rename.takt"),
        false,
        takt_lang::VerifyScope::File,
    );
    assert_eq!(outcome.skipped, vec!["Motor".to_string()]);
    assert!(outcome.results.iter().all(|r| r.verdict == Verdict::Holds));
}

/// A2/Р3: **контрпример к сужению** - локальная вложенная модель проверяется при любой
/// области.
///
/// Без него "сузили область" неотличимо от "перестали проверять вложенные модели
/// вообще".
#[test]
fn local_nested_model_is_always_verified() {
    for scope in [takt_lang::VerifyScope::File, takt_lang::VerifyScope::All] {
        let outcome =
            takt_lang::verify_all_scoped(model_of("scope_local_nested.takt"), false, scope);
        assert!(
            outcome
                .results
                .iter()
                .any(|r| matches!(r.verdict, Verdict::Violated(_))),
            "своя вложенная модель нарушает свойство — область {scope:?} не вправе её скрыть"
        );
        assert!(
            outcome.skipped.is_empty(),
            "локальная модель не пропускается"
        );
    }
}

/// Р2: отсечение - поддеревом целиком, а не по одному узлу.
///
/// У `badlib.takt` формулы есть и в корне, и во вложенной `Engine`. Вложенная несёт
/// `origin = Local` (она локальна для своего файла), поэтому обход, проверяющий признак
/// поузлово, зашёл бы внутрь и проверил её.
#[test]
fn imported_subtree_is_cut_entirely() {
    let outcome = takt_lang::verify_all_scoped(
        model_with_imports("scope_plain.takt"),
        false,
        takt_lang::VerifyScope::File,
    );
    assert!(
        !outcome.results.iter().any(|r| r.model == "Engine"),
        "Engine — вложенная модель ИМПОРТИРОВАННОГО файла: поддерево отсекается целиком, \
         получено {:?}",
        outcome.results.iter().map(|r| &r.model).collect::<Vec<_>>()
    );
}

/// Умолчание публичного `verify_all` - область `file`.
#[test]
fn verify_all_defaults_to_file_scope() {
    let results = takt_lang::verify_all(model_with_imports("scope_plain.takt"));
    assert!(results.iter().all(|r| r.verdict == Verdict::Holds));
}
