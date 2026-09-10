//! Табличная форма автомата у цели `c`.
//!
//! # Что доказывает набор
//!
//! 1. **Форма печатается**: таблица `..._TRANSITIONS[]`, стражи, действия и
//!    диспетчер - и переходы уходят из тел `case` (в теле не остаётся ни
//!    присваивания состояния, ни условия ребра).
//! 2. **Умолчание не изменилось**: без флага вывод прежний, таблицы в нём нет.
//!    Это контроль, без которого "форма печатается" ничего не значит.
//! 3. **Вывод собирается флагами проверки цели** (`cc -Wall -Wextra -Werror`):
//!    указатели на функции, заглушки неиспользуемых параметров и приведение
//!    `int` ↔ перечисление - те места, где `cc` возражает молча у автора и
//!    громко в проверке.
//! 4. **Композиция выражается таблицей** - и параллельная, и последовательная
//!    страж строки состояния-цепочки есть условие "цепочка на
//!    последнем шаге, и он завершён", то есть ровно то, под которым переход
//!    печатает форма `switch`. Машина шагов при этом остаётся в теле такта: она
//!    ведёт переходы внутри состояния.
//! 5. **Флаг у чужой цели - ошибка CLI**: `--fsm=table` с целью `st`
//!    отвергается с перечислением поддерживающих целей. Молчаливо
//!    проигнорированный флаг означал бы "форма как получится".
//!
//! Тождественность поведения обеих форм этим набором **не** доказывается: таблица,
//! переставившая две строки, собирается тем же `cc` без замечаний. Это предмет
//! потактовой сверки `conformance_fsm_table_tests` (крейт `takt-sim`).

use std::path::{Path, PathBuf};
use std::process::Command;

/// Простой автомат: условное ребро, блоки `enter`/`exit`, самопереход.
const SIMPLE: &str = "\
model Counter {
    var n: u8 := 0;
    out probe: u8;

    start Low {
        always {
            n := n + 1;
            probe := n;
        }
        ref High: n = 3;
    }

    state High {
        enter {
            probe := 100;
        }
        always {
            n := n + 1;
            probe := n;
        }
        ref Low: n = 6;
        exit {
            probe := 200;
        }
    }
}
start Main = Counter;
";

/// Последовательная композиция: машина шагов остаётся в теле, выход из состояния -
/// строка таблицы.
const CHAIN: &str = "\
model First {
    out a: u8;
    start Go {
        always {
            a := 1;
        }
        next Done;
    }
    state Done;
}

model Second {
    out b: u8;
    start Go {
        always {
            b := 2;
        }
        next Done;
    }
    state Done;
}

model Both {
    start Chain = First + Second;
}
start Main = Both;
";

/// Параллельная композиция: выход из состояния - тоже строка таблицы, а не переход в
/// теле.
const PARALLEL: &str = "\
model Ping {
    out ping: u8;
    start Go {
        always {
            ping := 1;
        }
        next Fin;
    }
    state Fin;
}

model Pong {
    out pong: u8;
    start Go {
        always {
            pong := 2;
        }
        next Fin;
    }
    state Fin;
}

model Pair {
    var k: u8 := 0;
    out probe: u8;

    start Both = Ping | Pong {
        next Tail;
    }

    state Tail {
        always {
            k := k + 1;
            probe := k;
        }
        ref Tail: k > 0;
    }
}
start Main = Pair;
";

fn taktc() -> Command {
    Command::new(env!("CARGO_BIN_EXE_taktc"))
}

/// Уникальный по тесту каталог (: тесты идут параллельно, и имя потока уникально лишь
/// внутри процесса - отсюда звено `takt_pid<PID>`).
fn work_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("main")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0435_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог теста");
    dir
}

/// Компилирует исходник целью `c` и отдаёт `(код возврата, stderr, текст .c)`.
fn compile(dir: &Path, source: &str, extra: &[&str]) -> (bool, String, String) {
    let input = dir.join("probe.takt");
    std::fs::write(&input, source).expect("запись пробы");
    let out = taktc()
        .arg("compile")
        .args(extra)
        .arg(&input)
        .arg("-o")
        .arg(dir.join("out"))
        .output()
        .expect("запуск taktc compile");
    let text = std::fs::read_to_string(dir.join("out").join("probe.c")).unwrap_or_default();
    (
        out.status.success(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
        text,
    )
}

/// Тело `switch` порождённого файла - от `switch (model->state)` до строки вызова
/// диспетчера либо конца функции.
fn switch_bodies(source: &str) -> String {
    let mut inside = false;
    let mut collected = String::new();
    for line in source.lines() {
        if line.contains("switch (model->state)") {
            inside = true;
            continue;
        }
        if inside && line.trim() == "}" && line.starts_with("    }") {
            inside = false;
            continue;
        }
        if inside {
            collected.push_str(line);
            collected.push('\n');
        }
    }
    collected
}

#[test]
fn table_form_prints_transitions_as_data() {
    let dir = work_dir("prints");
    let (ok, stderr, text) = compile(&dir, SIMPLE, &["--fsm=table"]);
    assert!(ok, "компиляция табличной формы: {stderr}");
    assert!(
        text.contains("PROBE_COUNTER_TRANSITIONS[]"),
        "нет таблицы переходов:\n{text}"
    );
    assert!(
        text.contains("static void ProbeCounter_dispatch("),
        "нет диспетчера:\n{text}"
    );
    assert!(
        text.contains("static bool ProbeCounter_guard_"),
        "нет функции-стража:\n{text}"
    );
    assert!(
        text.contains("static void ProbeCounter_action_"),
        "нет функции-действия (блоки enter/exit):\n{text}"
    );
    // Строка таблицы читается глазами: откуда -> страж -> действие -> куда.
    assert!(
        text.contains("{ PROBE_COUNTER_LOW, ProbeCounter_guard_1, ProbeCounter_action_1, PROBE_COUNTER_HIGH },"),
        "строка таблицы не в ожидаемой форме:\n{text}"
    );
    assert!(
        text.contains("ProbeCounter_dispatch(model, main);"),
        "такт не зовёт диспетчер:\n{text}"
    );
}

#[test]
fn table_form_empties_case_bodies_of_transitions() {
    let dir = work_dir("bodies");
    let (ok, stderr, text) = compile(&dir, SIMPLE, &["--fsm=table"]);
    assert!(ok, "компиляция табличной формы: {stderr}");
    let bodies = switch_bodies(&text);
    assert!(
        !bodies.contains("model->state ="),
        "переход остался в теле case:\n{bodies}"
    );
    assert!(
        !bodies.contains("if (model->n == 3)"),
        "условие ребра осталось в теле case:\n{bodies}"
    );
    // Контроль: тело состояния на месте - таблицей выражается переход, а не содержимое
    // такта.
    assert!(
        bodies.contains("model->n = model->n + 1;"),
        "тело состояния потеряно:\n{bodies}"
    );
}

#[test]
fn default_form_has_no_table() {
    let dir = work_dir("default");
    let (ok, stderr, text) = compile(&dir, SIMPLE, &[]);
    assert!(ok, "компиляция формы по умолчанию: {stderr}");
    assert!(
        !text.contains("_TRANSITIONS"),
        "умолчание изменилось — в выводе появилась таблица:\n{text}"
    );
    assert!(
        text.contains("if (model->n == 3) {"),
        "форма по умолчанию потеряла условие ребра:\n{text}"
    );
}

#[test]
fn table_form_output_compiles_with_gate_flags() {
    let dir = work_dir("cc");
    let (ok, stderr, _) = compile(&dir, SIMPLE, &["--fsm=table"]);
    assert!(ok, "компиляция табличной формы: {stderr}");
    let cc = Command::new("cc")
        .args([
            "-std=c11",
            "-Wall",
            "-Wextra",
            "-Wno-unused-parameter",
            "-Werror",
            "-c",
        ])
        .arg(dir.join("out").join("probe.c"))
        .arg("-I")
        .arg(dir.join("out"))
        .arg("-o")
        .arg(dir.join("probe.o"))
        .output();
    let Ok(cc) = cc else {
        eprintln!("cc недоступен — шаг пропущен");
        return;
    };
    assert!(
        cc.status.success(),
        "cc не собрал табличную форму флагами гейта цели:\n{}",
        String::from_utf8_lossy(&cc.stderr)
    );
}

#[test]
fn chain_state_becomes_a_table_row() {
    let dir = work_dir("chain");
    let (ok, stderr, text) = compile(&dir, CHAIN, &["--fsm=table"]);
    assert!(ok, "цепочка обязана переводиться таблицей: {stderr}");
    // Страж строки - условие внешнего перехода формы `switch`: "цепочка на последнем
    // шаге, и он завершён".
    assert!(
        text.contains("model->chain_state == PROBE_BOTH_CHAIN_SECOND1")
            && text.contains("ProbeSecond_is_done(&model->chain_second1)"),
        "страж строки не проверяет последний шаг цепочки:\n{text}"
    );
    // Машина шагов осталась в теле такта: она ведёт переходы внутри состояния, и
    // таблица её не заменяет.
    let bodies = switch_bodies(&text);
    assert!(
        bodies.contains("model->chain_state =") && bodies.contains("ProbeFirst_tick("),
        "машина шагов пропала из тела такта:\n{bodies}"
    );
    // Контроль: наружу состояние уходит только строкой таблицы.
    assert!(
        !bodies.contains("model->state ="),
        "внешний переход остался в теле case:\n{bodies}"
    );
}

#[test]
fn chain_table_output_compiles_with_gate_flags() {
    let dir = work_dir("chain_cc");
    let (ok, stderr, _) = compile(&dir, CHAIN, &["--fsm=table"]);
    assert!(ok, "компиляция цепочки табличной формой: {stderr}");
    let cc = Command::new("cc")
        .args([
            "-std=c11",
            "-Wall",
            "-Wextra",
            "-Wno-unused-parameter",
            "-Werror",
            "-c",
        ])
        .arg(dir.join("out").join("probe.c"))
        .arg("-I")
        .arg(dir.join("out"))
        .arg("-o")
        .arg(dir.join("probe.o"))
        .output();
    let Ok(cc) = cc else {
        eprintln!("cc недоступен — шаг пропущен");
        return;
    };
    assert!(
        cc.status.success(),
        "cc не собрал табличную форму цепочки флагами гейта цели:\n{}",
        String::from_utf8_lossy(&cc.stderr)
    );
}

#[test]
fn unknown_form_is_refused_with_list() {
    let dir = work_dir("unknown");
    let (ok, stderr, _) = compile(&dir, SIMPLE, &["--fsm=matrix"]);
    assert!(!ok, "неизвестная форма обязана быть ошибкой:\n{stderr}");
    assert!(
        stderr.contains("switch") && stderr.contains("table"),
        "ошибка не перечисляет допустимые формы:\n{stderr}"
    );
}

#[test]
fn parallel_state_leaves_no_transition_in_the_body() {
    let dir = work_dir("parallel");
    let (ok, stderr, text) = compile(&dir, PARALLEL, &["--fsm=table"]);
    assert!(ok, "параллель обязана переводиться таблицей: {stderr}");
    // Страж строки - конъюнкция готовностей ветвей.
    assert!(
        text.contains("ProbePing_is_done(&model->both.ping0)")
            && text.contains("ProbePong_is_done(&model->both.pong1)"),
        "страж строки не проверяет обе ветви параллели:\n{text}"
    );
    let bodies = switch_bodies(&text);
    // Тик ветвей остался в теле, а переход - нет.
    assert!(
        bodies.contains("ProbePing_tick("),
        "тик ветви пропал из тела такта:\n{bodies}"
    );
    assert!(
        !bodies.contains("model->state ="),
        "внешний переход остался в теле case — он напечатан ДВАЖДЫ (и телом, и \
         таблицей); трассы этого не видят, потому что тело меняет состояние \
         раньше, чем диспетчер до него доходит:\n{bodies}"
    );
}
