//! Усыновление импортированного поддерева.
//!
//! Предмет: объявление, пришедшее `import`ом, обязано вести себя **точно так же**, как
//! объявленное в импортёре. До фичи это было не так, и пять потребителей семантического
//! дерева расходились на одном входе: цели `c` и `sv` печатали доступ "как к своей
//! переменной" и порождали невалидный вывод (при рапорте `taktc` об успехе), цели
//! `rust`/`st` были случайно правы, симулятор падал `SIM-009`.
//!
//! Здесь проверяются **текст порождённого кода** и **диагностики**: потактовая сверка
//! значений живёт в `takt-sim/tests/conformance_c_import_tests.rs` - компиляция
//! доказывает валидность, но не верность.

use std::path::{Path, PathBuf};
use std::process::Command;
use takt_lang::GenerateOptions;

/// Библиотека: объявления контура на верхнем уровне + модель, которая их пишет.
const LIB: &str = r#"
var ctrl: u8 := 0;
var meas: u8 := 0;

model Pid {
    start Control {
        always {
            ctrl := meas + 1;
        }
    }
}
"#;

/// Применение: импортирует модель **и** объявления, замыкает связь своей моделью.
const APP: &str = r#"
import { Pid, ctrl, meas } from "lib.takt";

model Plant {
    out lvl: u8;
    start Run {
        always {
            meas := meas + ctrl;
            lvl := meas;
        }
    }
}

start App = Pid | Plant;
"#;

/// Готовит каталог с библиотекой и применением, возвращает путь каталога.
fn fixture_dir(tag: &str, lib: &str, app: &str) -> PathBuf {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0184_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание каталога фикстуры");
    std::fs::write(dir.join("lib.takt"), lib).expect("запись библиотеки");
    std::fs::write(dir.join("app.takt"), app).expect("запись применения");
    dir
}

/// Компилирует применение в C; возвращает текст порождённого `.c`.
fn compile_app_to_c(dir: &Path, name: &str) -> Result<String, String> {
    let source = std::fs::read_to_string(dir.join("app.takt")).expect("чтение применения");
    takt_lang::compile_to_c(
        name,
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[dir.to_string_lossy().into_owned()],
        &GenerateOptions::default(),
    )
    .map_err(|d| format!("{d:?}"))?;
    Ok(std::fs::read_to_string(dir.join(format!("{name}.c"))).expect("чтение порождённого C"))
}

/// Строит семантическое дерево применения (для проверки диагностик).
fn build_app(dir: &Path) -> Result<(), String> {
    let source = std::fs::read_to_string(dir.join("app.takt")).expect("чтение применения");
    let (ast, _) = takt_lang::parse(&source, 0).map_err(|d| format!("{d:?}"))?;
    takt_lang::semantic::tree::construct_model(&ast, None, &[dir.to_string_lossy().into_owned()])
        .map(|_| ())
        .map_err(|d| d.message.clone())
}

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Компилирует порождённый C настоящим `cc` под теми же флагами, что и проверка корпуса.
/// Без этого тест доказывал бы лишь текст, а дефект был именно в том, что текст **не
/// собирается**.
fn cc_accepts(dir: &Path, name: &str) {
    if !cc_available() {
        eprintln!("[ПРОПУСК] проверка `cc`: компилятор не найден");
        return;
    }
    let out = Command::new("cc")
        .args(["-std=c11", "-Wall", "-Werror", "-c"])
        .arg(dir.join(format!("{name}.c")))
        .arg("-I")
        .arg(dir)
        .arg("-o")
        .arg(dir.join(format!("{name}.o")))
        .output()
        .expect("запуск cc");
    assert!(
        out.status.success(),
        "порождённый C не компилируется:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
}

/// **R1/R3: импортированное объявление - переменная корня, а не под-модели.**
///
/// В цели `c` это видно буквально: под-модель обращается к переменной корня через
/// `main->`, к своей - через `model->`. До фичи печаталось `model->meas` в обеих
/// под-моделях, и `cc` отвергал вывод (`no member named 'meas'`).
#[test]
fn imported_variable_is_accessed_through_root_in_c() {
    let dir = fixture_dir("root_access", LIB, APP);
    let c = compile_app_to_c(&dir, "app").expect("применение компилируется в C");
    assert!(
        c.contains("main->ctrl = main->meas + 1;"),
        "импортированная модель обязана обращаться к переменной корня через `main->`:\n{c}"
    );
    assert!(
        c.contains("main->meas = main->meas + main->ctrl;"),
        "локальная модель импортёра — так же:\n{c}"
    );
    // Проверяется отсутствие именно ошибочных форм из тел под-моделей. Голое
    // `model->meas` в выводе законно: так корень обращается к своим переменным в
    // `App_init`/`App_reset`, и там `model` - сам корень.
    assert!(
        !c.contains("model->ctrl = model->meas + 1;")
            && !c.contains("model->meas = model->meas + model->ctrl;"),
        "доступа «как к своей переменной» в телах под-моделей быть не должно — \
         именно он не компилируется:\n{c}"
    );
    cc_accepts(&dir, "app");
}

/// **R5: частичный импорт не молчит.** Модель импортирована без объявлений, на
/// которые ссылается её тело, - это `SE-074`, а не повисшая привязка (которая
/// раньше давала невалидный C при рапорте об успехе).
#[test]
fn partial_import_is_se074() {
    let app = r#"
import { Pid } from "lib.takt";
start App = Pid;
"#;
    let dir = fixture_dir("partial", LIB, app);
    let err = build_app(&dir).expect_err("частичный импорт обязан отказать");
    assert!(
        err.contains("не импортированные вместе с ней"),
        "ожидалась диагностика о неимпортированных объявлениях, получено: {err}"
    );
    assert!(
        err.contains("'ctrl'") && err.contains("'meas'"),
        "диагностика обязана назвать ВСЕ пропущенные объявления, а не первое: {err}"
    );
}

/// **R7: область видимости не расширилась.** Переменная импортёра библиотечной
/// модели по-прежнему не видна - `SE-003`. Это граница механизма, а не дефект:
/// библиотека не может зависеть от того, что объявит её потребитель.
#[test]
fn importer_variable_stays_invisible_to_library() {
    let lib = r#"
model Pid {
    start Control {
        always {
            ctrl := meas + 1;
        }
    }
}
"#;
    let app = r#"
import { Pid } from "lib.takt";
var ctrl: u8 := 0;
var meas: u8 := 0;
start App = Pid;
"#;
    let dir = fixture_dir("scope", lib, app);
    let err = build_app(&dir).expect_err("библиотека не видит объявлений импортёра");
    assert!(
        err.contains("не найден в области видимости"),
        "ожидалась SE-003 об области видимости, получено: {err}"
    );
}

/// **Алиас модели: `import { Pid as Loop }`.** Уникальное имя модели строится
/// обходом владельцев от имени узла, поэтому узел обязан носить имя, под которым
/// внесён в список импортёра. Иначе цель `c` ищет `App:Pid`, а зарегистрирована
/// `App:Loop` - `CC-004` (дефект существовал и до фичи).
#[test]
fn model_alias_compiles_in_c() {
    let app = r#"
import { Pid as Loop, ctrl, meas } from "lib.takt";

model Plant {
    out lvl: u8;
    start Run {
        always {
            meas := meas + ctrl;
            lvl := meas;
        }
    }
}

start App = Loop | Plant;
"#;
    let dir = fixture_dir("alias", LIB, app);
    let c = compile_app_to_c(&dir, "app").expect("импорт с алиасом модели компилируется");
    assert!(
        c.contains("main->ctrl = main->meas + 1;"),
        "тело модели под алиасом обязано остаться собой:\n{c}"
    );
    cc_accepts(&dir, "app");
}

/// **Псевдоним объявления: `import { meas as pv }`.** Тело импортированной
/// модели обязано ссылаться на имя, под которым объявление живёт у импортёра, -
/// иначе генератор напечатает доступ к несуществующему полю.
#[test]
fn declaration_alias_is_renamed_in_body() {
    let app = r#"
import { Pid, ctrl as u, meas as pv } from "lib.takt";

model Plant {
    out lvl: u8;
    start Run {
        always {
            pv := pv + u;
            lvl := pv;
        }
    }
}

start App = Pid | Plant;
"#;
    let dir = fixture_dir("var_alias", LIB, app);
    let c = compile_app_to_c(&dir, "app").expect("импорт с псевдонимом объявления компилируется");
    assert!(
        c.contains("main->u = main->pv + 1;"),
        "тело обязано ссылаться на псевдоним, а не на имя из библиотеки:\n{c}"
    );
    assert!(
        !c.contains("meas") && !c.contains("ctrl"),
        "имён библиотеки в выводе быть не должно:\n{c}"
    );
    cc_accepts(&dir, "app");
}

/// **Два импорта одного файла - два независимых состояния.** Каждая ветка
/// импорта строит своё дерево, поэтому переменные не склеиваются; проверяется
/// прямо: обе копии объявлены в корне и обе используются.
#[test]
fn two_imports_of_one_file_stay_independent() {
    let app = r#"
import { Pid as A, ctrl as ca, meas as ma } from "lib.takt";
import { Pid as B, ctrl as cb, meas as mb } from "lib.takt";

model Plant {
    out lvl: u8;
    start Run {
        always {
            ma := ma + ca;
            mb := mb + cb;
            lvl := ma + mb;
        }
    }
}

start App = A | B | Plant;
"#;
    let dir = fixture_dir("twice", LIB, app);
    let c = compile_app_to_c(&dir, "app").expect("двойной импорт компилируется");
    for field in ["ca", "ma", "cb", "mb"] {
        assert!(
            c.contains(&format!("main->{field}")),
            "копия '{field}' обязана быть самостоятельной переменной корня:\n{c}"
        );
    }
    cc_accepts(&dir, "app");
}

/// **R6: импорт файла целиком больше не отказывает.** Прежде цель `c` падала
/// `CC-004: Model with name ' ()' not found` - имя подключённого корня не
/// записывалось в узел.
#[test]
fn whole_file_import_compiles_in_c() {
    let lib = r#"
var z: u8 := 0;
model Inner {
    start S {
        always {
            z := z + 1;
        }
    }
}
start Top = Inner;
"#;
    let app = r#"
import "lib.takt";
model Watch {
    out probe: u8;
    start R {
        always {
            probe := 1;
        }
    }
}
start App = Lib | Watch;
"#;
    let dir = fixture_dir("whole", lib, app);
    let c = compile_app_to_c(&dir, "app").expect("импорт файла целиком компилируется");
    assert!(
        c.contains("main->z"),
        "объявление подключённого файла обязано стать переменной корня импортёра:\n{c}"
    );
    cc_accepts(&dir, "app");
}
