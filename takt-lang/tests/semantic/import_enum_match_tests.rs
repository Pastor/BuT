//! Вариант импортированного перечисления в образце `match` -.
//!
//! # Что здесь проверяется
//!
//!
//! **Поэтому здесь стоит тест, а не починка.** Корпус класс не покрывает:
//! перечисления в `examples/` границу импорта не пересекают, а существующий тест исправления
//! проверяет перечисление только в **инициализаторе** объявления (`var m: Mode :=
//! Auto;`). Путь образца `match` не проверял никто - то есть вылеченный класс мог
//! вернуться молча.

use std::path::{Path, PathBuf};
use std::process::Command;

/// Библиотека: одно перечисление, ни одного состояния.
const LIBRARY: &str = "\
enum Op {
    Lda,
    Hlt
}
";

/// Применение с **выборочным** импортом: `match` по варианту библиотеки.
const SELECTIVE: &str = "\
import {Op} from \"library.takt\";

var v: Op := Lda;
var seen: u8 := 0;

start Run {
    always {
        match v {
            Lda => { seen := 1; }
            Hlt => { seen := 2; }
        }
    }
    ref Done: v = Hlt;
}
state Done;
";

/// То же **целым файлом**: вторая форма импорта - второй путь переноса.
const WHOLE_FILE: &str = "\
import \"library.takt\";

var v: Op := Hlt;
var seen: u8 := 0;

start Run {
    always {
        match v {
            Lda => { seen := 1; }
            Hlt => { seen := 2; }
        }
    }
    ref Done;
}
state Done;
";

/// Контр-пример: варианта `Nop` в перечислении нет.
const UNKNOWN_VARIANT: &str = "\
import {Op} from \"library.takt\";

var v: Op := Lda;
var seen: u8 := 0;

start Run {
    always {
        match v {
            Nop => { seen := 1; }
        }
    }
    ref Done;
}
state Done;
";

/// Уникальный по тесту каталог.
fn work_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("main")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0206_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог теста");
    dir
}

/// Готовит пару "библиотека + применение" и компилирует применение целью `c`.
fn compile_c(tag: &str, application: &str) -> (PathBuf, String, i32) {
    let dir = work_dir(tag);
    std::fs::write(dir.join("library.takt"), LIBRARY).expect("запись библиотеки");
    let app = dir.join("app.takt");
    std::fs::write(&app, application).expect("запись применения");
    let out = dir.join("out");
    let result = Command::new(env!("CARGO_BIN_EXE_taktc"))
        .arg("compile")
        .arg("-t")
        .arg("c")
        .arg(&app)
        .arg("-o")
        .arg(&out)
        .output()
        .expect("запуск taktc");
    (
        out,
        String::from_utf8_lossy(&result.stderr).into_owned(),
        result.status.code().unwrap_or(-1),
    )
}

/// Текст порождённого `.c`.
fn generated_c(out: &Path) -> String {
    std::fs::read_to_string(out.join("app.c")).expect("порождённый .c")
}

/// **T1.** Выборочный импорт: обе ветви `match` доезжают до вывода **разными**
/// номерами вариантов.
#[test]
fn selective_import_variants_reach_generated_switch() {
    let (out, stderr, code) = compile_c("selective", SELECTIVE);
    assert_eq!(code, 0, "трансляция обязана удаться:\n{stderr}");
    let text = generated_c(&out);
    assert!(
        text.contains("case 0:") && text.contains("case 1:"),
        "варианты обязаны разойтись по номерам:\n{text}"
    );
    assert!(
        text.contains("seen = 1") && text.contains("seen = 2"),
        "тела ветвей обязаны попасть в вывод:\n{text}"
    );
}

/// **T2.** Импорт целым файлом - та же запись, другой путь переноса.
#[test]
fn whole_file_import_variants_reach_generated_switch() {
    let (out, stderr, code) = compile_c("whole", WHOLE_FILE);
    assert_eq!(code, 0, "трансляция обязана удаться:\n{stderr}");
    let text = generated_c(&out);
    assert!(
        text.contains("case 0:") && text.contains("case 1:"),
        "варианты обязаны разойтись по номерам:\n{text}"
    );
}

/// **T3. Контр-пример.** Несуществующий вариант - по-прежнему ошибка.
///
/// Без него тест доказывал бы лишь то, что образец **что-нибудь** принимает:
/// разрешение "всякого имени в образце" прошло бы T1 и T2.
#[test]
fn unknown_variant_is_still_rejected() {
    let (_, stderr, code) = compile_c("unknown", UNKNOWN_VARIANT);
    assert_ne!(code, 0, "несуществующий вариант обязан быть отвергнут");
    assert!(
        stderr.contains("Nop"),
        "диагностика обязана назвать имя из образца:\n{stderr}"
    );
}

/// **T4.** Условие ребра на том же варианте - путь, который карточка называла
/// работающим.
///
/// Проверяется вместе с `match`: до исправления **обе** формы падали, и асимметрия,
/// записанная в карточке, на этом входе не воспроизводится.
#[test]
fn edge_condition_on_imported_variant_reaches_output() {
    let (out, stderr, code) = compile_c("cond", SELECTIVE);
    assert_eq!(code, 0, "{stderr}");
    let text = generated_c(&out);
    assert!(
        text.contains("model->v == 1"),
        "условие ребра обязано сравниться с номером варианта:\n{text}"
    );
}
