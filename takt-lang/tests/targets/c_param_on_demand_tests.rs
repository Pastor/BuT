//! Указатель на состояние печатается по нужде.

use std::path::PathBuf;
use std::process::Command;
use takt_lang::generator::GenerateOptions;

/// Функция без обращения к состоянию - и вторая, с обращением: контроль обязателен,
/// иначе правка читается как "параметр не печатается никогда".
const SRC: &str = "var acc: u8 := 0;\n\
     fn constant() -> u8 {\n    return 7;\n}\n\
     fn accumulated() -> u8 {\n    return acc + 1;\n}\n\
     var o: u8 := 0;\nout probe: u8 at 0;\n\
     start Run {\n    always {\n        o := constant() + accumulated();\n\
     \x20       probe := o;\n    }\n    ref Run;\n}\n";

fn out_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0396_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог вывода");
    dir
}

fn generate(tag: &str, src: &str) -> (PathBuf, String) {
    let dir = out_dir(tag);
    takt_lang::compile_to_c(
        tag,
        src,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение C");
    let text = std::fs::read_to_string(dir.join(format!("{tag}.c"))).expect("чтение вывода");
    (dir, text)
}

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Предмет: функция, не трогающая состояние, параметра не получает.
#[test]
fn function_without_state_has_no_pointer() {
    let (_, text) = generate("cp0396", SRC);
    assert!(
        text.contains("static uint8_t Cp0396_constant(void)"),
        "функция без обращения к состоянию обязана печататься без параметра:\n{text}"
    );
    assert!(
        !text.contains("(void)model;"),
        "заглушка не нужна там, где параметра нет:\n{text}"
    );
}

/// **Контроль:** функция, читающая переменную модели, параметр получает.
#[test]
fn function_with_state_keeps_the_pointer() {
    let (_, text) = generate("cp0396c", SRC);
    assert!(
        text.contains("_accumulated(const ") && text.contains("*model)"),
        "функция, читающая переменную модели, обязана получить указатель:\n{text}"
    );
}

/// Пустой список параметров печатается `void`, а не пустотой.
///
/// `f()` в C означает "список неизвестен" (K&R), и `-Wstrict-prototypes` отвечает "a
/// function declaration without a prototype is deprecated". До фичи случай не возникал -
/// указатель стоял всегда.
#[test]
fn empty_parameter_list_is_void() {
    let (dir, text) = generate("cp0396v", SRC);
    assert!(
        text.contains("(void);") && text.contains("(void) {"),
        "пустой список обязан печататься `void`:\n{text}"
    );
    if !cc_available() {
        eprintln!("[ПРОПУСК] `cc` не найден; текст вывода уже проверен");
        return;
    }
    let out = Command::new("cc")
        .args([
            "-std=c11",
            "-Wall",
            "-Wextra",
            "-Wstrict-prototypes",
            "-Wno-unused-parameter",
            "-Werror",
            "-c",
        ])
        .arg("-I")
        .arg(&dir)
        .arg(dir.join("cp0396v.c"))
        .arg("-o")
        .arg(dir.join("cp0396v.o"))
        .output()
        .expect("запуск cc");
    assert!(
        out.status.success(),
        "порождённый C обязан собираться под -Wstrict-prototypes:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
}

/// Под-модель без обращения к корню параметра `main` не получает.
///
/// Порт требует `main` **всегда**, даже объявленный в самой под-модели: колбэки HAL
/// живут в структуре корня. Поэтому у фикстуры портов нет.
#[test]
fn submodel_without_root_access_has_no_main() {
    let src = "model Child {\n    var n: u8 := 0;\n\
         \x20   start Go { always { n := n + 1; } ref Go; }\n}\n\
         start Main = Child;\n";
    let (_, text) = generate("cp0396s", src);
    assert!(
        text.contains("Child_tick(Cp0396sChild *model);"),
        "под-модель без обращения к корню обязана печататься без `main`:\n{text}"
    );
    assert!(
        text.contains("Child_tick(&model->main);"),
        "вызов обязан согласоваться с сигнатурой:\n{text}"
    );
}

// --- Вызов из условия -------------------------------------------
//
// Признак нужды один, а печатников вызова у цели `c` **два**: выражения и условия.
// Второй передавал указатель безусловно, и функция без обращения к состоянию
// объявлялась без него, а из условия ребра звалась с ним - `cc` отвечал "too many
// arguments to function call" при нулевом коде возврата `taktc`. Проверка цели класса не
// видел: в корпусе такого вызова нет.

/// Вход: обе функции зовутся только из условий рёбер.
const COND_SRC: &str = "out probe: u8 at 0;\nvar ticks: u8 := 0;\n\
     fn twice(v: u8) -> u8 { return v + v; }\n\
     fn accumulated() -> u8 { return ticks + 1; }\n\
     start Run {\n    always { ticks := ticks + 1; probe := ticks; }\n\
     \x20   ref Hot: twice(ticks) > 3;\n    ref Run: accumulated() < 100;\n}\n\
     state Hot { always { probe := 9; } ref Hot; }\n";

/// Предмет: вызов из условия не передаёт указатель, которого нет в сигнатуре.
#[test]
fn condition_call_omits_pointer_when_unused() {
    let (_, text) = generate("cp0502", COND_SRC);
    assert!(
        text.contains("Cp0502_twice(model->ticks)"),
        "вызов из условия обязан согласоваться с сигнатурой:\n{text}"
    );
}

/// **Контроль:** функция, читающая переменную модели, указатель получает и в
/// условии - иначе правка читалась бы как "в условиях не передавать никогда".
#[test]
fn condition_call_keeps_pointer_when_needed() {
    let (_, text) = generate("cp0502c", COND_SRC);
    assert!(
        text.contains("Cp0502c_accumulated(model)"),
        "функция, читающая состояние, обязана получить указатель:\n{text}"
    );
}

/// Вывод принимает `cc` флагами проверки цели - главный тест класса.
#[test]
fn condition_call_output_compiles() {
    if !cc_available() {
        eprintln!("[ПРОПУСК] `cc` не найден; форма вывода проверена отдельно");
        return;
    }
    let (dir, _) = generate("cp0502cc", COND_SRC);
    let out = Command::new("cc")
        .args([
            "-std=c11",
            "-Wall",
            "-Wextra",
            "-Wno-unused-parameter",
            "-Werror",
            "-c",
        ])
        .arg("-I")
        .arg(&dir)
        .arg(dir.join("cp0502cc.c"))
        .arg("-o")
        .arg(dir.join("cp0502cc.o"))
        .output()
        .expect("запуск cc");
    assert!(
        out.status.success(),
        "порождённый C обязан собираться (прежде — «too many arguments»):\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
}

/// У под-Модели работает то же правило, и указатель зовётся `main`.
#[test]
fn submodel_condition_call_follows_the_need() {
    let src = "out probe: u8 at 0;\nvar shared: u8 := 0;\n\
         model Worker {\n    var step: u8 := 0;\n\
         \x20   fn pure_twice(v: u8) -> u8 { return v + v; }\n\
         \x20   fn read_shared() -> u8 { return shared + 1; }\n\
         \x20   start Busy {\n\
         \x20       always { step := step + 1; shared := step; probe := step; }\n\
         \x20       ref Done: pure_twice(step) > 4;\n\
         \x20       ref Busy: read_shared() < 100;\n    }\n\
         \x20   state Done { always { probe := 99; } ref Done; }\n}\n\
         start Main = Worker;\n";
    let (_, text) = generate("cp0502s", src);
    assert!(
        text.contains("Worker_pure_twice(model->step)"),
        "чистая функция под-модели зовётся без указателя:\n{text}"
    );
    assert!(
        text.contains("Worker_read_shared(main)"),
        "функция, читающая общее объявление корня, получает `main`:\n{text}"
    );
}
