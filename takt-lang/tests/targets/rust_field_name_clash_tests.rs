//! Столкновение имени объявления с полем, которое печатает цель `rust`.
//!
//! # Что тестытся
//!
//! Отказ на **каждый** вид (тест падает списком), контроли "то же самое без
//! столкновения", и - главное - [`every_service_field_is_claimed`]: перечень служебных
//! полей берётся из **напечатанной** структуры, а не из списка в тесте. Новое служебное
//! поле, забытое в наборе занятых, роняет тест.

use std::process::Command;
use takt_lang::generator::GenerateOptions;

/// Входы, где имя объявления сталкивается с полем структуры.
///
/// Каждая строка - **отдельный источник** поля: состояние автомата, механизм времени,
/// аккумулятор `every`, шаг цепочки, экземпляр под-модели, разделяемые переменные и
/// аппаратный слой приходят в структуру разными путями, и набор занятых имён обязан
/// видеть их все.
const CLASHES: &[(&str, &str, &str)] = &[
    (
        "служебное 'state' цели",
        "RS-026",
        "var State: u8 := 0;\n\
         out probe: u8;\n\
         start Run { always { State := State + 1; probe := State; } ref Run: State < 200; }\n",
    ),
    (
        "счётчик тактов 'takt_dwell'",
        "RS-026",
        "var takt_dwell: u8 := 0;\n\
         out probe: u8;\n\
         start Run { always { takt_dwell := takt_dwell + 1; probe := takt_dwell; } ref Hold: after 5t; }\n\
         state Hold { always { probe := 0; } }\n",
    ),
    (
        "метка времени 'takt_entry_ms'",
        "RS-026",
        "var takt_entry_ms: u8 := 0;\n\
         out probe: u8;\n\
         start Run { always { takt_entry_ms := takt_entry_ms + 1; probe := takt_entry_ms; } ref Hold: after 1s; }\n\
         state Hold { always { probe := 0; } }\n",
    ),
    (
        "прошлое состояние 'takt_prev_state'",
        "RS-026",
        "var takt_prev_state: u8 := 0;\n\
         out probe: u8;\n\
         start Run { always { takt_prev_state := takt_prev_state + 1; probe := takt_prev_state; } ref Hold: after 1s; }\n\
         state Hold { always { probe := 0; } }\n",
    ),
    (
        "аккумулятор 'takt_every0'",
        "RS-026",
        "var takt_every0: u8 := 0;\n\
         out probe: u8;\n\
         start Run { every 500ms { takt_every0 := takt_every0 + 1; } always { probe := takt_every0; } ref Run: takt_every0 < 200; }\n",
    ),
    (
        "шаг цепочки '<состояние>_seq'",
        "RS-026",
        "model StepA { out a_out: u8; start Go { always { a_out := 1; } next End; } state End; }\n\
         model StepB { out b_out: u8; start Go { always { b_out := 2; } next End; } state End; }\n\
         var work_seq: u8 := 0;\n\
         out probe: u8;\n\
         start Work = StepA + StepB;\n\
         state Done { always { probe := work_seq; } }\n",
    ),
    (
        "экземпляр под-модели (имя состояния)",
        "RS-026",
        "model Worker { out w_out: u8; start Go { always { w_out := 1; } next End; } state End; }\n\
         var run: u8 := 0;\n\
         out probe: u8;\n\
         start Run = Worker;\n\
         state Done { always { probe := run; } }\n",
    ),
    (
        "разделяемые переменные 'shared'",
        "RS-026",
        "model Reader { start Go { always { carry := carry + 1; } next End; } state End; }\n\
         var carry: u8 := 0;\n\
         var shared: u8 := 0;\n\
         out probe: u8;\n\
         start Run { always { shared := shared + 1; probe := shared; } ref Work: shared > 3; }\n\
         state Work = Reader;\n",
    ),
    (
        "аппаратный слой 'hal'",
        "RS-026",
        "var hal: u8 := 0;\n\
         out probe: u8;\n\
         start Run { always { hal := hal + 1; probe := hal; } ref Run: hal < 200; }\n",
    ),
    // Обе стороны написал автор - предмет отказа другой (слипание после приведения
    // регистра), и код у него свой.
    (
        "две переменные с одним snake_case",
        "RS-005",
        "var fooBar: u8 := 0;\n\
         var foo_bar: u8 := 0;\n\
         out probe: u8;\n\
         start Run { always { fooBar := fooBar + 1; foo_bar := foo_bar + 2; probe := fooBar + foo_bar; } ref Run: fooBar < 200; }\n",
    ),
];

/// Те же модели без столкновения: цель обязана переводить их по-прежнему.
///
/// Контроли обязательны: без них "столкновения отвергаются" означало бы лишь, что цель
/// отвергает всё похожее - например любое имя, начинающееся с `takt_`, или любую
/// переменную рядом с `every`.
const CONTROLS: &[(&str, &str)] = &[
    (
        "переменная рядом со служебным состоянием",
        "var level: u8 := 0;\n\
         out probe: u8;\n\
         start Run { always { level := level + 1; probe := level; } ref Run: level < 200; }\n",
    ),
    (
        "имя с приставкой 'takt_', но не служебное",
        "var takt_level: u8 := 0;\n\
         out probe: u8;\n\
         start Run { always { takt_level := takt_level + 1; probe := takt_level; } ref Hold: after 1s; }\n\
         state Hold { always { probe := 0; } }\n",
    ),
    (
        "переменная рядом с аккумулятором 'every'",
        "var beats: u8 := 0;\n\
         out probe: u8;\n\
         start Run { every 500ms { beats := beats + 1; } always { probe := beats; } ref Run: beats < 200; }\n",
    ),
    (
        "переменная рядом с экземпляром под-модели",
        "model Worker { out w_out: u8; start Go { always { w_out := 1; } next End; } state End; }\n\
         var counter: u8 := 0;\n\
         out probe: u8;\n\
         start Run = Worker;\n\
         state Done { always { probe := counter; } }\n",
    ),
];

/// Модель со **всеми** видами служебных полей в одной структуре.
///
/// `{EXTRA}` - объявление, которым тест занимает имя поля, `{USE}` - его
/// употребление. Употребление обязательно: неиспользуемую переменную цель не печатает
/// вовсе (фильтр `unused`), и столкновению неоткуда взяться - тест молча проходил бы
/// на любом имени.
const SAMPLE: &str = "\
model Reader { start Go { always { carry := carry + 1; } next End; } state End; }\n\
model StepA { out a_out: u8; start Go { always { a_out := 1; } next End; } state End; }\n\
model StepB { out b_out: u8; start Go { always { b_out := 2; } next End; } state End; }\n\
var carry: u8 := 0;\n\
var level: u8 := 0;\n\
out probe: u8;\n\
{EXTRA}\
start Run {\n\
    every 500ms { level := level + 1; }\n\
    always { {USE}probe := level; }\n\
    ref Work: after 1s;\n\
    ref Hold: after 5t;\n\
}\n\
state Work = StepA + StepB;\n\
state Hold = Reader;\n";

/// Поля образца, которые объявил автор: занимать их тесту нечем.
const AUTHOR_FIELDS: &[&str] = &["level"];

fn out_dir(tag: &str) -> std::path::PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0483_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    dir
}

fn compile_rust(
    tag: &str,
    source: &str,
) -> Result<std::path::PathBuf, takt_lang::diagnostics::Diagnostic> {
    let dir = out_dir(tag);
    takt_lang::compile_to_rust(
        "probe",
        source,
        dir.to_str().expect("путь"),
        &[],
        &GenerateOptions::default(),
    )
    .map(|_| dir)
}

fn tag_of(kind: &str) -> String {
    kind.chars()
        .map(|c| if c.is_ascii_alphanumeric() { c } else { '_' })
        .collect()
}

/// Каждый вид столкновения отвергается своим кодом; тест падает **списком**.
#[test]
fn every_kind_of_clash_is_refused() {
    let mut missed = Vec::new();
    for (kind, code, source) in CLASHES {
        match compile_rust(&tag_of(kind), source) {
            Ok(dir) => {
                let _ = std::fs::remove_dir_all(&dir);
                missed.push(format!(
                    "{kind}: цель приняла вход — вывод невалиден для rustc (E0124)"
                ));
            }
            Err(err) if err.code.as_deref() != Some(code) => {
                missed.push(format!("{kind}: код {:?}, ожидался {code}", err.code));
            }
            Err(_) => {}
        }
    }
    assert!(missed.is_empty(), "не отвергнуто:\n{}", missed.join("\n"));
}

/// Отказ называет обе стороны, объясняет чей это предмет и несёт координату.
///
/// Без имени поля автор видит одно объявление и не знает, с чем оно столкнулось:
/// служебного поля в его исходнике нет вовсе.
#[test]
fn refusal_names_both_sides_and_scope() {
    let err = compile_rust("text", CLASHES[0].2).expect_err("ожидался отказ цели");
    assert!(
        err.message.contains("'State'") && err.message.contains("'state'"),
        "отказ обязан назвать обе стороны: {}",
        err.message
    );
    assert!(
        err.message.contains("остаётся валидной"),
        "отказ принадлежит цели — текст обязан это сказать: {}",
        err.message
    );
    assert!(
        !matches!(err.loc, takt_lang::diagnostics::Location::Codegen),
        "координата обязана указывать на объявление, а не на генерацию"
    );
}

/// **Тест набора:** каждое служебное поле напечатанной структуры занято.
///
/// Перечень берётся из **вывода**, а не из списка в тесте: служебное поле, добавленное
/// в печать и забытое в наборе занятых, обязано ронять этот тест - иначе класс вернётся
/// ровно тем же путём, каким пришёл.
#[test]
fn every_service_field_is_claimed() {
    let dir = compile_rust("sample", &sample("", "")).expect("образец переводится");
    let text = std::fs::read_to_string(dir.join("probe.rs")).expect("вывод читается");
    let _ = std::fs::remove_dir_all(&dir);

    let fields = struct_fields(&text, "pub struct Probe<");
    assert!(
        fields.len() > 8,
        "образец обязан показывать все виды полей, а показал {fields:?}"
    );

    let mut missed = Vec::new();
    for field in &fields {
        if AUTHOR_FIELDS.contains(&field.as_str()) {
            continue;
        }
        // Имя поля бывает ключевым словом языка (`state`): написать такое объявление
        // нельзя вовсе, и занять поле автор может лишь другим регистром - цель приводит
        // его к тому же snake_case.
        let written = writable_name(field);
        let source = sample(
            &format!("var {written}: u8 := 0;\n"),
            &format!("{written} := {written} + 1; "),
        );
        match compile_rust(&tag_of(field), &source) {
            Ok(dir) => {
                let _ = std::fs::remove_dir_all(&dir);
                missed.push(format!("{field}: поле печатается, но именем не занято"));
            }
            Err(err) if err.code.as_deref() != Some("RS-026") => {
                missed.push(format!(
                    "{field} (объявлено как '{written}'): код {:?}, ожидался RS-026",
                    err.code
                ));
            }
            Err(_) => {}
        }
    }
    assert!(
        missed.is_empty(),
        "поля структуры, не попавшие в набор занятых:\n{}",
        missed.join("\n")
    );
}

/// Подставляет в образец объявление и его употребление.
fn sample(extra: &str, use_stmt: &str) -> String {
    SAMPLE.replace("{EXTRA}", extra).replace("{USE}", use_stmt)
}

/// Имя, которым автор может занять поле `field`.
///
/// Ключевое слово языка объявлением быть не может (`var state` - `SY-002`), а первая
/// заглавная даёт тот же snake_case: `State` -> `state`.
fn writable_name(field: &str) -> String {
    if takt_lang::parser::lexer::is_keyword(field) {
        let mut chars = field.chars();
        return match chars.next() {
            Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
            None => field.to_string(),
        };
    }
    field.to_string()
}

/// Имена полей структуры, объявление которой начинается с `head`.
fn struct_fields(text: &str, head: &str) -> Vec<String> {
    let start = text.find(head).expect("структура образца напечатана");
    let body = &text[start..];
    let end = body.find("\n}").expect("структура закрыта");
    body[..end]
        .lines()
        .skip(1)
        .map(str::trim)
        .filter(|line| !line.is_empty() && !line.starts_with("//"))
        .filter_map(|line| line.split(':').next())
        .map(str::to_string)
        .collect()
}

/// **Контроли:** цель переводит их по-прежнему, и `rustc` вывод принимает.
///
/// Прогон инструмента здесь не украшение: отказ цели снят, но остаётся вопрос, валиден
/// ли вывод - на него отвечает только `rustc`.
#[test]
fn controls_are_still_translated_and_accepted() {
    let rustc = Command::new("rustc").arg("--version").output().is_ok();
    let mut failed = Vec::new();
    for (kind, source) in CONTROLS {
        let dir = match compile_rust(&tag_of(kind), source) {
            Ok(dir) => dir,
            Err(err) => {
                failed.push(format!("{kind}: цель отказала — {:?}", err.code));
                continue;
            }
        };
        if rustc {
            let out = Command::new("rustc")
                .args(["--edition", "2021", "--crate-type=lib", "-D", "warnings"])
                .arg("--emit=metadata")
                .arg("-o")
                .arg(dir.join("probe.rmeta"))
                .arg(dir.join("probe.rs"))
                .output()
                .expect("rustc запускается");
            if !out.status.success() {
                failed.push(format!(
                    "{kind}: rustc отверг вывод:\n{}",
                    String::from_utf8_lossy(&out.stderr)
                ));
            }
        }
        let _ = std::fs::remove_dir_all(&dir);
    }
    assert!(failed.is_empty(), "контроли:\n{}", failed.join("\n"));
}
