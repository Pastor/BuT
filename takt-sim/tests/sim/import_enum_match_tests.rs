//! Эталон различает варианты импортированного перечисления в `match`.

use takt_sim::{TickResult, Value};

/// Каталог фикстур: импорт ищет файл рядом с импортирующим.
const FIXTURE_DIR: &str = "tests/data/eval";
const APP: &str = "import_enum_app.takt";

/// Трасса `seen` за три такта.
fn trace() -> Vec<i128> {
    let path = std::path::Path::new(FIXTURE_DIR).join(APP);
    let source = std::fs::read_to_string(&path).expect("фикстура применения читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор применения");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[FIXTURE_DIR.to_string()])
        .expect("семантика применения");
    let mut unit = takt_sim::build_unit(model).expect("построение Unit");
    let mut out = Vec::new();
    for _ in 0..3 {
        let result = unit.tick();
        assert!(
            !matches!(result, TickResult::Failed(_)),
            "прогон не должен падать: {result:?}"
        );
        match unit.variable("seen") {
            Some(Value::Number(v)) => out.push(v),
            other => panic!("переменная 'seen' обязана быть числом, получено {other:?}"),
        }
    }
    out
}

/// **T5.** По варианту `Lda` берётся первая ветвь, по `Hlt` - вторая.
///
/// Первый такт: `v = Lda` -> `seen = 1`, и тело переводит `v` в `Hlt`. Дальше берётся
/// вторая ветвь -> `seen = 2`. Трасса `1, 2, 2` возможна **только** при верном
/// разрешении обоих вариантов: перепутанные номера дали бы `2, ...`, а неразрешённый
/// вариант - отказ в такте.
#[test]
fn reference_picks_arm_by_imported_variant() {
    assert_eq!(trace(), vec![1, 2, 2], "эталон выбрал не ту ветвь `match`");
}
