//! Эталон отвечает на пустую реализацию так же, как цели -.

use takt_lang::parse;
use takt_lang::semantic::tree::construct_model;

/// Прямая ссылка на модель без состояний.
const DIRECT: &str = "model Empty { var z: u8 := 0; }\n\
                      start App = Empty;\n";

/// Последовательная композиция: рабочий шаг рядом с пустой моделью.
const SEQUENCE: &str = "model Empty { var z: u8 := 0; }\n\
                        model Work { var w: u8 := 0; start W { always { w := 1; } } }\n\
                        start App = Work + Empty;\n";

/// Контрпример: модель без состояний вне реализации - законный контейнер.
const CONTAINER: &str = "model Lib { var z: u8 := 0; }\n\
                         var q: u8 := 0;\n\
                         start S { always { q := 1; } }\n";

/// Код диагностики, которой отвечает построение дерева.
fn refusal(source: &str) -> Option<String> {
    let (ast, _) = parse(source, 0).expect("разбор пробы");
    construct_model(&ast, None, &[])
        .err()
        .and_then(|d| d.code.clone())
}

#[test]
fn empty_model_in_implementation_stops_the_run() {
    assert_eq!(
        refusal(DIRECT).as_deref(),
        Some("SE-106"),
        "эталон обязан отвечать тем же, чем цели: прежде он давал пустую трассу \
         `[—]` и рапортовал об успешном завершении"
    );
}

#[test]
fn sequence_with_empty_step_stops_the_run() {
    assert_eq!(
        refusal(SEQUENCE).as_deref(),
        Some("SE-106"),
        "в последовательной композиции пустой шаг съедал и рабочий: трасса `[—]` \
         вместо исполнения `Work`"
    );
}

#[test]
fn declaration_container_still_runs() {
    assert!(
        refusal(CONTAINER).is_none(),
        "модель без состояний ВНЕ реализации — контейнер объявлений, прогон \
         обязан идти как прежде"
    );
}
