//! `textDocument/definition`.
//!
//! Проверяется не "функция вернула Some", а **согласие двух методов**: `definition` и
//! `declaration` обязаны отвечать одинаково на один и тот же курсор. В Takt объявление
//! и определение - одно и то же, и разъехаться они могут только по недосмотру: разными
//! обработчиками в сервере либо разными библиотечными входами.

#[cfg(feature = "lsp")]
mod definition_matches_declaration {
    use lsp_types::Position;
    use takt_lang::lsp::{goto_declaration_at, server_capabilities};

    /// Модель с использованиями всех видов, на которых работает переход: переменная в
    /// условии ребра, имя состояния, имя модели.
    const SRC: &str = r#"model Helper {
    var speed: u8 := 0;
    start Idle {
        ref Done: speed > 10;
    }
    state Done;
}

start Root = Helper;
"#;

    /// Позиция курсора на первом вхождении подстроки.
    fn cursor_on(source: &str, needle: &str) -> Position {
        let offset = source
            .find(needle)
            .unwrap_or_else(|| panic!("в исходнике нет `{needle}`"));
        let head = &source[..offset];
        let line = head.matches('\n').count() as u32;
        let col = head.rsplit('\n').next().map_or(0, |l| l.chars().count()) as u32;
        Position::new(line, col)
    }

    /// Обе возможности объявлены клиенту.
    ///
    /// Без `definition_provider` F12 в VS Code не работает вовсе - это и была исходная
    /// боль фичи.
    #[test]
    fn both_providers_are_advertised() {
        let caps = server_capabilities();
        assert!(
            caps.declaration_provider.is_some(),
            "declaration_provider потерян"
        );
        assert!(
            caps.definition_provider.is_some(),
            "definition_provider не объявлен — F12 в VS Code не заработает"
        );
    }

    /// У обоих методов **один** обработчик в сервере.
    ///
    /// Тест текстовый, потому что бинарник тестами не покрыть, а разъезд возможен
    /// ровно одним способом - второй веткой `match`. Тест валится, если кто-то заведёт
    /// `GotoDefinition::METHOD` отдельно от `GotoDeclaration`.
    #[test]
    fn server_handles_both_methods_in_one_branch() {
        let src = std::fs::read_to_string("src/bin/takt_lsp.rs")
            .expect("не прочитать src/bin/takt_lsp.rs");
        let joint = src
            .lines()
            .filter(|l| l.contains("GotoDeclaration::METHOD"))
            .collect::<Vec<_>>();
        assert_eq!(
            joint.len(),
            1,
            "ожидалась ровно одна ветка с GotoDeclaration::METHOD, найдено: {joint:?}"
        );
        assert!(
            joint[0].contains("GotoDefinition::METHOD"),
            "GotoDefinition обязан обслуживаться ТОЙ ЖЕ веткой, что GotoDeclaration; \
             найдено: {}",
            joint[0].trim()
        );
        assert_eq!(
            src.matches("GotoDefinition::METHOD").count(),
            1,
            "второй обработчик GotoDefinition — это и есть разъезд, который \
             сторож обязан ловить"
        );
    }

    /// Переход по каждому виду ссылки разрешается - то есть общий вход,
    /// который обслуживает оба метода, действительно работает.
    #[test]
    fn shared_entry_resolves_every_reference_kind() {
        for needle in ["speed > 10", "Done: speed", "Helper;"] {
            let position = cursor_on(SRC, needle);
            let found = goto_declaration_at("model.takt", SRC, position, &[]);
            assert!(
                found.is_some(),
                "переход с позиции `{needle}` не разрешился"
            );
        }
    }

    /// Курсор за пределами текста перехода не даёт (и не роняет сервер).
    ///
    /// Позиция "на ключевом слове" для этого не годится: `model` лежит внутри диапазона
    /// узла самой модели, и переход туда штатно разрешается - таково поведение с, эта
    /// задача его не меняет.
    #[test]
    fn cursor_beyond_text_yields_nothing() {
        let position = Position::new(999, 0);
        assert!(goto_declaration_at("model.takt", SRC, position, &[]).is_none());
    }
}
