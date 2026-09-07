# Фича: LSP-сервер lam-lsp

- **Номер:** 0011
- **Статус:** ГОТОВО
- **Зависит от:** 0001
- **Крейт:** `grammar` (feature `lsp`)

## Краткое описание

Языковой сервер `lam-lsp`: поиск узла по позиции, hover-подсказки, подсветка
встроенных типов и ключевых слов.

## Итог (что сделано)

- `semantic/index.rs` — `SemanticIndex`, `SemanticNodeRef`, `SemanticNodeKind`.
- `lsp.rs` — `position_to_offset`, `node_at_position`, `hover_info` (2-этапный
  поиск: по позиции + резервный по имени); `Display` для `TypeNode` (без `{:?}`).
- Подсветка встроенных типов, `while`/`match`/`inout`. Бинарник `bin/lam_lsp.rs`.
- Тесты: `grammar/tests/lsp_tests.rs`, фикстуры `grammar/tests/data/lsp/`.

> Ретроспективная карточка. Источники: память проекта (LSP1),
> коммиты `c7c7027`, `908ccc5`, `CHANGES.md`.
