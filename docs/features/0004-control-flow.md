# Фича: Управляющие конструкции и match/switch

- **Номер:** 0004
- **Статус:** ГОТОВО
- **Зависит от:** 0001
- **Крейт:** `grammar`

## Краткое описание

Конструкции `if`/`while`/`for`/`loop` в блоках кода и полноценный оператор
`match`/`switch` (лексер → грамматика → AST → семантика → C-генератор).

## Итог (что сделано)

- `while` как синоним `loop` (`lexer.rs`, `grammar.lalrpop`).
- `match`/`switch`: `lexer.rs`, `grammar.lalrpop`, `parser/ast.rs`
  (`Statement`, `MatchPattern`), `semantic/statement.rs`, `generator/c/c_expr.rs`.
- Фикстура `if_while_for.lam`.

> Ретроспективная карточка. Источники: `STATUS.md` (задачи 8, 10),
> коммиты `a49fcfd`, `b703e74`, `CHANGES.md`.
