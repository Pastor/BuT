# Фича: Перечисления (enum)

- **Номер:** 0005
- **Статус:** ГОТОВО
- **Зависит от:** 0001, 0002
- **Крейт:** `grammar`

## Краткое описание

Синтаксис и семантика перечислений `enum` с вариантами и значениями, включая
диагностику дубликатов вариантов.

## Итог (что сделано)

- AST: `EnumDefine`, `EnumVariant` (`parser/ast.rs`); `Token::Enum` (`lexer.rs`);
  правила `EnumDefine`/`EnumVariant` (`grammar.lalrpop`).
- Семантика: `EnumNode` (`semantic/enum_node.rs`, `semantic/tree.rs`).
- Фикстуры: `enum_basic.lam`, `enum_with_values.lam`; контрпример
  `enum_duplicate_variant.lam`.

> Ретроспективная карточка. Источники: память проекта (FE1),
> `CHANGES.md`.
