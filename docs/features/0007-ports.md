# Фича: Порты и индексный доступ

- **Номер:** 0007
- **Статус:** ГОТОВО
- **Зависит от:** 0001, 0002
- **Крейт:** `grammar`

## Краткое описание

Порты (`bit`/`rational`/`numeric`) с направлениями `in`/`out`/`inout` и доступ по
индексу-выражению (`arr[i]`, `port[i]`) в условиях и выражениях.

## Итог (что сделано)

- Направление `inout` (двунаправленный порт): `parser/ast.rs` (`PortDirection`),
  `lexer.rs`, `grammar.lalrpop`, `generator/c/{mod,c_header}.rs`.
- Индекс массива/порта — произвольное выражение: `ast.rs`, `grammar.lalrpop`,
  `semantic/{mod,condition,expression,validate}.rs`, `generator/c/c_expr.rs`.
- Интерфейс портов в C: `read_bit`/`write_bit`/`read_float`/`write_float`/
  `read_numeric`/`write_numeric`.
- Фикстуры: `array_access.lam`; контрпримеры `array_out_of_bounds.lam`,
  `non_array_subscript.lam`.

> Ретроспективная карточка (правило 17). Источники: `STATUS.md` (задачи 3, 12),
> `CHANGES.md`.
