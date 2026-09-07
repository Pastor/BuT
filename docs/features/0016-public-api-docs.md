# Фича: Документирование публичного API

- **Номер:** 0016
- **Статус:** ГОТОВО
- **Зависит от:** 0001
- **Крейты:** `grammar`, `simulation`

## Краткое описание

Doc-комментарии (`///`) для всего публичного API, crate-level doc-comment'ы и
компилируемые doctests; сопутствующая чистка предупреждений clippy.

## Итог (что сделано)

- `///`-документация: `verification::ltl::Ltl`, `verification::buchi::BuchiAutomaton`,
  `semantic::formula::Formula`, `semantic::minimap::{Name, StateExtend, Element, Map}`.
- Crate-level doc в `simulation/src/{lib,bin/simulation}.rs`; type alias `PortMap`,
  `NodeMap` для упрощения сложных типов.
- Устранены предупреждения `private_interfaces`, `needless_return`,
  `collapsible_if` и др.

> Ретроспективная карточка. Источники: `CHANGES.md` (раздел
> «[Не выпущено]»).
