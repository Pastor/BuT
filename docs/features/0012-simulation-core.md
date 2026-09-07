# Фича: Крейт simulation — симуляция моделей

- **Номер:** 0012
- **Статус:** ГОТОВО
- **Зависит от:** 0001
- **Крейт:** `simulation`

## Краткое описание

Крейт `simulation`: пошаговая симуляция семантических моделей с иерархическим
контекстом (родительские цепочки, ленивое копирование), CLI-бинарник.

## Итог (что сделано)

- Построение дерева `Unit` из `ModelNode`: `simulation/src/unit/builder.rs`,
  `unit/mod.rs`, `unit/viewport.rs`; `ModelNodeContext` (`context.rs`) —
  `Rc<RefCell<ModelNode>>`, ленивая копия, цепочка родителей.
- Предикаты переходов `Predicate` (`predicate.rs`) — именованный struct,
  `Rc<dyn Fn>`, читаемые метки (`condition_label`).
- Раннер (`runner.rs`), CLI (`bin/simulation.rs`), лимит шагов из сценария.

> Ретроспективная карточка. Источники: `STATUS.md` (TASKS §1),
> коммиты `2c4527e` (П1–П8), `483aaf0`, `CHANGES.md`.
