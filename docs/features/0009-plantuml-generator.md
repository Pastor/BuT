# Фича: Генератор диаграмм PlantUML

- **Номер:** 0009
- **Статус:** ГОТОВО
- **Зависит от:** 0001
- **Крейт:** `grammar`

## Краткое описание

Второй целевой генератор — диаграммы состояний PlantUML (`--target plantuml`).

## Итог (что сделано)

- `generator/plantuml/puml_map.rs` — снимок семантической карты модели.
- `generator/plantuml/mod.rs` — генерация `.puml` из `PumlMap`.
- `Language::PlantUML` (`generator/mod.rs`), API `compile_to_plantuml()` (`lib.rs`),
  поддержка `--target plantuml` в CLI (`bin/lamc.rs`).

> Ретроспективная карточка (правило 17). Источники: `CHANGES.md`,
> коммит `bdcbb1a`.
