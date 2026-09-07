# Фича: Семантические диагностики и предупреждения

- **Номер:** 0008
- **Статус:** ГОТОВО
- **Зависит от:** 0001
- **Крейт:** `grammar`

## Краткое описание

Набор диагностик анализа: неиспользуемые переменные, недетерминированные
переходы, недостижимые состояния, константные условия, «висячие» точки с запятой,
неизвестные именованные блоки, неявный bool, документирующие комментарии.

## Итог (что сделано)

- Ce13 — неиспользуемые переменные: `semantic/unused.rs`, API
  `unused_variable_warnings()` (порты/константы не предупреждаются).
- Ce14 — недетерминированные переходы: `check_nondeterministic_transitions`
  (`semantic/validate.rs`), API `nondeterministic_transition_warnings()`.
- SE-044 `StraySemicolon`, SE-045 неизвестный именованный блок, SE-046
  недостижимые состояния, SE-047 константные условия переходов (`validate.rs`, `lib.rs`).
- SE-048 висячая привязка адреса (`address` для несуществующего порта),
  SE-049 конфликт источников адреса (inline + `address`, либо дубликат `address`)
  — оператор `address` фичи (`check_port_addresses` в `validate.rs`).
- SE-050 внешняя карта переопределяет адрес порта (наложение), SE-051 запись
  внешней карты для несуществующего порта — фича
  (`address_map_overlay_warnings` в `address_map.rs`).
- SE-052 используемый (достижимый кодогенерацией) порт без адреса — фича
  (`check_port_address_completeness` в `validate.rs`, опора на `unused.rs`).
- AM-001…006 — ошибки формата внешней `.ld`-подобной карты адресов
  (`parse_address_map` в `address_map.rs`): имя/`=`/адрес/`;`/литерал/дубликат.
- Неявный bool и документирующие комментарии (Ce12): `semantic/docs.rs`,
  `check_implicit_bool_conditions`.
- Фикстуры `unused_variable.lam`, `nondeterministic_warn.lam`, `implicit_bool_*.lam`,
  `doc_comments.lam`; контрпример `double_next.lam`.

> Ретроспективная карточка. Источники: `STATUS.md` (задачи 14, 16, 17, 18),
> память проекта (FE3, FE4), `CHANGES.md`.
