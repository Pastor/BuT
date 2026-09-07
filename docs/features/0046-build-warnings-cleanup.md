# Фича: Устранение всех предупреждений сборки (rustc + clippy)

- **Номер:** 0046 (перенумерована с 0026 — коллизия с
  [генератор C: typedef корневой структуры для одиночной модели](0026-c-root-typedef.md) «Генератор C: typedef корневой структуры»,
  заведённой параллельно; решение заказчика 2026-07-15, свод: номер
  сквозной и уникальный)
- **Статус:** ГОТОВО (закрыта 2026-07-19)
- **Зависит от:** **0036** — «Согласование видимости публичного API крейта
  `simulation`» (проставлено координатором при разведении объёмов, 2026-07-15;
  аналитик подтверждает или снимает на стадии анализа). 19 из ~125 предупреждений
  (`private_interfaces`) — объём 0036, и она уже в `РАЗРАБОТКА` с принятым ADR.
  Пока 0036 не закрыта, объём 0046 нельзя зафиксировать: остаток пересчитывается
  по факту. Пересечение с [разделение переросших модулей (validate.rs, lsp.rs, c_expr.rs)](0027-module-size-split.md) («`validate.rs` — 3648
  строк») косвенное — та перекладывает `c_expr.rs` и `lsp.rs`, где живут
  крупнейшие группы clippy-долга (`needless_ref` 23, `collapsible_if` 18);
  порядок 0027 → 0046 дешевле обратного.
- **Связанные issue (анализ):** новая фича (из бэклога кандидатов `FEATURES.md`;
  выявлено при обзоре 2026-07-15)
- **Крейт:** `grammar` и `simulation` (плюс тестовые цели обоих)

## Стадии жизненного цикла

Все стадии — **разделы этой карточки**: «Архитектура (ADR)»,
«Анализ», «Разработка», «Тест-план», «Отчёт о тестировании», «Итог».
Отдельным артефактом остаются только исправления —
[`docs/fixes/`](../fixes/README.md) (при необходимости `0046-YY-*`).

## Краткое описание

Привести сборку обоих крейтов к нулю предупреждений. `cargo build --all-features
--all-targets` выдаёт **19** предупреждений rustc, `cargo clippy --all-features
--all-targets` — **~106** (суммарно ~125). Это фоновый шум, скрывающий новые,
осмысленные предупреждения, и накопленный технический долг стиля/видимости. Цель —
вычистить его и закрепить результат в CI (`-D warnings`), чтобы долг не
накапливался снова.

> Фича зарегистрирована из бэклога кандидатов `FEATURES.md`; далее проходит
> жизненный цикл по своду: архитектура (ADR) → анализ → разработка.

## Мотивация / контекст

Предупреждения органичны и большинство давно висят, но их масса (~125) означает,
что **новое** предупреждение — например, признак свежего дефекта — тонет в шуме и
не замечается. CI сегодня (`cargo build`/`check`/`test`) предупреждения **не**
проваливает, поэтому долг растёт бесконтрольно. `precheck.sh` гоняет clippy, но
без `-D warnings`.

## Инвентарь предупреждений (снимок 2026-07-15, до починки)

### rustc — 19

| Линт | Кол-во | Места |
|---|---|---|
| `private_interfaces` | 11 | `simulation/src/unit/mod.rs` (`Context`/`Value`/`Predicate`/`Flow` — `pub(crate)` при публичных полях `Unit`) |
| `unused_imports` | 2 | `simulation/src/unit/builder.rs:9`; `grammar/tests/lsp_tests.rs:12-13` |
| `missing_docs` | 1 | `grammar/src/lsp.rs:157` |
| `dead_code` | 1 | `grammar/src/format/comments.rs:27` |

### clippy — ~106 (крупнейшие группы)

| Линт | Кол-во | Основные места |
|---|---|---|
| `needless_ref` | 23 | `grammar/src/generator/c/c_expr.rs` |
| `collapsible_if` | 18 | `grammar/src/lsp.rs` |
| `clone_on_copy` | 16 | `grammar/tests/lexer_tests.rs` (`Location` — `Copy`) |
| `private_interfaces` | 8 | `simulation/src/unit/mod.rs` |
| `needless_borrow` | 6 | `grammar/src/lsp.rs` |
| `map_or_else` | 5 | `lexer_tests.rs`, `unit/mod.rs`, `unit/viewport.rs` |
| `assertions_on_constants` | 3 | `grammar/tests/parser_tests.rs` |
| прочее (единичное) | ~27 | `derivable_impl`, `redundant_guard` (`eval/ops.rs`), `map_values`, `missing_docs_in_private_items` и др. |

## Объём (предварительно, уточняет аналитик)

> **Разграничение с фичей [согласование видимости публичного API крейта simulation](0036-sim-visibility.md) (решение заказчика
> 2026-07-15).** `private_interfaces` **исключён** из объёма 0046 — им занимается
> 0036, которая уже в статусе `РАЗРАБОТКА` с принятым ADR. Первоначальная
> формулировка карточки («поглощён карточкой 0026») отменена: поглощать оказалось
> нечего — узкий пункт бэклога к тому моменту уже был проработан до решения,
> причём нетривиального. Прямой путь «сузить поля `Unit` до `pub(crate)`»
> **невозможен** (`E0449`: варианты `pub enum` всегда разделяют его видимость) —
> 0036 принял инкапсуляцию `pub struct Unit(UnitKind)`, что **ломает публичный
> API крейта** и требует подъёма его версии. В общей чистке это решение было бы
> принято мимоходом и без обоснования. Поэтому: **0046 зависит от 0036**.

1. **rustc-долг** — 19 предупреждений, из которых **11 (`private_interfaces`) —
   объём фичи**, а не 0046. Остаётся **8**: `unused_imports` (2),
   `missing_docs` (1), `dead_code` (1) и прочее. Учтите, что 0036 задачей уже забрала `unused_imports` в `simulation/src/unit/builder.rs:9` —
   аналитику 0046 пересчитать остаток по факту закрытия 0036.
2. **clippy-долг** — ~106 предупреждений, в основном механические автоправки
   (`needless_ref`/`collapsible_if`/`clone_on_copy`); часть в тестовых целях.
   Здесь же **8** clippy-`private_interfaces` — тоже за 0036.
3. **CI-закрепление** — рассмотреть `-D warnings` (или `RUSTFLAGS`/`clippy
   --deny warnings`) в `.github/workflows/ci.yml` и/или `precheck.sh`, чтобы
   ноль-долг не разъехался снова. **Учесть при анализе:** глобальный
   `-D warnings` на сборку workspace завалит CI, пока жив долг **любого** крейта
   (находка 0036) — поэтому 0036 закрепляет свой результат точечным
   `#![deny(private_interfaces)]`. Прецедент точечного линта —
   `#![deny(clippy::wildcard_enum_match_arm)]` в `simulation/src/eval/mod.rs`.
   `#![deny(warnings)]` вводить нельзя — прямой запрет `docs/CODE.md`.

## Осторожно (не сломать)

- В коде **~38 директив `#[allow(...)]` в 20 файлах** — это осознанные
  компромиссы (`large_enum_variant`, `too_many_arguments`, `dead_code`,
  `type_complexity`). **Не снимать** без разбора: часть удерживает инварианты.
- `deny(clippy::wildcard_enum_match_arm)` в `simulation/src/eval/` —
  **инвариант из [ADR](0025-simulator-expression-eval.md#архитектура-adr)**, не
  трогать (исключение — `coerce_to_type` для `#[non_exhaustive] TypeNode`).
- Правки в `grammar/src/generator/c/` могут менять **вывод C**; правки, задевающие
  фикстуры `tests/data/`-снапшотов, — сверять codegen побайтно (`git diff`
  порождённого кода).

## Обратная совместимость

Язык и его семантика **не меняются** — чистка внутренняя. Версия языка не растёт. Видимая часть — возможный бамп версий крейтов при расширении
публичного API (`private_interfaces` → `pub`), решает аналитик.

## Архитектура (ADR)

- **Status:** Accepted (2026-07-19)
- **Date:** 2026-07-15 (решение — 2026-07-19)
- **Authors:** Архитектор
- **Related issues:** [Фича](0046-build-warnings-cleanup.md)

### Context

Сборка обоих крейтов даёт ~125 предупреждений (19 rustc + ~106 clippy). Основной
вопрос стадии 2 — не «как переписать каждую строку» (правки в массе механические),
а **политика**: чистить ли всё разом или по группам, и главное — как **закрепить**
ноль-долг, не поломав осознанные `#[allow(...)]` и инвариант
`deny(clippy::wildcard_enum_match_arm)` из [ADR](0025-simulator-expression-eval.md#архитектура-adr).

### Decision Drivers

1. **Шум скрывает сигнал.** Новое предупреждение (признак дефекта) тонет в массе.
2. **Долг не должен возвращаться.** Нужна автоматическая преграда (CI), иначе
   чистка — разовая и бессмысленная.
3. **Не сломать осознанные компромиссы.** ~38 `#[allow]` и `deny`-инвариант.

### Considered Options

#### Option A. Разовая чистка без CI-проверки

**Pros:**
- Быстро, один PR.

**Cons:**
- Долг вернётся — история проекта это подтверждает.

#### Option B. Чистка + `-D warnings` в CI/precheck

**Pros:**
- Ноль-долг закреплён компилятором, а не дисциплиной.

**Cons:**
- Требует аккуратной ревизии `#[allow]`; риск задеть вывод C-генератора.

#### Option C. Только CI-проверка на новые предупреждения (baseline старых)

**Pros:**
- Не требует массовых правок сразу.

**Cons:**
- Инструментов baseline у cargo/clippy штатно нет; сложно поддерживать.

### Decision

**Option B — разовая чистка + `-D warnings` в CI/`precheck.sh`.**

Долг сведён к нулю и закреплён CLI-уровневым `-D warnings` на шаге clippy (clippy
гоняет и clippy-, и rustc-линты — один шаг покрывает оба набора). Это **не**
запрещённый `#![deny(warnings)]` в коде (docs/CODE.md): флаг живёт в скрипте
проверки, а не в крейте, поэтому обновление компилятора не ломает сборку у
пользователя — оно ломает лишь `precheck`/CI, что и требуется, чтобы предупреждение
устранили, а не накопили. Точечные исключения — `#[allow(...)]` у места (как ~38
существующих осознанных подавлений). Option C (baseline старых) отвергнут: штатных
инструментов baseline у cargo/clippy нет. Option A (без проверки) — история проекта
показала, что долг вернётся.

#### Решение по `result_large_err` (вскрыто разработкой 2026-07-19)

Свежий инвентарь дал **549** clippy-предупреждений (снимок 2026-07-15 — ~106):
**414** из них — один класс `clippy::result_large_err` (стал `#[warn]` по
умолчанию в clippy 0.1.99, в снимке его не было). Причина — `Diagnostic` = **136
байт** > порога 128 на каждом `Result<_, Diagnostic>` (их 203+). Развилка (решение
заказчика 2026-07-19): **ужать `Location::Source(u64, usize, usize)` до
`(u32, u32, u32)`** — вариант 16 байт вместо 32, `Diagnostic` → 120 байт, все 414
исчезают **без** `#[allow]` и **без** правки сигнатур `Result` (отвергнуты
`Box<Diagnostic>` — 203+ сигнатур + публичный API — и крейт-level `#[allow]` —
маскировка). Публичный API методов `Location` остался в `usize`/`String`: каст
локализован в аксессорах (`start`/`end`/`range`/…) и в хелпере-конструкторе
`Location::source(u64, usize, usize)`, которым заменены 160 конструирований в
`grammar.lalrpop` и лексере. Номера файлов и байтовые смещения `.lam` с запасом
влезают в 32 бита.

### Consequences

#### Положительные

- Новое предупреждение (признак дефекта) больше не тонет в шуме — CI красит на
  первом же.
- `Diagnostic` вдвое меньше по варианту `Err` (120 vs 136 байт) — стековые кадры
  фаллибельных функций легче; попутная выгода честной правки (в отличие от
  `#[allow]`).

#### Отрицательные / Action items

- `-D warnings` на CI/precheck делает сборку чувствительной к обновлению clippy:
  новый линт по умолчанию → красный CI. Это **осознанная цена** (иначе долг
  вернётся); реакция — устранить или локально `#[allow]` с обоснованием.
- Мёртвый **коммитнутый** `grammar/src/grammar.rs` (~29k строк, не компилируется —
  сборка идёт из `OUT_DIR`) остаётся в репозитории. Вне объёма 0046 (не даёт
  предупреждений — не компилируется); кандидат на удаление.

#### Acceptance criteria

1. `cargo build --all-features --all-targets` — **0** предупреждений.
2. `cargo clippy --all-features --all-targets` — **0** предупреждений.
3. Осознанные `#[allow(...)]` и `deny(clippy::wildcard_enum_match_arm)`
   сохранены; вывод C-генератора не изменился (сверка снапшотов).
4. Ноль-долг закреплён в CI/`precheck.sh` (способ — по решению ADR).

## Анализ

### Цель и контекст

Свести предупреждения сборки обоих крейтов к нулю и закрепить результат, не
сломав осознанные подавления и инвариант `deny(clippy::wildcard_enum_match_arm)`
([ADR](0025-simulator-expression-eval.md#архитектура-adr)). Инвентарь предупреждений —
в [карточке фичи](0046-build-warnings-cleanup.md).

### Зависимости фичи

> **Обязательный раздел аналитика.** Заполнить на стадии анализа: проверить
> зависимости и проставить «Зависит от» в карточке и в `FEATURES.md`.

- **Зависит от:** **0036** (закрыта 2026-07-19) — она забрала 19
  `private_interfaces` из объёма 0046. Прочих зависимостей нет: 0027 (деление
  модулей) уже закрыта, конфликта по файлам не осталось.
- **Влияние на порядок разработки:** 0046 берётся сразу после 0036.

### Инвентарь пересчитан по факту (2026-07-19)

Снимок карточки (2026-07-15, ~125) **устарел**: свежий прогон дал **19 rustc → 3**
(0036 убрала 11 `private_interfaces`, ещё несколько — попутные фичи) и **~106
clippy → 549**. Взрыв clippy — **один класс**: `clippy::result_large_err` (**414**),
включённый по умолчанию в clippy 0.1.99 (в снимке его не было). Причина —
`Diagnostic` = 136 байт > порога 128 на каждом из 203+ `Result<_, Diagnostic>`.
Разрешение (решение заказчика) — ужать `Location::Source` до `u32×3`
(`Diagnostic` → 120 байт); детали и отвергнутые альтернативы — в
[ADR](0046-build-warnings-cleanup.md#архитектура-adr), раздел «Решение по `result_large_err`».

### Декомпозиция

- **0046-01** — ужатие `Location::Source(u64,usize,usize) → (u32,u32,u32)`:
  хелпер-конструктор `Location::source`, касты в аксессорах, ~200 сайтов
  (конструирование через хелпер/sed, разбор — `as usize`). Закрывает 414.
- **0046-02** — механическая чистка остатка: `clippy --fix` (needless_ref,
  collapsible_if, clone_on_copy, …) + ручные (`too_many_arguments` → `#[allow]`,
  `doc_lazy_continuation`, `redundant_guards`, `field_reassign_with_default`,
  `assertions_on_constants`, `needless_range_loop`) + 3 rustc (`unused_imports`,
  `missing_docs`, `dead_code`).
- **0046-03** — закрепление: `-D warnings` на clippy в `precheck.sh` и CI.

### Требования и проверяемые условия

- **R1. Ноль rustc-предупреждений.** `cargo build --all-features --all-targets`.
- **R2. Ноль clippy-предупреждений.** `cargo clippy --all-features --all-targets`.
- **R3. Инварианты целы.** `#[allow(...)]` и `deny(wildcard_enum_match_arm)` не
  сняты без обоснования; вывод C-генератора байт-в-байт неизменен.
- **R4. Долг закреплён.** Преграда в CI/`precheck.sh` (способ — за ADR).

### Критерии приёмки и способ проверки

| # | Критерий | Способ проверки |
|---|---|---|
| A1 | 0 rustc-warnings | `cargo build --all-features --all-targets 2>&1 \| grep -c warning` = 0 |
| A2 | 0 clippy-warnings | `cargo clippy --all-features --all-targets 2>&1 \| grep -c warning` = 0 |
| A3 | Codegen не изменился | сверка снапшотов `examples/generated/`, тесты conformance |
| A4 | Инвариант цел | негативная проба: `_ =>` в `eval/` валит сборку |

### Особенности по обратной функциональности

Язык не тронут; версия языка не растёт. Возможен бамп версий крейтов
при расширении публичного API (`private_interfaces` → `pub`).

### Риски и зависимости

- Массовые автоправки clippy в `generator/c/` рискуют задеть вывод C — сверять.
- Снятие `#[allow]` может обнажить настоящий долг (например, `too_many_arguments`)
  → решать точечно, а не флагом.

<!-- свод: при большом объёме декомпозировать на docs/analyze/0046-YY-slug.md. -->

## Разработка

### Задача

#### Что было

`Location::Source(u64, usize, usize)` = 32 байта → `Diagnostic` = **136** байт >
порога 128 линта `clippy::result_large_err` (включён по умолчанию в clippy 0.1.99).
**414** предупреждений на 203+ `Result<_, Diagnostic>`.

#### Что сделано

1. **`diagnostics.rs`:** `Source(u32, u32, u32)` (вариант 16 байт → `Diagnostic`
   120 байт). Аксессоры (`start`/`end`/`range`/`try_start`/`try_end`/`try_range`)
   кастуют `u32 → usize` внутри — публичный API методов неизменен. Новый
   хелпер-конструктор `Location::source(file: u64, start: usize, end: usize)`
   кастует «широкие» типы вызывающего в `u32`.
2. **Конструирование (160 в `grammar.lalrpop` + 9 в лексере)** — sed
   `Location::Source(` → `Location::source(` (всё в action-коде — конструирование,
   не паттерны). `grammar.rs` генерируется в `OUT_DIR` из `.lalrpop`.
3. **Разбор** (`index.rs`, `docs`, `comments`, `lib.rs`, `address_map`, `lsp/*`,
   тесты) — `as usize` у места использования смещений, `as u64` при сравнении
   `file_no` с `ROOT_FILE_NO`/передаче в `path`.

#### Проверки

- `cargo build --all-targets --all-features` — 0 ошибок; `result_large_err` = 0.
- `cargo test -p grammar --all-features` — позиции/LSP/диагностики зелёные
  (усечение `usize → u32` безопасно: смещения `.lam` малы).
- `git diff examples/generated` — пусто (вывод не зависит от типа `Location`).

### Задача

#### Что было

После [устранение всех предупреждений сборки (rustc + clippy)](0046-build-warnings-cleanup.md#разработка) — ~135 clippy (обычных) + 3 rustc.

#### Что сделано

- **`cargo clippy --fix --all-targets --all-features`** (два прохода): needless_ref
  (24), collapsible_if (18), clone_on_copy (16), needless_borrow (6),
  redundant_closure, map_or_else, … Вывод генераторов сверен — байт-в-байт неизменен.
- **Ручные `#[allow]` с обоснованием у места** (осознанные компромиссы, как ~38
  существующих): `too_many_arguments` (печатники `c_expr/fixed.rs`
  `binary`/`negate`/`cast`/`rescale`, `st_stmt::print_for`, `viewport::create_svg`,
  `SimulationRunner::new`); `large_enum_variant` (`UnitKind` — `Node` доминирует);
  `type_complexity` (`Predicate.func`); `upper_case_acronyms` (`Viewport::SVG`);
  `redundant_guards` (`if b == 0.0` — паттерн `0.0` дал бы
  `illegal_floating_point_literal_pattern`).
- **Ручные правки:** `doc_lazy_continuation` (×3 — `//` между doc-блоками сливал
  список и прозу → разделены пустым `///`); `field_reassign_with_default`
  (struct-update + сжат doc-хелпер, чтобы `c_header.rs` не вырос сверх реестра);
  `assertions_on_constants` (`assert!(false, …)` → `panic!`);
  `needless_range_loop` (→ `enumerate`); `get(k).is_none()` → `!contains_key(k)`.
- **rustc:** `unused_imports` (clippy --fix); `missing_docs`
  (`lsp::collect_diagnostics` — добавлен doc); `dead_code` (`Item.end` в
  `format/comments.rs` — мёртвое поле удалено).

#### Проверки

- `cargo clippy --all-targets --all-features -- -D warnings` — EXIT=0.
- `git diff examples/generated` — пусто. `precheck.sh` — тесты зелёные.
- Ловушка: `grep -c warning` по кэшированному clippy дал ложный **0** —
  истинный остаток виден лишь на свежей компиляции / под `-D warnings`.

### Задача

#### Что было

`precheck.sh` гонял `cargo clippy --all-targets --all-features` **без** `-D
warnings` (информационно); CI — `build`/`check`/`test` без проверки предупреждений.
Долг копился молча (549 к 2026-07-19).

#### Что сделано

- **`scripts/precheck.sh`:** шаг clippy → `cargo clippy --all-targets
  --all-features -- -D warnings`. Clippy гоняет и clippy-, и rustc-линты — один
  флаг закрывает оба набора.
- **`.github/workflows/ci.yml`:** новый шаг «Линты (0 предупреждений)»
  — `clippy --all-targets --all-features -- -D warnings` после «Проверка».
- **Почему CLI-уровень, а не `#![deny(warnings)]`:** запрет `docs/CODE.md` —
  `deny(warnings)` в коде ломает сборку **у пользователя** при обновлении
  компилятора. Флаг в скрипте ломает лишь `precheck`/CI — что и требуется:
  предупреждение устраняют, а не копят. Точечные исключения — `#[allow(...)]` с
  обоснованием у места (прецедент — `deny(clippy::wildcard_enum_match_arm)` в
  `eval/mod.rs`).

#### Проверки

- `cargo clippy --all-targets --all-features -- -D warnings` — EXIT=0.
- `./scripts/precheck.sh` — EXIT=0 (шаг clippy проходит).
- Проба: временно вернуть предупреждение → `precheck`/CI падает (защёлка работает).

## Тест-план

### Критерии и проверки

| # | Критерий (R/A) | Проверка | Ожидание |
|---|---|---|---|
| T1 | A1 — 0 rustc | `cargo build --all-targets --all-features 2>&1 \| grep -c "^warning"` (без `src/grammar.rs`) | 0 |
| T2 | A2 — 0 clippy | `cargo clippy --all-targets --all-features -- -D warnings` | EXIT=0 |
| T3 | A3 — codegen не изменился | `git diff examples/generated` после регенерации | пусто |
| T4 | A3 — поведение | `cargo test -- --test-threads=1`; `conformance_{c,rust,st,sv}` | зелёные |
| T5 | A4 — инвариант цел | `deny(clippy::wildcard_enum_match_arm)` в `eval/mod.rs` на месте; проба `_ =>` в `eval/` валит сборку | сохранён |
| T6 | R4 — закрепление | `grep "\-D warnings" scripts/precheck.sh .github/workflows/ci.yml` | найдено |
| T7 | R4 — защёлка работает | временно вернуть предупреждение → `precheck`/clippy падает | падает |
| T8 | `result_large_err` | `cargo clippy … 2>&1 \| grep -c "result_large_err"` | 0 |
| T9 | `Diagnostic` < 128 | `result_large_err` не срабатывает (косвенно) | подтверждено |
| T10 | Позиции целы | `cargo test -p grammar --all-features` (LSP/диагностики/позиции по смещениям) | зелёные |
| T11 | Осознанные `#[allow]` | новые `#[allow(...)]` имеют обоснование у места | да |
| T12 | precheck | `./scripts/precheck.sh` | EXIT=0 |

### Направление ошибки

Усечение `usize → u32` для смещений/номеров файлов безопасно для `.lam` (влезают
с запасом); ошибка в сторону паники (`as` усечёт, но значения малы) — не тихого
неверного результата. Контроль — `cargo test -p grammar` (позиции по смещениям:
`line_column`, LSP goto, диагностики).

## Отчёт о тестировании

- **Фича:** [устранение всех предупреждений сборки (rustc + clippy)](0046-build-warnings-cleanup.md)
- **ADR:** [устранение всех предупреждений сборки (rustc + clippy)](0046-build-warnings-cleanup.md#архитектура-adr) · **Анализ:** [устранение всех предупреждений сборки (rustc + clippy)](0046-build-warnings-cleanup.md#анализ) · **Тест-план:** [устранение всех предупреждений сборки (rustc + clippy)](0046-build-warnings-cleanup.md#тест-план)
- **Задачи:** 0046-01 (ужатие `Location`), 0046-02 (механическая чистка), 0046-03 (закрепление)
- **Дата:** 2026-07-19
- **Вердикт:** ✅ **ГОТОВО**. `./scripts/precheck.sh` — EXIT=0; rustc 0 / clippy 0; вывод генераторов байт-в-байт неизменен; ноль-долг закреплён `-D warnings`.

### Сводка

Предупреждения сборки обоих крейтов сведены к нулю и закреплены `-D warnings`
на шаге clippy в `precheck.sh` и CI (Option B ADR). Главная находка разработки:
свежий инвентарь дал **549** clippy (снимок карточки 2026-07-15 — ~106), из них
**414 — один класс `result_large_err`** (стал `#[warn]` по умолчанию в clippy
0.1.99). Разрешено ужатием `Diagnostic` ниже порога 128 байт через `Location`
(решение заказчика).

### Эталон «до» → «после»

| Инструмент | До (свежий, 2026-07-19) | После |
|---|---|---|
| rustc (`build --all-targets --all-features`) | 3 (`unused_imports`, `missing_docs`, `dead_code`) | **0** |
| clippy (`--all-targets --all-features`) | **549** (из них 414 `result_large_err`) | **0** |

Инвентарь карточки (2026-07-15: 19 rustc + ~106 clippy) **устарел**: 0036
убрала 11 `private_interfaces`, а clippy 0.1.99 добавил `result_large_err` (414).

### Что сделано

- **0046-01 — `Location::Source(u64,usize,usize) → (u32,u32,u32)`.** Вариант 16
  байт вместо 32 → `Diagnostic` 136 → **120** байт → все **414**
  `result_large_err` исчезли **без** `#[allow]` и **без** правки сигнатур
  `Result`. Публичный API методов `Location` остался в `usize`/`String`: каст
  локализован в аксессорах (`start`/`end`/`range`/`try_*`) и в новом
  хелпере-конструкторе `Location::source(u64, usize, usize)`, которым заменены
  **160** конструирований в `grammar.lalrpop` и лексере (sed); разбор
  (`index.rs`, `docs`, `comments`, `lsp/*`) — `as usize` у места использования.
- **0046-02 — механическая чистка остатка (~135).** `cargo clippy --fix`
  (needless_ref 24, collapsible_if 18, clone_on_copy 16, needless_borrow 6, …) +
  ручные: `too_many_arguments` (7 → `#[allow]` с обоснованием — печатники
  генератора, `SimulationRunner::new`), `large_enum_variant` (`UnitKind` →
  `#[allow]`: `Node` — доминирующий вариант), `type_complexity`
  (`Predicate.func`), `upper_case_acronyms` (`Viewport::SVG`),
  `doc_lazy_continuation` (×3 — `//` между doc-блоками сливал список и прозу,
  разделены пустым `///`), `redundant_guards` (`if b == 0.0` → `#[allow]`: паттерн
  `0.0` дал бы `illegal_floating_point_literal_pattern`),
  `field_reassign_with_default` (struct-update), `assertions_on_constants`
  (`assert!(false, …)` → `panic!`), `needless_range_loop` (→ `enumerate`),
  `get(k).is_none()` → `!contains_key(k)`. rustc: `unused_imports` (clippy --fix),
  `missing_docs` (`collect_diagnostics` — добавлен doc), `dead_code`
  (мёртвое поле `Item.end` — удалено).
- **0046-03 — закрепление.** `cargo clippy --all-targets --all-features -- -D
  warnings` в `precheck.sh` и новый шаг CI. Clippy гоняет и clippy-, и
  rustc-линты — один флаг покрывает оба набора. CLI-уровень (**не** запрещённый
  `#![deny(warnings)]` в коде): обновление компилятора ломает `precheck`/CI, а не
  сборку у пользователя.

### Сверка с тест-планом

| # | Проверка | Результат |
|---|---|---|
| T1 | 0 rustc | ✅ 0 (без мёртвого `src/grammar.rs`) |
| T2 | 0 clippy (`-D warnings`) | ✅ EXIT=0, «No issues found» |
| T3 | Codegen байт-в-байт | ✅ `git diff examples/generated` — **пусто** |
| T4 | Поведение (тесты + conformance) | ✅ `precheck` тесты зелёные |
| T5 | Инвариант (`wildcard_enum_match_arm`) | ✅ не тронут |
| T6/T7 | Закрепление `-D warnings` | ✅ в `precheck.sh` и CI |
| T8/T9 | `result_large_err` = 0, `Diagnostic` < 128 | ✅ 414 → 0 |
| T10 | Позиции целы (LSP/диагностики) | ✅ `cargo test -p grammar --all-features` зелёные |
| T12 | precheck | ✅ EXIT=0 |

### Находки и отклонения

- **Ловушка кэша clippy.** `cargo clippy 2>&1 | grep -c warning` дал ложный
  **0** — при отсутствии перекомпиляции clippy не переэмитит предупреждения.
  Истинный остаток (25 не-машинных) виден лишь при `-D warnings` (промотирует в
  ошибки) или после `touch`. Урок: считать предупреждения только на свежей
  компиляции.
- **`cargo build --all-targets` Не компилирует inline `#[cfg(test)]` юниты lib.**
  8 «ошибок» первого precheck — clippy `-D warnings` на тестовом коде, невидимом
  для `build --all-targets`. Полный охват даёт только `cargo test`/clippy.
- **Мёртвый коммитнутый `grammar/src/grammar.rs`** (~29k строк) не компилируется
  (сборка из `OUT_DIR`) — вне объёма 0046, кандидат на удаление (в ADR).
- **База карточки устарела за 4 дня** (0036 + clippy 0.1.99) — инвентарь
  пересчитан по факту (урок повторился).

### Дефекты

Не найдено. Исправления (`docs/fixes/0046-YY-*`) не заводились.

### Итог

Критерии A1–A4 и требования R1–R4 выполнены. Язык **не менялся**:
`Location` — внутренняя структура диагностик, не конструкция языка. Крейт
`grammar` — минорный бамп **0.6.0 → 0.7.0**: смена типов полей публичного
`Location::Source(u64,usize,usize) → (u32,u32,u32)` — ломающее изменение API по
SemVer 0.x (внешний код, конструирующий/разбирающий вариант, требует правки; в
репозитории — уже поправлен). Фича закрыта.

## Итог (что сделано)

Предупреждения обоих крейтов сведены к **нулю** (rustc 0, clippy 0) и закреплены
`-D warnings` на шаге clippy в `precheck.sh` и CI (Option B ADR). Вывод генераторов
байт-в-байт неизменен (`git diff examples/generated` пуст), поведение цело (тесты
+ conformance зелёные).

**Инвентарь пересчитан по факту:** снимок карточки (2026-07-15, ~125) устарел за
4 дня. Свежий прогон — **549 clippy** (не ~106), из них **414 — один класс
`clippy::result_large_err`** (стал `#[warn]` по умолчанию в clippy 0.1.99;
`Diagnostic` = 136 байт > порога 128). Разрешено **ужатием `Location::Source(u64,
usize, usize) → (u32,u32,u32)`** (`Diagnostic` → 120 байт; решение заказчика,
отвергнуты `Box<Diagnostic>` и крейт-level `#[allow]`). Публичный API методов
`Location` остался в `usize`/`String` — каст локализован в аксессорах и
хелпере-конструкторе `Location::source`, которым заменены 160 конструирований в
грамматике/лексере. Остаток (~135) — `clippy --fix` + ручные (`too_many_arguments`
и др. → `#[allow]` с обоснованием) + 3 rustc.

- **Крейт `grammar` 0.6.0 → 0.7.0** (смена типов полей публичного `Location::Source`
  — ломающее по SemVer 0.x). Язык **не менялся**.
- **Разблокировала** себя закрытием зависимости 0036 (была `ЗАБЛОКИРОВАНА`).
- **Отчёт:** [`0046-build-warnings-cleanup.md#отчёт-о-тестировании`](0046-build-warnings-cleanup.md#отчёт-о-тестировании)
  (precheck EXIT=0; rustc 0 / clippy 0).
- **Исправления:** не заводились.
- Уроки: *кэш clippy даёт ложный 0 предупреждений* (считать на свежей
  компиляции); *`cargo build --all-targets` не компилирует inline `#[cfg(test)]`
  юниты lib* (полный охват — только `cargo test`/clippy); *база карточки —
  снимок, сверяй с кодом на момент взятия в работу* (урок повторился).
- Кандидат (вне области): мёртвый коммитнутый `grammar/src/grammar.rs` (~29k строк,
  сборка из `OUT_DIR`) — на удаление.
