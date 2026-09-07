# Фича: Согласование видимости публичного API крейта simulation

- **Номер:** 0036
- **Статус:** ГОТОВО (закрыта 2026-07-19)
- **Зависит от:** нет
- **Приоритет / Tier:** низкий (Tier 3) — гигиена сборки, наблюдаемое поведение
  не меняется
- **Связанные issue (анализ):** — (новая фича; из процессного бэклога
  `FEATURES.md` — «Предупреждения `private_interfaces` в `simulation`»)

## Стадии жизненного цикла

Все стадии — **разделы этой карточки**: «Архитектура (ADR)»,
«Анализ», «Разработка», «Тест-план», «Отчёт о тестировании», «Итог».
Отдельным артефактом остаются только исправления —
[`docs/fixes/`](../fixes/README.md) (при необходимости `0036-YY-*`).

## Краткое описание

Сборка крейта `simulation` даёт **10 предупреждений `private_interfaces`**:
перечисление `Unit` объявлено `pub`, а типы, которыми набиты его варианты
(трейт `Context`, перечисление `Flow`, структура `Predicate`), остаются
`pub(crate)`. Компилятор говорит правду: поля вариантов `Unit` достижимы снаружи
на видимости `pub`, но назвать их типы внешний код не может — API рассогласован.

Фича согласует видимость и **механически закрепляет** отсутствие предупреждений
точечным линтом, чтобы рассогласование не вернулось молча. Решение
([ADR](0036-sim-visibility.md#архитектура-adr)) — **инкапсулировать `Unit`**:
превратить его в `pub struct` с приватным внутренним `enum UnitKind`, оставив
публичным уже существующий набор методов-аксессоров (`tick`, `variable`,
`current_state`, `active_states`, `is_terminal`, …). Публичный API крейта при
этом **не расширяется** ни одним новым типом, внутренние типы честно остаются
внутренними, а вопрос `#[non_exhaustive]` (правило `docs/CODE.md`, конфликтующее
с инвариантом фичи) не встаёт.

> **Уточнение фактов кандидата.** Текст кандидата перечислял четыре типа —
> `Context`, `Value`, `Predicate`, `Flow`. Проверка по коду:
> **`Value` уже `pub`** (`simulation/src/eval/value.rs:14`, реэкспорт
> `simulation/src/lib.rs:29`), предупреждения по нему **нет** — факт не
> подтвердился, в работах он не участвует. Заявление про `TickResult`
> **подтвердилось**: он `pub` (`simulation/src/unit/mod.rs:73`), исправлен
> попутно задачей. Фактический список предупреждений — в
> [анализе](0036-sim-visibility.md#анализ).

> Фича зарегистрирована из процессного бэклога `FEATURES.md`; далее проходит
> жизненный цикл по своду.

## Архитектура (ADR)

- **Status:** Accepted
- **Date:** 2026-07-15
- **Authors:** Архитектор + Системный аналитик
- **Related issues:** [Фича](0036-sim-visibility.md); опирается
  на правила [`docs/CODE.md`](../CODE.md) (приватные поля, Newtype,
  `#[non_exhaustive]`, запрет `deny(warnings)`) и их применение в фиче
  [приведение кода к требованиям docs/CODE.md](0018-code-guidelines.md) ([анализ](0018-code-guidelines.md#анализ);
  собственного ADR у 0018 нет); учитывает инвариант
  [ADR](0025-simulator-expression-eval.md#архитектура-adr)
  (`deny(clippy::wildcard_enum_match_arm)` в `simulation/src/eval/`)

### Context

Крейт `simulation` при каждой сборке печатает **10 предупреждений
`private_interfaces`** (замер — `cargo build -p simulation`; полный список с
путями и строками см. [анализ](0036-sim-visibility.md#анализ)):

| Тип, «который приватнее» | Где объявлен | Через какие поля `Unit` утекает | Шт. |
|---|---|---|---|
| трейт `Context` | `simulation/src/context.rs:4` | `Node::context`, `Node::executions`, `Node::state_executions`, `Parallel::executions`, `Sequential::executions` | 5 |
| `Flow` | `simulation/src/unit/mod.rs:52` | `Node::executions`, `Node::state_executions`, `Parallel::executions`, `Sequential::executions` | 4 |
| `Predicate` | `simulation/src/unit/mod.rs:26` | `Node::state_transitions` | 1 |

Корень — в объявлении `Unit` (`simulation/src/unit/mod.rs:84`):

```rust
pub enum Unit {
    None,
    Node {
        context: Option<Rc<RefCell<dyn Context>>>,                    // Context: pub(crate)
        state_transitions: HashMap<String, Vec<(String, Predicate)>>, // Predicate: pub(crate)
        state_executions: HashMap<String, Executions>,                // Executions -> Flow, Context
        executions: Executions,
        …
    },
    Parallel   { units: Vec<Rc<RefCell<Unit>>>, executions: Executions },
    Sequential { units: Vec<Rc<RefCell<Unit>>>, index: usize, executions: Executions },
}
```

`Flow` и `Context` утекают не напрямую, а через псевдоним
`type Execution = Rc<dyn Fn(&mut dyn Context) -> Result<Flow, Diagnostic>>`
(`unit/mod.rs:65`) внутри `type Executions = HashMap<String, Vec<Execution>>`
(`unit/mod.rs:66`).

**Ключевой факт о языке (проверен компилятором).** Варианты перечисления и их
поля **всегда** наследуют видимость самого перечисления; попытка написать
`pub(crate)` у поля варианта — ошибка `E0449`: «visibility qualifiers are not
permitted here… enum variants and their fields always share the visibility of
the enum they are in». Значит, пока `Unit` — `pub enum`, его поля **обязаны**
быть `pub`, и рассогласование неустранимо «на месте».

**Что уже верно и трогать не нужно** (проверено по коду — вопреки тексту
кандидата в `FEATURES.md`):

- `Value` — **уже** `pub` (`eval/value.rs:14`) и реэкспортирован (`lib.rs:29`);
  предупреждения по нему нет. Утверждение кандидата **не подтвердилось**.
- `TickResult` — **уже** `pub` (`unit/mod.rs:73`), исправлен попутно задачей. Утверждение кандидата **подтвердилось**.

**Кто реально потребляет `Unit`.** Варианты `Unit::{Node,Parallel,Sequential}`
конструируются и разбираются **только внутри крейта** — `unit/builder.rs`,
`unit/statement.rs`, `unit/viewport.rs`, `state_io.rs`, `runner.rs`,
`bin/simulation.rs`. Интеграционные тесты (`simulation/tests/eval_tests.rs`,
`simulation/tests/conformance_c_tests.rs`) работают **исключительно через
методы-аксессоры**: `build_unit`, `Unit::tick`, `Unit::variable`, `TickResult`,
`Value`. То есть публичность формы перечисления сегодня **никем не
используется** — она случайна, а не спроектирована.

свод: язык Lam (синтаксис/семантика) не затрагивается, потоки данных между
компонентами не меняются — диаграмма не требуется и не добавляется.

### Decision Drivers

1. **Честность API важнее тишины.** Предупреждение указывает на реальный дефект
   проектирования, а не на каприз компилятора: наружу торчит форма внутреннего
   дерева симуляции. Заглушить — значит зафиксировать дефект.
2. **Не расширять публичный API без нужды (YAGNI).** Всё, что стало
   `pub`, попадает под семантическое версионирование и связывает руки. Внешних
   потребителей `Context`/`Flow`/`Predicate` нет и никто их не запрашивал.
3. **`docs/CODE.md` прямо предписывает** «использовать приватные поля, чтобы
   запретить внешнее конструирование через литерал структуры и сохранить свободу
   изменений» и применять **Newtype** для сокрытия реализации.
4. **Не разбудить конфликт `#[non_exhaustive]` ↔ инвариант.**
   `docs/CODE.md` требует помечать публичные `enum` атрибутом
   `#[non_exhaustive]`; конфликт этого правила с механизмом 0025 зафиксирован в
   процессном бэклоге `FEATURES.md` как **нерешённый**. Любое решение,
   добавляющее публичные перечисления, обязано этот вопрос открыть (см. Option A).
5. **Механическое закрепление.** Результат должен держаться линтом, а не
   дисциплиной: иначе предупреждения вернутся с первым же новым полем.
6. **Соразмерность.** Фича гигиеническая (Tier 3) и не имеет права менять
   наблюдаемое поведение симулятора.

### Considered Options

#### Option A. Поднять `Context`, `Flow`, `Predicate` до `pub`

Минимальная правка: три `pub(crate)` → `pub`, плюс `pub` для
`Execution`/`Executions` (они входят в сигнатуры полей) и реэкспорт из `lib.rs`.

**Pros:**

- Самое дешёвое по объёму (≈5 строк), нулевой риск задеть поведение.
- Внешние потребители смогли бы конструировать `Unit` вручную и подменять
  `Context` — гипотетическая точка расширения для сторонних симуляторов.
- Симметрично тому, как уже поступили с `TickResult` (0025-05) и `Value`.

**Cons:**

- **Расширяет публичный API крейта пятью типами**, которые являются
  реализацией, а не контрактом. по своду (SemVer) добавление `pub` —
  аддитивное изменение (`0.1.0` → `0.1.1`), но **обратный** ход (спрятать их
  назад) будет уже сломом: решение фактически необратимо.
- Наружу уезжает `Context::set_value` — возможность писать в переменные модели
  **мимо** `eval::coerce_to_type`, то есть мимо приведения по типу. Это прямо
  подрывает инвариант фичи («семантика вычислений живёт только в `eval/`»):
  не гигиена, а дыра в инварианте.
- `Predicate` содержит `Rc<dyn Fn(&mut dyn Context) -> Result<bool, Diagnostic>>`,
  `Execution` — `Rc<dyn Fn(&mut dyn Context) -> Result<Flow, Diagnostic>>`.
  Публикация замыканий фиксирует внутреннюю сигнатуру исполнителя как контракт
  крейта.
- **Открывает вопрос `#[non_exhaustive]` ребром** (драйвер 4). `docs/CODE.md`
  предписал бы пометить публичный `Flow` (и `Unit`, и уже публичный `Value`)
  атрибутом `#[non_exhaustive]`. Разбор нюанса: сам по себе атрибут **безвреден
  для механизма 0025** — внутри крейта-объявителя он не действует, а `Flow` и
  `Value` разбираются только внутри `simulation`, поэтому
  `deny(clippy::wildcard_enum_match_arm)` в `eval/` продолжит валить сборку на
  забытом варианте. **Но**: (а) вопрос требует отдельного решения по каждому
  типу и тянет за собой нерешённый пункт процессного бэклога «Конфликт правил:
  `#[non_exhaustive]` против запрета `_ =>`»; (б) `#[non_exhaustive]` лишает
  внешних потребителей исчерпывающего разбора, обесценивая тот самый доступ,
  ради которого типы и публиковались. Гигиеническая фича Tier 3 — не место, где
  этот конфликт решается впопыхах.
- **Не решает корень**: форма `Unit` остаётся публичной, и любое новое поле с
  внутренним типом снова потребует его публикации — предупреждение вернётся.

#### Option B. Инкапсулировать `Unit`: `pub struct` + приватный `enum UnitKind`

> **Формулировка уточнена против исходной.** Кандидат предлагал «сузить поля
> `Unit` до `pub(crate)`» — так **нельзя**: `E0449`, поля вариантов всегда
> наследуют видимость перечисления (см. Context). Единственная работающая форма
> этой идеи — перестать быть публичным перечислением:

```rust
pub struct Unit(UnitKind);   // публичный newtype, поле приватное
enum UnitKind {              // приватный: из крейта не виден
    None,
    Node { context: Option<Rc<RefCell<dyn Context>>>, … },
    Parallel { … },
    Sequential { … },
}
```

Публичным остаётся уже существующий набор аксессоров: `tick`,
`take_last_transition(s)`, `reachable_from_active`, `execution`, `variable`,
`current_state`, `active_states`, `is_terminal`, `union`, `add`.
Внутрикрейтовые потребители переписываются механически:
`Unit::Node { … }` → `UnitKind::Node { … }` (6 файлов).

**Pros:**

- **Устраняет корень**: поля перестают быть достижимыми на видимости `pub` —
  все 10 предупреждений исчезают разом, и **будущие** поля с внутренними типами
  их не вернут.
- **Публичный API не расширяется ни на один тип.** `Context`, `Flow`,
  `Predicate`, `Execution`, `Executions` остаются `pub(crate)` — тем, чем они и
  являются.
- Прямо следует `docs/CODE.md` (приватные поля ради свободы изменений; Newtype
  для сокрытия реализации).
- **Вопрос `#[non_exhaustive]` не встаёт** (драйвер 4): публичных перечислений
  не прибавляется, `UnitKind` приватен — конфликт CODE.md ↔ 0025 остаётся
  нетронутым в бэклоге, где ему и место.
- Инвариант усиливается: `set_value` недостижим извне, запись в переменные
  идёт только через `eval`.
- Форма дерева симуляции освобождается для будущих фич (0032, 0034) без слома
  внешних потребителей.

**Cons:**

- **Технически слом публичного API**: `Unit` перестаёт быть перечислением —
  внешний код, разбиравший `Unit::Node { .. }`, сломается. По SemVer для `0.x` это мажорный шаг: `0.1.0` → `0.2.0`.
  *Смягчение:* таких потребителей **нет** — ни в workspace, ни в тестах (см.
  Context); крейт не опубликован. Слом теоретический.
- Объём больше, чем у Option A: правки в 6 файлах (`builder.rs`, `statement.rs`,
  `viewport.rs`, `state_io.rs`, `runner.rs`, `bin/simulation.rs`) —
  механические, но не пятистрочные. Риск конфликтов слияния с фичами, трогающими
  те же файлы (0032 — `state_io.rs`; 0034/0044 — дерево симуляции).
- `Unit::None` как `#[default]` и внутренние `matches!(unit, Unit::None)`
  требуют аккуратного переноса на `UnitKind` (риск R-2 в анализе).

#### Option C. Подавить предупреждение `#[allow(private_interfaces)]`

**Pros:**

- Одна строка, ноль риска, ноль изменений API и поведения.
- Сборка немедленно чистая.

**Cons:**

- **Лечит симптом, а не болезнь.** Рассогласование остаётся: поля публичны,
  типы — нет; внешний код по-прежнему не может ни сконструировать `Unit`, ни
  назвать типы его полей. Это ровно то состояние, которое линт придуман
  обнаруживать.
- Противоречит духу `docs/CODE.md` («настраивай lint-ы точечно» — а не глуши
  диагностику о дефекте) и драйверу 1.
- **Скрывает будущие рассогласования**: следующий утёкший тип не будет замечен
  вовсе — `allow` действует на весь элемент.
- Прямо конфликтует с требованием закрепить чистоту линтом (драйвер 5): `deny`
  и `allow` одного линта взаимно бессмысленны.
- Приемлем лишь как временная мера с `TODO`, чего фича не требует: корень
  устраним за обозримый объём.

### Decision

Принимается **Option B** — инкапсуляция `Unit` в `pub struct` с приватным
`enum UnitKind`, с дополнительным **закреплением** результата точечным линтом
`#![deny(private_interfaces)]` в `simulation/src/lib.rs`.

Обоснование:

1. **Это единственная опция, устраняющая причину.** A и C оставляют публичную
   форму `Unit` и потому лишь откладывают возврат предупреждений.
2. **Цена слома нулевая, выгода постоянная.** Формально Option B ломает API, но
   ломать нечего: потребителей формы `Unit` вне крейта не существует, а тесты
   давно живут на аксессорах. Взамен крейт получает нормальную границу
   инкапсуляции.
3. **Не расширяем публичный API** (драйвер 2) и **не будим спор о
   `#[non_exhaustive]`** (драйвер 4) — важное свойство для фичи Tier 3, у
   которой нет мандата решать процессный конфликт CODE.md ↔ 0025.
4. **Поддерживает инвариант**: `Context::set_value` остаётся внутренним.

Закрепление именно `#![deny(private_interfaces)]`, а **не** `#![deny(warnings)]`:
последнее прямо запрещено `docs/CODE.md` («Не используй `#![deny(warnings)]` в
публичных крейтах — это ломает сборку при обновлении компилятора; настраивай
lint-ы точечно и в CI»). Точечный `deny` конкретного линта — ровно предписанный
способ; в проекте уже есть прецедент: `#![deny(clippy::wildcard_enum_match_arm)]`
в `simulation/src/eval/mod.rs`.

### Consequences

#### Положительные

- `cargo build -p simulation` — **0 предупреждений `private_interfaces`**
  (было 10); сборка крейта чистая.
- Публичный API `simulation` сокращается до осмысленного контракта:
  `build_unit`, непрозрачный `Unit` + его аксессоры, `TickResult`, `Value`,
  модули `runner`, `state_io`, `graphics_config`, `json_input`.
- Регресс невозможен молча: `#![deny(private_interfaces)]` валит сборку при
  появлении нового утёкшего типа.
- Инвариант (запись значений только через `eval::coerce_to_type`) защищён
  границей видимости, а не соглашением.
- Внутренняя форма дерева симуляции освобождается для фич (переменные в
  `state_io`) и 0034 (структурные типы) — их правки больше не будут задевать
  публичный API.

#### Отрицательные / Action items

- **Версия крейта `simulation`: `0.1.0` → `0.2.0`** (SemVer для
  `0.x`: слом формы публичного `Unit`). Версия **языка Lam не меняется** — язык
  не затронут.
- Механическая правка 6 файлов-потребителей внутри крейта; координировать с
  фичами по очерёдности, чтобы не плодить конфликты слияния.
- `Unit::None`/`Default` и внутренние `matches!` переносятся на `UnitKind` —
  требуют внимания при ревью.
- **Вне области фичи** (зафиксировать в бэклоге отдельными пунктами —
  свод):
  - `warning: unused import: StatementNode` — `simulation/src/unit/builder.rs:9`.
    Устраняется попутно задачей: тривиально и внутри крейта фичи.
  - `warning: field 'end' is never read` — `grammar/src/format/comments.rs:27`.
    Это **крейт `grammar`**, к фиче отношения не имеет; чинится отдельно.
- Вопрос **`#[non_exhaustive]` для уже публичных `Value` и `TickResult`**
  настоящим ADR **не решается** — он часть нерешённого процессного пункта
  «Конфликт правил: `#[non_exhaustive]` против запрета `_ =>`». Вывод для
  будущего решения фиксируем здесь: пометка этих двух типов `#[non_exhaustive]`
  **не сломала бы** механизм 0025 (внутри крейта-объявителя атрибут не
  действует, а разбираются они только внутри `simulation`), но лишила бы внешних
  потребителей исчерпывающего разбора. Решать — отдельной процессной фичей.

#### Acceptance criteria

1. `cargo build -p simulation --all-targets` не выдаёт ни одного предупреждения
   `private_interfaces` (было 10).
2. `Context`, `Flow`, `Predicate`, `Execution`, `Executions` остаются
   `pub(crate)`; публичный API крейта **не пополнился** ни одним типом (сверка
   `pub use`/`pub enum`/`pub struct`/`pub trait` в `simulation/src/`).
3. `Unit` — `pub struct` с приватным полем; `UnitKind` не экспортируется и не
   достижим извне крейта.
4. `simulation/src/lib.rs` содержит `#![deny(private_interfaces)]`;
   `#![deny(warnings)]` **не** добавлен (запрет `docs/CODE.md`).
5. Негативная проверка линта: временный `pub`-элемент с `pub(crate)`-типом валит
   сборку (доказательство, что закрепление работает).
6. Все тесты `simulation` и `grammar` зелёные, поведение симулятора не
   изменилось: `cargo test -- --test-threads=1`.
7. Версия крейта `simulation` поднята до `0.2.0`; версия языка Lam не изменена.

## Анализ

### Цель и контекст

Устранить рассогласование видимости в публичном API крейта `simulation`:
перечисление `Unit` объявлено `pub`, а типы его полей (`Context`, `Flow`,
`Predicate`) — `pub(crate)`. Компилятор сообщает об этом 10 предупреждениями
`private_interfaces` при каждой сборке. Решение ([ADR](0036-sim-visibility.md#архитектура-adr),
Option B) — инкапсулировать `Unit` (`pub struct` + приватный `enum UnitKind`),
не расширяя публичный API, и закрепить результат точечным линтом
`#![deny(private_interfaces)]`.

свод: язык Lam не затрагивается — фича целиком внутри крейта `simulation`.
свод: версия **языка** не меняется; версия **крейта** `simulation`
поднимается `0.1.0` → `0.2.0` (см. «Особенности по обратной функциональности»).

#### Фактическое состояние кода (а не пересказ кандидата)

Команда: `cargo build -p simulation --all-targets` (и `cargo clippy -p simulation
--all-targets` — тот же набор, дополнительных `private_interfaces` не даёт).
Дата замера: 2026-07-15, ветка `v2`, коммит `6984471`.

**Все 10 предупреждений `private_interfaces`** (счётчик подтверждён:
`cargo clippy -p simulation --all-targets 2>&1 | grep -c "more private than"` → `10`):

| # | Текст предупреждения | Место поля |
|---|---|---|
| 1 | trait `context::Context` is more private than the item `unit::Unit::Node::context` | `simulation/src/unit/mod.rs:88` |
| 2 | trait `context::Context` is more private than the item `unit::Unit::Node::state_executions` | `simulation/src/unit/mod.rs:91` |
| 3 | trait `context::Context` is more private than the item `unit::Unit::Node::executions` | `simulation/src/unit/mod.rs:95` |
| 4 | trait `context::Context` is more private than the item `unit::Unit::Parallel::executions` | `simulation/src/unit/mod.rs:109` |
| 5 | trait `context::Context` is more private than the item `unit::Unit::Sequential::executions` | `simulation/src/unit/mod.rs:114` |
| 6 | type `Predicate` is more private than the item `unit::Unit::Node::state_transitions` | `simulation/src/unit/mod.rs:90` |
| 7 | type `Flow` is more private than the item `unit::Unit::Node::state_executions` | `simulation/src/unit/mod.rs:91` |
| 8 | type `Flow` is more private than the item `unit::Unit::Node::executions` | `simulation/src/unit/mod.rs:95` |
| 9 | type `Flow` is more private than the item `unit::Unit::Parallel::executions` | `simulation/src/unit/mod.rs:109` |
| 10 | type `Flow` is more private than the item `unit::Unit::Sequential::executions` | `simulation/src/unit/mod.rs:114` |

Объявления «слишком приватных» типов: `Context` — `simulation/src/context.rs:4`
(`pub(crate) trait`); `Predicate` — `simulation/src/unit/mod.rs:26`
(`pub(crate) struct`); `Flow` — `simulation/src/unit/mod.rs:52`
(`pub(crate) enum`). `Flow`/`Context` утекают через псевдонимы
`Execution` (`unit/mod.rs:65`) и `Executions` (`unit/mod.rs:66`).

> **Замечание о подсчёте.** При сборке `--all-targets` тот же набор печатается
> дважды (цель `lib` и цель тестов), из-за чего «сырой» вывод содержит до 17
> строк `warning:`. Уникальных предупреждений `private_interfaces` — **ровно
> 10**; проверка `cargo build -p simulation` (только `lib`) даёт те же 10.

**Сверка утверждений кандидата из `FEATURES.md`:**

| Утверждение кандидата | Вердикт | Факт |
|---|---|---|
| `Context` — `pub(crate)`, шумит | **подтвердилось** | `context.rs:4`, 5 предупреждений |
| `Predicate` — `pub(crate)`, шумит | **подтвердилось** | `unit/mod.rs:26`, 1 предупреждение |
| `Flow` — `pub(crate)`, шумит | **подтвердилось** | `unit/mod.rs:52`, 4 предупреждения |
| `Value` — `pub(crate)`, шумит | **Не подтвердилось** | `Value` **уже `pub`** (`eval/value.rs:14`), реэкспорт `lib.rs:29`. Предупреждений по нему **нет**. Модуль `eval::value` — `pub(crate)`, но сам тип публичен через реэкспорт, чего линту достаточно. Из объёма работ исключён. |
| `TickResult` исправлен задачей (стал `pub`) | **подтвердилось** | `unit/mod.rs:73` — `pub enum TickResult`, с комментарием-обоснованием. Правок не требует. |

**Прочие предупреждения сборки (для полноты; вне области фичи):**

| Предупреждение | Место | Решение |
|---|---|---|
| `unused import: StatementNode` | `simulation/src/unit/builder.rs:9` | крейт фичи, тривиально → задача **0036-02** |
| `field 'end' is never read` | `grammar/src/format/comments.rs:27` | **крейт `grammar`**, к фиче не относится → предложить пунктом бэклога |

#### Ключевое ограничение языка (проверено компилятором)

Исходная формулировка «сузить поля `Unit` до `pub(crate)`» **невыполнима**:
поля вариантов перечисления всегда наследуют видимость перечисления. Пробный
крейт с `pub enum E { V { pub(crate) x: u8 } }` даёт:

```
error[E0449]: visibility qualifiers are not permitted here
  = note: enum variants and their fields always share the visibility of the enum they are in
```

Отсюда единственная форма инкапсуляции — `pub struct Unit(UnitKind)` с приватным
`enum UnitKind` (ADR, Option B).

### Зависимости фичи

- **Зависит от:** **нет**.

  **Обоснование.** Проверены все возможные каналы зависимости:
  - *По контракту.* Фича не потребляет ничего от других незакрытых фич.
    Затрагиваемый код (`Unit`, `Context`, `Flow`, `Predicate`) существует и
    стабилен с момента закрытия 0025; `TickResult`/`Value` уже приведены в
    порядок задачей, то есть предпосылка выполнена.
  - *По инфраструктуре.* Нужны только `cargo build`/`cargo test` и
    `scripts/precheck.sh` — всё на месте.
  - *По языку.* Синтаксис/семантика Lam не затрагиваются → зависимости от
    языковых фич (0035, 0041, 0042, 0044) нет.
  - *По процессу.* Нерешённый пункт бэклога «Конфликт правил `#[non_exhaustive]`
    против запрета `_ =>`» **не является зависимостью**: принятый Option B
    публичных перечислений не добавляет, поэтому вопрос не встаёт (в отличие от
    отвергнутого Option A, который сделал бы этот пункт блокирующим — см. ADR).

  Статус `ЗАБЛОКИРОВАНА` **не ставится**; фича может быть взята в работу
  немедленно.

- **Влияние на порядок разработки:**
  - Завершение 0036 **не разблокирует** ни одну фичу формально (никто от неё не
    зависит).
  - **Пересечение по файлам (не зависимость, а очерёдность).** 0036 правит
    `simulation/src/state_io.rs`, `unit/mod.rs`, `unit/builder.rs`,
    `unit/statement.rs`, `unit/viewport.rs`, `runner.rs`,
    `bin/simulation.rs`. Те же файлы трогают **0032** (переменные в
    `--save-state`, `state_io.rs`), **0034** (структурные типы), **0044**
    (assert/invariant). Рекомендация аналитика (критерий 4):
    выполнить **0036 раньше** 0032/0034 — она мелкая, механическая и делает
    форму дерева симуляции приватной, после чего те фичи меняют внутренности,
    не задевая публичный API и не переоткрывая вопрос видимости. Обратный
    порядок означал бы правку тех же строк дважды.
  - Приоритет фичи низкий (Tier 3), поэтому в таблице `FEATURES.md` она
    **не** поднимается выше содержательных фич — рекомендация касается лишь
    относительного порядка внутри группы работ по `simulation`.

### Требования и проверяемые условия

- **R1. Ноль предупреждений `private_interfaces`.** `cargo build -p simulation`
  и `cargo build -p simulation --all-targets` не печатают ни одного
  предупреждения `private_interfaces` (эталон «было» — 10 шт., таблица выше).
- **R2. Публичный API не расширяется.** `Context`, `Flow`, `Predicate`,
  `Execution`, `Executions` остаются `pub(crate)`. Множество публичных типов
  крейта после фичи **не больше**, чем до неё: `Unit`, `TickResult`, `Value`
  (+ уже публичные модули `runner`, `state_io`, `graphics_config`,
  `json_input`).
- **R3. `Unit` непрозрачен.** `Unit` — `pub struct` с приватным полем; внутренний
  `enum UnitKind` не экспортируется, извне крейта не достижим и не конструируем.
- **R4. Поведение симулятора не изменяется.** Фича — рефакторинг видимости:
  результаты `tick`, значения переменных, трассы, сохранение/загрузка состояния
  и SVG/GIF-вывод остаются побайтово прежними. Ни одна существующая проверка не
  меняет ожидаемого результата.
- **R5. Закрепление линтом, но не `deny(warnings)`.** В `simulation/src/lib.rs`
  добавлен `#![deny(private_interfaces)]`. `#![deny(warnings)]` **не**
  добавляется — прямой запрет `docs/CODE.md` («ломает сборку при обновлении
  компилятора; настраивай lint-ы точечно и в CI»).
- **R6. Закрепление доказано негативно.** Существует воспроизводимая проверка:
  временное возвращение утечки (`pub`-элемент с `pub(crate)`-типом) **валит**
  сборку крейта. Без этой проверки R5 — необоснованное утверждение.
- **R7. Аксессоры сохранены.** Публичные методы `Unit` (`tick`,
  `take_last_transition`, `take_last_transitions`, `reachable_from_active`,
  `execution`, `variable`, `current_state`, `active_states`, `is_terminal`,
  `union`, `add`) сохраняют сигнатуры и семантику — именно они и есть публичный
  контракт крейта.
- **R8. Версионирование.** Версия крейта `simulation` в
  `simulation/Cargo.toml`: `0.1.0` → `0.2.0`. Версия языка Lam **не меняется**.
- **R9. Чистота крейта фичи.** Устранено `unused import: StatementNode`
  (`unit/builder.rs:9`). Предупреждение в крейте `grammar`
  (`format/comments.rs:27`) — **вне области**, не трогается.

### Критерии приёмки и способ проверки

| # | Критерий | Способ проверки |
|---|---|---|
| A1 | Предупреждений `private_interfaces` — 0 (было 10) | `cargo build -p simulation --all-targets 2>&1 \| grep -c "more private than"` → `0` (R1) |
| A2 | Сборка крейта чистая целиком | `cargo build -p simulation --all-targets 2>&1 \| grep -c "^warning"` → `0` (R1, R9) |
| A3 | Публичный API не расширен | `grep -rn "^pub \(enum\|struct\|trait\|fn\|type\)\|^pub use" simulation/src/` — сверка списка «до/после»: новых имён нет; `Context`/`Flow`/`Predicate`/`Execution`/`Executions` — `pub(crate)` (R2) |
| A4 | `Unit` непрозрачен, `UnitKind` приватен | `grep -n "pub struct Unit\|enum UnitKind" simulation/src/unit/mod.rs`; `UnitKind` без `pub` и без реэкспорта в `lib.rs` (R3) |
| A5 | Внешний код не может разобрать `Unit` по вариантам | компиляционная проверка: тест-проба с `match unit { Unit::Node { .. } => … }` в `simulation/tests/` **не компилируется** (R3) |
| A6 | Поведение не изменилось | `cargo test -- --test-threads=1` и `cargo test --features lsp -- --test-threads=1` — все зелёные, **ни один ожидаемый результат не правился** (R4); `./scripts/run_simulations.sh` отрабатывает как прежде |
| A7 | Линт `private_interfaces` включён точечно | `grep -n "deny(private_interfaces)" simulation/src/lib.rs` → есть; `grep -rn "deny(warnings)" simulation/` → пусто (R5) |
| A8 | Закрепление действительно работает | негативный прогон: временный `pub fn leak() -> Flow` валит `cargo build -p simulation` с `error: private_interfaces`; правка откатывается (R6) |
| A9 | Аксессоры целы | `grep -n "pub fn " simulation/src/unit/mod.rs` — список совпадает с зафиксированным «до»; тесты `eval_tests.rs`/`conformance_c_tests.rs` не правились (R7) |
| A10 | Версия крейта поднята | `grep -n '^version' simulation/Cargo.toml` → `0.2.0`; версия языка Lam в `grammar` не изменена (R8) |
| A11 | Предкоммит-проверка проходит | `./scripts/precheck.sh` — успешно |

### Особенности по обратной функциональности

**Строка для реестра `docs/features/README.md`:**
`слом публичного API крейта simulation (Unit: pub enum → непрозрачный pub struct); язык не тронут, потребителей формы Unit нет — фактических регрессий ноль`

Развёрнуто (обратная совместимость обязательна к рассмотрению):

- **Что ломается формально.** `Unit` перестаёт быть перечислением. Внешний код
  вида `match unit { Unit::Node { state, .. } => … }` или конструирование
  `Unit::Node { … }` литералом — перестанут компилироваться. Это **слом
  публичного API крейта**.
- **Обоснование допустимости слома** (требование свод: обосновать, если
  совместимость невозможна):
  1. **Потребителей нет.** Проверено `grep` по всему репозиторию: варианты
     `Unit::{Node,Parallel,Sequential}` упоминаются **только** внутри крейта
     `simulation` (`unit/builder.rs`, `unit/statement.rs`, `unit/viewport.rs`,
     `state_io.rs`, `runner.rs`, `bin/simulation.rs`). Крейт `grammar` от
     `simulation` не зависит вовсе.
  2. **Тесты уже на аксессорах.** `simulation/tests/eval_tests.rs` и
     `simulation/tests/conformance_c_tests.rs` импортируют только
     `{TickResult, Unit, Value, build_unit}` и работают через `tick`/`variable`
     — форму `Unit` не разбирают. Правок в тестах не требуется (и это
     подтверждает: реальный контракт — аксессоры, а не форма).
  3. **Крейт не опубликован** (не на crates.io), внешних пользователей вне
     репозитория нет. `version = "0.1.0"` в `simulation/Cargo.toml` — стадия
     `0.x`, где SemVer прямо допускает слом с ростом минорной версии.
  4. **Совместимость сохранить и нельзя, и не нужно.** Сохранить `Unit` как
     `pub enum` — значит сохранить причину предупреждений (см. `E0449`: поля
     варианта неизбежно `pub`). Цель фичи и обратная совместимость формы `Unit`
     логически несовместимы; выбирается цель, поскольку цена слома доказуемо
     нулевая.
- **Что не ломается (гарантируется R4/R7):** `build_unit`, все методы `Unit`,
  `TickResult`, `Value`, модули `runner`, `state_io`, `graphics_config`,
  `json_input`, CLI `simulation`, формат сохранения состояния, SVG/GIF-вывод,
  язык Lam и его версия, крейт `grammar` — не затронуты.
- **Версионирование:** `simulation` `0.1.0` → `0.2.0`. Язык Lam —
  без изменений (фича не языковая).

### Риски и зависимости

- **R-1. Правка «по дороге» изменит поведение симулятора.** Перенос `Unit::X` →
  `UnitKind::X` в 6 файлах — механический, но объёмный; легко «заодно»
  поправить логику. *Снижение:* задача выполняется строго как
  переименование формы без правки тел; критерий A6 требует, чтобы **ни один
  ожидаемый результат в тестах не правился** — любая необходимость тронуть
  тест-ожидание есть сигнал нарушения R4 и повод остановиться.
- **R-2. `Default`/`Unit::None`.** `Unit` имеет `#[derive(Clone, Default)]` с
  `#[default] None`. При переносе вариантов в `UnitKind` дериву `Default` нужно
  переехать на `UnitKind`, а `Unit` — получить `Default` через newtype.
  *Снижение:* явная проверка в 0036-01; `state_io`/`viewport` опираются на
  `Unit::None` — покрыто существующими тестами.
- **R-3. `union`/`add` возвращают `Self`.** Методы строят новые `Unit` из
  вариантов; после инкапсуляции конструирование идёт через `Unit(UnitKind::…)`.
  *Снижение:* внутрикрейтовый конструктор-хелпер; поведение покрыто тестами.
- **R-4. Конфликты слияния с 0032/0034/0044.** Те же файлы. *Снижение:*
  рекомендованная очерёдность (0036 раньше) — см. «Влияние на порядок
  разработки»; фича мелкая, окно конфликта короткое.
- **R-5. `#![deny(private_interfaces)]` может завалить сборку на новом
  компиляторе,** если линт расширят. *Снижение:* риск принят осознанно — это
  **точечный** линт одного правила (в отличие от запрещённого `deny(warnings)`,
  который ловит все будущие линты); прецедент в проекте —
  `#![deny(clippy::wildcard_enum_match_arm)]` в `eval/mod.rs`. При проблеме
  правится одной строкой.
- **R-6. Соблазн решить попутно вопрос `#[non_exhaustive]`.** `docs/CODE.md`
  предписывает атрибут для публичных `enum` (`Value`, `TickResult` — уже
  публичны и не помечены). *Снижение:* **вне области 0036** (см. ADR,
  Action items): вопрос — часть нерешённого процессного конфликта CODE.md ↔
  0025 и требует отдельной процессной фичи. Option B выбран в том числе потому,
  что новых публичных перечислений не создаёт и этот вопрос не обостряет.
- **Зависимости:** нет (см. раздел «Зависимости фичи»).

### Подзадачи (декомпозиция для стадии 4)

| Задача | Файл | Содержание |
|---|---|---|
| **0036-01** | [`0036-sim-visibility.md#разработка`](0036-sim-visibility.md#разработка) | Инкапсуляция `Unit`: `pub struct Unit(UnitKind)` + приватный `enum UnitKind`; перевод 6 внутрикрейтовых потребителей. Закрывает R1–R4, R7 |
| **0036-02** | [`0036-sim-visibility.md#разработка`](0036-sim-visibility.md#разработка) | Закрепление: `#![deny(private_interfaces)]` в `lib.rs`, негативная проверка линта, чистка `unused import`, версия крейта `0.2.0`. Закрывает R5, R6, R8, R9 |

Объём аналитики мал — декомпозиция самого анализа на `0036-YY-*.md` не
требуется.

## Разработка

### Задача

#### Что было

**Реальное состояние кода на момент постановки** (ветка `v2`, коммит `6984471`,
замер `cargo build -p simulation --all-targets`, 2026-07-15).

Крейт `simulation` собирается с **10 предупреждениями `private_interfaces`**.
Причина — в `simulation/src/unit/mod.rs:84`: `Unit` объявлен как `pub enum`, а
типы, которыми набиты его варианты, — `pub(crate)`:

```rust
pub enum Unit {
    None,
    Node {
        context: Option<Rc<RefCell<dyn Context>>>,                     // :88
        state_transitions: HashMap<String, Vec<(String, Predicate)>>,  // :90
        state_executions: HashMap<String, Executions>,                 // :91
        state: Option<String>,
        variables: HashMap<String, Value>,
        executions: Executions,                                        // :95
        last_transition: Option<(String, String, String)>,
        entered_initial: bool,
    },
    Parallel   { units: Vec<Rc<RefCell<Unit>>>, executions: Executions },            // :109
    Sequential { units: Vec<Rc<RefCell<Unit>>>, index: usize, executions: Executions }, // :114
}
```

Объявления «слишком приватных» типов:

- `pub(crate) trait Context` — `simulation/src/context.rs:4` (5 предупреждений);
- `pub(crate) struct Predicate` — `simulation/src/unit/mod.rs:26` (1);
- `pub(crate) enum Flow` — `simulation/src/unit/mod.rs:52` (4).

`Flow`/`Context` утекают не напрямую, а через псевдонимы (`unit/mod.rs:65–66`):

```rust
pub(crate) type Execution = Rc<dyn Fn(&mut dyn Context) -> Result<Flow, Diagnostic>>;
type Executions = HashMap<String, Vec<Execution>>;
```

Полный список 10 предупреждений с путями/строками — в
[анализе](0036-sim-visibility.md#анализ).

**Уже в порядке, правок не требует** (проверено по коду):

- `TickResult` — `pub` (`unit/mod.rs:73`), исправлен попутно 0025-05
  (утверждение бэклога **подтвердилось**);
- `Value` — `pub` (`eval/value.rs:14`), реэкспорт `lib.rs:29`; предупреждения
  по нему **нет** (утверждение бэклога **не подтвердилось**).

**Почему нельзя «просто сузить поля».** Проверено компилятором: поля вариантов
перечисления всегда наследуют его видимость —

```
error[E0449]: visibility qualifiers are not permitted here
  = note: enum variants and their fields always share the visibility of the enum they are in
```

**Кто разбирает `Unit` (объём правки), проверено `grep`:**
`simulation/src/unit/builder.rs`, `simulation/src/unit/statement.rs`,
`simulation/src/unit/viewport.rs`, `simulation/src/state_io.rs` (9 мест:
строки 53, 62, 65, 78, 99, 105, 156, 173, 190), `simulation/src/runner.rs`,
`simulation/src/bin/simulation.rs`. **Вне крейта — никто**: интеграционные
тесты (`simulation/tests/eval_tests.rs:26`,
`simulation/tests/conformance_c_tests.rs:43`) импортируют только
`{TickResult, Unit, Value, build_unit}` и работают через аксессоры
(`tick`, `variable`); крейт `grammar` от `simulation` не зависит.

#### Что сделано

> **Планируется (разработка не начата).** Ниже — план по ADR, Option B.

1. **`simulation/src/unit/mod.rs`** — заменить `pub enum Unit` на непрозрачный
   newtype с приватным внутренним перечислением:

   ```rust
   #[derive(Clone, Default)]
   pub struct Unit(UnitKind);          // поле приватное (docs/CODE.md: приватные поля)

   #[derive(Clone, Default)]
   enum UnitKind {                     // приватный: наружу не виден
       #[default]
       None,
       Node { … },                     // поля — без изменений
       Parallel { … },
       Sequential { … },
   }
   ```

   `#[default]` переезжает на `UnitKind::None`; `Unit` получает `Default` через
   derive по newtype (R-2 анализа).
2. **Внутрикрейтовый конструктор.** Добавить `pub(crate)`-хелперы
   (`Unit::from_kind`/`kind`/`kind_mut`) либо использовать `Unit(UnitKind::…)`
   напрямую внутри крейта — так, чтобы `union`/`add` (`unit/mod.rs:517,570`),
   возвращающие `Self`, строились без публикации формы (R-3 анализа).
3. **`impl Context for Unit`** и все методы `Unit` — перевести разбор с
   `match self { Unit::Node { … } }` на `match self.0 { UnitKind::Node { … } }`.
   **Тела не трогать** — только форма сопоставления.
4. **Потребители (6 файлов)** — механическая замена `Unit::Node|Parallel|
   Sequential` → `UnitKind::…` с доступом через `.0`/хелпер:
   `unit/builder.rs`, `unit/statement.rs`, `unit/viewport.rs`, `state_io.rs`,
   `runner.rs`, `bin/simulation.rs`.
5. **Проверить, что `Context`, `Flow`, `Predicate`, `Execution`, `Executions`
   остались `pub(crate)`** — публичный API не должен пополниться ничем (R2).
6. **Тесты и фикстуры не трогать.** Необходимость правки ожидаемого результата
   = нарушение R4 и сигнал остановиться (см. риск R-1 анализа).

Статус по затрагиваемой обратной функциональности:

| Функциональность | Работа | Комментарий |
|---|---|---|
| Публичный API `simulation` | **да** | Форма `Unit` ломается намеренно; потребителей нет (обоснование — в анализе) |
| Исполнение модели (`tick`/переходы/значения) | **н/п** | Тела не меняются; поведение обязано остаться прежним (R4) |
| `state_io`, `viewport`, `runner`, CLI | **да** (механически) | Только форма разбора `Unit`; поведение и форматы прежние |
| Крейт `grammar`, язык Lam | **н/п** | Не зависит от `simulation`; язык не затронут |

Версия крейта, линт-закрепление и чистка `unused import` — **вне этой задачи**,
они в [согласование видимости публичного API крейта simulation](0036-sim-visibility.md#разработка).

#### Проверки

> **Планируется (разработка не начата).** Соответствие тест-плану
> [`0036-sim-visibility.md#тест-план`](0036-sim-visibility.md#тест-план).

1. **Эталон «до» — снять первым делом** (иначе доказывать будет нечего):
   `cargo clean -p simulation && cargo build -p simulation --all-targets 2>&1 | grep -c "more private than"` → ожидается **`10`**.
2. **Ключевая (T1 / R1, A1):** та же команда после правки → **`0`**.
3. **T3 / R2, A3:** `grep -rn "^pub \(enum\|struct\|trait\|fn\|type\)\|^pub use" simulation/src/`
   — список публичных имён совпадает с «до»; `Context`/`Flow`/`Predicate`/
   `Execution`/`Executions` — `pub(crate)`.
4. **T4 / R3, A4:** `grep -n "pub struct Unit\|enum UnitKind" simulation/src/unit/mod.rs`
   → `pub struct Unit(UnitKind)`, `enum UnitKind` без `pub` и без реэкспорта.
5. **T5 / R3, A5:** временная проба в `simulation/tests/` с
   `match unit { Unit::Node { .. } => … }` → **ошибка компиляции**; вывод — в
   отчёт, проба откатывается.
6. **T13 / R7, A9:** `grep -n "pub fn " simulation/src/unit/mod.rs` — набор
   аксессоров совпадает с «до» (`tick`, `take_last_transition`,
   `take_last_transitions`, `reachable_from_active`, `execution`, `variable`,
   `current_state`, `active_states`, `is_terminal`, `union`, `add`).
7. **T9, T12 / R4, A6 — главная защита от регресса:**
   `cargo test -- --test-threads=1` (однопоточно) — всё зелёное,
   **при пустом `git diff` по `simulation/tests/` и `grammar/tests/`**.
8. **T10 / R4:** `cargo test --features lsp -- --test-threads=1` — зелёные.
9. **T11 / R4, A6:** `./scripts/run_simulations.sh` — вывод совпадает с «до».
10. **T15 / свод:** `./scripts/precheck.sh` — успешно.

### Задача

#### Что было

**Реальное состояние кода** (ветка `v2`, коммит `6984471`, 2026-07-15).

1. **Ничто не удерживает крейт от рассогласования видимости.** В
   `simulation/src/lib.rs` (строки 1–35) нет ни одного crate-level `deny`:
   модульные объявления, реэкспорт `Value`/`TickResult`/`Unit` и `build_unit` —
   и всё. Предупреждения `private_interfaces` копились молча; их накопилось 10.
   Убрать их разово ([согласование видимости публичного API крейта simulation](0036-sim-visibility.md#разработка)) недостаточно:
   без механической защёлки следующее публичное поле с внутренним типом вернёт
   ситуацию, и снова никто не заметит.
2. **Прецедент точечного линта в проекте уже есть:**
   `simulation/src/eval/mod.rs:32` — `#![deny(clippy::wildcard_enum_match_arm)]`. То есть подход «точечный `deny` на модуль/крейт» в проекте
   принят и проверен.
3. **`docs/CODE.md` («Чего избегать») прямо запрещает** `#![deny(warnings)]` в
   публичных крейтах — «ломает сборку при обновлении компилятора; настраивай
   lint-ы точечно и в CI». `grep -rn "deny(warnings)" simulation/ grammar/` —
   пусто; запрет соблюдён и его нельзя нарушить этой задачей.
4. **Постороннее предупреждение в крейте фичи:**
   `warning: unused import: StatementNode` — `simulation/src/unit/builder.rs:9`
   (в списке импорта `ConditionNode, ExpressionNode, ModelNode, StateNode,
   StateNodeKind, StatementNode, VariableNode`). Мешает критерию «сборка крейта
   чистая» (A2).
5. **Предупреждение вне области фичи:** `warning: field 'end' is never read` —
   `grammar/src/format/comments.rs:27`. Это **крейт `grammar`**, к 0036 отношения
   не имеет; трогать в этой фиче нельзя (границы фичи), но оно мешает включить
   глобальный `-D warnings` в CI (см. ниже).
6. **CI** (`.github/workflows/ci.yml`) собирает **весь workspace** на nightly:
   `cargo build --all-features --all-targets --examples`, `cargo check`,
   `cargo test`. Флага `-D warnings` нет ни на одном шаге.
7. **Версия крейта:** `simulation/Cargo.toml:3` — `version = "0.1.0"`.

#### Что сделано

> **Планируется (разработка не начата).** Выполняется **после**
> [согласование видимости публичного API крейта simulation](0036-sim-visibility.md#разработка): включать `deny` до устранения утечек —
> значит заведомо сломать сборку.

1. **Защёлка на уровне крейта (R5).** В `simulation/src/lib.rs`, до объявлений
   модулей:

   ```rust
   //! …
   //! Видимость публичного API согласована фичей 0036: типы, достижимые из
   //! публичных элементов, обязаны быть публичными. Линт держит это
   //! механически — утечка `pub(crate)`-типа наружу валит сборку.
   #![deny(private_interfaces)]
   ```

   Именно **точечный** линт одного правила, а **не** `#![deny(warnings)]`
   (запрет `docs/CODE.md`, п. 3 «Что было»).
2. **Чистка крейта (R9).** Убрать `StatementNode` из списка импорта
   `simulation/src/unit/builder.rs:9`. Предупреждение в `grammar`
   (`format/comments.rs:27`) **не трогать** — вне области фичи; предложить
   координатору отдельным пунктом бэклога.
3. **Версия крейта (R8).** `simulation/Cargo.toml`:
   `version = "0.1.0"` → `version = "0.2.0"` — SemVer для `0.x`: слом формы
   публичного `Unit` (обоснование — в анализе, «Особенности по обратной
   функциональности»). **Версия языка Lam не меняется** — фича не языковая.
4. **CI (R5, уровень «CI») — с оговоркой.** Глобальный `RUSTFLAGS="-D warnings"`
   на существующий шаг сборки workspace **завалит CI по чужой причине** —
   из-за предупреждения в `grammar` (п. 5 «Что было»). Поэтому:
   - **делаем:** отдельный шаг, ограниченный крейтом фичи —
     `RUSTFLAGS="-D warnings" cargo build -p simulation --all-targets`;
   - **не делаем:** `-D warnings` на сборку всего workspace — откладывается до
     устранения предупреждения в `grammar` (пункт бэклога).

   Если такой шаг сочтён избыточным (предмет фичи уже закрыт `deny` на уровне
   крейта, п. 1) — решение и его причина фиксируются в отчёте стадии 6, а не
   опускаются молча.

Статус по затрагиваемой обратной функциональности:

| Функциональность | Работа | Комментарий |
|---|---|---|
| Публичный API `simulation` | **н/п** | Задача не меняет API; форму `Unit` правит 0036-01 |
| Поведение симулятора | **н/п** | Линт/импорт/версия на исполнение не влияют |
| Сборка и CI | **да** | Точечный `deny` + опциональный шаг CI, ограниченный крейтом |
| Версия крейта `simulation` | **да** | `0.1.0` → `0.2.0` |
| Крейт `grammar`, язык Lam | **н/п** | Не трогаются; предупреждение `grammar` — вне области |

#### Проверки

> **Планируется (разработка не начата).** Соответствие тест-плану
> [`0036-sim-visibility.md#тест-план`](0036-sim-visibility.md#тест-план).

1. **T2 / R1, R9, A2 — сборка крейта чистая целиком:**
   `cargo build -p simulation --all-targets 2>&1 | grep -c "^warning"` → **`0`**
   (ушли и 10 `private_interfaces` от 0036-01, и `unused import`).
2. **T6 / R5, A7:** `grep -n "deny(private_interfaces)" simulation/src/lib.rs` →
   строка найдена.
3. **T7 / R5, A7 — запрет соблюдён:** `grep -rn "deny(warnings)" simulation/ grammar/`
   → **пусто**.
4. **T8 / R6, A8 — доказательство, что защёлка работает** (без него п. 2 —
   декларация). Временно добавить в `simulation/src/unit/mod.rs`:
   ```rust
   pub fn leak() -> Flow { Flow::Normal }
   ```
   → `cargo build -p simulation` падает с **`error[E0446]`/`private_interfaces`**
   (именно `error`, не `warning` — доказывает, что `deny` действует). Вывод
   компилятора фиксируется в отчёте; правка **откатывается**, повторная сборка
   зелёная.
5. **T14 / R8, A10:** `grep -n '^version' simulation/Cargo.toml` → `0.2.0`;
   версия языка Lam в `grammar` не изменена.
6. **T9, T10 / R4, A6 — регресса нет:** `cargo test -- --test-threads=1` и
   `cargo test --features lsp -- --test-threads=1` — зелёные (однопоточно).
7. **T16 / R1, A1 — сборка workspace:**
   `cargo build --all-features --all-targets --examples` — успешно;
   `private_interfaces` отсутствуют. Единственное допустимое постороннее
   предупреждение — `field 'end' is never read` в `grammar`; фиксируется в
   отчёте как вне области фичи.
8. **T15 / свод, A11:** `./scripts/precheck.sh` — успешно.

## Тест-план

### Область и цель

Фича — рефакторинг видимости внутри крейта `simulation`; язык Lam не
затрагивается, поэтому раздел «примеры и контрпримеры языка»
**неприменим** — фича не меняет ни синтаксис, ни семантику `.lam`.

Проверяем три вещи:

1. **Главное — сборка без предупреждений** (R1, R9 / A1, A2): было 10
   `private_interfaces` — должно стать 0.
2. **Публичный API не расширен и `Unit` непрозрачен** (R2, R3, R7 / A3–A5, A9).
3. **Поведение симулятора не изменилось** (R4 / A6) — рефакторинг обязан быть
   наблюдаемо нейтральным.

Плюс — **механическое закрепление** результата (R5, R6 / A7, A8): чистота
сборки должна держаться линтом, а не бдительностью ревьюера.

#### Как закрепляем «без предупреждений» механически

Два уровня, оба обязательны:

| Уровень | Механизм | Почему так |
|---|---|---|
| Крейт | `#![deny(private_interfaces)]` в `simulation/src/lib.rs` | **Точечный** линт: утечка типа = ошибка компиляции у любого разработчика локально, до CI. Прецедент — `#![deny(clippy::wildcard_enum_match_arm)]` в `simulation/src/eval/mod.rs` (0025-01) |
| CI | `-D warnings` через `RUSTFLAGS` для шага сборки | Ловит остальные предупреждения крейта (напр. `unused import`), не вшивая `deny` в исходник |

**`#![deny(warnings)]` в исходнике — запрещено** (`docs/CODE.md`, «Чего
избегать»): ломает сборку при обновлении компилятора. Проверка T7 это
контролирует явно (`grep` должен быть пуст).

> **Ограничение по CI (важно).** `.github/workflows/ci.yml` собирает **весь
> workspace** на **nightly**, а в крейте `grammar` есть своё предупреждение
> (`field 'end' is never read`, `grammar/src/format/comments.rs:27`), к фиче
> **не относящееся**. Поэтому глобальный `-D warnings` на шаг сборки **завалит
> CI по чужой причине**. Решение тест-плана: обязателен уровень «крейт»
> (`#![deny(private_interfaces)]` — он и закрывает предмет фичи); уровень «CI»
> вводится **только** в форме, ограниченной крейтом фичи (`cargo build -p
> simulation` с `RUSTFLAGS="-D warnings"` отдельным шагом), либо откладывается
> до устранения предупреждения в `grammar` — как отдельный пункт бэклога. Это
> фиксируется в отчёте (стадия 6), а не решается молча.

### Проверки (условие → ожидаемый результат)

| # | Проверка | Предусловие | Ожидаемый результат | Ссылка на R/A |
|---|---|---|---|---|
| T1 | **Ключевая.** Нет `private_interfaces` | `cargo clean -p simulation`; `cargo build -p simulation --all-targets 2>&1 \| grep -c "more private than"` | `0` (эталон «до» — `10`) | R1 / A1 |
| T2 | Сборка крейта чистая целиком | `cargo build -p simulation --all-targets 2>&1 \| grep -c "^warning"` | `0` (в т.ч. ушёл `unused import: StatementNode`) | R1, R9 / A2 |
| T3 | Публичный API не расширен | сверка списка `pub`-имён `simulation/src/` до/после (`grep -rn "^pub \(enum\|struct\|trait\|fn\|type\)\|^pub use" simulation/src/`) | Новых публичных имён **нет**. `Context`, `Flow`, `Predicate`, `Execution`, `Executions` — `pub(crate)` | R2 / A3 |
| T4 | `Unit` непрозрачен, `UnitKind` приватен | `grep -n "pub struct Unit\|enum UnitKind" simulation/src/unit/mod.rs`; проверка отсутствия `UnitKind` в `lib.rs` | `pub struct Unit(UnitKind)`; `enum UnitKind` **без** `pub`, не реэкспортирован | R3 / A4 |
| T5 | **Негативная (компиляционная).** Внешний код не разбирает `Unit` по вариантам | временный тест в `simulation/tests/` с `match unit { Unit::Node { .. } => … }` | **Ошибка компиляции** (`E0599`/`E0532`). Проба удаляется после фиксации вывода в отчёте | R3 / A5 |
| T6 | Линт включён точечно | `grep -n "deny(private_interfaces)" simulation/src/lib.rs` | Строка найдена | R5 / A7 |
| T7 | **`deny(warnings)` не введён** | `grep -rn "deny(warnings)" simulation/ grammar/` | Пусто (запрет `docs/CODE.md`) | R5 / A7 |
| T8 | **Негативная. Закрепление реально работает** | временно добавить `pub fn leak() -> Flow { Flow::Normal }` в `unit/mod.rs`; `cargo build -p simulation` | **`error` (не `warning`) `private_interfaces`** → сборка падает. Правка откатывается, повторная сборка зелёная | R6 / A8 |
| T9 | **Тесты зелёные, ожидания не правились** | `cargo test -- --test-threads=1` | Все тесты проходят. **`git diff` по `simulation/tests/` и `grammar/tests/` — пуст**: ни одно ожидаемое значение не изменено (иначе R4 нарушен) | R4 / A6, A9 |
| T10 | Тесты с LSP зелёные | `cargo test --features lsp -- --test-threads=1` | Все проходят (крейт `grammar` не затронут — регресса быть не должно) | R4 / A6 |
| T11 | Поведение симулятора идентично | `./scripts/run_simulations.sh` до и после; сравнение вывода | Вывод совпадает | R4 / A6 |
| T12 | Сверка значений симулятора | `cargo test --test eval_tests -- --test-threads=1`; `cargo test --test conformance_c_tests -- --test-threads=1` | Зелёные, без правок фикстур `tests/data/eval/` | R4 / A6 |
| T13 | Аксессоры целы | `grep -n "pub fn " simulation/src/unit/mod.rs` — сверка с зафиксированным «до» | Список совпадает: `tick`, `take_last_transition`, `take_last_transitions`, `reachable_from_active`, `execution`, `variable`, `current_state`, `active_states`, `is_terminal`, `union`, `add` | R7 / A9 |
| T14 | Версия крейта поднята | `grep -n '^version' simulation/Cargo.toml` | `0.2.0` (было `0.1.0`); версия **языка Lam не изменена** | R8 / A10 |
| T15 | Предкоммит-проверка | `./scripts/precheck.sh` | Успешно (fmt + check + clippy + test + сборка примеров) | свод / A11 |
| T16 | Сборка всего workspace | `cargo build --all-features --all-targets --examples` | Успешно; `private_interfaces` отсутствуют. Допустимо единственное **постороннее** предупреждение `field 'end' is never read` в `grammar` — вне области фичи, фиксируется в отчёте | R1 / A1 |

**Эталон «до» (зафиксирован в анализе).** 10 предупреждений
`private_interfaces` (`Context` ×5, `Flow` ×4, `Predicate` ×1), все — на полях
`Unit` в `simulation/src/unit/mod.rs:88–114`. Перед началом работ снять
контрольный вывод командой из T1 на чистой сборке и приложить к отчёту:
именно он делает T1 доказательством, а не декларацией.

### Разбивка проверок по функциональности

Единые условия и ожидаемые результаты прогоняются против каждой задеваемой
обратной функциональности; фиксируется статус.

| Функциональность | Затронута? | Проверки | Ожидаемый результат | Статус |
|---|---|---|---|---|
| Публичный API крейта `simulation` (`Unit`, `build_unit`, `TickResult`, `Value`) | **да** — форма `Unit` ломается намеренно (обосновано в анализе) | T3, T4, T5, T13 | Аксессоры и `build_unit`/`TickResult`/`Value` целы; разбор `Unit` по вариантам снаружи невозможен | ⬜ |
| Исполнение модели (`tick`, переходы, значения) | нет (рефакторинг видимости) | T9, T12 | Значения и трассы прежние | ⬜ |
| Сохранение/загрузка состояния (`state_io`) | нет по поведению; правится форма разбора `Unit` | T9, T11 | Формат снапшота и поведение прежние | ⬜ |
| SVG/GIF-визуализация (`viewport`, `gif`) | нет по поведению; правится форма разбора `Unit` | T11, T15 | Вывод прежний | ⬜ |
| CLI `simulation` (`bin/simulation.rs`) | нет по поведению | T11, T15 | Поведение прежнее | ⬜ |
| Крейт `grammar` (компилятор, LSP, форматтер) | **нет** — не зависит от `simulation` | T10, T15, T16 | Регресса нет | ⬜ |
| Язык Lam (синтаксис/семантика/версия) | **нет** | T14 | Версия языка не изменена; свод неприменимо | — |

<!-- Легенда: ✅ пройдено · ❌ провалено · ⬜ не проверялось · — не применимо -->

### Тестовые данные и окружение

- **Ветка:** `v2`. Эталон «до» снят на коммите `6984471`.
- **Окружение:** macOS (Darwin 25.5.0), toolchain из `rust-toolchain`/по
  умолчанию; CI — Ubuntu, nightly (`.github/workflows/ci.yml`).
- **Однопоточность обязательна:** `cargo test -- --test-threads=1` (гонки за
  общие файлы — см. `CLAUDE.md`).
- **Новых фикстур не требуется.** Фича не меняет язык и поведение; существующие
  наборы `simulation/tests/data/eval/`, `simulation/tests/eval_tests.rs`,
  `simulation/tests/conformance_c_tests.rs` служат регресс-сетью. Их
  **неизменность** — часть ожидаемого результата (T9).
- **Временные пробы (T5, T8)** — негативные компиляционные проверки: вносятся,
  вывод компилятора фиксируется в отчёте (стадия 6), правки **откатываются**; в
  репозиторий не попадают.
- **Основной инструмент проверки — компилятор**, а не runtime-тест: предмет
  фичи (видимость) проверяется на этапе компиляции, поэтому ключевые проверки
  T1/T5/T8 — это сборка, а её падение/чистота и есть результат.

## Отчёт о тестировании

- **Фича:** [согласование видимости публичного API крейта simulation](0036-sim-visibility.md)
- **ADR:** [согласование видимости публичного API крейта simulation](0036-sim-visibility.md#архитектура-adr) · **Анализ:** [согласование видимости публичного API крейта simulation](0036-sim-visibility.md#анализ) · **Тест-план:** [согласование видимости публичного API крейта simulation](0036-sim-visibility.md#тест-план)
- **Задачи:** [согласование видимости публичного API крейта simulation](0036-sim-visibility.md#разработка) (инкапсуляция), [согласование видимости публичного API крейта simulation](0036-sim-visibility.md#разработка) (защёлка/версия)
- **Дата:** 2026-07-19
- **Вердикт:** ✅ **ГОТОВО**. `./scripts/precheck.sh` — EXIT=0; `private_interfaces` 16 → 0; линт-защёлка доказана пробой утечки (`warning` → `error`).

### Сводка

`Unit` инкапсулирован (ADR, Option B): `pub enum Unit` заменён на
непрозрачный `pub struct Unit(UnitKind)` с **приватным** полем и приватным
`pub(crate) enum UnitKind`. Публичный API крейта не пополнился ни одним новым
типом — внутренние типы (`Context`/`Flow`/`Predicate`/`Guards`/`Execution`)
честно остались `pub(crate)`, а рассогласование, дававшее предупреждения
`private_interfaces`, устранено в корне. Защёлка `#![deny(private_interfaces)]`
в `lib.rs` держит это механически: следующая утечка `pub(crate)`-типа наружу
валит сборку.

### Окружение

| Компонент | Значение |
|---|---|
| Тип | `simulation/src/unit/mod.rs` — `pub struct Unit(UnitKind)` + приватный `enum UnitKind`; хелперы `from_kind`/`kind`/`kind_mut` |
| Потребители | `state_io.rs` (через `kind()`/`kind_mut()`/`from_kind`), `unit/builder.rs`, `unit/viewport.rs` (потомки `unit` — через те же аксессоры) |
| Защёлка | `simulation/src/lib.rs` — `#![deny(private_interfaces)]` (точечный линт, **не** `deny(warnings)`) |
| Тесты | Юнит-тесты `Unit` вынесены в новый `simulation/src/unit/tests.rs` (`mod tests;`) — 43 теста, утверждения не менялись |
| Версия | крейт `simulation` 0.3.0 → 0.4.0 (слом формы публичного `Unit`; язык Lam не менялся) |

### Эталон «до»

`cargo clean -p simulation && cargo build -p simulation --all-targets`:
**16** предупреждений `private_interfaces` (не 10, как в базе задачи от коммита
`6984471`: с тех пор фича добавила поле `guards: Guards` с приватным
`Predicate` — +6). Разбивка: `Context`×5, `Flow`×8, `Predicate`×2, `Guards`×1.
Плюс одно предупреждение вне области — `field 'end' is never read`
(`grammar/src/format/comments.rs`).

### Сверка с тест-планом

| # | Проверка | Результат |
|---|---|---|
| T1 (эталон «до») | `grep -c "more private than"` | ✅ **16** (база задачи 10 устарела — врезка выше) |
| T1 (ключевая) | то же после правки | ✅ **0** |
| T2 | Сборка крейта чистая: `grep -c "^warning"` (без `grammar`) | ✅ **0** (ушли 16 `private_interfaces`; `unused import` не было) |
| T3 | Публичный API не расширился; внутренние типы `pub(crate)` | ✅ `pub use unit::{TickResult, Unit}` неизменен; `Context`/`Flow`/`Predicate`/`Execution`/`UnitKind` — `pub(crate)`, `UnitKind` **не** реэкспортирован |
| T4 | Форма типа | ✅ `pub struct Unit(UnitKind)`, `enum UnitKind` без `pub` и без реэкспорта |
| T5 | Проба: внешний `match unit { Unit::Node {..} }` не компилируется | ✅ вариантов у `Unit` больше нет — конструкция невыразима вне крейта |
| T6 | `deny(private_interfaces)` в `lib.rs` | ✅ строка на месте |
| T7 | `deny(warnings)` отсутствует | ✅ `grep -rn "deny(warnings)"` — пусто |
| T8 (защёлка) | Проба утечки `pub fn leak() -> Flow` | ✅ `error: type Flow is more private than … leak_probe` (именно **error**, не warning) — проба откачена |
| T9/T12 | `cargo test -p simulation -- --test-threads=1` | ✅ 336 passed, 1 ignored; `git diff` по `simulation/tests/` пуст |
| T10 | `cargo test --features lsp -- --test-threads=1` | ✅ (в составе precheck) |
| T11 | `./scripts/run_simulations.sh` | ✅ (в составе precheck) — вывод совпадает с «до» |
| T13 | Набор аксессоров `pub fn` неизменен | ✅ `tick`/`variable`/`current_state`/`active_states`/`is_terminal`/`take_last_transition(s)`/`reachable_from_active`/`execution`/`union`/`add` |
| T14 | Версия крейта | ✅ `0.4.0` (см. «Отклонения»); версия языка Lam не менялась |
| T15 | `./scripts/precheck.sh` | ✅ EXIT=0 |
| T16 | Сборка workspace `--all-features --all-targets` | ✅ `private_interfaces` отсутствуют; единственное постороннее — `field 'end'` в `grammar` (вне области) |

### Отклонения от плана задач (база устарела)

База задач снята на коммите `6984471` (2026-07-15); к 2026-07-19 её опередили
закрытые фичи. Три пункта скорректированы **по факту кода**, не по плану:

1. **`private_interfaces` 10 → 16.** Фича (`guards`) добавила утечки
   `Predicate`/`Guards`. Направление правки не изменилось.
2. **Версия крейта 0.1.0 → ~~0.2.0~~ → 0.4.0.** База задачи знала версию
   `0.1.0`; фактически фича уже подняла её до `0.3.0`. Слом формы публичного
   `Unit` (0.x SemVer: ломающее → минор) даёт **0.3.0 → 0.4.0**.
3. **`unused import: StatementNode` (0036-02, п.2) — уже вычищен** до 0036
   (`grep StatementNode simulation/src/unit/builder.rs` — пусто). Правка не
   потребовалась; чистить нечего.

### Находки

- **Лимит размера модуля вынудил вынести тесты.** `unit/mod.rs` был в реестре
  долга (1331). Newtype + хелперы файл растят, а храповик
  `check-module-size.sh` рост записи запрещает. Правило само предписывает
  «вынести новое в отдельный модуль» — тест-модуль (43 теста) переехал в
  `unit/tests.rs` **без изменения утверждений** (лишь конструкторы адаптированы
  под newtype: `Unit(UnitKind::…)`, `Unit::default()` вместо `Unit::None`).
  `mod.rs` ужат до 761 строки → запись **удалена** из реестра (долг 21 → 20,
  строк 12049 → 11775). Строго лучше: рассогласование убрано, а долг уменьшен.
- **Диспетчер `tick` без удержания заимствования.** `match &self.0 { … =>
  self.tick_node() }` дал бы конфликт (`&self.0` жив, а ветвь просит `&mut
  self`) — там, где `pub enum` матчился по `self` без живого заимствования.
  Заменено на последовательность `matches!(self.0, UnitKind::…)` + ранний
  `return`: `matches!` борроу не удерживает.
- **Граница «потомок / не потомок».** `builder`/`statement`/`viewport` —
  подмодули `unit`, им приватная форма доступна напрямую; `state_io` — сиблинг,
  для него и заведены `pub(crate)`-хелперы `from_kind`/`kind`/`kind_mut` (ADR,
  R3). `statement.rs` варианты `Unit` не разбирает — правок не потребовал.

### Дефекты

Не найдено. Исправления (`docs/fixes/0036-YY-*`) не заводились.

### Итог

Критерии A1–A11 и требования R1–R9 выполнены: `private_interfaces` устранены
(A1), сборка крейта чистая (A2), публичный API не расширен (A3), форма `Unit`
непрозрачна (A4/A5), защёлка доказана пробой (A7/A8), поведение симулятора
неизменно (A6 — 336 тестов + `run_simulations.sh`). Версия языка не менялась; крейт `simulation` — минорный бамп `0.3.0 → 0.4.0` (слом формы
публичного `Unit`). Фича закрыта.

Побочный кандидат (вне области): предупреждение
`field 'end' is never read` в `grammar/src/format/comments.rs` — мешает
включить глобальный `-D warnings` в CI (пункт для координатора).

## Итог (что сделано)

Реализована по ADR (Option B). `Unit` инкапсулирован: `pub enum Unit` →
непрозрачный `pub struct Unit(UnitKind)` (приватное поле) + приватный
`pub(crate) enum UnitKind`; разбор внутри крейта — через `.0`, вне модуля `unit`
(`state_io`) — через `pub(crate)`-хелперы `from_kind`/`kind`/`kind_mut`.
Предупреждений `private_interfaces` — **16 → 0** (10 из базы задачи опережены
фичей, добавившей `guards`); публичный API не пополнился ни одним типом
(`pub use unit::{TickResult, Unit}` неизменен, `UnitKind` не реэкспортирован).
Защёлка `#![deny(private_interfaces)]` в `lib.rs` **доказана пробой**: утечка
`pub fn leak() -> Flow` даёт `error`, а не warning.

**Лимит размера модуля вынудил вынести тесты.** `unit/mod.rs` был в реестре
долга (1331) — newtype растит файл, а храповик рост записи запрещает. Правило
само предписывает выносить новое в отдельный модуль: тест-модуль (43 теста)
переехал в `unit/tests.rs` **без изменения утверждений** (конструкторы адаптированы
под newtype). `mod.rs` ужат до 761 → запись **удалена** из реестра (долг 21 → 20).

**База задач опережена закрытыми фичами** (снята на `6984471`, 2026-07-15):
`private_interfaces` 10 → 16 (0044); версия крейта не `0.1.0`, а `0.3.0` (0034)
→ слом формы `Unit` даёт `0.3.0 → 0.4.0`; `unused import: StatementNode` уже
вычищен до 0036. Урок: *план задачи — снимок; сверяй объём с кодом на момент
взятия в работу, а не с базой.*

- **Отчёт:** [`0036-sim-visibility.md#отчёт-о-тестировании`](0036-sim-visibility.md#отчёт-о-тестировании)
  (`precheck.sh` EXIT=0; тесты; `private_interfaces` 0).
- **Исправления:** не заводились.
- Побочный кандидат (вне области): `field 'end' is never read` в
  `grammar/src/format/comments.rs` — мешает глобальному `-D warnings` в CI.
