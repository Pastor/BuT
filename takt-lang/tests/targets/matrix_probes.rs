//! Генератор матрицы проб: вид обращения к корню x форма реализации состояния.
//!
//! Носитель **общий** у двух тестов (правило "одно правило - один носитель"):
//!
//! - [`root_pointer_matrix_tests`](super::root_pointer_matrix_tests) - точность
//!   признака "нужен ли указатель на корень" у цели `c`;
//! - [`target_matrix_tests`](super::target_matrix_tests) - та же матрица через
//!   **все восемь** целей и их инструменты.
//!
//! Разъехавшись, два генератора дали бы двум тестам разные входы, и таблица ожиданий
//! одного перестала бы говорить о другом.
//!
//! Ось "форма объявления" (`Kind`) живёт рядом - в `matrix_kind`: там знание о том, как
//! выглядят объявление, чтение и запись каждой формы, здесь - сборка исходника пробы.
//!
//! Порты объявляются **с адресом**: он нужен целям `c-hal` и `st-at` (иначе `SE-052`),
//! а прочим безразличен - проверено прогоном (сигнатуры цели `c` от адреса не
//! меняются).

pub(crate) use super::matrix_kind::{KINDS, Kind};

/// Вид обращения к корню - то, ради чего указатель и печатается.
#[derive(Clone, Copy, PartialEq)]
pub(crate) enum Touch {
    /// Обращений нет вовсе.
    None,
    /// Запись выходного порта: она идёт через HAL корня.
    PortWrite,
    /// Чтение переменной, объявленной в корне.
    SharedRead,
    /// Выходной порт с начальным значением: запись печатается в `_init`.
    PortInit,
    /// Инициализатор переменной читает объявление корня.
    VarInit,
    /// Порт пишет функция модели - признак обязан быть транзитивным.
    Transitive,
    /// Профиль "часы" и выдержка `after Nms`: метка сравнивается с `main->now_ms(...)`
    /// и в такте, и при входе.
    ClockAfter,
    /// Вызов `extern fn`: печатается свободной функцией - контроль, что признак не
    /// срабатывает "на всякий случай".
    ExternCall,
    /// Чтение входного порта: значение приходит извне через HAL корня.
    PortRead,
    /// Чтение двунаправленного порта: у цели `sv` это отдельный сигнал `_i`.
    InoutRead,
    /// Запись двунаправленного порта: сигналы `_o` и строб `_we`.
    InoutWrite,
    /// Инвариант уровня модели: `invariant Имя = условие;` - сахар над `cond` плюс
    /// охранная формула.
    InvariantModel,
    /// Инвариант в теле состояния: то же обязательство, другое место объявления (одно
    /// из шести).
    InvariantState,
    /// Охранная формула краткой формой: `: условие;`.
    GuardFormula,
    /// Темпоральное свойство `: [LTL] φ;` - до целей оно не доезжает вовсе (предмет
    /// верификации), и это тоже часть таблицы.
    LtlFormula,
    /// Вызов функции из подключённого файла (`import "...";`).
    ImportFunction,
    /// То же выборочным импортом: `import { twice } from "...";`.
    ImportSelective,
    /// Тип (структура) из подключённого файла.
    ImportType,
    /// Модель подключённого файла реализует состояние обёртки - выборочным импортом,
    /// потому что полный вносит только контейнер файла.
    ImportModel,
    /// Полный импорт + попытка взять вложенную модель: законный отказ (`SE-106`), и он
    /// часть таблицы.
    ImportNestedModel,
    /// Транзитивный импорт: подключённый файл сам подключает третий.
    ImportTransitive,
    /// Охранная формула в теле блока `always`.
    GuardInBlock,
    /// Охранная формула в теле функции.
    ///
    /// Условие читает параметр, а не переменную модели: у цели `rust` функция
    /// порождается свободной (`RS-017`), и обращение к состоянию было бы границей цели,
    /// а не проверкой формулы.
    GuardInFunction,
    /// Охранная формула во вложенном блоке (`if` внутри `always`).
    GuardInNested,
    /// Темпоральная формула в теле блока: до целей не доезжает по существу.
    LtlInBlock,
    /// Темпоральная формула в теле `always` уровня модели.
    LtlInModelBlock,
    /// Темпоральная формула в теле функции.
    LtlInFunction,
    /// Темпоральная формула объявлением уровня состояния.
    ///
    /// Её область - прогоны из этого состояния (решение 0049): верификатор десахаризует
    /// её в `G (Состояние -> φ)`, и это наблюдаемое отличие от формулы уровня модели.
    LtlInState,
    /// Темпоральная формула во вложенном блоке (`if` внутри `always`).
    LtlInNested,
    /// Инвариант состояния при двух состояниях.
    ///
    /// Проверяет область: обязательство, объявленное в состоянии, говорит о прогонах
    /// этого состояния (0044/0051), и проверка обязана стоять в его ветви. Стой она в
    /// общем теле - второе состояние роняло бы её, а модель корректна.
    InvariantScoped,
    /// Инвариант с именем, доезжающим до сообщения цели `rust`.
    InvariantNamed,
    /// Именованное условие в условии ребра: `ref Go: Low;`.
    CondOnEdge,
    /// Именованное условие в теле: `if Low { ... }`.
    ///
    /// До 0331 этот вход давал пять ответов, а у цели `c` - ссылку на неопределённый
    /// идентификатор при нулевом коде возврата: печатник тела имел своё представление
    /// об условии.
    CondInBody,
    /// Именованное условие в охранной формуле: `: [Guard] Low;`.
    CondInGuard,
    /// Условие через условие: `cond A = B;` - раскрытие обязано быть транзитивным.
    CondNested,
    /// Цикл `for` со статическими границами.
    ///
    /// Единственная форма, которую цель `sv` разворачивает в схему: прочие она
    /// отвергает `SV-002`, и это граница цели, а не пробел.
    LoopForStatic,
    /// Цикл `for` без инициализатора: `for ; i < 3; i := i + 1`.
    LoopForNoInit,
    /// Цикл `while` (синоним `loop` с условием - 0024).
    LoopWhile,
    /// Бесконечный `loop` с выходом по `break`.
    LoopBreak,
    /// Вложенные циклы `for`.
    LoopNested,
    /// `match` с ветвью `_`.
    MatchWildcard,
    /// `match` без ветви `_`.
    ///
    /// У цели `sv` `default` печатается всегда: без него синтезатор выводит защёлку
    /// молча. Форма и проверяет, что ветвь появляется сама.
    MatchNoWildcard,
    /// Несколько образцов в одной ветви: `0, 1 => ...`.
    MatchMultiPattern,
    /// Образцы - варианты перечисления.
    ///
    /// У цели `rust` `match` печатается цепочкой сравнений, а не `match` языка: образец
    /// Takt - произвольное выражение, и `match s { p => ... }` связал бы `p` как новое
    /// имя вместо сравнения с ним.
    MatchEnum,
    /// Вложенный `match`.
    MatchNested,
    /// `continue` внутри цикла.
    ///
    /// У цели `st` это `ST-011`: в IEC есть `EXIT` (аналог `break`), а продолжения
    /// итерации нет вовсе - граница языка ПЛК, а не недоделка.
    LoopContinue,
    /// Модель подключённого файла носит имя файла: полный импорт вносит контейнер под
    /// тем же именем, и путь до состояния получает два одинаковых сегмента подряд.
    ImportNameClash,
    /// Модель с параметром, взятая с настройкой по умолчанию.
    ParameterDefault,
    /// То же с аргументом в месте инстанцирования: `M(portion := 7)`.
    ParameterArgument,
    /// Аргумент - константное выражение: `M(portion := BASE + 2)`.
    ParameterExpression,
    /// Адрес порта задан оператором `address`, а не inline.
    AddressOperator,
    /// Оператор `address` с позицией бита: `0x...:3`.
    AddressBit,
    /// Адрес - константное выражение (арифметика вычисляется компилятором).
    AddressExpression,
    /// Адрес приходит внешней картой (`--address-map`) и перекрывает inline - приоритет
    /// источников: inline < `address` < карта.
    AddressMap,
    /// Адрес опирается на `-D` определение среды вычислителя.
    AddressDefine,
    /// Тактовая выдержка `after Nt` - счёт тактов, а не миллисекунд.
    TimeAfterTicks,
    /// Периодический блок `every Nms`.
    TimeEvery,
    /// Выдержка от переменной типа `duration` - вычисляемая.
    TimeDurationVar,
    /// Вычисляемая выдержка выражением: `after (SETTLE + 500ms)`.
    TimeComputed,
    /// Модель объявляет такт: `clock 1kHz;` - флаг обязан совпасть.
    TimeClockDeclared,
    /// Частичное чтение входного порта: у составного значения читается одна часть.
    ///
    /// Модель вправе так делать, а структурный порт цель `sv` печатает одним сигналом -
    /// непрочитанные биты `verilator` под `-Wall` считает ошибкой. Вид заведён именно
    /// ради этого случая.
    PortReadPartial,
    /// Вызов функции из условия ребра: функция состояния не касается, и указателя на
    /// него получать не должна - ни в сигнатуре, ни в вызове.
    ///
    /// Вид заведён: печатников вызова у цели `c` два, и второй передавал указатель
    /// безусловно - `cc` отвечал "too many arguments" при нулевом коде возврата.
    /// Перебор этого места не знал.
    FnCallOnEdge,
}

/// Все виды обращения - перебор идёт по ним целиком.
pub(crate) const TOUCHES: [Touch; 62] = [
    Touch::None,
    Touch::PortWrite,
    Touch::SharedRead,
    Touch::PortInit,
    Touch::VarInit,
    Touch::Transitive,
    Touch::ClockAfter,
    Touch::ExternCall,
    Touch::PortRead,
    Touch::InoutRead,
    Touch::InoutWrite,
    Touch::PortReadPartial,
    Touch::FnCallOnEdge,
    Touch::InvariantModel,
    Touch::InvariantState,
    Touch::GuardFormula,
    Touch::LtlFormula,
    Touch::GuardInBlock,
    Touch::GuardInFunction,
    Touch::GuardInNested,
    Touch::LtlInBlock,
    Touch::LtlInModelBlock,
    Touch::LtlInFunction,
    Touch::LtlInState,
    Touch::LtlInNested,
    Touch::InvariantScoped,
    Touch::InvariantNamed,
    Touch::CondOnEdge,
    Touch::CondInBody,
    Touch::CondInGuard,
    Touch::CondNested,
    Touch::LoopForStatic,
    Touch::LoopForNoInit,
    Touch::LoopWhile,
    Touch::LoopBreak,
    Touch::LoopNested,
    Touch::LoopContinue,
    Touch::MatchWildcard,
    Touch::MatchNoWildcard,
    Touch::MatchMultiPattern,
    Touch::MatchEnum,
    Touch::MatchNested,
    Touch::ImportFunction,
    Touch::ImportSelective,
    Touch::ImportType,
    Touch::ImportModel,
    Touch::ImportNestedModel,
    Touch::ImportTransitive,
    Touch::ImportNameClash,
    Touch::ParameterDefault,
    Touch::ParameterArgument,
    Touch::ParameterExpression,
    Touch::AddressOperator,
    Touch::AddressBit,
    Touch::AddressExpression,
    Touch::AddressMap,
    Touch::AddressDefine,
    Touch::TimeAfterTicks,
    Touch::TimeEvery,
    Touch::TimeDurationVar,
    Touch::TimeComputed,
    Touch::TimeClockDeclared,
];

impl Touch {
    pub(crate) fn name(self) -> &'static str {
        match self {
            Touch::None => "none",
            Touch::PortWrite => "port_write",
            Touch::SharedRead => "shared_read",
            Touch::PortInit => "port_init",
            Touch::VarInit => "var_init",
            Touch::Transitive => "transitive",
            Touch::ClockAfter => "clock_after",
            Touch::ExternCall => "extern_call",
            Touch::PortRead => "port_read",
            Touch::InoutRead => "inout_read",
            Touch::InoutWrite => "inout_write",
            Touch::PortReadPartial => "port_read_partial",
            Touch::FnCallOnEdge => "fn_call_on_edge",
            Touch::InvariantModel => "invariant_model",
            Touch::InvariantState => "invariant_state",
            Touch::GuardFormula => "guard_formula",
            Touch::LtlFormula => "ltl_formula",
            Touch::GuardInBlock => "guard_in_block",
            Touch::GuardInFunction => "guard_in_function",
            Touch::GuardInNested => "guard_in_nested",
            Touch::LtlInBlock => "ltl_in_block",
            Touch::LtlInModelBlock => "ltl_in_model_block",
            Touch::LtlInFunction => "ltl_in_function",
            Touch::LtlInState => "ltl_in_state",
            Touch::LtlInNested => "ltl_in_nested",
            Touch::InvariantScoped => "invariant_scoped",
            Touch::InvariantNamed => "invariant_named",
            Touch::CondOnEdge => "cond_on_edge",
            Touch::CondInBody => "cond_in_body",
            Touch::CondInGuard => "cond_in_guard",
            Touch::CondNested => "cond_nested",
            Touch::LoopForStatic => "loop_for_static",
            Touch::LoopForNoInit => "loop_for_no_init",
            Touch::LoopWhile => "loop_while",
            Touch::LoopBreak => "loop_break",
            Touch::LoopNested => "loop_nested",
            Touch::LoopContinue => "loop_continue",
            Touch::MatchWildcard => "match_wildcard",
            Touch::MatchNoWildcard => "match_no_wildcard",
            Touch::MatchMultiPattern => "match_multi_pattern",
            Touch::MatchEnum => "match_enum",
            Touch::MatchNested => "match_nested",
            Touch::ImportFunction => "import_function",
            Touch::ImportSelective => "import_selective",
            Touch::ImportType => "import_type",
            Touch::ImportModel => "import_model",
            Touch::ImportNestedModel => "import_nested_model",
            Touch::ImportTransitive => "import_transitive",
            Touch::ImportNameClash => "import_name_clash",
            Touch::ParameterDefault => "parameter_default",
            Touch::ParameterArgument => "parameter_argument",
            Touch::ParameterExpression => "parameter_expression",
            Touch::AddressOperator => "address_operator",
            Touch::AddressBit => "address_bit",
            Touch::AddressExpression => "address_expression",
            Touch::AddressMap => "address_map",
            Touch::AddressDefine => "address_define",
            Touch::TimeAfterTicks => "time_after_ticks",
            Touch::TimeEvery => "time_every",
            Touch::TimeDurationVar => "time_duration_var",
            Touch::TimeComputed => "time_computed",
            Touch::TimeClockDeclared => "time_clock_declared",
        }
    }

    /// Значима ли для этого вида форма объявления.
    ///
    /// У обращений, которые не касаются объявленного значения (`none`, `clock_after`,
    /// `extern_call`), тип перебирать нечего - они идут однажды, со скаляром.
    pub(crate) fn varies_by_kind(self) -> bool {
        matches!(
            self,
            Touch::PortWrite
                | Touch::SharedRead
                | Touch::PortInit
                | Touch::VarInit
                | Touch::PortRead
                | Touch::InoutRead
                | Touch::InoutWrite
                | Touch::PortReadPartial
        )
    }

    /// Объявления файла (корня), нужные этому виду.
    fn root_declarations(self, kind: Kind) -> String {
        match self {
            Touch::SharedRead | Touch::VarInit => format!(
                "var shared: {} := {};\n\n",
                kind.type_name(),
                kind.root_literal()
            ),
            _ => String::new(),
        }
    }

    /// Объявления модели, делающей обращение.
    fn declarations(self, kind: Kind) -> String {
        match self {
            Touch::PortWrite | Touch::Transitive => {
                format!("    out a: {} at 0x40000100;\n", kind.type_name())
            }
            Touch::PortInit => format!(
                "    out a: {} at 0x40000100 := {};\n",
                kind.type_name(),
                kind.literal()
            ),
            Touch::VarInit => format!("    var seed: {} := shared;\n", kind.type_name()),
            Touch::PortRead | Touch::PortReadPartial => {
                format!("    in a: {} at 0x40000100;\n", kind.type_name())
            }
            // Адрес задаётся отдельно (оператором либо картой), поэтому у объявления
            // его нет. Оператор `address` действует в области своего объявления - он
            // стоит рядом с портом (замер 0458).
            Touch::AddressOperator => "    out a: u8;\n    address a = 0x40000200;\n".to_string(),
            Touch::AddressBit => "    out a: bit;\n    address a = 0x40000004:3;\n".to_string(),
            Touch::AddressExpression => {
                "    out a: u8;\n    address a = 0x40000000 + 8;\n".to_string()
            }
            // Карта и определение приходят снаружи: объявление обычное.
            Touch::AddressMap => "    out a: u8 at 0x40000100;\n".to_string(),
            Touch::AddressDefine => "    out a: u8;\n    address a = BASE + 4;\n".to_string(),
            // Время: выдержка от переменной и от константного выражения - две формы
            // вычисляемой выдержки.
            Touch::TimeDurationVar => "    var hold: duration := 5ms;\n".to_string(),
            Touch::TimeComputed => "    const SETTLE: duration := 5ms;\n".to_string(),
            // Модель объявляет такт устройства: флаг `--tick-hz` обязан совпасть
            // (контракт 0134, `SE-069`/`SE-070`).
            Touch::TimeClockDeclared => "    clock 1kHz;\n".to_string(),
            Touch::InoutRead | Touch::InoutWrite => {
                format!("    inout a: {} at 0x40000100;\n", kind.type_name())
            }
            Touch::ExternCall => "    extern fn probe_value() -> u8;\n".to_string(),
            // Обязательства уровня модели: инвариант - сахар над `cond` плюс охранная
            // формула, краткая форма - та же формула без имени.
            Touch::InvariantModel => "    invariant Bound = k < 200;\n".to_string(),
            Touch::GuardFormula => "    : k < 200;\n".to_string(),
            // Темпоральное свойство опирается на именованное условие: атом формулы
            // обязан иметь имя.
            Touch::LtlFormula => "    cond Low = k < 200;\n    : [LTL] G Low;\n".to_string(),
            // Атом темпоральной формулы обязан иметь имя - и тогда, когда сама формула
            // стоит в теле.
            Touch::LtlInBlock
            | Touch::LtlInFunction
            | Touch::LtlInState
            | Touch::LtlInNested
            | Touch::CondOnEdge
            | Touch::CondInBody
            | Touch::CondInGuard => "    cond Low = k < 200;\n".to_string(),
            // Циклам объявлений уровня модели не требуется: счётчик они объявляют в
            // заголовке (кроме формы без инициализатора выше).
            Touch::LoopForStatic
            | Touch::LoopWhile
            | Touch::LoopBreak
            | Touch::LoopNested
            | Touch::LoopContinue
            | Touch::MatchWildcard
            | Touch::MatchNoWildcard
            | Touch::MatchMultiPattern => String::new(),
            // Образцам-вариантам нужен свой тип: `Kind::Enum` задаёт форму значения, а
            // здесь перечисление - предмет разбора.
            Touch::MatchEnum | Touch::MatchNested => {
                "    enum Phase {\n        Low,\n        High\n    }\n    var phase: Phase := Low;\n".to_string()
            }
            // Условие через условие: раскрытие обязано быть транзитивным.
            Touch::CondNested => {
                "    cond Low = k < 200;\n    cond Nested = Low;\n".to_string()
            }
            // Циклу без инициализатора счётчик нужен переменной модели: своего
            // объявления у такого заголовка нет.
            Touch::LoopForNoInit => "    var i: u8 := 0;\n".to_string(),
            // Блок `always` уровня модели - ещё одно из шести мест: он исполняется
            // каждый такт до диспетчеризации состояния.
            Touch::LtlInModelBlock => {
                "    cond Low = k < 200;\n    always {\n        : [LTL] G Low;\n    }\n".to_string()
            }
            // Тип из подключённого файла - объявление обёртки.
            Touch::ImportType => "    var p: Pair := {1, 2};\n".to_string(),
            Touch::None
            | Touch::SharedRead
            | Touch::ClockAfter
            // Функция объявлена рядом (`functions`), а объявлений уровня модели виду не
            // нужно.
            | Touch::FnCallOnEdge
            | Touch::InvariantState
            | Touch::ImportFunction
            | Touch::ImportSelective
            | Touch::ImportModel
            | Touch::ImportNestedModel
            | Touch::ImportTransitive
            | Touch::ImportNameClash
            // Формула-оператор объявлений не требует: она сама и есть содержимое тела
            // (кроме темпоральной - ей нужен именованный атом).
            | Touch::GuardInBlock
            | Touch::GuardInFunction
            | Touch::GuardInNested
            // Инвариант объявляется В состоянии - объявлений уровня модели ему не
            // требуется.
            | Touch::InvariantScoped
            | Touch::InvariantNamed
            // Виды с параметром строят свой исходник целиком (см.
            | Touch::ParameterDefault
            | Touch::ParameterArgument
            | Touch::ParameterExpression
            // Тактовая выдержка и периодический блок объявлений не требуют.
            | Touch::TimeAfterTicks
            | Touch::TimeEvery => String::new(),
        }
    }

    /// Функции модели, делающей обращение.
    fn functions(self, kind: Kind) -> String {
        match self {
            // Формула в теле функции: условие читает параметр (см. врезку у вида) -
            // обращение к состоянию модели у цели `rust` невозможно.
            Touch::GuardInFunction => {
                "    fn bump(v: u8) -> u8 {\n        : [Guard] v < 200;\n        return v + 1;\n    }\n"
                    .to_string()
            }
            Touch::LtlInFunction => {
                "    fn bump(v: u8) -> u8 {\n        : [LTL] G Low;\n        return v + 1;\n    }\n"
                    .to_string()
            }
            // Чистая функция: состояния не касается - по ней и судится признак нужды в
            // указателе.
            Touch::FnCallOnEdge => {
                "    fn twice(v: u8) -> u8 {\n        return v + v;\n    }\n".to_string()
            }
            Touch::Transitive => format!(
                "    fn bump(v: u8) -> u8 {{\n{}        return v + 1;\n    }}\n",
                kind.write_statement("a")
                    .replace("            ", "        ")
                    .replace("k;", "v;")
            ),
            _ => String::new(),
        }
    }

    /// Тело блока `always`.
    fn body(self, kind: Kind) -> String {
        match self {
            Touch::PortWrite => format!("            k := k + 1;\n{}", kind.write_statement("a")),
            Touch::SharedRead => kind.read_statement("shared"),
            Touch::VarInit => kind.read_statement("seed"),
            Touch::Transitive => "            k := bump(k);\n".to_string(),
            Touch::PortRead | Touch::InoutRead => kind.read_statement("a"),
            // Читается одна часть значения: у скаляра и перечисления это то же самое,
            // что полное чтение, а у массива и структуры - нет.
            Touch::PortReadPartial => kind.partial_read_statement("a"),
            Touch::ImportFunction | Touch::ImportSelective => {
                "            k := twice(k) + 1;\n".to_string()
            }
            Touch::ImportType => "            k := k + p.hi;\n".to_string(),
            Touch::ImportTransitive => "            k := mid_value();\n".to_string(),
            // Однобитному порту пишется бит, прочим - счётчик.
            Touch::AddressBit => "            k := k + 1;\n            a := 1;\n".to_string(),
            Touch::AddressOperator
            | Touch::AddressExpression
            | Touch::AddressMap
            | Touch::AddressDefine => "            k := k + 1;\n            a := k;\n".to_string(),
            Touch::InoutWrite => format!("            k := k + 1;\n{}", kind.write_statement("a")),
            Touch::ExternCall => "            k := probe_value();\n".to_string(),
            // Формула-Оператор: три места, различающиеся вместилищем.
            Touch::GuardInBlock => {
                "            : [Guard] k < 200;\n            k := k + 1;\n".to_string()
            }
            Touch::GuardInFunction => "            k := bump(k);\n".to_string(),
            Touch::GuardInNested => {
                "            k := k + 1;\n            if k > 0 {\n                : [Guard] k < 200;\n            }\n".to_string()
            }
            Touch::LtlInBlock => {
                "            : [LTL] G Low;\n            k := k + 1;\n".to_string()
            }
            Touch::LtlInFunction => "            k := bump(k);\n".to_string(),
            Touch::LtlInNested => {
                "            k := k + 1;\n            if k > 0 {\n                : [LTL] G Low;\n            }\n".to_string()
            }
            // Именованное условие в теле и в охранной формуле.
            Touch::CondInBody => {
                "            if Low {\n                k := k + 1;\n            }\n".to_string()
            }
            Touch::CondInGuard => {
                "            : [Guard] Low;\n            k := k + 1;\n".to_string()
            }
            // Шесть форм цикла. Все дают одно и то же наблюдаемое приращение - так
            // расхождение целей видно по значению, а не по форме вывода.
            Touch::LoopForStatic => {
                "            for var n: u8 := 0; n < 3; n := n + 1 {\n                k := k + 1;\n            }\n".to_string()
            }
            Touch::LoopForNoInit => {
                "            for ; i < 3; i := i + 1 {\n                k := k + 1;\n            }\n".to_string()
            }
            Touch::LoopWhile => {
                "            while k < 3 {\n                k := k + 1;\n            }\n".to_string()
            }
            Touch::LoopBreak => {
                "            loop {\n                k := k + 1;\n                if k > 2 {\n                    break;\n                }\n            }\n".to_string()
            }
            Touch::LoopNested => {
                "            for var n: u8 := 0; n < 2; n := n + 1 {\n                for var m: u8 := 0; m < 2; m := m + 1 {\n                    k := k + 1;\n                }\n            }\n".to_string()
            }
            Touch::LoopContinue => {
                "            for var n: u8 := 0; n < 4; n := n + 1 {\n                if n = 1 {\n                    continue;\n                }\n                k := k + 1;\n            }\n".to_string()
            }
            // Пять форм `match`.
            Touch::MatchWildcard => {
                "            match k {\n                0 => { k := k + 1; }\n                _ => { k := k + 2; }\n            }\n".to_string()
            }
            Touch::MatchNoWildcard => {
                "            match k {\n                0 => { k := k + 1; }\n                1 => { k := k + 2; }\n            }\n".to_string()
            }
            Touch::MatchMultiPattern => {
                "            match k {\n                0, 1 => { k := k + 1; }\n                _ => { k := k + 2; }\n            }\n".to_string()
            }
            Touch::MatchEnum => {
                "            match phase {\n                Low => { k := k + 1; }\n                _ => { k := k + 2; }\n            }\n".to_string()
            }
            Touch::MatchNested => {
                "            match k {\n                0 => {\n                    match phase {\n                        Low => { k := k + 1; }\n                        _ => { k := k + 2; }\n                    }\n                }\n                _ => { k := k + 3; }\n            }\n".to_string()
            }
            _ => "            k := k + 1;\n".to_string(),
        }
    }

    /// Переход из стартового состояния.
    fn transition(self) -> &'static str {
        match self {
            // Выдержка - вид, которому нужен ход времени.
            Touch::ClockAfter | Touch::TimeClockDeclared => "        ref Done: after 5ms;\n",
            Touch::TimeAfterTicks => "        ref Done: after 5t;\n",
            Touch::TimeDurationVar => "        ref Done: after hold;\n",
            Touch::TimeComputed => "        ref Done: after (SETTLE + 500ms);\n",
            // Именованное условие на ребре: здесь оно и живёт штатно, а `Nested` - то
            // же через второе условие.
            Touch::CondOnEdge => "        ref Done: Low;\n",
            Touch::CondNested => "        ref Done: Nested;\n",
            // Вызов стоит В условии - это и есть предмет вида.
            Touch::FnCallOnEdge => "        ref Done: twice(k) > 3;\n",
            _ => "        next Done;\n",
        }
    }
}

/// Форма, которой состояние обёртки реализовано.
#[derive(Clone, Copy, PartialEq)]
pub(crate) enum Shape {
    /// Обычное состояние: обращение делает сама обёртка.
    Plain,
    /// `= First` - одна модель.
    Single,
    /// `= First | Second` - параллель.
    Parallel,
    /// `= First + Second` - цепочка.
    Chain,
    /// `= (First + Second) | Third` - вложенная композиция.
    Nested,
    /// `= ((First + Second) | Third) + Fourth` - цепочка внутри параллели, которая сама
    /// является шагом цепочки.
    ///
    /// Глубина здесь и есть предмет: цель `rust` знала ровно два случая - цепочка
    /// состояния и цепочка внутри параллели состояния, - и на этой форме цепочка `First
    /// + Second` тикала параллелью. Вывод оставался валидным, `clippy -D warnings` его
    /// принимал: расхождение видит только потактовая сверка
    /// (`conformance_nested_ready_tests`).
    Deep,
}

/// Все формы реализации.
pub(crate) const SHAPES: [Shape; 6] = [
    Shape::Plain,
    Shape::Single,
    Shape::Parallel,
    Shape::Chain,
    Shape::Nested,
    Shape::Deep,
];

impl Shape {
    pub(crate) fn name(self) -> &'static str {
        match self {
            Shape::Plain => "plain",
            Shape::Single => "single",
            Shape::Parallel => "parallel",
            Shape::Chain => "chain",
            Shape::Nested => "nested",
            Shape::Deep => "deep",
        }
    }

    fn implementation(self) -> &'static str {
        match self {
            Shape::Plain => "",
            Shape::Single => "First",
            Shape::Parallel => "First | Second",
            Shape::Chain => "First + Second",
            Shape::Nested => "(First + Second) | Third",
            Shape::Deep => "((First + Second) | Third) + Fourth",
        }
    }
}

/// Модель-спутник без единого обращения к корню.
fn plain_child(name: &str) -> String {
    format!(
        "model {name} {{\n    var k: u8 := 0;\n    start Go {{\n        always {{\n            k := k + 1;\n        }}\n        next Done;\n    }}\n    state Done;\n}}\n\n"
    )
}

/// Модель, делающая обращение вида `touch` над значением формы `kind`.
fn touching_model(name: &str, touch: Touch, kind: Kind) -> String {
    // Инвариант состояния объявляется внутри состояния - это другое из шести мест
    // объявления формулы.
    let in_state = match touch {
        Touch::InvariantState => "        invariant InState = k < 200;\n",
        // Инвариант, который держится только в своём состоянии: во втором `k` выходит
        // за границу, и вывод остаётся корректным лишь тогда, когда проверка стоит в
        // ветви состояния-владельца.
        Touch::InvariantScoped => "        invariant OnlyHere = k < 250;\n",
        // Имя обязательства доезжает до сообщения цели `rust`.
        Touch::InvariantNamed => "        invariant NamedHere = k < 200;\n",
        // Темпоральная формула уровня состояния: её область - прогоны из него.
        Touch::LtlInState => "        : [LTL] G Low;\n",
        // Периодический блок объявляется В состоянии - ещё одно из мест, где живёт
        // время.
        Touch::TimeEvery => "        every 3ms {\n            k := k + 2;\n        }\n",
        _ => "",
    };
    format!(
        "model {name} {{\n    var k: u8 := 0;\n{decl}{funcs}    start Go {{\n{in_state}        always {{\n{body}        }}\n{transition}    }}\n    state Done;\n}}\n\n",
        decl = touch.declarations(kind),
        funcs = touch.functions(kind),
        body = touch.body(kind),
        transition = touch.transition(),
    )
}

/// Файл пробы: имя (без каталога) и содержимое.
pub(crate) struct ProbeFile {
    /// Имя файла - им же зовётся модель-контейнер (правило "имя корневой модели берётся
    /// из имени файла").
    pub(crate) name: &'static str,
    /// Содержимое.
    pub(crate) text: String,
}

/// Подключаемые файлы случая: пусто у всех видов, кроме импортов.
///
/// Имена библиотек - `helper.takt` и `base.takt`: имя файла становится именем
/// модели-контейнера, и совпадение с именем пробы (`probe`) дало бы столкновение, а не
/// проверку импорта.
pub(crate) fn library_files(touch: Touch) -> Vec<ProbeFile> {
    let helper = |extra: &str| ProbeFile {
        name: "helper.takt",
        text: format!(
            "struct Pair {{\n    lo: u8,\n    hi: u8\n}}\n\nconst CAP: u8 := 9;\n\nfn twice(v: u8) -> u8 {{\n    return v * 2;\n}}\n\nmodel Engine {{\n    var h: u8 := 0;\n    start Work {{\n        always {{\n            h := h + 1;\n        }}\n        next Done;\n    }}\n    state Done;\n}}\n{extra}"
        ),
    };
    match touch {
        Touch::ImportFunction
        | Touch::ImportSelective
        | Touch::ImportType
        | Touch::ImportModel
        | Touch::ImportNestedModel => vec![helper("")],
        // Имя модели совпадает с именем файла: импортёр видит контейнер `Clash`, внутри
        // которого модель `Clash`.
        Touch::ImportNameClash => vec![ProbeFile {
            name: "clash.takt",
            // Модель пишет В порт намеренно: без этого признак "нужен ли HAL" отвечает
            // "нет" у обеих одноимённых моделей, и второй слой дефекта (вызов
            // `tick(&mut *hal)` из функции без `hal`) проба не показывает - мутация
            // признака остаётся непойманной.
            text: "model Clash {\n    out beat: u8 at 0x40000100;\n    var h: u8 := 0;\n    start Work {\n        always {\n            h := h + 1;\n            beat := h;\n        }\n        next Done;\n    }\n    state Done;\n}\n\nstart Root = Clash;\n".to_string(),
        }],
        // Внешняя карта адресов лежит рядом с пробой и перекрывает inline.
        Touch::AddressMap => vec![ProbeFile {
            name: "plat.map",
            text: "a = 0x00200004;\n".to_string(),
        }],
        // Транзитивный импорт: подключённый файл сам подключает третий.
        Touch::ImportTransitive => vec![
            ProbeFile {
                name: "base.takt",
                text: "fn base_value() -> u8 {\n    return 3;\n}\n".to_string(),
            },
            ProbeFile {
                name: "mid.takt",
                text: "import \"base.takt\";\n\nfn mid_value() -> u8 {\n    return base_value() + 1;\n}\n"
                    .to_string(),
            },
        ],
        _ => Vec::new(),
    }
}

/// Аргумент инстанцирования для видов с параметром; `None` - вид не про них.
///
/// Форма реализации (`Shape`) к этим видам не применяется: модель-донор
/// **сама** и есть реализация состояния обёртки.
fn parameter_argument(touch: Touch) -> Option<&'static str> {
    match touch {
        Touch::ParameterDefault => Some(""),
        Touch::ParameterArgument => Some("(portion := 7)"),
        Touch::ParameterExpression => Some("(portion := BASE + 2)"),
        _ => None,
    }
}

/// Дополнительные ключи CLI, которых требует вид обращения.
pub(crate) fn extra_flags(touch: Touch) -> Vec<String> {
    match touch {
        // Карта передаётся путём от рабочего каталога процесса, а не от каталога пробы
        // (в отличие от `import`, который ищется рядом с импортёром, - ). Путь
        // подставляет вызывающий: он один знает каталог случая.
        Touch::AddressMap => vec!["--address-map".to_string(), "{dir}/plat.map".to_string()],
        // Среда вычислителя адреса: имя видно только ему.
        Touch::AddressDefine => vec!["-DBASE=0x40000000".to_string()],
        // Модель объявила такт устройства - флаг обязан совпасть, иначе
        // `SE-069`/`SE-070` (контракт 0134). Это часть случая, а не настройка.
        Touch::TimeClockDeclared => vec!["--tick-hz=1000".to_string()],
        _ => Vec::new(),
    }
}

/// Строка подключения для вида обращения.
fn import_line(touch: Touch) -> &'static str {
    match touch {
        Touch::ImportFunction | Touch::ImportType | Touch::ImportNestedModel => {
            "import \"helper.takt\";\n\n"
        }
        Touch::ImportSelective => "import { twice } from \"helper.takt\";\n\n",
        Touch::ImportModel => "import { Engine } from \"helper.takt\";\n\n",
        Touch::ImportTransitive => "import \"mid.takt\";\n\n",
        Touch::ImportNameClash => "import \"clash.takt\";\n\n",
        _ => "",
    }
}

/// Исходник случая: обращение живёт в `First` (либо в самой обёртке).
pub(crate) fn source(shape: Shape, touch: Touch, kind: Kind) -> String {
    let mut text = String::new();
    text.push_str(import_line(touch));
    text.push_str(kind.type_declaration());
    text.push_str(&touch.root_declarations(kind));
    // Модель с параметром: донор объявляет `parameter`, обёртка берёт его реализацией -
    // с настройкой по умолчанию либо с аргументом.
    if let Some(argument) = parameter_argument(touch) {
        text.push_str("const BASE: u8 := 4;\n\n");
        text.push_str(
            "model Feeder {\n    parameter portion: u8 := 3;\n    var k: u8 := 0;\n    out led: u8 at 0x40000100;\n    start Go {\n        always {\n            k := k + portion;\n            led := k;\n        }\n        next Done;\n    }\n    state Done;\n}\n\n",
        );
        text.push_str(&format!(
            "model Wrap {{\n    start Only = Feeder{argument};\n}}\n\nstart Main = Wrap;\n"
        ));
        return text;
    }
    // Модель подключённого файла реализует состояние обёртки - формы реализации к ней
    // не применяются: она сама и есть реализация. Контейнер файла реализует состояние
    // обёртки, и имя у него то же, что у модели внутри.
    if touch == Touch::ImportNameClash {
        text.push_str("model Wrap {\n    start Only = Clash;\n}\n\nstart Main = Wrap;\n");
        return text;
    }
    if matches!(touch, Touch::ImportModel | Touch::ImportNestedModel) {
        let implementation = if touch == Touch::ImportModel {
            "Engine"
        } else {
            // Полный импорт вносит только контейнер файла; вложенная модель снаружи не
            // видна - законный отказ, часть таблицы.
            "Helper"
        };
        text.push_str(&format!(
            "model Wrap {{\n    start Only = {implementation};\n}}\n\n"
        ));
        text.push_str("start Main = Wrap;\n");
        return text;
    }
    if shape == Shape::Plain {
        // Обращение делает сама обёртка: спутники не нужны.
        text.push_str(&touching_model("Wrap", touch, kind));
    } else {
        text.push_str(&touching_model("First", touch, kind));
        text.push_str(&plain_child("Second"));
        text.push_str(&plain_child("Third"));
        // Четвёртый спутник нужен только глубокой форме: лишняя модель в прочих формах
        // меняла бы их вывод даром.
        if shape == Shape::Deep {
            text.push_str(&plain_child("Fourth"));
        }
        text.push_str(&format!(
            "model Wrap {{\n    start Only = {};\n}}\n\n",
            shape.implementation()
        ));
    }
    text.push_str("start Main = Wrap;\n");
    text
}

/// Все случаи перебора: форма реализации x вид обращения x форма объявления.
///
/// Форма объявления перебирается только у тех видов, которые её касаются
/// (`varies_by_kind`): у `none`, `clock_after` и `extern_call` объявленного значения
/// нет вовсе, и четыре одинаковых прогона были бы просто платой за время.
pub(crate) fn cases() -> Vec<(Shape, Touch, Kind)> {
    let mut out = Vec::new();
    for shape in SHAPES {
        for touch in TOUCHES {
            if touch.varies_by_kind() {
                for kind in KINDS {
                    out.push((shape, touch, kind));
                }
            } else {
                out.push((shape, touch, Kind::Scalar));
            }
        }
    }
    out
}

/// Имя случая - оно же тег каталога и строка отчёта.
pub(crate) fn case_name(shape: Shape, touch: Touch, kind: Kind) -> String {
    if touch.varies_by_kind() {
        format!("{}_{}_{}", shape.name(), touch.name(), kind.name())
    } else {
        format!("{}_{}", shape.name(), touch.name())
    }
}
