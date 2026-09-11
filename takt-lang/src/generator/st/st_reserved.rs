//! Проверка идентификаторов на столкновение со стандартной библиотекой и ключевыми
//! словами IEC 61131-3 - диагностика `ST-014`.
//!
//! ## Список - только проверенное пробой
//!
//! Каждое имя ниже **отвергается `iec2c`** - проверено прогоном `var <имя>: u8` через
//! MatIEC. Это существенно: список нельзя пополнять по памяти. `action`
//! MatIEC отвергает (SFC-ключевое слово), тогда как "здравый смысл" счёл бы его обычным
//! словом; наоборот, `remaining`, `count`, `mode`, `command` - **приняты** (в списке их
//! быть не должно).
//!
//! **Полнота не критична для худшего случая**: структурный дефект
//! (склейка одноимённых `fn`) закрывает префиксация POU
//! ([](../../../../../../../../docs/features/0065-st-namespace-isolation.md#разработка)) **независимо**
//! от этого списка; пропущенное здесь имя поймает `iec2c` - громко, пусть и
//! обманчиво. Список **улучшает диагностику**, а не является единственной
//! защитой. Поэтому в него включено только доказанно-отвергаемое: ложное
//! срабатывание (запрет имени, которое MatIEC принимает) было бы **хуже**
//! статус-кво - оно сломало бы валидную модель, которую сегодня `iec2c`
//! собирает.
//!
//! ## Где проверяется
//!
//! Проверка стоит **в точках эмиссии** (как `check_sv_name`), а не общим проходом по
//! дереву: так проверяется ровно то, что попадёт в `.st`. Неиспользуемая переменная
//! фильтруется генератором и в вывод не идёт - `iec2c` её тоже не увидит, поэтому и
//! `ST-014` на неё срабатывать не должен (иначе `elevator.takt`, где `var action:
//! Action` объявлен, но не используется, перестал бы собираться - регресс).

use crate::diagnostics::lang::keys;
use crate::diagnostics::{Diagnostic, Location};
use crate::msg;

/// Стандартные функции и ключевые слова IEC 61131-3, **отвергаемые `iec2c`** как
/// идентификаторы пользователя. Сравнение - без учёта регистра (см. [`check_st_name`]),
/// поэтому имена даны в нижнем регистре.
///
/// Пополнять **только** проверенным пробой (`var <имя>: u8` -> `iec2c`): см. заголовок
/// модуля.
const IEC_RESERVED: &[&str] = &[
    // Строковые функции.
    "left",
    "right",
    "mid",
    "concat",
    "len",
    "find",
    "insert",
    "delete",
    "replace",
    // Выбор / ограничение.
    "sel",
    "limit",
    "mux",
    "min",
    "max", // Числовые.
    "abs",
    "sqrt",
    "expt",
    // Сдвиги / вращения над битовыми строками.
    "shl",
    "shr",
    "rol",
    "ror", // Сравнения-функции.
    "gt",
    "ge",
    "le",
    "lt",
    "eq",
    "ne",
    // Стандартные функциональные блоки (триггеры, таймеры, счётчики).
    "sr",
    "rs",
    "ton",
    "tof",
    "tp",
    "ctu",
    "ctd",
    "ctud",
    "r_trig",
    "f_trig",
    // Ключевые слова SFC и управления.
    "step",
    "action",
    "exit",
    // Стандартные типы (идентификатором пользователя быть не могут).
    "time",
    "date",
    "real",
    "lreal",
    "int",
    "sint",
    "dint",
    "lint",
    "usint",
    "uint",
    "udint",
    "ulint",
    "byte",
    "word",
    "dword",
    "lword",
    "bool",
    "string",
    "wstring",
    // Математические функции. Замер прогоном `iec2c` - не догадка: `var ln: u8;` давал
    // невалидный ST при нулевом коде возврата `taktc`, а сообщение инструмента
    // ("invalid located variable declaration") причины не называло.
    "ln",
    "log",
    "exp",
    "sin",
    "cos",
    "tan",
    "asin",
    "acos",
    "atan",
    "trunc",
    // Операции, имеющие в IEC функциональную форму: `MOD`, `AND`, `ADD`, ...
    "mod",
    "and",
    "or",
    "xor",
    "not",
    "add",
    "sub",
    "mul",
    "div",
    "move",
    // `adr`, `size`, `bcd_to_int`, `int_to_bcd` в список не входят: первый замер счёл
    // их занятыми, а прогон теста показал, что `iec2c` их принимает. Отказ на них был
    // бы ложным - цель отвергала бы корректный исходник. Ключевые слова диапазона `FOR
    // ... TO ... BY`: `var from: u8;` и `var to: u8;` `iec2c` отвергает. В языке Takt
    // `from` стало обычным именем той же фичей, `to` было им всегда - то есть дыра
    // существовала и до неё.
    "from",
    "to",
    // Ключевые слова языка ST и объявлений. Прежний список знал функции и типы, но не
    // структуру программы: `var program: u8;` - имя из практики (счётчик программ
    // процессора) - давало невалидный ST при нулевом коде возврата `taktc`.
    //
    // Список - из прогона арбитра, и в него внесены только имена, которые язык Takt
    // принимает как идентификатор: `var`, `if`, `else`, `for`, `while`, `return`,
    // `true`, `false`, `type`, `struct`, `at` - ключевые слова самого Takt, и написать
    // их именем нельзя.
    //
    // `single`, `interval`, `priority` в список не вошли: тем же прогоном `iec2c` их
    // Принимает, и отказ на них был бы ложным.
    "program",
    "configuration",
    "resource",
    "task",
    "end_var",
    "function",
    "function_block",
    "end_type",
    "array",
    "of",
    "constant",
    "retain",
    "non_retain",
    "then",
    "elsif",
    "end_if",
    "case",
    "end_case",
    "do",
    "end_for",
    "end_while",
    "repeat",
    "until",
    "end_repeat",
    "with",
    "on",
    "transition",
    "initial_step",
    "by",
    "en",
    "eno",
    "f_edge",
    "r_edge",
];

/// Строит диагностику `ST-014` - идентификатор занят стандартной библиотекой IEC.
///
/// Текст **называет причину**, которой нет в диагностике `iec2c` (та говорит про `AT
/// %...`, которых в объявлении нет).
fn st014(name: &str, loc: Location) -> Diagnostic {
    Diagnostic::error(loc, msg!(keys::ST_014_RESERVED_NAME, name = name)).with_code("ST-014")
}

/// Имена, недопустимые как **имя поля структуры** - измерено прогоном `iec2c`.
///
/// Список **свой**, а не [`IEC_RESERVED`], и это замер: из 79 имён того списка поле
/// структуры принимает **52** (`left`, `min`, `abs`, `sel` - они стандартные функции, а
/// внутри `STRUCT` с ними ничто не сталкивается). Отвергаются только ключевые слова и
/// **имена типов** IEC. Запретить полю всё подряд значило бы отнять валидную модель -
/// ложное срабатывание здесь хуже пропуска.
const IEC_RESERVED_FIELD: &[&str] = &[
    // Ключевые слова языка и SFC.
    "action", "exit", "step", "from", "to",
    // Логические операции (в IEC это операторы, не имена).
    "and", "or", "xor", "not", "mod",
    // Имена типов IEC: в объявлении поля они стоят справа от двоеточия, и слева `iec2c`
    // их не принимает.
    "bool", "byte", "word", "dword", "lword", "sint", "int", "dint", "lint", "usint", "uint",
    "udint", "ulint", "real", "lreal", "time", "date", "string", "wstring",
];

/// Проверяет имя поля структуры.
///
/// Позицию берёт [`crate::generator::site`] - у имени поля своей координаты в печатнике
/// нет, а место оператора слой уже знает.
///
/// # Ошибки
/// [`ST-014`](st014) - имя занято ключевым словом либо типом IEC.
pub(crate) fn check_st_field_name(name: &str) -> Result<(), Diagnostic> {
    let loc = crate::generator::site::at(Location::Codegen);
    if IEC_RESERVED_FIELD
        .iter()
        .any(|kw| kw.eq_ignore_ascii_case(name))
    {
        return Err(st014(name, loc));
    }
    if let Some(ch) = non_ascii_char(name) {
        return Err(st020(name, ch, loc));
    }
    check_underscores(name, loc)?;
    Ok(())
}

/// Проверяет, что имя пригодно как идентификатор Structured Text.
///
/// Сравнение - **без учёта регистра**: идентификаторы IEC регистронезависимы, поэтому
/// `left`, `LEFT` и `Left` - одно и то же имя.
///
/// # Ошибки
/// [`ST-014`](st014) - имя занято стандартной библиотекой или ключевым словом IEC.
/// Проверяет имя состояния.
///
/// Имя состояния в выводе цели `st` - **комментарий**: автомат печатается числами
/// (`state := 2; (* Idle *)`), и идентификатором это имя нигде не становится. Поэтому
/// занятость имени стандартной библиотекой IEC его не касается: состояние `On`, `Exit`
/// или `Step` - законная модель, а отказ на неё был бы ложным.
///
/// Алфавит проверяется по-прежнему (`ST-020`): не-ASCII попадает в комментарий и в
/// разбор `CASE`, и до `iec2c` доезжать не должен.
pub(crate) fn check_st_state_name(name: &str, loc: Location) -> Result<(), Diagnostic> {
    if let Some(ch) = non_ascii_char(name) {
        return Err(st020(name, ch, loc));
    }
    Ok(())
}

pub(crate) fn check_st_name(name: &str, loc: Location) -> Result<(), Diagnostic> {
    if IEC_RESERVED.iter().any(|kw| kw.eq_ignore_ascii_case(name)) {
        return Err(st014(name, loc));
    }
    if let Some(ch) = non_ascii_char(name) {
        return Err(st020(name, ch, loc));
    }
    check_underscores(name, loc)?;
    Ok(())
}

/// Расположение подчёркивания в имени - диагностика `ST-026`.
///
/// Правило снято прогоном `iec2c`, а не чтением стандарта: хвостовое подчёркивание и
/// два подряд инструмент отвергает, ведущее принимает, хотя стандарт запрещает и его.
/// Отказ за то, что арбитр принимает, был бы ложным.
///
/// Без отказа такое имя доезжает до вывода как есть: `taktc` возвращает ноль, а `iec2c`
/// отвечает "invalid located variable declaration" - сообщением, по которому исходную
/// причину не опознать.
fn check_underscores(name: &str, loc: Location) -> Result<(), Diagnostic> {
    let trailing = name.ends_with('_');
    let doubled = name.contains("__");
    if !trailing && !doubled {
        return Ok(());
    }
    let what = if trailing {
        msg!(keys::ST_026_WHAT_TRAILING_UNDERSCORE)
    } else {
        msg!(keys::ST_026_WHAT_DOUBLE_UNDERSCORE)
    };
    Err(
        Diagnostic::error(loc, msg!(keys::ST_026_UNDERSCORE, name = name, what = what))
            .with_code("ST-026"),
    )
}

/// Первый символ имени вне алфавита идентификатора IEC 61131-3 (или `None`).
///
/// Алфавит - `[A-Za-z0-9_]`; всё прочее `iec2c` отвергает разбором.
fn non_ascii_char(name: &str) -> Option<char> {
    name.chars()
        .find(|c| !(c.is_ascii_alphanumeric() || *c == '_'))
}

/// Строит диагностику `ST-020` - символ вне алфавита идентификатора IEC.
///
/// Отказ принадлежит **цели**, а не языку: `c` и `rust` такие имена переводят, и их
/// проверки вывод принимают.
fn st020(name: &str, ch: char, loc: Location) -> Diagnostic {
    Diagnostic::error(loc, msg!(keys::ST_020_BAD_CHAR, name = name, ch = ch)).with_code("ST-020")
}

/// Проверка имени объявления - одна воронка.
///
/// Спрашивает всё, что делает имя непригодным для IEC: столкновение со стандартной
/// библиотекой и ключевыми словами (`ST-014`), символ вне алфавита (`ST-020`) и
/// совпадение с именем объявленного типа (`ST-023`). Место объявления знает модель,
/// поэтому воронка принимает её; там, где модели нет (имя POU, имя состояния), зовётся
/// [`check_st_name`].
pub(crate) fn check_st_declaration(
    name: &str,
    model: &crate::semantic::ModelNode,
    loc: Location,
) -> Result<(), Diagnostic> {
    check_st_name(name, loc)?;
    check_st_type_clash(name, model, loc)
}

/// Столкновение имени с объявленным типом - диагностика `ST-023`.
///
/// Пространство имён IEC плоское и **регистронезависимое**, поэтому
/// `var pair: Pair;` для MatIEC - два объявления одного идентификатора. Ответ
/// он даёт обманчивый ("invalid located variable declaration" с указанием на
/// `AT %...`, которых в объявлении нет вовсе), а `taktc` при этом возвращает
/// **ноль**: замер 2026-08-21 показал, что эталон и шесть остальных
/// потребителей тот же вход исполняют.
///
/// Отказ принадлежит **цели**, а не языку - как `ST-014` и `SV-012`: имя законно, и
/// модель остаётся валидной для прочих целей. Текст это называет.
///
/// Сравнение - **без учёта регистра**: `pair`/`PAIR`/`Pair` в IEC один идентификатор, и
/// совпадение по регистру ничего не значит.
pub(crate) fn check_st_type_clash(
    name: &str,
    model: &crate::semantic::ModelNode,
    loc: Location,
) -> Result<(), Diagnostic> {
    let Some(clash) = declared_type_names(model)
        .into_iter()
        .find(|ty| ty.eq_ignore_ascii_case(name))
    else {
        return Ok(());
    };
    Err(Diagnostic::error(
        loc,
        msg!(keys::ST_023_TYPE_CLASH, name = name, clash = clash),
    )
    .with_code("ST-023"))
}

/// Имена глобального пространства, которые печатает цель `st-at`.
///
/// Список - из **замера**, а не из стандарта: `iec2c` отвергает совпадение с именем
/// блока (`FUNCTION_BLOCK`), программы (`<Root>Main`), ресурса (`Res0`) и экземпляра
/// программы (`Inst0`), но **принимает** совпадение с именем конфигурации
/// (`<Root>Config`) и задачи (`Tick`). Прогон 2026-08-31 по семи именам - карточка
/// фичи.
pub(crate) fn global_pou_names(root: &str, blocks: &[String]) -> Vec<String> {
    let mut names: Vec<String> = blocks.to_vec();
    names.push(format!("{root}Main"));
    names.push("Res0".to_string());
    names.push("Inst0".to_string());
    names
}

/// Столкновение имени глобальной переменной с именем POU - `ST-024`.
pub(crate) fn check_st_global_clash(
    name: &str,
    occupied: &[String],
    loc: Location,
) -> Result<(), Diagnostic> {
    let Some(clash) = occupied
        .iter()
        .find(|taken| taken.eq_ignore_ascii_case(name))
    else {
        return Ok(());
    };
    Err(Diagnostic::error(
        loc,
        msg!(keys::ST_024_GLOBAL_CLASH, name = name, clash = clash),
    )
    .with_code("ST-024"))
}

/// Столкновение имён внутри одного POU - диагностика `ST-025`.
pub(crate) fn check_st_local_clash(
    name: &str,
    occupied: &[String],
    loc: Location,
) -> Result<(), Diagnostic> {
    let Some(clash) = occupied
        .iter()
        .find(|taken| taken.eq_ignore_ascii_case(name))
    else {
        return Ok(());
    };
    Err(Diagnostic::error(
        loc,
        msg!(keys::ST_025_POU_CLASH, name = name, clash = clash),
    )
    .with_code("ST-025"))
}

/// Имена типов, которые цель печатает в разделе `TYPE`, - модели и её предков.
///
/// **Только структуры, и это замер, а не забывчивость**. Перечисление цель печатает
/// целым типом плюс `VAR CONSTANT` (откат Option C: перечислимый тип с явными
/// значениями MatIEC не принимает), а псевдоним раскрывается в базовый тип - ни того,
/// ни другого имени в выводе нет, и занимать пространство имён IEC им нечем.
///
/// Отсюда правило: список строится из того, что цель **печатает**, а не из того, что
/// объявлено в модели.
fn declared_type_names(model: &crate::semantic::ModelNode) -> Vec<String> {
    let mut out: Vec<String> = model.structs.values().map(|d| d.name.clone()).collect();
    // Предки: структура, объявленная выше, печатается в тот же раздел `TYPE`.
    let mut upper = model.upper.clone();
    while let Some(weak) = upper {
        let Some(rc) = weak.upgrade() else { break };
        let parent = rc.borrow();
        out.extend(parent.structs.values().map(|d| d.name.clone()));
        upper = parent.upper.clone();
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn loc() -> Location {
        Location::Codegen
    }

    /// Проверенные пробой имена отвергаются.
    #[test]
    fn test_reserved_names_rejected() {
        for name in [
            "abs", "min", "max", "sel", "limit", "step", "left", "concat",
        ] {
            assert!(
                check_st_name(name, loc()).is_err(),
                "'{}' обязано отвергаться (ST-014)",
                name
            );
        }
    }

    /// Имя, совпавшее с именем типа, отвергается `ST-023`.
    #[test]
    fn test_variable_named_like_type_is_st023() {
        let (ast, _) = crate::parse(
            "struct Pair { lo: u8, hi: u8 }\nvar pair: Pair := {1, 2};\nstart Run { }",
            0,
        )
        .expect("разбор");
        let model = crate::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
        let model = model.borrow();
        let err = check_st_type_clash("pair", &model, loc()).expect_err("ожидался отказ");
        assert_eq!(err.code.as_deref(), Some("ST-023"));
        assert!(
            err.message.contains("'Pair'"),
            "отказ обязан назвать ТИП: {}",
            err.message
        );
        // Регистр не спасает: в IEC идентификаторы регистронезависимы.
        assert!(check_st_type_clash("PAIR", &model, loc()).is_err());
        // **Контрпример:** имя, ни с чем не столкнувшееся, проходит - иначе
        // проверка означала бы "запрещаем любое имя".
        assert!(check_st_type_clash("value", &model, loc()).is_ok());
    }

    /// Тёзка перечисления проходит: цель печатает его целым типом и константами, и
    /// имени `Mode` в выводе нет.
    ///
    /// Возьми проверка имена у `structs`, `enums` и `types` разом - она отвергнет
    /// `var mode: Mode;`, вход, который `iec2c` принимает. Ложный отказ хуже пропуска:
    /// он ломает валидную модель.
    #[test]
    fn test_enum_namesake_is_allowed() {
        let (ast, _) = crate::parse(
            "enum Mode { Idle = 1, Work = 2 }\nvar mode: Mode := Idle;\nstart Run { }",
            0,
        )
        .expect("разбор");
        let model = crate::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
        let model = model.borrow();
        assert!(
            check_st_type_clash("mode", &model, loc()).is_ok(),
            "перечисление имени в выводе цели не занимает"
        );
    }

    /// Тёзка псевдонима проходит по той же причине: он раскрывается в базовый тип, и
    /// его имени в выводе нет.
    #[test]
    fn test_alias_namesake_is_allowed() {
        let (ast, _) = crate::parse(
            "type Small = u8;\nvar small_v: Small := 1;\nstart Run { }",
            0,
        )
        .expect("разбор");
        let model = crate::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
        let model = model.borrow();
        assert!(check_st_type_clash("small", &model, loc()).is_ok());
    }

    /// Регистр не спасает: `left`/`LEFT`/`Left` - одно и то же.
    #[test]
    fn test_case_insensitive() {
        for name in ["left", "LEFT", "Left", "LeFt"] {
            let err = check_st_name(name, loc()).unwrap_err();
            assert_eq!(err.code.as_deref(), Some("ST-014"), "имя '{}'", name);
        }
    }

    /// Имена, которые `iec2c` принимает, проходить обязаны - иначе ложное срабатывание
    /// сломало бы валидную модель (страж против переусердствования со списком).
    #[test]
    fn test_accepted_names_pass() {
        for name in [
            "remaining",
            "count",
            "busy",
            "mode",
            "command",
            "value",
            "state",
            "delta",
            "x",
        ] {
            assert!(
                check_st_name(name, loc()).is_ok(),
                "'{}' MatIEC принимает — ST-014 срабатывать не должна",
                name
            );
        }
    }
}
