//! Пробы для перебора сценариев сборки.

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum Probe {
    /// Индексация переменным индексом в теле состояния.
    IndexInBody,
    /// Функция с параметром-массивом: `return a[i];`.
    IndexInFunction,
    /// Функция читает массив модели - массив приходит ей неявно.
    ModelArrayInFunction,
    /// Маленькие функции: предмет подстановки.
    SmallFunctions,
    /// Обязательство: инвариант и охранная формула.
    GuardFormula,
    /// Пять состояний и цепочка: предмет табличной формы.
    ManyStates,
}

/// Все пробы оси.
pub(crate) const PROBES: &[Probe] = &[
    Probe::IndexInBody,
    Probe::IndexInFunction,
    Probe::ModelArrayInFunction,
    Probe::SmallFunctions,
    Probe::GuardFormula,
    Probe::ManyStates,
];

/// Короткое имя пробы - часть имени случая.
pub(crate) fn probe_name(probe: Probe) -> &'static str {
    match probe {
        Probe::IndexInBody => "index_body",
        Probe::IndexInFunction => "index_fn",
        Probe::ModelArrayInFunction => "model_array_fn",
        Probe::SmallFunctions => "small_fns",
        Probe::GuardFormula => "guard_formula",
        Probe::ManyStates => "many_states",
    }
}

/// Исходник пробы.
///
/// У каждой пробы есть выходной порт **с адресом**: без адреса цели `c-hal` и `st-at`
/// отказывают `SE-052`, и перебор мерил бы отказ, а не вывод.
pub(crate) fn source(probe: Probe) -> String {
    match probe {
        Probe::IndexInBody => concat!(
            "model Bp {\n",
            "    var data: [u8;4] := {1, 2, 3, 4};\n",
            "    var idx: u8 := 0;\n",
            "    var acc: u8 := 0;\n",
            "    out o: u8 at 0x40000100;\n",
            "    start Work {\n",
            "        always {\n",
            "            acc := acc + data[idx];\n",
            "            idx := idx + 1;\n",
            "            o := acc;\n",
            "        }\n",
            "        ref Work: idx < 4;\n",
            "        next Done;\n",
            "    }\n",
            "    state Done;\n",
            "}\n\nstart Main = Bp;\n"
        )
        .to_string(),
        Probe::IndexInFunction => concat!(
            "model Bp {\n",
            "    var data: [u8;4] := {1, 2, 3, 4};\n",
            "    var idx: u8 := 0;\n",
            "    var acc: u8 := 0;\n",
            "    out o: u8 at 0x40000100;\n",
            "    fn pick(a: [u8;4], i: u8) -> u8 {\n",
            "        return a[i];\n",
            "    }\n",
            "    start Work {\n",
            "        always {\n",
            "            acc := acc + pick(data, idx);\n",
            "            idx := idx + 1;\n",
            "            o := acc;\n",
            "        }\n",
            "        ref Work: idx < 4;\n",
            "        next Done;\n",
            "    }\n",
            "    state Done;\n",
            "}\n\nstart Main = Bp;\n"
        )
        .to_string(),
        Probe::ModelArrayInFunction => concat!(
            "model Bp {\n",
            "    var data: [u8;4] := {1, 2, 3, 4};\n",
            "    var idx: u8 := 0;\n",
            "    var acc: u8 := 0;\n",
            "    out o: u8 at 0x40000100;\n",
            "    fn pick(i: u8) -> u8 {\n",
            "        return data[i];\n",
            "    }\n",
            "    start Work {\n",
            "        always {\n",
            "            acc := acc + pick(idx);\n",
            "            idx := idx + 1;\n",
            "            o := acc;\n",
            "        }\n",
            "        ref Work: idx < 4;\n",
            "        next Done;\n",
            "    }\n",
            "    state Done;\n",
            "}\n\nstart Main = Bp;\n"
        )
        .to_string(),
        Probe::SmallFunctions => concat!(
            "model Bp {\n",
            "    var k: u8 := 0;\n",
            "    out o: u8 at 0x40000100;\n",
            "    fn twice(v: u8) -> u8 {\n",
            "        return v + v;\n",
            "    }\n",
            "    fn capped(v: u8) -> u8 {\n",
            "        if v > 10 {\n",
            "            return 10;\n",
            "        }\n",
            "        return v;\n",
            "    }\n",
            "    start Work {\n",
            "        always {\n",
            "            k := twice(k) + capped(k);\n",
            "            o := k;\n",
            "        }\n",
            "        ref Work: k < 40;\n",
            "        next Done;\n",
            "    }\n",
            "    state Done;\n",
            "}\n\nstart Main = Bp;\n"
        )
        .to_string(),
        Probe::GuardFormula => concat!(
            "model Bp {\n",
            "    var k: u8 := 0;\n",
            "    out o: u8 at 0x40000100;\n",
            "    invariant Sane = k < 200;\n",
            "    start Work {\n",
            "        always {\n",
            "            k := k + 1;\n",
            "            o := k;\n",
            "        }\n",
            "        ref Work: k < 4;\n",
            "        next Done;\n",
            "    }\n",
            "    state Done;\n",
            "}\n\nstart Main = Bp;\n"
        )
        .to_string(),
        Probe::ManyStates => concat!(
            "model Bp {\n",
            "    var k: u8 := 0;\n",
            "    out o: u8 at 0x40000100;\n",
            "    start One {\n",
            "        always { k := k + 1; o := k; }\n",
            "        next Two;\n",
            "    }\n",
            "    state Two {\n",
            "        always { k := k + 2; o := k; }\n",
            "        ref Four: k > 100;\n",
            "        next Three;\n",
            "    }\n",
            "    state Three {\n",
            "        always { k := k + 3; o := k; }\n",
            "        next Four;\n",
            "    }\n",
            "    state Four {\n",
            "        always { o := k; }\n",
            "    }\n",
            "}\n\nstart Main = Bp;\n"
        )
        .to_string(),
    }
}

/// Сценарий сборки: набор флагов, которым зовут `taktc compile`.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) struct Scenario {
    /// Имя - часть имени случая.
    pub(crate) name: &'static str,
    /// Флаги сверх `-t <цель>`.
    pub(crate) flags: &'static [&'static str],
}

/// Сценарии сборки: одиночные флаги и их сочетания.
///
/// Сочетания - не украшение: `--inline=auto --bounds-check` дал форму, которую `clippy`
/// отверг, тогда как каждый флаг по отдельности проходил.
pub(crate) const SCENARIOS: &[Scenario] = &[
    Scenario {
        name: "base",
        flags: &[],
    },
    Scenario {
        name: "table",
        flags: &["--fsm=table"],
    },
    Scenario {
        name: "inline",
        flags: &["--inline=auto"],
    },
    Scenario {
        name: "bounds",
        flags: &["--bounds-check"],
    },
    Scenario {
        name: "inline_bounds",
        flags: &["--inline=auto", "--bounds-check"],
    },
    Scenario {
        name: "table_inline_bounds",
        flags: &["--fsm=table", "--inline=auto", "--bounds-check"],
    },
    Scenario {
        name: "guard_off",
        flags: &["--guard-disable"],
    },
];
