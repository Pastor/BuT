//! Генератор синтезируемого SystemVerilog (IEEE 1800) из семантического дерева Takt.
//!
//! Шестой целевой язык и **первый аппаратный**: C, ST и Rust - программные цели (такт =
//! итерация цикла сканирования), здесь такт Takt == **фронт тактового сигнала**
//! `posedge clk`. Архитектурное решение -.
//!
//! ## Форма вывода
//!
//! Один `.sv`-файл, **один `module` на корневую модель**: композиция `M1 | M2`
//! уплощается в общий `always_comb` (Option A′). Иерархия модулей SV дерево моделей не
//! повторяет - это плата за точную семантику `|`, где модель, идущая позже, обязана
//! видеть записи предыдущей **в том же такте**. Модуль-на-модель дал бы комбинационную
//! петлю (проверено: `verilator` -> `UNOPTFLAT`).
//!
//! ## Состав модуля
//!
//! `sv_map` (снимок карты) · `sv_type` (типы, `SV-002`...`SV-004`) · `sv_module`
//! (модуль и порты, `SV-006`/`SV-007`) · `sv_fsm` (автомат и сброс, `SV-008`) ·
//! `sv_expr` (выражения и функции, `SV-005`).

// Адаптер шины APB: обёртка над регистровым интерфейсом ядра.
mod sv_apb;
mod sv_arith;
mod sv_array;
mod sv_blocks;
mod sv_call;
mod sv_cast;
mod sv_compose;
mod sv_const;
mod sv_enums;
mod sv_expr;
mod sv_fixed;
mod sv_fsm;
mod sv_func;
mod sv_locals;
mod sv_map;
mod sv_mmio;
mod sv_module;
mod sv_names;
mod sv_scope;
mod sv_state_of;
mod sv_stmt;
mod sv_table;
mod sv_time;
mod sv_type;
mod sv_unroll;
mod sv_unused;

use crate::diagnostics::{Diagnostic, Location};
use crate::generator::Generator as AsGenerator;
use crate::generator::header::{CommentStyle, file_header};
use crate::generator::indent::Printer;
use crate::generator::{GenerateOptions, GeneratedFile, Output};
use crate::semantic::ModelNode;
use crate::semantic::minimap::{Element, Name};
use crate::semantic::naming::normalize_lowercase_snakecase;
use std::cell::RefCell;
use std::rc::Rc;
use sv_map::SvMap;

/// Размер одного уровня отступа в порождаемом SystemVerilog.
const INDENT: usize = 4;

/// Генератор SystemVerilog для модели Takt.
///
/// Флаг [`mmio`](Generator::mmio) выбирает форму вывода: `false` - цель `sv` (порты
/// Takt -> порты модуля, адрес не потребляется); `true` - цель `sv-mmio` (порт с
/// адресом -> бит регистрового файла на шинно-агностичном интерфейсе).
pub struct Generator {
    /// Режим регистрового файла (цель `sv-mmio`).
    pub mmio: bool,
}

impl AsGenerator for Generator {
    fn generate_texts(
        &self,
        model: &ModelNode,
        options: &GenerateOptions,
    ) -> Result<Output, Diagnostic> {
        // Профиль времени: `clock` модели - контракт, флаг обязан подтвердить. Единый
        // чекпойнт-энфорсмент `SE-069`/`SE-070`.
        let profile = crate::semantic::duration::resolve_profile(model.clock_hz, options.tick_hz)?;
        let map = SvMap::new(
            &normalize_lowercase_snakecase(model.name().to_string()),
            model,
            options.guard_enable,
        )?
        .with_time_profile(profile)
        .with_fsm(options.fsm)
        .with_comments(options.comments.clone());
        let (program, warnings, adapter) =
            generate_program(&map, self.mmio, &options.address_map, options.bus)?;
        let filename = map.get_filename();
        let mut files = vec![GeneratedFile {
            name: filename.to_owned() + ".sv",
            text: program,
        }];

        // Адаптер шины - Отдельный файл рядом с ядром: ядро остаётся шинно-агностичным,
        // и второй протокол не потребует его трогать. Без флага не эмитится ничего,
        // поэтому прежний вывод байт-в-байт цел.
        if let Some((suffix, text)) = adapter {
            files.push(GeneratedFile {
                name: format!("{filename}{suffix}.sv"),
                text,
            });
        }
        Ok(Output { files, warnings })
    }

    fn write_failure(&self, error: &std::io::Error) -> Diagnostic {
        Diagnostic::error(Location::Codegen, format!("{error}")).with_code("SV-001")
    }
}

/// Результат сборки модуля: текст, предупреждения цели и - при `--bus` - адаптер шины
/// (суфисправление имени файла и его текст).
///
/// Именованный тип, а не кортеж из трёх: `clippy::type_complexity` прав - читать
/// `Result<(String, Vec<Diagnostic>, Option<(&str, String)>), _>` невозможно.
type ProgramOutput = (String, Vec<Diagnostic>, Option<(&'static str, String)>);

/// Собирает текст модуля SystemVerilog из снимка модели.
///
/// Каркас: порождает минимальный **синтезируемый** модуль - объявление, служебные порты
/// `clk`/`rst_n`, регистр состояния, `always_comb` вместе с `always_ff` и выход
/// `is_done`. Это не "пустая заглушка", а вырожденный случай целевой формы: он проходит
/// оба проверки, которые тем самым ставятся **сразу**, а не после накопления кода.
/// Содержательное наполнение - ....
fn generate_program(
    map: &SvMap,
    mmio: bool,
    address_map: &std::collections::HashMap<String, crate::address_map::ResolvedAddress>,
    bus: Option<crate::generator::Bus>,
) -> Result<ProgramOutput, Diagnostic> {
    let Element::Model { .. } = map.model() else {
        return Err(Diagnostic::error(
            crate::generator::site::at(Location::Codegen),
            "Корневой элемент карты не является моделью".to_string(),
        )
        .with_code("SV-010"));
    };
    let root_name = map.root_name();
    let root = map.root_model_node().ok_or_else(|| {
        Diagnostic::error(
            crate::generator::site::at(Location::Codegen),
            format!("Корневая модель '{}' отсутствует в снимке карты", root_name),
        )
        .with_code("SV-010")
    })?;

    // Модуль один на корневую модель (Option A′): композиция уплощается, поэтому порты
    // собираются со всех уровней - в `elevator_mini.takt` они объявлены внутри
    // под-моделей. Порядок под-моделей задан `BTreeMap` карты - детерминизм достаётся
    // даром.
    let mut blocks: Vec<(Name, Rc<RefCell<ModelNode>>)> = Vec::new();
    let mut submodels: Vec<Name> = map
        .using_models()
        .into_iter()
        .filter_map(|element| match element {
            Element::Model { name, .. } => Some(name),
            _ => None,
        })
        .collect();
    submodels.sort_by(|a, b| a.unique().cmp(b.unique()));
    for name in submodels {
        let model = map.raw_model_at(name.clone())?;
        blocks.push((name, model));
    }
    blocks.push((root_name.clone(), root));

    // Режим `sv-mmio`: порт **с** адресом -> бит регистрового файла, порт **без**
    // адреса -> порт модуля. Карта адресов уже разрешена (`resolve_addresses`,
    // приоритет inline < `address` < внешняя) и передана в `options.address_map`
    // библиотечной обёрткой `compile_to_sv_mmio` - как для `c-hal`. В режиме `sv`
    // (`mmio == false`) `mmio_map` пуст и всё ниже вырождается в прежний вывод (T3/A3 -
    // побайтовое равенство). Анонимные ячейки собираются с корня: в регистровый файл
    // они входят наравне с адресованными портами.
    let anon_cells = map
        .root_model_node()
        .map(|root| crate::semantic::collect_anon_ports(&root))
        .unwrap_or_default();
    // Цель `sv` (без регистрового файла) адресного пространства не имеет: сигнал
    // приходит на вывод кристалла. Отказ - в точке входа, до первой строки вывода, а не
    // в печатнике: так режим цели не приходится тянуть через все печатники (тот же
    // приём, что у цели `st`).
    if !mmio && let Some(cell) = anon_cells.first() {
        return Err(Diagnostic::error(
            // Место обращения к ячейке: отказ приходит до первой строки вывода, когда
            // носитель позиции ещё пуст.
            crate::generator::site::at(cell.loc),
            format!(
                "обращение к ячейке по адресу ('#0x{:X}') требует адресного \
                 пространства, которого у RTL нет: сигнал приходит на вывод \
                 кристалла. Соберите целью 'sv-mmio'",
                cell.addr as u64
            ),
        )
        .with_code("SV-017"));
    }
    let mmio_map = if mmio {
        Some(sv_mmio::Mmio::build(&blocks, address_map, &anon_cells)?)
    } else {
        None
    };
    let addressed = mmio_map
        .as_ref()
        .map(|m| m.addressed_names())
        .unwrap_or_default();

    let ports = sv_module::collect_ports(map, &blocks, &addressed)?;
    let fsm = sv_fsm::Fsm::build(map, &blocks, &root_name, &ports, mmio_map.as_ref())?;
    let module = normalize_lowercase_snakecase(root_name.unique().replace(':', "_"));
    // Имя порта не должно совпадать с именем модуля: `verilator` отвечает `VARHIDDEN`,
    // а проверка цели считает предупреждение ошибкой. Проверка стоит здесь, а не в
    // `check_sv_name`: имя модуля известно только сейчас - оно строится из имени
    // модели, а у корневой из имени файла.
    sv_module::check_module_name_clash(&module, map, &blocks)?;

    let mut out = String::new();
    let mut p = Printer::new(INDENT, &mut out);

    let target = if mmio {
        "SystemVerilog (IEEE 1800, MMIO register file)"
    } else {
        "SystemVerilog (IEEE 1800)"
    };
    for line in file_header(target, CommentStyle::Slashes) {
        p.ident(&line).nl();
    }
    p.nl();

    // Служебный вход времени (профиль "часы" + длительностная выдержка в дереве);
    // ширина - по максимуму `after` (R8), общий источник с регистрами.
    let time_ms_bits = match map.root_model_node() {
        Some(root) if sv_time::needs_time_port(map, &root.borrow()) => {
            Some(sv_time::time_bits(map)?)
        }
        _ => None,
    };
    // Пользовательские типы объявляются вне модуля: порт может
    // иметь структурный тип, а шапка модуля печатается раньше тела - verilator
    // отвечал "Reference to 'pair_t' before declaration". Форма проверена
    // **обоими** инструментами: `typedef` на уровне файла принимают и
    // verilator, и yosys.
    // Перечисления печатаются перед структурами: поле
    // перечислимого типа делает структуру зависимой от `mode_e`, а обратной
    // зависимости не бывает - варианты перечисления суть литералы. Прежде
    // порядок был обратным, и `verilator` отвечал "Reference to 'mode_e'
    // before declaration" при нулевом коде возврата `taktc` (тот же класс, что
    // 0341 и 0347: порядок разделов файла).
    sv_enums::emit_enums(&mut p, &blocks)?;
    sv_type::emit_structs(&mut p, &blocks)?;

    // Комментарий автора перед объявлением модели. Печатается здесь, а не в
    // `emit_module_header`: узла модели тот не видит, а тащить его туда ради двух строк -
    // лишний параметр в общей функции печати заголовка.
    for line in crate::generator::comments::leading(
        map.root_model_loc(),
        crate::generator::header::CommentStyle::Slashes,
    ) {
        p.ident(&line).nl();
    }
    sv_module::emit_module_header(&mut p, &module, &ports, mmio_map.as_ref(), time_ms_bits);

    p.up();
    // Константы - после типов: `localparam cell_t ...` ссылается на `typedef struct
    // packed ... cell_t`.
    let array_consts = sv_const::emit_constants(&mut p, map, &blocks)?;
    sv_enums::emit_state_enums(&mut p, map, &blocks)?;
    sv_enums::emit_step_enums(&mut p, &fsm)?;
    // Таблица переходов - После перечислений состояний: её векторы ссылаются на
    // варианты (порядок разделов файла - ).
    let models: Vec<_> = blocks.iter().map(|(name, _)| name.clone()).collect();
    if map.fsm_table() {
        sv_table::emit_tables(&mut p, map, &models)?;
    }
    sv_fsm::emit_signals(&mut p, &fsm);
    if map.fsm_table() {
        sv_table::emit_signals(&mut p, map, &models)?;
    }
    // Поглотители непрочитанных полей входного порта-структуры: объявления обязаны
    // стоять до `always_comb`, который их пишет.
    sv_module::emit_port_sinks(&mut p, &fsm.structs, &ports, &blocks);
    sv_func::emit_functions(&mut p, map, &fsm, &blocks)?;
    sv_fsm::emit_comb(&mut p, map, &fsm, &root_name, &models)?;
    sv_fsm::emit_ff(&mut p, map, &fsm, &blocks, &array_consts)?;
    // Регистровый файл: объявление входных регистров, их защёлкивание шиной (`reg_wen`)
    // и комбинационное чтение (`reg_rdata`). Выходные адресуемые порты - уже регистры
    // автомата (защёлкнуты в `always_ff` выше), их только читает мультиплексор чтения.
    // Ничего не эмитится в режиме `sv`.
    if let Some(m) = &mmio_map {
        sv_mmio::emit_register_file(&mut p, m);
    }
    sv_fsm::emit_is_done(&mut p, &root_name);
    p.down();
    p.ident("endmodule").nl();

    // Доставка предупреждений генератора. До 0064 у цели `sv` его не было вовсе -
    // `SV-009` была немой; до 0168 он вёл в `eprintln!` прямо из библиотеки, мимо
    // `--quiet` и мимо общего формата. Теперь предупреждения - часть результата.
    let warnings = fsm.warnings.borrow().clone();

    // Адаптер шины строится здесь, где регистровый файл уже собран: вторая сборка
    // `Mmio` дала бы второй источник ширин, и адаптер разошёлся бы с ядром при первой
    // же правке.
    let adapter = match bus {
        None => None,
        Some(crate::generator::Bus::Apb) => {
            let Some(m) = &mmio_map else {
                // Флаг применим только к цели `sv-mmio`: у `sv` регистрового файла нет
                // по устройству, и просить у него шину - та же ошибка, что просить её у
                // модели без адресов.
                return Err(sv_apb::refuse_wrong_target());
            };
            Some(("_apb", sv_apb::generate_apb(map.get_filename(), m)?))
        }
    };

    Ok((out, warnings, adapter))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic::tree::construct_model;

    /// Строит снимок карты из исходника Takt (по образцу `rust::tests::make_map`).
    fn make_map(src: &str, name: &str) -> SvMap {
        let (ast, _) = crate::parse(src, 0).unwrap();
        let model_rc = construct_model(&ast, None, &[]).unwrap();
        model_rc.borrow_mut().name = Some(name.to_string());
        let model = model_rc.borrow();
        SvMap::new(name, &model, true).unwrap()
    }

    fn program_of(src: &str, name: &str) -> String {
        generate_program(
            &make_map(src, name),
            false,
            &std::collections::HashMap::new(),
            None,
        )
        .unwrap()
        .0
    }

    /// Корневая модель порождает `module` с именем в `snake_case`.
    #[test]
    fn root_model_emits_module() {
        let sv = program_of("start S;", "Root");
        assert!(sv.contains("module root ("), "нет module корня:\n{sv}");
        assert!(sv.contains("endmodule"), "нет endmodule:\n{sv}");
    }

    /// Служебные порты `clk`/`rst_n` эмитятся всегда: в `.takt` их нет.
    #[test]
    fn service_ports_are_emitted() {
        let sv = program_of("start S;", "Root");
        assert!(sv.contains("input  logic clk"), "нет порта clk:\n{sv}");
        assert!(sv.contains("input  logic rst_n"), "нет порта rst_n:\n{sv}");
    }

    /// Автомат - two-process: `always_comb` (тело) + `always_ff` (латч и сброс).
    #[test]
    fn fsm_is_two_process() {
        let sv = program_of("start S;", "Root");
        assert!(sv.contains("always_comb begin"), "нет always_comb:\n{sv}");
        assert!(
            sv.contains("always_ff @(posedge clk) begin"),
            "нет always_ff по фронту clk:\n{sv}"
        );
    }

    /// **Контракт:** стартовое состояние стоит в ветви сброса, а
    /// синтетического `INIT` нет вовсе.
    ///
    /// У целей `c`/`rust` вход в стартовое состояние требуется *убирать* правкой; здесь
    /// его просто нет - регистр состояния физически не может "побыть в INIT".
    #[test]
    fn reset_branch_carries_start_state_without_init() {
        let sv = program_of("start S;", "Root");
        assert!(
            sv.contains("if (!rst_n) begin"),
            "нет ветви синхронного сброса:\n{sv}"
        );
        // Сравнение идёт по строкам кода, а не подстрокой: шапка модуля сама объясняет,
        // почему INIT-состояния нет, и упоминает его текстом. Подстрочная проверка
        // ловила бы этот комментарий - что она и сделала при первом прогоне.
        let offender = sv
            .lines()
            .map(str::trim)
            .find(|line| !line.starts_with("//") && line.contains("INIT"));
        assert!(
            offender.is_none(),
            "синтетического INIT в цели sv быть не должно (контракт 0033), найдено: {:?}",
            offender
        );
    }

    /// Сброс **синхронный**: в списке чувствительности только `posedge clk`.
    #[test]
    fn reset_is_synchronous() {
        let sv = program_of("start S;", "Root");
        assert!(
            !sv.contains("negedge rst_n"),
            "сброс обязан быть синхронным (ADR 0045, Option A):\n{sv}"
        );
    }

    /// В `always_comb` - только блокирующие присваивания.
    ///
    /// Смешение даёт другую семантику: `v := 1; w := v;` в Takt даёт `w = 1`, а с
    /// неблокирующими `w` получил бы **старое** `v`.
    #[test]
    fn comb_block_has_default_assignment() {
        let sv = program_of("start S;", "Root");
        assert!(
            sv.contains("state_next = state;"),
            "нет умолчания в always_comb — неполное присваивание даёт защёлку (LATCH):\n{sv}"
        );
    }

    /// Порты модели попадают в заголовок; `bit` -> `logic`, а не `int`.
    ///
    /// **T13/A7 - контрпример к дефекту 2:** в цели `c` один провод
    /// занимает 32 бита (`int`).
    #[test]
    fn ports_are_emitted_as_module_ports() {
        let sv = program_of(
            "in req: bit; out ack: bit; \
             start S { always { ack := req; } }",
            "Root",
        );
        assert!(
            sv.contains("input  logic req,"),
            "нет входного порта:\n{sv}"
        );
        assert!(
            sv.contains("output logic ack,"),
            "нет выходного порта:\n{sv}"
        );
        assert!(!sv.contains("int req"), "повторён дефект 0029:\n{sv}");
        // Порт здесь и есть порт: ни колбэков, ни volatile, ни AT %.
        assert!(!sv.contains("write_bit") && !sv.contains("volatile"));
    }

    /// Запись идёт в комбинационную пару, а `always_ff` её защёлкивает.
    ///
    /// Разделение и делает такт тактом: чтение - из регистра, запись - в `_next`.
    #[test]
    fn writes_go_to_next_and_are_latched() {
        let sv = program_of("out ack: bit; start S { always { ack := 1; } }", "Root");
        assert!(
            sv.contains("ack_next = 1;"),
            "запись не в пару _next:\n{sv}"
        );
        assert!(
            sv.contains("ack <= ack_next;"),
            "пара не защёлкивается в always_ff:\n{sv}"
        );
    }

    // Тест "чтение внутри такта видит запись этого же такта" переехал в
    // `takt-lang/tests/sv_tick_read_tests.rs`. Он стоял на двух выходных портах и читал
    // один из них, а чтение выхода стало ошибкой во всех позициях. Источник переписан
    // на переменную модели, но тестовый хелпер `make_map` присваивает имя модели после
    // построения дерева, поэтому переменная печатается без преисправления модели и проба
    // перестаёт быть самосогласованной. Реальный путь (`compile_to_sv`) такого
    // расхождения не даёт - там тест и живёт.

    /// **Контракт:** стартовое состояние стоит в ветви сброса.
    #[test]
    fn start_state_is_in_reset_branch() {
        let sv = program_of("start Idle { ref Done: 1 = 1; } state Done;", "Root");
        assert!(
            sv.contains("state <= ROOT_IDLE;"),
            "стартовое состояние обязано стоять в ветви сброса:\n{sv}"
        );
    }

    /// Переходы образуют цепочку `if / else if` - первый сработавший выигрывает.
    ///
    /// В C каждый переход завершается `break`; независимые `if` дали бы срабатывание
    /// всех подходящих подряд, и последний затёр бы предыдущие.
    #[test]
    fn transitions_form_if_else_chain() {
        let sv = program_of(
            "in a: bit; in b: bit; \
             start S { ref T: a = 1; ref U: b = 1; } state T; state U;",
            "Root",
        );
        assert!(
            sv.contains("else if"),
            "переходы обязаны быть цепочкой:\n{sv}"
        );
    }

    /// Терминальная ветвь `case` есть всегда: `unique case` требует полноты.
    ///
    /// Без неё `verilator -Wall` даёт `CASEINCOMPLETE`.
    #[test]
    fn case_covers_end_variant() {
        let sv = program_of("start S;", "Root");
        assert!(sv.contains("unique case (state)"), "нет unique case:\n{sv}");
        assert!(sv.contains("ROOT_END: begin end"), "нет ветви END:\n{sv}");
    }

    /// Параллельная композиция даёт каждому уровню свой регистр состояния.
    ///
    /// Регистры под-моделей независимы (): они сбрасываются одним фронтом, отсюда сдвиг
    /// = 0 на любой глубине.
    #[test]
    fn parallel_composition_gives_each_level_its_own_state_register() {
        let src = "model A { start S; } model B { start T; } start E = A | B;";
        let sv = program_of(src, "Root");
        assert!(
            sv.contains("root_a_state_e root_a_state;"),
            "нет регистра состояния под-модели A:\n{sv}"
        );
        assert!(
            sv.contains("root_b_state_e root_b_state;"),
            "нет регистра состояния под-модели B:\n{sv}"
        );
        assert!(
            sv.contains("state_e state;"),
            "нет регистра состояния корня:\n{sv}"
        );
    }

    /// **Ключевой тест:** `is_done` под-модели читает `_next`, а не регистр.
    ///
    /// В C `_is_done` вызывается после `_tick` и видит значение, которое тик только что
    /// записал. Чтение регистра дало бы значение предыдущего такта - то есть сдвиг,
    /// ровно тот, который осуждает.
    #[test]
    fn submodel_done_reads_next_not_register() {
        let src = "model A { start S; } start E = A;";
        let sv = program_of(src, "Root");
        assert!(
            sv.contains("(root_a_state_next == ROOT_A_END)"),
            "готовность под-модели обязана читать _next:\n{sv}"
        );
        assert!(
            !sv.contains("(root_a_state == ROOT_A_END)"),
            "чтение регистра дало бы значение предыдущего такта:\n{sv}"
        );
    }

    /// **:** цепочка `+` получает служебный регистр шага.
    ///
    /// Регистр `<state>_step` сброшен в `STEP_0` и **не течёт наружу**: он не выходной
    /// порт и не участвует в `is_done` (тот смотрит на регистр корня).
    #[test]
    fn sequential_composition_emits_step_register() {
        let src = "model A { start S; } model B { start S; } start P = A + B;";
        let sv = program_of(src, "Root");
        assert!(
            sv.contains("root_p_step_e root_p_step;"),
            "нет регистра шага цепочки `+`:\n{sv}"
        );
        assert!(
            sv.contains("ROOT_P_STEP_0 = 1'd0"),
            "нет варианта STEP_0 перечисления шага:\n{sv}"
        );
        assert!(
            sv.contains("root_p_step <= ROOT_P_STEP_0;"),
            "регистр шага обязан сбрасываться в STEP_0:\n{sv}"
        );
        assert!(
            !sv.contains("output logic root_p_step"),
            "служебный регистр шага не должен быть выходным портом:\n{sv}"
        );
        assert!(
            sv.contains("assign is_done = (state == ROOT_END);"),
            "is_done обязан смотреть только на регистр состояния корня:\n{sv}"
        );
    }

    /// **/02:** активен ровно один шаг; продвижение - по done на `_next`.
    #[test]
    fn sequential_composition_inlines_one_active_step() {
        let src = "model A { start S; } model B { start S; } start P = A + B;";
        let sv = program_of(src, "Root");
        assert!(
            sv.contains("unique case (root_p_step)"),
            "нет case по регистру шага:\n{sv}"
        );
        // Продвижение шага - по готовности предыдущего, читаемой из `_next`.
        assert!(
            sv.contains("(root_a_state_next == ROOT_A_END)")
                && sv.contains("root_p_step_next = ROOT_P_STEP_1;"),
            "нет продвижения шага по done под-модели A:\n{sv}"
        );
    }

    /// **0427:** вложенная `+` внутри параллельного шага переводится.
    ///
    /// Прежде здесь стоял отказ `SV-002`, и тест закреплял его как замысел:
    /// конструкцию, которую исполняют эталон, `c` и `rust`, цель не переводила вовсе.
    /// Утверждение перестало быть верным вместе с -.
    ///
    /// Вложенная цепочка адресуется **местом в дереве** (`_c0`), иначе две цепочки
    /// одного состояния делили бы регистр шага, и у неё есть собственное терминальное
    /// состояние: выхода из состояния у вложенной цепочки нет, готовность читает
    /// вмещающая параллель.
    #[test]
    fn nested_concatenation_in_parallel_is_translated() {
        let src =
            "model A { start S; } model B { start S; } model C { start S; } start P = (A + B) | C;";
        let sv = program_of(src, "Root");
        assert!(
            sv.contains("unique case (root_p_step_c0)"),
            "нет case по регистру шага вложенной цепочки:\n{sv}"
        );
        assert!(
            sv.contains("root_p_step_c0_next = ROOT_P_STEP_C0_DONE;"),
            "последний шаг обязан уводить цепочку в её терминальное состояние:\n{sv}"
        );
        assert!(
            sv.contains(
                "(root_p_step_c0_next == ROOT_P_STEP_C0_DONE) && (root_c_state_next == ROOT_C_END)"
            ),
            "завершение состояния — по цепочке И параллельной ветви:\n{sv}"
        );
    }

    /// Вывод **воспроизводим**: одна модель - один и тот же текст.
    #[test]
    fn output_is_deterministic() {
        let src = "model A { start S; } model B { start T; } start E = A | B;";
        let first = program_of(src, "Root");
        for i in 1..8 {
            assert_eq!(
                first,
                program_of(src, "Root"),
                "прогон {i} дал другой вывод — вернулся недетерминизм порядка"
            );
        }
    }
}
