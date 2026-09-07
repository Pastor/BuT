//! Генератор `no_std` Rust из семантического дерева Takt.
//!
//! Пятый целевой язык. Архитектурное решение - (Option A по обеим развилкам, профиль
//! **`no_std`-прошивка**, форма вывода - **один `.rs`-файл**.
//!
//! ## Форма вывода: почему в файле нет `#![no_std]`
//!
//! требовал начинать модуль с `#![no_std]`. Проба при реализации
//! **опровергла** это требование в выбранной форме вывода: `#![no_std]` -
//! атрибут **корня крейта**, и в файле, подключённом через `mod`, он даёт
//! предупреждение "the `#![no_std]` attribute can only be used at the crate
//! root". То есть модуль с `#![no_std]` ломал бы сборку пользователя под
//! `-D warnings` - ровно тем, чем цель хвалится.
//!
//! `no_std` - свойство **крейта**, а не модуля. Поэтому модуль просто не обращается к
//! `std`, а `no_std`-совместимость **доказывается проверкой**: он оборачивает вывод в
//! корень крейта с `#![no_std]` и компилирует. Это строже проверки отдельного файла и
//! заодно подтверждает обещание "работает и на хосте" (проверено: тот же модуль
//! собирается и из `std`-крейта).
//!
//! ## Состав модуля
//!
//! `rust_name` (имена, `RS-004`/`RS-005`) · `rust_type` (типы, `repr` по диапазону) ·
//! `rust_port` (порты -> HAL-трейт) · `rust_expr` (выражения и условия) · `rust_stmt`
//! (операторы) · `rust_decl` (объявления) · `rust_func` (функции) · `rust_model`
//! (автомат) · `rust_map` (снимок карты).

mod rust_assign;
mod rust_assigned;
mod rust_bit;
mod rust_blocks;
mod rust_byref;
mod rust_chain;
mod rust_coerce;
mod rust_cond;
mod rust_ctx;
mod rust_decl;
mod rust_every;
mod rust_expr;
mod rust_fields;
mod rust_fixed;
mod rust_func;
mod rust_live;
mod rust_map;
mod rust_model;
mod rust_modulo;
mod rust_name;
// Носитель "что функции нужно сверх параметров" переиспользует цель `c`: вопрос
// семантический, и второго знания о нём быть не должно - /0193/0195.
mod rust_match;
pub(crate) mod rust_needs;
mod rust_port;
mod rust_port_init;
mod rust_shared;
mod rust_shift;
mod rust_stmt;
mod rust_struct;
mod rust_table;
mod rust_text;
mod rust_tick;
mod rust_time;
mod rust_type;
mod rust_unused;

use crate::diagnostics::{Diagnostic, Location};
use crate::generator::Generator as AsGenerator;
use crate::generator::header::{CommentStyle, file_header};
use crate::generator::indent::Printer;
use crate::generator::{GenerateOptions, GeneratedFile, Output};
use crate::semantic::ModelNode;
use crate::semantic::minimap::{Element, Name};
use crate::semantic::naming::normalize_lowercase_snakecase;
use rust_map::RustMap;
use std::cell::RefCell;
use std::rc::Rc;

/// Размер одного уровня отступа в порождаемом Rust (конвенция rustfmt).
const INDENT: usize = 4;

/// Генератор Rust для модели Takt.
pub struct Generator {}

impl AsGenerator for Generator {
    fn generate_texts(
        &self,
        model: &ModelNode,
        options: &GenerateOptions,
    ) -> Result<Output, Diagnostic> {
        // Молчаливое игнорирование флага недопустимо: пользователь решил бы, что
        // получил f32, тогда как `Rational` -> f64 - решение.
        rust_type::reject_float_width(options.float_width)?;

        // Профиль времени: `clock` модели - контракт, флаг обязан подтвердить. Единый
        // чекпойнт-энфорсмент: несовпадение -> `SE-069`/`SE-070` из `?`, покрывает все
        // пути генерации rust.
        let profile = crate::semantic::duration::resolve_profile(model.clock_hz, options.tick_hz)?;
        let map = RustMap::new(
            &normalize_lowercase_snakecase(model.name().to_string()),
            model,
            options.guard_enable,
        )?
        .with_time_profile(profile)
        .with_fsm(options.fsm)
        .with_comments(options.comments.clone());
        let (program, warnings) = generate_program(&map)?;
        let filename = map.get_filename();
        Ok(Output {
            files: vec![GeneratedFile {
                name: filename.to_owned() + ".rs",
                text: program,
            }],
            warnings,
        })
    }

    fn write_failure(&self, error: &std::io::Error) -> Diagnostic {
        Diagnostic::error(Location::Codegen, format!("{error}")).with_code("RS-001")
    }
}

/// Собирает текст модуля Rust из снимка модели.
///
/// Возвращает текст **и предупреждения цели** (`RS-010`): печатать их генератор не
/// вправе - доставку ведёт вызывающий.
fn generate_program(map: &RustMap) -> Result<(String, Vec<Diagnostic>), Diagnostic> {
    let Element::Model { .. } = map.model() else {
        return Err(Diagnostic::error(
            Location::Codegen,
            "Корневой элемент карты не является моделью".to_string(),
        )
        .with_code("RS-012"));
    };

    // Порядок объявлений в Rust не значим, поэтому топологической сортировки - в
    // отличие от целей `c` и `st` - не требуется. Порядок задан `BTreeMap` карты:
    // детерминизм достаётся даром.
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
    let root_name = map.root_name();
    let root = map.root_model_node().ok_or_else(|| {
        Diagnostic::error(
            Location::Codegen,
            format!("Корневая модель '{}' отсутствует в снимке карты", root_name),
        )
        .with_code("RS-012")
    })?;
    blocks.push((root_name.clone(), root));

    let mut ports = rust_decl::collect_ports(map, &blocks)?;
    // `debug` в профиле `no_std` printf не имеет. Решение (а): метод трейта. Профиль
    // `no_std` не означает "без вывода" - он означает "вывод решает пользователь". Тихо
    // отбросить нельзя: ровно этот дефект закрыла.
    ports.needs_debug = map.usage().functions.contains("debug");
    // Источник времени `now_ms` (профиль "часы") - метод трейта без тела, по образцу
    // `debug`: `no_std`-часов нет, реализует пользователь.
    ports.needs_now_ms = map
        .root_model_node()
        .is_some_and(|m| rust_time::needs_now_ms(map, &m.borrow()));

    let mut out = String::new();
    let mut p = Printer::new(INDENT, &mut out);
    let mut warnings: Vec<Diagnostic> = Vec::new();

    for line in file_header("Rust (no_std profile)", CommentStyle::Slashes) {
        p.ident(&line).nl();
    }
    p.nl();
    // Не декорация: делает "в порождённом коде нет unsafe" свойством, проверяемым
    // Компилятором, а не grep'ом.
    p.ident("#![forbid(unsafe_code)]").nl().nl();

    rust_decl::emit_structs(&mut p, &blocks)?;
    rust_decl::emit_enums(&mut p, &blocks)?;
    rust_decl::emit_constants(&mut p, map, &blocks)?;
    rust_decl::emit_hal(&mut p, &ports)?;
    rust_func::emit_functions(&mut p, map, &blocks, &mut warnings)?;

    for (name, model) in &blocks {
        let is_root = name.unique() == root_name.unique();
        rust_model::emit_model(
            &mut p,
            map,
            name,
            &model.borrow(),
            is_root,
            &ports,
            &mut warnings,
        )?;
    }

    Ok((out, warnings))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic::tree::construct_model;

    /// Строит снимок карты из исходника Takt (по образцу `st::tests::make_map`).
    fn make_map(src: &str, name: &str) -> RustMap {
        let (ast, _) = crate::parse(src, 0).unwrap();
        let model_rc = construct_model(&ast, None, &[]).unwrap();
        model_rc.borrow_mut().name = Some(name.to_string());
        let model = model_rc.borrow();
        RustMap::new(name, &model, true).unwrap()
    }

    fn program_of(src: &str, name: &str) -> String {
        generate_program(&make_map(src, name)).unwrap().0
    }

    /// Вырезает тело функции `fn <name>` из порождённого текста (для точечных проверок
    /// свёртки хвоста).
    fn fn_body(rs: &str, name: &str) -> String {
        let start = rs
            .find(&format!("fn {}(", name))
            .unwrap_or_else(|| panic!("нет функции '{name}':\n{rs}"));
        let tail = &rs[start..];
        // Тело функции завершается строкой из одной `}` на нулевом отступе.
        let end = tail.find("\n}\n").map(|i| i + 3).unwrap_or(tail.len());
        tail[..end].to_string()
    }

    /// **0058: хвостовой `if/else` со сворачиваемыми ветвями -> выражение.**
    ///
    /// `if a > b { return a; } else { return b; }` печатается как `if a > b { a } else
    /// { b }` - `return` исчезает, `needless_return` не возникает.
    #[test]
    fn tail_if_else_folds_to_expression() {
        let src = "fn pick(a: u8, b: u8) -> u8 { if a > b { return a; } else { return b; } } \
                   var v: u8 := 0; start S { always { v := pick(1, 2); } }";
        let body = fn_body(&program_of(src, "Root"), "pick");
        assert!(
            !body.contains("return"),
            "хвостовой if/else обязан свернуться — `return` исчезает:\n{body}"
        );
        assert!(
            body.contains("} else {"),
            "ветки обязаны сохраниться как if/else:\n{body}"
        );
    }

    /// **0058: цепочка `if / else if / else` с `return` сворачивается.**
    #[test]
    fn tail_else_if_chain_folds() {
        let src = "fn grade(a: u8) -> u8 { \
                   if a > 10 { return 3; } else if a > 5 { return 2; } else { return 1; } } \
                   var v: u8 := 0; start S { always { v := grade(7); } }";
        let body = fn_body(&program_of(src, "Root"), "grade");
        assert!(
            !body.contains("return"),
            "цепочка else if обязана свернуться целиком:\n{body}"
        );
    }

    /// **0058: несворачиваемый хвост печатается как сегодня (`return`
    /// остаётся).**
    ///
    /// `if` без `else` в НЕхвостовой позиции + завершающий `return` - только последний
    /// сворачивается; ранний `if`-выход сохраняет `return` ().
    #[test]
    fn non_tail_if_keeps_return() {
        let src = "fn clip(a: u8) -> u8 { if a > 10 { return 3; } return 1; } \
                   var v: u8 := 0; start S { always { v := clip(20); } }";
        let body = fn_body(&program_of(src, "Root"), "clip");
        assert!(
            body.contains("return 3;"),
            "ранний if-выход обязан сохранить `return`:\n{body}"
        );
        assert!(
            body.trim_end().ends_with("1\n}") || body.contains("\n    1\n"),
            "завершающий `return 1;` обязан свернуться в `1`:\n{body}"
        );
    }

    /// **0059: переменная корня, не нужная ни одной под-модели, в
    /// `Shared` не входит - остаётся прямым полем корня.**
    ///
    /// Иначе имя типа лжёт ("общая"), а поле ловит `dead_code`.
    #[test]
    fn shared_union_excludes_variables_no_submodel_needs() {
        let src = "var for_sub: u8 := 0; var only_root: u8 := 0; \
                   model M { start S { always { for_sub := 1; } } } \
                   start Root = M { always { only_root := 2; } }";
        let map = make_map(src, "Root");
        let union: Vec<String> = crate::generator::rust::rust_shared::shared_union(&map)
            .into_iter()
            .map(|(n, _)| n)
            .collect();
        assert!(
            union.contains(&"for_sub".to_string()),
            "for_sub нужна под-модели M → обязана быть в Shared: {union:?}"
        );
        assert!(
            !union.contains(&"only_root".to_string()),
            "only_root не нужна ни одной под-модели → в Shared не входит: {union:?}"
        );
    }

    /// **0059: модель без под-моделей структуры `Shared` не
    /// получает** (иначе `dead_code` на неиспользуемом типе).
    #[test]
    fn model_without_submodels_has_no_shared_struct() {
        let rs = program_of("var x: u8 := 0; start S { always { x := 1; } }", "Root");
        assert!(
            !rs.contains("struct RootShared"),
            "модель без под-моделей не должна иметь Shared:\n{rs}"
        );
    }

    /// **0059: такт под-модели не превышает трёх параметров** (`self` +
    /// `&mut Shared?` + `&mut H?`) - заглушки `#[allow]` больше нет.
    #[test]
    fn submodel_tick_has_at_most_three_params() {
        let src = "var a: u8 := 0; var b: u8 := 0; var c: u8 := 0; var d: u8 := 0; \
                   model M { start S { always { a := 1; b := 1; c := 1; d := 1; } } } \
                   start Root = M;";
        let rs = program_of(src, "Root");
        assert!(
            !rs.contains("allow(clippy::too_many_arguments)"),
            "заглушка линта не должна эмититься (фича 0059):\n{rs}"
        );
        // Под-модель получает общие переменные одним параметром `&mut RootShared`.
        assert!(
            rs.contains("shared: &mut RootShared"),
            "под-модель обязана принимать &mut Shared:\n{rs}"
        );
    }

    /// Корневая модель порождает `struct` и `impl`.
    #[test]
    fn root_model_emits_struct_and_impl() {
        let rs = program_of("start S;", "Root");
        assert!(rs.contains("pub struct Root"), "нет struct корня:\n{rs}");
        assert!(rs.contains("impl Root"), "нет impl корня:\n{rs}");
    }

    /// **Тест против дефекта 0026.**
    ///
    /// У цели `c` модель без под-моделей не получает typedef корня, и порождённый C не
    /// компилируется (8 ошибок `cc`) - простейший класс моделей. Здесь тип корня -
    /// обычная `struct`, отдельного объявления не требующая, поэтому класс дефекта не
    /// воспроизводится конструктивно.
    #[test]
    fn model_without_submodels_emits_root_type() {
        let rs = program_of("start S; state T;", "Root");
        assert!(
            rs.contains("pub struct Root {"),
            "модель без под-моделей обязана дать тип корня (дефект 0026):\n{rs}"
        );
    }

    /// Состояния дают `enum`, а не целочисленные константы.
    #[test]
    fn states_emit_enum_not_integer_constants() {
        let rs = program_of("start Idle { ref Done: 1 = 1; } state Done;", "Root");
        assert!(rs.contains("enum RootState {"), "нет enum состояний:\n{rs}");
        assert!(rs.contains("Idle,"), "нет варианта Idle:\n{rs}");
        assert!(rs.contains("Done,"), "нет варианта Done:\n{rs}");
    }

    /// **Контракт:** вход в стартовое состояние диспетчеризуется до `match`.
    ///
    /// Проверяется порядок в тексте: `if self.state == ...::Init` обязан стоять раньше
    /// `match self.state`. Тело стартового состояния исполняется в том же такте - иначе
    /// трасса разъедется с симулятором и с целью `c`.
    #[test]
    fn init_is_dispatched_before_match() {
        let rs = program_of("start S;", "Root");
        let init = rs
            .find("if self.state == RootState::Init")
            .expect("нет диспетчера Init");
        let switch = rs.find("match self.state").expect("нет match");
        assert!(
            init < switch,
            "Init обязан диспетчеризоваться ДО match (контракт 0033):\n{rs}"
        );
    }

    /// Вывод **воспроизводим**: одна модель - один и тот же текст.
    ///
    /// Тест детерминизма. Карта строится заново на каждой итерации - иначе тест
    /// проверял бы кэш, а не обход.
    #[test]
    fn output_is_deterministic() {
        let src = "model A { start S; } model B { start T; } model C { start U; } \
                   start E = A | B | C;";
        let first = program_of(src, "Root");
        for i in 1..8 {
            assert_eq!(
                first,
                program_of(src, "Root"),
                "прогон {i} дал другой вывод — вернулся недетерминизм порядка"
            );
        }
    }

    /// Параллельная композиция даёт по `struct` на каждую под-модель.
    #[test]
    fn parallel_composition_emits_struct_per_submodel() {
        let src = "model A { start S; } model B { start T; } start E = A | B;";
        let rs = program_of(src, "Root");
        assert!(rs.contains("pub struct RootA"), "нет struct A:\n{rs}");
        assert!(rs.contains("pub struct RootB"), "нет struct B:\n{rs}");
    }

    /// **A12/R10:** в порождённом коде нет `unsafe`, и это следит компилятор.
    #[test]
    fn generated_code_forbids_unsafe() {
        let rs = program_of("start S;", "Root");
        assert!(
            rs.contains("#![forbid(unsafe_code)]"),
            "модуль обязан запрещать unsafe:\n{rs}"
        );
        assert!(!rs.contains("unsafe {"), "unsafe-блок в выводе:\n{rs}");
    }

    /// `#![no_std]` в модуле **не эмитится**.
    ///
    /// Тест против возврата к букве R2: атрибут допустим только в корне крейта, и в
    /// файле, подключаемом через `mod`, он даёт предупреждение - то есть ломал бы
    /// сборку пользователя под `-D warnings`.
    #[test]
    fn no_std_attribute_is_not_emitted() {
        let rs = program_of("start S;", "Root");
        // Сравнение идёт по строкам кода, а не подстрокой: шапка модуля сама объясняет,
        // почему атрибута нет, и упоминает его текстом. Подстрочная проверка ловила бы
        // этот комментарий - что она и сделала при первом прогоне.
        let offender = rs
            .lines()
            .map(str::trim)
            .find(|line| !line.starts_with("//") && line.contains("#![no_std]"));
        assert!(
            offender.is_none(),
            "#![no_std] в модуле даёт предупреждение 'can only be used at the \
             crate root' — no_std обязан жить в корне крейта пользователя, \
             найдено: {:?}",
            offender
        );
    }

    /// Модель без портов не эмитит ни трейта `Hal`, ни параметра типа `H`.
    #[test]
    fn model_without_ports_emits_no_hal() {
        let rs = program_of("start S;", "Root");
        assert!(
            !rs.contains("trait Hal"),
            "трейт без портов не нужен:\n{rs}"
        );
        assert!(
            !rs.contains("<H: Hal>"),
            "параметр типа без портов не нужен:\n{rs}"
        );
    }
}
