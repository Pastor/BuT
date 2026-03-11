pub mod behavior;
pub mod c;
pub mod condition;
pub mod lc3;
pub mod ltl;
pub mod st;
pub mod thumb;
pub mod verilog;

use std::collections::HashMap;

use but_grammar::ast::{SourceUnit, SourceUnitPart, Type, VariableDefinition};

pub use behavior::{find_behavior, find_end_property, find_terminal_states, BehaviorKind};
pub use c::{generate_c_all, generate_c_all_named, generate_c_header, generate_c_source};
pub use lc3::{generate_lc3, generate_lc3_all};
pub use st::{generate_st_all, generate_st_decl, generate_st_program};
pub use thumb::{generate_thumb, generate_thumb_all};
pub use verilog::{generate_verilog, generate_verilog_all};

/// Стиль отступа при генерации кода.
///
/// Используется всеми бэкендами (C, ST, Verilog) для форматирования отступов.
/// По умолчанию — четыре пробела на уровень вложенности.
#[derive(Debug, Clone, PartialEq)]
pub enum IndentStyle {
    /// Отступ пробелами: задаётся количество пробелов на один уровень.
    Spaces(usize),
    /// Отступ символом табуляции: один `\t` на уровень.
    Tab,
}

impl IndentStyle {
    /// Сгенерировать строку отступа для заданного уровня вложенности.
    ///
    /// Например, `Spaces(4).level(2)` вернёт 8 пробелов.
    pub fn level(&self, n: usize) -> String {
        match self {
            IndentStyle::Spaces(size) => " ".repeat(size * n),
            IndentStyle::Tab => "\t".repeat(n),
        }
    }

    /// Единица отступа (один уровень).
    pub fn unit(&self) -> String {
        self.level(1)
    }
}

impl Default for IndentStyle {
    fn default() -> Self {
        IndentStyle::Spaces(4)
    }
}

/// Контекст для генераторов кода с глобальными объявлениями.
#[derive(Debug, Default)]
pub struct CodegenContext {
    /// Глобальные определения переменных/портов из исходного файла.
    pub global_vars: Vec<Box<VariableDefinition>>,
    /// Таблица псевдонимов типов: имя псевдонима → определение типа.
    pub type_aliases: HashMap<String, Type>,
    /// Стиль отступа для всех генераторов кода.
    pub indent: IndentStyle,
}

impl CodegenContext {
    pub fn new() -> Self {
        Self::default()
    }

    /// Построить контекст из SourceUnit, собрав глобальные объявления и псевдонимы типов.
    pub fn from_source(source: &SourceUnit) -> Self {
        let mut ctx = Self::new();
        for part in &source.0 {
            match part {
                SourceUnitPart::VariableDefinition(vd) => ctx.global_vars.push(vd.clone()),
                SourceUnitPart::TypeDefinition(td) => {
                    ctx.type_aliases.insert(td.name.name.clone(), td.ty.clone());
                }
                _ => {}
            }
        }
        ctx
    }

    /// Построить контекст с заданным стилем отступа.
    pub fn from_source_with_indent(source: &SourceUnit, indent: IndentStyle) -> Self {
        let mut ctx = Self::from_source(source);
        ctx.indent = indent;
        ctx
    }
}

/// Сгенерировать все выходные данные для всех моделей из SourceUnit (единый файл на язык).
///
/// Используйте [`AllOutput::generate_named`] для задания базового имени выходных файлов.
pub struct AllOutput {
    /// (заголовок, исходник) — один `.h` + один `.c` для всех моделей
    pub c: (String, String),
    /// Единый Verilog-файл со всеми модулями
    pub verilog: String,
    /// (декларации, программа) — один `.FB.DECL.st` + один `.FB.PRGS.st` для всех моделей
    pub st: (String, String),
    /// Единый ассемблерный файл LC-3
    pub lc3: String,
    /// Единый ассемблерный файл ARM Thumb
    pub thumb: String,
}

impl AllOutput {
    /// Сгенерировать все выходы с базовым именем «model» и настройками по умолчанию (4 пробела).
    pub fn generate(source: &SourceUnit) -> Self {
        Self::generate_named(source, "model")
    }

    /// Сгенерировать все выходы с заданным базовым именем файла.
    pub fn generate_named(source: &SourceUnit, base_name: &str) -> Self {
        let ctx = CodegenContext::from_source(source);
        Self::generate_with_ctx(source, base_name, &ctx)
    }

    /// Сгенерировать все выходы с заданным контекстом (включая стиль отступа).
    pub fn generate_with_ctx(source: &SourceUnit, base_name: &str, ctx: &CodegenContext) -> Self {
        Self {
            c: generate_c_all_named(source, base_name, ctx),
            verilog: generate_verilog_all(source, ctx),
            st: generate_st_all(source, ctx),
            lc3: generate_lc3_all(source, ctx),
            thumb: generate_thumb_all(source, ctx),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// BuT-источник модели Delay для тестов
    const DELAY_SRC: &str = r#"
model Delay {
    state One {
        ref None: input = 0;
        ref One : input = 1;
        enter -> { output = 1; }
    }
    state None {
        ref One : input = 1;
        ref None: input = 0;
        enter -> { output = 0; }
    }
    state End { }
    start -> None;
}
port input : bit = 0x01;
port output: bit = 0x02;
"#;

    /// Вспомогательная функция: разобрать источник и вернуть SourceUnit
    fn parse_delay() -> SourceUnit {
        but_grammar::parse(DELAY_SRC, 0)
            .expect("Исходный код Delay должен разбираться без ошибок")
            .0
    }

    // Источник с псевдонимами типов
    const TYPE_ALIAS_SRC: &str = r#"
type MyByte = u8;
type Counter = u32;
type Flag = bool;
type Nested = MyByte;
model M { start -> M; }
"#;

    fn parse_type_aliases() -> SourceUnit {
        but_grammar::parse(TYPE_ALIAS_SRC, 0)
            .expect("Исходный код с псевдонимами должен разбираться без ошибок")
            .0
    }

    // ===== CodegenContext::from_source =====

    #[test]
    fn from_source_собирает_глобальные_переменные() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source(&src);
        // В источнике объявлены два порта: input и output
        assert_eq!(ctx.global_vars.len(), 2);
    }

    #[test]
    fn from_source_пустой_источник_нет_переменных() {
        let (src, _) = but_grammar::parse("model Empty { start -> Empty; }", 0)
            .expect("Парсинг пустой модели");
        let ctx = CodegenContext::from_source(&src);
        assert_eq!(ctx.global_vars.len(), 0);
    }

    #[test]
    fn from_source_собирает_псевдонимы_типов() {
        let src = parse_type_aliases();
        let ctx = CodegenContext::from_source(&src);
        assert!(ctx.type_aliases.contains_key("MyByte"));
        assert!(ctx.type_aliases.contains_key("Counter"));
        assert!(ctx.type_aliases.contains_key("Flag"));
        assert!(ctx.type_aliases.contains_key("Nested"));
    }

    #[test]
    fn from_source_пустой_источник_нет_псевдонимов() {
        let (src, _) = but_grammar::parse("model Empty { start -> Empty; }", 0)
            .expect("Парсинг пустой модели");
        let ctx = CodegenContext::from_source(&src);
        assert!(ctx.type_aliases.is_empty());
    }

    #[test]
    fn from_source_имена_портов_корректны() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source(&src);
        let names: Vec<String> = ctx
            .global_vars
            .iter()
            .filter_map(|v| v.name.as_ref().map(|n| n.name.clone()))
            .collect();
        assert!(names.contains(&"input".to_string()));
        assert!(names.contains(&"output".to_string()));
    }

    // ===== AllOutput::generate =====

    #[test]
    fn all_output_generate_возвращает_данные_для_delay() {
        let src = parse_delay();
        let out = AllOutput::generate(&src);
        // Все выходы непусты
        assert!(!out.c.0.is_empty(), "C-заголовок должен быть непустым");
        assert!(!out.c.1.is_empty(), "C-исходник должен быть непустым");
        assert!(!out.verilog.is_empty(), "Verilog должен быть непустым");
        assert!(!out.st.0.is_empty() || !out.st.1.is_empty(), "ST должен быть непустым");
        assert!(!out.lc3.is_empty(), "LC-3 должен быть непустым");
        assert!(!out.thumb.is_empty(), "Thumb должен быть непустым");
    }

    #[test]
    fn all_output_содержит_имя_модели_delay() {
        let src = parse_delay();
        let out = AllOutput::generate(&src);
        assert!(out.c.0.contains("DELAY") || out.c.0.contains("delay"),
            "C-заголовок должен содержать имя модели Delay: {}", out.c.0);
        assert!(out.verilog.contains("delay") || out.verilog.contains("Delay"),
            "Verilog должен содержать имя модели: {}", out.verilog);
        assert!(out.st.0.contains("DELAY") || out.st.1.contains("DELAY"),
            "ST должен содержать имя модели DELAY");
    }

    // ===== generate_c_all =====

    #[test]
    fn generate_c_all_создаёт_заголовок_и_исходник() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source(&src);
        let (header, source) = generate_c_all(&src, &ctx);
        assert!(header.contains("DELAY") || header.contains("delay"),
            "Заголовок должен содержать имя модели: {}", header);
        assert!(!source.is_empty(), "Исходник не должен быть пустым");
    }

    // ===== generate_verilog_all =====

    #[test]
    fn generate_verilog_all_создаёт_модуль() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source(&src);
        let verilog = generate_verilog_all(&src, &ctx);
        assert!(!verilog.is_empty());
        assert!(verilog.contains("module"), "Verilog должен содержать 'module': {}", verilog);
        assert!(verilog.contains("delay"), "Verilog должен содержать имя модели: {}", verilog);
    }

    // ===== generate_st_all =====

    #[test]
    fn generate_st_all_создаёт_объявление_и_программу() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source(&src);
        let (decl, prog) = generate_st_all(&src, &ctx);
        assert!(!decl.is_empty() || !prog.is_empty(), "ST должен иметь непустой вывод");
        assert!(decl.contains("DELAY") || prog.contains("DELAY"), "ST должен содержать DELAY");
    }

    // ===== generate_lc3_all =====

    #[test]
    fn generate_lc3_all_создаёт_ассемблер() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source(&src);
        let asm = generate_lc3_all(&src, &ctx);
        assert!(!asm.is_empty(), "LC-3 ассемблер не должен быть пустым");
    }

    // ===== generate_thumb_all =====

    #[test]
    fn generate_thumb_all_создаёт_ассемблер() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source(&src);
        let asm = generate_thumb_all(&src, &ctx);
        assert!(!asm.is_empty(), "Thumb ассемблер не должен быть пустым");
    }

    // ===== Интеграционные тесты: псевдонимы типов в кодогенерации =====

    const TYPE_ALIAS_PORTS_SRC: &str = r#"
type Byte    = u8;
type Counter = u32;
type Flag    = bool;
type Speed   = f64;
type NestedAlias = Byte;
port sensor  : Byte        = 0x00;
port count   : Counter     = 0;
port alarm   : Flag        = false;
port rate    : Speed       = 0.0;
port raw     : NestedAlias = 0x00;
model Sensor {
    state Idle { }
    start -> Idle;
}
"#;

    fn parse_type_alias_ports() -> SourceUnit {
        but_grammar::parse(TYPE_ALIAS_PORTS_SRC, 0)
            .expect("Парсинг источника с псевдонимами типов")
            .0
    }

    #[test]
    fn c_header_разворачивает_псевдоним_byte_в_uint8_t() {
        let src = parse_type_alias_ports();
        let ctx = CodegenContext::from_source(&src);
        let (header, _) = generate_c_all(&src, &ctx);
        assert!(header.contains("uint8_t sensor"),
            "Ожидается uint8_t для Byte, заголовок:\n{}", header);
    }

    #[test]
    fn c_header_разворачивает_псевдоним_counter_в_uint32_t() {
        let src = parse_type_alias_ports();
        let ctx = CodegenContext::from_source(&src);
        let (header, _) = generate_c_all(&src, &ctx);
        assert!(header.contains("uint32_t count"),
            "Ожидается uint32_t для Counter, заголовок:\n{}", header);
    }

    #[test]
    fn c_header_разворачивает_псевдоним_flag_в_int() {
        let src = parse_type_alias_ports();
        let ctx = CodegenContext::from_source(&src);
        let (header, _) = generate_c_all(&src, &ctx);
        assert!(header.contains("int alarm"),
            "Ожидается int (bool→int) для Flag, заголовок:\n{}", header);
    }

    #[test]
    fn c_header_разворачивает_псевдоним_speed_в_double() {
        let src = parse_type_alias_ports();
        let ctx = CodegenContext::from_source(&src);
        let (header, _) = generate_c_all(&src, &ctx);
        assert!(header.contains("double rate"),
            "Ожидается double для Speed (f64), заголовок:\n{}", header);
    }

    #[test]
    fn c_header_разворачивает_вложенный_псевдоним_в_uint8_t() {
        let src = parse_type_alias_ports();
        let ctx = CodegenContext::from_source(&src);
        let (header, _) = generate_c_all(&src, &ctx);
        assert!(header.contains("uint8_t raw"),
            "Ожидается uint8_t для NestedAlias→Byte→u8, заголовок:\n{}", header);
    }

    #[test]
    fn st_decl_разворачивает_псевдоним_counter_в_dword() {
        let src = parse_type_alias_ports();
        let ctx = CodegenContext::from_source(&src);
        let (decl, _) = generate_st_all(&src, &ctx);
        assert!(decl.contains("DWORD"),
            "Ожидается DWORD для Counter→u32, объявление:\n{}", decl);
    }

    #[test]
    fn st_decl_разворачивает_псевдоним_flag_в_bool() {
        let src = parse_type_alias_ports();
        let ctx = CodegenContext::from_source(&src);
        let (decl, _) = generate_st_all(&src, &ctx);
        assert!(decl.contains("BOOL"),
            "Ожидается BOOL для Flag→bool, объявление:\n{}", decl);
    }

    // ===== Интеграционные тесты: система типов (bit и float как базовые) =====

    const STD_TYPES_SRC: &str = r#"
type u8   = [8:   bit];
type u16  = [16:  bit];
type u32  = [32:  bit];
type u64  = [64:  bit];
type u128 = [128: bit];
type bool = bit;
type Counter = u32;
port x8   : u8    = 0;
port x16  : u16   = 0;
port x32  : u32   = 0;
port x64  : u64   = 0;
port x128 : u128  = 0;
port flag : bool  = false;
port cnt  : Counter = 0;
model Test { state A {} start -> A; }
"#;

    fn parse_std_types() -> SourceUnit {
        but_grammar::parse(STD_TYPES_SRC, 0)
            .expect("Исходный код со std-типами должен разбираться без ошибок")
            .0
    }

    #[test]
    fn c_header_u8_через_массив_бит_даёт_uint8_t() {
        let src = parse_std_types();
        let ctx = CodegenContext::from_source(&src);
        let (header, _) = generate_c_all(&src, &ctx);
        assert!(header.contains("uint8_t x8"),
            "Ожидается uint8_t для u8=[8:bit], заголовок:\n{}", header);
    }

    #[test]
    fn c_header_u16_через_массив_бит_даёт_uint16_t() {
        let src = parse_std_types();
        let ctx = CodegenContext::from_source(&src);
        let (header, _) = generate_c_all(&src, &ctx);
        assert!(header.contains("uint16_t x16"),
            "Ожидается uint16_t для u16=[16:bit], заголовок:\n{}", header);
    }

    #[test]
    fn c_header_u32_через_массив_бит_даёт_uint32_t() {
        let src = parse_std_types();
        let ctx = CodegenContext::from_source(&src);
        let (header, _) = generate_c_all(&src, &ctx);
        assert!(header.contains("uint32_t x32"),
            "Ожидается uint32_t для u32=[32:bit], заголовок:\n{}", header);
    }

    #[test]
    fn c_header_u64_через_массив_бит_даёт_uint64_t() {
        let src = parse_std_types();
        let ctx = CodegenContext::from_source(&src);
        let (header, _) = generate_c_all(&src, &ctx);
        assert!(header.contains("uint64_t x64"),
            "Ожидается uint64_t для u64=[64:bit], заголовок:\n{}", header);
    }

    #[test]
    fn c_header_u128_через_массив_бит_даёт_uint64_t_массив() {
        let src = parse_std_types();
        let ctx = CodegenContext::from_source(&src);
        let (header, _) = generate_c_all(&src, &ctx);
        assert!(header.contains("uint64_t[2]"),
            "Ожидается uint64_t[2] для u128=[128:bit], заголовок:\n{}", header);
    }

    #[test]
    fn c_header_bool_через_бит_даёт_uint8_t() {
        let src = parse_std_types();
        let ctx = CodegenContext::from_source(&src);
        let (header, _) = generate_c_all(&src, &ctx);
        assert!(header.contains("uint8_t flag"),
            "Ожидается uint8_t для bool=bit, заголовок:\n{}", header);
    }

    #[test]
    fn c_header_counter_через_цепочку_псевдонимов() {
        let src = parse_std_types();
        let ctx = CodegenContext::from_source(&src);
        let (header, _) = generate_c_all(&src, &ctx);
        assert!(header.contains("uint32_t cnt"),
            "Ожидается uint32_t для Counter→u32→[32:bit], заголовок:\n{}", header);
    }

    #[test]
    fn st_decl_u8_через_массив_бит_даёт_byte() {
        let src = parse_std_types();
        let ctx = CodegenContext::from_source(&src);
        let (decl, _) = generate_st_all(&src, &ctx);
        assert!(decl.contains("BYTE"),
            "Ожидается BYTE для u8=[8:bit], объявление:\n{}", decl);
    }

    #[test]
    fn st_decl_u32_через_массив_бит_даёт_dword() {
        let src = parse_std_types();
        let ctx = CodegenContext::from_source(&src);
        let (decl, _) = generate_st_all(&src, &ctx);
        assert!(decl.contains("DWORD"),
            "Ожидается DWORD для u32=[32:bit], объявление:\n{}", decl);
    }

    #[test]
    fn st_decl_u128_через_массив_бит_даёт_массив_lword() {
        let src = parse_std_types();
        let ctx = CodegenContext::from_source(&src);
        let (decl, _) = generate_st_all(&src, &ctx);
        assert!(decl.contains("ARRAY [0..1] OF LWORD"),
            "Ожидается ARRAY [0..1] OF LWORD для u128=[128:bit], объявление:\n{}", decl);
    }

    // ===== IndentStyle =====

    #[test]
    fn indent_style_spaces_уровень_0_пустая_строка() {
        assert_eq!(IndentStyle::Spaces(4).level(0), "");
        assert_eq!(IndentStyle::Spaces(2).level(0), "");
        assert_eq!(IndentStyle::Tab.level(0), "");
    }

    #[test]
    fn indent_style_spaces_4_уровень_1() {
        assert_eq!(IndentStyle::Spaces(4).level(1), "    ");
    }

    #[test]
    fn indent_style_spaces_2_уровень_1() {
        assert_eq!(IndentStyle::Spaces(2).level(1), "  ");
    }

    #[test]
    fn indent_style_spaces_2_уровень_3() {
        assert_eq!(IndentStyle::Spaces(2).level(3), "      ");
    }

    #[test]
    fn indent_style_tab_уровень_1() {
        assert_eq!(IndentStyle::Tab.level(1), "\t");
    }

    #[test]
    fn indent_style_tab_уровень_3() {
        assert_eq!(IndentStyle::Tab.level(3), "\t\t\t");
    }

    #[test]
    fn indent_style_unit_возвращает_один_уровень() {
        assert_eq!(IndentStyle::Spaces(4).unit(), IndentStyle::Spaces(4).level(1));
        assert_eq!(IndentStyle::Tab.unit(), "\t");
    }

    #[test]
    fn indent_style_default_четыре_пробела() {
        assert_eq!(IndentStyle::default(), IndentStyle::Spaces(4));
    }

    // ===== Генерация C с настраиваемым отступом =====

    #[test]
    fn c_источник_с_отступом_2_пробела_содержит_двойной_отступ() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source_with_indent(&src, IndentStyle::Spaces(2));
        let (_, source) = generate_c_all(&src, &ctx);
        assert!(source.contains("\n  switch"),
            "Ожидается отступ в 2 пробела перед switch:\n{}", source);
    }

    #[test]
    fn c_источник_с_табуляцией_содержит_табы() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source_with_indent(&src, IndentStyle::Tab);
        let (_, source) = generate_c_all(&src, &ctx);
        assert!(source.contains('\t'),
            "Ожидается символ табуляции в коде с IndentStyle::Tab:\n{}", source);
    }

    #[test]
    fn c_источник_с_отступом_4_по_умолчанию_содержит_четыре_пробела() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source(&src);
        let (_, source) = generate_c_all(&src, &ctx);
        assert!(source.contains("\n    switch"),
            "Ожидается отступ в 4 пробела перед switch:\n{}", source);
    }

    #[test]
    fn c_источник_с_отступом_0_не_содержит_ведущих_пробелов_в_switch() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source_with_indent(&src, IndentStyle::Spaces(0));
        let (_, source) = generate_c_all(&src, &ctx);
        assert!(source.contains("\nswitch"),
            "При нулевом отступе switch должен быть без ведущих пробелов:\n{}", source);
    }

    // ===== Генерация ST с настраиваемым отступом =====

    #[test]
    fn st_объявление_с_отступом_2_содержит_двойной_отступ() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source_with_indent(&src, IndentStyle::Spaces(2));
        let (decl, _) = generate_st_all(&src, &ctx);
        assert!(decl.contains("\n  state"),
            "Ожидается 2-пробельный отступ перед 'state' в объявлении:\n{}", decl);
    }

    #[test]
    fn st_программа_с_табуляцией_содержит_табы() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source_with_indent(&src, IndentStyle::Tab);
        let (_, prog) = generate_st_all(&src, &ctx);
        assert!(prog.contains('\t'),
            "Ожидается символ табуляции в ST-программе с IndentStyle::Tab:\n{}", prog);
    }

    // ===== Генерация Verilog с настраиваемым отступом =====

    #[test]
    fn verilog_с_отступом_2_содержит_двойной_отступ() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source_with_indent(&src, IndentStyle::Spaces(2));
        let code = generate_verilog_all(&src, &ctx);
        assert!(code.contains("\n  input wire clk"),
            "Ожидается 2-пробельный отступ перед 'input wire clk':\n{}", code);
    }

    #[test]
    fn verilog_с_табуляцией_содержит_табы() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source_with_indent(&src, IndentStyle::Tab);
        let code = generate_verilog_all(&src, &ctx);
        assert!(code.contains('\t'),
            "Ожидается символ табуляции в Verilog-коде с IndentStyle::Tab:\n{}", code);
    }

    // ===== from_source_with_indent =====

    #[test]
    fn from_source_with_indent_устанавливает_стиль() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source_with_indent(&src, IndentStyle::Spaces(2));
        assert_eq!(ctx.indent, IndentStyle::Spaces(2));
        assert_eq!(ctx.global_vars.len(), 2);
    }

    // ===== Тесты behavior и end =====

    const BEHAVIOR_SRC: &str = r#"
model Worker {
    state Active { ref Done: ready; }
    state Done { }
    start -> Active;
    end -> { cleanup = 1; }
}
port ready : bit = 0;
port cleanup : bit = 0;
"#;

    const SEQUENTIAL_SRC: &str = r#"
model Phase1 {
    state Run { }
    start -> Run;
}
model Phase2 {
    state Run { }
    start -> Run;
}
model Pipeline {
    behavior -> Phase1 + Phase2;
    end -> { done = 1; }
}
port done : bit = 0;
"#;

    const PARALLEL_SRC: &str = r#"
model A { state S {} start -> S; }
model B { state S {} start -> S; }
model Combo {
    behavior -> A | B;
    end -> { finished = 1; }
}
port finished : bit = 0;
"#;

    fn parse_behavior(src: &str) -> SourceUnit {
        but_grammar::parse(src, 0)
            .expect("Парсинг behavior-источника")
            .0
    }

    #[test]
    fn c_header_содержит_is_done_для_терминального_состояния() {
        let src = parse_behavior(BEHAVIOR_SRC);
        let ctx = CodegenContext::from_source(&src);
        let (header, _) = generate_c_all(&src, &ctx);
        assert!(header.contains("is_done"),
            "Заголовок должен содержать is_done для терминального состояния Done:\n{}", header);
    }

    #[test]
    fn c_source_содержит_end_handler() {
        let src = parse_behavior(BEHAVIOR_SRC);
        let ctx = CodegenContext::from_source(&src);
        let (_, source) = generate_c_all(&src, &ctx);
        assert!(source.contains("end handler") || source.contains("cleanup"),
            "Исходник должен содержать end-обработчик:\n{}", source);
    }

    #[test]
    fn c_source_sequential_содержит_init_step() {
        let src = parse_behavior(SEQUENTIAL_SRC);
        let ctx = CodegenContext::from_source(&src);
        let (_, source) = generate_c_all(&src, &ctx);
        assert!(source.contains("pipeline_init") || source.contains("Pipeline_init"),
            "Исходник должен содержать pipeline_init:\n{}", source);
        assert!(source.contains("PHASE") || source.contains("phase"),
            "Исходник должен содержать фазы компоновки:\n{}", source);
    }

    #[test]
    fn c_source_parallel_содержит_parallel_logic() {
        let src = parse_behavior(PARALLEL_SRC);
        let ctx = CodegenContext::from_source(&src);
        let (_, source) = generate_c_all(&src, &ctx);
        assert!(source.contains("is_done") || source.contains("_done"),
            "Параллельная компоновка должна содержать проверку завершения:\n{}", source);
    }

    #[test]
    fn st_decl_sequential_содержит_phase() {
        let src = parse_behavior(SEQUENTIAL_SRC);
        let ctx = CodegenContext::from_source(&src);
        let (decl, _) = generate_st_all(&src, &ctx);
        assert!(decl.contains("phase") || decl.contains("PHASE"),
            "ST-декларация должна содержать переменную фазы:\n{}", decl);
    }

    #[test]
    fn verilog_содержит_done_signal_для_терминального_состояния() {
        let src = parse_behavior(BEHAVIOR_SRC);
        let ctx = CodegenContext::from_source(&src);
        let verilog = generate_verilog_all(&src, &ctx);
        assert!(verilog.contains("done"),
            "Verilog должен содержать done-сигнал:\n{}", verilog);
    }

    #[test]
    fn lc3_содержит_имя_модели() {
        let src = parse_behavior(BEHAVIOR_SRC);
        let ctx = CodegenContext::from_source(&src);
        let asm = generate_lc3_all(&src, &ctx);
        assert!(asm.contains("WORKER") || asm.contains("Worker"),
            "LC-3 должен содержать имя модели Worker:\n{}", asm);
    }

    #[test]
    fn thumb_содержит_init_function() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source(&src);
        let asm = generate_thumb_all(&src, &ctx);
        assert!(asm.contains("init"),
            "Thumb должен содержать функцию init:\n{}", asm);
    }
}
