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

pub use behavior::{model_composition, find_end_property, find_terminal_states, BehaviorKind};
pub use c::{generate_c_all, generate_c_all_named, generate_c_header, generate_c_source};
pub use lc3::{generate_lc3, generate_lc3_all};
pub use st::{generate_st_all, generate_st_decl, generate_st_program};
pub use thumb::{generate_thumb, generate_thumb_all};
pub use verilog::{generate_verilog, generate_verilog_all};

/// Indentation style for code generation.
///
/// Used by all backends (C, ST, Verilog) for formatting indentation.
/// Default is four spaces per nesting level.
#[derive(Debug, Clone, PartialEq)]
pub enum IndentStyle {
    /// Space indentation: specifies the number of spaces per level.
    Spaces(usize),
    /// Tab indentation: one `\t` per level.
    Tab,
}

impl IndentStyle {
    /// Generate an indentation string for a given nesting level.
    ///
    /// For example, `Spaces(4).level(2)` returns 8 spaces.
    pub fn level(&self, n: usize) -> String {
        match self {
            IndentStyle::Spaces(size) => " ".repeat(size * n),
            IndentStyle::Tab => "\t".repeat(n),
        }
    }

    /// One unit of indentation (one level).
    pub fn unit(&self) -> String {
        self.level(1)
    }
}

impl Default for IndentStyle {
    fn default() -> Self {
        IndentStyle::Spaces(4)
    }
}

/// Context for code generators with global declarations.
#[derive(Debug, Default)]
pub struct CodegenContext {
    /// Global variable/port definitions from the source file.
    pub global_vars: Vec<Box<VariableDefinition>>,
    /// Type alias table: alias name → type definition.
    pub type_aliases: HashMap<String, Type>,
    /// Indentation style for all code generators.
    pub indent: IndentStyle,
}

impl CodegenContext {
    pub fn new() -> Self {
        Self::default()
    }

    /// Build a context from a SourceUnit, collecting global declarations and type aliases.
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

    /// Build a context with a specified indentation style.
    pub fn from_source_with_indent(source: &SourceUnit, indent: IndentStyle) -> Self {
        let mut ctx = Self::from_source(source);
        ctx.indent = indent;
        ctx
    }
}

/// Generate all outputs for all models from a SourceUnit (one file per language).
///
/// Use [`AllOutput::generate_named`] to specify a base name for output files.
pub struct AllOutput {
    /// (header, source) — one `.h` + one `.c` for all models
    pub c: (String, String),
    /// Single Verilog file with all modules
    pub verilog: String,
    /// (declarations, program) — one `.FB.DECL.st` + one `.FB.PRGS.st` for all models
    pub st: (String, String),
    /// Single LC-3 assembly file
    pub lc3: String,
    /// Single ARM Thumb assembly file
    pub thumb: String,
}

impl AllOutput {
    /// Generate all outputs with the base name "model" and default settings (4 spaces).
    pub fn generate(source: &SourceUnit) -> Self {
        Self::generate_named(source, "model")
    }

    /// Generate all outputs with a specified base file name.
    pub fn generate_named(source: &SourceUnit, base_name: &str) -> Self {
        let ctx = CodegenContext::from_source(source);
        Self::generate_with_ctx(source, base_name, &ctx)
    }

    /// Generate all outputs with a specified context (including indentation style).
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

    /// BuT source for the Delay model used in tests
    const DELAY_SRC: &str = r#"
model Delay {
    state One {
        ref None: input = 0;
        ref One : input = 1;
        enter -> { output = 1; }
    }
    start None {
        ref One : input = 1;
        ref None: input = 0;
        enter -> { output = 0; }
    }
    state End { }
}
port input : bit = 0x01;
port output: bit = 0x02;
"#;

    /// Helper function: parse source and return SourceUnit
    fn parse_delay() -> SourceUnit {
        but_grammar::parse(DELAY_SRC, 0)
            .expect("Delay source code should parse without errors")
            .0
    }

    // Source with type aliases
    const TYPE_ALIAS_SRC: &str = r#"
type MyByte = u8;
type Counter = u32;
type Flag = bool;
type Nested = MyByte;
model M { start M { } }
"#;

    fn parse_type_aliases() -> SourceUnit {
        but_grammar::parse(TYPE_ALIAS_SRC, 0)
            .expect("Source with aliases should parse without errors")
            .0
    }

    // ===== CodegenContext::from_source =====

    #[test]
    fn from_source_collects_global_variables() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source(&src);
        // The source declares two ports: input and output
        assert_eq!(ctx.global_vars.len(), 2);
    }

    #[test]
    fn from_source_empty_source_no_variables() {
        let (src, _) = but_grammar::parse("model Empty { start Empty { } }", 0)
            .expect("Parsing empty model");
        let ctx = CodegenContext::from_source(&src);
        assert_eq!(ctx.global_vars.len(), 0);
    }

    #[test]
    fn from_source_collects_type_aliases() {
        let src = parse_type_aliases();
        let ctx = CodegenContext::from_source(&src);
        assert!(ctx.type_aliases.contains_key("MyByte"));
        assert!(ctx.type_aliases.contains_key("Counter"));
        assert!(ctx.type_aliases.contains_key("Flag"));
        assert!(ctx.type_aliases.contains_key("Nested"));
    }

    #[test]
    fn from_source_empty_source_no_aliases() {
        let (src, _) = but_grammar::parse("model Empty { start Empty { } }", 0)
            .expect("Parsing empty model");
        let ctx = CodegenContext::from_source(&src);
        assert!(ctx.type_aliases.is_empty());
    }

    #[test]
    fn from_source_port_names_correct() {
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
    fn all_output_generate_returns_data_for_delay() {
        let src = parse_delay();
        let out = AllOutput::generate(&src);
        // All outputs are non-empty
        assert!(!out.c.0.is_empty(), "C header should be non-empty");
        assert!(!out.c.1.is_empty(), "C source should be non-empty");
        assert!(!out.verilog.is_empty(), "Verilog should be non-empty");
        assert!(!out.st.0.is_empty() || !out.st.1.is_empty(), "ST should be non-empty");
        assert!(!out.lc3.is_empty(), "LC-3 should be non-empty");
        assert!(!out.thumb.is_empty(), "Thumb should be non-empty");
    }

    #[test]
    fn all_output_contains_model_name_delay() {
        let src = parse_delay();
        let out = AllOutput::generate(&src);
        assert!(out.c.0.contains("DELAY") || out.c.0.contains("delay"),
            "C header should contain the model name Delay: {}", out.c.0);
        assert!(out.verilog.contains("delay") || out.verilog.contains("Delay"),
            "Verilog should contain the model name: {}", out.verilog);
        assert!(out.st.0.contains("DELAY") || out.st.1.contains("DELAY"),
            "ST should contain the model name DELAY");
    }

    // ===== generate_c_all =====

    #[test]
    fn generate_c_all_creates_header_and_source() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source(&src);
        let (header, source) = generate_c_all(&src, &ctx);
        assert!(header.contains("DELAY") || header.contains("delay"),
            "Header should contain the model name: {}", header);
        assert!(!source.is_empty(), "Source should not be empty");
    }

    // ===== generate_verilog_all =====

    #[test]
    fn generate_verilog_all_creates_module() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source(&src);
        let verilog = generate_verilog_all(&src, &ctx);
        assert!(!verilog.is_empty());
        assert!(verilog.contains("module"), "Verilog should contain 'module': {}", verilog);
        assert!(verilog.contains("delay"), "Verilog should contain the model name: {}", verilog);
    }

    // ===== generate_st_all =====

    #[test]
    fn generate_st_all_creates_decl_and_program() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source(&src);
        let (decl, prog) = generate_st_all(&src, &ctx);
        assert!(!decl.is_empty() || !prog.is_empty(), "ST should have non-empty output");
        assert!(decl.contains("DELAY") || prog.contains("DELAY"), "ST should contain DELAY");
    }

    // ===== generate_lc3_all =====

    #[test]
    fn generate_lc3_all_creates_assembly() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source(&src);
        let asm = generate_lc3_all(&src, &ctx);
        assert!(!asm.is_empty(), "LC-3 assembly should not be empty");
    }

    // ===== generate_thumb_all =====

    #[test]
    fn generate_thumb_all_creates_assembly() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source(&src);
        let asm = generate_thumb_all(&src, &ctx);
        assert!(!asm.is_empty(), "Thumb assembly should not be empty");
    }

    // ===== Integration tests: type aliases in code generation =====

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
    start Idle { }
}
"#;

    fn parse_type_alias_ports() -> SourceUnit {
        but_grammar::parse(TYPE_ALIAS_PORTS_SRC, 0)
            .expect("Parsing source with type aliases")
            .0
    }

    #[test]
    fn c_header_resolves_byte_alias_to_uint8_t() {
        let src = parse_type_alias_ports();
        let ctx = CodegenContext::from_source(&src);
        let (header, _) = generate_c_all(&src, &ctx);
        assert!(header.contains("uint8_t sensor"),
            "Expected uint8_t for Byte, header:\n{}", header);
    }

    #[test]
    fn c_header_resolves_counter_alias_to_uint32_t() {
        let src = parse_type_alias_ports();
        let ctx = CodegenContext::from_source(&src);
        let (header, _) = generate_c_all(&src, &ctx);
        assert!(header.contains("uint32_t count"),
            "Expected uint32_t for Counter, header:\n{}", header);
    }

    #[test]
    fn c_header_resolves_flag_alias_to_int() {
        let src = parse_type_alias_ports();
        let ctx = CodegenContext::from_source(&src);
        let (header, _) = generate_c_all(&src, &ctx);
        assert!(header.contains("int alarm"),
            "Expected int (bool→int) for Flag, header:\n{}", header);
    }

    #[test]
    fn c_header_resolves_speed_alias_to_double() {
        let src = parse_type_alias_ports();
        let ctx = CodegenContext::from_source(&src);
        let (header, _) = generate_c_all(&src, &ctx);
        assert!(header.contains("double rate"),
            "Expected double for Speed (f64), header:\n{}", header);
    }

    #[test]
    fn c_header_resolves_nested_alias_to_uint8_t() {
        let src = parse_type_alias_ports();
        let ctx = CodegenContext::from_source(&src);
        let (header, _) = generate_c_all(&src, &ctx);
        assert!(header.contains("uint8_t raw"),
            "Expected uint8_t for NestedAlias→Byte→u8, header:\n{}", header);
    }

    #[test]
    fn st_decl_resolves_counter_alias_to_dword() {
        let src = parse_type_alias_ports();
        let ctx = CodegenContext::from_source(&src);
        let (decl, _) = generate_st_all(&src, &ctx);
        assert!(decl.contains("DWORD"),
            "Expected DWORD for Counter→u32, declaration:\n{}", decl);
    }

    #[test]
    fn st_decl_resolves_flag_alias_to_bool() {
        let src = parse_type_alias_ports();
        let ctx = CodegenContext::from_source(&src);
        let (decl, _) = generate_st_all(&src, &ctx);
        assert!(decl.contains("BOOL"),
            "Expected BOOL for Flag→bool, declaration:\n{}", decl);
    }

    // ===== Integration tests: type system (bit and float as primitive types) =====

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
model Test { start A { } }
"#;

    fn parse_std_types() -> SourceUnit {
        but_grammar::parse(STD_TYPES_SRC, 0)
            .expect("Source with std types should parse without errors")
            .0
    }

    #[test]
    fn c_header_u8_via_bit_array_gives_uint8_t() {
        let src = parse_std_types();
        let ctx = CodegenContext::from_source(&src);
        let (header, _) = generate_c_all(&src, &ctx);
        assert!(header.contains("uint8_t x8"),
            "Expected uint8_t for u8=[8:bit], header:\n{}", header);
    }

    #[test]
    fn c_header_u16_via_bit_array_gives_uint16_t() {
        let src = parse_std_types();
        let ctx = CodegenContext::from_source(&src);
        let (header, _) = generate_c_all(&src, &ctx);
        assert!(header.contains("uint16_t x16"),
            "Expected uint16_t for u16=[16:bit], header:\n{}", header);
    }

    #[test]
    fn c_header_u32_via_bit_array_gives_uint32_t() {
        let src = parse_std_types();
        let ctx = CodegenContext::from_source(&src);
        let (header, _) = generate_c_all(&src, &ctx);
        assert!(header.contains("uint32_t x32"),
            "Expected uint32_t for u32=[32:bit], header:\n{}", header);
    }

    #[test]
    fn c_header_u64_via_bit_array_gives_uint64_t() {
        let src = parse_std_types();
        let ctx = CodegenContext::from_source(&src);
        let (header, _) = generate_c_all(&src, &ctx);
        assert!(header.contains("uint64_t x64"),
            "Expected uint64_t for u64=[64:bit], header:\n{}", header);
    }

    #[test]
    fn c_header_u128_via_bit_array_gives_uint64_t_array() {
        let src = parse_std_types();
        let ctx = CodegenContext::from_source(&src);
        let (header, _) = generate_c_all(&src, &ctx);
        assert!(header.contains("uint64_t[2]"),
            "Expected uint64_t[2] for u128=[128:bit], header:\n{}", header);
    }

    #[test]
    fn c_header_bool_via_bit_gives_uint8_t() {
        let src = parse_std_types();
        let ctx = CodegenContext::from_source(&src);
        let (header, _) = generate_c_all(&src, &ctx);
        assert!(header.contains("uint8_t flag"),
            "Expected uint8_t for bool=bit, header:\n{}", header);
    }

    #[test]
    fn c_header_counter_via_alias_chain() {
        let src = parse_std_types();
        let ctx = CodegenContext::from_source(&src);
        let (header, _) = generate_c_all(&src, &ctx);
        assert!(header.contains("uint32_t cnt"),
            "Expected uint32_t for Counter→u32→[32:bit], header:\n{}", header);
    }

    #[test]
    fn st_decl_u8_via_bit_array_gives_byte() {
        let src = parse_std_types();
        let ctx = CodegenContext::from_source(&src);
        let (decl, _) = generate_st_all(&src, &ctx);
        assert!(decl.contains("BYTE"),
            "Expected BYTE for u8=[8:bit], declaration:\n{}", decl);
    }

    #[test]
    fn st_decl_u32_via_bit_array_gives_dword() {
        let src = parse_std_types();
        let ctx = CodegenContext::from_source(&src);
        let (decl, _) = generate_st_all(&src, &ctx);
        assert!(decl.contains("DWORD"),
            "Expected DWORD for u32=[32:bit], declaration:\n{}", decl);
    }

    #[test]
    fn st_decl_u128_via_bit_array_gives_lword_array() {
        let src = parse_std_types();
        let ctx = CodegenContext::from_source(&src);
        let (decl, _) = generate_st_all(&src, &ctx);
        assert!(decl.contains("ARRAY [0..1] OF LWORD"),
            "Expected ARRAY [0..1] OF LWORD for u128=[128:bit], declaration:\n{}", decl);
    }

    // ===== IndentStyle =====

    #[test]
    fn indent_style_spaces_level_0_empty_string() {
        assert_eq!(IndentStyle::Spaces(4).level(0), "");
        assert_eq!(IndentStyle::Spaces(2).level(0), "");
        assert_eq!(IndentStyle::Tab.level(0), "");
    }

    #[test]
    fn indent_style_spaces_4_level_1() {
        assert_eq!(IndentStyle::Spaces(4).level(1), "    ");
    }

    #[test]
    fn indent_style_spaces_2_level_1() {
        assert_eq!(IndentStyle::Spaces(2).level(1), "  ");
    }

    #[test]
    fn indent_style_spaces_2_level_3() {
        assert_eq!(IndentStyle::Spaces(2).level(3), "      ");
    }

    #[test]
    fn indent_style_tab_level_1() {
        assert_eq!(IndentStyle::Tab.level(1), "\t");
    }

    #[test]
    fn indent_style_tab_level_3() {
        assert_eq!(IndentStyle::Tab.level(3), "\t\t\t");
    }

    #[test]
    fn indent_style_unit_returns_one_level() {
        assert_eq!(IndentStyle::Spaces(4).unit(), IndentStyle::Spaces(4).level(1));
        assert_eq!(IndentStyle::Tab.unit(), "\t");
    }

    #[test]
    fn indent_style_default_four_spaces() {
        assert_eq!(IndentStyle::default(), IndentStyle::Spaces(4));
    }

    // ===== C generation with configurable indentation =====

    #[test]
    fn c_source_with_2_space_indent_contains_double_indent() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source_with_indent(&src, IndentStyle::Spaces(2));
        let (_, source) = generate_c_all(&src, &ctx);
        assert!(source.contains("\n  switch"),
            "Expected 2-space indent before switch:\n{}", source);
    }

    #[test]
    fn c_source_with_tab_indent_contains_tabs() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source_with_indent(&src, IndentStyle::Tab);
        let (_, source) = generate_c_all(&src, &ctx);
        assert!(source.contains('\t'),
            "Expected tab character in code with IndentStyle::Tab:\n{}", source);
    }

    #[test]
    fn c_source_with_default_4_space_indent_contains_four_spaces() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source(&src);
        let (_, source) = generate_c_all(&src, &ctx);
        assert!(source.contains("\n    switch"),
            "Expected 4-space indent before switch:\n{}", source);
    }

    #[test]
    fn c_source_with_0_indent_no_leading_spaces_before_switch() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source_with_indent(&src, IndentStyle::Spaces(0));
        let (_, source) = generate_c_all(&src, &ctx);
        assert!(source.contains("\nswitch"),
            "With zero indent, switch should have no leading spaces:\n{}", source);
    }

    // ===== ST generation with configurable indentation =====

    #[test]
    fn st_decl_with_2_space_indent_contains_double_indent() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source_with_indent(&src, IndentStyle::Spaces(2));
        let (decl, _) = generate_st_all(&src, &ctx);
        assert!(decl.contains("\n  state"),
            "Expected 2-space indent before 'state' in declaration:\n{}", decl);
    }

    #[test]
    fn st_program_with_tab_indent_contains_tabs() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source_with_indent(&src, IndentStyle::Tab);
        let (_, prog) = generate_st_all(&src, &ctx);
        assert!(prog.contains('\t'),
            "Expected tab character in ST program with IndentStyle::Tab:\n{}", prog);
    }

    // ===== Verilog generation with configurable indentation =====

    #[test]
    fn verilog_with_2_space_indent_contains_double_indent() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source_with_indent(&src, IndentStyle::Spaces(2));
        let code = generate_verilog_all(&src, &ctx);
        assert!(code.contains("\n  input wire clk"),
            "Expected 2-space indent before 'input wire clk':\n{}", code);
    }

    #[test]
    fn verilog_with_tab_indent_contains_tabs() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source_with_indent(&src, IndentStyle::Tab);
        let code = generate_verilog_all(&src, &ctx);
        assert!(code.contains('\t'),
            "Expected tab character in Verilog code with IndentStyle::Tab:\n{}", code);
    }

    // ===== from_source_with_indent =====

    #[test]
    fn from_source_with_indent_sets_style() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source_with_indent(&src, IndentStyle::Spaces(2));
        assert_eq!(ctx.indent, IndentStyle::Spaces(2));
        assert_eq!(ctx.global_vars.len(), 2);
    }

    // ===== Behavior and end tests =====

    const BEHAVIOR_SRC: &str = r#"
model Worker {
    start Active { ref Done: ready; }
    state Done { }
    end -> { cleanup = 1; }
}
port ready : bit = 0;
port cleanup : bit = 0;
"#;

    const SEQUENTIAL_SRC: &str = r#"
model Phase1 {
    start Run { }
}
model Phase2 {
    start Run { }
}
model Pipeline = Phase1 + Phase2 {
    end -> { done = 1; }
}
port done : bit = 0;
"#;

    const PARALLEL_SRC: &str = r#"
model A { start S { } }
model B { start S { } }
model Combo = A | B {
    end -> { finished = 1; }
}
port finished : bit = 0;
"#;

    fn parse_behavior(src: &str) -> SourceUnit {
        but_grammar::parse(src, 0)
            .expect("Parsing behavior source")
            .0
    }

    #[test]
    fn c_header_contains_is_done_for_terminal_state() {
        let src = parse_behavior(BEHAVIOR_SRC);
        let ctx = CodegenContext::from_source(&src);
        let (header, _) = generate_c_all(&src, &ctx);
        assert!(header.contains("is_done"),
            "Header should contain is_done for terminal state Done:\n{}", header);
    }

    #[test]
    fn c_source_contains_end_handler() {
        let src = parse_behavior(BEHAVIOR_SRC);
        let ctx = CodegenContext::from_source(&src);
        let (_, source) = generate_c_all(&src, &ctx);
        assert!(source.contains("end handler") || source.contains("cleanup"),
            "Source should contain end handler:\n{}", source);
    }

    #[test]
    fn c_source_sequential_contains_init_step() {
        let src = parse_behavior(SEQUENTIAL_SRC);
        let ctx = CodegenContext::from_source(&src);
        let (_, source) = generate_c_all(&src, &ctx);
        assert!(source.contains("pipeline_init") || source.contains("Pipeline_init"),
            "Source should contain pipeline_init:\n{}", source);
        assert!(source.contains("PHASE") || source.contains("phase"),
            "Source should contain composition phases:\n{}", source);
    }

    #[test]
    fn c_source_parallel_contains_parallel_logic() {
        let src = parse_behavior(PARALLEL_SRC);
        let ctx = CodegenContext::from_source(&src);
        let (_, source) = generate_c_all(&src, &ctx);
        assert!(source.contains("is_done") || source.contains("_done"),
            "Parallel composition should contain completion check:\n{}", source);
    }

    #[test]
    fn st_decl_sequential_contains_phase() {
        let src = parse_behavior(SEQUENTIAL_SRC);
        let ctx = CodegenContext::from_source(&src);
        let (decl, _) = generate_st_all(&src, &ctx);
        assert!(decl.contains("phase") || decl.contains("PHASE"),
            "ST declaration should contain phase variable:\n{}", decl);
    }

    #[test]
    fn verilog_contains_done_signal_for_terminal_state() {
        let src = parse_behavior(BEHAVIOR_SRC);
        let ctx = CodegenContext::from_source(&src);
        let verilog = generate_verilog_all(&src, &ctx);
        assert!(verilog.contains("done"),
            "Verilog should contain done signal:\n{}", verilog);
    }

    #[test]
    fn lc3_contains_model_name() {
        let src = parse_behavior(BEHAVIOR_SRC);
        let ctx = CodegenContext::from_source(&src);
        let asm = generate_lc3_all(&src, &ctx);
        assert!(asm.contains("WORKER") || asm.contains("Worker"),
            "LC-3 should contain model name Worker:\n{}", asm);
    }

    #[test]
    fn thumb_contains_init_function() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source(&src);
        let asm = generate_thumb_all(&src, &ctx);
        assert!(asm.contains("init"),
            "Thumb should contain init function:\n{}", asm);
    }
}
