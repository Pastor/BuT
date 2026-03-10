extern crate lalrpop;

fn main() {
    let _ = lalrpop::Configuration::new()
        .always_use_colors()
        .emit_rerun_directives(true)
        .process_current_dir();
}
