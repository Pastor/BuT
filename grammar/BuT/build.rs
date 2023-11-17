extern crate lalrpop;

fn main() {
    let _ = lalrpop::Configuration::new()
        .use_cargo_dir_conventions()
        .emit_rerun_directives(true)
        .process_current_dir();
}
