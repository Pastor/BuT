extern crate lalrpop;

fn main() {
    let _ = lalrpop::Configuration::new()
        .always_use_colors()
        .process_current_dir();
}
