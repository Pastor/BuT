pub mod dot;
pub mod render;

use std::path::Path;
use but_grammar::ast::{ModelDefinition, SourceUnit};
use thiserror::Error;

pub use dot::{model_to_dot, source_to_dots, condition_to_label};
pub use render::{render_to_png, render_to_svg, dot_available, RenderError};

/// Error during visualization.
#[derive(Debug, Error)]
pub enum VisualError {
    #[error("Render error: {0}")]
    Render(#[from] RenderError),
    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),
}

/// Visualize a single model to a PNG file.
/// If `current_state` is provided, that state will be highlighted in red.
pub fn visualize_model(
    model: &ModelDefinition,
    current_state: Option<&str>,
    out_path: &Path,
) -> Result<(), VisualError> {
    let dot_src = model_to_dot(model, current_state);

    // Always save the DOT file alongside the PNG
    let dot_path = out_path.with_extension("dot");
    std::fs::write(&dot_path, &dot_src)?;

    // Render to PNG if Graphviz is available
    if dot_available() {
        render_to_png(&dot_src, out_path)?;
        println!("[visual] Generated: {}", out_path.display());
    } else {
        println!(
            "[visual] DOT saved to {}. Install Graphviz to generate PNG.",
            dot_path.display()
        );
    }

    Ok(())
}

/// Visualize all models from a SourceUnit, saving PNGs to `out_dir/<model_name>.png`.
pub fn visualize_all(source: &SourceUnit, out_dir: &Path) -> Result<(), VisualError> {
    std::fs::create_dir_all(out_dir)?;
    let dots = source_to_dots(source);
    for (name, dot_src) in dots {
        let png_path = out_dir.join(format!("{}.png", name.to_lowercase()));
        let dot_path = out_dir.join(format!("{}.dot", name.to_lowercase()));
        std::fs::write(&dot_path, &dot_src)?;
        if dot_available() {
            render_to_png(&dot_src, &png_path)?;
            println!("[visual] Generated: {}", png_path.display());
        } else {
            println!(
                "[visual] DOT saved to {}. Install Graphviz to generate PNG.",
                dot_path.display()
            );
        }
    }
    Ok(())
}
