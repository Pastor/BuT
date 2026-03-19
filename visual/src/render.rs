use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};
use thiserror::Error;

/// Error during rendering.
#[derive(Debug, Error)]
pub enum RenderError {
    #[error("Command 'dot' (Graphviz) not found. Install with: brew install graphviz")]
    GraphvizNotFound,
    #[error("The dot command failed: {0}")]
    DotFailed(String),
    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),
}

/// Render a DOT string to a PNG file using the external `dot` command.
pub fn render_to_png(dot_src: &str, output_path: &Path) -> Result<(), RenderError> {
    render(dot_src, output_path, "png")
}

/// Render a DOT string to an SVG file using the external `dot` command.
pub fn render_to_svg(dot_src: &str, output_path: &Path) -> Result<(), RenderError> {
    render(dot_src, output_path, "svg")
}

/// Render a DOT string to a file using the external `dot` command.
pub fn render(dot_src: &str, output_path: &Path, format: &str) -> Result<(), RenderError> {
    // Create parent directories if necessary
    if let Some(parent) = output_path.parent() {
        std::fs::create_dir_all(parent)?;
    }

    let mut child = Command::new("dot")
        .arg(format!("-T{}", format))
        .arg("-o")
        .arg(output_path)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|e| {
            if e.kind() == std::io::ErrorKind::NotFound {
                RenderError::GraphvizNotFound
            } else {
                RenderError::Io(e)
            }
        })?;

    if let Some(stdin) = child.stdin.take() {
        let mut stdin = stdin;
        stdin.write_all(dot_src.as_bytes())?;
    }

    let output = child.wait_with_output()?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr).to_string();
        return Err(RenderError::DotFailed(stderr));
    }

    Ok(())
}

/// Check whether the `dot` command (Graphviz) is available in PATH.
pub fn dot_available() -> bool {
    Command::new("dot")
        .arg("-V")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .map(|s| s.success())
        .unwrap_or(false)
}
