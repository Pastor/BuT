use std::path::PathBuf;
use zed_extension_api::{self as zed, LanguageServerId, Result};

struct LamExtension;

impl zed::Extension for LamExtension {
    fn new() -> Self {
        LamExtension
    }

    fn language_server_command(
        &mut self,
        _language_server_id: &LanguageServerId,
        worktree: &zed::Worktree,
    ) -> Result<zed::Command> {
        let binary_path = find_lam_lsp(worktree)?;
        Ok(zed::Command {
            command: binary_path,
            args: vec![],
            env: vec![],
        })
    }
}

/// Находит исполняемый файл `takt-lsp`, последовательно проверяя:
///
/// 1. Директории переменной окружения `PATH` (через [`zed::Worktree::which`]).
/// 2. `$HOME/.cargo/bin/takt-lsp` - стандартное место установки через `cargo install`.
///
/// Возвращает полный путь к бинарнику или ошибку с описанием проблемы.
fn find_lam_lsp(worktree: &zed::Worktree) -> Result<String> {
    // Шаг 1: поиск в PATH
    if let Some(path) = worktree.which("takt-lsp") {
        return Ok(path);
    }

    // Шаг 2: получаем HOME из окружения оболочки и проверяем ~/.cargo/bin/takt-lsp
    let home = worktree
        .shell_env()
        .into_iter()
        .find(|(key, _)| key == "HOME")
        .map(|(_, value)| value)
        .ok_or_else(|| "переменная окружения HOME не задана".to_string())?;

    let fallback = PathBuf::from(&home).join(".cargo/bin/takt-lsp");
    if fallback.exists() {
        return Ok(fallback.to_string_lossy().into_owned());
    }

    Err(format!(
        "исполняемый файл takt-lsp не найден: \
         проверьте PATH или установите сервер командой \
         `cargo install --path <путь-к-проекту>/takt-lang --bin takt-lsp --features lsp`; \
         ожидаемый путь после установки: {}/.cargo/bin/takt-lsp",
        home
    ))
}

zed::register_extension!(LamExtension);
