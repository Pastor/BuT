//! LSP-сервер для языка Takt.
//!
//! Реализует протокол Language Server Protocol для редактора Zed и других LSP-клиентов.
//!
//! ## Поддерживаемые возможности
//!
//! - `textDocument/didOpen`, `didChange`, `didClose` - синхронизация документов
//! - `textDocument/publishDiagnostics` - отправка ошибок и предупреждений
//! - `textDocument/completion` - автодополнение ключевых слов и идентификаторов
//! - `textDocument/hover` - информация о типе идентификатора под курсором

use std::collections::HashMap;
use std::error::Error;
use std::process;

use lsp_server::{Connection, Message, Notification, Request, Response};
use lsp_types::notification::{
    DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, Notification as _,
    PublishDiagnostics,
};
use lsp_types::request::{
    Completion, Formatting, GotoDeclaration, GotoDeclarationParams, GotoDeclarationResponse,
    GotoDefinition, HoverRequest, PrepareRenameRequest, References, Rename, Request as _,
};
use lsp_types::*;

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    // Аргументы разбираются до открытия stdio: иначе `takt-lsp --version` уходит в
    // протокол и отвечает `ProtocolError("disconnected channel")`. Версия нужна, чтобы
    // отличить устаревший установленный сервер от дефекта языка: редактор держит свой
    // сервер и обновляется независимо от дерева.
    if let Some(code) =
        takt_lang::version::handle_server_args(&std::env::args().skip(1).collect::<Vec<_>>())
    {
        process::exit(code);
    }

    // Инициализируем соединение через stdin/stdout
    let (connection, io_threads) = Connection::stdio();

    // Описываем возможности сервера. Список живёт в библиотеке: бинарник тестами не
    // покрыть, а "что объявлено" - проверяемый факт.
    let server_capabilities = serde_json::to_value(takt_lang::lsp::server_capabilities())?;

    // Выполняем инициализационное рукопожатие. Параметры клиента больше не
    // игнорируются: из `initializationOptions.searchPaths` берутся пути поиска импортов
    // (аналог `-I` у `taktc`), иначе импорт из общей библиотеки вне каталога документа
    // в редакторе не находится.
    let (init_id, init_params) = connection.initialize_start()?;
    let search_paths = search_paths_from_init(&init_params);
    // Корни рабочей области: по ним идут `references` и `rename`. Клиент присылает либо
    // `workspaceFolders`, либо устаревший `root_uri`; если ни того, ни другого -
    // область сводится к каталогу открытого документа (подставляется при запросе).
    let workspace_roots = workspace_roots_from_init(&init_params);
    let init_result = InitializeResult {
        capabilities: serde_json::from_value(server_capabilities)?,
        server_info: Some(ServerInfo {
            name: "takt-lsp".to_string(),
            version: Some(env!("CARGO_PKG_VERSION").to_string()),
        }),
    };
    connection.initialize_finish(init_id, serde_json::to_value(init_result)?)?;

    // Запускаем основной цикл обработки сообщений
    main_loop(connection, search_paths, workspace_roots)?;
    io_threads.join()?;

    Ok(())
}

/// Пути поиска импортов из `initializationOptions` клиента.
///
/// `init_params` - сырой JSON `InitializeParams` из `initialize_start()`. Относительные
/// пути `searchPaths` разрешаются от корня рабочей области (`root_uri`); разбор и
/// устойчивость к плохому конфигу - в библиотеке
/// `takt_lang::lsp::search_paths_from_options` (ради тестируемости: бинарник
/// юнит-тестами не покрыть).
fn search_paths_from_init(init_params: &serde_json::Value) -> Vec<String> {
    let params: InitializeParams = match serde_json::from_value(init_params.clone()) {
        Ok(p) => p,
        // Нечитаемые параметры инициализации - работаем без путей поиска (прежнее
        // поведение), а не падаем на старте.
        Err(e) => {
            eprintln!("[takt-lsp] initializationOptions не разобраны: {e}");
            return Vec::new();
        }
    };
    #[allow(deprecated)] // root_uri помечен устаревшим, но именно его шлют клиенты (Zed).
    let root = params.root_uri.as_ref().map(uri_to_path);
    takt_lang::lsp::search_paths_from_options(
        params.initialization_options.as_ref(),
        root.as_deref(),
    )
}

/// Корни рабочей области из параметров инициализации.
///
/// `root_uri` помечен устаревшим, но именно его шлют живые клиенты (Zed), поэтому
/// берутся **оба** источника: сперва `workspaceFolders`, затем корень.
fn workspace_roots_from_init(init_params: &serde_json::Value) -> Vec<String> {
    let Ok(params) = serde_json::from_value::<InitializeParams>(init_params.clone()) else {
        return Vec::new();
    };
    let mut roots: Vec<String> = params
        .workspace_folders
        .unwrap_or_default()
        .iter()
        .map(|f| uri_to_path(&f.uri))
        .collect();
    #[allow(deprecated)]
    if let Some(root) = params.root_uri.as_ref().map(uri_to_path)
        && !roots.contains(&root)
    {
        roots.push(root);
    }
    roots
}

/// Состояние сервера: открытые документы + пути поиска импортов.
struct ServerState {
    /// Содержимое открытых документов: URI -> текст.
    documents: HashMap<Uri, String>,
    /// Пути поиска импортов (аналог `-I` у `taktc`), из `initializationOptions`. Пустой
    /// список = прежнее поведение (только каталог документа).
    search_paths: Vec<String>,
    /// Корни рабочей области.
    workspace_roots: Vec<String>,
}

impl ServerState {
    fn new(search_paths: Vec<String>, workspace_roots: Vec<String>) -> Self {
        ServerState {
            documents: HashMap::new(),
            search_paths,
            workspace_roots,
        }
    }

    /// Возвращает содержимое документа по URI.
    fn get_text(&self, uri: &Uri) -> Option<&str> {
        self.documents.get(uri).map(String::as_str)
    }

    /// Корни для запроса по документу: объявленные клиентом, иначе каталог самого
    /// документа - иначе области не было бы вовсе.
    fn roots_for(&self, path: &str) -> Vec<String> {
        if !self.workspace_roots.is_empty() {
            return self.workspace_roots.clone();
        }
        std::path::Path::new(path)
            .parent()
            .map(|d| vec![d.to_string_lossy().into_owned()])
            .unwrap_or_default()
    }

    /// Тексты открытых документов для рабочей области.
    ///
    /// У редактора текст свежее диска: правка, построенная по диску, встала бы не туда,
    /// а вхождение из несохранённой строки не нашлось бы вовсе.
    fn overlay(&self) -> impl Fn(&str) -> Option<String> + '_ {
        move |path: &str| {
            self.documents
                .iter()
                .find_map(|(uri, text)| (uri_to_path(uri) == path).then(|| text.clone()))
        }
    }
}

/// Основной цикл обработки LSP-сообщений.
fn main_loop(
    connection: Connection,
    search_paths: Vec<String>,
    workspace_roots: Vec<String>,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let mut state = ServerState::new(search_paths, workspace_roots);

    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                // Проверяем запрос на завершение работы
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                handle_request(&connection, &state, req)?;
            }
            Message::Notification(not) => {
                handle_notification(&connection, &mut state, not)?;
            }
            Message::Response(_) => {
                // Ответы от клиента игнорируем
            }
        }
    }
    Ok(())
}

/// Параметры запроса графа: документ, как у прочих операций редактора.
#[derive(Debug, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct GraphParams {
    text_document: TextDocumentIdentifier,
}

/// Обрабатывает входящий запрос от клиента.
fn handle_request(
    connection: &Connection,
    state: &ServerState,
    req: Request,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    match req.method.as_str() {
        Completion::METHOD => {
            let params: CompletionParams = serde_json::from_value(req.params)?;
            let uri = &params.text_document_position.text_document.uri;
            let text = state.get_text(uri).unwrap_or("");
            let items = takt_lang::lsp::completion_items(text);
            let result = CompletionResponse::Array(items);
            connection.sender.send(Message::Response(Response::new_ok(
                req.id,
                serde_json::to_value(result)?,
            )))?;
        }
        Formatting::METHOD => {
            let params: DocumentFormattingParams = serde_json::from_value(req.params)?;
            let uri = &params.text_document.uri;
            let text = state.get_text(uri).unwrap_or("");
            // `Ok(None)` - текст уже каноничен: правок нет, файл не трогаем. `Err` -
            // форматировать нельзя: логируем и отвечаем `null`. Молча "отформатировать
            // во что-то" хуже, чем не форматировать вовсе.
            let result = match takt_lang::lsp::formatting_edits(text) {
                Ok(edits) => edits,
                Err(e) => {
                    eprintln!("[takt-lsp] форматирование не выполнено: {e}");
                    None
                }
            };
            connection.sender.send(Message::Response(Response::new_ok(
                req.id,
                serde_json::to_value(result)?,
            )))?;
        }
        HoverRequest::METHOD => {
            let params: HoverParams = serde_json::from_value(req.params)?;
            let uri = &params.text_document_position_params.text_document.uri;
            let position = params.text_document_position_params.position;
            let text = state.get_text(uri).unwrap_or("");
            let hover = takt_lang::lsp::hover_info(text, position);
            connection.sender.send(Message::Response(Response::new_ok(
                req.id,
                serde_json::to_value(hover)?,
            )))?;
        }
        // Одна ветка на оба метода: в Takt объявление и определение - одно и то же, и
        // разделять их нечего. Разные редакторы шлют по F12 разное (VS Code -
        // `definition`, Zed - `declaration`); обслуживая их **разным** кодом, сервер
        // рано или поздно ответил бы по-разному на один и тот же курсор. Параметры и
        // ответ у методов совпадают по типу (`GotoDeclarationParams =
        // GotoDefinitionParams`), поэтому объединение бесплатно.
        GotoDeclaration::METHOD | GotoDefinition::METHOD => {
            let params: GotoDeclarationParams = serde_json::from_value(req.params)?;
            let uri = &params.text_document_position_params.text_document.uri;
            let position = params.text_document_position_params.position;
            let text = state.get_text(uri).unwrap_or("");
            // Кросс-файловый вариант с путём документа: каталог документа - неявный
            // путь импорта, без него переходить в чужой файл некуда.
            let result = takt_lang::lsp::goto_declaration_at(
                &uri_to_path(uri),
                text,
                position,
                &state.search_paths,
            )
            .and_then(|loc| {
                // Пустой URI - контракт "это текущий файл": подставляет вызывающий, у
                // которого URI документа и так на руках.
                let target = if loc.uri.is_empty() {
                    uri.clone()
                } else {
                    loc.uri.parse().ok()?
                };
                Some(GotoDeclarationResponse::Scalar(Location {
                    uri: target,
                    range: loc.range,
                }))
            });
            connection.sender.send(Message::Response(Response::new_ok(
                req.id,
                serde_json::to_value(result)?,
            )))?;
        }
        References::METHOD => {
            let params: ReferenceParams = serde_json::from_value(req.params)?;
            let uri = &params.text_document_position.text_document.uri;
            let position = params.text_document_position.position;
            // Вхождения ищутся по всей рабочей области: она сканируется в момент
            // запроса - 12 мс на 347 файлов, - поэтому индекса и слежения за файлами
            // сервер не держит.
            let path = uri_to_path(uri);
            let result = takt_lang::lsp::references_in_workspace(
                &path,
                position,
                params.context.include_declaration,
                &state.roots_for(&path),
                &state.search_paths,
                &state.overlay(),
            )
            .map(|refs| {
                refs.into_iter()
                    .map(|r| Location {
                        uri: path_to_uri(&r.path).unwrap_or_else(|| uri.clone()),
                        range: r.range,
                    })
                    .collect::<Vec<_>>()
            });
            connection.sender.send(Message::Response(Response::new_ok(
                req.id,
                serde_json::to_value(result)?,
            )))?;
        }
        PrepareRenameRequest::METHOD => {
            let params: TextDocumentPositionParams = serde_json::from_value(req.params)?;
            // Отказ приходит до ввода нового имени: редактор покажет причину, а
            // пользователь не потратит время впустую. Охват - рабочая область.
            let path = uri_to_path(&params.text_document.uri);
            let response = match takt_lang::lsp::prepare_rename_in_workspace(
                &path,
                params.position,
                &state.roots_for(&path),
                &state.search_paths,
                &state.overlay(),
            ) {
                Ok(range) => Response::new_ok(
                    req.id,
                    serde_json::to_value(PrepareRenameResponse::Range(range))?,
                ),
                Err(refusal) => Response::new_err(
                    req.id,
                    lsp_server::ErrorCode::InvalidRequest as i32,
                    refusal.message().to_string(),
                ),
            };
            connection.sender.send(Message::Response(response))?;
        }
        Rename::METHOD => {
            let params: RenameParams = serde_json::from_value(req.params)?;
            let uri = params.text_document_position.text_document.uri.clone();
            let position = params.text_document_position.position;
            // Либо все вхождения, либо ни одного: частичное переименование портит
            // исходник молча (затенение оставляет текст компилируемым, меняя смысл). С
            // "все" означает "все в рабочей области" - файл вне её сервер не видит
            // никогда.
            let path = uri_to_path(&uri);
            let response = match takt_lang::lsp::rename_in_workspace(
                &path,
                position,
                &params.new_name,
                &state.roots_for(&path),
                &state.search_paths,
                &state.overlay(),
            ) {
                Ok(per_file) => {
                    // `Uri` формально обладает интерьерной мутабельностью (кэш
                    // разбора), из-за чего clippy ругается на ключ словаря. Тип ключа
                    // задан протоколом (`WorkspaceEdit.changes`), и та же пара "Uri ->
                    // ..." уже живёт в состоянии сервера.
                    #[allow(clippy::mutable_key_type)]
                    let mut changes = HashMap::new();
                    for (file, edits) in per_file {
                        let target = path_to_uri(&file).unwrap_or_else(|| uri.clone());
                        changes.insert(target, edits);
                    }
                    let workspace_edit = WorkspaceEdit {
                        changes: Some(changes),
                        ..Default::default()
                    };
                    Response::new_ok(req.id, serde_json::to_value(workspace_edit)?)
                }
                Err(refusal) => Response::new_err(
                    req.id,
                    lsp_server::ErrorCode::InvalidRequest as i32,
                    refusal.message().to_string(),
                ),
            };
            connection.sender.send(Message::Response(response))?;
        }
        "textDocument/documentSymbol" => {
            let params: DocumentSymbolParams = serde_json::from_value(req.params)?;
            let uri = &params.text_document.uri;
            let text = state.get_text(uri).unwrap_or("");
            let symbols = takt_lang::lsp::document_symbols(text);
            connection.sender.send(Message::Response(Response::new_ok(
                req.id,
                serde_json::to_value(symbols)?,
            )))?;
        }
        // Граф модели для схемы. Запрос не из протокола: своего понятия "схема
        // состояний" у LSP нет, а панель редактора без графа нарисовать нечего.
        // Форму ответа строит тот же носитель, что у страницы, - иначе панель
        // рисовала бы не тот автомат.
        "takt/graph" => {
            let params: GraphParams = serde_json::from_value(req.params)?;
            let text = state.get_text(&params.text_document.uri).unwrap_or("");
            let response = match takt_lang::layout::graph_of(text) {
                Ok(graph) => Response::new_ok(
                    req.id,
                    serde_json::to_value(takt_lang::layout::json::graph_json(graph, text))?,
                ),
                // Неразбираемый текст - отказ с диагностикой разбора: панель
                // показывает причину, а не пустой лист без объяснения.
                Err(diagnostic) => Response::new_err(
                    req.id,
                    lsp_server::ErrorCode::InvalidRequest as i32,
                    diagnostic.message.clone(),
                ),
            };
            connection.sender.send(Message::Response(response))?;
        }
        "textDocument/semanticTokens/full" => {
            let params: SemanticTokensParams = serde_json::from_value(req.params)?;
            let uri = &params.text_document.uri;
            let text = state.get_text(uri).unwrap_or("");
            let tokens = takt_lang::lsp::semantic_tokens(text);
            connection.sender.send(Message::Response(Response::new_ok(
                req.id,
                serde_json::to_value(tokens)?,
            )))?;
        }
        _ => {
            // Неизвестный запрос - отвечаем ошибкой "метод не найден"
            connection.sender.send(Message::Response(Response::new_err(
                req.id,
                lsp_server::ErrorCode::MethodNotFound as i32,
                format!("неизвестный метод: {}", req.method),
            )))?;
        }
    }
    Ok(())
}

/// Обрабатывает входящее уведомление от клиента.
fn handle_notification(
    connection: &Connection,
    state: &mut ServerState,
    not: Notification,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    match not.method.as_str() {
        DidOpenTextDocument::METHOD => {
            let params: DidOpenTextDocumentParams = serde_json::from_value(not.params)?;
            let uri = params.text_document.uri;
            let text = params.text_document.text;
            let diagnostics = takt_lang::lsp::collect_diagnostics_at(
                &uri_to_path(&uri),
                &text,
                &state.search_paths,
            );
            state.documents.insert(uri.clone(), text);
            publish_diagnostics(connection, uri, diagnostics)?;
        }
        DidChangeTextDocument::METHOD => {
            let params: DidChangeTextDocumentParams = serde_json::from_value(not.params)?;
            let uri = params.text_document.uri;
            // Используем полный текст документа (sync kind FULL)
            if let Some(change) = params.content_changes.into_iter().last() {
                let text = change.text;
                let diagnostics = takt_lang::lsp::collect_diagnostics_at(
                    &uri_to_path(&uri),
                    &text,
                    &state.search_paths,
                );
                state.documents.insert(uri.clone(), text);
                publish_diagnostics(connection, uri, diagnostics)?;
            }
        }
        DidCloseTextDocument::METHOD => {
            let params: DidCloseTextDocumentParams = serde_json::from_value(not.params)?;
            let uri = params.text_document.uri;
            state.documents.remove(&uri);
            // Очищаем диагностику для закрытого документа
            publish_diagnostics(connection, uri, vec![])?;
        }
        _ => {
            // Неизвестные уведомления игнорируем
        }
    }
    Ok(())
}

/// Путь файла из URI документа.
///
/// Нужен, чтобы редактор разрешал `import` так же, как `taktc`: каталог документа -
/// неявный путь поиска.
///
/// Обрабатывается только схема `file:` - иные (например, `untitled:`) пути не имеют, и
/// импорт для них не разрешится: это честнее, чем угадывать каталог.
fn uri_to_path(uri: &Uri) -> String {
    let raw = uri.as_str();
    let path = raw.strip_prefix("file://").unwrap_or(raw);
    percent_decode(path)
}

/// Путь файла -> URI.
///
/// Кодируются только пробел и `%`: остальное клиенты принимают как есть, а полное
/// процентное кодирование пути дало бы URI, который не совпадёт с тем, что прислал сам
/// клиент, - и редактор счёл бы это другим файлом.
fn path_to_uri(path: &str) -> Option<Uri> {
    let encoded = path.replace('%', "%25").replace(' ', "%20");
    format!("file://{encoded}").parse().ok()
}

/// Раскодирует `%XX` в пути URI: пробелы и кириллица в путях реальны.
fn percent_decode(s: &str) -> String {
    let bytes = s.as_bytes();
    let mut out: Vec<u8> = Vec::with_capacity(bytes.len());
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'%'
            && i + 2 < bytes.len()
            && let Ok(byte) = u8::from_str_radix(&s[i + 1..i + 3], 16)
        {
            out.push(byte);
            i += 3;
            continue;
        }
        out.push(bytes[i]);
        i += 1;
    }
    String::from_utf8_lossy(&out).into_owned()
}

/// Отправляет диагностику клиенту.
fn publish_diagnostics(
    connection: &Connection,
    uri: Uri,
    diagnostics: Vec<Diagnostic>,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let params = PublishDiagnosticsParams {
        uri,
        diagnostics,
        version: None,
    };
    connection
        .sender
        .send(Message::Notification(Notification::new(
            PublishDiagnostics::METHOD.to_string(),
            params,
        )))?;
    Ok(())
}
