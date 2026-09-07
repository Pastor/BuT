//! Ручки архива: выгрузка и загрузка проекта.
//!
//! # Где собирается архив
//!
//! **На сервере** - Вывод целей в архиве получен
//! исполнением **того же байт-кода**, что и в браузере ([`crate::module`]), и
//! версия модуля берётся у проекта. Отсюда следствие, ради
//! которого решение и принималось: ссылка "скачать" работает **без открытой
//! страницы**.
//!
//! # Что кладётся
//!
//! Исходники - всегда. Вывод - **выбранной цели**: одна модель под восемь целей даёт до
//! полутора мегабайт текста, и снимок "всего" стоил бы дороже самого проекта. Отказ
//! цели - нормальный ответ, и он тоже попадает в архив, словами.
//!
//! # Кому можно
//!
//! Выгрузка - с уровня `view`: текст уже виден тому, кто читает проект, и
//! запрет скачать его неисполним. Загрузка - любому, кто вошёл: она заводит
//! **свой** проект и считается против его пределов.

use axum::body::Bytes;
use axum::extract::{Path, Query, State};
use axum::http::{HeaderMap, HeaderValue, StatusCode, header};
use axum::response::{IntoResponse, Response};
use axum::routing::{get, post};
use axum::{Json, Router};
use serde::Deserialize;
use std::sync::Arc;

use crate::access::Level;
use crate::archive::{self, Export, SourceFile};
use crate::db;
use crate::error::ApiError;
use crate::limits;
use crate::projects::{self, ProjectJson};
use crate::routes::{AppState, current_user, optional_user};
use crate::showcase;

/// Запрос выгрузки.
#[derive(Debug, Deserialize)]
pub struct ExportQuery {
    /// Цель генерации; пусто - архив только с исходниками.
    #[serde(default)]
    pub target: Option<String>,
    /// Ключи сборки, как у `taktc compile`; пусто - ключи проекта.
    ///
    /// Умолчание - не пустая строка: проект несёт выбор автора, и архив, собранный без
    /// его ключей, показывал бы вывод, которого автор не видел.
    #[serde(default)]
    pub args: Option<String>,
    /// Какой файл компилировать; пусто - активный.
    #[serde(default)]
    pub file: Option<String>,
}

/// Маршруты архива.
pub fn router() -> Router<Arc<AppState>> {
    Router::new()
        .route("/projects/{id}/archive", get(export))
        .route("/projects/import", post(import))
}

/// Выгружает проект архивом.
async fn export(
    State(state): State<Arc<AppState>>,
    headers: HeaderMap,
    Path(id): Path<String>,
    Query(request): Query<ExportQuery>,
) -> Result<Response, ApiError> {
    let viewer = optional_user(&state, &headers).await?;
    let client = state.pool.get().await?;
    let (project, level) = projects::resolve(&client, &state.store, &id, viewer.as_ref()).await?;
    // Выгрузка - с уровня чтения: текст уже виден тому, кто читает проект.
    projects::require_level(level, Level::View)?;

    let rows = client
        .query(
            "SELECT f.name, f.kind, p.owner_id FROM project_files f
             JOIN projects p ON p.id = f.project_id
             WHERE f.project_id = $1 ORDER BY f.name",
            &[&id],
        )
        .await?;
    // Тексты живут на диске: база ведёт состав.
    let mut sources: Vec<SourceFile> = Vec::new();
    for row in &rows {
        let name: String = row.get(0);
        let owner: String = row.get(2);
        let text = state
            .store
            .read(&owner, &id, &name)
            .map_err(ApiError::Internal)?;
        sources.push(SourceFile {
            name,
            kind: row.get(1),
            text,
        });
    }

    let (generated, refusal, target) = match request.target.as_deref().filter(|t| !t.is_empty()) {
        None => (Vec::new(), None, None),
        Some(target) => {
            let chosen = request
                .file
                .clone()
                .or_else(|| project.main_file.clone())
                .or_else(|| {
                    sources
                        .iter()
                        .find(|file| file.kind == "takt")
                        .map(|file| file.name.clone())
                });
            let Some(chosen) = chosen else {
                return Err(ApiError::BadRequest(
                    "в проекте нет файла модели: компилировать нечего".to_string(),
                ));
            };
            let Some(source) = sources.iter().find(|file| file.name == chosen) else {
                return Err(ApiError::BadRequest(format!(
                    "файла '{chosen}' в проекте нет"
                )));
            };
            // Имя файла - часть вывода: имя корневой модели берётся из него. Оно
            // передаётся модулю позиционным аргументом, ровно как в командной строке.
            let chosen_args = request
                .args
                .clone()
                .unwrap_or_else(|| project.build_args.clone());
            let args = format!("{chosen_args} {chosen}");
            let modules = state.modules.as_ref().ok_or_else(|| {
                ApiError::BadRequest(
                    "сборка с генерацией недоступна: модуля нет в статике".to_string(),
                )
            })?;
            match modules.compile(&project.takt_lang, target, args.trim(), &source.text) {
                Ok(files) => (files, None, Some(target.to_string())),
                // Отказ цели - нормальный ответ, а не ошибка сервиса: он записывается в
                // архив словами. Молча пропущенный вывод неотличим от "цель ничего не
                // печатает".
                Err(error) => (
                    Vec::new(),
                    Some(error.to_string()),
                    Some(target.to_string()),
                ),
            }
        }
    };

    let manifest = archive::manifest_of(&project, &sources, target);
    let bytes = archive::pack(&Export {
        manifest,
        sources,
        generated,
        refusal,
    })
    .map_err(ApiError::Internal)?;

    let name = archive::file_name(&project.name);
    Ok((
        [
            (
                header::CONTENT_TYPE,
                HeaderValue::from_static("application/zip"),
            ),
            (
                header::CONTENT_DISPOSITION,
                HeaderValue::from_str(&format!("attachment; filename=\"{name}\""))
                    .unwrap_or(HeaderValue::from_static("attachment")),
            ),
        ],
        bytes,
    )
        .into_response())
}

/// Загружает проект из архива.
///
/// Заводится **новый** проект: загрузка поверх существующего означала бы молчаливую
/// перезапись чужой (или своей вчерашней) работы - того самого, от чего бережёт
/// ревизия.
async fn import(
    State(state): State<Arc<AppState>>,
    headers: HeaderMap,
    body: Bytes,
) -> Result<Response, ApiError> {
    let user = current_user(&state, &headers).await?;
    let parsed = archive::unpack(&body)?;

    let mut client = state.pool.get().await?;
    // Вся загрузка - Одна транзакция: предел числа проектов, вставка и файлы обязаны
    // видеть одно состояние, иначе предел обходится двумя загрузками сразу.
    let transaction = client.transaction().await?;
    let count: i64 = transaction
        .query_one(
            "SELECT count(*) FROM projects WHERE owner_id = $1",
            &[&user.id],
        )
        .await?
        .get(0);
    if count >= limits::PROJECTS_PER_USER {
        return Err(limits::exceeded(
            "число проектов у владельца",
            limits::PROJECTS_PER_USER,
            count + 1,
        ));
    }

    // Версия модуля берётся из архива, но проверяется: она попадёт в путь при сборке
    // архива и в выбор модуля страницей.
    let takt_lang = if crate::module::is_version(&parsed.manifest.takt_lang) {
        parsed.manifest.takt_lang.clone()
    } else {
        state.module_version.clone()
    };
    // Цель и ключи автора проверяются, когда есть чем: архив - чужой вход, и негодная
    // пара показала бы читателю сборку, которой нет. Не прошла проверку либо проверить
    // нечем - берутся умолчания: терять проект из-за строки ключей несоразмерно. Тот же
    // приём, что у версии модуля строкой выше.
    let (build_target, build_args) = match build_pair(&state, &takt_lang, &parsed.manifest) {
        Some(pair) => pair,
        None => (DEFAULT_TARGET.to_string(), String::new()),
    };
    // Активный файл обязан существовать среди загруженных: иначе страница откроет
    // проект, показывая пустоту.
    let main_file = parsed
        .manifest
        .main_file
        .filter(|name| parsed.sources.iter().any(|file| &file.name == name));
    // Активный сценарий (09n) судится тем же правилом и дополнительно по виду: назови
    // архив сценарием пояснение - прогон пошёл бы по тексту.
    let main_scenario = parsed.manifest.main_scenario.filter(|name| {
        parsed
            .sources
            .iter()
            .any(|file| &file.name == name && file.kind == limits::Kind::Scenario.as_str())
    });

    let id = projects::new_id();
    let now = db::now();
    let size: i64 = parsed
        .sources
        .iter()
        .map(|file| file.text.len() as i64)
        .sum();
    transaction
        .execute(
            "INSERT INTO projects(id, owner_id, name, description, visibility,
                                  takt_lang, language_version, main_file, main_scenario,
                                  build_target, build_args, revision,
                                  size_bytes, created_at, updated_at, touched_at)
             VALUES ($1, $2, $3, $4, 'private', $5, $6, $7, $8, $9, $10, 1, $11, $12, $12, $12)",
            &[
                &id,
                &user.id,
                &parsed.manifest.name,
                &parsed.manifest.description,
                &takt_lang,
                &state.language_version,
                &main_file,
                &main_scenario,
                &build_target,
                &build_args,
                &size,
                &now,
            ],
        )
        .await?;
    for file in &parsed.sources {
        transaction
            .execute(
                "INSERT INTO project_files(project_id, name, kind, size_bytes)
                 VALUES ($1, $2, $3, $4)",
                &[&id, &file.name, &file.kind, &(file.text.len() as i64)],
            )
            .await?;
        state
            .store
            .write(&user.id, &id, &file.name, &file.text)
            .map_err(ApiError::Internal)?;
    }
    let body = projects::body_of(&*transaction, &id, &state.store, &user.id).await?;
    showcase::refresh(&transaction, &id, &body).await?;
    let row = transaction
        .query_one(projects::SELECT_PROJECT, &[&id])
        .await?;
    let created: ProjectJson = projects::project_of(&row);
    transaction.commit().await?;
    Ok((StatusCode::CREATED, Json(created)).into_response())
}

/// Цель по умолчанию: ею открывается проект, у которого выбора нет.
///
/// Значение то же, что в схеме (`build_target ... DEFAULT 'c'`), и это сознательное
/// повторение одной величины в двух видах: база отвечает за строку, уже лежащую в ней,
/// а эта константа - за строку, которую сервер подставляет сам. Тест - проверка
/// загрузки архива без пары.
const DEFAULT_TARGET: &str = "c";

/// Цель и ключи из архива, если они прошли проверку.
///
/// `None` - пары в архиве нет (прежняя версия формата), она не прошла разбор либо
/// проверить её нечем.
fn build_pair(
    state: &Arc<AppState>,
    version: &str,
    manifest: &archive::Manifest,
) -> Option<(String, String)> {
    if manifest.build_target.is_empty() {
        return None;
    }
    if limits::check_build_args(&manifest.build_args).is_err() {
        return None;
    }
    let modules = state.modules.as_ref()?;
    modules
        .check_flags(version, &manifest.build_target, &manifest.build_args)
        .ok()?;
    Some((manifest.build_target.clone(), manifest.build_args.clone()))
}
