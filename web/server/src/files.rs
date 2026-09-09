//! Файлы проекта: чтение, запись, удаление.

use std::sync::Arc;

use axum::extract::{Path, State};
use axum::http::HeaderMap;
use axum::response::{IntoResponse, Response};
use axum::routing::{get, post};
use axum::{Json, Router};

use crate::access::Level;
use crate::db;
use crate::error::ApiError;
use crate::limits;
use crate::projects::{
    FileJson, PutFileRequest, RenameFileRequest, WriteResponse, body_of, locked, require_level,
    resolve,
};
use crate::routes::{AppState, current_user, optional_user};
use crate::showcase;
use crate::store::Store;

/// Маршруты файлов проекта.
///
/// Путь полный, а роутер примешивается: вложение с внутренним маршрутом `/` дало бы
/// адрес с хвостовой косой чертой.
pub fn router() -> Router<Arc<AppState>> {
    Router::new()
        .route(
            "/projects/{id}/files/{name}",
            get(read_file).put(write_file).delete(remove_file),
        )
        // Переименование - своя ручка, а не пара "записать под новым именем и удалить
        // старое": пара из двух рейсов рвётся посередине, и проект остаётся либо с
        // двумя копиями файла, либо без него вовсе.
        .route("/projects/{id}/files/{name}/rename", post(rename_file))
}

async fn read_file(
    State(state): State<Arc<AppState>>,
    headers: HeaderMap,
    Path((id, name)): Path<(String, String)>,
) -> Result<Response, ApiError> {
    let viewer = optional_user(&state, &headers).await?;
    let client = state.pool.get().await?;
    let (project, _) = resolve(&client, &state.store, &id, viewer.as_ref()).await?;
    let row = client
        .query_opt(
            // Столбцы квалифицированы: `name` есть и у файла, и у проекта, и без
            // квалификации запрос двусмыслен - база отвечает отказом, а ручка
            // превращает его в `500` на совершенно обычном чтении.
            "SELECT f.name, f.kind, f.size_bytes, p.owner_id FROM project_files f
             JOIN projects p ON p.id = f.project_id
             WHERE f.project_id = $1 AND f.name = $2",
            &[&id, &name],
        )
        .await?;
    let Some(row) = row else {
        return Err(ApiError::NotFound);
    };
    let owner: String = row.get(3);
    // Текст живёт на диске: база ведёт состав, а не содержимое.
    let text = state
        .store
        .read(&owner, &id, &name)
        .map_err(ApiError::Internal)?;
    Ok(Json(FileJson {
        name: row.get(0),
        kind: row.get(1),
        text,
        size_bytes: row.get(2),
        revision: project.revision,
    })
    .into_response())
}

async fn write_file(
    State(state): State<Arc<AppState>>,
    headers: HeaderMap,
    Path((id, name)): Path<(String, String)>,
    Json(request): Json<PutFileRequest>,
) -> Result<Response, ApiError> {
    let user = current_user(&state, &headers).await?;
    let kind = limits::check_file_name(&name)?;
    limits::check_file(&request.text)?;

    let mut client = state.pool.get().await?;
    // Вся запись - Одна транзакция со строкой проекта под замком: ревизия, предел числа
    // файлов, предел размера и пересчёт суммы обязаны видеть одно и то же состояние.
    // Порознь их обходит вторая вкладка того же владельца.
    let transaction = client.transaction().await?;
    let (revision, level) = locked(&transaction, &state.store, &id, &user).await?;
    require_level(level, Level::Edit)?;
    let existing: Option<i64> = transaction
        .query_opt(
            "SELECT size_bytes FROM project_files WHERE project_id = $1 AND name = $2",
            &[&id, &name],
        )
        .await?
        .map(|row| row.get(0));

    match (request.revision, existing) {
        // Правка существующего файла обязана назвать ревизию, которую видела.
        (None, Some(_)) => {
            return Err(ApiError::Conflict {
                message: format!(
                    "файл '{name}' уже есть: правка требует ревизии (сейчас {revision})"
                ),
                seen: None,
                actual: revision,
            });
        }
        (Some(seen), _) if seen != revision => {
            return Err(ApiError::Conflict {
                message: format!("проект изменился: у вас ревизия {seen}, у проекта {revision}"),
                seen: Some(seen),
                actual: revision,
            });
        }
        _ => {}
    }

    if existing.is_none() {
        let count: i64 = transaction
            .query_one(
                "SELECT count(*) FROM project_files WHERE project_id = $1",
                &[&id],
            )
            .await?
            .get(0);
        if count >= limits::FILES_PER_PROJECT {
            return Err(limits::exceeded(
                "число файлов в проекте",
                limits::FILES_PER_PROJECT,
                count + 1,
            ));
        }
    }

    let size = request.text.len() as i64;
    let owner: String = transaction
        .query_one("SELECT owner_id FROM projects WHERE id = $1", &[&id])
        .await?
        .get(0);
    // Приведение обязательно: `sum()` над `bigint` в PostgreSQL даёт `numeric`, и
    // чтение его как `i64` кончается отказом разбора столбца.
    let others: i64 = transaction
        .query_one(
            "SELECT coalesce(sum(size_bytes), 0)::bigint FROM project_files
             WHERE project_id = $1 AND name <> $2",
            &[&id, &name],
        )
        .await?
        .get(0);
    if others + size > limits::PROJECT_BYTES {
        return Err(limits::exceeded(
            "размер проекта в байтах",
            limits::PROJECT_BYTES,
            others + size,
        ));
    }

    transaction
        .execute(
            "INSERT INTO project_files(project_id, name, kind, size_bytes)
             VALUES ($1, $2, $3, $4)
             ON CONFLICT (project_id, name) DO UPDATE
               SET kind = excluded.kind, size_bytes = excluded.size_bytes",
            &[&id, &name, &kind.as_str(), &size],
        )
        .await?;
    // Диск пишется до фиксации записи в базе: обратный порядок при отказе диска оставил
    // бы в составе файл, которого нет, - а состав читает страница. Отказ диска здесь
    // откатывает транзакцию, и база остаётся согласной.
    state
        .store
        .write(&owner, &id, &name, &request.text)
        .map_err(ApiError::Internal)?;
    let written = bump(&transaction, &id, &state.store, &owner).await?;
    transaction.commit().await?;
    Ok(Json(written).into_response())
}

/// Переименовывает файл проекта: меняется имя, а не содержимое и не род.
///
/// Род сохраняется намеренно: расширение ставит страница по роду файла, и смена рода
/// переименованием означала бы, что сценарий втихую стал моделью - с прежним текстом
/// внутри. Пара "модель и её раскладка" держится страницей: раскладка парна модели по
/// имени, и переименовать её обязан тот, кто знает про пару.
async fn rename_file(
    State(state): State<Arc<AppState>>,
    headers: HeaderMap,
    Path((id, name)): Path<(String, String)>,
    Json(request): Json<RenameFileRequest>,
) -> Result<Response, ApiError> {
    let user = current_user(&state, &headers).await?;
    let kind = limits::check_file_name(&name)?;
    let target_kind = limits::check_file_name(&request.to)?;
    if kind != target_kind {
        return Err(ApiError::BadRequest(format!(
            "переименование не меняет род файла: '{name}' и '{}' разных родов",
            request.to
        )));
    }
    if request.to == name {
        return Err(ApiError::BadRequest(
            "новое имя совпадает с прежним".to_string(),
        ));
    }
    let mut client = state.pool.get().await?;
    let transaction = client.transaction().await?;
    let (_, level) = locked(&transaction, &state.store, &id, &user).await?;
    require_level(level, Level::Edit)?;
    let owner: String = transaction
        .query_one("SELECT owner_id FROM projects WHERE id = $1", &[&id])
        .await?
        .get(0);
    let taken: i64 = transaction
        .query_one(
            "SELECT count(*) FROM project_files WHERE project_id = $1 AND name = $2",
            &[&id, &request.to],
        )
        .await?
        .get(0);
    if taken > 0 {
        return Err(ApiError::BadRequest(format!(
            "файл '{}' в проекте уже есть",
            request.to
        )));
    }
    let affected = transaction
        .execute(
            "UPDATE project_files SET name = $3 WHERE project_id = $1 AND name = $2",
            &[&id, &name, &request.to],
        )
        .await?;
    if affected == 0 {
        return Err(ApiError::NotFound);
    }
    // Диск переименовывается до фиксации: отказ диска откатывает транзакцию, и состав
    // остаётся согласным с тем, что лежит на диске.
    state
        .store
        .rename(&owner, &id, &name, &request.to)
        .map_err(ApiError::Internal)?;
    // Активный файл и активный сценарий названы именем: не переставь их - и проект
    // откроется, показывая пустоту, а прогон пошёл бы по сценарию, которого нет.
    transaction
        .execute(
            "UPDATE projects SET main_file = $3 WHERE id = $1 AND main_file = $2",
            &[&id, &name, &request.to],
        )
        .await?;
    transaction
        .execute(
            "UPDATE projects SET main_scenario = $3 WHERE id = $1 AND main_scenario = $2",
            &[&id, &name, &request.to],
        )
        .await?;
    let written = bump(&transaction, &id, &state.store, &owner).await?;
    transaction.commit().await?;
    Ok(Json(written).into_response())
}

async fn remove_file(
    State(state): State<Arc<AppState>>,
    headers: HeaderMap,
    Path((id, name)): Path<(String, String)>,
) -> Result<Response, ApiError> {
    let user = current_user(&state, &headers).await?;
    let mut client = state.pool.get().await?;
    let transaction = client.transaction().await?;
    let (_, level) = locked(&transaction, &state.store, &id, &user).await?;
    require_level(level, Level::Edit)?;
    let owner: String = transaction
        .query_one("SELECT owner_id FROM projects WHERE id = $1", &[&id])
        .await?
        .get(0);
    let affected = transaction
        .execute(
            "DELETE FROM project_files WHERE project_id = $1 AND name = $2",
            &[&id, &name],
        )
        .await?;
    if affected == 0 {
        return Err(ApiError::NotFound);
    }
    state
        .store
        .remove(&owner, &id, &name)
        .map_err(ApiError::Internal)?;
    // Активный файл, которого больше нет, забывается: иначе страница откроет проект,
    // показывая пустоту.
    transaction
        .execute(
            "UPDATE projects SET main_file = NULL WHERE id = $1 AND main_file = $2",
            &[&id, &name],
        )
        .await?;
    // То же и с активным сценарием (09n): забудь его - и прогон пошёл бы по сценарию,
    // которого нет, а страница молчала бы об этом.
    transaction
        .execute(
            "UPDATE projects SET main_scenario = NULL WHERE id = $1 AND main_scenario = $2",
            &[&id, &name],
        )
        .await?;
    let written = bump(&transaction, &id, &state.store, &owner).await?;
    transaction.commit().await?;
    Ok(Json(written).into_response())
}

/// Пересчитывает размер, поднимает ревизию и отмечает правку.
///
/// Размер считается **суммой по файлам**, а не приращением: приращение расходится с
/// истиной на первой же неудачной попытке, и разойдётся молча.
async fn bump(
    transaction: &tokio_postgres::Transaction<'_>,
    id: &str,
    store: &Store,
    owner: &str,
) -> Result<WriteResponse, ApiError> {
    // Тексты файлов идут в поиск, и пересчёт стоит **в той же транзакции**: отдельным
    // запросом он разошёлся бы с содержимым при первом же откате. Тело берётся с диска:
    // базе его взять неоткуда.
    let body = body_of(transaction, id, store, owner).await?;
    showcase::refresh(transaction, id, &body).await?;
    let row = transaction
        .query_one(
            "UPDATE projects SET
                 revision = revision + 1,
                 updated_at = $2,
                 size_bytes = (SELECT coalesce(sum(size_bytes), 0)::bigint
                               FROM project_files WHERE project_id = $1)
             WHERE id = $1
             RETURNING revision, size_bytes",
            &[&id, &db::now()],
        )
        .await?;
    Ok(WriteResponse {
        revision: row.get(0),
        size_bytes: row.get(1),
    })
}
