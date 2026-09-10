//! Выдача прав и копирование проекта себе.
//!
//! # Права
//!
//! Владелец выдаёт человеку уровень `view`, `fork` либо `edit` (лестница - в
//! [`crate::access`]). Права **по файлам** не заводятся, групп и организаций нет, срока
//! действия у права нет: каждая из этих трёх вещей - своя модель доступа, и заводить их
//! "на будущее" значит хранить правила, которых никто не проверяет.
//!
//! Право выдаётся **логином**, а не идентификатором: владелец знает соседа по имени, а
//! идентификатор проекта и человека - служебные строки. Логин ищется без учёта
//! регистра, как и при входе.
//!
//! Себе право не выдают: владелец и так выше любого уровня, а запись "сам себе `view`"
//! выглядела бы понижением, которого не происходит.
//!
//! # Копирование себе (копия)
//!
//! Новый проект получателя с копией файлов **на момент копирования**, той же версией
//! модуля и видимостью `private`. Связь односторонняя и без синхронизации: правки
//! исходника копию не трогают, удаление исходника её не уносит (`forked_from` объявлен
//! `ON DELETE SET NULL`).
//!
//! Копия считается против пределов получателя - числа проектов и размера: иначе предел
//! обходился бы копированием чужого.

use axum::extract::{Path, State};
use axum::http::{HeaderMap, StatusCode};
use axum::response::{IntoResponse, Response};
use axum::routing::{get, post};
use axum::{Json, Router};
use serde::{Deserialize, Serialize};
use std::sync::Arc;

use crate::access::Level;
use crate::auth::User;
use crate::db;
use crate::error::ApiError;
use crate::limits;
use crate::projects::{self, ProjectJson};
use crate::routes::{AppState, current_user};
use crate::showcase;

/// Выданное право.
#[derive(Debug, Serialize)]
pub struct GrantJson {
    /// Логин того, кому выдано.
    pub login: String,
    pub level: &'static str,
    pub granted_at: i64,
}

/// Запрос выдачи права.
#[derive(Debug, Deserialize)]
pub struct PutGrantRequest {
    pub level: String,
}

/// Копия проекта в чужих руках.
///
/// Показывается только **открытая** копия: закрытая - уже чужой проект, и перечислять
/// его автору исходника незачем. Число копий при этом полное: "сколько раз мой образец
/// взяли" - не то же, что "кто и куда".
#[derive(Debug, Serialize)]
pub struct ForkJson {
    pub id: String,
    pub name: String,
    pub owner: String,
}

/// Маршруты прав и копирования.
pub fn router() -> Router<Arc<AppState>> {
    Router::new()
        .route("/projects/{id}/fork", post(fork))
        .route(
            "/projects/{id}/grants/{login}",
            get(read_grant).put(put_grant).delete(remove_grant),
        )
}

/// Читает выданное право. Спрашивает владелец.
async fn read_grant(
    State(state): State<Arc<AppState>>,
    headers: HeaderMap,
    Path((id, login)): Path<(String, String)>,
) -> Result<Response, ApiError> {
    let user = current_user(&state, &headers).await?;
    let client = state.pool.get().await?;
    owner_of(&client, &id, &user).await?;
    let row = client
        .query_opt(
            "SELECT u.login, g.level, g.granted_at
             FROM project_grants g JOIN users u ON u.id = g.user_id
             WHERE g.project_id = $1 AND lower(u.login) = lower($2)",
            &[&id, &login],
        )
        .await?;
    let Some(row) = row else {
        return Err(ApiError::NotFound);
    };
    Ok(Json(grant_of(&row)).into_response())
}

/// Выдаёт или меняет право.
async fn put_grant(
    State(state): State<Arc<AppState>>,
    headers: HeaderMap,
    Path((id, login)): Path<(String, String)>,
    Json(request): Json<PutGrantRequest>,
) -> Result<Response, ApiError> {
    let user = current_user(&state, &headers).await?;
    let Some(level) = Level::grantable(&request.level) else {
        return Err(ApiError::BadRequest(
            "уровень: 'view', 'fork' либо 'edit'".to_string(),
        ));
    };
    let client = state.pool.get().await?;
    owner_of(&client, &id, &user).await?;
    let Some(row) = client
        .query_opt(
            "SELECT id, login FROM users WHERE lower(login) = lower($1)",
            &[&login],
        )
        .await?
    else {
        // Здесь ручка отвечает о существовании логина - и это осознанно: владелец
        // выдаёт право конкретному человеку и обязан узнать, что ошибся в имени.
        // Молчаливое "готово" оставило бы его в уверенности, что доступ выдан.
        return Err(ApiError::NotFound);
    };
    let target: String = row.get(0);
    let target_login: String = row.get(1);
    if target == user.id {
        return Err(ApiError::BadRequest(
            "право себе не выдают: владелец выше любого уровня".to_string(),
        ));
    }
    client
        .execute(
            "INSERT INTO project_grants(project_id, user_id, level, granted_at)
             VALUES ($1, $2, $3, $4)
             ON CONFLICT (project_id, user_id) DO UPDATE
               SET level = excluded.level, granted_at = excluded.granted_at",
            &[&id, &target, &level.as_str(), &db::now()],
        )
        .await?;
    Ok(Json(GrantJson {
        login: target_login,
        level: level.as_str(),
        granted_at: db::now(),
    })
    .into_response())
}

/// Отбирает право.
///
/// Права, которого не было, отбирается так же успешно: предмет просьбы - "чтобы у него
/// не было доступа", и он выполнен.
async fn remove_grant(
    State(state): State<Arc<AppState>>,
    headers: HeaderMap,
    Path((id, login)): Path<(String, String)>,
) -> Result<Response, ApiError> {
    let user = current_user(&state, &headers).await?;
    let client = state.pool.get().await?;
    owner_of(&client, &id, &user).await?;
    client
        .execute(
            "DELETE FROM project_grants
             WHERE project_id = $1
               AND user_id = (SELECT id FROM users WHERE lower(login) = lower($2))",
            &[&id, &login],
        )
        .await?;
    Ok(StatusCode::NO_CONTENT.into_response())
}

/// Копирует проект себе.
async fn fork(
    State(state): State<Arc<AppState>>,
    headers: HeaderMap,
    Path(id): Path<String>,
) -> Result<Response, ApiError> {
    let user = current_user(&state, &headers).await?;
    let mut client = state.pool.get().await?;
    // Вся копия - Одна транзакция: между чтением исходника и записью копии помещается
    // правка исходника, и копия вышла бы наполовину вчерашней.
    let transaction = client.transaction().await?;
    let (_, level) = projects::locked(&transaction, &state.store, &id, &user).await?;
    projects::require_level(level, Level::Fork)?;

    let source = transaction
        .query_one(
            "SELECT name, takt_lang, language_version, main_file, main_scenario,
                    owner_id, build_target, build_args, run_delays
             FROM projects WHERE id = $1",
            &[&id],
        )
        .await?;
    let owner: String = source.get("owner_id");
    if owner == user.id {
        return Err(ApiError::BadRequest(
            "свой проект копировать незачем: он уже ваш".to_string(),
        ));
    }

    // Предел числа проектов считается внутри транзакции - как и при создании: иначе его
    // обходят двумя копиями сразу.
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

    let copy = projects::new_id();
    let now = db::now();
    let name: String = source.get("name");
    let takt_lang: String = source.get("takt_lang");
    let language_version: String = source.get("language_version");
    let main_file: Option<String> = source.get("main_file");
    // Активный сценарий копируется вместе с составом (09n): копия - тот же проект, и
    // прогон в ней обязан начинаться с того же сценария.
    let main_scenario: Option<String> = source.get("main_scenario");
    // Проверять их заново нечего - они уже проверены при записи в исходный проект.
    let build_target: String = source.get("build_target");
    let build_args: String = source.get("build_args");
    // Темп прогона - часть показа модели автором, и копия показывает её так же.
    let run_delays: String = source.get("run_delays");
    transaction
        .execute(
            "INSERT INTO projects(id, owner_id, name, description, visibility,
                                  takt_lang, language_version, main_file, main_scenario,
                                  build_target, build_args, run_delays, revision,
                                  size_bytes, forked_from, created_at, updated_at,
                                  touched_at)
             VALUES ($1, $2, $3, '', 'private', $4, $5, $6, $7, $8, $9, $10, 0, 0, $11, $12, $12, $12)",
            &[
                &copy,
                &user.id,
                &name,
                &takt_lang,
                &language_version,
                &main_file,
                &main_scenario,
                &build_target,
                &build_args,
                &run_delays,
                &id,
                &now,
            ],
        )
        .await?;
    // Состав копируется запросом, а тексты - на диске: база их больше не хранит.
    transaction
        .execute(
            "INSERT INTO project_files(project_id, name, kind, size_bytes)
             SELECT $1, name, kind, size_bytes FROM project_files WHERE project_id = $2",
            &[&copy, &id],
        )
        .await?;
    let names: Vec<String> = transaction
        .query(
            "SELECT name FROM project_files WHERE project_id = $1 ORDER BY name",
            &[&id],
        )
        .await?
        .iter()
        .map(|row| row.get(0))
        .collect();
    for name in &names {
        let text = state
            .store
            .read(&owner, &id, name)
            .map_err(ApiError::Internal)?;
        state
            .store
            .write(&user.id, &copy, name, &text)
            .map_err(ApiError::Internal)?;
    }
    let size: i64 = transaction
        .query_one(
            "SELECT coalesce(sum(size_bytes), 0)::bigint FROM project_files WHERE project_id = $1",
            &[&copy],
        )
        .await?
        .get(0);
    transaction
        .execute(
            "UPDATE projects SET size_bytes = $2 WHERE id = $1",
            &[&copy, &size],
        )
        .await?;
    let body = projects::body_of(&*transaction, &copy, &state.store, &user.id).await?;
    showcase::refresh(&transaction, &copy, &body).await?;
    let row = transaction
        .query_one(projects::SELECT_PROJECT, &[&copy])
        .await?;
    let copied: ProjectJson = projects::project_of(&row);
    transaction.commit().await?;
    Ok((StatusCode::CREATED, Json(copied)).into_response())
}

/// Читает выданные права проекта. Зовётся показом проекта его владельцу.
///
/// # Ошибки
/// Отказ базы.
pub async fn of_project(
    client: &deadpool_postgres::Client,
    id: &str,
) -> Result<Vec<GrantJson>, ApiError> {
    let rows = client
        .query(
            "SELECT u.login, g.level, g.granted_at
             FROM project_grants g JOIN users u ON u.id = g.user_id
             WHERE g.project_id = $1 ORDER BY u.login",
            &[&id],
        )
        .await?;
    Ok(rows.iter().map(grant_of).collect())
}

/// Считает копии проекта и перечисляет открытые.
///
/// # Ошибки
/// Отказ базы.
pub async fn forks_of(
    client: &deadpool_postgres::Client,
    id: &str,
) -> Result<(i64, Vec<ForkJson>), ApiError> {
    let count: i64 = client
        .query_one(
            "SELECT count(*) FROM projects WHERE forked_from = $1",
            &[&id],
        )
        .await?
        .get(0);
    let rows = client
        .query(
            "SELECT p.id, p.name, u.login FROM projects p JOIN users u ON u.id = p.owner_id
             WHERE p.forked_from = $1 AND p.visibility = 'public'
             ORDER BY p.updated_at DESC",
            &[&id],
        )
        .await?;
    let open = rows
        .iter()
        .map(|row| ForkJson {
            id: row.get(0),
            name: row.get(1),
            owner: row.get(2),
        })
        .collect();
    Ok((count, open))
}

/// Убеждается, что спрашивающий владеет проектом.
async fn owner_of(
    client: &deadpool_postgres::Client,
    id: &str,
    user: &User,
) -> Result<(), ApiError> {
    let row = client
        .query_opt("SELECT owner_id FROM projects WHERE id = $1", &[&id])
        .await?;
    let Some(row) = row else {
        return Err(ApiError::NotFound);
    };
    // Права проекта - дело владельца, и чужому проект здесь **не существует**: `403`
    // сказал бы, что проект с таким адресом есть.
    if row.get::<_, String>(0) != user.id {
        return Err(ApiError::NotFound);
    }
    Ok(())
}

fn grant_of(row: &tokio_postgres::Row) -> GrantJson {
    let level: String = row.get(1);
    GrantJson {
        login: row.get(0),
        // Уровень из базы проходит через ту же лестницу, что и выданный: строка,
        // которой в ней нет, значила бы право без правил.
        level: Level::grantable(&level).unwrap_or(Level::None).as_str(),
        granted_at: row.get(2),
    }
}
