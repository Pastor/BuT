//! Проекты и их файлы.
//!
//! # Что такое проект
//!
//! Именованное хранилище исходников одного владельца: файлы `*.takt` и сценарии
//! `*.json`. Вывода целей, трасс, кадров и диагностик здесь нет: всё это воспроизводит
//! модуль версии проекта - генерация детерминирована, а раскладка кадров сеяна, - и
//! хранение стоило бы на порядок больше исходника.
//!
//! # Ревизия
//!
//! Счётчик обнаружения конфликта, **не журнал**: истории правок проект не хранит.
//! Запись файла шлёт ревизию, которую видела; разошлась - `409`, и выбор ("перечитать"
//! либо "перезаписать") делает автор, а не сервер.
//!
//! # Размер
//!
//! `projects.size_bytes` - сумма размеров файлов, и она пересчитывается **в той же
//! транзакции**, что запись. Считать её отдельным запросом значило бы иметь два ответа
//! на один вопрос: параллельная запись развела бы их молча.
//!
//! Чужой проект отвечает **`404`**, а не `403`: иначе ручка становится оракулом
//! существования - по ответам перечисляются чужие проекты.

use axum::extract::{Path, State};
use axum::http::{HeaderMap, StatusCode};
use axum::response::{IntoResponse, Response};
use axum::routing::get;
use axum::{Json, Router};
use serde::{Deserialize, Serialize};
use std::sync::Arc;

use crate::access::{self, Level};
use crate::auth::User;
use crate::db;
use crate::error::ApiError;
use crate::grants;
use crate::limits;
use crate::limits::Kind;
use crate::retention;
use crate::routes::{AppState, current_user, optional_user};
use crate::showcase;
use crate::store::Store;

/// Метаданные проекта.
#[derive(Debug, Serialize)]
pub struct ProjectJson {
    pub id: String,
    pub name: String,
    pub description: String,
    pub visibility: String,
    /// Логин владельца - псевдоним, а не персональные данные. Нужен
    /// странице открытого проекта: "чей это образец".
    pub owner: String,
    /// Версия модуля, которой открывается проект.
    pub takt_lang: String,
    pub language_version: String,
    pub main_file: Option<String>,
    /// Активный сценарий прогона; `null` - не назначен.
    ///
    /// Правило то же, что у [`ProjectJson::build_target`]: проект называет свой,
    /// черновик читателя его перекрывает.
    pub main_scenario: Option<String>,
    /// Цель сборки, выбранная автором.
    ///
    /// Свойство проекта, а не файла: проект с библиотекой и прошивкой собирается одной
    /// целью. Страница берёт её умолчанием при открытии, а черновик автора её
    /// перекрывает.
    pub build_target: String,
    /// Ключи сборки одной строкой - те же, что у `taktc compile`.
    pub build_args: String,
    pub revision: i64,
    pub size_bytes: i64,
    pub forked_from: Option<String>,
    pub created_at: i64,
    pub updated_at: i64,
}

/// Файл в списке: без текста - список бывает длинным.
#[derive(Debug, Serialize)]
pub struct FileEntryJson {
    pub name: String,
    pub kind: String,
    pub size_bytes: i64,
}

/// Запись списка проектов: сам проект и мой уровень доступа к нему.
///
/// Уровень назван **в каждой записи**: без него страница не знает, показать ли кнопку
/// сохранения, и решала бы это по владельцу - то есть завела бы вторую копию правила.
#[derive(Debug, Serialize)]
pub struct ProjectListJson {
    #[serde(flatten)]
    pub project: ProjectJson,
    pub level: &'static str,
}

/// Проект вместе со списком файлов.
#[derive(Debug, Serialize)]
pub struct ProjectWithFilesJson {
    #[serde(flatten)]
    pub project: ProjectJson,
    pub files: Vec<FileEntryJson>,
    /// Мой уровень доступа: по нему страница решает, показывать ли сохранение.
    pub level: &'static str,
    /// Выданные права - **только владельцу**.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub grants: Option<Vec<grants::GrantJson>>,
    /// Сколько раз проект скопировали себе (владельцу; иначе 0).
    pub forks: i64,
    /// Открытые копии: закрытая копия - уже чужой проект.
    pub open_forks: Vec<grants::ForkJson>,
}

/// Файл целиком.
#[derive(Debug, Serialize)]
pub struct FileJson {
    pub name: String,
    pub kind: String,
    pub text: String,
    pub size_bytes: i64,
    /// Ревизия проекта на момент чтения: её и шлют обратно при записи.
    pub revision: i64,
}

/// Запрос создания проекта.
#[derive(Debug, Deserialize)]
pub struct CreateRequest {
    pub name: String,
    #[serde(default)]
    pub description: String,
    /// Версия модуля; пусто - последняя, которую знает сервер.
    #[serde(default)]
    pub takt_lang: Option<String>,
}

/// Запрос правки метаданных. Отсутствующее поле не трогается.
#[derive(Debug, Deserialize)]
pub struct PatchRequest {
    #[serde(default)]
    pub name: Option<String>,
    #[serde(default)]
    pub description: Option<String>,
    #[serde(default)]
    pub visibility: Option<String>,
    #[serde(default)]
    pub main_file: Option<String>,
    /// Активный сценарий: имя файла вида `scenario`.
    #[serde(default)]
    pub main_scenario: Option<String>,
    /// Цель сборки; проверяется вместе с ключами.
    #[serde(default)]
    pub build_target: Option<String>,
    /// Ключи сборки одной строкой.
    #[serde(default)]
    pub build_args: Option<String>,
    /// Подъём версии модуля - **явное действие владельца**: после него
    /// вывод целей может измениться.
    #[serde(default)]
    pub takt_lang: Option<String>,
}

/// Запрос записи файла.
#[derive(Debug, Deserialize)]
pub struct PutFileRequest {
    pub text: String,
    /// Ревизия, которую видел автор. Пусто - только у нового файла.
    #[serde(default)]
    pub revision: Option<i64>,
}

/// Запрос переименования файла.
#[derive(Debug, Deserialize)]
pub struct RenameFileRequest {
    /// Новое имя целиком, с расширением: род файла им и задаётся.
    pub to: String,
}

/// Ответ записи: новая ревизия и новый размер проекта.
#[derive(Debug, Serialize)]
pub struct WriteResponse {
    pub revision: i64,
    pub size_bytes: i64,
}

/// Маршруты проектов.
///
/// Состояние не подставляется здесь: его ставит внешний роутер один раз на всё дерево
/// (`routes::router`). Подставь его дважды - и вложение перестанет собираться, потому
/// что типы состояния разойдутся.
pub fn router() -> Router<Arc<AppState>> {
    // Пути полные, а роутер примешивается, а не вкладывается: вложение с внутренним
    // маршрутом `/` даёт адрес с хвостовой косой чертой, и `/api/projects` отвечал бы
    // `404`.
    Router::new()
        .route("/projects", get(list).post(create))
        .route("/projects/{id}", get(read).patch(patch).delete(remove))
        // Файлы проекта живут своим модулем (`files.rs`): предмет другой - содержимое,
        // ревизия и пределы, а не метаданные.
        .merge(crate::files::router())
}

async fn list(
    State(state): State<Arc<AppState>>,
    headers: HeaderMap,
) -> Result<Response, ApiError> {
    let user = current_user(&state, &headers).await?;
    let client = state.pool.get().await?;
    // Свои И выданные мне - одним запросом с уровнем. Открытые чужие сюда не попадают:
    // "мои проекты" - это то, за что я отвечаю, а открытое чужое живёт в витрине.
    // Смешай их - и список станет каталогом сервиса.
    let rows = client
        .query(
            "SELECT p.id, p.name, p.description, p.visibility, u.login AS owner,
                    p.takt_lang, p.language_version, p.main_file,
                    p.main_scenario, p.build_target, p.build_args, p.revision,
                    p.size_bytes, p.forked_from, p.created_at, p.updated_at,
                    g.level AS granted
             FROM projects p
             JOIN users u ON u.id = p.owner_id
             LEFT JOIN project_grants g ON g.project_id = p.id AND g.user_id = $1
             WHERE p.owner_id = $1 OR g.user_id = $1
             ORDER BY p.updated_at DESC",
            &[&user.id],
        )
        .await?;
    let projects: Vec<ProjectListJson> = rows
        .iter()
        .map(|row| {
            let project = project_of(row);
            let granted: Option<String> = row.get("granted");
            let level = access::effective(
                row.get::<_, String>("owner") == user.login,
                &project.visibility,
                granted.as_deref().and_then(Level::grantable),
            );
            ProjectListJson {
                project,
                level: level.as_str(),
            }
        })
        .collect();
    Ok(Json(projects).into_response())
}

async fn create(
    State(state): State<Arc<AppState>>,
    headers: HeaderMap,
    Json(request): Json<CreateRequest>,
) -> Result<Response, ApiError> {
    let user = current_user(&state, &headers).await?;
    limits::check_project_name(&request.name)?;
    limits::check_description(&request.description)?;
    let mut client = state.pool.get().await?;
    // Счёт и вставка - в одной транзакции: между отдельными запросами помещается вторая
    // вкладка того же владельца, и предел обходится вдвоём с самим собой.
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
    let id = new_id();
    let now = db::now();
    let takt_lang = request
        .takt_lang
        .unwrap_or_else(|| state.module_version.clone());
    transaction
        .execute(
            // `touched_at` ставится сразу: ноль означал бы "не обращались никогда", и
            // подметание свернуло бы проект в тот же час, когда его завели.
            "INSERT INTO projects(id, owner_id, name, description, visibility,
                                  takt_lang, language_version, revision, size_bytes,
                                  created_at, updated_at, touched_at)
             VALUES ($1, $2, $3, $4, 'private', $5, $6, 0, 0, $7, $7, $7)",
            &[
                &id,
                &user.id,
                &request.name,
                &request.description,
                &takt_lang,
                &state.language_version,
                &now,
            ],
        )
        .await?;
    // У нового проекта файлов ещё нет - тело поиска пусто, и это законно.
    showcase::refresh(&transaction, &id, "").await?;
    let row = transaction.query_one(SELECT_PROJECT, &[&id]).await?;
    let project = project_of(&row);
    transaction.commit().await?;
    Ok((StatusCode::CREATED, Json(project)).into_response())
}

async fn read(
    State(state): State<Arc<AppState>>,
    headers: HeaderMap,
    Path(id): Path<String>,
) -> Result<Response, ApiError> {
    let viewer = optional_user(&state, &headers).await?;
    let client = state.pool.get().await?;
    let (project, level) = resolve(&client, &state.store, &id, viewer.as_ref()).await?;
    let rows = client
        .query(
            "SELECT name, kind, size_bytes FROM project_files
             WHERE project_id = $1 ORDER BY name",
            &[&id],
        )
        .await?;
    let files = rows
        .iter()
        .map(|row| FileEntryJson {
            name: row.get(0),
            kind: row.get(1),
            size_bytes: row.get(2),
        })
        .collect();
    // Права и копии показываются только владельцу: "кому я это открыл" - его дело, а
    // читателю список тех, кто ещё имеет доступ, не принадлежит.
    let (grants, forks, open_forks) = if level == Level::Owner {
        let (count, open) = grants::forks_of(&client, &id).await?;
        (Some(grants::of_project(&client, &id).await?), count, open)
    } else {
        (None, 0, Vec::new())
    };
    Ok(Json(ProjectWithFilesJson {
        project,
        files,
        level: level.as_str(),
        grants,
        forks,
        open_forks,
    })
    .into_response())
}

async fn patch(
    State(state): State<Arc<AppState>>,
    headers: HeaderMap,
    Path(id): Path<String>,
    Json(request): Json<PatchRequest>,
) -> Result<Response, ApiError> {
    let user = current_user(&state, &headers).await?;
    let mut client = state.pool.get().await?;
    let transaction = client.transaction().await?;
    let (_, level) = locked(&transaction, &state.store, &id, &user).await?;
    // Метаданные - только владелец: `edit` правит содержимое, а видимость, права и
    // версия модуля меняют, кому и чем проект открывается.
    require_level(level, Level::Owner)?;
    if let Some(name) = &request.name {
        limits::check_project_name(name)?;
        transaction
            .execute("UPDATE projects SET name = $1 WHERE id = $2", &[name, &id])
            .await?;
    }
    if let Some(description) = &request.description {
        limits::check_description(description)?;
        transaction
            .execute(
                "UPDATE projects SET description = $1 WHERE id = $2",
                &[description, &id],
            )
            .await?;
    }
    if let Some(visibility) = &request.visibility {
        // Поле ставится здесь, а действует с c: до неё чужой проект недоступен при
        // любом значении. Разрешить его сейчас честнее, чем отвергать: колонка есть, и
        // правило её значений - в схеме.
        if !["private", "link", "public"].contains(&visibility.as_str()) {
            return Err(ApiError::BadRequest(
                "видимость: 'private', 'link' либо 'public'".to_string(),
            ));
        }
        transaction
            .execute(
                "UPDATE projects SET visibility = $1 WHERE id = $2",
                &[visibility, &id],
            )
            .await?;
    }
    if let Some(main_file) = &request.main_file {
        // Активным можно назначить только тот файл, который есть: иначе страница
        // откроет проект, показывая пустоту. И только модель: с появлением пояснений
        // (09n) назначить активным `.md` стало можно, а страница открывает активный
        // файл как модель.
        has_kind(&transaction, &id, main_file, Kind::Takt, "активный файл").await?;
        transaction
            .execute(
                "UPDATE projects SET main_file = $1 WHERE id = $2",
                &[main_file, &id],
            )
            .await?;
    }
    if let Some(main_scenario) = &request.main_scenario {
        // Сценариев бывает несколько (09n), и "первый по имени" - не ответ: автор
        // показывает работу модели на одном из них.
        has_kind(
            &transaction,
            &id,
            main_scenario,
            Kind::Scenario,
            "активный сценарий",
        )
        .await?;
        transaction
            .execute(
                "UPDATE projects SET main_scenario = $1 WHERE id = $2",
                &[main_scenario, &id],
            )
            .await?;
    }
    if let Some(takt_lang) = &request.takt_lang {
        transaction
            .execute(
                "UPDATE projects SET takt_lang = $1 WHERE id = $2",
                &[takt_lang, &id],
            )
            .await?;
    }
    // Цель и ключи проверяются парой, а не по отдельности: применимость ключа -
    // свойство пары (`--bus=apb` годится `sv-mmio` и не годится `rust`), и смена одной
    // половины делает негодной другую. Отсюда чтение недостающей половины из базы.
    if request.build_target.is_some() || request.build_args.is_some() {
        let current = transaction
            .query_one(
                "SELECT build_target, build_args, takt_lang FROM projects WHERE id = $1",
                &[&id],
            )
            .await?;
        let target = request
            .build_target
            .clone()
            .unwrap_or_else(|| current.get(0));
        let args = request.build_args.clone().unwrap_or_else(|| current.get(1));
        let version: String = current.get(2);
        limits::check_build_args(&args)?;
        check_build(&state, &version, &target, &args)?;
        transaction
            .execute(
                "UPDATE projects SET build_target = $1, build_args = $2 WHERE id = $3",
                &[&target, &args, &id],
            )
            .await?;
    }
    transaction
        .execute(
            "UPDATE projects SET updated_at = $1 WHERE id = $2",
            &[&db::now(), &id],
        )
        .await?;
    // Имя и описание попадают в поиск: без пересчёта проект искался бы по
    // позавчерашнему имени, и увидеть это глазом нельзя. Тело при этом перечитывается с
    // диска: поисковое значение - одно, и собирается оно целиком.
    let owner: String = transaction
        .query_one("SELECT owner_id FROM projects WHERE id = $1", &[&id])
        .await?
        .get(0);
    let body = body_of(&*transaction, &id, &state.store, &owner).await?;
    showcase::refresh(&transaction, &id, &body).await?;
    let row = transaction.query_one(SELECT_PROJECT, &[&id]).await?;
    let project = project_of(&row);
    transaction.commit().await?;
    Ok(Json(project).into_response())
}

async fn remove(
    State(state): State<Arc<AppState>>,
    headers: HeaderMap,
    Path(id): Path<String>,
) -> Result<Response, ApiError> {
    let user = current_user(&state, &headers).await?;
    let client = state.pool.get().await?;
    let (_, level) = resolve(&client, &state.store, &id, Some(&user)).await?;
    require_level(level, Level::Owner)?;
    // Состав и права уходят каскадом (схема), копии - нет: у копии своя жизнь
    // (`forked_from` объявлен `ON DELETE SET NULL`).
    client
        .execute("DELETE FROM projects WHERE id = $1", &[&id])
        .await?;
    // Диск чистится после базы: обратный порядок при отказе базы оставил бы проект в
    // списке без единого файла. Осиротевший каталог хуже, но он виден подметанию, а
    // проект без исходников не виден никому.
    state
        .store
        .remove_project(&user.id, &id)
        .map_err(ApiError::Internal)?;
    Ok(StatusCode::NO_CONTENT.into_response())
}

/// Читает ревизию и уровень доступа, взяв строку проекта под замок.
///
/// Право спрашивается **внутри той же транзакции**, что и запись: между отдельными
/// запросами помещается отзыв права, и запись прошла бы по уже отобранному.
pub(crate) async fn locked(
    transaction: &tokio_postgres::Transaction<'_>,
    store: &Arc<Store>,
    id: &str,
    user: &User,
) -> Result<(i64, Level), ApiError> {
    let row = transaction
        .query_opt(
            "SELECT owner_id, visibility, revision, archived_at, touched_at
             FROM projects WHERE id = $1 FOR UPDATE",
            &[&id],
        )
        .await?;
    let Some(row) = row else {
        return Err(ApiError::NotFound);
    };
    let owner: String = row.get(0);
    let visibility: String = row.get(1);
    let revision: i64 = row.get(2);
    let granted = transaction
        .query_opt(
            "SELECT level FROM project_grants WHERE project_id = $1 AND user_id = $2",
            &[&id, &user.id],
        )
        .await?
        .map(|row| row.get::<_, String>(0));
    let level = access::effective(
        user.id == owner,
        &visibility,
        granted.as_deref().and_then(Level::grantable),
    );
    if level == Level::None {
        return Err(ApiError::NotFound);
    }
    // Запись - тоже обращение: счётчик сбрасывается, свёрнутый проект разворачивается.
    // Иначе запись легла бы поверх снятых с диска файлов.
    retention::touch(transaction, store, id, &owner, row.get(3), row.get(4))
        .await
        .map_err(ApiError::Internal)?;
    Ok((revision, level))
}

/// Собирает тело поиска - тексты файлов `*.takt` одной строкой.
///
/// Список имён берёт база, а тексты - диск: каталог знает про файлы, которых нет в
/// проекте (обрывок записи), а база - про состав.
///
/// Отказ чтения файла телом не считается ошибкой запроса: проект мог быть свёрнут либо
/// файл потерян, и терять из-за этого правку соседнего файла было бы хуже, чем искать
/// по неполному телу. Пропуск виден в журнале.
pub(crate) async fn body_of<C: tokio_postgres::GenericClient>(
    client: &C,
    id: &str,
    store: &Store,
    owner: &str,
) -> Result<String, ApiError> {
    // В поиск идут модель И пояснение: по слову из описания проект ищется охотнее, чем
    // по имени состояния. Сценарий не идёт: это числа и имена портов, и поиск по ним
    // дал бы совпадения там, где читатель ничего не искал.
    let rows = client
        .query(
            "SELECT name FROM project_files
             WHERE project_id = $1 AND kind IN ('takt', 'markdown') ORDER BY name",
            &[&id],
        )
        .await?;
    let mut body = String::new();
    for row in &rows {
        let name: String = row.get(0);
        match store.read(owner, id, &name) {
            Ok(text) => {
                body.push_str(&text);
                body.push('\n');
            }
            Err(error) => tracing::warn!(%error, project = %id, file = %name, "файл не прочитан"),
        }
    }
    Ok(body)
}

/// Читает проект и уровень доступа к нему спрашивающего.
///
/// Уровень считает один носитель [`access::effective`]; здесь только сбор исходных
/// данных. Ответь эта функция сама - правило стало бы жить в двух местах, и расхождение
/// проявилось бы не отказом, а чужой записью в чужой проект.
///
/// Проект, недоступный вовсе, отвечает `404`, а не `403`: `403` означал бы "он есть, но
/// не для вас", то есть ручка стала бы оракулом существования.
pub(crate) async fn resolve(
    client: &deadpool_postgres::Client,
    store: &Arc<Store>,
    id: &str,
    viewer: Option<&User>,
) -> Result<(ProjectJson, Level), ApiError> {
    let row = client.query_opt(SELECT_PROJECT, &[&id]).await?;
    let Some(row) = row else {
        return Err(ApiError::NotFound);
    };
    let project = project_of(&row);
    let owner: String = row.get("owner_id");
    let level = level_of(client, id, &owner, &project.visibility, viewer).await?;
    if level == Level::None {
        return Err(ApiError::NotFound);
    }
    // Обращение на чтение тоже сбрасывает счётчик хранения: пропусти его - и проект,
    // который читают каждый день, однажды свернётся под руками читателя. Здесь же
    // свёрнутый проект разворачивается обратно.
    retention::touch(
        &***client,
        store,
        id,
        &owner,
        row.get("archived_at"),
        row.get("touched_at"),
    )
    .await
    .map_err(ApiError::Internal)?;
    Ok((project, level))
}

/// Считает уровень доступа, спросив выданное право.
async fn level_of(
    client: &deadpool_postgres::Client,
    id: &str,
    owner: &str,
    visibility: &str,
    viewer: Option<&User>,
) -> Result<Level, ApiError> {
    let Some(user) = viewer else {
        // Без входа выданного права быть не может: оно выдаётся человеку.
        return Ok(access::effective(false, visibility, None));
    };
    let granted = client
        .query_opt(
            "SELECT level FROM project_grants WHERE project_id = $1 AND user_id = $2",
            &[&id, &user.id],
        )
        .await?
        .map(|row| row.get::<_, String>(0));
    Ok(access::effective(
        user.id == owner,
        visibility,
        granted.as_deref().and_then(Level::grantable),
    ))
}

/// Требует уровня не ниже названного.
///
/// Здесь `403`, а не `404`: до этой точки доходит тот, кому проект **видно**, и прятать
/// его уже незачем - а "не найдено" вместо "нет права" отправило бы человека искать
/// опечатку в ссылке.
pub(crate) fn require_level(level: Level, needed: Level) -> Result<(), ApiError> {
    if level >= needed {
        Ok(())
    } else {
        Err(ApiError::Forbidden)
    }
}

/// Требует, чтобы в проекте был файл названного имени И названного вида.
///
/// Вид проверяется, а не только наличие: активным файлом назначают модель, а активным
/// сценарием - сценарий. Перепутанные роли дали бы не отказ, а пустую страницу либо
/// прогон по пояснению.
///
/// # Ошибки
/// Файла нет либо он другого вида - `400` с названным именем.
async fn has_kind(
    transaction: &tokio_postgres::Transaction<'_>,
    id: &str,
    name: &str,
    kind: Kind,
    what: &str,
) -> Result<(), ApiError> {
    let found: i64 = transaction
        .query_one(
            "SELECT count(*) FROM project_files
             WHERE project_id = $1 AND name = $2 AND kind = $3",
            &[&id, &name, &kind.as_str()],
        )
        .await?
        .get(0);
    if found == 0 {
        return Err(ApiError::BadRequest(format!(
            "{what} '{name}': в проекте нет файла вида '{}' с таким именем",
            kind.as_str()
        )));
    }
    Ok(())
}

/// Проверяет цель и ключи сборки - разбором самого компилятора.
///
/// негодные ключи отвергаются при записи, а не при сборке. Своего списка ключей у
/// сервера при этом не появляется: спрашивается модуль проекта (`takt_flags`), то есть
/// тот же `parse_compile_args`, что у `taktc`. Заведи разбор здесь - он разошёлся бы с
/// компилятором молча.
///
/// Модуля нет - **отказ, а не молчаливый приём**: непроверенное значение доехало бы до
/// страницы читателя и отказало бы там, где он ничего не выбирал.
pub(crate) fn check_build(
    state: &Arc<AppState>,
    version: &str,
    target: &str,
    args: &str,
) -> Result<(), ApiError> {
    let modules = state.modules.as_ref().ok_or_else(|| {
        ApiError::BadRequest("ключи сборки проверить нечем: модуля нет в статике".to_string())
    })?;
    modules
        .check_flags(version, target, args)
        .map_err(|error| ApiError::BadRequest(format!("ключи сборки: {error}")))
}

pub(crate) const SELECT_PROJECT: &str = "SELECT p.id, p.name, p.description, p.visibility,
        u.login AS owner, p.takt_lang, p.language_version, p.main_file,
        p.main_scenario, p.build_target, p.build_args, p.revision,
        p.size_bytes, p.forked_from, p.created_at, p.updated_at, p.owner_id,
        p.touched_at, p.archived_at
    FROM projects p JOIN users u ON u.id = p.owner_id WHERE p.id = $1";

pub(crate) fn project_of(row: &tokio_postgres::Row) -> ProjectJson {
    ProjectJson {
        id: row.get("id"),
        name: row.get("name"),
        description: row.get("description"),
        visibility: row.get("visibility"),
        owner: row.get("owner"),
        takt_lang: row.get("takt_lang"),
        language_version: row.get("language_version"),
        main_file: row.get("main_file"),
        main_scenario: row.get("main_scenario"),
        build_target: row.get("build_target"),
        build_args: row.get("build_args"),
        revision: row.get("revision"),
        size_bytes: row.get("size_bytes"),
        forked_from: row.get("forked_from"),
        created_at: row.get("created_at"),
        updated_at: row.get("updated_at"),
    }
}

/// Идентификатор проекта: 16 случайных байт, base64url.
///
/// Не последовательный номер: при видимости `link` идентификатор И есть секрет, а
/// номера перечисляются перебором.
pub fn new_id() -> String {
    use base64::Engine as _;
    use rand::RngCore as _;
    let mut bytes = [0u8; 16];
    rand::thread_rng().fill_bytes(&mut bytes);
    base64::engine::general_purpose::URL_SAFE_NO_PAD.encode(bytes)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn identifier_is_random_and_url_safe() {
        let first = new_id();
        assert_ne!(first, new_id(), "идентификаторы не повторяются");
        assert_eq!(first.len(), 22, "16 байт в base64url без набивки");
        assert!(
            first
                .chars()
                .all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_'),
            "идентификатор попадает в адрес: {first}"
        );
    }
}
