//! Маршруты сервера: вход, здоровье, статика.
//!
//! # Что здесь есть
//!
//! Остов: учётные записи, обмен токенов, `/health` и раздача собранной статики. Ручки
//! проектов заводит   - здесь их нет намеренно, и список маршрутов
//! проверяется тестом: маршрут, заведённый мимо этого списка, иначе появился бы у
//! сервиса, не появившись в его описании.
//!
//! # Заголовки кеша
//!
//! Правило то же, что у выкладки: **форма адреса задаёт срок**. Помеченное отпечатком
//! (`/b/<хеш>/...`, `/wasm/<версия>/...`) - год и `immutable`, остальное - `no-cache`.
//! Правило продублировано здесь нарочно: без nginx
//! заголовки обязаны быть теми же, иначе то, что проверено локально, ведёт себя иначе
//! на стенде.

use axum::extract::{ConnectInfo, Request, State};
use axum::http::{HeaderMap, HeaderValue, StatusCode, header};
use axum::middleware::Next;
use axum::response::{IntoResponse, Response};
use axum::routing::{get, post};
use axum::{Json, Router};
use serde::{Deserialize, Serialize};
use std::net::{IpAddr, SocketAddr};
use std::sync::Arc;
use tower_http::services::ServeDir;

use crate::auth::{self, Role, User};
use crate::config::Config;
use crate::error::ApiError;
use crate::rate::{Decision, Window};

/// Общее состояние сервера.
pub struct AppState {
    pub config: Config,
    /// Пул соединений к PostgreSQL: у неё запись параллельна, и одно соединение под
    /// мьютексом создавало бы очередь, которой в базе нет.
    pub pool: deadpool_postgres::Pool,
    pub rate: Window,
    /// Версия модуля, которую сервер считает последней: её получает новый
    /// проект, и по ней страница берёт `wasm/<версия>/`.
    pub module_version: String,
    /// Версия языка Takt на момент создания проекта - для человека, а не для выбора
    /// модуля.
    pub language_version: String,
    /// Модули `takt-wasm` из статики: ими собирается вывод целей в архиве. `None` -
    /// статики нет, и выгрузка с генерацией отказывает словами вместо того, чтобы
    /// отдать архив без обещанного вывода.
    pub modules: Option<Arc<crate::module::Modules>>,
    /// Хранилище исходников: файловая система. Тексты живут
    /// **здесь**, а база ведёт состав и сведения о проекте.
    pub store: Arc<crate::store::Store>,
    /// Клиент исходящих запросов к площадкам входа.
    ///
    /// Один на сервер: у него пул соединений и таймаут, а клиент на запрос открывал бы
    /// новое TLS-соединение к площадке каждому входящему.
    pub http: reqwest::Client,
}

/// Список маршрутов сервиса.
///
/// Объявлен явно и проверяется тестом: маршрут, заведённый мимо этого списка, появился
/// бы у сервиса, не появившись в его описании, - и о нём не узнал бы никто, кроме
/// автора.
pub const ROUTES: &[(&str, &str)] = &[
    ("GET", "/health"),
    ("POST", "/api/register"),
    ("POST", "/api/token"),
    ("POST", "/api/revoke"),
    ("GET", "/api/me"),
    ("GET", "/api/projects"),
    ("POST", "/api/projects"),
    ("GET", "/api/projects/{id}"),
    ("PATCH", "/api/projects/{id}"),
    ("DELETE", "/api/projects/{id}"),
    ("GET", "/api/projects/{id}/files/{name}"),
    ("PUT", "/api/projects/{id}/files/{name}"),
    ("DELETE", "/api/projects/{id}/files/{name}"),
    ("POST", "/api/projects/{id}/files/{name}/rename"),
    ("GET", "/api/public"),
    ("POST", "/api/projects/{id}/fork"),
    ("GET", "/api/projects/{id}/grants/{login}"),
    ("PUT", "/api/projects/{id}/grants/{login}"),
    ("DELETE", "/api/projects/{id}/grants/{login}"),
    ("GET", "/api/projects/{id}/archive"),
    ("POST", "/api/projects/import"),
    ("GET", "/api/oauth/providers"),
    ("GET", "/api/oauth/identities"),
    ("POST", "/api/oauth/complete"),
    ("GET", "/api/oauth/{provider}/start"),
    ("GET", "/api/oauth/{provider}/callback"),
    ("DELETE", "/api/me/identities/{provider}"),
    ("PUT", "/api/me/password"),
];

/// Собирает роутер.
pub fn router(state: Arc<AppState>) -> Router {
    let api = Router::new()
        .route("/register", post(register))
        .route("/token", post(token))
        .route("/revoke", post(revoke))
        .route("/me", get(me))
        .merge(crate::projects::router())
        .merge(crate::grants::router())
        .merge(crate::archive_api::router())
        .merge(crate::oauth::api::router())
        .merge(crate::showcase::router());

    // Статика: файлы отдаются как есть, а страницы собирает `page`.
    //
    // Каталог сам `index.html` не подставляет
    // (`append_index_html_on_directories(false)`): корень - тоже страница, и ему, как и
    // `/p/<id>`, надо переписать корень адресов.
    let statics = ServeDir::new(&state.config.static_dir)
        .append_index_html_on_directories(false)
        .fallback(page_service(state.clone()));

    let app = Router::new()
        .route("/health", get(health))
        .nest("/api", api)
        .with_state(state.clone())
        .fallback_service(statics)
        .layer(axum::middleware::from_fn(cache_headers))
        .layer(axum::extract::DefaultBodyLimit::max(
            state.config.body_limit,
        ));

    // Префикс за обратным прокси. `/` - не префикс, и вкладывать в него ничего не надо:
    // `nest("/")` у axum запрещён.
    //
    // `nest_service`, а не `nest`: `nest` не сопоставляет адрес с завершающей косой -
    // запрос `/takt/` до вложенного роутера не доходит вовсе и получает 404 снаружи. А
    // ведь именно на `/takt/` ведёт наш же редирект в nginx, и именно этот адрес
    // открывает читатель.
    if state.config.base_path == "/" {
        app
    } else {
        Router::new().nest_service(&state.config.base_path, app)
    }
}

/// Проверка живости: база обязана отвечать.
///
/// `SELECT 1` через соединение, а не просто `200`: сервис, у которого база недоступна,
/// живым не является, и балансировщик обязан узнать об этом сам.
async fn health(State(state): State<Arc<AppState>>) -> Response {
    let ok = match state.pool.get().await {
        Ok(client) => client.query_one("SELECT 1", &[]).await.is_ok(),
        Err(_) => false,
    };
    if ok {
        (StatusCode::OK, Json(serde_json::json!({"status": "ok"}))).into_response()
    } else {
        (
            StatusCode::SERVICE_UNAVAILABLE,
            Json(serde_json::json!({"status": "db"})),
        )
            .into_response()
    }
}

/// Запрос регистрации.
#[derive(Deserialize)]
struct RegisterRequest {
    login: String,
    password: String,
}

/// Запрос выдачи токена - форма OAuth 2.0.
#[derive(Deserialize)]
struct TokenRequest {
    grant_type: String,
    #[serde(default)]
    login: String,
    #[serde(default)]
    password: String,
    #[serde(default)]
    refresh_token: String,
}

/// Запрос гашения токена.
#[derive(Deserialize)]
struct RevokeRequest {
    refresh_token: String,
}

/// Выданная пара.
#[derive(Serialize)]
struct TokenResponse {
    access_token: String,
    refresh_token: String,
    token_type: &'static str,
    expires_in: u64,
}

/// Сведения о себе.
#[derive(Serialize)]
struct MeResponse {
    id: String,
    login: String,
    role: Role,
    /// Есть ли у записи пароль.
    ///
    /// Нужно странице: без этого она предлагала бы "задать пароль" тому, у кого он
    /// есть, и узнавал бы человек об этом отказом. Само значение секретом не является -
    /// оно и так видно попыткой входа.
    has_password: bool,
}

async fn register(
    State(state): State<Arc<AppState>>,
    ConnectInfo(peer): ConnectInfo<SocketAddr>,
    Json(request): Json<RegisterRequest>,
) -> Result<Response, ApiError> {
    limit(&state, peer.ip())?;
    let client = state.pool.get().await?;
    let user = auth::register(&client, &request.login, &request.password, Role::User).await?;
    let pair = auth::start_session(
        &client,
        &state.config.jwt_secret,
        &user,
        state.config.access_ttl.as_secs() as i64,
        state.config.refresh_ttl.as_secs() as i64,
    )
    .await?;
    Ok((StatusCode::CREATED, Json(pair_body(&state, pair))).into_response())
}

async fn token(
    State(state): State<Arc<AppState>>,
    ConnectInfo(peer): ConnectInfo<SocketAddr>,
    Json(request): Json<TokenRequest>,
) -> Result<Response, ApiError> {
    let access_ttl = state.config.access_ttl.as_secs() as i64;
    let refresh_ttl = state.config.refresh_ttl.as_secs() as i64;
    let pair = match request.grant_type.as_str() {
        "password" => {
            // Частота ограничивается только у входа паролем: обмен refresh- токена
            // делает открытая вкладка сама, и окно било бы по своим.
            limit(&state, peer.ip())?;
            let client = state.pool.get().await?;
            let user = auth::authenticate(&client, &request.login, &request.password).await?;
            auth::start_session(
                &client,
                &state.config.jwt_secret,
                &user,
                access_ttl,
                refresh_ttl,
            )
            .await?
        }
        "refresh_token" => {
            let client = state.pool.get().await?;
            auth::refresh(
                &client,
                &state.config.jwt_secret,
                &request.refresh_token,
                access_ttl,
                refresh_ttl,
            )
            .await?
        }
        // Неизвестный `grant_type` отвечает тем же отказом, что неверный пароль:
        // перечислять поддерживаемые виды выдачи незачем.
        _ => return Err(ApiError::InvalidCredentials),
    };
    Ok(Json(pair_body(&state, pair)).into_response())
}

async fn revoke(
    State(state): State<Arc<AppState>>,
    Json(request): Json<RevokeRequest>,
) -> Result<Response, ApiError> {
    let client = state.pool.get().await?;
    auth::revoke(&client, &request.refresh_token).await?;
    Ok(StatusCode::NO_CONTENT.into_response())
}

async fn me(State(state): State<Arc<AppState>>, headers: HeaderMap) -> Result<Response, ApiError> {
    let user = current_user(&state, &headers).await?;
    let client = state.pool.get().await?;
    let has_password: bool = client
        .query_one(
            "SELECT pass_hash IS NOT NULL FROM users WHERE id = $1",
            &[&user.id],
        )
        .await?
        .get(0);
    Ok(Json(MeResponse {
        id: user.id,
        login: user.login,
        role: user.role,
        has_password,
    })
    .into_response())
}

/// Опознаёт того, кто пришёл.
///
/// Роль читается **из базы**, а не из токена: иначе снятие права администратора
/// действовало бы только после истечения часа.
pub async fn current_user(state: &AppState, headers: &HeaderMap) -> Result<User, ApiError> {
    optional_user(state, headers)
        .await?
        .ok_or(ApiError::Unauthorized)
}

/// Опознаёт того, кто пришёл, если он вообще назвался.
///
/// `None` - пришли **без** токена: открытый проект открыт и для того, у кого учётной
/// записи нет вовсе.
///
/// Испорченный токен - это `401`, а не "аноним": молчаливое понижение до анонима
/// показало бы владельцу чужую картину его же проекта и выглядело бы потерей прав, а не
/// просроченным входом.
pub async fn optional_user(
    state: &AppState,
    headers: &HeaderMap,
) -> Result<Option<User>, ApiError> {
    let Some(header) = headers
        .get(header::AUTHORIZATION)
        .and_then(|value| value.to_str().ok())
        .and_then(|value| value.strip_prefix("Bearer "))
    else {
        return Ok(None);
    };
    let claims =
        auth::read_access(&state.config.jwt_secret, header).ok_or(ApiError::Unauthorized)?;
    let client = state.pool.get().await?;
    let user = auth::load(&client, &claims.sub)
        .await?
        .ok_or(ApiError::Unauthorized)?;
    Ok(Some(user))
}

fn pair_body(state: &AppState, pair: auth::Pair) -> TokenResponse {
    TokenResponse {
        access_token: pair.access,
        refresh_token: pair.refresh,
        token_type: "Bearer",
        expires_in: state.config.access_ttl.as_secs(),
    }
}

fn limit(state: &AppState, address: IpAddr) -> Result<(), ApiError> {
    match state.rate.check(address) {
        Decision::Allow => Ok(()),
        Decision::Wait(after_secs) => Err(ApiError::TooManyRequests { after_secs }),
    }
}

/// Отдаёт страницу приложения на адрес, который ею и является.
///
/// Неизвестный путь `index.html` **не** получает. Пока получал, промах адреса выглядел
/// успехом: ссылка на бандл со страницы `/p/<id>` уходила в `/p/b/<отпечаток>/app.css`,
/// сервер отвечал `200` разметкой, и вкладка открывалась без стилей и без модуля, не
/// сказав ни слова.
fn page_service(
    state: Arc<AppState>,
) -> tower::util::BoxCloneSyncService<Request, Response, std::convert::Infallible> {
    tower::util::BoxCloneSyncService::new(tower::service_fn(move |request: Request| {
        let state = state.clone();
        async move {
            let path = request.uri().path().to_string();
            Ok(if is_page(&path) {
                page(&state).await
            } else {
                StatusCode::NOT_FOUND.into_response()
            })
        }
    }))
}

/// Собирает `index.html`, переписав корень адресов под префикс.
///
/// Читается на каждый запрос, а не при старте: страницу пересобирают чаще, чем
/// перезапускают сервер, и закешированная разметка отдавала бы вчерашний бандл,
/// которого на диске уже нет.
async fn page(state: &AppState) -> Response {
    let file = state.config.static_dir.join("index.html");
    let Ok(text) = tokio::fs::read_to_string(&file).await else {
        return (
            StatusCode::NOT_FOUND,
            Json(serde_json::json!({"error": "no_static"})),
        )
            .into_response();
    };
    let body = text.replace(BASE_TAG, &base_tag(&state.config.base_path));
    ([(header::CONTENT_TYPE, "text/html; charset=utf-8")], body).into_response()
}

/// Как корень адресов записан в исходной разметке.
const BASE_TAG: &str = "<base href=\"/\">";

/// Тот же тег под префиксом обратного прокси.
///
/// Косая черта в конце обязательна: без неё `takt` в `<base href="/takt">` браузер
/// считает именем файла и отбрасывает - адреса снова уезжают в корень.
fn base_tag(base_path: &str) -> String {
    if base_path == "/" {
        BASE_TAG.to_string()
    } else {
        format!("<base href=\"{base_path}/\">")
    }
}

/// Является ли адрес страницей приложения.
///
/// Страниц две формы: корень и живая страница проекта `/p/<id>`. Всё остальное - файл,
/// и промах по нему обязан быть промахом.
pub fn is_page(path: &str) -> bool {
    let path = path.trim_end_matches('/');
    if path.is_empty() {
        return true;
    }
    let Some(rest) = path.strip_prefix("/p/") else {
        return false;
    };
    !rest.is_empty()
        && !rest.contains('/')
        && rest
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_')
}

/// Заголовки кеша по форме адреса.
async fn cache_headers(request: Request, next: Next) -> Response {
    let path = request.uri().path().to_string();
    let mut response = next.run(request).await;
    let value = if is_fingerprinted(&path) {
        "public, max-age=31536000, immutable"
    } else {
        "no-cache"
    };
    response
        .headers_mut()
        .insert(header::CACHE_CONTROL, HeaderValue::from_static(value));
    response
}

/// Помечен ли адрес отпечатком: `/b/<хеш>/...` либо `/wasm/<версия>/...`.
///
/// Отдельной функцией, потому что проверяется тестом: ошибись правило в одну сторону -
/// читатель год видит вчерашнюю страницу, в другую - стенд отдаёт трёхмегабайтный
/// модуль каждому заходу.
pub fn is_fingerprinted(path: &str) -> bool {
    let mut parts = path.trim_start_matches('/').split('/');
    match (parts.next(), parts.next(), parts.next()) {
        (Some("b"), Some(mark), Some(_)) => {
            mark.len() >= 6 && mark.chars().all(|c| c.is_ascii_hexdigit())
        }
        (Some("wasm"), Some(version), Some(_)) => {
            version.contains('.') && version.chars().all(|c| c.is_ascii_digit() || c == '.')
        }
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn only_fingerprinted_addresses_live_forever() {
        assert!(is_fingerprinted("/b/06baef9f2dab/app.js"));
        assert!(is_fingerprinted("/b/06baef9f2dab/font/x.woff2"));
        assert!(is_fingerprinted("/wasm/0.58.0/takt.wasm"));
        // Описи читают ради свежести - вечными им быть нельзя.
        assert!(!is_fingerprinted("/version.json"));
        assert!(!is_fingerprinted("/wasm/index.json"));
        assert!(!is_fingerprinted("/index.html"));
        assert!(!is_fingerprinted("/"));
        assert!(!is_fingerprinted("/b/短/app.js"), "не отпечаток");
        assert!(!is_fingerprinted("/b/06baef9f2dab"), "каталог без файла");
    }

    #[test]
    fn a_page_is_a_page_and_a_file_is_a_file() {
        // Отдавай сервер `index.html` на любой неизвестный путь, промах по файлу
        // выглядел бы успехом. Со страницы
        // `/p/<id>` ссылка на бандл уходила в `/p/b/<отпечаток>/app.css`, сервер
        // отвечал `200` разметкой, и вкладка открывалась без стилей и без модуля, не
        // сказав ни слова.
        assert!(is_page("/"), "корень — страница");
        assert!(is_page("/p/_px-i2FkJsWvMobGAbI14g"));
        assert!(is_page("/p/AbCd/"), "косая черта в конце ничего не меняет");
        assert!(
            !is_page("/p/b/f8377401e12e/app.css"),
            "это файл, а не страница"
        );
        assert!(!is_page("/b/f8377401e12e/app.js"));
        assert!(!is_page("/wasm/0.58.0/takt.wasm"));
        assert!(!is_page("/p/"), "адрес без идентификатора");
        assert!(!is_page("/такого-нет"));
    }

    #[test]
    fn the_base_of_addresses_follows_the_prefix() {
        assert_eq!(base_tag("/"), BASE_TAG, "без прокси разметка не меняется");
        // Косая черта в конце обязательна: без неё браузер считает `takt` именем файла
        // и отбрасывает - адреса снова уезжают в корень.
        assert_eq!(base_tag("/takt"), "<base href=\"/takt/\">");
    }

    #[test]
    fn routes_are_listed_and_unique() {
        let mut seen = std::collections::BTreeSet::new();
        for route in ROUTES {
            assert!(seen.insert(route), "маршрут {route:?} перечислен дважды");
            assert!(route.1.starts_with('/'), "путь без косой: {route:?}");
        }
        assert_eq!(
            seen.len(),
            28,
            "вход, проекты, витрина, копия, права, архив, площадки и запись"
        );
    }
}
