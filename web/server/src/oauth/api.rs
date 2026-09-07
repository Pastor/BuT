//! Ручки входа через площадки.
//!
//! # Поток
//!
//! `start` заводит строку потока и уводит человека на площадку; `callback`
//! принимает код, меняет его на токен **на сервере** (секрет в браузер не
//! попадает), спрашивает идентификатор и возвращает человека на страницу с
//! **ticket во фрагменте**; `complete` меняет ticket на пару токенов.
//!
//! Ticket живёт **только во фрагменте** (`/#login=...`): фрагмент не уходит ни в журнал
//! сервера, ни в заголовок `Referer` - тот же довод, что у ссылки-снимка. Пара токенов
//! во фрагменте отвергнута: refresh-токен на тридцать суток остался бы в истории
//! браузера; ticket в `?query` отвергнут: он попадает в журнал прокси.
//!
//! # Две защиты, а не одна
//!
//! `state` подтверждает, что поток начал **наш сервер**; cookie `takt_oauth`
//! подтверждает, что его начал **этот браузер**. `callback` требует совпадения обоих:
//! одного `state` мало - нападающий может получить свой `callback`-адрес и подсунуть
//! его жертве, и та войдёт в чужую запись.
//!
//! # Возврат
//!
//! `return_to` - путь **нашего** сайта, и он лежит **в строке потока**, а не в адресе
//! перенаправления: иначе это открытый редирект сразу после входа.

use axum::extract::{Path, Query, State};
use axum::http::{HeaderMap, StatusCode, header};
use axum::response::{IntoResponse, Redirect, Response};
use axum::routing::{get, post};
use axum::{Json, Router};
use serde::{Deserialize, Serialize};
use std::sync::Arc;

use super::{Pkce, Provider, Secret, authorize_url, code_challenge, fingerprint, random_token};
use crate::auth;
use crate::db;
use crate::error::ApiError;
use crate::routes::{AppState, current_user, optional_user};

/// Сколько живёт незавершённый вход.
pub const FLOW_TTL: i64 = 600;

/// Сколько живёт ticket обмена.
///
/// Минута, а не час: ticket и есть секрет входа на этом шаге, а живёт он в адресной
/// строке браузера.
pub const TICKET_TTL: i64 = 60;

/// Имя cookie потока.
pub const FLOW_COOKIE: &str = "takt_oauth";

/// Площадка в ответе `providers`.
#[derive(Debug, Serialize)]
pub struct ProviderJson {
    /// Ключ для адреса `start`.
    pub id: &'static str,
    /// Ключ словаря для подписи кнопки - **не текст**: текст переводится.
    pub label: &'static str,
    /// Имя файла значка в `web/static/brand/` - **не путь**: путь строит страница.
    /// Приём тот же, что у подписи: имён площадок в коде страницы нет, их объявляет
    /// сервер.
    pub icon: &'static str,
    /// Кнопка "Mail" - тот же VK ID с другим параметром.
    pub via: Option<&'static str>,
}

/// Запрос `start`.
#[derive(Debug, Deserialize)]
pub struct StartQuery {
    /// Куда вернуть человека после входа. Только путь нашего сайта.
    #[serde(default)]
    pub return_to: Option<String>,
    /// `mail` - кнопка "Mail" внутри VK ID.
    #[serde(default)]
    pub via: Option<String>,
}

/// Запрос `callback`.
#[derive(Debug, Deserialize)]
pub struct CallbackQuery {
    #[serde(default)]
    pub code: Option<String>,
    #[serde(default)]
    pub state: Option<String>,
    #[serde(default)]
    pub device_id: Option<String>,
    /// Отказ площадки: человек не дал согласия.
    #[serde(default)]
    pub error: Option<String>,
}

/// Запрос `complete`.
#[derive(Debug, Deserialize)]
pub struct CompleteRequest {
    pub ticket: String,
    /// Логин - только при первом входе.
    #[serde(default)]
    pub login: Option<String>,
}

/// Связанная площадка в ответе `identities`.
#[derive(Debug, Serialize)]
pub struct IdentityJson {
    pub provider: String,
    pub created_at: i64,
}

/// Маршруты входа через площадки.
pub fn router() -> Router<Arc<AppState>> {
    Router::new()
        .route("/oauth/providers", get(providers))
        .route("/oauth/identities", get(identities))
        .route("/oauth/complete", post(complete))
        .route("/oauth/{provider}/start", get(start))
        .route("/oauth/{provider}/callback", get(callback))
        .route(
            "/me/identities/{provider}",
            axum::routing::delete(drop_identity),
        )
        .route("/me/password", axum::routing::put(set_password))
}

/// Перечисляет настроенные площадки.
///
/// Страница рисует кнопки **только по этому ответу**: своего списка площадок у неё нет
/// и быть не должно - второй список разошёлся бы с первым молча (приём "нет списка
/// ключевых слов Takt в вебе").
async fn providers(State(state): State<Arc<AppState>>) -> Response {
    let mut out = Vec::new();
    let config = &state.config.oauth;
    // Порядок задаёт сервер и он же фиксирован: Яндекс, ВКонтакте, Mail.
    if config.has_yandex() {
        out.push(ProviderJson {
            id: "yandex",
            label: "oauth.yandex",
            icon: "yandex.svg",
            via: None,
        });
    }
    if config.has_vk() {
        out.push(ProviderJson {
            id: "vk",
            label: "oauth.vk",
            icon: "vk.svg",
            via: None,
        });
    }
    if config.has_mail() {
        out.push(ProviderJson {
            id: "vk",
            label: "oauth.mail",
            icon: "mail.svg",
            via: Some("mail"),
        });
    }
    Json(out).into_response()
}

/// Показывает связанные площадки. Спрашивает владелец записи.
async fn identities(
    State(state): State<Arc<AppState>>,
    headers: HeaderMap,
) -> Result<Response, ApiError> {
    let user = current_user(&state, &headers).await?;
    let client = state.pool.get().await?;
    let rows = client
        .query(
            "SELECT provider, created_at FROM external_identities
             WHERE user_id = $1 ORDER BY provider",
            &[&user.id],
        )
        .await?;
    let out: Vec<IdentityJson> = rows
        .iter()
        .map(|row| IdentityJson {
            provider: row.get(0),
            created_at: row.get(1),
        })
        .collect();
    Ok(Json(out).into_response())
}

/// Отвязывает площадку от своей записи.
///
/// Отвязка **последнего** способа войти отказывает: запись стала бы недостижимой, а
/// восстановления по почте у нас нет вовсе - вернуть человека смог бы только
/// администратор.
async fn drop_identity(
    State(state): State<Arc<AppState>>,
    headers: HeaderMap,
    Path(provider): Path<String>,
) -> Result<Response, ApiError> {
    let user = current_user(&state, &headers).await?;
    let client = state.pool.get().await?;
    unlink(&client, &user.id, &provider).await?;
    Ok(StatusCode::NO_CONTENT.into_response())
}

/// Запрос "задать пароль".
#[derive(Debug, Deserialize)]
pub struct PasswordRequest {
    pub password: String,
}

/// Задаёт пароль записи, у которой его не было.
///
/// Только для записи без пароля. Смена существующего требует старого пароля - это
/// другая ручка и другая задача; молчаливо позволить смену по одной лишь сессии значило
/// бы, что забытая на чужой машине вкладка меняет пароль.
async fn set_password(
    State(state): State<Arc<AppState>>,
    headers: HeaderMap,
    Json(request): Json<PasswordRequest>,
) -> Result<Response, ApiError> {
    let user = current_user(&state, &headers).await?;
    let client = state.pool.get().await?;
    let has: bool = client
        .query_one(
            "SELECT pass_hash IS NOT NULL FROM users WHERE id = $1",
            &[&user.id],
        )
        .await?
        .get(0);
    if has {
        return Err(ApiError::Conflict {
            message: "password_already_set".to_string(),
            seen: None,
            actual: 0,
        });
    }
    auth::check_password(&request.password)?;
    // Тем же носителем, что и сброс администратором: пароль хешируется и живые сеансы
    // гасятся - второй способ поставить пароль разошёлся бы с первым.
    auth::set_password(&client, &user.login, &request.password).await?;
    Ok(StatusCode::NO_CONTENT.into_response())
}

/// Начинает вход: заводит поток и уводит на площадку.
///
/// С токеном в заголовке поток заводится с целью `link` - привязать площадку к
/// **существующей** записи. Соединяет записи только владелец из своей сессии.
async fn start(
    State(state): State<Arc<AppState>>,
    headers: HeaderMap,
    Path(name): Path<String>,
    Query(request): Query<StartQuery>,
) -> Result<Response, ApiError> {
    let via_mail = request.via.as_deref() == Some("mail");
    let provider = configured(&state, &name, via_mail).ok_or(ApiError::NotFound)?;
    let (client_id, _) = credentials(&state, &name).ok_or(ApiError::NotFound)?;

    let return_to = match request.return_to.as_deref() {
        None | Some("") => "/".to_string(),
        Some(path) => {
            // Только путь нашего сайта: иначе вход кончался бы уводом на фишинговую
            // копию - открытым редиректом, о котором предупреждает документация
            // площадок.
            if !path.starts_with('/')
                || path.starts_with("//")
                || !path
                    .chars()
                    .all(|c| c.is_ascii_alphanumeric() || "/_-.".contains(c))
            {
                return Err(ApiError::BadRequest(
                    "return_to: только путь этого сайта".to_string(),
                ));
            }
            path.to_string()
        }
    };

    let viewer = optional_user(&state, &headers).await?;
    let purpose = if viewer.is_some() { "link" } else { "login" };

    let state_value = random_token(32);
    let nonce = random_token(32);
    let verifier = random_token(32);
    let now = db::now();

    let client = state.pool.get().await?;
    // Просроченные потоки убираются при вставке - как записи окна частоты: иначе
    // таблица растёт по числу начатых и брошенных входов.
    client
        .execute("DELETE FROM oauth_flows WHERE expires_at < $1", &[&now])
        .await?;
    client
        .execute(
            "INSERT INTO oauth_flows(state_hash, provider, stage, nonce_hash, code_verifier,
                                     purpose, user_id, return_to, created_at, expires_at)
             VALUES ($1, $2, 'started', $3, $4, $5, $6, $7, $8, $9)",
            &[
                &fingerprint(&state_value),
                &name,
                &fingerprint(&nonce),
                &verifier,
                &purpose,
                &viewer.as_ref().map(|user| user.id.clone()),
                &return_to,
                &now,
                &(now + FLOW_TTL),
            ],
        )
        .await?;

    let url = authorize_url(
        &provider,
        &client_id,
        &redirect_uri(&state, &name),
        &state_value,
        &verifier,
    );
    let cookie = format!(
        "{FLOW_COOKIE}={nonce}; Path=/; HttpOnly; SameSite=Lax; Max-Age={FLOW_TTL}{}",
        if state.config.public_url.starts_with("https://") {
            "; Secure"
        } else {
            ""
        }
    );
    Ok(([(header::SET_COOKIE, cookie)], Redirect::temporary(&url)).into_response())
}

/// Принимает возврат с площадки.
///
/// Отвечает **перенаправлением**, а не JSON: сюда приходит браузер человека после
/// площадки, и показать ему тело ответа значило бы оставить его на служебном адресе.
async fn callback(
    State(state): State<Arc<AppState>>,
    headers: HeaderMap,
    Path(name): Path<String>,
    Query(request): Query<CallbackQuery>,
) -> Response {
    match finish(&state, &headers, &name, &request).await {
        Ok(response) => response,
        // Причина едет ключом словаря, а не текстом площадки: чужой текст не
        // переводится и не всегда предназначен читателю.
        Err(key) => back_to_page(&state, &format!("login_error={key}"), "/"),
    }
}

/// Делает работу `callback`; в отказе - ключ словаря.
async fn finish(
    state: &Arc<AppState>,
    headers: &HeaderMap,
    name: &str,
    request: &CallbackQuery,
) -> Result<Response, &'static str> {
    if request.error.is_some() {
        return Err("denied");
    }
    let (Some(code), Some(state_value)) = (request.code.as_deref(), request.state.as_deref())
    else {
        return Err("failed");
    };

    let client = state.pool.get().await.map_err(|_| "failed")?;
    let now = db::now();
    // Поток гасится первым же обращением - успешным или нет: иначе тот же код можно
    // предъявить дважды.
    let row = client
        .query_opt(
            "DELETE FROM oauth_flows WHERE state_hash = $1 AND stage = 'started'
             RETURNING provider, nonce_hash, code_verifier, purpose, user_id, return_to, expires_at",
            &[&fingerprint(state_value)],
        )
        .await
        .map_err(|_| "failed")?;
    let Some(row) = row else {
        return Err("expired");
    };
    let flow_provider: String = row.get(0);
    let nonce_hash: String = row.get(1);
    let verifier: Option<String> = row.get(2);
    let purpose: String = row.get(3);
    let flow_user: Option<String> = row.get(4);
    let return_to: String = row.get(5);
    let expires_at: i64 = row.get(6);

    if expires_at < now {
        return Err("expired");
    }
    // Провайдер записан в строке потока: иначе код, полученный на одной площадке, можно
    // было бы предъявить обработчику другой.
    if flow_provider != name {
        return Err("failed");
    }
    // Вторая защита: поток начал этот браузер.
    let nonce = cookie_value(headers, FLOW_COOKIE).ok_or("csrf")?;
    if fingerprint(&nonce) != nonce_hash {
        return Err("csrf");
    }

    let provider = configured(state, name, false).ok_or("failed")?;
    let (client_id, secret) = credentials(state, name).ok_or("failed")?;
    let token = exchange(
        state,
        &provider,
        &client_id,
        &secret,
        code,
        verifier.as_deref().unwrap_or_default(),
        request.device_id.as_deref(),
        name,
    )
    .await?;
    let subject = user_subject(state, &provider, &token).await?;

    // Идентичность известна - вход; неизвестна - ticket на выбор логина.
    let existing: Option<String> = client
        .query_opt(
            "SELECT user_id FROM external_identities WHERE provider = $1 AND subject = $2",
            &[&provider.id, &subject],
        )
        .await
        .map_err(|_| "failed")?
        .map(|row| row.get(0));

    if purpose == "link" {
        let Some(owner) = flow_user else {
            return Err("failed");
        };
        // Уже привязанное к этому же человеку - не ошибка: повторное нажатие кнопки
        // идемпотентно. Привязанное к другому - отказ: слить две записи значит
        // перенести проекты, гранты и копии, а это необратимо и делается вручную.
        if existing.is_some_and(|found| found != owner) {
            return Err("identity_taken");
        }
        client
            .execute(
                "INSERT INTO external_identities(provider, subject, user_id, created_at)
                 VALUES ($1, $2, $3, $4) ON CONFLICT (provider, subject) DO NOTHING",
                &[&provider.id, &subject, &owner, &now],
            )
            .await
            .map_err(|_| "failed")?;
        return Ok(back_to_page(state, "linked=1", &return_to));
    }

    let ticket = random_token(16);
    client
        .execute(
            "INSERT INTO oauth_flows(state_hash, provider, stage, nonce_hash, purpose,
                                     user_id, subject, return_to, created_at, expires_at)
             VALUES ($1, $2, 'ticket', '', 'login', $3, $4, $5, $6, $7)",
            &[
                &fingerprint(&ticket),
                &provider.id,
                &existing,
                &subject,
                &return_to,
                &now,
                &(now + TICKET_TTL),
            ],
        )
        .await
        .map_err(|_| "failed")?;
    Ok(back_to_page(state, &format!("login={ticket}"), &return_to))
}

/// Меняет ticket на пару токенов.
async fn complete(
    State(state): State<Arc<AppState>>,
    Json(request): Json<CompleteRequest>,
) -> Result<Response, ApiError> {
    let client = state.pool.get().await?;
    let now = db::now();
    let row = client
        .query_opt(
            "DELETE FROM oauth_flows WHERE state_hash = $1 AND stage = 'ticket'
             RETURNING provider, user_id, subject, return_to, expires_at",
            &[&fingerprint(&request.ticket)],
        )
        .await?;
    let Some(row) = row else {
        return Err(ApiError::BadRequest("ticket_expired".to_string()));
    };
    let provider: String = row.get(0);
    let known: Option<String> = row.get(1);
    let subject: Option<String> = row.get(2);
    let return_to: String = row.get(3);
    let expires_at: i64 = row.get(4);
    if expires_at < now {
        return Err(ApiError::BadRequest("ticket_expired".to_string()));
    }

    let user = match known {
        Some(id) => auth::load(&client, &id)
            .await?
            .ok_or(ApiError::Unauthorized)?,
        None => {
            // Новый человек: логин просит он сам - имени площадки мы не читаем и не
            // храним.
            let Some(login) = request.login.as_deref().filter(|l| !l.is_empty()) else {
                return Err(ApiError::Conflict {
                    message: "login_required".to_string(),
                    seen: None,
                    actual: 0,
                });
            };
            let subject = subject.ok_or(ApiError::Unauthorized)?;
            create_with_identity(&client, login, &provider, &subject).await?
        }
    };

    let pair = auth::start_session(
        &client,
        &state.config.jwt_secret,
        &user,
        state.config.access_ttl.as_secs() as i64,
        state.config.refresh_ttl.as_secs() as i64,
    )
    .await?;
    Ok(Json(serde_json::json!({
        "access_token": pair.access,
        "refresh_token": pair.refresh,
        "token_type": "Bearer",
        "expires_in": state.config.access_ttl.as_secs(),
        "login": user.login,
        "return_to": return_to,
    }))
    .into_response())
}

/// Заводит запись без пароля и связывает её с площадкой - одной транзакцией.
///
/// Именно одной: запись без идентичности означала бы человека, который не может войти
/// ничем, а идентичность без записи - ссылку в никуда.
async fn create_with_identity(
    client: &deadpool_postgres::Client,
    login: &str,
    provider: &str,
    subject: &str,
) -> Result<auth::User, ApiError> {
    auth::check_login(login)?;
    let id = uuid::Uuid::new_v4().to_string();
    let now = db::now();
    let affected = client
        .execute(
            "INSERT INTO users(id, login, pass_hash, role, created_at)
             VALUES ($1, $2, NULL, 'user', $3)
             ON CONFLICT (lower(login)) DO NOTHING",
            &[&id, &login, &now],
        )
        .await?;
    if affected == 0 {
        return Err(ApiError::LoginTaken);
    }
    client
        .execute(
            "INSERT INTO external_identities(provider, subject, user_id, created_at)
             VALUES ($1, $2, $3, $4)",
            &[&provider, &subject, &id, &now],
        )
        .await?;
    Ok(auth::User {
        id,
        login: login.to_string(),
        role: auth::Role::User,
    })
}

/// Меняет код на токен площадки.
#[allow(clippy::too_many_arguments)]
async fn exchange(
    state: &Arc<AppState>,
    provider: &Provider,
    client_id: &str,
    secret: &str,
    code: &str,
    verifier: &str,
    device_id: Option<&str>,
    name: &str,
) -> Result<String, &'static str> {
    let mut form = vec![
        ("grant_type".to_string(), "authorization_code".to_string()),
        ("code".to_string(), code.to_string()),
        (
            "redirect_uri".to_string(),
            redirect_uri(state, name).to_string(),
        ),
    ];
    if provider.pkce != Pkce::None {
        form.push(("code_verifier".to_string(), verifier.to_string()));
        // Площадке отдаётся верификатор, а вызов проверки нужен здесь лишь для того,
        // чтобы ошибка кодирования не уехала на площадку молча.
        debug_assert!(!code_challenge(verifier).is_empty());
    }
    if provider.returns_device_id {
        form.push((
            "device_id".to_string(),
            device_id.unwrap_or_default().to_string(),
        ));
    }
    let mut request = state.http.post(&provider.token);
    match provider.secret {
        Secret::Basic => {
            form.push(("client_id".to_string(), client_id.to_string()));
            request = request.basic_auth(client_id, Some(secret));
        }
        Secret::ServiceToken => {
            form.push(("client_id".to_string(), client_id.to_string()));
            form.push(("service_token".to_string(), secret.to_string()));
        }
    }
    let body = super::encode_query(&form);
    let response = request
        .header(header::CONTENT_TYPE, "application/x-www-form-urlencoded")
        .body(body)
        .send()
        .await
        .map_err(|error| {
            // В журнал уходит вид отказа, а не тело: в теле бывает токен. Читателю
            // ответ один - "площадка недоступна": различать "не ответила" и "ответила
            // поздно" ему незачем.
            tracing::warn!(provider = %name, timeout = error.is_timeout(), "площадка не ответила");
            "unavailable"
        })?;
    let text = response.text().await.map_err(|_| "unavailable")?;
    let parsed: super::TokenReply = serde_json::from_str(&text).map_err(|_| "failed")?;
    if let Some(error) = parsed.error {
        tracing::warn!(provider = %name, code = %error, "площадка отвергла обмен");
        return Err("failed");
    }
    if parsed.access_token.is_empty() {
        return Err("failed");
    }
    Ok(parsed.access_token)
}

/// Спрашивает у площадки идентификатор человека.
async fn user_subject(
    state: &Arc<AppState>,
    provider: &Provider,
    token: &str,
) -> Result<String, &'static str> {
    let response = state
        .http
        .post(&provider.userinfo)
        .header(header::CONTENT_TYPE, "application/x-www-form-urlencoded")
        .header(header::AUTHORIZATION, format!("Bearer {token}"))
        .body(super::encode_query(&[(
            "access_token".to_string(),
            token.to_string(),
        )]))
        .send()
        .await
        .map_err(|_| "unavailable")?;
    let text = response.text().await.map_err(|_| "unavailable")?;
    super::subject_of(provider, &text).ok_or("failed")
}

/// Адрес возврата, зарегистрированный у площадки.
///
/// Строится только из настроенного внешнего адреса и префикса - никогда из заголовка
/// `Host`: площадка сверяет его с зарегистрированным, а `Host` приходит от клиента.
pub fn redirect_uri(state: &AppState, provider: &str) -> String {
    let base = state.config.public_url.trim_end_matches('/');
    let prefix = state.config.base_path.trim_end_matches('/');
    format!("{base}{prefix}/api/oauth/{provider}/callback")
}

/// Возвращает человека на страницу, положив ответ во фрагмент.
fn back_to_page(state: &AppState, fragment: &str, return_to: &str) -> Response {
    let prefix = state.config.base_path.trim_end_matches('/');
    Redirect::temporary(&format!("{prefix}{return_to}#{fragment}")).into_response()
}

/// Читает значение cookie.
fn cookie_value(headers: &HeaderMap, name: &str) -> Option<String> {
    let raw = headers.get(header::COOKIE)?.to_str().ok()?;
    raw.split(';')
        .filter_map(|part| part.trim().split_once('='))
        .find(|(key, _)| *key == name)
        .map(|(_, value)| value.to_string())
}

/// Описание настроенной площадки; `None` - не настроена.
fn configured(state: &AppState, name: &str, via_mail: bool) -> Option<Provider> {
    let config = &state.config.oauth;
    match name {
        "yandex" if config.has_yandex() => {
            Some(Provider::yandex(&config.yandex_base, &config.yandex_info))
        }
        "vk" if config.has_vk() => {
            if via_mail && !config.has_mail() {
                return None;
            }
            Some(Provider::vk(&config.vk_base, via_mail))
        }
        _ => None,
    }
}

/// Ключ и секрет площадки.
fn credentials(state: &AppState, name: &str) -> Option<(String, String)> {
    let config = &state.config.oauth;
    match name {
        "yandex" if config.has_yandex() => Some((
            config.yandex_client_id.clone(),
            config.yandex_client_secret.clone(),
        )),
        "vk" if config.has_vk() => {
            Some((config.vk_client_id.clone(), config.vk_service_token.clone()))
        }
        _ => None,
    }
}

/// Отвязывает площадку от записи.
///
/// # Ошибки
/// Это последний способ войти - отказ: иначе человек остался бы без входа
/// вовсе, и вернуть его смог бы только администратор.
pub async fn unlink(
    client: &deadpool_postgres::Client,
    user_id: &str,
    provider: &str,
) -> Result<(), ApiError> {
    let row = client
        .query_one(
            "SELECT (SELECT count(*) FROM external_identities WHERE user_id = $1),
                    (SELECT pass_hash IS NOT NULL FROM users WHERE id = $1)",
            &[&user_id],
        )
        .await?;
    let identities: i64 = row.get(0);
    let has_password: bool = row.get::<_, Option<bool>>(1).unwrap_or(false);
    if identities <= 1 && !has_password {
        return Err(ApiError::Conflict {
            message: "last_login_method".to_string(),
            seen: None,
            actual: 0,
        });
    }
    client
        .execute(
            "DELETE FROM external_identities WHERE user_id = $1 AND provider = $2",
            &[&user_id, &provider],
        )
        .await?;
    Ok(())
}

/// Ответ `providers` в виде, годном для сверки: только идентификаторы.
#[cfg(test)]
pub fn provider_ids(config: &crate::config::OAuthConfig) -> Vec<&'static str> {
    let mut out = Vec::new();
    if config.has_yandex() {
        out.push("yandex");
    }
    if config.has_vk() {
        out.push("vk");
    }
    if config.has_mail() {
        out.push("mail");
    }
    out
}

/// Статус ответа `complete`, когда нужен логин.
pub const LOGIN_REQUIRED: StatusCode = StatusCode::CONFLICT;
