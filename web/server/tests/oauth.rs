//! Проверки входа через площадки.
//!
//! # Граница проводится по сокету
//!
//! Всё, что до него, - наш код, и его проверяет машина; сама площадка - человек по
//! чек-листу с датой. Здесь поднимается **поддельный провайдер**: маленький роутер
//! `axum` на `127.0.0.1:0`, адрес которого сервер получает переменными
//! `TAKT_WEB_OAUTH_*_BASE`.
//!
//! Подделка **строгая**: она проверяет ровно то, что проверила бы площадка - равенство
//! `redirect_uri`, `S256(code_verifier) == code_challenge`, одноразовость кода, вид
//! секрета. Иначе зелёный набор доказывал бы лишь, что подделка соглашается на всё. На
//! это стоит отдельный тест: подделку спрашивают **прямыми** запросами, минуя сервер.

mod common;

use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use axum::extract::{Query, State};
use axum::http::{HeaderMap, StatusCode, header};
use axum::response::{IntoResponse, Response};
use axum::routing::{get, post};
use axum::{Json, Router};
use common::{Stand, skipped};

/// Что подделка ждёт и что помнит.
#[derive(Default)]
struct Fake {
    /// Выданные коды: код -> (challenge, redirect_uri). Одноразовые.
    codes: Mutex<HashMap<String, (String, String)>>,
    /// Сколько ждать перед ответом обмена - для случая "площадка молчит".
    sleep_ms: Mutex<u64>,
}

/// Поднимает поддельного провайдера; возвращает его корень.
async fn fake_provider(state: Arc<Fake>) -> String {
    let app = Router::new()
        .route("/authorize", get(fake_authorize))
        .route("/token", post(fake_token))
        .route("/oauth2/auth", post(fake_token))
        .route("/info", post(fake_userinfo))
        .route("/oauth2/user_info", post(fake_userinfo))
        .with_state(state);
    let listener = tokio::net::TcpListener::bind("127.0.0.1:0")
        .await
        .expect("сокет");
    let address = listener.local_addr().expect("адрес");
    tokio::spawn(async move {
        let _ = axum::serve(listener, app).await;
    });
    format!("http://{address}")
}

/// `authorize`: площадка запоминает вызов и возвращает код.
async fn fake_authorize(
    State(fake): State<Arc<Fake>>,
    Query(params): Query<HashMap<String, String>>,
) -> Response {
    let Some(redirect) = params.get("redirect_uri") else {
        return (StatusCode::BAD_REQUEST, "invalid_request").into_response();
    };
    let challenge = params.get("code_challenge").cloned().unwrap_or_default();
    let code = format!("code-{}", fake.codes.lock().expect("замок").len());
    fake.codes
        .lock()
        .expect("замок")
        .insert(code.clone(), (challenge, redirect.clone()));
    let state_value = params.get("state").cloned().unwrap_or_default();
    // Площадка возвращает `state` и `device_id` без изменений.
    let mut back = format!("{redirect}?code={code}&state={state_value}");
    if params.contains_key("code_challenge") {
        back.push_str("&device_id=dev-1");
    }
    if let Some(via) = params.get("provider") {
        back.push_str(&format!("&via={via}"));
    }
    axum::response::Redirect::temporary(&back).into_response()
}

/// Обмен кода - строгий: подделка проверяет то же, что и площадка.
async fn fake_token(State(fake): State<Arc<Fake>>, headers: HeaderMap, body: String) -> Response {
    let wait = *fake.sleep_ms.lock().expect("замок");
    if wait > 0 {
        tokio::time::sleep(std::time::Duration::from_millis(wait)).await;
    }
    let form = parse_form(&body);
    let Some(code) = form.get("code") else {
        return Json(serde_json::json!({"error": "invalid_request"})).into_response();
    };
    // Код одноразовый: повторный обмен обязан отказать.
    let Some((challenge, redirect)) = fake.codes.lock().expect("замок").remove(code.as_str())
    else {
        return Json(serde_json::json!({"error": "invalid_grant"})).into_response();
    };
    if form.get("redirect_uri") != Some(&redirect) {
        return Json(serde_json::json!({"error": "invalid_request"})).into_response();
    }
    if !challenge.is_empty() {
        let verifier = form.get("code_verifier").cloned().unwrap_or_default();
        if takt_web_server::oauth::code_challenge(&verifier) != challenge {
            return Json(serde_json::json!({"error": "invalid_grant"})).into_response();
        }
    }
    // Секрет: либо `Basic` (Яндекс), либо сервисный ключ телом (VK ID).
    let has_basic = headers
        .get(header::AUTHORIZATION)
        .and_then(|value| value.to_str().ok())
        .is_some_and(|value| value.starts_with("Basic "));
    if !has_basic && !form.contains_key("service_token") {
        return Json(serde_json::json!({"error": "invalid_client"})).into_response();
    }
    Json(serde_json::json!({"access_token": "at-1", "expires_in": 3600})).into_response()
}

/// Сведения о человеке: тело - как у настоящей площадки, с лишним.
async fn fake_userinfo(headers: HeaderMap) -> Response {
    if headers.get(header::AUTHORIZATION).is_none() {
        return (StatusCode::UNAUTHORIZED, "no token").into_response();
    }
    // Нарочно с почтой, именем и телефоном: разбор обязан оставить только идентификатор -
    // обещание "персональных данных не храним" держится этим.
    Json(serde_json::json!({
        "id": "yandex-42",
        "login": "ivan",
        "default_email": "ivan@ya.ru",
        "real_name": "Иван Иванов",
        "user": {"user_id": "vk-42", "email": "ivan@vk.com", "first_name": "Иван"}
    }))
    .into_response()
}

fn parse_form(body: &str) -> HashMap<String, String> {
    body.split('&')
        .filter_map(|pair| pair.split_once('='))
        .map(|(key, value)| (unescape(key), unescape(value)))
        .collect()
}

fn unescape(value: &str) -> String {
    let bytes = value.as_bytes();
    let mut out = Vec::with_capacity(bytes.len());
    let mut index = 0;
    while index < bytes.len() {
        if bytes[index] == b'%'
            && index + 2 < bytes.len()
            && let Ok(byte) = u8::from_str_radix(&value[index + 1..index + 3], 16)
        {
            out.push(byte);
            index += 3;
            continue;
        }
        out.push(bytes[index]);
        index += 1;
    }
    String::from_utf8_lossy(&out).to_string()
}

/// Поднимает стенд с настроенной поддельной площадкой.
async fn stand_with_fake(tag: &str) -> Option<(Stand, Arc<Fake>)> {
    let fake = Arc::new(Fake::default());
    let base = fake_provider(fake.clone()).await;
    let stand = Stand::open_with(tag, |config| {
        config.public_url = "http://127.0.0.1:8730".to_string();
        config.oauth.yandex_client_id = "yid".to_string();
        config.oauth.yandex_client_secret = "ysecret".to_string();
        config.oauth.vk_client_id = "vid".to_string();
        config.oauth.vk_service_token = "vtoken".to_string();
        config.oauth.vk_mail = true;
        config.oauth.yandex_base = base.clone();
        config.oauth.yandex_info = base.clone();
        config.oauth.vk_base = base.clone();
    })
    .await?;
    Some((stand, fake))
}

/// Проходит поток до ticket; возвращает его.
async fn walk_to_ticket(
    stand: &Stand,
    provider: &str,
    token: Option<&str>,
) -> (StatusCode, String) {
    let (status, location, cookie) = stand
        .follow(&format!("/api/oauth/{provider}/start"), token)
        .await;
    assert_eq!(status, StatusCode::TEMPORARY_REDIRECT, "start: {location}");
    // Идём на подделку своим клиентом: браузера здесь нет.
    let client = reqwest::Client::builder()
        .redirect(reqwest::redirect::Policy::none())
        .build()
        .expect("клиент");
    let back = client.get(&location).send().await.expect("authorize");
    let callback = back
        .headers()
        .get(header::LOCATION)
        .expect("возврат")
        .to_str()
        .expect("строка")
        .to_string();
    // Возврат приходит абсолютным адресом нашего сервиса - берём путь с запросом.
    let path = callback
        .split_once("/api/")
        .map(|(_, rest)| format!("/api/{rest}"))
        .unwrap_or(callback);
    stand.callback(&path, &cookie).await
}

#[tokio::test]
async fn a_new_person_signs_in_and_picks_a_login() {
    let Some((stand, _fake)) = stand_with_fake("o_new").await else {
        return skipped("вход через площадку");
    };
    let (status, fragment) = walk_to_ticket(&stand, "yandex", None).await;
    assert_eq!(status, StatusCode::TEMPORARY_REDIRECT, "{fragment}");
    let ticket = fragment
        .split_once("#login=")
        .map(|(_, rest)| rest.to_string())
        .unwrap_or_else(|| panic!("во фрагменте нет ticket: {fragment}"));

    // Без логина - отказ: имя площадки мы не читаем и не храним, и подставить его
    // неоткуда.
    let (status, body) = stand
        .post("/api/oauth/complete", serde_json::json!({"ticket": ticket}))
        .await;
    assert_eq!(status, StatusCode::CONFLICT, "{body}");
    assert_eq!(body["message"], "login_required");

    // Ticket одноразовый - второй раз тем же не выйдет.
    let (status, _) = stand
        .post(
            "/api/oauth/complete",
            serde_json::json!({"ticket": ticket, "login": "ivan"}),
        )
        .await;
    assert_eq!(status, StatusCode::BAD_REQUEST, "ticket пережил обмен");

    // Полный круг заново, с логином сразу.
    let (_, fragment) = walk_to_ticket(&stand, "yandex", None).await;
    let ticket = fragment
        .split_once("#login=")
        .expect("ticket")
        .1
        .to_string();
    let (status, pair) = stand
        .post(
            "/api/oauth/complete",
            serde_json::json!({"ticket": ticket, "login": "ivan"}),
        )
        .await;
    assert_eq!(status, StatusCode::OK, "{pair}");
    assert!(pair["access_token"].is_string());
    assert_eq!(pair["login"], "ivan");

    // Запись заведена без пароля, и связь с площадкой есть.
    let (password, identities) = stand.identity_facts("ivan").await;
    assert!(!password, "у записи площадки пароля быть не должно");
    assert_eq!(identities, 1);

    // Войти паролем такой записи нельзя, и ответ - как у неверного пароля.
    let (status, _) = stand
        .post(
            "/api/token",
            serde_json::json!({"grant_type": "password", "login": "ivan", "password": "любой"}),
        )
        .await;
    assert_eq!(status, StatusCode::BAD_REQUEST);

    stand.drop_schema().await;
}

#[tokio::test]
async fn a_returning_person_gets_the_same_account() {
    let Some((stand, _fake)) = stand_with_fake("o_again").await else {
        return skipped("повторный вход");
    };
    let (_, fragment) = walk_to_ticket(&stand, "yandex", None).await;
    let ticket = fragment
        .split_once("#login=")
        .expect("ticket")
        .1
        .to_string();
    let (_, first) = stand
        .post(
            "/api/oauth/complete",
            serde_json::json!({"ticket": ticket, "login": "ivan"}),
        )
        .await;
    let token = first["access_token"].as_str().expect("токен").to_string();
    let (_, me) = stand.get_as("/api/me", &token).await;
    let id = me["id"].as_str().expect("идентификатор").to_string();

    // Второй круг: логин не спрашивается, запись та же.
    let (_, fragment) = walk_to_ticket(&stand, "yandex", None).await;
    let ticket = fragment
        .split_once("#login=")
        .expect("ticket")
        .1
        .to_string();
    let (status, second) = stand
        .post("/api/oauth/complete", serde_json::json!({"ticket": ticket}))
        .await;
    assert_eq!(status, StatusCode::OK, "{second}");
    let token = second["access_token"].as_str().expect("токен").to_string();
    let (_, me) = stand.get_as("/api/me", &token).await;
    assert_eq!(me["id"], id, "второй вход завёл вторую запись");
    // Семейство refresh - новое: вход есть вход.
    assert_ne!(first["refresh_token"], second["refresh_token"]);

    stand.drop_schema().await;
}

#[tokio::test]
async fn a_platform_is_linked_to_an_existing_account() {
    let Some((stand, _fake)) = stand_with_fake("o_link").await else {
        return skipped("привязка площадки");
    };
    let (status, body) = stand
        .post(
            "/api/register",
            serde_json::json!({"login": "ivan", "password": "пароль-пароль"}),
        )
        .await;
    assert_eq!(status, StatusCode::CREATED, "{body}");
    let token = body["access_token"].as_str().expect("токен").to_string();

    // Соединяет записи только владелец из своей сессии: поток с токеном заводится с
    // целью "привязать", и пары токенов он не выдаёт.
    let (status, fragment) = walk_to_ticket(&stand, "yandex", Some(&token)).await;
    assert_eq!(status, StatusCode::TEMPORARY_REDIRECT);
    assert!(fragment.contains("linked=1"), "{fragment}");

    let (_, identities) = stand.get_as("/api/oauth/identities", &token).await;
    assert_eq!(identities.as_array().expect("список").len(), 1);
    assert_eq!(identities[0]["provider"], "yandex");

    // Занятую идентичность второму человеку не отдают.
    let (_, other) = stand
        .post(
            "/api/register",
            serde_json::json!({"login": "vera", "password": "пароль-пароль"}),
        )
        .await;
    let others = other["access_token"].as_str().expect("токен").to_string();
    let (_, fragment) = walk_to_ticket(&stand, "yandex", Some(&others)).await;
    assert!(fragment.contains("identity_taken"), "{fragment}");
    let (_, identities) = stand.get_as("/api/oauth/identities", &others).await;
    assert_eq!(
        identities.as_array().expect("список").len(),
        0,
        "чужая связь"
    );

    stand.drop_schema().await;
}

#[tokio::test]
async fn a_broken_flow_is_refused_and_burned() {
    let Some((stand, fake)) = stand_with_fake("o_broken").await else {
        return skipped("испорченный поток");
    };

    // Чужой `state` - поток не найден.
    let (status, fragment) = stand
        .callback("/api/oauth/yandex/callback?code=c&state=чужой", "")
        .await;
    assert_eq!(status, StatusCode::TEMPORARY_REDIRECT);
    assert!(fragment.contains("login_error=expired"), "{fragment}");

    // Cookie нет - поток начал не этот браузер.
    let (status, location, _cookie) = stand.follow("/api/oauth/yandex/start", None).await;
    assert_eq!(status, StatusCode::TEMPORARY_REDIRECT);
    let client = reqwest::Client::builder()
        .redirect(reqwest::redirect::Policy::none())
        .build()
        .expect("клиент");
    let back = client.get(&location).send().await.expect("authorize");
    let callback = back
        .headers()
        .get(header::LOCATION)
        .expect("возврат")
        .to_str()
        .expect("строка")
        .to_string();
    let path = callback
        .split_once("/api/")
        .map(|(_, rest)| format!("/api/{rest}"))
        .expect("путь");
    let (_, fragment) = stand.callback(&path, "").await;
    assert!(fragment.contains("login_error=csrf"), "{fragment}");
    // Поток погашен первым же обращением: тот же адрес второй раз - уже "просрочен", а
    // не "csrf".
    let (_, fragment) = stand.callback(&path, "takt_oauth=что-угодно").await;
    assert!(fragment.contains("login_error=expired"), "{fragment}");

    // Площадка молчит дольше таймаута.
    *fake.sleep_ms.lock().expect("замок") = 1500;
    let (_, fragment) = walk_to_ticket(&stand, "yandex", None).await;
    assert!(fragment.contains("login_error=unavailable"), "{fragment}");
    *fake.sleep_ms.lock().expect("замок") = 0;

    stand.drop_schema().await;
}

#[tokio::test]
async fn what_is_not_configured_does_not_exist() {
    let Some(stand) = Stand::open("o_off").await else {
        return skipped("ненастроенная площадка");
    };
    // Площадки не настроены вовсе: список пуст, `start` отвечает `404`.
    let (status, list) = stand.get("/api/oauth/providers").await;
    assert_eq!(status, StatusCode::OK, "{list}");
    assert_eq!(list.as_array().expect("список").len(), 0);
    let (status, _) = stand.get("/api/oauth/yandex/start").await;
    assert_eq!(status, StatusCode::NOT_FOUND);
    stand.drop_schema().await;
}

#[tokio::test]
async fn the_configured_platforms_are_listed_in_order() {
    let Some((stand, _fake)) = stand_with_fake("o_list").await else {
        return skipped("список площадок");
    };
    let (status, list) = stand.get("/api/oauth/providers").await;
    assert_eq!(status, StatusCode::OK, "{list}");
    let ids: Vec<String> = list
        .as_array()
        .expect("список")
        .iter()
        .map(|item| item["label"].as_str().expect("подпись").to_string())
        .collect();
    // Порядок задаёт сервер и он фиксирован: Яндекс, ВКонтакте, Mail.
    assert_eq!(ids, vec!["oauth.yandex", "oauth.vk", "oauth.mail"]);
    // Подпись - Ключ словаря, а не текст: текст переводится страницей.
    assert!(ids.iter().all(|key| key.starts_with("oauth.")));

    stand.drop_schema().await;
}

#[tokio::test]
async fn the_return_path_must_be_ours() {
    let Some((stand, _fake)) = stand_with_fake("o_return").await else {
        return skipped("адрес возврата");
    };
    for bad in ["https://зло.example/", "//зло.example/", "/путь?x=1"] {
        let path = format!(
            "/api/oauth/yandex/start?return_to={}",
            bad.replace('/', "%2F").replace(':', "%3A")
        );
        let (status, body) = stand.get(&path).await;
        assert_eq!(status, StatusCode::BAD_REQUEST, "{bad}: {body}");
    }
    // Свой путь принимается и доезжает обратно в ответе `complete`.
    let (status, location, cookie) = stand
        .follow("/api/oauth/yandex/start?return_to=%2Fp%2Fabc", None)
        .await;
    assert_eq!(status, StatusCode::TEMPORARY_REDIRECT, "{location}");
    let client = reqwest::Client::builder()
        .redirect(reqwest::redirect::Policy::none())
        .build()
        .expect("клиент");
    let back = client.get(&location).send().await.expect("authorize");
    let callback = back
        .headers()
        .get(header::LOCATION)
        .expect("возврат")
        .to_str()
        .expect("строка")
        .to_string();
    let path = callback
        .split_once("/api/")
        .map(|(_, rest)| format!("/api/{rest}"))
        .expect("путь");
    let (_, fragment) = stand.callback(&path, &cookie).await;
    assert!(fragment.starts_with("/p/abc#login="), "{fragment}");

    stand.drop_schema().await;
}

#[tokio::test]
async fn the_schema_keeps_no_personal_data_from_the_platform() {
    let Some(stand) = Stand::open("o_schema").await else {
        return skipped("схема входа через площадки");
    };
    // Расширение теста 09a: площадки отдают почту, имя и фотографию, и единственная
    // защита обещания - тому негде лечь.
    let columns = stand.columns("external_identities").await;
    for forbidden in ["email", "name", "avatar", "token", "access_token", "phone"] {
        assert!(
            !columns.iter().any(|column| column.contains(forbidden)),
            "в схеме есть колонка '{forbidden}': {columns:?}"
        );
    }
    assert_eq!(
        columns,
        vec!["created_at", "provider", "subject", "user_id"],
        "состав таблицы связей"
    );
    // Токенов площадки нет и в потоке: там живёт только верификатор PKCE.
    let columns = stand.columns("oauth_flows").await;
    assert!(!columns.iter().any(|column| column.contains("access")));
    stand.drop_schema().await;
}

#[tokio::test]
async fn the_fake_provider_catches_what_a_platform_would() {
    // Тест подделки. Без него зелёный набор доказывал бы лишь, что она соглашается на
    // всё: подделка спрашивается прямыми запросами, минуя сервер (тестовых ручек в
    // сервере не заводится).
    let fake = Arc::new(Fake::default());
    let base = fake_provider(fake.clone()).await;
    let client = reqwest::Client::builder()
        .redirect(reqwest::redirect::Policy::none())
        .build()
        .expect("клиент");

    let verifier = takt_web_server::oauth::random_token(32);
    let challenge = takt_web_server::oauth::code_challenge(&verifier);
    let issue = |challenge: String| {
        let client = client.clone();
        let base = base.clone();
        async move {
            let url = format!(
                "{base}/authorize?redirect_uri=http%3A%2F%2Flocal%2Fcb&state=st&code_challenge={challenge}"
            );
            let response = client.get(url).send().await.expect("authorize");
            let location = response
                .headers()
                .get(header::LOCATION)
                .expect("возврат")
                .to_str()
                .expect("строка")
                .to_string();
            location
                .split_once("code=")
                .and_then(|(_, rest)| rest.split('&').next())
                .expect("код")
                .to_string()
        }
    };

    let exchange = |code: String, verifier: String| {
        let client = client.clone();
        let base = base.clone();
        async move {
            let body = format!(
                "grant_type=authorization_code&code={code}\
                 &redirect_uri=http%3A%2F%2Flocal%2Fcb&code_verifier={verifier}&service_token=t"
            );
            client
                .post(format!("{base}/token"))
                .header(header::CONTENT_TYPE, "application/x-www-form-urlencoded")
                .body(body)
                .send()
                .await
                .expect("обмен")
                .text()
                .await
                .expect("тело")
        }
    };

    // Чужой верификатор - отказ.
    let code = issue(challenge.clone()).await;
    let answer = exchange(code, "чужой".to_string()).await;
    assert!(answer.contains("invalid_grant"), "{answer}");

    // Повторный обмен того же кода - отказ.
    let code = issue(challenge.clone()).await;
    let first = exchange(code.clone(), verifier.clone()).await;
    assert!(first.contains("access_token"), "{first}");
    let second = exchange(code, verifier.clone()).await;
    assert!(second.contains("invalid_grant"), "{second}");

    // Другой адрес возврата - отказ.
    let code = issue(challenge.clone()).await;
    let body = format!(
        "grant_type=authorization_code&code={code}&redirect_uri=http%3A%2F%2Flocal%2Fдругое\
         &code_verifier={verifier}&service_token=t"
    );
    let answer = client
        .post(format!("{base}/token"))
        .header(header::CONTENT_TYPE, "application/x-www-form-urlencoded")
        .body(body)
        .send()
        .await
        .expect("обмен")
        .text()
        .await
        .expect("тело");
    assert!(answer.contains("invalid_request"), "{answer}");

    // Без секрета - отказ.
    let code = issue(challenge).await;
    let body = format!(
        "grant_type=authorization_code&code={code}&redirect_uri=http%3A%2F%2Flocal%2Fcb\
         &code_verifier={verifier}"
    );
    let answer = client
        .post(format!("{base}/token"))
        .header(header::CONTENT_TYPE, "application/x-www-form-urlencoded")
        .body(body)
        .send()
        .await
        .expect("обмен")
        .text()
        .await
        .expect("тело");
    assert!(answer.contains("invalid_client"), "{answer}");
}

#[tokio::test]
async fn the_matrix_of_linking_and_unlinking_holds() {
    let Some((stand, _fake)) = stand_with_fake("o_matrix").await else {
        return skipped("матрица привязки");
    };
    // Человек с паролем: у него уже есть один способ войти.
    let (_, body) = stand
        .post(
            "/api/register",
            serde_json::json!({"login": "ivan", "password": "пароль-пароль"}),
        )
        .await;
    let token = body["access_token"].as_str().expect("токен").to_string();

    let mut wrong = Vec::new();

    // Привязка свободной идентичности.
    let (_, fragment) = walk_to_ticket(&stand, "yandex", Some(&token)).await;
    if !fragment.contains("linked=1") {
        wrong.push(format!("привязка свободной: {fragment}"));
    }
    // Повторная привязка той же площадки тем же человеком - идемпотентна: повторное
    // нажатие кнопки не должно быть ошибкой.
    let (_, fragment) = walk_to_ticket(&stand, "yandex", Some(&token)).await;
    if !fragment.contains("linked=1") {
        wrong.push(format!("повторная привязка своей: {fragment}"));
    }
    let (_, list) = stand.get_as("/api/oauth/identities", &token).await;
    if list.as_array().expect("список").len() != 1 {
        wrong.push(format!(
            "связей стало {}",
            list.as_array().expect("список").len()
        ));
    }

    // Отвязка при живом пароле - можно.
    let (status, _) = stand.delete_as("/api/me/identities/yandex", &token).await;
    if status != StatusCode::NO_CONTENT {
        wrong.push(format!("отвязка при пароле: {status}"));
    }
    // Отвязка того, чего нет, - тоже успех: предмет просьбы выполнен.
    let (status, _) = stand.delete_as("/api/me/identities/yandex", &token).await;
    if status != StatusCode::NO_CONTENT {
        wrong.push(format!("отвязка отсутствующей: {status}"));
    }

    // Человек только с площадкой: пароля нет.
    let (_, fragment) = walk_to_ticket(&stand, "vk", None).await;
    let ticket = fragment
        .split_once("#login=")
        .expect("ticket")
        .1
        .to_string();
    let (_, pair) = stand
        .post(
            "/api/oauth/complete",
            serde_json::json!({"ticket": ticket, "login": "vera"}),
        )
        .await;
    let hers = pair["access_token"].as_str().expect("токен").to_string();

    // Отвязка последнего способа войти отказывает: запись стала бы недостижимой, а
    // восстановления по почте у нас нет вовсе.
    let (status, body) = stand.delete_as("/api/me/identities/vk", &hers).await;
    if status != StatusCode::CONFLICT || body["message"] != "last_login_method" {
        wrong.push(format!("отвязка последнего: {status} {body}"));
    }

    // Задать пароль - и тогда отвязка разрешена.
    let (status, _) = stand
        .put_as(
            "/api/me/password",
            &hers,
            serde_json::json!({"password": "новый-пароль"}),
        )
        .await;
    if status != StatusCode::NO_CONTENT {
        wrong.push(format!("задание пароля: {status}"));
    }
    // Пароль гасит живые сеансы (тот же носитель, что у сброса администратором),
    // поэтому входим заново - уже паролем.
    let (status, pair) = stand
        .post(
            "/api/token",
            serde_json::json!({"grant_type": "password", "login": "vera", "password": "новый-пароль"}),
        )
        .await;
    if status != StatusCode::OK {
        wrong.push(format!("вход паролем после его задания: {status} {pair}"));
    }
    let hers = pair["access_token"]
        .as_str()
        .unwrap_or_default()
        .to_string();
    let (status, _) = stand.delete_as("/api/me/identities/vk", &hers).await;
    if status != StatusCode::NO_CONTENT {
        wrong.push(format!("отвязка после пароля: {status}"));
    }

    // Задать пароль там, где он уже есть, - отказ: смена требует старого.
    let (status, body) = stand
        .put_as(
            "/api/me/password",
            &hers,
            serde_json::json!({"password": "ещё-один-пароль"}),
        )
        .await;
    if status != StatusCode::CONFLICT || body["message"] != "password_already_set" {
        wrong.push(format!("повторное задание пароля: {status} {body}"));
    }

    assert!(
        wrong.is_empty(),
        "матрица разошлась:\n  {}",
        wrong.join("\n  ")
    );
    stand.drop_schema().await;
}
