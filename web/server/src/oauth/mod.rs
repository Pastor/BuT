//! Вход через площадки: описание площадок и протокол.
//!
//! # Одна таблица площадок
//!
//! Адреса, требования к PKCE, вид секрета и имя поля с идентификатором лежат здесь и
//! только здесь ([`Provider`]). Вторая копия - в странице, в скрипте, в тесте -
//! разошлась бы с первой молча: страница рисует кнопки по ответу сервера, а не по
//! своему списку.
//!
//! # Что хранится и чего не хранится
//!
//! Из ответа площадки разбирается одно поле - `subject`. Ни почты, ни имени, ни
//! фотографии: обещание не хранить персональные данные держится тем, что структура
//! ответа объявлена без них, и `serde` остальное отбрасывает. Токены площадки не
//! хранятся: они нужны один раз, внутри обработчика `callback`.
//!
//! Отсюда названная цена: при первом входе логин просит человек, а не подставляется из
//! имени. Подставить его значило бы прочитать и сохранить то, чего мы обещали не
//! хранить.
//!
//! # Свой код протокола, а не крейт
//!
//! PKCE и `state` - сорок строк, и они проверяются вектором RFC 7636. Крейт `oauth2`
//! умеет то же, но `device_id` и сервисный ключ VK ID пошли бы у него "дополнительными
//! параметрами", а таблица площадок всё равно нужна - вышло бы два описания одного
//! протокола.

pub mod api;

use base64::Engine as _;
use serde::Deserialize;
use sha2::{Digest, Sha256};

/// Как площадка принимает PKCE.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Pkce {
    /// Обязателен (VK ID).
    Required,
    /// Принимается, но не обязателен; шлём всегда - хуже не будет.
    Optional,
    /// Не принимается: лишний параметр - отказ обмена.
    None,
}

/// Чем подписывается обмен кода.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Secret {
    /// `client_id`/`client_secret` заголовком `Authorization: Basic` (Яндекс).
    Basic,
    /// Сервисный ключ доступа полем тела (VK ID).
    ServiceToken,
}

/// Описание площадки.
///
/// Единственное место, где живут её адреса и повадки. Понадобится вторая площадка -
/// здесь появится строка, а не ветвь в обработчике.
#[derive(Debug, Clone)]
pub struct Provider {
    /// Ключ в адресах и в базе.
    pub id: &'static str,
    /// Подпись кнопки - **ключ словаря**, а не текст: текст переводится.
    pub label_key: &'static str,
    pub authorize: String,
    pub token: String,
    pub userinfo: String,
    pub pkce: Pkce,
    /// Дополнительные параметры запроса `authorize`.
    pub extra_authorize: Vec<(String, String)>,
    /// Возвращает ли площадка `device_id` (VK ID) - он нужен обмену.
    pub returns_device_id: bool,
    pub secret: Secret,
    /// Имя поля с идентификатором человека в ответе `userinfo`.
    pub subject_field: &'static str,
}

/// Корень адресов Яндекса.
pub const YANDEX_BASE: &str = "https://oauth.yandex.ru";
/// Корень адресов Яндекса для сведений о человеке.
pub const YANDEX_INFO: &str = "https://login.yandex.ru";
/// Корень адресов VK ID.
pub const VK_BASE: &str = "https://id.vk.com";

impl Provider {
    /// Яндекс.
    ///
    /// Идентификатор - глобальный `id`, а **не** `psuid`: `psuid` привязан к
    /// приложению, и перерегистрация (переезд стенда, второе приложение) осиротила бы
    /// все связи разом.
    pub fn yandex(base: &str, info: &str) -> Self {
        Self {
            id: "yandex",
            label_key: "oauth.yandex",
            authorize: format!("{base}/authorize"),
            token: format!("{base}/token"),
            userinfo: format!("{info}/info"),
            pkce: Pkce::Optional,
            extra_authorize: Vec::new(),
            returns_device_id: false,
            secret: Secret::Basic,
            subject_field: "id",
        }
    }

    /// VK ID. `via_mail` - кнопка "Mail": тот же поток с `provider=mail_ru`.
    ///
    /// Провайдер идентичности при этом остаётся **`vk`**: идентичность - это кто выдал
    /// `subject`, а не через какую кнопку пришли.
    pub fn vk(base: &str, via_mail: bool) -> Self {
        let mut extra = Vec::new();
        if via_mail {
            extra.push(("provider".to_string(), "mail_ru".to_string()));
        }
        Self {
            id: "vk",
            label_key: if via_mail { "oauth.mail" } else { "oauth.vk" },
            authorize: format!("{base}/authorize"),
            token: format!("{base}/oauth2/auth"),
            userinfo: format!("{base}/oauth2/user_info"),
            pkce: Pkce::Required,
            extra_authorize: extra,
            returns_device_id: true,
            secret: Secret::ServiceToken,
            subject_field: "user_id",
        }
    }
}

/// Случайная строка алфавита `[A-Za-z0-9_-]` из `bytes` случайных байт.
///
/// Алфавит важен: `state` и верификатор едут в адресе, а обычный base64 содержит `+`,
/// `/` и `=`, которые там значат другое.
pub fn random_token(bytes: usize) -> String {
    use rand::RngCore as _;
    let mut raw = vec![0u8; bytes];
    rand::thread_rng().fill_bytes(&mut raw);
    base64::engine::general_purpose::URL_SAFE_NO_PAD.encode(raw)
}

/// Считает `code_challenge` метода `S256` от верификатора.
///
/// Проверяется вектором приложения B RFC 7636: подмени здесь кодировку - и площадка
/// отвергала бы обмен, а причина выглядела бы отказом площадки.
pub fn code_challenge(verifier: &str) -> String {
    let digest = Sha256::digest(verifier.as_bytes());
    base64::engine::general_purpose::URL_SAFE_NO_PAD.encode(digest)
}

/// Отпечаток строки: `state` и nonce лежат в базе только им (приём 09a).
pub fn fingerprint(value: &str) -> String {
    format!("{:x}", Sha256::digest(value.as_bytes()))
}

/// Строит адрес `authorize`.
///
/// `redirect_uri` приходит **готовым** из конфигурации и никогда от клиента: площадка
/// сверяет его с зарегистрированным, а параметр от клиента был бы открытым редиректом
/// кода на чужой хост.
pub fn authorize_url(
    provider: &Provider,
    client_id: &str,
    redirect_uri: &str,
    state: &str,
    verifier: &str,
) -> String {
    let mut query = vec![
        ("response_type".to_string(), "code".to_string()),
        ("client_id".to_string(), client_id.to_string()),
        ("redirect_uri".to_string(), redirect_uri.to_string()),
        ("state".to_string(), state.to_string()),
    ];
    if provider.pkce != Pkce::None {
        query.push(("code_challenge".to_string(), code_challenge(verifier)));
        query.push(("code_challenge_method".to_string(), "S256".to_string()));
    }
    query.extend(provider.extra_authorize.iter().cloned());
    format!("{}?{}", provider.authorize, encode_query(&query))
}

/// Кодирует пары в `application/x-www-form-urlencoded`.
pub fn encode_query(pairs: &[(String, String)]) -> String {
    pairs
        .iter()
        .map(|(key, value)| format!("{}={}", escape(key), escape(value)))
        .collect::<Vec<_>>()
        .join("&")
}

/// Процентное кодирование по RFC 3986 (незарезервированные - как есть).
fn escape(value: &str) -> String {
    let mut out = String::with_capacity(value.len());
    for byte in value.as_bytes() {
        match byte {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'_' | b'.' | b'~' => {
                out.push(*byte as char);
            }
            _ => out.push_str(&format!("%{byte:02X}")),
        }
    }
    out
}

/// Ответ площадки на обмен кода.
///
/// Объявлено только то, что нужно: `refresh_token` площадки отбрасывается (повторный
/// вход идёт полным кругом - площадка вправе отозвать доступ, и мы узнаём об этом),
/// `id_token` не разбирается.
#[derive(Debug, Deserialize)]
pub struct TokenReply {
    #[serde(default)]
    pub access_token: String,
    /// Код отказа площадки. В журнал уходит он, а не тело.
    #[serde(default)]
    pub error: Option<String>,
}

/// Разбирает ответ `userinfo`, оставляя **только** идентификатор.
///
/// Ни почты, ни имени, ни фотографии: обещание не хранить персональные данные держится
/// тем, что их некуда положить. Тело в журнал не пишется.
pub fn subject_of(provider: &Provider, body: &str) -> Option<String> {
    let value: serde_json::Value = serde_json::from_str(body).ok()?;
    // Сначала корень, потом объект `user`: Яндекс кладёт `id` в корень, VK ID -
    // `user_id` внутрь `user`. Обратный порядок ("если есть `user`, смотрим только
    // туда") ломается на теле, где есть и то и другое: идентификатор Яндекса перестал
    // бы находиться, а отказ выглядел бы отказом площадки.
    let found = value.get(provider.subject_field).or_else(|| {
        value
            .get("user")
            .and_then(|user| user.get(provider.subject_field))
    })?;
    match found {
        serde_json::Value::String(text) if !text.is_empty() => Some(text.clone()),
        serde_json::Value::Number(number) => Some(number.to_string()),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pkce_matches_the_vector_of_rfc_7636() {
        // Приложение B RFC 7636. Проверка обязательна: подмени здесь кодировку -
        // площадка отвергала бы обмен, и причина выглядела бы её отказом, а не нашей
        // ошибкой.
        assert_eq!(
            code_challenge("dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"),
            "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"
        );
    }

    #[test]
    fn a_random_token_fits_the_address() {
        let first = random_token(32);
        assert_eq!(first.len(), 43, "32 байта в base64url без набивки");
        assert!(
            first
                .chars()
                .all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_'),
            "в адрес попадает: {first}"
        );
        assert_ne!(first, random_token(32), "два подряд не равны");
        // Верификатор PKCE обязан уложиться в 43...128 символов (RFC 7636).
        assert!((43..=128).contains(&random_token(32).len()));
    }

    #[test]
    fn the_authorize_address_carries_what_the_platform_expects() {
        let provider = Provider::yandex(YANDEX_BASE, YANDEX_INFO);
        let url = authorize_url(
            &provider,
            "cid",
            "https://takt.example/takt/api/oauth/yandex/callback",
            "st",
            "ver",
        );
        assert!(
            url.starts_with("https://oauth.yandex.ru/authorize?"),
            "{url}"
        );
        assert!(url.contains("response_type=code"));
        assert!(url.contains("client_id=cid"));
        // Адрес возврата закодирован: незакодированный `:` и `/` площадка разберёт не
        // так, и сверка с зарегистрированным не сойдётся.
        assert!(
            url.contains(
                "redirect_uri=https%3A%2F%2Ftakt.example%2Ftakt%2Fapi%2Foauth%2Fyandex%2Fcallback"
            ),
            "{url}"
        );
        assert!(url.contains("code_challenge_method=S256"));

        // Кнопка "Mail" - тот же поток VK ID с названным провайдером.
        let mail = Provider::vk(VK_BASE, true);
        let url = authorize_url(&mail, "cid", "https://takt.example/cb", "st", "ver");
        assert!(url.contains("provider=mail_ru"), "{url}");
        assert_eq!(mail.id, "vk", "идентичность выдаёт VK ID, а не кнопка");
    }

    #[test]
    fn only_the_subject_survives_the_answer() {
        // Тела - из документации площадок: у Яндекса почта и имя в корне, у VK ID - в
        // объекте `user`.
        let yandex = Provider::yandex(YANDEX_BASE, YANDEX_INFO);
        let body = r#"{"id":"1000034426","login":"ivan","default_email":"ivan@ya.ru",
                       "real_name":"Иван Иванов","psuid":"1.AAcCTA..."}"#;
        assert_eq!(subject_of(&yandex, body).as_deref(), Some("1000034426"));

        let vk = Provider::vk(VK_BASE, false);
        let body = r#"{"user":{"user_id":"7654321","first_name":"Иван",
                       "email":"ivan@vk.com","phone":"79001234567"}}"#;
        assert_eq!(subject_of(&vk, body).as_deref(), Some("7654321"));

        // Числовой идентификатор - тоже идентификатор: площадки отдают его и строкой, и
        // числом.
        assert_eq!(
            subject_of(&vk, r#"{"user":{"user_id":7654321}}"#).as_deref(),
            Some("7654321")
        );
        // Тело, где есть и корень, и `user`: каждая площадка обязана взять своё.
        // Смотри разбор только в `user`, и Яндекс перестанет находиться.
        let both = r#"{"id":"yandex-42","user":{"user_id":"vk-42"}}"#;
        assert_eq!(subject_of(&yandex, both).as_deref(), Some("yandex-42"));
        assert_eq!(subject_of(&vk, both).as_deref(), Some("vk-42"));

        // Ответ без идентификатора - не идентичность.
        assert_eq!(subject_of(&vk, r#"{"user":{"email":"x@y"}}"#), None);
        assert_eq!(subject_of(&vk, "не json"), None);
    }

    #[test]
    fn the_platforms_are_described_in_one_place() {
        // Предмет проверки - что повадки площадок записаны, а не разбросаны по
        // обработчикам: вторая их копия разошлась бы с первой молча.
        let vk = Provider::vk(VK_BASE, false);
        assert_eq!(vk.pkce, Pkce::Required, "VK ID требует PKCE");
        assert!(vk.returns_device_id, "VK ID возвращает device_id");
        assert_eq!(vk.secret, Secret::ServiceToken);

        let yandex = Provider::yandex(YANDEX_BASE, YANDEX_INFO);
        assert!(!yandex.returns_device_id);
        assert_eq!(yandex.secret, Secret::Basic);
        assert_eq!(
            yandex.subject_field, "id",
            "не psuid: он привязан к приложению"
        );
    }

    #[test]
    fn a_fingerprint_is_not_the_value() {
        // `state` в базе лежит отпечатком (приём refresh-токенов 09a): утечка таблицы
        // не даёт довести чужой вход.
        let value = random_token(32);
        assert_ne!(fingerprint(&value), value);
        assert_eq!(fingerprint(&value), fingerprint(&value), "устойчив");
        assert_eq!(
            fingerprint("a").len(),
            64,
            "SHA-256 в шестнадцатеричном виде"
        );
    }
}
