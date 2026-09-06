//! Учётные записи и токены.
//!
//! # Устройство
//!
//! - **Пароль** - argon2id (приём референса). Хеш и его параметры лежат в самой
//!   строке хеша, поэтому смена стоимости не требует миграции.
//! - **Access-токен** - JWT HS256 сроком час. Час, а не сутки как у референса:
//!   редактор обновляет пару refresh-токеном незаметно, а короткий срок
//!   сокращает окно, в котором "выйти" ещё не подействовало.
//! - **Refresh-токен** - непрозрачные 32 байта; в базе лежит **отпечаток**
//!   SHA-256. Утечка базы не даёт войти.
//! - **Семейство** - цепочка от одного входа. Токен **одноразовый**, и
//!   повторное предъявление гасит семейство целиком: так кража токена
//!   обнаруживается сама - настоящий владелец обменяет свой, украденный
//!   предъявят второй раз, и вход прекратится у обоих.
//!
//! **Роль читается из базы на каждый запрос**, а не из токена: иначе снятие права
//! администратора действовало бы только после истечения токена.

use anyhow::Context as _;
use argon2::Argon2;
use argon2::password_hash::rand_core::OsRng;
use argon2::password_hash::{PasswordHash, PasswordHasher, PasswordVerifier, SaltString};
use base64::Engine as _;
use rand::RngCore as _;
use serde::{Deserialize, Serialize};
use sha2::{Digest as _, Sha256};
use tokio_postgres::Client;

use crate::db;
use crate::error::ApiError;

/// Роль учётной записи.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Role {
    User,
    Admin,
}

impl Role {
    /// Имя роли в базе.
    pub fn as_str(self) -> &'static str {
        match self {
            Self::User => "user",
            Self::Admin => "admin",
        }
    }

    /// Разбирает имя роли; неизвестное - не администратор.
    ///
    /// Умолчание намеренно наименьшее: испорченная строка в базе не должна давать прав.
    pub fn parse(raw: &str) -> Self {
        if raw == "admin" {
            Self::Admin
        } else {
            Self::User
        }
    }
}

/// Учётная запись.
#[derive(Debug, Clone)]
pub struct User {
    pub id: String,
    pub login: String,
    pub role: Role,
}

/// Содержимое access-токена.
#[derive(Debug, Serialize, Deserialize)]
pub struct Claims {
    /// Идентификатор пользователя.
    pub sub: String,
    /// Момент истечения, Unix-секунды.
    pub exp: i64,
}

/// Правила имени: длина и алфавит.
///
/// Алфавит узкий намеренно: логин показывается у открытого проекта как имя автора, и
/// пробелы, управляющие символы и похожие начертания в нём - приглашение к подделке.
pub fn check_login(login: &str) -> Result<(), ApiError> {
    let length = login.chars().count();
    if !(3..=32).contains(&length) {
        return Err(ApiError::BadRequest(
            "логин: от 3 до 32 символов".to_string(),
        ));
    }
    if !login
        .chars()
        .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '-')
    {
        return Err(ApiError::BadRequest(
            "логин: латинские буквы, цифры, '_' и '-'".to_string(),
        ));
    }
    Ok(())
}

/// Правила пароля: только длина.
///
/// Требований "буква, цифра, знак" здесь нет намеренно: они укорачивают пароль, который
/// человек в состоянии запомнить, не прибавляя стойкости. Предел сверху есть - argon2
/// считает длинный вход столько же, сколько короткий, но принимать мегабайт паролем
/// незачем.
pub fn check_password(password: &str) -> Result<(), ApiError> {
    let length = password.chars().count();
    if !(8..=256).contains(&length) {
        return Err(ApiError::BadRequest(
            "пароль: от 8 до 256 символов".to_string(),
        ));
    }
    Ok(())
}

/// Считает хеш пароля.
pub fn hash_password(password: &str) -> anyhow::Result<String> {
    let salt = SaltString::generate(&mut OsRng);
    Argon2::default()
        .hash_password(password.as_bytes(), &salt)
        .map(|hash| hash.to_string())
        .map_err(|e| anyhow::anyhow!("argon2: {e}"))
}

/// Сверяет пароль с хешем.
pub fn verify_password(password: &str, hash: &str) -> bool {
    let Ok(parsed) = PasswordHash::new(hash) else {
        return false;
    };
    Argon2::default()
        .verify_password(password.as_bytes(), &parsed)
        .is_ok()
}

/// Отпечаток refresh-токена: в базе лежит он, а не сам токен.
pub fn fingerprint(token: &str) -> String {
    let digest = Sha256::digest(token.as_bytes());
    digest.iter().map(|b| format!("{b:02x}")).collect()
}

/// Новый непрозрачный токен: 32 случайных байта, base64url.
pub fn new_token() -> String {
    let mut bytes = [0u8; 32];
    rand::thread_rng().fill_bytes(&mut bytes);
    base64::engine::general_purpose::URL_SAFE_NO_PAD.encode(bytes)
}

/// Подписывает access-токен.
pub fn issue_access(secret: &str, user_id: &str, ttl_secs: i64) -> anyhow::Result<String> {
    let claims = Claims {
        sub: user_id.to_string(),
        exp: db::now() + ttl_secs,
    };
    jsonwebtoken::encode(
        &jsonwebtoken::Header::default(),
        &claims,
        &jsonwebtoken::EncodingKey::from_secret(secret.as_bytes()),
    )
    .context("подпись access-токена")
}

/// Разбирает access-токен; `None` - он негоден или просрочен.
pub fn read_access(secret: &str, token: &str) -> Option<Claims> {
    jsonwebtoken::decode::<Claims>(
        token,
        &jsonwebtoken::DecodingKey::from_secret(secret.as_bytes()),
        &jsonwebtoken::Validation::default(),
    )
    .ok()
    .map(|data| data.claims)
}

/// Заводит учётную запись.
///
/// # Ошибки
/// [`ApiError::LoginTaken`] - логин занят; [`ApiError::BadRequest`] - имя или
/// пароль не по правилам.
pub async fn register(
    client: &Client,
    login: &str,
    password: &str,
    role: Role,
) -> Result<User, ApiError> {
    check_login(login)?;
    check_password(password)?;
    let hash = hash_password(password).map_err(ApiError::Internal)?;
    let id = uuid::Uuid::new_v4().to_string();
    // Единственность держит индекс по `lower(login)`, поэтому и конфликт объявляется по
    // нему: `ON CONFLICT (login)` не сработал бы вовсе.
    let affected = client
        .execute(
            "INSERT INTO users(id, login, pass_hash, role, created_at)
             VALUES ($1, $2, $3, $4, $5)
             ON CONFLICT (lower(login)) DO NOTHING",
            &[&id, &login, &hash, &role.as_str(), &db::now()],
        )
        .await?;
    if affected == 0 {
        return Err(ApiError::LoginTaken);
    }
    Ok(User {
        id,
        login: login.to_string(),
        role,
    })
}

/// Находит пользователя по логину и паролю.
///
/// Отказ один на оба случая - нет такого логина и неверный пароль: иначе ручка
/// перечисляет заведённые логины.
pub async fn authenticate(client: &Client, login: &str, password: &str) -> Result<User, ApiError> {
    let rows = client
        .query(
            "SELECT id, login, pass_hash, role FROM users WHERE lower(login) = lower($1)",
            &[&login],
        )
        .await?;
    let Some(row) = rows.first() else {
        return Err(ApiError::InvalidCredentials);
    };
    // Пароля может не быть вовсе: человек, вошедший через площадку, его у нас не
    // заводил. Ответ тот же, что у неверного пароля - иначе ручка сообщала бы, каким
    // способом заведена чужая запись.
    let Some(hash) = row.get::<_, Option<String>>(2) else {
        return Err(ApiError::InvalidCredentials);
    };
    if !verify_password(password, &hash) {
        return Err(ApiError::InvalidCredentials);
    }
    Ok(User {
        id: row.get(0),
        login: row.get(1),
        role: Role::parse(&row.get::<_, String>(3)),
    })
}

/// Читает пользователя по идентификатору - вместе с ролью **из базы**.
pub async fn load(client: &Client, id: &str) -> Result<Option<User>, ApiError> {
    let rows = client
        .query("SELECT id, login, role FROM users WHERE id = $1", &[&id])
        .await?;
    Ok(rows.first().map(|row| User {
        id: row.get(0),
        login: row.get(1),
        role: Role::parse(&row.get::<_, String>(2)),
    }))
}

/// Меняет пароль. Команда администратора: восстановления по почте нет.
pub async fn set_password(client: &Client, login: &str, password: &str) -> Result<(), ApiError> {
    check_password(password)?;
    let hash = hash_password(password).map_err(ApiError::Internal)?;
    let affected = client
        .execute(
            "UPDATE users SET pass_hash = $1 WHERE lower(login) = lower($2)",
            &[&hash, &login],
        )
        .await?;
    if affected == 0 {
        return Err(ApiError::NotFound);
    }
    // Смена пароля гасит все живые цепочки: иначе украденный refresh пережил бы её и
    // остался бы входом.
    client
        .execute(
            "UPDATE refresh_tokens SET revoked_at = $1
             WHERE revoked_at IS NULL
               AND user_id = (SELECT id FROM users WHERE lower(login) = lower($2))",
            &[&db::now(), &login],
        )
        .await?;
    Ok(())
}

/// Выданная пара токенов.
#[derive(Debug, Clone)]
pub struct Pair {
    pub access: String,
    pub refresh: String,
}

/// Выдаёт пару токенов, начиная новое семейство.
pub async fn start_session(
    client: &Client,
    secret: &str,
    user: &User,
    access_ttl: i64,
    refresh_ttl: i64,
) -> Result<Pair, ApiError> {
    let family = uuid::Uuid::new_v4().to_string();
    issue_pair(client, secret, user, &family, access_ttl, refresh_ttl).await
}

/// Обменивает refresh-токен на новую пару.
///
/// Токен одноразовый, и предъявление уже погашенного гасит **всё семейство**: так кража
/// обнаруживается сама. Настоящий владелец обменяет свой, украденный предъявят вторым -
/// и вход прекратится у обоих, что заметно, в отличие от тихо работающей кражи.
pub async fn refresh(
    client: &Client,
    secret: &str,
    token: &str,
    access_ttl: i64,
    refresh_ttl: i64,
) -> Result<Pair, ApiError> {
    let hash = fingerprint(token);
    let rows = client
        .query(
            "SELECT id, user_id, family, expires_at, revoked_at
             FROM refresh_tokens WHERE token_hash = $1",
            &[&hash],
        )
        .await?;
    let Some(row) = rows.first() else {
        return Err(ApiError::InvalidCredentials);
    };
    let id: i64 = row.get(0);
    let user_id: String = row.get(1);
    let family: String = row.get(2);
    let expires_at: i64 = row.get(3);
    let revoked_at: Option<i64> = row.get(4);
    if revoked_at.is_some() {
        revoke_family(client, &family).await?;
        return Err(ApiError::InvalidCredentials);
    }
    if expires_at <= db::now() {
        return Err(ApiError::InvalidCredentials);
    }
    let Some(user) = load(client, &user_id).await? else {
        return Err(ApiError::InvalidCredentials);
    };
    client
        .execute(
            "UPDATE refresh_tokens SET revoked_at = $1 WHERE id = $2",
            &[&db::now(), &id],
        )
        .await?;
    issue_pair(client, secret, &user, &family, access_ttl, refresh_ttl).await
}

/// Гасит один токен.
///
/// Неизвестный токен - **не ошибка**: ответ "готово" одинаков для своего и чужого,
/// иначе ручка отвечает на вопрос "а такой токен бывает?".
pub async fn revoke(client: &Client, token: &str) -> Result<(), ApiError> {
    client
        .execute(
            "UPDATE refresh_tokens SET revoked_at = $1
             WHERE token_hash = $2 AND revoked_at IS NULL",
            &[&db::now(), &fingerprint(token)],
        )
        .await?;
    Ok(())
}

/// Гасит семейство целиком.
pub async fn revoke_family(client: &Client, family: &str) -> Result<(), ApiError> {
    client
        .execute(
            "UPDATE refresh_tokens SET revoked_at = $1
             WHERE family = $2 AND revoked_at IS NULL",
            &[&db::now(), &family],
        )
        .await?;
    Ok(())
}

async fn issue_pair(
    client: &Client,
    secret: &str,
    user: &User,
    family: &str,
    access_ttl: i64,
    refresh_ttl: i64,
) -> Result<Pair, ApiError> {
    let refresh = new_token();
    let now = db::now();
    client
        .execute(
            "INSERT INTO refresh_tokens(user_id, token_hash, family, created_at, expires_at)
             VALUES ($1, $2, $3, $4, $5)",
            &[
                &user.id,
                &fingerprint(&refresh),
                &family,
                &now,
                &(now + refresh_ttl),
            ],
        )
        .await?;
    let access = issue_access(secret, &user.id, access_ttl).map_err(ApiError::Internal)?;
    Ok(Pair { access, refresh })
}

#[cfg(test)]
mod tests {
    use super::*;

    // Здесь только то, чему база не нужна: пароли, токены, правила имени. Всё, что
    // ходит в PostgreSQL, живёт в `tests/http.rs` и пропускается, если базы нет, -
    // иначе `cargo test` требовал бы сервера субд.

    #[test]
    fn password_survives_round_trip_and_hash_differs_each_time() {
        let first = hash_password("правильный-пароль").expect("хеш");
        let second = hash_password("правильный-пароль").expect("хеш");
        assert_ne!(first, second, "соль обязана быть своя у каждого хеша");
        assert!(verify_password("правильный-пароль", &first));
        assert!(!verify_password("другой-пароль", &first));
        assert!(!verify_password("правильный-пароль", "не хеш вовсе"));
    }

    #[test]
    fn login_rules_are_named() {
        assert!(check_login("ab").is_err(), "короткий");
        assert!(check_login(&"a".repeat(33)).is_err(), "длинный");
        assert!(check_login("иван").is_err(), "не латиница");
        assert!(check_login("иван иванов").is_err(), "пробел");
        assert!(check_login("ivan_2-x").is_ok());
        assert!(check_password("короткий").is_ok());
        assert!(check_password("семь777").is_err());
    }

    #[test]
    fn access_token_carries_the_user_and_expires() {
        let token = issue_access("проба", "u-1", 60).expect("подпись");
        let claims = read_access("проба", &token).expect("разбор");
        assert_eq!(claims.sub, "u-1");
        assert!(
            read_access("другой секрет", &token).is_none(),
            "чужая подпись"
        );
        let stale = issue_access("проба", "u-1", -120).expect("подпись");
        assert!(
            read_access("проба", &stale).is_none(),
            "просроченный принят"
        );
    }

    #[test]
    fn fingerprint_is_stable_and_hides_the_token() {
        let token = new_token();
        assert_eq!(
            fingerprint(&token),
            fingerprint(&token),
            "отпечаток устойчив"
        );
        assert_ne!(
            fingerprint(&token),
            token,
            "в базе не должно быть самого токена"
        );
        assert_eq!(fingerprint(&token).len(), 64, "SHA-256 шестнадцатеричным");
        assert_ne!(new_token(), new_token(), "токены не повторяются");
    }

    #[test]
    fn unknown_role_is_not_an_admin() {
        assert_eq!(Role::parse("admin"), Role::Admin);
        assert_eq!(Role::parse("user"), Role::User);
        assert_eq!(
            Role::parse("КОРОЛЬ"),
            Role::User,
            "испорченная строка не даёт прав"
        );
    }
}
