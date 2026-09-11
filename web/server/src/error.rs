//! Единый тип ошибок API.

use axum::Json;
use axum::http::StatusCode;
use axum::response::{IntoResponse, Response};
use serde::Serialize;

/// Ошибка уровня API.
#[derive(Debug, thiserror::Error)]
pub enum ApiError {
    /// Токен отсутствует, просрочен или недействителен.
    #[error("не авторизован")]
    Unauthorized,
    /// Роль не даёт доступа к этому ресурсу.
    #[error("недостаточно прав")]
    Forbidden,
    /// Ресурса нет - либо он есть, но спрашивающему его не видно.
    #[error("не найдено")]
    NotFound,
    /// Логин уже занят.
    #[error("логин уже занят")]
    LoginTaken,
    /// Неверная пара логин/пароль либо неподдерживаемый `grant_type`.
    #[error("неверные учётные данные")]
    InvalidCredentials,
    /// Слишком часто: окно ограничения исчерпано.
    #[error("слишком много попыток, повторите через {after_secs} с")]
    TooManyRequests {
        /// Через сколько секунд окно освободится.
        after_secs: u64,
    },
    /// Предел хранилища превышен: названы и предел, и факт.
    #[error("{message}")]
    LimitExceeded {
        /// Что превышено, сколько можно и сколько получено.
        message: String,
    },
    /// Запись устарела: у ресурса другая ревизия.
    ///
    /// Числа едут **полями**, а не только в тексте. Страница обязана предложить выбор
    /// "перечитать / перезаписать", и для перезаписи ей нужна ревизия сервера;
    /// разбирать её из человеческого сообщения значило бы сделать текст отказа частью
    /// протокола - он перестал бы переводиться и перестал бы правиться.
    #[error("{message}")]
    Conflict {
        /// Текст для человека.
        message: String,
        /// Ревизия, которую назвал автор; `None` - не назвал вовсе.
        seen: Option<i64>,
        /// Ревизия ресурса сейчас.
        actual: i64,
    },
    /// Запрос не годится: причина названа.
    #[error("{0}")]
    BadRequest(String),
    /// Внутренняя ошибка. Подробности - в журнале, наружу они не едут.
    #[error("внутренняя ошибка")]
    Internal(#[from] anyhow::Error),
}

/// Тело ответа об ошибке.
#[derive(Serialize)]
struct ErrorBody<'a> {
    error: &'a str,
    message: String,
}

impl ApiError {
    /// Код HTTP и машинный код ошибки.
    pub fn status_and_code(&self) -> (StatusCode, &'static str) {
        match self {
            Self::Unauthorized => (StatusCode::UNAUTHORIZED, "unauthorized"),
            Self::Forbidden => (StatusCode::FORBIDDEN, "forbidden"),
            Self::NotFound => (StatusCode::NOT_FOUND, "not_found"),
            Self::LoginTaken => (StatusCode::CONFLICT, "login_taken"),
            // 400, как в OAuth 2.0: `invalid_grant` - это отказ выдачи токена, а не "вы
            // не авторизованы" (401 просил бы предъявить токен там, где его как раз и
            // получают).
            Self::InvalidCredentials => (StatusCode::BAD_REQUEST, "invalid_grant"),
            Self::TooManyRequests { .. } => (StatusCode::TOO_MANY_REQUESTS, "too_many_requests"),
            // 413 у всех пределов, включая число файлов и число проектов: клиент по
            // одному коду показывает одно - "столько нельзя", - а текст называет, чего
            // именно и сколько.
            Self::LimitExceeded { .. } => (StatusCode::PAYLOAD_TOO_LARGE, "limit_exceeded"),
            // 409, а не 412: ревизию клиент шлёт в теле, а не заголовком `If-Match`, и
            // предусловия HTTP здесь нет - есть расхождение состояний, о котором автору
            // предстоит решить.
            Self::Conflict { .. } => (StatusCode::CONFLICT, "revision_conflict"),
            Self::BadRequest(_) => (StatusCode::BAD_REQUEST, "bad_request"),
            Self::Internal(_) => (StatusCode::INTERNAL_SERVER_ERROR, "internal"),
        }
    }
}

/// Отказ крейта проекта: нарушение формы - запрос негоден, превышение - предел,
/// файловая система - сбой сервиса.
impl From<takt_project::Error> for ApiError {
    fn from(error: takt_project::Error) -> Self {
        match error {
            takt_project::Error::Invalid(message) => Self::BadRequest(message),
            takt_project::Error::Limit(message) => Self::LimitExceeded { message },
            takt_project::Error::Io(message) => Self::Internal(anyhow::anyhow!(message)),
        }
    }
}

impl From<tokio_postgres::Error> for ApiError {
    fn from(error: tokio_postgres::Error) -> Self {
        Self::Internal(anyhow::Error::new(error))
    }
}

impl From<deadpool_postgres::PoolError> for ApiError {
    fn from(error: deadpool_postgres::PoolError) -> Self {
        Self::Internal(anyhow::Error::new(error))
    }
}

impl IntoResponse for ApiError {
    fn into_response(self) -> Response {
        let (status, code) = self.status_and_code();
        // Внутренняя ошибка пишется в журнал целиком, а наружу уходит одним словом:
        // подробности внутренней ошибки - это подсказка нападающему.
        if let Self::Internal(ref cause) = self {
            tracing::error!(error = %cause, "внутренняя ошибка");
        }
        let body = ErrorBody {
            error: code,
            message: self.to_string(),
        };
        // Отказ ревизии несёт числа полями: по ним страница строит выбор "перечитать /
        // перезаписать", не разбирая человеческий текст.
        if let Self::Conflict { seen, actual, .. } = self {
            let mut value = serde_json::to_value(&body).unwrap_or_default();
            if let Some(map) = value.as_object_mut() {
                map.insert("revision".into(), actual.into());
                map.insert(
                    "seen".into(),
                    seen.map(serde_json::Value::from)
                        .unwrap_or(serde_json::Value::Null),
                );
            }
            return (status, Json(value)).into_response();
        }
        (status, Json(body)).into_response()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_error_has_its_own_code() {
        // Два разных отказа с одним кодом означают, что страница не сможет их
        // различить, а тексты она показывать обязана по-разному.
        let errors = [
            ApiError::Unauthorized,
            ApiError::Forbidden,
            ApiError::NotFound,
            ApiError::LoginTaken,
            ApiError::InvalidCredentials,
            ApiError::TooManyRequests { after_secs: 1 },
            ApiError::LimitExceeded {
                message: "предел 1, получено 2".into(),
            },
            ApiError::Conflict {
                message: "ревизия 1, у ресурса 2".into(),
                seen: Some(1),
                actual: 2,
            },
            ApiError::BadRequest("причина".into()),
            ApiError::Internal(anyhow::anyhow!("причина")),
        ];
        let mut seen = std::collections::BTreeSet::new();
        for error in &errors {
            let (_, code) = error.status_and_code();
            assert!(seen.insert(code), "код '{code}' встречается дважды");
            assert!(!error.to_string().is_empty(), "отказ без текста: {code}");
        }
    }

    #[test]
    fn internal_error_says_nothing_outward() {
        // Подробность внутренней ошибки - подсказка нападающему: наружу едет одно
        // слово, целиком она уходит в журнал.
        let error = ApiError::Internal(anyhow::anyhow!("путь /etc/secret не читается"));
        assert_eq!(error.to_string(), "внутренняя ошибка");
    }
}
