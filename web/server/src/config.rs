//! Конфигурация сервера из окружения.
//!
//! # Правила
//!
//! Один преисправление - `TAKT_WEB_`; у каждого ключа умолчание, годное для запуска
//! на своей машине без единой переменной. Разбор числа, который не удался, -
//! **отказ с названным ключом и значением**, а не молчаливое умолчание:
//! опечатка в переменной иначе даёт стенд, работающий не так, как написано в
//! конфигурации.
//!
//! `TAKT_WEB_BASE_PATH` - преисправление за обратным прокси. Он есть потому, что у
//! референса из его отсутствия родилась поломка: клиент ходил в API
//! **абсолютными** путями, и за прокси `/tamagotchi/` ссылки уводили в корень.
//! Здесь страница строит адреса относительно себя, а сервер знает преисправление -
//! обе половины, и ни одна не полагается на другую.

use std::net::{IpAddr, SocketAddr};
use std::path::PathBuf;
use std::time::Duration;

/// Настройки запуска.
#[derive(Debug, Clone)]
pub struct Config {
    /// Адрес и порт прослушивания.
    pub listen: SocketAddr,
    /// Строка подключения к PostgreSQL.
    pub database_url: String,
    /// Каталог собранной статики (`web/dist`).
    pub static_dir: PathBuf,
    /// Преисправление за обратным прокси: `/` либо `/takt`.
    pub base_path: String,
    /// Секрет подписи access-токенов.
    pub jwt_secret: String,
    /// Срок жизни access-токена.
    pub access_ttl: Duration,
    /// Срок жизни refresh-токена.
    pub refresh_ttl: Duration,
    /// Предел тела запроса в байтах.
    pub body_limit: usize,
    /// Окно ограничения частоты.
    pub rate_window: Duration,
    /// Сколько попыток входа и регистраций допускается в окне с одного адреса.
    pub rate_limit: u32,
    /// Каталог хранилища исходников: `<владелец>/<проект>/<файлы>`.
    pub projects_dir: PathBuf,
    /// Срок хранения без обращений: дольше - проект сворачивается в архив.
    pub retention: Duration,
    /// Внешний адрес сервиса: из него строится `redirect_uri` площадок.
    ///
    /// Выводить его из заголовка `Host` нельзя: `redirect_uri` обязан **в точности**
    /// совпасть с зарегистрированным у площадки, а `Host` приходит от клиента. Пусто -
    /// вход через площадки выключен целиком.
    pub public_url: String,
    /// Настройки площадок входа.
    pub oauth: OAuthConfig,
    /// Как часто обходить хранилище в поисках залежавшихся проектов.
    ///
    /// Ноль отключает обход **в процессе** - тогда его ставят в `cron` командой
    /// `takt-web-server sweep`. Обход по времени есть у того, у кого `cron` нет (своя
    /// машина, проба стенда).
    pub sweep: Duration,
}

/// Настройки входа через площадки.
///
/// Умолчание у каждой площадки - "не настроена", и это **не** молчание: половина пары
/// ключей роняет старт с названным ключом. Молчаливое "площадка выключена" при заданном
/// `CLIENT_ID` означало бы, что конфигурация врёт.
#[derive(Debug, Clone, Default)]
pub struct OAuthConfig {
    pub yandex_client_id: String,
    pub yandex_client_secret: String,
    pub vk_client_id: String,
    pub vk_service_token: String,
    /// Включена ли в кабинете VK "Авторизация через Mail".
    ///
    /// Сервер узнать этого сам не может - это настройка кабинета площадки.
    pub vk_mail: bool,
    /// Корни адресов площадок. На стенде не задаются; подменяются проверками на
    /// поддельного провайдера (§6 проработки).
    pub yandex_base: String,
    pub yandex_info: String,
    pub vk_base: String,
    /// Таймаут исходящего запроса к площадке.
    pub timeout: Duration,
}

impl OAuthConfig {
    /// Читает настройки площадок.
    ///
    /// # Ошибки
    /// Задана половина пары ключей - отказ с названным ключом.
    pub fn from_env() -> Result<Self, ConfigError> {
        let config = Self {
            yandex_client_id: var("TAKT_WEB_OAUTH_YANDEX_CLIENT_ID", ""),
            yandex_client_secret: var("TAKT_WEB_OAUTH_YANDEX_CLIENT_SECRET", ""),
            vk_client_id: var("TAKT_WEB_OAUTH_VK_CLIENT_ID", ""),
            vk_service_token: var("TAKT_WEB_OAUTH_VK_SERVICE_TOKEN", ""),
            vk_mail: var("TAKT_WEB_OAUTH_VK_MAIL", "0") == "1",
            yandex_base: var("TAKT_WEB_OAUTH_YANDEX_BASE", crate::oauth::YANDEX_BASE),
            yandex_info: var("TAKT_WEB_OAUTH_YANDEX_INFO", crate::oauth::YANDEX_INFO),
            vk_base: var("TAKT_WEB_OAUTH_VK_BASE", crate::oauth::VK_BASE),
            timeout: Duration::from_secs(parse("TAKT_WEB_OAUTH_TIMEOUT", "10", "секунды")?),
        };
        config.check_pairs()?;
        Ok(config)
    }

    /// Половина пары - отказ с названным ключом.
    fn check_pairs(&self) -> Result<(), ConfigError> {
        for (first, first_key, second, second_key) in [
            (
                &self.yandex_client_id,
                "TAKT_WEB_OAUTH_YANDEX_CLIENT_ID",
                &self.yandex_client_secret,
                "TAKT_WEB_OAUTH_YANDEX_CLIENT_SECRET",
            ),
            (
                &self.vk_client_id,
                "TAKT_WEB_OAUTH_VK_CLIENT_ID",
                &self.vk_service_token,
                "TAKT_WEB_OAUTH_VK_SERVICE_TOKEN",
            ),
        ] {
            if first.is_empty() != second.is_empty() {
                let (set, missing) = if first.is_empty() {
                    (second_key, first_key)
                } else {
                    (first_key, second_key)
                };
                tracing::error!("задан {set}, но не {missing}");
                return Err(ConfigError {
                    key: missing,
                    value: String::new(),
                    what: "задана половина пары: без него площадка не включится",
                });
            }
        }
        Ok(())
    }

    /// Настроен ли Яндекс.
    pub fn has_yandex(&self) -> bool {
        !self.yandex_client_id.is_empty()
    }

    /// Настроен ли VK ID.
    pub fn has_vk(&self) -> bool {
        !self.vk_client_id.is_empty()
    }

    /// Настроена ли кнопка "Mail" - она живёт внутри VK ID.
    pub fn has_mail(&self) -> bool {
        self.has_vk() && self.vk_mail
    }
}

/// Отказ разбора конфигурации: ключ назван, значение показано.
#[derive(Debug, thiserror::Error)]
#[error("переменная {key}: значение '{value}' не разбирается ({what})")]
pub struct ConfigError {
    key: &'static str,
    value: String,
    what: &'static str,
}

/// Секрет по умолчанию. Заметный нарочно: он обязан бросаться в глаза и в конфигурации,
/// и в предупреждении при запуске.
pub const DEV_SECRET: &str = "секрет-для-своей-машины-не-для-стенда";

/// Подключение по умолчанию - своя машина.
///
/// Хранилище - **PostgreSQL**. Умолчание годится для запуска на своей машине; на стенде
/// строка задаётся переменной, и пароля в дереве нет.
pub const DEV_DATABASE_URL: &str = "postgresql://localhost/takt_web";

impl Config {
    /// Читает конфигурацию из окружения.
    ///
    /// # Ошибки
    /// [`ConfigError`], если значение переменной не разбирается.
    pub fn from_env() -> Result<Self, ConfigError> {
        let host: IpAddr = parse("TAKT_WEB_HOST", "127.0.0.1", "адрес")?;
        let port: u16 = parse("TAKT_WEB_PORT", "8730", "порт")?;
        Ok(Self {
            listen: SocketAddr::new(host, port),
            database_url: var("TAKT_WEB_DB", DEV_DATABASE_URL),
            static_dir: var("TAKT_WEB_STATIC", "web/dist").into(),
            base_path: normalize_base(&var("TAKT_WEB_BASE_PATH", "/")),
            // Умолчание секрета годится только для своей машины, и сервер говорит об
            // этом при запуске: молчаливый общеизвестный секрет на стенде означает, что
            // подписать токен может кто угодно.
            jwt_secret: var("TAKT_WEB_JWT_SECRET", DEV_SECRET),
            access_ttl: Duration::from_secs(parse("TAKT_WEB_ACCESS_TTL", "3600", "секунды")?),
            refresh_ttl: Duration::from_secs(parse("TAKT_WEB_REFRESH_TTL", "2592000", "секунды")?),
            body_limit: parse("TAKT_WEB_BODY_LIMIT", "1048576", "байты")?,
            rate_window: Duration::from_secs(parse("TAKT_WEB_RATE_WINDOW", "60", "секунды")?),
            rate_limit: parse("TAKT_WEB_RATE_LIMIT", "10", "число")?,
            projects_dir: var("TAKT_WEB_PROJECTS", "web/projects").into(),
            // 90 дней - а не круглое число: квартал без единого открытия - внятный
            // признак, что проект оставлен, и он же переживает перерыв между
            // семестрами.
            retention: Duration::from_secs(
                60 * 60 * 24 * parse::<u64>("TAKT_WEB_RETENTION_DAYS", "90", "число дней")?,
            ),
            public_url: var("TAKT_WEB_PUBLIC_URL", "")
                .trim_end_matches('/')
                .to_string(),
            oauth: OAuthConfig::from_env()?,
            sweep: Duration::from_secs(
                60 * 60 * parse::<u64>("TAKT_WEB_SWEEP_HOURS", "6", "число часов")?,
            ),
        })
    }

    /// Секрет остался умолчанием - стенду это не годится.
    pub fn uses_dev_secret(&self) -> bool {
        self.jwt_secret == DEV_SECRET
    }
}

fn var(key: &str, default: &str) -> String {
    std::env::var(key).unwrap_or_else(|_| default.to_string())
}

fn parse<T: std::str::FromStr>(
    key: &'static str,
    default: &str,
    what: &'static str,
) -> Result<T, ConfigError> {
    let value = var(key, default);
    value.parse().map_err(|_| ConfigError { key, value, what })
}

/// Приводит преисправление к виду `/` либо `/имя` - без хвостовой косой черты.
///
/// Хвостовая косая - источник двойных `//` в собранных адресах: один и тот же ресурс
/// становится двумя разными путями, и правило кеша по форме адреса перестаёт работать
/// на одном из них.
pub fn normalize_base(raw: &str) -> String {
    let trimmed = raw.trim().trim_end_matches('/');
    if trimmed.is_empty() {
        "/".to_string()
    } else if trimmed.starts_with('/') {
        trimmed.to_string()
    } else {
        format!("/{trimmed}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn base_path_is_normalized() {
        assert_eq!(normalize_base("/"), "/");
        assert_eq!(normalize_base(""), "/");
        assert_eq!(normalize_base("   "), "/");
        assert_eq!(normalize_base("/takt/"), "/takt");
        assert_eq!(normalize_base("takt"), "/takt");
        assert_eq!(normalize_base("/takt///"), "/takt");
    }

    #[test]
    fn defaults_start_without_a_single_variable() {
        let config = Config::from_env().expect("умолчания годны");
        assert_eq!(config.base_path, "/");
        assert!(config.uses_dev_secret(), "секрет по умолчанию опознаётся");
        assert_eq!(
            config.access_ttl.as_secs(),
            3600,
            "час — решение проработки"
        );
    }
}
