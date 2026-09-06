//! Запуск сервера и команды администратора.
//!
//! Сам сервер живёт библиотекой (`lib.rs`): интеграционные тесты поднимают его роутер,
//! а до модулей бинарника дотянуться не могут.
//!
//! # Команды
//!
//! ```text
//! takt-web-server                           запустить сервер
//! takt-web-server admin <логин> <пароль>    завести администратора
//! takt-web-server passwd <логин> <пароль>   сменить пароль
//! takt-web-server sweep                     свернуть залежавшиеся и подмести
//!                                           осиротевшее
//! takt-web-server unlink <логин> <площадка> отвязать площадку от записи
//! ```
//!
//! Первый администратор и сброс пароля - командами: почта не хранится, и восстановления
//! по письму нет.

use std::net::SocketAddr;
use std::sync::Arc;

use takt_web_server::auth::{self, Role};
use takt_web_server::config::Config;
use takt_web_server::db;
use takt_web_server::module;
use takt_web_server::rate::Window;
use takt_web_server::retention;
use takt_web_server::routes::{self, AppState};
use takt_web_server::store::Store;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_env("TAKT_WEB_LOG")
                .unwrap_or_else(|_| "info".into()),
        )
        .init();

    let config = Config::from_env()?;
    let pool = db::pool(&config.database_url)?;
    {
        let mut client = pool.get().await?;
        db::prepare(&mut client).await?;
    }

    let arguments: Vec<String> = std::env::args().skip(1).collect();
    match arguments.first().map(String::as_str) {
        Some("admin") => return command_admin(&pool, &arguments).await,
        Some("passwd") => return command_passwd(&pool, &arguments).await,
        // Обход по сроку хранения командой - для `cron`. Он зовёт ту же функцию, что и
        // обход по времени внутри сервера: второй проход разошёлся бы с первым.
        Some("sweep") => return command_sweep(&pool, &config).await,
        // Отвязка администратором: человек потерял доступ к площадке, а отвязать её из
        // своей сессии уже не может - войти-то нечем.
        Some("unlink") => return command_unlink(&pool, &arguments).await,
        Some(unknown) => anyhow::bail!(
            "неизвестная команда '{unknown}'. Известны: admin, passwd, sweep, \
             unlink (без команды — запуск)"
        ),
        None => {}
    }

    if config.uses_dev_secret() {
        // Общеизвестный секрет подписи означает, что токен может подписать кто угодно.
        // Молчать об этом нельзя - но и падать нельзя: на своей машине умолчание и есть
        // удобство.
        tracing::warn!(
            "TAKT_WEB_JWT_SECRET не задан — работает умолчание для своей машины; \
             на стенде это означает, что access-токен может подписать кто угодно"
        );
    }

    let listen = config.listen;
    let rate = Window::new(config.rate_window, config.rate_limit);
    // Версии - из описи собранной статики: второго носителя у них нет, и сервер не
    // должен знать их числом.
    let (module_version, language_version) = versions(&config.static_dir);
    // Модули `takt-wasm` из статики: ими собирается вывод целей в архиве. Нет статики -
    // нет и модулей, и выгрузка с генерацией отказывает словами.
    let modules = match module::Modules::new(&config.static_dir) {
        Ok(modules) => Some(Arc::new(modules)),
        Err(error) => {
            tracing::warn!(%error, "модули takt-wasm недоступны — архив пойдёт без генерации");
            None
        }
    };
    let store = Arc::new(Store::new(&config.projects_dir)?);
    let sweep_every = config.sweep;
    let retention_secs = config.retention.as_secs() as i64;
    // Клиент площадок - один на сервер, с таймаутом из конфигурации: без него
    // обработчик висел бы вместе с площадкой, держа соединение к базе.
    let http = reqwest::Client::builder()
        .timeout(config.oauth.timeout)
        .build()?;
    // Настроенные способы входа печатаются одной строкой: "включено" и "выключено"
    // обязаны быть видны при запуске, а не выясняться нажатием.
    let mut ways = vec!["пароль".to_string()];
    if config.oauth.has_yandex() {
        ways.push("yandex".to_string());
    }
    if config.oauth.has_vk() {
        ways.push("vk".to_string());
    }
    if config.oauth.has_mail() {
        ways.push("mail".to_string());
    }
    if ways.len() > 1 && config.public_url.is_empty() {
        anyhow::bail!(
            "площадки входа настроены, но не задан TAKT_WEB_PUBLIC_URL — \
             redirect_uri построить не из чего"
        );
    }
    tracing::info!("вход через: {}", ways.join(", "));
    let state = Arc::new(AppState {
        config,
        pool,
        rate,
        module_version,
        language_version,
        modules,
        store: store.clone(),
        http,
    });

    // Обход по сроку хранения - в процессе. Он же доступен командой (`sweep`): у стенда
    // бывает `cron`, у своей машины - нет, и обе половины зовут одну функцию.
    if !sweep_every.is_zero() {
        let pool = state.pool.clone();
        let store = store.clone();
        tokio::spawn(async move {
            let mut ticker = tokio::time::interval(sweep_every);
            loop {
                ticker.tick().await;
                match pool.get().await {
                    Ok(client) => {
                        match retention::sweep(&client, &store, retention_secs).await {
                            Ok(0) => {}
                            Ok(packed) => {
                                tracing::info!(packed, "проекты свёрнуты по сроку хранения")
                            }
                            Err(error) => {
                                tracing::warn!(%error, "обход по сроку хранения не удался")
                            }
                        }
                        // Подметание сирот - тем же обходом: у стенда бывает `cron`, у
                        // своей машины нет, а мусор копится у обоих.
                        match retention::sweep_orphans(&client, &store, retention::ORPHAN_GRACE)
                            .await
                        {
                            Ok(0) => {}
                            Ok(swept) => tracing::info!(swept, "осиротевшие каталоги убраны"),
                            Err(error) => {
                                tracing::warn!(%error, "подметание осиротевших не удалось")
                            }
                        }
                    }
                    Err(error) => tracing::warn!(%error, "обход: базы нет"),
                }
            }
        });
    }

    let listener = tokio::net::TcpListener::bind(listen).await?;
    tracing::info!(%listen, "сервер слушает");
    axum::serve(
        listener,
        routes::router(state).into_make_service_with_connect_info::<SocketAddr>(),
    )
    .with_graceful_shutdown(shutdown())
    .await?;
    Ok(())
}

async fn command_admin(pool: &deadpool_postgres::Pool, arguments: &[String]) -> anyhow::Result<()> {
    let (Some(login), Some(password)) = (arguments.get(1), arguments.get(2)) else {
        anyhow::bail!("takt-web-server admin <логин> <пароль>");
    };
    let client = pool.get().await?;
    auth::register(&client, login, password, Role::Admin)
        .await
        .map_err(|e| anyhow::anyhow!("{e}"))?;
    println!("администратор '{login}' заведён");
    Ok(())
}

async fn command_passwd(
    pool: &deadpool_postgres::Pool,
    arguments: &[String],
) -> anyhow::Result<()> {
    let (Some(login), Some(password)) = (arguments.get(1), arguments.get(2)) else {
        anyhow::bail!("takt-web-server passwd <логин> <пароль>");
    };
    let client = pool.get().await?;
    auth::set_password(&client, login, password)
        .await
        .map_err(|e| anyhow::anyhow!("{e}"))?;
    println!("пароль '{login}' сменён; живые сеансы погашены");
    Ok(())
}

async fn command_sweep(pool: &deadpool_postgres::Pool, config: &Config) -> anyhow::Result<()> {
    let store = Arc::new(Store::new(&config.projects_dir)?);
    let client = pool.get().await?;
    let packed = retention::sweep(&client, &store, config.retention.as_secs() as i64).await?;
    // Второй проход идёт в обратную сторону: первый сворачивает то, что база знает,
    // второй убирает то, чего она не знает вовсе. Порядок именно такой - свёртка
    // заводит `.zip` рядом с каталогом, и подметание обязано видеть уже сложившееся
    // состояние, а не промежуточное.
    let swept = retention::sweep_orphans(&client, &store, retention::ORPHAN_GRACE).await?;
    println!(
        "свёрнуто проектов: {packed} (срок хранения — {} дней); \
         осиротевших каталогов убрано: {swept}",
        config.retention.as_secs() / 86_400
    );
    Ok(())
}

async fn command_unlink(
    pool: &deadpool_postgres::Pool,
    arguments: &[String],
) -> anyhow::Result<()> {
    let (Some(login), Some(provider)) = (arguments.get(1), arguments.get(2)) else {
        anyhow::bail!("takt-web-server unlink <логин> <площадка>");
    };
    let client = pool.get().await?;
    let row = client
        .query_opt(
            "SELECT id FROM users WHERE lower(login) = lower($1)",
            &[&login],
        )
        .await?;
    let Some(row) = row else {
        anyhow::bail!("нет такого логина: {login}");
    };
    let id: String = row.get(0);
    // Правило то же, что у ручки: отвязка последнего способа войти отказывает и
    // администратору. Иначе он одной командой делал бы запись недостижимой - и починить
    // её было бы уже нечем.
    takt_web_server::oauth::api::unlink(&client, &id, provider)
        .await
        .map_err(|error| anyhow::anyhow!("{error}"))?;
    println!("площадка '{provider}' отвязана от '{login}'");
    Ok(())
}

/// Читает версии из `version.json` собранной статики.
///
/// Умолчание - пустые строки, а не выдуманные числа: сервер, запущенный без собранной
/// статики, обязан это показывать, а не сообщать проекту версию, модуля которой на
/// диске нет.
fn versions(static_dir: &std::path::Path) -> (String, String) {
    let Ok(text) = std::fs::read_to_string(static_dir.join("version.json")) else {
        tracing::warn!("нет version.json в статике — новый проект не получит версию модуля");
        return (String::new(), String::new());
    };
    let parsed: serde_json::Value = serde_json::from_str(&text).unwrap_or_default();
    (
        parsed["takt_lang"].as_str().unwrap_or_default().to_string(),
        parsed["language"].as_str().unwrap_or_default().to_string(),
    )
}

/// Останов по сигналу: соединения дорабатываются, а не рвутся.
async fn shutdown() {
    let _ = tokio::signal::ctrl_c().await;
    tracing::info!("останов");
}
