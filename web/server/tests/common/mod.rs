//! Общий стенд проверок сервера.
//!
//! Отдельным модулем, потому что наборов проверок два, а стенд им нужен один: вторая
//! копия разошлась бы с первой при первой же правке схемы.
//!
//! # Изоляция
//!
//! Каждая проверка работает в **своей схеме** (`CREATE SCHEMA`), а не в своей базе:
//! схема создаётся мгновенно, а `search_path` уводит туда схему сервера целиком.
//! Соседние проверки друг друга не видят и идут параллельно.

#![allow(dead_code)]

use std::net::SocketAddr;
use std::sync::Arc;

use axum::body::Body;
use axum::http::{Request, StatusCode};
use http_body_util::BodyExt as _;
use tower::ServiceExt as _;

use takt_web_server::config::Config;
use takt_web_server::db;
use takt_web_server::projects;
use takt_web_server::rate::Window;
use takt_web_server::routes::{self, AppState};

/// Живой стенд: роутер и его состояние.
pub struct Stand {
    pub app: axum::Router,
    pub schema: String,
    pub url: String,
    /// Метка проверки: по ней зовётся и схема, и каталог исходников.
    pub tag: String,
    /// Хранилище исходников этой проверки.
    pub store: std::sync::Arc<takt_web_server::store::Store>,
    /// Версия модуля, которую стенд объявил проектам: её же ждут проверки архива.
    /// Второго носителя версии в проверках быть не должно.
    pub module_version: String,
}

/// Печатает причину пропуска: молча пропущенная проверка неотличима от прошедшей.
pub fn skipped(what: &str) {
    eprintln!("пропуск ({what}): не задан TAKT_WEB_TEST_DB — базы нет");
}

impl Stand {
    /// Поднимает стенд в своей схеме; `None` - базы нет, проверка пропускается.
    pub async fn open(tag: &str) -> Option<Stand> {
        Self::open_with(tag, |_| {}).await
    }

    /// То же, но с правкой конфигурации: окно частоты, сроки токенов.
    pub async fn open_with(tag: &str, tune: impl FnOnce(&mut Config)) -> Option<Stand> {
        let url = std::env::var("TAKT_WEB_TEST_DB").ok()?;
        let schema = format!("t_{tag}");
        // Схема пересоздаётся: прогон, оборвавшийся посередине, не должен мешать
        // следующему.
        let admin = db::pool(&url).ok()?;
        let client = admin.get().await.ok()?;
        client
            .batch_execute(&format!(
                "DROP SCHEMA IF EXISTS {schema} CASCADE; CREATE SCHEMA {schema}"
            ))
            .await
            .ok()?;

        let scoped = scoped_url(&url, &schema);
        let pool = db::pool(&scoped).ok()?;
        let mut client = pool.get().await.ok()?;
        db::prepare(&mut client).await.ok()?;

        // Хранилище исходников - свой каталог на каждую проверку: они идут параллельно,
        // и общий каталог сделал бы их зависимыми друг от друга (тот же приём, что у
        // каталогов тестов компилятора, 0190).
        let store = std::sync::Arc::new(
            takt_web_server::store::Store::new(
                std::env::temp_dir().join(format!("takt-web-{}-{tag}", std::process::id())),
            )
            .ok()?,
        );
        // Версия берётся из собранной статики, а не пишется числом: модуль лежит по
        // адресу с версией, и вписанное число отставало бы при каждом подъёме версии
        // крейта - проверки архива искали бы модуль, которого на диске нет.
        let module_version = static_version("takt_lang").unwrap_or_else(|| "0.0.0".to_string());
        let mut config = Config::from_env().ok()?;
        config.database_url = scoped;
        config.jwt_secret = "секрет-проверки".to_string();
        // Окно частоты у проверок широкое: узкое било бы по своим, а его предмет
        // проверяется отдельно.
        config.rate_limit = 1000;
        tune(&mut config);
        let rate = Window::new(config.rate_window, config.rate_limit);
        let state = Arc::new(AppState {
            config,
            pool,
            rate,
            // Версии берутся из собранной статики, а не пишутся числом: модуль лежит по
            // адресу с версией, и вписанное здесь число отставало бы при каждом подъёме
            // версии крейта - проверки архива начинали бы искать модуль, которого на
            // диске нет. Умолчание нужно там, где статики нет вовсе: тогда генерация в
            // архиве отказывает словами, и это отдельный проверяемый случай.
            module_version: module_version.clone(),
            language_version: static_version("language").unwrap_or_else(|| "0.0.0".to_string()),
            // Модули берутся из собранной статики, и в проверках её обычно нет:
            // выгрузка с генерацией тогда отказывает словами, а всё остальное -
            // исходники, метаданные, круговой рейс - проверяется без неё. Путь задаётся
            // `TAKT_WEB_TEST_STATIC`. Хранилище исходников - свой каталог на каждую
            // проверку: они идут параллельно, и общий каталог сделал бы их зависимыми
            // друг от друга (тот же приём, что у каталогов тестов компилятора, 0190).
            store: store.clone(),
            // Таймаут проверок короткий: случай "площадка не отвечает" проверяется
            // поддельным провайдером, который спит дольше него, и десять секунд
            // ожидания превратили бы набор в десять секунд сна.
            http: reqwest::Client::builder()
                .timeout(std::time::Duration::from_millis(700))
                .build()
                .ok()?,
            modules: std::env::var("TAKT_WEB_TEST_STATIC")
                .ok()
                .and_then(|dir| takt_web_server::module::Modules::new(dir).ok())
                .map(std::sync::Arc::new),
        });
        Some(Stand {
            app: routes::router(state),
            schema,
            url,
            tag: tag.to_string(),
            store,
            module_version,
        })
    }

    /// Строка подключения к схеме этого стенда.
    pub fn scoped(&self) -> String {
        scoped_url(&self.url, &self.schema)
    }

    /// Шлёт запрос от имени клиента с адресом.
    pub async fn call(&self, request: Request<Body>) -> (StatusCode, serde_json::Value) {
        let mut request = request;
        // Адрес нужен окну частоты: без него `ConnectInfo` не извлекается.
        request
            .extensions_mut()
            .insert(axum::extract::ConnectInfo(SocketAddr::from((
                [127, 0, 0, 1],
                40000,
            ))));
        let response = self.app.clone().oneshot(request).await.expect("ответ");
        let status = response.status();
        let bytes = response
            .into_body()
            .collect()
            .await
            .expect("тело")
            .to_bytes();
        let body = serde_json::from_slice(&bytes).unwrap_or(serde_json::Value::Null);
        (status, body)
    }

    pub async fn post(
        &self,
        path: &str,
        body: serde_json::Value,
    ) -> (StatusCode, serde_json::Value) {
        self.call(json_request("POST", path, None, body)).await
    }

    pub async fn post_as(
        &self,
        path: &str,
        token: &str,
        body: serde_json::Value,
    ) -> (StatusCode, serde_json::Value) {
        self.call(json_request("POST", path, Some(token), body))
            .await
    }

    pub async fn put_as(
        &self,
        path: &str,
        token: &str,
        body: serde_json::Value,
    ) -> (StatusCode, serde_json::Value) {
        self.call(json_request("PUT", path, Some(token), body))
            .await
    }

    pub async fn patch_as(
        &self,
        path: &str,
        token: &str,
        body: serde_json::Value,
    ) -> (StatusCode, serde_json::Value) {
        self.call(json_request("PATCH", path, Some(token), body))
            .await
    }

    pub async fn get_with(&self, path: &str, token: &str) -> (StatusCode, serde_json::Value) {
        self.get_as(path, token).await
    }

    /// Шлёт запрос и возвращает адрес перенаправления и cookie.
    ///
    /// Перенаправление не прослеживается: браузера здесь нет, а предмет проверки - что
    /// и куда сервер отправил.
    pub async fn follow(&self, path: &str, token: Option<&str>) -> (StatusCode, String, String) {
        let mut builder = Request::get(path);
        if let Some(token) = token {
            builder = builder.header("authorization", format!("Bearer {token}"));
        }
        let mut request = builder.body(Body::empty()).expect("запрос");
        request
            .extensions_mut()
            .insert(axum::extract::ConnectInfo(SocketAddr::from((
                [127, 0, 0, 1],
                40000,
            ))));
        let response = self.app.clone().oneshot(request).await.expect("ответ");
        let status = response.status();
        let location = response
            .headers()
            .get("location")
            .and_then(|value| value.to_str().ok())
            .unwrap_or_default()
            .to_string();
        // Из `Set-Cookie` берём только пару "имя=значение": остальное - свойства,
        // которые браузер обратно не шлёт.
        let cookie = response
            .headers()
            .get("set-cookie")
            .and_then(|value| value.to_str().ok())
            .and_then(|value| value.split(';').next())
            .unwrap_or_default()
            .to_string();
        (status, location, cookie)
    }

    /// Возврат с площадки: путь плюс cookie потока.
    pub async fn callback(&self, path: &str, cookie: &str) -> (StatusCode, String) {
        let mut request = Request::get(path)
            .header("cookie", cookie)
            .body(Body::empty())
            .expect("запрос");
        request
            .extensions_mut()
            .insert(axum::extract::ConnectInfo(SocketAddr::from((
                [127, 0, 0, 1],
                40000,
            ))));
        let response = self.app.clone().oneshot(request).await.expect("ответ");
        let status = response.status();
        let location = response
            .headers()
            .get("location")
            .and_then(|value| value.to_str().ok())
            .unwrap_or_default()
            .to_string();
        (status, location)
    }

    /// Есть ли у человека пароль и сколько у него связанных площадок.
    pub async fn identity_facts(&self, login: &str) -> (bool, i64) {
        let pool = db::pool(&self.scoped()).expect("пул");
        let client = pool.get().await.expect("соединение");
        let row = client
            .query_one(
                "SELECT pass_hash IS NOT NULL,
                        (SELECT count(*) FROM external_identities e WHERE e.user_id = u.id)
                 FROM users u WHERE lower(login) = lower($1)",
                &[&login],
            )
            .await
            .expect("человек");
        (row.get(0), row.get(1))
    }

    /// Состав колонок таблицы - по нему судится обещание "этого мы не храним".
    pub async fn columns(&self, table: &str) -> Vec<String> {
        let pool = db::pool(&self.scoped()).expect("пул");
        let client = pool.get().await.expect("соединение");
        let rows = client
            .query(
                "SELECT column_name FROM information_schema.columns
                 WHERE table_schema = $1 AND table_name = $2 ORDER BY column_name",
                &[&self.schema, &table],
            )
            .await
            .expect("колонки");
        rows.iter().map(|row| row.get(0)).collect()
    }

    /// Читает ответ байтами: архив - не JSON.
    pub async fn bytes(&self, path: &str, token: Option<&str>) -> (StatusCode, Vec<u8>) {
        let mut builder = Request::get(path);
        if let Some(token) = token {
            builder = builder.header("authorization", format!("Bearer {token}"));
        }
        let mut request = builder.body(Body::empty()).expect("запрос");
        request
            .extensions_mut()
            .insert(axum::extract::ConnectInfo(SocketAddr::from((
                [127, 0, 0, 1],
                40000,
            ))));
        let response = self.app.clone().oneshot(request).await.expect("ответ");
        let status = response.status();
        let body = response
            .into_body()
            .collect()
            .await
            .expect("тело")
            .to_bytes()
            .to_vec();
        (status, body)
    }

    /// Отправляет байты телом запроса: загрузка архива - не JSON.
    pub async fn upload(
        &self,
        path: &str,
        token: &str,
        body: &[u8],
    ) -> (StatusCode, serde_json::Value) {
        self.call(
            Request::post(path)
                .header("content-type", "application/zip")
                .header("authorization", format!("Bearer {token}"))
                .body(Body::from(body.to_vec()))
                .expect("запрос"),
        )
        .await
    }

    /// Шлёт запрос без токена: открытый проект открыт и для того, у кого учётной записи
    /// нет вовсе.
    pub async fn get(&self, path: &str) -> (StatusCode, serde_json::Value) {
        self.call(Request::get(path).body(Body::empty()).expect("запрос"))
            .await
    }

    pub async fn get_as(&self, path: &str, token: &str) -> (StatusCode, serde_json::Value) {
        self.call(
            Request::get(path)
                .header("authorization", format!("Bearer {token}"))
                .body(Body::empty())
                .expect("запрос"),
        )
        .await
    }

    pub async fn delete_as(&self, path: &str, token: &str) -> (StatusCode, serde_json::Value) {
        self.call(
            Request::delete(path)
                .header("authorization", format!("Bearer {token}"))
                .body(Body::empty())
                .expect("запрос"),
        )
        .await
    }

    /// Набивает владельцу проекты напрямую в базу.
    ///
    /// Мимо ручки нарочно: сотня запросов ради проверки предела - это сотня запросов ни
    /// за чем, а предмет проверки в том, что предел считается, а не в том, как проекты
    /// появились.
    pub async fn fill_projects(&self, login: &str, count: i64) {
        let pool = db::pool(&self.scoped()).expect("пул");
        let client = pool.get().await.expect("соединение");
        let owner: String = client
            .query_one(
                "SELECT id FROM users WHERE lower(login) = lower($1)",
                &[&login],
            )
            .await
            .expect("владелец")
            .get(0);
        for _ in 0..count {
            client
                .execute(
                    "INSERT INTO projects(id, owner_id, name, visibility, takt_lang,
                                          language_version, created_at, updated_at)
                     VALUES ($1, $2, 'набивка', 'private', '0.58.0', '0.17.0', 0, 0)",
                    &[&projects::new_id(), &owner],
                )
                .await
                .expect("проект");
        }
    }

    /// Набивает витрину открытыми проектами напрямую в базу.
    ///
    /// Мимо ручек нарочно, и вместе с поисковым значением: предмет замера -
    /// **поиск по десяти тысячам**, а не скорость десяти тысяч HTTP-запросов.
    /// Один проект несёт редкое слово `верёвкоукладчик`: искать по слову,
    /// которое есть у всех, значит мерить сортировку, а не индекс.
    pub async fn fill_public(&self, login: &str, count: i64) {
        let pool = db::pool(&self.scoped()).expect("пул");
        let client = pool.get().await.expect("соединение");
        let owner: String = client
            .query_one(
                "SELECT id FROM users WHERE lower(login) = lower($1)",
                &[&login],
            )
            .await
            .expect("владелец")
            .get(0);
        client
            .execute(
                "INSERT INTO projects(id, owner_id, name, description, visibility,
                                      takt_lang, language_version, created_at, updated_at, search)
                 SELECT md5(i::text || 'набивка'), $1,
                        'Образец ' || i, 'описание образца номер ' || i,
                        'public', '0.58.0', '0.17.0', i, i,
                        setweight(to_tsvector('russian', 'Образец ' || i), 'A') ||
                        setweight(to_tsvector('russian', 'описание образца номер ' || i), 'B') ||
                        setweight(to_tsvector('russian',
                            CASE WHEN i = 1 THEN 'model Верёвкоукладчик state Ожидание'
                                 ELSE 'model Реле state Ожидание переход Нагрев' END), 'C')
                 FROM generate_series(1, $2::bigint) AS i",
                &[&owner, &count],
            )
            .await
            .expect("набивка витрины");
        client
            .execute("ANALYZE projects", &[])
            .await
            .expect("статистика");
    }

    /// Отодвигает отметку обращения проекта назад - то же, что прошедшее время.
    ///
    /// Ждать девяносто дней проверка не может, а спать даже секунду - значит мерить не
    /// срок хранения, а терпение прогона. Двигается отметка: предмет проверки в том,
    /// что обход считает разницу.
    pub async fn age_project(&self, id: &str, seconds: i64) {
        let pool = db::pool(&self.scoped()).expect("пул");
        let client = pool.get().await.expect("соединение");
        client
            .execute(
                "UPDATE projects SET touched_at = touched_at - $2 WHERE id = $1",
                &[&id, &seconds],
            )
            .await
            .expect("отметка");
    }

    /// Гоняет обход по сроку хранения; возвращает, сколько свёрнуто.
    pub async fn sweep(&self, retention_days: i64) -> usize {
        let pool = db::pool(&self.scoped()).expect("пул");
        let client = pool.get().await.expect("соединение");
        takt_web_server::retention::sweep(&client, &self.store, retention_days * 86_400)
            .await
            .expect("обход")
    }

    /// Гоняет подметание осиротевших каталогов; возвращает, сколько убрано.
    ///
    /// Выдержка задаётся параметром (как срок хранения у `sweep`): ждать час, чтобы
    /// увидеть работу обхода, значило бы мерить терпение прогона. Оба значения - и
    /// "час", и "немедленно" - проверяются на одном и том же состоянии диска.
    pub async fn sweep_orphans(&self, grace_secs: i64) -> usize {
        let pool = db::pool(&self.scoped()).expect("пул");
        let client = pool.get().await.expect("соединение");
        takt_web_server::retention::sweep_orphans(&client, &self.store, grace_secs)
            .await
            .expect("подметание")
    }

    /// Убирает строку проекта мимо ручки - так, как это делает прямой `DELETE` в базе
    /// либо обрыв между двумя шагами удаления.
    pub async fn forget_project(&self, id: &str) {
        let pool = db::pool(&self.scoped()).expect("пул");
        let client = pool.get().await.expect("соединение");
        client
            .execute("DELETE FROM projects WHERE id = $1", &[&id])
            .await
            .expect("строка проекта");
    }

    /// Есть ли на диске каталог владельца.
    pub fn owner_dir_exists(&self, owner: &str) -> bool {
        self.store.root().join(owner).is_dir()
    }

    /// Свёрнут ли проект на диске.
    pub async fn is_packed(&self, id: &str) -> bool {
        let owner = self.owner_of(id).await;
        self.store.is_packed(&owner, id).expect("признак")
    }

    /// Есть ли на диске каталог проекта.
    ///
    /// Владелец передаётся, а не спрашивается у базы: проверка зовётся и после удаления
    /// проекта, когда строки уже нет.
    pub fn project_dir_exists(&self, owner: &str, id: &str) -> bool {
        self.store.root().join(owner).join(id).is_dir()
    }

    /// Идентификатор человека по логину.
    pub async fn user_id(&self, login: &str) -> String {
        let pool = db::pool(&self.scoped()).expect("пул");
        let client = pool.get().await.expect("соединение");
        client
            .query_one(
                "SELECT id FROM users WHERE lower(login) = lower($1)",
                &[&login],
            )
            .await
            .expect("человек")
            .get(0)
    }

    /// Владелец проекта - его идентификатор нужен для пути на диске.
    async fn owner_of(&self, id: &str) -> String {
        let pool = db::pool(&self.scoped()).expect("пул");
        let client = pool.get().await.expect("соединение");
        client
            .query_one("SELECT owner_id FROM projects WHERE id = $1", &[&id])
            .await
            .expect("проект")
            .get(0)
    }

    /// Убирает за собой: ни схема, ни каталог прогона не должны оставаться.
    pub async fn drop_schema(&self) {
        let _ = std::fs::remove_dir_all(std::env::temp_dir().join(format!(
            "takt-web-{}-{}",
            std::process::id(),
            self.tag
        )));
        if let Ok(pool) = db::pool(&self.url)
            && let Ok(client) = pool.get().await
        {
            let _ = client
                .batch_execute(&format!("DROP SCHEMA IF EXISTS {} CASCADE", self.schema))
                .await;
        }
    }
}

fn scoped_url(url: &str, schema: &str) -> String {
    format!("{url}?options=-c%20search_path%3D{schema}")
}

fn json_request(
    method: &str,
    path: &str,
    token: Option<&str>,
    body: serde_json::Value,
) -> Request<Body> {
    let mut builder = Request::builder()
        .method(method)
        .uri(path)
        .header("content-type", "application/json");
    if let Some(token) = token {
        builder = builder.header("authorization", format!("Bearer {token}"));
    }
    builder.body(Body::from(body.to_string())).expect("запрос")
}

/// Версия из `version.json` собранной статики; `None` - статики нет.
///
/// Тот же источник, что у сервера (`main.rs::versions`): второго носителя версии в
/// проекте быть не должно.
fn static_version(field: &str) -> Option<String> {
    let dir = std::env::var("TAKT_WEB_TEST_STATIC").ok()?;
    let text = std::fs::read_to_string(std::path::Path::new(&dir).join("version.json")).ok()?;
    let parsed: serde_json::Value = serde_json::from_str(&text).ok()?;
    parsed[field].as_str().map(str::to_string)
}
