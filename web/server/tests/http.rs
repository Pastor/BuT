//! Проверки входа через HTTP.
//!
//! Стенд и политика пропуска - в `common`: наборов проверок два, а стенд им нужен один.

mod common;

use axum::body::Body;
use axum::http::{Request, StatusCode};

use common::{Stand, skipped};
use takt_web_server::config::Config;
use takt_web_server::{auth, db};

#[tokio::test]
async fn health_answers_only_when_the_database_answers() {
    let Some(stand) = Stand::open("health").await else {
        return skipped("здоровье");
    };
    let (status, body) = stand
        .call(Request::get("/health").body(Body::empty()).expect("запрос"))
        .await;
    assert_eq!(status, StatusCode::OK);
    assert_eq!(body["status"], "ok");
    stand.drop_schema().await;
}

#[tokio::test]
async fn registration_gives_a_pair_and_the_login_is_taken_after() {
    let Some(stand) = Stand::open("register").await else {
        return skipped("регистрация");
    };
    let (status, body) = stand
        .post(
            "/api/register",
            serde_json::json!({"login": "ivan", "password": "пароль-пароль"}),
        )
        .await;
    assert_eq!(status, StatusCode::CREATED, "{body}");
    assert!(body["access_token"].is_string(), "{body}");
    assert!(body["refresh_token"].is_string(), "{body}");
    assert_eq!(body["token_type"], "Bearer");

    // Логин занят без учёта регистра: иначе два владельца получили бы неразличимые на
    // глаз имена.
    let (status, body) = stand
        .post(
            "/api/register",
            serde_json::json!({"login": "IVAN", "password": "другой-пароль"}),
        )
        .await;
    assert_eq!(status, StatusCode::CONFLICT, "{body}");
    assert_eq!(body["error"], "login_taken");
    stand.drop_schema().await;
}

#[tokio::test]
async fn bad_login_and_bad_password_answer_the_same() {
    // Разные ответы перечисляют заведённые логины.
    let Some(stand) = Stand::open("oracle").await else {
        return skipped("оракул логинов");
    };
    stand
        .post(
            "/api/register",
            serde_json::json!({"login": "ivan", "password": "пароль-пароль"}),
        )
        .await;
    let (no_user, first) = stand
        .post(
            "/api/token",
            serde_json::json!({"grant_type": "password", "login": "petr", "password": "пароль-пароль"}),
        )
        .await;
    let (bad_password, second) = stand
        .post(
            "/api/token",
            serde_json::json!({"grant_type": "password", "login": "ivan", "password": "не-тот"}),
        )
        .await;
    assert_eq!(no_user, bad_password);
    assert_eq!(first, second, "ответы обязаны совпадать целиком");
    assert_eq!(first["error"], "invalid_grant");
    stand.drop_schema().await;
}

#[tokio::test]
async fn refresh_exchange_and_reuse_kills_the_family() {
    // Главное свойство цепочки: кража обнаруживается сама. Владелец обменяет свой
    // токен, украденный предъявят вторым - и вход прекратится у обоих, что заметно, в
    // отличие от тихо работающей кражи.
    let Some(stand) = Stand::open("refresh").await else {
        return skipped("обмен токенов");
    };
    let (_, registered) = stand
        .post(
            "/api/register",
            serde_json::json!({"login": "ivan", "password": "пароль-пароль"}),
        )
        .await;
    let first = registered["refresh_token"]
        .as_str()
        .expect("токен")
        .to_string();

    let (status, exchanged) = stand
        .post(
            "/api/token",
            serde_json::json!({"grant_type": "refresh_token", "refresh_token": first}),
        )
        .await;
    assert_eq!(status, StatusCode::OK, "{exchanged}");
    let second = exchanged["refresh_token"]
        .as_str()
        .expect("токен")
        .to_string();
    assert_ne!(second, first, "обмен обязан выдать новый токен");

    let (status, _) = stand
        .post(
            "/api/token",
            serde_json::json!({"grant_type": "refresh_token", "refresh_token": first}),
        )
        .await;
    assert_eq!(status, StatusCode::BAD_REQUEST, "одноразовый принят дважды");

    let (status, body) = stand
        .post(
            "/api/token",
            serde_json::json!({"grant_type": "refresh_token", "refresh_token": second}),
        )
        .await;
    assert_eq!(
        status,
        StatusCode::BAD_REQUEST,
        "семейство обязано было погаснуть целиком: {body}"
    );
    stand.drop_schema().await;
}

#[tokio::test]
async fn revoke_says_the_same_about_a_stranger_token() {
    let Some(stand) = Stand::open("revoke").await else {
        return skipped("гашение токена");
    };
    let (_, registered) = stand
        .post(
            "/api/register",
            serde_json::json!({"login": "ivan", "password": "пароль-пароль"}),
        )
        .await;
    let token = registered["refresh_token"]
        .as_str()
        .expect("токен")
        .to_string();

    let (stranger, _) = stand
        .post(
            "/api/revoke",
            serde_json::json!({"refresh_token": "нет-такого"}),
        )
        .await;
    let (own, _) = stand
        .post("/api/revoke", serde_json::json!({"refresh_token": token}))
        .await;
    assert_eq!(stranger, StatusCode::NO_CONTENT);
    assert_eq!(own, stranger, "ответ обязан быть одним: ручка не оракул");

    let (status, _) = stand
        .post(
            "/api/token",
            serde_json::json!({"grant_type": "refresh_token", "refresh_token": token}),
        )
        .await;
    assert_eq!(status, StatusCode::BAD_REQUEST, "погашенный токен принят");
    stand.drop_schema().await;
}

#[tokio::test]
async fn me_needs_a_token_and_reads_the_role_from_the_database() {
    // Роль читается из базы, а не из токена: иначе снятие права администратора
    // действовало бы только после истечения часа.
    let Some(stand) = Stand::open("me").await else {
        return skipped("сведения о себе");
    };
    let (_, registered) = stand
        .post(
            "/api/register",
            serde_json::json!({"login": "ivan", "password": "пароль-пароль"}),
        )
        .await;
    let access = registered["access_token"]
        .as_str()
        .expect("токен")
        .to_string();

    let (status, body) = stand.get_with("/api/me", &access).await;
    assert_eq!(status, StatusCode::OK, "{body}");
    assert_eq!(body["login"], "ivan");
    assert_eq!(body["role"], "user");

    let (status, _) = stand
        .call(Request::get("/api/me").body(Body::empty()).expect("запрос"))
        .await;
    assert_eq!(status, StatusCode::UNAUTHORIZED, "без токена");

    let (status, _) = stand.get_with("/api/me", "не-токен-вовсе").await;
    assert_eq!(status, StatusCode::UNAUTHORIZED, "чужой токен");

    // Роль поднимают в базе - и тот же токен уже отвечает иначе.
    let pool = db::pool(&stand.url).expect("пул");
    let client = pool.get().await.expect("соединение");
    client
        .execute(
            &format!("UPDATE {}.users SET role = 'admin'", stand.schema),
            &[],
        )
        .await
        .expect("роль поднята");
    let (_, body) = stand.get_with("/api/me", &access).await;
    assert_eq!(
        body["role"], "admin",
        "роль читается из базы на каждый запрос"
    );
    stand.drop_schema().await;
}

#[tokio::test]
async fn rate_window_stops_the_flood_and_names_the_wait() {
    // Стенд с узким окном: у прочих проверок оно широкое, чтобы не мешать.
    let Some(stand) = Stand::open_with("rate", |config: &mut Config| config.rate_limit = 2).await
    else {
        return skipped("окно частоты");
    };

    for attempt in 1..=2 {
        let (status, _) = stand
            .post(
                "/api/token",
                serde_json::json!({"grant_type": "password", "login": "нет", "password": "нет-нет-нет"}),
            )
            .await;
        assert_eq!(status, StatusCode::BAD_REQUEST, "попытка {attempt} в окне");
    }
    let (status, body) = stand
        .post(
            "/api/token",
            serde_json::json!({"grant_type": "password", "login": "нет", "password": "нет-нет-нет"}),
        )
        .await;
    assert_eq!(status, StatusCode::TOO_MANY_REQUESTS, "{body}");
    assert_eq!(body["error"], "too_many_requests");
    assert!(
        body["message"].as_str().expect("текст").contains("с"),
        "отказ обязан назвать, сколько ждать: {body}"
    );
    stand.drop_schema().await;
}

#[tokio::test]
async fn unknown_grant_type_answers_like_a_wrong_password() {
    let Some(stand) = Stand::open("grant").await else {
        return skipped("вид выдачи");
    };
    let (status, body) = stand
        .post("/api/token", serde_json::json!({"grant_type": "магия"}))
        .await;
    assert_eq!(status, StatusCode::BAD_REQUEST);
    assert_eq!(
        body["error"], "invalid_grant",
        "виды выдачи не перечисляются"
    );
    stand.drop_schema().await;
}

#[tokio::test]
async fn every_listed_route_answers_something() {
    // Список маршрутов объявлен в коде и проверяется здесь: маршрут, заведённый мимо
    // списка, появился бы у сервиса, не появившись в его описании, - и о нём не узнал
    // бы никто, кроме автора.
    let Some(stand) = Stand::open("routes").await else {
        return skipped("список маршрутов");
    };
    for (method, path) in takt_web_server::routes::ROUTES {
        let request = Request::builder()
            .method(*method)
            .uri(*path)
            .header("content-type", "application/json")
            .body(Body::from("{}"))
            .expect("запрос");
        let (status, body) = stand.call(request).await;
        // Одного `404` мало, чтобы объявить маршрут незнакомым: с c ручки проектов
        // честно отвечают `404` на несуществующий проект, и литеральный `{id}` - как
        // раз такой. Знакомый маршрут узнаётся по ответу API: у него есть машинный код
        // ошибки, а промах роутера уходит в статику и никакого кода не несёт.
        let answered = status != StatusCode::NOT_FOUND || body["error"].is_string();
        assert!(
            answered,
            "{method} {path} перечислен, а сервис его не знает: {status} {body}"
        );
    }
    // И обратное: путь мимо API уходит в статику, а не отвечает от неё.
    let (status, _) = stand
        .call(
            Request::get("/api/такого-нет")
                .body(Body::empty())
                .expect("запрос"),
        )
        .await;
    assert_eq!(status, StatusCode::NOT_FOUND, "чужая ручка API");
    stand.drop_schema().await;
}

/// Под префиксом работают оба адреса корня - и с косой, и без.
///
/// Предмет - не косметика: на `<префикс>/` ведёт наш же редирект в nginx, и именно этот
/// адрес открывает читатель. `nest` такой адрес не сопоставляет - запрос до вложенного
/// роутера не доходит вовсе и получает 404 снаружи.
#[tokio::test]
async fn both_forms_of_the_prefixed_root_are_served() {
    // Проверке нужна собранная статика: страница читается с диска, и без неё обе формы
    // корня одинаково дали бы 404 - то есть тест молчал бы и при регрессе. Путь
    // задаётся `TAKT_WEB_TEST_STATIC`, как у проверок архива.
    let Ok(static_dir) = std::env::var("TAKT_WEB_TEST_STATIC") else {
        eprintln!("пропуск (страница под префиксом): не задан TAKT_WEB_TEST_STATIC");
        return;
    };
    let Some(stand) = Stand::open_with("prefix", |config| {
        config.base_path = "/takt".to_string();
        config.static_dir = static_dir.into();
    })
    .await
    else {
        return skipped("страница под префиксом");
    };

    for path in ["/takt", "/takt/", "/takt/health"] {
        let (status, _) = stand.get(path).await;
        assert_eq!(status, StatusCode::OK, "адрес '{path}' под префиксом");
    }
    // Промах по файлу обязан остаться промахом: страницу вместо файла отдавать нельзя -
    // вкладка откроется без стилей и без модуля.
    let (status, _) = stand.get("/takt/b/нет-такого.css").await;
    assert_eq!(status, StatusCode::NOT_FOUND, "промах по файлу — промах");
    // И корень домена сервису не принадлежит: под префиксом он там не отвечает.
    let (status, _) = stand.get("/").await;
    assert_eq!(status, StatusCode::NOT_FOUND, "корень домена — не наш");

    stand.drop_schema().await;
}

#[tokio::test]
async fn user_row_holds_nothing_personal() {
    // Предмет проверки - обещание, а не удобство: почты нет потому, что восстановление
    // пароля идёт сбросом администратора, а адреса нет потому, что сервис обещал его не
    // хранить. Колонка, заведённая "на будущее", - персональные данные, которых никто не
    // собирался собирать.
    let Some(stand) = Stand::open("schema").await else {
        return skipped("схема пользователей");
    };
    let pool = db::pool(&stand.url).expect("пул");
    let client = pool.get().await.expect("соединение");
    let rows = client
        .query(
            "SELECT column_name FROM information_schema.columns
             WHERE table_schema = $1 AND table_name = 'users' ORDER BY column_name",
            &[&stand.schema],
        )
        .await
        .expect("колонки");
    let have: Vec<String> = rows.iter().map(|row| row.get(0)).collect();
    assert_eq!(
        have,
        vec!["created_at", "id", "login", "pass_hash", "role"],
        "состав колонок изменился"
    );
    for forbidden in [
        "email",
        "mail",
        "phone",
        "name",
        "ip",
        "address",
        "user_agent",
        "last_seen_at",
        "timezone",
    ] {
        assert!(
            !have.iter().any(|c| c == forbidden),
            "в схеме появилось '{forbidden}'"
        );
    }
    stand.drop_schema().await;
}

#[tokio::test]
async fn tokens_follow_the_user_they_belong_to() {
    // Связи в схеме есть, и каскад обязан работать: иначе токены удалённого
    // пользователя остались бы живыми ключами в базе.
    let Some(stand) = Stand::open("cascade").await else {
        return skipped("каскадное удаление");
    };
    stand
        .post(
            "/api/register",
            serde_json::json!({"login": "ivan", "password": "пароль-пароль"}),
        )
        .await;
    let pool = db::pool(&stand.scoped()).expect("пул");
    let client = pool.get().await.expect("соединение");
    let before: i64 = client
        .query_one("SELECT count(*) FROM refresh_tokens", &[])
        .await
        .expect("счёт")
        .get(0);
    assert_eq!(before, 1, "вход завёл токен");
    client
        .execute("DELETE FROM users", &[])
        .await
        .expect("удаление");
    let after: i64 = client
        .query_one("SELECT count(*) FROM refresh_tokens", &[])
        .await
        .expect("счёт")
        .get(0);
    assert_eq!(after, 0, "токены не ушли вслед за пользователем");
    stand.drop_schema().await;
}

#[tokio::test]
async fn foreign_schema_version_is_refused_by_name() {
    // База новее сервера - переходить некуда: вперёд знает только тот сервер, который
    // её и создал. Молчаливое продолжение работы означало бы, что старый код правит
    // таблицы, о половине которых не знает.
    let Some(stand) = Stand::open("version").await else {
        return skipped("версия схемы");
    };
    let pool = db::pool(&stand.scoped()).expect("пул");
    let mut client = pool.get().await.expect("соединение");
    client
        .execute("UPDATE schema_version SET version = 99", &[])
        .await
        .expect("версия подменена");
    let error = db::prepare(&mut client).await.expect_err("чужая версия");
    let text = error.to_string();
    assert!(text.contains("99"), "{text}");
    assert!(text.contains(&db::SCHEMA_VERSION.to_string()), "{text}");
    stand.drop_schema().await;
}

/// Обе дороги к схеме дают одно И то же (главный тест перехода).
///
/// `SCHEMA` и шаги перехода - два носителя одного знания. Разъедься они, база, поднятая
/// с нуля, и база, прошедшая переход, окажутся разными при одинаковом номере версии - и
/// расхождение придёт не отказом, а отвергнутой записью у читателя стенда.
///
/// Проверка сравнивает состав колонок (имя, тип, обязательность, умолчание) и текст
/// ограничений `CHECK`: именно ими отличались версии 5 и 6.
#[tokio::test]
async fn both_roads_lead_to_the_same_schema() {
    let Some(fresh) = Stand::open("schema_fresh").await else {
        return skipped("сверка дорог к схеме");
    };
    // Вторая база строится как старая: схема версии 4 - это нынешняя без того, что
    // принесли шаги. Разворачиваем её обратно и заявляем версию 4, после чего `prepare`
    // обязан привести её к текущей.
    let Some(aged) = Stand::open("schema_aged").await else {
        return skipped("сверка дорог к схеме");
    };
    let pool = db::pool(&aged.scoped()).expect("пул");
    let mut client = pool.get().await.expect("соединение");
    client
        .batch_execute(
            "ALTER TABLE projects DROP COLUMN build_target;
             ALTER TABLE projects DROP COLUMN build_args;
             ALTER TABLE projects DROP COLUMN main_scenario;
             ALTER TABLE projects DROP COLUMN run_delays;
             ALTER TABLE project_files DROP CONSTRAINT project_files_kind_check;
             ALTER TABLE project_files
                 ADD CONSTRAINT project_files_kind_check
                 CHECK (kind IN ('takt', 'scenario'));
             UPDATE schema_version SET version = 4;",
        )
        .await
        .expect("схема состарена до версии 4");
    db::prepare(&mut client).await.expect("переход 4 → текущая");

    let version: i64 = client
        .query_one("SELECT version FROM schema_version", &[])
        .await
        .expect("версия")
        .get(0);
    assert_eq!(version, db::SCHEMA_VERSION, "переход не довёл до текущей");

    assert_eq!(
        columns(&fresh).await,
        columns(&aged).await,
        "состав колонок разошёлся: SCHEMA и шаги перехода описывают РАЗНОЕ"
    );
    assert_eq!(
        checks(&fresh).await,
        checks(&aged).await,
        "ограничения CHECK разошлись"
    );
    fresh.drop_schema().await;
    aged.drop_schema().await;
}

/// Состав колонок схемы стенда: таблица, колонка, тип, обязательность, умолчание.
async fn columns(stand: &Stand) -> Vec<(String, String, String, String, String)> {
    let pool = db::pool(&stand.scoped()).expect("пул");
    let client = pool.get().await.expect("соединение");
    client
        .query(
            "SELECT table_name, column_name, data_type, is_nullable,
                    coalesce(column_default, '')
             FROM information_schema.columns
             WHERE table_schema = current_schema()
             ORDER BY table_name, column_name",
            &[],
        )
        .await
        .expect("колонки")
        .iter()
        .map(|r| (r.get(0), r.get(1), r.get(2), r.get(3), r.get(4)))
        .collect()
}

/// Тексты ограничений `CHECK` схемы стенда.
async fn checks(stand: &Stand) -> Vec<String> {
    let pool = db::pool(&stand.scoped()).expect("пул");
    let client = pool.get().await.expect("соединение");
    let mut out: Vec<String> = client
        .query(
            "SELECT pg_get_constraintdef(c.oid)
             FROM pg_constraint c
             JOIN pg_namespace n ON n.oid = c.connamespace
             WHERE n.nspname = current_schema() AND c.contype = 'c'",
            &[],
        )
        .await
        .expect("ограничения")
        .iter()
        .map(|r| r.get::<_, String>(0))
        .collect();
    out.sort();
    out
}

/// Версия, до которой шага перехода нет, отвергается - и называет обе.
///
/// Контроль к переходу: без него "переход есть" доказывалось бы только удачными
/// случаями, а база из будущего прошлого (версия 1, шага с которой никто не писал)
/// молча осталась бы неприведённой.
#[tokio::test]
async fn version_without_a_step_is_refused_by_name() {
    let Some(stand) = Stand::open("nostep").await else {
        return skipped("версия без шага");
    };
    let pool = db::pool(&stand.scoped()).expect("пул");
    let mut client = pool.get().await.expect("соединение");
    client
        .execute("UPDATE schema_version SET version = 1", &[])
        .await
        .expect("версия подменена");
    let error = db::prepare(&mut client)
        .await
        .expect_err("шага перехода нет");
    let text = error.to_string();
    assert!(text.contains('1'), "{text}");
    assert!(text.contains(&db::SCHEMA_VERSION.to_string()), "{text}");
    stand.drop_schema().await;
}

#[tokio::test]
async fn password_change_kills_live_sessions() {
    let Some(stand) = Stand::open("passwd").await else {
        return skipped("смена пароля");
    };
    let (_, registered) = stand
        .post(
            "/api/register",
            serde_json::json!({"login": "ivan", "password": "пароль-пароль"}),
        )
        .await;
    let token = registered["refresh_token"]
        .as_str()
        .expect("токен")
        .to_string();

    let pool = db::pool(&stand.scoped()).expect("пул");
    let client = pool.get().await.expect("соединение");
    auth::set_password(&client, "ivan", "новый-пароль")
        .await
        .expect("смена");

    let (status, _) = stand
        .post(
            "/api/token",
            serde_json::json!({"grant_type": "refresh_token", "refresh_token": token}),
        )
        .await;
    assert_eq!(
        status,
        StatusCode::BAD_REQUEST,
        "смена пароля обязана выгонять того, ради кого её делают"
    );
    let (status, _) = stand
        .post(
            "/api/token",
            serde_json::json!({"grant_type": "password", "login": "ivan", "password": "новый-пароль"}),
        )
        .await;
    assert_eq!(status, StatusCode::OK, "новый пароль работает");
    stand.drop_schema().await;
}
