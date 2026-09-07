//! Проверки архива проекта.
//!
//! Политика та же, что у прочих наборов: нет базы - проверки не выполняются и говорят
//! об этом словами.
//!
//! Предмет - **круговой рейс**: выгрузить, загрузить, сравнить. "Архив собрался" не
//! доказывает ничего: испорченный архив тоже собирается, и узнаёт об этом тот, кто
//! попробовал его прочитать - то есть автор, у которого другой копии уже нет.

mod common;

use std::io::Read as _;

use axum::body::Body;
use axum::http::{Request, StatusCode};
use common::{Stand, skipped};

/// Заводит человека и возвращает его access-токен.
async fn person(stand: &Stand, login: &str) -> String {
    let (status, body) = stand
        .post(
            "/api/register",
            serde_json::json!({"login": login, "password": "пароль-пароль"}),
        )
        .await;
    assert_eq!(status, StatusCode::CREATED, "{body}");
    body["access_token"].as_str().expect("токен").to_string()
}

/// Создаёт проект с моделью и сценарием.
async fn project(stand: &Stand, token: &str, name: &str) -> String {
    let (status, body) = stand
        .post_as(
            "/api/projects",
            token,
            serde_json::json!({"name": name, "description": "проба выгрузки"}),
        )
        .await;
    assert_eq!(status, StatusCode::CREATED, "{body}");
    let id = body["id"].as_str().expect("идентификатор").to_string();
    let (status, body) = stand
        .put_as(
            &format!("/api/projects/{id}/files/model.takt"),
            token,
            serde_json::json!({"text": "var level: u8 := 0;\n\nstart Run {\n}\n"}),
        )
        .await;
    assert_eq!(status, StatusCode::OK, "{body}");
    let (status, body) = stand
        .put_as(
            &format!("/api/projects/{id}/files/run.json"),
            token,
            serde_json::json!({"text": "[]", "revision": 1}),
        )
        .await;
    assert_eq!(status, StatusCode::OK, "{body}");
    let (status, body) = stand
        .patch_as(
            &format!("/api/projects/{id}"),
            token,
            serde_json::json!({"main_file": "model.takt"}),
        )
        .await;
    assert_eq!(status, StatusCode::OK, "{body}");
    id
}

/// Читает состав архива: имя -> содержимое.
fn entries(bytes: &[u8]) -> std::collections::BTreeMap<String, String> {
    let mut zip = zip::ZipArchive::new(std::io::Cursor::new(bytes)).expect("это архив");
    let mut out = std::collections::BTreeMap::new();
    for index in 0..zip.len() {
        let mut entry = zip.by_index(index).expect("запись");
        let name = entry.name().to_string();
        let mut text = String::new();
        entry.read_to_string(&mut text).expect("текст");
        out.insert(name, text);
    }
    out
}

#[tokio::test]
async fn the_archive_makes_a_round_trip_through_the_service() {
    let Some(stand) = Stand::open("a_round").await else {
        return skipped("круговой рейс архива");
    };
    let author = person(&stand, "ivan").await;
    let id = project(&stand, &author, "Термореле").await;

    let (status, bytes) = stand
        .bytes(&format!("/api/projects/{id}/archive"), Some(&author))
        .await;
    assert_eq!(status, StatusCode::OK);
    let files = entries(&bytes);
    assert!(
        files.contains_key("takt-project.json"),
        "нет метаданных: {files:?}"
    );
    assert!(files.contains_key("src/model.takt"));
    assert!(files.contains_key("src/run.json"), "сценарий — исходник");
    // Без просьбы о цели вывода в архиве нет: он воспроизводим и весит на порядок
    // больше исходника.
    assert!(
        files.keys().all(|name| !name.starts_with("generated/")),
        "вывод без просьбы: {files:?}"
    );
    let manifest: serde_json::Value =
        serde_json::from_str(&files["takt-project.json"]).expect("метаданные");
    assert_eq!(manifest["format"], 3, "версия формата названа");
    assert_eq!(manifest["name"], "Термореле");
    // Версия сверяется с той, что объявил стенд, а не с числом в тесте: вписанное число
    // отставало бы при каждом подъёме версии крейта, и проверка судила бы вчерашнее.
    assert_eq!(
        manifest["takt_lang"], stand.module_version,
        "версия модуля — часть архива"
    );
    assert_eq!(manifest["main_file"], "model.takt");
    // Свойств места в архиве нет: восстановив их у себя, автор получил бы чужие права
    // на своей стороне.
    for absent in ["visibility", "owner", "forks", "grants"] {
        assert!(
            manifest.get(absent).is_none(),
            "в метаданных есть '{absent}'"
        );
    }

    // Загрузка тем же человеком заводит второй проект: перезапись поверх существующего
    // означала бы молчаливую потерю работы.
    let (status, created) = stand.upload("/api/projects/import", &author, &bytes).await;
    assert_eq!(status, StatusCode::CREATED, "{created}");
    let copy = created["id"].as_str().expect("идентификатор").to_string();
    assert_ne!(copy, id);
    assert_eq!(created["name"], "Термореле");
    assert_eq!(created["description"], "проба выгрузки");
    assert_eq!(
        created["takt_lang"], stand.module_version,
        "версия модуля пережила рейс"
    );
    assert_eq!(created["main_file"], "model.takt");
    // Загруженный проект закрыт: видимость - свойство места.
    assert_eq!(created["visibility"], "private");

    // Состав и тексты совпадают с исходными.
    let (_, read) = stand
        .get_as(&format!("/api/projects/{copy}"), &author)
        .await;
    let names: Vec<String> = read["files"]
        .as_array()
        .expect("список")
        .iter()
        .map(|file| file["name"].as_str().expect("имя").to_string())
        .collect();
    assert_eq!(names, vec!["model.takt", "run.json"]);
    let (_, file) = stand
        .get_as(&format!("/api/projects/{copy}/files/model.takt"), &author)
        .await;
    assert_eq!(file["text"], "var level: u8 := 0;\n\nstart Run {\n}\n");

    stand.drop_schema().await;
}

#[tokio::test]
async fn a_broken_archive_is_refused_by_words() {
    let Some(stand) = Stand::open("a_broken").await else {
        return skipped("испорченный архив");
    };
    let author = person(&stand, "ivan").await;

    // Не архив вовсе.
    let (status, body) = stand
        .upload("/api/projects/import", &author, "это не zip".as_bytes())
        .await;
    assert_eq!(status, StatusCode::BAD_REQUEST, "{body}");
    assert_eq!(body["error"], "bad_request");

    // Архив без метаданных: без них проект не восстановить, и половина восстановленного
    // хуже отказа.
    let mut buffer = std::io::Cursor::new(Vec::new());
    {
        use std::io::Write as _;
        let mut zip = zip::ZipWriter::new(&mut buffer);
        let options: zip::write::FileOptions<'_, ()> = zip::write::FileOptions::default();
        zip.start_file("src/model.takt", options).expect("файл");
        zip.write_all(b"model A {}").expect("запись");
        zip.finish().expect("конец");
    }
    let (status, body) = stand
        .upload("/api/projects/import", &author, &buffer.into_inner())
        .await;
    assert_eq!(status, StatusCode::BAD_REQUEST, "{body}");
    assert!(
        body["message"]
            .as_str()
            .expect("текст")
            .contains("takt-project.json"),
        "причина не названа: {body}"
    );

    // Загрузка требует входа: она заводит проект, а у проекта есть владелец.
    let (status, _) = stand
        .call(
            Request::post("/api/projects/import")
                .header("content-type", "application/zip")
                .body(Body::from(vec![0u8; 4]))
                .expect("запрос"),
        )
        .await;
    assert_eq!(status, StatusCode::UNAUTHORIZED);

    stand.drop_schema().await;
}

#[tokio::test]
async fn the_archive_follows_visibility() {
    let Some(stand) = Stand::open("a_access").await else {
        return skipped("доступ к архиву");
    };
    let author = person(&stand, "ivan").await;
    let stranger = person(&stand, "vera").await;
    let id = project(&stand, &author, "Термореле").await;

    // Закрытый чужой не существует.
    let (status, _) = stand
        .bytes(&format!("/api/projects/{id}/archive"), Some(&stranger))
        .await;
    assert_eq!(status, StatusCode::NOT_FOUND);
    let (status, _) = stand
        .bytes(&format!("/api/projects/{id}/archive"), None)
        .await;
    assert_eq!(status, StatusCode::NOT_FOUND);

    // Открытый скачивает всякий, в том числе без учётной записи: текст ему уже виден, и
    // запрет скачать его неисполним.
    let (status, _) = stand
        .patch_as(
            &format!("/api/projects/{id}"),
            &author,
            serde_json::json!({"visibility": "public"}),
        )
        .await;
    assert_eq!(status, StatusCode::OK);
    let (status, bytes) = stand
        .bytes(&format!("/api/projects/{id}/archive"), None)
        .await;
    assert_eq!(status, StatusCode::OK);
    assert!(entries(&bytes).contains_key("src/model.takt"));

    stand.drop_schema().await;
}

#[tokio::test]
async fn generation_goes_into_the_archive_when_asked() {
    let Some(stand) = Stand::open("a_generated").await else {
        return skipped("генерация в архиве");
    };
    if std::env::var("TAKT_WEB_TEST_STATIC").is_err() {
        // Пропуск называется: вывод целей собирает модуль `takt-wasm` из собранной
        // статики, и без неё проверять нечего. Молча пропущенная проверка неотличима от
        // прошедшей.
        eprintln!("пропуск (генерация в архиве): не задан TAKT_WEB_TEST_STATIC — статики нет");
        stand.drop_schema().await;
        return;
    }
    let author = person(&stand, "ivan").await;
    let id = project(&stand, &author, "Термореле").await;

    let (status, bytes) = stand
        .bytes(
            &format!("/api/projects/{id}/archive?target=c"),
            Some(&author),
        )
        .await;
    assert_eq!(status, StatusCode::OK);
    let files = entries(&bytes);
    assert!(
        files.keys().any(|name| name.starts_with("generated/")),
        "вывода цели нет: {:?}",
        files.keys().collect::<Vec<_>>()
    );
    // Имя корневой модели берётся из имени файла, и это видно в выводе.
    assert!(
        files.contains_key("generated/model.h") || files.contains_key("generated/model.c"),
        "вывод назван не по файлу: {:?}",
        files.keys().collect::<Vec<_>>()
    );
    let manifest: serde_json::Value =
        serde_json::from_str(&files["takt-project.json"]).expect("метаданные");
    assert_eq!(
        manifest["generated_target"], "c",
        "цель названа в метаданных"
    );

    // Загрузка вывод игнорирует: он воспроизводим и в проекте не хранится.
    let (status, created) = stand.upload("/api/projects/import", &author, &bytes).await;
    assert_eq!(status, StatusCode::CREATED, "{created}");
    let copy = created["id"].as_str().expect("идентификатор");
    let (_, read) = stand
        .get_as(&format!("/api/projects/{copy}"), &author)
        .await;
    let names: Vec<String> = read["files"]
        .as_array()
        .expect("список")
        .iter()
        .map(|file| file["name"].as_str().expect("имя").to_string())
        .collect();
    assert_eq!(
        names,
        vec!["model.takt", "run.json"],
        "вывод стал исходником"
    );

    stand.drop_schema().await;
}

#[tokio::test]
async fn a_target_that_refuses_says_so_in_the_archive() {
    let Some(stand) = Stand::open("a_refusal").await else {
        return skipped("отказ цели в архиве");
    };
    if std::env::var("TAKT_WEB_TEST_STATIC").is_err() {
        eprintln!("пропуск (отказ цели): не задан TAKT_WEB_TEST_STATIC — статики нет");
        stand.drop_schema().await;
        return;
    }
    let author = person(&stand, "ivan").await;
    let id = project(&stand, &author, "Термореле").await;
    // Вещественный тип цель `sv` не переводит - и это нормальный ответ.
    let (status, _) = stand
        .put_as(
            &format!("/api/projects/{id}/files/model.takt"),
            &author,
            serde_json::json!({
                // Переменная обязана использоваться: неиспользуемую цель до вывода не
                // доводит, и отказа не будет вовсе.
                "text": "var x: float := 1.0;\n\nstart Run {\n    always {\n        x := x + 1.0;\n    }\n}\n",
                "revision": 2
            }),
        )
        .await;
    assert_eq!(status, StatusCode::OK);

    let (status, bytes) = stand
        .bytes(
            &format!("/api/projects/{id}/archive?target=sv"),
            Some(&author),
        )
        .await;
    // Ответ `200`: отказ цели - не ошибка сервиса. Причина едет В архиве словами,
    // потому что молча пропущенный вывод неотличим от "цель ничего не печатает".
    assert_eq!(status, StatusCode::OK);
    let files = entries(&bytes);
    let reason = files
        .get("generated/REFUSAL.txt")
        .expect("отказ не записан в архив");
    assert!(reason.contains("SV-"), "причина без кода: {reason}");
    assert!(files.contains_key("src/model.takt"), "исходники на месте");

    stand.drop_schema().await;
}

#[tokio::test]
async fn the_build_target_and_flags_survive_the_round_trip() {
    // Пара проверяется модулем и при загрузке: без статики загрузка берёт умолчания, и
    // проверка судила бы не то.
    if std::env::var("TAKT_WEB_TEST_STATIC").is_err() {
        return skipped("цель и ключи в архиве: не задан TAKT_WEB_TEST_STATIC");
    }
    let Some(stand) = Stand::open("a_build").await else {
        return skipped("цель и ключи в архиве");
    };
    let author = person(&stand, "ivan").await;
    let id = project(&stand, &author, "Термореле").await;
    let (status, body) = stand
        .patch_as(
            &format!("/api/projects/{id}"),
            &author,
            serde_json::json!({"build_target": "sv-mmio", "build_args": "--bus=apb"}),
        )
        .await;
    assert_eq!(status, StatusCode::OK, "{body}");

    let (status, bytes) = stand
        .bytes(&format!("/api/projects/{id}/archive"), Some(&author))
        .await;
    assert_eq!(status, StatusCode::OK);
    let files = entries(&bytes);
    let manifest: serde_json::Value =
        serde_json::from_str(&files["takt-project.json"]).expect("метаданные");
    // Пара названа непустой и не умолчанием: на `c` без ключей потеря поля неотличима
    // от подстановки умолчания.
    assert_eq!(manifest["build_target"], "sv-mmio", "{manifest}");
    assert_eq!(manifest["build_args"], "--bus=apb", "{manifest}");
    // Полей про цель два, и они значат разное: выгрузка шла без генерации, значит
    // `generated_target` пуст, а выбор автора - на месте.
    assert!(manifest["generated_target"].is_null(), "{manifest}");

    // Круговой рейс: загруженный проект открывается сборкой автора.
    let (status, created) = stand.upload("/api/projects/import", &author, &bytes).await;
    assert_eq!(status, StatusCode::CREATED, "{created}");
    assert_eq!(created["build_target"], "sv-mmio", "{created}");
    assert_eq!(created["build_args"], "--bus=apb", "{created}");

    // Архив прежней версии формата пары не несёт, и его загрузка обязана дать
    // умолчание, а не пустую цель: пустой целью не собирается ничего, и страница
    // показала бы отказ там, где автор ничего не выбирал.
    let (status, old) = stand
        .upload("/api/projects/import", &author, &without_build_pair())
        .await;
    assert_eq!(status, StatusCode::CREATED, "{old}");
    assert_eq!(old["build_target"], "c", "{old}");
    assert_eq!(old["build_args"], "", "{old}");

    stand.drop_schema().await;
}

/// Архив прежней версии формата: метаданные без цели и ключей.
fn without_build_pair() -> Vec<u8> {
    use std::io::Write as _;
    let mut buffer = std::io::Cursor::new(Vec::new());
    {
        let mut zip = zip::ZipWriter::new(&mut buffer);
        let options: zip::write::FileOptions<'_, ()> = zip::write::FileOptions::default();
        zip.start_file("takt-project.json", options).expect("файл");
        let manifest = serde_json::json!({
            "format": 1,
            "name": "Прежний",
            "takt_lang": "0.58.0",
            "main_file": "model.takt",
        });
        zip.write_all(manifest.to_string().as_bytes())
            .expect("запись");
        zip.start_file("src/model.takt", options).expect("файл");
        zip.write_all(b"start Run;\n").expect("запись");
        zip.finish().expect("конец");
    }
    buffer.into_inner()
}
