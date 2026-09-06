//! Проверки проектов и файлов.
//!
//! Политика та же, что у `http.rs`: нет базы - проверки не выполняются и говорят об
//! этом словами, а решает это проверка `check-web-server.sh`.

mod common;

use axum::http::StatusCode;
use common::{Stand, skipped};

use takt_web_server::limits;

/// Заводит владельца и возвращает его access-токен.
async fn owner(stand: &Stand, login: &str) -> String {
    let (status, body) = stand
        .post(
            "/api/register",
            serde_json::json!({"login": login, "password": "пароль-пароль"}),
        )
        .await;
    assert_eq!(status, StatusCode::CREATED, "{body}");
    body["access_token"].as_str().expect("токен").to_string()
}

/// Создаёт проект и возвращает его идентификатор.
async fn project(stand: &Stand, token: &str, name: &str) -> String {
    let (status, body) = stand
        .post_as("/api/projects", token, serde_json::json!({"name": name}))
        .await;
    assert_eq!(status, StatusCode::CREATED, "{body}");
    body["id"].as_str().expect("идентификатор").to_string()
}

#[tokio::test]
async fn project_is_created_read_patched_and_removed() {
    let Some(stand) = Stand::open("p_crud").await else {
        return skipped("жизнь проекта");
    };
    let token = owner(&stand, "ivan").await;

    let (status, created) = stand
        .post_as(
            "/api/projects",
            &token,
            serde_json::json!({"name": "Термореле", "description": "проба"}),
        )
        .await;
    assert_eq!(status, StatusCode::CREATED, "{created}");
    assert_eq!(created["name"], "Термореле");
    // Умолчание видимости - закрытый: открытым проект становится по просьбе, а не по
    // умолчанию.
    assert_eq!(created["visibility"], "private");
    assert_eq!(created["revision"], 0);
    assert_eq!(created["size_bytes"], 0);
    let id = created["id"].as_str().expect("идентификатор").to_string();

    let (status, read) = stand.get_as(&format!("/api/projects/{id}"), &token).await;
    assert_eq!(status, StatusCode::OK, "{read}");
    assert_eq!(read["files"].as_array().expect("список").len(), 0);

    let (status, patched) = stand
        .patch_as(
            &format!("/api/projects/{id}"),
            &token,
            serde_json::json!({"name": "Термореле 2", "description": "иначе"}),
        )
        .await;
    assert_eq!(status, StatusCode::OK, "{patched}");
    assert_eq!(patched["name"], "Термореле 2");
    assert_eq!(patched["description"], "иначе");

    let (status, list) = stand.get_as("/api/projects", &token).await;
    assert_eq!(status, StatusCode::OK);
    assert_eq!(list.as_array().expect("список").len(), 1);

    let (status, _) = stand
        .delete_as(&format!("/api/projects/{id}"), &token)
        .await;
    assert_eq!(status, StatusCode::NO_CONTENT);
    let (status, _) = stand.get_as(&format!("/api/projects/{id}"), &token).await;
    assert_eq!(status, StatusCode::NOT_FOUND, "удалённый проект");
    stand.drop_schema().await;
}

#[tokio::test]
async fn a_stranger_project_is_not_found_rather_than_forbidden() {
    // `403` сделал бы ручку оракулом: по ответам перечислялись бы чужие проекты,
    // которых спрашивающий не видел.
    let Some(stand) = Stand::open("p_stranger").await else {
        return skipped("чужой проект");
    };
    let mine = owner(&stand, "ivan").await;
    let theirs = owner(&stand, "petr").await;
    let id = project(&stand, &mine, "Моё").await;

    for (method, status) in [
        ("get", StatusCode::NOT_FOUND),
        ("delete", StatusCode::NOT_FOUND),
    ] {
        let (got, _) = match method {
            "get" => stand.get_as(&format!("/api/projects/{id}"), &theirs).await,
            _ => {
                stand
                    .delete_as(&format!("/api/projects/{id}"), &theirs)
                    .await
            }
        };
        assert_eq!(got, status, "{method} чужого проекта");
    }
    let (status, _) = stand
        .patch_as(
            &format!("/api/projects/{id}"),
            &theirs,
            serde_json::json!({"name": "Чужое"}),
        )
        .await;
    assert_eq!(status, StatusCode::NOT_FOUND, "правка чужого проекта");
    // И список чужого не показывает.
    let (_, list) = stand.get_as("/api/projects", &theirs).await;
    assert_eq!(list.as_array().expect("список").len(), 0);
    stand.drop_schema().await;
}

#[tokio::test]
async fn file_write_read_and_delete_keep_the_size_in_step() {
    // Размер считается суммой по файлам, а не приращением: приращение расходится с
    // истиной на первой же неудачной попытке, и молча.
    let Some(stand) = Stand::open("p_size").await else {
        return skipped("размер проекта");
    };
    let token = owner(&stand, "ivan").await;
    let id = project(&stand, &token, "Проект").await;

    let first = "start Run {}\n";
    let (status, written) = stand
        .put_as(
            &format!("/api/projects/{id}/files/main.takt"),
            &token,
            serde_json::json!({"text": first}),
        )
        .await;
    assert_eq!(status, StatusCode::OK, "{written}");
    assert_eq!(written["revision"], 1, "ревизия поднялась");
    assert_eq!(written["size_bytes"], first.len(), "размер равен файлу");

    let second = "[{\"in_ports\": {\"x\": 1}}]\n";
    let (_, written) = stand
        .put_as(
            &format!("/api/projects/{id}/files/run.json"),
            &token,
            serde_json::json!({"text": second, "revision": 1}),
        )
        .await;
    assert_eq!(written["revision"], 2);
    assert_eq!(
        written["size_bytes"],
        first.len() + second.len(),
        "размер равен сумме"
    );

    let (status, file) = stand
        .get_as(&format!("/api/projects/{id}/files/run.json"), &token)
        .await;
    assert_eq!(status, StatusCode::OK, "{file}");
    assert_eq!(file["text"], second);
    assert_eq!(file["kind"], "scenario", "вид определён расширением");
    assert_eq!(file["revision"], 2, "ревизия ПРОЕКТА, её и шлют обратно");

    // Правка вдвое короче - сумма обязана уменьшиться.
    let shorter = "x\n";
    let (_, written) = stand
        .put_as(
            &format!("/api/projects/{id}/files/main.takt"),
            &token,
            serde_json::json!({"text": shorter, "revision": 2}),
        )
        .await;
    assert_eq!(written["size_bytes"], shorter.len() + second.len());

    let (status, written) = stand
        .delete_as(&format!("/api/projects/{id}/files/run.json"), &token)
        .await;
    assert_eq!(status, StatusCode::OK);
    assert_eq!(
        written["size_bytes"],
        shorter.len(),
        "удаление уменьшило сумму"
    );
    let (_, read) = stand.get_as(&format!("/api/projects/{id}"), &token).await;
    assert_eq!(read["size_bytes"], shorter.len(), "у проекта тот же размер");
    stand.drop_schema().await;
}

#[tokio::test]
async fn stale_revision_is_a_conflict_and_names_both_numbers() {
    let Some(stand) = Stand::open("p_revision").await else {
        return skipped("ревизия");
    };
    let token = owner(&stand, "ivan").await;
    let id = project(&stand, &token, "Проект").await;
    stand
        .put_as(
            &format!("/api/projects/{id}/files/main.takt"),
            &token,
            serde_json::json!({"text": "первое"}),
        )
        .await;
    stand
        .put_as(
            &format!("/api/projects/{id}/files/main.takt"),
            &token,
            serde_json::json!({"text": "второе", "revision": 1}),
        )
        .await;

    // Вторая вкладка видела ревизию 1 и о второй правке не знает.
    let (status, body) = stand
        .put_as(
            &format!("/api/projects/{id}/files/main.takt"),
            &token,
            serde_json::json!({"text": "третье", "revision": 1}),
        )
        .await;
    assert_eq!(status, StatusCode::CONFLICT, "{body}");
    assert_eq!(body["error"], "revision_conflict");
    // Числа едут полями, а не только в тексте: по ним страница строит выбор "перечитать
    // / перезаписать". Разбирай она их из сообщения - текст отказа стал бы частью
    // протокола и перестал бы переводиться.
    assert_eq!(body["seen"], 1, "названа ревизия автора");
    assert_eq!(body["revision"], 2, "названа ревизия проекта");
    let message = body["message"].as_str().expect("текст");
    assert!(message.contains('1') && message.contains('2'), "{message}");

    // Правка существующего файла без ревизии - тоже конфликт: молчаливая перезапись
    // чужой работы хуже отказа.
    let (status, body) = stand
        .put_as(
            &format!("/api/projects/{id}/files/main.takt"),
            &token,
            serde_json::json!({"text": "без ревизии"}),
        )
        .await;
    assert_eq!(status, StatusCode::CONFLICT, "{body}");

    let (_, file) = stand
        .get_as(&format!("/api/projects/{id}/files/main.takt"), &token)
        .await;
    assert_eq!(file["text"], "второе", "конфликт ничего не записал");
    stand.drop_schema().await;
}

#[tokio::test]
async fn every_limit_refuses_with_its_number() {
    // Условие приёмки задачи: `413` на каждом пределе, и в тексте обязаны быть и
    // предел, и факт.
    let Some(stand) = Stand::open("p_limits").await else {
        return skipped("пределы");
    };
    let token = owner(&stand, "ivan").await;
    let id = project(&stand, &token, "Проект").await;

    // Предел файла.
    let big = "x".repeat(limits::FILE_BYTES + 1);
    let (status, body) = stand
        .put_as(
            &format!("/api/projects/{id}/files/big.takt"),
            &token,
            serde_json::json!({"text": big}),
        )
        .await;
    assert_eq!(status, StatusCode::PAYLOAD_TOO_LARGE, "{body}");
    assert_eq!(body["error"], "limit_exceeded");
    let message = body["message"].as_str().expect("текст");
    assert!(
        message.contains(&limits::FILE_BYTES.to_string()),
        "{message}"
    );
    assert!(
        message.contains(&(limits::FILE_BYTES + 1).to_string()),
        "факт не назван: {message}"
    );

    // Предел размера проекта: восемь файлов по 64 КиБ - ровно предел, девятый не
    // влезает.
    let chunk = "y".repeat(limits::FILE_BYTES);
    let mut revision = 0i64;
    for index in 0..(limits::PROJECT_BYTES / limits::FILE_BYTES as i64) {
        let (status, body) = stand
            .put_as(
                &format!("/api/projects/{id}/files/f{index}.takt"),
                &token,
                serde_json::json!({"text": chunk, "revision": revision}),
            )
            .await;
        assert_eq!(status, StatusCode::OK, "файл {index}: {body}");
        revision = body["revision"].as_i64().expect("ревизия");
    }
    let (status, body) = stand
        .put_as(
            &format!("/api/projects/{id}/files/over.takt"),
            &token,
            serde_json::json!({"text": "ещё", "revision": revision}),
        )
        .await;
    assert_eq!(status, StatusCode::PAYLOAD_TOO_LARGE, "{body}");
    assert!(
        body["message"]
            .as_str()
            .expect("текст")
            .contains(&limits::PROJECT_BYTES.to_string()),
        "{body}"
    );
    stand.drop_schema().await;
}

#[tokio::test]
async fn the_number_of_files_is_limited_too() {
    let Some(stand) = Stand::open("p_files").await else {
        return skipped("число файлов");
    };
    let token = owner(&stand, "ivan").await;
    let id = project(&stand, &token, "Проект").await;
    let mut revision = 0i64;
    for index in 0..limits::FILES_PER_PROJECT {
        let (status, body) = stand
            .put_as(
                &format!("/api/projects/{id}/files/f{index}.takt"),
                &token,
                serde_json::json!({"text": "x", "revision": revision}),
            )
            .await;
        assert_eq!(status, StatusCode::OK, "файл {index}: {body}");
        revision = body["revision"].as_i64().expect("ревизия");
    }
    let (status, body) = stand
        .put_as(
            &format!("/api/projects/{id}/files/over.takt"),
            &token,
            serde_json::json!({"text": "x", "revision": revision}),
        )
        .await;
    assert_eq!(status, StatusCode::PAYLOAD_TOO_LARGE, "{body}");
    assert!(
        body["message"]
            .as_str()
            .expect("текст")
            .contains(&limits::FILES_PER_PROJECT.to_string()),
        "{body}"
    );
    // Правка существующего файла на пределе обязана проходить: предел на число файлов,
    // а не на число записей.
    let (status, body) = stand
        .put_as(
            &format!("/api/projects/{id}/files/f0.takt"),
            &token,
            serde_json::json!({"text": "yy", "revision": revision}),
        )
        .await;
    assert_eq!(status, StatusCode::OK, "{body}");
    stand.drop_schema().await;
}

#[tokio::test]
async fn file_name_is_checked_because_it_becomes_a_model_name() {
    // Имя файла становится именем корневой модели, а она попадает в порождённый код:
    // кириллица и пробел не пройдут дальше первой цели.
    let Some(stand) = Stand::open("p_names").await else {
        return skipped("имя файла");
    };
    let token = owner(&stand, "ivan").await;
    let id = project(&stand, &token, "Проект").await;
    for name in ["модель.takt", "два%20слова.takt", "main.c", "main"] {
        let (status, body) = stand
            .put_as(
                &format!("/api/projects/{id}/files/{name}"),
                &token,
                serde_json::json!({"text": "x"}),
            )
            .await;
        assert_eq!(status, StatusCode::BAD_REQUEST, "имя '{name}': {body}");
    }
    stand.drop_schema().await;
}

#[tokio::test]
async fn main_file_must_exist_and_is_forgotten_when_removed() {
    let Some(stand) = Stand::open("p_main").await else {
        return skipped("активный файл");
    };
    let token = owner(&stand, "ivan").await;
    let id = project(&stand, &token, "Проект").await;

    let (status, body) = stand
        .patch_as(
            &format!("/api/projects/{id}"),
            &token,
            serde_json::json!({"main_file": "main.takt"}),
        )
        .await;
    assert_eq!(status, StatusCode::BAD_REQUEST, "такого файла нет: {body}");

    stand
        .put_as(
            &format!("/api/projects/{id}/files/main.takt"),
            &token,
            serde_json::json!({"text": "start Run {}"}),
        )
        .await;
    let (_, patched) = stand
        .patch_as(
            &format!("/api/projects/{id}"),
            &token,
            serde_json::json!({"main_file": "main.takt"}),
        )
        .await;
    assert_eq!(patched["main_file"], "main.takt");

    let (_, _) = stand
        .delete_as(&format!("/api/projects/{id}/files/main.takt"), &token)
        .await;
    let (_, read) = stand.get_as(&format!("/api/projects/{id}"), &token).await;
    assert!(
        read["main_file"].is_null(),
        "активный файл, которого нет, обязан забыться: {read}"
    );
    stand.drop_schema().await;
}

#[tokio::test]
async fn a_new_project_gets_the_module_version_of_the_service() {
    // Решение A5: страница проекта грузит модуль его версии, и подъём - явное действие
    // владельца.
    let Some(stand) = Stand::open("p_version").await else {
        return skipped("версия модуля");
    };
    let token = owner(&stand, "ivan").await;
    let (_, created) = stand
        .post_as("/api/projects", &token, serde_json::json!({"name": "П"}))
        .await;
    assert_eq!(created["takt_lang"], stand.module_version, "версия сервиса");
    let id = created["id"].as_str().expect("идентификатор").to_string();

    let (_, patched) = stand
        .patch_as(
            &format!("/api/projects/{id}"),
            &token,
            serde_json::json!({"takt_lang": "0.57.0"}),
        )
        .await;
    assert_eq!(patched["takt_lang"], "0.57.0", "подъём — дело владельца");
    stand.drop_schema().await;
}

#[tokio::test]
async fn project_count_is_limited_per_owner() {
    let Some(stand) = Stand::open("p_count").await else {
        return skipped("число проектов");
    };
    let token = owner(&stand, "ivan").await;
    // Число берётся у самого предела через прямой INSERT.
    let filler = limits::PROJECTS_PER_USER - 1;
    stand.fill_projects("ivan", filler).await;
    let (status, body) = stand
        .post_as(
            "/api/projects",
            &token,
            serde_json::json!({"name": "последний"}),
        )
        .await;
    assert_eq!(status, StatusCode::CREATED, "предпоследний влезает: {body}");
    let (status, body) = stand
        .post_as(
            "/api/projects",
            &token,
            serde_json::json!({"name": "лишний"}),
        )
        .await;
    assert_eq!(status, StatusCode::PAYLOAD_TOO_LARGE, "{body}");
    assert!(
        body["message"]
            .as_str()
            .expect("текст")
            .contains(&limits::PROJECTS_PER_USER.to_string()),
        "{body}"
    );
    stand.drop_schema().await;
}

#[tokio::test]
async fn projects_need_a_token() {
    let Some(stand) = Stand::open("p_auth").await else {
        return skipped("вход обязателен");
    };
    let (status, _) = stand.get_as("/api/projects", "не-токен").await;
    assert_eq!(status, StatusCode::UNAUTHORIZED);
    stand.drop_schema().await;
}

#[tokio::test]
async fn the_build_target_and_flags_are_checked_as_a_pair() {
    // Проверка ключей идёт модулем: своего списка ключей у сервера нет и быть не
    // должно. Значит и здесь нужна собранная статика - ровно как проверкам генерации в
    // архиве.
    if std::env::var("TAKT_WEB_TEST_STATIC").is_err() {
        return skipped("ключи сборки: не задан TAKT_WEB_TEST_STATIC — модуля нет");
    }
    let Some(stand) = Stand::open("p_flags").await else {
        return skipped("ключи сборки");
    };
    let token = owner(&stand, "ivan").await;
    let id = project(&stand, &token, "Термореле").await;

    // Умолчание названо: проект без выбора собирается целью `c` без ключей.
    let (_, body) = stand.get_as(&format!("/api/projects/{id}"), &token).await;
    assert_eq!(body["build_target"], "c", "{body}");
    assert_eq!(body["build_args"], "", "{body}");

    // Годная пара принимается и переживает чтение.
    let (status, body) = stand
        .patch_as(
            &format!("/api/projects/{id}"),
            &token,
            serde_json::json!({"build_target": "sv-mmio", "build_args": "--bus=apb"}),
        )
        .await;
    assert_eq!(status, StatusCode::OK, "{body}");
    let (_, body) = stand.get_as(&format!("/api/projects/{id}"), &token).await;
    assert_eq!(body["build_target"], "sv-mmio");
    assert_eq!(body["build_args"], "--bus=apb");

    // Половина пары делает негодной другую: `--bus=apb` годится `sv-mmio` и не годится
    // `rust`. Проверяй сервер только присланное поле - эта смена прошла бы молча, и
    // проект собирался бы отказом.
    let (status, body) = stand
        .patch_as(
            &format!("/api/projects/{id}"),
            &token,
            serde_json::json!({"build_target": "rust"}),
        )
        .await;
    assert_eq!(status, StatusCode::BAD_REQUEST, "{body}");
    assert!(
        body["message"].as_str().expect("текст").contains("--bus"),
        "причина обязана называть ключ: {body}"
    );

    // Неизвестный ключ и неизвестная цель - тоже отказ.
    for wrong in [
        serde_json::json!({"build_args": "--нет-такого"}),
        serde_json::json!({"build_target": "verilog"}),
    ] {
        let (status, body) = stand
            .patch_as(&format!("/api/projects/{id}"), &token, wrong.clone())
            .await;
        assert_eq!(status, StatusCode::BAD_REQUEST, "{wrong} → {body}");
    }

    // Контроль: отказ не сплошной. Смена обеих половин разом законна, и без этой
    // проверки "отвергать всё" выглядело бы работающим правилом.
    let (status, body) = stand
        .patch_as(
            &format!("/api/projects/{id}"),
            &token,
            serde_json::json!({"build_target": "rust", "build_args": ""}),
        )
        .await;
    assert_eq!(status, StatusCode::OK, "{body}");
    assert_eq!(body["build_target"], "rust");

    // Длина строки ключей ограничена - до разбора: работа, объём которой задаёт
    // отправитель, границы не имеет.
    let (status, body) = stand
        .patch_as(
            &format!("/api/projects/{id}"),
            &token,
            serde_json::json!({"build_args": "x".repeat(limits::BUILD_ARGS_CHARS + 1)}),
        )
        .await;
    assert_eq!(status, StatusCode::PAYLOAD_TOO_LARGE, "{body}");

    stand.drop_schema().await;
}

#[tokio::test]
async fn markdown_lives_in_the_project_and_the_active_scenario_is_named() {
    let Some(stand) = Stand::open("p_kinds").await else {
        return skipped("роды файлов и активный сценарий");
    };
    let token = owner(&stand, "ivan").await;
    let id = project(&stand, &token, "Термореле").await;

    // Три рода файлов; вид выводится из расширения одним правилом.
    for (name, text) in [
        ("model.takt", "start Run {}"),
        ("run.json", "[]"),
        ("readme.md", "# Термореле\n\nГреет, пока холодно."),
        ("cold.json", "[]"),
    ] {
        let (status, body) = stand
            .put_as(
                &format!("/api/projects/{id}/files/{name}"),
                &token,
                serde_json::json!({"text": text}),
            )
            .await;
        assert_eq!(status, StatusCode::OK, "{name}: {body}");
    }
    let (_, read) = stand.get_as(&format!("/api/projects/{id}"), &token).await;
    let kinds: Vec<(&str, &str)> = read["files"]
        .as_array()
        .expect("список")
        .iter()
        .map(|file| {
            (
                file["name"].as_str().expect("имя"),
                file["kind"].as_str().expect("вид"),
            )
        })
        .collect();
    assert_eq!(
        kinds,
        vec![
            ("cold.json", "scenario"),
            ("model.takt", "takt"),
            ("readme.md", "markdown"),
            ("run.json", "scenario"),
        ],
        "роды выведены из расширения"
    );

    // Чужое расширение отвергается, и причина называет все три.
    let (status, body) = stand
        .put_as(
            &format!("/api/projects/{id}/files/notes.txt"),
            &token,
            serde_json::json!({"text": "нет"}),
        )
        .await;
    assert_eq!(status, StatusCode::BAD_REQUEST, "{body}");
    assert!(
        body["message"].as_str().expect("текст").contains(".md"),
        "причина не называет род: {body}"
    );

    // Активный файл - только модель, активный сценарий - только сценарий: перепутанные
    // роли дали бы не отказ, а пустую страницу либо прогон по пояснению.
    for (field, value) in [
        ("main_file", "readme.md"),
        ("main_file", "run.json"),
        ("main_scenario", "model.takt"),
        ("main_scenario", "readme.md"),
        ("main_scenario", "нет-такого.json"),
    ] {
        let (status, body) = stand
            .patch_as(
                &format!("/api/projects/{id}"),
                &token,
                serde_json::json!({ field: value }),
            )
            .await;
        assert_eq!(status, StatusCode::BAD_REQUEST, "{field}={value}: {body}");
    }

    // Контроль: годная пара ролей принимается - отказ не сплошной.
    let (status, patched) = stand
        .patch_as(
            &format!("/api/projects/{id}"),
            &token,
            serde_json::json!({"main_file": "model.takt", "main_scenario": "cold.json"}),
        )
        .await;
    assert_eq!(status, StatusCode::OK, "{patched}");
    assert_eq!(patched["main_scenario"], "cold.json");

    // Удалённый сценарий забывается - как и активный файл: иначе прогон шёл бы по
    // сценарию, которого нет.
    stand
        .delete_as(&format!("/api/projects/{id}/files/cold.json"), &token)
        .await;
    let (_, read) = stand.get_as(&format!("/api/projects/{id}"), &token).await;
    assert!(read["main_scenario"].is_null(), "{read}");

    stand.drop_schema().await;
}
