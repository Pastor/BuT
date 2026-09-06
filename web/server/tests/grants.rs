//! Проверки прав и копирования.
//!
//! Политика та же, что у прочих наборов: нет базы - проверки не выполняются и говорят
//! об этом словами, а решает это проверка `check-web-server.sh`.
//!
//! Предмет - **матрица "уровень x операция"**, и она проверяется целиком, а не по одной
//! клетке. Право, съехавшее на ступень, не проявляется отказом: сервис отвечает `200`,
//! и чужая запись ложится в чужой проект.

mod common;

use axum::http::StatusCode;
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

/// Создаёт закрытый проект с одним файлом.
async fn project(stand: &Stand, token: &str, name: &str) -> String {
    let (status, body) = stand
        .post_as("/api/projects", token, serde_json::json!({"name": name}))
        .await;
    assert_eq!(status, StatusCode::CREATED, "{body}");
    let id = body["id"].as_str().expect("идентификатор").to_string();
    let (status, body) = stand
        .put_as(
            &format!("/api/projects/{id}/files/model.takt"),
            token,
            serde_json::json!({"text": "model A {}"}),
        )
        .await;
    assert_eq!(status, StatusCode::OK, "{body}");
    id
}

/// Текущая ревизия проекта.
///
/// Спрашивается перед каждой записью, а не считается в уме: удавшаяся запись поднимает
/// ревизию, и матрица начала бы проверять конфликт вместо права - то есть краснела бы
/// не о том.
async fn revision(stand: &Stand, token: &str, id: &str) -> i64 {
    let (status, body) = stand.get_as(&format!("/api/projects/{id}"), token).await;
    assert_eq!(status, StatusCode::OK, "{body}");
    body["revision"].as_i64().expect("ревизия")
}

/// Выдаёт право и убеждается, что оно выдано.
async fn grant(stand: &Stand, owner: &str, id: &str, login: &str, level: &str) {
    let (status, body) = stand
        .put_as(
            &format!("/api/projects/{id}/grants/{login}"),
            owner,
            serde_json::json!({"level": level}),
        )
        .await;
    assert_eq!(status, StatusCode::OK, "{body}");
    assert_eq!(body["level"], level);
}

#[tokio::test]
async fn the_matrix_of_level_and_operation_holds() {
    let Some(stand) = Stand::open("g_matrix").await else {
        return skipped("матрица прав");
    };
    let owner = person(&stand, "ivan").await;
    let viewer = person(&stand, "vera").await;
    let forker = person(&stand, "fedor").await;
    let editor = person(&stand, "egor").await;
    let nobody = person(&stand, "nikto").await;

    let id = project(&stand, &owner, "Термореле").await;
    grant(&stand, &owner, &id, "vera", "view").await;
    grant(&stand, &owner, &id, "fedor", "fork").await;
    grant(&stand, &owner, &id, "egor", "edit").await;

    // Матрица собирается целиком и падает списком: проверяй её по клетке - и первый же
    // отказ спрятал бы остальные, а съехавшее на ступень право выглядит работающим
    // сервисом.
    let mut wrong = Vec::new();
    for (who, token, level) in [
        ("никто", &nobody, "none"),
        ("view", &viewer, "view"),
        ("fork", &forker, "fork"),
        ("edit", &editor, "edit"),
        ("владелец", &owner, "owner"),
    ] {
        // Проекта, к которому нет доступа вовсе, не существует: `404`.
        let expect_read = if level == "none" {
            StatusCode::NOT_FOUND
        } else {
            StatusCode::OK
        };
        let (status, body) = stand.get_as(&format!("/api/projects/{id}"), token).await;
        if status != expect_read {
            wrong.push(format!("{who}: чтение — {status}, ждали {expect_read}"));
        }
        if status == StatusCode::OK && body["level"] != level {
            wrong.push(format!("{who}: уровень в ответе {}", body["level"]));
        }

        // Чтение файла - тот же порог, что и проекта.
        let (status, _) = stand
            .get_as(&format!("/api/projects/{id}/files/model.takt"), token)
            .await;
        if status != expect_read {
            wrong.push(format!("{who}: чтение файла — {status}"));
        }

        // Запись файла - от `edit`.
        let expect_write = match level {
            "none" => StatusCode::NOT_FOUND,
            "view" | "fork" => StatusCode::FORBIDDEN,
            _ => StatusCode::OK,
        };
        let seen = revision(&stand, &owner, &id).await;
        let (status, _) = stand
            .put_as(
                &format!("/api/projects/{id}/files/model.takt"),
                token,
                serde_json::json!({"text": "model B {}", "revision": seen}),
            )
            .await;
        if status != expect_write {
            wrong.push(format!("{who}: запись — {status}, ждали {expect_write}"));
        }

        // Метаданные - только владелец: `edit` правит содержимое, а видимость и права
        // меняют, кому и чем проект открывается.
        let expect_patch = match level {
            "none" => StatusCode::NOT_FOUND,
            "owner" => StatusCode::OK,
            _ => StatusCode::FORBIDDEN,
        };
        let (status, _) = stand
            .patch_as(
                &format!("/api/projects/{id}"),
                token,
                serde_json::json!({"description": who}),
            )
            .await;
        if status != expect_patch {
            wrong.push(format!(
                "{who}: метаданные — {status}, ждали {expect_patch}"
            ));
        }

        // Права - только владелец, и чужому проект здесь не существует.
        let expect_grants = if level == "owner" {
            StatusCode::NO_CONTENT
        } else {
            StatusCode::NOT_FOUND
        };
        let (status, _) = stand
            .delete_as(&format!("/api/projects/{id}/grants/nikto"), token)
            .await;
        if status != expect_grants {
            wrong.push(format!("{who}: отзыв права — {status}"));
        }
    }
    assert!(
        wrong.is_empty(),
        "матрица разошлась:\n  {}",
        wrong.join("\n  ")
    );

    stand.drop_schema().await;
}

#[tokio::test]
async fn a_grant_is_given_by_login_read_and_taken_back() {
    let Some(stand) = Stand::open("g_life").await else {
        return skipped("жизнь права");
    };
    let owner = person(&stand, "ivan").await;
    let other = person(&stand, "vera").await;
    let id = project(&stand, &owner, "Термореле").await;

    // Логин ищется без учёта регистра - как и при входе: `Vera` и `vera` один человек,
    // иначе право досталось бы никому.
    grant(&stand, &owner, &id, "VERA", "view").await;
    let (status, body) = stand
        .get_as(&format!("/api/projects/{id}/grants/vera"), &owner)
        .await;
    assert_eq!(status, StatusCode::OK, "{body}");
    assert_eq!(body["login"], "vera");
    assert_eq!(body["level"], "view");

    // Право видит владелец в составе проекта, а получатель - своим уровнем.
    let (_, mine) = stand.get_as(&format!("/api/projects/{id}"), &owner).await;
    assert_eq!(mine["grants"][0]["login"], "vera");
    let (_, theirs) = stand.get_as(&format!("/api/projects/{id}"), &other).await;
    assert_eq!(theirs["level"], "view");
    // Читателю список тех, кто ещё имеет доступ, не принадлежит.
    assert!(theirs["grants"].is_null(), "чужие права видны читателю");

    // Повышение - та же ручка.
    grant(&stand, &owner, &id, "vera", "edit").await;
    let (_, theirs) = stand.get_as(&format!("/api/projects/{id}"), &other).await;
    assert_eq!(theirs["level"], "edit");

    // Отзыв действует немедленно - до истечения токена.
    let (status, _) = stand
        .delete_as(&format!("/api/projects/{id}/grants/vera"), &owner)
        .await;
    assert_eq!(status, StatusCode::NO_CONTENT);
    let (status, _) = stand.get_as(&format!("/api/projects/{id}"), &other).await;
    assert_eq!(status, StatusCode::NOT_FOUND, "отзыв действует сразу");
    // Отзыв того, чего нет, - тоже успех: предмет просьбы выполнен.
    let (status, _) = stand
        .delete_as(&format!("/api/projects/{id}/grants/vera"), &owner)
        .await;
    assert_eq!(status, StatusCode::NO_CONTENT);

    // Неизвестный логин при выдаче - отказ: владелец обязан узнать, что ошибся в имени,
    // иначе он уверен, что доступ выдан.
    let (status, _) = stand
        .put_as(
            &format!("/api/projects/{id}/grants/такого-нет"),
            &owner,
            serde_json::json!({"level": "view"}),
        )
        .await;
    assert_eq!(status, StatusCode::NOT_FOUND);

    // Уровень вне лестницы и право себе - отказы с причиной.
    for (login, level) in [("vera", "owner"), ("vera", "всё"), ("ivan", "edit")] {
        let (status, body) = stand
            .put_as(
                &format!("/api/projects/{id}/grants/{login}"),
                &owner,
                serde_json::json!({"level": level}),
            )
            .await;
        assert_eq!(status, StatusCode::BAD_REQUEST, "{login}/{level}: {body}");
        assert_eq!(body["error"], "bad_request");
    }

    stand.drop_schema().await;
}

#[tokio::test]
async fn a_fork_lives_its_own_life() {
    let Some(stand) = Stand::open("g_fork").await else {
        return skipped("копирование");
    };
    let owner = person(&stand, "ivan").await;
    let taker = person(&stand, "vera").await;
    let id = project(&stand, &owner, "Термореле").await;
    let (status, _) = stand
        .patch_as(
            &format!("/api/projects/{id}"),
            &owner,
            serde_json::json!({"visibility": "public", "main_file": "model.takt"}),
        )
        .await;
    assert_eq!(status, StatusCode::OK);

    let (status, copy) = stand
        .post_as(
            &format!("/api/projects/{id}/fork"),
            &taker,
            serde_json::json!({}),
        )
        .await;
    assert_eq!(status, StatusCode::CREATED, "{copy}");
    let copy_id = copy["id"].as_str().expect("идентификатор").to_string();
    assert_eq!(copy["owner"], "vera", "копия принадлежит взявшему");
    // Копия закрыта: взяли образец себе, а не переопубликовали чужое.
    assert_eq!(copy["visibility"], "private");
    assert_eq!(copy["forked_from"], id);
    assert_eq!(
        copy["takt_lang"], stand.module_version,
        "версия модуля — та же (A5)"
    );
    assert_eq!(copy["main_file"], "model.takt");
    assert!(
        copy["size_bytes"].as_i64().expect("размер") > 0,
        "файлы скопированы"
    );

    // Автор исходника видит число копий; открытых среди них пока нет.
    let (_, mine) = stand.get_as(&format!("/api/projects/{id}"), &owner).await;
    assert_eq!(mine["forks"], 1);
    assert_eq!(mine["open_forks"].as_array().expect("список").len(), 0);

    // Правка исходника копию не трогает: связь односторонняя и без синхронизации.
    let (status, _) = stand
        .put_as(
            &format!("/api/projects/{id}/files/model.takt"),
            &owner,
            serde_json::json!({"text": "model ИЗМЕНЁННЫЙ {}", "revision": 1}),
        )
        .await;
    assert_eq!(status, StatusCode::OK);
    let (_, file) = stand
        .get_as(&format!("/api/projects/{copy_id}/files/model.takt"), &taker)
        .await;
    assert_eq!(file["text"], "model A {}", "копия осталась собой");

    // Открытая копия попадает в список автору исходника.
    let (status, _) = stand
        .patch_as(
            &format!("/api/projects/{copy_id}"),
            &taker,
            serde_json::json!({"visibility": "public"}),
        )
        .await;
    assert_eq!(status, StatusCode::OK);
    let (_, mine) = stand.get_as(&format!("/api/projects/{id}"), &owner).await;
    assert_eq!(mine["open_forks"].as_array().expect("список").len(), 1);
    assert_eq!(mine["open_forks"][0]["owner"], "vera");

    // Удаление исходника копию не уносит: у неё своя жизнь.
    let (status, _) = stand
        .delete_as(&format!("/api/projects/{id}"), &owner)
        .await;
    assert_eq!(status, StatusCode::NO_CONTENT);
    let (status, still) = stand
        .get_as(&format!("/api/projects/{copy_id}"), &taker)
        .await;
    assert_eq!(status, StatusCode::OK, "{still}");
    assert!(
        still["forked_from"].is_null(),
        "ссылка на исходник обнулена"
    );

    stand.drop_schema().await;
}

#[tokio::test]
async fn forking_needs_the_right_and_is_not_for_ones_own() {
    let Some(stand) = Stand::open("g_fork_right").await else {
        return skipped("право на копию");
    };
    let owner = person(&stand, "ivan").await;
    let viewer = person(&stand, "vera").await;
    let stranger = person(&stand, "nikto").await;
    let id = project(&stand, &owner, "Термореле").await;

    // Закрытый чужой не существует.
    let (status, _) = stand
        .post_as(
            &format!("/api/projects/{id}/fork"),
            &stranger,
            serde_json::json!({}),
        )
        .await;
    assert_eq!(status, StatusCode::NOT_FOUND);

    // `view` читает, но не копирует.
    grant(&stand, &owner, &id, "vera", "view").await;
    let (status, body) = stand
        .post_as(
            &format!("/api/projects/{id}/fork"),
            &viewer,
            serde_json::json!({}),
        )
        .await;
    assert_eq!(status, StatusCode::FORBIDDEN, "{body}");

    // `fork` - копирует.
    grant(&stand, &owner, &id, "vera", "fork").await;
    let (status, body) = stand
        .post_as(
            &format!("/api/projects/{id}/fork"),
            &viewer,
            serde_json::json!({}),
        )
        .await;
    assert_eq!(status, StatusCode::CREATED, "{body}");

    // Свой проект копировать незачем - отказ с причиной, а не молчаливый дубликат: два
    // одинаковых проекта в списке автор заводит сам, если хочет.
    let (status, body) = stand
        .post_as(
            &format!("/api/projects/{id}/fork"),
            &owner,
            serde_json::json!({}),
        )
        .await;
    assert_eq!(status, StatusCode::BAD_REQUEST, "{body}");

    stand.drop_schema().await;
}

#[tokio::test]
async fn a_granted_project_is_in_my_list_with_its_level() {
    let Some(stand) = Stand::open("g_list").await else {
        return skipped("список с уровнем");
    };
    let owner = person(&stand, "ivan").await;
    let other = person(&stand, "vera").await;
    let mine = project(&stand, &owner, "Своё").await;
    let theirs = project(&stand, &other, "Чужое").await;
    grant(&stand, &other, &theirs, "ivan", "edit").await;

    // Открытый чужой проект в "мои проекты" не попадает: он живёт в витрине, а этот
    // список - то, за что я отвечаю.
    let public = project(&stand, &other, "Открытое").await;
    let (status, _) = stand
        .patch_as(
            &format!("/api/projects/{public}"),
            &other,
            serde_json::json!({"visibility": "public"}),
        )
        .await;
    assert_eq!(status, StatusCode::OK);

    let (status, list) = stand.get_as("/api/projects", &owner).await;
    assert_eq!(status, StatusCode::OK, "{list}");
    let rows = list.as_array().expect("список");
    let mut seen: Vec<(String, String)> = rows
        .iter()
        .map(|row| {
            (
                row["id"].as_str().expect("идентификатор").to_string(),
                row["level"].as_str().expect("уровень").to_string(),
            )
        })
        .collect();
    seen.sort();
    let mut expected = vec![(mine, "owner".to_string()), (theirs, "edit".to_string())];
    expected.sort();
    assert_eq!(seen, expected, "в списке своё и выданное, с уровнем");

    stand.drop_schema().await;
}
