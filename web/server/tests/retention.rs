//! Проверки срока хранения и свёртки.
//!
//! Предмет - **невидимость архивации для автора**: свёрнутый проект обязан
//! разворачиваться первым же обращением. Ошибка здесь не проявляется отказом сборки:
//! сервис отвечает, страница открывается, и только исходник оказывается пуст - то есть
//! автор теряет работу, ничего об этом не узнав.

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

/// Создаёт проект с одним файлом.
async fn project(stand: &Stand, token: &str, name: &str, text: &str) -> String {
    let (status, body) = stand
        .post_as("/api/projects", token, serde_json::json!({"name": name}))
        .await;
    assert_eq!(status, StatusCode::CREATED, "{body}");
    let id = body["id"].as_str().expect("идентификатор").to_string();
    let (status, body) = stand
        .put_as(
            &format!("/api/projects/{id}/files/model.takt"),
            token,
            serde_json::json!({"text": text}),
        )
        .await;
    assert_eq!(status, StatusCode::OK, "{body}");
    id
}

#[tokio::test]
async fn a_stale_project_is_packed_and_comes_back_on_the_first_touch() {
    let Some(stand) = Stand::open("r_pack").await else {
        return skipped("свёртка по сроку");
    };
    let author = person(&stand, "ivan").await;
    let id = project(&stand, &author, "Термореле", "model A {}").await;

    // Свежий проект обход не трогает: отметка обращения поставлена при создании. Ноль в
    // ней означал бы "не обращались никогда", и проект свернулся бы в тот же час, когда
    // его завели.
    assert_eq!(stand.sweep(90).await, 0, "свежий проект свёрнут");

    // Отодвигаем отметку на сто дней назад - то же, что прошедшее время.
    stand.age_project(&id, 100 * 86_400).await;
    assert_eq!(stand.sweep(90).await, 1, "залежавшийся не свёрнут");
    assert!(stand.is_packed(&id).await, "проект не свёрнут на диске");

    // Первое же обращение разворачивает его обратно - и текст тот же.
    let (status, file) = stand
        .get_as(&format!("/api/projects/{id}/files/model.takt"), &author)
        .await;
    assert_eq!(status, StatusCode::OK, "{file}");
    assert_eq!(file["text"], "model A {}", "текст пережил свёртку");
    assert!(!stand.is_packed(&id).await, "проект остался свёрнутым");

    // И счётчик сброшен: второй обход его не трогает.
    assert_eq!(stand.sweep(90).await, 0, "счётчик не сброшен обращением");

    stand.drop_schema().await;
}

#[tokio::test]
async fn a_touch_resets_the_countdown() {
    let Some(stand) = Stand::open("r_touch").await else {
        return skipped("сброс счётчика");
    };
    let author = person(&stand, "ivan").await;
    let id = project(&stand, &author, "Термореле", "model A {}").await;

    // Чтение проекта - обращение: правило говорит про чтение и запись, и пропуск чтения
    // свернул бы проект, который читают каждый день.
    stand.age_project(&id, 100 * 86_400).await;
    let (status, _) = stand.get_as(&format!("/api/projects/{id}"), &author).await;
    assert_eq!(status, StatusCode::OK);
    assert_eq!(
        stand.sweep(90).await,
        0,
        "чтение проекта не сбросило счётчик"
    );

    // Запись - тоже.
    stand.age_project(&id, 100 * 86_400).await;
    let (status, _) = stand
        .put_as(
            &format!("/api/projects/{id}/files/model.takt"),
            &author,
            serde_json::json!({"text": "model B {}", "revision": 1}),
        )
        .await;
    assert_eq!(status, StatusCode::OK);
    assert_eq!(stand.sweep(90).await, 0, "запись не сбросила счётчик");

    stand.drop_schema().await;
}

#[tokio::test]
async fn a_packed_project_accepts_a_write_after_unpacking() {
    let Some(stand) = Stand::open("r_write").await else {
        return skipped("запись в свёрнутый");
    };
    let author = person(&stand, "ivan").await;
    let id = project(&stand, &author, "Термореле", "model A {}").await;
    stand.age_project(&id, 100 * 86_400).await;
    assert_eq!(stand.sweep(90).await, 1);

    // Запись в свёрнутый проект обязана развернуть его до записи: иначе она легла бы
    // поверх снятых с диска файлов, и остальные исходники исчезли бы молча.
    let (status, written) = stand
        .put_as(
            &format!("/api/projects/{id}/files/second.takt"),
            &author,
            serde_json::json!({"text": "model B {}", "revision": 1}),
        )
        .await;
    assert_eq!(status, StatusCode::OK, "{written}");
    let (_, first) = stand
        .get_as(&format!("/api/projects/{id}/files/model.takt"), &author)
        .await;
    assert_eq!(first["text"], "model A {}", "соседний файл потерян");
    assert!(!stand.is_packed(&id).await);

    stand.drop_schema().await;
}

#[tokio::test]
async fn a_packed_project_is_searched_and_exported_like_any_other() {
    let Some(stand) = Stand::open("r_search").await else {
        return skipped("свёрнутый в витрине");
    };
    let author = person(&stand, "ivan").await;
    let id = project(&stand, &author, "Термореле", "model Верёвкоукладчик {}").await;
    let (status, _) = stand
        .patch_as(
            &format!("/api/projects/{id}"),
            &author,
            serde_json::json!({"visibility": "public"}),
        )
        .await;
    assert_eq!(status, StatusCode::OK);
    stand.age_project(&id, 100 * 86_400).await;
    assert_eq!(stand.sweep(90).await, 1);

    // Поисковое значение живёт в базе и свёртки не замечает: свёрнутый проект обязан
    // находиться, иначе витрина теряла бы проекты по сроку, не сказав о том ни слова.
    let (status, page) = stand.get("/api/public?q=верёвкоукладчик").await;
    assert_eq!(status, StatusCode::OK, "{page}");
    assert_eq!(page["items"][0]["id"], id, "свёрнутый выпал из поиска");

    // А выгрузка его разворачивает - как всякое обращение.
    let (status, bytes) = stand
        .bytes(&format!("/api/projects/{id}/archive"), Some(&author))
        .await;
    assert_eq!(status, StatusCode::OK);
    assert!(!bytes.is_empty());
    assert!(!stand.is_packed(&id).await, "выгрузка не развернула проект");

    stand.drop_schema().await;
}

#[tokio::test]
async fn removing_a_project_takes_its_files_off_the_disk() {
    let Some(stand) = Stand::open("r_remove").await else {
        return skipped("удаление с диска");
    };
    let author = person(&stand, "ivan").await;
    let id = project(&stand, &author, "Термореле", "model A {}").await;
    let owner = stand.user_id("ivan").await;
    assert!(stand.project_dir_exists(&owner, &id), "файлов нет на диске");

    let (status, _) = stand
        .delete_as(&format!("/api/projects/{id}"), &author)
        .await;
    assert_eq!(status, StatusCode::NO_CONTENT);
    // Диск чистится вместе с базой: осиротевший каталог не виден ни списку, ни
    // человеку, и растёт он молча.
    assert!(
        !stand.project_dir_exists(&owner, &id),
        "каталог проекта остался на диске"
    );

    stand.drop_schema().await;
}

#[tokio::test]
async fn an_orphaned_directory_is_swept_but_a_young_one_is_left_alone() {
    let Some(stand) = Stand::open("r_orphan").await else {
        return skipped("подметание осиротевших");
    };
    let author = person(&stand, "ivan").await;
    let id = project(&stand, &author, "Термореле", "model A {}").await;
    let owner = stand.user_id("ivan").await;
    assert!(stand.project_dir_exists(&owner, &id), "файлов нет на диске");

    // Живой проект подметанию не принадлежит - ни при какой выдержке.
    assert_eq!(stand.sweep_orphans(0).await, 0, "убран ЖИВОЙ проект");
    assert!(stand.project_dir_exists(&owner, &id));

    // Строка исчезает мимо ручки: прямой `DELETE` в базе либо обрыв между двумя шагами
    // удаления. Каталог остаётся, и не виден больше никому.
    stand.forget_project(&id).await;
    assert!(stand.project_dir_exists(&owner, &id), "каталог исчез сам");

    // Пока каталог молод, он не сирота, а идущая запись: между записью файла на диск и
    // закрытием транзакции проходит время, и подметание в этот миг унесло бы исходник у
    // автора прямо во время сохранения.
    assert_eq!(stand.sweep_orphans(3600).await, 0, "убрано молодое");
    assert!(stand.project_dir_exists(&owner, &id), "каталог убран рано");

    // А без выдержки - то же самое состояние диска судится как мусор.
    assert_eq!(stand.sweep_orphans(0).await, 1, "сирота не убрана");
    assert!(!stand.project_dir_exists(&owner, &id), "каталог остался");
    // Владелец без единого проекта каталога не заслуживает.
    assert!(!stand.owner_dir_exists(&owner), "пустой каталог остался");

    // Второй обход убирать уже нечего: подметание идемпотентно.
    assert_eq!(stand.sweep_orphans(0).await, 0);

    stand.drop_schema().await;
}

#[tokio::test]
async fn sweeping_orphans_spares_the_neighbours_and_takes_the_packed_form() {
    let Some(stand) = Stand::open("r_orphan2").await else {
        return skipped("подметание: соседи и свёрнутая форма");
    };
    let author = person(&stand, "ivan").await;
    let stranger = person(&stand, "petr").await;
    let doomed = project(&stand, &author, "Сирота", "model A {}").await;
    let kept = project(&stand, &author, "Живой", "model B {}").await;
    let alien = project(&stand, &stranger, "Соседский", "model C {}").await;
    let owner = stand.user_id("ivan").await;
    let other = stand.user_id("petr").await;

    // Сирота свёрнута: подметание обязано убирать обе формы - иначе `.zip` остаётся на
    // диске, а отчёт утверждает, что убрано.
    stand.age_project(&doomed, 100 * 86_400).await;
    assert_eq!(stand.sweep(90).await, 1);
    assert!(stand.is_packed(&doomed).await);
    stand.forget_project(&doomed).await;

    assert_eq!(stand.sweep_orphans(0).await, 1, "убрано не одно");
    assert!(
        !stand.store.is_packed(&owner, &doomed).expect("признак"),
        "свёрнутая форма сироты осталась"
    );
    // Соседи целы - и у того же владельца, и у другого.
    assert!(stand.project_dir_exists(&owner, &kept), "убран живой сосед");
    assert!(
        stand.project_dir_exists(&other, &alien),
        "убран чужой проект"
    );
    assert!(stand.owner_dir_exists(&owner), "каталог владельца снесён");

    // Сирота узнаётся по паре "владелец и проект", а не по одному идентификатору:
    // каталог с именем живого проекта, лежащий у чужого владельца, - мусор, и живая
    // строка его не оправдывает.
    stand
        .store
        .write(&other, &kept, "model.takt", "model X {}")
        .expect("подкладка");
    assert!(stand.project_dir_exists(&other, &kept));
    assert_eq!(stand.sweep_orphans(0).await, 1, "подкладка не убрана");
    assert!(
        !stand.project_dir_exists(&other, &kept),
        "подкладка осталась"
    );
    assert!(
        stand.project_dir_exists(&owner, &kept),
        "убран настоящий проект"
    );

    // И файлы живого соседа читаются, как читались.
    let (status, file) = stand
        .get_as(&format!("/api/projects/{kept}/files/model.takt"), &author)
        .await;
    assert_eq!(status, StatusCode::OK, "{file}");
    assert_eq!(file["text"], "model B {}");

    stand.drop_schema().await;
}
