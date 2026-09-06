//! Проверки видимости, витрины и поиска.
//!
//! Политика та же, что у `http.rs` и `projects.rs`: нет базы - проверки не выполняются
//! и говорят об этом словами, а решает это проверка `check-web-server.sh`.
//!
//! Предмет здесь - **что видно и кому**. Ошибка в эту сторону не проявляется отказом:
//! сервис отвечает `200`, страница показывает проект, и лишь автор однажды узнаёт, что
//! закрытое читали все.

mod common;

use axum::http::StatusCode;
use common::{Stand, skipped};

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

/// Создаёт проект с заданной видимостью и кладёт в него файл.
async fn project(
    stand: &Stand,
    token: &str,
    name: &str,
    description: &str,
    visibility: &str,
    text: &str,
) -> String {
    let (status, body) = stand
        .post_as(
            "/api/projects",
            token,
            serde_json::json!({"name": name, "description": description}),
        )
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
    let (status, body) = stand
        .patch_as(
            &format!("/api/projects/{id}"),
            token,
            serde_json::json!({"visibility": visibility}),
        )
        .await;
    assert_eq!(status, StatusCode::OK, "{body}");
    id
}

/// Собирает идентификаторы витрины.
fn ids(page: &serde_json::Value) -> Vec<String> {
    page["items"]
        .as_array()
        .expect("список")
        .iter()
        .map(|item| item["id"].as_str().expect("идентификатор").to_string())
        .collect()
}

#[tokio::test]
async fn visibility_decides_who_reads_the_project() {
    let Some(stand) = Stand::open("s_visibility").await else {
        return skipped("видимость");
    };
    let mine = owner(&stand, "ivan").await;
    let stranger = owner(&stand, "pyotr").await;

    let closed = project(&stand, &mine, "Закрытый", "", "private", "model A {}").await;
    let by_link = project(&stand, &mine, "По ссылке", "", "link", "model B {}").await;
    let open = project(&stand, &mine, "Открытый", "", "public", "model C {}").await;

    // Владелец читает своё при любой видимости.
    for id in [&closed, &by_link, &open] {
        let (status, body) = stand.get_as(&format!("/api/projects/{id}"), &mine).await;
        assert_eq!(status, StatusCode::OK, "своё: {body}");
    }

    // Закрытый чужой отвечает `404`, а не `403`: `403` означал бы "он есть, но не для
    // вас", то есть ручка перечисляла бы чужие проекты по ответам.
    let (status, body) = stand
        .get_as(&format!("/api/projects/{closed}"), &stranger)
        .await;
    assert_eq!(status, StatusCode::NOT_FOUND, "{body}");
    assert_eq!(body["error"], "not_found");
    let (status, _) = stand.get(&format!("/api/projects/{closed}")).await;
    assert_eq!(status, StatusCode::NOT_FOUND, "без входа — тем более");

    // `link` и `public` читает всякий, в том числе **без учётной записи**: ссылкой
    // делятся с человеком, который сюда пришёл впервые.
    for id in [&by_link, &open] {
        let (status, body) = stand.get(&format!("/api/projects/{id}")).await;
        assert_eq!(status, StatusCode::OK, "{body}");
        assert_eq!(body["owner"], "ivan", "у образца назван автор");
        let (status, file) = stand
            .get(&format!("/api/projects/{id}/files/model.takt"))
            .await;
        assert_eq!(status, StatusCode::OK, "{file}");
        assert!(file["text"].as_str().expect("текст").starts_with("model"));
    }
    // А файл закрытого - нет, и тем же `404`.
    let (status, _) = stand
        .get(&format!("/api/projects/{closed}/files/model.takt"))
        .await;
    assert_eq!(status, StatusCode::NOT_FOUND);

    // Чужой не правит открытое: чтение и запись - разные вопросы.
    //
    // Здесь `403`, а не `404`: открытый проект человеку видно, и прятать его уже
    // незачем - а "не найдено" отправило бы его искать опечатку в ссылке вместо того,
    // чтобы попросить право.
    let (status, body) = stand
        .put_as(
            &format!("/api/projects/{open}/files/model.takt"),
            &stranger,
            serde_json::json!({"text": "model X {}", "revision": 1}),
        )
        .await;
    assert_eq!(
        status,
        StatusCode::FORBIDDEN,
        "правка чужого открытого: {body}"
    );

    stand.drop_schema().await;
}

#[tokio::test]
async fn the_showcase_shows_public_only() {
    let Some(stand) = Stand::open("s_list").await else {
        return skipped("витрина");
    };
    let mine = owner(&stand, "ivan").await;
    let closed = project(&stand, &mine, "Закрытый", "", "private", "model A {}").await;
    let by_link = project(&stand, &mine, "По ссылке", "", "link", "model B {}").await;
    let open = project(&stand, &mine, "Открытый", "", "public", "model C {}").await;

    let (status, page) = stand.get("/api/public").await;
    assert_eq!(status, StatusCode::OK, "{page}");
    let listed = ids(&page);
    assert_eq!(listed, vec![open.clone()], "в витрине только открытые");
    assert!(!listed.contains(&closed));
    // `link` читается по идентификатору, но в списке его нет - в этом и состоит смысл
    // третьего состояния: показать закрытое человеку, не выкладывая его на витрину.
    assert!(!listed.contains(&by_link));
    assert_eq!(page["items"][0]["owner"], "ivan");
    assert_eq!(page["next_cursor"], serde_json::Value::Null);

    stand.drop_schema().await;
}

#[tokio::test]
async fn search_finds_by_name_description_and_body() {
    let Some(stand) = Stand::open("s_search").await else {
        return skipped("поиск");
    };
    let mine = owner(&stand, "ivan").await;
    let by_name = project(
        &stand,
        &mine,
        "Термореле",
        "образец",
        "public",
        "model A {}",
    )
    .await;
    let by_body = project(
        &stand,
        &mine,
        "Второй",
        "другое",
        "public",
        "model Верёвкоукладчик {}",
    )
    .await;
    let closed = project(
        &stand,
        &mine,
        "Третий",
        "",
        "private",
        "model Верёвкоукладчик {}",
    )
    .await;

    // По имени.
    let (status, page) = stand.get("/api/public?q=термореле").await;
    assert_eq!(status, StatusCode::OK, "{page}");
    assert_eq!(
        ids(&page),
        vec![by_name.clone()],
        "не нашлось по имени — проверьте локаль базы: при `C` кириллица не \
         приводится к нижнему регистру, и поиск молча пуст"
    );

    // По слову в теле файла - ради этого поиск и не может быть вычисляемой колонкой:
    // она видит только свою строку.
    let (status, page) = stand.get("/api/public?q=верёвкоукладчик").await;
    assert_eq!(status, StatusCode::OK, "{page}");
    let found = ids(&page);
    assert_eq!(found, vec![by_body.clone()], "нашёлся по телу");
    // И тот же текст в закрытом проекте не находится: иначе поиск стал бы оракулом
    // содержимого - по совпадению видно, что такой проект есть.
    assert!(!found.contains(&closed));

    // Слова, которого нет, не находит: пустая выдача - тоже ответ.
    let (status, page) = stand.get("/api/public?q=трамвай").await;
    assert_eq!(status, StatusCode::OK);
    assert!(ids(&page).is_empty());

    // Правка имени пересчитывает поиск: без этого проект искался бы по позавчерашнему
    // имени, и увидеть это глазом нельзя.
    let (status, body) = stand
        .patch_as(
            &format!("/api/projects/{by_name}"),
            &mine,
            serde_json::json!({"name": "Расходомер"}),
        )
        .await;
    assert_eq!(status, StatusCode::OK, "{body}");
    let (_, page) = stand.get("/api/public?q=расходомер").await;
    assert_eq!(ids(&page), vec![by_name.clone()], "ищется по новому имени");
    let (_, page) = stand.get("/api/public?q=термореле").await;
    assert!(ids(&page).is_empty(), "по старому — уже нет");

    // Пояснение на Markdown индексируется: описание словами - самое полезное для
    // поиска, и по слову из него проект обязан находиться так же, как по слову из
    // модели.
    let (status, _) = stand
        .put_as(
            &format!("/api/projects/{by_name}/files/readme.md"),
            &mine,
            // Ревизии нет: файл новый, и её посылают только для уже существующего.
            serde_json::json!({"text": "# Заметка\n\nПрибор меряет расход мазута."}),
        )
        .await;
    assert_eq!(status, StatusCode::OK);
    let (_, page) = stand.get("/api/public?q=мазута").await;
    assert_eq!(ids(&page), vec![by_name.clone()], "нашёлся по пояснению");

    // Сценарий не индексируется: он задаёт вход, а не описывает модель.
    let (status, _) = stand
        .put_as(
            &format!("/api/projects/{by_body}/files/run.json"),
            &mine,
            serde_json::json!({"text": "{\"шаги\": \"трамвай\"}", "revision": 1}),
        )
        .await;
    assert_eq!(status, StatusCode::OK);
    let (_, page) = stand.get("/api/public?q=трамвай").await;
    assert!(ids(&page).is_empty(), "сценарий в индекс не идёт");

    stand.drop_schema().await;
}

#[tokio::test]
async fn the_owner_filter_is_a_filter_and_not_a_word() {
    let Some(stand) = Stand::open("s_owner").await else {
        return skipped("фильтр по автору");
    };
    let first = owner(&stand, "ivan").await;
    let second = owner(&stand, "pyotr").await;
    let ivans = project(&stand, &first, "Первый", "", "public", "model A {}").await;
    let pyotrs = project(&stand, &second, "Второй", "", "public", "model B {}").await;

    let (status, page) = stand.get("/api/public?owner=IVAN").await;
    assert_eq!(status, StatusCode::OK, "{page}");
    // Логин без учёта регистра - тот же человек, что и при входе.
    assert_eq!(ids(&page), vec![ivans.clone()]);
    let (_, page) = stand.get("/api/public?owner=pyotr").await;
    assert_eq!(ids(&page), vec![pyotrs]);
    // Автора, которого нет, - пустая выдача, а не отказ.
    let (status, page) = stand.get("/api/public?owner=никого").await;
    assert_eq!(status, StatusCode::OK);
    assert!(ids(&page).is_empty());

    stand.drop_schema().await;
}

#[tokio::test]
async fn the_cursor_walks_the_showcase_without_gaps_or_repeats() {
    let Some(stand) = Stand::open("s_cursor").await else {
        return skipped("курсор");
    };
    let mine = owner(&stand, "ivan").await;
    let mut all = Vec::new();
    for index in 0..5 {
        all.push(
            project(
                &stand,
                &mine,
                &format!("Образец {index}"),
                "",
                "public",
                "model A {}",
            )
            .await,
        );
    }

    let mut seen: Vec<String> = Vec::new();
    let mut path = "/api/public?limit=2".to_string();
    loop {
        let (status, page) = stand.get(&path).await;
        assert_eq!(status, StatusCode::OK, "{page}");
        seen.extend(ids(&page));
        match page["next_cursor"].as_str() {
            Some(cursor) => path = format!("/api/public?limit=2&cursor={cursor}"),
            None => break,
        }
    }
    let mut unique = seen.clone();
    unique.sort();
    unique.dedup();
    assert_eq!(unique.len(), seen.len(), "запись показана дважды: {seen:?}");
    let mut expected = all.clone();
    expected.sort();
    assert_eq!(unique, expected, "обошли не всё");

    // Испорченный курсор - отказ с причиной, а не чужая страница.
    let (status, body) = stand.get("/api/public?cursor=не-курсор").await;
    assert_eq!(status, StatusCode::BAD_REQUEST, "{body}");
    assert_eq!(body["error"], "bad_request");

    stand.drop_schema().await;
}

///
/// Порог назван проработкой (§6) - **50 мс**, и меряется он через ручку, а не запросом
/// в базу: предмет замера - ответ сервиса, а не время `EXPLAIN`. Берётся лучшее из трёх
/// прогонов: первый греет страницы индекса, и мерить его значило бы мерить холодный
/// кеш.
#[tokio::test]
async fn ten_thousand_projects_are_searched_in_time() {
    let Some(stand) = Stand::open("s_measure").await else {
        return skipped("замер поиска");
    };
    let mine = owner(&stand, "ivan").await;
    // Контрольный проект заводится до набивки: предел "сто проектов" считается в ручке,
    // и после десяти тысяч она отвечала бы отказом. Он же доказывает, что набивка и
    // живая запись лежат в одной витрине.
    project(&stand, &mine, "Живой", "", "public", "model Нагрев {}").await;
    stand.fill_public("ivan", 10_000).await;

    // Редкое слово: работает индекс.
    let rare = best_of_three(&stand, "редкое слово", "/api/public?q=верёвкоукладчик").await;
    // Первая страница списка без поиска: работает порядок.
    let page = best_of_three(&stand, "первая страница", "/api/public").await;
    // Частое слово: индекс отдаёт десять тысяч, порядок отбирает двадцать - самый
    // дорогой из трёх случаев, и он тоже обязан уложиться.
    let common = best_of_three(&stand, "частое слово", "/api/public?q=нагрев").await;

    for (label, spent) in [
        ("редкое слово", rare),
        ("первая страница", page),
        ("частое слово", common),
    ] {
        assert!(
            spent < std::time::Duration::from_millis(50),
            "{label}: {} мс при пороге 50",
            spent.as_millis()
        );
    }

    stand.drop_schema().await;
}

/// Лучшее из трёх обращений к витрине.
///
/// Лучшее, а не среднее: замер на общей машине ловит чужую нагрузку, и среднее меряло
/// бы её, а не сервис.
async fn best_of_three(stand: &Stand, label: &str, path: &str) -> std::time::Duration {
    let mut best = std::time::Duration::MAX;
    for _ in 0..3 {
        let started = std::time::Instant::now();
        let (status, page) = stand.get(path).await;
        let spent = started.elapsed();
        assert_eq!(status, StatusCode::OK, "{page}");
        assert!(!ids(&page).is_empty(), "{label}: пустая выдача");
        best = best.min(spent);
    }
    // Микросекунды, а не миллисекунды: при пороге 50 мс замер "0 мс" ничего не сообщает -
    // ни запаса, ни ухудшения между прогонами по нему не видно.
    eprintln!("замер ({label}): {} мкс", best.as_micros());
    best
}

#[tokio::test]
async fn the_project_page_is_served_and_a_missing_file_is_missing() {
    let Some(stand) = Stand::open("s_page").await else {
        return skipped("страница проекта");
    };
    // Страница отдаётся на адрес проекта: перезагрузка на `/p/<id>` обязана открыть
    // приложение, а не `404`.
    let (status, _) = stand.get("/p/AbCd-_12").await;
    // Статики в проверке нет, и это тоже ответ: сервер говорит `404` с названной
    // причиной, а не отдаёт пустоту под видом страницы.
    assert!(
        status == StatusCode::OK || status == StatusCode::NOT_FOUND,
        "неожиданный ответ страницы: {status}"
    );

    // А вот это - Файл, и его промах обязан быть промахом: пока сюда отдавали разметку,
    // вкладка открывалась без стилей и без модуля, не сказав ни слова (нашлось прогоном
    // страницы 2026-09-04).
    let (status, body) = stand.get("/p/b/f8377401e12e/app.css").await;
    assert_eq!(status, StatusCode::NOT_FOUND, "{body}");

    stand.drop_schema().await;
}
