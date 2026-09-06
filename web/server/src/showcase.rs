//! Витрина открытых проектов: список, поиск, курсор.
//!
//! # Что видно и кому
//!
//! Витрина показывает **только `public`**, и проверка стоит **в запросе**, а не
//! фильтром после выборки: отфильтруй список в коде - и первая же забытая ветвь покажет
//! чужой закрытый проект. `link` в витрину не попадает намеренно: это и есть его смысл -
//! "читает всякий, у кого ссылка, но в списке его нет".
//!
//! # Один носитель поискового значения
//!
//! `search` кладёт [`refresh`], и второго места вычисления нет. Вычисляемой базой
//! колонка быть не может: искать надо и по тексту файлов, а `GENERATED` видит только
//! свою строку.
//!
//! Индексируются имя (вес `A`), описание (`B`) и тексты файлов `*.takt` (`C`). Сценарии
//! `*.json` не индексируются - проработка §3: они не описывают модель, а задают её
//! вход, и совпадение по числу из сценария было бы шумом.
//!
//! # Порядок и курсор
//!
//! Порядок один - `updated_at DESC, id DESC`, и он же у поиска. Ранжирование по
//! релевантности сюда не заводится: курсорная постраничность требует
//! **устойчивого полного порядка**, а вес `ts_rank` меняется от правки текста -
//! страницы поехали бы под читателем. Витрина - про свежее; ранжирование
//! остаётся кандидатом и названо здесь, а не забыто.

use axum::extract::{Query, State};
use axum::http::HeaderMap;
use axum::response::{IntoResponse, Response};
use axum::routing::get;
use axum::{Json, Router};
use serde::{Deserialize, Serialize};
use std::sync::Arc;

use crate::error::ApiError;
use crate::routes::AppState;

/// Сколько записей отдаётся за раз по умолчанию.
pub const PAGE: i64 = 20;

/// Наибольший размер страницы.
pub const PAGE_MAX: i64 = 50;

/// Как считается поисковое значение проекта.
///
/// Одна строка на весь сервис. Разведи её на "имя с описанием" и "тексты файлов" - и
/// веса разойдутся при первой же правке: одна половина будет пересчитана, другая
/// останется вчерашней, и поиск начнёт находить то, чего в проекте уже нет.
const SEARCH_SQL: &str = "
    UPDATE projects SET search =
        setweight(to_tsvector('russian', coalesce(name, '')), 'A') ||
        setweight(to_tsvector('russian', coalesce(description, '')), 'B') ||
        setweight(to_tsvector('russian', $2), 'C')
    WHERE id = $1";

/// Пересчитывает поисковое значение проекта.
///
/// Зовётся из **всех** точек записи: создание, правка метаданных, запись и удаление
/// файла, копия, загрузка архива. Пропусти одну - и проект будет искаться по
/// позавчерашнему имени, не отвечая ни на что видимое глазом.
///
/// `body` - тексты файлов `*.takt` одной строкой. Их приносит вызывающий, а не читает
/// запрос: с h тексты живут на диске, и базе их взять неоткуда. Пустая строка законна -
/// у проекта без файлов тела нет.
///
/// # Ошибки
/// Отказ базы.
pub async fn refresh(
    transaction: &tokio_postgres::Transaction<'_>,
    id: &str,
    body: &str,
) -> Result<(), ApiError> {
    transaction
        .execute(SEARCH_SQL, &[&id, &body])
        .await
        .map_err(|error| ApiError::Internal(error.into()))?;
    Ok(())
}

/// Запрос витрины.
#[derive(Debug, Deserialize)]
pub struct PublicQuery {
    /// Слова поиска. Пусто - просто список.
    #[serde(default)]
    pub q: Option<String>,
    /// Логин владельца. Отдельным параметром, а не полнотекстом: "проекты Ивана" - это
    /// фильтр, а не совпадение слова.
    #[serde(default)]
    pub owner: Option<String>,
    /// Курсор предыдущей страницы.
    #[serde(default)]
    pub cursor: Option<String>,
    /// Размер страницы.
    #[serde(default)]
    pub limit: Option<i64>,
}

/// Запись витрины.
///
/// Тела файлов здесь нет: витрина бывает длинной, а исходник читается страницей
/// проекта.
#[derive(Debug, Serialize)]
pub struct ShowcaseItem {
    pub id: String,
    pub name: String,
    pub description: String,
    /// Логин владельца - псевдоним, а не персональные данные (проработка §0).
    pub owner: String,
    pub takt_lang: String,
    pub language_version: String,
    pub size_bytes: i64,
    pub updated_at: i64,
}

/// Страница витрины.
#[derive(Debug, Serialize)]
pub struct ShowcasePage {
    pub items: Vec<ShowcaseItem>,
    /// Курсор следующей страницы; `null` - страница последняя.
    pub next_cursor: Option<String>,
}

/// Маршрут витрины.
pub fn router() -> Router<Arc<AppState>> {
    Router::new().route("/public", get(public))
}

/// Витрина: список и поиск открытых проектов.
///
/// Токена не требует: открытый проект открыт и для того, у кого учётной записи нет
/// вовсе.
async fn public(
    State(state): State<Arc<AppState>>,
    _headers: HeaderMap,
    Query(request): Query<PublicQuery>,
) -> Result<Response, ApiError> {
    let limit = page_size(request.limit);
    let words = request
        .q
        .as_deref()
        .map(str::trim)
        .filter(|q| !q.is_empty());
    let owner = request
        .owner
        .as_deref()
        .map(str::trim)
        .filter(|o| !o.is_empty());
    let (after_time, after_id) = match request.cursor.as_deref() {
        Some(cursor) => {
            let (time, id) = decode_cursor(cursor)
                .ok_or_else(|| ApiError::BadRequest("курсор не разбирается".to_string()))?;
            (Some(time), Some(id))
        }
        None => (None, None),
    };

    let client = state.pool.get().await?;
    let rows = client
        .query(
            SELECT_PUBLIC,
            &[
                &words.map(str::to_string),
                &owner.map(str::to_string),
                &after_time,
                &after_id,
                &limit,
            ],
        )
        .await?;
    let items: Vec<ShowcaseItem> = rows
        .iter()
        .map(|row| ShowcaseItem {
            id: row.get("id"),
            name: row.get("name"),
            description: row.get("description"),
            owner: row.get("owner"),
            takt_lang: row.get("takt_lang"),
            language_version: row.get("language_version"),
            size_bytes: row.get("size_bytes"),
            updated_at: row.get("updated_at"),
        })
        .collect();
    // Курсор отдаётся только у полной страницы: у неполной продолжения нет, а курсор в
    // ответе означал бы "есть ещё" и заставлял бы страницу ходить впустую.
    let next_cursor = (items.len() as i64 == limit)
        .then(|| items.last())
        .flatten()
        .map(|item| encode_cursor(item.updated_at, &item.id));
    Ok(Json(ShowcasePage { items, next_cursor }).into_response())
}

/// Размер страницы по просьбе читателя.
///
/// Просьба **прижимается**, а не отвергается: `limit=100000` - не злой умысел, а
/// любопытство, и отказ здесь ничего не защищает. Защищает потолок: без него один
/// запрос выносит витрину целиком, и "постранично" перестаёт что-либо значить.
fn page_size(asked: Option<i64>) -> i64 {
    asked.unwrap_or(PAGE).clamp(1, PAGE_MAX)
}

/// Выборка витрины.
///
/// `websearch_to_tsquery`, а не `to_tsquery`: второй отвечает отказом на человеческую
/// запись со скобкой или дефисом, то есть превращал бы опечатку читателя в ошибку
/// сервиса.
const SELECT_PUBLIC: &str = "
    SELECT p.id, p.name, p.description, u.login AS owner, p.takt_lang,
           p.language_version, p.size_bytes, p.updated_at
    FROM projects p JOIN users u ON u.id = p.owner_id
    WHERE p.visibility = 'public'
      AND ($1::text IS NULL OR p.search @@ websearch_to_tsquery('russian', $1))
      AND ($2::text IS NULL OR lower(u.login) = lower($2))
      AND ($3::bigint IS NULL OR (p.updated_at, p.id) < ($3, $4))
    ORDER BY p.updated_at DESC, p.id DESC
    LIMIT $5";

/// Собирает курсор из места, на котором остановилась страница.
///
/// Курсор - **не смещение**: по смещению вставленный соседом проект сдвигает выдачу, и
/// читатель либо видит запись дважды, либо не видит её вовсе.
fn encode_cursor(updated_at: i64, id: &str) -> String {
    use base64::Engine as _;
    base64::engine::general_purpose::URL_SAFE_NO_PAD.encode(format!("{updated_at}.{id}"))
}

/// Разбирает курсор. `None` - курсор испорчен.
fn decode_cursor(cursor: &str) -> Option<(i64, String)> {
    use base64::Engine as _;
    let raw = base64::engine::general_purpose::URL_SAFE_NO_PAD
        .decode(cursor)
        .ok()?;
    let text = String::from_utf8(raw).ok()?;
    let (time, id) = text.split_once('.')?;
    Some((time.parse().ok()?, id.to_string()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cursor_makes_a_round_trip() {
        let text = encode_cursor(1_725_000_000, "abcDEF-_123456789012");
        assert_eq!(
            decode_cursor(&text),
            Some((1_725_000_000, "abcDEF-_123456789012".to_string()))
        );
        assert!(
            text.chars()
                .all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_'),
            "курсор попадает в адрес: {text}"
        );
    }

    #[test]
    fn a_broken_cursor_is_refused_rather_than_guessed() {
        assert_eq!(decode_cursor("не-base64!!"), None);
        // Разобралось как base64, но внутри нет точки - тоже отказ: угадывать
        // "наверное, это время" значило бы отдать читателю чужую страницу.
        use base64::Engine as _;
        let no_dot = base64::engine::general_purpose::URL_SAFE_NO_PAD.encode("1725000000");
        assert_eq!(decode_cursor(&no_dot), None);
    }

    #[test]
    fn the_page_is_bounded_from_both_sides() {
        assert_eq!(page_size(None), PAGE, "без просьбы — умолчание");
        assert_eq!(page_size(Some(5)), 5, "разумную просьбу слушаем");
        assert_eq!(page_size(Some(100_000)), PAGE_MAX, "потолок прижимает");
        // Ноль и отрицательное - не отказ, а нижняя граница: пустая страница с курсором
        // заставила бы читателя ходить по кругу.
        assert_eq!(page_size(Some(0)), 1);
        assert_eq!(page_size(Some(-7)), 1);
    }

    #[test]
    fn the_body_comes_from_the_caller_and_not_from_the_database() {
        // С h тексты живут на диске: запрос, читающий их из базы, молча индексировал бы
        // пустоту - поиск по телу перестал бы находить, а отказа бы не было.
        assert!(SEARCH_SQL.contains("$2"), "тело — параметр запроса");
        assert!(
            !SEARCH_SQL.contains("project_files"),
            "поиск снова читает тексты из базы"
        );
    }

    #[test]
    fn the_showcase_asks_the_database_about_visibility() {
        // Проверка стоит В запросе: фильтр после выборки прячется в первой же забытой
        // ветви и показывает чужой закрытый проект.
        assert!(SELECT_PUBLIC.contains("p.visibility = 'public'"));
    }
}
