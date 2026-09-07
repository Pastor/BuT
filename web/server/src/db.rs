//! Хранилище: пул соединений и схема.
//!
//! # PostgreSQL, а не SQLite
//!
//! отменяет выбор проработки. Вместе с SQLite уходит и названная там граница "один
//! писатель": у PostgreSQL запись параллельна, и очередь на соединении была бы
//! выдуманной - отсюда пул, а не одно соединение под мьютексом.
//!
//! Полнотекстовый поиск  переезжает с FTS5 на `tsvector` средствами самой
//! базы; словарь `russian` разбирает кириллицу без дополнительных расширений.
//!
//! # Схема одним местом, переходы - приращениями
//!
//! Версия лежит в таблице `schema_version`, схема целиком описана [`SCHEMA`], а
//! [`MIGRATIONS`] несёт шаги "с версии на следующую".
//!
//! Шагов сперва не было намеренно: пока выпуска не случилось, переход с первой версии
//! на вторую - лишний носитель того же знания, а база чужой версии просто отвергалась
//! словами. **Стенд это допущение отменил**: 2026-09-06 на нём осталась база версии 4,
//! сервер ушёл на 6, и выкатка уронила работавший сервис отказом при старте. Данные
//! уцелели - отказ сработал как задумано, - но выкатываться стало нечем.
//!
//! Цена решения названа: носителей знания о схеме теперь **два**, и разъехаться они
//! могут молча - база с нуля и база после перехода окажутся разными при одном номере
//! версии. Тест - тест `both_roads_lead_to_the_same_schema`: он строит обе и
//! сравнивает состав колонок и ограничений.
//!
//! # Чего в схеме нет и почему
//!
//! Ни почты, ни имени, ни адреса, ни `User-Agent`, ни часового пояса, ни
//! `last_seen_at`. восстановление пароля -
//! **сброс администратором**, поэтому почта не нужна вовсе, а обещание A6 "не
//! хранить адрес" держится тем, что хранить его негде. Тест на это стоит
//! отдельным тестом: колонка, заведённая "на будущее", - это персональные
//! данные, которых никто не собирался собирать.

use deadpool_postgres::{Config as PoolConfig, Pool, Runtime};
use tokio_postgres::NoTls;

/// Версия схемы. Растёт вместе с изменением таблиц.
///
/// c подняла её со `1` до `2` (колонка `search` перестала быть вычисляемой базой), h -
/// до `3`: **тексты файлов ушли из базы на диск**, а у проекта появились отметка
/// последнего обращения и признак свёртки. p подняла до `5`: **цель и ключи сборки
/// стали свойством проекта**. Шага перехода нет намеренно (выпуска не было) - база
/// прежней версии **отвергается с обоими номерами**, и это видно словами, а не
/// проявляется потерей данных на стенде. n подняла до `6`: у проекта появился активный
/// Сценарий (их бывает несколько).
pub const SCHEMA_VERSION: i64 = 6;

/// Заводит пул соединений по строке подключения.
///
/// # Ошибки
/// Строка подключения не разбирается либо пул не создаётся.
pub fn pool(url: &str) -> anyhow::Result<Pool> {
    let mut config = PoolConfig::new();
    config.url = Some(url.to_string());
    let pool = config.create_pool(Some(Runtime::Tokio1), NoTls)?;
    Ok(pool)
}

/// Шаги перехода: "с какой версии" -> SQL, приводящий её к следующей.
///
/// Появились не сразу и по факту: пока выпуска не было, база прежней версии просто
/// отвергалась - "шага перехода нет намеренно". Стенд с живыми данными это допущение
/// отменил: там осталась база версии 4, а сервер ушёл на 6, и подъём свежего кода
/// уронил сервис отказом. Данные при этом уцелели - отказ сработал как задумано, - но
/// выкатываться стало нечем.
///
/// **Второй носитель того же знания.** [`SCHEMA`] описывает схему целиком, а шаги - её
/// приращения: разъедься они, база, поднятая с нуля, и база, прошедшая переход, будут
/// Разными при одинаковом номере версии.
///
/// Каждый шаг обязан быть идемпотентным по построению (`IF NOT EXISTS`): его повторное
/// применение случается при обрыве связи между `ALTER` и записью номера версии.
const MIGRATIONS: &[(i64, &str)] = &[
    // 4 -> 5: цель и ключи сборки стали свойством проекта.
    (
        4,
        "ALTER TABLE projects ADD COLUMN IF NOT EXISTS build_target TEXT NOT NULL DEFAULT 'c';
         ALTER TABLE projects ADD COLUMN IF NOT EXISTS build_args   TEXT NOT NULL DEFAULT '';",
    ),
    // 5 -> 6: активный сценарий и род файла `markdown`.
    //
    // Ограничение `CHECK` заменяется целиком: в PostgreSQL его нельзя "дополнить", а
    // без замены запись файла-пояснения отказала бы на последнем рубеже - том самом,
    // ради которого список родов в базе и повторён.
    (
        5,
        "ALTER TABLE projects ADD COLUMN IF NOT EXISTS main_scenario TEXT;
         ALTER TABLE project_files DROP CONSTRAINT IF EXISTS project_files_kind_check;
         ALTER TABLE project_files
             ADD CONSTRAINT project_files_kind_check
             CHECK (kind IN ('takt', 'scenario', 'markdown'));",
    ),
];

/// Приводит схему к [`SCHEMA_VERSION`].
///
/// Пустая база строится из [`SCHEMA`] целиком; база прежней версии проходит шаги
/// [`MIGRATIONS`] по одному, каждый - **в своей транзакции** вместе с записью нового
/// номера: обрыв посреди перехода иначе оставил бы схему изменённой, а номер - прежним.
///
/// # Ошибки
/// База новее сервера, шага перехода нет, либо шаг не применился. Отказ
/// **называет обе версии**: молчаливый переход означал бы порчу данных стенда.
pub async fn prepare(client: &mut tokio_postgres::Client) -> anyhow::Result<()> {
    client
        .batch_execute("CREATE TABLE IF NOT EXISTS schema_version (version BIGINT NOT NULL)")
        .await?;
    let rows = client
        .query("SELECT version FROM schema_version", &[])
        .await?;
    let Some(mut version) = rows.first().map(|row| row.get::<_, i64>(0)) else {
        client.batch_execute(SCHEMA).await?;
        client
            .execute(
                "INSERT INTO schema_version(version) VALUES ($1)",
                &[&SCHEMA_VERSION],
            )
            .await?;
        return Ok(());
    };

    if version > SCHEMA_VERSION {
        anyhow::bail!(
            "база версии {version} новее сервера ({SCHEMA_VERSION}): выкатите сервер посвежее"
        );
    }
    while version < SCHEMA_VERSION {
        let Some((_, sql)) = MIGRATIONS.iter().find(|(from, _)| *from == version) else {
            anyhow::bail!(
                "база версии {version}, а сервер знает {SCHEMA_VERSION}: шага перехода нет"
            );
        };
        let step = client.transaction().await?;
        step.batch_execute(sql).await?;
        step.execute("UPDATE schema_version SET version = $1", &[&(version + 1)])
            .await?;
        step.commit().await?;
        version += 1;
        tracing::info!(version, "схема переведена на следующую версию");
    }
    Ok(())
}

/// Схема целиком.
///
/// Таблицы проектов заводятся **здесь и сразу**, хотя ручки к ним появятся задачей
/// `09b`: схема - одно место, и дописывать её частями значило бы заводить переход между
/// версиями до первого выпуска.
pub const SCHEMA: &str = r#"
CREATE TABLE users (
    id         TEXT PRIMARY KEY,
    -- ⚠️ `CITEXT` в проекте не заводится (расширение ставится отдельно и на
    -- стенде его может не быть): единственность без учёта регистра держит
    -- индекс по `lower(login)`. `Ivan` и `ivan` — один человек, иначе вход
    -- зависел бы от регистра, а два владельца получили бы неразличимые на
    -- глаз имена.
    login      TEXT NOT NULL,
    -- ⚠️ Может быть NULL: человек, вошедший через площадку, пароля у нас не
    -- заводит вовсе (задача 09f-1). Проверка пароля обязана это учитывать —
    -- иначе пустой хеш сравнивался бы с введённым.
    pass_hash  TEXT,
    role       TEXT NOT NULL DEFAULT 'user' CHECK (role IN ('user', 'admin')),
    created_at BIGINT NOT NULL
);
CREATE UNIQUE INDEX users_login_lower ON users (lower(login));

CREATE TABLE refresh_tokens (
    id         BIGSERIAL PRIMARY KEY,
    user_id    TEXT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    -- Хранится ОТПЕЧАТОК, а не токен: утечка базы не даёт войти.
    token_hash TEXT NOT NULL UNIQUE,
    -- Семейство от одного входа: повторное предъявление гасит его целиком.
    family     TEXT NOT NULL,
    created_at BIGINT NOT NULL,
    expires_at BIGINT NOT NULL,
    revoked_at BIGINT
);
CREATE INDEX refresh_tokens_family ON refresh_tokens(family);

CREATE TABLE projects (
    id               TEXT PRIMARY KEY,
    owner_id         TEXT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    name             TEXT NOT NULL,
    description      TEXT NOT NULL DEFAULT '',
    visibility       TEXT NOT NULL CHECK (visibility IN ('private', 'link', 'public')),
    takt_lang        TEXT NOT NULL,
    language_version TEXT NOT NULL,
    main_file        TEXT,
    -- Цель и ключи сборки, выбранные АВТОРОМ (задача 09p, решение заказчика
    -- 2026-09-05). ⚠️ Свойство ПРОЕКТА, а не файла: проект с библиотекой и
    -- прошивкой собирается одной целью, и это названная граница решения.
    -- ⚠️ Без них чужой проект открывался бы целью ЧИТАТЕЛЯ: модель на
    -- `sv-mmio` с `--bus=apb` показывалась бы как `c` без ключей — то есть не
    -- тем, что имел в виду автор.
    build_target     TEXT NOT NULL DEFAULT 'c',
    build_args       TEXT NOT NULL DEFAULT '',
    -- Активный сценарий: их в проекте бывает несколько (задача 09n, поручение
    -- заказчика). ⚠️ Правило то же, что у `main_file`: проект НАЗЫВАЕТ свой, а
    -- страница вправе смотреть другой — выбор читателя живёт в черновике. Не
    -- назови проект свой — читатель прогонял бы первый по имени, то есть не
    -- тот, на котором автор показывает работу модели.
    main_scenario    TEXT,
    revision         BIGINT NOT NULL DEFAULT 0,
    size_bytes       BIGINT NOT NULL DEFAULT 0,
    forked_from      TEXT REFERENCES projects(id) ON DELETE SET NULL,
    created_at       BIGINT NOT NULL,
    updated_at       BIGINT NOT NULL,
    -- Когда к проекту обращались в последний раз — на чтение ИЛИ на запись
    -- (корректировка заказчика 2026-09-04). От неё считается срок хранения, и
    -- обращение счётчик сбрасывает.
    touched_at       BIGINT NOT NULL DEFAULT 0,
    -- Когда проект свёрнут в архив; `NULL` — развёрнут. Свёрнутый живёт одним
    -- `.zip` в хранилище, и первое же обращение разворачивает его обратно.
    archived_at      BIGINT
);
CREATE INDEX projects_owner ON projects(owner_id);
CREATE INDEX projects_public ON projects(visibility, updated_at);
-- Обход по сроку хранения идёт по этому индексу: без него подметание читало бы
-- таблицу целиком на каждом проходе.
CREATE INDEX projects_touched ON projects(archived_at, touched_at);

-- ⚠️ ТЕКСТА здесь нет (корректировка заказчика 2026-09-04): исходники живут в
-- файловой системе (`<владелец>/<проект>/<файл>`, модуль `store`), а база
-- ведёт СОСТАВ — имя, вид и размер. Порождённый вывод целей не хранится нигде:
-- он воспроизводим (0048) и нужен лишь при выгрузке в архив и при показе.
CREATE TABLE project_files (
    project_id TEXT NOT NULL REFERENCES projects(id) ON DELETE CASCADE,
    name       TEXT NOT NULL,
    -- ⚠️ Список видов повторяет `limits::Kind` намеренно: база — последний
    -- рубеж, и запись мимо `check_file_name` (миграция, чужой скрипт) обязана
    -- отказать здесь. Расхождение видно сразу — вставка падает, а не молча
    -- заводит файл, о роде которого страница ничего не знает. Сторож —
    -- проверка родов в `tests/projects.rs`.
    kind       TEXT NOT NULL CHECK (kind IN ('takt', 'scenario', 'markdown')),
    size_bytes BIGINT NOT NULL,
    PRIMARY KEY (project_id, name)
);

CREATE TABLE project_grants (
    project_id TEXT NOT NULL REFERENCES projects(id) ON DELETE CASCADE,
    user_id    TEXT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    level      TEXT NOT NULL CHECK (level IN ('view', 'fork', 'edit')),
    granted_at BIGINT NOT NULL,
    PRIMARY KEY (project_id, user_id)
);

-- Внешняя учётная запись → наша (задача 09f-1). Одна площадка даёт одному
-- человеку одну строку; у нашего пользователя строк — по числу площадок.
--
-- ⚠️ Персональных данных здесь нет и быть не может: из ответа площадки
-- разбирается ОДНО поле — идентификатор. Ни почты, ни имени, ни фотографии, ни
-- токенов площадки: последние нужны один раз, внутри обработчика.
CREATE TABLE external_identities (
    provider   TEXT NOT NULL CHECK (provider IN ('yandex', 'vk')),
    subject    TEXT NOT NULL,
    user_id    TEXT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    created_at BIGINT NOT NULL,
    PRIMARY KEY (provider, subject)
);
CREATE INDEX external_identities_user ON external_identities(user_id);

-- Незавершённый вход. Живёт минуты, персональных данных не несёт, чистится при
-- каждой вставке — как окно частоты (09a).
--
-- ⚠️ `state` лежит ОТПЕЧАТКОМ (приём refresh-токенов 09a): утечка таблицы не
-- даёт довести чужой вход. Ticket — та же строка со `stage = 'ticket'`: второй
-- таблицы с тем же сроком жизни и той же уборкой не заводится.
CREATE TABLE oauth_flows (
    state_hash    TEXT PRIMARY KEY,
    provider      TEXT NOT NULL,
    stage         TEXT NOT NULL CHECK (stage IN ('started', 'ticket')),
    nonce_hash    TEXT NOT NULL,
    code_verifier TEXT,
    purpose       TEXT NOT NULL CHECK (purpose IN ('login', 'link')),
    user_id       TEXT REFERENCES users(id) ON DELETE CASCADE,
    subject       TEXT,
    return_to     TEXT NOT NULL DEFAULT '/',
    created_at    BIGINT NOT NULL,
    expires_at    BIGINT NOT NULL
);
CREATE INDEX oauth_flows_expires ON oauth_flows(expires_at);

-- Поиск по открытым проектам (задача 09c).
--
-- ⚠️ Колонка НЕ вычисляемая базой (`GENERATED`), хотя такой заведена задачей
-- 09a. Причина названа проработкой: искать надо и по ТЕКСТУ файлов, а
-- вычисляемая колонка видит только свою строку — соседнюю таблицу ей не
-- прочесть. Значение кладёт один носитель `search::refresh`, и второго места
-- вычисления нет (класс 0084).
ALTER TABLE projects ADD COLUMN search tsvector;
CREATE INDEX projects_search ON projects USING gin(search);
"#;

/// Текущее время в Unix-секундах.
pub fn now() -> i64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_secs() as i64)
        .unwrap_or_default()
}
