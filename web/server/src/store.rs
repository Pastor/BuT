//! Хранилище исходников: файловая система.
//!
//! # Срок хранения
//!
//! Проект, к которому не обращались дольше срока, **сворачивается** в архив формата g,
//! а распакованные файлы снимаются с диска. Первое же обращение разворачивает его
//! обратно - для автора архивация невидима, кроме того, что первое открытие медленнее.
//! Обращение на чтение или запись сбрасывает счётчик.
//!
//! # Подметание осиротевших каталогов
//!
//! Состав ведёт база, и каталог, чьей строки в ней нет, не виден никому: ни витрине, ни
//! автору, ни выгрузке. Он лишь занимает место - и растёт, потому что удалить его
//! некому. Поэтому хранилище умеет **перечислять** то, что на нём лежит
//! ([`Store::owners`], [`Store::projects_of`]), а решение "это сирота" принимает
//! [`crate::retention`]: у базы спрашивает он.
//!
//! # Границы имён
//!
//! Идентификаторы и имена файлов попадают в путь. Они уже судятся выше
//! (`limits::check_file_name`, `projects::new_id`), но проверяются и здесь: хранилище -
//! последняя точка перед диском, и полагаться на то, что все вызывающие проверили,
//! значит однажды записать файл за пределы корня.

use std::io::{Read as _, Write as _};
use std::path::{Path, PathBuf};

use anyhow::Context as _;

/// Файл проекта: имя и текст.
#[derive(Debug, Clone)]
pub struct Stored {
    pub name: String,
    pub text: String,
}

/// Что лежит на диске: имя и когда его последний раз трогали.
///
/// Время нужно **подметанию**: каталог, заведённый только что, может принадлежать
/// записи, чья транзакция ещё не закрыта, - и сирота он лишь на вид. Отличает их
/// возраст, и потому он читается здесь, у диска, а не угадывается выше.
#[derive(Debug, Clone)]
pub struct Held {
    pub name: String,
    /// Время последней правки, секунды эпохи. `0` - время не читается (для подметания
    /// это "давно", и такой каталог судится наравне со старыми).
    pub modified: i64,
}

/// Корень хранилища исходников.
#[derive(Debug, Clone)]
pub struct Store {
    root: PathBuf,
}

impl Store {
    /// Заводит хранилище поверх каталога.
    ///
    /// # Ошибки
    /// Каталог не создаётся.
    pub fn new(root: impl Into<PathBuf>) -> anyhow::Result<Self> {
        let root = root.into();
        std::fs::create_dir_all(&root)
            .with_context(|| format!("каталог проектов {} не создаётся", root.display()))?;
        Ok(Self { root })
    }

    /// Корень хранилища.
    pub fn root(&self) -> &Path {
        &self.root
    }

    /// Читает файл проекта.
    ///
    /// # Ошибки
    /// Негодное имя, проекта нет на диске, файл не читается как UTF-8.
    pub fn read(&self, owner: &str, project: &str, name: &str) -> anyhow::Result<String> {
        let path = self.file_path(owner, project, name)?;
        std::fs::read_to_string(&path)
            .with_context(|| format!("файл '{name}' проекта {project} не читается"))
    }

    /// Пишет файл проекта, заводя каталоги по пути.
    ///
    /// # Ошибки
    /// Негодное имя либо отказ записи.
    pub fn write(&self, owner: &str, project: &str, name: &str, text: &str) -> anyhow::Result<()> {
        let path = self.file_path(owner, project, name)?;
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)
                .with_context(|| format!("каталог {} не создаётся", parent.display()))?;
        }
        // Запись через временный файл и переименование: обрыв посередине иначе оставил
        // бы половину исходника, и автор увидел бы отказ компилятора в месте, которого
        // не писал.
        let temporary = path.with_extension("part");
        std::fs::write(&temporary, text)
            .with_context(|| format!("файл '{name}' не записывается"))?;
        std::fs::rename(&temporary, &path)
            .with_context(|| format!("файл '{name}' не переименовывается"))?;
        Ok(())
    }

    /// Убирает файл проекта. Отсутствие файла - не ошибка.
    ///
    /// # Ошибки
    /// Негодное имя либо отказ удаления.
    pub fn remove(&self, owner: &str, project: &str, name: &str) -> anyhow::Result<()> {
        let path = self.file_path(owner, project, name)?;
        match std::fs::remove_file(&path) {
            Ok(()) => Ok(()),
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(()),
            Err(error) => Err(error).context("файл не удаляется"),
        }
    }

    /// Читает все файлы проекта - по списку имён, который ведёт база.
    ///
    /// Список берётся у базы, а не у каталога: каталог знает про файлы, которых нет в
    /// проекте (обрывок записи), а база - про состав.
    ///
    /// # Ошибки
    /// Файл из списка не читается.
    pub fn read_all(
        &self,
        owner: &str,
        project: &str,
        names: &[String],
    ) -> anyhow::Result<Vec<Stored>> {
        names
            .iter()
            .map(|name| {
                Ok(Stored {
                    name: name.clone(),
                    text: self.read(owner, project, name)?,
                })
            })
            .collect()
    }

    /// Убирает проект целиком - и распакованный, и свёрнутый.
    ///
    /// # Ошибки
    /// Негодный идентификатор либо отказ удаления.
    pub fn remove_project(&self, owner: &str, project: &str) -> anyhow::Result<()> {
        let dir = self.project_path(owner, project)?;
        if dir.exists() {
            std::fs::remove_dir_all(&dir).context("каталог проекта не удаляется")?;
        }
        let packed = self.packed_path(owner, project)?;
        if packed.exists() {
            std::fs::remove_file(&packed).context("свёрнутый проект не удаляется")?;
        }
        Ok(())
    }

    /// Свёрнут ли проект.
    ///
    /// # Ошибки
    /// Негодный идентификатор.
    pub fn is_packed(&self, owner: &str, project: &str) -> anyhow::Result<bool> {
        Ok(self.packed_path(owner, project)?.is_file())
    }

    /// Сворачивает проект: файлы уходят в один архив, каталог снимается.
    ///
    /// Порядок обязателен - сначала архив на диске, потом снятие файлов. Обратный
    /// порядок при обрыве оставляет проект без исходников.
    ///
    /// # Ошибки
    /// Негодный идентификатор, отказ чтения либо записи.
    pub fn pack(&self, owner: &str, project: &str, names: &[String]) -> anyhow::Result<()> {
        let files = self.read_all(owner, project, names)?;
        let target = self.packed_path(owner, project)?;
        if let Some(parent) = target.parent() {
            std::fs::create_dir_all(parent)?;
        }
        let temporary = target.with_extension("part");
        {
            let handle = std::fs::File::create(&temporary)
                .with_context(|| format!("архив {} не создаётся", temporary.display()))?;
            let mut zip = zip::ZipWriter::new(handle);
            let options: zip::write::FileOptions<'_, ()> = zip::write::FileOptions::default()
                .compression_method(zip::CompressionMethod::Deflated);
            for file in &files {
                zip.start_file(&file.name, options)?;
                zip.write_all(file.text.as_bytes())?;
            }
            zip.finish()?;
        }
        std::fs::rename(&temporary, &target).context("архив не переименовывается")?;
        let dir = self.project_path(owner, project)?;
        if dir.exists() {
            std::fs::remove_dir_all(&dir).context("каталог проекта не снимается")?;
        }
        Ok(())
    }

    /// Разворачивает свёрнутый проект обратно.
    ///
    /// Отсутствие архива - не ошибка: проект уже развёрнут.
    ///
    /// # Ошибки
    /// Негодный идентификатор, испорченный архив, отказ записи.
    pub fn unpack(&self, owner: &str, project: &str) -> anyhow::Result<()> {
        let packed = self.packed_path(owner, project)?;
        if !packed.is_file() {
            return Ok(());
        }
        let handle = std::fs::File::open(&packed).context("свёрнутый проект не открывается")?;
        let mut zip = zip::ZipArchive::new(handle).context("свёрнутый проект не разбирается")?;
        for index in 0..zip.len() {
            let mut entry = zip.by_index(index)?;
            let name = entry.name().to_string();
            let mut text = String::new();
            entry.read_to_string(&mut text)?;
            self.write(owner, project, &name, &text)?;
        }
        // Архив снимается после распаковки: обратный порядок при обрыве оставляет
        // проект без исходников вовсе.
        std::fs::remove_file(&packed).context("свёрнутый проект не удаляется")?;
        Ok(())
    }

    /// Перечисляет владельцев, у которых на диске что-то лежит.
    ///
    /// Сегмент, не годный в путь, пропускается, а не удаляется: наши каталоги так
    /// называться не могут, а чужое в корне хранилища - не предмет подметания.
    /// Молчаливое удаление чужого дороже мусора.
    ///
    /// # Ошибки
    /// Корень не читается.
    pub fn owners(&self) -> anyhow::Result<Vec<Held>> {
        let mut found = Vec::new();
        for entry in read_dir(&self.root)? {
            let name = entry.file_name().to_string_lossy().into_owned();
            if segment(&name).is_err() || !entry.path().is_dir() {
                continue;
            }
            found.push(Held {
                modified: modified_at(&entry.path()),
                name,
            });
        }
        found.sort_by(|a, b| a.name.cmp(&b.name));
        Ok(found)
    }

    /// Перечисляет проекты владельца - обе формы, каталог и свёрнутый архив.
    ///
    /// Формы **сливаются** в одну запись: проект бывает и развёрнутым, и свёрнутым
    /// разом (обрыв посередине), а подметание убирает обе - считать его дважды значило
    /// бы отчитаться о вдвое большей работе. Возраст берётся у более свежей формы:
    /// удалять надо то, чего давно не трогали.
    ///
    /// # Ошибки
    /// Негодный сегмент владельца. Отсутствие каталога - не ошибка: пусто.
    pub fn projects_of(&self, owner: &str) -> anyhow::Result<Vec<Held>> {
        let dir = self.root.join(segment(owner)?);
        let mut found: std::collections::BTreeMap<String, i64> = std::collections::BTreeMap::new();
        for entry in read_dir(&dir)? {
            let path = entry.path();
            let raw = entry.file_name().to_string_lossy().into_owned();
            // Свёрнутая форма - только `.zip`: `.part` есть обрывок записи, и он
            // снимается вместе с проектом, а сам по себе проектом не зовётся.
            let name = if path.is_dir() {
                raw
            } else if let Some(stem) = raw.strip_suffix(".zip") {
                stem.to_string()
            } else {
                continue;
            };
            if segment(&name).is_err() {
                continue;
            }
            let at = modified_at(&path);
            found
                .entry(name)
                .and_modify(|seen| *seen = (*seen).max(at))
                .or_insert(at);
        }
        Ok(found
            .into_iter()
            .map(|(name, modified)| Held { name, modified })
            .collect())
    }

    /// Убирает каталог владельца, если в нём ничего не осталось.
    ///
    /// Непустой каталог - не ошибка: у владельца просто есть проекты.
    ///
    /// # Ошибки
    /// Негодный сегмент либо отказ удаления.
    pub fn remove_owner_if_empty(&self, owner: &str) -> anyhow::Result<bool> {
        let dir = self.root.join(segment(owner)?);
        if !dir.is_dir() || read_dir(&dir)?.next().is_some() {
            return Ok(false);
        }
        std::fs::remove_dir(&dir).context("пустой каталог владельца не удаляется")?;
        Ok(true)
    }

    /// Путь каталога проекта.
    fn project_path(&self, owner: &str, project: &str) -> anyhow::Result<PathBuf> {
        Ok(self.root.join(segment(owner)?).join(segment(project)?))
    }

    /// Путь свёрнутого проекта.
    fn packed_path(&self, owner: &str, project: &str) -> anyhow::Result<PathBuf> {
        Ok(self
            .root
            .join(segment(owner)?)
            .join(format!("{}.zip", segment(project)?)))
    }

    /// Путь файла проекта.
    fn file_path(&self, owner: &str, project: &str, name: &str) -> anyhow::Result<PathBuf> {
        anyhow::ensure!(is_file_name(name), "имя файла '{name}' в путь не годится");
        Ok(self.project_path(owner, project)?.join(name))
    }
}

/// Читает каталог. Отсутствие каталога - пустой обход, а не отказ: подметание зовётся и
/// там, где хранилище ещё пусто.
fn read_dir(path: &Path) -> anyhow::Result<Box<dyn Iterator<Item = std::fs::DirEntry>>> {
    match std::fs::read_dir(path) {
        Ok(entries) => Ok(Box::new(entries.flatten())),
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
            Ok(Box::new(std::iter::empty()))
        }
        Err(error) => Err(error).with_context(|| format!("каталог {} не читается", path.display())),
    }
}

/// Когда путь последний раз правили, секунды эпохи.
///
/// Нечитаемое время - `0`, то есть "давно". Обратное умолчание ("только что") сделало
/// бы подметание бесполезным молча: сирота с нечитаемым временем оставалась бы на диске
/// навсегда.
fn modified_at(path: &Path) -> i64 {
    std::fs::metadata(path)
        .and_then(|meta| meta.modified())
        .ok()
        .and_then(|time| time.duration_since(std::time::UNIX_EPOCH).ok())
        .map(|since| since.as_secs() as i64)
        .unwrap_or(0)
}

/// Проверяет сегмент пути - идентификатор владельца либо проекта.
///
/// Последняя точка перед диском. Идентификаторы уже судятся выше, но "выше проверили" -
/// не свойство кода, а надежда: одна забытая ветвь пишет за корень хранилища.
fn segment(value: &str) -> anyhow::Result<&str> {
    anyhow::ensure!(!value.is_empty(), "пустой сегмент пути");
    anyhow::ensure!(
        value
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_'),
        "сегмент пути '{value}' содержит недопустимое"
    );
    Ok(value)
}

/// Годится ли имя в имя файла на диске.
fn is_file_name(name: &str) -> bool {
    !name.is_empty()
        && !name.contains('/')
        && !name.contains('\\')
        && name != "."
        && name != ".."
        && !name.contains("..")
        && name
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || c == '.' || c == '-' || c == '_')
}

#[cfg(test)]
mod tests {
    use super::*;

    fn temporary() -> PathBuf {
        let mut path = std::env::temp_dir();
        path.push(format!(
            "takt-store-{}-{:?}",
            std::process::id(),
            std::thread::current().id()
        ));
        let _ = std::fs::remove_dir_all(&path);
        path
    }

    #[test]
    fn a_file_makes_a_round_trip_through_the_disk() {
        let store = Store::new(temporary()).expect("хранилище");
        store
            .write("u1", "p1", "model.takt", "model A {}")
            .expect("запись");
        assert_eq!(
            store.read("u1", "p1", "model.takt").expect("чтение"),
            "model A {}"
        );
        // Раскладка названа: владелец -> проект -> файлы.
        assert!(
            store
                .root()
                .join("u1")
                .join("p1")
                .join("model.takt")
                .is_file()
        );
        store.remove("u1", "p1", "model.takt").expect("удаление");
        assert!(store.read("u1", "p1", "model.takt").is_err());
        // Удаление того, чего нет, - не ошибка: предмет просьбы выполнен.
        store.remove("u1", "p1", "model.takt").expect("повтор");
        let _ = std::fs::remove_dir_all(store.root());
    }

    #[test]
    fn packing_and_unpacking_keep_the_texts() {
        let store = Store::new(temporary()).expect("хранилище");
        store
            .write("u1", "p1", "a.takt", "model A {}")
            .expect("запись");
        store.write("u1", "p1", "b.json", "[]").expect("запись");
        let names = vec!["a.takt".to_string(), "b.json".to_string()];

        store.pack("u1", "p1", &names).expect("свёртка");
        assert!(store.is_packed("u1", "p1").expect("признак"));
        // Развёрнутых файлов больше нет: ради этого свёртка и делается.
        assert!(store.read("u1", "p1", "a.takt").is_err());

        store.unpack("u1", "p1").expect("развёртка");
        assert!(!store.is_packed("u1", "p1").expect("признак"));
        assert_eq!(
            store.read("u1", "p1", "a.takt").expect("чтение"),
            "model A {}"
        );
        assert_eq!(store.read("u1", "p1", "b.json").expect("чтение"), "[]");
        // Развернуть развёрнутое - не ошибка: обращений к проекту много, и каждое
        // проверяет признак.
        store.unpack("u1", "p1").expect("повтор");
        let _ = std::fs::remove_dir_all(store.root());
    }

    #[test]
    fn a_name_never_leads_out_of_the_root() {
        // Последняя точка перед диском. Проверки выше есть, но "выше проверили" -
        // надежда, а не свойство кода.
        let store = Store::new(temporary()).expect("хранилище");
        for (owner, project, name) in [
            ("../etc", "p1", "model.takt"),
            ("u1", "../p", "model.takt"),
            ("u1", "p1", "../model.takt"),
            ("u1", "p1", "a/b.takt"),
            ("u1", "p1", ".."),
            ("", "p1", "model.takt"),
        ] {
            assert!(
                store.write(owner, project, name, "x").is_err(),
                "путь '{owner}/{project}/{name}' принят"
            );
        }
        let _ = std::fs::remove_dir_all(store.root());
    }

    #[test]
    fn the_disk_can_be_enumerated_for_sweeping() {
        // Предмет - вход подметания: обход обязан видеть то, что лежит, и видеть каждый
        // проект один раз. Ошибись он в сторону слепоты - мусор копится молча; ошибись
        // в сторону двоения - отчёт врёт о работе.
        let store = Store::new(temporary()).expect("хранилище");
        store.write("u1", "p1", "a.takt", "model A {}").expect("p1");
        store.write("u1", "p2", "a.takt", "model B {}").expect("p2");
        store.write("u2", "p3", "a.takt", "model C {}").expect("p3");
        // Свёрнутый проект - тоже проект: подметание убирает обе формы.
        store
            .pack("u1", "p2", &["a.takt".to_string()])
            .expect("свёртка");

        let owners: Vec<String> = store
            .owners()
            .expect("владельцы")
            .into_iter()
            .map(|o| o.name)
            .collect();
        assert_eq!(owners, vec!["u1".to_string(), "u2".to_string()]);

        let projects: Vec<String> = store
            .projects_of("u1")
            .expect("проекты")
            .into_iter()
            .map(|p| p.name)
            .collect();
        assert_eq!(projects, vec!["p1".to_string(), "p2".to_string()]);

        // Обе формы разом (обрыв посередине) - по-прежнему одна запись.
        store.write("u1", "p2", "b.takt", "model D {}").expect("p2");
        assert_eq!(store.projects_of("u1").expect("проекты").len(), 2);

        // Обрывок записи проектом не зовётся: он снимается вместе с проектом, а сам по
        // себе означал бы каталог, которого нет.
        std::fs::write(store.root().join("u1").join("p9.part"), "x").expect("обрывок");
        assert_eq!(store.projects_of("u1").expect("проекты").len(), 2);

        // Чужое в корне хранилища пропускается, а не удаляется: наши каталоги так
        // называться не могут, а молчаливое удаление чужого дороже мусора.
        std::fs::create_dir_all(store.root().join("не наш каталог")).expect("чужое");
        assert_eq!(store.owners().expect("владельцы").len(), 2);

        // Пустой каталог владельца снимается, непустой - нет.
        assert!(!store.remove_owner_if_empty("u2").expect("непустой"));
        store.remove_project("u2", "p3").expect("удаление");
        assert!(store.remove_owner_if_empty("u2").expect("пустой"));
        assert!(!store.root().join("u2").exists());
        // Каталога нет вовсе - не отказ: обход зовётся и после чужой уборки.
        assert!(!store.remove_owner_if_empty("u2").expect("повтор"));
        // Как и перечисление проектов у владельца, которого нет.
        assert!(store.projects_of("u2").expect("пусто").is_empty());
        let _ = std::fs::remove_dir_all(store.root());
    }

    #[test]
    fn removing_a_project_takes_both_forms() {
        let store = Store::new(temporary()).expect("хранилище");
        store
            .write("u1", "p1", "a.takt", "model A {}")
            .expect("запись");
        store
            .pack("u1", "p1", &["a.takt".to_string()])
            .expect("свёртка");
        store
            .write("u1", "p1", "b.takt", "model B {}")
            .expect("запись");
        // Проект бывает и свёрнутым, и развёрнутым разом (обрыв посередине): удаление
        // обязано убрать обе формы, иначе диск копит мусор, которого не видит ни база,
        // ни человек.
        store.remove_project("u1", "p1").expect("удаление");
        assert!(!store.is_packed("u1", "p1").expect("признак"));
        assert!(store.read("u1", "p1", "b.takt").is_err());
        let _ = std::fs::remove_dir_all(store.root());
    }
}
