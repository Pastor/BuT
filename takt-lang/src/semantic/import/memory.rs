//! Файлы подключения в памяти: проект без файловой системы.
//!
//! # Что и зачем
//!
//! `import "lib.takt";` ищет файл по путям поиска на диске. У модуля в браузере
//! диска нет вовсе, и подключение там кончалось `SE-013`, хотя подключаемый файл
//! лежит в том же проекте, что и модель. Этот носитель подставляет вместо диска
//! **состав проекта**: имя файла - текст.
//!
//! # Как устроено
//!
//! Набор файлов - **потоковый** (`thread_local`), как у стража циклов
//! (`statement::loop_context`): библиотеку зовут из нескольких потоков, и общий
//! набор отдавал бы одному вызову чужой проект. Набор живёт ровно столько, сколько
//! страж [`MemoryGuard`]: ранний выход по `?` его не "залипит", а вложенная
//! установка возвращает прежний набор при разрушении.
//!
//! Путь поиска в памяти один - каталог проекта, `.`: состав проекта плоский, и
//! имя сверяется **точным ключом**. Обход каталогов (`../x.takt`) отсюда
//! невозможен по построению - ключа с косой чертой в проекте не бывает, - поэтому
//! проверки канонизации, нужные диску, здесь нет.

use std::cell::RefCell;
use std::collections::BTreeMap;

thread_local! {
    /// Состав проекта для текущего построения; `None` - читать с диска.
    static FILES: RefCell<Option<BTreeMap<String, String>>> = const { RefCell::new(None) };
}

/// Страж набора файлов: пока жив, подключения читаются из памяти.
#[derive(Debug)]
pub struct MemoryGuard {
    previous: Option<BTreeMap<String, String>>,
}

impl Drop for MemoryGuard {
    fn drop(&mut self) {
        let previous = self.previous.take();
        FILES.with(|files| *files.borrow_mut() = previous);
    }
}

/// Ставит состав проекта на время жизни стража.
///
/// Ключ - имя файла в проекте (`lib.takt`), значение - текст.
pub fn install(files: BTreeMap<String, String>) -> MemoryGuard {
    let previous = FILES.with(|current| current.borrow_mut().replace(files));
    MemoryGuard { previous }
}

/// Читаются ли подключения из памяти.
pub fn active() -> bool {
    FILES.with(|files| files.borrow().is_some())
}

/// Имя кандидата в составе проекта: путь поиска `.` даёт `./lib.takt`, а в проекте
/// файл зовётся `lib.takt`.
pub fn key_of(candidate: &str) -> &str {
    let mut key = candidate;
    while let Some(rest) = key.strip_prefix("./") {
        key = rest;
    }
    key
}

/// Текст файла проекта по имени кандидата; `None` - такого файла в проекте нет.
pub fn read(candidate: &str) -> Option<String> {
    let key = key_of(candidate);
    FILES.with(|files| {
        files
            .borrow()
            .as_ref()
            .and_then(|map| map.get(key).cloned())
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn project(pairs: &[(&str, &str)]) -> BTreeMap<String, String> {
        pairs
            .iter()
            .map(|(k, v)| (k.to_string(), v.to_string()))
            .collect()
    }

    #[test]
    fn files_are_read_only_while_the_guard_lives() {
        assert!(!active(), "без стража подключения идут с диска");
        {
            let _guard = install(project(&[("lib.takt", "start S;")]));
            assert!(active());
            assert_eq!(read("./lib.takt").as_deref(), Some("start S;"));
            assert_eq!(read("lib.takt").as_deref(), Some("start S;"));
            assert_eq!(read("./other.takt"), None, "чужого файла в проекте нет");
        }
        assert!(!active(), "страж снят - набор снят вместе с ним");
    }

    #[test]
    fn nested_install_restores_the_outer_project() {
        let _outer = install(project(&[("a.takt", "A")]));
        {
            let _inner = install(project(&[("b.takt", "B")]));
            assert_eq!(read("a.takt"), None, "внутренний набор заменяет внешний");
            assert_eq!(read("b.takt").as_deref(), Some("B"));
        }
        assert_eq!(read("a.takt").as_deref(), Some("A"), "внешний вернулся");
    }

    #[test]
    fn a_path_outside_the_project_is_not_a_key() {
        let _guard = install(project(&[("lib.takt", "L")]));
        assert_eq!(read("../lib.takt"), None, "обход каталога ключом не бывает");
        assert_eq!(read("sub/lib.takt"), None, "состав проекта плоский");
    }
}
