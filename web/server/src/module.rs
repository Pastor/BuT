//! Исполнение модуля `takt-wasm` на сервере.
//!
//! # Протокол
//!
//! Плоский C-ABI (фичи): `takt_io_ptr`, `takt_io_cap`, `takt_io_reserve`,
//! операции вида `takt_<имя>(длина) -> длина`. Запрос и ответ - UTF-8 JSON в одном
//! буфере.
//!
//! Адрес буфера перечитывается **после каждого вызова**: `Vec` при росте переезжает, а
//! память модуля тем более.
//!
//! # Один вызов - свой экземпляр
//!
//! Модуль однопоточен и не реентерабелен (его состояние живёт в `thread_local`),
//! поэтому на каждый запрос заводится свой `Store` и свой экземпляр. Дорого
//! компилировать байт-код, а не создавать экземпляр - компиляция кешируется по версии.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Mutex;

use anyhow::Context as _;
use wasmtime::{Engine, Instance, Memory, Module, Store, TypedFunc};

/// Наибольший ответ модуля.
///
/// Предел нужен: вывод цели `c` на модели в четыреста состояний - 200 КиБ, но
/// модель приходит извне, и "сколько получится" здесь не ответ. Число - вчетверо больше
/// предела проекта (512 КиБ).
pub const MAX_REPLY: u32 = 2 * 1024 * 1024;

/// Скомпилированные модули по версии.
///
/// Кеш по версии, а не один на сервер: у проекта своя версия модуля, и
/// старая ссылка обязана открываться своим компилятором.
pub struct Modules {
    engine: Engine,
    /// Каталог собранной статики: в нём лежит `wasm/<версия>/takt.wasm`.
    static_dir: PathBuf,
    compiled: Mutex<HashMap<String, Module>>,
}

impl Modules {
    /// Заводит кеш модулей поверх каталога статики.
    pub fn new(static_dir: impl Into<PathBuf>) -> anyhow::Result<Self> {
        let engine = Engine::default();
        Ok(Self {
            engine,
            static_dir: static_dir.into(),
            compiled: Mutex::new(HashMap::new()),
        })
    }

    /// Компилирует модель модулем названной версии.
    ///
    /// Возвращает файлы вывода - пары "имя, текст".
    ///
    /// # Ошибки
    /// - модуля такой версии нет на диске либо он не грузится;
    /// - модуль ответил отказом - тогда ошибка несёт его текст.
    pub fn compile(
        &self,
        version: &str,
        target: &str,
        args: &str,
        source: &str,
    ) -> anyhow::Result<Vec<(String, String)>> {
        let request = serde_json::json!({"target": target, "args": args, "source": source});
        let reply = self.call(version, "takt_compile", &request.to_string())?;
        let parsed: serde_json::Value =
            serde_json::from_str(&reply).context("ответ модуля не разбирается")?;
        if parsed["ok"] != serde_json::Value::Bool(true) {
            let message = parsed["error"]["message"]
                .as_str()
                .unwrap_or("модуль отказал без причины");
            // Код отказа приезжает отдельным полем, и без него причина в архиве
            // перестаёт быть отсылкой к справочнику диагностик: искать "вещественный
            // тип не существует" в приложении бесполезно, искать `SV-003` - нет.
            return match parsed["error"]["code"].as_str() {
                Some(code) => Err(anyhow::anyhow!("[{code}] {message}")),
                None => Err(anyhow::anyhow!("{message}")),
            };
        }
        let files = parsed["files"]
            .as_array()
            .context("ответ модуля без списка файлов")?
            .iter()
            .map(|file| {
                (
                    file["name"].as_str().unwrap_or_default().to_string(),
                    file["text"].as_str().unwrap_or_default().to_string(),
                )
            })
            .collect();
        Ok(files)
    }

    /// Проверяет цель и ключи сборки, ничего не компилируя.
    ///
    /// Своего разбора у сервера нет и быть не должно: список ключей и таблица
    /// применимости живут в `compile_cli`, и вторая копия разошлась бы с ней молча -
    /// сервер принимал бы ключ, который сборка отвергает. Отсюда тот же круговой рейс,
    /// что у выгрузки архива.
    ///
    /// # Ошибки
    /// - модуля такой версии нет на диске либо он не грузится;
    /// - модуль отверг цель либо ключи - тогда ошибка несёт его причину.
    pub fn check_flags(&self, version: &str, target: &str, args: &str) -> anyhow::Result<()> {
        let request = serde_json::json!({"target": target, "args": args});
        let reply = self.call(version, "takt_flags", &request.to_string())?;
        let parsed: serde_json::Value =
            serde_json::from_str(&reply).context("ответ модуля не разбирается")?;
        if parsed["ok"] == serde_json::Value::Bool(true) {
            return Ok(());
        }
        let message = parsed["error"]["message"]
            .as_str()
            .unwrap_or("модуль отказал без причины");
        Err(anyhow::anyhow!("{message}"))
    }

    /// Зовёт операцию модуля, передав и приняв JSON.
    fn call(&self, version: &str, name: &str, request: &str) -> anyhow::Result<String> {
        let module = self.module(version)?;
        let mut store = Store::new(&self.engine, ());
        let instance =
            Instance::new(&mut store, &module, &[]).context("экземпляр модуля не создаётся")?;
        let memory: Memory = instance
            .get_memory(&mut store, "memory")
            .context("у модуля нет памяти")?;
        let io_ptr: TypedFunc<(), u32> = instance.get_typed_func(&mut store, "takt_io_ptr")?;
        let io_cap: TypedFunc<(), u32> = instance.get_typed_func(&mut store, "takt_io_cap")?;
        let reserve: TypedFunc<u32, u32> =
            instance.get_typed_func(&mut store, "takt_io_reserve")?;
        let operation: TypedFunc<u32, u32> = instance.get_typed_func(&mut store, name)?;

        let bytes = request.as_bytes();
        let need = u32::try_from(bytes.len()).context("запрос не помещается в 32 бита")?;
        if io_cap.call(&mut store, ())? < need {
            reserve.call(&mut store, need)?;
        }
        // Адрес перечитывается после роста буфера: `Vec` при этом переезжает.
        let ptr = io_ptr.call(&mut store, ())? as usize;
        memory
            .write(&mut store, ptr, bytes)
            .context("запрос не записывается в память модуля")?;

        let len = operation.call(&mut store, need)?;
        if len > MAX_REPLY {
            anyhow::bail!("ответ модуля больше предела ({len} Б при пределе {MAX_REPLY})");
        }
        // И здесь тоже: операция могла вырастить буфер под свой ответ.
        let ptr = io_ptr.call(&mut store, ())? as usize;
        let mut out = vec![0u8; len as usize];
        memory
            .read(&store, ptr, &mut out)
            .context("ответ не читается из памяти модуля")?;
        String::from_utf8(out).context("ответ модуля — не UTF-8")
    }

    /// Берёт скомпилированный модуль версии, компилируя его при первом спросе.
    fn module(&self, version: &str) -> anyhow::Result<Module> {
        if let Ok(cache) = self.compiled.lock()
            && let Some(module) = cache.get(version)
        {
            return Ok(module.clone());
        }
        let path = self.path_of(version)?;
        let module = Module::from_file(&self.engine, &path)
            .with_context(|| format!("модуль {} не грузится", path.display()))?;
        if let Ok(mut cache) = self.compiled.lock() {
            cache.insert(version.to_string(), module.clone());
        }
        Ok(module)
    }

    /// Путь модуля версии.
    ///
    /// Версия проверяется на форму: она приходит из базы, но попадает в путь, и `../` в
    /// ней увёл бы чтение за каталог статики.
    fn path_of(&self, version: &str) -> anyhow::Result<PathBuf> {
        anyhow::ensure!(
            is_version(version),
            "версия модуля '{version}' не похожа на версию"
        );
        let path = self.static_dir.join("wasm").join(version).join("takt.wasm");
        anyhow::ensure!(
            Path::new(&path).is_file(),
            "модуля версии {version} нет в статике"
        );
        Ok(path)
    }
}

/// Похожа ли строка на номер версии.
pub fn is_version(text: &str) -> bool {
    !text.is_empty()
        && text.chars().all(|c| c.is_ascii_digit() || c == '.')
        && text.contains('.')
        && !text.contains("..")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_version_is_a_version_and_not_a_path() {
        assert!(is_version("0.58.0"));
        assert!(is_version("1.0"));
        // Версия приходит из базы, но попадает в путь: без проверки строка вида
        // `../../etc` увела бы чтение за каталог статики.
        assert!(!is_version("../../etc"));
        assert!(!is_version("0..58"));
        assert!(!is_version("latest"));
        assert!(!is_version(""));
        assert!(!is_version("058"), "без точки это не версия");
    }
}
