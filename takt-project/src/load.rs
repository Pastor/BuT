//! Проект с диска: каталог с манифестом, архив либо одна модель.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use crate::archive::{Limits, SourceFile, parse_manifest, unpack};
use crate::belongs::scenarios_of;
use crate::error::Error;
use crate::kind::{Kind, check_file_name, stem_of};
use crate::manifest::{MANIFEST, Manifest};

/// Форма, в которой проект пришёл.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Form {
    /// Каталог, где рядом с файлами лежит манифест.
    Directory,
    /// Архив `.zip` с манифестом и исходниками в `src/`.
    Archive,
    /// Одна модель и файлы рядом с ней по соглашению имён.
    Model,
}

/// Проект: состав и выбор автора.
#[derive(Debug, Clone)]
pub struct Project {
    /// Как проект пришёл.
    pub form: Form,
    /// Каталог проекта: у каталога - он сам, у модели - её каталог, у архива - его
    /// каталог. Туда же по умолчанию кладётся вывод.
    pub root: PathBuf,
    /// Метаданные. У одной модели манифеста нет, и поля собраны соглашением: имя -
    /// основа модели, активный файл - она сама, активный сценарий - единственный
    /// подходящий.
    pub manifest: Manifest,
    /// Файлы проекта, по имени.
    pub files: Vec<SourceFile>,
}

impl Project {
    /// Файл проекта по имени.
    pub fn file(&self, name: &str) -> Option<&SourceFile> {
        self.files.iter().find(|file| file.name == name)
    }

    /// Файлы рода, по имени.
    pub fn of_kind(&self, kind: Kind) -> impl Iterator<Item = &SourceFile> {
        self.files
            .iter()
            .filter(move |file| file.kind == kind.as_str())
    }

    /// Состав проекта для компилятора: имя модели -> текст.
    ///
    /// Импорт в проекте разрешается по составу, а не по диску: так же, как у
    /// страницы, - иначе проект собирался бы у одного и не собирался у другого.
    pub fn models(&self) -> BTreeMap<String, String> {
        self.of_kind(Kind::Takt)
            .map(|file| (file.name.clone(), file.text.clone()))
            .collect()
    }

    /// Раскладка модели (`<модель>.takt-ui`); `None` - её в проекте нет.
    pub fn layout_of(&self, model: &str) -> Option<&SourceFile> {
        let (stem, _) = stem_of(model)?;
        self.file(&format!("{stem}{}", Kind::Layout.extension()))
    }

    /// Сценарии модели по правилу принадлежности.
    pub fn scenarios_of(&self, model: &str) -> Vec<String> {
        let Some((stem, _)) = stem_of(model) else {
            return Vec::new();
        };
        let stems: Vec<&str> = self
            .of_kind(Kind::Takt)
            .filter_map(|file| stem_of(&file.name).map(|(stem, _)| stem))
            .collect();
        scenarios_of(
            stem,
            self.of_kind(Kind::Scenario).map(|f| f.name.as_str()),
            &stems,
        )
    }
}

/// Читает проект: каталог с манифестом, архив `.zip` либо файл модели `.takt`.
///
/// # Ошибки
/// Путь не читается; у каталога нет манифеста; манифест называет файл, которого
/// нет; имя файла негодно; архив не разбирается.
pub fn load(path: &Path) -> Result<Project, Error> {
    if path.is_dir() {
        return load_directory(path);
    }
    let name = path
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or_default();
    if name.ends_with(".zip") {
        let bytes = read_bytes(path)?;
        let import = unpack(&bytes, Limits::NONE)?;
        return Ok(Project {
            form: Form::Archive,
            root: parent_of(path),
            manifest: import.manifest,
            files: import.sources,
        });
    }
    if matches!(stem_of(name), Some((_, Kind::Takt))) {
        return load_model(path);
    }
    Err(Error::Invalid(format!(
        "'{}': проект - каталог с '{MANIFEST}', архив '.zip' либо модель '.takt'",
        path.display()
    )))
}

fn load_directory(dir: &Path) -> Result<Project, Error> {
    let manifest_path = dir.join(MANIFEST);
    if !manifest_path.is_file() {
        return Err(Error::Invalid(format!(
            "в каталоге '{}' нет '{MANIFEST}': укажите модель '.takt' либо заведите манифест",
            dir.display()
        )));
    }
    let manifest = parse_manifest(&read_text(&manifest_path)?)?;
    // Состав называет манифест: файл рядом, которого в нём нет, в проект не входит,
    // а названный и отсутствующий - отказ, иначе проект собрался бы наполовину.
    // Пустой состав - все файлы каталога известных родов.
    let names: Vec<String> = if manifest.files.is_empty() {
        known_files(dir)?
    } else {
        manifest.files.iter().map(|f| f.name.clone()).collect()
    };
    let mut files = Vec::new();
    for name in names {
        let kind = check_file_name(&name)?;
        let path = dir.join(&name);
        if !path.is_file() {
            return Err(Error::Invalid(format!(
                "'{MANIFEST}' называет файл '{name}', а его в каталоге нет"
            )));
        }
        files.push(SourceFile {
            text: read_text(&path)?,
            name,
            kind: kind.as_str().to_string(),
        });
    }
    files.sort_by(|a, b| a.name.cmp(&b.name));
    Ok(Project {
        form: Form::Directory,
        root: dir.to_path_buf(),
        manifest,
        files,
    })
}

fn load_model(model: &Path) -> Result<Project, Error> {
    let name = model
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or_default()
        .to_string();
    check_file_name(&name)?;
    let dir = parent_of(model);
    let (stem, _) = stem_of(&name).unwrap_or((&name, Kind::Takt));
    let stem = stem.to_string();
    let neighbours = known_files(&dir)?;
    let models: Vec<&str> = neighbours
        .iter()
        .filter_map(|n| match stem_of(n) {
            Some((s, Kind::Takt)) => Some(s),
            _ => None,
        })
        .collect();
    let scenarios = scenarios_of(&stem, neighbours.iter().map(String::as_str), &models);
    // Соседи по соглашению имён: раскладка, пояснение и карта адресов той же основы.
    let mut chosen = vec![name.clone()];
    for kind in [Kind::Layout, Kind::Markdown, Kind::AddressMap] {
        let own = format!("{stem}{}", kind.extension());
        if neighbours.contains(&own) {
            chosen.push(own);
        }
    }
    chosen.extend(scenarios.iter().cloned());
    let mut files = Vec::new();
    for file in &chosen {
        let kind = check_file_name(file)?;
        files.push(SourceFile {
            text: read_text(&dir.join(file))?,
            name: file.clone(),
            kind: kind.as_str().to_string(),
        });
    }
    files.sort_by(|a, b| a.name.cmp(&b.name));
    let manifest = Manifest {
        format: crate::manifest::FORMAT,
        name: stem,
        main_file: Some(name),
        // Активный сценарий по соглашению - только единственный: из нескольких
        // выбрать за автора значило бы прогнать не тот, и молча.
        main_scenario: (scenarios.len() == 1).then(|| scenarios[0].clone()),
        ..Manifest::default()
    };
    Ok(Project {
        form: Form::Model,
        root: dir,
        manifest,
        files,
    })
}

/// Имена файлов каталога известных родов с годными именами, по алфавиту.
fn known_files(dir: &Path) -> Result<Vec<String>, Error> {
    let entries = std::fs::read_dir(dir)
        .map_err(|error| Error::Io(format!("'{}': {error}", dir.display())))?;
    let mut out: Vec<String> = entries
        .filter_map(Result::ok)
        .filter(|entry| entry.path().is_file())
        .filter_map(|entry| entry.file_name().to_str().map(str::to_string))
        .filter(|name| check_file_name(name).is_ok())
        .collect();
    out.sort();
    Ok(out)
}

fn parent_of(path: &Path) -> PathBuf {
    match path.parent() {
        Some(parent) if !parent.as_os_str().is_empty() => parent.to_path_buf(),
        _ => PathBuf::from("."),
    }
}

fn read_text(path: &Path) -> Result<String, Error> {
    std::fs::read_to_string(path)
        .map_err(|error| Error::Io(format!("'{}': {error}", path.display())))
}

fn read_bytes(path: &Path) -> Result<Vec<u8>, Error> {
    std::fs::read(path).map_err(|error| Error::Io(format!("'{}': {error}", path.display())))
}
